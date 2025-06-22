(*
 *                       Pascal LSP Client
 *
 * Usage allowed under the restrictions of the MIT license.
 *
 * Unit owner : Rickard Johansson <support@rj-texted.se>
 * Web site   : https://www.rj-texted.se
 * Github     : https://github.com/rickard67/LSP-Pascal-Library
 *
 * Copyright (c) 2025 Rickard Johansson. All rights reserved.
 *
*)

unit XLSPExecute;

interface

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.RegularExpressions,
  System.Net.Socket;

type
  TTransportType = (ttStdIO, ttSocketClient, ttSocketServer);

  TReadFromServerEvent = procedure(Sender: TObject; const AJson: string) of object;
  TExitServerEvent = procedure(Sender: TObject; exitcode: Integer) of object;

  TLSPExecuteServerThread = class(TThread)
  private
    FCommandline: String;
    FDir: String;
    FExitCode: LongWord;
    FHost: string;
    FJson: string;
    FReadBytes: TBytes;
    FWriteBytes: TBytes;
    FAcceptEvent: TSimpleEvent;
    FWriteEvent: TSimpleEvent;
    FPort: Integer;
    FProcessInformation: TProcessInformation;
    FSocket: TSocket;
    FLspSocket: TSocket;
    FTransportType: TTransportType;
    FContentHeaderRE: TRegEx;
    FWriteLock: TRTLCriticalSection;
    FOnReadFromServer: TReadFromServerEvent;
    FOnReadErrorFromServer: TReadFromServerEvent;
    FOnExit: TExitServerEvent;
    FOnConnected: TNotifyEvent;
    procedure ExtractAndSendResponceMessages(Bytes: TBytes);
    procedure ProcessErrorOutput(Bytes: TBytes);
    procedure RunServer;
    procedure RunServerThroughSocket;
    procedure ReceiveFinished(const ASyncResult: IAsyncResult);
    procedure ConnectionAccepted(const ASyncResult: IAsyncResult);
    procedure SendToClient;
  protected
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create(const ACommandline, ADir: String);
    destructor Destroy; override;
    procedure SendToServer(Bytes: TBytes);
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property TransportType: TTransportType read FTransportType
      write FTransportType;
    property OnExit: TExitServerEvent read FOnExit write FOnExit;
    property OnReadFromServer: TReadFromServerEvent
      read FOnReadFromServer write FOnReadFromServer;
    property OnReadErrorFromServer: TReadFromServerEvent
      read FOnReadErrorFromServer write FOnReadErrorFromServer;
    property OnConnected: TNotifyEvent read FOnConnected
      write FOnConnected;
  end;

const
  FORCED_TERMINATION = $FE;
  AcceptTimeout = 5000;

implementation

uses
  System.StrUtils,
  XLSPUtils;

procedure TLSPExecuteServerThread.ConnectionAccepted(
  const ASyncResult: IAsyncResult);
begin
  FLspSocket := FSocket.EndAccept(ASyncResult);
  FAcceptEvent.SetEvent;
end;

constructor TLSPExecuteServerThread.Create(const ACommandline, ADir: String);
const
  // Matches the content length allowing for multiple headers
  // and ending with two line breaks
  ContentHeaderRE = '^(?:[^\r\n]+\r\n)*' +
                    'Content-Length:\s*(?P<length>\d+)\r\n' +
                    '(?:[^\r\n]+\r\n)*\r\n';
var
  Path: array[0..MAX_PATH] of WideChar;
begin
  inherited Create(True);
  ExpandEnvironmentStringsW(PWideChar(ACommandline), @Path, MAX_PATH);
  FCommandline := Path;
  if ADir <> '' then
  begin
    ExpandEnvironmentStringsW(PWideChar(ADir), @Path, MAX_PATH);
    FDir := Path;
  end;
  Priority := tpNormal;
  FWriteLock.Initialize;
  FAcceptEvent := TSimpleEvent.Create(nil, False, False, '');
  FWriteEvent := TSimpleEvent.Create(nil, False, False, '');

  FContentHeaderRE := CompiledRegEx(ContentHeaderRE, [roNotEmpty, roMultiLine], False);
end;

destructor TLSPExecuteServerThread.Destroy;
begin
  // The inherited destructor calls Termainate and waits
  inherited;
  FWriteLock.Destroy;
  FAcceptEvent.Free;
  FWriteEvent.Free;
  FLspSocket.Free;
  FSocket.Free;
end;


{ TLSPExecuteServerThread }

procedure TLSPExecuteServerThread.Execute;
begin
  FreeOnTerminate := True;
  if FTransportType = ttStdIO then
    RunServer
  else
    RunServerThroughSocket;
end;

procedure TLSPExecuteServerThread.ExtractAndSendResponceMessages(Bytes: TBytes);
var
  Content: string;
  Match: TMatch;
  BodyLen: Integer;
begin
  FReadBytes := FReadBytes + Bytes;

  Content := TEncoding.UTF8.GetString(FReadBytes);

  Match := fContentHeaderRE.Match(Content);
  if Match.Success then
  begin
    BodyLen := StrToInt(Match.Groups['length'].Value);
    // Match.Length should be equal to the bytes count
    // since the header contains only ascii characters
    if BodyLen + Match.Length <= Length(FReadBytes) then
    begin
      // Send just the JSON - strings are 1-based
      FJson := Copy(Content, Match.Index + Match.Length, BodyLen);
      // keep the additional bytes if any
      FReadBytes := Copy(FReadBytes, Match.Index + Match.Length + BodyLen - 1);
      SendToClient;
    end;
  end
  else
  begin
    // Otherwise send everything for logging
    FJson := Content;
    FReadBytes := [];
    SendToClient;
  end;
end;

type
  TOnReadProc = procedure(Bytes: TBytes) of object;

  PExtOverlapped = ^TExtOverlapped;
  TExtOverlapped = record
    Overlapped: TOverlapped;
    ServerThread: TLSPExecuteServerThread;
    Buffer: TBytes;
    OnReadProc: TOnReadProc;
    PipeHandle: THandle;
  end;

// Completion routine for ReadFileEx
procedure ReadCompletionRoutine(dwErrorCode: DWORD; dwNumberOfBytesTransfered: DWORD;
  lpOverlapped: POverlapped); stdcall;
begin
  // Check for errors or pipe closure
  if dwErrorCode <> 0 then
  begin
    if not PExtOverlapped(lpOverlapped).ServerThread.Terminated then
    begin
      OutputDebugString(PChar(
        Format('ReadCompletionRoutine called with dwErrorCode: %d',
        [dwErrorCode])));
      PExtOverlapped(lpOverlapped).ServerThread.Terminate;
    end;
    Exit;
  end;

  // Process received data
  if dwNumberOfBytesTransfered > 0 then
    PExtOverlapped(lpOverlapped).OnReadProc(
      Copy(PExtOverlapped(lpOverlapped).Buffer, 0, dwNumberOfBytesTransfered));

  ZeroMemory(lpOverlapped, SizeOf(TOverlapped));

  // Issue another read
  if not ReadFileEx(
    PExtOverlapped(lpOverlapped).PipeHandle,
    @PExtOverlapped(lpOverlapped).Buffer[0],
    Length(PExtOverlapped(lpOverlapped).Buffer),
    lpOverlapped,
    @ReadCompletionRoutine) and
    not PExtOverlapped(lpOverlapped).ServerThread.Terminated
  then
    PExtOverlapped(lpOverlapped).ServerThread.Terminate;
end;

procedure TLSPExecuteServerThread.RunServer;
const
  BUFFER_SIZE = 65536;
  ERROR_BUFFER_SIZE = 4096;
var
  StartupInfo: TStartupInfo;
  SecurityAttributes: TSecurityAttributes;
  ReadHandle, WriteHandle: THandle;
  ErrorReadHandle, ErrorWriteHandle: THandle;
  StdInReadPipe, StdInWriteTmpPipe, StdInWritePipe: THandle;
  dBytesWrite: DWORD;
  ExtOverlapped, ExtOverlappedError: TExtOverlapped;
  WaitHandles: TArray<THandle>;
  WaitResult: DWORD;
begin
  FExitcode := 0;

  SecurityAttributes.nLength := sizeof(SECURITY_ATTRIBUTES);
  SecurityAttributes.lpSecurityDescriptor := nil;
  SecurityAttributes.bInheritHandle := True;


  StdInWritePipe := 0;
  ReadHandle := 0;
  WriteHandle := 0;
  try
  // Create pipe for writing
    if not SafeCreatePipe(StdInReadPipe, StdInWriteTmpPipe, @SecurityAttributes, 0) then
      RaiseLastOSError;

    if not SafeDuplicateHandle(GetCurrentProcess, StdInWriteTmpPipe, GetCurrentProcess,
      @StdInWritePipe, 0, False, DUPLICATE_SAME_ACCESS)
    then
      RaiseLastOSError;

    // Create async pipe for reading stdout
    if not CreateAsyncPipe(ReadHandle, WriteHandle, @SecurityAttributes, BUFFER_SIZE) then
      RaiseLastOSError;

    // Create async pipe for reading stderror
    if not CreateAsyncPipe(ErrorReadHandle, ErrorWriteHandle, @SecurityAttributes, BUFFER_SIZE) then
      RaiseLastOSError;
  except
    SafeCloseHandle(StdInReadPipe);
    SafeCloseHandle(StdInWriteTmpPipe);
    SafeCloseHandle(StdInWritePipe);
    SafeCloseHandle(ReadHandle);
    SafeCloseHandle(WriteHandle);

    raise;
  end;

  try
    ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
    with StartupInfo do
    begin
      cb := SizeOf(StartupInfo);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := StdInReadPipe;
      hStdOutput := WriteHandle;
      hStdError :=  ErrorWriteHandle;
    end;

    // Run LSP server
    try
      if not CreateProcess(nil, PChar(FCommandline), nil, nil, True,
        NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW, nil, PChar(FDir),
        StartupInfo, FProcessInformation)
      then
        RaiseLastOSError;
    finally
      // Close handles no longer needed ASAP
      SafeCloseHandle(WriteHandle);  // Has been duplicated by CreateProcess
      SafeCloseHandle(ErrorWriteHandle);  // Has been duplicated by CreateProcess
      SafeCloseHandle(StdInReadPipe); // Has been duplicated by CreateProcess
    end;
    CloseHandle(FProcessInformation.hThread); // Not needed

    // Asynchronous read from stdout
    ExtOverlapped.ServerThread := Self;
    SetLength(ExtOverlapped.Buffer, BUFFER_SIZE);
    ExtOverlapped.PipeHandle := ReadHandle;
    ExtOverlapped.OnReadProc := ExtractAndSendResponceMessages;
    ZeroMemory(@ExtOverlapped.Overlapped, SizeOf(TOverlapped));

    if not ReadFileEx(
      ReadHandle,
      @ExtOverlapped.Buffer[0],
      BUFFER_SIZE,
      @ExtOverlapped.Overlapped,
      @ReadCompletionRoutine)
    then
      RaiseLastOSError;

    // Asynchronous read from stderror
    ExtOverlappedError.ServerThread := Self;
    SetLength(ExtOverlappedError.Buffer, ERROR_BUFFER_SIZE);
    ExtOverlappedError.PipeHandle := ErrorReadHandle;
    ExtOverlappedError.OnReadProc := ProcessErrorOutput;
    ZeroMemory(@ExtOverlappedError.Overlapped, SizeOf(TOverlapped));

    if not ReadFileEx(
      ErrorReadHandle,
      @ExtOverlappedError.Buffer[0],
      ERROR_BUFFER_SIZE,
      @ExtOverlappedError.Overlapped,
      @ReadCompletionRoutine)
    then
      RaiseLastOSError;

    if Assigned(FOnConnected) then
      FOnConnected(Self);

    // Setup wait handles
    WaitHandles := [FProcessInformation.hProcess, FWriteEvent.Handle];

    repeat
      // Alertable wait so the that read completion interrupts the wait
      WaitResult :=
        WaitForMultipleObjectsEx(2, @WaitHandles[0], False, INFINITE, True);

      case WaitResult of
        WAIT_OBJECT_0: Break;
        WAIT_OBJECT_0 + 1:
          begin
            // Write data to the server
            FWriteLock.Enter;
            try
              if Length(FWriteBytes) > 0 then
              begin
                if not WriteFile(StdInWritePipe, FWriteBytes[0],
                  Length(FWriteBytes), dBytesWrite, nil)
                then
                  Break;
                FWriteBytes := [];
              end;
            finally
              FWriteLock.Leave;
            end;
          end;
          WAIT_IO_COMPLETION: Continue;
        else
          RaiseLastOSError(WaitResult);
      end;
    until Terminated;

    // Close all remaining handles
    GetExitCodeProcess(FProcessInformation.hProcess,FExitCode);
    SafeCloseHandle(FProcessInformation.hProcess);
  finally
    SafeCloseHandle(StdInWritePipe);
    SafeCloseHandle(ReadHandle);
    SafeCloseHandle(ErrorReadHandle);
  end;

  if Assigned(FOnExit) then
    FOnExit(Self,FExitCode);
end;

procedure TLSPExecuteServerThread.ProcessErrorOutput(Bytes: TBytes);
begin
  if Assigned(FOnReadErrorFromServer) then
    try
      // first try UTF8
      FOnReadErrorFromServer(Self, TEncoding.UTF8.GetString(Bytes))
    except
      // fallback to ANSI
      FOnReadErrorFromServer(Self, TEncoding.ANSI.GetString(Bytes));
    end;
end;

procedure TLSPExecuteServerThread.ReceiveFinished(const ASyncResult: IAsyncResult);
var
  Data: TBytes;
begin
  try
    Data := FLspSocket.EndReceiveBytes(ASyncResult);
    if Length(Data) > 0 then
      ExtractAndSendResponceMessages(Data)
    else if not Terminated then
      // 0 Bytes are received only when either side of the socket is closed
      Terminate;
    // Keep on receiving
    if not Terminated then
      FLspSocket.BeginReceive(ReceiveFinished);
  except
    if not Terminated then
      Terminate;
  end;
end;

procedure TLSPExecuteServerThread.RunServerThroughSocket;
var
  StartupInfo: TStartupInfo;
  WaitHandles: TArray<THandle>;
  WaitResult: DWORD;
begin
  FExitcode := 0;

  ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
  with StartupInfo do
  begin
    cb := SizeOf(StartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_HIDE;
  end;

  // Make strings writable
  UniqueString(FCommandLine);
  UniqueString(FDir);

  // Create the socket
  FSocket := TSocket.Create(TSocketType.TCP);

  if FTransportType = ttSocketServer then
    // Try to accept client asynchronously
    try
      FSocket.Listen(FHost, '', Port);
      FSocket.BeginAccept(ConnectionAccepted, AcceptTimeout);
    except
      Exit;
    end;

  // Run LSP server
  if not CreateProcess(nil, PChar(FCommandline), nil, nil, True,
    NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW, nil, PChar(FDir),
    StartupInfo, FProcessInformation) then
  begin
    raise Exception.Create('Could not run language server!');
  end;

  CloseHandle(FProcessInformation.hThread); // Not needed

  if (FTransportType = ttSocketServer) then
  begin
    // Wait for accepted connemction
    if (WaitForSingleObject(FAcceptEvent.Handle, AcceptTimeout) = WAIT_TIMEOUT) or
      not Assigned(FLspSocket)
    then
      Terminate;
  end
  else
  begin
    try
      FSocket.Connect('', FHost, '', FPort);
    except
      Terminate;
    end;
    FLspSocket := FSocket;
    FSocket := nil;
  end;

  if not Terminated then
  begin
    // Receieve asynchrnously
    FLspSocket.ReceiveTimeout := -1; // Infinite
    FLspSocket.BeginReceive(ReceiveFinished);
  end;

  if Assigned(FOnConnected) then
    FOnConnected(Self);

  WaitHandles := [FProcessInformation.hProcess, FWriteEvent.Handle];

  repeat
    WaitResult := WaitForMultipleObjects(2, @WaitHandles[0], False, INFINITE);

    case WaitResult of
      WAIT_OBJECT_0: Break;
      WAIT_OBJECT_0 + 1:
        begin
          // Write data to the server
          FWriteLock.Enter;
          try
            if Length(FWriteBytes) > 0 then
            try
              // send synchronously
              FLspSocket.Send(FWriteBytes)
            except
              Break;
            end;
            FWriteBytes := [];
          finally
            FWriteLock.Leave;
          end;
        end;
        WAIT_IO_COMPLETION: Continue;
      else
        RaiseLastOSError(WaitResult);
    end;
  until Terminated;

  // Close all remaining handles
  GetExitCodeProcess(FProcessInformation.hProcess,FExitCode);
  CloseHandle(FProcessInformation.hProcess);

  if Assigned(FOnExit) then
    FOnExit(Self, FExitCode);
end;

procedure TLSPExecuteServerThread.SendToClient;
begin
  if Assigned(FOnReadFromServer) then
    FOnReadFromServer(Self, FJson);
end;

procedure TLSPExecuteServerThread.SendToServer(Bytes: TBytes);
begin
  FWriteLock.Enter;
  try
    FWriteBytes := FWriteBytes + Bytes;
  finally
    FWriteLock.Leave;
  end;
  FWriteEvent.SetEvent;
end;

procedure TLSPExecuteServerThread.TerminatedSet;
begin
  if FTransportType <> ttStdIO then
  begin
    if Assigned(FLspSocket) and (TSocketState.Connected in FLspSocket.State) then
      FlspSocket.Close(True);
    if Assigned(FSocket) and (TSocketState.Connected in FSocket.State) then
      FSocket.Close(True);
  end;

  // Kill the server when Terminate is called
  if Started and not Finished then
    TerminateProcess(FProcessInformation.hProcess, FORCED_TERMINATION);
  inherited;
end;

initialization
  //ReportMemoryLeaksOnShutdown := True;
end.
