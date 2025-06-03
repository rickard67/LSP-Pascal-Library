(*
 *                       Pascal LSP Client
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied.
 *
 * Unit owner : Rickard Johansson <support@rj-texted.se>
 * Web site   : https://www.rj-texted.se
 * Github     : https://github.com/rickard67/LSP-Pascal-Library
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 * Copyright © 2021 Rickard Johansson. All rights reserved.
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
    FWriteEvent: TSimpleEvent;
    FPort: Integer;
    FProcessInformation: TProcessInformation;
    FSocket: TSocket;
    FUseSocket: Boolean;
    FContentHeaderRE: TRegEx;
    FWriteLock: TRTLCriticalSection;
    FOnReadFromServer: TReadFromServerEvent;
    FOnReadErrorFromServer: TReadFromServerEvent;
    FOnExit: TExitServerEvent;
    procedure ExtractAndSendResponceMessages(Bytes: TBytes);
    procedure ProcessErrorOutput(Bytes: TBytes);
    procedure RunServer;
    procedure RunServerThroughSocket;
    procedure ReceiveFinished(const ASyncResult: IAsyncResult);
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
    property UseSocket: Boolean read FUseSocket write FUseSocket;
    property OnExit: TExitServerEvent read FOnExit write FOnExit;
    property OnReadFromServer: TReadFromServerEvent
      read FOnReadFromServer write FOnReadFromServer;
    property OnReadErrorFromServer: TReadFromServerEvent
      read FOnReadErrorFromServer write FOnReadErrorFromServer;
  end;

const
  FORCED_TERMINATION = $FE;

implementation

uses
  System.StrUtils,
  XLSPUtils;

constructor TLSPExecuteServerThread.Create(const ACommandline, ADir: String);
const
  // Matches the content length allowing for multiple headers
  // and ending with two line breaks
  ContentHeaderRE = '^(?:[^\r\n]+\r\n)*' +
                    'Content-Length:\s*(?P<length>\d+)\r\n' +
                    '(?:[^\r\n]+\r\n)*\r\n';
begin
  inherited Create(True);
  FUseSocket := False;
  FCommandline := ACommandline;
  FDir := ADir;
  Priority := tpNormal;
  FWriteLock.Initialize;
  FWriteEvent := TSimpleEvent.Create(nil, False, False, '');

  FContentHeaderRE := CompiledRegEx(ContentHeaderRE, [roNotEmpty, roMultiLine], False);
end;

destructor TLSPExecuteServerThread.Destroy;
begin
  // The inherited destructor calls Termainate and waits
  inherited;
  FWriteLock.Destroy;
  FWriteEvent.Free;
  FreeAndNil(FSocket);
end;


{ TLSPExecuteServerThread }

procedure TLSPExecuteServerThread.Execute;
begin
  FreeOnTerminate := True;
  if FUseSocket then
    RunServerThroughSocket
  else
    RunServer;
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
      PExtOverlapped(lpOverlapped).ServerThread.Terminate;
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

  // Create pipe for writing
  if not CreatePipe(StdInReadPipe, StdInWriteTmpPipe, @SecurityAttributes, 0) then
    RaiseLastOSError;
  if not  DuplicateHandle(GetCurrentProcess, StdInWriteTmpPipe, GetCurrentProcess,
    @StdInWritePipe, 0, False, DUPLICATE_SAME_ACCESS or DUPLICATE_CLOSE_SOURCE) then
  begin
    CloseHandle(StdInReadPipe);
    CloseHandle(StdInWriteTmpPipe);
    RaiseLastOSError;
  end;

  // Create async pipe for reading stdout
  if not CreateAsyncPipe(ReadHandle, WriteHandle, @SecurityAttributes, BUFFER_SIZE) then
  begin
    CloseHandle(StdInReadPipe);
    RaiseLastOSError;
  end;

  // Create async pipe for reading stderror
  if not CreateAsyncPipe(ErrorReadHandle, ErrorWriteHandle, @SecurityAttributes, BUFFER_SIZE) then
  begin
    CloseHandle(StdInReadPipe);
    CloseHandle(WriteHandle);
    RaiseLastOSError;
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

    // Make strings writable
    UniqueString(FCommandLine);
    UniqueString(FDir);

    // Run LSP server
    if not CreateProcess(nil, PChar(FCommandline), nil, nil, True,
      NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW, nil, PChar(FDir),
      StartupInfo, FProcessInformation)
    then
      RaiseLastOSError;

    // Close handles no longer needed ASAP
    CloseHandle(WriteHandle);  // Has been duplicated by CreateProcess
    CloseHandle(ErrorWriteHandle);  // Has been duplicated by CreateProcess
    CloseHandle(StdInReadPipe); // Has been duplicated by CreateProcess
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
    CloseHandle(FProcessInformation.hProcess);
  finally
    CloseHandle(StdInWritePipe);
    CloseHandle(ReadHandle);
    CloseHandle(ErrorReadHandle);
  end;

  if Assigned(FOnExit) then
    FOnExit(Self,FExitCode);
end;

procedure TLSPExecuteServerThread.ProcessErrorOutput(Bytes: TBytes);
begin
  if Assigned(FOnReadErrorFromServer) then
    FOnReadErrorFromServer(Self, TEncoding.UTF8.GetString(Bytes));
end;

procedure TLSPExecuteServerThread.ReceiveFinished(const ASyncResult: IAsyncResult);
var
  Data: TBytes;
begin
  try
    Data := FSocket.EndReceiveBytes(ASyncResult);
    if Length(Data) > 0 then
      ExtractAndSendResponceMessages(Data)
    else if not Terminated then
      // 0 Bytes are received only when either side of the socket is closed
      Terminate;
    // Keep on receiving
    if not Terminated then
      FSocket.BeginReceive(ReceiveFinished);
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

  // Run LSP server
  if not CreateProcess(nil, PChar(FCommandline), nil, nil, True,
    NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW, nil, PChar(FDir),
    StartupInfo, FProcessInformation) then
  begin
    raise Exception.Create('Could not run language server!');
  end;

  CloseHandle(FProcessInformation.hThread); // Not needed

  // Create the socket
  FSocket := TSocket.Create(TSocketType.TCP);

  // Connect synchronously with default timeout
  FSocket.Connect('', FHost, '', FPort);

  // Receieve asynchrnously
  FSocket.ReceiveTimeout := -1; // Infinite
  FSocket.BeginReceive(ReceiveFinished);

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
              FSocket.Send(FWriteBytes)
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
  if UseSocket and Assigned(FSocket) and (TSocketState.Connected in FSocket.State) then
    FSocket.Close(True);

  // Kill the server when Terminate is called
  if Started and not Finished then
    TerminateProcess(FProcessInformation.hProcess, FORCED_TERMINATION);
  inherited;
end;

end.
