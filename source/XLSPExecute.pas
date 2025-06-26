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
  Classes, Windows, XSuperObject, IdTCPServer, IdContext;

type
  TReadFromServerEvent = procedure(Sender: TObject; const AJson: ISuperObject; const APlainText: string) of object;
  TWriteToServerEvent = procedure(Sender: TObject; var s: RawByteString) of object;
  TExitServerEvent = procedure(Sender: TObject; exitcode: Integer) of object;

  TLSPExecuteServerThread = class(TThread)
  private
    FCommandline: String;
    FDir: String;
    FEnvironment: TStringlist;
    FEnvList: string;
    FExitCode: LongWord;
    FHost: string;
    FInputRaw: RawByteString;
    FJson: ISuperObject;
    FOnExit: TExitServerEvent;
    FOnWriteToServer: TWriteToServerEvent;
    FOutAction: Integer;
    FOutPutRaw: RawByteString;
    FOnReadFromServer: TReadFromServerEvent;
    FOutBuf: TMemoryStream;
    FPlainText: string;
    FPort: Integer;
    FProcessInformation: TProcessInformation;
    FServer: TIdTCPServer;
    FUseSocket: Boolean;
    function ExtractAndSendResponceMessages(const AStr: RawByteString): Boolean;
    procedure RunServer(const ACommandline: String);
    procedure GetDataFromClient;
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure RunServerThroughSocket(const ACommandline: String);
    procedure SendToClient;
  protected
    procedure Execute; override;
  public
    constructor Create(const ACommandline, ADir: String; const AEnvList: string = '');
    destructor Destroy; override;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property UseSocket: Boolean read FUseSocket write FUseSocket;
    property OnExit: TExitServerEvent read FOnExit write FOnExit;
    property OnReadFromServer: TReadFromServerEvent read FOnReadFromServer write FOnReadFromServer;
    property OnWriteToServer: TWriteToServerEvent read FOnWriteToServer write FOnWriteToServer;
  end;

implementation

uses SysUtils, System.StrUtils, IdGlobal;

function SetEnvVarValue(const VarName, VarValue: string): Boolean;
begin
  if Windows.SetEnvironmentVariable(PChar(VarName), PChar(VarValue)) then
    Result := True
  else
    Result := False;
end;

function SetEnvVarValues(const s: string): Boolean;
var
  sn,sv: string;
  i: Integer;
  ls: TStringlist;
begin
  Result := True;
  ls := TStringlist.Create;
  try
    ls.Delimiter := ';';
    ls.DelimitedText := s;
    for i := 0 to ls.Count - 1 do
    begin
      sn := ls.Names[i];
      sv := ls.ValueFromIndex[i];
      if (sn <> '') and (sv <> '') then
        Result := SetEnvVarValue(sn,sv);
    end;
  finally
    ls.Free;
  end;
end;

constructor TLSPExecuteServerThread.Create(const ACommandline, ADir: String; const AEnvList: string = '');
begin
  inherited Create(true);
  FUseSocket := False;
  FCommandline := ACommandline;
  FDir := ADir;
  FEnvList := AEnvList; // Environment variables seperated by ; e.g. 'PROG_STDIO=binary;MODE=2;NAME=foo'
  Priority := tpNormal;
end;

destructor TLSPExecuteServerThread.Destroy;
begin
  if Terminated then TerminateProcess(FProcessInformation.hProcess,4);
  inherited;
end;


{ TLSPExecuteServerThread }

procedure TLSPExecuteServerThread.Execute;
begin
  FreeOnTerminate := True;
  if FEnvList <> '' then SetEnvVarValues(FEnvList);
  if FUseSocket then
    RunServerThroughSocket(FCommandline)
  else
    RunServer(FCommandline);
end;

function TLSPExecuteServerThread.ExtractAndSendResponceMessages(const AStr: RawByteString): Boolean;
const
  CLHeader = 'Content-Length:';
  ContentId = #13#10#13#10+'{';
var
  s: string;
  w: string;
  n,nc,ln,len: Integer;
  cx: Integer;

  function FindHeader(var len: Integer; const cx: Integer): Integer;
  var
    i,j: Integer;
    w: string;
  begin
    // Find the next content-length header and extract
    // the content length.
    Result := 0;
    try
      len := 0;
      i := PosEx(CLHeader,s,cx);
      if i = 0 then Exit;

      j := PosEx(#13#10,s,i);
      if (j > i) then
      begin
        Result := i;
        i := i + 14;
        w := Trim(Copy(s,i+1,j-i-1));
        if w <> '' then
          len := StrToInt(w);
      end;
    except
    end;
  end;

  function FindContent(const hpos: Integer): Integer;
  var
    i: Integer;
  begin
    // Find the content of the request/response.
    Result := PosEx(ContentId, s, hpos);
    if Result > 0 then
      Result := Result + 4;
  end;

  procedure SendAsPlainText(const AStr: string);
  begin
    FPlainText := AStr;
    FJson := nil;
    Synchronize(SendToClient);
  end;
begin
  Result := True;
  s := UTF8ToString(AStr);

  cx := 1;
  n := 1;
  while n > 0 do
  begin
    nc := 0;
    ln := 0;
    n := FindHeader(ln,cx);
    if (n > 0) and (ln > 0) then
      nc := FindContent(n);

    if nc > 0 then
    begin
      // Find and extract plain text before request
      w := Copy(s,cx,n-cx);
      if Length(w) > 0 then
        SendAsPlainText(w);

      // Extract request
      w := Copy(s,nc,ln);
      if Length(w) > 0 then
      begin
        FPlainText := w;
        try
          FJson := TSuperObject.Create(w);
        except
          FJson := nil;
        end;
        Synchronize(SendToClient);
      end;
      cx := nc + ln;
    end
    else
    begin
      // We didn't find content so send the rest as plain text
      // to the client.
      w := Copy(s,cx,Length(s));
      SendAsPlainText(w);
      Break;
    end;
    FPlainText := '';
  end;
end;

procedure TLSPExecuteServerThread.RunServer(const ACommandline: String);
var
  SI: TStartupInfo;
  SA: TSecurityAttributes;
  SD: PSECURITY_DESCRIPTOR;
  outputreadtmp,outputwrite,errorwrite,inputRead,inputWritetmp: THandle;
  read_stdout, write_stdin: THandle;
  WasOK: Boolean;
  Buffer: RawByteString;
  dBytesRead: DWORD;
  dBytesWrite: DWORD;
  dAvailable: DWORD;
  LoopExit: DWORD;
  i: Integer;
  ln: Cardinal;
begin
  FExitcode := 0;
  LoopExit := 0;
  try
    with SA do
    begin
      GetMem(SD, sizeof(SECURITY_DESCRIPTOR));
      InitializeSecurityDescriptor(SD, SECURITY_DESCRIPTOR_REVISION);
      SetSecurityDescriptorDacl(SD, True, nil, False);
      lpSecurityDescriptor := SD;
      nLength := sizeof(SECURITY_ATTRIBUTES);
      bInheritHandle := True;
    end;

    // Create pipes
    if not(CreatePipe(outputreadtmp, outputwrite, @SA, 0)) then
      raise Exception.Create('Could not create pipe!');

    if not(DuplicateHandle(GetCurrentProcess, outputwrite, GetCurrentProcess,
      @errorwrite, 0, True, DUPLICATE_SAME_ACCESS)) then
      raise Exception.Create('Could not create pipe!');

    if not(CreatePipe(inputRead, inputWritetmp, @SA, 0)) then
      raise Exception.Create('Could not create pipe!');

    if not(DuplicateHandle(GetCurrentProcess, outputreadtmp,
      GetCurrentProcess, @read_stdout, 0, False, DUPLICATE_SAME_ACCESS)) then
      raise Exception.Create('Could not create pipe!');

    if not(DuplicateHandle(GetCurrentProcess, inputWritetmp,
      GetCurrentProcess, @write_stdin, 0, False, DUPLICATE_SAME_ACCESS)) then
      raise Exception.Create('Could not create pipe!');

    if not CloseHandle(outputreadtmp) then
      raise Exception.Create('Could not create pipe!');
    if not CloseHandle(inputWritetmp) then
      raise Exception.Create('Could not create pipe!');

    GetStartupInfo(SI);
    with SI do
    begin
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := inputRead;
      hStdOutput := outputwrite;
      hStdError := errorwrite;
    end;


    // Run LSP server
    SetCurrentDir(FDir);
    if not CreateProcess(nil, PChar(ACommandline), nil, nil, True, NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW, nil, PChar(FDir), SI, FProcessInformation) then
    begin
      raise Exception.Create('Could not run language server!');
    end;

    if not CloseHandle(outputwrite) then
      raise Exception.Create('Could not create pipe!');
    if not CloseHandle(inputRead) then
      raise Exception.Create('Could not create pipe!');
    if not CloseHandle(errorwrite) then
      raise Exception.Create('Could not create pipe!');
    sleep(500);
    try
      FInputRaw := '';
      FOutputRaw := '';
      repeat
        WasOK := True;

        // See if data is available from the server
        PeekNamedPipe(read_stdout, nil, 0, nil, @dAvailable, nil);
        while (dAvailable > 0) and WasOK do
        begin
          // Set buffer length
          if Cardinal(Length(Buffer)) < dAvailable then
            SetLength(Buffer, dAvailable);
          ln := 0;
          repeat
            // Read buffer
            WasOK := ReadFile(read_stdout, Buffer[1], dAvailable, dBytesRead, nil);

            // Did we read anything?
            if dBytesRead > 0 then
            begin
              // Combine the buffer with the rest of the previous runs
              FInputRaw := FInputRaw + Copy(Buffer,1,dBytesRead);
              Inc(ln, dBytesRead);
            end;
          until (ln = dAvailable) or (dBytesRead = 0);

          // Sometimes the server send large data amounts in chunks, so
          // we need to make sure we received everything.
          sleep(200);
          PeekNamedPipe(read_stdout, nil, 0, nil, @dAvailable, nil);
        end;

        // Done reading. Send server output to the client
        if FInputRaw <> '' then
        begin
          // Make sure we don't jump out of this loop if an exception occur.
          try
            ExtractAndSendResponceMessages(FInputRaw);
          except
            on E: Exception do
            begin
              // Send error message to client
              FPlainText := E.ClassName + ' : ' + E.Message;
              FJson := nil;
              Synchronize(SendToClient);
            end;
          end;
          FInputRaw := '';
          sleep(20);
        end;

        // Write data to the server
        Synchronize(GetDataFromClient);
        if (FOutputRaw <> '') then
        begin
          WriteFile(write_stdin, PRawByteString(FOutputRaw)^, Length(FOutputRaw), dBytesWrite, nil);
          FOutputRaw := '';
        end;

        sleep(20);
        if WaitForSingleObject(FProcessInformation.hProcess, 0) = WAIT_OBJECT_0 then
        begin
          // The server has terminated (or crashed)
          Break;
        end;
      until Terminated;
    finally
      TerminateProcess(FProcessInformation.hProcess, 0);

      // Close all remaining handles
      GetExitCodeProcess(FProcessInformation.hProcess,FExitCode);
      CloseHandle(FProcessInformation.hThread);
      CloseHandle(FProcessInformation.hProcess);
    end;
  finally
    CloseHandle(write_stdin);
    CloseHandle(read_stdout);
  end;

  if Assigned(FOnExit) then
    FOnExit(Self,FExitCode);
end;

procedure TLSPExecuteServerThread.GetDataFromClient;
var
  s: RawByteString;
begin
  if Assigned(FOnWriteToServer) then
    FOnWriteToServer(Self, s);
  FOutPutRaw := FOutPutRaw + s;
end;

procedure TLSPExecuteServerThread.IdTCPServerExecute(AContext: TIdContext);
var
  buf: TIdBytes;
  s: RawByteString;
  LSize,LTotalSize: Integer;
begin
  FInputRaw := '';
  FOutputRaw := '';
  LTotalSize := 0;

  // Read from the LSP server
  while AContext.Connection.IOHandler.Connected do
  begin
    if AContext.Connection.IOHandler.InputBufferIsEmpty then
      AContext.Connection.IOHandler.CheckForDataOnSource(10);
    if not AContext.Connection.IOHandler.InputBufferIsEmpty then
    begin
      LSize := AContext.Connection.IOHandler.InputBuffer.Size;
      LTotalSize := LTotalSize + LSize;
      AContext.Connection.IOHandler.InputBuffer.ExtractToBytes(buf,LSize,True);
    end
    else
      Break;
  end;

  if LTotalSize > 0 then
  begin
    SetLength(s, LTotalSize);
    Move(buf[0], s[1], LTotalSize);
  end;

  // Done reading. Parse the string and send data to the client
  if s <> '' then
  begin
    try
      ExtractAndSendResponceMessages(s);
    except
      on E: Exception do
      begin
        // Send error message to client
        FPlainText := E.ClassName + ' : ' + E.Message;
        FJson := nil;
        Synchronize(SendToClient);
      end;
    end;
    s := '';
    sleep(20);
  end;

  // Check for data to send to the LSP server
  if FOutputRaw = '' then
    Synchronize(GetDataFromClient);

  // Write to the LSP server
  if AContext.Connection.IOHandler.Connected and (FOutputRaw <> '') then
  begin
    SetLength(buf, Length(FOutputRaw));
    Move(FOutputRaw[1], buf[0], Length(FOutputRaw));
    AContext.Connection.IOHandler.Write(buf);
    FOutputRaw := '';
  end;
  sleep(20);
end;

procedure TLSPExecuteServerThread.RunServerThroughSocket(const ACommandline: String);
var
  SI: TStartupInfo;
  SA: TSecurityAttributes;
  SD: PSECURITY_DESCRIPTOR;
  bExit: Boolean;
begin
  FExitcode := 0;
  bExit := False;

  with SA do
  begin
    GetMem(SD, sizeof(SECURITY_DESCRIPTOR));
    InitializeSecurityDescriptor(SD, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(SD, True, nil, False);
    lpSecurityDescriptor := SD;
    nLength := sizeof(SECURITY_ATTRIBUTES);
    bInheritHandle := True;
  end;

  GetStartupInfo(SI);
  with SI do
  begin
    cb := SizeOf(SI);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_HIDE;
  end;

  FServer := TIdTCPServer.Create(nil);
  try
    // Start the TCP server to allow the LSP server to connect (which act as the TCP client)
    FServer.DefaultPort := Port;
    FServer.OnExecute := IdTCPServerExecute;
    FServer.Active := True;
    sleep(100);

    // Run LSP server
    SetCurrentDir(FDir);
    if not CreateProcess(nil, PChar(ACommandline), nil, nil, True, NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW, nil, PChar(FDir), SI, FProcessInformation) then
    begin
      raise Exception.Create('Could not run language server!');
    end;

    repeat
      if not FServer.Active then
      begin
        sleep(200);
        try
          FServer.Active := True;
        except
          Break;
        end;
      end;
      sleep(20);
      if WaitForSingleObject(FProcessInformation.hProcess, 0) = WAIT_OBJECT_0 then
      begin
        // The server has terminated (or crashed)
        FServer.OnConnect := nil;
        FServer.OnExecute := nil;
        Break;
      end;
    until Terminated or not FServer.Active;
  finally
    TerminateProcess(FProcessInformation.hProcess, 0);

    // Close all remaining handles
    GetExitCodeProcess(FProcessInformation.hProcess,FExitCode);
    CloseHandle(FProcessInformation.hThread);
    CloseHandle(FProcessInformation.hProcess);

    if FServer.Active then
    begin
      FServer.OnConnect := nil;
      FServer.OnExecute := nil;
      FServer.Active := False;
    end;
    FServer.Free;
  end;

  if Assigned(FOnExit) then
    FOnExit(Self,FExitCode);
end;

procedure TLSPExecuteServerThread.SendToClient;
begin
  if Assigned(FOnReadFromServer) then
    FOnReadFromServer(Self, FJson, FPlainText);
end;

end.
