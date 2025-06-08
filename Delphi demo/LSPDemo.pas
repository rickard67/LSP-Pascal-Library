unit LSPDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, XLSPTypes, XLSPClient, Vcl.ComCtrls, System.ImageList, Vcl.ImgList,
  Vcl.ToolWin, Vcl.ExtCtrls, System.IniFiles, System.Generics.Collections;

type
  TDiagnosticItem = class
    Range: TLSPRange;
    Severity: TLSPDiagnosticSeverity;
    MessageStr: string;
  end;

  TLSPDemoForm = class(TForm)
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    Panel1: TPanel;
    Memo1: TMemo;
    ToolButton5: TToolButton;
    tbOptions: TToolButton;
    ToolButton7: TToolButton;
    tbStartServer: TToolButton;
    tbCloseServer: TToolButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SendChangesTimer: TTimer;
    ListBox1: TListBox;
    HoverTimer: TTimer;
    HintWindow: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    StatusMemo: TMemo;
    ListBoxDiagnostics: TListBox;
    CompletionTimer: TTimer;
    procedure CompletionTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HoverTimerTimer(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxDiagnosticsDblClick(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1KeyPress(Sender: TObject; var Key: Char);
    procedure Memo1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SendChangesTimerTimer(Sender: TObject);
    procedure tbStartServerClick(Sender: TObject);
    procedure tbOpenClick(Sender: TObject);
    procedure tbOptionsClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure tbCloseServerClick(Sender: TObject);
  private
    FCompletionList: TList<TLSPCompletionItem>;
    FFileName: string;
    FInitDir: string;
    FLoaded: Boolean;
    FLSPClient: TLSPClient;
    FMemoChanges: TObjectList<TLSPBaseTextDocumentContentChangeEvent>;
    FServerArguments: string;
    FServerOptions: string;
    FServerPath: string;
    FServerRootPath: string;
    FX: Integer;
    FY: Integer;
    procedure ApplyChangesToDocument(const newText: string; const range: TLSPRange);
    function GetStringAtCaret: string;
    procedure InsertCompletionItem(const sn: string);
    procedure OnCompletion(Sender: TObject; const Id: Integer; const list:
        TLSPCompletionList);
    procedure OnError(Sender: TObject; const id, errorCode: Integer; const
        errorMsg: string; retriggerRequest: Boolean = False);
    procedure OnExit(Sender: TObject; exitCode: Integer; const bRestartServer: Boolean);
    procedure OnHover(Sender: TObject; const Id: Integer; const value:
        TLSPHoverResult);
    procedure OnInitialize(Sender: TObject; var value: TLSPInitializeParams);
    procedure OnInitialized(Sender: TObject; var value: TLSPInitializeResult);
    procedure OnLogMessages(Sender: TObject; const ntype: TLSPMessageType; const msg: string);
    procedure OnPublishDiagnostics(Sender: TObject; const uri: string; const version: Cardinal; const diagnostics: TArray<TLSPDiagnostic>);
    procedure OnShowMessage(Sender: TObject; const ntype: TLSPMessageType; const msg: string);
    procedure OnShutdown(Sender: TObject);
    procedure OnWorkspaceApplyEdit(Sender: TObject; const value:
        TLSPApplyWorkspaceEditParams; var responseValue:
        TLSPApplyWorkspaceEditResponse; var errorCode: Integer; var errorMessage:
        string);
    function SelectionToRange(const ASelStart, ASelLen: Integer; const ACaretPos: TPoint): TLSPRange;
    procedure SendCompletionRequest(const filename: string);
    procedure SendDidChangeDocumentToServer(const filename: string);
    procedure SendDidOpenDocumentToServer(const filename: string);
    procedure SendDidSaveDocumentToServer(const filename: string);
    procedure SendHoverRequest(const filename: string);
    procedure ShowDocumentRequest(Sender: TObject; const uri: string; const bExternal: Boolean; const bTakeFocus: Boolean; const startPos, endPos: TLSPPosition; var bSuccess: Boolean);
    procedure UpdateCompletionListBox;
  public
    ini: TMemIniFile;
  end;
var
  LSPDemoForm: TLSPDemoForm;

implementation

uses
  System.UITypes,
  System.Types,
  System.Math,
  System.JSON,
  options,
  XLSPFunctions,
  XLSPUtils;

{$R *.dfm}

procedure TLSPDemoForm.FormDestroy(Sender: TObject);
begin
  FMemoChanges.Free;
  FCompletionList.Free;
  ini.Free;
end;

procedure TLSPDemoForm.FormCreate(Sender: TObject);
var
  sz: string;
begin
  FMemoChanges := TObjectList<TLSPBaseTextDocumentContentChangeEvent>.Create;
  sz := ExtractFileDir(Application.ExeName) + '\LSPOptions.ini';
  ini := TMemIniFile.Create(sz);

  // Set options
  FServerPath := ini.ReadString('LSP','ServerPath','');
  FServerArguments := ini.ReadString('LSP','ServerArguments','');
  FInitDir := ini.ReadString('LSP','InitDir','');
  FServerRootPath := ini.ReadString('LSP','ServerRootPath','');
  FServerOptions := StringReplace(ini.ReadString('LSP','ServerOptions',''), '|', #13#10, [rfReplaceAll]);

  FCompletionList := TList<TLSPCompletionItem>.Create;

  FLSPClient := TLSPClient.Create(Self);
  FLSPClient.OnInitialize := OnInitialize;
  FLSPClient.OnInitialized := OnInitialized;
  FLSPClient.OnWorkspaceApplyEdit := OnWorkspaceApplyEdit;
  FLSPClient.OnShowDocument := ShowDocumentRequest;
  FLSPClient.OnShowMessage := OnShowMessage;
  FLSPClient.OnLogMessage := OnLogMessages;
  FLSPClient.OnCompletion := OnCompletion;
  FLSPClient.OnPublishDiagnostics := OnPublishDiagnostics;
  FLSPClient.OnHover := OnHover;
  FLSPClient.OnError := OnError;
  FLSPClient.OnShutdown := OnShutdown;
  FLSPClient.OnExit := OnExit;

  sz := ExtractFileDir(Application.ExeName) + '\LSPLog.txt';
  FLSPClient.LogFileName := sz;
  FLSPClient.LogToFile := True;

  FX := 0;
  FY := 0;
  FLoaded := False;
  tbCloseServer.Enabled := False;
  PageControl1.ActivePageIndex := 0;
end;

procedure TLSPDemoForm.OnCompletion(Sender: TObject; const Id: Integer; const
    list: TLSPCompletionList);
var
  r: LongInt;
  pt: TPoint;
  item,obj: TLSPCompletionItem;
begin
  if not ListBox1.Visible then
  begin
    r := Memo1.Perform(EM_POSFROMCHAR, Memo1.SelStart, 0);
    pt.Y := HiWord(r) + Memo1.Top + Toolbar1.Height;
    pt.X := LoWord(r) + Memo1.Left;
    ListBox1.Left := pt.X;
    ListBox1.Top := pt.Y + 20;
    if ListBox1.Top < 0 then ListBox1.Top := 1;
    if ListBox1.Top + ListBox1.Height > Memo1.Height then ListBox1.Top := Memo1.Height - ListBox1.Height - 1;

    ListBox1.Visible := True;

    HintWindow.Width := Memo1.Width div 2;
    HintWindow.Height := Memo1.Height div 3;
    HintWindow.Left := ListBox1.Left + ListBox1.Width + 2;
    HintWindow.Top := ListBox1.Top;
    HintWindow.Visible := True;
  end;

  FCompletionList.Clear;
  FCompletionList.AddRange(list.items);
  ListBox1.SetFocus;
  UpdateCompletionListBox;
end;

procedure TLSPDemoForm.OnError(Sender: TObject; const id, errorCode: Integer; const errorMsg: string; retriggerRequest: Boolean = False);
var
  s: string;
begin
  s := (Sender as TLSPClient).GetErrorCodeAsString(errorCode) + ': ' + errorMsg + #13#10;
  StatusMemo.Lines.Add(s);
end;

procedure TLSPDemoForm.OnInitialize(Sender: TObject; var value: TLSPInitializeParams);
var
  s: string;
  ls: TStringList;
begin
  StatusMemo.Lines.Add('Initializing...');

  // Set server options
  s := StringReplace(FServerOptions,#13#10,'',[rfReplaceAll]);
  s := StringReplace(s,#32,'',[rfReplaceAll]);
  value.initializationOptions := s;

  // Set root uri
  value.AddRoot(FServerRootPath);

  // Workspace folders
  ls := TStringList.Create;
  try
    ls.Add(FServerRootPath);
    value.AddWorkspaceFolders(ls);
  finally
    ls.Free;
  end;

  // Capabilities/textDocument/synchronization
  value.capabilities.AddSynchronizationSupport(True,False,False,False);

  // Capabilities/textDocument/publishDiagnostics
  value.capabilities.AddPublishDiagnosticsSupport(True,False,False,False);

  // Capabilities/textDocument/completion
  value.capabilities.AddCompletionSupport(False,True,False,True,True,False,False,
    False,False, [], ['details', 'documention']);

  // Capabilities/textDocument/hover
  value.capabilities.AddHoverSupport(False,True,True);

  // Capabilities/textDocument/signatureHelp
  value.capabilities.AddSignatureHelpSupport(False,True,True,False,False);

  // Capabilities/workspace
  value.capabilities.AddWorkspaceCapabilities(True,True,False);

  // Capabilities/workspace/workspaceEdit
  value.capabilities.AddWorkspaceEdit(True,False);

  // Capabilities/workspace/symbol
  value.capabilities.AddWorkspaceSymbol(False);

  // Capabilities/window
  value.capabilities.AddWindowShowMessage(False);

  // Process id
  value.processId := GetCurrentProcessId;
end;

procedure TLSPDemoForm.OnInitialized(Sender: TObject; var value:
    TLSPInitializeResult);
begin
  Memo1.Lines.Clear;
  Memo1.Enabled := False;
  StatusMemo.Lines.Add('Connected to server!');
  tbStartServer.Enabled := False;
  tbCloseServer.Enabled := True;
  tbOpen.Enabled := True;
end;

procedure TLSPDemoForm.OnLogMessages(Sender: TObject; const ntype: TLSPMessageType; const msg: string);
var
  s: string;
begin
  if ntype = TLSPMessageType.lspMsgError then
    s := 'Error: ' + msg
  else if ntype = TLSPMessageType.lspMsgWarning then
    s := 'Warning: ' + msg
  else if ntype = TLSPMessageType.lspMsgInfo then
    s := 'Info: ' + msg
  else if ntype = TLSPMessageType.lspMsgLog then
    s := 'Log: ' + msg;
  StatusMemo.Lines.Add(s);
end;

procedure TLSPDemoForm.OnPublishDiagnostics(Sender: TObject; const uri: string; const version: Cardinal;
  const diagnostics: TArray<TLSPDiagnostic>);
var
  i: Integer;
  s,ws: string;
  item: TDiagnosticItem;
begin
  // Only display diagnostics for the currently opened file
  s := UriToFilePath(uri);
  if (s = '') or (FFileName = '') or (LowerCase(s) <> LowerCase(FFileName)) then Exit;

  ListBoxDiagnostics.Items.BeginUpdate;
  try
    ListBoxDiagnostics.Items.Clear;
    for i := 0 to Length(diagnostics) - 1 do
    begin
      item := TDiagnosticItem.Create;
      item.Range := diagnostics[i].range;
      item.Severity := diagnostics[i].severity;
      item.MessageStr := diagnostics[i].message;

      // Construct display line
      case item.Severity of
        1: ws := '[Error]';
        2: ws := '[Warning]';
        3: ws := '[Information]';
        4: ws := '[Hint]';
        else
          ws := '';
      end;
      s := ws + ' Line: ' + IntToStr(item.Range.start.line) + ' - ' + item.MessageStr;

      ListBoxDiagnostics.Items.AddObject(s, item);
    end;
  finally
    ListBoxDiagnostics.Items.EndUpdate;
  end;
end;

procedure TLSPDemoForm.OnShowMessage(Sender: TObject; const ntype: TLSPMessageType; const msg: string);
var
  mt: TMsgDlgType;
  pt: TPoint;
begin
  case ntype of
    lspMsgError: mt := TMsgDlgType.mtError;
    lspMsgWarning: mt := TMsgDlgType.mtWarning;
    lspMsgInfo: mt := TMsgDlgType.mtInformation;
    else
      mt := TMsgDlgType.mtConfirmation;
  end;
  pt.X := Width div 2 - 200;
  pt.Y := Height div 2 - 100;
  pt := ClientToScreen(pt);
  MessageDlgPos(msg, mt, [mbOK], 0, pt.x, pt.y);
end;

procedure TLSPDemoForm.OnShutdown(Sender: TObject);
begin
  // Shutdown was successful. Send exit notification to close the server.
  (Sender as TLSPClient).NotifyServer(lspExit);
end;

procedure TLSPDemoForm.OnWorkspaceApplyEdit(Sender: TObject; const value: TLSPApplyWorkspaceEditParams;
  var responseValue: TLSPApplyWorkspaceEditResponse; var errorCode: Integer; var errorMessage: string);
var
  i,j: Integer;
  change: TLSPBaseParams;
  edit: TLSPAnnotatedTextEdit;
begin
  if not Assigned(value) then Exit;
  if not Assigned(value.edit) then Exit;

  if Length(value.edit.documentChanges) > 0 then
  begin
    // Process changes
    for i := 0 to Length(value.edit.documentChanges) - 1 do
    begin
      change := value.edit.documentChanges[i];
      if change is TLSPTextDocumentEdit then
      begin
        // (Retrieve document path). We can skip this part since we only have one document open.
        // sz := FLSPClient.UriToFilePath(TLSPTextDocumentEdit(change).textDocument.uri);

        // Get all changes to the document
        for j := 0 to Length(TLSPTextDocumentEdit(change).edits) - 1 do
        begin
          edit := TLSPTextDocumentEdit(change).edits[j];

          // Apply changes
          ApplyChangesToDocument(edit.newText, edit.range);
        end;
      end;
    end;
    responseValue.applied := True;
  end;
end;

procedure TLSPDemoForm.ShowDocumentRequest(Sender: TObject; const uri: string; const bExternal, bTakeFocus: Boolean;
  const startPos, endPos: TLSPPosition; var bSuccess: Boolean);
var
  s: string;
begin
  s := 'uri: ' + uri + #13#10;
  s := s + 'external: ' + BoolToStr(bExternal) + #13#10;
  s := s + 'takeFocus: ' + BoolToStr(bTakeFocus) + #13#10;
  s := s + 'startPos: ' + 'line=' + IntToStr(startPos.line) + ', character=' + IntToStr(startPos.character) + #13#10;
  s := s + 'endPos: ' + 'line=' + IntToStr(endPos.line) + ', character=' + IntToStr(endPos.character) + #13#10;
  StatusMemo.Lines.Text := s;
end;

procedure TLSPDemoForm.ApplyChangesToDocument(const newText: string; const range: TLSPRange);
var
  i, len: Integer;
begin
  if range.start.line = range.&end.line then
  begin
    // Single line

    // Length of selection
    len := range.&end.character - range.start.character;

    // Set selection in memo
    Memo1.CaretPos := Point(range.start.character, range.start.line);
    Memo1.SelLength := len;

    // Replace selection
    Memo1.SetSelText(newText);
  end
  else
  begin
    // Multiple lines

    // Calculate the length of the selection
    len := Length(Memo1.Lines[range.start.line]) - Integer(range.start.character);
    len := len + Integer(range.&end.character) + 2;

    if range.&end.line - 1 >= range.start.line + 1 then
    begin
      for i := range.start.line + 1 to range.&end.line - 1 do
        len := len + Length(Memo1.Lines[i]) + 2;
    end;

    // Set selection in memo
    Memo1.CaretPos := Point(range.start.character, range.start.line);
    Memo1.SelLength := len;

    // Replace selection
    Memo1.SetSelText(newText);
  end;
end;

procedure TLSPDemoForm.CompletionTimerTimer(Sender: TObject);
begin
  CompletionTimer.Enabled := False;
  SendCompletionRequest(FFilename);
end;

procedure TLSPDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FLSPClient.Initialized) and (FLSPClient.Id = 'Omnisharp') then
    FLSPClient.ExitServer(True);
end;

procedure TLSPDemoForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (SSCtrl in Shift) and (Key = VK_SPACE) then
  begin
    Key := 0;
  end;
end;

procedure TLSPDemoForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (GetKeyState(VK_CONTROL) < 0) and (Key = #32) then Key := #0;
end;

procedure TLSPDemoForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (SSCtrl in Shift) and (Key = VK_SPACE) then
  begin
    // Completion invoked
    CompletionTimer.Enabled := True;
    Key := 0;
  end;
end;

function TLSPDemoForm.GetStringAtCaret: string;
var
  s: string;
  cx,n: Integer;
begin
  Result := '';
  s := Memo1.Lines[Memo1.CaretPos.Y];
  cx := Memo1.CaretPos.X + 1;

  n := cx;
  while (n > 0) and CharInSet(s[n], ['a'..'z','A'..'Z','-','_']) do Dec(n);
  Inc(n);

  Result := Copy(s, n, cx - n);
end;

procedure TLSPDemoForm.HoverTimerTimer(Sender: TObject);
begin
  HoverTimer.Enabled := False;
  SendHoverRequest(FFilename);
end;

procedure TLSPDemoForm.InsertCompletionItem(const sn: string);
const
  csym: string = '()[]{},.;:"=<>+/\?!|¨_¤&´`^*%' + #39 + #9 + #32 + #160;
var
  s: string;
  x,z: Integer;
  selStart,selLength: Integer;
  CaretPos: TPoint;
  change: TLSPTextDocumentContentChangeEvent;
begin
  HintWindow.Visible := False;
  s := Memo1.Lines[Memo1.CaretPos.Y];

  // Identify word at cursor pos and replace. E.g. replace cur| with cursor
  x := Memo1.CaretPos.X;
  z := x + 1;
  while x > 0 do
  begin
    if Pos(s[x], csym) > 0 then Break;
    Dec(x);
  end;

  while z <= Length(s) do
  begin
    if Pos(s[z], csym) > 0 then Break;
    Inc(z);
  end;
  Dec(z);

  // Set selection and replace
  Memo1.CaretPos := Point(x, Memo1.CaretPos.Y);
  Memo1.SelLength := z - x;
  selStart := Memo1.SelStart;
  selLength := Memo1.SelLength;
  CaretPos := Memo1.CaretPos;
  Memo1.SetSelText(sn);

  // Update changes for the server
  change := TLSPTextDocumentContentChangeEvent.Create;
  change.text := sn;
  change.range := SelectionToRange(SelStart, SelLength, CaretPos);
  FMemoChanges.Add(change);
end;

procedure TLSPDemoForm.ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  cx,cy: Integer;
  item: TLSPCompletionItem;
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    item := FCompletionList[ListBox1.ItemIndex];
    InsertCompletionItem(item.&label);
  end
  else if (Key in [VK_LEFT,VK_RIGHT]) then
  begin
    cy := Memo1.CaretPos.Y;
    cx := Memo1.CaretPos.X;
    if Key = VK_LEFT then
      Dec(cx)
    else
      Inc(cx);

    Memo1.CaretPos := Point(cx,cy);
    UpdateCompletionListBox;
  end;
end;

procedure TLSPDemoForm.ListBox1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Item: TLSPCompletionItem;
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) or (Key = VK_ESCAPE) then
  begin
    HintWindow.Visible := False;
    ListBox1.Visible := False;
    Memo1.SetFocus;
  end
  else if (Key = VK_UP) or (Key = VK_DOWN) then
  begin
    if (ListBox1.Count > 0) and FLSPClient.IsRequestSupported(lspCompletionItemResolve) then
    begin
      var Params := TSmartPtr.Make(TLSPCompletionItemResolveParams.Create)();
      Params.completionItem := FCompletionList[ListBox1.ItemIndex];
      if FLSPClient.SendSyncRequest(lspCompletionItemResolve, Params,
        procedure(Json: TJSONObject)
        begin
          if ResponseError(Json) then Exit;
          Item := TSerializer.Deserialize<TLSPCompletionItem>(Json.Values['result']);
        end, 100) then
      begin
        Label1.Caption := item.detail;
        Label2.Caption := TLSPMarkupContent.FromJsonRaw(item.documentation).value;
      end;
    end;
  end;
end;

procedure TLSPDemoForm.ListBoxDiagnosticsDblClick(Sender: TObject);
var
  index: Integer;
  item: TDiagnosticItem;
begin
  index := ListBoxDiagnostics.ItemIndex;
  if (index < 0) or (index > ListBoxDiagnostics.Items.Count - 1) then Exit;

  item := TDiagnosticItem(ListBoxDiagnostics.Items.Objects[index]);

  // Move to position in code and select item
  Memo1.CaretPos := Point(item.Range.start.character, item.Range.start.line);
  Memo1.SelLength := item.Range.&end.character - item.Range.start.character;
  Memo1.SetFocus;
end;

procedure TLSPDemoForm.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  change: TLSPTextDocumentContentChangeEvent;
  selStart,selLen: Integer;
  pt: TPoint;
begin
  if Key = VK_DELETE then
  begin
    change := TLSPTextDocumentContentChangeEvent.Create;
    change.text := '';
    selStart := Memo1.SelStart;
    selLen := Memo1.SelLength;
    if selLen = 0 then selLen := 1;
    change.range := SelectionToRange(selStart, selLen, Memo1.CaretPos);
    change.rangeLength := 0;
    FMemoChanges.Add(change);
  end
  else if Key = VK_BACK then
  begin
    change := TLSPTextDocumentContentChangeEvent.Create;
    change.text := '';
    selStart := Memo1.SelStart;
    selLen := Memo1.SelLength;
    pt := Memo1.CaretPos;
    pt.X := pt.X - 1;
    if selLen = 0 then
    begin
      Dec(selStart);
      selLen := 1;
    end;
    change.range := SelectionToRange(selStart, selLen, pt);
    change.rangeLength := 0;
    FMemoChanges.Add(change);
  end;
end;

procedure TLSPDemoForm.Memo1KeyPress(Sender: TObject; var Key: Char);
var
  change: TLSPTextDocumentContentChangeEvent;
begin
  if Ord(Key) >= 32 then
  begin
    change := TLSPTextDocumentContentChangeEvent.Create;
    change.text := string(Key);
    change.range := SelectionToRange(Memo1.SelStart, Memo1.SelLength, Memo1.CaretPos);
    change.rangeLength := 0;
    FMemoChanges.Add(change);
  end;

  if CharInSet(Key, ['$','>']) then
  begin
    // Completion invoked
    CompletionTimer.Enabled := True;
  end;
end;

procedure TLSPDemoForm.Memo1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not FLoaded then Exit;
  if HintWindow.Visible and (Abs(FX - X) > 5) and (Abs(FY - Y) > 5) then
  begin
    HintWindow.Visible := False;
    FX := X;
    FY := Y;
  end;

  HoverTimer.Enabled := False;
  HoverTimer.Enabled := True;
end;

procedure TLSPDemoForm.OnExit(Sender: TObject; exitCode: Integer; const bRestartServer: Boolean);
var
  s: string;
begin
  s := 'The server has closed with exit code: ' + IntToStr(exitCode);
  StatusMemo.Lines.Add(s);

  tbStartServer.Enabled := True;
  tbCloseServer.Enabled := False;
  tbOpen.Enabled := False;
end;

procedure TLSPDemoForm.OnHover(Sender: TObject; const Id: Integer; const value:
    TLSPHoverResult);
var
  s,w: string;
  i: Integer;
  bContent: Boolean;
  ls: TStringlist;
  pt,pm: TPoint;
begin
  s := value.contents.value;
  if s = '' then Exit;

  Label1.Caption := '';
  Label2.Caption := '';
  bContent := False;
  ls := TStringlist.Create;
  try
    ls.Text := s;
    for i := 0 to ls.Count - 1 do
    begin
      w := ls[i];
      if Pos('```', w) = 0 then
      begin
        if not bContent then
        begin
          Label1.Caption := ls[i];
          bContent := True;
        end
        else
          Label2.Caption := Label2.Caption + ls[i] + #13#10;
      end;
    end;
  finally
    ls.Free;
  end;

  HintWindow.Width := Memo1.Width div 2;
  HintWindow.Height := Memo1.Height div 3;

  GetCursorPos(pt);
  pm := Memo1.ScreenToClient(pt);
  FX := pm.X;
  FY := pm.Y;
  pt := ScreenToClient(pt);
  pt.X := pt.X - Memo1.Left;
  pt.Y := pt.Y - Memo1.Top - ToolBar1.Height;
  HintWindow.Left := pt.X - 10;
  if pt.Y > 60 then
    HintWindow.Top := pt.Y - HintWindow.Height - 20
  else
    HintWindow.Top := pt.Y + 20;

  if HintWindow.Top < 0 then HintWindow.Top := 1;

  if not HintWindow.Visible then
    HintWindow.Visible := True;;
end;

function TLSPDemoForm.SelectionToRange(const ASelStart, ASelLen: Integer; const ACaretPos: TPoint): TLSPRange;
var
  i,ln,x: Integer;
  len: Integer;
  s: string;
begin
  // Convert memo selection to range
  ln := ACaretPos.Y;
  x := ACaretPos.X;
  Result.start.line := ln;
  Result.start.character := x;
  Result.&end.line := ln;
  Result.&end.character := x;
  i := 0;
  while i < ASelLen do
  begin
    s := Memo1.Lines[ln];
    len := Length(s) - x;
    if i + len >= ASelLen then
    begin
      Result.&end.line := ln;
      Result.&end.character := ASelLen - i + x;
      Break;
    end;
    Inc(i, len + 2); // Line length + linefeed
    Inc(ln);
    x := 0;
  end;
end;

procedure TLSPDemoForm.SendChangesTimerTimer(Sender: TObject);
begin
  SendDidChangeDocumentToServer(FFileName);
end;

procedure TLSPDemoForm.SendCompletionRequest(const filename: string);
var
  params: TLSPCompletionParams;
begin
  // Check if the server support completion requests
  if not FLSPClient.IsRequestSupported(lspCompletion) then Exit;

  // Make sure all changes have been sent to the server
  if FMemoChanges.Count > 0 then
    SendDidChangeDocumentToServer(fileName);

  params := TSmartPtr.Make(TLSPCompletionParams.Create)();
  params.textDocument.uri := FilePathToUri(fileName);
  params.position.line := Memo1.CaretPos.Y;
  params.position.character := Memo1.CaretPos.X;
  FLSPClient.SendRequest(lspCompletion, '', params);
end;

procedure TLSPDemoForm.SendDidChangeDocumentToServer(const filename: string);
var
  syncKind: Integer;
  params: TLSPDidChangeTextDocumentParams;
  content: TLSPBaseTextDocumentContentChangeEvent;
begin
  if FMemoChanges.Count = 0 then Exit;
  if not FLSPClient.IsRequestSupported(lspDidChangeTextDocument) then Exit;

  params := TSmartPtr.Make(TLSPDidChangeTextDocumentParams.Create)();
  params.textDocument.uri := FilePathToUri(filename);
  params.textDocument.version := Memo1.Tag + 1;
  Memo1.Tag := Memo1.Tag + 1;

  syncKind := FLSPClient.GetSyncKind;

  // Set changes
  if syncKind = TLSPTextDocumentSyncKindRec.Incremental then
    params.contentChanges := FMemoChanges.ToArray
  else if syncKind = TLSPTextDocumentSyncKindRec.Full then
  begin
    content := TLSPBaseTextDocumentContentChangeEvent.Create;
    content.text := Memo1.Lines.Text;
    params.contentChanges := [content]; // will be cleared when params is freed
  end;

  // Send request to server
  FLSPClient.NotifyServer(lspDidChangeTextDocument, '', params);

  if syncKind = TLSPTextDocumentSyncKindRec.Incremental then
    params.contentChanges := [];  // To avoid double freeing the changes
  FMemoChanges.Clear;
end;

procedure TLSPDemoForm.SendDidOpenDocumentToServer(const filename: string);
var
  params: TLSPDidOpenTextDocumentParams;
begin
  if not FLSPClient.IsRequestSupported(lspDidOpenTextDocument) then Exit;

  params := TSmartPtr.Make(TLSPDidOpenTextDocumentParams.Create)();
  params.textDocument.uri := FilePathToUri(filename);
  params.textDocument.version := 0;
  Memo1.Tag := 0;

  // Our demo server handles PHP documents
  params.textDocument.languageId := 'php';

  if FLSPClient.IncludeText(lspDidOpenTextDocument, True) then
  begin
    // Send the whole document to the server
    params.textDocument.text := Memo1.text;
  end;

  FLSPClient.NotifyServer(lspDidOpenTextDocument, '', params);
end;

procedure TLSPDemoForm.SendDidSaveDocumentToServer(const filename: string);
var
  params: TLSPDidSaveTextDocumentParams;
begin
  if not FLSPClient.IsRequestSupported(lspDidSaveTextDocument) then Exit;

  params := TLSPDidSaveTextDocumentParams.Create;
  params.textDocument.uri := FilePathToUri(filename);

  // Send the whole document to the server ?
  if FLSPClient.IncludeText(lspDidSaveTextDocument, False) then
    params.text := Memo1.text;

  FLSPClient.NotifyServer(lspDidSaveTextDocument, '', params);
end;

procedure TLSPDemoForm.SendHoverRequest(const filename: string);
var
  params: TLSPHoverParams;
  i,x,n,cx,cy: Integer;
  pt: TPoint;
  r: LongInt;
begin
  // Does the server support hover requests?
  if not FLSPClient.IsRequestSupported(lspHover) then Exit;

  // Get current mouse cursor position as character and line position
  GetCursorPos(pt);
  pt := Memo1.ScreenToClient(pt);
  r := Memo1.Perform(EM_CHARFROMPOS, 0, MakeLong(Pt.X, Pt.Y));
  cy := HiWord(r);
  x := LoWord(r);

  if (x <= 0) or (cy <= 0) then Exit;

  n := 0;
  for i := 0 to cy - 1 do
    n := n + Length(Memo1.Lines[i]) + 2;
  cx := x - n;

  // Check position
  if (cy < 0) or (cy >= Memo1.Lines.Count) then Exit;
  if (cx < 0) or (cx > Length(Memo1.Lines[cy])) then Exit;

  // Send hover request to server
  params := TSmartPtr.Make(TLSPHoverParams.Create)();
  params.textDocument.uri := FilePathToUri(FFileName);
  params.position.line := cy;
  params.position.character := cx;
  FLSPClient.SendRequest(lspHover, '', params);
end;

procedure TLSPDemoForm.tbStartServerClick(Sender: TObject);
var
  sz: string;
begin
  sz := FServerPath;
  if FServerArguments <> '' then
    sz := sz + #32 + FServerArguments;
  FLSPClient.Id := 'Omnisharp';
  FLSPClient.RunServer(sz, FInitDir);
  FLSPClient.SendRequest(lspInitialize);
end;

procedure TLSPDemoForm.tbOpenClick(Sender: TObject);
begin
  if not OpenDialog1.Execute(Handle) then Exit;

  Memo1.Enabled := True;
  FFileName := OpenDialog1.FileName;
  Memo1.Lines.LoadFromFile(FFileName);
  FLoaded := True;

  SendDidOpenDocumentToServer(FFilename);

  PageControl1.ActivePageIndex := 1;
  SendChangesTimer.Enabled := True;
end;

procedure TLSPDemoForm.tbOptionsClick(Sender: TObject);
var
  s: string;
begin
  OptionsForm.EditServer.Text := FServerPath;
  OptionsForm.EditArguments.Text := FServerArguments;
  OptionsForm.EditInitDir.Text := FInitDir;
  OptionsForm.EditRootPath.Text := FServerRootPath;
  OptionsForm.MemoOptions.Text := FServerOptions;

  if OptionsForm.ShowModal = mrCancel then Exit;

  FServerPath := OptionsForm.EditServer.Text;
  FServerArguments := OptionsForm.EditArguments.Text;
  FServerRootPath := OptionsForm.EditRootPath.Text;
  FInitDir := OptionsForm.EditInitDir.Text;
  FServerOptions := OptionsForm.MemoOptions.Text;

  // Save options
  ini.WriteString('LSP','ServerPath',FServerPath);
  ini.WriteString('LSP','ServerArguments',FServerArguments);
  ini.WriteString('LSP','InitDir',FInitDir);
  ini.WriteString('LSP','ServerRootPath',FServerRootPath);
  s := StringReplace(FServerOptions, #13#10, '|', [rfReplaceAll]);
  ini.WriteString('LSP','ServerOptions',s);
  ini.UpdateFile;
end;

procedure TLSPDemoForm.tbSaveClick(Sender: TObject);
begin
  Memo1.Lines.SaveToFile(FFileName);
  SendDidSaveDocumentToServer(FFileName);
end;

procedure TLSPDemoForm.tbCloseServerClick(Sender: TObject);
begin
  SendChangesTimer.Enabled := False;
  FLSPClient.CloseServer;
end;

procedure TLSPDemoForm.UpdateCompletionListBox;
var
  s,sf: string;
  item: TLSPCompletionItem;
begin
  // Get filter string at caret
  sf := GetStringAtCaret;

  // Add items to listbox based on filter
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Clear;
    for item in FCompletionList do
    begin
      s := item.&label;
      if (sf = '') or (Pos(sf, s) = 1) then
      begin
        if item.kind = TLSPCompletionItemKind.cMethod then
          s := 'Method ' + s
        else if item.kind = TLSPCompletionItemKind.cFunction then
          s := 'Function ' + s
        else if item.kind = TLSPCompletionItemKind.cConstructor then
          s := 'Constructor ' + s
        else if item.kind = TLSPCompletionItemKind.cField then
          s := 'Field ' + s
        else if item.kind = TLSPCompletionItemKind.cVariable then
          s := 'Variable ' + s
        else if item.kind = TLSPCompletionItemKind.cClass then
          s := 'Class ' + s
        else if item.kind = TLSPCompletionItemKind.cInterface then
          s := 'Interface ' + s
        else if item.kind = TLSPCompletionItemKind.cModule then
          s := 'Module ' + s
        else if item.kind = TLSPCompletionItemKind.cProperty then
          s := 'Property ' + s
        else if item.kind = TLSPCompletionItemKind.cUnit then
          s := 'Unit ' + s
        else if item.kind = TLSPCompletionItemKind.cValue then
          s := 'Value ' + s
        else if item.kind = TLSPCompletionItemKind.cEnum then
          s := 'Enum ' + s
        else if item.kind = TLSPCompletionItemKind.cKeyword then
          s := 'Keyword ' + s
        else if item.kind = TLSPCompletionItemKind.cSnippet then
          s := 'Snippet ' + s
        else if item.kind = TLSPCompletionItemKind.cColor then
          s := 'Color ' + s
        else if item.kind = TLSPCompletionItemKind.cConstant then
          s := 'Const ' + s;
        ListBox1.Items.Add(s);
      end;
    end;
  finally
    ListBox1.Items.EndUpdate;
  end;

  if ListBox1.Count > 0 then
  begin
    item := FCompletionList[0];
    Label1.Caption := item.detail;
    Label2.Caption := TLSPMarkupContent.FromJsonRaw(item.documentation).value;
  end;
end;

end.
