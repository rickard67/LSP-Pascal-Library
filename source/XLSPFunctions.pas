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
 * Copyright © 2023 Rickard Johansson. All rights reserved.
 *
*)

unit XLSPFunctions;

interface

uses
  System.Generics.Collections,
  System.JSON,
  System.JSON.Serializers,
  XLSPTypes,
  XLSPUtils;

function ResponseError(Json: TJsonObject; var ErrorCode: Integer;
    ErrorMessage: string): Boolean; overload;
function ResponseError(Json: TJsonObject): Boolean; overload;
function CreateJSONRequestParam(const lspKind: TLSPKind; Params:
    TLSPBaseParams): string;
function CreateJSONResponse(const id: Variant; params: TLSPBaseParams; error:
    TLSPResponseError; resultType: TLSPResultType; const resultString: string):
    string;
function GetKindFromMethod(method: string; const id: Integer = -1): Integer;
function GetMethodFromKind(const lspId: TLSPKind): string;
function GetKindFromId(Id: Integer): Integer;
function IsRequest(const lspId: TLSPKind): Boolean;
function JsonCallHierarchyIncommingResultToObject(const ResultJson:
    TJsonValue): TLSPCallHierarchyIncomingCallResult;
function JsonCallHierarchyOutgoingResultToObject(const ResultJson: TJSONValue):
    TLSPCallHierarchyOutgoingCallResult;
function JsonCodeActionResultToObject(const ResultJson: TJSONValue):
    TLSPCodeActionResult;
function JsonCodeActionResolveResultToObject(const ResultJson: TJSONValue):
    TLSPCodeActionResolveResult;
function JsonCodeLensResultToObject(const ResultJson: TJSONValue):
    TLSPCodeLensResult;
function JsonCodeLensResolveResultToObject(const ResultJson: TJSONValue):
    TLSPCodeLensResolveResult;
function JsonColorPresentationResultToObject(const ResultJson: TJSONValue):
    TLSPColorPresentationResult;
function JsonCompletionResultToObject(const ResultJson: TJSONValue):
    TLSPCompletionList;
function JsonCompletionItemResolveResultToObject(const ResultJson: TJSONValue):
    TLSPCompetionItemResolveResult;
function JsonConfigurationParamsToObjects(const ParamsJson: TJSONValue):
    TLSPConfigurationParams;
function JsonDocumentColorResultToObject(const ResultJson: TJSONValue):
    TLSPColorInformationResult;
function JsonDocumentDiagnosticReportToObject(const ResultJson: TJSONValue):
    TLSPRelatedDocumentDiagnosticReport;
function JsonDocumentFormattingResultToObject(const ResultJson: TJSONValue):
    TLSPTextEditResult;
function JsonDocumentHighlightResponseToObject(const ResultJson: TJSONValue):
    TLSPDocumentHighlightResult;
function JsonDocumentSymbolsResultToObject(const ResultJson: TJSONValue):
    TLSPDocumentSymbolsResult;
function JsonDocumentLinkResultToObject(const ResultJson: TJSONValue):
    TLSPDocumentLinkResult;
function JsonDocumentLinkResolveResultToObject(const ResultJson: TJSONValue):
    TLSPDocumentLinkResolveResult;
function JsonExecuteCommandResult(const ResultJson: TJSONValue): string;
function JsonFindReferencesResultToObject(const ResultJson: TJSONValue):
    TLSPFindReferencesResult;
function JsonFoldingRangeResultToObject(const ResultJson: TJSONValue):
    TLSPFoldingRangeResult;
function JsonGotoResultToObject(const ResultJson: TJSONValue):
    TLSPGotoResult;
function JsonHoverResultToObject(const ResultJson: TJSONValue): TLSPHoverResult;
function JsonInitializeResultToObject(const ResultJson: TJsonValue):
    TLSPInitializeResult;
function JsonInlayHintResolveResultToObject(const ResultJson: TJSONValue):
    TLSPInlayHintResolveResult;
function JsonInlayHintResultToObject(const ResultJson: TJSONValue):
    TLSPInlayHintResult;
function JsonInlineValueResultToObject(const ResultJson: TJSONValue):
    TLSPInlineValueResult;
function JsonLinkedEditingRangeResultToObject(const ResultJson: TJsonValue):
    TLSPLinkedEditingRangeResult;
function JsonLogTraceParamsToObject(const ParamsJson: TJsonValue):
    TLSPLogTraceParams;
function JsonMonikerResultToObject(const ResultJson: TJsonValue): TLSPMonikerResult;
function JsonPrepareCallHierarchyResultToObject(const ResultJson: TJsonValue):
    TLSPPrepareCallHierarchyResult;
function JsonPrepareTypeHierarchyResultToObject(const ResultJson: TJsonValue):
    TLSPPrepareTypeHierarchyResult;
function JsonProgressValueToResult(const kind: Integer; const ParamsJson:
    TJSONValue): TLSPBaseResult;
function JsonRegisterCapabilitiesToRegistrations(const ParamsJson: TJSONValue):
    TArray<TLSPRegistration>;
function JsonPrepareRenameResultToObject(const ResultJson: TJSONValue):
    TLSPPrepareRenameResult;
function JsonWorkspaceEditResultToObject(const ResultJson: TJSONValue):
    TLSPWorkspaceEdit;
function JsonSelectionRangeResultToObject(const ResultJson: TJSONValue):
    TLSPSelectionRangeResult;
function JsonSemanticTokensFullResultToObject(const ResultJson: TJSONValue):
    TLSPSemanticTokensResult;
function JsonSemanticTokensFullDeltaResultToObject(const ResultJson: TJSONValue):
    TLSPSemanticTokensDeltaResult;
function JsonShowDocumentRequestParams(const ParamsJson: TJSONValue):
    TLSPShowDocumentParams;
procedure JsonShowMessageParams(const ParamsJson: TJSONValue; var ntype:
    Integer; var msg: string);
function JsonShowMessageRequestParams(const ParamsJson: TJSONValue):
    TLSPShowMessageRequestParams;
function JsonSignatureHelpResultToObject(const ResultJson: TJSONValue):
    TLSPSignatureHelpResult;
function JsonTypeHierarchySupertypesResultToObject(const ResultJson:
    TJSONValue): TLSPPrepareTypeHierarchyResult;
function JsonUnregisterCapabilitiesToUnregistrations(const ParamsJson:
    TJSONValue): TArray<TLSPUnregistration>;
function JsonWillSaveWaitUntilResultToObject(const ResultJson: TJSONValue):
    TArray<TLSPAnnotatedTextEdit>;
procedure JsonWorkDoneProgressRequestParams(const ParamsJson: TJSONValue; var
    token: string);
function JsonWorkspaceApplyEditParamsToObject(const ParamsJson: TJSONValue):
    TLSPApplyWorkspaceEditParams;
function JsonWorkspaceDiagnosticReportToObject(const ResultJson: TJSONValue):
    TLSPWorkspaceDiagnosticReport;
function JsonWorkspaceSymbolResultToObject(const ResultJson: TJSONValue):
    TLSPWorkspaceSymbolInformationResult;
function FilePathToUri(const APath: string): string;
function UriToFilePath(const Uri: string): string;

implementation

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
  Winapi.ShLwApi,
  Winapi.WinInet,
  System.Win.ComObj,
{$ENDIF}
  System.Types,
  System.SysUtils,
  System.Variants,
  System.StrUtils,
  System.NetEncoding;

{$IFDEF MSWINDOWS}
function FilePathToURI(const APath: string): string;
const
  // The use of this flag in UrlCreateFromPath is undocumented.
  // It is used in URLEscape.  Its use fixes #1346.
  URL_ESCAPE_AS_UTF8: DWORD = $00040000;
begin
  var BufferLen: DWORD := INTERNET_MAX_URL_LENGTH;
  SetLength(Result, BufferLen);
  OleCheck(UrlCreateFromPath(PChar(APath), PChar(Result), @BufferLen, URL_ESCAPE_AS_UTF8));
  SetLength(Result, BufferLen);
end;

function URIToFilePath(const URI: string): string;
begin
  var DecodedURI := TURLEncoding.URL.Decode(URI);
  var BufferLen: DWORD := MAX_PATH;
  SetLength(Result, BufferLen);
  OleCheck(PathCreateFromUrl(PChar(DecodedURI), PChar(Result), @BufferLen, 0));
  SetLength(Result, BufferLen);
end;
{$ELSE}
function EncodeUri(const ASrc: string): string;
const
  HexNumbers: string = '0123456789ABCDEF';
var
  i, l: Integer;
  ch: AnsiChar;
  s: RawByteString;
begin
  i := 1;
  l := 1;
  s := UTF8Encode(ASrc);

  SetLength(Result, Length(s) * 3);

  while i <= Length(s) do
  begin
    ch := s[i];
    if CharInSet(ch, ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '~', '.']) then
    begin
      Result[l] := Char(ch);
      Inc(l);
    end
    else if ch = '\' then
    begin
      Result[l] := '/';
      Inc(l);
    end
    else
    begin
      Result[l] := '%';
      Result[l+1] := HexNumbers[(Byte(s[I]) shr 4) + 1];
      Result[l+2] := HexNumbers[(Byte(s[I]) and 15) + 1];
      Inc(l,3);
    end;
    Inc(i);
  end;

  SetLength(Result, l-1);
end;

function DecodeUri(AUri: string): string;
var
  i,l: Integer;
  s: RawByteString;
  ch: AnsiChar;
begin
  Result := '';
  s := UTF8Encode(AUri);
  l := 1;
  i := 1;
  SetLength(Result, Length(s));

  while (i <= Length(s)) do
  begin
    ch := s[i];
    if ch = '%' then
    begin
      Inc(i);
      Result[l] := Char(StrToInt('$' + Char(s[i]) + Char(s[i+1])));
      Inc(i);
      Inc(l);
    end
    else if ch = '/' then
    begin
      Result[l] := '\';
      Inc(l);
    end
    else
    begin
      Result[l] := Char(ch);
      Inc(l);
    end;
    Inc(i);
  end;

  SetLength(Result, l-1);
end;

function FilePathToUri(const APath: string): string;
begin
  Result := 'file:///' + EncodeUri(APath);
end;

function UriToFilePath(const Uri: string): string;
var
  s: string;
  n: Integer;
begin
  Result := '';
  n := Pos('///', Uri);
  if n > 0 then
    s := Copy(Uri, n+3, Length(Uri))
  else
    s := Uri;
  Result := DecodeUri(s);
end;
{$ENDIF MSWINDOWS}

function IsRequest(const lspId: TLSPKind): Boolean;
begin
  Result := lspId in LSPClientRequests;
end;

function LSPInitializeParamsToJSON(const initObj: TLSPInitializeParams): string;
var
  SFull: string;
begin
// Semantic Token
  if Assigned(initObj.capabilities) and
    Assigned(initObj.capabilities.textDocument) and
    Assigned(initObj.capabilities.textDocument.semanticTokens) then
  begin
    case initObj.capabilities.textDocument.semanticTokens.requests.semanticTokensType of
      semtokenFullFalse: SFull := 'false';
      semtokenFull: SFull := 'true';
      semtokenDelta: SFull := '{"delta": true}';
    end;
    initObj.capabilities.textDocument.semanticTokens.requests.full := SFull;
  end;
  Result := initObj.AsJSON([TSerializeOption.IgnoreNil, TSerializeOption.IgnoreEmpty]);
end;

function JsonCallHierarchyIncommingResultToObject(
  const ResultJson: TJSONValue): TLSPCallHierarchyIncomingCallResult;
begin
   Result := TLSPCallHierarchyIncomingCallResult.Create;

  if ResultJson is TJsonArray then
    Result.MemberFromJsonValue('items', ResultJson);
end;

function JsonCallHierarchyOutgoingResultToObject(
  const ResultJson: TJSONValue): TLSPCallHierarchyOutgoingCallResult;
begin
  Result := TLSPCallHierarchyOutgoingCallResult.Create;

  if ResultJson is TJsonArray then
    Result.MemberFromJsonValue('items', ResultJson);
end;

function JsonCodeActionResultToObject(const ResultJson: TJSONValue):
  TLSPCodeActionResult;
begin
  Result := TLSPCodeActionResult.Create;

  if ResultJson is TJsonObject then
    // Command
     Result.MemberFromJsonValue('command', ResultJson)
  else if ResultJson is TJsonArray then
    // codeAction[]
     Result.MemberFromJsonValue('codeActions', ResultJson);
end;

function JsonCodeActionResolveResultToObject(const ResultJson: TJSONValue):
  TLSPCodeActionResolveResult;
begin
  Result := TLSPCodeActionResolveResult.Create;

  if ResultJson is TJsonObject then
    Result.MemberFromJsonValue('codeAction', ResultJson);
end;

function JsonCodeLensResultToObject(const ResultJson: TJSONValue):
    TLSPCodeLensResult;
begin
  Result := TLSPCodeLensResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('codeLensArray', ResultJson);
end;

function JsonCodeLensResolveResultToObject(const ResultJson: TJSONValue):
  TLSPCodeLensResolveResult;
begin
  Result := TLSPCodeLensResolveResult.Create;

  if ResultJson is TJsonObject then
    Result.MemberFromJsonValue('codeLens', ResultJson);
end;

function JsonColorPresentationResultToObject(const ResultJson: TJSONValue): TLSPColorPresentationResult;
begin
  Result := TLSPColorPresentationResult.Create;
  if ResultJson is TJsonArray then
    Result.MemberFromJsonValue('colorPresentations', ResultJson);
end;

function JsonCompletionResultToObject(const ResultJson: TJSONValue): TLSPCompletionList;
begin
  Result := TLSPCompletionList.Create;

  if ResultJson is TJsonObject then
    Result.FromJSON(TJsonObject(ResultJson))
  else if ResultJson is TJsonArray then
    Result.MemberFromJsonValue('items', ResultJson);
end;

function JsonCompletionItemResolveResultToObject(const ResultJson: TJSONValue): TLSPCompetionItemResolveResult;
begin
  Result := TLSPCompetionItemResolveResult.Create;
  if ResultJson is TJsonObject then
    Result.MemberFromJsonValue('completionItem', ResultJson)
end;

function JsonConfigurationParamsToObjects(const ParamsJson: TJSONValue): TLSPConfigurationParams;
var
  I: Integer;
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
begin
  Result := nil;

  JsonValue := ParamsJson.FindValue('items');

  if Assigned(JsonValue) and (JsonValue is TJSONArray) then
  begin
    JsonArray := JsonValue as TJSONArray;
    SetLength(Result, JsonArray.Count);

    I := 0;
    for  JsonValue in JsonArray do
    begin
      if not (JsonValue is TJSONObject) then Continue;
      Result[I].scopeUri := TJSONObject(JsonValue).Values['scopeUri'].ToString;
      Result[I].section := TJSONObject(JsonValue).Values['section'].ToString;;
      Inc(I)
    end;

    if I <> Length(Result) then
      SetLength(Result, I);
  end;
end;

function JsonDocumentColorResultToObject(const ResultJson: TJSONValue): TLSPColorInformationResult;
begin
  Result := TLSPColorInformationResult.Create;
  if ResultJson is TJsonArray then
    Result.MemberFromJsonValue('colors', ResultJson);
end;

function JsonDocumentDiagnosticReportToObject(const ResultJson: TJSONValue): TLSPRelatedDocumentDiagnosticReport;
begin
  Result := TLSPRelatedDocumentDiagnosticReport.Create;

  if ResultJson is TJSONObject then
    Result.FromJSON(TJSONObject(ResultJson));
end;

function JsonDocumentFormattingResultToObject(const ResultJson: TJSONValue): TLSPTextEditResult;
begin
  Result := TLSPTextEditResult.Create;

  if ResultJson is TJsonArray then
    Result.MemberFromJsonValue('edits', ResultJson);
end;

function JsonDocumentHighlightResponseToObject(const ResultJson: TJSONValue): TLSPDocumentHighlightResult;
begin
  Result := TLSPDocumentHighlightResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('list', ResultJson);
end;

function JsonDocumentSymbolsResultToObject(const ResultJson: TJSONValue): TLSPDocumentSymbolsResult;
var
  HasDocumentSymbols: Boolean;
begin
  Result := TLSPDocumentSymbolsResult.Create;

  if (ResultJson is TJSONArray) and (TJSONArray(ResultJson).Count > 0) then
  begin
    HasDocumentSymbols := ResultJson.FindValue('[0].range') <> nil;
    if HasDocumentSymbols then
      Result.MemberFromJsonValue('symbols', ResultJson)
    else
      Result.MemberFromJsonValue('symbolInformations', ResultJson);
  end;
end;

function JsonDocumentLinkResolveResultToObject(const ResultJson: TJSONValue):
    TLSPDocumentLinkResolveResult;
begin
  Result := TLSPDocumentLinkResolveResult.Create;

  if ResultJson is TJSONObject then
    Result.MemberFromJsonValue('documentLink', ResultJson);
end;

function JsonDocumentLinkResultToObject(const ResultJson: TJSONValue): TLSPDocumentLinkResult;
begin
    Result := TLSPDocumentLinkResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('documentLinks', ResultJson);
end;

function JsonExecuteCommandResult(const ResultJson: TJSONValue): string;
begin
  // LSP any
  Result := ResultJson.ToJson;
end;

function JsonFindReferencesResultToObject(const ResultJson: TJSONValue): TLSPFindReferencesResult;
begin
  Result := TLSPFindReferencesResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('locations', ResultJson);
end;

function JsonFoldingRangeResultToObject(const ResultJson: TJSONValue): TLSPFoldingRangeResult;
begin
  Result := TLSPFoldingRangeResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('foldingRanges', ResultJson);
end;

function JsonGotoResultToObject(const ResultJson: TJSONValue): TLSPGotoResult;
var
  HasLinks: Boolean;
begin
  Result := TLSPGotoResult.Create;

  if ResultJson is TJSONObject then
    // TLSPLocation
    Result.FromJSON(TJSONObject(ResultJson))
  else if ResultJson is TJSONArray and (TJSONArray(ResultJson).Count > 0) then
  begin
    HasLinks := ResultJson.FindValue('[0].targetUri') <> nil;
    if HasLinks then
      // TArray<TLSPLocationLink>
      Result.MemberFromJsonValue('locationLinks', ResultJson)
    else
      // TArray<TLSPLocation>
      Result.MemberFromJsonValue('locations', ResultJson);
  end;
end;

function JsonHoverResultToObject(const ResultJson: TJSONValue): TLSPHoverResult;
var
  Contents: TJsonValue;
begin
  Result := TLSPHoverResult.Create;

  if not (ResultJson is TJSONObject) then Exit;

  Result.MemberFromJsonValue('range', ResultJson.FindValue('ramge'));

  Contents := ResultJson.FindValue('contents');

  if Contents is TJSONArray then
    Result.contentsMarkedArray :=
      TSerializer.Deserialize<TArray<TLSPMarkedString>>(Contents)
  else if Contents is TJSONObject then
  begin
    if Contents.GetValue<string>('language', '') <> '' then
      // MarkedString = language-value pair
      Result.MemberFromJsonValue('contentsMarked', Contents)
    else
      // MarkupContent
      Result.MemberFromJsonValue('contents', Contents);
  end
  else if Contents is TJSONString then
    // MarkedString = string
    Result.contents.value := TJSONString(Contents).value;
end;

function JsonInitializeResultToObject(const ResultJson: TJsonValue):TLSPInitializeResult;
var
  ResValue: TJSONValue;
  ResJson,
  LObject,
  LCapabilities: TJSONObject;
begin
  Result := TLSPInitializeResult.Create;

  if not (ResultJson is TJSONObject) then Exit;
  ResJson := TJSONObject(ResultJson);

  // Server info
  if ResJson.Values['serverInfo'] is TJSONObject then
    Result.MemberFromJsonValue('serverInfo', ResJson.Values['serverInfo']);

  // Server capabilities
  Result.capabilities := TLSPServerCapabilities.Create;

  ResValue := ResJson.Values['capabilities'];
  if not (ResValue is TJSONObject) then Exit;
  LCapabilities := TJSONObject(ResValue);

  // positionEncoding
  //
  // The position encoding the server picked from the encodings offered
	// by the client via the client capability `general.positionEncodings`.
  //
	// If the client didn't provide any position encodings the only valid
	// value that a server can return is 'utf-16'.
  //
	// If omitted it defaults to 'utf-16'.
  //
	// @since 3.17.0
  //
  if LCapabilities.Values['positionEncoding'] is TJSONString then
    Result.capabilities.positionEncoding :=
      TJSONString(LCapabilities.Values['positionEncoding']).value;

  // textDocumentSync
  //
  // Defines how text documents are synced.
  if LCapabilities.Values['textDocumentSync'] is TJSONObject then
  begin
    // TextDocumentSyncOptions
    LObject := TJsonObject(LCapabilities.Values['textDocumentSync']);
    Result.capabilities.textDocumentSync := TLSPTextDocumentServerSyncOptions.Create;

    // "change". Change notifications are sent to the server.
    Result.capabilities.textDocumentSync.change := LObject.GetValue<Integer>('change');

    // "openClose". Open and close notifications are sent to the server.
    LObject.TryGetValue('openClose', Result.capabilities.textDocumentSync.openClose);

    // "willSave". WillSave notifications are sent to the server.
    LObject.TryGetValue('willSave', Result.capabilities.textDocumentSync.willSave);

    // "willSaveWaitUntil". WillSaveWaitUntil notifications are sent to the server.
    LObject.TryGetValue('willSaveWaitUntil', Result.capabilities.textDocumentSync.willSaveWaitUntil);

    // "save" Save notifications are sent to the server.
    if not LObject.TryGetValue('save', Result.capabilities.textDocumentSync.save.value) and
      LObject.TryGetValue('save.includeText', Result.capabilities.textDocumentSync.save.includeText)
    then
        Result.capabilities.textDocumentSync.save.value := True;
  end
  else if LCapabilities.Values['textDocumentSync'] is TJSONNumber then
  begin
    Result.capabilities.textDocumentSync := TLSPTextDocumentServerSyncOptions.Create;

    // TextDocumentSyncKind
    Result.capabilities.textDocumentSync.change :=
      TJSONNumber(LCapabilities.Values['textDocumentSync']).AsInt;
    Result.capabilities.textDocumentSync.openClose :=
      Result.capabilities.textDocumentSync.change > 0;
  end;

  // notebookDocumentSync
  //
  // Defines how notebook documents are synced.
  if LCapabilities.Values['notebookDocumentSync'] is TJSONObject then
  begin
    Result.capabilities.notebookDocumentSync := TLSPNotebookDocumentSyncRegistrationOptions.Create;
    Result.capabilities.notebookDocumentSync.FromJSON(
      TJsonObject(LCapabilities.Values['notebookDocumentSync']));
  end;

  // completionProvider
  //
  // The server provides completion support.
  if LCapabilities.Values['completionProvider'] is TJSONObject then
  begin
    Result.capabilities.completionProvider := TLSPCompletionOptions.Create;
    if LCapabilities.Values['completionProvider'] is TJSONObject then
      Result.capabilities.completionProvider.FromJSON(
        TJsonObject(LCapabilities.Values['completionProvider']));
  end;

  // hoverProvider
  //
  // The server provides hover support.
  if (LCapabilities.Values['hoverProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('hoverProvider', False) then
  begin
    Result.capabilities.hoverProvider := TLSPHoverOptions.Create;
    Result.capabilities.hoverProvider.workDoneProgress :=
      LCapabilities.GetValue('hoverProvider.workDoneProgress', False);
  end;

  // signatureHelpProvider
  //
  // The server provides signature help support.
  if (LCapabilities.Values['signatureHelpProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('signatureHelpProvider', False) then
  begin
    Result.capabilities.signatureHelpProvider := TLSPSignatureHelpOptions.Create;
    if LCapabilities.Values['signatureHelpProvider'] is TJSONObject then
      Result.capabilities.signatureHelpProvider.FromJSON(
        TJsonObject(LCapabilities.Values['signatureHelpProvider']));
  end;

  // declarationProvider
  //
  // The server provides go to declaration support.
  if (LCapabilities.Values['declarationProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('declarationProvider', False) then
  begin
    Result.capabilities.declarationProvider := TLSPDeclarationRegistrationOptions.Create;
    if LCapabilities.Values['declarationProvider'] is TJSONObject then
      Result.capabilities.declarationProvider.FromJSON(
        TJsonObject(LCapabilities.Values['declarationProvider']));
  end;

  // definitionProvider
  //
  // The server provides goto definition support.
  if (LCapabilities.Values['definitionProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('definitionProvider', False) then
  begin
    Result.capabilities.definitionProvider := TLSPDefinitionOptions.Create;
    Result.capabilities.definitionProvider.workDoneProgress :=
      LCapabilities.GetValue('definitionProvider.workDoneProgress', False);
  end;

  // typeDefinitionProvider
  //
  // The server provides goto type definition support.
  if (LCapabilities.Values['typeDefinitionProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('typeDefinitionProvider', False) then
  begin
    Result.capabilities.typeDefinitionProvider := TLSPTypeDefinitionRegistrationOptions.Create;
    if LCapabilities.Values['typeDefinitionProvider'] is TJSONObject then
      Result.capabilities.typeDefinitionProvider.FromJSON(
        TJsonObject(LCapabilities.Values['typeDefinitionProvider']));
  end;

  // implementationProvider
  //
  // The server provides goto implementation support.
  if (LCapabilities.Values['implementationProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('implementationProvider', False) then
  begin
    Result.capabilities.implementationProvider := TLSPImplementationRegistrationOptions.Create;
    if LCapabilities.Values['implementationProvider'] is TJSONObject then
      Result.capabilities.implementationProvider.FromJSON(
        TJsonObject(LCapabilities.Values['implementationProvider']));
  end;

  // referencesProvider
  //
  // The server provides find references support.
  if (LCapabilities.Values['referencesProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('referencesProvider', False) then
  begin
    Result.capabilities.referencesProvider := TLSPReferenceOptions.Create;
    Result.capabilities.referencesProvider.workDoneProgress :=
      LCapabilities.GetValue('referencesProvider.workDoneProgress', False);
  end;

  // documentHighlightProvider
  //
  // The server provides document highlight support.
  if (LCapabilities.Values['documentHighlightProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('documentHighlightProvider', False) then
  begin
    Result.capabilities.documentHighlightProvider := TLSPDocumentHighlightOptions.Create;
    Result.capabilities.documentHighlightProvider.workDoneProgress :=
      LCapabilities.GetValue('documentHighlightProvider.workDoneProgress', False);
  end;

  // documentSymbolProvider
  //
  // The server provides document symbol support.
  if (LCapabilities.Values['documentSymbolProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('documentSymbolProvider', False) then
  begin
    Result.capabilities.documentSymbolProvider := TLSPDocumentSymbolOptions.Create;
    if LCapabilities.Values['documentSymbolProvider'] is TJSONObject then
      Result.capabilities.documentSymbolProvider.FromJSON(
        TJsonObject(LCapabilities.Values['documentSymbolProvider']));
  end;

  // codeActionProvider
  //
  // The server provides code actions. The `CodeActionOptions` return type is only valid if the client signals
  // code action literal support via the property `textDocument.codeAction.codeActionLiteralSupport`.
  if (LCapabilities.Values['codeActionProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('codeActionProvider', False) then
  begin
    Result.capabilities.codeActionProvider := TLSPCodeActionOptions.Create;
    if LCapabilities.Values['codeActionProvider'] is TJSONObject then
      Result.capabilities.codeActionProvider.FromJSON(
        TJsonObject(LCapabilities.Values['codeActionProvider']));
  end;

  // codeLensProvider
  //
  // The server provides CodeLens.
  if (LCapabilities.Values['codeLensProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('codeLensProvider', False) then
  begin
    Result.capabilities.codeLensProvider := TLSPCodeLensOptions.Create;
    if LCapabilities.Values['codeLensProvider'] is TJSONObject then
      Result.capabilities.codeLensProvider.FromJSON(
        TJsonObject(LCapabilities.Values['codeLensProvider']));
  end;

  // documentLinkProvider
  //
  // The server provides document link support.
  if (LCapabilities.Values['documentLinkProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('documentLinkProvider', False) then
  begin
    Result.capabilities.documentLinkProvider := TLSPDocumentLinkOptions.Create;
    if LCapabilities.Values['documentLinkProvider'] is TJSONObject then
      Result.capabilities.documentLinkProvider.FromJSON(
        TJsonObject(LCapabilities.Values['documentLinkProvider']));
  end;

  // colorProvider
  //
  // The server provides color provider support.
  if (LCapabilities.Values['colorProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('colorProvider', False) then
  begin
    Result.capabilities.colorProvider := TLSPDocumentColorRegistrationOptions.Create;
    if LCapabilities.Values['colorProvider'] is TJSONObject then
      Result.capabilities.colorProvider.FromJSON(
        TJsonObject(LCapabilities.Values['documentOnTypeFormattingProvider']));
  end;

  // documentFormattingProvider
  //
  // The server provides document formatting.
  if (LCapabilities.Values['documentFormattingProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('documentFormattingProvider', False) then
  begin
    Result.capabilities.documentFormattingProvider := TLSPDocumentFormattingOptions.Create;
    Result.capabilities.documentFormattingProvider.workDoneProgress :=
      LCapabilities.GetValue('documentFormattingProvider.workDoneProgress', False);
  end;

  // documentRangeFormattingProvider
  //
  // The server provides document range formatting.
  if (LCapabilities.Values['documentRangeFormattingProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('documentRangeFormattingProvider', False) then
  begin
    Result.capabilities.documentRangeFormattingProvider := TLSPDocumentRangeFormattingOptions.Create;
    Result.capabilities.documentRangeFormattingProvider.workDoneProgress :=
      LCapabilities.GetValue('documentRangeFormattingProvider.workDoneProgress', False);
  end;

  // documentOnTypeFormattingProvider
  //
  // The server provides document formatting on typing.
  if (LCapabilities.Values['documentOnTypeFormattingProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('documentOnTypeFormattingProvider', False) then
  begin
    Result.capabilities.documentOnTypeFormattingProvider := TLSPDocumentOnTypeFormattingOptions.Create;
    if LCapabilities.Values['documentOnTypeFormattingProvider'] is TJSONObject then
      Result.capabilities.documentOnTypeFormattingProvider.FromJSON(
        TJsonObject(LCapabilities.Values['documentOnTypeFormattingProvider']));
  end;

  // renameProvider
  //
  // The server provides rename support. RenameOptions may only be specified if the client states that it supports
  // `prepareSupport` in its initial `initialize` request.
  if (LCapabilities.Values['renameProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('renameProvider', False) then
  begin
    Result.capabilities.renameProvider := TLSPRenameOptions.Create;
    if LCapabilities.Values['renameProvider'] is TJSONObject then
      Result.capabilities.renameProvider.FromJSON(
        TJsonObject(LCapabilities.Values['renameProvider']));
  end;

  // foldingRangeProvider
  //
  // The server provides folding provider support.
  if (LCapabilities.Values['foldingRangeProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('foldingRangeProvider', False) then
  begin
    Result.capabilities.foldingRangeProvider := TLSPFoldingRangeRegistrationOptions.Create;
    if LCapabilities.Values['foldingRangeProvider'] is TJSONObject then
      Result.capabilities.foldingRangeProvider.FromJSON(
        TJsonObject(LCapabilities.Values['foldingRangeProvider']));
  end;

  // executeCommandProvider
  //
  // The server provides execute command support.
  if (LCapabilities.Values['executeCommandProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('executeCommandProvider', False) then
  begin
    Result.capabilities.executeCommandProvider := TLSPExecuteCommandOptions.Create;
    if LCapabilities.Values['executeCommandProvider'] is TJSONObject then
      Result.capabilities.executeCommandProvider.FromJSON(
        TJsonObject(LCapabilities.Values['executeCommandProvider']));
  end;

  // selectionRangeProvider
  //
  // The server provides selection range support.
  if (LCapabilities.Values['selectionRangeProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('selectionRangeProvider', False) then
  begin
    Result.capabilities.selectionRangeProvider := TLSPSelectionRangeRegistrationOptions.Create;
    if LCapabilities.Values['selectionRangeProvider'] is TJSONObject then
       Result.capabilities.selectionRangeProvider.FromJSON(
         TJsonObject(LCapabilities.Values['selectionRangeProvider']));
  end;

  // workspaceSymbolProvider
  //
  // The server provides workspace symbol support.
  if (LCapabilities.Values['workspaceSymbolProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('workspaceSymbolProvider', False) then
  begin
    Result.capabilities.workspaceSymbolProvider := TLSPWorkspaceSymbolOptions.Create;
    if LCapabilities.Values['workspaceSymbolProvider'] is TJSONObject then
      Result.capabilities.workspaceSymbolProvider.FromJSON(
        TJsonObject(LCapabilities.Values['workspaceSymbolProvider']));
  end;

  // linkedEditingRangeProvider
  //
  // The server provides linked editing range support.
  if (LCapabilities.Values['linkedEditingRangeProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('linkedEditingRangeProvider', False) then
  begin
    Result.capabilities.linkedEditingRangeProvider := TLSPLinkedEditingRangeRegistrationOptions.Create;
    if LCapabilities.Values['linkedEditingRangeProvider'] is TJSONObject then
      Result.capabilities.linkedEditingRangeProvider.FromJSON(
        TJsonObject(LCapabilities.Values['linkedEditingRangeProvider']));
  end;

  // callHierarchyProvider
  //
  // The server provides call hierarchy support.
  if (LCapabilities.Values['callHierarchyProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('callHierarchyProvider', False) then
  begin
    Result.capabilities.callHierarchyProvider := TLSPCallHierarchyRegistrationOptions.Create;
    if LCapabilities.Values['callHierarchyProvider'] is TJSONObject then
      Result.capabilities.callHierarchyProvider.FromJSON(
        TJsonObject(LCapabilities.Values['callHierarchyProvider']));
  end;

  // semanticTokensProvider
  //
  // The server provides semantic tokens support.
  if (LCapabilities.Values['semanticTokensProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('semanticTokensProvider', False) then
  begin
    Result.capabilities.semanticTokensProvider := TLSPSemanticTokensRegistrationOptions.Create;
    if LCapabilities.Values['semanticTokensProvider'] is TJSONObject then
    begin
      Result.capabilities.semanticTokensProvider.FromJSON(
        TJsonObject(LCapabilities.Values['semanticTokensProvider']));
      Result.capabilities.semanticTokensProvider.ProcessFull;
    end;
  end;

  // monikerProvider
  //
  // Whether server provides moniker support.
  if (LCapabilities.Values['monikerProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('monikerProvider', False) then
  begin
    Result.capabilities.monikerProvider := TLSPMonikerRegistrationOptions.Create;
    if LCapabilities.Values['monikerProvider'] is TJSONObject then
      Result.capabilities.monikerProvider.FromJSON(
        TJsonObject(LCapabilities.Values['monikerProvider']));
  end;

  // typeHierarchyProvider
  //
  // The server provides type hierarchy support.
  if (LCapabilities.Values['typeHierarchyProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('typeHierarchyProvider', False) then
  begin
    Result.capabilities.typeHierarchyProvider := TLSPTypeHierarchyRegistrationOptions.Create;
    if LCapabilities.Values['typeHierarchyProvider'] is TJSONObject then
      Result.capabilities.typeHierarchyProvider.FromJSON(
        TJsonObject(LCapabilities.Values['typeHierarchyProvider']));
  end;

  // inlineValueProvider
  //
  // Whether server provides inline values.
  if (LCapabilities.Values['inlineValueProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('inlineValueProvider', False) then
  begin
    Result.capabilities.inlineValueProvider := TLSPInlineValueRegistrationOptions.Create;
    if LCapabilities.Values['inlineValueProvider'] is TJSONObject then
      Result.capabilities.inlineValueProvider.FromJSON(
        TJsonObject(LCapabilities.Values['inlineValueProvider']));
  end;

  // inlayHintProvider
  //
  // Whether server provides moniker support.
  if (LCapabilities.Values['inlayHintProvider'] is TJSONObject) or
    LCapabilities.GetValue<Boolean>('inlayHintProvider', False) then
  begin
    Result.capabilities.inlayHintProvider := TLSPInlayHintRegistrationOptions.Create;
    if LCapabilities.Values['inlayHintProvider'] is TJSONObject then
      Result.capabilities.inlayHintProvider.FromJSON(
        TJsonObject(LCapabilities.Values['inlayHintProvider']));
  end;

  // diagnosticProvider
  //
  // The server has support for pull model diagnostics.
  if LCapabilities.Values['diagnosticProvider'] is TJSONObject then
  begin
    Result.capabilities.diagnosticProvider := TLSPDiagnosticRegistrationOptions.Create;
    Result.capabilities.diagnosticProvider.FromJSON(
      TJsonObject(LCapabilities.Values['diagnosticProvider']));
  end;

  // Workspace
  //
  // Workspace specific server capabilities
  if LCapabilities.Values['workspace'] is TJSONObject then
  begin
    Result.capabilities.workspace := TLSPWorkspaceServer.Create;
    Result.capabilities.workspace.FromJSON(
      TJsonObject(LCapabilities.Values['workspace']));
  end;

  // Experimental server capabilities.
  //
  // experimental: any;
  if LCapabilities.Values['experimental']  <> nil then
    Result.Capabilities.experimental := LCapabilities.Values['experimental'].ToJSON;
end;

function JsonLinkedEditingRangeResultToObject(const ResultJson: TJsonValue): TLSPLinkedEditingRangeResult;
begin
  Result := TLSPLinkedEditingRangeResult.Create;

  if ResultJson is TJSONObject then
    Result.FromJSON(TJSONObject(ResultJson))
end;

function JsonLogTraceParamsToObject(const ParamsJson: TJsonValue): TLSPLogTraceParams;
begin
  Result := TLSPLogTraceParams.Create;

  if ParamsJson is TJSONObject then
    Result.FromJSON(TJSONObject(ParamsJson));
end;

function JsonMonikerResultToObject(const ResultJson: TJsonValue): TLSPMonikerResult;
begin
  Result := TLSPMonikerResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('monikers', ResultJson);
end;

function JsonPrepareCallHierarchyResultToObject(const ResultJson: TJsonValue): TLSPPrepareCallHierarchyResult;
begin
  Result := TLSPPrepareCallHierarchyResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('items', ResultJson);
end;

function JsonPrepareTypeHierarchyResultToObject(const ResultJson: TJsonValue): TLSPPrepareTypeHierarchyResult;
begin
  Result := TLSPPrepareTypeHierarchyResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('items', ResultJson);
end;

function JsonProgressValueToResult(const kind: Integer; const ParamsJson: TJSONValue): TLSPBaseResult;
begin
  Result := nil;
  case TLSPKind(kind) of
    lspWorkspaceSymbol:
      Result := JsonSignatureHelpResultToObject(ParamsJson);
    lspCallHierarchyIncommingCalls:
      Result := JsonCallHierarchyIncommingResultToObject(ParamsJson);
    lspCallHierarchyOutgoingCalls:
      Result := JsonCallHierarchyOutgoingResultToObject(ParamsJson);
    lspColorPresentation:
      Result := JsonColorPresentationResultToObject(ParamsJson);
    lspDocumentColor:
      Result := JsonDocumentColorResultToObject(ParamsJson);
    lspDocumentHighlight:
      Result := JsonDocumentHighlightResponseToObject(ParamsJson);
    lspDocumentSymbol:
      Result := JsonDocumentSymbolsResultToObject(ParamsJson);
    lspDocumentLink:
      Result := JsonDocumentLinkResultToObject(ParamsJson);
    lspFoldingRange:
      Result := JsonFoldingRangeResultToObject(ParamsJson);
    lspGotoDeclaration,
    lspGotoDefinition,
    lspGotoTypeDefinition,
    lspGotoImplementation:
      Result := JsonGotoResultToObject(ParamsJson);
    lspMoniker:
      Result := JsonMonikerResultToObject(ParamsJson);
    lspReferences:
      Result := JsonFindReferencesResultToObject(ParamsJson);
    lspSelectionRange:
      Result := JsonSelectionRangeResultToObject(ParamsJson);
    lspSemanticTokensFull:
      Result := JsonSemanticTokensFullResultToObject(ParamsJson);
    lspSemanticTokensFullDelta:
      Result := JsonSemanticTokensFullDeltaResultToObject(ParamsJson);
  end;
end;

function JsonInlayHintResultToObject(const ResultJson: TJSONValue): TLSPInlayHintResult;
begin
  Result := TLSPInlayHintResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('inlayHints', ResultJson);
end;

function JsonInlayHintResolveResultToObject(const ResultJson: TJSONValue): TLSPInlayHintResolveResult;
begin
  Result := TLSPInlayHintResolveResult.Create;

  if ResultJson is TJSONObject then
    Result.MemberFromJsonValue('inlayHint', ResultJson);
end;

function JsonInlineValueResultToObject(const ResultJson: TJSONValue): TLSPInlineValueResult;
begin
  Result := TLSPInlineValueResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('inlineValues', ResultJson);
end;

function JsonRegisterCapabilitiesToRegistrations(const ParamsJson:
  TJSONValue): TArray<TLSPRegistration>;
var
  JsonValue: TJSONValue;
  RegOptionsClass: TLSPRegistrationOptionsClass;
  Reg: TLSPRegistration;
  Index: Integer;
begin
  Result := nil;

  JSONValue := ParamsJson.FindValue('registrations');

  if not (JSONValue is TJSONArray) then Exit;

  Result := TSerializer.Deserialize<TArray<TLSPRegistration>>(JsonValue);

  // Iterate backwards since we may be removing Result items
  for Index := Length(Result) - 1 downto 0 do
  begin
    Reg := Result[Index];

    if (Reg.Id = '') or (Reg.method = '') then
    begin
      Delete(Result, Index, 1);
      Continue;
    end;

    if Reg.registerOptions <> '' then
    begin
      if reg.registerOptions = '' then Continue;

      RegOptionsClass :=  TLSPRegistration.OptionsClass(Reg.method);
      if RegOptionsClass = nil then
        Reg.RegistrationOptions := nil
      else
      begin
        Reg.RegistrationOptions := RegOptionsClass.Create;
        try
          Reg.RegistrationOptions.FromJSON(Reg.registerOptions)
        except
          FreeAndNil(Reg.RegistrationOptions);
        end;
      end;
    end;
  end;
end;

function JsonPrepareRenameResultToObject(const ResultJson: TJSONValue): TLSPPrepareRenameResult;
begin
  Result := TLSPPrepareRenameResult.Create;

  if ResultJson is TJSONObject then
    Result.FromJSON(TJSONObject(ResultJson));
end;

function JsonWorkspaceEditResultToObject(const ResultJson: TJSONValue): TLSPWorkspaceEdit;
begin
  // WorkspaceEdit | null
  Result := nil;

  if ResultJson is TJSONObject then
  begin
    Result := TLSPWorkspaceEdit.Create;
    Result.FromJSON(TJSONObject(ResultJson));
  end;
end;

function JsonSelectionRangeResultToObject(const ResultJson: TJSONValue): TLSPSelectionRangeResult;
begin
  Result := TLSPSelectionRangeResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('selRanges', ResultJson);
end;

function JsonSemanticTokensFullResultToObject(const ResultJson: TJSONValue): TLSPSemanticTokensResult;
begin
  Result := TLSPSemanticTokensResult.Create;

  if ResultJson is TJSONObject then
    Result.FromJson(TJSONObject(ResultJson));
end;

function JsonSemanticTokensFullDeltaResultToObject(const ResultJson: TJSONValue): TLSPSemanticTokensDeltaResult;
begin
  Result := TLSPSemanticTokensDeltaResult.Create;

  if ResultJson is TJSONObject then
    Result.FromJson(TJSONObject(ResultJson));
end;

function JsonShowDocumentRequestParams(const ParamsJson: TJSONValue): TLSPShowDocumentParams;
begin
  Result := TLSPShowDocumentParams.Create;

  if ParamsJson is TJSONObject then
    Result.FromJSON(TJSONObject(ParamsJson));
end;

procedure JsonShowMessageParams(const ParamsJson: TJSONValue; var ntype: Integer; var msg: string);
begin
  // Extract message type and message string
  ntype := ParamsJson.GetValue<Integer>('type', 0);
  msg := ParamsJson.GetValue<string>('message', '');
end;

function JsonShowMessageRequestParams(const ParamsJson: TJSONValue): TLSPShowMessageRequestParams;
begin
  Result := TLSPShowMessageRequestParams.Create;

  if ParamsJson is TJSONObject then
    Result.FromJSON(TJSONObject(ParamsJson));
end;

function JsonSignatureHelpResultToObject(const ResultJson: TJSONValue): TLSPSignatureHelpResult;
begin
  Result := TLSPSignatureHelpResult.Create;

  if ResultJson is TJSONObject then
    Result.FromJSON(TJSONObject(ResultJson));
end;

function JsonTypeHierarchySupertypesResultToObject(const ResultJson: TJSONValue): TLSPPrepareTypeHierarchyResult;
begin
  Result := TLSPPrepareTypeHierarchyResult.Create;

  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('items', ResultJson);
end;

function JsonUnregisterCapabilitiesToUnregistrations(const ParamsJson: TJSONValue): TArray<TLSPUnregistration>;
var
  JsonValue: TJsonValue;
begin
  JsonValue := ParamsJson.FindValue('unregisterations');

  if JsonValue is TJSONArray then
    Result := TSerializer.Deserialize<TArray<TLSPUnregistration>>(JsonValue);
end;

function JsonWillSaveWaitUntilResultToObject(const ResultJson: TJSONValue): TArray<TLSPAnnotatedTextEdit>;
begin
  Result := [];

  if ResultJson is TJSONArray then
    Result := TSerializer.Deserialize<TArray<TLSPAnnotatedTextEdit>>(ResultJson);
end;

procedure JsonWorkDoneProgressRequestParams(const ParamsJson: TJSONValue; var token: string);
begin
  // Extract the token string
  token := ParamsJson.GetValue<string>('token', '');
end;

function JsonWorkspaceApplyEditParamsToObject(const ParamsJson: TJSONValue): TLSPApplyWorkspaceEditParams;
begin
  Result := TLSPApplyWorkspaceEditParams.Create;

  if ParamsJson is TJSONObject then
    Result.FromJSON(TJSONObject(ParamsJson));
end;

function JsonWorkspaceDiagnosticReportToObject(const ResultJson: TJSONValue): TLSPWorkspaceDiagnosticReport;
begin
  Result := TLSPWorkspaceDiagnosticReport.Create;

  if ResultJson is TJSONObject then
    Result.FromJSON(TJsonObject(ResultJson));
end;

function JsonWorkspaceSymbolResultToObject(const ResultJson: TJSONValue): TLSPWorkspaceSymbolInformationResult;
begin
  Result := TLSPWorkspaceSymbolInformationResult.Create;
  if ResultJson is TJSONArray then
    Result.MemberFromJsonValue('values', ResultJson);
end;

function CreateJSONRequestParam(const lspKind: TLSPKind; Params: TLSPBaseParams): string;
begin
  Result := '';
  if not Assigned(Params) then Exit;

  case lspKind of
    lspInitialize:                    Result := LSPInitializeParamsToJSON(TLSPInitializeParams(Params));
    lspDidSaveTextDocument:           Result := TLSPDidSaveTextDocumentParams(Params).AsJSON([TSerializeOption.IgnoreEmpty]);
    lspCompletionItemResolve:         Result := TSerializer.Serialize(TLSPCompletionItemResolveParams(Params).completionItem);
    lspCodeActionResolve:             Result := TSerializer.Serialize(TLSPCodeActionResolveParams(Params).codeaction);
    lspCodeLensResolve:               Result := TSerializer.Serialize(TLSPCodeLensResolveParams(Params).codeLens);
    lspDocumentLinkResolve:           Result := TSerializer.Serialize(TLSPDocumentLinkResolveParams(Params).documentLink);
    lspInlayHintResolve:              Result := TSerializer.Serialize(TLSPInlayHintResolveParams(Params).inlayHint);
    lspCompletion:                    Result := TLSPCompletionParams(Params).AsJSON([TSerializeOption.IgnoreEmpty]);
  else
    Result := Params.AsJSON;
  end;
end;

function CreateJSONResponse(const id: Variant; params: TLSPBaseParams;
    error: TLSPResponseError; resultType: TLSPResultType;
    const resultString: string): string;
var
  sResult, sError: string;
  sId: string;
begin
  sError := '';

  if VarType(Id) = varInteger then
    sId := IntToStr(id)
  else
    sId := #34 + Id + #34;

  Result := '{"jsonrpc": "2.0","id": ' + sId + ',';

  if Assigned(error) then
    Result := Result + '"error": ' + error.AsJSON +'}'
  else
  begin
    // Create the params string
    if resultType = lsprNull then
      sResult := 'null'
    else if resultType = lsprEmptyArray then
      sResult := '[]'
    else if resultType = lsprString then
      sResult := resultString
    else if Assigned(params) then
      sResult := params.AsJSON;

    if (sResult = '') then sResult := 'null';
    Result := Result + '"result": ' + sResult +'}'
  end;
end;

function GetKindFromMethod(method: string; const id: Integer = -1): Integer;
begin
  Result := IndexStr(method, LSPIdStrings);
  if Result < 0 then Result := id;
end;

function GetMethodFromKind(const lspId: TLSPKind): string;
begin
  Result := #34 + LSPIdStrings[Ord(lspId)] + #34;
end;

function GetKindFromId(Id: Integer): Integer;
begin
  Result := Id and $FF;
end;

function ResponseError(Json: TJsonObject; var ErrorCode: Integer;
  ErrorMessage: string): Boolean;
begin
  ErrorCode := 0;
  ErrorMessage := '';

  // Check for errors
  Result := (Json.Values['error'] is TJSONObject);

  if Result then
  begin
    // Extract error code and error message
    ErrorCode := Json.GetValue<integer>('error.code', 0);
    ErrorMessage := Json.GetValue<string>('error.message', '');
  end;
end;

function ResponseError(Json: TJsonObject): Boolean; overload;
var
  ErrorCode: Integer;
  ErrorMessage: string;
begin
  Result := ResponseError(Json, ErrorCode, ErrorMessage);
end;

end.
