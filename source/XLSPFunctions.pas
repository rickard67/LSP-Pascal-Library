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

unit XLSPFunctions;

interface

uses XLSPTypes, generics.collections, XSuperJSON, XSuperObject;

function CreateJSONRequestParam(const lspKind: TLSPKind; lspMsg: TLSPMessage): string;
function CreateJSONRequest(const lspKind: TLSPKind; lspMsg: TLSPMessage; const method: string = ''; const paramJSON:
    string = ''): string;
function CreateJSONResponse(const lspKind: TLSPKind; lspMsg: TLSPMessage; const method: string = ''; resultType:
    TLSPResultType = lsprObject; resultString: string = ''): string;
function GetKindFromMethod(s: string; const id: Integer = -1): Integer;
function GetMethodFromKind(const lspId: TLSPKind): string;
function IsRequest(const lspId: TLSPKind): Boolean;
function JsonCallHierarchyIncommingResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPCallHierarchyIncomingCallResponse;
function JsonCallHierarchyOutgoingResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPCallHierarchyOutgoingCallResponse;
function JsonCodeActionResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPCodeActionResponse;
function JsonCodeActionResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPCodeAction;
function JsonCodeLensToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPCodeLensResponse;
function JsonCodeLensResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPCodeLens;
function JsonColorPresentationValuesToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string; const
    path: string = ''): TLSPColorPresentationValues;
function JsonCompletionResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPCompletionList;
function JsonCompletionItemResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPCompletionItem;
function JsonConfigurationParamsToObjects(const LJson: ISuperObject): TLSPConfigurationParams;
function JsonDocumentColorValuesToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string; const
    path: string = ''): TLSPColorInformationValues;
function JsonDocumentFormattingResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPTextEditValues;
function JsonDocumentHighlightResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPDocumentHighlightResponse;
function JsonDocumentSymbolsResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPDocumentSymbolsResponse;
function JsonDocumentLinkResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string; const
    path: string = ''): TLSPDocumentLinkResponse;
function JsonDocumentLinkResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPDocumentLink;
function JsonExecuteCommandResult(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): string;
function JsonFindReferencesResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPFindReferencesResponse;
function JsonFoldingRangeResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string; const
    path: string = ''): TLSPFoldingRangeResponse;
function JsonGotoResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer;
    var ErrorMessage: string; const path: string = ''): TLSPGotoResponse;
function JsonHoverResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPHover;
function JsonInitializeResultToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPInitializeResultParams;
function JsonReadInitializeToClientCapabilities(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPClientCapabilities;
function JsonLinkedEditingRangesToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPLinkedEditingRanges;
function JsonLogTraceParamsToObject(const LJson: ISuperObject): TLSPLogTraceParams;
function JsonMonikerToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPMonikerResult;
function JsonPrepareCallHierarchyResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string): TLSPPrepareCallHierarchyResponse;
function JsonProgressToken(const LJson: ISuperObject): string;
function JsonProgressParamsToObject(const kind: Integer; const LJson: ISuperObject): TLSPBaseParams;
function JsonPublishDiagnosticsToObject(const LJson: ISuperObject): TLSPPublishDiagnosticsParams;
function JsonRegisterCapabilitiesToRegistrations(const LJson: ISuperObject): TArray<TLSPRegistration>;
function JsonPrepareRenameResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPPrepareRenameResponse;
function JsonRenameResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPWorkspaceEdit;
function JsonSelectionRangeResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPSelectionRangeResponse;
function JsonSemanticTokensFullToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPSemanticTokens;
function JsonSemanticTokensFullDeltaToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPSemanticTokensDelta;
procedure JsonSemanticTokensRefresh(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string);
function JsonShowDocumentRequestParams(const LJson: ISuperObject): TLSPShowDocumentParams;
procedure JsonShowMessageParams(const LJson: ISuperObject; var ntype: Integer; var msg: string);
procedure JsonShowMessageRequestParams(const LJson: ISuperObject; var ntype: Integer; var msg: string; var arr: TArray<string>);
function JsonShutdownResult(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): Boolean;
function JsonSignatureHelpResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPSignatureHelp;
function JsonUnregisterCapabilitiesToUnregistrations(const LJson: ISuperObject): TArray<TLSPUnregistration>;
function JsonWillSaveWaitUntilResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TArray<TLSPTextEdit>;
procedure JsonWorkDoneProgressRequestParams(const LJson: ISuperObject; var token: string);
function JsonWorkspaceApplyEditParamsToObject(const LJson: ISuperObject; key: string; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPApplyWorkspaceEditParams;
function JsonWorkspaceSymbolResultToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPSymbolInformations;
function FilePathToUri(const APath: string): string;
function UriToFilePath(const Uri: string): string;

implementation

uses
  System.SysUtils, System.Variants, System.StrUtils, System.Types;

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
var
  s: string;
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

function IsRequest(const lspId: TLSPKind): Boolean;
begin
  Result := lspId in [lspInitialize,lspShutdown,lspWorkspaceConfiguration,lspWorkspaceSymbol,lspWorkspaceExecuteCommand,
                      lspWorkspaceWillCreateFiles,lspWorkspaceWillRenameFiles,lspWorkspaceWillDeleteFiles,lspWillSaveWaitUntilTextDocument,
                      lspCompletion,lspCompletionItemResolve,lspHover,lspSignatureHelp,
                      lspGotoDeclaration,lspGotoDefinition,lspGotoTypeDefinition,lspGotoImplementation,
                      lspDocumentHighlight,lspDocumentSymbol,lspReferences,
                      lspCodeAction,lspCodeActionResolve,lspCodeLens,lspCodeLensResolve,lspCodeLensRefresh,
                      lspDocumentLink,lspDocumentLinkResolve,lspDocumentColor,lspColorPresentation,
                      lspDocumentFormatting,lspDocumentRangeFormatting,lspDocumentOnTypeFormatting,
                      lspRename,lspPrepareRename,lspFoldingRange,lspSelectionRange,
                      lspPrepareCallHierarchy,lspCallHierarchyIncommingCalls,lspCallHierarchyOutgoingCalls,
                      lspSemanticTokensFull,lspSemanticTokensFullDelta,lspSemanticTokensRange,lspSemanticTokensRefresh,
                      lspLinkedEditingRange,lspMoniker,
                      lspCancelRequest];
end;

function LSPCodeActionResolveParamsToJSON(const item: TLSPCodeAction): string;
var
  LJson: ISuperObject;
begin
  Result := item.AsJSON;

  LJson := TSuperObject.Create(Result);
  LJson.Raw['data'] := item.data;

  Result := LJson.AsJSON;
end;

function LSPCodeLensResolveParamsToJSON(const item: TLSPCodeLens): string;
var
  LJson: ISuperObject;
begin
  Result := item.AsJSON;

  LJson := TSuperObject.Create(Result);
  LJson.Raw['data'] := item.data;

  Result := LJson.AsJSON;
end;

function LSPDocumentLinkResolveParamsToJSON(const item: TLSPDocumentLink): string;
var
  LJson: ISuperObject;
begin
  Result := item.AsJSON;

  LJson := TSuperObject.Create(Result);
  LJson.Raw['data'] := item.data;

  Result := LJson.AsJSON;
end;

function LSPCompletionResolveParamsToJSON(const item: TLSPCompletionItem): string;
var
  LJson: ISuperObject;
begin
  Result := item.AsJSON;

  LJson := TSuperObject.Create(Result);
  LJson.Raw['data'] := item.data;

  Result := LJson.AsJSON;
end;

function LSPInitializeParamsToJSON(const initObj: TLSPInitializeParams): string;
var
  LJson: ISuperObject;
  LJObj: IJSONObject;
  LJBool: IJSONBoolean;
  LJNull: IJSONNull;
  bDelta: Boolean;
  s: string;
begin
  Result := initObj.AsJson;

  // Semantic Token
  if Assigned(initObj.capabilities) and Assigned(initObj.capabilities.textDocument) and Assigned(initObj.capabilities.textDocument.semanticTokens) and
     Assigned(initObj.capabilities.textDocument.semanticTokens.requests) and
     (initObj.capabilities.textDocument.semanticTokens.requests.semanticTokensType <> semtokenNone) then
  begin
    LJson := TSuperObject.Create(Result);

    s := 'capabilities.textDocument.semanticTokens.requests';
    if initObj.capabilities.textDocument.semanticTokens.requests.semanticTokensType = semtokenFull then
    begin
      LJBool := TJSONBoolean.Create(true);
      LJson[s].AsObject.Add('full',LJBool);
    end
    else if initObj.capabilities.textDocument.semanticTokens.requests.semanticTokensType = semtokenFullFalse then
    begin
      LJBool := TJSONBoolean.Create(false);
      LJson[s].AsObject.Add('full',LJBool);
    end
    else if initObj.capabilities.textDocument.semanticTokens.requests.semanticTokensType in [semtokenDelta,semtokenDeltaFalse] then
    begin
      bDelta := (initObj.capabilities.textDocument.semanticTokens.requests.semanticTokensType = semtokenDelta);
      LJBool := TJSONBoolean.Create(bDelta);
      LJObj := TJSONObject.Create;
      LJObj.AddPair('delta',LJBool);
      LJson[s].AsObject.Add('full',LJObj);
    end;
    Result := LJson.AsJSON;
  end;
end;

function JsonCallHierarchyIncommingResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var
    ErrorMessage: string; const path: string = ''): TLSPCallHierarchyIncomingCallResponse;
var
  LArray: ISuperArray;
  LMember: IMember;
  s: string;
  i: Integer;

  procedure ReadRange(var range: TLSPRange; const member: IMember);
  var
    supObj: ISuperObject;
  begin
    if member.DataType <> dtObject then Exit;
    supObj := member.AsObject;
    range.startPos.line := supObj.O['start'].I['line'];
    range.startPos.character := supObj.O['start'].I['character'];
    range.endPos.line := supObj.O['end'].I['line'];
    range.endPos.character := supObj.O['end'].I['character'];
  end;

  procedure ReadRangeObj(var range: TLSPRange; const supObj: ISuperObject);
  begin
    range.startPos.line := supObj.O['start'].I['line'];
    range.startPos.character := supObj.O['start'].I['character'];
    range.endPos.line := supObj.O['end'].I['line'];
    range.endPos.character := supObj.O['end'].I['character'];
  end;

  procedure ReadIncommingCall(var item: TLSPCallHierarchyIncomingCall; const member: IMember);
  var
    j: Integer;
    supObj: ISuperObject;
    LArr: ISuperArray;
    LMember: IMember;
  begin
    if member.DataType <> dtObject then Exit;
    supObj := member.AsObject;
    if (supObj.Expression['from'].DataType = dtObject) then
    begin
      item.callFrom.name := supObj.O['from'].S['name'];
      item.callFrom.kind := TLSPSymbolKind(supObj.O['from'].I['kind']);
      item.callFrom.detail := supObj.O['from'].S['detail'];
      item.callFrom.uri := supObj.O['from'].S['uri'];

      // Range
      if (supObj.O['from'].Expression['range'].DataType = dtObject) then
        ReadRangeObj(item.callFrom.range, supObj.O['from'].O['range']);

      // Selection Range
      if (supObj.O['from'].Expression['selectionRange'].DataType = dtObject) then
        ReadRangeObj(item.callFrom.selectionRange, supObj.O['from'].O['selectionRange']);

      // Tags
      if (supObj.O['from'].Expression['tags'].DataType = dtArray) then
      begin
        LArr := supObj.O['from'].A['tags'];
        SetLength(item.callFrom.tags, LArr.Length);
        for j := 0 to LArr.Length - 1 do
        begin
          item.callFrom.tags[j] := LArr.I[j];
        end;
      end;
    end;

    // fromRanges
    if (supObj.Expression['fromRanges'].DataType = dtArray) then
    begin
      LArr := supObj.A['fromRanges'];
      SetLength(item.fromRanges, LArr.Length);
      j := 0;
      for LMember in LArr do
      begin
        ReadRange(item.fromRanges[j], LMember);
        Inc(j);
      end;
    end;
  end;
begin
  Result := TLSPCallHierarchyIncomingCallResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  SetLength(Result.items, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    ReadIncommingCall(Result.items[i], LMember);
    Inc(i);
  end;
end;

function JsonCallHierarchyOutgoingResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string; const path: string = ''): TLSPCallHierarchyOutgoingCallResponse;
var
  LArray: ISuperArray;
  LMember: IMember;
  s: string;
  i: Integer;

  procedure ReadRange(var range: TLSPRange; const supObj: ISuperObject);
  begin
    range.startPos.line := supObj.O['start'].I['line'];
    range.startPos.character := supObj.O['start'].I['character'];
    range.endPos.line := supObj.O['end'].I['line'];
    range.endPos.character := supObj.O['end'].I['character'];
  end;

  procedure ReadOutgoingCall(var item: TLSPCallHierarchyOutgoingCall; const supObj: ISuperObject);
  var
    j: Integer;
    LArr: ISuperArray;
    LMember: IMember;
  begin
    if (supObj.Expression['to'].DataType = dtObject) then
    begin
      item.callTo.name := supObj.O['to'].S['name'];
      item.callTo.kind := TLSPSymbolKind(supObj.O['to'].I['kind']);
      item.callTo.detail := supObj.O['to'].S['detail'];
      item.callTo.uri := supObj.O['to'].S['uri'];

      // Range
      if (supObj.O['to'].Expression['range'].DataType = dtObject) then
        ReadRange(item.callTo.range, supObj.O['to'].O['range']);

      // Selection Range
      if (supObj.O['to'].Expression['selectionRange'].DataType = dtObject) then
        ReadRange(item.callTo.selectionRange, supObj.O['to'].O['selectionRange']);

      // Tags
      if (supObj.O['to'].Expression['tags'].DataType = dtArray) then
      begin
        LArr := supObj.O['to'].A['tags'];
        SetLength(item.callTo.tags, LArr.Length);
        j := 0;
        for LMember in LArr do
        begin
          if LMember.DataType = dtInteger then
          begin
            item.callTo.tags[j] := LMember.AsInteger;
            Inc(j);
          end;
        end;
        if j <> LArr.Length then SetLength(item.callTo.tags, j);
      end;
    end;

    // fromRanges
    if (supObj.Expression['fromRanges'].DataType = dtArray) then
    begin
      LArr := supObj.A['fromRanges'];
      SetLength(item.fromRanges, LArr.Length);
      j := 0;
      for LMember in LArr do
      begin
        if LMember.DataType = dtObject then
        begin
          ReadRange(item.fromRanges[j], LMember.AsObject);
          Inc(j);
        end;
      end;
      if j <> LArr.Length then SetLength(item.fromRanges, j);
    end;
  end;
begin
  Result := TLSPCallHierarchyOutgoingCallResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  SetLength(Result.items, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType = dtObject then
    begin
      ReadOutgoingCall(Result.items[i], LMember.AsObject);
      Inc(i);
    end;
  end;
  if i <> LArray.Length then SetLength(Result.items, i);
end;

function JsonCodeActionResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPCodeActionResponse;
var
  LArray,LArr,LArr2: ISuperArray;
  LRange,LObject: ISuperObject;
  LArrayO,LArrO,LArrO2: ISuperObject;
  LMember,LMember1,LMember2: IMember;
  s,w: string;
  i,j,k: Integer;
  params: TLSPTextDocumentEdit;
  edit: TLSPAnnotatedTextEdit;
  LAction: TLSPCodeAction;
begin
  Result := TLSPCodeActionResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  if LArray.Length = 0 then Exit;

  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;

    LArrayO := LMember.AsObject;

    LAction := TLSPCodeAction.Create;
    if (LArrayO.S['command'] <> '') then
    begin
      // TLSPCommand
      LAction.command.title := LArrayO.S['title'];
      LAction.command.command := LArrayO.S['command'];
      LAction.command.arguments := LArrayO.S['arguments'];
    end
    else
    begin
      // TLSPCodeAction
      LAction.title := LArrayO.S['title'];
      LAction.kind := LArrayO.S['kind'];

      // Diagnostics
      if LArrayO.Expression['diagnostics'].DataType = dtArray then
      begin
        LArr := LArrayO.A['diagnostics'];
        SetLength(LAction.diagnostics, LArr.Length);
        j := 0;
        for LMember1 in LArr do
        begin
          if LMember1.DataType <> dtObject then Continue;
          LArrO := LMember1.AsObject;
          if LArrO.Expression['range'].DataType = dtObject then
          begin
            LRange := LArrO.O['range'];
            LAction.diagnostics[j].range.startPos.line := LRange.O['start'].I['line'];
            LAction.diagnostics[j].range.startPos.character := LRange.O['start'].I['character'];
            LAction.diagnostics[j].range.endPos.line := LRange.O['end'].I['line'];
            LAction.diagnostics[j].range.endPos.character := LRange.O['end'].I['character'];
          end;

          LAction.diagnostics[j].severity := LArrO.I['severity'];
          LAction.diagnostics[j].code := LArrO.I['code'];
          LAction.diagnostics[j].source := LArrO.S['source'];
          LAction.diagnostics[j].messageString := LArrO.S['messageString'];

          // tags[]
          if (LArrO.Expression['tags'].DataType = dtArray) then
          begin
            LArr2 := LArr.O[j].A['tags'];
            SetLength(LAction.diagnostics[j].tags, LArr2.Length);
            k := 0;
            for LMember2 in LArr2 do
            begin
              LAction.diagnostics[j].tags[k] := LArr2.I[k];
              Inc(k);
            end;
          end;

          // relatedInformation[]
          if (LArrO.Expression['relatedInformation'].DataType = dtArray) then
          begin
            LArr2 := LArrO.A['relatedInformation'];
            SetLength(LAction.diagnostics[j].relatedInformation, LArr2.Length);
            k := 0;
            for LMember2 in LArr2 do
            begin
              if LMember2.DataType <> dtObject then Continue;
              LArrO2 := LMember2.AsObject;

              // Message string
              LAction.diagnostics[j].relatedInformation[k].messageString := LArrO2.S['messageString'];

              // Location and range
              if (LArrO2.Expression['location'].DataType = dtObject) and (LArrO2.O['location'].Expression['range'].DataType = dtObject) then
              begin
                LRange := LArrO2.O['location'].O['range'];
                LAction.diagnostics[j].relatedInformation[k].location.uri := LArrO2.O['location'].S['uri'];
                LAction.diagnostics[j].relatedInformation[k].location.range.startPos.line := LRange.O['start'].I['line'];
                LAction.diagnostics[j].relatedInformation[k].location.range.startPos.character := LRange.O['start'].I['character'];
                LAction.diagnostics[j].relatedInformation[k].location.range.endPos.line := LRange.O['end'].I['line'];
                LAction.diagnostics[j].relatedInformation[k].location.range.endPos.character := LRange.O['end'].I['character'];
              end;
              Inc(k);
            end;
          end;
          Inc(j);
        end;
      end;

      LAction.isPreferred := LArray.O[i].B['isPreferred'];
      LAction.disabled.reason := LArray.O[i].O['disabled'].S['reason'];

      // Workspace edit Changes
      if (LArrayO.Expression['edit'].DataType = dtObject) and (LArrayO.O['edit'].Expression['changes'].DataType = dtObject) then
      begin
        LAction.edit.changes.uri := LArrayO.O['edit'].O['changes'].S['uri'];

        if (LArrayO.O['edit'].O['changes'].Expression['values'].DataType = dtArray) then
        begin
          LArr := LArrayO.O['edit'].O['changes'].A['values'];
          SetLength(LAction.edit.changes.values, LArr.Length);
          j := 0;
          for LMember1 in LArr do
          begin
            LArrO := LMember1.AsObject;
            if LArrO.Expression['range'].DataType = dtObject then
            begin
              LRange := LArrO.O['range'];
              LAction.edit.changes.values[j].newText := LArrO.S['newText'];
              LAction.edit.changes.values[j].range.startPos.line := LRange.O['start'].I['line'];
              LAction.edit.changes.values[j].range.startPos.character := LRange.O['start'].I['character'];
              LAction.edit.changes.values[j].range.endPos.line := LRange.O['end'].I['line'];
              LAction.edit.changes.values[j].range.endPos.character := LRange.O['end'].I['character'];
              Inc(j);
            end;
          end;
          if j <> Length(LAction.edit.changes.values) then
            SetLength(LAction.edit.changes.values, j);
        end;
      end;

      // Workspace edit document changes
      if (LArrayO.Expression['edit'].DataType = dtObject) and (LArrayO.O['edit'].Expression['documentChanges'].DataType = dtArray) then
      begin
        LArr := LArrayO.O['edit'].A['documentChanges'];
        SetLength(LAction.edit.documentChanges, LArr.Length);
        j := 0;
        for LMember1 in LArr do
        begin
          if LMember1.DataType <> dtObject then Continue;
          LArrO := LMember1.AsObject;

          params := TLSPTextDocumentEdit.Create;
          if (LArrO.Expression['textDocument'].DataType = dtObject) then
          begin
            TLSPTextDocumentEdit(params).textDocument.uri := LArrO.O['textDocument'].S['uri'];
            if LArrO.O['textDocument'].Expression['version'].DataType = dtInteger then
              TLSPTextDocumentEdit(params).textDocument.version := LArrO.O['textDocument'].I['version']
            else if LArrO.O['textDocument'].Expression['version'].DataType = dtString then
              TLSPTextDocumentEdit(params).textDocument.version := StrToInt(LArrO.O['textDocument'].S['version']);
          end;

          if (LArrO.Expression['edits'].DataType = dtArray) then
          begin
            LArr2 := LArrO.A['edits'];
            SetLength(TLSPTextDocumentEdit(params).edits,LArr2.Length);

            // Retrieve edit's
            k := 0;
            for LMember2 in LArr2 do
            begin
              if LMember2.DataType <> dtObject then Continue;
              LArrO2 := LMember2.AsObject;

              edit := TLSPAnnotatedTextEdit.Create;
              edit.newText := LArrO2.S['newText'];
              edit.annotationId := LArrO2.S['annotationId'];

              // range
              if LArrO2.Expression['range'].DataType = dtObject then
              begin
                LRange := LArrO2.O['range'];
                edit.range.startPos.line := LRange.O['start'].I['line'];
                edit.range.startPos.character := LRange.O['start'].I['character'];
                edit.range.endPos.line := LRange.O['end'].I['line'];
                edit.range.endPos.character := LRange.O['end'].I['character'];
              end;

              TLSPTextDocumentEdit(params).edits[k] := edit;
              Inc(k);
            end;
            if k <> Length(TLSPTextDocumentEdit(params).edits) then
              SetLength(TLSPTextDocumentEdit(params).edits,k);
          end;
          LAction.edit.documentChanges[j] := params;
          Inc(j);
        end;
        if j <> Length(LAction.edit.documentChanges) then
          SetLength(LAction.edit.documentChanges, j);
      end;
    end;

    // Change annotation
    if (LArrayO.Expression['edit'].DataType = dtObject) and (LArrayO.O['edit'].Expression['changeAnnotations'].DataType = dtObject) then
    begin
      LArrayO.O['edit'].O['changeAnnotations'].First;
      w := LArrayO.O['edit'].O['changeAnnotations'].CurrentKey;
      LAction.edit.changeAnnotations.id := w;

      // Get annotations
      if LArrayO.O['edit'].O['changeAnnotations'].Expression[w].DataType = dtObject then
      begin
        LObject := LArrayO.O['edit'].O['changeAnnotations'].O[w];
        LAction.edit.changeAnnotations.values.slabel := LObject.S['label'];
        LAction.edit.changeAnnotations.values.needsConfirmation := LObject.B['needsConfirmation'];
        LAction.edit.changeAnnotations.values.description := LObject.S['description'];
      end;
    end;

    // Command
    if LArrayO.Expression['command'].DataType = dtObject then
    begin
      LAction.command.title := LArrayO.O['command'].S['title'];
      LAction.command.command := LArrayO.O['command'].S['command'];
      LAction.command.arguments := LArrayO.O['command'].S['arguments'];
    end;

    // Data (any)
    if (LArrayO.Check('data')) then
    begin
      if LArrayO.Expression['data'].DataType = dtObject then
        LAction.data := LArrayO.O['data'].AsJSON
      else if LArrayO.Expression['data'].DataType = dtArray then
        LAction.data := LArrayO.A['data'].AsJSON
      else
        LAction.data := LArrayO.V['data'].AsJSON;
    end;

    Result.codeActions.Add(LAction);
    Inc(i);
  end;
end;

function JsonCodeActionResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPCodeAction;
var
  LJsonResult: ISuperObject;
  LRange, LObject: ISuperObject;
  LArr,LArr2: ISuperArray;
  LArrO,LArrO2: ISuperObject;
  LMem,LMem2: IMember;
  s,w: string;
  j,k: Integer;
  params: TLSPTextDocumentEdit;
  edit: TLSPAnnotatedTextEdit;
begin
  Result := TLSPCodeAction.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtObject then Exit;

  Result := TLSPCodeAction.Create;
  LJsonResult := LJson[s].AsObject;

  Result.title := LJsonResult.S['title'];
  Result.kind := LJsonResult.S['kind'];

  // Diagnostics
  if LJsonResult.Expression['diagnostics'].DataType = dtArray then
  begin
    LArr := LJsonResult.A['diagnostics'];
    SetLength(Result.diagnostics, LArr.Length);
    j := 0;
    for LMem in LArr do
    begin
      if LMem.DataType <> dtObject then Continue;
      LArrO := LMem.AsObject;

      if LArrO.Expression['range'].DataType = dtObject then
      begin
        LRange := LArrO.O['range'];
        Result.diagnostics[j].range.startPos.line := LRange.O['start'].I['line'];
        Result.diagnostics[j].range.startPos.character := LRange.O['start'].I['character'];
        Result.diagnostics[j].range.endPos.line := LRange.O['end'].I['line'];
        Result.diagnostics[j].range.endPos.character := LRange.O['end'].I['character'];
      end;

      Result.diagnostics[j].severity := LArrO.I['severity'];
      Result.diagnostics[j].code := LArrO.I['code'];
      Result.diagnostics[j].source := LArrO.S['source'];
      Result.diagnostics[j].messageString := LArrO.S['messageString'];

      // tags[]
      if (LArrO.Expression['tags'].DataType = dtArray) then
      begin
        LArr2 := LArrO.A['tags'];
        SetLength(Result.diagnostics[j].tags, LArr2.Length);
        k := 0;
        for LMem2 in LArr2 do
        begin
          if LMem2.DataType = dtInteger then
          begin
            Result.diagnostics[j].tags[k] := LArr2.I[k];
            Inc(k);
          end;
        end;
        if k <> Length(Result.diagnostics[j].tags) then
          SetLength(Result.diagnostics[j].tags, k);
      end;

      // relatedInformation[]
      if (LArrO.Expression['relatedInformation'].DataType = dtArray) then
      begin
        LArr2 := LArrO.A['relatedInformation'];
        SetLength(Result.diagnostics[j].relatedInformation, LArr2.Length);
        k := 0;
        for LMem2 in LArr2 do
        begin
          if LMem2.DataType <> dtObject then Continue;
          LArrO2 := LMem2.AsObject;

          // Message string
          Result.diagnostics[j].relatedInformation[k].messageString := LArrO2.S['messageString'];

          // Location and range
          if (LArrO2.Expression['location'].DataType = dtObject) then
          begin
            Result.diagnostics[j].relatedInformation[k].location.uri := LArrO2.O['location'].S['uri'];
            if LArrO2.O['location'].Expression['range'].DataType = dtObject then
            begin
              LRange := LArrO2.O['location'].O['range'];
              Result.diagnostics[j].relatedInformation[k].location.range.startPos.line := LRange.O['start'].I['line'];
              Result.diagnostics[j].relatedInformation[k].location.range.startPos.character := LRange.O['start'].I['character'];
              Result.diagnostics[j].relatedInformation[k].location.range.endPos.line := LRange.O['end'].I['line'];
              Result.diagnostics[j].relatedInformation[k].location.range.endPos.character := LRange.O['end'].I['character'];
            end;
          end;
          Inc(k);
        end;
        if k <> Length(Result.diagnostics[j].relatedInformation) then
          SetLength(Result.diagnostics[j].relatedInformation, k);
      end;
      Inc(j);
    end;
  end;

  Result.isPreferred := LJsonResult.B['isPreferred'];
  if LJsonResult.Expression['disabled'].DataType = dtObject then
    Result.disabled.reason := LJsonResult.O['disabled'].S['reason'];

  // Workspace edit Changes
  if (LJsonResult.Expression['edit'].DataType = dtObject) and (LJsonResult.O['edit'].Expression['changes'].DataType = dtObject) then
  begin
    Result.edit.changes.uri := LJsonResult.O['edit'].O['changes'].S['uri'];
    if LJsonResult.O['edit'].O['changes'].Expression['values'].DataType = dtObject then
    begin
      LArr := LJsonResult.O['edit'].O['changes'].A['values'];
      SetLength(Result.edit.changes.values, LArr.Length);
      j := 0;
      for LMem in LArr do
      begin
        if LMem.DataType <> dtObject then Continue;
        LArrO := LMem.AsObject;

        Result.edit.changes.values[j].newText := LArrO.S['newText'];
        if LArrO.Expression['range'].DataType = dtObject then
        begin
          LRange := LArrO.O['range'];
          Result.edit.changes.values[j].range.startPos.line := LRange.O['start'].I['line'];
          Result.edit.changes.values[j].range.startPos.character := LRange.O['start'].I['character'];
          Result.edit.changes.values[j].range.endPos.line := LRange.O['end'].I['line'];
          Result.edit.changes.values[j].range.endPos.character := LRange.O['end'].I['character'];
        end;
        Inc(j);
      end;
    end;
  end;

  // Workspace edit document changes
  if (LJsonResult.Expression['edit'].DataType = dtObject) and (LJsonResult.O['edit'].Expression['documentChanges'].DataType = dtArray) then
  begin
    LArr := LJsonResult.O['edit'].A['documentChanges'];
    SetLength(Result.edit.documentChanges, LArr.Length);
    j := 0;
    for LMem in LArr do
    begin
      if LMem.DataType <> dtObject then Continue;
      LArrO := LMem.AsObject;

      params := TLSPTextDocumentEdit.Create;
      if LArrO.Expression['textDocument'].DataType = dtObject then
      begin
        TLSPTextDocumentEdit(params).textDocument.uri := LArrO.O['textDocument'].S['uri'];
        if LArrO.O['textDocument'].Expression['version'].DataType = dtInteger then
          TLSPTextDocumentEdit(params).textDocument.version := LArrO.O['textDocument'].I['version']
        else if LArrO.O['textDocument'].Expression['version'].DataType = dtString then
          TLSPTextDocumentEdit(params).textDocument.version := StrToInt(LArrO.O['textDocument'].S['version']);
      end;

      if LArrO.Expression['edits'].DataType = dtArray then
      begin
        LArr2 := LArrO.A['edits'];
        SetLength(TLSPTextDocumentEdit(params).edits,LArr2.Length);

        // Retrieve edit's
        k := 0;
        for LMem2 in LArr2 do
        begin
          if LMem2.DataType <> dtObject then Continue;
          LArrO2 := LMem2.AsObject;

          edit := TLSPAnnotatedTextEdit.Create;
          edit.newText := LArrO2.S['newText'];
          edit.annotationId := LArrO2.S['annotationId'];

          // range
          if LArrO2.Expression['range'].DataType = dtObject then
          begin
            LRange := LArrO2.O['range'];
            edit.range.startPos.line := LRange.O['start'].I['line'];
            edit.range.startPos.character := LRange.O['start'].I['character'];
            edit.range.endPos.line := LRange.O['end'].I['line'];
            edit.range.endPos.character := LRange.O['end'].I['character'];
          end;

          TLSPTextDocumentEdit(params).edits[k] := edit;
        end;
      end;
      Result.edit.documentChanges[j] := params;
      Inc(j);
    end;
    if j <> Length(Result.edit.documentChanges) then
      SetLength(Result.edit.documentChanges, j);
  end;

  // Change annotation
  if (LJsonResult.Expression['edit'].DataType = dtObject) and (LJsonResult.O['edit'].Expression['changeAnnotations'].DataType = dtObject) then
  begin
    LJsonResult.O['edit'].O['changeAnnotations'].First;
    w := LJsonResult.O['edit'].O['changeAnnotations'].CurrentKey;
    Result.edit.changeAnnotations.id := w;

    // Get annotations
    if (LJsonResult.O['edit'].O['changeAnnotations'].Expression[w].DataType = dtObject) then
    begin
      LObject := LJsonResult.O['edit'].O['changeAnnotations'].O[w];
      Result.edit.changeAnnotations.values.slabel := LObject.S['label'];
      Result.edit.changeAnnotations.values.needsConfirmation := LObject.B['needsConfirmation'];
      Result.edit.changeAnnotations.values.description := LObject.S['description'];
    end;
  end;

  // Command
  if (LJsonResult.Expression['command'].DataType = dtObject) then
  begin
    Result.command.title := LJsonResult.O['command'].S['title'];
    Result.command.command := LJsonResult.O['command'].S['command'];
    Result.command.arguments := LJsonResult.O['command'].S['arguments'];
  end;

  // Data (any)
  if (LJsonResult.Check('data')) then
  begin
    if LJsonResult.Expression['data'].DataType = dtObject then
      Result.data := LJsonResult.O['data'].AsJSON
    else if LJsonResult.Expression['data'].DataType = dtArray then
      Result.data := LJsonResult.A['data'].AsJSON
    else
      Result.data := LJsonResult.V['data'].AsJSON;
  end;
end;

function JsonCodeLensToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPCodeLensResponse;
var
  LArray: ISuperArray;
  LRange: ISuperObject;
  LArrayO: ISuperObject;
  LMember: IMember;
  s: string;
  lens: TLSPCodeLens;
begin
  Result := TLSPCodeLensResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;

    LArrayO := LMember.AsObject;
    lens := TLSPCodeLens.Create;

    // Range
    if LArrayO.Expression['range'].DataType = dtObject then
    begin
      LRange := LArrayO.O['range'];
      lens.range.startPos.line := LRange.O['start'].I['line'];
      lens.range.startPos.character := LRange.O['start'].I['character'];
      lens.range.endPos.line := LRange.O['end'].I['line'];
      lens.range.endPos.character := LRange.O['end'].I['character'];
    end;

    // Command
    if LArrayO.Expression['command'].DataType = dtObject then
    begin
      lens.command.title := LArrayO.O['command'].S['title'];
      lens.command.command := LArrayO.O['command'].S['command'];
      lens.command.arguments := LArrayO.O['command'].S['arguments'];
    end;

    // Data
    if (LArrayO.Check('data')) then
    begin
      if LArrayO.Expression['data'].DataType = dtObject then
        lens.data := LArrayO.O['data'].AsJSON
      else if LArrayO.Expression['data'].DataType = dtArray then
        lens.data := LArrayO.A['data'].AsJSON
      else
        lens.data := LArrayO.V['data'].AsJSON;
    end;

    Result.codeLensList.Add(lens);
  end;
end;

function JsonCodeLensResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPCodeLens;
var
  LJsonResult: ISuperObject;
  LRange: ISuperObject;
  s: string;
begin
  Result := TLSPCodeLens.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtObject then Exit;
  LJsonResult := LJson[s].AsObject;

  // Range
  if LJsonResult.Expression['range'].DataType = dtObject then
  begin
    LRange := LJsonResult.O['range'];
    Result.range.startPos.line := LRange.O['start'].I['line'];
    Result.range.startPos.character := LRange.O['start'].I['character'];
    Result.range.endPos.line := LRange.O['end'].I['line'];
    Result.range.endPos.character := LRange.O['end'].I['character'];
  end;

  // Command
  if LJsonResult.Expression['command'].DataType = dtObject then
  begin
    Result.command.title := LJsonResult.O['command'].S['title'];
    Result.command.command := LJsonResult.O['command'].S['command'];
    Result.command.arguments := LJsonResult.O['command'].S['arguments'];
  end;

  // Data
  if LJsonResult.Check('data') then
  begin
    if LJsonResult.Expression['data'].DataType = dtObject then
      Result.data := LJsonResult.O['data'].AsJSON
    else if LJsonResult.Expression['data'].DataType = dtArray then
      Result.data := LJsonResult.A['data'].AsJSON
    else
      Result.data := LJsonResult.V['data'].AsJSON;
  end;
end;

function JsonColorPresentationValuesToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string; const path: string = ''): TLSPColorPresentationValues;
var
  LArray,LArr: ISuperArray;
  LArrayObj,LArrObj: ISuperObject;
  LMember,LMem: IMember;
  s: string;
  i,j: Integer;

  procedure ReadTextEdit(var edit: TLSPTextEdit; const supObj: ISuperObject);
  begin
    if not Assigned(supObj) then Exit;

    // NewText
    edit.newText := supObj.S['newText'];

    // Range
    if supObj.Expression['range'].DataType = dtObject then
    begin
      edit.range.startPos.line := supObj.O['range'].O['start'].I['line'];
      edit.range.startPos.character := supObj.O['range'].O['start'].I['character'];
      edit.range.endPos.line := supObj.O['range'].O['end'].I['line'];
      edit.range.endPos.character := supObj.O['range'].O['end'].I['character'];
    end;
  end;
begin
  Result := TLSPColorPresentationValues.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  SetLength(Result.colorPresentations, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;

    // Label
    Result.colorPresentations[i].slabel := LArrayObj.S['label'];

    // Read text edit
    if LArrayObj.Expression['textEdit'].DataType = dtObject then
      ReadTextEdit(Result.colorPresentations[i].textEdit, LArrayObj.O['textEdit']);

    // Additional text edits
    if LArrayObj.Expression['additionalTextEdits'].DataType = dtArray then
    begin
      LArr := LArrayObj.A['additionalTextEdits'];
      SetLength(Result.colorPresentations[i].additionalTextEdits, LArr.Length);
      j := 0;
      for LMem in LArr do
      begin
        if LMem.DataType <> dtObject then Continue;
        LArrObj := LMem.AsObject;

        // Read text edit
        ReadTextEdit(Result.colorPresentations[i].additionalTextEdits[j], LArrObj);
        Inc(j);
      end;
    end;
    Inc(i);
  end;
end;

function JsonCompletionResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPCompletionList;
var
  LArray: ISuperArray;
  s: string;

  procedure ProcessArray(var items: TObjectList<TLSPCompletionItem>; const arr: ISuperArray);
  var
    i,j: Integer;
    LArr1: ISuperArray;
    LArrObj,LArrObj1: ISuperObject;
    LMem,LMem1: IMember;
    LRange: ISuperObject;
    item: TLSPCompletionItem;
    edit: TLSPTextEdit;
  begin
    if not Assigned(arr) then Exit;
    if arr.DataType <> dtArray then Exit;

    i := 0;
    for LMem in arr do
    begin
      if LMem.DataType <> dtObject then Continue;
      LArrObj := LMem.AsObject;
      item := TLSPCompletionItem.Create;

      item.slabel := LArrObj.S['label'];
      item.kind := LArrObj.I['kind'];
      item.detail := LArrObj.S['detail'];
      if LArrObj.Expression['documentation'].DataType = dtObject then
      begin
        item.documentation.kind := LArrObj.O['documentation'].S['kind'];
        item.documentation.value := LArrObj.O['documentation'].S['value'];
      end
      else if LArrObj.Expression['documentation'].DataType = dtString then
      begin
        item.documentation.kind := 'plaintext';
        item.documentation.value := LArrObj.S['documentation'];
      end;
      item.deprecated := LArrObj.B['deprecated'];
      item.preselect := LArrObj.B['preselect'];
      item.sortText := LArrObj.S['sortText'];
      item.filterText := LArrObj.S['filterText'];
      item.insertText := LArrObj.S['insertText'];
      item.insertTextFormat := LArrObj.I['insertTextFormat'];
      item.insertTextMode := LArrObj.I['insertTextMode'];

      // TextEdit object
      if LArrObj.Expression['textEdit'].DataType = dtObject then
      begin
        if LArrObj.O['textEdit'].Expression['replace'].DataType = dtObject then
          item.textEdit := TLSPInsertReplaceEdit.Create
        else
          item.textEdit := TLSPTextEdit.Create;

        item.textEdit.newText := LArrObj.O['textEdit'].S['newText'];
        if (LArrObj.O['textEdit'].Expression['insert'].DataType = dtObject) and
           (LArrObj.O['textEdit'].Expression['replace'].DataType = dtObject) then
        begin
          // Insert range
          LRange := LArrObj.O['textEdit'].O['insert'];
          TLSPInsertReplaceEdit(item.textEdit).insert.startPos.line := LRange.O['start'].I['line'];
          TLSPInsertReplaceEdit(item.textEdit).insert.startPos.character := LRange.O['start'].I['character'];
          TLSPInsertReplaceEdit(item.textEdit).insert.endPos.line := LRange.O['end'].I['line'];
          TLSPInsertReplaceEdit(item.textEdit).insert.endPos.character := LRange.O['end'].I['character'];

          // Replace range
          LRange := LArrObj.O['textEdit'].O['replace'];
          TLSPInsertReplaceEdit(item.textEdit).replace.startPos.line := LRange.O['start'].I['line'];
          TLSPInsertReplaceEdit(item.textEdit).replace.startPos.character := LRange.O['start'].I['character'];
          TLSPInsertReplaceEdit(item.textEdit).replace.endPos.line := LRange.O['end'].I['line'];
          TLSPInsertReplaceEdit(item.textEdit).replace.endPos.character := LRange.O['end'].I['character'];
        end
        else
        begin
          // Range
          if LArrObj.O['textEdit'].Expression['range'].DataType = dtObject then
          begin
            LRange := LArrObj.O['textEdit'].O['range'];
            item.textEdit.range.startPos.line := LRange.O['start'].I['line'];
            item.textEdit.range.startPos.character := LRange.O['start'].I['character'];
            item.textEdit.range.endPos.line := LRange.O['end'].I['line'];
            item.textEdit.range.endPos.character := LRange.O['end'].I['character'];
          end;
        end;
      end;

      // Tags array of integers
      if LArrObj.Expression['tags'].DataType = dtArray then
      begin
        LArr1 := LArrObj.A['tags'];
        SetLength(item.tags, LArr1.Length);
        for j := 0 to LArr1.Length - 1 do
          item.tags[j] := LArr1.I[j];
      end;

      // Additional text edits
      if LArrObj.Expression['additionalTextEdits'].DataType = dtArray then
      begin
        LArr1 := LArrObj.A['additionalTextEdits'];
        for LMem1 in LArr1 do
        begin
          if LMem1.DataType <> dtObject then Continue;
          edit := TLSPTextEdit.Create;
          LArrObj1 := LMem1.AsObject;

          edit.newText := LArrObj1.S['newText'];

          // Range
          if LArrObj1.Expression['range'].DataType = dtObject then
          begin
            LRange := LArrObj1.O['range'];
            edit.range.startPos.line := LRange.O['start'].I['line'];
            edit.range.startPos.character := LRange.O['start'].I['character'];
            edit.range.endPos.line := LRange.O['end'].I['line'];
            edit.range.endPos.character := LRange.O['end'].I['character'];
          end;
          item.additionalTextEdits.Add(edit);
        end;
      end;

      // Commit characters
      if LArrObj.Expression['commitCharacters'].DataType = dtArray then
      begin
        LArr1 := LArrObj.A['commitCharacters'];
        SetLength(item.commitCharacters, LArr1.Length);
        for j := 0 to LArr1.Length - 1 do
          item.commitCharacters[j] := LArr1.S[j];
      end;

      // Command
      if LArrObj.Check('command') then
      begin
        item.command.title := LArrObj.O['command'].S['title'];
        item.command.command := LArrObj.O['command'].S['command'];
        item.command.arguments := LArrObj.O['command'].S['arguments'];
      end;

      // Data (any JSON data that is preserved on a completion item between a completion and a completion resolve request.)
      if LArrObj.Check('data') then
      begin
        if LArrObj.Expression['data'].DataType = dtObject then
          item.data := LArrObj.O['data'].AsJSON
        else if LArrObj.Expression['data'].DataType = dtArray then
          item.data := LArrObj.A['data'].AsJSON
        else
          item.data := LArrObj.V['data'];
      end;

      items.Add(item);
      Inc(i);
    end;
  end;
begin
  Result := TLSPCompletionList.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if (LJson[s].DataType = dtArray) then
  begin
    // Process an array of completion items. The result is assumed to be complete.
    Result.isIncomplete := False;
    LArray := LJson[s].AsArray;
    if LArray.Length > 0 then
      ProcessArray(Result.items, LArray);
    Exit;
  end
  else if LJson[s].DataType = dtObject then
  begin
    // Process Completion list. There may be additional partial results.
    if LJson[s].AsObject.Expression[s].DataType = dtObject then
      Result.isIncomplete := LJson[s].AsObject.O[s].B['isInComplete'];

    if LJson[s].AsObject.Expression['items'].DataType = dtArray then
    begin
      LArray := LJson[s].AsObject.A['items'];
      ProcessArray(Result.items, LArray);
    end;
  end;

  // See if we have partial results.
  s := 'partial result';

  if LJson[s].DataType <> dtArray then Exit;
  LArray := LJson[s].AsArray;

  if (LArray.Length > 0) then
  begin
    // Process an array of completion items. If the result item was a completion list we should add
    // the partial result items to "Result.items".
    ProcessArray(Result.items, LArray);
  end;
end;

function JsonCompletionItemResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string): TLSPCompletionItem;
var
  s: string;
  i: Integer;
  LObject, LRange: ISuperObject;
  LArrayObj: ISuperObject;
  LMember: IMember;
  LArray: ISuperArray;
  edit: TLSPTextEdit;
begin
  Result := TLSPCompletionItem.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtObject then Exit;

  LObject := LJson[s].AsObject;
  Result.slabel := LObject.S['label'];
  Result.kind := LObject.I['kind'];
  Result.detail := LObject.S['detail'];
  if LObject.Expression['documentation'].DataType = dtObject then
  begin
    Result.documentation.kind := LObject.O['documentation'].S['kind'];
    Result.documentation.value := LObject.O['documentation'].S['value'];
  end
  else if LObject.Expression['documentation'].DataType = dtString then
  begin
    Result.documentation.kind := 'plaintext';
    Result.documentation.value := LObject.S['documentation'];
  end;
  Result.deprecated := LObject.B['deprecated'];
  Result.preselect := LObject.B['preselect'];
  Result.sortText := LObject.S['sortText'];
  Result.filterText := LObject.S['filterText'];
  Result.insertText := LObject.S['insertText'];
  Result.insertTextFormat := LObject.I['insertTextFormat'];
  Result.insertTextMode := LObject.I['insertTextMode'];

  // TextEdit object
  if LObject.Expression['textEdit'].DataType = dtObject then
  begin
    if LObject.O['textEdit'].Expression['replace'].DataType = dtObject then
      Result.textEdit := TLSPInsertReplaceEdit.Create
    else
      Result.textEdit := TLSPTextEdit.Create;

    Result.textEdit.newText := LObject.O['textEdit'].S['newText'];
    if (LObject.O['textEdit'].Expression['insert'].DataType = dtObject) and (LObject.O['textEdit'].Expression['replace'].DataType = dtObject) then
    begin
      // Insert range
      LRange := LObject.O['textEdit'].O['insert'];
      TLSPInsertReplaceEdit(Result.textEdit).insert.startPos.line := LRange.O['start'].I['line'];
      TLSPInsertReplaceEdit(Result.textEdit).insert.startPos.character := LRange.O['start'].I['character'];
      TLSPInsertReplaceEdit(Result.textEdit).insert.endPos.line := LRange.O['end'].I['line'];
      TLSPInsertReplaceEdit(Result.textEdit).insert.endPos.character := LRange.O['end'].I['character'];

      // Replace range
      LRange := LObject.O['textEdit'].O['replace'];
      TLSPInsertReplaceEdit(Result.textEdit).replace.startPos.line := LRange.O['start'].I['line'];
      TLSPInsertReplaceEdit(Result.textEdit).replace.startPos.character := LRange.O['start'].I['character'];
      TLSPInsertReplaceEdit(Result.textEdit).replace.endPos.line := LRange.O['end'].I['line'];
      TLSPInsertReplaceEdit(Result.textEdit).replace.endPos.character := LRange.O['end'].I['character'];
    end
    else
    begin
      // Range
      if LObject.O['textEdit'].Expression['range'].DataType = dtObject then
      begin
        LRange := LObject.O['textEdit'].O['range'];
        Result.textEdit.range.startPos.line := LRange.O['start'].I['line'];
        Result.textEdit.range.startPos.character := LRange.O['start'].I['character'];
        Result.textEdit.range.endPos.line := LRange.O['end'].I['line'];
        Result.textEdit.range.endPos.character := LRange.O['end'].I['character'];
      end;
    end;
  end;

  // Tags array of integers
  if LObject.Expression['tags'].DataType = dtArray then
  begin
    LArray := LObject.A['tags'];
    SetLength(Result.tags, LArray.Length);
    for i := 0 to LArray.Length - 1 do
      Result.tags[i] := LArray.I[i];
  end;

  // Additional text edits
  if LObject.Expression['additionalTextEdits'].DataType = dtArray then
  begin
    LArray := LObject.A['additionalTextEdits'];
    for LMember in LArray do
    begin
      if LMember.DataType <> dtObject then Continue;
      edit := TLSPTextEdit.Create;
      LArrayObj := LMember.AsObject;

      edit.newText := LArrayObj.S['newText'];

      // Range
      if LArrayObj.Expression['range'].DataType = dtObject then
      begin
        LRange := LArrayObj.O['range'];
        edit.range.startPos.line := LRange.O['start'].I['line'];
        edit.range.startPos.character := LRange.O['start'].I['character'];
        edit.range.endPos.line := LRange.O['end'].I['line'];
        edit.range.endPos.character := LRange.O['end'].I['character'];
      end;
      Result.additionalTextEdits.Add(edit);
    end;
  end;

  // Commit characters
  if LObject.Expression['commitCharacters'].DataType = dtArray then
  begin
    LArray := LObject.A['commitCharacters'];
    SetLength(Result.commitCharacters, LArray.Length);
    for i := 0 to LArray.Length - 1 do
      Result.commitCharacters[i] := LArray.S[i];
  end;

  // Command
  if LObject.Expression['command'].DataType = dtObject then
  begin
    Result.command.title := LObject.O['command'].S['title'];
    Result.command.command := LObject.O['command'].S['command'];
    Result.command.arguments := LObject.O['command'].S['arguments'];
  end;

  // Data (any JSON data that is preserved on a completion Result between a completion and a completion resolve request.)
  if (LObject.Check('data')) then
  begin
    if LObject.Expression['data'].DataType = dtObject then
      Result.data := LObject.O['data'].AsJSON
    else if LObject.Expression['data'].DataType = dtArray then
      Result.data := LObject.A['data'].AsJSON
    else
      Result.data := LObject.V['data'].AsJSON;
  end;
end;

function JsonConfigurationParamsToObjects(const LJson: ISuperObject): TLSPConfigurationParams;
var
  LArray: ISuperArray;
  LArrayObj: ISuperObject;
  LMember: IMember;
  s: string;
  i: Integer;
begin
  Result := nil;
  
  s := 'params.items';

  if LJson[s].DataType <> dtArray then Exit;

  // Get configurations array
  LArray := LJson[s].AsArray;
  SetLength(Result,LArray.Length);

  // Process array
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;
    Result[i].scopeUri := LArrayObj.S['scopeUri'];
    Result[i].section := LArrayObj.S['section'];
    Inc(i);
  end;
  if i <> Length(Result) then
    SetLength(Result, i);
end;

function JsonDocumentColorValuesToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPColorInformationValues;
var
  LArray: ISuperArray;
  LArrayObj: ISuperObject;
  LMember: IMember;
  LRange: ISuperObject;
  s: string;
  i: Integer;
begin
  Result := TLSPColorInformationValues.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  if LJson[s].DataType <> dtArray then Exit;

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  LArray := LJson[s].AsArray;
  SetLength(Result.colors, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;

    // Colors
    if (LArrayObj.Expression['color'].DataType = dtObject) then
    begin
      Result.colors[i].color.red := LArrayObj.O['color'].F['red'];
      Result.colors[i].color.green := LArrayObj.O['color'].F['green'];
      Result.colors[i].color.blue := LArrayObj.O['color'].F['blue'];
      Result.colors[i].color.alpha := LArrayObj.O['color'].F['alpha'];
    end;

    // Range
    if LArrayObj.Expression['range'].DataType = dtObject then
    begin
      LRange := LArrayObj.O['range'];
      Result.colors[i].range.startPos.line := LRange.O['start'].I['line'];
      Result.colors[i].range.startPos.character := LRange.O['start'].I['character'];
      Result.colors[i].range.endPos.line := LRange.O['end'].I['line'];
      Result.colors[i].range.endPos.character := LRange.O['end'].I['character'];
    end;
    Inc(i);
  end;
  if i <> Length(Result.colors) then
    SetLength(Result.colors, i);
end;

function JsonDocumentFormattingResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string): TLSPTextEditValues;
var
  LArray: ISuperArray;
  LArrayObj: ISuperObject;
  LMember: IMember;
  LRange: ISuperObject;
  s: string;
  i: Integer;
begin
  Result := TLSPTextEditValues.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  SetLength(Result.edits, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;
    Result.edits[i].newText := LArrayObj.S['newText'];
    if LArrayObj.Expression['range'].DataType = dtObject then
    begin
      LRange := LArrayObj.O['range'];
      Result.edits[i].range.startPos.line := LRange.O['start'].I['line'];
      Result.edits[i].range.startPos.character := LRange.O['start'].I['character'];
      Result.edits[i].range.endPos.line := LRange.O['end'].I['line'];
      Result.edits[i].range.endPos.character := LRange.O['end'].I['character'];
    end;
    Inc(i);
  end;
  if i <> Length(Result.edits) then
    SetLength(Result.edits, i);
end;

function JsonDocumentHighlightResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string; const path: string = ''): TLSPDocumentHighlightResponse;
var
  LArray: ISuperArray;
  LArrayObj: ISuperObject;
  LMember: IMember;
  LRange: ISuperObject;
  s: string;
  i: Integer;
begin
  Result := TLSPDocumentHighlightResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  SetLength(Result.list, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;
    Result.list[i].kind := LArrayObj.I['kind'];

    // Range
    if LArrayObj.Expression['range'].DataType = dtObject then
    begin
      LRange := LArrayObj.O['range'];
      Result.list[i].range.startPos.line := LRange.O['start'].I['line'];
      Result.list[i].range.startPos.character := LRange.O['start'].I['character'];
      Result.list[i].range.endPos.line := LRange.O['end'].I['line'];
      Result.list[i].range.endPos.character := LRange.O['end'].I['character'];
    end;
    Inc(i);
  end;
  if i <> Length(Result.list) then
    SetLength(Result.list, i);
end;

function JsonDocumentSymbolsResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string; const path: string = ''): TLSPDocumentSymbolsResponse;
var
  LArray: ISuperArray;
  LMember: IMember;
  s: string;
  i: Integer;
  bDocumentSymbols: Boolean;

  procedure ReadDocumentSymbol(var item: TLSPDocumentSymbol; const supObj: ISuperObject);
  var
    j: Integer;
    LArr: ISuperArray;
    LArrObj: ISuperObject;
    LMem: IMember;
    LRange: ISuperObject;
  begin
    item.name := supObj.S['name'];
    item.detail := supObj.S['detail'];
    item.kind := TLSPSymbolKind(supObj.I['kind']);
    item.isdeprecated := supObj.B['deprecated'];

    if supObj.Expression['tags'].DataType = dtArray then
    begin
      LArr := supObj.A['tags'];
      SetLength(item.tags, LArr.Length);
      for j := 0 to LArr.Length - 1 do
        item.tags[j] := LArr.I[j];
    end;

    // Range
    if supObj.Expression['range'].DataType = dtObject then
    begin
      LRange := supObj.O['range'];
      item.range.startPos.line := LRange.O['start'].I['line'];
      item.range.startPos.character := LRange.O['start'].I['character'];
      item.range.endPos.line := LRange.O['end'].I['line'];
      item.range.endPos.character := LRange.O['end'].I['character'];
    end;

    // Selection range
    if supObj.Expression['selectionRange'].DataType = dtObject then
    begin
      LRange := supObj.O['selectionRange'];
      item.selectionRange.startPos.line := LRange.O['start'].I['line'];
      item.selectionRange.startPos.character := LRange.O['start'].I['character'];
      item.selectionRange.endPos.line := LRange.O['end'].I['line'];
      item.selectionRange.endPos.character := LRange.O['end'].I['character'];
    end;

    // Children
    if supObj.Expression['children'].DataType = dtArray then
    begin
      LArr := supObj.A['children'];
      SetLength(item.children, LArr.Length);
      j := 0;
      for LMem in LArr do
      begin
        if LMem.DataType = dtObject then
        begin
          ReadDocumentSymbol(item.children[j], LMem.AsObject);
          Inc(j);
        end;
      end;
    end;
  end;

  procedure ReadSymbolInformation(var item: TLSPSymbolInformation; const supObj: ISuperObject);
  var
    j: Integer;
    LArr: ISuperArray;
    LRange: ISuperObject;
  begin
    item.name := supObj.S['name'];
    item.containerName := supObj.S['containerName'];
    item.kind := TLSPSymbolKind(supObj.I['kind']);
    item.isdeprecated := supObj.B['deprecated'];

    if supObj.Expression['tags'].DataType = dtArray then
    begin
      LArr := supObj.A['tags'];
      SetLength(item.tags, LArr.Length);
      for j := 0 to LArr.Length - 1 do
        item.tags[j] := LArr.I[j];
    end;

    // Location
    if supObj.Expression['location'].DataType = dtObject then
    begin
      item.location.uri := supObj.O['location'].S['name'];
      if supObj.O['location'].Expression['range'].DataType = dtObject then
      begin
        LRange := supObj.O['location'].O['range'];
        item.location.range.startPos.line := LRange.O['start'].I['line'];
        item.location.range.startPos.character := LRange.O['start'].I['character'];
        item.location.range.endPos.line := LRange.O['end'].I['line'];
        item.location.range.endPos.character := LRange.O['end'].I['character'];
      end;
    end;
  end;
begin
  Result := TLSPDocumentSymbolsResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;
  LArray := LJson[s].AsArray;

  bDocumentSymbols := False;
  for LMember in LArray do
  begin
    bDocumentSymbols := (LMember.DataType = dtObject) and (LMember.AsObject.Expression['range'].DataType = dtObject);
    Break;
  end;

  if (LArray.Length > 0) then
  begin
    if bDocumentSymbols then
    begin
      SetLength(Result.symbols, LArray.Length);
      i := 0;
      for LMember in LArray do
      begin
        if LMember.DataType = dtObject then
        begin
          ReadDocumentSymbol(Result.symbols[i], LMember.AsObject);
          Inc(i);
        end;
      end;
    end
    else
    begin
      SetLength(Result.symbolInformations, LArray.Length);
      i := 0;
      for LMember in LArray do
      begin
        if LMember.DataType = dtObject then
        begin
          ReadSymbolInformation(Result.symbolInformations[i], LMember.AsObject);
          Inc(i);
        end;
      end;
    end;
  end;
end;

function JsonDocumentLinkResolveToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPDocumentLink;
var
  LJsonLink: ISuperObject;
  s: string;
begin
  Result := TLSPDocumentLink.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtObject then Exit;

  Result := TLSPDocumentLink.Create;

  LJsonLink := LJson[s].AsObject;

  Result.target := LJsonLink.S['target'];
  Result.tooltip := LJsonLink.S['tooltip'];
  Result.data := LJsonLink.V['data'];

  // Range
  if LJsonLink.Expression['range'].DataType = dtObject then
  begin
    Result.range.startPos.line := LJsonLink.O['range'].O['start'].I['line'];
    Result.range.startPos.character := LJsonLink.O['range'].O['start'].I['character'];
    Result.range.endPos.line := LJsonLink.O['range'].O['end'].I['line'];
    Result.range.endPos.character := LJsonLink.O['range'].O['end'].I['character'];
  end;
end;

function JsonDocumentLinkResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPDocumentLinkResponse;
var
  LArray: ISuperArray;
  LArrayObj: ISuperObject;
  LMember: IMember;
  link: TLSPDocumentLink;
  s: string;
begin
  Result := TLSPDocumentLinkResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;

    link := TLSPDocumentLink.Create;

    link.target := LArrayObj.S['target'];
    link.tooltip := LArrayObj.S['tooltip'];
    link.data := LArrayObj.V['data'];

    // Range
    if LArrayObj.Expression['range'].DataType = dtObject then
    begin
      link.range.startPos.line := LArrayObj.O['range'].O['start'].I['line'];
      link.range.startPos.character := LArrayObj.O['range'].O['start'].I['character'];
      link.range.endPos.line := LArrayObj.O['range'].O['end'].I['line'];
      link.range.endPos.character := LArrayObj.O['range'].O['end'].I['character'];
    end;

    Result.documentLinks.Add(link);
  end;
end;

function JsonExecuteCommandResult(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): string;
begin
  Result := LJson.S['result'];
  ErrorCode := 0;
  ErrorMessage := '';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;
end;

function JsonFindReferencesResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPFindReferencesResponse;
var
  LArray: ISuperArray;
  s: string;
  i: Integer;

  procedure ReadLocation(var item: TLSPLocation; const supObj: ISuperObject);
  begin
    item.uri := supObj.S['uri'];

    // Range
    if supObj.Expression['range'].DataType = dtObject then
    begin
      item.range.startPos.line := supObj.O['range'].O['start'].I['line'];
      item.range.startPos.character := supObj.O['range'].O['start'].I['character'];
      item.range.endPos.line := supObj.O['range'].O['end'].I['line'];
      item.range.endPos.character := supObj.O['range'].O['end'].I['character'];
    end;
  end;

begin
  Result := TLSPFindReferencesResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  // The response can be null or TArray<TLSPLocation>
  if (LJson[s].DataType = dtArray) then
  begin
    // TArray<TLSPLocation>
    LArray := LJson[s].AsArray;
    if LArray.Length = 0 then Exit;
    if LArray.O[0].S['uri'] <> '' then
    begin
      SetLength(Result.locations, LArray.Length);
      for i := 0 to LArray.Length - 1 do
        ReadLocation(Result.locations[i], LArray.O[i]);
    end;
  end;
end;

function JsonFoldingRangeResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPFoldingRangeResponse;
var
  LArray: ISuperArray;
  LArrayObj: ISuperObject;
  LMember: IMember;
  s: string;
  i: Integer;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if LJson[s].DataType <> dtArray then Exit;

  Result := TLSPFoldingRangeResponse.Create;

  LArray := LJson[s].AsArray;
  SetLength(Result.foldingRanges, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;
    Result.foldingRanges[i].startLine := LArrayObj.I['startLine'];
    Result.foldingRanges[i].endLine := LArrayObj.I['endLine'];
    if LArrayObj.Contains('startCharacter') then
      Result.foldingRanges[i].startCharacter := LArrayObj.I['startCharacter']
    else
      Result.foldingRanges[i].startCharacter := -1;
    if LArrayObj.Contains('endCharacter') then
      Result.foldingRanges[i].endCharacter := LArrayObj.I['endCharacter']
    else
      Result.foldingRanges[i].endCharacter := -1;
    Result.foldingRanges[i].kind := LArrayObj.S['kind'];
    Inc(i);
  end;
  if i <> Length(Result.foldingRanges) then
    setLength(Result.foldingRanges, i);
end;

function JsonGotoResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string; const
    path: string = ''): TLSPGotoResponse;
var
  LArray: ISuperArray;
  s: string;
  i: Integer;

  procedure ReadLocation(var item: TLSPLocation; const supObj: ISuperObject);
  begin
    item.uri := supObj.S['uri'];

    // Range
    if supObj.Expression['range'].DataType = dtObject then
    begin
      item.range.startPos.line := supObj.O['range'].O['start'].I['line'];
      item.range.startPos.character := supObj.O['range'].O['start'].I['character'];
      item.range.endPos.line := supObj.O['range'].O['end'].I['line'];
      item.range.endPos.character := supObj.O['range'].O['end'].I['character'];
    end;
  end;

  procedure ReadLocationLink(var item: TLSPLocationLink; const supObj: ISuperObject);
  var
    LRange: ISuperObject;
  begin
    item.targetUri := supObj.S['targetUri'];

    // Origin selection range
    if supObj.Expression['originSelectionRange'].DataType = dtObject then
    begin
      LRange := supObj.O['originSelectionRange'];
      item.originSelectionRange.startPos.line := LRange.O['start'].I['line'];
      item.originSelectionRange.startPos.character := LRange.O['start'].I['character'];
      item.originSelectionRange.endPos.line := LRange.O['end'].I['line'];
      item.originSelectionRange.endPos.character := LRange.O['end'].I['character'];
    end;

    // Target range
    if supObj.Expression['targetRange'].DataType = dtObject then
    begin
      LRange := supObj.O['targetRange'];
      item.targetRange.startPos.line := LRange.O['start'].I['line'];
      item.targetRange.startPos.character := LRange.O['start'].I['character'];
      item.targetRange.endPos.line := LRange.O['end'].I['line'];
      item.targetRange.endPos.character := LRange.O['end'].I['character'];
    end;

    // Target selection range
    if supObj.Expression['targetSelectionRange'].DataType = dtObject then
    begin
      LRange := supObj.O['targetSelectionRange'];
      item.targetSelectionRange.startPos.line := LRange.O['start'].I['line'];
      item.targetSelectionRange.startPos.character := LRange.O['start'].I['character'];
      item.targetSelectionRange.endPos.line := LRange.O['end'].I['line'];
      item.targetSelectionRange.endPos.character := LRange.O['end'].I['character'];
    end;
  end;
begin
  Result := TLSPGotoResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  if not (LJson[s].DataType in [dtObject,dtArray]) then Exit;

  // The response can be null, TLSPLocation, TArray<TLSPLocation> or TArray<TLSPLocationLink>
  if (LJson[s].DataType = dtArray) then
  begin
    // TArray<TLSPLocation> or TArray<TLSPLocationLink>
    LArray := LJson[s].AsArray;
    if LArray.Length = 0 then Exit;
    if LArray.O[0].S['targetUri'] <> '' then
    begin
      // TArray<TLSPLocationLink>
      SetLength(Result.locationLinks, LArray.Length);
      for i := 0 to LArray.Length - 1 do
        ReadLocationLink(Result.locationLinks[i], LArray.O[i]);
    end
    else if LArray.O[0].S['uri'] <> '' then
    begin
      // TArray<TLSPLocation>
      SetLength(Result.locations, LArray.Length);
      for i := 0 to LArray.Length - 1 do
        ReadLocation(Result.locations[i], LArray.O[i]);
    end;
  end
  else
  begin
    // TLSPLocation or null
    if (LJson[s].DataType = dtObject) and (Ljson[s].AsObject.S['uri'] <> '') then
    begin
      // TLSPLocation
      ReadLocation(Result.location, Ljson[s].AsObject);
    end;
  end;
end;

function JsonHoverResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPHover;
var
  LArray: ISuperArray;
  LMember: IMember;
  s: string;
  i: Integer;

  function HandleLineBreaks(const s: string): string;
  begin
    Result := s;
    if Pos('\.',s) > 0 then Result := StringReplace(s, '\.', '.', [rfReplaceAll]);
    if Pos(#13#10, s) > 0 then Exit;
    if Pos(#10, s) > 0 then
      Result := StringReplace(s, #10, #13#10, [rfReplaceAll]);
  end;
begin
  Result := TLSPHover.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  // Figure out if 'contents' is a MarkedString (string or language-value pair) | MarkedString[] | MarkupContent
  if (LJson[s].DataType = dtObject) and (LJson[s].AsObject.Expression['contents'].DataType = dtArray) then
  begin
    // MarkedString array
    LArray := LJson[s].AsObject.A['contents'];
    SetLength(Result.contentsMarkedArray, LArray.Length);
    i := 0;
    for LMember in LArray do
    begin
      if LMember.DataType = dtObject then
      begin
        // MarkedString = language-value pair
        Result.contentsMarkedArray[i].language := LArray.O[i].S['language'];
        Result.contentsMarkedArray[i].value := HandleLineBreaks(LArray.O[i].S['value']);
      end
      else if LMember.DataType = dtString then
      begin
        // MarkedString = string
        Result.contentsMarkedArray[i].value := HandleLineBreaks(LArray.S[i]);
      end;
      Inc(i);
    end;
  end
  else if (LJson[s].DataType = dtObject) and (LJson[s].AsObject.Expression['contents'].DataType = dtObject) then
  begin
    // Object found (contents: { ... })
    if LJson[s].AsObject.O['contents'].S['language'] <> '' then
    begin
      // Language identifier and a value string
      Result.contentsMarked.language := LJson[s].AsObject.O['contents'].S['language'];
      Result.contentsMarked.value := HandleLineBreaks(LJson[s].AsObject.O['contents'].S['value']);
    end
    else if LJson[s].AsObject.O['contents'].S['kind'] <> '' then
    begin
      // MarkupContent
      Result.contents.kind := LJson[s].AsObject.O['contents'].S['kind'];
      Result.contents.value := HandleLineBreaks(LJson[s].AsObject.O['contents'].S['value']);
    end;
  end
  else if (LJson[s].DataType = dtObject) then
  begin
    // MarkedString = string
    Result.contents.value := HandleLineBreaks(LJson[s].AsObject.S['contents']);
  end;
end;

function JsonInitializeResultToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPInitializeResultParams;
var
  s: string;
  LObject,LCapabilities: ISuperObject;

  procedure ReadStringArray(const s: string; var arr: TArray<string>);
  var
    i: Integer;
    LArray: ISuperArray;
    LMember: IMember;
  begin
    if (LJson[s].DataType = dtArray) then
    begin
      LArray := LJson[s].AsArray;
      SetLength(arr,LArray.Length);
      i := 0;
      for LMember in LArray do
      begin
        if LMember.DataType = dtString then
        begin
          arr[i] := LMember.AsString;
          Inc(i);
        end;
      end;
      if i <> Length(arr) then
        SetLength(arr, i);
    end;
  end;

  procedure ReadDocumentSelector(const s: string; var arr: TArray<TLSPDocumentFilter>);
  var
    i: Integer;
    LArray: ISuperArray;
    LMember: IMember;
  begin
    // "documentSelector". A document selector to identify the scope of the registration.
    if (LJson[s].DataType = dtArray) then
    begin
      LArray := LJson[s].AsArray;
      SetLength(arr, LArray.Length);
      i := 0;
      for LMember in LArray do
      begin
        if LMember.DataType = dtObject then
        begin
          arr[i].language := LMember.AsObject.S['language'];
          arr[i].scheme := LMember.AsObject.S['scheme'];
          arr[i].pattern := LMember.AsObject.S['pattern'];
          Inc(i);
        end;
      end;
      if i <> Length(arr) then
        SetLength(arr, i);
    end;
  end;

  procedure ReadFileOperation(const s: string; var fileOp: TLSPFileOperationRegistrationOptions);
  var
    i: Integer;
    LArray: ISuperArray;
    LMember: IMember;
  begin
    if (LJson[s].DataType = dtObject) then
    begin
      if LJson[s].AsObject.Expression['filters'].DataType = dtArray then
      begin
        // "fileOperations". The options to register for file operations.
        LArray := LJson[s].AsObject.A['filters'];
        SetLength(fileOp.filters, LArray.Length);
        i := 0;
        for LMember in LArray do
        begin
          if LMember.DataType = dtObject then
          begin
            fileOp.filters[i].scheme := LMember.AsObject.S['scheme'];
            if LMember.AsObject.Expression['pattern'].DataType = dtObject then
            begin
              fileOp.filters[i].pattern.glob := LMember.AsObject.O['pattern'].S['glob'];
              fileOp.filters[i].pattern.matches := LMember.AsObject.O['pattern'].S['matches'];
              if LMember.AsObject.O['pattern'].Expression['options'].DataType = dtObject then
                fileOp.filters[i].pattern.options.ignoreCase := LMember.AsObject.O['pattern'].O['options'].B['ignoreCase'];
            end;
            Inc(i);
          end;
        end;
        if i <> Length(fileOp.filters) then
          SetLength(fileOp.filters, i);
      end;
    end;
  end;

begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error'].AsObject.S['message'] <> '') then
  begin
    ErrorCode := LJson['error'].AsObject.I['code'];
    ErrorMessage := LJson['error'].AsObject.S['message'];
  end;

  Result := TLSPInitializeResultParams.Create;

  // Server info
  Result.serverInfo := TLSPServerInfo.Create;
  s := 'result.clientInfo';
  if LJson[s].DataType = dtObject then
  begin
    LObject := LJson[s].AsObject;
    Result.serverInfo.name := LObject.S['.name'];
    Result.serverInfo.version := LObject.S['version'];
  end;

  // Server capabilities
  Result.capabilities := TLSPServerCapabilities.Create;

  // textDocumentSync
  //
  // Defines how text documents are synced.

  s := 'result.capabilities';

  if LJson[s].DataType <> dtObject then Exit;

  LCapabilities := LJson[s].AsObject;

  if LCapabilities.Expression['textDocumentSync'].DataType = dtObject then
  begin
    LObject := LCapabilities.O['textDocumentSync'];
    Result.capabilities.textDocumentSync := TLSPTextDocumentServerSyncOptions.Create;

    // "change". Change notifications are sent to the server.
    Result.capabilities.textDocumentSync.change := LObject.I['change'];

    // "openClose". Open and close notifications are sent to the server.
    Result.capabilities.textDocumentSync.openClose := LObject.B['openClose'];

    // "willSave". WillSave notifications are sent to the server.
    Result.capabilities.textDocumentSync.willSave := LObject.B['willSave'];

    // "willSaveWaitUntil". WillSaveWaitUntil notifications are sent to the server.
    Result.capabilities.textDocumentSync.willSaveWaitUntil := LObject.B['willSaveWaitUntil'];

    // "save". Save notifications are sent to the server.
    if LObject.Expression['save'].DataType = dtObject then
    begin
      Result.capabilities.textDocumentSync.save := TLSPSaveOption.Create;
      Result.capabilities.textDocumentSync.save.includeText := LObject.O['save'].B['includeText'];
    end
    else if (LObject.Expression['save'].DataType = dtBoolean) and (LObject.B['save']) then
    begin
      Result.capabilities.textDocumentSync.save := TLSPSaveOption.Create;
      Result.capabilities.textDocumentSync.save.includeText := False;
    end;
  end
  else if LCapabilities.Expression['textDocumentSync'].DataType = dtInteger then
  begin
    Result.capabilities.textDocumentSync := TLSPTextDocumentServerSyncOptions.Create;

    // "change". Change notifications are sent to the server.
    Result.capabilities.textDocumentSync.change := LCapabilities.I['textDocumentSync'];

    Result.capabilities.textDocumentSync.openClose := (Result.capabilities.textDocumentSync.change > 0);
  end;

  // completionProvider
  //
  // The server provides completion support.
  if LCapabilities.Expression['completionProvider'].DataType = dtObject then
  begin
    LObject := LCapabilities.O['completionProvider'].AsObject;
    Result.capabilities.completionProvider := TLSPCompletionOptions.Create;

    // "triggerCharacters". If code complete should automatically be triggered on characters
    ReadStringArray(s+'.completionProvider.triggerCharacters',  Result.capabilities.completionProvider.triggerCharacters);

    // "allCommitCharacters". The list of all possible characters that commit a completion.
    ReadStringArray(s+'.completionProvider.allCommitCharacters',  Result.capabilities.completionProvider.allCommitCharacters);

    // "resolveProvider". The server provides support to resolve additional information for a completion item.
    Result.capabilities.completionProvider.resolveProvider := LObject.B['resolveProvider'];

    // "workDoneProgress".
    Result.capabilities.completionProvider.workDoneProgress := LObject.B['workDoneProgress'];
  end;

  // hoverProvider
  //
  // The server provides hover support.
  if (LCapabilities.Expression['hoverProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['hoverProvider'].DataType = dtBoolean) and LCapabilities.B['hoverProvider']) then
  begin
    Result.capabilities.hoverProvider := TLSPHoverOptions.Create;
    if LCapabilities.Expression['hoverProvider'].DataType = dtObject then
      Result.capabilities.hoverProvider.workDoneProgress := LCapabilities.O['hoverProvider'].B['workDoneProgress'];
  end;

  // signatureHelpProvider
  //
  // The server provides signature help support.
  if (LCapabilities.Expression['signatureHelpProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['signatureHelpProvider'].DataType = dtBoolean) and LCapabilities.B['signatureHelpProvider']) then
  begin
    Result.capabilities.signatureHelpProvider := TLSPSignatureHelpOptions.Create;
    if LCapabilities.Expression['signatureHelpProvider'].DataType = dtObject then
    begin
      // "triggerCharacters". The characters that trigger signature help automatically.
      ReadStringArray(s+'.signatureHelpProvider.triggerCharacters',  Result.capabilities.signatureHelpProvider.triggerCharacters);

      // "retriggerCharacters". List of characters that re-trigger signature help.
      ReadStringArray(s+'.signatureHelpProvider.retriggerCharacters',  Result.capabilities.signatureHelpProvider.retriggerCharacters);

      // "workDoneProgress".
      Result.capabilities.signatureHelpProvider.workDoneProgress := LCapabilities.O['signatureHelpProvider'].B['workDoneProgress'];
    end;
  end;

  // declarationProvider
  //
  // The server provides go to declaration support.
  if (LCapabilities.Expression['declarationProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['declarationProvider'].DataType = dtBoolean) and LCapabilities.B['declarationProvider']) then
  begin
    Result.capabilities.declarationProvider := TLSPDeclarationRegistrationOptions.Create;
    if LCapabilities.Expression['declarationProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['declarationProvider'];

      // "id". The id used to register the request.
      Result.capabilities.declarationProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.declarationProvider.documentSelector', Result.capabilities.declarationProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.declarationProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // definitionProvider
  //
  // The server provides goto definition support.
  if (LCapabilities.Expression['definitionProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['definitionProvider'].DataType = dtBoolean) and LCapabilities.B['definitionProvider']) then
  begin
    Result.capabilities.definitionProvider := TLSPDefinitionOptions.Create;

    if LCapabilities.Expression['definitionProvider'].DataType = dtObject then
    begin
      // "workDoneProgress".
      Result.capabilities.definitionProvider.workDoneProgress := LCapabilities.O['definitionProvider'].B['workDoneProgress'];
    end;
  end;

  // typeDefinitionProvider
  //
  // The server provides goto type definition support.
  if (LCapabilities.Expression['typeDefinitionProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['typeDefinitionProvider'].DataType = dtBoolean) and LCapabilities.B['typeDefinitionProvider']) then
  begin
    Result.capabilities.typeDefinitionProvider := TLSPTypeDefinitionRegistrationOptions.Create;
    if LCapabilities.Expression['typeDefinitionProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['typeDefinitionProvider'];

      // "id". The id used to register the request.
      Result.capabilities.typeDefinitionProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.typeDefinitionProvider.documentSelector', Result.capabilities.typeDefinitionProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.typeDefinitionProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // implementationProvider
  //
  // The server provides goto implementation support.
  if (LCapabilities.Expression['implementationProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['implementationProvider'].DataType = dtBoolean) and LCapabilities.B['implementationProvider']) then
  begin
    Result.capabilities.implementationProvider := TLSPImplementationRegistrationOptions.Create;
    if LCapabilities.Expression['implementationProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['implementationProvider'];

      // "id". The id used to register the request.
      Result.capabilities.implementationProvider.id := Lobject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.implementationProvider.documentSelector', Result.capabilities.implementationProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.implementationProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // referencesProvider
  //
  // The server provides find references support.
  if (LCapabilities.Expression['referencesProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['referencesProvider'].DataType = dtBoolean) and LCapabilities.B['referencesProvider']) then
  begin
    Result.capabilities.referencesProvider := TLSPReferenceOptions.Create;

    if LCapabilities.Expression['referencesProvider'].DataType = dtObject then
    begin
      // "workDoneProgress".
      Result.capabilities.referencesProvider.workDoneProgress := LCapabilities.O['referencesProvider'].B['workDoneProgress'];
    end;
  end;

  // documentHighlightProvider
  //
  // The server provides document highlight support.
  if (LCapabilities.Expression['documentHighlightProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['documentHighlightProvider'].DataType = dtBoolean) and LCapabilities.B['documentHighlightProvider']) then
  begin
    Result.capabilities.documentHighlightProvider := TLSPDocumentHighlightOptions.Create;

    if LCapabilities.Expression['documentHighlightProvider'].DataType = dtObject then
    begin
      // "workDoneProgress".
      Result.capabilities.documentHighlightProvider.workDoneProgress := LCapabilities.O['documentHighlightProvider'].B['workDoneProgress'];
    end;
  end;

  // documentSymbolProvider
  //
  // The server provides document symbol support.
  if (LCapabilities.Expression['documentSymbolProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['documentSymbolProvider'].DataType = dtBoolean) and LCapabilities.B['documentSymbolProvider']) then
  begin
    Result.capabilities.documentSymbolProvider := TLSPDocumentSymbolOptions.Create;

    if LCapabilities.Expression['documentSymbolProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['documentSymbolProvider'];

      // "label". A human-readable string that is shown when multiple outlines trees
      // are shown for the same document.
      Result.capabilities.documentSymbolProvider.slabel := LObject.S['label'];

      // "workDoneProgress".
      Result.capabilities.documentSymbolProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // codeActionProvider
  //
  // The server provides code actions. The `CodeActionOptions` return type is only valid if the client signals
  // code action literal support via the property `textDocument.codeAction.codeActionLiteralSupport`.
  if (LCapabilities.Expression['codeActionProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['codeActionProvider'].DataType = dtBoolean) and LCapabilities.B['codeActionProvider']) then
  begin
    Result.capabilities.codeActionProvider := TLSPCodeActionOptions.Create;

    if LCapabilities.Expression['codeActionProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['codeActionProvider'];

      // "codeActionKinds". The characters that trigger signature help automatically.
      ReadStringArray(s+'.codeActionProvider.codeActionKinds',  Result.capabilities.codeActionProvider.codeActionKinds);

      // "resolveProvider". The server provides support to resolve additional information for a code action.
      Result.capabilities.codeActionProvider.resolveProvider := LObject.B['resolveProvider'];

      // "workDoneProgress".
      Result.capabilities.codeActionProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // codeLensProvider
  //
  // The server provides CodeLens.
  if (LCapabilities.Expression['codeLensProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['codeLensProvider'].DataType = dtBoolean) and LCapabilities.B['codeLensProvider']) then
  begin
    Result.capabilities.codeLensProvider := TLSPCodeLensOptions.Create;

    if LCapabilities.Expression['codeLensProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['codeLensProvider'];

      // "resolveProvider". The server provides support to resolve additional information for a code action.
      Result.capabilities.codeLensProvider.resolveProvider := LObject.B['resolveProvider'];

      // "workDoneProgress".
      Result.capabilities.codeLensProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // documentLinkProvider
  //
  // The server provides document link support.
  if (LCapabilities.Expression['documentLinkProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['documentLinkProvider'].DataType = dtBoolean) and LCapabilities.B['documentLinkProvider']) then
  begin
    Result.capabilities.documentLinkProvider := TLSPDocumentLinkOptions.Create;

    if LCapabilities.Expression['documentLinkProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['documentLinkProvider'];

      // "resolveProvider". The server provides support to resolve additional information for a code action.
      Result.capabilities.documentLinkProvider.resolveProvider := LObject.B['resolveProvider'];

      // "workDoneProgress".
      Result.capabilities.documentLinkProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // colorProvider
  //
  // The server provides color provider support.
  if (LCapabilities.Expression['colorProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['colorProvider'].DataType = dtBoolean) and LCapabilities.B['colorProvider']) then
  begin
    Result.capabilities.colorProvider := TLSPDocumentColorRegistrationOptions.Create;
    if LCapabilities.Expression['colorProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['colorProvider'];

      // "id". The id used to register the request.
      Result.capabilities.colorProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.colorProvider.documentSelector', Result.capabilities.colorProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.colorProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // documentFormattingProvider
  //
  // The server provides document formatting.
  if (LCapabilities.Expression['documentFormattingProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['documentFormattingProvider'].DataType = dtBoolean) and LCapabilities.B['documentFormattingProvider']) then
  begin
    Result.capabilities.documentFormattingProvider := TLSPDocumentFormattingOptions.Create;

    if LCapabilities.Expression['documentFormattingProvider'].DataType = dtObject then
    begin
      // "workDoneProgress".
      Result.capabilities.documentFormattingProvider.workDoneProgress := LCapabilities.O['documentFormattingProvider'].B['workDoneProgress'];
    end;
  end;

  // documentRangeFormattingProvider
  //
  // The server provides document range formatting.
  if (LCapabilities.Expression['documentRangeFormattingProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['documentRangeFormattingProvider'].DataType = dtBoolean) and LCapabilities.B['documentRangeFormattingProvider']) then
  begin
    Result.capabilities.documentRangeFormattingProvider := TLSPDocumentRangeFormattingOptions.Create;

    if LCapabilities.Expression['documentRangeFormattingProvider'].DataType = dtObject then
    begin
      // "workDoneProgress".
      Result.capabilities.documentRangeFormattingProvider.workDoneProgress := LCapabilities.O['documentRangeFormattingProvider'].B['workDoneProgress'];
    end;
  end;

  // documentOnTypeFormattingProvider
  //
  // The server provides document formatting on typing.
  if (LCapabilities.Expression['documentOnTypeFormattingProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['documentOnTypeFormattingProvider'].DataType = dtBoolean) and LCapabilities.B['documentOnTypeFormattingProvider']) then
  begin
    Result.capabilities.documentOnTypeFormattingProvider := TLSPDocumentOnTypeFormattingOptions.Create;

    if LCapabilities.Expression['documentOnTypeFormattingProvider'].DataType = dtObject then
    begin
      // "firstTriggerCharacter". A character on which formatting should be triggered, like `}`.
      Result.capabilities.documentOnTypeFormattingProvider.firstTriggerCharacter := LCapabilities.O['documentOnTypeFormattingProvider'].S['firstTriggerCharacter'];

      // "moreTriggerCharacter". More trigger characters.
      ReadStringArray(s+'.documentOnTypeFormattingProvider.moreTriggerCharacter', Result.capabilities.documentOnTypeFormattingProvider.moreTriggerCharacter);
    end;
  end;

  // renameProvider
  //
  // The server provides rename support. RenameOptions may only be specified if the client states that it supports
  // `prepareSupport` in its initial `initialize` request.
  if (LCapabilities.Expression['renameProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['renameProvider'].DataType = dtBoolean) and LCapabilities.B['renameProvider']) then
  begin
    Result.capabilities.renameProvider := TLSPRenameOptions.Create;

    if LCapabilities.Expression['renameProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['renameProvider'];

      // "prepareProvider". Renames should be checked and tested before being executed.
      Result.capabilities.renameProvider.prepareProvider := LObject.B['prepareProvider'];

      // "workDoneProgress".
      Result.capabilities.renameProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // foldingRangeProvider
  //
  // The server provides folding provider support.
  if (LCapabilities.Expression['foldingRangeProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['foldingRangeProvider'].DataType = dtBoolean) and LCapabilities.B['foldingRangeProvider']) then
  begin
    Result.capabilities.foldingRangeProvider := TLSPFoldingRangeRegistrationOptions.Create;
    if LCapabilities.Expression['foldingRangeProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['foldingRangeProvider'];

      // "id". The id used to register the request.
      Result.capabilities.foldingRangeProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.foldingRangeProvider.documentSelector', Result.capabilities.foldingRangeProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.foldingRangeProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // executeCommandProvider
  //
  // The server provides execute command support.
  if LCapabilities.Expression['executeCommandProvider'].DataType = dtObject then
  begin
    Result.capabilities.executeCommandProvider := TLSPExecuteCommandOptions.Create;

    // "commands". The server provides support to resolve additional information for a code action.
    ReadStringArray(s+'.executeCommandProvider.commands', Result.capabilities.executeCommandProvider.commands);

    // "workDoneProgress".
    Result.capabilities.executeCommandProvider.workDoneProgress := LCapabilities.O['executeCommandProvider'].B['workDoneProgress'];
  end;

  // selectionRangeProvider
  //
  // The server provides selection range support.
  if (LCapabilities.Expression['selectionRangeProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['selectionRangeProvider'].DataType = dtBoolean) and LCapabilities.B['selectionRangeProvider']) then
  begin
    Result.capabilities.selectionRangeProvider := TLSPSelectionRangeRegistrationOptions.Create;
    if LCapabilities.Expression['selectionRangeProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['selectionRangeProvider'];

      // "id". The id used to register the request.
      Result.capabilities.selectionRangeProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.selectionRangeProvider.documentSelector', Result.capabilities.selectionRangeProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.selectionRangeProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // workspaceSymbolProvider
  //
  // The server provides workspace symbol support.
  if (LCapabilities.Expression['workspaceSymbolProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['workspaceSymbolProvider'].DataType = dtBoolean) and LCapabilities.B['workspaceSymbolProvider']) then
  begin
    Result.capabilities.workspaceSymbolProvider := TLSPWorkspaceSymbolOptions.Create;

    if LCapabilities.Expression['workspaceSymbolProvider'].DataType = dtObject then
    begin
      // "workDoneProgress".
      Result.capabilities.workspaceSymbolProvider.workDoneProgress := LCapabilities.O['workspaceSymbolProvider'].B['workDoneProgress'];
    end;
  end;

  // linkedEditingRangeProvider
  //
  // The server provides linked editing range support.
  if (LCapabilities.Expression['linkedEditingRangeProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['linkedEditingRangeProvider'].DataType = dtBoolean) and LCapabilities.B['linkedEditingRangeProvider']) then
  begin
    Result.capabilities.linkedEditingRangeProvider := TLSPLinkedEditingRangeRegistrationOptions.Create;
    if LCapabilities.Expression['linkedEditingRangeProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['linkedEditingRangeProvider'];

      // "id". The id used to register the request.
      Result.capabilities.linkedEditingRangeProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.linkedEditingRangeProvider.documentSelector', Result.capabilities.linkedEditingRangeProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.linkedEditingRangeProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // callHierarchyProvider
  //
  // The server provides call hierarchy support.
  if (LCapabilities.Expression['callHierarchyProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['callHierarchyProvider'].DataType = dtBoolean) and LCapabilities.B['callHierarchyProvider']) then
  begin
    Result.capabilities.callHierarchyProvider := TLSPCallHierarchyRegistrationOptions.Create;
    if LCapabilities.Expression['callHierarchyProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['callHierarchyProvider'];

      // "id". The id used to register the request.
      Result.capabilities.callHierarchyProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.callHierarchyProvider.documentSelector', Result.capabilities.callHierarchyProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.callHierarchyProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // semanticTokensProvider
  //
  // The server provides semantic tokens support.
  if (LCapabilities.Expression['semanticTokensProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['semanticTokensProvider'].DataType = dtBoolean) and LCapabilities.B['semanticTokensProvider']) then
  begin
    Result.capabilities.semanticTokensProvider := TLSPSemanticTokensRegistrationOptions.Create;
    if LCapabilities.Expression['semanticTokensProvider'].DataType = dtObject then
    begin
      LObject := LCapabilities.O['semanticTokensProvider'];

      // "id". The id used to register the request.
      Result.capabilities.semanticTokensProvider.id := LObject.S['id'];

      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.semanticTokensProvider.documentSelector', Result.capabilities.semanticTokensProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.semanticTokensProvider.workDoneProgress := LObject.B['workDoneProgress'];
    end;
  end;

  // monikerProvider
  //
  // Whether server provides moniker support.
  if (LCapabilities.Expression['monikerProvider'].DataType = dtObject) or
     ((LCapabilities.Expression['monikerProvider'].DataType = dtBoolean) and LCapabilities.B['monikerProvider']) then
  begin
    Result.capabilities.monikerProvider := TLSPMonikerRegistrationOptions.Create;
    if LCapabilities.Expression['monikerProvider'].DataType = dtObject then
    begin
      // "documentSelector". A document selector to identify the scope of the registration.
      ReadDocumentSelector(s+'.monikerProvider.documentSelector', Result.capabilities.monikerProvider.documentSelector);

      // "workDoneProgress".
      Result.capabilities.monikerProvider.workDoneProgress := LCapabilities.O['monikerProvider'].B['workDoneProgress'];
    end;
  end;

  // Workspace
  //
  // Workspace specific server capabilities
  if LCapabilities.Expression['workspace'].DataType = dtObject then
  begin
    if (LCapabilities.O['workspace'].Expression['workspaceFolders'].DataType = dtObject) then
    begin
      LObject := LCapabilities.O['workspace'];

      // "workspaceFolders - supported".
      Result.capabilities.workspace.workspaceFolders.supported := LObject.O['workspaceFolders'].B['supported'];

      // "workspaceFolders - changeNotifications".
      Result.capabilities.workspace.workspaceFolders.changeNotifications := LObject.O['workspaceFolders'].V['changeNotifications'];
    end;

    // FileOperations @since 3.16.0
    if LCapabilities.O['workspace'].Expression['fileOperations'].DataType = dtObject then
    begin
      Result.capabilities.workspace.fileOperations := TLSPServerCapabilitiesFileOperations.Create;

      // "FileOperations - didCreate".
      if LCapabilities.O['workspace'].O['fileOperations'].Expression['didCreate'].DataType = dtObject then
      begin
        Result.capabilities.workspace.fileOperations.didCreate := TLSPFileOperationRegistrationOptions.Create;
        ReadFileOperation(s+'.workspace.fileOperations.didCreate', Result.capabilities.workspace.fileOperations.didCreate);
      end;

      // "FileOperations - willCreate".
      if LCapabilities.O['workspace'].O['fileOperations'].Expression['willCreate'].DataType = dtObject then
      begin
        Result.capabilities.workspace.fileOperations.willCreate := TLSPFileOperationRegistrationOptions.Create;
        ReadFileOperation(s+'.workspace.fileOperations.willCreate', Result.capabilities.workspace.fileOperations.willCreate);
      end;

      // "FileOperations - didRename".
      if LCapabilities.O['workspace'].O['fileOperations'].Expression['didRename'].DataType = dtObject then
      begin
        Result.capabilities.workspace.fileOperations.didRename := TLSPFileOperationRegistrationOptions.Create;
        ReadFileOperation(s+'.workspace.fileOperations.didRename', Result.capabilities.workspace.fileOperations.didRename);
      end;

      // "FileOperations - willRename".
      if LCapabilities.O['workspace'].O['fileOperations'].Expression['willrename'].DataType = dtObject then
      begin
        Result.capabilities.workspace.fileOperations.willRename := TLSPFileOperationRegistrationOptions.Create;
        ReadFileOperation(s+'.workspace.fileOperations.willRename', Result.capabilities.workspace.fileOperations.willRename);
      end;

      // "FileOperations - didDelete".
      if LCapabilities.O['workspace'].O['fileOperations'].Expression['didDelete'].DataType = dtObject then
      begin
        Result.capabilities.workspace.fileOperations.didDelete := TLSPFileOperationRegistrationOptions.Create;
        ReadFileOperation(s+'.workspace.fileOperations.didDelete', Result.capabilities.workspace.fileOperations.didDelete);
      end;

      // "FileOperations - willDelete".
      if LCapabilities.O['workspace'].O['fileOperations'].Expression['willDelete'].DataType = dtObject then
      begin
        Result.capabilities.workspace.fileOperations.willDelete := TLSPFileOperationRegistrationOptions.Create;
        ReadFileOperation(s+'.workspace.fileOperations.willDelete', Result.capabilities.workspace.fileOperations.willDelete);
      end;
    end;
  end;

  // Experimental server capabilities.
  //
  // experimental: any;
  if (LCapabilities.Check('experimental')) then
  begin
    if LCapabilities.Expression['experimental'].DataType = dtObject then
      Result.Capabilities.experimentalValue := LCapabilities.O['experimental'].AsJSON
    else if LCapabilities.Expression['experimental'].DataType = dtArray then
      Result.capabilities.experimentalValue := LCapabilities.A['experimental'].AsJSON
    else
      Result.capabilities.experimentalValue := LCapabilities.V['experimental'].AsJSON;
  end;
end;

function JsonLinkedEditingRangesToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): TLSPLinkedEditingRanges;
var
  LArray: ISuperArray;
  LMember: IMember;
  s: string;
  i: Integer;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtObject then Exit;

  Result := TLSPLinkedEditingRanges.Create;

  if LJson[s].AsObject.Expression['ranges'].DataType = dtArray then
  begin
    LArray := LJson[s].AsObject.A['ranges'];
    SetLength(Result.ranges, LArray.Length);
    i := 0;
    for LMember in LArray do
    begin
      if LMember.DataType = dtObject then
      begin
        // Range
        Result.ranges[i].startPos.line := LMember.AsObject.O['start'].I['line'];
        Result.ranges[i].startPos.character := LMember.AsObject.O['start'].I['character'];
        Result.ranges[i].endPos.line := LMember.AsObject.O['end'].I['line'];
        Result.ranges[i].endPos.character := LMember.AsObject.O['end'].I['character'];
        Inc(i);
      end;
    end;
    if i <> Length(Result.ranges) then
      SetLength(Result.ranges, i);
  end;
end;

function JsonLogTraceParamsToObject(const LJson: ISuperObject): TLSPLogTraceParams;
var
  s: string;
begin
  Result := TLSPLogTraceParams.Create;

  s := 'params';

  if LJson[s].DataType <> dtObject then Exit;

  // Get message
  Result.msg := LJson[s+'."message"'].AsString;

  // Get verbose
  Result.verbose := LJson[s+'."verbose"'].AsString;
end;

function JsonMonikerToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPMonikerResult;
var
  LArray: ISuperArray;
  LMember: IMember;
  s,w: string;
  i: Integer;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtArray then Exit;

  Result := TLSPMonikerResult.Create;
  LArray := LJson[s].AsArray;

  SetLength(Result.monikers, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType = dtObject then
    begin
      Result.monikers[i].scheme := LArray.O[i].S['scheme'];
      Result.monikers[i].identifier := LArray.O[i].S['identifier'];
      Result.monikers[i].unique := LArray.O[i].S['unique'];
      Result.monikers[i].kind := LArray.O[i].S['kind'];
      Inc(i);
    end;
  end;
  if i <> Length(Result.monikers) then
    SetLength(Result.monikers, i);
end;

function JsonPrepareCallHierarchyResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string): TLSPPrepareCallHierarchyResponse;
var
  LArray,LArr: ISuperArray;
  LRange: ISuperObject;
  LObject: ISuperObject;
  LMember: IMember;
  s: string;
  i,j: Integer;
begin
  Result := TLSPPrepareCallHierarchyResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;

  SetLength(Result.items, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType = dtObject then
    begin
      LObject := LMember.AsObject;
      Result.items[i].name := LObject.S['name'];
      Result.items[i].kind := TLSPSymbolKind(LObject.I['kind']);
      Result.items[i].detail := LObject.S['detail'];
      Result.items[i].uri := LObject.S['uri'];

      // Range
      if LObject.Expression['range'].DataType = dtObject then
      begin
        LRange := LObject.O['range'];
        Result.items[i].range.startPos.line := LRange.O['start'].I['line'];
        Result.items[i].range.startPos.character := LRange.O['start'].I['character'];
        Result.items[i].range.endPos.line := LRange.O['end'].I['line'];
        Result.items[i].range.endPos.character := LRange.O['end'].I['character'];
      end;

      // Selection Range
      if LObject.Expression['selectionRange'].DataType = dtObject then
      begin
        LRange := LObject.O['selectionRange'];
        Result.items[i].selectionRange.startPos.line := LRange.O['start'].I['line'];
        Result.items[i].selectionRange.startPos.character := LRange.O['start'].I['character'];
        Result.items[i].selectionRange.endPos.line := LRange.O['end'].I['line'];
        Result.items[i].selectionRange.endPos.character := LRange.O['end'].I['character'];
      end;

      // Tags
      if LArray.O[i].Expression['tags'].DataType = dtArray then
      begin
        LArr := LArray.O[i].A['tags'];
        SetLength(Result.items[i].tags, LArr.Length);
        for j := 0 to LArr.Length - 1 do
        begin
          Result.items[i].tags[j] := LArr.I[j];
        end;
      end;
      Inc(i);
    end;
  end;
  if i <> Length(Result.items) then
    SetLength(Result.items, i);
end;

function JsonProgressToken(const LJson: ISuperObject): string;
begin
  // Get token
  Result := LJson['params.token'].AsString;
end;

function JsonProgressParamsToObject(const kind: Integer; const LJson: ISuperObject): TLSPBaseParams;
var
  s: string;
  n: Integer;
  LObject: ISuperObject;
begin
  case TLSPKind(kind) of
    lspWorkspaceSymbol:
    begin
      Result := TLSPWorkspaceSymbolInformationParam.Create;
      TLSPWorkspaceSymbolInformationParam(Result).values := JsonWorkspaceSymbolResultToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspCallHierarchyIncommingCalls:
    begin
      Result := JsonCallHierarchyIncommingResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspCallHierarchyOutgoingCalls:
    begin
      Result := JsonCallHierarchyOutgoingResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspColorPresentation:
    begin
      Result := JsonColorPresentationValuesToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspDocumentColor:
    begin
      Result := JsonDocumentColorValuesToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspDocumentHighlight:
    begin
      Result := JsonDocumentHighlightResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspDocumentSymbol:
    begin
      Result := JsonDocumentSymbolsResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspDocumentLink:
    begin
      Result := JsonDocumentLinkResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspFoldingRange:
    begin
      Result := JsonFoldingRangeResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspGotoDeclaration,
    lspGotoDefinition,
    lspGotoTypeDefinition,
    lspGotoImplementation:
    begin
      Result := JsonGotoResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspMoniker:
    begin
      Result := JsonMonikerToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspSelectionRange:
    begin
      Result := JsonSelectionRangeResponseToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspSemanticTokensFull:
    begin
      Result := JsonSemanticTokensFullToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    lspSemanticTokensFullDelta:
    begin
      Result := JsonSemanticTokensFullDeltaToObject(LJson, n, s, 'params.value');
      Exit;
    end;
    else
      Result := TLSPProgressParams.Create;
  end;

  // Get token
  TLSPProgressParams(Result).token := LJson['params.token'].AsString;

  s := 'params.value';
  if LJson[s].DataType <> dtObject then Exit;

  LObject := LJson[s].AsObject;

  // Get value data
  TLSPProgressParams(Result).value.kind := LObject.S['kind'];
  TLSPProgressParams(Result).value.title := LObject.S['title'];
  TLSPProgressParams(Result).value.cancellable := LObject.B['cancellable'];
  TLSPProgressParams(Result).value.msg := LObject.S['message'];
  TLSPProgressParams(Result).value.percentage := LObject.I['percentage'];
end;

function JsonPublishDiagnosticsToObject(const LJson: ISuperObject): TLSPPublishDiagnosticsParams;
var
  LArray,LArray2,LArray3: ISuperArray;
  LRange: ISuperObject;
  LMember,LMem: IMember;
  LObject,LObj: ISuperObject;
  s: string;
  i,j,k: Integer;
begin
  Result := TLSPPublishDiagnosticsParams.Create;
  
  s := 'params';

  if LJson[s].DataType <> dtObject then Exit;

  // Document uri
  Result.uri := LJson[s].AsObject.S['uri'];

  // Optional version number
  Result.version := LJson[s].AsObject.I['version'];

  if LJson[s].AsObject.Expression['diagnostics'].DataType <> dtArray then Exit;

  // Array of diagnostic information
  LArray := LJson[s].AsObject.A['diagnostics'];
  SetLength(Result.diagnostics, LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LObject := LMember.AsObject;

    // The range at which the message applies.
    if LObject.Expression['range'].DataType = dtObject then
    begin
      LRange := LObject.O['range'];
      Result.diagnostics[i].range.startPos.line := LRange.O['start'].I['line'];
      Result.diagnostics[i].range.startPos.character := LRange.O['start'].I['character'];
      Result.diagnostics[i].range.endPos.line := LRange.O['end'].I['line'];
      Result.diagnostics[i].range.endPos.character := LRange.O['end'].I['character'];
    end;

    // The diagnostic's severity.
    Result.diagnostics[i].severity := LObject.I['severity'];

    // The diagnostic's code, which might appear in the user interface.
    if LObject.Expression['code'].DataType = dtInteger then
      Result.diagnostics[i].code := LObject.I['code']
    else if LObject.Expression['code'].DataType = dtString then
      Result.diagnostics[i].code := LObject.S['code'];

    // An optional property to describe the error code
    if LObject.Expression['codeDescription'].DataType = dtObject then
      Result.diagnostics[i].codeDescription.href := LObject.O['codeDescription'].S['href'];

    // A human-readable string describing the source of this diagnostic
    Result.diagnostics[i].source := LObject.S['source'];

    // The diagnostic's message.
    Result.diagnostics[i].messageString := LObject.S['message'];

    // Additional metadata about the diagnostic.
    if LObject.Expression['tags'].DataType = dtArray then
    begin
      LArray2 := LObject.A['tags'];
      SetLength(Result.diagnostics[i].tags, LArray2.Length);
      for j := 0 to LArray2.Length - 1 do
      begin
        Result.diagnostics[i].tags[j] := LArray2.I[j];
      end;
    end;

    // An array of related diagnostic information
    if LObject.Expression['relatedInformation'].DataType = dtArray then
    begin
      LArray3 := LObject.A['relatedInformation'];
      SetLength(Result.diagnostics[i].relatedInformation, LArray3.Length);
      k := 0;
      for LMem in LArray3 do
      begin
        if LMem.DataType <> dtObject then Continue;
        LObj := LMem.AsObject;
        if LObj.Expression['location'].DataType = dtObject then
        begin
          Result.diagnostics[i].relatedInformation[k].location.uri := LObj.O['location'].S['uri'];

          // The location of this related diagnostic information.
          if LObj.O['location'].Expression['range'].DataType = dtArray then
          LRange := LObj.O['location'].O['range'];
          Result.diagnostics[i].relatedInformation[k].location.range.startPos.line := LRange.O['start'].I['line'];
          Result.diagnostics[i].relatedInformation[k].location.range.startPos.character := LRange.O['start'].I['character'];
          Result.diagnostics[i].relatedInformation[k].location.range.endPos.line := LRange.O['end'].I['line'];
          Result.diagnostics[i].relatedInformation[k].location.range.endPos.character := LRange.O['end'].I['character'];
        end;

        // The message of this related diagnostic information.
        Result.diagnostics[i].relatedInformation[k].messageString := LObj.S['messageString'];
        Inc(k);
      end;
      if k <> Length(Result.diagnostics[i].relatedInformation) then
        SetLength(Result.diagnostics[i].relatedInformation, k);
    end;

    // Data (any)
    if (LObject.Check('data')) then
    begin
      if LObject.Expression['data'].DataType = dtObject then
        Result.diagnostics[i].data := LObject.O['data'].AsJSON
      else if LObject.Expression['data'].DataType = dtArray then
        Result.diagnostics[i].data := LObject.A['data'].AsJSON
      else
        Result.diagnostics[i].data := LObject.V['data'].AsJSON;
    end;
    Inc(i);
  end;
  if i <> Length(Result.diagnostics) then
    SetLength(Result.diagnostics, i);
end;

function JsonReadInitializeToClientCapabilities(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPClientCapabilities;
var
  s,w: string;
  LObject: ISuperObject;

  procedure ReadStringArray(const s: string; var arr: TArray<string>);
  var
    i: Integer;
    LArray: ISuperArray;
    LMem: IMember;
  begin
    if (LJson[s].DataType = dtArray) then
    begin
      LArray := LJson[s].AsArray;
      SetLength(arr,LArray.Length);
      i := 0;
      for LMem in LArray do
      begin
        if LMem.DataType = dtString then
        begin
          arr[i] := LMem.AsString;
          Inc(i);
        end;
      end;
      if i <> Length(arr) then
        SetLength(arr, i);
    end;
  end;

  procedure ReadIntegerArray(const s: string; var arr: TArray<Integer>);
  var
    i: Integer;
    LArray: ISuperArray;
    LMem: IMember;
  begin
    if (LJson[s].DataType = dtArray) then
    begin
      LArray := LJson[s].AsArray;
      SetLength(arr,LArray.Length);
      i := 0;
      for LMem in LArray do
      begin
        if LMem.DataType = dtInteger then
        begin
          arr[i] := LMem.AsInteger;
          Inc(i);
        end;
      end;
      if i <> Length(arr) then
        SetLength(arr, i);
    end;
  end;

  procedure ReadDocumentSelector(const s: string; var arr: TArray<TLSPDocumentFilter>);
  var
    i: Integer;
    LArray: ISuperArray;
    LMem: IMember;
  begin
    // "documentSelector". A document selector to identify the scope of the registration.
    if (LJson[s].DataType = dtArray) then
    begin
      LArray := LJson[s].AsArray;
      SetLength(arr, LArray.Length);
      i := 0;
      for LMem in LArray do
      begin
        if LMem.DataType = dtObject then
        begin
          arr[i].language := LMem.AsObject.S['language'];
          arr[i].scheme := LMem.AsObject.S['scheme'];
          arr[i].pattern := LMem.AsObject.S['pattern'];
          Inc(i);
        end;
      end;
      if i <> Length(arr) then
        SetLength(arr, i);
    end;
  end;

  procedure ReadFileOperation(const s: string; var fileOp: TLSPFileOperationRegistrationOptions);
  var
    i: Integer;
    LArray: ISuperArray;
    LObj: ISuperObject;
    LMem: IMember;
  begin
    if (LJson[s].DataType = dtObject) then
    begin
      if (LJson[s].AsObject.Expression['filters'].DataType = dtArray) then
      begin
        // "fileOperations". The options to register for file operations.
        LArray := LJson[s].AsObject.A['filters'];
        SetLength(fileOp.filters, LArray.Length);
        i := 0;
        for LMem in LArray do
        begin
          if LMem.DataType = dtObject then
          begin
            LObj := LMem.AsObject;
            fileOp.filters[i].scheme := Lobj.AsObject.S['scheme'];
            if LObj.Expression['pattern'].DataType = dtObject then
            begin
              fileOp.filters[i].pattern.glob := LObj.O['pattern'].S['glob'];
              fileOp.filters[i].pattern.matches := LObj.O['pattern'].S['matches'];
              if LObj.O['pattern'].Expression['options'].DataType = dtObject then
                fileOp.filters[i].pattern.options.ignoreCase := LObj.O['pattern'].O['options'].B['ignoreCase'];
            end;
            Inc(i);
          end;
        end;
        if i <> Length(fileOp.filters) then
          SetLength(fileOp.filters, i);
      end;
    end;
  end;

begin
  Result := nil;

  s := 'params.capabilities';

  if LJson[s].DataType <> dtObject then Exit;

  // Create client capabilities object
  Result := TLSPClientCapabilities.Create;

  // textDocument
  //
  // Defines capabilities the editor / tool provides on text documents.
  s := 'params.capabilities.textDocument';
  if (LJson[s].DataType = dtObject) then
  begin
    Result.textDocument := TLSPTextDocumentClientCapabilities.Create;

    // textDocument/synchronization
    w := 'params.capabilities.textDocument.synchronization';
    if (LJson[w].DataType = dtObject) then
    begin
      Result.textDocument.synchronization := TLSPTextDocumentSyncClientCapabilities.Create;

      LObject := LJson[w].AsObject;
      if Assigned(LObject) then
      begin
        Result.textDocument.synchronization.dynamicRegistration := LObject.B['dynamicRegistration'];
        Result.textDocument.synchronization.didSave := LObject.B['didSave'];
        Result.textDocument.synchronization.willSave := LObject.B['willSave'];
        Result.textDocument.synchronization.willSaveWaitUntil := LObject.B['willSaveWaitUntil'];
      end;
    end;

    // textDocument/completion
    w := 'params.capabilities.textDocument.completion';
    if (LJson[w].DataType = dtObject) then
    begin
      Result.textDocument.completion := TLSPCompletionClientCapabilities.Create;

      LObject := LJson[w].AsObject;
      Result.textDocument.completion.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.completion.contextSupport := LObject.B['contextSupport'];

      // textDocument/completion/completionItem
      if (LJson[w].AsObject.Expression['completionItem'].DataType = dtObject) then
      begin
        Result.textDocument.completion.completionItem := TLSPClientCompletionItem.Create;

        LObject := LJson[w+'.completionItem'].AsObject;
        Result.textDocument.completion.completionItem.snippetSupport := LObject.B['snippetSupport'];
        Result.textDocument.completion.completionItem.commitCharactersSupport := LObject.B['commitCharactersSupport'];
        Result.textDocument.completion.completionItem.deprecatedSupport := LObject.B['deprecatedSupport'];
        Result.textDocument.completion.completionItem.preselectSupport := LObject.B['preselectSupport'];

        ReadStringArray(w+'.completionItem.documentationFormat', Result.textDocument.completion.completionItem.documentationFormat);

        // textDocument/completion/completionItem/tagSupport
        if (LJson[w+'.completionItem'].AsObject.Expression['tagSupport'].DataType = dtObject) then
        begin
          Result.textDocument.completion.completionItem.tagSupport := TLSPTagSupport.Create;
          ReadIntegerArray(w+'.completionItem.tagSupport.valueSet', Result.textDocument.completion.completionItem.tagSupport.valueSet);
        end;
      end;

      // textDocument/completion/completionItemKind
      if (LJson[w].AsObject.Expression['completionItemKind'].DataType = dtObject) then
      begin
        Result.textDocument.completion.completionItemKind := TLSPCompletionItemKindValues.Create;
        ReadIntegerArray(w+'.completionItemKind.valueSet', Result.textDocument.completion.completionItemKind.valueSet);
      end;
    end;

    // textDocument/hover
    w := 'params.capabilities.textDocument.hover';
    if (LJson[w].DataType = dtObject) then
    begin
      Result.textDocument.hover := TLSPHoverClientCapabilities.Create;

      LObject := LJson[w].AsObject;
      Result.textDocument.hover.dynamicRegistration := LObject.B['dynamicRegistration'];
      ReadStringArray(w+'.contentFormat', Result.textDocument.hover.contentFormat);
    end;

    // textDocument/signatureHelp
    w := 'params.capabilities.textDocument.signatureHelp';
    if (LJson[w].DataType = dtObject) then
    begin
      Result.textDocument.signatureHelp := TLSPSignatureHelpClientCapabilities.Create;

      LObject := LJson[w].AsObject;
      Result.textDocument.signatureHelp.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.signatureHelp.contextSupport := LObject.B['contextSupport'];

      // textDocument/signatureHelp/signatureInformation
      LObject := LJson[w+'.signatureInformation'].AsObject;
      if Assigned(LObject) then
      begin
        Result.textDocument.signatureHelp.signatureInformation := TLSPClientSignatureInformation.Create;

        Result.textDocument.signatureHelp.signatureInformation.activeParameterSupport := LObject.B['activeParameterSupport'];
        ReadStringArray(w+'.signatureInformation.documentationFormat', Result.textDocument.signatureHelp.signatureInformation.documentationFormat);

        // textDocument/signatureHelp/signatureInformation/parameterInformation
        if LObject.Expression['parameterInformation'].DataType = dtObject then
        begin
          Result.textDocument.signatureHelp.signatureInformation.parameterInformation := TLSPClientParameterInformation.Create;
          Result.textDocument.signatureHelp.signatureInformation.parameterInformation.labelOffsetSupport := LObject.O['parameterInformation'].B['labelOffsetSupport'];
        end;
      end;
    end;

    // textDocument/declaration
    w := 'params.capabilities.textDocument.declaration';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.declaration := TLSPDeclarationClientCapabilities.Create;

      Result.textDocument.declaration.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.declaration.linkSupport := LObject.B['linkSupport'];
    end;

    // textDocument/definition
    w := 'params.capabilities.textDocument.definition';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.definition := TLSPDefinitionClientCapabilities.Create;

      Result.textDocument.definition.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.definition.linkSupport := LObject.B['linkSupport'];
    end;

    // textDocument/typeDefinition
    w := 'params.capabilities.textDocument.typeDefinition';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.typeDefinition := TLSPTypeDefinitionClientCapabilities.Create;

      Result.textDocument.typeDefinition.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.typeDefinition.linkSupport := LObject.B['linkSupport'];
    end;

    // textDocument/implementation
    w := 'params.capabilities.textDocument.implementation';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.fimplementation := TLSPImplementationClientCapabilities.Create;

      Result.textDocument.fimplementation.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.fimplementation.linkSupport := LObject.B['linkSupport'];
    end;

    // textDocument/references
    w := 'params.capabilities.textDocument.references';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.references := TLSPReferenceClientCapabilities.Create;
      Result.textDocument.references.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/documentHighlight
    w := 'params.capabilities.textDocument.documentHighlight';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.documentHighlight := TLSPDocumentHighlightClientCapabilities.Create;
      Result.textDocument.documentHighlight.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/documentSymbol
    w := 'params.capabilities.textDocument.documentSymbol';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.documentSymbol := TLSPDocumentSymbolClientCapabilities.Create;
      Result.textDocument.documentSymbol.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.documentSymbol.hierarchicalDocumentSymbolSupport := LObject.B['hierarchicalDocumentSymbolSupport'];

      if LObject.Expression['symbolKind'].DataType = dtObject then
      begin
        Result.textDocument.documentSymbol.symbolKind := TLSPSymbolKindValues.Create;
        ReadIntegerArray(w+'.symbolKind.valueSet', Result.textDocument.documentSymbol.symbolKind.valueSet);
      end;
    end;

    // textDocument/codeAction
    w := 'params.capabilities.textDocument.codeAction';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.codeAction := TLSPCodeActionClientCapabilities.Create;
      Result.textDocument.codeAction.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.codeAction.isPreferredSupport := LObject.B['isPreferredSupport'];
      Result.textDocument.codeAction.disabledSupport := LObject.B['disabledSupport'];
      Result.textDocument.codeAction.dataSupport := LObject.B['dataSupport'];
      Result.textDocument.codeAction.honorsChangeAnnotations := LObject.B['honorsChangeAnnotations'];

      LObject := LJson[w+'.codeActionLiteralSupport'].AsObject;
      if Assigned(LObject) then
      begin
        Result.textDocument.codeAction.codeActionLiteralSupport := TLSPCodeActionLiteralSupport.Create;

        if LObject.Expression['codeActionKind'].DataType = dtObject then
        begin
          Result.textDocument.codeAction.codeActionLiteralSupport.codeActionKind := TLSPCodeActionKindValues.Create;
          ReadStringArray(w+'.codeActionLiteralSupport.codeActionKind.valueSet', Result.textDocument.codeAction.codeActionLiteralSupport.codeActionKind.valueSet);
        end;
      end;

      if (LJson[w].AsObject.Expression['resolveSupport'].DataType = dtObject) then
      begin
        Result.textDocument.codeAction.resolveSupport := TLSPResolveSupport.Create;
        ReadStringArray(w+'.resolveSupport.properties', Result.textDocument.codeAction.resolveSupport.properties);
      end;
    end;

    // textDocument/codeLens
    w := 'params.capabilities.textDocument.codeLens';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.codeLens := TLSPCodeLensClientCapabilities.Create;
      Result.textDocument.codeLens.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/documentLink
    w := 'params.capabilities.textDocument.documentLink';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.documentLink := TLSPDocumentLinkClientCapabilities.Create;
      Result.textDocument.documentLink.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.documentLink.tooltipSupport := LObject.B['tooltipSupport'];
    end;

    // textDocument/colorProvider
    w := 'params.capabilities.textDocument.colorProvider';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.colorProvider := TLSPDocumentColorClientCapabilities.Create;
      Result.textDocument.colorProvider.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/formatting
    w := 'params.capabilities.textDocument.formatting';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.formatting := TLSPDocumentFormattingClientCapabilities.Create;
      Result.textDocument.formatting.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/rangeFormatting
    w := 'params.capabilities.textDocument.rangeFormatting';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.rangeFormatting := TLSPDocumentRangeFormattingClientCapabilities.Create;
      Result.textDocument.rangeFormatting.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/onTypeFormatting
    w := 'params.capabilities.textDocument.onTypeFormatting';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.onTypeFormatting := TLSPDocumentOnTypeFormattingClientCapabilities.Create;
      Result.textDocument.onTypeFormatting.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/rename
    w := 'params.capabilities.textDocument.rename';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.rename := TLSPRenameClientCapabilities.Create;
      Result.textDocument.rename.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.rename.prepareSupport := LObject.B['prepareSupport'];
      Result.textDocument.rename.honorsChangeAnnotations := LObject.B['honorsChangeAnnotations'];

      if LObject.Expression['prepareSupportDefaultBehavior'].DataType = dtObject then
      begin
        Result.textDocument.rename.prepareSupportDefaultBehavior := TLSPPrepareSupportDefaultBehavior.Create;
        Result.textDocument.rename.prepareSupportDefaultBehavior.Identifier := LObject.O['prepareSupportDefaultBehavior'].I['identifier'];
      end;
    end;

    // textDocument/publishDiagnostics
    w := 'params.capabilities.textDocument.publishDiagnostics';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.publishDiagnostics := TLSPPublishDiagnosticsClientCapabilities.Create;
      Result.textDocument.publishDiagnostics.relatedInformation := LObject.B['relatedInformation'];
      Result.textDocument.publishDiagnostics.versionSupport := LObject.B['versionSupport'];

      if LObject.Expression['tagSupport'].DataType = dtObject then
      begin
        Result.textDocument.publishDiagnostics.tagSupport := TLSPTagSupportValues.Create;
        ReadIntegerArray(w+'.tagSupport.valueSet', Result.textDocument.publishDiagnostics.tagSupport.valueSet);
      end;
    end;

    // textDocument/foldingRange
    w := 'params.capabilities.textDocument.foldingRange';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.foldingRange := TLSPFoldingRangeClientCapabilities.Create;
      Result.textDocument.foldingRange.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.textDocument.foldingRange.lineFoldingOnly := LObject.B['lineFoldingOnly'];
      Result.textDocument.foldingRange.rangeLimit := LObject.I['rangeLimit'];
    end;

    // textDocument/selectionRange
    w := 'params.capabilities.textDocument.selectionRange';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.selectionRange := TLSPSelectionRangeClientCapabilities.Create;
      Result.textDocument.selectionRange.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/linkedEditingRange
    w := 'params.capabilities.textDocument.linkedEditingRange';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.linkedEditingRange := TLSPLinkedEditingRangeClientCapabilities.Create;
      Result.textDocument.linkedEditingRange.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/callHierarchy
    w := 'params.capabilities.textDocument.callHierarchy';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.callHierarchy := TLSPCallHierarchyClientCapabilities.Create;
      Result.textDocument.callHierarchy.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;

    // textDocument/semanticTokens
    w := 'params.capabilities.textDocument.semanticTokens';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.semanticTokens := TLSPSemanticTokensClientCapabilities.Create;
      Result.textDocument.semanticTokens.dynamicRegistration := LObject.B['dynamicRegistration'];
      ReadStringArray(w+'.tokenTypes', Result.textDocument.semanticTokens.tokenTypes);
      ReadStringArray(w+'.tokenModifiers', Result.textDocument.semanticTokens.tokenModifiers);
      ReadStringArray(w+'.formats', Result.textDocument.semanticTokens.formats);
      Result.textDocument.semanticTokens.overlappingTokenSupport := LObject.B['overlappingTokenSupport'];
      Result.textDocument.semanticTokens.multilineTokenSupport := LObject.B['multilineTokenSupport'];

      LObject := LJson[w+'.requests'].AsObject;
      if Assigned(LObject) then
      begin
        Result.textDocument.semanticTokens.requests := TLSPRequests.Create;

        if (LJson[w+'.requests.range'].DataType = dtBoolean) then
          Result.textDocument.semanticTokens.requests.range := LObject.B['range'];

        if (LJson[w+'.requests.full'].DataType = dtBoolean) then
        begin
          if LJson[w+'.requests.full'].AsBoolean = True then
            Result.textDocument.semanticTokens.requests.semanticTokensType := TLSPSemanticTokenTypes.semtokenFull
          else
            Result.textDocument.semanticTokens.requests.semanticTokensType := TLSPSemanticTokenTypes.semtokenFullFalse;
        end
        else if (LJson[w+'.requests.full'].DataType = dtObject) then
        begin
          if (LJson[w+'.requests.full.delta'].DataType = dtBoolean) then
          begin
            if LJson[w+'.requests.full.delta'].AsBoolean = True then
              Result.textDocument.semanticTokens.requests.semanticTokensType := TLSPSemanticTokenTypes.semtokenDelta
            else
              Result.textDocument.semanticTokens.requests.semanticTokensType := TLSPSemanticTokenTypes.semtokenDeltaFalse;
          end;
        end;
      end;
    end;

    // textDocument/moniker
    w := 'params.capabilities.textDocument.moniker';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.textDocument.moniker := TLSPMonikerClientCapabilities.Create;
      Result.textDocument.moniker.dynamicRegistration := LObject.B['dynamicRegistration'];
    end;
  end;

  // workspace
  //
  // Workspace specific client capabilities.
  s := 'params.capabilities.workspace';
  LObject := LJson[s].AsObject;
  if Assigned(LObject) then
  begin
    Result.workspace := TLSPWorkspace.Create;

    // workspace/applyEdit
    Result.workspace.applyEdit := LObject.B['applyEdit'];

    // workspace/workspaceEdit
    if LObject.Expression['workspaceEdit'].DataType = dtObject then
    begin
      Result.workspace.workspaceEdit := TLSPWorkspaceEditClientCapabilities.Create;
      Result.workspace.workspaceEdit.documentChanges := LObject.O['workspaceEdit'].B['documentChanges'];
      ReadStringArray(s+'.workspaceEdit.resourceOperations', Result.workspace.workspaceEdit.resourceOperations);
    end;

    // workspace/didChangeConfiguration
    if LObject.Expression['didChangeConfiguration'].DataType = dtObject then
    begin
      Result.workspace.didChangeConfiguration := TLSPDidChangeConfigurationClientCapabilities.Create;
      Result.workspace.didChangeConfiguration.dynamicRegistration := LObject.O['didChangeConfiguration'].B['dynamicRegistration'];
    end;

    // workspace/didChangeWatchedFiles
    if LObject.Expression['didChangeWatchedFiles'].DataType = dtObject then
    begin
      Result.workspace.didChangeWatchedFiles := TLSPDidChangeWatchedFilesClientCapabilities.Create;
      Result.workspace.didChangeWatchedFiles.dynamicRegistration := LObject.O['didChangeWatchedFiles'].B['dynamicRegistration'];
    end;

    // workspace/symbol
    if LObject.Expression['symbol'].DataType = dtObject then
    begin
      Result.workspace.symbol := TLSPWorkspaceSymbolClientCapabilities.Create;
      Result.workspace.symbol.dynamicRegistration := LObject.O['symbol'].B['dynamicRegistration'];

      if LObject.O['symbol'].Expression['symbolKind'].DataType = dtObject then
      begin
        Result.workspace.symbol.symbolKind := TLSPSymbolKindValues.Create;
        ReadIntegerArray(s+'.symbol.symbolKind.valueSet', Result.workspace.symbol.symbolKind.valueSet);
      end;
    end;

    // workspace/executeCommand
    if LObject.Expression['executeCommand'].DataType = dtObject then
    begin
      Result.workspace.executeCommand := TLSPExecuteCommandClientCapabilities.Create;
      Result.workspace.executeCommand.dynamicRegistration := LObject.O['executeCommand'].B['dynamicRegistration'];
    end;

    // workspace/workspaceFolders
    Result.workspace.workspaceFolders := LObject.B['workspaceFolders'];

    // workspace/configuration
    Result.workspace.configuration := LObject.B['configuration'];

    // workspace/semanticTokens
    if LObject.Expression['semanticTokens'].DataType = dtObject then
    begin
      Result.workspace.semanticTokens := TLSPSemanticTokensWorkspaceClientCapabilities.Create;
      Result.workspace.semanticTokens.refreshSupport := LObject.O['semanticTokens'].B['refreshSupport'];
    end;

    // workspace/codeLens
    if LObject.Expression['codeLens'].DataType = dtObject then
    begin
      Result.workspace.codeLens := TLSPCodeLensWorkspaceClientCapabilities.Create;
      Result.workspace.codeLens.refreshSupport := LObject.O['codeLens'].B['refreshSupport'];
    end;

    // workspace/fileOperations
    w := s + '.fileOperations';
    LObject := LJson[w].AsObject;
    if Assigned(LObject) then
    begin
      Result.workspace.fileOperations := TLSPFileOperations.Create;
      Result.workspace.fileOperations.dynamicRegistration := LObject.B['dynamicRegistration'];
      Result.workspace.fileOperations.didCreate := LObject.B['didCreate'];
      Result.workspace.fileOperations.willCreate := LObject.B['willCreate'];
      Result.workspace.fileOperations.didRename := LObject.B['didRename'];
      Result.workspace.fileOperations.willRename := LObject.B['willRename'];
      Result.workspace.fileOperations.didDelete := LObject.B['didDelete'];
      Result.workspace.fileOperations.willDelete := LObject.B['willDelete'];
    end;
  end;

  // window
  //
  // Window specific client capabilities.
  s := 'params.capabilities.window';
  LObject := LJson[s].AsObject;
  if Assigned(LObject) then
  begin
    Result.window := TLSPWindow.Create;

    // window/workDoneProgress
    Result.window.workDoneProgress := LObject.B['workDoneProgress'];

    // window/showMessage
    if LObject.Expression['showMessage'].DataType = dtObject then
    begin
      Result.window.showMessage := TLSPShowMessageRequestClientCapabilities.Create;

      if LObject.O['showMessage'].Expression['messageActionItem'].DataType = dtObject then
      begin
        Result.window.showMessage.messageActionItem := TLSPMessageActionItem.Create;
        Result.window.showMessage.messageActionItem.additionalPropertiesSupport := LObject.O['showMessage'].O['messageActionItem'].B['additionalPropertiesSupport'];
      end;
    end;

    // window/showDocument
    if LObject.Expression['showDocument'].DataType = dtObject then
    begin
      Result.window.showDocument := TLSPShowDocumentClientCapabilities.Create;
      Result.window.showDocument.support := LObject.O['showDocument'].B['support'];
    end;
  end;

  // general
  //
  // General client capabilities.
  s := 'params.capabilities.general';
  LObject := LJson[s].AsObject;
  if Assigned(LObject) then
  begin
    Result.general := TLSPGeneralClientCapabilities.Create;

    // general/regularExpressions
    if LObject.Expression['regularExpressions'].DataType = dtObject then
    begin
      Result.general.regularExpressions := TLSPRegularExpressionsClientCapabilities.Create;
      Result.general.regularExpressions.engine := LObject.O['regularExpressions'].S['engine'];
      Result.general.regularExpressions.version := LObject.O['regularExpressions'].S['version'];
    end;

    // general/markdown
    if LObject.Expression['markdown'].DataType = dtObject then
    begin
      Result.general.markdown := TLSPMarkdownClientCapabilities.Create;
      Result.general.markdown.parser := LObject.O['markdown'].S['parser'];
      Result.general.markdown.version := LObject.O['markdown'].S['version'];
    end;
  end;
end;

function JsonRegisterCapabilitiesToRegistrations(const LJson: ISuperObject): TArray<TLSPRegistration>;
var
  LArray,LArray2: ISuperArray;
  LMember,LMem: IMember;
  LObject,LArrayObj,LArrayObj2: ISuperObject;
  s: string;
  i,k: Integer;
begin
  Result := nil;
  
  s := 'params."registrations"';

  // Get registrations array
  if LJson[s].DataType <> dtArray then Exit;
  LArray := LJson[s].AsArray;

  // Process array
  SetLength(Result,LArray.Length);
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;

    Result[i].id := LArrayObj.S['id'];
    Result[i].method := LArrayObj.S['method'];

    if LArrayObj.Expression['registerOptions'].DataType = dtObject then
    begin
      LObject := LArrayObj.O['registerOptions'];
      Result[i].registerOptions := TLSPTextDocumentRegistrationOptions.Create;
      Result[i].registerOptions.includeText := LObject.B['includeText'];
      Result[i].registerOptions.syncKind := LObject.I['syncKind'];
      if LObject.Expression['documentSelector'].DataType = dtArray then
      begin
        LArray2 := LObject.A['documentSelector'];
        SetLength(Result[i].registerOptions.documentSelector ,LArray2.Length);
        k := 0;
        for LMem in LArray2 do
        begin
          if LMem.DataType <> dtObject then Continue;
          LArrayObj2 := LMem.AsObject;
          Result[i].registerOptions.documentSelector[k].language := LArrayObj2.S['language'];
          Result[i].registerOptions.documentSelector[k].scheme := LArrayObj2.S['scheme'];
          Result[i].registerOptions.documentSelector[k].pattern := LArrayObj2.S['pattern'];
          Inc(k);
        end;
        if k <> Length(Result[i].registerOptions.documentSelector) then
          SetLength(Result[i].registerOptions.documentSelector, k);
      end;
    end;
    Inc(i);
  end;
  if i <> Length(Result) then
    SetLength(Result, i);
end;

function JsonPrepareRenameResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string): TLSPPrepareRenameResponse;
var
  LRange: ISuperObject;
  s: string;
  i: Integer;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtObject then Exit;

  Result := TLSPPrepareRenameResponse.Create;

  if LJson[s].AsObject.Expression['range'].DataType = dtObject then
  begin
    // { range: TLSPRange, placeholder: string }
    LRange := LJson[s].AsObject.O['range'];
    Result.range.startPos.line := LRange.O['start'].I['line'];
    Result.range.startPos.character := LRange.O['start'].I['character'];
    Result.range.endPos.line := LRange.O['end'].I['line'];
    Result.range.endPos.character := LRange.O['end'].I['character'];
    Result.placeholder := LJson[s].AsObject.S['placeholder'];
  end
  else if LJson[s].AsObject.Expression['start'].DataType = dtObject then
  begin
    // TLSPRange
    LRange := LJson[s].AsObject;
    Result.range.startPos.line := LRange.O['start'].I['line'];
    Result.range.startPos.character := LRange.O['start'].I['character'];
    Result.range.endPos.line := LRange.O['end'].I['line'];
    Result.range.endPos.character := LRange.O['end'].I['character'];
  end
  else
  begin
    Result.defaultBehavior := LJson[s].AsObject.B['defaultBehavior'];
  end;
end;

function JsonRenameResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string):
    TLSPWorkspaceEdit;
var
  LArray,LArr: ISuperArray;
  LMember, LMem: IMember;
  LArrayObj,LArrObj: ISuperObject;
  LObject,LRange: ISuperObject;
  s,w: string;
  i,k: Integer;
  params: TLSPTextDocumentEdit;
  edit: TLSPAnnotatedTextEdit;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtObject then Exit;

  Result := TLSPWorkspaceEdit.Create;

  // Workspace edit Changes
  if LJson[s].AsObject.Expression['changes'].DataType = dtObject then
  begin
    LObject := LJson[s].AsObject.O['changes'];
    Result.changes.uri := LObject.S['uri'];
    if LObject.Expression['values'].DataType = dtArray then
    begin
      LArray := LObject.A['values'];
      SetLength(Result.changes.values, LArray.Length);
      i := 0;
      for LMember in LArray do
      begin
        if LMember.DataType <> dtObject then Continue;
        LArrayObj := LMember.AsObject;

        Result.changes.values[i].newText := LArrayObj.S['newText'];
        if LArrayObj.Expression['range'].DataType = dtObject then
        begin
          LRange := LArrayObj.O['range'];
          Result.changes.values[i].range.startPos.line := LRange.O['start'].I['line'];
          Result.changes.values[i].range.startPos.character := LRange.O['start'].I['character'];
          Result.changes.values[i].range.endPos.line := LRange.O['end'].I['line'];
          Result.changes.values[i].range.endPos.character := LRange.O['end'].I['character'];
        end;
        Inc(i);
      end;
      if i <> Length(Result.changes.values) then
        SetLength(Result.changes.values, i);
    end;
  end;

  // Workspace edit document changes
  if LJson[s].AsObject.Expression['documentChanges'].DataType = dtArray then
  begin
    LArray := LJson[s].AsObject.A['documentChanges'];
    SetLength(Result.documentChanges, LArray.Length);
    i := 0;
    for LMember in LArray do
    begin
      if LMember.DataType <> dtObject then Continue;
      LArrayObj := LMember.AsObject;

      params := TLSPTextDocumentEdit.Create;
      if LArrayObj.Expression['textDocument'].DataType = dtObject then
      begin
        LObject := LArrayObj.O['textDocument'];
        TLSPTextDocumentEdit(params).textDocument.uri := LObject.S['uri'];
        if LObject.Expression['version'].DataType = dtInteger then
          TLSPTextDocumentEdit(params).textDocument.version := LObject.I['version']
        else if LObject.Expression['version'].DataType = dtString then
          TLSPTextDocumentEdit(params).textDocument.version := StrToInt(LObject.S['version']);
      end;

      if LArrayObj.Expression['edits'].DataType = dtArray then
      begin
        LArr := LArrayObj.A['edits'];
        SetLength(TLSPTextDocumentEdit(params).edits,LArr.Length);

        // Retrieve edit's
        k := 0;
        for LMem in LArr do
        begin
          if LMem.DataType <> dtObject then Continue;
          LArrObj := LMem.AsObject;

          edit := TLSPAnnotatedTextEdit.Create;
          edit.newText := LArrObj.S['newText'];
          edit.annotationId := LArrObj.S['annotationId'];

          // range
          if LArrObj.Expression['range'].DataType = dtObject then
          begin
            LRange := LArrObj.O['range'];
            edit.range.startPos.line := LRange.O['start'].I['line'];
            edit.range.startPos.character := LRange.O['start'].I['character'];
            edit.range.endPos.line := LRange.O['end'].I['line'];
            edit.range.endPos.character := LRange.O['end'].I['character'];
          end;

          TLSPTextDocumentEdit(params).edits[k] := edit;
          Inc(k);
        end;
        if k <> Length(TLSPTextDocumentEdit(params).edits) then
          SetLength(TLSPTextDocumentEdit(params).edits, k);
      end;
      Result.documentChanges[i] := params;
      Inc(i);
    end;
    if i <> Length(Result.documentChanges) then
      SetLength(Result.documentChanges, i);
  end;

  // Change annotation
  if LJson[s].AsObject.Expression['changeAnnotations'].DataType = dtObject then
  begin
    LObject := LJson[s].AsObject.O['changeAnnotations'];
    LObject.First;
    w := LObject.CurrentKey;
    Result.changeAnnotations.id := w;

    // Get annotations
    if LObject.Expression[w].DataType = dtObject then
    begin
      Result.changeAnnotations.values.slabel := LObject.O[w].S['label'];
      Result.changeAnnotations.values.needsConfirmation := LObject.O[w].B['needsConfirmation'];
      Result.changeAnnotations.values.description := LObject.O[w].S['description'];
    end;
  end;
end;

function JsonSelectionRangeResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string; const path: string = ''): TLSPSelectionRangeResponse;
var
  LArray: ISuperArray;
  LMember: IMember;
  LArrayObj: ISuperObject;
  LRange: ISuperObject;
  s: string;
  selRange: TLSPSelectionRange;

  procedure ProcessSelectionRange(const LObject: ISuperObject; var LSelRange: TLSPSelectionRange);
  begin
    // Range
    if LObject.Expression['range'].DataType = dtObject then
    begin
      LRange := LObject.O['range'];
      LSelRange.range.startPos.line := LRange.O['start'].I['line'];
      LSelRange.range.startPos.character := LRange.O['start'].I['character'];
      LSelRange.range.endPos.line := LRange.O['end'].I['line'];
      LSelRange.range.endPos.character := LRange.O['end'].I['character'];
    end;

    // Parent
    if LObject.Expression['parent'].DataType = dtObject then
    begin
      LSelRange.parent := TLSPSelectionRange.Create;
      ProcessSelectionRange(LObject.O['parent'], LSelRange.parent);
    end;
  end;
begin
  Result := TLSPSelectionRangeResponse.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNIL then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LArrayObj := LMember.AsObject;
    selRange := TLSPSelectionRange.Create;
    ProcessSelectionRange(LArrayObj, selRange);
    Result.selRanges.Add(selRange);
  end;
end;

function JsonSemanticTokensFullToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPSemanticTokens;
var
  LArray: ISuperArray;
  LMember: IMember;
  s: string;
  i: Integer;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  Result := TLSPSemanticTokens.Create;

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtObject then Exit;

  // Optional result id
  Result.resultId := LJson[s].AsObject.S['resultId'];

  if LJson[s].AsObject.Expression['data'].DataType = dtArray then
  begin
    LArray := LJson[s].AsObject.A['data'];

    // List of tokens found in document
    SetLength(Result.data, LArray.Length);
    i := 0;
    for LMember in LArray do
    begin
      if LMember.DataType = dtInteger then
      begin
        Result.data[i] := LMember.AsInteger;
        Inc(i);
      end;
    end;
    if i <> Length(Result.data) then
      SetLength(Result.data, i);
  end;
end;

function JsonSemanticTokensFullDeltaToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string; const path: string = ''): TLSPSemanticTokensDelta;
var
  LArray,LArr: ISuperArray;
  LMember,LMem: IMember;
  s: string;
  i,j: Integer;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;

  if LJson[s].DataType <> dtObject then Exit;

  Result := TLSPSemanticTokensDelta.Create;

  // Result id
  Result.resultId := LJson[s].AsObject.S['resultId'];

  // List of edits
  if LJson[s].AsObject.Expression['edits'].DataType = dtArray then
  begin
    LArray := LJson[s].AsObject.A['edits'];
    SetLength(Result.edits, LArray.Length);
    i := 0;
    for LMember in LArray do
    begin
      if LMember.DataType <> dtObject then Continue;
      Result.edits[i].start := LMember.AsObject.I['start'];
      Result.edits[i].deleteCount := LMember.AsObject.I['deleteCount'];

      if LMember.AsObject.Expression['data'].DataType = dtArray then
      begin
        LArr := LMember.AsObject.A['data'];
        SetLength(Result.edits[i].data, LArr.Length);
        j := 0;
        for LMem in LArr do
        begin
          if LMem.DataType = dtInteger then
          begin
            Result.edits[i].data[j] := LArr.I[j];
            Inc(j);
          end;
        end;
        if j <> Length(Result.edits[i].data) then
          SetLength(Result.edits[i].data, j);
      end;
      Inc(i);
    end;
    if i <> Length(Result.edits) then
      SetLength(Result.edits, i);
  end;
end;

procedure JsonSemanticTokensRefresh(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string);
begin
  ErrorCode := 0;
  ErrorMessage := '';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
  end;
end;

function JsonShowDocumentRequestParams(const LJson: ISuperObject): TLSPShowDocumentParams;
var
  s: string;
  LObject,LSelection: ISuperObject;
begin
  Result := TLSPShowDocumentParams.Create;

  s := 'params';
  if LJson[s].DataType <> dtObject then Exit;
  LObject := LJson[s].AsObject;

  // Get uri
  Result.uri := LObject.S['uri'];

  // Show in external program?
  Result.inexternal := LObject.B['external'];

  // Should the client application take focus?
  Result.takeFocus := LObject.B['takeFocus'];

  // Optional selection range
  if LObject.Expression['selection'].DataType = dtObject then
  begin
    LSelection := LObject.O['selection'];
    if LSelection.Expression['start'].DataType = dtObject then
    begin
      Result.selection.startPos.line := LSelection.O['start'].I['line'];
      Result.selection.startPos.character := LSelection.O['start'].I['character'];
    end;
    if LSelection.Expression['end'].DataType = dtObject then
    begin
      Result.selection.endPos.line := LSelection.O['end'].I['line'];
      Result.selection.endPos.character := LSelection.O['end'].I['character'];
    end;
  end;
end;

procedure JsonShowMessageParams(const LJson: ISuperObject; var ntype: Integer; var msg: string);
begin
  // Extract message type and message string
  ntype := LJson['params."type"'].AsInteger;
  msg := LJson['params."message"'].AsString;
end;

procedure JsonShowMessageRequestParams(const LJson: ISuperObject; var ntype: Integer; var msg: string; var arr:
    TArray<string>);

  procedure ReadStringActions(const s: string; var arr: TArray<string>);
  var
    i: Integer;
    LArray: ISuperArray;
    LMember: IMember;
  begin
    if LJson[s].DataType = dtArray then
    begin
      LArray := LJson[s].AsArray;
      SetLength(arr,LArray.Length);
      i := 0;
      for LMember in LArray do
      begin
        if LMember.DataType = dtObject then
        begin
          arr[i] := LMember.AsObject.S['title'];
          Inc(i);
        end;
      end;
      if i <> Length(arr) then
        SetLength(arr, i);
    end;
  end;
begin
  // Extract message type and message string
  ntype := LJson['params."type"'].AsInteger;
  msg := LJson['params."message"'].AsString;

  // Extract message actions
  ReadStringActions('params.actions', arr);
end;

function JsonShutdownResult(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string): Boolean;
begin
  Result := False;
  ErrorCode := 0;
  ErrorMessage := '';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    // Extract error code and error message
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
    Exit;
  end;
  Result := True;
end;

function JsonSignatureHelpResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string): TLSPSignatureHelp;
var
  LArray,LArray2: ISuperArray;
  LMember,LMem: IMember;
  LObject,LDoc: ISuperObject;
  s: string;
  i,j: Integer;
begin
  Result := TLSPSignatureHelp.Create;
  ErrorCode := 0;
  ErrorMessage := '';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    // Extract error code and error message
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
    Exit;
  end;

  s := 'result';
  if LJson[s].DataType <> dtObject then Exit;
  LObject := LJson[s].AsObject;

  // Signatures
  if LObject.Expression['signatures'].DataType = dtArray then
  begin
    LArray := LObject.A['signatures'];
    SetLength(Result.signatures, LArray.Length);
    i := 0;
    for LMember in LArray do
    begin
      if LMember.DataType <> dtObject then Continue;
      LObject := LMember.AsObject;

      Result.signatures[i].slabel := LObject.S['label'];
      Result.signatures[i].activeParameter := LObject.I['activeParameter'];

      if LObject.Expression['documentation'].DataType = dtObject then
      begin
        LDoc := LObject.O['documentation'];
        Result.signatures[i].documentation.kind := LDoc.S['kind'];
        Result.signatures[i].documentation.value := LDoc.S['value'];
      end;

      if LObject.Expression['parameters'].DataType = dtArray then
      begin
        LArray2 := LObject.A['parameters'];
        SetLength(Result.signatures[i].parameters, LArray2.Length);
        j := 0;
        for LMem in LArray2 do
        begin
          if LMem.DataType <> dtObject then Continue;
          LObject := LMem.AsObject;
          Result.signatures[i].parameters[j].slabel := LObject.S['label'];
          if LObject.Expression['documentation'].DataType = dtObject then
          begin
            LDoc := LObject.O['documentation'];
            Result.signatures[i].parameters[j].documentation.kind := LDoc.S['kind'];
            Result.signatures[i].parameters[j].documentation.value := LDoc.S['value'];
          end;
          Inc(j);
        end;
        if j <> Length(Result.signatures[i].parameters) then
          SetLength(Result.signatures[i].parameters, j);
      end;
      Inc(i);
    end;
    if i <> Length(Result.signatures) then
      SetLength(Result.signatures, i);
  end;

  // Active signature
  Result.activeSignature := LJson[s].AsObject.I['activeSignature'];

  // Active parameter
  Result.activeParameter := LJson[s].AsObject.I['activeParameter'];
end;

function JsonUnregisterCapabilitiesToUnregistrations(const LJson: ISuperObject): TArray<TLSPUnregistration>;
var
  LArray: ISuperArray;
  LMember: IMember;
  s: string;
  i: Integer;
begin
  Result := nil;
  
  s := 'params."unregisterations"';

  if LJson[s].DataType <> dtArray then Exit;

  // Get unregistrations array
  LArray := LJson[s].AsArray;

  SetLength(Result,LArray.Length);

  // Process array
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType = dtObject then
    begin
      Result[i].id := LMember.AsObject.S['id'];
      Result[i].method := LMember.AsObject.S['method'];
      Inc(i);
    end;
  end;
  if i <> Length(Result) then
    SetLength(Result, i);
end;

function JsonWillSaveWaitUntilResponseToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage:
    string): TArray<TLSPTextEdit>;
var
  LArray: ISuperArray;
  LArrayObj: ISuperObject;
  LMember: IMember;
  LRange: ISuperObject;
  s: string;
  i: Integer;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  s := 'result';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    // Extract error code and error message
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
    Exit;
  end;

  if LJson[s].DataType <> dtArray then Exit;

  LArray := LJson[s].AsArray;
  SetLength(Result,LArray.Length);

  // Retrieve edit's
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType = dtObject then
    begin
      LArrayObj := LMember.AsObject;
      Result[i].newText := LArrayObj.S['newText'];

      // range
      if LArrayObj.Expression['range'].DataType = dtObject then
      begin
        LRange := LArrayObj.O['range'];
        Result[i].range.startPos.line := LRange.O['start'].I['line'];
        Result[i].range.startPos.character := LRange.O['start'].I['character'];
        Result[i].range.endPos.line := LRange.O['end'].I['line'];
        Result[i].range.endPos.character := LRange.O['end'].I['character'];
      end;
    end;
    Inc(i);
  end;
  if i <> Length(Result) then
    SetLength(Result, i);
end;

procedure JsonWorkDoneProgressRequestParams(const LJson: ISuperObject; var token: string);
begin
  // Extract the token string
  token := LJson['params."token"'].AsString;
end;

function JsonWorkspaceApplyEditParamsToObject(const LJson: ISuperObject; key: string; var ErrorCode: Integer; var
    ErrorMessage: string): TLSPApplyWorkspaceEditParams;
var
  LArray,LArrayDocument,LArrayEdits: ISuperArray;
  LArrayObj,LArrObj,LObject,LRange: ISuperObject;
  LMember,LMem: IMember;
  s,sn: string;
  i,j: Integer;
  params: TLSPBaseParams;
  edit: TLSPAnnotatedTextEdit;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  if key = '' then key := 'params';
  s := key;

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    // Extract error code and error message
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
    Exit;
  end;

  // See if we have an object
  if LJson[key].DataType <> dtObject then Exit;

  Result := TLSPApplyWorkspaceEditParams.Create;

  // Get label
  Result.slabel := LJson[s].AsObject.S['label'];

  s := key + '.edit';
  if LJson[s].DataType = dtObject then
  begin
    if LJson[s].AsObject.Expression['documentChanges'].DataType = dtArray then
      LArrayDocument := LJson[s].AsObject.A['documentChanges'];
    if Assigned(LArrayDocument) and (LArrayDocument.Length > 0) then
    begin
      // May contain `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.

      SetLength(Result.edit.documentChanges, LArrayDocument.Length);
      s := key + '.edit.documentChanges';

      // Retrieve edit's
      i := 0;
      for LMember in LArrayDocument do
      begin
        if LMember.DataType <> dtObject then Continue;
        LArrayObj := LMember.AsObject;

        params := nil;
        sn := LArrayObj.S['kind'];
        if sn <> '' then
        begin
          // File operation (create/rename/delete)
          if sn = 'create' then
          begin
            params := TLSPCreateFile.Create;
            TLSPCreateFile(params).kind := sn;
            TLSPCreateFile(params).uri := LArrayObj.S['uri'];
            TLSPCreateFile(params).annotationId := LArrayObj.S['annotationId'];

            TLSPCreateFile(params).options.overwrite := LArrayObj.O['options'].B['overwrite'];
            TLSPCreateFile(params).options.ignoreIfExists := LArrayObj.O['options'].B['ignoreIfExists'];
          end
          else if sn = 'rename' then
          begin
            params := TLSPRenameFile.Create;
            TLSPRenameFile(params).kind := sn;
            TLSPRenameFile(params).oldUri := LArrayObj.S['oldUri'];
            TLSPRenameFile(params).newUri := LArrayObj.S['newUri'];
            TLSPRenameFile(params).annotationId := LArrayObj.S['annotationId'];

            TLSPRenameFile(params).options.overwrite := LArrayObj.O['options'].B['overwrite'];
            TLSPRenameFile(params).options.ignoreIfExists := LArrayObj.O['options'].B['ignoreIfExists'];
          end
          else if sn = 'delete' then
          begin
            params := TLSPDeleteFile.Create;
            TLSPDeleteFile(params).kind := sn;
            TLSPDeleteFile(params).uri := LArrayObj.S['uri'];
            TLSPDeleteFile(params).annotationId := LArrayObj.S['annotationId'];

            TLSPDeleteFile(params).options.recursive := LArrayObj.O['options'].B['recursive'];
            TLSPDeleteFile(params).options.ignoreIfNotExists := LArrayObj.O['options'].B['ignoreIfNotExists'];
          end;
        end
        else
        begin
          // `TextDocumentEdit`
          params := TLSPTextDocumentEdit.Create;
          if LArrayObj.Expression['textDocument'].DataType = dtObject then
          begin
            LObject := LArrayObj.O['textDocument'];
            TLSPTextDocumentEdit(params).textDocument := TLSPVersionedTextDocumentIdentifier.Create;
            TLSPTextDocumentEdit(params).textDocument.uri := LObject.S['uri'];
            if LObject.Expression['version'].DataType = dtInteger then
              TLSPTextDocumentEdit(params).textDocument.version := LObject.I['version']
            else if LObject.Expression['version'].DataType = dtString then
              TLSPTextDocumentEdit(params).textDocument.version := StrToInt(LObject.S['version'])
            else if LObject.Expression['version'].DataType = dtNull then
              TLSPTextDocumentEdit(params).textDocument.version := null;
          end;

          if LArrayObj.Expression['edits'].DataType = dtArray then
          begin
            LArrayEdits := LArrayObj.A['edits'];
            SetLength(TLSPTextDocumentEdit(params).edits,LArrayEdits.Length);

            // Retrieve edit's
            j := 0;
            for LMem in LArrayEdits do
            begin
              if LMem.DataType <> dtObject then Continue;
              LArrObj := LMem.AsObject;
              edit := TLSPAnnotatedTextEdit.Create;
              edit.newText := LArrObj.S['newText'];
              edit.annotationId := LArrObj.S['annotationId'];

              if LArrObj.O['range'].DataType = dtObject then
              begin
                LRange := LArrObj.O['range'];

                // range
                edit.range.startPos.line := LRange.O['start'].I['line'];
                edit.range.startPos.character := LRange.O['start'].I['character'];
                edit.range.endPos.line := LRange.O['end'].I['line'];
                edit.range.endPos.character := LRange.O['end'].I['character'];
              end;

              TLSPTextDocumentEdit(params).edits[j] := edit;
              Inc(j);
            end;
            if j <> Length(TLSPTextDocumentEdit(params).edits) then
              SetLength(TLSPTextDocumentEdit(params).edits, j);
          end;
        end;
        Result.edit.documentChanges[i] := params;
        Inc(i);
      end;
      if i <> Length(Result.edit.documentChanges) then
        SetLength(Result.edit.documentChanges, i);
    end
    else if LJson[s].AsObject.Expression['changes'].DataType = dtObject then
    begin
      // Only plain `TextEdit`s using the `changes` property are used.
      s := key + '.edit.changes';
      LObject := LJson[s].AsObject;
      LObject.First;
      sn := LObject.CurrentKey;
      Result.edit.changes.uri := sn;

      if LObject.Expression[sn].DataType = dtArray then
      begin
        LArray := LObject.A[sn];
        SetLength(Result.edit.changes.values,LArray.Length);

        // Retrieve edit's
        i := 0;
        for LMember in LArray do
        begin
          if LMember.DataType <> dtObject then Continue;
          LArrayObj := LMember.AsObject;
          Result.edit.changes.values[i].newText := LArrayObj.S['newText'];

          // range
          if LArrayObj.Expression['range'].DataType = dtObject then
          begin
            LRange := LArrayObj.O['range'];
            Result.edit.changes.values[i].range.startPos.line := LRange.O['startPos'].I['line'];
            Result.edit.changes.values[i].range.startPos.character := LRange.O['startPos'].I['character'];
            Result.edit.changes.values[i].range.endPos.line := LRange.O['endPos'].I['line'];
            Result.edit.changes.values[i].range.endPos.character := LRange.O['endPos'].I['character'];
          end;
          Inc(i);
        end;
        if i <> Length(Result.edit.changes.values) then
          SetLength(Result.edit.changes.values, i);
      end;
    end;
  end;

  // Change annotation
  s := key + '.edit.changeAnnotations';

  LObject := LJson[s].AsObject;
  if not Assigned(LObject) then Exit;

  LObject.First;
  if LObject.Count = 0 then Exit;
  sn := LObject.CurrentKey;
  Result.edit.changeAnnotations.id := sn;

  if LObject.Expression[sn].DataType = dtObject then
  begin
    LObject := LObject.O[sn];

    // Get annotations
    Result.edit.changeAnnotations.values.slabel := LObject.S['label'];
    Result.edit.changeAnnotations.values.needsConfirmation := LObject.B['needsConfirmation'];
    Result.edit.changeAnnotations.values.description := LObject.S['description'];
  end;
end;

function JsonWorkspaceSymbolResultToObject(const LJson: ISuperObject; var ErrorCode: Integer; var ErrorMessage: string;
    const path: string = ''): TLSPSymbolInformations;
var
  i,j: Integer;
  s: string;
  LObject,LRange: ISuperObject;
  LArray,LArrayTags: ISuperArray;
  LMember: IMember;
begin
  Result := nil;
  ErrorCode := 0;
  ErrorMessage := '';

  // Check for errors
  if (LJson['error'].DataType = dtObject) and (LJson['error.message'].AsString <> '') then
  begin
    // Extract error code and error message
    ErrorCode := LJson['error."code"'].AsInteger;
    ErrorMessage := LJson['error."message"'].AsString;
    Exit;
  end;

  if path <> '' then
    s := path
  else if LJson['result'].DataType <> dtNil then
    s := 'result'
  else
    s := 'partial result';

  if LJson[s].DataType <> dtArray then Exit;

  // Get symbol array
  LArray := LJson[s].AsArray;
  SetLength(Result,LArray.Length);

  // Process array
  i := 0;
  for LMember in LArray do
  begin
    if LMember.DataType <> dtObject then Continue;
    LObject := LMember.AsObject;

    Result[i].name := LObject.S['name'];
    Result[i].kind := TLSPSymbolKind(LObject.I['kind']);
    Result[i].containerName := LObject.S['containerName'];

    if LObject.Expression['location'].DataType = dtObject then
    begin
      Result[i].location.uri := LObject.O['location'].S['uri'];
      if LObject.O['location'].Expression['range'].DataType = dtObject then
      begin
        LRange := LObject.O['location'].O['range'];
        Result[i].location.range.startPos.line := LRange.O['start'].I['line'];
        Result[i].location.range.startPos.character := LRange.O['start'].I['character'];
        Result[i].location.range.endPos.line := LRange.O['end'].I['line'];
        Result[i].location.range.endPos.character := LRange.O['end'].I['character'];
      end;
    end;

    if LObject.Expression['tags'].DataType = dtArray then
    begin
      LArrayTags := LObject.A['tags'];
      SetLength(Result[i].tags, LArrayTags.Length);
      for j := 0 to LArrayTags.Length - 1 do
        Result[i].tags[j] := LArrayTags.I[i];
    end;
    Inc(i);
  end;
  if i <> Length(Result) then
    SetLength(Result, i);
end;

function CreateJSONRequestParam(const lspKind: TLSPKind; lspMsg: TLSPMessage): string;
  function DocumentFormattingParamToJSON(const params: TLSPDocumentFormattingParams): string;
  var
    LJson: ISuperObject;
    key: string;
    value: Variant;
  begin
    Result := params.AsJson;

    if params.options.key = '' then Exit;
    LJson := TSuperObject.Create(Result);

    key := params.options.key;
    value := params.options.value;
    if LJson['options'].DataType = dtObject then
      LJson['options'].AsObject.V[key] := value;

    Result := LJson.AsJSON;
  end;

  function DocumentRangeFormattingParamToJSON(const params: TLSPDocumentRangeFormattingParams): string;
  var
    LJson: ISuperObject;
    key: string;
    value: Variant;
  begin
    Result := params.AsJson;

    if params.options.key = '' then Exit;
    LJson := TSuperObject.Create(Result);

    key := params.options.key;
    value := params.options.value;
    if LJson['options'].DataType = dtObject then
      LJson['options'].AsObject.V[key] := value;

    Result := LJson.AsJSON;
  end;

  function DocumentOnTypeFormattingParamToJSON(const params: TLSPDocumentOnTypeFormattingParams): string;
  var
    LJson: ISuperObject;
    key: string;
    value: Variant;
  begin
    Result := params.AsJson;

    if params.options.key = '' then Exit;
    LJson := TSuperObject.Create(Result);

    key := params.options.key;
    value := params.options.value;
    if LJson['options'].DataType = dtObject then
      LJson['options'].AsObject.V[key] := value;

    Result := LJson.AsJSON;
  end;
begin
  Result := '';
  if not Assigned(lspMsg.paramObj) then Exit;

  case lspKind of
    lspInitialize:                    Result := LSPInitializeParamsToJSON(TLSPInitializeParams(lspMsg.paramObj));
    lspInitialized:                   Result := TLSPInitializedParams(lspMsg.paramObj).AsJSON;
    lspCancelRequest:                 Result := TLSPCancelParams(lspMsg.paramObj).AsJSON;
    lspShowMessage:                   Result := TLSPShowMessageParams(lspMsg.paramObj).AsJSON;
    lspShowMessageRequest:            Result := TLSPShowMessageParams(lspMsg.paramObj).AsJSON;
    lspShowDocumentRequest:           Result := TLSPShowDocumentParams(lspMsg.paramObj).AsJSON;
    lspWorkDoneProgress:              Result := TLSPWorkDoneProgressParams(lspMsg.paramObj).AsJSON;
    lspDidChangeWorkspaceFolders:     Result := TLSPDidChangeWorkspaceFoldersParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceApplyEdit:            Result := TLSPApplyWorkspaceEditParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceExecuteCommand:       Result := TLSPExecuteCommandParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceWillCreateFiles:      Result := TLSPCreateFilesParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceWillDeleteFiles:      Result := TLSPDeleteFilesParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceWillRenameFiles:      Result := TLSPRenameFilesParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceDidCreateFiles:       Result := TLSPCreateFilesParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceDidDeleteFiles:       Result := TLSPDeleteFilesParams(lspMsg.paramObj).AsJSON;
    lspWorkspaceDidRenameFiles:       Result := TLSPRenameFilesParams(lspMsg.paramObj).AsJSON;
    lspDidOpenTextDocument:           Result := TLSPDidOpenTextDocumentParams(lspMsg.paramObj).AsJSON;
    lspDidChangeTextDocument:         Result := TLSPDidChangeTextDocumentParams(lspMsg.paramObj).AsJSON;
    lspWillSaveTextDocument:          Result := TLSPWillSaveTextDocumentParams(lspMsg.paramObj).AsJSON;
    lspWillSaveWaitUntilTextDocument: Result := TLSPWillSaveTextDocumentParams(lspMsg.paramObj).AsJSON;
    lspDidSaveTextDocument:           Result := TLSPDidSaveTextDocumentParams(lspMsg.paramObj).AsJSON;
    lspDidCloseTextDocument:          Result := TLSPDidCloseTextDocumentParams(lspMsg.paramObj).AsJSON;
    lspPublishDiagnostics:            Result := TLSPPublishDiagnosticsParams(lspMsg.paramObj).AsJSON;
    lspCompletion:                    Result := TLSPCompletionParams(lspMsg.paramObj).AsJSON;
    lspCompletionItemResolve:         Result := LSPCompletionResolveParamsToJSON(TLSPCompletionItem(lspMsg.paramObj));
    lspHover:                         Result := TLSPHoverParams(lspMsg.paramObj).AsJSON;
    lspSignatureHelp:                 Result := TLSPSignatureHelpParams(lspMsg.paramObj).AsJSON;
    lspGotoDeclaration:               Result := TLSPDeclarationParams(lspMsg.paramObj).AsJSON;
    lspGotoDefinition:                Result := TLSPDefinitionParams(lspMsg.paramObj).AsJSON;
    lspGotoTypeDefinition:            Result := TLSPTypeDefinitionParams(lspMsg.paramObj).AsJSON;
    lspGotoImplementation:            Result := TLSPImplmentationParams(lspMsg.paramObj).AsJSON;
    lspReferences:                    Result := TLSPReferencesParams(lspMsg.paramObj).AsJSON;
    lspDocumentHighlight:             Result := TLSPDocumentHighlightParams(lspMsg.paramObj).AsJSON;
    lspDocumentSymbol:                Result := TLSPDocumentSymbolParams(lspMsg.paramObj).AsJSON;
    lspCodeAction:                    Result := TLSPCodeActionParams(lspMsg.paramObj).AsJSON;
    lspCodeActionResolve:             Result := LSPCodeActionResolveParamsToJSON(TLSPCodeAction(lspMsg.paramObj));
    lspCodeLens:                      Result := TLSPCodeLensParams(lspMsg.paramObj).AsJSON;
    lspCodeLensResolve:               Result := LSPCodeLensResolveParamsToJSON(TLSPCodeLens(lspMsg.paramObj));
    lspDocumentLink:                  Result := TLSPDocumentLinkParams(lspMsg.paramObj).AsJSON;
    lspDocumentLinkResolve:           Result := LSPDocumentLinkResolveParamsToJSON(TLSPDocumentLink(lspMsg.paramObj));
    lspDocumentColor:                 Result := TLSPDocumentColorParams(lspMsg.paramObj).AsJSON;
    lspColorPresentation:             Result := TLSPColorPresentationParams(lspMsg.paramObj).AsJSON;
    lspDocumentFormatting:            Result := DocumentFormattingParamToJSON(TLSPDocumentFormattingParams(lspMsg.paramObj));
    lspDocumentRangeFormatting:       Result := DocumentRangeFormattingParamToJSON(TLSPDocumentRangeFormattingParams(lspMsg.paramObj));
    lspDocumentOnTypeFormatting:      Result := DocumentOnTypeFormattingParamToJSON(TLSPDocumentOnTypeFormattingParams(lspMsg.paramObj));
    lspRename:                        Result := TLSPRenameParams(lspMsg.paramObj).AsJSON;
    lspPrepareRename:                 Result := TLSPPrepareRenameParams(lspMsg.paramObj).AsJSON;
    lspFoldingRange:                  Result := TLSPFoldingRangeParams(lspMsg.paramObj).AsJSON;
    lspSelectionRange:                Result := TLSPSelectionRangeParams(lspMsg.paramObj).AsJSON;
    lspPrepareCallHierarchy:          Result := '';
    lspCallHierarchyIncommingCalls:   Result := TLSPCallHierarchyIncomingCallsParams(lspMsg.paramObj).AsJSON;
    lspCallHierarchyOutgoingCalls:    Result := TLSPCallHierarchyOutgoingCallsParams(lspMsg.paramObj).AsJSON;
    lspSemanticTokensFull:            Result := TLSPSemanticTokensParams(lspMsg.paramObj).AsJSON;
    lspSemanticTokensFullDelta:       Result := TLSPSemanticTokensDeltaParams(lspMsg.paramObj).AsJSON;
    lspSemanticTokensRange:           Result := TLSPSemanticTokensRangeParams(lspMsg.paramObj).AsJSON;
    lspSemanticTokensRefresh:         Result := '';
    lspLinkedEditingRange:            Result := TLSPLinkedEditingRangeParams(lspMsg.paramObj).AsJSON;
    lspMoniker:                       Result := TLSPMonikerParams(lspMsg.paramObj).AsJSON;
    lspProgress:                      Result := TLSPProgressParams(lspMsg.paramObj).AsJSON;
    lspLogTrace:                      Result := TLSPLogTraceParams(lspMsg.paramObj).AsJSON;
    lspSetTrace:                      Result := TLSPSetTraceParams(lspMsg.paramObj).AsJSON;
    lspError:                         Result := '';
  end;
end;

function CreateJSONRequest(const lspKind: TLSPKind; lspMsg: TLSPMessage; const method: string = ''; const paramJSON:
    string = ''): string;
var
  nId: Integer;
  s: string;
  sParams: string;
  sId: string;
  sMethod: string;
begin
  // Get id
  nId := Ord(lspMsg.id);
  sId := IntToStr(nId);

  // Set the method string
  if method <> '' then
    sMethod := method
  else
    sMethod := GetMethodFromKind(lspKind);

  // Create the params string
  if paramJSON <> '' then
    sParams := paramJSON
  else
    sParams := CreateJSONRequestParam(lspKind, lspMsg);

  if sParams = '' then sParams := 'null';

  // Create a request or notification and insert params
  if IsRequest(TLSPKind(nId)) then
  begin
    // Request
    s := '{"jsonrpc": "2.0","id": ' + sId + ',"method": ' + sMethod + ',"params": ';
  end
  else
  begin
    // Notification
    s := '{"jsonrpc": "2.0","method": ' + sMethod + ',"params": ';
  end;
  s := s + sParams;
  Result := s + '}';
end;

function CreateJSONResponse(const lspKind: TLSPKind; lspMsg: TLSPMessage; const method: string = ''; resultType:
    TLSPResultType = lsprObject; resultString: string = ''): string;
var
  nId: Integer;
  s: string;
  sResult,sError: string;
  sId: string;
  sMethod: string;
begin
  sError := '';

  // Get id
  nId := Ord(lspMsg.id);
  sId := IntToStr(nId);

  // Set the method string
  if method <> '' then
    sMethod := method
  else
    sMethod := GetMethodFromKind(lspKind);

  // Create the params string
  if resultType = lsprNull then
    sResult := 'null'
  else if resultType = lsprVoid then
    sResult := 'void'
  else if resultType = lsprEmptyArray then
    sResult := '[]'
  else if resultType = lsprString then
    sResult := resultString
  else
    sResult := CreateJSONRequestParam(lspKind, lspMsg);

  // Create the error string
  if Assigned(lspMsg.errorObj) then
    sError := TLSPResponseError(lspMsg.errorObj).AsJSON;

  // Create the request and insert params
  s := '{"jsonrpc": "2.0","id": ' + sId + ',"method": ' + sMethod + ',"result": ';
  s := s + sResult;
  if sError <> '' then
    s := s + ',"error: ' + sError;
  Result := s + '}';
end;

function GetKindFromMethod(s: string; const id: Integer = -1): Integer;
begin
  Result := IndexStr(s, LSPIdStrings);
  if Result < 0 then Result := id;
end;

function GetMethodFromKind(const lspId: TLSPKind): string;
var
  n: Integer;
begin
  n := Ord(lspId);
  Result := #34 + LSPIdStrings[n] + #34;
end;

end.
