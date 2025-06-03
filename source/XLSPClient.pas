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
 * Changes by PyScripter (https://github.com/pyscripter)
 *  - Replaced XSuperObject with System.JSON and System.JSONSerializers
 *  - Remove Indy dependency (was not working anyway). Use System.Net.Socket instead.
 *  - Fixed warnings and hints
 *  - Fixed numerous memory leaks
 *  - Improvements to XLSPExecute
 *    - Asynchronous reading
 *    - Avoid calling Synchronize and Sleep
 *  - Allow for Handling server responses with anonymous method handlers.
 *    The handler is executed in the main thread.
 *    Example:
 *      FLSPClient.SendRequest(lspCompletionItemResolve, ResolveParams,
 *      procedure(Json: TJSONObject)
 *      var
 *        Item: TLSPCompletionItem;
 *      begin
 *        if ResponseError(Json) then Exit;
 *        Item := TSerializer.Deserialize<TLSPCompletionItem>(Json.Values['result']);
 *        Memo1.Lines.Add(TSerializer.Serialize(item));
 *      end);
 *  - Added Synchronous requests (SendSyncRequest).
 *    SendSyncRequest blocks until the server responds or a timeout expires.
 *    The handler is executed in the Server thread.
 *    Example:
 *      var Item: TLSPCompletionItem;
 *      if FLSPClient.SendSyncRequest(lspCompletionItemResolve, ResolveParams,
 *      procedure(Json: TJSONObject)
 *      begin
 *        if ResponseError(Json) then Exit;
 *        Item := TSerializer.Deserialize<TLSPCompletionItem>(Json.Values['result']);
 *      end, 400) then
 *        Memo1.Lines.Add(TSerializer.Serialize(Item));
 *  - Removed unnecessary aliases in XLSPTypes
 *  - Refactored error handling in XLSP functions
 *  - Unique request id passed to the request handlers
 *  - Pass Response Id to response handlers
*)

unit XLSPClient;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.SyncObjs,
  System.Generics.Collections,
  System.JSON,
  XLSPTypes,
  XLSPExecute,
  XLSPUtils,
  Vcl.ExtCtrls;

type
  TOnCallHierarchyIncommingEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPCallHierarchyIncomingCallResult) of object;
  TOnCallHierarchyOutgoingEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPCallHierarchyOutgoingCallResult) of object;
  TOnCodeActionEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPCodeActionResult) of object;
  TOnCodeActionResolveEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPCodeActionResolveResult) of object;
  TOnCodeLensEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPCodeLensResult) of object;
  TOnCodeLensResolveEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPCodeLensResolveResult) of object;
  TOnCodeLensRefreshEvent = procedure(Sender: TObject; var errorCode: Integer; var errorMessage: string) of object;
  TOnColorPresentationEvent = procedure(Sender: TObject; const Id: Integer; const values: TLSPColorPresentationResult) of object;
  TOnCompletionEvent = procedure(Sender: TObject; const Id: Integer; const list: TLSPCompletionList) of object;
  TOnCompletionItemResolveEvent = procedure(Sender: TObject; const Id: Integer; const item: TLSPCompletionItem) of object;
  TOnConfigurationRequestEvent = procedure(Sender: TObject; const values: TLSPConfigurationParams; var AJsonResult: string; var errorCode: Integer; var errorMessage: string) of object;
  TOnDocumentColorEvent = procedure(Sender: TObject; const Id: Integer; const values: TLSPColorInformationResult) of object;
  TOnDocumentDiagnosticEvent = procedure(Sender: TObject; const Id: Integer; const kind: string; const resultId: string; const items: TArray<TLSPDiagnostic>) of object;
  TOnDocumentFormattingEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPTextEditValues) of object;
  TOnDocumentOnTypeFormattingEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPTextEditValues) of object;
  TOnDocumentRangeFormattingEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPTextEditValues) of object;
  TOnDocumentHighlightEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPDocumentHighlightResult) of object;
  TOnDocumentLinkEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPDocumentLinkResult) of object;
  TOnDocumentLinkResolveEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPDocumentLinkResolveResult) of object;
  TOnDocumentSymbolsEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPDocumentSymbolsResult) of object;
  TOnResponseError = procedure(Sender: TObject; const id, errorCode: Integer; const errorMsg: string; retriggerRequest: Boolean = False) of object;
  TOnExecuteCommandRequestEvent = procedure(Sender: TObject; const Id: Integer; const Json: string) of object;
  TOnUnknownRequestEvent = procedure(Sender: TObject; const Id: Integer; const Json: string) of object;
  TOnExitEvent = procedure(Sender: TObject; exitCode: Integer; const bRestartServer: Boolean) of object;
  TOnFindReferencesEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPFindReferencesResult) of object;
  TOnFoldingRangeEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPFoldingRangeResult) of object;
  TOnGotoDeclarationEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPGotoResult) of object;
  TOnGotoDefinitionEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPGotoResult) of object;
  TOnGotoTypeDefinitionEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPGotoResult) of object;
  TOnGotoImplementationEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPGotoResult) of object;
  TOnHoverEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPHoverResult) of object;
  TOnInitializeEvent = procedure(Sender: TObject; var value: TLSPInitializeParams) of object;
  TOnInitializedEvent = procedure(Sender: TObject; var value: TLSPInitializeResult) of object;
  TOnLinkedEditingRangeEvent = procedure(Sender: TObject; const Id: Integer; const values: TLSPLinkedEditingRangeResult) of object;
  TOnLogTraceEvent = procedure(Sender: TObject; const value: TLSPLogTraceParams) of object;
  TOnMonikerEvent = procedure(Sender: TObject; const Id: Integer; const values: TLSPMonikerResult) of object;
  TOnPrepareCallHierarchyEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPPrepareCallHierarchyResult) of object;
  TOnPrepareTypeHierarchyEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPPrepareTypeHierarchyResult) of object;
  TOnPrepareRenameEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPPrepareRenameResult) of object;
  TOnPublishDiagnostics = procedure(Sender: TObject; const uri: string; const version: Cardinal; const diagnostics: TArray<TLSPDiagnostic>) of object;
  TOnProgressEvent = procedure(Sender: TObject; const id: TLSPKind; const token: string; const value: TObject) of object;
  TOnRegisterCapabilityEvent = procedure(Sender: TObject; const values: TLSPRegistrations; var errorCode: Integer; var errorMessage: string) of object;
  TOnRenameEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPWorkspaceEdit) of object;
  TOnSelectionRangeEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPSelectionRangeResult) of object;
  TOnSemanticTokensFullEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPSemanticTokensResult) of object;
  TOnSemanticTokensFullDeltaEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPSemanticTokensDeltaResult) of object;
  TOnSemanticTokensRangeEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPSemanticTokensResult) of object;
  TOnSemanticTokensRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnSignatureHelpEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPSignatureHelpResult) of object;
  TOnShowDocumentRequestEvent = procedure(Sender: TObject; const uri: string; const bExternal: Boolean; const bTakeFocus: Boolean; const startPos, endPos: TLSPPosition; var bSuccess: Boolean) of object;
  TOnShowMessageEvent = procedure(Sender: TObject; const ntype: TLSPMessageType; const msg: string) of object;
  TOnShowMessageRequestEvent = procedure(Sender: TObject; params: TLSPShowMessageRequestParams; var sAction: string) of object;
  TOnShutdownEvent = procedure(Sender: TObject) of object;
  TOnTelemetryEvent = procedure(Sender: TObject; Json: string) of object;
  TOnInlayHintEvent = procedure(Sender: TObject; const Id: Integer; const values: TLSPInlayHintResult) of object;
  TOnInlayHintResolveEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPInlayHintResolveResult) of object;
  TOnInlayHintRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnInlineValueEvent = procedure(Sender: TObject; const Id: Integer; const values: TLSPInlineValueResult) of object;
  TOnInlineValueRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnUnegisterCapabilityEvent = procedure(Sender: TObject; const values: TLSPUnregistrations; var errorCode: Integer; var errorMessage: string) of object;
  TOnWillSaveWaitUntilTextDocumentResponse = procedure(Sender: TObject; const Id: Integer; const values: TArray<TLSPAnnotatedTextEdit>) of object;
  TOnWorkDoneProgressEvent = procedure(Sender: TObject; const token: string; var errorCode: Integer; var errorMessage: string) of object;
  TOnWorkspaceApplyEditRequestEvent = procedure(Sender: TObject; const value: TLSPApplyWorkspaceEditParams; var responseValue: TLSPApplyWorkspaceEditResponse; var errorCode: Integer; var errorMessage: string) of object;
  TOnWorkspaceDiagnosticEvent = procedure(Sender: TObject; const Id: Integer; const items: TArray<TLSPWorkspaceDocumentDiagnosticReport>) of object;
  TOnWorkspaceDiagnosticRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnWorkspaceFoldersRequestEvent = procedure(Sender: TObject; var values: TLSPWorkspaceFolders; var bSingleFileOpen: Boolean; var bNoWorkspaceFolders: Boolean; var errorCode: Integer; var errorMessage: string) of object;
  TOnWorkspaceSymbolRequestEvent = procedure(Sender: TObject; const Id: Integer; const symbols: TLSPSymbolInformations) of object;
  TOnWorkspaceWillCreateFilesResponseEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPWorkspaceEdit) of object;
  TOnWorkspaceWillDeleteFilesResponseEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPWorkspaceEdit) of object;
  TOnWorkspaceWillRenameFilesResponseEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPWorkspaceEdit) of object;

  TLSPResponseHandler = reference to procedure(AJson: TJsonObject);

  TLSPClient = class(TComponent)
  private
    FClientName: string;
    FClientVersion: string;
    FRequestCount: Integer;
    FCloseTimeout: Integer;
    FCloseTimer: TTimer;
    FDynamicCapabilities: TList<TLSPRegistration>;
    FExitTimeout: Integer;
    FExitTimer: TTimer;
    FFileLogList: TStringList;
    FFileLog: TextFile;
    FOnTypeHierarchySupertypes: TOnPrepareTypeHierarchyEvent;
    FOnWorkspaceDiagnostic: TOnWorkspaceDiagnosticEvent;
    FId: string;
    FInitialized: Boolean;
    FInitializeResultObject: TLSPInitializeResult;
    FLogCommunication: Boolean;
    FLogFileName: string;
    FLogServerMessages: Boolean;
    FLogToFile: Boolean;
    FServerThread: TLSPExecuteServerThread;
    FOnCallHierarchyIncomming: TOnCallHierarchyIncommingEvent;
    FOnCallHierarchyOutgoing: TOnCallHierarchyOutgoingEvent;
    FOnCodeAction: TOnCodeActionEvent;
    FOnCodeActionResolve: TOnCodeActionResolveEvent;
    FOnCodeLens: TOnCodeLensEvent;
    FOnCodeLensRefresh: TOnCodeLensRefreshEvent;
    FOnCodeLensResolve: TOnCodeLensResolveEvent;
    FOnColorPresentation: TOnColorPresentationEvent;
    FOnCompletion: TOnCompletionEvent;
    FOnCompletionItemResolve: TOnCompletionItemResolveEvent;
    FOnConfiguration: TOnConfigurationRequestEvent;
    FOnDocumentColor: TOnDocumentColorEvent;
    FOnDocumentDiagnostic: TOnDocumentDiagnosticEvent;
    FOnDocumentFormatting: TOnDocumentFormattingEvent;
    FOnDocumentHighlight: TOnDocumentHighlightEvent;
    FOnDocumentLink: TOnDocumentLinkEvent;
    FOnDocumentLinkResolve: TOnDocumentLinkResolveEvent;
    FOnDocumentOnTypeFormatting: TOnDocumentOnTypeFormattingEvent;
    FOnDocumentRangeFormatting: TOnDocumentRangeFormattingEvent;
    FOnDocumentSymbols: TOnDocumentSymbolsEvent;
    FOnResponseError: TOnResponseError;
    FOnExit: TOnExitEvent;
    FOnInitialize: TOnInitializeEvent;
    FOnInitialized: TOnInitializedEvent;
    FOnLogMessage: TOnShowMessageEvent;
    FOnLogTrace: TOnLogTraceEvent;
    FOnProgress: TOnProgressEvent;
    FOnRegisterCapability: TOnRegisterCapabilityEvent;
    FOnShowDocument: TOnShowDocumentRequestEvent;
    FOnShowMessage: TOnShowMessageEvent;
    FOnShowMessageRequest: TOnShowMessageRequestEvent;
    FOnShutdown: TOnShutdownEvent;
    FOnTelemetryEvent: TOnTelemetryEvent;
    FOnUnregisterCapability: TOnUnegisterCapabilityEvent;
    FOnWorkDoneProgress: TOnWorkDoneProgressEvent;
    FOnWorkspaceFolders: TOnWorkspaceFoldersRequestEvent;
    FOnWorkspaceSymbol: TOnWorkspaceSymbolRequestEvent;
    FPartialTokens: TStringList;
    FOnExecuteCommandRequest: TOnExecuteCommandRequestEvent;
    FOnUnknownRequest: TOnUnknownRequestEvent;
    FOnFindReferences: TOnFindReferencesEvent;
    FOnFoldingRange: TOnFoldingRangeEvent;
    FOnGotoDeclaration: TOnGotoDeclarationEvent;
    FOnGotoDefinition: TOnGotoDefinitionEvent;
    FOnGotoTypeDefinition: TOnGotoTypeDefinitionEvent;
    FOnHover: TOnHoverEvent;
    FOnGotoImplementation: TOnGotoImplementationEvent;
    FOnInlayHint: TOnInlayHintEvent;
    FOnInlayHintRefresh: TOnInlayHintRefreshEvent;
    FOnInlayHintResolve: TOnInlayHintResolveEvent;
    FOnInlineValue: TOnInlineValueEvent;
    FOnInlineValueRefresh: TOnInlineValueRefreshEvent;
    FOnLinkedEditingRange: TOnLinkedEditingRangeEvent;
    FOnMoniker: TOnMonikerEvent;
    FOnPrepareCallHierarchy: TOnPrepareCallHierarchyEvent;
    FOnPrepareRename: TOnPrepareRenameEvent;
    FOnPrepareTypeHierarchy: TOnPrepareTypeHierarchyEvent;
    FOnPublishDiagnostics: TOnPublishDiagnostics;
    FOnRename: TOnRenameEvent;
    FOnSelectionRange: TOnSelectionRangeEvent;
    FOnSemanticTokensFull: TOnSemanticTokensFullEvent;
    FOnSemanticTokensFullDelta: TOnSemanticTokensFullDeltaEvent;
    FOnSemanticTokensRange: TOnSemanticTokensRangeEvent;
    FOnSemanticTokensRefresh: TOnSemanticTokensRefreshEvent;
    FOnSignatureHelp: TOnSignatureHelpEvent;
    FOnTypeHierarchySubtypes: TOnPrepareTypeHierarchyEvent;
    FOnWillSaveWaitUntilTextDocument: TOnWillSaveWaitUntilTextDocumentResponse;
    FOnWorkspaceApplyEdit: TOnWorkspaceApplyEditRequestEvent;
    FOnWorkspaceDiagnosticRefresh: TOnWorkspaceDiagnosticRefreshEvent;
    FOnWorkspaceWillCreateFiles: TOnWorkspaceWillCreateFilesResponseEvent;
    FOnWorkspaceWillDeleteFiles: TOnWorkspaceWillDeleteFilesResponseEvent;
    FOnWorkspaceWillRenameFiles: TOnWorkspaceWillRenameFilesResponseEvent;
    FResponseTimeout: Integer;
    FResponseTimer: TTimer;
    FRestartServer: Boolean;
    FStopwatch: TStopwatch;
    FTempOutput: TBytes;
    FHandlerDict: TDictionary<Integer, TLSPResponseHandler>;
    FSyncRequestEvent: TSimpleEvent;
    FSyncRequestId: Integer;
    procedure SendToServer(const AJson: string);
    procedure AddToTempOutput(const AJson: string);
    procedure OnCloseTimer(Sender: TObject);
    procedure OnExitServer(Sender: TObject; exitcode: Integer);
    procedure OnExitTimer(Sender: TObject);
    procedure OnResponseTimer(Sender: TObject);
    procedure OnServerThreadTerminate(Sender: TObject);
    procedure RegisterCapability(const item: TLSPRegistration);
    procedure SaveToLogFile(const w: string);
    procedure SendResponse(const id: Variant; params: TLSPBaseParams = nil;
      error: TLSPResponseError = nil; resultType: TLSPResultType = lsprNull;
      const resultString: string = '');
    procedure SetDefaultOptions;
    procedure SetCloseTimeout(const Value: Integer);
    procedure SetExitTimeout(const Value: Integer);
    procedure SetResponseTimeout(const Value: Integer);
    procedure UnRegisterCapability(const method: string);
    procedure DynCapabilitiesNotify(Sender: TObject; const Item: TLSPRegistration; Action: TCollectionNotification);
    function GetServerInfo: TLSPServerInfo;
    function GetServerCapabilities: TLSPServerCapabilities;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CloseServer;
    procedure ExitServer(const bNoExitEvent: Boolean = False);
    function FindDynamicCapability(const method: string; out Registration:
        TLSPRegistration): Boolean;
    function GetErrorCodeAsString(typ: Integer): string;
    function GetKindFromPartialResultToken(const LStr: string): Integer;
    procedure OnReadFromServer(Sender: TObject; const AJson: string);
    procedure OnReadErrorFromServer(Sender: TObject; const ErrorMsg: string);
    procedure ProcessServerMessage(LJson: TJsonObject);
    procedure ProcessServerRequest(LJson: TJsonObject);
    procedure ProcessServerNotification(LJson: TJsonObject);
    procedure ProcessServerResponse(LJson: TJsonObject);
    procedure RegisterPartialResultToken(const lspKind: TLSPKind; const token: string);
    procedure RunServer(const ACommandline, ADir: String; const AEnvList: string = ''; const AHost: string = ''; const
        APort: Integer = 0; const AUseSocket: Boolean = False);
    function GetRunTimeInSeconds: Double;
    function GetSyncKind: Integer;
    function IncludeText(lspKind: TLSPKind; includeDefault: Boolean): Boolean;
    function IsRequestSupported(const lspKind: TLSPKind): Boolean;
    function LSPKindFromMethod(const s: string): TLSPKind;
    procedure SendCancelRequest(const lspKind: TLSPKind);
    procedure SendCancelWorkDoneProgress(const token: string);
    procedure SendExecuteCommandRequest(const command: string; const argumentsJSON:
        string = '');
    // Send an LSP request messase to the server
    // lspKind is one of the client requests in LSPClientRequests
    // method: Only used if lspKind = lspUnknown
    //         This allows for requests unknown to XLSPClient to be sent.
    // params: is the message params TLSPBaseParams decendent
    // paramJson: if present takes precedence over params
    function SendRequest(const lspKind: TLSPKind; const method: string = '';
        params: TLSPBaseParams = nil; const paramJSON: string = ''): Integer; overload;
    // SemdRequest overload to handle responses by a provided handler
    // which can be an anonymous method, instead of the component events
    // The handler is executed in the main thread.
    function SendRequest(const lspKind: TLSPKind;  params: TLSPBaseParams;
      Handler: TLSPResponseHandler): Integer; overload;
    // Send a synchronous request.  The method does not return unti the
    // server responds.  The handler is executed asynchrounously
    function SendSyncRequest(const lspKind: TLSPKind;  params: TLSPBaseParams;
      Handler: TLSPResponseHandler; Timeout: Integer = 400): Boolean;
    // Send an LSP notification messase to the server
    // lspKind is one of the client notifications in LSPClientNotifications
    // method: Only used if lspKind = lspUnknown
    //         This allows for notifications unknown to XLSPClient to be sent.
    // params: is the message params TLSPBaseParams decendent
    // paramJson: if present takes precedence over params
    // return value: The request **unique** id
    procedure NotifyServer(const lspKind: TLSPKind; const method: string = '';
      const params: TLSPBaseParams = nil; const paramJSON: string = '');
    procedure SendSetTraceNotification(const traceValue: string);
    procedure UnRegisterPartialResultToken(const token: string);
    property Id: string read FId write FId;
    property Initialized: Boolean read FInitialized write FInitialized;
    property ServerCapabilities: TLSPServerCapabilities read GetServerCapabilities;
    property ServerInfo: TLSPServerInfo read GetServerInfo;
    property OnTypeHierarchySupertypes: TOnPrepareTypeHierarchyEvent
      read FOnTypeHierarchySupertypes write FOnTypeHierarchySupertypes;
    property OnWorkspaceDiagnostic: TOnWorkspaceDiagnosticEvent
      read FOnWorkspaceDiagnostic write FOnWorkspaceDiagnostic;
    property OnDocumentDiagnostic: TOnDocumentDiagnosticEvent
      read FOnDocumentDiagnostic write FOnDocumentDiagnostic;
    property OnInlayHint: TOnInlayHintEvent read FOnInlayHint write FOnInlayHint;
    property OnInlayHintRefresh: TOnInlayHintRefreshEvent
      read FOnInlayHintRefresh write FOnInlayHintRefresh;
    property OnInlayHintResolve: TOnInlayHintResolveEvent
      read FOnInlayHintResolve write FOnInlayHintResolve;
    property OnInlineValue: TOnInlineValueEvent read FOnInlineValue write FOnInlineValue;
    property OnInlineValueRefresh: TOnInlineValueRefreshEvent
      read FOnInlineValueRefresh write FOnInlineValueRefresh;
    property OnPrepareTypeHierarchy: TOnPrepareTypeHierarchyEvent
      read FOnPrepareTypeHierarchy write FOnPrepareTypeHierarchy;
    property OnRegisterCapability: TOnRegisterCapabilityEvent
      read FOnRegisterCapability write FOnRegisterCapability;
    property OnTypeHierarchySubtypes: TOnPrepareTypeHierarchyEvent
      read FOnTypeHierarchySubtypes write FOnTypeHierarchySubtypes;
    property OnWorkspaceDiagnosticRefresh: TOnWorkspaceDiagnosticRefreshEvent
      read FOnWorkspaceDiagnosticRefresh write FOnWorkspaceDiagnosticRefresh;
  published
    property ClientName: string read FClientName write FClientName;
    property ClientVersion: string read FClientVersion write FClientVersion;
    property CloseTimeout: Integer read FCloseTimeout write SetCloseTimeout default 3000;
    property ExitTimeout: Integer read FExitTimeout write SetExitTimeout default 1000;
    property LogCommunication: Boolean read FLogCommunication write FLogCommunication default False;
    property LogFileName: string read FLogFileName write FLogFileName;
    property LogServerMessages: Boolean read FLogServerMessages write FLogServerMessages default False;
    property LogToFile: Boolean read FLogToFile write FLogToFile default False;
    property ResponseTimeout: Integer read FResponseTimeout write SetResponseTimeout default 15000;
    property OnCallHierarchyIncomming: TOnCallHierarchyIncommingEvent read FOnCallHierarchyIncomming write
        FOnCallHierarchyIncomming;
    property OnCallHierarchyOutgoing: TOnCallHierarchyOutgoingEvent read FOnCallHierarchyOutgoing write
        FOnCallHierarchyOutgoing;
    property OnCodeAction: TOnCodeActionEvent read FOnCodeAction write FOnCodeAction;
    property OnCodeActionResolve: TOnCodeActionResolveEvent read FOnCodeActionResolve write FOnCodeActionResolve;
    property OnCodeLens: TOnCodeLensEvent read FOnCodeLens write FOnCodeLens;
    property OnCodeLensRefresh: TOnCodeLensRefreshEvent read FOnCodeLensRefresh write FOnCodeLensRefresh;
    property OnCodeLensResolve: TOnCodeLensResolveEvent read FOnCodeLensResolve write FOnCodeLensResolve;
    property OnColorPresentation: TOnColorPresentationEvent read FOnColorPresentation write FOnColorPresentation;
    property OnCompletion: TOnCompletionEvent read FOnCompletion write
        FOnCompletion;
    property OnCompletionItemResolve: TOnCompletionItemResolveEvent read
        FOnCompletionItemResolve write FOnCompletionItemResolve;
    property OnConfiguration: TOnConfigurationRequestEvent read FOnConfiguration
        write FOnConfiguration;
    property OnDocumentColor: TOnDocumentColorEvent read FOnDocumentColor write FOnDocumentColor;
    property OnDocumentFormatting: TOnDocumentFormattingEvent read FOnDocumentFormatting write FOnDocumentFormatting;
    property OnDocumentHighlight: TOnDocumentHighlightEvent read FOnDocumentHighlight write FOnDocumentHighlight;
    property OnDocumentLink: TOnDocumentLinkEvent read FOnDocumentLink write FOnDocumentLink;
    property OnDocumentLinkResolve: TOnDocumentLinkResolveEvent read FOnDocumentLinkResolve write FOnDocumentLinkResolve;
    property OnDocumentOnTypeFormatting: TOnDocumentOnTypeFormattingEvent read FOnDocumentOnTypeFormatting write
        FOnDocumentOnTypeFormatting;
    property OnDocumentRangeFormatting: TOnDocumentRangeFormattingEvent read FOnDocumentRangeFormatting write
        FOnDocumentRangeFormatting;
    property OnDocumentSymbols: TOnDocumentSymbolsEvent read FOnDocumentSymbols write FOnDocumentSymbols;
    property OnError: TOnResponseError read FOnResponseError write FOnResponseError;
    property OnExit: TOnExitEvent read FOnExit write FOnExit;
    property OnFoldingRange: TOnFoldingRangeEvent read FOnFoldingRange write FOnFoldingRange;
    property OnGotoDeclaration: TOnGotoDeclarationEvent read FOnGotoDeclaration
        write FOnGotoDeclaration;
    property OnGotoDefinition: TOnGotoDefinitionEvent read FOnGotoDefinition write
        FOnGotoDefinition;
    property OnGotoTypeDefinition: TOnGotoTypeDefinitionEvent read
        FOnGotoTypeDefinition write FOnGotoTypeDefinition;
    property OnHover: TOnHoverEvent read FOnHover write FOnHover;
    property OnGotoImplementation: TOnGotoImplementationEvent read
        FOnGotoImplementation write FOnGotoImplementation;
    property OnInitialize: TOnInitializeEvent read FOnInitialize write FOnInitialize;
    property OnInitialized: TOnInitializedEvent read FOnInitialized write FOnInitialized;
    property OnLinkedEditingRange: TOnLinkedEditingRangeEvent read FOnLinkedEditingRange write FOnLinkedEditingRange;
    property OnLogMessage: TOnShowMessageEvent read FOnLogMessage write FOnLogMessage;
    property OnLogTrace: TOnLogTraceEvent read FOnLogTrace write FOnLogTrace;
    property OnMoniker: TOnMonikerEvent read FOnMoniker write FOnMoniker;
    property OnPrepareCallHierarchy: TOnPrepareCallHierarchyEvent read FOnPrepareCallHierarchy write
        FOnPrepareCallHierarchy;
    property OnPrepareRename: TOnPrepareRenameEvent read FOnPrepareRename write FOnPrepareRename;
    property OnProgress: TOnProgressEvent read FOnProgress write FOnProgress;
    property OnPublishDiagnostics: TOnPublishDiagnostics read FOnPublishDiagnostics
        write FOnPublishDiagnostics;
    property OnRename: TOnRenameEvent read FOnRename write FOnRename;
    property OnSelectionRange: TOnSelectionRangeEvent read FOnSelectionRange write FOnSelectionRange;
    property OnSemanticTokensFull: TOnSemanticTokensFullEvent read FOnSemanticTokensFull write FOnSemanticTokensFull;
    property OnSemanticTokensFullDelta: TOnSemanticTokensFullDeltaEvent read FOnSemanticTokensFullDelta write
        FOnSemanticTokensFullDelta;
    property OnSemanticTokensRange: TOnSemanticTokensRangeEvent read FOnSemanticTokensRange write FOnSemanticTokensRange;
    property OnSemanticTokensRefresh: TOnSemanticTokensRefreshEvent read FOnSemanticTokensRefresh write
        FOnSemanticTokensRefresh;
    property OnShowDocument: TOnShowDocumentRequestEvent read FOnShowDocument write
        FOnShowDocument;
    property OnShowMessage: TOnShowMessageEvent read FOnShowMessage write FOnShowMessage;
    property OnShowMessageRequest: TOnShowMessageRequestEvent read FOnShowMessageRequest write FOnShowMessageRequest;
    property OnShutdown: TOnShutdownEvent read FOnShutdown write FOnShutdown;
    property OnSignatureHelp: TOnSignatureHelpEvent read FOnSignatureHelp write
        FOnSignatureHelp;
    property OnTelemetryEvent: TOnTelemetryEvent read FOnTelemetryEvent write FOnTelemetryEvent;
    property OnUnregisterCapability: TOnUnegisterCapabilityEvent read
        FOnUnregisterCapability write FOnUnregisterCapability;
    property OnWillSaveWaitUntilTextDocument:
        TOnWillSaveWaitUntilTextDocumentResponse read
        FOnWillSaveWaitUntilTextDocument write FOnWillSaveWaitUntilTextDocument;
    property OnWorkDoneProgress: TOnWorkDoneProgressEvent read FOnWorkDoneProgress
        write FOnWorkDoneProgress;
    property OnWorkspaceApplyEdit: TOnWorkspaceApplyEditRequestEvent read
        FOnWorkspaceApplyEdit write FOnWorkspaceApplyEdit;
    property OnWorkspaceFolders: TOnWorkspaceFoldersRequestEvent read
        FOnWorkspaceFolders write FOnWorkspaceFolders;
    property OnWorkspaceSymbol: TOnWorkspaceSymbolRequestEvent read
        FOnWorkspaceSymbol write FOnWorkspaceSymbol;
    property OnWorkspaceWillCreateFiles: TOnWorkspaceWillCreateFilesResponseEvent
        read FOnWorkspaceWillCreateFiles write FOnWorkspaceWillCreateFiles;
    property OnWorkspaceWillDeleteFiles: TOnWorkspaceWillDeleteFilesResponseEvent
        read FOnWorkspaceWillDeleteFiles write FOnWorkspaceWillDeleteFiles;
    property OnWorkspaceWillRenameFiles: TOnWorkspaceWillRenameFilesResponseEvent
        read FOnWorkspaceWillRenameFiles write FOnWorkspaceWillRenameFiles;
    property OnExecuteCommand: TOnExecuteCommandRequestEvent read FOnExecuteCommandRequest write FOnExecuteCommandRequest;
    property OnFindReferences: TOnFindReferencesEvent read FOnFindReferences write FOnFindReferences;
  end;

  procedure Register;

implementation

uses
  System.StrUtils,
  System.TimeSpan,
  System.Rtti,
  System.JSON.Serializers,
  XLSPFunctions;

resourcestring
  rsInvalidResponseId = 'Invalid response id';
  rsInvalidMethod = 'Invalid method';
  rsUnkownMethod = 'Unknown method';
  rsInitializeFailure = 'Failed to process response to initialize request';
  rsInvalidNotification = '"%s" is not a notification';
  rsInvalidRequest = '"%s" is not a request';
  rsLspUnknowEmptyMethod = '"lspUnknown" with empty method string';

procedure Register;
begin
  RegisterComponents('LSPComponents', [TLSPClient]);
end;

constructor TLSPClient.Create(Owner: TComponent);
begin
  inherited;
  SetDefaultOptions;
  FPartialTokens := TStringlist.Create;
  FRestartServer := False;
  FLogToFile := False;
  FInitialized := False;
  FLogCommunication := True;
  FLogServerMessages := False;
  FFileLogList := TStringlist.Create;
  FDynamicCapabilities := TList<TLSPRegistration>.Create;
  FDynamicCapabilities.OnNotify := DynCapabilitiesNotify;
  FHandlerDict := TDictionary<Integer, TLSPResponseHandler>.Create;
  FSyncRequestEvent := TSimpleEvent.Create(nil, False, False, '');
  FServerThread := nil;

  // Set default timeout values
  FResponseTimeout := 15000;
  FCloseTimeout := 3000;
  FExitTimeout := 1000;

  // Used when sending requests to the server. We give the server some time to
  // respond. If the server doesn't respond within a given time, lets terminate the server thread
  // so the main program can restart the server.
  FResponseTimer := TTimer.Create(Self);
  FResponseTimer.Enabled := False;
  FResponseTimer.Interval := FResponseTimeout;
  FResponseTimer.OnTimer := OnResponseTimer;

  // Used when closing the server. We give the server some time to close.
  // If it hasn't by then, we call ExitServer().
  FCloseTimer := TTimer.Create(Self);
  FCloseTimer.Enabled := False;
  FCloseTimer.Interval := FCloseTimeout;
  FCloseTimer.OnTimer := OnCloseTimer;

  // Used when exiting the server. We give the server some time to exit.
  // If it hasn't by then, we terminate the server thread.
  FExitTimer := TTimer.Create(Self);
  FExitTimer.Enabled := False;
  FExitTimer.Interval := FExitTimeout;
  FExitTimer.OnTimer := OnExitTimer;

  FStopwatch := TStopwatch.Create;
end;

destructor TLSPClient.Destroy;
begin
  if Assigned(FServerThread) then
  begin
    FServerThread.OnTerminate := nil;
    FServerThread.OnExit := nil;
    FServerThread.Terminate;
    // A bit of time for the thread queued events to execute
    // Without it the debugger may report memory leaks
    CheckSynchronize(100);
  end;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  FStopwatch.Stop;
  FPartialTokens.Free;
  FFileLogList.Free;
  FDynamicCapabilities.Free;
  FInitializeResultObject.Free;
  FHandlerDict.Free;
  FSyncRequestEvent.Free;
  inherited;
end;

procedure TLSPClient.DynCapabilitiesNotify(Sender: TObject;
  const Item: TLSPRegistration; Action: TCollectionNotification);
// Avoid memory leaks
begin
  if Action = cnRemoved then
    Item.RegistrationOptions.Free;
end;

function TLSPClient.FindDynamicCapability(const method: string;
  out Registration: TLSPRegistration): Boolean;
var
  Item: TLSPRegistration;
begin
  Result := False;
  for Item in FDynamicCapabilities do
    if Item.method = method then
    begin
      Registration := Item;
      Exit(True);
    end;
end;

procedure TLSPClient.SendToServer(const AJson: string);
var
  Content: TBytes;
  Header: AnsiString;
begin
  Content := TEncoding.UTF8.GetBytes(AJson);
  Header := AnsiString('Content-Length: ' + IntToStr(Length(Content)) + #13#10#13#10);

  if Assigned(FServerThread) then
    FServerThread.SendToServer(BytesOf(Header) + Content);
end;

procedure TLSPClient.AddToTempOutput(const AJson: string);
var
  Header: AnsiString;
  Content: TBytes;
begin
  Content := TEncoding.UTF8.GetBytes(AJson);
  Header := AnsiString('Content-Length: ' + IntToStr(Length(Content)) + #13#10#13#10);
  FTempOutput := FTempOutput + BytesOf(Header) + Content;
end;

procedure TLSPClient.CloseServer;
begin
  // Enable close down timer and send a shutdown request to the server
  FCloseTimer.Enabled := True;
  FResponseTimer.Enabled := False;
  SendRequest(lspShutdown);
end;

procedure TLSPClient.ExitServer(const bNoExitEvent: Boolean = False);
begin
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  if Assigned(FServerThread) then
  begin
    // Send an exit notification to the server
    if bNoExitEvent then
      FServerThread.OnExit := nil;
    NotifyServer(lspExit);
  end;

  // Give the server some time to exit and let the thread terminate
  FExitTimer.Enabled := True;
end;

function TLSPClient.GetErrorCodeAsString(typ: Integer): string;
begin
  Result := '';
  case typ of
    -32700: Result := 'ParseError: (-32700)';
	  -32600: Result := 'InvalidRequest: (-32600)';
	  -32601: Result := 'MethodNotFound: (-32601)';
	  -32602: Result := 'InvalidParams: (-32602)';
	  -32603: Result := 'InternalError: (-32603)';
    -32099: Result := 'jsonrpcReservedErrorRangeStart: (-32099)';
    -32002: Result := 'ServerNotInitialized (-32002)';
    -32001: Result := 'UnknownErrorCode: (-32001)';
    -32000: Result := 'jsonrpcReservedErrorRangeEnd: (-32000)';
    -32899: Result := 'lspReservedErrorRangeStart: (-32899)';
    -32801: Result := 'ContentModified: (-32801)';
    -32800: Result := 'RequestCancelled: (-32800)';
  end;
end;

function TLSPClient.GetKindFromPartialResultToken(const LStr: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  Index := FPartialTokens.IndexOfName(LStr);
  if Index >= 0 then
    Result := StrToInt(FPartialTokens.ValueFromIndex[Index]);
end;

procedure TLSPClient.ProcessServerMessage(LJson: TJsonObject);

procedure ExecuteInMainThread(AHandler: TLSPResponseHandler);
begin
  TThread.Queue(FServerThread, procedure
    var
      SharedJsonObject: TJSONObject;
    begin
       SharedJsonObject := TSmartPtr.Make(LJson)();
       AHandler(SharedJsonObject)
    end);
end;

var
  IdJson, MethodJson: TJSONValue;
  Handler: TLSPResponseHandler;
  Id: Integer;
begin
  if not Assigned(LJson) then Exit;

  IdJson := LJson.Values['id'];
  MethodJson := LJson.Values['method'];

  if Assigned(IdJson) and Assigned(MethodJson) then
    // server request
    ExecuteInMainThread(ProcessServerRequest)
  else if Assigned(MethodJson) then
    // server notification
    ExecuteInMainThread(ProcessServerNotification)
  else if Assigned(IdJson) then
  begin
    // server response
    Id := IdJson.AsType<Integer>;
    if FHandlerDict.TryGetValue(Id, Handler) then
    begin
      FHandlerDict.Remove(Id);
      if Id = FSyncRequestId then
      begin
        // Execute the handler in the thread
        try
          Handler(LJson);
        except
          // Swallow exceptions, otherwise the
          // main thread will be blocked forever
        end;
        // Set the sync event to complete SendSyncRequest
        FSyncRequestEvent.SetEvent;
        LJson.Free;
      end
      else
        ExecuteInMainThread(Handler);
    end
    else
      ExecuteInMainThread(ProcessServerResponse);
  end
   // else invalid message
end;

procedure TLSPClient.ProcessServerNotification(LJson: TJsonObject);
var
  MethodJson: TJSONValue;
  ParamsJson: TJSONValue;
  Params: TLSPBaseParams;
  PartialResult: TLSPBaseResult;
  method: string;
  kind: Integer;
  LInt: Integer;
  LKind: integer;
  LStr: string;
  Msg: string;
begin
  MethodJson := LJson.Values['method'];
  if MethodJson is TJsonString then
  begin
    method := TJSONString(MethodJson).Value;
    kind := GetKindFromMethod(method);
    if (kind < 0) or not (TLSPKind(kind) in LSPServerNotifications) then
      Exit;
  end
  else
    Exit;

  ParamsJson := LJson.Values['params'];
  Assert(Assigned(ParamsJson), 'All notification should have params');

  case TLSPKind(kind) of
    // ShowMessage notification sent from the server
    lspShowMessage:
      if Assigned(FOnShowMessage) then
      begin
        JsonShowMessageParams(ParamsJson, LInt, LStr);
        FOnShowMessage(Self, TLSPMessageType(LInt), LStr);
      end;

    // logMessage notification sent from the server to the client to ask the client to log a particular message.
    lspLogMessage:
      begin
        JsonShowMessageParams(ParamsJson, LInt, LStr);

        if Assigned(FOnLogMessage) then
          FOnLogMessage(Self, TLSPMessageType(LInt), LStr);

        if FLogToFile and FLogServerMessages then
        begin
          case TLSPMessageType(LInt) of
            TLSPMessageType.lspMsgError: Msg := 'Error: ';
            TLSPMessageType.lspMsgWarning: Msg := 'Warning: ';
            TLSPMessageType.lspMsgInfo: Msg := 'Info: ';
            TLSPMessageType.lspMsgLog: Msg := 'Log: ';
          end;
          SaveToLogFile(Msg + LStr + #13#10);
        end;
      end;

    // LogTrace notification from server
    lspLogTrace:
      begin
        Params := TSmartPtr.Make(JsonLogTraceParamsToObject(ParamsJson))();

        if Assigned(FOnLogTrace) then
          FOnLogTrace(Self, TLSPLogTraceParams(Params));

        if FLogToFile and FLogServerMessages then
        begin
          Msg := 'Logtrace: ' + TLSPLogTraceParams(params).message + #13#10;
          SaveToLogFile(Msg);
        end;
      end;

    // Telemetry notification sent from the server to ask the client to log a telemetry event.
    lspTelemetryEvent:
      if Assigned(FOnTelemetryEvent) then
        // OnTelemetry event. Just send the Json data with no processing.
          FOnTelemetryEvent(Self, ParamsJson.AsJSON);

    // Progress messages can be either word done or partial results messages
    // we need to check for the presence of the appropriate token
    lspProgress:
      begin
        // Get token and check if we have a partial result
        LStr := ParamsJson.GetValue('token', '');
        if LStr <> '' then
        begin
          LKind := GetKindFromPartialResultToken(LStr);
          if (LKind > 0) then
          begin
            // partial result Event
            PartialResult := TSmartPtr.Make(JsonProgressValueToResult(LKind,
              ParamsJson.FindValue('value')))();

            // unregistger if result is empty
            case TLSPKind(LKind) of
              lspWorkspaceSymbol:
                if Length(TLSPWorkspaceSymbolInformationResult(PartialResult).values) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspCallHierarchyIncommingCalls:
                if Length(TLSPCallHierarchyIncomingCallResult(PartialResult).items) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspCallHierarchyOutgoingCalls:
                if Length(TLSPCallHierarchyOutgoingCallResult(PartialResult).items) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspColorPresentation:
                if Length(TLSPColorPresentationResult(PartialResult).colorPresentations) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspDocumentColor:
                if Length(TLSPColorInformationResult(PartialResult).colors) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspDocumentHighlight:
                if Length(TLSPDocumentHighlightResult(PartialResult).list) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspDocumentLink:
                if Length(TLSPDocumentLinkResult(PartialResult).documentLinks) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspDocumentSymbol:
                if (Length(TLSPDocumentSymbolsResult(PartialResult).symbols) = 0) and
                   (Length(TLSPDocumentSymbolsResult(PartialResult).symbolInformations) = 0) then
                  UnRegisterPartialResultToken(LStr);
              lspFoldingRange:
                if not Assigned(PartialResult) or (Length(TLSPFoldingRangeResult(PartialResult).foldingRanges) = 0) then
                  UnRegisterPartialResultToken(LStr);
              lspMoniker:
                if not Assigned(PartialResult) or (Length(TLSPMonikerResult(PartialResult).monikers) = 0) then
                  UnRegisterPartialResultToken(LStr);
              lspInlayHint:
                if not Assigned(PartialResult) or (Length(TLSPInlayHintResult(PartialResult).inlayHints) = 0) then
                  UnRegisterPartialResultToken(LStr);
              lspSelectionRange:
                if Length(TLSPSelectionRangeResult(PartialResult).selRanges) = 0 then
                  UnRegisterPartialResultToken(LStr);
              lspSemanticTokensFull:
                if not Assigned(PartialResult) or (Length(TLSPSemanticTokensResult(PartialResult).data) = 0) then
                  UnRegisterPartialResultToken(LStr);
              lspSemanticTokensFullDelta:
                if not Assigned(PartialResult) or (Length(TLSPSemanticTokensDeltaResult(PartialResult).edits) = 0) then
                  UnRegisterPartialResultToken(LStr);
              lspSemanticTokensRange:
                if not Assigned(PartialResult) or (Length(TLSPSemanticTokensResult(PartialResult).data) = 0) then
                  UnRegisterPartialResultToken(LStr);
              lspGotoDeclaration,
              lspGotoDefinition,
              lspGotoTypeDefinition,
              lspGotoImplementation:
                if (Length(TLSPGotoResult(PartialResult).locationLinks) = 0) and
                   (Length(TLSPGotoResult(PartialResult).locations) = 0) and
                   (TLSPGotoResult(PartialResult).location.uri = '') then
                begin
                  UnRegisterPartialResultToken(LStr);
                end;
            end;

            if Assigned(FOnProgress) then
              FOnProgress(Self, TLSPKind(LKind), LStr, PartialResult);
          end
          else if ParamsJson is TJSONObject then
          begin
            // Work progress event
            Params := TSmartPtr.Make(TLSPProgressParams.Create)();
            Params.FromJSON(TJsonObject(ParamsJson));

            if Assigned(FOnProgress) then
              FOnProgress(Self, lspProgress, LStr, Params);
          end;
        end;
      end;

    // Publish diagnostics notification sent from the server.
    lspPublishDiagnostics:
      if Assigned(FOnPublishDiagnostics) then
      begin
        Params := TSmartPtr.Make(TLSPPublishDiagnosticsParams.Create)();
        Params.FromJSON(LJson.Values['params'] as TJSONObject);

        FOnPublishDiagnostics(Self,
          TLSPPublishDiagnosticsParams(params).uri,
          TLSPPublishDiagnosticsParams(params).version,
          TLSPPublishDiagnosticsParams(params).diagnostics);
      end;
  end;
end;

procedure TLSPClient.ProcessServerRequest(LJson: TJsonObject);
// Validate and process the request
// Requests demand responses!
var
  ErrorCode: Integer;
  ErrorMsg: string;
  Id: Variant;
  method: string;

  procedure HandleError;
  var
    ResponseError:  TLSPResponseError;
  begin
    ResponseError := TSmartPtr.Make(TLSPResponseError.Create)();
    ResponseError.code := ErrorCode;
    ResponseError.message := ErrorMsg;
    SendResponse(Id, nil, ResponseError, lsprObject);
  end;

var
  IdJson,
  MethodJson,
  ParamsJson: TJSONValue;
  kind: Integer;
  LWorkspaceFolders: TLSPWorkspaceFolders;
  RequestParams: TLSPBaseParams;
  ResponseParams: TLSPBaseParams;
  bValue, bValue2: Boolean;
  resultType: TLSPResultType;
  Action: string;
  LRegistrations: TLSPRegistrations;
  LUnregistrations: TLSPUnregistrations;
  LWorkspaceCfgs: TLSPConfigurationParams;
  Index: Integer;
  LStr: string;
begin
  IdJson := LJson.Values['id'];
  MethodJson := LJson.Values['method'];
  ParamsJson := LJson.Values['params'];

  Assert(Assigned(IdJson) and Assigned(MethodJson) and Assigned(ParamsJson),
    'ProcessServerRequest');

  // Validation

  // The protocol allows null values but I am not sure what that would mean
  if IdJson is TJSONNumber then
    Id := TJSONNumber(IdJson).AsInt
  else if IdJson is TJSONString then
    Id := TJsonString(IdJson).Value
  else
    Exit;

  if not (MethodJson is TJsonString) then
  begin
    ErrorCode := TLSPErrorCodes.InvalidRequest;
    ErrorMsg := rsInvalidMethod;
    HandleError;
    Exit;
  end;

  method := TJSONString(MethodJson).Value;
  kind := GetKindFromMethod(method);

  if (kind < 0) or not (TLSPKind(kind) in LSPServerRequests) then
  begin
    ErrorCode := TLSPErrorCodes.MethodNotFound;
    ErrorMsg := rsUnkownMethod;
    HandleError;
    Exit;
  end;

  // Process the request
  ErrorCode := 0;
  ErrorMsg := '';

  case TLSPKind(kind) of

     // workspace/workspaceFolders request is sent from the server to the client to fetch the current open list
    // of workspace folders. Returns null in the response if only a single file is open in the tool.
    // Returns an empty array if a workspace is open but no folders are configured.
    lspWorkspaceFolders:
      begin
        bValue := False;
        bValue2 := False;

        // No params are sent from the server.
        if Assigned(FOnWorkspaceFolders) then
          FOnWorkspaceFolders(Self, LWorkspaceFolders, bValue, bValue2, ErrorCode, ErrorMsg);

        // bValue  = True if a single file is opened and no workspace is opened [lsprNull]
        // bValue2 = True if a workspace is defined but no folders are configured [lsprEmptyArray]
        if bValue2 then
          resultType := lsprEmptyArray
        else if bValue then
          resultType := lsprNull
        else if Length(LWorkspaceFolders) > 0 then
          resultType := lsprString
        else
          resultType := lsprNull;

        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        if resultType = lsprString then
          LStr := TSerializer.Serialize(LWorkspaceFolders);

        // An error occurred. Send the error message back to the server.
        SendResponse(Id, nil, nil, resultType, LStr);
      end;

    lspWorkspaceApplyEdit:
      begin
        RequestParams := TSmartPtr.Make(JsonWorkspaceApplyEditParamsToObject(ParamsJson))();
        ResponseParams := TSmartPtr.Make(TLSPApplyWorkspaceEditResponse.Create)();

        if Assigned(FOnWorkspaceApplyEdit) then
          FOnWorkspaceApplyEdit(Self, TLSPApplyWorkspaceEditParams(RequestParams),
            TLSPApplyWorkspaceEditResponse(ResponseParams), ErrorCode, ErrorMsg);

        // An error occurred. Send the error message back to the server.
        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;
        SendResponse(Id, ResponseParams, nil, lsprObject);
      end;

    // ShowMessage request sent from the server. Return selected action to the server.
    lspShowMessageRequest:
      begin
        RequestParams := TSmartPtr.Make(JsonShowMessageRequestParams(ParamsJson))();

        // OnShowMessageRequest event
        if Assigned(FOnShowMessageRequest) then
          FOnShowMessageRequest(Self, TLSPShowMessageRequestParams(RequestParams), Action);

        // Return selected action to the server. nil if nothing was selected by the user.
        if Action <> '' then
        begin
          ResponseParams := TSmartPtr.Make(TLSPShowMessageRequestResponse.Create)();
          TLSPShowMessageRequestResponse(ResponseParams).title := Action;
          SendResponse(Id, ResponseParams, nil, lsprObject);
        end
        else
          SendResponse(Id);
      end;

    // ShowDocument request sent from the server. Return success (boolean) to the server.
    lspShowDocumentRequest:
      begin
        bValue := False;
        RequestParams := TSmartPtr.Make(JsonShowDocumentRequestParams(ParamsJson))();

        // OnShowDocument event
        if Assigned(FOnShowDocument) then
          FOnShowDocument(Self,
            TLSPShowDocumentParams(RequestParams).uri,
            TLSPShowDocumentParams(RequestParams).external,
            TLSPShowDocumentParams(RequestParams).takeFocus,
            TLSPShowDocumentParams(RequestParams).selection.start,
            TLSPShowDocumentParams(RequestParams).selection.&end,
            bValue);

        // Return the result of the show document request to the server.
        ResponseParams := TLSPShowDocumentResult.Create;
        TLSPShowDocumentResult(ResponseParams).success := bValue;
        SendResponse(Id, ResponseParams, nil, lsprObject);
      end;

    // Client/RegisterCapability request sent from the server to register for a new capability on the client side.
    lspClientRegisterCapabilities:
      begin
        LRegistrations := JsonRegisterCapabilitiesToRegistrations(ParamsJson);

        // OnRegisterCapabilities event
        if Assigned(FOnRegisterCapability) then
          FOnRegisterCapability(Self, LRegistrations, ErrorCode, ErrorMsg);

        // An error occurred. Send the error message back to the server.
        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit
        end;

        // Register dynamic options
        for Index := 0 to Length(LRegistrations) - 1 do
          RegisterCapability(LRegistrations[Index]);

        SendResponse(Id);
      end;

    // Client/unregisterCapability request sent from the server to unregister a previously registered capability.
    lspClientUnRegisterCapabilities:
      begin
        LUnregistrations := JsonUnregisterCapabilitiesToUnregistrations(ParamsJson);

        // OnRegisterCapabilities event
        if Assigned(FOnUnregisterCapability) then
          FOnUnregisterCapability(Self, LUnregistrations, ErrorCode, ErrorMsg);

        // An error occurred. Send the error message back to the server.
        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit
        end;

        // UnRegister dynamic options
        for Index := 0 to Length(LRegistrations) - 1 do
          UnRegisterCapability(LUnRegistrations[Index].method);

        SendResponse(Id);
      end;

    // A workspace diagnostic refresh request was sent from the server.
    lspWorkspaceDiagnosticRefresh:
      begin
        // OnWorkspaceDiagnosticRefresh event
        if Assigned(FOnWorkspaceDiagnosticRefresh) then
          FOnWorkspaceDiagnosticRefresh(Self, ErrorCode, ErrorMsg);

        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        SendResponse(Id);
      end;

    // A code lens refresh request was sent from the server.
    lspCodeLensRefresh:
      begin
        // OnCodeLensRefresh
        if Assigned(FOnCodeLensRefresh) then
          FOnCodeLensRefresh(Self, ErrorCode, ErrorMsg);

        // An error occurred. Send the error message back to the server.
        if errorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        SendResponse(Id);
      end;

    // An inlayHint refresh was sent from the server.
    lspInlayHintRefresh:
      begin
        if Assigned(FOnInlayHintRefresh) then
          FOnInlayHintRefresh(Self, ErrorCode, ErrorMsg);

        // An error occurred. Send the error message back to the server.
        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        SendResponse(Id);
      end;

    // An inlineValue refresh was sent from the server.
    lspInlineValueRefresh:
      begin
        if Assigned(FOnInlineValueRefresh) then
          FOnInlineValueRefresh(Self, ErrorCode, ErrorMsg);

        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        SendResponse(Id);
      end;

    // A semantic tokens refresh was sent from the server.
    lspSemanticTokensRefresh:
      begin
        if Assigned(FOnSemanticTokensRefresh) then
          FOnSemanticTokensRefresh(Self, ErrorCode, ErrorMsg);

        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        SendResponse(Id);
      end;

    // workspace/configuration request is sent from the server to the client to fetch configuration
    // settings from the client.
    lspWorkspaceConfiguration:
      begin
        LWorkspaceCfgs := JsonConfigurationParamsToObjects(ParamsJson);

        // OnConfiguration event
        if Assigned(FOnConfiguration) then
          FOnConfiguration(Self, LWorkspaceCfgs, LStr, ErrorCode, ErrorMsg);

        // An error occurred. Send the error message back to the server.
        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        if LStr = '' then LStr := '[null]';

        // Send responce to the server
        SendResponse(Id, nil, nil, lsprString, LStr);
      end;

    // WorkDoneProgress request is sent from the server to ask the client to create a work done progress.
    lspWorkDoneProgress:
      begin
        JsonWorkDoneProgressRequestParams(ParamsJson, LStr);

        // OnWorkDoneProgress event
        if Assigned(FOnWorkDoneProgress) then
          FOnWorkDoneProgress(Self, LStr, ErrorCode, ErrorMsg);

        if ErrorCode <> 0 then
        begin
          HandleError;
          Exit;
        end;

        SendResponse(Id, nil, nil, lsprNull);
      end;
  end;
end;

procedure TLSPClient.ProcessServerResponse(LJson: TJsonObject);
var
  Id: Integer;
  Kind: Integer;
  ErrorCode: Integer;
  ErrorMessage: string;
  retriggerRequest: Boolean;
  ResultObj: TLSPBaseResult;
  LStr: string;
  LTextEditArray: TArray<TLSPAnnotatedTextEdit>;
  IdJsonValue: TJSONValue;
  ResultJson: TJSONValue;
begin
  IdJsonValue := LJson.Values['id'];

  // Validation

  // Our requests have integer ids.  We expect the same from responses
  if not (IdJsonValue is TJSONNumber) then
    raise XLSPException.CreateRes(@rsInvalidResponseId);

  Id :=  TJSONNumber(IdJsonValue).AsInt;
  Kind :=  GetKindFromId(Id);

  // Kind is not negative and part of the TLSPKind enumeration
  if (Kind < 0) or (Kind > Ord(High(TLSPKind))) then
    raise XLSPException.CreateRes(@rsInvalidResponseId);

  // Handle response error
  if ResponseError(LJson, ErrorCode, ErrorMessage) then
  begin
    if Assigned(FOnResponseError) then
    begin
      retriggerRequest := False;
      if ErrorCode = TLSPErrorCodes.ServerCancelled then
      begin
        retriggerRequest := True;
        LJson.TryGetValue('errror.data.retriggerRequest', retriggerRequest);
      end;
      FOnResponseError(Self, id, ErrorCode, ErrorMessage, retriggerRequest);
    end;

    // If we failed to initialize then terminate the server
    if (TLspKind(id) = lspInitialize) and Assigned(FServerThread) then
      FServerThread.Terminate;

    Exit;
  end;

  ResultJson := LJson.Values['result'];

  // Handle responses
  case TLSPKind(Kind) of
    // Initialize response from the server
    lspInitialize:
    begin
      // Create result object from Json string and check for errors
      FInitializeResultObject := JsonInitializeResultToObject(ResultJson);

      if not Assigned(FInitializeResultObject) then
        raise XLSPException.CreateRes(@rsInitializeFailure);

      // Send Initialized notification to the server
      NotifyServer(lspInitialized);

      // OnInitialized event
      if Assigned(FOnInitialized) then
        FOnInitialized(Self, FInitializeResultObject);

      // The server is initialized and ready for communication
      FInitialized := True;

      // See if we have anything in our temp buffer string
      if Length(FTempOutput) > 0 then
      begin
        FServerThread.SendToServer(FTempOutput);
        FTempOutput := [];
      end;
    end;

    // Progress notification sent from server
    // A call hierarchy incomming calls request was sent to the server. An event is triggered when the server responds.
    lspCallHierarchyIncommingCalls:
      if Assigned(FOnCallHierarchyIncomming) then
      begin
        ResultObj := TSmartPtr.Make(JsonCallHierarchyIncommingResultToObject(ResultJson))();
        FOnCallHierarchyIncomming(Self, Id, TLSPCallHierarchyIncomingCallResult(ResultObj));
      end;

    // A call hierarchy outgoing calls request was sent to the server. An event is triggered when the server responds.
    lspCallHierarchyOutgoingCalls:
      if Assigned(FOnCallHierarchyOutgoing) then
      begin
        ResultObj := TSmartPtr.Make(JsonCallHierarchyOutgoingResultToObject(ResultJson))();
        FOnCallHierarchyOutgoing(Self, Id, TLSPCallHierarchyOutgoingCallResult(ResultObj));
      end;

    // A code action request was sent to the server. An event is triggered when the server responds.
    lspCodeAction:
      if Assigned(FOnCodeAction) then
      begin
        ResultObj := TSmartPtr.Make(JsonCodeActionResultToObject(ResultJson))();
        FOnCodeAction(Self, Id, TLSPCodeActionResult(ResultObj));
      end;

    // A code action resolve request was sent to the server. An event is triggered when the server responds.
    lspCodeActionResolve:
      if Assigned(FOnCodeActionResolve) then
      begin
        ResultObj := TSmartPtr.Make(JsonCodeActionResolveResultToObject(ResultJson))();
        FOnCodeActionResolve(Self, Id, TLSPCodeActionResolveResult(ResultObj));
      end;

    // A code lens request was sent to the server. An event is triggered when the server responds.
    lspCodeLens:
      if Assigned(FOnCodeLens) then
      begin
        ResultObj := TSmartPtr.Make(JsonCodeLensResultToObject(ResultJson))();
        FOnCodeLens(Self, Id, TLSPCodeLensResult(ResultObj));
      end;

    // A code lens resolve request was sent to the server. An event is triggered when the server responds.
    lspCodeLensResolve:
      if Assigned(FOnCodeLensResolve) then
      begin
        ResultObj := TSmartPtr.Make(JsonCodeLensResolveResultToObject(ResultJson))();
        FOnCodeLensResolve(Self, Id, TLSPCodeLensResolveResult(ResultObj));
      end;

    // A color Presentation request was sent to the server. An event is triggered when the server responds.
    lspColorPresentation:
      if Assigned(FOnColorPresentation) then
      begin
        ResultObj := TSmartPtr.Make(JsonColorPresentationResultToObject(ResultJson))();
        FOnColorPresentation(Self, Id, TLSPColorPresentationResult(ResultObj));
      end;

    // A completion request was sent to the server. An event is triggered when the server responds.
    lspCompletion:
      if Assigned(FOnCompletion) then
      begin
        ResultObj := TSmartPtr.Make(JsonCompletionResultToObject(ResultJson))();
        FOnCompletion(Self, Id, TLSPCompletionList(ResultObj));
      end;

    // A completion item resolve request was sent to the server. An event is triggered when the server responds.
    lspCompletionItemResolve:
      if Assigned(FOnCompletionItemResolve) then
      begin
        ResultObj := TSmartPtr.Make(JsonCompletionItemResolveResultToObject(ResultJson))();
        FOnCompletionItemResolve(Self, Id, TLSPCompetionItemResolveResult(ResultObj).completionItem);
      end;

    // A document color request was sent to the server. An event is triggered when the server responds.
    lspDocumentColor:
      if Assigned(FOnDocumentColor) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentColorResultToObject(ResultJson))();
        FOnDocumentColor(Self, Id, TLSPColorInformationResult(ResultObj));
      end;

    // A document formatting request was sent to the server. An event is triggered when the server responds.
    lspDocumentFormatting:
      if Assigned(FOnDocumentFormatting) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentFormattingResultToObject(ResultJson))();
        FOnDocumentFormatting(Self, Id, TLSPTextEditValues(ResultObj));
      end;

    // A document range formatting request was sent to the server. An event is triggered when the server responds.
    lspDocumentRangeFormatting:
      if Assigned(FOnDocumentRangeFormatting) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentFormattingResultToObject(ResultJson))();
        FOnDocumentRangeFormatting(Self, Id, TLSPTextEditValues(ResultObj));
      end;

    // A document on type formatting request was sent to the server. An event is triggered when the server responds.
    lspDocumentOnTypeFormatting:
      if Assigned(FOnDocumentOnTypeFormatting) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentFormattingResultToObject(ResultJson))();
        FOnDocumentOnTypeFormatting(Self, Id, TLSPTextEditValues(ResultObj));
      end;

    // A document highlight request was sent to the server. An event is triggered when the server responds.
    lspDocumentHighlight:
      if Assigned(FOnDocumentHighlight) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentHighlightResponseToObject(ResultJson))();
        FOnDocumentHighlight(Self, Id, TLSPDocumentHighlightResult(ResultObj));
      end;

    // A document link request was sent to the server. An event is triggered when the server responds.
    lspDocumentLink:
      if Assigned(FOnDocumentLink) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentLinkResultToObject(ResultJson))();
        FOnDocumentLink(Self, Id, TLSPDocumentLinkResult(ResultObj));
      end;

    // A document link resolve request was sent to the server. An event is triggered when the server responds.
    lspDocumentLinkResolve:
      if Assigned(FOnDocumentLinkResolve) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentLinkResolveResultToObject(ResultJson))();
        FOnDocumentLinkResolve(Self, Id, TLSPDocumentLinkResolveResult(ResultObj));
      end;

    // A document symbols request was sent to the server. An event is triggered when the server responds.
    lspDocumentSymbol:
      if Assigned(FOnDocumentSymbols) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentSymbolsResultToObject(ResultJson))();
        FOnDocumentSymbols(Self, Id, TLSPDocumentSymbolsResult(ResultObj));
      end;

    // A folding range request was sent to the server. An event is triggered when the server responds.
    lspFoldingRange:
      if Assigned(FOnFoldingRange) then
      begin
        ResultObj := TSmartPtr.Make(JsonFoldingRangeResultToObject(ResultJson))();
        FOnFoldingRange(Self, Id, TLSPFoldingRangeResult(ResultObj));
      end;

    // The go to declaration request is sent from the client to the server to resolve the
    // declaration location of a symbol at a given text document position. An event is triggered when the server responds.
    lspGotoDeclaration:
      if Assigned(FOnGotoDeclaration) then
      begin
        ResultObj := TSmartPtr.Make(JsonGotoResultToObject(ResultJson))();
        FOnGotoDeclaration(Self, Id, TLSPGotoResult(ResultObj));
      end;

    // The go to definition request is sent from the client to the server to resolve the definition
    // location of a symbol at a given text document position.
    lspGotoDefinition:
      if Assigned(FOnGotoDefinition) then
      begin
        ResultObj := TSmartPtr.Make(JsonGotoResultToObject(ResultJson))();
        FOnGotoDefinition(Self, Id, TLSPGotoResult(ResultObj));
      end;

    // The go to implementation request is sent from the client to the server to resolve the implementation
    // location of a symbol at a given text document position.
    lspGotoImplementation:
      if Assigned(FOnGotoImplementation) then
      begin
        ResultObj := TSmartPtr.Make(JsonGotoResultToObject(ResultJson))();
        FOnGotoImplementation(Self, Id, TLSPGotoResult(ResultObj));
      end;

    // The go to type definition request is sent from the client to the server to resolve the type definition
    // location of a symbol at a given text document position.
    lspGotoTypeDefinition:
      if Assigned(FOnGotoTypeDefinition) then
      begin
        ResultObj := TSmartPtr.Make(JsonGotoResultToObject(ResultJson))();
        FOnGotoTypeDefinition(Self, Id, TLSPGotoResult(ResultObj));
      end;

    // A hover request was sent to the server. An event is triggered when the server responds.
    lspHover:
      if Assigned(FOnHover) then
      begin
        ResultObj := TSmartPtr.Make(JsonHoverResultToObject(ResultJson))();
        FOnHover(Self, Id, TLSPHoverResult(ResultObj));
      end;

    // A linked editing range request was sent to the server. An event is triggered when the server responds.
    lspLinkedEditingRange:
      if Assigned(FOnLinkedEditingRange) then
      begin
        ResultObj := TSmartPtr.Make(JsonLinkedEditingRangeResultToObject(ResultJson))();
        FOnLinkedEditingRange(Self, Id, TLSPLinkedEditingRangeResult(ResultObj));
      end;

    // A moniker request was sent to the server. An event is triggered when the server responds.
    lspMoniker:
      if Assigned(FOnMoniker) then
      begin
        ResultObj := TSmartPtr.Make(JsonMonikerResultToObject(ResultJson))();
        FOnMoniker(Self, Id, TLSPMonikerResult(ResultObj));
      end;

    // An inlayHint request was sent to the server. An event is triggered when the server responds.
    lspInlayHint:
      if Assigned(FOnInlayHint) then
      begin
        ResultObj := TSmartPtr.Make(JsonInlayHintResultToObject(ResultJson))();
        FOnInlayHint(Self, Id, TLSPInlayHintResult(ResultObj));
      end;

    // An inlayHint resolve request was sent to the server. An event is triggered when the server responds.
    lspInlayHintResolve:
      if Assigned(FOnInlayHintResolve) then
      begin
        ResultObj := TSmartPtr.Make(JsonInlayHintResolveResultToObject(ResultJson))();
        FOnInlayHintResolve(Self, Id, TLSPInlayHintResolveResult(ResultObj));
      end;

    // An inlineValue request was sent to the server. An event is triggered when the server responds.
    lspInlineValue:
      if Assigned(FOnInlineValue) then
      begin
        ResultObj := TSmartPtr.Make(JsonInlineValueResultToObject(ResultJson))();
        FOnInlineValue(Self, Id, TLSPInlineValueResult(ResultObj));
      end;

    // Document diagnostic request was sent to the server. An event is triggered when the server responds.
    lspDocumentDiagnostic:
      if Assigned(FOnDocumentDiagnostic) then
      begin
        ResultObj := TSmartPtr.Make(JsonDocumentDiagnosticReportToObject(ResultJson))();
        FOnDocumentDiagnostic(Self, Id,
          TLSPDocumentDiagnosticReport(ResultObj).kind,
          TLSPDocumentDiagnosticReport(ResultObj).resultId,
          TLSPDocumentDiagnosticReport(ResultObj).items);
      end;

    // Workspace diagnostic request was sent to the server. An event is triggered when the server responds.
    lspWorkspaceDiagnostic:
      if Assigned(FOnWorkspaceDiagnostic) then
      begin
        ResultObj := TSmartPtr.Make(JsonWorkspaceDiagnosticReportToObject(ResultJson))();
        FOnWorkspaceDiagnostic(Self, Id, TLSPWorkspaceDiagnosticReport(ResultObj).items);
      end;

    // A prepare call hierarchy request was sent to the server. An event is triggered when the server responds.
    lspPrepareCallHierarchy:
      if Assigned(FOnPrepareCallHierarchy) then
      begin
        ResultObj := TSmartPtr.Make(JsonPrepareCallHierarchyResultToObject(ResultJson))();
        FOnPrepareCallHierarchy(Self, Id, TLSPPrepareCallHierarchyResult(ResultObj));
      end;

    // A prepare type hierarchy request was sent to the server. An event is triggered when the server responds.
    lspPrepareTypeHierarchy:
      if Assigned(FOnPrepareTypeHierarchy) then
      begin
        ResultObj := TSmartPtr.Make(JsonPrepareTypeHierarchyResultToObject(ResultJson))();
        FOnPrepareTypeHierarchy(Self, Id, TLSPPrepareTypeHierarchyResult(ResultObj));
      end;

    // A type hierarchy super types request was sent to the server. An event is triggered when the server responds.
    lspTypeHierarchySupertypes:
      if Assigned(FOnTypeHierarchySupertypes) then
      begin
        ResultObj := TSmartPtr.Make(JsonTypeHierarchySupertypesResultToObject(ResultJson))();
        FOnTypeHierarchySupertypes(Self, Id, TLSPPrepareTypeHierarchyResult(ResultObj));
      end;

    // A call hierarchy sub types request was sent to the server. An event is triggered when the server responds.
    lspTypeHierarchySubtypes:
      if Assigned(FOnTypeHierarchySubtypes) then
      begin
        ResultObj := TSmartPtr.Make(JsonTypeHierarchySupertypesResultToObject(ResultJson))();
        FOnTypeHierarchySubtypes(Self, Id, TLSPPrepareTypeHierarchyResult(ResultObj));
      end;

    // A prepare rename request was sent to the server. An event is triggered when the server responds.
    lspPrepareRename:
      if Assigned(FOnPrepareRename) then
      begin
        ResultObj := TSmartPtr.Make(JsonPrepareRenameResultToObject(ResultJson))();
        FOnPrepareRename(Self, Id, TLSPPrepareRenameResult(ResultObj));
      end;

    // A find references request is sent from the client to the server to resolve project wide
    // references for a symbol at a given text document position.
    lspReferences:
      if Assigned(FOnFindReferences) then
      begin
        ResultObj := TSmartPtr.Make(JsonFindReferencesResultToObject(ResultJson))();
        FOnFindReferences(Self, Id, TLSPFindReferencesResult(ResultObj));
      end;

    // A rename request was sent to the server. An event is triggered when the server responds.
    lspRename:
      if Assigned(FOnRename) then
      begin
        ResultObj := TSmartPtr.Make(JsonWorkspaceEditResultToObject(ResultJson))();
        FOnRename(Self, Id, TLSPWorkspaceEdit(ResultObj));
      end;

    // A selction range request was sent to the server. An event is triggered when the server responds.
    lspSelectionRange:
      if Assigned(FOnRename) then
      begin
        ResultObj := TSmartPtr.Make(JsonSelectionRangeResultToObject(ResultJson))();
        FOnSelectionRange(Self, Id, TLSPSelectionRangeResult(ResultObj));
      end;

    // A semantic tokens full request was sent to the server. An event is triggered when the server responds.
    lspSemanticTokensFull:
      if Assigned(FOnSemanticTokensFull) then
      begin
        ResultObj := TSmartPtr.Make(JsonSemanticTokensFullResultToObject(ResultJson))();
        FOnSemanticTokensFull(Self, Id, TLSPSemanticTokensResult(ResultObj));
      end;

    // A semantic tokens full delta request was sent to the server. An event is triggered when the server responds.
    lspSemanticTokensFullDelta:
      if Assigned(FOnSemanticTokensFullDelta) then
      begin
        ResultObj := TSmartPtr.Make(JsonSemanticTokensFullDeltaResultToObject(ResultJson))();
        FOnSemanticTokensFullDelta(Self, Id, TLSPSemanticTokensDeltaResult(ResultObj));
      end;

    // A semantic tokens range request was sent to the server. An event is triggered when the server responds.
    lspSemanticTokensRange:
      if Assigned(FOnSemanticTokensRange) then
      begin
        ResultObj := TSmartPtr.Make(JsonSemanticTokensFullResultToObject(ResultJson))();
        FOnSemanticTokensRange(Self, Id, TLSPSemanticTokensResult(ResultObj));
      end;

    // Shutdown result from server
    lspShutdown:
      begin
        if FCloseTimer.Enabled then
        begin
          // CloseServer() has been called
          FCloseTimer.Enabled := False;
          ExitServer(False);
          Exit;
        end;

        if Assigned(FOnShutdown) then
          FOnShutdown(Self);
      end;

    // A signature help request was sent to the server. An event is triggered when the server responds.
    lspSignatureHelp:
      if Assigned(FOnSignatureHelp) then
      begin
        ResultObj := TSmartPtr.Make(JsonSignatureHelpResultToObject(ResultJson))();
        FOnSignatureHelp(Self, Id, TLSPSignatureHelpResult(ResultObj));
      end;

    // Resonse after the client has sent an execute command request
    lspWorkspaceExecuteCommand:
      if Assigned(FOnExecuteCommandRequest) then
      begin
        // The result from the response can be any type of object. Or it can be null.
        LStr := JsonExecuteCommandResult(ResultJson);
        FOnExecuteCommandRequest(Self, Id, LStr);
      end;

    // Workspace symbols request was sent to the server. Handle the server result and trigger an event.
    lspWorkspaceSymbol:
      if Assigned(FOnWorkspaceSymbol) then
      begin
        ResultObj := TSmartPtr.Make(JsonSignatureHelpResultToObject(ResultJson))();
        FOnWorkspaceSymbol(Self, Id, TLSPWorkspaceSymbolInformationResult(ResultObj).values);
      end;

    // willCreateFiles request was sent to the server. Handle the response and trigger an event.
    lspWorkspaceWillCreateFiles:
      if Assigned(FOnWorkspaceWillCreateFiles) then
      begin
        ResultObj := TSmartPtr.Make(JsonWorkspaceEditResultToObject(ResultJson))();
        FOnWorkspaceWillCreateFiles(Self, Id, TLSPWorkspaceEdit(ResultObj));
       end;

    // willDeleteFiles request was sent to the server. Handle the response and trigger an event.
    lspWorkspaceWillDeleteFiles:
      if Assigned(FOnWorkspaceWillDeleteFiles) then
      begin
        ResultObj := TSmartPtr.Make(JsonWorkspaceEditResultToObject(ResultJson))();
        FOnWorkspaceWillDeleteFiles(Self, Id, TLSPWorkspaceEdit(ResultObj));
      end;

    // willRenameFiles request was sent to the server. Handle the response and trigger an event.
    lspWorkspaceWillRenameFiles:
      if Assigned(FOnWorkspaceWillRenameFiles) then
      begin
        ResultObj := TSmartPtr.Make(JsonWorkspaceEditResultToObject(ResultJson))();
        FOnWorkspaceWillRenameFiles(Self, Id, TLSPWorkspaceEdit(ResultObj));
      end;

    // WillSaveWaitUntilTextDocument request was sent to the server. Handle the response and trigger an event.
    lspWillSaveWaitUntilTextDocument:
      if Assigned(FOnWillSaveWaitUntilTextDocument) then
      begin
        LTextEditArray := JsonWillSaveWaitUntilResultToObject(ResultJson);
        FOnWillSaveWaitUntilTextDocument(Self, Id, LTextEditArray);
      end;

    // lspUnknown is used to send request that are not known to XLSPClient
    lspUnknown:
      if Assigned(FOnUnknownRequest) then
        FOnUnknownRequest(Self, Id, ResultJson.ToJSON);
  end;
end;

procedure TLSPClient.OnExitServer(Sender: TObject; exitcode: Integer);
begin
  // The server has closed down and the exit code is returned
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  if Assigned(FOnExit) then
    TThread.ForceQueue(nil, procedure
    begin
      FOnExit(Self, exitcode, FRestartServer);
    end);
end;

procedure TLSPClient.OnReadErrorFromServer(Sender: TObject;
  const ErrorMsg: string);
begin
  if FLogToFile and FLogCommunication and (ErrorMsg <> '') then
    SaveToLogFile('Error read from server:' + ErrorMsg);
  TThread.Queue(FServerThread, procedure
  begin
    if Assigned(FOnLogMessage) then
      FOnLogMessage(Self,TLSPMessageType.lspMsgError, ErrorMsg);
  end);
end;

procedure TLSPClient.OnReadFromServer(Sender: TObject; const AJson: string);
// Try to do as little as possible in the main thread
var
  JsonObject: TJSONObject;
begin
  FResponseTimer.Enabled := False;

  if FLogToFile and FLogCommunication and (AJson <> '') then
    SaveToLogFile('Read from server:' + #13#10 + AJson);

  try
    JsonObject := TJSONValue.ParseJSONValue(AJson) as TJSONObject;
  except
    Exit;
  end;

  ProcessServerMessage(JsonObject);

  if Assigned(FOnLogMessage) then
    TThread.Queue(FServerThread, procedure
    begin
      FOnLogMessage(Self,TLSPMessageType.lspMsgLog, AJson);
    end);
end;

procedure TLSPClient.OnServerThreadTerminate(Sender: TObject);
begin
  FServerThread := nil;
  FRestartServer := False;
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;

  FStopwatch.Stop;

  FInitialized := False;
  Id := '';
  FreeAndNil(FInitializeResultObject);
end;

procedure TLSPClient.RegisterCapability(const item: TLSPRegistration);
var
  Reg: TLSPRegistration;
begin
  if FindDynamicCapability(item.method, Reg) then
    FDynamicCapabilities.Remove(Reg);
  FDynamicCapabilities.Add(item);
end;

procedure TLSPClient.RegisterPartialResultToken(const lspKind: TLSPKind; const token: string);
var
  Kind: Integer;
begin
  if token <> '' then
  begin
    Kind := Ord(lspKind);
    FPartialTokens.Add(token + '=' + IntToStr(Kind));
  end;
end;

procedure TLSPClient.RunServer(const ACommandline, ADir: String; const AEnvList: string = ''; const AHost: string = '';
    const APort: Integer = 0; const AUseSocket: Boolean = False);
begin
  FServerThread := TLSPExecuteServerThread.Create(ACommandline, ADir);
  FServerThread.UseSocket := AUseSocket;
  FServerThread.Host := AHost;
  FServerThread.Port := APort;
  FServerThread.OnReadFromServer := OnReadFromServer;
  FServerThread.OnReadErrorFromServer := OnReadErrorFromServer;
  FServerThread.OnExit := OnExitServer;
  FServerThread.OnTerminate := OnServerThreadTerminate;
  FServerThread.Start;

  FRequestCount := 0;
  FStopwatch.Start;
end;

function TLSPClient.GetRunTimeInSeconds: Double;
var
  elapsed: TTimeSpan;
begin
  elapsed := FStopwatch.Elapsed;
  Result := elapsed.TotalSeconds;
end;

function TLSPClient.GetServerCapabilities: TLSPServerCapabilities;
begin
  if Assigned(FInitializeResultObject) then
    Result := FInitializeResultObject.capabilities
  else
    Result := nil;
end;

function TLSPClient.GetServerInfo: TLSPServerInfo;
begin
  if Assigned(FInitializeResultObject) then
    Result := FInitializeResultObject.serverInfo
  else
    Result := default(TLSPServerInfo);
end;

function TLSPClient.GetSyncKind: Integer;
begin
  Result := TLSPTextDocumentSyncKindRec.None;
  if not Assigned(ServerCapabilities.textDocumentSync) then Exit;

  if Assigned(ServerCapabilities.textDocumentSync) then
  begin
    Result := ServerCapabilities.textDocumentSync.change;
    Exit;
  end;
end;

function TLSPClient.IncludeText(lspKind: TLSPKind; includeDefault: Boolean): Boolean;
begin
  Result := includeDefault;

  if (lspKind = lspDidSaveTextDocument) and Assigned(ServerCapabilities)
    and Assigned(ServerCapabilities.textDocumentSync)
  then
    Result := ServerCapabilities.textDocumentSync.save.includeText;
end;

function TLSPClient.IsRequestSupported(const lspKind: TLSPKind): Boolean;
var
  LNotebookSelector: TLSPNoteBookSelector;
begin
  Result := False;
  if not Assigned(ServerCapabilities) then Exit;

  case lspKind of
    // A few notifications are added here as well as it can be useful...
    lspDidOpenTextDocument:
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
                ServerCapabilities.textDocumentSync.openClose;
    lspDidCloseTextDocument:
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
                ServerCapabilities.textDocumentSync.openClose;
    lspDidChangeTextDocument:
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
                (ServerCapabilities.textDocumentSync.change > 0);
    lspDidSaveTextDocument:
      Result := Assigned(ServerCapabilities) and
                Assigned(ServerCapabilities.textDocumentSync) and
                Assigned(ServerCapabilities.textDocumentSync) and
                ServerCapabilities.textDocumentSync.save.value;
    lspDidOpenNotebookDocument,
    lspDidCloseNotebookDocument,
    lspDidChangeNotebookDocument,
    lspDidSaveNotebookDocument:
    begin
      Result := Assigned(ServerCapabilities) and
        Assigned(ServerCapabilities.notebookDocumentSync);
      if Result and Assigned(ServerCapabilities) and
        Assigned(ServerCapabilities.notebookDocumentSync) then
      begin
        if Length(ServerCapabilities.notebookDocumentSync.notebookSelector) > 0 then
        begin
          LNotebookSelector := ServerCapabilities.notebookDocumentSync.notebookSelector[0];
          Result := (LNotebookSelector.notebook <> '');
        end;
      end;
    end;
    lspWorkspaceSymbol:
      Result := Assigned(ServerCapabilities.workspaceSymbolProvider);
    lspWorkspaceExecuteCommand:
      Result := Assigned(ServerCapabilities.executeCommandProvider);
    lspWorkspaceWillCreateFiles:
      Result := Assigned(ServerCapabilities.workspace.fileOperations) and
        Assigned(ServerCapabilities.workspace.fileOperations.willCreate);
    lspWorkspaceWillRenameFiles:
      Result := Assigned(ServerCapabilities.workspace.fileOperations) and
        Assigned(ServerCapabilities.workspace.fileOperations.willRename);
    lspWorkspaceWillDeleteFiles:
      Result := Assigned(ServerCapabilities.workspace.fileOperations) and
        Assigned(ServerCapabilities.workspace.fileOperations.willDelete);
    lspWillSaveTextDocument:
      Result := Assigned(ServerCapabilities.textDocumentSync) and
        ServerCapabilities.textDocumentSync.willSave;
    lspWillSaveWaitUntilTextDocument:
      Result := Assigned(ServerCapabilities.textDocumentSync) and
        ServerCapabilities.textDocumentSync.willSaveWaitUntil;
    lspCompletion:
      Result := Assigned(ServerCapabilities.completionProvider);
    lspCompletionItemResolve:
      Result := Assigned(ServerCapabilities.completionProvider) and
        ServerCapabilities.completionProvider.resolveProvider;
    lspHover:
      Result := Assigned(ServerCapabilities.hoverProvider);
    lspSignatureHelp:
      Result := Assigned(ServerCapabilities.signatureHelpProvider);
    lspGotoDeclaration:
      Result := Assigned(ServerCapabilities.declarationProvider);
    lspGotoDefinition:
      Result := Assigned(ServerCapabilities.definitionProvider);
    lspGotoTypeDefinition:
      Result := Assigned(ServerCapabilities.typeDefinitionProvider);
    lspGotoImplementation:
      Result := Assigned(ServerCapabilities.implementationProvider);
    lspDocumentHighlight:
      Result := Assigned(ServerCapabilities.documentHighlightProvider);
    lspDocumentSymbol:
      Result := Assigned(ServerCapabilities.documentSymbolProvider);
    lspReferences:
      Result := Assigned(ServerCapabilities.referencesProvider);
    lspCodeAction:
      Result := Assigned(ServerCapabilities.codeActionProvider);
    lspCodeActionResolve:
      Result := Assigned(ServerCapabilities.codeActionProvider) and
        ServerCapabilities.codeActionProvider.resolveProvider;
    lspCodeLens:
      Result := Assigned(ServerCapabilities.codeLensProvider);
    lspCodeLensResolve:
      Result := Assigned(ServerCapabilities.codeLensProvider) and
        ServerCapabilities.codeLensProvider.resolveProvider;
    lspDocumentLink:
      Result := Assigned(ServerCapabilities.documentLinkProvider);
    lspDocumentLinkResolve:
      Result := Assigned(ServerCapabilities.documentLinkProvider) and ServerCapabilities.documentLinkProvider.resolveProvider;
    lspDocumentColor:
      Result := Assigned(ServerCapabilities.colorProvider);
    lspDocumentFormatting:
      Result := Assigned(ServerCapabilities.documentFormattingProvider);
    lspDocumentRangeFormatting:
      Result := Assigned(ServerCapabilities.documentRangeFormattingProvider);
    lspDocumentOnTypeFormatting:
      Result := Assigned(ServerCapabilities.documentOnTypeFormattingProvider);
    lspDocumentDiagnostic:
      Result := Assigned(ServerCapabilities.diagnosticProvider);
    lspWorkspaceDiagnostic:
      Result := Assigned(ServerCapabilities.diagnosticProvider) and ServerCapabilities.diagnosticProvider.workspaceDiagnostics;
    lspRename:
      Result := Assigned(ServerCapabilities.renameProvider);
    lspPrepareRename:
      Result := Assigned(ServerCapabilities.renameProvider) and ServerCapabilities.renameProvider.prepareProvider;
    lspFoldingRange:
      Result := Assigned(ServerCapabilities.foldingRangeProvider);
    lspSelectionRange:
      Result := Assigned(ServerCapabilities.selectionRangeProvider);
    lspPrepareCallHierarchy,
    lspCallHierarchyIncommingCalls,
    lspCallHierarchyOutgoingCalls:
      Result := Assigned(ServerCapabilities.callHierarchyProvider);
    lspPrepareTypeHierarchy,
    lspTypeHierarchySupertypes,
    lspTypeHierarchySubtypes:
      Result := Assigned(ServerCapabilities.typeHierarchyProvider);
    lspSemanticTokensFull:
      Result := Assigned(ServerCapabilities.semanticTokensProvider)
        and (ServerCapabilities.semanticTokensProvider.semanticTokensType in
        [semtokenFull, semtokenDelta]);
    lspSemanticTokensFullDelta:
      Result := Assigned(ServerCapabilities.semanticTokensProvider) and
        (ServerCapabilities.semanticTokensProvider.semanticTokensType = semtokenDelta);
    lspSemanticTokensRange:
      Result := Assigned(ServerCapabilities.semanticTokensProvider);
    lspSemanticTokensRefresh:
      Result := Assigned(ServerCapabilities.semanticTokensProvider);
    lspLinkedEditingRange:
      Result := Assigned(ServerCapabilities.linkedEditingRangeProvider);
    lspMoniker:
      Result := Assigned(ServerCapabilities.monikerProvider);
    lspInlayHint,
    lspInlayHintRefresh:
      Result := Assigned(ServerCapabilities.inlayHintProvider);
    lspInlayHintResolve:
      Result := Assigned(ServerCapabilities.inlayHintProvider) and ServerCapabilities.inlayHintProvider.resolveProvider;
    lspInlineValue:
      Result := Assigned(ServerCapabilities.inlineValueProvider);
  end;
end;

function TLSPClient.LSPKindFromMethod(const s: string): TLSPKind;
begin
  Result := LSPKindFromMethod(s);
end;

procedure TLSPClient.NotifyServer(const lspKind: TLSPKind; const method: string;
  const params: TLSPBaseParams; const paramJSON: string);
const
  NotifyFormat = '{"jsonrpc": "2.0","method": %s,"params": %s}';
var
  Notification: string;
  sParams: string;
  sMethod: string;
begin
  if not (lspKind in LSPClientNotifications) then
    raise XLSPException.CreateResFmt(@rsInvalidNotification,
      [TRttiEnumerationType.GetName<TLSPKind>(lspKind)]);

  // Set the method string
  if lspKind = lspUnknown then
  begin
    if method <> '' then
      sMethod := method
    else
      raise XLSPException.CreateRes(@rsLspUnknowEmptyMethod);
  end
  else
    sMethod := GetMethodFromKind(lspKind);

  // Create the params string
  if paramJSON <> '' then
    sParams := paramJSON
  else if Assigned(params) then
    sParams := params.AsJson
  else
    sParams := '{}';

  // Create notification and insert params
  Notification := Format(NotifyFormat, [sMethod, sParams]);

  if not FInitialized and (lspKind <> lspInitialized) then
    // We shouldn't send anything to the server before it has been initialized.
    // Store requests in a temp string until we are connected and the server has
    // been initialized.
    AddToTempOutput(Notification)
  else if Assigned(FServerThread) then
    // Output to server
    SendToServer(Notification);
end;

procedure TLSPClient.OnCloseTimer(Sender: TObject);
begin
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  if Assigned(FServerThread) then
  begin
    // The server doesn't seem to have responded. Shut it down forcefully.
    ExitServer(True);
  end;
end;

procedure TLSPClient.OnExitTimer(Sender: TObject);
begin
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  if Assigned(FServerThread) then
  begin
    // The server doesn't seem to have responded to the exit notification.
    // Terminate the thread.
    FServerThread.Terminate;
  end;
end;

procedure TLSPClient.OnResponseTimer(Sender: TObject);
begin
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  if Assigned(FServerThread) then
    // The server doesn't seem to have responded to requests.
    // Terminate the thread.
    FServerThread.Terminate;
end;

procedure TLSPClient.SaveToLogFile(const w: string);
var
  i: Integer;
begin
  if FLogFileName = '' then Exit;
  FFileLogList.Text := w;

  AssignFile(FFileLog, FLogFileName);
  if not FileExists(FLogFileName) then
    ReWrite(FFileLog);
  Append(FFileLog);
  try
    WriteLn(FFileLog, '');
    for i := 0 to FFileLogList.Count - 1 do
      WriteLn(FFileLog, FFileLogList[i]);
    WriteLn(FFileLog, '');
  finally
    CloseFile(FFileLog);
  end;
end;

procedure TLSPClient.SendCancelRequest(const lspKind: TLSPKind);
var
  params: TLSPCancelParams;
begin
  params := TSmartPtr.Make(TLSPCancelParams.Create)();
  params.id := Ord(lspKind);
  SendRequest(lspCancelRequest, '', params);
end;

procedure TLSPClient.SendCancelWorkDoneProgress(const token: string);
var
  params: TLSPWorkDoneProgressCancelParams;
begin
  params := TSmartPtr.Make(TLSPWorkDoneProgressCancelParams.Create)();
  params.token := token;
  NotifyServer(lspWorkDoneProgressCancel, '', params);
end;

// The workspace/executeCommand request is sent from the client to the server
// to trigger command execution on the server.
// argumentsJSON is optional and should contain an array, e.g. '[{"range": {"end": {...},"start": {...}}}]'
procedure TLSPClient.SendExecuteCommandRequest(const command: string; const
    argumentsJSON: string = '');
var
  params: TLSPExecuteCommandParams;
begin
  params := TLSPExecuteCommandParams.Create;
  params.command := command;
  params.arguments := argumentsJSON;
  SendRequest(lspWorkspaceExecuteCommand, '', nil, argumentsJSON);
end;

function TLSPClient.SendRequest(const lspKind: TLSPKind;
  const method: string = ''; params: TLSPBaseParams = nil;
  const paramJSON: string = ''): Integer;
const
  RequestFormat = '{"jsonrpc": "2.0","id": %d,"method": %s,"params": %s}';
var
  Request: string;
  RttiType: TRttiType;
  RttiField: TRttiField;
  sParams: string;
  sMethod: string;
begin
  if not (lspKind in LSPClientRequests) then
    raise XLSPException.CreateResFmt(@rsInvalidRequest,
      [TRttiEnumerationType.GetName<TLSPKind>(lspKind)]);

  // Create params object. If "params" should be set to void then set LParams to nil.
  if lspKind = lspInitialize then
  begin
    // Special case.  Params created here

    Params := TSmartPtr.Make(TLSPInitializeParams.Create)();

    // Set options to client capabilities
    TLSPInitializeParams(Params).processId := 0;
    TLSPInitializeParams(Params).clientInfo.name := ClientName;
    TLSPInitializeParams(Params).clientInfo.version := ClientVersion;
    TLSPInitializeParams(Params).capabilities := TLSPClientCapabilities.Create;

    // Call OnInitialize event
    if Assigned(FOnInitialize) then
      FOnInitialize(Self, TLSPInitializeParams(Params));
  end
  else if Assigned(Params) then
  begin
    // Register partialResultToken if it exists
    RttiType := RttiContext.GetType(Params.ClassType);
    if Assigned(RttiType) then
    begin
      RttiField := RttiType.GetField('partialResultToken');
      if Assigned(RttiField) then
        RegisterPartialResultToken(lspKind,
         RttiField.GetValue(Params).AsString);
    end;
  end;

  // Now create the request

  // The request Id (the function result)
  AtomicIncrement(FRequestCount);
  Result := (FRequestCount shl 8) or Byte(lspKind);

  // Set the method string
  if lspKind = lspUnknown then
  begin
    if method <> '' then
      sMethod := method
    else
      raise XLSPException.CreateRes(@rsLspUnknowEmptyMethod);
  end
  else
    sMethod := GetMethodFromKind(lspKind);

  // Create the params string
  if paramJSON <> '' then
    sParams := paramJSON
  else if Assigned(params) then
    sParams := CreateJSONRequestParam(lspKind, Params)
  else
    sParams := '{}';

  Request := Format(RequestFormat, [Result, sMethod, sParams]);

  if not FInitialized and (lspKind <> lspInitialize) then
    // We shouldn't send anything to the server before it has been initialized.
    // Store requests in a temp string until we are connected and the server has
    // been initialized.
    AddToTempOutput(Request)
  else if Assigned(FServerThread) then
  begin
    // Start the response timer
    FResponseTimer.Enabled := True;
    FRestartServer := lspKind <> lspInitialize;
    // Output to server
    SendToServer(Request);
  end;
end;

function TLSPClient.SendRequest(const lspKind: TLSPKind;
  params: TLSPBaseParams; Handler: TLSPResponseHandler): Integer;
begin
  Result := SendRequest(lspKind, '', params);
  FHandlerDict.AddOrSetValue(Result, Handler);
end;

procedure TLSPClient.SendResponse(const id: Variant; params:
    TLSPBaseParams = nil; error: TLSPResponseError = nil; resultType:
    TLSPResultType = lsprNull; const resultString: string = '');
var
  response: string;
begin
  response := CreateJSONResponse(id, params, error, resultType, resultString);
  SendToServer(response);
end;

procedure TLSPClient.SendSetTraceNotification(const traceValue: string);
var
  params: TLSPSetTraceParams;
begin
  // TraceValue = 'off' | 'message' | 'verbose'
  params := TSmartPtr.Make(TLSPSetTraceParams.Create)();
  params.value := traceValue;
  NotifyServer(lspSetTrace, '', params);
end;

function TLSPClient.SendSyncRequest(const lspKind: TLSPKind; params:
    TLSPBaseParams; Handler: TLSPResponseHandler; Timeout: Integer): Boolean;
var
  Id: Integer;
begin
  Id := SendRequest(lspKind, params, Handler);
  FSyncRequestId := Id;
  // Wait for the responze
  Result := FSyncRequestEvent.WaitFor(Timeout) = wrSignaled;
  FSyncRequestEvent.ResetEvent;
  FSyncRequestId := -1;
end;

procedure TLSPClient.SetCloseTimeout(const Value: Integer);
begin
  FCloseTimeout := Value;
  FCloseTimer.Interval := Value;
end;

procedure TLSPClient.SetDefaultOptions;
begin
  ClientName := 'Pascal LSP Client';
  ClientVersion := '1.0';
end;

procedure TLSPClient.SetExitTimeout(const Value: Integer);
begin
  FExitTimeout := Value;
  FExitTimer.Interval := Value;
end;

procedure TLSPClient.SetResponseTimeout(const Value: Integer);
begin
  FResponseTimeout := Value;
  FResponseTimer.Interval := Value;
end;

procedure TLSPClient.UnRegisterCapability(const method: string);
var
  Reg: TLSPRegistration;
begin
  if FindDynamicCapability(method, Reg) then
    FDynamicCapabilities.Remove(Reg);
end;

procedure TLSPClient.UnRegisterPartialResultToken(const token: string);
var
  n: Integer;
begin
  n := FPartialTokens.IndexOfName(token);
  if n >= 0 then FPartialTokens.Delete(n);
end;

end.
