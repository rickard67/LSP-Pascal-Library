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

unit XLSPClient;

interface

uses System.Classes, XLSPTypes, XLSPExecute, XSuperObject, System.Diagnostics,
  Vcl.ExtCtrls;

type
  TSymbolKind = (
    skFile=1,skModule,skNamespace,skPackage,skClass,skMethod,skProperty,skField,skConstructor,skEnum,
    skInterface,skFunction,skVariable,skConstant,skString,skNumber,skBoolean,skArray,skObject,skKey,
    skNull,skEnumMember,skStruct,skEvent,skOperator,skTypeParameter);
  TSymbolKinds = set of TSymbolKind;

  TOnCallHierarchyIncommingEvent = procedure(Sender: TObject; const value: TLSPCallHierarchyIncomingCallResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnCallHierarchyOutgoingEvent = procedure(Sender: TObject; const value: TLSPCallHierarchyOutgoingCallResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnCodeActionEvent = procedure(Sender: TObject; const value: TLSPCodeActionResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnCodeActionResolveEvent = procedure(Sender: TObject; const value: TLSPCodeAction; const errorCode: Integer; const errorMessage: string) of object;
  TOnCodeLensEvent = procedure(Sender: TObject; const value: TLSPCodeLensResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnCodeLensResolveEvent = procedure(Sender: TObject; const value: TLSPCodeLens; const errorCode: Integer; const errorMessage: string) of object;
  TOnCodeLensRefreshEvent = procedure(Sender: TObject; var errorCode: Integer; var errorMessage: string) of object;
  TOnColorPresentationEvent = procedure(Sender: TObject; const values: TLSPColorPresentationValues; const errorCode: Integer; const errorMessage: string) of object;
  TOnCompletionEvent = procedure(Sender: TObject; const list: TLSPCompletionList; const errorCode: Integer; const errorMessage: string) of object;
  TOnCompletionItemResolveEvent = procedure(Sender: TObject; const item: TLSPCompletionItem; const errorCode: Integer; const errorMessage: string) of object;
  TOnConfigurationRequestEvent = procedure(Sender: TObject; const values: TLSPConfigurationParams; var AJsonResult: string; var errorCode: Integer; var errorMessage: string) of object;
  TOnDocumentColorEvent = procedure(Sender: TObject; const values: TLSPColorInformationValues; const errorCode: Integer; const errorMessage: string) of object;
  TOnDocumentDiagnosticEvent = procedure(Sender: TObject; const kind: string; const resultId: string; const items: TArray<TLSPDiagnostic>; const errorCode: Integer; const errorMessage: string; const retriggerRequest: Boolean) of object;
  TOnDocumentFormattingEvent = procedure(Sender: TObject; const value: TLSPTextEditValues; const errorCode: Integer; const errorMessage: string) of object;
  TOnDocumentOnTypeFormattingEvent = procedure(Sender: TObject; const value: TLSPTextEditValues; const errorCode: Integer; const errorMessage: string) of object;
  TOnDocumentRangeFormattingEvent = procedure(Sender: TObject; const value: TLSPTextEditValues; const errorCode: Integer; const errorMessage: string) of object;
  TOnDocumentHighlightEvent = procedure(Sender: TObject; const value: TLSPDocumentHighlightResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnDocumentLinkEvent = procedure(Sender: TObject; const value: TLSPDocumentLinkResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnDocumentLinkResolveEvent = procedure(Sender: TObject; const value: TLSPDocumentLink; const errorCode: Integer; const errorMessage: string) of object;
  TOnDocumentSymbolsEvent = procedure(Sender: TObject; const value: TLSPDocumentSymbolsResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnErrorEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMsg: string) of object;
  TOnExecuteCommandRequestEvent = procedure(Sender: TObject; Json: string; const errorCode: Integer; const errorMsg: string) of object;
  TOnExitEvent = procedure(Sender: TObject; exitCode: Integer; const bRestartServer: Boolean) of object;
  TOnFindReferencesEvent = procedure(Sender: TObject; const value: TLSPFindReferencesResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnFoldingRangeEvent = procedure(Sender: TObject; const value: TLSPFoldingRangeResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnGotoDeclarationEvent = procedure(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnGotoDefinitionEvent = procedure(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnGotoTypeDefinitionEvent = procedure(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnGotoImplementationEvent = procedure(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnHoverEvent = procedure(Sender: TObject; const value: TLSPHover; const errorCode: Integer; const errorMessage: string) of object;
  TOnInitializeEvent = procedure(Sender: TObject; var value: TLSPInitializeParams) of object;
  TOnInitializedEvent = procedure(Sender: TObject; var value: TLSPInitializeResultParams) of object;
  TOnLinkedEditingRangeEvent = procedure(Sender: TObject; const values: TLSPLinkedEditingRanges; const errorCode: Integer; const errorMessage: string) of object;
  TOnLogTraceEvent = procedure(Sender: TObject; const value: TLSPLogTraceParams) of object;
  TOnMonikerEvent = procedure(Sender: TObject; const values: TLSPMonikerResult; const errorCode: Integer; const errorMessage: string) of object;
  TOnPrepareCallHierarchyEvent = procedure(Sender: TObject; const value: TLSPPrepareCallHierarchyResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnPrepareTypeHierarchyEvent = procedure(Sender: TObject; const value: TLSPPrepareTypeHierarchyResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnPrepareRenameEvent = procedure(Sender: TObject; const value: TLSPPrepareRenameResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnPublishDiagnostics = procedure(Sender: TObject; const uri: string; const version: Cardinal; const diagnostics: TArray<TLSPDiagnostic>) of object;
  TOnProgressEvent = procedure(Sender: TObject; const id: TLSPKind; const value: TLSPBaseParams) of object;
  TOnRegisterCapabilityEvent = procedure(Sender: TObject; const values: TLSPRegistrations; var errorCode: Integer; var errorMessage: string) of object;
  TOnRenameEvent = procedure(Sender: TObject; const value: TLSPWorkspaceEdit; const errorCode: Integer; const errorMessage: string) of object;
  TOnSelectionRangeEvent = procedure(Sender: TObject; const value: TLSPSelectionRangeResponse; const errorCode: Integer; const errorMessage: string) of object;
  TOnSemanticTokensFullEvent = procedure(Sender: TObject; const value: TLSPSemanticTokens; const errorCode: Integer; const errorMessage: string) of object;
  TOnSemanticTokensFullDeltaEvent = procedure(Sender: TObject; const value: TLSPSemanticTokensDelta; const errorCode: Integer; const errorMessage: string) of object;
  TOnSemanticTokensRangeEvent = procedure(Sender: TObject; const value: TLSPSemanticTokens; const errorCode: Integer; const errorMessage: string) of object;
  TOnSemanticTokensRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnSignatureHelpEvent = procedure(Sender: TObject; const value: TLSPSignatureHelp; const errorCode: Integer; const errorMessage: string) of object;
  TOnShowDocumentRequestEvent = procedure(Sender: TObject; const uri: string; const bExternal: Boolean; const bTakeFocus: Boolean; const startPos, endPos: TLSPPosition; var bSuccess: Boolean) of object;
  TOnShowMessageEvent = procedure(Sender: TObject; const ntype: TLSPMessageType; const msg: string) of object;
  TOnShowMessageRequestEvent = procedure(Sender: TObject; const ntype: TLSPMessageType; const msg: string; const sActionValues: TArray<string>; var sAction: string) of object;
  TOnShutdownEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnTelemetryEvent = procedure(Sender: TObject; Json: string) of object;
  TOnInlayHintEvent = procedure(Sender: TObject; const values: TLSPInlayHintResult; const errorCode: Integer; const errorMessage: string) of object;
  TOnInlayHintResolveEvent = procedure(Sender: TObject; const value: TLSPInlayHint; const errorCode: Integer; const errorMessage: string) of object;
  TOnInlayHintRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnInlineValueEvent = procedure(Sender: TObject; const values: TLSPInlineValueResult; const errorCode: Integer; const errorMessage: string) of object;
  TOnInlineValueRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnUnegisterCapabilityEvent = procedure(Sender: TObject; const values: TLSPUnregistrations; var errorCode: Integer; var errorMessage: string) of object;
  TOnWillSaveWaitUntilTextDocumentResponse = procedure(Sender: TObject; const values: TArray<TLSPTextEdit>) of object;
  TOnWorkDoneProgressEvent = procedure(Sender: TObject; const token: string; var errorCode: Integer; var errorMessage: string) of object;
  TOnWorkspaceApplyEditRequestEvent = procedure(Sender: TObject; const value: TLSPApplyWorkspaceEditParams; var responseValue: TLSPApplyWorkspaceEditResponse; var errorCode: Integer; var errorMessage: string) of object;
  TOnWorkspaceDiagnosticEvent = procedure(Sender: TObject; const items: TArray<TLSPWorkspaceDocumentDiagnosticReport>; const errorCode: Integer; const errorMessage: string; const retriggerRequest: Boolean) of object;
  TOnWorkspaceDiagnosticRefreshEvent = procedure(Sender: TObject; const errorCode: Integer; const errorMessage: string) of object;
  TOnWorkspaceFoldersRequestEvent = procedure(Sender: TObject; var values: TLSPWorkspaceFolders; var bSingleFileOpen: Boolean; var bNoWorkspaceFolders: Boolean; var errorCode: Integer; var errorMessage: string) of object;
  TOnWorkspaceSymbolRequestEvent = procedure(Sender: TObject; const symbols: TLSPSymbolInformations) of object;
  TOnWorkspaceWillCreateFilesResponseEvent = procedure(Sender: TObject; const value: TLSPWorkspaceEdit) of object;
  TOnWorkspaceWillDeleteFilesResponseEvent = procedure(Sender: TObject; const value: TLSPWorkspaceEdit) of object;
  TOnWorkspaceWillRenameFilesResponseEvent = procedure(Sender: TObject; const value: TLSPWorkspaceEdit) of object;
  
  TLSPClient = class(TComponent)
  private
    FClientName: string;
    FClientVersion: string;
    FCloseTimeout: Integer;
    FCloseTimer: TTimer;
    FDynamicCapabilities: TStringList;
    FExitTimeout: Integer;
    FExitTimer: TTimer;
    FFileLogList: TStringList;
    FFileLog: TextFile;
    FFOnTypeHierarchySupertypes: TOnPrepareTypeHierarchyEvent;
    FFOnWorkspaceDiagnostic: TOnWorkspaceDiagnosticEvent;
    FId: string;
    FInitialized: Boolean;
    FInitializeResultObject: TLSPInitializeResultParams;
    FJSONString: RawByteString;
    FLogCommunication: Boolean;
    FLogFileName: string;
    FLogServerMessages: Boolean;
    FLogToFile: Boolean;
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
    FOnError: TOnErrorEvent;
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
    FServerCapabilities: TLSPServerCapabilities;
    FServerName: string;
    FServerThread: TLSPExecuteServerThread;
    FServerVersion: string;
    FOnShutdown: TOnShutdownEvent;
    FOnTelemetryEvent: TOnTelemetryEvent;
    FOnUnregisterCapability: TOnUnegisterCapabilityEvent;
    FOnWorkDoneProgress: TOnWorkDoneProgressEvent;
    FOnWorkspaceFolders: TOnWorkspaceFoldersRequestEvent;
    FOnWorkspaceSymbol: TOnWorkspaceSymbolRequestEvent;
    FPartialTokens: TStringList;
    FOnExecuteCommandRequest: TOnExecuteCommandRequestEvent;
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
    FStartTimeout: Integer;
    FStartTimer: TTimer;
    FStopwatch: TStopwatch;
    FTempOutputString: RawByteString;
    procedure AddToOutputString(const s: string);
    procedure AddToTempOutputString(const s: string);
    procedure OnCloseTimer(Sender: TObject);
    procedure OnExitServer(Sender: TObject; exitcode: Integer);
    procedure OnExitTimer(Sender: TObject);
    procedure OnResponseTimer(Sender: TObject);
    procedure OnServerThreadTerminate(Sender: TObject);
    procedure RegisterCapability(const item: TLSPRegistration);
    procedure SaveToLogFile(const w: string);
    procedure SendResponse(const id: Integer; const method: string = ''; const params: TLSPBaseParams = nil; const errors:
        TLSPBaseParams = nil; resultType: TLSPResultType = lsprObject; resultString: string = '');
    procedure SetDefaultOptions;
    procedure OnStartTimer(Sender: TObject);
    procedure SetCloseTimeout(const Value: Integer);
    procedure SetExitTimeout(const Value: Integer);
    procedure SetResponseTimeout(const Value: Integer);
    procedure SetStartTimeout(const Value: Integer);
    procedure UnRegisterCapability(const item: TLSPUnregistration);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CloseServer;
    procedure ExitServer(const bNoExitEvent: Boolean = False);
    function FindDynamicCapability(const method: string): TLSPTextDocumentRegistrationOptions;
    function GetErrorCodeAsString(typ: Integer): string;
    function GetIdFromPartialResultToken(const LStr: string): Integer;
    procedure OnReadFromServer(Sender: TObject; const AJson: ISuperObject; const APlainText: string);
    procedure OnWriteToServer(Sender: TObject; var s: RawByteString);
    function ProcessServerMessage(const LJson: ISuperObject): Boolean;
    procedure RegisterPartialResultToken(const lspKind: TLSPKind; const token: string);
    procedure RunServer(const ACommandline, ADir: String; const AEnvList: string =
        ''; const AHost: string = ''; const APort: Integer = 0; const
        AUseSocketAsServer: Boolean = False; const AUseSocketAsClient: Boolean =
        False);
    function GetRunTimeInSeconds: Double;
    function GetSyncKind: Integer;
    function IncludeText(const lspKind: TLSPKind; const filename: string; const includeDefault: Boolean): Boolean;
    function IsRequestSupported(const lspKind: TLSPKind; const ext: string = '*'): Boolean;
    function LSPKindFromMethod(const s: string): TLSPKind;
    procedure SendCancelRequest(const lspKind: TLSPKind);
    procedure SendCancelWorkDoneProgress(const token: string);
    procedure SendExecuteCommandRequest(const command: string; const argumentsJSON:
        string = '');
    procedure SendProgressNotification(token: string; const value: TLSPWorkDoneProgressValue);
    procedure SendRequest(const lspKind: TLSPKind; const method: string = ''; const params: TLSPBaseParams = nil; const
        paramJSON: string = '');
    procedure SendSetTraceNotification(const traceValue: string);
    procedure UnRegisterPartialResultToken(const token: string);
    property Id: string read FId write FId;
    property Initialized: Boolean read FInitialized write FInitialized;
    property JSONString: RawByteString read FJSONString write FJSONString;
    property ServerCapabilities: TLSPServerCapabilities read FServerCapabilities write FServerCapabilities;
    property ServerName: string read FServerName write FServerName;
    property ServerVersion: string read FServerVersion write FServerVersion;
    property FOnTypeHierarchySupertypes: TOnPrepareTypeHierarchyEvent read FFOnTypeHierarchySupertypes write
        FFOnTypeHierarchySupertypes;
    property FOnWorkspaceDiagnostic: TOnWorkspaceDiagnosticEvent read FFOnWorkspaceDiagnostic write FFOnWorkspaceDiagnostic;
    property OnDocumentDiagnostic: TOnDocumentDiagnosticEvent read FOnDocumentDiagnostic write FOnDocumentDiagnostic;
    property OnInlayHint: TOnInlayHintEvent read FOnInlayHint write FOnInlayHint;
    property OnInlayHintRefresh: TOnInlayHintRefreshEvent read FOnInlayHintRefresh write FOnInlayHintRefresh;
    property OnInlayHintResolve: TOnInlayHintResolveEvent read FOnInlayHintResolve write FOnInlayHintResolve;
    property OnInlineValue: TOnInlineValueEvent read FOnInlineValue write FOnInlineValue;
    property OnInlineValueRefresh: TOnInlineValueRefreshEvent read FOnInlineValueRefresh write FOnInlineValueRefresh;
    property OnPrepareTypeHierarchy: TOnPrepareTypeHierarchyEvent read FOnPrepareTypeHierarchy write
        FOnPrepareTypeHierarchy;
    property OnRegisterCapability: TOnRegisterCapabilityEvent read FOnRegisterCapability write FOnRegisterCapability;
    property OnTypeHierarchySubtypes: TOnPrepareTypeHierarchyEvent read FOnTypeHierarchySubtypes write
        FOnTypeHierarchySubtypes;
    property OnWorkspaceDiagnosticRefresh: TOnWorkspaceDiagnosticRefreshEvent read FOnWorkspaceDiagnosticRefresh write
        FOnWorkspaceDiagnosticRefresh;
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
    property StartTimeout: Integer read FStartTimeout write SetStartTimeout default 15000;
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
    property OnError: TOnErrorEvent read FOnError write FOnError;
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

uses XLSPFunctions, SysUtils, StrUtils, XSuperJSON, System.TimeSpan;

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
  FDynamicCapabilities := TStringList.Create;
  FServerThread := nil;

  // Set default timeout values
  FStartTimeout := 15000;
  FResponseTimeout := 15000;
  FCloseTimeout := 3000;
  FExitTimeout := 1000;

  // Used when initializing the server. We give the server some time to
  // initialize. If it hasn't responded by then, lets terminate the server thread.
  FStartTimer := TTimer.Create(Self);
  FStartTimer.Enabled := False;
  FStartTimer.Interval := FStartTimeout;
  FStartTimer.OnTimer := OnStartTimer;

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
  end;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  FStopwatch.Stop;
  FreeAndNil(FPartialTokens);
  FreeAndNil(FFileLogList);
  FreeAndNil(FDynamicCapabilities);
  inherited;
end;

function TLSPClient.FindDynamicCapability(const method: string): TLSPTextDocumentRegistrationOptions;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FDynamicCapabilities.Count - 1 do
  begin
    if FDynamicCapabilities.ValueFromIndex[i] = method then
    begin
      Result := TLSPTextDocumentRegistrationOptions(FDynamicCapabilities.Objects[i]);
    end;
  end;
end;

procedure TLSPClient.AddToOutputString(const s: string);
var
  ws: RawByteString;
  w: string;
begin
  ws := UTF8Encode(s);
  w := 'Content-Length: ' + IntToStr(Length(ws)) + Char(13) + Char(10) + Char(13) + Char(10);
  ws := UTF8Encode(w) + ws;
  JSONString := JSONString + ws;
  if Assigned(FServerThread) then
  begin
    FServerThread.SendToServer(JSONString);
    if FLogToFile and LogCommunication and (JSONString <> '') then
      SaveToLogFile('Write to server:' + #13#10 + string(JSONString));
    JSONString := '';
  end;
end;

procedure TLSPClient.AddToTempOutputString(const s: string);
var
  ws: RawByteString;
  w: string;
begin
  ws := UTF8Encode(s);
  w := 'Content-Length: ' + IntToStr(Length(ws)) + Char(13) + Char(10) + Char(13) + Char(10);
  FTempOutputString := FTempOutputString + UTF8Encode(w) + ws;
end;

procedure TLSPClient.CloseServer;
begin
  // Enable close down timer and send a shutdown request to the server
  FCloseTimer.Enabled := True;
  FStartTimer.Enabled := False;
  FResponseTimer.Enabled := False;
  SendRequest(lspShutdown);
end;

procedure TLSPClient.ExitServer(const bNoExitEvent: Boolean = False);
begin
  FStartTimer.Enabled := False;
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  if Assigned(FServerThread) then
  begin
    // Send an exit notification to the server
    if bNoExitEvent then
      FServerThread.OnExit := nil;
    SendRequest(lspExit);
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

function TLSPClient.GetIdFromPartialResultToken(const LStr: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := FPartialTokens.IndexOfName(LStr);
  if n >= 0 then Result := StrToInt(FPartialTokens.ValueFromIndex[n]);
end;

function TLSPClient.ProcessServerMessage(const LJson: ISuperObject): Boolean;
var
  id: Integer;
  i,LKind: Integer;
  method: string;
  bRequest: Boolean;
  errorCode: Integer;
  errorMessage: string;
  retriggerRequest: Boolean;
  params,rparams: TLSPBaseParams;
  LInt,LId: Integer;
  ws,LStr: string;
  LValue: string;
  bValue,bValue2: Boolean;
  resultType: TLSPResultType;
  LStrArray: TArray<string>;
  LRegistrations: TLSPRegistrations;
  LUnregistrations: TLSPUnregistrations;
  LWorkspaceFolders: TLSPWorkspaceFolders;
  LWorkspaceCfgs: TLSPConfigurationParams;
  LWorkspaceSymbols: TLSPSymbolInformations;
  LTextEditArray: TArray<TLSPTextEdit>;
begin
  Result := False;

  if not Assigned(LJson) then Exit;

  id := -1;

  if LJson['id'].DataType = dtInteger then
    id := LJson.AsObject.I['id']
  else if LJson['id'].DataType = dtString then
  begin
    ws := LJson.AsObject.S['id'];
    if ws <> '' then id := StrToInt(ws);
  end;
  method := LJson.AsObject.S['method'];

  LId := id;
  if (id < 0) then
    id := GetKindFromMethod(method);

  LKind := GetKindFromMethod(method, id);

  if id < 0 then
  begin
    if (LJson['error'].DataType = dtObject) and (LJson['error.code'].AsInteger < 0) then
      id := Ord(lspError)
    else
      id := Ord(lspShowMessage);
  end;

  params := nil;
  case TLSPKind(LKind) of
    // Error
    lspError:
    begin
      errorCode := LJson['error.code'].AsInteger;
      errorMessage := LJson['error.message'].AsString;
      if Assigned(FOnError) then
        FOnError(Self, errorCode, errorMessage);
      Result := True;
    end;

    // Initialize response from the server
    lspInitialize:
    begin
      // Create result object from Json string and check for errors
      FInitializeResultObject := JsonInitializeResultToObject(LJson, errorCode, errorMessage);

      if not Assigned(FInitializeResultObject) then
      begin
        // OnError event
        if Assigned(FOnError) then
          FOnError(Self, errorCode, errorMessage);
        Exit;
      end;
      FServerCapabilities := FInitializeResultObject.capabilities;
      FServerName := FInitializeResultObject.serverInfo.name;
      FServerVersion := FInitializeResultObject.serverInfo.version;

      // Send Initialized notification to the server
      SendRequest(lspInitialized);

      // OnInitialized event
      if Assigned(FOnInitialized) then
        FOnInitialized(Self, FInitializeResultObject);

      // The server is initialized and ready for communication
      FInitialized := True;

      // Stop the startup timer. The server has been initialized and everything is well
      FStartTimer.Enabled := False;

      // See if we have anything in our temp buffer string
      if FTempOutputString <> '' then
      begin
        // Send all requests and notifications made before the server was initialized
        JSONString := JSONString + FTempOutputString;
        FTempOutputString := '';
      end;
      Result := True;
    end;

    // logMessage notification sent from the server to the client to ask the client to log a particular message.
    lspLogMessage:
    begin
      JsonShowMessageParams(LJson, LInt, LStr);

      // OnLogMessage event
      if Assigned(FOnLogMessage) then
        FOnLogMessage(Self, TLSPMessageType(LInt), LStr);

      if FLogToFile and FLogServerMessages then
      begin
        case TLSPMessageType(LInt) of
          TLSPMessageType.lspMsgError: ws := 'Error: ' + LStr + #13#10;
          TLSPMessageType.lspMsgWarning: ws := 'Warning: ' + LStr + #13#10;
          TLSPMessageType.lspMsgInfo: ws := 'Info: ' + LStr + #13#10;
          TLSPMessageType.lspMsgLog: ws := 'Log: ' + LStr + #13#10;
        end;
        SaveToLogFile(ws);
      end;

      Result := True;
    end;

    // LogTrace notification from server
    lspLogTrace:
    begin
      params := JsonLogTraceParamsToObject(LJson);

      // OnLogTrace event
      if Assigned(FOnLogTrace) then
        FOnLogTrace(Self, TLSPLogTraceParams(params));

      if FLogToFile and FLogServerMessages then
      begin
        ws := 'Logtrace: ' + TLSPLogTraceParams(params).msg + #13#10;
        SaveToLogFile(ws);
      end;

      Result := True;
    end;

    // Progress notification sent from server
    lspProgress:
    begin
      // Get token and check if we have a partial result
      LStr := JsonProgressToken(LJson);
      LKind := GetIdFromPartialResultToken(LStr);
      if LKind < 0 then LKind := Ord(lspProgress);

      params := JsonProgressParamsToObject(LKind, LJson);

      // OnProgress event
      if Assigned(FOnProgress) then
        FOnProgress(Self, TLSPKind(LKind), params);

      case TLSPKind(LKind) of
        lspWorkspaceSymbol:
        begin
          if Length(TLSPWorkspaceSymbolInformationParam(params).values) = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspCallHierarchyIncommingCalls:
        begin
          if Length(TLSPCallHierarchyIncomingCallResponse(params).items) = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspCallHierarchyOutgoingCalls:
        begin
          if Length(TLSPCallHierarchyOutgoingCallResponse(params).items) = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspColorPresentation:
        begin
          if Length(TLSPColorPresentationValues(params).colorPresentations) = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspDocumentColor:
        begin
          if Length(TLSPColorInformationValues(params).colors) = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspDocumentHighlight:
        begin
          if Length(TLSPDocumentHighlightResponse(params).list) = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspDocumentLink:
        begin
          if TLSPDocumentLinkResponse(params).documentLinks.Count = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspDocumentSymbol:
        begin
          if (Length(TLSPDocumentSymbolsResponse(params).symbols) = 0) and
             (Length(TLSPDocumentSymbolsResponse(params).symbolInformations) = 0) then
            UnRegisterPartialResultToken(LStr);
        end;
        lspFoldingRange:
        begin
          if not Assigned(params) or (Length(TLSPFoldingRangeResponse(params).foldingRanges) = 0) then
            UnRegisterPartialResultToken(LStr);
        end;
        lspMoniker:
        begin
          if not Assigned(params) or (Length(TLSPMonikerResult(params).monikers) = 0) then
            UnRegisterPartialResultToken(LStr);
        end;
        lspInlayHint:
        begin
          if not Assigned(params) or (Length(TLSPInlayHintResult(params).inlayHints) = 0) then
            UnRegisterPartialResultToken(LStr);
        end;
        lspSelectionRange:
        begin
          if TLSPSelectionRangeResponse(params).selRanges.Count = 0 then
            UnRegisterPartialResultToken(LStr);
        end;
        lspSemanticTokensFull:
        begin
          if not Assigned(params) or (Length(TLSPSemanticTokens(params).data) = 0) then
            UnRegisterPartialResultToken(LStr);
        end;
        lspSemanticTokensFullDelta:
        begin
          if not Assigned(params) or (Length(TLSPSemanticTokensDelta(params).edits) = 0) then
            UnRegisterPartialResultToken(LStr);
        end;
        lspSemanticTokensRange:
        begin
          if not Assigned(params) or (Length(TLSPSemanticTokens(params).data) = 0) then
            UnRegisterPartialResultToken(LStr);
        end;
        lspGotoDeclaration,
        lspGotoDefinition,
        lspGotoTypeDefinition,
        lspGotoImplementation:
        begin
          if (Length(TLSPGotoResponse(params).locationLinks) = 0) and
             (Length(TLSPGotoResponse(params).locations) = 0) and
             (TLSPGotoResponse(params).location.uri = '') then
          begin
            UnRegisterPartialResultToken(LStr);
          end;
        end;
      end;

      Result := True;
    end;

    // A call hierarchy incomming calls request was sent to the server. An event is triggered when the server responds.
    lspCallHierarchyIncommingCalls:
    begin
      params := JsonCallHierarchyIncommingResponseToObject(LJson, errorCode, errorMessage);

      // OnCallHierarchyIncomming event
      if Assigned(FOnCallHierarchyIncomming) then
        FOnCallHierarchyIncomming(Self, TLSPCallHierarchyIncomingCallResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A call hierarchy outgoing calls request was sent to the server. An event is triggered when the server responds.
    lspCallHierarchyOutgoingCalls:
    begin
      params := JsonCallHierarchyOutgoingResponseToObject(LJson, errorCode, errorMessage);

      // OnCallHierarchyOutgoing event
      if Assigned(FOnCallHierarchyOutgoing) then
        FOnCallHierarchyOutgoing(Self, TLSPCallHierarchyOutgoingCallResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // Client/RegisterCapability request sent from the server to register for a new capability on the client side.
    lspClientRegisterCapabilities:
    begin
      errorCode := 0;
      LRegistrations := JsonRegisterCapabilitiesToRegistrations(LJson);

      // OnRegisterCapabilities event
      if Assigned(FOnRegisterCapability) then
        FOnRegisterCapability(Self, LRegistrations, errorCode, errorMessage);

      // An error occurred. Send the error message back to the server.
      if errorCode <> 0 then
      begin
        LId := id;
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
      end;

      // Register dynamic options
      if (errorCode = 0) then
      begin
        for i := 0 to Length(LRegistrations) - 1 do
          RegisterCapability(LRegistrations[i]);
      end;

      // Send responce to the server if the server expect a response (LId > -1)
      if LId > -1 then
        SendResponse(id,LSPIdStrings[LKind],nil,params,lsprVoid);

      Result := True;
    end;

    // A code action request was sent to the server. An event is triggered when the server responds.
    lspCodeAction:
    begin
      params := JsonCodeActionResponseToObject(LJson, errorCode, errorMessage);

      // OnCodeAction event
      if Assigned(FOnCodeAction) then
        FOnCodeAction(Self, TLSPCodeActionResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A code action resolve request was sent to the server. An event is triggered when the server responds.
    lspCodeActionResolve:
    begin
      params := JsonCodeActionResolveToObject(LJson, errorCode, errorMessage);

      // OnCodeActionResolve event
      if Assigned(FOnCodeActionResolve) then
        FOnCodeActionResolve(Self, TLSPCodeAction(params), errorCode, errorMessage);

      Result := True;
    end;

    // A code lens request was sent to the server. An event is triggered when the server responds.
    lspCodeLens:
    begin
      params := JsonCodeLensToObject(LJson, errorCode, errorMessage);

      // OnCodeAction event
      if Assigned(FOnCodeLens) then
        FOnCodeLens(Self, TLSPCodeLensResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A code lens resolve request was sent to the server. An event is triggered when the server responds.
    lspCodeLensResolve:
    begin
      params := JsonCodeLensResolveToObject(LJson, errorCode, errorMessage);

      // OnCodeLensResolve event
      if Assigned(FOnCodeLensResolve) then
        FOnCodeLensResolve(Self, TLSPCodeLens(params), errorCode, errorMessage);

      Result := True;
    end;

    // A code lens refresh request was sent from the server.
    lspCodeLensRefresh:
    begin
      // OnCodeLensRefresh
      if Assigned(FOnCodeLensRefresh) then
        FOnCodeLensRefresh(Self, errorCode, errorMessage);

      // An error occurred. Send the error message back to the server.
      if errorCode <> 0 then
      begin
        LId := id;
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
      end;

      // Send responce to the server if the server expect a response (LId > -1)
      if LId > -1 then
        SendResponse(id,LSPIdStrings[LKind], nil, params, lsprVoid);

      Result := True;
    end;

    // A color Presentation request was sent to the server. An event is triggered when the server responds.
    lspColorPresentation:
    begin
      params := JsonColorPresentationValuesToObject(LJson, errorCode, errorMessage);

      // OnColorPresentation event
      if Assigned(FOnColorPresentation) then
        FOnColorPresentation(Self, TLSPColorPresentationValues(params), errorCode, errorMessage);

      Result := True;
    end;

    // A completion request was sent to the server. An event is triggered when the server responds.
    lspCompletion:
    begin
      params := JsonCompletionResponseToObject(LJson, errorCode, errorMessage);

      // OnCompletion event
      if Assigned(FOnCompletion) then
        FOnCompletion(Self, TLSPCompletionList(params), errorCode, errorMessage);

      Result := True;
    end;

    // A completion item resolve request was sent to the server. An event is triggered when the server responds.
    lspCompletionItemResolve:
    begin
      params := JsonCompletionItemResolveToObject(LJson, errorCode, errorMessage);

      // OnCompletionItemResolve event
      if Assigned(FOnCompletionItemResolve) then
        FOnCompletionItemResolve(Self, TLSPCompletionItem(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document color request was sent to the server. An event is triggered when the server responds.
    lspDocumentColor:
    begin
      params := JsonDocumentColorValuesToObject(LJson, errorCode, errorMessage);

      // OnDocumentColor event
      if Assigned(FOnDocumentColor) then
        FOnDocumentColor(Self, TLSPColorInformationValues(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document formatting request was sent to the server. An event is triggered when the server responds.
    lspDocumentFormatting:
    begin
      params := JsonDocumentFormattingResponseToObject(LJson, errorCode, errorMessage);

      // OnDocumentFormatting event
      if Assigned(FOnDocumentFormatting) then
        FOnDocumentFormatting(Self, TLSPTextEditValues(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document range formatting request was sent to the server. An event is triggered when the server responds.
    lspDocumentRangeFormatting:
    begin
      params := JsonDocumentFormattingResponseToObject(LJson, errorCode, errorMessage);

      // OnDocumentRangeFormatting event
      if Assigned(FOnDocumentRangeFormatting) then
        FOnDocumentRangeFormatting(Self, TLSPTextEditValues(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document on type formatting request was sent to the server. An event is triggered when the server responds.
    lspDocumentOnTypeFormatting:
    begin
      params := JsonDocumentFormattingResponseToObject(LJson, errorCode, errorMessage);

      // OnDocumentOnTypeFormatting event
      if Assigned(FOnDocumentOnTypeFormatting) then
        FOnDocumentOnTypeFormatting(Self, TLSPTextEditValues(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document highlight request was sent to the server. An event is triggered when the server responds.
    lspDocumentHighlight:
    begin
      params := JsonDocumentHighlightResponseToObject(LJson, errorCode, errorMessage);

      // OnDocumentHighlight event
      if Assigned(FOnDocumentHighlight) then
        FOnDocumentHighlight(Self, TLSPDocumentHighlightResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document link request was sent to the server. An event is triggered when the server responds.
    lspDocumentLink:
    begin
      params := JsonDocumentLinkResponseToObject(LJson, errorCode, errorMessage);

      // OnDocumentLink event
      if Assigned(FOnDocumentLink) then
        FOnDocumentLink(Self, TLSPDocumentLinkResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document link resolve request was sent to the server. An event is triggered when the server responds.
    lspDocumentLinkResolve:
    begin
      params := JsonDocumentLinkResolveToObject(LJson, errorCode, errorMessage);

      // OnDocumentLinkResolve event
      if Assigned(FOnDocumentLinkResolve) then
        FOnDocumentLinkResolve(Self, TLSPDocumentLink(params), errorCode, errorMessage);

      Result := True;
    end;

    // A document symbols request was sent to the server. An event is triggered when the server responds.
    lspDocumentSymbol:
    begin
      params := JsonDocumentSymbolsResponseToObject(LJson, errorCode, errorMessage);

      // OnDocumentSymbols event
      if Assigned(FOnDocumentSymbols) then
        FOnDocumentSymbols(Self, TLSPDocumentSymbolsResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A folding range request was sent to the server. An event is triggered when the server responds.
    lspFoldingRange:
    begin
      params := JsonFoldingRangeResponseToObject(LJson, errorCode, errorMessage);

      // OnFoldingRange event
      if Assigned(FOnFoldingRange) then
        FOnFoldingRange(Self, TLSPFoldingRangeResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // The go to declaration request is sent from the client to the server to resolve the
    // declaration location of a symbol at a given text document position. An event is triggered when the server responds.
    lspGotoDeclaration:
    begin
      params := JsonGotoResponseToObject(LJson, errorCode, errorMessage);

      // OnGotoDeclaration event
      if Assigned(FOnGotoDeclaration) then
        FOnGotoDeclaration(Self, TLSPGotoResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // The go to definition request is sent from the client to the server to resolve the definition
    // location of a symbol at a given text document position.
    lspGotoDefinition:
    begin
      params := JsonGotoResponseToObject(LJson, errorCode, errorMessage);

      // OnGotoDefinition event
      if Assigned(FOnGotoDefinition) then
        FOnGotoDefinition(Self, TLSPGotoResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // The go to implementation request is sent from the client to the server to resolve the implementation
    // location of a symbol at a given text document position.
    lspGotoImplementation:
    begin
      params := JsonGotoResponseToObject(LJson, errorCode, errorMessage);

      // OnGotoImplementation event
      if Assigned(FOnGotoImplementation) then
        FOnGotoImplementation(Self, TLSPGotoResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // The go to type definition request is sent from the client to the server to resolve the type definition
    // location of a symbol at a given text document position.
    lspGotoTypeDefinition:
    begin
      params := JsonGotoResponseToObject(LJson, errorCode, errorMessage);

      // OnGotoTypeDefinition event
      if Assigned(FOnGotoTypeDefinition) then
        FOnGotoTypeDefinition(Self, TLSPGotoResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A hover request was sent to the server. An event is triggered when the server responds.
    lspHover:
    begin
      params := JsonHoverResponseToObject(LJson, errorCode, errorMessage);

      // OnHover event
      if Assigned(FOnHover) then
        FOnHover(Self, TLSPHover(params), errorCode, errorMessage);

      Result := True;
    end;

    // A linked editing range request was sent to the server. An event is triggered when the server responds.
    lspLinkedEditingRange:
    begin
      params := JsonLinkedEditingRangesToObject(LJson, errorCode, errorMessage);

      // OnLinkedEditingRange event
      if Assigned(FOnLinkedEditingRange) then
        FOnLinkedEditingRange(Self, TLSPLinkedEditingRanges(params), errorCode, errorMessage);

      Result := True;
    end;

    // A moniker request was sent to the server. An event is triggered when the server responds.
    lspMoniker:
    begin
      params := JsonMonikerToObject(LJson, errorCode, errorMessage);

      // OnMoniker event
      if Assigned(FOnMoniker) then
        FOnMoniker(Self, TLSPMonikerResult(params), errorCode, errorMessage);

      Result := True;
    end;

    // An inlayHint request was sent to the server. An event is triggered when the server responds.
    lspInlayHint:
    begin
      params := JsonInlayHintResponseToObject(LJson, errorCode, errorMessage);

      // OnInlayHint event
      if Assigned(FOnInlayHint) then
        FOnInlayHint(Self, TLSPInlayHintResult(params), errorCode, errorMessage);

      Result := True;
    end;

    // An inlayHint resolve request was sent to the server. An event is triggered when the server responds.
    lspInlayHintResolve:
    begin
      params := JsonInlayHintResolveToObject(LJson, errorCode, errorMessage);

      // OnInlayHintResolve event
      if Assigned(FOnInlayHintResolve) then
        FOnInlayHintResolve(Self, TLSPInlayHint(params), errorCode, errorMessage);

      Result := True;
    end;

    // An inlayHint refresh was sent from the server.
    lspInlayHintRefresh:
    begin
      JsonInlayHintRefreshToObject(LJson, errorCode, errorMessage);

      // OnInlayHintRefresh event
      if Assigned(FOnInlayHintRefresh) then
        FOnInlayHintRefresh(Self, errorCode, errorMessage);

      Result := True;
    end;

    // An inlineValue request was sent to the server. An event is triggered when the server responds.
    lspInlineValue:
    begin
      params := JsonInlineValueResponseToObject(LJson, errorCode, errorMessage);

      // OnInlineValue event
      if Assigned(FOnInlineValue) then
        FOnInlineValue(Self, TLSPInlineValueResult(params), errorCode, errorMessage);

      Result := True;
    end;

    // An inlineValue refresh was sent from the server.
    lspInlineValueRefresh:
    begin
      JsonInlineValueRefreshToObject(LJson, errorCode, errorMessage);

      // OnInlineValueRefresh event
      if Assigned(FOnInlineValueRefresh) then
        FOnInlineValueRefresh(Self, errorCode, errorMessage);

      Result := True;
    end;

    // Document diagnostic request was sent to the server. An event is triggered when the server responds.
    lspDocumentDiagnostic:
    begin
      params := JsonDocumentDiagnosticReportToObject(LJson, errorCode, errorMessage, retriggerRequest);

      // OnDocumentDiagnostic event
      if Assigned(FOnDocumentDiagnostic) then
        FOnDocumentDiagnostic(Self,
                              TLSPDocumentDiagnosticReport(params).kind,
                              TLSPDocumentDiagnosticReport(params).resultId,
                              TLSPDocumentDiagnosticReport(params).items,
                              errorCode, errorMessage, retriggerRequest);
    end;

    // Workspace diagnostic request was sent to the server. An event is triggered when the server responds.
    lspWorkspaceDiagnostic:
    begin
      params := JsonWorkspaceDiagnosticReportToObject(LJson, errorCode, errorMessage, retriggerRequest);

      // OnWorkspaceDiagnostic event
      if Assigned(FOnWorkspaceDiagnostic) then
        FOnWorkspaceDiagnostic(Self,
                              TLSPWorkspaceDiagnosticReport(params).items,
                              errorCode, errorMessage, retriggerRequest);
    end;

    // A workspace diagnostic refresh request was sent from the server.
    lspWorkspaceDiagnosticRefresh:
    begin
      JsonWorkspaceDiagnosticRefreshToObject(LJson, errorCode, errorMessage);

      // OnWorkspaceDiagnosticRefresh event
      if Assigned(FOnWorkspaceDiagnosticRefresh) then
        FOnWorkspaceDiagnosticRefresh(Self, errorCode, errorMessage);

      // An error occurred. Send the error message back to the server.
      if errorCode <> 0 then
      begin
        LId := id;
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
      end;

      // Send responce to the server if the server expect a response (LId > -1)
      if LId > -1 then
        SendResponse(id,LSPIdStrings[LKind], nil, params, lsprVoid);

      Result := True;
    end;

    // Publish diagnostics notification sent from the server.
    lspPublishDiagnostics:
    begin
      params := JsonPublishDiagnosticsToObject(LJson);

      // OnPublishDiagnostics event
      if Assigned(FOnPublishDiagnostics) then
        FOnPublishDiagnostics(Self,
                              TLSPPublishDiagnosticsParams(params).uri,
                              TLSPPublishDiagnosticsParams(params).version,
                              TLSPPublishDiagnosticsParams(params).diagnostics);
    end;

    // A prepare call hierarchy request was sent to the server. An event is triggered when the server responds.
    lspPrepareCallHierarchy:
    begin
      params := JsonPrepareCallHierarchyResponseToObject(LJson, errorCode, errorMessage);

      // OnPrepareCallHierarchy event
      if Assigned(FOnPrepareCallHierarchy) then
        FOnPrepareCallHierarchy(Self, TLSPPrepareCallHierarchyResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A prepare type hierarchy request was sent to the server. An event is triggered when the server responds.
    lspPrepareTypeHierarchy:
    begin
      params := JsonPrepareTypeHierarchyResponseToObject(LJson, errorCode, errorMessage);

      // OnPrepareTypeHierarchy event
      if Assigned(FOnPrepareTypeHierarchy) then
        FOnPrepareTypeHierarchy(Self, TLSPPrepareTypeHierarchyResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A type hierarchy super types request was sent to the server. An event is triggered when the server responds.
    lspTypeHierarchySupertypes:
    begin
      params := JsonTypeHierarchySupertypesResponseToObject(LJson, errorCode, errorMessage);

      // OnTypeHierarchySupertypes event
      if Assigned(FOnTypeHierarchySupertypes) then
        FOnTypeHierarchySupertypes(Self, TLSPPrepareTypeHierarchyResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A call hierarchy sub types request was sent to the server. An event is triggered when the server responds.
    lspTypeHierarchySubtypes:
    begin
      params := JsonTypeHierarchySupertypesResponseToObject(LJson, errorCode, errorMessage);

      // OnTypeHierarchySubtypes event
      if Assigned(FOnTypeHierarchySubtypes) then
        FOnTypeHierarchySubtypes(Self, TLSPPrepareTypeHierarchyResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A prepare rename request was sent to the server. An event is triggered when the server responds.
    lspPrepareRename:
    begin
      params := JsonPrepareRenameResponseToObject(LJson, errorCode, errorMessage);

      // OnPrepareRename event
      if Assigned(FOnPrepareRename) then
        FOnPrepareRename(Self, TLSPPrepareRenameResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A find references request is sent from the client to the server to resolve project wide
    // references for a symbol at a given text document position.
    lspReferences:
    begin
      params := JsonFindReferencesResponseToObject(LJson, errorCode, errorMessage);

      // OnFindReferences event
      if Assigned(FOnFindReferences) then
        FOnFindReferences(Self, TLSPFindReferencesResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A rename request was sent to the server. An event is triggered when the server responds.
    lspRename:
    begin
      params := JsonRenameResponseToObject(LJson, errorCode, errorMessage);

      // OnRename event
      if Assigned(FOnRename) then
        FOnRename(Self, TLSPWorkspaceEdit(params), errorCode, errorMessage);

      Result := True;
    end;

    // A selction range request was sent to the server. An event is triggered when the server responds.
    lspSelectionRange:
    begin
      params := JsonSelectionRangeResponseToObject(LJson, errorCode, errorMessage);

      // OnSelectionRange event
      if Assigned(FOnSelectionRange) then
        FOnSelectionRange(Self, TLSPSelectionRangeResponse(params), errorCode, errorMessage);

      Result := True;
    end;

    // A semantic tokens full request was sent to the server. An event is triggered when the server responds.
    lspSemanticTokensFull:
    begin
      params := JsonSemanticTokensFullToObject(LJson, errorCode, errorMessage);

      // OnSemanticTokensFull event
      if Assigned(FOnSemanticTokensFull) then
        FOnSemanticTokensFull(Self, TLSPSemanticTokens(params), errorCode, errorMessage);

      Result := True;
    end;

    // A semantic tokens full delta request was sent to the server. An event is triggered when the server responds.
    lspSemanticTokensFullDelta:
    begin
      params := JsonSemanticTokensFullDeltaToObject(LJson, errorCode, errorMessage);

      // OnSemanticTokensFullDelta event
      if Assigned(FOnSemanticTokensFullDelta) then
        FOnSemanticTokensFullDelta(Self, TLSPSemanticTokensDelta(params), errorCode, errorMessage);

      Result := True;
    end;

    // A semantic tokens refresh was sent from the server.
    lspSemanticTokensRefresh:
    begin
      JsonSemanticTokensRefresh(LJson, errorCode, errorMessage);

      // OnSemanticTokensRefresh event
      if Assigned(FOnSemanticTokensRefresh) then
        FOnSemanticTokensRefresh(Self, errorCode, errorMessage);

      Result := True;
    end;

    // A semantic tokens range request was sent to the server. An event is triggered when the server responds.
    lspSemanticTokensRange:
    begin
      params := JsonSemanticTokensFullToObject(LJson, errorCode, errorMessage);

      // OnSemanticTokensRange event
      if Assigned(FOnSemanticTokensRange) then
        FOnSemanticTokensRange(Self, TLSPSemanticTokens(params), errorCode, errorMessage);

      Result := True;
    end;

    // ShowDocument request sent from the server. Return success (boolean) to the server.
    lspShowDocumentRequest:
    begin
      bValue := False;
      params := JsonShowDocumentRequestParams(LJson);

      // OnShowDocument event
      if Assigned(FOnShowDocument) then
        FOnShowDocument(Self,
                               TLSPShowDocumentParams(params).uri,
                               TLSPShowDocumentParams(params).inexternal,
                               TLSPShowDocumentParams(params).takeFocus,
                               TLSPShowDocumentParams(params).selection.startPos,
                               TLSPShowDocumentParams(params).selection.endPos,
                               bValue);

      FreeAndNil(params);

      // Return the result of the show document request to the server.
      params := TLSPShowDocumentResult.Create;
      TLSPShowDocumentResult(params).success := bValue;
      SendResponse(id,LSPIdStrings[LKind],params);

      Result := True;
    end;

    // ShowMessage notification sent from the server
    lspShowMessage:
    begin
      JsonShowMessageParams(LJson, LInt, LStr);

      // OnShowMessage event
      if Assigned(FOnShowMessage) then
        FOnShowMessage(Self, TLSPMessageType(LInt), LStr);

      Result := True;
    end;

    // ShowMessage request sent from the server. Return selected action to the server.
    lspShowMessageRequest:
    begin
      JsonShowMessageRequestParams(LJson, LInt, LStr, LStrArray);

      // OnShowMessageRequest event
      if Assigned(FOnShowMessageRequest) then
        FOnShowMessageRequest(Self, TLSPMessageType(LInt), LStr, LStrArray, LValue);

      // Return selected action to the server. nil if nothing was selected by the user.
      if LValue <> '' then
      begin
        params := TLSPShowMessageRequestResponse.Create;
        TLSPShowMessageRequestResponse(params).title := LValue;
      end;
      SendResponse(id, LSPIdStrings[LKind], params);

      Result := True;
    end;

    // Shutdown result from server
    lspShutdown:
    begin
      JsonShutdownResult(LJson, errorCode, errorMessage);

      if FCloseTimer.Enabled then
      begin
        // CloseServer() has been called
        FCloseTimer.Enabled := False;
        ExitServer(False);
        Exit;
      end;

      // OnShutdown event
      if Assigned(FOnShutdown) then
        FOnShutdown(Self, errorCode, errorMessage);
    end;

    // A signature help request was sent to the server. An event is triggered when the server responds.
    lspSignatureHelp:
    begin
      params := JsonSignatureHelpResponseToObject(LJson, errorCode, errorMessage);

      // OnSignatureHelp event
      if Assigned(FOnSignatureHelp) then
        FOnSignatureHelp(Self, TLSPSignatureHelp(params), errorCode, errorMessage);

      Result := True;
    end;

    // Telemetry notification sent from the server to ask the client to log a telemetry event.
    lspTelemetryEvent:
    begin
      // OnTelemetry event. Just send the Json data with no processing.
      if Assigned(FOnTelemetryEvent) then
        FOnTelemetryEvent(Self, LJson.AsJSON);
    end;

    // Client/unregisterCapability request sent from the server to unregister a previously registered capability.
    lspClientUnRegisterCapabilities:
    begin
      errorCode := 0;
      LUnregistrations := JsonUnregisterCapabilitiesToUnregistrations(LJson);

      // OnRegisterCapabilities event
      if Assigned(FOnUnregisterCapability) then
        FOnUnregisterCapability(Self, LUnregistrations, errorCode, errorMessage);

      // An error occurred. Send the error message back to the server.
      if errorCode <> 0 then
      begin
        LId := id;
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
      end;

      // UnRegister dynamic options
      if (errorCode = 0) then
      begin
        for i := 0 to Length(LRegistrations) - 1 do
          UnRegisterCapability(LUnRegistrations[i]);
      end;

      // Send responce to the server if the server expect a response (LId > -1)
      if LId > -1 then
        SendResponse(id,LSPIdStrings[LKind],nil,params,lsprVoid);

      Result := True;
    end;

    // WorkDoneProgress request is sent from the server to ask the client to create a work done progress.
    lspWorkDoneProgress:
    begin
      errorCode := 0;
      errorMessage := '';
      JsonWorkDoneProgressRequestParams(LJson, LStr);

      // OnWorkDoneProgress event
      if Assigned(FOnWorkDoneProgress) then
        FOnWorkDoneProgress(Self, LStr, errorCode, errorMessage);

      // An error occurred. Send the error message back to the server.
      // The server doesn't expect a response to this request, unless there is an error.
      if errorCode <> 0 then
      begin
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
        SendResponse(id, LSPIdStrings[LKind], nil, params);
      end;
      Result := True;
    end;

    lspWorkspaceApplyEdit:
    begin
      errorCode := 0;
      errorMessage := '';
      params := JsonWorkspaceApplyEditParamsToObject(LJson, '', errorCode, errorMessage);
      rparams := TLSPApplyWorkspaceEditResponse.Create;

      // OnWorkspaceApplyEdit event
      if Assigned(FOnWorkspaceApplyEdit) then
        FOnWorkspaceApplyEdit(Self, TLSPApplyWorkspaceEditParams(params), TLSPApplyWorkspaceEditResponse(rparams), errorCode, errorMessage);

      params := nil;
      // An error occurred. Send the error message back to the server.
      if errorCode <> 0 then
      begin
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
      end;
      SendResponse(id, LSPIdStrings[LKind], rparams, params);
      Result := True;
    end;

    // workspace/configuration request is sent from the server to the client to fetch configuration
    // settings from the client.
    lspWorkspaceConfiguration:
    begin
      errorCode := 0;
      LWorkspaceCfgs := JsonConfigurationParamsToObjects(LJson);

      // OnConfiguration event
      if Assigned(FOnConfiguration) then
        FOnConfiguration(Self, LWorkspaceCfgs, LStr, errorCode, errorMessage);

      // An error occurred. Send the error message back to the server.
      if errorCode <> 0 then
      begin
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
      end;

      if LStr = '' then LStr := '[null]';

      // Send responce to the server
      SendResponse(id,LSPIdStrings[LKind],nil,params,lsprString,LStr);

      Result := True;
    end;

    // Resonse after the client has sent an execute command request
    lspWorkspaceExecuteCommand:
    begin
      // The result from the response can be any type of object. Or it can be null.
      LStr := JsonExecuteCommandResult(LJson, errorCode, errorMessage);

      // OnExecuteCommandRequest event
      if Assigned(FOnExecuteCommandRequest) then
        FOnExecuteCommandRequest(Self, LStr, errorCode, errorMessage);
    end;

    // workspace/workspaceFolders request is sent from the server to the client to fetch the current open list
    // of workspace folders. Returns null in the response if only a single file is open in the tool.
    // Returns an empty array if a workspace is open but no folders are configured.
    lspWorkspaceFolders:
    begin
      LWorkspaceFolders := nil;
      errorCode := 0;
      errorMessage := '';
      bValue := False;
      bValue2 := False;
      LStr := '';

      // No params are sent from the server.

      // OnWorkspaceFolders event
      if Assigned(FOnWorkspaceFolders) then
        FOnWorkspaceFolders(Self, LWorkspaceFolders, bValue, bValue2, errorCode, errorMessage);

      // bValue  = True if a single file is opened and no workspace is opened [lsprNull]
      // bValue2 = True if a workspace is defined but no folders are configured [lsprEmptyArray]
      if bValue2 then
        resultType := lsprEmptyArray
      else if bValue then
        resultType := lsprNull
      else
        resultType := lsprString;

      if (resultType = lsprString) and (Length(LWorkspaceFolders) > 0) then
        LStr := TJSON.Stringify<TArray<TLSPWorkspaceFolder>>(LWorkspaceFolders);

      // An error occurred. Send the error message back to the server.
      if errorCode <> 0 then
      begin
        params := TLSPResponseError.Create;
        TLSPResponseError(params).code := errorCode;
        TLSPResponseError(params).msg := errorMessage;
      end;
      SendResponse(id, LSPIdStrings[LKind], nil, params, resultType, LStr);
      Result := True;
    end;

    // Workspace symbols request was sent to the server. Handle the server result and trigger an event.
    lspWorkspaceSymbol:
    begin
      // Create result object from Json string and check for errors
      LWorkspaceSymbols := JsonWorkspaceSymbolResultToObject(LJson, errorCode, errorMessage);

      if errorCode <> 0 then
      begin
        // OnError event
        if Assigned(FOnError) then
          FOnError(Self, errorCode, errorMessage);
        Exit;
      end;

      // OnWorkspaceSymbol event
      if Assigned(FOnWorkspaceSymbol) then
        FOnWorkspaceSymbol(Self, LWorkspaceSymbols);

      Result := True;
    end;

    // willCreateFiles request was sent to the server. Handle the response and trigger an event.
    lspWorkspaceWillCreateFiles:
    begin
      // Create result object from Json string and check for errors
      LStr := 'result';
      params := JsonWorkspaceApplyEditParamsToObject(LJson, LStr, errorCode, errorMessage);

      if errorCode <> 0 then
      begin
        // OnError event
        if Assigned(FOnError) then
          FOnError(Self, errorCode, errorMessage);
        Exit;
      end;

      // OnWorkspaceWillCreateFiles event
      if Assigned(FOnWorkspaceWillCreateFiles) then
      begin
        if Assigned(params) then
          FOnWorkspaceWillCreateFiles(Self, TLSPWorkspaceEdit(TLSPApplyWorkspaceEditParams(params).edit))
        else
          FOnWorkspaceWillCreateFiles(Self, nil);
      end;

      Result := True;
    end;

    // willDeleteFiles request was sent to the server. Handle the response and trigger an event.
    lspWorkspaceWillDeleteFiles:
    begin
      // Create result object from Json string and check for errors
      LStr := 'result';
      params := JsonWorkspaceApplyEditParamsToObject(LJson, LStr, errorCode, errorMessage);

      if errorCode <> 0 then
      begin
        // OnError event
        if Assigned(FOnError) then
          FOnError(Self, errorCode, errorMessage);
        Exit;
      end;

      // OnWorkspaceWillDeleteFiles event
      if Assigned(FOnWorkspaceWillDeleteFiles) then
      begin
        if Assigned(params) then
          FOnWorkspaceWillDeleteFiles(Self, TLSPWorkspaceEdit(TLSPApplyWorkspaceEditParams(params).edit))
        else
          FOnWorkspaceWillDeleteFiles(Self, nil);
      end;

      Result := True;
    end;

    // willRenameFiles request was sent to the server. Handle the response and trigger an event.
    lspWorkspaceWillRenameFiles:
    begin
      // Create result object from Json string and check for errors
      LStr := 'result';
      params := JsonWorkspaceApplyEditParamsToObject(LJson, LStr, errorCode, errorMessage);

      if errorCode <> 0 then
      begin
        // OnError event
        if Assigned(FOnError) then
          FOnError(Self, errorCode, errorMessage);
        Exit;
      end;

      // OnWorkspaceWillRenameFiles event
      if Assigned(FOnWorkspaceWillRenameFiles) then
      begin
        if Assigned(params) then
          FOnWorkspaceWillRenameFiles(Self, TLSPWorkspaceEdit(TLSPApplyWorkspaceEditParams(params).edit))
        else
          FOnWorkspaceWillRenameFiles(Self, nil);
      end;

      Result := True;
    end;

    // WillSaveWaitUntilTextDocument request was sent to the server. Handle the response and trigger an event.
    lspWillSaveWaitUntilTextDocument:
    begin
      // Create result object from Json string and check for errors
      LTextEditArray := JsonWillSaveWaitUntilResponseToObject(LJson, errorCode, errorMessage);

      if errorCode <> 0 then
      begin
        // OnError event
        if Assigned(FOnError) then
          FOnError(Self, errorCode, errorMessage);
        Exit;
      end;

      // OnWillSaveWaitUntilTextDocument event
      if Assigned(FOnWillSaveWaitUntilTextDocument) then
        FOnWillSaveWaitUntilTextDocument(Self, LTextEditArray);

      Result := True;
    end;

    else
    begin
      if (LJson['error'].DataType = dtObject) and (LJson['error.code'].AsInteger < 0) then
      begin
        errorCode := LJson['error.code'].AsInteger;
        errorMessage := LJson['error.message'].AsString;
        if Assigned(FOnError) then
          FOnError(Self, errorCode, errorMessage);
        Result := True;
      end;
    end;
  end;

  if Assigned(params) then
    params.Free;
end;

procedure TLSPClient.OnExitServer(Sender: TObject; exitcode: Integer);
begin
  // The server has closed down and the exit code is returned
  FStartTimer.Enabled := False;
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  if Assigned(FOnExit) then
    FOnExit(Self, exitcode, FRestartServer);
end;

procedure TLSPClient.OnReadFromServer(Sender: TObject; const AJson: ISuperObject; const APlainText: string);
begin
  FResponseTimer.Enabled := False;
  if FLogToFile and FLogCommunication and (APlainText <> '') then
    SaveToLogFile('Read from server:' + #13#10 + APlainText);

  // Handle JSON input from LSP server
  if (AJson = nil) and (APlainText <> '') then
  begin
    if Assigned(FOnLogMessage) then
      FOnLogMessage(Self,TLSPMessageType.lspMsgLog,APlainText);
  end
  else if Assigned(AJson) then
  begin
    ProcessServerMessage(AJson);
  end;
end;

procedure TLSPClient.OnServerThreadTerminate(Sender: TObject);
begin
  FServerThread := nil;
  FRestartServer := False;
  FStartTimer.Enabled := False;
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;

  FStopwatch.Stop;

  Initialized := False;
  Id := '';
  ServerName := '';
  FreeAndNil(FServerCapabilities);
  FreeAndNil(FInitializeResultObject);
end;

procedure TLSPClient.OnStartTimer(Sender: TObject);
begin
  FStartTimer.Enabled := False;
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  if Assigned(FServerThread) then
  begin
    // The server doesn't seem to have responded at startup.
    // Terminate the thread.
    FServerThread.Terminate;
  end;
end;

procedure TLSPClient.OnWriteToServer(Sender: TObject; var s: RawByteString);
begin
  // Send JSON string to server
  s := FJSONString;
  FJSONString := '';
  if FLogToFile and LogCommunication and (s <> '') then
    SaveToLogFile('Write to server:' + #13#10 + string(s));
end;

procedure TLSPClient.RegisterCapability(const item: TLSPRegistration);
var
  id,sm: string;
  options: TLSPTextDocumentRegistrationOptions;
begin
  id := item.id;
  sm := item.method;
  options := item.registerOptions;
  FDynamicCapabilities.AddObject(id + '=' + sm, options);
end;

procedure TLSPClient.RegisterPartialResultToken(const lspKind: TLSPKind; const token: string);
var
  n: Integer;
begin
  n := Ord(lspKind);
  FPartialTokens.Add(token + '=' + IntToStr(n));
end;

procedure TLSPClient.RunServer(const ACommandline, ADir: String; const
    AEnvList: string = ''; const AHost: string = ''; const APort: Integer = 0;
    const AUseSocketAsServer: Boolean = False; const AUseSocketAsClient:
    Boolean = False);
begin
  FServerThread := TLSPExecuteServerThread.Create(ACommandline, ADir);
  if AUseSocketAsServer then
    FServerThread.TransportType := TTransportType.ttSocketServer
  else if AUseSocketAsClient then
    FServerThread.TransportType := TTransportType.ttSocketClient
  else
    FServerThread.TransportType := TTransportType.ttStdIO;
  FServerThread.Host := AHost;
  FServerThread.Port := APort;
  FServerThread.OnReadFromServer := OnReadFromServer;
  FServerThread.OnExit := OnExitServer;
  FServerThread.OnTerminate := OnServerThreadTerminate;
  FServerThread.Start;

  FStopwatch.Start;
end;

function TLSPClient.GetRunTimeInSeconds: Double;
var
  elapsed: TTimeSpan;
begin
  elapsed := FStopwatch.Elapsed;
  Result := elapsed.TotalSeconds;
end;

function TLSPClient.GetSyncKind: Integer;
var
  sm: string;
  options: TLSPTextDocumentRegistrationOptions;
begin
  Result := TLSPTextDocumentSyncKindRec.None;
  if not Assigned(ServerCapabilities.textDocumentSync) then Exit;

  if Assigned(ServerCapabilities.textDocumentSync) then
  begin
    Result := ServerCapabilities.textDocumentSync.change;
    Exit;
  end;
end;

function TLSPClient.IncludeText(const lspKind: TLSPKind; const filename: string; const includeDefault: Boolean):
    Boolean;
begin
  Result := includeDefault;
  case lspKind of
    lspDidSaveTextDocument: begin
      if Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
         Assigned(ServerCapabilities.textDocumentSync.save) then
      begin
        Result := ServerCapabilities.textDocumentSync.save.includeText;
        Exit;
      end;
    end;
  end;
end;

function TLSPClient.IsRequestSupported(const lspKind: TLSPKind; const ext: string = '*'): Boolean;
var
  LNotebookSelector: TLSPNoteBookSelector;
begin
  Result := False;
  if not Assigned(ServerCapabilities) then Exit;

  case lspKind of
    // A few notifications are added here as well as it can be useful...
    lspDidOpenTextDocument: begin
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
                ServerCapabilities.textDocumentSync.openClose;
    end;
    lspDidCloseTextDocument: begin
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
                ServerCapabilities.textDocumentSync.openClose;
    end;
    lspDidChangeTextDocument: begin
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
                (ServerCapabilities.textDocumentSync.change > 0);
    end;
    lspDidSaveTextDocument: begin
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.textDocumentSync) and
                Assigned(ServerCapabilities.textDocumentSync.save);
    end;
    lspDidOpenNotebookDocument,
    lspDidCloseNotebookDocument,
    lspDidChangeNotebookDocument,
    lspDidSaveNotebookDocument:
    begin
      Result := Assigned(ServerCapabilities) and Assigned(ServerCapabilities.notebookDocumentSync);
      if Result and Assigned(ServerCapabilities) and Assigned(ServerCapabilities.notebookDocumentSync) then
      begin
        if Length(ServerCapabilities.notebookDocumentSync.notebookSelector) > 0 then
        begin
          LNotebookSelector := ServerCapabilities.notebookDocumentSync.notebookSelector[0];
          Result := (LNotebookSelector.notebook <> '') or Assigned(LNotebookSelector.notebookFilter);
        end;
      end;
    end;
    lspWorkspaceSymbol: begin
      Result := Assigned(ServerCapabilities.workspaceSymbolProvider);
    end;
    lspWorkspaceExecuteCommand: begin
      Result := Assigned(ServerCapabilities.executeCommandProvider);
    end;
    lspWorkspaceWillCreateFiles: begin
      Result := Assigned(ServerCapabilities.workspace.fileOperations) and Assigned(ServerCapabilities.workspace.fileOperations.willCreate);
    end;
    lspWorkspaceWillRenameFiles: begin
      Result := Assigned(ServerCapabilities.workspace.fileOperations) and Assigned(ServerCapabilities.workspace.fileOperations.willRename);
    end;
    lspWorkspaceWillDeleteFiles: begin
      Result := Assigned(ServerCapabilities.workspace.fileOperations) and Assigned(ServerCapabilities.workspace.fileOperations.willDelete);
    end;
    lspWillSaveTextDocument: begin
      Result := Assigned(ServerCapabilities.textDocumentSync) and ServerCapabilities.textDocumentSync.willSave;
    end;
    lspWillSaveWaitUntilTextDocument: begin
      Result := Assigned(ServerCapabilities.textDocumentSync) and ServerCapabilities.textDocumentSync.willSaveWaitUntil;
    end;
    lspCompletion: begin
      Result := Assigned(ServerCapabilities.completionProvider);
    end;
    lspCompletionItemResolve: begin
      Result := Assigned(ServerCapabilities.completionProvider) and ServerCapabilities.completionProvider.resolveProvider;
    end;
    lspHover: begin
      Result := Assigned(ServerCapabilities.hoverProvider);
    end;
    lspSignatureHelp: begin
      Result := Assigned(ServerCapabilities.signatureHelpProvider);
    end;
    lspGotoDeclaration: begin
      Result := Assigned(ServerCapabilities.declarationProvider);
    end;
    lspGotoDefinition: begin
      Result := Assigned(ServerCapabilities.definitionProvider);
    end;
    lspGotoTypeDefinition: begin
      Result := Assigned(ServerCapabilities.typeDefinitionProvider);
    end;
    lspGotoImplementation: begin
      Result := Assigned(ServerCapabilities.implementationProvider);
    end;
    lspDocumentHighlight: begin
      Result := Assigned(ServerCapabilities.documentHighlightProvider);
    end;
    lspDocumentSymbol: begin
      Result := Assigned(ServerCapabilities.documentSymbolProvider);
    end;
    lspReferences: begin
      Result := Assigned(ServerCapabilities.referencesProvider);
    end;
    lspCodeAction: begin
      Result := Assigned(ServerCapabilities.codeActionProvider);
    end;
    lspCodeActionResolve: begin
      Result := Assigned(ServerCapabilities.codeActionProvider) and ServerCapabilities.codeActionProvider.resolveProvider;
    end;
    lspCodeLens: begin
      Result := Assigned(ServerCapabilities.codeLensProvider);
    end;
    lspCodeLensResolve: begin
      Result := Assigned(ServerCapabilities.codeLensProvider) and ServerCapabilities.codeLensProvider.resolveProvider;
    end;
    lspDocumentLink: begin
      Result := Assigned(ServerCapabilities.documentLinkProvider);
    end;
    lspDocumentLinkResolve: begin
      Result := Assigned(ServerCapabilities.documentLinkProvider) and ServerCapabilities.documentLinkProvider.resolveProvider;
    end;
    lspDocumentColor: begin
      Result := Assigned(ServerCapabilities.colorProvider);
    end;
    lspDocumentFormatting: begin
      Result := Assigned(ServerCapabilities.documentFormattingProvider);
    end;
    lspDocumentRangeFormatting: begin
      Result := Assigned(ServerCapabilities.documentRangeFormattingProvider);
    end;
    lspDocumentOnTypeFormatting: begin
      Result := Assigned(ServerCapabilities.documentOnTypeFormattingProvider);
    end;
    lspDocumentDiagnostic: begin
      Result := Assigned(ServerCapabilities.diagnosticProvider);
    end;
    lspWorkspaceDiagnostic: begin
      Result := Assigned(ServerCapabilities.diagnosticProvider) and ServerCapabilities.diagnosticProvider.workspaceDiagnostics;
    end;
    lspRename: begin
      Result := Assigned(ServerCapabilities.renameProvider);
    end;
    lspPrepareRename: begin
      Result := Assigned(ServerCapabilities.renameProvider) and ServerCapabilities.renameProvider.prepareProvider;
    end;
    lspFoldingRange: begin
      Result := Assigned(ServerCapabilities.foldingRangeProvider);
    end;
    lspSelectionRange: begin
      Result := Assigned(ServerCapabilities.selectionRangeProvider);
    end;
    lspPrepareCallHierarchy,
    lspCallHierarchyIncommingCalls,
    lspCallHierarchyOutgoingCalls: begin
      Result := Assigned(ServerCapabilities.callHierarchyProvider);
    end;
    lspPrepareTypeHierarchy,
    lspTypeHierarchySupertypes,
    lspTypeHierarchySubtypes: begin
      Result := Assigned(ServerCapabilities.typeHierarchyProvider);
    end;
    lspSemanticTokensFull: begin
      Result := Assigned(ServerCapabilities.semanticTokensProvider) and ServerCapabilities.semanticTokensProvider.full;
    end;
    lspSemanticTokensFullDelta: begin
      Result := Assigned(ServerCapabilities.semanticTokensProvider) and ServerCapabilities.semanticTokensProvider.full and
                ServerCapabilities.semanticTokensProvider.delta;
    end;
    lspSemanticTokensRange: begin
      Result := Assigned(ServerCapabilities.semanticTokensProvider);
    end;
    lspSemanticTokensRefresh: begin
      Result := Assigned(ServerCapabilities.semanticTokensProvider);
    end;
    lspLinkedEditingRange: begin
      Result := Assigned(ServerCapabilities.linkedEditingRangeProvider);
    end;
    lspMoniker: begin
      Result := Assigned(ServerCapabilities.monikerProvider);
    end;
    lspInlayHint,
    lspInlayHintRefresh:
    begin
      Result := Assigned(ServerCapabilities.inlayHintProvider);
    end;
    lspInlayHintResolve: begin
      Result := Assigned(ServerCapabilities.inlayHintProvider) and ServerCapabilities.inlayHintProvider.resolveProvider;
    end;
    lspInlineValue: begin
      Result := Assigned(ServerCapabilities.inlineValueProvider);
    end;
  end;
end;

function TLSPClient.LSPKindFromMethod(const s: string): TLSPKind;
begin
  Result := LSPKindFromMethod(s);
end;

procedure TLSPClient.OnCloseTimer(Sender: TObject);
begin
  FStartTimer.Enabled := False;
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
  FStartTimer.Enabled := False;
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
  FStartTimer.Enabled := False;
  FResponseTimer.Enabled := False;
  FCloseTimer.Enabled := False;
  FExitTimer.Enabled := False;
  if Assigned(FServerThread) then
  begin
    // The server doesn't seem to have responded to requests.
    // Terminate the thread.
    FRestartServer := True;
    FServerThread.Terminate;
  end;
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
  params := TLSPCancelParams.Create;
  params.id := Ord(lspKind);
  SendRequest(lspCancelRequest, '$/cancelRequest', params);
end;

procedure TLSPClient.SendCancelWorkDoneProgress(const token: string);
var
  params: TLSPWorkDoneProgressCancelParams;
begin
  params := TLSPWorkDoneProgressCancelParams.Create;
  params.token := token;
  SendRequest(lspWorkDoneProgressCancel, '', params);
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
  SendRequest(lspWorkspaceExecuteCommand,'',nil,argumentsJSON);
end;

procedure TLSPClient.SendProgressNotification(token: string; const value: TLSPWorkDoneProgressValue);
var
  params: TLSPProgressParams;
begin
  params := TLSPProgressParams.Create;
  params.token := token;
  params.value := value;
  SendRequest(lspProgress, '$/progress', params);
end;

procedure TLSPClient.SendRequest(const lspKind: TLSPKind; const method: string = ''; const params: TLSPBaseParams =
    nil; const paramJSON: string = '');
var
  msg: TLSPMessage;
  LParams: TLSPBaseParams;
  s: string;
begin
  msg := TLSPMessage.Create;
  msg.id := Ord(lspKind);

  // Start the response timer if we have a request
  if IsRequest(lspKind) and not FResponseTimer.Enabled then
    FResponseTimer.Enabled := True;

  // Create params object. If "params" should be set to void then set LParams to nil.
  LParams := params;
  case lspKind of
    lspInitialize:
    begin
      LParams := TLSPInitializeParams.Create;

      // Set options to client capabilities
      TLSPInitializeParams(LParams).processId := 0;
      TLSPInitializeParams(LParams).clientInfo.name := ClientName;
      TLSPInitializeParams(LParams).clientInfo.version := ClientVersion;
      TLSPInitializeParams(LParams).capabilities := TLSPClientCapabilities.Create;

      // Call OnInitialize event
      if Assigned(FOnInitialize) then
        FOnInitialize(Self, TLSPInitializeParams(LParams));

      // Start the startup timer
      FStartTimer.Enabled := True;
    end;

    lspInitialized:
    begin
      LParams := TLSPInitializedParams.Create;
      msg.id := 1;
    end;

    lspCallHierarchyIncommingCalls:
    begin
      s := TLSPCallHierarchyIncomingCallsParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspCallHierarchyIncommingCalls, s);
    end;

    lspCallHierarchyOutgoingCalls:
    begin
      s := TLSPCallHierarchyOutgoingCallsParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspCallHierarchyOutgoingCalls, s);
    end;

    lspColorPresentation:
    begin
      s := TLSPColorPresentationParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspColorPresentation, s);
    end;

    lspDocumentColor:
    begin
      s := TLSPDocumentColorParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspDocumentColor, s);
    end;

    lspDocumentHighlight:
    begin
      s := TLSPDocumentHighlightParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspDocumentHighlight, s);
    end;

    lspDocumentSymbol:
    begin
      s := TLSPDocumentSymbolParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspDocumentSymbol, s);
    end;

    lspFoldingRange:
    begin
      s := TLSPFoldingRangeParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspFoldingRange, s);
    end;

    lspMoniker:
    begin
      s := TLSPMonikerParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspMoniker, s);
    end;

    lspSelectionRange:
    begin
      s := TLSPSelectionRangeParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspSelectionRange, s);
    end;

    lspSemanticTokensFull:
    begin
      s := TLSPSemanticTokensParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspSemanticTokensFull, s);
    end;

    lspSemanticTokensFullDelta:
    begin
      s := TLSPSemanticTokensDeltaParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspSemanticTokensFullDelta, s);
    end;

    lspSemanticTokensRange:
    begin
      s := TLSPSemanticTokensRangeParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspSemanticTokensRange, s);
    end;

    lspWorkspaceSymbol:
    begin
      s := TLSPWorkspaceSymbolParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspWorkspaceSymbol, s);
    end;

    lspGotoDeclaration:
    begin
      s := TLSPDeclarationParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspGotoDeclaration, s);
    end;

    lspGotoDefinition:
    begin
      s := TLSPDefinitionParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspGotoDefinition, s);
    end;

    lspGotoTypeDefinition:
    begin
      s := TLSPTypeDefinitionParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspGotoTypeDefinition, s);
    end;

    lspGotoImplementation:
    begin
      s := TLSPImplmentationParams(LParams).partialResultToken;
      if s <> '' then
        RegisterPartialResultToken(lspGotoImplementation, s);
    end;
  end;
  msg.paramObj := LParams;
  s := CreateJSONRequest(lspKind, msg, method, paramJSON);

  if not FInitialized and not (lspKind in [lspInitialize, lspInitialized]) then
  begin
    // We shouldn't send anything to the server before it has been initialized.
    // Store requests in a temp string until we are connected and the server has
    // been initialized.
    AddToTempOutputString(s);
  end
  else
  begin
    // Output to server
    AddToOutputString(s);
  end;

  if Assigned(LParams) and not Assigned(params) then LParams.Free;
end;

procedure TLSPClient.SendResponse(const id: Integer; const method: string = ''; const params: TLSPBaseParams = nil;
    const errors: TLSPBaseParams = nil; resultType: TLSPResultType = lsprObject; resultString: string = '');
var
  msg: TLSPMessage;
  s: string;
  lspKind: TLSPKind;
begin
  msg := TLSPMessage.Create;
  msg.id := id;

  lspKind := TLSPKind(GetKindFromMethod(method, id));
  msg.paramObj := params;
  msg.errorObj := errors;
  s := CreateJSONResponse(lspKind, msg, method,resultType,resultString);
  AddToOutputString(s);
end;

procedure TLSPClient.SendSetTraceNotification(const traceValue: string);
var
  params: TLSPSetTraceParams;
begin
  // TraceValue = 'off' | 'message' | 'verbose'
  params := TLSPSetTraceParams.Create;
  params.value := traceValue;
  SendRequest(lspProgress, '$/setTrace', params);
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

procedure TLSPClient.SetStartTimeout(const Value: Integer);
begin
  FStartTimeout := Value;
  FStartTimer.Interval := Value;
end;

procedure TLSPClient.UnRegisterCapability(const item: TLSPUnregistration);
var
  n: Integer;
begin
  n := FDynamicCapabilities.IndexOfName(item.id);
  if n >= 0 then
    FDynamicCapabilities.Delete(n);
end;

procedure TLSPClient.UnRegisterPartialResultToken(const token: string);
var
  n: Integer;
begin
  n := FPartialTokens.IndexOfName(token);
  if n >= 0 then FPartialTokens.Delete(n);
end;

end.
