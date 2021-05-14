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

unit XLSPTypes;

interface

uses XSuperJSON, XSuperObject, generics.collections, System.Classes;

type
  TLSPBaseParams = class
  end;

  // A notification message. A processed notification message must not send a response
  // back. They work like events.
  //
  // NotificationMessage = record
  //   // The method to be invoked.
  //   method: string;
  //
  //   // The notification's params.
  //   params?: array | object;
  // end;
  //
  // $ Notifications and Requests
  // Notification and requests whose methods start with ‘$/’ are messages which are
  // protocol implementation dependent and might not be implementable in all clients or
  // servers. For example if the server implementation uses a single threaded synchronous
  // programming language then there is little a server can do to react to a ‘$/cancelRequest’
  // notification. If a server or client receives notifications starting with ‘$/’ it is free
  // to ignore the notification. If a server or client receives a requests starting with ‘$/’
  // it must error the request with error code MethodNotFound (e.g. -32601).

  // The base protocol offers support for request cancellation. To cancel a request,
  // a notification message with the following properties is sent:
  //
  // Notification:
  //
  // method: ‘$/cancelRequest’
  // params: CancelParams defined as follows:
  TLSPCancelParams = class(TLSPBaseParams)
  public
    // The request id to cancel.
    id: Integer;
  end;

  TLSPProgressToken = string;

  TLSPDocumentUri = string;

  TLSPTextDocumentSyncKindRec = record
    // Documents should not be synced at all.
    const None = 0;

    // Documents are synced by always sending the full content
    // of the document.
    const Full = 1;

    // Documents are synced by sending the full content on open.
    // After that only incremental updates to the document are
    // send.
    const Incremental = 2;
  end;
  TLSPTextDocumentSyncKind = Integer;

  TLSPWorkDoneProgressParams = class(TLSPBaseParams)
  public
     // An optional token that a server can use to report work done progress.
     workDoneToken: TLSPProgressToken;
  end;

  TLSPWorkDoneProgressCreateParams = class(TLSPBaseParams)
  public
    // The token to be used to report progress.
    token: string;
  end;

  TLSPWorkDoneProgressValue = class(TLSPBaseParams)
  public
    // 'begin, report or end'
    kind: string;

    // Mandatory title of the progress operation. Used to briefly inform about
    // the kind of operation being performed.
    //
    // Examples: "Indexing" or "Linking dependencies".
    title: string;

    // Controls if a cancel button should show to allow the user to cancel the
    // long running operation. Clients that don't support cancellation are
    // allowed to ignore the setting.
    cancellable: boolean;

    // Optional, more detailed associated progress message. Contains
    // complementary information to the `title`.
    //
    // Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
    // If unset, the previous progress message (if any) is still valid.
    [ALIAS('message')]
    msg: string;

    // Optional progress percentage to display (value 100 is considered 100%).
    // If not provided infinite progress is assumed and clients are allowed
    // to ignore the `percentage` value in subsequent in report notifications.
    //
    // The value should be steadily rising. Clients are free to ignore values
    // that are not following this rule. The value range is [0, 100]
    percentage: Cardinal;
  end;

  TLSPProgressParams = class(TLSPBaseParams)
  public
    // The progress token provided by the client or server.
    token: string;

    // The progress data.
    value: TLSPWorkDoneProgressValue;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPWorkDoneProgressOptions = class
  public
	  workDoneProgress: boolean;
  end;

  TLSPWorkDoneProgressCancelParams = class(TLSPBaseParams)
  public
    // The token to be used to report progress.
    token: string;
  end;

  TLSPWorkDoneProgressBegin = class(TLSPBaseParams)
  public
    // 'begin'
    kind: string;

    // Mandatory title of the progress operation. Used to briefly inform about
    // the kind of operation being performed.
    //
    // Examples: "Indexing" or "Linking dependencies".
    title: string;

    // Controls if a cancel button should show to allow the user to cancel the
    // long running operation. Clients that don't support cancellation are
    // allowed to ignore the setting.
    cancellable: boolean;

    // Optional, more detailed associated progress message. Contains
    // complementary information to the `title`.
    //
    // Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
    // If unset, the previous progress message (if any) is still valid.
    [ALIAS('message')]
    msg: string;

    // Optional progress percentage to display (value 100 is considered 100%).
    // If not provided infinite progress is assumed and clients are allowed
    // to ignore the `percentage` value in subsequent in report notifications.
    //
    // The value should be steadily rising. Clients are free to ignore values
    // that are not following this rule. The value range is [0, 100]
    percentage: Cardinal;
    constructor Create;
  end;

  TLSPWorkDoneProgressReport = class(TLSPBaseParams)
  protected
    constructor Create;
  public
    // 'report'
    kind: string;

    // Controls enablement state of a cancel button. This property is only valid
	  // if a cancel button got requested in the `WorkDoneProgressStart` payload.
    //
	  // Clients that don't support cancellation or don't support control the
	  // button's enablement state are allowed to ignore the setting.
    cancellable: boolean;

    // Optional, more detailed associated progress message. Contains
    // complementary information to the `title`.
    //
    // Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
    // If unset, the previous progress message (if any) is still valid.
    [ALIAS('message')]
    msg: string;

    // Optional progress percentage to display (value 100 is considered 100%).
    // If not provided infinite progress is assumed and clients are allowed
    // to ignore the `percentage` value in subsequent in report notifications.
    //
    // The value should be steadily rising. Clients are free to ignore values
    // that are not following this rule. The value range is [0, 100]
    percentage: Cardinal;
  end;

  WorkDoneProgressEnd  = class(TLSPBaseParams)
  protected
    constructor Create;
  public
    // 'end'
    kind: string;

    // Optional, a final message indicating to for example indicate the outcome
    // of the operation.
    [ALIAS('message')]
    msg: string;
  end;

  TLSPSetTraceParams = class(TLSPBaseParams)
  public
    // The new value that should be assigned to the trace setting.
    // TraceValue = 'off' | 'message' | 'verbose'
    value: string;
  end;

  TLSPLogTraceParams = class(TLSPBaseParams)
  public
    // The message to be logged.
    [ALIAS('message')]
    msg: string;

    // Additional information that can be computed if the `trace` configuration
    // is set to `'verbose'`
    verbose: string;
  end;

  TLSPMessageType = (lspMsgError=1,lspMsgWarning,lspMsgInfo,lspMsgLog);

  TLSPShowMessageParams = class(TLSPBaseParams)
  public
    // The message type.
    //
	  // const Error = 1;
    // const Warning = 2;
    // const Info = 3;
    // const Log = 4;
    [ALIAS('type')]
    msgtype: Integer;

    // The actual message.
    [ALIAS('message')]
    msg: string;
  end;

  TLSPResponseError = class(TLSPBaseParams)
  public
    // A number indicating the error type that occurred.
    code: integer;

    // A string providing a short description of the error.
    [ALIAS('message')]
    msg: string;

    // A primitive or structured value that contains additional
    // information about the error. Can be omitted.
    data: string;
  end;

  TLSPInitializedParams = class(TLSPBaseParams)
  end;

  // Position in a text document expressed as zero-based line and zero-based character
  // offset. A position is between two characters like an ‘insert’ cursor in a editor.
  // Special values like for example -1 to denote the end of a line are not supported.
  TLSPPosition = record
    // Line position in a document (zero-based).
    line: Cardinal;

    // Character offset on a line in a document (zero-based). Assuming that the
    // line is represented as a string, the `character` value represents the gap
    // between the `character` and `character + 1`.
    //
    // If the character value is greater than the line length it defaults back
    // to the line length.
    character: Cardinal;
  end;

  // A range in a text document expressed as (zero-based) start and end positions.
  // A range is comparable to a selection in an editor.
  TLSPRange = record
    // The range's start position.
    [ALIAS('start')]
    startPos: TLSPPosition;

    // The range's end position.
    [ALIAS('end')]
    endPos: TLSPPosition;
  end;

  // Represents a location inside a resource, such as a line inside a text file.
  TLSPLocation = record
    uri: TLSPDocumentUri;
    range: TLSPRange;
  end;

  TLSPLocationLink = record
    // Span of the origin of this link.
    //
    // Used as the underlined span for mouse interaction.
    // Defaults to the word range at the mouse position.
    originSelectionRange: TLSPRange;

    // The target resource identifier of this link.
    targetUri: TLSPDocumentUri;

    // The full target range of this link.
    // For example, if the target is a symbol, then target range is the range
    // enclosing this symbol not including leading/trailing whitespace but
    // everything else like comments.
    // This information is typically used to highlight the range in the editor.
    targetRange: TLSPRange;

    // The range that should be selected and revealed when this link is being
    // followed, for example, the name of a function.
    // Must be contained by the the `targetRange`.
    targetSelectionRange: TLSPRange;
  end;

  // DiagnosticSeverity := 1 | 2 | 3 | 4
  //
  // Reports an error.
  // Error = 1;
  //
  // Reports a warning.
  // Warning = 2;
  //
  // Reports an information.
  // Information = 3;
  //
  // Reports a hint.
  // Hint = 4;
  TLSPDiagnosticSeverity = Integer;

  // Represents a related message and source code location for a diagnostic.
  // This should be used to point to code locations that cause or are related
  // to a diagnostics, for example, when duplicating a symbol in a scope.
  TLSPDiagnosticRelatedInformation = record
    // The location of this related diagnostic information.
    location: TLSPLocation;

    // The message of this related diagnostic information.
    messageString: string;
  end;

  // Structure to capture a description for an error code.
  //
  // @since 3.16.0
  //
  TLSPCodeDescription = record
    // An URI to open with more information about the diagnostic error.
    href: string;
  end;

  // DiagnosticTag = 1 | 2
  TLSPDiagnosticTag = Integer;
    // Unused or unnecessary code.
    //
    // Clients are allowed to render diagnostics with this tag faded out
    // instead of having an error squiggle.
    // const Unnecessary = 1;

    // Deprecated or obsolete code.
    //
    // Clients are allowed to rendered diagnostics with this tag strike through.
    // Deprecated = 2;

  TLSPDiagnostic = record
    // The range at which the message applies.
    range: TLSPRange;

    // The diagnostic's severity. Can be omitted. If omitted it is up to the
    // client to interpret diagnostics as error, warning, info or hint.
    severity: TLSPDiagnosticSeverity;

    // The diagnostic's code, which might appear in the user interface.
    code: Variant; // integer | string

    // An optional property to describe the error code.
    //
    // @since 3.16.0
    //
    codeDescription: TLSPCodeDescription;

    // A human-readable string describing the source of this diagnostic, e.g. 'typescript' or 'super lint'.
    source: string;

    // The diagnostic's message.
    messageString: string;

    // Additional metadata about the diagnostic.
    //
    // @since 3.15.0
    tags: TArray<TLSPDiagnosticTag>;

    // An array of related diagnostic information, e.g. when symbol-names within
    // a scope collide all definitions can be marked via this property.
    relatedInformation: TArray<TLSPDiagnosticRelatedInformation>;

    // A data entry field that is preserved between a
    // `textDocument/publishDiagnostics` notification and
    // `textDocument/codeAction` request.
    //
    // @since 3.16.0
    //
    data: string;
  end;

  // Represents a reference to a command. Provides a title which will be used to
  // represent a command in the UI. Commands are identified by a string identifier.
  // The recommended way to handle commands is to implement their execution on the
  // server side if the client and server provides the corresponding capabilities.
  // Alternatively the tool extension code could handle the command. The protocol
  // currently doesn’t specify a set of well-known commands.
  TLSPCommand = record
  public
    // Title of the command, like `save`.
    title: string;

    // The identifier of the actual command handler.
    command: string;

    // Arguments that the command handler should be
    // invoked with.
    arguments: string;
  end;

  TLSPDocumentFilter = record
  public
    // A language id, like `typescript`.
    language: string;

    // A Uri [scheme](#Uri.scheme), like `file` or `untitled`.
    scheme: string;

    // A glob pattern, like `*.{ts,js}`.
    //
    // Glob patterns can have the following syntax:
    // - `*` to match one or more characters in a path segment
    // - `?` to match on one character in a path segment
    // - `**` to match any number of path segments, including none
    // - `{}` to group conditions
    //   (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
    // - `[]` to declare a range of characters to match in a path segment
    //   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
    // - `[!...]` to negate a range of characters to match in a path segment
    //   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`,
    //    but not `example.0`)
    pattern: string;
  end;

  // A set of predefined code action kinds.
  TLSPCodeActionKind = string;

  (*
   Empty kind.
   CodeActionKind = '';

   Base kind for quickfix actions: 'quickfix'.
   CodeActionKind = 'quickfix';

   Base kind for refactoring actions: 'refactor'.
   CodeActionKind = 'refactor';

   Base kind for refactoring extraction actions: 'refactor.extract'.

   Example extract actions:

   - Extract method
   - Extract function
   - Extract variable
   - Extract interface from class
   - ...

   CodeActionKind = 'refactor.extract';

   Base kind for refactoring inline actions: 'refactor.inline'.

   Example inline actions:

   - Inline function
   - Inline variable
   - Inline constant
   - ...

   CodeActionKind = 'refactor.inline';

   Base kind for refactoring rewrite actions: 'refactor.rewrite'.

   Example rewrite actions:

   - Convert JavaScript function to class
   - Add or remove parameter
   - Encapsulate field
   - Make method static
   - Move method to base class
   - ...

   CodeActionKind = 'refactor.rewrite';

   Base kind for source actions: `source`.

   Source code actions apply to the entire file.
   CodeActionKind = 'source';

   Base kind for an organize imports source action `source.organizeImports`.
   CodeActionKind = 'source.organizeImports';
  *)
  TLSPCodeActionKindValues = class
  public
    // The code action kind values the client supports. When this
    // property exists the client also guarantees that it will
    // handle values outside its set gracefully and falls back
    // to a default value when unknown.
    valueSet: TArray<TLSPCodeActionKind>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPDocumentSelector = TArray<TLSPDocumentFilter>;

  TLSPHoverOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPSignatureHelpOptions = class(TLSPWorkDoneProgressOptions)
  public
    // The characters that trigger signature help
    // automatically.
    triggerCharacters: TArray<string>;

    // List of characters that re-trigger signature help.
    //
    // These trigger characters are only active when signature help is already
    // showing. All trigger characters are also counted as re-trigger
    // characters.
    //
    // @since 3.15.0
    //
    retriggerCharacters: TArray<string>;
  end;

  TLSPDeclarationOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPDefinitionOptions = class(TLSPWorkDoneProgressOptions)
  end;

  // Options to create a file.
  TLSPCreateFileOptions = record
    // Overwrite existing file. Overwrite wins over `ignoreIfExists`
    overwrite: boolean;

    // Ignore if exists.
    ignoreIfExists: boolean;
  end;

  // Create file operation
  TLSPCreateFile = class(TLSPBaseParams)
  public
    // A create
    kind: string; // 'create'

    // The resource to create.
    uri: TLSPDocumentUri;

    // Additional options
    options: TLSPCreateFileOptions;

    // An optional annotation identifer describing the operation.
    //
    // @since 3.16.0
    //
    annotationId: string;
    constructor Create;
  end;

  // Rename file options
  TLSPRenameFileOptions = record
    // Overwrite target if existing. Overwrite wins over `ignoreIfExists`
    overwrite: boolean;

    // Ignores if target exists.
    ignoreIfExists: boolean;
  end;

  // Rename file operation
  TLSPRenameFile = class(TLSPBaseParams)
  public
    // A rename
    kind: string; // 'rename'

    // The old (existing) location.
    oldUri: TLSPDocumentUri;

    // The new location.
    newUri: TLSPDocumentUri;

    // Rename options.
    options: TLSPRenameFileOptions;

    // An optional annotation identifer describing the operation.
    //
    // @since 3.16.0
    //
    annotationId: string;
    constructor Create;
  end;

  // Delete file options
  TLSPDeleteFileOptions = record
    // Delete the content recursively if a folder is denoted.
    recursive: boolean;

    // Ignore the operation if the file doesn't exist.
    ignoreIfNotExists: boolean;
  end;

  // Delete file operation
  TLSPDeleteFile = class(TLSPBaseParams)
  public
    // A delete
    kind: string; // 'delete';

    // The file to delete.
    uri: TLSPDocumentUri;

    // Delete options.
    options: TLSPDeleteFileOptions;

    // An optional annotation identifer describing the operation.
    //
    // @since 3.16.0
    //
    annotationId: string;
    constructor Create;
  end;

  TLSPStaticRegistrationOptions = class
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;
  end;

  TLSPTextDocumentRegistrationOptions = class
  public
    // The client is supposed to include the content on save.
	  includeText: boolean;

    // How documents are synced to the server. See TextDocumentSyncKind.Full
	  // and TextDocumentSyncKind.Incremental.
	  syncKind: TLSPTextDocumentSyncKind;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPDeclarationRegistrationOptions = class(TLSPDeclarationOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPTypeDefinitionOptions = class(TLSPWorkDoneProgressOptions)
  public
    supported: Boolean;
  end;

  TLSPImplementationOptions = class(TLSPWorkDoneProgressOptions)
  public
    supported: Boolean;
  end;

  TLSPTypeDefinitionRegistrationOptions = class(TLSPTypeDefinitionOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPImplementationRegistrationOptions = class(TLSPImplementationOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPReferenceOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPDocumentHighlightOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPDocumentSymbolOptions = class(TLSPWorkDoneProgressOptions)
  public
    // A human-readable string that is shown when multiple outlines trees
    // are shown for the same document.
    //
    // @since 3.16.0 - proposed state
    //
    [ALIAS('label')]
    slabel: string;
  end;

  TLSPCodeActionOptions = class(TLSPWorkDoneProgressOptions)
  public
    // CodeActionKinds that this server may return.
    //
    // The list of kinds may be generic, such as `CodeActionKind.Refactor`,
    // or the server may list out every specific kind they provide.
    codeActionKinds: TArray<string>;

    // The server provides support to resolve additional
    // information for a code action.
    //
    // @since 3.16.0
    //
    resolveProvider: boolean;
  end;

  TLSPCodeLensOptions = class(TLSPWorkDoneProgressOptions)
  public
    // Code lens has a resolve provider as well.
    resolveProvider: boolean;
  end;

  TLSPDocumentLinkOptions = class(TLSPWorkDoneProgressOptions)
  public
    // Document links have a resolve provider as well.
    resolveProvider: boolean;
  end;

  TLSPDocumentColorOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPDocumentColorRegistrationOptions = class(TLSPDocumentColorOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPFoldingRangeOptions = class(TLSPWorkDoneProgressOptions)
 end;

  TLSPFoldingRangeRegistrationOptions = class(TLSPFoldingRangeOptions)
	public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPSelectionRangeOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPSelectionRangeRegistrationOptions = class(TLSPSelectionRangeOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPLinkedEditingRangeOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPLinkedEditingRangeRegistrationOptions = class(TLSPLinkedEditingRangeOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPCallHierarchyOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPCallHierarchyRegistrationOptions = class(TLSPCallHierarchyOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPSemanticTokensLegend = class
  public
    // The token types a server uses.
    tokenTypes: TArray<string>;

    // The token modifiers a server uses.
    tokenModifiers: TArray<string>;
  end;

  TLSPSemanticTokensOptions = class(TLSPWorkDoneProgressOptions)
  public
    // The legend used by the server
    legend: TLSPSemanticTokensLegend;

    // Server supports providing semantic tokens for a specific range
    // of a document.
    range: boolean;

    // Server supports providing semantic tokens for a full document.
    full: boolean;

    // The server supports deltas for full documents.
    // full: {
    //   delta: boolean;
    // }
    delta: boolean;
  end;

  TLSPSemanticTokensRegistrationOptions = class(TLSPSemanticTokensOptions)
  public
    // The id used to register the request. The id can be used to deregister
    // the request again. See also Registration#id.
    id: string;

    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPMonikerOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPMonikerRegistrationOptions = class(TLSPMonikerOptions)
  public
    // A document selector to identify the scope of the registration.
    // If set to null, the document selector provided on the client side
    // will be used.
    documentSelector: TLSPDocumentSelector;
  end;

  TLSPWorkspaceSymbolOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPWorkspaceSymbolRegistrationOptions = class(TLSPWorkspaceSymbolOptions)
  end;

  TLSPDocumentFormattingOptions = class(TLSPWorkDoneProgressOptions)
  end;

  TLSPDocumentRangeFormattingOptions =class(TLSPWorkDoneProgressOptions)
  end;

  TLSPDocumentOnTypeFormattingOptions = class
  public
    // A character on which formatting should be triggered, like `}`.
    firstTriggerCharacter: string;

    // More trigger characters.
    moreTriggerCharacter: TArray<string>;
  end;

  TLSPRenameOptions = class(TLSPWorkDoneProgressOptions)
  public
    // Renames should be checked and tested before being executed.
    prepareProvider: boolean;
  end;

  TLSPExecuteCommandOptions = class(TLSPWorkDoneProgressOptions)
  public
    // The commands to be executed on the server
    commands: TArray<string>;
  end;

  TLSPTextEdit = class
  public
    // The range of the text document to be manipulated. To insert
    // text into a document create a range where start === end.
    range: TLSPRange;

    // The string to be inserted. For delete operations use an
    // empty string.
    newText: string;
  end;

  TLSPAnnotatedTextEdit = class(TLSPTextEdit)
    // The actual annotation identifier.
    annotationId: string;
  end;

  // A symbol kind.
  TLSPSymbolKind = (symFile=1,symModule,symNamespace,symPackage,symClass,symMethod,symProperty,symField,
                    symConstructor,symEnum,symInterface,symFunction,symVariable,symConstant,symString,
                    symNumber,symBoolean,symArray,symObject,symKey,symNull,symEnumMember,symStruct,
                    symEvent,symOperator,symTypeParameter);

  // 'create' = Supports creating new files and folders.
  // 'rename' = Supports renaming existing files and folders.
  // 'delete' = Supports deleting existing files and folders.
  TLSPResourceOperationKind = string;

  TLSPSymbolKindValues = class
  public
    // The symbol kind values the client supports. When this
    // property exists the client also guarantees that it will
    // handle values outside its set gracefully and falls back
    // to a default value when unknown.
    //
    // If this property is not present the client only supports
    // the symbol kinds from `File` to `Array` as defined in
    // the initial version of the protocol.
    valueSet: TArray<Integer>;
  public
    destructor Destroy; override;
  end;

  TLSPSymbolTag = Integer;

  TLSPSymbolInformation = record
    // The name of this symbol.
    name: string;

    // The kind of this symbol.
    kind: TLSPSymbolKind;

    // Tags for this completion item.
    //
    // @since 3.16.0
    //
    tags: TArray<TLSPSymbolTag>;

    // Indicates if this symbol is deprecated.
    //
    // @deprecated Use tags instead
    //
    [ALIAS('deprecated')]
    isdeprecated: boolean;

    // The location of this symbol. The location's range is used by a tool
    // to reveal the location in the editor. If the symbol is selected in the
    // tool the range's start information is used to position the cursor. So
    // the range usually spans more then the actual symbol's name and does
    // normally include things like visibility modifiers.
    //
    // The range doesn't have to denote a node range in the sense of a abstract
    // syntax tree. It can therefore not be used to re-construct a hierarchy of
    // the symbols.
    location: TLSPLocation;

    // The name of the symbol containing this symbol. This information is for
    // user interface purposes (e.g. to render a qualifier in the user interface
    // if necessary). It can't be used to re-infer a hierarchy for the document
    // symbols.
    containerName: string;
  end;
  TLSPSymbolInformations = TArray<TLSPSymbolInformation>;

  // Applying the workspace change is simply aborted if one of the changes
  // provided fails.
  // All operations executed before the failing operation stay executed.
  // FailureHandlingKind = 'abort';

  // All operations are executed transactional. That means they either all
  // succeed or no changes at all are applied to the workspace.
  // FailureHandlingKind = 'transactional';

  // If the workspace edit contains only textual file changes, they are
  // executed transactionally.
  // If resource changes (create, rename or delete file) are part of the
  // change, the failure handling strategy is abort.
  // FailureHandlingKind = 'textOnlyTransactional';

  // The client tries to undo the operations already executed. But there is no
  // guarantee that this is succeeding.
  // FailureHandlingKind = 'undo';
  TLSPFailureHandlingKind = string;

  TLSPTextDocumentIdentifier = record
  public
    // The text document's URI.
    uri: TLSPDocumentUri;
  end;

  TLSPVersionedTextDocumentIdentifier = class
  public
    // The text document's URI.
    uri: TLSPDocumentUri;

    // The version number of this document.
    // If a versioned text document identifier is sent from the server to the
    // client and the file is not open in the editor (the server has not
    // received an open notification before), the server can send `null` to
    // indicate that the version is known and the content on disk is the
    // master (as specified with document content ownership).
    //
    // The version number of a document will increase after each change,
    // including undo/redo. The number doesn't need to be consecutive.
    version: Integer;
  end;

  // TextDocumentEdit
  //
  // Describes textual changes on a single text document. The text document is
  // referred to as a VersionedTextDocumentIdentifier to allow clients to check
  // the text document version before an edit is applied. A TextDocumentEdit describes
  // all changes on a version Si and after they are applied move the document to
  // version Si+1. So the creator of a TextDocumentEdit doesn’t need to sort the
  // array of edits or do any kind of ordering. However the edits must be non overlapping.
  TLSPTextDocumentEdit = class(TLSPBaseParams)
  public
    // The text document to change.
    textDocument: TLSPVersionedTextDocumentIdentifier;

    // The edits to be applied.
    edits: TArray<TLSPAnnotatedTextEdit>;
  end;

  TLSPDidChangeConfigurationClientCapabilities = class
  public
    // Did change configuration notification supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPDidChangeWatchedFilesClientCapabilities = class
  public
    // Did change watched files notification supports dynamic registration.
    // Please note that the current protocol doesn't support static
    // configuration for file changes from the server side.
    dynamicRegistration: boolean;
  end;

  // Render a completion as obsolete, usually using a strike-out.
  // const Deprec = 1;
  TLSPCompletionItemTag = Integer;

  // Can be 'plaintext' or 'markdown';
  TLSPMarkupKind = string;

  TLSPTagSupport = class
  public
    // The tags supported by the client.
    valueSet: TArray<TLSPCompletionItemTag>;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPWorkspaceSymbolClientCapabilities = class
  public
    // Symbol request supports dynamic registration.
    dynamicRegistration: boolean;

    // Specific capabilities for the `SymbolKind` in the
    // `workspace/symbol` request.
    symbolKind: TLSPSymbolKindValues;

    // The client supports tags on `SymbolInformation`.
	  // Clients supporting tags have to handle unknown tags gracefully.
    //
	  // @since 3.16.0
    //

  tagSupport: TLSPTagSupport;
  public
    destructor Destroy; override;
  end;

  TLSPchangeAnnotationSupport = class
    // Whether the client groups edits with equal labels into tree nodes,
    // for instance all edits labelled with "Changes in Strings" would
    // be a tree node.
    groupsOnLabel: boolean;
  end;

  TLSPWorkspaceEditClientCapabilities = class
  public
    // The client supports versioned document changes in `WorkspaceEdit`s
    documentChanges: boolean;

    // The resource operations the client supports. Clients should at least
    // support 'create', 'rename' and 'delete' files and folders.
    //
    // @since 3.13.0
    //
    resourceOperations: TArray<TLSPResourceOperationKind>;

    // The failure handling strategy of a client if applying the workspace edit
    // fails.
    //
    // @since 3.13.0
    //
    failureHandling: TLSPFailureHandlingKind;

    // Whether the client normalizes line endings to the client specific
    // setting.
    // If set to `true` the client will normalize line ending characters
    // in a workspace edit to the client specific new line character(s).
    //
    // @since 3.16.0
    //
    normalizesLineEndings: boolean;

    // Whether the client in general supports change annotations on text edits,
    // create file, rename file and delete file changes.
    //
    // @since 3.16.0
    //
    changeAnnotationSupport: TLSPchangeAnnotationSupport;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPChangeAnnotation = record
    // A human-readable string describing the actual change. The string
    // is rendered prominent in the user interface.
    [ALIAS('label')]
    slabel: string;

    // A flag which indicates that user confirmation is needed
    // before applying the change.
    needsConfirmation: boolean;

    // A human-readable string which is rendered less prominent in
    // the user interface.
    description: string;
  end;

  TLSPEditChanges = record
    uri: TLSPDocumentUri;
    values: TArray<TLSPTextEdit>;
  end;

  TLSPEditChangeAnnotations = record
    [DISABLE]
    id: string;
    [DISABLE]
    values: TLSPChangeAnnotation;
  end;

  TLSPWorkspaceEdit = class(TLSPBaseParams)
  public
    // Holds changes to existing resources.
    changes: TLSPEditChanges;
    (* changes?: { [uri: DocumentUri]: TextEdit[]; }; *)

    // Depending on the client capability
    // `workspace.workspaceEdit.resourceOperations` document changes are either
    // an array of `TextDocumentEdit`s to express changes to n different text
    // documents where each text document edit addresses a specific version of
    // a text document. Or it can contain above `TextDocumentEdit`s mixed with
    // create, rename and delete file / folder operations.
    //
    // Whether a client supports versioned document edits is expressed via
    // `workspace.workspaceEdit.documentChanges` client capability.
    //
    // If a client neither supports `documentChanges` nor
    // `workspace.workspaceEdit.resourceOperations` then only plain `TextEdit`s
    // using the `changes` property are supported.
    documentChanges: TArray<TLSPBaseParams>;
    (*
      TextDocumentEdit[] |
      (TextDocumentEdit | CreateFile | RenameFile | DeleteFile)[]
    *)

    // A map of change annotations that can be referenced in
    // `AnnotatedTextEdit`s or create, rename and delete file / folder
    // operations.
    //
    // Whether clients honor this property depends on the client capability
    // `workspace.changeAnnotationSupport`.
    //
    // @since 3.16.0
    //
    changeAnnotations: TLSPEditChangeAnnotations;
    (*
      {
        [id: string /* ChangeAnnotationIdentifier */]: ChangeAnnotation;
      }
    *)
  end;

  TLSPExecuteCommandClientCapabilities = class
  public
    // Execute command supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPClientInfo = class
  public
		// The name of the client as defined by the client.
    name: string;

		// The client's version as defined by the client.
    version: string;
	end;

  TLSPServerInfo = class
  public
    // The name of the server as defined by the server.
    name: string;

    // The server's version as defined by the server.
    version: string;
  end;

  TLSPMessageActionItem = class
    // Whether the client supports additional attributes which
    // are preserved and sent back to the server in the
    // request's response.
    additionalPropertiesSupport: boolean;
  end;

  TLSPShowMessageRequestClientCapabilities = class
    // Capabilities specific to the `MessageActionItem` type.
    messageActionItem: TLSPMessageActionItem;
  public
    destructor Destroy; override;
  end;

  TLSPSemanticTokensWorkspaceClientCapabilities = class
    // Whether the client implementation supports a refresh request sent from
    // the server to the client.
    //
    // Note that this event is global and will force the client to refresh all
    // semantic tokens currently shown. It should be used with absolute care
    // and is useful for situation where a server for example detect a project
    // wide change that requires such a calculation.
    refreshSupport: boolean;
  end;

  TLSPCodeLensWorkspaceClientCapabilities = class
    // Whether the client implementation supports a refresh request sent from the
    // server to the client.
    //
    // Note that this event is global and will force the client to refresh all
    // code lenses currently shown. It should be used with absolute care and is
    // useful for situation where a server for example detect a project wide
    // change that requires such a calculation.
    refreshSupport: boolean;
  end;

  TLSPFileOperations = class
    // Whether the client supports dynamic registration for file
    // requests/notifications.
    dynamicRegistration: boolean;

    // The client has support for sending didCreateFiles notifications.
    didCreate: boolean;

    // The client has support for sending willCreateFiles requests.
    willCreate: boolean;

    // The client has support for sending didRenameFiles notifications.
    didRename: boolean;

    // The client has support for sending willRenameFiles requests.
    willRename: boolean;

    // The client has support for sending didDeleteFiles notifications.
    didDelete: boolean;

    // The client has support for sending willDeleteFiles requests.
    willDelete: boolean;
  end;

  // FileOperationPatternKind = 'file' | 'folder';
  TLSPFileOperationPatternKind = string;

  TLSPFileOperationPatternOptions = record
    // The pattern should be matched ignoring casing.
	  ignoreCase: boolean;
  end;

  TLSPFileOperationPattern = record
    // The glob pattern to match. Glob patterns can have the following syntax:
    // - `*` to match one or more characters in a path segment
    // - `?` to match on one character in a path segment
    // - `**` to match any number of path segments, including none
    // - `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript
    //   and JavaScript files)
    // - `[]` to declare a range of characters to match in a path segment
    //   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
    // - `[!...]` to negate a range of characters to match in a path segment
    //   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but
    //   not `example.0`)
    glob: string;

    // Whether to match files or folders with this pattern.
    //
    // Matches both if undefined.
    matches: TLSPFileOperationPatternKind;

    // Additional options used during matching.
    options: TLSPFileOperationPatternOptions;
  end;

  TLSPFileOperationFilter = record
    // A Uri like `file` or `untitled`.
    scheme: string;

    // The actual file operation pattern.
    pattern: TLSPFileOperationPattern;
  end;

  TLSPFileOperationRegistrationOptions = class
    // The actual filters.
    filters: TArray<TLSPFileOperationFilter>;
  end;

  TLSPServerCapabilitiesFileOperations = class
    // Whether the client supports dynamic registration for file
    // requests/notifications.
    dynamicRegistration: TLSPFileOperationRegistrationOptions;

    // The client has support for sending didCreateFiles notifications.
    didCreate: TLSPFileOperationRegistrationOptions;

    // The client has support for sending willCreateFiles requests.
    willCreate: TLSPFileOperationRegistrationOptions;

    // The client has support for sending didRenameFiles notifications.
    didRename: TLSPFileOperationRegistrationOptions;

    // The client has support for sending willRenameFiles requests.
    willRename: TLSPFileOperationRegistrationOptions;

    // The client has support for sending didDeleteFiles notifications.
    didDelete: TLSPFileOperationRegistrationOptions;

    // The client has support for sending willDeleteFiles requests.
    willDelete: TLSPFileOperationRegistrationOptions;
  public
    destructor Destroy; override;
  end;

  TLSPWorkspace = class
  public
		// The client supports applying batch edits
		// to the workspace by supporting the request
		// 'workspace/applyEdit'
    applyEdit: boolean;

		// Capabilities specific to `WorkspaceEdit`s
    workspaceEdit: TLSPWorkspaceEditClientCapabilities;

		// Capabilities specific to the `workspace/didChangeConfiguration`
		// notification.
    didChangeConfiguration: TLSPDidChangeConfigurationClientCapabilities;

		// Capabilities specific to the `workspace/didChangeWatchedFiles`
		// notification.
    didChangeWatchedFiles: TLSPDidChangeWatchedFilesClientCapabilities;

		// Capabilities specific to the `workspace/symbol` request.
		symbol: TLSPWorkspaceSymbolClientCapabilities;

		// Capabilities specific to the `workspace/executeCommand` request.
    executeCommand: TLSPExecuteCommandClientCapabilities;

		// The client has support for workspace folders.
		//
		// Since 3.6.0
    workspaceFolders: boolean;

		// The client supports `workspace/configuration` requests.
		//
		// Since 3.6.0
    //
		configuration: boolean;

    // Capabilities specific to the semantic token requests scoped to the
		// workspace.
    //
		// @since 3.16.0
    //
    semanticTokens: TLSPSemanticTokensWorkspaceClientCapabilities;

		// Capabilities specific to the code lens requests scoped to the
		// workspace.
    //
		// @since 3.16.0
    //
		codeLens: TLSPCodeLensWorkspaceClientCapabilities;

		// The client has support for file requests/notifications.
    //
		// @since 3.16.0
    //
		fileOperations: TLSPFileOperations;
  public
    constructor Create;
    destructor Destroy; override;
	end;

  TLSPWorkspaceFoldersServerCapabilities = record
  public
    // The server has support for workspace folders
    supported: boolean;

    // Whether the server wants to receive workspace folder
    // change notifications.
    //
    // If a string is provided, the string is treated as an ID
    // under which the notification is registered on the client
    // side. The ID can be used to unregister for these events
    // using the `client/unregisterCapability` request.
    changeNotifications: Variant;
  end;

  TLSPWorkspaceServer = record
  public
    // The server supports workspace folder.
    //
    // @since 3.6.0
    //
    workspaceFolders: TLSPWorkspaceFoldersServerCapabilities;

    // The server is interested in file notifications/requests.
    //
		// @since 3.16.0
    //
		fileOperations: TLSPServerCapabilitiesFileOperations;
  end;

  TLSPShowDocumentClientCapabilities = class
    // The client has support for the show document
    // request.
    support: boolean;
  end;

  TLSPWindow = class
  public
    // Whether client supports handling progress notifications.
    // If set, servers are allowed to report in `workDoneProgress` property
    // in the request specific server capabilities.
    //
    // Since 3.15.0
    //
    workDoneProgress: boolean;

    // Capabilities specific to the showMessage request
    //
		// @since 3.16.0
    //
    showMessage: TLSPShowMessageRequestClientCapabilities;

		// Client capabilities for the show document request.
    //
		// @since 3.16.0
    //
    showDocument: TLSPShowDocumentClientCapabilities;
    destructor Destroy; override;
  end;

  TLSPTextDocumentSyncClientCapabilities = class
  public
    // Whether text document synchronization supports dynamic registration.
    dynamicRegistration: boolean;

    // The client supports sending will save notifications.
    willSave: boolean;

    // The client supports sending a will save request and
    // waits for a response providing text edits which will
    // be applied to the document before it is saved.
    willSaveWaitUntil: boolean;

    // The client supports did save notifications.
    didSave: boolean;
    constructor Create;
  end;

  // How whitespace and indentation is handled during completion
  // item insertion.
  //
  // @since 3.16.0
  //
  TLSPInsertTextModeType = record
    // The insertion or replace strings is taken as it is. If the
    // value is multi line the lines below the cursor will be
    // inserted using the indentation defined in the string value.
    // The client will not apply any kind of adjustments to the
    // string.
    const asIs = 1;

    // The editor adjusts leading whitespace of new lines so that
    // they match the indentation up to the cursor of the line for
    // which the item is accepted.
    //
    // Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
    // multi line completion item is indented using 2 tabs and all
    // following lines inserted will be indented using 2 tabs as well.
    const adjustIndentation = 2;
  end;
  TLSPInsertTextMode = Cardinal;

  TLSPResolveSupport = class
    // The properties that a client can resolve lazily. ('documentation' and 'details')
		properties: TArray<string>;
  end;

  TLSPInsertTextModeSupport = class
		valueSet: TArray<TLSPInsertTextMode>;
	end;

  TLSPClientCompletionItem = class
  public
    // Client supports snippets as insert text.
    //
    // A snippet can define tab stops and placeholders with `$1`, `$2`
    // and `${3:foo}`. `$0` defines the final tab stop, it defaults to
    // the end of the snippet.
    // Placeholders with equal identifiers are linked, so that typing in
    // one will update others as well.
    snippetSupport: boolean;

    // Client supports commit characters on a completion item.
    commitCharactersSupport: boolean;

    // Client supports the follow content formats for the documentation
    // property. The order describes the preferred format of the client.
    documentationFormat: TArray<string>; // E.g. ['plaintext','markdown']

    // Client supports the deprecated property on a completion item.
    deprecatedSupport: boolean;

    // Client supports the preselect property on a completion item.
    preselectSupport: boolean;

    // Client supports the tag property on a completion item.
    // Clients supporting tags have to handle unknown tags gracefully.
    // Clients especially need to preserve unknown tags when sending
    // a completion item back to the server in a resolve call.
    //
    // @since 3.15.0
    tagSupport: TLSPTagSupport;

    // Client supports insert replace edit to control different behavior if
		// a completion item is inserted in the text or should replace text.
    //
		// @since 3.16.0
    //
		insertReplaceSupport: boolean;

		// Indicates which properties a client can resolve lazily on a
		// completion item. Before version 3.16.0 only the predefined properties
		// `documentation` and `details` could be resolved lazily.
    //
		// @since 3.16.0
    //
		resolveSupport: TLSPResolveSupport;

		// The client supports the `insertTextMode` property on
		// a completion item to override the whitespace handling mode
		// as defined by the client (see `insertTextMode`).
    //
		// @since 3.16.0
    //
		insertTextModeSupport: TLSPInsertTextModeSupport;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPHoverClientCapabilities = class
  public
    // Whether hover supports dynamic registration.
    dynamicRegistration: boolean;

    // Client supports the follow content formats for the content
    // property. The order describes the preferred format of the client.
    contentFormat: TArray<TLSPMarkupKind>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPWorkspaceFolder = record
    // The associated URI for this workspace folder.
    uri: TLSPDocumentUri;

    // The name of the workspace folder. Used to refer to this
    // workspace folder in the user interface.
    name: string;
  end;
  TLSPWorkspaceFolders = TArray<TLSPWorkspaceFolder>;

  // The kind of a completion entry.
  TLSPCompletionItemKind = record
    const cText = 1;
    const cMethod = 2;
    const cFunction = 3;
    const cConstructor = 4;
    const cField = 5;
    const cVariable = 6;
    const cClass = 7;
    const cInterface = 8;
    const cModule = 9;
    const cProperty = 10;
    const cUnit = 11;
    const cValue = 12;
    const cEnum = 13;
    const cKeyword = 14;
    const cSnippet = 15;
    const cColor = 16;
    const cFile = 17;
    const cReference = 18;
    const cFolder = 19;
    const cEnumMember = 20;
    const cConstant = 21;
    const cStruct = 22;
    const cEvent = 23;
    const cOperator = 24;
    const cTypeParameter = 25;
  end;

  TLSPCompletionItemKindValues = class
  public
    // The completion item kind values the client supports. When this
    // property exists the client also guarantees that it will
    // handle values outside its set gracefully and falls back
    // to a default value when unknown.
    //
    // If this property is not present the client only supports
    // the completion items kinds from `Text` to `Reference` as defined in
    // the initial version of the protocol.
    valueSet: TArray<Integer>;
    destructor Destroy; override;
  end;

  TLSPCompletionClientCapabilities = class
  public
    // Whether completion supports dynamic registration.
    dynamicRegistration: boolean;

    // The client supports the following `CompletionItem` specific
    // capabilities.
    completionItem: TLSPClientCompletionItem;

    completionItemKind: TLSPCompletionItemKindValues;

    // The client supports to send additional context information for a
    // `textDocument/completion` request.
    contextSupport: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPClientParameterInformation = class
  public
    // The client supports processing label offsets instead of a
    // simple label string.
    //
    // @since 3.14.0
    labelOffsetSupport: boolean;
    constructor Create;
  end;

  TLSPClientSignatureInformation = class
  public
    // Client supports the follow content formats for the documentation
    // property. The order describes the preferred format of the client.
    documentationFormat: TArray<TLSPMarkupKind>;

    // Client capabilities specific to parameter information.
    parameterInformation: TLSPClientParameterInformation;

    // The client supports the `activeParameter` property on
		// `SignatureInformation` literal.
    //
		// @since 3.16.0
    //
		activeParameterSupport: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPSignatureHelpClientCapabilities = class
  public
    // Whether signature help supports dynamic registration.
    dynamicRegistration: boolean;

    // The client supports the following `SignatureInformation`
    // specific properties.
    signatureInformation: TLSPClientSignatureInformation;

    // The client supports to send additional context information for a
    // `textDocument/signatureHelp` request. A client that opts into
    // contextSupport will also support the `retriggerCharacters` on
    // `SignatureHelpOptions`.
    //
    // @since 3.15.0
    //
    contextSupport: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPDeclarationClientCapabilities = class
  public
    // Whether declaration supports dynamic registration.
    // If this is set to `true`, the client supports the new
    // `DeclarationRegistrationOptions` return value for the
    // corresponding server capability as well.
    dynamicRegistration: boolean;

    // The client supports additional metadata in the form of declaration links.
    linkSupport: boolean;
  end;

  TLSPDefinitionClientCapabilities = class
  public
    // Whether definition supports dynamic registration.
    dynamicRegistration: boolean;

    // The client supports additional metadata in the form of definition links.
    //
    // @since 3.14.0
    //
    linkSupport: boolean;
  end;

  TLSPTypeDefinitionClientCapabilities = class
  public
    // Whether implementation supports dynamic registration.
    // If this is set to `true`, the client supports the new `
    // TypeDefinitionRegistrationOptions` return value for the
    // corresponding server capability as well.
    dynamicRegistration: boolean;

    // The client supports additional metadata in the form of definition links.
    //
    // @since 3.14.0
    //
    linkSupport: boolean;
  end;

  TLSPImplementationClientCapabilities = class
  public
    // Whether implementation supports dynamic registration.
    // If this is set to `true`, the client supports the new
    // `ImplementationRegistrationOptions` return value for the
    // corresponding server capability as well.
    dynamicRegistration: boolean;

    // The client supports additional metadata in the form of definition links.
    //
    // @since 3.14.0
    //
    linkSupport: boolean;
  end;

  TLSPReferenceClientCapabilities = class
  public
    // Whether references supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPDocumentHighlightClientCapabilities = class
  public
    // Whether document highlight supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPCodeActionLiteralSupport = class
    // The code action kind is supported with the following value
    // set.
    codeActionKind: TLSPCodeActionKindValues;
  public
    destructor Destroy; override;
  end;

  TLSPCodeActionClientCapabilities = class
  public
    // Whether code action supports dynamic registration.
    dynamicRegistration: boolean;

    // The client supports code action literals as a valid
    // response of the `textDocument/codeAction` request.
    //
    // @since 3.8.0
    //
    codeActionLiteralSupport: TLSPCodeActionLiteralSupport;

    // Whether code action supports the `isPreferred` property.
    // @since 3.15.0
    //
    isPreferredSupport: boolean;

    // Whether code action supports the `disabled` property.
    //
    // @since 3.16.0
    //
    disabledSupport: boolean;

    // Whether code action supports the `data` property which is
    // preserved between a `textDocument/codeAction` and a
    // `codeAction/resolve` request.
    //
    // @since 3.16.0
    //
    dataSupport: boolean;


    // Whether the client supports resolving additional code action
    // properties via a separate `codeAction/resolve` request.
    //
    // @since 3.16.0
    //
    resolveSupport: TLSPResolveSupport;

    // Whether the client honors the change annotations in
    // text edits and resource operations returned via the
    // `CodeAction#edit` property by for example presenting
    // the workspace edit in the user interface and asking
    // for confirmation.
    //
    // @since 3.16.0
    //
    honorsChangeAnnotations: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPDocumentSymbolClientCapabilities = class
  public
    // Whether document symbol supports dynamic registration.
    dynamicRegistration: boolean;

    // Specific capabilities for the `SymbolKind` in the
    // `textDocument/documentSymbol` request.
    symbolKind: TLSPSymbolKindValues;

    // The client supports hierarchical document symbols.
    hierarchicalDocumentSymbolSupport: boolean;

    // The client supports tags on `SymbolInformation`. Tags are supported on
    // `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
    // Clients supporting tags have to handle unknown tags gracefully.
    //
    // @since 3.16.0
    //
    tagSupport: TLSPtagSupport;

    // The client supports an additional label presented in the UI when
    // registering a document symbol provider.
    //
    // @since 3.16.0
    //
    labelSupport: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPCodeLensClientCapabilities = class
  public
    // Whether CodeLens supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPDocumentLinkClientCapabilities = class
  public
    // Whether document link supports dynamic registration.
    dynamicRegistration: boolean;

    // Whether the client supports the `tooltip` property on `DocumentLink`.
    //
    // @since 3.15.0
    //
    tooltipSupport: boolean;
  end;

  TLSPDocumentColorClientCapabilities = class
  public
    // Whether document color supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPDocumentFormattingClientCapabilities = class
  public
    // Whether formatting supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPDocumentRangeFormattingClientCapabilities = class
  public
    // Whether formatting supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPDocumentOnTypeFormattingClientCapabilities = class
  public
    // Whether on type formatting supports dynamic registration.
    dynamicRegistration: boolean;
  end;

  TLSPPrepareSupportDefaultBehavior = class
    // The client's default behavior is to select the identifier
    // according the to language's syntax rule.
    Identifier: Cardinal;
  end;

  TLSPRenameClientCapabilities = class
  public
    // Whether rename supports dynamic registration.
    dynamicRegistration: boolean;

    // Client supports testing for validity of rename operations
    // before execution.
    //
    // @since version 3.12.0
    //
    prepareSupport: boolean;

    // Client supports the default behavior result
    // (`{ defaultBehavior: boolean }`).
    //
    // The value indicates the default behavior used by the
    // client.
    //
    // @since 3.16.0
    //
    prepareSupportDefaultBehavior: TLSPPrepareSupportDefaultBehavior;

    // Whether th client honors the change annotations in
    // text edits and resource operations returned via the
    // rename request's workspace edit by for example presenting
    // the workspace edit in the user interface and asking
    // for confirmation.
    //
    // @since 3.16.0
    //
    honorsChangeAnnotations: boolean;
    destructor Destroy; override;
  end;

  TLSPFoldingRangeClientCapabilities = class
  public
    // Whether the implementation supports dynamic registration for
    // folding range providers.
    // If this is set to `true`, the client supports the new
    // `FoldingRangeRegistrationOptions` return value for the corresponding
    // server capability as well.
    dynamicRegistration: boolean;

    // The maximum number of folding ranges that the client prefers to
    // receive per document.
    // The value serves as a hint, servers are free to follow the limit.
    rangeLimit: Integer;

    // If set, the client signals that it only supports folding complete lines.
    // If set, the client will ignore specified `startCharacter` and
    // `endCharacter` properties in a FoldingRange.
    lineFoldingOnly: boolean;
  public
    constructor Create;
  end;

  TLSPTagSupportValues = class
  public
    // The tags supported by the client. TLSPDiagnosticTag := 1 | 2. E.g. valueSet := [1,2];
    valueSet: TArray<TLSPDiagnosticTag>;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPPublishDiagnosticsClientCapabilities = class
  public
    // Whether the clients accepts diagnostics with related information.
    relatedInformation: boolean;

    // Client supports the tag property to provide meta data about a diagnostic.
    // Clients supporting tags have to handle unknown tags gracefully.
    //
    // @since 3.15.0
    //
    tagSupport: TLSPTagSupportValues;

    // Whether the client interprets the version property of the
    // `textDocument/publishDiagnostics` notification's parameter.
    //
    // @since 3.15.0
    //
    versionSupport: boolean;

    // Client supports a codeDescription property
    //
    // @since 3.16.0
    //
    codeDescriptionSupport: boolean;

    // Whether code action supports the `data` property which is
    // preserved between a `textDocument/publishDiagnostics` and
    // `textDocument/codeAction` request.
    //
    // @since 3.16.0
    //
    dataSupport: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPSelectionRangeClientCapabilities = class
  public
    // Whether implementation supports dynamic registration for selection
    // range providers.
    // If set to `true`, the client supports the new
    // `SelectionRangeRegistrationOptions` return value for the corresponding
    // server capability as well.
    dynamicRegistration: boolean;
  end;

  TLSPLinkedEditingRangeClientCapabilities = class
  public
    // Whether implementation supports dynamic registration.
    // If this is set to `true` the client supports the new
    // `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
    // return value for the corresponding server capability as well.
    //
    dynamicRegistration: boolean;
  end;

  TLSPCallHierarchyClientCapabilities = class
  public
    // Whether implementation supports dynamic registration. If this is set to
    // `true` the client supports the new `(TextDocumentRegistrationOptions &
    // StaticRegistrationOptions)` return value for the corresponding server
    // capability as well.
    dynamicRegistration: boolean;
  end;

  TLSPMonikerClientCapabilities = class
  public
    // Whether implementation supports dynamic registration. If this is set to
    // `true` the client supports the new `(TextDocumentRegistrationOptions &
    // StaticRegistrationOptions)` return value for the corresponding server
    // capability as well.
    dynamicRegistration: boolean;
  end;

  TLSPSemanticTokenTypes = (semtokenNone,semtokenFull,semtokenDelta,semtokenFullFalse,semtokenDeltaFalse);

  TLSPRequests = class
  public
    // The client will send the `textDocument/semanticTokens/range` request
    // if the server provides a corresponding handler.
    range: boolean;

    // The client will send the `textDocument/semanticTokens/full` request
    // if the server provides a corresponding handler.
    // full: boolean;
    {
      /**
       * The client will send the `textDocument/semanticTokens/full/delta`
       * request if the server provides a corresponding handler.
      */
      delta?: boolean
    }
    [DISABLE]
    semanticTokensType: TLSPSemanticTokenTypes;
  end;

  TLSPSemanticTokensClientCapabilities = class
  public
    // Whether implementation supports dynamic registration. If this is set to
    // `true` the client supports the new `(TextDocumentRegistrationOptions &
    // StaticRegistrationOptions)` return value for the corresponding server
    // capability as well.
    //
    dynamicRegistration: boolean;

    // Which requests the client supports and might send to the server.
    requests: TLSPRequests;

    // The token types that the client supports.
      // namespace = 'namespace',
      // type = 'type',
      // class = 'class',
      // enum = 'enum',
      // interface = 'interface',
      // struct = 'struct',
      // typeParameter = 'typeParameter',
      // parameter = 'parameter',
      // variable = 'variable',
      // property = 'property',
      // enumMember = 'enumMember',
      // event = 'event',
      // function = 'function',
      // method = 'method',
      // macro = 'macro',
      // keyword = 'keyword',
      // modifier = 'modifier',
      // comment = 'comment',
      // string = 'string',
      // number = 'number',
      // regexp = 'regexp',
      // operator = 'operator'
    tokenTypes: TArray<string>;

    // The token modifiers that the client supports.
      // declaration = 'declaration',
      // definition = 'definition',
      // readonly = 'readonly',
      // static = 'static',
      // deprecated = 'deprecated',
      // abstract = 'abstract',
      // async = 'async',
      // modification = 'modification',
      // documentation = 'documentation',
      // defaultLibrary = 'defaultLibrary'
    tokenModifiers: TArray<string>;

    // The formats the clients supports.
    formats: TArray<string>; // formats = ['relative']

    // Whether the client supports tokens that can overlap each other.
    overlappingTokenSupport: boolean;

    // Whether the client supports tokens that can span multiple lines.
    multilineTokenSupport: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  // Text document specific client capabilities.
  TLSPTextDocumentClientCapabilities = class
  public
    synchronization: TLSPTextDocumentSyncClientCapabilities;

    // Capabilities specific to the `textDocument/completion` request.
    completion: TLSPCompletionClientCapabilities;

    // Capabilities specific to the `textDocument/hover` request.
    hover: TLSPHoverClientCapabilities;

    // Capabilities specific to the `textDocument/signatureHelp` request.
    signatureHelp: TLSPSignatureHelpClientCapabilities;

    // Capabilities specific to the `textDocument/declaration` request.
    //
    // @since 3.14.0
    //
    declaration: TLSPDeclarationClientCapabilities;

    // Capabilities specific to the `textDocument/definition` request.
    definition: TLSPDefinitionClientCapabilities;

    // Capabilities specific to the `textDocument/typeDefinition` request.
    //
    // @since 3.6.0
    //
    typeDefinition: TLSPTypeDefinitionClientCapabilities;

    // Capabilities specific to the `textDocument/implementation` request.
    //
    // @since 3.6.0
    //
    [ALIAS('implementation')]
    fimplementation: TLSPImplementationClientCapabilities;

    // Capabilities specific to the `textDocument/references` request.
    references: TLSPReferenceClientCapabilities;

    // Capabilities specific to the `textDocument/documentHighlight` request.
    documentHighlight: TLSPDocumentHighlightClientCapabilities;

    // Capabilities specific to the `textDocument/documentSymbol` request.
    documentSymbol: TLSPDocumentSymbolClientCapabilities;

    // Capabilities specific to the `textDocument/codeAction` request.
    codeAction: TLSPCodeActionClientCapabilities;

    // Capabilities specific to the `textDocument/codeLens` request.
    codeLens: TLSPCodeLensClientCapabilities;

    // Capabilities specific to the `textDocument/documentLink` request.
    documentLink: TLSPDocumentLinkClientCapabilities;

    // Capabilities specific to the `textDocument/documentColor` and the
    // `textDocument/colorPresentation` request.
    //
    // @since 3.6.0
    //
    colorProvider: TLSPDocumentColorClientCapabilities;

    // Capabilities specific to the `textDocument/formatting` request.
    formatting: TLSPDocumentFormattingClientCapabilities;

    // Capabilities specific to the `textDocument/rangeFormatting` request.
    rangeFormatting: TLSPDocumentRangeFormattingClientCapabilities;

    // Capabilities specific to the `textDocument/onTypeFormatting` request.
    onTypeFormatting: TLSPDocumentOnTypeFormattingClientCapabilities;

    // Capabilities specific to the `textDocument/rename` request.
    rename: TLSPRenameClientCapabilities;

    // Capabilities specific to the `textDocument/publishDiagnostics`
    // notification.
    publishDiagnostics: TLSPPublishDiagnosticsClientCapabilities;

    // Capabilities specific to the `textDocument/foldingRange` request.
    //
    // @since 3.10.0
    //
    foldingRange: TLSPFoldingRangeClientCapabilities;

    // Capabilities specific to the `textDocument/selectionRange` request.
    //
    // @since 3.15.0
    //
    selectionRange: TLSPSelectionRangeClientCapabilities;

    // Capabilities specific to the `textDocument/linkedEditingRange` request.
    //
    // @since 3.16.0
    //
    linkedEditingRange: TLSPLinkedEditingRangeClientCapabilities;

    // Capabilities specific to the various call hierarchy requests.
    //
    // @since 3.16.0
    //
    callHierarchy: TLSPCallHierarchyClientCapabilities;

    // Capabilities specific to the various semantic token requests.
    //
    // @since 3.16.0
    //
    semanticTokens: TLSPSemanticTokensClientCapabilities;

    // Capabilities specific to the `textDocument/moniker` request.
    //
    // @since 3.16.0
    //
    moniker: TLSPMonikerClientCapabilities;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPRegularExpressionsClientCapabilities = class
    // The engine's name.
    engine: string;

    // The engine's version.
    version: string;
  end;

  TLSPMarkdownClientCapabilities = class
    // The name of the parser.
    parser: string;

    // The version of the parser.
    version: string;
  end;

  TLSPGeneralClientCapabilities = class
		// Client capabilities specific to regular expressions.
    //
		// @since 3.16.0
    //
		regularExpressions: TLSPRegularExpressionsClientCapabilities;

		// Client capabilities specific to the client's markdown parser.
    //
		// @since 3.16.0
    //
		markdown: TLSPMarkdownClientCapabilities;
  public
    destructor Destroy; override;
	end;

  TLSPClientCapabilities = class
  public
	  // Workspace specific client capabilities.
    workspace: TLSPWorkspace;

    // Text document specific client capabilities.
    textDocument: TLSPTextDocumentClientCapabilities;

    // Window specific client capabilities.
    window: TLSPWindow;

    // General client capabilities.
    //
	  // @since 3.16.0
    //
    general: TLSPGeneralClientCapabilities;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCallHierarchySupport(const dynamicRegistration: Boolean);
    procedure AddCodeActionSupport(const dynamicRegistration, isPreferredSupport, disabledSupport, dataSupport,
        honorsChangeAnnotations: Boolean; const codeActions: TArray<String> = nil; const resolveProperties: TArray<String>
        = nil);
    procedure AddCodeLensSupport(const dynamicRegistration: Boolean);
    procedure AddColorProvider(const dynamicRegistration: Boolean);
    procedure AddCompletionSupport(const dynamicRegistration, snippetSupport, commitCharactersSupport,
        documentationFormatPlainText, documentationFormatMarkdown, deprecatedSupport, preselectSupport,
        insertReplaceSupport, contextSupport: Boolean; const completionItemKindValues: TArray<Integer> = nil; const
        resolveSuppert: TArray<String> = nil; const insertTextModeSupportValues: TArray<TLSPInsertTextMode> = nil; const
        tagSupport: TLSPtagSupportValues = nil);
    procedure AddDeclarationSupport(const bLinkSupport: Boolean = False);
    procedure AddDefinitionSupport(const bLinkSupport: Boolean = False);
    procedure AddDocumentHighlightsSupport(const dynamicRegistration: Boolean);
    procedure AddDocumentLinkSupport(const dynamicRegistration, toolTipSupport: Boolean);
    procedure AddDocumentSymbolSupport(const hierarchicalSymbolSupport, dynamicRegistration, labelSupport: Boolean; const
        values: TArray<Integer> = nil; const tagSupport: TLSPtagSupport = nil);
    procedure AddFoldingRangeSupport(const dynamicRegistration: Boolean; const rangeLimit: Integer; const lineFoldingOnly:
        Boolean);
    procedure AddFormattingSupport(const dynamicRegistration: Boolean);
    procedure AddGeneralMarkdown(const parser, version: string);
    procedure AddGeneralRegularExpressions(const engine, version: string);
    procedure AddHoverSupport(const dynamicRegistration, contentFormatPlainText, contentFormatMarkdown: Boolean);
    procedure AddImplementationSupport(const bLinkSupport: Boolean = False);
    procedure AddLinkedEditingRangeSupport(const dynamicRegistration: Boolean);
    procedure AddMonikerSupport(const dynamicRegistration: Boolean);
    procedure AddOnTypeFormattingSupport(const dynamicRegistration: Boolean);
    procedure AddPublishDiagnosticsSupport(const relatedInformation, codeDescriptionSupport, versionSupport, dataSupport:
        Boolean; const tagSupport: TLSPtagSupportValues = nil);
    procedure AddRangeFormattingSupport(const dynamicRegistration: Boolean);
    procedure AddReferencesSupport(const dynamicRegistration: Boolean);
    procedure AddRenameSupport(const dynamicRegistration, prepareSupport, honorsChangeAnnotations: Boolean; const
        prepareSupportDefaultBehaviorId: Integer = 0);
    procedure AddSelectionRangeSupport(const dynamicRegistration: Boolean);
    procedure AddSemanticTokensSupport(const dynamicRegistration, overlappingTokenSupport, multilineTokenSupport, range:
        Boolean; const semanticToken: TLSPSemanticTokenTypes; const tokenTypes: TArray<String> = nil; const tokenModifiers:
        TArray<String> = nil; const formats: TArray<String> = nil);
    procedure AddSignatureHelpSupport(const dynamicRegistration, contentFormatPlainText, contentFormatMarkdown,
        contextSupport, labelOffsetSupport: Boolean);
    procedure AddSynchronizationSupport(const didSave, willSave, willSaveWaitUntil, dynamicRegistration: Boolean);
    procedure AddTypeDefinitionSupport(const bLinkSupport: Boolean = False);
    procedure AddWindowShowDocument;
    procedure AddWindowShowMessage(const additionalPropertiesSupport: Boolean);
    procedure AddWindowWorkDoneProgress(const value: Boolean = True);
    procedure AddWorkspaceCapabilities(const applyEdit, workspaceFolders, configuration: Boolean);
    procedure AddWorkspaceCodeLens(const refreshSupport: Boolean);
    procedure AddWorkspaceDidChangeConfiguration(const dynamicRegistration: Boolean);
    procedure AddWorkspaceDidChangeWatchedFiles(const dynamicRegistration: Boolean);
    procedure AddWorkspaceEdit(const documentChanges, normalizeLineEndings: Boolean; const failureHandling: string = '';
        const changeAnnotationSupport: Boolean = False; const changeAnnotationGroupsOnLabel: Boolean = False; const
        resourceOperations: TArray<TLSPResourceOperationKind> = nil);
    procedure AddWorkspaceExecuteCommand(const dynamicRegistration: Boolean);
    procedure AddWorkspaceFileOperations(const dynamicRegistration, didCreate, willCreate, didRename, willRename,
        didDelete, willDelete: Boolean);
    procedure AddWorkspaceSemanticTokens(const refreshSupport: Boolean);
    procedure AddWorkspaceSymbol(const dynamicRegistration: Boolean; const symbolKindValues: TArray<Integer> = nil; const
        tagSupportValues: TArray<TLSPCompletionItemTag> = nil);
  end;

  TLSPInitializeParams = class(TLSPBaseParams)
  public
	  // The process ID of the parent process that started the server.
	  // Is null if the process has not been started by another process.
	  // If the parent process is not alive, then the server should exit
	  // (see exit notification) its process.
	  processId: Variant; // integer | null

	  // Information about the client
    //
	  // @since 3.15.0
	  //
	  clientInfo: TLSPClientInfo;

    // The locale the client is currently showing the user interface
    // in. This must not necessarily be the locale of the operating
    // system.
    //
    // Uses IETF language tags as the value's syntax
    // (See https://en.wikipedia.org/wiki/IETF_language_tag)
    //
    // @since 3.16.0
    //
    locale: string;

	  // The rootPath of the workspace. Is null
	  // if no folder is open.
    //
	  // @deprecated in favour of rootUri.
    //
	  rootPath: string;

	  // The rootUri of the workspace. Is null if no
	  // folder is open. If both `rootPath` and `rootUri` are set
	  // `rootUri` wins.
	  rootUri: TLSPDocumentUri;

	  // User provided initialization options.
	  initializationOptions: ISuperObject;

	  // The capabilities provided by the client (editor or tool)
    capabilities: TLSPClientCapabilities;

	  // The initial trace setting. If omitted trace is disabled ('off').
	  trace: string; //'off' | 'messages' | 'verbose';

	  // The workspace folders configured in the client when the server starts.
	  // This property is only available if the client supports workspace folders.
	  // It can be `null` if the client supports workspace folders but none are
	  // configured.
	  //
	  // @since 3.6.0
    //
    [REVAL(roEmptyArrayToNull)]
	  workspaceFolders: TArray<TLSPWorkspaceFolder>; //WorkspaceFolder[] | null;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRoot(const sz: string);
    procedure AddWorkspaceFolders(const ls: TStringList);
  end;

  TLSPSaveOption = class
  public
    // The client is supposed to include the content on save.
    includeText: Boolean;
  end;

  TLSPTextDocumentSyncOptions = class
  public
    // Open and close notifications are sent to the server.
    // If omitted open close notification should not be sent.
    openClose: boolean;

    // Change notifications are sent to the server.
    // See TextDocumentSyncKind.None, TextDocumentSyncKind.Full,
    // and TextDocumentSyncKind.Incremental.
    // If omitted, it defaults to TextDocumentSyncKind.None.
    change: TLSPTextDocumentSyncKind;
  end;

  TLSPTextDocumentServerSyncOptions = class
  public
    // Open and close notifications are sent to the server. If omitted open
    // close notification should not be sent.
    openClose: boolean;

    // Change notifications are sent to the server. See
    // TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
    // TextDocumentSyncKind.Incremental. If omitted it defaults to
    // TextDocumentSyncKind.None.
    change: TLSPTextDocumentSyncKind;

    // If present will save notifications are sent to the server. If omitted
    // the notification should not be sent.
    willSave: boolean;

    // If present will save wait until requests are sent to the server. If
    // omitted the request should not be sent.
    willSaveWaitUntil: boolean;

    // If present save notifications are sent to the server. If omitted the
    // notification should not be sent.
    save: TLSPSaveOption;
  end;

  // Completion options.
  TLSPCompletionOptions = class
    // Most tools trigger completion request automatically without explicitly
    // requesting it using a keyboard shortcut (for example Ctrl+Space).
    // Typically they do so when the user starts to type an identifier.
    // For example, if the user types `c` in a JavaScript file, code complete
    // will automatically display `console` along with others as a
    // completion item.
    // Characters that make up identifiers don't need to be listed here.
    //
    // If code complete should automatically be triggered on characters
    // not being valid inside an identifier (for example `.` in JavaScript),
    // list them in `triggerCharacters`.
    triggerCharacters: TArray<string>;

    // The list of all possible characters that commit a completion.
    // This field can be used if clients don't support individual commit
    // characters per completion item. See `ClientCapabilities.`
    // `textDocument.completion.completionItem.commitCharactersSupport`.
    //
    // If a server provides both `allCommitCharacters` and commit characters
    // on an individual completion item, the ones on the completion item win.
    //
    // @since 3.2.0
    allCommitCharacters: TArray<string>;

    // The server provides support to resolve additional
    // information for a completion item.
    resolveProvider: boolean;

    workDoneProgress: boolean;
  end;

  TLSPServerCapabilities = class(TLSPBaseParams)
  public
    // Defines how text documents are synced.
    // Is either a detailed structure defining each notification
    // or for backwards compatibility, the TextDocumentSyncKind number.
    // If omitted, it defaults to `TextDocumentSyncKind.None`.
    textDocumentSync: TLSPTextDocumentServerSyncOptions; { textDocumentSync = TLSPTextDocumentServerSyncOptions | TLSPTextDocumentSyncKind }

    // The server provides completion support.
    completionProvider: TLSPCompletionOptions;

    // The server provides hover support.
    hoverProvider: TLSPHoverOptions; { hoverProvider = TLSPHoverOptions | Boolean }

    // The server provides signature help support.
    signatureHelpProvider: TLSPSignatureHelpOptions;

    // The server provides go to declaration support.
    //
    // @since 3.14.0
    //
    declarationProvider: TLSPDeclarationRegistrationOptions; { declarationProvider = TLSPDeclarationRegistrationOptions | TLSPDeclarationOptions | Boolean }

    // The server provides goto definition support.
    definitionProvider: TLSPDefinitionOptions; { definitionProvider = TLSPDefinitionOptions | Boolean }

    // The server provides goto type definition support.
    //
    // @since 3.6.0
    //
    typeDefinitionProvider: TLSPTypeDefinitionRegistrationOptions; { typeDefinitionProvider = boolean | TLSPTypeDefinitionOptions | TLSPTypeDefinitionRegistrationOptions }

    // The server provides goto implementation support.
    //
    // @since 3.6.0
    //
    implementationProvider: TLSPImplementationRegistrationOptions; { implementationProvider = boolean | TLSPImplementationOptions | TLSPImplementationRegistrationOptions }

    // The server provides find references support.
    //
    referencesProvider: TLSPReferenceOptions; { referencesProvider = boolean | TLSPReferenceOptions }

    // The server provides document highlight support.
    documentHighlightProvider: TLSPDocumentHighlightOptions; { documentHighlightProvider = boolean | TLSPDocumentHighlightOptions }

    // The server provides document symbol support.
    documentSymbolProvider: TLSPDocumentSymbolOptions; { documentSymbolProvider = boolean | TLSPDocumentSymbolOptions }

    // The server provides code actions.
    // The `CodeActionOptions` return type is only valid if the client signals
    // code action literal support via the property
    // `textDocument.codeAction.codeActionLiteralSupport`.
    codeActionProvider: TLSPCodeActionOptions; { codeActionProvider = boolean | TLSPCodeActionOptions }

    // The server provides CodeLens.
    codeLensProvider: TLSPCodeLensOptions;

    // The server provides document link support.
    documentLinkProvider: TLSPDocumentLinkOptions;

    // The server provides color provider support.
    //
    // @since 3.6.0
    //
    colorProvider: TLSPDocumentColorRegistrationOptions; { colorProvider = boolean | TLSPDocumentColorOptions | TLSPDocumentColorRegistrationOptions }

    // The server provides document formatting.
    documentFormattingProvider: TLSPDocumentFormattingOptions; { documentFormattingProvider = boolean | TLSPDocumentFormattingOptions }

    // The server provides document range formatting.
    documentRangeFormattingProvider: TLSPDocumentRangeFormattingOptions; { documentRangeFormattingProvider = boolean | TLSPDocumentRangeFormattingOptions }

    // The server provides document formatting on typing.
    documentOnTypeFormattingProvider: TLSPDocumentOnTypeFormattingOptions;

    // The server provides rename support. RenameOptions may only be
    // specified if the client states that it supports
    // `prepareSupport` in its initial `initialize` request.
    renameProvider: TLSPRenameOptions; { renameProvider = boolean | TLSPRenameOptions }

    // The server provides folding provider support.
    //
    // @since 3.10.0
    //
    foldingRangeProvider: TLSPFoldingRangeRegistrationOptions; { foldingRangeProvider = boolean | TLSPFoldingRangeOptions | TLSPFoldingRangeRegistrationOptions }

    // The server provides execute command support.
    executeCommandProvider: TLSPExecuteCommandOptions;

    // The server provides selection range support.
    //
    // @since 3.15.0
    //
    selectionRangeProvider: TLSPSelectionRangeRegistrationOptions; { selectionRangeProvider = boolean | TLSPSelectionRangeOptions | TLSPSelectionRangeRegistrationOptions }

    // The server provides workspace symbol support.
    workspaceSymbolProvider: TLSPWorkspaceSymbolOptions; { workspaceSymbolProvider = boolean | TLSPWorkspaceSymbolOptions }

    // The server provides linked editing range support.
    //
    // @since 3.16.0
    //
    linkedEditingRangeProvider: TLSPLinkedEditingRangeRegistrationOptions; { linkedEditingRangeProvider = boolean | TLSPLinkedEditingRangeOptions | LinkedEditingRangeRegistrationOptions }

    // The server provides call hierarchy support.
    //
    // @since 3.16.0
    //
    callHierarchyProvider: TLSPCallHierarchyRegistrationOptions; { callHierarchyProvider = boolean | TLSPCallHierarchyOptions | TLSPCallHierarchyRegistrationOptions }

    // The server provides semantic tokens support.
    //
    // @since 3.16.0
    //
    semanticTokensProvider: TLSPSemanticTokensRegistrationOptions; { semanticTokensProvider = TLSPSemanticTokensOptions | TLSPSemanticTokensRegistrationOptions }

    // Whether server provides moniker support.
    //
    // @since 3.16.0
    //
    monikerProvider: TLSPMonikerRegistrationOptions; { monikerProvider = boolean | TLSPMonikerOptions | TLSPMonikerRegistrationOptions }

    // Workspace specific server capabilities
    workspace: TLSPWorkspaceServer;

    // Experimental server capabilities.
    //
    [ALIAS('experimental')]
    experimentalValue: Variant;
  end;

  TLSPInitializeResultParams = class(TLSPBaseParams)
  public
    // The capabilities the language server provides.
    capabilities: TLSPServerCapabilities;

    // Information about the server.
    //
    // @since 3.15.0
    //
    serverInfo: TLSPServerInfo;
  end;

  TLSPShowMessageRequestResponse = class(TLSPBaseParams)
  public
    // Selected action
    title: string;
  end;

  TLSPShowDocumentParams = class(TLSPBaseParams)
  protected
  public
    // The document uri to show.
    uri: TLSPDocumentUri;

    // Indicates to show the resource in an external program.
    // To show for example `https://code.visualstudio.com/`
    // in the default WEB browser set `external` to `true`.
    [ALIAS('external')]
    inexternal: boolean;

    // An optional property to indicate whether the editor
    // showing the document should take focus or not.
    // Clients might ignore this property if an external
    // program is started.
    takeFocus: boolean;

    // An optional selection range if the document is a text
    // document. Clients might ignore the property if an
    // external program is started or the file is not a text
    // file.
    selection: TLSPRange;
  end;

  TLSPShowDocumentResult = class(TLSPBaseParams)
  public
    // A boolean indicating if the show was successful.
    success: boolean;
  end;

  TLSPLogMessageParams = class(TLSPBaseParams)
  public
    // The message type.
    [ALIAS('type')]
    typ: TLSPMessageType;

    // The actual message
    [ALIAS('message')]
    msg: string;
  end;

  TLSPRegistration = record
    // The id used to register the request. The id can be used to deregister
    // the request again.
    id: string;

    // The method / capability to register for.
    method: string;

    // Options necessary for the registration.
    registerOptions: TLSPTextDocumentRegistrationOptions;
  end;
  TLSPRegistrations = TArray<TLSPRegistration>;

  TLSPUnregistration = record
    // The id used to unregister the request or notification. Usually an id
    // provided during the register request.
    id: string;

    // The method / capability to unregister for.
    method: string;
  end;
  TLSPUnregistrations = TArray<TLSPUnregistration>;

  TLSPWorkspaceFoldersChangeEvent = record
    // The array of added workspace folders
    added: TLSPWorkspaceFolders;

    // The array of the removed workspace folders
    removed: TLSPWorkspaceFolders;
  end;

  TLSPDidChangeWorkspaceFoldersParams = class(TLSPBaseParams)
  public
    // The actual workspace folder change event.
    event: TLSPWorkspaceFoldersChangeEvent;
  end;

  TLSPDidChangeConfigurationParams = class(TLSPBaseParams)
  public
    // The actual changed settings. Can be anything, so to use this you'll have to
    // create your own Json string.
    settings: string;
  end;

  TLSPConfigurationItem = record
  public
    // The scope to get the configuration section for.
    scopeUri: string;

    // The configuration section asked for.
    section: string;
  end;
  TLSPConfigurationParams = TArray<TLSPConfigurationItem>;

  TLSPFileEvent = record
    // The file's URI.
    uri: TLSPDocumentUri;

    // The change type. 1=created, 2=changed, 3=deleted
    [ALIAS('type')]
    typ: Cardinal;
  end;
  TLSPFileEvents = TArray<TLSPFileEvent>;

  TLSPDidChangeWatchedFilesParams = class(TLSPBaseParams)
  public
    // The actual file events.
    changes: TLSPFileEvents;
  end;

  TLSPWorkspaceSymbolParams = class(TLSPBaseParams)
  public
    // A query string to filter symbols by. Clients may send an empty
    // string here to request all symbols.
    query: string;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
    partialResultToken: TLSPProgressToken;
  end;

  TLSPWorkspaceSymbolInformationParam = class(TLSPBaseParams)
  public
    values: TLSPSymbolInformations;
  end;

  TLSPExecuteCommandParams = class(TLSPWorkDoneProgressParams)
  public
    // The identifier of the actual command handler.
    command: string;

    // Arguments that the command should be invoked with.
    arguments: string;
  end;

  TLSPApplyWorkspaceEditParams = class(TLSPBaseParams)
  public
    // An optional label of the workspace edit. This label is
    // presented in the user interface for example on an undo
    // stack to undo the workspace edit.
    [ALIAS('label')]
    slabel: string;

    // The edits to apply.
    edit: TLSPWorkspaceEdit;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPApplyWorkspaceEditResponse = class(TLSPBaseParams)
  public
    // Indicates whether the edit was applied or not.
    applied: boolean;

    // An optional textual description for why the edit was not applied.
    // This may be used by the server for diagnostic logging or to provide
    // a suitable error for a request that triggered the edit.
    failureReason: string;

    // Depending on the client's failure handling strategy `failedChange`
    // might contain the index of the change that failed. This property is
    // only available if the client signals a `failureHandlingStrategy`
    // in its client capabilities.
    failedChange: Cardinal;
  end;

  // Represents information on a file/folder create.
  //
  // @since 3.16.0
  //
  TLSPFileCreate = record
    // A file:// URI for the location of the file/folder being created.
    uri: TLSPDocumentUri;
  end;

  // The parameters sent in notifications/requests for user-initiated creation
  // of files.
  //
  // @since 3.16.0
  //
  TLSPCreateFilesParams = class(TLSPBaseParams)
  public
    // An array of all files/folders created in this operation.
    files: TArray<TLSPFileCreate>;
  end;

  // Represents information on a file/folder rename.
  //
  // @since 3.16.0
  //
  TLSPFileRename = record
    // A file:// URI for the original location of the file/folder being renamed.
    oldUri: TLSPDocumentUri;

    // A file:// URI for the new location of the file/folder being renamed.
    newUri: TLSPDocumentUri;
  end;

  // The parameters sent in notifications/requests for user-initiated renames
  // of files.
  //
  // @since 3.16.0
  //
  TLSPRenameFilesParams = class(TLSPBaseParams)
  public
    // An array of all files/folders renamed in this operation. When a folder
    // is renamed, only the folder will be included, and not its children.
    files: TArray<TLSPFileRename>;
  end;

  // Represents information on a file/folder delete.
  //
  // @since 3.16.0
  //
  TLSPFileDelete = record
    // A file:// URI for the location of the file/folder being deleted.
    uri: TLSPDocumentUri;
  end;

  // The parameters sent in notifications/requests for user-initiated deletes
  // of files.
  //
  // @since 3.16.0
  //
  TLSPDeleteFilesParams = class(TLSPBaseParams)
  public
    // An array of all files/folders deleted in this operation.
    files: TArray<TLSPFileDelete>;
  end;

  TLSPTextDocumentItem = record
    // The text document's URI.
    uri: TLSPDocumentUri;

    // The text document's language identifier.
    languageId: string;

    // The version number of this document (it will increase after each
    // change, including undo/redo).
    version: integer;

    // The content of the opened text document.
    text: string;
  end;

  TLSPTextDocumentItemParam = class(TLSPBaseParams)
    // The text document's URI.
    uri: TLSPDocumentUri;

    // The text document's language identifier.
    languageId: string;

    // The version number of this document (it will increase after each
    // change, including undo/redo).
    version: integer;

    // The content of the opened text document.
    text: string;
  end;

  // The document open notification is sent from the client to the server to signal
  // newly opened text documents. The document’s content is now managed by the client
  // and the server must not try to read the document’s content using the document’s
  // Uri. Open in this sense means it is managed by the client.
  TLSPDidOpenTextDocumentParams = class(TLSPBaseParams)
  public
    // The document that was opened.
    textDocument: TLSPTextDocumentItem;
  end;

  // An event describing a change to a text document.
  TLSPBaseTextDocumentContentChangeEvent = class
    // The new text for the provided range.
    text: string;
  end;

  // An event describing a change to a text document.
  TLSPTextDocumentContentChangeEvent = class(TLSPBaseTextDocumentContentChangeEvent)
    // The range of the document that changed.
    range: TLSPRange;

    // The optional length of the range that got replaced.
    //
    // @deprecated use range instead.
    //
    [DISABLE]
    rangeLength: Cardinal;
  end;

  // The document change notification is sent from the client to the server to
  // signal changes to a text document. Before a client can change a text document
  // it must claim ownership of its content using the textDocument/didOpen notification.
  TLSPDidChangeTextDocumentParams = class(TLSPBaseParams)
  public
    // The document that did change. The version number points
    // to the version after all provided content changes have
    // been applied.
    textDocument: TLSPVersionedTextDocumentIdentifier;

    // The actual content changes. The content changes describe single state
    // changes to the document. So if there are two content changes c1 (at
    // array index 0) and c2 (at array index 1) for a document in state S then
    // c1 moves the document from S to S' and c2 from S' to S''. So c1 is
    // computed on the state S and c2 is computed on the state S'.
    //
    // To mirror the content of a document using change events use the following
    // approach:
    // - start with the same initial content
    // - apply the 'textDocument/didChange' notifications in the order you
    //   receive them.
    // - apply the `TextDocumentContentChangeEvent`s in a single notification
    //   in the order you receive them.
    contentChanges: TObjectList<TLSPBaseTextDocumentContentChangeEvent>;
    constructor Create;
    destructor Destroy; override;
  end;

  // The parameters send in a will save text document notification.
  TLSPWillSaveTextDocumentParams = class(TLSPBaseParams)
  public
    // The document that will be saved.
    textDocument: TLSPTextDocumentIdentifier;

    // The 'TextDocumentSaveReason'. Can take the values 1, 2 or 3.
    //
    // Manually triggered, e.g. by the user pressing save, by starting
    // debugging, or by an API call.
    // 1 = Manual
    //
    // Automatic after a delay.
    // 2 = AfterDelay
    //
    // When the editor lost focus.
    // 3 = FocusOut
    //
    reason: Cardinal;
  end;

  // The document save notification is sent from the client to the server when
  // the document was saved in the client.
  TLSPDidSaveTextDocumentParams = class(TLSPBaseParams)
  public
    // The document that was saved.
    textDocument: TLSPTextDocumentIdentifier;

    // Optional the content when saved. Depends on the includeText value
    // when the save notification was requested.
    text: string;
  end;

  // The document close notification is sent from the client to the server when
  // the document got closed in the client. The document’s master now exists where
  // the document’s Uri points to (e.g. if the document’s Uri is a file Uri the
  // master now exists on disk)
  TLSPDidCloseTextDocumentParams = class(TLSPBaseParams)
  public
    // The document that was closed.
    textDocument: TLSPTextDocumentIdentifier;
  end;

  // Diagnostics notification are sent from the server to the client to signal
  // results of validation runs.
  TLSPPublishDiagnosticsParams = class(TLSPBaseParams)
  public
    // The URI for which diagnostic information is reported.
    uri: TLSPDocumentUri;

    // Optional the version number of the document the diagnostics are published
    // for.
    //
    // @since 3.15.0
    //
    version: Cardinal;

    // An array of diagnostic information items.
    diagnostics: TArray<TLSPDiagnostic>;
  end;

  TLSPTextDocumentPositionParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // The position inside the text document.
    position: TLSPPosition;
  end;

    TLSPMarkupContent = record
    // The type of the Markup. MarkupKind = 'plaintext' | 'markdown';
    kind: TLSPMarkupKind;

    // The content itself
    value: string;
  end;

  // Completion was triggered by typing an identifier (24x7 code
	// complete), manual invocation (e.g Ctrl+Space) or via API.
  //
	// const Invoked: 1 = 1;
  //
	// Completion was triggered by a trigger character specified by
	// the `triggerCharacters` properties of the
	// `CompletionRegistrationOptions`.
  //
	// const TriggerCharacter: 2 = 2;
  //
	// Completion was re-triggered as the current completion list is incomplete.
  //
  // const TriggerForIncompleteCompletions: 3 = 3;
  //
  TLSPCompletionTriggerKind = Cardinal; // TLSPCompletionTriggerKind = 1 | 2 | 3;

  // Contains additional information about the context in which a completion
  // request is triggered.
  TLSPCompletionContext = record
    // How the completion was triggered.
    triggerKind: TLSPCompletionTriggerKind;

    // The trigger character (a single character) that has trigger code
    // complete. Is undefined if
    // `triggerKind !== CompletionTriggerKind.TriggerCharacter`
    triggerCharacter: string;
  end;

  // The Completion request is sent from the client to the server to compute completion
  // items at a given cursor position. Completion items are presented in the IntelliSense user interface.
  TLSPCompletionParams = class(TLSPTextDocumentPositionParams)
    // WorkDoneProgressParams, PartialResultParams
    // An optional token that a server can use to report work done progress.
     workDoneToken: TLSPProgressToken;

    // The completion context. This is only available if the client specifies
    // to send this using the client capability
    // `completion.contextSupport === true`
    context: TLSPCompletionContext;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
    partialResultToken: TLSPProgressToken;
  end;

  TLSPInsertTextFormats = record
    const plainText = 1;
    const snippet = 2;
  end;
  // Defines whether the insert text in a completion item should be interpreted as
  // plain text or a snippet.
  //
  // 1 (plainText)
  // The primary text to be inserted is treated as a plain string.
  //
  // 2 (snippet)
  // The primary text to be inserted is treated as a snippet.
  //
  // A snippet can define tab stops and placeholders with `$1`, `$2`
  // and `${3:foo}`. `$0` defines the final tab stop, it defaults to
  // the end of the snippet. Placeholders with equal identifiers are linked,
  // that is typing in one will update others too.
  TLSPInsertTextFormat = Cardinal; // 1 | 2;

  // A special text edit to provide an insert and a replace operation.
  //
  // @since 3.16.0
  //
  TLSPInsertReplaceEdit = class(TLSPTextEdit)
    // The range if the insert is requested.
    insert: TLSPRange;

    // The range if the replace is requested.
    replace: TLSPRange;
  end;

  TLSPCompletionInsertReplaceEdit = record
    // This record combine TLSPTextEdit + TLSPInsertReplaceEdit
    // A boolean to inform us if a TLSPInsertReplaceEdit object was found
    insertReplaceEdit: Boolean;

    // The string to be inserted.
    newText: string;

    // The range if the insert is requested
    insert: TLSPRange;

    // The range if the replace is requested.
    replace: TLSPRange;

    // The range of the text document to be manipulated. To insert
    // text into a document create a range where start === end.
    range: TLSPRange;
  end;

  TLSPCompletionItem = class(TLSPBaseParams)
  public
    // For private use only. You can use this flag in your own code to find out
    // if the completion item comes from OnCompletion or OnCompletionResolve.
    [DISABLE]
    resolved: Boolean;

    // The label of this completion item. By default
    // also the text that is inserted when selecting
    // this completion.
    [ALIAS('label')]
    slabel: string;

    // The kind of this completion item. Based of the kind
    // an icon is chosen by the editor. The standardized set
    // of available values is defined in `CompletionItemKind`.
    kind: Integer;

    // Tags for this completion item.
    //
    // @since 3.15.0
    //
    tags: TArray<TLSPCompletionItemTag>;

    // A human-readable string with additional information
    // about this item, like type or symbol information.
    detail: string;

    // A human-readable string that represents a doc-comment.
    documentation: TLSPMarkupContent;

    // Indicates if this item is deprecated.
    //
    // @deprecated Use `tags` instead if supported.
    //
    deprecated: boolean;

    // Select this item when showing.
    //
    // *Note* that only one completion item can be selected and that the
    // tool / client decides which item that is. The rule is that the *first*
    // item of those that match best is selected.
    preselect: boolean;

    // A string that should be used when comparing this item
    // with other items. When `falsy` the label is used.
    sortText: string;

    // A string that should be used when filtering a set of
    // completion items. When `falsy` the label is used.
    filterText: string;

    // A string that should be inserted into a document when selecting
    // this completion. When `falsy` the label is used.
    //
    // The `insertText` is subject to interpretation by the client side.
    // Some tools might not take the string literally. For example
    // VS Code when code complete is requested in this example
    // `con<cursor position>` and a completion item with an `insertText` of
    // `console` is provided it will only insert `sole`. Therefore it is
    // recommended to use `textEdit` instead since it avoids additional client
    // side interpretation.
    insertText: string;

    // The format of the insert text. The format applies to both the
    // `insertText` property and the `newText` property of a provided
    // `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
    insertTextFormat: TLSPInsertTextFormat;

    // How whitespace and indentation is handled during completion
    // item insertion. If not provided the client's default value depends on
    // the `textDocument.completion.insertTextMode` client capability.
    //
    // @since 3.16.0
    //
    insertTextMode: TLSPInsertTextMode;

    // An edit which is applied to a document when selecting this completion.
    // When an edit is provided the value of `insertText` is ignored.
    //
    // *Note:* The range of the edit must be a single line range and it must
    // contain the position at which completion has been requested.
    //
    // Most editors support two different operations when accepting a completion
    // item. One is to insert a completion text and the other is to replace an
    // existing text with a completion text. Since this can usually not be
    // predetermined by a server it can report both ranges. Clients need to
    // signal support for `InsertReplaceEdits` via the
    // `textDocument.completion.insertReplaceSupport` client capability
    // property.
    //
    // *Note 1:* The text edit's range as well as both ranges from an insert
    // replace edit must be a [single line] and they must contain the position
    // at which completion has been requested.
    // *Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
    // must be a prefix of the edit's replace range, that means it must be
    // contained and starting at the same position.
    //
    // @since 3.16.0 additional type `InsertReplaceEdit`
    //
    textEdit: TLSPTextEdit;

    // An optional array of additional text edits that are applied when
    // selecting this completion. Edits must not overlap (including the same
    // insert position) with the main edit nor with themselves.
    //
    // Additional text edits should be used to change text unrelated to the
    // current cursor position (for example adding an import statement at the
    // top of the file if the completion item will insert an unqualified type).
    additionalTextEdits: TObjectList<TLSPTextEdit>;

    // An optional set of characters that when pressed while this completion is
    // active will accept it first and then type that character. *Note* that all
    // commit characters should have `length=1` and that superfluous characters
    // will be ignored.
    commitCharacters: TArray<string>;

    // An optional command that is executed *after* inserting this completion.
    // *Note* that additional modifications to the current document should be
    // described with the additionalTextEdits-property.
    command: TLSPCommand;

    // A data entry field that is preserved on a completion item between
    // a completion and a completion resolve request.
    data: Variant;
    constructor Create;
    destructor Destroy; override;
  end;

  // Represents a collection of [completion items](#CompletionItem) to be
  // presented in the editor.
  TLSPCompletionList = class(TLSPBaseParams)
  public
    // This list it not complete. Further typing should result in recomputing
    // this list.
    isIncomplete: boolean;

    // The completion items.
    items: TObjectList<TLSPCompletionItem>;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPMarkedString = record
    // Language identifier
    language: string;

    // The content itself
    value: string;
  end;

  TLSPHoverParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  // The result of a hover request.
  TLSPHover = class(TLSPBaseParams)
  public
    // The hover's content
    contents: TLSPMarkupContent;  // MarkedString | MarkedString[] | MarkupContent;
    [DISABLE]
    contentsMarked: TLSPMarkedString;
    [DISABLE]
    contentsMarkedArray: TArray<TLSPMarkedString>;

    // An optional range is a range inside a text document
    // that is used to visualize a hover, e.g. by changing the background color.
    range: TLSPRange;
  end;

  TLSPParameterInformation = record
    // The label of this parameter information.
    //
    // Either a string or an inclusive start and exclusive end offsets within
    // its containing signature label. (see SignatureInformation.label). The
    // offsets are based on a UTF-16 string representation as `Position` and
    // `Range` does.
    //
    // *Note*: a label of type string should be a substring of its containing
    // signature label. Its intended use case is to highlight the parameter
    // label part in the `SignatureInformation.label`.
    [ALIAS('label')]
    slabel: string; // string | [uinteger, uinteger];

    // The human-readable doc-comment of this parameter. Will be shown
    // in the UI but can be omitted.
    documentation: TLSPMarkupContent; // string | MarkupContent;
  end;

  TLSPSignatureInformation = record
    // The label of this signature. Will be shown in
    // the UI.
    [ALIAS('label')]
    slabel: string;

    // The human-readable doc-comment of this signature. Will be shown
    // in the UI but can be omitted.
    documentation: TLSPMarkupContent; // string | MarkupContent;

    // The parameters of this signature.
    parameters: TArray<TLSPParameterInformation>;

    // The index of the active parameter.
    //
    // If provided, this is used in place of `SignatureHelp.activeParameter`.
    //
    // @since 3.16.0
    //
    activeParameter: Cardinal;
  end;

  TLSPSignatureHelp = class(TLSPBaseParams)
  public
    // One or more signatures. If no signatures are available the signature help
    // request should return `null`.
    signatures: TArray<TLSPSignatureInformation>;

    // The active signature. If omitted or the value lies outside the
    // range of `signatures` the value defaults to zero or is ignore if
    // the `SignatureHelp` as no signatures.
    //
    // Whenever possible implementors should make an active decision about
    // the active signature and shouldn't rely on a default value.
    //
    // In future version of the protocol this property might become
    // mandatory to better express this.
    activeSignature: Cardinal;

    // The active parameter of the active signature. If omitted or the value
    // lies outside the range of `signatures[activeSignature].parameters`
    // defaults to 0 if the active signature has parameters. If
    // the active signature has no parameters it is ignored.
    // In future version of the protocol this property might become
    // mandatory to better express the active parameter if the
    // active signature does have any.
    activeParameter: Cardinal;
  end;

  TLSPSignatureHelpContext = record
    // Action that caused signature help to be triggered.
    // SignatureHelpTriggerKind = 1 | 2 | 3;
    //
    // 1 - Invoked: Signature help was invoked manually by the user or by a command.
    // 2 - TriggerCharacter: Signature help was triggered by a trigger character.
    // 3 - ContentChange: ignature help was triggered by the cursor moving or by the document content changing.
    triggerKind: Cardinal;

    // Character that caused signature help to be triggered.
    //
    // This is undefined when triggerKind !==
    // SignatureHelpTriggerKind.TriggerCharacter
    triggerCharacter: string;

    // `true` if signature help was already showing when it was triggered.
    //
    // Retriggers occur when the signature help is already active and can be
    // caused by actions such as typing a trigger character, a cursor move, or
    // document content changes.
    isRetrigger: boolean;

    // The currently active `SignatureHelp`.
    //
    // The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field
    // updated based on the user navigating through available signatures.
    activeSignatureHelp: TLSPSignatureHelp;
  end;

  TLSPSignatureHelpParams = class(TLSPTextDocumentPositionParams)
  public
    // The signature help context. This is only available if the client
    // specifies to send this using the client capability
    // `textDocument.signatureHelp.contextSupport === true`
    //
    // @since 3.15.0
    //
    context: TLSPSignatureHelpContext;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPDeclarationParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPDefinitionParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPImplmentationParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPTypeDefinitionParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPGotoResponse = class(TLSPBaseParams)
  public
    location: TLSPLocation;
    locations: TArray<TLSPLocation>;
    locationLinks: TArray<TLSPLocationLink>;
  end;

  TLSPDocumentHighlightParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  // A document highlight kind.
  //
  // A textual occurrence.
  // const Text = 1;
  //
  // Read-access of a symbol, like reading a variable.
  // const Read = 2;
  //
	// Write-access of a symbol, like writing to a variable.
  // const Write = 3;
  //
  TLSPDocumentHighlightKind = Cardinal; // 1 | 2 | 3;

  // A document highlight is a range inside a text document which deserves
  // special attention. Usually a document highlight is visualized by changing
  // the background color of its range.
  TLSPDocumentHighlight = record
    // The range this highlight applies to.
    range: TLSPRange;

    // The highlight kind, default is DocumentHighlightKind.Text.
    kind: TLSPDocumentHighlightKind;
  end;

  TLSPDocumentHighlightResponse = class(TLSPBaseParams)
  public
    list: TArray<TLSPDocumentHighlight>;
  end;

  TLSPDocumentSymbolParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

   // Represents programming constructs like variables, classes, interfaces etc.
   // that appear in a document. Document symbols can be hierarchical and they
   // have two ranges: one that encloses its definition and one that points to its
   // most interesting range, e.g. the range of an identifier.
  TLSPDocumentSymbol = record
    // The name of this symbol. Will be displayed in the user interface and
    // therefore must not be an empty string or a string only consisting of
    // white spaces.
    name: string;

    // More detail for this symbol, e.g the signature of a function.
    detail: string;

    // The kind of this symbol.
    kind: TLSPSymbolKind;

    // Tags for this document symbol.
    //
    // @since 3.16.0
    //
    tags: TArray<TLSPSymbolTag>;

    // Indicates if this symbol is deprecated.
    //
    // @deprecated Use tags instead
    //
    isdeprecated: boolean;

    // The range enclosing this symbol not including leading/trailing whitespace
    // but everything else like comments. This information is typically used to
    // determine if the clients cursor is inside the symbol to reveal in the
    // symbol in the UI.
    range: TLSPRange;

    // The range that should be selected and revealed when this symbol is being
    // picked, e.g. the name of a function. Must be contained by the `range`.
    selectionRange: TLSPRange;

    // Children of this symbol, e.g. properties of a class.
    children: TArray<TLSPDocumentSymbol>;
  end;

  TLSPCodeActionContext = record
    // An array of diagnostics known on the client side overlapping the range
    // provided to the `textDocument/codeAction` request. They are provided so
    // that the server knows which errors are currently presented to the user
    // for the given range. There is no guarantee that these accurately reflect
    // the error state of the resource. The primary parameter
    // to compute code actions is the provided range.
    diagnostics: TArray<TLSPDiagnostic>;

    // Requested kind of actions to return.
    //
    // Actions not of this kind are filtered out by the client before being
    // shown. So servers can omit computing them.
    only: TArray<TLSPCodeActionKind>;
  end;

  TLSPDocumentSymbolsResponse = class(TLSPBaseParams)
  public
    symbols: TArray<TLSPDocumentSymbol>;
    symbolInformations: TArray<TLSPSymbolInformation>;
  end;

  TLSPCodeActionParams = class(TLSPBaseParams)
  public
    // The document in which the command was invoked.
    textDocument: TLSPTextDocumentIdentifier;

    // The range for which the command was invoked.
    range: TLSPRange;

    // Context carrying additional information.
    context: TLSPCodeActionContext;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPDisabled = record
    // Human readable description of why the code action is currently
    // disabled.
    //
    // This is displayed in the code actions UI.
    reason: string;
  end;

  // A code action represents a change that can be performed in code, e.g. to fix
  // a problem or to refactor code.
  //
  // A CodeAction must set either `edit` and/or a `command`. If both are supplied,
  // the `edit` is applied first, then the `command` is executed.
  TLSPCodeAction = class(TLSPBaseParams)
  public
    // A short, human-readable, title for this code action.
    title: string;

    // The kind of the code action.
    //
    // Used to filter code actions.
    kind: TLSPCodeActionKind;

    // The diagnostics that this code action resolves.
    diagnostics: TArray<TLSPDiagnostic>;

    // Marks this as a preferred action. Preferred actions are used by the
    // `auto fix` command and can be targeted by keybindings.
    //
    // A quick fix should be marked preferred if it properly addresses the
    // underlying error. A refactoring should be marked preferred if it is the
    // most reasonable choice of actions to take.
    //
    // @since 3.15.0
    //
    isPreferred: boolean;

    // Marks that the code action cannot currently be applied.
    //
    // Clients should follow the following guidelines regarding disabled code
    // actions:
    //
    // - Disabled code actions are not shown in automatic lightbulbs code
    //   action menus.
    //
    // - Disabled actions are shown as faded out in the code action menu when
    //   the user request a more specific type of code action, such as
    //   refactorings.
    //
    // - If the user has a keybinding that auto applies a code action and only
    //   a disabled code actions are returned, the client should show the user
    //   an error message with `reason` in the editor.
    //
    // @since 3.16.0
    //
    disabled: TLSPDisabled;

    // The workspace edit this code action performs.
    edit: TLSPWorkspaceEdit;

    // A command this code action executes. If a code action
    // provides an edit and a command, first the edit is
    // executed and then the command.
    command: TLSPCommand;

    // A data entry field that is preserved on a code action between
    // a `textDocument/codeAction` and a `codeAction/resolve` request.
    //
    // @since 3.16.0
    //
    data: string;
  end;

  TLSPCodeActionResponse = class(TLSPBaseParams)
  public
    codeActions: TObjectList<TLSPCodeAction>;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPCodeLensParams = class(TLSPBaseParams)
  public
    // The document to request code lens for.
    textDocument: TLSPTextDocumentIdentifier;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  // A code lens represents a command that should be shown along with
  // source text, like the number of references, a way to run tests, etc.
  //
  // A code lens is _unresolved_ when no command is associated to it. For
  // performance reasons the creation of a code lens and resolving should be done
  // in two stages.
  TLSPCodeLens = class(TLSPBaseParams)
  public
    // The range in which this code lens is valid. Should only span a single
    // line.
    range: TLSPRange;

    // The command this code lens represents.
    command: TLSPCommand;

    // A data entry field that is preserved on a code lens item between
    // a code lens and a code lens resolve request.
    data: string;
  end;

  TLSPCodeLensResponse = class(TLSPBaseParams)
  public
    codeLensList: TObjectList<TLSPCodeLens>;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPDocumentLinkParams = class(TLSPBaseParams)
  public
    // The document to provide document links for.
    textDocument: TLSPTextDocumentIdentifier;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  // A document link is a range in a text document that links to an internal or
  // external resource, like another text document or a web site.
  TLSPDocumentLink = class(TLSPBaseParams)
  public
    // The range this link applies to.
    range: TLSPRange;

    // The uri this link points to. If missing a resolve request is sent later.
    target: TLSPDocumentUri;

    // The tooltip text when you hover over this link.
    //
    // If a tooltip is provided, is will be displayed in a string that includes
    // instructions on how to trigger the link, such as `{0} (ctrl + click)`.
    // The specific instructions vary depending on OS, user settings, and
    // localization.
    //
    // @since 3.15.0
    //
    tooltip: string;

    // A data entry field that is preserved on a document link between a
    // DocumentLinkRequest and a DocumentLinkResolveRequest.
    data: Variant;
  end;

  TLSPDocumentLinkResponse = class(TLSPBaseParams)
  public
    documentLinks: TObjectList<TLSPDocumentLink>;
    constructor Create;
    destructor Destroy; override;
  end;

  TLSPDocumentColorParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  // Represents a color in RGBA space.
  TLSPColor = record
    // The red component of this color in the range [0-1].
    red: double;

    // The green component of this color in the range [0-1].
    green: double;

    // The blue component of this color in the range [0-1].
    blue: double;

    // The alpha component of this color in the range [0-1].
    alpha: double;
  end;

  TLSPColorInformation = record
    // The range in the document where this color appears.
    range: TLSPRange;

    // The actual color value for this color range.
    color: TLSPColor;
  end;

  TLSPColorInformationValues = class(TLSPBaseParams)
  public
    colors: TArray<TLSPColorInformation>;
  end;

  TLSPColorPresentationParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // The color information to request presentations for.
    color: TLSPColor;

    // The range where the color would be inserted. Serves as a context.
    range: TLSPRange;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPColorPresentation = record
    // The label of this color presentation. It will be shown on the color
    // picker header. By default this is also the text that is inserted when
    // selecting this color presentation.
    slabel: string;

    // An [edit](#TextEdit) which is applied to a document when selecting
    // this presentation for the color.  When `falsy` the
    // [label](#ColorPresentation.label) is used.
    textEdit: TLSPTextEdit;

    // An optional array of additional [text edits](#TextEdit) that are applied
    // when selecting this color presentation. Edits must not overlap with the
    // main [edit](#ColorPresentation.textEdit) nor with themselves.
    additionalTextEdits: TArray<TLSPTextEdit>;
  end;

  TLSPColorPresentationValues = class(TLSPBaseParams)
  public
    colorPresentations: TArray<TLSPColorPresentation>;
  end;

  // Value-object describing what options formatting should use.
  TLSPFormattingOptions = record
    // Size of a tab in spaces.
    tabSize: Cardinal;

    // Prefer spaces over tabs.
    insertSpaces: boolean;

    // Trim trailing whitespace on a line.
    //
    // @since 3.15.0
    //
    trimTrailingWhitespace: boolean;

    // Insert a newline character at the end of the file if one does not exist.
    //
    // @since 3.15.0
    //
    insertFinalNewline: boolean;

    // Trim all newlines after the final newline at the end of the file.
    //
    // @since 3.15.0
    //
    trimFinalNewlines: boolean;

    // Signature for further properties. ([key: string]: boolean | integer | string)
    // key and value below are converted in "CreateJSONRequestParam()" to: "key": value;
    [DISABLE]
    key: string;
    [DISABLE]
    value: Variant;
  end;

  TLSPDocumentFormattingParams = class(TLSPBaseParams)
  public
    // The document to format.
    textDocument: TLSPTextDocumentIdentifier;

    // The format options.
    options: TLSPFormattingOptions;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPDocumentRangeFormattingParams = class(TLSPBaseParams)
  public
    // The document to format.
    textDocument: TLSPTextDocumentIdentifier;

    // The range to format
    range: TLSPRange;

    // The format options
    options: TLSPFormattingOptions;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPDocumentOnTypeFormattingParams = class(TLSPTextDocumentPositionParams)
  public
    // The character that has been typed.
    ch: string;

    // The format options.
    options: TLSPFormattingOptions;
  end;

  TLSPTextEditValues = class(TLSPBaseParams)
  public
    edits: TArray<TLSPTextEdit>;
  end;

  TLSPRenameParams = class(TLSPTextDocumentPositionParams)
  public
    // The new name of the symbol. If the given name is not valid the
    // request must return a [ResponseError](#ResponseError) with an
    // appropriate message set.
    newName: string;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPPrepareRenameParams = class(TLSPTextDocumentPositionParams)
  public
  end;

  TLSPPrepareRenameResponse = class(TLSPBaseParams)
  public
    // Describing a Range of the string to rename
    range: TLSPRange;

    // Placeholder text of the string content to be renamed
    placeholder: string;

    // If { defaultBehavior: boolean } is returned (since 3.16) the rename position is
    // valid and the client should use its default behavior to compute the rename range.
    defaultBehavior: Boolean;
  end;

  // Folding Range Requests

  TLSPFoldingRangeParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  // Represents a folding range. To be valid, start and end line must be bigger
  // than zero and smaller than the number of lines in the document. Clients
  // are free to ignore invalid ranges.
  TLSPFoldingRange = record
    // The zero-based start line of the range to fold. The folded area starts
    // after the line's last character. To be valid, the end must be zero or
    // larger and smaller than the number of lines in the document.
    startLine: Cardinal;

    // The zero-based character offset from where the folded range starts. If
    // not defined, defaults to the length of the start line (-1).
    startCharacter: Integer;

    // The zero-based end line of the range to fold. The folded area ends with
    // the line's last character. To be valid, the end must be zero or larger
    // and smaller than the number of lines in the document.
    endLine: Cardinal;

    // The zero-based character offset before the folded range ends. If not
    // defined, defaults to the length of the end line (-1).
    endCharacter: Integer;

    // Describes the kind of the folding range such as 'comment', 'imports' or 'region'.
    // The kind is used to categorize folding ranges and used by commands like
    // 'Fold all comments'. See [FoldingRangeKind](#FoldingRangeKind) for an
    // enumeration of standardized kinds.
    kind: string;
  end;

  TLSPFoldingRangeResponse = class(TLSPBaseParams)
  public
    foldingRanges: TArray<TLSPFoldingRange>;
  end;

  // Selection Range Requests

  TLSPSelectionRangeParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // The positions inside the text document.
    positions: TArray<TLSPPosition>;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPSelectionRange = class(TLSPBaseParams)
  public
    // The [range](#Range) of this selection range.
    range: TLSPRange;

    // The parent selection range containing this range. Therefore
    // `parent.range` must contain `this.range`.
    parent: TLSPSelectionRange;
  end;

  TLSPSelectionRangeResponse = class(TLSPBaseParams)
  public
    selRanges: TObjectList<TLSPSelectionRange>;
    constructor Create;
    destructor Destroy; override;
  end;

  // Call Hierarchy Calls

  TLSPCallHierarchyPrepareParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPCallHierarchyItem = record
    // The name of this item.
    name: string;

    // The kind of this item.
    kind: TLSPSymbolKind;

    // Tags for this item.
    tags: TArray<TLSPSymbolTag>;

    // More detail for this item, e.g. the signature of a function.
    detail: string;

    // The resource identifier of this item.
    uri: TLSPDocumentUri;

    // The range enclosing this symbol not including leading/trailing whitespace
    // but everything else, e.g. comments and code.
    range: TLSPRange;

    // The range that should be selected and revealed when this symbol is being
    // picked, e.g. the name of a function. Must be contained by the
    // [`range`](#CallHierarchyItem.range).
    selectionRange: TLSPRange;

    // A data entry field that is preserved between a call hierarchy prepare and
    // incoming calls or outgoing calls requests.
    data: Variant;
  end;

  TLSPPrepareCallHierarchyResponse = class(TLSPBaseParams)
  public
    items: TArray<TLSPCallHierarchyItem>;
  end;

  TLSPCallHierarchyIncomingCallsParams = class(TLSPBaseParams)
  public
    item: TLSPCallHierarchyItem;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPCallHierarchyIncomingCall = record
    // The item that makes the call.
    [ALIAS('from')]
    callFrom: TLSPCallHierarchyItem;

    // The ranges at which the calls appear. This is relative to the caller
    // denoted by [`this.from`](#CallHierarchyIncomingCall.from).
    fromRanges: TArray<TLSPRange>;
  end;

  TLSPCallHierarchyIncomingCallResponse = class(TLSPBaseParams)
  public
    items: TArray<TLSPCallHierarchyIncomingCall>;
  end;

  TLSPCallHierarchyOutgoingCallsParams = class(TLSPBaseParams)
  public
    item: TLSPCallHierarchyItem;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPCallHierarchyOutgoingCall = record
    // The item that is called.
    [ALIAS('to')]
    callTo: TLSPCallHierarchyItem;

    // The range at which this item is called. This is the range relative to
    // the caller, e.g the item passed to `callHierarchy/outgoingCalls` request.
    fromRanges: TArray<TLSPRange>;
  end;

  TLSPCallHierarchyOutgoingCallResponse = class(TLSPBaseParams)
  public
    items: TArray<TLSPCallHierarchyOutgoingCall>;
  end;

  // Sementic Tokens

  TLSPSemanticTokensParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPSemanticTokens = class(TLSPBaseParams)
  public
    // An optional result id. If provided and clients support delta updating
    // the client will include the result id in the next semantic token request.
    // A server can then instead of computing all semantic tokens again simply
    // send a delta.
    resultId: string;

    // The actual tokens.
    data: TArray<Cardinal>;
  end;

  TLSPSemanticTokensPartialResult = class(TLSPBaseParams)
  public
    data: TArray<Cardinal>;
  end;

  TLSPSemanticTokensDeltaParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // The result id of a previous response. The result Id can either point to
    // a full response or a delta response depending on what was received last.
    previousResultId: string;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPSemanticTokensEdit = record
    // The start offset of the edit.
    start: Cardinal;

    // The count of elements to remove.
    deleteCount: Cardinal;

    // The elements to insert.
    data: TArray<Cardinal>;
  end;

  TLSPSemanticTokensDelta = class(TLSPBaseParams)
  public
    resultId: string;

    // The semantic token edits to transform a previous result into a new
    // result.
    edits: TArray<TLSPSemanticTokensEdit>;
  end;

  TLSPSemanticTokensDeltaPartialResult = class(TLSPBaseParams)
  public
    edits: TArray<TLSPSemanticTokensEdit>;
  end;

  TLSPSemanticTokensRangeParams = class(TLSPBaseParams)
  public
    // The text document.
    textDocument: TLSPTextDocumentIdentifier;

    // The range the semantic tokens are requested for.
    range: TLSPRange;

    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPLinkedEditingRangeParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  TLSPLinkedEditingRanges = class(TLSPBaseParams)
  public
    // A list of ranges that can be renamed together. The ranges must have
    // identical length and contain identical text content. The ranges cannot overlap.
    ranges: TArray<TLSPRange>;

    // An optional word pattern (regular expression) that describes valid contents for
    // the given ranges. If no pattern is provided, the client configuration's word
    // pattern will be used.
    wordPattern: string;
  end;

  TLSPMonikerParams = class(TLSPTextDocumentPositionParams)
  public
    // An optional token that a server can use to report partial results (e.g.
	  // streaming) to the client.
	  partialResultToken: TLSPProgressToken;

    // An optional token that a server can use to report work done progress.
    workDoneToken: TLSPProgressToken;
  end;

  // Moniker uniqueness level to define scope of the moniker.
  //
  // document - The moniker is only unique inside a document
  // project  - The moniker is unique inside a project for which a dump got created
  // group    - The moniker is unique inside the group to which a project belongs
  // scheme   - The moniker is unique inside the moniker scheme.
  // global   - The moniker is globally unique
  TLSPUniquenessLevel = string;

  // The moniker kind.
  //
  // import - The moniker represent a symbol that is imported into a project
  // export - The moniker represents a symbol that is exported from a project
  // local  - The moniker represents a symbol that is local to a project (e.g. a local
  //          variable of a function, a class not visible outside the project, ...)
  TLSPMonikerKind = string;

  // Moniker definition to match LSIF 0.5 moniker definition.
  TLSPMoniker = record
    // The scheme of the moniker. For example tsc or .Net
    scheme: string;

    // The identifier of the moniker. The value is opaque in LSIF however
    // schema owners are allowed to define the structure if they want.
    identifier: string;

    // The scope in which the moniker is unique
    unique: TLSPUniquenessLevel;

    // The moniker kind if known.
    kind: TLSPMonikerKind;
  end;

  TLSPMonikerResult = class(TLSPBaseParams)
  public
    monikers: TArray<TLSPMoniker>;
  end;


  TLSPKind = (lspInitialize,lspInitialized,lspShutdown,lspExit,
            lspShowMessage,lspShowMessageRequest,lspShowDocumentRequest,lspLogMessage,
            lspWorkDoneProgress,lspWorkDoneProgressCancel,
            lspTelemetryEvent,
            lspClientRegisterCapabilities,lspClientUnRegisterCapabilities,
            lspWorkspaceFolders,lspDidChangeWorkspaceFolders,lspDidChangeConfiguration,
            lspWorkspaceConfiguration,lspDidChangeWatchedFiles,lspWorkspaceSymbol,lspWorkspaceExecuteCommand,
            lspWorkspaceApplyEdit,lspWorkspaceWillCreateFiles,lspWorkspaceDidCreateFiles,
            lspWorkspaceWillRenameFiles,lspWorkspaceDidRenameFiles,lspWorkspaceWillDeleteFiles,lspWorkspaceDidDeleteFiles,
            lspDidOpenTextDocument,lspDidChangeTextDocument,lspWillSaveTextDocument,lspWillSaveWaitUntilTextDocument,
            lspDidSaveTextDocument,lspDidCloseTextDocument,
            lspPublishDiagnostics,
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
            lspCancelRequest,lspProgress,lspLogTrace,lspSetTrace,lspError);

const
  LSPIdStrings : array[0..73] of String = (
    'initialize',
    'initialized',
    'shutdown',
    'exit',
    'window/ShowMessage',
    'window/showMessageRequest',
    'window/showDocument',
    'window/logMessage',
    'window/workDoneProgress/create',
    'window/workDoneProgress/cancel',
    'telemetry/event',
    'client/registerCapability',
    'client/unregisterCapability',
    'workspace/workspaceFolders',
    'workspace/didChangeWorkspaceFolders',
    'workspace/didChangeConfiguration',
    'workspace/configuration',
    'workspace/didChangeWatchedFiles',
    'workspace/symbol',
    'workspace/executeCommand',
    'workspace/applyEdit',
    'workspace/willCreateFiles',
    'workspace/didCreateFiles',
    'workspace/willRenameFiles',
    'workspace/didRenameFiles',
    'workspace/willDeleteFiles',
    'workspace/didDeleteFiles',
    'textDocument/didOpen',
    'textDocument/didChange',
    'textDocument/willSave',
    'textDocument/willSaveWaitUntil',
    'textDocument/didSave',
    'textDocument/didClose',
    'textDocument/publishDiagnostics',
    'textDocument/completion',
    'completionItem/resolve',
    'textDocument/hover',
    'textDocument/signatureHelp',
    'textDocument/declaration',
    'textDocument/definition',
    'textDocument/typeDefinition',
    'textDocument/implementation',
    'textDocument/documentHighlight',
    'textDocument/documentSymbol',
    'textDocument/references',
    'textDocument/codeAction',
    'CodeAction/resolve',
    'textDocument/codeLens',
    'codeLens/resolve',
    'workspace/codeLens/refresh',
    'textDocument/documentLink',
    'documentLink/resolve',
    'textDocument/documentColor',
    'textDocument/colorPresentation',
    'textDocument/formatting',
    'textDocument/rangeFormatting',
    'textDocument/onTypeFormatting',
    'textDocument/rename',
    'textDocument/prepareRename',
    'textDocument/foldingRange',
    'textDocument/selectionRange',
    'textDocument/prepareCallHierarchy',
    'callHierarchy/incomingCalls',
    'callHierarchy/outgoingCalls',
    'textDocument/semanticTokens/full',
    'textDocument/semanticTokens/full/delta',
    'textDocument/semanticTokens/range',
    'workspace/semanticTokens/refresh',
    'textDocument/linkedEditingRange',
    'textDocument/moniker',
    '$/cancelRequest',
    '$/progress',
    '$/logTrace',
    '$/setTrace'
  );

type

  TLSPMessage = class
  public
    id: Integer;
    paramObj: TLSPBaseParams;
    errorObj: TLSPBaseParams;
  end;

  TLSPResultType = (lsprObject, lsprString, lsprNull, lsprVoid, lsprEmptyArray);

implementation

uses System.SysUtils, XLSPFunctions;

// TLSPInitializeParams

constructor TLSPInitializeParams.Create;
begin
  inherited;
  processId := 0;
  rootPath := '';
  rootUri := '';
  initializationOptions := nil;
  trace := 'off';
  clientInfo := TLSPClientInfo.Create;
end;

destructor TLSPInitializeParams.Destroy;
begin
  SetLength(workspaceFolders,0);
  FreeAndNil(clientInfo);
  FreeAndNil(capabilities);
  inherited;
end;

procedure TLSPInitializeParams.AddRoot(const sz: string);
begin
  rootPath := sz;
  rootUri := FilePathToUri(sz);
end;

procedure TLSPInitializeParams.AddWorkspaceFolders(const ls: TStringList);
var
  i: Integer;
  s: string;
begin
  // Set workspace folders
  SetLength(workspaceFolders,ls.Count);
  for i := 0 to ls.Count - 1 do
  begin
    s := ls[i];
    if s <> '' then
    begin
      workspaceFolders[i].uri := FilePathToUri(s);
      workspaceFolders[i].name := ExtractFileName(s);
    end;
  end;
end;

// TLSPClientCapabilities

constructor TLSPClientCapabilities.Create;
begin
  inherited;
end;

destructor TLSPClientCapabilities.Destroy;
begin
  FreeAndNil(workspace);
  FreeAndNil(textDocument);
  FreeAndNil(window);
  inherited;
end;

procedure TLSPClientCapabilities.AddCallHierarchySupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/callHierarchy
  textDocument.callHierarchy := TLSPCallHierarchyClientCapabilities.Create;
  textDocument.callHierarchy.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddCodeActionSupport(const dynamicRegistration, isPreferredSupport, disabledSupport,
    dataSupport, honorsChangeAnnotations: Boolean; const codeActions: TArray<String> = nil; const resolveProperties:
    TArray<String> = nil);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // Capabilities/textDocument/codeAction
  textDocument.codeAction := TLSPCodeActionClientCapabilities.Create;
  textDocument.codeAction.dynamicRegistration := dynamicRegistration;
  textDocument.codeAction.isPreferredSupport := isPreferredSupport;
  textDocument.codeAction.disabledSupport := disabledSupport;
  textDocument.codeAction.dataSupport := dataSupport;
  textDocument.codeAction.honorsChangeAnnotations := honorsChangeAnnotations;

  if Assigned(codeActions) then
  begin
    textDocument.codeAction.codeActionLiteralSupport := TLSPCodeActionLiteralSupport.Create;
    textDocument.codeAction.codeActionLiteralSupport.codeActionKind := TLSPCodeActionKindValues.Create;
    textDocument.codeAction.codeActionLiteralSupport.codeActionKind.valueSet := Copy(codeActions);
  end;

  if Assigned(resolveProperties) then
  begin
    textDocument.codeAction.resolveSupport := TLSPResolveSupport.Create;
    textDocument.codeAction.resolveSupport.properties := Copy(resolveProperties);
  end;
end;

procedure TLSPClientCapabilities.AddCodeLensSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // Capabilities/textDocument/codeLens
  textDocument.codeLens := TLSPCodeLensClientCapabilities.Create;
  textDocument.codeLens.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddColorProvider(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/colorProvider
  textDocument.colorProvider := TLSPDocumentColorClientCapabilities.Create;
  textDocument.colorProvider.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddCompletionSupport(const dynamicRegistration, snippetSupport,
    commitCharactersSupport, documentationFormatPlainText, documentationFormatMarkdown, deprecatedSupport,
    preselectSupport, insertReplaceSupport, contextSupport: Boolean; const completionItemKindValues: TArray<Integer> =
    nil; const resolveSuppert: TArray<String> = nil; const insertTextModeSupportValues: TArray<TLSPInsertTextMode> =
    nil; const tagSupport: TLSPtagSupportValues = nil);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // Capabilities/textDocument/completion
  textDocument.completion := TLSPCompletionClientCapabilities.Create;

  // Capabilities/textDocument/completion/completionItem
  textDocument.completion.completionItem := TLSPClientCompletionItem.Create;
  textDocument.completion.completionItem.snippetSupport := snippetSupport;
  textDocument.completion.completionItem.commitCharactersSupport := commitCharactersSupport;
  textDocument.completion.completionItem.deprecatedSupport := deprecatedSupport;
  textDocument.completion.completionItem.preselectSupport := preselectSupport;
  textDocument.completion.completionItem.insertReplaceSupport := insertReplaceSupport;

  if documentationFormatPlainText and documentationFormatMarkdown then
  begin
    SetLength(textDocument.completion.completionItem.documentationFormat, 2);
    textDocument.completion.completionItem.documentationFormat := ['plaintext','markdown'];
  end
  else if documentationFormatPlainText then
  begin
    SetLength(textDocument.completion.completionItem.documentationFormat, 1);
    textDocument.completion.completionItem.documentationFormat := ['plaintext'];
  end
  else if documentationFormatMarkdown then
  begin
    SetLength(textDocument.completion.completionItem.documentationFormat, 1);
    textDocument.completion.completionItem.documentationFormat := ['markdown'];
  end;

  // Capabilities/textDocument/completion/completionItem/resolveSupport
  if Assigned(resolveSuppert) then
  begin
    textDocument.completion.completionItem.resolveSupport := TLSPResolveSupport.Create;
    textDocument.completion.completionItem.resolveSupport.properties := Copy(resolveSuppert);
  end;

  // Capabilities/textDocument/completion/completionItem/insertTextModeSupport
  if Assigned(insertTextModeSupportValues) then
  begin
    textDocument.completion.completionItem.insertTextModeSupport := TLSPInsertTextModeSupport.Create;
    textDocument.completion.completionItem.insertTextModeSupport.valueSet := Copy(insertTextModeSupportValues);
  end;

  // Capabilities/textDocument/completion/completionItemKind
  textDocument.completion.completionItemKind := TLSPCompletionItemKindValues.Create;
  if Assigned(completionItemKindValues) then
  begin
    textDocument.completion.completionItemKind.valueSet := Copy(completionItemKindValues);
  end
  else
  begin
    SetLength(textDocument.completion.completionItemKind.valueSet, 25);
    textDocument.completion.completionItemKind.valueSet := [TLSPCompletionItemKind.cText,
                                                           TLSPCompletionItemKind.cMethod,
                                                           TLSPCompletionItemKind.cFunction,
                                                           TLSPCompletionItemKind.cConstructor,
                                                           TLSPCompletionItemKind.cField,
                                                           TLSPCompletionItemKind.cVariable,
                                                           TLSPCompletionItemKind.cClass,
                                                           TLSPCompletionItemKind.cInterface,
                                                           TLSPCompletionItemKind.cModule,
                                                           TLSPCompletionItemKind.cProperty,
                                                           TLSPCompletionItemKind.cUnit,
                                                           TLSPCompletionItemKind.cValue,
                                                           TLSPCompletionItemKind.cEnum,
                                                           TLSPCompletionItemKind.cKeyword,
                                                           TLSPCompletionItemKind.cSnippet,
                                                           TLSPCompletionItemKind.cColor,
                                                           TLSPCompletionItemKind.cFile,
                                                           TLSPCompletionItemKind.cReference,
                                                           TLSPCompletionItemKind.cFolder,
                                                           TLSPCompletionItemKind.cEnumMember,
                                                           TLSPCompletionItemKind.cConstant,
                                                           TLSPCompletionItemKind.cStruct,
                                                           TLSPCompletionItemKind.cEvent,
                                                           TLSPCompletionItemKind.cOperator,
                                                           TLSPCompletionItemKind.cTypeParameter];
  end;

  textDocument.completion.contextSupport := contextSupport;
end;

procedure TLSPClientCapabilities.AddDeclarationSupport(const bLinkSupport: Boolean = False);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/declaration
  textDocument.declaration := TLSPDeclarationClientCapabilities.Create;
  textDocument.declaration.linkSupport := bLinkSupport;
end;

procedure TLSPClientCapabilities.AddDefinitionSupport(const bLinkSupport: Boolean = False);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/definition
  textDocument.definition := TLSPDefinitionClientCapabilities.Create;
  textDocument.definition.linkSupport := True;
end;

procedure TLSPClientCapabilities.AddDocumentHighlightsSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/documentHighlight
  textDocument.documentHighlight := TLSPDocumentHighlightClientCapabilities.Create;
  textDocument.documentHighlight.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddDocumentLinkSupport(const dynamicRegistration, toolTipSupport: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/documentLink
  textDocument.documentLink := TLSPDocumentLinkClientCapabilities.Create;
  textDocument.documentLink.dynamicRegistration := dynamicRegistration;
  textDocument.documentLink.tooltipSupport := toolTipSupport;
end;

procedure TLSPClientCapabilities.AddDocumentSymbolSupport(const hierarchicalSymbolSupport, dynamicRegistration,
    labelSupport: Boolean; const values: TArray<Integer> = nil; const tagSupport: TLSPtagSupport = nil);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/documentSymbol
  textDocument.documentSymbol := TLSPDocumentSymbolClientCapabilities.Create;
  textDocument.documentSymbol.dynamicRegistration := dynamicRegistration;
  textDocument.documentSymbol.labelSupport := labelSupport;

  // textDocument/documentSymbol/symbolKind
  textDocument.documentSymbol.symbolKind := TLSPSymbolKindValues.Create;
  if Assigned(values) then
  begin
    textDocument.documentSymbol.symbolKind.valueSet := Copy(values);
  end
  else
  begin
    SetLength(textDocument.documentSymbol.symbolKind.valueSet,26);
    textDocument.documentSymbol.symbolKind.valueSet := [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26];
  end;
  textDocument.documentSymbol.hierarchicalDocumentSymbolSupport := hierarchicalSymbolSupport;

  // tagSupport
  if Assigned(tagSupport) then
  begin
    textDocument.documentSymbol.tagSupport := TLSPTagSupport.Create;
    textDocument.documentSymbol.tagSupport.valueSet := Copy(tagSupport.valueSet);
  end;
end;

procedure TLSPClientCapabilities.AddFoldingRangeSupport(const dynamicRegistration: Boolean; const rangeLimit: Integer;
    const lineFoldingOnly: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/rangeFormatting
  textDocument.foldingRange := TLSPFoldingRangeClientCapabilities.Create;
  textDocument.foldingRange.dynamicRegistration := dynamicRegistration;
  textDocument.foldingRange.rangeLimit := rangeLimit;
  textDocument.foldingRange.lineFoldingOnly := lineFoldingOnly;
end;

procedure TLSPClientCapabilities.AddFormattingSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/formatting
  textDocument.formatting := TLSPDocumentFormattingClientCapabilities.Create;
  textDocument.formatting.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddGeneralMarkdown(const parser, version: string);
begin
  // general
  if not Assigned(general) then
    general := TLSPGeneralClientCapabilities.Create;

  // general/markdown
  general.markdown := TLSPMarkdownClientCapabilities.Create;

  general.markdown.parser := parser;
  general.markdown.version := version;
end;

procedure TLSPClientCapabilities.AddGeneralRegularExpressions(const engine, version: string);
begin
  // general
  if not Assigned(general) then
    general := TLSPGeneralClientCapabilities.Create;

  // general/regularExpressions
  general.regularExpressions := TLSPRegularExpressionsClientCapabilities.Create;

  general.regularExpressions.engine := engine;
  general.regularExpressions.version := version;
end;

procedure TLSPClientCapabilities.AddHoverSupport(const dynamicRegistration, contentFormatPlainText,
    contentFormatMarkdown: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/hover
  textDocument.hover := TLSPHoverClientCapabilities.Create;
  textDocument.hover.dynamicRegistration := dynamicRegistration;

  if contentFormatPlainText and contentFormatMarkdown then
  begin
    SetLength(textDocument.hover.contentFormat,2);
    textDocument.hover.contentFormat := ['markdown','plaintext'];
  end
  else if contentFormatPlainText then
  begin
    SetLength(textDocument.hover.contentFormat,1);
    textDocument.hover.contentFormat := ['plaintext'];
  end
  else if contentFormatMarkdown then
  begin
    SetLength(textDocument.hover.contentFormat,1);
    textDocument.hover.contentFormat := ['markdown'];
  end;
end;

procedure TLSPClientCapabilities.AddImplementationSupport(const bLinkSupport: Boolean = False);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/implementation
  textDocument.fimplementation := TLSPImplementationClientCapabilities.Create;
  textDocument.fimplementation.linkSupport := bLinkSupport;
end;

procedure TLSPClientCapabilities.AddLinkedEditingRangeSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/linkedEditingRange
  textDocument.linkedEditingRange := TLSPLinkedEditingRangeClientCapabilities.Create;
  textDocument.linkedEditingRange.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddMonikerSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/moniker
  textDocument.moniker := TLSPMonikerClientCapabilities.Create;
  textDocument.moniker.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddOnTypeFormattingSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/onTypeFormatting
  textDocument.onTypeFormatting := TLSPDocumentOnTypeFormattingClientCapabilities.Create;
  textDocument.onTypeFormatting.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddPublishDiagnosticsSupport(const relatedInformation, codeDescriptionSupport,
    versionSupport, dataSupport: Boolean; const tagSupport: TLSPtagSupportValues = nil);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/publishDiagnostics
  textDocument.publishDiagnostics := TLSPPublishDiagnosticsClientCapabilities.Create;

  if Assigned(tagSupport) then
  begin
    textDocument.publishDiagnostics.tagSupport := TLSPTagSupportValues.Create;
    textDocument.publishDiagnostics.tagSupport.valueSet := Copy(tagSupport.valueSet);
  end;

  textDocument.publishDiagnostics.relatedInformation := relatedInformation;
  textDocument.publishDiagnostics.codeDescriptionSupport := True;
  textDocument.publishDiagnostics.versionSupport := versionSupport;
  textDocument.publishDiagnostics.dataSupport := dataSupport;
end;

procedure TLSPClientCapabilities.AddRangeFormattingSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/rangeFormatting
  textDocument.rangeFormatting := TLSPDocumentRangeFormattingClientCapabilities.Create;
  textDocument.rangeFormatting.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddReferencesSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/references
  textDocument.references := TLSPReferenceClientCapabilities.Create;
  textDocument.references.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddRenameSupport(const dynamicRegistration, prepareSupport, honorsChangeAnnotations:
    Boolean; const prepareSupportDefaultBehaviorId: Integer = 0);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/rangeFormatting
  textDocument.rename := TLSPRenameClientCapabilities.Create;
  textDocument.rename.dynamicRegistration := dynamicRegistration;
  textDocument.rename.prepareSupport := prepareSupport;
  textDocument.rename.honorsChangeAnnotations := honorsChangeAnnotations;

  if prepareSupportDefaultBehaviorId > 0 then
  begin
    textDocument.rename.prepareSupportDefaultBehavior := TLSPPrepareSupportDefaultBehavior.Create;
    textDocument.rename.prepareSupportDefaultBehavior.Identifier := prepareSupportDefaultBehaviorId;
  end;
end;

procedure TLSPClientCapabilities.AddSelectionRangeSupport(const dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/selectionRange
  textDocument.selectionRange := TLSPSelectionRangeClientCapabilities.Create;
  textDocument.selectionRange.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddSemanticTokensSupport(const dynamicRegistration, overlappingTokenSupport,
    multilineTokenSupport, range: Boolean; const semanticToken: TLSPSemanticTokenTypes; const tokenTypes:
    TArray<String> = nil; const tokenModifiers: TArray<String> = nil; const formats: TArray<String> = nil);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/semanticTokens
  textDocument.semanticTokens := TLSPSemanticTokensClientCapabilities.Create;
  textDocument.semanticTokens.dynamicRegistration := dynamicRegistration;
  textDocument.semanticTokens.overlappingTokenSupport := overlappingTokenSupport;
  textDocument.semanticTokens.multilineTokenSupport := multilineTokenSupport;

  textDocument.semanticTokens.requests := TLSPRequests.Create;
  textDocument.semanticTokens.requests.semanticTokensType := semanticToken;
  textDocument.semanticTokens.requests.range := range;

  if Assigned(tokenTypes) then
    textDocument.semanticTokens.tokenTypes := Copy(tokenTypes);

  if Assigned(tokenModifiers) then
    textDocument.semanticTokens.tokenModifiers := Copy(tokenModifiers);

  if Assigned(formats) then
    textDocument.semanticTokens.formats := Copy(formats);
end;

procedure TLSPClientCapabilities.AddSignatureHelpSupport(const dynamicRegistration, contentFormatPlainText,
    contentFormatMarkdown, contextSupport, labelOffsetSupport: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/signatureHelp
  textDocument.signatureHelp := TLSPSignatureHelpClientCapabilities.Create;
  textDocument.signatureHelp.dynamicRegistration := dynamicRegistration;
  textDocument.signatureHelp.contextSupport := contextSupport;

  textDocument.signatureHelp.signatureInformation := TLSPClientSignatureInformation.Create;
  if contentFormatPlainText and contentFormatMarkdown then
  begin
    SetLength(textDocument.signatureHelp.signatureInformation.documentationFormat,2);
    textDocument.signatureHelp.signatureInformation.documentationFormat := ['plaintext', 'markdown'];
  end
  else if contentFormatPlainText then
  begin
    SetLength(textDocument.signatureHelp.signatureInformation.documentationFormat,1);
    textDocument.signatureHelp.signatureInformation.documentationFormat := ['plaintext'];
  end
  else if contentFormatMarkdown then
  begin
    SetLength(textDocument.signatureHelp.signatureInformation.documentationFormat,1);
    textDocument.signatureHelp.signatureInformation.documentationFormat := ['markdown'];
  end;

  if labelOffsetSupport then
  begin
    textDocument.signatureHelp.signatureInformation.parameterInformation := TLSPClientParameterInformation.Create;
    textDocument.signatureHelp.signatureInformation.parameterInformation.labelOffsetSupport := labelOffsetSupport;
  end;
end;

procedure TLSPClientCapabilities.AddSynchronizationSupport(const didSave, willSave, willSaveWaitUntil,
    dynamicRegistration: Boolean);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/synchronization
  textDocument.synchronization := TLSPTextDocumentSyncClientCapabilities.Create;
  textDocument.synchronization.dynamicRegistration := dynamicRegistration;
  textDocument.synchronization.willSave := willSave;
  textDocument.synchronization.didSave := didSave;
  textDocument.synchronization.willSaveWaitUntil := willSaveWaitUntil;
end;

procedure TLSPClientCapabilities.AddTypeDefinitionSupport(const bLinkSupport: Boolean = False);
begin
  // textDocument
  if not Assigned(textDocument) then
    textDocument := TLSPTextDocumentClientCapabilities.Create;

  // textDocument/typeDefinition
  textDocument.typeDefinition := TLSPTypeDefinitionClientCapabilities.Create;
  textDocument.typeDefinition.linkSupport := bLinkSupport;
end;

procedure TLSPClientCapabilities.AddWindowShowDocument;
begin
  // window
  if not Assigned(window) then
    window := TLSPWindow.Create;

  window.showDocument := TLSPShowDocumentClientCapabilities.Create;
  window.showDocument.support := True;
end;

procedure TLSPClientCapabilities.AddWindowShowMessage(const additionalPropertiesSupport: Boolean);
begin
  // window
  if not Assigned(window) then
    window := TLSPWindow.Create;

  window.showMessage := TLSPShowMessageRequestClientCapabilities.Create;
  window.showMessage.messageActionItem := TLSPMessageActionItem.Create;
  window.showMessage.messageActionItem.additionalPropertiesSupport := additionalPropertiesSupport;
end;

procedure TLSPClientCapabilities.AddWindowWorkDoneProgress(const value: Boolean = True);
begin
  // window
  if not Assigned(window) then
    window := TLSPWindow.Create;

  window.workDoneProgress := value;
end;

procedure TLSPClientCapabilities.AddWorkspaceCapabilities(const applyEdit, workspaceFolders, configuration: Boolean);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  workspace.applyEdit := applyEdit;
  workspace.workspaceFolders := workspaceFolders;
  workspace.configuration := configuration;
end;

procedure TLSPClientCapabilities.AddWorkspaceCodeLens(const refreshSupport: Boolean);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  // workspace/codeLens
  workspace.codeLens := TLSPCodeLensWorkspaceClientCapabilities.Create;
  workspace.codeLens.refreshSupport := refreshSupport;
end;

procedure TLSPClientCapabilities.AddWorkspaceDidChangeConfiguration(const dynamicRegistration: Boolean);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  // workspace/didChangeConfiguration
  workspace.didChangeConfiguration := TLSPDidChangeConfigurationClientCapabilities.Create;
  workspace.didChangeConfiguration.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddWorkspaceDidChangeWatchedFiles(const dynamicRegistration: Boolean);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  // workspace/didChangeWatchedFiles
  workspace.didChangeWatchedFiles := TLSPDidChangeWatchedFilesClientCapabilities.Create;
  workspace.didChangeWatchedFiles.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddWorkspaceEdit(const documentChanges, normalizeLineEndings: Boolean; const
    failureHandling: string = ''; const changeAnnotationSupport: Boolean = False; const changeAnnotationGroupsOnLabel:
    Boolean = False; const resourceOperations: TArray<TLSPResourceOperationKind> = nil);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  // workspace/workspaceEdit
  workspace.workspaceEdit := TLSPWorkspaceEditClientCapabilities.Create;
  workspace.workspaceEdit.documentChanges := documentChanges;
  workspace.workspaceEdit.failureHandling := failureHandling;
  workspace.workspaceEdit.normalizesLineEndings := normalizeLineEndings;

  if Assigned(resourceOperations) then
    workspace.workspaceEdit.resourceOperations := Copy(resourceOperations);

  if changeAnnotationSupport then
  begin
    workspace.workspaceEdit.changeAnnotationSupport := TLSPchangeAnnotationSupport.Create;
    workspace.workspaceEdit.changeAnnotationSupport.groupsOnLabel := changeAnnotationGroupsOnLabel;
  end;
end;

procedure TLSPClientCapabilities.AddWorkspaceExecuteCommand(const dynamicRegistration: Boolean);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  workspace.executeCommand := TLSPExecuteCommandClientCapabilities.Create;
  workspace.executeCommand.dynamicRegistration := dynamicRegistration;
end;

procedure TLSPClientCapabilities.AddWorkspaceFileOperations(const dynamicRegistration, didCreate, willCreate,
    didRename, willRename, didDelete, willDelete: Boolean);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  // workspace/semanticTokens
  workspace.fileOperations := TLSPFileOperations.Create;

  workspace.fileOperations.dynamicRegistration := dynamicRegistration;
  workspace.fileOperations.didCreate := didCreate;
  workspace.fileOperations.willCreate := willCreate;
  workspace.fileOperations.didRename := didRename;
  workspace.fileOperations.willRename := willRename;
  workspace.fileOperations.didDelete := didDelete;
  workspace.fileOperations.willDelete := willDelete;
end;

procedure TLSPClientCapabilities.AddWorkspaceSemanticTokens(const refreshSupport: Boolean);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  // workspace/semanticTokens
  workspace.semanticTokens := TLSPSemanticTokensWorkspaceClientCapabilities.Create;
  workspace.semanticTokens.refreshSupport := refreshSupport;
end;

procedure TLSPClientCapabilities.AddWorkspaceSymbol(const dynamicRegistration: Boolean; const symbolKindValues:
    TArray<Integer> = nil; const tagSupportValues: TArray<TLSPCompletionItemTag> = nil);
begin
  // workspace
  if not Assigned(workspace) then
    workspace := TLSPWorkspace.Create;

  // workspace/symbol
  workspace.symbol := TLSPWorkspaceSymbolClientCapabilities.Create;
  workspace.symbol.dynamicRegistration := dynamicRegistration;

  // workspace/symbol/symbolKind
  workspace.symbol.symbolKind := TLSPSymbolKindValues.Create;
  if Assigned(symbolKindValues) then
  begin
    workspace.symbol.symbolKind.valueSet := Copy(symbolKindValues);
  end
  else
  begin
    SetLength(workspace.symbol.symbolKind.valueSet,26);
    workspace.symbol.symbolKind.valueSet := [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26];
  end;

  // workspace/symbol/tagSupport
  if Assigned(tagSupportValues) then
  begin
    workspace.symbol.tagSupport := TLSPTagSupport.Create;
    workspace.symbol.tagSupport.valueSet := Copy(tagSupportValues);
  end;
end;

// TLSPWorkspace

constructor TLSPWorkspace.Create;
begin
  inherited;
  applyEdit := False;
  workspaceFolders := False;
  configuration := False;
end;

destructor TLSPWorkspace.Destroy;
begin
  FreeAndNil(workspaceEdit);
  FreeAndNil(didChangeConfiguration);
  FreeAndNil(didChangeWatchedFiles);
  FreeAndNil(symbol);
  FreeAndNil(executeCommand);
  FreeAndNil(semanticTokens);
  FreeAndNil(codeLens);
  FreeAndNil(fileOperations);
  inherited;
end;

destructor TLSPWorkspaceSymbolClientCapabilities.Destroy;
begin
  FreeAndNil(symbolKind);
  inherited;
end;

destructor TLSPSymbolKindValues.Destroy;
begin
  SetLength(valueSet,0);
  inherited;
end;

// TLSPWorkspaceEditClientCapabilities

constructor TLSPWorkspaceEditClientCapabilities.Create;
begin
  inherited;
  SetLength(resourceOperations,3);
  resourceOperations := ['create','rename','delete'];
  failureHandling := 'transactional';
  documentChanges := True;
end;

destructor TLSPWorkspaceEditClientCapabilities.Destroy;
begin
  SetLength(resourceOperations,0);
  inherited;
end;

// TLSPCodeActionKindValues

constructor TLSPCodeActionKindValues.Create;
begin
  inherited;
end;

destructor TLSPCodeActionKindValues.Destroy;
begin
  SetLength(valueSet,0);
  inherited;
end;

// TLSPCodeActionClientCapabilities

constructor TLSPCodeActionClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;
  isPreferredSupport := False;
end;

destructor TLSPCodeActionClientCapabilities.Destroy;
begin
  FreeAndNil(codeActionLiteralSupport);
  inherited;
end;

// TLSPDocumentSymbolClientCapabilities

constructor TLSPDocumentSymbolClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;
  hierarchicalDocumentSymbolSupport := False;
end;

destructor TLSPDocumentSymbolClientCapabilities.Destroy;
begin
  FreeAndNil(symbolKind);
  inherited;
end;

// TLSPFoldingRangeClientCapabilities

constructor TLSPFoldingRangeClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;
  rangeLimit := 0;
  lineFoldingOnly := True;
end;

// TLSPPublishDiagnosticsClientCapabilities

constructor TLSPPublishDiagnosticsClientCapabilities.Create;
begin
  inherited;
  relatedInformation := False;
  versionSupport := False;
end;

destructor TLSPPublishDiagnosticsClientCapabilities.Destroy;
begin
  FreeAndNil(tagSupport);
  inherited;
end;

// TLSPHoverClientCapabilities

constructor TLSPHoverClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;
end;

destructor TLSPHoverClientCapabilities.Destroy;
begin
  SetLength(contentFormat,0);
  inherited;
end;

// TLSPCompletionClientCapabilities

constructor TLSPCompletionClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;
  contextSupport := True;
end;

destructor TLSPCompletionClientCapabilities.Destroy;
begin
  FreeAndNil(completionItem);
  FreeAndNil(completionItemKind);
  inherited;
end;

// TLSPClientCompletionItem

constructor TLSPClientCompletionItem.Create;
begin
  inherited;
  snippetSupport := True;
  commitCharactersSupport := True;
  deprecatedSupport := True;
  preselectSupport := True;

  SetLength(documentationFormat,1);
  documentationFormat := ['plaintext'];
end;

destructor TLSPClientCompletionItem.Destroy;
begin
  SetLength(documentationFormat,0);
  FreeAndNil(tagSupport);
  inherited;
end;

// TLSPSignatureHelpClientCapabilities

constructor TLSPSignatureHelpClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;
  contextSupport := False;
end;

destructor TLSPSignatureHelpClientCapabilities.Destroy;
begin
  FreeAndNil(signatureInformation);
  inherited;
end;

// TLSPClientSignatureInformation

constructor TLSPClientSignatureInformation.Create;
begin
  inherited;
  SetLength(documentationFormat,1);
  documentationFormat := ['plaintext'];
end;

destructor TLSPClientSignatureInformation.Destroy;
begin
  SetLength(documentationFormat,0);
  FreeAndNil(parameterInformation);
  inherited;
end;

// TLSPTextDocumentClientCapabilities

constructor TLSPTextDocumentClientCapabilities.Create;
begin
  inherited;
end;

destructor TLSPTextDocumentClientCapabilities.Destroy;
begin
  inherited;
  FreeAndNil(synchronization);
  FreeAndNil(completion);
  FreeAndNil(hover);
  FreeAndNil(signatureHelp);
  FreeAndNil(declaration);
  FreeAndNil(definition);
  FreeAndNil(typeDefinition);
  FreeAndNil(fimplementation);
  FreeAndNil(references);
  FreeAndNil(documentHighlight);
  FreeAndNil(documentSymbol);
  FreeAndNil(codeAction);
  FreeAndNil(codeLens);
  FreeAndNil(documentLink);
  FreeAndNil(colorProvider);
  FreeAndNil(formatting);
  FreeAndNil(rangeFormatting);
  FreeAndNil(onTypeFormatting);
  FreeAndNil(rename);
  FreeAndNil(publishDiagnostics);
  FreeAndNil(foldingRange);
  FreeAndNil(selectionRange);
  FreeAndNil(linkedEditingRange);
  FreeAndNil(callHierarchy);
  FreeAndNil(semanticTokens);
  FreeAndNil(moniker);
end;

destructor TLSPCompletionItemKindValues.Destroy;
begin
  SetLength(valueSet,0);
  inherited;
end;

// TLSPTextDocumentSyncClientCapabilities

constructor TLSPTextDocumentSyncClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;
  willSave := False;
  willSaveWaitUntil := False;
  didSave := True;
end;

// TLSPClientParameterInformation

constructor TLSPClientParameterInformation.Create;
begin
  inherited;
  labelOffsetSupport := True;
end;

// TLSPTagSupportValues

constructor TLSPTagSupportValues.Create;
begin
  inherited;
end;

destructor TLSPTagSupportValues.Destroy;
begin
  SetLength(valueSet,0);
  inherited;
end;

// TLSPTagSupport

constructor TLSPTagSupport.Create;
begin
  inherited;
end;

destructor TLSPTagSupport.Destroy;
begin
  SetLength(valueSet,0);
  inherited;
end;

// TLSPSemanticTokensClientCapabilities

constructor TLSPSemanticTokensClientCapabilities.Create;
begin
  inherited;
  dynamicRegistration := False;

  SetLength(tokenTypes,0);

  SetLength(tokenModifiers,0);

  // The formats the clients supports.
  SetLength(formats,0);

  // Whether the client supports tokens that can overlap each other.
  overlappingTokenSupport := False;

  // Whether the client supports tokens that can span multiple lines.
  multilineTokenSupport := False;
end;

destructor TLSPSemanticTokensClientCapabilities.Destroy;
begin
  FreeAndNil(requests);
  SetLength(tokenTypes,0);
  SetLength(tokenModifiers,0);
  inherited;
end;

constructor TLSPWorkDoneProgressBegin.Create;
begin
  inherited;
  kind := 'begin';
end;

{ TLSPWorkDoneProgressReport }

constructor TLSPWorkDoneProgressReport.Create;
begin
  inherited;
  kind := 'report';
end;

constructor WorkDoneProgressEnd.Create;
begin
  inherited;
  kind := 'end';
end;

constructor TLSPProgressParams.Create;
begin
  inherited;
  value := TLSPWorkDoneProgressValue.Create;
end;

destructor TLSPProgressParams.Destroy;
begin
  FreeAndNil(value);
  inherited;
end;

constructor TLSPCreateFile.Create;
begin
  inherited;
  kind := 'create';
end;

constructor TLSPRenameFile.Create;
begin
  inherited;
  kind := 'rename';
end;

constructor TLSPDeleteFile.Create;
begin
  inherited;
  kind := 'delete';
end;

constructor TLSPApplyWorkspaceEditParams.Create;
begin
  inherited;
  edit := TLSPWorkspaceEdit.Create;
end;

destructor TLSPApplyWorkspaceEditParams.Destroy;
begin
  FreeAndNil(edit);
  inherited;
end;

constructor TLSPCompletionList.Create;
begin
  inherited;
  items := TObjectList<TLSPCompletionItem>.Create;
end;

destructor TLSPCompletionList.Destroy;
begin
  items.Free;
  inherited;
end;

destructor TLSPCodeActionLiteralSupport.Destroy;
begin
  FreeAndNil(codeActionKind);
  inherited;
end;

constructor TLSPCodeActionResponse.Create;
begin
  inherited;
  codeActions := TObjectList<TLSPCodeAction>.Create;
end;

destructor TLSPCodeActionResponse.Destroy;
begin
  FreeAndNil(codeActions);
  inherited;
end;

constructor TLSPCodeLensResponse.Create;
begin
  inherited;
  codeLensList := TObjectList<TLSPCodeLens>.Create;
end;

destructor TLSPCodeLensResponse.Destroy;
begin
  FreeAndNil(codeLensList);
  inherited;
end;

constructor TLSPDocumentLinkResponse.Create;
begin
  inherited;
  documentLinks := TObjectList<TLSPDocumentLink>.Create;
end;

destructor TLSPDocumentLinkResponse.Destroy;
begin
  FreeAndNil(documentLinks);
  inherited;
end;

constructor TLSPSelectionRangeResponse.Create;
begin
  inherited;
  selRanges := TObjectList<TLSPSelectionRange>.Create;
end;

destructor TLSPSelectionRangeResponse.Destroy;
begin
  FreeAndNil(selRanges);
  inherited;
end;

destructor TLSPWindow.Destroy;
begin
  FreeAndNil(showMessage);
  FreeAndNil(showDocument);
  inherited;
end;

destructor TLSPRenameClientCapabilities.Destroy;
begin
  FreeAndNil(prepareSupportDefaultBehavior);
  inherited;
end;

destructor TLSPShowMessageRequestClientCapabilities.Destroy;
begin
  FreeAndNil(messageActionItem);
  inherited;
end;

destructor TLSPGeneralClientCapabilities.Destroy;
begin
  FreeAndNil(regularExpressions);
  FreeAndNil(markdown);
  inherited;
end;

constructor TLSPDidChangeTextDocumentParams.Create;
begin
  inherited;
  contentChanges := TObjectList<TLSPBaseTextDocumentContentChangeEvent>.Create;
end;

destructor TLSPDidChangeTextDocumentParams.Destroy;
begin
  FreeAndNil(contentChanges);
  inherited;
end;

constructor TLSPCompletionItem.Create;
begin
  inherited;
  additionalTextEdits := TObjectList<TLSPTextEdit>.Create;
end;

destructor TLSPCompletionItem.Destroy;
begin
  additionalTextEdits.Free;
  inherited;
end;

destructor TLSPServerCapabilitiesFileOperations.Destroy;
begin
  FreeAndNil(dynamicRegistration);
  FreeAndNil(didCreate);
  FreeAndNil(willCreate);
  FreeAndNil(didRename);
  FreeAndNil(willRename);
  FreeAndNil(didDelete);
  FreeAndNil(willDelete);
  inherited;
end;

end.

