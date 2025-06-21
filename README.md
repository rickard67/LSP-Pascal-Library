# Delphi/Pascal LSP Client V1.x

A language server protocol client written in Pascal (Delphi).

This version is compatible with older Delphi versions (as well as new ones).


# Content

- [Basic Information](#basic-information)
- [Demo](#demo)
- [Using the Library](#using-the-library)
   - [Running the LSP server](#running-the-lsp-server)
   - [Send requests and notifications to the server](#send-requests-and-notifications-to-the-server)
   - [Notifications or responces sent from the server](#notifications-or-responces-sent-from-the-server)
- [Initialize](#initialize)
- [Register/Unregister Capability](#registerunregister-capability)
- [Closing or exiting the server](#closing-or-exiting-the-server)
- [Text Document Synchronization](#text-document-synchronization)
   - [DidOpenTextDocument Notification](#didopentextdocument-notification)
   - [DidChangeTextDocument Notification](#didchangetextdocument-notification)
   - [DidCloseTextDocument Notification](#didclosetextdocument-notification)
   - [DidSaveTextDocument Notification](#didsavetextdocument-notification)
   - [WillSaveTextDocument Notification](#willsavetextdocument-notification)
   - [WillSaveWaitUntilTextDocument Request](#willsavewaituntiltextdocument-request)
- [Notebook Document Synchronization](#notebook-document-synchronization)
   - [DidOpenNotebookDocument Notification](#didopennotebookdocument-notification)
   - [DidChangeNotebookDocument Notification](#didchangenotebookdocument-notification)
   - [DidSaveNotebookDocument Notification](#didsavenotebookdocument-notification)
   - [DidCloseNotebookDocument Notification](#didclosenotebookdocument-notification)
- [Language Features](#language-features)
   - [Completion Request](#completion-request)
      - [Completion Item Resolve Request](#completion-item-resolve-request)
   - [Hover request](#hover-request)
   - [Signature Help Request](#signature-help-request)
   - [Publish Diagnostics Notification](#publish-diagnostics-notification)
   - [Pull Diagnostics](#pull-diagnostics)
      - [Document Diagnostics](#document-diagnostics)
      - [Workspace Diagnostics](#workspace-diagnostics)
      - [Workspace Diagnostic Refresh Request](#workspace-diagnostic-refresh-request)
   - [Goto Requests](#goto-requests)
      - [Goto Declaration Request](#goto-declaration-request)
      - [Goto Definition Request](#goto-definition-request)
      - [Goto Implementation Request](#goto-implementation-request)
      - [Goto Type Definition Request](#goto-type-definition-request)
   - [Find References Request](#find-references-request)
   - [Document Highlight Request](#document-highlight-request)
   - [Document Symbols Request](#document-symbols-request)
   - [Code Action Request](#code-action-request)
      - [Code Action Resolve Request](#code-action-resolve-request)
   - [Code Lens Request](#code-lens-request)
      - [Code Lens Resolve Request](#code-lens-resolve-request)
      - [Code Lens Refresh Request](#code-lens-refresh-request)
   - [Document Link Request](#document-link-request)
      - [Document Link Resolve Request](#document-link-resolve-request)
   - [Document Color Request](#document-color-request)
   - [Color Presentation Request](#color-presentation-request)
   - [Document Formatting Request](#document-formatting-request)
   - [Document Range Formatting Request](#document-range-formatting-request)
   - [Document OnTypeFormatting Request](#document-ontypeformatting-request)
   - [Folding Range Request](#folding-range-request)
   - [Selection Range Request](#selection-range-request)
   - [Prepare Call Hierarchy Request](#prepare-call-hierarchy-request)
   - [Call Hierarchy Incomming Calls](#call-hierarchy-incomming-calls)
   - [Call Hierarchy Outgoing Calls](#call-hierarchy-outgoing-calls)
   - [Prepare Type Hierarchy Request](#prepare-type-hierarchy-request)
      - [Type Hierarchy Supertypes](#type-hierarchy-supertypes)
      - [Type Hierarchy Subtypes](#type-hierarchy-subtypes)
   - [Semantic Tokens](#semantic-tokens)
      - [Semantic Tokens for a whole file](#semantic-tokens-for-a-whole-file)
      - [Semantic Tokens delta for a whole file](#semantic-tokens-delta-for-a-whole-file)
      - [Semantic Tokens for a range](#semantic-tokens-for-a-range)
      - [Semantic Tokens Refresh](#semantic-tokens-refresh)
   - [Linked Editing Range](#linked-editing-range)
   - [Monikers](#monikers)
   - [Inlay Hint Request](#inlay-hint-request)
      - [Inlay Hint Resolve Request](#inlay-hint-resolve-request)
      - [Inlay Hint Refresh Request](#inlay-hint-refresh-request)
   - [Inline Value Request](#inline-value-request)
      - [Inline Value Refresh Request](#inline-value-refresh-request)
- [Workspace Features](#workspace-features)
   - [Execute Command Request](#execute-command-request)
   - [DidChangeWorkspaceFolders Notification](#didchangeworkspacefolders-notification)
   - [DidChangeConfiguration Notification](#didchangeconfiguration-notification)
   - [DidChangeWatchedFiles Notification](#didchangewatchedfiles-notification)
   - [DidCreateFiles Notification](#didcreatefiles-notification)
   - [DidDeleteFiles Notification](#diddeletefiles-notification)
   - [DidRenameFiles Notification](#didrenamefiles-notification)
   - [WillCreateFiles request](#willcreatefiles-request)
   - [WillDeleteFiles request](#willdeletefiles-request)
   - [WillRenameFiles request](#willrenamefiles-request)
   - [Workspace Symbols Request](#workspace-symbols-request)
   - [Rename Request](#rename-request)
   - [Prepare Rename Request](#prepare-rename-request)
   - [OnConfiguration](#onconfiguration)
   - [OnProgress](#onprogress)
   - [OnWorkspaceApplyEdit](#onworkspaceapplyedit)
- [Language identifiers](#language-identifiers)

## Basic Information

The LSP client was written to make communication with language servers easier. Use
the client to read or send notifications and requests to and from the server. Handle
the notifications and request from the server using events.

The client also handles client/registerCapability request sent from the server to
the client to register for a new capability on the client side.

This component was created for use in RJ TextEd to add language server support. It has been
tested with several language servers (https://www.rj-texted.se/Forum/viewforum.php?f=23).

The client component support both stdio and tcp/ip socket communication. When communicating
over a socket - the client act as a server, which is why you must add a port as an argument
when starting the server. E.g. server.exe --port=5000.

The component handle all features found in the 3.17.x version of the language server
protocol.

## Demo

Included is a simple demo to demonstrate how to use the client.

To run you need to install the PHP Intelephense language server.

- Install Nodejs for Windows https://nodejs.org/en/download/
- Install Intelephense by open a command prompt and type:

``` >npm install intelephense -g ```

Run the demo and open settings. Make sure it says something like below:

##### LSP Server
intelephense.cmd

##### Arguments
--stdio

##### Initial dir
%appdata%\npm

##### LSP root path
..\LSP-Pascal-Library\demo\examples\Class Member

## Using the Library

Add one or more client components to a form or create clients at runtime. You need one client
for each LSP server you intend to run.

### Running the LSP server

Use the function below to run a server.

``` LSPClient1.RunServer(const ACommandline, ADir: string); ```


### Send requests and notifications to the server

Use the function "SendRequest" to send a request or notification to the server. The first
argument indicate the type of request and automatically set the "method" in the Json
request that is sent to the server.

The function is declared as:

```
procedure TLSPClient.SendRequest(const lspKind: TLSPKind; const method: string = ''; 
    const params: TLSPBaseParams = nil; const paramJSON: string = '');
```

### Notifications or responces sent from the server
All responces and notifications are handled as events. You can check for an error
in the event. Error codes are stored in TLSPErrorCodes as enumerated types values,
as seen below.

```
// The event catches the response from the server
FLSPClient1.OnOnDocumentDiagnostic := OnDocumentDiagnostic1;

procedure OnOnDocumentDiagnostic1(Sender: TObject; const kind: string; const resultId: string; 
    const items: TArray<TLSPDiagnostic>; const errorCode: Integer; const errorMessage: string;
    const retriggerRequest: Boolean);
begin
  if kind = 'unchanged' then Exit;
  
  if (ErrorCode = Integer(TLSPErrorCodes.ServerCancelled)) and retriggerRequest then
  begin
    // Re-trigger the document diagnostic request
    ...
  end;
  
  ...
end;
``` 

## Initialize

To communicate with the server you must first send an initialize request. The request 
should contain the client capabilities, letting the server know what types of requests
and notifications the client can handle.

You should always handle the following events in your application.

``` 
  LSPClient1.OnInitialize := OnInitialize1;
  LSPClient1.OnInitialized := OnInitialized1;
```
 
You set the client capabilities in the OnInitialize1 event.

Send the initialize request to the server:

```
LSPClient1.SendRequest(lspInitialize);

procedure OnInitialize1(Sender: TObject; var value: TLSPInitializeParams);
begin
  // Set only the options your client can handle.
  
  // Set root path and uri
  value.AddRoot(FSourcePath);
  
  // Workspace folders
  value.AddWorkspaceFolders(FStringlist);

  // Client Capabilities
  
  // Capabilities/textDocument/synchronization
  value.capabilities.AddSynchronizationSupport(True,True,False,False);
  
  // Capabilities/textDocument/completion
  value.capabilities.AddCompletionSupport(False,True,False,True,True,False,False,False,False);

  // Capabilities/textDocument/hover
  value.capabilities.AddHoverSupport(False,True,True);

  // Capabilities/textDocument/signatureHelp
  value.capabilities.AddSignatureHelpSupport(False,True,True,False,False);
  
  // Capabilities/textDocument/publishDiagnostics
  value.capabilities.AddPublishDiagnosticsSupport(True,False,False,False);

  // Process id
  value.processId := GetCurrentProcessId;
  
  ...
  
end;
```

The OnInitialized1 event is triggered after the server has responded to the initialize
request. The server capabilities displayed in the OnInitialized1 event are stored for 
you in LSPClient1.ServerCapablitities.

You should check LSPClient1.ServerCapablitities before sending requests and notifications
to make sure the server can handle them.

E.g. in a SendWillSaveNotification() function you could add
``` 
if not LClient.IsRequestSupported(lspWillSaveTextDocument) then Exit;

// Send request
...
```

## Register/Unregister Capability

The client/registerCapability request is sent from the server to the client to register
for a new capability on the client side. Not all clients need to support dynamic capability
registration. A client opts in via the dynamicRegistration property on the specific client
capabilities.

Server must not register the same capability both statically through the initialize
result and dynamically for the same document selector. If a server wants to support
both static and dynamic registration it needs to check the client capability in the
initialize request and only register the capability statically if the client doesn’t
support dynamic registration for that capability.

Register/UnRegister capability requests are stored in the LSP client, but can be
handled by the program using events.

Use the method below to find a registered capability. 
```
FindDynamicCapability(const method: string): TLSPTextDocumentRegistrationOptions
```
Test if the returned object is of the correct type
and typecast it (see below).

``` 
var
  i: Integer;
  watcher: TLSPFileSystemWatcher;
  dynOptions: TLSPTextDocumentRegistrationOptions;
  changeOptions: TLSPDidChangeWatchedFilesRegistrationOptions;
begin
  // Find capability for 'workspace/didChangeWatchedFiles' 
  dynOptions := FLSPClient.FindDynamicCapability('workspace/didChangeWatchedFiles');
  
  // If dynOptions is of the correct type - proceed
  if dynOptions is TLSPDidChangeWatchedFilesRegistrationOptions then
  begin
    // Typecast dynOptions
    changeOptions := dynOptions as TLSPDidChangeWatchedFilesRegistrationOptions;
    
    // The server want the program to send a 'workspace/didChangeWatchedFiles' notification
    // when the client detects changes to files and folders watched by the language server.
    // 
    // Find out if we should send a notification to the server by checking the "watchers" array.
    for i := 0 to Length(changeOptions.watchers) - 1 do
    begin
      watcher := changeOptions.watchers[i];
      if watcher.kind and TLSPWatchKind.Delete then
      begin
        // Do our deleted file match the pattern
        if MatchesMask(szOurDeletedFile, watcher.globPattern.pattern) then
        begin
          // Send a lspDidChangeWatchedFiles notification to the server with the
          // uri to the file and type as "Deleted".
          ...
        end;
      end;
    end;    
  end;
end;
```

It is possible to handle "register capability requests" inside the program directly.

```
FLSPClient.OnRegisterCapability := OnRegisterCapability1;
FLSPClient.OnUnRegisterCapability := OnUnRegisterCapability1;

procedure OnRegisterCapability1(Sender: TObject; const values: TLSPRegistrations; var errorCode: Integer; 
    var errorMessage: string);
var
  s: string;
  id: string;
  kind: TLSPKind;
  registerOptions: TLSPTextDocumentRegistrationOptions;
begin
  // Handle capabilities the server want to register
  for i := 0 to Lenght(values[i]) - 1 do
  begin
    id := values[i].id;
    s := values[i].method;
    kind := (Sender as TLSPClient).LSPKindFromMethod(s);
    
    // Store the id so it's possible to unregister the capability using the same id.
    case kind of
      lspDidChangeTextDocument: begin
        // Register sync kind dynamically.
        registerOptions := values[i].registerOptions;
        ResisterSyncKind(id,
                         TLSPTextDocumentChangeRegistrationOptions(registerOptions).syncKind,
                         TLSPTextDocumentChangeRegistrationOptions(registerOptions).documentSelector);
      end;
      lspWillSaveWaitUntilTextDocument: begin
        // The server want the client to send lspWillSaveWaitUntilTextDocument requests
        // before the document is actually saved.
        ResisterWillSaveWaitUntilTextDocument(id,values[i].registerOptions.documentSelector);
      end;
    end;
  end;
end;

procedure OnRegisterCapability1(Sender: TObject; const values: TLSPUnRegistrations; var errorCode: Integer; 
    var errorMessage: string; var Handled: Boolean);
begin
  ...
end;
```

## Closing or exiting the server

You can close the server by manually send a shutdown request and then an exit notification.
But then you need to handle situations where the server may not respond to your
request.

The LSPClient1.CloseServer function handles that for you. If the server does not respond properly
the client will close the server for you. You can also use LSPClient1.ExitServer(True)
if you are in a hurry and want to close the server down quickly.

``` 
// Close the server gracefully
LSPClient.CloseServer;

// Exit the server quickly
LSPClient.ExitServer(True);
```

### Shutdown Request

The shutdown request is sent from the client to the server. It asks the server to 
shut down, but to not exit (otherwise the response might not be delivered correctly 
to the client). 

The correct way to close a server is to first send a shutdown request and in the
OnShutdown event - send an exit notification. But you can skip the shutdown request
and only send the exit notification. The return code from the server should then
be 1.

```
FLSPClient.SendRequest(lspShutdown); (or FLSPClient1.CloseServer;)

procedure OnShutdown(Sender: TObject; const errorCode: Integer; const errorMessage: string);
begin
  if errorMessage <> '' then
  begin
    StatusMemo.Lines.Add('ERROR: ' + errorMessage);
    StatusMemo.Lines.Add('Trying to force the server to close!');
    (Sender as TLSPClient).ExitServer(False);
    Exit;
  end;

  // Shutdown was successful. Send exit request to close the server.
  (Sender as TLSPClient).SendRequest(lspExit);
end;
```


### Exit Notification

A notification to ask the server to exit its process.
The return code from the server should be 0, if you first sent a shutdown request.
Otherwise it should return 1.

```
FLSPClient.SendRequest(lspExit); (or LSPClient.ExitServer(True);)

procedure OnExit(Sender: TObject; exitCode: Integer; const bRestartServer: Boolean);
var
  s: string;
begin
  s := 'The server has closed with exit code: ' + IntToStr(exitCode);
  StatusMemo.Lines.Add(s);
  
  // If the server crashed we could restart it
  if ((exitCode <> 0) or bRestartServer) and (FLSPClient.GetRunTimeInSeconds > 20) then
    RestartLSPServer;
end;

```

## Text Document Synchronization

Client support for textDocument/didOpen, textDocument/didChange and textDocument/didClose
notifications is mandatory in the protocol and clients can not opt out supporting them.

### DidOpenTextDocument Notification

The document open notification is sent from the client to the server to signal newly
opened text documents. Some features may not work unless you send a didOpen
notification. 

The client must send didOpen, didChange and didClose notifications to the server. It
should send didSave (and maybe willSave if the server wants it) as well.

```
var
  params: TLSPDidOpenTextDocumentParams;
begin
  params := TLSPDidOpenTextDocumentParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.php');
  params.textDocument.version := 1;
  params.textDocument.languageId := 'php';
  params.textDocument.text := Memo1.text;

  FLSPClient1.SendRequest(lspDidOpenTextDocument, '', params);
 ```
 Language identifiers are listed at the end of this document.
 
1. A language server can handle several different languageid values. E.g. 'typescript' and 'javascript'. You could connect a file extension to a language id.
2. Version should be an incremental value that increse with every change (didChangeTextDocument).
 
### DidChangeTextDocument Notification

The document change notification is sent from the client to the server to signal changes
to a text document. Before a client can change a text document it must claim ownership of 
its content using the textDocument/didOpen notification above.

```
var
  ext: string;
  syncKind: Integer;
  params: TLSPDidChangeTextDocumentParams;
begin
  ext := '.cpp';  
  syncKind := FLSPClient1.GetSyncKind(ext);
  if synckind = TLSPTextDocumentSyncKindRec.None then Exit;
  
  params := TLSPDidChangeTextDocumentParams.Create;
  params.textDocument := TLSPVersionedTextDocumentIdentifier.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.textDocument.version := version + 1;
  Inc(version);
    
  // Set changes
  if syncKind = TLSPTextDocumentSyncKindRec.Incremental then
  begin
    // Go through our list of changes (MemoChanges: TList<TLSPTextDocumentContentChangeEvent>)
    for i := 0 to AMemo.MemoChanges.Count - 1 do
      params.contentChanges.Add(AMemo.MemoChanges[i]);
  end
  else if syncKind = TLSPTextDocumentSyncKindRec.Full then
  begin
    content := TLSPBaseTextDocumentContentChangeEvent.Create;
    content.text := AMemo.Lines.Text;
    params.contentChanges.Add(content);
  end;
  
  FLSPClient1.SendRequest(lspDidChangeTextDocument, '', params);
end;
```

### DidCloseTextDocument Notification

The document close notification is sent from the client to the server when the document 
got closed in the client. The document’s master now exists where the document’s Uri 
points to.

```
var
  params: TLSPDidCloseTextDocumentParams;
begin
  params := TLSPDidCloseTextDocumentParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.c');
  
  LClient.SendRequest(lspDidCloseTextDocument, '', params); 
end;
```

### DidSaveTextDocument Notification

The document save notification is sent from the client to the server when the document 
was saved in the client.

```
var
  params: TLSPDidSaveTextDocumentParams;
begin
  if not LClient.IsRequestSupported(lspDidSaveTextDocument) then Exit;
  
  params := TLSPDidSaveTextDocumentParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.c');
  
  // Optionally include the content when saved. Depends on the includeText option value.
  if LClient.IncludeText(lspDidSaveTextDocument, sz, False) then
  begin
    params.text := Memo1.Lines.Text;
  end;
  
  LClient.SendRequest(lspDidSaveTextDocument, '', params); 
end;
```

### WillSaveTextDocument Notification

The document will save notification is sent from the client to the server before the 
document is actually saved.

```
var
  i: Integer;
  params: TLSPWillSaveTextDocumentParams;
begin
  if not LClient.IsRequestSupported(lspWillSaveTextDocument) then Exit;
  
  params := TLSPWillSaveTextDocumentParams.Create;

  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.reason := 1;
  
  LClient.SendRequest(lspWillSaveTextDocument, '', params);
end;
```

The reason value can take 3 different values.
```
1 = Manually triggered, e.g. by the user pressing save, by starting debugging, or by an API call.
2 = Automatic after a delay.
3 = When the editor lost focus.
```

### WillSaveWaitUntilTextDocument Request

The document will save request is sent from the client to the server before the document 
is actually saved. The request can return an array of TextEdits which will be applied to 
the text document before it is saved.

```
FLSPClient1.OnWillSaveWaitUntilTextDocument := OnWillSaveWaitUntilTextDocument1;

var
  i: Integer;
  params: TLSPWillSaveTextDocumentParams;
begin
  if not LClient.IsRequestSupported(lspWillSaveWaitUntilTextDocument) then Exit;
  
  params := TLSPWillSaveTextDocumentParams.Create;

  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.reason := 1;
  
  LClient.SendRequest(lspWillSaveWaitUntilTextDocument, '', params);
end;

procedure OnWillSaveWaitUntilTextDocument1(Sender: TObject; const value: TLSPWorkspaceEdit);
begin
  if Assigned(value) then
  begin
    // Process value object. See OnWorkspaceApplyEdit on how to process 
    // the TLSPTextEdit object.
    ...
  end;
end;
```

The reason value can take 3 different values.
```
1 = Manually triggered, e.g. by the user pressing save, by starting debugging, or by an API call.
2 = Automatic after a delay.
3 = When the editor lost focus.
```

## Notebook Document Synchronization

A NOTEBOOK file contains a notebook created by SMART Notebook software, which allows
e.g. teachers to create dynamic classroom lecture materials. It may store notes, diagrams,
images, audio, and video. NOTEBOOK files can be used for storing and sharing digital
lesson notes with teachers and students.

- A notebook document a collection of notebook cells typically stored in a file. A notebook document has a type and can be uniquely identified using a resource URI.
- A notebook cell holds the actual text content. Cells have a kind (either code or markdown). The actual text content of the cell is stored in a text document.

### DidOpenNotebookDocument Notification

The open notification is sent from the client to the server when a notebook document is
opened. It is only sent by a client if the server requested the 'notebook' synchronization mode 
in its notebookDocumentSync capability. Otherwise a standard textDocument/didOpen notification
should be sent.

```
var
  params: TLSPDidOpenNotebookDocumentParams;
  cell: TLSPNotebookCell;
begin
  if not LClient.IsRequestSupported(lspDidOpenNotebookDocument) then Exit;
  params := TLSPDidOpenNotebookDocumentParams.Create;
  params.notebookDocument := TLSPNotebookDocument.Create;
  params.notebookDocument.uri := FilePathToUri('c:\source\foo.notebook');
  params.notebookDocument.version := 1;
  
  SetLength(params.notebookDocument.cells,2);
  cell.kind := 2;
  cell.document := FilePathToUri('c:\source\hello.py');
  params.notebookDocument.cells[0] := cell;
  
  cell.kind := 2;
  cell.document := FilePathToUri('c:\source\hi.py');
  params.notebookDocument.cells[1] := cell;

  FLSPClient1.SendRequest(lspDidOpenNotebookDocument, '', params);
 ```
 
### DidChangeNotebookDocument Notification

The change notification is sent from the client to the server when a notebook document
changes. It is only sent by a client if the server requested the 'notebook' synchronization mode
in its notebookDocumentSync capability. Otherwise a standard textDocument/didChange notification
should be sent.

```
var
  ext: string;
  syncKind: Integer;
  params: TLSPDidChangeNotebookDocumentParams;
begin
  if not LClient.IsRequestSupported(lspDidChangeNotebookDocument) then Exit;
  ext := '.notebook';  
  syncKind := FLSPClient1.GetSyncKind(ext);
  if synckind = TLSPTextDocumentSyncKindRec.None then Exit;
  
  params := TLSPDidChangeNotebookDocumentParams.Create;
  params.notebookDocument.uri := FilePathToUri('c:\source\foo.notebook');
  params.notebookDocument.version := version + 1;
  Inc(version);
    
  // Set changes
  if syncKind = TLSPTextDocumentSyncKindRec.Incremental then
  begin
    // Go through our list of changes (MemoChanges: TList<TLSPTextDocumentContentChangeEvent>)
    for i := 0 to AMemo.MemoChanges.Count - 1 do
      params.change.cells.textContent.changes.Add(AMemo.MemoChanges[i]);
  end
  else if syncKind = TLSPTextDocumentSyncKindRec.Full then
  begin
    content := TLSPBaseTextDocumentContentChangeEvent.Create;
    content.text := AMemo.Lines.Text;
    params.change.cells.textContent.changes.Add(content);
  end;
  
  FLSPClient1.SendRequest(lspDidChangeNotebookDocument, '', params);
end;
```

### DidSaveNotebookDocument Notification

The save notification is sent from the client to the server when a notebook document
is saved. It is only sent by a client if the server requested the 'notebook' synchronization mode
in its notebookDocumentSync capability. Otherwise a standard textDocument/didSave notification
should be sent.

```
var
  params: TLSPDidSaveNotebookDocumentParams;
begin
  params := TLSPDidSaveNotebookDocumentParams.Create;
  params.notebookDocument.uri := FilePathToUri('c:\source\foo.notebook');
  
  LClient.SendRequest(lspDidSaveNotebookDocument, '', params); 
end;
```

### DidCloseNotebookDocument Notification

The close notification is sent from the client to the server when a notebook document
is closed. It is only sent by a client if the server requested the 'notebook' synchronization mode
in its notebookDocumentSync capability. Otherwise a standard textDocument/didClose notification
should be sent.

```
var
  params: TLSPDidCloseNotebookDocumentParams;
begin
  params := TLSPDidCloseNotebookDocumentParams.Create;
  params.notebookDocument.uri := FilePathToUri('c:\source\foo.notebook');
  
  // The text documents that represent the content
  // of a notebook cell that got closed.
  SetLength(params.cellTextDocuments,1);
  params.cellTextDocuments[0].uri := FilePathToUri('c:\source\foo.py');  
  
  LClient.SendRequest(lspDidCloseNotebookDocument, '', params); 
end;
```

## Language Features

### Completion Request

The Completion request is sent from the client to the server to compute completion 
items at a given cursor position. Completion items are presented in the IntelliSense 
user interface.

By default the request can only delay the computation of the detail and documentation 
properties. Since 3.16.0 the client can signal that it can resolve more properties lazily.

The response from the server is handled in the OnCompletion() event.

```
// The event catches the response from the server
FLSPClient1.OnCompletion := OnCompletion1;

var
  params: TLSPCompletionParams;
begin
  if not FLSPClient1.IsRequestSupported(lspCompletion) then Exit;
  
  // You can also make sure the server has been initialized before you send
  // a completion request.
  if not FLSPClient1.Initialized then Exit;
  
  params := TLSPCompletionParams.Create;
  
  // Text document and position
  params.textDocument.uri := FilePathToUri('c:\source.foo.c');
  params.position.line := 12;
  params.position.character := 6;
  
  FLSPClient1.SendRequest(lspCompletion, '', params);
end;

procedure OnCompletion1(Sender: TObject; const list: TLSPCompletionList; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  s: string;
  item,obj: TLSPCompletionItem;
begin
  // ---------------------------------------------------------------------------
  // Item defaults ?
  // Since 3.17.0 the server can return default item values that can be used if
  // none is provided in the item.
  //
  // commitCharacters: TArray<string>
  FCompletion.CommitCharacters := Copy(list.itemDefaults.commitCharacters);
  if list.itemDefaults.editRange is TLSPInsertReplaceEdit then
  begin
    FCompletion.InsertRange := TLSPInsertReplaceEdit(list.itemDefaults.editRange).insert;
    FCompletion.ReplaceRange := TLSPInsertReplaceEdit(list.itemDefaults.editRange).replace;
  end
  else list.itemDefaults.editRange is TLSPTextEdit then
  begin
    FCompletion.Range := list.itemDefaults.editRange.range;
  end;
  // ---------------------------------------------------------------------------
    
  // Read completion items to a list
  for i := 0 to list.items.Count - 1 do
  begin
    // Copy objects to ensure the object is still available if we need to send
    // a resolve request to the server.
    item := list.items[i];
    s := item.slabel;

    obj := TLSPCompletionItem.Create;
    obj.slabel := item.slabel;
    obj.kind := item.kind;
    obj.detail := item.detail;
    obj.documentation := item.documentation;
    obj.data := item.data; // Always include this otherwise CompletionResolve will not work.
    FCompletionList.AddObject(s,obj);
  end;
  UpdateCompletionList;
end;
```

### Completion Item Resolve Request

The request is sent from the client to the server to resolve additional information 
for a given completion item. This is typically used to display additional information
for the selected item in an auto completion list.

The completion request will be quicker since the server can skip some information.
This is why you use completion item resolve requests to recieve the information for 
a selected item (when needed).

The response is handled in the OnCompletionItemResolve() event.

```
// The event catches the response from the server
FLSPClient1.OnCompletionItemResolve := OnCompletionItemResolve1;

var
  item: TLSPCompletionItem;
begin
  // Make sure the request is supported
  if not FLSPClient1.IsRequestSupported(lspCompletionItemResolve) then Exit;
  
  // Get the item to resolve from our completion list
  item := TLSPCompletionItem(FCompletionList.Objects[index]);  
  
  FLSPClient1.SendRequest(lspCompletionItemResolve, '', item);
end;

procedure OnCompletionItemResolve1(Sender: TObject; const item: TLSPCompletionItem; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
begin
  // Get the information you want to display
  detail := item.detail;
  documentation := item.documentation;
  ...
end;
```

### Hover request

The hover request is sent from the client to the server to request hover information 
at a given text document position.

```
// The event catches the response from the server
FLSPClient1.OnHover := OnHover1;

var
  params: TLSPHoverParams;
begin
  if not FLSPClient1.IsRequestSupported(lspHover) then Exit;
  
  params := TLSPHoverParams.Create;
  params.textDocument.uri := FilePathToUri(FFileName);
  params.position.line := 6;
  params.position.character := 8;
  
  FLSPClient1.SendRequest(lspHover, '', params);
end;

procedure OnHover1(Sender: TObject; const value: TLSPHover; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
begin
  // Check to see what type of data the server returned
  if Length(value.contentsMarkedArray) > 0 then
  begin
    // Array of TLSPMarkedString items (or strings)
    for i := 0 to Length(value.contentsMarkedArray) - 1 do
    begin
      lang := value.contentsMarkedArray[i].language;
      val := value.contentsMarkedArray[i].value;
      
      // Store lang and val pairs
      ...
    end;
  end
  else if value.contents.value <> '' then
  begin
    // The server returned a TLSPMarkupContent object. kind = 'plaintext' | 'markdown';
    kind := value.contents.kind;
    val := value.contents.value;
  end
  else
  begin
    // The server returned a single TLSPMarkedString object or a string
    lang := value.contentMarked.language;
    val := value.contentMarked.value;
  end;
  
  ShowHintWindow(...);
end;
```

### Signature Help Request

The signature help request is sent from the client to the server to request signature 
information at a given cursor position. Usually used to display parameter hints.
There may be several signatures if the function is overloaded and comes with different 
parameters.

```
// The event catches the response from the server
FLSPClient1.OnSignatureHelp := OnSignatureHelp1;

var
  params: TLSPSignatureHelpParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSignatureHelp) then Exit;
  
  params := TLSPSignatureHelpParams.Create;
  params.position.line := 6;
  params.position.character := 8;
  
  FLSPClient1.SendRequest(lspSignatureHelp, '', params);
end;

procedure OnSignatureHelp1(Sender: TObject; const value: TLSPSignatureHelp; const errorCode: Integer; 
    const errorMessage: string);
var
  i,j: Integer;
  ...
begin
  // Process one or more signatures (there may be overloaded functions with different parameters).
  for i := 0 to Length(value.signatures) - 1 do
  begin
    sLabel := value.signatures[i].slabel;
    documentationKind := value.signatures[i].documentation.kind;
    documentationValue := value.signatures[i].documentation.value;
    
    for j := 0 to Length(value.signatures[i].parameters) - 1 do
    begin
      arr[j].sLabel := value.signatures[i].parameters[j].slabel;
      arr[j].documentationKind := value.signatures[i].documentation.kind;
      arr[j].documentationValue := value.signatures[i].documentation.value;
      ...
    end;
    
    // Store the data
    ...
  end;
  
  // Active signature
  activeSignature := value.activeSignature;
  
  // Active parameter of the active signature
  activeParameter := value.activeParameter;
end;
```


### Publish Diagnostics Notification

Diagnostics notification are sent from the server to the client to signal results of 
validation runs. 
Use this in the application to display errors, warnings, information or hints
generated by the language server.

#### OnPublishDiagnostics

```
procedure OnPublishDiagnostics1(Sender: TObject; const uri: string; const version: Cardinal; 
    const diagnostics: TArray<TLSPDiagnostic>);
var
  i: Integer;
  s,ws: string;
  item: TDiagnosticItem;
begin
  for i := 0 to Length(diagnostics) - 1 do
  begin
    // Process information in the array
    item := TDiagnosticItem.Create;
    item.Range := diagnostics[i].range;
    item.Severity := diagnostics[i].severity;
    item.MessageStr := diagnostics[i].messageString;
    
    case item.Severity of
      1: ws := '[Error]';
      2: ws := '[Warning]';
      3: ws := '[Information]';
      4: ws := '[Hint]';
      else
        ws := '';
    end;
    s := ws + ' ' + item.MessageStr;

    ListBoxDiagnostics.Items.AddObject(s, item);
    ...
  end;
end;
```

### Pull Diagnostics

The specification introduces the concept of diagnostic pull requests to give
a client more control over the documents for which diagnostics should be computed and
at which point in time.

#### Document Diagnostics

The text document diagnostic request is sent from the client to the server to ask
the server to compute the diagnostics for a given document.

```
// The event catches the response from the server
FLSPClient1.OnOnDocumentDiagnostic := OnDocumentDiagnostic1;

var
  item: TLSPDocumentDiagnosticParams;
begin
  // Make sure the request is supported
  if not FLSPClient1.IsRequestSupported(lspDocumentDiagnostic) then Exit;
  
  item := TLSPDocumentDiagnosticParams.Create;
  item.textDocument.uri := FilePathToUri(FFileName);  
  
  FLSPClient1.SendRequest(lspDocumentDiagnostic, '', item);
end;

procedure OnDocumentDiagnostic1(Sender: TObject; const kind: string; const resultId: string; 
    const items: TArray<TLSPDiagnostic>; const errorCode: Integer; const errorMessage: string;
    const retriggerRequest: Boolean);
var
  i: Integer;
  s,ws: string;
  item: TDiagnosticItem;
begin
  if kind = 'unchanged' then Exit;
  
  if (ErrorCode = Integer(TLSPErrorCodes.ServerCancelled)) and retriggerRequest then
  begin
    // Re-trigger the document diagnostic request
    ...
  end;
  
  for i := 0 to Length(items) - 1 do
  begin
    // Process information in the array
    item := TDiagnosticItem.Create;
    item.Range := items[i].range;
    item.Severity := items[i].severity;
    item.MessageStr := items[i].messageString;
    
    case item.Severity of
      1: ws := '[Error]';
      2: ws := '[Warning]';
      3: ws := '[Information]';
      4: ws := '[Hint]';
      else
        ws := '';
    end;
    s := ws + ' ' + item.MessageStr;

    ListBoxDiagnostics.Items.AddObject(s, item);
    ...
  end;
end;
```


#### Workspace Diagnostics

The workspace diagnostic request is sent from the client to the server to ask the
server to compute workspace wide diagnostics which previously where pushed from the
server to the client. In contrast to the document diagnostic request the workspace
request can be long running and is not bound to a specific workspace or document state.

```
// The event catches the response from the server
FLSPClient1.OnOnWorkspaceDiagnostic := OnWorkspaceDiagnostic1;

var
  item: TLSPWorkspaceDiagnosticParams;
begin
  // Make sure the request is supported
  if not FLSPClient1.IsRequestSupported(lspWorkspaceDiagnostic) then Exit;
  
  item := TLSPWorkspaceDiagnosticParams.Create; 
  
  FLSPClient1.SendRequest(lspWorkspaceDiagnostic, '', item);
end;

procedure OnWorkspaceDiagnostic1(Sender: TObject; reports: TArray<TLSPWorkspaceDocumentDiagnosticReport>;
    const errorCode: Integer; const errorMessage: string; const retriggerRequest: Boolean);
var
  i,j,ver: Integer;
  s,sz,ws: string;
  item: TDiagnosticItem;
begin
  if kind = 'unchanged' then Exit;
  
  if (ErrorCode = Integer(TLSPErrorCodes.ServerCancelled)) and retriggerRequest then
  begin
    // Re-trigger the document diagnostic request
    ...
  end;
  
  for i := 0 to Length(reports) - 1 do
  begin
    sz := reports[i].uri;
    ver := reports[i].version;
    
    // Process diagnostics in the array
    for j := 0 to Length(reports[i].items) - 1 do
    begin
      item := TDiagnosticItem.Create;
      item.Range := reports[i].items[j].range;
      item.Severity := reports[i].items[j].severity;
      item.MessageStr := reports[i].items[j].messageString;
        
      case item.Severity of
        1: ws := '[Error]';
        2: ws := '[Warning]';
        3: ws := '[Information]';
        4: ws := '[Hint]';
        else
          ws := '';
      end;
      s := ws + ' ' + item.MessageStr;

      ListBoxDiagnostics.reports.AddObject(s, sz, ver, item);
      ...
    end;
  end;
end;
```

#### Workspace Diagnostic Refresh Request

The workspace/diagnostic/refresh request is sent from the server to the client.
Servers can use it to ask clients to refresh all needed document and workspace
diagnostics. This is useful if a server detects a project wide configuration
change which requires a re-calculation of all diagnostics.

```
// Set event handler to be able to recieve refresh requests
LSPClient1.OnWorkspaceDiagnosticRefresh := OnWorkspaceDiagnosticRefresh1;

procedure OnCodeLensRefresh1(Sender: TObject; var errorCode: Integer; var errorMessage: string);
var
  item: TLSPWorkspaceDiagnosticParams;
begin
  // Pull for new diagnostics.
  item := TLSPWorkspaceDiagnosticParams.Create;  
  FLSPClient1.SendRequest(lspWorkspaceDiagnostic, '', item);
end;

```

### Goto Requests

A goto request is sent from the client to the server to resolve one or more locations
of a symbol at a given position in your document. Usualy under the mouse cursor.

Goto requests support streaming (if you set a partial result token) using the OnProgress 
event. See OnProgress for example.

#### Goto Declaration Request

The goto declaration request is sent from the client to the server to resolve the 
declaration location of a symbol at a given text document position.

```
// The event catches the response from the server
FLSPClient1.OnGotoDeclaration := OnGotoDeclaration1;

var
  params: TLSPDeclarationParams;
begin
  if not FLSPClient1.IsRequestSupported(lspGotoDeclaration) then Exit;
  
  params := TLSPDeclarationParams.Create;
  params.position.line := 126;
  params.position.character := 10;
  
  FLSPClient1.SendRequest(lspGotoDeclaration, '', params);
end;

procedure OnGotoDeclaration1(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  uri: string;
  range: TLSPRange;
  location: TLSPLocation;
  locationLink: TLSPLocationLink;
begin
  // The response can be null, TLSPLocation, TArray<TLSPLocation> or TArray<TLSPLocationLink>
  if Length(value.locationLinks) > 0 then
  begin
    for i := 0 to Length(value.locationLinks) - 1 do
    begin
      uri := value.locationLinks[i].targetUri;
      locationLink := value.locationLinks[i];
      
      // Add data to a list ...
      AddToDeclList(uri, locationLink);
    end;
  end
  else if Length(value.locations) > 0 then
  begin
    for i := 0 to Length(value.locations) - 1 do
    begin
      uri := value.locations[i].uri;
      location := value.locations[i];
      
      // Add data to a list ...
      AddToDeclList(uri, location);
    end;
  end
  else
  begin
    uri := value.location.uri;
    range := value.location.range;
    
    if uri <> '' then  
    begin
      // Add data to a list ...
      AddToDeclList(uri, range);
    end;
  end;
  
  // Display the locations and let the user select, or just open the first
  // file and highlight the location.
  ...
end;
```

#### Goto Definition Request

The go to definition request is sent from the client to the server to resolve the definition 
location of a symbol at a given text document position.

```
// The event catches the response from the server
FLSPClient1.OnGotoDefinition := OnGotoDefinition1;

var
  params: TLSPDefinitionParams;
begin
  if not FLSPClient1.IsRequestSupported(lspGotoDefinition) then Exit;
  
  params := TLSPDefinitionParams.Create;
  params.position.line := 102;
  params.position.character := 20;
  
  FLSPClient1.SendRequest(lspGotoDefinition, '', params);
end;

procedure OnGotoDefinition1(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; 
    const errorMessage: string);
begin    
  // See Goto Declaration Request for example
end;
```

#### Goto Implementation Request

The go to implementation request is sent from the client to the server to resolve the 
implementation location of a symbol at a given text document position.

```
// The event catches the response from the server
FLSPClient1.OnGotoImplementation := OnGotoImplementation1;

var
  params: TLSPImplementationParams;
begin
  if not FLSPClient1.IsRequestSupported(lspGotoImplementation) then Exit;
  
  params := TLSPImplementationParams.Create;
  params.position.line := 11;
  params.position.character := 3;
  
  FLSPClient1.SendRequest(lspGotoImplementation, '', params);
end;

procedure OnGotoImplementation1(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; 
    const errorMessage: string);
begin    
  // See Goto Declaration Request for example
end;
```

#### Goto Type Definition Request

The go to type definition request is sent from the client to the server to resolve the 
type definition location of a symbol at a given text document position.

```
// The event catches the response from the server
FLSPClient1.OnGotoTypeDefinition := OnGotoTypeDefinition1;

var
  params: TLSPTypeDefinitionParams;
begin
  if not FLSPClient1.IsRequestSupported(lspGotoTypeDefinition) then Exit;
  
  params := TLSPTypeDefinitionParams.Create;
  params.position.line := 12;
  params.position.character := 11;
  
  FLSPClient1.SendRequest(lspGotoTypeDefinition, '', params);
end;

procedure OnGotoTypeDefinition1(Sender: TObject; const value: TLSPGotoResponse; const errorCode: Integer; 
    const errorMessage: string);
begin    
  // See Goto Declaration Request for example
end;
```

### Find References Request

The references request is sent from the client to the server to resolve project-wide 
references for the symbol denoted by the given text document position.

```
// The event catches the response from the server
FLSPClient1.OnFindReferences := OnFindReferences1;

var
  params: TLSPReferencesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspReferences) then Exit;
  
  params := TLSPReferencesParams.Create;
  params.position.line := 11;
  params.position.character := 3;
  params.context.includeDeclaration := True;
  
  FLSPClient1.SendRequest(lspReferences, '', params);
end;

procedure OnFindReferences1(Sender: TObject; const value: TLSPFindReferencesResponse;
  const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  LRange: TLSPRange;
begin    
  // Display all references found in the project
  for i := 0 to Length(value.locations) - 1 do
  begin
    sz := UriToFilePath(value.locations[i].uri);
    LRange := value.locations[i].range;
    ProcessItem(sz, LRange);
  end;
end;

procedure ProcessItem(const sz: string; const range: TLSPRange);
begin
  // You could display found references in a tree
  //
  // [-] C:\MyFolder\Source\Foo.cpp
  //       Line: 12: <The actual text line ...>
  //       Line: 28: ...
  
end;
```

### Document Highlight Request

The document highlight request is sent from the client to the server to resolve a document 
highlights for a given text document position. For programming languages this usually 
highlights all references to the symbol scoped to this file.

The request uses text as the kind and is allowed to be more fuzzy than a references request.

The request support streaming (if you set a partial result token) and use the OnProgress 
event to retrieve partial data. See OnProgress for example.

```
// The event catches the response from the server
FLSPClient1.OnDocumentHighlight := OnDocumentHighlight1;

var
  params: TLSPDocumentHighlightParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentHighlight) then Exit;
  
  params := TLSPDocumentHighlightParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.position.line := 12;
  params.position.character := 11;
  
  FLSPClient1.SendRequest(lspDocumentHighlight, '', params);
end;

procedure OnDocumentHighlight1(Sender: TObject; const value: TLSPDocumentHighlightResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  kind: Cardinal;
  range: TLSPRange;
begin    
  for i := 0 to Length(value.list) - 1 do
  begin
    kind := value.list[i].kind;
    range := value.list[i].range;
    
    // Add values
    AddSelections(kind, range);
  end;
  ...
end;
```

### Document Symbols Request

The document symbol request is sent from the client to the server to retrieve all
symbols found in the given text document (e.g. classes, properties, methods...).

The returned result is either a flat list of symbols (array of TLSPSymbolInformation) or 
a hierarchy of symbols found (array of TLSPDocumentSymbol) in a given text document.

The request support streaming (if you set a partial result token) and use the OnProgress 
event to retrieve partial data. See OnProgress for example.

```
// The event catches the response from the server
FLSPClient1.OnDocumentSymbols := OnDocumentSymbols1;

var
  params: TLSPDocumentSymbolParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentSymbol) then Exit;
  
  params := TLSPDocumentSymbolParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.java');
  
  FLSPClient1.SendRequest(lspDocumentSymbol, '', params);
end;

procedure OnDocumentSymbols1(Sender: TObject; const value: TLSPDocumentSymbolsResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  sym: TLSPDocumentSymbol;
begin    
  for i := 0 to Length(value.symbols) - 1 do
  begin
    sym := value.symbols[i];
    
    // Add symbols
    AddSymbols(sym.name, sym);
  end;
  ...
end;
```

### Code Action Request

The code action request is sent from the client to the server to compute commands 
for a given text document and range. These commands are typically code fixes to 
either fix problems or to beautify/refactor code.

```
// The event catches the response from the server
FLSPClient1.OnCodeAction := OnCodeAction1;

var
  params: TLSPCodeActionParams;
begin
  if not FLSPClient1.IsRequestSupported(lspCodeAction) then Exit;
  
  params := TLSPCodeActionParams.Create;
  
  // Document
  params.textDocument.uri := FilePathToUri('c:\source\foo.js');
  
  // Code action kind
  params.context.only := ['quickfix','refactor','refactor.extract','refactor.inline','refactor.rewrite',
                          'source','source.organizeImports'];
  
  // Range
  params.range.startPos.line := 2;
  params.range.startPos.character := 0;
  params.range.endPos.line := 16;
  params.range.endPos.character := 0;
  
  // Include diagnostics errors inside the passed range
  ...  
  
  FLSPClient1.SendRequest(lspCodeAction, '', params);
end;

procedure OnCodeAction1(Sender: TObject; const value: TLSPCodeActionResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  item: TLSPCodeAction;
  f: TSelectForm;
begin
  f := TSelectForm.Create(Self);
  try
    for i := 0 to value.codeActions.Count - 1 do
    begin
      item := TLSPCodeAction.Create;
      if value.codeActions[i].title <> '' then
      begin
        // Code action 
        item.title := value.codeActions[i].title;
        item.kind := value.codeActions[i].kind;
        if Assigned(value.codeActions[i].edit) then
        begin
          item.edit := TLSPWorkspaceEdit.Create;
          if Assigned(value.codeActions[i].edit.changes) then
            item.edit.changes.Create(value.codeActions[i].edit.changes);
          if Length(value.codeActions[i].edit.documentChanges) > 0 then
            item.edit.documentChanges := Copy(value.codeActions[i].edit.documentChanges);
        end;
      end
      else
      begin
        // Command
        item.title := value.codeActions[i].command.title;
      end;
      item.command := value.codeActions[i].command;
      item.data := value.codeActions[i].data;

      f.List.Items.AddObject(item.title, item);
    end;
    
    if f.List.Items.Count = 0 then Exit;

    if f.ShowModal = mrOK then
    begin
      if f.List.ItemIndex >= 0 then
      begin
        obj := TLSPCodeAction(f.List.Items.Objects[f.List.ItemIndex]);

        // Check for edit or command. If none found - send a codeActionResolve
        if Assigned(obj.edit) then
          LSPApplyChanges(obj.edit)
        else if obj.command.command <> '' then
          LSPSendExecuteCommand(obj.command)
        else
          LSPSendCodeActionResolve(obj);
        end;
      end;
    end;
  finally
    f.Release;
  end;
end;
```

### Code Action Resolve Request

The request is sent from the client to the server to resolve additional information 
for a given code action. This is usually used to compute the edit property of a code 
action to avoid its unnecessary computation during the textDocument/codeAction request.

```
// The event catches the response from the server
FLSPClient1.OnCodeActionResolve := OnCodeActionResolve1;

var
  params: TLSPCodeAction;
begin
  if not FLSPClient1.IsRequestSupported(lspCodeActionResolve) then Exit;
  
  // Resolve code action
  params := FCodeActionList[FIndex];
  
  FLSPClient1.SendRequest(lspCodeActionResolve, '', params);
end;

procedure OnCodeActionResolve1(Sender: TObject; const value: TLSPCodeAction; const errorCode: Integer; 
    const errorMessage: string);
begin
  if Assigned(value.edit) then
    LSPApplyChanges(value.edit)
end;
```

### Code Lens Request

The code lens request is sent from the client to the server to compute code lenses 
for a given text document.

```
// Event to catch the code lens response from the server
FLSPClient1.OnCodeLens := OnCodeLens1;

var
  params: TLSPCodeLensParams;
begin
  if not FLSPClient1.IsRequestSupported(lspCodeLens) then Exit;
  
  params := TLSPCodeLensParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.pas');
  
  FLSPClient1.SendRequest(lspCodeLens, '', params);
end;

procedure OnCodeLens1(Sender: TObject; const value: TLSPCodeLensResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  lens: TLSPCodeLens;
begin
  for i := 0 to value.codeLensList.Count - 1 do
  begin
    lens := value.codeLensList[i];
    AddCodeLensToList(lens);
  end;
  ...
end;
```

### Code Lens Resolve Request

The code lens resolve request is sent from the client to the server to resolve the 
command for a given code lens item.

```
// The event catches the response from the server
FLSPClient1.OnCodeLensResolve := OnCodeLensResolve1;

var
  params: TLSPCodeLens;
begin
  if not FLSPClient1.IsRequestSupported(lspCodelensResolve) then Exit;
  
  params := GetCodeLensFromList(FIndex);
  
  FLSPClient1.SendRequest(lspCodeLensResolve, '', params);
end;

procedure OnCodeLensResolve1(Sender: TObject; const value: TLSPCodeLens; const errorCode: Integer; 
    const errorMessage: string);
begin
  ResolveLens(value);
  ...
end;
```

### Code Lens Refresh Request

The workspace/codeLens/refresh request is sent from the server to the client. Servers 
can use it to ask clients to refresh the code lenses currently shown in editors. As a 
result the client should ask the server to recompute the code lenses for these editors.

```
// Set event handler to be able to recieve refresh requests
LSPClient1.OnCodeLensRefresh := OnCodeLensRefresh1;

procedure OnCodeLensRefresh1(Sender: TObject; var errorCode: Integer; var errorMessage: string);
begin
  // Refresh code lenses.
  ...
  
  FLSPClient1.SendResponse(lspCodeLensRefresh);
end;

```

### Document Link Request

The document links request is sent from the client to the server to request the location 
of links in a document.

```
// Event to catch response from the server
FLSPClient1.OnDocumentLink := OnDocumentLink1;

var
  params: TLSPDocumentLinkParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentLink) then Exit;
  
  params := TLSPDocumentLinkParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  
  FLSPClient1.SendRequest(lspDocumentLink, '', params);
end;

procedure OnDocumentLink1(Sender: TObject; const value: TLSPDocumentLinkResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  link: TLSPDocumentLink;
begin
  for i := 0 to value.documentLinks.Count - 1 do
  begin
    link := value.documentLinks[i];
    AddDocumentLinkToList(link);
  end;
  ...
end;
```

### Document Link Resolve Request

The document link resolve request is sent from the client to the server to resolve 
the target of a given document link.

```
// Catch the response from the server
FLSPClient1.OnDocumentLinkResolve := OnDocumentLinkResolve1;

var
  params: TLSPDocumentLink;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentLinkResolve) then Exit;
  
  // Resolve document link
  params := FDocumentLinkList[FIndex];
  
  FLSPClient1.SendRequest(lspDocumentLinkResolve, '', params);
end;

procedure OnDocumentLinkResolve1(Sender: TObject; const value: TLSPDocumentLink; const errorCode: Integer; 
    const errorMessage: string);
begin
  // Get information from value
  ...
end;
```

### Document Color Request

The document color request is sent from the client to the server to list all color 
references found in a given text document. Along with the range, a color value in 
RGB is returned.

```
// Event to catch response from the server
FLSPClient1.OnDocumentColor := OnDocumentColor1;

var
  params: TLSPDocumentColorParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentColor) then Exit;
  
  params := TLSPDocumentColorParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  
  FLSPClient1.SendRequest(lspDocumentColor, '', params);
end;

procedure OnDocumentColor1(Sender: TObject; const values: TLSPColorInformationValues; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  colorInfo: TLSPColorInformation;
  alpha: Cardinal;
  red, green, blue: Cardinal;
  range: TLSPRange;
begin
  for i := 0 to Length(values.colors) - 1 do
  begin
    colorInfo := values.colors[i];
    
    // Convert colors
    red := Round(255 * colorInfo.color.red);
    green := Round(255 * colorInfo.color.green);
    blue := Round(255 * colorInfo.color.blue);
    alpha := Round(255 * colorInfo.color.alpha);
    
    // Where the color was found
    range := colorInfo.range;
    
    AddColorInfo(RGB(red, green, blue), alpha, range);
  end;
  ...
end;
```

### Color Presentation Request

The color presentation request is sent from the client to the server to obtain a list 
of presentations for a color value at a given location. Clients can use the result to

* a color reference.
* show in a color picker and let users pick one of the presentations

```
// Event to catch response from the server
FLSPClient1.OnColorPresentation := OnColorPresentation1;

var
  params: TLSPColorPresentationParams;
begin
  if not FLSPClient1.IsRequestSupported(lspColorPresentation) then Exit;
  
  params := TLSPColorPresentationParams.Create;
  
  // Text document
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  
  // Color information
  params.color := AColor;
  
  // Range where the color is inserted
  params.range := ARange;  
  
  FLSPClient1.SendRequest(lspColorPresentation, '', params);
end;

procedure OnColorPresentation1(Sender: TObject; const values: TLSPColorPresentationValues; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  colorPresentation: TLSPColorPresentation;
  s: string;
  range: TLSPRange;
begin
  for i := 0 to Length(values.colorPresentations) - 1 do
  begin
    colorPresentation := values.colorPresentations[i];
    
    // Label or text
    s := colorPresentation.slabel;
    if colorPresentation.textEdit.newText <> '' then
      s := colorPresentation.textEdit.newText;
    
    // Range
    range := colorPresentation.textEdit.range;
    
    AddColorPresentation(s, range);
  end;
  ...
end;
```

### Document Formatting Request

The document formatting request is sent from the client to the server to format a 
whole document.

The returned array should be used to edit your document. You should always apply changes from
the end of the document. Some servers may reverse the returned array for you. Others
may not. So you need to check the returned array.

```
// Event to catch response from the server
FLSPClient1.OnDocumentFormatting := OnDocumentFormatting1;

var
  params: TLSPDocumentFormattingParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentFormatting) then Exit;
  
  params := TLSPDocumentFormattingParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.options.tabSize := 2;
  params.options.insertSpaces := True;
  params.options.trimTrailingWhitespace := true;

  FLSPClient.SendRequest(lspDocumentFormatting, '', params);
end;

procedure OnDocumentFormatting1(Sender: TObject; const value: TLSPTextEditValues; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  edit: TLSPTextEdit;
begin
  // The returned array may be reversed by the server so we need to check it.
  // You should apply changes from the end of document to the beginning.
  if IsReversed(value.edits) then
  begin 
    for i := 0 to Length(value.edits) - 1 do
    begin
      edit := value.edits[i];
      ModifyDocument(edit.newText, edit.range);
    end;
  end
  else
  begin
    for i := Length(value.edits) - 1 downto 0 do
    begin
      edit := value.edits[i];
      ModifyDocument(edit.newText, edit.range);
    end;
  end;
end;
```

### Document Range Formatting Request

The document range formatting request is sent from the client to the server to format 
a given range in a document.

```
// Event to catch response from the server
FLSPClient1.OnDocumentRangeFormatting := OnDocumentRangeFormatting1;

var
  params: TLSPDocumentRangeFormattingParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentRangeFormatting) then Exit;
  
  params := TLSPDocumentRangeFormattingParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.options.tabSize := 2;
  params.options.insertSpaces := True;
  params.options.trimTrailingWhitespace := true;
  
  params.range.startPos.line := 10;
  params.range.startPos.character := 0;
  params.range.endPos.line := 200;
  params.range.endPos.character := 0;

  FLSPClient.SendRequest(lspDocumentRangeFormatting, '', params);
end;

procedure OnDocumentRangeFormatting1(Sender: TObject; const value: TLSPTextEditValues; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  edit: TLSPTextEdit;
begin
  // The returned array may be reversed by the server so we need to check it.
  // You should apply changes from the end of document to the beginning.
  if IsReversed(value.edits) then
  begin 
    for i := 0 to Length(value.edits) - 1 do
    begin
      edit := value.edits[i];
      ModifyDocument(edit.newText, edit.range);
    end;
  end
  else
  begin
    for i := Length(value.edits) - 1 downto 0 do
    begin
      edit := value.edits[i];
      ModifyDocument(edit.newText, edit.range);
    end;
  end;
end;
```

### Document OnTypeFormatting Request

The document on type formatting request is sent from the client to the server to format 
parts of the document during typing.

```
// Event to catch response from the server
FLSPClient1.OnDocumentOnTypeFormatting := OnDocumentOnTypeFormatting1;

var
  params: TLSPDocumentOnTypeFormattingParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentOnTypeFormatting) then Exit;
  
  params := TLSPDocumentOnTypeFormattingParams.Create;
  
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.ch := ';';
  
  params.options.tabSize := 2;
  params.options.insertSpaces := True;
  params.options.trimTrailingWhitespace := true;
  
  params.position.line := 10;
  params.position.character := 8;

  FLSPClient.SendRequest(lspDocumentOnTypeFormatting, '', params);
end;

procedure OnDocumentOnTypeFormatting1(Sender: TObject; const values: TLSPTextEditValues; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  edit: TLSPTextEdit;
begin
  // The returned array may be reversed by the server so we need to check it.
  // You should apply changes from the end of document to the beginning.
  if IsReversed(value.edits) then
  begin 
    for i := 0 to Length(value.edits) - 1 do
    begin
      edit := value.edits[i];
      ModifyDocument(edit.newText, edit.range);
    end;
  end
  else
  begin
    for i := Length(value.edits) - 1 downto 0 do
    begin
      edit := value.edits[i];
      ModifyDocument(edit.newText, edit.range);
    end;
  end;
end;
```

### Folding Range Request

The folding range request is sent from the client to the server to return all folding 
ranges found in a given text document.

```
FLSPClient1.OnFoldingRange := OnFoldingRange1;

var
  i: Integer;
  params: TLSPFoldingRangeParams;
begin
  if not FLSPClient1.IsRequestSupported(lspFoldingRange) then Exit;
  
  params := TLSPFoldingRangeParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  
  FLSPClient1.SendRequest(lspFoldingRange, '', params);
end;

procedure OnFoldingRange1(Sender: TObject; const values: TLSPFoldingRangeResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  foldRange: TLSPFoldingRange;
begin
  if Assigned(values) then
  begin
    for i := 0 to Length(values.foldingRanges) - 1 do
    begin
      foldRange := values.foldingRanges[i];
      AddFoldingRange(foldRange.kind, foldRange.startLine, foldRange.endLine);
    end;
  end;
end;
```

### Selection Range Request

The selection range request is sent from the client to the server to return suggested 
selection ranges at an array of given positions. A selection range is a range around 
the cursor position which the user might be interested in selecting.

```
FLSPClient1.OnSelectionRange := OnSelectionRange1;

var
  i: Integer;
  params: TLSPSelectionRangeParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSelectionRange) then Exit;
  
  params := TLSPSelectionRangeParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  
  // Set position(s)
  SetLength(params.positions, 1);
  params.positions[0].line := 10;
  params.positions[0].character := 34;
  
  FLSPClient1.SendRequest(lspSelectionRange, '', params);
end;

procedure OnSelectionRange1(Sender: TObject; const values: TLSPSelectionRangeResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  selRange: TLSPSelectionRange;
begin
  for i := 0 to values.selRanges.Count - 1 do
  begin
    selRange := values.selRanges[i];
    AddSelectionRange(selRange.range, selRange.parent);
  end;
end;
```

### Prepare Call Hierarchy Request

The call hierarchy request is sent from the client to the server to return a call 
hierarchy for the language element of given text document positions. The call hierarchy 
requests are executed in two steps:

1. first a call hierarchy item is resolved for the given text document position
2. for a call hierarchy item the incoming or outgoing call hierarchy items are resolved.

```
FLSPClient1.OnPrepareCallHierarchy := OnPrepareCallHierarchy1;

var
  i: Integer;
  params: TLSPCallHierarchyPrepareParams;
begin
  if not FLSPClient1.IsRequestSupported(lspPrepareCallHierarchy) then Exit;
  
  params := TLSPCallHierarchyPrepareParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.position.line := 4;
  params.position.character := 8;
  
  FLSPClient1.SendRequest(lspPrepareCallHierarchy, '', params);
end;

procedure OnPrepareCallHierarchy1(Sender: TObject; const values: TLSPPrepareCallHierarchyResponse; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  item: TLSPCallHierarchyItem;
begin
  for i := 0 to Length(values) - 1 do
  begin
    // item may include:
    // name           = 'foo'
    // kind           = symMethod
    // uri            = 'c:\source\foo.java'
    // range          = range of the method
    // selectionRange = range of the method name

    item := values[i];    
    AddCallHierarchy(item.name, item);
  end;
end;
```

### Call Hierarchy Incomming Calls

The request is sent from the client to the server to resolve incoming calls for a given 
call hierarchy item.

```
FLSPClient1.OnCallHierarchyIncomming := OnCallHierarchyIncomming1;

var
  i: Integer;
  params: TLSPCallHierarchyIncomingCallsParams;
begin
  if not FLSPClient1.IsRequestSupported(lspCallHierarchyIncommingCalls) then Exit;
  
  // Get the item for our method, e.g. "Foo()"
  params := GetCallHierarchyItem(FIndex);
    
  FLSPClient1.SendRequest(lspCallHierarchyIncommingCalls, '', params);
end;

procedure OnCallHierarchyIncomming1(Sender: TObject; const values: TLSPCallHierarchyIncomingCallResponse; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  item: TLSPCallHierarchyIncomingCall;
begin
  for i := 0 to Length(values) - 1 do
  begin
    // Collect all methods, functions etc. from where the method "Foo()" is called.
    item := values[i];    
    AddIncommingHierarchyCalls(item.name, item);
  end;
end;
```

### Call Hierarchy Outgoing Calls

The request is sent from the client to the server to resolve outgoing calls for a given 
call hierarchy item.

```
FLSPClient1.OnCallHierarchyOutgoing := OnCallHierarchyOutgoing1;

var
  i: Integer;
  params: TLSPCallHierarchyOutgoingCallsParams;
begin
  if not FLSPClient1.IsRequestSupported(lspCallHierarchyOutgoingCalls) then Exit;
  
  // Get the item for our method, e.g. "Foo()"
  params := GetCallHierarchyItem(FIndex);
    
  FLSPClient1.SendRequest(lspCallHierarchyOutgoingCalls, '', params);
end;

procedure OnCallHierarchyOutgoing1(Sender: TObject; const values: TLSPCallHierarchyOutgoingCallResponse; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  item: TLSPCallHierarchyOutgoingCall;
begin
  for i := 0 to Length(values) - 1 do
  begin
    // Collect a list of all calls made from Foo().
    item := values[i];    
    AddOutgoingHierarchyCalls(item.name, item);
  end;
end;
```

### Prepare Type Hierarchy Request

The type hierarchy request is sent from the client to the server to return a type
hierarchy for the language element of given text document positions. Will return
null if the server couldn’t infer a valid type from the position. The type hierarchy
requests are executed in two steps:

1. first a type hierarchy item is prepared for the given text document position.
2. for a type hierarchy item the supertype or subtype type hierarchy items are resolved.

```
FLSPClient1.OnPrepareTypeHierarchy := OnPrepareTypeHierarchy1;

var
  i: Integer;
  params: TLSPTypeHierarchyPrepareParams;
begin
  if not FLSPClient1.IsRequestSupported(lspPrepareTypeHierarchy) then Exit;
  
  params := TLSPTypeHierarchyPrepareParams.Create;
  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.position.line := 4;
  params.position.character := 8;
  
  FLSPClient1.SendRequest(lspPrepareTypeHierarchy, '', params);
end;

procedure OnPrepareTypeHierarchy1(Sender: TObject; const values: TLSPPrepareTypeHierarchyResponse; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  item: TLSPTypeHierarchyItem;
begin
  for i := 0 to Length(values) - 1 do
  begin
    // item may include:
    // name           = 'foo'
    // kind           = symMethod
    // uri            = 'c:\source\foo.java'
    // range          = range of the method
    // selectionRange = range of the method name

    item := values[i];    
    AddTypeHierarchy(item.name, item);
  end;
end;
```

### Type Hierarchy Supertypes

The request is sent from the client to the server to resolve the supertypes for a
given type hierarchy item.

```
FLSPClient1.OnTypeHierarchySupertypes := OnTypeHierarchySupertypes1;

var
  i: Integer;
  params: TLSPTypeHierarchySupertypesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspTypeHierarchySupertypes) then Exit;
  
  // Get the item for our method, e.g. "Foo()"
  params := GetTypeHierarchyItem(FIndex);
    
  FLSPClient1.SendRequest(lspTypeHierarchySupertypes, '', params);
end;

procedure OnTypeHierarchySupertypes1(Sender: TObject; const values: TLSPPrepareTypeHierarchyResponse; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  item: TLSPTypeHierarchyItem;
begin
  for i := 0 to Length(values) - 1 do
  begin
    item := values[i];    
    AddTypeHierarchySupertypes(item.name, item);
  end;
end;
```

### Type Hierarchy Subtypes

The request is sent from the client to the server to resolve the subtypes for a
given type hierarchy item.

```
FLSPClient1.OnTypeHierarchySubtypes := OnTypeHierarchySubtypes1;

var
  i: Integer;
  params: TLSPTypeHierarchySubtypesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspTypeHierarchySubtypes) then Exit;
  
  // Get the item for our method, e.g. "Foo()"
  params := GetTypeHierarchyItem(FIndex);
    
  FLSPClient1.SendRequest(lspTypeHierarchySubtypes, '', params);
end;

procedure OnTypeHierarchySubtypes1(Sender: TObject; const values: TLSPPrepareTypeHierarchyResponse; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  item: TLSPTypeHierarchyItem;
begin
  for i := 0 to Length(values) - 1 do
  begin
    item := values[i];    
    AddTypeHierarchySubtypes(item.name, item);
  end;
end;
```

### Semantic Tokens

The request is sent from the client to the server to resolve semantic tokens for a given 
file. Semantic tokens are used to add additional color information to a file that depends 
on language specific symbol information. A semantic token request usually produces a large 
result. The protocol therefore supports encoding tokens with numbers. In addition optional 
support for deltas is available.

The request support streaming by setting a partial result id.

### Semantic Tokens for a whole file

```
FLSPClient1.OnSemanticTokensFull := OnSemanticTokensFull1;

var
  i: Integer;
  params: TLSPSemanticTokensParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSemanticTokensFull) then Exit;
  
  params := TLSPSemanticTokensParams.Create;
  
  // Find all tokens in the given file
  params.textDocument.uri := FilePathToUri('c:\source\foo.h');
    
  FLSPClient1.SendRequest(lspSemanticTokensFull, '', params);
end;

procedure OnSemanticTokensFull1(Sender: TObject; const values: TLSPSemanticTokens; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
begin
  if not Assigned(values) then Exit;
  
  // Optional result id if delta updating is used.
  FTokenResultId := Values.resultId;
  
  // Collect a list of tokens.
  for i := 0 to Length(values.data) - 1 do
    FTokens.Add(values.data[i]);    
end;
```

### Semantic Tokens delta for a whole file

```
FLSPClient1.OnSemanticTokensFullDelta := OnSemanticTokensFullDelta1;

var
  i: Integer;
  params: TLSPSemanticTokensDeltaParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSemanticTokensDelta) then Exit;
  
  params : TLSPSemanticTokensDeltaParams.Create;
  
  // Find all tokens in the given file
  params.textDocument.uri := FilePathToUri('c:\source\foo.h');
  
  // Result id from a previous request
  params.previousResultId := FTokenResultId;
    
  FLSPClient1.SendRequest(lspSemanticTokensFullDelta, '', params);
end;

procedure OnSemanticTokensFullDelta1(Sender: TObject; const values: TLSPSemanticTokensDelta; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
  edit: TLSPSemanticTokensEdit;
begin
  if not Assigned(values) then Exit;
  
  // Result id
  FTokenResultId := Values.resultId;
  
  // Edit the list of tokens
  for i := 0 to Length(values.data) - 1 do
    EditTokensList(values.edits[i]);    
end;
```

### Semantic Tokens for a range

```
FLSPClient1.OnSemanticTokensRange := OnSemanticTokensRange1;

var
  i: Integer;
  params: TLSPSemanticTokensRangeParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSemanticTokensRange) then Exit;
  
  params := TLSPSemanticTokensRangeParams.Create;
  
  // Find all tokens in the given file
  params.textDocument.uri := FilePathToUri('c:\source\foo.h');
  
  // Range
  params.range.startPos.line := 0;
  params.range.startPos.character := 0;
  params.range.endPos.line := 200;
  params.range.endPos.character := 0;
    
  FLSPClient1.SendRequest(lspSemanticTokensRange, '', params);
end;

procedure OnSemanticTokensRange1(Sender: TObject; const values: TLSPSemanticTokens; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
begin
  if not Assigned(values) then Exit;
  
  // Optional result id if delta updating is used.
  FTokenResultId := Values.resultId;
  
  // Collect a list of tokens.
  for i := 0 to Length(values.data) - 1 do
    FTokens.Add(values.data[i]);    
end;
```

### Semantic Tokens Refresh

The workspace/semanticTokens/refresh request is sent from the server to the client. 
Servers can use it to ask clients to refresh the editors for which this server provides 
semantic tokens. As a result the client should ask the server to recompute the semantic 
tokens for these editors.

```
FLSPClient1.OnSemanticTokensRefresh := OnSemanticTokensRefresh1;

procedure OnSemanticTokensRange1(Sender: TObject; const errorCode: Integer; const errorMessage: string);
begin
  // Send a new semantic tokens request a file?
  ...  
end;

```

### Linked Editing Range

The linked editing request is sent from the client to the server to return for a given 
position in a document the range of the symbol at the position and all ranges that have 
the same content.

```
FLSPClient1.OnLinkedEditingRange := OnLinkedEditingRange1;

var
  i: Integer;
  params: TLSPLinkedEditingRangeParams;
begin
  if not FLSPClient1.IsRequestSupported(lspLinkedEditingRange) then Exit;
  
  params := TLSPLinkedEditingRangeParams.Create;
  
  // Find all instances of the text (e.g. variable) in the document
  params.textDocument.uri := FilePathToUri('c:\source\foo.h');
  
  // Position
  params.position.line := 10;
  params.position.character := 0;
    
  FLSPClient1.SendRequest(lspLinkedEditingRange, '', params);
end;

procedure OnLinkedEditingRange1(Sender: TObject; const values: TLSPLinkedEditingRanges; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
begin
  if not Assigned(values) then Exit;
  
  // A list of ranges that can be renamed together.
  for i := 0 to Length(values.ranges) - 1 do
    AddEditingRange(values.ranges[i]);    
end;
```

### Monikers

The textDocument/moniker request is sent from the client to the server to get the symbol 
monikers for a given text document position. An array of Moniker types is returned as 
response to indicate possible monikers at the given location.

```
FLSPClient1.OnMoniker := OnMoniker1;

var
  i: Integer;
  params: TLSPMonikerParams;
begin
  if not FLSPClient1.IsRequestSupported(lspMoniker) then Exit;
  
  params := TLSPMonikerParams.Create;
  
  // Get symbol monikers from document
  params.textDocument.uri := FilePathToUri('c:\source\foo.h');
  
  // Position in the text
  params.position.line := 10;
  params.position.character := 0;
    
  FLSPClient1.SendRequest(lspMoniker, '', params);
end;

procedure OnMoniker1(Sender: TObject; const values: TLSPMonikerResult; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
begin
  if not Assigned(values) then Exit;
  
  // A list of moniker types
  for i := 0 to Length(values.monikers) - 1 do
    AddMonikerToList(values.monikers[i]);    
end;
```

### Inlay Hint Request

The inlay hints request is sent from the client to the server to compute inlay hints 
for a given [text document, range] tuple that may be rendered in the editor in place
with other text.

```
FLSPClient1.OnInlayHint := OnInlayHint1;

var
  i: Integer;
  params: TLSPInlayHintParams;
begin
  if not FLSPClient1.IsRequestSupported(lspInlayHint) then Exit;
  
  params := TLSPInlayHintParams.Create;
  
  // Get text document
  params.textDocument.uri := FilePathToUri('c:\source\foo.php');
  
  // Position in the text
  params.position.line := 10;
  params.position.character := 0;
    
  FLSPClient1.SendRequest(lspInlayHint, '', params);
end;

procedure OnInlayHint1(Sender: TObject; const values: TLSPInlayHintResult; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
begin
  if not Assigned(values) then Exit;
  
  // Fill a list of InlayHint types
  for i := 0 to Length(values.inlayHints) - 1 do
    AddInlayHintsToList(values.inlayHints[i]);    
end;
```

### Inlay Hint Resolve Request

The request is sent from the client to the server to resolve additional information
for a given inlay hint. This is usually used to compute the tooltip, location or 
command properties of an inlay hint’s label part to avoid its unnecessary computation
during the textDocument/inlayHint request.

The response is handled in the OnInlayHintResolve() event.

```
// The event catches the response from the server
FLSPClient1.OnInlayHintResolve := OnInlayHintResolve1;

var
  item: TLSPInlayHint;
begin
  // Make sure the request is supported
  if not FLSPClient1.IsRequestSupported(lspInlayHintResolve) then Exit;
  
  // Get the item to resolve from our hints list
  item := TLSPInlayHint(FHintsList.Objects[index]);  
  
  FLSPClient1.SendRequest(lspInlayHintResolve, '', item);
end;

procedure OnInlayHintResolve1(Sender: TObject; const item: TLSPInlayHint; const errorCode: Integer; 
    const errorMessage: string);
var
  s: string;
begin
  // Get the information you want to use
  s := item.tooltip.value;
  ...
end;
```

### Inlay Hint Refresh Request

The workspace/inlayHint/refresh request is sent from the server to the client.
Servers can use it to ask clients to refresh the inlay hints currently shown in
editors. As a result the client should ask the server to recompute the inlay hints
for these editors.

```
// Set event handler to be able to recieve refresh requests
LSPClient1.OnInlayHintRefresh := OnInlayHintRefresh1;

procedure OnInlayHintRefresh1(Sender: TObject; var errorCode: Integer; var errorMessage: string);
begin
  // Refresh inlay hints.
  ...
  
  FLSPClient1.SendResponse(lspInlayHintRefresh);
end;

```

### Inline Value Request

The inline value request is sent from the client to the server to compute inline
values for a given text document that may be rendered in the editor at the end of lines.

```
FLSPClient1.OnInlineValue := OnInlineValue1;

var
  i: Integer;
  params: TLSPInlineValueParams;
begin
  if not FLSPClient1.IsRequestSupported(lspInlineValue) then Exit;
  
  params := TLSPInlineValueParams.Create;
  
  // Get text document
  params.textDocument.uri := FilePathToUri('c:\source\foo.php');
  
  // The document range for which inline values should be computed.
  params.range.startPos.line := 10;
  params.range.startPos.character := 4;
  params.range.endPos.line := 20;
  params.range.endPos.character := 14;
    
  FLSPClient1.SendRequest(lspInlineValue, '', params);
end;

procedure OnInlineValue1(Sender: TObject; const values: TLSPInlineValueResult; 
    const errorCode: Integer; const errorMessage: string);
var
  i: Integer;
begin
  if not Assigned(values) then Exit;
  
  // Handle inline values
  for i := 0 to values.inlineValues.Count - 1 do
  begin
    if values.inlineValues[i] is TLSPInlineValueText then
    begin
      s := TLSPInlineValueText(values.inlineValues[i]).text;
      ...
    end
    else if values.inlineValues[i] is TLSPInlineValueVariableLookup then
    begin
      s := TLSPInlineValueVariableLookup(values.inlineValues[i]).variableName;
      ...
    end
    else if values.inlineValues[i] is TLSPInlineValueEvaluatableExpression then
    begin
      s := TLSPInlineValueEvaluatableExpression(values.inlineValues[i]).expression;
      ...
    end;
  end;    
end;
```

### Inline Value Refresh Request

The workspace/inlineValue/refresh request is sent from the server to the client.
Servers can use it to ask clients to refresh the inline values currently shown in
editors. As a result the client should ask the server to recompute the inline values
for these editors.

```
// Set event handler to be able to recieve refresh requests
LSPClient1.OnInlineValueRefresh := OnInlineValueRefresh1;

procedure OnInlineValueRefresh1(Sender: TObject; var errorCode: Integer; var errorMessage: string);
begin
  // Refresh inline values.
  ...
  
  FLSPClient1.SendResponse(lspInlineValueRefresh);
end;

```


## Workspace Features


### Execute Command Request

The workspace/executeCommand request is sent from the client to the server to trigger
command execution on the server.

The command and arguments may have come from the server after a codeAction request.

In most cases the server creates a WorkspaceEdit structure and applies the changes to
the workspace using the request workspace/applyEdit which is sent from the server to the client.
```
// Set event handlers
LSPClient1.OnExecuteCommand := OnExecuteCommand1;
LSPClient1.OnWorkspaceApplyEdit := OnWorkspaceApplyEdit1;

procedure LSPSendExecuteCommand(const item: TLSPCommand);
var
  params: TLSPExecuteCommandParams;
begin
  if not LClient.IsRequestSupported(lspWorkspaceExecuteCommand) then Exit;

  // Send execute command request to server
  params := TLSPExecuteCommandParams.Create;
  params.command := item.command;
  params.arguments := item.arguments;

  LClient.SendRequest(lspWorkspaceExecuteCommand, '', params);
end;

procedure OnWorkspaceApplyEdit1(Sender: TObject; const value: TLSPApplyWorkspaceEditParams;
  var responseValue: TLSPApplyWorkspaceEditResponse; var errorCode: Integer; var errorMessage: string);
begin
  LSPApplyChanges(value.edit);
end;

procedure OnExecuteCommand1(Sender: TObject; Json: string; const errorCode: Integer;
  const errorMsg: string);
begin
  if errorMsg <> '' then
    OnError(Sender, errorCode, errorMsg);
end;
```

### DidChangeWorkspaceFolders Notification

The workspace/didChangeWorkspaceFolders notification is sent from the client to the server 
to inform the server about workspace folder configuration changes.

```
var
  params: TLSPDidChangeWorkspaceFoldersParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDidChangeWorkspaceFolders) then Exit;
  
  params := TLSPDidChangeWorkspaceFoldersParams.Create;

  SetLength(params.event.added, 1);
  params.event.added[i].name := 'NewFolder';
  params.event.added[i].uri := FilePathToUri('c:\source\NewFolder');

  SetLength(params.event.Removed, 1);
  params.event.removed[i].name := 'oldFolder';
  params.event.removed[i].uri := FilePathToUri('c:\source\OldFolder');

  FLSPClient1.SendRequest(lspDidChangeWorkspaceFolders, '', params);
end;
```

### DidChangeConfiguration Notification

A notification sent from the client to the server to signal the change of configuration settings.
The string is server dependent and can contain anything.
E.g. this could be sent to a CSS server.

```
  s := '{"settings": {
          "css": {
              "lint": {
                  "argumentsInColorFunction": "error",
                  "boxModel": "ignore",
                  "compatibleVendorPrefixes": "ignore",
                  ...
              },
              "trace": {
                  "server": "verbose"
              },
              "validate": true
          },
          "scss": {
              ...
          }
          }
        }';

   LSPClient.SendRequest(lspDidChangeConfiguration, '', nil, s);
```


### DidChangeWatchedFiles Notification

The watched files notification is sent from the client to the server when the client 
detects changes to files and folders watched by the language client.

```
// We have created a new file (new.cpp) and deleted (old.cpp). Let the server know.
// The change type. 1=created, 2=changed, 3=deleted

var
  params: TLSPDidChangeWatchedFilesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDidChangeWatchedFiles) then Exit;
  
  params := TLSPDidChangeWatchedFilesParams.Create;

  SetLength(params.changes, 2);
  params.changes[0].typ := 1;
  params.changes[0].uri := FilePathToUri('c:\source\new.cpp');
  params.changes[1].typ := 3;
  params.changes[1].uri := FilePathToUri('c:\source\old.cpp');
  
  FLSPClient1.SendRequest(lspDidChangeWatchedFiles, '', params);
end;
```

### DidCreateFiles Notification

The did create files notification is sent from the client to the server when files/folders
were created from within the client.

```
procedure DidCreateFiles(const files: TStringlist);
var
  i: Integer;
  params: TLSPCreateFilesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspWorkspaceDidCreateFiles) then Exit;
  
  params := TLSPCreateFilesParams.Create;

  SetLength(params.files, files.Count);
  for i := 0 to files.Count - 1 do
    params.files[i].uri := files[i];

  FLSPClient1.SendRequest(lspWorkspaceDidCreateFiles, '', params);
end;
```

### DidDeleteFiles Notification

The did delete files notification is sent from the client to the server when files/folders
were deleted from within the client.

```
procedure DidDeleteFiles(const files: TStringlist);
var
  i: Integer;
  params: TLSPDeleteFilesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspWorkspaceDidDeleteFiles) then Exit;
  
  params := TLSPDeleteFilesParams.Create;

  SetLength(params.files, files.Count);
  for i := 0 to files.Count - 1 do
    params.files[i].uri := files[i];

  FLSPClient1.SendRequest(lspWorkspaceDidDeleteFiles, '', params);
end;
```
 
### DidRenameFiles Notification

The did rename files notification is sent from the client to the server when files/folders
were renamed from within the client.

```
procedure DidRenameFiles(const files: TStringlist);
var
  i: Integer;
  params: TLSPRenameFilesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspWorkspaceDidRenameFiles) then Exit;
  
  params := TLSPRenameFilesParams.Create;

  SetLength(params.files, files.Count);
  for i := 0 to files.Count - 1 do
  begin
    params.files[i].oldUri := files.Names[i];
    params.files[i].newUri := files.ValueFromIndex[i];
  end;

  FLSPClient1.SendRequest(lspWorkspaceDidRenameFiles, '', params);
end;
```

### WillCreateFiles request

The will create files request is sent from the client to the server before files 
are actually created.

```
FLSPClient1.OnWorkspaceWillCreateFiles := OnWorkspaceWillCreateFiles1;

// Add files and folder paths to the stringlist and make the request
files.Add('c:\dummy\foo.c');
WillCreateFiles(files);

procedure WillCreateFiles(const files: TStringList);
var
  i: Integer;
  params: TLSPCreateFilesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspWorkspaceWillCreateFiles) then Exit;
  
  params := TLSPCreateFilesParams.Create;

  SetLength(params.files, files.Count);
  for i := 0 to files.Count - 1 do
    params.files[i].uri := files[i];

  FLSPClient1.SendRequest(lspWorkspaceWillCreateFiles, '', params);
end;

procedure OnWorkspaceWillCreateFiles1(Sender: TObject; const value: TLSPWorkspaceEdit);
begin
  // The request can return a WorkspaceEdit which will be applied to workspace before 
  // the files are created.
  if Assigned(value) then
  begin
    // Process value object
    ...
  end;
end;
```

### WillDeleteFiles request

The will delete files request is sent from the client to the server before files 
are actually deleted.

```
FLSPClient1.OnWorkspaceWillDeleteFiles := OnWorkspaceWillDeleteFiles1;

// Add files and folder paths to the stringlist and send the request
files.Add('c:\dummy\foo.c');
WillDeleteFiles(files);

procedure WillDeleteFiles(const files: TStringList);
var
  i: Integer;
  params: TLSPDeleteFilesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspWorkspaceWillDeleteFiles) then Exit;
  
  params := TLSPDeleteFilesParams.Create;

  SetLength(params.files, files.Count);
  for i := 0 to files.Count - 1 do
    params.files[i].uri := files[i];
  
  FLSPClient1.SendRequest(lspWorkspaceWillDeleteFiles, '', params);
end;

procedure OnWorkspaceWillDeleteFiles1(Sender: TObject; const value: TLSPWorkspaceEdit);
begin
  // The request can return a WorkspaceEdit which will be applied to workspace before 
  // the files are deleted.
  if Assigned(value) then
  begin
    // Process value object
    ...
  end;
end;
```

### WillRenameFiles request

The will rename files request is sent from the client to the server before files 
are actually renamed.

```
FLSPClient1.OnWorkspaceWillRenameFiles := OnWorkspaceWillRenameFiles1;

// Add old and new files and folder paths to the stringlist and send the request
old := 'c:\source\old_foo.c';
new := 'c:\source\new_foo.c';
files.Add(old + '=' + new);
WillRenameFiles(files);

procedure WillRenameFiles(const files: TStringList);
var
  i: Integer;
  params: TLSPRenameFilesParams;
begin
  if not FLSPClient1.IsRequestSupported(lspWorkspaceWillRenameFiles) then Exit;
  
  params := TLSPRenameFilesParams.Create;

  SetLength(params.files, files.Count);
  for i := 0 to files.Count - 1 do
  begin
    params.files[i].oldUri := files.Names[i];
    params.files[i].newUri := files.ValueFromIndex[i];
  end;

  FLSPClient1.SendRequest(lspWorkspaceWillRenameFiles, '', params);
end;

procedure OnWorkspaceWillRenameFiles1(Sender: TObject; const value: TLSPWorkspaceEdit);
begin
  // The request can return a WorkspaceEdit which will be applied to workspace before 
  // the files are renamed.
  if Assigned(value) then
  begin
    // Process value object
    ...
  end;
end;
```

### Workspace Symbols Request

The workspace symbol request is sent from the client to the server to list project-wide
symbols matching the query string.

```
  LSPClient1.OnWorkspaceSymbols := OnWorkspaceSymbols1;
  LSPClient1.OnWorkDoneProgress := OnWorkDoneProgress1;
  LSPClient1.OnProgress := OnProgress1;
  
  if not FLSPClient1.IsRequestSupported(lspWorkspaceSymbol) then Exit;
  params := TLSPWorkspaceSymbolParams.Create;
  params.query := 'single';
  params.workDoneToken := '28c6150c-bd7b-11e7-abc4-cec278b6b50a';
  params.partialResultToken := '';
  LSPClient1.SendRequest(lspWorkspaceSymbol, '', params);
  
  // Handle the response in the event handler
  procedure OnWorkspaceSymbolsRequest1(Sender: TObject; const symbols: TLSPSymbolInformations);
  var
    i: Integer;
    name: string;
    kind: TLSPSymbolKind;
    location: TLSPLocation;
  begin
    for i := 0 to Length(symbols) - 1 do
    begin
      name := symbols[i].name;
      kind := symbols[i].kind;
      location := symbols[i].location;
      
      // Process symbol (add to stringlist ...)
      ...
    end;
  end;
```
It is also possible to set a partial result token and catch the symbols in the
OnProgress event instead (see OnProgress example).

### Rename Request

The rename request is sent from the client to the server to ask the server to compute a 
workspace change so that the client can perform a workspace-wide rename of a symbol.

```
FLSPClient1.OnRename := OnRename1;

var
  i: Integer;
  params: TLSPRenameParams;
begin
  if not FLSPClient1.IsRequestSupported(lspRename) then Exit;
  
  params := TLSPRenameParams.Create;

  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.position.line := 1;
  params.position.character := 8;
  params.newName := 'foo';
  
  FLSPClient1.SendRequest(lspRename, '', params);
end;

procedure OnRename1(Sender: TObject; const value: TLSPWorkspaceEdit; const errorCode: Integer; 
    const errorMessage: string);
begin
  if Assigned(value) then
  begin
    // Process the changes to the document
    ApplyChanges(value);
  end;
end;
```

### Prepare Rename Request

The prepare rename request is sent from the client to the server to setup and test the 
validity of a rename operation at a given location.

```
FLSPClient1.OnPrepareRename := OnPrepareRename1;

var
  i: Integer;
  params: TLSPPrepareRenameParams;
begin
  if not FLSPClient1.IsRequestSupported(lspPrepareRename) then Exit;
  
  params := TLSPPrepareRenameParams.Create;

  params.textDocument.uri := FilePathToUri('c:\source\foo.cpp');
  params.position.line := 1;
  params.position.character := 8;
  
  FLSPClient1.SendRequest(lspPrepareRename, '', params);
end;

procedure OnPrepareRename1(Sender: TObject; const value: TLSPPrepareRenameResponse; const errorCode: Integer; 
    const errorMessage: string);
begin
  if Assigned(value) then
  begin
    // If value.defaultBehavior is true the rename position is valid and the client should
    // use its default behavior to compute the rename range.
    ...
      
    // Otherwise a range of the string to be renamed is provided and optionally a placeholder 
    // text of the string content to be renamed.
    ...
  end
  else
  begin
    // Rename is not valid at the given position
    ...
  end;
end;
```

### OnConfiguration

The workspace/configuration request is sent from the server to the client to fetch 
configuration settings from the client. The request can fetch several configuration
settings in one roundtrip. The order of the returned configuration settings correspond
to the order of the passed TLSPConfigurationItems.

If the client can’t provide a configuration setting for a given scope then null
needs to be present in the returned array.

```
procedure OnConfiguration1(Sender: TObject; const values: TLSPConfigurationParams; 
    var AJsonResult: string; var errorCode: Integer; var errorMessage: string);
var
  i: Integer;
  item: TLSPConfigurationItem;
  s: string;
begin
  for i := 0 to Length(values) do
  begin
    item := values[i];
    id s <> '' then s := s + ',';
    
    // Find configuration and return a Json string. Return null if the setting isn't found.
    s := s + GetConfigAsJson(item.scopeUri, item.section);
  end;
  AJsonResult := '[' + s + ']';
end;
```
 
### OnProgress

The base protocol offers support to report progress in a generic fashion. This 
mechanism can be used to report any kind of progress including work done progress 
(usually used to report progress in the user interface using a progress bar) and 
partial result progress to support streaming of results.

Below is an example of using progress events to recieve partial results. 
You need to set the partial result token in a request for this to work.

```
  LSPClient1.OnProgress := OnProgress1;
  
  params := TLSPWorkspaceSymbolParams.Create;
  params.query := 'single';
  params.workDoneToken := '';
  params.partialResultToken := '28c6150c-bd7b-11e7-abc4-cec278b6b50a';
  LSPClient1.SendRequest(lspWorkspaceSymbol, '', params);
  
  // Handle the responses in the OnProgress event handler
  procedure OnProgress1(Sender: TObject; const id: TLSPId; const value: TLSPBaseParams);
  var
    i: Integer;
    name: string;
    uri: string;
    range: TLSPRange;
    kind: TLSPSymbolKind;
    location: TLSPLocation;
    symbols: TLSPSymbolInformations;
    gotoInfo: TLSPGotoResponse;
  begin
    case id of
      lspWorkspaceSymbol:
      begin
        symbols := TLSPWorkspaceSymbolInformationParam(value).values;
        for i := 0 to Length(symbols) - 1 do
        begin
          name := symbols[i].name;
          kind := symbols[i].kind;
          location := symbols[i].location;
      
          // Process symbol (e.g. add to stringlist ...)
          ...
        end;
      end;
      lspGotoDeclaration:
      begin
        gotoInfo := TLSPGotoResponse(value);
        for i := 0 to Length(gotoInfo.locationLinks) - 1 do
        begin
          uri := gotoInfo.locationLinks[i].targetUri;
          range := gotoInfo.locationLinks[i].targetRange;
          ...
          // Maybe add to a list
          ...
        end;
      end;
    end;
  end;
```

### OnWorkspaceApplyEdit

The workspace/applyEdit request is sent from the server to the client to modify 
resource on the client side.

```
FLSPClient1.OnWorkspaceApplyEdit := OnWorkspaceApplyEdit1;

procedure OnWorkspaceApplyEdit1(Sender: TObject; const value: TLSPApplyWorkspaceEditParams; 
    var responseValue: TLSPApplyWorkspaceEditResponse; var errorCode: Integer; var errorMessage: string);
var
  i,j: Integer;
  lbl,sz: string;
  change: TLSPBaseParams;
  edit: TLSPTextEdit;
begin
  lbl := value.slabel;
  
  if Length(value.edit.documentChanges) > 0 then
  begin
    // Process changes
    for i := 0 to Length(value.edit.documentChanges) - 1 do
    begin
      change := value.edit.documentChanges[i];
      if change is TLSPTextDocumentEdit then
      begin
        // Retrieve document path
        sz := FLSPClient.UriToFilePath(TLSPTextDocumentEdit(change).textDocument.uri);
        
        // Get all changes to the document
        for j := 0 to Length(TLSPTextDocumentEdit(change).edits) - 1 do
        begin
          edit := TLSPTextDocumentEdit(change).edits[j];
                    
          // Apply changes
          ModifyDocument(sz, edit.newText, edit.range);
        end;
      end;
    end;
    responseValue.applied := True;
  end;
end;
```

## Language identifiers

``` 
Language           Identifier
-------------------------------
ABAP               abap
Windows Bat        bat
BibTeX             bibtex
Clojure            clojure
Coffeescript       coffeescript
C                  c
C++                cpp
C#                 csharp
CSS                css
Diff               diff
Dart               dart
Dockerfile         dockerfile
Elixir             elixir
Erlang             erlang
F#                 fsharp
Git                git-commit and git-rebase
Go                 go
Groovy             groovy
Handlebars         handlebars
HTML               html
Ini                ini
Java               java
JavaScript         javascript
JavaScript React   javascriptreact
JSON               json
LaTeX              latex
Less               less
Lua                lua
Makefile           makefile
Markdown           markdown
Objective-C        objective-c
Objective-C++      objective-cpp
Perl               perl
Perl 6             perl6
PHP                php
Powershell         powershell
Pug                jade
Python             python
R                  r
Razor (cshtml)     razor
Ruby               ruby
Rust               rust
SCSS               scss (syntax using curly brackets), sass (indented syntax)
Scala              scala
ShaderLab          shaderlab
Shell Script       shellscript
SQL                sql
Swift              swift
TypeScript         typescript
TypeScript React   typescriptreact
TeX                tex
Visual Basic       vb
XML                xml
XSL                xsl
YAML               yaml
```

## License

Copyright (c) 2021+, Rickard Johansson (https://www.rj-texted.se)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
