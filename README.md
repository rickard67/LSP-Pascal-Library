# Delphi/Pascal LSP Client

A language server protocal client written in Pascal (Delphi).

## Basic Information

The LSP client was written to make communication with language servers easier. Use
the client to read or send notifications and requests to and from the server. Handle
the notifications and request from the server using events.

This component was created for use in RJ TextEd to add language server support. It has been
tested with several language servers (https://www.rj-texted.se/Forum/viewforum.php?f=23).

The client component support both stdio and tcp/ip socket communication. When communicating
over a socket - the client act as a server, which is why you must add a port as an argument
when starting the server. E.g. server.exe --port=5000.

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
<AppData>\Roaming\npm

##### LSP root path
..\LSP-Pascal-Library\demo\examples\Class Member

## Using the Library

Add one or more client components to a form or create clients at runtime. You need one client
for each LSP server you intend to run.

### Running LSP server(s)

Use the function below to run a server.

``` LSPClient1.RunServer(const ACommandline, ADir: string); ```

### Closing or exiting the server

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

## Send requests and notifications to the server

Use the function "SendRequest" to send a request or notification to the server. The first
argument indicate the type of request and automatically set the "method" in the Json
request that is sent to the server.

The function is declared as:

```
procedure TLSPClient.SendRequest(const lspKind: TLSPKind; const method: string = ''; 
    const params: TLSPBaseParams = nil; const paramJSON: string = '');
```

### Initialize

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
  value.AddRoot(FServerRootPath);

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

#### Shutdown Request

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


#### Exit Notification

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

#### DidOpenTextDocument Notification

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
  params.textDocument.uri := FilePathToUri('c:\source\foo.cs');
  params.textDocument.version := 1;
  params.textDocument.languageId := 'php';
  params.textDocument.text := Memo1.text;

  FLSPClient1.SendRequest(lspDidOpenTextDocument, '', params);
 ```
 Language identifiers are listed at the end of this document.
 
1. A language server can handle several different languageid values. E.g. 'typescript' and 'javascript'. You could connect a file extension to a language id.
2. Version should be an incremental value that increse with every change (didChangeTextDocument).
 
#### DidChangeTextDocument Notification

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

#### DidCloseTextDocument Notification

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

#### DidSaveTextDocument Notification

The document save notification is sent from the client to the server when the document 
was saved in the client.

```
var
  params: TLSPDidSaveTextDocumentParams;
begin
  if not LClient.IsRequestSupported(lspDidSaveTextDocument) then Exit;
  
  params := TLSPDidSaveTextDocumentParams.Create;
  params.textDocument.uri := 'c:\source\foo.c';
  
  // Optionally include the content when saved. Depends on the includeText option value.
  if LClient.IncludeText(lspDidSaveTextDocument, sz, False) then
  begin
    params.text := Memo1.Lines.Text;
  end;
  
  LClient.SendRequest(lspDidSaveTextDocument, '', params); 
end;
```

#### WillSaveTextDocument Notification

The document will save notification is sent from the client to the server before the 
document is actually saved.

```
var
  i: Integer;
  params: TLSPWillSaveTextDocumentParams;
begin
  if not LClient.IsRequestSupported(lspWillSaveTextDocument) then Exit;
  
  params := TLSPWillSaveTextDocumentParams.Create;

  params.textDocument.uri := 'c:\source\foo.cpp';
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

#### WillSaveWaitUntilTextDocument Request

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

  params.textDocument.uri := 'c:\source\foo.cpp';
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

#### Completion Request

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

#### Completion Item Resolve Request

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

#### Hover request

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

#### OnPublishDiagnostics

Diagnostics notification are sent from the server to the client to signal results of 
validation runs. 
Use this in the application to display errors, warnings, information or hints
generated by the language server.

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

##### Signature Help Request

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

## Goto Requests

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

#### Document Highlight Request

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

#### Document Symbols Request

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
  params.textDocument.uri := 'c:\source\foo.java';
  
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

#### Code Action Request

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
  params.textDocument.uri := 'c:\source\foo.js';
  
  // Code action kind
  SetLength(params.context.only, 1);
  params.context.only[0] := 'refactor.rewrite';
  
  // Range
  params.range.startPos.line := 2;
  params.range.startPos.character := 0;
  params.range.endPos.line := 16;
  params.range.endPos.character := 0;  
  
  FLSPClient1.SendRequest(lspCodeAction, '', params);
end;

procedure OnCodeAction1(Sender: TObject; const value: TLSPCodeActionResponse; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  title: string;
  kind: TLSPCodeActionKind;
  edit: TLSPWorkspaceEdit;
begin
  for i := 0 to value.codeActions.Count - 1 do
  begin
    title := value.codeActions[i].title;
    kind := value.codeActions[i].kind;
    edit := value.codeActions[i].edit;
    AddToList(title, kind, edit);
  end;
  ApplyChanges;
end;
```

#### Code Action Resolve Request

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
var
  title: string;
  edit: TLSPWorkspaceEdit;
begin
  title := value.title;
  edit := value.edit;
  
  ApplyChanges(title, edit);
end;
```

#### Code Lens Request

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
  params.textDocument.uri := 'c:\source\foo.pas';
  
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

#### Code Lens Resolve Request

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

#### Code Lens Refresh Request

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

#### Document Link Request

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
  params.textDocument.uri := 'c:\source\foo.cpp';
  
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

#### Document Link Resolve Request

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

#### Document Color Request

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
  params.textDocument.uri := 'c:\source\foo.cpp';
  
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

#### Color Presentation Request

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
  params.textDocument.uri := 'c:\source\foo.cpp';
  
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

#### Document Formatting Request

The document formatting request is sent from the client to the server to format a 
whole document.

```
// Event to catch response from the server
FLSPClient1.OnDocumentFormatting := OnDocumentFormatting1;

var
  params: TLSPDocumentFormattingParams;
begin
  if not FLSPClient1.IsRequestSupported(lspDocumentFormatting) then Exit;
  
  params := TLSPDocumentFormattingParams.Create;
  params.textDocument.uri := 'c:\source\foo.cpp';
  params.options.tabSize := 2;
  params.options.insertSpaces := True;
  params.options.trimTrailingWhitespace := true;

  FLSPClient.SendRequest(lspDocumentFormatting, '', params);
end;

procedure OnDocumentFormatting1(Sender: TObject; const values: TLSPTextEditValues; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  edit: TLSPTextEdit;
begin
  for i := 0 to values.edits.Count - 1 do
  begin
    edit := values.edits[i];
    ModifyDocument(edit.newText, edit.range);
  end;
end;
```

#### Document Range Formatting Request

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
  params.textDocument.uri := 'c:\source\foo.cpp';
  params.options.tabSize := 2;
  params.options.insertSpaces := True;
  params.options.trimTrailingWhitespace := true;
  
  params.range.startPos.line := 10;
  params.range.startPos.character := 0;
  params.range.endPos.line := 200;
  params.range.endPos.character := 0;

  FLSPClient.SendRequest(lspDocumentRangeFormatting, '', params);
end;

procedure OnDocumentRangeFormatting1(Sender: TObject; const values: TLSPTextEditValues; const errorCode: Integer; 
    const errorMessage: string);
var
  i: Integer;
  edit: TLSPTextEdit;
begin
  for i := 0 to values.edits.Count - 1 do
  begin
    edit := values.edits[i];
    ModifyDocument(edit.newText, edit.range);
  end;
end;
```

#### Document OnTypeFormatting Request

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
  
  params.textDocument.uri := 'c:\source\foo.cpp';
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
  for i := 0 to values.edits.Count - 1 do
  begin
    edit := values.edits[i];
    ModifyDocument(edit.newText, edit.range);
  end;
end;
```

#### Execute Command Request

The workspace/executeCommand request is sent from the client to the server to trigger
command execution on the server.

```
// Set event handler
LSPClient1.OnExecuteCommand := OnExecuteCommand1;

// Command
sc := 'server.command-112233';

// Arguments (optional)
args := '[{"range":{"end":{"character":14,"line":14},"start":{"character":0,"line":14}},
         "uri":"file:///Sources/main.rs"},"create_function"]';
         
// Send request
LSPClient1.SendExecuteCommand(sc, args);

You can also create a class or record and use x-superobject to create the JSON string.

myArgs := TMyArgs;

// Set arguments
myArgs.range.start.character := 0;
...

args := '[' + myArgs.AsJSON + ']';

```

#### DidChangeWorkspaceFolders Notification

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
  params.event.added[i].uri := 'c:\source\NewFolder';

  SetLength(params.event.Removed, 1);
  params.event.removed[i].name := 'oldFolder';
  params.event.removed[i].uri := 'c:\source\OldFolder';

  FLSPClient1.SendRequest(lspDidChangeWorkspaceFolders, '', params);
end;
```

#### DidChangeConfiguration Notification

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


#### DidChangeWatchedFiles Notification

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
  params.changes[0].uri := 'c:\source\new.cpp';
  params.changes[1].typ := 3;
  params.changes[1].uri := 'c:\source\old.cpp';
  
  FLSPClient1.SendRequest(lspDidChangeWatchedFiles, '', params);
end;
```

#### DidCreateFiles Notification

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

#### DidDeleteFiles Notification

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
 
#### DidRenameFiles Notification

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

#### WillCreateFiles request

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

#### WillDeleteFiles request

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

#### WillRenameFiles request

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

#### Workspace Symbols Request

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

#### Rename Request

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

  params.textDocument.uri := 'c:\source\foo.cpp';
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

#### Prepare Rename Request

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

  params.textDocument.uri := 'c:\source\foo.cpp';
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

#### Folding Range Request

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
  params.textDocument.uri := 'c:\source\foo.cpp';
  
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

#### Selection Range Request

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
  params.textDocument.uri := 'c:\source\foo.cpp';
  
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

#### Prepare Call Hierarchy Request

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
  params.textDocument.uri := 'c:\source\foo.cpp';
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

#### Call Hierarchy Incomming Calls

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

#### Call Hierarchy Outgoing Calls

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

### Semantic Tokens

The request is sent from the client to the server to resolve semantic tokens for a given 
file. Semantic tokens are used to add additional color information to a file that depends 
on language specific symbol information. A semantic token request usually produces a large 
result. The protocol therefore supports encoding tokens with numbers. In addition optional 
support for deltas is available.

The request support streaming by setting a partial result id.

#### Semantic Tokens for a whole file

```
FLSPClient1.OnSemanticTokensFull := OnSemanticTokensFull1;

var
  i: Integer;
  params: TLSPSemanticTokensParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSemanticTokensFull) then Exit;
  
  // Find all tokens in the given file
  params.textDocument.uri := 'c:\source\foo.h';
    
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

#### Semantic Tokens delta for a whole file

```
FLSPClient1.OnSemanticTokensFullDelta := OnSemanticTokensFullDelta1;

var
  i: Integer;
  params: TLSPSemanticTokensDeltaParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSemanticTokensDelta) then Exit;
  
  // Find all tokens in the given file
  params.textDocument.uri := 'c:\source\foo.h';
  
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

#### Semantic Tokens for a range

```
FLSPClient1.OnSemanticTokensRange := OnSemanticTokensRange1;

var
  i: Integer;
  params: TLSPSemanticTokensRangeParams;
begin
  if not FLSPClient1.IsRequestSupported(lspSemanticTokensRange) then Exit;
  
  // Find all tokens in the given file
  params.textDocument.uri := 'c:\source\foo.h';
  
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

#### Semantic Tokens Refresh

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

#### Linked Editing Range

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
  
  // Find all instances of the text (e.g. variable) in the document
  params.textDocument.uri := 'c:\source\foo.h';
  
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

#### Monikers

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
  
  // Get symbol monikers from document
  params.textDocument.uri := 'c:\source\foo.h';
  
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

#### OnConfiguration

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
 
#### OnProgress

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

#### OnWorkspaceApplyEdit

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
