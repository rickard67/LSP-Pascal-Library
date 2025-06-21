# Move from version 1.x to version 2.x
This is a new version of the component with many code changes. If you use the old implementation you will have to make several changes in order to use this new version.

But, don't worry; many features still work basically the same.

## Events
Most events are defined with different arguments in the new version. You can find all declared events in XLSPClient.pas.

```pascal
E.g.
    TOnCodeActionResolveEvent = procedure(Sender: TObject; const value: TLSPCodeAction; const errorCode: Integer; const errorMessage: string) of object;
```

is now declared as

```pascal
    TOnCodeActionResolveEvent = procedure(Sender: TObject; const Id: Integer; const value: TLSPCodeActionResolveResult) of object;
```

Id is an unique number to identify the request made to the server (in case you made several requests).
TLSPCodeAction has changed to TLSPCodeActionResolveResult and errorCode and errorMessage has been removed.


## Values
Some values have changed name.

```
    .slabel         =>   .&label
    .startPos       =>   .start
    .endPos         =>   .&end
    .messageString  =>   .&message

E.g.
    startx := LRange.start.character;
    starty := LRange.start.line;
    endx := LRange.&end.character;
    endy := LRange.&end.line;
```


## Notifications
Notifications are not sent using SendRequest() anymore. Use SendNotification().

This means you have to change the function call for the following notification calls:

```pascal
LSPClientNotifications: set of TLSPKind = [
    lspInitialized, lspExit, lspWorkDoneProgressCancel,
    lspDidOpenTextDocument, lspDidChangeTextDocument,
    lspDidSaveTextDocument, lspDidCloseTextDocument,
    lspWillSaveTextDocument, lspDidChangeWatchedFiles,
    lspDidOpenNotebookDocument, lspDidChangeNotebookDocument,
    lspDidSaveNotebookDocument, lspDidCloseNotebookDocument,
    lspDidChangeWorkspaceFolders, lspDidChangeConfiguration,
    lspWorkspaceDidCreateFiles, lspWorkspaceDidRenameFiles,
    lspWorkspaceDidDeleteFiles, lspSetTrace
  ];
```

