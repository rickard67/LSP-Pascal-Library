// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program LSPDemoProject;

uses
  Vcl.Forms,
  LSPDemo in 'LSPDemo.pas' {LSPDemoForm},
  options in 'options.pas' {OptionsForm},
  XLSPClient in '..\source\XLSPClient.pas',
  XLSPExecute in '..\source\XLSPExecute.pas',
  XLSPFunctions in '..\source\XLSPFunctions.pas',
  XLSPTypes in '..\source\XLSPTypes.pas',
  XSuperJSON in '..\source\x-superobject\XSuperJSON.pas',
  XSuperObject in '..\source\x-superobject\XSuperObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLSPDemoForm, LSPDemoForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.



