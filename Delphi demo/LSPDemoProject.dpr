// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program LSPDemoProject;

uses
  Vcl.Forms,
  LSPDemo in 'LSPDemo.pas' {LSPDemoForm},
  options in 'options.pas' {OptionsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLSPDemoForm, LSPDemoForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.



