unit options;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TOptionsForm = class(TForm)
    EditServer: TEdit;
    Label1: TLabel;
    Button1: TButton;
    EditArguments: TEdit;
    Label2: TLabel;
    EditRootPath: TEdit;
    Label3: TLabel;
    Button2: TButton;
    MemoOptions: TMemo;
    Label4: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    Label5: TLabel;
    OpenDialog1: TOpenDialog;
    btnInitDir: TButton;
    EditInitDir: TEdit;
    Label6: TLabel;
    procedure btnInitDirClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  Vcl.FileCtrl;

{$R *.dfm}

procedure TOptionsForm.btnInitDirClick(Sender: TObject);
var
  sd: string;
begin
  sd := EditInitDir.Text;
  if not SelectDirectory(sd, [sdPrompt],0) then Exit;

  EditInitDir.Text := sd;
end;

procedure TOptionsForm.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute(Handle) then Exit;

  EditServer.Text := OpenDialog1.FileName;
end;

procedure TOptionsForm.Button2Click(Sender: TObject);
var
  sd: string;
begin
  sd := EditRootPath.Text;
  if not SelectDirectory(sd, [sdPrompt],0) then Exit;

  EditRootPath.Text := sd;
end;

end.
