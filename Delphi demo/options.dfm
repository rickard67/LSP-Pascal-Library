object OptionsForm: TOptionsForm
  Left = 0
  Top = 0
  Caption = 'OptionsForm'
  ClientHeight = 495
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 53
    Width = 52
    Height = 13
    Caption = 'LSP Server'
  end
  object Label2: TLabel
    Left = 32
    Top = 109
    Width = 52
    Height = 13
    Caption = 'Arguments'
  end
  object Label3: TLabel
    Left = 32
    Top = 221
    Width = 65
    Height = 13
    Caption = 'LSP root path'
  end
  object Label4: TLabel
    Left = 32
    Top = 285
    Width = 225
    Height = 13
    Caption = 'Options (initialize details as JSON object string)'
  end
  object Label5: TLabel
    Left = 16
    Top = 16
    Width = 220
    Height = 13
    Caption = 'Provide settings to start the language server.'
  end
  object Label6: TLabel
    Left = 32
    Top = 165
    Width = 41
    Height = 13
    Caption = 'Initial dir'
  end
  object EditServer: TEdit
    Left = 32
    Top = 72
    Width = 377
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 415
    Top = 70
    Width = 58
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object EditArguments: TEdit
    Left = 32
    Top = 128
    Width = 377
    Height = 21
    TabOrder = 2
    Text = '--stdio'
  end
  object EditRootPath: TEdit
    Left = 32
    Top = 240
    Width = 377
    Height = 21
    TabOrder = 3
  end
  object Button2: TButton
    Left = 415
    Top = 238
    Width = 58
    Height = 25
    Caption = '...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object MemoOptions: TMemo
    Left = 32
    Top = 304
    Width = 377
    Height = 89
    Lines.Strings = (
      '')
    TabOrder = 5
  end
  object btnOk: TButton
    Left = 288
    Top = 448
    Width = 90
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 6
  end
  object btnCancel: TButton
    Left = 384
    Top = 448
    Width = 89
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object btnInitDir: TButton
    Left = 415
    Top = 182
    Width = 58
    Height = 25
    Caption = '...'
    TabOrder = 8
    OnClick = btnInitDirClick
  end
  object EditInitDir: TEdit
    Left = 32
    Top = 184
    Width = 377
    Height = 21
    TabOrder = 9
  end
  object OpenDialog1: TOpenDialog
    Left = 40
    Top = 416
  end
end
