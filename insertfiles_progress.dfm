object frmInsertFilesProgress: TfrmInsertFilesProgress
  Left = 609
  Top = 147
  BorderStyle = bsDialog
  Caption = 'Inserting files...'
  ClientHeight = 142
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 32
    Width = 45
    Height = 13
    Caption = 'Filename:'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 84
    Height = 13
    Caption = 'Current operation:'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 14
    Height = 13
    Caption = 'Nr.'
  end
  object Label4: TLabel
    Left = 112
    Top = 16
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 112
    Top = 32
    Width = 32
    Height = 13
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 112
    Top = 48
    Width = 32
    Height = 13
    Caption = 'Label6'
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 72
    Width = 385
    Height = 17
    Step = 1
    TabOrder = 0
  end
  object Button1: TButton
    Left = 168
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = ProcessFiles
    Left = 24
    Top = 104
  end
end
