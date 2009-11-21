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
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 32
    Width = 46
    Height = 13
    Caption = 'Filename:'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 90
    Height = 13
    Caption = 'Current operation:'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 15
    Height = 13
    Caption = 'Nr.'
  end
  object lblNumber: TLabel
    Left = 131
    Top = 16
    Width = 47
    Height = 13
    Caption = 'lblNumber'
  end
  object lblFilename: TLabel
    Left = 131
    Top = 32
    Width = 52
    Height = 13
    Caption = 'lblFilename'
  end
  object lblOperation: TLabel
    Left = 131
    Top = 48
    Width = 58
    Height = 13
    Caption = 'lblOperation'
  end
  object pbReadingFiles: TProgressBar
    Left = 16
    Top = 72
    Width = 385
    Height = 17
    Step = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 168
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object timerStartReading: TTimer
    Enabled = False
    Interval = 1
    OnTimer = ProcessFiles
    Left = 24
    Top = 104
  end
end
