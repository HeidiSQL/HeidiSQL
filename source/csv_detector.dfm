object frmCsvDetector: TfrmCsvDetector
  Left = 0
  Top = 0
  Caption = 'Detect CSV layout'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    635
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object btnScan: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Scan file...'
    Images = MainForm.VirtualImageListMain
    TabOrder = 0
    OnClick = btnScanClick
  end
  object SynMemoCreateTable: TSynMemo
    Left = 8
    Top = 39
    Width = 619
    Height = 221
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    FontSmoothing = fsmNone
  end
  object btnCancel: TButton
    Left = 505
    Top = 266
    Width = 122
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnSave: TButton
    Left = 377
    Top = 266
    Width = 122
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok, create table'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object TimerStartScan: TTimer
    Enabled = False
    Interval = 100
    OnTimer = btnScanClick
    Left = 56
    Top = 64
  end
end
