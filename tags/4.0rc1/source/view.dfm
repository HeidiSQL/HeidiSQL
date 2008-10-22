object frmView: TfrmView
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'frmView'
  ClientHeight = 275
  ClientWidth = 422
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 360
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    422
    275)
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblSelect: TLabel
    Left = 8
    Top = 124
    Width = 85
    Height = 13
    Caption = 'Select statement:'
  end
  object editName: TEdit
    Left = 45
    Top = 5
    Width = 371
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'editName'
    OnChange = editNameChange
  end
  object rgAlgorithm: TRadioGroup
    Left = 8
    Top = 32
    Width = 201
    Height = 86
    Caption = 'Algorithm'
    ItemIndex = 0
    Items.Strings = (
      'Undefined'
      'Merge'
      'Temptable')
    TabOrder = 1
  end
  object SynMemoSelect: TSynMemo
    Left = 8
    Top = 143
    Width = 408
    Height = 95
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 10
    Gutter.RightOffset = 0
    Gutter.ShowLineNumbers = True
    Options = [eoAutoIndent, eoDropFiles, eoGroupUndo, eoShowScrollHint]
    RightEdge = 40
    WantTabs = True
  end
  object btnCancel: TButton
    Left = 341
    Top = 244
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 260
    Top = 244
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOKClick
  end
  object rgCheck: TRadioGroup
    Left = 215
    Top = 32
    Width = 201
    Height = 86
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Check option for updates'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Cascaded'
      'Local')
    TabOrder = 5
  end
  object btnHelp: TButton
    Left = 8
    Top = 244
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = btnHelpClick
  end
end
