object optimize: Toptimize
  Left = 734
  Top = 126
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Maintenance'
  ClientHeight = 409
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    431
    409)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSelect: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 13
    Caption = 'Select Database and Table(s):'
  end
  object lblResults: TLabel
    Left = 8
    Top = 211
    Width = 39
    Height = 13
    Caption = 'Results:'
  end
  object TablesCheckListBox: TCheckListBox
    Left = 8
    Top = 50
    Width = 239
    Height = 155
    OnClickCheck = TablesCheckListBoxClickCheck
    Anchors = [akLeft, akTop, akRight]
    Columns = 2
    ItemHeight = 13
    TabOrder = 2
  end
  object DBComboBox: TComboBox
    Left = 54
    Top = 26
    Width = 193
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = DBComboBoxChange
  end
  object btnClose: TButton
    Left = 326
    Top = 378
    Width = 99
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    TabOrder = 12
  end
  object cbxQuickRepair: TCheckBox
    Left = 358
    Top = 170
    Width = 57
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Quick'
    TabOrder = 9
  end
  object cbxQuickCheck: TCheckBox
    Left = 358
    Top = 90
    Width = 57
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Quick'
    TabOrder = 5
  end
  object btnOptimize: TButton
    Left = 253
    Top = 50
    Width = 99
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Optimize'
    TabOrder = 3
    OnClick = Optimize
  end
  object btnCheck: TButton
    Left = 253
    Top = 90
    Width = 99
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Check'
    TabOrder = 4
    OnClick = Check
  end
  object btnAnalyze: TButton
    Left = 253
    Top = 130
    Width = 99
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Analyze'
    TabOrder = 7
    OnClick = Analyze
  end
  object btnRepair: TButton
    Left = 253
    Top = 170
    Width = 99
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Repair'
    TabOrder = 8
    OnClick = Repair
  end
  object ListResults: TListView
    Left = 8
    Top = 230
    Width = 417
    Height = 142
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    ColumnClick = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 11
    ViewStyle = vsReport
  end
  object tlbCheckToggle: TToolBar
    Left = 8
    Top = 26
    Width = 46
    Height = 22
    Align = alNone
    AutoSize = True
    Caption = 'tlbCheckToggle'
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = MainForm.PngImageListMain
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object tlbCheckNone: TToolButton
      Left = 0
      Top = 0
      Hint = 'Check none'
      Caption = 'tlbCheckNone'
      ImageIndex = 65
      ParentShowHint = False
      ShowHint = True
      OnClick = CheckListToggle
    end
    object tlbCheckAll: TToolButton
      Tag = 1
      Left = 23
      Top = 0
      Hint = 'Check all'
      Caption = 'tlbCheckAll'
      ImageIndex = 64
      ParentShowHint = False
      ShowHint = True
      OnClick = CheckListToggle
    end
  end
  object cbxExtendedCheck: TCheckBox
    Left = 358
    Top = 108
    Width = 70
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Extended'
    TabOrder = 6
  end
  object cbxExtendedRepair: TCheckBox
    Left = 358
    Top = 188
    Width = 70
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Extended'
    TabOrder = 10
  end
end
