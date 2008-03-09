object optimize: Toptimize
  Left = 734
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Table-Diagnostics'
  ClientHeight = 386
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 145
    Height = 13
    Caption = 'Select Database and Table(s):'
  end
  object Label3: TLabel
    Left = 16
    Top = 224
    Width = 39
    Height = 13
    Caption = 'Results:'
  end
  object TablesCheckListBox: TCheckListBox
    Left = 16
    Top = 56
    Width = 218
    Height = 145
    OnClickCheck = TablesCheckListBoxClickCheck
    Columns = 2
    ItemHeight = 13
    TabOrder = 2
  end
  object DBComboBox: TComboBox
    Left = 62
    Top = 32
    Width = 171
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = DBComboBoxChange
  end
  object Button3: TButton
    Left = 328
    Top = 360
    Width = 99
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    TabOrder = 12
  end
  object CheckBoxQuickRepair: TCheckBox
    Left = 360
    Top = 168
    Width = 57
    Height = 17
    Caption = 'Quick'
    TabOrder = 9
  end
  object CheckBoxQuickCheck: TCheckBox
    Left = 360
    Top = 88
    Width = 57
    Height = 17
    Caption = 'Quick'
    TabOrder = 5
  end
  object btnOptimize: TButton
    Left = 248
    Top = 56
    Width = 99
    Height = 25
    Caption = 'Optimze'
    TabOrder = 3
    OnClick = Optimze
  end
  object btnCheck: TButton
    Left = 248
    Top = 96
    Width = 99
    Height = 25
    Caption = 'Check'
    TabOrder = 4
    OnClick = Check
  end
  object btnAnalyze: TButton
    Left = 248
    Top = 136
    Width = 99
    Height = 25
    Caption = 'Analyze'
    TabOrder = 7
    OnClick = Analyze
  end
  object btnRepair: TButton
    Left = 248
    Top = 176
    Width = 99
    Height = 25
    Caption = 'Repair'
    TabOrder = 8
    OnClick = Repair
  end
  object ListViewResults: TListView
    Left = 16
    Top = 240
    Width = 409
    Height = 105
    Columns = <>
    ColumnClick = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 11
    ViewStyle = vsReport
  end
  object ToolBar1: TToolBar
    Left = 16
    Top = 32
    Width = 46
    Height = 22
    Align = alNone
    AutoSize = True
    Caption = 'ToolBar1'
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = MainForm.PngImageListMain
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Hint = 'Check none'
      Caption = 'ToolButton1'
      ImageIndex = 65
      ParentShowHint = False
      ShowHint = True
      OnClick = CheckListToggle
    end
    object ToolButton2: TToolButton
      Tag = 1
      Left = 23
      Top = 0
      Hint = 'Check all'
      Caption = 'ToolButton2'
      ImageIndex = 64
      ParentShowHint = False
      ShowHint = True
      OnClick = CheckListToggle
    end
  end
  object cbxExtendedCheck: TCheckBox
    Left = 360
    Top = 111
    Width = 70
    Height = 17
    Caption = 'Extended'
    TabOrder = 6
  end
  object cbxExtendedRepair: TCheckBox
    Left = 360
    Top = 191
    Width = 70
    Height = 17
    Caption = 'Extended'
    TabOrder = 10
  end
end
