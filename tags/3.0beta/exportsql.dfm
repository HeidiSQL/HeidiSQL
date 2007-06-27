object ExportSQLForm: TExportSQLForm
  Left = 429
  Top = 112
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'Export Tables...'
  ClientHeight = 405
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 367
    Width = 558
    Height = 38
    Align = alBottom
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 144
    Height = 13
    Caption = 'Select Database and Table(s):'
  end
  object Label2: TLabel
    Left = 16
    Top = 328
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Button1: TButton
    Left = 395
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Export!'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 477
    Top = 376
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object TablesCheckListBox: TCheckListBox
    Left = 16
    Top = 48
    Width = 217
    Height = 145
    ItemHeight = 13
    TabOrder = 2
  end
  object DBComboBox: TComboBox
    Left = 16
    Top = 24
    Width = 169
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = DBComboBoxChange
  end
  object GroupBox1: TGroupBox
    Left = 256
    Top = 80
    Width = 297
    Height = 241
    Caption = 'Output'
    TabOrder = 4
    object BitBtn1: TBitBtn
      Left = 259
      Top = 43
      Width = 22
      Height = 22
      TabOrder = 0
      OnClick = BitBtn1Click
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777777777777777777777000000000007777700333333333077770B033333333
        307770FB03333333330770BFB0333333333070FBFB000000000070BFBFBFBFB0
        777770FBFBFBFBF0777770BFB000000077777700077777777000777777777777
        7700777777777077707077777777770007777777777777777777}
    end
    object EditFileName: TEdit
      Left = 32
      Top = 44
      Width = 225
      Height = 21
      TabOrder = 1
      Text = 'EditFileName'
      OnDblClick = BitBtn1Click
    end
    object RadioButtonDB: TRadioButton
      Left = 16
      Top = 85
      Width = 113
      Height = 17
      Caption = 'Another Database:'
      TabOrder = 2
      OnClick = RadioButtonDBClick
    end
    object RadioButtonFile: TRadioButton
      Left = 16
      Top = 24
      Width = 49
      Height = 17
      Caption = 'File:'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RadioButtonFileClick
    end
    object ComboBoxODB: TComboBox
      Left = 32
      Top = 103
      Width = 249
      Height = 21
      Style = csDropDownList
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 4
    end
    object RadioButtonHost: TRadioButton
      Left = 16
      Top = 151
      Width = 113
      Height = 17
      Caption = 'Another Host / DB:'
      TabOrder = 5
      OnClick = RadioButtonHostClick
    end
    object ComboBoxHost: TComboBox
      Left = 32
      Top = 171
      Width = 249
      Height = 21
      Style = csDropDownList
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 6
    end
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 344
    Width = 537
    Height = 17
    Min = 0
    Max = 0
    Smooth = True
    Step = 1
    TabOrder = 5
  end
  object GroupBox2: TGroupBox
    Left = 256
    Top = 16
    Width = 297
    Height = 57
    Caption = 'What to export'
    TabOrder = 6
    object CheckBox1: TCheckBox
      Left = 16
      Top = 24
      Width = 73
      Height = 17
      Caption = 'Structure'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 112
      Top = 24
      Width = 73
      Height = 17
      Caption = 'Data'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox2Click
    end
  end
  object ToolBar1: TToolBar
    Left = 187
    Top = 24
    Width = 46
    Height = 22
    Align = alNone
    AutoSize = True
    Caption = 'ToolBar1'
    EdgeInner = esNone
    EdgeOuter = esNone
    Flat = True
    Images = MainForm.ImageList1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Hint = 'Check none'
      Caption = 'ToolButton1'
      ImageIndex = 42
      OnClick = CheckListToggle
    end
    object ToolButton2: TToolButton
      Tag = 1
      Left = 23
      Top = 0
      Hint = 'Check all'
      Caption = 'ToolButton2'
      ImageIndex = 43
      OnClick = CheckListToggle
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 200
    Width = 217
    Height = 121
    Caption = 'Options'
    TabOrder = 8
    object CheckBoxWithUseDB: TCheckBox
      Left = 8
      Top = 24
      Width = 193
      Height = 17
      Caption = 'Include "USE dbname"-Statement'
      TabOrder = 0
    end
    object CheckBoxWithDropTable: TCheckBox
      Left = 8
      Top = 48
      Width = 193
      Height = 17
      Caption = 'Include "DROP TABLE"-Statements'
      TabOrder = 1
    end
    object CheckBoxCompleteInserts: TCheckBox
      Left = 8
      Top = 72
      Width = 193
      Height = 17
      Caption = 'Complete insert statements'
      TabOrder = 2
    end
    object CheckBoxUseBackticks: TCheckBox
      Left = 8
      Top = 96
      Width = 193
      Height = 17
      Caption = 'Use backticks (`) for names'
      TabOrder = 3
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 520
    Top = 96
  end
end
