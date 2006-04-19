object ExportSQLForm: TExportSQLForm
  Left = 429
  Top = 106
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'Export Tables...'
  ClientHeight = 343
  ClientWidth = 558
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
  object Label2: TLabel
    Left = 8
    Top = 264
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Button1: TButton
    Left = 395
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Export!'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 477
    Top = 312
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object GroupBox1: TGroupBox
    Left = 256
    Top = 0
    Width = 297
    Height = 257
    Caption = 'Output'
    TabOrder = 2
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
    Left = 8
    Top = 280
    Width = 545
    Height = 17
    Min = 0
    Max = 0
    Smooth = True
    Step = 1
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 4
    Top = 0
    Width = 241
    Height = 257
    ActivePage = TabSheet2
    TabIndex = 1
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'Selection'
      DesignSize = (
        233
        229)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 144
        Height = 13
        Caption = 'Select Database and Table(s):'
      end
      object TablesCheckListBox: TCheckListBox
        Left = 8
        Top = 52
        Width = 216
        Height = 167
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
      object DBComboBox: TComboBox
        Left = 8
        Top = 24
        Width = 161
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = DBComboBoxChange
      end
      object ToolBar1: TToolBar
        Left = 177
        Top = 24
        Width = 46
        Height = 22
        Align = alNone
        Anchors = [akTop, akRight]
        AutoSize = True
        Caption = 'ToolBar1'
        EdgeInner = esNone
        EdgeOuter = esNone
        Flat = True
        Images = MainForm.ImageList1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
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
    end
    object TabSheet2: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      object CheckBoxWithUseDB: TCheckBox
        Left = 8
        Top = 16
        Width = 193
        Height = 17
        Caption = 'Include "USE dbname"-statement'
        TabOrder = 0
      end
      object CheckBoxWithDropTable: TCheckBox
        Left = 24
        Top = 96
        Width = 193
        Height = 17
        Caption = 'Include "DROP TABLE"-statements'
        TabOrder = 1
      end
      object CheckBoxCompleteInserts: TCheckBox
        Left = 24
        Top = 152
        Width = 193
        Height = 17
        Caption = 'Complete INSERT statements'
        TabOrder = 2
      end
      object CheckBoxUseBackticks: TCheckBox
        Left = 8
        Top = 40
        Width = 193
        Height = 17
        Caption = 'Use backticks (`) for names'
        TabOrder = 3
      end
      object CheckBox1: TCheckBox
        Left = 8
        Top = 72
        Width = 73
        Height = 17
        Caption = 'Structure'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = CheckBox1Click
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 128
        Width = 73
        Height = 17
        Caption = 'Data'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = CheckBox2Click
      end
      object CheckBoxExtendedInsert: TCheckBox
        Left = 24
        Top = 176
        Width = 177
        Height = 17
        Caption = 'Extended INSERT'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 520
    Top = 96
  end
end
