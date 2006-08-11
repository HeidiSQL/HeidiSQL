object ExportSQLForm: TExportSQLForm
  Left = 422
  Top = 230
  Width = 580
  Height = 430
  BorderWidth = 5
  Caption = 'Export Tables...'
  Color = clBtnFace
  Constraints.MinHeight = 430
  Constraints.MinWidth = 580
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    562
    386)
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgress: TLabel
    Left = 4
    Top = 309
    Width = 51
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblProgress'
  end
  object btnExport: TButton
    Left = 397
    Top = 357
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Export!'
    Default = True
    TabOrder = 0
    OnClick = btnExportClick
  end
  object btnCancel: TButton
    Left = 479
    Top = 357
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object groupOutput: TGroupBox
    Left = 256
    Top = 0
    Width = 299
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Output'
    TabOrder = 2
    DesignSize = (
      299
      169)
    object btnFileBrowse: TBitBtn
      Left = 261
      Top = 43
      Width = 22
      Height = 22
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnFileBrowseClick
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
    object editFileName: TEdit
      Left = 32
      Top = 42
      Width = 227
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnDblClick = btnFileBrowseClick
    end
    object radioOtherDatabase: TRadioButton
      Left = 16
      Top = 69
      Width = 113
      Height = 17
      Caption = 'Another Database:'
      TabOrder = 2
      OnClick = radioOtherDatabaseClick
    end
    object radioFile: TRadioButton
      Left = 16
      Top = 24
      Width = 49
      Height = 17
      Caption = 'File:'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = radioFileClick
    end
    object comboOtherDatabase: TComboBox
      Left = 32
      Top = 87
      Width = 251
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 4
    end
    object radioOtherHost: TRadioButton
      Left = 16
      Top = 116
      Width = 113
      Height = 17
      Caption = 'Another Host / DB:'
      TabOrder = 5
      OnClick = radioOtherHostClick
    end
    object comboOtherHost: TComboBox
      Left = 32
      Top = 131
      Width = 251
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 6
    end
  end
  object barProgress: TProgressBar
    Left = 4
    Top = 325
    Width = 551
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Min = 0
    Max = 0
    Smooth = True
    Step = 1
    TabOrder = 3
  end
  object pageControl1: TPageControl
    Left = 4
    Top = 0
    Width = 238
    Height = 303
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akBottom]
    TabIndex = 0
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'Selection'
      DesignSize = (
        230
        275)
      object lblSelectDbTables: TLabel
        Left = 8
        Top = 8
        Width = 144
        Height = 13
        Caption = 'Select Database and Table(s):'
      end
      object checkListTables: TCheckListBox
        Left = 8
        Top = 52
        Width = 213
        Height = 213
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
      object comboSelectDatabase: TComboBox
        Left = 8
        Top = 24
        Width = 158
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = comboSelectDatabaseChange
      end
      object toolbarSelectTools: TToolBar
        Left = 174
        Top = 24
        Width = 46
        Height = 22
        Align = alNone
        Anchors = [akTop, akRight]
        AutoSize = True
        Caption = 'toolbarSelectTools'
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
          ImageIndex = 35
          OnClick = CheckListToggle
        end
        object ToolButton2: TToolButton
          Tag = 1
          Left = 23
          Top = 0
          Hint = 'Check all'
          Caption = 'ToolButton2'
          ImageIndex = 36
          OnClick = CheckListToggle
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      DesignSize = (
        230
        275)
      object lblTargetCompat: TLabel
        Left = 8
        Top = 225
        Width = 94
        Height = 13
        Caption = 'Target compatibility:'
      end
      object cbxStructure: TCheckBox
        Left = 8
        Top = 9
        Width = 73
        Height = 17
        Caption = 'Structure'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbxStructureClick
      end
      object cbxData: TCheckBox
        Left = 8
        Top = 140
        Width = 73
        Height = 17
        Caption = 'Data'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbxDataClick
      end
      object cbxExtendedInsert: TCheckBox
        Left = 24
        Top = 186
        Width = 177
        Height = 17
        Caption = 'Extended INSERT (faster import)'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cbxExtendedInsertClick
      end
      object comboTargetCompat: TComboBox
        Left = 24
        Top = 244
        Width = 190
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 3
        Text = 'MySQL 3.23 - 5.0'
        Items.Strings = (
          'MySQL 3.23 - 5.0'
          'MySQL 5.1 and above')
      end
      object cbxDatabase: TCheckBox
        Left = 24
        Top = 30
        Width = 97
        Height = 17
        Caption = 'Database'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = cbxDatabaseClick
      end
      object cbxTables: TCheckBox
        Left = 24
        Top = 78
        Width = 57
        Height = 17
        Caption = 'Tables'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = cbxTablesClick
      end
      object comboTables: TComboBox
        Left = 41
        Top = 98
        Width = 176
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 6
        Text = 'Create if necessary'
        OnChange = comboTablesChange
        Items.Strings = (
          'Recreate (remove data)'
          'Create'
          'Create if necessary')
      end
      object comboDatabase: TComboBox
        Left = 41
        Top = 51
        Width = 176
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 7
        Text = 'Create if necessary'
        OnChange = comboDatabaseChange
        Items.Strings = (
          'Recreate (remove all tables)'
          'Create'
          'Create if necessary')
      end
      object comboData: TComboBox
        Left = 24
        Top = 159
        Width = 193
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 3
        TabOrder = 8
        Text = 'Update existing data'
        OnChange = comboDataChange
        Items.Strings = (
          'Replace (truncate existing data)'
          'Insert'
          'Insert new data (do not update existing)'
          'Update existing data')
      end
    end
  end
  object groupExampleSql: TGroupBox
    Left = 256
    Top = 175
    Width = 299
    Height = 128
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Example SQL'
    TabOrder = 5
    object SynMemoExampleSQL: TSynMemo
      Left = 2
      Top = 15
      Width = 295
      Height = 111
      Cursor = crIBeam
      Align = alClient
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      BorderStyle = bsNone
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Terminal'
      Gutter.Font.Style = []
      Gutter.Visible = False
      Keystrokes = <
        item
          Command = ecUp
          ShortCut = 38
        end
        item
          Command = ecSelUp
          ShortCut = 8230
        end
        item
          Command = ecScrollUp
          ShortCut = 16422
        end
        item
          Command = ecDown
          ShortCut = 40
        end
        item
          Command = ecSelDown
          ShortCut = 8232
        end
        item
          Command = ecScrollDown
          ShortCut = 16424
        end
        item
          Command = ecLeft
          ShortCut = 37
        end
        item
          Command = ecSelLeft
          ShortCut = 8229
        end
        item
          Command = ecWordLeft
          ShortCut = 16421
        end
        item
          Command = ecSelWordLeft
          ShortCut = 24613
        end
        item
          Command = ecRight
          ShortCut = 39
        end
        item
          Command = ecSelRight
          ShortCut = 8231
        end
        item
          Command = ecWordRight
          ShortCut = 16423
        end
        item
          Command = ecSelWordRight
          ShortCut = 24615
        end
        item
          Command = ecPageDown
          ShortCut = 34
        end
        item
          Command = ecSelPageDown
          ShortCut = 8226
        end
        item
          Command = ecPageBottom
          ShortCut = 16418
        end
        item
          Command = ecSelPageBottom
          ShortCut = 24610
        end
        item
          Command = ecPageUp
          ShortCut = 33
        end
        item
          Command = ecSelPageUp
          ShortCut = 8225
        end
        item
          Command = ecPageTop
          ShortCut = 16417
        end
        item
          Command = ecSelPageTop
          ShortCut = 24609
        end
        item
          Command = ecLineStart
          ShortCut = 36
        end
        item
          Command = ecSelLineStart
          ShortCut = 8228
        end
        item
          Command = ecEditorTop
          ShortCut = 16420
        end
        item
          Command = ecSelEditorTop
          ShortCut = 24612
        end
        item
          Command = ecLineEnd
          ShortCut = 35
        end
        item
          Command = ecSelLineEnd
          ShortCut = 8227
        end
        item
          Command = ecEditorBottom
          ShortCut = 16419
        end
        item
          Command = ecSelEditorBottom
          ShortCut = 24611
        end
        item
          Command = ecToggleMode
          ShortCut = 45
        end
        item
          Command = ecCopy
          ShortCut = 16429
        end
        item
          Command = ecCut
          ShortCut = 8238
        end
        item
          Command = ecPaste
          ShortCut = 8237
        end
        item
          Command = ecDeleteChar
          ShortCut = 46
        end
        item
          Command = ecDeleteLastChar
          ShortCut = 8
        end
        item
          Command = ecDeleteLastChar
          ShortCut = 8200
        end
        item
          Command = ecDeleteLastWord
          ShortCut = 16392
        end
        item
          Command = ecUndo
          ShortCut = 32776
        end
        item
          Command = ecRedo
          ShortCut = 40968
        end
        item
          Command = ecLineBreak
          ShortCut = 13
        end
        item
          Command = ecLineBreak
          ShortCut = 8205
        end
        item
          Command = ecTab
          ShortCut = 9
        end
        item
          Command = ecShiftTab
          ShortCut = 8201
        end
        item
          Command = ecContextHelp
          ShortCut = 16496
        end
        item
          Command = ecSelectAll
          ShortCut = 16449
        end
        item
          Command = ecCopy
          ShortCut = 16451
        end
        item
          Command = ecPaste
          ShortCut = 16470
        end
        item
          Command = ecCut
          ShortCut = 16472
        end
        item
          Command = ecBlockIndent
          ShortCut = 24649
        end
        item
          Command = ecBlockUnindent
          ShortCut = 24661
        end
        item
          Command = ecLineBreak
          ShortCut = 16461
        end
        item
          Command = ecInsertLine
          ShortCut = 16462
        end
        item
          Command = ecDeleteWord
          ShortCut = 16468
        end
        item
          Command = ecDeleteLine
          ShortCut = 16473
        end
        item
          Command = ecDeleteEOL
          ShortCut = 24665
        end
        item
          Command = ecUndo
          ShortCut = 16474
        end
        item
          Command = ecRedo
          ShortCut = 24666
        end
        item
          Command = ecGotoMarker0
          ShortCut = 16432
        end
        item
          Command = ecGotoMarker1
          ShortCut = 16433
        end
        item
          Command = ecGotoMarker2
          ShortCut = 16434
        end
        item
          Command = ecGotoMarker3
          ShortCut = 16435
        end
        item
          Command = ecGotoMarker4
          ShortCut = 16436
        end
        item
          Command = ecGotoMarker5
          ShortCut = 16437
        end
        item
          Command = ecGotoMarker6
          ShortCut = 16438
        end
        item
          Command = ecGotoMarker7
          ShortCut = 16439
        end
        item
          Command = ecGotoMarker8
          ShortCut = 16440
        end
        item
          Command = ecGotoMarker9
          ShortCut = 16441
        end
        item
          Command = ecSetMarker0
          ShortCut = 24624
        end
        item
          Command = ecSetMarker1
          ShortCut = 24625
        end
        item
          Command = ecSetMarker2
          ShortCut = 24626
        end
        item
          Command = ecSetMarker3
          ShortCut = 24627
        end
        item
          Command = ecSetMarker4
          ShortCut = 24628
        end
        item
          Command = ecSetMarker5
          ShortCut = 24629
        end
        item
          Command = ecSetMarker6
          ShortCut = 24630
        end
        item
          Command = ecSetMarker7
          ShortCut = 24631
        end
        item
          Command = ecSetMarker8
          ShortCut = 24632
        end
        item
          Command = ecSetMarker9
          ShortCut = 24633
        end
        item
          Command = ecNormalSelect
          ShortCut = 24654
        end
        item
          Command = ecColumnSelect
          ShortCut = 24643
        end
        item
          Command = ecLineSelect
          ShortCut = 24652
        end
        item
          Command = ecMatchBracket
          ShortCut = 24642
        end>
      Options = [eoAutoIndent, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
      ReadOnly = True
    end
  end
  object dialogSave: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 8
    Top = 360
  end
end
