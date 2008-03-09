object ExportSQLForm: TExportSQLForm
  Left = 0
  Top = 0
  BorderWidth = 5
  Caption = 'Export Tables...'
  ClientHeight = 417
  ClientWidth = 634
  Color = clBtnFace
  Constraints.MinHeight = 423
  Constraints.MinWidth = 580
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    634
    417)
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgress: TLabel
    Left = 3
    Top = 396
    Width = 52
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblProgress'
  end
  object btnExport: TButton
    Left = 469
    Top = 388
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Export!'
    Default = True
    TabOrder = 0
    OnClick = btnExportClick
  end
  object btnCancel: TButton
    Left = 551
    Top = 388
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object barProgress: TProgressBar
    Left = 3
    Top = 365
    Width = 623
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 0
    Smooth = True
    Step = 1
    TabOrder = 2
  end
  object pageControl1: TPageControl
    Left = 4
    Top = 0
    Width = 622
    Height = 353
    ActivePage = TabSheet2
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Source'
      DesignSize = (
        614
        325)
      object lblSelectDbTables: TLabel
        Left = 8
        Top = 8
        Width = 145
        Height = 13
        Caption = 'Select Database and Table(s):'
      end
      object checkListTables: TCheckListBox
        Left = 8
        Top = 52
        Width = 597
        Height = 263
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 1
        OnKeyDown = checkListTablesKeyDown
      end
      object comboSelectDatabase: TComboBox
        Left = 63
        Top = 24
        Width = 542
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = comboSelectDatabaseChange
      end
      object toolbarSelectTools: TToolBar
        Left = 8
        Top = 24
        Width = 46
        Height = 22
        Align = alNone
        AutoSize = True
        Caption = 'toolbarSelectTools'
        EdgeInner = esNone
        EdgeOuter = esNone
        Images = MainForm.PngImageListMain
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        object ToolButton1: TToolButton
          Left = 0
          Top = 0
          Hint = 'Check none'
          Caption = 'ToolButton1'
          ImageIndex = 65
          OnClick = CheckListToggle
        end
        object ToolButton2: TToolButton
          Tag = 1
          Left = 23
          Top = 0
          Hint = 'Check all'
          Caption = 'ToolButton2'
          ImageIndex = 64
          OnClick = CheckListToggle
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Destination'
      ImageIndex = 1
      DesignSize = (
        614
        325)
      object groupOutput: TGroupBox
        Left = 235
        Top = 0
        Width = 376
        Height = 200
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Output'
        TabOrder = 0
        DesignSize = (
          376
          200)
        object btnFileBrowse: TPngSpeedButton
          Left = 344
          Top = 42
          Width = 22
          Height = 22
          Anchors = [akTop, akRight]
          Flat = True
          OnClick = btnFileBrowseClick
          PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
        end
        object editFileName: TEdit
          Left = 26
          Top = 42
          Width = 318
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnDblClick = btnFileBrowseClick
        end
        object radioOtherDatabase: TRadioButton
          Left = 9
          Top = 110
          Width = 113
          Height = 17
          Caption = 'Another database:'
          TabOrder = 4
          OnClick = radioOtherDatabaseClick
        end
        object radioFile: TRadioButton
          Left = 9
          Top = 25
          Width = 49
          Height = 17
          Caption = 'File:'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = radioFileOrDirClick
          OnDblClick = btnFileBrowseClick
        end
        object comboOtherDatabase: TComboBox
          Left = 26
          Top = 127
          Width = 340
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          Enabled = False
          ItemHeight = 13
          TabOrder = 6
        end
        object radioOtherHost: TRadioButton
          Left = 9
          Top = 152
          Width = 161
          Height = 17
          Caption = 'Another host and database'
          TabOrder = 8
          OnClick = radioOtherHostClick
        end
        object comboOtherHost: TComboBox
          Left = 26
          Top = 169
          Width = 137
          Height = 21
          Style = csDropDownList
          Color = clBtnFace
          Enabled = False
          ItemHeight = 13
          TabOrder = 5
          OnSelect = comboOtherHostSelect
        end
        object comboOtherHostDatabase: TComboBox
          Left = 168
          Top = 169
          Width = 198
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          Enabled = False
          ItemHeight = 13
          TabOrder = 7
        end
        object radioDirectory: TRadioButton
          Left = 9
          Top = 67
          Width = 352
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory (one .sql-file per table)'
          TabOrder = 2
          OnClick = radioFileOrDirClick
          OnDblClick = btnDirectoryBrowseClick
        end
        object editDirectory: TEdit
          Left = 26
          Top = 85
          Width = 318
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          Enabled = False
          TabOrder = 3
          OnDblClick = btnDirectoryBrowseClick
        end
        object btnDirectoryBrowse: TPngSpeedButton
          Left = 344
          Top = 85
          Width = 22
          Height = 22
          Anchors = [akTop, akRight]
          Flat = True
          OnClick = btnDirectoryBrowseClick
          PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
        end
      end
      object groupExampleSql: TGroupBox
        Left = 235
        Top = 206
        Width = 376
        Height = 107
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Example SQL'
        TabOrder = 1
        object SynMemoExampleSQL: TSynMemo
          Left = 2
          Top = 15
          Width = 372
          Height = 90
          Align = alClient
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          BorderStyle = bsNone
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Terminal'
          Gutter.Font.Style = []
          Gutter.Visible = False
          Options = [eoAutoIndent, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
          ReadOnly = True
          RemovedKeystrokes = <
            item
              Command = ecContextHelp
              ShortCut = 112
            end>
          AddedKeystrokes = <
            item
              Command = ecContextHelp
              ShortCut = 16496
            end>
        end
      end
      object groupOptions: TGroupBox
        Left = 3
        Top = 0
        Width = 226
        Height = 313
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Options'
        TabOrder = 2
        object lblTargetCompat: TLabel
          Left = 9
          Top = 221
          Width = 98
          Height = 13
          Caption = 'Target compatibility:'
        end
        object cbxStructure: TCheckBox
          Left = 9
          Top = 25
          Width = 73
          Height = 17
          Caption = 'Structure'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbxStructureClick
        end
        object cbxDatabase: TCheckBox
          Left = 25
          Top = 46
          Width = 97
          Height = 17
          Caption = 'Database'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbxDatabaseClick
        end
        object comboDatabase: TComboBox
          Left = 32
          Top = 67
          Width = 176
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 2
          TabOrder = 2
          Text = 'Create if necessary'
          OnChange = comboDatabaseChange
          Items.Strings = (
            'Recreate (remove all tables)'
            'Create'
            'Create if necessary')
        end
        object cbxTables: TCheckBox
          Left = 25
          Top = 94
          Width = 57
          Height = 17
          Caption = 'Tables'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = cbxTablesClick
        end
        object comboTables: TComboBox
          Left = 32
          Top = 114
          Width = 176
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 2
          TabOrder = 4
          Text = 'Create if necessary'
          OnChange = comboTablesChange
          Items.Strings = (
            'Recreate (remove data)'
            'Create'
            'Create if necessary')
        end
        object cbxData: TCheckBox
          Left = 9
          Top = 160
          Width = 73
          Height = 17
          Caption = 'Data'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = cbxDataClick
        end
        object comboData: TComboBox
          Left = 25
          Top = 179
          Width = 183
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 3
          TabOrder = 6
          Text = 'Update existing data'
          OnChange = comboDataChange
          Items.Strings = (
            'Replace (truncate existing data)'
            'Insert'
            'Insert new data (do not update existing)'
            'Update existing data')
        end
        object comboTargetCompat: TComboBox
          Left = 25
          Top = 240
          Width = 180
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 7
          OnChange = comboTargetCompatChange
        end
      end
    end
  end
  object dialogSave: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 8
    Top = 360
  end
end
