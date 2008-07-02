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
    ModalResult = 2
    TabOrder = 1
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
          PngImage.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000001974455874536F6674776172650041646F626520496D616765526561
            647971C9653C000002A04944415478DA8D924B4894611486DF6FC699B10BA6A6
            74752A17BA8816DE6D0CDA0546102D8B16455AA99578CB16A545178A042D3349
            87891645814952C6384A174A94F19E894269E122B3209C1975749CFF3F9DEF53
            8236E2371CCE59FCEF732EEF8894DCD60200DB38F670C472AC052884B300413D
            A341C0C041A443D7316536E1AB7F7EE15657EDFE06C1807AFBF98C2CF97DFC16
            8BD2904E0804391674255E6531400860DAAF637A4EA3D18969ED9173D433FCFD
            77A9048CD84B32E21BDD3E941E8A96729CBDD385DBA79310D481509380D964C0
            3CC33CB31A67C2D8E42C8CD071E561EFBC044CD59764AC7BDEE945F1C1283572
            614D376E9C4C84C9C8DDCD061E1DF0FA35CCCCC915088EB65FC84C0A43617507
            4940A0B6C8666A727B9197B95E012ED9FB702D2B01AB438D30F10AFE002F3EA3
            F144EA060C9844C2760B2A9EF482012EFD5EA14DBCE415B2F7452A006BB0C662
            E4EE02418DBBF3E8B3014D89897F0ED74FECB29A51F5AC4F01E86EBE0DCDDD5E
            4CFC18E76309053085F0E11816E491837C8C2543D40B0F8FC44EAB05350D030C
            C87151657E3A5E7779D4AE52A5FC530A02EBB9330348D6A46A033789DB2850F7
            6270115071261DCE1E8FA2FF335F401D4C0A97FC579964C1DFC4460B385E7D5E
            04DCCC49455BBF77694062AD50599300D95D4EA2CBD0D47F842B5823098F9DC3
            12D042D74FA5E2ED800FE547B66225AFB86E08311184A7AD2310697CC4CB5929
            783FE845F9E11854B59F58569C6FB3A3E8C1276C0E2734BEF9C2803C17951D4F
            C6C7211FCA18E01E7FB72C2025662F0AEEF7635318A1E9030352735BF48BC792
            45C7B004585734C1B9EA1E6C604073FB2889E49C96B10B471377480F0FA445AD
            E806571D6E756067E7B73FF288956C551C5F7D374322D83C651DA465A4FF9741
            8BB6AA02E483300CFE058C4769232FDE7CCE0000000049454E44AE426082}
          PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
        end
        object btnDirectoryBrowse: TPngSpeedButton
          Left = 344
          Top = 85
          Width = 22
          Height = 22
          Anchors = [akTop, akRight]
          Flat = True
          OnClick = btnDirectoryBrowseClick
          PngImage.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000001974455874536F6674776172650041646F626520496D616765526561
            647971C9653C0000028F4944415478DAA5935D48935118C7FF67DBBB4DDBAB13
            1336BDB0A9F3332DCA1911E58D50981A06918644521974551741109404DD04DD
            2404DD441F04A510986284267DDCC8C83011CDA69656CBA9B9CD6DBAF7F374DE
            BD13EC26B01E38EF799EC3F9FFDEFF733887504AF13F41267B768F58D34A4B09
            316C58A61022D3DF1439B49715C29F8AC43756543F2627CACFCF3DF182BA210B
            2144D325C460B91C5F4260A45DA4C9552D8CE6344A156921EA1FBC5478E443A7
            0E7856192A6878972ECEDF81221A01431AD3F3B066D7B09C63724DAFB2594DC0
            89C126FABAF7F9DC8DDEED3AE0A96725BF71808F7F7FC8F650A8C232A458000A
            6B89B33A9946627A917134C732F8E2CBF8D2571B751F7DCFEB80CE4AD155D7CB
            4567BA40C528A49545F025C761CD2C4936BCEE80EA0E4C76F8BAF6C4DDC7BC29
            3AE07195E83E31C4697F59DFC87CB0F207E4D000A696B76226BC0D8B6B0EA458
            3864DA14E44C9C11CA9A5F5A75C07D8F50D0F2C6ACAE8E822A213622A07208AA
            14C454D081AFD2411467DB60E28C58088B588E0810E7FA9586FA66930EB8EB89
            E7B7BEB22891D74C1866807002A0E53D3F2FA23C97475436425269A29974AB11
            63BE79341DC8253AA0A372ADE05CBF550A762785A124248C277357515DE14020
            2283E921C81419A91CC66796D0529DA303266FEDF2BB4E77A68ABFBC3CD43503
            A57176146CB0B977E91476E4D9B110A39055FD3AD8B770181E9D455B6DA10EF8
            74A3EC020CA48AF9AB6187CE6FBC74534557B8AC8AC346CE6286A050980C043F
            8312FC1F0769AEEF76EBA1EB030FC8DFDE4247DFACCB62C2C90C9BB95D33B012
            135439304EF667F9496CFA6D647A62B88D6CF631755FAB3DEB743A6E66B976DA
            27BC2F56370DD0E2D1F9A226675EF93D559553FE09B0317E035C7D448FD2E97A
            180000000049454E44AE426082}
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
