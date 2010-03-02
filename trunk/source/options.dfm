object optionsform: Toptionsform
  Left = 547
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 395
  ClientWidth = 472
  Color = clBtnFace
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
    472
    395)
  PixelsPerInch = 96
  TextHeight = 13
  object pagecontrolMain: TPageControl
    Left = 8
    Top = 8
    Width = 458
    Height = 352
    ActivePage = tabMisc
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnChange = pagecontrolMainChange
    OnChanging = pagecontrolMainChanging
    object tabMisc: TTabSheet
      Caption = 'Miscellaneous'
      object chkAutoReconnect: TCheckBox
        Left = 16
        Top = 32
        Width = 353
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Automatically reconnect to last session-account on startup'
        TabOrder = 1
        OnClick = Modified
      end
      object chkRestoreLastDB: TCheckBox
        Left = 16
        Top = 56
        Width = 316
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Restore last used database on startup'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = Modified
      end
      object chkUpdatecheck: TCheckBox
        Left = 16
        Top = 78
        Width = 300
        Height = 28
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Automatically check for updates / Interval [days]:'#13#10'(0 = each ti' +
          'me the application is started)'
        TabOrder = 3
        WordWrap = True
        OnClick = chkUpdatecheckClick
      end
      object editUpdatecheckInterval: TEdit
        Left = 335
        Top = 78
        Width = 43
        Height = 21
        Anchors = [akTop, akRight]
        Enabled = False
        TabOrder = 4
        Text = '1'
        OnChange = Modified
      end
      object updownUpdatecheckInterval: TUpDown
        Left = 378
        Top = 78
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editUpdatecheckInterval
        Enabled = False
        Max = 999
        Position = 1
        TabOrder = 5
        OnChanging = anyUpDownLimitChanging
      end
      object chkUpdateCheckBuilds: TCheckBox
        Left = 34
        Top = 112
        Width = 266
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Also check for updated nightly builds'
        Enabled = False
        TabOrder = 6
        OnClick = Modified
      end
      object chkDoStatistics: TCheckBox
        Left = 16
        Top = 135
        Width = 398
        Height = 48
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Count in usage statistics. This option, if enabled, will cause H' +
          'eidiSQL to ping heidisql.com at most once every month.  This is ' +
          'used to count the used HeidiSQL and MySQL versions.'
        TabOrder = 7
        WordWrap = True
        OnClick = Modified
      end
      object chkAllowMultiInstances: TCheckBox
        Left = 16
        Top = 9
        Width = 398
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Allow multiple application instances'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
    end
    object tabLogging: TTabSheet
      Caption = 'Logging'
      ImageIndex = 5
      object Label4: TLabel
        Left = 16
        Top = 18
        Width = 37
        Height = 13
        Caption = 'Log last'
      end
      object lblLogLinesHint: TLabel
        Left = 240
        Top = 18
        Width = 71
        Height = 13
        Caption = 'lines in SQL log'
      end
      object lblLogSnipHint: TLabel
        Left = 238
        Top = 45
        Width = 139
        Height = 13
        Caption = 'characters  (0 = no snipping)'
      end
      object lblLogSnip: TLabel
        Left = 16
        Top = 45
        Width = 96
        Height = 13
        Caption = 'Snip SQL log lines to'
      end
      object lblLogLevel: TLabel
        Left = 16
        Top = 102
        Width = 57
        Height = 13
        Caption = 'Log events:'
      end
      object editLogLines: TEdit
        Left = 159
        Top = 15
        Width = 53
        Height = 21
        TabOrder = 0
        Text = '1'
        OnChange = Modified
      end
      object updownLogLines: TUpDown
        Left = 212
        Top = 15
        Width = 16
        Height = 21
        Associate = editLogLines
        Min = 1
        Max = 32767
        Position = 1
        TabOrder = 1
        Wrap = True
        OnChanging = anyUpDownLimitChanging
      end
      object updownLogSnip: TUpDown
        Left = 212
        Top = 42
        Width = 16
        Height = 21
        Associate = editLogSnip
        Max = 32767
        Position = 2000
        TabOrder = 3
        OnChanging = anyUpDownLimitChanging
      end
      object editLogSnip: TEdit
        Left = 159
        Top = 42
        Width = 53
        Height = 21
        TabOrder = 2
        Text = '2000'
        OnChange = Modified
      end
      object chkLogToFile: TCheckBox
        Left = 16
        Top = 71
        Width = 182
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Write SQL log to file'
        TabOrder = 4
        OnClick = Modified
      end
      object btnOpenLogFolder: TButton
        Left = 159
        Top = 68
        Width = 106
        Height = 20
        Caption = 'Open log folder ...'
        TabOrder = 5
        OnClick = btnOpenLogFolderClick
      end
      object chkLogEventErrors: TCheckBox
        Left = 159
        Top = 101
        Width = 150
        Height = 17
        Caption = 'Errors'
        TabOrder = 6
        OnClick = Modified
      end
      object chkLogEventUserFiredSQL: TCheckBox
        Left = 159
        Top = 120
        Width = 150
        Height = 17
        Caption = 'User fired SQL queries'
        TabOrder = 7
        OnClick = Modified
      end
      object chkLogEventSQL: TCheckBox
        Left = 159
        Top = 139
        Width = 150
        Height = 17
        Caption = 'Internal SQL queries'
        TabOrder = 8
        OnClick = Modified
      end
      object chkLogEventInfo: TCheckBox
        Left = 159
        Top = 158
        Width = 150
        Height = 17
        Caption = 'Information messages'
        TabOrder = 9
        OnClick = Modified
      end
      object chkLogEventDebug: TCheckBox
        Left = 159
        Top = 177
        Width = 150
        Height = 17
        Caption = 'Debug messages'
        TabOrder = 10
        OnClick = Modified
      end
    end
    object tabSQL: TTabSheet
      BorderWidth = 5
      Caption = 'SQL'
      ImageIndex = 1
      object grpSQLFont: TGroupBox
        Left = 1
        Top = 1
        Width = 329
        Height = 56
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Editor font'
        TabOrder = 0
        object lblSQLFontSize: TLabel
          Left = 307
          Top = 26
          Width = 10
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'pt'
        end
        object comboSQLFontName: TComboBox
          Left = 16
          Top = 23
          Width = 225
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = SQLFontChange
        end
        object editSQLFontSize: TEdit
          Left = 244
          Top = 23
          Width = 41
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 1
          Text = '9'
          OnExit = SQLFontChange
        end
        object updownSQLFontSize: TUpDown
          Left = 285
          Top = 23
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = editSQLFontSize
          Position = 9
          TabOrder = 2
          OnClick = updownSQLFontSizeClick
        end
      end
      object grpSQLColors: TGroupBox
        Left = 1
        Top = 61
        Width = 438
        Height = 68
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Syntax'
        TabOrder = 1
        object lblSQLColElement: TLabel
          Left = 15
          Top = 17
          Width = 42
          Height = 13
          Caption = 'Element:'
        end
        object lblSQLColForeground: TLabel
          Left = 247
          Top = 16
          Width = 60
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Foreground:'
        end
        object lblSQLColBackground: TLabel
          Left = 247
          Top = 42
          Width = 60
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Background:'
        end
        object comboSQLColElement: TComboBox
          Left = 67
          Top = 13
          Width = 164
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = comboSQLColElementChange
        end
        object cboxSQLColForeground: TColorBox
          Left = 318
          Top = 13
          Width = 110
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
          Anchors = [akTop, akRight]
          TabOrder = 1
          OnChange = SQLFontChange
        end
        object chkSQLBold: TCheckBox
          Left = 67
          Top = 41
          Width = 57
          Height = 17
          Caption = 'Bold'
          TabOrder = 2
          OnClick = SQLFontChange
        end
        object chkSQLItalic: TCheckBox
          Left = 130
          Top = 41
          Width = 91
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Italic'
          TabOrder = 3
          OnClick = SQLFontChange
        end
        object cboxSQLColBackground: TColorBox
          Left = 318
          Top = 39
          Width = 110
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
          Anchors = [akTop, akRight]
          TabOrder = 4
          OnChange = SQLFontChange
        end
      end
      object grpSQLSample: TGroupBox
        Left = 0
        Top = 132
        Width = 438
        Height = 179
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Sample'
        TabOrder = 2
        object SynMemoSQLSample: TSynMemo
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 428
          Height = 156
          Cursor = crHandPoint
          SingleLineMode = False
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          OnClick = SynMemoSQLSampleClick
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Visible = False
          Gutter.Width = 0
          Highlighter = SynSQLSynSQLSample
          Lines.Strings = (
            'SynMemoSQLSample')
          Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          ReadOnly = True
          RightEdge = 0
          TabWidth = 2
          WordWrap = True
          OnChange = SQLFontChange
        end
      end
      object grpSQLTabWidth: TGroupBox
        Left = 336
        Top = 1
        Width = 102
        Height = 56
        Anchors = [akTop, akRight]
        Caption = 'Tab width'
        TabOrder = 3
        object editSQLTabWidth: TEdit
          Left = 14
          Top = 23
          Width = 43
          Height = 21
          TabOrder = 0
          Text = '0'
          OnExit = SQLFontChange
        end
        object updownSQLTabWidth: TUpDown
          Left = 57
          Top = 23
          Width = 16
          Height = 21
          Associate = editSQLTabWidth
          TabOrder = 1
          OnClick = updownSQLFontSizeClick
        end
      end
    end
    object tabData: TTabSheet
      BorderWidth = 5
      Caption = 'Data'
      ImageIndex = 4
      object lblMaxColWidth: TLabel
        Left = 4
        Top = 8
        Width = 177
        Height = 13
        Caption = 'Maximum column-width in data-grids:'
      end
      object lblDataFontHint: TLabel
        Left = 362
        Top = 84
        Width = 29
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'points'
      end
      object lblDataFont: TLabel
        Left = 4
        Top = 84
        Width = 26
        Height = 13
        Caption = '&Font:'
        FocusControl = comboDataFontName
      end
      object lblMaxTotalRows: TLabel
        Left = 4
        Top = 33
        Width = 267
        Height = 13
        Caption = 'Number of rows displayed in data tab (Step, Maximum):'
      end
      object lblGridRowsLinecount: TLabel
        Left = 4
        Top = 58
        Width = 122
        Height = 13
        Caption = 'Lines of text in grid rows:'
      end
      object editMaxColWidth: TEdit
        Left = 299
        Top = 5
        Width = 42
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 0
        Text = '1'
        OnChange = Modified
      end
      object updownMaxColWidth: TUpDown
        Left = 341
        Top = 5
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editMaxColWidth
        Min = 1
        Max = 1000
        Position = 1
        TabOrder = 1
        OnChanging = anyUpDownLimitChanging
      end
      object comboDataFontName: TComboBox
        Left = 59
        Top = 80
        Width = 234
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = DataFontsChange
      end
      object editDataFontSize: TEdit
        Left = 299
        Top = 80
        Width = 42
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 3
        Text = '8'
        OnChange = DataFontsChange
      end
      object updownDataFontSize: TUpDown
        Left = 341
        Top = 80
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editDataFontSize
        Position = 8
        TabOrder = 4
      end
      object grpFieldLayout: TGroupBox
        Left = 4
        Top = 112
        Width = 433
        Height = 195
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Field colors and editors'
        TabOrder = 5
        object lblFieldDatetime: TLabel
          Left = 12
          Top = 96
          Width = 51
          Height = 13
          Caption = '&Date/time:'
          FocusControl = cboxDatetime
        end
        object lblFieldText: TLabel
          Left = 12
          Top = 48
          Width = 26
          Height = 13
          Caption = '&Text:'
          FocusControl = cboxText
        end
        object lblFieldBinary: TLabel
          Left = 12
          Top = 72
          Width = 34
          Height = 13
          Caption = '&Binary:'
          FocusControl = cboxBinary
        end
        object lblFieldNumeric: TLabel
          Left = 12
          Top = 24
          Width = 46
          Height = 13
          Caption = '&Numbers:'
          FocusControl = cboxNumeric
        end
        object lblFieldEnum: TLabel
          Left = 12
          Top = 120
          Width = 30
          Height = 13
          Caption = '&Enum:'
          FocusControl = cboxEnum
        end
        object lblFieldSet: TLabel
          Left = 12
          Top = 144
          Width = 20
          Height = 13
          Caption = '&Set:'
        end
        object lblFieldNull: TLabel
          Left = 12
          Top = 168
          Width = 28
          Height = 13
          Caption = 'N&ULL:'
        end
        object cboxText: TColorBox
          Left = 100
          Top = 45
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 1
          OnChange = Modified
        end
        object cboxBinary: TColorBox
          Left = 100
          Top = 69
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 2
          OnChange = Modified
        end
        object cboxDatetime: TColorBox
          Left = 100
          Top = 93
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 4
          OnChange = Modified
        end
        object cboxNumeric: TColorBox
          Left = 100
          Top = 21
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 0
          OnChange = Modified
        end
        object cboxEnum: TColorBox
          Left = 100
          Top = 117
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 6
          OnChange = Modified
        end
        object chkEditorBinary: TCheckBox
          Left = 234
          Top = 71
          Width = 154
          Height = 17
          Caption = 'Enable popup HEX editor'
          TabOrder = 3
          OnClick = Modified
        end
        object chkEditorDatetime: TCheckBox
          Left = 234
          Top = 97
          Width = 154
          Height = 17
          Caption = 'Enable calendar editor'
          TabOrder = 5
          OnClick = Modified
        end
        object chkEditorEnum: TCheckBox
          Left = 234
          Top = 120
          Width = 154
          Height = 17
          Caption = 'Enable pulldown editor'
          TabOrder = 7
          OnClick = Modified
        end
        object cboxSet: TColorBox
          Left = 100
          Top = 141
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 8
          OnChange = Modified
        end
        object chkEditorSet: TCheckBox
          Left = 234
          Top = 143
          Width = 154
          Height = 17
          Caption = 'Enable checkbox editor'
          TabOrder = 9
          OnClick = Modified
        end
        object chkNullBG: TCheckBox
          Left = 234
          Top = 167
          Width = 154
          Height = 17
          Caption = 'Enable background color'
          TabOrder = 11
          OnClick = chkNullBGClick
        end
        object cboxNullBG: TColorBox
          Left = 100
          Top = 165
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          Enabled = False
          TabOrder = 10
          OnChange = Modified
        end
      end
      object editGridRowCountMax: TEdit
        Left = 364
        Top = 30
        Width = 73
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 6
        OnChange = Modified
      end
      object editGridRowCountStep: TEdit
        Left = 299
        Top = 30
        Width = 57
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 7
      end
      object editGridRowsLineCount: TEdit
        Left = 299
        Top = 55
        Width = 42
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 8
        Text = '1'
        OnChange = Modified
      end
      object updownGridRowsLineCount: TUpDown
        Left = 341
        Top = 55
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editGridRowsLineCount
        Min = 1
        Position = 1
        TabOrder = 9
        OnChanging = anyUpDownLimitChanging
      end
    end
    object tabCSV: TTabSheet
      BorderWidth = 5
      Caption = 'Export'
      ImageIndex = 2
      object lblCopyDataMaxSize: TLabel
        Left = 8
        Top = 200
        Width = 244
        Height = 13
        Caption = 'Limit "Copy as CSV/HTML/XML/SQL" actions to [MB]'
      end
      object grpCSV: TGroupBox
        Left = 0
        Top = 0
        Width = 440
        Height = 177
        Align = alTop
        Caption = 'CSV-Strings for copying/saving CSV-data'
        TabOrder = 0
        object lblCSVSeparator: TLabel
          Left = 16
          Top = 32
          Width = 98
          Height = 13
          Caption = 'Fields separated by:'
        end
        object lblCSVTerminator: TLabel
          Left = 16
          Top = 82
          Width = 98
          Height = 13
          Caption = 'Lines terminated by:'
        end
        object lblCSVEncloser: TLabel
          Left = 16
          Top = 58
          Width = 91
          Height = 13
          Caption = 'Fields enclosed by:'
        end
        object lblCSVHintCR: TLabel
          Left = 248
          Top = 120
          Width = 102
          Height = 13
          Caption = '\r  =  Carriage return'
        end
        object lblCSVHintLF: TLabel
          Left = 248
          Top = 136
          Width = 70
          Height = 13
          Caption = '\n  =  New line'
        end
        object lblCSVHintTAB: TLabel
          Left = 248
          Top = 152
          Width = 46
          Height = 13
          Caption = '\t  =  Tab'
        end
        object lblCSVHintEscaped: TLabel
          Left = 16
          Top = 120
          Width = 219
          Height = 13
          AutoSize = False
          Caption = 'Note: You can use these escaped characters:'
        end
        object editCSVSeparator: TEdit
          Left = 120
          Top = 30
          Width = 57
          Height = 21
          MaxLength = 10
          TabOrder = 0
          Text = 'editCSVSeparator'
          OnChange = Modified
        end
        object editCSVEncloser: TEdit
          Left = 120
          Top = 54
          Width = 57
          Height = 21
          MaxLength = 10
          TabOrder = 1
          Text = 'editCSVEncloser'
          OnChange = Modified
        end
        object editCSVTerminator: TEdit
          Left = 120
          Top = 79
          Width = 57
          Height = 21
          MaxLength = 10
          TabOrder = 2
          Text = 'editCSVTerminator'
          OnChange = Modified
        end
      end
      object editCopyDataMaxSize: TEdit
        Left = 355
        Top = 197
        Width = 65
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 1
        Text = '0'
        OnChange = Modified
      end
      object updownCopyDataMaxSize: TUpDown
        Left = 420
        Top = 197
        Width = 17
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editCopyDataMaxSize
        Max = 999
        TabOrder = 2
        Wrap = True
        OnChanging = anyUpDownLimitChanging
      end
      object chkExportLocaleNumbers: TCheckBox
        Left = 8
        Top = 232
        Width = 426
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Use locale number format in grid exports (MS Excel paste compati' +
          'bility)'
        TabOrder = 3
      end
    end
    object tabShortcuts: TTabSheet
      Caption = 'Shortcuts'
      ImageIndex = 4
      DesignSize = (
        450
        324)
      object lblShortcut1: TLabel
        Left = 199
        Top = 62
        Width = 45
        Height = 13
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Shortcut:'
      end
      object lblShortcutHint: TLabel
        Left = 199
        Top = 3
        Width = 211
        Height = 72
        Anchors = [akLeft, akTop, akBottom]
        AutoSize = False
        Caption = 'Please select a shortcut item in the tree.'
        WordWrap = True
      end
      object lblShortcut2: TLabel
        Left = 199
        Top = 107
        Width = 98
        Height = 13
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Secondary shortcut:'
      end
      object TreeShortcutItems: TVirtualStringTree
        Left = 3
        Top = 3
        Width = 190
        Height = 292
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        Images = MainForm.ImageListMain
        TabOrder = 0
        OnFocusChanged = TreeShortcutItemsFocusChanged
        OnGetText = TreeShortcutItemsGetText
        OnGetImageIndex = TreeShortcutItemsGetImageIndex
        OnGetNodeDataSize = TreeShortcutItemsGetNodeDataSize
        OnInitChildren = TreeShortcutItemsInitChildren
        OnInitNode = TreeShortcutItemsInitNode
        Columns = <>
      end
      object Shortcut1: TSynHotKey
        Left = 199
        Top = 81
        Width = 207
        Height = 19
        HotKey = 0
        Modifiers = []
        OnChange = Shortcut1Change
        OnEnter = ShortcutEnter
        OnExit = ShortcutExit
      end
      object Shortcut2: TSynHotKey
        Left = 199
        Top = 125
        Width = 207
        Height = 19
        HotKey = 0
        Modifiers = []
        OnChange = Shortcut2Change
        OnEnter = ShortcutEnter
        OnExit = ShortcutExit
      end
    end
  end
  object btnCancel: TButton
    Left = 311
    Top = 364
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 231
    Top = 364
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Apply
  end
  object btnApply: TButton
    Left = 391
    Top = 364
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    TabOrder = 2
    OnClick = Apply
  end
  object btnRestoreDefaults: TButton
    Left = 8
    Top = 364
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Factory defaults'
    TabOrder = 4
    OnClick = btnRestoreDefaultsClick
  end
  object SynSQLSynSQLSample: TSynSQLSyn
    SQLDialect = sqlMySQL
    Left = 112
    Top = 360
  end
end
