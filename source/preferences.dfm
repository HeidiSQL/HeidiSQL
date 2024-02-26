object frmPreferences: TfrmPreferences
  Left = 547
  Top = 163
  BorderIcons = [biSystemMenu]
  Caption = 'Preferences'
  ClientHeight = 482
  ClientWidth = 712
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    712
    482)
  TextHeight = 14
  object pagecontrolMain: TPageControl
    Left = 8
    Top = 8
    Width = 684
    Height = 435
    ActivePage = tabMisc
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = MainForm.VirtualImageListMain
    TabOrder = 4
    OnChange = pagecontrolMainChange
    OnChanging = pagecontrolMainChanging
    object tabMisc: TTabSheet
      Caption = 'General'
      ImageIndex = 137
      ImageName = 'icons8-settings'
      DesignSize = (
        676
        406)
      object lblMySQLBinaries: TLabel
        Left = 8
        Top = 177
        Width = 174
        Height = 14
        Caption = 'MySQL command line programs:'
      end
      object lblLanguage: TLabel
        Left = 8
        Top = 231
        Width = 127
        Height = 14
        Caption = 'Application language: *'
      end
      object lblCustomSnippetsDirectory: TLabel
        Left = 8
        Top = 204
        Width = 146
        Height = 14
        Caption = 'Custom snippets directory:'
      end
      object lblGUIFont: TLabel
        Left = 8
        Top = 258
        Width = 62
        Height = 14
        Caption = 'GUI font: *'
      end
      object lblGUIFontSize: TLabel
        Left = 640
        Top = 258
        Width = 12
        Height = 14
        Anchors = [akTop, akRight]
        Caption = 'pt'
      end
      object lblTheme: TLabel
        Left = 8
        Top = 285
        Width = 85
        Height = 14
        Caption = 'Style Theme: *'
      end
      object lblIconPack: TLabel
        Left = 8
        Top = 312
        Width = 57
        Height = 14
        Caption = 'Icon pack:'
      end
      object lblWebSearchBaseUrl: TLabel
        Left = 8
        Top = 339
        Width = 115
        Height = 14
        Hint = 'Used in footer of various message dialogs'
        Caption = 'Web search base url:'
      end
      object chkAutoReconnect: TCheckBox
        Left = 220
        Top = 31
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Automatically reconnect to previously opened sessions on startup'
        TabOrder = 1
        OnClick = Modified
      end
      object chkRestoreLastDB: TCheckBox
        Left = 220
        Top = 54
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Restore last used database on startup'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = Modified
      end
      object chkUpdatecheck: TCheckBox
        Left = 220
        Top = 77
        Width = 183
        Height = 17
        Hint = '0 = on each application start'
        Caption = 'Check for updates each [day]:'
        TabOrder = 3
        OnClick = chkUpdatecheckClick
      end
      object editUpdatecheckInterval: TEdit
        Left = 409
        Top = 75
        Width = 43
        Height = 22
        Enabled = False
        TabOrder = 4
        Text = '1'
        OnChange = Modified
      end
      object updownUpdatecheckInterval: TUpDown
        Left = 452
        Top = 75
        Width = 16
        Height = 22
        Associate = editUpdatecheckInterval
        Enabled = False
        Max = 999
        Position = 1
        TabOrder = 5
        OnChanging = anyUpDownLimitChanging
      end
      object chkUpdateCheckBuilds: TCheckBox
        Left = 488
        Top = 77
        Width = 183
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Also check for updated nightly builds'
        Enabled = False
        TabOrder = 6
        OnClick = Modified
      end
      object chkDoStatistics: TCheckBox
        Left = 220
        Top = 100
        Width = 451
        Height = 17
        Hint = 
          'This option, if enabled, will cause HeidiSQL to ping heidisql.co' +
          'm at most once every month.  This is used to count the used Heid' +
          'iSQL and server versions.'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Send usage statistics'
        TabOrder = 7
        OnClick = Modified
      end
      object chkAllowMultiInstances: TCheckBox
        Left = 220
        Top = 8
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Allow multiple application instances'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = Modified
      end
      object chkColorBars: TCheckBox
        Left = 220
        Top = 148
        Width = 475
        Height = 17
        Caption = 'Display bars in various list columns'
        TabOrder = 9
        OnClick = Modified
      end
      object editMySQLBinaries: TButtonedEdit
        Left = 220
        Top = 174
        Width = 451
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 10
        Text = 'editMySQLBinaries'
        TextHint = 'Find mysql.exe directory'
        OnChange = Modified
        OnDblClick = editMySQLBinariesRightButtonClick
        OnRightButtonClick = editMySQLBinariesRightButtonClick
      end
      object comboAppLanguage: TComboBox
        Tag = 1
        Left = 220
        Top = 228
        Width = 451
        Height = 22
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 12
        OnClick = Modified
      end
      object editCustomSnippetsDirectory: TButtonedEdit
        Left = 220
        Top = 201
        Width = 451
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 11
        Text = 'editCustomSnippetsDirectory'
        TextHint = 'Set custom directory for SQL snippet files'
        OnChange = Modified
        OnDblClick = editCustomSnippetsDirectoryRightButtonClick
        OnRightButtonClick = editCustomSnippetsDirectoryRightButtonClick
      end
      object comboGUIFont: TComboBox
        Tag = 1
        Left = 220
        Top = 255
        Width = 338
        Height = 22
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 13
        OnChange = comboGUIFontChange
      end
      object editGUIFontSize: TEdit
        Tag = 1
        Left = 565
        Top = 255
        Width = 50
        Height = 22
        Anchors = [akTop, akRight]
        TabOrder = 14
        Text = '8'
        OnChange = Modified
      end
      object updownGUIFontSize: TUpDown
        Tag = 1
        Left = 615
        Top = 255
        Width = 16
        Height = 22
        Anchors = [akTop, akRight]
        Associate = editGUIFontSize
        Min = 4
        Position = 8
        TabOrder = 15
        OnChanging = anyUpDownLimitChanging
      end
      object chkWheelZoom: TCheckBox
        Left = 220
        Top = 123
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use Ctrl+Mousewheel for zooming'
        TabOrder = 8
        OnClick = Modified
      end
      object comboTheme: TComboBox
        Tag = 1
        Left = 220
        Top = 282
        Width = 338
        Height = 22
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        Sorted = True
        TabOrder = 16
        OnSelect = comboThemeSelect
      end
      object comboIconPack: TComboBox
        Left = 220
        Top = 309
        Width = 451
        Height = 22
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 18
        OnChange = Modified
      end
      object comboWebSearchBaseUrl: TComboBox
        Left = 220
        Top = 336
        Width = 451
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 19
        Text = 'comboWebSearchBaseUrl'
        OnChange = Modified
        Items.Strings = (
          'https://www.ecosia.org/search?q=%query'
          'https://www.startpage.com/do/search?query=%query'
          'https://duckduckgo.com/?q=%query'
          'https://www.baidu.com/s?wd=%query'
          'https://www.searchencrypt.com/search?q=%query'
          'https://yandex.com/search/?text=%query'
          'https://www.bing.com/search?q=%query'
          'https://www.google.com/search?q=%query')
      end
      object chkThemePreview: TCheckBox
        Left = 564
        Top = 284
        Width = 97
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Preview'
        TabOrder = 17
        OnClick = chkThemePreviewClick
      end
    end
    object tabLogging: TTabSheet
      Caption = 'Logging'
      ImageIndex = 56
      ImageName = 'icons8-index'
      DesignSize = (
        676
        406)
      object Label4: TLabel
        Left = 8
        Top = 11
        Width = 42
        Height = 14
        Caption = 'Log last'
      end
      object lblLogLinesHint: TLabel
        Left = 297
        Top = 11
        Width = 82
        Height = 14
        Caption = 'lines in SQL log'
      end
      object lblLogSnipHint: TLabel
        Left = 297
        Top = 38
        Width = 160
        Height = 14
        Caption = 'characters  (0 = no snipping)'
      end
      object lblLogSnip: TLabel
        Left = 8
        Top = 38
        Width = 112
        Height = 14
        Caption = 'Snip SQL log lines to'
      end
      object lblLogLevel: TLabel
        Left = 8
        Top = 95
        Width = 65
        Height = 14
        Caption = 'Log events:'
      end
      object lblQueryHistoryKeepDays: TLabel
        Left = 438
        Top = 247
        Width = 241
        Height = 14
        Caption = 'days to keep queries before removing them'
      end
      object editLogLines: TEdit
        Left = 220
        Top = 8
        Width = 53
        Height = 22
        TabOrder = 0
        Text = '1'
        OnChange = Modified
      end
      object updownLogLines: TUpDown
        Left = 273
        Top = 8
        Width = 16
        Height = 22
        Associate = editLogLines
        Min = 1
        Max = 32767
        Position = 1
        TabOrder = 1
        Wrap = True
        OnChanging = anyUpDownLimitChanging
      end
      object updownLogSnip: TUpDown
        Left = 273
        Top = 35
        Width = 16
        Height = 22
        Associate = editLogSnip
        Max = 32767
        Position = 2000
        TabOrder = 3
        OnChanging = anyUpDownLimitChanging
      end
      object editLogSnip: TEdit
        Left = 220
        Top = 35
        Width = 53
        Height = 22
        TabOrder = 2
        Text = '2000'
        OnChange = Modified
      end
      object chkLogToFile: TCheckBox
        Left = 220
        Top = 64
        Width = 135
        Height = 17
        Caption = 'Write SQL log to file'
        TabOrder = 4
        OnClick = chkLogToFileClick
      end
      object chkLogEventErrors: TCheckBox
        Left = 220
        Top = 94
        Width = 475
        Height = 17
        Caption = 'Errors'
        TabOrder = 6
        OnClick = Modified
      end
      object chkLogEventUserGeneratedSQL: TCheckBox
        Left = 220
        Top = 117
        Width = 475
        Height = 17
        Caption = 'User-generated SQL queries'
        TabOrder = 7
        OnClick = Modified
      end
      object chkLogEventSQL: TCheckBox
        Left = 220
        Top = 140
        Width = 475
        Height = 17
        Caption = 'Internal SQL queries'
        TabOrder = 8
        OnClick = Modified
      end
      object chkLogEventInfo: TCheckBox
        Left = 220
        Top = 186
        Width = 475
        Height = 17
        Caption = 'Information messages'
        TabOrder = 10
        OnClick = Modified
      end
      object chkLogEventDebug: TCheckBox
        Left = 220
        Top = 209
        Width = 475
        Height = 17
        Caption = 'Debug messages'
        TabOrder = 11
        OnClick = Modified
      end
      object editLogDir: TButtonedEdit
        Left = 361
        Top = 62
        Width = 310
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 5
        Text = 'editLogDir'
        TextHint = 'Select output directory'
        OnChange = Modified
        OnDblClick = editLogDirRightButtonClick
        OnRightButtonClick = editLogDirRightButtonClick
      end
      object chkQueryHistory: TCheckBox
        Left = 220
        Top = 246
        Width = 135
        Height = 17
        Caption = 'Enable query history'
        TabOrder = 12
        OnClick = chkQueryHistoryClick
      end
      object chkHorizontalScrollbar: TCheckBox
        Left = 220
        Top = 269
        Width = 475
        Height = 17
        Caption = 'Horizontal scrollbar'
        TabOrder = 15
        OnClick = Modified
      end
      object editQueryHistoryKeepDays: TEdit
        Left = 361
        Top = 244
        Width = 53
        Height = 22
        Enabled = False
        TabOrder = 13
        Text = '1'
        OnChange = Modified
      end
      object updownQueryHistoryKeepDays: TUpDown
        Left = 414
        Top = 244
        Width = 16
        Height = 22
        Associate = editQueryHistoryKeepDays
        Enabled = False
        Min = 1
        Max = 365
        Position = 1
        TabOrder = 14
        OnChanging = anyUpDownLimitChanging
      end
      object chkLogEventScript: TCheckBox
        Left = 220
        Top = 163
        Width = 475
        Height = 17
        Caption = 'Import/script queries'
        TabOrder = 9
        OnClick = Modified
      end
      object chkLogTimestamp: TCheckBox
        Left = 220
        Top = 292
        Width = 475
        Height = 17
        Caption = 'Add timestamp to all log messages'
        TabOrder = 16
        OnClick = Modified
      end
    end
    object tabSQL: TTabSheet
      Caption = 'SQL'
      ImageIndex = 57
      ImageName = 'icons8-play'
      DesignSize = (
        676
        406)
      object lblFont: TLabel
        Left = 8
        Top = 11
        Width = 63
        Height = 14
        Caption = 'Editor font:'
      end
      object lblSQLFontSizeUnit: TLabel
        Left = 446
        Top = 11
        Width = 12
        Height = 14
        Caption = 'pt'
      end
      object Label1: TLabel
        Left = 8
        Top = 37
        Width = 60
        Height = 14
        Caption = 'Tab width:'
      end
      object lblMaxQueryResults: TLabel
        Left = 8
        Top = 64
        Width = 114
        Height = 14
        Caption = 'Maximum result sets:'
      end
      object lblSQLColElement: TLabel
        Left = 8
        Top = 186
        Width = 49
        Height = 14
        Caption = 'Element:'
      end
      object lblSQLColBackground: TLabel
        Left = 220
        Top = 280
        Width = 68
        Height = 14
        Caption = 'Background:'
      end
      object lblSQLColForeground: TLabel
        Left = 220
        Top = 233
        Width = 67
        Height = 14
        Caption = 'Foreground:'
      end
      object lblEditorColorsPreset: TLabel
        Left = 8
        Top = 159
        Width = 75
        Height = 14
        Caption = 'Colors preset:'
      end
      object lblCompletionProposal: TLabel
        Left = 8
        Top = 87
        Width = 144
        Height = 14
        Caption = 'Auto completion proposal:'
      end
      object lblCompletionProposalIntervalUnit: TLabel
        Left = 446
        Top = 87
        Width = 15
        Height = 14
        Caption = 'ms'
        Enabled = False
      end
      object comboSQLFontName: TComboBox
        Left = 220
        Top = 8
        Width = 145
        Height = 22
        Style = csDropDownList
        TabOrder = 0
        OnChange = SQLFontChange
      end
      object editSQLFontSize: TEdit
        Left = 371
        Top = 8
        Width = 43
        Height = 22
        TabOrder = 1
        Text = '9'
        OnExit = SQLFontChange
      end
      object updownSQLFontSize: TUpDown
        Left = 414
        Top = 8
        Width = 16
        Height = 22
        Associate = editSQLFontSize
        Position = 9
        TabOrder = 2
        OnClick = updownSQLFontSizeClick
      end
      object chkCompletionProposal: TCheckBox
        Left = 220
        Top = 87
        Width = 101
        Height = 17
        Caption = 'Enable'
        TabOrder = 8
        OnClick = chkCompletionProposalClick
      end
      object chkTabsToSpaces: TCheckBox
        Left = 294
        Top = 36
        Width = 136
        Height = 17
        Caption = 'Tabs to spaces'
        TabOrder = 5
        OnClick = Modified
      end
      object editSQLTabWidth: TEdit
        Left = 220
        Top = 34
        Width = 41
        Height = 22
        TabOrder = 3
        Text = '0'
        OnExit = SQLFontChange
      end
      object updownSQLTabWidth: TUpDown
        Left = 261
        Top = 34
        Width = 16
        Height = 22
        Associate = editSQLTabWidth
        TabOrder = 4
        OnClick = updownSQLFontSizeClick
      end
      object editMaxQueryResults: TEdit
        Left = 220
        Top = 61
        Width = 41
        Height = 22
        TabOrder = 6
        Text = '1'
        OnChange = Modified
      end
      object updownMaxQueryResults: TUpDown
        Left = 261
        Top = 61
        Width = 16
        Height = 22
        Associate = editMaxQueryResults
        Min = 1
        Position = 1
        TabOrder = 7
        OnChanging = anyUpDownLimitChanging
      end
      object comboSQLColElement: TComboBox
        Left = 220
        Top = 183
        Width = 145
        Height = 22
        Style = csDropDownList
        TabOrder = 15
        OnChange = comboSQLColElementChange
      end
      object chkSQLBold: TCheckBox
        Left = 220
        Top = 210
        Width = 61
        Height = 17
        Caption = 'Bold'
        TabOrder = 16
        OnClick = SQLFontChange
      end
      object chkSQLItalic: TCheckBox
        Left = 294
        Top = 210
        Width = 50
        Height = 17
        Caption = 'Italic'
        TabOrder = 17
        OnClick = SQLFontChange
      end
      object cboxSQLColForeground: TColorBox
        Left = 220
        Top = 252
        Width = 145
        Height = 22
        NoneColorColor = clNone
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 18
        OnChange = SQLFontChange
      end
      object cboxSQLColBackground: TColorBox
        Left = 220
        Top = 299
        Width = 145
        Height = 22
        NoneColorColor = clNone
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 19
        OnChange = SQLFontChange
      end
      object SynMemoSQLSample: TSynMemo
        AlignWithMargins = True
        Left = 371
        Top = 156
        Width = 300
        Height = 223
        Cursor = crHandPoint
        SingleLineMode = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 20
        OnClick = SynMemoSQLSampleClick
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
        Gutter.Visible = False
        Gutter.Width = 0
        Highlighter = SynSQLSynSQLSample
        Lines.Strings = (
          'SynMemoSQLSample')
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        RightEdge = 0
        TabWidth = 2
        WordWrap = True
        OnChange = SQLFontChange
        FontSmoothing = fsmNone
      end
      object chkQueryWarningsMessage: TCheckBox
        Left = 220
        Top = 110
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show query warnings dialog'
        TabOrder = 12
        OnClick = Modified
      end
      object chkAutoUppercase: TCheckBox
        Left = 220
        Top = 133
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Auto uppercase reserved words and functions'
        TabOrder = 13
        OnClick = Modified
      end
      object comboEditorColorsPreset: TComboBox
        Left = 220
        Top = 156
        Width = 145
        Height = 22
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 14
        Text = 'Current custom settings'
        OnChange = comboEditorColorsPresetChange
        Items.Strings = (
          'Current custom settings')
      end
      object chkCompletionProposalSearchOnMid: TCheckBox
        Left = 496
        Top = 87
        Width = 175
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Find matches in the middle'
        Enabled = False
        TabOrder = 11
        OnClick = Modified
      end
      object editCompletionProposalInterval: TEdit
        Left = 371
        Top = 84
        Width = 43
        Height = 22
        Enabled = False
        TabOrder = 9
        Text = '0'
        OnChange = Modified
      end
      object updownCompletionProposalInterval: TUpDown
        Left = 414
        Top = 84
        Width = 20
        Height = 22
        Associate = editCompletionProposalInterval
        Enabled = False
        TabOrder = 10
        OnChanging = anyUpDownLimitChanging
      end
    end
    object tabGridFormatting: TTabSheet
      Caption = 'Grid formatting'
      ImageIndex = 41
      ImageName = 'icons8-data-grid'
      DesignSize = (
        676
        406)
      object lblMaxColWidth: TLabel
        Left = 8
        Top = 11
        Width = 132
        Height = 14
        Caption = 'Maximum column width:'
      end
      object lblDataFontHint: TLabel
        Left = 444
        Top = 93
        Width = 12
        Height = 14
        Caption = 'pt'
      end
      object lblDataFont: TLabel
        Left = 8
        Top = 92
        Width = 29
        Height = 14
        Caption = 'Font:'
        FocusControl = comboDataFontName
      end
      object lblMaxTotalRows: TLabel
        Left = 8
        Top = 38
        Width = 165
        Height = 14
        Caption = 'Rows per page and maximum:'
      end
      object lblGridRowsLinecount: TLabel
        Left = 8
        Top = 65
        Width = 140
        Height = 14
        Caption = 'Lines of text in grid rows:'
      end
      object lblGridTextColors: TLabel
        Left = 8
        Top = 119
        Width = 87
        Height = 14
        Caption = 'Grid text colors:'
      end
      object lblNullBackground: TLabel
        Left = 8
        Top = 173
        Width = 100
        Height = 14
        Hint = 'Use "None" to disable'
        Caption = 'NULL background:'
      end
      object Label2: TLabel
        Left = 8
        Top = 201
        Width = 157
        Height = 14
        Caption = 'Alternating row background:'
      end
      object Label3: TLabel
        Left = 8
        Top = 229
        Width = 129
        Height = 14
        Caption = 'Same text background:'
      end
      object lblLongSortRowNum: TLabel
        Left = 8
        Top = 285
        Width = 146
        Height = 14
        Caption = 'Sort warning on grid rows:'
      end
      object lblRealTrailingZeros: TLabel
        Left = 8
        Top = 257
        Width = 153
        Height = 14
        Caption = 'Max decimal zeros for floats:'
      end
      object lblRealTrailingZerosHint: TLabel
        Left = 296
        Top = 257
        Width = 143
        Height = 14
        Caption = 'Set to -1 to keep all zeros'
      end
      object editMaxColWidth: TEdit
        Left = 220
        Top = 8
        Width = 42
        Height = 22
        TabOrder = 0
        Text = '1'
        OnChange = Modified
      end
      object updownMaxColWidth: TUpDown
        Left = 262
        Top = 8
        Width = 16
        Height = 22
        Associate = editMaxColWidth
        Min = 1
        Max = 1000
        Position = 1
        TabOrder = 1
        OnChanging = anyUpDownLimitChanging
      end
      object comboDataFontName: TComboBox
        Left = 220
        Top = 89
        Width = 145
        Height = 22
        Style = csDropDownList
        TabOrder = 6
        OnChange = DataFontsChange
      end
      object editDataFontSize: TEdit
        Left = 371
        Top = 89
        Width = 42
        Height = 22
        TabOrder = 7
        Text = '8'
        OnChange = DataFontsChange
      end
      object updownDataFontSize: TUpDown
        Left = 413
        Top = 89
        Width = 16
        Height = 22
        Associate = editDataFontSize
        Position = 8
        TabOrder = 8
      end
      object editGridRowCountMax: TEdit
        Left = 371
        Top = 35
        Width = 132
        Height = 22
        NumbersOnly = True
        TabOrder = 3
        OnChange = Modified
        OnExit = editGridRowCountExit
      end
      object editGridRowCountStep: TEdit
        Left = 220
        Top = 35
        Width = 145
        Height = 22
        NumbersOnly = True
        TabOrder = 2
        OnChange = Modified
        OnExit = editGridRowCountExit
      end
      object editGridRowsLineCount: TEdit
        Left = 220
        Top = 62
        Width = 42
        Height = 22
        TabOrder = 4
        Text = '1'
        OnChange = Modified
      end
      object updownGridRowsLineCount: TUpDown
        Left = 262
        Top = 62
        Width = 16
        Height = 22
        Associate = editGridRowsLineCount
        Min = 1
        Position = 1
        TabOrder = 5
        OnChanging = anyUpDownLimitChanging
      end
      object comboGridTextColors: TComboBox
        Left = 220
        Top = 143
        Width = 145
        Height = 22
        Style = csDropDownList
        TabOrder = 10
        OnSelect = comboGridTextColorsSelect
      end
      object colorBoxGridTextColors: TColorBox
        Left = 371
        Top = 143
        Width = 132
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
        TabOrder = 11
        OnSelect = colorBoxGridTextColorsSelect
      end
      object cboxNullBackground: TColorBox
        Left = 220
        Top = 170
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        TabOrder = 12
        OnChange = Modified
        OnSelect = Modified
      end
      object cboxRowBackgroundOdd: TColorBox
        Left = 371
        Top = 198
        Width = 132
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        TabOrder = 14
        OnChange = Modified
      end
      object cboxRowBackgroundEven: TColorBox
        Left = 220
        Top = 198
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        TabOrder = 13
        OnChange = Modified
      end
      object chkLocalNumberFormat: TCheckBox
        Left = 220
        Top = 310
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Local number format'
        TabOrder = 20
        OnClick = Modified
      end
      object chkHintsOnResultTabs: TCheckBox
        Left = 220
        Top = 356
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Pop up SQL text over result tabs'
        TabOrder = 22
        OnClick = Modified
      end
      object cboxRowHighlightSameText: TColorBox
        Left = 220
        Top = 226
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        TabOrder = 15
        OnSelect = Modified
      end
      object comboGridTextColorsPreset: TComboBox
        Left = 220
        Top = 116
        Width = 145
        Height = 22
        Style = csDropDownList
        TabOrder = 9
        OnSelect = comboGridTextColorsPresetSelect
      end
      object editLongSortRowNum: TEdit
        Left = 220
        Top = 282
        Width = 145
        Height = 22
        TabOrder = 18
        Text = '0'
      end
      object updownLongSortRowNum: TUpDown
        Left = 365
        Top = 282
        Width = 16
        Height = 22
        Associate = editLongSortRowNum
        Max = 2147483647
        TabOrder = 19
        OnChanging = anyUpDownLimitChanging
      end
      object chkLowercaseHex: TCheckBox
        Left = 220
        Top = 333
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Lowercase hexadecimal'
        TabOrder = 21
      end
      object editRealTrailingZeros: TEdit
        Left = 220
        Top = 254
        Width = 42
        Height = 22
        TabOrder = 16
        Text = '0'
      end
      object updownRealTrailingZeros: TUpDown
        Left = 262
        Top = 254
        Width = 16
        Height = 22
        Associate = editRealTrailingZeros
        Min = -1
        TabOrder = 17
      end
      object chkShowRowId: TCheckBox
        Left = 220
        Top = 379
        Width = 453
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show static row id column'
        TabOrder = 23
        OnClick = Modified
      end
    end
    object tabDataEditors: TTabSheet
      Caption = 'Data editors'
      ImageIndex = 33
      ImageName = 'icons8-compose'
      DesignSize = (
        676
        406)
      object lblLineBreakStyle: TLabel
        Left = 3
        Top = 235
        Width = 124
        Height = 14
        Caption = 'Default linebreak style:'
      end
      object chkEditorBinary: TCheckBox
        Left = 220
        Top = 8
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable popup BLOB/HEX editor'
        TabOrder = 0
        OnClick = Modified
      end
      object chkEditorDatetime: TCheckBox
        Left = 220
        Top = 31
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable inplace date/time editor'
        TabOrder = 1
        OnClick = Modified
      end
      object chkPrefillDateTime: TCheckBox
        Left = 220
        Top = 54
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Prefill empty date/time fields with current date/time'
        TabOrder = 2
        OnClick = Modified
      end
      object chkEditorEnum: TCheckBox
        Left = 220
        Top = 77
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable ENUM pulldown editor'
        TabOrder = 3
        OnClick = Modified
      end
      object chkEditorSet: TCheckBox
        Left = 220
        Top = 100
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable SET checkbox editor'
        TabOrder = 4
        OnClick = Modified
      end
      object chkReuseEditorConfiguration: TCheckBox
        Left = 220
        Top = 163
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Remember filters, sorting and column selection across sessions'
        TabOrder = 6
        OnClick = Modified
      end
      object chkForeignDropDown: TCheckBox
        Left = 220
        Top = 186
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show values in foreign key columns'
        TabOrder = 7
        OnClick = Modified
      end
      object comboLineBreakStyle: TComboBox
        Left = 220
        Top = 232
        Width = 145
        Height = 22
        Style = csDropDownList
        TabOrder = 9
        OnClick = Modified
      end
      object chkColumnHeaderClick: TCheckBox
        Left = 220
        Top = 140
        Width = 475
        Height = 17
        Caption = 'Click on column headers toggles sorting'
        TabOrder = 5
        OnClick = Modified
      end
      object chkIncrementalSearch: TCheckBox
        Left = 220
        Top = 209
        Width = 469
        Height = 17
        Caption = 'Incremental search through typing'
        TabOrder = 8
        OnClick = Modified
      end
    end
    object tabShortcuts: TTabSheet
      Caption = 'Shortcuts'
      ImageIndex = 13
      ImageName = 'icons8-lightning-bolt-100'
      DesignSize = (
        676
        406)
      object lblShortcut1: TLabel
        Left = 306
        Top = 64
        Width = 52
        Height = 14
        Caption = 'Shortcut:'
      end
      object lblShortcutHint: TLabel
        Left = 306
        Top = 3
        Width = 367
        Height = 55
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Please select a shortcut item in the tree.'
        WordWrap = True
      end
      object lblShortcut2: TLabel
        Left = 306
        Top = 108
        Width = 111
        Height = 14
        Caption = 'Secondary shortcut:'
      end
      object TreeShortcutItems: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 300
        Height = 406
        Align = alLeft
        Colors.BorderColor = 15987699
        Colors.DisabledColor = clGray
        Colors.DropMarkColor = 15385233
        Colors.DropTargetColor = 15385233
        Colors.DropTargetBorderColor = 15385233
        Colors.FocusedSelectionColor = 15385233
        Colors.FocusedSelectionBorderColor = 15385233
        Colors.GridLineColor = 15987699
        Colors.HeaderHotColor = clBlack
        Colors.HotColor = clBlack
        Colors.SelectionRectangleBlendColor = 15385233
        Colors.SelectionRectangleBorderColor = 15385233
        Colors.SelectionTextColor = clBlack
        Colors.TreeLineColor = 9471874
        Colors.UnfocusedColor = clGray
        Colors.UnfocusedSelectionColor = clWhite
        Colors.UnfocusedSelectionBorderColor = clWhite
        Header.AutoSizeIndex = 0
        Header.Height = 14
        Header.MainColumn = -1
        Images = MainForm.VirtualImageListMain
        TabOrder = 0
        OnFocusChanged = TreeShortcutItemsFocusChanged
        OnGetText = TreeShortcutItemsGetText
        OnGetImageIndex = TreeShortcutItemsGetImageIndex
        OnGetNodeDataSize = TreeShortcutItemsGetNodeDataSize
        OnInitChildren = TreeShortcutItemsInitChildren
        OnInitNode = TreeShortcutItemsInitNode
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <>
      end
      object btnRemoveHotKey1: TButton
        Left = 592
        Top = 80
        Width = 81
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Remove'
        ImageIndex = 26
        Images = MainForm.VirtualImageListMain
        TabOrder = 1
        OnClick = btnRemoveHotKeyClick
      end
      object btnRemoveHotKey2: TButton
        Left = 592
        Top = 144
        Width = 81
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Remove'
        ImageIndex = 26
        Images = MainForm.VirtualImageListMain
        TabOrder = 2
        OnClick = btnRemoveHotKeyClick
      end
    end
    object tabFiles: TTabSheet
      Caption = 'Files and tabs'
      ImageIndex = 10
      ImageName = 'icons8-save-button-100'
      DesignSize = (
        676
        406)
      object Label5: TLabel
        Left = 8
        Top = 103
        Width = 148
        Height = 14
        Caption = 'Grayscale inactive tab icons'
      end
      object chkAskFileSave: TCheckBox
        Left = 220
        Top = 8
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Prompt to save modified files on tab close'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = Modified
      end
      object chkRestoreTabs: TCheckBox
        Left = 220
        Top = 31
        Width = 451
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Reopen previously used SQL files and unsaved content in tabs *'
        TabOrder = 1
        OnClick = Modified
      end
      object chkTabCloseOnDoubleClick: TCheckBox
        Left = 220
        Top = 54
        Width = 477
        Height = 17
        Caption = 'Close tab on doubleclick'
        TabOrder = 2
        OnClick = Modified
      end
      object chkTabCloseOnMiddleClick: TCheckBox
        Left = 220
        Top = 77
        Width = 453
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Close tab on middleclick'
        TabOrder = 3
        OnClick = Modified
      end
      object comboTabIconsGrayscaleMode: TComboBox
        Left = 220
        Top = 100
        Width = 451
        Height = 22
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnClick = Modified
        Items.Strings = (
          'Color icons on all tabs'
          'Grayscale icons on inactive query tabs only'
          'Grayscale icons on every inactive tab')
      end
    end
  end
  object btnCancel: TButton
    Left = 537
    Top = 449
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 457
    Top = 449
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = Apply
  end
  object btnApply: TButton
    Left = 617
    Top = 449
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    TabOrder = 3
    OnClick = Apply
  end
  object btnRestoreDefaults: TButton
    Left = 8
    Top = 449
    Width = 177
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Restore defaults'
    TabOrder = 0
    OnClick = btnRestoreDefaultsClick
  end
  object SynSQLSynSQLSample: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    SQLDialect = sqlMySQL
    Left = 584
    Top = 392
  end
  object SynSQLSyn_Dark: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = 8710076
    ConditionalCommentAttri.Foreground = 8710076
    DataTypeAttri.Foreground = 11184895
    DelimitedIdentifierAttri.Foreground = 6460927
    FunctionAttri.Foreground = 15792639
    IdentifierAttri.Foreground = 6460927
    KeyAttri.Foreground = 15792639
    NumberAttri.Foreground = 4610525
    StringAttri.Foreground = 5293907
    SymbolAttri.Foreground = 15792639
    TableNameAttri.Foreground = 16755327
    VariableAttri.Foreground = clPurple
    SQLDialect = sqlMySQL
    Left = 592
    Top = 168
  end
  object SynSQLSyn_Light: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clGray
    ConditionalCommentAttri.Foreground = clGray
    DataTypeAttri.Foreground = clMaroon
    DelimitedIdentifierAttri.Foreground = clOlive
    FunctionAttri.Foreground = clNavy
    IdentifierAttri.Foreground = clOlive
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clPurple
    StringAttri.Foreground = clGreen
    SymbolAttri.Foreground = clBlue
    TableNameAttri.Foreground = clFuchsia
    VariableAttri.Foreground = clPurple
    SQLDialect = sqlMySQL
    Left = 592
    Top = 112
  end
  object SynSQLSyn_Black: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clBlack
    ConditionalCommentAttri.Foreground = clBlack
    DataTypeAttri.Foreground = clBlack
    DelimitedIdentifierAttri.Foreground = clBlack
    FunctionAttri.Foreground = clBlack
    IdentifierAttri.Foreground = clBlack
    KeyAttri.Foreground = clBlack
    NumberAttri.Foreground = clBlack
    StringAttri.Foreground = clBlack
    SymbolAttri.Foreground = clBlack
    TableNameAttri.Foreground = clBlack
    VariableAttri.Foreground = clBlack
    SQLDialect = sqlMySQL
    Left = 588
    Top = 228
  end
  object SynSQLSyn_White: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clWhite
    ConditionalCommentAttri.Foreground = clWhite
    DataTypeAttri.Foreground = clWhite
    DelimitedIdentifierAttri.Foreground = clWhite
    FunctionAttri.Foreground = clWhite
    IdentifierAttri.Foreground = clWhite
    KeyAttri.Foreground = clWhite
    NumberAttri.Foreground = clWhite
    StringAttri.Foreground = clWhite
    SymbolAttri.Foreground = clWhite
    TableNameAttri.Foreground = clWhite
    VariableAttri.Foreground = clWhite
    SQLDialect = sqlMySQL
    Left = 588
    Top = 284
  end
  object SynSQLSyn_Material: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = 8023636
    ConditionalCommentAttri.Foreground = 12108397
    DataTypeAttri.Foreground = 15372999
    DelimitedIdentifierAttri.Foreground = 16757122
    FunctionAttri.Foreground = 14929603
    IdentifierAttri.Foreground = 16757122
    KeyAttri.Foreground = 14929603
    NumberAttri.Foreground = 7361535
    StringAttri.Foreground = 8906947
    SymbolAttri.Foreground = 12897152
    TableNameAttri.Foreground = 6911735
    VariableAttri.Foreground = 7064575
    SQLDialect = sqlMySQL
    Left = 584
    Top = 340
  end
end
