object optionsform: Toptionsform
  Left = 547
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 470
  ClientWidth = 732
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    732
    470)
  PixelsPerInch = 96
  TextHeight = 13
  object pagecontrolMain: TPageControl
    Left = 8
    Top = 8
    Width = 718
    Height = 427
    ActivePage = tabMisc
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = MainForm.ImageListMain
    TabOrder = 4
    OnChange = pagecontrolMainChange
    OnChanging = pagecontrolMainChanging
    object tabMisc: TTabSheet
      Caption = 'General'
      ImageIndex = 137
      object pnlDpiHelperGeneral: TPanel
        Left = 0
        Top = 0
        Width = 710
        Height = 398
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlDpiHelperGeneral'
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          710
          398)
        object lblMySQLBinaries: TLabel
          Left = 8
          Top = 177
          Width = 152
          Height = 13
          Caption = 'MySQL command line programs:'
        end
        object lblLanguage: TLabel
          Left = 8
          Top = 231
          Width = 112
          Height = 13
          Caption = 'Application language: *'
        end
        object lblCustomSnippetsDirectory: TLabel
          Left = 8
          Top = 204
          Width = 129
          Height = 13
          Caption = 'Custom snippets directory:'
        end
        object lblGUIFont: TLabel
          Left = 8
          Top = 258
          Width = 54
          Height = 13
          Caption = 'GUI font: *'
        end
        object lblGUIFontSize: TLabel
          Left = 674
          Top = 258
          Width = 10
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'pt'
        end
        object lblTheme: TLabel
          Left = 8
          Top = 285
          Width = 72
          Height = 13
          Caption = 'Style Theme: *'
        end
        object chkAutoReconnect: TCheckBox
          Left = 191
          Top = 31
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatically reconnect to previously opened sessions on startup'
          TabOrder = 1
          OnClick = Modified
        end
        object chkRestoreLastDB: TCheckBox
          Left = 191
          Top = 54
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Restore last used database on startup'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = Modified
        end
        object chkUpdatecheck: TCheckBox
          Left = 191
          Top = 77
          Width = 177
          Height = 17
          Hint = '0 = on each application start'
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Check for updates each [day]:'
          TabOrder = 3
          WordWrap = True
          OnClick = chkUpdatecheckClick
        end
        object editUpdatecheckInterval: TEdit
          Left = 374
          Top = 75
          Width = 43
          Height = 21
          Anchors = [akTop, akRight]
          Enabled = False
          TabOrder = 4
          Text = '1'
          OnChange = Modified
        end
        object updownUpdatecheckInterval: TUpDown
          Left = 417
          Top = 75
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
          Left = 488
          Top = 77
          Width = 209
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Also check for updated nightly builds'
          Enabled = False
          TabOrder = 6
          OnClick = Modified
        end
        object chkDoStatistics: TCheckBox
          Left = 191
          Top = 100
          Width = 510
          Height = 17
          Hint = 
            'This option, if enabled, will cause HeidiSQL to ping heidisql.co' +
            'm at most once every month.  This is used to count the used Heid' +
            'iSQL and server versions.'
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Send usage statistics'
          TabOrder = 7
          WordWrap = True
          OnClick = Modified
        end
        object chkAllowMultiInstances: TCheckBox
          Left = 191
          Top = 8
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Allow multiple application instances'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object chkColorBars: TCheckBox
          Left = 191
          Top = 148
          Width = 510
          Height = 17
          Caption = 'Display bars in various list columns'
          TabOrder = 9
          OnClick = Modified
        end
        object editMySQLBinaries: TButtonedEdit
          Left = 191
          Top = 174
          Width = 510
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Images = MainForm.ImageListMain
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
          Left = 191
          Top = 228
          Width = 510
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 12
          OnClick = Modified
        end
        object editCustomSnippetsDirectory: TButtonedEdit
          Left = 191
          Top = 201
          Width = 510
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Images = MainForm.ImageListMain
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
          Left = 191
          Top = 255
          Width = 402
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 13
          OnChange = comboGUIFontChange
        end
        object editGUIFontSize: TEdit
          Tag = 1
          Left = 599
          Top = 255
          Width = 50
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 14
          Text = '8'
          OnChange = Modified
        end
        object updownGUIFontSize: TUpDown
          Tag = 1
          Left = 649
          Top = 255
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = editGUIFontSize
          Min = 4
          Position = 8
          TabOrder = 15
          OnChanging = anyUpDownLimitChanging
        end
        object chkWheelZoom: TCheckBox
          Left = 191
          Top = 123
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Use Ctrl+Mousewheel for zooming'
          TabOrder = 8
          OnClick = Modified
        end
        object comboTheme: TComboBox
          Tag = 1
          Left = 191
          Top = 282
          Width = 510
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          Sorted = True
          TabOrder = 16
          OnChange = Modified
        end
      end
    end
    object tabLogging: TTabSheet
      Caption = 'Logging'
      ImageIndex = 56
      object pnlDpiHelperLogging: TPanel
        Left = 0
        Top = 0
        Width = 710
        Height = 398
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlDpiHelperLogging'
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          710
          398)
        object Label4: TLabel
          Left = 8
          Top = 11
          Width = 37
          Height = 13
          Caption = 'Log last'
        end
        object lblLogLinesHint: TLabel
          Left = 267
          Top = 11
          Width = 71
          Height = 13
          Caption = 'lines in SQL log'
        end
        object lblLogSnipHint: TLabel
          Left = 267
          Top = 38
          Width = 139
          Height = 13
          Caption = 'characters  (0 = no snipping)'
        end
        object lblLogSnip: TLabel
          Left = 8
          Top = 38
          Width = 96
          Height = 13
          Caption = 'Snip SQL log lines to'
        end
        object lblLogLevel: TLabel
          Left = 8
          Top = 95
          Width = 57
          Height = 13
          Caption = 'Log events:'
        end
        object lblQueryHistoryKeepDays: TLabel
          Left = 421
          Top = 212
          Width = 209
          Height = 13
          Caption = 'days to keep queries before removing them'
        end
        object editLogLines: TEdit
          Left = 191
          Top = 8
          Width = 53
          Height = 21
          TabOrder = 0
          Text = '1'
          OnChange = Modified
        end
        object updownLogLines: TUpDown
          Left = 244
          Top = 8
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
          Left = 244
          Top = 35
          Width = 16
          Height = 21
          Associate = editLogSnip
          Max = 32767
          Position = 2000
          TabOrder = 3
          OnChanging = anyUpDownLimitChanging
        end
        object editLogSnip: TEdit
          Left = 191
          Top = 35
          Width = 53
          Height = 21
          TabOrder = 2
          Text = '2.000'
          OnChange = Modified
        end
        object chkLogToFile: TCheckBox
          Left = 191
          Top = 64
          Width = 147
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Write SQL log to file'
          TabOrder = 4
          OnClick = chkLogToFileClick
        end
        object chkLogEventErrors: TCheckBox
          Left = 191
          Top = 94
          Width = 510
          Height = 17
          Caption = 'Errors'
          TabOrder = 6
          OnClick = Modified
        end
        object chkLogEventUserGeneratedSQL: TCheckBox
          Left = 191
          Top = 114
          Width = 510
          Height = 17
          Caption = 'User-generated SQL queries'
          TabOrder = 7
          OnClick = Modified
        end
        object chkLogEventSQL: TCheckBox
          Left = 191
          Top = 134
          Width = 510
          Height = 17
          Caption = 'Internal SQL queries'
          TabOrder = 8
          OnClick = Modified
        end
        object chkLogEventInfo: TCheckBox
          Left = 191
          Top = 154
          Width = 510
          Height = 17
          Caption = 'Information messages'
          TabOrder = 9
          OnClick = Modified
        end
        object chkLogEventDebug: TCheckBox
          Left = 191
          Top = 174
          Width = 510
          Height = 17
          Caption = 'Debug messages'
          TabOrder = 10
          OnClick = Modified
        end
        object editLogDir: TButtonedEdit
          Left = 344
          Top = 62
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          Images = MainForm.ImageListMain
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
          Left = 191
          Top = 211
          Width = 145
          Height = 17
          Caption = 'Enable query history'
          TabOrder = 11
          OnClick = chkQueryHistoryClick
        end
        object chkHorizontalScrollbar: TCheckBox
          Left = 191
          Top = 234
          Width = 510
          Height = 17
          Caption = 'Horizontal scrollbar'
          TabOrder = 12
          OnClick = Modified
        end
        object editQueryHistoryKeepDays: TEdit
          Left = 344
          Top = 209
          Width = 53
          Height = 21
          Enabled = False
          TabOrder = 13
          Text = '1'
          OnChange = Modified
        end
        object updownQueryHistoryKeepDays: TUpDown
          Left = 397
          Top = 209
          Width = 16
          Height = 21
          Associate = editQueryHistoryKeepDays
          Enabled = False
          Min = 1
          Max = 365
          Position = 1
          TabOrder = 14
          OnChanging = anyUpDownLimitChanging
        end
      end
    end
    object tabSQL: TTabSheet
      Caption = 'SQL'
      ImageIndex = 57
      object pnlDpiHelperSql: TPanel
        Left = 0
        Top = 0
        Width = 710
        Height = 398
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlDpiHelperSql'
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          710
          398)
        object lblFont: TLabel
          Left = 8
          Top = 11
          Width = 55
          Height = 13
          Caption = 'Editor font:'
        end
        object lblSQLFontSizeUnit: TLabel
          Left = 689
          Top = 11
          Width = 10
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'pt'
        end
        object Label1: TLabel
          Left = 8
          Top = 37
          Width = 51
          Height = 13
          Caption = 'Tab width:'
        end
        object lblMaxQueryResults: TLabel
          Left = 8
          Top = 64
          Width = 101
          Height = 13
          Caption = 'Maximum result sets:'
        end
        object lblSQLColElement: TLabel
          Left = 8
          Top = 220
          Width = 42
          Height = 13
          Caption = 'Element:'
        end
        object lblSQLColBackground: TLabel
          Left = 489
          Top = 246
          Width = 60
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Background:'
        end
        object lblSQLColForeground: TLabel
          Left = 489
          Top = 220
          Width = 60
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Foreground:'
        end
        object lblEditorColorsPreset: TLabel
          Left = 8
          Top = 193
          Width = 68
          Height = 13
          Caption = 'Colors preset:'
        end
        object lblSqlFontSize: TLabel
          Left = 489
          Top = 11
          Width = 23
          Height = 13
          Caption = 'Size:'
        end
        object comboSQLFontName: TComboBox
          Left = 191
          Top = 8
          Width = 283
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = SQLFontChange
        end
        object editSQLFontSize: TEdit
          Left = 583
          Top = 8
          Width = 81
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 1
          Text = '9'
          OnExit = SQLFontChange
        end
        object updownSQLFontSize: TUpDown
          Left = 664
          Top = 8
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = editSQLFontSize
          Position = 9
          TabOrder = 2
          OnClick = updownSQLFontSizeClick
        end
        object chkCompletionProposal: TCheckBox
          Left = 191
          Top = 87
          Width = 508
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable autocompletion'
          TabOrder = 8
          OnClick = Modified
        end
        object chkTabsToSpaces: TCheckBox
          Left = 264
          Top = 36
          Width = 437
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Tabs to spaces'
          TabOrder = 5
          OnClick = Modified
        end
        object editSQLTabWidth: TEdit
          Left = 191
          Top = 34
          Width = 41
          Height = 21
          TabOrder = 3
          Text = '0'
          OnExit = SQLFontChange
        end
        object updownSQLTabWidth: TUpDown
          Left = 232
          Top = 34
          Width = 16
          Height = 21
          Associate = editSQLTabWidth
          TabOrder = 4
          OnClick = updownSQLFontSizeClick
        end
        object chkAskFileSave: TCheckBox
          Left = 191
          Top = 111
          Width = 506
          Height = 17
          Caption = 'Prompt to save modified files on tab close'
          Checked = True
          State = cbChecked
          TabOrder = 9
          OnClick = Modified
        end
        object editMaxQueryResults: TEdit
          Left = 191
          Top = 61
          Width = 41
          Height = 21
          TabOrder = 6
          Text = '1'
          OnChange = Modified
        end
        object updownMaxQueryResults: TUpDown
          Left = 232
          Top = 61
          Width = 16
          Height = 21
          Associate = editMaxQueryResults
          Min = 1
          Position = 1
          TabOrder = 7
          OnChanging = anyUpDownLimitChanging
        end
        object comboSQLColElement: TComboBox
          Left = 190
          Top = 217
          Width = 284
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 13
          OnChange = comboSQLColElementChange
        end
        object chkSQLBold: TCheckBox
          Left = 190
          Top = 244
          Width = 81
          Height = 17
          Caption = 'Bold'
          TabOrder = 14
          OnClick = SQLFontChange
        end
        object chkSQLItalic: TCheckBox
          Left = 277
          Top = 244
          Width = 197
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Italic'
          TabOrder = 15
          OnClick = SQLFontChange
        end
        object cboxSQLColForeground: TColorBox
          Left = 583
          Top = 217
          Width = 118
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
          Anchors = [akTop, akRight]
          TabOrder = 16
          OnChange = SQLFontChange
        end
        object cboxSQLColBackground: TColorBox
          Left = 583
          Top = 243
          Width = 118
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
          Anchors = [akTop, akRight]
          TabOrder = 17
          OnChange = SQLFontChange
        end
        object SynMemoSQLSample: TSynMemo
          AlignWithMargins = True
          Left = 191
          Top = 269
          Width = 510
          Height = 119
          Cursor = crHandPoint
          SingleLineMode = False
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 18
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
          Left = 191
          Top = 134
          Width = 508
          Height = 17
          Caption = 'Show query warnings dialog'
          TabOrder = 10
          OnClick = Modified
        end
        object chkAutoUppercase: TCheckBox
          Left = 191
          Top = 159
          Width = 508
          Height = 17
          Caption = 'Auto uppercase reserved words and functions'
          TabOrder = 11
          OnClick = Modified
        end
        object comboEditorColorsPreset: TComboBox
          Left = 190
          Top = 190
          Width = 284
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 12
          Text = 'Current custom settings'
          OnChange = comboEditorColorsPresetChange
          Items.Strings = (
            'Current custom settings')
        end
      end
    end
    object tabGridFormatting: TTabSheet
      Caption = 'Grid formatting'
      ImageIndex = 41
      object pnlDpiHelperGrid: TPanel
        Left = 0
        Top = 0
        Width = 710
        Height = 398
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlDpiHelperGrid'
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          710
          398)
        object lblMaxColWidth: TLabel
          Left = 8
          Top = 11
          Width = 113
          Height = 13
          Caption = 'Maximum column width:'
        end
        object lblDataFontHint: TLabel
          Left = 393
          Top = 93
          Width = 10
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'pt'
        end
        object lblDataFont: TLabel
          Left = 8
          Top = 92
          Width = 26
          Height = 13
          Caption = 'Font:'
          FocusControl = comboDataFontName
        end
        object lblMaxTotalRows: TLabel
          Left = 8
          Top = 38
          Width = 144
          Height = 13
          Caption = 'Rows per page and maximum:'
        end
        object lblGridRowsLinecount: TLabel
          Left = 8
          Top = 65
          Width = 122
          Height = 13
          Caption = 'Lines of text in grid rows:'
        end
        object lblGridTextColors: TLabel
          Left = 8
          Top = 119
          Width = 77
          Height = 13
          Caption = 'Grid text colors:'
        end
        object lblNullBackground: TLabel
          Left = 8
          Top = 173
          Width = 87
          Height = 13
          Hint = 'Use "None" to disable'
          Caption = 'NULL background:'
        end
        object Label2: TLabel
          Left = 8
          Top = 201
          Width = 137
          Height = 13
          Caption = 'Alternating row background:'
        end
        object Label3: TLabel
          Left = 8
          Top = 229
          Width = 112
          Height = 13
          Caption = 'Same text background:'
        end
        object editMaxColWidth: TEdit
          Left = 191
          Top = 8
          Width = 42
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 0
          Text = '1'
          OnChange = Modified
        end
        object updownMaxColWidth: TUpDown
          Left = 233
          Top = 8
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
          Left = 191
          Top = 89
          Width = 133
          Height = 21
          Style = csDropDownList
          Anchors = [akTop, akRight]
          TabOrder = 6
          OnChange = DataFontsChange
        end
        object editDataFontSize: TEdit
          Left = 330
          Top = 89
          Width = 42
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 7
          Text = '8'
          OnChange = DataFontsChange
        end
        object updownDataFontSize: TUpDown
          Left = 372
          Top = 89
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = editDataFontSize
          Position = 8
          TabOrder = 8
        end
        object editGridRowCountMax: TEdit
          Left = 330
          Top = 35
          Width = 132
          Height = 21
          Anchors = [akTop, akRight]
          NumbersOnly = True
          TabOrder = 3
          OnChange = Modified
          OnExit = editGridRowCountExit
        end
        object editGridRowCountStep: TEdit
          Left = 191
          Top = 35
          Width = 133
          Height = 21
          Anchors = [akTop, akRight]
          NumbersOnly = True
          TabOrder = 2
          OnChange = Modified
          OnExit = editGridRowCountExit
        end
        object editGridRowsLineCount: TEdit
          Left = 191
          Top = 62
          Width = 42
          Height = 21
          Anchors = [akTop, akRight]
          TabOrder = 4
          Text = '1'
          OnChange = Modified
        end
        object updownGridRowsLineCount: TUpDown
          Left = 233
          Top = 62
          Width = 16
          Height = 21
          Anchors = [akTop, akRight]
          Associate = editGridRowsLineCount
          Min = 1
          Position = 1
          TabOrder = 5
          OnChanging = anyUpDownLimitChanging
        end
        object comboGridTextColors: TComboBox
          Left = 191
          Top = 143
          Width = 133
          Height = 21
          Style = csDropDownList
          Anchors = [akTop, akRight]
          TabOrder = 10
          OnSelect = comboGridTextColorsSelect
        end
        object colorBoxGridTextColors: TColorBox
          Left = 330
          Top = 143
          Width = 132
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
          Anchors = [akTop, akRight]
          TabOrder = 11
          OnSelect = colorBoxGridTextColorsSelect
        end
        object cboxNullBackground: TColorBox
          Left = 191
          Top = 170
          Width = 133
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
          Anchors = [akTop, akRight]
          TabOrder = 12
          OnChange = Modified
          OnSelect = Modified
        end
        object cboxRowBackgroundOdd: TColorBox
          Left = 330
          Top = 198
          Width = 132
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
          Anchors = [akTop, akRight]
          TabOrder = 14
          OnChange = Modified
        end
        object cboxRowBackgroundEven: TColorBox
          Left = 191
          Top = 198
          Width = 133
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
          Anchors = [akTop, akRight]
          TabOrder = 13
          OnChange = Modified
        end
        object chkLocalNumberFormat: TCheckBox
          Left = 191
          Top = 254
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Local number format'
          TabOrder = 16
          OnClick = Modified
        end
        object chkHintsOnResultTabs: TCheckBox
          Left = 191
          Top = 277
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Pop up SQL text over result tabs'
          TabOrder = 17
          OnClick = Modified
        end
        object cboxRowHighlightSameText: TColorBox
          Left = 191
          Top = 226
          Width = 133
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
          Anchors = [akTop, akRight]
          TabOrder = 15
        end
        object comboGridTextColorsPreset: TComboBox
          Left = 191
          Top = 116
          Width = 133
          Height = 21
          Style = csDropDownList
          TabOrder = 9
          OnSelect = comboGridTextColorsPresetSelect
        end
      end
    end
    object tabDataEditors: TTabSheet
      Caption = 'Data editors'
      ImageIndex = 33
      object pnlDpiHelperData: TPanel
        Left = 0
        Top = 0
        Width = 710
        Height = 398
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlDpiHelperData'
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          710
          398)
        object lblLineBreakStyle: TLabel
          Left = 8
          Top = 188
          Width = 111
          Height = 13
          Caption = 'Default linebreak style:'
        end
        object chkEditorBinary: TCheckBox
          Left = 191
          Top = 8
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable popup BLOB/HEX editor'
          TabOrder = 0
          OnClick = Modified
        end
        object chkEditorDatetime: TCheckBox
          Left = 191
          Top = 31
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable inplace date/time editor'
          TabOrder = 1
          OnClick = Modified
        end
        object chkPrefillDateTime: TCheckBox
          Left = 191
          Top = 54
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Prefill empty date/time fields with current date/time'
          TabOrder = 2
          OnClick = Modified
        end
        object chkEditorEnum: TCheckBox
          Left = 191
          Top = 77
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable ENUM pulldown editor'
          TabOrder = 3
          OnClick = Modified
        end
        object chkEditorSet: TCheckBox
          Left = 191
          Top = 100
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable SET checkbox editor'
          TabOrder = 4
          OnClick = Modified
        end
        object chkReuseEditorConfiguration: TCheckBox
          Left = 191
          Top = 139
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Remember filters, sorting and column selection across sessions'
          TabOrder = 5
        end
        object chkForeignDropDown: TCheckBox
          Left = 191
          Top = 162
          Width = 510
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Show values in foreign key columns'
          TabOrder = 6
        end
        object comboLineBreakStyle: TComboBox
          Left = 191
          Top = 185
          Width = 132
          Height = 21
          Style = csDropDownList
          TabOrder = 7
        end
      end
    end
    object tabShortcuts: TTabSheet
      Caption = 'Shortcuts'
      ImageIndex = 13
      object pnlDpiHelperShortcuts: TPanel
        Left = 0
        Top = 0
        Width = 710
        Height = 398
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlDpiHelperShortcuts'
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          710
          398)
        object lblShortcut1: TLabel
          Left = 306
          Top = 64
          Width = 45
          Height = 16
          Anchors = [akLeft, akTop, akBottom]
          Caption = 'Shortcut:'
        end
        object lblShortcutHint: TLabel
          Left = 306
          Top = 3
          Width = 279
          Height = 103
          Anchors = [akLeft, akTop, akBottom]
          AutoSize = False
          Caption = 'Please select a shortcut item in the tree.'
          WordWrap = True
        end
        object lblShortcut2: TLabel
          Left = 306
          Top = 107
          Width = 98
          Height = 16
          Anchors = [akLeft, akTop, akBottom]
          Caption = 'Secondary shortcut:'
        end
        object TreeShortcutItems: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 300
          Height = 398
          Align = alLeft
          Header.AutoSizeIndex = 0
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
          Left = 306
          Top = 81
          Width = 279
          Height = 19
          HotKey = 32768
          Modifiers = []
          OnChange = Shortcut1Change
          OnEnter = ShortcutEnter
          OnExit = ShortcutExit
        end
        object Shortcut2: TSynHotKey
          Left = 306
          Top = 125
          Width = 279
          Height = 19
          HotKey = 32768
          Modifiers = []
          OnChange = Shortcut2Change
          OnEnter = ShortcutEnter
          OnExit = ShortcutExit
        end
      end
    end
  end
  object btnCancel: TButton
    Left = 571
    Top = 439
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 491
    Top = 439
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
    Left = 651
    Top = 439
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
    Top = 439
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
    Left = 432
    Top = 352
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
    Left = 200
    Top = 352
  end
  object SynSQLSyn_Light: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clGray
    ConditionalCommentAttri.Foreground = clGray
    DataTypeAttri.Foreground = 3148901
    DelimitedIdentifierAttri.Foreground = 6383516
    FunctionAttri.Foreground = 7811334
    IdentifierAttri.Foreground = 6383516
    KeyAttri.Foreground = 11684361
    NumberAttri.Foreground = 6754886
    StringAttri.Foreground = clGreen
    SymbolAttri.Foreground = 11684361
    TableNameAttri.Foreground = 13378700
    VariableAttri.Foreground = clPurple
    SQLDialect = sqlMySQL
    Left = 120
    Top = 352
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
    Left = 196
    Top = 404
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
    Left = 284
    Top = 404
  end
end
