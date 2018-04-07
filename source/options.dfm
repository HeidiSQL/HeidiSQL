object optionsform: Toptionsform
  Left = 547
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 424
  ClientWidth = 732
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
    732
    424)
  PixelsPerInch = 96
  TextHeight = 13
  object pagecontrolMain: TPageControl
    Left = 8
    Top = 8
    Width = 718
    Height = 381
    ActivePage = tabMisc
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = MainForm.ImageListMain
    Style = tsFlatButtons
    TabOrder = 4
    OnChange = pagecontrolMainChange
    OnChanging = pagecontrolMainChanging
    object tabMisc: TTabSheet
      Caption = 'General'
      ImageIndex = 137
      DesignSize = (
        710
        349)
      object lblMySQLBinaries: TLabel
        Left = 8
        Top = 219
        Width = 152
        Height = 13
        Caption = 'MySQL command line programs:'
      end
      object lblLanguage: TLabel
        Left = 8
        Top = 273
        Width = 189
        Height = 13
        Caption = 'Application language (requires restart):'
      end
      object lblCustomSnippetsDirectory: TLabel
        Left = 8
        Top = 246
        Width = 129
        Height = 13
        Caption = 'Custom snippets directory:'
      end
      object lblGUIFont: TLabel
        Left = 8
        Top = 300
        Width = 131
        Height = 13
        Caption = 'GUI font (requires restart):'
      end
      object lblGUIFontSize: TLabel
        Left = 694
        Top = 300
        Width = 10
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'pt'
      end
      object chkAutoReconnect: TCheckBox
        Left = 8
        Top = 31
        Width = 677
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Automatically reconnect to previously opened sessions on startup'
        TabOrder = 1
        OnClick = Modified
      end
      object chkRestoreLastDB: TCheckBox
        Left = 8
        Top = 55
        Width = 677
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Restore last used database on startup'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = Modified
      end
      object chkUpdatecheck: TCheckBox
        Left = 8
        Top = 77
        Width = 620
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
        Left = 648
        Top = 77
        Width = 43
        Height = 21
        Anchors = [akTop, akRight]
        Enabled = False
        TabOrder = 4
        Text = '1'
        OnChange = Modified
      end
      object updownUpdatecheckInterval: TUpDown
        Left = 691
        Top = 77
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
        Left = 26
        Top = 111
        Width = 659
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Also check for updated nightly builds'
        Enabled = False
        TabOrder = 6
        OnClick = Modified
      end
      object chkDoStatistics: TCheckBox
        Left = 8
        Top = 134
        Width = 685
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
        Left = 8
        Top = 8
        Width = 677
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Allow multiple application instances'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkColorBars: TCheckBox
        Left = 8
        Top = 190
        Width = 310
        Height = 17
        Caption = 'Display bars in various list columns'
        TabOrder = 8
        OnClick = chkColorBarsClick
      end
      object cboxColorBars: TColorBox
        Left = 272
        Top = 188
        Width = 435
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 9
        OnClick = Modified
      end
      object editMySQLBinaries: TButtonedEdit
        Left = 272
        Top = 216
        Width = 435
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
        Left = 272
        Top = 270
        Width = 435
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 12
        OnClick = Modified
      end
      object editCustomSnippetsDirectory: TButtonedEdit
        Left = 272
        Top = 243
        Width = 435
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
        Left = 272
        Top = 297
        Width = 334
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 13
        OnChange = comboGUIFontChange
      end
      object editGUIFontSize: TEdit
        Left = 612
        Top = 297
        Width = 57
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 14
        Text = '8'
        OnChange = Modified
      end
      object updownGUIFontSize: TUpDown
        Left = 669
        Top = 297
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editGUIFontSize
        Min = 4
        Position = 8
        TabOrder = 15
        OnChanging = anyUpDownLimitChanging
      end
    end
    object tabLogging: TTabSheet
      Caption = 'Logging'
      ImageIndex = 56
      DesignSize = (
        710
        349)
      object Label4: TLabel
        Left = 8
        Top = 11
        Width = 37
        Height = 13
        Caption = 'Log last'
      end
      object lblLogLinesHint: TLabel
        Left = 278
        Top = 11
        Width = 71
        Height = 13
        Caption = 'lines in SQL log'
      end
      object lblLogSnipHint: TLabel
        Left = 278
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
        Left = 278
        Top = 201
        Width = 209
        Height = 13
        Caption = 'days to keep queries before removing them'
      end
      object editLogLines: TEdit
        Left = 202
        Top = 8
        Width = 53
        Height = 21
        TabOrder = 0
        Text = '1'
        OnChange = Modified
      end
      object updownLogLines: TUpDown
        Left = 255
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
        Left = 255
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
        Left = 202
        Top = 35
        Width = 53
        Height = 21
        TabOrder = 2
        Text = '2.000'
        OnChange = Modified
      end
      object chkLogToFile: TCheckBox
        Left = 8
        Top = 64
        Width = 442
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Write SQL log to file'
        TabOrder = 4
        OnClick = chkLogToFileClick
      end
      object chkLogEventErrors: TCheckBox
        Left = 202
        Top = 94
        Width = 240
        Height = 17
        Caption = 'Errors'
        TabOrder = 6
        OnClick = Modified
      end
      object chkLogEventUserGeneratedSQL: TCheckBox
        Left = 202
        Top = 113
        Width = 240
        Height = 17
        Caption = 'User-generated SQL queries'
        TabOrder = 7
        OnClick = Modified
      end
      object chkLogEventSQL: TCheckBox
        Left = 202
        Top = 132
        Width = 240
        Height = 17
        Caption = 'Internal SQL queries'
        TabOrder = 8
        OnClick = Modified
      end
      object chkLogEventInfo: TCheckBox
        Left = 202
        Top = 151
        Width = 240
        Height = 17
        Caption = 'Information messages'
        TabOrder = 9
        OnClick = Modified
      end
      object chkLogEventDebug: TCheckBox
        Left = 202
        Top = 170
        Width = 240
        Height = 17
        Caption = 'Debug messages'
        TabOrder = 10
        OnClick = Modified
      end
      object editLogDir: TButtonedEdit
        Left = 202
        Top = 62
        Width = 495
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
        Left = 8
        Top = 200
        Width = 176
        Height = 17
        Caption = 'Enable query history'
        TabOrder = 11
        OnClick = chkQueryHistoryClick
      end
      object chkHorizontalScrollbar: TCheckBox
        Left = 8
        Top = 223
        Width = 434
        Height = 17
        Caption = 'Horizontal scrollbar'
        TabOrder = 12
        OnClick = Modified
      end
      object editQueryHistoryKeepDays: TEdit
        Left = 202
        Top = 198
        Width = 53
        Height = 21
        Enabled = False
        TabOrder = 13
        Text = '1'
        OnChange = Modified
      end
      object updownQueryHistoryKeepDays: TUpDown
        Left = 255
        Top = 198
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
    object tabSQL: TTabSheet
      Caption = 'SQL'
      ImageIndex = 57
      DesignSize = (
        710
        349)
      object lblFont: TLabel
        Left = 8
        Top = 11
        Width = 55
        Height = 13
        Caption = 'Editor font:'
      end
      object lblSQLFontSize: TLabel
        Left = 689
        Top = 11
        Width = 12
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
        Top = 158
        Width = 42
        Height = 13
        Caption = 'Element:'
      end
      object lblSQLColBackground: TLabel
        Left = 489
        Top = 184
        Width = 60
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Background:'
      end
      object lblSQLColForeground: TLabel
        Left = 489
        Top = 158
        Width = 60
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Foreground:'
      end
      object comboSQLFontName: TComboBox
        Left = 191
        Top = 8
        Width = 423
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = SQLFontChange
      end
      object editSQLFontSize: TEdit
        Left = 623
        Top = 8
        Width = 41
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
        Left = 8
        Top = 90
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable autocompletion'
        TabOrder = 5
        OnClick = Modified
      end
      object chkTabsToSpaces: TCheckBox
        Left = 264
        Top = 36
        Width = 437
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Tabs to spaces'
        TabOrder = 6
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
        Left = 8
        Top = 113
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Prompt to save modified files on tab close'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = Modified
      end
      object editMaxQueryResults: TEdit
        Left = 191
        Top = 61
        Width = 41
        Height = 21
        TabOrder = 8
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
        TabOrder = 9
        OnChanging = anyUpDownLimitChanging
      end
      object comboSQLColElement: TComboBox
        Left = 67
        Top = 155
        Width = 407
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 10
        OnChange = comboSQLColElementChange
      end
      object chkSQLBold: TCheckBox
        Left = 67
        Top = 182
        Width = 130
        Height = 17
        Caption = 'Bold'
        TabOrder = 11
        OnClick = SQLFontChange
      end
      object chkSQLItalic: TCheckBox
        Left = 203
        Top = 182
        Width = 271
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Italic'
        TabOrder = 12
        OnClick = SQLFontChange
      end
      object cboxSQLColForeground: TColorBox
        Left = 583
        Top = 155
        Width = 118
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akTop, akRight]
        TabOrder = 13
        OnChange = SQLFontChange
      end
      object cboxSQLColBackground: TColorBox
        Left = 583
        Top = 181
        Width = 118
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akTop, akRight]
        TabOrder = 14
        OnChange = SQLFontChange
      end
      object SynMemoSQLSample: TSynMemo
        AlignWithMargins = True
        Left = 8
        Top = 208
        Width = 693
        Height = 133
        Cursor = crHandPoint
        SingleLineMode = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 15
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
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        RightEdge = 0
        TabWidth = 2
        WordWrap = True
        OnChange = SQLFontChange
        FontSmoothing = fsmNone
      end
    end
    object tabGridFormatting: TTabSheet
      Caption = 'Grid formatting'
      ImageIndex = 41
      DesignSize = (
        710
        349)
      object lblMaxColWidth: TLabel
        Left = 8
        Top = 11
        Width = 176
        Height = 13
        Caption = 'Maximum column width in data-grids:'
      end
      object lblDataFontHint: TLabel
        Left = 632
        Top = 87
        Width = 10
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'pt'
      end
      object lblDataFont: TLabel
        Left = 8
        Top = 87
        Width = 26
        Height = 13
        Caption = '&Font:'
        FocusControl = comboDataFontName
      end
      object lblMaxTotalRows: TLabel
        Left = 8
        Top = 36
        Width = 267
        Height = 13
        Caption = 'Number of rows displayed in data tab (Step, Maximum):'
      end
      object lblGridRowsLinecount: TLabel
        Left = 8
        Top = 61
        Width = 122
        Height = 13
        Caption = 'Lines of text in grid rows:'
      end
      object lblGridTextColors: TLabel
        Left = 8
        Top = 112
        Width = 77
        Height = 13
        Caption = 'Grid text colors:'
      end
      object lblNullBackground: TLabel
        Left = 8
        Top = 140
        Width = 226
        Height = 13
        Caption = 'NULL background color (use "None" to disable):'
      end
      object Label2: TLabel
        Left = 8
        Top = 168
        Width = 137
        Height = 13
        Caption = 'Alternating row background:'
      end
      object editMaxColWidth: TEdit
        Left = 430
        Top = 8
        Width = 42
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 0
        Text = '1'
        OnChange = Modified
      end
      object updownMaxColWidth: TUpDown
        Left = 472
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
        Left = 430
        Top = 83
        Width = 133
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 6
        OnChange = DataFontsChange
      end
      object editDataFontSize: TEdit
        Left = 569
        Top = 83
        Width = 42
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 7
        Text = '8'
        OnChange = DataFontsChange
      end
      object updownDataFontSize: TUpDown
        Left = 611
        Top = 83
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editDataFontSize
        Position = 8
        TabOrder = 8
      end
      object editGridRowCountMax: TEdit
        Left = 569
        Top = 33
        Width = 132
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 3
        OnChange = Modified
        OnExit = editGridRowCountExit
      end
      object editGridRowCountStep: TEdit
        Left = 430
        Top = 33
        Width = 133
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 2
        OnChange = Modified
        OnExit = editGridRowCountExit
      end
      object editGridRowsLineCount: TEdit
        Left = 430
        Top = 58
        Width = 42
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 4
        Text = '1'
        OnChange = Modified
      end
      object updownGridRowsLineCount: TUpDown
        Left = 472
        Top = 58
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
        Left = 430
        Top = 109
        Width = 133
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 9
        OnSelect = comboGridTextColorsSelect
      end
      object colorBoxGridTextColors: TColorBox
        Left = 569
        Top = 109
        Width = 132
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        TabOrder = 10
        OnSelect = colorBoxGridTextColorsSelect
      end
      object cboxNullBackground: TColorBox
        Left = 430
        Top = 137
        Width = 133
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        TabOrder = 11
        OnChange = Modified
        OnSelect = Modified
      end
      object cboxRowBackgroundOdd: TColorBox
        Left = 569
        Top = 165
        Width = 132
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        TabOrder = 13
        OnChange = Modified
      end
      object cboxRowBackgroundEven: TColorBox
        Left = 430
        Top = 165
        Width = 133
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        TabOrder = 12
        OnChange = Modified
      end
      object chkLocalNumberFormat: TCheckBox
        Left = 8
        Top = 193
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Local number format'
        TabOrder = 14
        OnClick = Modified
      end
      object chkHintsOnResultTabs: TCheckBox
        Left = 8
        Top = 216
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Pop up SQL text over result tabs'
        TabOrder = 15
        OnClick = Modified
      end
      object chkHightlightSameText: TCheckBox
        Left = 8
        Top = 239
        Width = 691
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Hightlight fields with same text as in focused field'
        TabOrder = 16
        OnClick = Modified
      end
    end
    object tabDataEditors: TTabSheet
      Caption = 'Data editors'
      ImageIndex = 33
      DesignSize = (
        710
        349)
      object lblLineBreakStyle: TLabel
        Left = 8
        Top = 188
        Width = 111
        Height = 13
        Caption = 'Default linebreak style:'
      end
      object chkEditorBinary: TCheckBox
        Left = 8
        Top = 8
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable popup BLOB/HEX editor'
        TabOrder = 0
        OnClick = Modified
      end
      object chkEditorDatetime: TCheckBox
        Left = 8
        Top = 31
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable inplace date/time editor'
        TabOrder = 1
        OnClick = Modified
      end
      object chkPrefillDateTime: TCheckBox
        Left = 24
        Top = 54
        Width = 677
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Prefill empty date/time fields with current date/time'
        TabOrder = 2
        OnClick = Modified
      end
      object chkEditorEnum: TCheckBox
        Left = 8
        Top = 77
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable ENUM pulldown editor'
        TabOrder = 3
        OnClick = Modified
      end
      object chkEditorSet: TCheckBox
        Left = 8
        Top = 100
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable SET checkbox editor'
        TabOrder = 4
        OnClick = Modified
      end
      object chkReuseEditorConfiguration: TCheckBox
        Left = 8
        Top = 139
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Remember filters, sorting and column selection across sessions'
        TabOrder = 5
      end
      object chkForeignDropDown: TCheckBox
        Left = 8
        Top = 162
        Width = 693
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show values in foreign key columns'
        TabOrder = 6
      end
      object comboLineBreakStyle: TComboBox
        Left = 314
        Top = 185
        Width = 271
        Height = 21
        Style = csDropDownList
        TabOrder = 7
      end
    end
    object tabShortcuts: TTabSheet
      Caption = 'Shortcuts'
      ImageIndex = 13
      DesignSize = (
        710
        349)
      object lblShortcut1: TLabel
        Left = 306
        Top = 64
        Width = 45
        Height = 13
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Shortcut:'
      end
      object lblShortcutHint: TLabel
        Left = 306
        Top = 3
        Width = 279
        Height = 55
        Anchors = [akLeft, akTop, akBottom]
        AutoSize = False
        Caption = 'Please select a shortcut item in the tree.'
        WordWrap = True
      end
      object lblShortcut2: TLabel
        Left = 306
        Top = 107
        Width = 98
        Height = 13
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Secondary shortcut:'
      end
      object TreeShortcutItems: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 300
        Height = 349
        Align = alLeft
        Header.AutoSizeIndex = 0
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
  object btnCancel: TButton
    Left = 571
    Top = 393
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
    Top = 393
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
    Top = 393
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
    Top = 393
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
end
