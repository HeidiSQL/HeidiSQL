object optionsform: Toptionsform
  Left = 547
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 369
  ClientWidth = 431
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
    431
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object pagecontrolMain: TPageControl
    Left = 8
    Top = 8
    Width = 417
    Height = 326
    ActivePage = tabMisc
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnChange = pagecontrolMainChange
    OnChanging = pagecontrolMainChanging
    object tabMisc: TTabSheet
      Caption = 'Miscellaneous'
      object Label4: TLabel
        Left = 16
        Top = 82
        Width = 37
        Height = 13
        Caption = 'Log last'
      end
      object lblLogLinesHint: TLabel
        Left = 240
        Top = 82
        Width = 71
        Height = 13
        Caption = 'lines in SQL log'
      end
      object lblLogSnip: TLabel
        Left = 16
        Top = 109
        Width = 96
        Height = 13
        Caption = 'Snip SQL log lines to'
      end
      object lblLogSnipHint: TLabel
        Left = 238
        Top = 109
        Width = 139
        Height = 13
        Caption = 'characters  (0 = no snipping)'
      end
      object chkAutoReconnect: TCheckBox
        Left = 16
        Top = 16
        Width = 334
        Height = 17
        Caption = 'Automatically reconnect to last session-account on startup'
        TabOrder = 0
        OnClick = Modified
      end
      object updownLogLines: TUpDown
        Left = 212
        Top = 79
        Width = 17
        Height = 21
        Associate = editLogLines
        Min = 1
        Max = 32767
        Position = 1
        TabOrder = 3
        Wrap = True
        OnChanging = anyUpDownLimitChanging
      end
      object editLogLines: TEdit
        Left = 159
        Top = 79
        Width = 53
        Height = 21
        TabOrder = 2
        Text = '1'
        OnChange = Modified
      end
      object chkRestoreLastDB: TCheckBox
        Left = 16
        Top = 40
        Width = 297
        Height = 17
        Caption = 'Restore last used database on startup'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = Modified
      end
      object chkLogToFile: TCheckBox
        Left = 16
        Top = 135
        Width = 141
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Write SQL log to file'
        TabOrder = 6
        OnClick = Modified
      end
      object btnOpenLogFolder: TButton
        Left = 159
        Top = 132
        Width = 106
        Height = 20
        Caption = 'Open log folder ...'
        TabOrder = 7
        OnClick = btnOpenLogFolderClick
      end
      object editLogSnip: TEdit
        Left = 159
        Top = 106
        Width = 53
        Height = 21
        TabOrder = 4
        Text = '2000'
        OnChange = Modified
      end
      object updownLogSnip: TUpDown
        Left = 212
        Top = 106
        Width = 17
        Height = 21
        Associate = editLogSnip
        Max = 32767
        Position = 2000
        TabOrder = 5
        OnChanging = anyUpDownLimitChanging
      end
      object chkUpdatecheck: TCheckBox
        Left = 16
        Top = 176
        Width = 281
        Height = 28
        Caption = 
          'Automatically check for updates / Interval [days]:'#13#10'(0 = each ti' +
          'me the application is started)'
        TabOrder = 8
        WordWrap = True
        OnClick = chkUpdatecheckClick
      end
      object editUpdatecheckInterval: TEdit
        Left = 316
        Top = 176
        Width = 43
        Height = 21
        Enabled = False
        TabOrder = 9
        Text = '1'
        OnChange = Modified
      end
      object updownUpdatecheckInterval: TUpDown
        Left = 359
        Top = 176
        Width = 16
        Height = 21
        Associate = editUpdatecheckInterval
        Enabled = False
        Max = 999
        Position = 1
        TabOrder = 10
        OnChanging = anyUpDownLimitChanging
      end
      object chkUpdateCheckBuilds: TCheckBox
        Left = 34
        Top = 210
        Width = 247
        Height = 17
        Caption = 'Also check for updated nightly builds'
        Enabled = False
        TabOrder = 11
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
        Width = 397
        Height = 56
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Editor font'
        TabOrder = 0
        object lblSQLFontSize: TLabel
          Left = 248
          Top = 26
          Width = 23
          Height = 13
          Caption = 'Size:'
        end
        object comboSQLFontName: TComboBox
          Left = 16
          Top = 23
          Width = 193
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = SQLFontChange
        end
        object editSQLFontSize: TEdit
          Left = 294
          Top = 23
          Width = 41
          Height = 21
          TabOrder = 1
          Text = '9'
          OnExit = SQLFontChange
        end
        object updownSQLFontSize: TUpDown
          Left = 335
          Top = 23
          Width = 16
          Height = 21
          Associate = editSQLFontSize
          Position = 9
          TabOrder = 2
          OnClick = updownSQLFontSizeClick
        end
      end
      object grpSQLColors: TGroupBox
        Left = 1
        Top = 61
        Width = 397
        Height = 68
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Syntax'
        TabOrder = 1
        object lblSQLColElement: TLabel
          Left = 19
          Top = 18
          Width = 42
          Height = 13
          Caption = 'Element:'
        end
        object lblSQLColColor: TLabel
          Left = 188
          Top = 18
          Width = 29
          Height = 13
          Caption = 'Color:'
        end
        object comboSQLColElement: TComboBox
          Left = 16
          Top = 33
          Width = 161
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = comboSQLColElementChange
        end
        object cboxSQLColColor: TColorBox
          Left = 188
          Top = 33
          Width = 124
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
          ItemHeight = 16
          TabOrder = 1
          OnChange = SQLFontChange
        end
        object chkSQLBold: TCheckBox
          Left = 322
          Top = 26
          Width = 72
          Height = 17
          Caption = 'Bold'
          TabOrder = 2
          OnClick = SQLFontChange
        end
        object chkSQLItalic: TCheckBox
          Left = 322
          Top = 45
          Width = 65
          Height = 17
          Caption = 'Italic'
          TabOrder = 3
          OnClick = SQLFontChange
        end
      end
      object grpSQLSample: TGroupBox
        Left = 0
        Top = 132
        Width = 397
        Height = 153
        Caption = 'Sample'
        TabOrder = 2
        object SynMemoSQLSample: TSynMemo
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 387
          Height = 130
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
          Lines.UnicodeStrings = 'SynMemoSQLSample'
          Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          ReadOnly = True
          RightEdge = 0
          TabWidth = 2
          WordWrap = True
          OnChange = SQLFontChange
        end
      end
    end
    object tabData: TTabSheet
      BorderWidth = 5
      Caption = 'Data'
      ImageIndex = 4
      object lblMaxColWidth: TLabel
        Left = 4
        Top = 12
        Width = 177
        Height = 13
        Caption = 'Maximum column-width in data-grids:'
      end
      object lblDataFontHint: TLabel
        Left = 340
        Top = 40
        Width = 29
        Height = 13
        Caption = 'points'
      end
      object lblDataFont: TLabel
        Left = 4
        Top = 40
        Width = 26
        Height = 13
        Caption = '&Font:'
        FocusControl = comboDataFontName
      end
      object editMaxColWidth: TEdit
        Left = 277
        Top = 9
        Width = 42
        Height = 21
        TabOrder = 0
        Text = '1'
        OnChange = Modified
      end
      object updownMaxColWidth: TUpDown
        Left = 319
        Top = 9
        Width = 16
        Height = 21
        Associate = editMaxColWidth
        Min = 1
        Max = 1000
        Position = 1
        TabOrder = 1
        OnChanging = anyUpDownLimitChanging
      end
      object comboDataFontName: TComboBox
        Left = 60
        Top = 36
        Width = 193
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = DataFontsChange
      end
      object editDataFontSize: TEdit
        Left = 277
        Top = 36
        Width = 42
        Height = 21
        TabOrder = 3
        Text = '8'
        OnChange = DataFontsChange
      end
      object updownDataFontSize: TUpDown
        Left = 319
        Top = 36
        Width = 16
        Height = 21
        Associate = editDataFontSize
        Position = 8
        TabOrder = 4
      end
      object grpFieldLayout: TGroupBox
        Left = 4
        Top = 72
        Width = 391
        Height = 209
        Anchors = [akLeft, akTop, akBottom]
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
          ItemHeight = 16
          TabOrder = 1
          OnChange = Modified
        end
        object cboxBinary: TColorBox
          Left = 100
          Top = 69
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          ItemHeight = 16
          TabOrder = 2
          OnChange = Modified
        end
        object cboxDatetime: TColorBox
          Left = 100
          Top = 93
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          ItemHeight = 16
          TabOrder = 4
          OnChange = Modified
        end
        object cboxNumeric: TColorBox
          Left = 100
          Top = 21
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          ItemHeight = 16
          TabOrder = 0
          OnChange = Modified
        end
        object cboxEnum: TColorBox
          Left = 100
          Top = 117
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          ItemHeight = 16
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
          ItemHeight = 16
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
          ItemHeight = 16
          TabOrder = 10
          OnChange = Modified
        end
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
        Width = 399
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
        Left = 314
        Top = 197
        Width = 65
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 1
        Text = '0'
        OnChange = Modified
      end
      object updownCopyDataMaxSize: TUpDown
        Left = 377
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
    end
  end
  object btnCancel: TButton
    Left = 270
    Top = 338
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 190
    Top = 338
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
    Left = 350
    Top = 338
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    TabOrder = 2
    OnClick = Apply
  end
  object SynSQLSynSQLSample: TSynSQLSyn
    SQLDialect = sqlMySQL
    Left = 8
    Top = 336
  end
end
