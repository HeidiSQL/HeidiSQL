object MainForm: TMainForm
  Left = 241
  Top = 114
  ClientHeight = 366
  ClientWidth = 632
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = True
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 347
    Width = 632
    Height = 19
    AutoHint = True
    Panels = <
      item
        Width = 170
      end
      item
        Width = 130
      end
      item
        Style = psOwnerDraw
        Width = 280
      end>
    OnDrawPanel = StatusBarDrawPanel
    ExplicitTop = 327
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 632
    Height = 52
    Align = alTop
    AutoSize = True
    BevelKind = bkNone
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object ToolBarStandard: TToolBar
      Left = 11
      Top = 2
      Width = 421
      Height = 22
      Align = alNone
      AutoSize = True
      Caption = 'Main'
      DragKind = dkDock
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = ImageList1
      TabOrder = 0
      Wrapable = False
      object ToolButton9: TToolButton
        Left = 0
        Top = 0
        Action = FileNew1
        AutoSize = True
        DropdownMenu = menuConnections
        Style = tbsDropDown
      end
      object FileCloseItem2: TToolButton
        Left = 36
        Top = 0
        Action = FileClose1
        AutoSize = True
      end
      object ToolButton3: TToolButton
        Left = 59
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ToolButton5: TToolButton
        Left = 67
        Top = 0
        Action = EditCopy1
        AutoSize = True
      end
      object ToolButton6: TToolButton
        Left = 90
        Top = 0
        Action = EditPaste1
        AutoSize = True
      end
      object ToolButton14: TToolButton
        Left = 113
        Top = 0
        Hint = 'Undo'
        Action = EditUndo1
      end
      object ToolButton12: TToolButton
        Left = 136
        Top = 0
        Action = PrintList
      end
      object ToolButton7: TToolButton
        Left = 159
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object ButtonCreateDatabase: TToolButton
        Left = 167
        Top = 0
        Hint = 'Create Database'
        AutoSize = True
        Caption = 'Create Database'
        Enabled = False
        ImageIndex = 73
        OnClick = ButtonCreateDatabaseClick
      end
      object ButtonDropDatabase: TToolButton
        Left = 190
        Top = 0
        Hint = 'Drop Database...'
        AutoSize = True
        Caption = 'Drop Database'
        Enabled = False
        ImageIndex = 22
        OnClick = ButtonDropDatabaseClick
      end
      object ButtonCreateTable: TToolButton
        Left = 213
        Top = 0
        Hint = 'Create Table'
        AutoSize = True
        Caption = 'Create Table'
        Enabled = False
        ImageIndex = 72
        OnClick = ButtonCreateTableClick
      end
      object ButtonDropTable: TToolButton
        Left = 236
        Top = 0
        Action = DropTable
        AutoSize = True
      end
      object ToolButton4: TToolButton
        Left = 259
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        ImageIndex = 19
        Style = tbsSeparator
      end
      object ButtonRefresh: TToolButton
        Left = 267
        Top = 0
        Hint = 'Refresh'
        AutoSize = True
        Caption = 'Refresh'
        Enabled = False
        ImageIndex = 18
        OnClick = ButtonRefreshClick
      end
      object ButtonReload: TToolButton
        Tag = 2
        Left = 290
        Top = 0
        Hint = 'Reload User-Privileges'
        AutoSize = True
        Caption = 'Reload'
        Enabled = False
        ImageIndex = 16
        OnClick = FlushClick
      end
      object ToolButton13: TToolButton
        Left = 313
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        ImageIndex = 19
        Style = tbsSeparator
      end
      object ButtonUserManager: TToolButton
        Left = 321
        Top = 0
        Action = UserManager
        AutoSize = True
      end
      object ButtonImportTextfile: TToolButton
        Left = 344
        Top = 0
        Hint = 'Import CSV file'
        AutoSize = True
        Caption = 'Import CSV file'
        Enabled = False
        ImageIndex = 24
        OnClick = ButtonImportTextfileClick
      end
      object ButtonExport: TToolButton
        Left = 367
        Top = 0
        Hint = 'Export tables'
        Action = ExportTables
        AutoSize = True
      end
      object ToolButton1: TToolButton
        Left = 390
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 97
        Style = tbsSeparator
      end
      object btnSQLHelp: TToolButton
        Left = 398
        Top = 0
        Hint = 'SQL Help'
        Caption = 'btnSQLHelp'
        Enabled = False
        ImageIndex = 96
        OnClick = btnSQLHelpClick
      end
    end
    object ToolBarData: TToolBar
      Left = 11
      Top = 28
      Width = 489
      Height = 22
      Align = alNone
      AutoSize = True
      Caption = 'Data'
      DragKind = dkDock
      EdgeInner = esNone
      EdgeOuter = esNone
      TabOrder = 1
      Visible = False
      Wrapable = False
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 0
        Width = 230
        Height = 22
        Flat = True
        ConfirmDelete = False
        TabOrder = 0
        BeforeAction = DBNavigator1BeforeAction
      end
      object PanelLimit: TPanel
        Left = 230
        Top = 0
        Width = 225
        Height = 22
        BevelOuter = bvNone
        TabOrder = 1
        object CheckBoxLimit: TCheckBox
          Left = 7
          Top = 3
          Width = 50
          Height = 17
          Caption = 'Limit:'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CheckBoxLimitClick
          OnEnter = LimitPanelEnter
          OnExit = LimitPanelExit
        end
        object EditLimitStart: TEdit
          Left = 56
          Top = 0
          Width = 57
          Height = 21
          AutoSelect = False
          TabOrder = 1
          Text = '0'
          OnEnter = LimitPanelEnter
          OnExit = LimitPanelExit
        end
        object EditLimitEnd: TEdit
          Left = 146
          Top = 0
          Width = 57
          Height = 21
          AutoSelect = False
          TabOrder = 2
          Text = '50'
          OnEnter = LimitPanelEnter
          OnExit = LimitPanelExit
        end
        object UpDownLimitStart: TUpDown
          Left = 113
          Top = 0
          Width = 17
          Height = 21
          Associate = EditLimitStart
          Max = 32767
          TabOrder = 3
          Thousands = False
          Wrap = True
          OnEnter = LimitPanelEnter
          OnExit = LimitPanelExit
        end
        object UpDownLimitEnd: TUpDown
          Left = 203
          Top = 0
          Width = 17
          Height = 21
          Associate = EditLimitEnd
          Max = 32767
          Position = 50
          TabOrder = 4
          Thousands = False
          Wrap = True
          OnEnter = LimitPanelEnter
          OnExit = LimitPanelExit
        end
      end
      object ButtonOK: TButton
        Left = 455
        Top = 0
        Width = 34
        Height = 22
        Caption = 'OK'
        TabOrder = 2
        OnClick = ButtonOKClick
        OnEnter = LimitPanelEnter
        OnExit = LimitPanelExit
      end
    end
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 8
    Top = 88
    object File1: TMenuItem
      Tag = 17
      Caption = '&File'
      Hint = 'File related commands'
      object FileNewItem: TMenuItem
        Tag = 23
        Action = FileNew1
        Caption = '&Connection...'
        Hint = 'Connection...|Establish connection with MySQL-Host'
      end
      object FileCloseItem: TMenuItem
        Tag = 24
        Action = FileClose1
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ResetWindowOptions1: TMenuItem
        Tag = 25
        Caption = 'Reset Window-Options'
        OnClick = ResetWindowOptions1Click
      end
      object ExportSettings1: TMenuItem
        Caption = 'Export settings...'
        OnClick = ExportSettings1Click
      end
      object Importsettings1: TMenuItem
        Caption = 'Import settings...'
        OnClick = Importsettings1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Tag = 26
        Action = FileExit1
        ShortCut = 32883
      end
    end
    object Edit1: TMenuItem
      Tag = 18
      Caption = '&Edit'
      Hint = 'Edit commands'
      object CopyItem: TMenuItem
        Action = EditCopy1
      end
      object PasteItem: TMenuItem
        Action = EditPaste1
      end
    end
    object Extra1: TMenuItem
      Tag = 19
      Caption = 'Tools'
      object MenuRefresh: TMenuItem
        Tag = 28
        Caption = 'Refresh'
        Enabled = False
        ImageIndex = 18
        OnClick = ButtonRefreshClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuCreateDatabase: TMenuItem
        Tag = 29
        Caption = 'Create Database...'
        Enabled = False
        ImageIndex = 73
        OnClick = ButtonCreateDatabaseClick
      end
      object MenuCreateTable: TMenuItem
        Tag = 30
        Caption = 'Create Table...'
        Enabled = False
        ImageIndex = 20
        OnClick = ButtonCreateTableClick
      end
      object MenuDropDatabase: TMenuItem
        Tag = 31
        Caption = 'Drop Database...'
        Enabled = False
        ImageIndex = 22
        OnClick = ButtonDropDatabaseClick
      end
      object MenuDropTable: TMenuItem
        Tag = 32
        Action = DropTable
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Flush1: TMenuItem
        Tag = 33
        Caption = 'Flush'
        object MenuFlushHosts: TMenuItem
          Tag = 37
          Caption = 'Hosts'
          Enabled = False
          Hint = 'Flush Privileges|Flush Hosts'
          OnClick = FlushClick
        end
        object MenuFlushLogs: TMenuItem
          Tag = 38
          Caption = 'Logs'
          Enabled = False
          Hint = 'Flush Logs'
          OnClick = FlushClick
        end
        object FlushUserPrivileges1: TMenuItem
          Tag = 39
          Caption = 'Privileges'
          Enabled = False
          Hint = 'Flush Privileges|Flush User-Privileges'
          ImageIndex = 16
          OnClick = FlushClick
        end
        object MenuFlushTables: TMenuItem
          Tag = 40
          Caption = 'Tables'
          Enabled = False
          Hint = 'Flush Privileges|Flush Tables'
          OnClick = FlushClick
        end
        object MenuFlushTableswithreadlock: TMenuItem
          Tag = 41
          Caption = 'Tables with read lock'
          Enabled = False
          Hint = 'Flush Privileges|Flush Tables with read lock'
          OnClick = FlushClick
        end
        object MenuFlushStatus: TMenuItem
          Tag = 42
          Caption = 'Status'
          Enabled = False
          Hint = 'Flush Status'
          OnClick = FlushClick
        end
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MenuUserManager: TMenuItem
        Action = UserManager
      end
      object OptimizeTables1: TMenuItem
        Action = Diagnostics
        Caption = 'Maintenance'
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object MenuPreferences: TMenuItem
        Tag = 36
        Caption = 'Preferences'
        OnClick = MenuPreferencesClick
      end
    end
    object Import1: TMenuItem
      Caption = 'Import'
      object MenuImportTextFile: TMenuItem
        Tag = 43
        Caption = 'Import CSV file...'
        Enabled = False
        ImageIndex = 24
        OnClick = ButtonImportTextfileClick
      end
      object ImportSQL1: TMenuItem
        Action = LoadSQL
      end
      object InsertfilesintoBLOBfields1: TMenuItem
        Action = InsertFiles
      end
    end
    object ImExport1: TMenuItem
      Tag = 20
      Caption = 'Export'
      object MenuExport: TMenuItem
        Tag = 45
        Action = ExportTables
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object MenuCopyCSV: TMenuItem
        Tag = 48
        Action = Copy2CSV
      end
      object CopyContentsasHTMLTable1: TMenuItem
        Tag = 49
        Action = CopyHTMLtable
      end
      object CopyasXMLdata1: TMenuItem
        Action = Copy2XML
      end
      object Exportdata1: TMenuItem
        Action = ExportData
      end
    end
    object menuWindow: TMenuItem
      Caption = '&Window'
      OnClick = menuWindowClick
      object miFake: TMenuItem
        Caption = 'fake item - see notes in menuWindowClick()'
      end
    end
    object Help1: TMenuItem
      Tag = 22
      Caption = '&Help'
      Hint = 'Help topics'
      object menuSQLHelp: TMenuItem
        Caption = 'SQL Help'
        Enabled = False
        ShortCut = 112
        OnClick = btnSQLHelpClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object menuWebsite: TMenuItem
        Tag = 56
        Caption = 'HeidiSQL Website'
        Hint = 'http://www.heidisql.com/'
        OnClick = OpenURL
      end
      object menuDownload: TMenuItem
        Caption = 'Download-Page'
        Hint = 'http://download.heidisql.com/'
        OnClick = OpenURL
      end
      object menuSupportForum: TMenuItem
        Caption = 'Support-Forum'
        Hint = 'http://www.heidisql.com/forum/'
        OnClick = OpenURL
      end
      object menuBugtracker: TMenuItem
        Caption = 'Bugtracker'
        Hint = 'http://bugs.heidisql.com/'
        OnClick = OpenURL
      end
      object menuFeaturetracker: TMenuItem
        Caption = 'Featuretracker'
        Hint = 'http://rfe.heidisql.com/'
        OnClick = OpenURL
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object menuReadme: TMenuItem
        Tag = 55
        Caption = 'Readme'
        OnClick = menuReadmeClick
      end
      object menuAbout: TMenuItem
        Action = ShowAboutBox
        Hint = 'About HeidiSQL'
      end
    end
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 72
    Top = 120
    object FileNew1: TAction
      Category = 'File'
      Caption = '&Connection'
      Hint = 'Connection...|Establish Connection with MySQL-Host'
      ImageIndex = 93
      ShortCut = 16462
      OnExecute = ShowConnections
    end
    object FileClose1: TWindowClose
      Category = 'File'
      Caption = '&Close'
      Hint = 'Close|Close Connection'
      ImageIndex = 90
    end
    object FileExit1: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Exit application'
      OnExecute = FileExit1Execute
    end
    object EditCopy1: TEditCopy
      Tag = 27
      Category = 'Edit'
      Caption = '&Copy'
      Enabled = False
      Hint = 'Copy|Copy to Clipboard'
      ImageIndex = 1
      ShortCut = 16451
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
    end
    object EditPaste1: TEditPaste
      Tag = 58
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Paste from Clipboard'
      ImageIndex = 2
      ShortCut = 16470
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
    end
    object UserManager: TAction
      Tag = 34
      Caption = 'User-Manager'
      Enabled = False
      Hint = 'User-Manager'
      ImageIndex = 25
      OnExecute = UserManagerExecute
    end
    object ShowAboutBox: TAction
      Tag = 57
      Caption = 'About...'
      Hint = 'About MySQL-Front'
      OnExecute = ShowAboutBoxExecute
    end
    object Diagnostics: TAction
      Tag = 35
      Caption = 'Table-Diagnostics'
      Enabled = False
      Hint = 'Optimize, repair and analyse tables'
      ImageIndex = 27
      OnExecute = DiagnosticsExecute
    end
    object CopyHTMLtable: TAction
      Tag = 49
      Caption = 'Copy as HTML-table'
      Enabled = False
      Hint = 'Copy data as HTML-table'
      ImageIndex = 28
      OnExecute = CopyHTMLtableExecute
    end
    object Copy2CSV: TAction
      Tag = 48
      Caption = 'Copy as CSV-data'
      Enabled = False
      Hint = 'Copy data as CSV-data'
      ImageIndex = 17
      OnExecute = Copy2CSVExecute
    end
    object Copy2XML: TAction
      Caption = 'Copy as XML-data'
      Enabled = False
      Hint = 'Copy data as XML-data'
      ImageIndex = 48
      OnExecute = Copy2XMLExecute
    end
    object ExportData: TAction
      Caption = 'Export data ...'
      Enabled = False
      Hint = 'Save table-data to file ...'
      ImageIndex = 49
      OnExecute = ExportDataExecute
    end
    object PrintList: TAction
      Tag = 61
      Caption = 'Print...'
      Enabled = False
      Hint = 'Print List or Data'
      ImageIndex = 29
      ShortCut = 16464
      OnExecute = PrintListExecute
    end
    object CopyTable: TAction
      Tag = 62
      Caption = 'Copy Table...'
      Enabled = False
      Hint = 'Copy table...|Create a copy of this table'
      ImageIndex = 30
      OnExecute = CopyTableExecute
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Enabled = False
      ImageIndex = 3
      ShortCut = 32776
    end
    object ExecuteQuery: TAction
      Caption = 'Run'
      Enabled = False
      Hint = 'Execute SQL...|Execute SQL-query/queries...'
      ImageIndex = 44
      ShortCut = 120
      OnExecute = ExecuteQueryExecute
    end
    object ExecuteSelection: TAction
      Caption = 'Run Selection'
      Enabled = False
      Hint = 'Execute selected SQL...|Execute selected SQL-query/queries...'
      ImageIndex = 45
      ShortCut = 16504
      OnExecute = ExecuteSelectionExecute
    end
    object ExecuteLine: TAction
      Caption = 'Run current line'
      Hint = 'Execute Line|Executes the current line of SQL'
      ImageIndex = 45
      ShortCut = 24696
      OnExecute = ExecuteLineExecute
    end
    object HTMLview: TAction
      Caption = 'HTML-view'
      Hint = 'View contents as HTML-file'
      ImageIndex = 53
      OnExecute = HTMLviewExecute
    end
    object InsertFiles: TAction
      Caption = 'Insert files into BLOB-fields...'
      Enabled = False
      ImageIndex = 55
      OnExecute = InsertFilesExecute
    end
    object ExportTables: TAction
      Caption = 'Export tables as SQL'
      Enabled = False
      ImageIndex = 19
      OnExecute = ExportTablesExecute
    end
    object LoadSQL: TAction
      Caption = 'Load SQL-file ...'
      Enabled = False
      Hint = 'Load SQL-file ...'
      ImageIndex = 85
      OnExecute = LoadSQLExecute
    end
    object DataSearch: TAction
      Caption = 'Find...'
      ImageIndex = 50
      ShortCut = 16454
      OnExecute = DataSearchExecute
    end
    object DropTable: TAction
      Caption = 'Drop Table ...'
      Enabled = False
      Hint = 'Drop Table ...'
      ImageIndex = 84
      OnExecute = DropTableExecute
    end
  end
  object ImageList1: TImageList
    ShareImages = True
    Left = 8
    Top = 120
    Bitmap = {
      494C010163006800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000A0010000010020000000000000A0
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099009900000000009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009900990099009900990099009900990099009900990099009900
      990099009900990099009900990000000000000000000000000099999900CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000990099009900990000000000FFFFFF00CCCCCC00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099009900FFFFFF00000000009999990099999900CCCCCC00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00990099000000000000000000000000009999990066FF
      FF0066FFFF0066FFFF0066FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009900990099009900990099000000000099999900FFFFFF00CCCCCC00FFFF
      FF00CCCCCC000000000000000000000000000000000000000000000000009900
      990099009900FFFFFF0000000000CCCCCC00FFFFFF009999990099999900CCCC
      CC00FFFFFF00FFFFFF00990099000000000000000000000000009999990099FF
      FF0066FFFF0066FFFF0066FFFF0066FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009900
      990099009900990099009900990099999900000000000000000099999900CCCC
      CC00FFFFFF00CCCCCC0000000000000000000000000000000000990099009900
      9900FFFFFF00FFFFFF0000000000CCCCCC00FFFFFF00FFFFFF00FFFFFF009999
      9900CCCCCC00FFFFFF00990099000000000000000000000000009999990099FF
      FF0099FFFF00FF000000FF000000FF000000FF000000FF00000000FFFF0000FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000990099009900
      9900990099009900990099999900CC00CC00CC00CC00CC00CC00000000000000
      000099999900FFFFFF009999990000000000000000000000000099009900FFFF
      FF00FFFFFF00FFFFFF0000000000CCCCCC00FFFFFF0000FFFF00FFFFFF00FFFF
      FF0099999900FFFFFF00990099000000000000000000000000009999990099FF
      FF0099FFFF0099FFFF0099FFFF0066FFFF0066FFFF0066FFFF0066FFFF0000FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000033CCFF0033CCFF0033CCFF0033CC
      FF009900990099999900CC00CC0033CCFF0033CCFF0033CCFF0033CCFF00CC00
      CC0033CCFF0033CCFF0033CCFF0033CCFF00000000000000000099009900FFFF
      FF00FFFFFF00FFFFFF0000000000CCCCCC00FFFFFF00FFFFFF00FFFFFF0000FF
      FF0099999900FFFFFF00990099000000000000000000000000009999990099FF
      FF0099FFFF00FF000000FF000000FF000000FF000000FF00000066FFFF0066FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000990099009900990033CC
      FF0033CCFF00CC00CC0033CCFF0033CCFF0033CCFF0033CCFF00CC00CC00CC00
      CC0033CCFF0033CCFF000000000000000000000000000000000099009900FFFF
      FF00FFFFFF00FFFFFF0000000000CCCCCC00FFFFFF0000FFFF00FFFFFF00FFFF
      FF0099999900FFFFFF00990099000000000000000000000000009999990099FF
      FF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0066FFFF0066FFFF0066FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033CCFF0033CCFF0033CC
      FF00CC00CC00CC00CC0033CCFF0033CCFF00CC00CC0033CCFF0033CCFF00CC00
      CC0033CCFF0033CCFF000000000000000000000000000000000099009900FFFF
      FF00FFFFFF00FFFFFF0000000000CCCCCC00FFFFFF00FFFFFF00FFFFFF0000FF
      FF0099999900FFFFFF009900990000000000000000000000000099999900CCFF
      FF0099FFFF00FF000000FF000000FF000000FF000000FF00000099FFFF0066FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000033CCFF0033CCFF0099999900CC00
      CC00CC00CC00CC00CC0033CCFF0033CCFF00CC00CC0033CCFF0033CCFF00CC00
      CC0033CCFF0033CCFF000000000000000000000000000000000099009900FFFF
      FF00FFFFFF00FFFFFF0000000000CCCCCC00FFFFFF0000FFFF00FFFFFF00FFFF
      FF0099999900FFFFFF009900990000000000000000000000000099999900CCFF
      FF00CCFFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033CCFF0033CCFF0033CC
      FF0033CCFF00CC00CC00CC00CC0033CCFF0033CCFF0033CCFF00CC00CC00CC00
      CC0033CCFF0033CCFF000000000000000000000000000000000099009900FFFF
      FF00FFFFFF00CCCCCC009999990099999900FFFFFF00FFFFFF00FFFFFF0000FF
      FF0099999900000000000000000000000000000000000000000099999900CCFF
      FF00CCFFFF00FF000000FF000000FF000000FF00000099FFFF0099FFFF0099FF
      FF00CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CC00
      CC00CC00CC00CC00CC00CC00CC00CC00CC00CC00CC00CC00CC00CC00CC000000
      000000000000000000000000000000000000000000000000000099009900FFFF
      FF00FFFFFF009999990000000000000000009999990099999900FFFFFF00FFFF
      FF0099999900000000000000000000000000000000000000000099999900CCFF
      FF00CCFFFF00CCFFFF00CCFFFF00CCFFFF0099FFFF0099FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CC00CC00CC00CC00CC00CC00CC00CC00CC00CC00000000000000
      000000000000000000000000000000000000000000000000000099009900FFFF
      FF00CCCCCC009999990000000000000000000000000000000000999999009999
      990099999900000000000000000000000000000000000000000099999900CCFF
      FF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF0099FFFF00CCCCCC00FFFF
      FF00999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CC00CC00CC00CC0000000000000000000000
      000000000000000000000000000000000000000000000000000099999900CCCC
      CC00999999000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000099999900CCFF
      FF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCCCCC009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999009999
      9900999999009999990099999900999999009999990099999900999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000333333000000000000000000000000000000
      0000333333000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033333300333333003333330000000000000000003333
      3300333333003333330000000000000000000000000000000000000000000000
      0000CC333300CC333300CC333300CC333300CC333300CC333300CC333300CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003333330033333300333333003333330033333300333333003333
      3300333333003333330033333300000000000000000000000000000000000000
      0000CC333300CC999900CC999900CC999900CC999900CC999900CC999900CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033333300333333003333
      3300333333003333330033333300333333003333330033333300333333003333
      3300333333003333330000000000000000000000000000000000000000000000
      0000CC333300CC999900CC999900CC999900CC999900CC999900CC999900CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033333300999999009999
      9900999999009999990099999900333333003333330033333300333333003333
      3300333333000000000000000000000000000000000000000000000000000000
      0000CC333300CC333300CC999900CC999900CC999900CC999900CC333300CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033333300999999009999
      9900999999009999990099999900333333003333330033333300333333003333
      3300333333000000000000000000000000000000000000000000000000000000
      000000000000CC333300CC333300CC333300CC333300CC333300CC3333000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033333300333333009999
      9900999999009999990033333300333333003333330033333300333333003333
      3300333333003333330000000000000000000000000000000000000000000000
      0000CC999900FFFF9900FFFF9900FFFF9900FFCC9900FFCC9900FFCC6600CC33
      3300000000000000000000000000000000000000000000000000000000006666
      6600666666006666660066666600666666006666660066666600666666006666
      6600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000333333003333
      3300333333003333330033333300333333003333330033333300333333003333
      3300333333003333330033333300000000000000000000000000000000000000
      0000CC999900FFFF9900FFFFCC00FFFF9900FFCC9900FFCC9900FFCC6600CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000666666006666660066666600666666006666660066666600666666000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000066666600666666006666660000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00CCCCCC0033333300333333003333330000000000000000003333
      3300333333003333330000000000000000000000000000000000000000000000
      00003366FF00FFFF9900FFFF9900FFFF9900FFCC9900FFCC9900FFCC6600CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000006666660066666600666666006666660066666600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006666660066666600666666006666660066666600000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00CCCCCC00CCCCCC00333333003333330000000000000000000000
      00003333330000000000000000000000000000000000000000003366FF006666
      CC003366FF006666CC003366CC00FFCC9900FFCC9900FFCC9900FFCC6600CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000066666600666666006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000666666006666660066666600666666006666660066666600666666000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      000000000000000000000000000000000000000000000000000099CCFF000099
      FF0000CCFF000099FF0099999900FFCC9900FFCC9900FFCC6600FFCC6600CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006666
      6600666666006666660066666600666666006666660066666600666666006666
      6600000000000000000000000000000000000000000099999900CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      000000000000000000000000000000000000000000003366FF003366FF0000CC
      FF0066CCFF0000CCFF003366CC003366FF00FFCC6600FFCC6600FFCC6600CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      00000000000000000000000000000000000000000000000000009999CC000066
      FF0000CCFF000066FF006666CC00CC999900CC999900CC999900CC999900CC33
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      00000000000000000000000000000000000000000000000000003366FF009999
      CC003366FF009999CC003333FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999003333330000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006666FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000009900000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000666666000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000099
      990000000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000990000009900000099000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000066666600666666006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000099
      990000000000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00000000000000CC333300CC333300CC33
      3300CC3333000099000000990000009900000099000000000000000000000000
      00000000000000000000000000000000000000000000CC333300CC333300CC33
      3300CC333300CC3333000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000033333300333333003333
      3300333333006666660066666600666666006666660000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000099
      990000000000FF000000FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FFFFFF00FFFFFF00FFFFFF00FF00000000000000CC333300CC999900CC99
      9900009900000099000000990000CC9999000099000000990000000000000000
      00000000000000000000000000000000000000000000CC333300CC999900CC99
      9900CC999900CC999900CC9999000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000033333300999999009999
      9900666666006666660066666600999999006666660066666600000000000000
      0000000000000000000000000000000000000000000000000000009999000099
      990000000000FF000000FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FF00
      0000FF000000FFFFFF00FFFFFF00FF00000000000000CC333300CC9999000099
      00000099000000990000CC999900CC999900CC33330000990000009900000000
      00000000000000000000000000000000000000000000CC333300CC999900CC99
      9900CC999900CC999900CC9999000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000033333300999999006666
      6600666666006666660099999900999999003333330066666600666666000000
      0000000000000000000000000000000000000000000000000000009999000099
      990000999900FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FF00
      0000FFFFFF00FFFFFF00FFFFFF00FF00000000000000CC333300CC333300CC99
      990000990000CC999900CC999900CC333300CC33330000000000009900000000
      00000000000000000000000000000000000000000000CC333300CC333300CC99
      9900CC999900CC9999000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000033333300333333009999
      9900666666009999990099999900333333003333330000000000666666000000
      0000000000000000000000000000000000000000000000000000009999000099
      990000000000FF000000FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF0000000000000000000000CC333300CC33
      3300CC333300CC333300CC333300CC3333000000000000000000000000000099
      0000000000000000000000000000000000000000000000000000CC333300CC33
      3300CC3333000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000333333003333
      3300333333003333330033333300333333000000000000000000000000006666
      6600000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00FF000000FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FF00
      0000FF000000FFFFFF00FFFFFF00FF00000000000000CC999900FFFF9900FFFF
      9900FFFF9900FFCC9900FFCC9900FFCC6600CC33330000000000000000000000
      00000099000000000000000000000000000000000000CC999900FFFF9900FFFF
      9900FFFF9900FFCC99000000FF000000FF000000FF0000000000000000000000
      FF000000FF000000FF0000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      0000666666000000000000000000000000000000000000000000009999000000
      0000CCCCCC00FF000000FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FFFFFF00FFFFFF00FFFFFF00FF00000000000000CC999900FFFF9900FFFF
      CC00FFFF9900FFCC9900FFCC9900FFCC6600CC33330000000000000000000000
      00000000000000990000000000000000000000000000CC999900FFFF9900FFFF
      CC00FFFF9900FFCC9900FFCC99000000FF00CC33330000000000000000000000
      00000000FF000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      0000000000006666660000000000000000000000000000000000009999000000
      0000CCCCCC00FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00000000000000CC999900FFFF9900FFFF
      9900FFFF9900FFCC9900FFCC9900FFCC6600CC33330000000000000000000000
      00000000000000000000009900000000000000000000CC999900FFFF9900FFFF
      9900FFFF9900FFCC9900FFCC9900FFCC6600CC33330000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      0000000000000000000066666600000000000000000000000000009999000000
      0000CCCCCC00FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000000000000CC999900FFCC9900FFCC
      9900FFCC9900FFCC9900FFCC9900FFCC6600CC33330000000000000000000000
      00000000000000000000000000000000000000000000CC999900FFCC9900FFCC
      9900FFCC9900FFCC9900FFCC9900FFCC6600CC33330000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC000000000000000000000000000000000000000000CC999900FFCC9900FFCC
      9900FFCC9900FFCC9900FFCC6600FFCC6600CC33330000000000000000000000
      00000000000000000000000000000000000000000000CC999900FFCC9900FFCC
      9900FFCC9900FFCC9900FFCC6600FFCC6600CC33330000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC0000000000CCCCCC00000000000000000000000000CC999900FFCC6600FFCC
      6600FFCC6600FFCC6600FFCC6600FFCC6600CC33330000000000000000000000
      00000000000000000000000000000000000000000000CC999900FFCC6600FFCC
      6600FFCC6600FFCC6600FFCC6600FFCC6600CC33330000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC003333330000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC999900CC999900CC99
      9900CC999900CC999900CC999900CC999900CC33330000000000000000000000
      00000000000000000000000000000000000000000000CC999900CC999900CC99
      9900CC999900CC999900CC999900CC999900CC33330000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999003333330000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099CCCC00000000000000
      00000000000099CCCC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099CCCC000000000000000000339999003399
      9900666666000000000099CCCC00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FF000000FF000000FF00
      0000FF000000FFFFFF00FFFFFF00000000000000000033333300333333009999
      9900CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003366990033669900336666003366
      660033669900339999000000000099CCCC000000000000000000FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FF00000099999900FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000003333330000FFFF0000CC
      CC003333330099999900CCCCCC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000339999003366660033999900000033000000
      3300000000003399990033669900000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF00FFFFFF000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000003333330000FF
      FF0000FFFF00333333003333330099999900CCCCCC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000066CCCC00000000003366990000000000000033006699CC0033CC
      CC006699CC000000330033666600000000000000000000000000FFFFFF000000
      0000FFFFFF00000000000000FF000000FF000000FF0000000000FFFFFF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FFFFFF00FF000000FF000000FF00
      0000FF000000FFFFFF00FFFFFF00000000000000000000000000000000003333
      330000FFFF0000FFFF0000CCCC00333333003333330099999900000000000000
      00000000000000000000000000000000000000000000000000000000000099CC
      CC000000000000000000000000000000330066CCCC006699CC0066FFFF0099FF
      FF0066CCFF0066CCCC0000003300000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      00003333330000FFFF0000FFFF0000FFFF003333330066333300999999000000
      0000000000000000000000000000000000000000000099CCCC00000000000000
      000066CCCC006699CC000000330066CCCC0099FFFF0033CCCC0099FFFF0066FF
      FF000000330099FFFF0066CCCC00000000000000000000000000FFFFFF000000
      0000FFFFFF00000000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0099009900FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000333333003333330000FFFF0000FFFF0000FFFF003333330033333300CCCC
      CC0000000000000000000000000000000000000000000000000066CCCC006699
      CC003399990033666600000033000000000099FFFF0099FFFF006699CC0066CC
      FF0099FFFF0099FFFF006699CC00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900990099009900FFFFFF00FFFFFF0000000000FF000000FF00
      0000FF000000FFFFFF00FFFFFF00000000000000000000000000000000003333
      33003333330000FFFF0000FFFF0000FFFF003333330033333300CCCCCC000000
      0000000000000000000000000000000000000000000066CCCC00666699003366
      9900000000000000330099FFFF0066FFFF003399CC0066FFFF0099FFFF0066CC
      CC0099FFFF0099FFFF00CCFFFF00000000000000000099000000990000009900
      00009900000099000000990000000000FF000000FF000000FF000000FF000000
      FF000000FF0000000000000000000000000000000000FFFFFF00990099009900
      990099009900990099009900990099009900FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000333333003333330000FFFF0000FFFF0000FFFF003333330033333300CCCC
      CC00000000000000000000000000000000000000330033666600000000000000
      000099FFFF0099FFFF003399CC0066CCCC0099FFFF000000330099FFFF00CCFF
      FF00CCFFFF000000000000000000000000000000000099000000990000009900
      000099000000990000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00000000000000000000000000FFFFFF00990099009900
      990099009900990099009900990099009900FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000333333003333330000FFFF0000FFFF00333333003333
      3300CCCCCC00000000000000000000000000000000000000000099FFFF0099FF
      FF0066CCCC000000000099FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900990099009900FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000333333003333330000FFFF003333
      330033333300CCCCCC0000000000000000000000000099FFFF006699CC0066CC
      CC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0099009900FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000333333003333
      3300333333000000000000000000000000000033330000000000CCFFFF0099FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000000000000000000000000000
      00000000FF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF009999990099999900999999009999
      990099999900FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000999999009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000999900009999
      000099990000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000099999900999999009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000999900009999
      000099990000000000000000000000000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000099990000999900009999
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF009999990099999900FFFFFF009999
      990099999900FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000099999900CCCCCC009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000999900009999
      000099990000000000000000000000000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000099990000999900009999
      000000000000000000000000FF000000FF000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000FFFFFF009999
      9900999999009999990000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF009999
      990099999900FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000099999900CCCCCC009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000000000000000000000000000000000000000000099990000999900009999
      000000000000000000000000FF000000FF000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF009999990000000000FFFFFF009999990099999900FFFFFF009999
      990099999900FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000099999900CCCCCC009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099000000990000009900000000000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      FF000000FF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00999999009999990000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000099999900CCCCCC009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099000000990000009900000000000000FF000000FF000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000009900
      00009900000099000000000000000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000FFFFFF009999
      9900999999009999990000000000FFFFFF009999990099999900999999009999
      990099999900FFFFFF0000000000000000000000000000000000000000000000
      00000000000099999900FFFFFF00CCCCCC009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      000099000000990000009900000000000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000009900
      0000990000009900000000000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000FFFFFF009999
      9900999999009999990000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000099999900FFFFFF00FFFFFF00CCCCCC00CCCCCC0099999900000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000000000000000000000000000FF000000FF000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF00009900
      00009900000099000000000000000000FF000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000FFFFFF009999
      9900999999009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00FFFFFF00FFFFFF00CCCCCC00CCCCCC0099999900999999000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000000000000000000000000000FF00000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000000000000000FF000000FF000000FF0000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF009999990099999900999999009999990099999900FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00CCCCCC00CCCCCC00CCCCCC00999999009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999009999990099999900999999009999
      9900999999009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF009999990099999900FFFFFF00999999009999990099999900999999009999
      9900FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF009999990099999900FFFFFF00999999009999990099999900999999009999
      9900FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF00000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF009999990099999900FFFFFF00999999009999990099999900999999009999
      9900FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF009999990099999900FFFFFF00999999009999990099999900999999009999
      9900FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF009999990099999900FFFFFF00999999009999990099999900999999009999
      9900FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF00000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF009999990099999900FFFFFF00999999009999990099999900999999009999
      9900FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000099000000990000009900000099000000990000009900000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF000099990000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000990000CCCCCC00FFFFFF00CCCCCC00FFFFFF00CCCCCC00FFFFFF000099
      0000000000000000000000000000000000000000000000000000000000000000
      0000009900000099000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000099990000FFFF0000999900000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      0000CCCCCC00FFFFFF00CCCCCC0000990000CCCCCC00FFFFFF00CCCCCC00FFFF
      FF00009900000000000000000000000000000000000000000000000000000099
      0000009900000099000000990000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000999999000099990000FF
      FF00FFFFFF00FFFFFF000099990000FFFF000099990000999900000000000000
      000000000000000000000000000000000000000000000000000099990000CCCC
      CC00FFFFFF00CCCCCC000099000000990000FFFFFF00CCCCCC00FFFFFF00CCCC
      CC00FFFFFF000099000000000000000000000000000000000000009900000099
      000000990000009900000099000000990000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000099990000FFFF0000999900000000000000
      0000000000000000000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF000099000000990000009900000099000000990000CCCCCC00FFFF
      FF00CCCCCC00FFFFFF0000990000000000000000000000990000009900000099
      0000FFFFFF00009900000099000000990000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000FFFFFF000000
      0000CCCCCC000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000099990000FFFF000099990000000000CCCC
      CC00000000000000000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000099000000990000FFFFFF009999000099990000CCCC
      CC00FFFFFF00CCCCCC000099000000000000009900000099000000990000FFFF
      FF000000000000000000009900000099000000990000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000CCCCCC0000000000000000000000000000000000999999000099990000FF
      FF00FFFFFF00FFFFFF000099990000FFFF00009999000099990000000000CCCC
      CC00000000000000000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000990000CCCCCC00FFFFFF0099990000FFFF
      FF00CCCCCC00FFFFFF0000990000000000000099000000990000FFFFFF000000
      0000000000000000000000000000009900000099000000990000FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000FFFFFF009999
      9900CCCCCC009999990000FFFF00000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF0000999900FFFFFF000099990099999900CCCC
      CC009999990000FFFF0000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF0099990000FFFFFF00FFFFFF0000990000CCCCCC00FFFFFF00CCCC
      CC00FFFFFF00CCCCCC0000990000000000000000000000000000000000000000
      000000000000000000000000000000000000009900000099000000990000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CCCC
      CC009999990000FFFF000000000000000000000000009999990000999900FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00CCCCCC009999
      990000FFFF000000000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF009999000099990000FFFFFF000099000000990000CCCCCC00FFFF
      FF00CCCCCC00FFFFFF0000990000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000990000009900000099
      0000FFFFFF000000000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009999990099999900CCCCCC00CCCC
      CC00CCCCCC009999990099999900000000000000000099999900FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF009999990099999900CCCCCC00CCCCCC00CCCC
      CC00999999009999990000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000099000000990000009900000099000000990000CCCC
      CC00FFFFFF00CCCCCC0000990000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009900000099
      000000990000FFFFFF0000000000000000000000000099000000990000009900
      000099000000990000009999990000FFFF0000FFFF00CCCCCC0000FFFF00CCCC
      CC0000FFFF0000FFFF0000FFFF0000000000000000000000000099999900FFFF
      FF0000FFFF009999990000FFFF0000FFFF00CCCCCC0000FFFF00CCCCCC0000FF
      FF0000FFFF0000FFFF000000000000000000000000000000000099990000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000099000000990000FFFFFF00FFFF
      FF00CCCCCC000099000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      000000990000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009999990000FFFF009999990000FF
      FF009999990000FFFF0000000000000000000000000000000000000000009999
      99009999990099999900999999009999990000FFFF009999990000FFFF009999
      990000FFFF000000000000000000000000000000000000000000000000009999
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000990000FFFFFF00FFFFFF00FFFF
      FF00009900000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000099000000990000FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000009999990000FFFF00000000009999990000FF
      FF00000000009999990000FFFF00000000000000000000000000000000000000
      000000000000000000009999990000FFFF00000000009999990000FFFF000000
      00009999990000FFFF0000000000000000000000000000000000000000000000
      000099990000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009999
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000099000000990000FFFFFF000000000000000000000000000000
      000000000000000000000000000000FFFF0000000000000000009999990000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000000000000000009999990000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999000099990000999900009999000099990000999900000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000990000FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009999990000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009999990000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000009900000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF000099990000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000099990000FFFF0000999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      00000000000000000000000000000000000000000000999999000099990000FF
      FF00FFFFFF00FFFFFF000099990000FFFF000099990000999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099999900FFFFFF00CCCCCC0099999900CCCCCC009999
      9900CCCCCC009999990099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000099990000FFFF0000999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000000FF0000FFFF0000999900000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF0000000000000000000000FF000000FF000000000000000000000000000000
      00000000000000000000000000000000000000000000999999000099990000FF
      FF00FFFFFF00FFFFFF000000FF000000FF000000FF0000999900000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009999990000999900FFFF
      FF0000FFFF00FFFFFF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000009900000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF00000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000009900000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000099999900FFFF
      FF0000FFFF00FFFFFF0000FFFF000000FF000000FF000000FF000000FF000000
      FF000000FF0000000000000000000000000099000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000990000009900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      990099999900999999000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00000000000000000099000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000009900000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000000000000000000000
      FF000000FF000000FF0000000000000000009900000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF0000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      000000000000000000000000990000000000000000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      0000000000000000000099000000000000000000000000000000000000000000
      0000FF000000FF00000000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      00000000000000009900FFFFFF0099999900000000000000000000000000FF00
      00000000000000000000FF000000000000000000000000000000000000000000
      00000000000099000000FFFFFF00999999000000000000000000000000000000
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000099999900FFFFFF00CCCCCC0099999900CCCCCC009999
      9900CCCCCC009999990099999900000000000000FF0000000000000000000000
      0000000000000000000099999900FFFFFF00CCCCCC0099999900CCCCCC009999
      9900CCCCCC009999990099999900000000000000000000000000000000000000
      000000009900FFFFFF000000FF00000099009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099000000FFFFFF00FF000000990000009999990000000000000000000000
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000999999000000FF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      9900FFFFFF000000FF00000099000000FF000000990099999900000000000000
      0000000000000000000000000000000000000000000000000000000000009900
      0000FFFFFF00FF00000099000000FF0000009900000099999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      990099999900000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000FF00FFFFFF00CCCCCC009999
      990099999900000000000000000000000000000000000000000000009900FFFF
      FF000000FF00000099000000FF00000099000000FF0000009900999999000000
      000000000000000000000000000000000000000000000000000099000000FFFF
      FF00FF00000099000000FF00000099000000FF00000099000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      990000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000FF000000FF000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      9900FFFFFF000000FF00000099000000FF000000990099999900000000000000
      0000000000000000000000000000000000000000000000000000000000009900
      0000FFFFFF00FF00000099000000FF0000009900000099999900000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000999999000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF0000000000000000000000FF000000FF000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000009900FFFFFF000000FF00000099009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099000000FFFFFF00FF000000990000009999990000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000009900FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000099000000FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000009900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      000099000000FF000000FF000000CCCCCC00CCCCCC0000000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CCCCCC00CCCCCC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      000000000000CCCCCC00CCCCFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC00CCCCCC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000990000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000099CCFF00CCCCFF00CCCCCC00CCCCCC00CCCCCC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000099999900FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000990000FFFFFF00999999000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC0099CCFF0099CCFF00CCCCFF00CCCCFF00CCFFFF000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF00CCCCCC00999999009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000990000FFFFFF0000FF0000009900009999990000000000000000000000
      000000000000FF0000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC0099CCFF0099CCFF00CCCCFF00CCCCFF00CCFF
      FF0000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00FFFF
      FF00000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00CCCCCC0099999900CCCCCC009999990099999900000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      0000FFFFFF0000FF00000099000000FF00000099000099999900000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC000000000099CCFF0099CCFF00CCCCFF00CCCC
      FF0000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC0000000000CCCCCC00CCCCCC00CCCCCC00CCCC
      CC0000000000000000000000000000000000000000000000000099999900FFFF
      FF00CCCCCC0099999900CCCCCC0099999900CCCCCC0099999900999999000000
      000000000000000000000000000000000000000000000000000000990000FFFF
      FF0000FF00000099000000FF00000099000000FF000000990000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CCCCCC00CCCCCC00CCCCCC000000000099CCFF0099CCFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CCCCCC00CCCCCC00CCCCCC0000000000CCCCCC00CCCCCC00000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00CCCCCC0099999900CCCCCC009999990099999900000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      0000FFFFFF0000FF00000099000000FF00000099000099999900000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC0000000000CCCCCC00CCCCCC000000000066CCFF0099CCFF0099CCFF000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC0000000000CCCCCC00CCCCCC0000000000CCCCCC00CCCCCC00CCCCCC000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF00CCCCCC00999999009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000990000FFFFFF0000FF0000009900009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CCCCCC0000000000CCCCCC00CCCCCC000000000066CCFF0099CCFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CCCCCC0000000000CCCCCC00CCCCCC0000000000CCCCCC00CCCCCC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000099999900FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000990000FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC0000000000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC0000000000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000990000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000990000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000009900000099000000990000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099993300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099993300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000990000009900000099000000990000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033990000999933000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033990000999933000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      00000099000000990000FFFFFF000099000000990000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099993300339900009999330000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099993300339900009999
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000009900000099
      000000990000FFFFFF0000000000000000000099000000990000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033990000999933003399000099993300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033990000999933003399
      0000999933000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00CCCCCC00CCCCCC00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000099
      0000FFFFFF000000000000000000000000000000000000990000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099993300339900009999330033990000999933000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099993300339900009999
      3300339900009999330000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000990000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033990000999933003399000099993300339900009999
      3300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033990000999933003399
      000099993300339900009999330000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000009900000099000000990000009900
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000099993300339900009999330033990000999933003399
      0000000000000000000000000000000000000000000000000000990000000000
      0000000000000000000000000000000000000000000099993300339900009999
      330033990000999933003399000000000000000000000000000000000000FFFF
      FF00CCCCCC00CCCCCC00FFFFFF00000000009900000099000000990000009900
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000990000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000033990000999933003399000099993300339900000000
      0000000000000000000000000000000000009900000099000000990000009900
      0000000000000000000000000000000000000000000033990000999933003399
      000099993300339900000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000990000FFFFFF00000000000000000000000000000000000000
      0000000000000000000099993300339900009999330033990000000000000000
      0000000000000000000000000000000000000000000000000000990000000000
      0000000000000000000000000000000000000000000099993300339900009999
      3300339900000000000000000000000000000000000000000000990000009900
      0000990000009900000099000000990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000990000FFFFFF000000000000000000000000000000
      0000000000000000000033990000999933003399000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033990000999933003399
      0000000000000000000000000000000000000000000000000000990000009900
      0000990000009900000099000000990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000009900000000000000000000000000000000
      0000000000000000000099993300339900000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099993300339900000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033990000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033990000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000006666
      6600666666006666660066666600666666006666660066666600666666006666
      6600666666006666660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF0099FFFF00FFFFFF0099FFFF00FFFFFF0099FFFF00FFFF
      FF00FFFFFF00CCCCCC0000000000000000000000000000000000000000000000
      0000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000CC996600CC99
      6600CC996600CC996600CC996600CC6666009966660099666600996666009966
      6600996666009966660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF00FF996600FF996600FFFFFF00FF996600FF996600FF99
      6600FFFFFF00CCCCCC0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000099669900CC996600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009966660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF0099FFFF00FFFFFF0099FFFF00FFFFFF0099FFFF00FFFF
      FF0099FFFF00CCCCCC0000000000000000000000000000000000000000000000
      0000FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000006699CC0099669900CC99
      9900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009966660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF00FF996600FF996600FFFFFF00FF996600FF996600FF99
      6600FFFFFF00CCCCCC0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000000000003399FF006699CC009966
      9900CCCCCC000033000000330000003300000033000000330000003300000033
      0000FFFFFF009966660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF0099FFFF00FFFFFF0099FFFF00FFFFFF0099FFFF00FFFF
      FF0099FFFF00CCCCCC0000000000000000000000000000000000000000000000
      0000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000000000000FFCC99003399
      FF0099669900FFCCCC00CC990000CC990000CC990000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009966660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF0099FFFF00FFFF
      FF0099999900FFFFFF00FF996600FF996600FFFFFF00FF996600FF996600FF99
      6600FFFFFF00CCCCCC0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFCC9900CCCC
      CC00CCCCCC00FF990000FFFFCC00FFFFCC00FFCCCC00FF990000003300000033
      0000FFFFFF009966660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF0099FF
      FF0099999900FFFFFF00FFFFFF00FFFFFF0099FFFF00FFFFFF00FFFFFF00FFFF
      FF00CCCCCC00CCCCCC0000000000000000000000000000000000000000009900
      0000990000009900000099000000000000009900000099000000990000009900
      0000FFFFFF0000000000FFFFFF00000000000000000000000000FFCC9900FFFF
      FF00FF990000FFFFCC00FFFFCC00FFFFFF00FFFFFF00FFCCCC00FF990000FFFF
      FF00FFFFFF00CC66660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF0099FFFF00FFFF
      FF00999999009999990099999900999999009999990099999900999999009999
      9900999999009999990099999900000000000000FF0000000000000000009900
      0000990000009900000099000000000000009900000099000000990000009900
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFCC9900FFFF
      FF00CC990000FFFFCC00FFFFCC00FFFFFF00FFFFFF00FFFFCC00CC9900000033
      0000FFFFFF00CC99660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF0099FF
      FF00FFFFFF0099FFFF00FFFFFF00CCCCCC000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF00000000000000
      000000000000000000000000000000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000000000000FFCC9900FFFF
      FF00CC990000FFFFCC00FFFFCC00FFFFCC00FFFFCC00FFFFCC00CC990000FFFF
      FF00FFFFFF00CC99660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF0099FFFF00FFFF
      FF0099FFFF00FFFFFF0099FFFF00CCCCCC000000000000000000000000000000
      0000990000000000000000000000000000000000FF000000FF000000FF000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFCC9900FFFF
      FF00FF990000FFCCCC00FFCCCC00FFFFCC00FFFFCC00CC999900003300000033
      0000FFFFFF00CC99660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF0099FF
      FF00FFFFFF0099FFFF00CCCCCC00CCCCCC000000000000000000000000009900
      0000993300009900000000000000000000000000FF000000FF000000FF000000
      FF00000000000000000000000000990000009900000099000000990000009900
      0000990000009900000099000000990000000000000000000000FFCC9900FFFF
      FF00FFFFFF00FF990000CC990000CC990000CC990000FF990000FFFFFF00FFFF
      FF00FFFFFF00CC99660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF0099FFFF000000000000000000000000000000000000000000CC330000CC33
      0000993300009900000099000000000000000000FF000000FF000000FF000000
      0000000000000000000000000000990000009900000099000000990000009900
      0000990000009900000099000000990000000000000000000000FFCC9900FFFF
      FF00003300000033000000330000003300000033000000330000FFFFFF009966
      6600996666009966660066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0099999900FFFFFF00000000000000000000000000000000000000
      0000990000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFCC9900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009966
      6600CC999900CC99990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009999990000000000000000000000000099000000990000009900
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFCC9900FFCC
      9900FFCC9900CCCC9900CCCC9900CC999900CC999900CC999900CC9999009966
      6600CC9999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009999990099999900999999009999
      9900999999009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099009900000000000000
      0000990099000000000000000000990099000000000000000000000000009900
      9900990099000000000000000000000000009999990099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000009999
      9900999999009999990099999900999999009999990099999900999999009999
      9900999999009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099009900000000000000
      0000990099000000000000000000000000009900990000000000000000009900
      99000000000099009900000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000009999
      9900CCCCCC00CCCCCC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CCCCCC00CCCC
      CC00CCCCCC009999990099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099009900990099009900
      9900990099000000000099009900990099009900990099009900000000009900
      99009900990000000000000000000000000099999900FFFFFF00FFFFFF009999
      0000999900009999000099990000999900009999000099990000999900009999
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000009999
      9900CCCCCC00CCCCCC0099999900000000000000000000000000CCCCCC000099
      000000FF00009999990099999900000000000000000000FFFF00000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00000000000000000000000000000000000000000099009900000000000000
      0000990099000000000000000000000000009900990000000000000000009900
      99000000000099009900000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000099999900CCCCCC00CCCCCC009999
      9900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009999990099999900000000000000000000FFFF00000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000990099009900
      9900000000000000000000000000990099000000000000000000000000009900
      99009900990000000000000000000000000099999900FFFFFF00FFFFFF009999
      0000999900009999000099990000999900009999000099990000999900009999
      0000FFFFFF00FFFFFF00000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF0099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC0099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000099999900FFFFFF0099000000FFFF
      FF00990000009999990099999900999999009999990099999900999999009999
      990099999900999999009999990099999900000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00CCCC
      CC00FFFFFF00FFFFFF00990000009900000099000000FFFFFF00FFFFFF00CCCC
      CC00FFFFFF00FFFFFF00000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CCCCCC0000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      00000000000000000000CCCCCC00000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      9900000000000000000000000000000000000000000000000000000000000099
      99000000000000000000000000000000000099999900FFFFFF00CCCCCC009900
      0000FFFFFF00FF000000FFFF000000990000FF00000099000000FFFFFF009900
      0000CCCCCC00FFFFFF00000000000000000099999900FFFFFF0099000000FFFF
      FF00990000009900000099000000FFFFFF00CCCCCC0000000000000000000000
      0000000000000000FF000000000000000000000000000000000000FFFF000000
      00000000000000000000CCCCCC00000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999990000000000000000000000000099999900000000000000
      00000000000000000000000000000000000099999900CCCCCC0099000000FFFF
      FF00FFFFFF0099990000FFFF0000FFFF00000099000099000000FFFFFF00FFFF
      FF0099000000CCCCCC00000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CCCCCC0000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009900
      0000FFFFFF0099990000FFFFFF00FFFF0000FFFF000099000000FFFFFF009900
      0000CCCCCC00FFFFFF00000000000000000099999900FFFFFF0099000000FFFF
      FF00990000009900000099000000FFFFFF00CCCCCC0000000000000000000000
      FF000000FF000000FF000000FF000000FF0000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      9900000000000000000000000000000000000099990000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00CCCC
      CC00FFFFFF00FFFFFF009999000099990000FF000000FFFFFF00FFFFFF00CCCC
      CC00FFFFFF00FFFFFF00000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      9900000000000000000000000000000000000099990000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000000000000000000099999900FFFFFF0099000000FFFF
      FF009900000099000000CCCCCC00FFFFFF009999990000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CCCC
      CC00FFFFFF0099999900000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC00999999000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      000000FFFF00000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000009999000000000000000000000000000000
      0000000000000000000000000000000000009999990099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CCCC
      CC00999999000000000000000000000000009999990099999900999999009999
      990099999900999999009999990000000000000000000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999009999990099999900999999009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900000000000000
      0000000000000000000000000000000000000000000000000000990000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000009999990099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000099999900999999000000
      0000000000000000000000000000000000000000000099000000990000009900
      0000000000000000000000FFFF0000999900000000000000000000FFFF000000
      0000000000000099990000999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF0000009999990099999900999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000099999900999999009999
      9900000000000000000000000000000000009900000099000000990000009900
      000099000000000000000000000000FFFF000099990000999900FFFFFF000000
      00000099990000FFFF0000000000CCCCCC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF00000099999900999999009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000999999009999
      9900999999000000000000000000000000000000000000000000000000000000
      00000000000000000000009999000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000999999009999990099999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000FF0000009999
      9900999999009999990000000000000000000000000000000000000000000000
      000000000000000000000099990000FFFF00FFFFFF0000000000000000000000
      0000FFFFFF0000FFFF0000999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF0000009999990099999900999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000FF000000FF00
      0000999999009999990099999900000000000000000000000000000000000000
      0000000000000000000000000000009999000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF00000099999900999999009999
      990000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF0000000000FF000000FF000000FF000000FF000000FF00
      0000FF0000009999990099999900999999000000000000000000999999000000
      000000000000999999000000000000FFFF000099990099999900FFFFFF000000
      000000FFFF00009999000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      00000000FF000000FF000000FF0000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF0000009999990000000000CCCCCC0000000000CCCCCC009999
      990099999900FFFFFF0000000000009999000000000000000000FFFFFF000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF00000099999900000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF0000000000FF000000FF000000FF000000FF000000FF00
      0000FF000000999999000000000000000000000000009999990000000000FFFF
      FF0099999900FFFFFF00FFFFFF0000000000FFFFFF00CCCCCC00999999000000
      0000000000000000000000000000000000000000000099009900990099009900
      9900990099009900990099009900990099009900990099009900990099000000
      000000000000000000000000FF0000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF0000009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000FF000000FF00
      00009999990000000000000000000000000000000000CCCCCC00FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000FF0000009999
      9900000000000000000000000000000000009999990000000000999999000000
      000000000000FFFFFF0000000000000000009999990000000000000000000000
      0000000000000000000000000000000000000000000099009900990099009900
      9900990099009900990099009900990099009900990099009900990099000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF00000099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000999999000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0000000000FFFFFF00CCCCCC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF0000009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000099999900000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC000000
      0000FFFFFF00999999000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900999999009999990099999900999999009999990099999900999999009999
      9900999999009999990099999900000000000000000000000000000000009999
      990099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC000000FF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000990000009900000099000000660000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000CC00000099000000990000009900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009999990000000000000000000000000099999900FF00
      000099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      9900999999000000000099999900000000000000000000000000000000000000
      000000CC000000CC000000990000009900000099000000990000009900000066
      0000000000000000000000000000000000000000000000000000000000000000
      00000000CC000000CC000000CC000000CC000000CC0000009900000099000000
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF0000FF
      FF0000FFFF00000000009999990000000000000000009999990099990000FF00
      000099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      99009999990000000000999999000000000000000000000000000000000000CC
      000000CC000000CC000000CC000000CC00000099000000990000009900000099
      0000006600000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000CC000000CC000000CC000000CC00000099000000
      990000006600000000000000000000000000000000000000000099999900FFFF
      FF00CCCCCC00CCCCCC00FFFFFF00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC0000FFFF00000000009999990000000000000000009999990000990000FF00
      000099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      99009999990000000000999999000000000000000000000000000000000000CC
      000000CC000000CC000000CC000000CC000000CC000000CC0000009900000099
      0000006600000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000CC000000CC000000CC000000
      990000009900000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000099999900000000009999990000990000FF000000FF00
      000099999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009999
      990099999900000000009999990000000000000000000000000000CC000000CC
      000000CC000000CC000000CC000000CC000000CC000000CC0000009900000099
      00000099000000660000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000CC000000CC000000
      CC0000009900000099000000000000000000000000000000000099999900FFFF
      FF00CCCCCC00CCCCCC00FFFFFF00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00FFFFFF000000000099999900000000009999990099990000FF000000FF00
      0000999999009999990099999900999999009999990099999900999999000000
      000099999900000000009999990000000000000000000000000000CC000000FF
      000000FF000000FF000000CC000000CC000000CC000000CC000000CC00000099
      00000099000000990000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000CC000000
      CC000000CC00000099000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000099999900000000009999990099990000FF000000FF00
      000099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      990000000000000000009999990000000000000000000000000000CC000000FF
      000000FF000000FF000066FF660033FF330000CC000000CC000000CC000000CC
      00000099000000990000000000000000000000000000000000000000FF000000
      FF000000FF000000FF006666FF003333FF000000FF000000FF000000FF000000
      CC000000CC00000099000000000000000000000000000000000099999900FFFF
      FF00CCCCCC00CCCCCC00FFFFFF00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00FFFFFF000000000099999900000000009999990099990000FF000000FF00
      000099999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009999
      990099999900000000000000000000000000000000000000000000CC000000FF
      000000FF000000FF000066FF660033FF330000CC000000CC000000CC000000CC
      00000099000000990000000000000000000000000000000000000000FF000000
      FF000000FF000000FF006666FF003333FF000000FF000000FF000000FF000000
      CC000000CC00000099000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000FFFF0000000000999999000000000099999900FFFFFF0099990000FF00
      0000999999009999990099999900999999009999990099999900999999000000
      00009999990000000000999999000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000CC000000CC000000CC
      0000009900000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      CC000000CC00000000000000000000000000000000000000000099999900FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000099999900000000000000000099999900FFFFFF00FFFF
      FF0099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      99000000000000000000999999000000000000000000000000000000000000CC
      000000FF000000FF000000FF000000FF000000FF000000CC000000CC000000CC
      0000009900000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      CC000000CC00000000000000000000000000000000000000000099999900FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000099999900000000000000000099999900FFFFFF00CCCC
      CC0099999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009999
      9900999999000000000000000000000000000000000000000000000000000000
      000000CC000000FF000000FF000000FF000000FF000000CC000000CC000000CC
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      CC00000000000000000000000000000000000000000000000000999999009999
      9900999999009999990099999900999999009999990099999900999999009999
      990099999900000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF0099999900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00999999000000000099999900000000000000000000000000000000000000
      0000000000000000000000CC000000CC000000CC000000CC0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      990099999900FFFFFF0099999900999999009999990099999900999999009999
      990099999900CCCCCC0099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999990099999900999999009999990099999900999999009999
      9900999999009999990099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0099999900FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000099990000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      99009999990099999900FFFFFF0099999900FFFFFF0099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000999900FFFFFF0000999900FFFFFF000099990000FFFF000099
      9900000000000000000000000000000000000000000000000000000000009999
      9900999999009999990099999900999999009999990099999900999999009999
      9900999999009999990099999900000000000000000099999900FFFFFF000000
      0000FFFFFF0000000000FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      990099999900FFFFFF00FFFFFF00FFFFFF0099999900FFFFFF00999999009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000FFFF00FFFFFF00FFFFFF000099990000FFFF00009999000099
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099999900000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000999999000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000009999
      99009999990099999900FFFFFF0099999900FFFFFF0099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000999900FFFFFF0000999900FFFFFF000099990000FFFF000099
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000099999900000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000009999
      990099999900FFFFFF00FFFFFF00FFFFFF0099999900FFFFFF00999999009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000FFFF00FFFFFF00FFFFFF000099990000FFFF00009999000099
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00CCCCCC00CCCCCC00FFFFFF00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00FFFFFF000000000099999900000000000000000099999900999999009999
      9900999999009999990099999900999999000000000099999900FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00999999000000000000000000000000009999
      99009999990099999900FFFFFF0099999900FFFFFF0099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000999900FFFFFF0000999900FFFFFF000099990000FFFF000099
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF000000
      0000FFFFFF0000000000FFFFFF00999999000000000000000000000000009999
      990099999900FFFFFF00FFFFFF00FFFFFF0099999900FFFFFF00999999009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000FFFF00FFFFFF00FFFFFF000099990000FFFF00009999000099
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00CCCCCC00CCCCCC00FFFFFF00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00FFFFFF000000000099999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000999999000000000000000000000000009999
      99009999990099999900FFFFFF0099999900FFFFFF0099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000999900FFFFFF0000999900FFFFFF000099990000FFFF000099
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000099999900000000000000000099999900999999009999
      9900999999009999990099999900999999000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000009999
      990099999900FFFFFF00FFFFFF00FFFFFF0099999900FFFFFF00999999009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000FFFF00FFFFFF00FFFFFF000099990000FFFF00009999000099
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00CCCCCC00CCCCCC00FFFFFF00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00FFFFFF000000000099999900000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000009999
      99009999990099999900FFFFFF0099999900FFFFFF0099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000009999
      99000099990000999900FFFFFF0000999900FFFFFF0000999900FFFFFF000099
      990000000000000000000000000000000000000000000000000099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000099999900000000000000000099999900FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      990099999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000009999
      990000999900FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000999999006666
      6600666666006666660066666600666666006666660066666600666666006666
      6600666666000000000099999900000000000000000099999900FFFFFF000000
      0000FFFFFF0000000000FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000999999006666
      6600666666006666660066666600666666006666660066666600666666006666
      6600666666000000000099999900000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      000099999900FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF009999
      9900000000000000000000000000000000000000000000000000999999009999
      9900999999009999990099999900999999009999990099999900999999009999
      9900999999000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999990099999900999999009999990099999900999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999990099999900999999009999990099999900999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000000099999900FFFFFF00CCCCCC0099999900CCCCCC009999
      9900CCCCCC009999990099999900000000000000000000000000000000000000
      0000000000000000000099999900FFFFFF00CCCCCC0099999900CCCCCC009999
      9900CCCCCC009999990099999900000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000999999000000FF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00CCCCCC0099999900CCCC
      CC00999999009999990000000000000000000000000099999900999999009999
      9900999999009999990099999900999999000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000FF00FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000FF000000FF000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF009999
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF0000000000000000000000FF000000FF000000000000000000999999000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000990000009900000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900999999009999990099999900999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000CCCCCC00999999009999990000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009900
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000CCCCCC00CCCCCC0066666600000000000000000099999900666666000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000066660099FFFF0000CCFF0000CCFF0000CC
      FF000099CC000099CC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099000000FFFFFF000000
      00000000000000000000FFFFFF00000000000000000000000000FFFFFF009900
      0000FFFFFF000000000000000000000000000000000000000000CCCCCC00CCCC
      CC006666660066666600CCCCCC00999999009999990000000000000000009999
      9900666666000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000066660099FFFF00FFFFFF0000CCFF0000CC
      FF0000CCFF000099CC0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009900
      0000FFFFFF000000000000000000000000000000000099999900666666006666
      6600CCCCCC00CCCCCC00CCCCCC00999999009999990099999900999999000000
      0000000000006666660000000000000000000000000000000000000000000000
      00000000000000000000000000000066660099FFFF0000CCFF00FFFFFF0000CC
      FF0000CCFF000099CC0000000000000000000000000000000000FFFFFF009999
      990099999900999999000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000099000000FFFFFF000000
      00000000000000000000FFFFFF00000000000000000000000000FFFFFF009900
      0000FFFFFF000000000000000000000000009999990066666600CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00999999009999990099999900999999009999
      9900999999000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000066660099FFFF0099FFFF0099FFFF0099FF
      FF0099FFFF0099FFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
      FF000000FF00FFFFFF0000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009900
      0000FFFFFF0000000000000000000000000099999900FFFFFF00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00FFFFFF00999999009999990099999900999999009999
      990066666600999999006666660000000000000000000000000066666600CCCC
      CC00CCCCCC00CCCCCC00CCCCCC000066660099FFFF0099FFFF0099FFFF000066
      6600006666000066660000000000000000000000000000000000FFFFFF009999
      99009999990099999900FFFFFF00FFFFFF000000FF000000FF00FFFFFF000000
      FF00FFFFFF00FFFFFF0000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000FFFFFF0000000000000000000000000099999900FFFFFF00CCCCCC00CCCC
      CC00FFFFFF00FFFFFF00CCCCCC00CCCCCC00CCCCCC0099999900999999009999
      99009999990066666600666666000000000000000000000000006666660099FF
      FF00FFFFFF0099FFFF00FFFFFF00CCCCCC000066660000666600006666000066
      6600000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000099000000FFFFFF009900
      00009900000099000000FFFFFF00990000009900000099000000FFFFFF009900
      0000FFFFFF0000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF00CCCCCC00CCCCCC000000FF003300CC00CCCCCC00CCCCCC00CCCCCC009999
      99009999990099999900666666000000000099999900CCCCCC0066666600FFFF
      FF0099FFFF00FFFFFF0099FFFF00CCCCCC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF009999
      99009999990099999900FFFFFF00FFFFFF000000FF000000FF00FFFFFF000000
      FF00FFFFFF00FFFFFF0000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000FFFFFF0000000000000000000000000099999900FFFFFF00CCCCCC00CCCC
      CC0000FF000000996600CCCCCC00CCCCCC00CCCCCC0066666600999999000000
      0000CCCCCC0066666600666666000000000099999900CCCCCC0066666600FFFF
      FF00FFFFFF0099FFFF00FFFFFF00CCCCCC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009999990099999900FFFF
      FF00CCCCCC00CCCCCC00CCCCCC009999990066666600CCCCCC00CCCCCC000000
      0000CCCCCC0066666600666666000000000099999900CCCCCC0066666600FFFF
      FF0099FFFF00FFFFFF0099FFFF00CCCCCC000000000000000000000000000000
      0000000000009900000000000000000000000000000000000000FFFFFF009999
      9900999999009999990099999900FFFFFF009999990099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000009900000000000000
      9900000000000000000000009900000000000000000000009900000000000000
      0000000099000000000000009900000099000000000000000000000000009999
      990099999900FFFFFF0066666600CCCCCC00CCCCCC00CCCCCC00FFFFFF00CCCC
      CC000000000000000000000000000000000099999900CCCCCC0066666600FFFF
      FF00FFFFFF0099FFFF00FFFFFF00CCCCCC000000000000000000000000000000
      0000990000009933000099000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000009900000099000000
      9900000000000000000000009900000000000000000000009900000000000000
      0000000099000000000000009900000000000000000000000000000000000000
      0000000000009999990099999900CCCCCC00CCCCCC00FFFFFF00FFFFFF00FFFF
      FF00CCCCCC0000000000000000000000000099999900CCCCCC0066666600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00CCCCCC00000000000000000000000000CC33
      0000CC3300009933000099000000990000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000099000000000000000000000000009900000000000000
      9900000000000000000000009900000000000000000000009900000099000000
      9900000099000000000000009900000000000000000000000000000000000000
      000000000000000000000000000099999900FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CCCCCC00000000000000000099999900CCCCCC00666666006666
      6600666666006666660066666600666666000000000000000000000000000000
      0000000000009900000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000099000000000000000000000000009900000000000000
      9900000000000000990000009900000099000000000000009900000000000000
      0000000099000000000000009900000000000000000000000000000000000000
      00000000000000000000000000000000000099999900FFFFFF00FFFFFF00FFFF
      FF009999990099999900000000000000000099999900CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC0000000000000000000000000000000000990000009900
      0000990000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900999999009999
      9900000000000000000000000000000000009999990099999900999999009999
      9900999999009999990066666600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000999900000000000099
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC00000000000000000000000000000000009999
      000099990000990000009900000000000000CCCCCC000000000000FFFF000000
      0000009999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC000000000000000000FF00000099990000FF00
      0000FF000000FF000000FF000000FF00000000000000CCCCCC00000000000099
      9900000000000000000000999900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009900000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00CCCCCC00000000000000000099990000FF0000000000
      000000000000000000000000000000000000000099000000000000FFFF000000
      000000FFFF0000FFFF0000000000009999000000000000000000000000000000
      0000CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000990000009900000000000000999999000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC0000000000000000000000000000000000CCCC
      CC00009999000000000000009900000099000000FF00CCCCCC000099990000FF
      FF0000FFFF000099990000FFFF00000000000000000000000000000000000000
      0000CCCCCC000099990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099000000990000009900000099999900000000009999
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00CCCCCC00000000000000000000000000CCCCCC000099
      990099999900000000000000000000000000CCCCCC00CCCCCC0000999900CCCC
      CC00009999000000000000FFFF00000000000000000000000000000000000000
      0000CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000990000009900000099000000990000009900000099000000999999000000
      000000000000999999000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC0000000000000000000000000000000000CCCC
      CC0000999900CCCCCC0000FFFF0000000000000000000000000000999900CCCC
      CC00CCCCCC0000FFFF0000000000009999000000000000000000000000000000
      0000CCCCCC000099990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC009900
      00009900000099000000FFFFFF00CCCCCC009900000099000000990000009900
      0000999999000000000099999900000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0099009900FFFFFF00FFFFFF000000000000000000000000000000
      0000FFFFFF00FFFFFF00CCCCCC000000000000000000000000000000000000FF
      FF00CCCCCC0000FFFF00CCCCCC000000000000999900CCCCCC00000000000099
      9900009999000000000000999900000000000000000000000000000000000000
      0000CCCCCC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900990000009900
      000099000000FFFFFF00FFFFFF00FFFFFF009900000099000000990000009900
      00009900000000000000999999000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900990099009900FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC0000000000000000000000000000FFFF00CCCC
      CC0000FFFF00FFFFFF0000FFFF00CCCCCC000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      9900CCCCCC000099990000000000000000000000000000000000000000000000
      00000000000000000000000000000000000099999900CCCCCC00990000009900
      0000FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF0099000000990000009900
      000099000000CCCCCC00000000000000000000000000FFFFFF00990099009900
      990099009900990099009900990099009900FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF00CCCCCC00000000000000000000000000CCCCCC0000FF
      FF00CCCCCC0000FFFF00000000000000000000999900CCCCCC00009999009999
      9900000000000000000000000000000000000000000000000000CCCCCC00CCCC
      CC00CCCCCC000099990000999900000000000000000000000000000000000000
      00000000000000000000000000000000000099999900CCCCCC00990000009900
      0000990000009900000099000000CCCCCC00FFFFFF00CCCCCC00990000009900
      00009900000099000000000000000000000000000000FFFFFF00990099009900
      990099009900990099009900990099009900FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC00000000000000000000000000000000000000
      000000FFFF00CCCCCC0000FFFF0000000000CCCCCC0000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00CCCC
      CC00000000000099990000999900000000000000000000000000000000000000
      00000000000000000000000000000000000099999900CCCCCC00990000009900
      000099000000990000009900000099000000FFFFFF00FFFFFF00990000009900
      000099000000CCCCCC00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900990099009900FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CCCCCC00000000000000000000000000000000000000
      00000000000000FFFF00CCCCCC00000000000099990000FFFF00000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00CCCCCC00CCCCCC00CCCCCC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC009900
      000099000000990000009900000099000000CCCCCC00FFFFFF00CCCCCC009900
      0000990000009999990099999900000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0099009900FFFFFF00FFFFFF000000000000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000CCCCCC0000999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900CCCCCC00CCCC
      CC00990000009900000099000000990000009900000099000000990000009900
      0000CCCCCC00999999000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000099999900FFFFFF0099999900FFFF
      FF0099999900FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999009999
      9900CCCCCC009900000099000000990000009900000099000000CCCCCC009999
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000999999009999990099999900999999009999990099999900999999000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000999999000000
      0000000000000000000000000000999999000000000000000000000000000000
      0000999999000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF000099990000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000099990000FFFF0000999900000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009999990000000000000000009999990000999900FFFFFF00FFFFFF000099
      99009999990000000000000000000000000000000000999999000099990000FF
      FF00FFFFFF00FFFFFF000099990000FFFF000099990000999900000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000FF00000000000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF0000000000CCCC
      CC00000000000000000000000000000000000000000000000000000000000000
      00000000000099999900000000000099990000FFFF00FFFFFF00FFFFFF0000FF
      FF00009999000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000099990000FFFF0000999900000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000CCCC
      CC00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000999900FFFFFF00FFFFFF000099
      990000FFFF000000000000000000000000000000000099999900009999000099
      9900FFFFFF0000999900FFFFFF000000FF0000FFFF0000999900000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000000000000000FF000000
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF0099999900CCCC
      CC009999990000FFFF0000000000000000000000000099999900000000009999
      9900999999009999990000000000009999000000000000000000000000000000
      00000099990000000000999999000000000000000000999999000099990000FF
      FF00FFFFFF00FFFFFF000000FF000000FF000000FF0000999900000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CCCCCC009999
      990000FFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000099999900009999000099
      9900FFFFFF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000990000009900
      00009900000099000000990000009999990099999900CCCCCC00CCCCCC00CCCC
      CC00999999009999990000000000000000000000000000000000000000000000
      0000000000009999990000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009999990000999900FFFF
      FF0000FFFF00FFFFFF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000990000009900
      0000990000009999990000FFFF0000FFFF00CCCCCC0000FFFF00CCCCCC0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000999999000000000000000000999999000000000000000000999999000000
      0000000000000000000000000000000000000000000099999900FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009999990000FFFF009999990000FFFF009999
      990000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000999999000000000000000000000000000000
      000000000000000000000000000000000000000000000000000099999900FFFF
      FF0000FFFF00FFFFFF0000FFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009999990000FFFF00000000009999990000FFFF000000
      00009999990000FFFF0000000000000000000000000000000000999999000000
      0000000000000000000000000000999999000000000000000000000000000000
      0000999999000000000000000000000000000000000000000000000000009999
      990099999900999999000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00000000000000000000000000000000000000FF000000
      FF000000FF000000FF00000000000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000000000000000009999990000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009999990000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000999999000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000000000000000000000
      FF000000FF000000FF000000000000000000000000000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099000000990000009900000099000000990000009900
      0000990000009900000099000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0099000000000000000000000000000000000000000000
      0000000000000099000000990000009900000099000000990000009900000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0099000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0099000000000000000000000000000000000000000000
      000000990000CCCCCC00FFFFFF00CCCCCC00FFFFFF00CCCCCC00FFFFFF000099
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF009900
      9900FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0099000000000000000000000000000000000000009999
      0000CCCCCC00FFFFFF00CCCCCC0000990000CCCCCC00FFFFFF00CCCCCC00FFFF
      FF00009900000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF009900
      990099009900FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF009999
      990000000000FFFFFF0099000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF009900000000000000000000000000000099990000CCCC
      CC00FFFFFF00CCCCCC000099000000990000FFFFFF00CCCCCC00FFFFFF00CCCC
      CC00FFFFFF0000990000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF009900990099009900990099009900
      99009900990099009900FFFFFF00000000000000000099000000990000009900
      0000990000009900000099000000990000000000000000000000990000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0099000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF000099000000990000009900000099000000990000CCCCCC00FFFF
      FF00CCCCCC00FFFFFF00009900000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF009900990099009900990099009900
      99009900990099009900FFFFFF00000000000000000099000000990000009900
      0000990000009900000099000000990000000000000000000000990000009900
      0000000000000000000000000000000000000000000000000000FFFFFF009999
      990000000000FFFFFF0099000000FFFFFF000000000000000000FFFFFF009900
      0000990000009900000099000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000099000000990000FFFFFF009999000099990000CCCC
      CC00FFFFFF00CCCCCC00009900000000000000000000FFFFFF00FFFFFF00FF00
      0000FF000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF009900
      990099009900FFFFFF00FFFFFF00000000000000000099000000990000009900
      0000990000009900000099000000990000000000000000000000990000009900
      0000990000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF009900
      0000FFFFFF009900000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000990000CCCCCC00FFFFFF0099990000FFFF
      FF00CCCCCC00FFFFFF00009900000000000000000000FFFFFF00FFFFFF00FF00
      000099999900FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF009900
      9900FFFFFF00FFFFFF0000000000000000000000000099000000990000009900
      0000990000009900000099000000990000000000000000000000990000009900
      0000990000009900000000000000000000000000000000000000FFFFFF009999
      990000000000FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF009900
      0000990000000000000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF0099990000FFFFFF00FFFFFF0000990000CCCCCC00FFFFFF00CCCC
      CC00FFFFFF00CCCCCC00009900000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000000000000000000000990000009900
      0000990000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0099000000990000009900000099000000990000009900
      0000FFFFFF000000000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF009999000099990000FFFFFF000099000000990000CCCCCC00FFFF
      FF00CCCCCC00FFFFFF00009900000000000000000000FFFFFF00FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000000000000000000000990000009900
      0000000000000000000000000000000000000000000000000000FFFFFF009999
      9900000000000000000000000000000000000000000000000000999999009999
      9900FFFFFF000000000000000000000000000000000099990000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000099000000990000009900000099000000990000CCCC
      CC00FFFFFF00CCCCCC00009900000000000000000000FFFFFF00FFFFFF00FF00
      000099999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000000000000000000000990000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000099990000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000099000000990000FFFFFF00FFFF
      FF00CCCCCC0000990000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF009999
      990099999900FFFFFF009999990099999900FFFFFF009999990000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000009999
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000990000FFFFFF00FFFFFF00FFFF
      FF000099000000000000000000000000000000000000FFFFFF00FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FF000000FF000000FF000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099990000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009999
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FF00
      000099999900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999000099990000999900009999000099990000999900000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099000000990000009900000099000000990000009900
      0000990000009900000099000000990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000000000000000000000000
      0000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000000000000000000000000
      0000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000000000000000000000000
      0000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000000000000000000009900
      0000990000009900000099000000990000009900000099000000990000009900
      000099000000990000009900000099000000000000000000000000FFFF0000FF
      FF00000000000000000099000000990000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000000000000000000009900
      0000FFFFFF00FFFFFF0099000000990000009900000099000000990000009900
      00009900000099000000FFFFFF00990000000099990000FFFF00000000000000
      0000FF000000FF000000FF0000000000FF000000FF00FFFFFF00FFFFFF000000
      FF00FFFFFF00FFFFFF000000FF000000FF000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000FFFFFF009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000000000000000000009900
      0000FFFFFF00FFFFFF0099000000990000009900000099000000990000009900
      00009900000099000000990000009900000000999900FFFFFF00000000000000
      0000FF000000FF000000000000000000FF000000FF000000FF00FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF000000FF000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000000000000000000009900
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00990000000000000000000000000000000099990000FFFF0000FFFF000000
      0000FF00000000000000000000000000FF000000FF00FFFFFF00FFFFFF000000
      FF00FFFFFF00FFFFFF000000FF000000FF000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      00009900000000000000000000000000000000999900FFFFFF0000FFFF0000FF
      FF000000000000FFFF0000000000009900000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF009900
      000099000000990000009900000099000000990000009900000099000000FFFF
      FF00990000000000000000000000000000000000000000999900FFFFFF0000FF
      FF0000FFFF0000FFFF0000000000009900000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009900000000000000000000000000000099000000FFFFFF009900
      0000990000009900000099000000990000009900000099000000990000009900
      000099000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF0000FFFF0000000000FF00000000990000009999000000FF000000
      FF000000FF0000FFFF0000FFFF00000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000099000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00990000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000FF000000FF0000000099990000999900FF00
      000000999900FFFFFF0000FFFF00000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000FFFFFF009900000000000000000000000000000099000000990000009900
      00009900000099000000FFFFFF00990000009900000099000000990000009900
      0000FFFFFF009900000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000000000
      0000000000000000000000000000000000000099990000999900009999000099
      990000999900009999000000000099990000FF00000000999900FF0000000099
      00000000000000999900FFFFFF00000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000000000000000000000000000099000000990000009900
      00009900000099000000990000009900000099000000FFFFFF00990000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900CCCCCC00CCCCCC00999999009999000099990000FF00000000990000FF00
      0000000000000099990000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099000000990000009900
      0000990000009900000099000000990000009900000099000000990000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009999990099999900CCCCCC00FF000000FF00000099990000999999000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099999900999999009999990099999900000000000000
      0000009999000099990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099999900000000000000
      0000999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC000000
      0000CCCCCC0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000099999900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000099999900CCCCCC00CCCCCC009999
      99000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC0000FFFF0000FFFF0000FFFF00CCCCCC00CCCC
      CC000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000099999900CCCCCC00CCCCCC00FFFF00009999
      99009999990000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000099999900000000000000
      000099999900999999009999990099999900FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00999999009999990099999900CCCCCC00CCCC
      CC0000000000CCCCCC00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000CCCCCC00CCCCCC00CCCCCC00CCCCCC009999
      9900CCCCCC0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC00CCCCCC000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000CCCCCC00FFFF0000CCCCCC00CCCCCC009999
      9900CCCCCC0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF00000000009999990099999900FFFFFF009999990099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC000000
      0000CCCCCC0000000000CCCCCC000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000099999900FFFF0000FFFF0000CCCCCC009999
      99009999990000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000CCCCCC0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC0000000000CCCCCC00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000099999900CCCCCC00CCCCCC009999
      99000000000000000000000000000000000000000000FFFFFF0000000000CCCC
      CC0000000000FFFFFF0000000000CCCCCC0000000000CCCCCC00000000000000
      000000000000000000009900000099000000000000000000000000FFFF00FFFF
      FF00000000009999990099999900FFFFFF009999990099999900FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000CCCCCC0000000000CCCCCC000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000CCCCCC0000000000CCCCCC0000000000CCCCCC0000000000CCCCCC00CCCC
      CC00CCCCCC000000000099000000990000000000000000000000FFFFFF0000FF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC0000000000CCCCCC0000000000CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC0099000000990000000000000099999900000000000000
      000099999900999999009999990099999900FFFFFF009999990099999900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CCCCCC0000000000CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC00CCCCCC009900000099000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000CCCCCC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00CCCCCC000000000099000000990000000000000000000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000099000000990000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099000000990000000000000000000000990000009900
      0000990000009900000099000000990000009900000099000000990000009900
      0000990000009900000099000000990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00000000000000000099000000990000009900000099000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000099
      9900000000000000000000000000000000000000000000000000CCCCCC00CCCC
      CC00000000000099990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000099990000FFFF00000000000000
      0000FF000000FF000000FF00000000990000FF000000FF000000FF0000009900
      0000000000000000000000000000000000000000000000000000009999000099
      9900009999000099990000999900009999000099990000999900009999000000
      0000000000000000000000000000000000000000000000000000009999000099
      9900000000000000000000000000000000000000000000000000CCCCCC00CCCC
      CC00000000000099990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000999900FFFFFF00000000000000
      0000FF000000FF00000000000000009900000099000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000099
      9900009999000099990000999900009999000099990000999900009999000099
      9900000000000000000000000000000000000000000000000000009999000099
      9900000000000000000000000000000000000000000000000000CCCCCC00CCCC
      CC00000000000099990000000000000000000000000000000000000000009900
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000099990000FFFF0000FFFF000000
      0000FF000000000000000000000000990000009900000099990000FFFF0000FF
      FF0000FFFF0000FFFF00000000000000000000000000FFFFFF0000FFFF000000
      0000009999000099990000999900009999000099990000999900009999000099
      9900009999000000000000000000000000000000000000000000009999000099
      9900000000000000000000000000000000000000000000000000000000000000
      0000000000000099990000000000000000000000000000000000000000009900
      0000000000000000000000000000000000000000000099000000990000009900
      00009900000099000000000000000000000000999900FFFFFF0000FFFF0000FF
      FF000000000000FFFF0000000000009900000099000000999900FFFFFF0000FF
      FF0000FFFF000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000099990000999900009999000099990000999900009999000099
      9900009999000099990000000000000000000000000000000000009999000099
      9900009999000099990000999900009999000099990000999900009999000099
      9900009999000099990000000000000000000000000000000000990000000000
      0000000000000000000000000000000000000000000000000000990000009900
      0000990000009900000000000000000000000000000000999900FFFFFF0000FF
      FF0000FFFF0000FFFF000000000000990000009900000099990000FFFF00FFFF
      FF0000FFFF0000FFFF00000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000099
      9900000000000000000000000000000000000000000000000000000000000000
      0000009999000099990000000000000000000000000000000000990000000000
      0000000000000000000000000000000000000000000000000000000000009900
      000099000000990000000000000000000000000000000000000000000000FFFF
      FF0000FFFF0000FFFF0000000000FF0000000099000000999900FFFFFF000099
      9900FFFFFF0000FFFF0000FFFF00000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000099990000000000000000000000000000000000990000000000
      0000000000000000000000000000000000000000000000000000990000000000
      0000990000009900000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000FF000000FF0000000099990000999900FF00
      000000999900FFFFFF0000FFFF000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000099990000000000000000000000000000000000000000009900
      0000000000000000000000000000000000009900000099000000000000000000
      0000000000009900000000000000000000000099990000999900009999000099
      990000999900009999000000000099990000FF00000000999900FF0000000099
      00000000000000999900FFFFFF00000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000099990000000000000000000000000000000000000000000000
      0000990000009900000099000000990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900CCCCCC00CCCCCC00999999009999000099990000FF00000000990000FF00
      0000000000000099990000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000099990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009999990099999900CCCCCC00FF000000FF00000099990000999999000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099999900999999009999990099999900000000000000
      0000009999000099990000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC0000000000CCCCCC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009900000099000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099000000990000009900000099000000990000009900
      0000990000009900000099000000990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000990000000000000000000000990000000000000000000000990000009900
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000990000009900000099000000990000009900
      0000990000009900000099000000990000000000000000000000000000000000
      0000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000990000000000000000000000990000000000000099000000000000000000
      0000990000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000099999900009999009999
      9900009999009999990099000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000990000000000000000000000990000000000000099000000000000000000
      0000990000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00990000000000000000999900999999000099
      9900999999000099990099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009900000099000000990000000000000099000000000000000000
      0000990000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000099999900009999009999
      9900009999009999990099000000FFFFFF00000000000000000000000000FFFF
      FF00990000009900000099000000990000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000990000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000990000000000000099000000990000009900
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00990000000000000000999900999999000099
      9900999999000099990099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0099000000FFFFFF0099000000000000000000000000000000990000009900
      0000990000009900000099000000000000000000000000000000000000000000
      0000990000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000990000000000000099000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00990000000000000099999900009999009999
      9900009999009999990099000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00990000009900000000000000000000000000000000000000990000009900
      0000990000009900000000000000000000000000000000000000000000000000
      0000000000009900000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF000000000000000000FFFF
      FF00990000009900000099000000990000000000000000999900999999000099
      9900999999000099990099000000990000009900000099000000990000009900
      0000990000000000000000000000000000000000000000000000990000009900
      0000990000000000000000000000000000000000000000000000000000000000
      0000000000009900000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0099000000FFFFFF0099000000000000000000000099999900009999009999
      9900009999009999990000999900999999000099990099999900009999009999
      9900009999000000000000000000000000000000000000000000990000009900
      0000000000009900000000000000000000000000000000000000000000000000
      0000000000009900000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0099000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00990000009900000000000000000000000000000000999900999999000000
      0000000000000000000000000000000000000000000000000000000000009999
      9900999999000000000000000000000000000000000000000000990000000000
      0000000000000000000099000000990000000000000000000000000000000000
      0000990000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000990000009900000099000000990000009900
      0000990000000000000000000000000000000000000099999900999999000000
      0000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00000000009999
      9900009999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009900000099000000990000009900
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000999900999999000099
      99000000000000FFFF00000000000000000000FFFF0000000000999999000099
      9900999999000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000A00100000100010000000000000D00000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFF0000FC7FF800C0030000
      F81FF000C0030000F007E000C0030000E001C000C0030000C0008000C0030000
      80018000C003000000008000C003000000008000C003000000018000C0030000
      00038000C003000000038001C0030000800F8307C0030000E01F83C7C0070000
      F83FC7FFC00F0000FE7FEFFFC01F0000FEF7FFFFFFFFFFFFFC63F00FFFFFFFFF
      F801F00FFFFFFFFF8003F00FFFFFFFFF8007F00FFFFFFFFF8007F81FFFFFFFFF
      8003F00FE00FFEFFC001F00FF01FFC7F8063F00FF83FF83F8077C00FFC7FF01F
      807FC00FFEFFE00F807F800FFFFFFFFF807FC00FFFFFFFFF807FC1FFFFFFFFFF
      807FF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF7FFFFC001FEFFFC63FEFF
      8000FC7FF801FC7F8000807F8003807F8000803F8007803F8000801F8007801F
      8000805F8003805F8000C0EFC001C0EF80008077806380778000807B8077807B
      8000807D807F807D8000807F807F807F8001807F807F807F8001807F807F807F
      8001807F807F807FFFFFFFFFFFFFFFFFFFFFE000FFFFFFFFFFFFE000FFFFFF83
      800FE000FFFFFE01800FE00087FFFC00800FE00081FFFC008007E000C07FF800
      8003E000E03FE0008001C000F01F800080038000F00F000080070000E01F0000
      80070000F00F000180030000FC070047F8010000FF0305FFFC63807FFFC707FF
      FEF7C0FFFFFFCFFFFFFFE1FFFFFFFFFFFC01FFFFFFFFFFFFFC01FFBFFFFFFFFF
      FC01FF3FFFFFFFFFFC01FE3FC77FFBFFFC01FC3FC73F89FB8001FC3FC71F8CFB
      8001FC3FFF0F8CE78001FC3FF107FE678001FC3FF10FE20F8001F81FC11FE31F
      8001F00FC73F821F8001E007C77F88CF800FC003FFFF8FE7800F8001FFFFFFF3
      800F8001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC003FFFFFFFFFFFFC003
      833FFFDFFBFFC003FF1FFF9FF9FFC003878FFF1FF8FFC003FF07FE1FF87FC003
      8383FC1FF83FC003FE01F81FF81FC0038600F01FF80FC003FF07F81FF81FC003
      8383FC1FF83FC003FF81FE1FF87FC00383C0FF1FF8FFC003FFE0FF9FF9FFC003
      FFFFFFDFFBFFC003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0FFF81FFFFF
      800FC03FF00FF3FF800F801FE007E0FF800F801FC003C07F800F801F8001807F
      8007800F80010C3F8007800F80011E1F800180038001FF0F800380078001FF87
      800180038001FFC38001C003C003FFE3FF03E007E007FFF1FE49FC93F00FFFF8
      FECFFD9FF81FFFFCFFCFFF9FFFFFFFFEFFFFFFFFFFFFFFFFFFCFFFFFFFFFF0FF
      FF87FFFFFFFFC03FFF035FFF7FF8801FFE01FFF83F78801FFC00ABF89F7F801F
      FE01F9FF9CF88017FF0358F8CCF88003FD87F878C1F88001F8C7F878E3F88003
      4047FC38C3F88007000FFC3819F8C007001FFE18FCFFE003003FFE3FFE7FF801
      007FFFFFFFFFFC6340FFFFFFFFFFFEF7FFFFFFFFFFFFFFFFFFF3FFFFFFCFFFCF
      FFEDFFF7FF87FF87FFEDFFF7FF03FF03FCEDFCF3FE01FE01F86DF877FC007C00
      F03FF031FE013E01E01FE01FFF039F03C00FC00FC3879C87E01FE01FC3CFCCCF
      F03FF03F00FFC1FFF87FF87F00FFE3FFFCFFFCFF00FFC3FFFFFFFFFF00FF19FF
      FFFFFFFFC3FFFCFFFFFFFFFFC3FFFE7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB
      FFFFFFFFFFFFFFFBF01FF01FFFFFFFFBF01FF01FFCFFFCFBC00FC00FF87FF87F
      C00FC00FF03FF03BC007C007E01FE01FC007C007C00FC00FD00FD00FE01FE01F
      C80FC80FF03FF03FE40FE40FF87FF87FF207F207FCFFFCFFF80FF80FFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFDFFFFBF
      FFFFF87FFCFFFF9FFFFFF07FFC7FFF8FFC0FE03FFC3FFF87FC0FC31FFC1FF883
      FC0FE79FFC0FFF81C00FFFCFFC07F880C00FF9E7FC0FDF81C00F1313FC1F0883
      C0FF5579FC3FDF87C0FFD57CFC7FF88FC0FF357EFCFFFF9FFFFF557FFDFFFFBF
      FFFF117FFFFFFFFFFFFFFFFFFFFFFFFFE00FFFFFFFFFF001E00FE001FFFFF001
      E00FC001FFFFF001E00F8001FFFFF001E0008001FFFFF001E0008001FDFF0001
      E000C001F9FF0001E000C001F01F0001E000C001F9DF00016000C001FDDF007F
      3E00C001FFDF00771E00C001F81F00630E00C001FFFF00411E00C001FFFF00F7
      3FFFC003FFFF018F7FFFC007FFFF03FF8003E003FFFFB6E70001E001FFFFB76B
      0001E00007C184270001000007C1B76B0001000007C1CEE7000100000101FFFF
      000100000001C7C70001003F0001C7C70001003B0001C387000100318003C007
      00010020C107C0070001003BC107C0070001007BE38FC007000300F7E38FC007
      0007018FE38FF39F800FFFFFFFFFF39FFFFFFFFFFF8FFFFFFBFFFFBFDC89FFFF
      E9FFFE9F8800FFFFE0FF820F0400801FE07FFE07F800FFFFE03F8203F800801D
      E01FFE019000FFF9E00F820040018011E01FFE010001FFF9E03F8203001B801D
      E07FFE07001FFFFFE0FF820F001F801FE1FFFE1F803FFFFFE3FF823F803FFFFF
      EFFFFEFFD17FFFFFFFFFFFFFFBFFFFFFFFFFF80FFFFFFFFFFFFFF003FFFFFFFF
      E001E001FC3FFC3FC001C000F00FF00FC0018000E007E007C0018000E007E007
      C0010000C003C003C0010000C003C003C0010000C003C003C0010000C003C003
      C0010000E007E007C0018000E007E007C0018000F00FF00FC003C000FC3FFC3F
      FFFFE000FFFFFFFFFFFFF801FFFFFFFF80FFFC3FFC3FFFFF80FFF00FF00FFFFF
      80FFE007E007E00180FFE007E007C0018080E007E007C0018080E007E007C001
      8080E007E007C001FF80E007E007C001FF80E007E007C0018080E007E007C001
      8080E007E007C00180FFE007E007C00180FFE007E007C00180FFF00FF00FC003
      80FFF81FF81FFFFF80FFFFFFFFFFFFFFFFFFFFFFFFFF80FFFFFFFFCFFFCF80FF
      FFFFFF87FF8780FF5FFFFF03FF0380FFFFF8FE01FE018080ABF87C00FC008080
      F9FF3E01FE01808058F89F03FF03FF80F8789C87C387FF80F878CCCFC3CF8080
      FC38C1FF00FF8080FC38E3FF00FF80FFFE18C3FF00FF80FFFE3F19FF00FF80FF
      FFFFFCFFC3FF80FFFFFFFE7FC3FF80FFFFFFFC7FFFFFFFFF8007F01FFE01FFFF
      8007C007FE01800180078001FE01800180078001FE01800180070001C0018001
      80070000C003800180070000000F800180070000007F800180070000007F8001
      FFFF8001007B8001ADB4E007007180018DB5F80300608001AD85FE00007B8001
      A8B5FF0301C7FFFFFFFFFF8F01FFFFFFF001FF1FFFFFFFFFE000FE0FFFFFFF9F
      E000C007FFFFFF1FE0000001F783FE1FE0000000E3FFFC1FE0000000E1FFF007
      E0008000E383C003C000C000E1FF800180008001E3FF80010000001FC1830001
      0000000F80FF00010000001F80FF00010000001F808380018000000FC1FF8003
      C001800FFFFFC007E0ABE01FFFFFF01FFFFFFFFFFFFFFFFFFFFFFEFFF0FFFFFF
      FFFFDEF7C03FDFFBC01FFE07801F8FFFC01FF403801F87F7C00FF803801FC7EF
      C00FFC038017E3CFC003A0018003F19FC007FC038001F83FC003FA078003FC7F
      C003F6DF8007F83FFE07FEFFC007F19FFC93DEF7E003C3CFFD9FFFFFF80187E7
      FF9FFEFFFC638FFBFFFFFFFFFEF7FFFFFFFFFC01FFFFFF87FFFFF001F81FFF03
      FFFF8001F00FFE01FFFF8001E0070000FFFF8001C003000080DF800180010000
      80CF80018001000080C780038001000180C380038001000380C7800380010007
      80CF80038001000780DF8003C0030007FFFF8007E0070007FFFF800FF00F0007
      FFFF801FF81F0007FFFFFFFFFFFF0007FFFFFFFFFC00FFFF80038003FC00FFFF
      80038003FC00FFC780038003FC00CC0180038003E000800180038003E0000000
      80038003E000000080038003E007000080038003800700018003800380078001
      800380038007C00080038003801F800080038003801F000080038003801FE000
      FFFFFFFF801FF001FFFFFFFFFFFFFC33FFFFFFFF87FFFFFF000C000F87FFC007
      0008000F87FF80030001000F800000010003000FC00000010003000F80000001
      0003000F800000000003000F800000000003000F80008000000700048000C000
      000F00008000E001000F00008000E007000FF800C000F007001FFC00C000F003
      003FFE04C000F803007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCC3FFFFFC001
      FFFF800F001F8001FFFF0007000F8001FFFF000000078001EFFF000100038001
      EF83000300018001DFC3800100008001DFE3C000001F8001DFD38000001F8001
      EF3B0000001F8001F0FFE0008FF18001FFFFF001FFF98001FFFFFC33FF758001
      FFFFFFFFFF8F8001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FFFFFFFC00FFFF
      F6CFFE008000FFFFF6B7FE000000FFFFF6B7FE000000FFFFF8B780000000FFF7
      FE8F80000001C1F7FE3F80000003C3FBFF7F80000003C7FBFE3F80010003CBFB
      FEBF80030003DCF7FC9F80070003FF0FFDDF807F0003FFFFFDDF80FF8007FFFF
      FDDF81FFF87FFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 40
    Top = 120
  end
  object SQLFunctions: TPopupMenu
    Images = ImageList1
    OnPopup = SQLFunctionsPopup
    Left = 40
    Top = 88
    object MenuRun: TMenuItem
      Action = ExecuteQuery
    end
    object MenuRunSelection: TMenuItem
      Action = ExecuteSelection
    end
    object MenuRunLine: TMenuItem
      Action = ExecuteLine
      Enabled = False
    end
    object MenuSetFilter: TMenuItem
      Caption = 'Set Filter'
      ImageIndex = 10
      Visible = False
      OnClick = MenuSetFilterClick
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object menucopy: TMenuItem
      Action = EditCopy1
    end
    object menupaste: TMenuItem
      Action = EditPaste1
    end
    object menuload: TMenuItem
      Caption = 'Load from File...'
      ImageIndex = 6
    end
    object menusave: TMenuItem
      Caption = 'Save to file...'
      ImageIndex = 7
    end
    object menuclear: TMenuItem
      Caption = 'Clear'
      ShortCut = 16471
      OnClick = menuclearClick
    end
    object MenuFind: TMenuItem
      Caption = 'Find...'
      ImageIndex = 50
      ShortCut = 16454
      OnClick = MenuFindClick
    end
    object MenuReplace: TMenuItem
      Caption = 'Replace ...'
      Hint = 'Search and replace...'
      ImageIndex = 51
      ShortCut = 16466
      OnClick = MenuReplaceClick
    end
    object N12: TMenuItem
      Caption = '-'
    end
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'reg'
    Filter = 'Registry-files (*.reg)|*.reg|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Export MySQL-Front settings...'
    Left = 8
    Top = 56
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'reg'
    Filter = 'Registry-files (*.reg)|*.reg|All files (*.*)|*.*'
    Title = 'Import MySQL-Front settings...'
    Left = 40
    Top = 56
  end
  object menuConnections: TPopupMenu
    Images = ImageList1
    OnPopup = menuConnectionsPopup
    Left = 72
    Top = 88
    object miNewConnection: TMenuItem
      Caption = 'New Connection...'
      SubMenuImages = ImageList1
      Default = True
      ImageIndex = 93
      OnClick = ShowConnections
    end
  end
end
