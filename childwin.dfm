object MDIChild: TMDIChild
  Left = 646
  Top = 110
  Caption = 'MySQL-Host'
  ClientHeight = 420
  ClientWidth = 677
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  ParentFont = True
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 0
    Top = 274
    Width = 677
    Height = 4
    Cursor = crSizeNS
    Align = alBottom
    AutoSnap = False
    Beveled = True
    ResizeStyle = rsUpdate
    OnMoved = Splitter2Moved
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 677
    Height = 274
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 169
      Top = 0
      Width = 4
      Height = 274
      Cursor = crSizeWE
      Beveled = True
      ResizeStyle = rsUpdate
    end
    object DBtree: TTreeView
      Left = 0
      Top = 0
      Width = 169
      Height = 274
      Align = alLeft
      ChangeDelay = 50
      Constraints.MinWidth = 40
      DragMode = dmAutomatic
      HotTrack = True
      Images = MainForm.ImageList1
      Indent = 19
      PopupMenu = popupTreeView
      ReadOnly = True
      RightClickSelect = True
      RowSelect = True
      ShowRoot = False
      TabOrder = 0
      OnChange = DBtreeChange
    end
    object TableShow: TPanel
      Left = 173
      Top = 0
      Width = 504
      Height = 274
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object PageControl1: TPageControl
        Left = 0
        Top = 0
        Width = 504
        Height = 274
        ActivePage = SheetTable
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        HotTrack = True
        Images = MainForm.ImageList1
        MultiLine = True
        ParentFont = False
        TabHeight = 22
        TabOrder = 0
        OnChange = pcChange
        object SheetHost: TTabSheet
          Caption = 'Host'
          ImageIndex = 41
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageControl2: TPageControl
            Left = 0
            Top = 17
            Width = 496
            Height = 225
            ActivePage = TabSheet6
            Align = alClient
            HotTrack = True
            TabOrder = 0
            OnChange = PageControl2Change
            object TabSheet6: TTabSheet
              Caption = 'Variables'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object ListVariables: TSortListView
                Tag = -1
                Left = 0
                Top = 0
                Width = 488
                Height = 197
                Align = alClient
                Columns = <
                  item
                    Caption = 'Variable'
                    Tag = 63
                    Width = 130
                  end
                  item
                    Caption = 'Value'
                    Tag = 64
                    Width = 354
                  end>
                GridLines = True
                ReadOnly = True
                RowSelect = True
                PopupMenu = popupHost
                TabOrder = 0
                ViewStyle = vsReport
              end
            end
            object TabSheet7: TTabSheet
              Caption = 'Process-List'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object ListProcesses: TSortListView
                Tag = -1
                Left = 0
                Top = 0
                Width = 488
                Height = 197
                Align = alClient
                Columns = <
                  item
                    Caption = 'id'
                    Tag = 65
                    Width = -1
                    WidthType = (
                      -1)
                  end
                  item
                    Caption = 'User'
                    Tag = 66
                    Width = 80
                  end
                  item
                    Caption = 'Host'
                    Tag = 67
                    Width = 80
                  end
                  item
                    Caption = 'DB'
                    Tag = 68
                    Width = -1
                    WidthType = (
                      -1)
                  end
                  item
                    Caption = 'Command'
                    Tag = 69
                    Width = 80
                  end
                  item
                    Caption = 'Time'
                    Tag = 70
                  end
                  item
                    Caption = 'State'
                    Tag = 71
                  end
                  item
                    Caption = 'Info'
                    Tag = 72
                    Width = 126
                  end>
                GridLines = True
                ReadOnly = True
                RowSelect = True
                PopupMenu = popupHost
                SmallImages = MainForm.ImageList1
                TabOrder = 0
                ViewStyle = vsReport
                OnChange = ListProcessesChange
              end
            end
          end
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 17
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'Host'
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
        end
        object SheetDatabase: TTabSheet
          Caption = 'Database'
          ImageIndex = 38
          DesignSize = (
            496
            242)
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 17
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'Database'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object ListTables: TSortListView
            Tag = -1
            Left = 30
            Top = 17
            Width = 466
            Height = 218
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Table'
                Tag = 73
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Alignment = taRightJustify
                Caption = 'Records'
                Tag = 74
                Width = 80
              end
              item
                Alignment = taRightJustify
                Caption = 'Size'
                Tag = 75
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Caption = 'Created'
                Tag = 76
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Caption = 'Updated'
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Caption = 'Type'
                Tag = 77
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Caption = 'Comment'
                Tag = 78
                Width = 328
              end>
            GridLines = True
            MultiSelect = True
            RowSelect = True
            ParentShowHint = False
            PopupMenu = popupDbGrid
            ShowHint = False
            SmallImages = MainForm.ImageList1
            TabOrder = 1
            ViewStyle = vsReport
            OnChange = ListTablesChange
            OnColumnRightClick = ListTablesColumnRightClick
            OnDblClick = ListTablesDblClick
            OnEdited = ListTablesEdited
            OnEditing = ListTablesEditing
            OnMouseDown = ListTablesMouseDown
          end
          object ToolBar1: TToolBar
            Left = 3
            Top = 20
            Width = 26
            Height = 146
            Align = alNone
            Caption = 'ToolBar1'
            Color = clBtnFace
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = MainForm.ImageList1
            ParentColor = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            Transparent = True
            object btnDbViewData: TToolButton
              Left = 0
              Top = 0
              Hint = 'View Data'
              Caption = 'View Data'
              Enabled = False
              ImageIndex = 8
              Wrap = True
              OnClick = viewdata
            end
            object btnDbProperties: TToolButton
              Left = 0
              Top = 22
              Hint = 'Show Table-Properties'
              Caption = 'Show Table-Properties'
              Enabled = False
              ImageIndex = 9
              Wrap = True
              OnClick = btnDbPropertiesClick
            end
            object btnDbEmptyTable: TToolButton
              Left = 0
              Top = 44
              Hint = 'Empty Table ...'
              Caption = 'btnDbEmptyTable'
              Enabled = False
              ImageIndex = 31
              Wrap = True
              OnClick = TabelleLeeren
            end
            object btnDbDropTable: TToolButton
              Left = 0
              Top = 66
              Action = MainForm.DropTable
              Wrap = True
            end
            object btnDbCopyTable: TToolButton
              Left = 0
              Top = 88
              Action = MainForm.CopyTable
              Wrap = True
            end
          end
          object ToolBar5: TToolBar
            Left = 3
            Top = 153
            Width = 23
            Height = 29
            Align = alNone
            Caption = 'ToolBar5'
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = MainForm.ImageList1
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            Transparent = True
            object btnDbInsertRecord: TToolButton
              Left = 0
              Top = 0
              Hint = 'Insert Record...|Insert new Record into Table...'
              Caption = 'Insert Record'
              Enabled = False
              ImageIndex = 32
              OnClick = InsertRecord
            end
          end
        end
        object SheetTable: TTabSheet
          Caption = 'Table'
          ImageIndex = 40
          DesignSize = (
            496
            242)
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 17
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'Table-Properties'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object ToolBar2: TToolBar
            Left = 3
            Top = 20
            Width = 26
            Height = 146
            Align = alNone
            ButtonHeight = 23
            Caption = 'ToolBar2'
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = MainForm.ImageList1
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            Transparent = True
            Wrapable = False
            object btnTableViewData: TToolButton
              Left = 0
              Top = 0
              Hint = 'View Data'
              Caption = 'btnTableViewData'
              ImageIndex = 8
              Wrap = True
              OnClick = viewdata
            end
            object btnTableEditField: TToolButton
              Left = 0
              Top = 23
              Hint = 'Edit Field / Index...'
              Caption = 'btnTableEditField'
              ImageIndex = 9
              Wrap = True
              OnClick = UpdateField
            end
            object btnTableAddField: TToolButton
              Left = 0
              Top = 46
              Hint = 'Add Field / Index...'
              Caption = 'btnTableAddField'
              ImageIndex = 34
              Wrap = True
              OnClick = UpdateField
            end
            object btnTableDropField: TToolButton
              Left = 0
              Top = 69
              Hint = 'Drop Field ...'
              Caption = 'btnTableDropField'
              ImageIndex = 33
              Wrap = True
              OnClick = DropField
            end
            object btnTableManageIndexes: TToolButton
              Left = 0
              Top = 92
              Hint = 'Manages indexes'
              Caption = 'btnTableManageIndexes'
              ImageIndex = 76
              OnClick = ManageIndexes1Click
            end
          end
          object Panel9: TPanel
            Left = 32
            Top = 17
            Width = 465
            Height = 218
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 2
            object ListColumns: TSortListView
              Tag = -1
              Left = 0
              Top = 0
              Width = 465
              Height = 218
              Align = alClient
              Columns = <
                item
                  Caption = 'Name'
                  Tag = 79
                  Width = -1
                  WidthType = (
                    -1)
                end
                item
                  Caption = 'Type'
                  Tag = 80
                  Width = 100
                end
                item
                  Caption = 'Null'
                  Tag = 81
                  Width = 40
                end
                item
                  Caption = 'Default'
                  Tag = 80
                  Width = 115
                end
                item
                  Caption = 'Extra'
                  Tag = 83
                  Width = 200
                end>
              GridLines = True
              ReadOnly = True
              RowSelect = True
              PopupMenu = popupTableGrid
              SmallImages = MainForm.ImageList1
              TabOrder = 0
              ViewStyle = vsReport
              OnChange = ListColumnsChange
              OnDblClick = UpdateField
            end
          end
          object ToolBar6: TToolBar
            Left = 2
            Top = 143
            Width = 23
            Height = 39
            Align = alNone
            ButtonHeight = 23
            Caption = 'ToolBar2'
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = MainForm.ImageList1
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            Transparent = True
            Wrapable = False
            object btnTableInsertRecord: TToolButton
              Left = 0
              Top = 0
              Hint = 'Insert Record...|Insert new Record into Table...'
              Caption = 'btnTableInsertRecord'
              ImageIndex = 32
              Wrap = True
              OnClick = InsertRecord
            end
          end
        end
        object SheetData: TTabSheet
          Caption = 'Data'
          ImageIndex = 39
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 23
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'Data'
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            DesignSize = (
              496
              23)
            object EditDataSearch: TEdit
              Left = 317
              Top = 0
              Width = 121
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnEnter = EditDataSearchEnter
              OnExit = EditDataSearchExit
            end
            object ButtonDataSearch: TButton
              Left = 440
              Top = 0
              Width = 51
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Search'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              OnClick = ButtonDataSearchClick
            end
          end
          object gridData: TSMDBGrid
            Left = 0
            Top = 23
            Width = 496
            Height = 219
            Align = alClient
            DataSource = DataSource1
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
            ParentFont = False
            PopupMenu = popupDataGrid
            TabOrder = 1
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            OnColEnter = DBGridColEnter
            OnColumnMoved = gridDataColumnMoved
            OnDblClick = DBGridDblClick
            OnEnter = DBGridEnter
            OnExit = DBGridExit
            OnTitleClick = gridDataTitleClick
            ExOptions = [eoBooleanAsCheckBox, eoCheckBoxSelect, eoCellHint, eoENTERlikeTAB, eoKeepSelection]
            OnGetCellParams = DBGridGetCellParams
            RegistryKey = 'Software\MikeSoft'
            RegistrySection = 'SMDBGrid'
            WidthOfIndicator = 23
            ScrollBars = ssHorizontal
            ColCount = 2
            RowCount = 2
            Col = 1
            Row = 1
          end
        end
        object SheetQuery: TTabSheet
          Caption = 'Query'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 57
          ParentFont = False
          object Splitter3: TSplitter
            Left = 0
            Top = 125
            Width = 496
            Height = 4
            Cursor = crSizeNS
            Align = alTop
            Beveled = True
            ResizeStyle = rsUpdate
          end
          object LabelResultinfo: TLabel
            Left = 0
            Top = 146
            Width = 496
            Height = 13
            Align = alTop
            ExplicitWidth = 3
          end
          object Panel6: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 29
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'SQL-Query:'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            object Panel8: TPanel
              Left = 288
              Top = 1
              Width = 207
              Height = 27
              Align = alRight
              BevelOuter = bvNone
              BorderWidth = 1
              Color = clWindow
              TabOrder = 0
              object ToolBarQuery: TToolBar
                Left = 1
                Top = 1
                Width = 205
                Height = 25
                Align = alClient
                ButtonHeight = 25
                Caption = 'Query'
                Color = clBtnFace
                DragKind = dkDock
                EdgeInner = esNone
                EdgeOuter = esNone
                Images = MainForm.ImageList1
                ParentColor = False
                ParentShowHint = False
                ShowHint = True
                TabOrder = 0
                Transparent = True
                Wrapable = False
                object btnQueryRun: TToolButton
                  Left = 0
                  Top = 0
                  Action = MainForm.ExecuteQuery
                end
                object btnQueryRunSelected: TToolButton
                  Left = 23
                  Top = 0
                  Action = MainForm.ExecuteSelection
                end
                object btnQueryLoad: TToolButton
                  Left = 46
                  Top = 0
                  Hint = 'Load SQL from Textfile'
                  Caption = 'Load SQL...'
                  DropdownMenu = popupQueryLoad
                  ImageIndex = 6
                  Style = tbsDropDown
                  OnClick = btnQueryLoadClick
                end
                object btnQuerySave: TToolButton
                  Left = 82
                  Top = 0
                  Hint = 'Save SQL to Textfile'
                  Caption = 'Save SQL...'
                  Enabled = False
                  ImageIndex = 7
                  OnClick = btnQuerySaveClick
                end
                object btnQuerySaveSnippet: TToolButton
                  Left = 105
                  Top = 0
                  Hint = 'Save SQL as snippet...'
                  Caption = 'btnQuerySaveSnippet'
                  ImageIndex = 88
                  OnClick = btnQuerySaveSnippetClick
                end
                object btnQueryFind: TToolButton
                  Left = 128
                  Top = 0
                  Hint = 'Find Text...'
                  Caption = 'Find...'
                  ImageIndex = 50
                  OnClick = btnQueryFindClick
                end
                object btnQueryReplace: TToolButton
                  Left = 151
                  Top = 0
                  Hint = 'Search and replace...'
                  Caption = 'Replace ...'
                  ImageIndex = 51
                  OnClick = btnQueryReplaceClick
                end
                object btnQueryStopOnErrors: TToolButton
                  Left = 174
                  Top = 0
                  Hint = 'Stop on MySQL-errors in batch-mode'
                  Caption = 'btnQueryStopOnErrors'
                  Down = True
                  ImageIndex = 47
                  Style = tbsCheck
                  OnClick = btnQueryStopOnErrorsClick
                end
              end
            end
            object PanelCharsInQueryWindow: TPanel
              Left = 172
              Top = 1
              Width = 116
              Height = 27
              Align = alRight
              Alignment = taRightJustify
              BevelOuter = bvNone
              BorderWidth = 2
              Caption = '0 Characters'
              Color = clWindow
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
            end
          end
          object Panel7: TPanel
            Left = 0
            Top = 29
            Width = 496
            Height = 96
            Align = alTop
            BevelOuter = bvNone
            Constraints.MinHeight = 10
            TabOrder = 1
            object SynMemoQuery: TSynMemo
              Left = 0
              Top = 0
              Width = 496
              Height = 96
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Courier New'
              Font.Style = []
              PopupMenu = MainForm.SQLFunctions
              TabOrder = 0
              OnDragDrop = SynMemoQueryDragDrop
              OnDragOver = SynMemoQueryDragOver
              OnKeyUp = SynMemoQueryKeyUp
              OnMouseUp = SynMemoQueryMouseUp
              Gutter.AutoSize = True
              Gutter.Font.Charset = DEFAULT_CHARSET
              Gutter.Font.Color = clWindowText
              Gutter.Font.Height = -11
              Gutter.Font.Name = 'Terminal'
              Gutter.Font.Style = []
              Gutter.LeftOffset = 10
              Gutter.RightOffset = 0
              Gutter.ShowLineNumbers = True
              Highlighter = SynSQLSyn1
              Options = [eoAutoIndent, eoDropFiles, eoGroupUndo, eoShowScrollHint]
              RightEdge = 40
              SearchEngine = SynEditSearch1
              TabWidth = 2
              WantTabs = True
              OnChange = SynMemoQueryChange
              OnDropFiles = SynMemoQueryDropFiles
              RemovedKeystrokes = <
                item
                  Command = ecDeleteLastChar
                  ShortCut = 8200
                end
                item
                  Command = ecLineBreak
                  ShortCut = 8205
                end
                item
                  Command = ecContextHelp
                  ShortCut = 112
                end
                item
                  Command = ecDeleteLine
                  ShortCut = 16473
                end>
              AddedKeystrokes = <>
            end
          end
          object gridQuery: TSMDBGrid
            Left = 0
            Top = 159
            Width = 496
            Height = 83
            Align = alClient
            DataSource = DataSource2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Options = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
            ParentFont = False
            PopupMenu = popupResultGrid
            TabOrder = 2
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            OnColEnter = DBGridColEnter
            OnDblClick = DBGridDblClick
            OnEnter = DBGridEnter
            OnExit = DBGridExit
            ExOptions = [eoBooleanAsCheckBox, eoCellHint, eoENTERlikeTAB, eoKeepSelection]
            OnGetCellParams = DBGridGetCellParams
            RegistryKey = 'Software\MikeSoft'
            RegistrySection = 'SMDBGrid'
            WidthOfIndicator = 11
            ScrollBars = ssHorizontal
            ColCount = 2
            RowCount = 2
            Col = 1
            Row = 1
          end
          object ProgressBarQuery: TProgressBar
            Left = 0
            Top = 129
            Width = 496
            Height = 17
            Align = alTop
            Step = 1
            TabOrder = 3
            Visible = False
          end
        end
      end
    end
  end
  object PageControlBottom: TPageControl
    Left = 0
    Top = 278
    Width = 677
    Height = 142
    ActivePage = TabSheet1
    Align = alBottom
    HotTrack = True
    Images = MainForm.ImageList1
    TabHeight = 20
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'SQL Log'
      ImageIndex = 79
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object SynMemoSQLLog: TSynMemo
        Left = 0
        Top = 0
        Width = 669
        Height = 112
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = popupSqlLog
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Terminal'
        Gutter.Font.Style = []
        Highlighter = SynSQLSyn1
        Options = [eoAutoIndent, eoDragDropEditing, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        RightEdge = 40
        RemovedKeystrokes = <
          item
            Command = ecDeleteLastChar
            ShortCut = 8200
          end
          item
            Command = ecLineBreak
            ShortCut = 8205
          end
          item
            Command = ecContextHelp
            ShortCut = 112
          end
          item
            Command = ecDeleteLine
            ShortCut = 16473
          end>
        AddedKeystrokes = <>
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'BLOB-Editor'
      ImageIndex = 80
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ToolBar3: TToolBar
        Left = 0
        Top = 0
        Width = 23
        Height = 112
        Align = alLeft
        AutoSize = True
        Caption = 'ToolBar1'
        EdgeInner = esNone
        EdgeOuter = esNone
        Images = MainForm.ImageList1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Transparent = True
        Wrapable = False
        object btnBlobWordWrap: TToolButton
          Left = 0
          Top = 0
          Hint = 'Wordwrap'
          Caption = 'ToolButton1'
          ImageIndex = 54
          Wrap = True
          Style = tbsCheck
          OnClick = btnBlobWordWrapClick
        end
        object btnBlobLoad: TToolButton
          Left = 0
          Top = 22
          Hint = 'Open|Open file'
          Caption = 'ToolButton4'
          ImageIndex = 6
          Wrap = True
          OnClick = btnBlobLoadClick
        end
        object btnBlobSave: TToolButton
          Left = 0
          Top = 44
          Hint = 'Save|Save to File'
          Caption = 'ToolButton5'
          ImageIndex = 7
          Wrap = True
          OnClick = btnBlobSaveClick
        end
        object btnBlobViewAsHtml: TToolButton
          Left = 0
          Top = 66
          Action = MainForm.HTMLview
          Wrap = True
        end
        object btnBlobCopy: TToolButton
          Left = 0
          Top = 88
          Hint = 'Copy to clipboard'
          Caption = 'btnBlobCopy'
          ImageIndex = 1
          OnClick = btnBlobCopyClick
        end
      end
      object PageControl4: TPageControl
        Left = 23
        Top = 0
        Width = 646
        Height = 112
        ActivePage = TabSheet3
        Align = alClient
        TabOrder = 1
        OnChange = PageControl4Change
        object TabSheet3: TTabSheet
          Caption = 'Text'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object DBMemo1: TDBMemo
            Left = 0
            Top = 0
            Width = 638
            Height = 84
            Align = alClient
            DataSource = DataSource1
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 0
            OnKeyDown = DBMemo1KeyDown
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'Image'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object ScrollBox1: TScrollBox
            Left = 0
            Top = 0
            Width = 638
            Height = 84
            Align = alClient
            TabOrder = 0
            object EDBImage1: TEDBImage
              Left = 0
              Top = 0
              Width = 89
              Height = 80
              Align = alLeft
              BorderStyle = bsNone
              Color = clBtnFace
              Stretch = True
              TabOrder = 0
            end
          end
        end
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Filter'
      ImageIndex = 81
      object SynMemoFilter: TSynMemo
        Left = 0
        Top = 29
        Width = 669
        Height = 83
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = MainForm.SQLFunctions
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Terminal'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 10
        Gutter.RightOffset = 0
        Gutter.ShowLineNumbers = True
        Highlighter = SynSQLSyn1
        Options = [eoAutoIndent, eoDragDropEditing, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces]
        RightEdge = 40
        RemovedKeystrokes = <
          item
            Command = ecDeleteLastChar
            ShortCut = 8200
          end
          item
            Command = ecLineBreak
            ShortCut = 8205
          end
          item
            Command = ecContextHelp
            ShortCut = 112
          end
          item
            Command = ecDeleteLine
            ShortCut = 16473
          end>
        AddedKeystrokes = <>
      end
      object Panel10: TPanel
        Left = 0
        Top = 0
        Width = 669
        Height = 29
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          669
          29)
        object ComboBoxWhereFilters: TComboBox
          Left = 168
          Top = 2
          Width = 498
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 0
          OnChange = ComboBoxWhereFiltersChange
        end
        object ToolBar4: TToolBar
          Left = 0
          Top = 0
          Width = 159
          Height = 29
          Align = alLeft
          AutoSize = True
          Caption = 'ToolBar1'
          EdgeInner = esNone
          EdgeOuter = esNone
          Images = MainForm.ImageList1
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Transparent = True
          Wrapable = False
          object btnFilterSet: TToolButton
            Left = 0
            Top = 0
            Hint = 'Set Filter'
            Caption = 'Set Filter'
            ImageIndex = 10
            OnClick = setFilter
          end
          object btnFilterLoad: TToolButton
            Left = 23
            Top = 0
            Hint = 'Open File'
            DropdownMenu = popupFilterOpenFile
            ImageIndex = 6
            Style = tbsDropDown
            OnClick = btnFilterLoadClick
          end
          object btnFilterSave: TToolButton
            Left = 59
            Top = 0
            Hint = 'Save|Save to File'
            ImageIndex = 7
            OnClick = btnFilterSaveClick
          end
          object btnFilterClear: TToolButton
            Left = 82
            Top = 0
            Hint = 'Clear Filter'
            Caption = 'Clear'
            ImageIndex = 70
            OnClick = ClearFilter
          end
          object sepFilter1: TToolButton
            Left = 105
            Top = 0
            Width = 8
            Caption = 'sepFilter1'
            ImageIndex = 9
            Style = tbsSeparator
          end
          object btnFilterPrevious: TToolButton
            Left = 113
            Top = 0
            Hint = 'Previous filter'
            Enabled = False
            ImageIndex = 77
            OnClick = btnFilterPreviousClick
          end
          object btnFilterNext: TToolButton
            Left = 136
            Top = 0
            Hint = 'Next filter'
            Enabled = False
            ImageIndex = 78
            OnClick = btnFilterNextClick
          end
        end
      end
    end
  end
  object popupTreeView: TPopupMenu
    Images = MainForm.ImageList1
    OnPopup = popupTreeViewPopup
    Left = 8
    Top = 48
    object NewDatabase1: TMenuItem
      Caption = 'Create Database...'
      ImageIndex = 73
      OnClick = CreateDatabase
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object PopupmenuDropDatabase: TMenuItem
      Caption = 'Drop Database...'
      ImageIndex = 22
      OnClick = DBLoeschen
    end
    object PopupMenuDropTable: TMenuItem
      Action = MainForm.DropTable
    end
    object Exporttables2: TMenuItem
      Action = MainForm.ExportTables
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Drop1: TMenuItem
      Caption = 'Refresh'
      ImageIndex = 18
      ShortCut = 116
      OnClick = ReadDatabasesAndTables
    end
  end
  object popupDbGrid: TPopupMenu
    AutoHotkeys = maManual
    AutoPopup = False
    Images = MainForm.ImageList1
    OnPopup = popupDbGridPopup
    Left = 72
    Top = 48
    object menuproperties: TMenuItem
      Caption = 'Properties'
      Default = True
      Enabled = False
      ImageIndex = 9
      OnClick = ShowTableProperties
    end
    object MenuAdvancedProperties: TMenuItem
      Caption = 'Advanced Properties'
      Enabled = False
      OnClick = MenuAdvancedPropertiesClick
    end
    object menuviewdata: TMenuItem
      Caption = 'View Data'
      Enabled = False
      ImageIndex = 8
      OnClick = TabelleAnzeigen
    end
    object menuinsert: TMenuItem
      Caption = 'Insert Record'
      Enabled = False
      ImageIndex = 32
      ShortCut = 45
      OnClick = InsertRecord
    end
    object InsertfilesintoBLOBfields1: TMenuItem
      Action = MainForm.InsertFiles
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object menudroptable: TMenuItem
      Action = MainForm.DropTable
    end
    object menuemptytable: TMenuItem
      Caption = 'Empty ...'
      Enabled = False
      ImageIndex = 31
      ShortCut = 8238
      OnClick = TabelleLeeren
    end
    object MenuRenameTable: TMenuItem
      Caption = 'Rename'
      Enabled = False
      ShortCut = 113
      OnClick = MenuRenameTableClick
    end
    object MenuTableComment: TMenuItem
      Caption = 'Edit Comment'
      Enabled = False
      ShortCut = 114
      OnClick = MenuTableCommentClick
    end
    object Table1: TMenuItem
      Caption = 'Maintenance'
      object MenuOptimize: TMenuItem
        Caption = 'OPTIMIZE'
        Enabled = False
        OnClick = MenuOptimizeClick
      end
      object MenuCheck: TMenuItem
        Caption = 'CHECK'
        Enabled = False
        OnClick = MenuCheckClick
      end
      object MenuAnalyze: TMenuItem
        Caption = 'ANALYZE'
        Enabled = False
        OnClick = MenuAnalyzeClick
      end
      object MenuRepair: TMenuItem
        Caption = 'REPAIR'
        Enabled = False
        OnClick = MenuRepairClick
      end
      object More1: TMenuItem
        Caption = 'More Maintenance...'
        OnClick = More1Click
      end
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object MenuCopyTable: TMenuItem
      Action = MainForm.CopyTable
    end
    object MenuChangeType: TMenuItem
      Caption = 'Change Type'
      object MenuChangeType1: TMenuItem
        Caption = 'ISAM'
        OnClick = MenuChangeTypeClick
      end
      object MenuChangeType2: TMenuItem
        Caption = 'MyISAM'
        OnClick = MenuChangeTypeClick
      end
      object MenuChangeType3: TMenuItem
        Caption = 'HEAP'
        OnClick = MenuChangeTypeClick
      end
      object MenuChangeType4: TMenuItem
        Caption = 'MERGE'
        OnClick = MenuChangeTypeClick
      end
      object MenuChangeType5: TMenuItem
        Caption = 'InnoDB'
        OnClick = MenuChangeTypeClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object MenuChangeTypeOther: TMenuItem
        Caption = 'Other...'
        OnClick = MenuChangeTypeOtherClick
      end
    end
    object menucreatetable: TMenuItem
      Caption = 'Create new Table...'
      ImageIndex = 72
      OnClick = CreateTable
    end
    object Exporttables1: TMenuItem
      Action = MainForm.ExportTables
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object PrintList3: TMenuItem
      Action = MainForm.PrintList
    end
    object menurefresh: TMenuItem
      Tag = 28
      Caption = 'Refresh'
      ImageIndex = 18
      ShortCut = 116
      OnClick = ShowDBProperties
    end
    object selectall1: TMenuItem
      Caption = 'select all'
      ShortCut = 16449
      Visible = False
      OnClick = selectall1Click
    end
  end
  object popupHost: TPopupMenu
    Images = MainForm.ImageList1
    OnPopup = popupHostPopup
    Left = 41
    Top = 48
    object Kill1: TMenuItem
      Caption = 'Kill Process...'
      Enabled = False
      ImageIndex = 83
      ShortCut = 46
      OnClick = KillProcess
    end
    object MenuAutoupdate: TMenuItem
      Caption = 'Auto-refresh'
      object Set1: TMenuItem
        Caption = 'Set interval...'
        OnClick = Autoupdate1Click
      end
      object EnableAutoRefresh: TMenuItem
        Caption = 'Active'
        RadioItem = True
        OnClick = EnableAutoRefreshClick
      end
      object DisableAutoRefresh: TMenuItem
        Caption = 'Inactive'
        Checked = True
        RadioItem = True
        OnClick = DisableAutoRefreshClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object PrintList2: TMenuItem
      Action = MainForm.PrintList
    end
    object Refresh1: TMenuItem
      Tag = 28
      Caption = 'Refresh'
      ImageIndex = 18
      ShortCut = 116
      OnClick = ShowVariablesAndProcesses
    end
  end
  object SynSQLSyn1: TSynSQLSyn
    DefaultFilter = 'SQL files (*.sql)|*.sql'
    CommentAttri.Foreground = clGray
    DataTypeAttri.Foreground = clMaroon
    FunctionAttri.Foreground = clNavy
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clPurple
    StringAttri.Foreground = clGreen
    SymbolAttri.Foreground = clBlue
    TableNameAttri.Foreground = clFuchsia
    VariableAttri.Foreground = clPurple
    SQLDialect = sqlMySQL
    Left = 7
    Top = 192
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 7
    Top = 120
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 7
    Top = 157
  end
  object popupTableGrid: TPopupMenu
    Images = MainForm.ImageList1
    Left = 104
    Top = 48
    object MenuEditField: TMenuItem
      Caption = 'Properties'
      Default = True
      ImageIndex = 9
      ShortCut = 13
      OnClick = UpdateField
    end
    object MenuAddField: TMenuItem
      Caption = 'Add Field...'
      ImageIndex = 34
      ShortCut = 16449
      OnClick = MenuAddFieldClick
    end
    object DropField1: TMenuItem
      Caption = 'Drop Field...'
      Hint = 'Delete Field from Table'
      ImageIndex = 33
      ShortCut = 46
      OnClick = DropField
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object ManageIndexes1: TMenuItem
      Caption = '&Manage Indexes...'
      OnClick = ManageIndexes1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Insertrecord2: TMenuItem
      Caption = 'Insert record...'
      ImageIndex = 32
      ShortCut = 45
      OnClick = InsertRecord
    end
    object InsertfilesintoBLOBfields2: TMenuItem
      Action = MainForm.InsertFiles
    end
    object PrintList4: TMenuItem
      Action = MainForm.PrintList
    end
    object Refresh2: TMenuItem
      Tag = 28
      Caption = 'Refresh'
      Hint = 'Refresh|Refresh Field-List'
      ImageIndex = 18
      ShortCut = 116
      OnClick = ShowTableProperties
    end
  end
  object popupDataGrid: TPopupMenu
    Images = MainForm.ImageList1
    OnPopup = popupDataGridPopup
    Left = 136
    Top = 48
    object Copy3: TMenuItem
      Action = MainForm.EditCopy1
    end
    object Copy2: TMenuItem
      Action = MainForm.ManualCopy
    end
    object Paste2: TMenuItem
      Action = MainForm.EditPaste1
    end
    object setNULL1: TMenuItem
      Caption = 'Set NULL'
      ShortCut = 24654
      OnClick = setNULL1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Caption = 'Delete record(s)'
      ImageIndex = 23
      ShortCut = 16430
      OnClick = Delete1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object QuickFilter1: TMenuItem
      Caption = 'Quick Filter'
      object QF1: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Field = Value'
        OnClick = QuickFilterClick
      end
      object QF2: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Field != Value'
        OnClick = QuickFilterClick
      end
      object QF3: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Field > Value'
        OnClick = QuickFilterClick
      end
      object QF4: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Field < Value'
        OnClick = QuickFilterClick
      end
      object QF5: TMenuItem
        Caption = 'Field like Value%'
        OnClick = QuickFilterClick
      end
      object QF6: TMenuItem
        Caption = 'Field like %Value'
        OnClick = QuickFilterClick
      end
      object QF7: TMenuItem
        Caption = 'Field like %Value%'
        OnClick = QuickFilterClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object QF8: TMenuItem
        Caption = 'Field = ?'
        OnClick = QuickFilterClick
      end
      object QF9: TMenuItem
        Caption = 'Field != ?'
        OnClick = QuickFilterClick
      end
      object QF10: TMenuItem
        Caption = 'Field > ?'
        OnClick = QuickFilterClick
      end
      object QF11: TMenuItem
        Caption = 'Field < ?'
        OnClick = QuickFilterClick
      end
      object QF12: TMenuItem
        Caption = 'Field like ?'
        OnClick = QuickFilterClick
      end
      object N7: TMenuItem
        AutoHotkeys = maManual
        Caption = '-'
      end
      object DropFilter1: TMenuItem
        Caption = 'Drop Filter'
        OnClick = DropFilter1Click
      end
    end
    object Filter1: TMenuItem
      Caption = 'Filter...'
      OnClick = Filter1Click
    end
    object MenuLimit: TMenuItem
      Caption = 'Limit'
      Checked = True
      OnClick = MenuLimitClick
    end
    object Find1: TMenuItem
      Action = MainForm.DataSearch
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object Copytableas1: TMenuItem
      Tag = 46
      Caption = 'Copy data'
      object CopyasCSVData1: TMenuItem
        Tag = 48
        Action = MainForm.Copy2CSV
      end
      object CopycontentsasHTML1: TMenuItem
        Tag = 49
        Action = MainForm.CopyHTMLtable
      end
      object CopyasXMLdata1: TMenuItem
        Action = MainForm.Copy2XML
      end
    end
    object Exportdata2: TMenuItem
      Action = MainForm.ExportData
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object DataInsertDateTime: TMenuItem
      Caption = 'Insert Date/Time'
      object DataDateTime: TMenuItem
        Caption = 'datetime'
        Hint = 'Insert datetime-value'
        OnClick = InsertDate
      end
      object DataDate: TMenuItem
        Caption = 'date'
        Hint = 'Insert date-value'
        OnClick = InsertDate
      end
      object DataTime: TMenuItem
        Caption = 'time'
        Hint = 'Insert time-value'
        OnClick = InsertDate
      end
      object DataTimestamp: TMenuItem
        Caption = 'timestamp'
        Hint = 'Insert timestamp-value'
        OnClick = InsertDate
      end
      object DataYear: TMenuItem
        Caption = 'year'
        Hint = 'Insert year-value'
        OnClick = InsertDate
      end
    end
    object MenuViewBlob: TMenuItem
      Caption = 'Edit Memo/BLOB'
      OnClick = MenuViewBlobClick
    end
    object ViewasHTML1: TMenuItem
      Action = MainForm.HTMLview
    end
    object InsertfilesintoBLOBfields3: TMenuItem
      Action = MainForm.InsertFiles
    end
    object N19: TMenuItem
      Caption = '-'
    end
    object Refresh3: TMenuItem
      Tag = 28
      Caption = 'Refresh'
      ImageIndex = 18
      ShortCut = 116
      OnClick = viewdata
    end
  end
  object popupResultGrid: TPopupMenu
    Images = MainForm.ImageList1
    OnPopup = popupResultGridPopup
    Left = 8
    Top = 80
    object Copy4: TMenuItem
      Action = MainForm.EditCopy1
    end
    object Copyfieldcontents1: TMenuItem
      Action = MainForm.ManualCopy
    end
    object HTMLview1: TMenuItem
      Action = MainForm.HTMLview
    end
    object N14: TMenuItem
      Caption = '-'
    end
    object Copyrecords1: TMenuItem
      Tag = 48
      Action = MainForm.Copy2CSV
    end
    object CopycontentsasHTML2: TMenuItem
      Tag = 49
      Action = MainForm.CopyHTMLtable
    end
    object CopyasXMLdata2: TMenuItem
      Action = MainForm.Copy2XML
    end
    object Exportdata1: TMenuItem
      Action = MainForm.ExportData
    end
  end
  object TimerConnected: TTimer
    Enabled = False
    OnTimer = TimerConnectedTimer
    Left = 103
    Top = 157
  end
  object popupSqlLog: TPopupMenu
    Images = MainForm.ImageList1
    Left = 40
    Top = 80
    object Copy1: TMenuItem
      Action = MainForm.EditCopy1
    end
    object Clear2: TMenuItem
      Caption = 'Clear'
      OnClick = Clear2Click
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object Markall3: TMenuItem
      Caption = 'Select all'
      OnClick = Markall3Click
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object EditQuery1: TMenuItem
      Caption = 'Edit Selection'
      OnClick = EditQuery1Click
    end
    object Saveastextfile1: TMenuItem
      Caption = 'Save as textfile...'
      ImageIndex = 7
      OnClick = Saveastextfile1Click
    end
  end
  object Timer5: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer5Timer
    Left = 39
    Top = 157
  end
  object DataSource1: TDataSource
    DataSet = ZQuery2
    OnDataChange = DataSourceDataChange
    Left = 304
    Top = 64
  end
  object DataSource2: TDataSource
    DataSet = ZQuery1
    OnDataChange = DataSourceDataChange
    Left = 304
    Top = 96
  end
  object popupFilterOpenFile: TPopupMenu
    Left = 272
    Top = 278
  end
  object OpenDialog2: TOpenDialog
    Left = 302
    Top = 278
  end
  object TimerProcesslist: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = ShowProcessList
    Left = 72
    Top = 157
  end
  object SaveDialogExportData: TExportSaveDialog
    DefaultExt = 'csv'
    Filter = 
      'CSV-Files (*.csv)|*.csv|Hypertext-Files (*.html)|*.html|XML-File' +
      's (*.xml)|*.xml'
    Options = [ofOverwritePrompt, ofEnableSizing]
    OnTypeChange = SaveDialogExportDataTypeChange
    VisibleOptions = voCSV
    ConvertHTMLSpecialChars = False
    Left = 72
    Top = 120
  end
  object ZConn: TZConnection
    Protocol = 'mysql'
    SQLHourGlass = True
    Left = 224
    Top = 64
  end
  object ZQuery1: TZQuery
    Connection = ZConn
    BeforeClose = ZQuery2BeforeClose
    OnDeleteError = ZQuery1EditError
    OnEditError = ZQuery1EditError
    OnPostError = ZQuery1EditError
    ParamCheck = False
    Params = <>
    UpdateMode = umUpdateAll
    Left = 272
    Top = 96
  end
  object ZQuery2: TZQuery
    Connection = ZConn
    BeforeOpen = ZQueryBeforeSendingSQL
    BeforeClose = ZQuery2BeforeClose
    BeforePost = ZQueryBeforeSendingSQL
    AfterPost = ZQuery2AfterPost
    AfterDelete = ZQuery2AfterPost
    ParamCheck = False
    Params = <>
    UpdateMode = umUpdateAll
    Left = 272
    Top = 64
  end
  object ZQuery3: TZReadOnlyQuery
    Connection = ZConn
    AutoCalcFields = False
    ParamCheck = False
    Params = <>
    Left = 272
    Top = 128
  end
  object ZSQLMonitor1: TZSQLMonitor
    Active = True
    MaxTraceCount = 100
    OnLogTrace = ZSQLMonitor1LogTrace
    Left = 224
    Top = 96
  end
  object popupDbGridHeader: TPopupMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    Images = MainForm.ImageList1
    Left = 72
    Top = 80
    object DefaultColumnLayout1: TMenuItem
      Tag = 1
      Caption = 'Default columns'
      Checked = True
      OnClick = MenuTablelistColumnsClick
    end
    object N20: TMenuItem
      Caption = '-'
    end
  end
  object SynCompletionProposal1: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'databasemore'
      end
      item
        BiggestWord = 'IHAVENOCLUEWHICHLENGTH'
        DefaultFontStyle = [fsBold]
      end>
    ItemHeight = 18
    Images = MainForm.ImageList1
    Margin = 1
    OnExecute = SynCompletionProposal1Execute
    ShortCut = 16416
    Editor = SynMemoQuery
    TimerInterval = 500
    OnAfterCodeCompletion = SynCompletionProposal1AfterCodeCompletion
    OnCodeCompletion = SynCompletionProposal1CodeCompletion
    Left = 40
    Top = 192
  end
  object popupQueryLoad: TPopupMenu
    Left = 104
    Top = 80
  end
  object OpenDialogSQLFile: TOpenDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 40
    Top = 120
  end
  object SaveDialogSQLFile: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 104
    Top = 120
  end
  object FindDialogQuery: TFindDialog
    OnFind = FindDialogQueryFind
    Left = 8
    Top = 224
  end
  object SynEditSearch1: TSynEditSearch
    Left = 72
    Top = 192
  end
  object ReplaceDialogQuery: TReplaceDialog
    OnFind = ReplaceDialogQueryFind
    OnReplace = ReplaceDialogQueryReplace
    Left = 40
    Top = 224
  end
end
