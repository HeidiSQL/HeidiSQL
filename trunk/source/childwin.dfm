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
  OnDeactivate = FormDeactivate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 0
    Top = 252
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
    Height = 252
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 169
      Top = 0
      Width = 4
      Height = 252
      Cursor = crSizeWE
      Beveled = True
      ResizeStyle = rsUpdate
    end
    object DBtree: TTreeView
      Left = 0
      Top = 0
      Width = 169
      Height = 252
      Align = alLeft
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
      OnExpanding = DBtreeExpanding
    end
    object TableShow: TPanel
      Left = 173
      Top = 0
      Width = 504
      Height = 252
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object PageControlMain: TPageControl
        Left = 0
        Top = 0
        Width = 504
        Height = 252
        ActivePage = tabHost
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
        object tabHost: TTabSheet
          Caption = 'Host'
          ImageIndex = 41
          object PageControlHost: TPageControl
            Left = 0
            Top = 17
            Width = 496
            Height = 203
            ActivePage = tabVariables
            Align = alClient
            HotTrack = True
            TabOrder = 0
            OnChange = PageControlHostChange
            object tabVariables: TTabSheet
              Caption = 'Variables'
              object ListVariables: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 488
                Height = 175
                Align = alClient
                DragOperations = []
                Header.AutoSizeIndex = 1
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
                Header.SortColumn = 0
                Header.Style = hsXPStyle
                Images = MainForm.ImageList1
                IncrementalSearch = isInitializedOnly
                TabOrder = 0
                TreeOptions.MiscOptions = [toToggleOnDblClick]
                TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
                OnCompareNodes = vstCompareNodes
                OnGetText = vstGetText
                OnGetImageIndex = vstGetImageIndex
                OnGetNodeDataSize = vstGetNodeDataSize
                OnHeaderClick = vstHeaderClick
                OnInitNode = ListVariablesInitNode
                Columns = <
                  item
                    Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
                    Position = 0
                    Width = 160
                    WideText = 'Variable'
                  end
                  item
                    Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
                    Position = 1
                    Width = 324
                    WideText = 'Value'
                  end>
              end
            end
            object tabProcessList: TTabSheet
              Caption = 'Process-List'
              ImageIndex = 1
              object ListProcesses: TSortListView
                Tag = -1
                Left = 0
                Top = 0
                Width = 488
                Height = 175
                Align = alClient
                Columns = <
                  item
                    Caption = 'id'
                    Width = -1
                    WidthType = (
                      -1)
                  end
                  item
                    Caption = 'User'
                    Width = 80
                  end
                  item
                    Caption = 'Host'
                    Tag = -1
                    Width = 80
                  end
                  item
                    Caption = 'DB'
                    Width = -1
                    WidthType = (
                      -1)
                  end
                  item
                    Caption = 'Command'
                    Width = 80
                  end
                  item
                    Caption = 'Time'
                  end
                  item
                    Caption = 'State'
                  end
                  item
                    Caption = 'Info'
                    Width = 126
                  end>
                GridLines = True
                ReadOnly = True
                RowSelect = True
                PopupMenu = popupHost
                SmallImages = MainForm.ImageList1
                TabOrder = 0
                ViewStyle = vsReport
                OnSelectItem = ListProcessesSelectItem
                ImageIndexSortAsc = 95
                ImageIndexSortDesc = 94
              end
            end
            object tabCommandStats: TTabSheet
              Caption = 'Command-Statistics'
              ImageIndex = 2
              object ListCommandStats: TSortListView
                Left = 0
                Top = 0
                Width = 488
                Height = 175
                Align = alClient
                Columns = <
                  item
                    Caption = 'Command-type'
                    Width = 120
                  end
                  item
                    Alignment = taRightJustify
                    Caption = 'Total count'
                    Width = 100
                  end
                  item
                    Alignment = taRightJustify
                    Caption = 'Average per hour'
                    Width = 100
                  end
                  item
                    Alignment = taRightJustify
                    Caption = 'Average per second'
                    Width = 100
                  end
                  item
                    Alignment = taRightJustify
                    Caption = 'Percentage'
                    Width = 67
                  end>
                GridLines = True
                ReadOnly = True
                RowSelect = True
                PopupMenu = popupHost
                SmallImages = MainForm.ImageList1
                SortType = stBoth
                TabOrder = 0
                ViewStyle = vsReport
                ImageIndexSortAsc = 95
                ImageIndexSortDesc = 94
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
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
        end
        object tabDatabase: TTabSheet
          Caption = 'Database'
          ImageIndex = 38
          DesignSize = (
            496
            220)
          object pnlDatabaseTop: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 17
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'Database'
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
            Height = 196
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Table'
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Alignment = taRightJustify
                Caption = 'Records'
                Width = 80
              end
              item
                Alignment = taRightJustify
                Caption = 'Size'
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Caption = 'Created'
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
                Width = -1
                WidthType = (
                  -1)
              end
              item
                Caption = 'Comment'
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
            OnColumnRightClick = ListTablesColumnRightClick
            OnDblClick = ListTablesDblClick
            OnEdited = ListTablesEdited
            OnEditing = ListTablesEditing
            OnMouseDown = ListTablesMouseDown
            OnSelectItem = ListTablesSelectItem
            ImageIndexSortAsc = 95
            ImageIndexSortDesc = 94
          end
          object tlbDataLeft1: TToolBar
            Left = 3
            Top = 20
            Width = 26
            Height = 146
            Align = alNone
            Caption = 'tlbDataLeft1'
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
              OnClick = btnDbViewDataClick
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
              OnClick = EmptyTable
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
          object tlbDataLeft2: TToolBar
            Left = 3
            Top = 153
            Width = 23
            Height = 29
            Align = alNone
            Caption = 'tlbDataLeft2'
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
        object tabTable: TTabSheet
          Caption = 'Table'
          ImageIndex = 40
          DesignSize = (
            496
            220)
          object pnlTableTop: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 17
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'Table-Properties'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object tlbTableLeft1: TToolBar
            Left = 3
            Top = 20
            Width = 26
            Height = 146
            Align = alNone
            ButtonHeight = 23
            Caption = 'tlbTableLeft1'
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
              OnClick = btnTableViewDataClick
            end
            object btnTableEditField: TToolButton
              Left = 0
              Top = 23
              Hint = 'Edit Field...'
              Caption = 'btnTableEditField'
              ImageIndex = 9
              Wrap = True
              OnClick = UpdateField
            end
            object btnTableAddField: TToolButton
              Left = 0
              Top = 46
              Hint = 'Add Field...'
              Caption = 'btnTableAddField'
              ImageIndex = 34
              Wrap = True
              OnClick = MenuAddFieldClick
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
          object pnlTableList: TPanel
            Left = 32
            Top = 17
            Width = 465
            Height = 196
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            TabOrder = 2
            object ListColumns: TSortListView
              Tag = -1
              Left = 0
              Top = 0
              Width = 465
              Height = 196
              Align = alClient
              Columns = <
                item
                  Caption = 'Name'
                  Width = -1
                  WidthType = (
                    -1)
                end
                item
                  Caption = 'Type'
                  Width = 100
                end
                item
                  Caption = 'Null'
                  Width = 40
                end
                item
                  Caption = 'Default'
                  Width = 115
                end
                item
                  Caption = 'Extra'
                  Width = 200
                end>
              GridLines = True
              MultiSelect = True
              RowSelect = True
              PopupMenu = popupTableGrid
              SmallImages = MainForm.ImageList1
              TabOrder = 0
              ViewStyle = vsReport
              OnDblClick = UpdateField
              OnEdited = ListColumnsEdited
              OnKeyUp = controlsKeyUp
              OnSelectItem = ListColumnsSelectItem
              ImageIndexSortAsc = 95
              ImageIndexSortDesc = 94
            end
          end
          object tlbTableLeft2: TToolBar
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
        object tabData: TTabSheet
          Caption = 'Data'
          ImageIndex = 39
          object pnlDataTop: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 31
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            DesignSize = (
              496
              31)
            object btnColumnSelection: TSpeedButton
              Left = 187
              Top = 4
              Width = 70
              Height = 21
              AllowAllUp = True
              Anchors = [akTop, akRight]
              GroupIndex = 10
              Caption = 'Columns'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Glyph.Data = {
                52000000424D52000000000000003E0000002800000009000000050000000100
                010000000000140000000000000000000000020000000200000000000000FFFF
                FF00F7806300E3805C00C18032008080300000004700}
              Layout = blGlyphRight
              ParentFont = False
              OnClick = btnDataClick
            end
            object lblDataTop: TLabel
              Left = 1
              Top = 1
              Width = 104
              Height = 29
              Align = alLeft
              Anchors = [akLeft, akTop, akRight, akBottom]
              AutoSize = False
              Caption = 'lblDataTop'
              Layout = tlCenter
              WordWrap = True
            end
            object btnDataSorting: TSpeedButton
              Left = 111
              Top = 4
              Width = 70
              Height = 21
              AllowAllUp = True
              Anchors = [akTop, akRight]
              GroupIndex = 10
              Caption = 'Sorting'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Glyph.Data = {
                52000000424D52000000000000003E0000002800000009000000050000000100
                010000000000140000000000000000000000020000000200000000000000FFFF
                FF00F7806300E3805C00C18032008080300000004700}
              Layout = blGlyphRight
              ParentFont = False
              OnClick = btnDataClick
            end
            object EditDataSearch: TEdit
              Left = 317
              Top = 4
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
              OnKeyUp = controlsKeyUp
            end
            object ButtonDataSearch: TButton
              Left = 440
              Top = 4
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
            object CheckBoxDataSearch: TCheckBox
              Left = 264
              Top = 6
              Width = 49
              Height = 17
              Anchors = [akTop, akRight]
              Caption = 'NOT'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
            end
          end
          object gridData: TSMDBGrid
            Left = 0
            Top = 31
            Width = 496
            Height = 189
            Align = alClient
            DataSource = DataSource1
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect]
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
            OnKeyUp = controlsKeyUp
            OnMouseDown = gridMouseDown
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
        object tabQuery: TTabSheet
          Caption = 'Query'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageIndex = 57
          ParentFont = False
          object spltQuery: TSplitter
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
          end
          object pnlQueryTop: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 29
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            Caption = 'SQL-Query:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            object pnlQueryToolbar: TPanel
              Left = 272
              Top = 1
              Width = 223
              Height = 27
              Align = alRight
              BevelOuter = bvNone
              BorderWidth = 1
              TabOrder = 0
              object ToolBarQuery: TToolBar
                Left = 1
                Top = 1
                Width = 221
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
                object btnAltTerminator: TToolButton
                  Left = 197
                  Top = 0
                  Hint = 'Use alternate '#39'//'#39' SQL sentence terminator'
                  Caption = 'btnAltTerminator'
                  ImageIndex = 54
                  Style = tbsCheck
                end
              end
            end
            object PanelCharsInQueryWindow: TPanel
              Left = 200
              Top = 1
              Width = 72
              Height = 27
              Align = alRight
              Alignment = taRightJustify
              BevelOuter = bvNone
              BorderWidth = 2
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
            end
          end
          object pnlQueryMemo: TPanel
            Left = 0
            Top = 29
            Width = 496
            Height = 96
            Align = alTop
            BevelOuter = bvNone
            Constraints.MinHeight = 10
            TabOrder = 1
            object spltQueryHelpers: TSplitter
              Left = 332
              Top = 0
              Width = 4
              Height = 96
              Cursor = crSizeWE
              Align = alRight
              ResizeStyle = rsUpdate
            end
            object SynMemoQuery: TSynMemo
              Left = 0
              Top = 0
              Width = 332
              Height = 96
              SingleLineMode = False
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Courier New'
              Font.Style = []
              PopupMenu = popupQuery
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
            object pnlQueryHelpers: TPanel
              Left = 336
              Top = 0
              Width = 160
              Height = 96
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 1
              object tabsetQueryHelpers: TTabSet
                Left = 0
                Top = 72
                Width = 160
                Height = 24
                Align = alBottom
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentShowHint = False
                ShowHint = False
                Style = tsModernTabs
                Tabs.Strings = (
                  'Cols'
                  'SQL fn'
                  'SQL kw'
                  'Snippets')
                TabIndex = 0
                OnChange = tabsetQueryHelpersChange
              end
              object lboxQueryHelpers: TListBox
                Left = 0
                Top = 0
                Width = 160
                Height = 72
                Align = alClient
                DragMode = dmAutomatic
                ItemHeight = 13
                PopupMenu = popupQueryHelpers
                TabOrder = 1
                OnClick = lboxQueryHelpersClick
                OnDblClick = lboxQueryHelpersDblClick
                OnKeyUp = controlsKeyUp
              end
            end
          end
          object gridQuery: TSMDBGrid
            Left = 0
            Top = 159
            Width = 496
            Height = 61
            Align = alClient
            DataSource = DataSource2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Options = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect]
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
            OnMouseDown = gridMouseDown
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
    Top = 256
    Width = 677
    Height = 164
    ActivePage = tabSQLLog
    Align = alBottom
    HotTrack = True
    Images = MainForm.ImageList1
    TabHeight = 20
    TabOrder = 1
    object tabSQLLog: TTabSheet
      Caption = 'SQL Log'
      ImageIndex = 79
      object SynMemoSQLLog: TSynMemo
        Left = 0
        Top = 0
        Width = 669
        Height = 134
        SingleLineMode = True
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = popupSqlLog
        TabOrder = 0
        OnKeyUp = controlsKeyUp
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
    object tabBlobEditor: TTabSheet
      Caption = 'BLOB-Editor'
      ImageIndex = 80
      object ToolBar3: TToolBar
        Left = 0
        Top = 0
        Width = 23
        Height = 134
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
        object btnUnsafeEdit: TToolButton
          Left = 0
          Top = 0
          Caption = 'btnUnsafeEdit'
          Enabled = False
          ImageIndex = 32
          Wrap = True
          OnClick = btnUnsafeEditClick
        end
        object btnBlobWordWrap: TToolButton
          Left = 0
          Top = 22
          Hint = 'Wordwrap'
          Caption = 'ToolButton1'
          ImageIndex = 54
          Wrap = True
          Style = tbsCheck
          OnClick = btnBlobWordWrapClick
        end
        object btnBlobLoad: TToolButton
          Left = 0
          Top = 44
          Hint = 'Open|Open file'
          Caption = 'ToolButton4'
          ImageIndex = 6
          Wrap = True
          OnClick = btnBlobLoadClick
        end
        object btnBlobSave: TToolButton
          Left = 0
          Top = 66
          Hint = 'Save|Save to File'
          Caption = 'ToolButton5'
          ImageIndex = 7
          Wrap = True
          OnClick = btnBlobSaveClick
        end
        object btnBlobViewAsHtml: TToolButton
          Left = 0
          Top = 88
          Action = MainForm.HTMLview
          Wrap = True
        end
        object btnBlobCopy: TToolButton
          Left = 0
          Top = 110
          Hint = 'Copy to clipboard'
          Caption = 'btnBlobCopy'
          ImageIndex = 1
          OnClick = btnBlobCopyClick
        end
      end
      object PageControlBlobEditors: TPageControl
        Left = 23
        Top = 0
        Width = 646
        Height = 134
        ActivePage = tabBlobEditorText
        Align = alClient
        TabOrder = 1
        OnChange = PageControlBlobEditorsChange
        object tabBlobEditorText: TTabSheet
          Caption = 'Text'
          object DBMemo1: TDBMemo
            Left = 0
            Top = 0
            Width = 638
            Height = 106
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
            OnExit = DBMemo1Exit
            OnKeyUp = controlsKeyUp
          end
        end
        object tabBlobEditorImage: TTabSheet
          Caption = 'Image'
          ImageIndex = 1
          object ScrollBox1: TScrollBox
            Left = 0
            Top = 0
            Width = 638
            Height = 106
            Align = alClient
            TabOrder = 0
            object EDBImage1: TEDBImage
              Left = 0
              Top = 0
              Width = 89
              Height = 102
              BorderStyle = bsNone
              Color = clBtnFace
              Stretch = True
              TabOrder = 0
            end
          end
        end
      end
    end
    object tabFilter: TTabSheet
      Caption = 'Filter'
      ImageIndex = 81
      object SynMemoFilter: TSynMemo
        Left = 0
        Top = 29
        Width = 669
        Height = 105
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = popupQuery
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
      OnClick = DropDB
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
      OnClick = ListTablesDblClick
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
      OnClick = ShowTable
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
      OnClick = EmptyTable
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
  object TimerHostUptime: TTimer
    OnTimer = TimerHostUptimeTimer
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
      ShortCut = 32781
      OnClick = UpdateField
    end
    object MenuAddField: TMenuItem
      Caption = 'Add Field...'
      ImageIndex = 34
      ShortCut = 16449
      OnClick = MenuAddFieldClick
    end
    object DropField1: TMenuItem
      Caption = 'Drop Field(s)...'
      Hint = 'Delete Field(s) from Table'
      ImageIndex = 33
      ShortCut = 16430
      OnClick = DropField
    end
    object menuRenameColumn: TMenuItem
      Caption = 'Rename Field'
      ShortCut = 113
      OnClick = menuRenameColumnClick
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
    AutoHotkeys = maManual
    Images = MainForm.ImageList1
    OnPopup = popupDataGridPopup
    Left = 136
    Top = 48
    object Copy3: TMenuItem
      Action = MainForm.EditCopy1
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
      ImageIndex = 81
      object QF1: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column = Value'
        OnClick = QuickFilterClick
      end
      object QF2: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column != Value'
        OnClick = QuickFilterClick
      end
      object QF3: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column > Value'
        OnClick = QuickFilterClick
      end
      object QF4: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column < Value'
        OnClick = QuickFilterClick
      end
      object QF5: TMenuItem
        Caption = 'Column LIKE Value%'
        OnClick = QuickFilterClick
      end
      object QF6: TMenuItem
        Caption = 'Column LIKE %Value'
        OnClick = QuickFilterClick
      end
      object QF7: TMenuItem
        Caption = 'Column LIKE %Value%'
        OnClick = QuickFilterClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object QF8: TMenuItem
        Caption = 'Column = ...'
        OnClick = QuickFilterClick
      end
      object QF9: TMenuItem
        Caption = 'Column != ...'
        OnClick = QuickFilterClick
      end
      object QF10: TMenuItem
        Caption = 'Column > ...'
        OnClick = QuickFilterClick
      end
      object QF11: TMenuItem
        Caption = 'Column < ...'
        OnClick = QuickFilterClick
      end
      object QF12: TMenuItem
        Caption = 'Column like ...'
        OnClick = QuickFilterClick
      end
      object N7: TMenuItem
        AutoHotkeys = maManual
        Caption = '-'
      end
      object QF13: TMenuItem
        Caption = 'Column = CLIPBOARD'
        ImageIndex = 2
        OnClick = QuickFilterClick
      end
      object QF14: TMenuItem
        Caption = 'Column != CLIPBOARD'
        ImageIndex = 2
        OnClick = QuickFilterClick
      end
      object QF15: TMenuItem
        Caption = 'Column > CLIPBOARD'
        ImageIndex = 2
        OnClick = QuickFilterClick
      end
      object QF16: TMenuItem
        Caption = 'Column < CLIPBOARD'
        ImageIndex = 2
        OnClick = QuickFilterClick
      end
      object QF17: TMenuItem
        Caption = 'Column LIKE %CLIPBOARD%'
        ImageIndex = 2
        OnClick = QuickFilterClick
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object DropFilter1: TMenuItem
        Caption = 'Drop Filter'
        ImageIndex = 70
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
  object TimerConnectErrorCloseWindow: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerConnectErrorCloseWindowTimer
    Left = 39
    Top = 157
  end
  object DataSource1: TDataSource
    OnDataChange = DataSourceDataChange
    Left = 304
    Top = 136
  end
  object DataSource2: TDataSource
    OnDataChange = DataSourceDataChange
    Left = 304
    Top = 168
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
  object ZSQLMonitor1: TZSQLMonitor
    Active = True
    MaxTraceCount = 100
    OnLogTrace = ZSQLMonitor1LogTrace
    Left = 224
    Top = 168
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
    EndOfTokenChr = '()[]. ='
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
  object popupQuery: TPopupMenu
    Images = MainForm.ImageList1
    OnPopup = popupQueryPopup
    Left = 136
    Top = 80
    object MenuRun: TMenuItem
      Action = MainForm.ExecuteQuery
    end
    object MenuRunSelection: TMenuItem
      Action = MainForm.ExecuteSelection
    end
    object MenuRunLine: TMenuItem
      Action = MainForm.ExecuteLine
    end
    object MenuSetFilter: TMenuItem
      Caption = 'Set Filter'
      ImageIndex = 10
      Visible = False
      OnClick = setFilter
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object menucopy: TMenuItem
      Action = MainForm.EditCopy1
    end
    object menupaste: TMenuItem
      Action = MainForm.EditPaste1
    end
    object menuclear: TMenuItem
      Caption = 'Clear'
      ShortCut = 16471
      OnClick = menuclearClick
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object MenuFind: TMenuItem
      Caption = 'Find...'
      ImageIndex = 50
      ShortCut = 16454
      OnClick = btnQueryFindClick
    end
    object MenuReplace: TMenuItem
      Caption = 'Replace ...'
      Hint = 'Search and replace...'
      ImageIndex = 51
      ShortCut = 16466
      OnClick = btnQueryReplaceClick
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object menuload: TMenuItem
      Caption = 'Load from file ...'
      ImageIndex = 6
      OnClick = btnQueryLoadClick
    end
    object menuInsertFileAtCursor: TMenuItem
      Caption = 'Insert file at cursor ...'
      ImageIndex = 6
      OnClick = menuInsertFileAtCursorClick
    end
    object menusave: TMenuItem
      Caption = 'Save to file ...'
      ImageIndex = 7
      OnClick = btnQuerySaveClick
    end
    object menuSaveSelectionToFile: TMenuItem
      Tag = 1
      Caption = 'Save selection to file ...'
      ImageIndex = 7
      OnClick = btnQuerySaveClick
    end
    object menuSaveAsSnippet: TMenuItem
      Caption = 'Save as snippet ...'
      ImageIndex = 88
      OnClick = btnQuerySaveSnippetClick
    end
    object menuSaveSelectionAsSnippet: TMenuItem
      Tag = 1
      Caption = 'Save selection as snippet ...'
      ImageIndex = 88
      OnClick = btnQuerySaveSnippetClick
    end
    object N23: TMenuItem
      Caption = '-'
    end
  end
  object popupQueryHelpers: TPopupMenu
    Images = MainForm.ImageList1
    Left = 168
    Top = 80
    object menuDeleteSnippet: TMenuItem
      Caption = 'Delete ...'
      Enabled = False
      ImageIndex = 23
      ShortCut = 46
      OnClick = menuDeleteSnippetClick
    end
    object menuHelp: TMenuItem
      Caption = 'Help'
      ImageIndex = 96
      ShortCut = 112
      OnClick = CallSQLHelp
    end
  end
end
