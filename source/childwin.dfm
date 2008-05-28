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
      Images = MainForm.PngImageListMain
      Indent = 19
      ReadOnly = True
      RightClickSelect = True
      RowSelect = True
      ShowRoot = False
      TabOrder = 0
      OnChange = DBtreeChange
      OnChanging = DBtreeChanging
      OnContextPopup = DBtreeContextPopup
      OnDblClick = DBtreeDblClick
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
        Font.Name = 'Tahoma'
        Font.Style = []
        HotTrack = True
        Images = MainForm.PngImageListMain
        MultiLine = True
        ParentFont = False
        TabHeight = 22
        TabOrder = 0
        OnChange = pcChange
        object tabHost: TTabSheet
          Caption = 'Host'
          ImageIndex = 1
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
                Top = 25
                Width = 488
                Height = 150
                Align = alClient
                DragOperations = []
                Header.AutoSizeIndex = 1
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.SortColumn = 0
                HintMode = hmTooltip
                Images = MainForm.PngImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toToggleOnDblClick]
                TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
                OnBeforePaint = vstBeforePaint
                OnCompareNodes = vstCompareNodes
                OnDblClick = ListVariablesDblClick
                OnFreeNode = vstFreeNode
                OnGetText = vstGetText
                OnGetImageIndex = vstGetImageIndex
                OnGetHint = vstGetHint
                OnGetNodeDataSize = vstGetNodeDataSize
                OnHeaderClick = vstHeaderClick
                OnInitNode = vstInitNode
                Columns = <
                  item
                    Position = 0
                    Width = 160
                    WideText = 'Variable'
                  end
                  item
                    Position = 1
                    Width = 324
                    WideText = 'Value'
                  end>
              end
              object pnlFilterVariables: TPanel
                Left = 0
                Top = 0
                Width = 488
                Height = 25
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 1
                object lblFilterVariables: TLabel
                  Left = 1
                  Top = 4
                  Width = 28
                  Height = 13
                  Caption = 'Filter:'
                end
                object editFilterVariables: TEdit
                  Left = 34
                  Top = 1
                  Width = 154
                  Height = 21
                  TabOrder = 0
                  OnChange = editFilterVTChange
                end
              end
            end
            object tabStatus: TTabSheet
              Caption = 'Status'
              object ListStatus: TVirtualStringTree
                Left = 0
                Top = 25
                Width = 488
                Height = 150
                Align = alClient
                DragOperations = []
                Header.AutoSizeIndex = 1
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.SortColumn = 0
                HintMode = hmTooltip
                Images = MainForm.PngImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toToggleOnDblClick]
                TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
                OnBeforePaint = vstBeforePaint
                OnCompareNodes = vstCompareNodes
                OnFreeNode = vstFreeNode
                OnGetText = vstGetText
                OnGetImageIndex = vstGetImageIndex
                OnGetHint = vstGetHint
                OnGetNodeDataSize = vstGetNodeDataSize
                OnHeaderClick = vstHeaderClick
                OnInitNode = vstInitNode
                Columns = <
                  item
                    Position = 0
                    Width = 160
                    WideText = 'Variable'
                  end
                  item
                    Position = 1
                    Width = 324
                    WideText = 'Value'
                  end>
              end
              object pnlFilterStatus: TPanel
                Left = 0
                Top = 0
                Width = 488
                Height = 25
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 1
                object lblFilterStatus: TLabel
                  Left = 1
                  Top = 4
                  Width = 28
                  Height = 13
                  Caption = 'Filter:'
                end
                object editFilterStatus: TEdit
                  Left = 34
                  Top = 1
                  Width = 154
                  Height = 21
                  TabOrder = 0
                  OnChange = editFilterVTChange
                end
              end
            end
            object tabProcessList: TTabSheet
              Caption = 'Process-List'
              ImageIndex = 1
              object Splitter3: TSplitter
                Left = 0
                Top = 102
                Width = 488
                Height = 4
                Cursor = crSizeNS
                Align = alBottom
                ResizeStyle = rsUpdate
              end
              object ListProcesses: TVirtualStringTree
                Left = 0
                Top = 25
                Width = 488
                Height = 77
                Align = alClient
                Header.AutoSizeIndex = 7
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.SortColumn = 0
                Header.SortDirection = sdDescending
                HintMode = hmTooltip
                Images = MainForm.PngImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
                TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
                OnBeforePaint = vstBeforePaint
                OnChange = ListProcessesChange
                OnCompareNodes = vstCompareNodes
                OnFreeNode = vstFreeNode
                OnGetText = vstGetText
                OnGetImageIndex = vstGetImageIndex
                OnGetHint = vstGetHint
                OnGetNodeDataSize = vstGetNodeDataSize
                OnHeaderClick = vstHeaderClick
                OnInitNode = vstInitNode
                Columns = <
                  item
                    Alignment = taRightJustify
                    Position = 0
                    Width = 70
                    WideText = 'id'
                  end
                  item
                    Position = 1
                    Width = 80
                    WideText = 'User'
                  end
                  item
                    Position = 2
                    Width = 80
                    WideText = 'Host'
                  end
                  item
                    Position = 3
                    Width = 80
                    WideText = 'DB'
                  end
                  item
                    Position = 4
                    Width = 80
                    WideText = 'Command'
                  end
                  item
                    Position = 5
                    WideText = 'Time'
                  end
                  item
                    Position = 6
                    WideText = 'State'
                  end
                  item
                    Position = 7
                    Width = 10
                    WideText = 'Info'
                  end>
              end
              object pnlProcessViewBox: TPanel
                Left = 0
                Top = 106
                Width = 488
                Height = 69
                Align = alBottom
                BevelOuter = bvNone
                TabOrder = 1
                object pnlProcessView: TPanel
                  Left = 0
                  Top = 0
                  Width = 488
                  Height = 15
                  Align = alTop
                  Alignment = taLeftJustify
                  BevelOuter = bvNone
                  Caption = 'Process SQL:'
                  TabOrder = 0
                end
                object SynMemoProcessView: TSynMemo
                  Left = 0
                  Top = 15
                  Width = 488
                  Height = 54
                  SingleLineMode = False
                  Align = alClient
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -13
                  Font.Name = 'Courier New'
                  Font.Style = []
                  TabOrder = 1
                  Gutter.Font.Charset = DEFAULT_CHARSET
                  Gutter.Font.Color = clWindowText
                  Gutter.Font.Height = -11
                  Gutter.Font.Name = 'Courier New'
                  Gutter.Font.Style = []
                  Gutter.LeftOffset = 0
                  Gutter.ShowLineNumbers = True
                  Highlighter = SynSQLSyn1
                  Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
                  ReadOnly = True
                end
              end
              object pnlFilterProcesses: TPanel
                Left = 0
                Top = 0
                Width = 488
                Height = 25
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 2
                object lblFilterProcesses: TLabel
                  Left = 1
                  Top = 4
                  Width = 28
                  Height = 13
                  Caption = 'Filter:'
                end
                object editFilterProcesses: TEdit
                  Left = 34
                  Top = 1
                  Width = 154
                  Height = 21
                  TabOrder = 0
                  OnChange = editFilterVTChange
                end
              end
            end
            object tabCommandStats: TTabSheet
              Caption = 'Command-Statistics'
              ImageIndex = 2
              object ListCommandStats: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 488
                Height = 175
                Align = alClient
                Header.AutoSizeIndex = 4
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.SortColumn = 1
                Header.SortDirection = sdDescending
                HintMode = hmTooltip
                Images = MainForm.PngImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toFullRowSelect]
                OnBeforeCellPaint = ListCommandStatsBeforeCellPaint
                OnBeforePaint = vstBeforePaint
                OnCompareNodes = vstCompareNodes
                OnFreeNode = vstFreeNode
                OnGetText = vstGetText
                OnGetImageIndex = vstGetImageIndex
                OnGetHint = vstGetHint
                OnGetNodeDataSize = vstGetNodeDataSize
                OnHeaderClick = vstHeaderClick
                OnInitNode = vstInitNode
                Columns = <
                  item
                    Position = 0
                    Width = 120
                    WideText = 'Command-type'
                  end
                  item
                    Alignment = taRightJustify
                    Position = 1
                    Width = 100
                    WideText = 'Total count'
                  end
                  item
                    Alignment = taRightJustify
                    Position = 2
                    Width = 100
                    WideText = 'Average per hour'
                  end
                  item
                    Alignment = taRightJustify
                    Position = 3
                    Width = 100
                    WideText = 'Average per second'
                  end
                  item
                    Position = 4
                    Width = 64
                    WideText = 'Percentage'
                  end>
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
          ImageIndex = 5
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
          object ListTables: TVirtualStringTree
            Left = 28
            Top = 17
            Width = 468
            Height = 203
            Align = alClient
            EditDelay = 500
            Header.AutoSizeIndex = -1
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.Height = 20
            Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
            Header.PopupMenu = popupDbGridHeader
            Header.SortColumn = 0
            HintMode = hmTooltip
            Images = MainForm.PngImageListMain
            IncrementalSearch = isInitializedOnly
            ParentShowHint = False
            PopupMenu = popupDbGrid
            ShowHint = True
            TabOrder = 1
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
            TreeOptions.MiscOptions = [toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
            TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
            TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
            OnBeforePaint = vstBeforePaint
            OnChange = ListTablesChange
            OnCompareNodes = vstCompareNodes
            OnDblClick = ListTablesDblClick
            OnFreeNode = vstFreeNode
            OnGetText = vstGetText
            OnGetImageIndex = vstGetImageIndex
            OnGetHint = vstGetHint
            OnGetNodeDataSize = vstGetNodeDataSize
            OnHeaderClick = vstHeaderClick
            OnHeaderDraggedOut = vstHeaderDraggedOut
            OnInitNode = vstInitNode
            OnNewText = ListTablesNewText
            Columns = <
              item
                Position = 0
                Width = 120
                WideText = 'Name'
              end
              item
                Alignment = taRightJustify
                Position = 1
                Width = 70
                WideText = 'Rows'
              end
              item
                Alignment = taRightJustify
                Position = 2
                Width = 70
                WideText = 'Size'
              end
              item
                Position = 3
                Width = 120
                WideText = 'Created'
              end
              item
                Position = 4
                Width = 120
                WideText = 'Updated'
              end
              item
                Position = 5
                Width = 70
                WideText = 'Engine'
              end
              item
                Position = 6
                Width = 100
                WideText = 'Comment'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 7
                WideText = 'Version'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 8
                Width = 70
                WideText = 'Row format'
              end
              item
                Alignment = taRightJustify
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 9
                Width = 70
                WideText = 'Avg row length'
              end
              item
                Alignment = taRightJustify
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 10
                Width = 70
                WideText = 'Max data length'
              end
              item
                Alignment = taRightJustify
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 11
                Width = 70
                WideText = 'Index length'
              end
              item
                Alignment = taRightJustify
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 12
                Width = 70
                WideText = 'Data free'
              end
              item
                Alignment = taRightJustify
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 13
                Width = 90
                WideText = 'Auto increment'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 14
                Width = 120
                WideText = 'Check time'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 15
                Width = 70
                WideText = 'Collation'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 16
                Width = 70
                WideText = 'Checksum'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
                Position = 17
                Width = 70
                WideText = 'Create options'
              end
              item
                Position = 18
                WideText = 'Type'
              end>
          end
          object pnlDatabaseToolbar: TPanel
            Left = 0
            Top = 17
            Width = 28
            Height = 203
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 2
            object tlbDataLeft1: TToolBar
              Left = 2
              Top = 1
              Width = 25
              Height = 123
              Align = alNone
              Caption = 'tlbDataLeft1'
              Color = clBtnFace
              EdgeInner = esNone
              EdgeOuter = esNone
              Images = MainForm.PngImageListMain
              ParentColor = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              Transparent = True
              object btnDbViewData: TToolButton
                Left = 0
                Top = 0
                Hint = 'View Data'
                Caption = 'View Data'
                Enabled = False
                ImageIndex = 41
                Wrap = True
                OnClick = MenuViewDataClick
              end
              object btnDbProperties: TToolButton
                Left = 0
                Top = 22
                Hint = 'Show Table-Properties'
                Caption = 'Show Table-Properties'
                Enabled = False
                ImageIndex = 44
                Wrap = True
                OnClick = ListTablesDblClick
              end
              object btnDbEmptyTable: TToolButton
                Left = 0
                Top = 44
                Hint = 'Empty Table ...'
                Caption = 'btnDbEmptyTable'
                Enabled = False
                ImageIndex = 46
                Wrap = True
                OnClick = EmptyTable
              end
              object btnDbDropTable: TToolButton
                Left = 0
                Top = 66
                Action = MainForm.DropTablesAndViews
                Wrap = True
              end
              object btnDbCopyTable: TToolButton
                Left = 0
                Top = 88
                Action = MainForm.CopyTable
              end
            end
            object tlbDataLeft2: TToolBar
              Left = 2
              Top = 126
              Width = 23
              Height = 29
              Align = alNone
              Caption = 'tlbDataLeft2'
              EdgeInner = esNone
              EdgeOuter = esNone
              Images = MainForm.PngImageListMain
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              Transparent = True
            end
          end
        end
        object tabTable: TTabSheet
          Caption = 'Table'
          ImageIndex = 14
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
          object pnlTableToolbar: TPanel
            Left = 0
            Top = 17
            Width = 28
            Height = 203
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object tlbTableLeft1: TToolBar
              Left = 2
              Top = 1
              Width = 25
              Height = 123
              Align = alNone
              ButtonHeight = 23
              Caption = 'tlbTableLeft1'
              EdgeInner = esNone
              EdgeOuter = esNone
              Images = MainForm.PngImageListMain
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              Transparent = True
              Wrapable = False
              object btnTableViewData: TToolButton
                Left = 0
                Top = 0
                Hint = 'View Data'
                Caption = 'btnTableViewData'
                ImageIndex = 41
                Wrap = True
                OnClick = btnTableViewDataClick
              end
              object btnTableEditField: TToolButton
                Left = 0
                Top = 23
                Hint = 'Edit Field...'
                Caption = 'btnTableEditField'
                ImageIndex = 44
                Wrap = True
                OnClick = UpdateField
              end
              object btnTableAddField: TToolButton
                Left = 0
                Top = 46
                Hint = 'Add Field...'
                Caption = 'btnTableAddField'
                ImageIndex = 45
                Wrap = True
                OnClick = MenuAddFieldClick
              end
              object btnTableDropField: TToolButton
                Left = 0
                Top = 69
                Hint = 'Drop Field ...'
                Caption = 'btnTableDropField'
                ImageIndex = 46
                Wrap = True
                OnClick = DropField
              end
              object btnTableManageIndexes: TToolButton
                Left = 0
                Top = 92
                Hint = 'Manages indexes'
                Caption = 'btnTableManageIndexes'
                ImageIndex = 13
                OnClick = ManageIndexes1Click
              end
            end
            object tlbTableLeft2: TToolBar
              Left = 2
              Top = 129
              Width = 23
              Align = alNone
              ButtonHeight = 23
              Caption = 'ToolBar2'
              EdgeInner = esNone
              EdgeOuter = esNone
              Images = MainForm.PngImageListMain
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              Transparent = True
              Wrapable = False
            end
          end
          object ListColumns: TVirtualStringTree
            Left = 28
            Top = 17
            Width = 468
            Height = 203
            Align = alClient
            EditDelay = 500
            Header.AutoSizeIndex = -1
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.Height = 20
            Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
            HintMode = hmTooltip
            Images = MainForm.PngImageListMain
            IncrementalSearch = isInitializedOnly
            ParentShowHint = False
            PopupMenu = popupTableGrid
            ShowHint = True
            TabOrder = 2
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
            TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
            TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
            OnBeforePaint = vstBeforePaint
            OnCompareNodes = vstCompareNodes
            OnDblClick = UpdateField
            OnFreeNode = vstFreeNode
            OnGetText = vstGetText
            OnGetImageIndex = vstGetImageIndex
            OnGetHint = vstGetHint
            OnGetNodeDataSize = vstGetNodeDataSize
            OnHeaderClick = vstHeaderClick
            OnInitNode = vstInitNode
            OnKeyUp = controlsKeyUp
            OnNewText = ListColumnsNewText
            OnStateChange = ListColumnsStateChange
            Columns = <
              item
                Position = 0
                Width = 120
                WideText = 'Name'
              end
              item
                Position = 1
                Width = 120
                WideText = 'Type'
              end
              item
                Position = 2
                Width = 40
                WideText = 'Null'
              end
              item
                Position = 3
                Width = 115
                WideText = 'Default'
              end
              item
                Position = 4
                Width = 80
                WideText = 'Extra'
              end
              item
                Position = 5
                Width = 120
                WideText = 'Comment'
              end>
          end
        end
        object tabData: TTabSheet
          Caption = 'Data'
          ImageIndex = 41
          object pnlDataTop: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 31
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            TabOrder = 0
            DesignSize = (
              496
              31)
            object btnColumnSelection: TPngSpeedButton
              Left = 187
              Top = 4
              Width = 70
              Height = 21
              AllowAllUp = True
              Anchors = [akTop, akRight]
              GroupIndex = 10
              Caption = 'Columns'
              Layout = blGlyphRight
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
              Caption = 'Data'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Layout = tlCenter
              WordWrap = True
            end
            object btnDataSorting: TPngSpeedButton
              Left = 111
              Top = 4
              Width = 70
              Height = 21
              AllowAllUp = True
              Anchors = [akTop, akRight]
              GroupIndex = 10
              Caption = 'Sorting'
              Layout = blGlyphRight
              OnClick = btnDataClick
            end
            object EditDataSearch: TEdit
              Left = 317
              Top = 4
              Width = 121
              Height = 21
              Anchors = [akTop, akRight]
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
              TabOrder = 2
            end
          end
          object gridData: TTntDBGrid
            Left = 0
            Top = 31
            Width = 496
            Height = 189
            Align = alClient
            DataSource = DataSource1
            Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect]
            PopupMenu = popupDataGrid
            TabOrder = 1
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            OnColEnter = DBGridColEnter
            OnColumnMoved = gridDataColumnMoved
            OnDrawColumnCell = GridDrawColumnCell
            OnDblClick = DBGridDblClick
            OnKeyUp = controlsKeyUp
            OnMouseDown = gridMouseDown
            OnTitleClick = gridDataTitleClick
          end
        end
        object tabQuery: TTabSheet
          Caption = 'Query'
          ImageIndex = 57
          object spltQuery: TSplitter
            Left = 0
            Top = 125
            Width = 496
            Height = 4
            Cursor = crSizeNS
            Align = alTop
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
            TabOrder = 1
            object pnlQueryToolbar: TPanel
              Left = 175
              Top = 1
              Width = 199
              Height = 27
              Align = alRight
              BevelOuter = bvNone
              BorderWidth = 1
              TabOrder = 0
              object ToolBarQuery: TToolBar
                Left = 1
                Top = 1
                Width = 197
                Height = 25
                Align = alClient
                ButtonHeight = 25
                Caption = 'Query'
                Color = clBtnFace
                DragKind = dkDock
                EdgeInner = esNone
                EdgeOuter = esNone
                Images = MainForm.PngImageListMain
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
                  ImageIndex = 52
                  Style = tbsDropDown
                  OnClick = btnQueryLoadClick
                end
                object btnQuerySave: TToolButton
                  Left = 82
                  Top = 0
                  Hint = 'Save SQL to Textfile'
                  Caption = 'Save SQL...'
                  Enabled = False
                  ImageIndex = 10
                  OnClick = btnQuerySaveClick
                end
                object btnQuerySaveSnippet: TToolButton
                  Left = 105
                  Top = 0
                  Hint = 'Save SQL as snippet...'
                  Caption = 'btnQuerySaveSnippet'
                  ImageIndex = 54
                  OnClick = btnQuerySaveSnippetClick
                end
                object btnQueryFind: TToolButton
                  Left = 128
                  Top = 0
                  Hint = 'Find Text...'
                  Caption = 'Find...'
                  ImageIndex = 30
                  OnClick = btnQueryFindClick
                end
                object btnQueryReplace: TToolButton
                  Left = 151
                  Top = 0
                  Hint = 'Search and replace...'
                  Caption = 'Replace ...'
                  ImageIndex = 59
                  OnClick = btnQueryReplaceClick
                end
                object btnQueryStopOnErrors: TToolButton
                  Left = 174
                  Top = 0
                  Hint = 'Stop on MySQL-errors in batch-mode'
                  Caption = 'btnQueryStopOnErrors'
                  Down = True
                  ImageIndex = 63
                  Style = tbsCheck
                  OnClick = btnQueryStopOnErrorsClick
                end
              end
            end
            object PanelCharsInQueryWindow: TPanel
              Left = 103
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
            object PanelQueryDelimiter: TPanel
              Left = 374
              Top = 1
              Width = 121
              Height = 27
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 2
              object LabelQueryDelimiter: TLabel
                Left = 6
                Top = 7
                Width = 55
                Height = 13
                Caption = 'Delimiter:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
              end
              object ComboBoxQueryDelimiter: TComboBox
                Left = 64
                Top = 4
                Width = 55
                Height = 21
                ItemHeight = 13
                TabOrder = 0
                OnExit = ComboBoxQueryDelimiterExit
                Items.Strings = (
                  ';'
                  ';;'
                  '//')
              end
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
            TabOrder = 0
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
              ActiveLineColor = clWindow
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
                Images = MainForm.PngImageListMain
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
                OnGetImageIndex = tabsetQueryHelpersGetImageIndex
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
          object gridQuery: TTntDBGrid
            Left = 0
            Top = 159
            Width = 496
            Height = 61
            Align = alClient
            DataSource = DataSource2
            Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect]
            PopupMenu = popupResultGrid
            TabOrder = 2
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            OnColEnter = DBGridColEnter
            OnDrawColumnCell = GridDrawColumnCell
            OnDblClick = DBGridDblClick
            OnKeyUp = controlsKeyUp
            OnMouseDown = gridMouseDown
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
    Images = MainForm.PngImageListMain
    TabHeight = 20
    TabOrder = 1
    object tabSQLLog: TTabSheet
      Caption = 'SQL Log'
      ImageIndex = 56
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
        Gutter.ShowLineNumbers = True
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
      ImageIndex = 47
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
        Images = MainForm.PngImageListMain
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Transparent = True
        object btnUnsafeEdit: TToolButton
          Left = 0
          Top = 0
          Caption = 'btnUnsafeEdit'
          Enabled = False
          ImageIndex = 63
          Wrap = True
          OnClick = btnUnsafeEditClick
        end
        object btnBlobWordWrap: TToolButton
          Left = 0
          Top = 22
          Hint = 'Wordwrap'
          Caption = 'ToolButton1'
          ImageIndex = 62
          Wrap = True
          Style = tbsCheck
          OnClick = btnBlobWordWrapClick
        end
        object btnBlobLoad: TToolButton
          Left = 0
          Top = 44
          Hint = 'Open|Open file'
          Caption = 'ToolButton4'
          ImageIndex = 52
          Wrap = True
          OnClick = btnBlobLoadClick
        end
        object btnBlobSave: TToolButton
          Left = 0
          Top = 66
          Hint = 'Save|Save to File'
          Caption = 'ToolButton5'
          ImageIndex = 9
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
          ImageIndex = 3
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
      ImageIndex = 53
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
        PopupMenu = popupFilter
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
          Images = MainForm.PngImageListMain
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Transparent = True
          Wrapable = False
          object btnFilterSet: TToolButton
            Left = 0
            Top = 0
            Hint = 'Apply Filter'
            Caption = 'Apply Filter'
            ImageIndex = 55
            OnClick = setFilter
          end
          object btnFilterLoad: TToolButton
            Left = 23
            Top = 0
            Hint = 'Open File'
            DropdownMenu = popupFilterOpenFile
            ImageIndex = 52
            Style = tbsDropDown
            OnClick = btnFilterLoadClick
          end
          object btnFilterSave: TToolButton
            Left = 59
            Top = 0
            Hint = 'Save|Save to File'
            ImageIndex = 10
            OnClick = btnFilterSaveClick
          end
          object btnFilterClear: TToolButton
            Left = 82
            Top = 0
            Hint = 'Clear Filter'
            Caption = 'Clear'
            ImageIndex = 26
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
            ImageIndex = 60
            OnClick = btnFilterPreviousClick
          end
          object btnFilterNext: TToolButton
            Left = 136
            Top = 0
            Hint = 'Next filter'
            Enabled = False
            ImageIndex = 61
            OnClick = btnFilterNextClick
          end
        end
      end
    end
  end
  object popupTreeView: TPopupMenu
    Images = MainForm.PngImageListMain
    OnPopup = popupTreeViewPopup
    Left = 8
    Top = 16
    object NewDatabase1: TMenuItem
      Caption = 'Create database...'
      ImageIndex = 6
      OnClick = CreateDatabase
    end
    object menuAlterdatabase: TMenuItem
      Caption = 'Alter database ...'
      Enabled = False
      ImageIndex = 8
      OnClick = menuAlterdatabaseClick
    end
    object PopupmenuDropDatabase: TMenuItem
      Caption = 'Drop database...'
      ImageIndex = 7
      OnClick = DropDB
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object PopupMenuCreateTable: TMenuItem
      Caption = 'Create table...'
      ImageIndex = 15
      OnClick = CreateTable
    end
    object menuTreeAlterTable: TMenuItem
      Caption = 'Alter table ...'
      Enabled = False
      ImageIndex = 17
      OnClick = menuAlterTableClick
    end
    object menuTreeCreateView: TMenuItem
      Action = MainForm.actCreateView
    end
    object menuTreeEditView: TMenuItem
      Action = MainForm.actEditView
    end
    object PopupMenuDropTable: TMenuItem
      Action = MainForm.DropTablesAndViews
    end
    object Exporttables2: TMenuItem
      Action = MainForm.ExportTables
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Drop1: TMenuItem
      Caption = 'Refresh'
      ImageIndex = 0
      ShortCut = 116
      OnClick = ReadDatabasesAndTables
    end
  end
  object popupDbGrid: TPopupMenu
    AutoHotkeys = maManual
    Images = MainForm.PngImageListMain
    Left = 72
    Top = 16
    object menuproperties: TMenuItem
      Caption = 'Properties'
      Default = True
      Enabled = False
      ImageIndex = 44
      OnClick = ListTablesDblClick
    end
    object menuAlterTable: TMenuItem
      Caption = 'Alter table ...'
      Enabled = False
      ImageIndex = 17
      OnClick = menuAlterTableClick
    end
    object actView1: TMenuItem
      Action = MainForm.actEditView
    end
    object menuviewdata: TMenuItem
      Caption = 'View Data'
      Enabled = False
      ImageIndex = 41
      OnClick = MenuViewDataClick
    end
    object InsertfilesintoBLOBfields1: TMenuItem
      Action = MainForm.InsertFiles
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object menudroptable: TMenuItem
      Action = MainForm.DropTablesAndViews
    end
    object menuemptytable: TMenuItem
      Caption = 'Empty ...'
      Enabled = False
      ImageIndex = 46
      ShortCut = 8238
      OnClick = EmptyTable
    end
    object MenuRenameTable: TMenuItem
      Caption = 'Rename'
      Enabled = False
      ImageIndex = 33
      ShortCut = 113
      OnClick = MenuRenameTableClick
    end
    object menuMaintenance: TMenuItem
      Action = MainForm.actMaintenance
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object MenuCopyTable: TMenuItem
      Action = MainForm.CopyTable
    end
    object menucreatetable: TMenuItem
      Caption = 'Create new Table...'
      ImageIndex = 15
      OnClick = CreateTable
    end
    object Createview1: TMenuItem
      Action = MainForm.actCreateView
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
      ImageIndex = 0
      ShortCut = 116
      OnClick = MenuRefreshClick
    end
    object selectall1: TMenuItem
      Caption = 'select all'
      ShortCut = 16449
      Visible = False
      OnClick = selectall1Click
    end
  end
  object popupHost: TPopupMenu
    Images = MainForm.PngImageListMain
    OnPopup = popupHostPopup
    Left = 41
    Top = 16
    object Kill1: TMenuItem
      Caption = 'Kill Process(es)...'
      Enabled = False
      ImageIndex = 26
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
        ShortCut = 16500
        OnClick = EnableAutoRefreshClick
      end
      object DisableAutoRefresh: TMenuItem
        Caption = 'Inactive'
        Checked = True
        RadioItem = True
        ShortCut = 27
        OnClick = DisableAutoRefreshClick
      end
    end
    object menuEditVariable: TMenuItem
      Caption = 'Edit ...'
      ImageIndex = 33
      ShortCut = 13
      OnClick = menuEditVariableClick
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
      ImageIndex = 0
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
    Images = MainForm.PngImageListMain
    Left = 104
    Top = 16
    object MenuEditField: TMenuItem
      Caption = 'Properties'
      Default = True
      ImageIndex = 44
      ShortCut = 32781
      OnClick = UpdateField
    end
    object MenuAddField: TMenuItem
      Caption = 'Add Field...'
      ImageIndex = 45
      ShortCut = 16449
      OnClick = MenuAddFieldClick
    end
    object DropField1: TMenuItem
      Caption = 'Drop Field(s)...'
      Hint = 'Delete Field(s) from Table'
      ImageIndex = 46
      ShortCut = 16430
      OnClick = DropField
    end
    object menuRenameColumn: TMenuItem
      Caption = 'Rename Field'
      ImageIndex = 33
      ShortCut = 113
      OnClick = menuRenameColumnClick
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object ManageIndexes1: TMenuItem
      Caption = '&Manage Indexes...'
      ImageIndex = 13
      OnClick = ManageIndexes1Click
    end
    object N3: TMenuItem
      Caption = '-'
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
      ImageIndex = 0
      ShortCut = 116
      OnClick = RefreshFieldListClick
    end
  end
  object popupDataGrid: TPopupMenu
    AutoHotkeys = maManual
    Images = MainForm.PngImageListMain
    OnPopup = popupDataGridPopup
    Left = 8
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
      ImageIndex = 26
      ShortCut = 16430
      OnClick = Delete1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object QuickFilter1: TMenuItem
      Caption = 'Quick Filter'
      ImageIndex = 53
      object QF1: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column = Value'
        ImageIndex = 61
        OnClick = QuickFilterClick
      end
      object QF2: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column != Value'
        ImageIndex = 61
        OnClick = QuickFilterClick
      end
      object QF3: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column > Value'
        ImageIndex = 61
        OnClick = QuickFilterClick
      end
      object QF4: TMenuItem
        AutoHotkeys = maManual
        Caption = 'Column < Value'
        ImageIndex = 61
        OnClick = QuickFilterClick
      end
      object QF5: TMenuItem
        Caption = 'Column LIKE Value%'
        ImageIndex = 61
        OnClick = QuickFilterClick
      end
      object QF6: TMenuItem
        Caption = 'Column LIKE %Value'
        ImageIndex = 61
        OnClick = QuickFilterClick
      end
      object QF7: TMenuItem
        Caption = 'Column LIKE %Value%'
        ImageIndex = 61
        OnClick = QuickFilterClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object QF8: TMenuItem
        Caption = 'Column = ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF9: TMenuItem
        Caption = 'Column != ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF10: TMenuItem
        Caption = 'Column > ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF11: TMenuItem
        Caption = 'Column < ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF12: TMenuItem
        Caption = 'Column like ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object N7: TMenuItem
        AutoHotkeys = maManual
        Caption = '-'
      end
      object QF13: TMenuItem
        Caption = 'Column = CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF14: TMenuItem
        Caption = 'Column != CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF15: TMenuItem
        Caption = 'Column > CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF16: TMenuItem
        Caption = 'Column < CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF17: TMenuItem
        Caption = 'Column LIKE %CLIPBOARD%'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object DropFilter1: TMenuItem
        Caption = 'Drop Filter'
        ImageIndex = 26
        OnClick = DropFilter1Click
      end
    end
    object Filter1: TMenuItem
      Caption = 'Filter...'
      OnClick = Filter1Click
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
      ImageIndex = 80
      object DataDateTime: TMenuItem
        Caption = 'datetime'
        Hint = 'Insert datetime-value'
        ImageIndex = 80
        OnClick = InsertDate
      end
      object DataDate: TMenuItem
        Caption = 'date'
        Hint = 'Insert date-value'
        ImageIndex = 80
        OnClick = InsertDate
      end
      object DataTime: TMenuItem
        Caption = 'time'
        Hint = 'Insert time-value'
        ImageIndex = 80
        OnClick = InsertDate
      end
      object DataTimestamp: TMenuItem
        Caption = 'timestamp'
        Hint = 'Insert timestamp-value'
        ImageIndex = 80
        OnClick = InsertDate
      end
      object DataYear: TMenuItem
        Caption = 'year'
        Hint = 'Insert year-value'
        ImageIndex = 80
        OnClick = InsertDate
      end
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
    object menuSQLhelpData: TMenuItem
      Caption = 'Lookup field datatype in SQL help ...'
      ImageIndex = 31
      ShortCut = 112
      OnClick = menuSQLhelpClick
    end
    object Refresh3: TMenuItem
      Tag = 28
      Caption = 'Refresh'
      ImageIndex = 0
      ShortCut = 116
      OnClick = viewdata
    end
  end
  object popupResultGrid: TPopupMenu
    Images = MainForm.PngImageListMain
    OnPopup = popupResultGridPopup
    Left = 40
    Top = 48
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
    Images = MainForm.PngImageListMain
    Left = 72
    Top = 48
    object Copy1: TMenuItem
      Action = MainForm.EditCopy1
    end
    object Clear2: TMenuItem
      Caption = 'Clear'
      ImageIndex = 58
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
      ImageIndex = 33
      OnClick = EditQuery1Click
    end
    object Saveastextfile1: TMenuItem
      Caption = 'Save as textfile...'
      ImageIndex = 10
      OnClick = Saveastextfile1Click
    end
    object menuLogToFile: TMenuItem
      Caption = 'Log to file'
      OnClick = menuLogToFileClick
    end
    object menuOpenLogFolder: TMenuItem
      Caption = 'Open log folder ...'
      Enabled = False
      ImageIndex = 51
      OnClick = menuOpenLogFolderClick
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
    Left = 48
    Top = 318
  end
  object OpenDialog2: TOpenDialog
    Left = 48
    Top = 350
  end
  object TimerHost: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = ShowVariablesAndProcesses
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
    Left = 104
    Top = 48
  end
  object SynCompletionProposal1: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 262
    EndOfTokenChr = '()[]. ='#9
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
        BiggestWordW = 'databasemore'
      end
      item
        BiggestWord = 'IHAVENOCLUEWHICHLENGTH'
        DefaultFontStyle = [fsBold]
        BiggestWordW = 'IHAVENOCLUEWHICHLENGTH'
      end>
    ItemHeight = 18
    Images = MainForm.PngImageListMain
    Margin = 1
    OnExecute = SynCompletionProposal1Execute
    ShortCut = 16416
    Editor = SynMemoQuery
    TimerInterval = 500
    OnAfterCodeCompletion = SynCompletionProposal1AfterCodeCompletion
    OnCodeCompletion = SynCompletionProposal1CodeCompletion
    Left = 40
    Top = 192
    EndOfTokenChrW = '()[]. ='#9
    TriggerCharsW = '.'
  end
  object popupQueryLoad: TPopupMenu
    Left = 8
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
    Images = MainForm.PngImageListMain
    OnPopup = popupQueryPopup
    Left = 40
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
      ImageIndex = 58
      ShortCut = 16471
      OnClick = menuclearClick
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object MenuFind: TMenuItem
      Caption = 'Find...'
      ImageIndex = 30
      ShortCut = 16454
      OnClick = btnQueryFindClick
    end
    object MenuReplace: TMenuItem
      Caption = 'Replace ...'
      Hint = 'Search and replace...'
      ImageIndex = 59
      ShortCut = 16466
      OnClick = btnQueryReplaceClick
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object menuload: TMenuItem
      Caption = 'Load from file ...'
      ImageIndex = 52
      ShortCut = 16463
      OnClick = btnQueryLoadClick
    end
    object menuInsertFileAtCursor: TMenuItem
      Caption = 'Insert file at cursor ...'
      ImageIndex = 52
      ShortCut = 24655
      OnClick = menuInsertFileAtCursorClick
    end
    object menusave: TMenuItem
      Caption = 'Save to file ...'
      ImageIndex = 10
      ShortCut = 16467
      OnClick = btnQuerySaveClick
    end
    object menuSaveSelectionToFile: TMenuItem
      Tag = 1
      Caption = 'Save selection to file ...'
      ImageIndex = 10
      ShortCut = 24659
      OnClick = btnQuerySaveClick
    end
    object menuSaveAsSnippet: TMenuItem
      Caption = 'Save as snippet ...'
      ImageIndex = 54
      OnClick = btnQuerySaveSnippetClick
    end
    object menuSaveSelectionAsSnippet: TMenuItem
      Tag = 1
      Caption = 'Save selection as snippet ...'
      ImageIndex = 54
      OnClick = btnQuerySaveSnippetClick
    end
    object N23: TMenuItem
      Caption = '-'
    end
    object menuSQLhelp: TMenuItem
      Caption = 'Lookup in SQL help ...'
      ImageIndex = 31
      ShortCut = 112
      OnClick = menuSQLhelpClick
    end
    object N24: TMenuItem
      Caption = '-'
    end
  end
  object popupQueryHelpers: TPopupMenu
    Images = MainForm.PngImageListMain
    Left = 72
    Top = 80
    object menuInsertSnippetAtCursor: TMenuItem
      Caption = 'Insert at cursor'
      Default = True
      Enabled = False
      ImageIndex = 52
      OnClick = menuInsertSnippetAtCursorClick
    end
    object menuLoadSnippet: TMenuItem
      Caption = 'Load'
      Enabled = False
      ImageIndex = 52
      OnClick = menuLoadSnippetClick
    end
    object menuDeleteSnippet: TMenuItem
      Caption = 'Delete ...'
      Enabled = False
      ImageIndex = 26
      ShortCut = 46
      OnClick = menuDeleteSnippetClick
    end
    object menuExplore: TMenuItem
      Caption = 'Explore folder'
      Enabled = False
      ImageIndex = 51
      OnClick = menuExploreClick
    end
    object menuHelp: TMenuItem
      Caption = 'Help'
      ImageIndex = 31
      ShortCut = 112
      OnClick = CallSQLHelp
    end
  end
  object popupFilter: TPopupMenu
    Images = MainForm.PngImageListMain
    OnPopup = popupFilterPopup
    Left = 80
    Top = 318
    object menuApplyFilter: TMenuItem
      Caption = 'Appply filter ...'
      ImageIndex = 55
      ShortCut = 120
      OnClick = setFilter
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object menuFilterCopy: TMenuItem
      Action = MainForm.EditCopy1
    end
    object menuFilterPaste: TMenuItem
      Action = MainForm.EditPaste1
    end
    object menuFilterClear: TMenuItem
      Caption = 'Clear'
      ImageIndex = 58
      ShortCut = 16471
      OnClick = menuclearClick
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object menuFilterSQLhelp: TMenuItem
      Caption = 'Lookup in SQL help'
      ImageIndex = 31
      OnClick = menuSQLhelpClick
    end
    object N25: TMenuItem
      Caption = '-'
    end
  end
end
