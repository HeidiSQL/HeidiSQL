object MainForm: TMainForm
  Left = 241
  Top = 114
  ClientHeight = 486
  ClientWidth = 824
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesigned
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spltTopBottom: TSplitter
    Left = 0
    Top = 383
    Width = 824
    Height = 4
    Cursor = crSizeNS
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  object SynMemoSQLLog: TSynMemo
    Left = 0
    Top = 387
    Width = 824
    Height = 80
    SingleLineMode = True
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = popupSqlLog
    TabOrder = 1
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Terminal'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 2
    Gutter.ShowLineNumbers = True
    Highlighter = SynSQLSyn1
    Options = [eoAutoIndent, eoDragDropEditing, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces]
    ReadOnly = True
    RightEdge = 0
    ScrollBars = ssVertical
    FontSmoothing = fsmNone
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 467
    Width = 824
    Height = 19
    AutoHint = True
    DoubleBuffered = True
    Panels = <
      item
        Width = 150
      end
      item
        Width = 110
      end
      item
        Style = psOwnerDraw
        Width = 140
      end
      item
        Style = psOwnerDraw
        Width = 170
      end
      item
        Width = 170
      end
      item
        Style = psOwnerDraw
        Width = 170
      end
      item
        Style = psOwnerDraw
        Width = 250
      end>
    ParentDoubleBuffered = False
    OnClick = StatusBarClick
    OnMouseLeave = StatusBarMouseLeave
    OnMouseMove = StatusBarMouseMove
    OnDrawPanel = StatusBarDrawPanel
  end
  object panelTop: TPanel
    Left = 0
    Top = 54
    Width = 824
    Height = 329
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    OnDblClick = panelTopDblClick
    object spltDBtree: TSplitter
      Left = 169
      Top = 0
      Width = 4
      Height = 329
      Cursor = crSizeWE
      ResizeStyle = rsUpdate
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 169
      Height = 329
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = pnlLeftResize
      object spltPreview: TSplitter
        Left = 0
        Top = 225
        Width = 169
        Height = 4
        Cursor = crSizeNS
        Align = alBottom
        ResizeStyle = rsUpdate
        Visible = False
        OnMoved = spltPreviewMoved
      end
      object DBtree: TVirtualStringTree
        Left = 0
        Top = 22
        Width = 169
        Height = 203
        Align = alClient
        Constraints.MinWidth = 40
        DragMode = dmAutomatic
        DragType = dtVCL
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
        HintMode = hmTooltip
        HotCursor = crHandPoint
        Images = ImageListMain
        IncrementalSearch = isInitializedOnly
        Indent = 12
        ParentShowHint = False
        PopupMenu = popupDB
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseExplorerTheme, toHideTreeLinesIfThemed]
        TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
        OnAfterCellPaint = DBtreeAfterCellPaint
        OnBeforeCellPaint = DBtreeBeforeCellPaint
        OnChange = DBtreeChange
        OnDblClick = DBtreeDblClick
        OnExpanded = DBtreeExpanded
        OnExpanding = DBtreeExpanding
        OnFocusChanged = DBtreeFocusChanged
        OnFocusChanging = DBtreeFocusChanging
        OnFreeNode = DBtreeFreeNode
        OnGetText = DBtreeGetText
        OnPaintText = DBtreePaintText
        OnGetImageIndex = DBtreeGetImageIndex
        OnGetHint = AnyGridGetHint
        OnGetNodeDataSize = DBtreeGetNodeDataSize
        OnInitChildren = DBtreeInitChildren
        OnInitNode = DBtreeInitNode
        OnMouseUp = DBtreeMouseUp
        Columns = <
          item
            Position = 0
            Width = 165
            WideText = 'Name'
          end
          item
            Alignment = taRightJustify
            MinWidth = 0
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
            Position = 1
            Width = 55
            WideText = 'Size'
          end>
        WideDefaultText = ''
      end
      object pnlPreview: TPanel
        Left = 0
        Top = 229
        Width = 169
        Height = 100
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        DesignSize = (
          169
          100)
        object imgPreview: TImage
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 169
          Height = 75
          Margins.Left = 0
          Margins.Top = 25
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          Center = True
          Proportional = True
          Stretch = True
        end
        object lblPreviewTitle: TLabel
          Left = 3
          Top = 0
          Width = 85
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preview ...'
          EllipsisPosition = epEndEllipsis
          ParentShowHint = False
          ShowAccelChar = False
          ShowHint = True
          Layout = tlCenter
        end
        object ToolBarPreview: TToolBar
          Left = 100
          Top = 1
          Width = 70
          Height = 23
          Align = alNone
          Anchors = [akTop, akRight]
          Caption = 'ToolBarPreview'
          Images = ImageListMain
          TabOrder = 0
          Wrapable = False
          object btnPreviewCopy: TToolButton
            Left = 0
            Top = 0
            Action = actCopy
          end
          object btnPreviewSaveToFile: TToolButton
            Left = 23
            Top = 0
            Action = actDataSaveBlobToFile
          end
          object btnPreviewClose: TToolButton
            Left = 46
            Top = 0
            Hint = 'Close preview'
            Caption = 'btnPreviewClose'
            ImageIndex = 26
            OnClick = actDataPreviewExecute
          end
        end
      end
      object ToolBarTree: TToolBar
        Left = 0
        Top = 0
        Width = 169
        Height = 22
        Caption = 'ToolBarTree'
        Images = ImageListMain
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Wrapable = False
        object editDatabaseFilter: TButtonedEdit
          Left = 0
          Top = 0
          Width = 50
          Height = 22
          Hint = 
            'Database filter|A list of databases, separated by semicolon. Can' +
            ' contain regular expressions, e.g. "mydb;test.*;project\d+".'
          Images = ImageListMain
          LeftButton.ImageIndex = 5
          LeftButton.Visible = True
          RightButton.ImageIndex = 26
          TabOrder = 0
          TextHint = 'Database filter'
          OnChange = editDatabaseTableFilterChange
          OnExit = editDatabaseTableFilterExit
          OnKeyPress = editDatabaseTableFilterKeyPress
          OnLeftButtonClick = editDatabaseTableFilterLeftButtonClick
          OnRightButtonClick = editDatabaseTableFilterRightButtonClick
        end
        object editTableFilter: TButtonedEdit
          Left = 50
          Top = 0
          Width = 50
          Height = 22
          Hint = 'Table filter|Can contain regular expressions, e.g. "phpbb_\d"'
          Images = ImageListMain
          LeftButton.ImageIndex = 14
          LeftButton.Visible = True
          RightButton.ImageIndex = 26
          TabOrder = 1
          TextHint = 'Table filter'
          OnChange = editDatabaseTableFilterChange
          OnExit = editDatabaseTableFilterExit
          OnKeyPress = editDatabaseTableFilterKeyPress
          OnLeftButtonClick = editDatabaseTableFilterLeftButtonClick
          OnRightButtonClick = editDatabaseTableFilterRightButtonClick
        end
        object btnTreeFavorites: TToolButton
          Left = 100
          Top = 0
          Action = actFavoriteObjectsOnly
        end
      end
    end
    object pnlRight: TPanel
      Left = 173
      Top = 0
      Width = 651
      Height = 329
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pnlFilterVT: TPanel
        Left = 0
        Top = 303
        Width = 651
        Height = 26
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        Visible = False
        object lblFilterVT: TLabel
          Left = 36
          Top = 6
          Width = 28
          Height = 13
          Caption = 'Filter:'
        end
        object lblFilterVTInfo: TLabel
          Left = 239
          Top = 6
          Width = 66
          Height = 13
          Caption = 'lblFilterVTInfo'
        end
        object btnCloseFilterPanel: TSpeedButton
          Left = 5
          Top = 4
          Width = 16
          Height = 16
          Hint = 'Hides the filter panel'
          Flat = True
          OnClick = actFilterPanelExecute
        end
        object editFilterVT: TButtonedEdit
          Left = 70
          Top = 3
          Width = 154
          Height = 21
          Images = ImageListMain
          RightButton.Hint = 'Clear filter'
          RightButton.ImageIndex = 26
          RightButton.Visible = True
          TabOrder = 0
          OnChange = editFilterVTChange
          OnRightButtonClick = editFilterVTRightButtonClick
        end
      end
      object PageControlMain: TPageControl
        Left = 0
        Top = 0
        Width = 651
        Height = 303
        ActivePage = tabHost
        Align = alClient
        HotTrack = True
        Images = ImageListMain
        MultiLine = True
        PopupMenu = popupMainTabs
        TabOrder = 1
        OnChange = PageControlMainChange
        OnChanging = PageControlMainChanging
        OnContextPopup = PageControlMainContextPopup
        OnMouseUp = PageControlMainMouseUp
        object tabHost: TTabSheet
          Caption = 'Host'
          ImageIndex = 1
          object PageControlHost: TPageControl
            Left = 0
            Top = 0
            Width = 643
            Height = 274
            ActivePage = tabDatabases
            Align = alClient
            HotTrack = True
            Images = ImageListMain
            TabOrder = 0
            OnChange = PageControlHostChange
            object tabDatabases: TTabSheet
              Caption = 'Databases'
              ImageIndex = 5
              object ListDatabases: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 635
                Height = 245
                Align = alClient
                Header.AutoSizeIndex = 0
                Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.ParentFont = True
                Header.PopupMenu = popupListHeader
                Header.SortColumn = 0
                Images = ImageListMain
                PopupMenu = popupHost
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseExplorerTheme, toHideTreeLinesIfThemed]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
                OnAfterPaint = AnyGridAfterPaint
                OnBeforeCellPaint = ListDatabasesBeforeCellPaint
                OnBeforePaint = ListDatabasesBeforePaint
                OnCompareNodes = AnyGridCompareNodes
                OnDblClick = ListDatabasesDblClick
                OnGetText = ListDatabasesGetText
                OnGetImageIndex = ListDatabasesGetImageIndex
                OnGetHint = AnyGridGetHint
                OnGetNodeDataSize = ListDatabasesGetNodeDataSize
                OnHeaderClick = AnyGridHeaderClick
                OnHeaderDraggedOut = AnyGridHeaderDraggedOut
                OnInitNode = ListDatabasesInitNode
                Columns = <
                  item
                    Position = 0
                    Width = 150
                    WideText = 'Database'
                  end
                  item
                    Position = 1
                    Width = 80
                    WideText = 'Size'
                  end
                  item
                    Position = 2
                    WideText = 'Items'
                  end
                  item
                    Position = 3
                    WideText = 'Last modification'
                  end
                  item
                    Position = 4
                    WideText = 'Tables'
                  end
                  item
                    Position = 5
                    WideText = 'Views'
                  end
                  item
                    Position = 6
                    WideText = 'Functions'
                  end
                  item
                    Position = 7
                    WideText = 'Procedures'
                  end
                  item
                    Position = 8
                    WideText = 'Triggers'
                  end
                  item
                    Position = 9
                    WideText = 'Events'
                  end
                  item
                    Position = 10
                    Width = 120
                    WideText = 'Default collation'
                  end>
              end
            end
            object tabVariables: TTabSheet
              Caption = 'Variables'
              ImageIndex = 137
              object ListVariables: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 635
                Height = 245
                Align = alClient
                DragOperations = []
                Header.AutoSizeIndex = 2
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.ParentFont = True
                Header.PopupMenu = popupListHeader
                Header.SortColumn = 0
                HintMode = hmTooltip
                Images = ImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toToggleOnDblClick]
                TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
                OnAfterPaint = AnyGridAfterPaint
                OnBeforeCellPaint = ListVariablesBeforeCellPaint
                OnBeforePaint = HostListBeforePaint
                OnCompareNodes = AnyGridCompareNodes
                OnDblClick = ListVariablesDblClick
                OnGetText = HostListGetText
                OnPaintText = ListVariablesPaintText
                OnGetImageIndex = HostListGetImageIndex
                OnGetHint = AnyGridGetHint
                OnGetNodeDataSize = AnyGridGetNodeDataSize
                OnHeaderClick = AnyGridHeaderClick
                OnHeaderDraggedOut = AnyGridHeaderDraggedOut
                OnInitNode = AnyGridInitNode
                Columns = <
                  item
                    Position = 0
                    Width = 160
                    WideText = 'Variable'
                  end
                  item
                    Position = 1
                    Width = 200
                    WideText = 'Session'
                  end
                  item
                    Position = 2
                    Width = 271
                    WideText = 'Global'
                  end>
              end
            end
            object tabStatus: TTabSheet
              Caption = 'Status'
              ImageIndex = 13
              object ListStatus: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 635
                Height = 245
                Align = alClient
                DragOperations = []
                Header.AutoSizeIndex = 1
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.ParentFont = True
                Header.PopupMenu = popupListHeader
                Header.SortColumn = 0
                HintMode = hmTooltip
                Images = ImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toToggleOnDblClick]
                TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
                OnAfterPaint = AnyGridAfterPaint
                OnBeforePaint = HostListBeforePaint
                OnCompareNodes = AnyGridCompareNodes
                OnGetText = HostListGetText
                OnGetImageIndex = HostListGetImageIndex
                OnGetHint = AnyGridGetHint
                OnGetNodeDataSize = AnyGridGetNodeDataSize
                OnHeaderClick = AnyGridHeaderClick
                OnHeaderDraggedOut = AnyGridHeaderDraggedOut
                OnInitNode = AnyGridInitNode
                Columns = <
                  item
                    Position = 0
                    Width = 160
                    WideText = 'Variable'
                  end
                  item
                    Alignment = taRightJustify
                    Position = 1
                    Width = 271
                    WideText = 'Value'
                  end
                  item
                    Alignment = taRightJustify
                    Position = 2
                    Width = 100
                    WideText = 'Avg per hour'
                  end
                  item
                    Alignment = taRightJustify
                    Position = 3
                    Width = 100
                    WideText = 'Avg per second'
                  end>
              end
            end
            object tabProcessList: TTabSheet
              Caption = 'Processes'
              ImageIndex = 57
              object spltProcessList: TSplitter
                Left = 0
                Top = 172
                Width = 635
                Height = 4
                Cursor = crSizeNS
                Align = alBottom
                ResizeStyle = rsUpdate
              end
              object ListProcesses: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 635
                Height = 172
                Align = alClient
                Header.AutoSizeIndex = 7
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.ParentFont = True
                Header.PopupMenu = popupListHeader
                Header.SortColumn = 0
                Header.SortDirection = sdDescending
                HintMode = hmTooltip
                Images = ImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
                TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
                OnAfterPaint = AnyGridAfterPaint
                OnBeforeCellPaint = HostListBeforeCellPaint
                OnBeforePaint = HostListBeforePaint
                OnCompareNodes = AnyGridCompareNodes
                OnFocusChanged = ListProcessesFocusChanged
                OnGetText = HostListGetText
                OnGetImageIndex = HostListGetImageIndex
                OnGetHint = AnyGridGetHint
                OnGetNodeDataSize = AnyGridGetNodeDataSize
                OnHeaderClick = AnyGridHeaderClick
                OnHeaderDraggedOut = AnyGridHeaderDraggedOut
                OnInitNode = AnyGridInitNode
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
                    Width = 141
                    WideText = 'Info'
                  end>
              end
              object pnlProcessViewBox: TPanel
                Left = 0
                Top = 176
                Width = 635
                Height = 69
                Align = alBottom
                BevelOuter = bvNone
                TabOrder = 1
                object pnlProcessView: TPanel
                  Left = 0
                  Top = 0
                  Width = 635
                  Height = 18
                  Align = alTop
                  Alignment = taLeftJustify
                  BevelOuter = bvNone
                  Caption = 'Process SQL:'
                  TabOrder = 0
                  object lblExplainProcess: TLabel
                    Left = 87
                    Top = 2
                    Width = 41
                    Height = 13
                    Cursor = crHandPoint
                    Hint = 'Analyze this query'
                    Caption = 'EXPLAIN'
                    Enabled = False
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clBlue
                    Font.Height = -11
                    Font.Name = 'Default'
                    Font.Style = [fsUnderline]
                    ParentFont = False
                    OnClick = lblExplainProcessClick
                  end
                  object lblExplainProcessAnalyzer: TLabel
                    Left = 142
                    Top = 2
                    Width = 163
                    Height = 13
                    Cursor = crHandPoint
                    Hint = 'Analyze this query on MariaDB.org'
                    Caption = 'EXPLAIN Analyzer on MariaDB.org'
                    Enabled = False
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clBlue
                    Font.Height = -11
                    Font.Name = 'Default'
                    Font.Style = [fsUnderline]
                    ParentFont = False
                    OnClick = lblExplainProcessAnalyzerClick
                  end
                end
                object SynMemoProcessView: TSynMemo
                  Left = 0
                  Top = 18
                  Width = 635
                  Height = 51
                  SingleLineMode = False
                  Align = alClient
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -13
                  Font.Name = 'Courier New'
                  Font.Style = []
                  TabOrder = 1
                  Gutter.AutoSize = True
                  Gutter.DigitCount = 2
                  Gutter.Font.Charset = DEFAULT_CHARSET
                  Gutter.Font.Color = clWindowText
                  Gutter.Font.Height = -11
                  Gutter.Font.Name = 'Courier New'
                  Gutter.Font.Style = []
                  Gutter.LeftOffset = 2
                  Gutter.ShowLineNumbers = True
                  Highlighter = SynSQLSyn1
                  Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
                  ReadOnly = True
                  RightEdge = 0
                  WordWrap = True
                  FontSmoothing = fsmNone
                end
              end
            end
            object tabCommandStats: TTabSheet
              Caption = 'Command-Statistics'
              ImageIndex = 145
              object ListCommandStats: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 635
                Height = 245
                Align = alClient
                Header.AutoSizeIndex = 4
                Header.Height = 20
                Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
                Header.ParentFont = True
                Header.PopupMenu = popupListHeader
                Header.SortColumn = 1
                Header.SortDirection = sdDescending
                HintMode = hmTooltip
                Images = ImageListMain
                IncrementalSearch = isInitializedOnly
                ParentShowHint = False
                PopupMenu = popupHost
                ShowHint = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
                TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
                OnAfterPaint = AnyGridAfterPaint
                OnBeforeCellPaint = HostListBeforeCellPaint
                OnBeforePaint = HostListBeforePaint
                OnCompareNodes = AnyGridCompareNodes
                OnGetText = HostListGetText
                OnGetImageIndex = HostListGetImageIndex
                OnGetHint = AnyGridGetHint
                OnGetNodeDataSize = AnyGridGetNodeDataSize
                OnHeaderClick = AnyGridHeaderClick
                OnHeaderDraggedOut = AnyGridHeaderDraggedOut
                OnInitNode = AnyGridInitNode
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
                    Width = 211
                    WideText = 'Percentage'
                  end>
              end
            end
          end
        end
        object tabDatabase: TTabSheet
          Caption = 'Database'
          ImageIndex = 5
          object ListTables: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 643
            Height = 274
            Align = alClient
            EditDelay = 500
            Header.AutoSizeIndex = -1
            Header.Height = 20
            Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
            Header.ParentFont = True
            Header.PopupMenu = popupListHeader
            Header.SortColumn = 0
            HintMode = hmTooltip
            Images = ImageListMain
            IncrementalSearch = isInitializedOnly
            ParentShowHint = False
            PopupMenu = popupDB
            ShowHint = True
            TabOrder = 0
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
            TreeOptions.MiscOptions = [toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
            TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
            TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
            OnAfterPaint = AnyGridAfterPaint
            OnBeforeCellPaint = ListTablesBeforeCellPaint
            OnBeforePaint = ListTablesBeforePaint
            OnChange = ListTablesChange
            OnCompareNodes = AnyGridCompareNodes
            OnDblClick = ListTablesDblClick
            OnEditing = ListTablesEditing
            OnGetText = ListTablesGetText
            OnGetImageIndex = ListTablesGetImageIndex
            OnGetHint = AnyGridGetHint
            OnGetNodeDataSize = ListTablesGetNodeDataSize
            OnHeaderClick = AnyGridHeaderClick
            OnHeaderDraggedOut = AnyGridHeaderDraggedOut
            OnInitNode = ListTablesInitNode
            OnKeyPress = ListTablesKeyPress
            OnNewText = ListTablesNewText
            Columns = <
              item
                Position = 0
                Width = 120
                WideText = 'Name'
              end
              item
                Position = 1
                Width = 70
                WideText = 'Rows'
              end
              item
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
        end
        object tabEditor: TTabSheet
          Caption = 'Table'
          ImageIndex = 14
        end
        object tabData: TTabSheet
          Caption = 'Data'
          ImageIndex = 41
          object lblSorryNoData: TLabel
            Left = 0
            Top = 91
            Width = 643
            Height = 183
            Align = alClient
            Alignment = taCenter
            Caption = 'No data available for this item.'
            Layout = tlCenter
            WordWrap = True
            ExplicitWidth = 147
            ExplicitHeight = 13
          end
          object pnlDataTop: TPanel
            Left = 0
            Top = 0
            Width = 643
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            BorderWidth = 1
            TabOrder = 0
            object lblDataTop: TLabel
              Left = 1
              Top = 1
              Width = 361
              Height = 23
              Align = alLeft
              Anchors = [akLeft, akTop, akRight, akBottom]
              AutoSize = False
              Caption = 'Data'
              Layout = tlCenter
              WordWrap = True
            end
            object tlbDataButtons: TToolBar
              Left = 299
              Top = 1
              Width = 343
              Height = 23
              Align = alRight
              AutoSize = True
              ButtonWidth = 67
              Caption = 'tlbDataButtons'
              Images = ImageListMain
              List = True
              ParentShowHint = False
              ShowCaptions = True
              ShowHint = True
              TabOrder = 0
              Wrapable = False
              object tbtnDataNext: TToolButton
                Left = 0
                Top = 0
                Action = actDataShowNext
              end
              object tbtnDataShowAll: TToolButton
                Left = 67
                Top = 0
                Action = actDataShowAll
              end
              object ToolButton2: TToolButton
                Left = 134
                Top = 0
                Width = 8
                Caption = 'ToolButton2'
                ImageIndex = 108
                Style = tbsSeparator
              end
              object tbtnDataSorting: TToolButton
                Left = 142
                Top = 0
                AllowAllUp = True
                Caption = 'Sorting'
                ImageIndex = 107
                Style = tbsTextButton
                OnClick = btnDataClick
              end
              object tbtnDataColumns: TToolButton
                Left = 209
                Top = 0
                AllowAllUp = True
                Caption = 'Columns'
                ImageIndex = 107
                Style = tbsTextButton
                OnClick = btnDataClick
              end
              object tbtnDataFilter: TToolButton
                Left = 276
                Top = 0
                AllowAllUp = True
                Caption = 'Filter'
                ImageIndex = 107
                OnClick = btnDataClick
              end
            end
          end
          object pnlFilter: TPanel
            Left = 0
            Top = 25
            Width = 643
            Height = 66
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            Visible = False
            DesignSize = (
              643
              66)
            object lblTableFilter: TLabel
              Left = 480
              Top = 0
              Width = 123
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Create multi column filter:'
            end
            object lblRecentFilters: TLabel
              Left = 1
              Top = 3
              Width = 68
              Height = 13
              Caption = 'Recent filters:'
            end
            object btnFilterApply: TButton
              Left = 480
              Top = 41
              Width = 76
              Height = 22
              Action = actApplyFilter
              Anchors = [akTop, akRight]
              TabOrder = 2
            end
            object btnFilterClear: TButton
              Left = 560
              Top = 41
              Width = 76
              Height = 22
              Action = actClearFilterEditor
              Anchors = [akTop, akRight]
              TabOrder = 3
            end
            object SynMemoFilter: TSynMemo
              Left = 0
              Top = 21
              Width = 477
              Height = 42
              SingleLineMode = False
              Anchors = [akLeft, akTop, akRight, akBottom]
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
              Gutter.Visible = False
              Gutter.Width = 0
              Highlighter = SynSQLSyn1
              Options = [eoAutoIndent, eoDragDropEditing, eoDropFiles, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTabIndent]
              RightEdge = 0
              ScrollBars = ssVertical
              WantTabs = True
              WordWrap = True
              OnStatusChange = SynMemoFilterStatusChange
              FontSmoothing = fsmNone
            end
            object editFilterSearch: TEdit
              Left = 480
              Top = 15
              Width = 156
              Height = 21
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnChange = editFilterSearchChange
              OnEnter = editFilterSearchEnter
              OnExit = editFilterSearchExit
            end
            object comboRecentFilters: TComboBox
              Left = 77
              Top = 0
              Width = 400
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 4
              OnSelect = LoadRecentFilter
            end
          end
          object DataGrid: TVirtualStringTree
            Left = 0
            Top = 91
            Width = 643
            Height = 183
            Align = alClient
            AutoScrollDelay = 50
            EditDelay = 0
            Header.AutoSizeIndex = -1
            Header.Height = 20
            Header.Images = ImageListMain
            Header.MainColumn = -1
            Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowHint, hoShowImages, hoVisible]
            Header.ParentFont = True
            IncrementalSearch = isInitializedOnly
            LineStyle = lsSolid
            PopupMenu = popupDataGrid
            TabOrder = 2
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight, toEditOnClick]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toAlwaysHideSelection]
            TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
            WantTabs = True
            OnAfterCellPaint = AnyGridAfterCellPaint
            OnBeforeCellPaint = AnyGridBeforeCellPaint
            OnBeforePaint = DataGridBeforePaint
            OnChange = AnyGridChange
            OnColumnResize = DataGridColumnResize
            OnCreateEditor = AnyGridCreateEditor
            OnEditCancelled = AnyGridEditCancelled
            OnEdited = AnyGridEdited
            OnEditing = AnyGridEditing
            OnEnter = ValidateControls
            OnExit = ValidateControls
            OnFocusChanged = AnyGridFocusChanged
            OnFocusChanging = AnyGridFocusChanging
            OnGetText = AnyGridGetText
            OnPaintText = AnyGridPaintText
            OnGetNodeDataSize = AnyGridGetNodeDataSize
            OnHeaderClick = DataGridHeaderClick
            OnInitNode = AnyGridInitNode
            OnKeyDown = AnyGridKeyDown
            OnMouseUp = AnyGridMouseUp
            OnMouseWheel = AnyGridMouseWheel
            OnNewText = AnyGridNewText
            Columns = <>
          end
        end
        object tabQuery: TTabSheet
          Caption = 'Query'
          ImageIndex = 57
          object spltQuery: TSplitter
            Left = 0
            Top = 96
            Width = 643
            Height = 4
            Cursor = crSizeNS
            Align = alTop
            AutoSnap = False
            ResizeStyle = rsUpdate
          end
          object pnlQueryMemo: TPanel
            Left = 0
            Top = 0
            Width = 643
            Height = 96
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            OnCanResize = pnlQueryMemoCanResize
            object spltQueryHelpers: TSplitter
              Left = 448
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
              Width = 448
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
              Gutter.AutoSize = True
              Gutter.DigitCount = 2
              Gutter.Font.Charset = DEFAULT_CHARSET
              Gutter.Font.Color = clWindowText
              Gutter.Font.Height = -11
              Gutter.Font.Name = 'Terminal'
              Gutter.Font.Style = []
              Gutter.LeftOffset = 2
              Gutter.RightOffset = 0
              Gutter.ShowLineNumbers = True
              Highlighter = SynSQLSyn1
              Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDropFiles, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTabIndent]
              RightEdge = 0
              TabWidth = 3
              WantTabs = True
              OnDropFiles = SynMemoQueryDropFiles
              OnReplaceText = SynMemoQueryReplaceText
              OnStatusChange = SynMemoQueryStatusChange
              OnPaintTransient = SynMemoQueryPaintTransient
              FontSmoothing = fsmNone
              RemovedKeystrokes = <>
              AddedKeystrokes = <
                item
                  Command = ecUpperCaseBlock
                  ShortCut = 16469
                end
                item
                  Command = ecLowerCaseBlock
                  ShortCut = 16460
                end>
            end
            object treeQueryHelpers: TVirtualStringTree
              Left = 452
              Top = 0
              Width = 191
              Height = 96
              Align = alRight
              Constraints.MinWidth = 10
              DragMode = dmAutomatic
              DragType = dtVCL
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
              Images = ImageListMain
              IncrementalSearch = isAll
              PopupMenu = popupQueryHelpers
              RootNodeCount = 6
              TabOrder = 1
              TextMargin = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
              TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
              OnBeforeCellPaint = treeQueryHelpersBeforeCellPaint
              OnChecking = treeQueryHelpersChecking
              OnContextPopup = treeQueryHelpersContextPopup
              OnCreateEditor = treeQueryHelpersCreateEditor
              OnDblClick = treeQueryHelpersDblClick
              OnEditing = treeQueryHelpersEditing
              OnFocusChanging = treeQueryHelpersFocusChanging
              OnFreeNode = treeQueryHelpersFreeNode
              OnGetText = treeQueryHelpersGetText
              OnPaintText = treeQueryHelpersPaintText
              OnGetImageIndex = treeQueryHelpersGetImageIndex
              OnInitChildren = treeQueryHelpersInitChildren
              OnInitNode = treeQueryHelpersInitNode
              OnNewText = treeQueryHelpersNewText
              OnNodeClick = treeQueryHelpersNodeClick
              OnResize = treeQueryHelpersResize
              Columns = <
                item
                  Position = 0
                  Width = 70
                  WideText = 'Main column'
                end
                item
                  Position = 1
                  Width = 100
                  WideText = 'Attributes'
                end>
            end
          end
          object QueryGrid: TVirtualStringTree
            Left = 0
            Top = 124
            Width = 643
            Height = 150
            Align = alClient
            AutoScrollDelay = 50
            EditDelay = 0
            Header.AutoSizeIndex = -1
            Header.Height = 20
            Header.Images = ImageListMain
            Header.MainColumn = -1
            Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowHint, hoShowImages, hoShowSortGlyphs]
            Header.ParentFont = True
            IncrementalSearch = isAll
            LineStyle = lsSolid
            PopupMenu = popupDataGrid
            TabOrder = 1
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight, toEditOnClick]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toAlwaysHideSelection]
            TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect, toRightClickSelect]
            Visible = False
            WantTabs = True
            OnAfterCellPaint = AnyGridAfterCellPaint
            OnAfterPaint = AnyGridAfterPaint
            OnBeforeCellPaint = AnyGridBeforeCellPaint
            OnChange = AnyGridChange
            OnCompareNodes = AnyGridCompareNodes
            OnCreateEditor = AnyGridCreateEditor
            OnEditCancelled = AnyGridEditCancelled
            OnEdited = AnyGridEdited
            OnEditing = AnyGridEditing
            OnEndOperation = AnyGridEndOperation
            OnEnter = ValidateControls
            OnExit = ValidateControls
            OnFocusChanged = AnyGridFocusChanged
            OnFocusChanging = AnyGridFocusChanging
            OnGetText = AnyGridGetText
            OnPaintText = AnyGridPaintText
            OnGetNodeDataSize = AnyGridGetNodeDataSize
            OnHeaderClick = AnyGridHeaderClick
            OnInitNode = AnyGridInitNode
            OnKeyDown = AnyGridKeyDown
            OnMouseUp = AnyGridMouseUp
            OnMouseWheel = AnyGridMouseWheel
            OnNewText = AnyGridNewText
            OnStartOperation = AnyGridStartOperation
            Columns = <>
          end
          object tabsetQuery: TTabSet
            Left = 0
            Top = 100
            Width = 643
            Height = 24
            Align = alTop
            DitherBackground = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Images = ImageListMain
            SelectedColor = clWindow
            SoftTop = True
            Style = tsSoftTabs
            TabPosition = tpTop
            UnselectedColor = clBtnFace
            OnClick = tabsetQueryClick
            OnGetImageIndex = tabsetQueryGetImageIndex
            OnMouseLeave = tabsetQueryMouseLeave
            OnMouseMove = tabsetQueryMouseMove
          end
        end
      end
    end
  end
  object ProgressBarStatus: TProgressBar
    Left = 535
    Top = 405
    Width = 81
    Height = 17
    Step = 1
    TabOrder = 3
  end
  object CoolBarMainMenu: TCoolBar
    Left = 0
    Top = 0
    Width = 824
    Height = 54
    AutoSize = True
    Bands = <
      item
        Control = pnlMainMenu
        HorizontalOnly = True
        ImageIndex = -1
        MinHeight = 23
        MinWidth = 430
        Width = 818
      end
      item
        Control = ToolBarMainButtons
        HorizontalOnly = True
        ImageIndex = -1
        Width = 818
      end>
    object pnlMainMenu: TPanel
      Left = 11
      Top = 0
      Width = 809
      Height = 23
      Align = alClient
      BevelOuter = bvNone
      Caption = 'pnlMainMenu'
      ShowCaption = False
      TabOrder = 0
      object ToolBarMainMenu: TToolBar
        Left = 0
        Top = 0
        Width = 222
        Height = 23
        Align = alLeft
        AutoSize = True
        ButtonHeight = 21
        ButtonWidth = 40
        Caption = 'ToolBarMainMenu'
        ShowCaptions = True
        TabOrder = 0
        object btnFile: TToolButton
          Left = 0
          Top = 0
          Hint = 'File related commands'
          AutoSize = True
          Caption = 'File'
          Grouped = True
          MenuItem = MainMenuFile
        end
        object btnEdit: TToolButton
          Left = 27
          Top = 0
          Hint = 'Edit commands'
          AutoSize = True
          Caption = 'Edit'
          Grouped = True
          MenuItem = MainMenuEdit
        end
        object btnSearch: TToolButton
          Left = 56
          Top = 0
          AutoSize = True
          Caption = 'Search'
          Grouped = True
          MenuItem = MainMenuSearch
        end
        object btnTools: TToolButton
          Left = 100
          Top = 0
          AutoSize = True
          Caption = 'Tools'
          Grouped = True
          MenuItem = MainMenuTools
        end
        object ToolButton11: TToolButton
          Left = 146
          Top = 0
          AutoSize = True
          Caption = 'Go to'
          Grouped = True
          MenuItem = MainMenuGoto
        end
        object btnHelp: TToolButton
          Left = 186
          Top = 0
          Hint = 'Help topics'
          AutoSize = True
          Caption = 'Help'
          Grouped = True
          MenuItem = MainMenuHelp
        end
      end
      object ToolBarExtraButtons: TToolBar
        Left = 632
        Top = 0
        Width = 177
        Height = 23
        Align = alRight
        AutoSize = True
        ButtonWidth = 107
        Caption = 'ToolBarExtraButtons'
        Images = ImageListMain
        List = True
        ShowCaptions = True
        TabOrder = 1
        Wrapable = False
        object btnDonate: TToolButton
          Left = 0
          Top = 0
          Hint = 
            'Send an arbitrary amount as donation to the author - per PayPal ' +
            '(also supports credit cards)'
          AutoSize = True
          Caption = 'Donate'
          ImageIndex = 185
          OnClick = DonateClick
        end
        object btnUpdateAvailable: TToolButton
          Left = 66
          Top = 0
          AutoSize = True
          Caption = 'Update available'
          ImageIndex = 94
          Visible = False
          OnClick = actUpdateCheckExecute
        end
      end
    end
    object ToolBarMainButtons: TToolBar
      Left = 11
      Top = 25
      Width = 809
      Height = 25
      Caption = 'ToolBarMainButtons'
      Images = ImageListMain
      TabOrder = 1
      Wrapable = False
      object ToolButton9: TToolButton
        Left = 0
        Top = 0
        Action = actSessionManager
        AutoSize = True
        DropdownMenu = menuConnections
        Style = tbsDropDown
      end
      object btnExit: TToolButton
        Left = 44
        Top = 0
        Action = actDisconnect
      end
      object tlbSep1: TToolButton
        Left = 67
        Top = 0
        Width = 8
        Caption = 'tlbSep1'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ToolButton5: TToolButton
        Left = 75
        Top = 0
        Action = actCopy
        AutoSize = True
      end
      object ToolButton6: TToolButton
        Left = 98
        Top = 0
        Action = actPaste
        AutoSize = True
      end
      object ToolButton14: TToolButton
        Left = 121
        Top = 0
        Hint = 'Undo'
        Action = actUndo
      end
      object ToolButton12: TToolButton
        Left = 144
        Top = 0
        Action = actPrintList
      end
      object tlbSep2: TToolButton
        Left = 167
        Top = 0
        Width = 8
        Caption = 'tlbSep2'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object ButtonRefresh: TToolButton
        Left = 175
        Top = 0
        Action = actRefresh
        AutoSize = True
        DropdownMenu = popupRefresh
        Style = tbsDropDown
      end
      object ButtonUserManager: TToolButton
        Left = 219
        Top = 0
        Action = actUserManager
        AutoSize = True
      end
      object ButtonImportTextfile: TToolButton
        Left = 242
        Top = 0
        Action = actImportCSV
        AutoSize = True
      end
      object ButtonExport: TToolButton
        Left = 265
        Top = 0
        Action = actExportTables
        AutoSize = True
      end
      object tlbSep6: TToolButton
        Left = 288
        Top = 0
        Width = 8
        Caption = 'tlbSep6'
        ImageIndex = 97
        Style = tbsSeparator
      end
      object btnSQLHelp: TToolButton
        Left = 296
        Top = 0
        Action = actSQLhelp
      end
      object ToolButton3: TToolButton
        Left = 319
        Top = 0
        Action = actDataFirst
      end
      object ToolButton4: TToolButton
        Left = 342
        Top = 0
        Action = actDataLast
      end
      object ToolButton7: TToolButton
        Left = 365
        Top = 0
        Action = actDataInsert
      end
      object ToolButton8: TToolButton
        Left = 388
        Top = 0
        Action = actDataDelete
      end
      object ToolButton10: TToolButton
        Left = 411
        Top = 0
        Action = actDataPostChanges
      end
      object ToolButton1: TToolButton
        Left = 434
        Top = 0
        Action = actDataCancelChanges
      end
      object btnExecuteQuery: TToolButton
        Left = 457
        Top = 0
        Action = actExecuteQuery
        DropdownMenu = popupExecuteQuery
        Style = tbsDropDown
      end
      object btnLoadSQL: TToolButton
        Left = 495
        Top = 0
        Action = actLoadSQL
        DropdownMenu = PopupQueryLoad
        Style = tbsDropDown
      end
      object btnSaveSQL: TToolButton
        Left = 533
        Top = 0
        Action = actSaveSQL
      end
      object btnSaveSQLSnippet: TToolButton
        Left = 556
        Top = 0
        Action = actSaveSQLSnippet
      end
      object btnQueryFind: TToolButton
        Left = 579
        Top = 0
        Action = actQueryFind
      end
      object btnQueryReplace: TToolButton
        Left = 602
        Top = 0
        Action = actQueryReplace
      end
      object btnReformatSQL: TToolButton
        Left = 625
        Top = 0
        Action = actReformatSQL
      end
      object btnStopOnErrors: TToolButton
        Left = 648
        Top = 0
        Action = actQueryStopOnErrors
      end
      object btnBlobAsText: TToolButton
        Left = 671
        Top = 0
        Action = actBlobAsText
      end
      object btnQueryWordwrap: TToolButton
        Left = 694
        Top = 0
        Action = actQueryWordWrap
      end
      object btnSetDelimiter: TToolButton
        Left = 717
        Top = 0
        Action = actSetDelimiter
      end
      object btnCancelOperation: TToolButton
        Left = 740
        Top = 0
        Action = actCancelOperation
      end
    end
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 424
    Top = 152
    object MainMenuFile: TMenuItem
      Caption = 'File'
      Hint = 'File related commands'
      OnClick = MainMenuFileClick
      object Sessionmanager1: TMenuItem
        Action = actSessionManager
      end
      object menuConnectTo: TMenuItem
        Caption = 'Connect to'
      end
      object FileNewItem: TMenuItem
        Action = actNewWindow
      end
      object Newquerytab1: TMenuItem
        Action = actNewQueryTab
      end
      object Closetab1: TMenuItem
        Action = actCloseQueryTab
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object LoadSQLfile2: TMenuItem
        Action = actLoadSQL
      end
      object RunSQLfiles1: TMenuItem
        Action = actRunSQL
      end
      object Save1: TMenuItem
        Action = actSaveSQL
      end
      object Saveassnippet1: TMenuItem
        Action = actSaveSQLSnippet
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object actSelectTreeBackground1: TMenuItem
        Action = actSelectTreeBackground
      end
      object ExportSettings1: TMenuItem
        Action = actExportSettings
      end
      object Importsettings1: TMenuItem
        Action = actImportSettings
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Action = actExitApplication
        ShortCut = 32883
      end
    end
    object MainMenuEdit: TMenuItem
      Caption = 'Edit'
      Hint = 'Edit commands'
      object CopyItem: TMenuItem
        Action = actCopy
      end
      object PasteItem: TMenuItem
        Action = actPaste
      end
      object Cut1: TMenuItem
        Action = actCut
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object actSelectAll1: TMenuItem
        Action = actSelectAll
      end
      object Inverseselection1: TMenuItem
        Action = actSelectInverse
      end
      object actFindInVT1: TMenuItem
        Action = actFilterPanel
        AutoCheck = True
      end
      object ReformatSQL2: TMenuItem
        Action = actReformatSQL
      end
    end
    object MainMenuSearch: TMenuItem
      Caption = 'Search'
      object Findtext1: TMenuItem
        Action = actQueryFind
      end
      object actQueryFindAgain1: TMenuItem
        Action = actQueryFindAgain
      end
      object Replacetext1: TMenuItem
        Action = actQueryReplace
      end
      object actFindTextOnServer1: TMenuItem
        Action = actFindTextOnServer
      end
    end
    object MainMenuTools: TMenuItem
      Caption = 'Tools'
      object Previoustab1: TMenuItem
        Action = actPreviousTab
      end
      object Nexttab1: TMenuItem
        Action = actNextTab
      end
      object Previousresulttab1: TMenuItem
        Action = actPreviousResult
      end
      object Nextresulttab1: TMenuItem
        Action = actNextResult
      end
      object Flush1: TMenuItem
        Caption = 'Flush'
        object MenuFlushHosts: TMenuItem
          Action = actFlushHosts
        end
        object MenuFlushLogs: TMenuItem
          Action = actFlushLogs
        end
        object FlushUserPrivileges1: TMenuItem
          Action = actFlushPrivileges
        end
        object MenuFlushTables: TMenuItem
          Action = actFlushTables
        end
        object MenuFlushTableswithreadlock: TMenuItem
          Action = actFlushTableswithreadlock
        end
        object MenuFlushStatus: TMenuItem
          Action = actFlushStatus
        end
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MenuUserManager: TMenuItem
        Action = actUserManager
      end
      object menuMaintenance: TMenuItem
        Action = actMaintenance
      end
      object Bulktableeditor1: TMenuItem
        Action = actBulkTableEdit
      end
      object Launchcommandline1: TMenuItem
        Action = actLaunchCommandline
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object ExportdatabaseasSQL1: TMenuItem
        Action = actExportTables
      end
      object Exportgridrows1: TMenuItem
        Action = actExportData
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object ImportCSVfile1: TMenuItem
        Action = actImportCSV
      end
      object InsertfilesintoTEXTBLOBfields1: TMenuItem
        Action = actInsertFiles
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuPreferences: TMenuItem
        Action = actPreferences
      end
    end
    object MainMenuGoto: TMenuItem
      Caption = 'Go to'
      object actGotoFilter1: TMenuItem
        Action = actGotoFilter
      end
      object actGotoDbTree1: TMenuItem
        Action = actGotoDbTree
      end
      object actGotoTab11: TMenuItem
        Action = actGotoTab1
      end
      object actGotoTab12: TMenuItem
        Action = actGotoTab2
      end
      object actGotoTab31: TMenuItem
        Action = actGotoTab3
      end
      object actGotoTab41: TMenuItem
        Action = actGotoTab4
      end
      object actGotoTab51: TMenuItem
        Action = actGotoTab5
      end
    end
    object MainMenuHelp: TMenuItem
      Caption = 'Help'
      Hint = 'Help topics'
      object menuSQLHelp1: TMenuItem
        Action = actSQLhelp
      end
      object menuReadme: TMenuItem
        Action = actHelp
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object menuUpdateCheck: TMenuItem
        Action = actUpdateCheck
      end
      object menuDownload: TMenuItem
        Action = actWebDownloadpage
      end
      object menuSupportForum: TMenuItem
        Action = actWebForum
      end
      object menuFeaturetracker: TMenuItem
        Action = actWebChangelog
      end
      object menuAbout: TMenuItem
        Action = actAboutBox
      end
    end
  end
  object ActionList1: TActionList
    Images = ImageListMain
    Left = 424
    Top = 104
    object actSessionManager: TAction
      Category = 'File'
      Caption = 'Session manager'
      Hint = 'Display session manager'
      ImageIndex = 37
      OnExecute = actSessionManagerExecute
    end
    object actNewWindow: TAction
      Category = 'File'
      Caption = 'New &window'
      Hint = 'New window...'
      ImageIndex = 37
      ShortCut = 16462
      OnExecute = actNewWindowExecute
    end
    object actExitApplication: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Exit application'
      ImageIndex = 26
      OnExecute = actExitApplicationExecute
    end
    object actCopy: TAction
      Category = 'Various'
      Caption = '&Copy'
      Hint = 'Copy|Copy to Clipboard'
      ImageIndex = 3
      ShortCut = 16451
      OnExecute = actCopyOrCutExecute
    end
    object actPaste: TAction
      Category = 'Various'
      Caption = '&Paste'
      Hint = 'Paste|Paste from Clipboard'
      ImageIndex = 4
      ShortCut = 16470
      OnExecute = actPasteExecute
    end
    object actUserManager: TAction
      Category = 'Tools'
      Caption = 'User manager'
      Hint = 'Manage user authentication and privileges'
      ImageIndex = 11
      OnExecute = actUserManagerExecute
    end
    object actCut: TAction
      Category = 'Various'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 2
      ShortCut = 16472
      OnExecute = actCopyOrCutExecute
    end
    object actUndo: TEditUndo
      Category = 'Various'
      Caption = '&Undo'
      Enabled = False
      ImageIndex = 40
      ShortCut = 32776
    end
    object actAboutBox: TAction
      Category = 'Various'
      Caption = 'About...'
      Hint = 'About this application'
      ImageIndex = 99
      OnExecute = actAboutBoxExecute
    end
    object actMaintenance: TAction
      Category = 'Tools'
      Caption = 'Maintenance'
      Hint = 'Optimize, repair and analyse tables'
      ImageIndex = 39
      OnExecute = actTableToolsExecute
    end
    object actFindTextOnServer: TAction
      Category = 'Tools'
      Caption = 'Find text on server'
      Hint = 'Searches selected tables for text occurences'
      ImageIndex = 146
      ShortCut = 24646
      OnExecute = actTableToolsExecute
    end
    object actExportData: TAction
      Category = 'Export/Import'
      Caption = 'Export grid rows'
      Enabled = False
      Hint = 'Export rows to file or copy to clipboard, in various formats'
      ImageIndex = 20
      OnExecute = actExportDataExecute
    end
    object actPrintList: TAction
      Category = 'Various'
      Caption = 'Print...'
      Hint = 'Print List or Data'
      ImageIndex = 34
      ShortCut = 16464
      OnExecute = actPrintListExecute
    end
    object actCopyTable: TAction
      Category = 'Database'
      Caption = 'Table copy'
      Enabled = False
      Hint = 'Create a base table copy of this table or view'
      ImageIndex = 19
      OnExecute = actCopyTableExecute
    end
    object actExecuteQuery: TAction
      Category = 'SQL'
      Caption = 'Run'
      Enabled = False
      Hint = 'Execute SQL...|Execute SQL-query/queries...'
      ImageIndex = 57
      ShortCut = 120
      OnExecute = actExecuteQueryExecute
    end
    object actExecuteSelection: TAction
      Category = 'SQL'
      Caption = 'Run Selection'
      Enabled = False
      Hint = 'Execute selected SQL...|Execute selected SQL-query/queries...'
      ImageIndex = 104
      ShortCut = 16504
      OnExecute = actExecuteQueryExecute
    end
    object actExecuteCurrentQuery: TAction
      Category = 'SQL'
      Caption = 'Run current query'
      Enabled = False
      Hint = 'Run current query|Run currently focused SQL query'
      ImageIndex = 105
      ShortCut = 24696
      OnExecute = actExecuteQueryExecute
    end
    object actExplainCurrentQuery: TAction
      Category = 'SQL'
      Caption = 'Explain current query'
      Hint = 'Run EXPLAIN <current query> and show results'
      OnExecute = actExecuteQueryExecute
    end
    object actExplainAnalyzeCurrentQuery: TAction
      Category = 'SQL'
      Caption = 'Explain analyzer for current query'
      Hint = 'Run EXPLAIN <current query> and send results to MariaDB.org'
      OnExecute = actExplainAnalyzeCurrentQueryExecute
    end
    object actDataPreview: TAction
      Category = 'Data'
      Caption = 'Image preview'
      Hint = 'Preview image contents from BLOB cells'
      ImageIndex = 152
      OnExecute = actDataPreviewExecute
      OnUpdate = actDataPreviewUpdate
    end
    object actInsertFiles: TAction
      Category = 'Export/Import'
      Caption = 'Insert files into TEXT/BLOB fields...'
      ImageIndex = 47
      OnExecute = actInsertFilesExecute
    end
    object actExportTables: TAction
      Category = 'Export/Import'
      Caption = 'Export database as SQL'
      Hint = 'Dump database objects to an SQL file'
      ImageIndex = 9
      OnExecute = actTableToolsExecute
    end
    object actLoadSQL: TAction
      Category = 'SQL'
      Caption = 'Load SQL file...'
      Hint = 'Load SQL file...'
      ImageIndex = 51
      ShortCut = 16463
      OnExecute = actLoadSQLExecute
    end
    object actRunSQL: TAction
      Category = 'SQL'
      Caption = 'Run SQL file...'
      Hint = 'Run SQL file(s) directly, without loading into the editor'
      ImageIndex = 189
      OnExecute = actLoadSQLExecute
    end
    object actDropObjects: TAction
      Category = 'Database'
      Caption = 'Drop ...'
      Enabled = False
      Hint = 'Deletes tables, views, procedures and functions'
      ImageIndex = 131
      OnExecute = actDropObjectsExecute
    end
    object actCreateView: TAction
      Category = 'Database'
      Caption = 'View'
      Enabled = False
      Hint = 'Create view ...'
      ImageIndex = 81
      OnExecute = actCreateDBObjectExecute
    end
    object actDataFirst: TAction
      Category = 'Data'
      Caption = '&First'
      Enabled = False
      Hint = 'First'
      ImageIndex = 89
      OnExecute = actDataFirstExecute
    end
    object actDataLast: TAction
      Category = 'Data'
      Caption = '&Last'
      Enabled = False
      Hint = 'Last'
      ImageIndex = 90
      OnExecute = actDataLastExecute
    end
    object actDataInsert: TAction
      Category = 'Data'
      Caption = '&Insert row'
      Enabled = False
      Hint = 'Insert row into table'
      ImageIndex = 45
      ShortCut = 45
      OnExecute = actDataInsertExecute
    end
    object actDataDuplicateRow: TAction
      Category = 'Data'
      Caption = 'Duplicate row'
      Enabled = False
      ImageIndex = 45
      ShortCut = 16429
      OnExecute = actDataInsertExecute
    end
    object actDataDelete: TAction
      Category = 'Data'
      Caption = '&Delete selected row(s)'
      Enabled = False
      Hint = 'Delete selected row(s)'
      ImageIndex = 46
      ShortCut = 16430
      OnExecute = actDataDeleteExecute
    end
    object actDataPostChanges: TAction
      Category = 'Data'
      Caption = 'P&ost'
      Enabled = False
      Hint = 'Post'
      ImageIndex = 55
      ShortCut = 16397
      OnExecute = actDataPostChangesExecute
    end
    object actDataCancelChanges: TAction
      Category = 'Data'
      Caption = 'Cancel editing'
      Enabled = False
      Hint = 'Cancel editing'
      ImageIndex = 26
      ShortCut = 27
      OnExecute = actDataCancelChangesExecute
    end
    object actCreateTable: TAction
      Category = 'Database'
      Caption = 'Table'
      Enabled = False
      Hint = 'Create new table in selected database'
      ImageIndex = 14
      OnExecute = actCreateDBObjectExecute
    end
    object actEmptyTables: TAction
      Category = 'Database'
      Caption = 'Empty table(s) ...'
      Enabled = False
      Hint = 'Delete all rows in selected table(s)'
      ImageIndex = 46
      ShortCut = 8238
      OnExecute = actEmptyTablesExecute
    end
    object actCreateDatabase: TAction
      Category = 'Database'
      Caption = 'Database'
      Hint = 'Create a new, blank database'
      ImageIndex = 5
      OnExecute = actCreateDatabaseExecute
    end
    object actSQLhelp: TAction
      Category = 'Tools'
      Caption = 'SQL help'
      Enabled = False
      Hint = 'SQL help browser'
      ImageIndex = 31
      ShortCut = 112
      OnExecute = actSQLhelpExecute
    end
    object actRefresh: TAction
      Category = 'Various'
      Caption = 'Refresh'
      Hint = 'Refresh'
      ImageIndex = 0
      ShortCut = 116
      OnExecute = actRefreshExecute
    end
    object actFullRefresh: TAction
      Category = 'Various'
      Caption = 'Full status refresh'
      Enabled = False
      Hint = 
        'Get full statistics refresh on table data. Slow on InnoDB tables' +
        '!'
      ImageIndex = 184
      ShortCut = 8308
      OnExecute = actFullRefreshExecute
    end
    object actImportCSV: TAction
      Category = 'Export/Import'
      Caption = 'Import CSV file...'
      Hint = 'Import a CSV or tab delimited file'
      ImageIndex = 50
      OnExecute = actImportCSVExecute
    end
    object actExportSettings: TAction
      Category = 'Export/Import'
      Caption = 'Export settings file ...'
      ImageIndex = 100
      OnExecute = actExportSettingsExecute
    end
    object actImportSettings: TAction
      Category = 'Export/Import'
      Caption = 'Import settings file ...'
      ImageIndex = 101
      OnExecute = actImportSettingsExecute
    end
    object actPreferences: TAction
      Category = 'Tools'
      Caption = 'Preferences'
      ImageIndex = 98
      OnExecute = actPreferencesExecute
    end
    object actPreferencesLogging: TAction
      Category = 'Tools'
      Caption = 'Logging preferences'
    end
    object actPreferencesData: TAction
      Category = 'Tools'
      Caption = 'Data grid preferences'
    end
    object actFlushHosts: TAction
      Category = 'Tools'
      Caption = 'Hosts'
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushLogs: TAction
      Category = 'Tools'
      Caption = 'Logs'
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushPrivileges: TAction
      Category = 'Tools'
      Caption = 'Privileges'
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushTables: TAction
      Category = 'Tools'
      Caption = 'Tables'
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushTableswithreadlock: TAction
      Category = 'Tools'
      Caption = 'Tables with read lock'
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushStatus: TAction
      Category = 'Tools'
      Caption = 'Status'
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actUpdateCheck: TAction
      Category = 'Tools'
      Caption = 'Check for updates ...'
      ImageIndex = 94
      OnExecute = actUpdateCheckExecute
    end
    object actWebDownloadpage: TAction
      Category = 'Various'
      Caption = 'Download page'
      Hint = 'http://www.heidisql.com/download.php'
      ImageIndex = 69
      OnExecute = actWebbrowse
    end
    object actWebForum: TAction
      Category = 'Various'
      Caption = 'Support forum'
      Hint = 'http://www.heidisql.com/forum.php'
      ImageIndex = 95
      OnExecute = actWebbrowse
    end
    object actWebChangelog: TAction
      Category = 'Various'
      Caption = 'Changelog'
      Hint = 'https://sourceforge.net/p/heidisql/code/HEAD/log/?path=/'
      ImageIndex = 68
      OnExecute = actWebbrowse
    end
    object actHelp: TAction
      Category = 'Various'
      Caption = 'General help'
      Hint = 'General online help document'
      ImageIndex = 99
      OnExecute = actHelpExecute
    end
    object actSaveSQLAs: TAction
      Category = 'SQL'
      Caption = 'Save as ...'
      Enabled = False
      Hint = 'Save SQL to a textfile'
      ImageIndex = 10
      ShortCut = 123
      OnExecute = actSaveSQLAsExecute
    end
    object actSaveSQLselection: TAction
      Category = 'SQL'
      Caption = 'Save selection to file ...'
      Enabled = False
      Hint = 'Save selected text to a file'
      ImageIndex = 10
      ShortCut = 24659
      OnExecute = actSaveSQLAsExecute
    end
    object actSaveSQLSnippet: TAction
      Category = 'SQL'
      Caption = 'Save as snippet ...'
      Enabled = False
      Hint = 'Save as snippet ...'
      ImageIndex = 54
      OnExecute = actSaveSQLAsExecute
    end
    object actSaveSQLSelectionSnippet: TAction
      Category = 'SQL'
      Caption = 'Save selection as snippet ...'
      Enabled = False
      Hint = 'Save selected text as snippet ...'
      ImageIndex = 54
      OnExecute = actSaveSQLAsExecute
    end
    object actClearQueryEditor: TAction
      Category = 'SQL'
      Caption = 'Clear'
      Enabled = False
      Hint = 'Clear query editor'
      ImageIndex = 58
      ShortCut = 16471
      OnExecute = actClearEditorExecute
    end
    object actClearFilterEditor: TAction
      Category = 'Data'
      Caption = 'Clear'
      Hint = 'Clear filter editor'
      ImageIndex = 58
      ShortCut = 16471
      OnExecute = actClearEditorExecute
    end
    object actClearQueryLog: TAction
      Category = 'SQL'
      Caption = 'Clear'
      Hint = 'Clear query log'
      ImageIndex = 58
      ShortCut = 16465
      OnExecute = actClearEditorExecute
    end
    object actQueryStopOnErrors: TAction
      Category = 'SQL'
      AutoCheck = True
      Caption = 'Stop on errors in batch mode'
      Checked = True
      Hint = 'Stop on errors in batch mode'
      ImageIndex = 63
      OnExecute = actQueryStopOnErrorsExecute
    end
    object actQueryWordWrap: TAction
      Category = 'SQL'
      AutoCheck = True
      Caption = 'Wrap long lines'
      Hint = 'Wrap long lines'
      ImageIndex = 62
      OnExecute = actQueryWordWrapExecute
    end
    object actQueryFind: TAction
      Category = 'SQL'
      Caption = 'Find text ...'
      Hint = 'Find text ...'
      ImageIndex = 30
      ShortCut = 16454
      OnExecute = actQueryFindReplaceExecute
    end
    object actQueryReplace: TAction
      Category = 'SQL'
      Caption = 'Replace text ...'
      Hint = 'Replace text ...'
      ImageIndex = 59
      ShortCut = 16466
      OnExecute = actQueryFindReplaceExecute
    end
    object actQueryFindAgain: TAction
      Category = 'SQL'
      Caption = 'Find or replace again'
      ImageIndex = 142
      ShortCut = 114
      OnExecute = actQueryFindAgainExecute
    end
    object actSetDelimiter: TAction
      Category = 'SQL'
      Caption = 'Set delimiter used in SQL execution'
      Enabled = False
      Hint = 'Set delimiter used in SQL execution'
      ImageIndex = 106
      OnExecute = actSetDelimiterExecute
    end
    object actApplyFilter: TAction
      Category = 'Data'
      Caption = 'Apply filter'
      ImageIndex = 55
      ShortCut = 120
      OnExecute = actApplyFilterExecute
    end
    object actRemoveFilter: TAction
      Category = 'Data'
      Caption = 'Remove filter'
      ImageIndex = 26
      OnExecute = actRemoveFilterExecute
    end
    object actSelectTreeBackground: TAction
      Category = 'File'
      Caption = 'Select session background color ...'
      Hint = 'Lets you chose a per session color value for the database tree'
      ImageIndex = 115
      OnExecute = actSelectTreeBackgroundExecute
    end
    object actPreviousTab: TPreviousTab
      Category = 'Tools'
      TabControl = PageControlMain
      Caption = '&Previous tab'
      Hint = 'Previous tab|Go back to the previous tab'
      ImageIndex = 117
      ShortCut = 24585
      Wrap = True
    end
    object actNextTab: TNextTab
      Category = 'Tools'
      TabControl = PageControlMain
      Caption = '&Next tab'
      Hint = 'Next tab|Go to the next tab'
      ImageIndex = 116
      ShortCut = 16393
      Wrap = True
    end
    object actSelectAll: TAction
      Category = 'Various'
      Caption = 'Select all'
      Hint = 'Select all|Select all items or text'
      ImageIndex = 118
      ShortCut = 16449
      OnExecute = actSelectAllExecute
      OnUpdate = ValidateControls
    end
    object actCreateRoutine: TAction
      Category = 'Database'
      Caption = 'Stored routine'
      Hint = 'Create stored routine|Create stored procedure or function'
      ImageIndex = 119
      OnExecute = actCreateDBObjectExecute
    end
    object actNewQueryTab: TAction
      Category = 'File'
      Caption = 'New query tab'
      Hint = 'Open a blank query tab'
      ImageIndex = 132
      ShortCut = 16468
      OnExecute = actNewQueryTabExecute
    end
    object actCloseQueryTab: TAction
      Category = 'File'
      Caption = 'Close query tab'
      Enabled = False
      ImageIndex = 133
      ShortCut = 16499
      OnExecute = actCloseQueryTabExecute
    end
    object actSelectInverse: TAction
      Category = 'Various'
      Caption = 'Invert selection'
      ImageIndex = 138
      ShortCut = 16457
      OnExecute = actSelectInverseExecute
    end
    object actFilterPanel: TAction
      Category = 'Various'
      AutoCheck = True
      Caption = 'Filter panel'
      Hint = 'Activates the filter panel'
      ImageIndex = 30
      ShortCut = 49222
      OnExecute = actFilterPanelExecute
    end
    object actBulkTableEdit: TAction
      Category = 'Tools'
      Caption = 'Bulk table editor'
      ImageIndex = 19
      OnExecute = actTableToolsExecute
    end
    object actCreateTrigger: TAction
      Category = 'Database'
      Caption = 'Trigger'
      Hint = 'Create a trigger'
      ImageIndex = 137
      OnExecute = actCreateDBObjectExecute
    end
    object actSaveSQL: TAction
      Category = 'SQL'
      Caption = 'Save'
      Enabled = False
      Hint = 'Save SQL to file'
      ImageIndex = 10
      ShortCut = 16467
      OnExecute = actSaveSQLExecute
    end
    object actDataResetSorting: TAction
      Category = 'Data'
      Caption = 'Reset sorting'
      ImageIndex = 139
      ShortCut = 32851
      OnExecute = actDataResetSortingExecute
    end
    object actReformatSQL: TAction
      Category = 'SQL'
      Caption = 'Reformat SQL'
      Hint = 
        'Automatically reformat disordered SQL in active editor to make i' +
        't more readable'
      ImageIndex = 140
      ShortCut = 16503
      OnExecute = actReformatSQLExecute
    end
    object actBlobAsText: TAction
      Category = 'Data'
      AutoCheck = True
      Caption = 'View binary data as text (instead of HEX)'
      Hint = 'View binary data as text (instead of HEX)'
      ImageIndex = 141
      OnExecute = actBlobAsTextExecute
    end
    object actDataShowNext: TAction
      Category = 'Data'
      Caption = 'Next'
      Hint = 'Next X rows'
      ImageIndex = 79
      ShortCut = 49186
      OnExecute = actDataShowNextExecute
    end
    object actDataShowAll: TAction
      Category = 'Data'
      Caption = 'Show all'
      Hint = 'Show all rows'
      ImageIndex = 143
      ShortCut = 49187
      OnExecute = actDataShowAllExecute
    end
    object actRunRoutines: TAction
      Category = 'Database'
      Caption = 'Run routine(s) ...'
      ImageIndex = 35
      OnExecute = actRunRoutinesExecute
    end
    object actCreateEvent: TAction
      Category = 'Database'
      Caption = 'Event'
      Enabled = False
      Hint = 'Create new event in selected database'
      ImageIndex = 80
      OnExecute = actCreateDBObjectExecute
    end
    object actDataSetNull: TAction
      Category = 'Data'
      Caption = 'NULL'
      Enabled = False
      Hint = 'Set focused cell to NULL'
      ImageIndex = 92
      ShortCut = 24654
      OnExecute = actDataSetNullExecute
    end
    object actDataSaveBlobToFile: TAction
      Category = 'Data'
      Caption = 'Save BLOB to file ...'
      Hint = 'Save contents to local file ...'
      ImageIndex = 10
      OnExecute = actDataSaveBlobToFileExecute
    end
    object actDisconnect: TAction
      Category = 'File'
      Caption = 'Disconnect'
      Hint = 'Close selected database connection'
      ImageIndex = 29
      OnExecute = actDisconnectExecute
    end
    object actBatchInOneGo: TAction
      Category = 'SQL'
      AutoCheck = True
      Caption = 'Send batch in one go'
      GroupIndex = 1
      Hint = 'Send up to max_allowed_packet batch at once'
      OnExecute = actBatchInOneGoExecute
    end
    object actSingleQueries: TAction
      Category = 'SQL'
      AutoCheck = True
      Caption = 'Send queries one by one'
      Checked = True
      GroupIndex = 1
      OnExecute = actBatchInOneGoExecute
    end
    object actCancelOperation: TAction
      Category = 'Various'
      Caption = 'Cancel running operation'
      Enabled = False
      Hint = 'Cancel running operation'
      ImageIndex = 159
      ShortCut = 27
      OnExecute = actCancelOperationExecute
    end
    object actToggleComment: TAction
      Category = 'SQL'
      Caption = 'Un/comment'
      Hint = 'Makes selected SQL a comment or removes comment chars'
      ImageIndex = 165
      OnExecute = actToggleCommentExecute
    end
    object actSynchronizeDatabase: TAction
      Category = 'Export/Import'
      Caption = 'Synchronize database'
      ImageIndex = 27
      OnExecute = actSynchronizeDatabaseExecute
    end
    object actLaunchCommandline: TAction
      Category = 'Tools'
      Caption = 'Launch command line'
      ImageIndex = 170
      OnExecute = actLaunchCommandlineExecute
    end
    object actGridEditFunction: TAction
      Category = 'Data'
      Caption = 'SQL function'
      Hint = 'Insert SQL function call in this grid cell, e.g. NOW()'
      ImageIndex = 13
      ShortCut = 16497
      OnExecute = actGridEditFunctionExecute
    end
    object actLogHorizontalScrollbar: TAction
      Category = 'Various'
      AutoCheck = True
      Caption = 'Horizontal scrollbar'
      OnExecute = actLogHorizontalScrollbarExecute
    end
    object actGroupObjects: TAction
      Category = 'Various'
      AutoCheck = True
      Caption = 'Group objects by type'
      OnExecute = actGroupObjectsExecute
    end
    object actUnixTimestampColumn: TAction
      Category = 'Data'
      AutoCheck = True
      Caption = 'This is a UNIX timestamp column'
      Enabled = False
      OnExecute = actUnixTimestampColumnExecute
    end
    object actFavoriteObjectsOnly: TAction
      Category = 'Various'
      AutoCheck = True
      Caption = 'Show only favorites'
      Hint = 'Show only favorite tree items'
      ImageIndex = 112
      OnExecute = actFavoriteObjectsOnlyExecute
    end
    object actPreviousResult: TAction
      Category = 'Data'
      Caption = 'Previous result tab'
      ImageIndex = 117
      ShortCut = 32805
      OnExecute = actPreviousResultExecute
    end
    object actNextResult: TAction
      Category = 'Data'
      Caption = 'Next result tab'
      ImageIndex = 116
      ShortCut = 32807
      OnExecute = actNextResultExecute
    end
    object actSaveSynMemoToTextfile: TAction
      Category = 'Various'
      Caption = 'Save as textfile...'
      Hint = 'Save contents to a textfile'
      ImageIndex = 10
      ShortCut = 16467
      OnExecute = actSaveSynMemoToTextfileExecute
      OnUpdate = ValidateControls
    end
    object actGotoDbTree: TAction
      Category = 'Various'
      Caption = 'Database tree'
      ShortCut = 16452
      OnExecute = actGotoDbTreeExecute
    end
    object actGotoFilter: TAction
      Category = 'Various'
      Caption = 'Table filter'
      ShortCut = 16453
      OnExecute = actGotoFilterExecute
    end
    object actGotoTab1: TAction
      Category = 'Various'
      Caption = 'Tab 1'
      ShortCut = 16433
      OnExecute = actGotoTabNumberExecute
    end
    object actGotoTab2: TAction
      Category = 'Various'
      Caption = 'Tab 2'
      ShortCut = 16434
      OnExecute = actGotoTabNumberExecute
    end
    object actGotoTab3: TAction
      Category = 'Various'
      Caption = 'Tab 3'
      ShortCut = 16435
      OnExecute = actGotoTabNumberExecute
    end
    object actGotoTab4: TAction
      Category = 'Various'
      Caption = 'Tab 4'
      ShortCut = 16436
      OnExecute = actGotoTabNumberExecute
    end
    object actGotoTab5: TAction
      Category = 'Various'
      Caption = 'Tab 5'
      ShortCut = 16437
      OnExecute = actGotoTabNumberExecute
    end
    object actCopyRows: TAction
      Category = 'Various'
      Caption = 'Copy selected rows'
      Hint = 'Copy selected rows in custom format'
      ImageIndex = 155
      ShortCut = 24643
      OnExecute = actCopyOrCutExecute
    end
  end
  object menuConnections: TPopupMenu
    AutoHotkeys = maManual
    Images = ImageListMain
    OnPopup = menuConnectionsPopup
    Left = 344
    Top = 200
  end
  object ImageListMain: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 504
    Top = 104
    Bitmap = {
      494C0101BF005001900410001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000000003000001002000000000000000
      0300000000000000000000000000000000000000000000000000000000000000
      00000000000000000004000000080000000C0000000C00000008000000030000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000681AEB0279
      1CFF00000004000000000000000000000000000000000000000C737373D89999
      99F90404042B0000000000000000000000000000000000000000000000000202
      02208F8F8FF0828282E501010118000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0004000000110202001D04040128211C0C69614F25B27D602BCF66451AC0311E
      0A8904030129000000040000000B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000006E23EB41A0
      5DFF004E13CC0000000600000000000000000000000001010115A3A3A3F3DBDB
      DBFF999999ED1F160B81453118C0684A25ED684A25ED453118C01E160B80A0A0
      9FF2D9D9D9FFADADADFB03030323000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000200000000000000060101
      00190504012B120F04503E361591C7A12DFFC69E28FFC29722FFBB881AFFAF6D
      13FF6E481BCA0000003100000057000000000000000000000000000000000000
      000000000000000000001F944EFD198E46FD138C40FD0E8839FD389E5CFF7EC0
      95FF44A260FF005314D1000000070000000000000000000000000C0C0C41D3D3
      D3FFBDBAB8FC9B6939FFA56A33FFAD6D35FFAD6D35FFA56A33FFA07448FFC2C1
      C0FEC3C3C2FE0B0B0B3E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000291C077F51310FB3302411843329
      1287624F1CB8BFA03BF9554C23A6D6BA52FFD4B849FFD0B03DFFCAA52FFFC093
      20FFAF7C28FA0504012D0000001000000000000000000819297012395CA62470
      B5EA297FCAF7297FCAF7279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B5
      84FF81C196FF46A464FF036C2BEE00000009000000000000000016100867967D
      5FF5AE7642FFBA8B5CFFC1926FFFC39072FFC28E6FFFBE8F6AFFB7895BFFB385
      57FF94795AF51610086700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000D0A0346A96211FFBC891BFFC9A3
      2DFFD3B443FFDCC566FF7C723BC4C8B775F3E4D38CFFDFCA73FFD6BA4DFFCBA8
      33FFC19423FF26200D720000000F00000000000000002872B0E6C3D4DDF0A4D9
      F0FD9DDBF4FF95DAF3FF2F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF007722F90000000006040234674A27D5B27B
      44FFC1986FFFD8A988FFEFDFCFFFFAF6F3FFFAF5F3FFEFDFD1FFD7A684FFBD93
      6AFFB27B44FF674A27D506040234000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000FA6771FF3C49A24FFD0B0
      3CFFDDC669FFE7D89AFFBFB381EC9B905AD9F0E6BEFFEBDEAAFFE2CF80FFD4B8
      49FFC8A12BFF61501EB40202001B00000003000000003089CCF7EFFAFEFFA0E9
      F9FF90E5F8FF80E1F7FF35A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB
      8FFF89C7A0FF44A466FF078636FF0000000100000000281D1080AF7E48FFC59A
      6DFFD9A987FFFFFEEBFFFFFAEEFFFFFDF2FFFFFEF2FFFFFCEFFFFFFDEBFFD9A2
      78FFC1986AFFAF7E48FF281D1080000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000A69531BBCC9A32DFFD6BB
      4EFFE4D38CFFEEE4B9FFEDE4C2FE887A3AD2EFE8C7FFB6A970EA827439CD705D
      25C27C6322CD91772FDD0403002700000007000000003291CEF8F2FAFDFFB2ED
      FAFFA3E9F9FF94E6F8FF3BA46DFF36A36DFF32A167FF2E9D61FF53AE7AFF90CB
      A9FF4DAA72FF189046FF308DCBF70000000000000000614627C0BE8E56FFCDA2
      80FFF0DBC0FFFFF7E6FFFAF2E3FFC4BFB5FFF1EDDEFFFFFCE8FFFFF5DFFFEFD4
      AEFFC89870FFBE8E56FF614627C0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000E322C1281CBA732FFD9C0
      5BFFE8D99CFFF1E9C6FFF5EFD6FF968944DC8A782CD8A18F57E1CDBD88F5CDB6
      72F8A6873AE8665221B90C0A03400000000B000000003298D0F9F6FCFEFFC8F2
      FCFFB8EFFBFFABECFAFF9BE8F9FF8AE3F7FF7AE0F6FF6ADCF6FF32A36CFF58B2
      80FF269755FFD0F3FCFF3094CCF700000000000000009D7342EDC99B64FFCF9E
      7EFFFAF3E2FFFDF6E9FFF2F0E3FF8C8B83FF807D75FFD2CFC1FFFFFBEBFFFAEC
      CCFFC78F6BFFC99B64FF9D7342ED000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000E0908023A524015AA9680
      39DBC5B36CF3BEB478ED948C4CD87D6E23D1BDB07CEEF4EED2FFEEE4B9FFE3D1
      86FFD4B747FFC7A02AFF3A2B13900000000B00000000339FD1FAFEFFFFFFF8FD
      FFFFF6FDFFFFF5FCFFFFF3FCFEFFD8F6FCFF93E6F8FF84E3F7FF3AA774FF2F9E
      63FF5AD8F4FFD7F4FCFF3099CCF70000000000000000A67C47EDCEA56FFFD1A4
      88FFFAF6E9FFFDF8EEFFFFFFF9FF807E77FFB0ADA3FF807D75FFC9C5B9FFFAF0
      D3FFC99570FFCEA56FFFA67C47ED000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000A040401298F7530DA8B76
      34D675652BC6867439D3BFA97DF2E1D8B7FA938A4DD7F2EAC9FFEBDFAAFFDFCB
      75FFD1B23FFFC49B25FF785820CE000000080000000031A4D1FAE8F6FBFF93D4
      EFFF87CEEEFF71C0E9FFC9E9F6FFF2FCFEFFF3FCFEFFF2FCFEFFF0FCFEFFEFFB
      FEFFEEFBFEFFFEFFFFFF31A0CCF70000000000000000745533C0D3AB73FFD9B6
      97FFF1E3D6FFFEFBF5FFF5F5F3FF9E9D94FFF0F0E9FFF8F8F2FFBDB8ACFFF0DA
      C1FFD2A986FFD3AB73FF745533C0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000050202001F544417A6CFAF
      3BFFDFC971FFEBDEAAFFF2E9C7FFF3ECCEFF796F35C5EADEADFFE4D38AFFD8BE
      55FFCCA934FFBF901EFFAC7224FB0101001A000000002F98C4F2F1FAFDFF93DE
      F5FF92DCF4FF80D5F2FF68CAEDFF6ACBEAFF84D3EFFF7ED2EFFF78D0EFFF74CF
      EEFF70CFEEFFE9F7FBFF2E9DC6F30000000000000000342E228BD3AC75FFDFC3
      99FFDEBCA9FFFFFFFFFFF0F0EEFFE7E5DAFFFFFFF7FFFFFFF7FFFFFEF3FFDCB3
      97FFDBBF95FFC1A97CFF352A1C85000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000001010000131E190964C9A3
      2EFFD4B849FFE0CC79FFE8D99DFFEBDEA9FF857C48C9C0AD66F0D9C059FFCFAF
      3AFFC59C26FFB67C17FFA55911FF1913085E000000002D9AC1F0F7FCFEFF8DE4
      F8FF90DEF5FF9EE0F5FFABE1F6FFEFFBFEFFF4FDFEFFF3FCFEFFF1FCFEFFEFFB
      FEFFEEFBFEFFEEF1F3F9247896D40000000000080C3D076F9CD9298FB4F5DEBE
      89FFE2C9A6FFDFBEADFFF1E3DCFFF3F1ECFFF4F1EBFFF1E2D9FFDCB9A2FFDFC2
      9DFF8BB0A8FF339BC5F808638CCE000305270000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000503030024B38C
      2BF4C9A42EFFD2B342FFD9BF58FFDBC463FFAF9B4DE674652BC1A98F33E8493E
      159C241D09702219077142270BA23B2B13920000000032A8CEF8FDFEFEFFFEFF
      FFFFFEFEFFFFFDFEFFFFFEFFFFFFEAF7FBFF65BDD9F963BCD7F863BCD7F863BC
      D7F86FC0D8F7589DB2E105131856000000000026367D69B4D1ED54AACBEF369D
      BFFBE2C791FFE6D1A8FFE0C1A5FFD5AD95FFD5AA91FFDEBC9FFFE4CFA5FFBEC0
      9DFF44AED3FDA6DDF0FC1C7FA7D9000305270000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000085F46
      1AB7BB871AFFC49A25FFC9A42EFFCBA732FFC1A03AFA1E1B09660C0B03410504
      012B0101001800000006000000000000000200000000227790D058B9D8FA5ABA
      D9FA5ABAD9FA5ABAD9FA59BAD9FA2C88A4DD000101160000000F0000000F0000
      000F0000000F0000000F00000004000000000026357B4AA2C2E5B4E8F9FF54AF
      CEEF3399B9F3ECC387FFE9CB93FFE7D09CFFE7D09CFFE9CB93FFDEC18DFF2C9B
      C0F575C3DEF55FAFCCEA006B96CD000000050000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000202
      01202B1E0C7F5F4315BA705217C8503D14A814110554040301270202001E0000
      0011000000040000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000050032468A47A2C3E4BEEC
      FAFF4BA9C9E92C6876C48D6A41C0D7A263EDD7A263ED8D6A41C02E6773C258B1
      CFEC69B9D4ED006B94C90002031D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000050000000A0000000D0000000D00000009000000040000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000002002D3D800079
      A4D1007199CA0000000700000000000000000000000000000000000101170079
      A4D1007097C90004052700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000031B0B1A78000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007D5328E89864
      31FF986431FF412B15A700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000004242424AE807A75F94D4C4CCC0101
      0140000000000000000000000000000000000000000000000000000000000000
      0000000000003517329C5B2756D2000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D3D3D3E8FFFFFFFFFFFF
      FFFFDCCAB8FF764E26E100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000C0A083B81140F91C91B13E8FF0000072D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000202024DBBA891FF916632FF9E784AFF7F7A
      73F0000000100000000000000000000000000000000000000000000000000000
      000033183090AF64A7FFA95FA2FF8A3B83FF82357AFF793074FF60235BEA3713
      34B7130612700100011D0000000000000000532800E8643100FF643100FF9A77
      56FFF3EDE7FF986431FF1F150A75000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040411452926
      B6DE5654EBFF6A6BE7FF312BE9FF0000072D653215D2592A11C9231006810000
      000C000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000015151582B0926BFF916632FF916632FFB2A0
      8AFF000000540000000000000000000000000000000000000000000000002F18
      2C84BC71B3FFD195C8FFCF93C7FFCD8EC5FFCA89C2FFC784C0FFBD78B6FFAC63
      A5FF91498BFF30102EB00401043900000000381B00BF643100FF643100FF7649
      1CFFFFFFFFFFC8AB90FF674521D3000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008081A546060F3FF7B7D
      EEFF9398EBFF8C90E9FF3935ECFF0100072D6D3619D2D68D60FFB76441FF6931
      13DE0A0401480000000000000000000000000000000000000000000000000000
      0000000000000000000A0000001D21212195A9895FFF916632FF916632FFBEA6
      88FF03030377000000000000000000000000000000000000000000000000361B
      338AC377B9FFD79DCEFFD49ACBFFD296CAFFCF91C7FFCC8CC3FFCA87C1FFC178
      B9FFC37CBCFFA35B9DFF321130B00100011D1F0F008F643100FF643100FF6431
      00FFB8A089FFEDE5DCFF986431FF090603400000000000000000000000000000
      000000000000000000000000000000000000000001155555E0F39498F2FFA4A9
      EEFF7980E6FF9699EBFF423EEFFF0101072D723B1DD2DB986EFFE6A37DFFCB7E
      55FFA5532FFF0E06025400000000000000000000000000000000000000444E4E
      4EDB1C1C1CBD74716DF68F877CFF868585FDA8865CFF916632FF916632FFB295
      70FF1E1E1EC1000000710000002B000000000000000000000000000000000000
      00003E203B93C779BCFFC175B7FFAA52A0FFA24C99FF9A4792FFAB61A3FFC282
      BAFFC986C0FFC581BEFF964E90FF140713700B050056643100FF643100FF6431
      00FF7E542AFFFFFFFFFFB28B64FF986431FF986431FF986431FF986431FF7B51
      28E65E3F1FCA26190C8100000000000000000000011126265A995C5CEDF98384
      F4FFA2A7EFFF9FA4EDFF4D4AF1FF0101072D794122D2DEA077FFDE8859FFE6A5
      7DFFD0865AFF823D18F300000015000000000000000000000011686662ECA27E
      52FFBAA892FF918F8DFFB0A18EFFBFA789FFA8865CFF916632FF916632FFA785
      5AFFD1C1ACFFC1AE96FF908A83FD0404046B0000000000000000000000000000
      0000000000004925459F7E3F76D20000000000000000000000002F162D8D7434
      6CE1C585BDFFC987C1FFB26DABFF3C1639B700000019532800E8643100FF6431
      00FF643100FFD9CCBEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F0EBFFEADF
      D5FFCDB299FF664321D1412B15A7000000000D1E0F7309170A660101021B1E1E
      4C8D5252E0F37A7CF3FF5756F4FF0101072D7D4827D2E1A780FFE09162FFD977
      4AFFE7A783FFBD6E48FF26120784000000000000000005050577B9A48AFF9166
      32FF926834FFBEB4A7FFB4AB9FFFD3C3AEFF936936FF916632FF916632FF9369
      36FFE2D8CAFF787878F66A6A6AE61313139E1509146114091361130712611207
      116111061061000000032C17297B000000000000000000000000000000002913
      2783833C7CED863B7EF67F3678F8692A62E800000000391C00C0643100FF6431
      00FF643100FF643100FF643100FF643100FF643100FF734517FF855B33FFB197
      7CFFEDE3DAFFDCCAB8FF784F26E222160B7A326C37CF8FBE92FF347B39EA0B1B
      0C720101021D1515397B4948D5ED0202072D844E2DD2E4AF8AFFE39B6BFFDD84
      52FFE29565FFDA966BFF643115CF0000000000000000484848D09E784BFF9166
      32FF9C7546FFBFA788FF926834FFC5AF93FF916632FF916632FF916632FFA17C
      4FFFE6DCD0FF89847CFB0000002500000000853E7DE8904188F8863B7EF67331
      6EED210D1F83000000000000000000000000000000001506157B00000003180C
      1761170A1561160A15611509136113081261000000001F0F008F643100FF6431
      00FF643100FF643100FF643100FF643100FF643100FF643100FF643100FF8156
      2CFF9A7756FFF3EEE8FFCAAE94FF5C3D1EC84B9A53F0CAE8C8FFAED9AAFF91BE
      93FF317936ED0C1F0D7E0000011401010118895632D2E7B692FFE7A575FFE08F
      5BFFDE8D5AFFE6AC87FF934A23F3000000000000001F938B81FF916632FF9166
      32FFB0926CFFA48154FF916632FFC4AD91FF916632FF916632FF916632FFC7B2
      97FFC1AA8DFFBAA183FF1212129C00000000572A52B7C27EB9FFCC8CC3FFC382
      BCFF6B2E65E12710258D000000000000000000000000441641D2250B239F0000
      000000000000000000000000000000000000000000000B050057643100FF6431
      00FF643100FFA58768FFCBB8A7FFBAA38CFFA28363FF8E6843FF643100FF6431
      00FF75481BFFB59C84FFE9DED3FF905E2FF85AAF65F9B5E0B0FF79CC6DFF91D2
      88FFACD9A8FF88B78AFF1331159D18100A56C37E4CF6EFC8A8FFE6A571FFE29A
      65FFE29764FFE7B28EFF965027F0000000000000005FB7A48CFF916632FF9166
      32FFAA8A61FFA9895FFF916632FFC2AB8EFF916633FF916632FF9D7648FFBAA0
      7EFFC2AB8FFF956B39FF75726FF50000000D22112070BE73B5FFD297C9FFCF93
      C7FFC785BFFFA65C9FFF85377DFF7C3276FF742C6FFF8F4789FF8A4385FF200A
      1F93000000000000000000000000000000000000000000000019532800E86431
      00FF643100FF8E6843FFFFFFFFFFFFFFFFFFFFFFFFFFE3DAD1FF8B643EFF6431
      00FF643100FF865D35FFFCFBF9FF946130FB437E4CCFB9DFB7FF78CC6BFF64C5
      57FF70C864FFA7DAA2FF66A76BFF0307033C7E5435C3E6B48DFFF0CCABFFE5A5
      6FFFE9B083FFE3AE87FF764021CF0000000013131394B59874FF916632FF9166
      32FFAA8A61FFAC8D64FFAC8C64FFCAB59CFF916632FF916632FFB39570FFAF91
      6AFFC5AE93FF926733FFAE9E8AFF000000540201021D552B50B0C984BFFFD49A
      CBFFCE8EC4FFCF91C7FFCC8CC3FFCA87C1FFC582BDFFC37CBCFFC077BAFF8C45
      87FF1D091C8A0000000000000000000000000000000000000000391C00C06431
      00FF643100FF7A4F23FFD7C9BBFFFFFFFFFFFFFFFFFFFFFFFFFFAF9479FF6431
      00FF643100FF683706FFFDFDFCFF52361BBC1E352184B2DAB4FFA1D899FF6BCA
      5DFF64C556FF7ECC73FFA2CEA2FF28602DCF08050332D38E5AFCF2D1B1FFEBB8
      8CFFF0C9A9FFD89B6EFF321C0F8400000000323232B7A48155FF916632FF9166
      32FFA8865CFFD4C3AFFFC7B196FFB69B77FF916632FF916632FF9A7242FFD1BF
      A9FFDFD3C4FF966D3BFFBA9F7DFF0C0C0C8E0000000009040839572C52B0C277
      B8FFCA88C0FFD093C8FFD194C8FFCF8FC5FFCB8BC2FFC986C0FFC37EBCFF944C
      8EFF1C0A1B8400000000000000000000000000000000000000001F0F008F6431
      00FF643100FF6B3B0BFF9E7D5DFFFFFFFFFFFFFFFFFFFFFFFFFFA08060FF6431
      00FF643100FF663403FFFFFFFFFF070502390001001268B774F3BDE1BDFF93D4
      88FF6CCA60FF6CC95EFF9DD696FF92C095FF0A190B694D352296E7B38DFFF3D3
      B4FFE9BA97FFB87243F30000001200000000313131B5A48256FF916632FF9166
      32FF976E3DFFBEA587FF916632FF916632FF916632FF916632FF916632FF9166
      33FFC9B49AFF916632FFAB8B62FF242424AC00000000000000000201021D2311
      21705C2E56B791478AEAA54E9BFF9D4995FF95438DFFA75E9FFFA1599AFF2710
      25900000000000000000000000000000000000000000000000000B0500576F3F
      11FF643100FF643100FF724315FFB59D85FFA98D70FF9A7756FF7D5227FF6431
      00FF643100FF8A653EFE3129227F00000000000000000C140D519FD5A7FFB9E0
      BAFFA6DA9FFF7BCE6EFF7DCD71FFAFD9ACFF3E8A45ED02020123C08858EDEBC1
      9DFFDDA171FF140D085100000000000000000A0A0A84BAA284FF916632FF9166
      32FF916632FFC7B298FF936835FF916632FF916632FF916632FF916632FFBFA7
      89FFA38054FF916632FFB89E7CFF1414149A0000000000000000000000000000
      00000000000000000000000000000000000000000000662F61D23517329C0000
      0000000000000000000000000000000000000000000000000000000000193018
      00B1643100FF643100FF643100FF643100FF643100FF643100FF643100FF6637
      06FD653608FB2E28227F00000000000000000000000000000000070C083F5A9B
      63DEB3DCB6FFBBE0B9FFB8E1B4FFCEEACBFFA3CEA5FF19371C96261B1269A978
      4EDE0D09053F0000000000000000000000000000001961605CE8B79C7BFF9D77
      48FF9F794CFFBFA789FFD1C3B2FFA38053FF98703FFFA9895FFFC8B399FFA887
      5DFF956B39FFB79E7DFF5B5957E6000000220000000000000000000000000000
      0000000000000000000000000000000000000000000023102178000000030000
      0000000000000000000000000000000000000000000000000000000000000E07
      0061321800B5643100FF643100FF643100FF643100FF643100FF401F00CC2C15
      00A90C06005A0000000000000000000000000000000000000000000000000000
      000C1D32207E44784BC666BD73FC5FB76BFC366D3DC61429167D000000030000
      000A00000000000000000000000000000000000000000000001E131313914141
      41C23C3C3CBD1515159203030372383838BC4F4F4FD03E3E3EC46E6C6AEB817C
      77FC646361E41313139600000019000000000000000000000000000000000000
      0000000000000000000000000000000000000000006DFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF0000006D0000000000000000000000000000
      00000000000000000000000000000000000000000000000000000200011A3300
      1A80870043D57E003DD5270012800100001A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000C09
      07497D6047D6BA8F66FF7A583DDA0D0A074C0000000000000000000000000000
      000000000000000000000000000000000000000000001A1A1A5D363636870000
      001200000012323232871717175D000000000000000000000000000000000000
      0000000000000000000000000000000000000000006DFCFCFCFF0F0F0F6D0F0F
      0F6D0F0F0F6D0F0F0F6DFCFCFCFF0000006D0000000000000000000000000000
      000000000000000000000000000000000000000000000000000039001E80EE7A
      B8FFF58FC9FFF384C2FFE476B0FF280012800000000000000000000000000000
      000000000000000000000000000000000000000000000B100B561B2C1D87C4A0
      7EFFD6BCA1FFC4A07AFFB78C5FFFB08054FF0000000000000000000000000000
      0000000000000000000000000000000000000000000010101048CCCCCCFFADAD
      ADF3A9A9A9F3C0C0C0FF0E0E0E48000000000000000000000000000000000000
      0000000000000000000000000000000000000000006DFCFCFCFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFBFBFBFF0000006D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A70059D5F8A8
      D6FFF56EBCFFF465B6FFF385C1FF820040D50000000000000000000000000000
      0000000000000000000000000000000000004B684EC88FBD96FF84B78BFFC7A4
      82FFC9A98BFFCBA782FFB88C61FFB48558FF0000000000000000000000000000
      000000000000000000000000000000000000000000000606062DCCCCCCFFDBDB
      DBFFD7D7D7FFBEBEBEFF0505052D000000000000000000000000000000000000
      0000000000000000000000000000000000000000006DFCFCFCFF0F0F0F6D0F0F
      0F6D0F0F0F6D0F0F0F6DFAFAFAFF0000006D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AD005ED5FBB7
      DFFFF87CC7FFF56CBBFFF590C9FF8D0047D50000000000000000000000000000
      00000000000000000000000000000000000091BD97FFABCFB1FF79A29DFF6765
      C3FF6367E8FF605FC2FFA48B8CFFA17F5CF10000000000000000000000000000
      00000000000000000000000000000000000000000003636363B1D7D7D7FFDEDE
      DEFFDBDBDBFFCDCDCDFF595959B1000000030000000000000000000000000000
      0000000000000000000000000000000000000000006DFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF6F6F6FF0000006D0000000000000000000000000000
      000000000000000000000000000000000000000000000000000040002380FD7E
      C3FFFBB8DFFFFAB0DBFFF07BBAFF36001C800000000000000000000000000000
      0000000000000000000000000000000000007EB386FF80B588FF6D71E9FF9493
      F4FF7174ECFF5B5FE8FF5057E2FF0101021F0000000000000000000000000000
      0000000000000000000000000000000000004B4B4B99D5D5D5FFD7D7D7FFDEDE
      DEFFDADADAFFCCCCCCFFC0C0C0FF414141990000000000000000000000000000
      0000000000000000000000000000000000000000006DFBFBFBFF0F0F0F6D0F0F
      0F6D0F0F0F6DF4F4F4FFF7F7F7FF0000006D0000000000000000000000000000
      00000000000000000000000000000000000000000000000000000200011A4000
      2380AF005ED5AB005BD53B0020800200011A0000000000000000000000000000
      0000000000000000000000000000000000001F2C21834D7554D67A7CECFF7072
      EEFF8281F2FF5E61E8FF555BE5FF000101180000000000000000000000000000
      000000000000000000000000000000000000000000090C0C0C3F37373784D3D3
      D3FFCFCFCFFF353535870B0B0B3F000000090000000000000000000000000000
      0000000000000000000000000000000000000000006DF8F8F8FFF7F7F7FFF4F4
      F4FFEDEDEDFFF5F5F5FF0000006D000000090000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005F61CBF16E6F
      EDFF6265EAFF6F70EDFF5D60CAF10000000A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007F7F
      7FC97C7C7CC90000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000006D0000006D0000006D0000
      006D0000006D0000006D00000009000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000110B0B
      1957141538800B0B195700000011000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000D0D
      0D420D0D0D420000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002050231315834C16D93
      61F66B9765F62B5531C502050236000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010101171011
      127D0E10107D0101021B00000000000000000000000000000000000000000000
      00001815137B24272D9F0302022F0000000000000000000000002018149D1915
      158C000000010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000205033067AB7AF4A8CF98FFCBAF
      72FFC5A76BFF74BA88FF599868F6020502360000000000000000000000000000
      00000000000000000000000000000000000000000000000000001013146ABCBA
      B9FFA7A4A3FF0C0E107200000000000000000000000000000000000000000000
      00001514136124282D914E6177E62A3139A81C1E238D20252CA1383131D45674
      96FF2F2118B4362E2FD2000000090000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000396942BD90D39AFFA5B96DFFD6A7
      57FFD09D4EFFA09F55FF7DB386FF2B5531C60000000000000000000000000000
      000000000000000000000000000000000000000000000000000D395F7ECB50B0
      CCFF48A4BEFF1C466ECC00000112000000000000000000000000000000000000
      0000000000000000000008070745829DB6F96E9CC5FF6896C0FF6591BBFF698F
      B1FF6189AFFF373134CE000000000000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000082C796F690D580FFC4BC73FFD4A9
      58FFBEAA5CFFB4A256FF72A564FF67A375F70000000000000000000000000000
      00000000000000000000000000000000000000000001416680BC87C7F3FF6FE2
      F6FF6BE1F6FF6EBAF0FF396A99D3000000090000000000000000000000000000
      0000000000000000000000000000040302313F4244AB6C7F91ED7EA4C6FF6D9B
      C5FF6B9AC4FF556E89FA0000000D0000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000082C898F58BE087FFCCE7AFFFCAB3
      65FFA7D670FF69D446FF65BC64FF6AA77AF70000000000000000000000000000
      0000000000000000000000000000000000000E181D5BC4EAF9FF68D1F4FF6ACA
      F3FF61C5F2FF49C7F1FFA7DDF5FF0B1A286E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000002060403394C54
      5DC56D9CC5FF628EB8FF070504480000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFF808080FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000003B6C46B9B6EFC1FFC3EDB4FFD7D5
      9AFFCCBD72FFA5CD73FF93DD9EFF325E3AC10000000000000000000000000000
      0000000000000000000000000000000000001B2B3477D5F5FDFFDCF8FDFFE0F8
      FDFFDAF6FDFFB9F2FBFFB1ECFAFF0C21347D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000D526172DE6A99C3FF1B1D21850000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF808080FF808080FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000205032B7FC293F0CFF1DBFFD1E4
      BDFFD8D7A4FFCDCA98FF7AB581F4020503310000000000000000000000000000
      00000000000000000000000000000000000003040525ACD2E0F2E0F9FDFFE3FA
      FEFFDEF9FDFFD2F7FDFF87C2E8FB030609330000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000070605466B90B4FE607A93F40908094B00000000808080FFC0C0C0FFFFFF
      FFFF808080FF808080FF808080FF808080FF808080FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000205022A3E6B46B885C3
      8DF489C388F8406E46BD03060331000000000000000000000000000000000000
      0000000000000000000000000000000000000000000007090A346C8793C4ACD6
      E6F6A5D4EAF95A8AA6D4060C0F43000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000D0C0B56464341BB4F4138DC00000000808080FFFFFFFFFF8080
      80FF808080FF808080FFFFFFFFFF808080FF808080FF808080FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFC0C0C0FFFFFF
      FFFF808080FFFFFFFFFFC0C0C0FFFFFFFFFF808080FF808080FF808080FFFFFF
      FFFFC0C0C0FF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFF808080FF808080FF8080
      80FFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFF808080FFFFFF
      FFFFC0C0C0FF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000069D0000069D0000069D000026B
      D400026BD400026BD400026BD400026BD400026BD400026BD400026BD400026B
      D400026BD400026BD400026BD400026BD4000000000000000000000000000000
      0000000000000000000000000000336B96C62C9FF8FF04090C39000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000026BD400FFFFFF00FFFFFF00F6FD
      FA00F6FDFA00F6FDFA00F6FDFA00F6FDFA00F6FDFA00F6FDFA00F6FDFA00F6FD
      FA00F6FDFA00F6FDFA00FFFFFF00026BD4000000000000000000000000000000
      00000000000000000000336B96C61C98F8FF1C98F8FF2C9FF8FF0202031C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000026BD400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00026BD4000000000000000000000000000000
      000000000000336B96C61C98F8FF1C98F8FF1C98F8FF1C98F8FF2C9FF8FF0202
      031C00000000000000000000000000000000000000000819297012395CA62470
      B5EA297FCAF7297FCAF7297FCAF7297FCAF7297FCAF7297FCAF7297FCAF7297F
      CAF7297ECAF72779C0F10E2C47930000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF0000000000000000026BD400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F6FDFA00F6FDFA00F6FDFA00F9FFFE00F9FFFE00F9FF
      FE00F9FFFE00F9FFFE00F9FFFE00026BD4000000000000000000000000000000
      0000263C4E8E1C98F8FF1C98F8FF1C98F8FF1C98F8FF1C98F8FF1C98F8FF3687
      C5E300000000000000000000000000000000000000002872B0E6C3D4DDF0A4D9
      F0FD9DDBF4FF95DAF3FF8DD8F3FF85D7F3FF7DD4F2FF77D3F2FF70D2F1FF6AD0
      F1FF67CFF1FFBFE8F6FE2C7DC0F00000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF00000000000000000069D000FFFFFF00FFFFFF00FFFF
      FF00EEF0F000F6F6F600F6F6F600EEF3F100F9FFFE00F9FFFE00F1F6F500EEF3
      F100F6FDFA00FFFFFF00FFFFFF00026BD4000000000000000000000000000000
      0000000000001A364D8E1C98F8FF1C98F8FF1C98F8FF1C98F8FF2F84C5E30000
      000000000000000000000000000000000000000000003089CCF7EFFAFEFFA0E9
      F9FF90E5F8FF80E1F7FF70DEF6FF61DAF5FF52D7F4FF45D3F3FF37D0F2FF2CCD
      F1FF24CBF0FFCAF2FBFF3089CCF70000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF0000000000000000026BD400FFFFFF00FFFFFF00CED2
      D3006C7271007C818000DCDCDC00FFFFFF00F6FDFA00AAAFAD00636664007B80
      7E00C8CFCC00FFFFFF00FFFFFF000069D0000000000000000000000000000000
      000000000000000000001A364D8E1C98F8FF1C98F8FF3285C5E3000000000000
      000000000000000000000000000000000000000000003291CEF8F2FAFDFFB2ED
      FAFFA3E9F9FF94E6F8FF84E2F7FF74DEF6FF63DBF5FF55D7F4FF47D4F3FF39D1
      F2FF2ECEF1FFCCF2FBFF308DCCF70000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF0000000000000000026BD400FFFFFF00C4C6C6006366
      6400787D7C008A8C8C00E9EEEC00DCDCDC00555757000D1210004E514F008A8C
      8C0099999900D1D2D000F6FDFA00026BD4000202021CA8A6A5E3181817550000
      00000202021C575655C6121212550C203071296696C60A0B0C39A5A4A3E30A0A
      0A39000000000202021C777776E308080839000000003298D0F9F6FCFEFFC8F2
      FCFFB8EFFBFFABECFAFF9BE8F9FF8AE3F7FF7AE0F6FF6ADCF6FF5BD9F5FF4DD6
      F4FF42D3F3FFD0F3FCFF3094CCF70000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF0000000000000000026BD400F6FDFA00999999004A4F
      4E00FFFFFF00FFFFFF00B2B4B4001E1E1E000000000073767400E4E5E300FFFF
      FF007C81800084848400E9EEEC00026BD400A3A2A1E3C0BDBCFFC4C2C1FF1818
      1855737270E3656362FF706E6DFF07070739000000007A7978C6C0BDBCFFC7C4
      C3FF181818556B6968E3656362FF757372FF00000000339FD1FAFEFFFFFFF8FD
      FFFFF6FDFFFFF5FCFFFFF3FCFEFFD8F6FCFF93E6F8FF84E3F7FF74DFF6FF66DB
      F5FF5AD8F4FFD7F4FCFF3099CCF70000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF0000000000000000026BD400FFFFFF00E4E5E3009999
      9900848484006A6C6C001B1D1D0033333300BFC1C100E9EEEC0090979400666B
      6A004A4F4E00A3A8A600F6F6F600026BD400403F3F8EC0BDBCFFC0BDBCFFA4A1
      A1FF656362FF656362FF454444C60202021C0000000027262671C0BDBCFFC0BD
      BCFF9D9A99FF656362FF656362FF4D4C4CC60000000031A4D1FAE8F6FBFF93D4
      EFFF87CEEEFF71C0E9FFC9E9F6FFF2FCFEFFF3FCFEFFF2FCFEFFF0FCFEFFEFFB
      FEFFEEFBFEFFFEFFFFFF31A0CCF70000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF0000000000000000026BD400FFFFFF00FFFFFF00EEF0
      F0008A8C8C00575D5C008A8C8C00E9EEEC00FFFFFF00F1F6F500A3AAA7006C72
      7100A3AAA700F1F6F500FFFFFF00026BD40000000000403F3F8EC4C2C1FFA29F
      9EFF656362FF454444C60202021C000000000000000000000000403F3F8EC0BD
      BCFF9B9897FF656362FF4D4C4CC600000000000000002F98C4F2F1FAFDFF93DE
      F5FF92DCF4FF80D5F2FF68CAEDFF6ACBEAFF84D3EFFF7ED2EFFF78D0EFFF74CF
      EEFF70CFEEFFE9F7FBFF2E9DC6F30000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF0000000000000000026BD400FFFFFF00FFFFFF00FFFF
      FF00F6F6F600E9EEEC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F6F6
      F600FFFFFF00FFFFFF00FFFFFF00026BD400000000000000000027262671A29F
      9EFF4D4C4CC60A0A0A390A0A0A3900000000000000000202021C07070739403F
      3F8E9B9897FF4D4C4CC60000000000000000000000002D9AC1F0F7FCFEFF8DE4
      F8FF90DEF5FF9EE0F5FFABE1F6FFEFFBFEFFF4FDFEFFF3FCFEFFF1FCFEFFEFFB
      FEFFEEFBFEFFEEF1F3F9247896D40000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF0000000000000000026BD400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00026BD4000000000000000000000000003A3A
      3A8E000000007A7978C6C4C2C1FF0A0A0A3900000000545352C6706E6DFF0202
      021C3838388E0000000000000000000000000000000032A8CEF8FDFEFEFFFEFF
      FFFFFEFEFFFFFDFEFFFFFEFFFFFFEAF7FBFF65BDD9F963BCD7F863BCD7F863BC
      D7F86FC0D8F7589DB2E1051318560000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF0000000000000000026BD400FFFFFF00FFFFFF00F6FD
      FA00F6FDFA00F6FDFA00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00026BD4000000000000000000000000000000
      00004242428EC0BDBCFFC0BDBCFFC4C2C1FF757473E3656362FF656362FF6D6B
      6AE30000000000000000000000000000000000000000227790D058B9D8FA5ABA
      D9FA5ABAD9FA5ABAD9FA59BAD9FA2C88A4DD000101160000000F0000000F0000
      000F0000000F0000000F000000040000000000000000808080FFFFFFFFFFC0C0
      C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0
      C0FFFFFFFFFF808080FF0000000000000000026BD400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00F6FDFA00F6FDFA00F6FDFA00F6FDFA00F6FD
      FA00F6FDFA00F9FFFE00F9FFFE00026BD4000000000000000000000000000000
      00000000000027262671C4C2C1FFC0BDBCFF827E7DFF656362FF4D4C4CC60000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFC0C0C0FFFFFF
      FFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFFFFFFC0C0C0FFFFFF
      FFFFC0C0C0FF808080FF0000000000000000026BD400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00026BD4000000000000000000000000000000
      00000000000000000000403F3F8EC0BDBCFF7C7978FF636160E3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF00000000000000000069D000026BD400026BD400026B
      D400026BD400026BD400026BD400026BD400026BD400026BD400026BD400026B
      D400026BD400026BD400026BD400026BD4000000000000000000000000000000
      0000000000000000000000000000403F3F8E777675E30202021C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000081A205D0C3745870000
      0112000001120732448703171F5D000000000000000000000000000000000000
      00000000000000020E44020E4A98031467B3031364B1020C3E8C00030F460000
      0008000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000402
      00262F1F02822618017400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000051013484BCFF7FF21B1
      DEF31BADDDF331C2F4FF010E1348000000000000000000000000000000030000
      03230000000003115AA70628D1FF0628D1FF0628D1FF0628D1FF0628CFFE041A
      8AD000020D410000000000000000000000004735279BC28D66FFBF8A64FFBD87
      62FFBA845FFFB8825DFFB57E5CFFB37C5AFFB17A58FFB07956FFAD7755FFAC74
      54FFAA7352FFA87151FFA86F4FFF3B281D9B000000000000000000000000160E
      0059B97907FF654204BD00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000206072D4AD0F7FF82DE
      F9FF78D9F9FF2AC0F4FF0005072D000000000000000000000004020F4F9D010D
      42910000000003115AA70628D1FF0628D1FF0628D1FF0628D1FF0628D1FF0628
      D1FF0626C5F80107246B0000000000000000C8916AFF505050FF515151FF5252
      52FF535353FF545454FF555555FF555555FF565656FF575757FF585858FF5959
      59FF5A5A5AFF5A5A5AFF5B5B5BFFA8704FFF000000000000000000000000160E
      0059B97907FF784E04CD00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000003266577B172DAF8FF8BE0
      FAFF82DDF9FF52D0F6FF0D5975B10000000300000000020A35810628D1FF010D
      43920000000003115AA7051C93D603115AA7031362AF0521AAE60628D1FF0628
      D1FF0628D1FF0627C9FA0003104800000000CA936CFF4C4C4CFF3B3B3BFF3B3B
      3BFF3C3C3CFF3D3D3DFF3F3F3FFF404040FF414141FF424242FF434343FF4444
      44FF444444FF464646FF585858FFA97151FF0000000000000000000000001F14
      016AB97907FF7E5304D300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000224E599968D8F9FF71DAF8FF8AE0
      FAFF81DDF9FF4FCEF7FF30C2F4FF094157990001062C0625C1F50628D1FF010D
      4392000000000000052A0000000600000000000000000000000E010C3E8C0628
      CDFD0628D1FF0628D1FF051C93D60000000ACC966DFF494949FF363636FF3737
      37FF383838FF3A3A3AFF3B3B3BFF3D3D3DFF3D3D3DFF3F3F3FFF404040FF4040
      40FF424242FF434343FF555555FFAB7352FF0000000000000000000000002216
      016FB97907FF7E5304D300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000009050D0F3F1538428464D7
      F8FF57D2F8FF0A364587010B0E3F00000009020D43910628D1FF0628D1FF010D
      4392000000000000000004171D5816647FB7135970AC020A0D3C00000002020D
      47950628D1FF0628D1FF0628D1FF00041653CF9970FF454545FF313131FF3232
      32FF343434FF353535FF373737FF383838FF393939FF3A3A3AFF3C3C3CFF3D3D
      3DFF3E3E3EFF3F3F3FFF535353FFAC7654FF0000000000000000000000002216
      016FB97907FF7E5304D300000000000000000000000000000000000000000000
      0000000000000000000000000001000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002F81
      99C9267F99C9000000000000000000000000051D96D80628D1FF0628D1FF010D
      43920000000004151A5429BEF2FC2AC3F8FF2AC3F8FF26B1E0F3010406280000
      01170627CAFB0628D1FF0628D1FF020E4896D19B71FF404040FF2D2D2DFF2E2E
      2EFF2F2F2FFF313131FF323232FF343434FF343434FF363636FF383838FF3939
      39FF3B3B3BFF3B3B3BFF505050FFAF7856FF0000000000000000000000002216
      016FB97907FF7E5304D300000000000000000000000000000000000000000000
      0000000000000101001826180174000000030000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000050D
      1042040D10420000000000000000000000000628C9FA0629D1FF0629D1FF020D
      449200000000135C75B02AC3F8FF2AC3F8FF2AC3F8FF2AC3F8FF092D397B0000
      0000041D92D50629D1FF0629D1FF03156BB7D49D73FF3B3B3BFF272727FFD1D1
      D1FFACACACFF2C2C2CFF2D2D2DFF2F2F2FFF303030FF323232FF343434FF3535
      35FF363636FF383838FF4C4C4CFFB17A58FF0000000000000000000000002216
      016FB97907FF7E5304D300000000000000000000000000000000000000000000
      0000000000000D080045B77907FE472F03A00000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000934CDFA0A37D5FF0A37D5FF0313
      4A970000000011526CA92AC3F8FF2AC3F8FF2AC3F8FF2AC3F8FF082732740000
      0000062694D50A37D5FF0A37D5FF051B6EB7D59F74FF373737FF232323FF2424
      24FFDEDEDEFF727272FF282828FF2A2A2AFF2B2B2BFF2D2D2DFF2F2F2FFF3030
      30FF323232FF333333FF494949FFB47C5AFF00000000030200244C3103A4784E
      04CDB97907FFAD7107F77E5305D3503403A90000000000000000000000000000
      00000000000034230289B97907FF644204BC0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000A349CD80E4ADAFF0E4ADAFF0932
      91D00000000102090C3B26AFE1F42AC3F8FF2AC3F8FF2096C3E3000101170000
      01150E48D1FA0E4ADAFF0E4ADAFF04194C97D8A177FF323232FF1E1E1EFFCFCF
      CFFFA7A7A7FF222222FF232323FF242424FF262626FF282828FF2A2A2AFF2B2B
      2BFF2D2D2DFF2F2F2FFF444444FFB6805CFF000000000C080043B97907FFB979
      07FFB97907FFB97907FFB97907FF6D4704C40000000000000000000000000000
      00000000000D885905DBB97907FF291B01790000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000051D4892125DDEFF125DDEFF125D
      DEFF020B195600000000010608300A314385092A387B0002031D00000002061E
      4892125DDEFF125DDEFF125DDEFF01091754D9A277FF323232FF1E1E1EFF1F1F
      1FFF202020FF222222FF232323FF242424FF262626FF282828FF2A2A2AFF2B2B
      2BFF2D2D2DFF2F2F2FFF444444FFB9845EFF000000000201001E2316016F2316
      016F1F15016A160E0059160E00590F0900490000000000000000000000000000
      00002116016EB77907FEAD7107F7030200240000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000003072E1666D3F61770E3FF1770
      E3FF1564CBF1030E1C5B0000000500000000000000000000000D06214189176E
      DDFC1770E3FF1770E3FF104EA2D80000000BDBA378FF2F2F2FFF303030FF3131
      31FF323232FF333333FF343434FF353535FF373737FF393939FF3A3A3AFF3B3B
      3BFF3D3D3DFF3F3F3FFF414141FFBC8661FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000503
      002E9C6606EAB97907FF422B0299000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000006213D841B84E8FF1B84
      E8FF1B84E8FF1B84E8FF135BA0D40C3861A50D3D69AC1669B9E41B84E8FF1B84
      E8FF1B84E8FF1B7FE0FB020A134A00000000DCA679FFDBA378FFDAA277FFD8A1
      77FFD7A076FFD59E74FFD39D72FFD19B71FFCF9970FFCD966EFFCB946CFFC993
      6AFFC79069FFC38E67FFC28C65FFBF8A64FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000030200258154
      05D5B97907FF815405D501000015000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000050B375CA02094
      EBFE2097EDFF2097EDFF2097EDFF2097EDFF2097EDFF2097EDFF2097EDFF2097
      EDFF1E8EE0F905192B6E0000000000000000D9A882FDF1DCCEFFEAC09FFFE8B8
      91FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFCDC8C4FFE8B891FFCDC8
      C4FFE8B891FF4262FFFFE8C3A6FFBC8D6BFD0000000000000000000000000000
      000000000000000000000000000000000000000000030E0900478C5B06DEB979
      07FF855605D80503002C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000003051B
      296B1D88C6E824AAF2FF24AAF2FF24AAF2FF24AAF2FF24AAF2FF24A7F0FE186F
      A3D2020A104400000000000000000000000080634CC2CAA180F4DCA679FFDCA5
      78FFDAA378FFD8A177FFD8A077FFD59F74FFD49D73FFD29C71FFCF9970FFCE98
      6EFFCB956DFFC9936AFFB38C6EF4705440C20000000000000000000000000000
      000000000005000000070000000F0F0A004B563903AFB57707FCB37507FB5438
      03AD0201001C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000011304151E5B0D3F589A13597BB5125676B20B364C8F020D13490000
      0008000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000503002D2F1F0282734C04CA936006E3744C04CA3A25028F0906003A0000
      0001000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0008000000010000000000000000000000000000000000000000000000000000
      0001000000080000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002121215CFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE9E7E1FE342F249B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000012940
      4795172428770000001200000001000000000000000000000001000000140423
      2C7B0635428F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000037373777FFFFFFFFFFFFFFFFFDFD
      FDFEF5F5F2FF897E5DF33B3118BA000000060000000000000000B3B3B3FFB3B3
      B3FFB3B3B3FF000000002A2A2AFF232323FF1D1D1DFF161616FF111111FF0B0B
      0BFF000000000000000000000000000000000000000000000000000000002238
      3E8667E0FEFF4B9CB4DF0D1A1F6B0000000E00000010031C2370299BB9E243DA
      FEFF0529327A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002F2E2E70F9F9F9FCFFFFFFFFFFFFFFFFFFFF
      FFFF796B46F429210CA602010122000000000000000000000000B3B3B3FFE4E4
      E4FFB3B3B3FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000003000000031017
      1A5A94EAFEFF96EAFEFF69DFFDFF378AA2D72C879FDC59DBF9FF1BD5FEFF29D6
      FEFF010F134F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000636332C8EF9F9F9FDFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFE3E0D8FFA59D87F4514117E6040301350000000000000000B4B4B4FFE5E5
      E5FFB3B3B3FF000000003C3C3CFF353535FF2E2E2EFF272727FF202020FF1A1A
      1AFF131313FF0E0E0EFF0101016B000000000000000D332F2EB62D2B2CB01212
      127893E6FAFFBDF3FFFF98EAFFFF6BDDFAFF516361FF386571FF0DB6D8FF30D5
      FBFF000203270000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000114436
      15D10E0B04613F3721B8F3F2EFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFDCD9CEFF5A4E31D706050242000000000000000000000000B6B6B6FFE6E6
      E6FFB3B3B3FF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000001000021605A57CC7D8894EE778B
      A2FF6A8997FF94A7A6FF83A5ABFF6293A1FF4D6669FF536D8DFF416571FF4774
      7BFF29211FBC0000000B00000000000000000000000000000000000000000000
      0000000000000000001A01010180000000D5000000D5000000800000001A0000
      00000000000000000000000000000000000000000000000000001B160983B1AA
      98F84F3E10EDA59F8DEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E6E0FF6657
      31EC322C1AA40000000E00000000000000000000000000000000B8B8B8FFE8E8
      E8FFB5B5B5FF000000004E4E4EFF474747FF404040FF393939FF313131FF2A2A
      2AFF000000000000000000000000000000000000000000000000000000032829
      28959CB7D0FF6D93B9FF5981ACFF5A85B0FF5B86B1FF60758DFF5A7EA4FF464B
      5CFF241D1CB10000000200000000000000000000000000000000000000000000
      000000000000040404805F5F5FFF828282FF7D7D7DFF565656FF000000800000
      00000000000000000000000000000000000000000000000000003D331CBAF9F9
      F8FF7F7049F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF827450F60A08
      044C000000000000000000000000000000000000000000000000BABABAFFEBEB
      EBFFB7B7B7FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000002020D105061C4
      DFF488AAB1FF9BB1C6FF89AED0FF729FC7FF6B9AC3FF6998C2FF6795BFFF517A
      A6FF364B53F101090B4700000001000000000000000000000000000000000000
      000000000000111111D5888888FF616161FF555555FF7D7D7DFF000000D50000
      0000000000000000000000000000000000000000000000000000362F19AFFDFC
      FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E7E0FF4C3E1ED40000
      0000000000000000000000000000000000000000000000000000BBBBBBFFEDED
      EDFFB9B9B9FF000000005F5F5FFF585858FF525252FF4B4B4BFF444444FF3C3C
      3CFF353535FF2E2E2EFF0707076B0000000000000002020D115339C1E2F53CD9
      FEFF7CE4FFFF9AC7CEFF9FAFB2FF94A2ABFF869FB7FF719EC6FF6A99C3FF6492
      BCFF485C74FF3EB0CDF2020C104F000000010000000000000000000000000000
      000000000000141414D58A8A8AFF666666FF5C5C5CFF818181FF020202D50000
      00000000000000000000000000000000000000000000000000001F1B1083DAD6
      CBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF867A5AF10A08044F0000
      0000000000000000000000000000000000000000000000000000BCBCBCFFEFEF
      EFFFBBBBBBFF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000020E125026C3EAF82ED6FDFF20D5
      FEFF59DCFEFF97EAFFFFBBF3FFFFC6F6FEFFA7DAE3FF8AA6ADFF7CA0C0FF6998
      C2FF4C749DFF52AABAFF34C0E2F501090C450000000000000000000000000000
      00000000000008080880696969FF8A8A8AFF868686FF626262FF020202800000
      0000000000000000000000000000000000000000000000000000261F0E98CDC8
      BCF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8E6DFFF493B17D7000000000000
      0000000000000000000000000000000000000000000000000000BCBCBCFFF0F0
      F0FFBBBBBBFF000000006C6C6CFF676767FF626262FF5B5B5BFF555555FF4E4E
      4EFF000000000000000000000000000000000000000400020323010D11470621
      296C1B434E92478799CBA3EDFEFFC5F6FFFFBEF4FFFF7EE4FDFF64787BEB729F
      C7FF5F8DB7FF1C282DA700020321000000030000000000000000000000000000
      0000000000000000001A08080880151515D5131313D5060606800000001A0000
      000000000000000000000000000000000000000000000101002172674DDEFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFBFF695A35EA03020031000000000000
      0000000000000000000000000000000000000000000000000000BCBCBCFFF0F0
      F0FFBCBCBCFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000011674C6DCF1B9F3FFFFC4F5FFFF63BCD5ED010101286F7B
      87EB6D9BC4FF556A80EC05030342000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1A0C88D6D2C8FBE1DD
      D5FFF0EFEBFFFFFFFFFFF8F7F6FF726543EB110E076500000000000000000000
      0000000000000000000000000000000000000000000000000000BCBCBCFFBCBC
      BCFFBCBCBCFF00000000767676FF737373FF6F6F6FFF6A6A6AFF646464FF5F5F
      5FFF585858FF525252FF0D0D0D6B000000000000000000000000000000000000
      000000000000000000001E384088A0ECFEFFA5EDFEFF1E31387F000000000B08
      064D8B96A1F8818D99FF6E889FF40201002A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000013615537DCFDFDFDFFCBC6
      B7FFF8F7F6FFD7D3C7FF60532BEA0B0A05530000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000011671C6DCF17FC4D6EE00000012000000000000
      00000100001F110D0A5F2318108D0301002E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003E341BBCE7E5DFFCD6D1C4FF7D70
      4DF2615331E6362D17B000000018000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001F3B448C1D32388000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000554A2AD7675835EA2F2710AD0000
      0010000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000101160000000F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002975A7EC2B7CB5F62A7AB4F62878B3F62776B2F62575B1F62473
      B0F62271AFF6216FAEF61F6DAEF613436DC30000000000000000000000000000
      000000000000000000004A89BDFF6DA7CBFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000061E0F88155F
      30F2176935FF155F30F2061E0F88000000000000000000000000000000000000
      000000A5CEEC00C0F1FF00C0F1FF00C0F1FF00BEEFFF00BEEFFF00BDEDFF00A3
      CCED000000000000000000000000000000000000000000000000000000000000
      0000000000002B73A1E4DDECF6FFBCEEF9FFABEAF8FFAAEAF8FFAAEAF8FFAAEA
      F8FFACEAF8FFD4F3FBFFA3C8E4FF103654AB0000000000000000000000000000
      00002C6584FF93C7F9FF90C9F9FF3F84C9FF2469ADFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000061C0E84268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF071F108C00000000000000000000000000A6
      D2EC18CFF5FF3AE3F9FF38E1F8FF36E1F8FF33E1F8FF32DFF7FF30E0F7FF12CC
      F1FF009CC4E80000000000000000000000000000000000000000000000000000
      0000000000000A1A236B8DC1E1FF96E8F9FF5FDCF6FF59DBF5FF3087C1FF59DB
      F5FF68DEF6FFB0E7F6FF246CA1E90103052A0000000000000000000000000000
      00004188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF468DC7FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000186434F760B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF166332F7000000000000000000A8D4EC1BD3
      F9FF42E5FAFF22D3F9FF11C9F7FF0EC8F6FF0BC5F5FF08C5F5FF14CDF5FF31E0
      F7FF12CCF1FF009CC4E800000000000000000000000000000000000000000000
      000000000000000000062A6485CCAADBEFFF72E0F7FF56DAF5FF56DAF5FF5BDB
      F5FF8FE6F8FF91C0E1FF0B212F7E000000000000000000000000000000000000
      0000A4C2D7FF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4F9CDDFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF0000000000AAD4EC1DD4FAFF48E6
      FBFF40DBFBFFC9F3FDFF39D3F9FF12C9F7FF0FC8F6FF31D0F7FFC4F2FDFF2ED4
      F7FF32E0F7FF12CDF2FF009CC4E8000000000000000000000000000000000000
      00000000000000000000050D104879BCDFFFA1EAF9FF5FDCF6FF2F86C1FF75E1
      F7FFB5DEF0FF28668ED60000000D000000000000000000000000000000000000
      000000000000B1D5E5FF74B9D7FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4797
      DCFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003E7E55F78FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF166332F70000000000C9FBFF4DE8FDFF30D8
      FBFFCBF4FEFFF2FCFFFFEBFAFEFF39D3F9FF37D2F8FFEAFAFEFFF1FBFEFFC4F2
      FDFF16CFF6FF31E0F7FF00BEEFFF000000000000000000000000000000000000
      00000000000000000000000000021F4457A3ABD9ECFF81E3F8FF3187C1FFABED
      FAFF419BCDFE0610155300000000000000000000000000000000000000000000
      00000000000000000000AED4E5FF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4999DDFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000014231A805FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF071F108C0000000000CAFDFF50E8FDFF26D2
      FCFF45D8FCFFECFBFFFFF2FCFFFFECFBFFFFECFAFEFFF1FCFFFFEAFAFEFF32D0
      F7FF0AC5F5FF33E1F8FF00BEEFFF000000000000000000000000000000000000
      00000000000000000000000000000204052A4496BAEDB3EBF8FF8DE6F8FFB4DD
      EEFF224F66B20000000500000000000000000000000000000000000000000000
      0000000000000000000000000000BCE5F2FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4FA1E2FF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001422197C4E85
      61F24D8D64FF38754FF20A1C107C000000000000000000CAFDFF53E9FEFF2AD3
      FDFF27D2FCFF46D8FCFFEDFBFFFFF2FCFFFFF2FCFFFFECFAFEFF38D3F9FF11C9
      F7FF0EC8F6FF36E2F8FF00C0F1FF000000000000000000000000000000000000
      000000000000000000000000000000000000142B357EA3D7EBFFDCF4FBFF4699
      BDEF030709350000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B9E3F0FF7BD4EEFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF5087B7F0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000CAFDFF55EAFEFF2ED4
      FDFF2BD3FDFF49D9FCFFEDFBFFFFF2FCFFFFF2FCFFFFECFBFFFF3BD3F9FF14CB
      F8FF12C9F7FF3AE1F8FF00C0F1FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000E3D829BD6AADBEDFF1936
      418C000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7D9E8FF80D6EEFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF5E94C5F70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000CCFFFF58EAFEFF31D5
      FEFF4EDAFDFFEDFBFFFFF3FDFFFFEDFBFFFFEDFBFFFFF2FCFFFFECFAFEFF3CD4
      FAFF15CBF8FF3DE3F9FF00C0F1FF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A141855377388C80001
      0116000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AEE4F3FF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF3A72A4E80000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000CCFFFF59EBFFFF41DD
      FEFFCFF5FFFFF3FDFFFFEDFBFFFF4AD9FCFF48D8FCFFECFBFFFFF2FCFFFFCAF4
      FEFF28D5FAFF40E4FAFF00C1F3FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABD5
      E4FF56A4D8FF84B0DBFF449CD0FF05141C5E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000ADD8EB24D9FFFF5BEC
      FFFF56E1FFFFCFF5FFFF50DCFEFF2DD4FDFF2AD3FDFF49D9FCFFCCF4FEFF46DB
      FBFF47E5FBFF1BD3F9FF00A6D0EB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000ADD8EB24D9
      FFFF5BECFFFF41DDFFFF32D6FEFF30D5FEFF2ED4FDFF2BD3FDFF36D9FCFF4DE7
      FCFF1DD4FAFF00A7D2EB00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000AD
      D8EB24D9FFFF5AEBFFFF59EBFFFF58EAFEFF56EAFEFF54E9FEFF52E9FEFF20D5
      FCFF00B5E1F20000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000AFDAEC00CCFFFF00CCFFFF00CCFFFF00CAFDFF00CAFDFF00CAFDFF00AD
      D8EC000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000020000000360000003600000036000000360000
      00360000003600000036000000330000001D0000000000000000000000000000
      0003000000010000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000030303030D0E0000
      000000000000000000000000000001000C0E0000030300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000081B2C4A19507BCF1E6197FF1E61
      97FF1E6197FF1E6196FF174C76FFFCFCFCFFFCFCFCFFFFFFFFFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFEDEDEDF3000000330000000000000000050B0514265A
      2AAD132F145E0000000100000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000303161545493937BDC80B0B
      2C2F000000000000000007062A2F2222B5C70B0A424900000303000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000CEEC0000F1FF0000F1FF0000F1FF0000EFFF0000EFFF0000EDFF0000
      CCED00000000000000000000000000000000184B73C260A4D7FF63A7DAFF62A5
      D9FF60A3D8FF60A1D7FF4B7EA9FFFCFCFCFF0D7D00FF0D7D00FF0D7D00FF0D7D
      00FF0D7D00FF0D7D00FFFCFCFCFF0000003600000000060C061438763EC260B2
      69FF4C9B53FF0F27115300000001000000000000000000000000000000000000
      000000000000000000000000000000000000060612144845C5CD6260FAFF4341
      D3DF0C0C303309092F333331CFDF4D4BF7FF2827BCCD02021214000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      D2EC181EF5FF3A4AF9FF3847F8FF3645F8FF3343F8FF3241F7FF3040F7FF1219
      F1FF0000C4E80000000000000000000000001E6197FF66AADCFF468DCFFF448A
      CEFF4186CDFF4084CBFF3268A0FFFCFCFCFF0D7D00FF0D7D00FF0D7D00FF0D7D
      00FF0D7D00FF0D7D00FFFCFCFCFF0000003609130A1C42814ABE6EC079FF72C4
      7DFF70C37AFF499550F316331763000000010000000000000000000000000000
      0000000000000000000000000000000000000101020215153D3F5554DEE56664
      FBFF4D4BE0EA4745DFEB5957F9FF3C3AD6E40C0B3A3F00000202000000000000
      00000000000000000000000000000000000000000000000000000000D4EC1B21
      F9FF4251FAFF2227F9FF1111F7FF0E0EF6FF0B0BF5FF0808F5FF1419F5FF3141
      F7FF1219F1FF0000C4E800000000000000001E6197FF67ADDCFF4892D1FF468E
      D0FF448ACEFF4388CCFF346BA1FFFCFCFCFFFCFCFCFFFFFFFFFFFCFCFCFFFAFA
      FAFFF9F9F9FFFFFFFFFFFCFCFCFF0000003629542E776BBE77FB6FBF7AFE3D78
      44B4569F5FE473C57DFF4A9552F3102812530000000100000000000000000000
      0000000000000000000000000000000000000000000001010202181743455D5C
      E9EE6866FFFF6462FFFF4F4DE5EE0F0F40450000020200000000000000000000
      000000000000000000000000000000000000000000000000D4EC1D23FAFF4856
      FBFF4045FBFFC9C9FDFF3939F9FF1212F7FF0F0FF6FF3131F7FFC4C4FDFF2E33
      F7FF3242F7FF1219F2FF0000C4E8000000001E6197FF69B0DEFF4B96D3FF4992
      D2FF468ED0FF468CCEFF376EA2FFFCFCFCFFFFFFFFFFFCFCFCFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFCFCFCFF00000036040A050E3364388C336039850206
      02091023123957A161E575C781FF52A05AFE1532175F00000001000000000000
      000000000000000000000000000000000000000000000000000013133233615E
      E6EB706CFFFF6B69FFFF504EE1EB0D0C30330000000000000000000000000000
      000000000000000000000000000000000000000000000000FBFF4D5BFDFF3035
      FBFFCBCBFEFFF2F2FFFFEBEBFEFF3939F9FF3737F8FFEAEAFEFFF1F1FEFFC4C4
      FDFF161BF6FF3141F7FF0000EFFF000000001E6197FF6BB2DFFF4E9BD5FF4C97
      D3FF4993D1FF4891D0FF3872A4FFFCFCFCFFFCFCFCFFFFFFFFFFF8F8F8FFF6F6
      F6FFF3F3F3FFEFEFEFFFFCFCFCFF0000003600000000040A050E0409040D0000
      00000000000018331B51589D60DD77C883FF4D9755EF132D1555000000000000
      0000000000000000000000000000000000000000000014142F2F6360DCDF7A77
      FDFF6765EAEE615FE9EE6D6AFBFF4A47D5DF0D0D2C2F00000000000000000000
      000000000000000000000000000000000000000000000000FDFF505DFDFF2626
      FCFF4545FCFFECECFFFFF2F2FFFFECECFFFFECECFEFFF1F1FFFFEAEAFEFF3232
      F7FF0A0AF5FF3343F8FF0000EFFF000000001E6197FF6EB4E0FF509ED7FF4E9B
      D6FF4C97D4FF4B95D2FF3B75A5FFFCFCFCFF0D7D00FF0D7D00FF0D7D00FF0D7D
      00FF0D7D00FF0D7D00FFFCFCFCFF000000360000000000000000000000000000
      00000000000000000000152D17465BA365E45FAA69EE1D40226D000000000000
      000000000000000000000000000000000000020207075A58C4C4817DFEFF6967
      E2E41B1B4345191843455C59DEE4716FFCFF4644C4CD06061416000000000000
      000000000000000000000000000000000000000000000000FDFF5360FEFF2A2A
      FDFF2727FCFF4646FCFFEDEDFFFFF2F2FFFFF2F2FFFFECECFEFF3838F9FF1111
      F7FF0E0EF6FF3646F8FF0000F1FF000000001E6197FF71B6E1FF55A2D7FF519F
      D7FF4E9CD5FF4E9AD4FF3D79A7FFFCFCFCFF0D7D00FF0D7D00FF0D7D00FF0D7D
      00FF0D7D00FF0D7D00FFFCFCFCFF000000360000000000000000000000000000
      0000000000000000000001010102162D18461B371E5901030106000000000000
      000000000000000000000000000000000000000001011B1A3A3A5F5DC9C91C1B
      3F3F010102020101020217163D3F5554CED32E2C7E8403030E0F000000000000
      000000000000000000000000000000000000000000000000FDFF5562FEFF2E2E
      FDFF2B2BFDFF4949FCFFEDEDFFFFF2F2FFFFF2F2FFFFECECFFFF3B3BF9FF1414
      F8FF1212F7FF3A49F8FF0000F1FF000000001E6197FF74B8E2FF5AA6D9FF56A3
      D8FF519FD7FF509DD6FF3F7BA8FFFCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFCFCFCFFF6F6F6FF56565691000000200000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000010105050E0E0101
      02020000000000000000010102020C0B1F200606141500000000000000000000
      000000000000000000000000000000000000000000000000FFFF5865FEFF3131
      FEFF4E4EFDFFEDEDFFFFF3F3FFFFEDEDFFFFEDEDFFFFF2F2FFFFECECFEFF3C3C
      FAFF1515F8FF3D4CF9FF0000F1FF000000001E6197FF78BAE3FF5FA9DBFF58A4
      D9FF519FD7FF509ED7FF3F7CA9FFF6F8F9FFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFF8F8F8FF5454549100000020000000020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FFFF5966FFFF4145
      FEFFCFCFFFFFF3F3FFFFEDEDFFFF4A4AFCFF4848FCFFECECFFFFF2F2FFFFCACA
      FEFF282DFAFF404FFAFF0000F3FF000000001E6197FF7ABCE4FF63ADDDFF60AA
      DCFF5CA7DAFF5AA6D9FF4F92BEFF4783ABFF4783AAFF3F7BA8FF4B7EA9FF174C
      76FF000000360000002000000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000D8EB2429FFFF5B68
      FFFF5659FFFFCFCFFFFF5050FEFF2D2DFDFF2A2AFDFF4949FCFFCCCCFEFF464A
      FBFF4755FBFF1B21F9FF0000D0EB000000001E6197FF7DBEE4FF67B1DEFF489A
      DAFF4296DCFF4195DCFF4095DCFF4094DCFF3F93DAFF4F9CD5FF6AB0DDFF1E61
      96FF000000010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000D8EB2429
      FFFF5B68FFFF4145FFFF3232FEFF3030FEFF2E2EFDFF2B2BFDFF363AFCFF4D5B
      FCFF1D23FAFF0000D2EB00000000000000001B5585E06FB1DAFE7CBEE4FF4C9C
      DFFFB4EEFDFF73D4F0FF73D4F0FFB4EEFDFF499ADEFF6CB3E0FF69AED9F91D5D
      90F3000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      D8EB2429FFFF5A67FFFF5966FFFF5865FEFF5663FEFF5461FEFF525FFEFF2025
      FCFF0000E1F200000000000000000000000006121E32164368B21E6197FF3573
      A3FFB5EFFEFF7EDBF3FF7EDBF3FFB5EFFEFF2C6CA0FF1E6197FF133E61A50A1F
      3153000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000DAEC0000FFFF0000FFFF0000FFFF0000FDFF0000FDFF0000FDFF0000
      D8EC000000000000000000000000000000000000000000000000040F192A1E61
      97FF1E6197FF1E6197FF1E6197FF1E6197FF1D5B8EF000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000024000000520000
      0052000000520000005200000052000000520000005200000052000000290000
      0001000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000704024C1A05E36E6C67343FEB5693DF3B6693DF3B569
      3DF3B5693DF3B7693EF4A25F37E8B47048F10000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000052FDFDFCFFFBFB
      FBFFFCFCFCFFFDFDFDFFFDFDFDFFFEFEFEFFFDFDFDFFF7F7F7FE888888CA0000
      002E000000010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AC673EEDFCF3ECFFFAF1E8FFFAF0E7FFFBF1E9FFFBF2
      EAFFFBF2EAFFFBF2EBFFFDF4EEFFC0784EF92B73A8FF296FA4FF286CA0FF2669
      9CFF246598FF226294FF205F91FF1E5C8DFF1D5A8AFF1C5888FF1B5686FF1B56
      86FF1B5686FF1B5686FF1B5686FF1B5686FF0000000000000052FAFAF9FFF7F7
      F7FFFAFAFAFFFBFBFBFFFCFCFCFFFEFEFEFFFCFCFCFFF1F1F1FFD4D4D4FF9A9A
      9AE00000002F0000000100000000000000000000000000000000000000000000
      000000000000000000800000001C0000000000000000000000060000008F0000
      0005000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CF8151FFEFF1E7FFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFEFF2E8FFCE8054FF2D76ACFF5198C7FF4D93C3FF4B90
      C2FF4B8EC0FF4B8EC0FFC0E9EAFF76CDDFFF74CDDFFFABE1E7FF4783BAFF4580
      B9FF437DB7FF437CB7FF467EB8FF1B5686FF0000000000000052F9F9F9FFF7F7
      F7FFFAFAFAFFF7F7F7FFEFEFEFFFF8F8F8FFF9F9F9FFEEEEEEFFC9C9C9FFF5F5
      F5FF9A9A9AE00000002E00000001000000000000000000000000000000000000
      000000000030000000FD0000009300000000000000000000005B000000FE0000
      004E000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C67E4EFBFBF5EEFFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFFBF6EFFFCA8053FE2F79AFFF5099C7FF3982B4FF296F
      A4FF286CA0FF26699CFF246598FF226294FF205F91FF1E5C8DFF1D5A8AFF1C58
      88FF1B5686FF2B669AFF447DB7FF1B5686FF0000000000000052F9F9F9FFF6F6
      F6FFF9F9F9FFFBFBFBFF9D9D9DFFAEAEAEFFA5A5A5FFCACACAFFEBEBEBFFD4D4
      D4FFDBDBDBFF878787C900000029000000000000000000000000000000000000
      00000000009C000000FE000000F50000001E00000000000000C7000000FE0000
      00B000000000000000000000000000000000683E26B9834E30CE965732DD894F
      2FD48A4F2FD4894F2FD4CA8350FFFFF7F1FFC56C29FFC56C29FFFFE9D9FFC56C
      29FFC56C29FFC56C29FFFFF7F1FFC98253FE317CB3FF519CC9FF2D76ACFF76E4
      A1FF74E39FFF72E39EFF70E29CFF6EE29BFF6CE29AFF6BE199FF69E198FF67E1
      96FF66E095FF1B5686FF447EB8FF1B5686FF0000000000000052F2F2F1FFF1F1
      F0FFF7F7F7FFFAFAFAFF7D7D7DFFE3E3E3FFF1F1F1FF989898FFF8F8F8FFEAEA
      EAFFEEEEEEFFF8F8F8FE00000052000000000000000000000000000000000000
      0017000000F7000000FD0000006B0000009B00000038000000FE000000FA0000
      00BA0000001A000000000000000000000000885434D3BEB8B2DEBDB6B0DEBDB6
      AFDEBEB6B0DEBEB7B1DEE4B990FFFFF7F0FFFFE7D5FFFDE7D6FFFDE6D4FFFCE4
      D0FFFBE3CBFFFADCC1FFFEF3E8FFCA8354FE327EB6FF52A0CBFF2F79AFFF55DD
      8AFF54DC88FF78E4A2FF76E4A1FF74E39FFF4EDA81FF4DDA7EFF4BD97CFF4AD9
      7AFF49D879FF1C5888FF4582B9FF1B5686FF0000000000000052ECECECFFD7D7
      D7FF545454FFC5C5C5FF828282FFCACACAFFADADADFF878787FFBBBBBBFFFAFA
      FAFF565656FFD0D0D0FF00000052000000000000000000000000000000000000
      007D000000FE000000CB00000000000000AC000000C6000000FE000000AC0000
      00410000007D0000000000000000000000009C613DDEB5B6AFDEE0AF83FFE0AF
      83FFC1B0A4DEE0AF83FFE4BA90FFFFF7F2FFFEE7D5FFFEE7D5FFFDE5D1FFFAE0
      CAFFF9DEC3FFF7D9BBFFFDF2E7FFCA8455FE3381B8FF55A4CDFF317CB3FF69C9
      5AFF3BBA2FFF4ED572FF46CA58FF23CD58FF53BF46FFA8BD57FFE4A73AFFFC9F
      2EFFFF9B2CFF1D5A8AFF4886BCFF1B5686FF0000000000000052EAEAE9FFF3F3
      F3FF717171FFF8F8F8FF808080FFAEAEAEFFA2A2A2FFE4E4E4FFB3B3B3FF9B9B
      9BFFBCBCBCFFFEFEFEFF00000052000000000000000000000000000000050000
      00E4000000FE0000005F0000000000000048000000FE000000FE0000003F0000
      0001000000C0000000030000000000000000965F3BDBBEB9B4DEC1B0A4DEC1B1
      A6DEC1B0A4DEC1AFA3DEE4BA91FFFEF7F1FFFCE5D2FFFCE4D1FFFBE2CCFFF9DD
      C3FFF6D7BAFFF3D1AEFFFAEFE4FFCA8456FE3483BAFF58A8CFFF327EB6FF6ECB
      5EFF6CCB5DFF3AB926FF3CB92BFFD9BF63FFFFBA62FFFFB960FFFFB85EFFFFB7
      5DFFFFB65BFF1E5C8DFF4A8ABEFF1C5888FF0000000000000052E8E8E8FFF2F2
      F1FF717170FFF6F6F6FF727272FFDDDDDDFFE0E0E0FF9F9F9FFFD0D0D0FF6161
      61FFFCFCFCFFFCFCFCFF00000052000000000000000000000000000000580000
      00FE000000E800000007000000000000008D000000FE000000CF000000000000
      00000000007C00000051000000000000000094603BDBC1BBB6DEC56C29FFC56C
      29FFC1B0A4DEC56C29FFE4BA91FFFEF6F0FFFCE2CDFFFCE3CDFFFADFC8FFF7D9
      BBFFF5E9DDFFFAF3EBFFFBF8F3FFC88051FE3483BAFF58AAD1FF3381B8FF72CE
      62FF3DBD29FF3DBC29FFEFC169FFFFBF68FFFFBD66FFFFBC65FFE4C486FF98C8
      AEFF61D9EBFF205F91FF4B8DBFFF1D5A8AFF0000000000000052AFAFAFFFF0F0
      EFFF707070FFF4F4F4FF797979FFB0B0AFFFB1B1B1FFDDDDDDFF6A6A6AFFB8B8
      B8FFDDDDDDFFFCFCFCFF00000052000000000000000000000000000000C40000
      00FE000000830000000000000009000000EC000000FE000000BC000000000000
      00000000001B000000B6000000000000000095603BDBC1BBB6DEC1AFA1DEBFAF
      A2DEBFAEA1DEBEAC9DDEE4BA92FFFEF5EDFFFCDEC4FFFBE0C7FFF9DCC1FFF5D3
      B3FFFEF9F3FFFAE2C3FFECC092FF402817933483BAFF58ACD1FF3483BAFF94BD
      31FF93BC30FFD8C86BFFFFC36EFFFFC26CFFFFC16BFFFFC069FF9ECCB4FF5EDE
      F8FF85E5F9FF226294FF4B8FC1FF1E5C8DFF0000000000000052B1B1B1FF8C8C
      8BFF707070FF949494FF949494FFECECECFFEFEFEEFF969696FF8D8D8DFFF6F6
      F6FF747474FFE7E7E7FF00000052000000000000000000000033000000FD0000
      00FA0000001C0000000000000060000000FE000000E70000009C000000380000
      000000000000000000B0000000230000000095613CDBC1BBB7DEE0AF83FFE0AF
      83FFBFAD9EDEE0AF83FFE5BD95FFFFFFFEFFFDF3E9FFFDF3EAFFFCF2E8FFFAEF
      E3FFFAF2E7FFEABA87FF664129B30000000C3483BAFF58ADD3FF3483BAFFFFC8
      74FFFFC773FFFFC772FFFFC571FFFFC470FFFFC36FFFFFC36EFF6EDEEEFF8DE7
      FAFF8BE7FAFF246598FF4C92C2FF205F91FF0000000000000052EAEAEAFFECEC
      EBFFEEEEEDFFEAEAE9FFF2F2F1FFF3F3F2FFF4F4F4FFF5F5F5FFF6F6F6FFF7F7
      F6FFEFEFEFFFFAFAFAFF000000520000000000000000000000A0000000FE0000
      00B10000000000000000000000CD000000FE0000008800000020000000B30000
      0000000000000000004F000000840000000095613DDBC0BBB6DEBEAD9FDEBEAC
      9EDEBEAB9BDEBDA794DEEAC29CFFE6BE95FFE4BA91FFE4BA91FFC19362F5C292
      63F6956C44DA2F1D0F7E00000009000000003483BAFF59B0D4FF4294C3FF3483
      BAFF3483BAFF3381B8FF327EB6FF317CB3FF2F79AFFF2D76ACFF2B73A8FF296F
      A4FF286CA0FF367BADFF4E96C4FF226294FF0000000000000052EAEAE9FFE9E9
      E8FFE8E8E7FFE6E6E5FFEFEFEFFFF1F1F0FFF2F2F1FFF3F3F2FFF3F3F3FFEEEE
      EEFFE9E9E9FFF9F9F9FF000000520000000000000031000000F8000000FE0000
      00730000000400000062000000FE000000FE000000420000000D000000CF0000
      006F0000000D0000002D000000E40000002195613DDBC0BAB6DEBEAB9BDEBEAC
      9BDEBDA997DEBBA48EDEB9B0A7DEBDB8B2DEBEBCB8DE986E49DC000000000000
      0000000000000000000000000000000000003483BAFF5DB3D6FF5AB0D4FF59AF
      D4FF58AED3FF5AADD3FF5AADD2FF5AAAD1FF59A9D0FF57A7CFFF55A3CDFF52A0
      CBFF519EC9FF519CC9FF539CC8FF246598FF0000000000000052F0EFEFFFE4E4
      E3FFE7E6E5FFEAEAE9FFEDEDECFFEEEEEDFFEFEFEEFFF0F0EFFFF0F0EFFFEEEE
      EEFFEEEEEEFFF8F8F8FF00000052000000000000005D00000074000000710000
      00790000002B000000760000006B0000006F0000007B0000004E0000007D0000
      00730000003E00000061000000740000005095613EDAC0B9B3DEBEA895DEBEA9
      96DEBDA792DEB9A088DEC0BDB8DEBDAB94DEB2916EDE301F1280000000000000
      0000000000000000000000000000000000003483BAFF3483BAFF3483BAFF3483
      BAFF3483BAFF3483BAFF3483BAFF3381B8FF327EB6FF317CB3FF2F79AFFF2D76
      ACFF2B73A8FF296FA4FF286CA0FF26699CFF0000000000000052F9F8F8FFF2F2
      F2FFF4F3F3FFF4F4F4FFF5F5F5FFF6F6F6FFF7F7F6FFF7F7F7FFF7F7F7FFF7F7
      F7FFF7F7F7FFFBFBFBFF00000052000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000865737D0BCBCBCDBBFB8B0DEBFB8
      B1DEBEB7B0DEBDB5ACDEBDB7AFDEB18D66DE4D311F9C0000000A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001F000000520000
      0052000000520000005200000052000000520000005200000052000000520000
      0052000000520000005200000024000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000058351FAA855636CE9B6641DE9563
      3EDB96633EDB95623FDB865634D024160C6E0000000800000000000000000000
      0000000000000000000000000000000000000000000000000000112012314781
      4ACC182D194B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000040507082F445D6E4F6F
      97B35984B5DD5689BFF44C84BBFD3D79B2FF2D6EA7FF1F639CFD12578EF40A4B
      7BDD073B63B305243D6E00030408000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000001030105457446AF5DA4
      62FD69AD6DFF49944DFB1020103409140B220000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000013191F36354B5EA7436381E1496E91F6496E91F6436381E1354B5FA91319
      1F3700000000000000000000000000000000000000005578A5C486B6E0FF94C5
      E6FF9ACDE9FF9BCDE9FF98C7E7FF94C0E4FF8DB7E0FF88AFDDFF80A6D7FF719A
      CFFF5B8BC1FF3C76ADFF09416DC4000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000101B112779B87EFCB6DE
      BAFF65AB6AFF73B578FF4C964FFE3A703DB82449277A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000002C3B
      497D4A7092F35896B0FF84BCCEFFA3D6DEFF9FD5DEFF79B8CBFF4D8BAAFF486B
      8DF42C3B4882000000000000000000000000000000005578A5C497B8D4FFC4C7
      C3FFECD8C7FFF6EBE3FFFAF5F1FFF3E7DEFFDFBEA5FFD0A37EFFCB9972FFC896
      6FFFA2928BFF607D9BFF09416DC4000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000466E499B9BCD9FFF6DB1
      71FF8CC791FFA9DCAEFF74B579FF4F9A53FF75B679FF438446DD122713420A17
      0C290000000000000000000000000000000000000000000000002D3C48785387
      A5FF95C8D6FFCCCABEFFC89E85FFC49174FFC38F71FFC3997EFFC2C3B8FF7BBC
      CDFF457597FF2D3B4780000000000000000000000000231F1B2AE6C2A4FFEFD6
      BFFFFBF2EAFFFEFCFAFFFFFEFDFFFEFDFCFFFCF6F0FFF8E8D9FFF6E3D1FFF6E2
      CEFFE6C8ACFFCE9F79FF1B16112A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B130D1979BA7EFF75B67AFF90CB
      96FFAADEB0FF9BD7A1FFA9DDAFFF75B67AFF5EAB63FFADD8B1FF8AC390FF356D
      39B927542B910913092200000000000000000000000013191D30527C9BF6A1CC
      D8FFCDB6A4FFD8A988FFEFDFCFFFFAF6F3FFFAF5F3FFEFDFD1FFD7A684FFC2AB
      97FF81BCCEFF496B8EF713181E35000000000000000000000000A88C73B9EDD3
      BAFFFAF0E5FFFDF9F5FFFEFCFAFFFEFDFCFFFDF9F4FFF6E3CFFFF7E5D3FFF6E3
      D1FFDDB899FF916D50B800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004E7652A07DBC83FF96CE9BFFACDF
      B2FF6DB272FF95D59CFF9CD8A2FFA9DDAFFF76B77AFF70B974FFC2E7C8FFB1DA
      B4FF64B269F8549D57FF1E411F730C1C0C32000000003952639F76A8BFFFDAD1
      C5FFD9A987FFFFFEEBFFFFFAEEFFFFFDF2FFFFFEF2FFFFFCEFFFFFFDEBFFD9A2
      78FFCBC5B8FF5693B0FF374D5FAA00000000000000000000000000000000A086
      70B0E8C7AAFEF7EADEFFFEFAF7FFFEFCF9FFFDF7F2FFFAEDE2FFEBD0B6FFD0A0
      7CFE8D6C51AE0000000000000000000000000000000000000000000000000000
      0000000000000410051A174B1C8023762AD5216E28D5113916800209031A0000
      00000000000000000000000000000000000079BA7EFF8DC892FFAEDFB4FFA0DA
      A6FF97D79EFF96D69DFF7CBF82FF81C086FFAADDAFFF77B87BFF61AD65FFC3E7
      C8FFC0E4C3FFB8E0BDFF4D9951FF040A041100000000507B96DEB6D3DDFFD1A9
      8FFFF0DBC0FFFFF7E6FFFAF2E3FFC4BFB5FFF1EDDEFFFFFCE8FFFFF5DFFFEFD4
      AEFFC99A79FF91C1D0FF456683E2000000000000000000000000000000000000
      0000352D263ACDB39AE1F3E2D2FFFCF4EDFFFBF1E7FFE9CCB4FFBA9474DC2F24
      1C38000000000000000000000000000000000000000000000000000000000000
      0000000000001A531D8090CB94FFA0D4A2FF98D09AFF8BC18FFF123B17800000
      0000000000000000000000000000000000004563487D7BBA81FF8EC893FFAFE0
      B5FFA1DAA7FF7DC084FFA3D0A6FFDDEEDFFF7EB782FFAADEB0FF78B87DFF549E
      58FFC3E7C8FF76B77AFF274F298400000000000000006294B1F7DDEAECFFCF9E
      80FFFAF3E2FFFDF6E9FFF2F0E3FF8C8B83FF807D75FFD2CFC1FFFFFBEBFFFAEC
      CCFFC78F6BFFC0DDE1FF4E7395F6000000000000000000000000000000000000
      00000000000000000000B59982C7F4E3D0FFF3DFCDFFA9896FC3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002C9232D5B3DCB4FF89CC8AFF82CA84FF99D09CFF22712AD50000
      00000000000000000000000000000000000000000000141D152579B77EFA8FC9
      94FFAFE0B5FF84C189FFF7FCF8FF94C196FFDDEEDFFF81C186FFAADEB0FF79B9
      7DFF569F5AFF57A05BFF010201040000000000000000679BB6F7E4ECEEFFD1A4
      89FFFAF6E9FFFDF8EEFFFFFFF9FF807E77FFB0ADA3FF807D75FFC9C5B9FFFAF0
      D3FFC99470FFC9DFE2FF4F7597F6000000000000000000000000000000000000
      00000000000000000000B3967EC3F4E1CEFFF3DFCCFFAB8B71C2000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002E9733D5BFE1BFFF94D294FF87CC88FFA1D4A3FF257B2CD50000
      00000000000000000000000000000000000000000000000000000000000078B5
      7DF690CA95FFB0E0B5FFD9F3DDFFF7FCF8FFA3D0A6FF7CBF83FF9ED9A4FFABDE
      B1FF79BA7EFF539B57FB0000000000000000000000006192ABE7D3E1E6FFDBB8
      A3FFF1E3D6FFFEFBF5FFF5F5F3FF9E9D94FFF0F0E9FFF8F8F2FFBDB8ACFFF0DA
      C1FFD1A68AFFA8CAD3FF476A87DF000000000000000000000000000000000000
      0000342D2638D0B399DDF3DDC9FFFAEEE2FFF9EBDEFFEDD0B5FFC19E80DB3128
      2038000000000000000000000000000000000000000000000000000000000000
      0000000000001D5D208096D69AFFC0E2C1FFB9DFBAFF91CC94FF184E1C800000
      0000000000000000000000000000000000000000000000000000000000000000
      000078B57FF591CB96FFB0E1B5FF84C289FF7EC084FF98D79FFF97D79EFF9ED9
      A4FFABDFB1FF7BBA80FF539957F60000000000000000537284B6B6CFDBFFEDE0
      D7FFDEBCA9FFFFFFFFFFF0F0EEFFE7E5DAFFFFFFF7FFFFFFF7FFFFFEF3FFDCB3
      97FFE1D1C3FF71A3BAFF384E61A300000000000000000000000000000000A892
      7EAFF4D5B7FEF9ECE0FFFDF8F4FFFBF0E6FFF8E7D7FFF9EBDEFFF2DAC4FFE1B7
      95FE987B64AE0000000000000000000000000000000000000000000000000000
      0000000000000512051A1D5D20802E9833D52D9433D51B561E800411051A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007BB680F592CC97FFB0E1B6FFA2DBA8FF9AD8A1FF71B375FFAEDF
      B3FF86C28BFF63A967FF0409040E000000000000000020262B3C70AFC8FADFE9
      ECFFE6CFC1FFDFBEADFFF1E3DCFFF3F1ECFFF4F1EBFFF1E2D9FFDCB9A2FFDEC0
      AEFFB6CFD8FF537D9CF5131A1E31000000000000000000000000B39E87B8F9E3
      CDFFFBF3ECFFFEFAF7FFFEFBF8FFFCF5EDFFF7E6D5FFF6E1CCFFF9ECDFFFF9EB
      DDFFEBCEB3FFA1836AB800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007CB881F593CC98FFB1E2B6FFA2DCA9FFAFE0B5FF8BC5
      91FF6CB071FF0509050E000000000000000000000000000000004B5F6D9196CC
      DCFFE1EBEEFFEEE1D9FFDFBDABFFD5AC95FFD4A991FFDCB6A2FFE8DACEFFC5D9
      DFFF669AB5FF2C3D487500000000000000000000000025231F2ABFC4CDFF99B6
      DCFF7BA5D5FF6194CAFF4D86BDFF3D79B2FF2D6EA7FF21659EFF1C6197FF2A69
      99FF5182A6FF879399FF1E1B182A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007DB882F593CD99FFB2E2B6FF92CB97FF75B6
      7AFF050A050E0000000000000000000000000000000000000000000000004E63
      729673B4CDF9C8D9E3FFDDE7EAFFEEF1F1FFECF0F0FFD2E0E4FFA7C3D2FF6194
      AFF43346527D000000000000000000000000000000006285AFD186B6E0FF94C5
      E6FF9ACDE9FF9BCDE9FF98C7E7FF94C0E4FF8DB7E0FF88AFDDFF80A6D7FF719A
      CFFF5B8BC1FF3C76ADFF134B75D1000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007EB984F595CD9AFF7EBD84FF070A
      070E000000000000000000000000000000000000000000000000000000000000
      0000232B30425C7E92BD6EA5BFED77B3CCFA76AFC9FA6599B1E84D6B7FB01A22
      263900000000000000000000000000000000000000005578A5C486B6E0FF94C5
      E6FF9CCFEAFFA0D3ECFF9FD1EBFF9ACAE8FF95C1E5FF8EB9E2FF86B0DDFF77A2
      D4FF5F8FC3FF3C76ADFF09416DC4000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080BA85F5070A070E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000040507082F445D6E4F6F
      97B36089BADD6A97CDF46E9DD5FD6798D1FF5A8FC8FF4C84BBFD3B74AAF4275F
      91DD15466EB30827406E00030408000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000349CD9FF2F98D8FF2A93
      D7FF268FD6FF218BD5FF1C87D4FF1883D3FF137ED2FF107AD1FF0D77D1FF0974
      D0FF0671CFFF036ECFFF006CCEFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D97034FFD86E2FFFD76D
      2AFFD66C26FFD56A21FFD4681CFFD36718FFD26513FFD16410FFD1640DFFD062
      09FFCF6106FFCF6103FFCE5F00FF00000000000000003BA2DAFFBBEBFAFFBBEB
      FCFFBEEEFEFFC5F4FFFFCEF8FFFFD3FAFFFFD0F8FFFFC7F2FFFFB9E9FCFFB2E4
      F9FFAFE2F8FFAFE2F8FF046FCFFF0000000086427EFF823D7CFF7D367BFF7A31
      7AFF772C79FFC53812FFC53511FFC32F11FFBF2C10FFBF280FFFBE250EFF7A4D
      23FF78481FFF75441CFF734019FF713D17FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      020302080F1A00000000000000000000000000000000DA723BFFFACBBBFFFCCD
      BBFFFECFBEFFFFD1C5FFFFD5CEFFFFD8D3FFFFD7D0FFFFD4C7FFFCCDB9FFF9C8
      B2FFF8C5AFFFF8C5AFFFCF6104FF000000000000000041A7DBFFBEECFBFF57CF
      F5FF3FAFECFF4CB9EFFF58C1EFFF5EC5EFFF5AC3EFFF4AB5EFFF35A4E6FF2899
      E1FF36B7EEFFB0E3F8FF0873D0FF000000003F3BF3FF989AFEFF9597FEFF9294
      FEFF9092FEFFC94113FFE0925BFFDF8E57FFDF8C53FFDE894FFFBF2C10FFA1C9
      9DFF9CC598FF97C294FF93BF90FF256B2AFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010104061241
      68AD20659CFF082F4C87000000000000000000000000DB7441FFFBCEBEFFF57D
      57FFEC7B3FFFEF824CFFEF8658FFEF885EFFEF865AFFEF844AFFE67635FFE16F
      28FFEE6C36FFF8C5B0FFD06208FF000000000000000047ACDCFFC0EEFBFF5DD3
      F7FF6ADBFCFF7DE5FFFF8EEDFFFF96F2FFFF92EDFFFF7ADFFFFF59CCF8FF44BD
      EFFF3AB9EEFFB2E3F9FF0D77D1FF000000004845F5FF9D9FFEFF595CFEFF5457
      FEFF9496FEFFCD4A14FFE19661FFDA7E3FFFD97B39FFDF8E56FFC53311FFA7CD
      A3FF75AF6FFF6EAA68FF99C395FF2C7431FF0000000000000000000000000000
      00000000000000000000000000000000000000000000010304061B486FAD538C
      BBFF88B4DDFF165D96FF000000000000000000000000DC7647FFFBCEC0FFF782
      5DFFFC8C6AFFFF987DFFFFA08EFFFFA396FFFFA492FFFF9B7AFFF88659FFEF75
      44FFEE6E3AFFF9C9B2FFD1640DFF00000000000000004CB1DDFFC2EFFBFF63D6
      F8FF4AB5ECFF58BCEFFF94EBFFFF2E96DDFF4B81AAFF83E1FFFF3FA8E9FF309E
      E1FF40BDEFFFB3E5F9FF127CD2FF00000000514FF6FFA1A3FEFF6064FEFF5B5F
      FEFF989AFEFFCF5316FFE39B6AFFDC8548FFDB8242FFE0935CFFC53A12FFAED1
      AAFF7EB678FF77B171FF9FC89BFF337E3AFF0000000000000000000000000000
      00000000000000000000000000000000000001030406275076AD6296C4FF9CC0
      E4FF6498C7FF1B5B90EB000000000000000000000000DD774CFFFBCFC2FFF886
      63FFEC814AFFEF8B58FFFFA894FFDD742EFFAA744BFFFFA183FFE9803FFFE172
      30FFEF7140FFF9C8B3FFD26512FF000000000000000051B6DEFFC5F0FCFF68D9
      F8FF7AE2FDFF8FE8FFFF98E9FFFF309EDFFF528AB1FF89E2FFFF68D0F9FF4EC4
      F1FF44C0F0FFB5E7F9FF1682D3FF000000005959F8FFA7A9FEFF696DFEFF6367
      FEFF9D9FFEFFD15E18FFE5A172FFDE8B51FFDD884BFFE29864FFC94313FFB4D5
      AFFF89BC82FF82B77BFFA6CDA2FF3B8A42FF0000000000000000020101027F66
      5197BF9676E3D1A37CFDCE9E78FDB78A68E48C7F77DE73A1CCFFAACBE8FF74A3
      CEFF2B659AEB050F1827000000000000000000000000DE7851FFFCD2C5FFF888
      68FFFD967AFFFFA68FFFFFAE98FFDF7030FFB17952FFFFA689FFF99268FFF17A
      4EFFF07344FFF9C8B5FFD36616FF000000000000000056BADFFFC7F1FCFF6DDC
      F9FF54BAEDFF5FBCEFFF9AE7FFFF33A5E2FF49A3E1FF8FE2FFFF47ACE9FF36A3
      E3FF47C3F0FFB7E8F9FF1C87D4FF000000006162FAFFABAEFEFF7074FEFF6B6F
      FEFFA1A3FEFFD6671CFFE7A679FFE0915AFFDF8E54FFE49D6CFFCD4C14FFB9D9
      B4FFB6D6B0FFB2D3ADFFADD1A8FF43954BFF0000000002010102A88A70C0E8C9
      ADFFF5E1CDFFF7E5D3FFF7E5D1FFF3DDC8FFDFB99BFFC7A790FF85ADD5FF3A71
      A6EB08111B2700000000000000000000000000000000DF7A56FFFCD2C7FFF98B
      6DFFED8754FFEF925FFFFFB29AFFE26F33FFE18749FFFFAC8FFFE98447FFE375
      36FFF07347FFF9C9B7FFD4681CFF00000000000000005ABEE0FFC8F3FCFF73DF
      F9FF88E6FDFF94E7FFFF99E5FFFFA9EEFFFFA7EDFFFF98E3FFFF72D5F9FF57CC
      F3FF4DC8F1FFBAE9FAFF228CD5FF00000000686AFCFFAFB2FEFF777CFEFF7377
      FEFFA7A9FEFFD87228FFE8AC82FFE29763FFE1945EFFE5A274FFCF5516FF5EBB
      6AFF5AB565FF55AF5FFF50A75AFF4BA053FF0000000088735E97EDD0B6FFF8E8
      D9FFF5DEC8FFF3D8BCFFF3D6BAFFF4DBC1FFF7E4D2FFDFBA9CFF978E8CF70A14
      1D270000000000000000000000000000000000000000E07B5AFFFCD1C8FFF98E
      73FFFD9F88FFFFAC94FFFFB399FFFFBAA9FFFFB9A7FFFFB498FFF99772FFF37E
      57FFF1764DFFFACCBAFFD56A22FF00000000000000005EC1E1FFC9F3FCFFCBF3
      FDFFD4F6FEFFD7F6FFFFD8F4FFFFE0F8FFFFDFF8FFFFDAF5FFFFCDF1FCFFC1ED
      FAFFBCEBFAFFBCEBFAFF2992D6FF000000006E71FDFFB3B6FEFF8084FEFF7A80
      FEFFABAEFEFFD97937FFEAB189FFE49E6CFFE39B68FFE7A77BFFD16018FF0000
      00000000000000000000000000000000000000000000D2B294E3F7E7D7FFF6E1
      CCFFF4DBC1FFF4DABFFFF3D8BCFFF3D7BAFFF4DBC1FFF3DEC9FFB98F6EE70000
      00000000000000000000000000000000000000000000E17D5EFFFCD2C9FFFDD5
      CBFFFEDCD4FFFFE0D7FFFFE3D8FFFFE7E0FFFFE6DFFFFFE4DAFFFCD8CDFFFACF
      C1FFFACCBCFFFACCBCFFD66C29FF00000000000000005FC2E1FF879FA7FF9090
      90FF8D8D8DFF58B8DCFF53B7DFFF4FB4DEFF4BB0DDFF47ACDCFF44A7D7FF7676
      76FF747474FF637C8CFF2F98D8FF000000007276FEFFB7BAFEFFB5B8FEFFB2B5
      FEFFAFB2FEFFDF8343FFECB691FFE6A476FFE6A171FFE9AC84FFD6691CFF0000
      00000000000000000000000000000000000000000000EECCABFDF9ECDFFFF5DF
      C8FFF5DDC5FFF4DCC2FFF4DAC0FFF3D9BDFFF3D7BCFFF8E6D3FFD1A37CFD0000
      00000000000000000000000000000000000000000000E17D5FFF879FA7FF9090
      90FF8D8D8DFFDC7B58FFDF7A53FFDE784FFFDD774BFFDC7647FFD77344FF7676
      76FF747474FF637C8CFFD86E2FFF000000000000000000000000747474C3C5C5
      C5FF939393FF0404040800000000000000000000000000000000040404087B7B
      7BFFAAAAAAFF585858C300000000000000007276FEFF7276FEFF7074FEFF6C6F
      FDFF686AFCFFE18C50FFEDBA98FFE9AB80FFE7A77AFFEAB28CFFD8722AFF0000
      00000000000000000000000000000000000000000000F2D1B1FDF9EDE1FFF6E1
      CCFFF5DFC9FFF5DEC7FFF4DCC3FFF4DBC1FFF4DABFFFF8E7D6FFD5A884FD0000
      0000000000000000000000000000000000000000000000000000747474C3C5C5
      C5FF939393FF0404040800000000000000000000000000000000040404087B7B
      7BFFAAAAAAFF585858C3000000000000000000000000000000006A6A6AAEC3C3
      C3FFA0A0A0FF1717172900000000000000000000000000000000151515298888
      88FFA8A8A8FF515151AE00000000000000000000000000000000000000000000
      000000000000E2915AFFEEC09FFFEAB088FFE9AD84FFEDB693FFDB7A38FF0000
      00000000000000000000000000000000000000000000DBBFA4E3F9EBDEFFF7E7
      D6FFF6E1CCFFF5E0CAFFF5DEC8FFF5DDC4FFF6E1CBFFF5E2D0FFC39C7CE30000
      00000000000000000000000000000000000000000000000000006A6A6AAEC3C3
      C3FFA0A0A0FF1717172900000000000000000000000000000000151515298888
      88FFA8A8A8FF515151AE0000000000000000000000000000000049494975B9B9
      B9FFBEBEBEFF838383DD1212121F03030305030303051111111F787878E5A7A7
      A7FF9D9D9DFF3838387500000000000000000000000000000000000000000000
      000000000000E69A65FFF0C4A5FFECB590FFEBB28BFFEEBB9AFFDF8445FF0000
      0000000000000000000000000000000000000000000094826E97F8E2CCFFFAEE
      E3FFF7E7D6FFF6E2CEFFF6E1CBFFF6E3D0FFF9EADDFFECCFB4FF846B56970000
      000000000000000000000000000000000000000000000000000049494975B9B9
      B9FFBEBEBEFF838383DD1212121F03030305030303051111111F787878E5A7A7
      A7FF9D9D9DFF38383875000000000000000000000000000000000808080C8989
      89DBC3C3C3FFBDBDBDFFA0A0A0FF959595FF929292FF969696FFADADADFFADAD
      ADFF717171DB0505050900000000000000000000000000000000000000000000
      000000000000E89F6EFFF2C8ABFFF0C5A7FFF0C4A5FFEFC1A1FFE18D51FF0000
      0000000000000000000000000000000000000000000002020102BCA58EC0F9E2
      CDFFFAECDEFFF9EEE2FFF9EDE2FFF8E9DAFFF0D5BCFFAE9077C0020101020000
      00000000000000000000000000000000000000000000000000000808080C8989
      89DBC3C3C3FFBDBDBDFFA0A0A0FF959595FF929292FF969696FFADADADFFADAD
      ADFF717171DB0505050900000000000000000000000000000000000000001F1F
      1F308C8C8CDEBBBBBBFFCACACAFFCCCCCCFFCACACAFFC1C1C1FFACACACFF7979
      79DE1A1A1A300000000000000000000000000000000000000000000000000000
      000000000000EAA575FFE8A372FFE89F6CFFE69C69FFE49863FFE2945CFF0000
      0000000000000000000000000000000000000000000000000000020202029482
      7197DDC1A5E3F4D4B5FDF2D1B1FDD7B89BE38C78659702020102000000000000
      0000000000000000000000000000000000000000000000000000000000001F1F
      1F308C8C8CDEBBBBBBFFCACACAFFCCCCCCFFCACACAFFC1C1C1FFACACACFF7979
      79DE1A1A1A300000000000000000000000000000000000000000000000000000
      00000808080C52525281747474BA7D7D7DCC7A7A7ACC6C6C6CBA4A4A4A810707
      070C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000808080C52525281747474BA7D7D7DCC7A7A7ACC6C6C6CBA4A4A4A810707
      070C000000000000000000000000000000009B846BFE977B61FF937358FF906B
      50FF8C654AFF6E4D38C52E241D77000000360000003600000036000000360000
      003600000036000000330000001D00000000000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036000000360000
      003600000036000000330000001D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000071
      1CEB02791CFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000706655ADAF9B80FFC6A986FFD6AF
      89FFC99D78FFD58C5AFFAB7957FFECE8E5FFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFAFAFAFDEDEDEDF300000033000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFAFAFAFDEDEDEDF300000033000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000078
      26EB41A05DFF006217CC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000040402074542378AA49580FFD4BA
      96FFB9A085FFB48A64FFAC9278FFB1A79AFFE5EDF2FFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFD00000036000000000000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFF35007DFF35007DFFFCFCFCFFFCFCFCFF35007DFFFCFC
      FCFF35007DFFFAFAFAFD0000003600000000000000003C3732FF37322EFF312D
      29FF2A2723FF25221FFF1E1B19FF1F954FFD188E46FF128C40FF0D8838FF389E
      5CFF7EC095FF44A260FF006519D1000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000010F0F0D4AC2BEA9FFA99E
      91FFCDD2CFFFD3F2FCFFC0E5F2FF80ABBCFF2180B1FFA3C3D9FFFBFBFBFFFBFB
      FBFFFBFBFBFFFCFCFCFF00000036000000010000000100000036FCFCFCFFFCFC
      FCFFDDDDDDFF35007DFFD9D9D9FFD7D7D7FF35007DFFD4D4D4FF35007DFFD4D4
      D4FF35007DFFFCFCFCFF000000360000000100000000443F39FF84786EFFC2B7
      ADFF7A7066FF7D7369FF34302BFF279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C4
      9DFF68B584FF81C196FF46A464FF00681AD7B53711FF8B2F0EBF000000000000
      00000000000074320D9BC86018FF0000000000000000C86018FF74320D9B0000
      000000000000000000008B2F0EBFB53711FF0000000101010138F4F5F5FFB6C3
      C7FFA1AEB4FF9BACB2FF7BA5B4FF198BB9FF4EBEDDFF3197C1FFC0D5E3FFFAFA
      FAFFFAFAFAFFFCFCFCFF00000036000000010000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFF35007DFFFBFBFBFFFBFBFBFF35007DFFFAFAFAFFFAFAFAFF3500
      7DFFFAFAFAFFFCFCFCFF0000003600000001000000004B453FFF82766DFFCCC2
      B9FF766D63FF796F65FF2C2825F92F9E61FF93CDACFF6DB98DFF69B788FF64B5
      84FF5FB27EFF65B481FF82C197FF3A9F5AFFBA3F12FF8F3C0FBF000000000000
      00007D390FA2E3AB82FFCF722AFF0000000000000000CF722AFFE3AB82FF7D39
      0FA200000000000000008F3C0FBFBA3F12FF0000000100000036FCFCFCFFF7F8
      F8FFCFD8DAFFA8B9BEFF279DC3FF92DAECFF7DE8F8FF55C6E2FF217EB1FFD8E4
      ECFFF8F8F8FFFCFCFCFF00000036000000010000000100000036FCFCFCFFFCFC
      FCFFD6D6D6FF35007DFFCFCFCFFFCDCDCDFF35007DFFC8C8C8FF35007DFFC5C5
      C5FF35007DFFFCFCFCFF000000360000000100000000504943FC82766DFFCCC2
      B9FF776E64FF6F665DFF2C2925D535A269FF95CEAFFF93CDACFF90CBA9FF8FCB
      A7FF72BB8FFF89C7A0FF44A466FF098735FFBC4814FF944111BF00000000833E
      11A9D68444FFE5B089FFD27937FF0000000000000000D27937FFE5B089FFD684
      44FF833E11A900000000944111BFBC4814FF0000000100000036FCFCFCFFFCFC
      FCFFFBFBFBFFF9F9FAFFD6EBF2FF7AC4DCFFBFE9F4FF7DE8F8FF4ABBDAFF338A
      B7FFE9EFF2FFFCFCFCFF00000036000000010000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFF35007DFFFCFCFCFFFCFCFCFF35007DFFFBFBFBFF35007DFFF9F9
      F9FF35007DFFFCFCFCFF000000360000000100000000433D39C39E9185FFCCC2
      B9FFBFB3A9FFA5978AFF282422A83BA46DFF37A36DFF33A167FF309D61FF53AE
      7AFF90CBA9FF4DAA72FF188F46FF00000000BF5015FF974812C0874110ACD686
      49FFE4B088FFE8B590FFD88343FF0000000000000000D88343FFE8B590FFE4B0
      88FFD68649FF874110AC974812C0BF5015FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFD7ECF3FF7AC4DCFFBEE9F3FF7DE8F8FF43B3
      D5FF4A96BEFFF8F9FAFF00000036000000010000000100000036FCFCFCFFFCFC
      FCFFCECECEFF35007DFFC5C5C5FFC2C2C2FF35007DFFBCBCBCFFBBBBBBFFB9B9
      B9FFF6F6F6FFFCFCFCFF00000036000000010101010558514AF9403B36FF564F
      48FF3B3631FF312D29FF1F1B19E503030330151310B5181614FF23201EFF3BA2
      6AFF58B280FF269755FF0000000200000000C55716FFBD611AECDD9864FFE5B1
      8AFFE3AA81FFE9BA97FFD98A50FF0000000000000000D98A50FFE9BA97FFE3AA
      81FFE5B18AFFDD9864FFBD611AECC55716FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFD9EDF3FF7CC5DDFFBEE8F3FF7DE8
      F8FF3FAED2FF6EACCBFF00000036000000010000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF35007DFF35007DFFFBFBFBFFF8F8F8FFF6F6F6FFF3F3
      F3FFF2F2F2FFFCFCFCFF0000003600000001030303059C9084FFB0A295FF7D73
      69FF7A7066FF756B62FF6A6159FF2C2824FF544D46FF7E746AFF7A7066FF41A5
      71FF2F9E63FF000000FE0000000500000000C86018FFC96D27F8DF9C6CFFE8B6
      91FFE6B088FFEBBF9DFFDF915AFF0000000000000000DF915AFFEBBF9DFFE6B0
      88FFE8B691FFDF9C6CFFC96D27F8C86018FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFD9EAEFFF7CC4DDFFBEE8
      F3FF7DE8F8FF3DACD1FF0B364E90000000010000000100000036FCFCFCFFFCFC
      FCFFC7C7C7FFC1C1C1FFBDBDBDFFB7B7B7FFB3B3B3FFB0B0B0FFADADADFFABAB
      ABFFEDEDEDFFFCFCFCFF0000003600000001030202049A8D82E1B9ADA1FF8175
      6BFF81756BFFA99079FFB9A693FFB2A08AFAAF9680FF9E8C7BFF826B59FF6F61
      55FF94887BFF040402E00000000300000000CD691CFF9D5A29BF854D23A4DF9C
      67FFEABD9DFFEDC3A4FFE19A63FF0000000000000000E19A63FFEDC3A4FFEABD
      9DFFDF9C67FF854D23A49D5A29BFCD691CFF0000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFD1E2E7FF7BC4
      DCFFBDE7F3FF70DBF0FF1682B2FD062231490000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF00000036000000010605050826221F489A8D81FF9C90
      84FF85796FFF544D46FF4E4842FF7E746AFF6C645BFF816A56FFA5907BFF9383
      72FF544D46FF0604047A0000000100000000CF722AFFA16232BF000000008554
      2F9EE5A578FFEFC8AAFFE29F6EFF0000000000000000E29F6EFFEFC8AAFFE5A5
      78FF85542F9E00000000A16232BFCF722AFF0000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFEBEBEBFFFCFCFCFFE1F1
      F6FF80C6DEFFBDE7F2FF61CDE7FF1682B3FC0000000100000036FCFCFCFFF9F9
      F9FFBFBFBFFFB9B9B9FFB3B3B3FFAEAEAEFFA9A9A9FFA4A4A4FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF00000036000000010000000000000000726960FFA396
      89FF94887BFF9E9185FF3C3732FF000000004A443EFF7C7268FF84786EFF3C37
      32FF2D2924A70202000C0000000200000000D27937FFA3673CBF000000000000
      00007C54358EEFC8ACFFE6A575FF0000000000000000E6A575FFEFC8ACFF7C54
      358E0000000000000000A3673CBFD27937FF0000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFDDEBEFFF72B8D0F6A9E0EEFF1481ADEF0000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF5656569100000020000000000000000000000000000000000000
      00007D7368E2C2B7ADFF635B53FF000000007A7066FFA79A8DFF8A7F74E40000
      000000000000000000000000000000000000D98A50FFA9734ABF000000000000
      0000000000007A594187DEA27AF40000000000000000DEA27AF47A5941870000
      00000000000000000000A9734ABFD98A50FF0000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF54545491010F1337167696C30A485F800000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF545454910000002000000002000000000000000000000000000000000000
      0000897E73E2BBAFA3FF9C9084FF00000000AD9F92FF9C9084FF544E47DA0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      002000000002000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000029292963414141BF3C3C3CBF1F1F1F63000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1F1F42757575FF1F1F
      1F4800000000000000000000000000000000C28350FFC28350FFC28350FFC283
      50FFC28350FF7F502CB00000000000000000000000000000000000000000A378
      56C4CA9167F4D19566FFCE9161FFCB8D5CFF0000000000000000000000001111
      11290505050E01010102767676EABDBDBDFFB2B2B2FF545454EA010101020404
      040E0C0C0C290000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000D30
      0FB8000100060000000000000000000000000000000000000000000000000000
      0000000000000E0E0E193636366134343461333333639F9F9FFFD0D0D0FF9B9B
      9BFF2121214B1919193B1818183B07070710F0E2D8FFF0E2D8FFF0E2D8FFF0E2
      D8FFF0E2D8FFC2885AFD0000000000000000000000000000000000000000D7A0
      73FFF8F2EDFFF7F0EAFFF6EDE6FFF4EAE2FF00000000000000004E4E4E9B6D6D
      6DFD5A5A5AE70B0B0B19777777E7CBCBCBFFC7C7C7FF585858E7080808194F4F
      4FE74D4D4DFD2B2B2B9B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000051005381545
      18F7103912D9000000000000000000000000A37856C4CA9167F4D19566FFCE91
      61FFCB8D5CFFA99382FFBABABAFFB8B8B8FFB5B5B5FFDDDDDDFFEFEFEFFFD0D0
      D0FF9E9E9EFF939393FF8F8F8FFF40404095EAC7ACFFFFFFFFFFFFFFFFFFFFFF
      FFFFF0E2D8FFC58B5DFF0000000000000000000000000000000000000000D9A3
      78FFF9F3EEFFEBD2BDFFFFFFFFFFEBD3BEFF000000004F4F4F7BBCBCBCFFDEDE
      DEFFA6A6A6FF7D7D7DF4848484FEC4C4C4FFC2C2C2FF6C6C6CFE686868F4A6A6
      A6FFD2D2D2FF808080FF2727277B0000000043954BFF3F9047FF3B8A42FF3784
      3EFF337E39FF255E29C60B1D0D40153618901D4E21D81F5623FF346B38FF619B
      65FF457B49FF103612CF0000000000000000D7A073FFF8F2EDFFF7F0EAFFF6ED
      E6FFF4EAE2FFA9A7A6FFEEEEEEFFF4F4F4FFF1F1F1FFEDEDEDFFEBEBEBFFEBEB
      EBFFEDEDEDFFEDEDEDFFF4F4F4FF6F6F6FFCEACDB4FFEACDB4FFEACDB4FFEACD
      B4FF519856FF398740FF337E3AFF0C200D46000000000000000000000000DDA7
      7CFFF9F3EFFFEBD0B9FFEBD0BAFFEBD0BAFF000000005353537DA5A5A5FED5D5
      D5FFC5C5C5FFCBCBCBFFD1D1D1FFC9C9C9FFC7C7C7FFCCCCCCFFC5C5C5FFBDBD
      BDFFCBCBCBFF6D6D6DFE3131317D00000000489C50FF88CA8FFF85C88CFF82C5
      89FF7EC386FF61A967FF34773AFF5B9760FF84BD8AFF93CE98FF91CC96FF8FCB
      93FF8DCA91FF467C49FF103411C400000000D9A378FFF9F3EEFFEBD2BDFFFFFF
      FFFFEBD3BEFFB1B1B1FFEBEBEBFFF2F2F2FFEFEFEFFFEEEEEEFFECECECFFEBEB
      EBFFE9E9E9FFE8E8E8FFF0F0F0FF747474FFEACFB9FFFBF6F2FFFFFFFFFFFFFF
      FFFF58A15DFF78BE7EFF65AF6BFF35813CFE0C200F460000000000000000DFA9
      81FFF9F3EFFFEACEB6FFFFFFFFFFEBD0BAFF00000000000000005A5A5A85C5C5
      C5FFC1C1C1FFC5C5C5FFC7C7C7FFAAAAAAFFA7A7A7FFC1C1C1FFBEBEBEFFB5B5
      B5FFAAAAAAFF3636368500000000000000004CA255FF8DCC94FF8ACB91FF86C9
      8EFF83C78AFF6FB375FF4B8D52FF9BD2A1FF97CF9DFF97CF9CFF95CE9AFF92CC
      97FF90CC95FF49814DFF123A15C900000000DDA77CFFF9F3EFFFEBD0B9FFEBD0
      BAFFEBD0BAFFAFACAAFFF3F3F3FFF3F3F3FFF2F2F2FFF0F0F0FFEFEFEFFFEDED
      EDFFEBEBEBFFEAEAEAFFF1F1F1FF797979FFE8C7ABFFE8C7ABFFE8C8AFFFE8C8
      ADFF5EAB65FF499D51FF5AA661FF69B16FFF37843EFE0D210F4600000000E1AD
      86FFFAF4F0FFEACBB1FFEACCB2FFEACCB2FF838383CD7F7F7FE3959595EECFCF
      CFFFC6C6C6FFCCCCCCFF7A7A7AC629292944272727446F6F6FC6C1C1C1FFBCBC
      BCFFB9B9B9FF5C5C5CEE4E4E4EE3424242CD51A85AFF4DA356FF499E52FF489A
      51FF61AC69FF539B5BFF7AB582FF9FD4A5FF7BB683FF4C8A51FF28652DFF528D
      58FF508954FF19451BD10000000000000000DFA981FFF9F3EFFFEACEB6FFFFFF
      FFFFEBD0BAFFAEAEAEFFFAFAFAFFF5F5F5FFF4F4F4FFF2F2F2FFF1F1F1FFEFEF
      EFFFEEEEEEFFECECECFFF3F3F3FF808080FFE8C7ABFFFFFFFFFFFFFFFFFFFFFF
      FFFFF1E5DBFFC58553FF1B381D5A45974DFE6DB474FF398640FE0E241048E3B0
      8BFFFAF6F1FFEAC9ADFFFFFFFFFFEAC9AFFFBEBEBEFDE2E2E2FFD2D2D2FFC6C6
      C6FFCDCDCDFFB1B1B1FF27272744000000000000000028282844A8A8A8FFC2C2
      C2FFB7B7B7FFC0C0C0FFD2D2D2FF5F5F5FFD0000000000000000000000000307
      030C24502985418D4AFF9FD3A6FFA1D4A7FF34773BFC0E210F4B000000002865
      2EFD205324DC000000000000000000000000E1AD86FFFAF4F0FFEACBB1FFEACC
      B2FFEACCB2FFB2B2B2FFFCFCFCFFF8F8F8FFF6F6F6FFF5F5F5FFF3F3F3FFF2F2
      F2FFF0F0F0FFEFEFEFFFF4F4F4FF858585FFE8C7ABFFE9C9AFFFE8C8AFFFE8CC
      B4FFF2E7DEFFC88957FF0000000018351B5447994FFE72B778FF4E9A55FF3582
      3CFF418545FFE9C5A9FFE9C5ABFFEAC7ABFFC2C2C2FDE9E9E9FFD6D6D6FFC9C9
      C9FFCECECEFFA5A5A5FF23232344000000000000000029292944ACACACFFC4C4
      C4FFBABABAFFC6C6C6FFDDDDDDFF696969FD0000000000000000000000000307
      030C254E2A8571B279FFA9DAB0FF77B580FF398340FC0F27134B000000002E6F
      34FD26642ADC000000000000000000000000E3B08BFFFAF6F1FFEAC9ADFFFFFF
      FFFFEAC9AFFFBBBBBBFFF2F2F2FFFCFCFCFFFAFAFAFFF8F8F8FFF6F6F6FFF5F5
      F5FFF5F5F5FFF5F5F5FFF9F9F9FF8B8B8BFFE8C7ABFFFFFFFFFFFFFFFFFFFFFF
      FFFFF7F1EBFFCB8E5DFF000000000000000019361C54499C51FE79BC80FF80C1
      86FF488F4EFFE9C2A5FFFFFFFFFFE8C3A8FFA0A0A0CDADADADE3B3B3B3EED8D8
      D8FFCDCDCDFFBCBCBCFF656565C61F1F1F44222222446F6F6FC6C3C3C3FFC2C2
      C2FFCDCDCDFF838383EE787878E3696969CD55AF61FF53AC5EFF50A85BFF53A6
      5DFF79BB83FFA6D7AEFFACDCB3FF539B5BFF6AB071FF519D59FF38853FFF539C
      59FF4F9856FF246029D10000000000000000E5B38EFFFAF6F2FFE9C5A9FFE9C5
      ABFFEAC7ABFFD6C1B2FFC5C5C5FFC2C2C2FFC1C1C1FFDFDFDFFFFBFBFBFFDFDF
      DFFFB8B8B8FFADADADFFA9A9A9FF6B6B6BBCE9C2A5FFE9C2A5FFE9C2A5FFE9C2
      A5FFFBF7F4FFCE9262FF0000000000000000000000001A371D544BA053FF4598
      4DFF4F9956FFE9C2A5FFE9C2A5FFE9C2A5FF000000000000000066666685D4D4
      D4FFCCCCCCFFC9C9C9FFBABABAFF9C9C9CFFA1A1A1FFC2C2C2FFC6C6C6FFC1C1
      C1FFB7B7B7FF47474785000000000000000057B363FFB8E3BFFFB6E2BEFFB5E1
      BDFFB3E0BBFFB2DFB9FF99CFA2FF4E9C57FF8DCD95FF8BCB93FF88CA8FFF85C8
      8CFF82C589FF519957FF245D28C900000000E7B693FFFBF7F4FFE9C2A5FFFFFF
      FFFFE8C3A8FFFEFEFEFFF2F2F2FFF2F2F2FFDDC2ADFFB5B5B5FFE1E1E1FFB2B2
      B2FFE1DCD8FFC2916AFF1E1E1E3109090910FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFBF7F4FFD19668FF0000000000000000000000000000000000000000EBBC
      9AFFFBF7F4FFFFFFFFFFFFFFFFFFFFFFFFFF000000006363637DC3C3C3FEDCDC
      DCFFD4D4D4FFD9D9D9FFDBDBDBFFD6D6D6FFD4D4D4FFD9D9D9FFD2D2D2FFCBCB
      CBFFC8C8C8FF787878FE3636367D0000000058B464FFB9E3C1FFB8E3C0FFB7E2
      BFFFB6E1BDFF90CB98FF63AE6CFF5FAC68FF88C990FF8FCF98FF8DCC94FF8ACB
      91FF86C98EFF559E5CFF275F2BC400000000E9B997FFFBF7F4FFE9C2A5FFE9C2
      A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFDDBFA9FFB6B2B0FFD4BB
      A8FFFBF7F4FFCE9262FF000000000000000081C588FF7CC283FF78C07EFF74BD
      7AFFFBF7F4FFD49A6DFF0000000000000000000000000000000000000000ECBE
      9DFFFBF7F4FF9BD5A4FF97D3A0FF93D09CFF000000006363637BDCDCDCFFEDED
      EDFFDBDBDBFFB9B9B9F4BDBDBDFED6D6D6FFD4D4D4FFAFAFAFFEA4A4A4F4CBCB
      CBFFE7E7E7FFB7B7B7FF4343437B0000000058B464FF58B464FF58B464FF56B1
      62FF54AE60FF3F8448C6142A17402E5F3390428B4BD84A9F53FF56A55EFF71B7
      79FF5DA765FF2F6F36CF0000000000000000EBBC9AFFFBF7F4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFBF7F4FFD19668FF0000000000000000FBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFD69E72FE0000000000000000000000000000000000000000E4B8
      9AF5FBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FF00000000000000007F7F7F9BCCCC
      CCFDB7B7B7E713131319AFAFAFE7DEDEDEFFDDDDDDFFA1A1A1E7101010199C9C
      9CE7A6A6A6FD6363639B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000102313384596
      4CF7397F40D9000000000000000000000000ECBE9DFFFBF7F4FF9BD5A4FF97D3
      A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C07EFF74BD
      7AFFFBF7F4FFD49A6DFF0000000000000000E4B18BFFE2AE87FFE0AB83FFDDA8
      7EFFDCA47BFFAC805FCA0000000000000000000000000000000000000000765E
      507ED4AB8FE3EDBF9EFFEBBD9CFFEBBB99FF0000000000000000000000002121
      21290A0A0A0E02020202B6B6B6EAE5E5E5FFE4E4E4FF9E9E9EEA010101020A0A
      0A0E1C1C1C290000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003874
      3DB801040106000000000000000000000000DBB193EBFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFD19B6FF800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004E4E4E63959595BF929292BF4A4A4A63000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000765E507ED4AB8FE3EDBF9EFFEBBD
      9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB83FFDDA8
      7EFFDCA47BFFAC805FCA00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C381C881664
      33F2176935FF166433F20C381C88000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D275C78023A
      A1DF0340BAFE023DA4E30020587A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000103951F7265C84FB4685B9FB316A8EC1050F182200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B361B84268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C0000000000000000000000000000
      000000000000000000000000000000000000000000000D2B61802361C6FB1F75
      E6FF0477EAFF0062DDFF034ABAFC0020587A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A6280FC93C7F9FF90C9F9FF3F84C9FF1E62A6FF5A6874FF616161FF4F4F
      4FD7000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000196736F760B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F70000000000000000000000000000
      00000000000000000000000000000000000000000000023DA4E3619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF023CA5E40000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002A2A2A4B6E6E
      6ECC4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF4289C1FFC9D5E2FF6262
      62FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF0000000000000000000000000000
      000000000000000000000000000000000000000000000340BAFEADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BAFE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000797979C9B2B2B2FFD4D4
      D4FF94B0C4FF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4C98D9FF606B
      75FF000000000000000000000000000000009D9D9DFF9D9D9DFF9D9D9DFF9D9D
      9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF45875DFF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF1B6B38FF9D9D9DFF9D9D9DFF9D9D9DFF9D9D
      9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF164EB4FF8CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF154EB5FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1A1A1FFF0F0F0FFE0E0
      E0FFD4D2D2FF87A4B4FF73B7D5FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4293
      D7FF07111B200000000000000000000000009D9D9DFFBABDBDFFE6ECECFFE6ED
      EDFFE7EDEDFFE8EEEEFFE9EFEFFFE9EFEFFFEAEFEFFF9CBEABFF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF538163FF9D9D9DFFBABDBDFFE6ECECFFE6ED
      EDFFE7EDEDFFE8EEEEFFE9EFEFFFE9EFEFFFEAEFEFFF8AA8DBFF3A74D2FF8CB4
      F7FFB7D6FEFF70A7F5FF2D6ACBFF5A75A9FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A7FFEAEAEAFFDDDD
      DDFFD5D4D3FFBFB8B6FF85A5B5FF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF3D8CD0F20A1C2C34000000000000000000000000909090EADBE2E2FFCDCF
      CFFFCBCBCBFFCCCCCCFFCECECEFFCFCFCFFFD0D0D0FFD2D2D2FF97B3A1FF5C96
      71FF4D8D64FF47885EFF648870FB0000000000000000909090EADBE2E2FFCDCF
      CFFFCBCBCBFFCCCCCCFFCECECEFFCFCFCFFFD0D0D0FFD2D2D2FF7C98CCFF235B
      C1FF0441BBFF1B56BEFF516FA7FB000000000000000000000000000000000000
      000000000000000000FF000000FF0000000000000000000000FF000000FF0000
      00000000000000000000000000000000000000000000AEAEAEFFEBEBEBFFDDDD
      DDFFD7D5D5FFC0BAB8FFBFB8B7FF90B4C0FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF398ACBE80C1A27300000000000000000909090EAD9E1E1FFCBCC
      CCFFC7C7C7FFC8C8C8FFCACACAFFCBCBCBFFCDCDCDFFCECECEFFD0D0D0FFD1D1
      D1FFD3D4D4FFE5EBEBFF989898F70000000000000000909090EAD9E1E1FFCBCC
      CCFFC7C7C7FFC8C8C8FFCACACAFFCBCBCBFFCDCDCDFFCECECEFFD0D0D0FFD1D1
      D1FFD3D4D4FFE5EBEBFF989898F7000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF000000FF000000000000
      00000000000000000000000000000000000000000000B4B4B4FFECECECFFDEDE
      DEFFD9D8D8FFC2BCBAFFC1BAB9FFC0B9B8FF7BC2B5FF7AD4EDFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF548FC2EC0C161D2600000000909090EAD7DFDFFFC7C9
      C9FFC2C2C2FFC4C4C4FFC5C5C5FFC8C8C8FFC9C9C9FFCACACAFFCCCCCCFFCDCD
      CDFFD0D1D1FFE3EAEAFF989898F70000000000000000909090EAD7DFDFFFC7C9
      C9FFC2C2C2FFC4C4C4FFC5C5C5FFC8C8C8FFC9C9C9FFCACACAFFCCCCCCFFCDCD
      CDFFD0D1D1FFE3EAEAFF989898F7000000000000000000000000000000000000
      0000000000000000000000000000000000FF000000FF00000000000000000000
      00000000000000000000000000000000000000000000BABABAFFECECECFFDFDF
      DFFFDAD9D9FFC5BEBDFFC3BDBBFFC2BBBAFFC0BAB8FF8AB7C5FF7ED5EDFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF700000000909090EAD6DFDFFFC3C5
      C5FFC0C0C0FFC0C0C0FFC2C2C2FFC3C3C3FFC4C4C4FFC5C5C5FFC8C8C8FFC9C9
      C9FFCDCDCDFFE0E8E8FF989898F70000000000000000909090EAD6DFDFFFC3C5
      C5FFC0C0C0FFC0C0C0FFC2C2C2FFC3C3C3FFC4C4C4FFC5C5C5FFC8C8C8FFC9C9
      C9FFCDCDCDFFE0E8E8FF989898F7000000000000000000000000000000000000
      00000000000000000000000000FF000000FF000000FF000000FF000000000000
      00000000000000000000000000000000000000000000BFBFBFFFECECECFFE0E0
      E0FFDDDCDCFFC7C0BFFF919090FF8E8E8EFF8C8C8CFF898989FF97CAD9FF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E800000000848484D6C7CDCDFFDBE4
      E4FFDBE4E4FFDBE5E5FFDCE5E5FFDCE5E5FFDDE6E6FFDDE6E6FFDEE7E7FFDFE8
      E8FFE0E8E8FFCDD3D3FF8C8C8CE30000000000000000848484D6C7CDCDFFDBE4
      E4FFDBE4E4FFDBE5E5FFDCE5E5FFDCE5E5FFDDE6E6FFDDE6E6FFDEE7E7FFDFE8
      E8FFE0E8E8FFCDD3D3FF8C8C8CE3000000000000000000000000000000000000
      000000000000000000FF000000FF0000000000000000000000FF000000FF0000
      00000000000000000000000000000000000000000000C5C5C5FFEFEFEFFFE1E1
      E1FFDDDDDDFFC8C2C1FFC8C1C0FFC5C0BEFFC4BEBDFFC2BCBAFFDAD6D5FF709A
      A9FF56A4D8FF84B0DBFF449CD0FF0F374D5E00000000292929437B7B7BC89D9D
      9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D
      9DFF9D9D9DFF888888DD3030304E0000000000000000292929437B7B7BC89D9D
      9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D
      9DFF9D9D9DFF888888DD3030304E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CBCBCBFFE9E9E9FFE9E9
      E9FFE2E1E1FFCBC5C3FF979696FF959494FF939292FF919090FFDBD7D6FF9797
      97FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000039393948B6B6B6E4DFDF
      DFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3C2FFC8C2C0FFC7C0C0FFDCD9D8FF9D9D
      9DFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C0C0C0F8888
      88ABDCDCDCFFE6E3E3FFE1DEDCFFDFDCDCFFDFDCDBFFDEDBDBFFEEECECFFA4A4
      A4FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007F7F7F9FC6C6C6FBC3C3C3FFBFBFBFFFBABABAFFB5B5B5FFAFAFAFFF6E6E
      6EA7000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000103951F7265C84FB4685B9FB316A8EC1050F182200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C381C881664
      33F2176935FF166433F20C381C88000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D275C78023A
      A1DF0340BAFE023DA4E30020587A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000296280FB93C7F9FF90C9F9FF3F84C9FF195DA1F30715212F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B361B84268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C0000000000000000000000000000
      000000000000000000000000000000000000000000000D2B61802361C6FB1F75
      E6FF0477EAFF0062DDFF034ABAFC0020587A00000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF00000000000000006E4934A9C28D66FFBF8A64FFBD87
      62FF4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF3E80B5FF987769FFAA73
      52FFA87151FFA86F4FFF6E4934A9000000006E4934A9C28D66FFBF8A64FFBD87
      62FFBA845FFFB8825DFFB37C5AFFB17A58FFB07956FF1E6A38FF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F76E4934A9C28D66FFBF8A64FFBD87
      62FFBA845FFFB8825DFFB37C5AFFB17A58FFB07956FF1449ADFF619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF023CA5E400000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000C8916AFFE6E5E5FFE5E5E5FFE5E5
      E6FF96B4C9FF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4D9ADBFFCFD9
      E3FFE5E6E6FFE6E5E6FFA8704FFF00000000C8916AFFE6E5E5FFE5E5E5FFE5E5
      E6FFE5E5E5FFE5E5E5FFE6E5E5FFE5E5E5FFE6E5E5FF2F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FFC8916AFFE6E5E5FFE5E5E5FFE5E5
      E6FFE5E5E5FFE5E5E5FFE6E5E5FFE5E5E5FFE6E5E5FF0441BBFFADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BAFE00000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000CA936CFFE7E7E7FFE8E7E7FFE7E7
      E7FFE7E7E7FFA3C5D7FF73B7D6FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4898
      DCFFE6F1FAFFE7E7E7FFA97151FF00000000CA936CFFE7E7E7FFE8E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFC2C2C2FFFFFFFFFFFFFFFFFF488A60FF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F7CA936CFFE7E7E7FFE8E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFC2C2C2FFFFFFFFFFFFFFFFFF235BC1FF8CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF023AA0DE00000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000CC966DFFE9E9E9FFD28256FFD282
      56FFD28256FFE9E9E9FF88ADBEFF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4A99DEFFC3D7E7FFAB7352FF00000000CC966DFFE9E9E9FFD28256FFD282
      56FFD28256FFE9E9E9FFC2C2C2FFFFFFFFFFFFFFFFFFA7C7B2FF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF0D391E8CCC966DFFE9E9E9FFD28256FFD282
      56FFD28256FFE9E9E9FFC2C2C2FFFFFFFFFFFFFFFFFF95B0E3FF3B74D2FF8CB4
      F7FFB7D6FEFF70A7F5FF2C69CAFF021C4F6D00000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000D19B71FFECECECFFECECEBFFECEC
      EBFFECECECFFECEBECFFC2C2C2FF94BCCAFF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4E9FE0FF9B7C6CFF00000000D19B71FFECECECFFECECEBFFECEC
      EBFFECECECFFECEBECFFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FF8EAB99FF5B95
      70FF4D8D64FF47885EFF6F774FFF00000000D19B71FFECECECFFECECEBFFECEC
      EBFFECECECFFECEBECFFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FF7390C2FF2159
      BFFF0441BBFF1B56BEFF5D5F85FF0000000000000000808080FFFFFFFFFFFFFF
      FFFF000000FF000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000D49D73FFEFEEEEFFEFEFEFFFEFEE
      EEFFEFEEEEFFEEEFEEFFEEEEEEFFEEEFEFFFB4DEEBFF7BD4EEFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF6198C9FF0C161D26D49D73FFEFEEEEFFEFEFEFFFEFEE
      EEFFEFEEEEFFEEEFEEFFEEEEEEFFEEEFEFFFEEEEEEFFEEEEEEFFEEEEEEFFEFEE
      EEFFEEEEEEFFEEEEEFFFB17A58FF00000000D49D73FFEFEEEEFFEFEFEFFFEFEE
      EEFFEFEEEEFFEEEFEEFFEEEEEEFFEEEFEFFFEEEEEEFFEEEEEEFFEEEEEEFFEFEE
      EEFFEEEEEEFFEEEEEFFFB17A58FF0000000000000000808080FFFFFFFFFFFFFF
      FFFF000000FF000000FFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000D59F74FFF1F1F0FFF1F0F1FFF0F1
      F1FFF1F0F1FFF1F1F1FFC2C2C2FFFFFFFFFFFFFFFFFFB4E6F5FF80D6EEFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF7D59F74FFF1F1F0FFF1F0F1FFF0F1
      F1FFF1F0F1FFF1F1F1FFC2C2C2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF1F1F1FFB47C5AFF00000000D59F74FFF1F1F0FFF1F0F1FFF0F1
      F1FFF1F0F1FFF1F1F1FFC2C2C2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF1F1F1FFB47C5AFF0000000000000000808080FFFFFFFFFFFFFF
      FFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFF
      FFFFFFFFFFFF808080FF0000000000000000D8A177FFF2F2F2FFD28256FFD282
      56FFD28256FFF2F2F3FFC2C2C2FFFFFFFFFFFFFFFFFFFFFFFFFFB0E6F5FF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E8D8A177FFF2F2F2FFD28256FFD282
      56FFD28256FFF2F2F3FFC2C2C2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF3F3F3FFB6805CFF00000000D8A177FFF2F2F2FFD28256FFD282
      56FFD28256FFF2F2F3FFC2C2C2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF3F3F3FFB6805CFF0000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFF
      FFFFFFFFFFFF808080FF0000000000000000D9A277FFF5F5F5FFF5F5F4FFF4F5
      F4FFF4F4F4FFF5F5F4FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FF93BC
      CCFF56A4D8FF84B0DBFF449CD0FF0F374D5ED9A277FFF5F5F5FFF5F5F4FFF4F5
      F4FFF4F4F4FFF5F5F4FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
      C2FFC2C2C2FFF4F4F5FFB9845EFF00000000D9A277FFF5F5F5FFF5F5F4FFF4F5
      F4FFF4F4F4FFF5F5F4FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2
      C2FFC2C2C2FFF4F4F5FFB9845EFF0000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF
      FFFFFFFFFFFF808080FF0000000000000000DBA378FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFBC8661FF00000000DBA378FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFBC8661FF00000000DBA378FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFBC8661FF0000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000DCA679FFDCA679FFDCA679FFDCA6
      79FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA6
      79FFDCA679FFDCA679FFBF8A64FF00000000DCA679FFDCA679FFDCA679FFDCA6
      79FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA6
      79FFDCA679FFDCA679FFBF8A64FF00000000DCA679FFDCA679FFDCA679FFDCA6
      79FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA679FFDCA6
      79FFDCA679FFDCA679FFBF8A64FF0000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000DBA983FDE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFBE8E6CFD00000000DBA983FDE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFBE8E6CFD00000000DBA983FDE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B8
      91FFE8B891FFE8B891FFBE8E6CFD0000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF0000000000000000462E216BD3A886F4DCA679FFDCA5
      78FFDAA378FFD8A177FFD59F74FFD49D73FFD29C71FFCF9970FFCE986EFFCB95
      6DFFC9936AFFBA9273F4462E216B00000000462E216BD3A886F4DCA679FFDCA5
      78FFDAA378FFD8A177FFD59F74FFD49D73FFD29C71FFCF9970FFCE986EFFCB95
      6DFFC9936AFFBA9273F4462E216B00000000462E216BD3A886F4DCA679FFDCA5
      78FFDAA378FFD8A177FFD59F74FFD49D73FFD29C71FFCF9970FFCE986EFFCB95
      6DFFC9936AFFBA9273F4462E216B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000807
      061450443EB981736CFF685F5BCD423E3C873C383582574E49C0756862FB564F
      4BB30707050E00000000000000000000000000000000020303030E1D24380E39
      507E0A5C84D1072F46A300000024000000180000001C051E2A730C6591E2113D
      5179000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000030101066452
      49C8A18B7EFFB3A49AFFB1A49BFFA3928AFFA09087FFAC9B93FFB19F96FF9F8B
      81FF594D47A9000000000000000000000000000000001D91BEE70AAAE4FF0BB7
      EFFF0BBCF4FF0F7BA8FF4E4E4EFF464646FF0B0809FF053D58FF0BB8F0FF0CB3
      EBFF18789AB007171C2200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F3631858C75
      69FFA49185FFAF9C92FFB09F95FFAE9C92FFAD9A91FFAD9B91FFAE9A91FFA996
      8BFF958073FF211C18470000000000000000000000001485AEC60BBDF5FF0BBE
      F5FF0CBEF3FF6297A7FFFBFDFDFFFBFDFDFFD8DADAFF2D6177FF0BBAF2FF0BBD
      F4FF0BBEF5FF1D96BECE00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF0000000000000000000000000B0A09196D584CF68A76
      6BFF9A887DFF9F8E84FF9E8D84FF9D8C82FF9D8C83FFA08E85FFA18F86FF9D8B
      81FF958379FF513A31C00000000000000000000000001B90BAD20BBCF3FF0BBE
      F5FF0B84A7FF585757FFFBFDFDFFFBFDFDFFFBFDFDFF90C3D8FF0993C8FF0985
      B5FF16ADDCEA0C161A1D00000000000000000000000000000000000000007556
      0085D79A0BE9C99604E040350056000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF00000000000000000000000030251F7B6A5449FF816E
      65FF8B786FFF8D7A72FF8B7970FF8A786FFF8B7970FF8E7C73FF907E75FF8D79
      70FF806C62FF453228BE00000000000000000000000010242B341A627B950DAC
      DFFF748A91FFF8FAFAFFF6F8F8FFFBFDFDFFFBFDFDFFB6E5F5FF155365FF0605
      06FF195462A2000000000000000000000000000000000000000000000000C483
      08C9FC994EFFFC7400FFDA9C02F34137005D0000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF000000000000000000000000402F25C6675449FF7665
      5CFF7B6A62FF7D6B64FF7B6B64FF7B6A63FF7B6B64FF7E6D66FF806D66FF7663
      5AFF4B413AB90101010500000000000000000000000000000000010101381318
      1BFFF2F4F4FFFBFDFDFFF4F6F6FFFBFDFDFFFBFDFDFFFBFDFDFF686868FF0D0B
      0BFF0302028B000000000000000000000000000000000000000000000000CA8B
      0BCFFCB978FFFC7700FFFC7300FFDC9D02F5423A00620B0B000D0B0B000D0000
      00000000000000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000000000004C362CF775675FFF7464
      5CFF73635BFF73635BFF73635CFF73635CFF74645DFF75655DFF73635BFF6854
      4AFE111010280000000000000000000000000000000000000000000000030E0D
      0DD4D2D4D4FFFBFDFDFFF5F7F7FFFBFDFDFFFBFDFDFFFBFDFDFF616060FF0B09
      09FF0101017E0000000000000000000000000000000000000000000000005941
      005CC68D00CEFCB76EFFFC7408FFFC6E00FFDBA000F4D6AE00E8DBB200E83F39
      00530000000000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000000000004B3830FF847871FF8275
      6FFF7D716AFF7B6D67FF786A63FF756760FF72645CFF706058FF6D5D54FF5B4A
      42DA000000000000000000000000000000000000000000000000000000000404
      046A757575FFFBFDFDFFFAFCFCFFFBFDFDFFFBFDFDFFF5F7F7FF3D3D3DFF0908
      08FF0303033C0000000000000000000000000000000000000000000000000000
      000031240034C58607CAD88F11DEFC8619FFFC7000FFFC7B13FFFC7B0DFFE4BD
      00F7423C005900000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF000000000000000000000000443631EE8E8480FF8C82
      7CFF887D77FF857972FF82756EFF7E716BFF7B6E68FF786B64FF71625AFF6F5C
      52EB080808130000000000000000000000000000000000000000000000000000
      001B0F0E0EF6E1E2E2FFF5F7F7FFFAFCFCFFEAEBEBFF989898FF0A0808FF0404
      04CF000000010000000000000000000000000000000000000000000000000000
      00000000000000000000805E0086FCAC57FFFC8728FFFC8E35FFFC7E19FFFC7B
      0DFFE0BF05F300000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF000000000000000000000000221917A1908782FF9790
      8CFF938A87FF908782FF8D837DFF897E78FF847872FF80736CFF7B6D67FF816E
      64FF413834820000000000000000000000000000000000000000000000000000
      00000101015EAAABABFFE5E7E8FFF1F3F3FFFCFEFEFF505050FF040101EF0101
      0122000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A87400AFFCB781FFFC9744FFFCAE6CFFFCB14CFFFC9C
      49FFE5AC05F000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF0000000000000000000000000302021D584C49ED9993
      90FF9A9592FF99928FFF97908CFF968E8AFF928A86FF8C827DFF857872FF8072
      6BFF746057F31F18155500000000000000000000000000000000000000000000
      000000000000323535BE559BB6FF559DBEFFB7BFC3FF1D1C1CFF040303750000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000062490066FAA423FDFCAD76FFFCAF4EFFFCC251FFECB3
      19F54A3E005900000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF000000000000000000000000000000000505032E433B
      38CC726864FF776D69FF514945E8211B189A392E2BAE695B57F56F625CFF6254
      4EFF3A2C27CA0705041A00000000000000000000000000000000000000000000
      000000000000092938A70EB9E7FF1BCAEBFF0E7698FF0A0808FA0000000D0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000006C520070FCAD3AFFFCD3AEFFEA9C19ED4235
      00480000000000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      00000404041F05050528010101041E1A19424C4642A7100F0F2E0606062F0505
      0527000000010000000000000000000000000000000000000000000000000000
      000000000000373636B4316577FF3A7180FF5E6264FF030101E6000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000201000284600089C68507CB5F4700630000
      00000000000000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      00000000000000000000000000001E1815537E6B61FF75665FE7090808190000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000201E1EBB3D3C3CFF444444FF4A4A4AFF030101D6000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF808080FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000034E423CC9685449FF3D332E890000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000303039D030101FF030101FF191818FF0606069C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000005050314413937A2493E38BB0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000001010122030202CC030101F10E0C0CAB00000011000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001616161C4D66728E78C2DAFF76C4DDFF85C2D8FF96AD
      B8E37575758E0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001616161C00000000000000000000
      000000000000464342554C91B5E3299DD5FF12C4F7FF32D3FFFF39D6FEFF05CA
      FAFF90BED0FF1515141C00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000711CEB02791CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000002791CFF00711CEB0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000002791CFF00711CEB0000000000000000000000000000000000711CEB0279
      1CFF00000000000000000000000000000000C6986AFFC9AD9EFFB3A09BE3836D
      65AABF957BFFAA6447FF5C96B5E32E92CCFF17B9EEFF25D0FEFF42D6FEFF0AD1
      FEFF50BADCFF4746455500000000000000000000000000000000000000000000
      000000000000000000000000000000000000007826EB41A05DFF006217CC0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000006217CC41A05DFF007826EB0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000062
      17CC41A05DFF007826EB00000000000000000000000000000000007826EB41A0
      5DFF006217CC000000000000000000000000A07638C6DDAD76FFE4BB8CFFD59E
      59FFC17D3BFFAB5C31FF97AAB6FF308AC5FF20ADE3FF18CEFEFF41D6FEFF1BD4
      FFFF18BBE9FF8A8B8BAA00000000000000000000000000000000000000000000
      00001F954FFD198F47FD138D41FD0E8939FD389E5CFF7EC095FF44A260FF0065
      19D1000000000000000000000000000000000000000000000000000000000000
      0000006519D144A260FF7EC095FF389E5CFF0E8939FD138D41FD198F47FD1F95
      4FFD000000000000000000000000000000000000000000000000006519D144A2
      60FF7EC095FF389E5CFF0E8A39FF138E41FF188F46FF1E954EFF389E5CFF7EC0
      95FF44A260FF006519D100000000000000005A421A71D59E5CFFE7C094FFDAA7
      68FFC8883FFFB16533FFB19186FF338EC6FF27A2DAFF12C8FAFF39D4FEFF30D5
      FEFF02CBFBFF93A9B4E300000000000000000000000000000000000000000000
      0000279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B584FF81C196FF46A4
      64FF00681AD70000000000000000000000000000000000000000000000000068
      1AD746A464FF81C196FF68B584FF87C49DFF89C5A0FF8CC8A4FF8FCAA8FF279A
      59FF000000000000000000000000000000000000000000681AD746A464FF81C1
      96FF68B584FF87C49DFF89C5A0FF8CC8A4FF8FCAA8FF8CC8A3FF87C39EFF68B5
      84FF81C196FF46A464FF00681AD700000000342E2539CD9241FFE6BE90FFDEAF
      77FFCE9247FFB76F37FFAE6F55FF5DA3CCFF2B97D1FF26C5F4FF4CBDDBE366D5
      F2FF1ECFF9FF54A3BFE330303039000000000000000000000000000000000000
      00002F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB27EFF65B481FF82C1
      97FF3A9F5AFF037E26FF00000000000000000000000000000000037E26FF3A9F
      5AFF82C197FF65B481FF5FB27EFF64B584FF69B788FF6DB98DFF93CDACFF2F9E
      61FF00000000000000000000000000000000037E26FF3A9F5AFF82C197FF65B4
      81FF5FB27EFF64B584FF69B788FF6DB98CFF6BB88BFF6CBA8DFF65B585FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF037E26FF00000000D29F57FFE0B481FFE3B8
      86FFD49A52FFBD783AFFA85933FF728B9BC670A5B8E386B9A3FF66B995FF69BA
      9BFF7DB7A0FF9FBCB8FF47464655000000000000000000000000000000000000
      000035A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB8FFF89C7A0FF44A4
      66FF098735FF0000000000000000000000000000000000000000000000000987
      35FF44A466FF89C7A0FF72BB8FFF8FCBA7FF90CBA9FF93CDACFF95CEAFFF35A2
      69FF0000000000000000000000000000000000000000098735FF44A466FF89C7
      A0FF72BB8FFF8FCBA7FF90CBA9FF93CDACFF95CEAFFF91CCAAFF8FCAA7FF72BB
      8FFF89C7A0FF44A466FF098735FF00000000000000005E4B2E71D9B389FFE5BE
      92FFD8A260FFC58540FFAC7E5CE37D8284AA3D8D6CFF40A873FF45C185FF4AC5
      92FF4AC38CFF47B275FF949696C6000000000000000000000000000000000000
      00003BA46DFF37A36DFF33A167FF309D61FF53AE7AFF90CBA9FF4DAA72FF188F
      46FF000000000000000000000000000000000000000000000000000000000000
      0000188F46FF4DAA72FF90CBA9FF53AE7AFF309D61FF33A167FF37A36DFF3BA4
      6DFF000000000000000000000000000000000000000000000000188F46FF4DAA
      72FF90CBA9FF53AE7AFF309D61FF33A167FF37A36DFF3BA46DFF53AE7AFF90CB
      A9FF4DAA72FF188F46FF00000000000000000000000027334E555C92E3FF8EB0
      D9FFA4AFC7FF8DA3D2FF4B6DD7FF525CB8E3559E82FF3F9D6EFF44BD81FF48C4
      8EFF4BC591FF48BF7CFF9AB4A7FF000000000000000000000000000000000000
      0000000000000000000000000000000000003BA26AFF58B280FF269755FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000269755FF58B280FF3BA26AFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002697
      55FF58B280FF3BA26AFF000000000000000000000000000000003BA26AFF58B2
      80FF269755FF0000000000000000000000000000000018191B1C4386F3FF3FA6
      F9FF3796F7FF296DF0FF2850E1FF3A49D1FF7DB09FFF3D9469FF42B67AFF47C2
      8BFF4BC693FF48C083FF70AD8CFF474646550000000000000000000000000000
      00000000000000000000000000000000000041A571FF2F9E63FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002F9E63FF41A571FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002F9E63FF41A571FF0000000000000000000000000000000041A571FF2F9E
      63FF000000000000000000000000000000000000000000000000446EBCC63E9D
      F8FF3BA1F9FF2D78F3FF2858E6FF273CD5FF8EACBEFF3B8D66FF41AD76FF46C1
      87FF4BC593FF49C289FF4DAB76FF898989AA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000233A69713B91
      F7FF3EA8FAFF3085F5FF285FEAFF2844DAFF7E89CFFF449474FF46A777FF4899
      73C64B8B70AA449973C652BE81FF63746EAA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000242836393682
      F4FF40A9FAFF3490F6FF2968EEFF284CDEFF4552CFFF294A3E710F15131C0000
      000000000000000000001419151C28302C390000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003659
      898E56A8EEFF3693ECFF2A6EE6FF3960C7E33B46687100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000711CEB0279
      1CFF000200040000000000000000000000000000000000000000000000000002
      000402791CFF00711CEB00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6D2D2FF867C
      7BFFA9A3A1FF0000000000000000000000000B0603144A2E18845B391EA3663F
      22B6714625C97A4C28DA84522CEB8C572EFA945F37FF99673FFF707070FF5A5A
      5AFF7A4C28D91D11093400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007826EB41A0
      5DFF006217CC0003010600000000000000000000000000000000000301060062
      17CC41A05DFF007826EB00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000006C5F5DFFB2ACABFF000000008D8382FFBDBA
      BAFF8F8584FF0000000000000000000000003C251368B78F6BFFD6B9A2FFDFC5
      B2FFE7D4C2FFEEDFD3FFF5EAE2FFB3B1AFFF676767FFD8D7D6FFA1A1A1FF8A8A
      8AFFA18975FF5B5B5BFE3F3F3FB9000000000000000000000000000000000000
      000000000000000000001F954FFD198F47FD138D41FD0E8939FD389E5CFF7EC0
      95FF44A260FF006519D100020007000000000000000000020007006519D144A2
      60FF7EC095FF389E5CFF0E8939FD138D41FD198F47FD1F954FFD000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF0000000000000000000000FF7D7271FFA29B9AFFA19A98FF958C8BFFA19A
      99FFC8C3C2FF00000000000000000000000052341B89C7A384FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFEFCFF9F9F9FFFC9C9C9FFA4A4A4FFCACACAFFC1C1
      C1FFA0A0A0FFC3C3C3FF6B6B6BFF000000030000000000000000000000000000
      00000000000000000000279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B5
      84FF81C196FF46A464FF00681AD7000300090003000900681AD746A464FF81C1
      96FF68B584FF87C49DFF89C5A0FF8CC8A4FF8FCAA8FF279A59FF000000000000
      000000000000000000000000000000000000000000000000000000000000FADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF837876FFF4F3F3FFACA5A4FFDBDADAFF7064
      62FF9D9593FFA69E9DFF000000000000000036231357976335F6B38457FFD9A4
      79FFD89D6DFFD79A68FFD49666FFB8987EFFC9C9C9FFBCBCBCFF9A9795FF9998
      97FFB4B4B4FFC1C1C1FF4141419C060606129D9D9DFF9D9D9DFF9D9D9DFF9D9D
      9DFF9D9D9DFF9D9D9DFF2F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF037E26FF037E26FF3A9F5AFF82C197FF65B4
      81FF5FB27EFF64B584FF69B788FF6DB98DFF93CDACFF2F9E61FF9D9D9DFF9D9D
      9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF000000000000000000000000FADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF857A78FFFBFBFBFFF6F6F6FFF1F1F1FFE3E3
      E2FFCDCBCBFF8B8280FF00000000000000000D0805146D4928ABD5AD8BFFFDF0
      E5FFF7C7A1FFF7CFACFFA4A4A4FFB6B6B6FFE1E1E1FF9B9B9BFFFAE4CCFFFDF5
      ECFF999796FFD5D5D5FF989898FF636363FF9D9D9DFFBABDBDFFE6ECECFFE6ED
      EDFFE7EDEDFFE8EEEEFF35A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB
      8FFF89C7A0FF44A466FF098735FF9C9D9DFF9C9D9DFF098735FF44A466FF89C7
      A0FF72BB8FFF8FCBA7FF90CBA9FF93CDACFF95CEAFFF35A269FFE8EEEEFFE7ED
      EDFFE6EDEDFFE6ECECFFBABDBDFF9D9D9DFF0000000000000000000000FFFADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF867A78FFFEFEFEFFFBFBFBFFF6F6F6FFDEDD
      DDFF9F9896FFDEDBDBFF0000000000000000000000001B110A28B68554FFFEFE
      FDFFFADEC1FFFADCBEFFB3B3B3FFCACACAFFE8E8E8FF858585FFFADCC2FFFDEB
      DEFF989897FFE2E2E2FFB5B5B5FF868686FF00000000909090EADBE2E2FFCDCF
      CFFFCBCBCBFFCCCCCCFF3BA46DFF37A36DFF33A167FF309D61FF53AE7AFF90CB
      A9FF4DAA72FF188F46FF979898F70000000000000000979898F7188F46FF4DAA
      72FF90CBA9FF53AE7AFF309D61FF33A167FF37A36DFF3BA46DFFCCCCCCFFCBCB
      CBFFCDCFCFFFDBE2E2FF909090EA000000000000000000000000000000FFFADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF867A78FFFFFFFFFFFEFEFEFFEFEEEEFF9C94
      93FFDDDBDAFF00000000000000000000000000000000150D071EB88550FFFEFC
      F9FFF9DCBEFFF8DBBEFFF8DCBFFFD5C7BAFFD2D2D2FFAAAAAAFF808080FF8989
      89FFB3B3B3FFCACACAFF5A5A5A9E0000000000000000909090EAD9E1E1FFCBCC
      CCFFC7C7C7FFC8C8C8FFCACACAFFCBCBCBFFCDCDCDFFCECECEFF3BA26AFF58B2
      80FF269755FFE5EBEBFF989898F70000000000000000989898F7E5EBEBFF2697
      55FF58B280FF3BA26AFFCECECEFFCDCDCDFFCBCBCBFFCACACAFFC8C8C8FFC7C7
      C7FFCBCCCCFFD9E1E1FF909090EA00000000000000000000000000000000FADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF867A78FFFFFFFFFFF1F0EFFF988F8EFFE6E4
      E4FF000000000000000000000000000000000000000006050209B88449FFFEFB
      F7FFF9DCC0FFF8DCBEFFF8DCBEFFB9B9B9FFDADADAFFBEBEBEFFD6D6D6FFD8D8
      D8FFB7B7B7FFD4D4D4FF878787FF0505050A00000000909090EAD7DFDFFFC7C9
      C9FFC2C2C2FFC4C4C4FFC5C5C5FFC8C8C8FFC9C9C9FFCACACAFF41A571FF2F9E
      63FFD0D1D1FFE3EAEAFF989898F70000000000000000989898F7E3EAEAFFD0D1
      D1FF2F9E63FF41A571FFCACACAFFC9C9C9FFC8C8C8FFC5C5C5FFC4C4C4FFC2C2
      C2FFC7C9C9FFD7DFDFFF909090EA00000000000000000000000000000000FADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF867A78FFEAE8E8FFA19997FFE5E3E2FFF6DA
      DAFF000000FF0000000000000000000000000000000000000000B78447F9FCF6
      F0FFF9DFC7FFF9DCBCFFFADCBEFFDECEBFFFBCBBBAFFECD5BFFFBFBFBFFFBDBD
      BDFFE9E7E6FFA9A39CFF6563639E0000000000000000909090EAD6DFDFFFC3C5
      C5FFC0C0C0FFC0C0C0FFC2C2C2FFC3C3C3FFC4C4C4FFC5C5C5FFC8C8C8FFC9C9
      C9FFCDCDCDFFE0E8E8FF989898F70000000000000000989898F7E0E8E8FFCDCD
      CDFFC9C9C9FFC8C8C8FFC5C5C5FFC4C4C4FFC3C3C3FFC2C2C2FFC0C0C0FFC0C0
      C0FFC3C5C5FFD6DFDFFF909090EA000000000000000000000000000000FFFADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF746866FFA49C9BFFDFDCDCFFF7DBDBFFF6DA
      DAFF000000FF0000000000000000000000000000000000000000A67941DAF5E7
      D8FFFAE5D2FFF9DABBFFF9DBBBFFFADBBEFFFADDC0FFFADDC0FFB9B8B7FFB3B3
      B3FFFFFDFBFFC89355FF0D0A04120000000000000000848484D6C7CDCDFFDBE4
      E4FFDBE4E4FFDBE5E5FFDCE5E5FFDCE5E5FFDDE6E6FFDDE6E6FFDEE7E7FFDFE8
      E8FFE0E8E8FFCDD3D3FF8C8C8CE300000000000000008C8C8CE3CDD3D3FFE0E8
      E8FFDFE8E8FFDEE7E7FFDDE6E6FFDDE6E6FFDCE5E5FFDCE5E5FFDBE5E5FFDBE4
      E4FFDBE4E4FFC7CDCDFF848484D6000000000000000000000000000000FFFADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFF5B4D4AFFE2E0DFFFF7DBDBFFF7DBDBFFF6DA
      DAFF000000000000000000000000000000000000000000000000946C3ABBF0D9
      C0FFFBEDE1FFF9DABFFFF9DCC1FFF9DEC4FFFAE0C7FFFAE2CAFFFAE2CDFFFAE5
      D0FFFFFEFDFFCB8E58FFBF8B4CF13628154500000000292929437B7B7BC89D9D
      9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D
      9DFF9D9D9DFF888888DD3030304E00000000000000003030304E888888DD9D9D
      9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF9D9D
      9DFF9D9D9DFF7B7B7BC82929294300000000000000000000000000000000FADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFFF9DDDDFFF8DCDCFFF7DBDBFF000000FF0000
      0000000000000000000000000000000000000000000000000000856235A4EDD0
      B1FFFFF6F0FFFAE1CAFFFBE3CCFFFBE3D0FFFBE6D3FFFBE9D5FFFCE9D8FFFCEA
      DBFFFFFFFDFFD29C6FFFEED9C0FFBA894BE50000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFFF9DDDDFFF8DCDCFF000000FFF7DBDBFF0000
      00FF0000000000000000000000000000000000000000000000007A5A3192EBCA
      A4FFFFFDFBFFFDE9D5FFFDEBD8FFFDEADBFFFDEDDFFFFDF0E2FFFDF1E4FFFCF0
      E4FFFFFFFFFFE09F6EFFFFFBF9FFDFB786FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FFFADE
      DEFFFADEDEFFF9DDDDFFF9DDDDFFF9DDDDFFF8DCDCFF00000000000000FF0000
      000000000000000000000000000000000000000000000000000070542E84EBC5
      99FFFFFFFFFFFCEFE2FFFDF0E7FFFDF1EBFFFDF5EEFFFDF8F1FFFDFAF7FFFFFC
      FAFFFFFFFFFFFEFBF7FFF4DABFFFC89552EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF0000000000000000000000FF000000FF0000000000000000000000000000
      00000000000000000000000000000000000000000000000000005E47266DEABF
      8BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDF9F4FFFBF3EAFFF8EBD9FFF8E6
      D3FFF5DFC5FFE9CBA5FFCE9B56ED513D215D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000030241336AE85
      48C6EABB80FFE8B675FFE6B16BFFE4AF66FFD4A158F0C99853E3B68B4CCFB489
      4ACCA57C44BB94713DA841321B4B040301050000000000000000000000000C3C
      4F5C000000000000000000000000000000000000000000000000000000000000
      0000032944570003050600000000000000000000000000000000000000003C3C
      3C5C000000000000000000000000000000000000000000000000000000000000
      00002D2D2D570303030600000000000000000B0603144A2E18845B391EA3663F
      22B6714625C97A4C28DA84522CEB8C572EFA945F37FF99673FFFA2724DFFAC81
      5DFF7A4C28D91D11093400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000026A3
      D4F424A1D3F4135B788C00000000000000000000000000000000083C5A6E0E7C
      BFEC0B7FCBFE000000000000000000000000000000000000000000000000A3A3
      A3F4A1A1A1F45B5B5B8C000000000000000000000000000000003F3F3F6E8181
      81EC888888FE0000000000000000000000003C251368B78F6BFFD6B9A2FFDFC5
      B2FFE7D4C2FFEEDFD3FFF5EAE2FFFBF4EFFFFDFAF6FFFFFEFDFFFBEBDFFFFBEF
      E6FFC09C7EFF4028166F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000249A
      C4E14ECBEFFF37B6E5FF23A2D6F915688AA3125A7B901A93CEF528A2DDFF37AD
      E5FF1082C7F60000000000000000000000000000000000000000000000009898
      98E1C7C7C7FFB5B5B5FFA3A3A3F9696969A35B5B5B90969696F5A5A5A5FFB0B0
      B0FF898989F600000000000000000000000052341B89C7A384FFD8EFF9FFD9EF
      F8FFFFFFFFFFFFFFFFFFFFFEFCFFFEFBF7FFFEF7F1FFFEF6F1FFE4AD85FFFAE8
      DBFFCEAE94FF53351C8B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000228A
      AEC74BCAEEFF52CEF1FF4EC8EFFF3EB9E8FF37B4E5FF44BCEBFF42B9EBFF3CB2
      E8FF1281BEE70000000000000000000000000000000000000000000000008989
      89C7C5C5C5FFCACACAFFC4C4C4FFB8B8B8FFB4B4B4FFBCBCBCFFBABABAFFB4B4
      B4FF868686E700000000000000000000000036231357976335F68B8F7DFF3AA5
      CFFF9E9E90FFD79A68FFD89561FFD6915BFFD48E57FFD38D55FFE29C67FFFAE3
      D1FFD8BAA1FF613F229E00000000000000000000000000000000AF7856FFAF78
      56FFAF7856FF00000000DD9AD9FFDD9AD9FFDD9AD9FF00000000B075FFFFB075
      FFFFB075FFFF0000000000000000000000000000000000000000000000001C73
      8FA347C8ECFF47CCF1FF2FC2EDFF40C3EEFF44C2EDFF2AB5EAFF24B0E8FF3AB3
      E7FF157AADCF0000000000000000000000000000000000000000000000007171
      71A3C3C3C3FFC8C8C8FFBDBDBDFFC0C0C0FFC0C0C0FFB4B4B4FFB0B0B0FFB4B4
      B4FF7D7D7DCF0000000000000000000000000D0805146D4928ABD5AD8BFF5EBC
      DFFF41B4E3FF68B0CBFFEBD1B6FFF8DABCFFF8DEC1FFFAE1C5FFFAE4CCFFFDF5
      ECFFE2CEB9FF714C28B300000000000000000000000000000000AF7856FFAF78
      56FFAF7856FF00000000DD9AD9FFDD9AD9FFDD9AD9FF00000000B075FFFFB075
      FFFFB075FFFF0000000000000000000000000000000000000000000000001659
      6D7C42C5EAFF55D2F2FF25C1EDFF21BCECFF1DB6EAFF19B2E9FF34B8EAFF38B2
      E6FF156C92AE0000000000000000000000000000000000000000000000005858
      587CC0C0C0FFCDCDCDFFBCBCBCFFB8B8B8FFB3B3B3FFB0B0B0FFB7B7B7FFB3B3
      B3FF6D6D6DAE000000000000000000000000000000001B110A28B68554FFF2FA
      FBFF2FADDDFF4FB9E6FF38A8D7FFA6C2C9FFF9DDC0FFFADBBFFFFADCC2FFFDEB
      DEFFECDCCDFF865B31CA00000000000000000000000000000000AF7856FFAF78
      56FFAF7856FF00000000DD9AD9FFDD9AD9FFDD9AD9FF00000000B075FFFFB075
      FFFFB075FFFF000000000000000000000000000000000000000002090B0C2EAB
      D0E957D5F2FF44D0F2FF27C3EEFF23BFEDFF20BAEBFF1BB5E9FF1FB3E9FF44BC
      EBFF209ED3F70312191D000000000000000000000000000000000909090CA7A7
      A7E9CFCFCFFFCACACAFFBDBDBDFFBABABAFFB6B6B6FFB2B2B2FFB2B2B2FFBCBC
      BCFF9F9F9FF71212121D000000000000000018576974256E819A70A09EFF8BD5
      ECFF3BB4DDFF7BD0F0FF6BC7ECFF42B1E2FF6FB3CFFFDBD3C3FFFADDC2FFFBE7
      D4FFF5EDE2FF9D6C3AE400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000010505062FABCCE348CE
      EDFF5CDBF5FF2ECDF1FF2AC8EFFF25C2EEFF22BDECFF1EB7EAFF1AB3E9FF47BF
      ECFF35B1E4FF209CD1F4020F1417000000000000000004040406A5A5A5E3C8C8
      C8FFD4D4D4FFC4C4C4FFC0C0C0FFBDBDBDFFB9B9B9FFB4B4B4FFB1B1B1FFBEBE
      BEFFB1B1B1FF9D9D9DF40F0F0F17000000002C9BB7CA6CD8F0FF66D3EFFF64CF
      EDFF83D9F3FF87D7F4FF7BCFF1FF56BBE8FF50B6E5FF209CD7FFA2BFC9FFF1DE
      CBFFFCF9F5FFB37F44FB01010001000000000000000000000000B1EBD0FFB1EB
      D0FFB1EBD0FF000000006BCC4EFF6BCC4EFF6BCC4EFF00000000EBAF5EFFEBAF
      5EFFEBAF5EFF0000000000000000000000000000000031AAC9DD4AD2EEFF61E1
      F6FF3FD6F4FF30CFF2FF2CCBF0FF28C5EFFF24C0EDFF20BBEBFF1DB6EAFF1EB3
      E9FF45BDECFF34B1E4FF209ACDEF010B0F1200000000A6A6A6DDCBCBCBFFD8D8
      D8FFCDCDCDFFC7C7C7FFC3C3C3FFBFBFBFFFBBBBBBFFB7B7B7FFB3B3B3FFB1B1
      B1FFBDBDBDFFB1B1B1FF9A9A9AEF0C0C0C120C2A323735B4D5EB8FE6F8FF8CE3
      F7FF5ED2F2FF83D7F4FF39B4DEFF84C0D1FF89C0D0FF81BCD0FF8EBDCEFFE1D7
      CAFFFFFCFAFFC08C51FF0B08030F000000000000000000000000B1EBD0FFB1EB
      D0FFB1EBD0FF000000006BCC4EFF6BCC4EFF6BCC4EFF00000000EBAF5EFFEBAF
      5EFFEBAF5EFF00000000000000000000000031A8C3D44BD5EFFF56DCF2FF5CDE
      F5FF5EDEF5FF5DDCF6FF46D4F3FF2BC9EFFF27C3EEFF33C3EDFF4EC9EFFF4AC3
      EEFF43BDEBFF3BB6E7FF31AFE2FF2198C9EBA1A1A1D4CDCDCDFFD3D3D3FFD6D6
      D6FFD6D6D6FFD5D5D5FFCDCDCDFFC1C1C1FFBDBDBDFFBEBEBEFFC5C5C5FFC1C1
      C1FFBCBCBCFFB6B6B6FFAFAFAFFF999999EB010506061E68798469D9F1FF95E7
      F8FF45CFF2FF88DCF4FF36B6E1FFA2CACDFFFADDC0FFFADDC0FFF9DDC3FFFBE1
      C8FFFFFDFBFFC89355FF0D0A0412000000000000000000000000B1EBD0FFB1EB
      D0FFB1EBD0FF000000006BCC4EFF6BCC4EFF6BCC4EFF00000000EBAF5EFFEBAF
      5EFFEBAF5EFF000000000000000000000000030A0B0C164C5961278499A730A7
      C4D636BCDEF547CEEDFF59D9F4FF45D3F2FF38CBF0FF53CFF1FF3CBEE8FF2BAC
      D9F62597C0DB1D7698AE104A606E0313191D0909090C4A4A4A617D7D7DA7A1A1
      A1D6B6B6B6F5C8C8C8FFD2D2D2FFCCCCCCFFC4C4C4FFCBCBCBFFBBBBBBFFABAB
      ABF6969696DB747474AE4A4A4A6E1313131D00000000081A1D2040C4E1FC92E7
      F8FF5DD8F4FF8FE0F6FF7BD6F2FF50C2E9FFA9CED1FFFAE2CAFFFAE2CDFFFAE5
      D0FFFFFEFDFFCB8E58FFBF8B4CF1362815450000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001D62748047CEEDFF5BD9F4FF57D6F3FF41C4EAFF1F7995A70000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005F5F5F80C8C8C8FFD2D2D2FFD0D0D0FFC0C0C0FF777777A70000
      00000000000000000000000000000000000000000000000203035AA0A0DA72DE
      F3FF88E3F6FF84DEF5FF80D9F4FF76D3F1FF4EC1E8FFAFD5D9FFFAE9D8FFFCEA
      DBFFFFFFFDFFD29C6FFFEED9C0FFBA894BE50000000000000000696DFEFF696D
      FEFF696DFEFF0000000071A9FFFF71A9FFFF71A9FFFF0000000065D5F0FF65D5
      F0FF65D5F0FF0000000000000000000000000000000000000000000000000000
      0000000000000000000034B8D9EE54D6F2FF57D6F3FF33BAE1FA010405060000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B2B2B2EECFCFCFFFD0D0D0FFB5B5B5FA040404060000
      0000000000000000000000000000000000000000000000000000697863AF59CD
      E0FF58D2EDFF56CCE6FF54C9E5FF52C5E4FF4DC2E3FF58C2E3FFE6EAE4FFFCF0
      E4FFFFFFFFFFE09F6EFFFFFBF9FFDFB786FF0000000000000000696DFEFF696D
      FEFF696DFEFF0000000071A9FFFF71A9FFFF71A9FFFF0000000065D5F0FF65D5
      F0FF65D5F0FF0000000000000000000000000000000000000000000000000000
      0000000000000000000014434F5746CEEDFF46CDEDFF1C617480000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000041414157C7C7C7FFC7C7C7FF5E5E5E80000000000000
      000000000000000000000000000000000000000000000000000070542E84EBC5
      99FFFFFFFFFFFCEFE2FFFDF0E7FFFDF1EBFFFDF5EEFFFDF8F1FFFDFAF7FFFFFC
      FAFFFFFFFFFFFEFBF7FFF4DABFFFC89552EA0000000000000000696DFEFF696D
      FEFF696DFEFF0000000071A9FFFF71A9FFFF71A9FFFF0000000065D5F0FF65D5
      F0FF65D5F0FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000032ACC8DB36BBDAEF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A5A5A5DBB4B4B4EF00000000000000000000
      00000000000000000000000000000000000000000000000000005E47266DEABF
      8BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDF9F4FFFBF3EAFFF8EBD9FFF8E6
      D3FFF5DFC5FFE9CBA5FFCE9B56ED513D215D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000B292F3312414A5200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000272727333E3E3E5200000000000000000000
      000000000000000000000000000000000000000000000000000030241336AE85
      48C6EABB80FFE8B675FFE6B16BFFE4AF66FFD4A158F0C99853E3B68B4CCFB489
      4ACCA57C44BB94713DA841321B4B040301050000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AE5516E28A3D10B303010004000000000000
      0000000000000000000000000000000000000000000000000000000000009A9A
      9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A
      9AFF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009A9A9AFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000083491CA4D68749FFD37D41FF813710AC000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009A9A9AFF9A9A9AFF9A9A9AFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000085512A9EDD965FFFE4B089FFE2AA80FFD0783DFF7D330FA90000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B52348EE3A373FFE9BA98FFE3AA80FFE0A375FFE1A87AFFD0753BFF762D
      0CA2000000000000000000000000000000000000000000000000000000000000
      000000000000000000009A9A9AFF9A9A9AFF9A9A9AFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF0000
      0000000000000000000000000000000000000000FFFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007855
      3C87E8AF86FFEDC1A2FFE8B691FFE3A97EFFE0A272FFDFA070FFE0A577FFCF72
      3AFF732B0DA00000000000000000000000000000000000000000000000000000
      00000000000000000000000000009A9A9AFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009A9A
      9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A
      9AFF000000000000000000000000000000000000FFFF0000FFFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007A594187E7A7
      7AFEEFC8ACFFEEC7A7FFECC0A0FFEABA98FFE7B48FFFE4AE86FFE1A77CFFDFA2
      73FFCC6F39FF6F260C9B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEA27AF4EAAA
      7EFFE6A677FFE29F6CFFE19660FFDB8D51FFD68341FFD27533FFCD6C21FFC960
      18FFC35716FFBF4C14FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      FFFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000196F7A000417190000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BF4C14FF6F260C9B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BF4C14FF6F260C9B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000029B0C3002EC6DA0011464E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C35716FFCC6F39FF732B0DA00000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C35716FFCC6F39FF732B0DA00000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000002EC6DA002EC6DA00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002F2F2FBF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF2F2F2FBF00000000C96018FFDFA273FFCF723AFF762D
      0CA200000000000000000000000000000000000000002F2F2FBF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF2F2F2FBF00000000C96018FFDFA273FFCF723AFF762D
      0CA2000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000228D9C0036E8FF000A2E33000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CD6C21FFE1A77CFFE0A577FFD075
      3BFF7D330FA90000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CD6C21FFE1A77CFFE0A577FFD075
      3BFF7D330FA90000000000000000000000000000000000000000000000000000
      0000000000000000000000269EAE002EC6DA0036E8FF0016606B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000004B4B4BE2383838B301010104000000000000
      00000000000000000000000000000000000000000000BE1B00BFFF2400FFFF24
      00FFFF2400FFFF2400FFBE1B00BF00000000D27533FFE4AE86FFDFA070FFE1A8
      7AFFD0783DFF813710AC0301000400000000000000002F2F2FBF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF2F2F2FBF00000000D27533FFE4AE86FFDFA070FFE1A8
      7AFFD0783DFF813710AC03010004000000000000000000000000000000000000
      00000000000000000000002EC6DA0036E8FF0036E8FF00196F7A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000414141A47C7C7CFF747474FF333333AC000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D68341FFE7B48FFFE0A272FFE0A3
      75FFE2AA80FFD37D41FF8A3D10B3000000000000000000000000000000000000
      000000000000000000000000000000000000D68341FFE7B48FFFE0A272FFE0A3
      75FFE2AA80FFD37D41FF8A3D10B3000000000000000000000000000000000000
      00000000000000000000002EC6DA0036E8FF0036E8FF00196F7A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004B4B4B9E8D8D8DFFAAAAAAFFA3A3A3FF707070FF303030A90000
      00000000000000000000000000000000000000000000BE1B00BFFF2400FFFF24
      00FFFF2400FFFF2400FFBE1B00BF00000000DB8D51FFEABA98FFE3A97EFFE3AA
      80FFE4B089FFD68749FFAE5516E200000000000000000000BEBF0000FFFF0000
      FFFF0000FFFF0000FFFF0000BEBF00000000DB8D51FFEABA98FFE3A97EFFE3AA
      80FFE4B089FFD68749FFAE5516E2000000000000000000000000000000000000
      0000000000000000000000269EAE0029B0C30029B0C30016606B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004E4E4E8E9C9C9CFFB5B5B5FFA3A3A3FF9C9C9CFFA0A0A0FF6D6D6DFF2B2B
      2BA2000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E19660FFECC0A0FFE8B691FFE9BA
      98FFDD965FFF83491CA400000000000000000000000000000000000000000000
      000000000000000000000000000000000000E19660FFECC0A0FFE8B691FFE9BA
      98FFDD965FFF83491CA400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005252
      5287A9A9A9FFBCBCBCFFB0B0B0FFA2A2A2FF9A9A9AFF999999FF9E9E9EFF6B6B
      6BFF282828A0000000000000000000000000000000002F2F2FBF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF2F2F2FBF00000000E29F6CFFEEC7A7FFEDC1A2FFE3A3
      73FF85512A9E000000000000000000000000000000002F2F2FBF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF2F2F2FBF00000000E29F6CFFEEC7A7FFEDC1A2FFE3A3
      73FF85512A9E0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000055555587A0A0
      A0FEC3C3C3FFC1C1C1FFBBBBBBFFB5B5B5FFAEAEAEFFA8A8A8FFA0A0A0FF9B9B
      9BFF696969FF2626269B00000000000000000000000000000000000000000000
      000000000000000000000000000000000000E6A677FFEFC8ACFFE8AF86FF7B52
      348E000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E6A677FFEFC8ACFFE8AF86FF7B52
      348E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000196F7A00196F7A00196F7A000E3A40000000000000
      00000000000000000000000000000000000000000000000000009D9D9DF4A4A4
      A4FF9F9F9FFF979797FF8E8E8EFF848484FF777777FF6B6B6BFF606060FF5656
      56FF4F4F4FFF484848FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000EAAA7EFFE7A77AFE78553C870000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EAAA7EFFE7A77AFE78553C870000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000002EC6DA0036E8FF0036E8FF00196F7A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DEA27AF47A594187000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DEA27AF47A594187000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000002EC6DA0036E8FF0036E8FF00196F7A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000002EC6DA002EC6DA002EC6DA00196F7A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C381C881664
      33F2176935FF166433F20C381C88000000000000000000000000000000000000
      0000103951F7265C84FB4685B9FB316A8EC10000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003BA46DFF35A269FF2F9E61FF279A
      59FF1F954FFE0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000160530007C23FC0008
      020F000000000000000000000000000000000000000000000000000000000208
      0F1A0000020300000000000000000000000000000000537D62FF268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C0000000000000000000000000208
      0F1A296280FB93C7F9FF90C9F9FF3F84C9FF195DA1F300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000035A16AFD95CEAFFF93CDACFF8FCA
      A8FF198F47FE0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000013032A00711DEA3A9F5AFF0685
      33FD0006030C0000000000000000000000000000000000000000082F4C872065
      9CFF124168AD01010406000000000000000000000000206E3DFF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F70000000000000000082F4C872065
      9CFF4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF3A81B9FD000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000319F64FD93CDACFF6DB98DFF8CC8
      A4FF138D41FE0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000110327006F1BE746A464FF82C197FF44A4
      66FF158D43FD0005020900000000000000000000000000000000165D96FF88B4
      DDFF538CBBFF1B486FAD0103040600000000000000002F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF0000000000000000165D96FF88B4
      DDFF9BB9CEFF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4996D7FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002D9B5EFD90CBA9FF69B788FF89C5
      A0FF0E8939FE0000000000000000000000000000000000000000000000000000
      00000000000000000000000F0221006C1AE444A260FF81C196FF65B481FF89C7
      A0FF4DAA72FF269755FF020602090000000000000000000000001B5B90EB6498
      C7FF9CC0E4FF6296C4FF275076AD010304060000000046885EFF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F700000000000000001B5B90EB6498
      C7FF9CC0E4FF95B7C9FF73B8D6FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4696
      DAFF000000000000000000000000000000004C301E5BC87A4BF0C77544F36F3F
      22870000000000000000339B65F52D985DF553AE7AFF8FCBA7FF64B584FF87C4
      9DFF389E5CFF007D28F500761EF5000000004C301E5BC87A4BF0C77544F36F3F
      2287000000000000000002791CFF41A05DFF7EC095FF68B584FF5FB27EFF72BB
      8FFF90CBA9FF58B280FF2F9E63FF000000000000000000000000050F18272B65
      9AEB74A3CEFFAACBE8FF73A1CCFF8C7F77DEB78A68E487A693FF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF0D391E8C0000000000000000050F18272B65
      9AEB74A3CEFFAACBE8FF98BDCFFF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4494D7FF020101020000000000000000CD895FEFE6B79BFFE9BEA4FFDC9A
      72FF6D402684000000002F9E63FF58B280FF90CBA9FF72BB8FFF5FB27EFF68B5
      84FF7EC095FF41A05DFF02791CFF00000000CD895FEFE6B79BFFE9BEA4FFDC9A
      72FF6D4026840000000000761EF5007D28F5389E5CFF87C49DFF64B584FF8FCB
      A7FF53AE7AFF2D985DF5339B65F5000000000000000000000000000000000811
      1B273A71A6EB85ADD5FFC7A790FFDFB99BFFF3DDC8FFF7E5D1FF89A594FF5B95
      70FF4D8D64FF42845AFF1639227D000000000000000000000000000000000811
      1B273A71A6EB85ADD5FFC7A790FFA3CCD9FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4796D5F90201010200000000D6976FF7EBC3ACFFECC8B1FFEAC0
      A8FFDEA07AFF75472C8C02060209269755FF4DAA72FF89C7A0FF65B481FF81C1
      96FF44A260FF006C1AE4000F022100000000D6976FF7EBC3ACFFECC8B1FFEAC0
      A8FFDEA07AFF75472C8C00000000000000000E8939FE89C5A0FF69B788FF90CB
      A9FF2D9A5EFC0000000000000000000000000000000000000000000000000000
      00000A141D27978E8CF7DFBA9CFFF7E4D2FFF4DBC1FFF3D6BAFFF3D8BCFFF5DE
      C8FFF8E8D9FFEDD0B6FF88735E97000000000000000000000000000000000000
      00000A141D27978E8CF7DFBA9CFFF7E4D2FF96BFCDFF7AD4EEFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF5E96C9F700000000654A3C75D99C76F9ECC7AFFFEDCA
      B4FFE9BDA2FFD28A5FFC0000000000050209158D43FD44A466FF82C197FF46A4
      64FF006F1BE7001103270000000000000000654A3C75D99C76F9ECC7AFFFEDCA
      B4FFE9BDA2FFD28A5FFC0000000000000000138D41FE8CC8A4FF6DB98DFF93CD
      ACFF319E64FC0000000000000000000000000000000000000000000000001D1D
      1D781F1F1F807C6655F3988E83FF998C80FF988A7BFF9E8F81FFF4DABFFFF4DB
      C1FFF6E1CCFFF7E7D7FFD2B294E3000000000000000000000000000000001D1D
      1D781F1F1F807C6655F3988E83FF998C80FF988A7BFF88B9C9FF7ED4EDFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF700000000684F3F75DFA785FEEBC7
      AFFFDDA07AFFBBA494FF5D5D5DAF3131315F29302B5D068533FD3A9F5AFF0071
      1DEA0013032A00000000000000000000000000000000684F3F75DFA785FEEBC7
      AFFFDDA07AFFBBA494FF5D5D5DAF3131315F198F47FE8FCAA8FF93CDACFF95CE
      AFFF35A06AFC0000000000000000000000000000000000000000000000001F1F
      1F8000000000D1A37CFDF8E6D3FFF3D7BCFFF3D9BDFF998C7EFFF4DCC2FFF5DD
      C5FFF5DFC8FFF9ECDFFFEECCABFD000000000000000000000000000000001F1F
      1F8000000000D1A37CFDF8E6D3FFF3D7BCFFF3D9BDFF998C7EFF88BDCEFF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E8000000000000000072594981D7A6
      89F9C3AEA2FFD5D5D5FFBABABAFFA5A5A5FF9F9F9FFF6D7570E6007C23FD0016
      053000000000000000000000000000000000000000000000000072594981D7A6
      89F9C3AEA2FFD5D5D5FFBABABAFFA5A5A5FF1F964FFF279A59FF2F9E61FF35A2
      69FF3BA36DFE000000000000000000000000000000001D1D1D781F1F1F802E2E
      2EC01F1F1F80887260FE9B928AFF9E9083FFF4DBC1FF998D81FFF5DEC7FFF5DF
      C9FFF6E1CCFFF9EDE1FFF2D1B1FD00000000000000001D1D1D781F1F1F802E2E
      2EC01F1F1F80887260FE9B928AFF9E9083FFF4DBC1FF998D81FFF5DEC7FFAAD4
      E3FF56A4D8FF84B0DBFF449CD0FF0F374D5E0000000000000000000000000101
      0102727272C5CBCBCBFFD2D2D2FFC9C9C9FFD2D2D2FFC5C5C5FF787878E80000
      0000000000000000000000000000000000000000000000000000000000000101
      0102727272C5CBCBCBFFD2D2D2FFC9C9C9FFD2D2D2FFC5C5C5FF787878E80000
      000000000000000000000000000000000000000000001F1F1F80000000001F1F
      1F8000000000C39C7CE3F5E2D0FF9A8F84FFF5DDC4FF998E83FFF5E0CAFFF6E1
      CCFFF7E7D6FFF9EBDEFFDBBFA4E300000000000000001F1F1F80000000001F1F
      1F8000000000C39C7CE3F5E2D0FF9A8F84FFF5DDC4FF998E83FFF5E0CAFFF6E1
      CCFFF7E7D6FFF9EBDEFFDBBFA4E3000000000000000000000000000000000000
      000032323255B1B1B1FFD6D6D6FF7B7B7BDA878787F5BFBFBFFF878787FD0000
      0000000000000000000000000000000000000000000000000000000000000000
      000032323255B1B1B1FFD6D6D6FF7B7B7BDA878787F5BFBFBFFF878787FD0000
      000000000000000000000000000000000000000000001F1F1F80000000001F1F
      1F8000000000846B5697ECCFB4FF9B948DFFF6E3D0FF9A8F84FFF6E2CEFFF7E7
      D6FFFAEEE3FFF8E2CCFF94826E9700000000000000001F1F1F80000000001F1F
      1F8000000000846B5697ECCFB4FF9B948DFFF6E3D0FF9A8F84FFF6E2CEFFF7E7
      D6FFFAEEE3FFF8E2CCFF94826E97000000000000000000000000000000000000
      000033333354B4B4B4FFE6E6E6FF8A8A8AEF646464AF5C5C5CA64F4F4F900000
      0000000000000000000000000000000000000000000000000000000000000000
      000033333354B4B4B4FFE6E6E6FF8A8A8AEF646464AF5C5C5CA64F4F4F900000
      000000000000000000000000000000000000000000001F1F1F80000000001818
      18681F1F1F801F1E1E81766659E06A635DFF9B938CFFACA59FFFF9EEE2FFFAEC
      DEFFF9E2CDFFBCA58EC00202010200000000000000001F1F1F80000000001818
      18681F1F1F801F1E1E81766659E06A635DFF9B938CFFACA59FFFF9EEE2FFFAEC
      DEFFF9E2CDFFBCA58EC002020102000000000000000000000000000000000000
      00001111111B8A8A8AE4E1E1E1FFD2D2D2FF646464AB00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001111111B8A8A8AE4E1E1E1FFD2D2D2FF646464AB00000000000000000000
      000000000000000000000000000000000000000000001F1F1F80000000000000
      0000000000000000000002020102645A50CBD7B89BE3F2D1B1FDF4D4B5FDDDC1
      A5E394827197020202020000000000000000000000001F1F1F80000000000000
      0000000000000000000002020102645A50CBD7B89BE3F2D1B1FDF4D4B5FDDDC1
      A5E3948271970202020200000000000000000000000000000000000000000000
      0000000000003C3C3C628E8E8EE8969696F95757579200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C3C3C628E8E8EE8969696F95757579200000000000000000000
      00000000000000000000000000000000000000000000181818681F1F1F801F1F
      1F801F1F1F801F1F1F801F1F1F80181818680000000000000000000000000000
      00000000000000000000000000000000000000000000181818681F1F1F801F1F
      1F801F1F1F801F1F1F801F1F1F80181818680000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A551DFA1A501BF10000
      000000000000000000000000000000000000060C0F12111A212E532A18819F49
      20E4B1562AF5B4562BF8AF522AF5A44820EC562C1C8D0F19212C070C0F120000
      00000000000000000000000000000000000000000000256ED5D52780FFFF1F7B
      FFFF1151AFAF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000164A19C71C5C1FFF030B03230000
      0000000000000000000000000000000000003873AAC55B75A2FFC16F4BFFFFB8
      60FFFFB961FFFFB55FFFFFB95FFFFFAF5AFFB46041FF597DAEFF3A6FA2C10000
      0000000000000000000000000000000000003273CCCC5FA3FFFF96C1FFFF93BF
      FFFF4F97FFFF1551AFAF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000150A06234828187D7D462BDB824A30F3814A30F375432CDB4327187D120A
      0623000000000000000000000000000000000000000000000000000000000000
      00000000000016471BA1206A25FC1F6722FF1E6421FF174B18C5000000000000
      000000000000000000000000000000000000298AE5FF92A9CAFFE9A969FFFFB4
      5EFFFEAA5BFFFEA155FFFE9C50FFFF994CFFEA8B52FF95ADCDFF2681DDFF0000
      0000000000000000000000000000000000004794FFFF9FC7FFFF82B6FFFF7DB3
      FFFF96C1FFFF5198FFFF1553AFAF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000331B
      1053894B2DE6B17E55FFD5B692FFDBC2A5FFDAC2A5FFD2B38FFFAA7850FF7846
      2DE62C1910530000000000000000000000000000000000000000000000000000
      00001E5B21C128772CFF66A06DFF69A171FF286F2CFF1F6622FF0C270E640718
      07411B581EF01B5A1EFF030B0323000000003E241874B28273FFFED7A7FFFFCA
      70FFFFC163FFFFB75CFFFFAA54FFFEA457FFFEB883FFAC705FFE382319760000
      000000000000000000000000000000000000519CFFFFA3CBFFFF8ABBFFFF75AF
      FFFF7EB4FFFF97C2FFFF549BFFFF1653ACAC0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000361F10539750
      2FF4CBA67BFFD8BA9EFFC29B75FFB58960FFB3855EFFBD9570FFD1B296FFC4A2
      75FF7D4A31F42C19105300000000000000000000000000000000000000002265
      25C11E7221FF93BF9CFF6AAE81FF4B9D65FF6EAF83FF73A578FF206923FF1F66
      22FF1E6020F80E2B0E74000000000000000001000001581D0284DB9174FDFFEC
      B7FFC5B175FFC7A96FFFC7A06AFFFFD09BFFD78467FC511B0180010000010000
      000000000000000000000000000000000000335C929277B2FFFFA4CCFFFF8CBC
      FFFF77B1FFFF82B5FFFF98C2FFFF569BFFFF1854ACAC00000000000000000000
      00000000000000000000000000000000000000000000170F0722985D2EE5CFA9
      80FFDABBA1FFBD9064FFB98B60FFB6885DFFB2835CFFB0825BFFAF825AFFCDA9
      8CFFC5A477FF7A462DE5120B07220000000000000000000000001B4C1E872B8B
      30FF75B481FF4B9D52FF77B28BFF4BA068FF44995EFF6FAF83FF347B37FF216C
      24FF0C290E640000000000000000000000000000000002000003591D0098808D
      A8FE659FDBFF71A3D8FF6AA3DCFF81899EFE561A019502000003000000000000
      00000000000000000000000000000000000000000000355E929279B3FFFFA5CC
      FFFF8DBDFFFF7AB2FFFF84B7FFFF9AC4FFFF579DFFFF1B55AAAA000000000000
      000000000000000000000000000000000000000000005A3A1A7EBE905CFFE0C1
      A7FFC4956AFFC19067FFE1CBB7FFFEFDFCFFFFFFFEFFEADCD0FFB3845CFFB284
      5CFFD4B498FFAD7954FF46271A7E0000000000000000000000002E8E33F560AC
      66FF75BD90FF429F5BFF499E51FF76B38BFF4CA168FF459B60FF6FB084FF689F
      6DFF226F25FF01070312000000000F30107C00000000000000002621267473A5
      D7FEB0D6F7FFB1DAFFFFB3D7F8FF79A2C8FEA47951EC48352164000000000000
      0000000000000000000000000000000000000000000000000000365E92927AB5
      FFFFA7CEFFFF8FBEFFFF8ABCFFFF9FC8FFFF5FA3FFFF2B69C5C9000000000000
      00000000000000000000000000000000000000000000A06A2CDBDBBB9BFFD5AC
      88FFC7976AFFC29467FFC09265FFEDDFD3FFFAF7F4FFBA8A61FFB88961FFB789
      60FFC49C76FFD2B792FF7B442CDB000000000000000000000000309734F850A8
      57FF6BBB8BFF4BA96EFF409E5AFF4AA152FF82BE95FF4FA26BFF459C61FF88BD
      98FF247528FF227126FE216E25FF206A24FF00000000000000000B325CA1A9C7
      E3FFA8CFF2FF84ABD2FF749FC9FF8DAFCFFF72675CD1D4B598FFAF8052F4513B
      267100000000000000000000000000000000000000000000000000000000395F
      92927CB6FFFFA8CEFFFFA7CDFFFF6EABFFFF5D99EEFF838383F7121212230000
      00000000000000000000000000000000000000000000B77831F6E3C7AEFFD0A1
      74FFC49869FFC39668FFC39567FFEEE0D4FFFBF7F4FFBE8F64FFBD8E63FFBD8E
      62FFBD9167FFDFC5A9FF8C4B31F60000000000000000277F2BC731A036FF319D
      36FF93CFABFF5AB37CFF4CAB70FF3FA05BFF4CA555FF84C097FF55A771FF85BD
      98FF287C2BFF237227F40E31116D000000000000000000000000052D66B74775
      AAFF588BBFFF255588FF09386BFF0C335FFF18314CA29E744ADCF5EEE7FFD5B7
      9CFFB18253F75A422A7D00000000000000000000000000000000000000000000
      00003960929280B6FFFF7AB3FFFF6DA6F2FFD2D2D2FFB4B4B4FF878787FE8484
      84FB828282FB6D6D6DD6383838700000000000000000BD7F37F6E4C9AFFFD0A2
      78FFCC9C6FFFC7996AFFC49769FFFFFFFFFFFFFFFEFFC29567FFC09366FFC193
      66FFC2976BFFDFC4AAFF8F4E31F60000000034A839FA34A939FF206924A23195
      37EA4EAC54FF8CCCA4FF5CB47EFF4DAC71FF40A25BFF4FA957FF99CBA8FF8DBE
      94FF27822CFF183B27AF00000000000000000000000000000000071A33710749
      95FF05488BFF17528CFF7B7168FFB58556FFB78656FFD5B79CFFFFFFFFFFFFFF
      FFFFF8F3EFFFD5B79CFF966F47D31A130B240000000000000000000000000000
      0000000000003B6092925285C6CF8F8F8FFBC7C7C7FFCCCCCCFFC7C7C7FFC5C5
      C5FFC2C2C2FFBFBFBFFF7E7E7EF53939397000000000AE7936DBE0BB9EFFDBB2
      92FFCF9F73FFCD9D70FFCB9B6FFFDDBEA2FFDDBEA1FFC49869FFC49869FFC397
      69FFD1AA84FFD8B996FF87482CDB0000000033A639F106180723000000000000
      000032A438FE37A43DFF8FCEA7FF60B883FF56B179FF5EB174FF36963CFF2788
      2BFF55A360FF1F433ADB00000000000000000000000000000000000102040A1B
      37810F4888F0837264FBDAC0A8FFFEFEFDFFFFFFFFFFFFFFFFFFFEEEE2FFFDE3
      D0FFFEF2E9FFFFFFFFFFDFC9B3FF9D734ADB0000000000000000000000000000
      00000000000000000000000000002A2A2A48919191FED4D4D4FFC8C8C8FFBBBB
      BBFFB9B9B9FFC1C1C1FFC3C3C3FF6F6F6FD6000000006547227ECD9B66FFE7CB
      B3FFD4A478FFD09F75FFCF9D72FFFBF8F5FFFBF8F5FFCB9D6FFFCB9C6FFFCDA0
      75FFDFBFA4FFB88959FF512C197E000000000000000000000000000000000D2C
      0E4134AA3AFF339E39F043AB49FF9DD4ACFFA3D7B7FF84C28DFF268E2BFF4DB6
      5DFF96C8A9FF2A8C2FFF29882EFF28842CFF0000000000000000000000000000
      00000906030CB58455FCFEFEFDFFFEF2E9FFFEECDFFFFDE9D9FFFDE7D6FFFDE6
      D3FFFDE4D0FFFCE4D1FFFFFFFFFFC8A17BFF0000000000000000000000000000
      000000000000000000000000000000000000939393FCDDDDDDFFC4C4C4FF6B6B
      6BC16B6B6BC3ABABABFFD7D7D7FF848484FB000000001C130922B97F3EE5D9B1
      8BFFE6CAB2FFD6A87BFFD1A477FFE2C3A7FFE1C2A7FFD0A174FFD1A375FFDDBC
      A1FFD0AB84FF985A2EE5160D07220000000000000000000000000000000033A8
      39F034AB3AF80000000033A138F133A738FF32A437FF32A037FF70C17CFFAEE3
      C1FF5D8B76FF152D227500000000000000000000000000000000000000000000
      00001610091EBF946AFFFFFFFFFFFEF0E5FFFEEEE2FFFEEBDDFFFDE9DAFFFDE8
      D7FFFDE6D4FFFDE4D1FFFFFFFFFFCDA988FF0000000000000000000000000000
      000000000000000000000000000000000000959595FBE4E4E4FFCFCFCFFF6E6E
      6EC2000000008D8D8DFF8B8B8BFF878787FB0000000000000000452F1853C686
      42F4D9B18BFFE6CDB7FFE0B99CFFD7AA84FFD6A881FFD9B290FFE1C1AAFFD4AD
      85FFA86531F439231053000000000000000000000000000000000000000038B6
      3EFF18521C74000000000000000034A739F8319D35ED2D5140A536814CF7319B
      38FE1F3B2E890000000000000000000000000000000000000000000000000000
      00000906030CB58455FCFEFEFDFFFEF6F0FFFEF2E9FFFEEFE4FFFEEEE1FFFEEC
      DEFFFDEADBFFFDEADCFFFFFFFFFFC79F79FF0000000000000000000000000000
      000000000000000000000000000000000000818181D6E2E2E2FFE7E7E7FFB8B8
      B8FF929292FF0000000000000000000000000000000000000000000000004631
      1853BD8240E6D09F68FFE0BE9FFFE3C4ADFFE3C4ADFFDFBB9EFFC89660FFA96F
      2EE63C2810530000000000000000000000000000000000000000000000000719
      092300000000000000000000000036B13CFF257829B209130D2214431665309C
      35F2000000000000000000000000000000000000000000000000000000000000
      0000000000007F5D3BB1DAC0A8FFFDFCFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFDFC9B3FF9D734ADB0000000000000000000000000000
      00000000000000000000000000000000000045454570949494F5E4E4E4FFEEEE
      EEFF959595FF0000000000000000000000000000000000000000000000000000
      00001E150A236849247DB47D40DBC38440F3C1843CF3AC7735DB62441E7D1B12
      0723000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000038B63DFF0D2D0F41000000001038125233A5
      38F7000000000000000000000000000000000000000000000000000000000000
      0000000000000D090512755738A5B28354F9B78656FFB78656FFB78656FFB786
      56FFB78656FFB78656FF84623EBA1A130B240000000000000000000000000000
      0000000000000000000000000000000000000000000045454570828282D69797
      97FB969696FB0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001D000000340000
      00360000003600000036000000360000003600000036000000360C275B95023C
      A1E60340BAFE023CA3E902205889000000000000000000000000000000000000
      0000103951F7265C84FB4685B9FB316A8EC10000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      629A2F2F2F4E0000000000000000000000000000000000000000000000001B1B
      1B4D1A1A1A4A0000000300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF8AA8DFFF2664CAFF1F75
      E6FF0477EAFF0062DDFF034BB9FD0020587A0000000000000000000000000000
      0000296280FB93C7F9FF90C9F9FF3F84C9FF195DA1F300000000000000000000
      0000000000000000000000000000000000000000000000000000000000003232
      324B919191E44A4A4A7C00000000000000000000000000000000363636804B4B
      4BC0181818450000000000000000000000000000000000000000000000000000
      00000000000000000000140F091C7D5B3AAE1F170E2C00000000000000000000
      0000000000000000000000000000000000000000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF1C57BFFF619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF023CA5E40000000000000000000000000000
      00004188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF3A81B9FD4554619E0000
      0000000000000000000000000000000000000000000000000000000000000000
      00007F7F7FBC9D9D9DF75E5E5E9C00000000000000004E4E4EA36D6D6DF34848
      48AE000000000000000000000000000000000000000000000000000000000000
      000000000000000000000906030CAD7F51F1B18253F7241A1032000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFF0441BBFFADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BAFE0000000000000000000000000000
      00009BB9CEFF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4996D7FF727E
      88EA000000000000000000000000000000000000000000000000000000000000
      000022222230AAAAAAFDA1A1A1FE767676C46E6E6EC4848484FE767676F40F0F
      0F24000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009B7249D9E2CEB9FFB28254F8231A10310000
      0000000000000000000000000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFAFAFF225AC1FF8CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF023AA0DE0000000000000000000000000000
      00000000000095B7C9FF73B8D6FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4696
      DAFF758089ED0000000000000000000000000000000000000000000000000000
      0000000000006A6A6A96AAAAAAFCE7E4E3FFE5E2E1FF8D8D8DFC464646870000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003D2D1C55AD7F51F1F9F6F1FFE2CDB8FFB68556FE9E74
      4ADC966F47D3684C30910B08040F000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFBFBFF92ADE1FF3B74D2FF8CB4
      F7FFB7D6FEFF70A7F5FF2B68C8FD021B4F6E0000000000000000000000000000
      00000303030A3A3A3A9E98BDCFFF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4493D7FE0A1C2C34000000000000000061452D6F593D27660D09050F0000
      0000000000000D0D0D12ABABABF3ABABABFEA1A1A1FE898989E50707070C0000
      00000000000000000000351D0E3F47281155000000000F0B06155D442B811711
      0A210000000061472D87CFAD8DFFEBDED1FFFDFCFAFFFDFCFAFFFDFBF8FFF4EB
      E3FFF1E7DDFFD8BDA2FFB08153F6513C26720000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFF91ACE0FF285F
      C7FF0441BBFF1D58C0FF01205695000000010000000000000000000000000303
      030A3A3A3A9E7D7D7DE7ADADADFFA3CCD9FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF398ACBE80C1A273000000000C28D63DBE4AD83FFD69564F60504
      01063624153E00000000787878A3DAD9D8FFDBD9D8FF676767A2000000002416
      092C04010005BE6C30E4D98547FFB2652DD500000000060502097F5E3CB38360
      3DB71B130D25A57A4EE8F8F1ECFFFDFBF8FFF8F0E7FFF7EFE6FFF7EEE4FFF8F0
      E8FFF8F1EAFFFDFBF9FFF2E9E0FF9B7249D80000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFAFAFAFFF9F9F9FFF6F6
      F6FFF6F6F6FFFCFCFCFF000000360000000100000000000000000303030A3A3A
      3A9E858585E7BDBDBDFFB4B4B4FFAAAAAAFF96BFCDFF7AD4EEFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF548FC2EC0C161D26DFA67BF9EDC4A6FFE2A373FF0806
      0309DF9C66FFCA8A59EA3028213CBBBBBBFBB1B1B1FA352D2648BE753DE1D783
      41FF08050209D57D3AFFE3A678FFCB7334F30000000000000000745536A1A898
      89BD84613EB8AE8052F3FAF7F2FFFAF3ECFFF8F1E9FFF8F0E7FFF7EFE6FFF7EE
      E4FFF6ECE2FFF7EFE7FFF4ECE5FFA0754BDF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF6F6F6FFF3F3
      F3FFF2F2F2FFFCFCFCFF0000003600000001000000000303030A3A3A3A9E8C8C
      8CE7CECECEFFC4C4C4FFB9B9B9FFAEAEAEFFA3A3A3FF88B9C9FF7ED4EDFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF7E0AD85F9EECAACFFE5A97CFF0302
      0003DA9B6CF6E3A675FF3726183F3C3C3C4E1F1F1F2A3623133FDD945BFFD083
      46F603020003D5813EFFE3A97BFFC87434F0000000002B20133C7A5939AAB6B1
      ACBDA89887BDB48455FBFAF6F1FFFAF4EEFFF9F2EAFFF8F1E9FFF8F0E8FFF7EF
      E6FFF7EEE4FFF8F1E9FFF3EAE2FF9B734ADA0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF2F2F2FFEFEF
      EFFFEDEDEDFFFCFCFCFF0000003600000001000000002B2B2B776C6C6CDCDEDE
      DEFFD2D2D2FFC7C7C7FFBABABAFFB0B0B0FFA4A4A4FF989898FF88BDCEFF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E8C5997AD8EECBADFFE5B088FF0806
      0509987051ABE7B48DFF956B49AA3727193F3726173F93633DAAE2A474FF925F
      36AB08050209D98846FFE2A678FFB26C33D548352164987E67BDACA398BDBCBA
      B9BDBCBAB9BDB6885AFBFAF6F2FFFAF5EFFFFAF4EEFFFAF3EDFFF9F3EBFFF9F2
      EBFFF8F1E9FFFAF5EFFFF3EAE2FF9B734ADA0000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF0000003600000001000000000303030A3A3A3A9E8C8C
      8CE7CECECEFFC4C4C4FFB9B9B9FFAEAEAEFFA3A3A3FF909090FA585858DAAAD4
      E3FF56A4D8FF84B0DBFF449CD0FF0F374D5E846A5690ECC7A8FFEABC9AFF3C2E
      244218120E1BC7946FDEE8B48EFFE4AA7DFFE3A678FFE4AB7EFFC08254DE1610
      091B38251542DD965DFFE09F6CFF764A278D7A5A3AACB7B3AEBDBCBAB9BDB7B1
      ABBDB7B1A9BDB8926CF0E4D1BDFFFAF6F2FFFAF6F1FFFAF6F1FFFAF5F1FFFAF5
      F1FFFAF5F0FFF6F0E9FFDCC3ACFF7C5B3AAD0000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFEBEBEBFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF000000360000000100000000000000000303030A3A3A
      3A9E858585E7BDBDBDFFB4B4B4FFAAAAAAFF979797FA5A5A5ADA232323620000
      00000000000000000000000000000000000028221C2DE8BB99FCEECCAFFFE5B5
      91FC1B16111F0B08060C775A4384B18360C6B0815BC6755339840B07040C1B13
      0B1FDA945CFCE5AB7EFFD88C50FC24170C2A7F5E3CB3B9B7B3BDB9B4AEBDB7B3
      ACBDB7B1ABBDB7A898CBB69069EEB7895BFBB7895CFBB7895BFBB28354F9AC7E
      51EFAC7E51EFA3784DE460462E861D150D280000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF5656569100000020000000000000000000000000000000000303
      030A3A3A3A9E7D7D7DE7ADADADFF9B9B9BFA5B5B5BDA23232362000000000000
      00000000000000000000000000000000000000000000856E5B90EEC9ADFFEFCA
      ACFFE6B894FC3C2F2542080605090000000000000000080603093A2A1D42DD9E
      6DFCE5AF85FFE2A778FF7A52318D000000007F5E3CB2B9B6B3BDB9B4AFBDB9B3
      ACBDB7B3ACBDB7B1ACBDB7B1A9BDB7AFA9BDB7B3ACBDB4ACA8BD745537A20000
      0000000000000000000000000000000000000000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF545454910000002000000002000000000000000000000000000000000000
      00000303030A3A3A3A9E727272E45B5B5BDA2323236200000000000000000000
      0000000000000000000000000000000000000000000006050406A88C75B7EECA
      ADFFF0CDB3FFEBC1A1FFE8B994FFE8B590FFE6B28AFFE5AE85FFE7B28AFFEABA
      98FFE6B087FFA0704DB705040106000000007F5E3CB2B9B6B3BDB9B6B1BDB9B4
      AFBDB9B4AFBDB9B4AEBDB9B3AEBDB7B3ACBDB9B6B1BDB4ACA8BD745537A20000
      0000000000000000000000000000000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000000000000000000
      0000000000000303030A3A3A3A9E232323620000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006957
      4972E3BB9BF6EFCDB0FFF1D0B7FFF1D3BAFFF0D0B8FFEDCBADFFEBBF9FFFDDA7
      7DF6654C3772000000000000000000000000684C3091A99B8CBDB9B6B3BDB9B6
      B3BDB9B6B3BDB9B6B3BDB9B6B3BDB9B6B1BDB6B1ACBDA3917EBD5C432C800000
      000000000000000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000001010105000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000231E1827816B5B8DBC9B81CCE8BC9CFCE8BB97FCBA9578CC7F64508D231C
      1527000000000000000000000000000000001D150D28664B2F8E7F5D3BB17F5D
      3BB17F5D3BB17F5D3BB17F5D3BB17F5D3BB179593AA9473422631610091E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C381C881664
      33F2176935FF166433F20C381C88000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000537D62FF268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C860
      18FFC55A17FFC35316FF8F3C0FBF000000000000000000000000000000006C1D
      0A9BB53711FF000000000000000000000000000000000000000000000000C860
      18FF74320D9B000000000000000000000000000000008B2F0EBFB83C12FFB638
      12FFB53711FF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000206E3DFF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CD69
      1CFFE1A779FFD8905CFF944111BF00000000000000000000000072260BA0C562
      31FFB63A12FF000000000000000000000000000000000000000000000000CD69
      1CFFD38040FF79370FA00000000000000000000000008D360FBFD27E4CFFD893
      5CFFB63A12FF0000000000000000000000000000000000000000000000000000
      000000000000000000000101010500000000000000002F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CF72
      2AFFE3AB82FFDB9663FF954812BF0000000000000000762D0CA2CA6936FFDA96
      61FFBA3F12FF000000000000000000000000000000000000000000000000CF72
      2AFFE3AB82FFD38042FF7D390FA200000000000000008F3C0FBFD48451FFDA96
      61FFBA3F12FF0000000000000000000000000000000000000000000000000000
      0000000000000303030A3A3A3A9E232323620000000046885EFF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F70000000000000000000000000000
      0000626262AE8A8A8AFF848484FF7E7E7EFF787878FF737373FF4B4B4BAE0000
      000000000000000000000000000000000000000000000000000000000000D279
      37FFE5B089FFDE9C6BFF9A4E15BF00000000813A0FA9D0753BFFDD9E6CFFDC9A
      68FFBC4814FF000000000000000000000000000000000000000000000000D279
      37FFE5B089FFE4AD85FFD68444FF833E11A900000000944111BFD68A55FFDC9A
      68FFBC4814FF0000000000000000000000000000000000000000000000000000
      00000303030A3A3A3A9E727272E45B5B5BDA2323236287A693FF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF0D391E8C0000000000000000000000000000
      00009A9A9AFFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FF757575FF0000
      000000000000000000000000000000000000000000000000000000000000D883
      43FFE8B590FFE1A375FF9C5720C0894514ACD38042FFE0A678FFDA9661FFDE9E
      6EFFBF5015FF000000000000000000000000000000000000000000000000D883
      43FFE8B590FFE3A87DFFE4B088FFD68649FF874110AC974812C0D9915CFFDE9E
      6EFFBF5015FF0000000000000000000000000000000000000000000000000303
      030A3A3A3A9E7D7D7DE7ADADADFF9B9B9BFA5B5B5BDA2323236289A594FF5B95
      70FF4D8D64FF42835AFE1539227C000000000000000000000000000000000000
      0000A3A3A3FFC2C2C2FFADADADFFADADADFFADADADFFC2C2C2FF7E7E7EFF0000
      000000000000000000000000000000000000000000000000000000000000D98A
      50FFE9BA97FFE6B28DFFC27033ECDE9D6BFFE4AD85FFDFA06FFFDB9660FFDFA2
      74FFC55716FF000000000000000000000000000000000000000000000000D98A
      50FFE9BA97FFE3A97DFFE3AA81FFE5B18AFFDD9864FFBD611AECDFA272FFDFA2
      74FFC55716FF00000000000000000000000000000000000000000303030A3A3A
      3A9E858585E7BDBDBDFFB4B4B4FFAAAAAAFF979797FA5A5A5ADA232323620000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ACACACFFC2C2C2FFFFFFFFFFFFFFFFFFFFFFFFFFC2C2C2FF878787FF0000
      000000000000000000000000000000000000000000000000000000000000DF91
      5AFFEBBF9DFFE9BA97FFD07F3FF8E1A171FFE6B28DFFE1A677FFDD9B69FFE1A7
      7CFFC86018FF000000000000000000000000000000000000000000000000DF91
      5AFFEBBF9DFFE6AF86FFE6B088FFE8B691FFDF9C6CFFC96D27F8E2AA80FFE1A7
      7CFFC86018FF000000000000000000000000000000000303030A3A3A3A9E8C8C
      8CE7CECECEFFC4C4C4FFB9B9B9FFAEAEAEFFA3A3A3FF909090FA585858DA2323
      2362000000000000000000000000000000000000000000000000000000000000
      0000B4B4B4FFC2C2C2FFADADADFFADADADFFADADADFFC2C2C2FF909090FF0000
      000000000000000000000000000000000000000000000000000000000000E19A
      63FFEDC3A4FFE8B38EFFA3673CBF875229A4DD9662FFE6B48FFFE2A87CFFE4AC
      84FFCD691CFF000000000000000000000000000000000000000000000000E19A
      63FFEDC3A4FFEAB996FFEABD9DFFDF9C67FF854D23A49D5A29BFE0A171FFE4AC
      84FFCD691CFF000000000000000000000000000000002B2B2B776C6C6CDCDEDE
      DEFFD2D2D2FFC7C7C7FFBABABAFFB0B0B0FFA4A4A4FF989898FF818181F54E4E
      4ED31010102D0000000000000000000000000000000000000000000000000000
      0000BDBDBDFFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FF9A9A9AFF0000
      000000000000000000000000000000000000000000000000000000000000E29F
      6EFFEFC8AAFFEAB895FFA76D43BF0000000085512A9EDF9964FFE7B48EFFE6B1
      8CFFCF722AFF000000000000000000000000000000000000000000000000E29F
      6EFFEFC8AAFFEEC2A5FFE5A578FF85542F9E00000000A16232BFE3A779FFE6B1
      8CFFCF722AFF000000000000000000000000000000000303030A3A3A3A9E8C8C
      8CE7CECECEFFC4C4C4FFB9B9B9FFAEAEAEFFA3A3A3FF909090FA585858DA2323
      2362000000000000000000000000000000000000000000000000000000000000
      0000919191BDBFBFBFFFBABABAFFB4B4B4FFAFAFAFFFA9A9A9FF797979BD0000
      000000000000000000000000000000000000000000000000000000000000E6A5
      75FFF0CBAFFFECBD9CFFA9734ABF0000000000000000774A298EDF9C67FFE7B3
      8FFFD27937FF000000000000000000000000000000000000000000000000E6A5
      75FFEFC8ACFFE8AF87FF7C54358E0000000000000000A3673CBFE4AD83FFE8B6
      93FFD27937FF00000000000000000000000000000000000000000303030A3A3A
      3A9E858585E7BDBDBDFFB4B4B4FFAAAAAAFF979797FA5A5A5ADA232323620000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E8AA
      7EFFF2CEB3FFEEC0A1FFA97752BF00000000000000000000000071482887D887
      48FED88343FF000000000000000000000000000000000000000000000000E8AA
      7EFFE7A579FE78553B87000000000000000000000000A76D43BFE7B28AFFEABB
      99FFD88343FF0000000000000000000000000000000000000000000000000303
      030A3A3A3A9E7D7D7DE7ADADADFF9B9B9BFA5B5B5BDA23232362000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000EAAA
      7EFFEAAA7EFFE8A87CFFAC7C57BF00000000000000000000000000000000724A
      2A87CF824BF4000000000000000000000000000000000000000000000000DEA2
      7AF47A59418700000000000000000000000000000000A9734ABFDF945EFFDD8F
      57FFD98A50FF0000000000000000000000000000000000000000000000000000
      00000303030A3A3A3A9E727272E45B5B5BDA2323236200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000303030A3A3A3A9E232323620000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000001010105000000000000000000000000000000000000
      00000000000000000000000000000000000016243039273E577A713925B0CB6E
      32FF153D52FF295D85FF4987BAFF62879DFF864530E3323A49801E384E5B0000
      00000000000000000000000000000000000016243039273E577A713925B0CB6E
      32FFD27C3FFFD37C3FFFD17A3FFFCD783DFF933D1DDF323A49801B5140B21664
      33F2176935FF166433F20C381C880000000016243039273E577A713925B0CB6E
      32FFD27C3FFFD37C3FFFD17A3FFFCD783DFF933D1DDF323A49801E4585A8023A
      A1DF0340BAFE023DA4E30020587A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003782C9E66F84ABFFC87C57FFFFB9
      64FF2C6481FF93C7F9FF90C9F9FF3F84C9FF2364A5FF757288FF4A9AE6FF0000
      0000000000000000000000000000000000003782C9E66F84ABFFC87C57FFFFB9
      64FFFFB65FFFFFAF5CFFFFB15BFFFFB35AFFDE8755FF4C6D57FF268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C3782C9E66F84ABFFC87C57FFFFB9
      64FFFFB65FFFFFAF5CFFFFB15BFFFFB35AFFDE8755FF4E63A0FF2463C9FF1F75
      E6FF0477EAFF0062DDFF034ABAFC0020587A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003281CFFC92ABCCFFE7AE71FFFFB7
      5DFF4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF3F84BDFF4893DCFF0000
      0000000000000000000000000000000000003281CFFC92ABCCFFE7AE71FFFFB7
      5DFFFEAF5DFFFEA659FFFD9F53FFFF964BFFFD924BFF1E6C3AFF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F73281CFFC92ABCCFFE7AE71FFFFB7
      5DFFFEAF5DFFFEA659FFFD9F53FFFF964BFFFD924BFF144DB5FF619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF023CA5E40000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000032170A55AC6950F5FDD7B1FFFFD6
      83FFA6A378FF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF408AC9F60713
      1D220000000000000000000000000000000032170A55AC6950F5FDD7B1FFFFD6
      83FFFFCA63FFFFBF5CFFFFB354FFFFA655FFFFC491FF2F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF32170A55AC6950F5FDD7B1FFFFD6
      83FFFFCA63FFFFBF5CFFFFB354FFFFA655FFFFC491FF0440BBFFADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BAFE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000041170064C36C4CF3FFE8
      BFFFACA985FF77948FFF72B6D4FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF3F8F
      D3F607111B200000000000000000000000000000000041170064C36C4CF3FFE8
      BFFFACA985FFA69D7BFF9F9173FFF7CA9BFFE49373FF438258FC8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F70000000041170064C36C4CF3FFE8
      BFFFACA985FFA69D7BFF9F9173FFF7CA9BFFE49373FF103EA0F28CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF023AA0DE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000441800796975
      90FA71ACE7FF80B0E3FF59A1D7FF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4195D6FF337856E640333A801E384E5B0000000000000000441800796975
      90FA71ACE7FF80B0E3FF7BB1E7FF6A97C3FF72532AFF549864FF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF1B5341B50000000000000000441800796975
      90FA71ACE7FF80B0E3FF7BB1E7FF6A97C3FF72532AFF3D7E90FF3772CFFF8CB4
      F7FFB7D6FEFF70A7F5FF2C69C9FE123D7BA10000000000000000000000000000
      0000626262AE8A8A8AFF848484FF7E7E7EFF787878FF737373FF4B4B4BAE0000
      0000000000000000000000000000000000000000000000000000211D1F6670A2
      D5FEABD1F2FFADD7FDFFAFD4F5FF70BAE4FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF429AD5FF548E6EFF4A9AE6FF0000000000000000211D1F6670A2
      D5FEABD1F2FFADD7FDFFAFD4F5FF91BFE7FF528288FF77CF93FF66B17DFF5896
      6EFF4D8D64FF408558FF438350FF4A9AE6FF0000000000000000211D1F6670A2
      D5FEABD1F2FFADD7FDFFAFD4F5FF91BFE7FF528288FF77CF93FF4B95A6FF185A
      B9FF0341BBFF0E50B0FF2F6A86FF4A9AE6FF0000000000000000000000000000
      00009A9A9AFFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FF757575FF0000
      0000000000000000000000000000000000000000000000000000163963ABA6BE
      DBFF95C4F1FFA0D0FBFF95C5F3FFB5CFE4FF378AB7FF7AD4EDFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF5B9ACAFF4C94DBFF0000000000000000163963ABA6BE
      DBFF95C4F1FFA0D0FBFF95C5F3FFB5CFE4FF3F78A6FF76CE8EFF73CD88FF6ECB
      82FF68CB79FF68C977FF5E955FFF4C95DEFF0000000000000000163963ABA6BE
      DBFF95C4F1FFA0D0FBFF95C5F3FFB5CFE4FF3F78A6FF76CE8EFF73CD88FF6ECB
      82FF68CB79FF68C977FF5E955FFF4C95DEFF0000000000000000000000000000
      0000A3A3A3FFC2C2C2FFADADADFFFFFFFFFFADADADFFC2C2C2FF7E7E7EFF0000
      000000000000000000000000000000000000000000000000000025416DC44E67
      99FF617EADFF7AA5D6FF5371A4FF415B91FF426597FF5CC7B4FF7CD5ECFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF629BCDFC000000000000000025416DC44E67
      99FF617EADFF7AA5D6FF5371A4FF415B91FF426597FF7AD09EFF75CE96FF6FCD
      8DFF70CD87FF9CD8AAFF6BAF6EFF1F3C2096000000000000000025416DC44E67
      99FF617EADFF7AA5D6FF5371A4FF415B91FF426597FF7AD09EFF75CE96FF6FCD
      8DFF70CD87FF9CD8AAFF6BAF6EFF1F3C20960000000000000000000000000000
      0000ACACACFFC2C2C2FFFFFFFFFFFFFFFFFFFFFFFFFFC2C2C2FF878787FF0000
      0000000000000000000000000000000000000000000000000000080E1843425E
      93FD3F598FFF3F598FFF3F598FFF3F598FFF6D8E9DFF839892FF5B9FA8FF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407FB4E90000000000000000080E1843425E
      93FD3F598FFF3F598FFF3F598FFF3F598FFF6D8E9DFF839892FF7B9388FF738B
      7DFFA3D5B0FF81BE84FF174D1CA00103010A0000000000000000080E1843425E
      93FD3F598FFF3F598FFF3F598FFF3F598FFF6D8E9DFF839892FF7B9388FF738B
      7DFFA3D5B0FF81BE84FF174D1CA00103010A0000000000000000000000000000
      0000B4B4B4FFC2C2C2FFADADADFFFFFFFFFFADADADFFC2C2C2FF909090FF0000
      0000000000000000000000000000000000000000000000000000000000000E17
      25613D5A8CF13F5B91FF405D93FB234452CC67738EFA71ACE7FF80B0E3FF63B0
      E4FF56A4D8FF84B0DBFF449CD0FF0F374D5E0000000000000000000000000E17
      25613D5A8CF13F5B91FF405D93FB234452CC67738EFA71ACE7FF80B0E3FF7BB1
      E7FF6A97C3FF1D461FB60104010B000000000000000000000000000000000E17
      25613D5A8CF13F5B91FF405D93FB234452CC67738EFA71ACE7FF80B0E3FF7BB1
      E7FF6A97C3FF1D461FB60104010B000000000000000000000000000000000000
      0000BDBDBDFFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FFC2C2C2FF9A9A9AFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000211D1F6670A2D5FEABD1F2FFADD7FDFFAFD4
      F5FF91BFE7FF2A3E58AD00000000000000000000000000000000000000000000
      0000000000000000000000000000211D1F6670A2D5FEABD1F2FFADD7FDFFAFD4
      F5FF91BFE7FF2A3E58AD00000000000000000000000000000000000000000000
      0000000000000000000000000000211D1F6670A2D5FEABD1F2FFADD7FDFFAFD4
      F5FF91BFE7FF2A3E58AD00000000000000000000000000000000000000000000
      0000919191BDBFBFBFFFBABABAFFB4B4B4FFAFAFAFFFA9A9A9FF797979BD0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000072D528DB7D0E7FF95C4F1FFA0D0FBFF95C5
      F3FFB5CFE4FF2972A6DF00000000000000000000000000000000000000000000
      0000000000000000000000000000072D528DB7D0E7FF95C4F1FFA0D0FBFF95C5
      F3FFB5CFE4FF2972A6DF00000000000000000000000000000000000000000000
      0000000000000000000000000000072D528DB7D0E7FF95C4F1FFA0D0FBFF95C5
      F3FFB5CFE4FF2972A6DF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000A35589232ADE5FF63B7E6FF8CC8F6FF39AC
      E6FF1BA6E5FF1F8CC6EF00000000000000000000000000000000000000000000
      00000000000000000000000000000A35589232ADE5FF63B7E6FF8CC8F6FF39AC
      E6FF1BA6E5FF1F8CC6EF00000000000000000000000000000000000000000000
      00000000000000000000000000000A35589232ADE5FF63B7E6FF8CC8F6FF39AC
      E6FF1BA6E5FF1F8CC6EF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000007121C431FA1DDFD17A5E5FF17A5E5FF17A5
      E5FF17A5E5FF133B569000000000000000000000000000000000000000000000
      000000000000000000000000000007121C431FA1DDFD17A5E5FF17A5E5FF17A5
      E5FF17A5E5FF133B569000000000000000000000000000000000000000000000
      000000000000000000000000000007121C431FA1DDFD17A5E5FF17A5E5FF17A5
      E5FF17A5E5FF133B569000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000091F2E61218EC7F119A4E4FF1E9D
      DAFB1343619D0101010600000000000000000000000000000000000000000000
      000000000000000000000000000000000000091F2E61218EC7F119A4E4FF1E9D
      DAFB1343619D0101010600000000000000000000000000000000000000000000
      000000000000000000000000000000000000091F2E61218EC7F119A4E4FF1E9D
      DAFB1343619D0101010600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D275C78023A
      A1DF0340BAFE023DA4E30020587A0000000000000000A37856C4CA9167F4D195
      66FFCE9161FFCB8D5CFFC98959FFC78654FFC28350FFC28350FFC28350FFC283
      50FFC28350FFC28350FF7F502CB0000000000000000000000000000000000208
      0F1A000002030000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000002
      071A000000030000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000004090B0D0A08
      08136A2E11988E371AE3A2411FF5A74320FAA64120FA5C4970FB2561C5FF1F75
      E6FF0477EAFF0062DDFF034ABAFC0020587A00000000D7A073FFF8F2EDFFF7F0
      EAFFF6EDE6FFF4EAE2FFF3E7DEFFF1E4DBFFF0E2D8FFF0E2D8FFF0E2D8FFF0E2
      D8FFF0E2D8FFF0E2D8FFC2885AFD000000000000000000000000082F4C872065
      9CFF124168AD0101040600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010F2A870326
      5CFF00173CAD0001010600000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000324C64792375C9FB835D
      5DFBBE5E33FFFEB85FFFFEB860FFFEB860FFFEB860FF1D50AEFF619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF023CA5E400000000D9A378FFF9F3EEFFEBD0
      BBFFE7C0A3FFE7BEA0FFE6BD9EFFE6BA99FFE3B794FFE1B48EFFDEAF88FFDCAA
      82FFD9AD88FFF0E2D8FFC58B5DFF000000000000000000000000165D96FF88B4
      DDFF538CBBFF1B486FAD01030406000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000012055FF457C
      BDFF194A87FF041D45AD00010306000000000000000000000000000000000000
      00000000000000000000000000000000000000000000277BD0FE81B9EEFF9E64
      56FFF5BA83FFFFAB59FFFEA758FFFEA155FFFE9B51FF0440BBFFADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BAFE00000000DDA77CFFF9F3EFFFEBD0
      B9FFFFFFFFFFFEFEFEFFFEFFFEFFFEFFFEFFFFFFFFFFFEFEFEFFFFFFFFFFFFFF
      FFFFDBAF8BFFF0E2D8FFC5895AFF0000000000000000000000001B5B90EB6498
      C7FF9CC0E4FF6296C4FF275076AD010304060000000000000000000000000000
      0000000000000000000000000000000000000000000000000000022155EB2557
      98FF5C8FCAFF235595FF082550AD000103060000000000000000000000000000
      000000000000000000000000000000000000000000002679CBFC76B2EAFFB29D
      93FFFFB65EFFFFB561FFFEB15FFFFEAB5BFFFEA457FF224EAAFF8CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF023AA0DE00000000DFA981FFF9F3EFFFEACE
      B6FFFFFFFFFFBFDDC2FF60AA67FFA0CDA4FFAAD2AEFF66AD6CFFAAD2AEFFFFFF
      FFFFDDB390FFF0E2D8FFC88C5DFF000000000000000000000000050F18272B65
      9AEB74A3CEFFAACBE8FF73A1CCFF8C7F77DEB78A68E4CE9E78FDD1A37CFDBF96
      76E37F665197020101020000000000000000000000000000000001050E27072A
      62EB3265A3FF6E9FD2FF3263A0FF56473DDE91512DE4A65F37FDAA653BFD9D60
      3AE36B4329970101010200000000000000000000000031190C50895242FFFCC8
      AAFFFFD197FFFEC76BFFFEBE66FFFEB862FFFEB05CFF94818AFF3B72CEFF8CB4
      F7FFB7D6FEFF70A7F5FF2B68C8FD021C4F6D00000000E1AD86FFFAF4F0FFEACB
      B1FFFFFFFFFFFFFFFFFF60AA67FFD5E9D7FF8BC090FFFFFFFFFF60AA67FFFFFF
      FFFFDFB795FFF0E2D8FFC38552FF000000000000000000000000000000000811
      1B273A71A6EB85ADD5FFC7A790FFDFB99BFFF3DDC8FFF7E5D1FFF7E5D3FFF5E1
      CDFFE8C9ADFFA88A70C002010102000000000000000000000000000000000107
      11270D3572EB4272AFFF986A4EFFC0845BFFE7BD9AFFEECCA8FFEECCACFFEBC4
      A2FFD29B72FF94623FC0020101020000000000000000010000013F190460C34A
      1DFFF6E4D6FFFFE4A3FFFFD470FFFFC967FFFFBF61FFFFB55DFF948F9FFF275B
      C0FF0440BAFF0A3FA3EE0120577A0000000000000000E3B08BFFFAF6F1FFEAC9
      ADFFFFFFFFFFFFFFFFFF60AA67FFD5E9D7FF95C79AFFF9FCF9FF6AB071FFFFFF
      FFFFE1BC9CFFF0E2D8FFC58553FF000000000000000000000000000000000000
      00000A141D27978E8CF7DFBA9CFFF7E4D2FFF4DBC1FFF3D6BAFFF3D8BCFFF5DE
      C8FFF8E8D9FFEDD0B6FF88735E97000000000000000000000000000000000000
      00000309152759504CF7C0865CFFEECAAAFFE9B990FFE7B186FFE7B489FFEBBF
      9AFFF0D2B6FFDBA780FF7B563A9700000000000000000000000005010007431C
      0769BB461AFFF4E2D4FF4C79A8FF4B79A7FF4B79A7FF4C79A8FFF3D6C2FFBD44
      1AFF441B076F0603010A000000000000000000000000E5B38EFFFAF6F2FFE9C5
      A9FFFFFFFFFFAAD2AEFF6AB071FFD5E9D7FFAAD2AEFF63AC6AFFAAD2AEFFFFFF
      FFFFE4C0A3FFF2E7DEFFC88957FF000000000000000000000000000000001D1D
      1D781F1F1F807C6655F3988E83FF998C80FF988A7BFF9E8F81FFF4DABFFFF4DB
      C1FFF6E1CCFFF7E7D7FFD2B294E3000000000000000000000000000000000707
      0778070707803C291CF3574C40FF594A3DFF574739FF5F4D3EFFE9B88DFFE9B9
      90FFECC4A0FFEED0B3FFC1895EE3000000000000000000000000000000000300
      0005422416A2326BA6FF9BCCF8FFAED4F7FFAED4F7FFA4CFF6FF3272ADFF4528
      1DAD0502000900000000000000000000000000000000E7B693FFFBF7F4FFE9C2
      A5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFE5C5AAFFF7F1EBFFCB8E5DFF000000000000000000000000000000001F1F
      1F8000000000D1A37CFDF8E6D3FFF3D7BCFFF3D9BDFF998C7EFFF4DCC2FFF5DD
      C5FFF5DFC8FFF9ECDFFFEECCABFD000000000000000000000000000000000707
      078000000000AA653BFDF0CEACFFE7B389FFE7B68AFF594A3CFFE9BB92FFEBBD
      97FFEBC09AFFF2D9C0FFDFA271FD000000000000000000000000000000000000
      000020446FC4A5CAEEFFAACCEAFFA6D0F6FFA7D0F6FFAACCEAFFA6CDEEFF234E
      7ACC0000000000000000000000000000000000000000E9B997FFFBF7F4FF63A3
      FFFF62A2FFFF60A1FFFF5FA0FFFF5D9EFFFF5A9CFFFF5899FFFF5597FFFF5294
      FFFF5093FFFFFBF7F4FFCE9262FF00000000000000001D1D1D781F1F1F802E2E
      2EC01F1F1F80887260FE9B928AFF9E9083FFF4DBC1FF998D81FFF5DEC7FFF5DF
      C9FFF6E1CCFFF9EDE1FFF2D1B1FD000000000000000007070778070707800B0B
      0BC007070780463122FE5B5047FF5F4E40FFE9B990FF594B3EFFEBBF98FFEBC0
      9BFFECC4A0FFF2DBC4FFE7AA7AFD000000000000000000000000000000000000
      00001B568FEDD9E8F7FF96C4F1FF8DBAE5FF7DA8D1FF88B4DFFFCDDFEEFF2060
      9DF10104050600000000000000000000000000000000EBBC9AFFFBF7F4FF62A3
      FFFF77BCFFFF73BAFFFF6FB8FFFF6BB7FFFF66B2FFFF5FAFFFFF58AAFFFF52A6
      FFFF397BFFFFFBF7F4FFD19668FF00000000000000001F1F1F80000000001F1F
      1F8000000000C39C7CE3F5E2D0FF9A8F84FFF5DDC4FF998E83FFF5E0CAFFF6E1
      CCFFF7E7D6FFF9EBDEFFDBBFA4E3000000000000000007070780000000000707
      078000000000A46841E3EBC7A7FF5A4D41FFEBBD95FF594C40FFEBC29DFFECC4
      A0FFEED0B1FFF2D7BFFFD39F75E3000000000000000000000000000000000000
      00000B3C86FF7A96B7FF89B6E4FF6F9BC8FF133E6CFF174270FF204369FF1038
      62FA0102040700000000000000000000000000000000ECBE9DFFFBF7F4FF63A3
      FFFF62A2FFFF5E9FFFFF5B9DFFFF5698FFFF5295FFFF4B8FFFFF458AFFFF4083
      FFFF3B7DFFFFFBF7F4FFD49A6DFF00000000000000001F1F1F80000000001F1F
      1F8000000000846B5697ECCFB4FF9B948DFFF6E3D0FF9A8F84FFF6E2CEFFF7E7
      D6FFFAEEE3FFF8E2CCFF94826E97000000000000000007070780000000000707
      078000000000724A2F97D9A57CFF5B534BFFECC9A7FF5A4D41FFECC7A3FFEED0
      B1FFF4DDC9FFF0C7A0FF8F6E5197000000000000000000000000000000000000
      00000E4996FF11569EFF0E4889FF0E4986FF104986FF134A84FF113F73FF0E2F
      55F10000000000000000000000000000000000000000DBB193EBFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFD19B6FF800000000000000001F1F1F80000000001818
      18681F1F1F801F1E1E81766659E06A635DFF9B938CFFACA59FFFF9EEE2FFFAEC
      DEFFF9E2CDFFBCA58EC002020102000000000000000007070780000000000606
      066807070780080707813B2C22E02A2420FF5B524AFF716860FFF2DDC7FFF4D9
      BFFFF2C7A2FFB78B68C002010102000000000000000000000000000000000000
      000007162F77104C94FE11569AFF115698FF105192FF0E4886FF0D3C6FFE0A16
      24810000000000000000000000000000000000000000765E507ED4AB8FE3EDBF
      9EFFEBBD9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB
      83FFDDA87EFFDCA47BFFAC805FCA00000000000000001F1F1F80000000000000
      0000000000000000000002020102645A50CBD7B89BE3F2D1B1FDF4D4B5FDDDC1
      A5E3948271970202020200000000000000000000000007070780000000000000
      000000000000000000000201010231261ECBCA9367E3E7AA7AFDEAAF81FDD7A2
      77E38F6E53970201010200000000000000000000000000000000000000000000
      0000000000000818327D114386F40F498FFF0E4689FF103E76F509192E840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000181818681F1F1F801F1F
      1F801F1F1F801F1F1F801F1F1F80181818680000000000000000000000000000
      0000000000000000000000000000000000000000000006060668070707800707
      0780070707800707078007070780060606680000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000691D099BB33111FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BF4C14FF6F260C9B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000691D099BB33111FF0000000000000000691D
      099BB33111FF000000000000000000000000000000000000000000000000BF4C
      14FF6F260C9B0000000000000000BF4C14FF6F260C9B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000070230BA0C35D2FFFB63712FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C35716FFCC6F39FF732B0DA0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000070230BA0C35D2FFFB63712FF0000000070230BA0C35D
      2FFFB63712FF000000000000000000000000000000000000000000000000C357
      16FFCC6F39FF732B0DA000000000C35716FFCC6F39FF732B0DA0000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000762D0CA2C96935FFDA9460FFB83E12FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C96018FFDFA273FFCF723AFF762D0CA20000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000762D0CA2C96935FFDA9460FFB83E12FF762D0CA2C96935FFDA94
      60FFB83E12FF000000000000000000000000000000000000000000000000C960
      18FFDFA273FFCF723AFF762D0CA2C96018FFDFA273FFCF723AFF762D0CA20000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000813C0FA9D0753BFFDD9E6CFFDC9A67FFBC4513FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CD6C21FFE1A77CFFE0A577FFD0753BFF7D330FA900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000813C0FA9D0753BFFDD9E6CFFDC9A67FFBC4513FFD0753BFFDD9E6CFFDC9A
      67FFBC4513FF000000000000000000000000000000000000000000000000CD6C
      21FFE1A77CFFE0A577FFD0753BFFCD6C21FFE1A77CFFE0A577FFD0753BFF7D33
      0FA9000000000000000000000000000000000000000000000000000000000000
      0000030201048A491AACD68446FFE1A87AFFDB9764FFDEA06FFFBF5015FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D27533FFE4AE86FFDFA070FFE1A87AFFD0783DFF813710AC030100040000
      0000000000000000000000000000000000000000000000000000030201048A49
      1AACD68446FFE1A87AFFDB9764FFDEA06FFFBF5015FFE1A87AFFDB9764FFDEA0
      6FFFBF5015FF000000000000000000000000000000000000000000000000D275
      33FFE4AE86FFDFA070FFE1A87AFFD27533FFE4AE86FFDFA070FFE1A87AFFD078
      3DFF813710AC0301000400000000000000000000000000000000000000000000
      000096582CB3DC945DFFE5B28AFFE0A375FFDC9965FFE1A578FFC55A17FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D68341FFE7B48FFFE0A272FFE0A375FFE2AA80FFD37D41FF8A3D10B30000
      000000000000000000000000000000000000000000000000000096582CB3DC94
      5DFFE5B28AFFE0A375FFDC9965FFE1A578FFC55A17FFE0A375FFDC9965FFE1A5
      78FFC55A17FF000000000000000000000000000000000000000000000000D683
      41FFE7B48FFFE0A272FFE0A375FFD68341FFE7B48FFFE0A272FFE0A375FFE2AA
      80FFD37D41FF8A3D10B300000000000000000000000000000000000000000000
      0000C07945E2E09C69FFE8B794FFE3AA80FFDFA070FFE3AC82FFC96319FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DB8D51FFEABA98FFE3A97EFFE3AA80FFE4B089FFD68749FFAE5516E20000
      0000000000000000000000000000000000000000000000000000C07945E2E09C
      69FFE8B794FFE3AA80FFDFA070FFE3AC82FFC96319FFE3AA80FFDFA070FFE3AC
      82FFC96319FF000000000000000000000000000000000000000000000000DB8D
      51FFEABA98FFE3A97EFFE3AA80FFDB8D51FFEABA98FFE3A97EFFE3AA80FFE4B0
      89FFD68749FFAE5516E200000000000000000000000000000000000000000000
      0000000000008C5A34A4E09F6FFFE9BA98FFE5AE85FFE6B18CFFCF7028FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E19660FFECC0A0FFE8B691FFE9BA98FFDD965FFF83491CA4000000000000
      0000000000000000000000000000000000000000000000000000000000008C5A
      34A4E09F6FFFE9BA98FFE5AE85FFE6B18CFFCF7028FFE9BA98FFE5AE85FFE6B1
      8CFFCF7028FF000000000000000000000000000000000000000000000000E196
      60FFECC0A0FFE8B691FFE9BA98FFE19660FFECC0A0FFE8B691FFE9BA98FFDD96
      5FFF83491CA40000000000000000000000000000000000000000000000000000
      000000000000000000008759369EE3A373FFE9BA98FFE8B894FFD47A38FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E29F6CFFEEC7A7FFEDC1A2FFE3A373FF85512A9E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008759369EE3A373FFE9BA98FFE8B894FFD47A38FFE3A373FFE9BA98FFE8B8
      94FFD47A38FF000000000000000000000000000000000000000000000000E29F
      6CFFEEC7A7FFEDC1A2FFE3A373FFE29F6CFFEEC7A7FFEDC1A2FFE3A373FF8551
      2A9E000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B52348EE5A578FFEABC99FFD98648FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E6A677FFEFC8ACFFE8AF86FF7B52348E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B52348EE5A578FFEABC99FFD98648FF7B52348EE5A578FFEABC
      99FFD98648FF000000000000000000000000000000000000000000000000E6A6
      77FFEFC8ACFFE8AF86FF7B52348EE6A677FFEFC8ACFFE8AF86FF7B52348E0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000076503487E09560FEDD8F57FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EAAA7EFFE7A77AFE78553C87000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000076503487E09560FEDD8F57FF0000000076503487E095
      60FEDD8F57FF000000000000000000000000000000000000000000000000EAAA
      7EFFE7A77AFE78553C8700000000EAAA7EFFE7A77AFE78553C87000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000077523787D7915FF40000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DEA27AF47A59418700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000077523787D7915FF400000000000000007752
      3787D7915FF4000000000000000000000000000000000000000000000000DEA2
      7AF47A5941870000000000000000DEA27AF47A59418700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C381C881664
      33F2176935FF166433F20C381C88000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000021212146626262D2727272FF6E6E6EFF696969FF3D674CFF268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002A2A2A4B6E6E
      6ECCA2A2A2FFC5C4C3FFD9D4D3FFD4CFCEFFD3CECDFF1F6D3CFF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000797979C9B2B2B2FFD4D4
      D4FFE1DFDFFFC4BEBCFFBCB4B2FFB8B0AFFFB8B0ADFF2F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A1A1A1FFF0F0F0FFE0E0
      E0FFD4D2D2FFBDB5B4FFBBB4B3FFBAB3B1FFB9B1B0FF46875EFF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEA27AF4EAAA
      7EFFE6A677FFE29F6CFFE19660FFDB8D51FFD68341FFD27533FFCD6C21FFC960
      18FFC35716FFBF4C14FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A7FFEAEAEAFFDDDD
      DDFFD5D4D3FFBFB8B6FFBDB6B5FFBCB5B3FFBAB3B2FF85A08BFF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF0D391E8C0000000000000000000000000000
      000000000000000000000A0A0A18676767EB0B0B0B1E00000000000000000000
      00000000000000000000000000000000000000000000000000007A594187E7A7
      7AFEEFC8ACFFEEC7A7FFECC0A0FFEABA98FFE7B48FFFE4AE86FFE1A77CFFDFA2
      73FFCC6F39FF6F260C9B00000000000000000000000000000000000000000000
      0000000000000000000000000000AE5516E28A3D10B303010004000000000000
      00000000000000000000000000000000000000000000AEAEAEFFEBEBEBFFDDDD
      DDFFD7D5D5FFC0BAB8FFBFB8B7FFBDB7B5FF39A040FF359335FF98B2A0FF5892
      6DFF4D8D64FF3B7C54F21539227C000000000000000000000000000000000000
      0000000000000E0E0E18858585F7949494FF717171F60D0D0D1E000000000000
      0000000000000000000000000000000000000000000000000000000000007855
      3C87E8AF86FFEDC1A2FFE8B691FFE3A97EFFE0A272FFDFA070FFE0A577FFCF72
      3AFF732B0DA00000000000000000000000000000000000000000000000000000
      0000000000000000000083491CA4D68749FFD37D41FF813710AC000000000000
      00000000000000000000000000000000000000000000B4B4B4FFECECECFFDEDE
      DEFFD9D8D8FFC2BCBAFFC1BAB9FFC0B9B8FF9FC8A3FF4EA854FFD7D3D1FF7C7C
      7CFF000000000000000000000000000000000000000000000000000000000000
      0000101010189B9B9BF4A8A8A8FFB1B1B1FF999999FF757575F60E0E0E210000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B52348EE3A373FFE9BA98FFE3AA80FFE0A375FFE1A87AFFD0753BFF762D
      0CA2000000000000000000000000000000000000000000000000000000000000
      00000000000085512A9EDD965FFFE4B089FFE2AA80FFD0783DFF7D330FA90000
      00000000000000000000000000000000000000000000BABABAFFECECECFFDFDF
      DFFFDAD9D9FFC5BEBDFFC3BDBBFFC2BBBAFFC0BAB8FFBFB8B7FFD7D3D3FF8383
      83FF000000000000000000000000000000000000000000000000000000000000
      0000B0B0B0F5BABABAFFBFBFBFFFB9B9B9FFB3B3B3FF9C9C9CFF666666CF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000085512A9EDD965FFFE4B089FFE2AA80FFD0783DFF7D330FA90000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B52348EE3A373FFE9BA98FFE3AA80FFE0A375FFE1A87AFFD0753BFF762D
      0CA20000000000000000000000000000000000000000BFBFBFFFECECECFFE0E0
      E0FFDDDCDCFFC7C0BFFF919090FF8E8E8EFF8C8C8CFF898989FFD9D5D4FF8A8A
      8AFF000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0FFBABABAFFB3B3B3FFABABABFFA2A2A2FF989898FF898989F60000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000083491CA4D68749FFD37D41FF813710AC000000000000
      0000000000000000000000000000000000000000000000000000000000007855
      3C87E8AF86FFEDC1A2FFE8B691FFE3A97EFFE0A272FFDFA070FFE0A577FFCF72
      3AFF732B0DA000000000000000000000000000000000C5C5C5FFEFEFEFFFE1E1
      E1FFDDDDDDFFC8C2C1FFC8C1C0FFC5C0BEFFC4BEBDFFC2BCBAFFDAD6D5FF9090
      90FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AE5516E28A3D10B303010004000000000000
      00000000000000000000000000000000000000000000000000007A594187E7A7
      7AFEEFC8ACFFEEC7A7FFECC0A0FFEABA98FFE7B48FFFE4AE86FFE1A77CFFDFA2
      73FFCC6F39FF6F260C9B000000000000000000000000CBCBCBFFE9E9E9FFE9E9
      E9FFE2E1E1FFCBC5C3FF979696FF959494FF939292FF919090FFDBD7D6FF9797
      97FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEA27AF4EAAA
      7EFFE6A677FFE29F6CFFE19660FFDB8D51FFD68341FFD27533FFCD6C21FFC960
      18FFC35716FFBF4C14FF00000000000000000000000039393948B6B6B6E4DFDF
      DFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3C2FFC8C2C0FFC7C0C0FFDCD9D8FF9D9D
      9DFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C0C0C0F8888
      88ABDCDCDCFFE6E3E3FFE1DEDCFFDFDCDCFFDFDCDBFFDEDBDBFFEEECECFFA4A4
      A4FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007F7F7F9FC6C6C6FBC3C3C3FFBFBFBFFFBABABAFFB5B5B5FFAFAFAFFF6E6E
      6EA7000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036000000360000
      003600000036000000330000001D000000000000000000000000000000000000
      0000112A1251235A26B72A6E2DE42C732EF52C732EF5286C2CE6027920FA0279
      1CFF000200040000000000000000000000000000000000000000000000000000
      000000000000000000000008080D001C1C31001A1A2F00040407000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFAFAFAFDEDEDEDF3000000330000000000000000000000000207030D2A65
      2EB637873CFB9AC49CFFB2C19CFFBCC09BFFBDC29CFFAFD2B0FF0A8732FF41A0
      5DFF06731FF2030C051C00000000000000000000000000000000000000000015
      15210063639D009595F000A0A0FF00AAAAFF00A6A6FF009494FF007F7FE7004A
      4A8A000B0B1600000000000000000000000000000000914D19C4B15919F4B658
      11FFB15209FFAC4C02FFA94500FFA64100FFA03C00FFA03C00FFA03C00FFA03C
      00FFA03C00FFA03C00FF651C00B0000000000000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFD0000003600000000000000000308030D397E3ED16EB6
      85FFADE8C7FFBFD29FFF20964FFF1A9047FF148E41FF0F8A39FF389E5CFF7EC0
      95FF44A260FF06781FF8030D051E000000000000000000000000002F2F4A009D
      9DF404CECEFFA2EDEDFFC5F4F4FFCCF5F5FFC5F4F4FFACEFEFFF69E2E2FF00B9
      B9FF007C7CE7001C1C35000000000000000000000000BF6925FFF4EAE2FFF2E7
      DEFFF0E2D7FFEDDED1FFECD9CBFFE9D4C5FFE7D1C0FFE7D1C0FFE7D1C0FFE7D1
      C0FFE7D1C0FFE7D1C0FFA24500FD000000000000000100000036FCFCFCFFFCFC
      FCFFDDDDDDFFDBDBDBFFD9D9D9FFD7D7D7FFD5D5D5FFD4D4D4FFD4D4D4FFD4D4
      D4FFFBFBFBFFFCFCFCFF00000036000000010000000034743BB174BF8CFF98D7
      B2FF77BF7EFFBBC17CFF279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B5
      84FF81C196FF46A464FF057620F5000300090000000000000000009A9AEA54DE
      DEFFC0F3F3FF45DBDBFF17D2D2FF45DBDBFF2BD6D6FF00BFBFFF00C9C9FF7DE6
      E6FF00C3C3FF007F7FEA000000000000000000000000C26E2CFFF5ECE4FFDFB7
      98FFFFFFFFFFDFB999FFFFFFFFFFFFFFFFFFFFFFFFFFDEA67CFFFFFFFFFFFFFF
      FFFFFFFFFFFFE7D1C0FFA44903FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFCFCFCFF00000036000000011631194650AB5AFBB4EAD3FF67BB
      72FF6CBC6FFFBDB56BFF2F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF007B23FB0000000000000000009D9DEAB1F0
      F0FF54DEDEFF21D4D4FF17D2D2FF45DBDBFF2BD6D6FF00BEBEFF00AFAFFF00B2
      B2FF54DEDEFF008383EA000000000000000000000000C97433FFF5ECE5FFDFB4
      92FFDFB493FFDFB493FFDFB493FFDFB493FFDFB696FFDEAF8AFFDEAF8AFFDEAF
      8AFFDEAF8AFFE7D1C0FFA44500FF000000000000000100000036FCFCFCFFFCFC
      FCFFD6D6D6FFD3D3D3FFCFCFCFFFCDCDCDFFCBCBCBFFC8C8C8FFC8C8C8FFC6C6
      C6FFF8F8F8FFFCFCFCFF00000036000000013D8146B291D7AEFF9FDEB3FF83C5
      6EFFA7D07EFFC4A45AFF35A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB
      8FFF89C7A0FF44A466FF088735FF215A24BF0000000000000000009F9FEABBF2
      F2FF69E2E2FF36D8D8FF26D5D5FF4ADCDCFF36D8D8FF00BFBFFF00B2B2FF00B6
      B6FF5EE0E0FF008989EA000000000000000000000000CC7839FFF5ECE5FFDEB1
      8DFFFFFFFFFFDFB493FFFFFFFFFFFFFFFFFFFFFFFFFFDEB292FFF8F0EAFFFFFF
      FFFFFFFFFFFFE7D1C0FFA74A03FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF9F9F9FFF9F9
      F9FFF8F8F8FFFCFCFCFF000000360000000152AA5DE4AFE9CFFF82D48FFFBEDC
      89FFC2CB81FFCCA154FF3BA46DFF38A26BFF33A065FF2F9C5FFF53AE7AFF90CB
      A9FF4DAA72FF168E44FFA0D8BBFF296D2CE8000000000000000000A0A0EAC0F3
      F3FF83E7E7FF4FDDDDFF3BD9D9FF5EE0E0FF45DBDBFF00C3C3FF00B7B7FF00BA
      BAFF64E1E1FF008D8DEA000000000000000000000000D07E41FFF7EDE7FFDEAC
      85FFDEAE87FFDEAE87FFDEAE87FFDEAE87FFDEB18DFFDBA67BFFDBA67BFFDBA7
      82FFDBA77EFFE7D1C0FFA13F00FF000000000000000100000036FCFCFCFFFCFC
      FCFFCECECEFFCACACAFFC6C6C6FFC3C3C3FFC0C0C0FFBDBDBDFFBCBCBCFFBABA
      BAFFF6F6F6FFFCFCFCFF00000036000000015BBB67F6BDEFDDFF71D17BFF8FD1
      6AFFBBE09DFFC7A65CFFD3AF5CFFC59851FFC5BB6CFFAED178FF39A162FF58B2
      80FF269755FF58A265FFB0E3CEFF2D7832F7000000000000000000A2A2EAC5F4
      F4FF92EAEAFF64E1E1FF4FDDDDFF6EE3E3FF54DEDEFF00C9C9FF00BCBCFF00BE
      BEFF69E2E2FF009090EA000000000000000000000000D38349FFF7F0E9FFDEA9
      7EFFFFFFFFFFDEA982FFFFFFFFFFFFFFFFFFFFFFFFFFDBA67BFFFFFFFFFFFFFF
      FFFFFFFFFFFFE9D6C5FFA43F00FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF6F6F6FFF3F3
      F3FFF2F2F2FFFCFCFCFF00000036000000015BBB68F5BDF0DCFF80D882FF75DB
      6BFFBEE599FFCCDFA6FFCAA75BFFC1BC6AFFB7DA8AFFA5D85EFF3AA666FF2F9E
      63FF57BB4EFF61AA6AFFB1E4CEFF317C35F6000000000000000000A4A4EACCF5
      F5FFA7EEEEFF7DE6E6FF69E2E2FF7DE6E6FF64E1E1FF00CDCDFF00C1C1FF00C1
      C1FF6EE3E3FF009292EA000000000000000000000000D6884DFFF7F0EAFFDCA4
      78FFDCA47BFFDEA67BFFDCA67CFFDCA97EFFDCA982FFDBA67BFFDCA982FFDBA7
      82FFDBAE8AFFEAD9CBFFA74500FF000000000000000100000036FCFCFCFFFCFC
      FCFFC7C7C7FFC2C2C2FFBEBEBEFFB8B8B8FFB4B4B4FFB1B1B1FFAEAEAEFFACAC
      ACFFEDEDEDFFFCFCFCFF000000360000000154AD60E2B2ECD2FF9AE2A1FF9CEA
      8CFFD4EDB6FFD0EAC7FFCFB86CFFCCB064FFCBC973FF74DB65FF64D94BFF63D7
      4BFF6AD35BFF71BA7CFFA4DBC1FF337A36E6000000000000000000A5A5EAD1F6
      F6FF98EBEBFF54DEDEFF2BD6D6FF26D5D5FF12D1D1FF00C2C2FF00BBBBFF00BF
      BFFF78E5E5FF009494EA000000000000000000000000D98D55FFF8F2EDFFDCA0
      71FFFFFFFFFFDBA176FFFFFFFFFFFFFFFFFFFFFFFFFFDBA67BFFFFFFFFFFFFFF
      FFFFFFFFFFFFF2E9DFFFAC4D03FF000000000000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF0000003600000001408449AD97DEB4FFB4EBCCFFB0EF
      A6FFC9EEA8FFD1EAC9FFD5CF8CFFD9CB8AFFCDB364FFBBBB65FF99D66FFF81DE
      71FF78DC6FFF90D0A2FF87C8A3FF2C672FB9000000000000000000A7A7EAD6F7
      F7FF83E7E7FFACEFEFFFD1F6F6FFEAFBFBFFE5FAFAFFB1F0F0FF54DEDEFF00C2
      C2FF7DE6E6FF009595EA000000000000000000000000DC925BFFF8F2EDFFDCA0
      71FFDCA071FFDCA071FFDCA071FFDCA071FFDCA071FFDCA071FFDCA071FFDCA0
      71FFDCA071FFF8F2EDFFB1540BFF000000000000000100000036FCFCFCFFF9F9
      F9FFC0C0C0FFBABABAFFB4B4B4FFAFAFAFFFAAAAAAFFA5A5A5FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF0000003600000001162E1A3C5DBF6AFABFF3E2FFB4EF
      B3FFB4F0ABFFC0EDB6FFD4E3B6FFD9D89BFFDAD394FFCDB46BFFC7B26BFFB4CB
      83FF93DF99FFAEE7CDFF43984AFC142D154F000000000000000000A9A9EADBF8
      F8FFF9FEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAFB
      FBFF92EAEAFF009898EA000000000000000000000000DF9660FFF8F2EDFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF8F2EDFFB65A13FF000000000000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF575757910000002000000000000000003D7E46A586D79FFFBFF2
      DEFFC7F2D6FFD5EFD5FFD0E9CFFFD5DBA5FFDCDEAAFFDBCD8FFFD7C88AFFC9C0
      8DFFBCD5AEFF78C790FF33743BB6000000000000000000000000008B8BBF64E1
      E1FFF9FEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAFB
      FBFF09CFCFFF006A6AA1000000000000000000000000E19965FFF8F2EDFF62BC
      70FF5BB969FF55B463FF4FB15BFF47AC54FF41A94CFF39A444FF33A03CFF2C9D
      36FF269830FFF8F2EDFFBA601BFF000000000000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF545454910000002000000002000000000000000001040105499653C48BD8
      A1FFCDF5E8FFD4EDDAFFCEEDD3FFCFDFAEFFD6DEB4FFD4D4A1FFCED0A0FFC3D0
      A9FF86C990FF428E4BD1040A040F000000000000000000000000001F1F2A0097
      97D10ED0D0FF88E8E8FFC0F3F3FFF4FDFDFFEFFCFCFFA2EDEDFF5EE0E0FF00C1
      C1FF008484C30012121B000000000000000000000000D29161EBF8F2EDFFF8F2
      EDFFF8F2EDFFF8F2EDFFF8F2EDFFF8F2EDFFF8F2EDFFF8F2EDFFF8F2EDFFF8F2
      EDFFF8F2EDFFF8F2EDFFBA6522F8000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000000000000000000000020302043D7C
      45A45DBD69F8A4E1BAFFB9EACCFFC4E0BDFFC4DAB3FFBCD7AFFFA5D7ABFF5ABA
      65FB3F8447B60309040C00000000000000000000000000000000000000000005
      0507003E3E55007E7EAE009A9AD600B3B3FB00B1B1F9009090CD007676A90030
      30460002020300000000000000000000000000000000714E357ECB8C5FE3E29B
      66FFDF9863FFDF955EFFDC9258FFD98D54FFD78A4FFFD48549FFD18042FFCE7B
      3CFFC97636FFC87031FF9B5623CA00000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000162E193D3F8249AA53AB5EDF5BBA67F45DBD69F857B263E942884BB21B36
      1D48000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A3A249C935E3BFF925E
      3AFF915D39FF915C38FF905B37FF8F5B36FF8F5A36FF8E5935FF8E5834FF8D58
      33FF623C23B10000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFF9EDE4FFF9EBE1FFF8E9
      DEFFF7E7DBFFF6E5D8FFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFF9EDE4FFF9EBE1FFF8E9
      DEFFF7E7DBFFF6E5D8FFCB926CFF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000825536E1CCAA84FFD1AE
      88FFD2AF87FFD1AC84FFD0A980FFCFA67DFFCEA379FFCDA078FFCB9E75FFC798
      6FFF925F3CFF312014A100000000000000000000000000000000000000000000
      000000000000000000005033159F664015E1623A11E15E3D1ABA000000000000
      000000000000000000000000000000000000CB926CFFFAEFE6FFF9EDE4FF0000
      00FFF8E9DEFFF7E7DBFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFAEFE6FFF9EDE4FFF9EB
      E1FFF8E9DEFFF7E7DBFFCB926CFF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000825537E1CAA884FFA276
      52FFC59965FFC39760FFC1935CFFC08F58FFBF8B54FFBD8851FFBE8753FFC99C
      73FF92603DFF493322E10000000000000000A27440FF9D6E3BFF976936FF9263
      31FF8C5E2CFF865827FF815222FF926637FF8F6334FF704314FF6B3E10FF673A
      0CFF633609FF5F3205FF5C2F02FF592C00FFCB926CFFFBF1E9FF000000FFF9ED
      E4FF000000FFF8E9DEFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFBF1E9FFFAEFE6FFF9ED
      E4FFF9EBE1FFF8E9DEFFCB926CFF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000825737E1CBAB87FFA57C
      55FFC99E6CFFC79C67FFC59864FFC2965FFFC1915AFFC08E57FFC08D59FFCAA0
      76FF93603EFF553C28FF0000000000000000AA7C48FFB28A5BFFB28A5BFFB28A
      5BFFB28A5BFFB28A5BFFB28A5BFF7B4F1FFF774A1BFFB28A5BFFB28A5BFFB28A
      5BFFB28A5BFFB28A5BFFB28A5BFF663A0DFFCB926CFFFBF3ECFFFBF1E9FFFAEF
      E6FFF9EDE4FF000000FFCB926CFF00000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF00000000CB926CFFFBF3ECFFFBF1E9FFFAEF
      E6FFF9EDE4FFF9EBE1FFCB926CFF00000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF000000000000000084573AE1CEAF8DFFA984
      59FFCBA373FFCAA16FFFC89D69FFC59A66FFC49861FFC1935CFFC3935FFFCCA4
      7BFF93613FFF553C28FF0000000000000000AF824DFF9D6E3BFF976936FF9263
      31FF8C5E2CFF865827FF815222FFAA8F72FFA98E71FF704314FF6B3E10FF673A
      0CFF633609FF5F3205FF5C2F02FF6A3D11FFCB926CFFFCF5EFFFFBF3ECFFFBF1
      E9FFFAEFE6FFF9EDE4FFCB926CFF00000000CB926CFFF9EDE4FFF9EBE1FFF8E9
      DEFFF7E7DBFFF6E5D8FFCB926CFF00000000CB926CFFFCF5EFFFFBF3ECFFFBF1
      E9FFFAEFE6FFF9EDE4FFCB926CFF00000000CB926CFFF9EDE4FFF9EBE1FFF8E9
      DEFFF7E7DBFFF6E5D8FFCB926CFF000000000000000085583AE1D0B391FFAE8A
      5EFFCEA87AFFCCA575FFCBA272FFC99F6CFFC79C68FFC59964FFC59A66FFCDA7
      81FF946240FF553C28FF0000000000000000B58752FFA47541FFF9F9F9FFF9F9
      F9FFF9F9F9FFF8F8F8FFF1F1F1FFE3E3E3FFF3F3F3FFF8F8F8FFF9F9F9FFF8F8
      F8FFE9E9E9FFF8F8F8FF603306FF6E4214FFCB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF00000000CB926CFFFAEFE6FFF9EDE4FF0000
      00FFF8E9DEFFF7E7DBFFCB926CFF00000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF00000000CB926CFFFAEFE6FFF9EDE4FFF9EB
      E1FFF8E9DEFFF7E7DBFFCB926CFF000000000000000086593BE1D2B695FFB391
      63FFD1AD82FFCFAA7DFFCDA778FFCBA474FFCAA16FFFC89D6BFFC89E6BFFD0AD
      85FF956342FF553C28FF0000000000000000B98C56FFAA7B47FFF9F9F9FFF1F1
      F1FFEDEDEDFFE7E7E7FFD3D3D3FFA4A4A4FFD7D7D7FFF4F4F4FFEEEEEEFFEAEA
      EAFFDCDCDCFFF8F8F8FF64370AFF724618FF0000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFBF1E9FF000000FFF9ED
      E4FF000000FFF8E9DEFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFBF1E9FFFAEFE6FFF9ED
      E4FFF9EBE1FFF8E9DEFFCB926CFF0000000000000000865A3CE1D5BA9AFFB799
      67FFD4B289FFD2AE86FFD0AC81FFCFA97BFFCDA576FFCBA372FFCCA373FFD2B0
      89FF966543FF553C28FF0000000000000000BD905AFFB0824CFFF9F9F9FFCCCC
      CCFFC9C9C9FFC4C4C4FFD3D3D3FFA5A5A5FFD8D8D8FFF4F4F4FFCACACAFFC5C5
      C5FFB9B9B9FFF8F8F8FF693C0FFF784B1CFF0000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFBF3ECFFFBF1E9FFFAEF
      E6FFF9EDE4FF000000FFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFBF3ECFFFBF1E9FFFAEF
      E6FFF9EDE4FFF9EBE1FFCB926CFF0000000000000000875B3DE1D7BE9EFFBC9E
      6DFFD4B48FFFCBAC87FFC7A780FFC5A47BFFC3A078FFC4A074FFCBA679FFD4B3
      8FFF976644FF553C28FF0000000000000000C1945EFFB68851FFFAFAFAFFF1F1
      F1FFEDEDEDFFE9E9E9FFD4D4D4FFA5A5A5FFD8D8D8FFF5F5F5FFEFEFEFFFEBEB
      EBFFDCDCDCFFF8F8F8FF6F4213FF7D5121FFCB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF00000000CB926CFFFCF5EFFFFBF3ECFFFBF1
      E9FFFAEFE6FFF9EDE4FFCB926CFF00000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF00000000CB926CFFFCF5EFFFFBF3ECFFFBF1
      E9FFFAEFE6FFF9EDE4FFCB926CFF0000000000000000885C3EE1D9C1A3FFC0A3
      70FFD2B793FFF5EDE4FFF4ECE2FFF4ECE1FFF4EBE0FFF3EADFFFCBA87DFFD6B7
      95FF986745FF553C28FF0000000000000000C59760FFBB8D56FFFAFAFAFFCCCC
      CCFFCACACAFFC4C4C4FFD4D4D4FFA6A6A6FFD8D8D8FFF5F5F5FFCBCBCBFFC7C7
      C7FFBABABAFFF8F8F8FF754818FF835626FFCB926CFFF9EDE4FFF9EBE1FFF8E9
      DEFFF7E7DBFFF6E5D8FFCB926CFF00000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF00000000CB926CFFF9EDE4FFF9EBE1FFF8E9
      DEFFF7E7DBFFF6E5D8FFCB926CFF00000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF0000000000000000885C40E1DBC4A5FFC5A6
      74FFD9BE9AFFD1B692FFCBAF8DFFCAAD89FFC8AA86FFCBAA82FFD1AF86FFD8BA
      9AFF986846FF553C28FF0000000000000000C59861FFC0925AFFFAFAFAFFF2F2
      F2FFEFEFEFFFEAEAEAFFD5D5D5FFB4B4B4FFDFDFDFFFF5F5F5FFF1F1F1FFECEC
      ECFFDEDEDEFFF9F9F9FF7B4E1EFF895C2BFFCB926CFFFAEFE6FFF9EDE4FF0000
      00FFF8E9DEFFF7E7DBFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFAEFE6FFF9EDE4FFF9EB
      E1FFF8E9DEFFF7E7DBFFCB926CFF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000895E40E1DDC8A8FFD4BC
      94FFE6D4B8FFE6D1B5FFE4D0B4FFE3CEB2FFE2CBAFFFE0C9ACFFDFC7A8FFDABE
      9FFF996948FF553C28FF0000000000000000C59861FFC4965EFFFAFAFAFFCDCD
      CDFFCACACAFFC5C5C5FFF2F2F2FFDDD8D2FFEBE5DFFFFCFCFCFFCCCCCCFFC9C9
      C9FFBCBCBCFFF9F9F9FF835524FF906131FFCB926CFFFBF1E9FF000000FFF9ED
      E4FF000000FFF8E9DEFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFBF1E9FFFAEFE6FFF9ED
      E4FFF9EBE1FFF8E9DEFFCB926CFF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000885D40E0D9C1A4FFB490
      6EFFAA7E5DFFA87C5CFFA87C5CFFA77B5AFFA77A59FFA67958FFA57757FF9B69
      47FF95623FFF553C28FF0000000000000000C59861FFC89961FFFAFAFAFFFAFA
      FAFFFAFAFAFFF8F8F8FFD9C5B0FF9F7545E69A7041E6D7C3AEFFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FF8A5B2AFF966736FFCB926CFFFBF3ECFFFBF1E9FFFAEF
      E6FFF9EDE4FF000000FFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFBF3ECFFFBF1E9FFFAEF
      E6FFF9EDE4FFF9EBE1FFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000007E573BCFB99676FFD8C4
      B8FFE8DED7FFE7DDD6FFE6DCD6FFE4DBD4FFE3DAD3FFE2D8D1FFE1D7D0FFEBE7
      E5FFEFEFEFFF553C28FF0000000000000000281F1333C99A62FFD3AD81FFD0AB
      7EFFCEA87BFFCBA579FFC59F72FF1F180D2D1F170D2DBA9468FFB79368FFB38F
      64FFAE8A60FFAA865CFF916230FF1E140A33CB926CFFFCF5EFFFFBF3ECFFFBF1
      E9FFFAEFE6FFF9EDE4FFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFFCF5EFFFFBF3ECFFFBF1
      E9FFFAEFE6FFF9EDE4FFCB926CFF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000593C2B919B6B49FEBA96
      76FFAB8160FFAB805FFFAA7E5EFFAA7D5DFFA97D5CFFA77C5BFFA77A5AFF9C6B
      4AFF966441FF875A3AFF000000000000000000000000281F1333281F1333281F
      1333271E1233271E1233261D11330000000000000000241A0F3323180E332217
      0D3320160C331F150B331E140A3300000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000CB926CFFCB926CFFCB926CFFCB92
      6CFFCB926CFFCB926CFFCB926CFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A110C2B835A3ED67952
      38C59C6A49FF9B6A48FF9B6947FF9A6847FF9A6846FF996745FF986644FF9765
      43FF926241FF724F34DE00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D275C78023A
      A1DF0340BAFE023DA4E30020587A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000D2B61802361C6FB1F75
      E6FF0477EAFF0062DDFF034ABAFC0020587A0000000000000000000000000000
      00000000000000000000000000000000000000000000691D099BB33111FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BF4C14FF6F260C9B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001D6A90A22BA6E0FF29A3DFFF27A0
      DDFF259DDCFF239ADAFF2197D9FF1E92D6FF198BD3FF044ABAFF619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF0242B1F50000000000000000000000000000
      00000000000000000000000000000000000070230BA0C35D2FFFB63712FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C35716FFCC6F39FF732B0DA0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002A99CAE3BDE3F5FFF4FCFEFFEFFB
      FEFFEEFBFEFFEEFBFEFFEFFCFEFFEFFCFEFFEFFBFEFF0441BBFFADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BBFF0000000000000000000000000000
      0000000000000000000000000000762D0CA2C96935FFDA9460FFB83E12FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C96018FFDFA273FFCF723AFF762D0CA20000000000000000000000000000
      00000000000000000000000000000000000000000000848484FF727272FF6A6A
      6AFF595959FF515151FF414141FF000000000000000000000000000000000000
      0000000000000000000000000000000000000E32434B60BEE8FFF4FCFEFFB4EF
      FAFF56DAF5FF56DAF5FF55D8F3FF56D7F2FF56D6F2FF0D56C0FF8CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF023EA7E80000000000000000000000000000
      00000000000000000000813C0FA9D0753BFFDD9E6CFFDC9A67FFBC4513FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CD6C21FFE1A77CFFE0A577FFD0753BFF7D330FA900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003218005CA45100FF0000
      00000000000000000000000000000000000000000000278AB4CA9CD7F1FFE7F9
      FDFF8AE5F8FF58DBF6FF59DAF4FF3BA0D5FF3BA0D5FF399ADDFF3773D2FF8CB4
      F7FFB7D6FEFF70A7F5FF2B69CAFE021C4F6D0000000000000000000000000000
      0000030201048A491AACD68446FFE1A87AFFDB9764FFDEA06FFFBF5015FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D27533FFE4AE86FFDFA070FFE1A87AFFD0783DFF813710AC030100040000
      000000000000000000000000000000000000000000007A7A7AFF6A6A6AFF6262
      62FF515151FF0000000000000000000000003218005CA45100FFA55100FF0000
      000000000000000000000000000000000000000000000719202432ADE1FBF3FB
      FEFFC2F2FBFF5ADCF6FF5ADAF4FF62DFF6FF55CBEBFF53D6F2FF399BDEFF1C5D
      C7FF0441BBFF044ABAFE0328668B000000000000000000000000000000000000
      000096582CB3DC945DFFE5B28AFFE0A375FFDC9965FFE1A578FFC55A17FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D68341FFE7B48FFFE0A272FFE0A375FFE2AA80FFD37D41FF8A3D10B30000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AD5100FFA55100FF9C4900FF9C49
      00FF944900FF3218005C02010004000000000000000000000000237799AA8BD2
      F0FFEAFBFEFF93E6F8FF5ADAF4FF45B0DDFF3BA0D5FF54D7F2FF5ADBF5FFDEF8
      FDFF7BBFE7FF0E4B708700000000000000000000000000000000000000000000
      0000C07945E2E09C69FFE8B794FFE3AA80FFDFA070FFE3AC82FFC96319FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DB8D51FFEABA98FFE3A97EFFE3AA80FFE4B089FFD68749FFAE5516E20000
      00000000000000000000000000000000000000000000727272FF626262FF5959
      59FF494949FF0000000000000000000000003218005CA45100FF9C4900FF0000
      00003218005C8C4100FF0000000000000000000000000000000003090B0C30A5
      D4ECC8EAF7FFE6FAFDFF5BDAF4FF3BA0D5FF3BA0D5FF55D7F2FFC7F3FCFFBFE3
      F4FF1F88C2E402070A0C00000000000000000000000000000000000000000000
      0000000000008C5A34A4E09F6FFFE9BA98FFE5AE85FFE6B18CFFCF7028FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E19660FFECC0A0FFE8B691FFE9BA98FFDD965FFF83491CA4000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003218005C9C4900FF0000
      000000000000844100FF0000000000000000000000000000000000000000123F
      515A6FC8EDFFF9FEFFFF5CDCF4FF3CA1D5FF3CA1D5FF5AD9F4FFEDFBFEFF66BA
      E5FF0D374C5A0000000000000000000000000000000000000000000000000000
      000000000000000000008759369EE3A373FFE9BA98FFE8B894FFD47A38FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E29F6CFFEEC7A7FFEDC1A2FFE3A373FF85512A9E00000000000000000000
      000000000000000000000000000000000000000000006A6A6AFF595959FF5151
      51FF414141FF383838FF282828FF000000000000000000000000000000000000
      000000000000844100FF00000000000000000000000000000000000000000000
      00002D97C0D4A8DFF4FFEDF9FDFF3CA2D6FF3CA2D6FFD4F5FCFFA1D7F1FF1F7C
      A9C3000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B52348EE5A578FFEABC99FFD98648FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E6A677FFEFC8ACFFE8AF86FF7B52348E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003218005C844100FF00000000000000000000000000000000000000000000
      00000A232C3036B4E6FDF9FDFFFF93E9F9FF9DEBFAFFECFAFEFF2DA6DEFC081F
      2A30000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000076503487E09560FEDD8F57FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EAAA7EFFE7A77AFE78553C87000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B55900FFAD5100FFA55100FF9C4900FF944900FF8C49
      00FF8C4100FF3218005C02010004000000000000000000000000000000000000
      0000000000002882A3B493D8F2FFF3FCFEFFE7FAFEFF8ED3F0FF1C6586960000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000077523787D7915FF40000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DEA27AF47A59418700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000040D111235AFDDF2F1FAFDFFD5EFFAFF30A5D5ED040C10120000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000216D889584D4F1FF81D1F0FF154A5F69000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000020303309AC3D42C8CB3C300020303000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008343
      09A9A95416B6100A001100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F07011182441399D96E1DFFD96C1AFFAC5614CC64300C77642F
      0C77C75C17EEC75B17EE7F390D990E0501110000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000874C
      07C3D7814BFFAC6B1CE932210338000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000058310F66E18E4AFFEBB082FFE3904CFFDA7121FFDA7226FFE087
      46FFE7A16EFFE3925AFFDB7331FF391A06440000000000000000000000000000
      000000000000000000000000000000000000040404040C0C0C0C131313131818
      18181B1B1B1B17171717111111110A0A0A0A000000000000000000000000864E
      09C3E9B158FFE8AE53FFB97419FB693B05A40000000000000000000000000000
      0000000000000000000000000000000000001D1D1D344A4A4AB8454545B84646
      46B7464646B7464646B7464646B7464646B7464646B7464646B7464646B74646
      46B7464646B7454545B84B4B4BB8151515340000000000000000000000000000
      0000000000003A220D44E28C42FFECB487FFDC782AFF0F0701111D0F0322BC5F
      19DDE6A36FFFDC7832FF3A1A064400000000040404070F0F0F121919191F2424
      242C2D2D2D373535353F3434333C3232323C2A2A2A3420201F283030303F0B0B
      0B0F00000000040404060404040700000001000000000000000000000000854C
      09C3DF8E20FFDE8E20FFE4A648FFE6A23AFF8C5008CB24120142000000000000
      000000000000000000000000000000000000696969AAAEAEAEFCACACACFFAEAE
      AEFFAEAEAEFFAEAEAEFFAEAEAEFFAEAEAEFFAEAEAEFFAEAEAEFFAEAEAEFFAEAE
      AEFFAEAEAEFFACACACFFA5A5A5FC4D4D4DAA0000000000000000000000000000
      00000000000000000000B26726CCE69857FFE49653FF66381277000000006635
      0E77E18B4AFFE18A47FF64310C77000000002B2B2BE2424242E3515151E35C5C
      5CE2646464E2444444DF282828CE404040DE646464E3606060E2606060E25454
      54E3454545E32B2B2BE52F2F2FF72D2D2DAF000000000000000000000000844B
      09C3DC8A23FFD77C0EFFDB8619FFE09D3EFFE4A547FFB36D14E5412303640000
      000000000000000000000000000000000000838383D7ACACACFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFACACACFF535353D70000000000000000000000000000
      000000000000000000000F090311A46025BBE59754FFE59451FFB06220CC924F
      19AAE69C61FFE8A36BFFAE5817CC00000000505050FFFFFFFFFFFFFFFFFFFCFC
      FAFFF4F4F3FFF2F2F1FFB0AFAFFFB0AFAFFFF2F2F1FFF4F4F3FFF4F4F3FFFCFC
      FAFFFFFFFFFF8A8A8BFFE1E1E0FF3C3C3CBE0000000000000000000000008348
      09C3D88523FFD3760EFFD57A10FFD77E13FFDB9230FFE1A54DFFC67E22FC683A
      0592000000000000000000000000000000007F7F7FD2ACACACFFFFFFFFFFA4A4
      A4FF515151FFFBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFACACACFF535353D20000000000000000000000000000
      000000000000388140E537843DFF2F7835FF7D7830FF92762CFCA45F21BBDF7C
      2AFFE3914EFFEBB285FFDC7526FF0F070111505050FFFFFFFFFFFFFFFFFFFAFA
      F8FFF6F5F5FFF0F0EFFFE3E3E2FFE3E3E2FFF0F0EFFFF6F5F5FFF6F5F5FFFAFA
      F8FFFFFFFFFF8A8A8BFFE1E1E0FF3B3B3BBD0000000000000000000000008147
      09C3D47E22FFCE6E0CFFD0720FFFD1740EFFD3760FFFD68622FFDD9E49FFDE98
      3BFF915109C21209001500000000000000007F7F7FD2ACACACFFFFFFFFFF9898
      98FF393939FFFBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFACACACFF535353D20000000000000000000000000000
      000000000000000000003D8D46F9BBB170FFE89F5CFFBB8035FE0F0902110000
      0000854A1A99E69D5FFFE1914CFF582F0D66505050FFFFFFFFFFE1E1E0FFE1E1
      E0FFE1E1E0FFF0F0EFFF8A8A8BFF8A8A8BFFF0F0EFFFE1E1E0FFE1E1E0FFE1E1
      E0FFFFFFFFFF8A8A8BFFE1E1E0FF383838B80000000000000000000000007F46
      07C3D37B20FFCD6D0CFFCF710EFFD0730FFFD1750FFFD3760EFFD57E19FFDB98
      3DFFE1A653FFC87227E9351C0340000000007F7F7FD2ACACACFFFFFFFFFF9A9A
      9AFF3D3D3DFFFBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFACACACFF535353D20000000000000000000000000000
      0000000000002A5A2F8A6FB578FFB5C48FFFE69A53FFE8A363FF79491D880000
      000077461C88E69E5FFFE59756FF67381377505050FFF6FDFFFFFFFFFFFFF9F8
      F9FFF6F5F5FFF0F0EFFFE3E3E2FFE3E3E2FFF0F0EFFFF6F5F5FFF6F5F5FFF9F8
      F9FFFFFFFFFF8A8A8BFFE1E1E0FF3C3C3CBE0000000000000000000000007E43
      07C3E19125FFE28E13FFE28E15FFE28F15FFE28F15FFE28F15FFE38E12FFE594
      1AFFE3A449FFBB6725F53C1D0348000000007F7F7FD2ACACACFFFFFFFFFF9A9A
      9AFF3D3D3DFFFBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFACACACFF535353D282431199BC5D17DDBB5C17DDD867
      18FF888D43F07EC487FFAFDCB5FF73B77AFF8A8D43FAE49548FFE8A25FFFE28A
      3AFFE69D5AFFE9A86DFFE49854FF59341366505050FFFFFFFEFFE1E1E0FFE1E1
      E0FFE1E1E0FFF0F0EFFF8A8A8AFF8A8A8AFFF0F0EFFFE1E1E0FFE1E1E0FFE1E1
      E0FFFFFFFFFF8A8A8BFFE1E1E0FF3B3B3BBD0000000000000000000000007C41
      07C3E79C27FFED9F17FFEC9E18FFEC9E18FFEC9E18FFEE9F16FFEEA220FFDE8C
      22FF924E07CC190D011E00000000000000007F7F7FD2ACACACFFFFFFFFFF9796
      97FF393939FFF8F8F8FFFFFFFFFFFDFDFCFFFDFDFDFFFDFCFCFFFDFCFCFFFDFD
      FDFFFCFDFCFFFFFFFFFFACACACFF535353D249260C55E08640FF743C0F880000
      00006F904AD987CB8FFF82C58AFF346C3BA8000200034E813FE569441E77A569
      2EBBA5662BBB975C25AA5A36156600000000505050FFFFFFFFFFFFFFFFFFF8F9
      F9FFF6F5F5FFF0F0EFFFE3E2E1FFE3E2E1FFF0F0EFFFF6F5F5FFF6F5F5FFF8F9
      F9FFFFFFFFFF8A8A8BFFE1E1E0FF383838B80000000000000000000000007B3E
      06C3EDA429FFF6AC1AFFF5AB1BFFF5AB1AFFF8B01BFFF2AA26FFC37219FF6B36
      069D020000030000000000000000000000007F7F7FD2ACACACFFFFFFFFFFA0A0
      A1FF505050FFF5F6F6FFFCFCFCFFFAF9F9FFF9F9FAFFFAF9F9FFFAF9F9FFF9F9
      F9FFF9F9F9FFFFFFFFFFACACACFF535353D20F070211DE7C2DFF924F19AA0000
      00000F0701118FA250FF819D4DF9000200030000000000000000000000000000
      000000000000000000000000000000000000505050FFFFFFFEFFE1E1E0FFE1E1
      E0FFE1E1E0FFF1F1F0FF8A8A8BFF8A8A8BFFF1F1F0FFE1E1E0FFE1E1E0FFE1E1
      E0FFFFFFFFFF8A8A8BFFE1E1E0FF373737BA0000000000000000000000007A3D
      06C3F3AC2AFFFFBA1DFFFEB81CFFFFBE21FFEFA62AFFAC5E13EC391D02490000
      000000000000000000000000000000000000848484D7ACACACFFFEFFFEFFFAFB
      FAFFFBFCFCFFFAF9FAFFFAFAF9FFFAF9FAFFFAF9FAFFFAFAFAFFF9FAFAFFF9FA
      FAFFFAFAFAFFFFFFFFFFACACACFF545454D700000000A45F23BBDF7C2CFF0000
      000000000000AF5B1BCCDE8238FF3A1C06440000000000000000000000000000
      000000000000000000000000000000000000505050FFFFFFFFFFFFFFFFFFFCFC
      F9FFF6F5F5FFF9F9F8FFEDECEBFFEDECEBFFF9F9F8FFF6F5F5FFF6F5F5FFFCFC
      F9FFFFFFFFFF8A8A8BFFE1E1E0FF3A3A3ABF000000000000000000000000783A
      06C3F3AC28FFFFBF1BFFFFBF26FFE39527FF8A450AC9140A001B000000000000
      000000000000000000000000000000000000696969AAACACACFFACACACFFACAC
      ACFFACACACFFACACACFFACACACFFACACACFFACACACFFACACACFFACACACFFACAC
      ACFFACACACFFACACACFFACACACFF4D4D4DAA00000000693F1A77E59A57FFA45F
      23BB77421788E3914EFFDD7D30FF1D0F05220000000000000000000000000000
      000000000000000000000000000000000000484848B35F5F5FCC757575F58080
      80F57B7B7BDA767676CC5B5B5BD5525252C1767676CC737373CC737373CC6A6A
      6ACC626262CC545454B65F5F5FDA2424247E0000000000000000000000007739
      05C3F3B236FFFBBA2DFFBE6D19FD642E04990201000200000000000000000000
      0000000000000000000000000000000000001F1F1F346D6D6DB86D6D6DB86D6D
      6DB76D6D6DB76D6D6DB76D6D6DB76D6D6DB76D6D6DB76D6D6DB76D6D6DB76D6D
      6DB76D6D6DB76D6D6DB8717171B817171734000000002D1C0C33E5934AFFA564
      28BB955823AAA45F23BB4A290E55000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000302020300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007739
      05C3E7B887FFA85B19EA32170145000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E28F3FFFC37A
      36DD000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007A4F
      2DAA945E34CD1208001900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005C3C1B66E69445FFE699
      4CFF1E1309220000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000008080819121214281D1D1D3205050507000000000000000000000000100D
      07343A250EA30C09042A00000000000000000000000000000000000000000000
      00000000000000000000000000004C47456AB8AFACE94E45437F020202030000
      0000000000000000000000000000000000000B0603144A2E18845B391EA3663F
      22B6714625C97A4C28DAAC6130FAC28356FFD38A66FFE18E6EFFDC8C6AFFDA8A
      6BFFD7896CFFCD8A6AFFAA6B42FFA55D2CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000101010B0707
      0849636364C1B8B8B9E6BABABBF67D7D7DB3565656893939394F0808080C0E0D
      0D35774922CF603915B80E09052E000000000000000000000000000000000000
      0000000000000000000000000000756C6CB3E3D9D6FF6F605DC00604040B0000
      0000000000000000000000000000000000003C251368B78F6BFFD6B9A2FFDFC5
      B2FFE7D4C2FFEEDFD3FFC58253FFEFCEB9FFDDFFFFFF86EEC7FFA1F4D7FFA1F6
      D7FF8BEEC7FFE0FFFFFFDDA184FFAA683CFF0000000000000000000000000000
      0000000000000105010A00020007000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000309090A541313
      139DB8B8B9F6FFFFFFFFF5F5F5FFE7E7E8FFE1E1E1FFC0C0C0F98C8E90D9605D
      58C8976D48E9B96A28E5724619C6241507380000000000000000000000000000
      00000000000000000000000000006A6060AAD6CAC8FF5B4B49B6040303080000
      00000000000000000000000000000000000052341B89C7A384FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFC27D4FFFEFB599FFEAF3E8FF4FBE83FF6DC997FF6FC9
      98FF52BE83FFE4F4E9FFDD9B79FFA96738FF0000000000000000000000000000
      00000105010A35823BFF2F7734FB000200070000000000000000000000000000
      00000000000000000000000000000000000000000002050505200B0B0C5F0C0C
      0D7CA2A2A2EAFDFDFDFFF1F1F1FFF4F4F4FFF4F4F4FFF4F4F3FFEFF1F2FFDBD9
      D5FFC49E7AFFD4A06EFFDEAA70FF71522FCE0000000000000000000000000000
      0000000000000000000000000000625857A6D2C5C3FF554746C2020202090000
      00000000000000000000000000000000000036231357976335F6B38457FFD9A4
      79FFD89D6DFFD79A68FFC38052FFEAB596FFF3F3EAFFEDF1E6FFEFF1E6FFEFF0
      E6FFEDF1E5FFF3F5EDFFD59B77FFAF6E42FF0000000000000000000000000306
      030A3E8D45FF52A25AFF4D9E55FF307A36FE0104020800000000000000000000
      00000000000000000000000000000000000002020310080808360606073D0808
      095A9D9D9EE5FEFEFEFFF2F2F2FFF1F1F1FFF1F1F1FFF2F2F2FFE6E7E9FFC4C1
      BCFFC6A582FFDCAE7DFFE6BE92FF806447DE0000000000000000000000000000
      000000000000000000001915152FAB9D9DE4D6C9C8FF857575F81A16164A0000
      0000000000000000000000000000000000000D0805146D4928ABD5AD8BFFFDF0
      E5FFF7C7A1FFF7CFACFFC98A5FFFE6B491FFE2A680FFE1A680FFDEA27BFFDCA0
      79FFDB9E77FFD99D75FFD49971FFBA7C55FF00000000000000000306030A4799
      4FFF59AB62FF75CA81FF72C87CFF4F9F57FF317B37FE01040208000000000000
      000000000000000000000000000000000000010101060202020C000000090404
      0431A3A3A5E5FFFFFFFFF8F8F8FFF6F6F6FFF5F5F5FFF1F1F1FFE3E4E6FFBDBB
      B9FFD2B797FFE3B687FFE8BF8FFF7F6343DB0000000000000000000000000000
      000000000000030303067F7374B9EADCDDFFB3A8A8FFA99D9DFF695C5CDB0504
      040F00000000000000000000000000000000000000001B110A28B68554FFFEFE
      FDFFFADEC1FFFADCBEFFCA8C63FFEAB798FFDDA47CFFDDA57EFFDBA27AFFD99F
      78FFD99F77FFD89E76FFD89D76FFBE835BFF000000000307030A4FA558FF61B4
      6BFF7CCE88FF79CC86FF74CA80FF74C980FF50A158FF327C38FE010402080000
      0000000000000000000000000000000000000000000000000000000000000202
      020F585859ADF8F8F8FFFCFCFCFFFBFBFBFFFCFCFCFFF2F2F3FFDCDEDFFFC2B8
      A8FFE6CCAFFFEBCAA2FFEDC695FF816646DB0000000000000000000000000000
      0000000000004840417BEEE3E3FFE8DDDDFFA39999FFDCD1CFFFBAABAAFF4035
      36A50101010700000000000000000000000000000000150D071EB88550FFFEFC
      F9FFF9DCBEFFF8DBBEFFC8875BFFEFBEA0FFFDFCFAFFFEFCFBFFFEFDFDFFFEFD
      FCFFFDFBFAFFFDFCFBFFDDA784FFC07D51FF0206030957AF61FF69BC74FF83D2
      8FFF78C984FF5EB168FF61B36BFF76C982FF76CB81FF51A25AFF327C38FD0204
      0208000000000000000000000000000000000000000000000000000000000000
      000012121256DDDDDDFDFFFFFFFFFBFBFBFFFDFDFDFFF6F6F6FFE0E0E1FFC6B7
      A3FFF3D9B9FFEFD6B8FFF3D3AAFF836747DB1311112C787271B17F7675B92420
      1F531511113ACBBEBFF7FFFDFCFFC1B7B6FFAAA09FFFE5D9D8FFDBCDCCFF8675
      75FB231D1D610000000000000000000000000000000006050209B88449FFFEFB
      F7FFF9DCC0FFF8DCBEFFC78559FFEFBF9DFFFFFFFFFFCC926CFFFFFFFFFFFFFF
      FFFFFFFBF7FFFFF8F1FFE4AE8BFFC7895FFF19331C464FA159E477C985FF7ECE
      8CFF4EA357FC2046246F27542B8B5AAC65FF7ACC85FF77CB84FF52A35BFF337E
      39FC020402080000000000000000000000000000000000000000000000000000
      00000E0E0E55DFDFDFFDFFFFFFFFFCFCFCFFFDFDFDFFF6F6F6FFE1E2E2FFCCBC
      ABFFF6DEC0FFF2DCBFFFF9E2C2FF846A4DDB4D4242A96864627C54504F624E41
      41AD807070EDFBF1F1FFF4EBE9FFA39897FFB4A8A7FFE9DDDCFFDACBC9FFAB9A
      99FF655556F10D0A0A2A00000000000000000000000000000000B78447F9FCF6
      F0FFF9DFC7FFF9DCBCFFCC8C63FFF3CDAFFFFFFFFFFFE3C7B2FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFEABEA0FFC9885EFF00000000162C183C51A35AE66BBF
      77FF244A286F000000000000000029592D915CAD66FF7BCD88FF7ACD86FF54A4
      5DFF347E3AFC0204020800000000000000000000000000000000000000000000
      000010101057E1E1E1FDFFFFFFFFFDFDFDFFFEFEFEFFF7F7F7FFE2E2E3FFCDBD
      ACFFFEE8CBFFF6E3C7FFFDEACFFF877157DB362E2C890605050805050507392F
      2F91D4C4C4FFFFFCFCFFDBD6D4FFA39896FFB9ACACFFEFE4E3FFD5C6C4FFB2A1
      9FFF7B6969FF3B2F31BB01010106000000000000000000000000A67941DAF5E7
      D8FFFAE5D2FFF9DABBFFD4966CFFD49D79FFD0976FFFD6A381FFCD8D66FFCD8F
      67FFD09973FFD19871FFC88A60FF24120636000000000000000018301C432348
      27650000000000000000000000000000000029592F915DAE67FF7DCE89FF7CCE
      88FF55A55EFF357F3BFC02040208000000000000000000000000000000000000
      000012121257E2E2E2FDFFFFFFFFFDFDFDFFFFFFFFFFF8F8F8FFE3E3E4FFCEC1
      B0FFFFEDD3FFFCEAD0FFFFF0D7FF88765DDB352F2E72706969BD706564C0A998
      98FFF8EEEEFFFEFEFEFFCDC9C8FFAEA19FFFCABFBDFFFCF8F7FFF5F0EFFFD9CF
      CFFF8E7A7AFF5D4C4DFF1E17177E000000000000000000000000946C3ABBF0D9
      C0FFFBEDE1FFF9DABFFFF9DCC1FFF9DEC4FFFAE0C7FFFAE2CAFFFAE2CDFFFAE5
      D0FFFFFEFDFFCB8E58FFBF8B4CF1362815450000000000000000000000000000
      00000000000000000000000000000000000000000000295A2F915EAF68FF80CF
      8CFF7DCF8AFF56A65FFF37843EFF020402080000000000000000000000000000
      000013131357E3E3E3FDFFFFFFFFFEFEFEFFFFFFFFFFF8F8F8FFE2E2E3FFCEC1
      B1FFFFF1D9FFFFEFD7FFFFF5DFFF89765EDB0604040C3A343580877876FEECE1
      E0FFFFFFFFFFFEFEFEFFEAE4E4FFDBD4D3FFECE5E5FFFCF9F9FFF5F0EFFFD9CF
      CFFFD9CFCFFF79696BFF49393BF6140F105A0000000000000000856235A4EDD0
      B1FFFFF6F0FFFAE1CAFFFBE3CCFFFBE3D0FFFBE6D3FFFBE9D5FFFCE9D8FFFCEA
      DBFFFFFFFDFFD29C6FFFEED9C0FFBA894BE50000000000000000000000000000
      00000000000000000000000000000000000000000000000000002B5A2F9160B1
      6AFF81D18EFF78C884FF55A55EFF1A3F1D7B0000000000000000000000000000
      000012121258E3E3E3FDFFFFFFFFFEFEFEFFFEFEFEFFFBFBFCFFE8E8E9FFD5C7
      B7FFFFF4DDFFFFF4E1FFFFFAE8FF897761DB0000000027212264B5A7A5FDF7EF
      EEFFD5D1D2FFAAA4A4FFA7A3A3FFADAAAAFFA8A6A6FF97908FFF9F9C9CFFADAA
      AAFFD9CFCFFFD9CFCFFF918588FF473D40E600000000000000007A5A3192EBCA
      A4FFFFFDFBFFFDE9D5FFFDEBD8FFFDEADBFFFDEDDFFFFDF0E2FFFDF1E4FFFCF0
      E4FFFFFFFFFFE09F6EFFFFFBF9FFDFB786FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002B5B
      2F9161B26BFF5DAE67FF1D442279000000000000000000000000000000000000
      00001111115BEEEEEEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAFAFFD6C5
      B0FFEBD9C1FFFEF4E2FFFFFFF2FF8A7A65DC000000004A444481CCC0C1FFF7EF
      EEFFA19695FF8A7D7CFF998C8BFFA69F9FFF9E9695FF796867FF847677FF8C86
      86FF928887FFEFE4E3FFD8D0D0FF73696CEB000000000000000070542E84EBC5
      99FFFFFFFFFFFCEFE2FFFDF0E7FFFDF1EBFFFDF5EEFFFDF8F1FFFDFAF7FFFFFC
      FAFFFFFFFFFFFEFBF7FFF4DABFFFC89552EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002B5C3091234E277F00000000000000000000000000000000000000000000
      0000090909258C8C8CD3B8B8B8F0B3B3B3EBC8C8C8EAFFFFFFFFFFFFFFFFF9F4
      ECFFDDD0BEFFE4D2BEFFFFFFF4FF8A7C69D80000000017151523595253958980
      81DCF7EFEEFFEFE4E3FFEFE4E3FFEFE4E3FFEFE4E3FFEFE4E3FFEFE4E3FFEFE4
      E3FFEFE4E3FF9A9193EF6A6262B52A26264900000000000000005E47266DEABF
      8BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDF9F4FFFBF3EAFFF8EBD9FFF8E6
      D3FFF5DFC5FFE9CBA5FFCE9B56ED513D215D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C0C0C1F1C1C1C2E2424243231313173464646885050509A5B5B
      5DA7686867B77F7F7DD28D857CD2574938A90000000000000000000000000A07
      08282E28288B766D6FCE8C8385E4958C8EE8988F91E9918788E98E8486E56B64
      66BF312B2B7B0D0A0B340302020900000000000000000000000030241336AE85
      48C6EABB80FFE8B675FFE6B16BFFE4AF66FFD4A158F0C99853E3B68B4CCFB489
      4ACCA57C44BB94713DA841321B4B040301050000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004E2B146B8749
      22BCAD5D2CEDB3612DF7B3612DF7B3612DF7B3602DF7B3602CF7B2602BF7B260
      2BF7B25F2BF7AC5C2AEF87471FBD47251163000000004E2B146B874922BCAD5D
      2CEDB3612DF7B3612DF7B3612DF7B3602DF7B3602CF7B2602BF7B2602BF7B25F
      2BF7AC5C2AEF87471FBD4725116300000000000000000000001D000000340000
      003600000036000000360000003600000036000000360000003600000036255E
      28D41F5523D0000000330000001D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A35B2CDEEBE4
      DEF2F5EADDFDF6EBDEFFF6EADEFFF6EADCFFF6EADCFFFAF3EBFFFAF3EBFFFAF2
      EAFFFCF7F3FFFAF6F2FDEFEFEEF0984F22D500000000A35B2CDEEBE4DEF2F5EA
      DDFDF6EBDEFFF6EADEFFF6EADCFFF6EADCFFFAF3EBFFFAF3EBFFFAF2EAFFFCF7
      F3FFFAF6F2FDEFEFEEF0984F22D5000000000000000000000034EFEFEFF5FAFA
      FAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF5D9E63FF539D
      5AFF4E9754FF4F8A54FD00000033000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000D2338584F86B4F64E89B9FF0000000000000000B66B34F5F4EA
      DEFEFDBE66FFFCBC65FFFBBD63FFFCBD62FFFCBD62FFFCBC60FFFBBC61FFFBBB
      5FFFFCBD5EFFFCBB60FFFBF9F6FDAF5D29F300000000B66B34F5F4EADEFEFCE4
      D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4
      D1FFFCE4D1FFFBF9F6FDAF5D29F3000000000000000100000036FBFBFBFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8FAF8FF62AA69FF61AB69FF87C9
      8FFF81C589FF509956FF25632AE1010301060000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001C3146655A8EBCEF91B8DEFF447DAEF50000000000000000BA7238F7F7ED
      E3FFFDC16CFFFFD89FFFFFD79DFFFFD69AFFFFD797FFFFD695FFFFD694FFFFD5
      93FFFFD492FFFBBD63FFFBF7F4FFB4622DF700000000BA7238F7F7EDE3FFFCE4
      D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4D1FFFCE4
      D1FFFCE4D1FFFBF7F4FFB4622DF7000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFF82C389FF55AA5FFF68B371FF8FCE
      96FF89CB90FF68AF6EFF327A39F9215626B80000000000000000000000000000
      00000000000000000000000000001D171321705C4C7FE6C5AAFFE6C4A8FFDDB7
      99FDB4A195FE9BB7D3FF578CBEF20E2439580000000000000000BE743AF7E091
      5EFFE08B4AFFF7B354FFE2954DFFE2883FFFF6AC4EFFF7B250FFF7B250FFF7B1
      4FFFF7B14DFFF7B14DFFFCF9F5FFB86A33F700000000BD763CF7F7F0E6FFFCE4
      D1FFFCE4D1FFE5D9C1FF669D6EFF559563FF579664FF6A9F71FFFCE4D1FFFCE4
      D1FFFCE4D1FFFCF9F5FFB86A33F7000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFF52AA5CFF95D2
      9EFF90CF98FF519E59FF0D1D0E6200000001123A5E701D588DA6287BC6EA2A83
      D1F72A83D1F72A83D1F72A83D1F7A5ACB2FCF2DCC9FFF8E3CEFFF7E0C7FFF8E2
      CBFFF3D1B2FFB0A197FE182E445F00000000000000000F060112D57D3CFDE5A2
      63FFE19056FFFDE5D3FFE59C5AFFE7A766FFE3965DFFF9DAC3FFFCE2CEFFFCE2
      CCFFFBE0C9FFFBE1C8FFFDFAF7FFBA7037F716672CE4187331FF177331FF1772
      31FF428950FF5F9A69FFBAD6C2FF76BA83FF5FAA68FF559562FFFCE2CCFFFBE0
      C9FFFBE1C8FFFDFAF7FFBA7037F7000000000000000100000036FCFCFCFFFCFC
      FCFFA4A4A4FFB5B5B5FFFBFBFBFFA4A4A4FFB5B5B5FFFAFAFAFF59B363FF9CD6
      A5FF98D3A1FF499D51FF00000036000000012C7FC3E6CFE1EBF0A5DBF2FD9DDB
      F4FF95DAF3FF8DD8F3FF85D7F3FFE5C3A7FFF5E5D6FFF4DAC0FFF3D8BCFFF3D8
      BCFFF8E3CCFFD9B599FF00000000000000004A260D58DD9951F7E8A968FFE39A
      5BFFF9D8C2FFFDE7D6FFF9DBC2FFE59F5BFFE8A968FFE39A54FFEEB593FFFCE2
      CDFFFBE1CBFFFBE1C9FFFBF7F2FFBE763BF7092D1366197331FF599F6CFF4795
      5AFF458F59FFC7DDCDFF5BB56FFF65AD73FF428C56FF197331FFFCE2CDFFFBE1
      CBFFFBE1C9FFFBF7F2FFBE763BF7000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFF5EBB6AFF5AB5
      65FF55AF5FFF50A75AFF0000003600000001328ED3F7EFFAFEFFA0E9F9FF90E5
      F8FF80E1F7FF70DEF6FF61DAF5FFE0BC9DFFF8EADCFFF4DDC5FFF4DCC3FFF3D8
      BCFFF8E2CDFFE4C0A3FF0000000000000000DA9D50F1ECB777FFE5A356FFF2D8
      C3FFFEE8D6FFFEE8D7FFFDE7D6FFF6D1B2FFE6A558FFE9B173FFE49C56FFFAE0
      C8FFFADFC7FFFADFC5FFFAF2EAFFBF7A3EF7000000007B7A3CFA1D7635FF4690
      5BFFC7DDCDFF68BF83FF6FB581FF428D57FFB0C0A0FFFBE4D0FFFBE3CCFFFADF
      C7FFFADFC5FFFAF2EAFFBF7A3EF7000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFAFAFAFFF9F9F9FFF6F6
      F6FFF6F6F6FFFCFCFCFF00000036000000013395D4F8F2FAFDFFB2EDFAFFA3E9
      F9FF94E6F8FF84E2F7FF74DEF6FFE6C5A9FFF3E4D6FFF6E0CAFFF5DEC5FFF5DE
      C4FFF8E6D3FFE0C1A7FF00000000000000004D2D0F58E1A456F7EAB46FFFE8A5
      61FFFADBC4FFFEE8D8FFFBDDC4FFE9AA5FFFEAB46FFFE8A65AFFEFB992FFFAE0
      C7FFF9DDC2FFF8DCC1FFFAF4EDFFBF7D41F700000000BB8345F75F9D6FFFC4DC
      CCFF74C996FF71BB86FF418C56FF53925EFFF5E0CCFFFBE1CCFFFAE0C7FFF9DD
      C2FFF8DCC1FFFAF4EDFFBF7D41F7000000000000000100000036FCFCFCFFFCFC
      FCFFA4A4A4FFB5B5B5FFFCFCFCFFA4A4A4FFB5B5B5FFF8F8F8FFC1C1C1FFF0F0
      F0FFF2F2F2FFFCFCFCFF0000003600000001339CD5F9F6FCFEFFC8F2FCFFB8EF
      FBFFABECFAFF9BE8F9FF8AE3F7FFA2D2D3FFE9CDB3FFF5E7DBFFF8ECDFFFF2DD
      C9FFEBD0B7FFA4B0AFFC00000000000000000000000010090312DC9546FDEBB5
      70FFE8A65FFFFDE7D6FFECB163FFECBA74FFEAAB65FFF9DAC0FFFADFC7FFF8DC
      C1FFF6DABCFFF6D8BAFFFAF4EFFFBF7E42F7010401076C8850FCBFD9C8FF81D3
      A2FF6BC089FF529461FF49955EFF4F9662FF659966FFF4DCC2FFF8DCC1FFF6DA
      BCFFF6D8BAFFFAF4EFFFBF7E42F7000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF2F2F2FFEFEF
      EFFFEDEDEDFFFCFCFCFF000000360000000134A2D5FAFEFFFFFFF8FDFFFFF6FD
      FFFFF5FCFFFFF3FCFEFFD8F6FCFF94E6F8FFA8D4D4FFC8C5B2FFE0BC9DFFE5C3
      A6FFE2CFB9FF43A1CEF800000000000000000000000000000000C18445F7E9B0
      6CFFE8AD5DFFFCE6D4FFECB563FFECB167FFF9DEC3FFFAE0C8FFF8DCC1FFF5D6
      BAFFF3D4B4FFF1D2B2FFF8F4F0FFBD7D42F72D5E3AACB4D3BDFF9BDAB4FF72C8
      94FF529461FF48925DFF5BA372FF57A06CFF4E9662FF609660FFE9D1B3FFF3D4
      B4FFF1D2B2FFF8F4F0FFBD7D42F7000000000000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF000000360000000132A6D5FAE8F6FBFF93D4EFFF87CE
      EEFF71C0E9FFC9E9F6FFF2FCFEFFF3FCFEFFF2FCFEFFF0FCFEFFEFFBFEFFEEFB
      FEFFFEFFFFFF33A4D3F700000000000000000000000000000000BF8447F7F8EF
      E6FFFCE3CFFFFBE4D0FFFCE4CFFFFCE3CDFFFAE1CAFFF9DDC3FFF6D9BBFFF4E9
      DFFFF7F2ECFFFBF7F3FFF5EFE9FFBE7A42FB387A4BDE538A54FE519564FF5294
      61FFA0B894FF8CAD82FF2C7D40FF2C7D3FFF388346FF348143FF8FB38FFFF7F2
      ECFFFBF7F3FFF5EFE9FFBE7A42FB000000000000000100000036FCFCFCFFF9F9
      F9FFACACACFFB4B4B4FFF7F7F7FFA4A4A4FFB2B2B2FFEBEBEBFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF000000360000000132A0CFF2F1FAFDFF93DEF5FF92DC
      F4FF80D5F2FF68CAEDFF6ACBEAFF84D3EFFF7ED2EFFF78D0EFFF74CFEEFF70CF
      EEFFE9F7FBFF30A5CFF300000000000000000000000000000000BE8348F6F9F5
      F1FFFCE3CDFFFBE3CEFFFBE3CDFFFBE2CBFFF9E0C8FFF8DCC1FFF5D6B9FFFDFB
      F8FFFCE6CDFFFAE5C9FFE2B583FF7B4D2AA600000000BE8348F6F9F5F1FFFCE3
      CDFFFBE3CEFFFBE3CDFFFBE2CBFFF9E0C8FFF8DCC1FFF5D6B9FFFDFBF8FFFCE6
      CDFFFAE5C9FFE2B583FF7B4D2AA6000000000000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF56565691000000200000000030A4CDF0F7FCFEFF8DE4F8FF90DE
      F5FF9EE0F5FFABE1F6FFEFFBFEFFF4FDFEFFF3FCFEFFF1FCFEFFEFFBFEFFEEFB
      FEFFF4F7F9F92B90B5D400000000000000000000000000000000B47C43EAF7F3
      EFFCFAE0C7FFFBE1C9FFFBE2C9FFFBE0C8FFF9DFC4FFF8DBC0FFF4D6B7FFFFFB
      F8FFF6D8B3FFE1AF7BFFD38C60F60502010700000000B47C43EAF7F3EFFCFAE0
      C7FFFBE1C9FFFBE2C9FFFBE0C8FFF9DFC4FFF8DBC0FFF4D6B7FFFFFBF8FFF6D8
      B3FFE1AF7BFFD38C60F605020107000000000000000000000036F9F9F9FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF5454549100000020000000020000000033ADD4F8FDFEFEFFFEFFFFFFFEFE
      FFFFFDFEFFFFFEFFFFFFEAF7FBFF68C2DEF966C1DCF866C1DCF866C1DCF873C6
      DEF764B2CAE1103B485600000000000000000000000000000000956536C3E4E0
      DAECF5F1EBFCF8F4EDFFF8F3EDFFF8F3EDFFF8F3EDFFF8F2ECFFF7F2ECFFF2E6
      D7FFE2B17BFFD28D5FF5050201070000000000000000956536C3E4E0DAECF5F1
      EBFCF8F4EDFFF8F3EDFFF8F3EDFFF8F3EDFFF8F2ECFFF7F2ECFFF2E6D7FFE2B1
      7BFFD28D5FF50502010700000000000000000000000000000033E9E9E9F0F9F9
      F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF5454
      5491000000200000000200000000000000002992B1D05ABDDCFA5CBEDDFA5CBE
      DDFA5CBEDDFA5BBEDDFA329DBDDD030F1216020A0D0F020A0D0F020A0D0F020A
      0D0F020A0D0F0103030400000000000000000000000000000000482E18609265
      37BBBA8248EEC1864AF6C1874BF7C1874BF7C1884BF7C2874BF7C0864BF7A371
      3ED46F42209104010106000000000000000000000000482E1860926537BBBA82
      48EEC1864AF6C1874BF7C1874BF7C1884BF7C2874BF7C0864BF7A3713ED46F42
      209104010106000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000711CEB0279
      1CFF000200040000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005B3D
      2B8A82553BC39F653FF09F633DF09F633DF09F633DF09F633DF09F633DF09F68
      44F05F402D900000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007826EB41A0
      5DFF006217CC0003010600000000000000000000000000000000000000000000
      000009140A231C46227D30773ADB33823BF32E8038F328712EDB143F187D0410
      0623000000000000000000000000000000000000000000000000000000000000
      0000090A1B231B275F7D2F46A2DB314BB3F32C47B2F326419DDB1424597D0409
      192300000000000000000000000000000000000000000000000000000000935E
      3EE1EEECEAFFF7F2F0FFF8F3F0FFF8F3EFFFF7F2EEFFF7F2EEFFF8F2EEFFF1EE
      EAFF9F6744EF0000000000000000000000000000000000000000000000000000
      000000000000000000001F954FFD198F47FD138D41FD0E8939FD389E5CFF7EC0
      95FF44A260FF006519D100020007000000000000000000000000000000001731
      1B533D8649E63F984EFF7BC18EFF95D0A5FF95CFA5FF76BD88FF348C40FF2673
      2DE60C280E53000000000000000000000000000000000000000000000000161B
      41533A4BB1E63A50CCFF7378E8FF8E91EEFF8E91EEFF6F76E4FF314BC0FF2541
      A4E60C173B530000000000000000000000000000000000000000000000009B5C
      35F0F6F0ECFFFDE8D7FFFEE8D7FFFEE8D7FFFEE8D7FFFEE8D7FFFDE8D7FFF7EC
      E5FF9F633DF0000000000000000000000000000000004BA2EAFF449DE8FF3C99
      E7FF3394E6FF1C8BE2FF279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B5
      84FF81C196FF46A464FF00681AD700030009000000000000000019331E534894
      57F462B376FFA7DBB4FF86CC97FF64BB7BFF62B97AFF85CB97FFA4D9B3FF56A9
      69FF287A30F40C280E5300000000000000000000000000000000181C43534453
      C2F45A63E0FFA0A5F5FF7C85EFFF5961E9FF575BE7FF7B83EEFF9D9FF4FF4F5B
      D7FF2845AEF40C173B5300000000000000000000000000000000000000009B5C
      34F0F7F0ECFF806040FFAB8156FFAB8156FFAB8156FFAB8156FF806040FFF6EC
      E5FFA86941FE9F633DF09F6844F05F402D900000000053A6EBFFB5E6F9FF94D1
      F1FF49A6E8FF58AEEBFF2F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF007A23F9000000000B160D224C905AE568B8
      7BFFA7DBB1FF5EBB75FF5AB971FF57B76EFF57B46DFF56B46DFF59B672FFA4D9
      B2FF58A96AFF26742DE50511052200000000000000000B0D1C224955BAE55F69
      E3FFA0ABF5FF525DECFF4E5AEAFF4B57E9FF4C57E6FF4A54E6FF4E54E6FF9DA1
      F4FF525ED6FF2441A4E505091822000000000000000000000000000000009D5E
      35F0F7F1ECFFAB8156FFE5AC73FFE5AC73FFE5AC73FFE5AC73FFAB8156FFF6ED
      E5FFAE734DFFF8F2EEFFF1EEEAFF9F6744EF000000005BAAEBFFB3E3F8FF309E
      E6FF39AAE9FFA7E9F8FF35A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB
      8FFF89C7A0FF44A466FF068432FB00010001000000002C53367E51AA66FFA9DD
      B3FF62C077FF5DBD6FFF5EBB75FFFFFFFFFFFFFFFFFF57B76EFF56B46CFF5AB6
      72FFA5DAB3FF368E41FF1540197E00000000000000002A2E697E4954DBFFA1AA
      F6FF5462F0FF5064EEFF4B57E9FF4B57E9FF4B57E9FF4B57E9FF4A56E6FF5058
      E6FF9EA2F5FF324EC3FF13235A7E000000005B3D2B8A82553BC39F653FF0A664
      3AFEF7F2EDFFA57D54FFDEA971FFDEA971FFDCA670FFDCA670FFA37B53FFF6ED
      E6FFAE724CFFFDE8D7FFF7ECE5FF9F633DF0000000005BABECFF53ACEBFF45AE
      EBFFABE9F9FFACEAFAFF3BA46DFF36A36DFF32A167FF2E9D61FF53AE7AFF90CB
      A9FF4DAA72FF158C42FB000100010000000000000000519363DB89CC97FF88D3
      95FF69C578FF61C06EFF53AA63FFFFFFFFFFFFFFFFFF57B76EFF57B76EFF59B8
      70FF84CC96FF79BD8CFF28712FDB00000000000000004D52B8DB808BEEFF7C90
      F7FF5B71F3FF4B57E9FF4B57E9FF4B57E9FF4B57E9FF4B57E9FF4B57E9FF4D59
      E9FF7982F0FF7379E2FF26409FDB00000000935E3EE1EEECEAFFF7F2F0FFAC6C
      45FFF7F4F1FFBDAA87FFFEE5B4FFFDE4B4FFFDE4B4FFFDE4B4FFBDAA87FFF6EF
      E7FFA56B4CFF5659B4FFF6ECE5FF9F633DF0000000005CACEBFF82C2F0FFD3F3
      FCFFD1F3FCFFB1EDFAFF59BBEDFF6ADAF4FF60D9F4FF58D6F3FF32A26CFF58B2
      80FF269755FF0000000000000000000000000000000060A874F6A8DDB2FF7BCF
      89FF73CC80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF57B7
      6EFF65BD7BFF9BD4AAFF318239F600000000000000005A5ED2F6A0AAF7FF6E85
      F8FF6681F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4B57
      E9FF5A64EAFF959BF1FF2F4BB4F6000000009B5C35F0F6F0ECFFFDE8D7FFAC6C
      43FFF7F4F3FFB5A380FFF2DCACFFF5DEAFFFFAE1B1FFFCE3B3FFBDAA87FFF7EF
      E8FFA86D4EFF6066DEFFF6EDE5FF9F653FF0000000006FB5EEFF9DD1F4FF76CA
      F0FF70C7F0FF6CC5F0FF5EBDEDFF74DDF5FF6ADBF4FF62D9F4FF38A674FF2F9E
      63FF218EE8FF0000000000000000000000000000000063AB78F6B5E2BDFF8AD5
      96FF78C985FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF57B7
      6EFF67C07CFF9CD4A9FF34853EF600000000000000006063D3F6AEB8F9FF7D92
      FAFF6E84F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4B57
      E9FF5C68EEFF959CF1FF324AB6F6000000009B5C34F0F7F0ECFF346A38FFA16D
      44FFF3F2F1FF95805AFFBAA57EFFBAA57EFFBAA57EFFBBA782FF8E7E63FFF7F1
      EAFFA76C4EFF5F64D6FFF6EDE6FF9F653FF00000000077B8F0FFDBF6FCFF9BE7
      F8FF94E5F8FF8EE3F7FF85E1F7FF7CDFF5FF74DDF5FF6BDBF4FF62D9F4FFBFF0
      FBFF2A93E9FF000000000000000000000000000000005C9A70DBABDDB5FFA5DF
      AEFF80CB8BFF7AC985FF6CBC77FFFFFFFFFFFFFFFFFF59AB68FF5EBB75FF5AB9
      71FF8AD198FF7EC491FF32793BDB00000000000000005759BEDBA4AEF5FF9CAA
      FAFF758BF0FF525DECFF525DECFF525DECFF525DECFF525DECFF525DECFF6175
      F2FF808DF4FF767DE9FF3046A4DB000000009D5E35F0F7F1ECFF458F4AFFA37D
      57FFDEDEDCFFF3F3F2FFF7F4F3FFF7F4F2FFF8F2EDFFF8F2EBFFF7F2EBFFF1EE
      E8FFAB7353FF978FE2FFF6EFE7FF9F653FF0000000008BC4F4FFDDF7FDFFA1E8
      F8FF9CE7F8FF94E5F8FF8EE3F7FF86E1F7FF7DDFF6FF75DDF5FF6BDBF4FFC3F0
      FBFF3597EAFF00000000000000000000000000000000365A427E84C796FFD2EE
      D7FF94D99FFF89D393FF7DC888FFFFFFFFFFFFFFFFFF77CD84FF69C27AFF6DC7
      7CFFABDFB4FF439D55FF1F47247E000000000000000033326F7E7B82EAFFCDD4
      FCFF8A9CFAFF7C92F7FF7389EEFF6A83F6FF6A83F6FF6A83F6FF6A83F6FF6177
      F3FFA3AEF8FF3C4DD0FF1E285E7E000000009D5E38F0F7F2EDFF438A49FF8797
      5DFFA37B57FFA36F46FFA76438FFAC6C43FFA66441FFA86744FFA96744FFA96E
      4CFFB6969CFF978FE2FFF7EFE8FFA0653FF00000000092C8F5FFE1F7FDFFA9EA
      F9FFA2E8F9FF9CE7F8FF95E5F8FF8FE3F7FF87E1F7FF7EDFF6FF75DDF5FFC8F1
      FBFF3E9DEBFF000000000000000000000000000000000F19122263A478E5A9DA
      B6FFD8F1DCFF91D89CFF87CD92FF83CC8DFF8AD495FF89D494FF82D28DFFAEE0
      B6FF69B87BFF3F874CE509140B2200000000000000000D0D1E225D5DC9E5A2A6
      F3FFD4DBFDFF8699FAFF7D90F0FF788DF1FF7D93F8FF7C91F9FF748BF8FFA7B5
      F8FF616CE3FF3C4CB2E5090B1A22000000009D5E38F0F7F4F1FF6E9E78FF93D4
      A1FF93D3A2FF93D3A2FFA67149FFF3F2F1FF736CC3FF8F87E0FF8F87E0FF8F87
      E0FF948BE0FF7E79B8FFF7F1EAFFA0653FF00000000096CCF6FFF9FDFFFFF0FB
      FEFFF0FBFEFFF0FCFEFFEEFBFDFFEFFBFDFFEFFBFDFFF0FBFDFFF0FBFDFFF7FD
      FEFF48A2EDFF0000000000000000000000000000000000000000243C2D5369AF
      80F4AEDCBAFFDCF2E0FFB5E4BCFF9ADBA4FF95D99FFFA4DFAEFFBFE8C4FF77C1
      89FF4B9659F417321C5300000000000000000000000000000000232349536464
      D6F4A9ACF2FFD8DCFDFFADB9FAFF90A2FAFF8A9CFAFF9BA8FBFFB9C7FCFF6E79
      E9FF4755C3F4171B425300000000000000009D5E38F0F7F4F3FF689772FF8DCA
      9CFF8FCD9DFF91D19FFFA8805CFFDEDEDCFFF3F3F2FFF7F4F3FFF7F4F2FFF8F2
      EDFFF8F2EBFFF7F2EBFFF1EEE8FF9E6744EF00000000B1DCFDFFFAFEFFFFFAFE
      FFFFFAFEFFFFFAFEFFFFFAFEFFFFFAFDFEFFFAFDFFFFFAFEFFFFFAFEFFFFFAFD
      FFFF51A7EEFF000000000000000000000000000000000000000000000000243C
      2D5364A579E693CEA3FFC2E6CBFFCFEBD4FFC9E9CEFFAEDDB7FF6BB87DFF4D94
      5DE61A3420530000000000000000000000000000000000000000000000002323
      49535E5EC9E68D92EDFFBDC2F8FFCCD3F9FFC3CBF9FFA9B3F4FF646EE2FF4953
      BDE6191D43530000000000000000000000009D6540F0F3F2F1FF497C53FF679B
      72FF679B72FF679B72FF8D8966FFA27556FFAC734EFFA66439FE9D5E35F09D5C
      35F09D5C35F09D5C35F09D633DF05E3F2C8F00000000B5DFFEFFA1D2FAFF9BD1
      F9FF96CDF8FF91CAF7FF8BC5F6FF84C1F5FF7CBEF4FF74B9F4FF6CB5F1FF63B1
      F0FF5BACF0FF0000000000000000000000000000000000000000000000000000
      00000F191323365A427D5D9A71DB63A978F360A874F3539464DB2E53377D0B17
      0E23000000000000000000000000000000000000000000000000000000000000
      00000E0E1F2333326D7D5759BDDB5D5DD1F35A5DCFF34E53BADB2B2F687D0B0C
      1C23000000000000000000000000000000009B6D4EE9DDDDDBFEF3F3F2FFF7F4
      F3FFF7F4F2FFF8F2EDFFF8F2EBFFF7F2EBFFF1EEE8FF9E6744EF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005D42318D9A6A4EE89D6540F09D5E
      35F09D5E35F09D5C35F09D5C35F09D5C35F09D633DF05E3F2C8F000000000000
      0000000000000000000000000000000000000000000000000000000000000002
      0003173919780000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002252
      279C2B6A31D20000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000010101050000000000000000000000000000
      000000000000000000000000000000000000000000000000000004090B0D0A08
      08136A2E11988E371AE3A2411FF5A74320FAA64120FAA03D1EF691361CEA8138
      1ABA0B08091604090B0D000000000000000000000000000000002551299058A3
      60FF56A05CFF35823CFF327C38FF2F7834FF2C7331FF27662AEA1B4D1EB70F2D
      1170030A031D00000000000000000000000000000000000000003B3B3BFF3434
      34FF2E2E2EFF282828FF232323FF0000000000000000131313FF0E0E0EFF0A0A
      0AFF060606FF020202FF00000000000000000000000000000000000000000000
      000000000000000000000303030A3A3A3A9E2323236200000000000000000000
      00000000000000000000000000000000000000000000324C64792375C9FB835D
      5DFBBE5E33FFFEB85FFFFEB860FFFEB860FFFEB860FFFEB85FFFFEB85FFFB047
      22FF735E67F52B79C8F836536B8300000000000000002450288460AB68FF84C8
      8CFF84C78AFF81C587FF7DC385FF7AC181FF77C07EFF6FB876FF5DA763FF4790
      4CFF18471CB00816083900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000303030A3A3A3A9E727272E45B5B5BDA23232362000000000000
      00000000000000000000000000000000000000000000277BD0FE81B9EEFF9E64
      56FFF5BA83FFFFAB59FFFEA758FFFEA155FFFE9B51FFFFA253FFFF9E4EFFF8AD
      76FFA35C48FF82BBEFFF2875C9FE000000000000000028572D8A64AF6CFF89CA
      91FF88CA8FFF85C88CFF82C589FF7EC486FF7CC283FF79C180FF6BBA72FF74BD
      7AFF579F5BFF1A481CB0030C031D000000000000000000000000494949FF4343
      43FF3D3D3DFF373737FF313131FF00000000000000001F1F1FFF1A1A1AFF1515
      15FF101010FF0B0B0BFF00000000000000000000000000000000000000000000
      00000303030A3A3A3A9E7D7D7DE7ADADADFF9B9B9BFA5B5B5BDA232323620000
      000000000000000000000000000000000000000000002679CBFC76B2EAFFB29D
      93FFFFB65EFFFFB561FFFEB15FFFFEAB5BFFFEA457FFFD9D51FFFE964CFFFF8C
      41FFBB8E81FF7CB7EDFF2770C3FA0000000000000000000000002B5D309365B0
      6DFF62AC69FF419349FF3E8E45FF3A8941FF37843EFF529C58FF72B978FF77C0
      7DFF75BE7BFF48904DFF0F2E1170000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      030A3A3A3A9E858585E7BDBDBDFFB4B4B4FFAAAAAAFF979797FA5A5A5ADA2323
      2362000000000000000000000000000000000000000031190C50895242FFFCC8
      AAFFFFD197FFFEC76BFFFEBE66FFFEB862FFFEB05CFFFEA757FFFD9F52FFFFB6
      78FFFEA87EFF874E40FF341B1060000000000000000000000000000000002F65
      369F3C8144D2000000000000000000000000000000001F4B228F2E7134E273BA
      79FF6EBC75FF61AA67FF1C4F20B7000000000000000000000000575757FF5151
      51FF4B4B4BFF454545FF3F3F3FFF00000000000000002D2D2DFF272727FF2222
      22FF1C1C1CFF171717FF000000000000000000000000000000000303030A3A3A
      3A9E8C8C8CE7CECECEFFC4C4C4FFB9B9B9FFAEAEAEFFA3A3A3FF909090FA5858
      58DA2323236200000000000000000000000000000000010000013F190460C34A
      1DFFF6E4D6FFFFE4A3FFFFD470FFFFC967FFFFBF61FFFFB55DFFFFC07EFFF6D7
      C5FFC4471DFF4119046902010004000000000000000000000000000000000002
      0003254E297B00000000000000000000000000000000000000001F4B238F5FA7
      65FF79C181FF74BB7AFF286A2CEA000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002B2B2B776C6C
      6CDCDEDEDEFFD2D2D2FFC7C7C7FFBABABAFFB0B0B0FFA4A4A4FF989898FF8181
      81F54E4E4ED31010102D0000000000000000000000000000000005010007431C
      0769BB461AFFF4E2D4FF4C79A8FF4B79A7FF4B79A7FF4C79A8FFF3D6C2FFBD44
      1AFF441B076F0603010A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000224F258F62AA
      69FF7DC385FF77BD80FF2B6E30EA000000000000000000000000636363FF5E5E
      5EFF595959FF535353FF4E4E4EFF00000000000000003C3C3CFF363636FF3030
      30FF2A2A2AFF242424FF000000000000000000000000000000000303030A3A3A
      3A9E8C8C8CE7CECECEFFC4C4C4FFB9B9B9FFAEAEAEFFA3A3A3FF909090FA5858
      58DA232323620000000000000000000000000000000000000000000000000300
      0005422416A2326BA6FF9BCCF8FFAED4F7FFAED4F7FFA4CFF6FF3272ADFF4528
      1DAD050200090000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000027542B8F398140E280C2
      87FF7BC384FF6CB273FF25592AB7000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      030A3A3A3A9E858585E7BDBDBDFFB4B4B4FFAAAAAAFF979797FA5A5A5ADA2323
      2362000000000000000000000000000000000000000000000000000000000000
      000020446FC4A5CAEEFFAACCEAFFA6D0F6FFA7D0F6FFAACCEAFFA6CDEEFF234E
      7ACC0000000000000000000000000000000000000000000000005DB968FF5AB5
      65FF57B262FF54AD5EFF51A95BFF4EA557FF4BA054FF66B06EFF87C88FFF8CCC
      94FF8ACB91FF5BA462FF183A1A700000000000000000000000006E6E6EFF6A6A
      6AFF656565FF606060FF5B5B5BFF00000000000000004A4A4AFF444444FF3E3E
      3EFF383838FF323232FF00000000000000000000000000000000000000000000
      00000303030A3A3A3A9E7D7D7DE7ADADADFF9B9B9BFA5B5B5BDA232323620000
      0000000000000000000000000000000000000000000000000000000000000000
      00001B568FEDD9E8F7FF96C4F1FF8DBAE5FF7DA8D1FF88B4DFFFCDDFEEFF2060
      9DF101040506000000000000000000000000000000000000000060BD6BFFA4DA
      ADFFA1D8ABFFA0D8A9FF9DD6A6FF9BD5A4FF98D4A1FF96D29EFF8BCD94FF90CF
      98FF71B779FF2A6230B00710071D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000303030A3A3A3A9E727272E45B5B5BDA23232362000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000B3C86FF7A96B7FF89B6E4FF6F9BC8FF133E6CFF174270FF204369FF1038
      62FA01020407000000000000000000000000000000000000000062C06EFFA5DB
      AFFFA5DAAEFFA2D9ACFFA1D8AAFF9ED7A7FF9BD5A4FF93D09CFF82C48BFF6AB3
      72FF306937B00E21103900000000000000000000000000000000757575FF7373
      73FF6F6F6FFF6B6B6BFF676767FF0000000000000000585858FF525252FF4D4D
      4DFF474747FF414141FF00000000000000000000000000000000000000000000
      000000000000000000000303030A3A3A3A9E2323236200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000E4996FF11569EFF0E4889FF0E4986FF104986FF134A84FF113F73FF0E2F
      55F100000000000000000000000000000000000000000000000064C370FF62C1
      6EFF60BE6CFF5EBB69FF5BB766FF59B463FF55AE5FFD4C9D55EA397841B72147
      26700712081D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000010101050000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000007162F77104C94FE11569AFF115698FF105192FF0E4886FF0D3C6FFE0A16
      2481000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000818327D114386F40F498FFF0E4689FF103E76F509192E840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000060302097A472AB7A05935FA0401
      0106000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000761EF50279
      1CFF000F022100000000000000000000000000000000B27047D5D5824FFFD17B
      49FF8E512EAF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000021212146626262D2727272FF6E6E6EFF696969FF656565FF6161
      61FF4F4F4FD7000000000000000000000000845733B7BE865AFFB87C54FFA35E
      37FA482618742F170E4E21100A391107051E23222149626262D2727272FF6E6E
      6EFF696969FF656565FF616161FF4F4F4FD70000000000000000000000000000
      000021212146626262D2727272FF6E6E6EFF696969FF656565FF04812BFF41A0
      5DFF006C1AE4001103270000000000000000AE734FCCDFA380FFEAC1AAFFEABF
      A7FFDC986FFF8E532FAF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002A2A
      2A4B6E6E6ECCA2A2A2FFC5C4C3FFD9D4D3FFD4CFCEFFD3CECDFFD3CECDFFE8E5
      E5FF626262FF000000000000000000000000BF8456FAC59166FFCDA17EFFC495
      6EFFB57951FFAA6844FFA25C3BFF9B5033FF945745FFC3BFBEFFD9D4D3FFD4CF
      CEFFD3CECDFFD3CECDFFE8E5E5FF626262FF00000000000000002A2A2A4B6E6E
      6ECCA2A2A2FFC5C4C3FF209650FF1A9048FF148E42FF0F8A3AFF389E5CFF7EC0
      95FF44A260FF006F1BE70013032A00000000DB966DFFEDC8B2FFE7B79AFFE6B3
      97FFEAC2AAFFDE9B71FF8E5430AF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000797979C9B2B2
      B2FFD4D4D4FFE1DFDFFFC4BEBCFFBCB4B2FFB8B0AFFFB8B0ADFFB6AEADFFD3CE
      CDFF666666FF00000000000000000000000005030106C1885CFAD1A582FFCC9E
      79FFCB9D79FFC79872FFC2916AFFBD8C63FFA76743FFAD8779FFBCB4B2FFB8B0
      AFFFB8B0ADFFB6AEADFFD3CECDFF666666FF00000000797979C9B2B2B2FFD4D4
      D4FFE1DFDFFFC4BEBCFF279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B5
      84FF81C196FF46A464FF00711DEA00160530DD9E77FFEDCCB6FFE8BCA2FFE4B0
      91FFE6B599FFEAC2ABFFDE9B72FF8E5433AC0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A1A1A1FFF0F0
      F0FFE0E0E0FFD4D2D2FFBDB5B4FFBBB4B3FFBAB3B1FFB9B1B0FFB8B0AEFFD4CF
      CEFF6B6B6BFF000000000000000000000000000000005E453174D0A07AFFD7AD
      8EFFC9966DFFC28E64FFBC875AFFBF8B62FFBB875FFF83513CFFB9B0AFFFBAB3
      B1FFB9B1B0FFB8B0AEFFD4CFCEFF6B6B6BFF00000000A1A1A1FFF0F0F0FFE0E0
      E0FFD4D2D2FFBDB5B4FF2F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF007C23FC805E4792E6B293FFEECCB7FFE9BD
      A4FFE5B293FFE6B69AFFEAC3ACFFDE9D76FF8E5635AC00000000000000000000
      0000000000000000000000000000000000000000000000000000A7A7A7FFEAEA
      EAFFDDDDDDFFD5D4D3FFBFB8B6FFBDB6B5FFBCB5B3FFBAB3B2FFBAB2B0FFD4D0
      CFFF717171FF000000000000000000000000000000004231274ED7A581FFDCB5
      98FFD0A07BFFCB9971FFCFA381FFC79872FF886A56FF868686FF767473FFBCB5
      B3FFBAB3B2FFBAB2B0FFD4D0CFFF717171FF00000000A7A7A7FFEAEAEAFFDDDD
      DDFFD5D4D3FFBFB8B6FF35A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB
      8FFF89C7A0FF44A466FF068533FD0008020F00000000805E4A92E6B397FFEECD
      B9FFE9BEA4FFE5B395FFE7B89CFFEBC5ADFFDE9E77FF8E5735AA000000000000
      0000000000000000000000000000000000000000000000000000AEAEAEFFEBEB
      EBFFDDDDDDFFD7D5D5FFC0BAB8FFBFB8B7FFBDB7B5FF39A040FF359335FFD6D1
      D0FF767676FF0000000000000000000000000000000032261F39DDAD8BFFE2BD
      A3FFD8AA88FFD9B293FFCF9E78FFA37859FF929292FFA8A8A8FF636262FFBCB6
      B4FF39A040FF359335FFD6D1D0FF767676FF00000000AEAEAEFFEBEBEBFFDDDD
      DDFFD7D5D5FFC0BAB8FF3BA46DFF36A26CFF31A065FF2D9C5FFF53AE7AFF90CB
      A9FF4DAA72FF158D43FD0006030C000000000000000000000000805F4C92E6B6
      98FFEECEBAFFE9BFA6FFE8BCA2FFECC8B2FFDFA380FFA66B49C9000000000000
      0000000000000000000000000000000000000000000000000000B4B4B4FFECEC
      ECFFDEDEDEFFD9D8D8FFC2BCBAFFC1BAB9FFC0B9B8FF9FC8A3FF4EA854FFD7D3
      D1FF7C7C7CFF000000000000000000000000000000001B16121EE3B392FFE8C5
      ACFFE3BFA5FFDBAF8EFFB48E72FFD7C9BEFF979797FF737272FFBFB9B8FFC0B9
      B8FF9FC8A3FF4EA854FFD7D3D1FF7C7C7CFF00000000B4B4B4FFECECECFFDEDE
      DEFFD9D8D8FFC2BCBAFFC1BAB9FFC0B9B8FF9FC8A3FF4EA854FF36A065FF58B2
      80FF269755FF0005020900000000000000000000000000000000000000008060
      4C92E6B69BFFEFCFBBFFEECEB9FFE2AD8BFFD29A79FF838383F7121212230000
      0000000000000000000000000000000000000000000000000000BABABAFFECEC
      ECFFDFDFDFFFDAD9D9FFC5BEBDFFC3BDBBFFC2BBBAFFC0BAB8FFBFB8B7FFD7D3
      D3FF838383FF0000000000000000000000000000000004030304C49F85DAE8BF
      A2FFE5BEA2FFB49C8AFFADADADFFA5A5A5FFD9D8D8FFC4BDBCFFC3BDBBFFC2BB
      BAFFC0BAB8FFBFB8B7FFD7D3D3FF838383FF00000000BABABAFFECECECFFDFDF
      DFFFDAD9D9FFC5BEBDFFC3BDBBFFC2BBBAFFC0BAB8FFBFB8B7FF3BA46DFF2F9E
      63FF020602090000000000000000000000000000000000000000000000000000
      000080604E92E7B89BFFE6B597FFD8A88BFFD2D2D2FFB4B4B4FF878787FE8484
      84FB828282FB6D6D6DD638383870000000000000000000000000BFBFBFFFECEC
      ECFFE0E0E0FFDDDCDCFFC7C0BFFF919090FF8E8E8EFF8C8C8CFF898989FFD9D5
      D4FF8A8A8AFF000000000000000000000000000000000000000008060609A181
      6CAFC8A995F8B6B6B6FFBBBBBBFFABABABFFDDDCDCFFC7C0BFFF919090FF8E8E
      8EFF8C8C8CFF898989FFD9D5D4FF8A8A8AFF00000000BFBFBFFFECECECFFE0E0
      E0FFDDDCDCFFC7C0BFFF919090FF8E8E8EFF8C8C8CFF898989FFD9D5D4FF8A8A
      8AFF000000000000000000000000000000000000000000000000000000000000
      00000000000082624F92B1876ECF8F8F8FFBC7C7C7FFCCCCCCFFC7C7C7FFC5C5
      C5FFC2C2C2FFBFBFBFFF7E7E7EF5393939700000000000000000C5C5C5FFEFEF
      EFFFE1E1E1FFDDDDDDFFC8C2C1FFC8C1C0FFC5C0BEFFC4BEBDFFC2BCBAFFDAD6
      D5FF909090FF0000000000000000000000000000000000000000000000000000
      00001C16131EADADADFFB8B8B8FFE1E1E1FFDDDDDDFFC8C2C1FFC8C1C0FFC5C0
      BEFFC4BEBDFFC2BCBAFFDAD6D5FF909090FF00000000C5C5C5FFEFEFEFFFE1E1
      E1FFDDDDDDFFC8C2C1FFC8C1C0FFC5C0BEFFC4BEBDFFC2BCBAFFDAD6D5FF9090
      90FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002A2A2A48919191FED4D4D4FFC8C8C8FFBBBB
      BBFFB9B9B9FFC1C1C1FFC3C3C3FF6F6F6FD60000000000000000CBCBCBFFE9E9
      E9FFE9E9E9FFE2E1E1FFCBC5C3FF979696FF959494FF939292FF919090FFDBD7
      D6FF979797FF0000000000000000000000000000000000000000000000000000
      000000000000CBCBCBFFE9E9E9FFE9E9E9FFE2E1E1FFCBC5C3FF979696FF9594
      94FF939292FF919090FFDBD7D6FF979797FF00000000CBCBCBFFE9E9E9FFE9E9
      E9FFE2E1E1FFCBC5C3FF979696FF959494FF939292FF919090FFDBD7D6FF9797
      97FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000939393FCDDDDDDFFC4C4C4FF6B6B
      6BC16B6B6BC3ABABABFFD7D7D7FF848484FB000000000000000039393948B6B6
      B6E4DFDFDFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3C2FFC8C2C0FFC7C0C0FFDCD9
      D8FF9D9D9DFF0000000000000000000000000000000000000000000000000000
      00000000000039393948B6B6B6E4DFDFDFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3
      C2FFC8C2C0FFC7C0C0FFDCD9D8FF9D9D9DFF0000000039393948B6B6B6E4DFDF
      DFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3C2FFC8C2C0FFC7C0C0FFDCD9D8FF9D9D
      9DFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000959595FBE4E4E4FFCFCFCFFF6E6E
      6EC2000000008D8D8DFF8B8B8BFF878787FB0000000000000000000000000C0C
      0C0F888888ABDCDCDCFFE6E3E3FFE1DEDCFFDFDCDCFFDFDCDBFFDEDBDBFFEEEC
      ECFFA4A4A4FF0000000000000000000000000000000000000000000000000000
      000000000000000000000C0C0C0F888888ABDCDCDCFFE6E3E3FFE1DEDCFFDFDC
      DCFFDFDCDBFFDEDBDBFFEEECECFFA4A4A4FF00000000000000000C0C0C0F8888
      88ABDCDCDCFFE6E3E3FFE1DEDCFFDFDCDCFFDFDCDBFFDEDBDBFFEEECECFFA4A4
      A4FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000818181D6E2E2E2FFE7E7E7FFB8B8
      B8FF929292FF0000000000000000000000000000000000000000000000000000
      0000000000007F7F7F9FC6C6C6FBC3C3C3FFBFBFBFFFBABABAFFB5B5B5FFAFAF
      AFFF6E6E6EA70000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F7F7F9FC6C6C6FBC3C3C3FFBFBF
      BFFFBABABAFFB5B5B5FFAFAFAFFF6E6E6EA70000000000000000000000000000
      00007F7F7F9FC6C6C6FBC3C3C3FFBFBFBFFFBABABAFFB5B5B5FFAFAFAFFF6E6E
      6EA7000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000045454570949494F5E4E4E4FFEEEE
      EEFF959595FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000045454570828282D69797
      97FB969696FB0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004E2B146B874922BCAD5D
      2CEDB3612DF7B3612DF7B3612DF7B3602DF7B3602CF7B2602BF7B2602BF7B25F
      2BF7AC5C2AEF87471FBD4725116300000000000000000000000000000000C693
      5EFDCA9763FFCA9663FFCA9663FFCA9663FFCA9662FFC99662FFC99662FFCA97
      63FFC6925EFD0000000000000000000000000B0603144A2E18845B391EA3663F
      22B6714625C97A4C28DA84522CEB8C572EFA945F37FF99673FFF057B21FF0279
      1CFF695126DE1D11093400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A35B2CDEEBE4DEF2F5EA
      DDFDF6EBDEFFF6EADEFFF6EADCFFF6EADCFFFAF3EBFFFAF3EBFFFAF2EAFFFCF7
      F3FFFAF6F2FDEFEFEEF0984F22D5000000003A3A3A994B4B4BD2565656FFC794
      5FFFF9F7F6FFF9F1ECFFF9F1EBFFF8F0E9FFF7EDE6FFF4EAE1FFF2E8DEFFFAF8
      F6FFC7935FFF222222FF1B1B1BD10F0F0F7A3C251368B78F6BFFD6B9A2FFDFC5
      B2FFE7D4C2FFEEDFD3FFF5EAE2FFFBF4EFFFFDFAF6FFFFFEFDFF0A8630FF41A0
      5DFF127D27FF363415850000000000000000CD6C21FFC96319FFC85E18FFC557
      16FFC15115FFBF4C14FFBC4513FFBA3F13FFB83C12FFB63712FFB53311FFB331
      11FFB33111FFB33111FFB33111FFB33111FF00000000B66B34F5F4EADEFEFDBE
      66FFFCBC65FFFBBD63FFFCBD62FFFCBD62FFFCBC60FFFBBC61FFFBBB5FFFFCBD
      5EFFFCBB60FFFBF9F6FDAF5D29F300000000676767FDA6A6A6FFB4B4B4FF8080
      80FFAEABA9FFC4BFBCFFC4BFBCFFC4BFBCFFC4BFBCFFC4BFBCFFC4BFBCFFACA9
      A7FF2A2A2AFFB4B4B4FF9A9A9AFF212121FF52341B89C7A384FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF209650FF1A9048FF148E42FF0F8A3AFF389E5CFF7EC0
      95FF44A260FF07751DF40013032A00000000D27533FFE4AE86FFE3AA80FFE1A7
      79FFDFA274FFDEA06FFFDC9C6BFFDB9866FFDA9661FFD9935CFFD79059FFD78E
      55FFD58C52FFD58B51FFD58B51FFB33111FF00000000BA7238F7F7EDE3FFFDC1
      6CFF164055FF295F86FF4A88BBFF6E9EB2FFE3C999FFFFD694FFFFD593FFFFD4
      92FFFBBD63FFFBF7F4FFB4622DF7000000006E6E6EFFB4B4B4FFB4B4B4FF9494
      94FF808080FF808080FF777777FF6C6C6CFF5F5F5FFF505050FF414141FF4040
      40FF6C6C6CFFB4B4B4FFB4B4B4FF232323FF36231357976335F6B38457FFD9A4
      79FFD89D6DFFD79A68FF279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B5
      84FF81C196FF46A464FF00711DEA00160530D68341FFE7B48FFFE0A272FFDE9D
      6CFFDC9965FFDB945EFFD99058FFD78C51FFD5884BFFD38446FFD28041FFD07C
      3CFFCF7937FFCE7733FFD58C52FFB33111FF00000000BD763CF7F7F0E6FFF8B3
      53FF2C6481FF93C7F9FF90C9F9FF3F84C9FF2466A5FFD2A763FFF7B14FFFF7B1
      4DFFF7B14DFFFCF9F5FFB86A33F700000000737373FFBABABAFFBABABAFF8C8C
      8CFFD4D4D4FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFB8B8B8FFD3D3
      D3FF828282FFBABABAFFBABABAFF282828FF0D0805146D4928ABD5AD8BFFFDF0
      E5FFF7C7A1FFF7CFACFF2F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF007C23FCDB8D51FFEABA98FFFCF6F2FFE1A5
      77FFFCF5F1FFDD9C69FFFCF6F2FFDA935CFFF7E9DEFFD68B4FFFEFD1B9FFDEA3
      75FFFDFAF7FFF7E8DDFFD78E57FFB53311FF00000000BE7A3EF7F8F1E8FFFEE5
      D5FF4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF468BC1FFDAD2CDFFFBE0
      C9FFFBE1C8FFFDFAF7FFBA7037F700000000787878FFD7D7D7FFD7D7D7FF9696
      96FFD8D8D8FFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFD7D7
      D7FF8D8D8DFFD7D7D7FFD7D7D7FF3D3D3DFF000000001B110A28B68554FFFEFE
      FDFFFADEC1FFFADCBEFF35A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB
      8FFF89C7A0FF44A466FF068533FD0008020FE19660FFECC0A0FFFCF7F3FFE5AC
      83FFFCF6F2FFE1A375FFFCF7F3FFDD9A67FFF8EBE0FFF0D3BDFFF3DECEFFE0A8
      7DFFFAF0E9FFD28242FFD9935CFFB63A12FF00000000BE7D41F7F8F2EBFFFEE7
      D6FFA5B5BEFF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4F9AD9FFE1D6
      CDFFFBE1C9FFFBF7F2FFBE763BF7000000007C7C7CFFF9F9F9FFF9F9F9FFAAAA
      AAFFDFDFDFFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFDFDF
      DFFFA2A2A2FFF9F9F9FFF9F9F9FF5F5F5FFF00000000150D071EB88550FFFEFC
      F9FFF9DCBEFFF8DBBEFF3BA46DFF37A26CFF33A066FF2F9D60FF53AE7AFF90CB
      A9FF4DAA72FF168E43FF0006030C00000000E29F6CFFEEC7A7FFFEFDFCFFFDF7
      F3FFFEFAF8FFE3AB80FFFCF7F4FFE0A272FFF9ECE3FFFAF2EBFFFDF8F4FFE3AD
      85FFFAF1EAFFD5884BFFDA9864FFBC4113FF00000000BF7F43F7F9F3ECFFFEE8
      D6FFFEE8D7FFB2C5CCFF74B8D6FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4797
      DAFFE2D5C8FFFAF2EAFFBF7A3EF7000000007D7D7DF9FCFCFCFFFCFCFCFFCBCB
      CBFFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFC5C5C5FFFCFCFCFFFCFCFCFF6E6E6EFE0000000006050209B88449FFFEFB
      F7FFF9DCC0FFF8DCBEFFF8DCBEFFF8DBBFFFF9DDBFFFF9DDBFFF37A065FF58B2
      80FF269755FFAE8044FB0101000100000000E6A677FFF0CBAFFFFDF8F5FFEAB9
      97FFFDF8F4FFE7B28BFFFDF8F5FFE3A97EFFF9EEE5FFEECDB3FFFDF8F5FFE5B2
      8DFFFDF9F6FFD88E55FFDD9D6BFFBF4C14FF00000000BF8345F7F9F4EDFFFEE8
      D8FFFEE8D8FFFEE8D7FFAFC5CCFF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4998DBFFD2DFE9FFBF7D41F700000000696969D2D2D2D2FFE8E8E8FF7B7B
      7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B
      7BFF7B7B7BFFE8E8E8FFC3C3C3FF494949DC0000000000000000B78447F9FCF6
      F0FFF9DFC7FFF9DCBCFFFADCBEFFFADBC0FFFADDC2FFFADDC1FF3DA46CFF2F9E
      63FFF8F9F5FFC08C51FF0B08030F00000000EAAA7EFFF2CFB4FFFCF4EEFFECBE
      9EFFFBF3EDFFFDF8F4FFFDF7F4FFFCF7F3FFF4DBC9FFE7B38DFFF7E6DAFFE3AC
      82FFF6E4D6FFDB9660FFDFA274FFC35716FF00000000BF8346F7F9F4EFFFFEE7
      D7FFFDE7D6FFFDE7D5FFFDE6D4FFBCD6D5FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4FA0E0FFA8815CF90000000023232345999999FFCCCCCCFFC78A
      4CFFF9F4EDFFFEE8D8FFFEE8D7FFFDE5D3FFFCE4D1FFFAE0C7FFF9DDC2FFFAF4
      EDFFC78448FFC2C2C2FF727272FF121212450000000000000000A67941DAF5E7
      D8FFFAE5D2FFF9DABBFFF9DBBBFFFADBBEFFFADDC0FFFADDC0FFF9DDC3FFFBE1
      C8FFFFFDFBFFC89355FF0D0A041200000000EAAA7EFFF3D0B6FFEFC5A8FFEFC3
      A5FFEEC1A1FFECBE9DFFEBBB97FFE9B792FFE8B38DFFE6AF87FFE3AB80FFE2A6
      79FFE0A272FFDE9D6CFFE2A97EFFC96018FF00000000BF8447F7F9F4F0FFFCE6
      D3FFFCE6D4FFFDE7D3FFFCE4D1FFFBE3CDFFBDD4D0FF7BD4EEFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF6297C7FE0C161D2600000000323232637A7A7AF3C488
      4AFFF9F4EFFFFEE7D7FFFDE7D5FFFCE6D2FFFBE1CCFFF8DCC1FFF6DABCFFFAF4
      EFFFC38246FF535353F31F1F1F63000000000000000000000000946C3ABBF0D9
      C0FFFBEDE1FFF9DABFFFF9DCC1FFF9DEC4FFFAE0C7FFFAE2CAFFFAE2CDFFFAE5
      D0FFFFFEFDFFCB8E58FFBF8B4CF136281545EAAA7EFFF3D0B6FFF3D0B6FFF3D0
      B6FFF2D0B6FFF1CEB2FFF0CBAFFFEFC9ABFFEEC5A7FFEDC1A2FFEBBF9DFFEABA
      98FFE8B693FFE6B38EFFE4AF88FFCD6C21FF00000000BF8447F7F9F5F1FFFCE3
      CFFFFBE4D0FFFCE4CFFFFCE3CDFFFAE1CAFFF9DDC3FFAECDC9FF80D5EEFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF7000000000000000005050509BF85
      47F9F9F4F0FFFCE6D3FFFDE7D3FFFBE3CDFFFAE0C8FFF5D6BAFFF3D4B4FFF8F4
      F0FFBD7E43F90202020900000000000000000000000000000000856235A4EDD0
      B1FFFFF6F0FFFAE1CAFFFBE3CCFFFBE3D0FFFBE6D3FFFBE9D5FFFCE9D8FFFCEA
      DBFFFFFFFDFFD29C6FFFEED9C0FFBA894BE5EAAA7EFFEAAA7EFFEAAA7EFFEAAA
      7EFFEAAA7EFFEAAA7EFFE8A87AFFE6A375FFE29F6EFFE29A69FFE19660FFDD8F
      57FFD98A50FFD88447FFD67E3CFFD27533FF00000000BE8348F6F9F5F1FFFCE3
      CDFFFBE3CEFFFBE3CDFFFBE2CBFFF9E0C8FFF8DCC1FFF5D6B9FFAEE3F1FF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E8000000000000000000000000BF84
      47F7F9F5F1FFFCE3CFFFFCE4CFFFFAE1CAFFF9DDC3FFF4E9DFFFF7F2ECFFF5EF
      E9FFBE7A42FB00000000000000000000000000000000000000007A5A3192EBCA
      A4FFFFFDFBFFFDE9D5FFFDEBD8FFFDEADBFFFDEDDFFFFDF0E2FFFDF1E4FFFCF0
      E4FFFFFFFFFFE09F6EFFFFFBF9FFDFB786FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B47C43EAF7F3EFFCFAE0
      C7FFFBE1C9FFFBE2C9FFFBE0C8FFF9DFC4FFF8DBC0FFF4D6B7FFFFFBF8FFB5CB
      C1FF56A4D8FF84B0DBFF449CD0FF0F374D5E000000000000000000000000BE83
      48F6F9F5F1FFFCE3CDFFFBE3CDFFF9E0C8FFF8DCC1FFFDFBF8FFFCE6CDFFE2B5
      83FF7B4D2AA6000000000000000000000000000000000000000070542E84EBC5
      99FFFFFFFFFFFCEFE2FFFDF0E7FFFDF1EBFFFDF5EEFFFDF8F1FFFDFAF7FFFFFC
      FAFFFFFFFFFFFEFBF7FFF4DABFFFC89552EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000956536C3E4E0DAECF5F1
      EBFCF8F4EDFFF8F3EDFFF8F3EDFFF8F3EDFFF8F2ECFFF7F2ECFFF2E6D7FFE2B1
      7BFFD28D5FF5050201070000000000000000000000000000000000000000BF82
      46FAF7F2ECFFF8F4EEFFF8F3EDFFF8F3EDFFF8F2ECFFF2E6D7FFE2B17BFFD28B
      5EF60502010700000000000000000000000000000000000000005E47266DEABF
      8BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDF9F4FFFBF3EAFFF8EBD9FFF8E6
      D3FFF5DFC5FFE9CBA5FFCE9B56ED513D215D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000482E1860926537BBBA82
      48EEC1864AF6C1874BF7C1874BF7C1884BF7C2874BF7C0864BF7A3713ED46F42
      209104010106000000000000000000000000000000000000000000000000482E
      1860926537BBC78A4DFEC88B4DFFC1884BF7C2874BF7C3874AFE714322940000
      000000000000000000000000000000000000000000000000000030241336AE85
      48C6EABB80FFE8B675FFE6B16BFFE4AF66FFD4A158F0C99853E3B68B4CCFB489
      4ACCA57C44BB94713DA841321B4B040301050000000000000000000000000000
      0000000000000000000000000000000000000000000317451BAA000000000000
      000000000000000000000000000000000000060302097A472AB7A05935FA0401
      0106000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000808080D1C1C1C311A1A1A2F04040407215725C0205B24D2000000000000
      000000000000000000000000000000000000845733B7BE865AFFB87C54FFA35E
      37FA482618742F170E4E21100A391107051E0201010400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000180F092353361E7D915E35DB9F6638F39E6436F38D582EDB5031187D160D
      06230000000000000000000000000000000000000000151515216363639D9595
      95F0A0A0A0FFAAAAAAFFA6A6A6FF508754FF549C5CFF519858FF286F2DFF2469
      29FF216425FF17481AC00717084000000000BF8456FAC59166FFCDA17EFFC495
      6EFFB57951FFAA6844FFA25C3BFF9B5033FF703321C605020009000000000000
      000000000000000000000000000000000000000000003C3732FF37322EFF312D
      29FF2A2723FF25221FFF1E1B19FF0202021A0E0C0BDB0A0908FF060605FF0303
      02FF000000FF000000FF00000000000000000000000000000000000000003B28
      1953A16E45E6D7BAA2FFE9DACAFFECE0D1FFECE0D1FFE8D8C8FFD3B49BFF9661
      33E6362111530000000000000000000000002F2F2F4A9D9D9DF4CECECEFFEDED
      EDFFF4F4F4FFF5F5F5FF4F9957FF5EA666FF8CCD96FF89CB93FF86CA90FF83C9
      8DFF80C88BFF5EA666FF184E1BCC0718084005030106C1885CFAD1A582FFCC9E
      79FFCB9D79FFC79872FFC2916AFFBD8C63FFA76743FF44201476000000000000
      00000000000000000000000000000000000000000000443F39FF84786EFFC2B7
      ADFF7A7066FF7D7369FF34302BFF0101000F1F1D1AD494887BFFB9ADA1FF7A70
      66FF7D7369FF000000FF000000000000000000000000000000003C2C1E53B280
      55F4E7D5C3FFE5D2BEFFC9A584FFB78D65FFB58963FFC4A07EFFE0CCB9FFE3D0
      BDFFA2693CF43723135300000000000000009A9A9AEADEDEDEFFF3F3F3FFDBDB
      DBFFD2D2D2FFDBDBDBFFD6D6D6FF629F68FF60A868FF5BA363FF337E39FF2F78
      34FF4F9656FF81C88CFF59A061FF133A1699000000005E453174D0A07AFFD7AD
      8EFFC9966DFFC28E64FFBC875AFFBF8B62FFBB875FFF7F4E39FA030202090000
      000000000000000000000000000000000000000000004B453FFF82766DFFCCC2
      B9FF766D63FF796F65FF2C2825F9000000011A1815EE94887BFFC1B7ACFF766D
      63FF7A7066FF050404FF0000000000000000000000001A140D22AE825DE5EAD8
      C9FFE3CDB9FFBF9369FFB98B60FFCFAF93FFCFAF93FFB6885DFFB1865FFFDABF
      A9FFE4D1BFFF9B673DE5170F0922000000009D9D9DEAF0F0F0FFDEDEDEFFD4D4
      D4FFD2D2D2FFDBDBDBFFD1D4D2FFBDBEBDFF5D9A62FF529458FFDEDEDEFF8383
      83EA255C29C22B7331FF276D2CFF1F5B23E0000000004231274ED7A581FFDCB5
      98FFD0A07BFFCB9971FFCFA381FFC79872FF886A56FF868686FF2F2F2F9F0000
      00000000000000000000000000000000000000000000504943FC82766DFFCCC2
      B9FF776E64FF6F665DFF2C2925D5000000001D1C19D584786EFFC1B7ACFF766D
      63FF796F65FF090807FC000000000000000000000000644E3A7EE4CCB8FFEAD6
      C4FFC7986FFFBE8F64FFBE8F64FFF7F1ECFFF6F0EAFFB6885DFFB6885DFFB488
      61FFE2CEBAFFD9BCA5FF573B257E000000009F9F9FEAF2F2F2FFE2E2E2FFD8D8
      D8FFD5D5D5FFDCDCDCFFD8D8D8FFBFBFBFFFB2B2B2FF69A06EFFE0E0E0FF8989
      89EA000000000000000000000000000000000000000032261F39DDAD8BFFE2BD
      A3FFD8AA88FFD9B293FFCF9E78FF9F7454FA929292FFA8A8A8FF464646D80404
      04121007031E00000000000000000000000000000000433D39C39E9185FFCCC2
      B9FFBFB3A9FFA5978AFF282422A8000000001C1917A88F8377FFC1B7ACFFBFB3
      A9FFA79A8DFF0C0A0AC3000000000000000000000000B58F71DBEFE1D3FFD9B4
      94FFC7976AFFC29467FFC09265FFBE8F64FFBE8F64FFBA8A61FFB88961FFB789
      60FFCBA685FFEADCCCFF9D7049DB00000000A0A0A0EAF3F3F3FFE7E7E7FFDDDD
      DDFFD9D9D9FFE0E0E0FFDBDBDBFFC3C3C3FFB6B7B6FFBABABAFFE1E1E1FF5990
      5EF800020003000000000000000000000000000000001B16121EE3B392FFE8C5
      ACFFE3BFA5FFDBAF8EFFAF896CF92E21153E4545459F565656D8090909182310
      0B3C883F28F55F2B1AAF05020009000000000101010558514AF9403B36FF564F
      48FF3B3631FF312D29FF1F1B19E503030330151310B5181614FF23201EFF1715
      13FF0E0D0CFF000000EE000000020000000000000000D1A989F6F2E4D9FFD1A4
      78FFC49869FFC39668FFC39567FFFAF6F2FFF3EAE1FFC1946BFFBD8E63FFBD8E
      62FFBF946BFFEFE3D5FFB7865CF600000000A2A2A2EAF4F4F4FFEAEAEAFFE1E1
      E1FFDDDDDDFF6FC178FF5BB766FF58B262FF6DB073FFBEBEBEFFE2E2E2FF5397
      5AFB327038C20001010202050209000000000000000004030304C49F85DAE8BF
      A2FFE5BEA2FFB19987FBADADADFF5151519F000000030707071227160C3C9852
      31F6AC704AFFA15D3DFF793622DA02010104030303059C9084FFB0A295FF7D73
      69FF7A7066FF756B62FF6A6159FF2C2824FF544D46FF7E746AFF7A7066FF756B
      62FF6E655CFF000000FE000000050000000000000000D8B293F6F2E5DAFFD1A5
      7CFFCC9C6FFFC7996AFFC49769FFE2CCB5FFF8F3EEFFF6EEE8FFD9BCA0FFC193
      66FFC49A6FFFF0E2D6FFBD8F66F600000000A4A4A4EAF5F5F5FFEEEEEEFFE6E6
      E6FFE2E2E2FF98D09EFF8DCF96FFA9D9B0FF78C282FF55AE5FFF50A85AFF6DB6
      76FF69B272FF316D36BB0000000000000000000000000000000008060609A181
      6CAFC8A995F8B6B6B6FFBBBBBBFF5757579F000000002A1B0F3CA7673CF6BA82
      5AFFBF8E65FFBA895EFF984E31FF1107051E030202049A8D82E1B9ADA1FF8175
      6BFF81756BFFA99079FFB9A693FFB2A08AFAAF9680FF9E8C7BFF826B59FF6F61
      55FF94887BFF040402E0000000030000000000000000C6A58BDBF3E5D9FFDFBA
      9DFFCF9F73FFCD9D70FFF5EBE3FFE4CBB3FFE7D3BEFFFBF8F6FFE5D3BEFFC397
      69FFD6B390FFEEE0D2FFAF8765DB00000000A5A5A5EAF6F6F6FFE2E2E2FFCDCD
      CDFFC3C3C3FFB2CAB5FF77C382FF93D29BFFAADAB1FFA7D9AEFFA4D8ACFFA1D6
      A9FF9ED5A6FF6AB372FF3C8142DC000000000000000000000000000000000000
      00001C16131E6060609C5F5F5F9F000000006045307CBC8152F7C9966DFFCB9E
      7AFFBB8457FFC29169FFA5613CFF22100A390605050826221F489A8D81FF9C90
      84FF85796FFF544D46FF4E4842FF7E746AFF6C645BFF816A56FFA5907BFF9383
      72FF544D46FF0604047A0000000100000000000000007462557EF4E3D4FFEFDC
      CDFFD5A77CFFD09F75FFFBF8F5FFFCF8F5FFFCF8F5FFFBF8F5FFD1A780FFCFA3
      79FFEAD5C2FFEAD4C1FF6752407E00000000A7A7A7EAF7F7F7FFD0D0D0FFDCDC
      DCFFE8E8E8FFF9F9F9FFD4ECD7FF85CD8EFF61BF6DFF5EBB69FF5BB665FF77C1
      81FF73BD7CFF39773FB900000000000000000000000000000000000000000000
      000000000000000000000000000071574385D9AA87FFDAB193FFD8AF91FFCB98
      70FFC38F66FFC89B76FFB17048FF311A114E0000000000000000726960FFA396
      89FF94887BFF9E9185FF3C3732FF000000004A443EFF7C7268FF84786EFF3C37
      32FF2D2924A70202000C000000020000000000000000201C1822D6B9A0E5F6E9
      DDFFECD8C5FFD7AB80FFDCBA99FFF6ECE3FFF5ECE2FFE4C8ADFFD2A679FFE6CE
      B9FFF1E2D5FFC4A081E51D17132200000000A9A9A9EAF8F8F8FFFEFEFEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFEAEAEAFF65B1
      6EFB42864BC00000000000000000000000000000000000000000000000000000
      0000000000000000000005040406DBAA89F7E7C0A7FFE0B99EFFD8AB8AFFD2A4
      81FFCE9C75FFD1A583FFBD855AFF4D2E1A740000000000000000000000000000
      00007D7368E2C2B7ADFF635B53FF000000007A7066FFA79A8DFF8A7F74E40000
      00000000000000000000000000000000000000000000000000004F453C53E7C9
      AFF4F7EADFFFEEDED0FFE3C0A6FFD8AD88FFD7AB85FFDDBA9BFFEBD6C7FFF3E6
      D9FFD9B597F4483C325300000000000000008B8B8BBFE1E1E1FFFEFEFEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFCFCFCFFF63A1
      6AE0000200030000000000000000000000000000000000000000000000000000
      00000000000000000000000000006B574976E8BFA3FFE9C8AFFFE5C2A8FFE1BC
      A1FFDCB598FFD5AA89FFD0A381FFB07241FA0000000000000000000000000000
      0000897E73E2BBAFA3FF9C9084FF00000000AD9F92FF9C9084FF544E47DA0000
      0000000000000000000000000000000000000000000000000000000000005046
      3D53DDC1A9E6F9E9DCFFF6E8DDFFF3E5DAFFF3E5DAFFF5E7DCFFF5E4D6FFD4B4
      9AE64B4037530000000000000000000000001F1F1F2A979797D1D0D0D0FFE8E8
      E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC1C1C1FF848484C31212
      121B000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000008060609B7927CC6E7BA9CFFE4B596FFE0B1
      91FFDAAD8EFFDCB497FFCF9E78FF8B5F3DB70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000221E1B237A6A5F7DD4BAA4DBEACDB4F3E9CBB2F3D0B59EDB7666597D201C
      19230000000000000000000000000000000000000000050505073E3E3E557E7E
      7EAE9A9A9AD6B3B3B3FBB1B1B1F9909090CD767676A930303046020202030000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000040303041C17131E342A23394637
      2D4E654F3E74D4A37DFA967053B7050301060000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000808080D1C1C1C311A1A1A2F0404040700761EF50279
      1CFF000F02210000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000F0E
      3B41020207080000000000000000000000000000000000000000000000000101
      070806063A410000000000000000000000000000000000000000000000001515
      15216363639D959595F0A0A0A0FFAAAAAAFFA6A6A6FF949494FF05812CFE41A0
      5DFF006D1BE60011032700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000013123C414D4A
      F2FF3E3CEBFD0202070800000000000000000000000000000000010107082220
      E1FC2F2DEAFF07063A41000000000000000000000000000000002F2F2F4A9D9D
      9DF4CECECEFFEDEDEDFF209650FF1A9048FF148E42FF0F8A3AFF389E5CFF7EC0
      95FF44A260FF02721DEC0013032A000000000000000000000000000000000606
      7685181EDAE91410CBE00A054156000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001B49
      68853D90C2E9317BB2E00C213756000000000000000000000000000000000000
      0000000000000000000000000000000000000000000014133D415654F5FF615F
      FAFF5653F6FF3F3DEAFC020207080000000000000000010107082A28E3FC3F3D
      F1FF4A48F6FF2F2DEAFF07063A410000000000000000000000009A9A9AEADEDE
      DEFFF3F3F3FFDBDBDBFF279A59FF8FCAA8FF8CC8A4FF89C5A0FF87C49DFF68B5
      84FF81C196FF46A464FF00711DEA001605300000000000000000000000001320
      C5C9588AFEFF0F4DFEFF1113DDF30A05435D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003788
      B3C985E9F9FF4BD9F5FF368AC2F30D22395D0000000000000000000000000000
      000000000000000000000000000000000000000000000E0D282B5956F6FF6360
      FAFF6F6EFFFF5754F6FF3F3EEBFC02020708020207083330E6FC4543F2FF6160
      FFFF4846F4FF2D2BE9FF0605252B0000000000000000000000009D9D9DEAF0F0
      F0FFDEDEDEFFD4D4D4FF2F9E61FF93CDACFF6DB98DFF69B788FF64B584FF5FB2
      7EFF65B481FF82C197FF3A9F5AFF007C23FC000000000000000000000000161F
      CBCF829FFEFF0D48FEFF0D4BFEFF1114DEF50D04456203010B0D02010B0D0000
      000000000000000000000000000000000000000000000000000000000000398A
      B6CFA0E6F8FF36D2F2FF45D6F6FF378DC5F50D223A620103090D01040A0D0000
      00000000000000000000000000000000000000000000000000000E0E282B5957
      F6FF6461FAFF726FFFFF5856F6FF403FEBFC3C3AEAFD4E4BF4FF6665FFFF4E4C
      F5FF3432EBFF0706262B000000000000000000000000000000009F9F9FEAF2F2
      F2FFE2E2E2FFD8D8D8FF35A269FF95CEAFFF93CDACFF90CBA9FF8FCBA7FF72BB
      8FFF89C7A0FF44A466FF068533FD0008020F0000000000000000000000000505
      595C0B0FC8CE7795FEFF1657FEFF0D50FEFF0D0DDEF41E0ED9E81E0EDDE80D04
      4153000000000000000000000000000000000000000000000000000000001036
      4F5C2D7FB3CE98E2F6FF51DCF5FF44D9F6FF3389C3F42F76BCE83077BFE80A1D
      3653000000000000000000000000000000000000000000000000000000000E0E
      282B5A58F6FF6562FAFF7270FFFF716EFFFF6E6CFFFF6C6AFFFF5553F7FF3D3B
      EEFF0707262B0000000000000000000000000000000000000000A0A0A0EAF3F3
      F3FFE7E7E7FFDDDDDDFF3BA46DFF36A26CFF32A066FF2E9C60FF53AE7AFF90CB
      A9FF4DAA72FF168E44FF0006030C000000000000000000000000000000000000
      000004033234121AC6CA1B2EDBDE275BFEFF0F52FEFF215FFEFF1B57FEFF210D
      E8F70F0444590000000000000000000000000000000000000000000000000000
      0000081E2C343686B2CA449FC7DE5CD9F2FF4CDBF6FF59DDF7FF53D8F5FF2B7A
      C6F70B1D37590000000000000000000000000000000000000000000000000000
      00000E0E282B5B59F7FF7774FFFF5754FFFF5552FFFF706EFFFF4644F0FF0908
      272B000000000000000000000000000000000000000000000000A2A2A2EAF4F4
      F4FFEAEAEAFFE1E1E1FFDDDDDDFFE3E3E3FFDEDEDEFFC9C9C9FF349F65FF58B2
      80FF269755FF8C918EEB00000000000000000000000000000000000000000000
      00000000000000000000060681866285FEFF356DFEFF4175FEFF2763FEFF1B57
      FEFF2A12E3F30000000000000000000000000000000000000000000000000000
      000000000000000000001A50738688DDF4FF68E0F6FF71E2F7FF5DDFF6FF53DA
      F6FF3477C3F30000000000000000000000000000000000000000000000000000
      0000030308085A57F4FD7B77FFFF5C59FFFF5956FFFF7472FFFF4441EDFD0202
      0708000000000000000000000000000000000000000000000000A4A4A4EAF5F5
      F5FFEEEEEEFFE6E6E6FFE2E2E2FFE6E6E6FFE1E1E1FFCDCDCDFF3AA36CFF2F9E
      63FFDDE1DEFF929292EA00000000000000000000000000000000000000000000
      000000000000000000000A0DAAAF89AAFEFF4F81FEFF759AFEFF576FFEFF5482
      FEFF1612E8F00000000000000000000000000000000000000000000000000000
      00000000000000000000276E97AFA8EEF9FF7CE6F8FF99E8F8FF7CD1F0FF7EE2
      F6FF3A8ECCF00000000000000000000000000000000000000000000000000303
      08086360F6FC6E6BFBFF7E7CFFFF7C79FFFF7A77FFFF7775FFFF5C5AF7FF4441
      ECFC020207080000000000000000000000000000000000000000A5A5A5EAF6F6
      F6FFEBEBEBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC2C2C2FFBBBBBBFFBFBF
      BFFFE5E5E5FF949494EA00000000000000000000000000000000000000000000
      0000000000000000000007056366304BFCFD80A7FEFF5874FEFF5B66FEFF2727
      EEF50C044B590000000000000000000000000000000000000000000000000000
      00000000000000000000113C57665ABEE8FDA2F0FBFF7ED4F0FF7CC7ECFF4B9B
      D4F50B253F590000000000000000000000000000000000000000040308086B68
      F9FC7572FDFF8581FFFF7471FCFF6260F8FF5E5BF7FF6B68FAFF7977FFFF5E5B
      F7FF4542ECFC0202070800000000000000000000000000000000A7A7A7EAF7F7
      F7FFE7E7E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC2C2
      C2FFE6E6E6FF959595EA00000000000000000000000000000000000000000000
      000000000000000000000000000008066C70465EFEFFB4C8FEFF263AECED0703
      4248000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000113F5E706DC9ECFFC9F3FBFF4FACD6ED0925
      3948000000000000000000000000000000000000000004040808716EFCFD7B78
      FEFF8986FFFF7A77FDFF6A67FBFF100F292B0F0E292B5F5CF8FF6C6AFAFF7B78
      FFFF5F5DF7FF4643EDFC00000505000000000000000000000000A9A9A9EAF8F8
      F8FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFEAEAEAFF989898EA00000000000000000000000000000000000000000000
      00000000000000000000000000000000020208088589121EC7CB06055F630000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000010202195275893687B3CB0F3A55630000
      000000000000000000000000000000000000000000000D0D1F1F7875FFFF807C
      FFFF807CFEFF726FFDFF1111292B00000000000000000F0F292B605DF8FF6D6B
      FBFF7C7AFFFF605DF8FF201D686F0101020200000000000000008B8B8BBFE1E1
      E1FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFCFCFCFFF6A6A6AA100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D0D1F1F7875
      FFFF7774FEFF12122B2B000000000000000000000000000000000F0F292B625F
      F8FF6866F9FF3634A0A80C0C27290000000000000000000000001F1F1F2A9797
      97D1D0D0D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC1C1
      C1FF848484C31212121B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000D0D
      1F1F13122B2B0000000000000000000000000000000000000000000000000F0F
      292B27276B6F14143C3E00000000000000000000000000000000000000000505
      05073E3E3E557E7E7EAE9A9A9AD6B3B3B3FBB1B1B1F9909090CD767676A93030
      3046020202030000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000003030A0A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008F5026C5C28356FFD38A66FFE18E6EFFDC8C6AFFDA8A
      6BFFD7896CFFCD8A6AFFAA6B42FFA55D2CFF0000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C381C881664
      33F2176935FF166433F20C381C88000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C58253FFEFCEB9FFDDFFFFFF86EEC7FFA1F4D7FFA1F6
      D7FF8BEEC7FFE0FFFFFFDDA184FFAA683CFF000000000000000004090B0D0A08
      08136A2E11988E371AE3A2411FF5A74320FAA64120FA59542BFB268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A37856C4CA9167F4D19566FFCE91
      61FFCB8D5CFFC98959FFC27D4FFFEFB599FFEAF3E8FF4FBE83FF6DC997FF6FC9
      98FF52BE83FFE4F4E9FFDD9B79FFA96738FF00000000324C64792375C9FB835D
      5DFBBE5E33FFFEB85FFFFEB860FFFEB860FFFEB860FF206C38FF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7A073FFF8F2EDFFF7F0EAFFF6ED
      E6FFF4EAE2FFF3E7DEFFC38052FFEAB596FFF3F3EAFFEDF1E6FFEFF1E6FFEFF0
      E6FFEDF1E5FFF3F5EDFFD59B77FFAF6E42FF00000000277BD0FE81B9EEFF9E64
      56FFF5BA83FFFFAB59FFFEA758FFFEA155FFFE9B51FF2F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF0000000000000000000000005E34
      1685AF6538E9A15F2DE0321D0B56000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000165E
      368538AF69E92DA162E00B321F56000000000000000000000000000000000000
      000000000000000000000000000000000000D9A378FFF9F3EEFFEBD2BDFFFFFF
      FFFFEBD3BEFFFFFFFFFFC98A5FFFE6B491FFE2A680FFE1A680FFDEA27BFFDCA0
      79FFDB9E77FFD99D75FFD49971FFBA7C55FF000000002679CBFC76B2EAFFB29D
      93FFFFB65EFFFFB561FFFEB15FFFFEAB5BFFFEA457FF48875BFF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F7000000000000000000000000A359
      33C9E28878FFDF6145FFB16433F3331E0A5D0000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000033A3
      5DC978E28CFF45DF66FF33B168F30A331F5D0000000000000000000000000000
      000000000000000000000000000000000000DDA77CFFF9F3EFFFEBD0B9FFEBD0
      BAFFEBD0BAFFEBD0BAFFCA8C63FFEAB798FFDDA47CFFDDA57EFFDBA27AFFD99F
      78FFD99F77FFD89E76FFD89D76FFBE835BFF0000000031190C50895242FFFCC8
      AAFFFFD197FFFEC76BFFFEBE66FFFEB862FFFEB05CFFA69A5FFF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF0D391E8C000000000000000000000000A55C
      33CFE2A391FFDB5032FFDF5B3EFFB26532F535210B620805010D0905010D0000
      00000000000000000000000000000000000000000000000000000000000033A5
      60CF91E2A6FF32DB56FF3EDF61FF32B269F50B3522620108050D0109050D0000
      000000000000000000000000000000000000DFA981FFF9F3EFFFEACEB6FFFFFF
      FFFFEBD0BAFFFFFFFFFFC8875BFFEFBEA0FFFDFCFAFFFEFCFBFFFEFDFDFFFEFD
      FCFFFDFBFAFFFDFCFBFFDDA784FFC07D51FF00000000010000013F190460C34A
      1DFFF6E4D6FFFFE4A3FFFFD470FFFFC967FFFFBF61FFFFB55DFFADAA76FF5E96
      70FF4D8D64FF3E7E52F71739227E000000000000000000000000000000004725
      0F5CA2592ACEDF9D8CFFDF624AFFDF583EFFB2652EF4AB6A2AE8AD6B2BE8311E
      0953000000000000000000000000000000000000000000000000000000000F47
      275C2AA25DCE8CDFA0FF4ADF67FF3EDF5EFF2EB268F42AAB6EE82BAD6FE80931
      1F5300000000000000000000000000000000E1AD86FFFAF4F0FFEACBB1FFEACC
      B2FFEACCB2FFEACCB2FFC78559FFEFBF9DFFFFFFFFFFCC926CFFFFFFFFFFFFFF
      FFFFFFFBF7FFFFF8F1FFE4AE8BFFC7895FFF000000000000000005010007431C
      0769BB461AFFF4E2D4FF4C79A8FF4B79A7FF4B79A7FF4C79A8FFF3D6C2FFBD44
      1AFF441B076F0603010A00000000000000000000000000000000000000000000
      000028140734A25A32CAB4623DDEDB6C53FFDF5F45FFDF6A50FFDF664CFFB46D
      27F733210A590000000000000000000000000000000000000000000000000000
      00000728153432A25ECA3DB467DE53DB70FF45DF63FF50DF6EFF4CDF6CFF27B4
      71F70A332259000000000000000000000000E3B08BFFFAF6F1FFEAC9ADFFFFFF
      FFFFEAC9AFFFFFFFFFFFCC8C63FFF3CDAFFFFFFFFFFFE3C7B2FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFEABEA0FFC9885EFF0000000000000000000000000300
      0005422416A2326BA6FF9BCCF8FFAED4F7FFAED4F7FFA4CFF6FF3272ADFF4528
      1DAD050200090000000000000000000000000000000000000000000000000000
      0000000000000000000068381886DE917CFFDF7560FFDF7A67FFDF6C55FFDF66
      4CFFB17330F30000000000000000000000000000000000000000000000000000
      00000000000000000000186839867CDE95FF60DF79FF67DF80FF55DF70FF4CDF
      6CFF30B178F3000000000000000000000000E5B38EFFFAF6F2FFE9C5A9FFE9C5
      ABFFEAC7ABFFE9C7ACFFD4966CFFD49D79FFD0976FFFD6A381FFCD8D66FFCD8F
      67FFD09973FFD19871FFC88A60FF241206360000000000000000000000000000
      000020446FC4A5CAEEFFAACCEAFFA6D0F6FFA7D0F6FFAACCEAFFA6CDEEFF234E
      7ACC000000000000000000000000000000000000000000000000000000000000
      000000000000000000008A4A23AFE2A499FFE28371FFE29C8DFFDB9072FFDF88
      74FFB96E36F00000000000000000000000000000000000000000000000000000
      00000000000000000000238A4DAF99E2A6FF71E286FF8DE29FFF72DB93FF74DF
      8CFF36B972F0000000000000000000000000E7B693FFFBF7F4FFE9C2A5FFFFFF
      FFFFE8C3A8FFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFFFFFFFFFF
      FFFFF7F1EBFFCB8E5DFF00000000000000000000000000000000000000000000
      00001B568FEDD9E8F7FF96C4F1FF8DBAE5FF7DA8D1FF88B4DFFFCDDFEEFF2060
      9DF1010405060000000000000000000000000000000000000000000000000000
      000000000000000000004F291166D27852FDE49E93FFDB8F74FFD69571FFC177
      45F53A210A590000000000000000000000000000000000000000000000000000
      00000000000000000000114F2B6652D27CFD93E4A1FF74DB91FF71D699FF45C1
      7DF50A3A2259000000000000000000000000E9B997FFFBF7F4FFE9C2A5FFE9C2
      A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2
      A5FFFBF7F4FFCE9262FF00000000000000000000000000000000000000000000
      00000B3C86FF7A96B7FF89B6E4FF6F9BC8FF133E6CFF174270FF204369FF1038
      62FA010204070000000000000000000000000000000000000000000000000000
      0000000000000000000000000000562B0E70D68564FFE4BFB8FFC37047ED351B
      0948000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000E562E7064D689FFB8E4C0FF47C372ED0935
      1C4800000000000000000000000000000000EBBC9AFFFBF7F4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFBF7F4FFD19668FF00000000000000000000000000000000000000000000
      00000E4996FF11569EFF0E4889FF0E4986FF104986FF134A84FF113F73FF0E2F
      55F1000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000201000269371789A35932CB4C260F630000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000002010217693A8932A35DCB0F4C29630000
      000000000000000000000000000000000000ECBE9DFFFBF7F4FF9BD5A4FF97D3
      A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C07EFF74BD
      7AFFFBF7F4FFD49A6DFF00000000000000000000000000000000000000000000
      000007162F77104C94FE11569AFF115698FF105192FF0E4886FF0D3C6FFE0A16
      2481000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBB193EBFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFD19B6FF800000000000000000000000000000000000000000000
      0000000000000818327D114386F40F498FFF0E4689FF103E76F509192E840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000765E507ED4AB8FE3EDBF9EFFEBBD
      9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB83FFDDA8
      7EFFDCA47BFFAC805FCA00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D275C78023A
      A1DF0340BAFE023DA4E30020587A000000000000000000000000000000000000
      0000103951F7265C84FB4685B9FB316A8EC1050F182200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001B4968853D90C2E9317BB2E00C213756000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A37856C4CA9167F4D19566FFCE9161FFC78654FFC28350FFC283
      50FFC28350FFC28350FFC28350FF7F502CB000000000A37856C4CA9167F4D195
      66FFCE9161FFCB8D5CFFC98959FFC78654FFC28350FF6C6B8AFF2563C7FF1F75
      E6FF0477EAFF0062DDFF044BBAFE0020587A00000000A37856C4CA9167F4D195
      66FF2C6481FF93C7F9FF90C9F9FF3F84C9FF2264A5FFA78163FFC28350FFC283
      50FFC28350FFC28350FF7F502CB0000000000000000000000000000000000000
      000000000000000000003788B3C985E9F9FF4BD9F5FF368AC2F30D22395D0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D7A073FFF8F2EDFFF7F0EAFFF6EDE6FFF1E4DBFFF0E2D8FFF0E2
      D8FFF0E2D8FFF0E2D8FFF0E2D8FFC2885AFD00000000D7A073FFF8F2EDFFF7F0
      EAFFF6EDE6FFF4EAE2FFF3E7DEFFF1E4DBFFF0E2D8FF1B54BBFF619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF023CA5E400000000D7A073FFF8F2EDFFF7F0
      EAFF4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF458BC2FFD0D2D7FFF0E2
      D8FFF0E2D8FFF0E2D8FFC2885AFD00000000A37856C4CA9167F4D19566FFCE91
      61FFCB8D5CFFC98959FF5EA3C7FFA0E6F8FF36D2F2FF45D6F6FF3E92C8FF8572
      6BFFB98155FF7C5134B40000000000000000000000000000000000000000956D
      4DB1B6835DDCE1AD86FFFAF4F0FFEACBB1FFEACCB2FFE8C7ABFFE8C7ABFFE8C7
      ABFFE8C8AFFFE8C8ADFFF0E2D8FFC38552FF00000000D9A378FFF9F3EEFFEBD2
      BDFFFFFFFFFFEBD3BEFFFFFFFFFFFFFFFFFFFFFFFFFF0441BBFFADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BAFE00000000D9A378FFF9F3EEFFEBD2
      BDFFA6C4D9FF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4F9CDDFFE4F0
      FAFFFFFFFFFFF0E2D8FFC58B5DFF00000000D7A073FFF8F2EDFFF7F0EAFFF6ED
      E6FFF4EAE2FFF3E7DEFFAAC9DBFF5BAADCFF98E2F6FF51DCF5FF44D9F6FF3D93
      CDFF448BD0FF4184C7FF0A1D365300000000000000000000000000000000C290
      68E6DFDAD5E6E3B08BFFFAF6F1FFEAC9ADFFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF1E5DBFFC58553FF00000000DDA77CFFF9F3EFFFEBD0
      B9FFEBD0BAFFEBD0BAFFEBD0BAFFEBD0BAFFEBD1BCFF2054B7FF8CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF023AA0DE00000000DDA77CFFF9F3EFFFEBD0
      B9FFEBD0BAFFA6B6B8FF74B8D5FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4796
      DAFFD4C4B8FFF0E2D8FFC5895AFF00000000D9A378FFF9F3EEFFEBD2BDFFFFFF
      FFFFEBD3BEFFFFFFFFFFFFFFFFFFD4E9F7FF66AFD6FF65C0E8FF5CD9F2FF4CDB
      F6FF59DDF7FF53D8F5FF2B7AC6F70B1D3759000000008361459DA17453C3DBA7
      81FAF2E7E1FAE5B38EFFFAF6F2FFE9C5A9FFE9C5ABFFE8C7ABFFE8C7ABFFE9C9
      AFFFE8C8AFFFE8CCB4FFF2E7DEFFC88957FF00000000DFA981FFF9F3EFFFEACE
      B6FFFFFFFFFFEBD0BAFFFFFFFFFFFFFFFFFFFFFFFFFF8A96BEFF3B74D2FF8CB4
      F7FFB7D6FEFF70A7F5FF2D69CAFF021C4F6D00000000DFA981FFF9F3EFFFEACE
      B6FFFFFFFFFFEBD0BAFFB0D6E7FF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4A99DEFFCAD0D8FFC88C5DFF00000000DDA77CFFF9F3EFFFEBD0B9FFEBD0
      BAFFEBD0BAFFEBD0BAFFEBD0BAFFEBD1BCFFEACDB4FF89B1C8FF88DDF4FF68E0
      F6FF71E2F7FF5DDFF6FF53DAF6FF3477C3F300000000AC805CCCC6C2BECCE1B1
      8FFAF5F0EBFAE7B693FFFBF7F4FFE9C2A5FFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF7F1EBFFCB8E5DFF00000000E1AD86FFFAF4F0FFEACB
      B1FFEACCB2FFEACCB2FFEACCB2FFEACCB2FFEACEB6FFE8C7ABFF8892B6FF2659
      BDFF0441BBFF1C55BCFF676583FF0000000000000000E1AD86FFFAF4F0FFEACB
      B1FFEACCB2FFEACCB2FFEACCB2FFAFC3BEFF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4E9FDEFFAB8669FF00000000DFA981FFF9F3EFFFEACEB6FFFFFF
      FFFFEBD0BAFFFFFFFFFFFFFFFFFFFFFFFFFFEACFB9FF75BBE4FFA8EEF9FF7CE6
      F8FF99E8F8FF7CD1F0FF7EE2F6FF3A8ECCF000000000B48A6BCCC8C3C0CCE1B1
      8EFAF4EEE8FAE9B997FFFBF7F4FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2
      A5FFE9C2A5FFE9C2A5FFFBF7F4FFCE9262FF00000000E3B08BFFFAF6F1FFEAC9
      ADFFFFFFFFFFEAC9AFFFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFF1E5DBFFC58553FF0000000000000000E3B08BFFFAF6F1FFEAC9
      ADFFFFFFFFFFEAC9AFFFFFFFFFFFFFFFFFFFC0EBF7FF7BD4EDFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF6398C9FF0C161D26E1AD86FFFAF4F0FFEACBB1FFEACC
      B2FFEACCB2FFEACCB2FFEACCB2FFEACEB6FFE8C7ABFF9DB2BDFF5CC0EAFFA2F0
      FBFF7ED4F0FF7CC7ECFF4B9BD4F50B253F5900000000B68D6FCCC8C4C0CCE2B3
      92FAF6F3F0FAEBBC9AFFFBF7F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFBF7F4FFD19668FF00000000E5B38EFFFAF6F2FFE9C5
      A9FFE9C5ABFFEAC7ABFFE9C7ACFFE9C9ADFFE9C9AFFFE8C7ABFFE9C9AFFFE8C8
      AFFFE8CCB4FFF2E7DEFFC88957FF0000000000000000E5B38EFFFAF6F2FFE9C5
      A9FFE9C5ABFFEAC7ABFFE9C7ACFFE9C9ADFFE9C9AFFFA5C0BDFF80D5EDFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF7E3B08BFFFAF6F1FFEAC9ADFFFFFF
      FFFFEAC9AFFFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFA0CFEEFF6DC9
      ECFFC9F3FBFF5DB5DDFF092539480000000000000000B78F72CCC8C4C2CCE4B6
      95FAF5EEE9FAECBE9DFFFBF7F4FF9BD5A4FF97D3A0FF8ACB92FF81C588FF7CC2
      83FF78C07EFF74BD7AFFFBF7F4FFD49A6DFF00000000E7B693FFFBF7F4FFE9C2
      A5FFFFFFFFFFE8C3A8FFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFF7F1EBFFCB8E5DFF0000000000000000E7B693FFFBF7F4FFE9C2
      A5FFFFFFFFFFE8C3A8FFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFB0E6F5FF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E8E5B38EFFFAF6F2FFE9C5A9FFE9C5
      ABFFEAC7ABFFE9C7ACFFE9C9ADFFE9C9AFFFE8C7ABFFE9C9AFFFE7C8AFFF85B0
      C9FF67B6E0FF8A8E8AFF000000000000000000000000B89276CCC8C6C3CCE6B8
      98FAF6F3F0FAEFC5A7FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFD19B6FF800000000E9B997FFFBF7F4FFE9C2
      A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2
      A5FFE9C2A5FFFBF7F4FFCE9262FF0000000000000000E9B997FFFBF7F4FFE9C2
      A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFACBC
      B7FF56A4D8FF84B0DBFF449CD0FF0F374D5EE7B693FFFBF7F4FFE9C2A5FFFFFF
      FFFFE8C3A8FFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFFFFFFFFFF
      FFFFF7F1EBFFCB8E5DFF000000000000000000000000BA9479CCC8C6C3CCE7BA
      9BFAF5EEE9FAC6C8A1FCE5C1A0FEEDBF9EFFEBBD9CFFE7B692FFE4B18BFFE2AE
      87FFE0AB83FFDDA87EFFDCA47BFFAC805FCA00000000EBBC9AFFFBF7F4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFBF7F4FFD19668FF0000000000000000EBBC9AFFFBF7F4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFBF7F4FFD19668FF00000000E9B997FFFBF7F4FFE9C2A5FFE9C2
      A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2
      A5FFFBF7F4FFCE9262FF000000000000000000000000BC967BCCC8C6C3CCE7C3
      A7F6F6F3F0FAF6F3F0FAF6F3F0FAF6F3F0FAF6F3F0FAF6F3F0FAF6F2EFFAF3EA
      E4FAE2DEDCE6BD8C64E0000000000000000000000000ECBE9DFFFBF7F4FF9BD5
      A4FF97D3A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C0
      7EFF74BD7AFFFBF7F4FFD49A6DFF0000000000000000ECBE9DFFFBF7F4FF9BD5
      A4FF97D3A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C0
      7EFF74BD7AFFFBF7F4FFD49A6DFF00000000EBBC9AFFFBF7F4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFBF7F4FFD19668FF000000000000000000000000BC987ECCC8C6C3CCAFB4
      91E3D8BB9BF5E1BC9AFADEBA97FADAB38EFAD7AF87FAD5AB83FADDAD89FAD7A4
      7BFAC6946FE69C7455B6000000000000000000000000DBB193EBFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFD19B6FF80000000000000000DBB193EBFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFD19B6FF800000000ECBE9DFFFBF7F4FF9BD5A4FF97D3
      A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C07EFF74BD
      7AFFFBF7F4FFD49A6DFF000000000000000000000000AE8E76BCC8C6C3CCC8C6
      C3CCC8C6C3CCC8C6C3CCC8C6C3CCC8C6C3CCC8C6C3CCC8C6C3CCC8C6C3CCA77A
      58C60000000000000000000000000000000000000000765E507ED4AB8FE3EDBF
      9EFFEBBD9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB
      83FFDDA87EFFDCA47BFFAC805FCA0000000000000000765E507ED4AB8FE3EDBF
      9EFFEBBD9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB
      83FFDDA87EFFDCA47BFFAC805FCA00000000DBB193EBFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFD19B6FF80000000000000000000000005F4C4065AB8973B6BE99
      7ECCBC977DCCB89275CCB68E6FCCB48B6CCCB38969CCB08666CCB08362CC8967
      4CA2000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000765E507ED4AB8FE3EDBF9EFFEBBD
      9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB83FFDDA8
      7EFFDCA47BFFAC805FCA00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000103951F7265C84FB4685B9FB316A8EC1050F182200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000724
      2F37000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C381C881664
      33F2176935FF166433F20C381C88000000000000000004090B0D0A0808136A2E
      11982B6280FF93C7F9FF90C9F9FF3F84C9FF2160A3FF7F4237EE81381ABA0B08
      091604090B0D000000000000000000000000000000000000000000000000218F
      B8D5229AC9E902080A0C00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A37856C4CA9167F4D195
      66FFCE9161FFCB8D5CFFC98959FFC78654FFC28350FFC28350FFC28350FFC283
      50FFC28350FFC28350FF7F502CB00000000000000000A37856C4CA9167F4D195
      66FFCE9161FFCB8D5CFFC98959FFC78654FFC28350FF697542FF268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C324C64792375C9FB835D5DFBBE5E
      33FF4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF4787B6FF9B5240FF735E
      67F52B79C8F836536B8300000000000000000000000000000000000000000208
      0A0C2BADDFFF29AADEFF1258778A000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D7A073FFF8F2EDFFF7F0
      EAFFF6EDE6FFF4EAE2FFF3E7DEFFF1E4DBFFF0E2D8FFF0E2D8FFF0E2D8FFF0E2
      D8FFF0E2D8FFF0E2D8FFC2885AFD0000000000000000D7A073FFF8F2EDFFF7F0
      EAFFF6EDE6FFF4EAE2FFF3E7DEFFF1E4DBFFF0E2D8FF206E3CFF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F7277BD0FE81B9EEFF9E6456FFF5BA
      83FFA69172FF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4F95D2FF9562
      5BFF82BBEFFF2875C9FE00000000000000000000000000000000000000000000
      0000218DB5CF4DBBE7FF4AB9E6FF229ACBED020F141700000000000000000000
      00000000000000000000000000000000000000000000D9A378FFF9F3EEFFEBD2
      BDFFFFFFFFFFEBD3BEFFFFFFFFFFFFFFFFFFFFFFFFFFEAC7ACFFFFFFFFFFFFFF
      FFFFFFFFFFFFF0E2D8FFC58B5DFF0000000000000000D9A378FFF9F3EEFFEBD2
      BDFFFFFFFFFFEBD3BEFFFFFFFFFFFFFFFFFFFFFFFFFF2F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF2679CBFC76B2EAFFB29D93FFFFB6
      5EFFFFB561FFB2A27BFF74B7D3FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4594
      D8FF73B1EAFF2770C3FA00000000000000000000000000000000000000000000
      00000000000029AEDFFF83D3F2FF53BCE7FF2CA9DEFF135C7C90000000000000
      00000000000000000000000000000000000000000000DDA77CFFF9F3EFFFEBD0
      B9FFEBD0BAFFEBD0BAFFEBD0BAFFEBD0BAFFEBD1BCFFEACDB4FFEACDB4FFEACD
      B4FFEACDB4FFF0E2D8FFC5895AFF0000000000000000DDA77CFFF9F3EFFFEBD0
      B9FFEBD0BAFFEBD0BAFFEBD0BAFFEBD0BAFFEBD1BCFF47885EFF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F731190C50895242FFFCC8AAFFFFD1
      97FFFEC76BFFFEBE66FFAFA67EFF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4490D4FF3433398000000000000000000000000000000000000000000000
      0000000000002185A8C06ECCEEFF82D2F2FF7CCEF1FF48B5E4FF209BCFF1020F
      14170000000000000000000000000000000000000000DFA981FFF9F3EFFFEACE
      B6FFFFFFFFFFEBD0BAFFFFFFFFFFFFFFFFFFFFFFFFFFEACFB9FFFBF6F2FFFFFF
      FFFFFFFFFFFFF0E2D8FFC88C5DFF0000000000000000DFA981FFF9F3EFFFEACE
      B6FFFFFFFFFFEBD0BAFFFFFFFFFFFFFFFFFFFFFFFFFF9CAE90FF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF0D391E8C010000013F190460C34A1DFFF6E4
      D6FFFFE4A3FFFFD470FFFFC967FFBEBA85FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF398ACBE80C1A27300000000033B5D9F033BADFF832B7DEF830B5
      DDF82EB2DCF82CB0DBF885D7F3FF2DB5EBFF48BBECFF7ECEF1FF4FB8E6FF2AA7
      DDFF1463869D00000000000000000000000000000000E1AD86FFFAF4F0FFEACB
      B1FFEACCB2FFEACCB2FFEACCB2FFEACCB2FFEACEB6FFE8C7ABFFE8C7ABFFE8C8
      AFFFE8C8ADFFF0E2D8FFC38552FF0000000000000000E1AD86FFFAF4F0FFEACB
      B1FFEACCB2FFEACCB2FFEACCB2FFEACCB2FFEACEB6FFE8C7ABFFA1AD8DFF5D96
      6FFF4D8D64FF47885DFF797D4DFF000000000000000005010007431C0769BB46
      1AFFF4E2D4FF4C79A8FF4B79A7FF4B79A7FF408BB9FF7BD4EEFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF548FC2EC0C161D2633B2D4E973DAF2FF92E6F8FF90E3
      F7FF8CE0F6FF89DCF5FF89DBF5FF87D7F4FF83D3F2FF7DCFF1FF7ACCF0FF78C9
      EFFF46B3E3FF209BD1F503161E230000000000000000E3B08BFFFAF6F1FFEAC9
      ADFFFFFFFFFFEAC9AFFFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFF1E5DBFFC58553FF0000000000000000E3B08BFFFAF6F1FFEAC9
      ADFFFFFFFFFFEAC9AFFFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFF1E5DBFFC58553FF000000000000000000000000030000054224
      16A2326BA6FF9BCCF8FFAED4F7FFAED4F7FFA4CFF6FF2C89BEFF7BD1E9FDB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF70C2A32374DCDECFF97E9F9FF48D5
      F3FF43CFF1FF3ECAF0FF36C1EEFF88D9F4FF2CB2E0FE29ABD9F828A8D9F826A6
      D8F824A3D6F822A0D5F81E94C7E90000000000000000E5B38EFFFAF6F2FFE9C5
      A9FFE9C5ABFFEAC7ABFFE9C7ACFFE9C9ADFFE9C9AFFFE8C7ABFFE9C9AFFFE8C8
      AFFFE8CCB4FFF2E7DEFFC88957FF0000000000000000E5B38EFFFAF6F2FFE9C5
      A9FFE9C5ABFFEAC7ABFFE9C7ACFFE9C9ADFFE9C9AFFFE8C7ABFFE9C9AFFFE8C8
      AFFFE8CCB4FFF2E7DEFFC88957FF000000000000000000000000000000002044
      6FC4A5CAEEFFAACCEAFFA6D0F6FFA7D0F6FFAACCEAFFA6CDEEFF21729DDE75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E80000000036BEDFF47EE1F5FF8DE6
      F8FF41D2F3FF3DCDF1FF37C7EFFF8BDCF5FF56C5EAFF13516574000000000000
      00000000000000000000000000000000000000000000E7B693FFFBF7F4FFE9C2
      A5FFFFFFFFFFE8C3A8FFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFF7F1EBFFCB8E5DFF0000000000000000E7B693FFFBF7F4FFE9C2
      A5FFFFFFFFFFE8C3A8FFFFFFFFFFFFFFFFFFFFFFFFFFE8C7ABFFFFFFFFFFFFFF
      FFFFFFFFFFFFF7F1EBFFCB8E5DFF000000000000000000000000000000001B56
      8FEDD9E8F7FF96C4F1FF8DBAE5FF7DA8D1FF88B4DFFFCDDFEEFF20609DF1123B
      4B5756A4D8FF84B0DBFF449CD0FF0F374D5E0000000014414D5459D4EFFF98EA
      F9FF45D6F4FF40D0F2FF3BCBF0FF6CD5F3FF7DD7F3FF48BFE7FF0D34414A0000
      00000000000000000000000000000000000000000000E9B997FFFBF7F4FFE9C2
      A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2
      A5FFE9C2A5FFFBF7F4FFCE9262FF0000000000000000E9B997FFFBF7F4FFE9C2
      A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2A5FFE9C2
      A5FFE9C2A5FFFBF7F4FFCE9262FF000000000000000000000000000000000B3C
      86FF7A96B7FF89B6E4FF6F9BC8FF133E6CFF174270FF204369FF103862FA0102
      04070000000000000000000000000000000000000000000000003AC5E3F992E9
      F9FF70E1F7FF43D4F3FF3FCEF2FF3AC9F0FF89DCF5FF6ED0EFFF3BBAE4FF0619
      1F230000000000000000000000000000000000000000EBBC9AFFFBF7F4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFBF7F4FFD19668FF0000000000000000EBBC9AFFFBF7F4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFBF7F4FFD19668FF000000000000000000000000000000000E49
      96FF11569EFF0E4889FF0E4986FF104986FF134A84FF113F73FF0E2F55F10000
      00000000000000000000000000000000000000000000000000001F68798362D9
      F1FF99EBFAFF46D8F4FF42D3F3FF3DCEF1FF38C8F0FF8BDCF5FF60CBEDFF2FB5
      E0FC0000000000000000000000000000000000000000ECBE9DFFFBF7F4FF9BD5
      A4FF97D3A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C0
      7EFF74BD7AFFFBF7F4FFD49A6DFF0000000000000000ECBE9DFFFBF7F4FF9BD5
      A4FF97D3A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C0
      7EFF74BD7AFFFBF7F4FFD49A6DFF000000000000000000000000000000000716
      2F77104C94FE11569AFF115698FF105192FF0E4886FF0D3C6FFE0A1624810000
      0000000000000000000000000000000000000000000000000000000000003DCA
      E9FD99EDFAFF98EBF9FF96E8F9FF93E5F8FF90E2F7FF8DDFF6FF8ADBF5FF54C7
      EBFF2DAED8F300000000000000000000000000000000DBB193EBFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFD19B6FF80000000000000000DBB193EBFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7F4FFFBF7
      F4FFFBF7F4FFFBF7F4FFD19B6FF8000000000000000000000000000000000000
      00000818327D114386F40F498FFF0E4689FF103E76F509192E84000000000000
      0000000000000000000000000000000000000000000000000000000000002782
      95A23DCCEBFF3CCBEAFF3AC9E9FF39C7E9FF38C3E8FF36C1E7FF34BFE6FF33BC
      E5FF31BAE4FF2AA0C8E1000000000000000000000000765E507ED4AB8FE3EDBF
      9EFFEBBD9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB
      83FFDDA87EFFDCA47BFFAC805FCA0000000000000000765E507ED4AB8FE3EDBF
      9EFFEBBD9CFFEBBB99FFE9B995FFE7B692FFE6B48FFFE4B18BFFE2AE87FFE0AB
      83FFDDA87EFFDCA47BFFAC805FCA000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000103951F7265C84FB4685B9FB316A8EC1050F182200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008F5026C5C28356FFD38A66FFE18E6EFFDC8C6AFFDA8A
      6BFFD7896CFFCD8A6AFFAA6B42FFA55D2CFF673A1D8F834924B5AB6031EEB666
      33FFB46633FFB36532FFB16432FFAF6331FFAD6231FFAB6130FFA96030FFA85F
      30FFA75E2FFFA55E2FFE9C592DF1804924C416243039273E577A713925B0CB6E
      32FFD27C3FFFD37C3FFFD17A3FFFCD783DFF933D1DDF323A49801E384E5B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000296280FB93C7F9FF90C9F9FF3F84C9FF195DA1F30715212F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000808080D1C1C1C31C58253FFEFCEB9FFDDFFFFFF86EEC7FFA1F4D7FFA1F6
      D7FF8BEEC7FFE0FFFFFFDDA184FFAA683CFFA15B2DDEEBC5ACFFEAC4ACFFFEFB
      F8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFB
      F8FFFEFBF8FFC8997AFFC79777FF9A572CED3782C9E66F84ABFFC87C57FFFFB9
      64FFFFB65FFFFFAF5CFFFFB15BFFFFB35AFFDE8755FF86717CFF4A9AE6FF0000
      0000000000000000000000000000000000000000000000000000151515216363
      639D4188A9FFE0F2FFFF5299D8FF1878BDFF4797C4FF3A81B9FD4554619E0B0B
      0B160000000000000000000000000000000000000000151515216363639D9595
      95F0A0A0A0FFAAAAAAFFC27D4FFFEFB599FFEAF3E8FF4FBE83FF6DC997FF6FC9
      98FF52BE83FFE4F4E9FFDD9B79FFA96738FFB86935FEEDCAB2FFE0A178FFFEFA
      F7FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF
      87FFFDF9F6FFCA8C63FFC99A7AFFA55E2FFE3281CFFC92ABCCFFE7AE71FFFFB7
      5DFFFEAF5DFFFEA659FFFD9F53FFFF964BFFFD924BFFB29EA1FF4C95DEFF0000
      000000000000000000000000000000000000000000002F2F2F4A9D9D9DF4CECE
      CEFF9BB9CEFF78B5D5FF8FB6D1FF53C9E4FF59DFF5FF76D0EDFF4996D7FF727E
      88EA1C1C1C350000000000000000000000002F2F2F4A9D9D9DF4CECECEFFEDED
      EDFFF4F4F4FFF5F5F5FFC38052FFEAB596FFF3F3EAFFEDF1E6FFEFF1E6FFEFF0
      E6FFEDF1E5FFF3F5EDFFD59B77FFAF6E42FFBA6A36FFEECCB5FFE1A178FFFEFA
      F7FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDC
      C1FFFDF9F6FFCD8F66FFCC9D80FFA75F30FF32170A55AC6950F5FDD7B1FFFFD6
      83FFFFCA63FFFFBF5CFFFFB354FFFFA655FFFFC491FFD67B59FF4E2615960000
      000000000000000000000000000000000000000000009A9A9AEADEDEDEFFF3F3
      F3FFDBDBDBFF95B7C9FF73B8D6FFC1F6FDFF61DFF7FF5BE2F8FF77D3F0FF4696
      DAFF758089ED0000000000000000000000009A9A9AEADEDEDEFFF3F3F3FFDBDB
      DBFFD2D2D2FFDBDBDBFFC98A5FFFE6B491FFE2A680FFE1A680FFDEA27BFFDCA0
      79FFDB9E77FFD99D75FFD49971FFBA7C55FFBA6936FFEFCEB7FFE1A177FFFEFA
      F7FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF
      87FFFDF9F6FFCF9268FFCEA283FFA95F30FF0000000041170064C36C4CF3FFE8
      BFFFACA985FFA69D7BFF9F9173FFF7CA9BFFE49373FF692306A00601000A0000
      000000000000000000000000000000000000000000009D9D9DEAF0F0F0FFDEDE
      DEFFD4D4D4FFD2D2D2FF98BDCFFF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4493D7FE0A1C2C3400000000000000009D9D9DEAF0F0F0FFDEDEDEFFD4D4
      D4FFD2D2D2FFDBDBDBFFCA8C63FFEAB798FFDDA47CFFDDA57EFFDBA27AFFD99F
      78FFD99F77FFD89E76FFD89D76FFBE835BFFB96834FFEFD0BAFFE2A178FFFEFB
      F8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFB
      F8FFFEFBF8FFD3956BFFD2A689FFAA6030FF0000000000000000441800796975
      90FA71ACE7FF80B0E3FF7BB1E7FF6A97C3FF72532AFF5AA362FF57A766FF57A6
      65FF54A262FF327335DF40333A801E384E5B000000009F9F9FEAF2F2F2FFE2E2
      E2FFD8D8D8FFD5D5D5FFDCDCDCFFA3CCD9FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF398ACBE80C1A2730000000009F9F9FEAF2F2F2FFE2E2E2FFD8D8
      D8FFD5D5D5FFDCDCDCFFC8875BFFEFBEA0FFFDFCFAFFFEFCFBFFFEFDFDFFFEFD
      FCFFFDFBFAFFFDFCFBFFDDA784FFC07D51FFBA6834FFF0D2BDFFE2A278FFE2A2
      78FFE1A278FFE2A279FFE1A279FFE0A076FFDE9E75FFDD9E74FFDC9C72FFD99A
      70FFD8986FFFD6986EFFD5AA8DFFAC6131FF0000000000000000211D1F6670A2
      D5FEABD1F2FFADD7FDFFAFD4F5FF91BFE7FF528288FF77CF93FF75CE8DFF74CE
      8EFF74CE90FF69B372FF588E58FF4A9AE6FF00000000A0A0A0EAF3F3F3FFE7E7
      E7FFDDDDDDFFD9D9D9FFE0E0E0FFDBDBDBFF96BFCDFF7AD4EEFFC3F6FDFF6ADD
      F6FF6BCAEDFF61A2D7FF548FC2EC0C161D26A0A0A0EAF3F3F3FFE7E7E7FFDDDD
      DDFFD9D9D9FFE0E0E0FFC78559FFEFBF9DFFFFFFFFFFCC926CFFFFFFFFFFFFFF
      FFFFFFFBF7FFFFF8F1FFE4AE8BFFC7895FFFBA6834FFF2D5C1FFE3A278FFE3A2
      78FFE2A279FFE2A279FFE2A379FFE1A177FFE0A076FFDE9F75FFDE9D73FFDC9C
      72FFDA9A71FFD99A71FFDAAF94FFAE6231FF0000000000000000163963ABA6BE
      DBFF95C4F1FFA0D0FBFF95C5F3FFB5CFE4FF3F78A6FF76CE8EFF73CD88FF6ECB
      82FF68CB79FF68C977FF5E955FFF4C95DEFF00000000A2A2A2EAF4F4F4FFEAEA
      EAFFE1E1E1FFDDDDDDFFE3E3E3FFDEDEDEFFC9C9C9FF88B9C9FF7ED4EDFFB1E3
      F9FF8ABFE7FFADD3F6FFC3E0FCFF6199CCF7A2A2A2EAF4F4F4FFEAEAEAFFE1E1
      E1FFDDDDDDFFE3E3E3FFCC8C63FFF3CDAFFFFFFFFFFFE3C7B2FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFEABEA0FFC9885EFFBA6834FFF2D8C4FFE3A379FFE3A2
      78FFE3A378FFE2A379FFE2A279FFE1A279FFE1A177FFDF9F75FFDE9E74FFDD9D
      72FFDB9B70FFDC9C72FFDDB499FFB06332FF000000000000000025416DC44E67
      99FF617EADFF7AA5D6FF5371A4FF415B91FF426597FF7AD09EFF75CE96FF6FCD
      8DFF70CD87FF9CD8AAFF6BAF6EFF1F3C209600000000A4A4A4EAF5F5F5FFEEEE
      EEFFE6E6E6FFE2E2E2FFE6E6E6FFE1E1E1FFCDCDCDFFC1C1C1FF88BDCEFF75BD
      E7FFB3D2F0FFE5F3FFFFABD2EFFF407DB5E8A4A4A4EAF5F5F5FFEEEEEEFFE6E6
      E6FFE2E2E2FFE6E6E6FFD4966CFFD49D79FFD0976FFFD6A381FFCD8D66FFCD8F
      67FFD09973FFD19871FFC88A60FF24120636BA6934FFF4D9C7FFE6A57BFFC88B
      62FFC98C63FFC98D65FFCB916AFFCB916BFFCA8F67FFC88B63FFC88B62FFC88B
      62FFC88B62FFDA9B72FFE1B99EFFB26432FF0000000000000000080E1843425E
      93FD3F598FFF3F598FFF3F598FFF3F598FFF6D8E9DFF839892FF7B9388FF738B
      7DFFA3D5B0FF81BE84FF174D1CA00103010A00000000A5A5A5EAF6F6F6FFEBEB
      EBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC2C2C2FFBBBBBBFFBFBFBFFFAAD4
      E3FF56A4D8FF84B0DBFF449CD0FF0F374D5EA5A5A5EAF6F6F6FFEBEBEBFFDEDE
      DEFFD6D6D6FFD5D5D5FFD1D1D1FFC2C2C2FFBBBBBBFFBFBFBFFFE5E5E5FF9494
      94EA00000000000000000000000000000000B96934FEF4DCC9FFE7A67BFFF9EC
      E1FFF9ECE1FFF9EDE3FFFCF4EEFFFDFAF7FFFDF7F3FFFAEDE5FFF7E7DBFFF7E5
      D9FFF6E5D8FFDE9F75FFE4BDA3FFB36532FF0000000000000000000000000E17
      25613D5A8CF13F5B91FF405D93FB234452CC67738EFA71ACE7FF80B0E3FF7BB1
      E7FF6A97C3FF1D461FB60104010B0000000000000000A7A7A7EAF7F7F7FFE7E7
      E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC2C2C2FFE6E6
      E6FF959595EA000000000000000000000000A7A7A7EAF7F7F7FFE7E7E7FFEFEF
      EFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC2C2C2FFE6E6E6FF9595
      95EA00000000000000000000000000000000B76733FAF5DDCCFFE7A77CFFFAF0
      E8FFFAF0E8FFC98C64FFFAF0E9FFFDF8F3FFFEFAF8FFFCF4EFFFF9E9DFFFF7E7
      DBFFF7E5D9FFE0A176FFE7C1A8FFB56633FF0000000000000000000000000000
      0000000000000000000000000000211D1F6670A2D5FEABD1F2FFADD7FDFFAFD4
      F5FF91BFE7FF2A3E58AD000000000000000000000000A9A9A9EAF8F8F8FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFEAEA
      EAFF989898EA000000000000000000000000A9A9A9EAF8F8F8FFFEFEFEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFEAEAEAFF9898
      98EA00000000000000000000000000000000B06331F0F6DFD0FFE8A77CFFFCF6
      F1FFFCF6F1FFC88B62FFFAF1E9FFFBF4EEFFFDFAF7FFFDF9F6FFFAF0E8FFF8E8
      DDFFF7E6DBFFE1A278FFEFD5C2FFB56733FE0000000000000000000000000000
      0000000000000000000000000000072D528DB7D0E7FF95C4F1FFA0D0FBFF95C5
      F3FFB5CFE4FF2972A6DF0000000000000000000000008B8B8BBFE1E1E1FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFCFCF
      CFFF6A6A6AA10000000000000000000000008B8B8BBFE1E1E1FFFEFEFEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFCFCFCFFF6A6A
      6AA1000000000000000000000000000000009E592CD8F6DFD1FFE9A97EFFFEFA
      F6FFFDFAF6FFC88B62FFFBF3EEFFFBF1EAFFFCF6F2FFFEFBF8FFFCF6F1FFF9EC
      E2FFF8E7DBFFEED0B9FFECD0BCFFB56B3BF80000000000000000000000000000
      00000000000000000000000000000A35589232ADE5FF63B7E6FF8CC8F6FF39AC
      E6FF1BA6E5FF1F8CC6EF0000000000000000000000001F1F1F2A979797D1D0D0
      D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC1C1C1FF8484
      84C31212121B0000000000000000000000001F1F1F2A979797D1D0D0D0FFE8E8
      E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC1C1C1FF848484C31212
      121B00000000000000000000000000000000723F209BF6E0D1FFF7E0D1FFFEFB
      F8FFFEFBF7FFFDF9F6FFFCF5F0FFFAF0EAFFFBF2EDFFFDF9F6FFFDFAF7FFFBF1
      EBFFF7E8DEFEE8CCB9FBBA7E55EC452714630000000000000000000000000000
      000000000000000000000000000007121C431FA1DDFD17A5E5FF17A5E5FF17A5
      E5FF17A5E5FF133B569000000000000000000000000000000000050505073E3E
      3E557E7E7EAE9A9A9AD6B3B3B3FBB1B1B1F9909090CD767676A9303030460202
      02030000000000000000000000000000000000000000050505073E3E3E557E7E
      7EAE9A9A9AD6B3B3B3FBB1B1B1F9909090CD767676A930303046020202030000
      000000000000000000000000000000000000522E16716A3B1D9096542ACCAF62
      31EEB76733FAB96934FEBA6934FFBA6834FFBA6834FFBB6A37FFBC6C39FFBA6B
      38FFAE6233EF945831CB3C211154000000000000000000000000000000000000
      000000000000000000000000000000000000091F2E61218EC7F119A4E4FF1E9D
      DAFB1343619D0101010600000000000000000000000000000000000000000000
      00000000000001010102292929772E2E2E8A2C2C2C8A2B2B2B8A2A2A2A8A2828
      288A2727278A2525258A2424248A1B1B1B660000000000000000000000000000
      000000000000000000000808080D1D1D1D311A1A1A2F04040407000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000808080D1C1C1C311A1A1A2F040404070C381C881664
      33F2176935FF166433F20C381C88000000000000000000000000000000000000
      000000000000000000000808080D1C1C1C311A1A1A2F040404070D275C78023A
      A1DF0340BAFE023DA4E30020587A00000000081B2C4A19507BCF1E6197FF1E61
      97FF1E6197FF226194FF567187FFF7F7F7FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0
      F0FFF0F0F0FFF0F0F0FFEEEEEEFA242424840000000000000000000000001515
      15216363639D969696F0A1A1A1FFABABABFFA7A7A7FF959595FF808080E74A4A
      4A8A0C0C0C160000000000000000000000000000000000000000000000001515
      15216363639D959595F0A0A0A0FFAAAAAAFFA6A6A6FF537D62FF268B51FF62B9
      8CFF94D2B1FF62B98CFF268B51FF0D391E8C0000000000000000000000001515
      15216363639D959595F0A0A0A0FFAAAAAAFFA6A6A6FF5574ABFF2563C8FF1F75
      E6FF0477EAFF0062DDFF034ABAFC0020587A184B73C260A4D7FF63A7DAFF62A5
      D9FF60A3D8FF609ED1FF738DA3FFEFEFEFFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE6E6E6FFE6E6E6FFE7E7E7FA2424248400000000000000002F2F2F4A9E9E
      9EF4CECECEFFEDEDEDFFF4F4F4FFF5F5F5FFF4F4F4FFEFEFEFFFE2E2E2FFBABA
      BAFF7D7D7DE71C1C1C35000000000000000000000000000000002F2F2F4A9D9D
      9DF4CECECEFFEDEDEDFFF4F4F4FFF5F5F5FFF4F4F4FF206E3DFF60B98AFF5EB9
      86FFFFFFFFFF5EB886FF65BB8EFF176634F700000000000000002F2F2F4A9D9D
      9DF4CECECEFFEDEDEDFFF4F4F4FFF5F5F5FFF4F4F4FF1B56BEFF619CF4FF167D
      FFFF0074F8FF0074EEFF0266E1FF023CA5E41E6197FF66AADCFF468DCFFF448A
      CEFF4186CDFF4283C5FF6684A0FFF0F0F0FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3
      B3FFB3B3B3FFB2B2B2FFE8E8E8FA2626268400000000000000009B9B9BEADEDE
      DEFFF3F3F3FFDBDBDBFFD2D2D2FFDBDBDBFFD6D6D6FFC0C0C0FFC9C9C9FFE6E6
      E6FFC4C4C4FF7F7F7FEA000000000000000000000000000000009A9A9AEADEDE
      DEFFF3F3F3FFDBDBDBFFD2D2D2FFDBDBDBFFD6D6D6FF2F794AFF9BD4B5FFFFFF
      FFFFFFFFFFFFFFFFFFFF94D2B1FF176935FF00000000000000009A9A9AEADEDE
      DEFFF3F3F3FFDBDBDBFFD2D2D2FFDBDBDBFFD6D6D6FF0440BBFFADCDFEFFFFFF
      FFFFFFFFFFFFFFFFFFFF167DEFFF0340BAFE1E6197FF67ADDCFF4892D1FF468E
      D0FF448ACEFF4587C7FF6A87A2FFF0F0F0FFE8E8E8FFE8E8E8FFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE8E8E8FA2727278400000000000000009E9E9EEAF0F0
      F0FFDEDEDEFFD4D4D4FFD2D2D2FFDBDBDBFFD6D6D6FFBFBFBFFFB0B0B0FFB3B3
      B3FFDEDEDEFF848484EA000000000000000000000000000000009D9D9DEAF0F0
      F0FFDEDEDEFFD4D4D4FFD2D2D2FFDBDBDBFFD6D6D6FF46885EFF8FD3B0FF91D6
      B0FFFFFFFFFF63BB8BFF65BB8EFF176634F700000000000000009D9D9DEAF0F0
      F0FFDEDEDEFFD4D4D4FFD2D2D2FFDBDBDBFFD6D6D6FF1A52B9FF8CB4F6FF4B91
      FFFF1075FFFF1F85FFFF3E89EBFF023AA0DE1E6197FF69B0DEFF4B96D3FF4992
      D2FF468ED0FF488BC9FF6D8AA4FFF1F1F1FFB5B5B5FFB4B4B4FFB4B4B4FFB3B3
      B3FFB3B3B3FFB3B3B3FFE8E8E8FA2929298400000000000000009F9F9FEAF2F2
      F2FFE2E2E2FFD8D8D8FFD5D5D5FFDCDCDCFFD8D8D8FFC0C0C0FFB3B3B3FFB7B7
      B7FFE0E0E0FF8A8A8AEA000000000000000000000000000000009F9F9FEAF2F2
      F2FFE2E2E2FFD8D8D8FFD5D5D5FFDCDCDCFFD8D8D8FF87A693FF5FAA80FF94D4
      B3FFB9E6D0FF68BA8EFF2B8E55FF0D391E8C00000000000000009F9F9FEAF2F2
      F2FFE2E2E2FFD8D8D8FFD5D5D5FFDCDCDCFFD8D8D8FF728EC1FF3973D0FF8CB4
      F7FFB7D6FEFF70A7F5FF2B68C8FC021C4F6D1E6197FF6BB2DFFF4E9BD5FF4C97
      D3FF4993D1FF4A90CBFF6E8DA6FFF1F1F1FFE9E9E9FFE9E9E9FFE8E8E8FFE8E8
      E8FFE8E8E8FFE7E7E7FFE8E8E8FA2B2B2B840000000000000000A2A2A2EAF3F3
      F3FFE7E7E7FFDDDDDDFFD9D9D9FFE0E0E0FFDBDBDBFFC4C4C4FFB8B8B8FFBBBB
      BBFFE1E1E1FF8E8E8EEA00000000000000000000000000000000A0A0A0EAF3F3
      F3FFE7E7E7FFDDDDDDFFD9D9D9FFE0E0E0FFDBDBDBFFC3C3C3FF89A594FF5B95
      70FF4D8D64FF42835AFE1539227C000000000000000000000000A0A0A0EAF3F3
      F3FFE7E7E7FFDDDDDDFFD9D9D9FFE0E0E0FFDBDBDBFFC3C3C3FF6E8ABDFF2058
      BEFF0441BBFF124CB3FD001F5778000000001E6197FF6EB4E0FF509ED7FF4E9B
      D6FF4C97D4FF4D94CDFF7190A9FFF1F1F1FFB6B6B6FFB5B5B5FFB5B5B5FFB5B5
      B5FFB4B4B4FFB4B4B4FFE9E9E9FA2D2D2D840000000000000000A3A3A3EAF4F4
      F4FFEAEAEAFFE1E1E1FFDDDDDDFFE3E3E3FFDEDEDEFFC9C9C9FFBDBDBDFFBFBF
      BFFFE2E2E2FF919191EA00000000000000000000000000000000A2A2A2EAF4F4
      F4FFEAEAEAFFE1E1E1FFDDDDDDFFE3E3E3FFDEDEDEFFC9C9C9FFBCBCBCFFBEBE
      BEFFE2E2E2FF909090EA00000000000000000000000000000000A2A2A2EAF4F4
      F4FFEAEAEAFFE1E1E1FFDDDDDDFFE3E3E3FFDEDEDEFFC9C9C9FFBCBCBCFFBEBE
      BEFFE2E2E2FF909090EA00000000000000001E6197FF71B6E1FF55A2D7FF519F
      D7FF4E9CD5FF5098CFFF7393ABFFF8F8F8FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFF2F2F2FFF1F1F1FFEFEFEFFA2F2F2F840000000000000000A5A5A5EAF5F5
      F5FFEEEEEEFFE6E6E6FFE2E2E2FFE6E6E6FFE1E1E1FFCDCDCDFFC2C2C2FFC2C2
      C2FFE3E3E3FF939393EA00000000000000000000000000000000A4A4A4EAF5F5
      F5FFEEEEEEFFE6E6E6FFE2E2E2FFE6E6E6FFE1E1E1FFCDCDCDFFC1C1C1FFC1C1
      C1FFE3E3E3FF929292EA00000000000000000000000000000000A4A4A4EAF5F5
      F5FFEEEEEEFFE6E6E6FFE2E2E2FFE6E6E6FFE1E1E1FFCDCDCDFFC1C1C1FFC1C1
      C1FFE3E3E3FF929292EA00000000000000001E6197FF74B8E2FF5AA6D9FF56A3
      D8FF519FD7FF519DD5FF5F8AA8FF6287A0FF6286A0FF61859FFF67869EFF4866
      80FF3838388A3737378A3535358A252525630000000000000000A6A6A6EAF6F6
      F6FFEBEBEBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC3C3C3FFBCBCBCFFC0C0
      C0FFE5E5E5FF959595EA00000000000000000000000000000000A5A5A5EAF6F6
      F6FFEBEBEBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC2C2C2FFBBBBBBFFBFBF
      BFFFE5E5E5FF949494EA00000000000000000000000000000000A5A5A5EAF6F6
      F6FFEBEBEBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC2C2C2FFBBBBBBFFBFBF
      BFFFE5E5E5FF949494EA00000000000000001E6197FF78BAE3FF5FA9DBFF58A4
      D9FF519FD7FF509ED7FF509ED7FF509ED7FF509ED7FF509ED7FF60A2D8FF1E61
      97FF000000000000000000000000000000000000000000000000A8A8A8EAF7F7
      F7FFE7E7E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC3C3
      C3FFE6E6E6FF979797EA00000000000000000000000000000000A7A7A7EAF7F7
      F7FFE7E7E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC2C2
      C2FFE6E6E6FF959595EA00000000000000000000000000000000A7A7A7EAF7F7
      F7FFE7E7E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC2C2
      C2FFE6E6E6FF959595EA00000000000000001E6197FF7ABCE4FF63ADDDFF60AA
      DCFF5CA7DAFF5AA6D9FF5AA6D9FF5AA6D9FF5AA6D9FF509ED7FF60A2D8FF1E61
      97FF000000000000000000000000000000000000000000000000AAAAAAEAF8F8
      F8FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFEAEAEAFF999999EA00000000000000000000000000000000A9A9A9EAF8F8
      F8FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFEAEAEAFF989898EA00000000000000000000000000000000A9A9A9EAF8F8
      F8FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFEAEAEAFF989898EA00000000000000001E6197FF7DBEE4FF67B1DEFF489A
      DAFF4296DCFF4195DCFF4095DCFF4094DCFF3F94DBFF4F9DD6FF6AB1DEFF1E61
      97FF0000000000000000000000000000000000000000000000008B8B8BBFE1E1
      E1FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFCFCFCFFF6A6A6AA1000000000000000000000000000000008B8B8BBFE1E1
      E1FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFCFCFCFFF6A6A6AA1000000000000000000000000000000008B8B8BBFE1E1
      E1FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFCFCFCFFF6A6A6AA100000000000000001B5585E06FB1DAFE7CBEE4FF4C9C
      DFFFB4EEFDFF73D4F0FF73D4F0FFB4EEFDFF499ADEFF6CB3E0FF69AED9F91D5D
      90F30000000000000000000000000000000000000000000000001F1F1F2A9999
      99D1D0D0D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC2C2
      C2FF848484C31212121B000000000000000000000000000000001F1F1F2A9797
      97D1D0D0D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC1C1
      C1FF848484C31212121B000000000000000000000000000000001F1F1F2A9797
      97D1D0D0D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC1C1
      C1FF848484C31212121B000000000000000006121E32164368B21E6197FF3573
      A3FFB5EFFEFF7EDBF3FF7EDBF3FFB5EFFEFF2C6CA0FF1E6197FF133E61A50A1F
      3153000000000000000000000000000000000000000000000000000000000505
      05073E3E3E557F7F7FAE9A9A9AD6B4B4B4FBB1B1B1F9909090CD777777A93131
      3146020202030000000000000000000000000000000000000000000000000505
      05073E3E3E557E7E7EAE9A9A9AD6B3B3B3FBB1B1B1F9909090CD767676A93030
      3046020202030000000000000000000000000000000000000000000000000505
      05073E3E3E557E7E7EAE9A9A9AD6B3B3B3FBB1B1B1F9909090CD767676A93030
      3046020202030000000000000000000000000000000000000000040F192A1E61
      97FF1E6197FF1E6197FF1E6197FF1E6197FF1D5B8EF000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000020003173A1A78000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000020202036868
      68CC7C7C7CFF777777FF727272FF6E6E6EFF535353CA00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000001050A1017679F323BC2F7171F7EB90000
      0F1C000000000000000000000000000000000000000000000000000000000000
      00000000000000000000945531C1B1683DE6C77343FEBD6E40F3BE6E40F3BD6E
      40F3BD6E40F3BF6E41F4B2683CE8BF764CF10000000000000000000000000000
      00000000000027582B9C2F6F36D2000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C0C0C159191
      91FFD5D3D3FFE2E0DFFFDFDCDBFFE1DFDFFF696969F400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000001
      111F010115260000020300000000070B44703E49D9FF252CA4DD3B45D1FF1117
      6CA4000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B97042EDFCF3ECFFFAF1E8FFFAF0E7FFFBF1E9FFFBF2
      EAFFFBF2EAFFFBF2EBFFFDF4EEFFC57B50F90000000000000000000000000000
      00002A5B309064AF6CFF5FA966FF3B8A42FF35823CFF307935FF276A2CEA1B4D
      1EB70E2B1170020A031D00000000000000000000000000000000000000003E3E
      3E6F666666BDC3C1C0FFD4CFCEFF757575F64848489C626262D2727272FF6E6E
      6EFF696969FF656565FF616161FF4F4F4FD7000000000000000005083A632F3A
      C1F8353FCFFF141C75B000010D18171E7BAF333ECAFD00010C180F166295313B
      BEF800010B140000000000000000000000000000000000000000000000000000
      00000000000000000000CF8151FFEFF1E7FFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFEFF2E8FFCE8054FF0000000000000000000000002E5D
      338471BC7AFF95D19EFF93CF9BFF8ECD95FF89CA90FF84C78AFF78BD80FF63AC
      6AFF49914FFF18461CB006150839000000005B5B5B8D818181CC7C7C7CCC7878
      78CC7D7D7DDE9D9D9DFF999999FF7D7D7DF8838383FF868685FF868584FFA19F
      9EFFD3CECDFFD3CECDFFE8E5E5FF626262FF00000000000000002731A3DE2E37
      B5E51A2386C23D48DDFF161F82C21B248DCB2F37BBF200000203070C4877353F
      C8FF010212200000000000000000000000000000000000000000000000000000
      00000000000000000000C8804FFBFBF5EEFFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFFBF6EFFFCB8153FE0000000000000000000000003265
      398A77C382FF9DD7A6FF9AD4A3FF96D29EFF91CF99FF8CCC94FF87CA8FFF78C1
      81FF7CC384FF5BA361FF1A481CB0030A031DADADADFFE4E2E2FFD7D5D5FFD5D3
      D2FFD1CECDFFCAC3C2FFC8C2C1FFCDC9C8FFCCCACAFFCCCAC9FFD8D6D6FF7271
      71FFB8B0ADFFB6AEADFFD3CECDFF666666FF00000000000000002D36B3EC171E
      76AF000000000E166499404ED2FFCAA173FE3F4DD6FF0C13639D262F9FD4272F
      A5DF0001050A0000000000000000000000008F5634B9A2603BCEAD653ADDA45F
      39D4A65F39D4A45F39D4CA8350FFFFF7F1FFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFFFF7F1FFCA8353FE0000000000000000000000000000
      0000376D3E9379C784FF75C180FF52AA5CFF4CA255FF479A4FFF61AB69FF82C2
      8AFF86C98EFF81C588FF4E9654FF112E1270B3B3B3FFE0DDDDFFA6704BFFA670
      4BFFA6704BFFA6704BFFA6704BFFA6704BFFA6704BFFA6704BFFDAD5D4FF7676
      76FFB9B1B0FFB8B0AEFFD4CFCEFF6B6B6BFF00000000000000001117669C3944
      CCFF0204213A01052F533946D4FFDBBC9BFFEECCA5FF3E4ADEFF3841D1FF0609
      3E6900000000000000000000000000000000A4653FD3DBD4CCDED9D1CADED9D0
      C9DEDBD1CADEDBD2CBDEE4B990FFFFF7F0FFFFE7D5FFFDE7D6FFFDE6D4FFFCE4
      D0FFFBE3CBFFFADCC1FFFEF3E8FFCB8454FE0000000000000000000000000000
      0000000000003C77439F4D9955D200000000000000000000000028572C8D3B82
      42E185C58DFF87C98EFF6DB274FF205323B7B9B9B9FFDEDBDBFFB47E58FFCE97
      6EFFD8AD90FFD9AE90FFD9AE90FFDAAE90FFD69F75FFA6704BFFD7D3D1FF7C7C
      7CFFBAB3B2FFBAB2B0FFD4D0CFFF717171FF00000000000000000001111F242B
      9BD73A44CFFF2F39BFF23D4AD7FFD8BB99FFF6EAE1FF8B6D42BF2215053B0100
      000100000000000000000000000000000000B47047DED0D1C9DEDECABDDEDECB
      BEDEDECABDDEDEC9BBDEE4BA90FFFFF7F2FFFEE7D5FFFEE7D5FFFDE5D1FFFAE0
      CAFFF9DEC3FFF7D9BBFFFDF2E7FFCB8555FE19391C611736196114331761122F
      1561112C126100020003305D357B000000000000000000000000000000002652
      2A83408D47ED3D8B45F638833EF82F7435E8BFBFBFFFDFDCDCFFB37D57FFCB94
      6CFFCD966DFFCF986FFFD19A70FFD29B72FFD49D73FFA6704BFFD7D4D3FF8383
      83FF39A040FF359335FFD6D1D0FF767676FF0000000000000000000000000001
      111F10166499202894CC090E5489957952CCF0E0D0FF8D6F48C50603000B0000
      000000000000000000000000000000000000AF7046DBDBD5CFDEDECABDDEDECB
      BEDEDECABDDEDEC9BBDEE4BA91FFFEF7F1FFFCE5D2FFFCE4D1FFFBE2CCFFF9DD
      C3FFF6D7BAFFF3D1AEFFFAEFE4FFCB8556FE44914CE843944BF83D8B45F6357C
      3CED1B401D83000000000000000000000000000000000D2C0E7B000000031F41
      22611C3E20611A3B1D6119381C6116341961C4C4C4FFE1DEDCFFB27B56FFC790
      69FFC9926BFFCB946CFFCD966EFFCF986FFFD19A71FFA6704BFFD9D5D4FF8A8A
      8AFF9FC8A3FF4EA854FFD7D3D1FF7C7C7CFF0000000000000000000000000000
      000000000000000001020000000065503290F6EADDFFE1CDB3FF846944BF0000
      000000000000000000000000000000000000AC7146DBDED7D1DEDECABDDEDECB
      BEDEDECABDDEDEC9BBDEE4BA91FFFEF6F0FFFCE2CDFFFCE3CDFFFADFC8FFF7D9
      BBFFF5E9DDFFFAF3EBFFFBF8F3FFC98151FE3C7A42B77EC288FF8CCC94FF82C3
      89FF347A3AE11D48218D0000000000000000000000001B521FD2123B159F0000
      000000000000000000000000000000000000CACACAFFE1DFDEFFB07A55FFC38D
      66FFC58F68FFC89169FFCA936BFFCC956DFFCE976EFFA6704BFFDBD6D6FF9090
      90FFC0BAB8FFBFB8B7FFD7D3D3FF838383FF0000000000000000000000000000
      0000000000000000000000000000604B2E89F1E2D4FFC5A987F4F5EBE0FF664D
      2C9D00000000000000000000000000000000AE7146DBDED7D0DEDEC9B9DEDCC9
      BADEDCC8B9DEDBC5B5DEE4BA92FFFEF5EDFFFCDEC4FFFBE0C7FFF9DCC1FFF5D3
      B3FFFEF9F3FFFAE2C3FFECC092FF6F462893274F2B7073BE7CFF97D2A0FF93CF
      9BFF85C78CFF5CA663FF37853EFF327C38FF2C7431FF478F4DFF438A48FF1137
      149300000000000000000000000000000000CECECEFFE2DFDFFFAF7954FFB079
      54FFB07A55FFB17B56FFB27C56FFB37D57FFB47E58FFA6704BFFDCD8D7FF9797
      97FF8C8C8CFF898989FFD9D5D4FF8A8A8AFF0000000000000000000000000000
      00000000000000000000000000005B462984EEDFCEFF3B270B65A58968DAE7D6
      C2FD513E247B000000000000000000000000AE7247DBDED7D2DEDDC9B9DEDDC9
      B9DEDCC7B6DED9C3B0DEE5BD95FFFFFFFEFFFDF3E9FFFDF3EAFFFCF2E8FFFAEF
      E3FFFAF2E7FFEABA87FF915C3AB30804030C0A150C1D3E7D45B084C98DFF9AD4
      A3FF8ECE97FF91CF99FF8CCC94FF87CA8FFF82C58AFF7CC384FF77C07DFF458C
      4AFF1135128A000000000000000000000000D3D3D3FFF1EFEFFFE2DFDFFFE2DF
      DFFFE1DFDEFFE1DEDDFFE0DDDCFFDFDCDBFFDEDBDBFFDEDBD9FFEDECEBFF9D9D
      9DFFC4BEBDFFC2BCBAFFDAD6D5FF909090FF0000000000000000000000000000
      00000000000000000000000000005541257CE7D5C0FF1910052B0F09021BB196
      76E7BCA589E1453825620000000000000000AE7247DBDDD7D1DEDBC7B6DEDBC5
      B6DEDBC4B1DED8C0A9DEEAC29CFFE6BE95FFE4BA91FFE4BA91FFC89966F5C897
      67F6AE7F50DA5F3A1F7E060302090000000000000000152A17393F7F46B077C2
      82FF88CA91FF93D09BFF94D19DFF8FCF98FF8BCB93FF86C98EFF7EC386FF4C94
      52FF14371684000000000000000000000000D1D1D1F9D3D3D3FFD0D0D0FFCCCC
      CCFFC8C8C8FFC2C2C2FFBEBEBEFFB9B9B9FFB4B4B4FFAFAFAFFFA9A9A9FFA4A4
      A4FF939292FF919090FFDBD7D6FF979797FF0000000000000000000000000000
      00000000000000000000000000004E3C2175DEC9AEFF120B031F00000000160D
      0226B39670EB8F724DCB0000000000000000AE7247DBDDD6D0DEDBC4B1DEDBC5
      B1DED9C2AEDED7BDA3DED5CAC0DED9D4CCDEDBD7D4DEB18055DC000000000000
      00000000000000000000000000000000000000000000000000000A150C1D2852
      2D70417F47B74D9E55EA4EA558FF499D51FF43954BFF5EA766FF59A160FF1C46
      2090000000000000000000000000000000000000000000000000000000000000
      00000000000039393948B6B6B6E4DFDFDFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3
      C2FFC8C2C0FFC7C0C0FFDCD9D8FF9D9D9DFF0000000000000000000000000000
      00000000000000000000000000004A381D6FCFB699F709060210000000000000
      00001A11042C86643ACB0000000000000000AE7248DADDD5CEDEDBC1AADEDBC3
      ADDED8C0A8DED5B79CDEDDD8D4DED9C4A9DECCA77FDE613E2480000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000397D3FD227582B9C0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000C0C0C0F888888ABDCDCDCFFE6E3E3FFE1DEDCFFDFDC
      DCFFDFDCDBFFDEDBDBFFEEECECFFA4A4A4FF0000000000000000000000000000
      000000000000000000000000000045321B69B39A79E005020009000000000000
      000000000000030101060000000000000000A46C43D0DBDBDBDBDCD4CADEDCD4
      CBDEDBD2CADED9D0C5DED9D2C9DECBA276DE7F51329C0703010A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000244C2778000200030000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007F7F7F9FC6C6C6FBC3C3C3FFBFBF
      BFFFBABABAFFB5B5B5FFAFAFAFFF6E6E6EA70000000000000000000000000000
      00000000000000000000000000001D1306316A4E2AA103010005000000000000
      000000000000000000000000000000000000854F2FAAA46B43CEB1764ADEAE74
      48DBAF7448DBAE7349DBA46A3FD054331B6E0603020800000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000000300000100010000000000001800000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object PopupQueryLoad: TPopupMenu
    OnPopup = PopupQueryLoadPopup
    Left = 272
    Top = 104
  end
  object popupDB: TPopupMenu
    Images = ImageListMain
    OnPopup = popupDBPopup
    Left = 272
    Top = 152
    object menuEditObject: TMenuItem
      Caption = 'Edit'
      Hint = 'Edit selected object'
      ImageIndex = 129
      ShortCut = 32781
      OnClick = menuEditObjectClick
    end
    object menuDeleteObject: TMenuItem
      Action = actDropObjects
    end
    object menuEmptyTables: TMenuItem
      Action = actEmptyTables
    end
    object Runroutines1: TMenuItem
      Action = actRunRoutines
    end
    object menuCreateObject: TMenuItem
      Caption = 'Create new'
      ImageIndex = 130
      object menuCreateDB: TMenuItem
        Action = actCreateDatabase
      end
      object menuCreateTable: TMenuItem
        Action = actCreateTable
      end
      object menuCreateTableCopy: TMenuItem
        Action = actCopyTable
      end
      object menuCreateView: TMenuItem
        Action = actCreateView
      end
      object menuCreateRoutine: TMenuItem
        Action = actCreateRoutine
      end
      object menuCreateTrigger: TMenuItem
        Action = actCreateTrigger
      end
      object Event1: TMenuItem
        Action = actCreateEvent
      end
    end
    object menuClearDataTabFilter: TMenuItem
      Caption = 'Clear data tab filter'
      OnClick = menuClearDataTabFilterClick
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object menuExporttables: TMenuItem
      Action = actExportTables
    end
    object menuMaintenance2: TMenuItem
      Action = actMaintenance
    end
    object Findtextonserver1: TMenuItem
      Action = actFindTextOnServer
    end
    object menuBulkTableEdit: TMenuItem
      Action = actBulkTableEdit
    end
    object N5a: TMenuItem
      Caption = '-'
    end
    object menuTreeExpandAll: TMenuItem
      Caption = 'Expand all'
      ImageIndex = 87
      OnClick = menuTreeExpandAllClick
    end
    object menuTreeCollapseAll: TMenuItem
      Caption = 'Collapse all'
      ImageIndex = 88
      OnClick = menuTreeCollapseAllClick
    end
    object menuTreeOptions: TMenuItem
      Caption = 'Tree style options'
      object menuGroupObjects: TMenuItem
        Action = actGroupObjects
        AutoCheck = True
      end
      object menuShowSizeColumn: TMenuItem
        Caption = 'Display size of objects'
        OnClick = menuShowSizeColumnClick
      end
      object menuAutoExpand: TMenuItem
        Caption = 'Auto expand on click'
        OnClick = menuAutoExpandClick
      end
      object menuDoubleClickInsertsNodeText: TMenuItem
        Caption = 'Doubleclick inserts node text'
        OnClick = menuDoubleClickInsertsNodeTextClick
      end
      object menuSelectBGColor: TMenuItem
        Action = actSelectTreeBackground
      end
      object actFavoriteObjectsOnly1: TMenuItem
        Action = actFavoriteObjectsOnly
        AutoCheck = True
      end
    end
    object menuPrint: TMenuItem
      Action = actPrintList
    end
    object menuRefreshDB: TMenuItem
      Action = actRefresh
    end
    object Disconnect1: TMenuItem
      Action = actDisconnect
    end
  end
  object popupHost: TPopupMenu
    Images = ImageListMain
    OnPopup = popupHostPopup
    Left = 201
    Top = 104
    object Copy2: TMenuItem
      Action = actCopy
    end
    object N26: TMenuItem
      Caption = '-'
    end
    object menuFetchDBitems: TMenuItem
      Caption = 'Fetch database items'
      Enabled = False
      OnClick = menuFetchDBitemsClick
    end
    object Kill1: TMenuItem
      Caption = 'Kill Process(es)...'
      Enabled = False
      ImageIndex = 26
      ShortCut = 46
      OnClick = KillProcess
    end
    object menuEditVariable: TMenuItem
      Caption = 'Edit ...'
      ImageIndex = 33
      ShortCut = 13
      OnClick = menuEditVariableClick
    end
    object menuExplainProcess: TMenuItem
      Caption = 'EXPLAIN query'
      Enabled = False
      Hint = 'Analyze selected process SQL'
      ImageIndex = 39
      OnClick = lblExplainProcessClick
    end
    object menuExplainAnalyzer: TMenuItem
      Caption = 'EXPLAIN analyzer on MariaDB.org'
      Enabled = False
      Hint = 'Pass EXPLAIN output to MariaDB'#39's analyzer webpage'
      ImageIndex = 39
      OnClick = lblExplainProcessAnalyzerClick
    end
    object N1a: TMenuItem
      Caption = '-'
    end
    object PrintList2: TMenuItem
      Action = actPrintList
    end
    object Refresh1: TMenuItem
      Action = actRefresh
    end
  end
  object SynSQLSyn1: TSynSQLSyn
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
    Left = 591
    Top = 152
  end
  object TimerHostUptime: TTimer
    OnTimer = TimerHostUptimeTimer
    Left = 687
    Top = 101
  end
  object popupDataGrid: TPopupMenu
    AutoHotkeys = maManual
    Images = ImageListMain
    OnPopup = popupDataGridPopup
    Left = 200
    Top = 248
    object Copy3: TMenuItem
      Action = actCopy
    end
    object Copyselectedrows1: TMenuItem
      Action = actCopyRows
    end
    object Paste2: TMenuItem
      Action = actPaste
    end
    object DataInsertValue: TMenuItem
      Caption = 'Insert value'
      ImageIndex = 80
      OnClick = DataInsertValueClick
      object setNULL1: TMenuItem
        Action = actDataSetNull
      end
      object InsertSQLfunction1: TMenuItem
        Action = actGridEditFunction
      end
      object DataDefaultValue: TMenuItem
        Caption = 'default'
        ImageIndex = 28
        OnClick = InsertValue
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object DataDateTime: TMenuItem
        Caption = 'datetime'
        Hint = 'Insert datetime-value'
        ImageIndex = 80
        OnClick = InsertValue
      end
      object DataDate: TMenuItem
        Caption = 'date'
        Hint = 'Insert date-value'
        ImageIndex = 80
        OnClick = InsertValue
      end
      object DataTime: TMenuItem
        Caption = 'time'
        Hint = 'Insert time-value'
        ImageIndex = 80
        OnClick = InsertValue
      end
      object DataYear: TMenuItem
        Caption = 'year'
        Hint = 'Insert year-value'
        ImageIndex = 80
        OnClick = InsertValue
      end
      object DataUNIXtimestamp: TMenuItem
        Caption = 'unix timestamp'
        Hint = 'Insert UNIX timestamp'
        ImageIndex = 80
        OnClick = InsertValue
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object DataGUID: TMenuItem
        Caption = 'GUID'
        ImageIndex = 112
        OnClick = InsertValue
      end
      object DataGUIDwobraces: TMenuItem
        Caption = 'GUID without braces'
        ImageIndex = 112
        OnClick = InsertValue
      end
    end
    object InsertfilesintoBLOBfields3: TMenuItem
      Action = actInsertFiles
    end
    object SaveBLOBtofile1: TMenuItem
      Action = actDataSaveBlobToFile
    end
    object Gridviewoptions1: TMenuItem
      Caption = 'Grid view options'
      object hisisaUNIXtimestampcolumn1: TMenuItem
        Action = actUnixTimestampColumn
        AutoCheck = True
      end
      object ViewasHTML1: TMenuItem
        Action = actDataPreview
      end
      object ViewbinarydataastextinsteadofHEX2: TMenuItem
        Action = actBlobAsText
        AutoCheck = True
      end
      object Datapreferences1: TMenuItem
        Action = actPreferencesData
      end
    end
    object N4a: TMenuItem
      Caption = '-'
    end
    object Insert1: TMenuItem
      Action = actDataInsert
    end
    object Duplicaterow1: TMenuItem
      Action = actDataDuplicateRow
    end
    object DataPost1: TMenuItem
      Action = actDataPostChanges
    end
    object Cancelediting1: TMenuItem
      Action = actDataCancelChanges
    end
    object Delete1: TMenuItem
      Action = actDataDelete
    end
    object N6a: TMenuItem
      Caption = '-'
    end
    object Resetsorting1: TMenuItem
      Action = actDataResetSorting
    end
    object menuQuickFilter: TMenuItem
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
      object QFvalues: TMenuItem
        Caption = 'More values ...'
        ImageIndex = 61
        OnClick = QFvaluesClick
        object TMenuItem
        end
      end
      object N11a: TMenuItem
        Caption = '-'
      end
      object QF8: TMenuItem
        Tag = 1
        Caption = 'Column = ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF9: TMenuItem
        Tag = 1
        Caption = 'Column != ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF10: TMenuItem
        Tag = 1
        Caption = 'Column > ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF11: TMenuItem
        Tag = 1
        Caption = 'Column < ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF12: TMenuItem
        Tag = 1
        Caption = 'Column like ...'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF13: TMenuItem
        Caption = 'Column IS NULL'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object QF14: TMenuItem
        Caption = 'Column IS NOT NULL'
        ImageIndex = 58
        OnClick = QuickFilterClick
      end
      object N7a: TMenuItem
        AutoHotkeys = maManual
        Caption = '-'
      end
      object QF15: TMenuItem
        Caption = 'Column = CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF16: TMenuItem
        Caption = 'Column != CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF17: TMenuItem
        Caption = 'Column > CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF18: TMenuItem
        Caption = 'Column < CLIPBOARD'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF19: TMenuItem
        Caption = 'Column LIKE %CLIPBOARD%'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object QF20: TMenuItem
        Caption = 'Column IN ()'
        ImageIndex = 4
        OnClick = QuickFilterClick
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object DropFilter1: TMenuItem
        Action = actRemoveFilter
      end
    end
    object N9a: TMenuItem
      Caption = '-'
    end
    object Exportdata2: TMenuItem
      Action = actExportData
    end
    object menuSQLhelpData: TMenuItem
      Action = actSQLhelp
    end
    object Refresh3: TMenuItem
      Action = actRefresh
    end
  end
  object TimerConnected: TTimer
    OnTimer = TimerConnectedTimer
    Left = 687
    Top = 245
  end
  object popupSqlLog: TPopupMenu
    Images = ImageListMain
    OnPopup = popupSqlLogPopup
    Left = 272
    Top = 200
    object Copy1: TMenuItem
      Action = actCopy
    end
    object Copylinetonewquerytab1: TMenuItem
      Caption = 'Copy line to new query tab'
      ImageIndex = 132
      OnClick = Copylinetonewquerytab1Click
    end
    object Clear2: TMenuItem
      Action = actClearQueryLog
    end
    object menuLogHorizontalScrollbar: TMenuItem
      Action = actLogHorizontalScrollbar
      AutoCheck = True
    end
    object Saveastextfile1: TMenuItem
      Action = actSaveSynMemoToTextfile
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
    object N15: TMenuItem
      Caption = '-'
    end
    object Loggingpreferences1: TMenuItem
      Action = actPreferencesLogging
    end
  end
  object TimerRefresh: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = actRefreshExecute
    Left = 688
    Top = 197
  end
  object popupListHeader: TVTHeaderPopupMenu
    Images = ImageListMain
    Left = 424
    Top = 208
  end
  object SynCompletionProposal: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    NbLinesInWindow = 12
    Width = 350
    EndOfTokenChr = ',()[]. ='#9
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
        ColumnWidth = 100
      end
      item
        ColumnWidth = 100
      end>
    ItemHeight = 18
    Images = ImageListMain
    Margin = 1
    OnExecute = SynCompletionProposalExecute
    ShortCut = 16416
    Editor = SynMemoQuery
    TimerInterval = 500
    OnAfterCodeCompletion = SynCompletionProposalAfterCodeCompletion
    OnCodeCompletion = SynCompletionProposalCodeCompletion
    Left = 592
    Top = 208
  end
  object popupQuery: TPopupMenu
    Images = ImageListMain
    OnPopup = popupQueryPopup
    Left = 344
    Top = 104
    object MenuRun: TMenuItem
      Action = actExecuteQuery
    end
    object MenuRunSelection: TMenuItem
      Action = actExecuteSelection
    end
    object MenuRunLine: TMenuItem
      Action = actExecuteCurrentQuery
    end
    object menuQueryExplain: TMenuItem
      Caption = 'Explain'
      OnClick = menuQueryExplainClick
      object Explaincurrentquery1: TMenuItem
        Action = actExplainCurrentQuery
      end
      object Explainanalyzerforcurrentquery1: TMenuItem
        Action = actExplainAnalyzeCurrentQuery
      end
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object menuQueryCut: TMenuItem
      Action = actCut
    end
    object menucopy: TMenuItem
      Action = actCopy
    end
    object menupaste: TMenuItem
      Action = actPaste
    end
    object menuQuerySelectall: TMenuItem
      Action = actSelectAll
    end
    object menuclear: TMenuItem
      Action = actClearQueryEditor
    end
    object ReformatSQL1: TMenuItem
      Action = actReformatSQL
    end
    object Uncomment1: TMenuItem
      Action = actToggleComment
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object MenuFind: TMenuItem
      Action = actQueryFind
    end
    object MenuReplace: TMenuItem
      Action = actQueryReplace
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object menuload: TMenuItem
      Action = actLoadSQL
    end
    object menuSaveSQL: TMenuItem
      Action = actSaveSQL
    end
    object menusave: TMenuItem
      Action = actSaveSQLAs
    end
    object menuSaveSelectionToFile: TMenuItem
      Action = actSaveSQLselection
    end
    object menuSaveAsSnippet: TMenuItem
      Action = actSaveSQLSnippet
    end
    object menuSaveSelectionAsSnippet: TMenuItem
      Action = actSaveSQLSelectionSnippet
    end
    object N23: TMenuItem
      Caption = '-'
    end
    object menuSQLhelp2: TMenuItem
      Action = actSQLhelp
    end
    object menuQueryInsertFunction: TMenuItem
      Caption = 'Insert function'
      ImageIndex = 13
    end
  end
  object popupQueryHelpers: TPopupMenu
    Images = ImageListMain
    Left = 344
    Top = 152
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
      Action = actSQLhelp
    end
    object menuClearQueryHistory: TMenuItem
      Caption = 'Clear query history ...'
      Enabled = False
      OnClick = menuClearQueryHistoryClick
    end
    object menuQueryHelpersGenerateSelect: TMenuItem
      Caption = 'Generate SELECT ...'
      Enabled = False
      ImageIndex = 114
      OnClick = menuQueryHelpersGenerateStatementClick
    end
    object menuQueryHelpersGenerateInsert: TMenuItem
      Caption = 'Generate INSERT ...'
      Enabled = False
      ImageIndex = 114
      OnClick = menuQueryHelpersGenerateStatementClick
    end
    object menuQueryHelpersGenerateUpdate: TMenuItem
      Caption = 'Generate UPDATE ...'
      Enabled = False
      ImageIndex = 114
      OnClick = menuQueryHelpersGenerateStatementClick
    end
    object menuQueryHelpersGenerateDelete: TMenuItem
      Caption = 'Generate DELETE ...'
      Enabled = False
      ImageIndex = 114
      OnClick = menuQueryHelpersGenerateStatementClick
    end
  end
  object popupFilter: TPopupMenu
    Images = ImageListMain
    Left = 344
    Top = 248
    object menuFilterCopy: TMenuItem
      Action = actCopy
    end
    object menuFilterPaste: TMenuItem
      Action = actPaste
    end
    object menuFilterClear: TMenuItem
      Action = actClearFilterEditor
    end
    object N8a: TMenuItem
      Caption = '-'
    end
    object menuFilterApply: TMenuItem
      Action = actApplyFilter
    end
    object menuRecentFilters: TMenuItem
      Caption = 'Recent filters'
      Enabled = False
      ImageIndex = 53
    end
    object menuFilterInsertFunction: TMenuItem
      Caption = 'Insert function'
      ImageIndex = 13
    end
  end
  object popupRefresh: TPopupMenu
    Images = ImageListMain
    Left = 200
    Top = 200
    object Fullstatusrefresh1: TMenuItem
      Action = actFullRefresh
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object menuAutoRefresh: TMenuItem
      Caption = 'Auto refresh'
      ShortCut = 16500
      OnClick = AutoRefreshToggle
    end
    object menuAutoRefreshSetInterval: TMenuItem
      Caption = 'Set interval ...'
      OnClick = AutoRefreshSetInterval
    end
  end
  object popupMainTabs: TPopupMenu
    AutoPopup = False
    Images = ImageListMain
    OnPopup = popupMainTabsPopup
    Left = 200
    Top = 152
    object menuNewQueryTab: TMenuItem
      Action = actNewQueryTab
    end
    object menuCloseTab: TMenuItem
      Caption = 'Close query tab'
      ImageIndex = 133
      OnClick = menuCloseQueryTab
    end
  end
  object TimerFilterVT: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerFilterVTTimer
    Left = 688
    Top = 149
  end
  object BalloonHint1: TBalloonHint
    Delay = 100
    HideAfter = 10000
    Left = 424
    Top = 264
  end
  object popupExecuteQuery: TPopupMenu
    Images = ImageListMain
    Left = 272
    Top = 248
    object Run1: TMenuItem
      Action = actExecuteQuery
    end
    object RunSelection1: TMenuItem
      Action = actExecuteSelection
    end
    object Runcurrentquery1: TMenuItem
      Action = actExecuteCurrentQuery
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Sendqueriesonebyone1: TMenuItem
      Action = actSingleQueries
      AutoCheck = True
    end
    object Runbatchinonego1: TMenuItem
      Action = actBatchInOneGo
      AutoCheck = True
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnDeactivate = ApplicationEvents1Deactivate
    OnIdle = ApplicationEvents1Idle
    Left = 504
    Top = 152
  end
end
