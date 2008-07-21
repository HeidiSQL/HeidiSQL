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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splitterTopBottom: TSplitter
    Left = 0
    Top = 252
    Width = 677
    Height = 4
    Cursor = crSizeNS
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
    OnMoved = splitterTopBottomMoved
  end
  object panelTop: TPanel
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
    object DBtree: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 169
      Height = 252
      Align = alLeft
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
      Images = MainForm.PngImageListMain
      IncrementalSearch = isInitializedOnly
      Indent = 16
      Margin = 2
      ParentShowHint = False
      PopupMenu = popupTreeView
      ShowHint = True
      TabOrder = 0
      TextMargin = 2
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toRightClickSelect]
      OnChange = DBtreeChange
      OnDblClick = DBtreeDblClick
      OnGetText = DBtreeGetText
      OnPaintText = DBtreePaintText
      OnGetImageIndex = DBtreeGetImageIndex
      OnGetHint = vstGetHint
      OnGetNodeDataSize = DBtreeGetNodeDataSize
      OnInitChildren = DBtreeInitChildren
      OnInitNode = DBtreeInitNode
      Columns = <
        item
          Position = 0
          Width = 110
          WideText = 'Name'
        end
        item
          Alignment = taRightJustify
          MinWidth = 0
          Position = 1
          Width = 55
          WideText = 'Size'
        end>
    end
    object PageControlMain: TPageControl
      Left = 173
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
      TabOrder = 1
      OnChange = pcChange
      object tabHost: TTabSheet
        Caption = 'Host'
        ImageIndex = 1
        object PageControlHost: TPageControl
          Left = 0
          Top = 0
          Width = 496
          Height = 220
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
              Height = 167
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
              Height = 167
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
                  Width = 328
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
              Top = 119
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
              Height = 94
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
              Top = 123
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
              Height = 192
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
                  Width = 68
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
          Width = 496
          Height = 220
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
          TabOrder = 0
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
      end
      object tabTable: TTabSheet
        Caption = 'Table'
        ImageIndex = 14
        object ListColumns: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 496
          Height = 220
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
          TabOrder = 0
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
          OnBeforePaint = vstBeforePaint
          OnChange = ListColumnsChange
          OnCompareNodes = vstCompareNodes
          OnDblClick = ListColumnsDblClick
          OnFreeNode = vstFreeNode
          OnGetText = vstGetText
          OnGetImageIndex = vstGetImageIndex
          OnGetHint = vstGetHint
          OnGetNodeDataSize = vstGetNodeDataSize
          OnHeaderClick = vstHeaderClick
          OnInitNode = vstInitNode
          OnKeyUp = controlsKeyUp
          OnNewText = ListColumnsNewText
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
          Height = 25
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 1
          TabOrder = 0
          object lblDataTop: TLabel
            Left = 1
            Top = 1
            Width = 287
            Height = 23
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
          object tlbDataButtons: TToolBar
            Left = 294
            Top = 1
            Width = 201
            Height = 23
            Align = alRight
            AutoSize = True
            ButtonWidth = 67
            Caption = 'tlbDataButtons'
            Images = MainForm.PngImageListMain
            List = True
            ShowCaptions = True
            TabOrder = 0
            Wrapable = False
            object tbtnDataSorting: TToolButton
              Left = 0
              Top = 0
              AllowAllUp = True
              Caption = 'Sorting'
              ImageIndex = 107
              Style = tbsTextButton
              OnClick = btnDataClick
            end
            object tbtnDataColumns: TToolButton
              Left = 67
              Top = 0
              AllowAllUp = True
              Caption = 'Columns'
              ImageIndex = 107
              Style = tbsTextButton
              OnClick = btnDataClick
            end
            object tbtnDataFilter: TToolButton
              Left = 134
              Top = 0
              AllowAllUp = True
              Caption = 'Filter'
              ImageIndex = 107
              OnClick = btnDataClick
            end
          end
        end
        object gridData: TTntDBGrid
          Left = 0
          Top = 91
          Width = 496
          Height = 129
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
        object pnlFilter: TPanel
          Left = 0
          Top = 25
          Width = 496
          Height = 66
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          Visible = False
          DesignSize = (
            496
            66)
          object lblTableFilter: TLabel
            Left = 339
            Top = 0
            Width = 114
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Create table wide filter:'
          end
          object btnFilterApply: TButton
            Left = 339
            Top = 41
            Width = 76
            Height = 22
            Action = MainForm.actApplyFilter
            Anchors = [akTop, akRight]
            TabOrder = 2
          end
          object btnFilterClear: TButton
            Left = 419
            Top = 41
            Width = 76
            Height = 22
            Action = MainForm.actClearFilterEditor
            Anchors = [akTop, akRight]
            TabOrder = 3
          end
          object SynMemoFilter: TSynMemo
            Left = 0
            Top = 0
            Width = 335
            Height = 63
            SingleLineMode = False
            Anchors = [akLeft, akTop, akRight, akBottom]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            PopupMenu = popupFilter
            TabOrder = 0
            OnKeyUp = controlsKeyUp
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 10
            Gutter.RightOffset = 0
            Gutter.ShowLineNumbers = True
            Gutter.Visible = False
            Highlighter = SynSQLSyn1
            Options = [eoAutoIndent, eoDragDropEditing, eoDropFiles, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTabIndent]
            RightEdge = 0
            ScrollBars = ssVertical
            WantTabs = True
            WordWrap = True
            OnChange = SynMemoFilterChange
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
          object editFilterSearch: TEdit
            Left = 339
            Top = 15
            Width = 156
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnChange = editFilterSearchChange
            OnEnter = editFilterSearchEnter
            OnExit = editFilterSearchExit
          end
        end
      end
      object tabQuery: TTabSheet
        Caption = 'Query'
        ImageIndex = 57
        object spltQuery: TSplitter
          Left = 0
          Top = 96
          Width = 496
          Height = 4
          Cursor = crSizeNS
          Align = alTop
          ResizeStyle = rsUpdate
        end
        object LabelResultinfo: TLabel
          Left = 0
          Top = 117
          Width = 496
          Height = 13
          Align = alTop
        end
        object pnlQueryMemo: TPanel
          Left = 0
          Top = 0
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
            OnKeyUp = controlsKeyUp
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
            Options = [eoAutoIndent, eoDropFiles, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTabIndent]
            RightEdge = 40
            SearchEngine = SynEditSearch1
            TabWidth = 2
            WantTabs = True
            OnDropFiles = SynMemoQueryDropFiles
            OnStatusChange = SynMemoQueryStatusChange
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
              MultiSelect = True
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
          Top = 130
          Width = 496
          Height = 90
          Align = alClient
          DataSource = DataSource2
          Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect]
          PopupMenu = popupResultGrid
          TabOrder = 1
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
          Top = 100
          Width = 496
          Height = 17
          Align = alTop
          Step = 1
          TabOrder = 2
          Visible = False
        end
      end
    end
  end
  object pageCtlBottom: TPageControl
    Left = 0
    Top = 256
    Width = 677
    Height = 164
    ActivePage = tabLog
    Align = alBottom
    HotTrack = True
    Images = MainForm.PngImageListMain
    TabHeight = 20
    TabOrder = 1
    object tabLog: TTabSheet
      Caption = 'Log'
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
        Options = [eoAutoIndent, eoDragDropEditing, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces]
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
    object tabEditors: TTabSheet
      Caption = 'Editors'
      ImageIndex = 47
      object toolbarEditors: TToolBar
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
          Action = MainForm.actHTMLview
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
      object pageCtlEditors: TPageControl
        Left = 23
        Top = 0
        Width = 646
        Height = 134
        ActivePage = tabEditorText
        Align = alClient
        TabOrder = 1
        OnChange = pageCtlEditorsChange
        object tabEditorText: TTabSheet
          Caption = 'Text'
          object DBMemo1: TTntDBMemo
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
        object tabEditorImage: TTabSheet
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
  end
  object popupTreeView: TPopupMenu
    Images = MainForm.PngImageListMain
    OnPopup = popupTreeViewPopup
    Left = 8
    Top = 16
    object NewDatabase1: TMenuItem
      Action = MainForm.actCreateDatabase
    end
    object menuAlterdatabase: TMenuItem
      Action = MainForm.actEditDatabase
    end
    object PopupmenuDropDatabase: TMenuItem
      Action = MainForm.actDropDatabase
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object PopupMenuCreateTable: TMenuItem
      Action = MainForm.actCreateTable
    end
    object menuTreeAlterTable: TMenuItem
      Action = MainForm.actEditTableProperties
    end
    object menuTreeCreateView: TMenuItem
      Action = MainForm.actCreateView
    end
    object menuTreeEditView: TMenuItem
      Action = MainForm.actEditView
    end
    object PopupMenuDropTable: TMenuItem
      Action = MainForm.actDropTablesAndViews
    end
    object Exporttables2: TMenuItem
      Action = MainForm.actExportTables
    end
    object N5: TMenuItem
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
    object menuRefreshDBTree: TMenuItem
      Caption = 'Refresh'
      ImageIndex = 0
      ShortCut = 116
      OnClick = menuRefreshDBTreeClick
    end
  end
  object popupDbGrid: TPopupMenu
    AutoHotkeys = maManual
    Images = MainForm.PngImageListMain
    Left = 72
    Top = 16
    object menuproperties: TMenuItem
      Action = MainForm.actEditTableFields
      Default = True
    end
    object menuAlterTable: TMenuItem
      Action = MainForm.actEditTableProperties
    end
    object actView1: TMenuItem
      Action = MainForm.actEditView
    end
    object InsertfilesintoBLOBfields1: TMenuItem
      Action = MainForm.actInsertFiles
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object menudroptable: TMenuItem
      Action = MainForm.actDropTablesAndViews
    end
    object menuemptytable: TMenuItem
      Action = MainForm.actEmptyTables
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
      Action = MainForm.actCopyTable
    end
    object menucreatetable: TMenuItem
      Action = MainForm.actCreateTable
    end
    object Createview1: TMenuItem
      Action = MainForm.actCreateView
    end
    object Exporttables1: TMenuItem
      Action = MainForm.actExportTables
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object PrintList3: TMenuItem
      Action = MainForm.actPrintList
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
      Action = MainForm.actPrintList
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
      Action = MainForm.actEditField
      Default = True
    end
    object MenuAddField: TMenuItem
      Action = MainForm.actCreateField
    end
    object DropField1: TMenuItem
      Action = MainForm.actDropFields
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
      Action = MainForm.actEditIndexes
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object InsertfilesintoBLOBfields2: TMenuItem
      Action = MainForm.actInsertFiles
    end
    object PrintList4: TMenuItem
      Action = MainForm.actPrintList
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
      Action = MainForm.actCopy
    end
    object Paste2: TMenuItem
      Action = MainForm.actPaste
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
      Action = MainForm.actDataSetDelete
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
    object N9: TMenuItem
      Caption = '-'
    end
    object Copytableas1: TMenuItem
      Tag = 46
      Caption = 'Copy data'
      object CopyasCSVData1: TMenuItem
        Tag = 48
        Action = MainForm.actCopyAsCSV
      end
      object CopycontentsasHTML1: TMenuItem
        Tag = 49
        Action = MainForm.actCopyAsHTML
      end
      object CopyasXMLdata1: TMenuItem
        Action = MainForm.actCopyAsXML
      end
    end
    object Exportdata2: TMenuItem
      Action = MainForm.actExportData
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
      Action = MainForm.actHTMLview
    end
    object InsertfilesintoBLOBfields3: TMenuItem
      Action = MainForm.actInsertFiles
    end
    object N19: TMenuItem
      Caption = '-'
    end
    object menuSQLhelpData: TMenuItem
      Action = MainForm.actSQLhelp
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
      Action = MainForm.actCopy
    end
    object HTMLview1: TMenuItem
      Action = MainForm.actHTMLview
    end
    object N14: TMenuItem
      Caption = '-'
    end
    object Copyrecords1: TMenuItem
      Tag = 48
      Action = MainForm.actCopyAsCSV
    end
    object CopycontentsasHTML2: TMenuItem
      Tag = 49
      Action = MainForm.actCopyAsHTML
    end
    object CopyasXMLdata2: TMenuItem
      Action = MainForm.actCopyAsXML
    end
    object Exportdata1: TMenuItem
      Action = MainForm.actExportData
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
      Action = MainForm.actCopy
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
  object SynEditSearch1: TSynEditSearch
    Left = 72
    Top = 192
  end
  object popupQuery: TPopupMenu
    Images = MainForm.PngImageListMain
    OnPopup = popupQueryPopup
    Left = 40
    Top = 80
    object MenuRun: TMenuItem
      Action = MainForm.actExecuteQuery
    end
    object MenuRunSelection: TMenuItem
      Action = MainForm.actExecuteSelection
    end
    object MenuRunLine: TMenuItem
      Action = MainForm.actExecuteLine
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object menucopy: TMenuItem
      Action = MainForm.actCopy
    end
    object menupaste: TMenuItem
      Action = MainForm.actPaste
    end
    object menuclear: TMenuItem
      Action = MainForm.actClearQueryEditor
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object MenuFind: TMenuItem
      Action = MainForm.actQueryFind
    end
    object MenuReplace: TMenuItem
      Action = MainForm.actQueryReplace
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object menuload: TMenuItem
      Action = MainForm.actLoadSQL
    end
    object menusave: TMenuItem
      Action = MainForm.actSaveSQL
    end
    object menuSaveSelectionToFile: TMenuItem
      Tag = 1
      Action = MainForm.actSaveSQLselection
    end
    object menuSaveAsSnippet: TMenuItem
      Action = MainForm.actSaveSQLSnippet
    end
    object menuSaveSelectionAsSnippet: TMenuItem
      Tag = 1
      Action = MainForm.actSaveSQLSelectionSnippet
    end
    object N23: TMenuItem
      Caption = '-'
    end
    object menuSQLhelp: TMenuItem
      Action = MainForm.actSQLhelp
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
      Action = MainForm.actSQLhelp
    end
  end
  object popupFilter: TPopupMenu
    Images = MainForm.PngImageListMain
    Left = 8
    Top = 80
    object menuFilterCopy: TMenuItem
      Action = MainForm.actCopy
    end
    object menuFilterPaste: TMenuItem
      Action = MainForm.actPaste
    end
    object menuFilterClear: TMenuItem
      Action = MainForm.actClearFilterEditor
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object menuFilterApply: TMenuItem
      Action = MainForm.actApplyFilter
    end
    object N20: TMenuItem
      Caption = '-'
    end
  end
end
