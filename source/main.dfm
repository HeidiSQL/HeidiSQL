object MainForm: TMainForm
  Left = 241
  Top = 114
  ClientHeight = 487
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splitterTopBottom: TSplitter
    Left = 0
    Top = 384
    Width = 683
    Height = 4
    Cursor = crSizeNS
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  object SynMemoSQLLog: TSynMemo
    Left = 0
    Top = 388
    Width = 683
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
    Gutter.ShowLineNumbers = True
    Highlighter = SynSQLSyn1
    Options = [eoAutoIndent, eoDragDropEditing, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces]
    ReadOnly = True
    RightEdge = -1
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 468
    Width = 683
    Height = 19
    AutoHint = True
    Panels = <
      item
        Width = 170
      end
      item
        Width = 90
      end
      item
        Width = 130
      end
      item
        Width = 90
      end
      item
        Width = 150
      end
      item
        Width = 80
      end
      item
        Width = 250
      end>
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 683
    Height = 52
    Align = alTop
    AutoSize = True
    BevelKind = bkNone
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object ToolBarStandard: TToolBar
      Left = 11
      Top = 2
      Width = 377
      Height = 22
      Align = alNone
      AutoSize = True
      Caption = 'Main'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = PngImageListMain
      TabOrder = 0
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
        Left = 38
        Top = 0
        Action = actExitApplication
      end
      object tlbSep1: TToolButton
        Left = 61
        Top = 0
        Width = 8
        Caption = 'tlbSep1'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ToolButton5: TToolButton
        Left = 69
        Top = 0
        Action = actCopy
        AutoSize = True
      end
      object ToolButton6: TToolButton
        Left = 92
        Top = 0
        Action = actPaste
        AutoSize = True
      end
      object ToolButton14: TToolButton
        Left = 115
        Top = 0
        Hint = 'Undo'
        Action = actUndo
      end
      object ToolButton12: TToolButton
        Left = 138
        Top = 0
        Action = actPrintList
      end
      object tlbSep2: TToolButton
        Left = 161
        Top = 0
        Width = 8
        Caption = 'tlbSep2'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object ButtonCreateDatabase: TToolButton
        Left = 169
        Top = 0
        Action = actCreateDatabase
        AutoSize = True
      end
      object btnEditDatabase: TToolButton
        Left = 192
        Top = 0
        Action = actEditDatabase
      end
      object ButtonDropDatabase: TToolButton
        Left = 215
        Top = 0
        Action = actDropDatabase
        AutoSize = True
      end
      object ToolButton2: TToolButton
        Left = 238
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 32
        Style = tbsSeparator
      end
      object ButtonRefresh: TToolButton
        Left = 246
        Top = 0
        Action = actRefresh
        AutoSize = True
      end
      object tlbSep5: TToolButton
        Left = 269
        Top = 0
        Width = 8
        Caption = 'tlbSep5'
        ImageIndex = 19
        Style = tbsSeparator
      end
      object ButtonUserManager: TToolButton
        Left = 277
        Top = 0
        Action = actUserManager
        AutoSize = True
      end
      object ButtonImportTextfile: TToolButton
        Left = 300
        Top = 0
        Action = actImportCSV
        AutoSize = True
      end
      object ButtonExport: TToolButton
        Left = 323
        Top = 0
        Action = actExportTables
        AutoSize = True
      end
      object tlbSep6: TToolButton
        Left = 346
        Top = 0
        Width = 8
        Caption = 'tlbSep6'
        ImageIndex = 97
        Style = tbsSeparator
      end
      object btnSQLHelp: TToolButton
        Left = 354
        Top = 0
        Action = actSQLhelp
      end
    end
    object ToolBarData: TToolBar
      Left = 11
      Top = 28
      Width = 138
      Height = 22
      Align = alNone
      AutoSize = True
      Caption = 'Data'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = PngImageListMain
      TabOrder = 1
      Wrapable = False
      object ToolButton3: TToolButton
        Left = 0
        Top = 0
        Action = actDataFirst
      end
      object ToolButton4: TToolButton
        Left = 23
        Top = 0
        Action = actDataLast
      end
      object ToolButton7: TToolButton
        Left = 46
        Top = 0
        Action = actDataInsert
      end
      object ToolButton8: TToolButton
        Left = 69
        Top = 0
        Action = actDataDelete
      end
      object ToolButton10: TToolButton
        Left = 92
        Top = 0
        Action = actDataPostChanges
      end
      object ToolButton1: TToolButton
        Left = 115
        Top = 0
        Action = actDataCancelChanges
      end
    end
    object ToolBarDatabase: TToolBar
      Left = 407
      Top = 2
      Width = 115
      Height = 22
      Align = alNone
      AutoSize = True
      Caption = 'ToolBarDatabase'
      Images = PngImageListMain
      TabOrder = 2
      Wrapable = False
      object btnCreateTable: TToolButton
        Left = 0
        Top = 0
        Action = actCreateTable
      end
      object btnCreateView: TToolButton
        Left = 23
        Top = 0
        Action = actCreateView
      end
      object btnDBDropTable: TToolButton
        Left = 46
        Top = 0
        Action = actDropDBobjects
      end
      object btnDBCopyTable: TToolButton
        Left = 69
        Top = 0
        Action = actCopyTable
      end
      object btnDBCreateRoutine: TToolButton
        Left = 92
        Top = 0
        Action = actCreateRoutine
      end
    end
    object ToolBarQuery: TToolBar
      Left = 398
      Top = 28
      Width = 268
      Height = 22
      Align = alNone
      AutoSize = True
      Caption = 'ToolBarQuery'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = PngImageListMain
      TabOrder = 3
      Wrapable = False
      object btnExecuteQuery: TToolButton
        Left = 0
        Top = 0
        Action = actExecuteQuery
      end
      object btnExecuteSelection: TToolButton
        Left = 23
        Top = 0
        Action = actExecuteSelection
      end
      object btnExecuteLine: TToolButton
        Left = 46
        Top = 0
        Action = actExecuteLine
      end
      object btnLoadSQL: TToolButton
        Left = 69
        Top = 0
        Action = actLoadSQL
        DropdownMenu = PopupQueryLoad
        Style = tbsDropDown
      end
      object btnSaveSQL: TToolButton
        Left = 107
        Top = 0
        Action = actSaveSQL
      end
      object btnSaveSQLSnippet: TToolButton
        Left = 130
        Top = 0
        Action = actSaveSQLSnippet
      end
      object btnQueryFind: TToolButton
        Left = 153
        Top = 0
        Action = actQueryFind
      end
      object btnQueryReplace: TToolButton
        Left = 176
        Top = 0
        Action = actQueryReplace
      end
      object btnStopOnErrors: TToolButton
        Left = 199
        Top = 0
        Action = actQueryStopOnErrors
      end
      object btnQueryWordwrap: TToolButton
        Left = 222
        Top = 0
        Action = actQueryWordWrap
      end
      object btnSetDelimiter: TToolButton
        Left = 245
        Top = 0
        Action = actSetDelimiter
      end
    end
  end
  object panelTop: TPanel
    Left = 0
    Top = 52
    Width = 683
    Height = 332
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 169
      Top = 0
      Width = 4
      Height = 332
      Cursor = crSizeWE
      ResizeStyle = rsUpdate
    end
    object DBtree: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 169
      Height = 332
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
      Images = PngImageListMain
      IncrementalSearch = isInitializedOnly
      Indent = 16
      ParentShowHint = False
      PopupMenu = popupTreeView
      ShowHint = True
      TabOrder = 0
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
      Width = 510
      Height = 332
      ActivePage = tabHost
      Align = alClient
      HotTrack = True
      Images = PngImageListMain
      MultiLine = True
      TabHeight = 22
      TabOrder = 1
      OnChange = PageControlMainChange
      object tabHost: TTabSheet
        Caption = 'Host'
        ImageIndex = 1
        object PageControlHost: TPageControl
          Left = 0
          Top = 0
          Width = 502
          Height = 271
          ActivePage = tabVariables
          Align = alClient
          HotTrack = True
          TabOrder = 0
          OnChange = editFilterVTChange
          object tabVariables: TTabSheet
            Caption = 'Variables'
            object ListVariables: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 494
              Height = 243
              Align = alClient
              DragOperations = []
              Header.AutoSizeIndex = 1
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clBlack
              Header.Font.Height = -11
              Header.Font.Name = 'Default'
              Header.Font.Style = []
              Header.Height = 20
              Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
              Header.ParentFont = True
              Header.SortColumn = 0
              HintMode = hmTooltip
              Images = PngImageListMain
              IncrementalSearch = isInitializedOnly
              ParentShowHint = False
              PopupMenu = popupHost
              ShowHint = True
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toToggleOnDblClick]
              TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
              OnAfterPaint = vstAfterPaint
              OnBeforePaint = ListVariablesBeforePaint
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
                  Width = 330
                  WideText = 'Value'
                end>
            end
          end
          object tabStatus: TTabSheet
            Caption = 'Status'
            object ListStatus: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 494
              Height = 243
              Align = alClient
              DragOperations = []
              Header.AutoSizeIndex = 1
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clBlack
              Header.Font.Height = -11
              Header.Font.Name = 'Default'
              Header.Font.Style = []
              Header.Height = 20
              Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
              Header.ParentFont = True
              Header.SortColumn = 0
              HintMode = hmTooltip
              Images = PngImageListMain
              IncrementalSearch = isInitializedOnly
              ParentShowHint = False
              PopupMenu = popupHost
              ShowHint = True
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toToggleOnDblClick]
              TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
              OnAfterPaint = vstAfterPaint
              OnBeforePaint = ListStatusBeforePaint
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
                  Alignment = taRightJustify
                  Position = 1
                  Width = 130
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
            Caption = 'Process-List'
            ImageIndex = 1
            object Splitter3: TSplitter
              Left = 0
              Top = 170
              Width = 494
              Height = 4
              Cursor = crSizeNS
              Align = alBottom
              ResizeStyle = rsUpdate
            end
            object ListProcesses: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 494
              Height = 170
              Align = alClient
              Header.AutoSizeIndex = 7
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clBlack
              Header.Font.Height = -11
              Header.Font.Name = 'Default'
              Header.Font.Style = []
              Header.Height = 20
              Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
              Header.ParentFont = True
              Header.SortColumn = 0
              Header.SortDirection = sdDescending
              HintMode = hmTooltip
              Images = PngImageListMain
              IncrementalSearch = isInitializedOnly
              ParentShowHint = False
              PopupMenu = popupHost
              ShowHint = True
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
              TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
              OnAfterPaint = vstAfterPaint
              OnBeforePaint = ListProcessesBeforePaint
              OnCompareNodes = vstCompareNodes
              OnFocusChanged = ListProcessesFocusChanged
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
              Top = 174
              Width = 494
              Height = 69
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 1
              object pnlProcessView: TPanel
                Left = 0
                Top = 0
                Width = 494
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
                Width = 494
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
                WordWrap = True
              end
            end
          end
          object tabCommandStats: TTabSheet
            Caption = 'Command-Statistics'
            ImageIndex = 2
            object ListCommandStats: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 494
              Height = 243
              Align = alClient
              Header.AutoSizeIndex = 4
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clBlack
              Header.Font.Height = -11
              Header.Font.Name = 'Default'
              Header.Font.Style = []
              Header.Height = 20
              Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
              Header.ParentFont = True
              Header.SortColumn = 1
              Header.SortDirection = sdDescending
              HintMode = hmTooltip
              Images = PngImageListMain
              IncrementalSearch = isInitializedOnly
              ParentShowHint = False
              PopupMenu = popupHost
              ShowHint = True
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
              OnAfterPaint = vstAfterPaint
              OnBeforeCellPaint = ListCommandStatsBeforeCellPaint
              OnBeforePaint = ListCommandStatsBeforePaint
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
                  Width = 70
                  WideText = 'Percentage'
                end>
            end
          end
        end
        object pnlFilterVT: TPanel
          Left = 0
          Top = 271
          Width = 502
          Height = 29
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object lblFilterVT: TLabel
            Left = 0
            Top = 8
            Width = 28
            Height = 12
            Caption = 'Filter:'
          end
          object lblFilterVTInfo: TLabel
            Left = 203
            Top = 8
            Width = 66
            Height = 13
            Caption = 'lblFilterVTInfo'
          end
          object editFilterVT: TEdit
            Left = 34
            Top = 5
            Width = 154
            Height = 20
            TabOrder = 0
            OnChange = editFilterVTChange
          end
        end
      end
      object tabDatabase: TTabSheet
        Caption = 'Database'
        ImageIndex = 5
        object ListTables: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 502
          Height = 300
          Align = alClient
          EditDelay = 500
          Header.AutoSizeIndex = -1
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clBlack
          Header.Font.Height = -11
          Header.Font.Name = 'Default'
          Header.Font.Style = []
          Header.Height = 20
          Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
          Header.ParentFont = True
          Header.PopupMenu = popupDbGridHeader
          Header.SortColumn = 0
          HintMode = hmTooltip
          Images = PngImageListMain
          IncrementalSearch = isInitializedOnly
          ParentShowHint = False
          PopupMenu = popupDbGrid
          ShowHint = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
          TreeOptions.MiscOptions = [toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
          OnAfterPaint = vstAfterPaint
          OnChange = ListTablesChange
          OnCompareNodes = vstCompareNodes
          OnDblClick = ListTablesDblClick
          OnEditing = ListTablesEditing
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
        object lblSorryNoFields: TLabel
          Left = 0
          Top = 0
          Width = 502
          Height = 340
          Align = alClient
          Alignment = taCenter
          Caption = 'Please use the stored routine editor to edit attributes.'
          Layout = tlCenter
          WordWrap = True
          OnClick = actEditRoutineExecute
        end
        object ListColumns: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 502
          Height = 300
          Align = alClient
          EditDelay = 500
          Header.AutoSizeIndex = -1
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clBlack
          Header.Font.Height = -11
          Header.Font.Name = 'Default'
          Header.Font.Style = []
          Header.Height = 20
          Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
          Header.ParentFont = True
          HintMode = hmTooltip
          Images = PngImageListMain
          IncrementalSearch = isInitializedOnly
          ParentShowHint = False
          PopupMenu = popupTableGrid
          ShowHint = True
          TabOrder = 0
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
          OnAfterPaint = vstAfterPaint
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
        object lblSorryNoData: TLabel
          Left = 0
          Top = 91
          Width = 502
          Height = 249
          Align = alClient
          Alignment = taCenter
          Caption = 'Stored routines don'#39't provide any data you could edit here.'
          Layout = tlCenter
          WordWrap = True
        end
        object pnlDataTop: TPanel
          Left = 0
          Top = 0
          Width = 502
          Height = 25
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          BorderWidth = 1
          TabOrder = 0
          object lblDataTop: TTntLabel
            Left = 1
            Top = 1
            Width = 220
            Height = 23
            Align = alLeft
            Anchors = [akLeft, akTop, akRight, akBottom]
            AutoSize = False
            Caption = 'Data'
            Layout = tlCenter
            WordWrap = True
          end
          object tlbDataButtons: TToolBar
            Left = 169
            Top = 1
            Width = 332
            Height = 23
            Align = alRight
            AutoSize = True
            ButtonWidth = 83
            Caption = 'tlbDataButtons'
            Images = PngImageListMain
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
              Left = 83
              Top = 0
              AllowAllUp = True
              Caption = 'Columns'
              ImageIndex = 107
              Style = tbsTextButton
              OnClick = btnDataClick
            end
            object tbtnDataFilter: TToolButton
              Left = 166
              Top = 0
              AllowAllUp = True
              Caption = 'Filter'
              ImageIndex = 107
              OnClick = btnDataClick
            end
            object tbtnDataView: TToolButton
              Left = 249
              Top = 0
              Caption = 'Perspective'
              DropdownMenu = popupDataView
              ImageIndex = 112
            end
          end
        end
        object pnlFilter: TPanel
          Left = 0
          Top = 25
          Width = 502
          Height = 66
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          Visible = False
          DesignSize = (
            502
            66)
          object lblTableFilter: TLabel
            Left = 339
            Top = 0
            Width = 114
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Create table wide filter:'
          end
          object lblRecentFilters: TLabel
            Left = 1
            Top = 3
            Width = 68
            Height = 13
            Caption = 'Recent filters:'
          end
          object btnFilterApply: TButton
            Left = 339
            Top = 41
            Width = 76
            Height = 22
            Action = actApplyFilter
            Anchors = [akTop, akRight]
            TabOrder = 2
          end
          object btnFilterClear: TButton
            Left = 419
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
            Width = 336
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
          object comboRecentFilters: TTntComboBox
            Left = 75
            Top = 0
            Width = 261
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 4
            OnSelect = LoadRecentFilter
          end
        end
        object DataGrid: TVirtualStringTree
          Left = 0
          Top = 91
          Width = 502
          Height = 209
          Align = alClient
          BottomSpace = 120
          EditDelay = 0
          Header.AutoSizeIndex = -1
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clBlack
          Header.Font.Height = -11
          Header.Font.Name = 'Default'
          Header.Font.Style = []
          Header.Height = 20
          Header.Images = PngImageListMain
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowHint, hoShowImages]
          Header.ParentFont = True
          IncrementalSearch = isInitializedOnly
          LineStyle = lsSolid
          PopupMenu = popupDataGrid
          TabOrder = 2
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toAlwaysHideSelection]
          TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect, toRightClickSelect]
          WantTabs = True
          OnAfterCellPaint = DataGridAfterCellPaint
          OnBeforeCellPaint = GridBeforeCellPaint
          OnChange = DataGridChange
          OnColumnResize = DataGridColumnResize
          OnCreateEditor = DataGridCreateEditor
          OnEditCancelled = DataGridEditCancelled
          OnEdited = DataGridEdited
          OnEditing = DataGridEditing
          OnFocusChanging = DataGridFocusChanging
          OnGetText = GridGetText
          OnPaintText = GridPaintText
          OnHeaderClick = DataGridHeaderClick
          OnKeyDown = GridKeyDown
          OnMouseUp = DataGridMouseUp
          OnNewText = DataGridNewText
          Columns = <>
        end
      end
      object tabQuery: TTabSheet
        Caption = 'Query'
        ImageIndex = 57
        object spltQuery: TSplitter
          Left = 0
          Top = 96
          Width = 502
          Height = 4
          Cursor = crSizeNS
          Align = alTop
          ResizeStyle = rsUpdate
        end
        object LabelResultinfo: TLabel
          Left = 0
          Top = 117
          Width = 502
          Height = 13
          Align = alTop
        end
        object pnlQueryMemo: TPanel
          Left = 0
          Top = 0
          Width = 502
          Height = 96
          Align = alTop
          BevelOuter = bvNone
          Constraints.MinHeight = 10
          TabOrder = 0
          OnCanResize = pnlQueryMemoCanResize
          object spltQueryHelpers: TSplitter
            Left = 338
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
            Width = 338
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
            Left = 342
            Top = 0
            Width = 160
            Height = 96
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            OnCanResize = pnlQueryHelpersCanResize
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
            object lboxQueryHelpers: TTntListBox
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
            end
          end
        end
        object QueryGrid: TVirtualStringTree
          Left = 0
          Top = 113
          Width = 502
          Height = 187
          Align = alClient
          EditDelay = 0
          Header.AutoSizeIndex = -1
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clBlack
          Header.Font.Height = -11
          Header.Font.Name = 'Default'
          Header.Font.Style = []
          Header.Height = 20
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint]
          Header.ParentFont = True
          IncrementalSearch = isAll
          LineStyle = lsSolid
          PopupMenu = popupResultGrid
          TabOrder = 1
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toAlwaysHideSelection]
          TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect, toRightClickSelect]
          WantTabs = True
          OnBeforeCellPaint = GridBeforeCellPaint
          OnFocusChanging = QueryGridFocusChanging
          OnGetText = GridGetText
          OnPaintText = GridPaintText
          OnKeyDown = GridKeyDown
          Columns = <>
        end
      end
    end
  end
  object ProgressBarStatus: TProgressBar
    Left = 545
    Top = 445
    Width = 81
    Height = 17
    Step = 1
    TabOrder = 4
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Images = PngImageListMain
    Left = 40
    Top = 64
    object File1: TMenuItem
      Tag = 17
      Caption = '&File'
      Hint = 'File related commands'
      OnClick = File1Click
      object Sessionmanager1: TMenuItem
        Action = actSessionManager
      end
      object menuConnectTo: TMenuItem
        Caption = 'Connect to'
        Enabled = False
      end
      object FileNewItem: TMenuItem
        Tag = 23
        Action = actNewWindow
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
        Tag = 26
        Action = actExitApplication
        ShortCut = 32883
      end
    end
    object Edit1: TMenuItem
      Tag = 18
      Caption = '&Edit'
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
    end
    object Extra1: TMenuItem
      Tag = 19
      Caption = '&Tools'
      object Nexttab1: TMenuItem
        Action = actNextTab
      end
      object Previoustab1: TMenuItem
        Action = actPreviousTab
      end
      object MenuRefresh1: TMenuItem
        Tag = 28
        Action = actRefresh
      end
      object MenuAutoupdate: TMenuItem
        Caption = 'Automatic refresh'
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
          ShortCut = 24692
          OnClick = DisableAutoRefreshClick
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuCreateDatabase: TMenuItem
        Tag = 29
        Action = actCreateDatabase
      end
      object MenuCreateTable: TMenuItem
        Tag = 30
        Action = actCreateTable
      end
      object Createview1: TMenuItem
        Action = actCreateView
      end
      object MenuDropDatabase: TMenuItem
        Tag = 31
        Action = actDropDatabase
      end
      object MenuDropTable: TMenuItem
        Tag = 32
        Action = actDropDBobjects
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Flush1: TMenuItem
        Tag = 33
        Caption = 'Flush'
        object MenuFlushHosts: TMenuItem
          Tag = 37
          Action = actFlushHosts
        end
        object MenuFlushLogs: TMenuItem
          Tag = 38
          Action = actFlushLogs
        end
        object FlushUserPrivileges1: TMenuItem
          Tag = 39
          Action = actFlushPrivileges
        end
        object MenuFlushTables: TMenuItem
          Tag = 40
          Action = actFlushTables
        end
        object MenuFlushTableswithreadlock: TMenuItem
          Tag = 41
          Action = actFlushTableswithreadlock
        end
        object MenuFlushStatus: TMenuItem
          Tag = 42
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
      object N7: TMenuItem
        Caption = '-'
      end
      object MenuPreferences: TMenuItem
        Tag = 36
        Action = actPreferences
      end
    end
    object Import1: TMenuItem
      Caption = '&Import'
      object MenuImportTextFile: TMenuItem
        Tag = 43
        Action = actImportCSV
      end
      object ImportSQL1: TMenuItem
        Action = actLoadSQL
      end
      object InsertfilesintoBLOBfields1: TMenuItem
        Action = actInsertFiles
      end
    end
    object ImExport1: TMenuItem
      Tag = 20
      Caption = 'E&xport'
      object MenuExport: TMenuItem
        Tag = 45
        Action = actExportTables
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object MenuCopyCSV: TMenuItem
        Tag = 48
        Action = actCopyAsCSV
      end
      object CopyContentsasHTMLTable1: TMenuItem
        Tag = 49
        Action = actCopyAsHTML
      end
      object CopyasXMLdata1: TMenuItem
        Action = actCopyAsXML
      end
      object CopyAsSQLdata: TMenuItem
        Action = actCopyAsSQL
      end
      object Exportdata1: TMenuItem
        Action = actExportData
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
      object menuSQLHelp1: TMenuItem
        Action = actSQLhelp
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object menuUpdateCheck: TMenuItem
        Action = actUpdateCheck
      end
      object menuWebsite: TMenuItem
        Tag = 56
        Action = actWebMainsite
      end
      object menuDownload: TMenuItem
        Action = actWebDownloadpage
      end
      object menuSupportForum: TMenuItem
        Action = actWebForum
      end
      object menuBugtracker: TMenuItem
        Action = actWebBugtracker
      end
      object menuFeaturetracker: TMenuItem
        Action = actWebFeaturetracker
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object menuReadme: TMenuItem
        Tag = 55
        Action = actReadme
      end
      object menuAbout: TMenuItem
        Action = actAboutBox
      end
    end
  end
  object ActionList1: TActionList
    Images = PngImageListMain
    Left = 8
    Top = 64
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
      ImageIndex = 29
      OnExecute = actExitApplicationExecute
    end
    object actCopy: TAction
      Tag = 27
      Category = 'Various'
      Caption = '&Copy'
      Hint = 'Copy|Copy to Clipboard'
      ImageIndex = 3
      ShortCut = 16451
      OnExecute = actCopyOrCutExecute
    end
    object actCopyAsSQL: TAction
      Category = 'Export/Import'
      Caption = 'Copy grid data as SQL'
      Enabled = False
      Hint = 'Copy grid data as SQL'
      ImageIndex = 114
      OnExecute = actCopyAsSQLExecute
    end
    object actPaste: TAction
      Tag = 58
      Category = 'Various'
      Caption = '&Paste'
      Hint = 'Paste|Paste from Clipboard'
      ImageIndex = 4
      ShortCut = 16470
      OnExecute = actPasteExecute
    end
    object actUserManager: TAction
      Tag = 34
      Category = 'Tools'
      Caption = 'User manager'
      Enabled = False
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
      Tag = 57
      Category = 'Various'
      Caption = 'About...'
      Hint = 'About this application'
      ImageIndex = 99
      OnExecute = actAboutBoxExecute
    end
    object actMaintenance: TAction
      Tag = 35
      Category = 'Tools'
      Caption = 'Maintenance'
      Enabled = False
      Hint = 'Optimize, repair and analyse tables'
      ImageIndex = 39
      OnExecute = actMaintenanceExecute
    end
    object actCopyAsHTML: TAction
      Tag = 49
      Category = 'Export/Import'
      Caption = 'Copy grid data as HTML'
      Enabled = False
      Hint = 'Copy grid data as HTML'
      ImageIndex = 32
      OnExecute = actCopyAsHTMLExecute
    end
    object actCopyAsCSV: TAction
      Tag = 48
      Category = 'Export/Import'
      Caption = 'Copy grid data as CSV'
      Enabled = False
      Hint = 'Copy grid contents as CSV data'
      ImageIndex = 49
      OnExecute = actCopyAsCSVExecute
    end
    object actCopyAsXML: TAction
      Category = 'Export/Import'
      Caption = 'Copy grid data as XML'
      Enabled = False
      Hint = 'Copy grid data as XML'
      ImageIndex = 48
      OnExecute = actCopyAsXMLExecute
    end
    object actExportData: TAction
      Category = 'Export/Import'
      Caption = 'Export grid data as CSV, HTML, XML or SQL ...'
      Enabled = False
      Hint = 'Save grid data to text file'
      ImageIndex = 20
      OnExecute = actExportDataExecute
    end
    object actPrintList: TAction
      Tag = 61
      Category = 'Various'
      Caption = 'Print...'
      Enabled = False
      Hint = 'Print List or Data'
      ImageIndex = 34
      ShortCut = 16464
      OnExecute = actPrintListExecute
    end
    object actCopyTable: TAction
      Tag = 62
      Category = 'Tools'
      Caption = 'Create table copy ...'
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
      OnExecute = actExecuteSelectionExecute
    end
    object actExecuteLine: TAction
      Category = 'SQL'
      Caption = 'Run current line'
      Enabled = False
      Hint = 'Execute Line|Executes the current line of SQL'
      ImageIndex = 105
      ShortCut = 24696
      OnExecute = actExecuteLineExecute
    end
    object actHTMLview: TAction
      Category = 'Export/Import'
      Caption = 'Image view'
      Hint = 'View image contents'
      ImageIndex = 32
      OnExecute = actHTMLviewExecute
    end
    object actInsertFiles: TAction
      Category = 'Export/Import'
      Caption = 'Insert files into TEXT/BLOB fields...'
      Enabled = False
      ImageIndex = 47
      OnExecute = actInsertFilesExecute
    end
    object actExportTables: TAction
      Category = 'Export/Import'
      Caption = 'Export database as SQL'
      Enabled = False
      Hint = 'Dump database objects to an SQL file'
      ImageIndex = 20
      OnExecute = actExportTablesExecute
    end
    object actLoadSQL: TAction
      Category = 'SQL'
      Caption = 'Load SQL file...'
      Enabled = False
      Hint = 'Load SQL file...'
      ImageIndex = 52
      ShortCut = 16463
      OnExecute = actLoadSQLExecute
    end
    object actDropDBobjects: TAction
      Category = 'Database'
      Caption = 'Drop ...'
      Enabled = False
      Hint = 'Deletes tables, views, procedures and functions'
      ImageIndex = 16
      OnExecute = actDropDBobjectsExecute
    end
    object actEditView: TAction
      Category = 'Database'
      Caption = 'Edit view ...'
      Enabled = False
      Hint = 'Edit view ...'
      ImageIndex = 103
      OnExecute = actEditViewExecute
    end
    object actCreateView: TAction
      Category = 'Database'
      Caption = 'Create view ...'
      Enabled = False
      Hint = 'Create view ...'
      ImageIndex = 102
      OnExecute = actCreateViewExecute
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
      Caption = 'Create new table'
      Enabled = False
      Hint = 'Create new table in selected database'
      ImageIndex = 15
      OnExecute = actCreateTableExecute
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
    object actEditTableFields: TAction
      Category = 'Database'
      Caption = 'Table properties'
      Enabled = False
      ImageIndex = 44
      ShortCut = 16397
      OnExecute = actEditTableFieldsExecute
    end
    object actEditTableProperties: TAction
      Category = 'Database'
      Caption = 'Alter table ...'
      Enabled = False
      Hint = 'Edit table engine, charset, auto increment etc.'
      ImageIndex = 17
      OnExecute = actEditTablePropertiesExecute
    end
    object actEditField: TAction
      Category = 'Table'
      Caption = 'Edit column'
      Enabled = False
      Hint = 'Edit column properties'
      ImageIndex = 93
      ShortCut = 32781
      OnExecute = actEditFieldExecute
    end
    object actCreateField: TAction
      Category = 'Table'
      Caption = 'Add column'
      Enabled = False
      Hint = 'Create new column in selected table'
      ImageIndex = 91
      OnExecute = actCreateFieldExecute
    end
    object actDropFields: TAction
      Category = 'Table'
      Caption = 'Delete selected column(s) ...'
      Enabled = False
      Hint = 'Delete selected column(s)'
      ImageIndex = 92
      OnExecute = actDropFieldsExecute
    end
    object actEditIndexes: TAction
      Category = 'Table'
      Caption = 'Edit indexes'
      Enabled = False
      Hint = 'Create and edit indexes for the selected table'
      ImageIndex = 18
      OnExecute = actEditIndexesExecute
    end
    object actDropDatabase: TAction
      Category = 'Database'
      Caption = 'Drop database'
      Enabled = False
      Hint = 'Delete database and all contained tables and objects'
      ImageIndex = 7
      OnExecute = actDropDatabaseExecute
    end
    object actCreateDatabase: TAction
      Category = 'Database'
      Caption = 'Create database'
      Enabled = False
      Hint = 'Create a new, blank database'
      ImageIndex = 6
      OnExecute = actCreateDatabaseExecute
    end
    object actEditDatabase: TAction
      Category = 'Database'
      Caption = 'Edit database'
      Enabled = False
      Hint = 'Edit database properties'
      ImageIndex = 8
      OnExecute = actEditDatabaseExecute
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
      Enabled = False
      Hint = 'Refresh'
      ImageIndex = 0
      ShortCut = 116
      OnExecute = actRefreshExecute
    end
    object actImportCSV: TAction
      Category = 'Export/Import'
      Caption = 'Import CSV file...'
      Enabled = False
      Hint = 'Import a CSV or tab delimited file'
      ImageIndex = 50
      OnExecute = actImportCSVExecute
    end
    object actExportSettings: TAction
      Category = 'Export/Import'
      Caption = 'Export settings ...'
      ImageIndex = 100
      OnExecute = actExportSettingsExecute
    end
    object actImportSettings: TAction
      Category = 'Export/Import'
      Caption = 'Import settings ...'
      ImageIndex = 101
      OnExecute = actImportSettingsExecute
    end
    object actPreferences: TAction
      Category = 'Tools'
      Caption = 'Preferences'
      ImageIndex = 98
      OnExecute = actPreferencesExecute
    end
    object actFlushHosts: TAction
      Category = 'Tools'
      Caption = 'Hosts'
      Enabled = False
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushLogs: TAction
      Category = 'Tools'
      Caption = 'Logs'
      Enabled = False
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushPrivileges: TAction
      Category = 'Tools'
      Caption = 'Privileges'
      Enabled = False
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushTables: TAction
      Category = 'Tools'
      Caption = 'Tables'
      Enabled = False
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushTableswithreadlock: TAction
      Category = 'Tools'
      Caption = 'Tables with read lock'
      Enabled = False
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actFlushStatus: TAction
      Category = 'Tools'
      Caption = 'Status'
      Enabled = False
      ImageIndex = 28
      OnExecute = actFlushExecute
    end
    object actUpdateCheck: TAction
      Category = 'Tools'
      Caption = 'Check for updates ...'
      ImageIndex = 94
      OnExecute = actUpdateCheckExecute
    end
    object actWebMainsite: TAction
      Category = 'Various'
      Caption = 'HeidiSQL website'
      Hint = 'http://www.heidisql.com/'
      ImageIndex = 69
      OnExecute = actWebbrowse
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
      Caption = 'User to user forum'
      Hint = 'http://www.heidisql.com/forum/'
      ImageIndex = 95
      OnExecute = actWebbrowse
    end
    object actWebBugtracker: TAction
      Category = 'Various'
      Caption = 'Bug tracker'
      Hint = 'http://bugs.heidisql.com/'
      ImageIndex = 96
      OnExecute = actWebbrowse
    end
    object actWebFeaturetracker: TAction
      Category = 'Various'
      Caption = 'Feature tracker'
      Hint = 'http://rfe.heidisql.com/'
      ImageIndex = 97
      OnExecute = actWebbrowse
    end
    object actReadme: TAction
      Category = 'Various'
      Caption = 'Readme'
      Hint = 'Open readme file'
      ImageIndex = 68
      OnExecute = actReadmeExecute
    end
    object actSaveSQL: TAction
      Category = 'SQL'
      Caption = 'Save to file ...'
      Enabled = False
      Hint = 'Save SQL to a textfile'
      ImageIndex = 10
      ShortCut = 16467
      OnExecute = actSaveSQLExecute
    end
    object actSaveSQLselection: TAction
      Tag = 1
      Category = 'SQL'
      Caption = 'Save selection to file ...'
      Enabled = False
      Hint = 'Save selected text to a file'
      ImageIndex = 10
      ShortCut = 24659
      OnExecute = actSaveSQLExecute
    end
    object actSaveSQLSnippet: TAction
      Category = 'SQL'
      Caption = 'Save as snippet ...'
      Enabled = False
      Hint = 'Save as snippet ...'
      ImageIndex = 54
      OnExecute = actSaveSQLSnippetExecute
    end
    object actSaveSQLSelectionSnippet: TAction
      Tag = 1
      Category = 'SQL'
      Caption = 'Save selection as snippet ...'
      Enabled = False
      Hint = 'Save selected text as snippet ...'
      ImageIndex = 54
      OnExecute = actSaveSQLSnippetExecute
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
    object actQueryStopOnErrors: TAction
      Category = 'SQL'
      AutoCheck = True
      Caption = 'Stop on errors in batch mode'
      Checked = True
      Enabled = False
      Hint = 'Stop on errors in batch mode'
      ImageIndex = 63
      OnExecute = actQueryStopOnErrorsExecute
    end
    object actQueryWordWrap: TAction
      Category = 'SQL'
      AutoCheck = True
      Caption = 'Wrap long lines'
      Enabled = False
      Hint = 'Wrap long lines'
      ImageIndex = 62
      OnExecute = actQueryWordWrapExecute
    end
    object actQueryFind: TAction
      Category = 'SQL'
      Caption = 'Find text ...'
      Enabled = False
      Hint = 'Find text ...'
      ImageIndex = 30
      ShortCut = 16454
      OnExecute = actQueryFindExecute
    end
    object actQueryReplace: TAction
      Category = 'SQL'
      Caption = 'Replace text ...'
      Enabled = False
      Hint = 'Replace text ...'
      ImageIndex = 59
      ShortCut = 16466
      OnExecute = actQueryReplaceExecute
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
    end
    object actCreateRoutine: TAction
      Category = 'Database'
      Caption = 'Create stored routine ...'
      Hint = 'Create stored routine|Create stored procedure or function'
      ImageIndex = 119
      OnExecute = actCreateRoutineExecute
    end
    object actEditRoutine: TAction
      Category = 'Database'
      Caption = 'Edit stored routine ...'
      Hint = 'Edit stored procedure or function'
      ImageIndex = 119
      OnExecute = actEditRoutineExecute
    end
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'reg'
    Filter = 'Registry-files (*.reg)|*.reg|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Export settings from registry...'
    Left = 8
    Top = 200
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'reg'
    Filter = 'Registry-files (*.reg)|*.reg|All files (*.*)|*.*'
    Title = 'Import settings to registry...'
    Left = 72
    Top = 232
  end
  object menuConnections: TPopupMenu
    AutoHotkeys = maManual
    Images = PngImageListMain
    OnPopup = menuConnectionsPopup
    Left = 72
    Top = 64
  end
  object PngImageListMain: TPngImageList
    PngImages = <
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002774944415478DA8D536D485351187EEF364AB7248BE9BE
          528C1512850442B03FE1A0B0C8329811146596F6A35C1A610D4C88CAB0245256
          426499F52F8A90024B4BA3AF61392109BB6B9BD3C24CF6A31ABAED9E73EED6BB
          0F876B8A1D38BCF79EFB3ECF7DDFE7790F170E8761B1B5F7FE2E0B09D2E6C755
          DDE2BFDFB8FF2128BD593C4C82E443774D7FE58204275E1DC9619499A8C08A89
          400BA940B284200504826EA50E08A1C0BB1CADEFCF0FD6A61098FB0E1B286567
          74725DE9F2B44C902F518084E380862850C6A2314804B07F1A029E77347EB63A
          CE26088EF796E750C2ACF9CA75A5D98A6C9826D308960215698C8062151809BE
          DB0787C0C13B9B9DB7464F27082A9FECABD528B4D7F44A3D8218FC09FA80F7F0
          E0F438B17C0A02B6B04AAB4322065F79E775D7ED3173520B071E98BAD7AB376C
          5B26CF8099801F7A6C3DDFB0EF87AF1B064ECD26E51FD50FA33E7677C778458A
          8865774B7E2220137748089000C6B6B7E73E36CC4D5A5D9E5B2F32B1A968B7E1
          60A422121737F2CC95DCD852113B24899231F11E022C2D9BDB1BE712EDE9DC79
          01F38EE14EC79F4930FE9E770ED0D20E2CB9B06D6B67C1EC99E9CE8EAB082833
          6E34E67232094C4E4DC2CB37FDCF5208D052AB5AAEA96684817BCA0DB3256B57
          68214F9507B2A532208C462D1D19F9723289A0FAC5A12BAA344DDD1AE55A1045
          116D64313BE396FA023E904A64303AE6019B6DA00BED372711543DDD7F5195AE
          AED767E9410C85A2FE47C09119F0FB67C0FBCB0B13133F801F7174A1AD97D152
          5B4A0B78715A5419EA9AC8E8BAC65D71B59148205E1C713B8EFA73D4E7115AFA
          7DC1CBB4BDD5D88E2A6FEAB3BC2B8045D6BC0445970C5214AE0E2F4ED362047F
          01BF2299BDF1621A240000000049454E44AE426082}
        Name = 'PngImage0'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002AB4944415478DA7D525F48536114FF7DBBD64037DD7457
          E7FEE814A230C4CC36B1C860F48F8AE8A1C79ECAE8D1B70814DF222D4A1F144A
          A795C52D6AD25434FF4453584146B6A92D11225F0AE9C1D43162BAEDEBDCCF26
          0AE97739DFB9DFBDDFF99DDF39E7C738E7D8694D4E4E3E8CC7E397565757D3C8
          636D6D4D189D55FF9C6D07100A85B228C06330182E6AB5E9885110924988DB14
          63341A30343488FF020483C1D36AB0C964B2666464E0FBFC0FCA16A7788E244F
          121087395FC6BBC03858381C8E251289DD645029AAA6D2DB4C7713652C2EFE26
          A0244E9E3A83F1B13760D3D3D3DC662BC0D2D2B2C8CED587AB2CB930F5403991
          6DCC425FAF0F8EA2BD481080D562C6E80895404DE246A309D73B3ED13506C668
          5FDF84572DB2B2823BD75C18F38F221A8D42657BF6DC050C0EF4824D4C4C7093
          9C871B9ECF3066E740A3D1806924E1D7DF35F836378BDB579D181919C0BE9203
          E0D4039BD50C9FCF0B160804B8D962475D57083926792368B39FFBFA058D3515
          22637EB58CBE791FEAF737C2FB4201F3FBFDDC6E77A0BE7B0672AE796BB05A02
          F9D99929DCBA520EDF2B2FDEDA5F231A89A3BDFA119E298FC18687877971F11E
          343C0D23D76C41516EBAA87F7D31FC5C8E233C15C4CDCBE522A3D375549D220A
          0B2C78D2DD05D6DFDFCF4B4A4A51DBFA7E474536D61CA2804E541D76130087A3
          D086AECEFB603D3D3DDCE9AC44241211E35105138DC6687A4968B5BBA80C2ED4
          A7D7E9E0F174E048F509A14887C38EF607AD608AA2DC25811C4B09857CB65EAF
          2F5285B4B0B010A37E68359224B461B158515676503032E7C96868A8DB2AE596
          96965A0239EFAAAC7267661AE17DA9FCA1992BA405177D2F4DA933A558FA37BB
          01D0DCDC2C4B92F4CBED3E0E9D5E4F1712489334047A2F4AC0BAED7AB301D0D4
          D42411FD0F94A122A5FF7F193FB6B5B5B9B603F80B10F37EDA972DDD14000000
          0049454E44AE426082}
        Name = 'PngImage1'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002754944415478DA63FCFFFF3F032EA09E3DCB504658A0E0
          EAA317292F16E4FDC6A686119F014EB52B323D8D55CB3BD71ED17CB338FF3B4C
          5C3C6112DBCB0579BF081AE0DEB82A3FC246B7A36AC91EC9E7F3733F80C414D2
          A6E9684B09CFDFDA106E4AD000EFE635A529EEC65D79B3B6293C9E93FD1068B3
          5AAA8BE1D1E76F3E3D985BE04DD88080F6F53585FE96CD71FD6B75BE7CFFF5A1
          C0DBEC8C94A8E0AFE3D7EF37CEC9F59E47D080E0CE8D2D35E176D5816D2BD262
          6CB54B3DCD34943B57EEAFDB5417DE4A542086F76CEEAC0BB72FDB79FEEED760
          2B75EECCC91BE66D6B8C4A26180B1D3C8C4C155FFEFF8BEADB32A125D639FFDA
          E3D70C2BF79F3FB0B8D8DF116F34F6F0B1C84B4B885709F3F11B3F7EF162EF99
          9A95CE199E66C67D1B8E1DDB7AE686DB9BC5855F711A00D2ACABAEB1435C5040
          E3F78FEF9F79F978780F15CC62F0D09763A8587EE4C0BE0B376B98FEFF3DF66C
          7EFE7FAC06ACD4909DA32227977CF8EC396B86FFFF255DCD8CD71C2F5DC8C025
          20CC60591FC8F0F9FB2F8663D7AE65653CFF301DAB01876D8CDEBCFAF4F170F0
          A5BB812B54A5666A6B6BA42D0AEEFA9FB4A694F1CCA5ABF3642524CDFFFDFFCF
          7DF6DA55EDD24F7FBF6118B0D74AFF1E2F2B1BFBCB8F1FEE6A4B49D99E7BF0E0
          301B1B2BA7080F9FFA81F3E705C505044B74D4543B8E5EBAA458F4F6FB030C03
          A64B0A241BAA284DE5606165BFFDE2C5C6DB8F1E65E829294D971416F2DB71EA
          8CB4B2AC74ADACB844F2F18B17654A3EFE7E8335167A05D8E4191919D9FFFCFE
          7DB7ECCBBFBFB365840BECF575FB9FBE7A734F584850E9FA83074BC36F3E8E21
          980E10D1CACC2525293955454232E2E9BBB787EE3E7C945CF2E9CF1364350013
          7833F03519402B0000000049454E44AE426082}
        Name = 'PngImage2'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002604944415478DA85935D48935118C7FF67EF6699A82392
          6A52B458765110E4D64D19E1452054A0AB8BE84229BC88B0D475B1D84DF97193
          D54518F6E117215D4497051911F485B2694E05238746E8C0B51C6BCD6DEFDEB3
          737A36D9456BAB03870387E7FCCE737EFC0F3BEE1C7B5A566AB0E874304AC814
          C0C0A484CAE55A24A60E2643A1871F1E9C4AA0C060F62EB7F799CB56F23D2A14
          461B820E6B2901BD020C8F8731E9F6BF59595EBEFCBEEF442C2FE04CB767FE7E
          6BB5FE734040D14924B94024CE71ACAA18DE00C7D462042F5ECF3D7ED959733D
          2FE074B7C7D7DF56AD2CFC10E9EEA111E0573285DD4681B24D066C3430B4F5CF
          8847970E5A0A0206DAAB956F2101EA1E9A90886B025FFC3FCD2AE7E0044C8F79
          FF6A8C09C92209BEBCB4141F21373DE426CEEC04186AB72A4B619929E40450B5
          143C0B41F3C55A13825181026E9E939BB3ACA1CBE31B7458157F38FD024185EB
          9077732BE673874DF88F9B5E56DFE9F60D5FB5292B1140C7900108EAFAD5ACDF
          DC78C4847FB969BC3925597DC7846FC071400946F57F019A6A4CC87513274894
          E6A19D1BE0E89B91ACE1C6D4E2BDD65D88248C7F00460970BEA612B96E545A13
          04DABFBD08577AA98393AEF1D921A7B5848BF59B748C9248A027630173D3D14A
          E4BAD1A828A64AECDDAA47CBDD4F92D539C746E9484592A31C90F1CC8321515A
          AEEC2337C875439743A5C0575500176E4F488ABDCC9BF186CE498DDCE873DD64
          01962D1CCDB7A60B03EC1D5E99CF4D16B0C31846CB9DAF2808A8BB36BE3AE2B2
          6DCE7593892F4D85BE6F73CF64B220A0D6F1F1ADA2637BA8741B3959CBBA5907
          304A358F96971415FF069DF9921C980053860000000049454E44AE426082}
        Name = 'PngImage12'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002494944415478DA7D934F68134114C6BFDD9460B191600B
          A2422D34BD7812110F0942298A69D39418A5480D91544313E2C14B4128585173
          B0DED58B201E3CA9287AD08B16935C3C89E2411224693592522D3524D9BF337D
          BBD984FCD3C77ECC2C33EFB7DF7B332B70CE61C4834B238769F88AEED88E3DCC
          3BF18F10EECF1F3A4AE30D927F60EF3E5C585A4665F3A3B9B87BE8381E5FBF86
          5AE5AFF1FA84F49C60CF3A01F9938133C3FD030E41FDFE09C3C105948BABE6A2
          E3C0380A4FEF41DC3F0A72AABC79F14A2180A313C0FDA18B50CA1BD04A05889A
          02986571EBE150FB9DD83574106F5FBE0601846EC05C08F276095CD7C1748DF2
          595B9D4CAA42E873E2FDEABBDE80E9F37390B67E81A9B229588D6DDBF83F806F
          7616B53F4572405FD7543032F0E5F71E48BAAD5E0601B960C75AA18092387297
          D1069DDC92BE9980A9736751DDFC011080D182A289C8A9A3F0476F4351942E37
          06D066B3A9F1787CC5044C0603A86DAC910306A631C8BA809CE68237B28C4C26
          03DE525263EE76BBD54422510778037E544BEB6632D3C80101B2BA0BBECB3721
          49525B62636EB7DBD5582C56079C9EF1A15224071640D64564F918FC0BB7904E
          A79BC9AD10C341341AAD034EF92651F959B000F512B218C34C3CD9D381118683
          48246201BC5E94D7F3660F3839900C80E042E0CA1DA452A99E4DF4783C6A381C
          36011FC627264E5057C1191D171D91AC029FB706313DBFD474D0D944C3412814
          320153F49E241D696CD2D187B2F318AE261F4196E59E7FA1718CC1607045E03D
          6E1D5913E9922C9206AD0BD39471895A94DB01E2AE726CADF7CA7A0000000049
          454E44AE426082}
        Name = 'PngImage13'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100804000000B5FA37
          EA0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000001614944415478DA
          7DD1BF4B82411807F0EF6BA6A544D31B2D12525616B435673FA6060984B00824
          68696AA845E84FE82F085A2A2884888A24AC3797A0A1A5C141221A02C1EEC2B2
          FCC97BDEF5A0BE260DDD70C33D9F7BBECF719AC2FF4B6B00C3A9826A498E2A9F
          9232A59EE59E34166A2D608CA9237DA21F6EB8A090C7375EC1EF5438946982EB
          E448C003ADADB1400A4FFB8B912648B029DDF1077CC0E0E1BE26B86201FD135D
          70A293224AC821834124F9B205E26C5A2F527285CA025FA8C20E1F6EF98A052E
          0954A92CA860E21D453AF35244C402176C462FA14CC04180A3001B45DCF0550B
          9CB159BD40D982A630F146A003C348F0350B9CB2391AB2841A01812C4D63871F
          71BE6E811302390212DDF50E79EA308E73BE6181189BD475BA59AE0346BB072F
          B8E75B16389E9731BF7B8002347AA84911693C664530FAD0FAAC43AFDC96215B
          AF8B66C8A1921507B59D286BFBCDC6DAED1143429AE9CDEAEFD90F714CA40171
          C73EFC0000000049454E44AE426082}
        Name = 'PngImage3'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000028E4944415478DAA5935D4893511CC69FCDE9A6438260C3
          2EC648A6AC4542145E4417F32B624531126C0E6948DD18045D044511044605ED
          D268174164B02C99632863EEA38B44EAA64877E148B1C54279DFE9B6F629EEA3
          F33FD0EAA58F9B0E3C9CF39EF7FDFFCEF37F38AFAC56ABE17F86EC5740281452
          B2E7334CB66AB56A647307135B56A36C5E65F313A6B0D56AADFC0660C526B676
          6BB5DAAEB6B636A8D56AB4B4B480DE67321964B359C4E37188A2B8C0F6CE0F0E
          0E7E9500E6E7E75F1B8D46B34EA7834C26FBA3DD72B98C68348A582CF66C6868
          E8820410080404B3D9AC696A6AFA2720954A915BD166B3692500BFDF2FF4F4F4
          68D2E934542A15944A251A1B1B790B854201DBDBDB48241230180C884422A2DD
          6E9702E6E6E684DEDE5E4D3E9FE7FD964A255E4CA752063B3B3B502814E8ECEC
          44381C16474646A480D9D9590EA00FA9980AA960777717C96412B95C8E7FD7DE
          DECE5B70381C5280CFE713FAFAFA3464B7582C7200E54100963C772597CB790B
          C160501C1D1D9502BC5EAFD0DFDFAFA193084200CA81009B9B9BF8F865116BE9
          65E44ADF5028E66B5B59E1B6FFFED2781DE0F178848181011E22012A950A0710
          28F87E065B8A188E988E42B7B70391A8176FA36FF07935E9AC03A6A7A73980D2
          2600BB71686E6EE60E1EF8AEC27AEA24D020C7D90357F03074090D90E3C58CAF
          54074C4D4D09DDDDDD1A36B865CAE107E0DAF3618C0D5F86E5E0C5FA9DF02D4F
          E0EEE3F19F19B8DD6E0B3BF5A5C96452EBF57A6E9F2E14BDB7DC3C84D31633CA
          A8E2C689A7B8177040D5A0943AA0313939B99F416E319D6389EFA17F813208AF
          78906A5DC6B1AEE3E8D877189F363E6071690189F58C53F6B7DFD9E572B5B262
          035395B5B1F26ADD79876D8F31B53265991EBD9BD8B8FE1D5FA69F9C7154F3A1
          0000000049454E44AE426082}
        Name = 'PngImage4'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002914944415478DAA5934B6853411486FF9B479334684048
          C8260643AC2182828BE046489A46C585D05AA8A1146B5DB8E8AA6B5F3B71E1C6
          9514716317B1124A088DA5495385560A3EA842D0162CA59A68736F4C52D3BC9A
          DCC43903062F3E360E1C66EE9D73BEF9CFCF8CD06EB7F13F43F815B0B0B0A063
          DFE759045BAD968BCD8759B0652BC5E68F6C7EC822D9DFDF2FFF0660C56EB60E
          592C966356AB1546A311DDDDDDA0FD9D9D1D944A256C6D6D4192A465F6EFE2E0
          E060460188C7E3CF5C2E97D766B34110843FCA6D369B48A552585F5F7F343434
          744901989F9F17BD5EAFB9ABABEB9F8042A1406AA56030685100E6E6E6449FCF
          672E168BD0EBF5D0E974D06AB5BC854AA5827C3E8F743A0DA7D389C5C5456978
          78580988C562626F6FAFB95C2EF37E6BB51A2FA653C9837ABD0E8D46839E9E1E
          2493496964644409989D9DE5004AA4622AA48246A3815C2E87DDDD5D9EE77038
          780BA3A3A34A40341A15FD7EBF99E456AB550E203F08C09CE7AA542A156F2191
          48486363634A40241211FBFAFACC74124108403E10607B7B1B7B2B6108AB51C8
          D934F68CFB5BEA6AF1DA9944F34E07303333230602016E22016459E60002659F
          3E80E9CB0A8E7807A0731C45F55D1CEF9712727E6D6DA2030887C31C406E1380
          DD38180C06AEE0F38D004E5D9E8061E339907901984CC869EC78135FDEEC00A6
          A7A7458FC76366834B261F7E0232E3C7E1BFFB18C28981CE9D28DEB2E2F5D2B7
          7607100A85CEB1539FB8DD6EA3DD6EE7F2E942D1FECB2B2E7802A761FC1443BD
          9A4585E597BEABF1610319C5639A9A9A3AC420D7595C608E9BE82D9007DAD508
          6C85B73878600F1A550625A989CDAC5A6ED4DA3785BF3DE7C9C9C97DACD8C9A2
          C5DA583BF9EADE4439FFF5AA5A16ECB2BA9D6155F7CF269AB77F00C983A9A032
          4E43BE0000000049454E44AE426082}
        Name = 'PngImage5'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002D74944415478DA7D93DD4B53611CC7BF47F7E65B23F51C
          8CB298AD5AEB0D168C880C5F490A0BC132F1422ABAA82E2D0AE922E822FA07EA
          A602F5621926228A38DF7A9D9045968B1C893A70E8CE99736B673B67EE6CEB79
          CE4538B4BEF0E3F770CEF97E7ECFEFF73C8749A7D3A01A1D1DD593F57912CDA9
          54CA42F23E12649972933C47F27312630D0D0D496C104301C46C25D9C171DCD1
          929212E4E5E521373717F45D381C46241281D7EB8520081FC8B3CB8D8D8DBE0C
          80D3E99CB0582C15A5A5A56018065B495114B8DD6E783C9ECEA6A6A6D60CC0F0
          F0305F5151C1EA74BAFF02D6D6D6E86E85E6E6662E03303434C4575656B2A150
          080683017ABD1E5AAD566D21168B21180C6269690966B319E3E3E3424B4B4B26
          60707090AFAAAA62A3D1A8DAAF2CCBAA9956A53388C7E3D0683430B302A2D3AF
          919D7F04D1C05B30B2AF55050C0C0CA800FA213553233524120904020188A288
          0265123B10440E5B0EA3C986D0820BB3C3CFFC2AA0BFBF9FAFAEAE66E9762549
          5201741E1440260F842660DE2921DF684370CE0363F15EE80A38FC1CEF945540
          5F5F1F5F5353C3D24A144201740E14F07B690426D60763591DE2CB2F21AD32E0
          676348C8EBABB2249E5401BDBDBD7C6D6DAD3A440A4826932A20199A04ABFF01
          A3F90264DF5364E91424C43D587DE7F207454D9DAD6D745A05F4F4F4A8003A6D
          0A20370E49C185926DF3E00E3710F313646915AC474C587E339566A4907DFFED
          4F9FFF9E427777376FB7DB5922ACACAC903E9D28601670E0743DD6FD1D60B2D7
          218777817FFF154EF144F0D69D074519C7E87038CE92AAAFAC566B1E842FD02B
          F350180E45451FB19DD311732978D70CC622C785B06238D7DEDE3E9501A0EAEA
          EA3211C8FD9C95CEAB976EBE80C7D106DFA20BFADDC7200991945767EFF0278B
          EF1133BFE967DAA887D70F25EE5EBBA1415612D3237D585CF8251AD9C253671E
          7DFFB6D515DF04B8585E28D9CA388DFDA009696DDC25F1C295FAC733F3F887FE
          0095EFBA08CBF72DB80000000049454E44AE426082}
        Name = 'PngImage6'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002E74944415478DA7D93DF4B536118C7BFEF9AFBA9A6D0C6
          209705FE188B02B3A678219B9B41BF9316B6C48220E9A6FE80E8A22E24A42BEB
          CAA28B94A636648DC970E9B48B02C9B49B45D90F4ADDCCCE6A9B4EF7C36DE7F4
          BE278256D1737878DE73CEFB7CCEF37C9FF710411030313121A7F1387507CFF3
          061AABA9D3251FA4F1038DF7A907DADADAF2F8C3C8F8F8B8916E1AD46AB57B75
          3A1DD46A35542A1518787575158944020B0B0B884422CFE8B33376BB3D5C00F0
          FBFD530683C1ACD7EB4108C1BF2C97CB21180C627E7EBEBFBDBDFD7C01606C6C
          8C339BCD1A994CF65F402C1663AD461C0E87B600E0F3F9388BC5A289C7E35028
          1490CBE5282A2A125B4826938846A3088542A8AAAAC2E4E464A4A3A3A310303A
          3ACAB5B4B468363636C47ED3E9B498CCBECA34C86432904AA5A8A9A941201088
          7476761602BC5EAF08601B59324B6409371F47512207929B399417CBB0CCA540
          2440B16A0B788120B696C2E4AB5003F1783C9CD56AD5B07253A99408607ADC18
          FE8A23965A9CDC570CEF7408C71A2B0A74793015C1D48B4F206EB79BB3D96C9A
          F5F575B16706603A5C7DF805D626032E98CB70E5CE2C6E5FAE1713D33960939E
          86BBBE305EBF5F02191919E15A5B5B451119209FCF8B80EBAE6FB035D4E26C73
          2908BD2474400205F0BC0036AC1ED767CCBDA10097CB250298DA0C404F1C944A
          257A3C31ECDFBD13E1EF09313997E7C153671026325B849797418686863893C9
          A4A186959515510706E876475167D453B1722C85B69617E1AC025EE0E97D968E
          97029C4EE761FAE291D168545756568AE5B30375EED61CAC076A616F5622C76F
          528844FCAA407828A432DC722E6226F80E8495333030B08B42AE513F259148B6
          B27FC119D4E150C31E9C68CEE2DE4C17B6A977302510D9584297A917BD83194C
          CDBEFC09F8DDFAFAFA4AE824AA9E46EBE62E1E35E2605D14D3B16194C9B68B45
          C4D31C1ACB4FA3BB3F0FFFF4ECDF805F567FE9C9F3EA0A655399BC9C965C4ADB
          C882500D8840B52049BC5DFC086E2D8B1FCF1E95A402AAA15F0000000049454E
          44AE426082}
        Name = 'PngImage7'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002A04944415478DA8D924B4894611486DF6FC699B10BA6A6
          74752A17BA8816DE6D0CDA0546102D8B16455AA99578CB16A545178A042D3349
          87891645814952C6384A174A94F19E894269E122B3209C1975749CFF3F9DEF53
          8236E2371CCE59FCEF732EEF8894DCD60200DB38F670C472AC052884B300413D
          A341C0C041A443D7316536E1AB7F7EE15657EDFE06C1807AFBF98C2CF97DFC16
          8BD2904E0804391674255E6531400860DAAF637A4EA3D18969ED9173D433FCFD
          77A9048CD84B32E21BDD3E941E8A96729CBDD385DBA79310D481509380D964C0
          3CC33CB31A67C2D8E42C8CD071E561EFBC044CD59764AC7BDEE945F1C1283572
          614D376E9C4C84C9C8DDCD061E1DF0FA35CCCCC915088EB65FC84C0A43617507
          4940A0B6C8666A727B9197B95E012ED9FB702D2B01AB438D30F10AFE002F3EA3
          F144EA060C9844C2760B2A9EF482012EFD5EA14DBCE415B2F7452A006BB0C662
          E4EE02418DBBF3E8B3014D89897F0ED74FECB29A51F5AC4F01E86EBE0DCDDD5E
          4CFC18E76309053085F0E11816E491837C8C2543D40B0F8FC44EAB05350D030C
          C87151657E3A5E7779D4AE52A5FC530A02EBB9330348D6A46A033789DB2850F7
          6270115071261DCE1E8FA2FF335F401D4C0A97FC579964C1DFC4460B385E7D5E
          04DCCC49455BBF77694062AD50599300D95D4EA2CBD0D47F842B5823098F9DC3
          12D042D74FA5E2ED800FE547B66225AFB86E08311184A7AD2310697CC4CB5929
          783FE845F9E11854B59F58569C6FB3A3E8C1276C0E2734BEF9C2803C17951D4F
          C6C7211FCA18E01E7FB72C2025662F0AEEF7635318A1E9030352735BF48BC792
          45C7B004585734C1B9EA1E6C604073FB2889E49C96B10B471377480F0FA445AD
          E806571D6E756067E7B73FF288956C551C5F7D374322D83C651DA465A4FF9741
          8BB6AA02E483300CFE058C4769232FDE7CCE0000000049454E44AE426082}
        Name = 'PngImage8'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002BF4944415478DA85D17B4853511C07F0EFDDE6B46D0EAD
          ACAC6513532786D2141FAD049122A530CCA9052142A91B54D61F419882547F89
          A8911945D1044B0A17685119E22B45CB47A9203ADFA998E6B33DEEEEEEDDEDCE
          3F3495F4C0F9E7FCCEF99CF3FB1E8265596C35EE24EE4F4B891117B94BCD52CA
          26A4EBBB88B2B7ADD054B50D538E3AB115703D4E1673E382538D904FF2FE5DAF
          6E161567940C656D0B3C4BF578101B7FE82A213A0A3777051627F460A9712C2C
          0A17033543EEDB028F12DD0A12722A6E8A8CC31049C4204D262C89E418A8CE1E
          8DCEE9F2DE1648884B4ABF7F51F65816748C100B7920191E9667C7F1E4B95E9F
          5B567F7E13901EED9B162C3F58C4D0AC54C0E7D13D96991F01F2A8A0A4301727
          5A208580A03138B5006D47DD07673742FD2DBFD7B40AA4AA0EC784F9C96B6C14
          B32E306968104C220F986D56984912CAB860F4FE1A46455DB98E6F62B5AB4061
          B2F28B78A7976AF7DE5D08F0F34467473FE666E621141170D64680B4DB606129
          5020E0230E4473DF77B47D6A2D5D051E26FBBC399D792F718F70615D608DAFF3
          31AD0E07C532A03884E408BB1D986E372317F36B19FC2F305D5323C4677D39C0
          CEDDCDC2D1DFCF4913A60C0B28902CAF01FC90E2DBC5A706F3124385EB02CBAC
          754144FC340402068EAD0CCD60606C72347CFCA44E1BD96F580178AA8ACBE722
          FC0AAFDD524A5A1A2A613152208D56F847A6A0BF6D0E25E54D3A9E70496B6CC8
          306FFC6A421E5BAA0EF595BDB8A4891375B33C8C1901AB091CC015FF002A2FA0
          ED631FBEB6E84B0DB5D9DA4D40F7DD109A52A4F25FF2D5F8BD631F7792056D01
          282B0BD2C2754CDBA196BCC709CB53786BAB884DC04851D454BB51E959694D81
          EBF170D818AE4F2BC03D6665DA7A47E03F5B018DA2199E57DE6D06FACBD22307
          4766A3F37A12C298038A7857A9B363198EBFB2DB694C741A46B38E7CD69D0934
          19BCD35E956F04FE02AF7A54F136CCCBE90000000049454E44AE426082}
        Name = 'PngImage9'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000003084944415478DAA5D36B4893611407F0FFB3779B5ACEE9
          A64BB3655EA2344DB4CC72599FC46E921566526A4AA44650242295D29D34A320
          21B3B228141704155D30B5A28B16A5A3B432BB985463DE66DB9C7BE7FB6E7B9A
          0B8202EB43CFB773E0FC383CE71C4229C5FF3CF227B02DCEB734544E8B19C631
          99B3538C70024B8F913959D73E58F24F206F9EBC383A901E154D22C4E18C399E
          C2CC3A3060A6F4BD4174E876C7D0BEBF02552BBDA9BB84C1A0C90E3B19CF08E0
          200C0CA30EE84C76536DBB41FA57E0E812098DC9D881A8195EF8A6D5A1AEB616
          42A11063C403BCC5C02F4E5449B2CA6E8E4D089CCB0AD2675634C9B88FF760B6
          B0181A36C2CB7312A47392D05096668A4F966DE52DA16AC6330AA3430F41ACDA
          ECDF80531B239E64EE2C51D90422780A6DE33ECC36064262C3B33B073EC724AC
          0EF6F04B8434381686CFAD7877F77CFF2F60CACA3A9197B1A5E660CCF3CCB59B
          F341DDE5AE3CE52D78D6588EA815A90E2FEFF982E18FDD90FA86422C51A0EBFE
          65AB0BF05B51EF461DF65C77F89FBE7B6E115E9FC900C54F58E6338AB8E52A48
          439231A65383D5130CBCB380B7727A2B6B4E20F2656AC2D991ED230EBCD8503D
          0FEDF080C600B0A380D2F80005018D9086AD86555B0581D806DE1C04FDA3D6FE
          61B370596C61F34B12B1B62635800C5DCB2B2A84DE83C11B0B05CF02FE7D3790
          3FF32914916B9CC5A72110D9C08D04E36BF30BD4BC093970E2EC99FDAE2968F6
          CEB24D8B4F65AE0CAAD0169002C6D9F954FD4DA4FB3D46C4D255E0FA2F81301C
          ACC669E87BA8814C39179F1EDDE2171EEF12BB805725B31DCAC40C52705589C8
          B47418BA6E2037AC1336A2805CDE021F85D859AC84B6A51BC73449A8DCA0C787
          A67A1A57FE56E0023A2F6DDFCD75351CACECCBC16BBA4858147E18EBB75D4077
          7D21B4BDAD709B1E0D93F34F2ADAD6618E778F2577EA2D4614B6B42232ABB2F4
          D722F5AA0B24173A02C597DF461FC9F1DD93B77B4B9E738BED78D9741DEF3F7D
          41B56EEB779D38BC6D559066D32E553F3B3DBD6A64C26B4C4B94B1B1210AE182
          F06050D1582B3B30989352DED933D139FF00B0986008DCC8D9BB000000004945
          4E44AE426082}
        Name = 'PngImage10'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002214944415478DAA593CD4F134118C6DF6DE90AA585600B
          18214B94AE440D462E3440101363A28198F811BFF838E83FE0454FDE3CEA857F
          C0838989040209099C211E442B36D54842EB029A286CE952A44BBBCEEECCF0EE
          D2464C4B94F8269399FDF83DCFEEF3CE089C73F89F12F60A68D1F32F38E383F6
          3DCE1800C3D9DA5D3B83E6D7947D6BBC146D2A1248CDF7D0EA63FD2E1401A0F6
          8BF4F7B0286453113036133AC2FEA22F40F889E8971F571C6E07A28D03372D60
          A6099C98E0095E0323BD8002518A7059C95F588F74FFAA6EBA29326319ACCC27
          0477053C812B40F41FB09D9C074AB642526F4C2912587FD77DC353298D7AEBCE
          81991A0786AE36ECF6B601B86A405F8BA0C8F75EA92F365D32C4E45C97EE3B7A
          B1D2EDA942812984D19D10106BAFC3B6FA1E721B0B0FA5BE8FCF4A76417DD319
          768BC1397FE36520A949C79DA33BCB19201EB90A5AFC25C7D449A103CC09D719
          3FE541A55E587BDDA17AEBBBEA449F947F6039890346C32D042CFBDACC778381
          9EFC00B97422CB4C7AA6E5DE8A22ACCE863751B97C4F8F9DB9E6F8AD43C6D208
          08620306D98E21C66C1083CC3D978794FB253752A1BE4EB6CE54355CE8291303
          8E7B66157348C7B750FC843CB4A4EEBB130BB532719A07E43B1820BA6A8BE89A
          1D0E0D7C79F0D7AD9C87A3E5FED059CE70E769F10D743D85B00AFBD41F0208FB
          80BB32760ED4244F4377138F0E749896C74E7EC6360551A01961FDC0A75179D5
          126EBEBDF8F65FC042ED00F1D977F04CCE67F20000000049454E44AE426082}
        Name = 'PngImage11'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000023B4944415478DAA5937B4853511CC7BFD756AC442AD992
          CC3F0A8A227B09E5BBC0C2A2081A2608825090414145DA06BB8141C522102288
          082CF336616A160E02C11EA20889FE61B1F92AB3A8906C2BE74298DDF3E8DC7B
          A717C3C0F0C0E1FB3BAFCFEFF73BE77724CE3916D3240DE0ED8C540929A79CA5
          31C640194034A59ACD41849A9D82122EE659AFA76C53A60E503A2642C55949B6
          FFF17CA56164BAFAC416AB0EA86DFFC14B7256A265B44E7826C20B812A54A5AA
          A144A8989B5973E77A207B8771BB3C5DD201352FC3BC346F1512AD9605799F8A
          11381F0EE0EE991D06E05EDB775EB62F19FEDE9F0B021CDB938C0B3501DC3F97
          6100EEB47EE3270B6C3AA074EF1A34768DA3243F65F64073F7388AB3CD71381A
          43E58320948BBB0DC0AD6763FCD4013B5AFB2690A0ED489020814312036D4CC4
          4B58C41CC4DEA522CBADEB56407E34009F2BCB0054FBBFF2D30753F022104151
          A61D2D3D213884FE2B8291B128DC75413C96730DC08DA79FF9D9436BD1311881
          24CDCDD72242989A66862DD6342B3DD58ACBDE009AE57C0370B5E9133F7F2415
          AFDF4FE270861DCFDF8651B8D32C8BB63721E46C5E8DA4E5C62B0D7F9984A769
          084A653C85AA86515E71340D3D1FA2F82B00ED3A102373CB7DA37D19AEF9FA51
          EF8AA720D7BFE34EC77AF47D8C62FF761BDA8361146C332378D21DC2F16CF34E
          F4081AFBA15C8A039CCA1077176D88D7FD4CEDF3D9BF305FBBEE0B32AF2B6F89
          0EA8A81DEC222ADDF59BB2449550689D1226CA5703A9F30222BF62FE57370B1D
          D262BFF31F245748F06A3AB2700000000049454E44AE426082}
        Name = 'PngImage14'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002804944415478DAA5937F48535114C7BF4F8D2411435CB2
          32A83F06A565ADCC9559649192199A08C26060E082948C8C455B6014A9FDA184
          902298E63268FD207F1088A58935CAF40F2987569A8AC96CB9724EACCD77DFBB
          DDB7978E8260D185C3F79DF3EEFB9C73CF3B97A394E27F1627019A9EBB4A98E8
          052AC688A208410488A482F44C4198FA4D8040288B8BFD653A55A20F60EE999D
          C9D18447FD4BE64B96516F45DEA6501FA0A1FB2BCDDD138196B1469699B02C04
          3C535EE065254C596CE99D31A90CA6A6F7A8D2C7713E405D97936AF7AE465868
          4840D9173C04865B43A839152F036A9F7CA1BAFD9168EDFF161020735724CED4
          0DE2E669B50CB8D1FE999E4889F201B4FBD6E09ED581DCE4E8E50F1EF63A90B3
          DBEF3BDD1E9CABB7C17C3641065C7F6CA7F98714681F984590B4238803070A8E
          39924FD89F086131B0BD2BD82963D7AD82E9F610EE9ED7C8808AD6297A32351A
          9D832E64272AD0D237832CA67FAB60D4EE86B1D18607A6241950FE689216A429
          D133EC02C7FD7EDE1056C28257C4A4BD039F1C5D58F8E182975F84EBFB0EB45E
          2E950157EE4FD0A2F4B578353287236A059EBE71E2F036FF585435D7C345ACD8
          199B80F5912A3CB3B5A0D7F60213A3CE4A1FA0C432468B3362D0F7D18D3F0A90
          DA81A6761D8E1F4D03828390B9B908159D7A04B3EE589ADB3C3E80E9CE076AC8
          DA808171370E6E8D42B7CD89942DFE0A322EAA51A02D447A5CFE72AC6DB01AA5
          B557E5BB6030BFA3C6EC8DBFE67E69F6E9F25DD057AA712CFD00084418531B51
          DE9187D0E095FE0A8A1B86AD8417B62F0A62184F04482610918DAF04E2E19EB7
          2042D185A4F864A8946A8C4C0FE0E55B2BA6C6E7E41E04B23485CA6B4C0A9885
          339B6756F3BA7AFAC24F6D84699C023FE9B60000000049454E44AE426082}
        Name = 'PngImage15'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002874944415478DAA5937F48934118C7BFAFAEB44628634B
          2BC30A8448FB6194A6656292F60B121104412AC8A22223C5C8210A11F6874204
          9185A533835646A814923F3073A1B84244532BDDC29AB99C6E4DCCE97BEF7BDD
          FBBEE928088C0E8EEF3DCFDD7D9EE7EE9EE328A5F89FC64980EA57AE42265902
          154344518420024452411A5310A6DE2E402094F94573716658940CA86A738EA7
          45AFD0FE4BE422E3D06CE9F18DFE32A0A27582A6C704A0D6626091098B42C033
          E5055E51C294F9E6E7F2638BA1AF7E8F1B59E19C0C286F71D08CDD8150FBAB16
          157DDA439057D98F5B67B62880DB8DDF68E65E0DEACC938B021CDDA9C185F25E
          DC3D1FA9006E368CD113095A199011B7128F4C76A4EF095AD8F0A4D38EB45D5E
          DBE1F620F75E1FAA2EEE5000D79F8DD293893A34743BE123ADF0E1C081826386
          6413F6122AE6035BBB849D72D39AE5D0DFEFC7C34BD10AA0B4EE0B3D951484E6
          5E1752A374A8ED1A470AD3BF653034EA46BEA10F35FA580570EDE9083D9BBC0A
          6D032E70DCEFE755B114A66745A8CD06F87556421CB380D306A323F0300AEE94
          29802B8F3FD1EC43ABD1F1F13B0E46EAD0D4E3C0FEADDEB2682D2B81D65A8FB0
          F814F86D08C74C4F23DEB5B7C039D89F2D030A8D169A7324045DC36EFC918074
          1DE0AF4621FED8392C1B7E09D85E03010170A842F1B6D1649501FA071F685ECA
          3A745BDDD8B7598BD63E071222BC1934272E45628911DCF6D4059FAB28186FDA
          27A80CC8AB1AA4F9A9EB7FD5FD7CEDD385BFF039370231C949508F3CC7EC8C1D
          3F1860CAED8B8161D864404EC58089F0C2B6394154F34480D40522B2F295403C
          62EC3588F3BC40A8668E5DAA0D53E30416BB8A128F58C02DF63B9BD2D75E9E9E
          FC7ADA57E042055F6A63BBCA0E3491E29F738A71A00824EE120000000049454E
          44AE426082}
        Name = 'PngImage16'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002CF4944415478DAA5936B48935118C7FFEFBC6478D95237
          49E765566494A67E50D214224A3223134B10A58C14F2431F3425FDA050B23084
          08CC84C25A0A6A4528045233C5D4E52550312F4B9B9679996E3AE76573BEEF7B
          7AF7AE9282A2E8C0E17F2EFC7FCF39CF790E4508C1FF34CA0AA87E6328E22493
          21AC946559302C405B95B18E09684EB73A038626DC3ADB2B4FDF13C103146D4B
          0BC991AE9EFF12B9B86E7CA3EC4290130FA86AD59394434234681E7191692E0A
          8D4D4E37994D9BD29C726BDFF70AA2E428AC56E34EE67E8A07DC7FAD23A9D122
          383BD9FF55F435338DBC87C3A8B81C620354BE9A27E9B1EE68EC5DFCA3D1DDD2
          0ED9DC53D8B904634DD706CA3C7D9E079437CD918C239E3C20354682FA0E2D52
          0E7BFD303EEBD222DEBF0BC61115B68B63209485C330A1C2E8CB075A1E70FBC5
          0CB974548CA6BE2508AC0E01050A041437B1CEDD4CCD08158FC1C52D0C8BE36A
          083D77C1D155829196C7661E50D6F885641DF742F3A00149116234F42C209153
          6B3373475DD736431418878DD93A98F414E647D7615C1780DAD0EDE501379F7F
          26D9713BD1366200456DDDD9C5D48950513784BB4FC33C7D0F0247EE4556FDA1
          EB1E46D96C1AEE1624DB9278FDC924B912EF8DB763CB3811268672408760C74E
          2EDD2A480E9CE1CC151038D0B0ACC8A0EF1C8025B218F266168ADC481BA0A84E
          437212A4E8F96884F500EC8C12BE823E04C526C0A25580B2B3C0BC2CC57C7B3F
          34216508F093E246ED106AF2A36C80C29A0F242F31007D1346488D4AD0CB83A0
          29093C3C3AB143E2C8997D6178A7866F72391C843E504F2D435E3F04C5D56F80
          3CC528294892F175DF5A7912E7B2ABA0AECDC5F4A40ADBFC0E825D65218EBB05
          4A14F0233F25B5EFD9EAFC683B1E905335D2416F32A1168675F6EE4FC3B58B59
          DCFB31E8573660FCD3145AA525D00B7C7E2A2AC38AB9B1A5F45822F5EB773E1B
          E36E0A0F94D847EC9381386CA84CF30B19A74A0735BFABCEAF48937D08DD0BD1
          F90000000049454E44AE426082}
        Name = 'PngImage17'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002C74944415478DA95536D489351147EDE39CC3235C7A628
          FEA82088D45233B332C144CB1012112445E84383040B2B038B8A7EE41F2D08C9
          E8C3FC045332AC589A496A889642F37B5BBA6CCEA8B9A5733A7DB7BDEF7BBB73
          99240676E0F2DC73EEBDCF39E7DEE73295EDD3D70941164F84004110C00B00E7
          40DE3127E0282E0F1E3C47685CE829C8D816016A4C79DB942165AF8714FF6137
          9E8E5A8B4E6C775B24287D6724A9FBBCD0F0A59C66E668160E768A76DEEE448E
          228D2DADE5EF2FC0952A35EE6605328B040F5B0C24EDC026B8BB89D794DDC272
          C82B1B46C9D99D4E82FB6FF424235A82173D536B2238B64782F38F06F03827D4
          4950FCFA3B3919235D24483BE883DA0E3D52A37CFF1C78F6418F94C865DF6866
          71B1741015B9E14E823BAFBE91CC58191A15D310392222060C0818EA387C8EBE
          8498C64004F8FF78025F571DEA545B91979BE724286CD09133F1BE6819302139
          4286866E039228AEAC60BAB70282ED13BC76A463FCFD3D98149D096157479A98
          827A2DC93EEC8776A5090CF377BF625A82C52A40629023D8B30DDE2171B04C68
          C0CD0A9819E99F57F5F46532376BC7C8B9A3FEE81A994142A80C6FFB8C88DBB5
          2C8BE65E03028D4590ED0E036187685F0160F51370F51641276F9D61AED568C8
          85C400746BCC585180E33AC052E54927CA1014442072D1D2BB590FC21B01FB3C
          94D5BA3926BF4A4DF29236433166C6A160295A078D88097256C04E0E61ACB318
          126F820D7E0BB425DAA6C80382DD06558D9A1D54994F3197CA95243F79CB6FDD
          2F699FEA7DBC118CF625FC23A3406CCD60F859289BAD7061D761C1CA634CAD49
          395EF9B59EC92D1DEEE0EC7C888D17DCED1C0FC7E0390139FE25088D8D81CD78
          1F62B117FA5BACA8D6C663D2331C73B3F372796152E2E23312FA1557B389E747
          3EFA44454770A62E289B7ECEF52A26B34F978D56ADDCF74F82EEDBE1B7364ABC
          33E7A72C0BEACF86CBE90F46EA56DBF70B1C24721A65B8F1E10000000049454E
          44AE426082}
        Name = 'PngImage18'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002574944415478DAA5924D68134114C75FD6AC06636B8C4D
          97F8018A58A48545845A5BB558041541D45214822D2DA8072F82D243EAC18BD4
          4BC18B07A134B6C65A2D628D2845A94A345A312B1512FA01C58348509B346B62
          924D7666D6994D0C8D49C9C1816576D8E1F77EEFFFD6A0691AFCCF323080FB8D
          7C8E6E27B146AA0921904C2348AB181026B90703461A6042A67BDBB777140186
          BCD189B6860A4BB96A57EFCF67FA3A773415015CAF23D2E9C6B53016FC281065
          931561234FAB416A89C94ACE04B16406088630FDF6999A1CCD03FA5F8625C75E
          0B3C0F246A8F886663A9EA2CAA8482C16C5A11CE996CC9036EBDF829B5375B61
          74F29778A2BE129ECD4E7124B599C37895A14C267E1D7073FCBBD4D5520523EF
          64D1B1CF024F3EFDE68FED5A537602D424AD036E3C0D49670FDAE09E2F2A9E6A
          5C0763FE18CF3279FC651010411053E290CCA440C5483F23BA3B9B7AA1C73D97
          6DA1CFF34D3A7F4880BBDE88D8494D867D32CF32319B8CCB564F2808BA6F4F67
          01D71F7D952E1CB6D3712E88679AAB60F4BDCCB34C3CFEC56501C7EBAD70B13F
          9005740FCD4ACED6AD30FC3622761CB081DB1BE159260CE0D85F0D0F3FFC80B6
          3D4201201C53E0F240300BB8E49A19402AAE11D6573474B508F060729167998C
          4F458163B7B9ECEF6EE40CFA3C79DA59EDC6D5D07327D7C2DF454D42CCC4F56A
          C1CE329908C8D0BADB56D2603E1403E760B010404D3CD4A4CE6EABDCC632F1CE
          C860A045D32AC9DF31D2333BD56D30C11577A010B0C4446326986874641A2452
          AAFEFEEFBA3612242501D4C4474D76663031AB08433C91A620B5E89E1C573C7F
          00C06D6C79705B8DEE0000000049454E44AE426082}
        Name = 'PngImage19'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002D24944415478DA95526B485361187E3E132F7969CA4CAA
          15F64314CD6A79995949661996A18822084246930A34ACFC6150119114566042
          91267981D42E600886252EC16A696B85A699AD956E86CE9C2DBC6DE79CAFEF9C
          651614D80B2FCFF7BDE77B9FF7721E52DB613D4D29D43C151482208017004E44
          5E3C53700C179C07CF511617BA4B728263C08C543F99B064A87CE4F80F3BD3F0
          61EED2FE500F89A0AA7D9C666D5E86A68FD5AC32C7AA70703074F00E27720C59
          6CFE5B715C094ED60DA04C1D4E24828A360BCDDE22839787EBA2AA4FCD7228BA
          D5876B87D73B09AEB78ED29C787F3CE89E5814416AB43F8E56F6E066BED24950
          DEF285E626C82582EC6DCBD1D8398AACAD81BF12EE69479111BB701FB7CDE278
          552F6A0AA39C04579ACDF46062001EEAAD7011232E0404141D7A0B7CDC81693B
          073F6F378C8CCD80B007DE4B9740A00456DB0CDAF52615296D1AA679498168EB
          99447A4C009ABA2C4863A8BEACC3DE8410A46DF246B3D6847DB18A3F46A9D158
          A0E9328294DCFF4C8FEC5E818EFE4910B2F0A0A1D580C4B8501CD82E4341B90E
          57F323A538DB21EC3C50D162C6DBC16190B38D465AB067259E0F7E43B232008F
          DF8C63D70639722EBCC04E5508B2E37D210EC52663830102139758E8E2DD4F78
          D5CF084ED51BE8B11405BA0C36FCD600EEB41911151E04F3D7EF52B2A84481B9
          48429974C583796404A4B86E8016A505416FB46147841C9ADE7124AC9323B7B4
          0BCAB0D56C599C98028EE3214A5DEC40A04CD64C602613233851DD4F8BD3D7FE
          D4FDBCF6298A6EBC4662740832E23D990AEDE2EF91AA5222C0C3D50DA5B787D0
          DDFB1EA4B0AAAF9373F01BEDBCE0E5605544E73901D353734856452035DE81CA
          EE3CC8BDD6889B80656A1879316528AB9F8346F712449AE72F9679EE1955A784
          21493901ADB51132B755521393B36388F5CBC4F95A1EAD5ADDBF09220F3D7A1A
          ACF08C93B9FBB1967DD9180E10B60342D92EC834DE0D19306673E0072BD575A4
          9F7968B30000000049454E44AE426082}
        Name = 'PngImage20'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002B84944415478DAA5936B48544114C7FFE3DDF5BD6A3EA8
          CD24418C7C25A662A6865F3252237B486CA95964BE2885081314C1328D12A1D2
          0CC942D6520A2D43D22CBF6451A219BA6594A490A1E86ABBBAEEEADDBB771A57
          B0A0B4A0339C0F73E6F0E33FE74128A5F81F237F026485BA167AB9D03C8E13ED
          7813C52C6FA1FFA2E52AEA7B270BFE0A480F76C90B74A7A5525B424476E78D14
          3A8388091DA59F34D2F3ADFDEAA2550137E29CA8B58CC3E48C0926B218B18048
          3868E6448CCD986694BD1AC75501A53B6434489183004F078C7E1B43BD520989
          4482056203A35E638C8C8A90A5943D5E58115093B2712AF97287333FF41C3ABD
          01EA692D1CEC6DE1E8B7136D658933FEDB76B905A456F12B02AE1EF1ED4ACE2D
          88102CA4B097088B29D0091C2444C0FDEB177B4FD60E84ACF885B571F55207ED
          CB5BC541DDC9FB533340AD5DCC716AD4E389B20245DD214DE356C149931D6986
          DF006EB1F7ACA8683A6E8D7555ED35E150552B40B1F446D8599F74178A9C6E18
          84E15C02638DBA3343BF0C70DDDD4878138EAEB174BFDD763318BDB0C15B0D60
          98636D64690273F95C1378750B6BE97768E766E7B5BAE9D2B64BEF8ACD00BF03
          B50972A26E4E3F7B0653361CDEEB298C4CA4C04AC5CF03DC44033C9D9E22D837
          041ECEDEE8543DC46BD50B8C0CA9CBCD80BE82CDC286B004AE7132023DF23DE0
          987251A06680C01A26F91A8783F1318C6481BD3EA771E5591A38361F0DCD2DF3
          66407FA18FE811A520990F3CE09F7808835A5B500630F14B2A1686B722EB7036
          62FD4E2C17BC65A01225D517966AA0AA3B95CF0FB6155F1B3F06150D97F82BA2
          417E69D5C7562FEC8B8F860011F9317750DA9E0A6BCEEAA782451B69C894D5F6
          BB5BD67D082CA19AA9384E6AEB0CC28641A4B07379443685F648B66F8984B73C
          089FC7FAF0AABF0BA3C3DA72F2AFEB1C962D2F5B5C54E632E6B3CCABDE548E9D
          FB01011A4D9CE12FC5C40000000049454E44AE426082}
        Name = 'PngImage21'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000000D04944415478DA6364A01030D2D40069DB2A96A787DBFE90
          650050332790EA03E2634043169364004CF367592D77F60F2F3FB37D7EDB83CB
          10465C9A3F299B787D96D39163FAFEF9A7F095FD37D9BEBCC36A082356CDEA56
          5E9F150CE518FEFE6260F8F39381E9DBC79FC2D70E010D798F61082386662D07
          AFCFCAE672208D0C7F7E30C068A66F1F7E0ADF387E93EDEB071443900D28FA21
          A698F4D62E519BE1F77788C6DF3F20864069D64FAF3E8B5C3F7683E9DFBF44A0
          2157A9EB028AC3802AB1409574409594886408F979815800000BCAB51102FAB3
          960000000049454E44AE426082}
        Name = 'PngImage23'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000000DC4944415478DA6364A01030D2D6803A3E5686A64FBFC933
          A08E8F0B484E01E2234043E691660054B3909880DBB72FDFBEFDF8F6AB039721
          8CB8348BCA897A8A48084BFCFAF6EBF7D3BB4F1EFCF88EDD10466C9A2554C53D
          C5644525FEFEFCC7F0EFE75F869F5F7FFE7E7AFFD9839F3F300D6144D72CA525
          E929A12221F1EFC75F863F400CA68186FCFAF2F3F7F327CF1FFCFCF91BC51064
          03CAF9C47852355C3495FF7EFFCBF0F7C71F0608FD0F4803D94083BE7FFEFEED
          D9F317F7FEFFFF1F0534E432955D407118502516A8920ED00C61202B25220CA1
          202F100900A0BDC31179096B110000000049454E44AE426082}
        Name = 'PngImage24'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000000B64944415478DA6364A01030D2D4803A0606962606863F64
          1900D4CC09A4FA80F818D090C5241900D3ACC5C0E0FE9281E1F35B06861E5C86
          30E2D26CC2C0E0A5C3C020F79981E1E77E06869BEF7018C2884DB32550B32150
          F36F20E727107F0252078186BCC7620823BA667BA0665324CD3F910C390234E4
          039A21C8061429303024C5313068FF00F27F2169FE01A55F03C30368C88D7F0C
          0C894043AE52D7051487015562812AE900DD10067252229221E4E70562010091
          D36011DDA2E8350000000049454E44AE426082}
        Name = 'PngImage26'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000016E4944415478DA63FCFFFF3F03258071701B707B9A2E13
          03334B3BBB90A8D3CFD74F67A8665D9D4B920137276A168858BAF5F36B4533BC
          39D6FBEED9E973D146D5B7771065C0AD697A6982DAFAD3050DDC98BE3EB9CBF0
          E7F33F868FB72F7DBB71FA628AD7A4FBCB091A706FA1F57A19BFBC80FF3FAE32
          3030C930FC78F984814D9089E1F1D6FD1FD5320E0A1034E0F64C933629D7D04A
          26E6870C8C4C9C0CFFFFBE6160F8FD8DE1FA92C75F8C1A4FF0E234E0D6545D13
          0E51C9692C7C5C22BC4A7C8A2C4C1F8006F032FCFBFD8BE1C6F29B3FAEDCF894
          14B7E201762FDC9E6110C3CCC65524E91A6FF8FFD72E06C6BF9F19AE6DFBF895
          F9F3BF2FCC8C8C0C576EBDCA8E5CF4602DCE407CB4DAE3BCB84DA4C1AF37D319
          5858F819AEEFFDF2EDF4F117B9F23FBEAFE6E066667058F4F833DE687CB2CEE3
          A4988D9DD99F0FC719AEEF78FBE5C2F95759C9F3EF2C263A219DEA3569E51112
          4CF9F6EEEBF79BB75E9745CFBCBD8A810018E47981180000BE43C2E179B4B8A1
          0000000049454E44AE426082}
        Name = 'PngImage25'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002544944415478DA95924D68D36018C79F7C746D92A6B5DD
          86B2B5540FE2A111BC7A109908823741D4F939F5205E4451F4EC511041143C09
          DE446FE2555BBFE7C9EDD04EBC48B77569BA43ED47DABC5992373E6F628BA5DB
          612F24E125EFEFF73CF93FE17CDF87ED2C7BEEAA1C7DF1BCD7DF7343823BF772
          788FC3C307E54DE14B57F2BEEB1CA58EBB20BF7EF96958C06041A8B0BD6FDB1A
          FFF8D190C4BE78398FEF4B5C740C36EAEBAF50745F7DFBE66728F80743340A4C
          E7773A4009D1C4674F0309B93097E778BE242454109349B057AB400CE326759C
          27A1E0F6DD984F69172489E7541568BB0DB4D94489A561BBC0601EE148220964
          AD0A76CDF0109652C577CEE013E88D5B31DFF34C4E92041E0F7AAD26B87F1AE0
          A3404069249900525D63953D6C3F9E2ABE272321BAD7AEC7A8EB99BC2C0B626A
          078047D1CC2E0F2B33B81EC21F0A64F3298461A1C4ED4AB9DD3CAC2CA3C403C8
          4C4373619152D751D21F8BE4FFF323026BF63C06C69562D319804A25EC0005AD
          A525703AA63631FFA5BCA5C03A732E0F088BB28281A908FB41FB4C42EA75B00C
          03363A1D6DE7E28FF288A077FA6C5039841360D57406B1B4B9582ACD2B53BBA0
          5733A0A7EB2831B5A95F4BE581A0776A3698B328CB08C7C1D20D20EB0C76E318
          1AE0D394C6D3829ACD4057AF411703659D642ABFCB81C03C7172868F888588A2
          806B76C14218C7171FFFF63908CCD87F0047EC9ACAE4A4C0CE986C228DC691EC
          EA7271F009AD63C751122938ED16C58A0A863594B6BE775FF09F8CA9AAD08747
          426C1C3A3C83F0FCC4F7AF43707F55737B98E460B6BA52DC728CDB5D7F01E142
          7068B693C6920000000049454E44AE426082}
        Name = 'PngImage27'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002AE4944415478DAA5935D48536118C7FF5BBAC9A644D6C6
          90962453D6082B08E94262739B17428224D83053FAC09B080ABC89A04BEF8282
          2E7621445E8C85898822B37DA099A2F4013186F625B3896367CD0D37B79DCFDE
          F740C383D14D0F3C3CEF39E7FDFFCEF33CEFFBA82449C2FF98EA2020180C6AC9
          7337718F288A56129B8993A51825F11B8963C4433D3D3DC2210011DBC8DA6734
          1A5B4D2613F47A3D743A1DE8F75C2E87BDBD3DC4E371300CB344DE5DEBEDEDDD
          5600E6E7E72356ABD56E369BA152A9FE9A2ECFF38846A3D8D8D878D9D7D737A8
          0004028194DD6E3768349A7F0276777769B68CC7E3312A007373732987C361C8
          66B3A8A9A98156AB457575B55CC2FEFE3E32990C1289042C160BC2E130D3DFDF
          AF04CCCECEA63A3A3A0C854241AEB7542AC962FA57DA8372B98CAAAA2AB4B4B4
          20140A310303034AC0CCCC8C0CA01BA9980AA980E338A4D369BC88DD43912BE0
          A1F33516C28BCCD0D09012303D3D9D723A9D069A6EB1589401B41F14403A8FB1
          D85D341CB720B6FD112ECDFDF49D9BC30605606A6A2AE572B90CF97C5EAEF9E9
          CA202491032BF26009ACA1BE19ADE676AC6D86F139B18A22CFD6BD1DC9E42B80
          C9C9C994DBED969B4801CFD66EC07D76108224421005889090CC2670ACD68895
          EF417CD85A144B3C5B5F014C4C4CC800DA6D0A78B2761D5DE76E219EFE0A8E64
          C1936C3881835AAD81B1EE24967E04B0BAF94EA800FC7E7FAAADADCD400CC964
          128F2357D07D7E980805F0822067B293DB427DAD099F7E2EE3CD7A8061797456
          003E9FAF8BDCF357369B4DDFD8D888DBFE8B28B12CE9018B12CFA1E9C4195C6A
          EAC4FBAD6544BE847F9578E1F2CEA814530CD3F8F8F869027944FCAA5AAD3E4A
          67819E062DCB9B79205D38D5AE5AF812D9298B928388370E4DE341F37ABD7544
          6C212E92A35C7F9E1BC908E2115D51149A985169F3CFBEDF40BCC217C87334FE
          0000000049454E44AE426082}
        Name = 'PngImage28'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002E84944415478DA7D935F4853511CC77F57C5B94D1D0BEE
          B274D89FC5C640A320DF82F96708429124D19FC79E32082209FA43D443F4103D
          840F62D8832EB6D190B536D7E6265A9651300859BAB9B99C16CEDD75756E6EBB
          DE3FEBDC03599AF4831FBF73EF39E7F3FBFECEF91DA2582C82DFEF97A0781AF9
          05411074281E418E864210C5288ACF918F757676F2B0C3089FCFA7478B2C2A95
          AAB1A6A606E47239C8643210C1E9741A32990CC4E371A028EA3DFA77BEABABEB
          C73680D7EB1DD7E97406B55A0D0441C06EC6711C048341B0AD98E2345087FBDA
          87B694101E8F27693018C8F2F2F2FF02565757E1D1E77BDCBA90FE3070CA6CD8
          02B8DDEE64737333190E87A1B2B212BB52A9845826026FE20E58589F47001E38
          96875A791D8645129151CBC557ED18E072B9922D2D2D642A9512EB845C2E07EB
          9255083053B04F510BAA8ABDC017791090F3028FC7815800E696E6E65F5F19D5
          104EA7130318868142A18033F86937148A1BB05F5607D32B5F20969A07769385
          BA6A359E0F2DCEF95D577D46ACC0E170245B5B5B4931733E9FC70B06BEF7029D
          4B61D94A81848EEA33A0D168E0B6E70647333F3FB20C674040C9F8ADA93C61B7
          DB936D6D6D64369BC5F24580442201966521914840269B81B2D232D06AB5F0C0
          798759CC2FC87D3D937F6E6178783869341AC9B5B5350CE0791E0344D0F2F232
          EE8331DA03D70D3761646484EAEEEE566DEB039BCD8601344D6300EA38904AA5
          906772402553608E0E426E73031E763C06542E65A5864248FE49769343257271
          C26AB5269B9A9A486458B2780E22402CA1F7D31300A980C733D110889B0EA90F
          C089E3C760261486779353CF08B3D9DC81B2BED4EBF5F2FAFA7A2C5F6CA8CB2F
          2E81A24A0147B50D5BD7F7FB3AA3B16FE8FD4C04788EEF21C49E37994C0711E4
          2EF2B32525250AF12D8867608FDBA0822CC38DF435340B1C96CD8BDF6FD1E6FB
          0B434B1318F0B7F5F7F757A1CD1AE402921EB2AC0C7A91F43DD34F671B776BF3
          7F003BADE19AAE1465B5CCF645CEED36FF0B846DD8A9AC4A7566000000004945
          4E44AE426082}
        Name = 'PngImage29'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002BE4944415478DA95925D48936114C7CFEBF6BABDBAF955
          D9D2B28BAC0B6FBA2BB4A0C082BCA9EBBA5B141244444C0B8D22A2A20FE8A60B
          E922ECC284104B41FC0031359C41E1C7D0A6E698733A37A76E7BF7BECFFB7C6D
          3D0EEA223FA0037FCED5F99DC3FFFCA5743A0DBBD5C791B899705642082F4506
          DBA763724CE8A98648BFAAE34BD26E80D6AF31450CF6334E2B09E5401903CE08
          28962C084454980FA93DBB02DEF547EAB34DE9E73645024C58460853886B180C
          D18726837447C0DB9E50B68659FC40816C45184334618861029AC1C0AE98C0EB
          8FC2C2E246DDAE173C6A9D7D62314383CD22412A95027F58051D195060936172
          3A0C6B2BB1AD1EBCFFB256C67D1D0F28A5A6DADA5AE7F5D7A3F71388363A0A2D
          56BB350BE697635064B780676211D448A2EA2FA065785D11467DA0F160CD21EB
          8A459665181F1F6F76B95CCE73F55DA719C27D078BED4AAED88E2905CF880F0C
          553F9C0188AD66B171DA224B4721CD21E41B63E585D83C3535D58C10BA26943F
          BA7EA2821A469FA3344F51D7348804A273A914A9C8009A7A426594F185922241
          2714C2310302F35E12A5F6F37BC39F9DBAAE570AC8D989E8C97286492F4FB13E
          CEE995B9A1069401BC6A5FA804E0238E021996D674C8B54AB01CD5C137E3A1A7
          4A6372717131B4B5B5CD608CAB66E255B699C17B813F9E65000F5BBC174D1274
          EC1780E06A1274F1E3FC1C33243403829E41385E9A02B7DBDDDCD9D9E9FCF753
          19C0EDA631974DC97AB947B8EB0F27001958400858CD26913C0673DFDC941192
          3FDAF5186D01DC78F3235BFC3679A4C4266FA60B0410539649DAA6DBFE5F1108
          FBA37727BB6FBED82E2BD2E567C3242F274B761458219644B0B8140775430394
          34C048624009CDCD39AFFE3970076D0BA869ECED2ECA952F104420185CA75433
          1A3861B3DC20ABE2EC25E1F8B277A08EED9456E9CCAD4F0A43B45D04A59A2162
          FBDE7695C07FD46FB5CDC52402304EE90000000049454E44AE426082}
        Name = 'PngImage30'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000029C4944415478DABD935B48544118C7FFC775D71B89CA22
          7431D4F66C5E365359052B377DC8400DA482245FA214A2282B0A04ED21447C88
          3229E8265A4AA8ADF914AD0A126E9B17D6CC44EB210AD7CBBA9AAD7B3F7BF69C
          B3A7D9F3602F450F4503C3C0CCF7FDBEF9FEFF194A1445FCCDA0FE39A0B1A56D
          21252539C962B1A0BEF60C75F749BFE8726E607A72FCB3BEEBB1FA8F80BAC6DB
          0BF9797949DDCF3A4112A8B3571A4475BA065E8F67F9FAE5EA1DBF059C3E7F55
          4CD364C3ED72222A2A1AFDDD1D981C33524525E56246562E148A08985E0F86D6
          1EEBE27C8DCBE5F0D9D7BF05A90EFD40E4CC87A9A2B878A541260BC3FADA2A3E
          CD4E83E70208019D0E3B38851234AD02E3B663D66C8469D850CFF15C33EBF78B
          544BDBF37032BEC46E89D9699E18976E13BB2D0D5B13E3101004CEED66E5BC6F
          035FE797C0FBEC50AB55686EB86411787E17B9BD4035B5B6CB453118884F5062
          6E660A822090EA1C189F17E6312352E934A8766780E779693F1060D1F5A835D4
          77040170544D6D9DDCB6BCE859B3591509CA44ACAE2CC3CFF8E0723AAC8CCF53
          26574434916AA5D1D131F07A3D60FD0C3C6ED7064956921994442C3F7AF2CDBE
          A24307B439597038DDB02C59D1FBF4412F11B1924ED7F41CAFAA3EA1D5E642E0
          83585AB1E1C6B57326A24DE1A60B25E5C7F8744DB6ACACAC147D2FFA48153FDE
          9B473FCEBC9BC84C4EA5E72A2A4F6514EA0A31607885B030193A1FDE11488BE1
          9B80E2C347C4AC9C7C8400FA3EBD143432F432C0B2FE4886F17DAFB958179FBD
          778F0490C9C2D17EEF66480B6A13B0BFB844CC2B38089D4E87A1A14112248371
          D8402CB5C59137F1B6AAFA42264DD3183519A5B3FBB71A110C067F02B4053A31
          A43047BC0FA92CA9CDB26059663B17E046C8BE4A1078C92122A86435C9A37EF9
          94FFFB6FFC012DD473F0AA3A62EA0000000049454E44AE426082}
        Name = 'PngImage31'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002E34944415478DAA5935B48146114C7FFAED7D56A4365F3
          B299E86A1B6A8961A622A2545A861A12A690964420E18BF8D08B8AD255892033
          AA072D25AD70C90AA9104BD3D412CD5A2F99EB566AEEA2E625DD9DDD9D9D6FFA
          760C1F325FEACCC3F9E683F39BFF9CF33F363CCFE37FC2E64F4087DAEC4F783E
          93103E8B660F420838C2EB68AEA1B9EE70A8646C5DC0AB11268D1656F8BAD979
          DAF01C388E13EE452211CCF4383869D0721CC94B8F922AD7009A07F5691C2177
          151EF62E8BCB0C86C6F518FFA1B77E1D52890382659BE0BAD9096F8617F41492
          7D6ABF8F7215D0F4E1A71F95D81EE4EDE8A59B37A05935034F893D0E857981A7
          4F8F7A1EBD9A39C42ADC2173774173FFF41485C4E4A7066A0480F2DD6C91C24B
          5C6232B168EC9982C9C8A23823C8CAFF2D9447F9A321982D048961DEB08A6EFB
          A82B2ECC08291500B56DDAD188808DF28EC1590C7F5FA0B209F29315E8FF3A0F
          17275BEC0D7047F7E8341A3A340894B9213EC403F75A34EA4B39BB0304C0AD17
          DF0CF121AEE2DAD62F586258B01C0B9E70480AF741E47629CC2CC1F3BE09740D
          EB2076B4C5E9C460543E1960AE9F89761600571B3F1B92C23DC5772860717989
          16F382CCB29311985F36A1FC611F8C660BACED163B88909B128AF2FBEF99EA82
          B815C0B9FA81D1D46899BC4D358DE189393A3E16D6F9F31C113AC0B116DA0622
          3454BED51D097BB6A1ACAE57FDA03071E517CE56F516C5EDF228D9E0EC8086D7
          63D0338C5078312712D44C28B8D92ABCDB81E0D8819D98996350FFF253F1D3F3
          C92B4DCCABECF4A3A6693F91A0F05AD09BF0B8530DC66846985C2A14F68C6821
          222C0E46EE80D4D519176ADF4E517FC4B45C49D3AC1A29FB724B9A851A293725
          C485A5D23B559318D7CD81D0D1796F91202A58063B3B5B945677E9E994B2DBAF
          A52BD758F9485193D58D154763033C15BEAE7074B015EE19A3052AF50CAA9EA9
          B4D4EA79DD3732957FDD056BEC2B50FA537974994816751B5D26DEEA0B1DED45
          0D3DD7F5DE3EBEFE32FD4BFC02BC68A6F0E552D6EB0000000049454E44AE4260
          82}
        Name = 'PngImage32'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002344944415478DACD935D48145114C7FFD3B6AEA6B11FCA
          DABAC4426044E14394AD4820486D21496556B010545060228954281122454811
          54508610C106AB19A1064A2CF405D183500F3E88B0A8ADECAE1FA445B2CCCECC
          9DB99D3BB34B3DF4D6430D339C3373CFFF7767CEFF8CC439C7DF1CD2BF07740F
          2DFF91C00C039A6640618C2220330E596150340659E5C8D09A2C330B10DC5AF4
          9B94939843D7ADC848A889CB048A9CA26E40A5D8FF66E917E04786819E5B2251
          A0E78A492C8A15A643258042B94ABB7B4AD6A3FFD58205A8AE2C446DE5068C4C
          7CC7E16A271CF675E245E8E4984A66F1617A0D8DBB5C18FCF815CD35A5B8FD32
          85320244C693903AA3691E2471A86A2322EF5770AAAE147DB1655C0879716B34
          8DF3FBBDF83C9741FD0E271EC516D17260134E3F88C3EBB4E3D9C83CA4B627F3
          BC765B098EEF7199CDB1DB24F48EA670E9900FDDCF93686FF0A1B0404251810D
          F7C6D238B7AF1C67FBE2F0B9EC181E22C09987095EB7BD18C7822EDC1D5BC2C5
          8672DC7891C2B5663F2E3F4DA0F3A81F338B59B88B6DF4892B683D5881F0FD69
          54B81D181F9C81D47427CE43554E84F77A70733885AE237E740D24D01B0EA0F5
          F12C7A4E6C4647640E2D211F5E4F7E43CFC900D2AB2AAE46BF20169D85547F7D
          8A37EE7423B9AA98DD155D56B47CE72DBBF26E08FBB49CBD813207DE0AC0EE2B
          93BCA9C6438524D2ADE1D14CFF7322839B229DD6E811596D203FBCEF0608B0A5
          ED13CFD24471DAD550747055E4544439548A6238C43DC140E2FFEF67FA093B34
          5E6A57699F620000000049454E44AE426082}
        Name = 'PngImage33'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000003304944415478DA55936B48145114C7FF77B69D6635DDD2
          5A7B87955498F4707A5056F662B38C88A03E14F4F8103D2D2B23B08784810641
          11F4A2A2AC5CBF0415158AD562BA5606256A9AA116D592AE8F56F735BB3B3BB7
          3B339BD1857BEFCC87F33BFF7BCEFF104A29569DAADA618A17B67204E90004B6
          09103D293BA8025989B4BB7BE94B32F03BAFE6DAFA10A28BA880AC7335D5F773
          C4191C478653C0008AC145B56F8A6E6F180FDEBBD1F93DE86F6E6A49705C5B1F
          1C04AC2B72B8CA0ECD4FF8E482812344CBCC92425633CB1481B08CE529B17857
          FF1C49AD25300C4B83AFA70A44726ED700D9C5B5FED283A2A9ADCFC062A92640
          614744511064002914C1C2B80AF8BFBC8569D4129893E7C2FDB5169F2B6E76E9
          0A8A6B03B683A2D0A102880A20FF0178F733A4080D888D9F83BEB65698474E01
          1F6741CBAB126910507A40147E0C18A275638008654FA030F4BFC644D8619E6C
          45F0571902BD04AECF7EF4FB09E582BDD335C0DA2206600A3A3D46ED01345A3C
          E2B6634CE405CC533740725E05C7CB087B27A1AFAE05677E6E0BDDC9DF3454EF
          02033C60806EAF51AF3CDBBEF6C718C7BF8165E646167C059C5146C8930C574D
          3DE2AD45D85AE2912A4F2F3269006B21537058145C3EA3F6045FFB53C478AA31
          6D6936425D77410C2148FDE3B5E0BEA5373079C2686CB9F05E7A551005AC3EEB
          08D87299021F8FCE0F3624A10932B12031D18111169E054F404F5D2BBC199720
          C78EC50C0B905DF84EAA2A5CAC03561430C01151E8F5F268BAB70A9BF7DD46AB
          ED289CDF6A3174E22C44BC14C3AC9721C7276BC69C3586C3CAFC6AC9717E990E
          C83C5913B0E589424E3541DA937938B16B37C045505FF9086D3F9CB0673E444F
          4C0A94A83B6D6B8CC8CC7B2DD55D5AAE03324E5405CA8E2F10CE37F1E82A188B
          D99312316F7A320296A9F8957A0CE1B8F15A57D4DDD025E3E22220E3905DFA78
          7DB50E58986BF795E52F3015350A84320729513321EA48D59DEA4DD9B4714CC6
          D54CA2887B2AFD8D37AD711A207DEF8B8EF2E225237D611ACBFE39B58F7FE59A
          4D430687CA64E4A07ACCC4739E993BCB9DCD77B2523580B8AF72BF1CA61B0921
          7358EE987FA3A87B82FE1D4FFD921445F9C001A50DB7B36EFD017239AC084E0A
          BBC10000000049454E44AE426082}
        Name = 'PngImage34'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002AD4944415478DA8D935F4853511CC7BF6773B6810DA5B1
          52837A93507B708E19382173D3CC27218C14A9F920CAC481BE0C1C3989724122
          A24284B407537A10F1650F054AAD0475329814D4CC8464485AA97373DEBB7B4F
          E7DEFCD39F11FD2EF7DCDF9FF3FB9CDF39F77708A5148752E17A794BA355D72B
          08AE3093B027014A2008FC89EF5FE9F09B07A576FC21E457C0D57BAFB9D1B662
          9582110EDDD2776397C3D3852D443E25F6C21FDE67BD1AAAD94F09A8F1CCC6C7
          DA8A35CBDF94522A7841C47E92C2744E85B71B14BE6004D3FEE5F1E9FB976FA6
          045CF3CCEE8DB715AB572400C111E06C068F931A25DB8A88FABE40F2454F99EA
          2F80DBEDD6CCAB2BB6C6ECC6F4CF3B1280804B8A0C42B113E7C189224ACFAB70
          FDE13C97B33292E9F57AF78E002CB989E9FD0BEA8A8C51BB11EB511544B60581
          C5389E22C60BE019C09093861B7D73C87A37B4CB719C63626262847477775BB2
          B3B39F1B0C06B8A779785B8BD8A1A920D59564AB8B0C123FA824FF344143FF22
          9C653CA6A6A6B0B4B4649500A1DADADA42BD5E8FC1C141FC8FD8ED7684C3610C
          0C0C7C9400FB5D5D5DE95220180C222F2FEF9FC96C55984C2659EFE8E8A0C4E5
          725106901D525946A3119B9B9B2993753A1DFC7E3FEAEAEA64BBB9B919C4E974
          D2C6C646D931333383F2F27244229194007656F0F97CA8AEAE3E0674767652AB
          D52A3B42A110AAAAAAB0B6B69612909B9B8BC9C949949494C8767B7B3B081BA8
          C562911DABABAB47E5271209C4623144A3516C6F6FCBAFF4CB0B0A0A50595929
          CF71381C202D2D2D82D96C5668B55A0402015CC8BF88533A7DCA0A36BEACE36E
          CF1D783C1E799BBDBDBD20369BED116B8A4B3CCF176ACC0E0C371521CE41EE03
          A9070441BA130204D6486732D5687DBC087F7F83544D884D79F6DB5DB83D3427
          A429150A1CBB0ED49F2361ED9D4C8AE213BB497918FF010A84544AA258ABE500
          00000049454E44AE426082}
        Name = 'PngImage35'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000032F4944415478DA55936B48145114C7FF33BBB3AEB9B2AE
          AD4B99A52E25159566A5668682662FB207F6C0477D08CA2F96F40EA10F113DE9
          1D694514F49228A8A0C4B2883473CB1289B4AC5C77352B3473CDB5759E9D99B5
          A52EFC3933E79EF3BB67CE99CB288A0275ADBBD59144E6843DCC387B5090A14A
          9065FC16650FF97F917A4812E927E907E90AE921F3175058E12C8E1F653AB52D
          2302FF2E2F4F30D24F9F045E00FA8724B4F70CA0F293B797B64B0280FCAB1FF7
          A5DB2DA51B52ADE8E893C0310CF40CA0D39374808125CBB2E0E8D9272AA8770E
          E282A3AB3B005873A9F9D49229B6E2D533AC68ED91299812684BAFF303588281
          62C7987528B9ECC2E182681CAD6E4700B0F27CD3D5C2E4B1F9E971E168ED16C0
          52061DEE4F562160A023883D4287AD17DF62D9DC289C79EAEE0A00569C79F568
          73765C56AC35046D3F6402C05F0581343B0C9B3A9A43C18906F838A59F5CB901
          40CEB1DAD6237989137899C3772FF918FFA92A485D3A28E0F42C6644E9917FD6
          81DFBC30F2DE96B4DE0060F1C1C743D73767185ABE4BF0D2B0583599B654801A
          71EB451625F5E3647E3D0A4FD7C3278861F777657A34C0FCBD95C17A1D3B787B
          67361C2E0132FC5046355AF7801B35E9880C8F45F39737E8EB3E2C42E10C557B
          162A1A20B3F4EE387388D1554180E7ED82D6ED3B8E2C28B2085E952422D2321E
          D3C6A6E1A5F3099A3A1D344A3EB4667BEF8006C8D87E73A2CD12DA52B17B016A
          DB454832955C97867953D64152647A97B4AABEF575C262B2E1C5E76ABC763F93
          0912CECCD974C54A15962D4B4FCCCDCB9C88EA161E2146E041E36C2C8A5F0F57
          CF47FAA54588B2004152C76B802D340AB56D5570389F4B4C4AD1C5D504B81413
          650BB6C7C4616048A140051E2C474EC2464A94204A9256C9578F1BE1A65168EC
          A8C3A3F755DDBC886C66D6FAF2434B33E2779416A4FC77075694D9E1E379EA01
          4FDF2BC06E9D84147B361ADC7578F2A1C63B24F992BE1E509A99C4B5A78B52A7
          8F2F4B9A1C8D305330C2CC465842826026858E3020882E4110CD7FE64113A646
          26E3655B13D86FB9FBDF9D2B2FD5269590775CED4109299A3472589661998D06
          8E331A39B8ADC5D40B09E6CE9D4EBD18B1AAF15A49830AF803CC786FC8223523
          300000000049454E44AE426082}
        Name = 'PngImage36'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000022E4944415478DAA5934F88127114C7BF42B090C4CE824B
          E3BAB966A70C2F06E1C52EC2166D60873C849D3C79F3E42913F2E461C5837BB0
          D53633D82416B44059B2830B2514224A972EB1CBAA07DB8BA8D9AC3AFE7ABF29
          F73212440F1E6F86DFFB7EDE9F999F863186FF31CD3C40AD567B3E994C1E8E46
          A37314311E8F15A7771EB36EB7FBC15C40A3D15824414A1084FB0B0BE7714A22
          4CA75032286F6949C0FE7E111E8F47A302D4EBF5DB5CACD3E90C5AAD1687476D
          AA36213DC3944D09C420EA97F1F1C301BC5EAF1A50AD5699D168549E25E914C7
          AD0EBE34AA04989EF9FAAD3B3828BF87CFE753032A950AB3582CE8F57AE073B7
          DA27BC6B2A2C8351759900861511A57745F8FD7E35A05C2E33ABD58AC1604000
          19CDD67734EA9FA9B2AC549765191B77EFA15878834020A006944A2566B3D9D0
          EFF769DB63A5033E3BFBB3031E570D22F2F93D04834135A0502830BBDDAE8CC0
          017C075CC4CF4F160FF1F6288FE0B508F65EEF221C0EAB01B95C8E391C0E0520
          492334DB9DB3EA9B9DC7F8D19F60FB661AAF765F201289A801D96C96399D4E65
          84E14F49D901E3DBA7F3DF115833AEE0656607D168540DC86432CCE572A1DBED
          5217031C37A903360330259AD656B1F32C81783CAE062493C94FA228DE309BCD
          D06A2FE0DB6113C3A104993EE99427501726D3256C3FDD4222915003B8C562B1
          0DFAD79FE8F5FAEB97CD5720083AE50ECC72C48BCB08851E21954ACD07CC2C14
          0AAD937093DC3ABB4C3C72A7FFE16B3A9DBEFA57C0BFD82FA83A8AF084C76A9E
          0000000049454E44AE426082}
        Name = 'PngImage37'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002BC4944415478DAA5935F48536118C69F633B6E3A6DD3DC
          9C9B4E41BA101131438C2083D034438ABA28882EBA896E22A28BC8B0CB8AEACE
          0B93B22CB408310D324B2F1C5644ADB9618496E69667A52EE7D46DE7FF397D67
          CB0832223A8797F73B70DEDFF37EEFF37D94AAAAF89F87FA1BC0E3F1DC9224E9
          8820083A92218A6222C8B796EFFF11E0F3F94CA4E086D96C3EA8D7A783274550
          1424FE26355959660C0E0EACDF81D7EBADD78A7372721C46A31133FE20519348
          BD0A45550848852DCF8217CF5DEB03DC6EB7EA743A136B8EE3F19999C7B8CF4D
          00CACFA8DBBD07AE91E1DF019D238BCECCB02BA02997949440DB37130C695D13
          61192A519709C061B761E8D92F5BE81A0DA709A2DC2D2E330D0586393D4DD388
          4422A8A8D8825966013EEF6BA22C27D4655946E3DE7D1878DC9F0410551D99E8
          7B3D4D6D065199FB3486E22C9EB4CFA1B2726BB203EDFD31032DE73B6CE8EBEB
          4902DA06BF3A45490ED8B369F08288F90887E0CC241AB71763D23382A9A969D4
          D6356AAE23649AC1237F1FCE975E42CF83EE24E06A6F601B20BFB499690417E3
          301A287CF91647E0E33B54E72DC16AB562686818FB0F1CC2B58516C45625B4EF
          B88D7BDD9D49C085AE89A60D14FA730980094511E74598D27558897160C65D28
          7728082F2DA366672D695FD15C44A1D38EBB773A9280536D636732D252AE6CCA
          D4C33FBF02965817E70518741B201317A6DFBCC2E9A35548212AC939A8282ACC
          47C7CD3650275ADFA6C6592E5A6CCFA039A2ACF9C59343A3ADB5D3E79F5A4055
          612AAACB4C0998A25946BA282A2A40FBF55650872F8E0A1BD353689BD9804894
          C56C7019AB4B31B0510E5C9447B69EC7D9E3E5B058AC893BB066BB2DD7829696
          66500DCD4F9F641BE97A8115C03061518C71E76441FA207342481284605375A4
          148A70991497AD5D262D6B41CEC3045573F2619AC48ABD12CBEF925821C3DD73
          4CF897EBFC1DCC5BB4DA1BF6ECEC0000000049454E44AE426082}
        Name = 'PngImage38'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000029E4944415478DAA593DF4B536118C7BFB348D814B7B164
          736578822069150B6217ED6AB44223BBC88B50083448E8C28B18DDA4D0953791
          7F8091E4D5AE32A32D6D26B374A5ACB525485936DD0F744E62E972C7F3EBED39
          87146A76D50B0F0FEFFB9EEFE7F9719E57C718C3FF2CDDDF80582C36284952BB
          2008FBC9431445CD68AF7A7F6B6BEBB53D018944A286040F8D46E3D5CA4A3DB6
          49044581764BDF984C468C8E06D1D6D6A62B03C4E3F18BAAD862B1D80D060392
          4B598A26919E41610A8118ACB683989E9A4447474739201A8DB2FAFA7AED80E7
          B791CAE43097881240D935EF85264C86C7D1D5D5550E884422ACB1B1111B1B1B
          50EBCE64F36AD6145806A3E83201EC7556845E06D1DDDD5D0E0887C3CCE170A0
          582C1240463AB386447C9622CB5A745996D17CE90A828111F87CBE7240281462
          4EA7139B9B9BD46D51CB40AD9DFDEE81EA5FACF7604B28425024FDE3CEB9D21F
          804020C05C2E9756820A507BA08AD4BB7C4D12CF969EC2C4AFA2CECC613E1B03
          2F97F423B7964BBB80E1E161E676BB3500CF0B48677308AEDD2588886D3281FA
          62371FC3C9C3E7309B9C4022F34EE125B1E68DEF7B5103F8FD7EE6F178B412B6
          4ABCD683E76B7770FEC475C85482ACF6822662B59081A9AA166F17C7F13EF59A
          208259030C0D0DB1969616140A05CAA288543A8727B9DB683AD589E5F52F1015
          09126522CA222A2A0EA0B6FA10A6BE8D6126392D6B8081818119ABD57A96E338
          180CD5584CA6F1E8EB0D5C3E7D93843224FA0B6A262B3F52305759F1211D41E8
          D3585E90E0DD1DE5FEFEFE669AF57B369BED4C0377140F3EB683A7F917140154
          2F38CB71B8382FA2A9085E2D845649EC59E963F3658FA9B7B7D74BA0FB648E9D
          C7A4FA887D10CE236E841726B282A2893FEFF91AFFB51A7A743F65659FBEA4C8
          5CBE8F2577CE7F0165A4CF1731C9CE6F0000000049454E44AE426082}
        Name = 'PngImage39'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002274944415478DA8D91C18B12511CC7BF63CE3A6D6D6B6B
          0B1591B1D42199F6506097DA830489E02188FA07224118E9561825B18844208B
          A048A2970A895834A296E8A479288BC030B7D196AD69C3C0157777C63674C6E9
          8DD542E5A63FDEEF3DDECC7C3EEFF77B43A9AA8A7E9148242E753A9D8BE4DB43
          6485CBE5A27FBFA3FA09E2F1B806FB1D0EC73009249349B8DD6E6A60412C162B
          D9EDF6C3344DA35028A05C2E83E3B8C105D168B4ED743AF5B55A0DD96CF6B9C7
          E3990A0683AAD60A49B9AF201289C8A4822DD56A15F97C3E4B209DD96C3EC1B2
          2C52A9D4FF05E170D84E8027369B8D922409A2286AA7826118E8743A643299F9
          9E825BB34B21A5B57E72B836C75AAD56BDD67FBBDDEEC28AA2743397CB7D23FB
          ABFF086EDE174226E310272B2A24710DA72D2D08828062B1A8F5ADFCEABD42F2
          B6CFE79BF9431020F0F80E9A3BB0672B3AE4F9DB45098DFA0A46EA8FBF1380F5
          7ABD0B7F57BB21F02785D02E23CD4DEC66B0BCDA86568171BB1E6F2A223E7C11
          F3772E1F39DEEB9EBA82E97B1F43E3A3066E622F831A81579B320CB40E3B47F4
          78C58B78BFB4FE2075CD72AEA7E0FADD4552B6813B48E03A811B1AACA730A6C1
          6509F39F9BB30F7DECD9CDFE147525B1A04E4D1AB1BC42604906434ED6E017BC
          049EC08F6E6C0E77059E48453D75740C5F1B2D28B20AD3E84FB8F4494CCF4D4F
          9E419FA02ECCF0AFF799868E59F66F83D2015EF26B2809CDF4537F7F78E312CF
          074AEF5A322C2D3291917E16180CD6E20770933A74CA760DAD0000000049454E
          44AE426082}
        Name = 'PngImage41'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002344944415478DAAD525D485361187EE71CD59542103B3B
          9C3094752744E0C508A746C12E142AB08B8848A28B703403231002C92C8BD822
          221221AA1B2FDAC5A0D5161648AE557A0A8E546C3BD6DA889C434F69C5DAF97E
          7ACF36AD933783FAE0E3FDBE8BE779DFF7791E0BE71CFEE558FE1BC1A9A9E39C
          31068C32A0C62574B532AAD34542A88C354A0809DE3B1ACEAE23F04D1EE36D62
          07185FC6295E0E142BC2A1A01740FBA6C1BC9603F5532A44743A72BF77226E22
          F03EE9E1EDD26E600860C04B40C6598584C10FFD3BD4D6D820339F8199773292
          10EFE3D353D93582138F8EF0F2D814B043690562FCF1EDA817A1C1DE00B65A1B
          123250120AA4D2A9BEC981E781AA44ECBAB1D78F1D0FB4EE704BD442E08BF615
          9ECDC423B1C1694FD52EECB9D2EA776E75FA444104BD5884F04424FF72F8F516
          13C1A1F1FD561CF9CCF8E1D0F9BF09DA2FB8FCCE6DDB7DA2C301C5A20E0FA20F
          F3AF2ECF9A090EDEEDBA85FBEF0CF6849BFF04B70DBB4A2BB85D6E89D4E8B0BC
          B402B1583CA204DEFE5EA1FB4EE735A1CED16B0897FEFCA1242482CA22DA5144
          0945DC6003232B8A320B6A52ED7B733D5916B1FB76E7257B9DD0DF2436AD5968
          B8B16AA151577E2EC346EB26F898C9823C2D87D0256FE2E65CD9C67D639E21A1
          5E1868941AD17B2867002F296501835428C0A2B604B9DC02A80915C16C24313A
          670E92E76A4740D86C3F89C98574A6B242250758F3D48832A1519C2C981C7DBF
          3ECA15A5C710D8F2F4EC8B66A8F29808760DB658B14B7FFC9C7CB15A825FD1E4
          94F06357F04D0000000049454E44AE426082}
        Name = 'PngImage40'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100804000000B5FA37
          EA0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000000814944415478DA
          63FCCF801F30524741F9FF7F0C7FC0703A230343D4FF5F0CBF197E01E15E4662
          4D28F8FF07A807041703F5F8FCFFC5008167883621F5FF6FB0ADBF193600F5D8
          C14DB845B40991703DFB817AF4A1BEF8CDF09C6813BCE1269C05EA51FCFF1BEA
          A7CF449B60FBFF2758FF6F86DB403DA2401320E1FA876813F00100085D5801C5
          46697F0000000049454E44AE426082}
        Name = 'PngImage41'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000000CE4944415478DA6364A01030D2D480B8B838D6458B16FD26
          CB00A0662E203505888F000D9947920130CD8A8A8A6E6FDFBEFDF6E9D3A70E5C
          8630E2D2ACA3A3E3A9ACAC2CF1F1E3C7DFE7CE9D7B80CB10466C9A4D4D4D3DB5
          B4B424BE7FFFCEF0EDDB37860F1F3EFCBE70E1C2832F5FBE6018C288AED9D6D6
          D6D3D8D85802A4F1EBD7AF700C32E4EAD5AB0F80E22886201B50AEA0A0909A90
          90A00CB409ACE9F3E7CF601780F820FCEEDDBB6F376FDEBCF7FFFFFF28A02197
          A9EB028AC3802AB1409574409594886408F97981580000C8CCD011BD717B9300
          00000049454E44AE426082}
        Name = 'PngImage42'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002A74944415478DAA5936D48535118C7FF67779B3A9D9653
          4CCD2C8AD09C88A8642CB32F1566811422235D2A942F145A21122882512A0646
          8616230B999A181565646805561F926661BE9045F6A150D3C536F7E2EEBDBBA7
          EB24096A46F41C0E9CF39C871FFFE7FCCF219452FC4F903F014A5342AA37AB68
          25C308FEAC9B628195383E5998A60EE35CD55F014549AACA84485A2753102288
          7B96A3B039057CB3513A69969D7B38325FB32AA035730DF5553298B3BAE1264B
          190904C2C06C17306D755B0D4673D0AA80BA5D4A9AA82D43FCC6407CF93A8D0E
          830152A9142EE207CE61E676A66994BAFA072EAF00BD2EDA94D7D81FCC7E7C02
          9BC389F9EF160406281014B7077DF5D95675EABED0F8FC16D62BE0F2916D2FF2
          CAAB34BC44860029BF54021BCF404A78F45CB9603CDEF62ED96B0B61991DB240
          CBCBEBB589437987F28B417D559E3CE51C78646842CD50F29D199FA4DCB9FE63
          CEDF00A1FBBB7CA8E02EF4C5BA96C7FA1D18BDAA05C5F2191147446E27B46543
          70F253E5049C7EFE69B163051092D14D58378EAE9547DEE8BB960423FC306C06
          9C76D146B18C17E7267F606F9813DA53C330D9DF974A1997DEFCAC84F700E20E
          B7658593F9BB45156760F26330E6A0E04491BC7855EC223C6B6E91626B980472
          2BC56057036679A660ACB7E2A607F0A62A865FBF3D8BE99ED3E075F84130A272
          81A71E002F1AC6BA44A08BC0EE2048970CE044EC2026077AB8D48B13720F60A4
          3A56884AD39292DB515067E760C2A20015016E765985C447EC5A342426C88591
          7B3DE8D44DE1437F174D6918977800A3ED27CFB2137DB5CD330518A53BA46AED
          6E905FACFAE9D3F34BF79111F9CA5118D1CBC8B6A437AA75CDD52B2E7CBE55A2
          6C1B8994B78F279CA766532623530483888F4110BD70F3BCC02FDA64C1AAB707
          A287734F6B669D1B725A17BCFEC67F891FCDC250F0BA2A75D70000000049454E
          44AE426082}
        Name = 'PngImage43'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000027C4944415478DAA593DD4B544118C69F73C45DA5D56D6D
          5D0BEC43C32EFA322BA22FFAB82828A8D008BD688B92C80BBBB00FACA0200ABC
          090ABAF00F90104B6FBA58D82C9152D3708DA24C364C33C362CDDD55F29C3D67
          66DEE61C353C44203430BC33C33CBFF79979671422C2FF34C502D07088D8D438
          880C1033415C76D31AA7E43805983A38D3E55CB3C7DEE30D8A0320069F905A78
          E82FBA65CEDE39B75DCEE38F2AE0AB687102B4483D659654828436BB6A89C53C
          E54C545437E20FCB90130C3901D33D0F28A3F818D854372052F651EC28AC28AD
          DB5D83CB5F8644732DFC67DB9D805F9D7729734B19CC64874C6DCE8AE68BAD3E
          0D776E39E28D97905BD5E30424C3D7C8B3A7122CD93E2FB306218598CD6ECDDD
          792731D1508D40F57B27201EAA2157FE06B05414E0969881EB93C85815B0335B
          905B917EC4750D771293C8AB197402C65B4E517AA0089E9252B924208C614CF5
          35C1BDD2278F342D4BA9E37A6F144B16AF41FFB73E34550D38013F9B83448AB4
          AB4E227BD739288A82CB4FEFDBF7614837066358E62BC2C6E5BBF17AA80D6F47
          BB99CE4CD7CB2B1334E3E07190B2769463A2F526D23C99F0EEBD808BE17B38B0
          FE34B82C27175CFA227C4F8CC2E709E0D5E03344465E4888E19A01349EA0EC7D
          67A0C786A07D08212DC38BDAC4471C2EAEC497F14F30A50B264C98F285AAAA0B
          81AC7C747C0EA367A8D3B001B18623244C79FBD673353530ED07AE7A17E1E8A6
          F352C8C138B79D8C254790E3598A375FBBD03A101E3018362BFFFA4CA5F5055C
          370C790706E479B1DABF56DD5E7810BD235D781E6D7D27C5DBC6EA28A52CF437
          AEBBADC6B6AED8EF6F8FB6450C819D526CFCA9C2425AC10D25C6459A5F133C3D
          56476C6EFD376EC494DFFFCB76EC0000000049454E44AE426082}
        Name = 'PngImage44'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002BD4944415478DAA5935D4893511CC61FA79B2E5147989B
          1F11393517A2859A14AD297993941012A951E285177D48924965305951525E04
          F346F1A29412224548574E45243315A4462853E647687EA492B8ED7DA77BCF79
          DF5E375C507A53FF73713817CFEFFCCFFF3C8F9F2008F89FF2FB1350D37D5B4D
          795A4828BD422851119E82A364493C37897BB331FFE5D4AE809AEEF23C42F9DA
          FD4A7564A22A05726908DCBC1B3F9CF318B4F562797565911052DA50D4D2FA17
          C02BA68D69F1BAE08488644CDA27B0E49A07C77308938521421E8D810933C626
          AD8CD859D1AB928E561FE0695779ACD876FF11F589A804650AFA16CD6019169C
          4010141408415C9BFC2634A149F834DE838999A9058E106DDB8D9E690FA0BAB3
          4CAF50EC33641DCAC5D0CA0738DD0C1886C19D0C83A7BB479FEFC1C5D8219106
          2043A545536F3D180757D571B3EF8107F0D0546A4BD5E8E236256E4CADDAC477
          737038ECA83E6DF400AEB55C4288420AC2132488B3D97430300DF74C9A6F7D8C
          F700F46FAFB29947CFC9271C635873AEA1F2D8E31DBFACACB300A1720592C2D3
          50F7AEC1D55B31B4C703A86C2B618FA764CBADF651B8E806AA526B760584C842
          91AC4C87B1BDCED57F77C40BA86829B6A90F6AE2F84001DFEDB360B90D30CB76
          345C78E3136E95C44F82F8F0C360D759B40C9A2607EF5BBC4F287B7D59BF2161
          0D67532F6260AE0B1B94838CF7C393EC173E004F05C82401C88CCBC1B30E23D6
          1DCEAA11FDA87788D79B0B62394AFB9511E151E9EA5318F8D6E91998F8B510E8
          16428054E20F9D3A07668B0943E35F1744CF682D06EBB4CF48C5CFCF8B46228D
          D12A65B036311B73EB33985F9B81C0F388D91B8B98D003786F69C7B0759411ED
          5D248A7F1B69BBF2EBCFE4897EAFA5FE2452A739096558E4D6E598FB390BF397
          5ED89DECA2282EDD16EF18A65C63965A841472BE3011314CD41326CA93668B61
          7CF730FD4BFD0276F996F0D72273FA0000000049454E44AE426082}
        Name = 'PngImage45'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002954944415478DAA593ED4B536118C6AF33DBD636734C13
          7599914E320B3243C9425351316C516964122E2A08C5FD0581627F4120414484
          1A884403732A4A6AA010256192F992DB7CD964BE8496DACE1967E7799E8E130D
          6B7EA9EBD3CD03D78FFBB9EFEBE61863F81F717F023C566B1223B44202A96462
          20968282316991803653465A4E3C6F75ED0998AFB1964A9434441C37C5E9D2D2
          C019F4805F4060D18BD5DE5E7C5F5D59208C584FBF68B3FD05D83413429A220B
          7275BAB45320AE3160C9038822B87003B8230958B177C133EBF4512259325ABB
          6D3B00774D4DA2DCF660544E96519B910ED2DF0E5EF0832301A8554A70948189
          7E20351DDFBADF607EC9ED254CCA3EFFF2ED74103057555DAB3546D5475EBB0A
          FABE0F84DF802008D82F0ABBE6437C02949979986C7C8A3525EA725E0D3C0C02
          66AAEF3B620A734C4AC90F7E6E0AD2A80BA1A48AD543119300DF9A80F12F03CE
          5CDBBBE420C059758F4FB852A2E1A646C0AFAF41FFE05148C0F24D33940603C2
          CE9CC5C7F64621FFF590360870DCB5F0878BF3359CE313A8DC85A6F6C99E00EE
          801EEACC7318EA782614B40D6F0126EFDC72C4A4269B344A02C93D03DEFB2374
          6A140AA8534E62DD2F6262A4C759D4F979EB0B63B7CB6BD5FC46FDA1EB3720F6
          74802A24043602BB07B8B96E850AFA8B660CDB1EE3A7C8D715778D6F0D71D452
          9648146CF0A036C21899970BC16E87BC6B304240C0C96E2A23F621E2D265B8FB
          BB30B73CEE251CCB2EE99C98DE09D27085B99432A9293A3C4A179D570871D605
          D1E5021529D4C74C50C51F85A7CF8E9995291FE5A84536FF0ED2B63E9417C969
          941AD4448C8B4FC992D7660CBEF35E379C6303E0897F41365BB7CD218F69B0EC
          42129302155218AD6494C4CA0705B95E94EB662A492D25DD5FF73EA67FD12FCA
          BE71F0E82977BB0000000049454E44AE426082}
        Name = 'PngImage46'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002A14944415478DA7D935D48935118C7FFE7DDE6B4D22935
          834CEB42A9302FFA409A52106146269AE20C6B6E626AD684665FD2F20B749A6E
          2A82D2554A6653B3566A630649DDE87D667899491765E99465DBDCDEB7F36EF9
          EA68F85C9EC3F93DBFF33FCF2105F7074D5E0FB9BCB8EA0D75B8E02F16428549
          818830F4588C9774085224BF6AE87B63E97179B45C4E081183A38B6B2CF9C7E0
          E0F5B851546FB35B3B94514101695AB37DD8704156DC9A8F7D7171C286633101
          F65F7238D96D58585EE32878259819492D31DB6DEDD9B2BABE4A1C3A160F2262
          C088194C5876A3B63019D40C5B99F900560A681EAAC2C1640AE0FC1D067A76C2
          5C97898FFA7AC4EE89D9682D966076690147EF55A1403FBAEC038C98B265AA47
          99D8157E06EE6F7BB1E4A0BEBFC5D81E2A01EB744344ADE86D114AB5B30E84E3
          AC6B0E211A0DAE5453808202468DD932F5E32C842FA9D0A049DD525B6B788BB6
          989F9068D450F18013C5FDF631538E2C537F0A3B445A0C37E4E07ADB13C42724
          08D62286E0EBEC340C95E52879F00A4DCE71443677A3B09637A08051D345D982
          538A8A460B2C4D19305926705871921E14811149C0300C3E4D7D40515A0ACA6A
          5FA3DD350EA9A10B6A1E90AE35CFF5D5A5C785848441A97F83E714D06D9D4452
          EA696ACD0916D393EFA14953A0B4DA82B2C5111CE9EC81AA8602726E0DB67858
          A2585DE392FEB8D8081A28B9667C89D84485FFE65E7F02F39FA7D0A9CB4599C1
          86963B598810BB29606499709BBAA4940EF802BDD1314601290113373F338936
          6D06CA9B6CE8AD390F8FC70575FDB83D00A0B8FACC17689EEE21A2F72706007E
          7C99C1D3D6BBBC367F60452AA1D328466FA00105AC07CABF7B607182F6BB2E65
          A4F0173603CE550C0881D241F0CDC1C63E11B4AD1D79514101B9B78784405916
          FFD5BAF60BA3F2E6FADA5FFEA232A23E0F4AED0000000049454E44AE426082}
        Name = 'PngImage47'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002FD4944415478DA5593D94F135114C6BF3BD3C2B4A52CA5
          6C0163442A6040A26C4A1421F2A0511E7C0089BCA871DF08261023FAE4123091
          F0263124BC28F01F484425C892206E5110312C09C6B0B40265EB368B67A6A5D5
          49CE9DCCBDF7FCCE37F77C97298A02F529BBD777C6102954730CB9F42950306D
          411D151A1419A22C4DADFC51DEB0D5E5BA81D672AFB6BC0538F670A0FFF9CDBC
          4C8E63D134C3C33FAD3DFE2D0AECEB3EBC1859C1FCAC67F3FBE8B865B0B5DC13
          041C6F1C5CECAA29B08C2D82E718D32A5351886A655181CB27A2D46642CF940B
          C3E376CCCD7A5D3F277EC40401279A86363B6EE419269778CA553401320D922C
          C34300B757C2619B8031BBA2415F7D99C7DBFEC9CE9082A62157DBA55C61615D
          07C65400FB1F204A488B966136F008D7EBB0E9F6A2BAF983183A83A661972D63
          87E09414D41FB0A2E5F30A7422509D6D869E4ED6231288547809285314A786A3
          E2C9885B03E43C9AB124C5C0BE7BBB995B764AA82F8D43F3FB65C41800A753C6
          E99C28AACAE09564F83400909FA243653301B2EF4F5765A79BDAE38D4C58A5CD
          E70A2CB09A75706C4868FBB8846823837D5546616C38F2D34DF09142B5685622
          8753AA82CA8E39676A8C3E72C6E1C1F57C3362A323FCADA74D938B1E748EAE20
          3982C72FBB0FB565F15A4B49006CB1F00376DD9AA8DA9315D11E67E28535A70F
          178BE3618D2205EB229E0D386036F270D07C71B20945D99174B08A06C88CA7CE
          3D18F69FC18EF363966D29BC3D7D6724B7B624E26E450A1A5FCE23C2C4C1B52C
          E202553609BC9628516B5463E6247138D2D0EF0E76A1F8CEA02B353145709041
          8D1909F0CCDAB1BA0644A559C185B1A023E5803B3B8FEA5152F72E043878BBCF
          D5555F283C1E0DC3BE04CEEF013598DF504A00A0C6D705112D459453D31B02EC
          AFEDDDE86A2834347E1398421972C04C08385275A706224F7024E3690993F32E
          F76C0601B9575E4F77371DB26EF81413CD71F8476E944117FC05839E23A74236
          84716B5967BB7F070179577BAE893EE524636C2FD53686AE6240FED6F5F4BFDC
          E4C64F1CD0F1177A4C92825D50BA970000000049454E44AE426082}
        Name = 'PngImage48'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000031F4944415478DA8D93796C0C7114C7BFBFD9DDEE4E9DDD
          521A84D0A26913B4EB88966D1DA16997208EB404898823258E8A04758428A189
          3F24C45557F50F91F847C4D2A697044DA9AB0745EA68595D9BB233B333BBF3F3
          767653FF7A79F3667E93DFFBBCF77BEFFD18E71CF30ED4AC1307DA0A05860C00
          367A1810B59C0CD711D4431DBE1EFE88F5FE2AAE3FE75211151606E41EABAFBB
          B1CD9122086C30074CE0E8136E7C7378FE68B8F9CC87EECE80F4F6758BBDE19C
          2BD007C82B6DF851B97D9AFDCD0F9804C68CC81414C170E42087AC059193DC0F
          EE0E194F5A3CE8EA54E5F6B6D6B8DAB3F90103907FE2B15451E410DF7B4DE4CB
          8D047432215D4780008A1A8233D986371E6E401FBCE84655DDFB5B55C7730AD8
          CA53733EE8DC3E6670ACCE4274DEF42417BCFE9FA86EBE6A80AC96FE040A222F
          BD006B9C9B686D86A4A8282C6B0CBA8FCCB68401B7372ED8B94CB676E193B715
          4D8D9FA8601286C6C5430D0521493274D8B134B314CC140B9DB29A3DD68AE5A7
          9F290F4AB2C430C0AAE969FE3D8B9798DAD55A74F578D0F6BA0B2126C3CC6260
          3625A0706E1965638646CEA4983AD28C1565043830538C74A1B4569E9870C5B6
          DAB9014F7BEEC2D71B4273F30BD862EC285A74958A1A038DF66921AA0FBDD386
          0B584919B84BA20067C9415551CB2DE31353B0C37508F73AAF4197AC68697D85
          851945989C9447C5033432940092E36100AA0E11207D77E2460A713E33351B5E
          C90BAEEAD8B5E830AEB79E855989C5E78F1D78D7DD820BDB3AA93311404A0290
          7FF489527334D3007CA17F23C2C312D6D109A9488C1B85555905A8F9F208DF7C
          DFA17A246C76555037B83198931205CCDD57A7349C74468E90BDBF5EBE55ECB0
          79FE588CE9F3F9BFE34EFD11F8955F604CC000311E6BE79FE99BCEA4A1407671
          ADF2F44C4E0490B5B746AEDC33DDE6952DF81F19174F3EDBAB95E7E7E7470033
          7654FB2BF74D177B0356F63F8009C398EED8E4965E5D5C30C000646C7EF8E1FE
          895943FC1AEF476B215C0B3DBA799068EEBB54A245A02341176384DF69EBEF7F
          7D5B9E9B6A001C5BDC5B831A5FC2189B42658AFD77150D8DDE8EC89A44A1696C
          12808A9797732FFD054DC492F05A96D8690000000049454E44AE426082}
        Name = 'PngImage49'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001EB4944415478DA9593CF6B135110C7BFBB0921064C4802F1
          E08F4B4DF12241B4951CDB90E46FE84D4C0F411390827F83370BB9780828480A
          09923631207810B5171B5B6D0F016D0F5A6AA1D05CC526EFBDDD38B3E495ACA4
          51078637ECCEF733336F778CC16000C3302E01B840EEC1643B263F208DAD1F18
          43C0CD5EAFF7C6E3F19CA7D81CA7544AA15C2E978AC5E2F228440366A5941FFA
          FDBE49B14B64DB3684108846A3E876BBA8D7EBA542A1700AE16CF6194A6E1304
          A6693A220DB02CCB01442211E7E477954AA594CFE79709B0EF027072ABD582DF
          EF472A95E26A0806834826930887C3AE91BC5EEF6D3A365D80E1384E02C70C64
          E74EB83BAECE712C161B0FF8F3E24621DA19120A85CE0654AB55A7DD6C367B1A
          F338ED761B8944C2B9A34020F06F1D68E3AABA1B369FCFF77F00163F78B70829
          147D0909D9935859589B3C42269341AD5673E2743A8D7BAFEF606E6A1ECA5268
          7E7C89D5DCABC91D70D5D133D758C0DCB579084B627D7B1D5BCD9DDC5EF3DBB3
          BF7E85C71B8FF0E5A883C8B928AE5E9C2680C0F7C37D6C6D6F8ABE906F47011B
          C3D8BD03B6C252E33EFFF4885F89432881CF9D1DEC7EDD3BA04B8DEB5DB8457B
          F09E96C94F9AB1CB947F7E1727835FCE25E2A761BF78D89825ED270DB83C5CE7
          B1626D3796AE3F952742759EECE678B549FBE337F4FC4D72BE47841E00000000
          49454E44AE426082}
        Name = 'PngImage50'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000028F4944415478DAA5935D48935118C7FF67DBBB4DDBAB13
          1336BDB0A9F3332DCA1911E58D50981A06918644521974551741109404DD04DD
          2404DD441F04A510986284267DDCC8C83011CDA69656CBA9B9CD6DBAF7F374DE
          BD13EC26B01E38EF799EC3F9FFDEFF733887504AF13F41267B768F58D34A4B09
          316C58A61022D3DF1439B49715C29F8AC43756543F2627CACFCF3DF182BA210B
          2144D325C460B91C5F4260A45DA4C9552D8CE6344A156921EA1FBC5478E443A7
          0E7856192A6878972ECEDF81221A01431AD3F3B066D7B09C63724DAFB2594DC0
          89C126FABAF7F9DC8DDEED3AE0A96725BF71808F7F7FC8F650A8C232A458000A
          6B89B33A9946627A917134C732F8E2CBF8D2571B751F7DCFEB80CE4AD155D7CB
          4567BA40C528A49545F025C761CD2C4936BCEE80EA0E4C76F8BAF6C4DDC7BC29
          3AE07195E83E31C4697F59DFC87CB0F207E4D000A696B76226BC0D8B6B0EA458
          3864DA14E44C9C11CA9A5F5A75C07D8F50D0F2C6ACAE8E822A213622A07208AA
          14C454D081AFD2411467DB60E28C58088B588E0810E7FA9586FA66930EB8EB89
          E7B7BEB22891D74C1866807002A0E53D3F2FA23C97475436425269A29974AB11
          63BE79341DC8253AA0A372ADE05CBF550A762785A124248C277357515DE14020
          2283E921C81419A91CC66796D0529DA303266FEDF2BB4E77A68ABFBC3CD43503
          A57176146CB0B977E91476E4D9B110A39055FD3AD8B770181E9D455B6DA10EF8
          74A3EC020CA48AF9AB6187CE6FBC74534557B8AC8AC346CE6286A050980C043F
          8312FC1F0769AEEF76EBA1EB030FC8DFDE4247DFACCB62C2C90C9BB95D33B012
          135439304EF667F9496CFA6D647A62B88D6CF631755FAB3DEB743A6E66B976DA
          27BC2F56370DD0E2D1F9A226675EF93D559553FE09B0317E035C7D448FD2E97A
          180000000049454E44AE426082}
        Name = 'PngImage51'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C1800000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000003404944415478DA6593FD4B5B
          6714C7BFF726E6269162D21AAB89F1A533D56BAB4ED0457F59AD505B9D59E9C4
          C6D19F677FB350A8F40F28946D30363A94766C4D59AB166A14D18858990C747D
          2FAD685CCCB42F59516350935C9B979B7BF7DC07125A76E072E0B9E77C9EEFF9
          DE7399AEAE2E283138388874F4F4F414D7D4D4E458AD56F5C4C4C47113099665
          F7EBB49A7DAC5A63D0EAB22D1267C0EC33AF97713A9DB469686888E69696165B
          7777B7BBA0A0C02A4952323F3F5FB6D96C26E55D4848E0A53F00B01C3EABB2E0
          DB6BB7FF661C0E076D1C1B1BA3B9AEAEEEDCF4F4F46DA3D1880F239914E179BC
          0263AE09A59603306433B8F2C3AD7F98D6D6565AE0F17868B6DBED5D23232377
          CC66332BCB323D9352298442213C5B0BE1705525D4E48CD300DFFD74CBCF9C52
          00A4707272921637343438DD6EF7401AC0300CE2F138829B9B04B0858A4F6B21
          498096007EFCD9E5679A9B9BA156AB3135354501F5F5F5CED1D1D1018BC5C212
          0F40CCA380AD60104F5683F8E4682D524411A751A1EFFA4D3F53595E8EC5E5E5
          CCACC4030A282C2CA480B40205F0702588E223B510933168390EFD378882F2B2
          322CAFAC6400D5D5D59DE3E3E343454545AC725306B04500BE204C872A214B22
          541A0ED77F211E54D86CF0FA7C19406363E359B21383252525AC288A9911140F
          FEF4BE43DEA12AC82911E1941A7707EE7C0C68B0DB73DADBDB7FBBD4DBFB95E2
          4BDAC4F77B7B44C12651F00E5C6E1922521622228BFB63772980F8094D2C164B
          91A7C2E5728D9F6C6B332BB72A1E28104589100963C1EBC3AA68C28E6C845697
          8559CF3D0A3010C061D2C0C513096B5F7FFFF75F9E3E6D8E46A348EF01552144
          B1E05BC57C8085A8CB43AE418F3F3CC36B0A802535458964F2188128809E7687
          C3A400542A150528183995C4DBF52D0CCC6D20CA1AC0EAF57834ED7E4E3D20A1
          4A2412674861EB4D97EBEB634D4DBA3D3277241C065185402090F8F7CDDAAB07
          2F96B757C197FA655E140273AF65BDF162C64492B389079F7F73FEFC65B2DE36
          41100CE1DDDD99D0F6F652FEC183F3D19DD04CDFAFBFF3EB795FDC8808E18DD4
          FAD3DEC05FC32FFFF7193B3B3A0A789ECF217FA3BE82E717DFC762F12CB50A27
          5A4E826FEAA8DB11F517349270F5F5BC7B49A9FF0FF8D2935990210E17000000
          0049454E44AE426082}
        Name = 'PngImage52'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C1800000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000003624944415478DA7D927B4C53
          571CC7BFA7C5DE826D79B5F5B560B2CE64191837B1F601086E8998B06CF13996
          E9E6182CBA1933AB712C5B24F391CCFDB9996D6036306635206E1A8222C3604B
          B24CBA05D74AC70A83E1A0D0A608B5EDBDB797B6D7D3EBFC0335FE4E72721EF9
          7ECE37BFF3258DDF9E834C26837F72125D5D5DCFE7E974EF6D282D7945A55AAC
          671826432E971396E301514C442261DF6DEFDF3DA33EDFB5670D86C1170A8B38
          72A6F147D03244A2519B92515456BF556D88C5783CADBAAF76CECDCE857F15E2
          C9D3E4D417A7914AA53EB75AAD4757AF29C4F9F36D989E0E40A3C986365F8DF4
          EB1ECF08723459D0642B41640AECDAB51B33A100ECE7EC21F26E8D0D5C2C5AFB
          DA962D8D9B2B2B64137746E1F60C52C749308C0289C43C02C159A8554A0AD560
          D5AA22C9C5B52B5DB8D1DB3B450E1D3EAEE2A27387D5B9BAA3FBF6D6929505F9
          542CC2EF0F81DA445E6E2EB4DA1CEA248E7FC72624F1F767CEC2ED7673917B77
          6BC9BE0F3FB13D675859C5F349797E9EB67CE7CED79193A340BF6B007F0D8DA3
          A0E019582D6BC1F30F00EDED9DF0B86F61FCCE5843786EF618D9B6A3EED4F637
          B6356854D9E20F4DCD5F1EB41D3860361762C83702A7F337E8B57A98CCEBA9C0
          8FE1E17FE0BAD90FDF90B76762F2BF2A968B0AA4B6EE486BDDDE773C8BE48CFF
          EBAFBEAB5ABA445F515F7F308F17E2F8E9E2CFD44D2E8C462366EE86716BE04F
          F4391CF7A6A7FC164110BCA95412A4A6C656B4F1E5B2932991289DCEBEA6E0F4
          CC54FDC7B6EB26CB6A6577B713814000164B09A25116ED172E2686BCDEF75996
          6D060808A1F39E3D1FA1BCA284AE647038FA109E8DC0B8AEF8D3DD6F579FA0AF
          A0A7E717AC375B30EC1BC585D6363BCFB135F1B810A75F2F357401A0B7D78158
          84C5F2E52B14C56B5FEA282BB36E72FD7E13B1188B8101CF483010584793197E
          285E0010A92597EB0F9A090EF43790B128638DC562EA5CBA6CC98ACB973AE2DE
          C1DB5BD52AF51522F996A40B0189640A63A3E39817E6690E5248D2BD4EAFB397
          6D287DB3A9A9B96F2614AC641825F768AC2540C5460A48880806431239CD16E9
          C8CACC6C335B4D3B5A5A5AAFD293ADF4827F22A0D8F822642403E9A64916D300
          9AC6C559991DA5E5A5AFDAED971CD45515BD8B3D06D8BFFF3398AC6649F0BFF6
          418948F7E1039D4EDBD0EF727F332F08272820F928E03E8E9190AAD5122B2200
          00000049454E44AE426082}
        Name = 'PngImage53'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000003294944415478DA55917948545114C6BF3B9B6333E54C4D
          526869522A96985A6A463A61585196855198151449FF58421BE11F511464D1E2
          0286B61B6665D0465159061558667B4D68E5B80DB86559A3E3BCF7EEEBCC1B1B
          EAF13EEE7BF79EF33BE77E87C9B20CCFB3A1A63D8196136126FD9C4181C32381
          730C89FC27EDFF22F59224523FA98F5449BACFFE02D655B7E4C54C3016EFB08E
          C7BF8FD34D3052BF4B825B00068625D87B7FE3EE17E7773ACEF701D65E6C3E90
          1A662EC84DB6A0FD87042D63D03040AD21A9019D8A56950A5AFA768932EA5B06
          51F1DCD1E303AC39FBA938634660DEEA780B9A7A390553021D69D45E808A60A0
          D8A00035F2CFB5E2704E088ED6DAE103AC2A7F7B715DE2A4B5A9E163D1D42340
          451954DC9BEC8180414D90B0F16A6C3FFD1E99F38251FAB8CDE103AC2C6D78B0
          2D3D7CC1148B01DFFA3801E0ED8240CA3A028B9EA845CE89977069E501DACAF2
          01961D7BDA74243B6E9A9B6BD1E59471B3B619A3FD40068A301B7570740F8111
          C8384A0D2E33F40F0CE1D1EB8E441F60C9A187C355DBAC3A5B9704270DABFAFA
          7B2C991F81CC38236ED577202329F8BFE99CAFEB41DD8B16AF070BF7DFF5D7A8
          5583D776A7E379AB000E19A72EBF465A7224365A4DD85AD288E2BC7825D12502
          6E2A507EA7131F9BDBBD80B4821B93030CFAD66A023CB30B8ADB676ADE604162
          04B253C6907D4C9982A757CE65D0845178D58E57B6118075E795C840F3685BF5
          9E45786A172171A0EAFA1BCC9A1E8ACEBE5F4AB2489B5CE20A44B936BD9D0E07
          D8DCAD9516DA2BCB4C8DCBCA4E8B44ADCD0D831EB8FDE01D62A3269159A22705
          A2285175AE74C0654EFF023A3A0890B4E5F46A8A381B1A1CE81F161A8EDFC332
          044946B7A31769B3239095E20F91BB2944A5549519875EA3C391AA36347C6802
          9BBDE964E1726BCCAE829CA4FF5C5EB1F70916274663798A808A865C580C933D
          4EA0C7D98EDC8422145D1A465DE34BB0B8F5255B9263A796254485C064F48729
          400FB3C10FFB2A6DD8BC340AE9B1DF51DF7F19265D90D2C40F573792CCAB70F0
          82847BF58D6033B38F7B3CC8278590C68DC8AC1A3525745AC858ADC9CF4C2D8F
          A16B0860E40193C90B3688CF6D5FD13D20E00F61EB66554FF244350000000049
          454E44AE426082}
        Name = 'PngImage54'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001C44944415478DABD92CB2B44511CC77F487291E68E7B85
          C90CFE021BA52C4429CF85678A2C58C802C54A36F2663576926736281B855858
          7A8E67B9C3CC65664416B29B71CD3D0FE7DE74892152CED99ECFE77BCEF7FCC2
          28A5F09715F6EF829AB932BBAAA2C7E5C6D59E5F0BAA674BED89BCD88A10028F
          CFD7B5D6B635F86341D54C890E67D86C107852C025CB707DE5EDFE91A072BAD8
          2E9A0406A70161FB5CBAD060083EABD186A065B3211F239C3E5E383FF11EAE98
          D2E084D634964CD996182CBB3D18A92876BBD7A1E882968D861C84F0A2993325
          DD3DDCB7CF562ED935B87CB2E815B6B25C0A4EE9D28077FA0E15BDC4E6F5FA02
          963C65135253A222A3C0219D68073A58D316D164EEB46AC91483D3E9822BB757
          8777FB8F14E31B9B566AA309A201731C0FC96212F89F02707B7F0711E111208A
          02D070804B4987095271CCDEC01B6C7C63DD42054730F19BE379B024A7002118
          824405766BB870BAC1E3F65176236E7FF058F958B051222B8B238849781E52AD
          16404CE29264F0C83E8A3478E8E413FC69900AC7F274892024808A54F0CA373A
          7C307C1A120E3989B9FDD91C26C41F548288BD39D63172FAFCDD8C841CA4ACAE
          4C8E25E3C3D1B36FE12F05BF592F295C195A0BA58B2F0000000049454E44AE42
          6082}
        Name = 'PngImage55'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1300000B1301009A9C1800000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000001EE4944415478DAA593BB4E1B
          411486CFECECC5EB65F1A589234BD04496A1F233E00251BA88E8A36469084E9D
          0748B3E40130900A3A7043038A62F208A952587211DB69A2D8F17AB1E5BDCC2E
          67C7C632C820A38C74A4D933B3FF7CFF9933240C43F89F41E6097CFAF2937E7C
          B3CE1612300CE3229D4EBFA494AEC5E371D034EDB7288AFD7ABDAEE2FADF5C2E
          A7F9BECF370F0603180E8780DF7F1CC779857B4E48A9540AABD52ADFE03206BE
          E3021529D8F60DCFE9FA12307F0C232A32C894C2BE69C2EBED6D28EFED7DE702
          E728D06C36A758C964120E0F0EF8FCDDCE0EF47ABDE9DAEACACA54E0FDEEEED7
          FB04AE837801B89E0BFDBECD73CBCB3AC8923C26100590656596E06A2E412291
          80C34A654C60186059D65C0214A88D09CE4E0104155CF4EF631D2202DBB62735
          982140FF32D6E14EE043B97C3DB5F0EB01C1D184E0EDD30457F76AE0781E30CF
          7F94804A22289234C7C244C0437C6734822008A0F6ADC6731BC50D100481CF95
          580CA4996BE4160A8542339BCD92542A159D16AAAA0A8D4683B55AAD7F994C26
          99CFE7256C1CDEAE688574BB5D68B7DB91621AFFB924A6692AC56251C34ED4F1
          249D10A20994C6B01B19761B5EAD3BF23CEF8631F6C2EAF57F6C6D6D5A95A363
          B9D3E990CFFBA64B16784C22462C72802145A5C0F08330C4C665CE2202CF7F8D
          CF19B7028B0A6228AD1E400000000049454E44AE426082}
        Name = 'PngImage56'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000024A4944415478DA95935D48536118C79FF36E3B9EB98F064B37C3CAA1
          326B7DACAB2CDE9BC2826A154D69D41209645917DE8597791D74D14516B52416
          1413A5BA08854084D74A2D51C9D697C41AEEA3CDCD6D676B79E6D9E96C20352C
          B7FEF05CFD797EFC1FFEEF4B0982006B6AE919191000D58F2E208B30D4128432
          44FD09604E38496FB705DF1A9C8D845315F695C7875EFC1FC0729F982FD97086
          6160E9F9CB5CD41B704AABEA2EA7EE1DC89505909FEA27868EB398D52A419137
          3D8BE01F9F9E12943A2B7BB7D95F1A607D40EAED6D98552B41C80920A3299087
          E2F07DEC4DF42752B7B3B7F70F6F08A86C7311C3190B4E30F2BC017987924940
          C171C04ECEC17230D24FEB0D8EF80D33FF5780C2F6906C3B7A04C7257401B0A6
          1C85A0520280C271087A3E4D23ED162B7B7D8F6F3DE0FC2352DB8C718CA70015
          DCDF517981028646B05793838509EF0F3FAF694DF5ED1A290228DBDDA4BACE84
          97921C2029020A8904710431818241D0504343B59A860F1FB9F0B73867636FEE
          1C2B02A83A0688CE60C2E1380712112088CB1442A0DD2401A3BE02921C0573F3
          5E42696B5A13BD4D917527A82F0E125DE36E1C49660B09F290AD5AA93834BCFF
          12E283C1685FFACEB1EE7FB6A0EE1C223AA31947D82C28E45268D4C940CE4861
          C6B31848F8BE5ECBB8CE3937AC51ED784236EFD887412CB0494F437215C1ECF8
          D4DB15DFE79EECF0D5D1920F49D5F5949C3C7E106779015E4DCEAFC6022177C6
          65BF50F65F50389E91C3B546FC2EED0F27B79B4EC7AEE827A0848A4FE87AEDD6
          6A8CAA74036F0F77562D975ACEEB1711B60AF0391528540000000049454E44AE
          426082}
        Name = 'PngImage57'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000B1200000B1201D2DD7EFC00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB0000012D4944415478DAB5D33F6F82
          401806F0E70581B010F553B874F39340FA6D1C9D5D8B81908830D806523734C1
          8491C1C50E2A1F85BFBDC3D6A64BA392DE7277C3FD9E7B2FEF51D334E832A833
          609AE69888A6755D83CD37692C94645946966513721CE755D3B4E747D2D3347D
          23DBB68FFD7E7FA41B3ACAB2844084BA6AD010AFEFFB9E6DEE752349128220C0
          66B33971E0C08027C330E02E16785FAFE1FB1E64594155B1B2D8C1E6225C117E
          7D0E6CB7DB8F5F80F96262E92DB1DBEDA0280CE0EFD2D6FC954D683951107F00
          CBB20E83C1A005D81A9EE7218E63F47ABD3FEBFF1F603E9FC3755D2449F218E0
          FB3EC230C46AB5BA0B383260C4817B0607A2283AB58D341C0E75D603A2200837
          1DE65D5B1445B5DFEF439ACD66E3F3F93CCDF3FCAE565655152C70D2FD337505
          3E017D28BD4672B4937B0000000049454E44AE426082}
        Name = 'PngImage58'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000029E4944415478DA8D935F4853511CC77F73FF7226F3EA50
          53B4BB3F4DFBE35266491094B01111141116D5FB7A2DB1EE42EAC18A36093109
          623EF41041A54F410F81038332A1B60235756DAE7975AED039476EBBDE7BEE39
          9DBD480FD3FCBE9D03DFCF39E7FBFB1E0521044E3D4DB800834FAB06F89345E6
          D1EB7551D8A11479405EEDFD0B846088BEEFAC33EFD4BC0938D9B760DFAD5106
          9219E44708A7900C8C8C6070A2871DDE11E09897E7D87295673621FAA9316832
          A8383E29423A83CD738F2CD1FF025A7A6223D4E8A0272AF29BE6AE48A04AAFB4
          F3CB927371C0EADF16D074E727A300B22A6CC8C321AFE5E2DECE08636F79BB4A
          484664F493BA67E75EC9DB02ACB7C21DB565EAA1B965C93BDF67715F1EBA176A
          33A9AD9228C1B7F9AFAF858C70359B29E32545EF808CC141F3018C214510F18F
          DFAE1F54D4DF08FB64841D04132032980EDBDEA54FD8527A5B75337C0A8FC1E7
          D01E49A5BEA4AE2A5541F897086C850A2A4A94905893E0C76FE9DAE618FFD595
          97E77B593D7BB352EF84B1E916282B2E82A9B8E81DE7EADC47EEC73C876AB55C
          6E83C0C482E02E08C8EBF4E3F68DFDC66ECDCA6A1384F8298295E68A2FDD6CCA
          7637E6B3B35A57725D8299B858F806D41CCFAE0B35F4FD20C15940A4522E2A3E
          338AE8A42C06151C35EE82E71FD3909348F99637687BC83358C6434683C621D3
          E066968420CD2A7ADC5AD2A1D316C19B403AC8F75B5BB704D06E041AABB47644
          239F5CCCB9671F58BCC6AE0877B05AE391A965623EE78E3F69F0160434F7C41C
          8612E588A351072FC6D620279272848121188F5C682D357D98CEC2521239977C
          0DFE828003DD5117ADB3AFCDA4832F3101BEF33950D38EB2060D34D66861258D
          6086172091949C0501FBB808430BE3C108BB6879807624483B324CBF7CBE451C
          D04C08E0FCDAFD17FC436734AC8073EA0000000049454E44AE426082}
        Name = 'PngImage59'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001454944415478DA63FCFFFF3F032580912E0664CF7ED4FE
          EB1F43EEEC74391E920D489DF9A0FDEF3FA6F2EF3FFF302E2F506224C98084A9
          F7FAA404390A15C45919369C7ACFB0AD528D7803A226DC99272DC291A822CEC6
          F0E7DF7F86A587DF301C6BD126CE80E0DE5B4BE484D9A335A43919DE7EF9C3C0
          CECCC830F7C04B866B7D06840D08E8BCF5485A845D564B9A83E1CDE7DF0C1FBF
          FD6110E4616198B1E319C3D3D966840D706CB8BE594E94CD47478E93E1CDC7DF
          0C1F800608F3B032CCDFF684E1F9122BC20668159C610186FA0A4571CE601315
          6E8657EF7F31707332332CDBF890E1E53A07E2035132E6C84A1569DE30430D5E
          86DF7FFE33AC59F380E1F54E67D2A251CC6BEF224545DE586D2D41864DABEF31
          BC39E04E9A01202062B96581AC9A40FCE35B9F19DE1CF724DD00101032D8309B
          919925E5ED591FF20CC0072836000012139BE17CCC00550000000049454E44AE
          426082}
        Name = 'PngImage60'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001524944415478DA63FCFFFF3F03258091EA0634AC7EF9E5
          DF3F86C94DE1E295641AF0FAFF9F7FFFFEFFF8F9ABB3274E96A0211806542E7F
          FEDF448993E1D6F39F0C379F7EED5F90AD54449201790B1EFFB7D2E061F8FDEB
          3FC3A547DF182E3DF83C7F678D6612D106244D7FF4DF5E8B9BE1E3B73F0CEC2C
          8C0C27EE7C61387DFBC3D22B7D4631441910DC77E7BF9B2E3FC3F3F73F191819
          191838D918190E5EFFC470FED6FBC7CFE659C91134C0B9E5FA7F5F43418627EF
          7E3230030DE06267623874E523C3B55B9FB63C5F6EE54BD000D3F2CBFF832C84
          185E7FFACDC0C5C1C470F8E27B869B37DFAFFDFBE97BC4EBDD9E7F081AA09C77
          EE7FB49508C3EF7FFF198E5CFCC070EBE69B552F373887131D885249A7FE473B
          8A311C0769BEF676F1AB6DCE712445A344E491FFCA12C07470E5CDC2D7BBDD13
          1808000C03C4FC0EFCFFF7F9FB9C37FB3D530969C66A00A9806203003187C0E1
          8827483F0000000049454E44AE426082}
        Name = 'PngImage61'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000001874455874536F6674776172650050
          61696E742E4E45542076332E313072B22592000000D14944415478DA63FCFFFF
          3F03258071F01AC018BDED3FC38F1F401610FFF80EA1C16C90ECBF5886BF5F57
          FC3FD0F0876417303A77C73030FC5AFC7F6F3523DC05393939FFDFBF7F0FB4F0
          07C3870F1F18BE7FFF0EA6AF5DBBC688DD90D6FF280610652B87D0E2FF5B9319
          B11A505252F21FD9E6AD5BB732A26816905A0C0A0F9001102F3001BD504C9C0B
          1883D7FD470424033420BFA106624D4D0DD805A0300085C5DAB56B315DC0C0C8
          F07F6D144698101F064043FEAF8DC66E406B6B2B3C16906363EFDEBD8C040D1F
          BC49996E060000AC71B1E153B456B50000000049454E44AE426082}
        Name = 'PngImage62'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002A34944415478DA8D936B48935118C7FFEF362F9BB39586
          3A73CE444B1144CA9C4986DDD0D887B22CC118F64188F6250C8A2E60451049F8
          2589511F22942E4654A066A5E5709ADDA649E50DF2B239D7DC257137DDF69EB7
          E30431D2F2F9F2701E9EFFEF9CFF73CE61388EC372616FCFE7732CD7C711F28B
          10AE20B6A89B5DAE8F5909606BDB712B54BA5FCDB12CBCC666759CF28366D500
          2A8E22848CADDFF53292754FC0D651EE202C49DD50AC77AC0A30F52AAF499854
          A614A71C07880B33430FE11CACD32494F4AAFF0BA0E22CEA5B1F5BD8C1F3D9EE
          80E18920901C80F1E96E966349B6BCECEB977F022C2DB9DFC4E99519C2F87D18
          6A3818AC6D3AAC81DBA087FD5375E746D5F7FC15019617B96521D1DBEF47E554
          C33D5281F1AEB1605D9E9B0851F24D98DB2FC033FEF6584AC5F083BF00542C9E
          1FDC3A456D74E8DA4478C7CE62FCDD0240A6888728A112B3D3B330355798A895
          B4CDEA11D71F007363CE6351D2D1236BD2CBE1B33620E07A0F4BBF958A3C1420
          055F108A70D9554C75DF85BDE7DEF58CD3C6F38B002A4EA1831B902AB502D6DB
          0BFFD46D1002F042C2E1B63911164160EB9A844567846BE22720040706357B1B
          7D678280C9E7D91FC56927B7899395F0DBEA413CBD201C432D1882A70BF711B8
          8709520B4A10969C016FDF6BF4EB5A39C7E0E029C6F46CAB922FC96C8AD9598B
          80530BBFB53E28A2CF171C85D0BDD073A50539AA2A087F680153172091C02690
          43DFD669618C4FB28C518A9A04A154419BFD082A82E698854C976F0A63B0E7C6
          23305B0E2DDED8F4A5387CD6D9C1181A32EBE83355D1C982662CCDAC9F053BE7
          87596B455EB10A118666CC792DF0508073868F81517856FC4C4B43571A7F392C
          32E2A22C3A2010F04C705A0318B5F059FF2C57B52AC07C7496CACEB91DE6137C
          9691B37CCE44559AA2D6C0B5DF04FA65C0D8A404910000000049454E44AE4260
          82}
        Name = 'PngImage63'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000001874455874536F6674776172650050
          61696E742E4E45542076332E313072B22592000000AA4944415478DA63CC9D74
          FA3F030E3029D784918100600419D0946CC8F0F7DF7F206600D3FF80B873E925
          E20DA84F3440D12C23CCCE001427DE809A780306717E5686276F7F8235DF7BF9
          83A17FE565E20DA88CD587DB0CD20C72C9A4D5578837A03C5A0FEE05189EBAF6
          2AD880BCC967F0063223A1580019802F9019FFFFC7A91F0C4006E00B64A20CC0
          17C84479015F20134C482003F00532C18444542C0CEE84449401F8FC48C80000
          19440DF071A774310000000049454E44AE426082}
        Name = 'PngImage64'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000001874455874536F6674776172650050
          61696E742E4E45542076332E313072B225920000009C4944415478DA63CC9D74
          FA3F030E3039CF9491010FF8FFFF3F0323C880A6644386BFFFFE03310398FE07
          C49D4B2F116F407DA2018A6610BB67390906D4C41BA06806B1FB575E26DE80CA
          587D14CD203C69F515E20D288FD643D10CC253D75E051B802F9027E59A303212
          8A057C810C36808100C017C8441B802B9089F602AE40861B802F21E10B64B801
          F8121251B130B813125106E08B054206000003A83DF06CB332CB000000004945
          4E44AE426082}
        Name = 'PngImage65'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002B94944415478DA5D93DB4FD45010C6BFAECB3D0404562E
          812E2CF882C1F822411241546EA2283112E28D282ADEF0C1271FFC274C144541
          1335318AC6400C3EA00B2C4B001196885C648DD04554D004D6287B69BB754E4B
          564ADBD369CEE9F79B99337338455170E0CAFD2D645B1560090AF2157AD134D8
          4B334A70046411A2CF3D637B74DD424BE0D8E4FE86961BF463434E36CF2DFE74
          AB624D0068B000E8C1B2FB2F3C3E3F7C2B6ED81F5FE382808A8616474E76DAB6
          A2AD66985362553153079890D980E6DD141B89C87023F22BEB31617BBA0670B9
          79F668758179C8E1C4D2D20A790BB0350228FFA321C8F2EF1578298285E91E3D
          60DFA566A1F6E46EFED71F0FB292E220CB50C3669E6599060125FAF678443C7B
          DE85D9B1B77A40F9C57BC2896345FCA86B119B9312E0F57A69A3FCAA4896498C
          0D30868422212A0C2F5E76C3F5D1AA079411E0784D213FE2FA0E4B421CAA7233
          B5F0B542A8293CB04E2131260A6D6D36CC8DAF03945EB82BD45417F0A3B3F3C8
          D894441178E0132548E45D9658041C4242C3608A8E40C7AB5E7C9DE8D2034ACE
          3709470EEFE447665CC8484E534522E52D52FE2203D03747B7293A149D1D76CC
          4F76EB01C5F54D42D5A17CDEE114C0A7F2AA6791E52E6B108920A447724C38AC
          AFFBF0EDD3BA2AEC3D7747A8ACDCC18F4C7E412A9FAE8AD9EEFB57216C18E8F7
          94E87074BFE9C78F699B1EB0E7EC6DA1A2228F1F1E7722C56C5177DF2F31884C
          A9501AA24C753020756304ECD67E2C387BF580A2338D4269592EEFF8308DC48C
          2C357F6D1F08206ABD60A06AA4C54762A067108B9FD70176D5350AC525DB7987
          630AF1964C15C0F29624451DAC170C0A87745314DEF50D5227DA5C53F6567310
          5078FA9650529EC71B8D2150384EF528C9ECE4695659ED46BF5FC2F0C010E6C6
          3A1FCEBC6FAF0D020A4EDD6C2653A79DBC35FD0FF538EA4E263DED434FAE1EC4
          EAF50F19ADFEF0ED58420E0000000049454E44AE426082}
        Name = 'PngImage67'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002594944415478DAA5934D68134114C7FF8B8456C9EE2644
          A39244504490361E12E9A1341F8A164A412B7E1D44A4B6423DD44341F4D21E5A
          44100F0A8AC52205EB41A42055EAC7A107C14A2D06F221114149F46091C4C67C
          279BEC8EFB261A09A4A70E0CB3F366DEFFFDDEDB3702630CEB1902090C4E2DBB
          FF1934B50A5555F97C74C91F205BDFB5E7EE6AA5820A9F0A2A4A6D5DBA3B1410
          06EEBF27E70F3B2C9BA069E4A8E9222ACA8A82D8CFF43E72B4993786C15454AB
          55D09ED68F5F57D0D2DAB25F383FB9C8206CC064BFAB8EA5691A5E05BEE1C1C2
          A71572B8D0DDBEBDB7635703BA6BE0368C4623B880AA314C0D76201289707442
          753A9D88FF4840142538B69A110C06F959369B85C7E341FBD91B108D624DA0AC
          E7343DD48952A9541748241288C562686B6B83D56AE5D864CF6432B0D96CD873
          6A02A2A40BF4DF5B6485420133C33E84C3618EAFE8F99B4C26BE77B95C70381C
          8846A35C8408BC5E2F761E1B834402E7EEBC61D95C1E8F470EA1582CD60AA54F
          2208854270BBDD5C8044C99E4EA761B7DB61EFBD0A4996209CB9B5C032992C66
          AFF434D44096654E4002E4400464CFE572F0F97CD8767804A27E47387DF3354B
          FF4E636EF428AF0145A168C964B22901D580042DFEE11AC1F1EBF32C954AE1E5
          C4C9FA2FA2E62A97CBFC32D5C260304010046EA714884EEEBA58133832FE94AD
          FE5A6DE8B08A52FBEEEBDACB9D665E2CD51B48FD5B234A559674819ED127CC22
          B636EDF3B7EF9675C72AFCBECEA6E7CFE6E6211CB8FC90FD8FDC48B065B39947
          8AC7BF3725A09617D67A8DBB4F8C4FE7F3B983E4289BCC9FBFCC8E75AFF91AD7
          33FE0004129944E573617C0000000049454E44AE426082}
        Name = 'PngImage68'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100804000000B5FA37
          EA0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000000FC4944415478DA
          6D91BD6A02411485CFCC2E31082218888DD6F6C61F040BF31479040B7D17616B
          DBB4BE418A5426111B4BAB443BED45DDF9F1CE9D1533EA1DE6877BBE73666084
          85A8A08C08616DB0B2C61D04012FFB8FA820E445551827C391471CD04AA70729
          32C9E088276C3149068C080834D5770A09C38026A044D3E03DE98FEC6F06684C
          A97944CAE32DBB2A6E6396017411352CF935A5A4E45778FE0F00336A2BCA500C
          19BCA21802E7B22C6BDAF321B020AF773BD1A08B87FB0960D9AD71082CF97986
          E3416BFD1AF05E3F7DC910F863B71B1EA9917001BE20705B26EED81FF7178DC3
          67F408792DEB5DAE67E70EA8D277CB3B091A1BBB3E012E688B13C4F22AE80000
          000049454E44AE426082}
        Name = 'PngImage66'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000003764944415478DA55936D6C535518C7FFB7EDA55DD7EEA5
          69479BAE2373B8C0C690340803E60C084C9630612A44E31842982F71BE46FC62
          A209066362FC22C1C444191F0431F261E240948D652D939139DEC6325760525B
          DA6EC2D6BBF5EEF6DEF3E2D9E2089EE49F7372F23CBFE725CF2371CEF1F07933
          FCF213E26A157A5CC82364114A0BF571C60F7F59DBF6CBC3F6D23C4038CE1AB6
          093DB7B564A3D595E3415ACD85AA03846A2048A1F3EE194209FD8D10BAE3DBFA
          13D30F00C25916EFF32B3D95EBEA021BA0685664A123A24858C0ADB8A73270A2
          23071924B221F4C7FBAF1183D41EDFD19E9E077C177497BFF8A47F0D34D8C069
          1E74DDC0902243365B119FE0300C02B3A6A0D43385A8D2899EC89533279B4ED7
          4BADA1DD35227AD7FB2B9AE5AB24034D0272A90350DD886764640D09C377A600
          A663A1EF1E8ADCA35869F3E3A3535F5191C5B3B3809375FED58D5E6700231602
          13F7A194D8108A5BC0880591E43424DB7DD8FDB761B225C19886E0F44248DA0C
          8EF476FC3A0B88EE5BDA18B8AC26316E33A1C25C8D424A706C8043D328F27D1C
          8AEB07DC6593220B8E02624569C28A8DA5417CF8E3A1C42C20F3565593FD5832
          8431B38A2AFBD370EA79387D9121DF338345252A6EB0FBB00D1C06D532180F2C
          41F1841D7BD73E83D7BF393023BDD1D3ACBE53B52BE748B40313082098BF0CDD
          BDA207390E64520AFC6580A32885B12B9F2350B80843F10114E406F1EAFA9D68
          F9FA634D7AADAB29D652D9E8EF1EBF0EDD56036DC282C1213B7CE6B745BD143A
          33A01302BF6B3196076A7069B40BD7627DD85FFF19F61FFD2225ED3BFBC24F5B
          4AAAB7CAD63C9C4BC4047D316E0E0690CFDEC5A665CDA09C810A909804242763
          287414E1F75BE770F9EF300CE5D16E69F7A9E7371383767C50B3C7D276F56750
          4906C96E839A7A0FF58FEDC59D7F22301801119918D480C9B40045CE62846F9F
          45DFE8053237483BBF6F685FE55DD250EE2DC31FC9BFC04C4FE1D69FAD6858F1
          8A70A46294E95C268974142E875744EF45E7F0795523BA7B0ED078748BD3D069
          784D71C5F20DC15A0C8FB9D01EDE034DD7450F7468C440997B29AA1FD98CFE68
          2FBA477AB2765A5C3E78F066F4C132D51D5AEF1493754294B369D73AB158D40B
          CEED904C12DC050A0E74BC844ADF6A84462E4CE5F240C5F58391D8FFB671FEAC
          FD64D576B1712D94B22A4659E17FDFE949F30D376190671895C73FE564DEFE5F
          67A6B969C4CDA1940000000049454E44AE426082}
        Name = 'PngImage69'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B225920000026F4944415478DAA5934B4854511CC6BFA333737D106EBA62
          8BC9B251A7415C85B41C5F9B1682A4D820A208B3312817BA50C2858BDAB40942
          10716106669A39A6888F99828A2132B01A32311782E578CFE48319AF0EF3B8FD
          EF11C2B94A9B0EFC39E7C2F97EF7FB7FE71CA6691AFE67B09380C54526D1670D
          952B99849DE642AA24AD0334FFA07990CA5B5BAB254E0148ECA0E5486E6E4E69
          5E5E17B2B3AF232BEB1A342D86FDFD570887BDD8D81802E77847FB6ED5D5693F
          5300F3F3ECB5DDEE765AAD8FC198E54CBBF13847207009ABABEA938606AD3905
          3037C714A7734DB6582EFE13B0BBFB8CDCDEE12E97969B02989D654A79F93779
          6FEF053232AE42920A61365F102DA8EA1276769E6273730C365B0F7CBE5EDED8
          6800CCCC30A5A2E2B37C70E017FD1E1DAD08713C1EA20C7E231A054C26A0A8A8
          0B5EEF03DED464004C4FEB8025391A5D13625D68329D472CB68550680091C871
          1B050577A98547BCA5C500989A624A65E57B59553FE1F0F0ABE8D762C927C02F
          4A7E8C5C016969A016DAB0B0D0C75B5B0D80C949A65455BD912391B7D021BA03
          49BA221C0483F3C2417A3A505CECA6C007B8DB6D004C4C30A5BA7A81427C2900
          89C41E016C02B4B5F54138D03370389A29AF21DED666008C8FEB008FACA7AD03
          9249159999A5A285603040411E3B2829A987C733C6DBDB0D80D151A69495D5CB
          B27C9B04F729872F7F01DBDB01BA9580D5DA83F5F55EF8FDE09D9D06C0C808BB
          41F7FCB9C361CECECF1F14FD3326D15146450EE1B00F2B2B7D585E46301E474D
          77B7F6F1D4631A1E669709728FEA26259EA3FF9536D325021DAD100E27127848
          62E5CCD77872F4F7B37324B0512563317CEFE8202B678C3F3ADA7DF021D33179
          0000000049454E44AE426082}
        Name = 'PngImage71'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B225920000024E4944415478DAA5937F48535114C7BF6F03332A8430A218
          E8CC18FDA08298FD31888D7E50081645E5CA2C8AB56ADAA230CB988C6654D00F
          C4509098108561148C26B1357FF43B94FE5022A18C8DCAAC19F5C78AF6DE7DF7
          DDEE7B6F380403C3038773EFFBF139DF73EF3902630CD331410578DBBED42B0C
          2EA25013A5148402128F3251D70A241E45EE92CC9D101089F1E772FF60A3AD44
          0378822363811DF9F9FF93794DED4BF17DB33D5703B85B3FB20BCEF968E8F340
          A422CF22214D259E35ADC5B41A6589ABD2DFC5760F60A5F7093E0537081AE040
          4B825DAE5880B973664C29FB8F9408CB916E8CDDDAAC03F6367D608DFB4D387F
          FFF3940067B799507030825447990ED875F51D6B711568802BFB16E1F4CD615C
          AC2C1EFFC1D73E8C8033BB4F245358EC8A8284B6EB802D9786D88DC3665C0B8F
          C0A87E613040800203DFA87B8932E41805805F552EAFD2B12C0FD6EA2EB048B9
          0E286D78C3DAAA8BD11C19857FA719818E387C3CFE4BC18BA1246C471F81F5EC
          D101EBFC03ECF6310B823DA36AF20996C325FCFC4DF53517A1AE1C4B67C3EE8D
          7240A50EB0D5BD66774F2EC19DE7DF70A2CC8CEB9D095495168E439A1EC451BE
          7621E6E5E9B7F478F02BD6D7F582843325584FF5B150ED72DC7B95E4B54F3423
          57F44B9CD8EE25453361AF51CF2053C28AE34FD943DF6A84FB93706F2A446B34
          81431BB30AEADBE338E7CC9E89A6E04C1748670660F1F4B298DF0A99F7BD4894
          4CCF336D06D45998CC1C353145895418354091BBFB99949657FD2174565A2210
          B953911F1707F1C999BC9BBEA742EC6DD55661BAE3FC17255D45F09208A98F00
          00000049454E44AE426082}
        Name = 'PngImage70'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002904944415478DAA5935F48537114C7BFD3C2708833166C
          5946131F9A2C48A324F2210413FFB0827C287B8851200AD98B60B084C44842F3
          C9088C121F6284B025CCAC494CB29136D6A61284C5869BC8B2627AE7BC6EBBFB
          757E97D4E8F610F483C3E1DE73BF9FF3E7FE8E8A3186FF39AA3F013E9FEF493A
          9DBE9C4C267791472A95928D9EB9B73536365EFC2B201008E493E09146A3B990
          93938B4D122193811CA56F0A0A34181F1F435353934A01F0FBFD355CACD56A0B
          D56A3582A125CA96263D43866508C4A0D3EFC3DBA949582C1625C0EBF5B2A2A2
          22F985286E623112C55CC04B80CCB6559FADC5A47B02CDCDCD4A80C7E36146A3
          116B6B6BE07D47965678D5945802A3EC12010AF7EBE07A3586B6B63625C0ED76
          3393C984783C4E0009E1C85704FC33945992B34B9284BAFA7318733E477B7BBB
          12E072B9585959190441A069A7E40A78EFECD70CB83F50A883C33102ABD5AA04
          389D4E56515121B7C0017C065CC4632BF9418C861CB096F660E4D95374757529
          0176BB9D555656CA00514C22BC14DDCEDE1BBD8575218D964316F84293D848AD
          4348AC0AB1F88F7B2F7A66BB6580CD6663555555720B890D519E01E3D3A718F7
          9FBEBFC77AEE02CA8DC771706F095ECF3BF06EFE0D429FBFF5C980E1E1616636
          9B118BC5A88A3816C35401DB0250851FEFE37C7D0D909D05F391EBE89DB8866C
          64C1661F1565C0E0E0E0B44EA73B613018A056E7E14B308C44428444BF94AE11
          1ECF74A0E5522B6A4BAF6EF73E3A37803B0FBB77AE727F7F7F1DDDF5DB7ABDBE
          FCB0A1181A8D56DE011EBF31D48086BA33E0B89BD543B8FBF20AF664E7EC54F0
          FBE9ECECAC26612F99696B99C29979EC2E8EE2D4D1D328D11FC3C2F2077866A7
          1009AEF6A9FE759D4FB6EA7BC8B590E59109640FA607963B7E0270DBAE9CA9CB
          A1C30000000049454E44AE426082}
        Name = 'PngImage72'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100804000000B5FA37
          EA0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000000664944415478DA
          63FCCF801F300E290507FFFF62F8C9F083E13BC337866FDF0BB93014ECF8CACC
          C5CAF00B085F323CBED06088A1608DC4F73B6CDC9C0C2F181E7EFFAEDD771F8B
          1BA6487CBBCFCDF1E8FB778D498F7038B25AE2FBE51FC6D390A4075940E10200
          4C6934019470C4260000000049454E44AE426082}
        Name = 'PngImage73'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001254944415478DA63FCFFFF3F03258071D400FC0648251D7B
          F4FF3BC3C53F6F3E06BEDEEDF9872403748ACE2D9117E38CFEF1FD2FC3D52B6F
          D6BE58EB1042B401EE2DD7E74909B3276A4AB1337CFFF58F61DFF9770C37AEBD
          59F572837338410312A6DEEB13E5672B5411676378F1E117C3B79F7F19585919
          190E9E7BCF70F3F29BC5AFB639C7E134A064D1E376613EB60A19215686F75F7F
          33BCFBFC9BE10BD0807FFFFE33B0030D390C32E4E2CB85AF77BB27601850B7F2
          65FB3F068672430576C6DF40B19F3FFF31FC003AFFFBAFBF100C34E8FBEF7F0C
          C7CEBD65B879E1D59C37FB3D53510C6858FDF20B030313F7CF3F7F18BEFEFCC3
          F0E5FB7F868FDF7F317CF8F69BE1D3E73F0CEFBEFF66F8FEE50FC33FA0D8BFAF
          7F185E6D7260A47D3A181A0600008212BEE1A428F7270000000049454E44AE42
          6082}
        Name = 'PngImage75'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001224944415478DA63FCFFFF3F032580711819D0B0FAE51706
          0626EE9F7FFE307CFDF987E1CBF7FF0C1FBFFF62F8F0ED37C3A7CF7F18DE7DFF
          CDF0FDCB1F867F40B17F5FFF30BCDAE4C0886240DDCA97EDFF1818CA0D15D819
          7F03C57EFEFCC7F0E3D73F86EFBFFE42F04F20FEFD8FE1D8B9B70C372FBC9AF3
          66BF672A86174A163D6E17E663AB9011626578FFF537C3BBCFBF19BE0035FEFB
          F79F819D9591E1F0B9F70C372FBE5CF87AB77B02CE3048987AAF4F949FAD5045
          9C8DE1C5875F0CDF8006B002351F0469BEFC66F1AB6DCE710403D1BDE5FA3C29
          61F6444D2976A0F3FF31EC3BFF8EE1C6B537AB5E6E700E273A16748ACE2D9117
          E38CFEF1FD2FC3D52B6FD6BE58EB104272344A251D7BF4FF3BC3C53F6F3E06BE
          DEEDF987B6E960041B0000CAD0BEE161DA12680000000049454E44AE426082}
        Name = 'PngImage74'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001454944415478DA63FCFFFF3F032580912E0664CF7ED4FE
          EB1F43EEEC74391E920D489DF9A0FDEF3FA6F2EF3FFF302E2F506224C98084A9
          F7FAA404390A15C45919369C7ACFB0AD528D7803A226DC99272DC291A822CEC6
          F0E7DF7F86A587DF301C6BD126CE80E0DE5B4BE484D9A335A43919DE7EF9C3C0
          CECCC830F7C04B866B7D06840D08E8BCF5485A845D564B9A83E1CDE7DF0C1FBF
          FD6110E4616198B1E319C3D3D966840D706CB8BE594E94CD47478E93E1CDC7DF
          0C1F800608F3B032CCDFF684E1F9122BC20668159C610186FA0A4571CE601315
          6E8657EF7F31707332332CDBF890E1E53A07E2035132E6C84A1569DE30430D5E
          86DF7FFE33AC59F380E1F54E67D2A251CC6BEF224545DE586D2D41864DABEF31
          BC39E04E9A01202062B96581AC9A40FCE35B9F19DE1CF724DD00101032D8309B
          919925E5ED591FF20CC0072836000012139BE17CCC00550000000049454E44AE
          426082}
        Name = 'PngImage79'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001524944415478DA63FCFFFF3F03258091EA0634AC7EF9E5
          DF3F86C94DE1E295641AF0FAFF9F7FFFFEFFF8F9ABB3274E96A0211806542E7F
          FEDF448993E1D6F39F0C379F7EED5F90AD54449201790B1EFFB7D2E061F8FDEB
          3FC3A547DF182E3DF83C7F678D6612D106244D7FF4DF5E8B9BE1E3B73F0CEC2C
          8C0C27EE7C61387DFBC3D22B7D4631441910DC77E7BF9B2E3FC3F3F73F191819
          191838D918190E5EFFC470FED6FBC7CFE659C91134C0B9E5FA7F5F43418627EF
          7E3230030DE06267623874E523C3B55B9FB63C5F6EE54BD000D3F2CBFF832C84
          185E7FFACDC0C5C1C470F8E27B869B37DFAFFDFBE97BC4EBDD9E7F081AA09C77
          EE7FB49508C3EF7FFF198E5CFCC070EBE69B552F373887131D885249A7FE473B
          8A311C0769BEF676F1AB6DCE712445A344E491FFCA12C07470E5CDC2D7BBDD13
          1808000C03C4FC0EFCFFF7F9FB9C37FB3D530969C66A00A9806203003187C0E1
          8827483F0000000049454E44AE426082}
        Name = 'PngImage76'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001744944415478DA6364A01030D2D480AC590FDB7FFD63C89D
          9321CF838D8FD7809419F7DBFFFE632AFFFEF30FE38A426546743E5E03E2A7DC
          ED9312E42854106765D870EA3D83181F4B3F327F7B953A6E0322FB6FCF9316E1
          4854116763F8F3EF3FC3D2C36F18AC35F81990F9C75B75B01B10D47373899C30
          7BB4863427C3DB2F7F18D8991919E61E78C990EF2985C2BFDE6F8869807FC7CD
          47D222ECB25AD21C0C6F3EFF66F8F8ED0F83200F0BC38C1DCF18D2DCA450F8CF
          E698631AE0507F6DB39C289B8F8E1C27C39B8FBF193E003508F3B032CCDFF684
          21CE431A85FF62A935A6019AF9A75980A1BC42519C33D844859BE1D5FB5F0CDC
          9CCC0CCB363E64480D5742E1BF5AEF883B1025A20FAF5491E60D33D4E065F8FD
          E73FC39A350F18340C441890F96F76B9E08F4651CF3D8B14157963B5B5041936
          ADBEC7C0C4C5B21899FFF6A0077E034040D862F302593581F8C7B73E33BC3DE1
          C588CE272A290BEAAF9FCDC8CC92F2EE9C2F23363E41038801141B00002368B2
          11A3306C600000000049454E44AE426082}
        Name = 'PngImage77'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001804944415478DA6364A0103052D580FA552FBEFCFBC730B9
          3942A2121B9F08035EFDFFF3EFDFFF1F3F7F75F6C6CB55A2F3091A50B1ECD97F
          13254E865BCF7F32DC7CFAB55F5288B31099BF3047B908AF01B9F31FFDB7D2E0
          61F8FDEB3FC3A547DF187EFEF9CF80CCBFF4E0F3FC5DB55A49380D489CF6F0BF
          BD1637C3C76F7F18D85918194EDEF9C180CC3F71E70BC3E9DB1F965EED378EC1
          6A4050EFEDFF6EBAFC0CCFDFFF646004CA5C7AFC830199CFC9C6C870F0FA2786
          F3B7DE3F7E3EDF5A0EC300A7E66BFF7D0D05199EBCFBC9C00C9439F3E01B0332
          9F8B9D89E1D0958F0CD76E7DDAF26285B52F8601266597FE07590831BCFEF49B
          818B838961E7B98F0CC8FCC317DF33DCBCF97EEDDF4FDF23DEECF1FA83618052
          EED9FFD156220CBFFFFD673872F103C3B3EF7F1990F9B76EBE59F56AA34B38CE
          40944C3CF93FDA518CE13848F1B5B78B5924786391F9AFB7BBC4E18D46F188C3
          FF952580F17EE5CDC2377B3C12D0F9041392A8EFFEFFFF3E7F9FF3F680572A36
          3E4103C801141B00005934EC113B30569C0000000049454E44AE426082}
        Name = 'PngImage78'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000027F4944415478DAA5937F48535114C7BF0F294229CB36C5
          F08F22B4C0CA247FA0AE648CD60FFAB1440B04E9071A29943475E00C830A2358
          84201698C6DC4873520D244B5A6B250589F4C79C33B52152663F5D85D0F6DE7D
          B7FBDE9B492041781F87EF39E7F13EE79E77CFE528A558CCE22480ED59B09E49
          19A16292288A202220484A249F42603A6F0444A02C2F0E34942467C900AB67E6
          7361F672D5FF543EDF391EB21CDBB84C06B4B9BFD22339B188E92E07A4968800
          8804E079C50F87981F0642BC9C9BAD75C36C7B83C6B2544E06B4B8BED0E2BC95
          0C700A4854B38F05C5A8108148006602830C7FC06C9D0735B786D15CBE4501DC
          E8FB444B76C4CDEF808A8AB2FF20EF84CC2991DFCD9676A1B2C58B9BA7D31540
          53EF343DAE55C139F00DC5DBE3FFD9BBE3C514B49BE250D53A04EBD90C0570AD
          678A96EAD4E87D3D83C3B9F1B8E775A03FE041594E05FC1F7D708DF64197A2C7
          A1CD45B07BDE236FC30A98DB87D161CA560016E73B7A529F80C7DE200AB2D472
          A572C70954E657E379E0298E6696A2B6C788AB079BD0EA9A84262516E76C3E38
          CCB90AE0F2DD495AB12B111E7F100732FE063CF23F90558AAF17B5A1F9E10474
          A9AB5067F3A2DBAC510017BA26E899BD6BF072EC3BF6A4ABE5AAB707DBA1599F
          8FE825D1708FB9A04DD6C92D34F604B03B6D351ABA4660AD8AB450DF19A0C67D
          4978F5F607F4692AC8E3CD4546953D52CC714AC2727F1CFBB7A971B1C307BB29
          D282D93E4A6B0C6B31C800D34141393596E7A5596247176689300BC2BC88A551
          0486CC0434DCF1C15A1D01D45847686DC1BAC8DCCFCD3EFD7317165A973A8644
          9B292F4A0618DBFCFD024FB6B24A31BC4020191144565D02F10B02823F7F399F
          5CD969E0167B9D7F03EBEF86F007A58FFC0000000049454E44AE426082}
        Name = 'PngImage80'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000002734944415478DA9D935D48536118C7FFEFC7391E75C2B495
          6B6892267395F6A5E9D5B0CDAF14B3948CA5E545119ADD64105E89D445175D44
          1225D455CCAE7252AD2B91DC50A288928261456966B2A165562673EEAC7792F9
          B1D0E80FCF792ECEFBFF9D3FCFFB1C0221B3D95C2F9A1E91F2BADDEE76AC22F2
          1BD02A0EB62E7DD1D13F4E5D9DD79E941C6BD24B9C1938A3608C8D71469C94D2
          C6FC54AE460016935062CA2DAA9B99FC9492B56327D625C4C3BABF1C732AC1E4
          CF10BEFA898B326AD927202B01F3BDEDE1D08D54435CFDEBFE2E582B6AE0BC6B
          4769552D1489202E8AC1F7431510DA5E902E37FC1570B9EBEDC7C23D86246767
          07F20FD4E0D13D3B8A2B6B452E82189940E1146F26D4D1125374F202A0593445
          54BEA8DEB49CB216534A02F1AB0CE6B26AF4DEEF40D1A1300060E2B15EC3F072
          2CA096676A18593AB885042DF657C1C39674EA9B0A8050C0FDE00EAC076BC044
          02314B24C6713C1F9951AB766B2300F34992B20ACE99523768826244736A0892
          24CF2709278895A92882810FD3A3B63C5D325979AFDD174B8D1E6DE540ACD1AA
          ECDDAAC7E44C10FE4008547C5923CCF1310CEFBCD318FF1668AF33EB1B96011C
          E7F38CB2ACF46DDE55A8EB9EB52036418FCC2D3A442B1CC28FD93915C3DEEFF0
          7EF1BB2489594E1524A97F00B74F1B8D92A4F419B38B74868C1C8C7A9EA2672A
          7368227ABB2C166923971864CEC684D12971DED85491B6B84861DD3AB9697C5B
          76B12E392317C39EC7981819748834B6D20B3DB36BAE7258D78F27868AABCFE2
          FDE033F8863D0E498AB21DB9F26255F33240DBD1F84B311A6D7330E077701E65
          3B7173684DF3324058576D5A83F8617C67EC9F83FF628E00FC8F7E01EC32D911
          8AC559EE0000000049454E44AE426082}
        Name = 'PngImage81'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B225920000027A4944415478DA9D925D48935118C7FFEFFB9E73DEC9E6F2
          63EA267E207E2C2C0CA7A5239134210AB4A40C464A44E447422AD3B48B6A5031
          BA88B41025BBA88BEA46F4422FEC261416E20253288428D234CD6609BB70CBB6
          D7CE24F363A1D11F1E1E0ECFF3FCCEFF7C08E0D26AB5D53CE911AC2F6EB7BB13
          DB48F80DB0F146DBC6424DC784F8E241E54849E353BD2CB358462510CA661915
          FB25426A1B0A544A1060DD89201852F79F5D76CF25A66764223A3A1A958D3678
          7D22A6BF2BDC963824315268E590AD80D55C7A7DB0636F9AA1FA75EF0D9C696A
          45D7AD3A9CBBD2066D88087D28C3C4571FE6DDA4B3F988A6E6AF8063979F4F57
          949AE2BAECF53859D78AEED63A9437B5411205846B44686502C77BDFCCD5E288
          F835400B4F2A1E87780C46A598AFA52646093F1482E3176FA3F77E032C560EE0
          DD94084889611818F728374FC548C2C68B5B739057D5EDAFAF2A123F7CF64090
          80BE762BCAEAEF828A2224BE4E33C8E8732E2A772A128300AB4EC2124CD6E424
          BDC6AF88F0F91550A642097712E80E534B88E4C71870BA66DA2B8DF1C2D677B5
          17E88C231147C742B3CEAB8A8B3230BBF8134B5E6575E77035415C2485F3AD0B
          332E4FE7C34B19359B004DD96AA38A1187C99CAD7BE42D43A86137F2CD466877
          C920BCBEE4F5637C620E9FE6DC434C66854F9A0F287F0017D2A99111C9919BBB
          47B7CF9C87D1970E3CFB96F971419DCF28A506C22818A3B394D17EC6E4DA3EFB
          E1F58F145045AAE43A9895AC33E5E4E0D5F030A627E77BB81B8BCDB1B8BCE357
          0EE87492B852555E04E7E81B4C4E2DF4C844B2DC1BF36C3BBC09509A20D83521
          528BDFA7F45049B43C7EE7DB71781320A013F18825A230DF3DB5E2FF97E120C0
          FFE817C6E7D911F90566F20000000049454E44AE426082}
        Name = 'PngImage82'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002BB4944415478DAA5937D48535118C69FE3BDD3399D2B3F
          5033518B205311D3283123C9BE3441162223350B4C0D4221C2024D3052214128
          D344321035C5A83F4A324711A810E20A4D532433A2A1B64DE7749BDEDD7B4FD7
          09129416740EE78FF39EF7FDF19EE73987504AF13F83FC0970F9806FD96E1F5A
          C230A20727502C712EB62F8B4C6D9BCE50FA57407EAC4F497410AD92290811A5
          3DE7A058B68BF8B14CE9A45976AB7BC458BE25A021751B952B19182C0204B216
          7181481898AD22662C82A55567566D09A83AA2A4319A2244857AE1BB7E066DAD
          AD605916ABC41D0E9BD9713831419953FD7C755340534E8829FB8ED69BFBFC1A
          CB363B8CF38BF0F2544015711C3DD51996C84327FDA272EBB94D0177CFEDEBCF
          2E2E4DE05D64F064F9B5142CF30C58C2A3ABAE5277A9F963DCA657F04F6D9379
          2D0E3CAC8819CC56E71680CA7D9C71EAB0E1656B2DCA07E39ECEBAC56619B479
          F6DF007E298FDDA8285C9423A0FE55533C461F6840B17E46A4B923AB1D9AA241
          D8F9E962024793F14D816D03E07BBA937002CE6F770D7AD4D3180B1DDCF1DE0C
          D8AD928D521A2FADA4C9FB08D3D581D34FC1CAB82EB882AB49D172954E40C4D9
          E6F440627C967FED2A4CEE0CC66C140EA9495E928A5B01A286EB916CE9407852
          3ADC7645C03EDC8BB13EADB8303151EC047C28DDCBEF3C98CE741A1230149806
          46EA5CE4A913C04B86A9BBF6E04C5E21DCA7DE02FA0140A582910D81AEB77FDA
          0918290B17831335A4F0493022333231BEA800950002B7DE4561BB1B8ED57480
          EC576F086E2E0FC0509F893A01A32D576E70E33D15F7662F6094C6B3919AA320
          BF5895D6188A13A9C9F0F8D68D55FB1CD6D45BB230189F827EC385AF1D85CAE6
          9120D7964FD1B7A9D994CAC814DE20D2631029D4F21744EDFF8E0DF3E5C0BAE8
          B164E0313DC7088E157A93FCEB77EECF0CBE6E9D9FC96704122230542F55359C
          D2F2953F01332057A0274B93790000000049454E44AE426082}
        Name = 'PngImage83'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002E54944415478DA85D26948545118C6F1E7DED16B5E7574
          5CA6C646BDEE6B4AA399669B0C894B64995BAB186529ED460B6520D69720D428
          0D4949A1144A8B8C2025D25033D14CAD68DCD351C975CC1967BCCE3099906BE8
          F9780EEFEFC3FFBC8456ABC54AE746A4657CACD82083C79DE4B2D394BAA29128
          78518BC4579F3AD9BFEFC44AC0B950A1F8C201DD328AA32217DE9756D399271F
          749C5F15C88DB3B817126E7386A037C284E70299B4045AB60763324AE69ED8C1
          5B15C88A34B91B915274919677823634804AA1C038CDA0B5F47A77604AA3EDAA
          40446874C2ED83C28742CF2D84014542A521F17BA80739792525370B2AF62F03
          12021DE3BD18AB0C8D5ACBD5E190EA16E560932BB3DD33DA778DAE5A870B0252
          C8E513E0186E8062B81284AA2F6E0E880B7010FB3A3165D3AC665130AE8F2714
          B405F87A12ECB0B684BEC53618DB8A20EBAAC1F7B78F06E780F418519581A975
          80F95A33B83A09F0B9418291C151503401AB784BF8711918717D30DA2E81B1B9
          3D28233EBEBECBD7CC01F763EC9F059FBA15C9A7C616056B294F836FA8182676
          C1981A28827284C0E08F494CA858F0D8F1F906FF0B36D6FF01B4400FC60EE150
          F56583A4D49896DBA0B7AA0E3972535C319C9C0738DE99D73283DA53237DA8D9
          60F2F15698B809C1F7D837339C0552570D76C2167D95B5B823530E987707E524
          F94BDA660132A0E8F85E3FA7F4B39745863595C5A087EAB1CB6E1AAE3BF780FD
          950F82C342352EC450659D52A354B8395FAAEDFE17996042B2A37C1C858F8F24
          86D2CD5A12930D85386CDA0235C187995935787C6A66D80AD2F7F54A3D8DDCC3
          39B9B673E12F11CD69DE6AD6258EF394138561FD75086B12233A290F92C264F4
          75D740CFDA0B53232AD818EBC3FEF41B62E9B2115D19DBFBEBE52241F1542C8C
          B66E86F0B927AE1E4B00480DBE94BF44FB4F297AB91138EAFC0D8213AF970392
          8204FFF6AEA1C0D496085FCD7A9770A6310C22C61C9B5C19F442809C8FDED243
          8E0DB9BBDD156DB6F1854F96027F00878B3EF0E6507EDA0000000049454E44AE
          426082}
        Name = 'PngImage84'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002F44944415478DA85D36B4853611807F0FF3947CF6CCBA1
          7631F3D2BACC3463E214AF219921398A85792B0813CA72D1F54324A520157D30
          49A39504450A85146A2818DABD4C142D4B07619A96985AA673B9CBD9D9393B1D
          0D6612E50BEF97F7E1FDF1F07FDE97100401FF5B67D357E66627CBCABCE55639
          EBA0B9E75D44555D1BF2EBDB07D8993AF13FE0982620F9C46EF7669A62C83FCF
          1B5E4BCB0FEA3F1D5F10B899B3EC4AAA76D511421A012FEF104C0DD742608760
          9CA2A7C2F23F792F085C4BF72A4D2BAC3E29350F40BA5806C6628149AAC0C786
          339F930ABB562F08A46932F32EEC09A80850C513329A04C393F8393E841BB76A
          6B8BAA9EEFFA0BC84B52E6862B02CB784E90BB5124D763FBFE3E5491A8CA8CF6
          70E7DCE4702338F48F18A17BF3ECA1C48BC8E82831585C404EC2BAE4E86045B3
          83E5E705268F52C1225D06ABC30E2BC340AD0987E1DB00AA9FDDA9A42C82CE05
          5CCE52B7C87C821296FA2E4168B01FDEBEE9C5C4F749D05202125D2C18A70336
          81050B026B656178FDE11DDA9BDAAEBB80AB596BEF6F3B743E7D396D9C17D8CB
          7B2518CB88012BF06045841109A71318EBB4A208937319FC2BB0CA572F21DBA1
          C4F89011A6AF3F61B5596066A6C58E28E87D25730015595E509ED25F9C1E45CF
          0BECD0130FC46F1D82BF8F0C911BA210E8A3C453C303B4195E811E7334CE0264
          42F5FE9DB1C1978F9E522F6E7D51039B990563B6637D5C367ADB273062CAE533
          B51A0A14096DE8515C7A7C00144854D7D5338422F57A469432E0F6DE7C8DB45B
          20F1C50CD82D100171C6D3404210D0DC1801DD9EC3D084ED774DA7BE478F0B15
          E741749F8BE4D8901CEA2E95811F8B56883705703680B50B606CE24439278C7D
          6BA0DDBE051C9C2848B98D8B4DFBE041497E7730589638D26956FBD5D8B3E1B9
          29060E1EE0ED80D8CCEC761806418E1E031BF811F1AA4D50FA45A06FB40BADDD
          2D181E349512BD557971FD83E349C53D69D1BC7F88D6532E9979A0989995D3C9
          61F86DDFE7E31B1F5576502DEA5EB369B358F414F7F4CC5769D78F9EFE057D5D
          68D5293A9D4C0000000049454E44AE426082}
        Name = 'PngImage85'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002F54944415478DA85D2694814510007F0FFCC6C63CDEA7A
          9465B6E576686B96D26A9A6D2422842D58616B59102694A550991F82300DE9F8
          6291466504451A8514292411599179A179759862B96E8996B7967BCCCEB1D3E8
          074D227DF0BEBCE3C7E3FF7F842449986D9C35FA2627C628F33C553615C7D342
          450B51545A87D4A7F55DDCC43E311B70D2A08E39B57F5E394DB1E4DFEB65354C
          FED11BA6F439813B49DED776ECF23B4E301BE1E1A9C5584F0924AE1BA363F458
          50AAC9734EE0A6D1E34A7C56710663E902E3AA046BB5E217A3C197B2CC6FD159
          2D2BE704E20D7B532E1E50DF52076F219434095624F17BB01BB7EF9694641755
          ECF9074889F64F0ED12CCF130549A5A048E1937DE043A0665BF0DEF0F9F30485
          0A0A4240E78F51A435BD79EEE2412434E4B65AA78024FD9A98F0004D39CF8933
          02538505C3CA78C3C63B606359E80C2168EDEF42F19B078594554A9B02AEEED3
          552BBD56E8172D5988C080A5686EEAC0F0C0086886804BDA66B04E1E76890307
          02AB9541A8697F8FFA17750553C0F57DAB1FC71EBB605C4C8FCE08ACF2512EFA
          1222C0492238196165C2E904FA1A6DC8C6C87406FF0BACB0AA12CA387F2CAC6D
          956713C89F83B0BA2BD1E6BB128742964D035468FE99FCED9D39C6307A4660C7
          5ECFC791A00644593868A38D70591504FB87727CAE7A25F1B65FF72701525F7C
          78F7E680AB274EEB5C6BDF3E815D3ECC5A1C581B99888EFA616C2ADD301E9791
          E9B6C05401F4D600EEEE1852F8A1A9BCDA4C6876142484F9ABEF1D4C35301F25
          12DF2D80C30A19903B1E07F42B80755934622E1783D0C54FB53376CE078D55C3
          12F1F17CA8C06993A88754028616F8C83725087680734860ED72A38213A9AF7C
          10B5330ECAEE6770D8FB619381F1DF14DA4DE825CC79DB7E345A744B9F3812E1
          B63502BC08880E407ECCE4E45BCD30984E21106D58EEC54341F6627C5080B99F
          127956CA263A8A52223BCD83D1399FE2C3C565DA5D6E2A97890F8A89AE9C4E01
          3DCD5FBFA5AF7F5918C9BED30E77B4455022E1275252AF1C7D41EC4BE1D21F0B
          486EE742C9B8830000000049454E44AE426082}
        Name = 'PngImage86'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100804000000B5FA37
          EA0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000000744944415478DA
          63FCCF801F30D251C1D1BDBF9D7E33FC6200E15FFB629C3114ECFBCF0AD77583
          21951143C1D6FF7C0CAF198218D63188329C63C8C754B0F6BF18832D987598E1
          284305A68225FFE5E126EC6568C05430FBBF06DC0D5B183A31154C5CF72B10E2
          87DF0CBFD74F081A8890245B01004AB13601E90325CA0000000049454E44AE42
          6082}
        Name = 'PngImage87'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100804000000B5FA37
          EA0000000467414D410000AFC837058AE90000001974455874536F6674776172
          650041646F626520496D616765526561647971C9653C000000714944415478DA
          63FCCF801F30D251C1D1BDBF9D7E33FC6200E15FFB629C3114ECFBCF0AD77583
          21951143C1D6FF7C0CAFC12C5186730CF9980AD6FE1763B005B30E331C65A8C0
          54B0E4BF3CDC84BD0C0D980A66FFD780BB610B4327A68289EB7E0542FCF09BE1
          F7FA0941031192642B0000194B34014AEFE10F0000000049454E44AE426082}
        Name = 'PngImage88'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000021E4944415478DAADD35D4853611807F0FF62E9665B4C36
          D4256E95321BAD58269645E10A8B8637251458D20779154541B08BBAE84211E9
          8308BA88858944628BCAC25D18D5B1A44D842114AC09D1DC68763C3BD3D9666E
          E7E3EDB00B2978BBC99EBB075E7E0F7FDEE7511142B09252FD57E09A8F2B34B2
          2C2327C94C4F9BD9F5FBE3D69B53DD9288F3CF3D36DD5F0096D4D79420B52022
          F435C3DC3E55B50CB4F444BA65A83C824054AFAED6AAA880E7D10C69B4958057
          80E0972CE3EDB014007757F896B5427BA9B65C83C1008740D7163A70B12F4E76
          DBF5E0E6057C8864998717D6BB0E76867BAD65DAD3F64A0D6489E0C13B169F6E
          38E9408737469AEC3ACC2AC0FB709A99CB0ADFAACBB5C71D162D521911C5EA55
          B8F78645EC6E1D1D3871274AF66DD5212FC8F08DF339A7654D7165E96A701901
          0B5911469D1AF7476690E8DD41078E5C9F22079C062C2E49F005937C7DB5CE68
          361481FB2120BD28C2A400FDFE04BE0FECA203CD9D11D25267402295C3F0646A
          54999AB455685A1B6C7AB0F379E8356A0CBC9C06FBB4890E345EF9480E371811
          4FE630149A6394AC2E73DBD8604D95FEE8B64D7A8822C1936751CCFAF7D301C7
          E549726CA709316E09C3133CA3642D7C6399FB75FFC60D6BDB373B0C187A1C45
          F26D331DB09C0B91F63D26C4D99F1809F28C927579914C7BFD7D569BE1E4F4E7
          34926387E8C0BA33E384E425909C0C22898C92F58F55366E7FE14591FA2C1F70
          D381151FD3BFD42F23AE17F064CBF3E20000000049454E44AE426082}
        Name = 'PngImage90'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000021C4944415478DA63FCFFFF3F03258091AA0634AC7EF9E5
          DF3F86C94DE1E295C88A52673FDAFFFBD73F879FBFFF31FCFCFD87615DA91A23
          2E03FE03E5FF7FFFFDABB32F4E166E48CCA407FBF515391CF8399919569F78C7
          B0BB461DBB01E5CB9EFF3753E664B8FDFC27C3ADE7DFFBE7652A1481C483BA6F
          ED3752E67610E06461587AE415C3F1565DEC06142C78FCDF5A9397E1F7AFFF0C
          571E7F07E2CFF33795AB27B9B6DCDC6FA6C2ED20C4C5CC30FFD04B862B3D06D8
          0D00FAF5BF83260FC3A76F7F19D85918184EDFFFCA70E1DEA7A54065D256EA7C
          0E42DC2C0C33F7BD647834D508BB0140BFFE77D2E36178F9E13703135009271B
          13C39D67DF19F65CFDF033DC5298FDEFDFFF0C73763D677836CF1CBB0140BFFE
          7733106078FEFE170333C80076266078FC60D871E9C3DB300B216126A09A45DB
          9E31BC586E85DD00A05FFFFB180930BCFBFC9B818B8391E1D0B54F0CE7EF7C5A
          CBF09F41C4C540D05E88979561F9E6870C2FD7396037C0B2FAF2FF403361863F
          40A71EB8FE89E1CAEDF7AB9E2DB00A97883CB6DFDE48D04158808D61CDFA070C
          AFB639633740A7E4C2FF700B118683573F305CBDFD69F1F32556712071F1A003
          FB6D8C451D0484D81836AE7AC0F066BF2B7603E4B2CFFD57116363B87EE3C3C2
          E7CB6D1260E2625E7BF75B9889390888B0316C5B718FE1CD114FEC0648259DFC
          FFEFF3F7392F563BA422276511C7DDFB19FEFE7360F8F38F01A4FEED712FEC06
          509C99C801005B260DF0DF9EE51F0000000049454E44AE426082}
        Name = 'PngImage89'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001904944415478DA6364C001E2E2E258172D5AF41B9BDCFFFF
          FFE16C461C9AB980D414203E0234641E4906C0342B2A2ABABD7DFBF6DBA74F9F
          3AD00DC169004CB38E8E8EA7B2B2B2C4C78F1F7F9F3B77EE01BA21580D806936
          3535F5D4D2D292F8FEFD3BC3B76FDF183E7CF8F0FBC2850B0FBE7CF9023704C3
          0098665B5B5B4F6363630990C6AF5FBFC231C890AB57AF3E008A830DC16640B9
          8282426A42428232D026B0A6CF9F3F835D00E283F0BB77EFBEDDBC79F31E5073
          D4C2850B2F53D70514870131B160E6AE3DF7EE87CB0C5F7E7C62F8F6FDEBFFB7
          9F5FD56DEFB8D442543A30F5D09CFB96E52683B1960983AC902AC3BE2B1B184E
          5C39CCF0E0CE9B5EA252627C87FBFF406F0F06066626067FCD3C869E3DA90CCC
          0C4C0C2BD66FFA81D500F4BCE05D65F03F2B2A9BC14B3B052EBFE9F25486D619
          2D0C8CC801820BD81728FDF7F17260F8C3F08FA1D26D0143FBCE04060E667688
          0B883120A53EF4FF7BDECB0C567A360CAA92860CB79F9F673876E908C393FB1F
          7B89320004CCB3253B80541610F302F167209E7672EAF30A003B6D369CE30A69
          6F0000000049454E44AE426082}
        Name = 'PngImage92'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001994944415478DA6364C001E2E2E258172D5AF41B9BDCFFFF
          FFE16C461C9AB980D414203E0234641E4906C0342B2A2ABABD7DFBF6DBA74F9F
          3AD00DC169004CB38E8E8EA7B2B2B2C4C78F1F7F9F3B77EE01BA21580D806936
          3535F5D4D2D292F8FEFD3BC3B76FDF183E7CF8F0FBC2850B0FBE7CF9023704C3
          0098665B5B5B4F6363630990C6AF5FBFC231C890AB57AF3E008A830DC16640B9
          8282426A42428232D026B0A6CF9F3F835D00E283F0BB77EFBEDDBC79F31E5073
          D4C2850B2F53D70514870131B110AF233897F1FC2686BF2F9F30FCE2E6FBC7FC
          FD43B5FBEE3F1D44A5836403D1B9FCCF8E33A83B0431B02B69337CBFB88BE1DA
          E1DD7FDFDDB85148544ADC192CFDDF36B19081F3EE010686A7471918F8F919DE
          B0C8339CDD75E43E5603D0F3C26E47D6FFCE3D2B18188D82E0F21FEA2518CE1C
          7EFB9F1139407081FD11B2FFCD5CDD18B81F6D65F8F9FD25C337A0D8E74FCC0C
          D7EF323C25CA803565E1FF65DF5F609013FAC5C0C2F494E1F3EB3F0CF75F32FF
          FDFDE37F1D510680C09170D98AAFEF9EA733FF6594FFCBFCFF2950D7748FDD7F
          DA001A5942A001F668F20000000049454E44AE426082}
        Name = 'PngImage91'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B225920000021C4944415478DA95924B6813511486FF49F340690C1A3344
          7C34D3692BC6889090208528A848B31071619769B474A1762351B2702374A31B
          573E169642B35104C19DD447F119F0411C90608BD2443094A49D30C94C66A699
          E8385398A049A7EA81C3BD97CBF79DC3BD878049C4E3715B3A9D56D6BA5355B5
          B5274CE08DDA7243CBD79A64EABF04064C51D4319665C55AAD76B55D622A30E0
          402010A369DA5BAD56956C365B6897AC2930E070381CF3FBFD5E4992208A2238
          8E531886290882D09274080C381A8DC642A1905707EBF57A2B75492E972B5C1E
          3FDC5F671EA0AB7B1FEACB2F40C8C5114390F2F97C63894482D62AAD423CCFAF
          76A09FF58CF473D8860A3678A270514170F90CE666264BFFD4C1005944DF7609
          DDAE202A5FE7E1DA4AC3EE24F179362DFFF50D68CF32284F11AEDE21AC2CDE83
          C41228CF8950E4062B4BC2E0BABF10E811E171E4E0EA3B01B9781B167B138AD0
          03F665A65411AC43C1E453C6740E2E8C0C9EF66E5A001938A9C1B760B135D1E0
          292C3E7FAF12121719B8F8EE83E924BE7D7453751279EC3E781C8DD23488AE06
          E4EA0E945F7DC463E140E5FCA52B6ED349CCCEDC511DCD053409126EF71B6C26
          ED1ABC13E5CC273CE3434BE3A909F28F39F8FDA0C7FD6B47D4E1735398BF9B44
          B1908163D77E484BFCCF6FF6C8F468EAFA99F6821D8289B1BD4A6AF4AC15961F
          609E3C4421FF45189EFCEE844974084E45B748C15ED21AD94341B5AD648E2667
          0F619DF8058CAA2D08015B55F90000000049454E44AE426082}
        Name = 'PngImage95'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000003024944415478DA7D925D48544114C7CFDDEBEEBAABB6A6
          29F8099AB03D98482F59664A4104D207523DA542C8822528E543695684819088
          0F16EA83A0AB529681515269AE652505956548A262F9D9B67EBB7BEFDE3B7766
          3A2B0466D6C070EECC3DBFFFFCE7CC1138E7B07EB4BC9AB762B0510EE954E3C9
          047F53CA0654C25F10CEEA8B332386D7E70BEB05103E8F60B925403419451D88
          2200C30D0FA1B0E26630B9A0CA4483CBD74F4556FD2180A001653ACC46DD6103
          820B6E02AB5E0A9A06A0520644E3A0F7D3C1167F1116250DA617D427B87FAC2E
          2F4E5D1368EA9BAF0E34EA0AB920C0CF2595A2DD4368B71FEDCABE534AEF4C99
          54CAF7108D3D0BB7E845378A4F2C90AAE6B3F11784865ED74E4AE1E3368B284E
          B910666C3BDA9C57357E11E3019F0314ECF15256A11116AA503E1617E62F0E4E
          4A149D2509B55DCEEAE040B170C5C36049D20E62F2305EB9CF6216E370BD66DF
          A01760D2A58CE3C1698432ABC9A07B6E3189303CE3AD12AA1FFFF81A136EB08E
          4CCBA0686056086B0A0DF23B3131A7DC5335764B410185C2B9488BFEE498D37B
          5F259043804BBB624DF07A647548A8E898511222FC0D9FC6655FC1FCD0C1AA5E
          2F98165749C69924A74596E5D19A819830B341D73BEF26325E2D48A34C4BDBB1
          05BA07579685AB6DD38A35D2DFF061DC035EC2CC847189A8CC9518EEC94A0E99
          73A000FFEC341E75CC46D7698CC7628DCC9C82B41F057ABEACB885E2E689118B
          499730B3A8CA683718EDA660755F3A1C0E7B6454F4697C6978F7B6DF9E9D9D9D
          B3EFCA5022B6C5306730BE35C02F6AD1A30D09F90DDF0AB1BA3256F76E637EFC
          B2EFD9100EC193BFE30C90240930CE62B4161515B97F37504AE9503A86706163
          2BFB466767E76D846CBB535245C6183C686FA32870B3A4A4E4D2C6DCBF04104E
          46F83D0202C6373E0738F7E2B78433B9B2B272F4BF02EDEDEDDD08A462728BCD
          66CBF3ED959595D9717DDCEBF576D5D4D464FD5300E14C84EB30B916E1F2F589
          050505D71445C95055F546636363D7A60276BBBD1EE147083F844D466E6EEE11
          42B04F019EB6B6B6FA22FC02E86CFCA1C124C2C80000000049454E44AE426082}
        Name = 'PngImage93'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001EA4944415478DA630C6FDBAEC1C0C09006C4CA0CD8C10F
          20DE05C4BB57547A3C429764041A3031CD5B57C754558CE13F4CF43F82FAF9FB
          0FC3E1CBCF18561FBE7D18684003360336CD2E70E6FE0B54FD1F8AFF81B48268
          20878D8589E1F3D79F0CA5738E3C051A1087D580592003FE3130FCF9F71F68C0
          7F88214002C86560656664F8F9F3375E0336CF2E74E6FAF58781E1EFBF7F604D
          371EBFE3DB7CFCAEF2EB8FDF7919A15EF9F51B6405C337A8BEAF40BC08844106
          AC999AED28C8CBC5CAF0EEEB6FB00BFAD69C35F63453E027225CD6810C483151
          158F74369461D05110062B4AEADB63372DC791858870B9CD08B231A27D870D50
          5FDEA42C7BE1F377DF30CCDF79D50E182E2C44840BC40010001AB230C9435B66
          DE8EABBF815C6B60B8F02087CB7FA8E67F400264C0BFBF7F198A661D7E007301
          2750D32A206601B904880F03C3459C07182EEFA1E102D2CCC2C4C800440CFC40
          F1E7EFBE3294CF3D7A1366802850530E102F0046D57D20BFDD4C5DA2C205182E
          9A72426017820C78FDE13BC3AF3F7F198E5E7BCEB0F5E4FDEF40E11AB817D001
          D09050203567568113DFF1EB2F41E10252780F88411AF701711DD0B28F380D80
          1A7213182E6AC070F909E45A02359CC74848785CC003A49E02311B2850819ACF
          615387CF0019203509EAD42BB85C89D70BC40000F78E35C339D06BB400000000
          49454E44AE426082}
        Name = 'PngImage94'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002CD4944415478DA95936B48935118C7FF6F736D19D352A6
          8299979979492D6F9BDA565A82A5E08520AC4F06067D0BB2BBDF8534A2D20FA1
          16E125232D492ACBC844A7165ED0BCA5CE9C4DAC799DBAD76DBE7B4F675222A9
          410FFCE13DBCE7F93DE77F9EF33084106C1547DEC41058497C63726B83F2B93C
          9DF0A4BCF9D4E71DEBF730EB0171F5B1B28604B5E6CF5AF552418895D4067F73
          BDD8ED3E798C7EDF6E39D3EEB025E028AD48AB5C6E4C6ACDB7AD0F57CB4D34C9
          18D4B7AB4E2F5A4AD7BB9A457B7BC5799577D4573705A86A15F38487B829A54D
          ACAC5218AC9CD58102000E48F056E1B88F12AF2B6BD0F7F5CB95DAA2CEBC0D00
          EA338E267CD8C66F33480412C7730119D829D88EFE292D1EB59722CC7F3F929C
          1291DB5C08C211F0548CB2466EA0C70E6B4AFBB4EA3DA622821712217321F034
          46F43D5836B35881186D1383985B3020D23D04E3555D23EF6B86F6AD9E20B62A
          CA42AB0AA94AED7841A29BD84D9AE2A5C4F08F6E2C9A0CD0B38BE0448E48F539
          81E9C5253C682E82A49FAB513F194E5BB3105D16D14A010A5881ECB04CF4E8DA
          B0645AC094711116A10471EE720C4FF6C0CF2514051FCB605DE18C74FFF5815B
          C3F7D7EE40511C6EF296788AC29DDDA19B1DC3B4D10893C01EF11E720CE83AC1
          5A584479C7E3EEDB12EA9DB7B597B1DDC31A405E18663CE476D03E60B714EDE3
          1D608908D1D2608CCEF6C3643143260D847ECE8CBAEE77605688F36081665676
          DE2B670D10732F32D764315D4B0D38899E9FFD50480FD0E45EB066337C5D82F0
          AA438D19C30CA8CDACA1424DF1A6EF203C37B4717EC6A0BA919C8D89B9412A2D
          3C9C646059219EA9AB617B135466AA9CD187DAFC0D005B84DC0C6C040F559CBF
          0A768C00F55D0D609797C1F0C8B272BC8626BFA072A4ADF7D556E834CC66C314
          74C9EF3167E1CE5A3922F85DB565B4441BBBD9C031FF9A465BF8647AAEB698CA
          77ACFCBBE6BF01B6F0CCD863A133B23CFE54E7F8F7BF5FE18295A4AA20FEE700
          00000049454E44AE426082}
        Name = 'PngImage97'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002B64944415478DA7DD27B4853511C07F0EFB95B63E9CC4D
          9D663E46AE071A880F4813F1418456122C95EC41FF6865264524350B83500A0D
          54528B2048B3C48C902C7B2061642EB5B20C094B8D992F5059EAE6DCC3BBD371
          91865B1DF871B9DCDFF7730F3F7E84528A9527E3CA730FF6A86095C64A04E7A7
          85550EF907F04815A3DC931AAB8450C0394D3FE9D2E2EECBBE0107808525EC31
          5DA74E12E84D807901B0F200CFDA78DB625110D6A09473B850DD016740A0D71A
          F150E589048CCD507BC80EB0E0320284FA71286EE8C67F6F3065008C16C0C2FF
          86FE000494010214DCE95C06B2223C851211290892DB4E7D09CE734DDC9D2E4C
          8FDB008E10FBF7E19F14BA390A5F77026F37E2388373B19EA715EEC65223B581
          B878605091059DE736F63B0E3BC20391991C62EF9B3698F1F0CD205A3EFEE865
          AFAA25A02C61F533FFA88464B95C86CE771F30323C028355005FBF75B5DFA3CA
          63CAB3E394CD9DDAC5201B2D2ED7E727172EE696808ABD5E35AAB3D70FBB8B29
          B403039837CC40245D8BDA4FB6CF9BB6EE0C6DED199E636D052C58F6F7CC9680
          F494B4BCA283EB4B1461B1442C2430F31C7ADEBFA5A5DA281D0422350BDE72B6
          0F76C027A54E30313E5375FFD0E4D108D928E18512086146DBB80FCDBC27BB21
          93AF3A39D5728C770A4893EA895ECF159EC988CC4F3C10C4B5B66B603171B0CE
          036161D1E87EDACFDF6CEC287271315CD2BF3AEEB0B6245A559C171B1E7C3532
          3D055D7A023D0B9A8D80490F082C145BBC80C1178DF46B7FB75AF3B8A8C40118
          2D09A1A38A6C54631FE65CBDC11B292C0CB15A81059E40343B89F3410F20EDBD
          86A08B7DC40118A8DAD5AE19F28869B01C813C390E8B7B6363FBCF86002BBBC9
          7853337203EA1022D7BFDE9CDB14EF007CBB97E3A19D30F9AB35DB536779B74C
          B1582423940828CFF3F366EBBC944CD694C4B7DD0EF0918C6DDC5FA95B09FC02
          3B6F461332CB79570000000049454E44AE426082}
        Name = 'PngImage97'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002064944415478DA8D915D68526118C7FFC73C60A356238A
          593121E862EDC02E067653BBF0EA2078B18882DD445049C291DD66173242BA93
          212996D38B60488CB0ABAEFA40F362494582CD52569B12066EDAD08DB1F3D573
          3CC6A85CC7075E5EDE73CEEFF77F9EF730AAAAC2A81289C48CA228B7E8DBB3B4
          C3ED76B3BFDF314682783CAEC101A7D33940856432098FC7C3F42D88C562CB3C
          CF8FB22C8B7C3E8F52A9044110FA1744A351D1E57299EBF53A3299CC1BAFD73B
          190C06556D145A92A120128948D4C1815AAD865C2E9721C864B3D92E701C8754
          2AF57F41381CE60978EE70389876BB8D56ABA5A5C262B1C06432219D4E177B0B
          166F87B677958BF31B639CDD6E376BF38BA2D8816559EEAC6C36BB4DE7BBFF0A
          9EDC0C61F0B40059EC24AE723750A954502814B4B9E5EEEC655A8FFC7EFFDC9F
          8224C147AD0286C700EDF9D725341B4D3C6C8CEE10C0F97CBE95BF9BDD132C5C
          0F61E894002B07FCFC01282270E838504E43AC2EE7D83B6FCFF7BA275DB0702D
          8423D4B695923709DE6A00EC41E0F009E0CB2BA0FA7111FEE295DE82C7D3D4F6
          888093DDE4AD75C03CD0855F13FCFE2966CB97F7FB538C3A7F49C5F814C135A0
          BDAE270F0EEBC96B04DF5BD917D6050F7815135781661590251D2EBE002A1F9E
          21F06D0A06C5A87393EF70ECCC0446EC747132F0F92525137C7FCD10DEBBC4C0
          F82748BBE720ED807682BFF7056BF50B37AA1B74E330623B0000000049454E44
          AE426082}
        Name = 'PngImage98'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002DE4944415478DAA5936D48935114C7FF8FAFB939B569E5
          66A66E0E095F52D1FAB01484243045D028B050F04B5129FA21B21714853E2424
          88A4465128BD510D47504465465343372A34CB395F524B4D5D6A5337F7BCDCEE
          1ECB0F995FEAC0E5C2BDF7FCCE39FF7B0E4308C1FF18F32720A7D1AC66052E8F
          7042FE8A930DE6880002610A109A798EBF6338AB1DDA1090DDF029D7E964EB22
          FC8962775800FCBCDCC5F3053B0783E51B7A271627A94791B16A9F6E1D20ABBE
          2FD769679BD2351269CC7619BECE3A31B560072F10F84BDCA1F4DF04CBAC0D2D
          AD834BF04081B1365BB706C8ACEB55B12C67488F942AA31432740FCF83E358EC
          0ADB0CD0FBB7237360DC18442B7C313E6783FED9C004889062BC76785804ECAF
          7D571E21259599F14A749AADE0594E8C5C752446CCEEFCEDF714047879B8212E
          4C8E27DD66987B662A4CB78E568980B41A93253F6E4BE4A29DC59875913A53E1
          E84A54CB5D7EE81A985C2D98BE5506FA41EEEF8D1BD73B068DF70B3522407BB1
          63F9747AB84FB7791676270796672980C795E35A1170A2DE200269C5F0F46090
          1A1B8A4B358FED26DD498908482E6F5B3E97A1F1793360C5927D913E26AE6038
          75201AE1DB64286DA4007AE08279D28F494B52A1BAFA91DDA42FFE0D78613996
          1A1E695D70627CE607789A8140231665C522225886E2BA57347D573F106C0D94
          2124C40FCD575B078D2D25AB2524973D2DDFA9F0AECC4850A1FDC3172A228F6C
          6D38D21343E146D5EF199AC1E57B5D2E11A08D57A1B5D38C918FA315C696D255
          11934B5A5458E10C99691A65845C8AF6BE7170AE9FE07910BA935FD113A27660
          9675E0F903C30403A418F56786D71A29A9F06E2EB1399A327262A5AA2019FA47
          A731F3DD068113200F9040131A84CFF336BC7CD8B604C6B3803AEBD6B572D2A1
          9BB920CE3A75B442B1774F24027DBDC4B4A7171C78DDD98FB13ECB24C3B81519
          F565BABFCE82CB920F36A809E1F3A868F9101CC12EF11886A1C3846606EC9D6E
          FD858D87E95FEC27BCCF85F03B2A06720000000049454E44AE426082}
        Name = 'PngImage99'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B225920000027B4944415478DA8DD26D4853511807F0FFDD9C6B2EDF7A51
          B7A5423586A429336D5442100CFAD2A7A4A00F9584888648CC58B03E24361615
          4438A4A12622049525D187B472C6DA68641944A6AEE96C6E999BE99ADD7BB9DE
          97AE057DF025F787C38103CFEF3CE7F0105823EDEDED55822058799ECF14D79D
          9A9A9A7AAC13628D628D58E4351A8D9A582C0697CBF5B0AEAEAE3261C0E1701C
          57ABD50FF47A3D7A7A7A16446C90E3384ADC674D2653C386406B6B6BA508DCD7
          6AB5E8EFEFF789C51A114BF17ABDBCD96C96FE17B0DBED12F12697C160382097
          CB415114140A05E2F1389C4EE7778BC592B32E70EB71F00ABBC498B392C2C9A5
          BA2D0885428844226018061341DFB77BAC359B64501AB6E1C32AE0C6A36023C3
          70D70AF295C4D0D84FA89226100F0EF58AEDF72E0AF3FC0B694BA74EB54FE2F6
          BB23348BC333368CFC039ABA271BC5C3E6325D6AB24C4AE0479C857338CADD35
          ED493AD692A720197A715756B1A424B702EF836EF48DF6CD307F91B13F80A9CD
          4F1974699B64320214CDE36360119FBF523737AB4F5CA69796C8DCAD3A14E51E
          C4D49C1FDB5335783BF51A2F479F87481E15511B2689EADBBEF932AD32432197
          62F84B1C63D3D4D5A74D4596731D25C28E6D052850ED47782188056A0E32A902
          99CA2C78032E38C75F2168E508E2A4F5D329F109769A61A5626B2D03B6E24BCB
          ADD57695FFA25906AA8CDD29856207FED911A8D2776270FC09DE4DBF21494640
          A019CA5573B03267DAF672478BAB2493D171E4A4E5C3E1B60A9E8B31C9BA83B4
          32D59D7ACE5874563215F5213B3D0F1D1E9B3070219A3870BEAB9C3B52785AC2
          093C588E45A7E7BAF0AC219C3850DF7D48607906CBFF41B3344896167A6B0389
          031BE53739023420C4A2369B0000000049454E44AE426082}
        Name = 'PngImage101'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000002814944415478DA95D35D4853611807F0FFD9399E6D474BCB
          25B6255364CE942826D2D88ABE48EFBA4A100A1111118CE862C6C98690945454
          8438A5A159D155A64904815013661793919A0B197E54CEB574961EA71D993B67
          9D0925D166F58797F7E67D7EEF07CF4B204E3A3B3BABA2D168B3288A3BA471AF
          B6B6F63C12848853AC918A5C2525251A8EE3E0743A9FD6D5D595FD3360B7DB4F
          ABD5EA6E83C1809E9E9E25091B10048197E6798BC572E1AF407B7B7B99043CD1
          E974E8EFEF9F908A3512C6B85C2E916559724BC066B3C9A49D9C46A3D12497CB
          C1F33C944A2542A1101C0EC79CD56ACD4C08DC7DE6BB12590FB319D467BA48BF
          137EBF1FC16010DDDC653094028A8D418392D1D196B383B2DF805BBDBEFA7058
          B8B1579B4CB8BDCBD84D4D23E473F749C7EF1B4C69EBAA34D513144981246478
          E57928B4560C51BF80A6C71FEAD722B85AACDF46279104BE8522700C2F085D96
          C28D45C7EFA8C42A134BCC7133D0AAA47719EB12EC956F37014BC7146FD46F57
          242511E0D7448C7D5CC1F80C7FBBB7B1D0125B60BA992AD6981B882FCB9F90A3
          CAC3CBD1FBC283EA779B404DCBC462B12E394D2927313C19827796BFF6A2699F
          35DB8A55862650B4C7C81CCD3B8500378DDC8C02787C6F10589AFC1E7B8FB68A
          A164A2BCF9FD19E90AB6B570840C47D0FAFAFAFE4B3139AB818C1ECB3B8283D9
          87B1B83A8F7581479A321DEAB42C8C075C985D184747D508F1471FFC8C8A450E
          2383F344FE494DB1F61082213FB4E9B918934EE0FBEA857467E651B5874F08C4
          92C9424F531828CD2FCD34649931E273626A7E54646845CAF373337CDC4E8C83
          142824C49C6BDEE50DB84505C5A40E5E5C5C49D8CAF1A2667180A1E1565044BA
          A731CA6DF917FE373F00C2620920F80B9D7D0000000049454E44AE426082}
        Name = 'PngImage100'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000002E94944415478DA7D937B48536118C69F73D99CBA81DACAB5
          BCA025739AA679256298F766666A1953CB40032F515410D61F22596894445226
          D41F111AFD915A665099E5B58B448A8A68669A992953BC3B75EEAC6F92CB4BF5
          C0735EF8BE737EE7F9DE978F029142A148254582F51AAAABAB2BC27F44FD0664
          9317B3576E9434AAE9DAD29B1FC28F9C91F05846CA32341886196419AA92A6E9
          8C0047965B07F89384A6E47EA1499AB11FF6EE3B3CB0C1CA12417B23B1C85118
          9BD5637C9EAAA5193A700F81AC052CD58267BDB71DA5A2D4AEC672044525A0F2
          513194B18910F028884C180C4F73044217053BF1D3FE0AB85ADEFD3DC44B6A53
          595A8280FD0978F3A418613189241705333E05014BE3F30837102E37B55D0664
          9222200E20AED9EA139125B7B7A2E639068A8838D454942034DA000018F2D828
          64D03AA8E522DD840CB5B271CB09B28ADB7487029DE8E1092D281AA87BFA0041
          0712C09004A497B016B1F8D4AFE162775AAC032C25B1710F3E2B77DC24D49116
          2D727AF078FCA5248604E67C9A9842CBB7990195BFD8965A3BD7AA1CA5ACC322
          A6C55C1624F07591604CA3C3BC560F9AFC59483EB63463D0333403F5A4B62849
          21495B05283BE72FE3F3050D0E9E21E2AA8540985B49E0B64D0C53018BFA9A02
          F48CB7617A6E12B39A198C4EA9B39E5F69CD3102EEA7CB643C9EA041E61D2A96
          3AFB60A0A309D5136EBD23A6DBF976E62FB78CB25DF072F186AD95135EB73FC6
          FBF67AF47D19C93702EEA6D8A95DBDC3C4B6CE7EE8EB788791FECE329246A5BC
          58BD909417A68F8E08378C0051F293B8F6EA3818D078585E316704141EB5D687
          C59DC6D7CE8F18EEEB28E3F14C5487AF372F18F6222E78E8D3E333A0744D311E
          B7A2ED162E175D821150106F996B26B4C8D469E7CB58D644957CA777C1389D53
          0EFA7DCA002C82C3F9D07BC87D710C02C6647502836EA82CA4E4C20C9F281ED5
          AD5C4FCE3AA81F13B56197FB6E386DF644F7CF66BC6D6DC040EF44FEBA31FE4B
          BEE9923C52D28945C453865337150E65FE02346013BD19FD8F92000000004945
          4E44AE426082}
        Name = 'PngImage103'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000002C94944415478DA7D93494C53511486FFFB5E5B0AB49632D4
          5A0BA4052C75004106175A91790836A06214948543A0AE04A3AE08890B17AE24
          0648448C04D4858803680889940A4D8C0B9B101584508C16282050193A501E3E
          89CCCA9F9C9CDCE17CF973EE3D04AC341A4D119BA4D8AC1183C1508D2D44FE02
          CAD98BE56B0F1ABAC6A88EC6BBEFD3CF9648B91C5AC6A129D0343DC4A1493345
          519713941C661360D50945D4F1A985F6494B7044E47EF8F98A9194910D374330
          39B7882927E9A0682AF1280BD90858CA152DE62AA54C58D4DBD584246D3E9A9F
          D623F37801F85C02A1070DEB0CC342A8EAE4305EF13F01B79BFABEA71C90C99B
          1B1B90702C1FED2FEA91965BC0FA22F0E211F03914BE8E333FD2D59E81CB801B
          6CE2B391C0863E2436AB4C1DEC4B9C0C0D4D561EF42F1B909A5300F95C0B664D
          8DA005FB303BDE01E2B01492B58D5B765056DFBD7032318CB2DAE64128C0F0EA
          11B45193D8810978061C8648118D29B3113DAD35D68D802527F288E452B55222
          58605BE46616911D3985D09D760844D198E8EF85C83F043CA1045FDED639C8C6
          776DBB99A9FAEC936BF25625F1E3764B31DFF7008A000B44CA7438879FC0FE93
          60B4670E0E271C51176B3DD7019E5D3BA8E2F1F89D8AA814FF36572232C2FA11
          E0F109A2502D1C962A503C37E6678231D26972E995F73C2F25CB9915409D4EA5
          E272F99DAA98547F59782C867F4D40BA6D0092BD396C712528AE1BAE690586F4
          1FF026B09A2ED186AC7EA43FAAB91034B62726CD3F303C1E16BB134262864A93
          0D97F52108ED82C326C7E8BB8FD8A56B5DE77A6551796EFB625ADE15D8B87EF0
          700FC04D24F0F3EB8258C2638B03316AEC865AF77A53CF56362ACE886F79097C
          6E08950AE4E96AD1FBB8149641233C8222611F9B6652CABAE8FF0ED3B2EE9CF6
          91D9BC65DFAE9F2FE6805A80A9ED3906CD7D33A7EE5B845B4EE35A9D3824B647
          2B259C38B5028B5CA731E56AFB91ADC6F937C62B0A5B8A5A038F000000004945
          4E44AE426082}
        Name = 'PngImage102'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001674944415478DA6364A01030D2DC80FA552FBEFCFBC730B9
          3942A2924C035EFDFFF3EFDFFF1F3F7F75F6C6CB55926C40C5B267FF4D943819
          6E3DFFC970F3E9D7FE8539CA451806D8DBDBEF07520E687A0F1C3C78D03177FE
          A3FF561A3C0CBF7FFD67B8F4E81BC3A5079FE7EFAAD54A22DA0589D31EFEB7D7
          E266F8F8ED0F033B0B23C3893B5F184EDFFEB0F46ABF710CC200D57F401730A2
          B9E0FF0186DB4C8E41BDB7FFBBE9F2333C7FFF938111A89A938D91E1E0F54F0C
          E76FBD7FFC7CBEB51C411738355FFBEF6B28C8F0E4DD4F0666A06A2E76268643
          573E325CBBF569CB8B15D6BE045D605276E97F908510C3EB4FBF19B83898180E
          5F7CCF70F3E6FBB57F3F7D8F78B3C7EB0F411728E59EFD1F6D25C2F0FBDF7F86
          23173F30DCBAF966D5AB8D2EE144C78264E2C9FFD18E620CC7419AAFBD5DFC7A
          BB4B1C49E9403CE2F07F6509603AB8F266E19B3D1E09E8F2040D10F5DDFFFFDF
          E7EF73DE1EF04AC5264FFBCC4408000047A4A511D43B843E0000000049454E44
          AE426082}
        Name = 'PngImage104'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001684944415478DA6364A01030D2DC80FA552FBEFCFBC730B9
          3942A2924C035EFDFFF3EFDFFF1F3F7F75F6C6CB55926C40C5B267FF4D943819
          6E3DFFC970F3E9D7FE8539CA451806D8DBDBEF07520E687A0F1C3C78D03177FE
          A3FF561A3C0CBF7FFD67B8F4E81BC3A5079FE7EFAAD54A22DA0589D31EFEB7D7
          E266F8F8ED0F033B0B23C3893B5F184EDFFEB0F46ABF710CDC807F0C0CFB19D1
          5CF01FE802260606C7A0DEDBFFDD74F9199EBFFFC9C00854C4C9C6C870F0FA27
          86F3B7DE3F7E3EDF5A8EA00B9C9AAFFDF735146478F2EE2703335035173B13C3
          A12B1F19AEDDFAB4E5C50A6B5F8261605276E97F908510C3EB4FBF19B8389818
          0E5F7CCF70F3E6FBB57F3F7D8F78B3C7EB0F411728E59EFD1F6D25C2F0FBDF7F
          8623173F30DCBAF966D5AB8D2EE144C78264E2C9FFD18E620CC7419AAFBD5DFC
          7ABB4B1C49E9403CE2F07F6509603AB8F266E19B3D1E09E8F2040D10F5DDFFFF
          DFE7EF73DE1EF04AC5264FFBCC44080000116EA4116339DE180000000049454E
          44AE426082}
        Name = 'PngImage105'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000001874455874536F6674776172650050
          61696E742E4E45542076332E313072B225920000007D4944415478DA6364A010
          30E2927861CE700B89BB40E224431B3906A842B9D50362401512F718D0800324
          19407120526C00D00BEB80942894BB02E885A9A41A30F0B100F2820ED4901C92
          BD0035640E904A066213A00167C931009C9C819AD570A9C1E785C3404A1C884B
          80066C22C700504A9C0FD4FC1C9F2B294E480072902F11D735460A0000000049
          454E44AE426082}
        Name = 'PngImage106'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000000FD4944415478DA63FCFFFF3F032580711819B074E9D22F40
          8AFBF7EFDF0C3F7EFC60F8FEFD3BC3B76FDF18BE7CF902C65FBF7E05F37FFEFC
          09C6DBB66D63443160F1E2C5ED4076B9929212E3BF7FFF187EFDFA05C620C520
          1A6428C8F0F3E7CF335CBD7A75CE8103075231BC307BF6EC767E7EFE0A616161
          B0AD9F3E7D026B04A9616565653877EE1CC3B56BD716EEDDBB37016718F4F6F6
          F6090808144A4A4A32BC7BF70EEC02161616B0E61B376E2CDEB973671CC140AC
          AEAE9E077445A2ACAC2CD80090E69B376FAEDABA756B38D1B1909E9EBE445C5C
          3C1AE485EBD7AFAFDDBC797308C9D1181E1EFE081870173F7EFC1808F4F71FDA
          A683116C00007FC9C1E16F55BF0D0000000049454E44AE426082}
        Name = 'PngImage108'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000001224944415478DA63FCFFFF3F032580711819D0B0FAE51706
          0626EE9F7FFE307CFDF987E1CBF7FF0C1FBFFF62F8F0ED37C3A7CF7F18DE7DFF
          CDF0FDCB1F867F40B17F5FFF30BCDAE4C0886240DDCA97EDFF1818CA0D15D819
          7F03C57EFEFCC7F0E3D73F86EFBFFE42F04F20FEFD8FE1D8B9B70C372FBC9AF3
          66BF672A86174A163D6E17E663AB9011626578FFF537C3BBCFBF19BE0035FEFB
          F79F819D9591E1F0B9F70C372FBE5CF87AB77B02CE3048987AAF4F949FAD5045
          9C8DE1C5875F0CDF8006B002351F0469BEFC66F1AB6DCE710403D1BDE5FA3C29
          61F6444D2976A0F3FF31EC3BFF8EE1C6B537AB5E6E700E273A16748ACE2D9117
          E38CFEF1FD2FC3D52B6FD6BE58EB104272344A251D7BF4FF3BC3C53F6F3E06BE
          DEEDF987B6E960041B0000CAD0BEE161DA12680000000049454E44AE426082}
        Name = 'PngImage107'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000001874455874536F6674776172650050
          61696E742E4E45542076332E313072B22592000000414944415478DA63FCFFFF
          3F03258071D4001A1B307BF6ECFFA9A9A98C641900D20C63E33304AB01C89A09
          1982610036CDF80C4131009F665C860CF2681C1A06000066E343E1676FA2A300
          00000049454E44AE426082}
        Name = 'PngImage109'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE90000001874455874536F6674776172650050
          61696E742E4E45542076332E313072B22592000000434944415478DA63FCFFFF
          3F03258071D4002A1B307BF66C82A6A5A6A632E275013E43D035E3F4023643B0
          69C61B06C886E0D24C30104186E0D34CD00062C0A8010C0C0066E343E141CDA7
          F50000000049454E44AE426082}
        Name = 'PngImage110'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100403000000EDDDE2
          5200000030504C5445000000800000008000808000000080800080008080C0C0
          C0808080FF000000FF00FFFF000000FFFF00FF00FFFFFFFFFF4F132649000000
          1074524E53FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00E0235D1900000030494441
          5478DA639CC9C0C09006C48C40065B029401120031C00220065800C880080019
          100106C6F90950C67F06AA3000B0FC1001C5BAA2EF0000000049454E44AE4260
          82}
        Name = 'PngImage111'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000002694944415478DA9593CD4B545118C69F3B73BD33DE9951
          4727CB8F32B2514AC4442A137113D43E082B246AD1A24D10B4088A325A05FD01
          ED6B21110D0441AB08C43E2DC45A94367E306933E33833CED79D7BEFF9E8ED0A
          12384A1E389C03E77D7FEFF33EE71C454A89ADC6CAE4501F846CDB756CFCF956
          31CA7680E4C7C15902841AFB27823B06243F0C0EAB9A7F0C746C15B2F7F60CBD
          1FDD1120F17620AD075B830ADCC8C5A3B614C2DF7272D2FA2F40626260B44AAF
          BDEBF185203983914EC22C64C65A4F7F39BF09B0F279C82F853C4B154E51BF3D
          92CB364AD63DBE7A082B0551CE43515B606412308B5983CE9724175F29FE0DAD
          11858CFA56E5ABEB727B7C70BB3462BAA008095E4E801597202D4636F8E1D25A
          9C3DB74C30B30CDB2CC12CE5524E0BD46F540BD41FF0D6EE8628CE41986B108C
          3909C25E9FD2E6D40E39A277A25CC8C0C8A7B3A4A6DD01C4C74F682429EAA969
          68F5064260B9E9F524CBA695532BB427A0DBDF8D722E0D23B75AA4F8F6F0C5B9
          C48689CBAF8FFEF5225AD31C6E84BD0A968F39490EC0B601A1C2551D46E677B4
          2C99E8E8B8341FDB740BBF5EF5DE2615F7BD8106D8992992CD36AA2BE401B354
          E452CB91CECB0B672A5E63EC65CF633DD43C52A502AC1487A20421A50A9E5F26
          1516A4BB11E9D8FCD4E1AB4BBD15018B2FBADFD534EDEF77AB1EA7B2B99605B7
          4D68DE00C005B86921F9F37BBCFB7AB2A9226021D2B558B737BCAFB49280914D
          5982CBA792F35561B22B5A7540F7FA6A919CF9611CB999D62B02E69F1D2A08AA
          440FE4D1C10BB337FE7D71D30F43D7C8D03BDC62F57D77F3AE8A80E858C748FB
          B99927D8667CBA553D2C188F1C7F6039FFE20F461E81FFCAFB3AB60000000049
          454E44AE426082}
        Name = 'PngImage112'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E33
          36A9E7E2250000020C4944415478DA9593CB6B1A5114C6EFD4F73882538A5829
          2934904D16A574D39D2EA408EE0A252811DDC49574210537A526D47FC05DF629
          182A7640DCB8F02D745B68772562291585FAC0D7E8F8CA39379990C744EA85C3
          DC3BF79CDFFDCE776718B266542A9597CBE5F2A9DD6EFF7A5F0EB30E502A957E
          AD56AB470E8783DF18502C16F7743ADD29CE47A3D191D3E93CDC085028143A66
          B399671886B45AAD1928E15C2E97F45F807C3E7FC8B26C14828007A4D7EBA18A
          53B7DBEDB903A856AB1CD0DF42E26B783E47D3D88B4124492293C984402B1432
          1E8F45D8FFBB582C7E406E119E02532E977F1A8DC65DAD564B542A15A5C2262D
          1C0E87643E9F136C0321389FCD66143C9D4E89288AFF98CB7ECF388E7B663299
          502A4DC0E4DB81ED401E0543F460BD4D01B95C4E0B8B33003CC104948B0520F1
          EA549CE30183C10063042AB77D3E5FEBCAC46C36CB21C46AB55AB00013650006
          0EF4A5D96C4EE0FD8EDFEFFF73E7163299CC0750F0094FEA76BB374E973D68B7
          DB42201078A3788DE974FA84E7F97D8D4643BD904D95CD54ABD5A4D1687C0F06
          832F140182207CB3582CAF108046CA857ABD9E1A88EFEAF57A33140A3D5604A4
          52A9DF369B6DABD3E9907EBF2F81F42F50D886C20383C1C0C275935AAD2686C3
          615611904C2687D82F141D7BBDDEF7D7F7E2F1F83B50F311E261241279A00848
          2412FB1E8FE733593362B1D81E7E81D16894FE17E70A015B0D98808536000000
          0049454E44AE426082}
        Name = 'PngImage113'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000036F4944415478DA4D930D6C535514C7FFF7BDBE7EAD5D5B
          F6B6E0D6D05A3E034160BAC918B2A9894641D785652E3295405408601A64C684
          84440209A08B9B128711020435C628C8571650E26640D9DCC2E23473800EB7D0
          8CAD5DD78EF5E37D7ADE0B106EF27BE7E6BEDCFF3DE77FEE65BAAEC3186F7C37
          524EA125E4B557A4650D06B2A621A368495A9F2262844A2488387182B8C8EE0B
          BCF6CDD0B625335D9FECA82EC4C3635A223122915521C9402AA7E256EC2EDA6F
          4E4FD0EFC80381755FDED85315F2ED7C6B85889149150263B03080B7103C60E5
          28721C049A67151D5787D2F8A22B3AFE40A0F5CCE15361FFCF61B7A0234727EA
          2AA118918E451ED88266522942898747E4D87F38D01840F34FB7600AC4FB9EE6
          AFC5170D2C0FD9E7F2422174593151A50C32137F2329AE872EAE06CF38840A79
          BC7BA41FE1A7FC38D8311C350562BD55C19C6CB92906C3BC92EC01675F06251D
          4726F617465D0DC889B5A0CCCD52163F22A0B1A50759414FD1521D1BEF5965A5
          7477D9BC8FEDB4E52F82347A1A9CFB19C4AF9F424CDC82A98206B3441E3A040B
          87C7FD16ACFBAC0B19492E38B37DE504BBD355F9A42E6BE7BCA17A515755C889
          7E70C21C8CFDD38E29D78BC63640C942636E28C56BB174EE2C34B474222B2BDE
          F3EF3F9B64A3972BBAEDE21365BA2C4123D3ACCE59609A056A769A4AB80195D6
          53D13EA4667F002E5083852579A8DD7751A1A4AC1776BDA0B36847F98EC9B463
          DED5B1D2375F2E2B4672E8340AFCD5503293488F0D20CE2DC6DDE016E8CE0066
          3819F2AD12D637FF78FBD2DE1ABF519A696275D3B70B8A7CEE81AF9B5662F8EC
          2A385C41241377902ADE864CC95AE81AA30E007E6A612299C0F643977EEFF8B0
          BEDC14A87CE78448B12D5C555A57E739004B7604A3CA7C8C3FBA1B56879B8C03
          2C3C071B7520E8E3D13B781BBB8F759EBBDCDAF89229B07CD39157281E7D6EFE
          84A3B6E80774E6366250A9404E6690C813852E944659CEF078B0F9F999E81BF8
          176D27AF1CFDAD6DC30653A06CE3A1FD35D54BDE6B5AE323C7196CDE7966DB54
          BAA0327D24C32E9A1F6C8FA2BEB208177EFD13C7CF777FD47DF8ED2653A0F4F5
          4F37AD5836A7AD7C61005E97035E8F1DBE3C1B3C84DB69858D1E818DFA3F1C57
          28029F7F7F05677FE98FF41EDFDA6A0A2C7DF563C3830811200AEEE1BB87C76E
          1504BB5D407E1E89E73BF1C7E0C875E3ED5DFB2AD26308FC0FDF3573A18B3D62
          A30000000049454E44AE426082}
        Name = 'PngImage114'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000000784944415478DA63FCFFFF3F032580916A06FCCFCF869B
          C438712A23C3AA6284C961BD8C1FAFA5C3F9FC5A331907A1011487C1C5379BE0
          26E98BF831069CCD85F337184F664CD8F006CE5F1020C238080DA0380C22AA36
          C04D5AD116C0786BF63D385F2D558991A17C23C2A64E7FC6416800C561306006
          0000763095E15A4C85FC0000000049454E44AE426082}
        Name = 'PngImage115'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001DC4944415478DADD933D485B5114C7FFEF2516042D55F3
          A2C1A6689DFCA0092AF50311B4A64307A1202E1D843838B5B45B079D44DC1571
          9004E9E41C15FC0C0E6A696A5B1191A0A8AD55237946348BC97BF7A3374F5F54
          A8A074EB8573CF597EBF7B38F75E89738E7F59D27F22F0FBFD2F44DD27A2E68E
          DC8E880F5EAF37600A36DCEEB2D2FC7CDB855532F6CBFA2A5FD4125435865068
          755F089CA620DADC5CA71C1EC6A0EBD40064594E43A9B0582C46B65AAD282CCC
          C3E4E47C420832AF09EA9568F414C9A49E864D2815635B1F114F9EE06DD5281C
          8E3C8C8FCFDC14B4B43428AA1A171D1003302566F6ADBF83F2C8898D83EF186A
          0B221098BA29F0781A95582C0E4298017C0ABF073881C644100A474E099E391B
          10DA0D62FDE02BDA1FF60A4167A6E4F3F9785191132E57457A782941F7442B3C
          151DA09C81320A068EA3D37DE464D9F1797B0E3F7E2FA1CB3E0043F0B77B5A92
          06F1CAD5895FC75BD0451784E9D0696A3E0F60CF7E8CC59D69ACFCFC72FB437A
          3352CA5BDD5D02A420941A9D44CEF6909B55204E5FC66C783AAC1154DE2A783D
          5C4C139A2666A021417494D8CAE4DAA72FB1B2B78CF9CDD935013F8FF4F3E49D
          9F7279AFAC563F69B22D6C06BF690CF502D6EEF5178A7B2495328BED9CD10CB5
          5F5CCFE5FA039136FC0376C1E82D0000000049454E44AE426082}
        Name = 'PngImage116'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E33
          36A9E7E225000001DE4944415478DA6364A010300E1303E6CE9DEB07A4FA8158
          89487D2781B83A3939792FCC80C766660632A2A2C240DE7F86FFFF2118046034
          441CC27AF9F20DC3850BD7AE030DD08219F0DDDBDB99E3E9D3B70C7FFEFC016B
          FAFBF72FDC2010FEF7EF1F58332B2B3383949430C3BE7DC75F030D10831BE0EB
          EBC6F1FCF95B86C9671318F8D8851822543BE09A6186810C6167676510131300
          1A700CD5003F3F0F8EEC354E0C5AD2460CAF3F3C6648D69904D7844CB3B2B230
          888AF231ECD97304D98039DF577DAAE5D091366530537462B8F4F808C3F3F777
          19D8589819D898588041CDC210A731016C000B0B1383B0301FC3EEDD87200600
          35FF9FF92A8FC150D69AC152D985E1FD97570C1202320C4CC008626662666066
          6462D87D6521438BCF2668804202F3E2C52B0C0F1E3C666054AF67F96FA260CE
          60A3E4CEF0EAF313A0537F31B032B332B030B132B0026D97175165D876712E83
          F5FF5CECE940AA8281938D85E19CAB86BB86A1AC15C3BB2F2F1824F9E5C036B3
          3033030D6266D8746126C3B2B41B58131D5850A282811D68C8296735573D1339
          2B8613F77631DC7D73ED1F070B2B300CD81838D8D81836643D60C69B948186B0
          B131311C735073323EF368FF9B6B75FF4549CE0B22150C2C9C4CCCBF9999FEBE
          79D0C24094010063E4D2BD2D7ABAAB0000000049454E44AE426082}
        Name = 'PngImage117'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E33
          36A9E7E225000001624944415478DA6364A0103022B1FFA389FDC7A206BF01F7
          EEFD8230FEFF470802D92A2A1C8C500319F119C00034E03F0E03186EDFFECEA0
          AACA89A187280340189B66825EC82B4C669832A98DE1CF6F51B8B89A1A1723D1
          0674F5D4309818EB33383A5803C5246006A0E8C3EB8539F37A19F2735318D66F
          DCCEE0EDE5C2F0E3BB20E906B4B554327CF8F88961FEFCE50C61A17E0C323252
          C47B01660088FFE1C34786A5CBD632B8BB3B32A8A92A33126D406B7305C3DFBF
          7F197EFFFECDF0FCF90B869DBB0F32B83ADB0363458991082FF4303435948135
          83F0C74F9F185EBE78C5B075FB3E8686BA52C206CC9AD3C5505355C8B071D30E
          0633537D86CEEE690C06063A0C5C9C9C0CC94951180660E485A2E23A060D0D15
          867F40033F7FFAC2C0C7C7C3A0A9A9CE60676B813D16D0415C42EE7F5F1F5786
          D0103FC6A6E6DEFF16E6060CA74E5F62A8A92E24CE006470E9D2B5FFAB566F60
          50555366888F0D87EB0300ABBCD31120BDDC3E0000000049454E44AE426082}
        Name = 'PngImage118'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000033E4944415478DA5593694C134114C7FFBBDBC536959422
          4255CAA98D168D82F70989C603CF180D06F08826C62F95C633866FC60F1EF188
          1A356A830A0A887C50A284C32B6804AD31A411902A056D48CAE101424BB7BBEB
          DB161B9DE497D9CCCCFBCDDB37338C2CCB50DACE07DFE6517721254ABD705890
          A0204812BC01E9178D0F127D8448FC20FA8962A296F92BD85EE6B2CC348CBD78
          286B3CFE6D437E92113F7C22FC02303022A2B3EF37AA3F0F7DA7696B589057E2
          3C9199A22FDCBB2806DF7E8AE019062A06E054040744B0D4B32C78FAF6056434
          BA8671A3A9BB372CD856D47271FDF4584BCEEC18B4F749B49802684AC585042C
          C9406B27E938586F75E1747E22CED677222CD87ABDB964FB7C635EA6291AEDBD
          02588AA0CD43C18A040C3892A48CE770D0E6C0A6A5F1B8FCE26B7758B0F9F2BB
          BA8295A615C9315A74F44B2440280B1205FB51D98C093CF22FD8E1E3E5011ADA
          12166C38F7AAFD4C6EC614BFC4C33344634C685745A434AFF33902821FABB3D7
          21EF4A13BC7E61DCA3034BBE87056B4F3E1DB9579015D1EA11314487C5523033
          E086D0DF017EE22CA0C701898E759A793A9ED4BF0027F98B76EDC8DF1D14AC3A
          5EAD5171EC70E5D19568EA1220212415BEBC44727232DC6E370C060365C3C2E5
          72C16834A2AAAA4A116A8382E5850F13745A755719095E770AC16AD32942E8FD
          028356445B5B1B5A5A5AA0D16860369B11171787C6C6C6228BC512CA20EBF0FD
          A9B1FAC8D6B263ABF1AA330051A20BD45A135C989090009BCD86A53987114F47
          585379036BB3B3E17038D0DCDC0C66F1FEE218CAF6EAA6CC8C2DB9CBA7A2BED5
          0FAD9A2ECBC74A444747232D2D0DB5B5B5485FB313497A0EF7EFDC44E6B22568
          686880DD6E07B3609F2D87044549F1B19A9424137E8FC8104419524084817523
          3B5D8FC1C1417C7276D06592618C9F84C8C848545454D87C3EDF1166EE9E6BA7
          3666CD3C5298BFE0BF37400E14DFBE15FC67A7D30993C904AFD71B2C626A6A2A
          CACBCBA94E8C8AC9D87169DFA2F4C957E7991311355683289D1A7AED18E888F7
          6F9EA1A6FA31F61758F1EE6D133C1E0FFAE438743BEAE4402050575A5ABA8A99
          957B5EA981954824C68DA21F45A78EE079B59A47BAA60D3D3D3DF828CF6957DE
          DE87BB56BB92E91FC57C618DF121CA350000000049454E44AE426082}
        Name = 'PngImage119'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E33
          36A9E7E2250000012D4944415478DA6364A010308E00038A963AFDFFF4F31DC3
          9CA40B8C641990BDC8E6BFA8802CC3B5A7E7185665DE64246840DE129BFF0CFF
          FF30FCFA07C47FFE32480A2A33E8C9DA309CBABF8FE1CAD3D30C3B0B9F33E235
          206B91D97F579D7886BFFFFF31FCFDF797E11FC37F86171F9E3008F288311CBF
          BB87E1FCE3A30CA72ABF31E23420699EC17F2FFD6486876F6E33FC06BAE2CFBF
          DF0CBFFFFE666062626310E3956138726F27C3990727196E36FE61C46A40D42C
          8DFF7E06E9408D7F19FEFCFD0B76C9F38F8F1884782480B61F63D87D63E78D07
          2D0C9A385D10304DE1EF8F5FBF8061F08BE1C79FDF0CCA225A4C164A6E0C671E
          1D63D87B6BF7A5472D0CFA24C5825613E36B1339479103B7F69D7DD4C6604272
          342AD430BCFEFB8F59E471DB5FF2D2012130F00600002C1881115D33879F0000
          000049454E44AE426082}
        Name = 'PngImage122'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E33
          36A9E7E225000001244944415478DA6364A010300E630352E619FCE7631762E8
          8BDEC748B20161D3D5FF6B491B31BCFEF098616ADC11D20C70EF97FCAF236DCA
          60A6E8C470E9F11186E7EFEF32B0B13033B031B10055B3304C8A4135108563D6
          CEF5DF50D69AC152D985E1FD97570C1202320C4C4025CC4CCC0CCC8C4C0CBBAF
          2C649816770ABB01EAF52CFF4D14CC196C94DC195E7D7EC2F0EFDF2F06566656
          061626560656A0EDF222AA0CDB2ECE6598977401B70B146A18AEBB6AB86B18CA
          5A31BCFBF28241925F0E6C330B3333D02066864D1766322C4BBB81DB001090AB
          61B8E8ACE6AA672267C570E2DE2E86BB6FAEFDE360610586011B03071B1BC386
          AC07CC046341AE8AE18C839A93F19947FBDF5CABFB2F4A563A90AD62FECFCCF4
          F7CD831606F20C20160CBC0100DB5A4E11E73E07AB0000000049454E44AE4260
          82}
        Name = 'PngImage121'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E33
          36A9E7E225000001C14944415478DA6364A010300E430352E619FCE7631762E8
          8BDE872257B4D4E9FFA79FEF18E6245D60C46940D874F5FF5AD2460CAF3F3C66
          981A7704452E7B91CD7F510159866B4FCF31ACCABCC98861807BBFE47F1D6953
          06334527864B8F8F303C7F7F97818D8599818D8905A88A8581974B82414FD686
          E1D4FD7D0C579E9E66D859F89C116E80593BD77F43596B064B651786F75F5E31
          4808C8303001A598999819981999187EFFFBC3F0F7DF3F86171F9F3008F28831
          1CBFBB87E1FCE3A30CA72ABF3132AAD7B3FC37513067B051726778F5F909C3BF
          7FBF18589959195898409885E1FFFF7F0C4C4C8C0C3F7EFF64F8F5F71790CDC6
          20C62BC370E4DE4E86330F4E425CA050C370DD55C35DC350D68AE1DD97170C92
          FC72609B599899818630035DF097E1F7EFDF0CCF3E3D6210E29100DA7E8C61F7
          8D9D371EB43068C2C340AE86E1A2B39AAB9E899C15C3897BBB18EEBEB9C6C0C1
          C2CAC0CAC8CAC0C1C6C6C0C729C560A1E4C670E6D13186BDB7765F7AD4C2A08F
          110B72550C671CD49C8CCF3CDAFFE65ADD7F516439AD26C6D726728E22076EED
          3BFBA88DC104673A90AD62FECFCCF4F70DD079280600BDF9FAEF3F6691C76D7F
          71A703AAA444BA1B0000092995BA9A525B530000000049454E44AE426082}
        Name = 'PngImage120'
        Background = clWindow
      end>
    PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    Left = 104
    Top = 232
    Bitmap = {}
  end
  object FindDialogQuery: TFindDialog
    OnFind = FindDialogQueryFind
    Left = 104
    Top = 200
  end
  object ReplaceDialogQuery: TReplaceDialog
    OnFind = ReplaceDialogQueryFind
    OnReplace = ReplaceDialogQueryReplace
    Left = 136
    Top = 200
  end
  object PopupQueryLoad: TPopupMenu
    Left = 104
    Top = 64
  end
  object popupTreeView: TPopupMenu
    Images = PngImageListMain
    OnPopup = popupTreeViewPopup
    Left = 136
    Top = 64
    object NewDatabase1: TMenuItem
      Action = actCreateDatabase
    end
    object menuAlterdatabase: TMenuItem
      Action = actEditDatabase
    end
    object PopupmenuDropDatabase: TMenuItem
      Action = actDropDatabase
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object PopupMenuCreateTable: TMenuItem
      Action = actCreateTable
    end
    object menuTreeAlterTable: TMenuItem
      Action = actEditTableProperties
    end
    object menuTreeCreateView: TMenuItem
      Action = actCreateView
    end
    object menuTreeEditView: TMenuItem
      Action = actEditView
    end
    object PopupMenuDropTable: TMenuItem
      Action = actDropDBobjects
    end
    object menuTreeCreateRoutine: TMenuItem
      Action = actCreateRoutine
    end
    object menuTreeEditRoutine: TMenuItem
      Action = actEditRoutine
    end
    object Exporttables2: TMenuItem
      Action = actExportTables
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
    object menuShowSizeColumn: TMenuItem
      Caption = 'Display size of objects'
      OnClick = menuShowSizeColumnClick
    end
    object Selectbackgroundcolor1: TMenuItem
      Action = actSelectTreeBackground
    end
    object menuRefreshDBTree: TMenuItem
      Action = actRefresh
    end
  end
  object popupDbGrid: TPopupMenu
    AutoHotkeys = maManual
    Images = PngImageListMain
    Left = 40
    Top = 96
    object menuproperties: TMenuItem
      Action = actEditTableFields
      Default = True
    end
    object menuAlterTable: TMenuItem
      Action = actEditTableProperties
    end
    object menuEditView: TMenuItem
      Action = actEditView
    end
    object menuEditRoutine: TMenuItem
      Action = actEditRoutine
    end
    object InsertfilesintoBLOBfields1a: TMenuItem
      Action = actInsertFiles
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object menudroptablea: TMenuItem
      Action = actDropDBobjects
    end
    object menuemptytable: TMenuItem
      Action = actEmptyTables
    end
    object MenuRenameTable: TMenuItem
      Caption = 'Rename'
      Enabled = False
      ImageIndex = 33
      ShortCut = 113
      OnClick = MenuRenameTableClick
    end
    object menuMaintenancea: TMenuItem
      Action = actMaintenance
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object MenuCopyTable: TMenuItem
      Action = actCopyTable
    end
    object menucreatetablea: TMenuItem
      Action = actCreateTable
    end
    object Createview2: TMenuItem
      Action = actCreateView
    end
    object actCreateRoutine1: TMenuItem
      Action = actCreateRoutine
    end
    object Exporttables1: TMenuItem
      Action = actExportTables
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object PrintList3: TMenuItem
      Action = actPrintList
    end
    object menurefresh2: TMenuItem
      Tag = 28
      Action = actRefresh
    end
  end
  object popupHost: TPopupMenu
    Images = PngImageListMain
    OnPopup = popupHostPopup
    Left = 9
    Top = 96
    object Copy2: TMenuItem
      Action = actCopy
    end
    object N26: TMenuItem
      Caption = '-'
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
    object N1a: TMenuItem
      Caption = '-'
    end
    object PrintList2: TMenuItem
      Action = actPrintList
    end
    object Refresh1: TMenuItem
      Tag = 28
      Action = actRefresh
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
    Top = 304
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 7
    Top = 232
  end
  object TimerHostUptime: TTimer
    OnTimer = TimerHostUptimeTimer
    Left = 7
    Top = 269
  end
  object popupTableGrid: TPopupMenu
    Images = PngImageListMain
    Left = 72
    Top = 96
    object MenuEditField: TMenuItem
      Action = actEditField
      Default = True
    end
    object MenuAddField: TMenuItem
      Action = actCreateField
    end
    object DropField1: TMenuItem
      Action = actDropFields
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
      Action = actEditIndexes
    end
    object N3a: TMenuItem
      Caption = '-'
    end
    object InsertfilesintoBLOBfields2: TMenuItem
      Action = actInsertFiles
    end
    object PrintList4: TMenuItem
      Action = actPrintList
    end
    object Refresh2: TMenuItem
      Tag = 28
      Action = actRefresh
    end
  end
  object popupDataGrid: TPopupMenu
    AutoHotkeys = maManual
    Images = PngImageListMain
    OnPopup = popupDataGridPopup
    Left = 104
    Top = 96
    object Copy3: TMenuItem
      Action = actCopy
    end
    object Paste2: TMenuItem
      Action = actPaste
    end
    object setNULL1: TMenuItem
      Caption = 'Set NULL'
      ShortCut = 24654
      OnClick = setNULL1Click
    end
    object N4a: TMenuItem
      Caption = '-'
    end
    object Insert1: TMenuItem
      Action = actDataInsert
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
      object N11a: TMenuItem
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
    object Copytableas1: TMenuItem
      Tag = 46
      Caption = 'Copy data'
      object CopyasCSVData1: TMenuItem
        Tag = 48
        Action = actCopyAsCSV
      end
      object CopycontentsasHTML1: TMenuItem
        Tag = 49
        Action = actCopyAsHTML
      end
      object CopyasXMLdata3: TMenuItem
        Action = actCopyAsXML
      end
      object CopygriddataasSQL2: TMenuItem
        Action = actCopyAsSQL
      end
    end
    object Exportdata2: TMenuItem
      Action = actExportData
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
      Action = actHTMLview
    end
    object InsertfilesintoBLOBfields3: TMenuItem
      Action = actInsertFiles
    end
    object N19: TMenuItem
      Caption = '-'
    end
    object menuSQLhelpData: TMenuItem
      Action = actSQLhelp
    end
    object Refresh3: TMenuItem
      Tag = 28
      Action = actRefresh
    end
  end
  object popupResultGrid: TPopupMenu
    Images = PngImageListMain
    OnPopup = popupResultGridPopup
    Left = 136
    Top = 96
    object Copy4: TMenuItem
      Action = actCopy
    end
    object HTMLview1: TMenuItem
      Action = actHTMLview
    end
    object N14: TMenuItem
      Caption = '-'
    end
    object Copyrecords1: TMenuItem
      Tag = 48
      Action = actCopyAsCSV
    end
    object CopycontentsasHTML2: TMenuItem
      Tag = 49
      Action = actCopyAsHTML
    end
    object CopyasXMLdata2: TMenuItem
      Action = actCopyAsXML
    end
    object CopygriddataasSQL1: TMenuItem
      Action = actCopyAsSQL
    end
    object Exportdata3: TMenuItem
      Action = actExportData
    end
  end
  object TimerConnected: TTimer
    Enabled = False
    OnTimer = TimerConnectedTimer
    Left = 103
    Top = 269
  end
  object popupSqlLog: TPopupMenu
    Images = PngImageListMain
    Left = 8
    Top = 128
    object Copy1: TMenuItem
      Action = actCopy
    end
    object Clear2: TMenuItem
      Caption = 'Clear'
      ImageIndex = 58
      OnClick = Clear2Click
    end
    object N15: TMenuItem
      Caption = '-'
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
  object TimerRefresh: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = actRefreshExecute
    Left = 72
    Top = 269
  end
  object SaveDialogExportData: TSaveDialog
    DefaultExt = 'csv'
    Filter = 
      'CSV-Files (*.csv)|*.csv|Hypertext-Files (*.html)|*.html|XML-File' +
      's (*.xml)|*.xml|SQL script (*.sql)|*.sql'
    Options = [ofOverwritePrompt, ofEnableSizing]
    OnTypeChange = SaveDialogExportDataTypeChange
    Left = 40
    Top = 200
  end
  object ZSQLMonitor1: TZSQLMonitor
    Active = True
    MaxTraceCount = 100
    OnLogTrace = ZSQLMonitor1LogTrace
    Left = 104
    Top = 304
  end
  object popupDbGridHeader: TPopupMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    Left = 40
    Top = 128
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
    Images = PngImageListMain
    Margin = 1
    OnExecute = SynCompletionProposal1Execute
    ShortCut = 16416
    Editor = SynMemoQuery
    TimerInterval = 500
    OnAfterCodeCompletion = SynCompletionProposal1AfterCodeCompletion
    OnCodeCompletion = SynCompletionProposal1CodeCompletion
    Left = 40
    Top = 304
    EndOfTokenChrW = '()[]. ='#9
    TriggerCharsW = '.'
  end
  object OpenDialogSQLFile: TOpenDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*'
    Left = 40
    Top = 232
  end
  object SaveDialogSQLFile: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Scripts (*.sql)|*.sql|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 72
    Top = 200
  end
  object SynEditSearch1: TSynEditSearch
    Left = 72
    Top = 304
  end
  object popupQuery: TPopupMenu
    Images = PngImageListMain
    OnPopup = popupQueryPopup
    Left = 104
    Top = 128
    object MenuRun: TMenuItem
      Action = actExecuteQuery
    end
    object MenuRunSelection: TMenuItem
      Action = actExecuteSelection
    end
    object MenuRunLine: TMenuItem
      Action = actExecuteLine
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object menucopy: TMenuItem
      Action = actCopy
    end
    object menupaste: TMenuItem
      Action = actPaste
    end
    object menuclear: TMenuItem
      Action = actClearQueryEditor
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
    object menusave: TMenuItem
      Action = actSaveSQL
    end
    object menuSaveSelectionToFile: TMenuItem
      Tag = 1
      Action = actSaveSQLselection
    end
    object menuSaveAsSnippet: TMenuItem
      Action = actSaveSQLSnippet
    end
    object menuSaveSelectionAsSnippet: TMenuItem
      Tag = 1
      Action = actSaveSQLSelectionSnippet
    end
    object N23: TMenuItem
      Caption = '-'
    end
    object menuSQLhelp2: TMenuItem
      Action = actSQLhelp
    end
    object N24: TMenuItem
      Caption = '-'
    end
  end
  object popupQueryHelpers: TPopupMenu
    Images = PngImageListMain
    Left = 136
    Top = 128
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
  end
  object popupFilter: TPopupMenu
    Images = PngImageListMain
    Left = 72
    Top = 128
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
    object N20: TMenuItem
      Caption = '-'
    end
  end
  object popupDataView: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = popupDataViewPopup
    Left = 8
    Top = 160
    object menuViewSave: TMenuItem
      Caption = 'Save perspective ...'
      Hint = 'Save current view details (columns, filter, sorting) permanently'
      ImageIndex = 10
      OnClick = menuViewSaveClick
    end
    object menuViewDefault: TMenuItem
      Caption = 'Load xyz by default'
      Checked = True
      Visible = False
      OnClick = menuViewDefaultClick
    end
    object N25: TMenuItem
      Caption = '-'
    end
  end
end
