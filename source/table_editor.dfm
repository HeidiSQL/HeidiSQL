object frmTableEditor: TfrmTableEditor
  Left = 0
  Top = 0
  Width = 700
  Height = 500
  TabOrder = 0
  DesignSize = (
    700
    500)
  object SplitterTopBottom: TSplitter
    AlignWithMargins = True
    Left = 3
    Top = 153
    Width = 694
    Height = 8
    Cursor = crSizeNS
    Margins.Top = 0
    Margins.Bottom = 0
    Align = alTop
    ResizeStyle = rsUpdate
  end
  object btnSave: TButton
    Left = 165
    Top = 471
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 5
    OnClick = btnSaveClick
  end
  object btnDiscard: TButton
    Left = 84
    Top = 471
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Discard'
    TabOrder = 4
    OnClick = btnDiscardClick
  end
  object btnHelp: TButton
    Left = 3
    Top = 471
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object listColumns: TVirtualStringTree
    AlignWithMargins = True
    Left = 3
    Top = 186
    Width = 694
    Height = 282
    Margins.Bottom = 32
    Align = alClient
    Constraints.MinHeight = 70
    DragMode = dmAutomatic
    EditDelay = 0
    Header.AutoSizeIndex = -1
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoVisible]
    Header.ParentFont = True
    Header.PopupMenu = MainForm.popupListHeader
    Images = MainForm.ImageListMain
    IncrementalSearch = isAll
    PopupMenu = popupColumns
    TabOrder = 2
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag, toEditOnClick]
    TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
    WantTabs = True
    OnAfterCellPaint = listColumnsAfterCellPaint
    OnBeforeCellPaint = listColumnsBeforeCellPaint
    OnClick = listColumnsClick
    OnCreateEditor = listColumnsCreateEditor
    OnDragOver = listColumnsDragOver
    OnDragDrop = listColumnsDragDrop
    OnEditing = listColumnsEditing
    OnFocusChanged = listColumnsFocusChanged
    OnGetText = listColumnsGetText
    OnPaintText = listColumnsPaintText
    OnGetNodeDataSize = listColumnsGetNodeDataSize
    OnInitNode = listColumnsInitNode
    OnKeyPress = listColumnsKeyPress
    OnNewText = listColumnsNewText
    OnNodeMoved = listColumnsNodeMoved
    Columns = <
      item
        Alignment = taRightJustify
        Color = clBtnFace
        MinWidth = 20
        Options = [coDraggable, coEnabled, coParentBidiMode, coShowDropMark, coVisible, coAllowFocus]
        Position = 0
        Width = 20
        WideText = '#'
      end
      item
        MinWidth = 50
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 1
        Width = 100
        WideText = 'Name'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 2
        Width = 90
        WideText = 'Datatype'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 3
        Width = 90
        WideText = 'Length/Set'
      end
      item
        Alignment = taCenter
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 4
        Width = 60
        WideText = 'Unsigned'
      end
      item
        Alignment = taCenter
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 5
        Width = 65
        WideText = 'Allow NULL'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 6
        WideText = 'Zerofill'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 7
        Width = 100
        WideText = 'Default'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 8
        Width = 130
        WideText = 'Comment'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 9
        Width = 100
        WideText = 'Collation'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 10
        Width = 100
        WideText = 'Expression'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 11
        Width = 100
        WideText = 'Virtuality'
      end>
  end
  object PageControlMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 694
    Height = 150
    Margins.Bottom = 0
    ActivePage = tabBasic
    Align = alTop
    Images = MainForm.ImageListMain
    TabOrder = 0
    OnChange = PageControlMainChange
    object tabBasic: TTabSheet
      Caption = 'Basic'
      ImageIndex = 14
      DesignSize = (
        686
        121)
      object lblName: TLabel
        Left = 4
        Top = 6
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblComment: TLabel
        Left = 4
        Top = 33
        Width = 49
        Height = 13
        Caption = 'Comment:'
      end
      object editName: TEdit
        Left = 96
        Top = 3
        Width = 589
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'editName'
        TextHint = 'Enter table name'
        OnChange = Modification
      end
      object memoComment: TMemo
        Left = 96
        Top = 30
        Width = 589
        Height = 80
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'memoComment')
        MaxLength = 60
        ScrollBars = ssVertical
        TabOrder = 1
        OnChange = Modification
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 39
      DesignSize = (
        686
        121)
      object lblAutoinc: TLabel
        Left = 4
        Top = 6
        Width = 77
        Height = 13
        Caption = 'Auto increment:'
      end
      object lblAvgRowLen: TLabel
        Left = 4
        Top = 29
        Width = 99
        Height = 13
        Caption = 'Average row length:'
      end
      object lblInsertMethod: TLabel
        Left = 294
        Top = 98
        Width = 79
        Height = 13
        Caption = 'INSERT method:'
      end
      object lblUnion: TLabel
        Left = 294
        Top = 52
        Width = 63
        Height = 13
        Caption = 'Union tables:'
      end
      object lblMaxRows: TLabel
        Left = 4
        Top = 52
        Width = 99
        Height = 13
        Caption = 'Maximum row count:'
      end
      object lblRowFormat: TLabel
        Left = 4
        Top = 98
        Width = 60
        Height = 13
        Caption = 'Row format:'
      end
      object lblCollation: TLabel
        Left = 294
        Top = 6
        Width = 81
        Height = 13
        Caption = 'Default collation:'
      end
      object lblEngine: TLabel
        Left = 294
        Top = 29
        Width = 36
        Height = 13
        Caption = 'Engine:'
      end
      object editAvgRowLen: TEdit
        Left = 178
        Top = 26
        Width = 110
        Height = 21
        TabOrder = 1
        OnChange = editNumEditChange
      end
      object editMaxRows: TEdit
        Left = 178
        Top = 49
        Width = 110
        Height = 21
        TabOrder = 2
        OnChange = editNumEditChange
      end
      object chkChecksum: TCheckBox
        Left = 4
        Top = 75
        Width = 189
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Checksum for rows:'
        TabOrder = 3
        OnClick = Modification
      end
      object comboRowFormat: TComboBox
        Left = 178
        Top = 95
        Width = 110
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = Modification
      end
      object memoUnionTables: TMemo
        Left = 408
        Top = 49
        Width = 277
        Height = 44
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'memoUnion')
        TabOrder = 7
        OnChange = Modification
      end
      object comboInsertMethod: TComboBox
        Left = 408
        Top = 95
        Width = 277
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
        OnClick = Modification
      end
      object editAutoInc: TEdit
        Left = 178
        Top = 3
        Width = 110
        Height = 21
        TabOrder = 0
        OnChange = editNumEditChange
      end
      object comboCollation: TComboBox
        Left = 408
        Top = 3
        Width = 158
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        Sorted = True
        TabOrder = 5
        OnChange = chkCharsetConvertClick
      end
      object comboEngine: TComboBox
        Left = 408
        Top = 26
        Width = 277
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        OnSelect = comboEngineSelect
      end
      object chkCharsetConvert: TCheckBox
        Left = 574
        Top = 5
        Width = 107
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Convert data'
        TabOrder = 9
        OnClick = chkCharsetConvertClick
      end
    end
    object tabIndexes: TTabSheet
      Caption = 'Indexes'
      ImageIndex = 13
      object treeIndexes: TVirtualStringTree
        AlignWithMargins = True
        Left = 69
        Top = 0
        Width = 614
        Height = 121
        Margins.Top = 0
        Margins.Bottom = 0
        Align = alClient
        DragMode = dmAutomatic
        EditDelay = 0
        Header.AutoSizeIndex = 0
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Header.ParentFont = True
        Images = MainForm.ImageListMain
        PopupMenu = popupIndexes
        TabOrder = 1
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme, toHideTreeLinesIfThemed]
        TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
        OnBeforePaint = treeIndexesBeforePaint
        OnClick = treeIndexesClick
        OnCreateEditor = treeIndexesCreateEditor
        OnDragOver = treeIndexesDragOver
        OnDragDrop = treeIndexesDragDrop
        OnEditing = treeIndexesEditing
        OnFocusChanged = treeIndexesFocusChanged
        OnGetText = treeIndexesGetText
        OnGetImageIndex = treeIndexesGetImageIndex
        OnInitChildren = treeIndexesInitChildren
        OnInitNode = treeIndexesInitNode
        OnNewText = treeIndexesNewText
        Columns = <
          item
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 0
            Width = 430
            WideText = 'Name'
          end
          item
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
            Position = 1
            Width = 100
            WideText = 'Type / Length'
          end
          item
            Position = 2
            Width = 80
            WideText = 'Algorithm'
          end>
      end
      object tlbIndexes: TToolBar
        Left = 0
        Top = 0
        Width = 66
        Height = 121
        Align = alLeft
        AutoSize = True
        ButtonWidth = 66
        Caption = 'tlbIndexes'
        Images = MainForm.ImageListMain
        List = True
        ShowCaptions = True
        TabOrder = 0
        object btnAddIndex: TToolButton
          Left = 0
          Top = 0
          Hint = 'Add index'
          Caption = 'Add'
          ImageIndex = 45
          Wrap = True
          OnClick = btnAddIndexClick
        end
        object btnRemoveIndex: TToolButton
          Left = 0
          Top = 22
          Hint = 'Remove index'
          Caption = 'Remove'
          ImageIndex = 46
          Wrap = True
          OnClick = btnRemoveIndexClick
        end
        object btnClearIndexes: TToolButton
          Left = 0
          Top = 44
          Hint = 'Clear indexes'
          Caption = 'Clear'
          ImageIndex = 26
          Wrap = True
          OnClick = btnClearIndexesClick
        end
        object btnMoveUpIndex: TToolButton
          Left = 0
          Top = 66
          Hint = 'Move up'
          Caption = 'Up'
          ImageIndex = 74
          Wrap = True
          OnClick = btnMoveUpIndexClick
        end
        object btnMoveDownIndex: TToolButton
          Left = 0
          Top = 88
          Hint = 'Move down'
          Caption = 'Down'
          ImageIndex = 75
          OnClick = btnMoveDownIndexClick
        end
      end
    end
    object tabForeignKeys: TTabSheet
      Caption = 'Foreign keys'
      ImageIndex = 136
      object tlbForeignKeys: TToolBar
        Left = 0
        Top = 0
        Width = 66
        Height = 121
        Align = alLeft
        AutoSize = True
        ButtonWidth = 66
        Caption = 'tlbForeignKeys'
        Images = MainForm.ImageListMain
        List = True
        ShowCaptions = True
        TabOrder = 0
        object btnAddForeignKey: TToolButton
          Left = 0
          Top = 0
          Caption = 'Add'
          ImageIndex = 45
          Wrap = True
          OnClick = btnAddForeignKeyClick
        end
        object btnRemoveForeignKey: TToolButton
          Left = 0
          Top = 22
          Caption = 'Remove'
          Enabled = False
          ImageIndex = 46
          Wrap = True
          OnClick = btnRemoveForeignKeyClick
        end
        object btnClearForeignKeys: TToolButton
          Left = 0
          Top = 44
          Caption = 'Clear'
          Enabled = False
          ImageIndex = 26
          OnClick = btnClearForeignKeysClick
        end
      end
      object pnlNoForeignKeys: TPanel
        Left = 66
        Top = 0
        Width = 527
        Height = 121
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlNoForeignKeys'
        TabOrder = 1
        VerticalAlignment = taAlignBottom
        object listForeignKeys: TVirtualStringTree
          AlignWithMargins = True
          Left = 3
          Top = 0
          Width = 521
          Height = 121
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alClient
          EditDelay = 0
          Header.AutoSizeIndex = 0
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          Header.ParentFont = True
          Images = MainForm.ImageListMain
          TabOrder = 0
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
          TreeOptions.SelectionOptions = [toExtendedFocus]
          OnBeforePaint = listForeignKeysBeforePaint
          OnClick = treeIndexesClick
          OnCreateEditor = listForeignKeysCreateEditor
          OnEditing = listForeignKeysEditing
          OnFocusChanged = listForeignKeysFocusChanged
          OnGetText = listForeignKeysGetText
          OnGetImageIndex = listForeignKeysGetImageIndex
          OnNewText = listForeignKeysNewText
          Columns = <
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
              Position = 0
              Width = 101
              WideText = 'Key name'
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
              Position = 1
              Width = 80
              WideText = 'Columns'
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
              Position = 2
              Width = 100
              WideText = 'Reference table'
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
              Position = 3
              Width = 80
              WideText = 'Foreign columns'
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
              Position = 4
              Width = 80
              WideText = 'On UPDATE'
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
              Position = 5
              Width = 80
              WideText = 'On DELETE'
            end>
        end
      end
    end
    object tabPartitions: TTabSheet
      Caption = 'Partitions'
      ImageIndex = 186
      object SynMemoPartitions: TSynMemo
        Left = 0
        Top = 0
        Width = 593
        Height = 121
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        Highlighter = MainForm.SynSQLSyn1
        Lines.Strings = (
          'SynMemoPartitions')
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        FontSmoothing = fsmNone
      end
    end
    object tabCREATEcode: TTabSheet
      Caption = 'CREATE code'
      ImageIndex = 119
      object SynMemoCREATEcode: TSynMemo
        Left = 0
        Top = 0
        Width = 593
        Height = 121
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        Highlighter = MainForm.SynSQLSyn1
        Lines.Strings = (
          'SynMemoALTERcode')
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        FontSmoothing = fsmNone
      end
    end
    object tabALTERCode: TTabSheet
      Caption = 'ALTER code'
      ImageIndex = 119
      object SynMemoALTERcode: TSynMemo
        Left = 0
        Top = 0
        Width = 593
        Height = 121
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        Highlighter = MainForm.SynSQLSyn1
        Lines.Strings = (
          'SynMemoALTERcode')
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        FontSmoothing = fsmNone
      end
    end
  end
  object pnlColumnsTop: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 161
    Width = 694
    Height = 22
    Margins.Top = 0
    Margins.Bottom = 0
    Align = alTop
    Alignment = taLeftJustify
    AutoSize = True
    BevelOuter = bvNone
    Caption = 'Columns:'
    TabOrder = 1
    object tlbColumns: TToolBar
      AlignWithMargins = True
      Left = 100
      Top = 0
      Width = 594
      Height = 22
      Margins.Left = 100
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      ButtonWidth = 66
      Caption = 'Columns:'
      Images = MainForm.ImageListMain
      List = True
      ShowCaptions = True
      TabOrder = 0
      object btnAddColumn: TToolButton
        Left = 0
        Top = 0
        Hint = 'Add column'
        Caption = 'Add'
        ImageIndex = 45
        OnClick = btnAddColumnClick
      end
      object btnRemoveColumn: TToolButton
        Left = 66
        Top = 0
        Hint = 'Remove column'
        Caption = 'Remove'
        ImageIndex = 46
        OnClick = btnRemoveColumnClick
      end
      object btnMoveUpColumn: TToolButton
        Left = 132
        Top = 0
        Hint = 'Move up'
        Caption = 'Up'
        ImageIndex = 74
        OnClick = btnMoveUpColumnClick
      end
      object btnMoveDownColumn: TToolButton
        Left = 198
        Top = 0
        Hint = 'Move down'
        Caption = 'Down'
        ImageIndex = 75
        OnClick = btnMoveDownColumnClick
      end
    end
  end
  object popupIndexes: TPopupMenu
    Images = MainForm.ImageListMain
    Left = 344
    Top = 360
    object menuAddIndex: TMenuItem
      Caption = 'Add index'
      ImageIndex = 45
      ShortCut = 16429
      OnClick = btnAddIndexClick
    end
    object menuAddIndexColumn: TMenuItem
      Caption = 'Add column'
      ImageIndex = 91
      ShortCut = 24621
      OnClick = menuAddIndexColumnClick
    end
    object menuRemoveIndex: TMenuItem
      Caption = 'Remove'
      ImageIndex = 46
      ShortCut = 16430
      OnClick = btnRemoveIndexClick
    end
    object menuClearIndexes: TMenuItem
      Caption = 'Clear'
      ImageIndex = 26
      ShortCut = 24622
      OnClick = btnClearIndexesClick
    end
    object menuMoveUpIndex: TMenuItem
      Caption = 'Up'
      ImageIndex = 74
      ShortCut = 16469
      OnClick = btnMoveUpIndexClick
    end
    object menuMoveDownIndex: TMenuItem
      Caption = 'Down'
      ImageIndex = 75
      ShortCut = 16452
      OnClick = btnMoveDownIndexClick
    end
  end
  object popupColumns: TPopupMenu
    Images = MainForm.ImageListMain
    OnPopup = popupColumnsPopup
    Left = 312
    Top = 360
    object menuCopyColumnCell: TMenuItem
      Action = MainForm.actCopy
    end
    object menuCopyColumns: TMenuItem
      Caption = 'Copy selected columns'
      ImageIndex = 155
      OnClick = menuCopyColumnsClick
    end
    object menuPasteColumns: TMenuItem
      Caption = 'Paste columns'
      ImageIndex = 156
      OnClick = menuPasteColumnsClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object menuAddColumn: TMenuItem
      Caption = 'Add column'
      ImageIndex = 45
      ShortCut = 16429
      OnClick = btnAddColumnClick
    end
    object menuRemoveColumn: TMenuItem
      Caption = 'Remove column'
      ImageIndex = 46
      ShortCut = 16430
      OnClick = btnRemoveColumnClick
    end
    object menuMoveUpColumn: TMenuItem
      Caption = 'Move up'
      ImageIndex = 74
      ShortCut = 16469
      OnClick = btnMoveUpColumnClick
    end
    object menuMoveDownColumn: TMenuItem
      Caption = 'Move down'
      ImageIndex = 75
      ShortCut = 16452
      OnClick = btnMoveDownColumnClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object menuCreateIndex: TMenuItem
      Caption = 'Create new index'
      ImageIndex = 13
    end
    object menuAddToIndex: TMenuItem
      Caption = 'Add to index'
      ImageIndex = 13
    end
  end
end
