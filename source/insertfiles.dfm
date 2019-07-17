object frmInsertFiles: TfrmInsertFiles
  Left = 262
  Top = 131
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Insert files...'
  ClientHeight = 491
  ClientWidth = 511
  Color = clBtnFace
  Constraints.MinHeight = 353
  Constraints.MinWidth = 475
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    511
    491)
  PixelsPerInch = 96
  TextHeight = 13
  object btnInsert: TButton
    Left = 272
    Top = 458
    Width = 130
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Import files'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
    OnClick = btnInsertClick
  end
  object btnCancel: TButton
    Left = 408
    Top = 458
    Width = 95
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object grpSelectObject: TGroupBox
    Left = 8
    Top = 8
    Width = 495
    Height = 234
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Target table and columns'
    TabOrder = 2
    DesignSize = (
      495
      234)
    object lblTable: TLabel
      Left = 10
      Top = 26
      Width = 81
      Height = 13
      Caption = 'Database, table:'
    end
    object lblFilecontents: TLabel
      Left = 10
      Top = 53
      Width = 376
      Height = 13
      Caption = 
        'Column values (Hint: Assign "%filecontent%" value to a BLOB or T' +
        'EXT column)'
    end
    object comboDBs: TComboBox
      Left = 170
      Top = 23
      Width = 151
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = comboDBsChange
    end
    object comboTables: TComboBox
      Left = 327
      Top = 23
      Width = 159
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = comboTablesChange
    end
    object ListColumns: TVirtualStringTree
      Left = 10
      Top = 72
      Width = 476
      Height = 151
      Anchors = [akLeft, akTop, akRight]
      EditDelay = 0
      Header.AutoSizeIndex = 2
      Header.Images = MainForm.VirtualImageListMain
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Header.PopupMenu = MainForm.popupListHeader
      TabOrder = 2
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
      TreeOptions.SelectionOptions = [toExtendedFocus]
      OnCreateEditor = ListColumnsCreateEditor
      OnEditing = ListColumnsEditing
      OnFreeNode = ListColumnsFreeNode
      OnGetText = ListColumnsGetText
      OnPaintText = ListColumnsPaintText
      OnGetNodeDataSize = ListColumnsGetNodeDataSize
      OnNewText = ListColumnsNewText
      Columns = <
        item
          Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
          Position = 0
          Text = 'Column'
          Width = 100
        end
        item
          Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
          Position = 1
          Text = 'Datatype'
          Width = 80
        end
        item
          Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
          Position = 2
          Text = 'Value'
          Width = 296
        end>
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 248
    Width = 495
    Height = 204
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Files to import'
    TabOrder = 3
    DesignSize = (
      495
      204)
    object lblDropHint: TLabel
      Left = 248
      Top = 18
      Width = 235
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Hint: You can drop files from your Windows Explorer onto the lis' +
        't.'
      Layout = tlCenter
    end
    object lblFileCount: TLabel
      Left = 10
      Top = 179
      Width = 28
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '0 files'
    end
    object ListFiles: TVirtualStringTree
      Left = 10
      Top = 45
      Width = 476
      Height = 128
      Anchors = [akLeft, akTop, akRight, akBottom]
      Header.AutoSizeIndex = 0
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoHotTrack, hoShowImages, hoShowSortGlyphs, hoVisible]
      Header.PopupMenu = MainForm.popupListHeader
      Header.SortColumn = 0
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
      TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
      OnAfterCellPaint = GridAfterCellPaint
      OnBeforeCellPaint = ListFilesBeforeCellPaint
      OnChange = ListFilesChange
      OnClick = GridClick
      OnCompareNodes = ListFilesCompareNodes
      OnDblClick = ListFilesDblClick
      OnFreeNode = ListFilesFreeNode
      OnGetText = ListFilesGetText
      OnGetImageIndex = ListFilesGetImageIndex
      OnGetNodeDataSize = ListFilesGetNodeDataSize
      OnHeaderClick = ListFilesHeaderClick
      OnKeyPress = GridKeyPress
      OnKeyUp = ListFilesKeyUp
      OnStructureChange = ListFilesStructureChange
      Columns = <
        item
          Position = 0
          Text = 'Filename'
          Width = 326
        end
        item
          CheckBox = True
          Position = 1
          Text = 'Binary'
          Width = 70
        end
        item
          Alignment = taRightJustify
          Position = 2
          Text = 'Size'
          Width = 80
        end>
    end
    object ToolBar1: TToolBar
      Left = 10
      Top = 18
      Width = 232
      Height = 22
      Align = alNone
      ButtonWidth = 66
      Caption = 'ToolBarFiles'
      Images = MainForm.VirtualImageListMain
      List = True
      ShowCaptions = True
      TabOrder = 1
      object btnAddFiles: TToolButton
        Left = 0
        Top = 0
        Caption = 'Add'
        ImageIndex = 45
        OnClick = btnAddFilesClick
      end
      object btnRemoveFiles: TToolButton
        Left = 66
        Top = 0
        Caption = 'Remove'
        Enabled = False
        ImageIndex = 46
        OnClick = btnRemoveFilesClick
      end
      object btnClearFiles: TToolButton
        Left = 132
        Top = 0
        Caption = 'Clear'
        Enabled = False
        ImageIndex = 26
        OnClick = btnRemoveFilesClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'All files (*.*)|*.*|Common images (*.jpg, *.gif, *.bmp, *.png)|*' +
      '.jpg;*.gif;*.bmp;*.png'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 456
  end
end
