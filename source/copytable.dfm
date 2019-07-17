object CopyTableForm: TCopyTableForm
  Left = 393
  Top = 115
  Caption = 'Copy Table...'
  ClientHeight = 304
  ClientWidth = 364
  Color = clBtnFace
  Constraints.MinHeight = 340
  Constraints.MinWidth = 380
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    364
    304)
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewTablename: TLabel
    Left = 8
    Top = 8
    Width = 135
    Height = 13
    Caption = 'Copy "%s" to new db.table:'
  end
  object lblItems: TLabel
    Left = 8
    Top = 51
    Width = 155
    Height = 13
    Caption = 'Elements to create in new table:'
  end
  object lblWhere: TLabel
    Left = 8
    Top = 167
    Width = 155
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'WHERE clause for data copying:'
  end
  object editNewTablename: TEdit
    Left = 159
    Top = 24
    Width = 197
    Height = 21
    TabOrder = 1
    OnChange = editNewTablenameChange
  end
  object btnCancel: TButton
    Left = 273
    Top = 271
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object comboDatabase: TComboBox
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 184
    Top = 271
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = btnOKClick
  end
  object TreeElements: TVirtualStringTree
    Left = 8
    Top = 70
    Width = 348
    Height = 88
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    Images = MainForm.VirtualImageListMain
    TabOrder = 2
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
    OnChecked = TreeElementsChecked
    OnGetText = TreeElementsGetText
    OnGetImageIndex = TreeElementsGetImageIndex
    OnInitChildren = TreeElementsInitChildren
    OnInitNode = TreeElementsInitNode
    Columns = <>
  end
  object MemoFilter: TSynMemo
    Left = 8
    Top = 192
    Width = 348
    Height = 73
    SingleLineMode = False
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 4
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.ShowLineNumbers = True
    Highlighter = MainForm.SynSQLSynUsed
    Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent]
    WantTabs = True
    FontSmoothing = fsmNone
  end
  object btnRecentFilters: TButton
    Left = 248
    Top = 164
    Width = 108
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Recent filters'
    DropDownMenu = popupRecentFilters
    Style = bsSplitButton
    TabOrder = 3
    OnClick = btnRecentFiltersClick
  end
  object popupRecentFilters: TPopupMenu
    AutoHotkeys = maManual
    Left = 208
    Top = 160
  end
end
