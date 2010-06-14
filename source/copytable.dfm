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
  OnShow = FormShow
  DesignSize = (
    364
    304)
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewTablename: TLabel
    Left = 8
    Top = 8
    Width = 119
    Height = 13
    Caption = 'Copy .. to new db.table:'
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
    Top = 164
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
    Anchors = [akLeft, akTop, akRight]
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
    TabOrder = 4
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
    TabOrder = 5
    OnClick = btnOKClick
  end
  object TreeElements: TVirtualStringTree
    Left = 8
    Top = 70
    Width = 348
    Height = 88
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = MainForm.ImageListMain
    TabOrder = 2
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
    OnChecked = TreeElementsChecked
    OnGetText = TreeElementsGetText
    OnGetImageIndex = TreeElementsGetImageIndex
    OnInitChildren = TreeElementsInitChildren
    OnInitNode = TreeElementsInitNode
    Columns = <>
  end
  object MemoWhereClause: TSynMemo
    Left = 8
    Top = 183
    Width = 348
    Height = 82
    SingleLineMode = False
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 3
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.ShowLineNumbers = True
    Highlighter = MainForm.SynSQLSyn1
    Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent]
    WantTabs = True
  end
end
