object frmSQLhelp: TfrmSQLhelp
  Left = 0
  Top = 0
  Caption = 'Integrated SQL-help'
  ClientHeight = 355
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnSearchOnline: TButton
    Left = 363
    Top = 322
    Width = 103
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Search online'
    ImageIndex = 69
    Images = MainForm.VirtualImageListMain
    TabOrder = 0
    OnClick = ButtonOnlinehelpClick
  end
  object ButtonClose: TButton
    Left = 472
    Top = 322
    Width = 102
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 1
    OnClick = ButtonCloseClick
  end
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 566
    Height = 307
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 40
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 153
      Top = 0
      Width = 8
      Height = 307
      Cursor = crSizeWE
      ResizeStyle = rsUpdate
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 153
      Height = 307
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object editFilter: TButtonedEdit
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 153
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Images = MainForm.VirtualImageListMain
        LeftButton.Hint = 'Search'
        LeftButton.ImageIndex = 53
        LeftButton.Visible = True
        RightButton.ImageIndex = 193
        TabOrder = 0
        TextHint = 'Filter'
        OnChange = editFilterChange
        OnRightButtonClick = editFilterRightButtonClick
      end
      object treeTopics: TVirtualStringTree
        AlignWithMargins = True
        Left = 0
        Top = 24
        Width = 153
        Height = 283
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.MainColumn = -1
        Images = MainForm.VirtualImageListMain
        TabOrder = 1
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
        OnFocusChanged = treeTopicsFocusChanged
        OnFreeNode = treeTopicsFreeNode
        OnGetText = treeTopicsGetText
        OnGetImageIndex = treeTopicsGetImageIndex
        OnGetNodeDataSize = treeTopicsGetNodeDataSize
        OnInitChildren = treeTopicsInitChildren
        OnInitNode = treeTopicsInitNode
        Columns = <>
      end
    end
    object pnlRight: TPanel
      Left = 161
      Top = 0
      Width = 405
      Height = 307
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter2: TSplitter
        Left = 0
        Top = 182
        Width = 405
        Height = 8
        Cursor = crSizeNS
        Align = alTop
        ResizeStyle = rsUpdate
      end
      object lblDescription: TLabel
        Left = 0
        Top = 16
        Width = 405
        Height = 13
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Description:'
      end
      object lblKeyword: TLabel
        Left = 0
        Top = 0
        Width = 405
        Height = 13
        Align = alTop
        ShowAccelChar = False
      end
      object lblExample: TLabel
        Left = 0
        Top = 190
        Width = 405
        Height = 13
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Example:'
      end
      object memoDescription: TSynMemo
        Left = 0
        Top = 29
        Width = 405
        Height = 153
        SingleLineMode = False
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        OnKeyDown = memosKeyDown
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Gutter.Width = 0
        Highlighter = URIHighlighter
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        RightEdge = 0
        WordWrap = True
      end
      object MemoExample: TSynMemo
        Left = 0
        Top = 203
        Width = 405
        Height = 104
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 1
        OnKeyDown = memosKeyDown
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Gutter.Width = 0
        Highlighter = URIHighlighter
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        RightEdge = 0
        WordWrap = True
      end
    end
  end
  object URIOpenerDescription: TSynURIOpener
    CtrlActivatesLinks = False
    Editor = memoDescription
    URIHighlighter = URIHighlighter
    Left = 448
    Top = 8
  end
  object URIHighlighter: TSynURISyn
    Left = 416
    Top = 8
  end
  object URIOpenerExample: TSynURIOpener
    Editor = MemoExample
    URIHighlighter = URIHighlighter
    Left = 480
    Top = 8
  end
  object timerSearch: TTimer
    Interval = 500
    OnTimer = DoSearch
    Left = 8
    Top = 320
  end
end
