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
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 153
    Top = 0
    Width = 8
    Height = 336
    Cursor = crSizeWE
    ResizeStyle = rsUpdate
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 153
    Height = 336
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object lblTopics: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 8
      Width = 147
      Height = 13
      Align = alBottom
      Caption = 'Topics:'
    end
    object treeTopics: TTreeView
      Left = 0
      Top = 24
      Width = 153
      Height = 312
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      ChangeDelay = 50
      Images = MainForm.PngImageListMain
      Indent = 19
      ReadOnly = True
      ShowLines = False
      TabOrder = 0
      OnChange = treeTopicsChange
      OnExpanding = treeTopicsExpanding
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 336
    Width = 582
    Height = 19
    Panels = <>
  end
  object pnlRight: TPanel
    Left = 161
    Top = 0
    Width = 421
    Height = 336
    Align = alClient
    BevelOuter = bvNone
    Padding.Top = 3
    Padding.Right = 3
    Padding.Bottom = 3
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 0
      Top = 171
      Width = 418
      Height = 8
      Cursor = crSizeNS
      Align = alTop
      ResizeStyle = rsUpdate
    end
    object pnlRightTop: TPanel
      Left = 0
      Top = 3
      Width = 418
      Height = 168
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 0
      object lblKeyword: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 5
        Width = 418
        Height = 16
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'lblKeyword'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDescription: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 26
        Width = 418
        Height = 13
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Description:'
      end
      object memoDescription: TSynMemo
        AlignWithMargins = True
        Left = 0
        Top = 44
        Width = 418
        Height = 124
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        SingleLineMode = False
        Align = alClient
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
    end
    object pnlRightBottom: TPanel
      Left = 0
      Top = 179
      Width = 418
      Height = 154
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        418
        154)
      object lblExample: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 5
        Width = 418
        Height = 13
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Example:'
      end
      object btnSearchOnline: TPngSpeedButton
        Left = 207
        Top = 128
        Width = 103
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Search online'
        OnClick = ButtonOnlinehelpClick
      end
      object MemoExample: TSynMemo
        AlignWithMargins = True
        Left = 0
        Top = 23
        Width = 418
        Height = 90
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        SingleLineMode = False
        Align = alTop
        Anchors = [akLeft, akTop, akRight, akBottom]
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
      object ButtonClose: TButton
        Left = 316
        Top = 128
        Width = 102
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Close'
        Default = True
        ModalResult = 1
        TabOrder = 1
        OnClick = ButtonCloseClick
      end
    end
  end
  object URIOpenerDescription: TSynURIOpener
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
end
