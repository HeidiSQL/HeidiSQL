object frmTextEditor: TfrmTextEditor
  Left = 0
  Top = 0
  Caption = 'Text editor'
  ClientHeight = 153
  ClientWidth = 482
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 14
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 131
    Width = 472
    Height = 22
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object lblTextLength: TLabel
      AlignWithMargins = True
      Left = 409
      Top = 3
      Width = 76
      Height = 16
      Align = alLeft
      BiDiMode = bdLeftToRight
      Caption = 'lblTextLength'
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object tlbStandard: TToolBar
      Left = 0
      Top = 0
      Width = 261
      Height = 22
      Align = alLeft
      AutoSize = True
      Caption = 'tlbStandard'
      Images = MainForm.VirtualImageListMain
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Wrapable = False
      object btnWrap: TToolButton
        Left = 0
        Top = 0
        Hint = 'Wrap long lines'
        Caption = 'Wrap long lines'
        ImageIndex = 62
        ImageName = 'icons8-word-wrap'
        OnClick = btnWrapClick
      end
      object btnLinebreaks: TToolButton
        Left = 23
        Top = 0
        Caption = 'Linebreaks'
        DropdownMenu = popupLinebreaks
        ImageIndex = 123
        ImageName = 'icons8-windows-xp'
        Style = tbsDropDown
      end
      object btnLoadText: TToolButton
        Left = 65
        Top = 0
        Hint = 'Load textfile'
        Caption = 'Load textfile'
        ImageIndex = 52
        ImageName = 'icons8-opened-folder'
        OnClick = btnLoadTextClick
      end
      object btnCancel: TToolButton
        Left = 88
        Top = 0
        Hint = 'Cancel'
        Caption = 'Cancel'
        ImageIndex = 26
        ImageName = 'icons8-close-button'
        OnClick = btnCancelClick
      end
      object btnApply: TToolButton
        Left = 111
        Top = 0
        Hint = 'Apply changes'
        Caption = 'Apply changes'
        ImageIndex = 55
        ImageName = 'icons8-checked'
        OnClick = btnApplyClick
      end
      object btnSeparator1: TToolButton
        Left = 134
        Top = 0
        Width = 8
        Caption = 'btnSeparator1'
        ImageIndex = 60
        ImageName = 'icons8-sort-left'
        Style = tbsSeparator
      end
      object btnSearchFind: TToolButton
        Left = 142
        Top = 0
        Action = MainForm.actQueryFind
      end
      object btnSearchFindNext: TToolButton
        Left = 165
        Top = 0
        Action = MainForm.actQueryFindAgain
      end
      object btnSearchReplace: TToolButton
        Left = 188
        Top = 0
        Action = MainForm.actQueryReplace
      end
      object ToolButton1: TToolButton
        Left = 211
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 60
        Style = tbsSeparator
      end
      object btnCustomizeHighlighter: TToolButton
        Left = 219
        Top = 0
        Caption = 'Customize highlighter'
        DropdownMenu = popupHighlighter
        ImageIndex = 39
        Style = tbsDropDown
        OnClick = btnCustomizeHighlighterClick
      end
    end
    object comboHighlighter: TComboBox
      Left = 261
      Top = 0
      Width = 145
      Height = 22
      Align = alLeft
      Style = csDropDownList
      Sorted = True
      TabOrder = 1
      OnSelect = comboHighlighterSelect
    end
  end
  object MemoText: TSynMemo
    Left = 0
    Top = 0
    Width = 482
    Height = 131
    SingleLineMode = False
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = popupEditor
    TabOrder = 1
    OnClick = MemoTextClick
    OnKeyDown = MemoTextKeyDown
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.AutoSize = True
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.ShowModification = True
    Lines.Strings = (
      'MemoText')
    Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces]
    RightEdge = 0
    WantTabs = True
    OnChange = MemoTextChange
    FontSmoothing = fsmNone
  end
  object popupLinebreaks: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 8
    Top = 16
    object menuWindowsLB: TMenuItem
      Caption = 'Windows linebreaks'
      ImageIndex = 123
      ImageName = 'icons8-windows-xp'
      OnClick = SelectLinebreaks
    end
    object menuUnixLB: TMenuItem
      Caption = 'UNIX linebreaks'
      ImageIndex = 125
      ImageName = 'icons8-linux'
      OnClick = SelectLinebreaks
    end
    object menuMacLB: TMenuItem
      Caption = 'Mac OS linebreaks'
      ImageIndex = 124
      ImageName = 'icons8-apple-logo'
      OnClick = SelectLinebreaks
    end
    object menuWideLB: TMenuItem
      Caption = 'Unicode linebreaks'
      ImageIndex = 68
      ImageName = 'icons8-brief'
      OnClick = SelectLinebreaks
    end
    object menuMixedLB: TMenuItem
      Caption = 'Mixed linebreaks'
      ImageIndex = 122
      ImageName = 'icons8-refresh'
      OnClick = SelectLinebreaks
    end
  end
  object TimerMemoChange: TTimer
    Interval = 200
    OnTimer = TimerMemoChangeTimer
    Left = 120
    Top = 16
  end
  object popupEditor: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 240
    Top = 16
    object Selectall1: TMenuItem
      Action = MainForm.actSelectAll
    end
    object Copy1: TMenuItem
      Action = MainForm.actCopy
    end
    object Paste1: TMenuItem
      Action = MainForm.actPaste
    end
    object Undo1: TMenuItem
      Action = MainForm.actUndo
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Findtext1: TMenuItem
      Action = MainForm.actQueryFind
    end
    object Findorreplaceagain1: TMenuItem
      Action = MainForm.actQueryFindAgain
    end
    object Replacetext1: TMenuItem
      Action = MainForm.actQueryReplace
    end
  end
  object popupHighlighter: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 352
    Top = 32
    object menuCustomizeHighlighter: TMenuItem
      Caption = 'Customize highlighter'
      ImageIndex = 39
      OnClick = btnCustomizeHighlighterClick
    end
    object menuFormatCodeOnce: TMenuItem
      Caption = 'Format code once'
      OnClick = menuFormatCodeOnceClick
    end
    object menuAlwaysFormatCode: TMenuItem
      AutoCheck = True
      Caption = 'Always format code'
      OnClick = menuAlwaysFormatCodeClick
    end
  end
end
