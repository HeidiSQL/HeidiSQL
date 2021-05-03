object frmTextEditor: TfrmTextEditor
  Left = 0
  Top = 0
  Caption = 'Text editor'
  ClientHeight = 104
  ClientWidth = 571
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 130
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 82
    Width = 561
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
      Left = 355
      Top = 3
      Width = 65
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
      Width = 207
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
        Left = 61
        Top = 0
        Hint = 'Load textfile'
        Caption = 'Load textfile'
        ImageIndex = 52
        ImageName = 'icons8-opened-folder'
        OnClick = btnLoadTextClick
      end
      object btnCancel: TToolButton
        Left = 84
        Top = 0
        Hint = 'Cancel'
        Caption = 'Cancel'
        ImageIndex = 26
        ImageName = 'icons8-close-button'
        OnClick = btnCancelClick
      end
      object btnApply: TToolButton
        Left = 107
        Top = 0
        Hint = 'Apply changes'
        Caption = 'Apply changes'
        ImageIndex = 55
        ImageName = 'icons8-checked'
        OnClick = btnApplyClick
      end
      object btnSeparator1: TToolButton
        Left = 130
        Top = 0
        Width = 8
        Caption = 'btnSeparator1'
        ImageIndex = 60
        ImageName = 'icons8-sort-left'
        Style = tbsSeparator
      end
      object btnSearchFind: TToolButton
        Left = 138
        Top = 0
        Action = MainForm.actQueryFind
      end
      object btnSearchFindNext: TToolButton
        Left = 161
        Top = 0
        Action = MainForm.actQueryFindAgain
      end
      object btnSearchReplace: TToolButton
        Left = 184
        Top = 0
        Action = MainForm.actQueryReplace
      end
    end
    object comboHighlighter: TComboBox
      Left = 207
      Top = 0
      Width = 145
      Height = 21
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
    Width = 571
    Height = 82
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
end
