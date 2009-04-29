object frmTextEditor: TfrmTextEditor
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Text editor'
  ClientHeight = 95
  ClientWidth = 253
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
  DesignSize = (
    253
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTextLength: TLabel
    Left = 137
    Top = 77
    Width = 65
    Height = 13
    Anchors = [akLeft, akBottom]
    BiDiMode = bdLeftToRight
    Caption = 'lblTextLength'
    ParentBiDiMode = False
  end
  object memoText: TTntMemo
    Left = 0
    Top = 0
    Width = 253
    Height = 72
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'memoText')
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    OnChange = memoTextChange
    OnKeyDown = memoTextKeyDown
  end
  object tlbStandard: TToolBar
    Left = 0
    Top = 73
    Width = 131
    Height = 22
    Align = alNone
    Anchors = [akLeft, akBottom]
    Caption = 'tlbStandard'
    Images = MainForm.PngImageListMain
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object btnWrap: TToolButton
      Left = 0
      Top = 0
      Hint = 'Wrap long lines'
      Caption = 'Wrap long lines'
      ImageIndex = 62
      OnClick = btnWrapClick
    end
    object btnLinebreaks: TToolButton
      Left = 23
      Top = 0
      Caption = 'Linebreaks'
      DropdownMenu = popupLinebreaks
      ImageIndex = 123
      Style = tbsDropDown
    end
    object btnLoadText: TToolButton
      Left = 61
      Top = 0
      Hint = 'Load textfile'
      Caption = 'Load textfile'
      ImageIndex = 52
      OnClick = btnLoadTextClick
    end
    object btnCancel: TToolButton
      Left = 84
      Top = 0
      Hint = 'Cancel'
      Caption = 'Cancel'
      ImageIndex = 26
      OnClick = btnCancelClick
    end
    object btnApply: TToolButton
      Left = 107
      Top = 0
      Hint = 'Apply changes'
      Caption = 'Apply changes'
      ImageIndex = 55
      OnClick = btnApplyClick
    end
  end
  object popupLinebreaks: TPopupMenu
    Images = MainForm.PngImageListMain
    Left = 8
    Top = 16
    object menuWindowsLB: TMenuItem
      Caption = 'Windows linebreaks'
      ImageIndex = 123
      OnClick = SelectLinebreaks
    end
    object menuUnixLB: TMenuItem
      Caption = 'UNIX linebreaks'
      ImageIndex = 125
      OnClick = SelectLinebreaks
    end
    object menuMacLB: TMenuItem
      Caption = 'Mac OS linebreaks'
      ImageIndex = 124
      OnClick = SelectLinebreaks
    end
    object menuWideLB: TMenuItem
      Caption = 'Unicode linebreaks'
      ImageIndex = 68
      OnClick = SelectLinebreaks
    end
    object menuMixedLB: TMenuItem
      Caption = 'Mixed linebreaks'
      ImageIndex = 122
      OnClick = SelectLinebreaks
    end
  end
end
