object frmMemoEditor: TfrmMemoEditor
  Left = 0
  Top = 0
  Caption = 'frmMemoEditor'
  ClientHeight = 95
  ClientWidth = 215
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 130
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    215
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTextLength: TLabel
    Left = 103
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
    Width = 215
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
    Width = 97
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
    object btnLoadText: TToolButton
      Left = 23
      Top = 0
      Hint = 'Load textfile'
      Caption = 'Load textfile'
      ImageIndex = 52
      OnClick = btnLoadTextClick
    end
    object btnCancel: TToolButton
      Left = 46
      Top = 0
      Hint = 'Cancel'
      Caption = 'Cancel'
      ImageIndex = 26
      OnClick = btnCancelClick
    end
    object btnApply: TToolButton
      Left = 69
      Top = 0
      Hint = 'Apply changes'
      Caption = 'Apply changes'
      ImageIndex = 55
      OnClick = btnApplyClick
    end
  end
end
