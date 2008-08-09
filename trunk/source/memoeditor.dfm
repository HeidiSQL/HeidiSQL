object frmMemoEditor: TfrmMemoEditor
  Left = 0
  Top = 0
  Caption = 'frmMemoEditor'
  ClientHeight = 155
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object memoText: TTntMemo
    Left = 0
    Top = 0
    Width = 264
    Height = 137
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'memoText')
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    OnKeyDown = memoTextKeyDown
  end
end
