object OptionsForm: TOptionsForm
  Left = 394
  Top = 254
  VertScrollBar.Smooth = True
  VertScrollBar.Tracking = True
  BorderStyle = bsSingle
  ClientHeight = 480
  ClientWidth = 592
  Color = clBtnFace
  Constraints.MinHeight = 470
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    592
    480)
  PixelsPerInch = 96
  TextHeight = 14
  object List: TTreeView
    Left = 0
    Top = 0
    Width = 177
    Height = 428
    Anchors = [akLeft, akTop, akBottom]
    Indent = 19
    ReadOnly = True
    ShowButtons = False
    ShowRoot = False
    TabOrder = 0
    OnClick = ListClick
  end
end
