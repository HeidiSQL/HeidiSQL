object frmThemePreview: TfrmThemePreview
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Theme preview'
  ClientHeight = 118
  ClientWidth = 229
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBarMain: TStatusBar
    Left = 0
    Top = 99
    Width = 229
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ScrollBoxImage: TScrollBox
    Left = 0
    Top = 0
    Width = 229
    Height = 99
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 1
    OnMouseWheel = ScrollBoxImageMouseWheel
    object imagePreview: TImage
      Left = 0
      Top = 0
      Width = 100
      Height = 100
      AutoSize = True
    end
  end
end
