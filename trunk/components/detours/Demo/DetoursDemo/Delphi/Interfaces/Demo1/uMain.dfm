object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Hooking Method in interface'
  ClientHeight = 133
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnCallShowMsg: TButton
    Left = 32
    Top = 24
    Width = 241
    Height = 25
    Caption = 'Call ShowMsg'
    TabOrder = 0
    OnClick = BtnCallShowMsgClick
  end
  object BtnHook: TButton
    Left = 32
    Top = 55
    Width = 241
    Height = 25
    Caption = 'Hook ShowMsg'
    TabOrder = 1
    OnClick = BtnHookClick
  end
  object BtnUnHook: TButton
    Left = 32
    Top = 86
    Width = 241
    Height = 25
    Caption = 'UnHook ShowMsg '
    TabOrder = 2
    OnClick = BtnUnHookClick
  end
end
