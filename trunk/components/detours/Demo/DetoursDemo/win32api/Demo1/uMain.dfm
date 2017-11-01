object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Hooking MessageBox'
  ClientHeight = 133
  ClientWidth = 188
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnHook: TButton
    Left = 16
    Top = 24
    Width = 153
    Height = 25
    Caption = 'Hook MessageBox'
    TabOrder = 0
    OnClick = BtnHookClick
  end
  object BtnUnHook: TButton
    Left = 16
    Top = 88
    Width = 153
    Height = 25
    Caption = 'UnHook MessageBox'
    TabOrder = 1
    OnClick = BtnUnHookClick
  end
  object BtnTestMsgBox: TButton
    Left = 16
    Top = 57
    Width = 153
    Height = 25
    Caption = 'Call MessageBox'
    TabOrder = 2
    OnClick = BtnTestMsgBoxClick
  end
end
