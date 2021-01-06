object Main: TMain
  Left = 192
  Top = 125
  Width = 238
  Height = 191
  BorderStyle = bsSizeToolWin
  Caption = 'Main'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtnHook: TButton
    Left = 46
    Top = 24
    Width = 131
    Height = 25
    Caption = 'Hook'
    TabOrder = 0
    OnClick = BtnHookClick
  end
  object BtnMsgBox: TButton
    Left = 46
    Top = 64
    Width = 131
    Height = 25
    Caption = 'MsgBox'
    TabOrder = 1
    OnClick = BtnMsgBoxClick
  end
  object BtnUnhook: TButton
    Left = 46
    Top = 104
    Width = 131
    Height = 25
    Caption = 'Unhook'
    TabOrder = 2
    OnClick = BtnUnhookClick
  end
end
