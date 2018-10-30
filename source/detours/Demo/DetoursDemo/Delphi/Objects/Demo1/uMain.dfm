object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Hooking TControl.SetTextBuf'
  ClientHeight = 164
  ClientWidth = 178
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnEnableHook: TButton
    Left = 17
    Top = 90
    Width = 151
    Height = 25
    Caption = 'BtnEnableHook'
    TabOrder = 0
    OnClick = BtnEnableHookClick
  end
  object BtnDisableHook: TButton
    Left = 17
    Top = 116
    Width = 151
    Height = 25
    Caption = 'BtnDisableHook'
    TabOrder = 1
    OnClick = BtnDisableHookClick
  end
  object Edit1: TEdit
    Left = 17
    Top = 24
    Width = 151
    Height = 21
    TabOrder = 2
    Text = 'Hi'
  end
  object BtnClickMe: TButton
    Left = 17
    Top = 51
    Width = 151
    Height = 25
    Caption = 'Click Me'
    TabOrder = 3
    OnClick = BtnClickMeClick
  end
end
