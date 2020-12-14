object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Main'
  ClientHeight = 134
  ClientWidth = 257
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
    Left = 48
    Top = 16
    Width = 161
    Height = 25
    Caption = 'Hook'
    TabOrder = 0
    OnClick = BtnHookClick
  end
  object BtnMsgBox: TButton
    Left = 48
    Top = 55
    Width = 161
    Height = 25
    Caption = 'MessageBox'
    TabOrder = 1
    OnClick = BtnMsgBoxClick
  end
  object BtnUnHook: TButton
    Left = 48
    Top = 91
    Width = 161
    Height = 25
    Caption = 'Unhook'
    TabOrder = 2
    OnClick = BtnUnHookClick
  end
end
