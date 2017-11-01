object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Hooking GetMemory'
  ClientHeight = 167
  ClientWidth = 447
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
  object MemLog: TMemo
    Left = 0
    Top = 0
    Width = 447
    Height = 113
    Align = alTop
    Lines.Strings = (
      'MemLog')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BtnTest: TButton
    Left = 302
    Top = 136
    Width = 137
    Height = 25
    Caption = 'Test'
    TabOrder = 1
    OnClick = BtnTestClick
  end
end
