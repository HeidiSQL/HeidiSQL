object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 264
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
    Height = 89
    Align = alTop
    Lines.Strings = (
      'MemLog')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BtnEnableHook1: TButton
    Left = 292
    Top = 120
    Width = 147
    Height = 25
    Caption = 'Enable Hook1'
    TabOrder = 1
    OnClick = BtnEnableHook1Click
  end
  object BtnEnableHook2: TButton
    Left = 292
    Top = 151
    Width = 147
    Height = 25
    Caption = 'Enable Hook2'
    TabOrder = 2
    OnClick = BtnEnableHook2Click
  end
  object BtnEnableHook3: TButton
    Left = 292
    Top = 182
    Width = 147
    Height = 25
    Caption = 'Enable Hook3'
    TabOrder = 3
    OnClick = BtnEnableHook3Click
  end
  object BtnRemoveHook1: TButton
    Left = 28
    Top = 120
    Width = 147
    Height = 25
    Caption = 'Remove Hook1'
    TabOrder = 4
    OnClick = BtnRemoveHook1Click
  end
  object BtnRemoveHook2: TButton
    Left = 28
    Top = 151
    Width = 147
    Height = 25
    Caption = 'Remove Hook2'
    TabOrder = 5
    OnClick = BtnRemoveHook2Click
  end
  object BtnRemoveHook3: TButton
    Left = 28
    Top = 182
    Width = 147
    Height = 25
    Caption = 'Remove Hook3'
    TabOrder = 6
    OnClick = BtnRemoveHook3Click
  end
  object BtnTest: TButton
    Left = 292
    Top = 231
    Width = 147
    Height = 25
    Caption = 'Test'
    TabOrder = 7
    OnClick = BtnTestClick
  end
end
