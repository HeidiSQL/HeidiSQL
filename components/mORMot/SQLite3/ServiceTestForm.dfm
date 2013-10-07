object MainServiceTest: TMainServiceTest
  Left = 636
  Top = 366
  BorderStyle = bsSingle
  Caption = 'Synopse SQLite3 Framework Service Test'
  ClientHeight = 253
  ClientWidth = 291
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
  object Label1: TLabel
    Left = 24
    Top = 160
    Width = 241
    Height = 73
    AutoSize = False
    Transparent = False
    WordWrap = True
  end
  object BtnStart: TButton
    Left = 24
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Start'
    Enabled = False
    TabOrder = 0
    OnClick = BtnClick
  end
  object BtnStop: TButton
    Left = 24
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = BtnClick
  end
  object BtnCreate: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Install'
    TabOrder = 2
    OnClick = BtnCreateClick
  end
  object BtnShutDown: TButton
    Left = 24
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Remove'
    Enabled = False
    TabOrder = 3
    OnClick = BtnClick
  end
  object BtnPause: TButton
    Left = 112
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Pause'
    Enabled = False
    TabOrder = 4
    OnClick = BtnClick
  end
  object BtnResume: TButton
    Left = 192
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Resume'
    Enabled = False
    TabOrder = 5
    OnClick = BtnClick
  end
end
