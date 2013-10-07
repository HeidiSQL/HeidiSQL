object MainForm: TMainForm
  Left = 214
  Top = 192
  Width = 561
  Height = 443
  Caption = ' External DB performance tests - Synopse mORMot'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 32
    Width = 51
    Height = 13
    Caption = 'Oracle DB:'
  end
  object Label2: TLabel
    Left = 88
    Top = 56
    Width = 329
    Height = 13
    AutoSize = False
    Caption = 'If no Oracle DB is set, it won'#39't use it'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 240
    Top = 100
    Width = 3
    Height = 13
  end
  object LogMemo: TMemo
    Left = 0
    Top = 143
    Width = 545
    Height = 262
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 200
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object OraTNSName: TEdit
    Left = 88
    Top = 32
    Width = 121
    Height = 21
    Hint = 'TNS name'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object OraUser: TEdit
    Left = 224
    Top = 32
    Width = 121
    Height = 21
    Hint = 'User name'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object OraPass: TEdit
    Left = 352
    Top = 32
    Width = 121
    Height = 21
    Hint = 'Password'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
  object BtnRunTests: TButton
    Left = 32
    Top = 88
    Width = 185
    Height = 41
    Caption = 'Run tests'
    Default = True
    TabOrder = 0
    OnClick = BtnRunTestsClick
  end
end
