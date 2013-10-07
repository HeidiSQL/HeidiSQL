object Form1: TForm1
  Left = 334
  Top = 330
  Width = 322
  Height = 280
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object lblA: TLabel
    Left = 56
    Top = 50
    Width = 17
    Height = 16
    Caption = 'A='
  end
  object lblB: TLabel
    Left = 56
    Top = 98
    Width = 16
    Height = 16
    Caption = 'B='
  end
  object lblResult: TLabel
    Left = 76
    Top = 200
    Width = 184
    Height = 16
    Caption = 'Enter numbers, then Call Server'
  end
  object edtA: TEdit
    Left = 80
    Top = 48
    Width = 153
    Height = 24
    TabOrder = 0
  end
  object edtB: TEdit
    Left = 80
    Top = 96
    Width = 153
    Height = 24
    TabOrder = 1
  end
  object btnCall: TButton
    Left = 56
    Top = 152
    Width = 97
    Height = 25
    Caption = 'Call Server'
    TabOrder = 2
    OnClick = btnCallClick
  end
  object btnCancel: TButton
    Left = 168
    Top = 152
    Width = 97
    Height = 25
    Caption = 'Quit'
    TabOrder = 3
    OnClick = btnCancelClick
  end
end
