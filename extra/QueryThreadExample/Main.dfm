object Form1: TForm1
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akBottom]
  Caption = 'Form1'
  ClientHeight = 319
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    451
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 82
    Width = 48
    Height = 13
    Caption = 'Enter SQL'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label3: TLabel
    Left = 144
    Top = 8
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object TLabel
    Left = 208
    Top = 8
    Width = 22
    Height = 13
    Caption = 'User'
  end
  object Label5: TLabel
    Left = 280
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Pass'
  end
  object Label6: TLabel
    Left = 370
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Database'
  end
  object Button1: TButton
    Left = 383
    Top = 99
    Width = 60
    Height = 25
    Caption = 'Exec Async'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 4
    Top = 136
    Width = 439
    Height = 175
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 101
    Width = 369
    Height = 21
    TabOrder = 2
  end
  object edHost: TEdit
    Left = 8
    Top = 24
    Width = 130
    Height = 21
    TabOrder = 3
    Text = 'localhost'
  end
  object edPort: TEdit
    Left = 144
    Top = 24
    Width = 41
    Height = 21
    TabOrder = 4
    Text = '3306'
  end
  object edUser: TEdit
    Left = 208
    Top = 24
    Width = 66
    Height = 21
    TabOrder = 5
  end
  object edPass: TEdit
    Left = 280
    Top = 24
    Width = 66
    Height = 21
    TabOrder = 6
  end
  object edDatabase: TEdit
    Left = 370
    Top = 24
    Width = 73
    Height = 21
    TabOrder = 7
  end
end
