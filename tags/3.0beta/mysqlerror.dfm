object FormError: TFormError
  Left = 206
  Top = 111
  BorderStyle = bsDialog
  Caption = 'MySQL-Error...'
  ClientHeight = 124
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 10
    Width = 32
    Height = 32
    AutoSize = True
    Picture.Data = {
      07544269746D617076020000424D760200000000000076000000280000002000
      0000200000000100040000000000000200000000000000000000100000001000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00AAAAAAAAAAAAA88888888AAAAAAAAAAAAAAAAAAAAA88888888888888AAAA
      AAAAAAAAAAAAA8811111111888888AAAAAAAAAAAAAA81119999999911188888A
      AAAAAAAAAA8199999999999999188888AAAAAAAAA11999999999999999911888
      8AAAAAAA1999999999999999999991888AAAAAA1999999999999999999999918
      88AAAAA1999999999999999999999918888AAA1999999F9999999999F9999991
      888AA1999999FFF99999999FFF999999188AA199999FFFFF999999FFFFF99999
      1888A1999999FFFFF9999FFFFF9999991888199999999FFFFF99FFFFF9999999
      91881999999999FFFFFFFFFF99999999918819999999999FFFFFFFF999999999
      9188199999999999FFFFFF99999999999188199999999999FFFFFF9999999999
      918819999999999FFFFFFFF99999999991881999999999FFFFFFFFFF99999999
      918A199999999FFFFF99FFFFF9999999918AA1999999FFFFF9999FFFFF999999
      188AA199999FFFFF999999FFFFF9999918AAA1999999FFF99999999FFF999999
      1AAAAA1999999F9999999999F99999918AAAAAA1999999999999999999999918
      AAAAAAA199999999999999999999991AAAAAAAAA1999999999999999999991AA
      AAAAAAAAA11999999999999999911AAAAAAAAAAAAAA1999999999999991AAAAA
      AAAAAAAAAAAA11199999999111AAAAAAAAAAAAAAAAAAAAA11111111AAAAAAAAA
      AAAA}
    Transparent = True
  end
  object Label1: TLabel
    Left = 56
    Top = 10
    Width = 76
    Height = 13
    Caption = 'MySQL-Error:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 56
    Top = 88
    Width = 89
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 248
    Top = 88
    Width = 83
    Height = 25
    Caption = 'Online-Help...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 152
    Top = 88
    Width = 89
    Height = 25
    Caption = 'Copy'
    TabOrder = 1
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 56
    Top = 28
    Width = 321
    Height = 53
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    TabOrder = 3
  end
end