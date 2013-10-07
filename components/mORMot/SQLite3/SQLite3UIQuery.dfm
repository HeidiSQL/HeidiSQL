object QueryForm: TQueryForm
  Left = 312
  Top = 114
  Width = 317
  Height = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 24
    Width = 125
    Height = 16
    Caption = '1. Where to search'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 48
    Top = 112
    Width = 110
    Height = 16
    Caption = '2. How to search'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 48
    Top = 176
    Width = 117
    Height = 16
    Caption = '3. What to search'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 48
    Top = 240
    Width = 137
    Height = 16
    Caption = '4. Action to be taken'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Where: TComboBox
    Left = 48
    Top = 48
    Width = 169
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = WhereChange
  end
  object Operation: TComboBox
    Left = 48
    Top = 136
    Width = 169
    Height = 21
    Style = csDropDownList
    DropDownCount = 16
    ItemHeight = 13
    TabOrder = 2
  end
  object MarkedOnly: TCheckBox
    Left = 48
    Top = 80
    Width = 233
    Height = 17
    Caption = 'Only within already marked entries'
    TabOrder = 1
  end
end
