object CopyTableForm: TCopyTableForm
  Left = 443
  Top = 79
  BorderStyle = bsDialog
  Caption = 'Copy Table...'
  ClientHeight = 251
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 101
    Height = 13
    Caption = 'Copy .. to new Table:'
  end
  object Label2: TLabel
    Left = 160
    Top = 136
    Width = 164
    Height = 52
    Caption = 
      'Note: This copy is done using a special and fast create-statemen' +
      't, which will work only in MySQL-Versions 3.23.x and above'
    WordWrap = True
  end
  object Edit1: TEdit
    Left = 16
    Top = 32
    Width = 313
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object RadioButton1: TRadioButton
    Left = 160
    Top = 72
    Width = 81
    Height = 17
    Caption = 'Structure'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 160
    Top = 96
    Width = 113
    Height = 17
    Caption = 'Structure and Data'
    TabOrder = 4
    OnClick = RadioButton2Click
  end
  object CheckListBoxFields: TCheckListBox
    Left = 16
    Top = 120
    Width = 121
    Height = 113
    Enabled = False
    ItemHeight = 13
    TabOrder = 2
  end
  object CheckBoxWithAllFields: TCheckBox
    Left = 16
    Top = 96
    Width = 97
    Height = 17
    Caption = 'With all Fields'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBoxWithAllFieldsClick
  end
  object ButtonOK: TBitBtn
    Left = 160
    Top = 208
    Width = 83
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = ButtonOKClick
    Glyph.Data = {
      26050000424D26050000000000003604000028000000100000000F0000000100
      080000000000F000000000000000000000000001000000010000000000000000
      330000006600000099000000CC000000FF000033000000333300003366000033
      99000033CC000033FF00006600000066330000666600006699000066CC000066
      FF00009900000099330000996600009999000099CC000099FF0000CC000000CC
      330000CC660000CC990000CCCC0000CCFF0000FF000000FF330000FF660000FF
      990000FFCC0000FFFF00330000003300330033006600330099003300CC003300
      FF00333300003333330033336600333399003333CC003333FF00336600003366
      330033666600336699003366CC003366FF003399000033993300339966003399
      99003399CC003399FF0033CC000033CC330033CC660033CC990033CCCC0033CC
      FF0033FF000033FF330033FF660033FF990033FFCC0033FFFF00660000006600
      330066006600660099006600CC006600FF006633000066333300663366006633
      99006633CC006633FF00666600006666330066666600666699006666CC006666
      FF00669900006699330066996600669999006699CC006699FF0066CC000066CC
      330066CC660066CC990066CCCC0066CCFF0066FF000066FF330066FF660066FF
      990066FFCC0066FFFF00990000009900330099006600990099009900CC009900
      FF00993300009933330099336600993399009933CC009933FF00996600009966
      330099666600996699009966CC009966FF009999000099993300999966009999
      99009999CC009999FF0099CC000099CC330099CC660099CC990099CCCC0099CC
      FF0099FF000099FF330099FF660099FF990099FFCC0099FFFF00CC000000CC00
      3300CC006600CC009900CC00CC00CC00FF00CC330000CC333300CC336600CC33
      9900CC33CC00CC33FF00CC660000CC663300CC666600CC669900CC66CC00CC66
      FF00CC990000CC993300CC996600CC999900CC99CC00CC99FF00CCCC0000CCCC
      3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF3300CCFF6600CCFF
      9900CCFFCC00CCFFFF00FF000000FF003300FF006600FF009900FF00CC00FF00
      FF00FF330000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF66
      3300FF666600FF669900FF66CC00FF66FF00FF990000FF993300FF996600FF99
      9900FF99CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCC
      FF00FFFF0000FFFF3300FFFF6600FFFF9900FFFFCC00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B9B9B9B9B9B
      9B00000000000000009B9B9B9B9B9B9B9B0E8F1D1D1D1616009B9B9B9B9B9B9B
      9B0E8FD71D1D1D16009B9B9B9B9B9B9B9B0E8F1DD71D1D16009B9B9B00000000
      000E8F8F8F8F8F8F009B9B9B56ACACACAC0E8F8F8F0E0E0E9B9B0000568FD78F
      D7AC0E0E0E0E9B9B9B9B81AC56D78FD78FAC009B9B9B9B9B9B9B81AC56D7D78F
      D7AC009B9B9B9B9B9B9B81AC56D78FD78FAC009B9B9B9B6C9B9B81AC56D7D78F
      D7AC009B9B9B6C726C9B81AC56D7D7D7D7AC009B9B9696726C6C81AC56565656
      5656009B9B9B9B6C9B9B81ACACACACAC009B9B9B6C6C6C9B9B9B818181818181
      569B9B9B9B9B9B9B9B9B}
  end
  object ButtonCancel: TButton
    Left = 256
    Top = 208
    Width = 83
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = ButtonCancelClick
  end
  object CheckBoxWithIndexes: TCheckBox
    Left = 16
    Top = 72
    Width = 97
    Height = 17
    Caption = 'With Indexes'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
end