object Form1: TForm1
  Left = 255
  Top = 208
  Width = 337
  Height = 355
  Caption = ' SQLite3Pages Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 32
    Top = 16
    Width = 62
    Height = 13
    Caption = 'Enter a Title:'
  end
  object Label1: TLabel
    Left = 32
    Top = 80
    Width = 81
    Height = 13
    Caption = 'Enter some text:'
  end
  object edt1: TEdit
    Left = 32
    Top = 32
    Width = 233
    Height = 21
    TabOrder = 0
    Text = 'This is a Title from a field'
  end
  object mmo1: TMemo
    Left = 32
    Top = 96
    Width = 233
    Height = 153
    Lines.Strings = (
      
        'In our Synopse SQLite3 framework there is a very easy Open Sourc' +
        'e reporting system.'
      ''
      
        'You create your report from code, then you can preview it on the' +
        ' screen.'
      'You can then print or export the report as PDF.'
      ''
      'Just right click on the report preview to see options.')
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object btn1: TButton
    Left = 32
    Top = 264
    Width = 113
    Height = 33
    Caption = 'Create Report'
    TabOrder = 2
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 152
    Top = 264
    Width = 113
    Height = 33
    Caption = 'Quit'
    TabOrder = 3
    OnClick = btn2Click
  end
end
