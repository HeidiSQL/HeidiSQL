object Form1: TForm1
  Left = 217
  Top = 262
  BorderStyle = bsSingle
  Caption = ' Synopse mORMot Release Notes tool'
  ClientHeight = 602
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 24
    Width = 546
    Height = 13
    Caption = 
      'Purpose of this tool is to create the release notes details by a' +
      'utomated extraction from the several .pas unit files.'
  end
  object Label2: TLabel
    Left = 48
    Top = 56
    Width = 80
    Height = 13
    Caption = 'Release Version:'
  end
  object Edit1: TEdit
    Left = 136
    Top = 53
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '1.16'
  end
  object Button1: TButton
    Left = 56
    Top = 88
    Width = 201
    Height = 57
    Caption = 'Generate'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 280
    Top = 48
    Width = 561
    Height = 537
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object Button2: TButton
    Left = 160
    Top = 152
    Width = 99
    Height = 25
    Caption = 'Open in browser'
    TabOrder = 3
    OnClick = Button2Click
  end
end
