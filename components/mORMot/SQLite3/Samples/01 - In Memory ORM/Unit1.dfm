object Form1: TForm1
  Left = 604
  Top = 370
  BorderStyle = bsSingle
  ClientHeight = 286
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 40
    Top = 16
    Width = 67
    Height = 16
    Caption = 'Your name:'
  end
  object Label2: TLabel
    Left = 40
    Top = 72
    Width = 86
    Height = 16
    Caption = 'Your message:'
  end
  object QuestionMemo: TMemo
    Left = 32
    Top = 88
    Width = 409
    Height = 121
    TabOrder = 0
  end
  object NameEdit: TEdit
    Left = 32
    Top = 32
    Width = 217
    Height = 24
    TabOrder = 1
  end
  object AddButton: TButton
    Left = 48
    Top = 232
    Width = 145
    Height = 25
    Caption = 'Add the message'
    TabOrder = 2
    OnClick = AddButtonClick
  end
  object QuitButton: TButton
    Left = 296
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Quit'
    TabOrder = 3
    OnClick = QuitButtonClick
  end
  object FindButton: TButton
    Left = 256
    Top = 32
    Width = 185
    Height = 25
    Caption = 'Find a previous message'
    TabOrder = 4
    OnClick = FindButtonClick
  end
end
