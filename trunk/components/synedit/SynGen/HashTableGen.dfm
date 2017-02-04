object FrmHashTableGen: TFrmHashTableGen
  Left = 299
  Top = 214
  BorderStyle = bsDialog
  Caption = 'Generate Hash Table'
  ClientHeight = 166
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelParams: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Parameters:'
  end
  object LabelD: TLabel
    Left = 29
    Top = 27
    Width = 9
    Height = 13
    Caption = 'd:'
  end
  object LabelC: TLabel
    Left = 117
    Top = 27
    Width = 9
    Height = 13
    Caption = 'c:'
  end
  object LabelM: TLabel
    Left = 205
    Top = 27
    Width = 11
    Height = 13
    Caption = 'm:'
  end
  object Label1: TLabel
    Left = 8
    Top = 96
    Width = 363
    Height = 13
    Caption = 
      'm is the size of the hash-table. If it is small enough searching' +
      ' can be stopped.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 112
    Width = 197
    Height = 13
    Caption = 'Normally it won'#39't change much after 3-4%.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LabelPercentage: TLabel
    Left = 374
    Top = 66
    Width = 14
    Height = 13
    Caption = '0%'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 64
    Width = 361
    Height = 17
    Max = 1000
    TabOrder = 4
  end
  object EditD: TMemo
    Left = 48
    Top = 24
    Width = 57
    Height = 21
    Alignment = taRightJustify
    Lines.Strings = (
      '0')
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object EditC: TMemo
    Left = 136
    Top = 24
    Width = 57
    Height = 21
    Alignment = taRightJustify
    Lines.Strings = (
      '0')
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  object EditM: TMemo
    Left = 224
    Top = 24
    Width = 57
    Height = 21
    Alignment = taRightJustify
    Lines.Strings = (
      '0')
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
  object ButtonFindHash: TButton
    Left = 8
    Top = 136
    Width = 113
    Height = 23
    Caption = 'Find Hash Params'
    Default = True
    TabOrder = 0
    OnClick = ButtonFindHashClick
  end
end
