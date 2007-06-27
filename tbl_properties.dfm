object tbl_properties_form: Ttbl_properties_form
  Left = 611
  Top = 109
  Width = 304
  Height = 376
  BorderIcons = [biSystemMenu]
  BorderWidth = 5
  Caption = 'Table-Properties'
  Color = clBtnFace
  Constraints.MaxHeight = 550
  Constraints.MaxWidth = 450
  Constraints.MinHeight = 376
  Constraints.MinWidth = 304
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 230
    Width = 286
    Height = 8
    Align = alBottom
    Shape = bsSpacer
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 286
    Height = 230
    Align = alClient
    HotTrack = True
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 238
    Width = 286
    Height = 65
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 91
      Height = 13
      Caption = 'Size of Table-Data:'
    end
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 75
      Height = 13
      Caption = 'Size of Indexes:'
    end
    object Label3: TLabel
      Left = 246
      Top = 8
      Width = 32
      Height = 13
      Alignment = taRightJustify
      Caption = 'Label3'
    end
    object Label4: TLabel
      Left = 246
      Top = 24
      Width = 32
      Height = 13
      Alignment = taRightJustify
      Caption = 'Label4'
    end
    object Label5: TLabel
      Left = 8
      Top = 45
      Width = 29
      Height = 13
      Caption = 'Sum:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 240
      Top = 45
      Width = 39
      Height = 13
      Alignment = taRightJustify
      Caption = 'Label6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 303
    Width = 286
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 96
      Top = 9
      Width = 89
      Height = 25
      Cancel = True
      Caption = 'Close'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
