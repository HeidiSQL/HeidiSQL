object tbl_properties_form: Ttbl_properties_form
  Left = 611
  Top = 109
  BorderIcons = [biSystemMenu]
  BorderWidth = 5
  Caption = 'Table-Properties'
  ClientHeight = 339
  ClientWidth = 286
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
  object Bevel: TBevel
    Left = 0
    Top = 230
    Width = 286
    Height = 8
    Align = alBottom
    Shape = bsSpacer
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 286
    Height = 230
    Align = alClient
    HotTrack = True
    TabOrder = 0
  end
  object PanelSummary: TPanel
    Left = 0
    Top = 238
    Width = 286
    Height = 65
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 1
    object LabelSizeOfTableData: TLabel
      Left = 8
      Top = 8
      Width = 91
      Height = 13
      Caption = 'Size of Table-Data:'
    end
    object LabelIndexes: TLabel
      Left = 8
      Top = 24
      Width = 75
      Height = 13
      Caption = 'Size of Indexes:'
    end
    object LabelSizeOfTableDataValue: TLabel
      Left = 144
      Top = 8
      Width = 134
      Height = 13
      Alignment = taRightJustify
      Caption = 'LabelSizeOfTableDataValue'
    end
    object LabelIndexesValue: TLabel
      Left = 188
      Top = 24
      Width = 90
      Height = 13
      Alignment = taRightJustify
      Caption = 'LabelIndexesValue'
    end
    object LabelSum: TLabel
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
    object LabelSumValue: TLabel
      Left = 191
      Top = 45
      Width = 88
      Height = 13
      Alignment = taRightJustify
      Caption = 'LabelSumValue'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object PanelControl: TPanel
    Left = 0
    Top = 303
    Width = 286
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnClose: TButton
      Left = 96
      Top = 9
      Width = 89
      Height = 25
      Cancel = True
      Caption = 'Close'
      Default = True
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
end
