object tbl_properties_form: Ttbl_properties_form
  Left = 771
  Top = 113
  BorderIcons = [biSystemMenu]
  Caption = 'Table-Properties'
  ClientHeight = 415
  ClientWidth = 334
  Color = clBtnFace
  Constraints.MinHeight = 376
  Constraints.MinWidth = 304
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 328
    Height = 286
    Margins.Bottom = 0
    Align = alClient
    HotTrack = True
    TabOrder = 0
  end
  object PanelSummary: TPanel
    Left = 0
    Top = 289
    Width = 334
    Height = 107
    Align = alBottom
    BevelOuter = bvNone
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
      Left = 112
      Top = 8
      Width = 134
      Height = 13
      Caption = 'LabelSizeOfTableDataValue'
    end
    object LabelIndexesValue: TLabel
      Left = 112
      Top = 24
      Width = 90
      Height = 13
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
      Left = 112
      Top = 45
      Width = 88
      Height = 13
      Caption = 'LabelSumValue'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnClose: TButton
      Left = 112
      Top = 73
      Width = 97
      Height = 25
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 396
    Width = 334
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object popupSynMemo: TPopupMenu
    Images = MainForm.ImageList1
    Left = 16
    Top = 24
    object Copy1: TMenuItem
      Action = MainForm.EditCopy1
    end
    object menuSelectAll: TMenuItem
      Caption = 'Select all'
      ShortCut = 16449
      OnClick = menuSelectAllClick
    end
  end
end
