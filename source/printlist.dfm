object printlistForm: TprintlistForm
  Left = 521
  Top = 91
  BorderStyle = bsDialog
  Caption = 'Print List...'
  ClientHeight = 92
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblSelect: TLabel
    Left = 8
    Top = 11
    Width = 68
    Height = 13
    Caption = '&Select printer:'
    FocusControl = comboPrinters
  end
  object comboPrinters: TComboBox
    Left = 82
    Top = 8
    Width = 206
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = comboPrintersChange
  end
  object btnConfigure: TButton
    Left = 294
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Configure'
    TabOrder = 1
    OnClick = btnConfigureClick
  end
  object btnCancel: TButton
    Left = 294
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnPrint: TButton
    Left = 213
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Print'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnPrintClick
  end
  object chkPrintHeader: TCheckBox
    Left = 82
    Top = 36
    Width = 206
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Print column headers'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object PrinterSetup: TPrinterSetupDialog
    Left = 8
    Top = 32
  end
end
