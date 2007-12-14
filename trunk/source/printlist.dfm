object printlistForm: TprintlistForm
  Left = 521
  Top = 91
  BorderStyle = bsDialog
  Caption = 'Print List...'
  ClientHeight = 115
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
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
  object boxColumns: TGroupBox
    Left = 8
    Top = 39
    Width = 280
    Height = 68
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Print what...'
    TabOrder = 2
    object chkAllColumns: TCheckBox
      Left = 10
      Top = 24
      Width = 97
      Height = 17
      Caption = 'All Columns'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkAllColumnsClick
    end
  end
  object btnCancel: TButton
    Left = 294
    Top = 82
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnPrint: TButton
    Left = 294
    Top = 50
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Print'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnPrintClick
  end
  object PrinterSetup: TPrinterSetupDialog
    Left = 216
    Top = 56
  end
end
