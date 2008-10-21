object FrmDataViewSave: TFrmDataViewSave
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Save perspective'
  ClientHeight = 129
  ClientWidth = 273
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    273
    129)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSave: TLabel
    Left = 8
    Top = 13
    Width = 103
    Height = 13
    Caption = 'Name of perspective:'
  end
  object comboSave: TComboBox
    Left = 8
    Top = 32
    Width = 257
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 0
    TabOrder = 0
    OnChange = comboSaveChange
  end
  object btnOK: TButton
    Left = 59
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 140
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object chkDefault: TCheckBox
    Left = 8
    Top = 59
    Width = 257
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Save as default perspective for this table'
    TabOrder = 1
  end
end
