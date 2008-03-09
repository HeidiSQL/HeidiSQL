object frmEditVariable: TfrmEditVariable
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit server variable'
  ClientHeight = 142
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    301
    142)
  PixelsPerInch = 96
  TextHeight = 13
  object lblValue: TLabel
    Left = 8
    Top = 11
    Width = 54
    Height = 13
    Caption = 'New value:'
  end
  object editValue: TEdit
    Left = 8
    Top = 29
    Width = 285
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'editValue'
    OnChange = editValueChange
  end
  object rgScope: TRadioGroup
    Left = 8
    Top = 56
    Width = 285
    Height = 45
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Scope'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'This session'
      'Global')
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 73
    Top = 109
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 154
    Top = 109
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
