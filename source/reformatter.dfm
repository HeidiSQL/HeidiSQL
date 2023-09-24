object frmReformatter: TfrmReformatter
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Reformat SQL'
  ClientHeight = 172
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    328
    172)
  TextHeight = 15
  object grpReformatter: TRadioGroup
    Left = 8
    Top = 8
    Width = 312
    Height = 125
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Select reformatter'
    ItemIndex = 0
    Items.Strings = (
      'Internal'
      'Online (heidisql.com)')
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 245
    Top = 139
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitTop = 200
  end
  object btnOk: TButton
    Left = 164
    Top = 139
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
    ExplicitTop = 200
  end
end
