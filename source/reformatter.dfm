object frmReformatter: TfrmReformatter
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Reformat SQL'
  ClientHeight = 217
  ClientWidth = 503
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
    503
    217)
  TextHeight = 15
  object grpReformatter: TRadioGroup
    Left = 8
    Top = 8
    Width = 487
    Height = 128
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Select reformatter'
    ItemIndex = 0
    Items.Strings = (
      'Internal'
      'Online (heidisql.com)'
      'Online (sqlformat.org)')
    TabOrder = 0
    OnClick = grpReformatterClick
  end
  object btnCancel: TButton
    Left = 420
    Top = 184
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 339
    Top = 184
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
  end
  object lblFormatProviderLink: TLinkLabel
    Left = 8
    Top = 142
    Width = 149
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = 'lblFormatProviderLink'
    TabOrder = 3
    UseVisualStyle = True
    Visible = False
    OnLinkClick = lblFormatProviderLinkLinkClick
  end
  object chkKeepAsking: TCheckBox
    Left = 8
    Top = 188
    Width = 257
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Keep asking this question.'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
end
