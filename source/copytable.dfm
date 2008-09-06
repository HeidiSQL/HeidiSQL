object CopyTableForm: TCopyTableForm
  Left = 393
  Top = 115
  BorderStyle = bsDialog
  Caption = 'Copy Table...'
  ClientHeight = 329
  ClientWidth = 302
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
  DesignSize = (
    302
    329)
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewTablename: TLabel
    Left = 8
    Top = 8
    Width = 103
    Height = 13
    Caption = 'Copy .. to new table:'
  end
  object lblTargetDB: TLabel
    Left = 8
    Top = 56
    Width = 84
    Height = 13
    Caption = 'Target database:'
  end
  object editNewTablename: TTntEdit
    Left = 8
    Top = 24
    Width = 286
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = editNewTablenameChange
  end
  object radioStructure: TRadioButton
    Left = 154
    Top = 109
    Width = 140
    Height = 17
    Caption = 'Structure'
    TabOrder = 3
    OnClick = radioStructureClick
  end
  object radioStructureAndData: TRadioButton
    Left = 154
    Top = 133
    Width = 140
    Height = 17
    Caption = 'Structure and Data'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = radioStructureAndDataClick
  end
  object CheckListBoxFields: TTntCheckListBox
    Left = 8
    Top = 157
    Width = 286
    Height = 108
    OnClickCheck = CheckListBoxFieldsClickCheck
    Anchors = [akLeft, akTop, akBottom]
    Columns = 2
    ItemHeight = 13
    TabOrder = 2
  end
  object CheckBoxWithAllFields: TCheckBox
    Left = 8
    Top = 133
    Width = 121
    Height = 17
    Caption = 'Include all columns'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBoxWithAllFieldsClick
  end
  object ButtonCancel: TButton
    Left = 211
    Top = 296
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object CheckBoxWithIndexes: TCheckBox
    Left = 8
    Top = 109
    Width = 121
    Height = 17
    Caption = 'Include indexes'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object ComboSelectDatabase: TTntComboBox
    Left = 8
    Top = 72
    Width = 286
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 7
  end
  object ButtonOK: TButton
    Left = 122
    Top = 296
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = ButtonOKClick
  end
  object chkSelectAll: TCheckBox
    Left = 8
    Top = 271
    Width = 286
    Height = 17
    Caption = 'Select / deselect all'
    TabOrder = 9
    OnClick = chkSelectAllClick
  end
end
