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
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    302
    329)
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewTablename: TLabel
    Left = 16
    Top = 16
    Width = 101
    Height = 13
    Caption = 'Copy .. to new Table:'
  end
  object lblTargetDB: TLabel
    Left = 16
    Top = 64
    Width = 83
    Height = 13
    Caption = 'Target Database:'
  end
  object editNewTablename: TEdit
    Left = 16
    Top = 32
    Width = 270
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = editNewTablenameChange
    ExplicitWidth = 361
  end
  object radioStructure: TRadioButton
    Left = 160
    Top = 117
    Width = 81
    Height = 17
    Caption = 'Structure'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = radioStructureClick
  end
  object radioStructureAndData: TRadioButton
    Left = 160
    Top = 141
    Width = 113
    Height = 17
    Caption = 'Structure and Data'
    TabOrder = 4
    OnClick = radioStructureAndDataClick
  end
  object CheckListBoxFields: TCheckListBox
    Left = 16
    Top = 165
    Width = 270
    Height = 108
    Anchors = [akLeft, akTop, akBottom]
    Columns = 2
    Enabled = False
    ItemHeight = 13
    TabOrder = 2
  end
  object CheckBoxWithAllFields: TCheckBox
    Left = 16
    Top = 141
    Width = 97
    Height = 17
    Caption = 'With all Fields'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBoxWithAllFieldsClick
  end
  object ButtonCancel: TButton
    Left = 203
    Top = 293
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = ButtonCancelClick
  end
  object CheckBoxWithIndexes: TCheckBox
    Left = 16
    Top = 117
    Width = 97
    Height = 17
    Caption = 'With Indexes'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object ComboSelectDatabase: TComboBox
    Left = 16
    Top = 83
    Width = 270
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 7
    ExplicitWidth = 361
  end
  object ButtonOK: TButton
    Left = 114
    Top = 293
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 8
    OnClick = ButtonOKClick
  end
end
