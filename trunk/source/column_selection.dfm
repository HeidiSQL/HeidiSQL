object ColumnSelectionForm: TColumnSelectionForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Select columns'
  ClientHeight = 243
  ClientWidth = 184
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  DesignSize = (
    184
    243)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 101
    Top = 210
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 20
    Top = 210
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object chkSort: TCheckBox
    Left = 8
    Top = 187
    Width = 168
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Sort alphabetically'
    TabOrder = 2
    OnClick = chkSortClick
  end
  object chkSelectAll: TCheckBox
    Left = 8
    Top = 169
    Width = 168
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Select / Deselect all'
    TabOrder = 3
    OnClick = chkSelectAllClick
  end
  object chklistColumns: TCheckListBox
    Left = 0
    Top = 0
    Width = 184
    Height = 163
    OnClickCheck = chklistColumnsClickCheck
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
  end
end
