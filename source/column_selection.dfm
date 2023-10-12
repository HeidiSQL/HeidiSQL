object frmColumnSelection: TfrmColumnSelection
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
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  DesignSize = (
    184
    243)
  TextHeight = 14
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
    Top = 167
    Width = 168
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Sort alphabetically'
    TabOrder = 2
    OnClick = PopulateList
  end
  object chkSelectAll: TCheckBox
    Left = 8
    Top = 6
    Width = 49
    Height = 17
    Hint = 'Select / Deselect all'
    Caption = 'All'
    TabOrder = 3
    OnClick = chkSelectAllClick
  end
  object chklistColumns: TCheckListBox
    Left = 8
    Top = 31
    Width = 168
    Height = 130
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
    OnClickCheck = chklistColumnsClickCheck
  end
  object editFilter: TButtonedEdit
    Left = 63
    Top = 4
    Width = 113
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Images = MainForm.VirtualImageListMain
    LeftButton.ImageIndex = 146
    LeftButton.Visible = True
    RightButton.ImageIndex = 193
    TabOrder = 5
    TextHint = 'Filter'
    OnChange = PopulateList
    OnLeftButtonClick = editFilterLeftButtonClick
  end
  object chkShowRowId: TCheckBox
    Left = 8
    Top = 190
    Width = 168
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Show static row id column'
    TabOrder = 6
  end
end
