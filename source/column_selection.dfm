object ColumnSelectionForm: TColumnSelectionForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'ColumnSelectionForm'
  ClientHeight = 277
  ClientWidth = 166
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBevel: TPanel
    Left = 0
    Top = 0
    Width = 166
    Height = 277
    Align = alClient
    BorderWidth = 3
    TabOrder = 0
    object btnCancel: TButton
      Left = 85
      Top = 246
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnOK: TButton
      Left = 4
      Top = 246
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 2
      OnClick = btnOKClick
    end
    object chkSelectAll: TCheckBox
      Left = 4
      Top = 207
      Width = 150
      Height = 17
      Caption = 'Select / Deselect all'
      TabOrder = 1
      OnClick = chkSelectAllClick
    end
    object chklistColumns: TCheckListBox
      Left = 4
      Top = 4
      Width = 158
      Height = 197
      OnClickCheck = chklistColumnsClickCheck
      Align = alTop
      ItemHeight = 13
      TabOrder = 0
    end
    object chkSort: TCheckBox
      Left = 4
      Top = 225
      Width = 125
      Height = 17
      Caption = 'Sort alphabetically'
      TabOrder = 4
      OnClick = chkSortClick
    end
  end
end
