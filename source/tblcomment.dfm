object tablecomment: Ttablecomment
  Left = 592
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Edit Table-Comment'
  ClientHeight = 133
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 89
    Height = 13
    Caption = 'Comment for Table'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 92
    Width = 342
    Height = 41
    Align = alBottom
    Shape = bsTopLine
  end
  object EditComment: TEdit
    Left = 16
    Top = 56
    Width = 313
    Height = 21
    MaxLength = 60
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 168
    Top = 104
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 256
    Top = 104
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object ComboBoxTableName: TComboBox
    Left = 208
    Top = 16
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = ComboBoxTableNameChange
  end
end
