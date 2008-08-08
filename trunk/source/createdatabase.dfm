object CreateDatabaseForm: TCreateDatabaseForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Create database ...'
  ClientHeight = 218
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    317
    218)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDBName: TLabel
    Left = 8
    Top = 19
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = editDBName
  end
  object lblCharset: TLabel
    Left = 8
    Top = 46
    Width = 70
    Height = 13
    Caption = '&Character set:'
    FocusControl = comboCharset
  end
  object lblCollation: TLabel
    Left = 8
    Top = 73
    Width = 45
    Height = 13
    Caption = 'C&ollation:'
    FocusControl = comboCollation
  end
  object lblPreview: TLabel
    Left = 8
    Top = 138
    Width = 178
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'SQL &preview for CREATE DATABASE:'
  end
  object editDBName: TTntEdit
    Left = 88
    Top = 16
    Width = 221
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = editDBNameChange
  end
  object comboCharset: TComboBox
    Left = 88
    Top = 43
    Width = 221
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnChange = comboCharsetChange
  end
  object btnOK: TButton
    Left = 153
    Top = 102
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 234
    Top = 102
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object comboCollation: TComboBox
    Left = 88
    Top = 70
    Width = 221
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    OnChange = Modified
  end
  object SynMemoPreview: TSynMemo
    Left = 8
    Top = 154
    Width = 301
    Height = 56
    SingleLineMode = False
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 5
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    ReadOnly = True
    ScrollBars = ssNone
    WordWrap = True
  end
end
