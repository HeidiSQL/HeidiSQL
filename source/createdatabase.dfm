object CreateDatabaseForm: TCreateDatabaseForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Create database ...'
  ClientHeight = 227
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
  OnShow = FormShow
  DesignSize = (
    317
    227)
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
  object lblCollation: TLabel
    Left = 8
    Top = 45
    Width = 45
    Height = 13
    Caption = 'C&ollation:'
    FocusControl = comboCollation
  end
  object lblCreateCode: TLabel
    Left = 8
    Top = 133
    Width = 65
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'CREATE code'
  end
  object lblServerDefaultCollation: TLabel
    Left = 96
    Top = 69
    Width = 78
    Height = 13
    Caption = 'Servers default:'
  end
  object editDBName: TEdit
    Left = 96
    Top = 16
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Enter database name'
    OnChange = Modified
  end
  object btnOK: TButton
    Left = 153
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 234
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object comboCollation: TComboBox
    Left = 96
    Top = 42
    Width = 213
    Height = 21
    Style = csDropDownList
    DropDownCount = 16
    Sorted = True
    TabOrder = 1
    OnChange = Modified
  end
  object SynMemoCreateCode: TSynMemo
    Left = 8
    Top = 152
    Width = 301
    Height = 67
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 4
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Gutter.Width = 0
    ReadOnly = True
    RightEdge = 0
    ScrollBars = ssVertical
    WordWrap = True
    FontSmoothing = fsmNone
  end
end
