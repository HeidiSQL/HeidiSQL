object CreateTableForm: TCreateTableForm
  Left = 584
  Top = 95
  Caption = 'Create Table...'
  ClientHeight = 330
  ClientWidth = 493
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    493
    330)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 10
    Width = 69
    Height = 13
    Caption = 'Table name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 101
    Width = 44
    Height = 13
    Caption = 'Columns:'
  end
  object Label3: TLabel
    Left = 8
    Top = 60
    Width = 49
    Height = 13
    Caption = 'Comment:'
  end
  object Label4: TLabel
    Left = 8
    Top = 35
    Width = 62
    Height = 13
    Caption = 'In database:'
  end
  object Label5: TLabel
    Left = 255
    Top = 10
    Width = 55
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Table type:'
    OnClick = Button1Click
  end
  object lblCharset: TLabel
    Left = 255
    Top = 35
    Width = 70
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Character set:'
  end
  object lblCollation: TLabel
    Left = 255
    Top = 60
    Width = 45
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Collation:'
  end
  object ButtonMoveUp: TPngSpeedButton
    Left = 182
    Top = 235
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Flat = True
    OnClick = ButtonMoveUpClick
  end
  object ButtonMoveDown: TPngSpeedButton
    Left = 182
    Top = 260
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Flat = True
    OnClick = ButtonMoveDownClick
  end
  object GroupBox1: TGroupBox
    Left = 255
    Top = 101
    Width = 228
    Height = 187
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Column properties:'
    TabOrder = 9
    object lblFieldType: TLabel
      Left = 16
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Type:'
      Enabled = False
    end
    object lblLengthSet: TLabel
      Left = 16
      Top = 48
      Width = 57
      Height = 13
      Caption = 'Length/Set:'
      Enabled = False
    end
    object lblDefault: TLabel
      Left = 16
      Top = 72
      Width = 68
      Height = 13
      Caption = 'Default Value:'
      Enabled = False
    end
    object ComboBoxType: TComboBox
      Left = 96
      Top = 24
      Width = 105
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBoxTypeChange
      Items.Strings = (
        'TINYINT'
        'SMALLINT'
        'MEDIUMINT'
        'INT'
        'BIGINT'
        'FLOAT'
        'DOUBLE'
        'DECIMAL'
        'DATE'
        'DATETIME'
        'TIMESTAMP'
        'TIME'
        'YEAR'
        'CHAR'
        'VARCHAR'
        'TINYBLOB'
        'TINYTEXT'
        'TEXT'
        'BLOB'
        'MEDIUMBLOB'
        'MEDIUMTEXT'
        'LONGBLOB'
        'LONGTEXT'
        'ENUM'
        'SET')
    end
    object EditLengthSet: TEdit
      Left = 96
      Top = 48
      Width = 105
      Height = 21
      Enabled = False
      TabOrder = 1
      OnChange = EditLengthSetChange
    end
    object EditDefault: TEdit
      Left = 96
      Top = 72
      Width = 105
      Height = 21
      Enabled = False
      TabOrder = 2
      OnChange = EditDefaultChange
    end
    object CheckBoxPrimary: TCheckBox
      Left = 16
      Top = 104
      Width = 57
      Height = 17
      Caption = 'Primary'
      Enabled = False
      TabOrder = 3
      OnClick = CheckBoxPrimaryClick
    end
    object CheckBoxBinary: TCheckBox
      Left = 16
      Top = 128
      Width = 49
      Height = 17
      Caption = 'Binary'
      Enabled = False
      TabOrder = 6
      OnClick = CheckBoxBinaryClick
    end
    object CheckBoxIndex: TCheckBox
      Left = 88
      Top = 104
      Width = 49
      Height = 17
      Caption = 'Index'
      Enabled = False
      TabOrder = 4
      OnClick = CheckBoxIndexClick
    end
    object CheckBoxUnique: TCheckBox
      Left = 144
      Top = 104
      Width = 57
      Height = 17
      Caption = 'Unique'
      Enabled = False
      TabOrder = 5
      OnClick = CheckBoxUniqueClick
    end
    object CheckBoxUnsigned: TCheckBox
      Left = 16
      Top = 144
      Width = 73
      Height = 17
      Caption = 'Unsigned'
      Enabled = False
      TabOrder = 7
      OnClick = CheckBoxUnsignedClick
    end
    object CheckBoxZerofill: TCheckBox
      Left = 16
      Top = 160
      Width = 57
      Height = 17
      Caption = 'Zerofill'
      Enabled = False
      TabOrder = 8
      OnClick = CheckBoxZerofillClick
    end
    object CheckBoxNotNull: TCheckBox
      Left = 88
      Top = 128
      Width = 65
      Height = 17
      Caption = 'Not Null'
      Enabled = False
      TabOrder = 9
      OnClick = CheckBoxNotNullClick
    end
    object CheckBoxAutoincrement: TCheckBox
      Left = 88
      Top = 144
      Width = 89
      Height = 17
      Caption = 'AutoIncrement'
      Enabled = False
      TabOrder = 10
      OnClick = CheckBoxAutoincrementClick
    end
  end
  object EditTablename: TTntEdit
    Left = 88
    Top = 8
    Width = 154
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 64
    TabOrder = 0
    OnChange = EditTablenameChange
  end
  object EditDescription: TTntEdit
    Left = 88
    Top = 58
    Width = 154
    Height = 21
    Hint = 'A 60 characters comment for this table'
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 60
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object ListboxColumns: TListBox
    Left = 8
    Top = 146
    Width = 169
    Height = 142
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExtendedSelect = False
    ItemHeight = 13
    TabOrder = 5
    OnClick = ListboxColumnsClick
  end
  object ButtonAdd: TButton
    Left = 182
    Top = 122
    Width = 60
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 6
    OnClick = Button1Click
  end
  object ButtonDelete: TButton
    Left = 182
    Top = 186
    Width = 60
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Remove'
    Enabled = False
    TabOrder = 8
    OnClick = ButtonDeleteClick
  end
  object ButtonChange: TButton
    Left = 182
    Top = 154
    Width = 60
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Change'
    Enabled = False
    TabOrder = 7
    OnClick = ButtonChangeClick
  end
  object EditFieldname: TEdit
    Left = 8
    Top = 122
    Width = 169
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 64
    TabOrder = 4
    Text = 'FieldName'
    OnChange = EditFieldnameChange
    OnEnter = EditFieldnameEnter
    OnExit = EditFieldnameExit
  end
  object DBComboBox: TTntComboBox
    Left = 88
    Top = 33
    Width = 154
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
  end
  object ComboBoxTableType: TComboBox
    Left = 329
    Top = 8
    Width = 153
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
  end
  object comboCharset: TComboBox
    Left = 329
    Top = 33
    Width = 153
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 13
    Sorted = True
    TabOrder = 10
    OnChange = comboCharsetChange
  end
  object comboCollation: TComboBox
    Left = 329
    Top = 58
    Width = 153
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 13
    Sorted = True
    TabOrder = 11
  end
  object ButtonCreate: TButton
    Left = 314
    Top = 297
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create!'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 12
    OnClick = ButtonCreateClick
  end
  object ButtonCancel: TButton
    Left = 402
    Top = 297
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 13
  end
end
