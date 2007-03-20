object CreateTableForm: TCreateTableForm
  Left = 584
  Top = 95
  BorderStyle = bsDialog
  Caption = 'Create Table...'
  ClientHeight = 354
  ClientWidth = 499
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
    Top = 16
    Width = 73
    Height = 13
    Caption = 'Table-Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 91
    Width = 43
    Height = 13
    Caption = 'Columns:'
  end
  object Label3: TLabel
    Left = 264
    Top = 16
    Width = 47
    Height = 13
    Caption = 'Comment:'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 313
    Width = 499
    Height = 41
    Align = alBottom
    Shape = bsTopLine
  end
  object Label4: TLabel
    Left = 16
    Top = 41
    Width = 61
    Height = 13
    Caption = 'In Database:'
  end
  object Label5: TLabel
    Left = 264
    Top = 41
    Width = 57
    Height = 13
    Caption = 'Table-Type:'
    OnClick = Button1Click
  end
  object Bevel2: TBevel
    Left = 16
    Top = 76
    Width = 473
    Height = 9
    Shape = bsTopLine
  end
  object ButtonCancel: TButton
    Left = 408
    Top = 324
    Width = 83
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 13
    OnClick = ButtonCancelClick
  end
  object ButtonCreate: TButton
    Left = 312
    Top = 324
    Width = 83
    Height = 25
    Caption = 'Create!'
    Enabled = False
    TabOrder = 12
    OnClick = ButtonCreateClick
  end
  object GroupBox1: TGroupBox
    Left = 264
    Top = 112
    Width = 225
    Height = 185
    Caption = 'Column-Properties:'
    TabOrder = 11
    object lblFieldType: TLabel
      Left = 16
      Top = 24
      Width = 27
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
      Width = 67
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
  object EditTablename: TEdit
    Left = 96
    Top = 14
    Width = 153
    Height = 21
    MaxLength = 64
    TabOrder = 0
    OnChange = EditTablenameChange
  end
  object EditDescription: TEdit
    Left = 336
    Top = 14
    Width = 153
    Height = 21
    Hint = 'A 60 characters comment for this table'
    MaxLength = 60
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object ListboxColumns: TListBox
    Left = 16
    Top = 136
    Width = 137
    Height = 161
    ExtendedSelect = False
    ItemHeight = 13
    TabOrder = 5
    OnClick = ListboxColumnsClick
  end
  object ButtonMoveUp: TBitBtn
    Left = 160
    Top = 248
    Width = 25
    Height = 25
    Enabled = False
    TabOrder = 9
    OnClick = ButtonMoveUpClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000009410
      00009C180000A5210000AD290000B5310000BD390000C6420000CE4A0000D652
      00000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AAAAAAAAAAAA
      AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA00000AA
      AAAAAAAAA04320AAAAAAA0000054300000AAAA09876543220AAAAAA098765430
      AAAAAAAA0987650AAAAAAAAAA09870AAAAAAAAAAAA090AAAAAAAAAAAAAA0AAAA
      AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA}
  end
  object ButtonMoveDown: TBitBtn
    Left = 160
    Top = 272
    Width = 25
    Height = 25
    Enabled = False
    TabOrder = 10
    OnClick = ButtonMoveDownClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      040000000000800000000000000000000000100000001000000000000000A521
      0000AD290000B5310000BD390000C6420000CE4A0000D6520000DE5A0000E763
      00000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AAAAAAAAAAAA
      AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA0AAAA
      AAAAAAAAAA010AAAAAAAAAAAA03210AAAAAAAAAA0543210AAAAAAAA076543210
      AAAAAA09876543210AAAA0000076500000AAAAAAA08760AAAAAAAAAAA00000AA
      AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA}
  end
  object ButtonAdd: TButton
    Left = 160
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 6
    OnClick = Button1Click
  end
  object ButtonDelete: TButton
    Left = 160
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Remove'
    Enabled = False
    TabOrder = 8
    OnClick = ButtonDeleteClick
  end
  object ButtonChange: TButton
    Left = 160
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Change'
    Enabled = False
    TabOrder = 7
    OnClick = ButtonChangeClick
  end
  object EditFieldname: TEdit
    Left = 16
    Top = 112
    Width = 137
    Height = 21
    MaxLength = 64
    TabOrder = 4
    Text = 'FieldName'
    OnChange = EditFieldnameChange
  end
  object DBComboBox: TComboBox
    Left = 96
    Top = 39
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object ComboBoxTableType: TComboBox
    Left = 336
    Top = 39
    Width = 153
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Text = '<Automatic>'
    Items.Strings = (
      '<Automatic>'
      'ISAM'
      'MyISAM'
      'HEAP'
      'MERGE'
      'InnoDB'
      'BDB'
      'Gemini')
  end
end
