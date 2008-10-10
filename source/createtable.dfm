object CreateTableForm: TCreateTableForm
  Left = 584
  Top = 95
  BorderStyle = bsDialog
  Caption = 'Create Table...'
  ClientHeight = 366
  ClientWidth = 499
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
    499
    366)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 71
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
    Left = 16
    Top = 106
    Width = 44
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Columns:'
  end
  object Label3: TLabel
    Left = 16
    Top = 66
    Width = 49
    Height = 13
    Caption = 'Comment:'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 325
    Width = 499
    Height = 41
    Align = alBottom
    Shape = bsTopLine
  end
  object Label4: TLabel
    Left = 16
    Top = 41
    Width = 63
    Height = 13
    Caption = 'In database:'
  end
  object Label5: TLabel
    Left = 264
    Top = 16
    Width = 58
    Height = 13
    Caption = 'Table type:'
    OnClick = Button1Click
  end
  object Bevel2: TBevel
    Left = 18
    Top = 94
    Width = 473
    Height = 9
    Shape = bsTopLine
  end
  object lblCharset: TLabel
    Left = 264
    Top = 41
    Width = 70
    Height = 13
    Caption = 'Character set:'
  end
  object lblCollation: TLabel
    Left = 264
    Top = 66
    Width = 45
    Height = 13
    Caption = 'Collation:'
  end
  object ButtonMoveUp: TPngSpeedButton
    Left = 160
    Top = 262
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Flat = True
    OnClick = ButtonMoveUpClick
  end
  object ButtonMoveDown: TPngSpeedButton
    Left = 160
    Top = 287
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Flat = True
    OnClick = ButtonMoveDownClick
  end
  object ButtonCancel: TButton
    Left = 408
    Top = 333
    Width = 83
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 13
  end
  object ButtonCreate: TButton
    Left = 312
    Top = 333
    Width = 83
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Create!'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 11
    OnClick = ButtonCreateClick
  end
  object GroupBox1: TGroupBox
    Left = 264
    Top = 128
    Width = 225
    Height = 185
    Anchors = [akLeft, akRight, akBottom]
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
    Left = 96
    Top = 14
    Width = 153
    Height = 21
    MaxLength = 64
    TabOrder = 0
    OnChange = EditTablenameChange
  end
  object EditDescription: TTntEdit
    Left = 96
    Top = 64
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
    Top = 151
    Width = 137
    Height = 161
    Anchors = [akLeft, akBottom]
    ExtendedSelect = False
    ItemHeight = 13
    TabOrder = 5
    OnClick = ListboxColumnsClick
  end
  object ButtonAdd: TButton
    Left = 160
    Top = 127
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 6
    OnClick = Button1Click
  end
  object ButtonDelete: TButton
    Left = 160
    Top = 191
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remove'
    Enabled = False
    TabOrder = 8
    OnClick = ButtonDeleteClick
  end
  object ButtonChange: TButton
    Left = 160
    Top = 159
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Change'
    Enabled = False
    TabOrder = 7
    OnClick = ButtonChangeClick
  end
  object EditFieldname: TEdit
    Left = 16
    Top = 127
    Width = 137
    Height = 21
    Anchors = [akLeft, akBottom]
    MaxLength = 64
    TabOrder = 4
    Text = 'FieldName'
    OnChange = EditFieldnameChange
  end
  object DBComboBox: TTntComboBox
    Left = 96
    Top = 39
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 1
  end
  object ComboBoxTableType: TComboBox
    Left = 338
    Top = 14
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 3
  end
  object comboCharset: TComboBox
    Left = 338
    Top = 39
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    Sorted = True
    TabOrder = 10
    OnChange = comboCharsetChange
  end
  object comboCollation: TComboBox
    Left = 338
    Top = 64
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    Sorted = True
    TabOrder = 12
  end
end
