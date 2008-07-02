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
    Caption = 'Table-Name:'
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
    Caption = 'In Database:'
  end
  object Label5: TLabel
    Left = 264
    Top = 16
    Width = 58
    Height = 13
    Caption = 'Table-Type:'
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
    PngImage.Data = {
      89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
      610000001974455874536F6674776172650041646F626520496D616765526561
      647971C9653C000001654944415478DA63FCFFFF3F032580119F01C5C7D24FFE
      FDF3EFF704BBD936241B507434EDA2189B84DEDF7F7F191EBF7F746186C76243
      A20C283991C1F6EFCFBFABA2EC622A0A3C4A0CFFFEFD63B8F1FA3AC3C3378F6E
      FDFEF55B7779D4865F780D283C9CBAFBEF9FBF36A2ECE21C0AFC8A0CFFFEFF63
      78FAFE29C3A587977E000D38B836699B07515E48DD12FDDF4ADE9AE12FD00527
      6F9F6478FBE18DE5BA94ED27880E83F8B561FFAD956D812EF8CBB0E3F40E860D
      693B18490AC4C86501FF6DD4EDC0066C3FB69D615BDE3ED20C089EE7F5DF4ED7
      81E12FD080AD07B632EC2D3B429A013E535DFF3B18030D0046E3963D5B180ED7
      9D22CD00B75EFBFF8E164EC068FCCBB071FB6686536DE74933C0BED5F2BFB3AD
      0B380C366CDCC870A1EF2A690658D61AFF777576031BB07ECD0686AB536F9166
      8071A9EE7F770F77867F40B86EC506865BB3EF9166806E9EC6FF3FBFFF32FCF9
      FD8701983219EE2D78449A01C40200E3CDD6E1230A89BB0000000049454E44AE
      426082}
  end
  object ButtonMoveDown: TPngSpeedButton
    Left = 160
    Top = 287
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Flat = True
    OnClick = ButtonMoveDownClick
    PngImage.Data = {
      89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
      610000001974455874536F6674776172650041646F626520496D616765526561
      647971C9653C000001654944415478DA63FCFFFF3F032580119701252732FEFF
      FFF79FE1EF9F7F40FC9761B2D33C46920C283A9AF6DF52C486E1DFFF7F0CFB1F
      EC6598E1B1983403F20F24FFB710B766F8F7EF1FC3BEBB7B19E6FA2D27CD80AC
      DDF1FF2DA5ACC12ED8736D37C3E2B0B5A41990BA25FABF95BC35C35FA00B765F
      DCC5B0326E136906C4AF0DFB6FAD6C0B74C15F861DA777306C48DB419A0191CB
      02FEDBA8DB810DD87E6C3BC3B6BC7DA419103CCFEBBF9DAE03C35FA0015B0F6C
      65D85B768434037CA6BAFE7730061AF0EF2FC3963D5B180ED79D22DE00EF29CE
      165C6CDCC78DB48D81D1F89761E3F6CD0CA7DACE136780D724A71DBF7FFDB657
      57D4E01013950087C1858B1719EEDCBEF3E3CFEFBF476ECCB8E38AD700E72E1B
      B6DF3FFF5C961493545357D7001B70FEDC05863B37EFDEF9F3E7AFF6DD790F7E
      111506C6A5BAE7A5A4A40DFEFEFD0BD27CE9D6EC7BFA240522086866AA1EF9F3
      FB0FEBED39F7CD71A961A4343B0300ACFAD2E11B8572350000000049454E44AE
      426082}
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
    Caption = 'Column-Properties:'
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
    Left = 338
    Top = 14
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object comboCharset: TComboBox
    Left = 338
    Top = 39
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
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
    ItemHeight = 13
    Sorted = True
    TabOrder = 12
  end
end
