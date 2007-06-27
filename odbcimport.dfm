object odbcimportform: Todbcimportform
  Left = 352
  Top = 125
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsSingle
  Caption = 'ODBC Import'
  ClientHeight = 442
  ClientWidth = 488
  Color = clBtnFace
  Constraints.MaxHeight = 476
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    488
    442)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 184
    Width = 35
    Height = 13
    Caption = 'Tables:'
  end
  object Label2: TLabel
    Left = 144
    Top = 184
    Width = 30
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Fields:'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 401
    Width = 488
    Height = 41
    Align = alBottom
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 272
    Top = 200
    Width = 27
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Type:'
  end
  object Label4: TLabel
    Left = 272
    Top = 224
    Width = 63
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Length / Set:'
  end
  object Label5: TLabel
    Left = 272
    Top = 248
    Width = 67
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Default Value:'
  end
  object Label6: TLabel
    Left = 272
    Top = 272
    Width = 39
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Options:'
  end
  object Label7: TLabel
    Left = 32
    Top = 136
    Width = 83
    Height = 13
    Caption = 'Target Database:'
  end
  object Bevel2: TBevel
    Left = 16
    Top = 168
    Width = 457
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 457
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Source'
    TabOrder = 0
    object Image1: TImage
      Left = 16
      Top = 24
      Width = 16
      Height = 16
      Picture.Data = {
        07544269746D6170F6000000424DF60000000000000076000000280000001000
        0000100000000100040000000000800000000000000000000000100000001000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00DDDDD8888DDDDDDDDDD06666088DDDDDDD6666662608DDDDDEEE66622660
        DDDD0EEEEE6226668DDDEEEEE22222660008EEEEE322236608F8EEEE2EE22226
        08F8EEEEE22E226608F80EEE2222222278F8DEEE62E22220F8F8D0EEE2222228
        F8F8DD0EEE2220F8F8F8DDDD0000F8F8F8F8DDDD444444444448DDDD44444444
        444D}
      Transparent = True
    end
    object Image2: TImage
      Left = 16
      Top = 64
      Width = 16
      Height = 16
      Picture.Data = {
        07544269746D6170F6000000424DF60000000000000076000000280000001000
        0000100000000100040000000000800000000000000000000000100000001000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00DDD8000000000000DDD8777777777770DDD8FFFFFFFFFF70DDD8FF777777
        7F705151515151FF7F701FFFFFFFF5FF7F705F518FFFF1FF7F701F1518FFF577
        7F705FF15158F1777F701FFF1515F57FFF705FFF51F1F17FFF701FFF8518F57F
        FF705FFFFFFFF17F00001515151515FF8F0DDDD8FFFFFFFF80DDDDD888888888
        8DDD}
      Transparent = True
    end
    object ComboBoxDSN: TComboBox
      Left = 152
      Top = 24
      Width = 281
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = SourceSpecified
    end
    object RadioButton1: TRadioButton
      Left = 40
      Top = 24
      Width = 89
      Height = 17
      Caption = 'Data-Source:'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = ToggleSource
    end
    object RadioButton2: TRadioButton
      Left = 40
      Top = 64
      Width = 97
      Height = 17
      Caption = 'MS Access File:'
      TabOrder = 2
      OnClick = ToggleSource
    end
    object EditFileName: TEdit
      Left = 152
      Top = 64
      Width = 257
      Height = 21
      Color = clBtnFace
      Enabled = False
      ReadOnly = True
      TabOrder = 3
    end
    object ButtonOpenFile: TBitBtn
      Left = 408
      Top = 64
      Width = 22
      Height = 22
      Hint = 'Browse...'
      Enabled = False
      TabOrder = 4
      OnClick = ButtonOpenFileClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777777777777777777777000000000007777700333333333077770B033333333
        307770FB03333333330770BFB0333333333070FBFB000000000070BFBFBFBFB0
        777770FBFBFBFBF0777770BFB000000077777700077777777000777777777777
        7700777777777077707077777777770007777777777777777777}
    end
  end
  object CheckListBoxTables: TCheckListBox
    Left = 16
    Top = 200
    Width = 121
    Height = 185
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnClick = CheckListBoxTablesClick
  end
  object ListBoxFields: TListBox
    Left = 144
    Top = 200
    Width = 121
    Height = 185
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnClick = ListBoxFieldsClick
  end
  object ButtonImport: TButton
    Left = 313
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Import!'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = ButtonImportClick
  end
  object ButtonCancel: TButton
    Left = 401
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
    OnClick = ButtonCancelClick
  end
  object ComboBoxType: TComboBox
    Left = 344
    Top = 200
    Width = 129
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    Enabled = False
    ItemHeight = 13
    TabOrder = 5
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
    Left = 344
    Top = 224
    Width = 129
    Height = 21
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 6
    OnChange = EditLengthSetChange
  end
  object EditDefault: TEdit
    Left = 344
    Top = 248
    Width = 129
    Height = 21
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 7
    OnChange = EditDefaultChange
  end
  object CheckListBoxOptions: TCheckListBox
    Left = 344
    Top = 272
    Width = 129
    Height = 113
    Anchors = [akTop, akRight]
    Enabled = False
    ItemHeight = 13
    Items.Strings = (
      'Primary'
      'Index'
      'Unique'
      'Binary'
      'Unsigned'
      'Zerofill'
      'Not Null'
      'AutoIncrement')
    TabOrder = 8
    OnClick = CheckListBoxOptionsClick
  end
  object ComboBoxTargetDB: TComboBox
    Left = 168
    Top = 133
    Width = 281
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 9
  end
  object ODBCConnection: TADOConnection
    CursorLocation = clUseServer
    Left = 24
    Top = 208
  end
  object ADOTable1: TADOTable
    Connection = ODBCConnection
    CursorLocation = clUseServer
    Left = 152
    Top = 208
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Access-Files (*.mdb)|*.mdb|All Files (*.*)|*.*'
    Left = 448
    Top = 80
  end
  object ADOQuery1: TADOQuery
    Connection = ODBCConnection
    Parameters = <>
    Left = 280
    Top = 408
  end
  object Table1: TTable
    Left = 184
    Top = 208
  end
end
