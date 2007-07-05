object loaddataform: Tloaddataform
  Left = 212
  Top = 111
  BorderStyle = bsDialog
  Caption = 'Import Textfile'
  ClientHeight = 341
  ClientWidth = 497
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
  object bvlBottom: TBevel
    Left = 0
    Top = 300
    Width = 497
    Height = 41
    Align = alBottom
    Shape = bsTopLine
  end
  object lblTable: TLabel
    Left = 16
    Top = 64
    Width = 78
    Height = 13
    Caption = 'Import into table:'
  end
  object lblFilename: TLabel
    Left = 208
    Top = 24
    Width = 45
    Height = 13
    Caption = 'Filename:'
  end
  object lblFields: TLabel
    Left = 208
    Top = 56
    Width = 30
    Height = 13
    Caption = 'Fields:'
  end
  object lblLines: TLabel
    Left = 208
    Top = 144
    Width = 28
    Height = 13
    Caption = 'Lines:'
  end
  object lblIgnoreLines: TLabel
    Left = 415
    Top = 169
    Width = 25
    Height = 13
    Caption = 'Lines'
    Enabled = False
  end
  object lblColumns: TLabel
    Left = 16
    Top = 112
    Width = 65
    Height = 13
    Caption = 'Use Columns:'
  end
  object lblDatabase: TLabel
    Left = 16
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Database:'
  end
  object lblNote: TLabel
    Left = 208
    Top = 256
    Width = 240
    Height = 39
    Caption = 
      'Note: the LOCAL INFILE feature is only available for MySQL-Versi' +
      'ons > 3.22.6. On previous versions you will get an error!'
    WordWrap = True
  end
  object btnImport: TButton
    Left = 328
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Import!'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnImportClick
  end
  object btnCancel: TButton
    Left = 408
    Top = 312
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object editFilename: TEdit
    Left = 272
    Top = 24
    Width = 193
    Height = 21
    TabOrder = 2
    Text = 'editFilename'
    OnDblClick = btnOpenFileClick
  end
  object btnOpenFile: TBitBtn
    Left = 464
    Top = 24
    Width = 22
    Height = 22
    TabOrder = 3
    OnClick = btnOpenFileClick
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
  object chkLowPriority: TCheckBox
    Left = 272
    Top = 200
    Width = 81
    Height = 17
    Caption = 'Low Priority'
    TabOrder = 4
  end
  object comboTable: TComboBox
    Left = 16
    Top = 80
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = comboTableChange
  end
  object chkFieldsTerminated: TCheckBox
    Left = 272
    Top = 56
    Width = 97
    Height = 17
    Caption = 'terminated by'
    TabOrder = 6
    OnClick = chkFieldsTerminatedClick
  end
  object editFieldTerminator: TEdit
    Left = 360
    Top = 56
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 7
    Text = '\t'
  end
  object chkFieldsEnclosed: TCheckBox
    Left = 272
    Top = 80
    Width = 97
    Height = 17
    Caption = 'enclosed by'
    TabOrder = 8
    OnClick = chkFieldsEnclosedClick
  end
  object editFieldEncloser: TEdit
    Left = 360
    Top = 80
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 9
  end
  object chkFieldsEscaped: TCheckBox
    Left = 272
    Top = 104
    Width = 97
    Height = 17
    Caption = 'escaped by'
    TabOrder = 10
    OnClick = chkFieldsEscapedClick
  end
  object editFieldEscaper: TEdit
    Left = 360
    Top = 104
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 11
    Text = '\\'
  end
  object chkFieldsEnclosedOptionally: TCheckBox
    Left = 416
    Top = 80
    Width = 73
    Height = 17
    Caption = 'optionally'
    Enabled = False
    TabOrder = 12
  end
  object chkLinesTerminated: TCheckBox
    Left = 272
    Top = 144
    Width = 89
    Height = 17
    Caption = 'terminated by'
    TabOrder = 13
    OnClick = chkLinesTerminatedClick
  end
  object editLineTerminator: TEdit
    Left = 360
    Top = 144
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 14
    Text = '\n'
  end
  object chkLinesIgnore: TCheckBox
    Left = 272
    Top = 168
    Width = 73
    Height = 17
    Caption = 'ignore'
    TabOrder = 15
    OnClick = chkLinesIgnoreClick
  end
  object chklistColumns: TCheckListBox
    Left = 16
    Top = 128
    Width = 121
    Height = 161
    ItemHeight = 13
    TabOrder = 16
  end
  object comboDatabase: TComboBox
    Left = 16
    Top = 32
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 17
    OnChange = comboDatabaseChange
  end
  object chkReplace: TCheckBox
    Left = 272
    Top = 224
    Width = 65
    Height = 17
    Caption = 'Replace'
    TabOrder = 18
    OnClick = chkReplaceClick
  end
  object chkIgnore: TCheckBox
    Left = 360
    Top = 224
    Width = 57
    Height = 17
    Caption = 'Ignore'
    TabOrder = 19
    OnClick = chkIgnoreClick
  end
  object btnColUp: TBitBtn
    Left = 144
    Top = 178
    Width = 25
    Height = 25
    TabOrder = 21
    OnClick = btnColUpClick
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
  object btnColDown: TBitBtn
    Left = 144
    Top = 202
    Width = 25
    Height = 25
    TabOrder = 20
    OnClick = btnColDownClick
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
  object editIgnoreLines: TEdit
    Left = 360
    Top = 167
    Width = 33
    Height = 21
    Enabled = False
    TabOrder = 22
    Text = '1'
  end
  object updownIgnoreLines: TUpDown
    Left = 391
    Top = 168
    Width = 18
    Height = 21
    Associate = editIgnoreLines
    Enabled = False
    Max = 32767
    Position = 1
    TabOrder = 23
  end
  object OpenDialogCSVFile: TOpenDialog
    DefaultExt = 'csv'
    Filter = 
      'CSV-Files (*.csv)|*.csv|Textfiles (*.txt)|*.txt|All Files (*.*)|' +
      '*.*'
    Left = 80
  end
end
