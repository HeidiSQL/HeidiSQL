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
  object Bevel1: TBevel
    Left = 0
    Top = 300
    Width = 497
    Height = 41
    Align = alBottom
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 16
    Top = 64
    Width = 78
    Height = 13
    Caption = 'Import into table:'
  end
  object Label2: TLabel
    Left = 208
    Top = 24
    Width = 45
    Height = 13
    Caption = 'Filename:'
  end
  object Label3: TLabel
    Left = 208
    Top = 56
    Width = 30
    Height = 13
    Caption = 'Fields:'
  end
  object Label4: TLabel
    Left = 208
    Top = 144
    Width = 28
    Height = 13
    Caption = 'Lines:'
  end
  object Label5: TLabel
    Left = 416
    Top = 168
    Width = 25
    Height = 13
    Caption = 'Lines'
    Enabled = False
  end
  object Label6: TLabel
    Left = 16
    Top = 112
    Width = 65
    Height = 13
    Caption = 'Use Columns:'
  end
  object Label7: TLabel
    Left = 16
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Database:'
  end
  object Label8: TLabel
    Left = 208
    Top = 256
    Width = 240
    Height = 39
    Caption = 
      'Note: the LOCAL INFILE feature is only available for MySQL-Versi' +
      'ons > 3.22.6. On previous versions you will get an error!'
    WordWrap = True
  end
  object Button1: TButton
    Left = 328
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Import!'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 408
    Top = 312
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object EditFileName: TEdit
    Left = 272
    Top = 24
    Width = 193
    Height = 21
    TabOrder = 2
    Text = 'EditFileName'
    OnDblClick = BitBtn1Click
  end
  object BitBtn1: TBitBtn
    Left = 464
    Top = 24
    Width = 22
    Height = 22
    TabOrder = 3
    OnClick = BitBtn1Click
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
  object CheckBox1: TCheckBox
    Left = 272
    Top = 200
    Width = 81
    Height = 17
    Caption = 'Low Priority'
    TabOrder = 4
  end
  object TablesComboBox: TComboBox
    Left = 16
    Top = 80
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = TablesComboBoxChange
  end
  object CheckBox2: TCheckBox
    Left = 272
    Top = 56
    Width = 97
    Height = 17
    Caption = 'terminated by'
    TabOrder = 6
    OnClick = CheckBox2Click
  end
  object Edit2: TEdit
    Left = 360
    Top = 56
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 7
    Text = '\t'
  end
  object CheckBox3: TCheckBox
    Left = 272
    Top = 80
    Width = 97
    Height = 17
    Caption = 'enclosed by'
    TabOrder = 8
    OnClick = CheckBox3Click
  end
  object Edit3: TEdit
    Left = 360
    Top = 80
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 9
  end
  object CheckBox4: TCheckBox
    Left = 272
    Top = 104
    Width = 97
    Height = 17
    Caption = 'escaped by'
    TabOrder = 10
    OnClick = CheckBox4Click
  end
  object Edit4: TEdit
    Left = 360
    Top = 104
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 11
    Text = '\\'
  end
  object CheckBox5: TCheckBox
    Left = 416
    Top = 80
    Width = 73
    Height = 17
    Caption = 'optionally'
    Enabled = False
    TabOrder = 12
  end
  object CheckBox6: TCheckBox
    Left = 272
    Top = 144
    Width = 89
    Height = 17
    Caption = 'terminated by'
    TabOrder = 13
    OnClick = CheckBox6Click
  end
  object Edit5: TEdit
    Left = 360
    Top = 144
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 14
    Text = '\n'
  end
  object CheckBox7: TCheckBox
    Left = 272
    Top = 168
    Width = 73
    Height = 17
    Caption = 'ignore'
    TabOrder = 15
    OnClick = CheckBox7Click
  end
  object SpinEdit1: TSpinEdit
    Left = 360
    Top = 168
    Width = 49
    Height = 22
    Enabled = False
    MaxValue = 99999
    MinValue = 1
    TabOrder = 16
    Value = 1
  end
  object ColumnsCheckListBox: TCheckListBox
    Left = 16
    Top = 128
    Width = 121
    Height = 161
    ItemHeight = 13
    TabOrder = 17
  end
  object DBComboBox: TComboBox
    Left = 16
    Top = 32
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 18
    OnChange = DBComboBoxChange
  end
  object CheckBox8: TCheckBox
    Left = 272
    Top = 224
    Width = 65
    Height = 17
    Caption = 'Replace'
    TabOrder = 19
    OnClick = CheckBox8Click
  end
  object CheckBox9: TCheckBox
    Left = 360
    Top = 224
    Width = 57
    Height = 17
    Caption = 'Ignore'
    TabOrder = 20
    OnClick = CheckBox9Click
  end
  object BitBtn2: TBitBtn
    Left = 144
    Top = 178
    Width = 25
    Height = 25
    TabOrder = 21
    OnClick = BitBtn2Click
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
  object BitBtn3: TBitBtn
    Left = 144
    Top = 202
    Width = 25
    Height = 25
    TabOrder = 22
    OnClick = BitBtn3Click
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
  object OpenDialog1: TOpenDialog
    DefaultExt = 'csv'
    Filter = 
      'CSV-Files (*.csv)|*.csv|Textfiles (*.txt)|*.txt|All Files (*.*)|' +
      '*.*'
    Left = 80
  end
end
