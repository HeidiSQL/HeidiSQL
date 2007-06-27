object frmInsertFiles: TfrmInsertFiles
  Left = 262
  Top = 131
  Width = 519
  Height = 475
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Insert files...'
  Color = clBtnFace
  Constraints.MinHeight = 353
  Constraints.MinWidth = 475
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    511
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 195
    Width = 49
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Database:'
  end
  object Label2: TLabel
    Left = 8
    Top = 219
    Width = 30
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Table:'
  end
  object Label3: TLabel
    Left = 8
    Top = 243
    Width = 38
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Column:'
  end
  object LabelFileCount: TLabel
    Left = 8
    Top = 163
    Width = 27
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '0 files'
  end
  object Label5: TLabel
    Left = 392
    Top = 120
    Width = 104
    Height = 39
    Anchors = [akTop, akRight]
    Caption = 'Note: You can drop files from explorer into the list.'
    WordWrap = True
  end
  object ListViewFiles: TListView
    Left = 8
    Top = 8
    Width = 375
    Height = 151
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Filename'
      end
      item
        Alignment = taRightJustify
        Caption = 'Size [KB]'
        Width = 71
      end>
    ColumnClick = False
    GridLines = True
    LargeImages = LargeImages
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    SmallImages = SmallImages
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewFilesChange
    OnClick = ListViewFilesClick
    OnDblClick = ListViewFilesDblClick
    OnKeyUp = ListViewFilesKeyUp
  end
  object ButtonAddFiles: TButton
    Left = 390
    Top = 8
    Width = 113
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add files ...'
    TabOrder = 1
    OnClick = ButtonAddFilesClick
  end
  object ComboBoxDBs: TComboBox
    Left = 88
    Top = 191
    Width = 295
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
    OnChange = ComboBoxDBsChange
  end
  object ComboBoxTables: TComboBox
    Left = 88
    Top = 215
    Width = 295
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
    OnChange = ComboBoxTablesChange
  end
  object ComboBoxColumns: TComboBox
    Left = 88
    Top = 239
    Width = 295
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
    OnChange = ComboBoxColumnsChange
  end
  object ButtonInsert: TButton
    Left = 390
    Top = 259
    Width = 113
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Insert files!'
    Default = True
    Enabled = False
    TabOrder = 9
    OnClick = ButtonInsertClick
  end
  object ButtonCancel: TButton
    Left = 390
    Top = 227
    Width = 113
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = ButtonCancelClick
  end
  object ButtonRemoveFiles: TButton
    Left = 390
    Top = 40
    Width = 113
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Remove selected'
    Enabled = False
    TabOrder = 2
    OnClick = ButtonRemoveFilesClick
  end
  object CheckBoxShowOnlyBlobs: TCheckBox
    Left = 88
    Top = 265
    Width = 290
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Show only BLOB- and MEMO-columns'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = DisplayColumns
  end
  object ButtonClearList: TButton
    Left = 390
    Top = 72
    Width = 113
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Clear list'
    Enabled = False
    TabOrder = 3
    OnClick = ButtonClearListClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 294
    Width = 492
    Height = 129
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Other field-values'
    TabOrder = 10
    DesignSize = (
      492
      129)
    object Label4: TLabel
      Left = 170
      Top = 66
      Width = 261
      Height = 26
      Caption = 
        'Note: Don'#39't quote functions or NULL-values, otherwise they will ' +
        'be inserted as ordinary string-values.'
      WordWrap = True
    end
    object ListBoxOtherFields: TListBox
      Left = 16
      Top = 24
      Width = 121
      Height = 89
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBoxOtherFieldsClick
    end
    object CheckBoxQuote: TCheckBox
      Left = 152
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Quote "value"'
      TabOrder = 2
      OnClick = FieldChange
    end
    object ComboBoxValue: TComboBox
      Left = 152
      Top = 24
      Width = 316
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      Text = 'NULL'
      OnChange = FieldChange
      Items.Strings = (
        'NULL'
        '%filename%'
        '%filepath%'
        '%filesize%'
        '%filedate%'
        '%filedatetime%'
        '%filetime%'
        'NOW()'
        'LOWER("%filename%")'
        'UPPER("%filenname%")'
        'UNIX_TIMESTAMP("%filedatetime%")'
        'ENCODE("%filename%", "password")')
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'All files (*.*)|*.*|Common images (*.jpg, *.gif, *.bmp, *.png)|*' +
      '.jpg;*.gif;*.bmp;*.png'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 392
    Top = 176
  end
  object LargeImages: TImageList
    Left = 424
    Top = 176
  end
  object SmallImages: TImageList
    Left = 456
    Top = 176
  end
end
