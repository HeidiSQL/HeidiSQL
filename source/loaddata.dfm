object loaddataform: Tloaddataform
  Left = 212
  Top = 111
  BorderWidth = 3
  Caption = 'Import text file'
  ClientHeight = 488
  ClientWidth = 408
  Color = clBtnFace
  Constraints.MinHeight = 530
  Constraints.MinWidth = 430
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
    408
    488)
  PixelsPerInch = 96
  TextHeight = 13
  object btnImport: TButton
    Left = 244
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Import!'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 6
    OnClick = btnImportClick
  end
  object btnCancel: TButton
    Left = 325
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object grpFilename: TGroupBox
    Left = 8
    Top = 8
    Width = 392
    Height = 84
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Input file'
    TabOrder = 0
    DesignSize = (
      392
      84)
    object lblFilename: TLabel
      Left = 10
      Top = 27
      Width = 46
      Height = 13
      Caption = 'Filename:'
      FocusControl = editFilename
    end
    object lblEncoding: TLabel
      Left = 10
      Top = 54
      Width = 47
      Height = 13
      Caption = 'Encoding:'
    end
    object editFilename: TButtonedEdit
      Left = 88
      Top = 24
      Width = 294
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.ImageListMain
      RightButton.ImageIndex = 51
      RightButton.Visible = True
      TabOrder = 0
      Text = 'editFilename'
      OnChange = editFilenameChange
      OnDblClick = btnOpenFileClick
      OnRightButtonClick = btnOpenFileClick
    end
    object comboEncoding: TComboBox
      Left = 88
      Top = 51
      Width = 294
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      TabOrder = 1
      OnSelect = comboEncodingSelect
    end
  end
  object grpFields: TGroupBox
    Left = 8
    Top = 93
    Width = 209
    Height = 109
    Caption = 'Fields'
    TabOrder = 1
    DesignSize = (
      209
      109)
    object lblFieldTerminater: TLabel
      Left = 10
      Top = 26
      Width = 67
      Height = 13
      Caption = 'terminated by'
    end
    object lblFieldEncloser: TLabel
      Left = 10
      Top = 51
      Width = 57
      Height = 13
      Caption = 'enclosed by'
    end
    object lblFieldEscaper: TLabel
      Left = 10
      Top = 75
      Width = 55
      Height = 13
      Caption = 'escaped by'
    end
    object editFieldEscaper: TEdit
      Left = 88
      Top = 72
      Width = 49
      Height = 21
      TabOrder = 2
      Text = '"'
    end
    object editFieldEncloser: TEdit
      Left = 88
      Top = 48
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '"'
    end
    object editFieldTerminator: TEdit
      Left = 88
      Top = 24
      Width = 49
      Height = 21
      TabOrder = 0
      Text = ';'
    end
    object chkFieldsEnclosedOptionally: TCheckBox
      Left = 143
      Top = 50
      Width = 62
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'optionally'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
  object grpLines: TGroupBox
    Left = 223
    Top = 93
    Width = 177
    Height = 109
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Lines'
    TabOrder = 2
    object lblIgnoreLinesCount: TLabel
      Left = 143
      Top = 44
      Width = 24
      Height = 13
      Caption = 'Lines'
    end
    object lblLineTerminator: TLabel
      Left = 10
      Top = 20
      Width = 67
      Height = 13
      Caption = 'terminated by'
    end
    object lblIgnoreLines: TLabel
      Left = 10
      Top = 44
      Width = 30
      Height = 13
      Caption = 'ignore'
    end
    object updownIgnoreLines: TUpDown
      Left = 121
      Top = 41
      Width = 16
      Height = 21
      Associate = editIgnoreLines
      Max = 32767
      Position = 1
      TabOrder = 2
    end
    object editIgnoreLines: TEdit
      Left = 88
      Top = 41
      Width = 33
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object editLineTerminator: TEdit
      Left = 88
      Top = 17
      Width = 49
      Height = 21
      TabOrder = 0
      Text = '\r\n'
    end
  end
  object grpDuplicates: TRadioGroup
    Left = 8
    Top = 208
    Width = 209
    Height = 130
    Caption = 'Handling of duplicate rows'
    ItemIndex = 2
    Items.Strings = (
      'INSERT (may throw errors)'
      'INSERT IGNORE (duplicates)'
      'REPLACE (duplicates)')
    TabOrder = 3
  end
  object grpParseMethod: TRadioGroup
    Left = 8
    Top = 344
    Width = 209
    Height = 105
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Method'
    ItemIndex = 0
    Items.Strings = (
      'Server parses file contents (LOAD DATA)'
      'Client parses file contents')
    TabOrder = 4
    WordWrap = True
    OnClick = grpParseMethodClick
  end
  object grpDestination: TGroupBox
    Left = 223
    Top = 208
    Width = 177
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Destination'
    TabOrder = 5
    DesignSize = (
      177
      241)
    object lblDatabase: TLabel
      Left = 10
      Top = 24
      Width = 50
      Height = 13
      Caption = 'Database:'
    end
    object lblTable: TLabel
      Left = 10
      Top = 48
      Width = 30
      Height = 13
      Caption = 'Table:'
    end
    object lblColumns: TLabel
      Left = 10
      Top = 72
      Width = 44
      Height = 13
      Caption = 'Columns:'
    end
    object comboDatabase: TComboBox
      Left = 64
      Top = 21
      Width = 103
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboDatabaseChange
    end
    object comboTable: TComboBox
      Left = 64
      Top = 45
      Width = 103
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = comboTableChange
    end
    object chklistColumns: TCheckListBox
      Left = 10
      Top = 91
      Width = 128
      Height = 141
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 2
    end
    object ToolBarColMove: TToolBar
      Left = 144
      Top = 91
      Width = 23
      Height = 44
      Align = alNone
      Anchors = [akTop, akRight]
      AutoSize = True
      Caption = 'ToolBarColMove'
      Images = MainForm.ImageListMain
      TabOrder = 3
      object btnColUp: TToolButton
        Left = 0
        Top = 0
        Caption = 'btnColUp'
        ImageIndex = 74
        Wrap = True
        OnClick = btnColMoveClick
      end
      object btnColDown: TToolButton
        Left = 0
        Top = 22
        Caption = 'btnColDown'
        ImageIndex = 75
        OnClick = btnColMoveClick
      end
    end
  end
end
