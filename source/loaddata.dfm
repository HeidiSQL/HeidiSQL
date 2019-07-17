object loaddataform: Tloaddataform
  Left = 212
  Top = 111
  Caption = 'Import text file'
  ClientHeight = 494
  ClientWidth = 509
  Color = clBtnFace
  Constraints.MinHeight = 530
  Constraints.MinWidth = 525
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    509
    494)
  PixelsPerInch = 96
  TextHeight = 13
  object btnImport: TButton
    Left = 345
    Top = 461
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
    Left = 426
    Top = 461
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
    Width = 493
    Height = 84
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Input file'
    TabOrder = 0
    DesignSize = (
      493
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
      Width = 395
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
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
      Width = 395
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      Sorted = True
      TabOrder = 1
    end
  end
  object grpChars: TGroupBox
    Left = 223
    Top = 98
    Width = 278
    Height = 135
    Anchors = [akTop, akRight]
    Caption = 'Control characters'
    TabOrder = 2
    DesignSize = (
      278
      135)
    object lblFieldTerminater: TLabel
      Left = 10
      Top = 26
      Width = 97
      Height = 13
      Caption = 'Fields terminated by'
    end
    object lblFieldEncloser: TLabel
      Left = 10
      Top = 51
      Width = 87
      Height = 13
      Caption = 'Fields enclosed by'
    end
    object lblFieldEscaper: TLabel
      Left = 10
      Top = 75
      Width = 85
      Height = 13
      Caption = 'Fields escaped by'
    end
    object lblLineTerminator: TLabel
      Left = 10
      Top = 100
      Width = 94
      Height = 13
      Caption = 'Lines terminated by'
    end
    object editFieldEscaper: TEdit
      Left = 145
      Top = 72
      Width = 49
      Height = 21
      TabOrder = 3
      Text = '"'
    end
    object editFieldEncloser: TEdit
      Left = 145
      Top = 48
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '"'
    end
    object editFieldTerminator: TEdit
      Left = 145
      Top = 23
      Width = 49
      Height = 21
      TabOrder = 0
      Text = ';'
    end
    object chkFieldsEnclosedOptionally: TCheckBox
      Left = 200
      Top = 50
      Width = 75
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'optionally'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object editLineTerminator: TEdit
      Left = 145
      Top = 97
      Width = 49
      Height = 21
      TabOrder = 4
      Text = '\r\n'
    end
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 98
    Width = 209
    Height = 135
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      209
      135)
    object lblIgnoreLinesCount: TLabel
      Left = 143
      Top = 26
      Width = 21
      Height = 13
      Caption = 'lines'
    end
    object lblIgnoreLines: TLabel
      Left = 10
      Top = 26
      Width = 54
      Height = 13
      Caption = 'Ignore first'
    end
    object updownIgnoreLines: TUpDown
      Left = 121
      Top = 23
      Width = 16
      Height = 21
      Associate = editIgnoreLines
      Max = 32767
      Position = 1
      TabOrder = 1
    end
    object editIgnoreLines: TEdit
      Left = 88
      Top = 23
      Width = 33
      Height = 21
      TabOrder = 0
      Text = '1'
    end
    object chkLowPriority: TCheckBox
      Left = 10
      Top = 51
      Width = 196
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Low priority, avoid high server load'
      Checked = True
      State = cbChecked
      TabOrder = 2
      WordWrap = True
    end
    object chkLocalNumbers: TCheckBox
      Left = 10
      Top = 70
      Width = 196
      Height = 35
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Input file contains local formatted numbers, e.g. 1.234,56 in Ge' +
        'rmany'
      TabOrder = 3
      WordWrap = True
    end
    object chkTruncateTable: TCheckBox
      Left = 10
      Top = 108
      Width = 196
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Truncate destination table before import'
      TabOrder = 4
    end
  end
  object grpDuplicates: TRadioGroup
    Left = 8
    Top = 239
    Width = 209
    Height = 123
    Anchors = [akLeft, akTop, akRight]
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
    Top = 368
    Width = 209
    Height = 87
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    Top = 239
    Width = 278
    Height = 216
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Destination'
    TabOrder = 5
    DesignSize = (
      278
      216)
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
      Left = 112
      Top = 21
      Width = 156
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboDatabaseChange
    end
    object comboTable: TComboBox
      Left = 112
      Top = 45
      Width = 156
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = comboTableChange
    end
    object chklistColumns: TCheckListBox
      Left = 112
      Top = 72
      Width = 153
      Height = 129
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 2
      OnClick = chklistColumnsClick
    end
    object ToolBarColMove: TToolBar
      Left = 10
      Top = 91
      Width = 58
      Height = 66
      Align = alNone
      AutoSize = True
      ButtonWidth = 58
      Caption = 'ToolBarColMove'
      Images = MainForm.VirtualImageListMain
      List = True
      ParentShowHint = False
      ShowCaptions = True
      ShowHint = True
      TabOrder = 3
      object btnColUp: TToolButton
        Left = 0
        Top = 0
        Hint = 'Move up'
        Caption = 'Up'
        ImageIndex = 74
        Wrap = True
        OnClick = btnColMoveClick
      end
      object btnColDown: TToolButton
        Left = 0
        Top = 22
        Hint = 'Move down'
        Caption = 'Down'
        ImageIndex = 75
        Wrap = True
        OnClick = btnColMoveClick
      end
      object btnCheckAll: TToolButton
        Left = 0
        Top = 44
        Hint = 'Select / Deselect all'
        Caption = 'All'
        ImageIndex = 128
        OnClick = btnCheckAllClick
      end
    end
  end
end
