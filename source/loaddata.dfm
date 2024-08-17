object loaddataform: Tloaddataform
  Left = 212
  Top = 111
  Caption = 'Import text file'
  ClientHeight = 548
  ClientWidth = 513
  Color = clBtnFace
  Constraints.MinHeight = 550
  Constraints.MinWidth = 525
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    513
    548)
  TextHeight = 14
  object btnImport: TButton
    Left = 345
    Top = 515
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
    Top = 515
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
      Width = 51
      Height = 14
      Caption = 'Filename:'
      FocusControl = editFilename
    end
    object lblEncoding: TLabel
      Left = 10
      Top = 54
      Width = 54
      Height = 14
      Caption = 'Encoding:'
    end
    object editFilename: TButtonedEdit
      Left = 88
      Top = 24
      Width = 395
      Height = 22
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
      Height = 22
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
      Width = 110
      Height = 14
      Caption = 'Fields terminated by'
    end
    object lblFieldEncloser: TLabel
      Left = 10
      Top = 51
      Width = 98
      Height = 14
      Caption = 'Fields enclosed by'
    end
    object lblFieldEscaper: TLabel
      Left = 10
      Top = 75
      Width = 95
      Height = 14
      Caption = 'Fields escaped by'
    end
    object lblLineTerminator: TLabel
      Left = 10
      Top = 100
      Width = 108
      Height = 14
      Caption = 'Lines terminated by'
    end
    object editFieldEscaper: TEdit
      Left = 145
      Top = 72
      Width = 49
      Height = 22
      TabOrder = 3
      Text = '"'
    end
    object editFieldEncloser: TEdit
      Left = 145
      Top = 48
      Width = 49
      Height = 22
      TabOrder = 1
      Text = '"'
    end
    object editFieldTerminator: TEdit
      Left = 145
      Top = 23
      Width = 49
      Height = 22
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
      Height = 22
      TabOrder = 4
      Text = '\r\n'
    end
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 98
    Width = 209
    Height = 175
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      209
      175)
    object lblIgnoreLinesCount: TLabel
      Left = 143
      Top = 26
      Width = 23
      Height = 14
      Caption = 'lines'
    end
    object lblIgnoreLines: TLabel
      Left = 10
      Top = 26
      Width = 60
      Height = 14
      Caption = 'Ignore first'
    end
    object updownIgnoreLines: TUpDown
      Left = 121
      Top = 23
      Width = 16
      Height = 22
      Associate = editIgnoreLines
      Max = 32767
      Position = 1
      TabOrder = 1
    end
    object editIgnoreLines: TEdit
      Left = 88
      Top = 23
      Width = 33
      Height = 22
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
    object chkKeepDialogOpen: TCheckBox
      Left = 10
      Top = 138
      Width = 196
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Keep dialog open after import'
      TabOrder = 5
    end
  end
  object grpDuplicates: TRadioGroup
    Left = 8
    Top = 279
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
    Top = 408
    Width = 209
    Height = 101
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
    Height = 270
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Destination'
    TabOrder = 5
    DesignSize = (
      278
      270)
    object lblDatabase: TLabel
      Left = 10
      Top = 24
      Width = 54
      Height = 14
      Caption = 'Database:'
    end
    object lblTable: TLabel
      Left = 10
      Top = 48
      Width = 34
      Height = 14
      Caption = 'Table:'
    end
    object lblColumns: TLabel
      Left = 10
      Top = 72
      Width = 49
      Height = 14
      Caption = 'Columns:'
    end
    object comboDatabase: TComboBox
      Left = 112
      Top = 21
      Width = 156
      Height = 22
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboDatabaseChange
    end
    object comboTable: TComboBox
      Left = 112
      Top = 45
      Width = 156
      Height = 22
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = comboTableChange
    end
    object chklistColumns: TCheckListBox
      Left = 112
      Top = 72
      Width = 153
      Height = 183
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 14
      TabOrder = 2
      OnClick = chklistColumnsClick
    end
    object ToolBarColMove: TToolBar
      Left = 10
      Top = 91
      Width = 87
      Height = 66
      Align = alNone
      ButtonWidth = 59
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
        ImageName = 'icons8-sort-up'
        Wrap = True
        OnClick = btnColMoveClick
      end
      object btnColDown: TToolButton
        Left = 0
        Top = 22
        Hint = 'Move down'
        Caption = 'Down'
        ImageIndex = 75
        ImageName = 'icons8-caret-arrowhead-facing-down'
        Wrap = True
        OnClick = btnColMoveClick
      end
      object btnCheckAll: TToolButton
        Left = 0
        Top = 44
        Hint = 'Select / Deselect all'
        Caption = 'All'
        ImageIndex = 128
        ImageName = 'icons8-checked-checkbox-other'
        OnClick = btnCheckAllClick
      end
    end
  end
end
