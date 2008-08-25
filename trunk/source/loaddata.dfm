object loaddataform: Tloaddataform
  Left = 212
  Top = 111
  BorderStyle = bsDialog
  BorderWidth = 3
  Caption = 'Import Textfile'
  ClientHeight = 343
  ClientWidth = 423
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
    423
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object btnImport: TButton
    Left = 268
    Top = 316
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Import!'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
    OnClick = btnImportClick
  end
  object btnCancel: TButton
    Left = 348
    Top = 316
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControlMain: TPageControl
    Left = 0
    Top = 0
    Width = 423
    Height = 310
    ActivePage = tabSource
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object tabSource: TTabSheet
      Caption = 'Source'
      DesignSize = (
        415
        282)
      object grpFilename: TGroupBox
        Left = 10
        Top = 10
        Width = 394
        Height = 63
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Filename'
        TabOrder = 0
        DesignSize = (
          394
          63)
        object btnOpenFile: TPngSpeedButton
          Left = 353
          Top = 24
          Width = 22
          Height = 22
          Anchors = [akTop, akRight]
          Flat = True
          OnClick = btnOpenFileClick
        end
        object editFilename: TEdit
          Left = 16
          Top = 24
          Width = 331
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'editFilename'
          OnChange = editFilenameChange
          OnDblClick = btnOpenFileClick
        end
      end
      object grpFields: TGroupBox
        Left = 10
        Top = 78
        Width = 394
        Height = 109
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Fields'
        TabOrder = 1
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
          Left = 104
          Top = 72
          Width = 49
          Height = 21
          TabOrder = 0
          Text = '\\'
        end
        object editFieldEncloser: TEdit
          Left = 104
          Top = 48
          Width = 49
          Height = 21
          TabOrder = 1
        end
        object editFieldTerminator: TEdit
          Left = 104
          Top = 24
          Width = 49
          Height = 21
          TabOrder = 2
          Text = '\t'
        end
        object chkFieldsEnclosedOptionally: TCheckBox
          Left = 167
          Top = 50
          Width = 73
          Height = 17
          Caption = 'optionally'
          TabOrder = 3
        end
      end
      object grpLines: TGroupBox
        Left = 10
        Top = 193
        Width = 394
        Height = 74
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Lines'
        TabOrder = 2
        object lblIgnoreLinesCount: TLabel
          Left = 166
          Top = 44
          Width = 24
          Height = 13
          Caption = 'Lines'
        end
        object lblLineTerminator: TLabel
          Left = 16
          Top = 20
          Width = 67
          Height = 13
          Caption = 'terminated by'
        end
        object lblIgnoreLines: TLabel
          Left = 16
          Top = 44
          Width = 30
          Height = 13
          Caption = 'ignore'
        end
        object updownIgnoreLines: TUpDown
          Left = 141
          Top = 41
          Width = 16
          Height = 21
          Associate = editIgnoreLines
          Max = 32767
          Position = 1
          TabOrder = 0
        end
        object editIgnoreLines: TEdit
          Left = 108
          Top = 41
          Width = 33
          Height = 21
          TabOrder = 1
          Text = '1'
        end
        object editLineTerminator: TEdit
          Left = 108
          Top = 17
          Width = 49
          Height = 21
          TabOrder = 2
          Text = '\n'
        end
      end
    end
    object tabDestination: TTabSheet
      Caption = 'Destination'
      ImageIndex = 1
      DesignSize = (
        415
        282)
      object lblDatabase: TLabel
        Left = 10
        Top = 10
        Width = 50
        Height = 13
        Caption = 'Database:'
      end
      object lblTable: TLabel
        Left = 10
        Top = 53
        Width = 84
        Height = 13
        Caption = 'Import into table:'
      end
      object lblColumns: TLabel
        Left = 10
        Top = 101
        Width = 65
        Height = 13
        Caption = 'Use Columns:'
      end
      object btnColUp: TPngSpeedButton
        Left = 150
        Top = 117
        Width = 25
        Height = 25
        Anchors = [akTop, akRight]
        Flat = True
        OnClick = btnColUpClick
      end
      object btnColDown: TPngSpeedButton
        Left = 150
        Top = 142
        Width = 25
        Height = 25
        Anchors = [akTop, akRight]
        Flat = True
        OnClick = btnColDownClick
      end
      object comboDatabase: TTntComboBox
        Left = 10
        Top = 26
        Width = 164
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = comboDatabaseChange
      end
      object comboTable: TTntComboBox
        Left = 10
        Top = 69
        Width = 164
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = comboTableChange
      end
      object chklistColumns: TTntCheckListBox
        Left = 10
        Top = 117
        Width = 133
        Height = 150
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
      object grpOptions: TGroupBox
        Left = 196
        Top = 10
        Width = 209
        Height = 105
        Anchors = [akTop, akRight]
        Caption = 'Options'
        TabOrder = 3
        object lblDuplicates: TLabel
          Left = 16
          Top = 54
          Width = 143
          Height = 13
          Caption = 'Handling of duplicate records:'
        end
        object chkLowPriority: TCheckBox
          Left = 16
          Top = 23
          Width = 81
          Height = 17
          Caption = 'Low Priority'
          TabOrder = 0
        end
        object chkReplace: TCheckBox
          Left = 16
          Top = 71
          Width = 65
          Height = 17
          Caption = 'Replace'
          TabOrder = 1
          OnClick = chkReplaceClick
        end
        object chkIgnore: TCheckBox
          Left = 96
          Top = 70
          Width = 57
          Height = 17
          Caption = 'Ignore'
          TabOrder = 2
          OnClick = chkIgnoreClick
        end
      end
    end
  end
  object OpenDialogCSVFile: TOpenDialog
    DefaultExt = 'csv'
    Filter = 
      'CSV-Files (*.csv)|*.csv|Textfiles (*.txt)|*.txt|All Files (*.*)|' +
      '*.*'
    Left = 392
  end
end
