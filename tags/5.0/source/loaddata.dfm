object loaddataform: Tloaddataform
  Left = 212
  Top = 111
  BorderStyle = bsDialog
  BorderWidth = 3
  Caption = 'Import text file'
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
  OnDestroy = FormDestroy
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
        Left = 5
        Top = 2
        Width = 403
        Height = 90
        Anchors = [akLeft, akTop, akRight]
        Caption = 'File'
        TabOrder = 0
        DesignSize = (
          403
          90)
        object lblFilename: TLabel
          Left = 10
          Top = 27
          Width = 46
          Height = 13
          Caption = 'Filename:'
          FocusControl = editFilename
        end
        object lblCharset: TLabel
          Left = 10
          Top = 54
          Width = 70
          Height = 13
          Caption = '&Character set:'
          FocusControl = comboCharset
        end
        object editFilename: TButtonedEdit
          Left = 104
          Top = 24
          Width = 289
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
        object comboCharset: TComboBox
          Left = 104
          Top = 51
          Width = 289
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 16
          TabOrder = 1
        end
      end
      object grpFields: TGroupBox
        Left = 5
        Top = 93
        Width = 403
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
          Text = '"'
        end
        object editFieldEncloser: TEdit
          Left = 104
          Top = 48
          Width = 49
          Height = 21
          TabOrder = 1
          Text = '"'
        end
        object editFieldTerminator: TEdit
          Left = 104
          Top = 24
          Width = 49
          Height = 21
          TabOrder = 2
          Text = ';'
        end
        object chkFieldsEnclosedOptionally: TCheckBox
          Left = 167
          Top = 50
          Width = 73
          Height = 17
          Caption = 'optionally'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object grpLines: TGroupBox
        Left = 5
        Top = 202
        Width = 403
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
          Text = '\r\n'
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
      object comboDatabase: TComboBox
        Left = 10
        Top = 26
        Width = 164
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = comboDatabaseChange
      end
      object comboTable: TComboBox
        Left = 10
        Top = 69
        Width = 164
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = comboTableChange
      end
      object chklistColumns: TCheckListBox
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
      object ToolBarColMove: TToolBar
        Left = 149
        Top = 117
        Width = 23
        Height = 44
        Align = alNone
        AutoSize = True
        Caption = 'ToolBarColMove'
        Images = MainForm.ImageListMain
        TabOrder = 4
        object btnColUp: TToolButton
          Left = 0
          Top = 0
          Caption = 'btnColUp'
          ImageIndex = 74
          Wrap = True
          OnClick = btnColUpClick
        end
        object btnColDown: TToolButton
          Left = 0
          Top = 22
          Caption = 'btnColDown'
          ImageIndex = 75
          OnClick = btnColDownClick
        end
      end
    end
  end
  object OpenDialogCSVFile: TOpenDialog
    DefaultExt = 'csv'
    Filter = 
      'MySQL CSV files (*.csv)|*.csv|Text files (*.txt)|*.txt|All files' +
      ' (*.*)|*.*'
    Left = 392
  end
end