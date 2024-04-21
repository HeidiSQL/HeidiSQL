object frmExportGrid: TfrmExportGrid
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Export grid rows'
  ClientHeight = 412
  ClientWidth = 574
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 530
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    574
    412)
  TextHeight = 14
  object btnOK: TButton
    Left = 410
    Top = 379
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 491
    Top = 379
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object grpSelection: TRadioGroup
    Left = 8
    Top = 168
    Width = 558
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Row selection'
    ItemIndex = 1
    Items.Strings = (
      'Selected rows'
      'All loaded rows')
    TabOrder = 2
  end
  object grpOutput: TGroupBox
    Left = 8
    Top = 8
    Width = 558
    Height = 98
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Output target'
    TabOrder = 0
    DesignSize = (
      558
      98)
    object lblEncoding: TLabel
      Left = 8
      Top = 72
      Width = 54
      Height = 14
      Caption = 'Encoding:'
    end
    object radioOutputCopyToClipboard: TRadioButton
      Left = 8
      Top = 18
      Width = 540
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Copy to clipboard'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ValidateControls
    end
    object radioOutputFile: TRadioButton
      Left = 8
      Top = 43
      Width = 55
      Height = 17
      Caption = 'File'
      TabOrder = 1
      OnClick = ValidateControls
    end
    object editFilename: TButtonedEdit
      Left = 79
      Top = 42
      Width = 469
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      LeftButton.DropDownMenu = popupRecentFiles
      LeftButton.ImageIndex = 75
      LeftButton.Visible = True
      RightButton.ImageIndex = 51
      RightButton.Visible = True
      TabOrder = 2
      TextHint = 'Doubleclick to select file'
      OnChange = editFilenameChange
      OnDblClick = editFilenameRightButtonClick
      OnRightButtonClick = editFilenameRightButtonClick
    end
    object comboEncoding: TComboBox
      Left = 79
      Top = 69
      Width = 469
      Height = 22
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 240
    Width = 558
    Height = 133
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Options'
    TabOrder = 3
    DesignSize = (
      558
      133)
    object lblSeparator: TLabel
      Left = 279
      Top = 18
      Width = 83
      Height = 14
      Caption = 'Field separator:'
    end
    object lblEncloser: TLabel
      Left = 279
      Top = 44
      Width = 49
      Height = 14
      Caption = 'Encloser:'
    end
    object lblTerminator: TLabel
      Left = 279
      Top = 71
      Width = 87
      Height = 14
      Caption = 'Line terminator:'
    end
    object lblNull: TLabel
      Left = 279
      Top = 97
      Width = 64
      Height = 14
      Caption = 'NULL value:'
    end
    object chkIncludeColumnNames: TCheckBox
      Left = 8
      Top = 18
      Width = 257
      Height = 17
      Caption = 'Include column names'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object editSeparator: TButtonedEdit
      Left = 400
      Top = 15
      Width = 148
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 4
      Text = ';'
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object editEncloser: TButtonedEdit
      Left = 400
      Top = 41
      Width = 148
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 5
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object editTerminator: TButtonedEdit
      Left = 400
      Top = 68
      Width = 148
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 6
      Text = '\r\n'
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object chkIncludeAutoIncrement: TCheckBox
      Left = 8
      Top = 41
      Width = 257
      Height = 17
      Caption = 'Include auto increment column'
      TabOrder = 1
    end
    object chkIncludeQuery: TCheckBox
      Left = 8
      Top = 64
      Width = 257
      Height = 17
      Caption = 'Include SQL query'
      TabOrder = 2
    end
    object editNull: TButtonedEdit
      Left = 400
      Top = 94
      Width = 148
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 7
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object chkRemoveLinebreaks: TCheckBox
      Left = 8
      Top = 87
      Width = 257
      Height = 17
      Caption = 'Remove linebreaks from contents'
      TabOrder = 3
    end
  end
  object btnSetClipboardDefaults: TButton
    Left = 8
    Top = 379
    Width = 153
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save clipboard settings'
    ImageIndex = 4
    ImageName = 'icons8-paste-100'
    Images = MainForm.VirtualImageListMain
    TabOrder = 4
    OnClick = btnSetClipboardDefaultsClick
  end
  object grpFormat: TGroupBox
    Left = 8
    Top = 112
    Width = 558
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Output format'
    TabOrder = 1
    DesignSize = (
      558
      50)
    object comboFormat: TComboBoxEx
      Left = 8
      Top = 18
      Width = 540
      Height = 23
      ItemsEx = <
        item
          Caption = 'Excel CSV'
          ImageIndex = 49
          SelectedImageIndex = 49
        end
        item
          Caption = 'CSV'
          ImageIndex = 50
          SelectedImageIndex = 50
        end
        item
          Caption = '...'
        end>
      Style = csExDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Images = MainForm.VirtualImageListMain
      DropDownCount = 20
    end
  end
  object popupCSVchar: TPopupMenu
    AutoHotkeys = maManual
    Left = 160
    Top = 6
    object menuCSVtab: TMenuItem
      Caption = 'Tab'
      Hint = '\t'
      OnClick = menuCSVClick
    end
    object menuCSVcomma: TMenuItem
      Caption = 'Comma'
      Hint = ','
      OnClick = menuCSVClick
    end
    object menuCSVsemicolon: TMenuItem
      Caption = 'Semicolon'
      Hint = ';'
      OnClick = menuCSVClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object menuCSVsinglequote: TMenuItem
      Caption = 'Single quote'
      Hint = #39
      OnClick = menuCSVClick
    end
    object menuCSVdoublequote: TMenuItem
      Caption = 'Double quote'
      Hint = '"'
      OnClick = menuCSVClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object menuCSVwinlinebreak: TMenuItem
      Caption = 'Windows linebreak'
      Hint = '\r\n'
      OnClick = menuCSVClick
    end
    object menuCSVunixlinebreak: TMenuItem
      Caption = 'UNIX linebreak'
      Hint = '\n'
      OnClick = menuCSVClick
    end
    object menuCSVmaclinebreak: TMenuItem
      Caption = 'Mac OS linebreak'
      Hint = '\r'
      OnClick = menuCSVClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object menuCSVnul: TMenuItem
      Caption = 'NUL character'
      Hint = '\0'
      OnClick = menuCSVClick
    end
    object menuCSVbackspace: TMenuItem
      Caption = 'Backspace'
      Hint = '\b'
      OnClick = menuCSVClick
    end
    object menuCSVcontrolz: TMenuItem
      Caption = 'Control+Z'
      Hint = '\Z'
      OnClick = menuCSVClick
    end
  end
  object popupRecentFiles: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = popupRecentFilesPopup
    Left = 248
    Top = 6
  end
end
