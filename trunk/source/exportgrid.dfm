object frmExportGrid: TfrmExportGrid
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Export grid rows'
  ClientHeight = 351
  ClientWidth = 383
  Color = clBtnFace
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
  OnShow = FormShow
  DesignSize = (
    383
    351)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 219
    Top = 318
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 300
    Top = 318
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object grpFormat: TRadioGroup
    Left = 8
    Top = 112
    Width = 161
    Height = 200
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Output format'
    ItemIndex = 0
    Items.Strings = (
      'Excel compatible'
      'Delimited text'
      'HTML table'
      'XML'
      'SQL INSERTs'
      'LaTeX'
      'Wiki markup')
    TabOrder = 2
    OnClick = ValidateControls
  end
  object grpSelection: TRadioGroup
    Left = 175
    Top = 112
    Width = 200
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Row selection'
    ItemIndex = 1
    Items.Strings = (
      'Selected rows'
      'All loaded rows')
    TabOrder = 3
  end
  object grpOutput: TGroupBox
    Left = 8
    Top = 8
    Width = 367
    Height = 98
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Output target'
    TabOrder = 4
    DesignSize = (
      367
      98)
    object lblEncoding: TLabel
      Left = 8
      Top = 72
      Width = 47
      Height = 13
      Caption = 'Encoding:'
    end
    object radioOutputCopyToClipboard: TRadioButton
      Left = 8
      Top = 18
      Width = 345
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
      Width = 65
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'File'
      TabOrder = 1
      OnClick = ValidateControls
    end
    object editFilename: TButtonedEdit
      Left = 79
      Top = 42
      Width = 274
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.ImageListMain
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
      Width = 274
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  object grpOptions: TGroupBox
    Left = 175
    Top = 184
    Width = 200
    Height = 128
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Options'
    TabOrder = 5
    DesignSize = (
      200
      128)
    object lblSeparator: TLabel
      Left = 6
      Top = 45
      Width = 76
      Height = 13
      Caption = 'Field separator:'
    end
    object lblEncloser: TLabel
      Left = 6
      Top = 70
      Width = 44
      Height = 13
      Caption = 'Encloser:'
    end
    object lblTerminator: TLabel
      Left = 6
      Top = 96
      Width = 76
      Height = 13
      Caption = 'Line terminator:'
    end
    object chkColumnHeader: TCheckBox
      Left = 8
      Top = 18
      Width = 177
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Column names in first row'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object editSeparator: TButtonedEdit
      Left = 106
      Top = 41
      Width = 80
      Height = 21
      Images = MainForm.ImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 1
      Text = ';'
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object editEncloser: TButtonedEdit
      Left = 106
      Top = 67
      Width = 80
      Height = 21
      Images = MainForm.ImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 2
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object editTerminator: TButtonedEdit
      Left = 106
      Top = 93
      Width = 80
      Height = 21
      Images = MainForm.ImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 3
      Text = '\r\n'
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
  end
  object popupCSVchar: TPopupMenu
    AutoHotkeys = maManual
    Left = 8
    Top = 320
    object menuCSVtab: TMenuItem
      Caption = 'Tab'
      Hint = '\t'
      OnClick = menuCSVClick
    end
    object menuCSVwinlinebreak: TMenuItem
      Caption = 'Windows linebreak'
      Hint = '\r\n'
      OnClick = menuCSVClick
    end
    object menuCSVunixlinebreak: TMenuItem
      Caption = 'UNIX linebreak'
      Hint = '\n'
    end
    object menuCSVmaclinebreak: TMenuItem
      Caption = 'Mac OS linebreak'
      Hint = '\r'
    end
    object menuCSVnul: TMenuItem
      Caption = 'NUL character'
      Hint = '\0'
    end
    object menuCSVbackspace: TMenuItem
      Caption = 'Backspace'
      Hint = '\b'
    end
    object menuCSVcontrolz: TMenuItem
      Caption = 'Control+Z'
      Hint = '\Z'
    end
  end
  object popupRecentFiles: TPopupMenu
    AutoHotkeys = maManual
    Left = 40
    Top = 320
  end
end
