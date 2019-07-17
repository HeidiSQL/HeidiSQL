object frmExportGrid: TfrmExportGrid
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Export grid rows'
  ClientHeight = 422
  ClientWidth = 373
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 350
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
    373
    422)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 209
    Top = 389
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
    Left = 290
    Top = 389
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
    Width = 137
    Height = 271
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Output format'
    ItemIndex = 0
    Items.Strings = (
      'Excel compatible'
      'Delimited text'
      'HTML table'
      'XML'
      'SQL INSERTs'
      'SQL REPLACEs'
      'LaTeX'
      'Wiki markup'
      'PHP Array')
    TabOrder = 2
    OnClick = grpFormatClick
  end
  object grpSelection: TRadioGroup
    Left = 151
    Top = 112
    Width = 214
    Height = 66
    Anchors = [akTop, akRight]
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
    Width = 357
    Height = 98
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Output target'
    TabOrder = 4
    DesignSize = (
      357
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
      Width = 335
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
      Width = 264
      Height = 21
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
      Width = 264
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  object grpOptions: TGroupBox
    Left = 151
    Top = 184
    Width = 214
    Height = 199
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Options'
    TabOrder = 5
    DesignSize = (
      214
      199)
    object lblSeparator: TLabel
      Left = 6
      Top = 97
      Width = 76
      Height = 13
      Caption = 'Field separator:'
    end
    object lblEncloser: TLabel
      Left = 6
      Top = 122
      Width = 44
      Height = 13
      Caption = 'Encloser:'
    end
    object lblTerminator: TLabel
      Left = 6
      Top = 148
      Width = 76
      Height = 13
      Caption = 'Line terminator:'
    end
    object lblNull: TLabel
      Left = 6
      Top = 175
      Width = 57
      Height = 13
      Caption = 'NULL value:'
    end
    object chkIncludeColumnNames: TCheckBox
      Left = 6
      Top = 18
      Width = 191
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Include column names'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object editSeparator: TButtonedEdit
      Left = 106
      Top = 93
      Width = 93
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 3
      Text = ';'
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object editEncloser: TButtonedEdit
      Left = 106
      Top = 119
      Width = 93
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 4
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object editTerminator: TButtonedEdit
      Left = 106
      Top = 145
      Width = 93
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 5
      Text = '\r\n'
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
    object chkIncludeAutoIncrement: TCheckBox
      Left = 6
      Top = 41
      Width = 191
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Include auto increment column'
      TabOrder = 1
    end
    object chkIncludeQuery: TCheckBox
      Left = 6
      Top = 64
      Width = 177
      Height = 17
      Caption = 'Include SQL query'
      TabOrder = 2
    end
    object editNull: TButtonedEdit
      Left = 106
      Top = 172
      Width = 93
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.VirtualImageListMain
      RightButton.DisabledImageIndex = 107
      RightButton.ImageIndex = 108
      RightButton.Visible = True
      TabOrder = 6
      OnChange = editCSVChange
      OnRightButtonClick = editCSVRightButtonClick
    end
  end
  object btnSetClipboardDefaults: TButton
    Left = 8
    Top = 389
    Width = 153
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save clipboard settings'
    ImageIndex = 4
    Images = MainForm.VirtualImageListMain
    TabOrder = 6
    OnClick = btnSetClipboardDefaultsClick
  end
  object popupCSVchar: TPopupMenu
    AutoHotkeys = maManual
    Left = 224
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
    Left = 312
    Top = 6
  end
end
