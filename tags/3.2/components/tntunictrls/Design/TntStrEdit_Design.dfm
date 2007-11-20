object TntStrEditDlg: TTntStrEditDlg
  Left = 267
  Top = 258
  BorderStyle = bsDialog
  Caption = 'Wide String List Editor'
  ClientHeight = 279
  ClientWidth = 430
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  PopupMenu = StringEditorMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TntGroupBox1: TTntGroupBox
    Left = 8
    Top = 3
    Width = 413
    Height = 234
    TabOrder = 4
    object LineCount: TTntLabel
      Left = 9
      Top = 9
      Width = 169
      Height = 17
      AutoSize = False
      Caption = '0 lines'
    end
    object UnicodeEnabledLbl: TTntLabel
      Left = 325
      Top = 9
      Width = 79
      Height = 13
      Alignment = taRightJustify
      Caption = 'Unicode Enabled'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object Memo: TTntMemo
      Left = 8
      Top = 28
      Width = 397
      Height = 197
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      OnChange = UpdateStatus
      OnKeyDown = Memo1KeyDown
    end
  end
  object CodeWndBtn: TTntButton
    Left = 8
    Top = 248
    Width = 75
    Height = 25
    Caption = '&Code Editor...'
    Enabled = False
    TabOrder = 0
    OnClick = CodeWndBtnClick
  end
  object HelpButton: TTntButton
    Left = 345
    Top = 248
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object OKButton: TTntButton
    Left = 185
    Top = 248
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TTntButton
    Left = 265
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OpenDialog: TTntOpenDialog
    HelpContext = 26040
    DefaultExt = 'TXT'
    Filter = 
      'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|' +
      'Batch files (*.BAT)|*.BAT|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Load string list'
    Left = 200
    Top = 88
  end
  object SaveDialog: TTntSaveDialog
    HelpContext = 26050
    Filter = 
      'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|' +
      'Batch files (*.BAT)|*.BAT|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist, ofEnableSizing]
    Title = 'Save string list'
    Left = 228
    Top = 88
  end
  object StringEditorMenu: TTntPopupMenu
    Left = 168
    Top = 88
    object LoadItem: TTntMenuItem
      Caption = '&Load...'
      OnClick = FileOpenClick
    end
    object SaveItem: TTntMenuItem
      Caption = '&Save...'
      OnClick = FileSaveClick
    end
    object CodeEditorItem: TTntMenuItem
      Caption = '&Code Editor...'
      Enabled = False
      Visible = False
      OnClick = CodeWndBtnClick
    end
  end
end
