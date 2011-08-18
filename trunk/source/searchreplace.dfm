object frmSearchReplace: TfrmSearchReplace
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Search and replace text'
  ClientHeight = 256
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    388
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSearch: TLabel
    Left = 8
    Top = 11
    Width = 60
    Height = 13
    Caption = '&Text to find:'
    FocusControl = comboSearch
  end
  object lblReplaceHint: TLabel
    Left = 90
    Top = 58
    Width = 259
    Height = 13
    Caption = 'Replacement can have \n for new lines and \t for tabs'
    Enabled = False
  end
  object btnCancel: TButton
    Left = 305
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object btnReplaceAll: TButton
    Left = 224
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Replace &all'
    ModalResult = 8
    TabOrder = 8
  end
  object chkReplace: TCheckBox
    Left = 8
    Top = 35
    Width = 73
    Height = 17
    Caption = '&Replace:'
    TabOrder = 1
    OnClick = chkReplaceClick
  end
  object comboSearch: TComboBox
    Left = 90
    Top = 8
    Width = 290
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'comboSearch'
    TextHint = 'Enter text to find ...'
    OnExit = comboSearchReplaceExit
  end
  object comboReplace: TComboBox
    Left = 90
    Top = 33
    Width = 290
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 2
    Text = 'comboReplace'
    TextHint = 'Enter replacement pattern ...'
    OnExit = comboSearchReplaceExit
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 80
    Width = 372
    Height = 69
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Options'
    TabOrder = 3
    object chkCaseSensitive: TCheckBox
      Left = 12
      Top = 18
      Width = 130
      Height = 17
      Caption = 'Case sensitive'
      TabOrder = 0
    end
    object chkWholeWords: TCheckBox
      Left = 12
      Top = 41
      Width = 130
      Height = 17
      Caption = 'Whole words'
      TabOrder = 1
    end
    object chkRegularExpression: TCheckBox
      Left = 156
      Top = 18
      Width = 130
      Height = 17
      Caption = 'Regular expression'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ValidateControls
    end
    object chkPromptOnReplace: TCheckBox
      Left = 156
      Top = 41
      Width = 130
      Height = 17
      Caption = 'Prompt on replace'
      TabOrder = 3
    end
  end
  object grpDirection: TRadioGroup
    Left = 8
    Top = 155
    Width = 120
    Height = 62
    Anchors = [akLeft, akTop, akBottom]
    Caption = '&Direction'
    ItemIndex = 0
    Items.Strings = (
      '&Forward'
      '&Backward')
    TabOrder = 4
  end
  object grpOrigin: TRadioGroup
    Left = 134
    Top = 155
    Width = 120
    Height = 62
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Or&igin'
    ItemIndex = 0
    Items.Strings = (
      'From cursor'
      'Entire scope')
    TabOrder = 5
  end
  object grpScope: TRadioGroup
    Left = 260
    Top = 155
    Width = 120
    Height = 62
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '&Scope'
    ItemIndex = 0
    Items.Strings = (
      'Global'
      'Selected text')
    TabOrder = 6
  end
  object btnOK: TButton
    Left = 143
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
end
