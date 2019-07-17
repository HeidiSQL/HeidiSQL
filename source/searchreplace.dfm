object frmSearchReplace: TfrmSearchReplace
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Search and replace text'
  ClientHeight = 311
  ClientWidth = 434
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    434
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSearch: TLabel
    Left = 8
    Top = 40
    Width = 60
    Height = 13
    Caption = '&Text to find:'
    FocusControl = comboSearch
  end
  object lblReplaceHint: TLabel
    Left = 90
    Top = 87
    Width = 288
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replacement can have \n for new lines and \t for tabs'
    Enabled = False
    WordWrap = True
  end
  object lblSearchIn: TLabel
    Left = 8
    Top = 13
    Width = 48
    Height = 13
    Caption = 'Search in:'
  end
  object btnCancel: TButton
    Left = 351
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object btnReplaceAll: TButton
    Left = 270
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Replace &all'
    ModalResult = 12
    TabOrder = 9
    OnClick = DoSearchReplace
  end
  object chkReplace: TCheckBox
    Left = 8
    Top = 64
    Width = 73
    Height = 17
    Caption = '&Replace:'
    TabOrder = 2
    OnClick = chkReplaceClick
  end
  object comboSearch: TComboBox
    Left = 90
    Top = 37
    Width = 336
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'comboSearch'
    TextHint = 'Enter text to find ...'
    OnExit = comboSearchReplaceExit
  end
  object comboReplace: TComboBox
    Left = 90
    Top = 62
    Width = 336
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 3
    Text = 'comboReplace'
    TextHint = 'Enter replacement pattern ...'
    OnExit = comboSearchReplaceExit
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 109
    Width = 418
    Height = 69
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Options'
    TabOrder = 4
    object chkCaseSensitive: TCheckBox
      Left = 12
      Top = 18
      Width = 166
      Height = 17
      Caption = 'Case sensitive'
      TabOrder = 0
    end
    object chkWholeWords: TCheckBox
      Left = 12
      Top = 41
      Width = 166
      Height = 17
      Caption = 'Whole words'
      TabOrder = 1
    end
    object chkRegularExpression: TCheckBox
      Left = 184
      Top = 18
      Width = 177
      Height = 17
      Caption = 'Regular expression'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ValidateControls
    end
    object chkPromptOnReplace: TCheckBox
      Left = 184
      Top = 41
      Width = 177
      Height = 17
      Caption = 'Prompt on replace'
      TabOrder = 3
    end
  end
  object grpDirection: TRadioGroup
    Left = 8
    Top = 184
    Width = 120
    Height = 88
    Anchors = [akLeft, akTop, akBottom]
    Caption = '&Direction'
    ItemIndex = 0
    Items.Strings = (
      '&Forward'
      '&Backward')
    TabOrder = 5
  end
  object grpOrigin: TRadioGroup
    Left = 134
    Top = 184
    Width = 120
    Height = 88
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Or&igin'
    ItemIndex = 1
    Items.Strings = (
      'From cursor'
      'Entire scope')
    TabOrder = 6
  end
  object grpScope: TRadioGroup
    Left = 260
    Top = 184
    Width = 166
    Height = 88
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '&Scope'
    ItemIndex = 0
    Items.Strings = (
      'Global'
      'Selection')
    TabOrder = 7
  end
  object btnOK: TButton
    Left = 189
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = DoSearchReplace
  end
  object comboSearchIn: TComboBox
    Left = 90
    Top = 10
    Width = 336
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object SynEditSearch1: TSynEditSearch
    Left = 24
    Top = 248
  end
  object SynEditRegexSearch1: TSynEditRegexSearch
    Left = 120
    Top = 248
  end
end
