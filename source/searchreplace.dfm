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
    Width = 259
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replacement can have \n for new lines and \t for tabs'
    Enabled = False
  end
  object lblSearchIn: TLabel
    Left = 8
    Top = 13
    Width = 48
    Height = 13
    Caption = 'Search in:'
  end
  object btnCancel: TButton
    Left = 327
    Top = 278
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object btnReplaceAll: TButton
    Left = 221
    Top = 278
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Replace &all'
    ModalResult = 12
    TabOrder = 11
    OnClick = DoSearchReplace
  end
  object chkReplace: TCheckBox
    Left = 8
    Top = 64
    Width = 73
    Height = 17
    Caption = '&Replace:'
    TabOrder = 3
    OnClick = chkReplaceClick
  end
  object comboSearch: TComboBox
    Left = 90
    Top = 37
    Width = 305
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
    Width = 305
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 4
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
    TabOrder = 6
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
    TabOrder = 7
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
    TabOrder = 8
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
    TabOrder = 9
  end
  object btnOK: TButton
    Left = 115
    Top = 278
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 10
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
  object btnSearchHints: TButton
    Left = 401
    Top = 35
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #9207
    DropDownMenu = popupSearchHints
    TabOrder = 2
    OnClick = btnWithDropDownClick
  end
  object btnReplaceHints: TButton
    Left = 401
    Top = 60
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #9207
    DropDownMenu = popupReplaceHints
    Enabled = False
    TabOrder = 5
    OnClick = btnWithDropDownClick
  end
  object SynEditSearch1: TSynEditSearch
    Left = 24
    Top = 248
  end
  object SynEditRegexSearch1: TSynEditRegexSearch
    Left = 120
    Top = 248
  end
  object popupSearchHints: TPopupMenu
    Left = 328
    Top = 24
  end
  object popupReplaceHints: TPopupMenu
    Left = 328
    Top = 72
  end
end
