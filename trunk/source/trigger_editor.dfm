object frmTriggerEditor: TfrmTriggerEditor
  Left = 0
  Top = 0
  Width = 477
  Height = 332
  TabOrder = 0
  DesignSize = (
    477
    332)
  object lblName: TLabel
    Left = 3
    Top = 6
    Width = 31
    Height = 13
    Caption = 'Name:'
    FocusControl = editName
  end
  object lblTable: TLabel
    Left = 3
    Top = 33
    Width = 45
    Height = 13
    Caption = 'On table:'
  end
  object lblBody: TLabel
    Left = 3
    Top = 87
    Width = 327
    Height = 13
    Caption = 'Trigger statement: (e.g. "SET NEW.columnA = TRIM(OLD.columnA)"'
  end
  object lblEvent: TLabel
    Left = 3
    Top = 59
    Width = 32
    Height = 13
    Caption = 'Event:'
  end
  object lblDefiner: TLabel
    Left = 247
    Top = 6
    Width = 39
    Height = 13
    Caption = 'Definer:'
  end
  object editName: TEdit
    Left = 84
    Top = 3
    Width = 157
    Height = 21
    TabOrder = 0
    Text = 'editName'
    TextHint = 'Enter trigger name'
    OnChange = Modification
  end
  object SynMemoBody: TSynMemo
    Left = 3
    Top = 106
    Width = 471
    Height = 192
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 4
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Lines.Strings = (
      'SynMemoBody')
    OnChange = Modification
  end
  object btnHelp: TButton
    Left = 3
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object btnDiscard: TButton
    Left = 84
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Discard'
    TabOrder = 6
    OnClick = btnDiscardClick
  end
  object btnSave: TButton
    Left = 165
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 7
    OnClick = btnSaveClick
  end
  object comboTable: TComboBox
    Left = 84
    Top = 30
    Width = 390
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = Modification
  end
  object comboTiming: TComboBox
    Left = 84
    Top = 56
    Width = 157
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = Modification
  end
  object comboEvent: TComboBox
    Left = 247
    Top = 56
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = Modification
  end
  object comboDefiner: TComboBox
    Left = 304
    Top = 3
    Width = 170
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    Text = 'comboDefiner'
    OnChange = Modification
    OnDropDown = comboDefinerDropDown
  end
  object SynCompletionProposalStatement: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'CONSTRUCTOR'
      end>
    Images = MainForm.ImageListMain
    OnExecute = SynCompletionProposalStatementExecute
    ShortCut = 16416
    Editor = SynMemoBody
    Left = 264
    Top = 304
  end
end
