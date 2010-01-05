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
    Caption = 'Trigger statement: (e.g. "SET NEW.columnA = TRIM(OLD.columnA")'
  end
  object lblEvent: TLabel
    Left = 3
    Top = 59
    Width = 32
    Height = 13
    Caption = 'Event:'
  end
  object editName: TEdit
    Left = 96
    Top = 3
    Width = 378
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'editName'
    OnChange = Modification
  end
  object SynMemoStatement: TSynMemo
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
    TabOrder = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Lines.Strings = ('SynMemoStatement')
    OnChange = Modification
  end
  object btnHelp: TButton
    Left = 3
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object btnDiscard: TButton
    Left = 84
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Discard'
    TabOrder = 4
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
    TabOrder = 5
    OnClick = btnSaveClick
  end
  object comboTable: TComboBox
    Left = 96
    Top = 30
    Width = 378
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnChange = Modification
  end
  object comboTiming: TComboBox
    Left = 96
    Top = 56
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    OnChange = Modification
  end
  object comboEvent: TComboBox
    Left = 247
    Top = 56
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    OnChange = Modification
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
        BiggestWordW = 'CONSTRUCTOR'
      end>
    Images = MainForm.PngImageListMain
    OnExecute = SynCompletionProposalStatementExecute
    ShortCut = 16416
    Editor = SynMemoStatement
    Left = 264
    Top = 304
    EndOfTokenChrW = '()[]. '
    TriggerCharsW = '.'
  end
end
