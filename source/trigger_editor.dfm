object frmTriggerEditor: TfrmTriggerEditor
  Left = 0
  Top = 0
  Width = 700
  Height = 500
  TabOrder = 0
  DesignSize = (
    700
    500)
  object lblBody: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 141
    Width = 694
    Height = 13
    Align = alTop
    Caption = 'Trigger statement: (e.g. "SET NEW.columnA = TRIM(OLD.columnA)"'
  end
  object PageControlMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 694
    Height = 132
    ActivePage = tabOptions
    Align = alTop
    Images = MainForm.VirtualImageListMain
    TabOrder = 4
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 39
      ImageName = 'icons8-support'
      DesignSize = (
        686
        103)
      object lblDefiner: TLabel
        Left = 247
        Top = 6
        Width = 39
        Height = 13
        Caption = 'Definer:'
      end
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
      object lblEvent: TLabel
        Left = 3
        Top = 59
        Width = 32
        Height = 13
        Caption = 'Event:'
      end
      object comboDefiner: TComboBox
        Left = 304
        Top = 3
        Width = 379
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'comboDefiner'
        OnChange = Modification
        OnDropDown = comboDefinerDropDown
      end
      object editName: TEdit
        Left = 84
        Top = 3
        Width = 157
        Height = 21
        TabOrder = 1
        Text = 'editName'
        TextHint = 'Enter trigger name'
        OnChange = Modification
      end
      object comboTable: TComboBox
        Left = 84
        Top = 30
        Width = 599
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = comboChange
      end
      object comboTiming: TComboBox
        Left = 84
        Top = 56
        Width = 157
        Height = 21
        Style = csDropDownList
        TabOrder = 3
        OnChange = comboChange
      end
      object comboEvent: TComboBox
        Left = 247
        Top = 56
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = comboChange
      end
    end
    object tabCreateCode: TTabSheet
      Caption = 'CREATE code'
      ImageIndex = 119
      ImageName = 'icons8-source-code-other'
      object SynMemoCreateCode: TSynMemo
        Left = 0
        Top = 0
        Width = 686
        Height = 103
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        CodeFolding.GutterShapeSize = 11
        CodeFolding.CollapsedLineColor = clGrayText
        CodeFolding.FolderBarLinesColor = clGrayText
        CodeFolding.IndentGuidesColor = clGray
        CodeFolding.IndentGuides = True
        CodeFolding.ShowCollapsedLine = False
        CodeFolding.ShowHintMark = True
        UseCodeFolding = False
        Gutter.AutoSize = True
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        Highlighter = MainForm.SynSQLSynUsed
        Lines.Strings = (
          'SynMemoCreateCode')
        ReadOnly = True
        WantTabs = True
        FontSmoothing = fsmNone
      end
    end
  end
  object SynMemoBody: TSynMemo
    AlignWithMargins = True
    Left = 3
    Top = 160
    Width = 694
    Height = 308
    Margins.Bottom = 32
    SingleLineMode = False
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Lines.Strings = (
      'SynMemoBody')
    OnChange = Modification
    FontSmoothing = fsmNone
  end
  object btnHelp: TButton
    Left = 3
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 1
    OnClick = btnHelpClick
  end
  object btnDiscard: TButton
    Left = 84
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Discard'
    TabOrder = 2
    OnClick = btnDiscardClick
  end
  object btnSave: TButton
    Left = 165
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 3
    OnClick = btnSaveClick
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
        ColumnWidth = 100
      end>
    Images = MainForm.VirtualImageListMain
    OnExecute = SynCompletionProposalStatementExecute
    ShortCut = 16416
    Editor = SynMemoBody
    Left = 264
    Top = 304
  end
end
