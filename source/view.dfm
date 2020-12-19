object frmView: TfrmView
  Left = 0
  Top = 0
  Width = 700
  Height = 500
  TabOrder = 0
  DesignSize = (
    700
    500)
  object lblSelect: TLabel
    Left = 3
    Top = 149
    Width = 85
    Height = 13
    Caption = 'Select statement:'
  end
  object lblDisabledWhy: TLabel
    Left = 280
    Top = 472
    Width = 416
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'You need the SHOW VIEW privilege in order to edit a view.'
    Layout = tlCenter
    Visible = False
  end
  object SynMemoBody: TSynMemo
    Left = 3
    Top = 179
    Width = 693
    Height = 288
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 2
    Gutter.RightOffset = 0
    Gutter.ShowLineNumbers = True
    Options = [eoAutoIndent, eoDropFiles, eoGroupUndo, eoShowScrollHint]
    RightEdge = 0
    WantTabs = True
    OnChange = Modification
    FontSmoothing = fsmNone
    RemovedKeystrokes = <
      item
        Command = ecDeleteLine
        ShortCut = 16473
      end>
    AddedKeystrokes = <
      item
        Command = ecRedo
        ShortCut = 16473
      end>
  end
  object btnDiscard: TButton
    Left = 84
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Discard'
    TabOrder = 3
    OnClick = btnDiscardClick
  end
  object btnSave: TButton
    Left = 162
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnHelp: TButton
    Left = 3
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object PageControlMain: TPageControl
    Left = 0
    Top = 0
    Width = 700
    Height = 177
    ActivePage = tabOptions
    Align = alTop
    Images = MainForm.VirtualImageListMain
    TabOrder = 0
    ExplicitTop = -4
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 14
      DesignSize = (
        692
        148)
      object lblName: TLabel
        Left = 3
        Top = 6
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblDefiner: TLabel
        Left = 408
        Top = 6
        Width = 39
        Height = 13
        Caption = 'Definer:'
      end
      object lblSecurity: TLabel
        Left = 408
        Top = 32
        Width = 64
        Height = 13
        Caption = 'SQL security:'
      end
      object rgAlgorithm: TRadioGroup
        Left = 3
        Top = 57
        Width = 391
        Height = 86
        Caption = 'Algorithm'
        ItemIndex = 0
        Items.Strings = (
          'UNDEFINED'
          'MERGE'
          'TEMPTABLE')
        TabOrder = 3
        OnClick = Modification
      end
      object editName: TEdit
        Left = 84
        Top = 3
        Width = 310
        Height = 21
        TabOrder = 0
        Text = 'editName'
        TextHint = 'Enter view name'
        OnChange = Modification
      end
      object comboDefiner: TComboBox
        Left = 485
        Top = 3
        Width = 204
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'comboDefiner'
        OnChange = Modification
        OnDropDown = comboDefinerDropDown
      end
      object comboSecurity: TComboBox
        Left = 485
        Top = 30
        Width = 204
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = Modification
      end
      object rgCheck: TRadioGroup
        Left = 404
        Top = 57
        Width = 285
        Height = 86
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Check option for updates'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'CASCADED'
          'LOCAL')
        TabOrder = 4
        OnClick = Modification
      end
    end
    object tabCreateCode: TTabSheet
      Caption = 'CREATE code'
      ImageIndex = 39
      object SynMemoCreateCode: TSynMemo
        Left = 0
        Top = 0
        Width = 692
        Height = 148
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
        ReadOnly = True
        FontSmoothing = fsmNone
      end
    end
  end
end
