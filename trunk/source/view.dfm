object frmView: TfrmView
  Left = 0
  Top = 0
  Width = 700
  Height = 500
  TabOrder = 0
  DesignSize = (
    700
    500)
  object lblName: TLabel
    Left = 3
    Top = 6
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblSelect: TLabel
    Left = 3
    Top = 124
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
  object lblDefiner: TLabel
    Left = 408
    Top = 6
    Width = 39
    Height = 13
    Caption = 'Definer:'
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
  object rgAlgorithm: TRadioGroup
    Left = 3
    Top = 32
    Width = 391
    Height = 86
    Caption = 'Algorithm'
    ItemIndex = 0
    Items.Strings = (
      'UNDEFINED'
      'MERGE'
      'TEMPTABLE')
    TabOrder = 2
    OnClick = Modification
  end
  object SynMemoBody: TSynMemo
    Left = 3
    Top = 143
    Width = 693
    Height = 324
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 4
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
    TabOrder = 6
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
    TabOrder = 7
    OnClick = btnSaveClick
  end
  object rgCheck: TRadioGroup
    Left = 408
    Top = 32
    Width = 288
    Height = 86
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Check option for updates'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'CASCADED'
      'LOCAL')
    TabOrder = 3
    OnClick = Modification
  end
  object btnHelp: TButton
    Left = 3
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object comboDefiner: TComboBox
    Left = 489
    Top = 3
    Width = 207
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'comboDefiner'
    OnChange = Modification
    OnDropDown = comboDefinerDropDown
  end
end
