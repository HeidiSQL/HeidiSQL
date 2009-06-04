object frmView: TfrmView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Constraints.MinHeight = 240
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    451
    304)
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
  object editName: TEdit
    Left = 42
    Top = 3
    Width = 405
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'editName'
    OnChange = editNameChange
  end
  object rgAlgorithm: TRadioGroup
    Left = 3
    Top = 32
    Width = 206
    Height = 86
    Caption = 'Algorithm'
    ItemIndex = 0
    Items.Strings = (
      'Undefined'
      'Merge'
      'Temptable')
    TabOrder = 1
  end
  object SynMemoSelect: TSynMemo
    Left = 3
    Top = 143
    Width = 444
    Height = 128
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 10
    Gutter.RightOffset = 0
    Gutter.ShowLineNumbers = True
    Options = [eoAutoIndent, eoDropFiles, eoGroupUndo, eoShowScrollHint]
    RightEdge = 0
    WantTabs = True
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
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Discard'
    TabOrder = 3
    OnClick = btnDiscardClick
  end
  object btnSave: TButton
    Left = 162
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object rgCheck: TRadioGroup
    Left = 215
    Top = 32
    Width = 232
    Height = 86
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Check option for updates'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Cascaded'
      'Local')
    TabOrder = 5
  end
  object btnHelp: TButton
    Left = 3
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = btnHelpClick
  end
end
