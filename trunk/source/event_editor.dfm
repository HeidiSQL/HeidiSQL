object frmEventEditor: TfrmEventEditor
  Left = 0
  Top = 0
  Width = 672
  Height = 438
  TabOrder = 0
  DesignSize = (
    672
    438)
  object lblBody: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 175
    Width = 666
    Height = 13
    Align = alTop
    Caption = 'Execution body:'
    FocusControl = SynMemoBody
  end
  object SynMemoBody: TSynMemo
    AlignWithMargins = True
    Left = 3
    Top = 194
    Width = 666
    Height = 214
    Margins.Bottom = 30
    SingleLineMode = False
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 2
    Gutter.ShowLineNumbers = True
    Highlighter = MainForm.SynSQLSyn1
    Lines.Strings = (
      'SynMemoBody')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    OnChange = Modification
  end
  object btnHelp: TButton
    Left = 3
    Top = 410
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object btnDiscard: TButton
    Left = 84
    Top = 410
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Discard'
    TabOrder = 3
    OnClick = btnDiscardClick
  end
  object btnSave: TButton
    Left = 165
    Top = 410
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object PageControlMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 666
    Height = 166
    ActivePage = tabSettings
    Align = alTop
    Images = MainForm.ImageListMain
    TabOrder = 0
    OnChange = PageControlMainChange
    object tabSettings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 39
      DesignSize = (
        658
        137)
      object lblName: TLabel
        Left = 3
        Top = 6
        Width = 31
        Height = 13
        Caption = 'Name:'
        FocusControl = editName
      end
      object lblComment: TLabel
        Left = 3
        Top = 33
        Width = 49
        Height = 13
        Caption = 'Comment:'
      end
      object lblDefiner: TLabel
        Left = 408
        Top = 6
        Width = 39
        Height = 13
        Caption = 'De&finer:'
      end
      object editName: TEdit
        Left = 84
        Top = 3
        Width = 310
        Height = 21
        TabOrder = 0
        Text = 'editName'
        TextHint = 'Enter event name ...'
        OnChange = Modification
      end
      object chkDropAfterExpiration: TCheckBox
        Left = 84
        Top = 57
        Width = 571
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Drop event after expiration'
        TabOrder = 2
        OnClick = Modification
      end
      object editComment: TEdit
        Left = 84
        Top = 30
        Width = 571
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'editComment'
        OnChange = Modification
      end
      object grpState: TRadioGroup
        Left = 84
        Top = 80
        Width = 353
        Height = 52
        Caption = 'State'
        Columns = 3
        TabOrder = 3
        OnClick = Modification
      end
      object comboDefiner: TComboBox
        Left = 489
        Top = 3
        Width = 166
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = 'comboDefiner'
        OnChange = Modification
        OnDropDown = comboDefinerDropDown
      end
    end
    object tabScheduling: TTabSheet
      Caption = 'Timing'
      ImageIndex = 80
      object radioOnce: TRadioButton
        Left = 3
        Top = 15
        Width = 74
        Height = 17
        Caption = 'Once, at:'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = radioScheduleClick
      end
      object radioEvery: TRadioButton
        Left = 3
        Top = 50
        Width = 54
        Height = 17
        Caption = 'Every'
        TabOrder = 3
        OnClick = radioScheduleClick
      end
      object dateOnce: TDateTimePicker
        Left = 96
        Top = 11
        Width = 133
        Height = 21
        Date = 40273.547337048610000000
        Time = 40273.547337048610000000
        TabOrder = 1
        OnChange = Modification
      end
      object timeOnce: TDateTimePicker
        Left = 235
        Top = 11
        Width = 133
        Height = 21
        Date = 40273.548026377310000000
        Time = 40273.548026377310000000
        Kind = dtkTime
        TabOrder = 2
        OnChange = Modification
      end
      object editEveryQuantity: TEdit
        Left = 96
        Top = 48
        Width = 53
        Height = 21
        TabOrder = 4
        Text = '1'
        OnChange = Modification
      end
      object udEveryQuantity: TUpDown
        Left = 149
        Top = 48
        Width = 16
        Height = 21
        Associate = editEveryQuantity
        Min = 1
        Max = 400000
        Position = 1
        TabOrder = 5
      end
      object comboEveryInterval: TComboBox
        Left = 172
        Top = 48
        Width = 133
        Height = 21
        Style = csDropDownList
        TabOrder = 6
        OnChange = comboEveryIntervalChange
      end
      object chkStarts: TCheckBox
        Left = 96
        Top = 78
        Width = 70
        Height = 17
        Caption = 'Starts at:'
        TabOrder = 7
        OnClick = chkStartsEndsClick
      end
      object chkEnds: TCheckBox
        Left = 96
        Top = 105
        Width = 70
        Height = 17
        Caption = 'Ends at:'
        TabOrder = 10
        OnClick = chkStartsEndsClick
      end
      object dateStarts: TDateTimePicker
        Left = 171
        Top = 75
        Width = 133
        Height = 21
        Date = 40273.548478379630000000
        Time = 40273.548478379630000000
        TabOrder = 8
        OnChange = Modification
      end
      object timeStarts: TDateTimePicker
        Left = 310
        Top = 75
        Width = 133
        Height = 21
        Date = 40273.549206851850000000
        Time = 40273.549206851850000000
        Kind = dtkTime
        TabOrder = 9
        OnChange = Modification
      end
      object timeEnds: TDateTimePicker
        Left = 310
        Top = 102
        Width = 133
        Height = 21
        Date = 40273.549548981480000000
        Time = 40273.549548981480000000
        Kind = dtkTime
        TabOrder = 12
        OnChange = Modification
      end
      object dateEnds: TDateTimePicker
        Left = 171
        Top = 102
        Width = 133
        Height = 21
        Date = 40273.549452245370000000
        Time = 40273.549452245370000000
        TabOrder = 11
        OnChange = Modification
      end
    end
    object tabCREATEcode: TTabSheet
      Caption = 'CREATE code'
      ImageIndex = 119
      object SynMemoCREATEcode: TSynMemo
        Left = 0
        Top = 0
        Width = 658
        Height = 137
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        Highlighter = MainForm.SynSQLSyn1
        Lines.Strings = (
          'SynMemoCREATEcode')
        Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
      end
    end
    object tabALTERcode: TTabSheet
      Caption = 'ALTER code'
      ImageIndex = 119
      TabVisible = False
      object SynMemoALTERcode: TSynMemo
        Left = 0
        Top = 0
        Width = 658
        Height = 137
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        Highlighter = MainForm.SynSQLSyn1
        Lines.Strings = (
          'SynMemoALTERcode')
        Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
      end
    end
  end
end
