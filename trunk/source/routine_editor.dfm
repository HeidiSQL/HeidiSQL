object frmRoutineEditor: TfrmRoutineEditor
  Left = 0
  Top = 0
  Width = 475
  Height = 484
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
    475
    484)
  object lblName: TLabel
    Left = 3
    Top = 11
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = editName
  end
  object lblType: TLabel
    Left = 3
    Top = 36
    Width = 28
    Height = 13
    Caption = '&Type:'
    FocusControl = comboType
  end
  object lblReturns: TLabel
    Left = 3
    Top = 61
    Width = 42
    Height = 13
    Caption = '&Returns:'
    FocusControl = comboReturns
  end
  object lblParameters: TLabel
    Left = 3
    Top = 187
    Width = 59
    Height = 13
    Caption = 'Parameters:'
  end
  object lblSQL: TLabel
    Left = 3
    Top = 87
    Width = 62
    Height = 13
    Caption = '&Data access:'
    FocusControl = comboDataAccess
  end
  object lblSecurity: TLabel
    Left = 3
    Top = 112
    Width = 65
    Height = 13
    Caption = 'SQL Se&curity:'
    FocusControl = comboSecurity
  end
  object lblComment: TLabel
    Left = 3
    Top = 137
    Width = 49
    Height = 13
    Caption = '&Comment:'
    FocusControl = editComment
  end
  object lblSQLcode: TLabel
    Left = 3
    Top = 317
    Width = 68
    Height = 13
    Caption = '&Routine body:'
    FocusControl = SynMemoBody
  end
  object btnSave: TButton
    Left = 165
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 12
    OnClick = PostChanges
  end
  object btnDiscard: TButton
    Left = 84
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Discard'
    ModalResult = 2
    TabOrder = 11
    OnClick = btnDiscardClick
  end
  object btnHelp: TButton
    Left = 3
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 10
    OnClick = btnHelpClick
  end
  object comboReturns: TComboBox
    Left = 95
    Top = 58
    Width = 377
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    Text = 'comboReturns'
    OnChange = Modification
  end
  object comboType: TTntComboBox
    Left = 95
    Top = 33
    Width = 377
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnSelect = comboTypeSelect
  end
  object editName: TTntEdit
    Left = 95
    Top = 8
    Width = 377
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'editName'
    OnChange = editNameChange
  end
  object tlbParameters: TToolBar
    Left = 3
    Top = 206
    Width = 72
    Height = 84
    Align = alNone
    ButtonWidth = 66
    Caption = 'tlbParameters'
    Images = MainForm.PngImageListMain
    List = True
    ShowCaptions = True
    TabOrder = 7
    object btnAddParam: TToolButton
      Left = 0
      Top = 0
      Caption = 'Add'
      ImageIndex = 45
      Wrap = True
      OnClick = btnAddParamClick
    end
    object btnRemoveParam: TToolButton
      Left = 0
      Top = 22
      Caption = 'Remove'
      ImageIndex = 46
      Wrap = True
      OnClick = btnRemoveParamClick
    end
    object btnClearParams: TToolButton
      Left = 0
      Top = 44
      Caption = 'Clear'
      ImageIndex = 26
      OnClick = btnClearParamsClick
    end
  end
  object listParameters: TVirtualStringTree
    Left = 95
    Top = 206
    Width = 377
    Height = 100
    Anchors = [akLeft, akTop, akRight]
    DragImageKind = diMainColumnOnly
    DragMode = dmAutomatic
    DragType = dtVCL
    EditDelay = 0
    Header.AutoSizeIndex = 1
    Header.DefaultHeight = 17
    Header.MainColumn = 1
    Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoVisible]
    Header.ParentFont = True
    Images = MainForm.PngImageListMain
    NodeDataSize = 0
    TabOrder = 8
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
    OnBeforePaint = listParametersBeforePaint
    OnCreateEditor = listParametersCreateEditor
    OnEditing = listParametersEditing
    OnFocusChanged = listParametersFocusChanged
    OnGetText = listParametersGetText
    OnPaintText = listParametersPaintText
    OnGetImageIndex = listParametersGetImageIndex
    OnNewText = listParametersNewText
    Columns = <
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible, coAllowFocus]
        Position = 0
        Width = 25
        WideText = '#'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 1
        Width = 188
        WideText = 'Name'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 2
        Width = 90
        WideText = 'Datatype'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
        Position = 3
        Width = 70
        WideText = 'Context'
      end>
  end
  object comboDataAccess: TComboBox
    Left = 95
    Top = 84
    Width = 377
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
    OnChange = Modification
  end
  object comboSecurity: TComboBox
    Left = 95
    Top = 109
    Width = 377
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
    OnChange = Modification
  end
  object editComment: TTntEdit
    Left = 95
    Top = 134
    Width = 377
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = 'editComment'
    OnChange = Modification
  end
  object chkDeterministic: TCheckBox
    Left = 95
    Top = 161
    Width = 377
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Deterministic'
    TabOrder = 6
    OnClick = Modification
  end
  object SynMemoBody: TSynMemo
    Left = 3
    Top = 336
    Width = 469
    Height = 113
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 9
    OnDragDrop = SynMemoBodyDragDrop
    OnDragOver = SynMemoBodyDragOver
    Gutter.DigitCount = 3
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = MainForm.SynSQLSyn1
    Lines.UnicodeStrings = 'SynMemoBody'
    RightEdge = 0
    TabWidth = 3
    WantTabs = True
    OnChange = Modification
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
end
