object frmRoutineEditor: TfrmRoutineEditor
  Left = 0
  Top = 0
  Width = 606
  Height = 484
  TabOrder = 0
  DesignSize = (
    606
    484)
  object lblSQLcode: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 175
    Width = 600
    Height = 13
    Align = alTop
    Caption = 'Routine body:'
    FocusControl = SynMemoBody
  end
  object lblDisabledWhy: TLabel
    Left = 256
    Top = 460
    Width = 177
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'You have no privilege to this routine.'
    Visible = False
  end
  object btnSave: TButton
    Left = 165
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Default = True
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnDiscard: TButton
    Left = 84
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Discard'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnDiscardClick
  end
  object btnHelp: TButton
    Left = 3
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object SynMemoBody: TSynMemo
    AlignWithMargins = True
    Left = 3
    Top = 194
    Width = 600
    Height = 250
    Margins.Bottom = 40
    SingleLineMode = False
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    OnDragDrop = SynMemoBodyDragDrop
    OnDragOver = SynMemoBodyDragOver
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
  object PageControlMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 600
    Height = 166
    ActivePage = tabOptions
    Align = alTop
    Images = MainForm.ImageListMain
    TabOrder = 0
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 39
      DesignSize = (
        592
        137)
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
        Top = 65
        Width = 28
        Height = 13
        Caption = '&Type:'
        FocusControl = comboType
      end
      object lblReturns: TLabel
        Left = 3
        Top = 90
        Width = 42
        Height = 13
        Caption = '&Returns:'
        FocusControl = comboReturns
      end
      object lblSQL: TLabel
        Left = 408
        Top = 65
        Width = 62
        Height = 13
        Caption = '&Data access:'
        FocusControl = comboDataAccess
      end
      object lblSecurity: TLabel
        Left = 408
        Top = 90
        Width = 65
        Height = 13
        Caption = 'SQL Se&curity:'
        FocusControl = comboSecurity
      end
      object lblComment: TLabel
        Left = 3
        Top = 38
        Width = 49
        Height = 13
        Caption = '&Comment:'
        FocusControl = editComment
      end
      object lblDefiner: TLabel
        Left = 408
        Top = 11
        Width = 39
        Height = 13
        Caption = 'De&finer:'
      end
      object chkDeterministic: TCheckBox
        Left = 84
        Top = 114
        Width = 508
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Deterministic'
        TabOrder = 7
        OnClick = Modification
      end
      object editComment: TEdit
        Left = 84
        Top = 35
        Width = 505
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'editComment'
        OnChange = Modification
      end
      object comboSecurity: TComboBox
        Left = 489
        Top = 87
        Width = 100
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        OnChange = Modification
      end
      object comboDataAccess: TComboBox
        Left = 489
        Top = 62
        Width = 100
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        OnChange = Modification
      end
      object comboReturns: TComboBox
        Left = 84
        Top = 87
        Width = 310
        Height = 21
        TabOrder = 4
        Text = 'comboReturns'
        OnChange = Modification
      end
      object comboType: TComboBox
        Left = 84
        Top = 62
        Width = 310
        Height = 21
        Style = csDropDownList
        TabOrder = 3
        OnSelect = comboTypeSelect
      end
      object editName: TEdit
        Left = 84
        Top = 8
        Width = 310
        Height = 21
        TabOrder = 0
        Text = 'editName'
        TextHint = 'Enter routine name'
        OnChange = Modification
      end
      object comboDefiner: TComboBox
        Left = 489
        Top = 8
        Width = 100
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'comboDefiner'
        OnChange = Modification
        OnDropDown = comboDefinerDropDown
      end
    end
    object tabParameters: TTabSheet
      Caption = 'Parameters'
      ImageIndex = 122
      object listParameters: TVirtualStringTree
        Left = 82
        Top = 0
        Width = 510
        Height = 137
        Align = alClient
        DragImageKind = diMainColumnOnly
        DragMode = dmAutomatic
        DragType = dtVCL
        EditDelay = 0
        Header.AutoSizeIndex = 1
        Header.MainColumn = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoVisible]
        Header.ParentFont = True
        Images = MainForm.ImageListMain
        NodeDataSize = 0
        TabOrder = 0
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
            Width = 321
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
      object tlbParameters: TToolBar
        Left = 0
        Top = 0
        Width = 82
        Height = 137
        Align = alLeft
        AutoSize = True
        ButtonWidth = 82
        Caption = 'tlbParameters'
        Images = MainForm.ImageListMain
        List = True
        ShowCaptions = True
        TabOrder = 1
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
          Enabled = False
          ImageIndex = 46
          Wrap = True
          OnClick = btnRemoveParamClick
        end
        object btnClearParams: TToolButton
          Left = 0
          Top = 44
          Caption = 'Clear'
          ImageIndex = 26
          Wrap = True
          OnClick = btnClearParamsClick
        end
        object btnMoveUpParam: TToolButton
          Left = 0
          Top = 66
          Caption = 'Move up'
          Enabled = False
          ImageIndex = 74
          Wrap = True
          OnClick = btnMoveParamClick
        end
        object btnMoveDownParam: TToolButton
          Left = 0
          Top = 88
          Caption = 'Move down'
          Enabled = False
          ImageIndex = 75
          OnClick = btnMoveParamClick
        end
      end
    end
    object tabCreateCode: TTabSheet
      Caption = 'CREATE code'
      ImageIndex = 119
      object SynMemoCREATEcode: TSynMemo
        Left = 0
        Top = 0
        Width = 592
        Height = 137
        SingleLineMode = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Highlighter = MainForm.SynSQLSyn1
        Lines.Strings = (
          'SynMemoCREATEcode')
        ReadOnly = True
      end
    end
  end
  object btnRunProc: TButton
    Left = 480
    Top = 455
    Width = 123
    Height = 25
    Action = MainForm.actRunRoutines
    Anchors = [akRight, akBottom]
    Images = MainForm.ImageListMain
    TabOrder = 5
  end
end
