object frmSyncDB: TfrmSyncDB
  Left = 0
  Top = 0
  Caption = 'frmSyncDB'
  ClientHeight = 362
  ClientWidth = 534
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    534
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSource: TLabel
    Left = 8
    Top = 8
    Width = 169
    Height = 13
    Caption = 'Select source database or table(s):'
  end
  object lblDifferences: TLabel
    Left = 207
    Top = 191
    Width = 59
    Height = 13
    Caption = 'Differences:'
  end
  object treeSource: TVirtualStringTree
    Left = 8
    Top = 24
    Width = 193
    Height = 299
    AccessibleName = 'tree'
    Anchors = [akLeft, akTop, akBottom]
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Images = MainForm.VirtualImageListMain
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseExplorerTheme, toHideTreeLinesIfThemed]
    OnChange = treeSourceChange
    OnChecked = treeSourceChecked
    OnChecking = treeSourceChecking
    OnGetText = treeSourceGetText
    OnPaintText = treeSourcePaintText
    OnGetImageIndex = treeSourceGetImageIndex
    OnGetNodeDataSize = treeSourceGetNodeDataSize
    OnInitChildren = treeSourceInitChildren
    OnInitNode = treeSourceInitNode
    Columns = <
      item
        Position = 0
        Text = 'Name'
        Width = 193
      end>
  end
  object grpTarget: TGroupBox
    Left = 207
    Top = 8
    Width = 319
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Target database or table'
    TabOrder = 1
    DesignSize = (
      319
      105)
    object lblTargetServer: TLabel
      Left = 9
      Top = 21
      Width = 36
      Height = 13
      Caption = 'Server:'
    end
    object lblTargetDatabase: TLabel
      Left = 9
      Top = 48
      Width = 50
      Height = 13
      Caption = 'Database:'
    end
    object lblTargetTable: TLabel
      Left = 9
      Top = 75
      Width = 30
      Height = 13
      Caption = 'Table:'
    end
    object comboTargetServer: TComboBox
      Left = 88
      Top = 18
      Width = 223
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboTargetServerChange
    end
    object comboTargetDatabase: TComboBox
      Left = 88
      Top = 45
      Width = 223
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = comboTargetDatabaseChange
    end
    object comboTargetTable: TComboBox
      Left = 88
      Top = 72
      Width = 223
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
  object btnClose: TButton
    Left = 451
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ImageIndex = 26
    Images = MainForm.VirtualImageListMain
    ModalResult = 2
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 370
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    ImageIndex = 120
    Images = MainForm.VirtualImageListMain
    TabOrder = 3
    OnClick = btnApplyClick
  end
  object btnAnalyze: TButton
    Left = 289
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Analyze'
    ImageIndex = 146
    Images = MainForm.VirtualImageListMain
    TabOrder = 4
    OnClick = btnAnalyzeClick
  end
  object grpOptions: TGroupBox
    Left = 207
    Top = 119
    Width = 319
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options'
    TabOrder = 5
    object radioOptionsStructure: TCheckBox
      Left = 88
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Structure'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object radioOptionsData: TCheckBox
      Left = 88
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Data'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object treeDifferences: TVirtualStringTree
    Left = 207
    Top = 208
    Width = 319
    Height = 115
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    Images = MainForm.VirtualImageListMain
    TabOrder = 6
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseExplorerTheme, toHideTreeLinesIfThemed]
    OnGetText = treeDifferencesGetText
    OnGetImageIndex = treeDifferencesGetImageIndex
    OnGetNodeDataSize = treeDifferencesGetNodeDataSize
    OnInitChildren = treeDifferencesInitChildren
    OnInitNode = treeDifferencesInitNode
    Columns = <>
  end
end
