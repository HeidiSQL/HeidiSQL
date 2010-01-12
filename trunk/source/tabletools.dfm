object frmTableTools: TfrmTableTools
  Left = 734
  Top = 126
  Caption = 'Table tools'
  ClientHeight = 383
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    544
    383)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSkipLargeTablesMB: TLabel
    Left = 192
    Top = 355
    Width = 87
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'MB (0 = unlimited)'
  end
  object lblSkipLargeTables: TLabel
    Left = 8
    Top = 355
    Width = 107
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Skip tables larger than'
  end
  object btnClose: TButton
    Left = 461
    Top = 350
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    TabOrder = 0
  end
  object pnlTop: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 528
    Height = 336
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    object spltHorizontally: TSplitter
      Left = 144
      Top = 0
      Width = 4
      Height = 336
      Cursor = crSizeWE
      ResizeStyle = rsUpdate
    end
    object TreeObjects: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 144
      Height = 336
      Align = alLeft
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
      Images = MainForm.PngImageListMain
      NodeDataSize = 0
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
      OnChange = TreeObjectsChange
      OnChecked = TreeObjectsChecked
      OnChecking = TreeObjectsChecking
      OnGetText = TreeObjectsGetText
      OnPaintText = TreeObjectsPaintText
      OnGetImageIndex = TreeObjectsGetImageIndex
      OnInitChildren = TreeObjectsInitChildren
      OnInitNode = TreeObjectsInitNode
      Columns = <
        item
          Position = 0
          Width = 90
          WideText = 'Dummy, keeps compatibility to mainform.dbtree'
        end
        item
          Alignment = taRightJustify
          Position = 1
          WideText = 'Size'
        end>
    end
    object pnlRight: TPanel
      Left = 148
      Top = 0
      Width = 380
      Height = 336
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object ResultGrid: TVirtualStringTree
        Left = 0
        Top = 145
        Width = 380
        Height = 191
        Align = alClient
        Header.AutoSizeIndex = -1
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Images = MainForm.PngImageListMain
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnCompareNodes = ResultGridCompareNodes
        OnGetText = ResultGridGetText
        OnPaintText = ResultGridPaintText
        OnGetNodeDataSize = ResultGridGetNodeDataSize
        OnHeaderClick = ResultGridHeaderClick
        OnInitNode = ResultGridInitNode
        Columns = <>
      end
      object tabsTools: TPageControl
        Left = 0
        Top = 0
        Width = 380
        Height = 145
        ActivePage = tabSQLexport
        Align = alTop
        Images = MainForm.PngImageListMain
        TabOrder = 1
        OnChange = ValidateControls
        object tabMaintenance: TTabSheet
          Caption = 'Maintenance'
          ImageIndex = 39
          DesignSize = (
            372
            116)
          object lblOperation: TLabel
            Left = 3
            Top = 14
            Width = 52
            Height = 13
            Caption = 'Operation:'
          end
          object lblOptions: TLabel
            Left = 3
            Top = 39
            Width = 41
            Height = 13
            Caption = 'Options:'
          end
          object comboOperation: TComboBox
            Left = 80
            Top = 11
            Width = 289
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 0
            OnChange = ValidateControls
          end
          object chkQuick: TCheckBox
            Left = 81
            Top = 38
            Width = 97
            Height = 17
            Caption = 'Quick'
            TabOrder = 1
            OnClick = ValidateControls
          end
          object chkFast: TCheckBox
            Left = 81
            Top = 57
            Width = 97
            Height = 17
            Caption = 'Fast'
            TabOrder = 2
            OnClick = ValidateControls
          end
          object chkMedium: TCheckBox
            Left = 81
            Top = 76
            Width = 97
            Height = 17
            Caption = 'Medium'
            TabOrder = 3
            OnClick = ValidateControls
          end
          object chkExtended: TCheckBox
            Left = 184
            Top = 38
            Width = 97
            Height = 17
            Caption = 'Extended'
            TabOrder = 4
            OnClick = ValidateControls
          end
          object chkChanged: TCheckBox
            Left = 184
            Top = 57
            Width = 97
            Height = 17
            Caption = 'Changed'
            TabOrder = 5
            OnClick = ValidateControls
          end
          object chkUseFrm: TCheckBox
            Left = 184
            Top = 76
            Width = 97
            Height = 17
            Caption = 'Use FRM file'
            TabOrder = 6
            OnClick = ValidateControls
          end
          object btnHelp: TButton
            Left = 294
            Top = 38
            Width = 75
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Help'
            TabOrder = 7
            OnClick = btnHelpClick
          end
        end
        object tabFind: TTabSheet
          Caption = 'Find text'
          ImageIndex = 30
          DesignSize = (
            372
            116)
          object lblFindText: TLabel
            Left = 3
            Top = 14
            Width = 60
            Height = 13
            Caption = 'Text to find:'
          end
          object lblDataTypes: TLabel
            Left = 80
            Top = 93
            Width = 151
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Restrict search to column types'
          end
          object memoFindText: TMemo
            Left = 80
            Top = 11
            Width = 289
            Height = 73
            Anchors = [akLeft, akTop, akRight, akBottom]
            ScrollBars = ssVertical
            TabOrder = 0
            OnChange = ValidateControls
          end
          object comboDataTypes: TComboBox
            Left = 264
            Top = 90
            Width = 105
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 1
          end
        end
        object tabSQLexport: TTabSheet
          Caption = 'SQL export'
          ImageIndex = 9
          DesignSize = (
            372
            116)
          object lblExportData: TLabel
            Left = 224
            Top = 4
            Width = 27
            Height = 13
            Caption = 'Data:'
          end
          object lblExportOutputType: TLabel
            Left = 3
            Top = 51
            Width = 38
            Height = 13
            Caption = 'Output:'
          end
          object lblExportDatabases: TLabel
            Left = 3
            Top = 4
            Width = 63
            Height = 13
            Caption = 'Database(s):'
          end
          object lblExportTables: TLabel
            Left = 3
            Top = 25
            Width = 43
            Height = 13
            Caption = 'Table(s):'
          end
          object lblExportOutputTarget: TLabel
            Left = 2
            Top = 77
            Width = 46
            Height = 13
            Caption = 'Filename:'
          end
          object btnExportOutputTargetSelect: TPngSpeedButton
            Left = 346
            Top = 74
            Width = 23
            Height = 21
            Hint = 'Browse filesystem'
            Anchors = [akTop, akRight]
            Flat = True
            OnClick = btnExportOutputTargetSelectClick
          end
          object chkExportDatabasesCreate: TCheckBox
            Left = 144
            Top = 3
            Width = 73
            Height = 17
            Caption = 'Create'
            TabOrder = 0
            OnClick = chkExportOptionClick
          end
          object chkExportDatabasesDrop: TCheckBox
            Left = 80
            Top = 3
            Width = 60
            Height = 17
            Caption = 'Drop'
            TabOrder = 1
            OnClick = chkExportOptionClick
          end
          object chkExportTablesDrop: TCheckBox
            Left = 80
            Top = 24
            Width = 60
            Height = 17
            Caption = 'Drop'
            TabOrder = 2
            OnClick = chkExportOptionClick
          end
          object chkExportTablesCreate: TCheckBox
            Left = 144
            Top = 24
            Width = 74
            Height = 17
            Caption = 'Create'
            TabOrder = 3
            OnClick = chkExportOptionClick
          end
          object comboExportData: TComboBox
            Left = 224
            Top = 22
            Width = 145
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 4
          end
          object comboExportOutputType: TComboBox
            Left = 80
            Top = 48
            Width = 289
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 5
            OnChange = comboExportOutputTypeChange
          end
          object comboExportOutputTarget: TComboBox
            Left = 80
            Top = 74
            Width = 263
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 6
            Text = 'comboExportOutputTarget'
            OnExit = comboExportOutputTargetExit
          end
        end
        object tabBulkTableEdit: TTabSheet
          Caption = 'Bulk table editor'
          ImageIndex = 19
          DesignSize = (
            372
            116)
          object chkBulkTableEditDatabase: TCheckBox
            Left = 3
            Top = 5
            Width = 135
            Height = 17
            Caption = 'Move to database:'
            TabOrder = 0
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditDatabase: TComboBox
            Left = 168
            Top = 3
            Width = 200
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            Enabled = False
            ItemHeight = 13
            TabOrder = 1
          end
          object chkBulkTableEditResetAutoinc: TCheckBox
            Left = 3
            Top = 97
            Width = 166
            Height = 17
            Caption = 'Reset auto increment value'
            TabOrder = 2
          end
          object chkBulkTableEditCollation: TCheckBox
            Left = 3
            Top = 51
            Width = 150
            Height = 17
            Caption = 'Change default collation:'
            TabOrder = 3
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditCollation: TComboBox
            Left = 168
            Top = 49
            Width = 200
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            Enabled = False
            ItemHeight = 13
            TabOrder = 4
          end
          object chkBulkTableEditEngine: TCheckBox
            Left = 3
            Top = 28
            Width = 150
            Height = 17
            Caption = 'Change table engine:'
            TabOrder = 5
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditEngine: TComboBox
            Left = 168
            Top = 26
            Width = 200
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            Enabled = False
            ItemHeight = 13
            TabOrder = 6
          end
          object chkBulkTableEditCharset: TCheckBox
            Left = 3
            Top = 74
            Width = 150
            Height = 17
            Caption = 'Convert to charset:'
            TabOrder = 7
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditCharset: TComboBox
            Left = 168
            Top = 72
            Width = 200
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            Enabled = False
            ItemHeight = 13
            TabOrder = 8
          end
        end
      end
    end
  end
  object btnExecute: TButton
    Left = 380
    Top = 350
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Execute'
    TabOrder = 2
    OnClick = Execute
  end
  object editSkipLargeTables: TEdit
    Left = 130
    Top = 352
    Width = 40
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = '20'
  end
  object udSkipLargeTables: TUpDown
    Left = 170
    Top = 352
    Width = 16
    Height = 21
    Anchors = [akLeft, akBottom]
    Associate = editSkipLargeTables
    Position = 20
    TabOrder = 4
  end
end
