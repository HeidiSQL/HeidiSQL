object frmTableTools: TfrmTableTools
  Left = 734
  Top = 126
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'Table tools'
  ClientHeight = 383
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    764
    383)
  TextHeight = 14
  object lblCheckedSize: TLabel
    Left = 8
    Top = 355
    Width = 79
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'lblCheckedSize'
  end
  object btnCloseOrCancel: TButton
    Left = 661
    Top = 350
    Width = 95
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnCloseOrCancelClick
  end
  object pnlTop: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 748
    Height = 336
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object spltHorizontally: TSplitter
      Left = 185
      Top = 0
      Width = 4
      Height = 336
      Cursor = crSizeWE
      ResizeStyle = rsUpdate
      OnMoved = spltHorizontallyMoved
    end
    object pnlRight: TPanel
      Left = 189
      Top = 0
      Width = 559
      Height = 336
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object ResultGrid: TVirtualStringTree
        Left = 0
        Top = 193
        Width = 559
        Height = 143
        Align = alClient
        Header.AutoSizeIndex = -1
        Header.Height = 14
        Header.Images = MainForm.VirtualImageListMain
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible, hoDisableAnimatedResize, hoAutoResizeInclCaption]
        Header.PopupMenu = MainForm.popupListHeader
        IncrementalSearch = isAll
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnCompareNodes = ResultGridCompareNodes
        OnGetText = ResultGridGetText
        OnPaintText = ResultGridPaintText
        OnGetNodeDataSize = ResultGridGetNodeDataSize
        OnHeaderClick = ResultGridHeaderClick
        OnInitNode = ResultGridInitNode
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <>
      end
      object tabsTools: TPageControl
        Left = 0
        Top = 0
        Width = 559
        Height = 193
        ActivePage = tabSQLexport
        Align = alTop
        Images = MainForm.VirtualImageListMain
        TabOrder = 1
        OnChange = ValidateControls
        object tabMaintenance: TTabSheet
          Caption = 'Maintenance'
          ImageIndex = 39
          ImageName = 'icons8-support'
          DesignSize = (
            551
            164)
          object lblOperation: TLabel
            Left = 3
            Top = 14
            Width = 58
            Height = 14
            Caption = 'Operation:'
          end
          object lblOptions: TLabel
            Left = 3
            Top = 39
            Width = 46
            Height = 14
            Caption = 'Options:'
          end
          object comboOperation: TComboBox
            Left = 80
            Top = 11
            Width = 467
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
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
          object btnHelpMaintenance: TButton
            Left = 473
            Top = 38
            Width = 75
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Help'
            TabOrder = 7
            OnClick = btnHelpMaintenanceClick
          end
          object chkForUpgrade: TCheckBox
            Left = 81
            Top = 96
            Width = 97
            Height = 17
            Caption = 'For Upgrade'
            TabOrder = 8
            OnClick = ValidateControls
          end
        end
        object tabFind: TTabSheet
          Caption = 'Find text'
          ImageIndex = 30
          ImageName = 'icons8-find'
          DesignSize = (
            551
            164)
          object lblFindText: TLabel
            Left = 3
            Top = 27
            Width = 70
            Height = 14
            Caption = 'Text to find:'
          end
          object lblDataTypes: TLabel
            Left = 3
            Top = 90
            Width = 131
            Height = 14
            Anchors = [akLeft, akBottom]
            Caption = 'Search in column types:'
          end
          object lblMatchType: TLabel
            Left = 3
            Top = 140
            Width = 66
            Height = 14
            Anchors = [akLeft, akBottom]
            Caption = 'Match type:'
          end
          object comboDataTypes: TComboBox
            Left = 208
            Top = 87
            Width = 340
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akRight, akBottom]
            TabOrder = 0
          end
          object chkCaseSensitive: TCheckBox
            Left = 208
            Top = 114
            Width = 340
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = 'Case sensitive'
            TabOrder = 1
          end
          object comboMatchType: TComboBox
            Left = 208
            Top = 137
            Width = 340
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akRight, akBottom]
            ItemIndex = 0
            TabOrder = 2
            Text = 'Left and right wildcard'
            Items.Strings = (
              'Left and right wildcard'
              'Exact match'
              'Left wildcard'
              'Right wildcard'
              'Regular expression')
          end
          object tabsTextType: TPageControl
            Left = 208
            Top = 3
            Width = 340
            Height = 78
            ActivePage = tabSimpleText
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 3
            OnChange = ValidateControls
            object tabSimpleText: TTabSheet
              Caption = 'Simple text'
              object memoFindText: TMemo
                Left = 0
                Top = 0
                Width = 332
                Height = 49
                Align = alClient
                ScrollBars = ssVertical
                TabOrder = 0
                OnChange = ValidateControls
              end
            end
            object tabSQL: TTabSheet
              Caption = 'SQL'
              ImageIndex = 1
              object SynMemoFindText: TSynMemo
                Left = 0
                Top = 0
                Width = 332
                Height = 49
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
                Gutter.Visible = False
                Gutter.Width = 0
                Highlighter = MainForm.SynSQLSynUsed
                Lines.Strings = (
                  '> NOW()')
                Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
                FontSmoothing = fsmNone
              end
            end
          end
        end
        object tabSQLexport: TTabSheet
          Caption = 'SQL export'
          ImageIndex = 9
          ImageName = 'icons8-outgoing-data-100'
          DesignSize = (
            551
            164)
          object lblExportData: TLabel
            Left = 3
            Top = 50
            Width = 29
            Height = 14
            Caption = 'Data:'
          end
          object lblExportOutputType: TLabel
            Left = 3
            Top = 104
            Width = 44
            Height = 14
            Caption = 'Output:'
          end
          object lblExportDatabases: TLabel
            Left = 3
            Top = 4
            Width = 69
            Height = 14
            Caption = 'Database(s):'
          end
          object lblExportTables: TLabel
            Left = 3
            Top = 25
            Width = 49
            Height = 14
            Caption = 'Table(s):'
          end
          object lblExportOutputTarget: TLabel
            Left = 2
            Top = 130
            Width = 51
            Height = 14
            Caption = 'Filename:'
          end
          object lblInsertSize: TLabel
            Left = 3
            Top = 77
            Width = 93
            Height = 14
            Caption = 'Max INSERT size:'
          end
          object lblInsertSizeUnit: TLabel
            Left = 242
            Top = 77
            Width = 134
            Height = 14
            Caption = 'KB (0 = Single INSERTs)'
          end
          object btnExportOutputTargetSelect: TButton
            Left = 525
            Top = 127
            Width = 23
            Height = 21
            Hint = 'Browse filesystem'
            Anchors = [akTop, akRight]
            ImageIndex = 51
            ImageName = 'icons8-folder'
            Images = MainForm.VirtualImageListMain
            TabOrder = 9
            OnClick = btnExportOutputTargetSelectClick
          end
          object chkExportDatabasesCreate: TCheckBox
            Left = 192
            Top = 3
            Width = 90
            Height = 17
            Caption = 'Create'
            TabOrder = 0
            OnClick = chkExportOptionClick
          end
          object chkExportDatabasesDrop: TCheckBox
            Left = 100
            Top = 3
            Width = 90
            Height = 17
            Caption = 'Drop'
            TabOrder = 1
            OnClick = chkExportOptionClick
          end
          object chkExportTablesDrop: TCheckBox
            Left = 100
            Top = 24
            Width = 90
            Height = 17
            Caption = 'Drop'
            TabOrder = 2
            OnClick = chkExportOptionClick
          end
          object chkExportTablesCreate: TCheckBox
            Left = 192
            Top = 24
            Width = 90
            Height = 17
            Caption = 'Create'
            TabOrder = 3
            OnClick = chkExportOptionClick
          end
          object comboExportData: TComboBox
            Left = 100
            Top = 47
            Width = 448
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
            OnChange = ValidateControls
          end
          object comboExportOutputType: TComboBox
            Left = 100
            Top = 101
            Width = 448
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 16
            TabOrder = 7
            OnChange = comboExportOutputTypeChange
          end
          object comboExportOutputTarget: TComboBox
            Left = 100
            Top = 127
            Width = 422
            Height = 22
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 16
            ParentShowHint = False
            ShowHint = True
            TabOrder = 8
            Text = 'comboExportOutputTarget'
            OnChange = comboExportOutputTargetChange
          end
          object editInsertSize: TEdit
            Left = 100
            Top = 74
            Width = 120
            Height = 22
            TabOrder = 5
            Text = '0'
          end
          object updownInsertSize: TUpDown
            Left = 220
            Top = 74
            Width = 16
            Height = 22
            Associate = editInsertSize
            Max = 2147483647
            TabOrder = 6
            Wrap = True
          end
          object btnExportOptions: TButton
            Left = 423
            Top = 72
            Width = 125
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Options'
            DropDownMenu = popupExportOptions
            Style = bsSplitButton
            TabOrder = 10
            OnClick = btnExportOptionsClick
          end
        end
        object tabBulkTableEdit: TTabSheet
          Caption = 'Bulk table editor'
          ImageIndex = 19
          ImageName = 'icons8-sheets-100'
          DesignSize = (
            551
            164)
          object chkBulkTableEditDatabase: TCheckBox
            Left = 3
            Top = 5
            Width = 199
            Height = 17
            Caption = 'Move to database:'
            TabOrder = 0
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditDatabase: TComboBox
            Left = 208
            Top = 3
            Width = 339
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            Enabled = False
            TabOrder = 1
          end
          object chkBulkTableEditResetAutoinc: TCheckBox
            Left = 3
            Top = 97
            Width = 366
            Height = 17
            Caption = 'Reset auto increment value'
            TabOrder = 2
            OnClick = ValidateControls
          end
          object chkBulkTableEditCollation: TCheckBox
            Left = 3
            Top = 51
            Width = 199
            Height = 17
            Caption = 'Change default collation:'
            TabOrder = 3
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditCollation: TComboBox
            Left = 208
            Top = 49
            Width = 339
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 16
            Enabled = False
            Sorted = True
            TabOrder = 4
          end
          object chkBulkTableEditEngine: TCheckBox
            Left = 3
            Top = 28
            Width = 199
            Height = 17
            Caption = 'Change table engine:'
            TabOrder = 5
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditEngine: TComboBox
            Left = 208
            Top = 26
            Width = 339
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            Enabled = False
            TabOrder = 6
          end
          object chkBulkTableEditCharset: TCheckBox
            Left = 3
            Top = 74
            Width = 199
            Height = 17
            Caption = 'Convert to charset:'
            TabOrder = 7
            OnClick = chkBulkTableEditCheckComboClick
          end
          object comboBulkTableEditCharset: TComboBox
            Left = 208
            Top = 72
            Width = 339
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 16
            Enabled = False
            TabOrder = 8
          end
        end
      end
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 336
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'pnlLeft'
      ShowCaption = False
      TabOrder = 1
      object pnlLeftTop: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 29
        Align = alTop
        BevelOuter = bvNone
        Caption = 'pnlLeftTop'
        ShowCaption = False
        TabOrder = 0
        object editDatabaseFilter: TButtonedEdit
          Left = 6
          Top = 1
          Width = 49
          Height = 22
          Hint = 
            'Database filter|A list of databases, separated by semicolon. Can' +
            ' contain regular expressions, e.g. "mydb;test.*;project\d+".'
          Images = MainForm.VirtualImageListMain
          LeftButton.ImageIndex = 53
          LeftButton.Visible = True
          RightButton.ImageIndex = 193
          TabOrder = 0
          Text = 'editDatabaseFilter'
          TextHint = 'Database filter'
          OnChange = editDatabaseTableFilterChange
          OnKeyPress = editDatabaseTableFilterKeyPress
          OnRightButtonClick = editDatabaseTableFilterRightButtonClick
        end
        object editTableFilter: TButtonedEdit
          Left = 61
          Top = 1
          Width = 68
          Height = 22
          Hint = 'Table filter|Can contain regular expressions, e.g. "phpbb_\d"'
          Images = MainForm.VirtualImageListMain
          LeftButton.ImageIndex = 53
          LeftButton.Visible = True
          RightButton.ImageIndex = 193
          TabOrder = 1
          Text = 'editTableFilter'
          TextHint = 'Table filter'
          OnChange = editDatabaseTableFilterChange
          OnKeyPress = editDatabaseTableFilterKeyPress
          OnRightButtonClick = editDatabaseTableFilterRightButtonClick
        end
      end
      object TreeObjects: TVirtualStringTree
        Left = 0
        Top = 29
        Width = 185
        Height = 307
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.Height = 18
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
        Images = MainForm.VirtualImageListMain
        IncrementalSearch = isInitializedOnly
        PopupMenu = popupTree
        TabOrder = 1
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseExplorerTheme, toHideTreeLinesIfThemed]
        TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
        OnBeforeCellPaint = TreeObjectsBeforeCellPaint
        OnChange = TreeObjectsChange
        OnChecked = TreeObjectsChecked
        OnChecking = TreeObjectsChecking
        OnExpanded = TreeObjectsExpanded
        OnGetText = TreeObjectsGetText
        OnPaintText = TreeObjectsPaintText
        OnGetImageIndex = TreeObjectsGetImageIndex
        OnGetNodeDataSize = TreeObjectsGetNodeDataSize
        OnInitChildren = TreeObjectsInitChildren
        OnInitNode = TreeObjectsInitNode
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Position = 0
            Text = 'Dummy, keeps compatibility to mainform.dbtree'
            Width = 131
          end
          item
            Alignment = taRightJustify
            Position = 1
            Text = 'Size'
          end>
      end
    end
  end
  object btnExecute: TButton
    Left = 560
    Top = 350
    Width = 95
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Execute'
    TabOrder = 2
    OnClick = Execute
  end
  object btnSeeResults: TButton
    Left = 436
    Top = 350
    Width = 118
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'See results'
    ModalResult = 1
    TabOrder = 1
    Visible = False
    OnClick = btnSeeResultsClick
  end
  object popupTree: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 144
    Top = 352
    object menuCheckNone: TMenuItem
      Caption = 'Check none'
      OnClick = CheckAllClick
    end
    object menuCheckAll: TMenuItem
      Caption = 'Check all'
      OnClick = CheckAllClick
    end
    object menuCheckByType: TMenuItem
      Caption = 'Check ...'
    end
  end
  object popupExportOptions: TPopupMenu
    Left = 176
    Top = 352
    object menuExportAddComments: TMenuItem
      AutoCheck = True
      Caption = 'Add comments'
    end
    object menuExportRemoveAutoIncrement: TMenuItem
      AutoCheck = True
      Caption = 'Remove AUTO_INCREMENT clauses'
    end
    object menuExportRemoveDefiner: TMenuItem
      AutoCheck = True
      Caption = 'Remove DEFINER clauses'
    end
    object menuCopyMysqldumpCommand: TMenuItem
      Caption = 'Copy mysqldump command'
      OnClick = menuCopyMysqldumpCommandClick
    end
  end
  object timerCalcSize: TTimer
    Enabled = False
    Interval = 200
    OnTimer = timerCalcSizeTimer
    Left = 264
    Top = 352
  end
end
