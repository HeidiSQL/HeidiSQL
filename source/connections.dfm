object connform: Tconnform
  Left = 288
  Top = 129
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'Session manager'
  ClientHeight = 448
  ClientWidth = 649
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    649
    448)
  PixelsPerInch = 96
  TextHeight = 13
  object splitterMain: TSplitter
    AlignWithMargins = True
    Left = 208
    Top = 8
    Width = 8
    Height = 400
    Cursor = crSizeWE
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 40
    ResizeStyle = rsUpdate
    OnMoved = splitterMainMoved
  end
  object btnSave: TButton
    Left = 64
    Top = 415
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object btnOpen: TButton
    Left = 389
    Top = 415
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open'
    Default = True
    Enabled = False
    TabOrder = 4
    OnClick = btnOpenClick
  end
  object btnCancel: TButton
    Left = 475
    Top = 415
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object ListSessions: TVirtualStringTree
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 200
    Height = 400
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 40
    Align = alLeft
    Constraints.MinWidth = 200
    DragMode = dmAutomatic
    Header.AutoSizeIndex = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
    Header.PopupMenu = MainForm.popupListHeader
    Header.SortColumn = 0
    HintMode = hmTooltip
    Images = MainForm.VirtualImageListMain
    IncrementalSearch = isAll
    PopupMenu = popupSessions
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag]
    TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnCreateEditor = ListSessionsCreateEditor
    OnDblClick = btnOpenClick
    OnDragOver = ListSessionsDragOver
    OnDragDrop = ListSessionsDragDrop
    OnFocusChanged = ListSessionsFocusChanged
    OnFocusChanging = ListSessionsFocusChanging
    OnGetText = ListSessionsGetText
    OnGetImageIndex = ListSessionsGetImageIndex
    OnGetNodeDataSize = ListSessionsGetNodeDataSize
    OnNewText = ListSessionsNewText
    OnStructureChange = ListSessionsStructureChange
    Columns = <
      item
        Position = 0
        Text = 'Session name'
        Width = 163
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 1
        Text = 'Host'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 2
        Text = 'User'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 3
        Text = 'Version'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 4
        Text = 'Last connect'
      end
      item
        Alignment = taRightJustify
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 5
        Text = 'Counter'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 6
        Text = 'Comment'
        Width = 33
      end>
  end
  object btnNew: TButton
    Left = 8
    Top = 415
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'New'
    DropDownMenu = popupNew
    Style = bsSplitButton
    TabOrder = 1
    OnClick = btnNewClick
  end
  object btnDelete: TButton
    Left = 120
    Top = 415
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object PageControlDetails: TPageControl
    AlignWithMargins = True
    Left = 216
    Top = 8
    Width = 425
    Height = 400
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 40
    ActivePage = tabStart
    Align = alClient
    Images = MainForm.VirtualImageListMain
    TabOrder = 7
    object tabStart: TTabSheet
      Caption = 'Start'
      ImageIndex = 112
      object lblHelp: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 10
        Width = 397
        Height = 161
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alTop
        AutoSize = False
        Caption = 'lblHelp'
        WordWrap = True
      end
      object btnImportSettings: TButton
        Left = 10
        Top = 184
        Width = 159
        Height = 25
        Caption = 'Import settings ...'
        ImageIndex = 101
        TabOrder = 0
        OnClick = btnImportSettingsClick
      end
    end
    object tabSettings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 39
      DesignSize = (
        417
        371)
      object lblPort: TLabel
        Left = 3
        Top = 199
        Width = 24
        Height = 13
        Caption = 'Port:'
        FocusControl = editPort
      end
      object lblPassword: TLabel
        Left = 3
        Top = 174
        Width = 50
        Height = 13
        Caption = 'Password:'
        FocusControl = editPassword
      end
      object lblHost: TLabel
        Left = 3
        Top = 76
        Width = 72
        Height = 13
        Caption = 'Hostname / IP:'
        FocusControl = editHost
      end
      object lblUsername: TLabel
        Left = 3
        Top = 149
        Width = 26
        Height = 13
        Caption = 'User:'
        FocusControl = editUsername
      end
      object lblNetworkType: TLabel
        Left = 3
        Top = 12
        Width = 69
        Height = 13
        Caption = 'Network type:'
      end
      object lblDatabase: TLabel
        Left = 3
        Top = 247
        Width = 55
        Height = 13
        Caption = 'Databases:'
      end
      object lblComment: TLabel
        Left = 3
        Top = 274
        Width = 49
        Height = 13
        Caption = 'Comment:'
      end
      object lblLibrary: TLabel
        Left = 3
        Top = 39
        Width = 37
        Height = 13
        Caption = 'Library:'
      end
      object chkCompressed: TCheckBox
        Left = 120
        Top = 221
        Width = 294
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Compressed client/server protocol'
        TabOrder = 9
        OnClick = Modification
      end
      object editPort: TEdit
        Left = 120
        Top = 196
        Width = 57
        Height = 21
        TabOrder = 7
        Text = '0'
        OnChange = Modification
      end
      object updownPort: TUpDown
        Left = 177
        Top = 196
        Width = 16
        Height = 21
        Associate = editPort
        Max = 2147483647
        TabOrder = 8
        Thousands = False
        Wrap = True
      end
      object editPassword: TEdit
        Left = 120
        Top = 171
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 6
        OnChange = Modification
      end
      object editUsername: TEdit
        Left = 120
        Top = 146
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        OnChange = Modification
      end
      object editHost: TEdit
        Left = 120
        Top = 73
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = editHostChange
      end
      object comboNetType: TComboBox
        Left = 120
        Top = 9
        Width = 294
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 12
        ItemIndex = 0
        TabOrder = 0
        Text = 'TCP/IP'
        OnChange = comboNetTypeChange
        Items.Strings = (
          'TCP/IP'
          'Named pipe'
          'SSH tunnel'
          'MSSQL')
      end
      object chkLoginPrompt: TCheckBox
        Left = 120
        Top = 100
        Width = 294
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Prompt for credentials'
        TabOrder = 3
        OnClick = chkLoginPromptClick
      end
      object chkWindowsAuth: TCheckBox
        Left = 120
        Top = 123
        Width = 294
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use Windows authentication'
        Enabled = False
        TabOrder = 4
        OnClick = chkLoginPromptClick
      end
      object memoComment: TMemo
        Left = 120
        Top = 271
        Width = 294
        Height = 97
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 11
        OnChange = Modification
      end
      object editDatabases: TButtonedEdit
        Left = 120
        Top = 244
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 75
        RightButton.Visible = True
        TabOrder = 10
        TextHint = 'Separated by semicolon'
        OnChange = Modification
        OnRightButtonClick = editDatabasesRightButtonClick
      end
      object comboLibrary: TComboBox
        Left = 120
        Top = 36
        Width = 294
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = Modification
      end
    end
    object tabSSHtunnel: TTabSheet
      Caption = 'SSH tunnel'
      ImageIndex = 147
      DesignSize = (
        417
        371)
      object lblSSHLocalPort: TLabel
        Left = 3
        Top = 190
        Width = 51
        Height = 13
        Caption = 'Local port:'
        FocusControl = editSSHlocalport
      end
      object lblSSHUser: TLabel
        Left = 3
        Top = 82
        Width = 52
        Height = 13
        Caption = 'Username:'
        FocusControl = editSSHUser
      end
      object lblSSHPassword: TLabel
        Left = 3
        Top = 109
        Width = 50
        Height = 13
        Caption = 'Password:'
        FocusControl = editSSHPassword
      end
      object lblSSHPlinkExe: TLabel
        Left = 3
        Top = 12
        Width = 87
        Height = 13
        Caption = 'plink.exe location:'
      end
      object lblSSHhost: TLabel
        Left = 3
        Top = 55
        Width = 81
        Height = 13
        Caption = 'SSH host + port:'
        FocusControl = editSSHhost
      end
      object lblSSHkeyfile: TLabel
        Left = 3
        Top = 163
        Width = 75
        Height = 13
        Caption = 'Private key file:'
        FocusControl = editSSHPrivateKey
      end
      object lblDownloadPlink: TLabel
        Left = 120
        Top = 33
        Width = 93
        Height = 13
        Cursor = crHandPoint
        Hint = 'http://www.chiark.greenend.org.uk/~sgtatham/putty/'
        Caption = 'Download plink.exe'
        OnClick = lblDownloadPlinkClick
      end
      object lblPlinkTimeout: TLabel
        Left = 3
        Top = 136
        Width = 86
        Height = 13
        Caption = 'plink.exe timeout:'
      end
      object editSSHlocalport: TEdit
        Left = 120
        Top = 187
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 8
        Text = 'editSSHlocalport'
        OnChange = Modification
      end
      object editSSHUser: TEdit
        Left = 120
        Top = 79
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'editSSHUser'
        TextHint = 'Your secure shell username'
        OnChange = Modification
      end
      object editSSHPassword: TEdit
        Left = 120
        Top = 106
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 4
        Text = 'editSSHPassword'
        TextHint = 'Your secure shell password'
        OnChange = Modification
      end
      object editSSHPlinkExe: TButtonedEdit
        Left = 120
        Top = 9
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 0
        Text = 'editSSHPlinkExe'
        TextHint = 'Doubleclick to select plink.exe ...'
        OnChange = editSSHPlinkExeChange
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object editSSHhost: TEdit
        Left = 120
        Top = 52
        Width = 230
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'editSSHhost'
        OnChange = Modification
      end
      object editSSHport: TEdit
        Left = 356
        Top = 52
        Width = 58
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 2
        Text = 'editSSHport'
        OnChange = Modification
      end
      object editSSHPrivateKey: TButtonedEdit
        Left = 120
        Top = 160
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 7
        Text = 'editSSHPrivateKey'
        TextHint = 'PuTTY private key (*.ppk)'
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object editSSHTimeout: TEdit
        Left = 120
        Top = 133
        Width = 60
        Height = 21
        TabOrder = 5
        Text = '1'
        OnChange = Modification
      end
      object updownSSHTimeout: TUpDown
        Left = 180
        Top = 133
        Width = 17
        Height = 21
        Associate = editSSHTimeout
        Min = 1
        Position = 1
        TabOrder = 6
        Wrap = True
      end
    end
    object tabAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 98
      DesignSize = (
        417
        371)
      object lblSSLPrivateKey: TLabel
        Left = 3
        Top = 39
        Width = 78
        Height = 13
        Caption = 'SSL private key:'
        FocusControl = editSSLPrivateKey
      end
      object lblSSLCACertificate: TLabel
        Left = 3
        Top = 68
        Width = 89
        Height = 13
        Caption = 'SSL CA certificate:'
        FocusControl = editSSLCACertificate
      end
      object lblSSLCertificate: TLabel
        Left = 3
        Top = 95
        Width = 72
        Height = 13
        Caption = 'SSL certificate:'
        FocusControl = editSSLCertificate
      end
      object lblStartupScript: TLabel
        Left = 3
        Top = 168
        Width = 69
        Height = 13
        Caption = 'Startup script:'
        FocusControl = editStartupScript
      end
      object lblQueryTimeout: TLabel
        Left = 3
        Top = 195
        Width = 73
        Height = 13
        Caption = 'Query timeout:'
      end
      object lblSSLcipher: TLabel
        Left = 3
        Top = 120
        Width = 53
        Height = 13
        Caption = 'SSL cipher:'
      end
      object lblKeepAlive: TLabel
        Left = 3
        Top = 224
        Width = 106
        Height = 13
        Caption = 'Ping every X seconds:'
      end
      object lblBackgroundColor: TLabel
        Left = 3
        Top = 318
        Width = 86
        Height = 13
        Caption = 'Background color:'
      end
      object editSSLPrivateKey: TButtonedEdit
        Left = 120
        Top = 36
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 1
        TextHint = 'Path to key file'
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object editSSLCACertificate: TButtonedEdit
        Left = 120
        Top = 65
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 2
        TextHint = 'Path to certificate authority file'
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object editSSLCertificate: TButtonedEdit
        Left = 120
        Top = 92
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 3
        TextHint = 'Path to certificate file'
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object chkWantSSL: TCheckBox
        Left = 120
        Top = 13
        Width = 313
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use SSL'
        TabOrder = 0
        OnClick = Modification
      end
      object chkLocalTimeZone: TCheckBox
        Left = 120
        Top = 246
        Width = 294
        Height = 17
        Hint = 
          'Use your client time zone in date/time SQL functions, e.g. NOW()' +
          ', for MySQL 4.1.3+'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use own client time zone'
        TabOrder = 8
        OnClick = Modification
      end
      object chkCleartextPluginEnabled: TCheckBox
        Left = 120
        Top = 292
        Width = 294
        Height = 17
        Hint = 'Send your password to the server in cleartext, for MySQL 5.5.47+'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable cleartext authentication'
        TabOrder = 13
        OnClick = Modification
      end
      object editStartupScript: TButtonedEdit
        Left = 120
        Top = 165
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 5
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object chkFullTableStatus: TCheckBox
        Left = 120
        Top = 269
        Width = 294
        Height = 17
        Hint = 
          'Disable to speed up internal queries on databases with many Inno' +
          'DB tables'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Get full table status'
        TabOrder = 9
        OnClick = Modification
      end
      object editQueryTimeout: TEdit
        Left = 120
        Top = 192
        Width = 90
        Height = 21
        NumbersOnly = True
        TabOrder = 6
        Text = '0'
        OnChange = Modification
      end
      object updownQueryTimeout: TUpDown
        Left = 210
        Top = 192
        Width = 16
        Height = 21
        Associate = editQueryTimeout
        Max = 2147483646
        TabOrder = 7
        Wrap = True
      end
      object editSSLcipher: TEdit
        Left = 120
        Top = 119
        Width = 294
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        TextHint = 'List of permissible ciphers to use for SSL encryption'
        OnChange = Modification
      end
      object editKeepAlive: TEdit
        Left = 120
        Top = 219
        Width = 90
        Height = 21
        TabOrder = 10
        Text = '0'
        OnChange = Modification
      end
      object updownKeepAlive: TUpDown
        Left = 210
        Top = 219
        Width = 16
        Height = 21
        Associate = editKeepAlive
        Max = 86400
        TabOrder = 11
      end
      object ColorBoxBackgroundColor: TColorBox
        Left = 120
        Top = 315
        Width = 294
        Height = 22
        NoneColorColor = clNone
        Selected = clNone
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        TabOrder = 12
        OnChange = Modification
        OnGetColors = ColorBoxBackgroundColorGetColors
      end
    end
    object tabStatistics: TTabSheet
      Caption = 'Statistics'
      ImageIndex = 145
      object lblLastConnectLeft: TLabel
        Left = 3
        Top = 31
        Width = 65
        Height = 13
        Caption = 'Last connect:'
      end
      object lblCounterLeft: TLabel
        Left = 3
        Top = 50
        Width = 100
        Height = 13
        Caption = 'Successful connects:'
      end
      object lblCreatedLeft: TLabel
        Left = 3
        Top = 12
        Width = 43
        Height = 13
        Caption = 'Created:'
      end
      object lblCreatedRight: TLabel
        Left = 204
        Top = 12
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblCounterRight1: TLabel
        Left = 204
        Top = 50
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblLastConnectRight: TLabel
        Left = 204
        Top = 31
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblCounterRight2: TLabel
        Left = 204
        Top = 69
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblCounterLeft2: TLabel
        Left = 3
        Top = 69
        Width = 112
        Height = 13
        Caption = 'Unsuccessful connects:'
      end
    end
  end
  object btnMore: TButton
    Left = 561
    Top = 415
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'More'
    DropDownMenu = popupMore
    Style = bsSplitButton
    TabOrder = 6
    OnClick = btnMoreClick
  end
  object popupSessions: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 23
    Top = 83
    object menuRename: TMenuItem
      Caption = 'Rename'
      Enabled = False
      ImageIndex = 58
      ShortCut = 113
      OnClick = menuRenameClick
    end
    object menuSave: TMenuItem
      Caption = 'Save'
      ImageIndex = 10
      ShortCut = 16467
      OnClick = btnSaveClick
    end
    object menuSaveAs: TMenuItem
      Caption = 'Save as ...'
      ImageIndex = 10
      ShortCut = 123
      OnClick = btnSaveAsClick
    end
    object menuDelete: TMenuItem
      Caption = 'Delete'
      ImageIndex = 26
      ShortCut = 46
      OnClick = btnDeleteClick
    end
    object menuContextNewSessionInFolder: TMenuItem
      Caption = 'New session'
      ImageIndex = 72
      OnClick = btnNewClick
    end
    object menuContextNewFolderInFolder: TMenuItem
      Caption = 'New folder'
      ImageIndex = 174
      OnClick = btnNewClick
    end
  end
  object TimerStatistics: TTimer
    Interval = 60000
    OnTimer = TimerStatisticsTimer
    Left = 24
    Top = 35
  end
  object timerSettingsImport: TTimer
    Enabled = False
    OnTimer = timerSettingsImportTimer
    Left = 110
    Top = 35
  end
  object popupNew: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 109
    Top = 82
    object menuNewSessionInRoot: TMenuItem
      Caption = 'Session in root folder'
      ImageIndex = 72
      OnClick = btnNewClick
    end
    object menuNewSessionInFolder: TMenuItem
      Caption = 'Session in selected folder'
      ImageIndex = 72
      OnClick = btnNewClick
    end
    object menuNewFolderInRoot: TMenuItem
      Caption = 'Folder in root folder'
      ImageIndex = 174
      OnClick = btnNewClick
    end
    object menuNewFolderInFolder: TMenuItem
      Caption = 'Folder in selected folder'
      ImageIndex = 174
      OnClick = btnNewClick
    end
  end
  object popupMore: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 24
    Top = 144
    object Preferences1: TMenuItem
      Action = MainForm.actPreferences
    end
    object Checkforupdates1: TMenuItem
      Action = MainForm.actUpdateCheck
    end
    object Importsettingsfile1: TMenuItem
      Action = MainForm.actImportSettings
    end
    object Exportsettingsfile1: TMenuItem
      Action = MainForm.actExportSettings
    end
    object menuMoreGeneralHelp: TMenuItem
      Action = MainForm.actHelp
    end
    object About1: TMenuItem
      Action = MainForm.actAboutBox
    end
  end
  object TimerButtonAnimation: TTimer
    Enabled = False
    Interval = 200
    OnTimer = TimerButtonAnimationTimer
    Left = 112
    Top = 144
  end
end
