object connform: Tconnform
  Left = 288
  Top = 129
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'Session manager'
  ClientHeight = 506
  ClientWidth = 749
  Color = clBtnFace
  Constraints.MinHeight = 470
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
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    749
    506)
  PixelsPerInch = 96
  TextHeight = 13
  object splitterMain: TSplitter
    AlignWithMargins = True
    Left = 208
    Top = 8
    Width = 8
    Height = 458
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
    Top = 473
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    ImageIndex = 10
    ImageName = 'icons8-save-button-100'
    Images = MainForm.VirtualImageListMain
    TabOrder = 1
    OnClick = btnSaveClick
  end
  object btnOpen: TButton
    Left = 489
    Top = 473
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = btnOpenClick
  end
  object btnCancel: TButton
    Left = 575
    Top = 473
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnNew: TButton
    Left = 8
    Top = 473
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'New'
    DropDownMenu = popupNew
    ImageIndex = 45
    ImageName = 'icons8-add'
    Images = MainForm.VirtualImageListMain
    Style = bsSplitButton
    TabOrder = 0
    OnClick = btnNewClick
  end
  object btnDelete: TButton
    Left = 120
    Top = 473
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    ImageIndex = 46
    ImageName = 'icons8-delete-button'
    Images = MainForm.VirtualImageListMain
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object PageControlDetails: TPageControl
    AlignWithMargins = True
    Left = 216
    Top = 8
    Width = 525
    Height = 458
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 40
    ActivePage = tabStart
    Align = alClient
    Images = MainForm.VirtualImageListMain
    TabOrder = 6
    object tabStart: TTabSheet
      Caption = 'Start'
      ImageIndex = 112
      ImageName = 'icons8-star-filled'
      object lblHelp: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 10
        Width = 497
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
      ImageName = 'icons8-support'
      DesignSize = (
        517
        429)
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
        Left = 190
        Top = 221
        Width = 320
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Compressed client/server protocol'
        TabOrder = 9
        OnClick = Modification
      end
      object editPort: TEdit
        Left = 190
        Top = 196
        Width = 57
        Height = 21
        TabOrder = 7
        Text = '0'
        OnChange = Modification
      end
      object updownPort: TUpDown
        Left = 247
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
        Left = 190
        Top = 171
        Width = 320
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 6
        OnChange = Modification
      end
      object editUsername: TEdit
        Left = 190
        Top = 146
        Width = 320
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        OnChange = Modification
        OnExit = editTrim
      end
      object editHost: TButtonedEdit
        Left = 190
        Top = 73
        Width = 320
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.DropDownMenu = popupHost
        RightButton.ImageIndex = 75
        RightButton.Visible = True
        TabOrder = 2
        OnChange = editHostChange
        OnDblClick = editHostDblClick
        OnExit = editTrim
      end
      object comboNetType: TComboBoxEx
        Left = 190
        Top = 8
        Width = 320
        Height = 22
        ItemsEx = <>
        Style = csExDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = comboNetTypeChange
        Images = MainForm.VirtualImageListMain
        DropDownCount = 12
      end
      object chkLoginPrompt: TCheckBox
        Left = 190
        Top = 100
        Width = 320
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Prompt for credentials'
        TabOrder = 3
        OnClick = chkLoginPromptClick
      end
      object chkWindowsAuth: TCheckBox
        Left = 190
        Top = 123
        Width = 320
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use Windows authentication'
        Enabled = False
        TabOrder = 4
        OnClick = chkLoginPromptClick
      end
      object memoComment: TMemo
        Left = 190
        Top = 271
        Width = 320
        Height = 153
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 11
        OnChange = Modification
      end
      object editDatabases: TButtonedEdit
        Left = 190
        Top = 244
        Width = 320
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
        Left = 190
        Top = 36
        Width = 320
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
      ImageName = 'icons8-lock'
      DesignSize = (
        517
        429)
      object lblSSHLocalPort: TLabel
        Left = 3
        Top = 174
        Width = 51
        Height = 13
        Caption = 'Local port:'
        FocusControl = editSSHlocalport
      end
      object lblSSHUser: TLabel
        Left = 3
        Top = 66
        Width = 52
        Height = 13
        Caption = 'Username:'
        FocusControl = editSSHUser
      end
      object lblSSHPassword: TLabel
        Left = 3
        Top = 93
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
        Top = 39
        Width = 81
        Height = 13
        Caption = 'SSH host + port:'
        FocusControl = editSSHhost
      end
      object lblSSHkeyfile: TLabel
        Left = 3
        Top = 147
        Width = 75
        Height = 13
        Caption = 'Private key file:'
        FocusControl = editSSHPrivateKey
      end
      object lblPlinkTimeout: TLabel
        Left = 3
        Top = 120
        Width = 86
        Height = 13
        Caption = 'plink.exe timeout:'
      end
      object editSSHlocalport: TEdit
        Left = 190
        Top = 171
        Width = 320
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 8
        Text = 'editSSHlocalport'
        OnChange = Modification
      end
      object editSSHUser: TEdit
        Left = 190
        Top = 63
        Width = 320
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'editSSHUser'
        TextHint = 'Your secure shell username'
        OnChange = Modification
        OnExit = editTrim
      end
      object editSSHPassword: TEdit
        Left = 190
        Top = 90
        Width = 320
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 4
        Text = 'editSSHPassword'
        TextHint = 'Your secure shell password'
        OnChange = Modification
      end
      object editSSHPlinkExe: TButtonedEdit
        Left = 190
        Top = 9
        Width = 320
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
        OnExit = editTrim
        OnRightButtonClick = PickFile
      end
      object editSSHhost: TEdit
        Left = 190
        Top = 36
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'editSSHhost'
        OnChange = Modification
        OnExit = editTrim
      end
      object editSSHport: TEdit
        Left = 456
        Top = 36
        Width = 54
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 2
        Text = 'editSSHport'
        OnChange = Modification
      end
      object editSSHPrivateKey: TButtonedEdit
        Left = 190
        Top = 144
        Width = 320
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
        OnExit = editTrim
        OnRightButtonClick = PickFile
      end
      object editSSHTimeout: TEdit
        Left = 190
        Top = 117
        Width = 60
        Height = 21
        TabOrder = 5
        Text = '1'
        OnChange = Modification
      end
      object updownSSHTimeout: TUpDown
        Left = 250
        Top = 117
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
      ImageName = 'icons8-support-orange'
      DesignSize = (
        517
        429)
      object lblStartupScript: TLabel
        Left = 3
        Top = 12
        Width = 69
        Height = 13
        Caption = 'Startup script:'
        FocusControl = editStartupScript
      end
      object lblQueryTimeout: TLabel
        Left = 3
        Top = 39
        Width = 73
        Height = 13
        Caption = 'Query timeout:'
      end
      object lblKeepAlive: TLabel
        Left = 3
        Top = 66
        Width = 106
        Height = 13
        Caption = 'Ping every X seconds:'
      end
      object lblBackgroundColor: TLabel
        Left = 3
        Top = 162
        Width = 86
        Height = 13
        Caption = 'Background color:'
      end
      object lblIgnoreDatabasePattern: TLabel
        Left = 3
        Top = 190
        Width = 112
        Height = 13
        Caption = 'Hide database pattern:'
      end
      object lblLogFile: TLabel
        Left = 3
        Top = 229
        Width = 89
        Height = 13
        Caption = 'Log queries to file:'
      end
      object chkLocalTimeZone: TCheckBox
        Left = 190
        Top = 90
        Width = 324
        Height = 17
        Hint = 
          'Use your client time zone in date/time SQL functions, e.g. NOW()' +
          ', for MySQL 4.1.3+'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use own client time zone'
        TabOrder = 5
        OnClick = Modification
      end
      object chkCleartextPluginEnabled: TCheckBox
        Left = 190
        Top = 136
        Width = 324
        Height = 17
        Hint = 'Send your password to the server in cleartext, for MySQL 5.5.47+'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable cleartext authentication'
        TabOrder = 7
        OnClick = Modification
      end
      object editStartupScript: TButtonedEdit
        Left = 190
        Top = 9
        Width = 324
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 0
        OnChange = Modification
        OnDblClick = PickFile
        OnExit = editTrim
        OnRightButtonClick = PickFile
      end
      object chkFullTableStatus: TCheckBox
        Left = 190
        Top = 113
        Width = 324
        Height = 17
        Hint = 
          'Disable to speed up internal queries on databases with many tabl' +
          'es'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Get full table status'
        TabOrder = 6
        OnClick = Modification
      end
      object editQueryTimeout: TEdit
        Left = 190
        Top = 36
        Width = 90
        Height = 21
        NumbersOnly = True
        TabOrder = 1
        Text = '0'
        OnChange = Modification
      end
      object updownQueryTimeout: TUpDown
        Left = 280
        Top = 36
        Width = 16
        Height = 21
        Associate = editQueryTimeout
        Max = 2147483646
        TabOrder = 2
        Wrap = True
      end
      object editKeepAlive: TEdit
        Left = 190
        Top = 63
        Width = 90
        Height = 21
        TabOrder = 3
        Text = '0'
        OnChange = Modification
      end
      object updownKeepAlive: TUpDown
        Left = 280
        Top = 63
        Width = 16
        Height = 21
        Associate = editKeepAlive
        Max = 86400
        TabOrder = 4
      end
      object ColorBoxBackgroundColor: TColorBox
        Left = 190
        Top = 159
        Width = 324
        Height = 22
        NoneColorColor = clNone
        Selected = clNone
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        TabOrder = 8
        OnChange = Modification
        OnGetColors = ColorBoxBackgroundColorGetColors
      end
      object editIgnoreDatabasePattern: TEdit
        Left = 190
        Top = 187
        Width = 324
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 9
        TextHint = 'Regular expression'
        OnChange = Modification
      end
      object chkLogFileDdl: TCheckBox
        Left = 190
        Top = 253
        Width = 324
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'DDL queries (CREATE, ALTER, ...)'
        TabOrder = 11
        OnClick = Modification
      end
      object editLogFilePath: TButtonedEdit
        Left = 190
        Top = 226
        Width = 324
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 10
        OnChange = Modification
        OnRightButtonClick = PickFile
      end
      object chkLogFileDml: TCheckBox
        Left = 190
        Top = 276
        Width = 324
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'DML queries (INSERT, UPDATE, ...)'
        TabOrder = 12
        OnClick = Modification
      end
    end
    object tabSSL: TTabSheet
      Caption = 'SSL'
      ImageIndex = 25
      DesignSize = (
        517
        429)
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
        Top = 66
        Width = 89
        Height = 13
        Caption = 'SSL CA certificate:'
        FocusControl = editSSLCACertificate
      end
      object lblSSLCertificate: TLabel
        Left = 3
        Top = 93
        Width = 72
        Height = 13
        Caption = 'SSL certificate:'
        FocusControl = editSSLCertificate
      end
      object lblSSLcipher: TLabel
        Left = 3
        Top = 120
        Width = 53
        Height = 13
        Caption = 'SSL cipher:'
      end
      object chkWantSSL: TCheckBox
        Left = 190
        Top = 13
        Width = 324
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use SSL'
        TabOrder = 0
        OnClick = Modification
      end
      object editSSLcipher: TEdit
        Left = 190
        Top = 117
        Width = 324
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        TextHint = 'List of permissible ciphers to use for SSL encryption'
        OnChange = Modification
        OnExit = editTrim
      end
      object editSSLCertificate: TButtonedEdit
        Left = 190
        Top = 90
        Width = 324
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 2
        TextHint = 'Path to certificate file'
        OnChange = Modification
        OnDblClick = PickFile
        OnExit = editTrim
        OnRightButtonClick = PickFile
      end
      object editSSLCACertificate: TButtonedEdit
        Left = 190
        Top = 63
        Width = 324
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 3
        TextHint = 'Path to certificate authority file'
        OnChange = Modification
        OnDblClick = PickFile
        OnExit = editTrim
        OnRightButtonClick = PickFile
      end
      object editSSLPrivateKey: TButtonedEdit
        Left = 190
        Top = 36
        Width = 324
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.VirtualImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 4
        TextHint = 'Path to key file'
        OnChange = Modification
        OnDblClick = PickFile
        OnExit = editTrim
        OnRightButtonClick = PickFile
      end
    end
    object tabStatistics: TTabSheet
      Caption = 'Statistics'
      ImageIndex = 145
      ImageName = 'icons8-bar-chart'
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
        Left = 190
        Top = 12
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblCounterRight1: TLabel
        Left = 190
        Top = 50
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblLastConnectRight: TLabel
        Left = 190
        Top = 31
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblCounterRight2: TLabel
        Left = 190
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
    Left = 661
    Top = 473
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'More'
    DropDownMenu = popupMore
    Style = bsSplitButton
    TabOrder = 5
    OnClick = btnMoreClick
  end
  object pnlLeft: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 200
    Height = 458
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 40
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 200
    TabOrder = 7
    object ListSessions: TVirtualStringTree
      Left = 0
      Top = 26
      Width = 200
      Height = 432
      Align = alClient
      DragMode = dmAutomatic
      Header.AutoSizeIndex = -1
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible, hoDisableAnimatedResize, hoAutoResizeInclCaption]
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
      OnBeforeCellPaint = ListSessionsBeforeCellPaint
      OnCreateEditor = ListSessionsCreateEditor
      OnDragOver = ListSessionsDragOver
      OnDragDrop = ListSessionsDragDrop
      OnFocusChanged = ListSessionsFocusChanged
      OnFocusChanging = ListSessionsFocusChanging
      OnGetText = ListSessionsGetText
      OnGetImageIndex = ListSessionsGetImageIndex
      OnGetNodeDataSize = ListSessionsGetNodeDataSize
      OnNewText = ListSessionsNewText
      OnNodeDblClick = ListSessionsNodeDblClick
      OnStructureChange = ListSessionsStructureChange
      Columns = <
        item
          Position = 0
          Text = 'Session name'
          Width = 163
        end
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
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
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
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
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus]
          Position = 6
          Text = 'Comment'
          Width = 10
        end>
    end
    object editSearch: TButtonedEdit
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 200
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alTop
      Images = MainForm.VirtualImageListMain
      LeftButton.ImageIndex = 30
      LeftButton.Visible = True
      RightButton.ImageIndex = 193
      TabOrder = 1
      TextHint = 'Filter ...'
      OnChange = editSearchChange
      OnRightButtonClick = editSearchRightButtonClick
    end
  end
  object popupSessions: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 23
    Top = 83
    object menuRename: TMenuItem
      Caption = 'Rename'
      Enabled = False
      ImageIndex = 58
      ImageName = 'icons8-rename'
      ShortCut = 113
      OnClick = menuRenameClick
    end
    object menuSave: TMenuItem
      Caption = 'Save'
      ImageIndex = 10
      ImageName = 'icons8-save-button-100'
      ShortCut = 16467
      OnClick = btnSaveClick
    end
    object menuSaveAs: TMenuItem
      Caption = 'Duplicate / save as ...'
      ImageIndex = 10
      ImageName = 'icons8-save-button-100'
      ShortCut = 123
      OnClick = btnSaveAsClick
    end
    object menuDelete: TMenuItem
      Caption = 'Delete'
      ImageIndex = 26
      ImageName = 'icons8-close-button'
      ShortCut = 46
      OnClick = btnDeleteClick
    end
    object menuContextNewSessionInFolder: TMenuItem
      Caption = 'New session'
      ImageIndex = 72
      ImageName = 'icons8-database-symbol'
      OnClick = btnNewClick
    end
    object menuContextNewFolderInFolder: TMenuItem
      Caption = 'New folder'
      ImageIndex = 174
      ImageName = 'icons8-folder-other'
      OnClick = btnNewClick
    end
    object Filter1: TMenuItem
      Action = actFilter
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
      ImageName = 'icons8-database-symbol'
      OnClick = btnNewClick
    end
    object menuNewSessionInFolder: TMenuItem
      Caption = 'Session in selected folder'
      ImageIndex = 72
      ImageName = 'icons8-database-symbol'
      OnClick = btnNewClick
    end
    object menuNewFolderInRoot: TMenuItem
      Caption = 'Folder in root folder'
      ImageIndex = 174
      ImageName = 'icons8-folder-other'
      OnClick = btnNewClick
    end
    object menuNewFolderInFolder: TMenuItem
      Caption = 'Folder in selected folder'
      ImageIndex = 174
      ImageName = 'icons8-folder-other'
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
  object popupHost: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 24
    Top = 208
    object menuFindDatabaseFiles: TMenuItem
      Caption = 'Find database files...'
      ImageIndex = 30
      ImageName = 'icons8-find'
      OnClick = FindAddDatabaseFilesClick
    end
    object menuAddDatabaseFiles: TMenuItem
      Caption = 'Add database files...'
      ImageIndex = 72
      ImageName = 'icons8-database-symbol'
      OnClick = FindAddDatabaseFilesClick
    end
  end
  object ActionListConnections: TActionList
    Images = MainForm.VirtualImageListMain
    Left = 112
    Top = 208
    object actFilter: TAction
      Caption = 'Filter ...'
      ImageIndex = 30
      ShortCut = 16454
      OnExecute = actFilterExecute
    end
  end
end
