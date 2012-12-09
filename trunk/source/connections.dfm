object connform: Tconnform
  Left = 288
  Top = 129
  BorderIcons = [biSystemMenu]
  Caption = 'Session manager'
  ClientHeight = 364
  ClientWidth = 494
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 510
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    494
    364)
  PixelsPerInch = 96
  TextHeight = 13
  object splitterMain: TSplitter
    AlignWithMargins = True
    Left = 175
    Top = 8
    Width = 8
    Height = 316
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
    Top = 331
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object btnOpen: TButton
    Left = 320
    Top = 331
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open'
    Default = True
    Enabled = False
    TabOrder = 5
    OnClick = btnOpenClick
  end
  object btnCancel: TButton
    Left = 406
    Top = 331
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object ListSessions: TVirtualStringTree
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 167
    Height = 316
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 40
    Align = alLeft
    DragMode = dmAutomatic
    EditDelay = 250
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
    Header.PopupMenu = MainForm.popupListHeader
    Header.SortColumn = 0
    HintMode = hmTooltip
    Images = MainForm.ImageListMain
    IncrementalSearch = isAll
    PopupMenu = popupSessions
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag, toEditOnClick]
    TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
    TreeOptions.SelectionOptions = [toRightClickSelect]
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
        Width = 163
        WideText = 'Session name'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 1
        WideText = 'Host'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 2
        WideText = 'User'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 3
        WideText = 'Version'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 4
        WideText = 'Last connect'
      end
      item
        Alignment = taRightJustify
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 5
        WideText = 'Counter'
      end>
  end
  object btnNew: TButton
    Left = 8
    Top = 331
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
    Top = 331
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object PageControlDetails: TPageControl
    AlignWithMargins = True
    Left = 183
    Top = 8
    Width = 303
    Height = 316
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 40
    ActivePage = tabSettings
    Align = alClient
    Images = MainForm.ImageListMain
    TabOrder = 4
    object tabStart: TTabSheet
      Caption = 'Start'
      ImageIndex = 112
      object lblHelp: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 10
        Width = 275
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
        295
        287)
      object lblStartupScript: TLabel
        Left = 3
        Top = 264
        Width = 69
        Height = 13
        Caption = 'Startup script:'
        FocusControl = editStartupScript
      end
      object lblPort: TLabel
        Left = 3
        Top = 168
        Width = 24
        Height = 13
        Caption = 'Port:'
        FocusControl = editPort
      end
      object lblPassword: TLabel
        Left = 3
        Top = 143
        Width = 50
        Height = 13
        Caption = 'Password:'
        FocusControl = editPassword
      end
      object lblHost: TLabel
        Left = 3
        Top = 45
        Width = 72
        Height = 13
        Caption = 'Hostname / IP:'
        FocusControl = editHost
      end
      object lblUsername: TLabel
        Left = 3
        Top = 118
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
        Top = 239
        Width = 55
        Height = 13
        Caption = 'Databases:'
      end
      object editStartupScript: TButtonedEdit
        Left = 120
        Top = 261
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 11
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object chkCompressed: TCheckBox
        Left = 120
        Top = 190
        Width = 172
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Compressed client/server protocol'
        TabOrder = 8
        OnClick = Modification
      end
      object editPort: TEdit
        Left = 120
        Top = 165
        Width = 57
        Height = 21
        TabOrder = 6
        Text = '0'
        OnChange = Modification
      end
      object updownPort: TUpDown
        Left = 177
        Top = 165
        Width = 16
        Height = 21
        Associate = editPort
        Max = 65535
        TabOrder = 7
        Thousands = False
        Wrap = True
      end
      object editPassword: TEdit
        Left = 120
        Top = 140
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 5
        OnChange = Modification
      end
      object editUsername: TEdit
        Left = 120
        Top = 115
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = Modification
      end
      object editHost: TEdit
        Left = 120
        Top = 42
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = editHostChange
      end
      object comboNetType: TComboBox
        Left = 120
        Top = 9
        Width = 172
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
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
      object comboDatabases: TComboBox
        Left = 120
        Top = 236
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 10
        TextHint = 'Separated by semicolon'
        OnChange = Modification
        OnDropDown = comboDatabasesDropDown
      end
      object chkLoginPrompt: TCheckBox
        Left = 120
        Top = 69
        Width = 172
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Prompt for credentials'
        TabOrder = 2
        OnClick = chkLoginPromptClick
      end
      object chkWindowsAuth: TCheckBox
        Left = 120
        Top = 92
        Width = 172
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use Windows authentication'
        Enabled = False
        TabOrder = 3
        OnClick = chkLoginPromptClick
      end
      object chkLocalTimeZone: TCheckBox
        Left = 120
        Top = 213
        Width = 172
        Height = 17
        Hint = 
          'Use your client time zone in date/time SQL functions, e.g. NOW()' +
          ', for MySQL 4.1.3+'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Set client time zone'
        TabOrder = 9
        OnClick = Modification
      end
    end
    object tabSSHtunnel: TTabSheet
      Caption = 'SSH tunnel'
      ImageIndex = 147
      DesignSize = (
        295
        287)
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
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
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
        Width = 172
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
        Width = 172
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
        Width = 172
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
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
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
        Width = 108
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'editSSHhost'
        OnChange = Modification
      end
      object editSSHport: TEdit
        Left = 234
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
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
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
        Text = '0'
        OnChange = Modification
      end
      object updownSSHTimeout: TUpDown
        Left = 180
        Top = 133
        Width = 17
        Height = 21
        Associate = editSSHTimeout
        TabOrder = 6
        Wrap = True
      end
    end
    object tabSSLOptions: TTabSheet
      Caption = 'SSL options'
      ImageIndex = 144
      DesignSize = (
        295
        287)
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
      object editSSLPrivateKey: TButtonedEdit
        Left = 120
        Top = 36
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 1
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object editSSLCACertificate: TButtonedEdit
        Left = 120
        Top = 65
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 2
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object editSSLCertificate: TButtonedEdit
        Left = 120
        Top = 92
        Width = 172
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 3
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object chkWantSSL: TCheckBox
        Left = 120
        Top = 13
        Width = 191
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use SSL'
        TabOrder = 0
        OnClick = Modification
      end
    end
    object tabStatistics: TTabSheet
      Caption = 'Statistics'
      ImageIndex = 145
      DesignSize = (
        295
        287)
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
        Width = 43
        Height = 13
        Caption = 'Counter:'
      end
      object lblCreatedLeft: TLabel
        Left = 3
        Top = 12
        Width = 43
        Height = 13
        Caption = 'Created:'
      end
      object lblCreatedRight: TLabel
        Left = 120
        Top = 12
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblCounterRight: TLabel
        Left = 120
        Top = 50
        Width = 172
        Height = 47
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '?'
        WordWrap = True
      end
      object lblLastConnectRight: TLabel
        Left = 120
        Top = 31
        Width = 5
        Height = 13
        Caption = '?'
      end
    end
  end
  object popupSessions: TPopupMenu
    Images = MainForm.ImageListMain
    Left = 23
    Top = 83
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
    Images = MainForm.ImageListMain
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
end
