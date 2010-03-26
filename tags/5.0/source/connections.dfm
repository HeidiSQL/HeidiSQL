object connform: Tconnform
  Tag = 1
  Left = 288
  Top = 129
  BorderIcons = [biSystemMenu]
  Caption = 'Session manager'
  ClientHeight = 274
  ClientWidth = 494
  Color = clBtnFace
  Constraints.MinHeight = 310
  Constraints.MinWidth = 510
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    494
    274)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSession: TLabel
    Tag = 5
    Left = 8
    Top = 8
    Width = 77
    Height = 13
    Caption = 'Saved sessions:'
  end
  object lblHelp: TLabel
    Left = 177
    Top = 27
    Width = 296
    Height = 190
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblHelp'
    Visible = False
    WordWrap = True
  end
  object btnSave: TButton
    Left = 64
    Top = 241
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object btnOpen: TButton
    Tag = 15
    Left = 320
    Top = 241
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
    Tag = 16
    Left = 406
    Top = 241
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object ListSessions: TVirtualStringTree
    Left = 9
    Top = 27
    Width = 162
    Height = 206
    Anchors = [akLeft, akTop, akBottom]
    EditDelay = 250
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    HintMode = hmTooltip
    Images = MainForm.ImageListMain
    IncrementalSearch = isAll
    PopupMenu = popupSessions
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnCreateEditor = ListSessionsCreateEditor
    OnDblClick = btnOpenClick
    OnFocusChanged = ListSessionsFocusChanged
    OnFocusChanging = ListSessionsFocusChanging
    OnGetText = ListSessionsGetText
    OnGetImageIndex = ListSessionsGetImageIndex
    OnNewText = ListSessionsNewText
    Columns = <
      item
        Position = 0
        Width = 158
        WideText = 'Title'
      end>
  end
  object btnNew: TButton
    Left = 8
    Top = 241
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'New'
    TabOrder = 1
    OnClick = btnNewClick
  end
  object btnDelete: TButton
    Left = 120
    Top = 241
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object PageControlDetails: TPageControl
    Left = 177
    Top = 8
    Width = 309
    Height = 226
    ActivePage = tabSettings
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = MainForm.ImageListMain
    TabOrder = 4
    object tabSettings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 39
      DesignSize = (
        301
        197)
      object lblStartupScript: TLabel
        Left = 3
        Top = 168
        Width = 69
        Height = 13
        Caption = 'Startup script:'
        FocusControl = editStartupScript
      end
      object lblPort: TLabel
        Tag = 9
        Left = 3
        Top = 120
        Width = 24
        Height = 13
        Caption = 'Port:'
        FocusControl = editPort
      end
      object lblPassword: TLabel
        Tag = 8
        Left = 3
        Top = 95
        Width = 50
        Height = 13
        Caption = 'Password:'
        FocusControl = editPassword
      end
      object lblHost: TLabel
        Tag = 6
        Left = 3
        Top = 45
        Width = 72
        Height = 13
        Caption = 'Hostname / IP:'
        FocusControl = editHost
      end
      object lblUsername: TLabel
        Tag = 7
        Left = 3
        Top = 70
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
      object editStartupScript: TButtonedEdit
        Left = 101
        Top = 165
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 8
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object chkCompressed: TCheckBox
        Tag = 12
        Left = 101
        Top = 142
        Width = 197
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Compressed client/server protocol'
        TabOrder = 7
        OnClick = Modification
      end
      object editPort: TEdit
        Left = 101
        Top = 117
        Width = 60
        Height = 21
        TabOrder = 5
        Text = '0'
        OnChange = editPortChange
      end
      object updownPort: TUpDown
        Left = 161
        Top = 117
        Width = 17
        Height = 21
        Max = 32767
        TabOrder = 6
        Thousands = False
        OnChangingEx = updownPortChangingEx
      end
      object editPassword: TEdit
        Left = 101
        Top = 92
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 4
        OnChange = Modification
      end
      object editUsername: TEdit
        Left = 101
        Top = 67
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = Modification
      end
      object editHost: TEdit
        Left = 101
        Top = 42
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = Modification
      end
      object radioTypeTCPIP: TRadioButton
        Left = 101
        Top = 11
        Width = 67
        Height = 17
        Caption = 'TCP/IP'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = radioNetTypeClick
      end
      object radioTypeNamedPipe: TRadioButton
        Left = 174
        Top = 11
        Width = 113
        Height = 17
        Caption = 'Named pipe'
        TabOrder = 1
        OnClick = radioNetTypeClick
      end
    end
    object tabSSLOptions: TTabSheet
      Caption = 'SSL options'
      ImageIndex = 144
      DesignSize = (
        301
        197)
      object lblSSLPrivateKey: TLabel
        Tag = 6
        Left = 3
        Top = 12
        Width = 78
        Height = 13
        Caption = 'SSL private key:'
        FocusControl = editSSLPrivateKey
      end
      object lblSSLCACertificate: TLabel
        Tag = 6
        Left = 3
        Top = 41
        Width = 89
        Height = 13
        Caption = 'SSL CA certificate:'
        FocusControl = editSSLCACertificate
      end
      object lblSSLCertificate: TLabel
        Tag = 6
        Left = 3
        Top = 68
        Width = 72
        Height = 13
        Caption = 'SSL certificate:'
        FocusControl = editSSLCertificate
      end
      object editSSLPrivateKey: TButtonedEdit
        Left = 101
        Top = 9
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Images = MainForm.ImageListMain
        RightButton.ImageIndex = 51
        RightButton.Visible = True
        TabOrder = 0
        OnChange = Modification
        OnDblClick = PickFile
        OnRightButtonClick = PickFile
      end
      object editSSLCACertificate: TButtonedEdit
        Left = 101
        Top = 38
        Width = 197
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
      object editSSLCertificate: TButtonedEdit
        Left = 101
        Top = 65
        Width = 197
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
    end
    object tabStatistics: TTabSheet
      Caption = 'Statistics'
      ImageIndex = 145
      DesignSize = (
        301
        197)
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
        Left = 101
        Top = 12
        Width = 5
        Height = 13
        Caption = '?'
      end
      object lblCounterRight: TLabel
        Left = 101
        Top = 50
        Width = 197
        Height = 39
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '?'
        WordWrap = True
      end
      object lblLastConnectRight: TLabel
        Left = 101
        Top = 31
        Width = 5
        Height = 13
        Caption = '?'
      end
    end
  end
  object popupSessions: TPopupMenu
    Images = MainForm.ImageListMain
    Left = 16
    Top = 35
    object Save1: TMenuItem
      Caption = 'Save'
      ImageIndex = 10
      ShortCut = 16467
      OnClick = btnSaveClick
    end
    object Saveas1: TMenuItem
      Caption = 'Save as ...'
      ImageIndex = 10
      ShortCut = 123
      OnClick = btnSaveAsClick
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      ImageIndex = 26
      ShortCut = 46
      OnClick = btnDeleteClick
    end
  end
  object TimerStatistics: TTimer
    Interval = 60000
    OnTimer = TimerStatisticsTimer
    Left = 48
    Top = 35
  end
end
