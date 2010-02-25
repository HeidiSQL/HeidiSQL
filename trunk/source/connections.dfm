object connform: Tconnform
  Tag = 1
  Left = 288
  Top = 129
  BorderIcons = [biSystemMenu]
  Caption = 'Session manager'
  ClientHeight = 303
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
    303)
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
    Top = 270
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object btnOpen: TButton
    Tag = 15
    Left = 320
    Top = 270
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
    Top = 270
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
    Height = 235
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
    Top = 270
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'New'
    TabOrder = 2
    OnClick = btnNewClick
  end
  object btnDelete: TButton
    Left = 120
    Top = 270
    Width = 50
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object grpDetails: TGroupBox
    Left = 177
    Top = 10
    Width = 309
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Details'
    TabOrder = 1
    Visible = False
    DesignSize = (
      309
      254)
    object lblHost: TLabel
      Tag = 6
      Left = 8
      Top = 45
      Width = 72
      Height = 13
      Caption = 'Hostname / IP:'
      FocusControl = editHost
    end
    object lblUsername: TLabel
      Tag = 7
      Left = 8
      Top = 70
      Width = 26
      Height = 13
      Caption = 'User:'
      FocusControl = editUsername
    end
    object lblPassword: TLabel
      Tag = 8
      Left = 8
      Top = 95
      Width = 50
      Height = 13
      Caption = 'Password:'
      FocusControl = editPassword
    end
    object lblPort: TLabel
      Tag = 9
      Left = 8
      Top = 120
      Width = 24
      Height = 13
      Caption = 'Port:'
      FocusControl = editPort
    end
    object lblLastConnectLeft: TLabel
      Left = 8
      Top = 191
      Width = 65
      Height = 13
      Caption = 'Last connect:'
    end
    object lblLastConnectRight: TLabel
      Left = 99
      Top = 191
      Width = 5
      Height = 13
      Caption = '?'
    end
    object lblCreatedLeft: TLabel
      Left = 8
      Top = 227
      Width = 43
      Height = 13
      Caption = 'Created:'
    end
    object lblCreatedRight: TLabel
      Left = 99
      Top = 227
      Width = 5
      Height = 13
      Caption = '?'
    end
    object lblNetworkType: TLabel
      Left = 8
      Top = 20
      Width = 69
      Height = 13
      Caption = 'Network type:'
    end
    object lblCounterLeft: TLabel
      Left = 8
      Top = 209
      Width = 43
      Height = 13
      Caption = 'Counter:'
    end
    object lblCounterRight: TLabel
      Left = 99
      Top = 209
      Width = 5
      Height = 13
      Caption = '?'
    end
    object lblStartupScript: TLabel
      Left = 8
      Top = 168
      Width = 69
      Height = 13
      Caption = 'Startup script:'
      FocusControl = editStartupScript
    end
    object editHost: TEdit
      Left = 99
      Top = 42
      Width = 205
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = Modification
    end
    object editUsername: TEdit
      Left = 99
      Top = 67
      Width = 205
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = Modification
    end
    object editPassword: TEdit
      Left = 99
      Top = 92
      Width = 205
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      PasswordChar = '*'
      TabOrder = 4
      OnChange = Modification
    end
    object editPort: TEdit
      Left = 99
      Top = 117
      Width = 60
      Height = 21
      TabOrder = 5
      Text = '0'
      OnChange = editPortChange
    end
    object chkCompressed: TCheckBox
      Tag = 12
      Left = 99
      Top = 142
      Width = 205
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Compressed client/server protocol'
      TabOrder = 7
      OnClick = Modification
    end
    object radioTypeTCPIP: TRadioButton
      Left = 99
      Top = 19
      Width = 67
      Height = 17
      Caption = 'TCP/IP'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = radioNetTypeClick
    end
    object radioTypeNamedPipe: TRadioButton
      Left = 172
      Top = 19
      Width = 113
      Height = 17
      Caption = 'Named pipe'
      TabOrder = 1
      OnClick = radioNetTypeClick
    end
    object updownPort: TUpDown
      Left = 159
      Top = 117
      Width = 17
      Height = 21
      Max = 32767
      TabOrder = 6
      Thousands = False
      OnChangingEx = updownPortChangingEx
    end
    object editStartupScript: TButtonedEdit
      Left = 99
      Top = 165
      Width = 205
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Images = MainForm.ImageListMain
      RightButton.ImageIndex = 52
      RightButton.Visible = True
      TabOrder = 8
      TextHint = 'Select SQL file ...'
      OnChange = Modification
      OnDblClick = editStartupScriptRightButtonClick
      OnRightButtonClick = editStartupScriptRightButtonClick
    end
  end
  object popupSessions: TPopupMenu
    Images = MainForm.ImageListMain
    Left = 176
    Top = 267
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
    Left = 208
    Top = 267
  end
end
