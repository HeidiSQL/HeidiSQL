object frmMain: TfrmMain
  Left = 205
  Top = 116
  Caption = 'ZeosDBO Simple Test'
  ClientHeight = 771
  ClientWidth = 894
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 690
    Height = 771
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splMain: TSplitter
      Left = 0
      Top = 121
      Width = 690
      Height = 8
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 528
    end
    object splLog: TSplitter
      Left = 0
      Top = 649
      Width = 690
      Height = 8
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 446
      ExplicitWidth = 528
    end
    object navMain: TDBNavigator
      Left = 0
      Top = 129
      Width = 690
      Height = 32
      DataSource = dsMain
      Align = alTop
      TabOrder = 0
    end
    object gdMain: TDBGrid
      Left = 0
      Top = 161
      Width = 690
      Height = 488
      Align = alClient
      DataSource = dsMain
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object memLog: TMemo
      Left = 0
      Top = 657
      Width = 690
      Height = 114
      Align = alBottom
      ReadOnly = True
      TabOrder = 2
    end
    object pcStatements: TPageControl
      Left = 0
      Top = 0
      Width = 690
      Height = 121
      ActivePage = tshQuery
      Align = alTop
      TabOrder = 3
      object tshQuery: TTabSheet
        Caption = ' &Query     '
        object memQuery: TMemo
          Left = 0
          Top = 0
          Width = 682
          Height = 93
          Align = alClient
          Lines.Strings = (
            'SELECT * FROM TSITE;')
          TabOrder = 0
          OnChange = PropertiesChange
          ExplicitLeft = -2
          ExplicitTop = -2
        end
      end
      object tshInsert: TTabSheet
        Caption = '&Insert     '
        ImageIndex = 1
        object memInsert: TMemo
          Left = 0
          Top = 0
          Width = 682
          Height = 93
          Align = alClient
          Lines.Strings = (
            '')
          TabOrder = 0
          OnChange = PropertiesChange
        end
      end
      object tshUpdate: TTabSheet
        Caption = '&Update    '
        ImageIndex = 2
        object memUpdate: TMemo
          Left = 0
          Top = 0
          Width = 682
          Height = 93
          Align = alClient
          TabOrder = 0
          OnChange = PropertiesChange
        end
      end
      object tshDelete: TTabSheet
        Caption = '&Delete     '
        ImageIndex = 3
        object memDelete: TMemo
          Left = 0
          Top = 0
          Width = 682
          Height = 93
          Align = alClient
          Lines.Strings = (
            '')
          TabOrder = 0
          OnChange = PropertiesChange
        end
      end
    end
  end
  object pnControl: TPanel
    Left = 690
    Top = 0
    Width = 204
    Height = 771
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object lblProtocol: TLabel
      Left = 8
      Top = 8
      Width = 42
      Height = 13
      Caption = 'Protocol:'
    end
    object lblHostName: TLabel
      Left = 8
      Top = 50
      Width = 56
      Height = 13
      Caption = 'Host Name:'
    end
    object lblDatabase: TLabel
      Left = 8
      Top = 92
      Width = 49
      Height = 13
      Caption = 'Database:'
    end
    object lblUserName: TLabel
      Left = 8
      Top = 134
      Width = 56
      Height = 13
      Caption = 'User Name:'
    end
    object lblPassword: TLabel
      Left = 8
      Top = 176
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object cbxProtocol: TComboBox
      Left = 8
      Top = 24
      Width = 193
      Height = 21
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 0
      Text = 'postgresql'
      OnChange = PropertiesChange
      Items.Strings = (
        'mysql'
        'postgresql'
        'postgresql-7.2'
        'interbase-5'
        'interbase-6'
        'firebird-1.0'
        'firebird-1.5'
        'mssql'
        'sybase'
        'oracle'
        'db2')
    end
    object edtHostName: TEdit
      Left = 8
      Top = 66
      Width = 193
      Height = 21
      TabOrder = 1
      Text = '10.0.0.10'
      OnChange = PropertiesChange
    end
    object edtDatabase: TEdit
      Left = 8
      Top = 108
      Width = 193
      Height = 21
      TabOrder = 2
      Text = 'firmtest'
      OnChange = PropertiesChange
    end
    object edtUserName: TEdit
      Left = 8
      Top = 150
      Width = 193
      Height = 21
      TabOrder = 3
      Text = 'firmos'
      OnChange = PropertiesChange
    end
    object edtPassword: TEdit
      Left = 8
      Top = 192
      Width = 193
      Height = 21
      TabOrder = 4
      Text = 'gimmehard'
      OnChange = PropertiesChange
    end
    object btnConnect: TButton
      Left = 8
      Top = 224
      Width = 193
      Height = 25
      Caption = '&Connect'
      TabOrder = 5
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 8
      Top = 256
      Width = 193
      Height = 25
      Caption = '&Disconnect'
      TabOrder = 6
      OnClick = btnDisconnectClick
    end
    object btnOpen: TButton
      Left = 6
      Top = 297
      Width = 193
      Height = 25
      Caption = '&Open'
      TabOrder = 7
      OnClick = btnOpenClick
    end
    object btnClose: TButton
      Left = 8
      Top = 328
      Width = 193
      Height = 25
      Caption = '&Close'
      TabOrder = 8
      OnClick = btnCloseClick
    end
    object btnExecute: TButton
      Left = 8
      Top = 368
      Width = 193
      Height = 25
      Caption = '&Execute'
      TabOrder = 9
      OnClick = btnExecuteClick
    end
    object btnPrint: TButton
      Left = 8
      Top = 464
      Width = 193
      Height = 25
      Caption = '&Print ResultSet'
      TabOrder = 10
      OnClick = btnPrintClick
    end
    object btnFilter: TButton
      Left = 8
      Top = 400
      Width = 193
      Height = 25
      Caption = 'Switch &Filter'
      TabOrder = 11
      OnClick = btnFilterClick
    end
    object btnLocate: TButton
      Left = 8
      Top = 432
      Width = 193
      Height = 25
      Caption = '&Locate'
      TabOrder = 12
      OnClick = btnLocateClick
    end
    object btnApplyUpdates: TButton
      Left = 8
      Top = 504
      Width = 193
      Height = 25
      Caption = '&Apply Updates'
      TabOrder = 13
      OnClick = btnApplyUpdatesClick
    end
    object btnCancelUpdates: TButton
      Left = 8
      Top = 536
      Width = 193
      Height = 25
      Caption = '&Cancel Updates'
      TabOrder = 14
      OnClick = btnCancelUpdatesClick
    end
    object ButtonRefresh: TButton
      Left = 6
      Top = 608
      Width = 193
      Height = 25
      Caption = 'Refresh'
      TabOrder = 15
      OnClick = ButtonRefreshClick
    end
  end
  object dsMain: TDataSource
    Left = 16
    Top = 64
  end
  object ZSQLMonitor: TZSQLMonitor
    Active = True
    MaxTraceCount = 100
    OnTrace = ZSQLMonitorTrace
    OnLogTrace = ZSQLMonitorLogTrace
    Left = 176
    Top = 296
  end
  object ZQueryZ: TZQuery
    Params = <>
    Left = 208
    Top = 384
  end
  object ZSequence: TZSequence
    Left = 344
    Top = 352
  end
end
