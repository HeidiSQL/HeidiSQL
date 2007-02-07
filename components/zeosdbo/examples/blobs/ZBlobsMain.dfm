object frmMain: TfrmMain
  Left = 195
  Top = 110
  Width = 738
  Height = 643
  HorzScrollBar.Range = 202
  ActiveControl = cbxProtocol
  BorderStyle = bsSingle
  Caption = 'ZeosDBO Blobs Test'
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnControl: TPanel
    Left = 530
    Top = 0
    Width = 202
    Height = 616
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object lblProtocol: TLabel
      Left = 4
      Top = 8
      Width = 42
      Height = 13
      Caption = 'Protocol:'
    end
    object lblHostName: TLabel
      Left = 4
      Top = 65
      Width = 56
      Height = 13
      Caption = 'Host Name:'
    end
    object lblDatabase: TLabel
      Left = 4
      Top = 107
      Width = 49
      Height = 13
      Caption = 'Database:'
    end
    object lblUserName: TLabel
      Left = 4
      Top = 149
      Width = 56
      Height = 13
      Caption = 'User Name:'
    end
    object lblPassword: TLabel
      Left = 4
      Top = 191
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object lblTableName: TLabel
      Left = 7
      Top = 233
      Width = 61
      Height = 13
      Caption = 'Table Name:'
    end
    object lblMemoColumn: TLabel
      Left = 8
      Top = 275
      Width = 70
      Height = 13
      Caption = 'Memo Column:'
    end
    object lblBlobColumn: TLabel
      Left = 9
      Top = 316
      Width = 62
      Height = 13
      Caption = 'Blob Column:'
    end
    object cbxProtocol: TComboBox
      Left = 4
      Top = 24
      Width = 193
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'postgresql'
      OnChange = PropertiesChange
      Items.Strings = (
        'db2'
        'firebird-1.0'
        'firebird-1.5'
        'firebird-2.0'
        'interbase-5'
        'interbase-6'
        'mssql'
        'mysql'
        'mysql-4.0'
        'mysql-4.1'
        'oracle'
        'postgresql'
        'postgresql-8.0'
        'postgresql-8.1'
        'sqlite-2.8'
        'sqlite-3'
        'sybase')
    end
    object edtHostName: TEdit
      Left = 4
      Top = 81
      Width = 193
      Height = 21
      TabOrder = 1
      Text = 'localhost'
      OnChange = PropertiesChange
    end
    object edtDatabase: TEdit
      Left = 4
      Top = 123
      Width = 193
      Height = 21
      TabOrder = 2
      Text = 'zeoslib'
      OnChange = PropertiesChange
    end
    object edtUserName: TEdit
      Left = 4
      Top = 165
      Width = 193
      Height = 21
      TabOrder = 3
      Text = 'root'
      OnChange = PropertiesChange
    end
    object edtPassword: TEdit
      Left = 4
      Top = 207
      Width = 193
      Height = 21
      TabOrder = 4
      OnChange = PropertiesChange
    end
    object btnConnect: TButton
      Left = 5
      Top = 371
      Width = 193
      Height = 25
      Caption = '&Connect'
      TabOrder = 6
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 5
      Top = 403
      Width = 193
      Height = 25
      Caption = '&Disconnect'
      TabOrder = 8
      OnClick = btnDisconnectClick
    end
    object btnOpen: TButton
      Left = 5
      Top = 443
      Width = 193
      Height = 25
      Caption = '&Open'
      TabOrder = 10
      OnClick = btnOpenClick
    end
    object btnClose: TButton
      Left = 5
      Top = 475
      Width = 193
      Height = 25
      Caption = '&Close'
      TabOrder = 12
      OnClick = btnCloseClick
    end
    object btnApplyUpdates: TButton
      Left = 5
      Top = 514
      Width = 193
      Height = 25
      Caption = '&Save Data'
      TabOrder = 14
      OnClick = btnApplyUpdatesClick
    end
    object btnCancelUpdates: TButton
      Left = 6
      Top = 546
      Width = 193
      Height = 25
      Caption = '&Cancel Updates'
      Enabled = False
      TabOrder = 5
      OnClick = btnCancelUpdatesClick
    end
    object edtTableName: TEdit
      Left = 4
      Top = 249
      Width = 193
      Height = 21
      TabOrder = 7
      Text = 'blob_values'
      OnChange = PropertiesChange
    end
    object edtMemoColumn: TEdit
      Left = 5
      Top = 291
      Width = 193
      Height = 21
      TabOrder = 9
      Text = 'b_text'
      OnChange = PropertiesChange
    end
    object edtBlobColumn: TEdit
      Left = 6
      Top = 332
      Width = 193
      Height = 21
      TabOrder = 11
      Text = 'b_image'
      OnChange = PropertiesChange
    end
    object btnLoadImage: TButton
      Left = 5
      Top = 586
      Width = 193
      Height = 25
      Caption = '&Load Image'
      TabOrder = 13
      OnClick = btnLoadImageClick
    end
    object cbxOidAsBlob: TCheckBox
      Left = 112
      Top = 49
      Width = 82
      Height = 17
      Caption = 'OID as BLOB'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 15
      OnClick = cbxOidAsBlobClick
    end
  end
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 530
    Height = 616
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object splMain: TSplitter
      Left = 0
      Top = 368
      Width = 530
      Height = 9
      Cursor = crVSplit
      Align = alBottom
    end
    object navMain: TDBNavigator
      Left = 0
      Top = 0
      Width = 530
      Height = 27
      DataSource = dsMain
      Align = alTop
      TabOrder = 0
    end
    object gdMain: TDBGrid
      Left = 0
      Top = 27
      Width = 530
      Height = 341
      Align = alClient
      DataSource = dsMain
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object pnDetail: TPanel
      Left = 0
      Top = 377
      Width = 530
      Height = 239
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object splDetail: TSplitter
        Left = 257
        Top = 0
        Width = 9
        Height = 239
      end
      object memText: TDBMemo
        Left = 0
        Top = 0
        Width = 257
        Height = 239
        Align = alLeft
        DataField = 'b_text'
        DataSource = dsMain
        TabOrder = 0
      end
      object imgBlob: TDBImage
        Left = 266
        Top = 0
        Width = 264
        Height = 239
        Align = alClient
        DataField = 'b_image'
        DataSource = dsMain
        Stretch = True
        TabOrder = 1
        TabStop = False
      end
    end
  end
  object dsMain: TDataSource
    Left = 16
    Top = 80
  end
  object dlgOpenFile: TOpenDialog
    Title = 'Open'
    Left = 48
    Top = 80
  end
end
