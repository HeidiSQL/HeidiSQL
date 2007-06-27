object MainForm: TMainForm
  Left = 207
  Top = 192
  BorderStyle = bsDialog
  Caption = 'Controls example'
  ClientHeight = 446
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 306
    Top = 3
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 46
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 363
      Top = 3
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label2: TLabel
      Left = 6
      Top = 3
      Width = 39
      Height = 13
      Caption = 'Protocol'
    end
    object Label3: TLabel
      Left = 432
      Top = 3
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label4: TLabel
      Left = 126
      Top = 3
      Width = 46
      Height = 13
      Caption = 'Database'
    end
    object SpeedButton1: TSpeedButton
      Left = 222
      Top = 18
      Width = 23
      Height = 22
      Caption = '...'
      Flat = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 501
      Top = 15
      Width = 64
      Height = 25
      Caption = 'Connect'
      Flat = True
      OnClick = SpeedButton2Click
    end
    object Label13: TLabel
      Left = 315
      Top = 3
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label14: TLabel
      Left = 246
      Top = 3
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object SpeedButton3: TSpeedButton
      Left = 567
      Top = 15
      Width = 67
      Height = 25
      Caption = 'Disconnect'
      Flat = True
      OnClick = SpeedButton3Click
    end
    object ZProtocol: TComboBox
      Left = 6
      Top = 18
      Width = 118
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'mysql-3.23'
      Items.Strings = (
        'mysql-3.20'
        'mysql-3.23'
        'mysql-4'
        'prostgresql'
        'prostgresql-7.2'
        'interbase-5'
        'interbase-6'
        'firebird-1.0'
        'firebird-1.5'
        'mssql'
        'sybase')
    end
    object ZPassword: TEdit
      Left = 432
      Top = 18
      Width = 67
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object ZDatabase: TEdit
      Left = 126
      Top = 18
      Width = 94
      Height = 21
      TabOrder = 1
      Text = 'zeoslib'
    end
    object ZUername: TEdit
      Left = 363
      Top = 18
      Width = 67
      Height = 21
      TabOrder = 2
      Text = 'root'
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 46
    Width = 688
    Height = 400
    ActivePage = TabSheet2
    Align = alClient
    TabIndex = 0
    TabOrder = 1
    object TabSheet2: TTabSheet
      Caption = 'Simple'
      ImageIndex = 1
      object DBGrid1: TDBGrid
        Left = 0
        Top = 30
        Width = 680
        Height = 342
        Align = alBottom
        DataSource = DSCargo
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBNavigator2: TDBNavigator
        Left = 0
        Top = 0
        Width = 240
        Height = 25
        TabOrder = 1
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Data Controls'
      object Label5: TLabel
        Left = 3
        Top = 168
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label6: TLabel
        Left = 57
        Top = 168
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Label7: TLabel
        Left = 3
        Top = 210
        Width = 87
        Height = 13
        Caption = 'Begin of work time'
      end
      object Label8: TLabel
        Left = 129
        Top = 210
        Width = 90
        Height = 13
        Caption = 'Finish of work time '
      end
      object Label9: TLabel
        Left = 3
        Top = 249
        Width = 39
        Height = 13
        Caption = 'Resume'
      end
      object Label10: TLabel
        Left = 459
        Top = 138
        Width = 28
        Height = 13
        Caption = 'Photo'
      end
      object Label11: TLabel
        Left = 258
        Top = 165
        Width = 55
        Height = 13
        Caption = 'Department'
      end
      object DBGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 680
        Height = 133
        Align = alTop
        DataSource = DSPeople
        ReadOnly = True
        TabOrder = 10
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBNavigator1: TDBNavigator
        Left = 3
        Top = 138
        Width = 240
        Height = 25
        DataSource = DSPeople
        TabOrder = 7
      end
      object DBId: TDBEdit
        Left = 3
        Top = 183
        Width = 49
        Height = 21
        DataField = 'p_id'
        DataSource = DSPeople
        TabOrder = 0
      end
      object DBName: TDBEdit
        Left = 57
        Top = 183
        Width = 196
        Height = 21
        DataField = 'p_name'
        DataSource = DSPeople
        TabOrder = 1
      end
      object DBBeginWorkTime: TDBEdit
        Left = 3
        Top = 225
        Width = 121
        Height = 21
        DataField = 'p_begin_work'
        DataSource = DSPeople
        TabOrder = 3
      end
      object DBResume: TDBMemo
        Left = 3
        Top = 264
        Width = 451
        Height = 109
        DataField = 'p_resume'
        DataSource = DSPeople
        TabOrder = 5
      end
      object DBPicture: TDBImage
        Left = 459
        Top = 153
        Width = 220
        Height = 219
        DataField = 'p_picture'
        DataSource = DSPeople
        TabOrder = 6
      end
      object DBEndWorkTime: TDBEdit
        Left = 129
        Top = 225
        Width = 121
        Height = 21
        DataField = 'p_end_work'
        DataSource = DSPeople
        TabOrder = 4
      end
      object LoadImageBtn: TButton
        Left = 336
        Top = 138
        Width = 75
        Height = 25
        Caption = 'Load Image'
        TabOrder = 9
        OnClick = LoadImageBtnClick
      end
      object LoadResumeBtn: TButton
        Left = 249
        Top = 138
        Width = 82
        Height = 25
        Caption = 'Load Resume'
        TabOrder = 8
        OnClick = LoadResumeBtnClick
      end
      object DBDepartment: TDBLookupComboBox
        Left = 258
        Top = 183
        Width = 145
        Height = 21
        DataField = 'p_dep_id'
        DataSource = DSPeople
        DropDownRows = 5
        KeyField = 'dep_id'
        ListField = 'dep_name'
        ListSource = DSDepartment
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Info'
      ImageIndex = 2
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object ZDBInfoType: TComboBox
          Left = 3
          Top = 3
          Width = 145
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          OnChange = ZDBInfoTypeChange
          Items.Strings = (
            'Procedures'
            'ProcedureColumns'
            'Tables'
            'Schemas'
            'Catalogs'
            'TableTypes'
            'Columns'
            'ColumnPrivileges'
            'TablePrivileges'
            'BestRowIdentifier'
            'VersionColumns'
            'PrimaryKeys'
            'ImportedKeys'
            'ExportedKeys'
            'CrossReference'
            'TypeInfo'
            'IndexInfo'
            'UserDefinedTypes')
        end
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 28
        Width = 680
        Height = 344
        Align = alClient
        DataSource = DSSQLMetadata
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'SQL Monitor'
      ImageIndex = 3
      object ZLogList: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 372
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object ZHost: TEdit
    Left = 246
    Top = 18
    Width = 67
    Height = 21
    TabOrder = 2
    Text = 'localhost'
  end
  object ZPort: TEdit
    Left = 315
    Top = 18
    Width = 46
    Height = 21
    TabOrder = 3
    Text = '(default)'
  end
  object ZConnection: TZConnection
    Protocol = 'mysql'
    Port = 0
    Database = 'zeoslib'
    User = 'root'
    AutoCommit = False
    ReadOnly = True
    TransactIsolationLevel = tiNone
    Connected = False
    Left = 30
    Top = 120
  end
  object ZPeople: TZQuery
    Connection = ZConnection
    RequestLive = True
    CachedUpdates = False
    SQL.Strings = (
      'select * from people')
    ParamCheck = True
    Params = <>
    ShowRecordTypes = [utModified, utInserted, utUnmodified]
    UpdateMode = umUpdateChanged
    WhereMode = wmWhereKeyOnly
    Options = [doCalcDefaults]
    Left = 477
    Top = 87
    object ZPeoplep_id: TSmallintField
      DisplayLabel = 'ID'
      FieldName = 'p_id'
      Required = True
    end
    object ZPeopledeprtment: TStringField
      FieldKind = fkLookup
      FieldName = 'deprtment'
      LookupDataSet = ZDepartment
      LookupKeyFields = 'dep_id'
      LookupResultField = 'dep_name'
      KeyFields = 'p_dep_id'
      Size = 32
      Lookup = True
    end
    object ZPeoplep_name: TStringField
      DisplayLabel = 'Name'
      FieldName = 'p_name'
      Size = 40
    end
    object ZPeoplep_begin_work: TTimeField
      DisplayLabel = 'Begin work'
      FieldName = 'p_begin_work'
    end
    object ZPeoplep_end_work: TTimeField
      DisplayLabel = 'End work'
      FieldName = 'p_end_work'
    end
    object ZPeoplep_picture: TBlobField
      DisplayLabel = 'Picture'
      FieldName = 'p_picture'
    end
    object ZPeoplep_resume: TMemoField
      DisplayLabel = 'Resume'
      FieldName = 'p_resume'
      BlobType = ftMemo
    end
    object ZPeoplep_redundant: TSmallintField
      DisplayLabel = 'Redundant'
      FieldName = 'p_redundant'
    end
    object ZPeoplep_dep_id: TSmallintField
      FieldName = 'p_dep_id'
      Visible = False
    end
  end
  object DSPeople: TDataSource
    DataSet = ZPeople
    Left = 543
    Top = 87
  end
  object ZDepartment: TZReadOnlyQuery
    Connection = ZConnection
    SQL.Strings = (
      'select * from department')
    ParamCheck = True
    Params = <>
    Options = [doCalcDefaults]
    Left = 477
    Top = 141
  end
  object DSDepartment: TDataSource
    DataSet = ZDepartment
    Left = 546
    Top = 138
  end
  object ZCargo: TZQuery
    Connection = ZConnection
    RequestLive = True
    CachedUpdates = False
    SQL.Strings = (
      'select * from cargo')
    ParamCheck = True
    Params = <>
    ShowRecordTypes = [utModified, utInserted, utUnmodified]
    UpdateMode = umUpdateChanged
    WhereMode = wmWhereKeyOnly
    Options = [doCalcDefaults]
    Left = 66
    Top = 153
  end
  object DSCargo: TDataSource
    DataSet = ZCargo
    Left = 102
    Top = 153
  end
  object ZSQLMetadata: TZSQLMetadata
    Connection = ZConnection
    MetadataType = mdProcedures
    Scope = 0
    Nullable = False
    Unique = False
    Approximate = False
    Left = 66
    Top = 120
  end
  object DSSQLMetadata: TDataSource
    DataSet = ZSQLMetadata
    Left = 102
    Top = 120
  end
  object ZSQLMonitor: TZSQLMonitor
    Active = True
    MaxTraceCount = 100
    OnTrace = ZSQLMonitorTrace
    Left = 66
    Top = 186
  end
end
