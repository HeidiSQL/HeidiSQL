unit dbconnection;

interface

uses
  Classes, SysUtils, windows, mysql_structures, SynRegExpr, Contnrs, Generics.Collections, Generics.Defaults,
  DateUtils, Types, ShellApi, Math, Dialogs, ADODB, DB, DBCommon, ComObj;


type
  { TDBObjectList and friends }

  TListNodeType = (lntNone, lntDb, lntTable, lntView, lntFunction, lntProcedure, lntTrigger, lntEvent, lntColumn);
  TListNodeTypes = Set of TListNodeType;
  TDBConnection = class;
  TDBQuery = class;
  TDBQueryList = TObjectList<TDBQuery>;
  TDBObject = class(TPersistent)
    private
      FCreateCode: String;
      FViewSelectCode: String;
      FCreateCodeFetched: Boolean;
      FConnection: TDBConnection;
      function GetObjType: String;
      function GetImageIndex: Integer;
      function GetCreateCode: String;
      procedure SetCreateCode(Value: String);
    public
      Name, Database, Column, Engine, Comment, RowFormat, CreateOptions, Collation: String;
      Created, Updated, LastChecked: TDateTime;
      Rows, Size, Version, AvgRowLen, MaxDataLen, IndexLen, DataLen, DataFree, AutoInc, CheckSum: Int64;
      NodeType: TListNodeType;
      constructor Create(OwnerConnection: TDBConnection);
      procedure Assign(Source: TPersistent); override;
      function IsSameAs(CompareTo: TDBObject): Boolean;
      function QuotedDatabase(AlwaysQuote: Boolean=True): String;
      function QuotedName(AlwaysQuote: Boolean=True): String;
      function QuotedColumn(AlwaysQuote: Boolean=True): String;
      property ObjType: String read GetObjType;
      property ImageIndex: Integer read GetImageIndex;
      property CreateCode: String read GetCreateCode write SetCreateCode;
      property ViewSelectCode: String read FViewSelectCode;
      property Connection: TDBConnection read FConnection;
  end;
  PDBObject = ^TDBObject;
  TDBObjectList = class(TObjectList<TDBObject>)
    private
      FDatabase: String;
      FDataSize: Int64;
      FLargestObjectSize: Int64;
      FLastUpdate: TDateTime;
      FCollation: String;
    public
      property Database: String read FDatabase;
      property DataSize: Int64 read FDataSize;
      property LargestObjectSize: Int64 read FLargestObjectSize;
      property LastUpdate: TDateTime read FLastUpdate;
      property Collation: String read FCollation;
  end;
  TDatabaseList = TObjectList<TDBObjectList>; // A list of db object lists, used for caching
  TDBObjectComparer = class(TComparer<TDBObject>)
    function Compare(const Left, Right: TDBObject): Integer; override;
  end;
  TDBObjectDropComparer = class(TComparer<TDBObject>)
    function Compare(const Left, Right: TDBObject): Integer; override;
  end;

  // General purpose editing status flag
  TEditingStatus = (esUntouched, esModified, esDeleted, esAddedUntouched, esAddedModified, esAddedDeleted);

  TColumnDefaultType = (cdtNothing, cdtText, cdtTextUpdateTS, cdtNull, cdtNullUpdateTS, cdtCurTS, cdtCurTSUpdateTS, cdtAutoInc);

  // Column object, many of them in a TObjectList
  TTableColumn = class(TObject)
    private
      FConnection: TDBConnection;
      procedure SetStatus(Value: TEditingStatus);
    public
      Name, OldName: String;
      DataType, OldDataType: TDBDatatype;
      LengthSet: String;
      Unsigned, AllowNull, ZeroFill, LengthCustomized: Boolean;
      DefaultType: TColumnDefaultType;
      DefaultText: String;
      Comment, Charset, Collation, Expression, Virtuality: String;
      FStatus: TEditingStatus;
      constructor Create(AOwner: TDBConnection);
      destructor Destroy; override;
      function SQLCode: String;
      property Status: TEditingStatus read FStatus write SetStatus;
  end;
  PTableColumn = ^TTableColumn;
  TTableColumnList = TObjectList<TTableColumn>;

  TTableKey = class(TObject)
    private
      FConnection: TDBConnection;
    public
      Name, OldName: String;
      IndexType, OldIndexType, Algorithm: String;
      Columns, SubParts: TStringList;
      Modified, Added: Boolean;
      constructor Create(AOwner: TDBConnection);
      destructor Destroy; override;
      procedure Modification(Sender: TObject);
      function SQLCode: String;
  end;
  TTableKeyList = TObjectList<TTableKey>;

  // Helper object to manage foreign keys in a TObjectList
  TForeignKey = class(TObject)
    private
      FConnection: TDBConnection;
    public
      KeyName, OldKeyName, ReferenceTable, OnUpdate, OnDelete: String;
      Columns, ForeignColumns: TStringList;
      Modified, Added, KeyNameWasCustomized: Boolean;
      constructor Create(AOwner: TDBConnection);
      destructor Destroy; override;
      function SQLCode(IncludeSymbolName: Boolean): String;
  end;
  TForeignKeyList = TObjectList<TForeignKey>;

  TRoutineParam = class(TObject)
    public
      Name, Context, Datatype: String;
  end;
  TRoutineParamList = TObjectList<TRoutineParam>;

  // Structures for in-memory changes of a TMySQLQuery
  TCellData = class(TObject)
    NewText, OldText: String;
    NewIsNull, OldIsNull: Boolean;
    Modified: Boolean;
    destructor Destroy; override;
  end;
  TRowData = class(TObjectList<TCellData>)
    RecNo: Int64;
    Inserted: Boolean;
  end;
  TUpdateData = TObjectList<TRowData>;

  // Custom exception class for any connection or database related error
  EDatabaseError = class(Exception);

  {$M+} // Needed to add published properties

  { TConnectionParameters and friends }

  TNetType = (ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel,
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP, ntMSSQL_SPX, ntMSSQL_VINES, ntMSSQL_RPC);
  TNetTypeGroup = (ngMySQL, ngMSSQL);

  TConnectionParameters = class(TObject)
    strict private
      FNetType: TNetType;
      FHostname, FUsername, FPassword, FAllDatabases, FStartupScriptFilename,
      FSessionName, FSSLPrivateKey, FSSLCertificate, FSSLCACertificate, FServerVersion,
      FSSHHost, FSSHUser, FSSHPassword, FSSHPlinkExe, FSSHPrivateKey: String;
      FPort, FSSHPort, FSSHLocalPort, FSSHTimeout: Integer;
      FLoginPrompt, FCompressed, FWindowsAuth: Boolean;
      function GetImageIndex: Integer;
    public
      constructor Create;
      function CreateConnection(AOwner: TComponent): TDBConnection;
      function CreateQuery(AOwner: TComponent): TDBQuery;
      function NetTypeName(NetType: TNetType; LongFormat: Boolean): String;
      function GetNetTypeGroup: TNetTypeGroup;
      function IsMariaDB: Boolean;
      function IsPercona: Boolean;
      property ImageIndex: Integer read GetImageIndex;
    published
      property NetType: TNetType read FNetType write FNetType;
      property NetTypeGroup: TNetTypeGroup read GetNetTypeGroup;
      property ServerVersion: String read FServerVersion write FServerVersion;
      property SessionName: String read FSessionName write FSessionName;
      property Hostname: String read FHostname write FHostname;
      property Port: Integer read FPort write FPort;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
      property WindowsAuth: Boolean read FWindowsAuth write FWindowsAuth;
      property AllDatabasesStr: String read FAllDatabases write FAllDatabases;
      property StartupScriptFilename: String read FStartupScriptFilename write FStartupScriptFilename;
      property Compressed: Boolean read FCompressed write FCompressed;
      property SSHHost: String read FSSHHost write FSSHHost;
      property SSHPort: Integer read FSSHPort write FSSHPort;
      property SSHUser: String read FSSHUser write FSSHUser;
      property SSHPassword: String read FSSHPassword write FSSHPassword;
      property SSHTimeout: Integer read FSSHTimeout write FSSHTimeout;
      property SSHPrivateKey: String read FSSHPrivateKey write FSSHPrivateKey;
      property SSHLocalPort: Integer read FSSHLocalPort write FSSHLocalPort;
      property SSHPlinkExe: String read FSSHPlinkExe write FSSHPlinkExe;
      property SSLPrivateKey: String read FSSLPrivateKey write FSSLPrivateKey;
      property SSLCertificate: String read FSSLCertificate write FSSLCertificate;
      property SSLCACertificate: String read FSSLCACertificate write FSSLCACertificate;
  end;
  PConnectionParameters = ^TConnectionParameters;


  { TDBConnection }

  TDBLogCategory = (lcInfo, lcSQL, lcUserFiredSQL, lcError, lcDebug);
  TDBLogEvent = procedure(Msg: String; Category: TDBLogCategory=lcInfo; Connection: TDBConnection=nil) of object;
  TDBLogItem = class(TObject)
    public
      Msg: String;
      Category: TDBLogCategory;
  end;
  TDBLogQueue = TObjectList<TDBLogItem>;
  TDBEvent = procedure(Connection: TDBConnection; Database: String) of object;
  TDBDataTypeArray = Array of TDBDataType;

  TDBConnection = class(TComponent)
    private
      FActive: Boolean;
      FConnectionStarted: Integer;
      FServerStarted: Integer;
      FParameters: TConnectionParameters;
      FLoginPromptDone: Boolean;
      FDatabase: String;
      FAllDatabases: TStringList;
      FLogPrefix: String;
      FLogQueue: TDBLogQueue;
      FOnLog: TDBLogEvent;
      FOnConnected: TDBEvent;
      FOnDatabaseChanged: TDBEvent;
      FOnDBObjectsCleared: TDBEvent;
      FRowsFound: Int64;
      FRowsAffected: Int64;
      FServerOS: String;
      FServerVersionUntouched: String;
      FRealHostname: String;
      FLastQueryDuration, FLastQueryNetworkDuration: Cardinal;
      FLastQuerySQL: String;
      FIsUnicode: Boolean;
      FIsSSL: Boolean;
      FTableEngines: TStringList;
      FTableEngineDefault: String;
      FCollationTable: TDBQuery;
      FCharsetTable: TDBQuery;
      FInformationSchemaObjects: TStringList;
      FDatabases: TDatabaseList;
      FObjectNamesInSelectedDB: TStrings;
      FResultCount: Integer;
      FCurrentUserHostCombination: String;
      FLockedByThread: TThread;
      FQuoteChar: Char;
      FDatatypes: TDBDataTypeArray;
      FThreadID: Cardinal;
      procedure SetActive(Value: Boolean); virtual; abstract;
      procedure DoBeforeConnect; virtual;
      procedure DoAfterConnect;
      procedure SetDatabase(Value: String);
      function GetThreadId: Cardinal; virtual; abstract;
      function GetCharacterSet: String; virtual; abstract;
      procedure SetCharacterSet(CharsetName: String); virtual; abstract;
      function GetLastErrorCode: Cardinal; virtual; abstract;
      function GetLastError: String; virtual; abstract;
      function GetServerVersionStr: String;
      function GetServerVersionInt: Integer; virtual; abstract;
      function GetAllDatabases: TStringList; virtual;
      function GetTableEngines: TStringList; virtual;
      function GetCollationTable: TDBQuery; virtual;
      function GetCollationList: TStringList;
      function GetCharsetTable: TDBQuery; virtual;
      function GetCharsetList: TStringList;
      function GetInformationSchemaObjects: TStringList; virtual;
      function GetConnectionUptime: Integer;
      function GetServerUptime: Integer;
      function GetCurrentUserHostCombination: String;
      function DecodeAPIString(a: AnsiString): String;
      procedure ClearCache(IncludeDBObjects: Boolean);
      procedure SetObjectNamesInSelectedDB;
      procedure SetLockedByThread(Value: TThread); virtual;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); virtual; abstract;
      procedure Log(Category: TDBLogCategory; Msg: String);
      function EscapeString(Text: String; ProcessJokerChars: Boolean=False; DoQuote: Boolean=True): String;
      function QuoteIdent(Identifier: String; AlwaysQuote: Boolean=True; Glue: Char=#0): String;
      function DeQuoteIdent(Identifier: String; Glue: Char=#0): String;
      function escChars(const Text: String; EscChar, Char1, Char2, Char3, Char4: Char): String;
      function UnescapeString(Text: String): String;
      function ConvertServerVersion(Version: Integer): String; virtual; abstract;
      function GetResults(SQL: String): TDBQuery;
      function GetCol(SQL: String; Column: Integer=0): TStringList;
      function GetVar(SQL: String; Column: Integer=0): String; overload;
      function GetVar(SQL: String; Column: String): String; overload;
      function Ping(Reconnect: Boolean): Boolean; virtual; abstract;
      function RefreshAllDatabases: TStringList;
      function GetDBObjects(db: String; Refresh: Boolean=False): TDBObjectList; virtual; abstract;
      function DbObjectsCached(db: String): Boolean;
      function ParseDateTime(Str: String): TDateTime;
      function GetKeyColumns(Columns: TTableColumnList; Keys: TTableKeyList): TStringList;
      function ConnectionInfo: TStringList;
      function GetLastResults: TDBQueryList; virtual; abstract;
      function GetCreateCode(Database, Name: String; NodeType: TListNodeType): String; virtual; abstract;
      function GetServerVariables: TDBQuery; virtual; abstract;
      procedure ClearDbObjects(db: String);
      procedure ClearAllDbObjects;
      procedure ParseTableStructure(CreateTable: String; Columns: TTableColumnList; Keys: TTableKeyList; ForeignKeys: TForeignKeyList);
      procedure ParseViewStructure(CreateCode, ViewName: String; Columns: TTableColumnList; var Algorithm, Definer, CheckOption, SelectCode: String);
      procedure ParseRoutineStructure(CreateCode: String; Parameters: TRoutineParamList;
        var Deterministic: Boolean; var Definer, Returns, DataAccess, Security, Comment, Body: String);
      function GetDatatypeByName(Datatype: String): TDBDatatype;
      function ApplyLimitClause(QueryType, QueryBody: String; Limit, Offset: Cardinal): String;
      property Parameters: TConnectionParameters read FParameters write FParameters;
      property ThreadId: Cardinal read GetThreadId;
      property ConnectionUptime: Integer read GetConnectionUptime;
      property ServerUptime: Integer read GetServerUptime;
      property CharacterSet: String read GetCharacterSet write SetCharacterSet;
      property LastErrorCode: Cardinal read GetLastErrorCode;
      property LastError: String read GetLastError;
      property ServerOS: String read FServerOS;
      property ServerVersionUntouched: String read FServerVersionUntouched;
      property ServerVersionStr: String read GetServerVersionStr;
      property ServerVersionInt: Integer read GetServerVersionInt;
      property RowsFound: Int64 read FRowsFound;
      property RowsAffected: Int64 read FRowsAffected;
      property LastQueryDuration: Cardinal read FLastQueryDuration;
      property LastQueryNetworkDuration: Cardinal read FLastQueryNetworkDuration;
      property IsUnicode: Boolean read FIsUnicode;
      property IsSSL: Boolean read FIsSSL;
      property AllDatabases: TStringList read GetAllDatabases;
      property TableEngines: TStringList read GetTableEngines;
      property TableEngineDefault: String read FTableEngineDefault;
      property CollationTable: TDBQuery read GetCollationTable;
      property CollationList: TStringList read GetCollationList;
      property CharsetTable: TDBQuery read GetCharsetTable;
      property CharsetList: TStringList read GetCharsetList;
      property InformationSchemaObjects: TStringList read GetInformationSchemaObjects;
      property ObjectNamesInSelectedDB: TStrings read FObjectNamesInSelectedDB write FObjectNamesInSelectedDB;
      property ResultCount: Integer read FResultCount;
      property CurrentUserHostCombination: String read GetCurrentUserHostCombination;
      property LockedByThread: TThread read FLockedByThread write SetLockedByThread;
      property Datatypes: TDBDataTypeArray read FDatatypes;
      property LogQueue: TDBLogQueue read FLogQueue;
    published
      property Active: Boolean read FActive write SetActive default False;
      property Database: String read FDatabase write SetDatabase;
      property LogPrefix: String read FLogPrefix write FLogPrefix;
      property OnLog: TDBLogEvent read FOnLog write FOnLog;
      property OnConnected: TDBEvent read FOnConnected write FOnConnected;
      property OnDatabaseChanged: TDBEvent read FOnDatabaseChanged write FOnDatabaseChanged;
      property OnDBObjectsCleared: TDBEvent read FOnDBObjectsCleared write FOnDBObjectsCleared;
  end;
  TDBConnectionList = TObjectList<TDBConnection>;


  { TMySQLConnection }

  TMySQLRawResults = Array of PMYSQL_RES;
  TMySQLConnection = class(TDBConnection)
    private
      FHandle: PMYSQL;
      FLastRawResults: TMySQLRawResults;
      FPlinkProcInfo: TProcessInformation;
      procedure SetActive(Value: Boolean); override;
      procedure DoBeforeConnect; override;
      procedure AssignProc(var Proc: FARPROC; Name: PAnsiChar);
      procedure ClosePlink;
      function GetThreadId: Cardinal; override;
      function GetCharacterSet: String; override;
      procedure SetCharacterSet(CharsetName: String); override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastError: String; override;
      function GetServerVersionInt: Integer; override;
      function GetAllDatabases: TStringList; override;
      function GetTableEngines: TStringList; override;
      function GetCollationTable: TDBQuery; override;
      function GetCharsetTable: TDBQuery; override;
      procedure SetLockedByThread(Value: TThread); override;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function ConvertServerVersion(Version: Integer): String; override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function GetDBObjects(db: String; Refresh: Boolean=False): TDBObjectList; override;
      function GetLastResults: TDBQueryList; override;
      function GetCreateCode(Database, Name: String; NodeType: TListNodeType): String; override;
      property LastRawResults: TMySQLRawResults read FLastRawResults;
      function GetServerVariables: TDBQuery; override;
  end;

  TAdoRawResults = Array of _RecordSet;
  TAdoDBConnection = class(TDBConnection)
    private
      FAdoHandle: TAdoConnection;
      FLastRawResults: TAdoRawResults;
      FLastError: String;
      procedure SetActive(Value: Boolean); override;
      function GetThreadId: Cardinal; override;
      function GetCharacterSet: String; override;
      procedure SetCharacterSet(CharsetName: String); override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastError: String; override;
      function GetServerVersionInt: Integer; override;
      function GetAllDatabases: TStringList; override;
      function GetCollationTable: TDBQuery; override;
      function GetCharsetTable: TDBQuery; override;
      function GetInformationSchemaObjects: TStringList; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function ConvertServerVersion(Version: Integer): String; override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function GetDBObjects(db: String; Refresh: Boolean=False): TDBObjectList; override;
      function GetLastResults: TDBQueryList; override;
      function GetCreateCode(Database, Name: String; NodeType: TListNodeType): String; override;
      function GetServerVariables: TDBQuery; override;
      property LastRawResults: TAdoRawResults read FLastRawResults;
  end;


  { TDBQuery }

  TDBQuery = class(TComponent)
    private
      FSQL: String;
      FConnection: TDBConnection;
      FRecNo,
      FRecordCount: Int64;
      FColumnNames: TStringList;
      FColumnOrgNames: TStringList;
      FColumnTypes: Array of TDBDatatype;
      FColumnLengths: TIntegerDynArray;
      FColumnFlags: TCardinalDynArray;
      FCurrentUpdateRow: TRowData;
      FEof: Boolean;
      FStoreResult: Boolean;
      FColumns: TTableColumnList;
      FKeys: TTableKeyList;
      FForeignKeys: TForeignKeyList;
      FEditingPrepared: Boolean;
      FUpdateData: TUpdateData;
      procedure SetRecNo(Value: Int64); virtual; abstract;
      procedure SetColumnOrgNames(Value: TStringList);
      procedure CreateUpdateRow;
      function GetKeyColumns: TStringList;
      function GetWhereClause: String;
      function ColAttributes(Column: Integer): TTableColumn;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; UseRawResult: Integer=-1); virtual; abstract;
      procedure First;
      procedure Next;
      function ColumnCount: Integer;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload; virtual; abstract;
      function Col(ColumnName: String; IgnoreErrors: Boolean=False): String; overload;
      function ColumnLengths(Column: Integer): Int64; virtual;
      function HexValue(Column: Integer; IgnoreErrors: Boolean=False): String; overload;
      function HexValue(BinValue: String): String; overload;
      function DataType(Column: Integer): TDBDataType;
      function MaxLength(Column: Integer): Int64;
      function ValueList(Column: Integer): TStringList;
      function ColExists(Column: String): Boolean;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean; virtual; abstract;
      function ColIsUniqueKeyPart(Column: Integer): Boolean; virtual; abstract;
      function ColIsKeyPart(Column: Integer): Boolean; virtual; abstract;
      function IsNull(Column: Integer): Boolean; overload; virtual; abstract;
      function IsNull(Column: String): Boolean; overload;
      function HasResult: Boolean; virtual; abstract;
      procedure CheckEditable;
      function DeleteRow: Boolean;
      function InsertRow: Cardinal;
      procedure SetCol(Column: Integer; NewText: String; Null: Boolean);
      function EnsureFullRow: Boolean;
      function HasFullData: Boolean;
      function Modified(Column: Integer): Boolean; overload;
      function Modified: Boolean; overload;
      function Inserted: Boolean;
      function SaveModifications: Boolean;
      function DatabaseName: String; virtual; abstract;
      function TableName: String; virtual; abstract;
      function QuotedDbAndTableName: String;
      procedure DiscardModifications;
      procedure PrepareEditing;
      property RecNo: Int64 read FRecNo write SetRecNo;
      property Eof: Boolean read FEof;
      property RecordCount: Int64 read FRecordCount;
      property ColumnNames: TStringList read FColumnNames;
      property StoreResult: Boolean read FStoreResult write FStoreResult;
      property ColumnOrgNames: TStringList read FColumnOrgNames write SetColumnOrgNames;
    published
      property SQL: String read FSQL write FSQL;
      property Connection: TDBConnection read FConnection write FConnection;
  end;
  PDBQuery = ^TDBQuery;

  { TMySQLQuery }

  TMySQLQuery = class(TDBQuery)
    private
      FResultList: TMySQLRawResults;
      FCurrentResults: PMYSQL_RES;
      FCurrentRow: PMYSQL_ROW;
      procedure SetRecNo(Value: Int64); override;
    public
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; UseRawResult: Integer=-1); override;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload; override;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean; override;
      function ColIsUniqueKeyPart(Column: Integer): Boolean; override;
      function ColIsKeyPart(Column: Integer): Boolean; override;
      function IsNull(Column: Integer): Boolean; overload; override;
      function HasResult: Boolean; override;
      function DatabaseName: String; override;
      function TableName: String; override;
  end;

  TAdoDBQuery = class(TDBQuery)
    private
      FCurrentResults: TAdoQuery;
      FResultList: Array of TAdoQuery;
      procedure SetRecNo(Value: Int64); override;
    public
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; UseRawResult: Integer=-1); override;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload; override;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean; override;
      function ColIsUniqueKeyPart(Column: Integer): Boolean; override;
      function ColIsKeyPart(Column: Integer): Boolean; override;
      function IsNull(Column: Integer): Boolean; overload; override;
      function HasResult: Boolean; override;
      function DatabaseName: String; override;
      function TableName: String; override;
  end;

function mysql_authentication_dialog_ask(
    Handle: PMYSQL;
    _type: Integer;
    prompt: PAnsiChar;
    buf: PAnsiChar;
    buf_len: Integer
    ): PAnsiChar; cdecl;

exports
  mysql_authentication_dialog_ask;

{$I const.inc}

var
  LibMysqlPath: String = 'libmysql.dll';
  LibMysqlHandle: HMODULE; // Shared module handle

  mysql_affected_rows: function(Handle: PMYSQL): Int64; stdcall;
  mysql_character_set_name: function(Handle: PMYSQL): PAnsiChar; stdcall;
  mysql_close: procedure(Handle: PMYSQL); stdcall;
  mysql_data_seek: procedure(Result: PMYSQL_RES; Offset: Int64); stdcall;
  mysql_errno: function(Handle: PMYSQL): Cardinal; stdcall;
  mysql_error: function(Handle: PMYSQL): PAnsiChar; stdcall;
  mysql_fetch_field_direct: function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD; stdcall;
  mysql_fetch_lengths: function(Result: PMYSQL_RES): PLongInt; stdcall;
  mysql_fetch_row: function(Result: PMYSQL_RES): PMYSQL_ROW; stdcall;
  mysql_free_result: procedure(Result: PMYSQL_RES); stdcall;
  mysql_get_client_info: function: PAnsiChar; stdcall;
  mysql_get_server_info: function(Handle: PMYSQL): PAnsiChar; stdcall;
  mysql_init: function(Handle: PMYSQL): PMYSQL; stdcall;
  mysql_num_fields: function(Result: PMYSQL_RES): Integer; stdcall;
  mysql_num_rows: function(Result: PMYSQL_RES): Int64; stdcall;
  mysql_options: function(Handle: PMYSQL; Option: TMySQLOption; arg: PAnsiChar): Integer; stdcall;
  mysql_ping: function(Handle: PMYSQL): Integer; stdcall;
  mysql_real_connect: function(Handle: PMYSQL; const Host, User, Passwd, Db: PAnsiChar; Port: Cardinal; const UnixSocket: PAnsiChar; ClientFlag: Cardinal): PMYSQL; stdcall;
  mysql_real_query: function(Handle: PMYSQL; const Query: PAnsiChar; Length: Cardinal): Integer; stdcall;
  mysql_ssl_set: function(Handle: PMYSQL; const key, cert, CA, CApath, cipher: PAnsiChar): Byte; stdcall;
  mysql_stat: function(Handle: PMYSQL): PAnsiChar; stdcall;
  mysql_store_result: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
  mysql_thread_id: function(Handle: PMYSQL): Cardinal; stdcall;
  mysql_next_result: function(Handle: PMYSQL): Integer; stdcall;
  mysql_set_character_set: function(Handle: PMYSQL; csname: PAnsiChar): Integer; stdcall;
  mysql_thread_init: function: Byte; stdcall;
  mysql_thread_end: procedure; stdcall;

implementation

uses helpers, loginform;



{ TConnectionParameters }

constructor TConnectionParameters.Create;
begin
  FNetType := ntMySQL_TCPIP;
  FHostname := DEFAULT_HOST;
  FUsername := DEFAULT_USER;
  FPassword := '';
  FPort := DEFAULT_PORT;
  FSSHPlinkExe := GetRegValue(REGNAME_PLINKEXE, '');
  FSSHPort := DEFAULT_SSHPORT;
  FSSHTimeout := DEFAULT_SSHTIMEOUT;
  FSSHLocalPort := FPort + 1;
  FSSLPrivateKey := '';
  FSSLCertificate := '';
  FSSLCACertificate := '';
  FStartupScriptFilename := '';
end;


function TConnectionParameters.CreateConnection(AOwner: TComponent): TDBConnection;
begin
  case NetTypeGroup of
    ngMySQL:
      Result := TMySQLConnection.Create(AOwner);
    ngMSSQL:
      Result := TAdoDBConnection.Create(AOwner);
    else
      raise Exception.CreateFmt(MsgUnhandledNetType, [Integer(FNetType)]);
  end;
  Result.Parameters := Self;
end;


function TConnectionParameters.CreateQuery(AOwner: TComponent): TDBQuery;
begin
  case NetTypeGroup of
    ngMySQL:
      Result := TMySQLQuery.Create(AOwner);
    ngMSSQL:
      Result := TAdoDBQuery.Create(AOwner);
    else
      raise Exception.CreateFmt(MsgUnhandledNetType, [Integer(FNetType)]);
  end;
end;


function TConnectionParameters.NetTypeName(NetType: TNetType; LongFormat: Boolean): String;
var
  My: String;
begin
  if IsMariaDB then
    My := 'MariaDB'
  else if IsPercona then
    My := 'Percona'
  else
    My := 'MySQL';
  if LongFormat then case NetType of
    ntMySQL_TCPIP:
      Result := My+' (TCP/IP)';
    ntMySQL_NamedPipe:
      Result := My+' (named pipe)';
    ntMySQL_SSHtunnel:
      Result := My+' (SSH tunnel)';
    ntMSSQL_NamedPipe:
      Result := 'Microsoft SQL Server (named pipe)';
    ntMSSQL_TCPIP:
      Result := 'Microsoft SQL Server (TCP/IP)';
    ntMSSQL_SPX:
      Result := 'Microsoft SQL Server (SPX/IPX)';
    ntMSSQL_VINES:
      Result := 'Microsoft SQL Server (Banyan VINES)';
    ntMSSQL_RPC:
      Result := 'Microsoft SQL Server (Windows RPC)';
  end else case NetType of
    ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel:
      Result := My;
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP:
      Result := 'MS SQL';
  end;
end;


function TConnectionParameters.GetNetTypeGroup: TNetTypeGroup;
begin
  case FNetType of
    ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel:
      Result := ngMySQL;
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP, ntMSSQL_SPX, ntMSSQL_VINES, ntMSSQL_RPC:
      Result := ngMSSQL;
    else
      raise Exception.CreateFmt(MsgUnhandledNetType, [Integer(FNetType)]);
  end;
end;


function TConnectionParameters.IsMariaDB: Boolean;
begin
  Result := Pos('-mariadb', LowerCase(ServerVersion)) > 0;
end;


function TConnectionParameters.IsPercona: Boolean;
begin
  Result := Pos('percona server', LowerCase(ServerVersion)) > 0;
end;


function TConnectionParameters.GetImageIndex: Integer;
begin
  case NetTypeGroup of
    ngMySQL: begin
      Result := 164;
      if IsMariaDB then
        Result := 166;
      if IsPercona then
        Result := 169;
    end;
    ngMSSQL: Result := 123;
    else Result := ICONINDEX_SERVER;
  end;
end;



{ TMySQLConnection }

constructor TDBConnection.Create(AOwner: TComponent);
begin
  inherited;
  FParameters := TConnectionParameters.Create;
  FRowsFound := 0;
  FRowsAffected := 0;
  FConnectionStarted := 0;
  FLastQueryDuration := 0;
  FLastQueryNetworkDuration := 0;
  FThreadID := 0;
  FLogPrefix := '';
  FLogQueue := TDBLogQueue.Create(True);
  FIsUnicode := False;
  FIsSSL := False;
  FDatabases := TDatabaseList.Create(True);
  FLoginPromptDone := False;
  FCurrentUserHostCombination := '';
end;


constructor TMySQLConnection.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FQuoteChar := '`';
  // The compiler complains that dynamic and static arrays are incompatible, so this does not work:
  // FDatatypes := MySQLDatatypes
  SetLength(FDatatypes, Length(MySQLDatatypes));
  for i:=0 to High(MySQLDatatypes) do
    FDatatypes[i] := MySQLDatatypes[i];
end;


constructor TAdoDBConnection.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FQuoteChar := '"';
  SetLength(FDatatypes, Length(MSSQLDatatypes));
  for i:=0 to High(MSSQLDatatypes) do
    FDatatypes[i] := MSSQLDatatypes[i];
end;


destructor TDBConnection.Destroy;
begin
  if Active then Active := False;
  FOnDBObjectsCleared := nil;
  ClearCache(True);
  inherited;
end;


destructor TAdoDBConnection.Destroy;
begin
  if Active then Active := False;
  FreeAndNil(FAdoHandle);
  inherited;
end;


function TDBConnection.GetDatatypeByName(Datatype: String): TDBDatatype;
var
  i: Integer;
begin
  for i:=0 to High(FDatatypes) do begin
    if AnsiCompareText(FDatatypes[i].Name, Datatype) = 0 then begin
      Result := FDatatypes[i];
      break;
    end;
  end;
end;


procedure TMySQLConnection.AssignProc(var Proc: FARPROC; Name: PAnsiChar);
begin
  // Map library procedure to internal procedure
  Log(lcDebug, 'Assign procedure "'+Name+'"');
  Proc := GetProcAddress(LibMysqlHandle, Name);
  if Proc = nil then begin
    LibMysqlHandle := 0;
    raise EDatabaseError.Create('Your '+LibMysqlPath+' is out-dated or somehow incompatible to '+APPNAME+'. Please use the one from the installer, or just reinstall '+APPNAME+'.');
  end;
end;


procedure TDBConnection.SetLockedByThread(Value: TThread);
begin
  FLockedByThread := Value;
end;


procedure TMySQLConnection.SetLockedByThread(Value: TThread);
begin
  if Value <> FLockedByThread then begin
    if Value <> nil then begin
      Log(lcDebug, 'mysql_thread_init, thread id #'+IntToStr(Value.ThreadID));
      mysql_thread_init;
    end else begin
      mysql_thread_end;
      Log(lcDebug, 'mysql_thread_end, thread id #'+IntToStr(FLockedByThread.ThreadID));
    end;
  end;
  FLockedByThread := Value;
end;


{**
  (Dis-)Connect to/from server
}
procedure TMySQLConnection.SetActive( Value: Boolean );
var
  Connected: PMYSQL;
  ClientFlags, FinalPort: Integer;
  Error, tmpdb, FinalHost, FinalSocket, PlinkCmd: String;
  CurCharset: String;
  StartupInfo: TStartupInfo;
  ExitCode: LongWord;
  sslca, sslkey, sslcert: PAnsiChar;
  PluginDir: AnsiString;
  DoSSL, SSLsettingsComplete: Boolean;
  Vars: TDBQuery;
begin
  if Value and (FHandle = nil) then begin
    DoBeforeConnect;

    // Get handle
    FHandle := mysql_init(nil);

    // Prepare special stuff for SSL and SSH tunnel
    FinalHost := FParameters.Hostname;
    FinalSocket := '';
    FinalPort := FParameters.Port;
    case FParameters.NetType of
      ntMySQL_TCPIP: begin
        sslca := nil;
        sslkey := nil;
        sslcert := nil;
        if FParameters.SSLCACertificate <> '' then
          sslca := PAnsiChar(AnsiString(FParameters.SSLCACertificate));
        if FParameters.SSLPrivateKey <> '' then
          sslkey := PAnsiChar(AnsiString(FParameters.SSLPrivateKey));
        if FParameters.SSLCertificate <> '' then
          sslcert := PAnsiChar(AnsiString(FParameters.SSLCertificate));
        DoSSL := (sslca<>nil) or (sslkey<>nil) or (sslcert<>nil);
        SSLsettingsComplete := ((sslca<>nil) and (sslkey<>nil) and (sslcert<>nil))
          or ((sslca<>nil) and (sslkey=nil) and (sslcert=nil));
        if DoSSL then begin
          if not SSLsettingsComplete then
            raise EDatabaseError.Create('SSL settings incomplete. Please set either CA certificate or all three SSL parameters.')
          else if SSLsettingsComplete then begin
            FIsSSL := True;
            { TODO : Use Cipher and CAPath parameters }
            mysql_ssl_set(FHandle, sslkey, sslcert, sslca, nil, nil);
            Log(lcInfo, 'SSL parameters successfully set.');
          end;
        end;
      end;

      ntMySQL_NamedPipe: begin
        FinalHost := '.';
        FinalSocket := FParameters.Hostname;
      end;

      ntMySQL_SSHtunnel: begin
        // Build plink.exe command line
        // plink bob@domain.com -pw myPassw0rd1 -P 22 -i "keyfile.pem" -L 55555:localhost:3306
        PlinkCmd := FParameters.SSHPlinkExe + ' -ssh ';
        if FParameters.SSHUser <> '' then
          PlinkCmd := PlinkCmd + FParameters.SSHUser + '@';
        if FParameters.SSHHost <> '' then
          PlinkCmd := PlinkCmd + FParameters.SSHHost
        else
          PlinkCmd := PlinkCmd + FParameters.Hostname;
        if FParameters.SSHPassword <> '' then
          PlinkCmd := PlinkCmd + ' -pw ' + FParameters.SSHPassword;
        if FParameters.SSHPort > 0 then
          PlinkCmd := PlinkCmd + ' -P ' + IntToStr(FParameters.SSHPort);
        if FParameters.SSHPrivateKey <> '' then
          PlinkCmd := PlinkCmd + ' -i "' + FParameters.SSHPrivateKey + '"';
        PlinkCmd := PlinkCmd + ' -N -L ' + IntToStr(FParameters.SSHLocalPort) + ':' + FParameters.Hostname + ':' + IntToStr(FParameters.Port);
        Log(lcInfo, 'Attempt to create plink.exe process, waiting '+FormatNumber(FParameters.SSHTimeout)+'s for response ...');
        // Create plink.exe process
        FillChar(FPlinkProcInfo, SizeOf(TProcessInformation), 0);
        FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
        StartupInfo.cb := SizeOf(TStartupInfo);
        if CreateProcess(nil, PChar(PlinkCmd), nil, nil, false,
          CREATE_DEFAULT_ERROR_MODE + NORMAL_PRIORITY_CLASS + CREATE_NO_WINDOW,
          nil, nil, StartupInfo, FPlinkProcInfo) then begin
          WaitForSingleObject(FPlinkProcInfo.hProcess, FParameters.SSHTimeout*1000);
          GetExitCodeProcess(FPlinkProcInfo.hProcess, ExitCode);
          if ExitCode <> STILL_ACTIVE then
            raise EDatabaseError.Create('PLink exited unexpected. Command line was:'+CRLF+PlinkCmd);
        end else begin
          ClosePlink;
          raise EDatabaseError.Create('Couldn''t execute PLink: '+CRLF+PlinkCmd);
        end;
        FinalHost := 'localhost';
        FinalPort := FParameters.SSHLocalPort;
      end;
    end;

    // Gather client options
    ClientFlags := CLIENT_LOCAL_FILES or CLIENT_INTERACTIVE or CLIENT_PROTOCOL_41 or CLIENT_MULTI_STATEMENTS;
    if Parameters.Compressed then
      ClientFlags := ClientFlags or CLIENT_COMPRESS;
    if FIsSSL then
      ClientFlags := ClientFlags or CLIENT_SSL;

    // Point libmysql to the folder with client plugins
    PluginDir := AnsiString(ExtractFilePath(ParamStr(0))+'plugins\');
    mysql_options(FHandle, MYSQL_PLUGIN_DIR, PAnsiChar(PluginDir));

    Connected := mysql_real_connect(
      FHandle,
      PAnsiChar(Utf8Encode(FinalHost)),
      PAnsiChar(Utf8Encode(FParameters.Username)),
      PAnsiChar(Utf8Encode(FParameters.Password)),
      nil,
      FinalPort,
      PAnsiChar(Utf8Encode(FinalSocket)),
      ClientFlags
      );
    if Connected = nil then begin
      Error := LastError;
      Log(lcError, Error);
      FConnectionStarted := 0;
      FHandle := nil;
      ClosePlink;
      raise EDatabaseError.Create(Error);
    end else begin
      FActive := True;
      Log(lcInfo, 'Connected. Thread-ID: '+IntToStr(ThreadId));
      CharacterSet := 'utf8';
      CurCharset := CharacterSet;
      Log(lcDebug, 'Characterset: '+CurCharset);
      FIsUnicode := CurCharset = 'utf8';
      FConnectionStarted := GetTickCount div 1000;
      FServerStarted := FConnectionStarted - StrToIntDef(GetVar('SHOW STATUS LIKE ''Uptime''', 1), 1);
      FServerVersionUntouched := DecodeAPIString(mysql_get_server_info(FHandle));
      Vars := GetServerVariables;
      while not Vars.Eof do begin
        if Vars.Col(0) = 'version_compile_os' then
          FServerOS := Vars.Col(1);
        if Vars.Col(0) = 'hostname' then
          FRealHostname := Vars.Col(1);
        if (Vars.Col(0) = 'version_comment') and (Vars.Col(1) <> '') then
          FServerVersionUntouched := FServerVersionUntouched + ' - ' + Vars.Col(1);
        Vars.Next;
      end;
      if FDatabase <> '' then begin
        tmpdb := FDatabase;
        FDatabase := '';
        try
          Database := tmpdb;
        except
          // Trigger OnDatabaseChange event for <no db> if wanted db is not available
          FDatabase := tmpdb;
          Database := '';
        end;
      end;
      DoAfterConnect;
    end;
  end

  else if (not Value) and (FHandle <> nil) then begin
    mysql_close(FHandle);
    FActive := False;
    ClearCache(False);
    FConnectionStarted := 0;
    FHandle := nil;
    ClosePlink;
    Log(lcInfo, Format(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
  end;

end;


procedure TAdoDBConnection.SetActive(Value: Boolean);
var
  tmpdb, Error, NetLib, DataSource: String;
  rx: TRegExpr;
  i: Integer;
begin
  if Value then begin
    DoBeforeConnect;
    try
      // Creating the ADO object throws exceptions if MDAC is missing, especially on Wine
      FAdoHandle := TAdoConnection.Create(Owner);
    except
      on E:Exception do
        raise EDatabaseError.Create(E.Message+CRLF+CRLF+
            'On Wine, you can try to install MDAC:'+CRLF+
            'sh winetricks mdac28');
    end;
    NetLib := '';
    case Parameters.NetType of
      ntMSSQL_NamedPipe: NetLib := 'DBNMPNTW';
      ntMSSQL_TCPIP: NetLib := 'DBMSSOCN';
      ntMSSQL_SPX: NetLib := 'DBMSSPXN';
      ntMSSQL_VINES: NetLib := 'DBMSVINN';
      ntMSSQL_RPC: NetLib := 'DBMSRPCN';
    end;
    DataSource := Parameters.Hostname;
    if Parameters.NetType = ntMSSQL_TCPIP then
      DataSource := DataSource + ','+IntToStr(Parameters.Port);
    FAdoHandle.ConnectionString := 'Provider=SQLOLEDB;'+
      'Password='+Parameters.Password+';'+
      'Persist Security Info=True;'+
      'User ID='+Parameters.Username+';'+
      'Network Library='+NetLib+';'+
      'Data Source='+DataSource+';'
      ;
    if Parameters.WindowsAuth then
      FAdoHandle.ConnectionString := FAdoHandle.ConnectionString + 'Integrated Security=SSPI;';
    // Show up dynamic connection properties, probably useful for debugging
    for i:=0 to FAdoHandle.Properties.Count-1 do
      Log(lcDebug, 'OLE DB property "'+FAdoHandle.Properties[i].Name+'": '+String(FAdoHandle.Properties[i].Value));
    try
      FAdoHandle.Connected := True;
      FConnectionStarted := GetTickCount div 1000;
      FActive := True;
      Log(lcInfo, 'Connected. Thread-ID: '+IntToStr(ThreadId));
      // No need to set a charset for MS SQL
      // CharacterSet := 'utf8';
      // CurCharset := CharacterSet;
      // Log(lcDebug, 'Characterset: '+CurCharset);
      FIsUnicode := True;
      FServerStarted := FConnectionStarted - StrToIntDef(GetVar('SELECT DATEDIFF(SECOND, '+QuoteIdent('login_time')+', CURRENT_TIMESTAMP) FROM '+QuoteIdent('dbo')+'.'+QuoteIdent('sysprocesses')+' WHERE '+QuoteIdent('spid')+'=1'), 1);
      // Microsoft SQL Server 2008 R2 (RTM) - 10.50.1600.1 (Intel X86) 
	    // Apr  2 2010 15:53:02 
	    // Copyright (c) Microsoft Corporation
      // Express Edition with Advanced Services on Windows NT 6.1 <X86> (Build 7600: )
      FServerVersionUntouched := Trim(GetVar('SELECT @@VERSION'));
      rx := TRegExpr.Create;
      rx.ModifierI := False;
      // Extract server OS
      rx.Expression := '\s+on\s+([^\r\n]+)';
      if rx.Exec(FServerVersionUntouched) then
        FServerOS := rx.Match[1];
      // Cut at first line break
      rx.Expression := '^([^\r\n]+)';
      if rx.Exec(FServerVersionUntouched) then
        FServerVersionUntouched := rx.Match[1];
      rx.Free;
      FRealHostname := Parameters.Hostname;
      DoAfterConnect;

      // Reopen closed datasets after reconnecting
      // ... does not work for some reason. Still getting "not allowed on a closed object" errors in grid.
      //for i:=0 to FAdoHandle.DataSetCount-1 do
      //  FAdoHandle.DataSets[i].Open;

      if FDatabase <> '' then begin
        tmpdb := FDatabase;
        FDatabase := '';
        try
          Database := tmpdb;
        except
          FDatabase := tmpdb;
          Database := '';
        end;
      end;
    except
      on E:EOleException do begin
        Error := LastError;
        Log(lcError, Error);
        FConnectionStarted := 0;
        raise EDatabaseError.Create(Error);
      end;
    end;
  end else begin
    FAdoHandle.Connected := False;
    FActive := False;
    ClearCache(False);
    FConnectionStarted := 0;
    Log(lcInfo, Format(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
  end;
end;


procedure TDBConnection.DoBeforeConnect;
var
  UsernamePrompt, PasswordPrompt: String;
  UsingPass: String;
begin
  // Prompt for password on initial connect
  if FParameters.LoginPrompt and (not FLoginPromptDone) then begin
    UsernamePrompt := FParameters.Username;
    PasswordPrompt := FParameters.Password;
    LoginPrompt('Login to '+FParameters.Hostname+':', UsernamePrompt, PasswordPrompt);
    FParameters.Username := UsernamePrompt;
    FParameters.Password := PasswordPrompt;
    FLoginPromptDone := True;
  end;

  // Prepare connection
  if FParameters.Password <> '' then UsingPass := 'Yes' else UsingPass := 'No';
  Log(lcInfo, 'Connecting to '+FParameters.Hostname+' via '+FParameters.NetTypeName(FParameters.NetType, True)+
    ', username '+FParameters.Username+
    ', using password: '+UsingPass+' ...');
end;


procedure TMySQLConnection.DoBeforeConnect;
begin
  // Init libmysql before actually connecting.
  // Each connection has its own library handle
  if LibMysqlHandle = 0 then begin
    Log(lcDebug, 'Loading library file '+LibMysqlPath+' ...');
    LibMysqlHandle := LoadLibrary(PWideChar(LibMysqlPath));
    if LibMysqlHandle = 0 then
      raise EDatabaseError.Create('Can''t find a usable '+LibMysqlPath+'. Please launch '+ExtractFileName(ParamStr(0))+' from the directory where you have installed it.')
    else begin
      AssignProc(@mysql_affected_rows, 'mysql_affected_rows');
      AssignProc(@mysql_character_set_name, 'mysql_character_set_name');
      AssignProc(@mysql_close, 'mysql_close');
      AssignProc(@mysql_data_seek, 'mysql_data_seek');
      AssignProc(@mysql_errno, 'mysql_errno');
      AssignProc(@mysql_error, 'mysql_error');
      AssignProc(@mysql_fetch_field_direct, 'mysql_fetch_field_direct');
      AssignProc(@mysql_fetch_lengths, 'mysql_fetch_lengths');
      AssignProc(@mysql_fetch_row, 'mysql_fetch_row');
      AssignProc(@mysql_free_result, 'mysql_free_result');
      AssignProc(@mysql_get_client_info, 'mysql_get_client_info');
      AssignProc(@mysql_get_server_info, 'mysql_get_server_info');
      AssignProc(@mysql_init, 'mysql_init');
      AssignProc(@mysql_num_fields, 'mysql_num_fields');
      AssignProc(@mysql_num_rows, 'mysql_num_rows');
      AssignProc(@mysql_ping, 'mysql_ping');
      AssignProc(@mysql_options, 'mysql_options');
      AssignProc(@mysql_real_connect, 'mysql_real_connect');
      AssignProc(@mysql_real_query, 'mysql_real_query');
      AssignProc(@mysql_ssl_set, 'mysql_ssl_set');
      AssignProc(@mysql_stat, 'mysql_stat');
      AssignProc(@mysql_store_result, 'mysql_store_result');
      AssignProc(@mysql_thread_id, 'mysql_thread_id');
      AssignProc(@mysql_next_result, 'mysql_next_result');
      AssignProc(@mysql_set_character_set, 'mysql_set_character_set');
      AssignProc(@mysql_thread_init, 'mysql_thread_init');
      AssignProc(@mysql_thread_end, 'mysql_thread_end');
      Log(lcDebug, LibMysqlPath + ' v' + DecodeApiString(mysql_get_client_info) + ' loaded.');
    end;
  end;
  inherited;
end;


procedure TDBConnection.DoAfterConnect;
begin
  OpenRegistry(FParameters.SessionName);
  MainReg.WriteString(REGNAME_SERVERVERSION_FULL, FServerVersionUntouched);
  FParameters.ServerVersion := FServerVersionUntouched;
  if Assigned(FOnConnected) then
    FOnConnected(Self, FDatabase);
end;


function TMySQLConnection.Ping(Reconnect: Boolean): Boolean;
begin
  Log(lcDebug, 'Ping server ...');
  if (FHandle=nil) or (mysql_ping(FHandle) <> 0) then begin
    // Be sure to release some stuff before reconnecting
    Active := False;
    if Reconnect then
      Active := True;
  end;
  Result := FActive;
end;


function TAdoDBConnection.Ping(Reconnect: Boolean): Boolean;
begin
  Log(lcDebug, 'Ping server ...');
  if FActive then try
    FAdoHandle.Execute('SELECT 1');
  except
    on E:EOleException do begin
      Log(lcError, E.Message);
      Active := False;
      if Reconnect then
        Active := True;
    end;
  end;

  Result := FActive;
end;


procedure TMySQLConnection.ClosePlink;
begin
  if FPlinkProcInfo.hProcess <> 0 then begin
    Log(lcInfo, 'Closing plink.exe process #'+IntToStr(FPlinkProcInfo.dwProcessId)+' ...');
    TerminateProcess(FPlinkProcInfo.hProcess, 0);
    CloseHandle(FPlinkProcInfo.hProcess);
  end;
end;


{**
   Executes a query
}
procedure TMySQLConnection.Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL);
var
  QueryStatus: Integer;
  NativeSQL: AnsiString;
  TimerStart: Cardinal;
  QueryResult: PMYSQL_RES;
begin
  if (FLockedByThread <> nil) and (FLockedByThread.ThreadID <> GetCurrentThreadID) then begin
    Log(lcDebug, 'Waiting for running query to finish ...');
    try
      FLockedByThread.WaitFor;
    except
      on E:EThread do;
    end;
  end;

  Ping(True);
  Log(LogCategory, SQL);
  FLastQuerySQL := SQL;
  if IsUnicode then
    NativeSQL := UTF8Encode(SQL)
  else
    NativeSQL := AnsiString(SQL);
  TimerStart := GetTickCount;
  SetLength(FLastRawResults, 0);
  FResultCount := 0;
  QueryStatus := mysql_real_query(FHandle, PAnsiChar(NativeSQL), Length(NativeSQL));
  FLastQueryDuration := GetTickCount - TimerStart;
  FLastQueryNetworkDuration := 0;
  if QueryStatus <> 0 then begin
    // Most errors will show up here, some others slightly later, after mysql_store_result()
    Log(lcError, GetLastError);
    raise EDatabaseError.Create(GetLastError);
  end else begin
    // We must call mysql_store_result() + mysql_free_result() to unblock the connection
    // See: http://dev.mysql.com/doc/refman/5.0/en/mysql-store-result.html
    FRowsAffected := mysql_affected_rows(FHandle);
    TimerStart := GetTickCount;
    QueryResult := mysql_store_result(FHandle);
    FLastQueryNetworkDuration := GetTickCount - TimerStart;
    if (QueryResult = nil) and (FRowsAffected = -1) then begin
      // Indicates a late error, e.g. triggered by mysql_store_result(), after selecting a stored
      // function with invalid SQL body. Also SHOW TABLE STATUS on older servers.
      Log(lcError, GetLastError);
      raise EDatabaseError.Create(GetLastError);
    end;
    if QueryResult <> nil then begin
      FRowsFound := mysql_num_rows(QueryResult);
      FRowsAffected := 0;
      Log(lcDebug, IntToStr(RowsFound)+' rows found.');

      while true do begin
        if QueryResult <> nil then begin
          if DoStoreResult then begin
            SetLength(FLastRawResults, Length(FLastRawResults)+1);
            FLastRawResults[Length(FLastRawResults)-1] := QueryResult;
          end else begin
            mysql_free_result(QueryResult);
          end;
        end;
        // more results? -1 = no, >0 = error, 0 = yes (keep looping)
        QueryStatus := mysql_next_result(FHandle);
        case QueryStatus of
          -1: break;
          0: QueryResult := mysql_store_result(FHandle);
          else begin
            Log(lcError, GetLastError);
            raise EDatabaseError.Create(GetLastError);
          end;
        end;
      end;
      FResultCount := Length(FLastRawResults);

    end else begin
      // Query did not return a result
      FRowsFound := 0;
      Log(lcDebug, IntToStr(RowsAffected)+' rows affected.');
      if UpperCase(Copy(SQL, 1, 3)) = 'USE' then begin
        FDatabase := Trim(Copy(SQL, 4, Length(SQL)-3));
        FDatabase := DeQuoteIdent(FDatabase);
        Log(lcDebug, 'Database "'+FDatabase+'" selected');
        if Assigned(FOnDatabaseChanged) then
          FOnDatabaseChanged(Self, Database);
      end;
    end;
  end;
end;


procedure TAdoDBConnection.Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL);
var
  TimerStart: Cardinal;
  VarRowsAffected: OleVariant;
  QueryResult, NextResult: _RecordSet;
  Affected: Int64;
begin
  if (FLockedByThread <> nil) and (FLockedByThread.ThreadID <> GetCurrentThreadID) then begin
    Log(lcDebug, 'Waiting for running query to finish ...');
    try
      FLockedByThread.WaitFor;
    except
      on E:EThread do;
    end;
  end;

  Ping(True);
  Log(LogCategory, SQL);
  FLastQuerySQL := SQL;
  TimerStart := GetTickCount;
  SetLength(FLastRawResults, 0);
  FResultCount := 0;
  FRowsFound := 0;
  FRowsAffected := 0;
  try
    QueryResult := FAdoHandle.ConnectionObject.Execute(SQL, VarRowsAffected, 1);
    FLastQueryDuration := GetTickCount - TimerStart;
    FLastQueryNetworkDuration := 0;

    // Handle multiple results
    while(QueryResult <> nil) do begin
      Affected := VarRowsAffected;
      Affected := Max(Affected, 0);
      Inc(FRowsAffected, Affected);
      NextResult := QueryResult.NextRecordset(VarRowsAffected);
      if QueryResult.Fields.Count > 0 then begin
        Inc(FRowsFound, QueryResult.RecordCount);
        if DoStoreResult then begin
          SetLength(FLastRawResults, Length(FLastRawResults)+1);
          FLastRawResults[Length(FLastRawResults)-1] := QueryResult;
        end else
          QueryResult := nil;
      end else
        QueryResult := nil;
      QueryResult := NextResult;
    end;
    FResultCount := Length(FLastRawResults);

    if UpperCase(Copy(SQL, 1, 3)) = 'USE' then begin
      FDatabase := Trim(Copy(SQL, 4, Length(SQL)-3));
      FDatabase := DeQuoteIdent(FDatabase);
      Log(lcDebug, 'Database "'+FDatabase+'" selected');
      if Assigned(FOnDatabaseChanged) then
        FOnDatabaseChanged(Self, Database);
    end;
  except
    on E:EOleException do begin
      FLastError := E.Message;
      Log(lcError, GetLastError);
      raise EDatabaseError.Create(GetLastError);
    end;
  end;
end;


function TMySQLConnection.GetLastResults: TDBQueryList;
var
  r: TDBQuery;
  i: Integer;
begin
  Result := TDBQueryList.Create(False);
  for i:=Low(FLastRawResults) to High(FLastRawResults) do begin
    r := Parameters.CreateQuery(nil);
    r.Connection := Self;
    r.SQL := FLastQuerySQL;
    r.Execute(False, i);
    Result.Add(r);
  end;
end;


function TAdoDBConnection.GetLastResults: TDBQueryList;
var
  r: TDBQuery;
  i: Integer;
begin
  Result := TDBQueryList.Create(False);
  for i:=Low(FLastRawResults) to High(FLastRawResults) do begin
    r := Parameters.CreateQuery(nil);
    r.Connection := Self;
    r.SQL := FLastQuerySQL;
    r.Execute(False, i);
    Result.Add(r);
  end;
end;


function TMySQLConnection.GetCreateCode(Database, Name: String; NodeType: TListNodeType): String;
var
  Column: Integer;
  ObjType: String;
  TmpObj: TDBObject;
begin
  Column := -1;
  TmpObj := TDBObject.Create(Self);
  TmpObj.NodeType := NodeType;
  ObjType := TmpObj.ObjType;
  case NodeType of
    lntTable, lntView: Column := 1;
    lntFunction, lntProcedure, lntTrigger: Column := 2;
    lntEvent: Column := 3;
    else Exception.Create('Unhandled list node type in '+ClassName+'.GetCreateCode');
  end;
  Result := GetVar('SHOW CREATE '+UpperCase(TmpObj.ObjType)+' '+QuoteIdent(Database)+'.'+QuoteIdent(Name), Column);
  TmpObj.Free;
end;


function TAdoDBConnection.GetCreateCode(Database, Name: String; NodeType: TListNodeType): String;
var
  Cols: TDBQuery;
begin
  Result := 'CREATE TABLE '+QuoteIdent(Name)+' (';
  Cols := GetResults('SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE '+
    'TABLE_CATALOG='+EscapeString(Database)+' AND TABLE_NAME='+EscapeString(Name));
  while not Cols.Eof do begin
    Result := Result + CRLF + #9 + QuoteIdent(Cols.Col('COLUMN_NAME')) + ' ' + UpperCase(Cols.Col('DATA_TYPE'));
    if not Cols.IsNull('CHARACTER_MAXIMUM_LENGTH') then
      Result := Result + '(' + Cols.Col('CHARACTER_MAXIMUM_LENGTH') + ')';
    if Cols.Col('IS_NULLABLE') = 'NO' then
      Result := Result + ' NOT';
    Result := Result + ' NULL';
    Result := Result + ',';
    Cols.Next;
  end;
  Cols.Free;
  Delete(Result, Length(Result), 1);
  Result := Result + ')';
end;


{**
  Set "Database" property and select that db if connected
}
procedure TDBConnection.SetDatabase(Value: String);
begin
  Log(lcDebug, 'SetDatabase('+Value+'), FDatabase: '+FDatabase);
  if Value <> FDatabase then begin
    if Value = '' then begin
      FDatabase := Value;
      if Assigned(FOnDatabaseChanged) then
        FOnDatabaseChanged(Self, Value);
    end else
      Query('USE '+QuoteIdent(Value), False);
    SetObjectNamesInSelectedDB;
  end;
end;


{**
  Return current thread id
}
function TMySQLConnection.GetThreadId: Cardinal;
begin
  if FThreadId = 0 then begin
    Ping(False);
    if FActive then
      FThreadID := mysql_thread_id(FHandle);
  end;
  Result := FThreadID;
end;


function TAdoDBConnection.GetThreadId: Cardinal;
begin
  if FThreadId = 0 then begin
    Ping(False);
    if FActive then
      FThreadID := StrToIntDef(GetVar('SELECT @@SPID'), 0);
  end;
  Result := FThreadID;
end;


{**
  Return currently used character set
}
function TMySQLConnection.GetCharacterSet: String;
begin
  Result := DecodeAPIString(mysql_character_set_name(FHandle));
end;


function TAdoDBConnection.GetCharacterSet: String;
begin
  Result := '';
end;


{**
  Switch character set
}
procedure TMySQLConnection.SetCharacterSet(CharsetName: String);
begin
  mysql_set_character_set(FHandle, PAnsiChar(Utf8Encode(CharsetName)));
end;


procedure TAdoDBConnection.SetCharacterSet(CharsetName: String);
begin
  // Not in use. No charset stuff going on here?
end;


function TMySQLConnection.GetLastErrorCode: Cardinal;
begin
  Result := mysql_errno(FHandle);
end;


function TAdoDBConnection.GetLastErrorCode: Cardinal;
begin
  Result := FAdoHandle.Errors[FAdoHandle.Errors.Count-1].NativeError;
end;


{**
  Return the last error nicely formatted
}
function TMySQLConnection.GetLastError: String;
var
  Msg, Additional: String;
  rx: TRegExpr;
begin
  Msg := DecodeAPIString(mysql_error(FHandle));
  // Find "(errno: 123)" in message and add more meaningful message from perror.exe
  rx := TRegExpr.Create;
  rx.Expression := '.+\(errno\:\s+(\d+)\)';
  if rx.Exec(Msg) then begin
    Additional := MySQLErrorCodes.Values[rx.Match[1]];
    if Additional <> '' then
      Msg := Msg + CRLF + CRLF + Additional;
  end;
  rx.Free;
  Result := Format(MsgSQLError, [LastErrorCode, Msg]);
end;


function TAdoDBConnection.GetLastError: String;
var
  Msg: String;
  rx: TRegExpr;
  E: Error;
begin
  E := FAdoHandle.Errors[FAdoHandle.Errors.Count-1];
  Msg := E.Description;
  // Remove stuff from driver in message "[DBNETLIB][ConnectionOpen (Connect()).]"
  rx := TRegExpr.Create;
  rx.Expression := '^\[DBNETLIB\]\[.*\](.+)$';
  if rx.Exec(Msg) then
    Msg := rx.Match[1];
  rx.Free;
  Result := Format(MsgSQLError, [LastErrorCode, Msg]);
end;


{**
  Get version string as normalized integer
  "5.1.12-beta-community-123" => 50112
}
function TMySQLConnection.GetServerVersionInt: Integer;
var
  i, dots: Byte;
  v1, v2, v3: String;
begin
  Result := -1;

  dots := 0;
  v1 := '';
  v2 := '';
  v3 := '';
  for i:=1 to Length(FServerVersionUntouched) do begin
    if FServerVersionUntouched[i] = '.' then begin
      inc(dots);
      // We expect exactly 2 dots.
      if dots > 2 then
        break;
    end else if CharInSet(FServerVersionUntouched[i], ['0'..'9']) then begin
      if dots = 0 then
        v1 := v1 + FServerVersionUntouched[i]
      else if dots = 1 then
        v2 := v2 + FServerVersionUntouched[i]
      else if dots = 2 then
        v3 := v3 + FServerVersionUntouched[i];
    end else // Don't include potential numbers of trailing string
      break;
  end;

  // Concat tokens
  if (Length(v1)>0) and (Length(v2)>0) and (Length(v3)>0) then begin
    Result := StrToIntDef(v1, 0) *10000 +
      StrToIntDef(v2, 0) *100 +
      StrToIntDef(v3, 0);
  end;

end;

function TAdoDBConnection.GetServerVersionInt: Integer;
var
  rx: TRegExpr;
begin
  rx := TRegExpr.Create;
  rx.ModifierG := False;
  rx.Expression := '(\d{4})\D';
  if rx.Exec(FServerVersionUntouched) then
    Result := MakeInt(rx.Match[1])
  else
    Result := 0;
  rx.Free;
end;


function TDBConnection.GetServerVersionStr: String;
begin
  Result := ConvertServerVersion(ServerVersionInt);
end;


function TDBConnection.GetAllDatabases: TStringList;
var
  rx: TRegExpr;
begin
  // Get user passed delimited list
  if not Assigned(FAllDatabases) then begin
    if FParameters.AllDatabasesStr <> '' then begin
      FAllDatabases := TStringList.Create;
      rx := TRegExpr.Create;
      rx.Expression := '[^;\s]+';
      rx.ModifierG := True;
      if rx.Exec(FParameters.AllDatabasesStr) then while true do begin
        // Add if not a duplicate
        if FAllDatabases.IndexOf(rx.Match[0]) = -1 then
          FAllDatabases.Add(rx.Match[0]);
        if not rx.ExecNext then
          break;
      end;
      rx.Free;
    end;
  end;
  Result := FAllDatabases;
end;


function TMySQLConnection.GetAllDatabases: TStringList;
begin
  Result := inherited;
  if not Assigned(Result) then begin
    try
      FAllDatabases := GetCol('SHOW DATABASES');
    except on E:EDatabaseError do
      try
        FAllDatabases := GetCol('SELECT '+QuoteIdent('SCHEMA_NAME')+' FROM '+QuoteIdent('information_schema')+'.'+QuoteIdent('SCHEMATA')+' ORDER BY '+QuoteIdent('SCHEMA_NAME'));
      except
        on E:EDatabaseError do begin
          FAllDatabases := TStringList.Create;
          Log(lcError, 'Database names not available due to missing privileges for user '+CurrentUserHostCombination+'.');
        end;
      end;
    end;
    Result := FAllDatabases;
  end;
end;


function TAdoDBConnection.GetAllDatabases: TStringList;
begin
  Result := inherited;
  if not Assigned(Result) then begin
    try
      case ServerVersionInt of
        2000:
          FAllDatabases := GetCol('SELECT '+QuoteIdent('name')+' FROM '+QuoteIdent('master')+'..'+QuoteIdent('sysdatabases')+' ORDER BY '+QuoteIdent('name'));
        else
          FAllDatabases := GetCol('SELECT '+QuoteIdent('name')+' FROM '+QuoteIdent('sys')+'.'+QuoteIdent('databases')+' ORDER BY '+QuoteIdent('name'));
      end;
    except on E:EDatabaseError do
      FAllDatabases := TStringList.Create;
    end;
    Result := FAllDatabases;
  end;
end;


function TDBConnection.RefreshAllDatabases: TStringList;
begin
  FreeAndNil(FAllDatabases);
  Result := AllDatabases;
end;


{**
  Convert integer version to real version string
}
function TMySQLConnection.ConvertServerVersion(Version: Integer): String;
var
  v : String;
  v1, v2 : Byte;
begin
  v := IntToStr( Version );
  v1 := StrToIntDef( v[2]+v[3], 0 );
  v2 := StrToIntDef( v[4]+v[5], 0 );
  Result := v[1] + '.' + IntToStr(v1) + '.' + IntToStr(v2);
end;


function TAdoDBConnection.ConvertServerVersion(Version: Integer): String;
begin
  Result := IntToStr(Version);
end;


function TDBConnection.GetResults(SQL: String): TDBQuery;
begin
  Result := Parameters.CreateQuery(Self);
  Result.Connection := Self;
  Result.SQL := SQL;
  try
    Result.Execute;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


{**
  Call log event if assigned to object
  If running a thread, log to queue and let the main thread later do logging
}
procedure TDBConnection.Log(Category: TDBLogCategory; Msg: String);
var
  LogItem: TDBLogItem;
begin
  if (FLockedByThread <> nil) and (FLockedByThread.ThreadID = GetCurrentThreadID) then begin
    LogItem := TDBLogItem.Create;
    LogItem.Msg := Msg;
    LogItem.Category := Category;
    FLogQueue.Add(LogItem);
  end else if Assigned(FOnLog) then begin
    if FLogPrefix <> '' then
      Msg := '['+FLogPrefix+'] ' + Msg;
    FOnLog(Msg, Category, Self);
  end;
end;


{**
  Escapes a string for usage in SQL queries
  - single-backslashes which represent normal parts of the text and not escape-sequences
  - characters which MySQL doesn't strictly care about, but which might confuse editors etc.
  - single and double quotes in a text string
  - joker-chars for LIKE-comparisons
  Finally, surround the text by single quotes.

  @param string Text to escape
  @param boolean Escape text so it can be used in a LIKE-comparison
  @return string
}
function TDBConnection.EscapeString(Text: String; ProcessJokerChars: Boolean=false; DoQuote: Boolean=True): String;
var
  c1, c2, c3, c4, EscChar: Char;
begin
  c1 := '''';
  c2 := '\';
  c3 := '%';
  c4 := '_';
  EscChar := '\';
  if not ProcessJokerChars then begin
    // Do not escape joker-chars which are used in a LIKE-clause
    c4 := '''';
    c3 := '''';
  end;
  Result := escChars(Text, EscChar, c1, c2, c3, c4);
  // Remove characters that SynEdit chokes on, so that
  // the SQL file can be non-corruptedly loaded again.
  c1 := #13;
  c2 := #10;
  c3 := #0;
  c4 := #0;
  // TODO: SynEdit also chokes on Char($2028) and possibly Char($2029).
  Result := escChars(Result, EscChar, c1, c2, c3, c4);
  if DoQuote then begin
    // Add surrounding single quotes
    Result := Char(#39) + Result + Char(#39);
  end;
end;


{***
 Attempt to do string replacement faster than StringReplace
}
function TDBConnection.escChars(const Text: String; EscChar, Char1, Char2, Char3, Char4: Char): String;
const
  // Attempt to match whatever the CPU cache will hold.
  block: Cardinal = 65536;
var
  bstart, bend, matches, i: Cardinal;
  // These could be bumped to uint64 if necessary.
  len, respos: Cardinal;
  next: Char;
begin
  len := Length(Text);
  Result := '';
  bend := 0;
  respos := 0;
  repeat
    bstart := bend + 1;
    bend := bstart + block - 1;
    if bend > len then bend := len;
    matches := 0;
    for i := bstart to bend do if
      (Text[i] = Char1) or
      (Text[i] = Char2) or
      (Text[i] = Char3) or
      (Text[i] = Char4)
    then Inc(matches);
    SetLength(Result, bend + 1 - bstart + matches + respos);
    for i := bstart to bend do begin
      next := Text[i];
      if
        (next = Char1) or
        (next = Char2) or
        (next = Char3) or
        (next = Char4)
      then begin
        Inc(respos);
        Result[respos] := EscChar;
        // Special values for MySQL escape.
        if next = #13 then next := 'r';
        if next = #10 then next := 'n';
        if next = #0 then next := '0';
      end;
      Inc(respos);
      Result[respos] := next;
    end;
  until bend = len;
end;


function TDBConnection.UnescapeString(Text: String): String;
begin
  // Return text with MySQL special sequences turned back to normal characters
  Result := StringReplace(Text, '\0', #0, [rfReplaceAll]);
  Result := StringReplace(Result, '\b', #8, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
  Result := StringReplace(Result, '\Z', #26, [rfReplaceAll]);
  Result := StringReplace(Result, '''''', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '\''', '''', [rfReplaceAll]);
end;


{**
  Add backticks to identifier
  Todo: Support ANSI style
}
function TDBConnection.QuoteIdent(Identifier: String; AlwaysQuote: Boolean=True; Glue: Char=#0): String;
var
  GluePos, i: Integer;
begin
  Result := Identifier;
  GluePos := 0;
  if Glue <> #0 then begin
    GluePos := Pos(Glue, Result);
    if GluePos > 0 then
      Result := QuoteIdent(Copy(Result, 1, GluePos-1)) + Glue + QuoteIdent(Copy(Result, GluePos+1, MaxInt));
  end;
  if GluePos = 0 then begin
    if not AlwaysQuote then begin
      if MySQLKeywords.IndexOf(Result) > -1 then
        AlwaysQuote := True
      else for i:=1 to Length(Result) do begin
        if not CharInSet(Result[i], IDENTCHARS) then begin
          AlwaysQuote := True;
          break;
        end;
      end;
    end;
    if AlwaysQuote then begin
      Result := StringReplace(Result, FQuoteChar, FQuoteChar+FQuoteChar, [rfReplaceAll]);
      Result := FQuoteChar + Result + FQuoteChar;
    end;
  end;
end;


function TDBConnection.DeQuoteIdent(Identifier: String; Glue: Char=#0): String;
begin
  Result := Identifier;
  if (Length(Identifier)>0) and (Result[1] = FQuoteChar) and (Result[Length(Identifier)] = FQuoteChar) then
    Result := Copy(Result, 2, Length(Result)-2);
  if Glue <> #0 then
    Result := StringReplace(Result, FQuoteChar+Glue+FQuoteChar, Glue, [rfReplaceAll]);
  Result := StringReplace(Result, FQuoteChar+FQuoteChar, FQuoteChar, [rfReplaceAll]);
end;


function TDBConnection.GetCol(SQL: String; Column: Integer=0): TStringList;
var
  Results: TDBQuery;
begin
  Results := GetResults(SQL);
  Result := TStringList.Create;
  if Results.RecordCount > 0 then while not Results.Eof do begin
    Result.Add(Results.Col(Column));
    Results.Next;
  end;
  FreeAndNil(Results);
end;


{**
  Get single cell value via SQL query, identified by column number
}
function TDBConnection.GetVar(SQL: String; Column: Integer=0): String;
var
  Results: TDBQuery;
begin
  Results := GetResults(SQL);
  if Results.RecordCount > 0 then
    Result := Results.Col(Column)
  else
    Result := '';
  FreeAndNil(Results);
end;


{**
  Get single cell value via SQL query, identified by column name
}
function TDBConnection.GetVar(SQL: String; Column: String): String;
var
  Results: TDBQuery;
begin
  Results := GetResults(SQL);
  if Results.RecordCount > 0 then
    Result := Results.Col(Column)
  else
    Result := '';
  FreeAndNil(Results);
end;


function TDBConnection.GetTableEngines: TStringList;
begin
  if not Assigned(FTableEngines) then
    FTableEngines := TStringList.Create;
  Result := FTableEngines;
end;


function TMySQLConnection.GetTableEngines: TStringList;
var
  Results: TDBQuery;
  engineName, engineSupport: String;
  rx: TRegExpr;
begin
  // After a disconnect Ping triggers the cached engines to be reset
  Log(lcDebug, 'Fetching list of table engines ...');
  Ping(True);
  if not Assigned(FTableEngines) then begin
    FTableEngines := TStringList.Create;
    try
      Results := GetResults('SHOW ENGINES');
      while not Results.Eof do begin
        engineName := Results.Col('Engine');
        engineSupport := LowerCase(Results.Col('Support'));
        // Add to dropdown if supported
        if (engineSupport = 'yes') or (engineSupport = 'default') then
          FTableEngines.Add(engineName);
        // Check if this is the default engine
        if engineSupport = 'default' then
          FTableEngineDefault := engineName;
        Results.Next;
      end;
      Results.Free;
    except
      // Ignore errors on old servers and try a fallback:
      // Manually fetch available engine types by analysing have_* options
      // This is for servers below 4.1 or when the SHOW ENGINES statement has
      // failed for some other reason
      Results := GetServerVariables;
      // Add default engines which will not show in a have_* variable:
      FTableEngines.CommaText := 'MyISAM,MRG_MyISAM,HEAP';
      FTableEngineDefault := 'MyISAM';
      rx := TRegExpr.Create;
      rx.ModifierI := True;
      rx.Expression := '^have_(ARCHIVE|BDB|BLACKHOLE|CSV|EXAMPLE|FEDERATED|INNODB|ISAM)(_engine)?$';
      while not Results.Eof do begin
        if rx.Exec(Results.Col(0)) and (LowerCase(Results.Col(1)) = 'yes') then
          FTableEngines.Add(UpperCase(rx.Match[1]));
        Results.Next;
      end;
      rx.Free;
      Results.Free;
    end;
  end;
  Result := FTableEngines;
end;


function TDBConnection.GetCollationTable: TDBQuery;
begin
  Log(lcDebug, 'Fetching list of collations ...');
  Ping(True);
  Result := FCollationTable;
end;


function TMySQLConnection.GetCollationTable: TDBQuery;
begin
  inherited;
  if (not Assigned(FCollationTable)) and (ServerVersionInt >= 40100) then
    FCollationTable := GetResults('SHOW COLLATION');
  if Assigned(FCollationTable) then
    FCollationTable.First;
  Result := FCollationTable;
end;


function TAdoDBConnection.GetCollationTable: TDBQuery;
begin
  inherited;
  if (not Assigned(FCollationTable)) then
    FCollationTable := GetResults('SELECT '+EscapeString('')+' AS '+QuoteIdent('Collation')+', '+
      EscapeString('')+' AS '+QuoteIdent('Charset')+', 0 AS '+QuoteIdent('Id')+', '+
      EscapeString('')+' AS '+QuoteIdent('Default')+', '+EscapeString('')+' AS '+QuoteIdent('Compiled')+', '+
      '1 AS '+QuoteIdent('Sortlen'));
  if Assigned(FCollationTable) then
    FCollationTable.First;
  Result := FCollationTable;
end;


function TDBConnection.GetCollationList: TStringList;
var
  c: TDBQuery;
begin
  c := CollationTable;
  Result := TStringList.Create;
  if Assigned(c) then while not c.Eof do begin
    Result.Add(c.Col('Collation'));
    c.Next;
  end;
end;


function TDBConnection.GetCharsetTable: TDBQuery;
begin
  Log(lcDebug, 'Fetching charset list ...');
  Ping(True);
  Result := nil;
end;


function TMySQLConnection.GetCharsetTable: TDBQuery;
begin
  inherited;
  if (not Assigned(FCharsetTable)) and (ServerVersionInt >= 40100) then
    FCharsetTable := GetResults('SHOW CHARSET');
  Result := FCharsetTable;
end;


function TAdoDBConnection.GetCharsetTable: TDBQuery;
begin
  inherited;
  if not Assigned(FCharsetTable) then
    FCharsetTable := GetResults('SELECT '+QuoteIdent('name')+' AS '+QuoteIdent('Charset')+', '+QuoteIdent('description')+' AS '+QuoteIdent('Description')+
      ' FROM '+QuoteIdent('sys')+'.'+QuoteIdent('syscharsets')
      );
  Result := FCharsetTable;
end;


function TDBConnection.GetCharsetList: TStringList;
var
  c: TDBQuery;
begin
  c := CharsetTable;
  Result := TStringList.Create;
  if Assigned(c) then begin
    c.First;
    while not c.Eof do begin
      Result.Add(c.Col('Description') + ' (' + c.Col('Charset') + ')');
      c.Next;
    end;
  end;
end;


function TMySQLConnection.GetServerVariables: TDBQuery;
begin
  // Return server variables
  Result := GetResults('SHOW VARIABLES');
end;


function TAdoDBConnection.GetServerVariables: TDBQuery;
begin
  // Enumerate some config values on MS SQL
  Result := GetResults('SELECT '+QuoteIdent('comment')+', '+QuoteIdent('value')+' FROM '+QuoteIdent('master')+'.'+QuoteIdent('dbo')+'.'+QuoteIdent('syscurconfigs')+' ORDER BY '+QuoteIdent('comment'));
end;


function TDBConnection.GetInformationSchemaObjects: TStringList;
var
  Objects: TDBObjectList;
  Obj: TDBObject;
begin
  Log(lcDebug, 'Fetching objects in information_schema db ...');
  Ping(True);
  if not Assigned(FInformationSchemaObjects) then begin
    FInformationSchemaObjects := TStringList.Create;
    // Gracefully return an empty list on old servers
    if AllDatabases.IndexOf('information_schema') > -1 then begin
      Objects := GetDBObjects('information_schema');
      for Obj in Objects do
        FInformationSchemaObjects.Add(Obj.Name);
    end;
  end;
  Result := FInformationSchemaObjects;
end;


function TAdoDBConnection.GetInformationSchemaObjects: TStringList;
begin
  // MS SQL hides information_schema
  inherited;
  if FInformationSchemaObjects.Count = 0 then begin
    FInformationSchemaObjects.CommaText := 'CHECK_CONSTRAINTS,'+
      'COLUMN_DOMAIN_USAGE,'+
      'COLUMN_PRIVILEGES,'+
      'COLUMNS,'+
      'CONSTRAINT_COLUMN_USAGE,'+
      'CONSTRAINT_TABLE_USAGE,'+
      'DOMAIN_CONSTRAINTS,'+
      'DOMAINS,'+
      'KEY_COLUMN_USAGE,'+
      'PARAMETERS,'+
      'REFERENTIAL_CONSTRAINTS,'+
      'ROUTINES,'+
      'ROUTINE_COLUMNS,'+
      'SCHEMATA,'+
      'TABLE_CONSTRAINTS,'+
      'TABLE_PRIVILEGES,'+
      'TABLES,'+
      'VIEW_COLUMN_USAGE,'+
      'VIEW_TABLE_USAGE,'+
      'VIEWS';
  end;
  Result := FInformationSchemaObjects;
end;


function TDBConnection.GetConnectionUptime: Integer;
begin
  // Return seconds since last connect
  if not FActive then
    Result := 0
  else
    Result := Integer(GetTickCount div 1000) - FConnectionStarted;
end;


function TDBConnection.GetServerUptime: Integer;
begin
  // Return server uptime in seconds
  Result := Integer(GetTickCount div 1000) - FServerStarted;
end;


function TDBConnection.GetCurrentUserHostCombination: String;
var
  sql: String;
begin
  // Return current user@host combination, used by various object editors for DEFINER clauses
  Log(lcDebug, 'Fetching user@host ...');
  Ping(True);
  if FCurrentUserHostCombination = '' then begin
    case Parameters.NetTypeGroup of
      ngMySQL:
        sql := 'SELECT CURRENT_USER()';
      ngMSSQL:
        sql := 'SELECT SYSTEM_USER';
      else
        raise Exception.CreateFmt(MsgUnhandledNetType, [Integer(Parameters.NetType)]);
    end;
    FCurrentUserHostCombination := GetVar(sql);
  end;
  Result := FCurrentUserHostCombination;
end;


procedure TDBConnection.ClearCache(IncludeDBObjects: Boolean);
begin
  // Free cached lists and results. Called when the connection was closed and/or destroyed
  FreeAndNil(FCollationTable);
  FreeAndNil(FCharsetTable);
  FreeAndNil(FTableEngines);
  FreeAndNil(FInformationSchemaObjects);
  if IncludeDBObjects then
    ClearAllDbObjects;
  FTableEngineDefault := '';
  FCurrentUserHostCombination := '';
  FThreadID := 0;
end;


procedure TDBConnection.ClearDbObjects(db: String);
var
  i: Integer;
begin
  // Free cached database object list
  for i:=0 to FDatabases.Count-1 do begin
    if FDatabases[i].Database = db then begin
      FDatabases.Delete(i);
      if Assigned(FOnDBObjectsCleared) then
        FOnDBObjectsCleared(Self, db);
      break;
    end;
  end;
end;


procedure TDBConnection.ClearAllDbObjects;
var
  i: Integer;
begin
  for i:=FDatabases.Count-1 downto 0 do
    ClearDbObjects(FDatabases[i].Database);
end;


function TDBConnection.DbObjectsCached(db: String): Boolean;
var
  i: Integer;
begin
  // Check if a table list is stored in cache
  Result := False;
  for i:=0 to FDatabases.Count-1 do begin
    if FDatabases[i].Database = db then begin
      Result := True;
      break;
    end;
  end;
end;


function TDBConnection.ParseDateTime(Str: String): TDateTime;
var
  rx: TRegExpr;
begin
  // Parse SQL date/time string value into a TDateTime
  Result := 0;
  rx := TRegExpr.Create;
  rx.Expression := '^(\d{4})\-(\d{2})\-(\d{2}) (\d{2})\:(\d{2})\:(\d{2})$';
  if rx.Exec(Str) then try
    Result := EncodeDateTime(
      StrToIntDef(rx.Match[1], 0),
      StrToIntDef(rx.Match[2], 1),
      StrToIntDef(rx.Match[3], 1),
      StrToIntDef(rx.Match[4], 0),
      StrToIntDef(rx.Match[5], 0),
      StrToIntDef(rx.Match[6], 0),
      0 // milliseconds, unused
      );
  except
    Result := 0;
  end;
end;


function TMySQLConnection.GetDbObjects(db: String; Refresh: Boolean=False): TDBObjectList;
var
  obj: TDBObject;
  Results: TDBQuery;
  rx: TRegExpr;
  i: Integer;
begin
  // Cache and return a db's table list
  if Refresh then
    ClearDbObjects(db);

  // Find list in cache
  Result := nil;
  for i:=0 to FDatabases.Count-1 do begin
    if FDatabases[i].Database = db then begin
      Result := FDatabases[i];
      break;
    end;
  end;

  if not Assigned(Result) then begin
    Result := TDBObjectList.Create(TDBObjectComparer.Create);
    Result.FLastUpdate := 0;
    Result.FDataSize := 0;
    Result.FDatabase := db;
    try
      Result.FCollation := GetVar('SELECT '+QuoteIdent('DEFAULT_COLLATION_NAME')+
        ' FROM '+QuoteIdent('information_schema')+'.'+QuoteIdent('SCHEMATA')+
        ' WHERE '+QuoteIdent('SCHEMA_NAME')+'='+EscapeString(db));
    except
      Result.FCollation := '';
    end;
    Results := nil;
    rx := TRegExpr.Create;
    rx.ModifierI := True;

    // Tables and views
    try
      Results := GetResults('SHOW TABLE STATUS FROM '+QuoteIdent(db));
    except
      on E:EDatabaseError do;
    end;
    if Assigned(Results) then begin
      while not Results.Eof do begin
        obj := TDBObject.Create(Self);
        Result.Add(obj);
        obj.Name := Results.Col('Name');
        obj.Database := db;
        obj.Rows := StrToInt64Def(Results.Col('Rows'), -1);
        if (not Results.IsNull('Data_length')) and (not Results.IsNull('Index_length')) then begin
          Obj.Size := StrToInt64Def(Results.Col('Data_length'), 0) + StrToInt64Def(Results.Col('Index_length'), 0);
          Inc(Result.FDataSize, Obj.Size);
          Result.FLargestObjectSize := Max(Result.FLargestObjectSize, Obj.Size);
        end;
        Obj.NodeType := lntTable;
        if Results.IsNull(1) and Results.IsNull(2) then // Engine column is NULL for views
          Obj.NodeType := lntView;
        Obj.Created := ParseDateTime(Results.Col('Create_time'));
        Obj.Updated := ParseDateTime(Results.Col('Update_time'));
        if Results.ColExists('Type') then
          Obj.Engine := Results.Col('Type')
        else
          Obj.Engine := Results.Col('Engine');
        Obj.Comment := Results.Col('Comment');
        // Sanitize comment from automatically appendage
        rx.Expression := '(;\s*)?InnoDB\s*free\:.*$';
        Obj.Comment := rx.Replace(Obj.Comment, '', False);
        Obj.Version := StrToInt64Def(Results.Col('Version', True), Obj.Version);
        Obj.AutoInc := StrToInt64Def(Results.Col('Auto_increment'), Obj.AutoInc);
        Obj.RowFormat := Results.Col('Row_format');
        Obj.AvgRowLen := StrToInt64Def(Results.Col('Avg_row_length'), Obj.AvgRowLen);
        Obj.MaxDataLen := StrToInt64Def(Results.Col('Max_data_length'), Obj.MaxDataLen);
        Obj.IndexLen := StrToInt64Def(Results.Col('Index_length'), Obj.IndexLen);
        Obj.DataLen := StrToInt64Def(Results.Col('Data_length'), Obj.DataLen);
        Obj.DataFree := StrToInt64Def(Results.Col('Data_free'), Obj.DataFree);
        Obj.LastChecked := ParseDateTime(Results.Col('Check_time'));
        Obj.Collation := Results.Col('Collation', True);
        Obj.CheckSum := StrToInt64Def(Results.Col('Checksum', True), Obj.CheckSum);
        Obj.CreateOptions := Results.Col('Create_options');
        Results.Next;
      end;
      FreeAndNil(Results);
    end;

    // Stored functions
    if ServerVersionInt >= 50000 then try
      Results := GetResults('SHOW FUNCTION STATUS WHERE '+QuoteIdent('Db')+'='+EscapeString(db));
    except
      on E:EDatabaseError do;
    end;
    if Assigned(Results) then begin
      while not Results.Eof do begin
        obj := TDBObject.Create(Self);
        Result.Add(obj);
        obj.Name := Results.Col('Name');
        obj.Database := db;
        Obj.NodeType := lntFunction;
        Obj.Created := ParseDateTime(Results.Col('Created'));
        Obj.Updated := ParseDateTime(Results.Col('Modified'));
        Obj.Comment := Results.Col('Comment');
        Results.Next;
      end;
      FreeAndNil(Results);
    end;

    // Stored procedures
    if ServerVersionInt >= 50000 then try
      Results := GetResults('SHOW PROCEDURE STATUS WHERE '+QuoteIdent('Db')+'='+EscapeString(db));
    except
      on E:EDatabaseError do;
    end;
    if Assigned(Results) then begin
      while not Results.Eof do begin
        obj := TDBObject.Create(Self);
        Result.Add(obj);
        obj.Name := Results.Col('Name');
        obj.Database := db;
        Obj.NodeType := lntProcedure;
        Obj.Created := ParseDateTime(Results.Col('Created'));
        Obj.Updated := ParseDateTime(Results.Col('Modified'));
        Obj.Comment := Results.Col('Comment');
        Results.Next;
      end;
      FreeAndNil(Results);
    end;

    // Triggers
    if ServerVersionInt >= 50010 then try
      Results := GetResults('SHOW TRIGGERS FROM '+QuoteIdent(db));
    except
      on E:EDatabaseError do;
    end;
    if Assigned(Results) then begin
      while not Results.Eof do begin
        obj := TDBObject.Create(Self);
        Result.Add(obj);
        obj.Name := Results.Col('Trigger');
        obj.Database := db;
        Obj.NodeType := lntTrigger;
        Obj.Created := ParseDateTime(Results.Col('Created'));
        Obj.Comment := Results.Col('Timing')+' '+Results.Col('Event')+' in table '+QuoteIdent(Results.Col('Table'));
        Results.Next;
      end;
      FreeAndNil(Results);
    end;

    // Events
    if ServerVersionInt >= 50100 then try
      Results := GetResults('SHOW EVENTS FROM '+QuoteIdent(db));
    except
      on E:EDatabaseError do;
    end;
    if Assigned(Results) then begin
      while not Results.Eof do begin
        if Results.Col('Db') = db then begin
          Obj := TDBObject.Create(Self);
          Result.Add(obj);
          Obj.Name := Results.Col('Name');
          Obj.Database := db;
          Obj.NodeType := lntEvent;
        end;
        Results.Next;
      end;
      FreeAndNil(Results);
    end;

    // Find youngest last update
    for i:=0 to Result.Count-1 do
      Result.FLastUpdate := Max(Result.FLastUpdate, Max(Result[i].Updated, Result[i].Created));
    // Sort list like it get sorted in AnyGridCompareNodes
    Result.Sort;
    // Add list of objects in this database to cached list of all databases
    FDatabases.Add(Result);
    SetObjectNamesInSelectedDB;
  end;

end;


function TAdoDBConnection.GetDbObjects(db: String; Refresh: Boolean=False): TDBObjectList;
var
  obj: TDBObject;
  Results: TDBQuery;
  i: Integer;
  tp, FromClause, CreateCol, UpdateCol, TypeCol: String;
begin
  // Cache and return a db's table list
  if Refresh then
    ClearDbObjects(db);

  // Find list in cache
  Result := nil;
  for i:=0 to FDatabases.Count-1 do begin
    if FDatabases[i].Database = db then begin
      Result := FDatabases[i];
      break;
    end;
  end;

  if not Assigned(Result) then begin
    Result := TDBObjectList.Create(TDBObjectComparer.Create);
    Result.FLastUpdate := 0;
    Result.FDataSize := 0;
    Result.FDatabase := db;
    Results := nil;

    // Tables, views and procedures
    case ServerVersionInt of
      2000: begin
        FromClause := QuoteIdent(db)+'..'+QuoteIdent('sysobjects');
        CreateCol := 'crdate';
        UpdateCol := '';
        TypeCol := 'xtype';
      end
      else begin
        FromClause := QuoteIdent(db)+'.'+QuoteIdent('sys')+'.'+QuoteIdent('objects');
        CreateCol := 'create_date';
        UpdateCol := 'modify_date';
        TypeCol := 'type';
      end;
    end;
    try
      Results := GetResults('SELECT * FROM '+FromClause+
        ' WHERE '+QuoteIdent('type')+' IN ('+EscapeString('P')+', '+EscapeString('U')+', '+EscapeString('V')+')');
    except
      on E:EDatabaseError do;
    end;
    if Assigned(Results) then begin
      while not Results.Eof do begin
        obj := TDBObject.Create(Self);
        Result.Add(obj);
        obj.Name := Results.Col('name');
        obj.Created := ParseDateTime(Results.Col(CreateCol, True));
        obj.Updated := ParseDateTime(Results.Col(UpdateCol, True));
        obj.Database := db;
        tp := Trim(Results.Col(TypeCol, True));
        if tp = 'U' then
          obj.NodeType := lntTable
        else if tp = 'P' then
          obj.NodeType := lntProcedure
        else if tp = 'V' then
          obj.NodeType := lntView;
        Results.Next;
      end;
      FreeAndNil(Results);
    end;

    // Find youngest last update
    for i:=0 to Result.Count-1 do
      Result.FLastUpdate := Max(Result.FLastUpdate, Max(Result[i].Updated, Result[i].Created));
    // Sort list like it get sorted in AnyGridCompareNodes
    Result.Sort;
    // Add list of objects in this database to cached list of all databases
    FDatabases.Add(Result);
    SetObjectNamesInSelectedDB;
  end;

end;


procedure TDBConnection.SetObjectNamesInSelectedDB;
var
  i: Integer;
  Objects: TDBObjectList;
  ObjNames: String;
begin
  // Add object names to additional stringlist
  if Assigned(FObjectNamesInSelectedDB) then begin
    if DbObjectsCached(FDatabase) then begin
      Objects := GetDbObjects(FDatabase);
      for i:=0 to Objects.Count-1 do
        ObjNames := ObjNames + Objects[i].Name + CRLF;
    end else
      ObjNames := '';
    if FObjectNamesInSelectedDB.Text <> ObjNames then
      FObjectNamesInSelectedDB.Text := ObjNames;
  end;
end;


function TDBConnection.GetKeyColumns(Columns: TTableColumnList; Keys: TTableKeyList): TStringList;
var
  i: Integer;
  AllowsNull: Boolean;
  Key: TTableKey;
  Col: TTableColumn;
begin
  Result := TStringList.Create;
  // Find best key for updates
  // 1. round: find a primary key
  for Key in Keys do begin
    if Key.Name = 'PRIMARY' then
      Result.Assign(Key.Columns);
  end;
  if Result.Count = 0 then begin
    // no primary key available -> 2. round: find a unique key
    for Key in Keys do begin
      if Key.IndexType = UKEY then begin
        // We found a UNIQUE key - better than nothing. Check if one of the key
        // columns allows NULLs which makes it dangerous to use in UPDATES + DELETES.
        AllowsNull := False;
        for i:=0 to Key.Columns.Count-1 do begin
          for Col in Columns do begin
            if Col.Name = Key.Columns[i] then
              AllowsNull := Col.AllowNull;
            if AllowsNull then break;
          end;
          if AllowsNull then break;
        end;
        if not AllowsNull then begin
          Result.Assign(Key.Columns);
          break;
        end;
      end;
    end;
  end;
end;


function TDBConnection.DecodeAPIString(a: AnsiString): String;
begin
  if IsUnicode then
    Result := Utf8ToString(a)
  else
    Result := String(a);
end;


function TDBConnection.ConnectionInfo: TStringList;
var
  Infos, Val: String;
  rx: TRegExpr;

  function EvalBool(B: Boolean): String;
  begin
    if B then Result := 'Yes' else Result := 'No';
  end;
begin
  Log(lcDebug, 'Get connection details ...');
  Result := TStringList.Create;
  if Assigned(Parameters) then
    Result.Values['Hostname'] := Parameters.Hostname;
  Ping(False);
  Result.Values['Connected'] := EvalBool(FActive);
  if FActive then begin
    Result.Values['Real Hostname'] := FRealHostname;
    Result.Values['Server OS'] := ServerOS;
    Result.Values['Server version'] := FServerVersionUntouched;
    Result.Values['Connection port'] := IntToStr(Parameters.Port);
    Result.Values['Compressed protocol'] := EvalBool(Parameters.Compressed);
    Result.Values['Unicode enabled'] := EvalBool(IsUnicode);
    Result.Values['SSL enabled'] := EvalBool(IsSSL);
    case Parameters.NetTypeGroup of
      ngMySQL: begin
        Result.Values['Client version (libmysql)'] := DecodeApiString(mysql_get_client_info);
        Infos := DecodeApiString(mysql_stat((Self as TMySQLConnection).FHandle));
        rx := TRegExpr.Create;
        rx.ModifierG := False;
        rx.Expression := '(\S.*)\:\s+(\S*)(\s+|$)';
        if rx.Exec(Infos) then while True do begin
          Val := rx.Match[2];
          if LowerCase(rx.Match[1]) = 'uptime' then
            Val := FormatTimeNumber(StrToIntDef(Val, 0), True)
          else
            Val := FormatNumber(Val);
          Result.Values[rx.Match[1]] := Val;
          if not rx.ExecNext then
            break;
        end;
        rx.Free;
      end;

      ngMSSQL: ; // Nothing specific yet
    end;
  end;
end;


procedure TDBConnection.ParseTableStructure(CreateTable: String; Columns: TTableColumnList; Keys: TTableKeyList; ForeignKeys: TForeignKeyList);
var
  ColSpec: String;
  rx, rxCol: TRegExpr;
  i: Integer;
  InLiteral: Boolean;
  Col: TTableColumn;
  Key: TTableKey;
  ForeignKey: TForeignKey;
  Collations: TDBQuery;
begin
  Ping(True);
  if Assigned(Columns) then Columns.Clear;
  if Assigned(Keys) then Keys.Clear;
  if Assigned(ForeignKeys) then ForeignKeys.Clear;
  if CreateTable = '' then
    Exit;
  rx := TRegExpr.Create;
  rx.ModifierS := False;
  rx.ModifierM := True;
  rx.Expression := '^\s+[`"]([^`"]+)[`"]\s(\w+)';
  rxCol := TRegExpr.Create;
  rxCol.ModifierI := True;
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(Columns) then
      break;
    ColSpec := '';
    for i:=rx.MatchPos[2]+rx.MatchLen[2] to Length(CreateTable) do begin
      if CharInSet(CreateTable[i], [#13, #10]) then
        break;
      ColSpec := ColSpec + CreateTable[i];
    end;

    // Strip trailing comma
    if (ColSpec <> '') and (ColSpec[Length(ColSpec)] = ',') then
      Delete(ColSpec, Length(ColSpec), 1);

    Col := TTableColumn.Create(Self);
    Columns.Add(Col);
    Col.Name := DeQuoteIdent(rx.Match[1]);
    Col.OldName := Col.Name;
    Col.Status := esUntouched;
    Col.LengthCustomized := True;

    // Datatype
    Col.DataType := GetDatatypeByName(UpperCase(rx.Match[2]));
    Col.OldDataType := GetDatatypeByName(UpperCase(rx.Match[2]));

    // Length / Set
    // Various datatypes, e.g. BLOBs, don't have any length property
    InLiteral := False;
    if (ColSpec <> '') and (ColSpec[1] = '(') then begin
      for i:=2 to Length(ColSpec) do begin
        if (ColSpec[i] = ')') and (not InLiteral) then
          break;
        if ColSpec[i] = '''' then
          InLiteral := not InLiteral;
      end;
      Col.LengthSet := Copy(ColSpec, 2, i-2);
      Delete(ColSpec, 1, i);
    end;
    ColSpec := Trim(ColSpec);

    // Unsigned
    if UpperCase(Copy(ColSpec, 1, 8)) = 'UNSIGNED' then begin
      Col.Unsigned := True;
      Delete(ColSpec, 1, 9);
    end else
      Col.Unsigned := False;

    // Zero fill
    if UpperCase(Copy(ColSpec, 1, 8)) = 'ZEROFILL' then begin
      Col.ZeroFill := True;
      Delete(ColSpec, 1, 9);
    end else
      Col.ZeroFill := False;

    // Charset
    rxCol.Expression := '^CHARACTER SET (\w+)\b\s*';
    if rxCol.Exec(ColSpec) then begin
      Col.Charset := rxCol.Match[1];
      Delete(ColSpec, 1, rxCol.MatchLen[0]);
    end;

    // Virtual columns
    rxCol.Expression := '^AS \((.+)\)\s+(VIRTUAL|PERSISTENT)\s*';
    if rxCol.Exec(ColSpec) then begin
      Col.Expression := rxCol.Match[1];
      Col.Virtuality := rxCol.Match[2];
      Delete(ColSpec, 1, rxCol.MatchLen[0]);
    end;

    // Collation - probably not present when charset present
    rxCol.Expression := '^COLLATE (\w+)\b\s*';
    if rxCol.Exec(ColSpec) then begin
      Col.Collation := rxCol.Match[1];
      Delete(ColSpec, 1, rxCol.MatchLen[0]);
    end;
    if Col.Collation = '' then begin
      if not Assigned(Collations) then
        Collations := CollationTable;
      if Assigned(Collations) then begin
        Collations.First;
        while not Collations.Eof do begin
          if (Collations.Col('Charset') = Col.Charset) and (Collations.Col('Default') = 'Yes') then begin
            Col.Collation := Collations.Col('Collation');
            break;
          end;
          Collations.Next;
        end;
      end;
    end;

    // Allow NULL
    if UpperCase(Copy(ColSpec, 1, 8)) = 'NOT NULL' then begin
      Col.AllowNull := False;
      Delete(ColSpec, 1, 9);
    end else begin
      Col.AllowNull := True;
      // Sporadically there is a "NULL" found at this position.
      if UpperCase(Copy(ColSpec, 1, 4)) = 'NULL' then
        Delete(ColSpec, 1, 5);
    end;

    // Default value
    Col.DefaultType := cdtNothing;
    Col.DefaultText := '';
    if UpperCase(Copy(ColSpec, 1, 14)) = 'AUTO_INCREMENT' then begin
      Col.DefaultType := cdtAutoInc;
      Col.DefaultText := 'AUTO_INCREMENT';
      Delete(ColSpec, 1, 15);
    end else if UpperCase(Copy(ColSpec, 1, 8)) = 'DEFAULT ' then begin
      Delete(ColSpec, 1, 8);
      if UpperCase(Copy(ColSpec, 1, 4)) = 'NULL' then begin
        Col.DefaultType := cdtNull;
        Col.DefaultText := 'NULL';
        Delete(ColSpec, 1, 5);
      end else if UpperCase(Copy(ColSpec, 1, 17)) = 'CURRENT_TIMESTAMP' then begin
        Col.DefaultType := cdtCurTS;
        Col.DefaultText := 'CURRENT_TIMESTAMP';
        Delete(ColSpec, 1, 18);
      end else if ColSpec[1] = '''' then begin
        InLiteral := True;
        for i:=2 to Length(ColSpec) do begin
          if ColSpec[i] = '''' then
            InLiteral := not InLiteral
          else if not InLiteral then
            break;
        end;
        Col.DefaultType := cdtText;
        Col.DefaultText := Copy(ColSpec, 2, i-3);
        // A single quote gets escaped by single quote - remove the escape char - escaping is done in Save action afterwards
        Col.DefaultText := StringReplace(Col.DefaultText, '''''', '''', [rfReplaceAll]);
        Delete(ColSpec, 1, i);
      end;
    end;
    if UpperCase(Copy(ColSpec, 1, 27)) = 'ON UPDATE CURRENT_TIMESTAMP' then begin
      // Adjust default type
      case Col.DefaultType of
        cdtText: Col.DefaultType := cdtTextUpdateTS;
        cdtNull: Col.DefaultType := cdtNullUpdateTS;
        cdtCurTS: Col.DefaultType := cdtCurTSUpdateTS;
      end;
      Delete(ColSpec, 1, 28);
    end;

    // Comment
    if UpperCase(Copy(ColSpec, 1, 9)) = 'COMMENT ''' then begin
      InLiteral := True;
      for i:=10 to Length(ColSpec) do begin
        if ColSpec[i] = '''' then
          InLiteral := not InLiteral
        else if not InLiteral then
          break;
      end;
      Col.Comment := Copy(ColSpec, 10, i-11);
      Col.Comment := StringReplace(Col.Comment, '''''', '''', [rfReplaceAll]);
      Delete(ColSpec, 1, i);
    end;

    if not rx.ExecNext then
      break;
  end;

  // Detect keys
  // PRIMARY KEY (`id`), UNIQUE KEY `id` (`id`), KEY `id_2` (`id`) USING BTREE,
  // KEY `Text` (`Text`(100)), FULLTEXT KEY `Email` (`Email`,`Text`)
  rx.Expression := '^\s+((\w+)\s+)?KEY\s+([`"]?([^`"]+)[`"]?\s+)?(USING\s+(\w+)\s+)?\((.+)\)(\s+USING\s+(\w+))?,?$';
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(Keys) then
      break;
    Key := TTableKey.Create(Self);
    Keys.Add(Key);
    Key.Name := rx.Match[4];
    if Key.Name = '' then Key.Name := rx.Match[2]; // PRIMARY
    Key.OldName := Key.Name;
    Key.IndexType := rx.Match[2];
    Key.OldIndexType := Key.IndexType;
    if rx.Match[6] <> '' then // 5.0 and below show USING ... before column list
      Key.Algorithm := rx.Match[6]
    else
      Key.Algorithm := rx.Match[9];
    if Key.IndexType = '' then Key.IndexType := 'KEY'; // KEY
    Key.Columns := Explode(',', rx.Match[7]);
    for i:=0 to Key.Columns.Count-1 do begin
      rxCol.Expression := '^[`"]?([^`"]+)[`"]?(\((\d+)\))?$';
      if rxCol.Exec(Key.Columns[i]) then begin
        Key.Columns[i] := rxCol.Match[1];
        Key.SubParts.Add(rxCol.Match[3]);
      end;
    end;
    if not rx.ExecNext then
      break;
  end;

  // Detect foreign keys
  // CONSTRAINT `FK1` FOREIGN KEY (`which`) REFERENCES `fk1` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
  rx.Expression := '\s+CONSTRAINT\s+[`"]([^`"]+)[`"]\sFOREIGN KEY\s+\(([^\)]+)\)\s+REFERENCES\s+[`"]([^\(]+)[`"]\s\(([^\)]+)\)(\s+ON DELETE (RESTRICT|CASCADE|SET NULL|NO ACTION))?(\s+ON UPDATE (RESTRICT|CASCADE|SET NULL|NO ACTION))?';
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(ForeignKeys) then
      break;
    ForeignKey := TForeignKey.Create(Self);
    ForeignKeys.Add(ForeignKey);
    ForeignKey.KeyName := rx.Match[1];
    ForeignKey.OldKeyName := ForeignKey.KeyName;
    ForeignKey.KeyNameWasCustomized := True;
    ForeignKey.ReferenceTable := StringReplace(rx.Match[3], '`', '', [rfReplaceAll]);
    ForeignKey.ReferenceTable := StringReplace(ForeignKey.ReferenceTable, '"', '', [rfReplaceAll]);
    ExplodeQuotedList(rx.Match[2], ForeignKey.Columns);
    ExplodeQuotedList(rx.Match[4], ForeignKey.ForeignColumns);
    if rx.Match[6] <> '' then
      ForeignKey.OnDelete := rx.Match[6];
    if rx.Match[8] <> '' then
      ForeignKey.OnUpdate := rx.Match[8];
    if not rx.ExecNext then
      break;
  end;

  FreeAndNil(rxCol);
  FreeAndNil(rx);
end;


procedure TDBConnection.ParseViewStructure(CreateCode, ViewName: String; Columns: TTableColumnList; var Algorithm, Definer, CheckOption, SelectCode: String);
var
  rx: TRegExpr;
  Col: TTableColumn;
  Results: TDBQuery;
  DbName, DbAndViewName: String;
begin
  if CreateCode <> '' then begin
    // CREATE
    //   [OR REPLACE]
    //   [ALGORITHM = {UNDEFINED | MERGE | TEMPTABLE}]
    //   [DEFINER = { user | CURRENT_USER }]
    //   [SQL SECURITY { DEFINER | INVOKER }]
    //   VIEW view_name [(column_list)]
    //   AS select_statement
    //   [WITH [CASCADED | LOCAL] CHECK OPTION]
    rx := TRegExpr.Create;
    rx.ModifierG := False;
    rx.ModifierI := True;
    rx.Expression := '^CREATE\s+(OR\s+REPLACE\s+)?'+
      '(ALGORITHM\s*=\s*(\w*)\s*)?'+
      '(DEFINER\s*=\s*(\S+)\s+)?'+
      '(SQL\s+SECURITY\s+\w+\s+)?'+
      'VIEW\s+(([^\.]+)\.)?([^\.]+)\s+'+
      '(\([^\)]\)\s+)?'+
      'AS\s+(.+)(\s+WITH\s+(\w+\s+)?CHECK\s+OPTION\s*)?$';
    if rx.Exec(CreateCode) then begin
      Algorithm := rx.Match[3];
      Definer := DeQuoteIdent(rx.Match[5], '@');
      // When exporting a view we need the db name for the below SHOW COLUMNS query,
      // if the connection is on a different db currently
      DbName := DeQuoteIdent(rx.Match[8]);
      ViewName := DeQuoteIdent(rx.Match[9]);
      CheckOption := Trim(rx.Match[13]);
      SelectCode := rx.Match[11];
    end else
      raise Exception.Create('Regular expression did not match the VIEW code in ParseViewStructure(): '+CRLF+CRLF+CreateCode);
    rx.Free;
  end;

  // Views reveal their columns only with a SHOW COLUMNS query.
  // No keys available in views - SHOW KEYS always returns an empty result
  if Assigned(Columns) then begin
    Columns.Clear;
    rx := TRegExpr.Create;
    rx.Expression := '^(\w+)(\((.+)\))?';
    if DbName <> '' then
      DbAndViewName := QuoteIdent(DbName)+'.';
    DbAndViewName := DbAndViewName + QuoteIdent(ViewName);
    Results := GetResults('SHOW /*!32332 FULL */ COLUMNS FROM '+DbAndViewName);
    while not Results.Eof do begin
      Col := TTableColumn.Create(Self);
      Columns.Add(Col);
      Col.Name := Results.Col('Field');
      Col.AllowNull := Results.Col('Null') = 'YES';
      if rx.Exec(Results.Col('Type')) then begin
        Col.DataType := GetDatatypeByName(rx.Match[1]);
        Col.LengthSet := rx.Match[3];
      end;
      Col.Unsigned := (Col.DataType.Category = dtcInteger) and (Pos('unsigned', Results.Col('Type')) > 0);
      Col.AllowNull := UpperCase(Results.Col('Null')) = 'YES';
      Col.Collation := Results.Col('Collation', True);
      Col.Comment := Results.Col('Comment', True);
      Col.DefaultText := Results.Col('Default');
      if Results.IsNull('Default') then begin
        if Col.AllowNull then
          Col.DefaultType := cdtNull
        else
          Col.DefaultType := cdtNothing;
      end else if Col.DataType.Index = dtTimestamp then
        Col.DefaultType := cdtCurTSUpdateTS
      else
        Col.DefaultType := cdtText;
      Results.Next;
    end;
    rx.Free;
  end;
end;


procedure TDBConnection.ParseRoutineStructure(CreateCode: String; Parameters: TRoutineParamList;
  var Deterministic: Boolean; var Definer, Returns, DataAccess, Security, Comment, Body: String);
var
  Params: String;
  ParenthesesCount: Integer;
  rx: TRegExpr;
  i: Integer;
  Param: TRoutineParam;
begin
  // Parse CREATE code of stored function or procedure to detect parameters
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.ModifierG := True;
  // CREATE DEFINER=`root`@`localhost` PROCEDURE `bla2`(IN p1 INT, p2 VARCHAR(20))
  // CREATE DEFINER=`root`@`localhost` FUNCTION `test3`(`?b` varchar(20)) RETURNS tinyint(4)
  // CREATE DEFINER=`root`@`localhost` PROCEDURE `test3`(IN `Param1` int(1) unsigned)

  rx.Expression := '\bDEFINER\s*=\s*(\S+)\s';
  if rx.Exec(CreateCode) then
    Definer := DequoteIdent(rx.Match[1], '@')
  else
    Definer := '';

  // Parse parameter list
  ParenthesesCount := 0;
  Params := '';
  for i:=1 to Length(CreateCode) do begin
    if CreateCode[i] = ')' then begin
      Dec(ParenthesesCount);
      if ParenthesesCount = 0 then
        break;
    end;
    if ParenthesesCount >= 1 then
      Params := Params + CreateCode[i];
    if CreateCode[i] = '(' then
      Inc(ParenthesesCount);
  end;
  rx.Expression := '(^|,)\s*((IN|OUT|INOUT)\s+)?(\S+)\s+([^\s,\(]+(\([^\)]*\))?[^,]*)';
  if rx.Exec(Params) then while true do begin
    Param := TRoutineParam.Create;
    Param.Context := UpperCase(rx.Match[3]);
    if Param.Context = '' then
      Param.Context := 'IN';
    Param.Name := DeQuoteIdent(rx.Match[4]);
    Param.Datatype := rx.Match[5];
    Parameters.Add(Param);
    if not rx.ExecNext then
      break;
  end;

  // Cut left part including parameters, so it's easier to parse the rest
  CreateCode := Copy(CreateCode, i+1, MaxInt);
  // CREATE PROCEDURE sp_name ([proc_parameter[,...]]) [characteristic ...] routine_body
  // CREATE FUNCTION sp_name ([func_parameter[,...]]) RETURNS type [characteristic ...] routine_body
  // LANGUAGE SQL
  //  | [NOT] DETERMINISTIC                                              // IS_DETERMINISTIC
  //  | { CONTAINS SQL | NO SQL | READS SQL DATA | MODIFIES SQL DATA }   // DATA_ACCESS
  //  | SQL SECURITY { DEFINER | INVOKER }                               // SECURITY_TYPE
  //  | COMMENT 'string'                                                 // COMMENT

  rx.Expression := '\bLANGUAGE SQL\b';
  if rx.Exec(CreateCode) then
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  rx.Expression := '\bRETURNS\s+(\w+(\([^\)]*\))?(\s+UNSIGNED)?)';
  if rx.Exec(CreateCode) then begin
    Returns := rx.Match[1];
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.Expression := '\b(NOT\s+)?DETERMINISTIC\b';
  if rx.Exec(CreateCode) then begin
    Deterministic := rx.MatchLen[1] = -1;
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.Expression := '\b(CONTAINS SQL|NO SQL|READS SQL DATA|MODIFIES SQL DATA)\b';
  if rx.Exec(CreateCode) then begin
    DataAccess := rx.Match[1];
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.Expression := '\bSQL\s+SECURITY\s+(DEFINER|INVOKER)\b';
  if rx.Exec(CreateCode) then begin
    Security := rx.Match[1];
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.ModifierG := False;
  rx.Expression := '\bCOMMENT\s+''((.+)[^''])''[^'']';
  if rx.Exec(CreateCode) then begin
    Comment := StringReplace(rx.Match[1], '''''', '''', [rfReplaceAll]);
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]-1);
  end;
  rx.Expression := '^\s*CHARSET\s+[\w\d]+\s';
  if rx.Exec(CreateCode) then
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]-1);
  // Tata, remaining code is the routine body
  Body := TrimLeft(CreateCode);

  rx.Free;
end;


function TDBConnection.ApplyLimitClause(QueryType, QueryBody: String; Limit, Offset: Cardinal): String;
begin
  Result := QueryType + ' ';
  if FParameters.NetTypeGroup = ngMSSQL then
    Result := Result + 'TOP('+IntToStr(Limit)+') ';
  Result := Result + QueryBody;
  if FParameters.NetTypeGroup = ngMySQL then begin
    Result := Result + ' LIMIT ';
    if Offset > 0 then
      Result := Result + IntToStr(Offset) + ', ';
    Result := Result + IntToStr(Limit);
  end;
end;



{ TMySQLQuery }

constructor TDBQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecNo := -1;
  FRecordCount := 0;
  FColumnNames := TStringList.Create;
  FColumnNames.CaseSensitive := True;
  FColumnOrgNames := TStringList.Create;
  FColumnOrgNames.CaseSensitive := True;
  FStoreResult := True;
end;


destructor TDBQuery.Destroy;
begin
  FreeAndNil(FColumnNames);
  FreeAndNil(FColumnOrgNames);
  FreeAndNil(FColumns);
  FreeAndNil(FKeys);
  FreeAndNil(FUpdateData);
  SetLength(FColumnFlags, 0);
  SetLength(FColumnLengths, 0);
  SetLength(FColumnTypes, 0);
  FSQL := '';
  FRecordCount := 0;
  inherited;
end;


destructor TMySQLQuery.Destroy;
var
  i: Integer;
begin
  if HasResult then for i:=Low(FResultList) to High(FResultList) do
    mysql_free_result(FResultList[i]);
  SetLength(FResultList, 0);
  inherited;
end;


destructor TAdoDBQuery.Destroy;
var
  i: Integer;
begin
  if HasResult then for i:=Low(FResultList) to High(FResultList) do begin
    FResultList[i].Close;
    FResultList[i].Free;
  end;
  SetLength(FResultList, 0);
  inherited;
end;


procedure TMySQLQuery.Execute(AddResult: Boolean=False; UseRawResult: Integer=-1);
var
  i, j, NumFields: Integer;
  NumResults: Int64;
  Field: PMYSQL_FIELD;
  IsBinary: Boolean;
  LastResult: PMYSQL_RES;
begin
  // Execute a query, or just take over one of the last result pointers
  if UseRawResult = -1 then begin
    Connection.Query(FSQL, FStoreResult);
    UseRawResult := 0;
  end;
  if Connection.ResultCount > UseRawResult then
    LastResult := TMySQLConnection(Connection).LastRawResults[UseRawResult]
  else
    LastResult := nil;
  if AddResult and (Length(FResultList) = 0) then
    AddResult := False;
  if AddResult then
    NumResults := Length(FResultList)+1
  else begin
    for i:=Low(FResultList) to High(FResultList) do
      mysql_free_result(FResultList[i]);
    NumResults := 1;
    FRecordCount := 0;
    FEditingPrepared := False;
  end;
  if LastResult <> nil then begin
    Connection.Log(lcDebug, 'Result #'+IntToStr(NumResults)+' fetched.');
    SetLength(FResultList, NumResults);
    FResultList[NumResults-1] := LastResult;
    FRecordCount := FRecordCount + LastResult.row_count;
  end;
  if not AddResult then begin
    if HasResult then begin
      // FCurrentResults is normally done in SetRecNo, but never if result has no rows
      FCurrentResults := LastResult;
      NumFields := mysql_num_fields(LastResult);
      SetLength(FColumnTypes, NumFields);
      SetLength(FColumnLengths, NumFields);
      SetLength(FColumnFlags, NumFields);
      FColumnNames.Clear;
      FColumnOrgNames.Clear;
      for i:=0 to NumFields-1 do begin
        Field := mysql_fetch_field_direct(LastResult, i);
        FColumnNames.Add(Connection.DecodeAPIString(Field.name));
        if Connection.ServerVersionInt >= 40100 then
          FColumnOrgNames.Add(Connection.DecodeAPIString(Field.org_name))
        else
          FColumnOrgNames.Add(Connection.DecodeAPIString(Field.name));
        FColumnFlags[i] := Field.flags;
        FColumnTypes[i] := FConnection.Datatypes[0];
        for j:=0 to High(FConnection.Datatypes) do begin
          if (Field.flags and ENUM_FLAG) = ENUM_FLAG then begin
            if FConnection.Datatypes[j].Index = dtEnum then
              FColumnTypes[i] := FConnection.Datatypes[j];
          end else if (Field.flags and SET_FLAG) = SET_FLAG then begin
            if FConnection.Datatypes[j].Index = dtSet then
              FColumnTypes[i] := FConnection.Datatypes[j];
          end else if Field._type = FConnection.Datatypes[j].NativeType then begin
            // Text and Blob types share the same constants (see FIELD_TYPEs)
            // Some function results return binary collation up to the latest versions. Work around
            // that by checking if this field is a real table field
            // See http://bugs.mysql.com/bug.php?id=10201
            if Connection.IsUnicode then
              IsBinary := (Field.charsetnr = COLLATION_BINARY) and (Field.org_table <> '')
            else
              IsBinary := (Field.flags and BINARY_FLAG) = BINARY_FLAG;
            if IsBinary and (FConnection.Datatypes[j].Category = dtcText) then
              continue;
            FColumnTypes[i] := FConnection.Datatypes[j];
            break;
          end;
        end;
      end;
      FRecNo := -1;
      First;
    end else begin
      SetLength(FColumnTypes, 0);
      SetLength(FColumnLengths, 0);
      SetLength(FColumnFlags, 0);
    end;
  end;
end;


procedure TAdoDBQuery.Execute(AddResult: Boolean=False; UseRawResult: Integer=-1);
var
  NumFields, i, j: Integer;
  TypeIndex: TDBDatatypeIndex;
  LastResult: TAdoQuery;
  NumResults: Int64;
begin
  // TODO: Handle multiple results
  if UseRawResult = -1 then begin
    Connection.Query(FSQL, FStoreResult);
    UseRawResult := 0;
  end;
  if Connection.ResultCount > UseRawResult then begin
    LastResult := TAdoQuery.Create(Self);
    LastResult.Recordset := TAdoDBConnection(Connection).LastRawResults[UseRawResult];
    LastResult.Open;
  end else
    LastResult := nil;
  if AddResult and (Length(FResultList) = 0) then
    AddResult := False;
  if AddResult then
    NumResults := Length(FResultList)+1
  else begin
    for i:=Low(FResultList) to High(FResultList) do begin
      FResultList[i].Close;
      FResultList[i].Free;
    end;
    NumResults := 1;
    FRecordCount := 0;
    FEditingPrepared := False;
  end;
  if LastResult <> nil then begin
    Connection.Log(lcDebug, 'Result #'+IntToStr(NumResults)+' fetched.');
    SetLength(FResultList, NumResults);
    FResultList[NumResults-1] := LastResult;
    FRecordCount := FRecordCount + LastResult.RecordCount;
  end;

  // Set up columns and data types
  if not AddResult then begin
    if HasResult then begin
      FCurrentResults := LastResult;
      NumFields := LastResult.FieldCount;
      SetLength(FColumnTypes, NumFields);
      SetLength(FColumnLengths, NumFields);
      SetLength(FColumnFlags, NumFields);
      FColumnNames.Clear;
      FColumnOrgNames.Clear;
      for i:=0 to NumFields-1 do begin
        FColumnNames.Add(LastResult.Fields[i].FieldName);
        FColumnOrgNames.Add(FColumnNames[i]);
        { ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
          ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
          ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
          ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
          ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
          ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
          ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval, // 38..41
          ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream, //42..48
          ftTimeStampOffset, ftObject, ftSingle //49..51 }
        case LastResult.Fields[i].DataType of
          ftSmallint, ftWord:
            TypeIndex := dtMediumInt;
          ftInteger, ftAutoInc:
            TypeIndex := dtInt;
          ftLargeint:
            TypeIndex := dtBigInt;
          ftBCD, ftFMTBcd:
            TypeIndex := dtDecimal;
          ftFixedChar:
            TypeIndex := dtChar;
          ftString, ftWideString, ftBoolean, ftGuid:
            TypeIndex := dtVarchar;
          ftMemo, ftWideMemo:
            TypeIndex := dtMediumText;
          ftBlob, ftVariant:
            TypeIndex := dtMediumBlob;
          ftBytes:
            TypeIndex := dtBinary;
          ftVarBytes:
            TypeIndex := dtVarbinary;
          ftFloat:
            TypeIndex := dtEnum;
          ftDate:
            TypeIndex := dtDate;
          ftTime:
            TypeIndex := dtTime;
          ftDateTime:
            TypeIndex := dtDateTime;
          else
            raise EDatabaseError.Create('Unknown data type for column #'+IntToStr(i)+' - '+FColumnNames[i]+': '+IntToStr(Integer(LastResult.Fields[i].DataType)));
        end;
        for j:=0 to High(FConnection.DataTypes) do begin
          if TypeIndex = FConnection.DataTypes[j].Index then
            FColumnTypes[i] := FConnection.DataTypes[j];
        end;

      end;
      FRecNo := -1;
      First;
    end else begin
      SetLength(FColumnTypes, 0);
      SetLength(FColumnLengths, 0);
      SetLength(FColumnFlags, 0);
    end;
  end;
end;


procedure TDBQuery.SetColumnOrgNames(Value: TStringList);
begin
  // Retrieve original column names from caller
  FColumnOrgNames.Text := Value.Text;
end;


procedure TDBQuery.First;
begin
  RecNo := 0;
end;


procedure TDBQuery.Next;
begin
  RecNo := RecNo + 1;
end;


procedure TMySQLQuery.SetRecNo(Value: Int64);
var
  LengthPointer: PLongInt;
  i, j: Integer;
  NumRows, WantedLocalRecNo: Int64;
  Row: TRowData;
  RowFound: Boolean;
begin
  if Value = FRecNo then
    Exit;
  if (not FEditingPrepared) and (Value >= RecordCount) then begin
    FRecNo := RecordCount;
    FEof := True;
  end else begin

    // Find row in edited data
    RowFound := False;
    if FEditingPrepared then begin
      for Row in FUpdateData do begin
        if Row.RecNo = Value then begin
          FCurrentRow := nil;
          FCurrentUpdateRow := Row;
          for i:=Low(FColumnLengths) to High(FColumnLengths) do
            FColumnLengths[i] := Length(FCurrentUpdateRow[i].NewText);
          RowFound := True;
          break;
        end;
      end;
    end;

    // Row not edited data - find it in normal result
    if not RowFound then begin
      NumRows := 0;
      for i:=Low(FResultList) to High(FResultList) do begin
        Inc(NumRows, FResultList[i].row_count);
        if NumRows > Value then begin
          FCurrentResults := FResultList[i];
          // Do not seek if FCurrentRow points to the previous row of the wanted row
          WantedLocalRecNo := FCurrentResults.row_count-(NumRows-Value);
          if (WantedLocalRecNo = 0) or (FRecNo+1 <> Value) or (FCurrentRow = nil) then
            mysql_data_seek(FCurrentResults, WantedLocalRecNo);
          FCurrentRow := mysql_fetch_row(FCurrentResults);
          FCurrentUpdateRow := nil;
          // Remember length of column contents. Important for Col() so contents of cells with #0 chars are not cut off
          LengthPointer := mysql_fetch_lengths(FCurrentResults);
          for j:=Low(FColumnLengths) to High(FColumnLengths) do
            FColumnLengths[j] := PInteger(Integer(LengthPointer) + j * SizeOf(Integer))^;
          break;
        end;
      end;
    end;

    FRecNo := Value;
    FEof := False;
  end;
end;


procedure TAdoDBQuery.SetRecNo(Value: Int64);
var
  i, j: Integer;
  RowFound: Boolean;
  Row: TRowData;
  NumRows, WantedLocalRecNo: Int64;
begin
  if Value = FRecNo then
    Exit;
  if (not FEditingPrepared) and (Value >= RecordCount) then begin
    FRecNo := RecordCount;
    FEof := True;
    FCurrentResults.Last;
  end else begin

    // Find row in edited data
    RowFound := False;
    if FEditingPrepared then begin
      for Row in FUpdateData do begin
        if Row.RecNo = Value then begin
          FCurrentUpdateRow := Row;
          for i:=Low(FColumnLengths) to High(FColumnLengths) do
            FColumnLengths[i] := Length(FCurrentUpdateRow[i].NewText);
          RowFound := True;
          break;
        end;
      end;
    end;

    // Row not edited data - find it in normal result
    if not RowFound then begin
      NumRows := 0;
      for i:=Low(FResultList) to High(FResultList) do begin
        Inc(NumRows, FResultList[i].RecordCount);
        if NumRows > Value then begin
          FCurrentResults := FResultList[i];
          WantedLocalRecNo := FCurrentResults.RecordCount-(NumRows-Value);
          FCurrentResults.RecNo := WantedLocalRecNo+1;
          FCurrentUpdateRow := nil;
          for j:=Low(FColumnLengths) to High(FColumnLengths) do
            FColumnLengths[j] := FCurrentResults.Fields[j].DataSize;
          break;
        end;
      end;
    end;

    FRecNo := Value;
    FEof := False;
  end;
end;


function TDBQuery.ColumnCount: Integer;
begin
  Result := ColumnNames.Count;
end;


function TMySQLQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
var
  AnsiStr: AnsiString;
  BitString: String;
  NumBit: Integer;
  ByteVal: Byte;
  c: Char;
begin
  if (Column > -1) and (Column < ColumnCount) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      // Row was edited and only valid in a TRowData
      Result := FCurrentUpdateRow[Column].NewText;
    end else begin
      // The normal case: Fetch cell from mysql result
      SetString(AnsiStr, FCurrentRow[Column], FColumnLengths[Column]);
      if Datatype(Column).Category in [dtcBinary, dtcSpatial] then
        Result := String(AnsiStr)
      else
        Result := Connection.DecodeAPIString(AnsiStr);
      // Create string bitmask for BIT fields
      if Datatype(Column).Index = dtBit then begin
        for c in Result do begin
          ByteVal := Byte(c);
          BitString := '';
          for NumBit:=0 to 7 do begin
            if (ByteVal shr NumBit and $1) = $1 then
              BitString := BitString + '1'
            else
              BitString := BitString + '0';
            if Length(BitString) >= MaxLength(Column) then
              break;
          end;
          if Length(BitString) >= MaxLength(Column) then
            break;
        end;
        Result := BitString;
      end;

    end;
  end else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt(MsgInvalidColumn, [Column, ColumnCount, RecordCount]);
end;


function TAdoDBQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
begin
  if (Column > -1) and (Column < ColumnCount) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      Result := FCurrentUpdateRow[Column].NewText;
    end else begin
      try
        Result := FCurrentResults.Fields[Column].AsString;
      except
        Result := String(FCurrentResults.Fields[Column].AsAnsiString);
      end;
    end;
  end else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt(MsgInvalidColumn, [Column, ColumnCount, RecordCount]);
end;


function TDBQuery.Col(ColumnName: String; IgnoreErrors: Boolean=False): String;
var
  idx: Integer;
begin
  idx := ColumnNames.IndexOf(ColumnName);
  if idx > -1 then
    Result := Col(idx)
  else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt('Column "%s" not available.', [ColumnName]);
end;


function TDBQuery.ColumnLengths(Column: Integer): Int64;
begin
  Result := FColumnLengths[Column];
end;


function TDBQuery.HexValue(Column: Integer; IgnoreErrors: Boolean=False): String;
begin
  // Return a binary column value as hex AnsiString
  Result := HexValue(Col(Column, IgnoreErrors));
end;


function TDBQuery.HexValue(BinValue: String): String;
var
  BinLen: Integer;
  Ansi: AnsiString;
begin
  // Return a binary value as hex AnsiString
  Ansi := AnsiString(BinValue);
  BinLen := Length(Ansi);
  if BinLen = 0 then begin
    Result := '';
  end else begin
    SetLength(Result, BinLen*2);
    BinToHex(PAnsiChar(Ansi), PChar(Result), BinLen);
    Result := '0x' + Result;
  end;
end;


function TDBQuery.DataType(Column: Integer): TDBDataType;
var
  Col: TTableColumn;
begin
  Col := ColAttributes(Column);
  if Assigned(Col) then
    Result := Col.DataType
  else
    Result := FColumnTypes[Column];
end;


function TDBQuery.MaxLength(Column: Integer): Int64;
var
  ColAttr: TTableColumn;
begin
  // Return maximum posible length of values in given columns
  // Note: PMYSQL_FIELD.max_length holds the maximum existing value in that column, which is useless here
  Result := MaxInt;
  ColAttr := ColAttributes(Column);
  if Assigned(ColAttr) then begin
    case ColAttr.DataType.Index of
      dtChar, dtVarchar, dtBinary, dtVarBinary, dtBit: Result := MakeInt(ColAttr.LengthSet);
      dtTinyText, dtTinyBlob: Result := 255;
      dtText, dtBlob: Result := 65535;
      dtMediumText, dtMediumBlob: Result := 16777215;
      dtLongText, dtLongBlob: Result := 4294967295;
    end;
  end;
end;


function TDBQuery.ValueList(Column: Integer): TStringList;
var
  ColAttr: TTableColumn;
begin
  Result := TStringList.Create;
  Result.QuoteChar := '''';
  Result.Delimiter := ',';
  ColAttr := ColAttributes(Column);
  if Assigned(ColAttr) and (ColAttr.DataType.Index in [dtEnum, dtSet]) then
    Result.DelimitedText := ColAttr.LengthSet;
end;


function TDBQuery.ColAttributes(Column: Integer): TTableColumn;
var
  i: Integer;
begin
  Result := nil;
  if (Column = -1) or (Column >= FColumnOrgNames.Count) then
    raise EDatabaseError.Create('Column #'+IntToStr(Column)+' not available.');
  if FEditingPrepared then begin
    for i:=0 to FColumns.Count-1 do begin
      if FColumns[i].Name = FColumnOrgNames[Column] then begin
        Result := FColumns[i];
        break;
      end;
    end;
  end;
end;


function TDBQuery.ColExists(Column: String): Boolean;
begin
  Result := (ColumnNames <> nil) and (ColumnNames.IndexOf(Column) > -1);
end;


function TMySQLQuery.ColIsPrimaryKeyPart(Column: Integer): Boolean;
begin
  Result := (FColumnFlags[Column] and PRI_KEY_FLAG) = PRI_KEY_FLAG;
end;


function TAdoDBQuery.ColIsPrimaryKeyPart(Column: Integer): Boolean;
begin
//  Result := FCurrentResults.Fields[0].KeyFields
  Result := False;
end;


function TMySQLQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
begin
  Result := (FColumnFlags[Column] and UNIQUE_KEY_FLAG) = UNIQUE_KEY_FLAG;
end;


function TAdoDBQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
begin
  Result := False;
end;


function TMySQLQuery.ColIsKeyPart(Column: Integer): Boolean;
begin
  Result := (FColumnFlags[Column] and MULTIPLE_KEY_FLAG) = MULTIPLE_KEY_FLAG;
end;


function TAdoDbQuery.ColIsKeyPart(Column: Integer): Boolean;
begin
  Result := FCurrentResults.Fields[Column].IsIndexField;
end;


function TMySQLQuery.IsNull(Column: Integer): Boolean;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsNull
  else
    Result := FCurrentRow[Column] = nil;
end;


function TDBQuery.IsNull(Column: String): Boolean;
begin
  Result := IsNull(FColumnNames.IndexOf(Column));
end;


function TAdoDBQuery.IsNull(Column: Integer): Boolean;
begin
  Result := FCurrentResults.Fields[Column].IsNull;
end;


function TMySQLQuery.HasResult: Boolean;
begin
  Result := Length(FResultList) > 0;
end;


function TAdoDBQuery.HasResult: Boolean;
begin
  Result := Length(FResultList) > 0;
end;


procedure TDBQuery.PrepareEditing;
var
  CreateCode, Dummy, DB, Table: String;
  DBObjects: TDBObjectList;
  Obj: TDBObject;
  ObjType: TListNodeType;
begin
  // Try to fetch column names and keys
  if FEditingPrepared then
    Exit;
  // This is probably a VIEW, so column names need to be fetched differently
  DB := DatabaseName;
  if DB = '' then
    DB := Connection.Database;
  DBObjects := Connection.GetDBObjects(DB);
  Table := TableName;
  ObjType := lntTable;
  for Obj in DBObjects do begin
    if (Obj.NodeType in [lntTable, lntView]) and (Obj.Name = Table) then begin
      ObjType := Obj.NodeType;
      break;
    end;
  end;
  CreateCode := Connection.GetCreateCode(DatabaseName, TableName, ObjType);
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
  case ObjType of
    lntTable:
      Connection.ParseTableStructure(CreateCode, FColumns, FKeys, FForeignKeys);
    lntView:
      Connection.ParseViewStructure(CreateCode, TableName, FColumns, Dummy, Dummy, Dummy, Dummy);
  end;  
  FreeAndNil(FUpdateData);
  FUpdateData := TUpdateData.Create(True);
  FEditingPrepared := True;
end;


function TDBQuery.DeleteRow: Boolean;
var
  sql: String;
  IsVirtual: Boolean;
begin
  // Delete current row from result
  PrepareEditing;
  IsVirtual := Assigned(FCurrentUpdateRow) and FCurrentUpdateRow.Inserted;
  if not IsVirtual then begin
    sql := Connection.ApplyLimitClause('DELETE', 'FROM ' + QuotedDbAndTableName + ' WHERE ' + GetWhereClause, 1, 0);
    Connection.Query(sql);
    if Connection.RowsAffected = 0 then begin
      raise EDatabaseError.Create(FormatNumber(Connection.RowsAffected)+' rows deleted when that should have been 1.');
      Result := False;
    end;
  end;
  if Assigned(FCurrentUpdateRow) then begin
    FUpdateData.Remove(FCurrentUpdateRow);
    FCurrentUpdateRow := nil;
    FRecNo := -1;
  end;
  Result := True;
end;


function TDBQuery.InsertRow: Cardinal;
var
  Row, OtherRow: TRowData;
  c: TCellData;
  i: Integer;
  ColAttr: TTableColumn;
  InUse: Boolean;
begin
  // Add new row and return row number
  PrepareEditing;
  Row := TRowData.Create(True);
  for i:=0 to ColumnCount-1 do begin
    c := TCellData.Create;
    Row.Add(c);
    c.OldText := '';
    c.OldIsNull := False;
    ColAttr := ColAttributes(i);
    if Assigned(ColAttr) then begin
      c.OldIsNull := ColAttr.DefaultType in [cdtNull, cdtNullUpdateTS, cdtAutoInc];
      if ColAttr.DefaultType in [cdtText, cdtTextUpdateTS] then
        c.OldText := ColAttr.DefaultText;
    end;
    c.NewText := c.OldText;
    c.NewIsNull := c.OldIsNull;
    c.Modified := False;
  end;
  Row.Inserted := True;
  // Find highest unused recno of inserted rows and use that for this row
  Result := High(Cardinal);
  while True do begin
    InUse := False;
    for OtherRow in FUpdateData do begin
      InUse := OtherRow.RecNo = Result;
      if InUse then break;
    end;
    if not InUse then break;
    Dec(Result);
  end;
  Row.RecNo := Result;
  FUpdateData.Add(Row);
end;


procedure TDBQuery.SetCol(Column: Integer; NewText: String; Null: Boolean);
begin
  PrepareEditing;
  if not Assigned(FCurrentUpdateRow) then begin
    CreateUpdateRow;
    EnsureFullRow;
  end;
  FCurrentUpdateRow[Column].NewIsNull := Null;
  if Null then
    FCurrentUpdateRow[Column].NewText := ''
  else
    FCurrentUpdateRow[Column].NewText := NewText;
  FCurrentUpdateRow[Column].Modified := (FCurrentUpdateRow[Column].NewText <> FCurrentUpdateRow[Column].OldText) or
    (FCurrentUpdateRow[Column].NewIsNull <> FCurrentUpdateRow[Column].OldIsNull);
end;


procedure TDBQuery.CreateUpdateRow;
var
  i: Integer;
  c: TCellData;
  Row: TRowData;
begin
  Row := TRowData.Create(True);
  for i:=0 to ColumnCount-1 do begin
    c := TCellData.Create;
    Row.Add(c);
    c.OldText := Col(i);
    c.NewText := c.OldText;
    c.OldIsNull := IsNull(i);
    c.NewIsNull := c.OldIsNull;
    c.Modified := False;
  end;
  Row.Inserted := False;
  Row.RecNo := RecNo;
  FCurrentUpdateRow := Row;
  FUpdateData.Add(FCurrentUpdateRow);
end;


function TDBQuery.EnsureFullRow: Boolean;
var
  i: Integer;
  sql: String;
  Data: TDBQuery;
begin
  // Load full column values
  Result := True;
  if not HasFullData then try
    PrepareEditing;
    for i:=0 to FColumnOrgNames.Count-1 do begin
      if sql <> '' then
        sql := sql + ', ';
      sql := sql + Connection.QuoteIdent(FColumnOrgNames[i]);
    end;
    sql := sql + ' FROM '+QuotedDbAndTableName+' WHERE '+GetWhereClause;
    sql := Connection.ApplyLimitClause('SELECT', sql, 1, 0);
    Data := Connection.GetResults(sql);
    Result := Data.RecordCount = 1;
    if Result then begin
      if not Assigned(FCurrentUpdateRow) then
        CreateUpdateRow;
      for i:=0 to Data.ColumnCount-1 do begin
        FCurrentUpdateRow[i].OldText := Data.Col(i);
        FCurrentUpdateRow[i].NewText := FCurrentUpdateRow[i].OldText;
        FCurrentUpdateRow[i].OldIsNull := Data.IsNull(i);
        FCurrentUpdateRow[i].NewIsNull := FCurrentUpdateRow[i].OldIsNull;
      end;
      Data.Free;
    end;
  except on E:EDatabaseError do
    Result := False;
  end;
end;


function TDBQuery.HasFullData: Boolean;
var
  Val: String;
  i: Integer;
begin
  Result := True;
  for i:=0 to ColumnCount-1 do begin
    if not (Datatype(i).Category in [dtcText, dtcBinary]) then
      continue;
    Val := Col(i);
    if Length(Val) = GRIDMAXDATA then begin
      Result := False;
      break;
    end;
  end;
end;


function TDBQuery.SaveModifications: Boolean;
var
  i: Integer;
  Row: TRowData;
  Cell: TCellData;
  sqlUpdate, sqlInsertColumns, sqlInsertValues, Val: String;
  RowModified: Boolean;
  ColAttr: TTableColumn;
begin
  Result := True;
  if not FEditingPrepared then
    raise EDatabaseError.Create('Internal error: Cannot post modifications before editing was prepared.');

  for Row in FUpdateData do begin
    // Prepare update and insert queries
    RecNo := Row.RecNo;
    sqlUpdate := '';
    sqlInsertColumns := '';
    sqlInsertValues := '';
    RowModified := False;
    for i:=0 to ColumnCount-1 do begin
      Cell := Row[i];
      if not Cell.Modified then
        continue;
      RowModified := True;
      if sqlUpdate <> '' then begin
        sqlUpdate := sqlUpdate + ', ';
        sqlInsertColumns := sqlInsertColumns + ', ';
        sqlInsertValues := sqlInsertValues + ', ';
      end;
      if Cell.NewIsNull then
        Val := 'NULL'
      else case Datatype(i).Category of
        dtcInteger, dtcReal: begin
          Val := Cell.NewText;
          if Datatype(i).Index = dtBit then
            Val := 'b' + Connection.EscapeString(Val);
        end;
        dtcBinary, dtcSpatial:
          Val := HexValue(Cell.NewText);
        else
          Val := Connection.EscapeString(Cell.NewText);
      end;
      sqlUpdate := sqlUpdate + Connection.QuoteIdent(FColumnOrgNames[i]) + '=' + Val;
      sqlInsertColumns := sqlInsertColumns + Connection.QuoteIdent(FColumnOrgNames[i]);
      sqlInsertValues := sqlInsertValues + Val;
    end;

    // Post query and fetch just inserted auto-increment id if applicable
    if RowModified then try
      if Row.Inserted then begin
        Connection.Query('INSERT INTO '+QuotedDbAndTableName+' ('+sqlInsertColumns+') VALUES ('+sqlInsertValues+')');
        for i:=0 to ColumnCount-1 do begin
          ColAttr := ColAttributes(i);
          if Assigned(ColAttr) and (ColAttr.DefaultType = cdtAutoInc) then begin
            Row[i].NewText := UnformatNumber(Row[i].NewText);
            if Row[i].NewText = '0' then
              Row[i].NewText := Connection.GetVar('SELECT LAST_INSERT_ID()');
            Row[i].NewIsNull := False;
            break;
          end;
        end;
      end else begin
        sqlUpdate := QuotedDbAndTableName+' SET '+sqlUpdate+' WHERE '+GetWhereClause;
        sqlUpdate := Connection.ApplyLimitClause('UPDATE', sqlUpdate, 1, 0);
        Connection.Query(sqlUpdate);
        if Connection.RowsAffected = 0 then begin
          raise EDatabaseError.Create(FormatNumber(Connection.RowsAffected)+' rows updated when that should have been 1.');
          Result := False;
        end;
      end;
      // Reset modification flags
      for i:=0 to ColumnCount-1 do begin
        Cell := Row[i];
        Cell.OldText := Cell.NewText;
        Cell.OldIsNull := Cell.NewIsNull;
        Cell.Modified := False;
      end;
      Row.Inserted := False;
      // TODO: Reload real row data from server if keys allow that???
    except
      on E:EDatabaseError do begin
        Result := False;
        ErrorDialog(E.Message);
      end;
    end;

  end;
end;


procedure TDBQuery.DiscardModifications;
var
  x: Integer;
  c: TCellData;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
    if FCurrentUpdateRow.Inserted then begin
      FUpdateData.Remove(FCurrentUpdateRow);
      FRecNo := -1;
    end else for x:=0 to FCurrentUpdateRow.Count-1 do begin
      c := FCurrentUpdateRow[x];
      c.NewText := c.OldText;
      c.NewIsNull := c.OldIsNull;
      c.Modified := False;
    end;
  end;
end;


function TDBQuery.Modified(Column: Integer): Boolean;
begin
  Result := False;
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then try
    Result := FCurrentUpdateRow[Column].Modified;
  except
    connection.Log(lcdebug, inttostr(column));
    raise;
  end;
end;


function TDBQuery.Modified: Boolean;
var
  x, y: Integer;
begin
  Result := False;
  if FEditingPrepared then for y:=0 to FUpdateData.Count-1 do begin
    for x:=0 to FUpdateData[y].Count-1 do begin
      Result := FUpdateData[y][x].Modified;
      if Result then
        break;
    end;
    if Result then
      break;
  end;
end;


function TDBQuery.Inserted: Boolean;
begin
  // Check if current row was inserted and not yet posted to the server
  Result := False;
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow.Inserted;
end;


function TMySQLQuery.DatabaseName: String;
var
  Field: PMYSQL_FIELD;
  i: Integer;
begin
  // Return first available Field.db property, or just the current database as fallback
  for i:=0 to ColumnCount-1 do begin
    Field := mysql_fetch_field_direct(FCurrentResults, i);
    if Field.db <> '' then begin
      Result := Connection.DecodeAPIString(Field.db);
      break;
    end;
  end;
  if Result = '' then
    Result := Connection.Database;
end;


function TAdoDBQuery.DatabaseName: String;
begin
  Result := Connection.Database;
end;


function TMySQLQuery.TableName: String;
var
  Field: PMYSQL_FIELD;
  i: Integer;
  tbl, db: AnsiString;
  Objects: TDBObjectList;
  Obj: TDBObject;
  IsView: Boolean;
begin
  IsView := False;
  for i:=0 to ColumnCount-1 do begin
    Field := mysql_fetch_field_direct(FCurrentResults, i);

    if Connection.DecodeAPIString(Field.table) <> Connection.DecodeAPIString(Field.org_table) then begin
      // Probably a VIEW, in which case we rely on the first column's table name.
      // TODO: This is unsafe when joining a view with a table/view.
      if Field.db <> '' then begin
        Objects := Connection.GetDBObjects(Connection.DecodeAPIString(Field.db));
        for Obj in Objects do begin
          if (Obj.Name = Connection.DecodeAPIString(Field.table)) and (Obj.NodeType = lntView) then begin
            tbl := Field.table;
            IsView := True;
            break;
          end;
        end;
      end;
      if IsView and (tbl <> '') then
        break;
    end;

    if (Field.org_table <> '') and (tbl <> '') and ((tbl <> Field.org_table) or (db <> Field.db)) then
      raise EDatabaseError.Create('More than one table involved.');
    if Field.org_table <> '' then begin
      tbl := Field.org_table;
      db := Field.db;
    end;
  end;
  if tbl = '' then
    raise EDatabaseError.Create('Could not determine name of table.')
  else
    Result := Connection.DecodeAPIString(tbl)
end;


function TAdoDBQuery.TableName: String;
var
  rx: TRegExpr;
begin
  // Untested with joins, compute columns and views
  Result := GetTableNameFromSQLEx(SQL, idMixCase);
  rx := TRegExpr.Create;
  rx.Expression := '\.([^\.]+)$';
  if rx.Exec(Result) then
    Result := rx.Match[1];
  rx.Free;
  if Result = '' then
    raise EDatabaseError.Create('Could not determine name of table.');
end;


function TDBQuery.QuotedDbAndTableName: String;
var
  db: String;
begin
  // Return `db`.`table` if necessairy, otherwise `table`
  db := DatabaseName;
  if Connection.Database <> db then
    Result := Connection.QuoteIdent(db)+'.';
  Result := Result + Connection.QuoteIdent(TableName);
end;


function TDBQuery.GetKeyColumns: TStringList;
var
  NeededCols: TStringList;
  i: Integer;
begin
  // Return key column names, or all column names if no good key present
  PrepareEditing;
  NeededCols := Connection.GetKeyColumns(FColumns, FKeys);
  if NeededCols.Count = 0 then begin
    // No good key found. Just expect all columns to be present.
    for i:=0 to FColumns.Count-1 do
      NeededCols.Add(FColumns[i].Name);
  end;

  Result := TStringList.Create;
  for i:=0 to NeededCols.Count-1 do begin
    if FColumnOrgNames.IndexOf(NeededCols[i]) > -1 then
      Result.Add(NeededCols[i]);
  end;
end;


procedure TDBQuery.CheckEditable;
var
  i: Integer;
begin
  if GetKeyColumns.Count = 0 then
    raise EDatabaseError.Create(MSG_NOGRIDEDITING);
  // All column names must be present in order to send valid INSERT/UPDATE/DELETE queries
  for i:=0 to FColumnOrgNames.Count-1 do begin
    if FColumnOrgNames[i] = '' then
      raise EDatabaseError.Create('Column #'+IntToStr(i)+' has an undefined origin: '+ColumnNames[i]);
  end;
end;


function TDBQuery.GetWhereClause: String;
var
  i, j: Integer;
  NeededCols: TStringList;
  ColVal: String;
  ColIsNull: Boolean;
begin
  // Compose WHERE clause including values from best key for editing
  NeededCols := GetKeyColumns;

  for i:=0 to NeededCols.Count-1 do begin
    j := FColumnOrgNames.IndexOf(NeededCols[i]);
    if j = -1 then
      raise EDatabaseError.Create('Cannot compose WHERE clause - column missing: '+NeededCols[i]);
    if Result <> '' then
      Result := Result + ' AND';
    Result := Result + ' ' + Connection.QuoteIdent(FColumnOrgNames[j]);
    if Modified(j) then begin
      ColVal := FCurrentUpdateRow[j].OldText;
      ColIsNull := FCurrentUpdateRow[j].OldIsNull;
    end else begin
      ColVal := Col(j);
      ColIsNull := IsNull(j);
    end;

    if ColIsNull then
      Result := Result + ' IS NULL'
    else begin
      case DataType(j).Category of
        dtcInteger, dtcReal: begin
          if DataType(j).Index = dtBit then
            Result := Result + '=b' + Connection.EscapeString(ColVal)
          else
            Result := Result + '=' + UnformatNumber(ColVal);
        end;
        dtcBinary:
          Result := Result + '=' + HexValue(ColVal);
        else
          Result := Result + '=' + Connection.EscapeString(ColVal);
      end;
    end;
  end;
end;



{ TCellData }

destructor TCellData.Destroy;
begin
  NewText := '';
  OldText := '';
end;



{ TDBObjectComparer }

function TDBObjectComparer.Compare(const Left, Right: TDBObject): Integer;
begin
  // Simple sort method for a TDBObjectList
  Result := CompareAnyNode(Left.Name, Right.Name);
end;


function TDBObjectDropComparer.Compare(const Left, Right: TDBObject): Integer;
begin
  // Sorting a TDBObject items so that dropping them does not trap in SQL errors
  if (Left.NodeType = lntTrigger) and (Right.NodeType <> lntTrigger) then
    Result := -1
  else if (Left.NodeType <> lntTrigger) and (Right.NodeType = lntTrigger) then
    Result := 1
  else if (Left.NodeType = lntView) and (Right.NodeType <> lntView) then
    Result := -1
  else if (Left.NodeType <> lntView) and (Right.NodeType = lntView) then
    Result := 1
  else
    Result := 0;
end;



{ TDBObject }

constructor TDBObject.Create(OwnerConnection: TDBConnection);
begin
  NodeType := lntNone;
  Name := '';
  Database := '';
  Rows := -1;
  Size := -1;
  Created := 0;
  Updated := 0;
  Engine := '';
  Comment := '';
  Version := -1;
  AutoInc := -1;
  RowFormat := '';
  AvgRowLen := -1;
  MaxDataLen := -1;
  IndexLen := -1;
  DataLen := -1;
  DataFree := -1;
  LastChecked := 0;
  Collation := '';
  CheckSum := -1;
  CreateOptions := '';
  FCreateCode := '';
  FCreateCodeFetched := False;
  FConnection := OwnerConnection;
end;


procedure TDBObject.Assign(Source: TPersistent);
var
  s: TDBObject;
begin
  if Source is TDBObject then begin
    s := Source as TDBObject;
    Name := s.Name;
    Column := s.Column;
    Collation := s.Collation;
    Engine := s.Engine;
    Database := s.Database;
    NodeType := s.NodeType;
    Created := s.Created;
    Updated := s.Updated;
    Comment := s.Comment;
    Rows := s.Rows;
    Size := s.Size;
    FCreateCode := s.FCreateCode;
    FCreateCodeFetched := s.FCreateCodeFetched;
    FViewSelectCode := s.FViewSelectCode;
  end else
    inherited;
end;


function TDBObject.IsSameAs(CompareTo: TDBObject): Boolean;
begin
  if not Assigned(CompareTo) then
    Result := False
  else
    Result := (Name = CompareTo.Name)
      and (NodeType = CompareTo.NodeType)
      and (Database = CompareTo.Database)
      and (Column = CompareTo.Column)
      and (Connection = CompareTo.Connection);
end;


function TDBObject.GetObjType: String;
begin
  case NodeType of
    lntTable: Result := 'Table';
    lntView: Result := 'View';
    lntFunction: Result := 'Function';
    lntProcedure: Result := 'Procedure';
    lntTrigger: Result := 'Trigger';
    lntEvent: Result := 'Event';
    lntColumn: Result := 'Column';
    else Result := 'Unknown, should never appear';
  end;
end;

function TDBObject.GetImageIndex: Integer;
begin
  // Detect key icon index for specified db object (table, trigger, ...)
  case NodeType of
    lntNone: Result := FConnection.Parameters.ImageIndex;

    lntDb: Result := ICONINDEX_DB;

    lntTable: Result := ICONINDEX_TABLE;
    lntFunction: Result := ICONINDEX_STOREDFUNCTION;
    lntProcedure: Result := ICONINDEX_STOREDPROCEDURE;
    lntView: Result := ICONINDEX_VIEW;
    lntTrigger: Result := ICONINDEX_TRIGGER;
    lntEvent: Result := ICONINDEX_EVENT;

    lntColumn: Result := ICONINDEX_FIELD;

    else Result := -1;
  end;
end;


function TDBObject.GetCreateCode: String;
var
  rx: TRegExpr;
begin
  if not FCreateCodeFetched then try
    FCreateCode := Connection.GetCreateCode(Database, Name, NodeType);
    if NodeType = lntView then begin
      FViewSelectCode := Connection.GetVar('SELECT LOAD_FILE(CONCAT(IFNULL(@@GLOBAL.datadir, CONCAT(@@GLOBAL.basedir, '+Connection.EscapeString('data/')+')), '+Connection.EscapeString(Database+'/'+Name+'.frm')+'))');
      rx := TRegExpr.Create;
      rx.ModifierI := True;
      rx.ModifierG := False;
      rx.Expression := '\nsource\=(.+)\n\w+\=';
      if rx.Exec(FViewSelectCode) then
        FViewSelectCode := Connection.UnescapeString(rx.Match[1])
      else
        FViewSelectCode := '';
    end;
  except
  end;
  FCreateCodeFetched := True;
  Result := FCreateCode;
end;

procedure TDBObject.SetCreateCode(Value: String);
begin
  // When manually clearing CreateCode from outside, also reset indicator for fetch attempt
  FCreateCode := Value;
  FCreateCodeFetched := Value <> '';
end;

function TDBObject.QuotedDatabase(AlwaysQuote: Boolean=True): String;
begin
  Result := Connection.QuoteIdent(Database, AlwaysQuote);
end;

function TDBObject.QuotedName(AlwaysQuote: Boolean=True): String;
begin
  Result := Connection.QuoteIdent(Name, AlwaysQuote);
end;

function TDBObject.QuotedColumn(AlwaysQuote: Boolean=True): String;
begin
  Result := Connection.QuoteIdent(Column, AlwaysQuote);
end;


{ *** TTableColumn }

constructor TTableColumn.Create(AOwner: TDBConnection);
begin
  inherited Create;
  FConnection := AOwner;
end;

destructor TTableColumn.Destroy;
begin
  inherited Destroy;
end;

procedure TTableColumn.SetStatus(Value: TEditingStatus);
begin
  // Set editing flag and enable "Save" button
  if (FStatus in [esAddedUntouched, esAddedModified]) and (Value = esModified) then
    Value := esAddedModified
  else if (FStatus in [esAddedUntouched, esAddedModified]) and (Value = esDeleted) then
    Value := esAddedDeleted;
  FStatus := Value;
end;

function TTableColumn.SQLCode: String;
var
  IsVirtual: Boolean;
begin
  Result := FConnection.QuoteIdent(Name) + ' ' +DataType.Name;
  IsVirtual := (Expression <> '') and (Virtuality <> '');
  if LengthSet <> '' then
    Result := Result + '(' + LengthSet + ')';
  if (DataType.Category in [dtcInteger, dtcReal]) and Unsigned then
    Result := Result + ' UNSIGNED';
  if (DataType.Category in [dtcInteger, dtcReal]) and ZeroFill then
    Result := Result + ' ZEROFILL';
  if not IsVirtual then begin
    if not AllowNull then
      Result := Result + ' NOT';
    Result := Result + ' NULL';
  end;
  if DefaultType <> cdtNothing then begin
    Result := Result + ' ' + GetColumnDefaultClause(DefaultType, DefaultText);
    Result := TrimRight(Result); // Remove whitespace for columns without default value
  end;
  if IsVirtual then
    Result := Result + ' AS ('+Expression+') '+Virtuality;
  if Comment <> '' then
    Result := Result + ' COMMENT '+esc(Comment);
  if Collation <> '' then
    Result := Result + ' COLLATE '+esc(Collation);
end;



{ *** TTableKey }

constructor TTableKey.Create(AOwner: TDBConnection);
begin
  inherited Create;
  FConnection := AOwner;
  Columns := TStringList.Create;
  SubParts := TStringList.Create;
  Columns.OnChange := Modification;
  Subparts.OnChange := Modification;
end;

destructor TTableKey.Destroy;
begin
  FreeAndNil(Columns);
  FreeAndNil(SubParts);
  inherited Destroy;
end;

procedure TTableKey.Modification(Sender: TObject);
begin
  if not Added then
    Modified := True;
end;

function TTableKey.SQLCode: String;
var
  i: Integer;
begin
  Result := '';
  // Supress SQL error  trying index creation with 0 column
  if Columns.Count = 0 then
    Exit;
  if IndexType = PKEY then
    Result := Result + 'PRIMARY KEY '
  else begin
    if IndexType <> KEY then
      Result := Result + IndexType + ' ';
    Result := Result + 'INDEX ' + FConnection.QuoteIdent(Name) + ' ';
  end;
  Result := Result + '(';
  for i:=0 to Columns.Count-1 do begin
    Result := Result + FConnection.QuoteIdent(Columns[i]);
    if SubParts[i] <> '' then
      Result := Result + '(' + SubParts[i] + ')';
    Result := Result + ', ';
  end;
  if Columns.Count > 0 then
    Delete(Result, Length(Result)-1, 2);

  Result := Result + ')';

  if Algorithm <> '' then
    Result := Result + ' USING ' + Algorithm;
end;




{ *** TForeignKey }

constructor TForeignKey.Create(AOwner: TDBConnection);
begin
  inherited Create;
  FConnection := AOwner;
  Columns := TStringList.Create;
  ForeignColumns := TStringList.Create;
end;

destructor TForeignKey.Destroy;
begin
  FreeAndNil(Columns);
  FreeAndNil(ForeignColumns);
  inherited Destroy;
end;

function TForeignKey.SQLCode(IncludeSymbolName: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  // Symbol names are unique in a db. In order to autocreate a valid name we leave the constraint clause away.
  if IncludeSymbolName then
    Result := 'CONSTRAINT '+FConnection.QuoteIdent(KeyName)+' ';
  Result := Result + 'FOREIGN KEY (';
  for i:=0 to Columns.Count-1 do
    Result := Result + FConnection.QuoteIdent(Columns[i]) + ', ';
  if Columns.Count > 0 then Delete(Result, Length(Result)-1, 2);
  Result := Result + ') REFERENCES ' + FConnection.QuoteIdent(ReferenceTable, True, '.') + ' (';
  for i:=0 to ForeignColumns.Count-1 do
    Result := Result + FConnection.QuoteIdent(ForeignColumns[i]) + ', ';
  if ForeignColumns.Count > 0 then Delete(Result, Length(Result)-1, 2);
  Result := Result + ')';
  if OnUpdate <> '' then
    Result := Result + ' ON UPDATE ' + OnUpdate;
  if OnDelete <> '' then
    Result := Result + ' ON DELETE ' + OnDelete;
end;




function mysql_authentication_dialog_ask;
var
  Username, Password: String;
begin
  {
  From client_plugin.h:
    The C function with the name "mysql_authentication_dialog_ask", if exists,
    will be used by the "dialog" client authentication plugin when user
    input is needed. This function should be of mysql_authentication_dialog_ask_t
    type. If the function does not exists, a built-in implementation will be
    used.
    @param mysql          mysql
    @param type           type of the input
                          1 - normal string input
                          2 - password string
    @param prompt         prompt
    @param buf            a buffer to store the use input
    @param buf_len        the length of the buffer
    @retval               a pointer to the user input string.
                          It may be equal to 'buf' or to 'mysql->password'.
                          In all other cases it is assumed to be an allocated
                          string, and the "dialog" plugin will free() it.
  Test suite:
    INSTALL PLUGIN three_attempts SONAME 'dialog.dll';
    CREATE USER test_dialog IDENTIFIED VIA three_attempts USING 'SECRET';
  }
  Username := '';
  Password := '';
  case _type of
    1: Username := String(buf);
    2: Password := String(buf);
    else raise EDatabaseError.Create('Unsupported type ('+IntToStr(_type)+') in mysql_authentication_dialog_ask.');
  end;
  LoginPrompt(String(prompt), Username, Password, _type=1, _type=2);
  Result := buf;
  case _type of
    1: Result := PAnsiChar(AnsiString(Username));
    2: Result := PAnsiChar(AnsiString(Password));
  end;
end;


initialization


finalization

// Release libmysql.dll handle
if LibMysqlHandle <> 0 then begin
  FreeLibrary(LibMysqlHandle);
  LibMysqlHandle := 0;
end;


end.
