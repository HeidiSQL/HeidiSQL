unit dbconnection;

interface

uses
  Classes, SysUtils, windows, dbstructures, SynRegExpr, Generics.Collections, Generics.Defaults,
  DateUtils, Types, Math, Dialogs, ADODB, DB, DBCommon, ComObj, Graphics, ExtCtrls, StrUtils,
  gnugettext, AnsiStrings, Controls, Forms, System.IOUtils;


type
  {$M+} // Needed to add published properties

  { TDBObjectList and friends }

  TListNodeType = (lntNone, lntDb, lntGroup, lntTable, lntView, lntFunction, lntProcedure, lntTrigger, lntEvent, lntColumn);
  TListNodeTypes = Set of TListNodeType;
  TDBConnection = class;
  TConnectionParameters = class;
  TDBQuery = class;
  TDBQueryList = TObjectList<TDBQuery>;
  TDBObject = class;

  TColumnPart = (cpAll, cpName, cpType, cpAllowNull, cpDefault, cpVirtuality, cpComment, cpCollation);
  TColumnParts = Set of TColumnPart;
  TColumnDefaultType = (cdtNothing, cdtText, cdtNull, cdtAutoInc, cdtExpression);
  // General purpose editing status flag
  TEditingStatus = (esUntouched, esModified, esDeleted, esAddedUntouched, esAddedModified, esAddedDeleted);

  // Column object, many of them in a TObjectList
  TTableColumn = class(TPersistent)
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
      OnUpdateType: TColumnDefaultType;
      OnUpdateText: String;
      Comment, Charset, Collation, Expression, Virtuality: String;
      FStatus: TEditingStatus;
      constructor Create(AOwner: TDBConnection; Serialized: String='');
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      function Serialize: String;
      function SQLCode(OverrideCollation: String=''; Parts: TColumnParts=[cpAll]): String;
      function ValueList: TStringList;
      procedure ParseDatatype(Source: String);
      function CastAsText: String;
      property Status: TEditingStatus read FStatus write SetStatus;
      property Connection: TDBConnection read FConnection;
  end;
  PTableColumn = ^TTableColumn;
  TTableColumnList = class(TObjectList<TTableColumn>)
    public
      Loaded: Boolean;
      procedure Assign(Source: TTableColumnList);
  end;

  TTableKey = class(TPersistent)
    const
      PRIMARY = 'PRIMARY';
      KEY = 'KEY';
      UNIQUE = 'UNIQUE';
      FULLTEXT = 'FULLTEXT';
      SPATIAL = 'SPATIAL';
    private
      FConnection: TDBConnection;
      function GetImageIndex: Integer;
    public
      Name, OldName: String;
      IndexType, OldIndexType, Algorithm, Comment: String;
      Columns, SubParts: TStringList;
      Modified, Added: Boolean;
      constructor Create(AOwner: TDBConnection);
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure Modification(Sender: TObject);
      function SQLCode: String;
      property ImageIndex: Integer read GetImageIndex;
  end;
  TTableKeyList = class(TObjectList<TTableKey>)
    public
      Loaded: Boolean;
      procedure Assign(Source: TTableKeyList);
  end;

  // Helper object to manage foreign keys in a TObjectList
  TForeignKey = class(TPersistent)
    private
      FConnection: TDBConnection;
    public
      KeyName, OldKeyName, ReferenceTable, OnUpdate, OnDelete: String;
      Columns, ForeignColumns: TStringList;
      Modified, Added, KeyNameWasCustomized: Boolean;
      constructor Create(AOwner: TDBConnection);
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      function SQLCode(IncludeSymbolName: Boolean): String;
      function ReferenceTableObj: TDBObject;
  end;
  TForeignKeyList = class(TObjectList<TForeignKey>)
    public
      Loaded: Boolean;
      procedure Assign(Source: TForeignKeyList);
  end;

  TRoutineParam = class(TObject)
    public
      Name, Context, Datatype: String;
  end;
  TRoutineParamList = TObjectList<TRoutineParam>;

  TDBObject = class(TPersistent)
    private
      FCreateCode: String;
      FCreateCodeLoaded: Boolean;
      FWasSelected: Boolean;
      FConnection: TDBConnection;
      FTableColumns: TTableColumnList;
      FTableKeys: TTableKeyList;
      FTableForeignKeys: TForeignKeyList;
      function GetObjType: String;
      function GetImageIndex: Integer;
      function GetOverlayImageIndex: Integer;
      function GetPath: String;
      function GetTableColumns: TTableColumnList;
      function GetTableKeys: TTableKeyList;
      function GetTableForeignKeys: TForeignKeyList;
    public
      // Table options:
      Name, Schema, Database, Column, Engine, Comment, RowFormat, CreateOptions, Collation: String;
      Created, Updated, LastChecked: TDateTime;
      Rows, Size, Version, AvgRowLen, MaxDataLen, IndexLen, DataLen, DataFree, AutoInc, CheckSum: Int64;
      // Routine options:
      Body, Definer, Returns, DataAccess, Security, ArgTypes: String;
      Deterministic: Boolean;

      NodeType, GroupType: TListNodeType;
      constructor Create(OwnerConnection: TDBConnection);
      procedure Assign(Source: TPersistent); override;
      procedure UnloadDetails;
      procedure Drop;
      function IsSameAs(CompareTo: TDBObject): Boolean;
      function QuotedDatabase(AlwaysQuote: Boolean=True): String;
      function QuotedName(AlwaysQuote: Boolean=True; SeparateSegments: Boolean=True): String;
      function QuotedDbAndTableName(AlwaysQuote: Boolean=True): String;
      function QuotedColumn(AlwaysQuote: Boolean=True): String;
      function RowCount(Reload: Boolean): Int64;
      function GetCreateCode: String; overload;
      function GetCreateCode(RemoveAutoInc, RemoveDefiner: Boolean): String; overload;
      property ObjType: String read GetObjType;
      property ImageIndex: Integer read GetImageIndex;
      property OverlayImageIndex: Integer read GetOverlayImageIndex;
      property Path: String read GetPath;
      property CreateCode: String read GetCreateCode;
      property WasSelected: Boolean read FWasSelected write FWasSelected;
      property Connection: TDBConnection read FConnection;
      property TableColumns: TTableColumnList read GetTableColumns;
      property TableKeys: TTableKeyList read GetTableKeys;
      property TableForeignKeys: TForeignKeyList read GetTableForeignKeys;
  end;
  PDBObject = ^TDBObject;
  TDBObjectList = class(TObjectList<TDBObject>)
    private
      FDatabase: String;
      FDataSize: Int64;
      FLargestObjectSize: Int64;
      FLastUpdate: TDateTime;
      FCollation: String;
      FOnlyNodeType: TListNodeType;
    public
      property Database: String read FDatabase;
      property DataSize: Int64 read FDataSize;
      property LargestObjectSize: Int64 read FLargestObjectSize;
      property LastUpdate: TDateTime read FLastUpdate;
      property Collation: String read FCollation;
      property OnlyNodeType: TListNodeType read FOnlyNodeType;
  end;
  TDatabaseCache = class(TObjectList<TDBObjectList>); // A list of db object lists, used for caching
  TDBObjectComparer = class(TComparer<TDBObject>)
    function Compare(const Left, Right: TDBObject): Integer; override;
  end;
  TDBObjectDropComparer = class(TComparer<TDBObject>)
    function Compare(const Left, Right: TDBObject): Integer; override;
  end;

  TOidStringPairs = TDictionary<POid, String>;

  // Structures for in-memory changes of a TDBQuery
  TGridValue = class(TObject)
    public
      NewText, OldText: String;
      NewIsNull, OldIsNull: Boolean;
      NewIsFunction, OldIsFunction: Boolean;
      Modified: Boolean;
      destructor Destroy; override;
  end;
  TGridRow = class(TObjectList<TGridValue>)
    public
      RecNo: Int64;
      Inserted: Boolean;
  end;
  TGridRows = class(TObjectList<TGridRow>);

  // PLink.exe related
  TProcessPipe = class(TObject)
    public
      ReadHandle: THandle;
      WriteHandle: THandle;
      constructor Create;
      destructor Destroy; override;
  end;
  TPlink = class(TObject)
    private
      FProcessInfo: TProcessInformation;
      FInPipe: TProcessPipe;
      FOutPipe: TProcessPipe;
      FErrorPipe: TProcessPipe;
      FConnection: TDBConnection;
      function ReadPipe(const Pipe: TProcessPipe): String;
      function AsciiToAnsi(Text: AnsiString): AnsiString;
      function CleanEscSeq(const Buffer: String): String;
      procedure SendText(Text: String);
     public
      procedure Connect;
      constructor Create(Connection: TDBConnection);
      destructor Destroy; override;
  end;


  { TConnectionParameters and friends }

  TNetType = (ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel,
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP, ntMSSQL_SPX, ntMSSQL_VINES, ntMSSQL_RPC,
    ntPgSQL_TCPIP, ntPgSQL_SSHtunnel,
    ntSQLite);
  TNetTypeGroup = (ngMySQL, ngMSSQL, ngPgSQL, ngSQLite);
  TNetGroupLibs = TDictionary<TNetTypeGroup, TStringList>;

  TConnectionParameters = class(TObject)
    strict private
      FNetType: TNetType;
      FHostname, FUsername, FPassword, FAllDatabases, FLibraryOrProvider, FComment, FStartupScriptFilename,
      FSessionPath, FSSLPrivateKey, FSSLCertificate, FSSLCACertificate, FSSLCipher, FServerVersion,
      FSSHHost, FSSHUser, FSSHPassword, FSSHPlinkExe, FSSHPrivateKey: String;
      FPort, FSSHPort, FSSHLocalPort, FSSHTimeout, FCounter, FQueryTimeout, FKeepAlive: Integer;
      FLoginPrompt, FCompressed, FLocalTimeZone, FFullTableStatus,
      FWindowsAuth, FWantSSL, FIsFolder, FCleartextPluginEnabled: Boolean;
      FSessionColor: TColor;
      FLastConnect: TDateTime;
      class var FLibraries: TNetGroupLibs;
      function GetImageIndex: Integer;
      function GetSessionName: String;
    public
      constructor Create; overload;
      constructor Create(SessionRegPath: String); overload;
      procedure SaveToRegistry;
      function CreateConnection(AOwner: TComponent): TDBConnection;
      function CreateQuery(Connection: TDbConnection): TDBQuery;
      function NetTypeName(LongFormat: Boolean): String;
      function IsCompatibleToWin10S: Boolean;
      function GetNetTypeGroup: TNetTypeGroup;
      function IsAnyMySQL: Boolean;
      function IsAnyMSSQL: Boolean;
      function IsAnyPostgreSQL: Boolean;
      function IsAnySQLite: Boolean;
      function IsMariaDB: Boolean;
      function IsMySQL: Boolean;
      function IsPercona: Boolean;
      function IsTokudb: Boolean;
      function IsInfiniDB: Boolean;
      function IsInfobright: Boolean;
      function IsAzure: Boolean;
      function IsMemSQL: Boolean;
      function IsRedshift: Boolean;
      property ImageIndex: Integer read GetImageIndex;
      function GetLibraries: TStringList;
      function DefaultLibrary: String;
      function DefaultPort: Integer;
      function DefaultUsername: String;
    published
      property IsFolder: Boolean read FIsFolder write FIsFolder;
      property NetType: TNetType read FNetType write FNetType;
      property NetTypeGroup: TNetTypeGroup read GetNetTypeGroup;
      property ServerVersion: String read FServerVersion write FServerVersion;
      property Counter: Integer read FCounter;
      property LastConnect: TDateTime read FLastConnect;
      property SessionPath: String read FSessionPath write FSessionPath;
      property SessionName: String read GetSessionName;
      property SessionColor: TColor read FSessionColor write FSessionColor;
      property Hostname: String read FHostname write FHostname;
      property Port: Integer read FPort write FPort;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
      property WindowsAuth: Boolean read FWindowsAuth write FWindowsAuth;
      property CleartextPluginEnabled: Boolean read FCleartextPluginEnabled write FCleartextPluginEnabled;
      property AllDatabasesStr: String read FAllDatabases write FAllDatabases;
      property LibraryOrProvider: String read FLibraryOrProvider write FLibraryOrProvider;
      property Comment: String read FComment write FComment;
      property StartupScriptFilename: String read FStartupScriptFilename write FStartupScriptFilename;
      property QueryTimeout: Integer read FQueryTimeout write FQueryTimeout;
      property KeepAlive: Integer read FKeepAlive write FKeepAlive;
      property Compressed: Boolean read FCompressed write FCompressed;
      property LocalTimeZone: Boolean read FLocalTimeZone write FLocalTimeZone;
      property FullTableStatus: Boolean read FFullTableStatus write FFullTableStatus;
      property SSHHost: String read FSSHHost write FSSHHost;
      property SSHPort: Integer read FSSHPort write FSSHPort;
      property SSHUser: String read FSSHUser write FSSHUser;
      property SSHPassword: String read FSSHPassword write FSSHPassword;
      property SSHTimeout: Integer read FSSHTimeout write FSSHTimeout;
      property SSHPrivateKey: String read FSSHPrivateKey write FSSHPrivateKey;
      property SSHLocalPort: Integer read FSSHLocalPort write FSSHLocalPort;
      property SSHPlinkExe: String read FSSHPlinkExe write FSSHPlinkExe;
      property WantSSL: Boolean read FWantSSL write FWantSSL;
      property SSLPrivateKey: String read FSSLPrivateKey write FSSLPrivateKey;
      property SSLCertificate: String read FSSLCertificate write FSSLCertificate;
      property SSLCACertificate: String read FSSLCACertificate write FSSLCACertificate;
      property SSLCipher: String read FSSLCipher write FSSLCipher;
  end;
  PConnectionParameters = ^TConnectionParameters;


  { TDBConnection }

  TDBLogCategory = (lcInfo, lcSQL, lcUserFiredSQL, lcError, lcDebug, lcScript);
  TDBLogEvent = procedure(Msg: String; Category: TDBLogCategory=lcInfo; Connection: TDBConnection=nil) of object;
  TDBEvent = procedure(Connection: TDBConnection; Database: String) of object;
  TDBDataTypeArray = Array of TDBDataType;
  TSQLSpecifityId = (spDatabaseTable, spDatabaseTableId,
    spDbObjectsTable, spDbObjectsCreateCol, spDbObjectsUpdateCol, spDbObjectsTypeCol,
    spEmptyTable, spRenameTable, spRenameView, spCurrentUserHost,
    spAddColumn, spChangeColumn,
    spSessionVariables, spGlobalVariables,
    spISTableSchemaCol,
    spUSEQuery, spKillQuery, spKillProcess,
    spFuncLength, spFuncCeil, spFuncLeft,
    spLockedTables);

  TDBConnection = class(TComponent)
    private
      FActive: Boolean;
      FConnectionStarted: Cardinal;
      FServerUptime: Integer;
      FServerDateTimeOnStartup: String;
      FParameters: TConnectionParameters;
      FPlink: TPlink;
      FLoginPromptDone: Boolean;
      FDatabase: String;
      FAllDatabases: TStringList;
      FLogPrefix: String;
      FOnLog: TDBLogEvent;
      FOnConnected: TDBEvent;
      FOnDatabaseChanged: TDBEvent;
      FOnObjectnamesChanged: TDBEvent;
      FRowsFound: Int64;
      FRowsAffected: Int64;
      FWarningCount: Cardinal;
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
      FSessionVariables: TDBQuery;
      FInformationSchemaObjects: TStringList;
      FDatabaseCache: TDatabaseCache;
      FCurrentUserHostCombination: String;
      FAllUserHostCombinations: TStringList;
      FLockedByThread: TThread;
      FQuoteChar: Char;
      FQuoteChars: String;
      FDatatypes: TDBDataTypeArray;
      FThreadID: Int64;
      FSQLSpecifities: Array[TSQLSpecifityId] of String;
      FKeepAliveTimer: TTimer;
      FFavorites: TStringList;
      FPrefetchResults: TDBQueryList;
      FForeignKeyQueriesFailed: Boolean;
      FInfSch: String;
      procedure SetActive(Value: Boolean); virtual; abstract;
      procedure DoBeforeConnect; virtual;
      procedure DoAfterConnect; virtual;
      procedure DetectUSEQuery(SQL: String); virtual;
      procedure SetDatabase(Value: String);
      function GetThreadId: Int64; virtual; abstract;
      function GetCharacterSet: String; virtual;
      procedure SetCharacterSet(CharsetName: String); virtual;
      function GetLastErrorCode: Cardinal; virtual; abstract;
      function GetLastErrorMsg: String; virtual; abstract;
      function GetAllDatabases: TStringList; virtual;
      function GetTableEngines: TStringList; virtual;
      function GetCollationTable: TDBQuery; virtual;
      function GetCollationList: TStringList;
      function GetCharsetTable: TDBQuery; virtual;
      function GetCharsetList: TStringList;
      function GetInformationSchemaObjects: TStringList; virtual;
      function GetConnectionUptime: Integer;
      function GetServerUptime: Integer;
      function GetServerNow: TDateTime;
      function GetCurrentUserHostCombination: String;
      function GetAllUserHostCombinations: TStringList;
      function DecodeAPIString(a: AnsiString): String;
      function GetRowCount(Obj: TDBObject): Int64; virtual; abstract;
      procedure ClearCache(IncludeDBObjects: Boolean);
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); virtual; abstract;
      procedure SetLockedByThread(Value: TThread); virtual;
      procedure KeepAliveTimerEvent(Sender: TObject);
      procedure Drop(Obj: TDBObject); virtual;
      procedure PrefetchResults(SQL: String);
      procedure FreeResults(Results: TDBQuery);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); virtual; abstract;
      procedure Log(Category: TDBLogCategory; Msg: String);
      function EscapeString(Text: String; ProcessJokerChars: Boolean=False; DoQuote: Boolean=True): String;
      function QuoteIdent(Identifier: String; AlwaysQuote: Boolean=True; Glue: Char=#0): String;
      function DeQuoteIdent(Identifier: String; Glue: Char=#0): String;
      function QuotedDbAndTableName(DB, Obj: String): String;
      function FindObject(DB, Obj: String): TDBObject;
      function escChars(const Text: String; EscChar, Char1, Char2, Char3, Char4: Char): String;
      function UnescapeString(Text: String): String;
      function ExtractLiteral(var SQL: String; Prefix: String): String;
      function GetResults(SQL: String): TDBQuery;
      function GetCol(SQL: String; Column: Integer=0): TStringList;
      function GetVar(SQL: String; Column: Integer=0): String; overload;
      function GetVar(SQL: String; Column: String): String; overload;
      function Ping(Reconnect: Boolean): Boolean; virtual; abstract;
      function RefreshAllDatabases: TStringList;
      function GetDBObjects(db: String; Refresh: Boolean=False; OnlyNodeType: TListNodeType=lntNone): TDBObjectList;
      function DbObjectsCached(db: String): Boolean;
      function ParseDateTime(Str: String): TDateTime;
      function GetKeyColumns(Columns: TTableColumnList; Keys: TTableKeyList): TStringList;
      function ConnectionInfo: TStringList; virtual;
      function GetLastResults: TDBQueryList; virtual;
      function GetCreateCode(Obj: TDBObject): String; virtual;
      procedure PrefetchCreateCode(Objects: TDBObjectList);
      function GetSessionVariables(Refresh: Boolean): TDBQuery;
      function GetSessionVariable(VarName: String; DefaultValue: String=''; Refresh: Boolean=False): String;
      function MaxAllowedPacket: Int64; virtual;
      function GetSQLSpecifity(Specifity: TSQLSpecifityId): String;
      function ExplainAnalyzer(SQL, DatabaseName: String): Boolean; virtual;
      function GetDateTimeValue(Input: String; Datatype: TDBDatatypeIndex): String;
      procedure ClearDbObjects(db: String);
      procedure ClearAllDbObjects;
      procedure ParseViewStructure(CreateCode: String; DBObj: TDBObject;
        var Algorithm, Definer, SQLSecurity, CheckOption, SelectCode: String);
      procedure ParseRoutineStructure(Obj: TDBObject; Parameters: TRoutineParamList);
      procedure PurgePrefetchResults;
      function GetDatatypeByName(var DataType: String; DeleteFromSource: Boolean; Identifier: String=''): TDBDatatype;
      function GetDatatypeByNativeType(NativeType: Integer; Identifier: String=''): TDBDatatype;
      function ApplyLimitClause(QueryType, QueryBody: String; Limit, Offset: Int64): String;
      function LikeClauseTail: String;
      property Parameters: TConnectionParameters read FParameters write FParameters;
      property ThreadId: Int64 read GetThreadId;
      property ConnectionUptime: Integer read GetConnectionUptime;
      property ServerUptime: Integer read GetServerUptime;
      property ServerNow: TDateTime read GetServerNow;
      property CharacterSet: String read GetCharacterSet write SetCharacterSet;
      property LastErrorCode: Cardinal read GetLastErrorCode;
      property LastErrorMsg: String read GetLastErrorMsg;
      property ServerOS: String read FServerOS;
      property ServerVersionUntouched: String read FServerVersionUntouched;
      property QuoteChar: Char read FQuoteChar;
      property QuoteChars: String read FQuoteChars;
      function ServerVersionStr: String;
      function ServerVersionInt: Integer;
      function NdbClusterVersionInt: Integer;
      property RowsFound: Int64 read FRowsFound;
      property RowsAffected: Int64 read FRowsAffected;
      property WarningCount: Cardinal read FWarningCount;
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
      function ResultCount: Integer;
      property CurrentUserHostCombination: String read GetCurrentUserHostCombination;
      property AllUserHostCombinations: TStringList read GetAllUserHostCombinations;
      property LockedByThread: TThread read FLockedByThread write SetLockedByThread;
      property Datatypes: TDBDataTypeArray read FDatatypes;
      property Favorites: TStringList read FFavorites;
      property InfSch: String read FInfSch;
      function GetLockedTableCount(db: String): Integer;
      function IdentifierEquals(Ident1, Ident2: String): Boolean;
      function GetTableColumns(Table: TDBObject): TTableColumnList; virtual;
      function GetTableKeys(Table: TDBObject): TTableKeyList; virtual;
      function GetTableForeignKeys(Table: TDBObject): TForeignKeyList; virtual;
    published
      property Active: Boolean read FActive write SetActive default False;
      property Database: String read FDatabase write SetDatabase;
      property LogPrefix: String read FLogPrefix write FLogPrefix;
      property OnLog: TDBLogEvent read FOnLog write FOnLog;
      property OnConnected: TDBEvent read FOnConnected write FOnConnected;
      property OnDatabaseChanged: TDBEvent read FOnDatabaseChanged write FOnDatabaseChanged;
      property OnObjectnamesChanged: TDBEvent read FOnObjectnamesChanged write FOnObjectnamesChanged;
  end;
  TDBConnectionList = TObjectList<TDBConnection>;


  { TMySQLConnection }

  TMySQLRawResults = Array of PMYSQL_RES;
  TMySQLConnection = class(TDBConnection)
    private
      FHandle: PMYSQL;
      FLib: TMySQLLib;
      FLastRawResults: TMySQLRawResults;
      FStatementNum: Cardinal;
      procedure SetActive(Value: Boolean); override;
      procedure DoBeforeConnect; override;
      procedure DoAfterConnect; override;
      function GetThreadId: Int64; override;
      function GetCharacterSet: String; override;
      procedure SetCharacterSet(CharsetName: String); override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastErrorMsg: String; override;
      function GetAllDatabases: TStringList; override;
      function GetTableEngines: TStringList; override;
      function GetCollationTable: TDBQuery; override;
      function GetCharsetTable: TDBQuery; override;
      function GetCreateViewCode(Database, Name: String): String;
      function GetRowCount(Obj: TDBObject): Int64; override;
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); override;
      procedure SetLockedByThread(Value: TThread); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property Lib: TMySQLLib read FLib;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function ConnectionInfo: TStringList; override;
      function GetCreateCode(Obj: TDBObject): String; override;
      property LastRawResults: TMySQLRawResults read FLastRawResults;
      function MaxAllowedPacket: Int64; override;
      function ExplainAnalyzer(SQL, DatabaseName: String): Boolean; override;
      function GetTableColumns(Table: TDBObject): TTableColumnList; override;
      function GetTableKeys(Table: TDBObject): TTableKeyList; override;
  end;

  TAdoRawResults = Array of _RecordSet;
  TAdoDBConnection = class(TDBConnection)
    private
      FAdoHandle: TAdoConnection;
      FLastRawResults: TAdoRawResults;
      FLastError: String;
      procedure SetActive(Value: Boolean); override;
      procedure DoAfterConnect; override;
      function GetThreadId: Int64; override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastErrorMsg: String; override;
      function GetAllDatabases: TStringList; override;
      function GetCollationTable: TDBQuery; override;
      function GetCharsetTable: TDBQuery; override;
      function GetInformationSchemaObjects: TStringList; override;
      function GetRowCount(Obj: TDBObject): Int64; override;
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function ConnectionInfo: TStringList; override;
      function GetLastResults: TDBQueryList; override;
      property LastRawResults: TAdoRawResults read FLastRawResults;
      function GetTableColumns(Table: TDBObject): TTableColumnList; override;
      function GetTableForeignKeys(Table: TDBObject): TForeignKeyList; override;
  end;

  TPGRawResults = Array of PPGresult;
  TPQerrorfields = (PG_DIAG_SEVERITY, PG_DIAG_SQLSTATE, PG_DIAG_MESSAGE_PRIMARY, PG_DIAG_MESSAGE_DETAIL, PG_DIAG_MESSAGE_HINT, PG_DIAG_STATEMENT_POSITION, PG_DIAG_INTERNAL_POSITION, PG_DIAG_INTERNAL_QUERY, PG_DIAG_CONTEXT, PG_DIAG_SOURCE_FILE, PG_DIAG_SOURCE_LINE, PG_DIAG_SOURCE_FUNCTION);
  TPgConnection = class(TDBConnection)
    private
      FHandle: PPGconn;
      FLib: TPostgreSQLLib;
      FLastRawResults: TPGRawResults;
      FRegClasses: TOidStringPairs;
      procedure SetActive(Value: Boolean); override;
      procedure DoBeforeConnect; override;
      function GetThreadId: Int64; override;
      procedure SetCharacterSet(CharsetName: String); override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastErrorMsg: String; override;
      function GetAllDatabases: TStringList; override;
      function GetCharsetTable: TDBQuery; override;
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); override;
      procedure Drop(Obj: TDBObject); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property Lib: TPostgreSQLLib read FLib;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function ConnectionInfo: TStringList; override;
      function GetRowCount(Obj: TDBObject): Int64; override;
      property LastRawResults: TPGRawResults read FLastRawResults;
      property RegClasses: TOidStringPairs read FRegClasses;
      function GetTableColumns(Table: TDBObject): TTableColumnList; override;
      function GetTableKeys(Table: TDBObject): TTableKeyList; override;
      function GetTableForeignKeys(Table: TDBObject): TForeignKeyList; override;
  end;

  TSQLiteConnection = class;
  TSQLiteGridRows = class(TGridRows)
    private
      FConnection: TSQLiteConnection;
    public
      Statement: Psqlite3_stmt; // Used for querying result structures
      constructor Create(AOwner: TSQLiteConnection);
      destructor Destroy; override;
  end;
  TSQLiteRawResults = Array of TSQLiteGridRows;
  TSQLiteConnection = class(TDBConnection)
    private
      FHandle: Psqlite3;
      FLib: TSQLiteLib;
      FLastRawResults: TSQLiteRawResults;
      procedure SetActive(Value: Boolean); override;
      procedure DoBeforeConnect; override;
      function GetThreadId: Int64; override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastErrorMsg: String; override;
      function GetAllDatabases: TStringList; override;
      function GetCharsetTable: TDBQuery; override;
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property Lib: TSQLiteLib read FLib;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function GetCreateCode(Obj: TDBObject): String; override;
      function GetRowCount(Obj: TDBObject): Int64; override;
      property LastRawResults: TSQLiteRawResults read FLastRawResults;
      function GetTableColumns(Table: TDBObject): TTableColumnList; override;
      function GetTableKeys(Table: TDBObject): TTableKeyList; override;
      function GetTableForeignKeys(Table: TDBObject): TForeignKeyList; override;
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
      FAutoIncrementColumn: Integer;
      FColumnTypes: Array of TDBDatatype;
      FColumnLengths: TIntegerDynArray;
      FColumnFlags: TCardinalDynArray;
      FCurrentUpdateRow: TGridRow;
      FEof: Boolean;
      FStoreResult: Boolean;
      FColumns: TTableColumnList;
      FKeys: TTableKeyList;
      FForeignKeys: TForeignKeyList;
      FEditingPrepared: Boolean;
      FUpdateData: TGridRows;
      FDBObject: TDBObject;
      FFormatSettings: TFormatSettings;
      procedure SetRecNo(Value: Int64); virtual; abstract;
      function ColumnExists(Column: Integer): Boolean;
      procedure SetColumnOrgNames(Value: TStringList);
      procedure SetDBObject(Value: TDBObject);
      procedure CreateUpdateRow;
      function GetKeyColumns: TStringList;
      function GridQuery(QueryType, QueryBody: String): String;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; UseRawResult: Integer=-1); virtual; abstract;
      procedure First;
      procedure Next;
      function ColumnCount: Integer;
      function GetColBinData(Column: Integer; var baData: TBytes): Boolean; virtual;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload; virtual; abstract;
      function Col(ColumnName: String; IgnoreErrors: Boolean=False): String; overload;
      function ColumnLengths(Column: Integer): Int64; virtual;
      function HexValue(Column: Integer; IgnoreErrors: Boolean=False): String; overload;
      function HexValue(BinValue: String): String; overload;
      function HexValue(var ByteData: TBytes): String; overload;
      function DataType(Column: Integer): TDBDataType;
      function MaxLength(Column: Integer): Int64;
      function ValueList(Column: Integer): TStringList;
      // Todo: overload ColumnExists:
      function ColExists(Column: String): Boolean;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean; virtual; abstract;
      function ColIsUniqueKeyPart(Column: Integer): Boolean; virtual; abstract;
      function ColIsKeyPart(Column: Integer): Boolean; virtual; abstract;
      function ColIsVirtual(Column: Integer): Boolean;
      function ColAttributes(Column: Integer): TTableColumn;
      function IsNull(Column: Integer): Boolean; overload; virtual; abstract;
      function IsNull(Column: String): Boolean; overload;
      function IsFunction(Column: Integer): Boolean;
      function HasResult: Boolean; virtual; abstract;
      function GetWhereClause: String;
      procedure CheckEditable;
      function IsEditable: Boolean;
      procedure DeleteRow;
      function InsertRow: Int64;
      procedure SetCol(Column: Integer; NewText: String; Null: Boolean; IsFunction: Boolean);
      function EnsureFullRow(Refresh: Boolean): Boolean;
      function HasFullData: Boolean;
      function Modified(Column: Integer): Boolean; overload;
      function Modified: Boolean; overload;
      function Inserted: Boolean;
      function SaveModifications: Boolean;
      function DatabaseName: String; virtual; abstract;
      function TableName: String; overload;
      function TableName(Column: Integer): String; overload; virtual; abstract;
      function QuotedDbAndTableName: String;
      procedure DiscardModifications;
      procedure PrepareColumnAttributes;
      procedure PrepareEditing;
      property RecNo: Int64 read FRecNo write SetRecNo;
      property Eof: Boolean read FEof;
      property RecordCount: Int64 read FRecordCount;
      property ColumnNames: TStringList read FColumnNames;
      property StoreResult: Boolean read FStoreResult write FStoreResult;
      property ColumnOrgNames: TStringList read FColumnOrgNames write SetColumnOrgNames;
      property AutoIncrementColumn: Integer read FAutoIncrementColumn;
      property DBObject: TDBObject read FDBObject write SetDBObject;
      property SQL: String read FSQL write FSQL;
      property Connection: TDBConnection read FConnection;
  end;
  PDBQuery = ^TDBQuery;

  { TMySQLQuery }

  TMySQLQuery = class(TDBQuery)
    private
      FConnection: TMySQLConnection;
      FResultList: TMySQLRawResults;
      FCurrentResults: PMYSQL_RES;
      FCurrentRow: PMYSQL_ROW;
      procedure SetRecNo(Value: Int64); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; UseRawResult: Integer=-1); override;
      function GetColBinData(Column: Integer; var baData: TBytes): Boolean; override;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload; override;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean; override;
      function ColIsUniqueKeyPart(Column: Integer): Boolean; override;
      function ColIsKeyPart(Column: Integer): Boolean; override;
      function IsNull(Column: Integer): Boolean; overload; override;
      function HasResult: Boolean; override;
      function DatabaseName: String; override;
      function TableName(Column: Integer): String; overload; override;
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
      function TableName(Column: Integer): String; overload; override;
  end;

  TPGQuery = class(TDBQuery)
    private
      FConnection: TPgConnection;
      FCurrentResults: PPGresult;
      FRecNoLocal: Integer;
      FResultList: TPGRawResults;
      procedure SetRecNo(Value: Int64); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; UseRawResult: Integer=-1); override;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload; override;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean; override;
      function ColIsUniqueKeyPart(Column: Integer): Boolean; override;
      function ColIsKeyPart(Column: Integer): Boolean; override;
      function IsNull(Column: Integer): Boolean; overload; override;
      function HasResult: Boolean; override;
      function DatabaseName: String; override;
      function TableName(Column: Integer): String; overload; override;
  end;

  TSQLiteQuery = class(TDBQuery)
    private
      FConnection: TSQLiteConnection;
      FCurrentResults: TSQLiteGridRows;
      FRecNoLocal: Integer;
      FResultList: TSQLiteRawResults;
      procedure SetRecNo(Value: Int64); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; UseRawResult: Integer=-1); override;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload; override;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean; override;
      function ColIsUniqueKeyPart(Column: Integer): Boolean; override;
      function ColIsKeyPart(Column: Integer): Boolean; override;
      function IsNull(Column: Integer): Boolean; overload; override;
      function HasResult: Boolean; override;
      function DatabaseName: String; override;
      function TableName(Column: Integer): String; overload; override;
  end;

  TIdentToken = class(TObject)
    private
      FOriginal: string;
      FValue: string;
      FIsQuoted: Boolean;
      FConn: TDBConnection;
      procedure SetValue(const Value: string);
    public
      constructor Create(AConn: TDBConnection);
      property Original: string read FOriginal;
      property Value: string read FValue write SetValue;
      property IsQuoted: Boolean read FIsQuoted;
      procedure Parse(Text: string);
  end;

  TIdent = class(TObject)
    private
      FToken1: TIdentToken;
      FToken2: TIdentToken;
      FToken3: TIdentToken;
      FConn: TDBConnection;
    public
      constructor Create(AConn: TDBConnection);
      destructor Destroy; override;
      property Token1: TIdentToken read FToken1;
      property Token2: TIdentToken read FToken2;
      property Token3: TIdentToken read FToken3;
      procedure Parse(Text: string); virtual; abstract;
  end;

  TFieldIdent = class(TIdent)
    public
      procedure Parse(Text: string); override;
  end;

  TTableIdent = class(TIdent)
    public
      procedure Parse(Text: string); override;
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



implementation

uses apphelpers, loginform, change_password;



{ TProcessPipe }

constructor TProcessPipe.Create;
var
  Success: Boolean;
begin
  inherited;
  Success := CreatePipe(ReadHandle, WriteHandle, nil, 8192);
  if Success then
    Success := DuplicateHandle(
      GetCurrentProcess, ReadHandle,
      GetCurrentProcess, @ReadHandle, 0, True,
      DUPLICATE_CLOSE_SOURCE OR DUPLICATE_SAME_ACCESS
    );
  if Success then
    Success := DuplicateHandle(
      GetCurrentProcess, WriteHandle,
      GetCurrentProcess, @WriteHandle, 0, True,
      DUPLICATE_CLOSE_SOURCE OR DUPLICATE_SAME_ACCESS
    );
  if not Success then
    raise EDbError.Create(_('Error creating I/O pipes'));
end;


destructor TProcessPipe.Destroy;
begin
  CloseHandle(ReadHandle);
  CloseHandle(WriteHandle);
  inherited;
end;



{ TPlink }

constructor TPlink.Create(Connection: TDBConnection);
begin
  inherited Create;
  FConnection := Connection;
  FInPipe := TProcessPipe.Create;
  FOutPipe := TProcessPipe.Create;
  FErrorPipe := TProcessPipe.Create;
end;


destructor TPlink.Destroy;
begin
  FConnection.Log(lcInfo, f_('Closing plink.exe process #%d ...', [FProcessInfo.dwProcessId]));
  TerminateProcess(FProcessInfo.hProcess, 0);
  CloseHandle(FProcessInfo.hProcess);
  CloseHandle(FProcessInfo.hThread);
  FInPipe.Free;
  FOutPipe.Free;
  FErrorPipe.Free;
  inherited;
end;


procedure TPlink.Connect;
var
  PlinkCmd, PlinkCmdDisplay: String;
  OutText, ErrorText, UserInput: String;
  rx: TRegExpr;
  StartupInfo: TStartupInfo;
  ExitCode: LongWord;
  Waited, ReturnedSomethingAt, PortChecks: Integer;
begin
  // Check if local port is open
  PortChecks := 0;
  while not PortOpen(FConnection.Parameters.SSHLocalPort) do begin
    Inc(PortChecks);
    if PortChecks >= 20 then
      raise EDbError.CreateFmt(_('Could not execute PLink: Port %d already in use.'), [FConnection.Parameters.SSHLocalPort]);
    FConnection.Log(lcInfo, f_('Port #%d in use. Checking if #%d is available...', [FConnection.Parameters.SSHLocalPort, FConnection.Parameters.SSHLocalPort+1]));
    FConnection.Parameters.SSHLocalPort := FConnection.Parameters.SSHLocalPort + 1;
  end;

  // Build plink.exe command line
  // plink bob@domain.com -pw myPassw0rd1 -P 22 -i "keyfile.pem" -L 55555:localhost:3306
  PlinkCmd := FConnection.Parameters.SSHPlinkExe + ' -ssh ';
  if FConnection.Parameters.SSHUser.Trim <> '' then
    PlinkCmd := PlinkCmd + FConnection.Parameters.SSHUser.Trim + '@';
  if FConnection.Parameters.SSHHost.Trim <> '' then
    PlinkCmd := PlinkCmd + FConnection.Parameters.SSHHost.Trim
  else
    PlinkCmd := PlinkCmd + FConnection.Parameters.Hostname;
  if FConnection.Parameters.SSHPassword <> '' then begin
    // Escape double quote with backslash, see issue #261
    PlinkCmd := PlinkCmd + ' -pw "' + StringReplace(FConnection.Parameters.SSHPassword, '"', '\"', [rfReplaceAll]) + '"';
  end;
  if FConnection.Parameters.SSHPort > 0 then
    PlinkCmd := PlinkCmd + ' -P ' + IntToStr(FConnection.Parameters.SSHPort);
  if FConnection.Parameters.SSHPrivateKey <> '' then
    PlinkCmd := PlinkCmd + ' -i "' + FConnection.Parameters.SSHPrivateKey + '"';
  PlinkCmd := PlinkCmd + ' -N -L ' + IntToStr(FConnection.Parameters.SSHLocalPort) + ':' + FConnection.Parameters.Hostname + ':' + IntToStr(FConnection.Parameters.Port);
  rx := TRegExpr.Create;
  rx.Expression := '(-pw\s+")[^"]*(")';
  PlinkCmdDisplay := rx.Replace(PlinkCmd, '${1}******${2}', True);
  FConnection.Log(lcInfo, f_('Attempt to create plink.exe process, waiting %ds for response ...', [FConnection.Parameters.SSHTimeout]));
  FConnection.Log(lcInfo, PlinkCmdDisplay);

  // Prepare process
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput:= FInPipe.ReadHandle;
  StartupInfo.hStdError:= FErrorPipe.WriteHandle;
  StartupInfo.hStdOutput:= FOutPipe.WriteHandle;

  // Create plink.exe process
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
  if not CreateProcess(
       nil,
       PChar(PlinkCmd),
       nil,
       nil,
       true,
       CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
       nil,
       PChar(GetCurrentDir),
       StartupInfo,
       FProcessInfo) then begin
    ErrorText := CRLF + CRLF + PlinkCmdDisplay + CRLF + CRLF + 'System message: ' + SysErrorMessage(GetLastError);
    ErrorText := f_('Could not execute PLink: %s', [ErrorText]);
    raise EDbError.Create(ErrorText);
  end;

  // Wait until timeout has finished, or some text returned.
  // Parse pipe output and probably show some message in a dialog.
  Waited := 0;
  ReturnedSomethingAt := -1;
  while Waited < FConnection.Parameters.SSHTimeout*1000 do begin
    Inc(Waited, 200);
    WaitForSingleObject(FProcessInfo.hProcess, 200);
    GetExitCodeProcess(FProcessInfo.hProcess, ExitCode);
    if ExitCode <> STILL_ACTIVE then
      raise EDbError.CreateFmt(_('PLink exited unexpected. Command line was: %s'), [CRLF+PlinkCmdDisplay]);

    OutText := Trim(ReadPipe(FOutPipe));
    ErrorText := ReadPipe(FErrorPipe);
    if (OutText <> '') or (ErrorText <> '') then begin
      ReturnedSomethingAt := Waited;
      FConnection.Log(lcDebug, Format('PLink output after %d ms. OutPipe: "%s"  ErrorPipe: "%s"', [Waited, OutText, ErrorText]));
    end;

    if OutText <> '' then begin
      if ExecRegExpr('login as\s*\:', OutText) then begin
        // Prompt for username
        UserInput := InputBox('PLink:', OutText, '');
        SendText(UserInput + CRLF);
      end else if ExecRegExpr('(password|Passphrase for key "[^"]+")\s*\:', OutText) then begin
        // Prompt for sensitive input. Send * as first char of prompt param so InputBox hides input characters
        UserInput := InputBox('PLink:', #31+OutText, '');
        SendText(UserInput + CRLF);
      end else begin
        // Informational message box
        rx.Expression := '^[^\.]+\.';
        if rx.Exec(OutText) then begin // First words end with a dot - use it as caption
          MessageDialog('PLink: '+rx.Match[0], OutText, mtInformation, [mbOK])
        end else begin
          MessageDialog('PLink:', OutText, mtInformation, [mbOK]);
        end;
      end;
    end;

    if ErrorText <> '' then begin
      rx.Expression := '([^\.]+\?)(\s*\(y\/n\s*(,[^\)]+)?\)\s*)$';
      if rx.Exec(ErrorText) then begin
        // Prompt user with question
        case MessageDialog(Trim(rx.Match[1]), Copy(ErrorText, 1, Length(ErrorText)-rx.MatchLen[2]), mtConfirmation, [mbYes, mbNo, mbCancel]) of
          mrYes:
            SendText('y');
          mrNo:
            SendText('n');
          mrCancel: begin
            Destroy;
            raise EDbError.Create(_('PLink cancelled'));
          end;
        end;
      end else if ErrorText.StartsWith('Using username ', True)
        or ErrorText.StartsWith('Pre-authentication banner ', True)
        then begin
        // See #577 - new plink version sends this informational text to error pipe
        FConnection.Log(lcError, 'PLink: '+ErrorText);
        SendText(CRLF);
      end else begin
        // Any other error message goes here.
        if ErrorText.Contains('Access denied') then begin
          // This is a final connection error - end loop in this case
          Destroy;
          raise EDbError.Create(ErrorText);
        end else begin
          // Just show error text and proceed looping
          MessageDialog('PLink:', ErrorText, mtError, [mbOK]);
        end;
      end;
    end;

    // Exit loop after 2s idletime when there was output earlier
    if (ReturnedSomethingAt > 0) and (Waited >= ReturnedSomethingAt+2000) then
      Break;
    
    Application.ProcessMessages;
  end;
  rx.Free;
end;


function TPlink.ReadPipe(const Pipe: TProcessPipe): String;
var
  BufferReadCount, OutLen: Cardinal;
  BytesRemaining: Cardinal;
  Buffer: array [0..1023] of AnsiChar;
  R: AnsiString;
begin
  Result := '';
  if Pipe.ReadHandle = INVALID_HANDLE_VALUE then
    raise EDbError.Create(_('Error reading I/O pipes'));

  // Check if there is data to read from stdout
  PeekNamedPipe(Pipe.ReadHandle, nil, 0, nil, @BufferReadCount, nil);

  if BufferReadCount <> 0 then begin
    FillChar(Buffer, sizeof(Buffer), 'z');
    // Read by 1024 bytes chunks
    BytesRemaining := BufferReadCount;
    OutLen := 0;
    while BytesRemaining >= 1024 do begin
      // Read stdout pipe
      ReadFile(Pipe.ReadHandle, Buffer, 1024, BufferReadCount, nil);
      Dec(BytesRemaining, BufferReadCount);

      SetLength(R, OutLen + BufferReadCount);
      Move(Buffer, R[OutLen + 1], BufferReadCount);
      Inc(OutLen, BufferReadCount);
    end;

    if BytesRemaining > 0 then begin
      ReadFile(Pipe.ReadHandle, Buffer, BytesRemaining, BufferReadCount, nil);
      SetLength(R, OutLen + BufferReadCount);
      Move(Buffer, R[OutLen + 1], BufferReadCount);
    end;

    R := AsciiToAnsi(R);
    {$WARNINGS OFF}
    Result := AnsiToUtf8(R);
    {$WARNINGS ON}

    Result := CleanEscSeq(Result);
  end;

  Result := StringReplace(Result, #13+CRLF, CRLF, [rfReplaceAll]);
end;


function TPlink.AsciiToAnsi(Text: AnsiString): AnsiString;
const
  cMaxLength = 255;
var
  PText: PAnsiChar;
begin
  Result := '';
  PText := AnsiStrings.AnsiStrAlloc(cMaxLength);
  while Text <> '' do begin
    AnsiStrings.StrPCopy(PText, copy(Text, 1, cMaxLength-1));
    OemToAnsi(PText, PText);
    Result := Result + AnsiStrings.StrPas(PText);
    Delete(Text, 1, cMaxLength-1);
  end;
  AnsiStrings.StrDispose(PText);
end;


function TPlink.CleanEscSeq(const Buffer: String): String;
var
  i: Integer;
  chr: Char;
  EscFlag, Process: Boolean;
  EscBuffer: String[80];
begin
  Result := '';
  EscFlag := False;
  for i:=1 to Length(Buffer) do begin
    chr := buffer[I];
    if EscFLag then begin
      Process := False;
      if (Length(EscBuffer) = 0) and CharInSet(Chr, ['D', 'M', 'E', 'H', '7', '8', '=', '>', '<']) then
        Process := True
      else if (Length(EscBuffer) = 1) and (EscBuffer[1] in ['(', ')', '*', '+']) then
        Process := True
      else if CharInSet(Chr, ['0'..'9', ';', '?', ' '])
        or ((Length(EscBuffer) = 0) and CharInSet(chr, ['[', '(', ')', '*', '+']))
        then begin
        {$WARNINGS OFF}
        EscBuffer := EscBuffer + Chr;
        {$WARNINGS ON}
        if Length(EscBuffer) >= High(EscBuffer) then begin
          MessageBeep(MB_ICONASTERISK);
          EscBuffer := '';
          EscFlag := FALSE;
        end;
      end else
        Process := True;

      if Process then begin
        EscBuffer := '';
        EscFlag := False;
      end;
    end else if chr = #27 then begin
      EscBuffer := '';
      EscFlag := True;
    end;
    Result := Result + chr;
  end;
end;


procedure TPlink.SendText(Text: String);
var
  WrittenBytes: Cardinal;
  TextA: AnsiString;
begin
  {$WARNINGS OFF}
  TextA := Utf8ToAnsi(Text);
  {$WARNINGS ON}
  if TextA <> '' then
    WriteFile(FInPipe.WriteHandle, TextA[1], Length(TextA), WrittenBytes, nil);
end;



{ TConnectionParameters }

constructor TConnectionParameters.Create;
begin
  inherited Create;
  FIsFolder := False;

  FNetType := TNetType(AppSettings.GetDefaultInt(asNetType));
  FHostname := AppSettings.GetDefaultString(asHost);
  FLoginPrompt := AppSettings.GetDefaultBool(asLoginPrompt);
  FWindowsAuth := AppSettings.GetDefaultBool(asWindowsAuth);
  FCleartextPluginEnabled := AppSettings.GetDefaultBool(asCleartextPluginEnabled);
  FUsername := DefaultUsername;
  FPassword := AppSettings.GetDefaultString(asPassword);
  FPort := DefaultPort;
  FCompressed := AppSettings.GetDefaultBool(asCompressed);
  FAllDatabases := AppSettings.GetDefaultString(asDatabases);
  FLibraryOrProvider := DefaultLibrary;
  FComment := AppSettings.GetDefaultString(asComment);

  FSSHHost := AppSettings.GetDefaultString(asSSHtunnelHost);
  FSSHPort := AppSettings.GetDefaultInt(asSSHtunnelHostPort);
  FSSHUser := AppSettings.GetDefaultString(asSSHtunnelUser);
  FSSHPassword := AppSettings.GetDefaultString(asSSHtunnelPassword);
  FSSHTimeout := AppSettings.GetDefaultInt(asSSHtunnelTimeout);
  FSSHPrivateKey := AppSettings.GetDefaultString(asSSHtunnelPrivateKey);
  FSSHLocalPort := FPort + 1;

  FWantSSL := AppSettings.GetDefaultBool(asSSLActive);
  FSSLPrivateKey := AppSettings.GetDefaultString(asSSLKey);
  FSSLCertificate := AppSettings.GetDefaultString(asSSLCert);
  FSSLCACertificate := AppSettings.GetDefaultString(asSSLCA);
  FSSLCipher := AppSettings.GetDefaultString(asSSLCipher);
  FStartupScriptFilename := AppSettings.GetDefaultString(asStartupScriptFilename);
  FQueryTimeout := AppSettings.GetDefaultInt(asQueryTimeout);
  FKeepAlive := AppSettings.GetDefaultInt(asKeepAlive);
  FLocalTimeZone := AppSettings.GetDefaultBool(asLocalTimeZone);
  FFullTableStatus := AppSettings.GetDefaultBool(asFullTableStatus);

  FSessionColor := AppSettings.GetDefaultInt(asTreeBackground);

  // Must be read without session path
  FSSHPlinkExe := AppSettings.ReadString(asPlinkExecutable);

  FLastConnect := 0;
  FCounter := 0;
  FServerVersion := '';
end;


constructor TConnectionParameters.Create(SessionRegPath: String);
var
  DummyDate: TDateTime;
begin
  // Parameters from stored registry key
  Create;

  if not AppSettings.SessionPathExists(SessionRegPath) then
    raise Exception.Create(f_('Error: Session "%s" not found in registry.', [SessionRegPath]));

  FSessionPath := SessionRegPath;
  AppSettings.SessionPath := SessionRegPath;

  if AppSettings.ValueExists(asSessionFolder) then begin
    FIsFolder := True;
  end else begin
    FSessionColor := AppSettings.ReadInt(asTreeBackground);
    FNetType := TNetType(AppSettings.ReadInt(asNetType));
    if (FNetType > High(TNetType)) or (FNetType < Low(TNetType)) then begin
      ErrorDialog(f_('Broken "NetType" value (%d) found in settings for session "%s".', [Integer(FNetType), FSessionPath])
        +CRLF+CRLF+
        f_('Please report that on %s', ['https://github.com/HeidiSQL/HeidiSQL'])
        );
      FNetType := ntMySQL_TCPIP;
    end;
    FHostname := AppSettings.ReadString(asHost);
    FUsername := AppSettings.ReadString(asUser);
    FPassword := decrypt(AppSettings.ReadString(asPassword));
    FLoginPrompt := AppSettings.ReadBool(asLoginPrompt);
    FWindowsAuth := AppSettings.ReadBool(asWindowsAuth);
    FCleartextPluginEnabled := AppSettings.ReadBool(asCleartextPluginEnabled);
    FPort := MakeInt(AppSettings.ReadString(asPort));
    FCompressed := AppSettings.ReadBool(asCompressed);
    FAllDatabases := AppSettings.ReadString(asDatabases);
    FLibraryOrProvider := AppSettings.ReadString(asLibrary);
    FComment := AppSettings.ReadString(asComment);

    FSSHHost := AppSettings.ReadString(asSSHtunnelHost);
    FSSHPort := AppSettings.ReadInt(asSSHtunnelHostPort);
    FSSHUser := AppSettings.ReadString(asSSHtunnelUser);
    FSSHPassword := decrypt(AppSettings.ReadString(asSSHtunnelPassword));
    FSSHTimeout := AppSettings.ReadInt(asSSHtunnelTimeout);
    if FSSHTimeout < 1 then
      FSSHTimeout := 1;
    FSSHPrivateKey := AppSettings.ReadString(asSSHtunnelPrivateKey);
    FSSHLocalPort := AppSettings.ReadInt(asSSHtunnelPort);

    FSSLPrivateKey := AppSettings.ReadString(asSSLKey);
    // Auto-activate SSL for sessions created before UseSSL was introduced:
    FWantSSL := AppSettings.ReadBool(asSSLActive, '', FSSLPrivateKey<>'');
    FSSLCertificate := AppSettings.ReadString(asSSLCert);
    FSSLCACertificate := AppSettings.ReadString(asSSLCA);
    FSSLCipher := AppSettings.ReadString(asSSLCipher);
    FStartupScriptFilename := AppSettings.ReadString(asStartupScriptFilename);
    FQueryTimeout := AppSettings.ReadInt(asQueryTimeout);
    FKeepAlive := AppSettings.ReadInt(asKeepAlive);
    if FKeepAlive = 0 then // Old connections had always 0 as default value
      FKeepAlive := AppSettings.GetDefaultInt(asKeepAlive);
    FLocalTimeZone := AppSettings.ReadBool(asLocalTimeZone);
    FFullTableStatus := AppSettings.ReadBool(asFullTableStatus);

    FServerVersion := AppSettings.ReadString(asServerVersionFull);
    DummyDate := 0;
    FLastConnect := StrToDateTimeDef(AppSettings.ReadString(asLastConnect), DummyDate);
    FCounter := AppSettings.ReadInt(asConnectCount);
    AppSettings.ResetPath;

    // Must be read without session path
    FSSHPlinkExe := AppSettings.ReadString(asPlinkExecutable);
  end;
end;


procedure TConnectionParameters.SaveToRegistry;
var
  IsNew: Boolean;
begin
  // Save current values to registry
  IsNew := not AppSettings.SessionPathExists(FSessionPath);
  AppSettings.SessionPath := FSessionPath;
  if IsNew then
    AppSettings.WriteString(asSessionCreated, DateTimeToStr(Now));
  if FIsFolder then
    AppSettings.WriteBool(asSessionFolder, True)
  else begin
    AppSettings.WriteString(asHost, FHostname);
    AppSettings.WriteBool(asWindowsAuth, FWindowsAuth);
    AppSettings.WriteBool(asCleartextPluginEnabled, FCleartextPluginEnabled);
    AppSettings.WriteString(asUser, FUsername);
    AppSettings.WriteString(asPassword, encrypt(FPassword));
    AppSettings.WriteBool(asLoginPrompt, FLoginPrompt);
    AppSettings.WriteString(asPort, IntToStr(FPort));
    AppSettings.WriteInt(asNetType, Integer(FNetType));
    AppSettings.WriteBool(asCompressed, FCompressed);
    AppSettings.WriteBool(asLocalTimeZone, FLocalTimeZone);
    AppSettings.WriteInt(asQueryTimeout, FQueryTimeout);
    AppSettings.WriteInt(asKeepAlive, FKeepAlive);
    AppSettings.WriteBool(asFullTableStatus, FFullTableStatus);
    AppSettings.WriteString(asDatabases, FAllDatabases);
    AppSettings.WriteString(asLibrary, FLibraryOrProvider);
    AppSettings.WriteString(asComment, FComment);
    AppSettings.WriteString(asStartupScriptFilename, FStartupScriptFilename);
    AppSettings.WriteInt(asTreeBackground, FSessionColor);
    AppSettings.WriteString(asSSHtunnelHost, FSSHHost);
    AppSettings.WriteInt(asSSHtunnelHostPort, FSSHPort);
    AppSettings.WriteString(asSSHtunnelUser, FSSHUser);
    AppSettings.WriteString(asSSHtunnelPassword, encrypt(FSSHPassword));
    AppSettings.WriteInt(asSSHtunnelTimeout, FSSHTimeout);
    AppSettings.WriteString(asSSHtunnelPrivateKey, FSSHPrivateKey);
    AppSettings.WriteInt(asSSHtunnelPort, FSSHLocalPort);
    AppSettings.WriteBool(asSSLActive, FWantSSL);
    AppSettings.WriteString(asSSLKey, FSSLPrivateKey);
    AppSettings.WriteString(asSSLCert, FSSLCertificate);
    AppSettings.WriteString(asSSLCA, FSSLCACertificate);
    AppSettings.WriteString(asSSLCipher, FSSLCipher);
    AppSettings.ResetPath;
    AppSettings.WriteString(asPlinkExecutable, FSSHPlinkExe);
  end;
end;


function TConnectionParameters.CreateConnection(AOwner: TComponent): TDBConnection;
begin
  case NetTypeGroup of
    ngMySQL:
      Result := TMySQLConnection.Create(AOwner);
    ngMSSQL:
      Result := TAdoDBConnection.Create(AOwner);
    ngPgSQL:
      Result := TPgConnection.Create(AOwner);
    ngSQLite:
      Result := TSQLiteConnection.Create(AOwner);
    else
      raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(FNetType)]);
  end;
  Result.Parameters := Self;
end;


function TConnectionParameters.CreateQuery(Connection: TDbConnection): TDBQuery;
begin
  case NetTypeGroup of
    ngMySQL:
      Result := TMySQLQuery.Create(Connection);
    ngMSSQL:
      Result := TAdoDBQuery.Create(Connection);
    ngPgSQL:
      Result := TPGQuery.Create(Connection);
    ngSQLite:
      Result := TSQLiteQuery.Create(Connection);
    else
      raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(FNetType)]);
  end;
end;


function TConnectionParameters.NetTypeName(LongFormat: Boolean): String;
var
  Prefix: String;
begin
  // Return the name of a net type, either in short or long format
  case NetTypeGroup of
    ngMySQL: begin
      if IsMariaDB then
        Prefix := 'MariaDB'
      else if IsPercona then
        Prefix := 'Percona'
      else if IsTokudb then
        Prefix := 'TokuDB'
      else if IsInfiniDB then
        Prefix := 'InfiniDB'
      else if IsInfobright then
        Prefix := 'Infobright'
      else if IsMemSQL then
        Prefix := 'MemSQL'
      else if IsMySQL then
        Prefix := 'MySQL'
      else
        Prefix := 'MariaDB or MySQL';
    end;
    ngMSSQL: begin
      Prefix := 'Microsoft SQL Server';
    end;
    ngPgSQL: begin
      if IsRedshift then
        Prefix := 'Redshift PG'
      else
        Prefix := 'PostgreSQL';
    end;
    ngSQLite: begin
      Prefix := 'SQLite';
    end;
  end;

  case LongFormat of
    True: case FNetType of
      ntMySQL_TCPIP:
        Result := Prefix+' (TCP/IP)';
      ntMySQL_NamedPipe:
        Result := Prefix+' (named pipe)';
      ntMySQL_SSHtunnel:
        Result := Prefix+' (SSH tunnel)';
      ntMSSQL_NamedPipe:
        Result := Prefix+' (named pipe)';
      ntMSSQL_TCPIP:
        Result := Prefix+' (TCP/IP)';
      ntMSSQL_SPX:
        Result := Prefix+' (SPX/IPX)';
      ntMSSQL_VINES:
        Result := Prefix+' (Banyan VINES)';
      ntMSSQL_RPC:
        Result := Prefix+' (Windows RPC)';
      ntPgSQL_TCPIP:
        Result := Prefix+' (TCP/IP)';
      ntPgSQL_SSHtunnel:
        Result := Prefix+' (SSH tunnel)';
      ntSQLite:
        Result := Prefix+' (Experimental)';
      else
        Result := Prefix;
    end;

    False: case NetTypeGroup of
      ngMSSQL:
        Result := 'MS SQL';
      else
        Result := Prefix;
    end;
  end;
end;


function TConnectionParameters.IsCompatibleToWin10S: Boolean;
begin
  // Using plink on 10S is not possible
  Result := (FNetType <> ntMySQL_SSHtunnel) and (FNetType <> ntPgSQL_SSHtunnel);
end;


function TConnectionParameters.GetNetTypeGroup: TNetTypeGroup;
begin
  case FNetType of
    ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel:
      Result := ngMySQL;
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP, ntMSSQL_SPX, ntMSSQL_VINES, ntMSSQL_RPC:
      Result := ngMSSQL;
    ntPgSQL_TCPIP, ntPgSQL_SSHtunnel:
      Result := ngPgSQL;
    ntSQLite:
      Result := ngSQLite;
    else begin
      // Return default net group here. Raising an exception lets the app die for some reason.
      // Reproduction: click drop-down button on "Database(s)" session setting
      //raise EDbError.CreateFmt(_(MsgUnhandledNetType), [Integer(FNetType)]);
      Result := ngMySQL;
    end;
  end;
end;


function TConnectionParameters.IsAnyMySQL: Boolean;
begin
  Result := NetTypeGroup = ngMySQL;
end;


function TConnectionParameters.IsAnyMSSQL: Boolean;
begin
  Result := NetTypeGroup = ngMSSQL;
end;


function TConnectionParameters.IsAnyPostgreSQL: Boolean;
begin
  Result := NetTypeGroup = ngPgSQL;
end;


function TConnectionParameters.IsAnySQLite;
begin
  Result := NetTypeGroup = ngSQLite;
end;


function TConnectionParameters.IsMariaDB: Boolean;
begin
  Result := IsAnyMySQL and (Pos('-mariadb', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.IsMySQL: Boolean;
begin
  Result := IsAnyMySQL
    and (not IsMariaDB)
    and (not IsPercona)
    and (not IsTokudb)
    and (not IsInfiniDB)
    and (not IsInfobright)
    and (not IsMemSQL);
end;


function TConnectionParameters.IsPercona: Boolean;
begin
  Result := IsAnyMySQL and (Pos('percona server', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.IsTokudb: Boolean;
begin
  Result := IsAnyMySQL and (Pos('tokudb', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.IsInfiniDB: Boolean;
begin
  Result := IsAnyMySQL and (Pos('infinidb', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.IsInfobright: Boolean;
begin
  Result := IsAnyMySQL and (Pos('infobright', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.IsAzure: Boolean;
begin
  Result := IsAnyMSSQL and (Pos('azure', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.IsMemSQL: Boolean;
begin
  Result := IsAnyMySQL and (Pos('memsql', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.IsRedshift: Boolean;
begin
  Result := IsAnyPostgreSQL and (Pos('redshift', LowerCase(ServerVersion)) > 0);
end;


function TConnectionParameters.GetImageIndex: Integer;
begin
  if IsFolder then
    Result := 174
  else case NetTypeGroup of
    ngMySQL: begin
      Result := 164;
      if IsMariaDB then Result := 166
      else if IsPercona then Result := 169
      else if IsTokudb then Result := 171
      else if IsInfiniDB then Result := 172
      else if IsInfobright then Result := 173
      else if IsMemSQL then Result := 194;
    end;
    ngMSSQL: begin
      Result := 123;
      if IsAzure then Result := 188;
    end;
    ngPgSQL: begin
      Result := 187;
      if IsRedshift then Result := 195;
    end;
    ngSQLite: begin
      Result := 196;
    end
    else Result := ICONINDEX_SERVER;
  end;
end;


function TConnectionParameters.DefaultPort: Integer;
begin
  case NetTypeGroup of
    ngMySQL: Result := 3306;
    ngMSSQL: Result := 1433;
    ngPgSQL: Result := 5432;
    else Result := 0;
  end;
end;


function TConnectionParameters.DefaultUsername: String;
begin
  case NetTypeGroup of
    ngMySQL: Result := 'root';
    ngMSSQL: Result := 'sa';
    ngPgSQL: Result := 'postgres';
    else Result := '';
  end;
end;


function TConnectionParameters.DefaultLibrary: String;
begin
  case NetTypeGroup of
    ngMySQL: Result := 'libmariadb.dll';
    ngMSSQL: Result := 'MSOLEDBSQL'; // Prefer MSOLEDBSQL provider on newer systems
    ngPgSQL: Result := 'libpq.dll';
    ngSQLite: Result := 'sqlite3.dll';
    else Result := '';
  end;
end;


function TConnectionParameters.GetLibraries: TStringList;
var
  rx: TRegExpr;
  Dlls: TStringDynArray;
  DllPath, DllFile: String;
  FoundLibs, Providers: TStringList;
  Provider: String;
begin
  if not Assigned(FLibraries) then begin
    FLibraries := TNetGroupLibs.Create;
  end;

  if not FLibraries.ContainsKey(NetTypeGroup) then begin
    FoundLibs := TStringList.Create;
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    case NetTypeGroup of
      ngMySQL: rx.Expression := '^lib(mysql|mariadb).*\.dll$';
      ngMSSQL: rx.Expression := '^(MSOLEDBSQL|SQLOLEDB)$';
      ngPgSQL: rx.Expression := '^libpq.*\.dll$';
      ngSQLite: rx.Expression := '^sqlite.*\.dll$';
    end;
    case NetTypeGroup of
      ngMySQL, ngPgSQL, ngSQLite: begin
        Dlls := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)), '*.dll');
        for DllPath in Dlls do begin
          DllFile := ExtractFileName(DllPath);
          if rx.Exec(DllFile) then begin
            FoundLibs.Add(DllFile);
          end;
        end;
        SetLength(Dlls, 0);
      end;
      ngMSSQL: begin
        Providers := TStringList.Create;
        GetProviderNames(Providers);
        for Provider in Providers do begin
          if rx.Exec(Provider) then begin
            FoundLibs.Add(Provider);
          end;
        end;
        Providers.Free;
      end;
    end;
    rx.Free;
    FLibraries.Add(NetTypeGroup, FoundLibs);
  end;
  FLibraries.TryGetValue(NetTypeGroup, Result);
end;


function TConnectionParameters.GetSessionName: String;
var
  LastBackSlash: Integer;
begin
  LastBackSlash := LastDelimiter('\', FSessionPath);
  if LastBackSlash > 0 then
    Result := Copy(FSessionPath, LastBackSlash+1, MaxInt)
  else
    Result := FSessionPath;
end;




{ TMySQLConnection }

constructor TDBConnection.Create(AOwner: TComponent);
begin
  inherited;
  FParameters := TConnectionParameters.Create;
  FRowsFound := 0;
  FRowsAffected := 0;
  FWarningCount := 0;
  FConnectionStarted := 0;
  FLastQueryDuration := 0;
  FLastQueryNetworkDuration := 0;
  FThreadID := 0;
  FLogPrefix := '';
  FIsUnicode := False;
  FIsSSL := False;
  FDatabaseCache := TDatabaseCache.Create(True);
  FLoginPromptDone := False;
  FCurrentUserHostCombination := '';
  FKeepAliveTimer := TTimer.Create(Self);
  FFavorites := TStringList.Create;
  FForeignKeyQueriesFailed := False;
  // System database/schema, should be uppercase on MSSQL only, see #855
  FInfSch := 'information_schema';
end;


constructor TMySQLConnection.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FQuoteChar := '`';
  FQuoteChars := '`"';
  FStatementNum := 0;
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
  FQuoteChars := '"[]';
  SetLength(FDatatypes, Length(MSSQLDatatypes));
  for i:=0 to High(MSSQLDatatypes) do
    FDatatypes[i] := MSSQLDatatypes[i];
  FInfSch := 'INFORMATION_SCHEMA';
end;


constructor TPgConnection.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FQuoteChar := '"';
  FQuoteChars := '"';
  SetLength(FDatatypes, Length(PostGreSQLDatatypes));
  for i:=0 to High(PostGreSQLDatatypes) do
    FDatatypes[i] := PostGreSQLDatatypes[i];
  // cache for 123::regclass queries:
  FRegClasses := TOidStringPairs.Create;
end;


constructor TSQLiteConnection.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FQuoteChar := '"';
  FQuoteChars := '"[]';
  SetLength(FDatatypes, Length(SQLiteDatatypes));
  for i:=0 to High(SQLiteDatatypes) do
    FDatatypes[i] := SQLiteDatatypes[i];
  // SQLite does not have IS:
  FInfSch := '';
end;


destructor TDBConnection.Destroy;
begin
  ClearCache(True);
  FKeepAliveTimer.Free;
  FFavorites.Free;
  inherited;
end;

destructor TMySQLConnection.Destroy;
begin
  if Active then Active := False;
  FLib.Free;
  inherited;
end;


destructor TAdoDBConnection.Destroy;
begin
  if Active then Active := False;
  FreeAndNil(FAdoHandle);
  inherited;
end;


destructor TPgConnection.Destroy;
begin
  if Active then Active := False;
  FRegClasses.Free;
  FLib.Free;
  inherited;
end;


destructor TSQLiteConnection.Destroy;
begin
  if Active then Active := False;
  FLib.Free;
  inherited;
end;


function TDBConnection.GetDatatypeByName(var DataType: String; DeleteFromSource: Boolean; Identifier: String=''): TDBDatatype;
var
  i, MatchLen: Integer;
  Match: Boolean;
  rx: TRegExpr;
  Types, tmp: String;
  TypesSorted: TStringList;
begin
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  MatchLen := 0;
  for i:=0 to High(FDatatypes) do begin
    Types := FDatatypes[i].Name;
    if FDatatypes[i].Names <> '' then begin
      Types := Types + '|' + FDatatypes[i].Names;
      // Move more exact (longer) types to the beginning
      TypesSorted := Explode('|', Types);
      TypesSorted.CustomSort(StringListCompareByLength);
      Types := ImplodeStr('|', TypesSorted);
      TypesSorted.Free;
    end;

    rx.Expression := '^('+Types+')\b(\[\])?';
    Match := rx.Exec(DataType);
    // Prefer a later match which is longer than the one found before.
    // See http://www.heidisql.com/forum.php?t=17061
    if Match and (rx.MatchLen[1] > MatchLen) then begin
      Log(lcDebug, 'GetDatatypeByName: "'+DataType+'" : '+rx.Match[1]);
      if (FParameters.NetTypeGroup = ngPgSQL) and (rx.MatchLen[2] > 0) then begin
        // TODO: detect array style datatypes, e.g. TEXT[]
      end else begin
        MatchLen := rx.MatchLen[1];
        Result := FDatatypes[i];
      end;
    end;
  end;

  if (MatchLen > 0) and DeleteFromSource then begin
    Delete(DataType, 1, MatchLen);
  end;

  if MatchLen = 0 then begin
    // Fall back to unknown type
    Result := Datatypes[0];
    rx.Expression := '^(\S+)';
    if rx.Exec(DataType) then
      tmp := rx.Match[1]
    else
      tmp := DataType;
    if Identifier <> '' then
      Log(lcError, f_('Unknown datatype "%0:s" for "%1:s". Fall back to %2:s.', [tmp, Identifier, Result.Name]))
    else
      Log(lcError, f_('Unknown datatype "%0:s". Fall back to %1:s.', [tmp, Result.Name]));
  end;
  rx.Free;
end;


function TDBConnection.GetDatatypeByNativeType(NativeType: Integer; Identifier: String=''): TDBDatatype;
var
  i: Integer;
  rx: TRegExpr;
  TypeFound: Boolean;
begin
  rx := TRegExpr.Create;
  TypeFound := False;
  for i:=0 to High(Datatypes) do begin
    if Datatypes[i].NativeTypes = '' then
      Continue;
    rx.Expression := '\b('+Datatypes[i].NativeTypes+')\b';
    if rx.Exec(IntToStr(NativeType)) then begin
      Result := Datatypes[i];
      TypeFound := True;
      break;
    end;
  end;
  if not TypeFound then begin
    // Fall back to unknown type
    Result := Datatypes[0];
    if Identifier <> '' then
      Log(lcError, f_('Unknown datatype oid #%0:d for "%1:s". Fall back to %2:s.', [NativeType, Identifier, Result.Name]))
    else
      Log(lcError, f_('Unknown datatype oid #%0:d. Fall back to %1:s.', [NativeType, Result.Name]));
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
      // We're running in a thread already. Ensure that Log() is able to detect that.
      FLockedByThread := Value;
      Log(lcDebug, 'mysql_thread_init, thread id #'+IntToStr(Value.ThreadID));
      FLib.mysql_thread_init;
    end else begin
      FLib.mysql_thread_end;
      Log(lcDebug, 'mysql_thread_end, thread id #'+IntToStr(FLockedByThread.ThreadID));
      FLockedByThread := Value;
    end;
  end;
end;


{**
  (Dis-)Connect to/from server
}
procedure TMySQLConnection.SetActive( Value: Boolean );
var
  Connected: PMYSQL;
  ClientFlags, FinalPort: Integer;
  Error, tmpdb, FinalHost, FinalSocket, StatusName: String;
  sslca, sslkey, sslcert, sslcipher: PAnsiChar;
  PluginDir: AnsiString;
  Status: TDBQuery;
  PasswordChangeDialog: TfrmPasswordChange;
  SetOptionResult: Integer;
begin
  if Value and (FHandle = nil) then begin

    // Die if trying to run plink on Win10S
    if RunningOnWindows10S and (not FParameters.IsCompatibleToWin10S) then begin
      raise EDbError.Create(_('The network type defined for this session is not compatible to your Windows 10 S'));
    end;

    DoBeforeConnect;

    // Get handle
    FHandle := FLib.mysql_init(nil);

    // Prepare special stuff for SSL and SSH tunnel
    FinalHost := FParameters.Hostname;
    FinalSocket := '';
    FinalPort := FParameters.Port;

    if FParameters.WantSSL then begin
      // mysql_ssl_set() wants nil, while PAnsiChar(AnsiString()) is never nil
      sslkey := nil;
      sslcert := nil;
      sslca := nil;
      sslcipher := nil;
      if FParameters.SSLPrivateKey <> '' then
        sslkey := PAnsiChar(AnsiString(FParameters.SSLPrivateKey));
      if FParameters.SSLCertificate <> '' then
        sslcert := PAnsiChar(AnsiString(FParameters.SSLCertificate));
      if FParameters.SSLCACertificate <> '' then
        sslca := PAnsiChar(AnsiString(FParameters.SSLCACertificate));
      if FParameters.SSLCipher <> '' then
        sslcipher := PAnsiChar(AnsiString(FParameters.SSLCipher));
      { TODO : Use Cipher and CAPath parameters }
      FLib.mysql_ssl_set(FHandle,
        sslkey,
        sslcert,
        sslca,
        nil,
        sslcipher);
      Log(lcInfo, _('SSL parameters successfully set.'));
    end;

    case FParameters.NetType of
      ntMySQL_TCPIP: begin
      end;

      ntMySQL_NamedPipe: begin
        FinalHost := '.';
        FinalSocket := FParameters.Hostname;
      end;

      ntMySQL_SSHtunnel: begin
        // Create plink.exe process
        FPlink := TPlink.Create(Self);
        FPlink.Connect;
        FinalHost := '127.0.0.1';
        FinalPort := FParameters.SSHLocalPort;
      end;
    end;

    // Gather client options
    ClientFlags := CLIENT_LOCAL_FILES or CLIENT_INTERACTIVE or CLIENT_PROTOCOL_41 or CLIENT_MULTI_STATEMENTS or CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS;
    if Parameters.Compressed then
      ClientFlags := ClientFlags or CLIENT_COMPRESS;
    if Parameters.WantSSL then
      ClientFlags := ClientFlags or CLIENT_SSL;

    // Point libmysql to the folder with client plugins
    PluginDir := AnsiString(ExtractFilePath(ParamStr(0))+'plugins');
    SetOptionResult := FLib.mysql_options(FHandle, Integer(MYSQL_PLUGIN_DIR), PAnsiChar(PluginDir));
    if SetOptionResult <> 0 then begin
      raise EDbError.Create(f_('Plugin directory %s could not be set.', [PluginDir]));
    end;

    // Define which TLS protocol versions are allowed.
    // See https://www.heidisql.com/forum.php?t=27158
    // See https://mariadb.com/kb/en/library/mysql_optionsv/
    FLib.mysql_options(FHandle, Integer(MARIADB_OPT_TLS_VERSION), PAnsiChar('TLSv1,TLSv1.1,TLSv1.2,TLSv1.3'));
    FLib.mysql_options(FHandle, Integer(MYSQL_OPT_TLS_VERSION), PAnsiChar('TLSv1,TLSv1.1,TLSv1.2,TLSv1.3'));

    // Enable cleartext plugin
    if Parameters.CleartextPluginEnabled then
      FLib.mysql_options(FHandle, Integer(MYSQL_ENABLE_CLEARTEXT_PLUGIN), PAnsiChar('1'));

    // Tell server who we are
    if Assigned(FLib.mysql_optionsv) then
      FLib.mysql_optionsv(FHandle, Integer(MYSQL_OPT_CONNECT_ATTR_ADD), 'program_name', APPNAME);

    Connected := FLib.mysql_real_connect(
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
      Error := LastErrorMsg;
      Log(lcError, Error);
      FConnectionStarted := 0;
      FHandle := nil;
      if FPlink <> nil then
        FPlink.Free;
      raise EDbError.Create(Error);
    end else begin
      FActive := True;
      // Catch late init_connect error by firing mysql_ping(), which detects a broken
      // connection without running into some access violation. See issue #3464.
      Ping(False);
      if not FActive then
        raise EDbError.CreateFmt(_('Connection closed immediately after it was established. '+
          'This is mostly caused by an "%s" server variable which has errors in itself, '+
          'or your user account does not have the required privileges for it to run.'+CRLF+CRLF+
          'You may ask someone with SUPER privileges'+CRLF+
          '* either to fix the "%s" variable,'+CRLF+
          '* or to grant you missing privileges.'),
          ['init_connect', 'init_connect']);
      // Try to fire the very first query against the server, which probably run into the following error:
      // "Error 1820: You must SET PASSWORD before executing this statement"
      try
        ThreadId;
      except
        on E:EDbError do begin
          if GetLastErrorCode =  1820 then begin
            PasswordChangeDialog := TfrmPasswordChange.Create(Self);
            PasswordChangeDialog.lblHeading.Caption := GetLastErrorMsg;
            PasswordChangeDialog.ShowModal;
            if PasswordChangeDialog.ModalResult = mrOk then begin
              if ExecRegExpr('\sALTER USER\s', GetLastErrorMsg) then
                Query('ALTER USER USER() IDENTIFIED BY '+EscapeString(PasswordChangeDialog.editPassword.Text))
              else
                Query('SET PASSWORD=PASSWORD('+EscapeString(PasswordChangeDialog.editPassword.Text)+')');
            end else // Dialog cancelled
              Raise;
            PasswordChangeDialog.Free;
          end else
            Raise;
        end;
      end;
      Log(lcInfo, f_('Connected. Thread-ID: %d', [ThreadId]));
      try
        CharacterSet := 'utf8mb4';
      except
        on E:EDbError do try
          Log(lcError, E.Message);
          CharacterSet := 'utf8';
        except
          on E:EDbError do
            Log(lcError, E.Message);
        end;
      end;
      Log(lcInfo, _('Characterset')+': '+GetCharacterSet);
      FConnectionStarted := GetTickCount div 1000;
      FServerUptime := -1;
      Status := GetResults('SHOW STATUS');
      while not Status.Eof do begin
        StatusName := LowerCase(Status.Col(0));
        if StatusName = 'uptime' then
          FServerUptime := StrToIntDef(Status.Col(1), FServerUptime)
        else if StatusName = 'ssl_cipher' then
          FIsSSL := Status.Col(1) <> '';
        Status.Next;
      end;
      FServerDateTimeOnStartup := GetVar('SELECT NOW()');
      FServerOS := GetSessionVariable('version_compile_os');
      FRealHostname := GetSessionVariable('hostname');
      FServerVersionUntouched := GetSessionVariable('version') + ' - ' + GetSessionVariable('version_comment');
      FServerVersionUntouched := FServerVersionUntouched.Trim([' ', '-']);
      if FServerVersionUntouched.IsEmpty then begin
        FServerVersionUntouched := DecodeAPIString(FLib.mysql_get_server_info(FHandle));
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
    FLib.mysql_close(FHandle);
    FActive := False;
    ClearCache(False);
    FConnectionStarted := 0;
    FHandle := nil;
    if FPlink <> nil then
      FPlink.Free;
    Log(lcInfo, f_(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
  end;

end;


procedure TAdoDBConnection.SetActive(Value: Boolean);
var
  tmpdb, Error, NetLib, DataSource, QuotedPassword, ServerVersion: String;
  rx: TRegExpr;
  i: Integer;
  IsOldProvider: Boolean;
begin
  if Value then begin
    DoBeforeConnect;
    try
      // Creating the ADO object throws exceptions if MDAC is missing, especially on Wine
      FAdoHandle := TAdoConnection.Create(Owner);
    except
      on E:Exception do
        raise EDbError.Create(E.Message+CRLF+CRLF+
            _('On Wine, you can try to install MDAC:')+CRLF+
            '> wget http://winetricks.org/winetricks'+CRLF+
            '> chmod +x winetricks'+CRLF+
            '> sh winetricks mdac28'+CRLF+
            '> sh winetricks native_mdac');
    end;

    IsOldProvider := Parameters.LibraryOrProvider = 'SQLOLEDB';
    if IsOldProvider then begin
      MessageDialog(
        f_('Security issue: Using %s %s with insecure %s.',
          [Parameters.LibraryOrProvider, 'ADO provider', 'TLS 1.0']) +
        f_('You should install %s from %s',
          ['Microsoft OLE DB Driver', 'https://www.microsoft.com/en-us/download/confirmation.aspx?id=56730']),
        mtWarning, [mbOK]);
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
    if (Parameters.NetType = ntMSSQL_TCPIP) and (Parameters.Port <> 0) then
      DataSource := DataSource + ','+IntToStr(Parameters.Port);

    // Quote password, just in case there is a semicolon or a double quote in it.
    // See http://forums.asp.net/t/1957484.aspx?Passwords+ending+with+semi+colon+as+the+terminal+element+in+connection+strings+
    if Pos('"', Parameters.Password) > 0 then
      QuotedPassword := ''''+Parameters.Password+''''
    else
      QuotedPassword := '"'+Parameters.Password+'"';

    FAdoHandle.ConnectionString := 'Provider='+Parameters.LibraryOrProvider+';'+
      'Password='+QuotedPassword+';'+
      'Persist Security Info=True;'+
      'User ID='+Parameters.Username+';'+
      'Network Library='+NetLib+';'+
      'Data Source='+DataSource+';'+
      'Application Name='+AppName+';'
      ;
    if Parameters.LibraryOrProvider = 'MSOLEDBSQL' then begin
      // Issue #423: MSOLEDBSQL compatibility with new column types
      // See https://docs.microsoft.com/en-us/sql/connect/oledb/applications/using-ado-with-oledb-driver-for-sql-server?view=sql-server-2017
      // Do not use with old driver, see https://www.heidisql.com/forum.php?t=35208
      FAdoHandle.ConnectionString := FAdoHandle.ConnectionString +
        'DataTypeCompatibility=80;';
    end;

    // Pass Database setting to connection string. Required on MS Azure?
    if (not Parameters.AllDatabasesStr.IsEmpty) and (Pos(';', Parameters.AllDatabasesStr)=0) then
      FAdoHandle.ConnectionString := FAdoHandle.ConnectionString + 'Database='+Parameters.AllDatabasesStr+';';

    if Parameters.WindowsAuth then begin
      if IsOldProvider then
        FAdoHandle.ConnectionString := FAdoHandle.ConnectionString + 'Integrated Security=SSPI;'
      else
        FAdoHandle.ConnectionString := FAdoHandle.ConnectionString + 'Trusted_Connection=yes;'
    end;

    try
      FAdoHandle.Connected := True;
      FConnectionStarted := GetTickCount div 1000;
      FActive := True;
      Log(lcInfo, f_('Connected. Thread-ID: %d', [ThreadId]));
      // No need to set a charset for MS SQL
      // CharacterSet := 'utf8';
      // CurCharset := CharacterSet;
      // Log(lcDebug, 'Characterset: '+CurCharset);
      FIsUnicode := True;
      FAdoHandle.CommandTimeout := Parameters.QueryTimeout;
      try
        // Gracefully accept failure on MS Azure (SQL Server 11), which does not have a sysprocesses table
        FServerUptime := StrToIntDef(GetVar('SELECT DATEDIFF(SECOND, '+QuoteIdent('login_time')+', CURRENT_TIMESTAMP) FROM '+QuoteIdent('master')+'.'+QuoteIdent('dbo')+'.'+QuoteIdent('sysprocesses')+' WHERE '+QuoteIdent('spid')+'=1'), -1);
      except
        FServerUptime := -1;
      end;
      FServerDateTimeOnStartup := GetVar('SELECT GETDATE()');
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
      try
        // Try to get more exact server version to avoid displaying "20.14" in some cases
        ServerVersion := GetVar('SELECT SERVERPROPERTY('+EscapeString('ProductVersion')+')');
        if ExecRegExpr('(\d+)\.(\d+)\.(\d+)\.(\d+)', ServerVersion) then
          FServerVersionUntouched := Copy(FServerVersionUntouched, 1, Pos(' - ', FServerVersionUntouched)+2) + ServerVersion;
      except
        // Above query only works on SQL Server 2008 and newer
        // Keep value from SELECT @@VERSION on older servers
      end;
      rx.Free;
      // See http://www.heidisql.com/forum.php?t=19779
      Query('SET TEXTSIZE 2147483647');
      FRealHostname := Parameters.Hostname;

      // Show up dynamic connection properties, probably useful for debugging
      for i:=0 to FAdoHandle.Properties.Count-1 do
        Log(lcDebug, f_('OLE DB property "%s": %s', [FAdoHandle.Properties[i].Name, String(FAdoHandle.Properties[i].Value)]));

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
      on E:Exception do begin
        FLastError := E.Message;
        Error := LastErrorMsg;
        Log(lcError, Error);
        FConnectionStarted := 0;
        raise EDbError.Create(Error);
      end;
    end;
  end else begin
    FAdoHandle.Connected := False;
    FActive := False;
    ClearCache(False);
    FConnectionStarted := 0;
    Log(lcInfo, f_(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
  end;
end;


procedure TPgConnection.SetActive(Value: Boolean);
var
  dbname, ConnInfo, Error, tmpdb: String;
  FinalHost: String;
  FinalPort: Integer;

  function EscapeConnectOption(Option: String): String;
  begin // See issue #704
    Result := StringReplace(Option, '\', '\\', [rfReplaceAll]);
  end;
begin
  if Value then begin
    DoBeforeConnect;
    // Simon Riggs:
    // "You should connect as "postgres" database by default, with an option to change. Don't use template1"
    dbname := FParameters.AllDatabasesStr;
    if dbname = '' then
      dbname := 'postgres';

    // Prepare special stuff for SSH tunnel
    FinalHost := FParameters.Hostname;
    FinalPort := FParameters.Port;

    case FParameters.NetType of
      ntPgSQL_SSHtunnel: begin
        // Create plink.exe process
        FPlink := TPlink.Create(Self);
        FPlink.Connect;
        FinalHost := '127.0.0.1';
        FinalPort := FParameters.SSHLocalPort;
      end;
    end;

    ConnInfo := 'host='''+FinalHost+''' '+
      'port='''+IntToStr(FinalPort)+''' '+
      'user='''+FParameters.Username+''' ' +
      'password='''+FParameters.Password+''' '+
      'dbname='''+dbname+''' '+
      'application_name='''+APPNAME+'''';
    if FParameters.WantSSL then begin
      ConnInfo := ConnInfo + ' sslmode=''require''';
      if FParameters.SSLPrivateKey <> '' then
        ConnInfo := ConnInfo + ' sslkey='''+EscapeConnectOption(FParameters.SSLPrivateKey)+'''';
      if FParameters.SSLCertificate <> '' then
        ConnInfo := ConnInfo + ' sslcert='''+EscapeConnectOption(FParameters.SSLCertificate)+'''';
      if FParameters.SSLCACertificate <> '' then
        ConnInfo := ConnInfo + ' sslrootcert='''+EscapeConnectOption(FParameters.SSLCACertificate)+'''';
      //if FParameters.SSLCipher <> '' then ??
    end;


    FHandle := FLib.PQconnectdb(PAnsiChar(AnsiString(ConnInfo)));
    if FLib.PQstatus(FHandle) = CONNECTION_BAD then begin
      Error := LastErrorMsg;
      Log(lcError, Error);
      FConnectionStarted := 0;
      try
        FLib.PQfinish(FHandle); // free the memory
      except
        on E:EAccessViolation do;
      end;
      FHandle := nil;
      if FPlink <> nil then
        FPlink.Free;
      raise EDbError.Create(Error);
    end;
    FActive := True;
    FServerDateTimeOnStartup := GetVar('SELECT NOW()');
    FServerVersionUntouched := GetVar('SELECT VERSION()');
    FConnectionStarted := GetTickCount div 1000;
    Log(lcInfo, f_('Connected. Thread-ID: %d', [ThreadId]));
    FIsUnicode := True;
    Query('SET statement_timeout TO '+IntToStr(Parameters.QueryTimeout*1000));
    try
      FServerUptime := StrToIntDef(GetVar('SELECT EXTRACT(EPOCH FROM CURRENT_TIMESTAMP - pg_postmaster_start_time())::INTEGER'), -1);
    except
      FServerUptime := -1;
    end;
    try
      FIsSSL := LowerCase(GetVar('SHOW ssl')) = 'on';
    except
      FIsSSL := False;
    end;

    DoAfterConnect;

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
  end else begin
    try
      FLib.PQfinish(FHandle);
    except
      on E:EAccessViolation do;
    end;
    FActive := False;
    ClearCache(False);
    FConnectionStarted := 0;
    if FPlink <> nil then
      FPlink.Free;
    Log(lcInfo, f_(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
  end;
end;


procedure TSQLiteConnection.SetActive(Value: Boolean);
var
  ConnectResult: Integer;
  tmpdb: String;
begin
  if Value then begin
    DoBeforeConnect;
    ConnectResult := FLib.sqlite3_open(
      PAnsiChar(Utf8Encode(Parameters.Hostname)),
      FHandle);

    if ConnectResult = SQLITE_OK then begin
      FActive := True;
      FServerDateTimeOnStartup := GetVar('SELECT DATETIME()');
      FServerVersionUntouched := GetVar('SELECT sqlite_version()');
      FConnectionStarted := GetTickCount div 1000;
      Log(lcInfo, f_('Connected. Thread-ID: %d', [ThreadId]));
      FIsUnicode := True;
      FServerUptime := -1;

      DoAfterConnect;

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
    end else begin
      Log(lcError, LastErrorMsg);
      FConnectionStarted := 0;
      FHandle := nil;
      raise EDbError.Create(LastErrorMsg);
    end;
  end else begin
    if FHandle <> nil then begin
      ClearCache(False);
      FLib.sqlite3_close(FHandle);
      FHandle := nil;
      FActive := False;
      Log(lcInfo, f_(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
    end;
  end;
end;


procedure TDBConnection.DoBeforeConnect;
var
  UsingPass: String;
  Dialog: TfrmLogin;
begin
  // Prompt for password on initial connect
  if FParameters.LoginPrompt and (not FLoginPromptDone) then begin
    Dialog := TfrmLogin.Create(Self);
    Dialog.Caption := APPNAME + ' - ' + FParameters.SessionName;
    Dialog.lblPrompt.Caption := f_('Login to %s:', [FParameters.Hostname]);
    Dialog.editUsername.Text := FParameters.Username;
    Dialog.editPassword.Text := FParameters.Password;
    Dialog.ShowModal;
    FParameters.Username := Dialog.editUsername.Text;
    FParameters.Password := Dialog.editPassword.Text;
    Dialog.Free;
    FLoginPromptDone := True;
  end;

  // Prepare connection
  if FParameters.Password <> '' then UsingPass := 'Yes' else UsingPass := 'No';
  Log(lcInfo, f_('Connecting to %s via %s, username %s, using password: %s ...',
    [FParameters.Hostname, FParameters.NetTypeName(True), FParameters.Username, UsingPass]
    ));

  case Parameters.NetTypeGroup of
    ngMySQL: begin
      FSQLSpecifities[spEmptyTable] := 'TRUNCATE ';
      FSQLSpecifities[spRenameTable] := 'RENAME TABLE %s TO %s';
      FSQLSpecifities[spRenameView] := FSQLSpecifities[spRenameTable];
      FSQLSpecifities[spCurrentUserHost] := 'SELECT CURRENT_USER()';
      FSQLSpecifities[spAddColumn] := 'ADD COLUMN %s';
      FSQLSpecifities[spChangeColumn] := 'CHANGE COLUMN %s %s';
      FSQLSpecifities[spSessionVariables] := 'SHOW VARIABLES';
      FSQLSpecifities[spGlobalVariables] := 'SHOW GLOBAL VARIABLES';
      FSQLSpecifities[spISTableSchemaCol] := 'TABLE_SCHEMA';
      FSQLSpecifities[spUSEQuery] := 'USE %s';
      FSQLSpecifities[spKillQuery] := 'KILL %d';
      FSQLSpecifities[spKillProcess] := 'KILL %d';
      FSQLSpecifities[spFuncLength] := 'LENGTH';
      FSQLSpecifities[spFuncCeil] := 'CEIL';
      FSQLSpecifities[spFuncLeft] := 'LEFT(%s, %d)';
      FSQLSpecifities[spLockedTables] := '';
    end;
    ngMSSQL: begin
      FSQLSpecifities[spEmptyTable] := 'DELETE FROM ';
      FSQLSpecifities[spRenameTable] := 'EXEC sp_rename %s, %s';
      FSQLSpecifities[spRenameView] := FSQLSpecifities[spRenameTable];
      FSQLSpecifities[spCurrentUserHost] := 'SELECT SYSTEM_USER';
      FSQLSpecifities[spAddColumn] := 'ADD %s';
      FSQLSpecifities[spChangeColumn] := 'ALTER COLUMN %s %s';
      FSQLSpecifities[spSessionVariables] := 'SELECT '+QuoteIdent('comment')+', '+QuoteIdent('value')+' FROM '+QuoteIdent('master')+'.'+QuoteIdent('dbo')+'.'+QuoteIdent('syscurconfigs')+' ORDER BY '+QuoteIdent('comment');
      FSQLSpecifities[spGlobalVariables] := FSQLSpecifities[spSessionVariables];
      FSQLSpecifities[spISTableSchemaCol] := 'TABLE_CATALOG';
      FSQLSpecifities[spUSEQuery] := 'USE %s';
      FSQLSpecifities[spKillQuery] := 'KILL %d';
      FSQLSpecifities[spKillProcess] := 'KILL %d';
      FSQLSpecifities[spFuncLength] := 'LEN';
      FSQLSpecifities[spFuncCeil] := 'CEILING';
      FSQLSpecifities[spFuncLeft] := 'LEFT(%s, %d)';
      FSQLSpecifities[spLockedTables] := '';
    end;
    ngPgSQL: begin
      FSQLSpecifities[spEmptyTable] := 'DELETE FROM ';
      FSQLSpecifities[spRenameTable] := 'ALTER TABLE %s RENAME TO %s';
      FSQLSpecifities[spRenameView] := 'ALTER VIEW %s RENAME TO %s';
      FSQLSpecifities[spCurrentUserHost] := 'SELECT CURRENT_USER';
      FSQLSpecifities[spAddColumn] := 'ADD %s';
      FSQLSpecifities[spChangeColumn] := 'ALTER COLUMN %s %s';
      FSQLSpecifities[spSessionVariables] := 'SHOW ALL';
      FSQLSpecifities[spGlobalVariables] := FSQLSpecifities[spSessionVariables];
      FSQLSpecifities[spISTableSchemaCol] := 'table_schema';
      FSQLSpecifities[spUSEQuery] := 'SET search_path TO %s';
      FSQLSpecifities[spKillQuery] := 'SELECT pg_cancel_backend(%d)';
      FSQLSpecifities[spKillProcess] := 'SELECT pg_cancel_backend(%d)';
      FSQLSpecifities[spFuncLength] := 'LENGTH';
      FSQLSpecifities[spFuncCeil] := 'CEIL';
      FSQLSpecifities[spFuncLeft] := 'LEFT(%s, %d)';
      FSQLSpecifities[spLockedTables] := '';
    end;
    ngSQLite: begin
      FSQLSpecifities[spEmptyTable] := 'TRUNCATE ';
      FSQLSpecifities[spRenameTable] := 'RENAME TABLE %s TO %s';
      FSQLSpecifities[spRenameView] := FSQLSpecifities[spRenameTable];
      FSQLSpecifities[spCurrentUserHost] := 'SELECT CURRENT_USER()';
      FSQLSpecifities[spAddColumn] := 'ADD COLUMN %s';
      FSQLSpecifities[spChangeColumn] := 'CHANGE COLUMN %s %s';
      FSQLSpecifities[spSessionVariables] := 'SELECT null, null'; // Todo: combine "PRAGMA pragma_list" + "PRAGMA a; PRAGMY b; ..."?
      FSQLSpecifities[spGlobalVariables] := 'SHOW GLOBAL VARIABLES';
      FSQLSpecifities[spISTableSchemaCol] := 'TABLE_SCHEMA';
      FSQLSpecifities[spUSEQuery] := '-- USE %s neither supported nor required'; // Cannot be empty without causing problems
      FSQLSpecifities[spKillQuery] := 'KILL %d';
      FSQLSpecifities[spKillProcess] := 'KILL %d';
      FSQLSpecifities[spFuncLength] := 'LENGTH';
      FSQLSpecifities[spFuncCeil] := 'CEIL';
      FSQLSpecifities[spFuncLeft] := 'SUBSTR(%s, 1, %d)';
      FSQLSpecifities[spLockedTables] := '';
    end;

  end;

end;


procedure TMySQLConnection.DoBeforeConnect;
var
  LibraryPath: String;
begin
  // Init libmysql before actually connecting.
  LibraryPath := ExtractFilePath(ParamStr(0)) + Parameters.LibraryOrProvider;
  Log(lcDebug, f_('Loading library file %s ...', [LibraryPath]));
  // Throws EDbError on any failure:
  FLib := TMySQLLib.Create(LibraryPath);
  Log(lcDebug, FLib.DllFile + ' v' + DecodeApiString(FLib.mysql_get_client_info) + ' loaded.');
  inherited;
end;


procedure TPgConnection.DoBeforeConnect;
var
  LibraryPath,
  msg: String;
begin
  // Init lib before actually connecting.
  LibraryPath := ExtractFilePath(ParamStr(0)) + Parameters.LibraryOrProvider;
  Log(lcDebug, f_('Loading library file %s ...', [LibraryPath]));
  try
    FLib := TPostgreSQLLib.Create(LibraryPath);
    Log(lcDebug, FLib.DllFile + ' v' + IntToStr(FLib.PQlibVersion) + ' loaded.');
  except
    on E:EDbError do begin
      // Try to explain what may cause this error
      msg := E.Message;
      if E.ErrorCode = TDbLib.LIB_PROC_ERROR then begin
        msg := msg + sLineBreak + sLineBreak +
          f_('Your %s is incompatible to %s, or your system is missing a dependent library.',
            [Parameters.LibraryOrProvider, APPNAME]);
      end;
      // In any case:
      msg := msg + sLineBreak + sLineBreak +
        f_('Installing %s might help. Please download from %s',
          ['VC Redistributable', 'https://support.microsoft.com/en-us/help/3179560/update-for-visual-c-2013-and-visual-c-redistributable-package']
          );
      raise EDbError.Create(msg, E.ErrorCode);
    end;
  end;
  inherited;
end;


procedure TSQLiteConnection.DoBeforeConnect;
var
  LibraryPath: String;
begin
  // Init lib before actually connecting.
  LibraryPath := ExtractFilePath(ParamStr(0)) + Parameters.LibraryOrProvider;
  Log(lcDebug, f_('Loading library file %s ...', [LibraryPath]));
  // Throws EDbError on any failure:
  FLib := TSQLiteLib.Create(LibraryPath);
  Log(lcDebug, FLib.DllFile + ' v' + ServerVersionUntouched + ' loaded.');
  inherited;
end;


procedure TDBConnection.DoAfterConnect;
begin
  AppSettings.SessionPath := FParameters.SessionPath;
  AppSettings.WriteString(asServerVersionFull, FServerVersionUntouched);
  FParameters.ServerVersion := FServerVersionUntouched;
  if Assigned(FOnConnected) then
    FOnConnected(Self, FDatabase);
  if FParameters.KeepAlive > 0 then begin
    FKeepAliveTimer.Interval := FParameters.KeepAlive * 1000;
    FKeepAliveTimer.OnTimer := KeepAliveTimerEvent;
  end;
end;


procedure TMySQLConnection.DoAfterConnect;
var
  TZI: TTimeZoneInformation;
  Minutes, Hours, i: Integer;
  Offset: String;
begin
  inherited;

  // Set timezone offset to UTC
  if (ServerVersionInt >= 40103) and Parameters.LocalTimeZone then begin
    Minutes := 0;
    case GetTimeZoneInformation(TZI) of
      TIME_ZONE_ID_STANDARD: Minutes := (TZI.Bias + TZI.StandardBias);
      TIME_ZONE_ID_DAYLIGHT: Minutes := (TZI.Bias + TZI.DaylightBias);
      TIME_ZONE_ID_UNKNOWN: Minutes := TZI.Bias;
      else RaiseLastOSError;
    end;
    Hours := Minutes div 60;
    Minutes := Minutes mod 60;
    if Hours < 0 then
      Offset := '+'
    else
      Offset := '-';
    Offset := Offset + Format('%.2d:%.2d', [Abs(Hours), Abs(Minutes)]);
    Query('SET time_zone='+EscapeString(Offset));
  end;

  // Support microseconds in some temporal datatypes of MariaDB 5.3+ and MySQL 5.6
  if ((ServerVersionInt >= 50300) and Parameters.IsMariaDB) or
    ((ServerVersionInt >= 50604) and (not Parameters.IsMariaDB)) then begin
    for i:=Low(FDatatypes) to High(FDatatypes) do begin
      if FDatatypes[i].Index in [dtDatetime, dtDatetime2, dtTime, dtTimestamp] then
        FDatatypes[i].HasLength := True;
    end;
  end;

  if ServerVersionInt >= 50000 then
    FSQLSpecifities[spKillQuery] := 'KILL QUERY %d';

  if ServerVersionInt >= 50124 then
    FSQLSpecifities[spLockedTables] := 'SHOW OPEN TABLES FROM %s WHERE '+QuoteIdent('in_use')+'!=0';
end;


procedure TAdoDBConnection.DoAfterConnect;
begin
  inherited;
  // See http://sqlserverbuilds.blogspot.de/
  case ServerVersionInt of
    0..899: begin
      FSQLSpecifities[spDatabaseTable] := QuoteIdent('master')+'..'+QuoteIdent('sysdatabases');
      FSQLSpecifities[spDatabaseTableId] := QuoteIdent('dbid');
      FSQLSpecifities[spDbObjectsTable] := '..'+QuoteIdent('sysobjects');
      FSQLSpecifities[spDbObjectsCreateCol] := 'crdate';
      FSQLSpecifities[spDbObjectsUpdateCol] := '';
      FSQLSpecifities[spDbObjectsTypeCol] := 'xtype';
    end;
    else begin
      FSQLSpecifities[spDatabaseTable] := QuoteIdent('sys')+'.'+QuoteIdent('databases');
      FSQLSpecifities[spDatabaseTableId] := QuoteIdent('database_id');
      FSQLSpecifities[spDbObjectsTable] := '.'+QuoteIdent('sys')+'.'+QuoteIdent('objects');
      FSQLSpecifities[spDbObjectsCreateCol] := 'create_date';
      FSQLSpecifities[spDbObjectsUpdateCol] := 'modify_date';
      FSQLSpecifities[spDbObjectsTypeCol] := 'type';
    end;
  end;
end;


function TMySQLConnection.Ping(Reconnect: Boolean): Boolean;
var
  IsDead: Boolean;
begin
  Log(lcDebug, 'Ping server ...');
  IsDead := True;
  try
    IsDead := (FHandle=nil) or (FLib.mysql_ping(FHandle) <> 0);
  except
    // silence dumb exceptions from mysql_ping
    on E:Exception do
      Log(lcError, E.Message);
  end;

  if IsDead then begin
    // Be sure to release some stuff before reconnecting
    Active := False;
    if Reconnect then
      Active := True;
  end;
  Result := FActive;
  // Restart keep-alive timer
  FKeepAliveTimer.Enabled := False;
  FKeepAliveTimer.Enabled := True;
end;


function TAdoDBConnection.Ping(Reconnect: Boolean): Boolean;
begin
  Log(lcDebug, 'Ping server ...');
  if FActive then try
    FAdoHandle.Execute('SELECT 1');
  except
    on E:EOleException do begin
      FLastError := E.Message;
      Log(lcError, E.Message);
      Active := False;
      if Reconnect then
        Active := True;
    end;
  end;
  Result := FActive;
  // Restart keep-alive timer
  FKeepAliveTimer.Enabled := False;
  FKeepAliveTimer.Enabled := True;
end;


function TPGConnection.Ping(Reconnect: Boolean): Boolean;
var
  PingResult: PPGResult;
  IsBroken: Boolean;
  PingStatus: Integer;
begin
  Log(lcDebug, 'Ping server ...');
  if FActive then begin
    IsBroken := FHandle = nil;
    if not IsBroken then begin
      PingStatus := FLib.PQsendQuery(FHandle, PAnsiChar(''));
      IsBroken := PingStatus <> 1;
      PingResult := FLib.PQgetResult(FHandle);
      while PingResult <> nil do begin
        FLib.PQclear(PingResult);
        PingResult := FLib.PQgetResult(FHandle);
      end;
    end;

    if IsBroken then begin
      // Be sure to release some stuff before reconnecting
      Active := False;
      if Reconnect then
        Active := True;
    end;
  end;
  Result := FActive;
  // Restart keep-alive timer
  FKeepAliveTimer.Enabled := False;
  FKeepAliveTimer.Enabled := True;
end;


function TSQLiteConnection.Ping(Reconnect: Boolean): Boolean;
begin
  Log(lcDebug, 'Ping server ...');
  if FActive then try
    FLib.sqlite3_exec(FHandle, nil, 0, nil, nil);
  except
    on E:Exception do begin
      Log(lcError, E.Message);
      Active := False;
      if Reconnect then
        Active := True;
    end;
  end;
  Result := FActive;
  // Restart keep-alive timer
  FKeepAliveTimer.Enabled := False;
  FKeepAliveTimer.Enabled := True;
end;


procedure TDBConnection.KeepAliveTimerEvent(Sender: TObject);
begin
  // Ping server in intervals, without automatically reconnecting
  if Active and (FLockedByThread = nil) then
    Ping(False);
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
    Log(lcDebug, _('Waiting for running query to finish ...'));
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
  FStatementNum := 1;
  QueryStatus := FLib.mysql_real_query(FHandle, PAnsiChar(NativeSQL), Length(NativeSQL));
  FLastQueryDuration := GetTickCount - TimerStart;
  FLastQueryNetworkDuration := 0;
  if QueryStatus <> 0 then begin
    // Most errors will show up here, some others slightly later, after mysql_store_result()
    Log(lcError, GetLastErrorMsg);
    raise EDbError.Create(GetLastErrorMsg, GetLastErrorCode);
  end else begin
    // We must call mysql_store_result() + mysql_free_result() to unblock the connection
    // See: http://dev.mysql.com/doc/refman/5.0/en/mysql-store-result.html
    FRowsAffected := 0;
    FWarningCount := FLib.mysql_warning_count(FHandle);
    FRowsFound := 0;
    TimerStart := GetTickCount;
    QueryResult := FLib.mysql_store_result(FHandle);
    FLastQueryNetworkDuration := GetTickCount - TimerStart;

    if (QueryResult = nil) and (FLib.mysql_affected_rows(FHandle) = -1) then begin
      // Indicates a late error, e.g. triggered by mysql_store_result(), after selecting a stored
      // function with invalid SQL body. Also SHOW TABLE STATUS on older servers.
      // See http://dev.mysql.com/doc/refman/5.0/en/mysql-affected-rows.html
      //   "An integer greater than zero indicates the number of rows affected or
      //   retrieved. Zero indicates that no records were updated for an UPDATE statement, no rows
      //   matched the WHERE clause in the query or that no query has yet been executed. -1
      //   indicates that the query returned an error or that, for a SELECT query,
      //   mysql_affected_rows() was called prior to calling mysql_store_result()."
      Log(lcError, GetLastErrorMsg);
      raise EDbError.Create(GetLastErrorMsg);
    end;

    if QueryResult = nil then
      DetectUSEQuery(SQL);

    while QueryStatus=0 do begin
      if QueryResult <> nil then begin
        // Statement returned a result set
        Inc(FRowsFound, FLib.mysql_num_rows(QueryResult));
        if DoStoreResult then begin
          SetLength(FLastRawResults, Length(FLastRawResults)+1);
          FLastRawResults[Length(FLastRawResults)-1] := QueryResult;
        end else begin
          FLib.mysql_free_result(QueryResult);
        end;
      end else begin
        // No result, but probably affected rows
        Inc(FRowsAffected, FLib.mysql_affected_rows(FHandle));
      end;
      // more results? -1 = no, >0 = error, 0 = yes (keep looping)
      Inc(FStatementNum);
      QueryStatus := FLib.mysql_next_result(FHandle);
      if QueryStatus = 0 then
        QueryResult := FLib.mysql_store_result(FHandle)
      else if QueryStatus > 0 then begin
        // MySQL stops executing a multi-query when an error occurs. So do we here by raising an exception.
        SetLength(FLastRawResults, 0);
        Log(lcError, GetLastErrorMsg);
        raise EDbError.Create(GetLastErrorMsg);
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
    Log(lcDebug, _('Waiting for running query to finish ...'));
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

    DetectUSEQuery(SQL);
  except
    on E:EOleException do begin
      FLastError := E.Message;
      Log(lcError, GetLastErrorMsg);
      raise EDbError.Create(GetLastErrorMsg);
    end;
  end;
end;


procedure TPGConnection.Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL);
var
  TimerStart: Cardinal;
  QueryResult: PPGresult;
  QueryStatus: Integer;
  NativeSQL: AnsiString;
begin
  if (FLockedByThread <> nil) and (FLockedByThread.ThreadID <> GetCurrentThreadID) then begin
    Log(lcDebug, _('Waiting for running query to finish ...'));
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
  FRowsFound := 0;
  FRowsAffected := 0;
  FWarningCount := 0;

  QueryStatus := FLib.PQsendQuery(FHandle, PAnsiChar(NativeSQL));

  FLastQueryDuration := GetTickCount - TimerStart;
  FLastQueryNetworkDuration := 0;
  if QueryStatus <> 1 then begin
    Log(lcError, GetLastErrorMsg);
    raise EDbError.Create(GetLastErrorMsg);
  end else begin
    FRowsAffected := 0;
    FRowsFound := 0;
    TimerStart := GetTickCount;
    QueryResult := FLib.PQgetResult(FHandle);
    FLastQueryNetworkDuration := GetTickCount - TimerStart;

    DetectUSEQuery(SQL);

    while QueryResult <> nil do begin
      if FLib.PQnfields(QueryResult) > 0 then begin
        // Statement returned a result set
        Inc(FRowsFound, FLib.PQntuples(QueryResult));
        if DoStoreResult then begin
          SetLength(FLastRawResults, Length(FLastRawResults)+1);
          FLastRawResults[Length(FLastRawResults)-1] := QueryResult;
        end else begin
          FLib.PQclear(QueryResult);
        end;
      end else begin
        Inc(FRowsAffected, StrToIntDef(String(FLib.PQcmdTuples(QueryResult)), 0));
      end;
      if LastErrorMsg <> '' then begin
        SetLength(FLastRawResults, 0);
        Log(lcError, GetLastErrorMsg);
        // Clear remaining results, to avoid "another command is already running"
        while QueryResult <> nil do begin
          FLib.PQclear(QueryResult);
          QueryResult := FLib.PQgetResult(FHandle);
        end;
        raise EDbError.Create(GetLastErrorMsg);
      end;
      // more results?
      QueryResult := FLib.PQgetResult(FHandle);
    end;

  end;

end;


procedure TSQLiteConnection.Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL);
var
  TimerStart, PrepareFlags: Cardinal;
  Rows: TSQLiteGridRows;
  Row: TGridRow;
  Value: TGridValue;
  QueryResult: Psqlite3_stmt;
  QueryStatus: Integer;
  i, OldRowsAffected: Integer;
  CurrentSQL, NextSQL: PAnsiChar;
begin
  if (FLockedByThread <> nil) and (FLockedByThread.ThreadID <> GetCurrentThreadID) then begin
    Log(lcDebug, _('Waiting for running query to finish ...'));
    try
      FLockedByThread.WaitFor;
    except
      on E:EThread do;
    end;
  end;

  Ping(True);
  Log(LogCategory, SQL);
  FLastQuerySQL := SQL;
  CurrentSQL := PAnsiChar(UTF8Encode(SQL));
  TimerStart := GetTickCount;
  SetLength(FLastRawResults, 0);
  FRowsFound := 0;
  FRowsAffected := 0;
  FWarningCount := 0;
  OldRowsAffected := FLib.sqlite3_total_changes(FHandle); // Temporary: substract these later from total num

  QueryResult := nil;
  NextSQL := nil;
  PrepareFlags := SQLITE_PREPARE_PERSISTENT;

  while True do begin
    QueryStatus := FLib.sqlite3_prepare_v3(FHandle, CurrentSQL, -1, PrepareFlags, QueryResult, NextSQL);
    FLastQueryDuration := GetTickCount - TimerStart;
    FLastQueryNetworkDuration := 0;

    if QueryStatus <> SQLITE_OK then begin
      Log(lcError, GetLastErrorMsg);
      raise EDbError.Create(GetLastErrorMsg);
    end;
    FRowsFound := 0;
    if DoStoreResult and (FLib.sqlite3_column_count(QueryResult) > 0) then begin
      Rows := TSQLiteGridRows.Create(Self);
      while FLib.sqlite3_step(QueryResult) = SQLITE_ROW do begin
        Row := TGridRow.Create;
        for i:=0 to FLib.sqlite3_column_count(QueryResult)-1 do begin
          Value := TGridValue.Create;
          Value.OldText := DecodeAPIString(FLib.sqlite3_column_text(QueryResult, i));
          Value.OldIsNull := FLib.sqlite3_column_text(QueryResult, i) = nil;
          Row.Add(Value);
        end;
        Rows.Add(Row);
      end;
      Inc(FRowsFound, Rows.Count);
      Rows.Statement := QueryResult;
      SetLength(FLastRawResults, Length(FLastRawResults)+1);
      FLastRawResults[Length(FLastRawResults)-1] := Rows;
    end else begin
      // Make one step through this non-result, otherwise SQLite does not seem to execute this query
      FLib.sqlite3_step(QueryResult);
      FLib.sqlite3_finalize(QueryResult);
    end;
    FRowsAffected := FLib.sqlite3_total_changes(FHandle) - OldRowsAffected;
    DetectUSEQuery(SQL);
    CurrentSQL := NextSQL;
    if Trim(CurrentSQL) = '' then
      Break;
  end;
  FLastQueryNetworkDuration := GetTickCount - TimerStart;
end;


function TDBConnection.GetLastResults: TDBQueryList;
var
  r: TDBQuery;
  i: Integer;
begin
  Result := TDBQueryList.Create(False);
  for i:=0 to ResultCount-1 do begin
    r := Parameters.CreateQuery(Self);
    r.SQL := FLastQuerySQL;
    r.Execute(False, i);
    Result.Add(r);
  end;
end;


function TAdoDBConnection.GetLastResults: TDBQueryList;
var
  r: TDBQuery;
  i: Integer;
  Batch: TSQLBatch;
begin
  Result := TDBQueryList.Create(False);
  Batch := TSQLBatch.Create;
  Batch.SQL := FLastQuerySQL;
  for i:=Low(FLastRawResults) to High(FLastRawResults) do begin
    r := Parameters.CreateQuery(Self);
    if Batch.Count > i then
      r.SQL := Batch[i].SQL
    else // See http://www.heidisql.com/forum.php?t=21036
      r.SQL := Batch.SQL;
    r.Execute(False, i);
    Result.Add(r);
  end;
  Batch.Free;
end;


function TMySQLConnection.GetCreateCode(Obj: TDBObject): String;
var
  ColIdx: Integer;
begin
  if Obj.NodeType = lntView then begin
    // Use our own baked CREATE VIEW code
    Result := GetCreateViewCode(Obj.Database, Obj.Name);
    Exit;
  end;
  case Obj.NodeType of
    lntTable: ColIdx := 1;
    lntFunction, lntProcedure, lntTrigger: ColIdx := 2;
    lntEvent: ColIdx := 3;
    else raise EDbError.CreateFmt(_('Unhandled list node type in %s.%s'), [ClassName, 'GetCreateCode']);
  end;
  Result := GetVar('SHOW CREATE '+Obj.ObjType.ToUpperInvariant+' '+QuoteIdent(Obj.Database)+'.'+QuoteIdent(Obj.Name), ColIdx);
end;


function TSQLiteConnection.GetCreateCode(Obj: TDBObject): String;
begin
  // PRAGMA table_info(customers):
  // cid name       type         notnull dflt_value pk
  // 0   CustomerId INTEGER      1       null       1
  // 1   FirstName  NVARCHAR(40) 1       null       0
  case Obj.NodeType of
    lntTable: begin
      Result := GetVar('SELECT '+QuoteIdent('sql')+' FROM sqlite_master'+
        ' WHERE '+QuoteIdent('type')+'='+EscapeString('table')+
        ' AND name='+EscapeString(Obj.Name));
    end;
    else begin
      // Let the generic method try to return code, which will most likely fail on SQLite
      Result := inherited;
    end;
  end;
end;


function TMySQLConnection.GetCreateViewCode(Database, Name: String): String;
var
  ViewIS: TDBQuery;
  Algorithm, CheckOption, SelectCode, Definer, SQLSecurity: String;
  AlternativeSelectCode: String;
  rx: TRegExpr;
  Obj: TDBObject;
begin
  // Get CREATE VIEW code, which can throw privilege errors and errors due to
  // references to renamed or deleted columns
  try
    Result := GetVar('SHOW CREATE VIEW '+QuoteIdent(Database)+'.'+QuoteIdent(Name), 1);
  except
    on E:EDbError do begin
      ViewIS := GetResults('SELECT * FROM '+InfSch+'.VIEWS WHERE '+
        'TABLE_SCHEMA='+EscapeString(Database)+' AND TABLE_NAME='+EscapeString(Name));
      Result := 'CREATE ';
      if ViewIS.Col('DEFINER') <> '' then
        Result := Result + 'DEFINER='+QuoteIdent(ViewIS.Col('DEFINER'), True, '@')+' ';
      Result := Result + 'VIEW '+QuoteIdent(Name)+' AS '+ViewIS.Col('VIEW_DEFINITION')+' ';
      if ViewIS.Col('CHECK_OPTION') <> 'NONE' then
        Result := Result + 'WITH '+Uppercase(ViewIS.Col('CHECK_OPTION'))+' CHECK OPTION';
    end;
  end;
  try
    // Try to fetch original VIEW code from .frm file
    AlternativeSelectCode := GetVar('SELECT CAST(LOAD_FILE('+
      'CONCAT('+
        'IFNULL(@@GLOBAL.datadir, CONCAT(@@GLOBAL.basedir, '+EscapeString('data/')+')), '+
        EscapeString(Database+'/'+Name+'.frm')+')'+
      ') AS CHAR CHARACTER SET utf8)');
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.ModifierG := False;
    rx.Expression := '\nsource\=(.+)\n\w+\=';
    if rx.Exec(AlternativeSelectCode) then begin
      // Put pieces of CREATE VIEW together
      Obj := FindObject(Database, Name);
      ParseViewStructure(Result, Obj, Algorithm, Definer, SQLSecurity, CheckOption, SelectCode);
      AlternativeSelectCode := UnescapeString(rx.Match[1]);
      Result := 'CREATE ';
      if Algorithm <> '' then
        Result := Result + 'ALGORITHM='+Uppercase(Algorithm)+' ';
      if Definer <> '' then
        Result := Result + 'DEFINER='+QuoteIdent(Definer, True, '@')+' ';
      if not SQLSecurity.IsEmpty then
        Result := Result + 'SQL SECURITY '+SQLSecurity+' ';
      Result := Result + 'VIEW '+Obj.QuotedName+' AS '+AlternativeSelectCode+' ';
      // WITH .. CHECK OPTION is already contained in the source
    end;
    rx.Free;
  except
    // Do not raise if that didn't work
    on E:EDbError do;
  end;
end;


function TDBConnection.GetCreateCode(Obj: TDBObject): String;
var
  ProcDetails: TDBQuery;
  DataType: String;
  ArgNames, ArgTypes, Arguments: TStringList;
  Rows: TStringList;
  i: Integer;
  TableCols: TTableColumnList;
  TableCol: TTableColumn;
  TableKeys: TTableKeyList;
  TableKey: TTableKey;
  TableForeignKeys: TForeignKeyList;
  TableForeignKey: TForeignKey;

  // Return fitting schema clause for queries in IS.TABLES, IS.ROUTINES etc.
  // TODO: Does not work on MSSQL 2000
  function SchemaClauseIS(Prefix: String): String;
  begin
    if Obj.Schema <> '' then
      Result := Prefix+'_SCHEMA='+EscapeString(Obj.Schema)
    else
      Result := Prefix+'_CATALOG='+EscapeString(Obj.Database);
  end;

begin
  case Obj.NodeType of
    lntTable: begin
      Result := 'CREATE TABLE '+QuoteIdent(Obj.Name)+' (';
      TableCols := Obj.GetTableColumns;
      for TableCol in TableCols do begin
        Result := Result + CRLF + #9 + TableCol.SQLCode + ',';
      end;
      TableCols.Free;

      TableKeys := Obj.GetTableKeys;
      for TableKey in TableKeys do begin
        Result := Result + CRLF + #9 + TableKey.SQLCode + ',';
      end;
      TableKeys.Free;

      TableForeignKeys := Obj.GetTableForeignKeys;
      for TableForeignKey in TableForeignKeys do begin
        Result := Result + CRLF + #9 + TableForeignKey.SQLCode(True) + ',';
      end;
      TableForeignKeys.Free;

      Delete(Result, Length(Result), 1);
      Result := Result + CRLF + ')';

    end;

    lntView: begin
      case FParameters.NetTypeGroup of
        ngPgSQL: begin
          // Prefer pg_catalog tables. See http://www.heidisql.com/forum.php?t=16213#p16685
          Result := 'CREATE VIEW ' + QuoteIdent(Obj.Name) + ' AS ' + GetVar('SELECT '+QuoteIdent('definition')+
            ' FROM '+QuoteIdent('pg_views')+
            ' WHERE '+QuoteIdent('viewname')+'='+EscapeString(Obj.Name)+
            ' AND '+QuoteIdent('schemaname')+'='+EscapeString(Obj.Schema)
            );
        end;
        ngMSSQL: begin
          // Overcome 4000 character limit in IS.VIEW_DEFINITION
          // See http://www.heidisql.com/forum.php?t=21097
          Result := GetVar('SELECT '+QuoteIdent('MODS')+'.'+QuoteIdent('DEFINITION')+
            ' FROM '+QuoteIdent('SYS')+'.'+QuoteIdent('OBJECTS')+' '+QuoteIdent('OBJ')+
            ' JOIN '+QuoteIdent('SYS')+'.'+QuoteIdent('SQL_MODULES')+' AS '+QuoteIdent('MODS')+' ON '+QuoteIdent('OBJ')+'.'+QuoteIdent('OBJECT_ID')+'='+QuoteIdent('MODS')+'.'+QuoteIdent('OBJECT_ID')+
            ' JOIN '+QuoteIdent('SYS')+'.'+QuoteIdent('SCHEMAS')+' AS '+QuoteIdent('SCHS')+' ON '+QuoteIdent('OBJ')+'.'+QuoteIdent('SCHEMA_ID')+'='+QuoteIdent('SCHS')+'.'+QuoteIdent('SCHEMA_ID')+
            ' WHERE '+QuoteIdent('OBJ')+'.'+QuoteIdent('TYPE')+'='+EscapeString('V')+
            '   AND '+QuoteIdent('SCHS')+'.'+QuoteIdent('NAME')+'='+EscapeString(Obj.Schema)+
            '   AND '+QuoteIdent('OBJ')+'.'+QuoteIdent('NAME')+'='+EscapeString(Obj.Name)
            );
        end;
        else begin
          Result := GetVar('SELECT VIEW_DEFINITION'+
            ' FROM '+InfSch+'.VIEWS'+
            ' WHERE TABLE_NAME='+EscapeString(Obj.Name)+
            ' AND '+SchemaClauseIS('TABLE')
            );
        end;
      end;
    end;

    lntFunction: begin
      case Parameters.NetTypeGroup of
        ngMSSQL: begin
          // Tested on MS SQL 8.0 and 11.0
          // See http://www.heidisql.com/forum.php?t=12495
          if not Obj.Schema.IsEmpty then
            Rows := GetCol('EXEC sp_helptext '+EscapeString(Obj.Schema+'.'+Obj.Name))
          else
            Rows := GetCol('EXEC sp_helptext '+EscapeString(Obj.Database+'.'+Obj.Name));
          // Do not use Rows.Text, as the rows already include a trailing linefeed
          Result := implodestr('', Rows);
          Rows.Free;
        end;
        ngPgSQL: begin
          Result := 'CREATE FUNCTION '+QuoteIdent(Obj.Name);
          ProcDetails := GetResults('SELECT '+
            QuoteIdent('p')+'.'+QuoteIdent('prosrc')+', '+
            QuoteIdent('p')+'.'+QuoteIdent('proargnames')+', '+
            QuoteIdent('p')+'.'+QuoteIdent('proargtypes')+', '+
            QuoteIdent('p')+'.'+QuoteIdent('prorettype')+' '+
            'FROM '+QuoteIdent('pg_catalog')+'.'+QuoteIdent('pg_namespace')+' AS '+QuoteIdent('n')+' '+
            'JOIN '+QuoteIdent('pg_catalog')+'.'+QuoteIdent('pg_proc')+' AS '+QuoteIdent('p')+' ON '+QuoteIdent('p')+'.'+QuoteIdent('pronamespace')+' = '+QuoteIdent('n')+'.'+QuoteIdent('oid')+' '+
            'WHERE '+
            QuoteIdent('n')+'.'+QuoteIdent('nspname')+'='+EscapeString(Obj.Database)+
            'AND '+QuoteIdent('p')+'.'+QuoteIdent('proname')+'='+EscapeString(Obj.Name)+
            'AND '+QuoteIdent('p')+'.'+QuoteIdent('proargtypes')+'='+EscapeString(Obj.ArgTypes)
            );
          ArgNames := Explode(',', Copy(ProcDetails.Col('proargnames'), 2, Length(ProcDetails.Col('proargnames'))-2));
          ArgTypes := Explode(' ', Copy(ProcDetails.Col('proargtypes'), 1, Length(ProcDetails.Col('proargtypes'))));
          Arguments := TStringList.Create;
          for i:=0 to ArgNames.Count-1 do begin
            if ArgTypes.Count > i then
              DataType := GetDatatypeByNativeType(MakeInt(ArgTypes[i]), ArgNames[i]).Name
            else
              DataType := '';
            Arguments.Add(ArgNames[i] + ' ' + DataType);
          end;
          Result := Result + '(' + implodestr(', ', Arguments) + ') '+
            'RETURNS '+GetDatatypeByNativeType(MakeInt(ProcDetails.Col('prorettype'))).Name+' '+
            'AS $$ '+ProcDetails.Col('prosrc')+' $$'
            // TODO: 'LANGUAGE SQL IMMUTABLE STRICT'
            ;
        end;
        else begin
          Result := GetVar('SELECT ROUTINE_DEFINITION'+
            ' FROM '+InfSch+'.ROUTINES'+
            ' WHERE ROUTINE_NAME='+EscapeString(Obj.Name)+
            ' AND ROUTINE_TYPE='+EscapeString('FUNCTION')+
            ' AND '+SchemaClauseIS('ROUTINE')
            );
        end;
      end;
    end;

    lntProcedure: begin
      case Parameters.NetTypeGroup of
        ngMSSQL: begin
          // See comments above
          if not Obj.Schema.IsEmpty then
            Rows := GetCol('EXEC sp_helptext '+EscapeString(Obj.Schema+'.'+Obj.Name))
          else
            Rows := GetCol('EXEC sp_helptext '+EscapeString(Obj.Database+'.'+Obj.Name));
          Result := implodestr('', Rows);
          Rows.Free;
        end;
        else begin
          Result := GetVar('SELECT ROUTINE_DEFINITION'+
            ' FROM '+InfSch+'.ROUTINES'+
            ' WHERE ROUTINE_NAME='+EscapeString(Obj.Name)+
            ' AND ROUTINE_TYPE='+EscapeString('PROCEDURE')+
            ' AND '+SchemaClauseIS('ROUTINE')
            );
        end;
      end;
    end;

  end;

end;


procedure TDBConnection.PrefetchCreateCode(Objects: TDBObjectList);
var
  Queries: TStringList;
  Obj: TDBObject;
  UseIt: Boolean;
begin
  // Cache some queries used in GetCreateCode for mass operations. See TMainForm.SynCompletionProposalExecute
  Queries := TStringList.Create;
  for Obj in Objects do begin
    case Parameters.NetTypeGroup of
      ngMySQL: begin
        UseIt := Obj.NodeType <> lntView;
        // SHOW CREATE TRIGGER was introduced in MySQL 5.1.21
        // See #111
        if Obj.NodeType = lntTrigger then
          UseIt := UseIt and (ServerVersionInt >= 50121);
        if UseIt then
          Queries.Add('SHOW CREATE '+UpperCase(Obj.ObjType)+' '+QuoteIdent(Obj.Database)+'.'+QuoteIdent(Obj.Name));
      end;
      ngMSSQL: begin
        if Obj.NodeType in [lntFunction, lntProcedure] then begin
          if not Obj.Schema.IsEmpty then
            Queries.Add('EXEC sp_helptext '+EscapeString(Obj.Schema+'.'+Obj.Name))
          else
            Queries.Add('EXEC sp_helptext '+EscapeString(Obj.Database+'.'+Obj.Name))
        end;
      end;
      else begin
        Log(lcDebug, 'No query logic for PrefetchCreateCode');
      end;
    end;
  end;
  if Queries.Count > 0 then try
    PrefetchResults(implodestr(';', Queries));
  except
    on E:EDbError do;
  end;

end;


{**
  Set "Database" property and select that db if connected
}
procedure TDBConnection.SetDatabase(Value: String);
var
  s: String;
begin
  Log(lcDebug, 'SetDatabase('+Value+'), FDatabase: '+FDatabase);
  if Value <> FDatabase then begin
    if Value = '' then begin
      FDatabase := Value;
      if Assigned(FOnDatabaseChanged) then
        FOnDatabaseChanged(Self, Value);
    end else begin
      if FParameters.NetTypeGroup = ngPgSQL then begin
        s := EscapeString(Value);
        // Get schema with the same name as user name in search path
        // See https://www.heidisql.com/forum.php?t=34558
        s := s + ', ' + EscapeString('$user');
        // Always keep public schema in search path, so one can use procedures from it without prefixing
        // See http://www.heidisql.com/forum.php?t=18581#p18905
        if Value <> 'public' then
          s := s + ', ' + EscapeString('public');
      end else
        s := QuoteIdent(Value);
      Query(Format(GetSQLSpecifity(spUSEQuery), [s]), False);
    end;
    if Assigned(FOnObjectnamesChanged) then
      FOnObjectnamesChanged(Self, FDatabase);
  end;
end;


procedure TDBConnection.DetectUSEQuery(SQL: String);
var
  rx: TRegExpr;
  Quotes: String;
begin
  // Detect query for switching current working database or schema
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.Expression := '^'+GetSQLSpecifity(spUSEQuery);
  Quotes := QuoteRegExprMetaChars(FQuoteChars+''';');
  rx.Expression := StringReplace(rx.Expression, ' ', '\s+', [rfReplaceAll]);
  rx.Expression := StringReplace(rx.Expression, '%s', '['+Quotes+']?([^'+Quotes+']+)['+Quotes+']*', [rfReplaceAll]);
  if rx.Exec(SQL) then begin
    FDatabase := Trim(rx.Match[1]);
    FDatabase := DeQuoteIdent(FDatabase);
    Log(lcDebug, f_('Database "%s" selected', [FDatabase]));
    if Assigned(FOnDatabaseChanged) then
      FOnDatabaseChanged(Self, Database);
  end;
  rx.Free;
end;


{**
  Return current thread id
}
function TMySQLConnection.GetThreadId: Int64;
begin
  if FThreadId = 0 then begin
    Ping(False);
    if FActive then
      FThreadID := StrToInt64Def(GetVar('SELECT CONNECTION_ID()'), 0);
  end;
  Result := FThreadID;
end;


function TAdoDBConnection.GetThreadId: Int64;
begin
  if FThreadId = 0 then begin
    Ping(False);
    if FActive then
      FThreadID := StrToInt64Def(GetVar('SELECT @@SPID'), 0);
  end;
  Result := FThreadID;
end;


function TPGConnection.GetThreadId: Int64;
begin
  if FThreadId = 0 then begin
    Ping(False);
    if FActive then
      FThreadID := FLib.PQbackendPID(FHandle);
  end;
  Result := FThreadID;
end;


function TSQLiteConnection.GetThreadId: Int64;
begin
  if FThreadId = 0 then begin
    Ping(False);
    if FActive then // We return the application process id, as there is no connection pid in SQLite
      FThreadID := Windows.GetCurrentProcessId;
  end;
  Result := FThreadID;
end;


{**
  Return currently used character set
}
function TDBConnection.GetCharacterSet: String;
begin
  Result := '';
end;


function TMySQLConnection.GetCharacterSet: String;
begin
  Result := inherited;
  Result := DecodeAPIString(FLib.mysql_character_set_name(FHandle));
end;


{**
  Switch character set
}
procedure TDBConnection.SetCharacterSet(CharsetName: String);
begin
  // Nothing to do by default
end;


procedure TMySQLConnection.SetCharacterSet(CharsetName: String);
var
  Return: Integer;
begin
  FStatementNum := 0;
  Return := FLib.mysql_set_character_set(FHandle, PAnsiChar(Utf8Encode(CharsetName)));
  if Return <> 0 then
    raise EDbError.Create(LastErrorMsg)
  else
    FIsUnicode := Pos('utf8', LowerCase(CharsetName)) = 1;
end;


procedure TPGConnection.SetCharacterSet(CharsetName: String);
begin
  // See issue #22
  Query('SET CLIENT_ENCODING TO ' + EscapeString('UTF8'));
end;


function TMySQLConnection.GetLastErrorCode: Cardinal;
begin
  Result := FLib.mysql_errno(FHandle);
end;


function TAdoDBConnection.GetLastErrorCode: Cardinal;
begin
  // SELECT @@SPID throws errors without filling the error pool. See issue #2684.
  if FAdoHandle.Errors.Count > 0 then
    Result := FAdoHandle.Errors[FAdoHandle.Errors.Count-1].NativeError
  else
    Result := 0;
end;


function TPgConnection.GetLastErrorCode: Cardinal;
begin
  Result := Cardinal(FLib.PQstatus(FHandle));
end;


function TSQLiteConnection.GetLastErrorCode: Cardinal;
begin
  Result := FLib.sqlite3_errcode(FHandle);
end;


{**
  Return the last error nicely formatted
}
function TMySQLConnection.GetLastErrorMsg: String;
var
  Msg, Additional: String;
  rx: TRegExpr;
begin
  Result := '';
  Additional := '';

  Msg := DecodeAPIString(FLib.mysql_error(FHandle));

  if SynRegExpr.ExecRegExpr('(Unknown SSL error|SSL connection error)', Msg) then begin
    // Find specific strings in error message and provide helpful message
    Additional := f_('Please select a different library in your session settings. (Current: "%s")', [FParameters.LibraryOrProvider]);
  end else begin
    // Find "(errno: 123)" in message and add more meaningful message from perror.exe
    rx := TRegExpr.Create;
    rx.Expression := '.+\(errno\:\s+(\d+)\)';
    if rx.Exec(Msg) then begin
      Additional := MySQLErrorCodes.Values[rx.Match[1]];
    end;
    rx.Free;
  end;

  if Additional <> '' then begin
    Msg := Msg + sLineBreak + sLineBreak + Additional;
  end;

  case FStatementNum of
    0: Result := Msg;
    1: Result := f_(MsgSQLError, [LastErrorCode, Msg]);
    else Result := f_(MsgSQLErrorMultiStatements, [LastErrorCode, FStatementNum, Msg]);
  end;
end;


function TAdoDBConnection.GetLastErrorMsg: String;
var
  Msg: String;
  rx: TRegExpr;
  E: Error;
begin
  if FAdoHandle.Errors.Count > 0 then begin
    E := FAdoHandle.Errors[FAdoHandle.Errors.Count-1];
    Msg := E.Description;
    // Remove stuff from driver in message "[DBNETLIB][ConnectionOpen (Connect()).]"
    rx := TRegExpr.Create;
    rx.Expression := '^\[DBNETLIB\]\[.*\](.+)$';
    if rx.Exec(Msg) then
      Msg := rx.Match[1];
    rx.Free;
  end else
    Msg := _('unknown');
  if (FLastError <> '') and (Pos(FLastError, Msg) = 0) then
    Msg := FLastError + CRLF + Msg;
  Result := f_(MsgSQLError, [LastErrorCode, Msg]);
end;


function TPgConnection.GetLastErrorMsg: String;
begin
  Result := DecodeAPIString(FLib.PQerrorMessage(FHandle));
  Result := Trim(Result);
end;


function TSQLiteConnection.GetLastErrorMsg: String;
begin
  Result := DecodeAPIString(FLib.sqlite3_errmsg(FHandle));
end;


{**
  Get version string as normalized integer
  "5.1.12-beta-community-123" => 50112
}
function TDBConnection.ServerVersionInt: Integer;
var
  rx: TRegExpr;
  v1, v2: String;
begin
  Result := 0;
  rx := TRegExpr.Create;
  case FParameters.NetTypeGroup of
    ngMySQL, ngPgSQL, ngSQLite: begin
      rx.Expression := '(\d+)\.(\d+)(\.(\d+))?';
      if rx.Exec(FServerVersionUntouched) then begin
        Result := StrToIntDef(rx.Match[1], 0) *10000 +
          StrToIntDef(rx.Match[2], 0) *100 +
          StrToIntDef(rx.Match[4], 0);
      end;
    end;
    ngMSSQL: begin
      // See http://support.microsoft.com/kb/321185
      // "Microsoft SQL Server 7.00 - 7.00.1094 (Intel X86)" ==> 700
      // "Microsoft SQL Server 2008 (RTM) - 10.0.1600.22 (Intel X86)" ==> 1000
      // "Microsoft SQL Server 2008 R2 (RTM) - 10.50.1600.1 (Intel X86)" ==> 1050
      rx.ModifierG := False;
      rx.Expression := '\s(\d+)\.(\d+)\D';
      if rx.Exec(FServerVersionUntouched) then begin
        v1 := rx.Match[1];
        v2 := rx.Match[2];
        Result := StrToIntDef(v1, 0) *100 +
          StrToIntDef(v2, 0);
      end else begin
        rx.Expression := '(\d+)[,\.](\d+)[,\.](\d+)[,\.](\d+)';
        if rx.Exec(FServerVersionUntouched) then begin
          Result := StrToIntDef(rx.Match[1], 0) *100 +
            StrToIntDef(rx.Match[2], 0);
        end;
      end;
    end;
    else begin
      raise EDbError.CreateFmt(_(MsgUnhandledNetType), [Integer(FParameters.NetType)]);
    end;
  end;
  rx.Free;
end;


function TDBConnection.ServerVersionStr: String;
var
  v: String;
  major, minor, build: Integer;
begin
  case FParameters.NetTypeGroup of
    ngMySQL, ngPgSQL, ngSQLite: begin
      v := IntToStr(ServerVersionInt);
      major := StrToIntDef(Copy(v, 1, Length(v)-4), 0);
      minor := StrToIntDef(Copy(v, Length(v)-3, 2), 0);
      build := StrToIntDef(Copy(v, Length(v)-1, 2), 0);
      Result := IntToStr(major) + '.' + IntToStr(minor) + '.' + IntToStr(build);
    end;
    ngMSSQL: begin
      major := ServerVersionInt div 100;
      minor := ServerVersionInt mod (ServerVersionInt div 100);
      Result := IntToStr(major) + '.' + IntToStr(minor);
    end;
    else begin
      raise EDbError.CreateFmt(_(MsgUnhandledNetType), [Integer(FParameters.NetType)]);
    end;
  end;
end;


function TDBConnection.NdbClusterVersionInt: Integer;
var
  rx: TRegExpr;
begin
  // 5.6.17-ndb-7.3.5
  Result := 0;
  rx := TRegExpr.Create;
  rx.Expression := '[\d+\.]+-ndb-(\d+)\.(\d+)\.(\d+)';
  if rx.Exec(FServerVersionUntouched) then begin
    Result := StrToIntDef(rx.Match[1], 0) *10000 +
      StrToIntDef(rx.Match[2], 0) *100 +
      StrToIntDef(rx.Match[3], 0);
  end;
  rx.Free;
end;


function TDBConnection.GetAllDatabases: TStringList;
var
  rx: TRegExpr;
  dbname: String;
begin
  // Get user passed delimited list
  if not Assigned(FAllDatabases) then begin
    if FParameters.AllDatabasesStr <> '' then begin
      FAllDatabases := TStringList.Create;
      rx := TRegExpr.Create;
      rx.Expression := '[^;]+';
      rx.ModifierG := True;
      if rx.Exec(FParameters.AllDatabasesStr) then while true do begin
        // Add if not a duplicate
        dbname := Trim(rx.Match[0]);
        if FAllDatabases.IndexOf(dbname) = -1 then
          FAllDatabases.Add(dbname);
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
    except on E:EDbError do
      try
        FAllDatabases := GetCol('SELECT '+QuoteIdent('SCHEMA_NAME')+' FROM '+QuoteIdent(InfSch)+'.'+QuoteIdent('SCHEMATA')+' ORDER BY '+QuoteIdent('SCHEMA_NAME'));
      except
        on E:EDbError do begin
          FAllDatabases := TStringList.Create;
          Log(lcError, f_('Database names not available due to missing privileges for user %s.', [CurrentUserHostCombination]));
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
      FAllDatabases := GetCol('SELECT '+QuoteIdent('name')+' FROM '+GetSQLSpecifity(spDatabaseTable)+' ORDER BY '+QuoteIdent('name'));
    except on E:EDbError do
      FAllDatabases := TStringList.Create;
    end;
    Result := FAllDatabases;
  end;
end;


function TPGConnection.GetAllDatabases: TStringList;
var
  DbQuery: String;
begin
  // In PostgreSQL, we display schemata, not databases.
  // The AllDatabasesStr is used to set the single database name
  if not Assigned(FAllDatabases) then begin
    try
      // Query is.schemata when using schemata, for databases use pg_database
      //FAllDatabases := GetCol('SELECT datname FROM pg_database WHERE datistemplate=FALSE');
      DbQuery := 'SELECT '+QuoteIdent('nspname')+
        ' FROM '+QuoteIdent('pg_catalog')+'.'+QuoteIdent('pg_namespace');
      if Parameters.IsRedshift then begin
        DbQuery := DbQuery + ' WHERE '+QuoteIdent('nspowner')+' != 1'+
          ' OR '+QuoteIdent('nspname')+' IN ('+EscapeString('pg_catalog')+', '+EscapeString(InfSch)+')';
      end;
      DbQuery := DbQuery + ' ORDER BY '+QuoteIdent('nspname');
      FAllDatabases := GetCol(DbQuery);
    except on E:EDbError do
      FAllDatabases := TStringList.Create;
    end;
  end;
  Result := FAllDatabases;
end;


function TSQLiteConnection.GetAllDatabases: TStringList;
begin
  Result := inherited;
  if not Assigned(Result) then begin
    FAllDatabases := TStringList.Create;
    FAllDatabases.Add('main');
    Result := FAllDatabases;
  end;
end;


function TDBConnection.RefreshAllDatabases: TStringList;
begin
  FreeAndNil(FAllDatabases);
  Result := AllDatabases;
end;


function TDBConnection.GetResults(SQL: String): TDBQuery;
var
  Query: TDBQuery;
begin
  Result := nil;

  // Look up query result in cache
  if Assigned(FPrefetchResults) then begin
    for Query in FPrefetchResults do begin
      if Query.SQL = SQL then begin
        Result := Query;
        Log(lcDebug, 'Using cached result for query: '+StrEllipsis(SQL, 100));
        Break;
      end;
    end;
  end;

  // Fire query
  if Result = nil then begin
    Result := Parameters.CreateQuery(Self);
    Result.SQL := SQL;
    try
      Result.Execute;
    except
      FreeAndNil(Result);
      Raise;
    end;
  end;
end;


procedure TDBConnection.PrefetchResults(SQL: String);
var
  LastResults: TDBQueryList;
  Batch: TSQLBatch;
  i: Integer;
begin
  Query(SQL, True);
  Batch := TSQLBatch.Create;
  Batch.SQL := SQL;
  FreeAndNil(FPrefetchResults);
  FPrefetchResults := TDBQueryList.Create(True);
  LastResults := GetLastResults;
  for i:=0 to LastResults.Count-1 do begin
    FPrefetchResults.Add(LastResults[i]);
    if Batch.Count > i then
      FPrefetchResults[i].SQL := Batch[i].SQL;
  end;
  Batch.Free;
end;


procedure TDBConnection.FreeResults(Results: TDBQuery);
begin
  // Free query result if it is not in prefetch cache
  if (not Assigned(FPrefetchResults)) or (not FPrefetchResults.Contains(Results)) then
    FreeAndNil(Results);
end;


{**
  Call log event if assigned to object
  If running a thread, log to queue and let the main thread later do logging
}
procedure TDBConnection.Log(Category: TDBLogCategory; Msg: String);
begin
  if Assigned(FOnLog) then begin
    if FLogPrefix <> '' then
      Msg := '['+FLogPrefix+'] ' + Msg;
    // If in a thread, synchronize logging with the main thread. Logging within a thread
    // causes SynEdit to throw exceptions left and right.
    if (FLockedByThread <> nil) and (FLockedByThread.ThreadID = GetCurrentThreadID) then
      (FLockedByThread as TQueryThread).LogFromOutside(Msg, Category)
    else
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
  case FParameters.NetTypeGroup of
    ngMySQL: begin
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
    end;

    ngMSSQL, ngSQLite: begin

      c1 := '''';
      c2 := '''';
      c3 := '''';
      c4 := '''';
      EscChar := '''';
      Result := escChars(Text, EscChar, c1, c2, c3, c4);

      // Escape joker chars % and _ in conjunction with a specified escape char after the WHERE clause.
      // See http://www.heidisql.com/forum.php?t=12747
      if ProcessJokerChars then begin
        c1 := '%';
        c2 := '_';
        c4 := '_';
        c3 := '_';
        EscChar := '\';
        Result := escChars(Result, EscChar, c1, c2, c3, c4);
      end;
    end;

    ngPgSQL: begin
      if ProcessJokerChars then begin
        c1 := '%';
        c2 := '_';
        c3 := '%';
        c4 := '%';
        EscChar := '\';
        Result := escChars(Text, EscChar, c1, c2, c3, c4);
      end else begin
        Result := Text;
      end;
      // Escape single quote with a second single quote
      Result := escChars(Result, '''', '''', '''', '''', '''');
    end;

  end;

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
  Result := StringReplace(Text, '\\', '\', [rfReplaceAll]);
  Result := StringReplace(Result, '\0', #0, [rfReplaceAll]);
  Result := StringReplace(Result, '\b', #8, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
  Result := StringReplace(Result, '\Z', #26, [rfReplaceAll]);
  Result := StringReplace(Result, '''''', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '\''', '''', [rfReplaceAll]);
end;


function TDBConnection.ExtractLiteral(var SQL: String; Prefix: String): String;
var
  i, LitStart: Integer;
  InLiteral: Boolean;
  rx: TRegExpr;
begin
  // Return comment from SQL and remove it from the original string
  // Single quotes are escaped by a second single quote
  Result := '';
  rx := TRegExpr.Create;
  if Prefix.IsEmpty then
    rx.Expression := '^\s*'''
  else
    rx.Expression := '^\s*'+QuoteRegExprMetaChars(Prefix)+'\s+''';
  rx.ModifierI := True;
  if rx.Exec(SQL) then begin
    LitStart := rx.MatchLen[0]+1;
    InLiteral := True;
    for i:=LitStart to Length(SQL) do begin
      if SQL[i] = '''' then
        InLiteral := not InLiteral
      else if not InLiteral then
        break;
    end;
    Result := Copy(SQL, LitStart, i-LitStart-1);
    Result := UnescapeString(Result);
    Delete(SQL, 1, i);
  end;
  rx.Free;
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
var
  Quote: Char;
begin
  Result := Identifier;
  if (Length(Identifier)>0) and (Result[1] = FQuoteChar) and (Result[Length(Identifier)] = FQuoteChar) then
    Result := Copy(Result, 2, Length(Result)-2);
  if Glue <> #0 then
    Result := StringReplace(Result, FQuoteChar+Glue+FQuoteChar, Glue, [rfReplaceAll]);
  Result := StringReplace(Result, FQuoteChar+FQuoteChar, FQuoteChar, [rfReplaceAll]);
  // Remove all probable quote characters, to fix various problems
  for Quote in FQuoteChars do begin
    Result := StringReplace(Result, Quote, '', [rfReplaceAll]);
  end;
end;


function TDBConnection.QuotedDbAndTableName(DB, Obj: String): String;
var
  o: TDBObject;
begin
  // Call TDBObject.QuotedDbAndTableName for db and table string.
  // Return fully qualified db and tablename, quoted, and including schema if required
  o := FindObject(DB, Obj);
  if o <> nil then
    Result := o.QuotedDbAndTableName()
  else begin
    // Fallback for target tables which do not yet exist. For example in copytable dialog.
    Result := QuoteIdent(DB) + '.';
    if Parameters.IsAnyMSSQL then
      Result := Result + '.';
    Result := Result + QuoteIdent(Obj);
  end;
end;


function TDBConnection.FindObject(DB, Obj: String): TDBObject;
var
  Objects: TDBObjectList;
  o: TDBObject;
begin
  // Find TDBObject by db and table string
  Objects := GetDBObjects(DB);
  Result := nil;
  for o in Objects do begin
    if o.Name = Obj then begin
      Result := o;
      Break;
    end;
  end;
  if not Assigned(Result) then begin
    Log(lcDebug, Format('Could not find object "%s" in database "%s"', [Obj, DB]));
  end;
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
  FreeResults(Results);
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
  FreeResults(Results);
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
  FreeResults(Results);
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
      Results := GetSessionVariables(False);
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
      ' FROM '+QuotedDbAndTableName('master', 'syscharsets')
      );
  Result := FCharsetTable;
end;


function TPgConnection.GetCharsetTable: TDBQuery;
begin
  inherited;
  if not Assigned(FCharsetTable) then
    FCharsetTable := GetResults('SELECT PG_ENCODING_TO_CHAR('+QuoteIdent('encid')+') AS '+QuoteIdent('Charset')+', '+EscapeString('')+' AS '+QuoteIdent('Description')+' FROM ('+
      'SELECT '+QuoteIdent('conforencoding')+' AS '+QuoteIdent('encid')+' FROM '+QuoteIdent('pg_conversion')+', '+QuoteIdent('pg_database')+' '+
      'WHERE '+QuoteIdent('contoencoding')+'='+QuoteIdent('encoding')+' AND '+QuoteIdent('datname')+'=CURRENT_DATABASE()) AS '+QuoteIdent('e')
      );
  Result := FCharsetTable;
end;


function TSQLiteConnection.GetCharsetTable;
begin
  inherited;
  if not Assigned(FCharsetTable) then begin
    //FCharsetTable := // Todo!
  end;
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
      Result.Add(c.Col('Charset') + ': ' + c.Col('Description'));
      c.Next;
    end;
    Result.Sort;
  end;
end;


function TDBConnection.GetSessionVariables(Refresh: Boolean): TDBQuery;
begin
  // Return server variables
  if (not Assigned(FSessionVariables)) or Refresh then begin
    if Assigned(FSessionVariables) then
      FreeAndNil(FSessionVariables);
    FSessionVariables := GetResults(GetSQLSpecifity(spSessionVariables));
  end;
  FSessionVariables.First;
  Result := FSessionVariables;
end;


function TDBConnection.GetSessionVariable(VarName: String; DefaultValue: String=''; Refresh: Boolean=False): String;
var
  Vars: TDBQuery;
  VarExists: Boolean;
begin
  // Return the value of a specific server variable
  Vars := GetSessionVariables(Refresh);
  Result := DefaultValue;
  VarExists := False;
  while not Vars.Eof do begin
    if Vars.Col(0) = VarName then begin
      Result := Vars.Col(1);
      VarExists := True;
      Break;
    end;
    Vars.Next;
  end;
  if not VarExists then begin
    Log(lcDebug, 'Variable "'+VarName+'" does not exist');
  end;
end;


function TDBConnection.MaxAllowedPacket: Int64;
begin
  // Default
  Result := SIZE_MB;
end;


function TMySQLConnection.MaxAllowedPacket: Int64;
begin
  Result := MakeInt(GetSessionVariable('max_allowed_packet'));
  if Result < SIZE_KB*10 then begin
    Result := SIZE_MB;
    Log(lcError, f_('The server did not return a non-zero value for the %s variable. Assuming %s now.', ['max_allowed_packet', FormatByteNumber(Result)]));
  end;

end;


function TDBConnection.GetLockedTableCount(db: String): Integer;
var
  sql: String;
  LockedTables: TStringList;
begin
  // Find tables which are currently locked.
  // Used to prevent waiting time in GetDBObjects.
  sql := GetSQLSpecifity(spLockedTables);
  Result := 0;
  if not sql.IsEmpty then try
    LockedTables := GetCol(Format(sql, [QuoteIdent(db,False)]));
    Result := LockedTables.Count;
    LockedTables.Free;
  except // Suppress errors, due to not working on all servers: https://www.heidisql.com/forum.php?t=34984
    on E:EDbError do;
  end;
end;


function TDBConnection.IdentifierEquals(Ident1, Ident2: String): Boolean;
var
  CaseSensitivity: Integer;
begin
  // Compare only name of identifier, in the case fashion the server tells us
  // 1 is probably a bad default value, as this expects the server to run on Windows
  CaseSensitivity := MakeInt(GetSessionVariable('lower_case_table_names', '1'));
  case CaseSensitivity of
    0: Result := Ident1 = Ident2;
    else Result := CompareText(Ident1, Ident2) = 0;
  end;
end;


function TDBConnection.GetTableColumns(Table: TDBObject): TTableColumnList;
var
  TableIdx: Integer;
  ColQuery: TDBQuery;
  Col: TTableColumn;
  dt, SchemaClause, DefText, ExtraText, MaxLen: String;
begin
  // Generic: query table columns from IS.COLUMNS
  Result := TTableColumnList.Create(True);
  TableIdx := InformationSchemaObjects.IndexOf('columns');
  if Table.Schema <> '' then
    SchemaClause := 'TABLE_SCHEMA='+EscapeString(Table.Schema)
  else
    SchemaClause := GetSQLSpecifity(spISTableSchemaCol)+'='+EscapeString(Table.Database);
  ColQuery := GetResults('SELECT * FROM '+QuoteIdent(InfSch)+'.'+QuoteIdent(InformationSchemaObjects[TableIdx])+
    ' WHERE '+SchemaClause+' AND TABLE_NAME='+EscapeString(Table.Name)+
    ' ORDER BY ORDINAL_POSITION');
  while not ColQuery.Eof do begin
    Col := TTableColumn.Create(Self);
    Result.Add(Col);
    Col.Name := ColQuery.Col('COLUMN_NAME');
    Col.OldName := Col.Name;
    // PG/MySQL use different fields:
    dt := IfThen(ColQuery.ColExists('COLUMN_TYPE'), 'COLUMN_TYPE', 'DATA_TYPE');
    Col.ParseDatatype(ColQuery.Col(dt));
    // PG/MSSQL don't include length in data type
    if Col.LengthSet.IsEmpty then begin
      if not ColQuery.IsNull('CHARACTER_MAXIMUM_LENGTH') then begin
        MaxLen := ColQuery.Col('CHARACTER_MAXIMUM_LENGTH');
        if MaxLen = '-1' then
          MaxLen := 'max';
      end else if not ColQuery.IsNull('NUMERIC_PRECISION') then begin
        MaxLen := ColQuery.Col('NUMERIC_PRECISION');
        if (not ColQuery.IsNull('NUMERIC_SCALE'))
          or (Col.DataType.Index in [dtDouble]) then begin
          MaxLen := MaxLen + ',' + StrToIntDef(ColQuery.Col('NUMERIC_SCALE'), 0).ToString;
        end;
      end else if not ColQuery.IsNull('DATETIME_PRECISION') then begin
        MaxLen := ColQuery.Col('DATETIME_PRECISION');
      end;
      if not MaxLen.IsEmpty then
        Col.LengthSet := MaxLen;
    end;
    Col.Charset := ColQuery.Col('CHARACTER_SET_NAME');
    Col.Collation := ColQuery.Col('COLLATION_NAME');
    // MSSQL has no expression
    Col.Expression := ColQuery.Col('GENERATION_EXPRESSION', True);
    // PG has no extra:
    ExtraText := ColQuery.Col('EXTRA', True);
    Col.Virtuality := RegExprGetMatch('^(\w+)\s+generated$', ExtraText.ToLowerInvariant, 1);
    Col.AllowNull := ColQuery.Col('IS_NULLABLE').ToLowerInvariant = 'yes';

    DefText := ColQuery.Col('COLUMN_DEFAULT');
    Col.OnUpdateType := cdtNothing;
    if DefText.ToLowerInvariant = 'null' then begin
      Col.DefaultType := cdtNull;
    end else if ColQuery.IsNull('COLUMN_DEFAULT') then begin
      if Col.AllowNull then
        Col.DefaultType := cdtNull
      else
        Col.DefaultType := cdtNothing;
    end else if ExecRegExpr('^auto_increment$', ExtraText.ToLowerInvariant) then begin
      Col.DefaultType := cdtAutoInc;
      Col.DefaultText := 'AUTO_INCREMENT';
    end else if DefText.StartsWith('''') then begin
      Col.DefaultType := cdtText;
      Col.DefaultText := ExtractLiteral(DefText, '');
    end else if DefText.IsEmpty or IsInt(DefText[1]) then begin
      // Inexact detection, wrong if MySQL allows 0+1 as default value at some point
      Col.DefaultType := cdtText;
      Col.DefaultText := DefText;
    end else begin
      Col.DefaultType := cdtExpression;
      Col.DefaultText := DefText;
    end;
    Col.OnUpdateText := RegExprGetMatch('^on update (.*)$', ExtraText, 1);
    if not Col.OnUpdateText.IsEmpty then begin
      Col.OnUpdateType := cdtExpression;
    end;

    // PG has no column_comment:
    Col.Comment := ColQuery.Col('COLUMN_COMMENT', True);
    ColQuery.Next;
  end;
  ColQuery.Free;
end;


function TMySQLConnection.GetTableColumns(Table: TDBObject): TTableColumnList;
var
  TableIdx: Integer;
  ColQuery: TDBQuery;
  Col: TTableColumn;
  DefText, ExtraText: String;
begin
  TableIdx := InformationSchemaObjects.IndexOf('columns');
  if TableIdx > -1 then begin
    Result := inherited;
    Exit;
  end;
  // !!Fallback!! for old MySQL pre-5.0 servers
  Result := TTableColumnList.Create(True);
  ColQuery := GetResults('SHOW FULL COLUMNS FROM '+QuoteIdent(Table.Database)+'.'+QuoteIdent(Table.Name));
  while not ColQuery.Eof do begin
    Col := TTableColumn.Create(Self);
    Result.Add(Col);
    Col.Name := ColQuery.Col(0);
    Col.OldName := Col.Name;
    Col.ParseDatatype(ColQuery.Col('Type'));
    Col.Collation := ColQuery.Col('Collation');
    if Col.Collation.ToLowerInvariant = 'null' then
      Col.Collation := '';
    Col.AllowNull := ColQuery.Col('Null').ToLowerInvariant = 'yes';

    DefText := ColQuery.Col('Default');
    ExtraText := ColQuery.Col('Extra');
    Col.OnUpdateType := cdtNothing;
    if ExecRegExpr('^auto_increment$', ExtraText.ToLowerInvariant) then begin
      Col.DefaultType := cdtAutoInc;
      Col.DefaultText := 'AUTO_INCREMENT';
    end else if ColQuery.IsNull('Default') then begin
      Col.DefaultType := cdtNothing;
    end else if DefText.StartsWith('''') then begin
      Col.DefaultType := cdtText;
      Col.DefaultText := ExtractLiteral(DefText, '');
    end else if DefText.IsEmpty or IsInt(DefText[1]) then begin
      // Inexact detection, wrong if MySQL allows 0+1 as default value at some point
      Col.DefaultType := cdtText;
      Col.DefaultText := DefText;
    end else begin
      Col.DefaultType := cdtExpression;
      Col.DefaultText := DefText;
    end;
    Col.OnUpdateText := RegExprGetMatch('^on update (.*)$', ExtraText, 1);
    if not Col.OnUpdateText.IsEmpty then begin
      Col.OnUpdateType := cdtExpression;
    end;

    Col.Comment := ColQuery.Col('Comment');
    ColQuery.Next;
  end;
  ColQuery.Free;
end;


function TAdoDBConnection.GetTableColumns(Table: TDBObject): TTableColumnList;
var
  Comments: TDBQuery;
  TableCol: TTableColumn;
begin
  // Parent method is sufficient for most things
  Result := inherited;

  // Remove surrounding parentheses from default value. See #721
  for TableCol in Result do begin
    if not TableCol.DefaultText.IsEmpty then
      TableCol.DefaultText := RegExprGetMatch('^\((.*)\)$', TableCol.DefaultText, 1);
  end;

  // Column comments in MSSQL. See http://www.heidisql.com/forum.php?t=19576
  try
    Comments := GetResults('SELECT c.name AS '+QuoteIdent('column')+', prop.value AS '+QuoteIdent('comment')+' '+
      'FROM sys.extended_properties AS prop '+
      'INNER JOIN sys.all_objects o ON prop.major_id = o.object_id '+
      'INNER JOIN sys.schemas s ON o.schema_id = s.schema_id '+
      'INNER JOIN sys.columns AS c ON prop.major_id = c.object_id AND prop.minor_id = c.column_id '+
      'WHERE '+
      '  prop.name='+EscapeString('MS_Description')+
      '  AND s.name='+EscapeString(Table.Schema)+
      '  AND o.name='+EscapeString(Table.Name)
      );
    while not Comments.Eof do begin
      for TableCol in Result do begin
        if TableCol.Name = Comments.Col('column') then begin
          TableCol.Comment := Comments.Col('comment');
          Break;
        end;
      end;
      Comments.Next;
    end;
  except // Fails on old servers
    on E:EDbError do;
  end;

end;

function TPgConnection.GetTableColumns(Table: TDBObject): TTableColumnList;
var
  Comments: TDBQuery;
  TableCol: TTableColumn;
begin
  Result := inherited;
  // Column comments in Postgre. See issue #859
  // Todo: add current schema to WHERE clause?
  Comments := GetResults('SELECT a.attname AS column, des.description AS comment'+
    ' FROM pg_attribute AS a, pg_description AS des, pg_class AS pgc'+
    ' WHERE'+
    '     pgc.oid = a.attrelid'+
    '     AND des.objoid = pgc.oid'+
    '     AND pg_table_is_visible(pgc.oid)'+
    '     AND pgc.relname = '+EscapeString(Table.Name)+
    '     AND a.attnum = des.objsubid'
    );
  while not Comments.Eof do begin
    for TableCol in Result do begin
      if TableCol.Name = Comments.Col('column') then begin
        TableCol.Comment := Comments.Col('comment');
        Break;
      end;
    end;
    Comments.Next;
  end;
end;

function TSQLiteConnection.GetTableColumns(Table: TDBObject): TTableColumnList;
var
  ColQuery: TDBQuery;
  Col: TTableColumn;
begin
  // SQLite has no IS.COLUMNS
  // Todo: include database name
  // Todo: default values
  Result := TTableColumnList.Create(True);
  ColQuery := GetResults('SELECT * FROM pragma_table_info('+EscapeString(Table.Name)+')');
  while not ColQuery.Eof do begin
    Col := TTableColumn.Create(Self);
    Result.Add(Col);
    Col.Name := ColQuery.Col('name');
    Col.OldName := Col.Name;
    Col.ParseDatatype(ColQuery.Col('type'));
    Col.AllowNull := ColQuery.Col('notnull') <> '1';
    Col.DefaultType := cdtNothing;
    Col.DefaultText := '';
    Col.OnUpdateType := cdtNothing;
    Col.OnUpdateText := '';
    ColQuery.Next;
  end;
  ColQuery.Free;
end;


function TDBConnection.GetTableKeys(Table: TDBObject): TTableKeyList;
var
  ColTableIdx, ConTableIdx: Integer;
  KeyQuery: TDBQuery;
  NewKey: TTableKey;
begin
  // Generic: query table keys from IS.KEY_COLUMN_USAGE
  Result := TTableKeyList.Create(True);
  ColTableIdx := InformationSchemaObjects.IndexOf('KEY_COLUMN_USAGE');
  ConTableIdx := InformationSchemaObjects.IndexOf('TABLE_CONSTRAINTS');
  KeyQuery := GetResults('SELECT * FROM '+
    QuoteIdent(InfSch)+'.'+QuoteIdent(InformationSchemaObjects[ColTableIdx])+' AS col'+
    ', '+QuoteIdent(InfSch)+'.'+QuoteIdent(InformationSchemaObjects[ConTableIdx])+' AS con'+
    ' WHERE col.TABLE_SCHEMA='+EscapeString(IfThen(Parameters.IsAnyMSSQL, Table.Schema, Table.Database))+
    ' AND col.TABLE_NAME='+EscapeString(Table.Name)+
    ' AND col.TABLE_SCHEMA=con.TABLE_SCHEMA'+
    ' AND col.TABLE_NAME=con.TABLE_NAME'+
    ' AND col.CONSTRAINT_NAME=con.CONSTRAINT_NAME'
    );
  NewKey := nil;
  while not KeyQuery.Eof do begin
    if (not KeyQuery.ColExists('REFERENCED_TABLE_NAME'))
      or KeyQuery.Col('REFERENCED_TABLE_NAME').IsEmpty then begin
      if (not Assigned(NewKey)) or (NewKey.Name <> KeyQuery.Col('CONSTRAINT_NAME')) then begin
        NewKey := TTableKey.Create(Self);
        Result.Add(NewKey);
        NewKey.Name := KeyQuery.Col('CONSTRAINT_NAME');
        NewKey.OldName := NewKey.Name;
        if KeyQuery.Col('CONSTRAINT_TYPE').ToLowerInvariant.StartsWith('primary') then
          NewKey.IndexType := TTableKey.PRIMARY
        else
          NewKey.IndexType := KeyQuery.Col('CONSTRAINT_TYPE');
        NewKey.OldIndexType := NewKey.IndexType;
      end;
      NewKey.Columns.Add(KeyQuery.Col('COLUMN_NAME'));
      NewKey.SubParts.Add('');
    end;
    KeyQuery.Next;
  end;
  KeyQuery.Free;
end;


function TMySQLConnection.GetTableKeys(Table: TDBObject): TTableKeyList;
var
  KeyQuery: TDBQuery;
  NewKey: TTableKey;
begin
  Result := TTableKeyList.Create(True);
  KeyQuery := GetResults('SHOW INDEXES FROM '+QuoteIdent(Table.Name)+' FROM '+QuoteIdent(Table.Database));
  NewKey := nil;
  while not KeyQuery.Eof do begin
    if (not Assigned(NewKey)) or (NewKey.Name <> KeyQuery.Col('Key_name')) then begin
      NewKey := TTableKey.Create(Self);
      Result.Add(NewKey);
      NewKey.Name := KeyQuery.Col('Key_name');
      NewKey.OldName := NewKey.Name;
      if NewKey.Name.ToLowerInvariant = 'primary' then
        NewKey.IndexType := TTableKey.PRIMARY
      else if KeyQuery.Col('Non_unique') = '0' then
        NewKey.IndexType := TTableKey.UNIQUE
      else if KeyQuery.Col('Index_type').ToLowerInvariant = 'fulltext' then
        NewKey.IndexType := TTableKey.FULLTEXT
      else
        NewKey.IndexType := TTableKey.KEY;
      // Todo: spatial keys
      NewKey.OldIndexType := NewKey.IndexType;
      if ExecRegExpr('(BTREE|HASH)', KeyQuery.Col('Index_type')) then
        NewKey.Algorithm := KeyQuery.Col('Index_type');
      NewKey.Comment := KeyQuery.Col('Index_comment', True);
    end;
    NewKey.Columns.Add(KeyQuery.Col('Column_name'));
    NewKey.SubParts.Add(KeyQuery.Col('Sub_part'));
    KeyQuery.Next;
  end;
  KeyQuery.Free;
end;


function TPGConnection.GetTableKeys(Table: TDBObject): TTableKeyList;
var
  KeyQuery: TDBQuery;
  NewKey: TTableKey;
begin
  Result := TTableKeyList.Create(True);
  // For PostgreSQL there seem to be privilege problems in IS.
  // See http://www.heidisql.com/forum.php?t=16213
  if ServerVersionInt >= 90000 then begin
    KeyQuery := GetResults('WITH ndx_list AS ('+
      '    SELECT pg_index.indexrelid, pg_class.oid'+
      '    FROM pg_index, pg_class'+
      '    WHERE pg_class.relname = '+EscapeString(Table.Name)+
      '    AND pg_class.oid = pg_index.indrelid'+
      '  ),'+
      '  ndx_cols AS ('+
      '    SELECT pg_class.relname, UNNEST(i.indkey) AS col_ndx,'+
      '    CASE i.indisprimary WHEN true THEN '+EscapeString(TTableKey.PRIMARY)+' ELSE CASE i.indisunique WHEN true THEN '+EscapeString(TTableKey.UNIQUE)+' ELSE '+EscapeString(TTableKey.KEY)+' END END AS CONSTRAINT_TYPE,'+
      '    pg_class.oid'+
      '    FROM pg_class'+
      '    JOIN pg_index i ON (pg_class.oid = i.indexrelid)'+
      '    JOIN ndx_list ON (pg_class.oid = ndx_list.indexrelid)'+
      '    WHERE pg_table_is_visible(pg_class.oid)'+
      '  )'+
      'SELECT ndx_cols.relname AS CONSTRAINT_NAME, ndx_cols.CONSTRAINT_TYPE, a.attname AS COLUMN_NAME '+
      'FROM pg_attribute a '+
      'JOIN ndx_cols ON (a.attnum = ndx_cols.col_ndx) '+
      'JOIN ndx_list ON (ndx_list.oid = a.attrelid AND ndx_list.indexrelid = ndx_cols.oid)'
      );
  end else begin
    KeyQuery := GetResults('SELECT '+QuoteIdent('c')+'.'+QuoteIdent('conname')+' AS '+QuoteIdent('CONSTRAINT_NAME')+', '+
      'CASE '+QuoteIdent('c')+'.'+QuoteIdent('contype')+' '+
      'WHEN '+EscapeString('c')+' THEN '+EscapeString('CHECK')+' '+
      'WHEN '+EscapeString('f')+' THEN '+EscapeString('FOREIGN KEY')+' '+
      'WHEN '+EscapeString('p')+' THEN '+EscapeString('PRIMARY KEY')+' '+
      'WHEN '+EscapeString('u')+' THEN '+EscapeString('UNIQUE')+' '+
      'END AS '+QuoteIdent('CONSTRAINT_TYPE')+', '+
      QuoteIdent('a')+'.'+QuoteIdent('attname')+' AS '+QuoteIdent('COLUMN_NAME')+' '+
      'FROM '+QuoteIdent('pg_constraint')+' AS '+QuoteIdent('c')+' '+
      'LEFT JOIN '+QuoteIdent('pg_class')+' '+QuoteIdent('t')+' ON '+QuoteIdent('c')+'.'+QuoteIdent('conrelid')+'='+QuoteIdent('t')+'.'+QuoteIdent('oid')+' '+
      'LEFT JOIN '+QuoteIdent('pg_attribute')+' '+QuoteIdent('a')+' ON '+QuoteIdent('t')+'.'+QuoteIdent('oid')+'='+QuoteIdent('a')+'.'+QuoteIdent('attrelid')+' '+
      'LEFT JOIN '+QuoteIdent('pg_namespace')+' '+QuoteIdent('n')+' ON '+QuoteIdent('t')+'.'+QuoteIdent('relnamespace')+'='+QuoteIdent('n')+'.'+QuoteIdent('oid')+' '+
      'WHERE c.contype IN ('+EscapeString('p')+', '+EscapeString('u')+') '+
      'AND '+QuoteIdent('a')+'.'+QuoteIdent('attnum')+'=ANY('+QuoteIdent('c')+'.'+QuoteIdent('conkey')+') '+
      'AND '+QuoteIdent('n')+'.'+QuoteIdent('nspname')+'='+EscapeString(Table.Schema)+' '+
      'AND '+QuoteIdent('t')+'.'+QuoteIdent('relname')+'='+EscapeString(Table.Name)+' '+
      'ORDER BY '+QuoteIdent('a')+'.'+QuoteIdent('attnum')
      );
  end;
  NewKey := nil;
  while not KeyQuery.Eof do begin
    if (not Assigned(NewKey)) or (NewKey.Name <> KeyQuery.Col('CONSTRAINT_NAME')) then begin
      NewKey := TTableKey.Create(Self);
      Result.Add(NewKey);
      NewKey.Name := KeyQuery.Col('CONSTRAINT_NAME');
      NewKey.OldName := NewKey.Name;
      NewKey.IndexType := KeyQuery.Col('CONSTRAINT_TYPE');
      if NewKey.IndexType.ToLowerInvariant.EndsWith(' key') then
        Delete(NewKey.IndexType, Length(NewKey.IndexType)-4, 4);
      NewKey.OldIndexType := NewKey.IndexType;
    end;
    NewKey.Columns.Add(KeyQuery.Col('COLUMN_NAME'));
    NewKey.SubParts.Add('');
    KeyQuery.Next;
  end;
  KeyQuery.Free;
end;


function TSQLiteConnection.GetTableKeys(Table: TDBObject): TTableKeyList;
var
  ColQuery, KeyQuery: TDBQuery;
  NewKey: TTableKey;
begin
  Result := TTableKeyList.Create(True);
  ColQuery := GetResults('SELECT * FROM pragma_table_info('+EscapeString(Table.Name)+') WHERE pk!=0 ORDER BY pk');
  NewKey := nil;
  while not ColQuery.Eof do begin
    if not Assigned(NewKey) then begin
      NewKey := TTableKey.Create(Self);
      Result.Add(NewKey);
      NewKey.Name := 'PRIMARY';
      NewKey.OldName := NewKey.Name;
      NewKey.IndexType := TTableKey.PRIMARY;
      NewKey.OldIndexType := NewKey.IndexType;
    end;
    NewKey.Columns.Add(ColQuery.Col('name'));
    NewKey.SubParts.Add('');
    ColQuery.Next;
  end;
  ColQuery.Free;

  KeyQuery := GetResults('SELECT * FROM pragma_index_list('+EscapeString(Table.Name)+') WHERE origin!='+EscapeString('pk'));
  while not KeyQuery.Eof do begin
    NewKey := TTableKey.Create(Self);
    Result.Add(NewKey);
    NewKey.Name := KeyQuery.Col('name');
    NewKey.OldName := NewKey.Name;
    NewKey.IndexType := IfThen(KeyQuery.Col('unique')='0', TTableKey.KEY, TTableKey.UNIQUE);
    NewKey.OldIndexType := NewKey.IndexType;
    ColQuery := GetResults('SELECT * FROM pragma_index_info('+EscapeString(NewKey.Name)+')');
    while not ColQuery.Eof do begin
      NewKey.Columns.Add(ColQuery.Col('name'));
      NewKey.SubParts.Add('');
      ColQuery.Next;
    end;
    ColQuery.Free;
    KeyQuery.Next;
  end;
  KeyQuery.Free;
end;


function TDBConnection.GetTableForeignKeys(Table: TDBObject): TForeignKeyList;
var
  ForeignQuery, ColQuery: TDBQuery;
  ForeignKey: TForeignKey;
begin
  // Generic: query table foreign keys from IS.?
  Result := TForeignKeyList.Create(True);
  if FForeignKeyQueriesFailed then begin
    Log(lcDebug, 'Avoid foreign key retrieval with queries which failed before');
    Exit;
  end;
  try
    // Combine two IS tables by hand, not by JOIN, as this is too slow. See #852
    ForeignQuery := GetResults('SELECT *'+
      ' FROM '+InfSch+'.REFERENTIAL_CONSTRAINTS'+
      ' WHERE'+
      '   CONSTRAINT_SCHEMA='+EscapeString(Table.Database)+
      '   AND TABLE_NAME='+EscapeString(Table.Name)+
      '   AND REFERENCED_TABLE_NAME IS NOT NULL'
      );
    ColQuery := GetResults('SELECT *'+
      ' FROM '+InfSch+'.KEY_COLUMN_USAGE'+
      ' WHERE'+
      '   CONSTRAINT_SCHEMA='+EscapeString(Table.Database)+
      '   AND TABLE_NAME='+EscapeString(Table.Name)+
      '   AND REFERENCED_TABLE_NAME IS NOT NULL'
      );
    try
      while not ForeignQuery.Eof do begin
        ForeignKey := TForeignKey.Create(Self);
        Result.Add(ForeignKey);
        ForeignKey.KeyName := ForeignQuery.Col('CONSTRAINT_NAME');
        ForeignKey.OldKeyName := ForeignKey.KeyName;
        ForeignKey.ReferenceTable := ForeignQuery.Col('UNIQUE_CONSTRAINT_SCHEMA') +
          '.' + ForeignQuery.Col('REFERENCED_TABLE_NAME');
        ForeignKey.OnUpdate := ForeignQuery.Col('UPDATE_RULE');
        ForeignKey.OnDelete := ForeignQuery.Col('DELETE_RULE');
        while not ColQuery.Eof do begin
          if ColQuery.Col('CONSTRAINT_NAME') = ForeignQuery.Col('CONSTRAINT_NAME') then begin
            ForeignKey.Columns.Add(ColQuery.Col('COLUMN_NAME'));
            ForeignKey.ForeignColumns.Add(ColQuery.Col('REFERENCED_COLUMN_NAME'));
          end;
          ColQuery.Next;
        end;
        ColQuery.First;
        ForeignQuery.Next;
      end;
      ForeignQuery.Free;
      ColQuery.Free;
    except
      // Don't silence errors here:
      on E:EDbError do
        Log(lcError, E.Message);
    end;
  except
    // Silently ignore non existent IS tables and/or columns
    // And remember to not fire such queries again here
    on E:EDbError do begin
      FForeignKeyQueriesFailed := True;
    end;
  end;
end;


function TAdoDbConnection.GetTableForeignKeys(Table: TDBObject): TForeignKeyList;
var
  ForeignQuery: TDBQuery;
  ForeignKey: TForeignKey;
begin
  // MS SQL: see #150
  Result := TForeignKeyList.Create(True);
  ForeignQuery := GetResults('SELECT'+
    '   f.name AS foreign_key_name,'+
    '   COL_NAME(fc.parent_object_id, fc.parent_column_id) AS constraint_column_name,'+
    '   OBJECT_NAME (f.referenced_object_id) AS referenced_object,'+
    '   COL_NAME(fc.referenced_object_id, fc.referenced_column_id) AS referenced_column_name,'+
    '   update_referential_action_desc,'+
    '   delete_referential_action_desc'+
    ' FROM sys.foreign_keys AS f'+
    ' INNER JOIN sys.foreign_key_columns AS fc'+
    '   ON f.object_id = fc.constraint_object_id'+
    ' WHERE f.parent_object_id = OBJECT_ID('+EscapeString(Table.Name)+')'
    );
  ForeignKey := nil;
  while not ForeignQuery.Eof do begin
    if (not Assigned(ForeignKey)) or (ForeignKey.KeyName <> ForeignQuery.Col('foreign_key_name')) then begin
      ForeignKey := TForeignKey.Create(Self);
      Result.Add(ForeignKey);
      ForeignKey.KeyName := ForeignQuery.Col('foreign_key_name');
      ForeignKey.OldKeyName := ForeignKey.KeyName;
      ForeignKey.ReferenceTable := ForeignQuery.Col('referenced_object');
      ForeignKey.OnUpdate := ForeignQuery.Col('update_referential_action_desc');
      ForeignKey.OnDelete := ForeignQuery.Col('delete_referential_action_desc');
    end;
    ForeignKey.Columns.Add(ForeignQuery.Col('constraint_column_name'));
    ForeignKey.ForeignColumns.Add(ForeignQuery.Col('referenced_column_name'));
    ForeignQuery.Next;
  end;
  ForeignQuery.Free;
end;


function TPgConnection.GetTableForeignKeys(Table: TDBObject): TForeignKeyList;
var
  ForeignQuery: TDBQuery;
  ForeignKey: TForeignKey;
begin
  // see #158
  Result := TForeignKeyList.Create(True);
  ForeignQuery := GetResults('SELECT'+
    '   refc.constraint_name,'+
    '   refc.update_rule,'+
    '   refc.delete_rule,'+
    '   kcu.table_name,'+
    '   STRING_AGG(distinct kcu.column_name, '','') AS columns,'+
    '   ccu.table_schema AS ref_schema,'+
    '   ccu.table_name AS ref_table,'+
    '   STRING_AGG(distinct ccu.column_name, '','') AS ref_columns,'+
    '   STRING_AGG(distinct kcu.ordinal_position::text, '','') AS ord_position'+
    ' FROM'+
    '   '+InfSch+'.referential_constraints AS refc,'+
    '   '+InfSch+'.key_column_usage AS kcu,'+
    '   '+InfSch+'.constraint_column_usage AS ccu'+
    ' WHERE'+
    '   refc.constraint_schema = '+EscapeString(Table.Schema)+
    '   AND refc.constraint_name = kcu.constraint_name'+
    '   AND refc.constraint_schema = kcu.table_schema'+
    '   AND ccu.constraint_name = refc.constraint_name'+
    '   AND kcu.table_name = '+EscapeString(Table.Name)+
    ' GROUP BY'+
    '   refc.constraint_name,'+
    '   refc.update_rule,'+
    '   refc.delete_rule,'+
    '   kcu.table_name,'+
    '   ccu.table_schema,'+
    '   ccu.table_name'+
    ' ORDER BY'+
    '   ord_position'
    );
  while not ForeignQuery.Eof do begin
    ForeignKey := TForeignKey.Create(Self);
    Result.Add(ForeignKey);
    ForeignKey.KeyName := ForeignQuery.Col('constraint_name');
    ForeignKey.OldKeyName := ForeignKey.KeyName;
    ForeignKey.ReferenceTable := ForeignQuery.Col('ref_schema')+'.'+ForeignQuery.Col('ref_table');
    ForeignKey.OnUpdate := ForeignQuery.Col('update_rule');
    ForeignKey.OnDelete := ForeignQuery.Col('delete_rule');
    ForeignKey.Columns.CommaText := ForeignQuery.Col('columns');
    ForeignKey.ForeignColumns.CommaText := ForeignQuery.Col('ref_columns');
    ForeignQuery.Next;
  end;
  ForeignQuery.Free;
end;


function TSQLiteConnection.GetTableForeignKeys(Table: TDBObject): TForeignKeyList;
var
  ForeignQuery: TDBQuery;
  ForeignKey: TForeignKey;
begin
  // SQLite: query PRAGMA foreign_key_list
  Result := TForeignKeyList.Create(True);
  ForeignQuery := GetResults('SELECT * from PRAGMA_foreign_key_list('+EscapeString(Table.Name)+')');
  ForeignKey := nil;
  while not ForeignQuery.Eof do begin
    if (not Assigned(ForeignKey)) or (ForeignKey.KeyName <> ForeignQuery.Col('id')) then begin
      ForeignKey := TForeignKey.Create(Self);
      Result.Add(ForeignKey);
      ForeignKey.KeyName := ForeignQuery.Col('id');
      ForeignKey.OldKeyName := ForeignKey.KeyName;
      ForeignKey.ReferenceTable := ForeignQuery.Col('table');
      ForeignKey.OnUpdate := ForeignQuery.Col('on_update');
      ForeignKey.OnDelete := ForeignQuery.Col('on_delete');
    end;
    ForeignKey.Columns.Add(ForeignQuery.Col('from'));
    ForeignKey.ForeignColumns.Add(ForeignQuery.Col('to'));
    ForeignQuery.Next;
  end;
  ForeignQuery.Free;
end;


function TMySQLConnection.GetRowCount(Obj: TDBObject): Int64;
var
  Rows: String;
begin
  // Get row number from a mysql table
  Rows := GetVar('SHOW TABLE STATUS LIKE '+EscapeString(Obj.Name), 'Rows');
  Result := MakeInt(Rows);
end;


function TAdoDBConnection.GetRowCount(Obj: TDBObject): Int64;
var
  Rows: String;
begin
  // Get row number from a mssql table
  if ServerVersionInt >= 900 then begin
    Rows := GetVar('SELECT SUM('+QuoteIdent('rows')+') FROM '+QuoteIdent('sys')+'.'+QuoteIdent('partitions')+
      ' WHERE '+QuoteIdent('index_id')+' IN (0, 1)'+
      ' AND '+QuoteIdent('object_id')+' = object_id('+EscapeString(Obj.Database+'.'+Obj.Schema+'.'+Obj.Name)+')'
      );
  end else begin
    Rows := GetVar('SELECT COUNT(*) FROM '+Obj.QuotedDbAndTableName);
  end;
  Result := MakeInt(Rows);
end;


function TPgConnection.GetRowCount(Obj: TDBObject): Int64;
var
  Rows: String;
begin
  // Get row number from a postgres table
  Rows := GetVar('SELECT '+QuoteIdent('reltuples')+'::bigint FROM '+QuoteIdent('pg_class')+
    ' LEFT JOIN '+QuoteIdent('pg_namespace')+
    '   ON ('+QuoteIdent('pg_namespace')+'.'+QuoteIdent('oid')+' = '+QuoteIdent('pg_class')+'.'+QuoteIdent('relnamespace')+')'+
    ' WHERE '+QuoteIdent('pg_class')+'.'+QuoteIdent('relkind')+'='+EscapeString('r')+
    ' AND '+QuoteIdent('pg_namespace')+'.'+QuoteIdent('nspname')+'='+EscapeString(Obj.Database)+
    ' AND '+QuoteIdent('pg_class')+'.'+QuoteIdent('relname')+'='+EscapeString(Obj.Name)
    );
  Result := MakeInt(Rows);
end;


function TSQLiteConnection.GetRowCount(Obj: TDBObject): Int64;
var
  Rows: String;
begin
  // Get row number from a table
  Rows := GetVar('SELECT COUNT(*) FROM '+EscapeString(Obj.Name), 0);
  Result := MakeInt(Rows);
end;



procedure TDBConnection.Drop(Obj: TDBObject);
begin
  Query('DROP '+UpperCase(Obj.ObjType)+' '+Obj.QuotedName);
end;


procedure TPgConnection.Drop(Obj: TDBObject);
var
  sql: String;
  i: Integer;
  Params: TRoutineParamList;
begin
  case Obj.NodeType of
    lntFunction, lntProcedure: begin
      sql := 'DROP '+UpperCase(Obj.ObjType)+' '+Obj.QuotedName+'(';
      Params := TRoutineParamList.Create;
      ParseRoutineStructure(Obj, Params);
      for i:=0 to Params.Count-1 do begin
        if Obj.NodeType = lntProcedure then
          sql := sql + Params[i].Context + ' ';
        sql := sql + QuoteIdent(Params[i].Name) + ' ' + Params[i].Datatype;
        if i < Params.Count-1 then
          sql := sql + ', ';
      end;
      sql := sql + ')';
      Query(sql);
    end;
    else
      inherited;
  end;
end;


function TDBConnection.GetSQLSpecifity(Specifity: TSQLSpecifityId): String;
begin
  // Return some version specific SQL clause or snippet
  Result := FSQLSpecifities[Specifity];
end;


function TDBConnection.GetInformationSchemaObjects: TStringList;
var
  Objects: TDBObjectList;
  Obj: TDBObject;
begin
  Log(lcDebug, 'Fetching objects in '+InfSch+' db ...');
  Ping(True);
  if not Assigned(FInformationSchemaObjects) then begin
    FInformationSchemaObjects := TStringList.Create;
    // Need to find strings case insensitively:
    FInformationSchemaObjects.CaseSensitive := False;
    // Gracefully return an empty list on old servers
    if AllDatabases.IndexOf(InfSch) > -1 then begin
      Objects := GetDBObjects(InfSch);
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


function TDBConnection.ResultCount;
begin
  case Parameters.NetTypeGroup of
    ngMySQL:
      Result := Length(TMySQLConnection(Self).LastRawResults);
    ngMSSQL:
      Result := Length(TAdoDBConnection(Self).LastRawResults);
    ngPgSQL:
      Result := Length(TPGConnection(Self).LastRawResults);
    ngSQLite:
      Result := Length(TSQLiteConnection(Self).LastRawResults);
    else
      raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(Parameters.NetType)]);
  end;
end;


function TDBConnection.GetConnectionUptime: Integer;
begin
  // Return seconds since last connect
  if not FActive then
    Result := 0
  else
    Result := (GetTickCount div 1000) - FConnectionStarted;
end;


function TDBConnection.GetServerUptime: Integer;
begin
  // Return server uptime in seconds. Return -1 if unknown.
  if FServerUptime > 0 then
    Result := Cardinal(FServerUptime) + ((GetTickCount div 1000) - FConnectionStarted)
  else
    Result := -1;
end;


function TDBConnection.GetServerNow: TDateTime;
var
  d: TDateTime;
begin
  // Return server datetime. Return -1 if unknown.
  if not FServerDateTimeOnStartup.IsEmpty then begin
    d := StrToDateTimeDef(FServerDateTimeOnStartup, 0);
    Result := IncSecond(d, (GetTickCount div 1000) - FConnectionStarted);
  end else
    Result := -1;
end;


function TDBConnection.GetCurrentUserHostCombination: String;
begin
  // Return current user@host combination, used by various object editors for DEFINER clauses
  Log(lcDebug, 'Fetching user@host ...');
  Ping(True);
  if FCurrentUserHostCombination = '' then
    FCurrentUserHostCombination := GetVar(GetSQLSpecifity(spCurrentUserHost));
  Result := FCurrentUserHostCombination;
end;


function TDBConnection.GetAllUserHostCombinations: TStringList;
begin
  // For populating combobox items
  if not Assigned(FAllUserHostCombinations) then begin
    try
      FAllUserHostCombinations := GetCol('SELECT CONCAT('+QuoteIdent('User')+', '+EscapeString('@')+', '+QuoteIdent('Host')+') '+
        'FROM '+QuoteIdent('mysql')+'.'+QuoteIdent('user')+' '+
        'WHERE '+QuoteIdent('User')+'!='+EscapeString('')+' '+
        'ORDER BY '+QuoteIdent('User')+', '+QuoteIdent('Host'));
    except on E:EDbError do
      FAllUserHostCombinations := TStringList.Create;
    end;
  end;
  Result := FAllUserHostCombinations;
end;


function TDBConnection.ExplainAnalyzer(SQL, DatabaseName: String): Boolean;
begin
  Result := False;
  MessageDialog(_('Not implemented for this DBMS'), mtError, [mbOK]);
end;


function TMySQLConnection.ExplainAnalyzer(SQL, DatabaseName: String): Boolean;
var
  Results: TDBQuery;
  Raw, URL: String;
  i: Integer;
begin
  // Send EXPLAIN output to MariaDB.org
  Result := True;
  Database := DatabaseName;
  Results := GetResults('EXPLAIN '+SQL);
  Raw := '+' + CRLF + '|';
  for i:=0 to Results.ColumnCount-1 do begin
    Raw := Raw + Results.ColumnNames[i] + '|';
  end;
  Raw := Raw + CRLF + '+';
  while not Results.Eof do begin
    Raw := Raw + CRLF + '|';
    for i:=0 to Results.ColumnCount-1 do begin
      Raw := Raw + Results.Col(i) + '|';
    end;
    Results.Next;
  end;
  Raw := Raw + CRLF;
  URL := 'https://mariadb.org/explain_analyzer/analyze/?raw_explain='+EncodeURLParam(Raw)+'&client='+APPNAME;
  ShellExec(URL);
end;


function TDBConnection.GetDateTimeValue(Input: String; Datatype: TDBDatatypeIndex): String;
var
  rx: TRegExpr;
begin
  // Return date/time string value as expected by server
  case Parameters.NetTypeGroup of
    ngMSSQL: begin
      rx := TRegExpr.Create;
      rx.Expression := '^(\d+\-\d+\-\d+)\s(\d+\:.+)$';
      Result := Input;
      if rx.Exec(Input) then begin
        // Inject "T" between date and time, for MSSQL. See http://www.heidisql.com/forum.php?t=18441
        Result := rx.Match[1] + 'T' + rx.Match[2];
      end;
      rx.Free;
    end;
    else
      Result := Input;
  end;
end;



procedure TDBConnection.ClearCache(IncludeDBObjects: Boolean);
begin
  // Free cached lists and results. Called when the connection was closed and/or destroyed
  PurgePrefetchResults;
  FreeAndNil(FCollationTable);
  FreeAndNil(FCharsetTable);
  FreeAndNil(FSessionVariables);
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
  for i:=FDatabaseCache.Count-1 downto 0 do begin
    if FDatabaseCache[i].Database = db then begin
      FDatabaseCache.Delete(i);
    end;
  end;
end;


procedure TDBConnection.ClearAllDbObjects;
var
  i: Integer;
begin
  for i:=FDatabaseCache.Count-1 downto 0 do begin
    if FDatabaseCache.Count > i then
      ClearDbObjects(FDatabaseCache[i].Database);
  end;
end;


function TDBConnection.DbObjectsCached(db: String): Boolean;
var
  i: Integer;
begin
  // Check if a table list is stored in cache
  Result := False;
  for i:=0 to FDatabaseCache.Count-1 do begin
    if FDatabaseCache[i].Database = db then begin
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
  rx.Expression := '^(\d{4})\-(\d{2})\-(\d{2}) (\d{2})\:(\d{2})\:(\d{2})';
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


function TDBConnection.GetDbObjects(db: String; Refresh: Boolean=False; OnlyNodeType: TListNodeType=lntNone): TDBObjectList;
var
  Cache: TDBObjectList;
  i: Integer;
begin
  // Cache and return a db's table list
  if Refresh then
    ClearDbObjects(db);

  // Find list in cache
  Cache := nil;
  for i:=0 to FDatabaseCache.Count-1 do begin
    if (FDatabaseCache[i].Database = db) and (FDatabaseCache[i].OnlyNodeType=lntNone) then begin
      Cache := FDatabaseCache[i];
      break;
    end;
  end;

  // Fill cache if not yet fetched
  if not Assigned(Cache) then begin
    Cache := TDBObjectList.Create(TDBObjectComparer.Create);
    Cache.OwnsObjects := True;
    Cache.FOnlyNodeType := lntNone;
    Cache.FLastUpdate := 0;
    Cache.FDataSize := 0;
    Cache.FDatabase := db;
    FetchDbObjects(db, Cache);
    // Find youngest last update
    for i:=0 to Cache.Count-1 do
      Cache.FLastUpdate := Max(Cache.FLastUpdate, Max(Cache[i].Updated, Cache[i].Created));
    // Sort list like it get sorted in AnyGridCompareNodes
    Cache.Sort;
    // Add list of objects in this database to cached list of all databases
    FDatabaseCache.Add(Cache);
    if Assigned(FOnObjectnamesChanged) then
      FOnObjectnamesChanged(Self, FDatabase);
  end;

  Result := nil;
  for i:=0 to FDatabaseCache.Count-1 do begin
    if (FDatabaseCache[i].Database = db) and (FDatabaseCache[i].OnlyNodeType=OnlyNodeType) then begin
      Result := FDatabaseCache[i];
      break;
    end;
  end;
  if not Assigned(Result) then begin
    Result := TDBObjectList.Create(TDBObjectComparer.Create);
    Result.OwnsObjects := False;
    Result.FOnlyNodeType := OnlyNodeType;
    Result.FLastUpdate := Cache.FLastUpdate;
    Result.FDataSize := Cache.FDataSize;
    Result.FDatabase := Cache.FDatabase;
    Result.FCollation := Cache.FCollation;
    for i:=0 to Cache.Count-1 do begin
      if Cache[i].NodeType = OnlyNodeType then
        Result.Add(Cache[i]);
    end;
  end;
end;


procedure TMySQLConnection.FetchDbObjects(db: String; var Cache: TDBObjectList);
var
  obj: TDBObject;
  Results: TDBQuery;
  rx: TRegExpr;
begin
  // Return a db's table list
  try
    Cache.FCollation := GetVar('SELECT '+QuoteIdent('DEFAULT_COLLATION_NAME')+
      ' FROM '+QuoteIdent(InfSch)+'.'+QuoteIdent('SCHEMATA')+
      ' WHERE '+QuoteIdent('SCHEMA_NAME')+'='+EscapeString(db));
  except
    Cache.FCollation := '';
  end;
  rx := TRegExpr.Create;
  rx.ModifierI := True;

  // Tables and views
  Results := nil;
  try
    if Parameters.FullTableStatus or (UpperCase(db) = UpperCase(InfSch)) then begin
      Results := GetResults('SHOW TABLE STATUS FROM '+QuoteIdent(db));
    end else begin
      Results := GetResults('SELECT '+
          QuoteIdent('TABLE_NAME')+' AS '+QuoteIdent('Name')+', '+
          QuoteIdent('ENGINE')+' AS '+QuoteIdent('Engine')+', '+
          QuoteIdent('VERSION')+' AS '+QuoteIdent('Version')+', '+
          QuoteIdent('TABLE_COLLATION')+' AS '+QuoteIdent('Collation')+', '+
          QuoteIdent('TABLE_COMMENT')+' AS '+QuoteIdent('Comment')+', '+
          'NULL AS '+QuoteIdent('Create_time')+', '+
          'NULL AS '+QuoteIdent('Update_time')+', '+
          'NULL AS '+QuoteIdent('Data_length')+', '+
          'NULL AS '+QuoteIdent('Index_length')+', '+
          'NULL AS '+QuoteIdent('Rows')+', '+
          'NULL AS '+QuoteIdent('Auto_increment')+', '+
          'NULL AS '+QuoteIdent('Row_format')+', '+
          'NULL AS '+QuoteIdent('Avg_row_length')+', '+
          'NULL AS '+QuoteIdent('Max_data_length')+', '+
          'NULL AS '+QuoteIdent('Data_free')+', '+
          'NULL AS '+QuoteIdent('Check_time')+', '+
          'NULL AS '+QuoteIdent('Checksum')+', '+
          'NULL AS '+QuoteIdent('Create_options')+
        ' FROM '+InfSch+'.TABLES'+
        ' WHERE TABLE_SCHEMA='+EscapeString(db)+' AND TABLE_TYPE IN('+EscapeString('BASE TABLE')+', '+EscapeString('VIEW')+')'
        );
    end;
  except
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
      obj.Name := Results.Col('Name');
      obj.Database := db;
      obj.Rows := StrToInt64Def(Results.Col('Rows'), -1);
      if (not Results.IsNull('Data_length')) and (not Results.IsNull('Index_length')) then begin
        Obj.Size := StrToInt64Def(Results.Col('Data_length'), 0) + StrToInt64Def(Results.Col('Index_length'), 0);
        Inc(Cache.FDataSize, Obj.Size);
        Cache.FLargestObjectSize := Max(Cache.FLargestObjectSize, Obj.Size);
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
      Obj.RowFormat := Results.Col('Row_format', True);
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
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
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
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
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
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
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
    if InformationSchemaObjects.IndexOf('EVENTS') > -1 then
      Results := GetResults('SELECT *, EVENT_SCHEMA AS '+QuoteIdent('Db')+', EVENT_NAME AS '+QuoteIdent('Name')+
        ' FROM '+InfSch+'.'+QuoteIdent('EVENTS')+' WHERE '+QuoteIdent('EVENT_SCHEMA')+'='+EscapeString(db))
    else
      Results := GetResults('SHOW EVENTS FROM '+QuoteIdent(db));
  except
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      if Results.Col('Db') = db then begin
        Obj := TDBObject.Create(Self);
        Cache.Add(obj);
        Obj.Name := Results.Col('Name');
        Obj.Created := ParseDateTime(Results.Col('CREATED', True));
        Obj.Updated := ParseDateTime(Results.Col('LAST_ALTERED', True));
        Obj.LastChecked := ParseDateTime(Results.Col('STARTS', True));
        Obj.Comment := Results.Col('EVENT_COMMENT', True);
        Obj.Size := Length(Results.Col('EVENT_DEFINITION', True));
        Obj.Database := db;
        Obj.NodeType := lntEvent;
      end;
      Results.Next;
    end;
    FreeAndNil(Results);
  end;
end;


procedure TAdoDBConnection.FetchDbObjects(db: String; var Cache: TDBObjectList);
var
  obj: TDBObject;
  Results: TDBQuery;
  tp, SchemaSelect: String;
begin
  // Tables, views and procedures
  Results := nil;
  // Schema support introduced in MSSQL 2005 (9.0). See issue #3212.
  SchemaSelect := EscapeString('');
  if ServerVersionInt >= 900 then
    SchemaSelect := 'SCHEMA_NAME('+QuoteIdent('schema_id')+')';
  try
    Results := GetResults('SELECT *, '+SchemaSelect+' AS '+EscapeString('schema')+
      ' FROM '+QuoteIdent(db)+GetSQLSpecifity(spDbObjectsTable)+
      ' WHERE '+QuoteIdent('type')+' IN ('+EscapeString('P')+', '+EscapeString('U')+', '+EscapeString('V')+', '+EscapeString('TR')+', '+EscapeString('FN')+', '+EscapeString('TF')+', '+EscapeString('IF')+')');
  except
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
      obj.Name := Results.Col('name');
      obj.Created := ParseDateTime(Results.Col(GetSQLSpecifity(spDbObjectsCreateCol), True));
      obj.Updated := ParseDateTime(Results.Col(GetSQLSpecifity(spDbObjectsUpdateCol), True));
      obj.Schema := Results.Col('schema');
      obj.Database := db;
      tp := Trim(Results.Col(GetSQLSpecifity(spDbObjectsTypeCol), True));
      if tp = 'U' then
        obj.NodeType := lntTable
      else if tp = 'P' then
        obj.NodeType := lntProcedure
      else if tp = 'V' then
        obj.NodeType := lntView
      else if tp = 'TR' then
        obj.NodeType := lntTrigger
      else if (tp = 'FN') or (tp = 'TF') or (tp = 'IF') then
        obj.NodeType := lntFunction;
      Results.Next;
    end;
    FreeAndNil(Results);
  end;
end;


procedure TPGConnection.FetchDbObjects(db: String; var Cache: TDBObjectList);
var
  obj: TDBObject;
  Results: TDBQuery;
  tp, SchemaTable: String;
  DataLenClause, IndexLenClause: String;
begin
  // Tables, views and procedures
  Results := nil;
  try
    // See http://www.heidisql.com/forum.php?t=16429
    if ServerVersionInt >= 70300 then
      SchemaTable := 'QUOTE_IDENT(t.TABLE_SCHEMA) || '+EscapeString('.')+' || QUOTE_IDENT(t.TABLE_NAME)'
    else
      SchemaTable := EscapeString(FQuoteChar)+' || t.TABLE_SCHEMA || '+EscapeString(FQuoteChar+'.'+FQuoteChar)+' || t.TABLE_NAME || '+EscapeString(FQuoteChar);
    // See http://www.heidisql.com/forum.php?t=16996
    if Parameters.FullTableStatus and (ServerVersionInt >= 90000) then
      DataLenClause := 'pg_table_size('+SchemaTable+')::bigint'
    else
      DataLenClause := 'NULL';
    // See https://www.heidisql.com/forum.php?t=34635
    if Parameters.FullTableStatus and (ServerVersionInt >= 80100) then
      IndexLenClause := 'pg_relation_size('+SchemaTable+')::bigint'
    else
      IndexLenClause := 'relpages * '+SIZE_KB.ToString;
    Results := GetResults('SELECT *,'+
      ' '+DataLenClause+' AS data_length,'+
      ' '+IndexLenClause+' AS index_length,'+
      ' c.reltuples, obj_description(c.oid) AS comment'+
      ' FROM '+QuoteIdent(InfSch)+'.'+QuoteIdent('tables')+' AS t'+
      ' LEFT JOIN '+QuoteIdent('pg_namespace')+' n ON t.table_schema = n.nspname'+
      ' LEFT JOIN '+QuoteIdent('pg_class')+' c ON n.oid = c.relnamespace AND c.relname=t.table_name'+
      ' WHERE t.'+QuoteIdent('table_schema')+'='+EscapeString(db)  // Use table_schema when using schemata
      );
  except
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
      obj.Name := Results.Col('table_name');
      obj.Created := 0;
      obj.Updated := 0;
      obj.Database := db;
      obj.Schema := Results.Col('table_schema'); // Remove when using schemata
      obj.Comment := Results.Col('comment');
      obj.Rows := StrToInt64Def(Results.Col('reltuples'), obj.Rows);
      obj.DataLen := StrToInt64Def(Results.Col('data_length'), obj.DataLen);
      obj.IndexLen := StrToInt64Def(Results.Col('index_length'), obj.IndexLen);
      obj.Size := obj.DataLen + obj.IndexLen;
      Inc(Cache.FDataSize, Obj.Size);
      Cache.FLargestObjectSize := Max(Cache.FLargestObjectSize, Obj.Size);
      tp := Results.Col('table_type', True);
      if tp = 'VIEW' then
        obj.NodeType := lntView
      else
        obj.NodeType := lntTable;
      Results.Next;
    end;
    FreeAndNil(Results);
  end;

  // Stored functions. No procedures in PostgreSQL.
  // See http://dba.stackexchange.com/questions/2357/what-are-the-differences-between-stored-procedures-and-stored-functions
  try
    Results := GetResults('SELECT '+QuoteIdent('p')+'.'+QuoteIdent('proname')+', '+QuoteIdent('p')+'.'+QuoteIdent('proargtypes')+' '+
      'FROM '+QuoteIdent('pg_catalog')+'.'+QuoteIdent('pg_namespace')+' AS '+QuoteIdent('n')+' '+
      'JOIN '+QuoteIdent('pg_catalog')+'.'+QuoteIdent('pg_proc')+' AS '+QuoteIdent('p')+' ON '+QuoteIdent('p')+'.'+QuoteIdent('pronamespace')+' = '+QuoteIdent('n')+'.'+QuoteIdent('oid')+' '+
      'WHERE '+QuoteIdent('n')+'.'+QuoteIdent('nspname')+'='+EscapeString(db)
      );
  except
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
      obj.Name := Results.Col('proname');
      obj.ArgTypes := Results.Col('proargtypes');
      obj.Database := db;
      obj.NodeType := lntFunction;
      Results.Next;
    end;
    FreeAndNil(Results);
  end;

end;


procedure TSQLiteConnection.FetchDbObjects(db: String; var Cache: TDBObjectList);
var
  obj: TDBObject;
  Results: TDBQuery;
begin
  // Tables, views and procedures
  Results := nil;
  try
    Results := GetResults('SELECT * FROM sqlite_master WHERE type='+EscapeString('table')+' AND name NOT LIKE '+EscapeString('sqlite_%'));
  except
    on E:EDbError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      obj := TDBObject.Create(Self);
      Cache.Add(obj);
      obj.Name := Results.Col('name');
      obj.Created := Now;
      obj.Updated := Now;
      obj.Database := db;
      obj.NodeType := lntTable;
      Results.Next;
    end;
    FreeAndNil(Results);
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
    if Key.IndexType = TTableKey.PRIMARY then
      Result.Assign(Key.Columns);
  end;
  if Result.Count = 0 then begin
    // no primary key available -> 2. round: find a unique key
    for Key in Keys do begin
      if Key.IndexType = TTableKey.UNIQUE then begin
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

  function EvalBool(B: Boolean): String;
  begin
    if B then Result := _('Yes') else Result := _('No');
  end;

begin
  Log(lcDebug, 'Get connection details ...');
  Result := TStringList.Create;
  if Assigned(Parameters) then begin
    Result.Values[_('Host')] := Parameters.Hostname;
    Result.Values[_('Network type')] := Parameters.NetTypeName(True);
  end;
  Ping(False);
  Result.Values[_('Connected')] := EvalBool(FActive);
  if FActive then begin
    Result.Values[_('Real Hostname')] := FRealHostname;
    Result.Values[_('Server OS')] := ServerOS;
    Result.Values[_('Server version')] := FServerVersionUntouched;
    Result.Values[_('Connection port')] := IntToStr(Parameters.Port);
    Result.Values[_('Compressed protocol')] := EvalBool(Parameters.Compressed);
    Result.Values[_('Unicode enabled')] := EvalBool(IsUnicode);
    Result.Values[_('SSL enabled')] := EvalBool(IsSSL);
    if Assigned(FSessionVariables) then
      Result.Values['max_allowed_packet'] := FormatByteNumber(MaxAllowedPacket);
  end;
end;


function TMySQLConnection.ConnectionInfo: TStringList;
var
  Infos, Val: String;
  rx: TRegExpr;
begin
  Result := Inherited;
  Result.Values[f_('Client version (%s)', [FLib.DllFile])] := DecodeApiString(FLib.mysql_get_client_info);
  if FActive then begin
    Infos := DecodeApiString(FLib.mysql_stat(FHandle));
    rx := TRegExpr.Create;
    rx.ModifierG := False;
    rx.Expression := '(\S.*)\:\s+(\S*)(\s+|$)';
    if rx.Exec(Infos) then while True do begin
      Val := rx.Match[2];
      if LowerCase(rx.Match[1]) = 'uptime' then
        Val := FormatTimeNumber(StrToFloatDef(Val, 0), True)
      else
        Val := FormatNumber(Val);
      Result.Values[_(rx.Match[1])] := Val;
      if not rx.ExecNext then
        break;
    end;
    rx.Free;
  end;
end;


function TAdoDBConnection.ConnectionInfo: TStringList;
var
  ConnectionString: String;
  rx: TRegExpr;
begin
  Result := Inherited;
  if FActive then begin
    // clear out password
    ConnectionString := FAdoHandle.ConnectionString;
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.Expression := '(\Wpassword=)([^;]*)';
    ConnectionString := rx.Replace(ConnectionString, '${1}******', True);
    rx.Free;
    Result.Values[_('Connection string')] := ConnectionString;
  end;
end;


function TPgConnection.ConnectionInfo: TStringList;
var
  v: String;
  major, minor, build: Integer;
begin
  Result := Inherited;
  v := IntToStr(FLib.PQlibVersion);
  major := StrToIntDef(Copy(v, 1, Length(v)-4), 0);
  minor := StrToIntDef(Copy(v, Length(v)-3, 2), 0);
  build := StrToIntDef(Copy(v, Length(v)-1, 2), 0);
  Result.Values[f_('Client version (%s)', [FLib.DllFile])] := IntToStr(major) + '.' + IntToStr(minor) + '.' + IntToStr(build);
end;


procedure TDBConnection.ParseViewStructure(CreateCode: String; DBObj: TDBObject;
  var Algorithm, Definer, SQLSecurity, CheckOption, SelectCode: String);
var
  rx: TRegExpr;
  EscQuote: String;
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
    EscQuote := QuoteRegExprMetaChars(FQuoteChar);
    rx.Expression := 'CREATE\s+(OR\s+REPLACE\s+)?'+
      '(ALGORITHM\s*=\s*(\w*)\s*)?'+
      '(DEFINER\s*=\s*(\S+|'+EscQuote+'[^@'+EscQuote+']+'+EscQuote+'@'+EscQuote+'[^'+EscQuote+']+'+EscQuote+')\s+)?'+
      '(SQL\s+SECURITY\s+(\S+)\s+)?'+
      'VIEW\s+[^\(]+\s+'+
      '(\([^\)]+\)\s+)?'+
      'AS\s+(.+)(\s+WITH\s+(\w+\s+)?CHECK\s+OPTION\s*)?$';
    if rx.Exec(CreateCode) then begin
      Algorithm := rx.Match[3];
      Definer := DeQuoteIdent(rx.Match[5], '@');
      SQLSecurity := rx.Match[7];
      if SQLSecurity.IsEmpty then
        SQLSecurity := 'DEFINER';
      CheckOption := Trim(rx.Match[11]);
      SelectCode := rx.Match[9];
    end else
      raise Exception.CreateFmt(_('Regular expression did not match the VIEW code in %s: %s'), ['ParseViewStructure()', CRLF+CRLF+CreateCode]);
    rx.Free;
  end;

end;


procedure TDBConnection.ParseRoutineStructure(Obj: TDBObject; Parameters: TRoutineParamList);
var
  CreateCode, Params, Body, Match: String;
  ParenthesesCount: Integer;
  rx: TRegExpr;
  i: Integer;
  Param: TRoutineParam;
  InLiteral: Boolean;
begin
  // Parse CREATE code of stored function or procedure to detect parameters
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.ModifierG := True;
  // CREATE DEFINER=`root`@`localhost` PROCEDURE `bla2`(IN p1 INT, p2 VARCHAR(20))
  // CREATE DEFINER=`root`@`localhost` FUNCTION `test3`(`?b` varchar(20)) RETURNS tinyint(4)
  // CREATE DEFINER=`root`@`localhost` PROCEDURE `test3`(IN `Param1` int(1) unsigned)
  // MSSQL: CREATE FUNCTION dbo.ConvertToInt(@string nvarchar(255), @maxValue int, @defValue int) RETURNS int

  CreateCode := Obj.CreateCode;

  rx.Expression := '\bDEFINER\s*=\s*(\S+)\s';
  if rx.Exec(CreateCode) then
    Obj.Definer := DequoteIdent(rx.Match[1], '@')
  else
    Obj.Definer := '';

  // Parse parameter list
  ParenthesesCount := 0;
  Params := '';
  InLiteral := False;
  for i:=1 to Length(CreateCode) do begin
    if (CreateCode[i] = ')') and (not InLiteral) then begin
      Dec(ParenthesesCount);
      if ParenthesesCount = 0 then
        break;
    end;
    if Pos(CreateCode[i], FQuoteChars) > 0 then
      InLiteral := not InLiteral;
    if ParenthesesCount >= 1 then
      Params := Params + CreateCode[i];
    if (CreateCode[i] = '(') and (not InLiteral) then
      Inc(ParenthesesCount);
  end;

  // Extract parameters from left part
  rx.Expression := '(^|,)\s*((IN|OUT|INOUT)\s+)?(\S+)\s+([^\s,\(]+(\([^\)]*\))?[^,]*)';
  if rx.Exec(Params) then while true do begin
    Param := TRoutineParam.Create;
    Param.Context := UpperCase(rx.Match[3]);
    if Param.Context = '' then
      Param.Context := 'IN';
    Param.Name := DeQuoteIdent(rx.Match[4]);
    Param.Datatype := Trim(rx.Match[5]);
    Parameters.Add(Param);
    if not rx.ExecNext then
      break;
  end;

  // Right part contains routine body
  Body := Copy(CreateCode, i+1, Length(CreateCode));
  // Remove "RETURNS x" and routine characteristics from body
  // LANGUAGE SQL
  // | [NOT] DETERMINISTIC
  // | { CONTAINS SQL | NO SQL | READS SQL DATA | MODIFIES SQL DATA }
  // | SQL SECURITY { DEFINER | INVOKER }
  // | COMMENT 'string'
  rx.Expression := '^\s*('+
    'RETURNS\s+(\S+(\s+UNSIGNED)?(\s+CHARSET\s+\S+)?(\s+COLLATE\s\S+)?)|'+
    // MySQL function characteristics - see http://dev.mysql.com/doc/refman/5.1/de/create-procedure.html
    'LANGUAGE\s+SQL|'+
    '(NOT\s+)?DETERMINISTIC|'+
    'CONTAINS\s+SQL|'+
    'NO\s+SQL|'+
    'READS\s+SQL\s+DATA|'+
    'MODIFIES\s+SQL\s+DATA|'+
    'SQL\s+SECURITY\s+(DEFINER|INVOKER)|'+
    // MS SQL function options - see http://msdn.microsoft.com/en-us/library/ms186755.aspx
    'AS|'+
    'WITH\s+ENCRYPTION|'+
    'WITH\s+SCHEMABINDING|'+
    'WITH\s+RETURNS\s+NULL\s+ON\s+NULL\s+INPUT|'+
    'WITH\s+CALLED\s+ON\s+NULL\s+INPUT|'+
    'WITH\s+EXECUTE_AS_Clause'+
    ')\s';
  if rx.Exec(Body) then while true do begin
    Match := UpperCase(rx.Match[1]);
    if Pos('RETURNS', Match) = 1 then
      Obj.Returns := rx.Match[2]
    else if Pos('DETERMINISTIC', Match) = 1 then
      Obj.Deterministic := True
    else if Pos('NOT DETERMINISTIC', Match) = 1 then
      Obj.Deterministic := False
    else if (Pos('CONTAINS SQL', Match) = 1) or (Pos('NO SQL', Match) = 1) or (Pos('READS SQL DATA', Match) = 1) or (Pos('MODIFIES SQL DATA', Match) = 1) then
      Obj.DataAccess := rx.Match[1]
    else if Pos('SQL SECURITY', Match) = 1 then
      Obj.Security := rx.Match[7];


    Delete(Body, 1, rx.MatchLen[0]);
    if not rx.Exec(Body) then
      break;
  end;
  Obj.Comment := ExtractLiteral(Body, 'COMMENT');
  Obj.Body := TrimLeft(Body);
  rx.Free;
end;


procedure TDBConnection.PurgePrefetchResults;
begin
  // Remove cached results
  if Assigned(FPrefetchResults) then
    FreeAndNil(FPrefetchResults);
end;


function TDBConnection.ApplyLimitClause(QueryType, QueryBody: String; Limit, Offset: Int64): String;
begin
  QueryType := UpperCase(QueryType);
  Result := QueryType + ' ';
  case FParameters.NetTypeGroup of
    ngMSSQL: begin
      if QueryType = 'UPDATE' then begin
        // TOP(x) clause for UPDATES + DELETES introduced in MSSQL 2005
        if ServerVersionInt >= 900 then
          Result := Result + 'TOP('+IntToStr(Limit)+') ';
      end else if QueryType = 'SELECT' then
        Result := Result + 'TOP '+IntToStr(Limit)+' ';
      Result := Result + QueryBody;
    end;
    ngMySQL: begin
      Result := Result + QueryBody + ' LIMIT ';
      if Offset > 0 then
        Result := Result + IntToStr(Offset) + ', ';
      Result := Result + IntToStr(Limit);
    end;
    ngPgSQL: begin
      if QueryType = 'SELECT' then begin
        Result := Result + QueryBody + ' LIMIT ' + IntToStr(Limit);
        if Offset > 0 then
          Result := Result + ' OFFSET ' + IntToStr(Offset);
      end else
        Result := Result + QueryBody;
    end;
    ngSQLite: begin
      // LIMIT supported only in SELECT queries
      // For UPDATEs and DELETEs only if we would compile sqlite library with SQLITE_ENABLE_UPDATE_DELETE_LIMIT compile flag
      Result := Result + QueryBody;
      if Result.StartsWith('SELECT') then begin
        Result := Result + ' LIMIT ';
        if Offset > 0 then
          Result := Result + IntToStr(Offset) + ', ';
        Result := Result + IntToStr(Limit);
      end;
    end;
  end;
end;


function TDBConnection.LikeClauseTail: String;
begin
  case FParameters.NetTypeGroup of
    ngMSSQL: Result := ' ESCAPE ' + EscapeString('\');
    else Result := '';
  end;
end;



{ TMySQLQuery }

constructor TDBQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := AOwner as TDbConnection;
  FRecNo := -1;
  FRecordCount := 0;
  FColumnNames := TStringList.Create;
  FColumnNames.CaseSensitive := False;
  FColumnOrgNames := TStringList.Create;
  FColumnOrgNames.CaseSensitive := False;
  FStoreResult := True;
  FDBObject := nil;
  FFormatSettings := TFormatSettings.Create('en-US');
end;


constructor TMySQLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // suspicous state here - what type has FConnection now?
  FConnection := AOwner as TMySQLConnection;
end;


constructor TPgQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := AOwner as TPgConnection;
end;


constructor TSQLiteQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := AOwner as TSQLiteConnection;
end;


destructor TDBQuery.Destroy;
begin
  FreeAndNil(FColumnNames);
  FreeAndNil(FColumnOrgNames);
  FreeAndNil(FColumns);
  FreeAndNil(FKeys);
  FreeAndNil(FUpdateData);
  if FDBObject <> nil then
    FDBObject.Free;
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
  if HasResult and (FConnection <> nil) and (FConnection.Active) then begin
    for i:=Low(FResultList) to High(FResultList) do
      FConnection.Lib.mysql_free_result(FResultList[i]);
  end;
  SetLength(FResultList, 0);
  inherited;
end;


destructor TAdoDBQuery.Destroy;
var
  i: Integer;
begin
  if HasResult and (FConnection <> nil) and (FConnection.Active) then begin
    for i:=Low(FResultList) to High(FResultList) do begin
      FResultList[i].Close;
      FResultList[i].Free;
    end;
  end;
  SetLength(FResultList, 0);
  inherited;
end;


destructor TPGQuery.Destroy;
var
  i: Integer;
begin
  if HasResult and (FConnection <> nil) and (FConnection.Active) then begin
    for i:=Low(FResultList) to High(FResultList) do
      FConnection.Lib.PQclear(FResultList[i]);
  end;
  SetLength(FResultList, 0);
  inherited;
end;


destructor TSQLiteQuery.Destroy;
var
  i: Integer;
begin
  if HasResult and (FConnection <> nil) and (FConnection.Active) then begin
    for i:=Low(FResultList) to High(FResultList) do
      FResultList[i].Free;
  end;
  SetLength(FResultList, 0);
  inherited;
end;


procedure TMySQLQuery.Execute(AddResult: Boolean=False; UseRawResult: Integer=-1);
var
  i, j, NumFields, NumResults: Integer;
  Field: PMYSQL_FIELD;
  IsBinary: Boolean;
  LastResult: PMYSQL_RES;
begin
  // Execute a query, or just take over one of the last result pointers
  if UseRawResult = -1 then begin
    Connection.Query(FSQL, FStoreResult);
    UseRawResult := 0;
  end;
  if Connection.ResultCount > UseRawResult then begin
    LastResult := TMySQLConnection(Connection).LastRawResults[UseRawResult]
  end else begin
    LastResult := nil;
  end;
  if AddResult and (Length(FResultList) = 0) then
    AddResult := False;
  if AddResult then
    NumResults := Length(FResultList)+1
  else begin
    for i:=Low(FResultList) to High(FResultList) do begin
      FConnection.Lib.mysql_free_result(FResultList[i]);
    end;
    NumResults := 1;
    FRecordCount := 0;
    FAutoIncrementColumn := -1;
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
      NumFields := FConnection.Lib.mysql_num_fields(LastResult);
      SetLength(FColumnTypes, NumFields);
      SetLength(FColumnLengths, NumFields);
      SetLength(FColumnFlags, NumFields);
      FColumnNames.Clear;
      FColumnOrgNames.Clear;
      for i:=0 to NumFields-1 do begin
        Field := FConnection.Lib.mysql_fetch_field_direct(LastResult, i);
        FColumnNames.Add(Connection.DecodeAPIString(Field.name));
        if Connection.ServerVersionInt >= 40100 then
          FColumnOrgNames.Add(Connection.DecodeAPIString(Field.org_name))
        else
          FColumnOrgNames.Add(Connection.DecodeAPIString(Field.name));
        FColumnFlags[i] := Field.flags;
        FColumnTypes[i] := FConnection.Datatypes[0];
        if (Field.flags and AUTO_INCREMENT_FLAG) = AUTO_INCREMENT_FLAG then
          FAutoIncrementColumn := i;
        for j:=0 to High(FConnection.Datatypes) do begin
          if (Field.flags and ENUM_FLAG) = ENUM_FLAG then begin
            if FConnection.Datatypes[j].Index = dtEnum then
              FColumnTypes[i] := FConnection.Datatypes[j];
          end else if (Field.flags and SET_FLAG) = SET_FLAG then begin
            if FConnection.Datatypes[j].Index = dtSet then
              FColumnTypes[i] := FConnection.Datatypes[j];
          end else if Field._type = Cardinal(FConnection.Datatypes[j].NativeType) then begin
            // Text and Blob types share the same constants (see FIELD_TYPEs)
            // See http://dev.mysql.com/doc/refman/5.7/en/c-api-data-structures.html
            if Connection.IsUnicode then
              IsBinary := Field.charsetnr = COLLATION_BINARY
            else
              IsBinary := (Field.flags and BINARY_FLAG) = BINARY_FLAG;
            if IsBinary and (FConnection.Datatypes[j].Index in [dtChar..dtLongtext]) then
              continue;
            FColumnTypes[i] := FConnection.Datatypes[j];
            break;
          end;
        end;
        FConnection.Log(lcDebug, 'Detected column type for '+FColumnNames[i]+': '+FColumnTypes[i].Name);
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
  i, j, NumFields, NumResults: Integer;
  TypeIndex: TDBDatatypeIndex;
  LastResult: TAdoQuery;
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
  end else begin
    LastResult := nil;
  end;
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
    FAutoIncrementColumn := -1;
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
          ftInteger:
            TypeIndex := dtInt;
          ftAutoInc: begin
            TypeIndex := dtInt;
            FAutoIncrementColumn := i;
          end;
          ftLargeint:
            TypeIndex := dtBigInt;
          ftBCD, ftFMTBcd:
            TypeIndex := dtDecimal;
          ftFixedChar, ftFixedWideChar:
            TypeIndex := dtChar;
          ftString, ftWideString, ftBoolean, ftGuid:
            TypeIndex := dtVarchar;
          ftMemo, ftWideMemo:
            TypeIndex := dtText;
          ftBlob, ftVariant:
            TypeIndex := dtMediumBlob;
          ftBytes:
            TypeIndex := dtBinary;
          ftVarBytes:
            TypeIndex := dtVarbinary;
          ftFloat:
            TypeIndex := dtFloat;
          ftDate:
            TypeIndex := dtDate;
          ftTime:
            TypeIndex := dtTime;
          ftDateTime:
            TypeIndex := dtDateTime;
          //ftTimeStampOffset: // this is NOT data type DATETIMEOFFSET
          //  TypeIndex := dtDatetime;
          else
            raise EDbError.CreateFmt(_('Unknown data type for column #%d - %s: %d'), [i, FColumnNames[i], Integer(LastResult.Fields[i].DataType)]);
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


procedure TPGQuery.Execute(AddResult: Boolean=False; UseRawResult: Integer=-1);
var
  i, NumFields, NumResults: Integer;
  FieldTypeOID: POid;
  LastResult: PPGresult;
begin
  if UseRawResult = -1 then begin
    Connection.Query(FSQL, FStoreResult);
    UseRawResult := 0;
  end;
  if Connection.ResultCount > UseRawResult then begin
    LastResult := TPGConnection(Connection).LastRawResults[UseRawResult]
  end else begin
    LastResult := nil;
  end;
  if AddResult and (Length(FResultList) = 0) then
    AddResult := False;
  if AddResult then
    NumResults := Length(FResultList)+1
  else begin
    for i:=Low(FResultList) to High(FResultList) do begin
      FConnection.Lib.PQclear(FResultList[i]);
    end;
    NumResults := 1;
    FRecordCount := 0;
    FAutoIncrementColumn := -1;
    FEditingPrepared := False;
  end;
  if LastResult <> nil then begin
    Connection.Log(lcDebug, 'Result #'+IntToStr(NumResults)+' fetched.');
    SetLength(FResultList, NumResults);
    FResultList[NumResults-1] := LastResult;
    FRecordCount := FRecordCount + FConnection.Lib.PQntuples(LastResult);
  end;
  if not AddResult then begin
    if HasResult then begin
      // FCurrentResults is normally done in SetRecNo, but never if result has no rows
      FCurrentResults := LastResult;
      NumFields := FConnection.Lib.PQnfields(LastResult);
      SetLength(FColumnTypes, NumFields);
      SetLength(FColumnLengths, NumFields);
      SetLength(FColumnFlags, NumFields);
      FColumnNames.Clear;
      FColumnOrgNames.Clear;
      for i:=0 to NumFields-1 do begin
        FColumnNames.Add(Connection.DecodeAPIString(FConnection.Lib.PQfname(LastResult, i)));
        FColumnOrgNames.Add(FColumnNames[FColumnNames.Count-1]);
        FieldTypeOID := FConnection.Lib.PQftype(LastResult, i);
        FColumnTypes[i] := FConnection.GetDatatypeByNativeType(FieldTypeOID, FColumnNames[FColumnNames.Count-1]);
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


procedure TSQLiteQuery.Execute(AddResult: Boolean=False; UseRawResult: Integer=-1);
var
  i, NumFields, NumResults: Integer;
  LastResult: TSQLiteGridRows;
  ColName, ColOrgName, DataTypeStr: String;
  StepResult: Integer;
begin
  if UseRawResult = -1 then begin
    Connection.Query(FSQL, FStoreResult);
    UseRawResult := 0;
  end;
  if Connection.ResultCount > UseRawResult then begin
    LastResult := TSQLiteConnection(Connection).LastRawResults[UseRawResult];
  end else begin
    LastResult := nil;
  end;
  if AddResult and (Length(FResultList) = 0) then
    AddResult := False;
  if AddResult then
    NumResults := Length(FResultList)+1
  else begin
    for i:=Length(FResultList)-1 downto 0 do begin
      FResultList[i].Free;
    end;
    NumResults := 1;
    FRecordCount := 0;
    FAutoIncrementColumn := -1;
    FEditingPrepared := False;
  end;
  if LastResult <> nil then begin
    Connection.Log(lcDebug, 'Result #'+IntToStr(NumResults)+' fetched.');
    SetLength(FResultList, NumResults);
    FResultList[NumResults-1] := LastResult;
    FRecordCount := FRecordCount + LastResult.Count;
  end;
  if not AddResult then begin
    if HasResult then begin
      // FCurrentResults is normally done in SetRecNo, but never if result has no rows
      FCurrentResults := LastResult;
      NumFields := FConnection.Lib.sqlite3_column_count(LastResult.Statement);
      SetLength(FColumnTypes, NumFields);
      SetLength(FColumnLengths, NumFields);
      SetLength(FColumnFlags, NumFields);
      FColumnNames.Clear;
      FColumnOrgNames.Clear;
      StepResult := -1;
      for i:=0 to NumFields-1 do begin
        ColName := FConnection.DecodeAPIString(FConnection.Lib.sqlite3_column_name(LastResult.Statement, i));
        FColumnNames.Add(ColName);
        ColOrgName := FConnection.DecodeAPIString(FConnection.Lib.sqlite3_column_origin_name(LastResult.Statement, i));
        FColumnOrgNames.Add(ColOrgName);
        DataTypeStr := FConnection.DecodeAPIString(FConnection.Lib.sqlite3_column_decltype(LastResult.Statement, i));
        if DataTypeStr.IsEmpty then begin
          if StepResult = -1 then
            StepResult := FConnection.Lib.sqlite3_step(LastResult.Statement);
          if StepResult = SQLITE_ROW then begin
            case FConnection.Lib.sqlite3_column_type(LastResult.Statement, i) of
              SQLITE_INTEGER: DataTypeStr := 'INTEGER';
              SQLITE_FLOAT: DataTypeStr := 'FLOAT';
              SQLITE_BLOB: DataTypeStr := 'BLOB';
              SQLITE3_TEXT: DataTypeStr := 'TEXT';
              // SQLITE_NULL gets "unknown"
            end;
          end else begin
            // No row available, fall back to TEXT
            DataTypeStr := 'TEXT';
          end;
        end;
        FColumnTypes[i] := FConnection.GetDatatypeByName(DataTypeStr, False);
      end;
      if StepResult <> -1 then begin
        FConnection.Lib.sqlite3_reset(LastResult.Statement);
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


procedure TDBQuery.SetDBObject(Value: TDBObject);
begin
  // Assign values from outside to a new tdbobject
  FDBObject := TDBObject.Create(FConnection);
  FDBObject.Assign(Value);
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
  Row: TGridRow;
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
            FConnection.Lib.mysql_data_seek(FCurrentResults, WantedLocalRecNo);
          FCurrentRow := FConnection.Lib.mysql_fetch_row(FCurrentResults);
          FCurrentUpdateRow := nil;
          // Remember length of column contents. Important for Col() so contents of cells with #0 chars are not cut off
          LengthPointer := FConnection.Lib.mysql_fetch_lengths(FCurrentResults);
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
  Row: TGridRow;
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
      try
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
      except
        // Catch broken connection
        on E:EOleException do begin
          FConnection.Active := False;
          FConnection.Log(lcError, E.Message);
        end;
      end;
    end;

    FRecNo := Value;
    FEof := False;
  end;
end;


procedure TPGQuery.SetRecNo(Value: Int64);
var
  i, j: Integer;
  RowFound: Boolean;
  Row: TGridRow;
  NumRows: Int64;
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
        Inc(NumRows, FConnection.Lib.PQntuples(FResultList[i]));
        if NumRows > Value then begin
          FCurrentResults := FResultList[i];
          FRecNoLocal := FConnection.Lib.PQntuples(FCurrentResults)-(NumRows-Value);
          FCurrentUpdateRow := nil;
          for j:=Low(FColumnLengths) to High(FColumnLengths) do
            FColumnLengths[j] := FConnection.Lib.PQgetlength(FCurrentResults, FRecNoLocal, j);
          break;
        end;
      end;
    end;

    FRecNo := Value;
    FEof := False;
  end;
end;


procedure TSQLiteQuery.SetRecNo(Value: Int64);
var
  i: Integer;
  RowFound: Boolean;
  Row: TGridRow;
  NumRows: Int64;
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
        Inc(NumRows, FResultList[i].Count);
        if NumRows > Value then begin
          FCurrentResults := FResultList[i];
          FRecNoLocal := FResultList[i].Count-(NumRows-Value);
          FCurrentUpdateRow := nil;
          //for j:=Low(FColumnLengths) to High(FColumnLengths) do
          //  FColumnLengths[j] := FConnection.Lib.PQgetlength(FCurrentResults, FRecNoLocal, j);
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


function TDBQuery.ColumnExists(Column: Integer): Boolean;
begin
  // Check if given column exists in current row
  // Prevents crash when cancelling new row insertion
  Result := FConnection.Active and (Column > -1) and (Column < ColumnCount);
  if Result and FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
    Result := FCurrentUpdateRow.Count > Column;
  end;
end;


function TDBQuery.GetColBinData(Column: Integer; var baData: TBytes): Boolean;
begin
  Raise EDbError.Create(SNotImplemented);
end;


function TMySQLQuery.GetColBinData(Column: Integer; var baData: TBytes): Boolean;
var
  AnsiStr: AnsiString;
begin
  Result := False;

  if ColumnExists(Column) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      // Row was edited and only valid in a TGridRow
      AnsiStr := AnsiString(FCurrentUpdateRow[Column].NewText);
    end else begin
      // The normal case: Fetch cell from mysql result
      SetString(AnsiStr, FCurrentRow[Column], FColumnLengths[Column]);
    end;

    if Datatype(Column).Category in [dtcBinary, dtcSpatial] then begin
      SetLength(baData, Length(AnsiStr));
      CopyMemory(baData, @AnsiStr[1], Length(AnsiStr));
      Result := True;
    end;
  end;
end;


function TMySQLQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
var
  AnsiStr: AnsiString;
  BitString: String;
  NumBit: Integer;
  ByteVal: Byte;
  c: Char;
  Field: PMYSQL_FIELD;
begin
  if ColumnExists(Column) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      // Row was edited and only valid in a TGridRow
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
        Field := FConnection.Lib.mysql_fetch_field_direct(FCurrentResults, column);
        // FConnection.Log(lcInfo, Field.name+':  def: '+field.def+'  length: '+inttostr(field.length)+'  max_length: '+inttostr(field.max_length)+'  decimals: '+inttostr(field.decimals));
        for c in Result do begin
          ByteVal := Byte(c);
          BitString := '';
          for NumBit:=0 to 7 do begin
            if (ByteVal shr NumBit and $1) = $1 then
              BitString := BitString + '1'
            else
              BitString := BitString + '0';
            if Length(BitString) >= Field.length then
              break;
          end;
          if Length(BitString) >= Field.length then
            break;
        end;
        Result := ReverseString(BitString);
      end;

    end;
  end else if not IgnoreErrors then
    Raise EDbError.CreateFmt(_(MsgInvalidColumn), [Column, ColumnCount, RecordCount]);
end;


function TAdoDBQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
begin
  if ColumnExists(Column) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      Result := FCurrentUpdateRow[Column].NewText;
    end else begin
      try
        case Datatype(Column).Category of
          dtcReal:
            Result := FloatToStr(FCurrentResults.Fields[Column].AsExtended, FFormatSettings);
          dtcTemporal:
            Result := FormatDateTime(Datatype(Column).Format, FCurrentResults.Fields[Column].AsFloat);
          else
            Result := FCurrentResults.Fields[Column].AsString;
        end;
      except
        Result := String(FCurrentResults.Fields[Column].AsAnsiString);
      end;
      if Datatype(Column).Index = dtBit then begin
        if UpperCase(Result) = 'TRUE' then
          Result := '1'
        else
          Result := '0';
      end
    end;
  end else if not IgnoreErrors then
    Raise EDbError.CreateFmt(_(MsgInvalidColumn), [Column, ColumnCount, RecordCount]);
end;


function TPGQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
var
  AnsiStr: AnsiString;
begin
  if ColumnExists(Column) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      Result := FCurrentUpdateRow[Column].NewText;
    end else begin
      SetString(AnsiStr, FConnection.Lib.PQgetvalue(FCurrentResults, FRecNoLocal, Column), FColumnLengths[Column]);
      if Datatype(Column).Category in [dtcBinary, dtcSpatial] then
        Result := String(AnsiStr)
      else if Datatype(Column).Index = dtbool then
        if AnsiStr='t' then Result := 'true' else Result := 'false'
      else
        Result := Connection.DecodeAPIString(AnsiStr);
    end;
  end else if not IgnoreErrors then
    Raise EDbError.CreateFmt(_(MsgInvalidColumn), [Column, ColumnCount, RecordCount]);
end;


function TSQLiteQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
begin
  if ColumnExists(Column) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      Result := FCurrentUpdateRow[Column].NewText;
    end else begin
      Result := FCurrentResults[RecNo][Column].OldText;
    end;
  end else if not IgnoreErrors then
    Raise EDbError.CreateFmt(_(MsgInvalidColumn), [Column, ColumnCount, RecordCount]);
end;


function TDBQuery.Col(ColumnName: String; IgnoreErrors: Boolean=False): String;
var
  idx: Integer;
begin
  // ColumnNames is case insensitive, so we can select wrong cased columns in MariaDB 10.4
  // See #599
  idx := ColumnNames.IndexOf(ColumnName);
  if idx > -1 then
    Result := Col(idx)
  else if not IgnoreErrors then
    Raise EDbError.CreateFmt(_('Column "%s" not available.'), [ColumnName]);
end;


function TDBQuery.ColumnLengths(Column: Integer): Int64;
begin
  Result := FColumnLengths[Column];
end;


function TDBQuery.HexValue(Column: Integer; IgnoreErrors: Boolean=False): String;
var
    baData: TBytes;
begin
  // Return a binary column value as hex AnsiString
  if FConnection.Parameters.IsAnyMysql then begin
    GetColBinData(Column, baData);
    Result := HexValue(baData);
  end else
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
    Result := Connection.EscapeString('');
  end else begin
    SetLength(Result, BinLen*2);
    BinToHex(PAnsiChar(Ansi), PChar(Result), BinLen);
    Result := '0x' + Result;
  end;
end;

function TDBQuery.HexValue(var ByteData: TBytes): String;
var
  BinLen: Integer;
  Ansi: AnsiString;
begin
  BinLen := Length(ByteData);
  SetString(Ansi, PAnsiChar(ByteData), BinLen);
  if BinLen = 0 then begin
    Result := Connection.EscapeString('');
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
      dtText, dtBlob: begin
        case FConnection.Parameters.NetTypeGroup of
          ngMySQL: Result := 65535;
          ngMSSQL: Result := MaxInt;
          ngPgSQL: Result := High(Int64);
        end;
      end;
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
  if Assigned(ColAttr) then case ColAttr.DataType.Index of
    dtEnum, dtSet:
      Result.DelimitedText := ColAttr.LengthSet;
    dtBool:
      Result.DelimitedText := 'true,false';
  end;
end;


function TDBQuery.ColAttributes(Column: Integer): TTableColumn;
var
  i: Integer;
begin
  Result := nil;
  if (Column < 0) or (Column >= FColumnOrgNames.Count) then
    raise EDbError.CreateFmt(_('Column #%s not available.'), [IntToStr(Column)]);
  if FColumns <> nil then begin
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


function TPGQuery.ColIsPrimaryKeyPart(Column: Integer): Boolean;
begin
  Result := False;
end;


function TSQLiteQuery.ColIsPrimaryKeyPart(Column: Integer): Boolean;
var
  MetaResult: Integer;
  TableNm, ColumnNm: String;
  DataType, CollSeq: PAnsiChar;
  NotNull, PrimaryKey, Autoinc: Integer;
begin
  Result := False;
  TableNm := TableName(Column);
  ColumnNm := FColumnOrgNames[Column];
  if not TableNm.IsEmpty then begin
    MetaResult := FConnection.Lib.sqlite3_table_column_metadata(FConnection.FHandle,
      PAnsiChar(Utf8Encode(FConnection.Database)),
      PAnsiChar(Utf8Encode(TableNm)),
      PAnsiChar(Utf8Encode(ColumnNm)),
      DataType, CollSeq, NotNull, PrimaryKey, Autoinc
      );
    if MetaResult <> SQLITE_ERROR then begin
      Result := PrimaryKey.ToBoolean;
    end;
  end;
end;


function TMySQLQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
begin
  Result := (FColumnFlags[Column] and UNIQUE_KEY_FLAG) = UNIQUE_KEY_FLAG;
end;


function TAdoDBQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
begin
  Result := False;
end;


function TPGQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
begin
  Result := False;
end;


function TSQLiteQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
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


function TPGQuery.ColIsKeyPart(Column: Integer): Boolean;
begin
  Result := False;
end;


function TSQLiteQuery.ColIsKeyPart(Column: Integer): Boolean;
begin
  Result := False;
end;


function TDBQuery.ColIsVirtual(Column: Integer): Boolean;
var
  Col: TTableColumn;
begin
  Result := False;
  Col := ColAttributes(Column);
  if Col <> nil then begin
    Result := not Col.Virtuality.IsEmpty;
  end;
end;


function TMySQLQuery.IsNull(Column: Integer): Boolean;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsNull
  else
    Result := FCurrentRow[Column] = nil;
end;


function TDBQuery.IsNull(Column: String): Boolean;
var
  i, idx: Integer;
begin
  idx := -1;
  for i:=0 to FColumnNames.Count-1 do begin
    if CompareText(Column, FColumnNames[i]) = 0 then begin
      idx := i;
      break;
    end;
  end;
  if idx > -1 then
    Result := IsNull(idx)
  else
    Result := True;
end;


function TAdoDBQuery.IsNull(Column: Integer): Boolean;
begin
  Result := False;
  // Catch broken connection
  if FConnection.Active then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then
      Result := FCurrentUpdateRow[Column].NewIsNull
    else begin
      try
        Result := FCurrentResults.Fields[Column].IsNull;
      except
        // Silence error: "Multiple-step operation generated errors. Check each status value."
        // @see #496
        on E:EOleException do;
      end;
    end;
  end;
end;


function TPGQuery.IsNull(Column: Integer): Boolean;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsNull
  else
    Result := FConnection.Lib.PQgetisnull(FCurrentResults, FRecNoLocal, Column) = 1;
end;


function TSQLiteQuery.IsNull(Column: Integer): Boolean;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsNull
  else
    Result := FCurrentResults[RecNo][Column].OldIsNull;
end;


function TDBQuery.IsFunction(Column: Integer): Boolean;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsFunction
  else
    Result := False;
end;


function TMySQLQuery.HasResult: Boolean;
begin
  Result := Length(FResultList) > 0;
end;


function TAdoDBQuery.HasResult: Boolean;
begin
  Result := Length(FResultList) > 0;
end;


function TPGQuery.HasResult: Boolean;
begin
  Result := Length(FResultList) > 0;
end;


function TSQLiteQuery.HasResult: Boolean;
begin
  Result := Length(FResultList) > 0;
end;


procedure TDBQuery.PrepareColumnAttributes;
var
  DB: String;
  DBObjects: TDBObjectList;
  LObj, Obj: TDBObject;
begin
  // Try to fetch column names and keys
  // This is probably a VIEW, so column names need to be fetched differently
  Obj := nil;
  if FDBObject <> nil then
    Obj := FDBObject
  else begin
    DB := DatabaseName;
    if DB = '' then
      DB := Connection.Database;
    DBObjects := Connection.GetDBObjects(DB);
    for LObj in DBObjects do begin
      if (LObj.NodeType in [lntTable, lntView]) and Connection.IdentifierEquals(LObj.Name, TableName) then begin
        Obj := LObj;
        break;
      end;
    end;
    if Obj = nil then
      raise EDbError.Create(f_('Could not find table or view %s.%s. Please refresh database tree.', [DB, TableName]));
  end;
  // Obj.NodeType must be lntTable or lntView here, otherwise we get no columns or keys
  FColumns := Obj.TableColumns;
  FKeys := Obj.TableKeys;
  FForeignKeys := Obj.TableForeignKeys;
end;


procedure TDBQuery.PrepareEditing;
begin
  // Try to fetch column names and keys and init update data
  if FEditingPrepared then
    Exit;
  PrepareColumnAttributes;
  FreeAndNil(FUpdateData);
  FUpdateData := TGridRows.Create;
  FEditingPrepared := True;
end;


procedure TDBQuery.DeleteRow;
var
  sql: String;
  IsVirtual: Boolean;
begin
  // Delete current row from result
  PrepareEditing;
  IsVirtual := Assigned(FCurrentUpdateRow) and FCurrentUpdateRow.Inserted;
  if not IsVirtual then begin
    sql := GridQuery('DELETE', 'FROM ' + QuotedDbAndTableName + ' WHERE ' + GetWhereClause);
    Connection.Query(sql);
    if Connection.RowsAffected = 0 then
      raise EDbError.Create(FormatNumber(Connection.RowsAffected)+' rows deleted when that should have been 1.');
  end;
  if Assigned(FCurrentUpdateRow) then begin
    FUpdateData.Remove(FCurrentUpdateRow);
    FCurrentUpdateRow := nil;
    FRecNo := -1;
  end;
end;


function TDBQuery.InsertRow: Int64;
var
  Row, OtherRow: TGridRow;
  c: TGridValue;
  i: Integer;
  ColAttr: TTableColumn;
  InUse: Boolean;
begin
  // Add new row and return row number
  PrepareEditing;
  Row := TGridRow.Create(True);
  for i:=0 to ColumnCount-1 do begin
    c := TGridValue.Create;
    Row.Add(c);
    c.OldText := '';
    c.OldIsFunction := False;
    c.OldIsNull := False;
    ColAttr := ColAttributes(i);
    if Assigned(ColAttr) then begin
      c.OldIsNull := ColAttr.DefaultType in [cdtNull, cdtAutoInc];
      if ColAttr.DefaultType in [cdtText] then
        c.OldText := FConnection.UnescapeString(ColAttr.DefaultText);
    end;
    c.NewText := c.OldText;
    c.NewIsFunction := c.OldIsFunction;
    c.NewIsNull := c.OldIsNull;
    c.Modified := False;
  end;
  Row.Inserted := True;
  // Find highest unused recno of inserted rows and use that for this row
  // Important: do not raise higher than what TVirtualStringTree.RootNodeCount can hold!
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


procedure TDBQuery.SetCol(Column: Integer; NewText: String; Null: Boolean; IsFunction: Boolean);
begin
  PrepareEditing;
  if not Assigned(FCurrentUpdateRow) then begin
    CreateUpdateRow;
    EnsureFullRow(False);
  end;
  FCurrentUpdateRow[Column].NewIsNull := Null;
  FCurrentUpdateRow[Column].NewIsFunction := IsFunction;
  FCurrentUpdateRow[Column].NewText := IfThen(Null, '', NewText);
  FCurrentUpdateRow[Column].Modified := (FCurrentUpdateRow[Column].NewText <> FCurrentUpdateRow[Column].OldText) or
    (FCurrentUpdateRow[Column].NewIsNull <> FCurrentUpdateRow[Column].OldIsNull) or
    (FCurrentUpdateRow[Column].NewIsFunction <> FCurrentUpdateRow[Column].OldIsFunction)
    ;
  // TODO: check if column allows NULL, otherwise force .Modified
end;


procedure TDBQuery.CreateUpdateRow;
var
  i: Integer;
  c: TGridValue;
  Row: TGridRow;
begin
  Row := TGridRow.Create(True);
  for i:=0 to ColumnCount-1 do begin
    c := TGridValue.Create;
    Row.Add(c);
    c.OldText := Col(i);
    c.NewText := c.OldText;
    c.OldIsNull := IsNull(i);
    c.NewIsNull := c.OldIsNull;
    c.OldIsFunction := False;
    c.NewIsFunction := c.OldIsFunction;
    c.Modified := False;
  end;
  Row.Inserted := False;
  Row.RecNo := RecNo;
  FCurrentUpdateRow := Row;
  FUpdateData.Add(FCurrentUpdateRow);
end;


function TDBQuery.EnsureFullRow(Refresh: Boolean): Boolean;
var
  i: Integer;
  sql: String;
  Data: TDBQuery;
begin
  // Load full column values
  Result := True;
  if Refresh or (not HasFullData) then try
    PrepareEditing;
    for i:=0 to FColumnOrgNames.Count-1 do begin
      if sql <> '' then
        sql := sql + ', ';
      sql := sql + Connection.QuoteIdent(FColumnOrgNames[i]);
    end;
    sql := sql + ' FROM '+QuotedDbAndTableName+' WHERE '+GetWhereClause;
    sql := GridQuery('SELECT', sql);
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
        FCurrentUpdateRow[i].OldIsFunction := False;
        FCurrentUpdateRow[i].NewIsFunction := FCurrentUpdateRow[i].OldIsFunction;
      end;
      Data.Free;
    end;
  except on E:EDbError do
    Result := False;
  end;
end;


function TDBQuery.HasFullData: Boolean;
var
  Val: String;
  i: Integer;
begin
  Result := True;
  // In case we created a update-row we know for sure that we already loaded full contents
  if Assigned(FCurrentUpdateRow) then
    Result := True
  else for i:=0 to ColumnCount-1 do begin
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
  Row: TGridRow;
  Cell: TGridValue;
  sqlUpdate, sqlInsertColumns, sqlInsertValues, Val: String;
  RowModified: Boolean;
  ColAttr: TTableColumn;
begin
  Result := True;
  if not FEditingPrepared then
    raise EDbError.Create(_('Internal error: Cannot post modifications before editing was prepared.'));

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
      else if Cell.NewIsFunction then
        Val := Cell.NewText
      else case Datatype(i).Category of
        dtcInteger, dtcReal: begin
          Val := Connection.EscapeString(Cell.NewText);
          if (Datatype(i).Index = dtBit) and FConnection.Parameters.IsAnyMySQL then
            Val := 'b' + Val;
        end;
        dtcBinary, dtcSpatial:
          Val := HexValue(Cell.NewText);
        else begin
          if Datatype(i).Index in [dtNchar, dtNvarchar, dtNtext] then
            Val := 'N' + Connection.EscapeString(Cell.NewText)
          else if Datatype(i).Category = dtcTemporal then
            Val := Connection.EscapeString(Connection.GetDateTimeValue(Cell.NewText, Datatype(i).Index))
          else
            Val := Connection.EscapeString(Cell.NewText);
        end;
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
        sqlUpdate := GridQuery('UPDATE', sqlUpdate);
        Connection.Query(sqlUpdate);
        if Connection.RowsAffected = 0 then begin
          raise EDbError.Create(FormatNumber(Connection.RowsAffected)+' rows updated when that should have been 1.');
          Result := False;
        end;
      end;
      // Reset modification flags
      for i:=0 to ColumnCount-1 do begin
        Cell := Row[i];
        Cell.OldText := Cell.NewText;
        Cell.OldIsNull := Cell.NewIsNull;
        Cell.OldIsFunction := False;
        Cell.NewIsFunction := False;
        Cell.Modified := False;
      end;
      Row.Inserted := False;
      // Reload real row data from server if keys allow that
      EnsureFullRow(True);
    except
      on E:EDbError do begin
        Result := False;
        ErrorDialog(E.Message);
      end;
    end;

  end;
end;


procedure TDBQuery.DiscardModifications;
var
  x: Integer;
  c: TGridValue;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
    if FCurrentUpdateRow.Inserted then begin
      FUpdateData.Remove(FCurrentUpdateRow);
      FRecNo := -1;
    end else for x:=0 to FCurrentUpdateRow.Count-1 do begin
      c := FCurrentUpdateRow[x];
      c.NewText := c.OldText;
      c.NewIsNull := c.OldIsNull;
      c.NewIsFunction := c.OldIsFunction;
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
  // Find and return name of database of current query
  if FDBObject <> nil then begin
    Result := FDBObject.Database;
  end else begin
    // Return first available Field.db property, or just the current database as fallback.
    // For a view in db1 selecting from db2, this returns db2, which triggers errors in GetCreateViewCode!
    for i:=0 to ColumnCount-1 do begin
      Field := FConnection.Lib.mysql_fetch_field_direct(FCurrentResults, i);
      if Field.db <> '' then begin
        Result := Connection.DecodeAPIString(Field.db);
        break;
      end;
    end;
    if Result = '' then
      Result := Connection.Database;
  end;
end;


function TAdoDBQuery.DatabaseName: String;
begin
  Result := Connection.Database;
end;


function TPGQuery.DatabaseName: String;
begin
  // TODO
  Result := Connection.Database;
end;


function TSQLiteQuery.DatabaseName: String;
begin
  // TODO
  Result := Connection.Database;
end;


function TDBQuery.TableName: String;
var
  i: Integer;
  NextTable: String;
  rx: TRegExpr;
begin
  // Get table name from a result set
  Result := '';
  for i:=0 to ColumnCount-1 do begin
    NextTable := TableName(i);
    if (not Result.IsEmpty) and (not NextTable.IsEmpty) and (Result <> NextTable) then
      raise EDbError.Create(_('More than one table involved.'));
    if not NextTable.IsEmpty then
      Result := NextTable;
  end;
  if Result.IsEmpty then begin
    // Untested with joins, compute columns and views
    Result := GetTableNameFromSQLEx(SQL, idMixCase);
    rx := TRegExpr.Create;
    rx.Expression := '\.([^\.]+)$';
    if rx.Exec(Result) then
      Result := rx.Match[1];
    rx.Free;
    if Result.IsEmpty then
      raise EDbError.Create('Could not determine name of table.');
  end;
end;


function TMySQLQuery.TableName(Column: Integer): String;
var
  Field: PMYSQL_FIELD;
  tbl: AnsiString;
  Objects: TDBObjectList;
  Obj: TDBObject;
begin
  Field := FConnection.Lib.mysql_fetch_field_direct(FCurrentResults, Column);

  if Field.table^ <> Field.org_table^ then begin
    // Probably a VIEW, in which case we rely on the first column's table name.
    // TODO: This is unsafe when joining a view with a table/view.
    if Field.db <> '' then begin
      Objects := Connection.GetDBObjects(Connection.DecodeAPIString(Field.db));
      for Obj in Objects do begin
        if (Obj.Name = Connection.DecodeAPIString(Field.table)) and (Obj.NodeType = lntView) then begin
          tbl := Field.table;
          break;
        end;
      end;
    end;
  end else begin
    // Normal table column
    // Note: this is empty on data tab TEXT columns with LEFT(..) clause
    tbl := Field.org_table;
  end;

  Result := Connection.DecodeAPIString(tbl);
end;


function TAdoDBQuery.TableName(Column: Integer): String;
begin
  Result := '';
end;

function TPGQuery.TableName(Column: Integer): String;
var
  FieldTypeOID: POid;
begin
  // Get table name from a result set
  Result := EmptyStr;
  FieldTypeOID := FConnection.Lib.PQftable(FCurrentResults, Column);
  if not FConnection.RegClasses.ContainsKey(FieldTypeOID) then begin
    Result := FConnection.GetVar('SELECT '+IntToStr(FieldTypeOID)+'::regclass');
    FConnection.RegClasses.Add(FieldTypeOID, Result);
  end else begin
    FConnection.RegClasses.TryGetValue(FieldTypeOID, Result);
  end;
end;


function TSQLiteQuery.TableName(Column: Integer): String;
var
  tblA: AnsiString;
begin
  Result := EmptyStr;
  tblA := FConnection.Lib.sqlite3_column_table_name(FCurrentResults.Statement, Column);
  Result := FConnection.DecodeAPIString(tblA);
end;


function TDBQuery.QuotedDbAndTableName: String;
begin
  // Prefer TDBObject when quoting as it knows its schema
  if FDBObject <> nil then
    Result := FDBObject.QuotedDbAndTableName
  else
    Result := FConnection.QuotedDbAndTableName(DatabaseName, TableName);
end;


function TDBQuery.GetKeyColumns: TStringList;
var
  i: Integer;
begin
  // Return key column names, or all column names if no good key present
  PrepareEditing;
  Result := Connection.GetKeyColumns(FColumns, FKeys);
  if Result.Count = 0 then begin
    // No good key found. Just expect all columns to be present.
    for i:=0 to FColumns.Count-1 do
      Result.Add(FColumns[i].Name);
  end;
end;


procedure TDBQuery.CheckEditable;
var
  i: Integer;
  KeyCols: TStringList;
begin
  KeyCols := GetKeyColumns;
  if KeyCols.Count = 0 then
    raise EDbError.Create(_(MSG_NOGRIDEDITING));
  // All column names must be present in order to send valid INSERT/UPDATE/DELETE queries
  for i:=0 to KeyCols.Count-1 do begin
    if FColumnOrgNames.IndexOf(KeyCols[i]) = -1 then
      raise EDbError.Create(_(MSG_NOGRIDEDITING));
  end;
  for i:=0 to FColumnOrgNames.Count-1 do begin
    if FColumnOrgNames[i] = '' then
      raise EDbError.CreateFmt(_('Column #%d has an undefined origin: %s'), [i, ColumnNames[i]]);
  end;
end;

function TDBQuery.IsEditable: Boolean;
begin
  try
    CheckEditable;
    Result := True;
  except
    on E:EDbError do begin
      FConnection.Log(lcDebug, E.Message);
      Result := False;
    end;
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
  Result := '';

  for i:=0 to NeededCols.Count-1 do begin
    j := FColumnOrgNames.IndexOf(NeededCols[i]);
    if j = -1 then
      raise EDbError.CreateFmt(_('Cannot compose WHERE clause - column missing: %s'), [NeededCols[i]]);
    if Result <> '' then
      Result := Result + ' AND';

    Result := Result + ' ' + Connection.QuoteIdent(FColumnOrgNames[j]);
    if (DataType(j).Index = dtJson) and (Self is TPGQuery) then begin
      Result := Result + '::text';
    end;

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
          else begin
            // Guess (!) the default value silently inserted by the server. This is likely
            // to be incomplete in cases where a UNIQUE key allows NULL here
            if ColVal='' then
              ColVal := '0';
            Result := Result + '=' + ColVal;
          end;
        end;
        dtcTemporal:
          Result := Result + '=' + Connection.EscapeString(Connection.GetDateTimeValue(ColVal, DataType(j).Index));
        dtcBinary:
          Result := Result + '=' + HexValue(ColVal);
        else
          Result := Result + '=' + Connection.EscapeString(ColVal);
      end;
    end;
  end;
end;


function TDBQuery.GridQuery(QueryType, QueryBody: String): String;
var
  KeyColumns: TStringList;
begin
  // Return automatic grid UPDATE/DELETE/SELECT, and apply LIMIT clause if no good key is present
  KeyColumns := Connection.GetKeyColumns(FColumns, FKeys);
  if KeyColumns.Count > 0 then
    Result := QueryType + ' ' + QueryBody
  else
    Result := Connection.ApplyLimitClause(QueryType, QueryBody, 1, 0);
end;



{ TGridValue }

destructor TGridValue.Destroy;
begin
  NewText := '';
  OldText := '';
  inherited;
end;


{ TSQLiteGridRows }

constructor TSQLiteGridRows.Create(AOwner: TSQLiteConnection);
begin
  inherited Create;
  FConnection := AOwner;
end;

destructor TSQLiteGridRows.Destroy;
begin
  try
    if Statement <> nil then
      FConnection.Lib.sqlite3_finalize(Statement);
  except
    on E:Exception do;
  end;
  inherited;
end;




{ TDBObjectComparer }

function TDBObjectComparer.Compare(const Left, Right: TDBObject): Integer;
begin
  // Simple sort method for a TDBObjectList
  Result := CompareAnyNode(Left.Schema+'.'+Left.Name, Right.Schema+'.'+Right.Name);
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
  Schema := '';
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
  FCreateCodeLoaded := False;
  FConnection := OwnerConnection;
  FTableColumns := TTableColumnList.Create;
  FTableColumns.Loaded := False;
  FTableKeys := TTableKeyList.Create;
  FTableKeys.Loaded := False;
  FTableForeignKeys := TForeignKeyList.Create;
  FTableForeignKeys.Loaded := False;
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
    Schema := s.Schema;
    Database := s.Database;
    NodeType := s.NodeType;
    GroupType := s.GroupType;
    Created := s.Created;
    Updated := s.Updated;
    Comment := s.Comment;
    Rows := s.Rows;
    Size := s.Size;
    ArgTypes := s.ArgTypes;
    FCreateCode := s.FCreateCode;
    FCreateCodeLoaded := s.FCreateCodeLoaded;
    FTableColumns.Assign(s.FTableColumns);
    FTableKeys.Assign(s.FTableKeys);
    FTableForeignKeys.Assign(s.FTableForeignKeys);
  end else
    inherited;
end;


procedure TDBObject.UnloadDetails;
begin
  FTableColumns.Clear;
  FTableColumns.Loaded := False;
  FTableKeys.Clear;
  FTableKeys.Loaded := False;
  FTableForeignKeys.Clear;
  FTableForeignKeys.Loaded := False;
  FCreateCode := '';
  FCreateCodeLoaded := False;
end;



function TDBObject.IsSameAs(CompareTo: TDBObject): Boolean;
begin
  if (not Assigned(CompareTo)) or (CompareTo = nil) then begin
    Result := False;
  end else begin
    Result := (Name = CompareTo.Name)
      and (NodeType = CompareTo.NodeType)
      and (Database = CompareTo.Database)
      and (Schema = CompareTo.Schema)
      and (Column = CompareTo.Column)
      and (ArgTypes = CompareTo.ArgTypes)
      and (Connection = CompareTo.Connection);
  end;
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
    else Result := _('Unknown, should never appear');
  end;
end;

function TDBObject.GetImageIndex: Integer;
begin
  // Detect key icon index for specified db object (table, trigger, ...)
  case NodeType of
    lntNone: Result := FConnection.Parameters.ImageIndex;

    lntDb: Result := ICONINDEX_DB;

    lntGroup: begin
      case GroupType of
        lntTable: Result := ICONINDEX_TABLE;
        lntFunction: Result := ICONINDEX_STOREDFUNCTION;
        lntProcedure: Result := ICONINDEX_STOREDPROCEDURE;
        lntView: Result := ICONINDEX_VIEW;
        lntTrigger: Result := ICONINDEX_TRIGGER;
        lntEvent: Result := ICONINDEX_EVENT;
        else Result := -1;
      end;
    end;

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


function TDBObject.GetOverlayImageIndex: Integer;
var
  EngineUpper: String;
begin
  // Detect small overlay icon index for specified table engine
  Result := -1;
  case NodeType of
    lntNone: begin
      if not Connection.Active then
        Result := 158;
    end;

    lntDb: begin
      if Database = Connection.Database then
        Result := ICONINDEX_HIGHLIGHTMARKER;
    end;

    lntTable: begin
      EngineUpper := UpperCase(Engine);
      if EngineUpper = 'FEDERATED' then
        Result := 177
      else if EngineUpper = 'MEMORY' then
        Result := 178
      else if EngineUpper = 'ARIA' then
        Result := 179
      else if EngineUpper = 'CSV' then
        Result := 180
      else if EngineUpper = 'PERFORMANCE_SCHEMA' then
        Result := 181
      else if EngineUpper = 'BLACKHOLE' then
        Result := 167
      else if EngineUpper = 'MRG_MYISAM' then
        Result := 182;
    end;

  end;
end;


function TDBObject.GetPath: String;
begin
  Result := Database + DELIM + Schema + DELIM + Name;
end;


function TDBObject.GetCreateCode: String;
begin
  if not FCreateCodeLoaded then try
    FCreateCode := Connection.GetCreateCode(Self);
    FCreateCodeLoaded := True;
  except on E:Exception do
    Connection.Log(lcError, E.Message);
  end;
  Result := FCreateCode;
end;

function TDBObject.GetCreateCode(RemoveAutoInc, RemoveDefiner: Boolean): String;

  procedure RemovePattern(RegExp: String);
  var
    rx: TRegExpr;
  begin
    // Remove first occurrence of pattern from result
    rx := TRegExpr.Create;
    rx.Expression := RegExp;
    rx.ModifierI := True;
    if rx.Exec(Result) then begin
      Delete(Result, rx.MatchPos[0], rx.MatchLen[0]-1);
    end;
    rx.Free;
  end;
begin
  Result := GetCreateCode;

  if RemoveAutoInc then begin
    // Remove AUTO_INCREMENT clause
    RemovePattern('\sAUTO_INCREMENT\s*\=\s*\d+\s');
  end;

  if RemoveDefiner then begin
    // Remove DEFINER clause
    RemovePattern('\sDEFINER\s*\=\s*\S+\s');
  end;

end;

function TDBObject.QuotedDatabase(AlwaysQuote: Boolean=True): String;
begin
  if FConnection.Parameters.NetTypeGroup = ngPgSQL then
    Result := Connection.QuoteIdent(Schema, AlwaysQuote)
  else
    Result := Connection.QuoteIdent(Database, AlwaysQuote);
end;

function TDBObject.QuotedName(AlwaysQuote: Boolean=True; SeparateSegments: Boolean=True): String;
begin
  Result := '';
  if FConnection.Parameters.IsAnyMSSQL then begin
    // MSSQL expects schema separated from table, and in some situations the whole string quoted as a whole
    if Schema <> '' then begin
      if SeparateSegments then
        Result := Result + Connection.QuoteIdent(Schema, AlwaysQuote)
      else
        Result := Result + Schema;
    end;
    Result := Result + '.';
    if SeparateSegments then
      Result := Result + Connection.QuoteIdent(Name, AlwaysQuote)
    else
      Result := Connection.QuoteIdent(Result + Name, AlwaysQuote);
  end else begin
    Result := Result + Connection.QuoteIdent(Name, AlwaysQuote);
  end;
end;

function TDBObject.QuotedDbAndTableName(AlwaysQuote: Boolean=True): String;
begin
  Result := QuotedDatabase(AlwaysQuote) + '.' + QuotedName(AlwaysQuote);
end;

function TDBObject.QuotedColumn(AlwaysQuote: Boolean=True): String;
begin
  Result := Connection.QuoteIdent(Column, AlwaysQuote);
end;

function TDBObject.RowCount(Reload: Boolean): Int64;
begin
  if (Rows = -1) or Reload then begin
    Rows := Connection.GetRowCount(Self);
  end;
  Result := Rows;
end;

procedure TDBObject.Drop;
begin
  Connection.Drop(Self);
end;


function TDBObject.GetTableColumns: TTableColumnList;
var
  Db: TDBObjectList;
  DbObj: TDBObject;
begin
  // Return columns from table object
  if not FTableColumns.Loaded then begin
    for Db in Connection.FDatabaseCache do begin
      for DbObj in Db do begin
        if (DbObj <> Self) and DbObj.IsSameAs(Self) then begin
          FConnection.Log(lcDebug, 'Getting columns from database cache for '+QuotedDbAndTableName);
          FTableColumns := Dbobj.GetTableColumns;
          Break;
        end;
      end;
      if FTableColumns.Loaded then
        Break;
    end;
    if not FTableColumns.Loaded then begin
      FConnection.Log(lcDebug, 'Getting fresh columns for '+QuotedDbAndTableName);
      FTableColumns := Connection.GetTableColumns(Self);
      FTableColumns.Loaded := True;
    end;
  end;
  Result := TTableColumnList.Create;
  Result.Assign(FTableColumns);
end;

function TDBObject.GetTableKeys: TTableKeyList;
var
  Db: TDBObjectList;
  DbObj: TDBObject;
begin
  // Return keys from table object
  if not FTableKeys.Loaded then begin
    for Db in Connection.FDatabaseCache do begin
      for DbObj in Db do begin
        if (DbObj <> Self) and DbObj.IsSameAs(Self) then begin
          FConnection.Log(lcDebug, 'Getting keys from database cache for '+QuotedDbAndTableName);
          FTableKeys := Dbobj.GetTableKeys;
          Break;
        end;
      end;
      if FTableKeys.Loaded then
        Break;
    end;
    if not FTableKeys.Loaded then begin
      FConnection.Log(lcDebug, 'Getting fresh keys for '+QuotedDbAndTableName);
      FTableKeys := Connection.GetTableKeys(Self);
      FTableKeys.Loaded := True;
    end;
  end;
  Result := TTableKeyList.Create;
  Result.Assign(FTableKeys);
end;

function TDBObject.GetTableForeignKeys: TForeignKeyList;
var
  Db: TDBObjectList;
  DbObj: TDBObject;
begin
  // Return foreign keys from table object
  if not FTableForeignKeys.Loaded then begin
    for Db in Connection.FDatabaseCache do begin
      for DbObj in Db do begin
        if (DbObj <> Self) and DbObj.IsSameAs(Self) then begin
          FConnection.Log(lcDebug, 'Getting foreign keys from database cache for '+QuotedDbAndTableName);
          FTableForeignKeys := Dbobj.GetTableForeignKeys;
          Break;
        end;
      end;
      if FTableForeignKeys.Loaded then
        Break;
    end;
    if not FTableForeignKeys.Loaded then begin
      FConnection.Log(lcDebug, 'Getting fresh foreign keys for '+QuotedDbAndTableName);
      FTableForeignKeys := Connection.GetTableForeignKeys(Self);
      FTableForeignKeys.Loaded := True;
    end;
  end;
  Result := TForeignKeyList.Create;
  Result.Assign(FTableForeignKeys);
end;



{ *** TTableColumn }

constructor TTableColumn.Create(AOwner: TDBConnection; Serialized: String='');
var
  Attributes: TStringList;
  DataTypeIdx, OldDataTypeIdx: TDBDatatypeIndex;
  i: Integer;
  NumVal: String;

  function FromSerialized(Name, Default: String): String;
  begin
    Result := Attributes.Values[Name];
    if Result.IsEmpty then
      Result := Default;
  end;
begin
  // Initialize column from serialized values or use defaults
  inherited Create;
  FConnection := AOwner;

  // Prepare serialized string
  Serialized := StringReplace(Serialized, CHR13REPLACEMENT, #13, [rfReplaceAll]);
  Serialized := StringReplace(Serialized, CHR10REPLACEMENT, #10, [rfReplaceAll]);
  Attributes := Explode(DELIMITER, Serialized);

  // Apply given or default attributes
  Name := FromSerialized('Name', '');
  OldName := FromSerialized('OldName', '');
  NumVal := FromSerialized('DataType', Integer(dtUnknown).ToString);
  DataTypeIdx := TDBDatatypeIndex(NumVal.ToInteger);
  NumVal := FromSerialized('OldDataType', Integer(dtUnknown).ToString);
  OldDataTypeIdx := TDBDatatypeIndex(NumVal.ToInteger);
  for i:=Low(Connection.Datatypes) to High(Connection.Datatypes) do begin
    if Connection.Datatypes[i].Index = DataTypeIdx then
      DataType := Connection.Datatypes[i];
    if Connection.Datatypes[i].Index = OldDataTypeIdx then
      OldDataType := Connection.Datatypes[i];
  end;
  LengthSet := FromSerialized('LengthSet', '');
  Unsigned := FromSerialized('Unsigned', '0').ToInteger.ToBoolean;
  AllowNull := FromSerialized('AllowNull', '1').ToInteger.ToBoolean;
  ZeroFill := FromSerialized('ZeroFill', '0').ToInteger.ToBoolean;
  LengthCustomized := FromSerialized('LengthCustomized', '0').ToInteger.ToBoolean;
  NumVal := FromSerialized('DefaultType', Integer(cdtNothing).ToString);
  DefaultType := TColumnDefaultType(NumVal.ToInteger);
  DefaultText := FromSerialized('DefaultText', '');
  NumVal := FromSerialized('OnUpdateType', Integer(cdtNothing).ToString);
  OnUpdateType := TColumnDefaultType(NumVal.ToInteger);
  OnUpdateText := FromSerialized('OnUpdateText', '');
  Comment := FromSerialized('Comment', '');
  Charset := FromSerialized('Charset', '');
  Collation := FromSerialized('Collation', '');
  Expression := FromSerialized('Expression', '');
  Virtuality := FromSerialized('Virtuality', '');
  NumVal := FromSerialized('Status', Integer(esUntouched).ToString);
  FStatus := TEditingStatus(NumVal.ToInteger);

  Attributes.Free;
end;

destructor TTableColumn.Destroy;
begin
  inherited Destroy;
end;

procedure TTableColumn.Assign(Source: TPersistent);
var
  s: TTableColumn;
begin
  if Source is TTableColumn then begin
    s := Source as TTableColumn;
    Name := s.Name;
    OldName := s.OldName;
    DataType := s.DataType;
    OldDataType := s.OldDataType;
    LengthSet := s.LengthSet;
    Unsigned := s.Unsigned;
    AllowNull := s.AllowNull;
    ZeroFill := s.ZeroFill;
    LengthCustomized := s.LengthCustomized;
    DefaultType := s.DefaultType;
    DefaultText := s.DefaultText;
    OnUpdateType := s.OnUpdateType;
    OnUpdateText := s.OnUpdateText;
    Comment := s.Comment;
    Charset := s.Charset;
    Collation := s.Collation;
    Expression := s.Expression;
    Virtuality := s.Virtuality;
    FStatus := s.FStatus;
  end else
    inherited;
end;

function TTableColumn.Serialize: String;
var
  s: TStringList;
begin
  // Return object attributes/fields in a one-line text format, which can later be
  // restored through passing that text to the constructor
  // We could also use the .SQLCode method to get a text representation, but that
  // would require a more complex deserializing method
  s := TStringList.Create;
  s.AddPair('Name', Name);
  s.AddPair('OldName', OldName);
  s.AddPair('DataType', Integer(DataType.Index).ToString);
  s.AddPair('OldDataType', Integer(OldDataType.Index).ToString);
  s.AddPair('LengthSet', LengthSet);
  s.AddPair('Unsigned', Unsigned.ToInteger.ToString);
  s.AddPair('AllowNull', AllowNull.ToInteger.ToString);
  s.AddPair('ZeroFill', ZeroFill.ToInteger.ToString);
  s.AddPair('LengthCustomized', LengthCustomized.ToInteger.ToString);
  s.AddPair('DefaultType', Integer(DefaultType).ToString);
  s.AddPair('DefaultText', DefaultText);
  s.AddPair('OnUpdateType', Integer(OnUpdateType).ToString);
  s.AddPair('OnUpdateText', OnUpdateText);
  s.AddPair('Comment', Comment);
  s.AddPair('Charset', Charset);
  s.AddPair('Collation', Collation);
  s.AddPair('Expression', Expression);
  s.AddPair('Virtuality', Virtuality);
  s.AddPair('Status', Integer(FStatus).ToString);

  Result := implodestr(DELIMITER, s);
  s.Free;
  Result := StringReplace(Result, #13, CHR13REPLACEMENT, [rfReplaceAll]);
  Result := StringReplace(Result, #10, CHR10REPLACEMENT, [rfReplaceAll]);
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

function TTableColumn.SQLCode(OverrideCollation: String=''; Parts: TColumnParts=[cpAll]): String;
var
  IsVirtual: Boolean;

  function InParts(Part: TColumnPart): Boolean;
  begin
    Result := (Part in Parts) or (cpAll in Parts);
  end;
begin
  Result := '';
  IsVirtual := (Expression <> '') and (Virtuality <> '');

  if InParts(cpName) then begin
    Result := Result + FConnection.QuoteIdent(Name) + ' ';
  end;

  if InParts(cpType) then begin
    Result := Result + DataType.Name;
    if (LengthSet <> '') and DataType.HasLength then
      Result := Result + '(' + LengthSet + ')';
    if (DataType.Category in [dtcInteger, dtcReal]) and Unsigned then
      Result := Result + ' UNSIGNED';
    if (DataType.Category in [dtcInteger, dtcReal]) and ZeroFill then
      Result := Result + ' ZEROFILL';
    Result := Result + ' '; // Add space after each part
  end;

  if InParts(cpAllowNull) then begin
    if not IsVirtual then begin
      if not AllowNull then
        Result := Result + 'NOT ';
      Result := Result + 'NULL ';
    end;
  end;

  if InParts(cpDefault) then begin
    if DefaultType <> cdtNothing then begin
      case DefaultType of
        // cdtNothing: leave out whole clause
        cdtText:           Result := Result + 'DEFAULT '+FConnection.EscapeString(DefaultText);
        cdtNull:           Result := Result + 'DEFAULT NULL';
        cdtAutoInc:        Result := Result + 'AUTO_INCREMENT';
        cdtExpression:     Result := Result + 'DEFAULT '+DefaultText;
      end;
      case OnUpdateType of
        // cdtNothing: leave out whole clause
        // cdtText:    not supported, but may be valid in MariaDB?
        // cdtNull:    not supported, but may be valid in MariaDB?
        // cdtAutoInc: not valid in ON UPDATE
        cdtExpression:     Result := Result + ' ON UPDATE '+OnUpdateText;
      end;
      Result := Result + ' ';
    end;
  end;

  if InParts(cpVirtuality) then begin
    if IsVirtual then
      Result := Result + 'AS ('+Expression+') ' + Virtuality + ' ';
  end;

  if InParts(cpComment) then begin
    if (Comment <> '') and FConnection.Parameters.IsAnyMySQL then
      Result := Result + 'COMMENT ' + FConnection.EscapeString(Comment) + ' ';
  end;

  if InParts(cpCollation) then begin
    if Collation <> '' then begin
      Result := Result + 'COLLATE ';
      if OverrideCollation <> '' then
        Result := Result + FConnection.EscapeString(OverrideCollation) + ' '
      else
        Result := Result + FConnection.EscapeString(Collation) + ' ';
    end;
  end;

  Result := Trim(Result);
end;


function TTableColumn.ValueList: TStringList;
begin
  // Same as TDBQuery.ValueList, but for callers which do not have a query result
  Result := TStringList.Create;
  Result.QuoteChar := '''';
  Result.Delimiter := ',';
  if DataType.Index in [dtEnum, dtSet] then
    Result.DelimitedText := LengthSet;
end;


procedure TTableColumn.ParseDatatype(Source: String);
var
  InLiteral: Boolean;
  ParenthLeft, i: Integer;
begin
  DataType := Connection.GetDatatypeByName(Source, True);
  // Length / Set
  // Various datatypes, e.g. BLOBs, don't have any length property
  InLiteral := False;
  ParenthLeft := Pos('(', Source);
  if ParenthLeft > 0 then begin
    for i:=ParenthLeft+1 to Length(Source) do begin
      if (Source[i] = ')') and (not InLiteral) then
        break;
      if Source[i] = '''' then
        InLiteral := not InLiteral;
    end;
    LengthSet := Copy(Source, ParenthLeft+1, i-2);
    Unsigned :=  ExecRegExpr('\sunsigned[\s\w]*$', Source.ToLowerInvariant);
    ZeroFill := ExecRegExpr('\szerofill[\s\w]*$', Source.ToLowerInvariant);
  end else begin
    LengthSet := '';
    Unsigned := False;
  end;
end;


function TTableColumn.CastAsText: String;
begin
  // Cast data types which are incompatible to string functions to text columns
  Result := FConnection.QuoteIdent(Name);
  case FConnection.Parameters.NetTypeGroup of
    ngMySQL, ngSQLite: begin
      if DataType.Index in [dtUnknown, dtDate, dtDatetime, dtTime, dtTimestamp] then
        Result := 'CAST('+Result+' AS CHAR)';
    end;
    ngMSSQL: begin
      if DataType.Index = dtUnknown then
        Result := 'CAST('+Result+' AS NVARCHAR('+IntToStr(SIZE_MB)+'))';
    end;
    ngPgSQL: begin
      if DataType.Index = dtUnknown then
        Result := Result + '::text';
    end;
  end;
end;

procedure TTableColumnList.Assign(Source: TTableColumnList);
var
  Item, ItemCopy: TTableColumn;
begin
  for Item in Source do begin
    ItemCopy := TTableColumn.Create(Item.FConnection);
    ItemCopy.Assign(Item);
    Add(ItemCopy);
  end;
  Loaded := Source.Loaded;
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

procedure TTableKey.Assign(Source: TPersistent);
var
  s: TTableKey;
begin
  if Source is TTableKey then begin
    s := Source as TTableKey;
    Name := s.Name;
    OldName := s.OldName;
    IndexType := s.IndexType;
    OldIndexType := s.OldIndexType;
    Algorithm := s.Algorithm;
    Comment := s.Comment;
    Columns.Assign(s.Columns);
    SubParts.Assign(s.SubParts);
    Modified := s.Modified;
    Added := s.Added;
  end else
    inherited;
end;

procedure TTableKey.Modification(Sender: TObject);
begin
  if not Added then
    Modified := True;
end;

function TTableKey.GetImageIndex: Integer;
begin
  // Detect key icon index for specified index
  if IndexType = TTableKey.PRIMARY then Result := ICONINDEX_PRIMARYKEY
  else if IndexType = TTableKey.KEY then Result := ICONINDEX_INDEXKEY
  else if IndexType = TTableKey.UNIQUE then Result := ICONINDEX_UNIQUEKEY
  else if IndexType = TTableKey.FULLTEXT then Result := ICONINDEX_FULLTEXTKEY
  else if IndexType = TTableKey.SPATIAL then Result := ICONINDEX_SPATIALKEY
  else Result := -1;
end;

function TTableKey.SQLCode: String;
var
  i: Integer;
begin
  Result := '';
  // Supress SQL error  trying index creation with 0 column
  if Columns.Count = 0 then
    Exit;
  if IndexType = TTableKey.PRIMARY then
    Result := Result + 'PRIMARY KEY '
  else begin
    if IndexType <> TTableKey.KEY then
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

procedure TTableKeyList.Assign(Source: TTableKeyList);
var
  Item, ItemCopy: TTableKey;
begin
  for Item in Source do begin
    ItemCopy := TTableKey.Create(Item.FConnection);
    ItemCopy.Assign(Item);
    Add(ItemCopy);
  end;
  Loaded := Source.Loaded;
end;




{ *** TForeignKey }

constructor TForeignKey.Create(AOwner: TDBConnection);
begin
  inherited Create;
  FConnection := AOwner;
  Columns := TStringList.Create;
  Columns.StrictDelimiter := True;
  ForeignColumns := TStringList.Create;
  ForeignColumns.StrictDelimiter := True;
end;

destructor TForeignKey.Destroy;
begin
  FreeAndNil(Columns);
  FreeAndNil(ForeignColumns);
  inherited Destroy;
end;

procedure TForeignKey.Assign(Source: TPersistent);
var
  s: TForeignKey;
begin
  if Source is TForeignKey then begin
    s := Source as TForeignKey;
    KeyName := s.KeyName;
    OldKeyName := s.OldKeyName;
    ReferenceTable := s.ReferenceTable;
    OnUpdate := s.OnUpdate;
    OnDelete := s.OnDelete;
    Columns.Assign(s.Columns);
    ForeignColumns.Assign(s.ForeignColumns);
    Modified := s.Modified;
    Added := s.Added;
    KeyNameWasCustomized := s.KeyNameWasCustomized;
  end else
    inherited;
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


function TForeignKey.ReferenceTableObj: TDBObject;
var
  RefDb, RefTable: String;
begin
  // Find database object of reference table
  RefDb := ReferenceTable.Substring(0, Pos('.', ReferenceTable)-1);
  if not RefDb.IsEmpty then begin
    RefTable := ReferenceTable.Substring(Length(RefDb)+1);
  end else begin
    RefDb := FConnection.Database;
    RefTable := ReferenceTable;
  end;
  Result := FConnection.FindObject(RefDb, RefTable);
end;


procedure TForeignKeyList.Assign(Source: TForeignKeyList);
var
  Item, ItemCopy: TForeignKey;
begin
  for Item in Source do begin
    ItemCopy := TForeignKey.Create(Item.FConnection);
    ItemCopy.Assign(Item);
    Add(ItemCopy);
  end;
  Loaded := Source.Loaded;
end;




function mysql_authentication_dialog_ask;
var
  Username, Password: String;
  Dialog: TfrmLogin;
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
  Dialog := TfrmLogin.Create(nil);
  Dialog.lblPrompt.Caption := String(prompt);
  Dialog.editUsername.Width := Dialog.editUsername.Width + (Dialog.editUsername.Left - Dialog.lblUsername.Left);
  Dialog.editPassword.Width := Dialog.editUsername.Width;
  Dialog.lblUsername.Visible := False;
  Dialog.lblPassword.Visible := False;
  Dialog.editUsername.Left := Dialog.lblUsername.Left;
  Dialog.editPassword.Left := Dialog.lblPassword.Left;
  Dialog.editUsername.Top := Dialog.lblPrompt.Top + Dialog.lblPrompt.Height + 15;
  Dialog.editPassword.Top := Dialog.editUsername.Top;
  Dialog.editUsername.Visible := _type=1;
  Dialog.editPassword.Visible := _type=2;
  Dialog.ShowModal;
  case _type of
    1: Result := PAnsiChar(AnsiString(Dialog.editUsername.Text));
    2: Result := PAnsiChar(AnsiString(Dialog.editPassword.Text));
    else raise EDbError.CreateFmt(_('Unsupported type (%d) in %s.'), [_type, 'mysql_authentication_dialog_ask']);
  end;
  Dialog.Free;
end;




{ TIdent }

constructor TIdent.Create(AConn: TDBConnection);
begin
  FConn := AConn;
  FToken1 := TIdentToken.Create(FConn);
  FToken2 := TIdentToken.Create(FConn);
  FToken3 := TIdentToken.Create(FConn);
end;

destructor TIdent.Destroy;
begin
  FreeAndNil(FToken1);
  FreeAndNil(FToken2);
  FreeAndNil(FToken3);
  inherited;
end;




{ TFieldIdent }

procedure TFieldIdent.Parse(Text: string);
var
  rx: TRegExpr;
begin
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.ModifierG := True;  // required (default)
  rx.ModifierX := True;  // required (if rx.Expression contains whitespaces, formatting etc)

  // Find token1.token2.token3, while cursor is somewhere in token3
  rx.Expression := ''
    + '('
    + '  ([^\s,)(=.]+)'
    + '|'
    + '  ([^\s,)(=."]+ | "([^"]|"")+") \.'
    + '  ([^\s,)(=.]+)?'
    + '|'
    + '  ([^\s,)(=."]+ | "([^"]|"")+") \.'
    + '  ([^\s,)(=."]+ | "([^"]|"")+") \.'
    + '  ([^\s,)(=.]+)?'
    + ')$';

  rx.Expression := ReplaceStr(rx.Expression, '"', '\' + FConn.QuoteChar);

  if rx.Exec(Trim(Text)) then begin
    // Keyword|Schema|Table|Column|Proc|etc
    if rx.Match[2] <> '' then begin
      Token1.Parse('');
      Token2.Parse('');
      Token3.Parse(rx.Match[2]);
    // Schema.Table|Table.Column
    end else if rx.Match[3] <> '' then begin
      Token1.Parse('');
      Token2.Parse(rx.Match[3]);
      Token3.Parse(rx.Match[5]);
    // Schema.Table.Column
    end else if rx.Match[6] <> '' then begin
      Token1.Parse(rx.Match[6]);
      Token2.Parse(rx.Match[8]);
      Token3.Parse(rx.Match[10]);
    end;
  end;

  FreeAndNil(rx);
end;




{ TTableIdent }

procedure TTableIdent.Parse(Text: string);
var
  rx: TRegExpr;
begin
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.ModifierG := True;  // required (default)
  rx.ModifierX := True;  // required (if rx.Expression contains whitespaces, formatting etc)

  // Find Token1.Token2[ as ]Token3
  rx.Expression := ''
    + '(([^\s,)(=."]+ | "([^"]|"")+") \.)?'
    + ' ([^\s,)(=."]+ | "([^"]|"")+") \s*(AS)??\s*'
    + ' ([^\s,)(=."]+ | "([^"]|"")+")?$';

  rx.Expression := ReplaceStr(rx.Expression, '"', '\' + FConn.QuoteChar);

  if rx.Exec(Trim(Text)) then begin
    Token1.Parse(rx.Match[2]);
    Token2.Parse(rx.Match[4]);
    Token3.Parse(rx.Match[7]);
  end;

  FreeAndNil(rx);
end;




{ TIdentToken }

constructor TIdentToken.Create(AConn: TDBConnection);
begin
  FConn := AConn;
end;

procedure TIdentToken.SetValue(const Value: string);
begin
  FValue := Value;
end;

procedure TIdentToken.Parse(Text: string);
var
  QuoteChar: Char;
begin
  FOriginal := Text;
  FValue    := Trim(Text);

  if FValue = '' then begin
    FIsQuoted := False;
    exit;
  end;

  QuoteChar := FConn.QuoteChar;
  FIsQuoted := (Length(FValue) > 2) and (FValue[1] = QuoteChar) and (FValue[Length(FValue)] = QuoteChar);

  // UnQuote ident without deleting all TDBConnection.FQuoteChars
  if FIsQuoted then begin
    FValue := Copy(FValue, 2, Length(FValue) - 2);
    FValue := StringReplace(FValue, QuoteChar + QuoteChar, QuoteChar, [rfReplaceAll]);
  end;

  if (not FIsQuoted) and (FConn is TPgConnection) then
    FValue := LowerCase(FValue);
end;

end.
