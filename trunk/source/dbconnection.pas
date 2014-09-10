unit dbconnection;

interface

uses
  Classes, SysUtils, windows, mysql_structures, SynRegExpr, Generics.Collections, Generics.Defaults,
  DateUtils, Types, Math, Dialogs, ADODB, DB, DBCommon, ComObj, Graphics, ExtCtrls, StrUtils,
  gnugettext, AnsiStrings, Controls, Forms;


type
  {$M+} // Needed to add published properties

  { TDBObjectList and friends }

  TListNodeType = (lntNone, lntDb, lntGroup, lntTable, lntView, lntFunction, lntProcedure, lntTrigger, lntEvent, lntColumn);
  TListNodeTypes = Set of TListNodeType;
  TDBConnection = class;
  TConnectionParameters = class;
  TDBQuery = class;
  TDBQueryList = TObjectList<TDBQuery>;
  TDBObject = class(TPersistent)
    private
      FCreateCode: String;
      FCreateCodeFetched: Boolean;
      FWasSelected: Boolean;
      FConnection: TDBConnection;
      function GetObjType: String;
      function GetImageIndex: Integer;
      function GetOverlayImageIndex: Integer;
      function GetPath: String;
      function GetCreateCode: String;
      procedure SetCreateCode(Value: String);
    public
      // Table options:
      Name, Schema, Database, Column, Engine, Comment, RowFormat, CreateOptions, Collation: String;
      Created, Updated, LastChecked: TDateTime;
      Rows, Size, Version, AvgRowLen, MaxDataLen, IndexLen, DataLen, DataFree, AutoInc, CheckSum: Int64;
      // Routine options:
      Body, Definer, Returns, DataAccess, Security: String;
      Deterministic: Boolean;

      NodeType, GroupType: TListNodeType;
      constructor Create(OwnerConnection: TDBConnection);
      procedure Assign(Source: TPersistent); override;
      function IsSameAs(CompareTo: TDBObject): Boolean;
      function QuotedDatabase(AlwaysQuote: Boolean=True): String;
      function QuotedName(AlwaysQuote: Boolean=True): String;
      function QuotedDbAndTableName(AlwaysQuote: Boolean=True): String;
      function QuotedColumn(AlwaysQuote: Boolean=True): String;
      property ObjType: String read GetObjType;
      property ImageIndex: Integer read GetImageIndex;
      property OverlayImageIndex: Integer read GetOverlayImageIndex;
      property Path: String read GetPath;
      property CreateCode: String read GetCreateCode write SetCreateCode;
      property WasSelected: Boolean read FWasSelected write FWasSelected;
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
      function SQLCode(OverrideCollation: String=''): String;
      function ValueList: TStringList;
      property Status: TEditingStatus read FStatus write SetStatus;
      property Connection: TDBConnection read FConnection;
  end;
  PTableColumn = ^TTableColumn;
  TTableColumnList = TObjectList<TTableColumn>;

  TTableKey = class(TObject)
    private
      FConnection: TDBConnection;
      function GetImageIndex: Integer;
    public
      Name, OldName: String;
      IndexType, OldIndexType, Algorithm: String;
      Columns, SubParts: TStringList;
      Modified, Added: Boolean;
      constructor Create(AOwner: TDBConnection);
      destructor Destroy; override;
      procedure Modification(Sender: TObject);
      function SQLCode: String;
      property ImageIndex: Integer read GetImageIndex;
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
    public
      NewText, OldText: String;
      NewIsNull, OldIsNull: Boolean;
      NewIsFunction, OldIsFunction: Boolean;
      Modified: Boolean;
      destructor Destroy; override;
  end;
  TRowData = class(TObjectList<TCellData>)
    public
      RecNo: Int64;
      Inserted: Boolean;
  end;
  TUpdateData = TObjectList<TRowData>;

  // Custom exception class for any connection or database related error
  EDatabaseError = class(Exception);

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
    ntPgSQL_TCPIP);
  TNetTypeGroup = (ngMySQL, ngMSSQL, ngPgSQL);

  TConnectionParameters = class(TObject)
    strict private
      FNetType: TNetType;
      FHostname, FUsername, FPassword, FAllDatabases, FComment, FStartupScriptFilename,
      FSessionPath, FSSLPrivateKey, FSSLCertificate, FSSLCACertificate, FServerVersion,
      FSSHHost, FSSHUser, FSSHPassword, FSSHPlinkExe, FSSHPrivateKey: String;
      FPort, FSSHPort, FSSHLocalPort, FSSHTimeout, FCounter, FQueryTimeout: Integer;
      FLoginPrompt, FCompressed, FLocalTimeZone, FFullTableStatus, FWindowsAuth, FWantSSL, FIsFolder: Boolean;
      FSessionColor: TColor;
      FLastConnect: TDateTime;
      function GetImageIndex: Integer;
      function GetSessionName: String;
    public
      constructor Create; overload;
      constructor Create(SessionRegPath: String); overload;
      procedure SaveToRegistry;
      function CreateConnection(AOwner: TComponent): TDBConnection;
      function CreateQuery(AOwner: TComponent): TDBQuery;
      function NetTypeName(NetType: TNetType; LongFormat: Boolean): String;
      function GetNetTypeGroup: TNetTypeGroup;
      function IsMySQL: Boolean;
      function IsMSSQL: Boolean;
      function IsMariaDB: Boolean;
      function IsPercona: Boolean;
      function IsTokudb: Boolean;
      function IsInfiniDB: Boolean;
      function IsInfobright: Boolean;
      property ImageIndex: Integer read GetImageIndex;
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
      property AllDatabasesStr: String read FAllDatabases write FAllDatabases;
      property Comment: String read FComment write FComment;
      property StartupScriptFilename: String read FStartupScriptFilename write FStartupScriptFilename;
      property QueryTimeout: Integer read FQueryTimeout write FQueryTimeout;
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
  end;
  PConnectionParameters = ^TConnectionParameters;


  { TDBConnection }

  TDBLogCategory = (lcInfo, lcSQL, lcUserFiredSQL, lcError, lcDebug);
  TDBLogEvent = procedure(Msg: String; Category: TDBLogCategory=lcInfo; Connection: TDBConnection=nil) of object;
  TDBEvent = procedure(Connection: TDBConnection; Database: String) of object;
  TDBDataTypeArray = Array of TDBDataType;
  TSQLSpecifityId = (spDatabaseTable, spDatabaseTableId,
    spDbObjectsTable, spDbObjectsCreateCol, spDbObjectsUpdateCol, spDbObjectsTypeCol,
    spEmptyTable, spRenameTable, spRenameView, spCurrentUserHost,
    spAddColumn, spChangeColumn,
    spSessionVariables, spGlobalVariables,
    spISTableSchemaCol,
    spUSEQuery);

  TDBConnection = class(TComponent)
    private
      FActive: Boolean;
      FConnectionStarted: Integer;
      FServerUptime: Integer;
      FParameters: TConnectionParameters;
      FLoginPromptDone: Boolean;
      FDatabase: String;
      FAllDatabases: TStringList;
      FLogPrefix: String;
      FOnLog: TDBLogEvent;
      FOnConnected: TDBEvent;
      FOnDatabaseChanged: TDBEvent;
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
      FObjectNamesInSelectedDB: TStrings;
      FResultCount: Integer;
      FStatementNum: Cardinal;
      FCurrentUserHostCombination: String;
      FLockedByThread: TThread;
      FQuoteChar: Char;
      FQuoteChars: String;
      FDatatypes: TDBDataTypeArray;
      FThreadID: Cardinal;
      FSQLSpecifities: Array[TSQLSpecifityId] of String;
      FKeepAliveTimer: TTimer;
      FFavorites: TStringList;
      procedure SetActive(Value: Boolean); virtual; abstract;
      procedure DoBeforeConnect; virtual;
      procedure DoAfterConnect; virtual;
      procedure DetectUSEQuery(SQL: String); virtual;
      procedure SetDatabase(Value: String);
      function GetThreadId: Cardinal; virtual; abstract;
      function GetCharacterSet: String; virtual;
      procedure SetCharacterSet(CharsetName: String); virtual; abstract;
      function GetLastErrorCode: Cardinal; virtual; abstract;
      function GetLastError: String; virtual; abstract;
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
      function ExtractIdentifier(var SQL: String): String;
      procedure ClearCache(IncludeDBObjects: Boolean);
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); virtual; abstract;
      procedure SetObjectNamesInSelectedDB;
      procedure SetLockedByThread(Value: TThread); virtual;
      procedure KeepAliveTimerEvent(Sender: TObject);
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
      function ConnectionInfo: TStringList;
      function GetLastResults: TDBQueryList; virtual; abstract;
      function GetCreateCode(Database, Schema, Name: String; NodeType: TListNodeType): String; virtual;
      function GetSessionVariables(Refresh: Boolean): TDBQuery;
      function MaxAllowedPacket: Int64; virtual; abstract;
      function GetSQLSpecifity(Specifity: TSQLSpecifityId): String;
      function ExplainAnalyzer(SQL, DatabaseName: String): Boolean; virtual;
      function GetDateTimeValue(Input: String; Datatype: TDBDatatypeIndex): String;
      procedure ClearDbObjects(db: String);
      procedure ClearAllDbObjects;
      procedure ParseTableStructure(CreateTable: String; Columns: TTableColumnList; Keys: TTableKeyList; ForeignKeys: TForeignKeyList);
      procedure ParseViewStructure(CreateCode: String; DBObj: TDBObject; Columns: TTableColumnList;
        var Algorithm, Definer, SQLSecurity, CheckOption, SelectCode: String);
      procedure ParseRoutineStructure(Obj: TDBObject; Parameters: TRoutineParamList);
      function GetDatatypeByName(var DataType: String; DeleteFromSource: Boolean): TDBDatatype;
      function ApplyLimitClause(QueryType, QueryBody: String; Limit, Offset: Int64): String;
      function LikeClauseTail: String;
      property Parameters: TConnectionParameters read FParameters write FParameters;
      property ThreadId: Cardinal read GetThreadId;
      property ConnectionUptime: Integer read GetConnectionUptime;
      property ServerUptime: Integer read GetServerUptime;
      property CharacterSet: String read GetCharacterSet write SetCharacterSet;
      property LastErrorCode: Cardinal read GetLastErrorCode;
      property LastError: String read GetLastError;
      property ServerOS: String read FServerOS;
      property ServerVersionUntouched: String read FServerVersionUntouched;
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
      property ObjectNamesInSelectedDB: TStrings read FObjectNamesInSelectedDB write FObjectNamesInSelectedDB;
      property ResultCount: Integer read FResultCount;
      property CurrentUserHostCombination: String read GetCurrentUserHostCombination;
      property LockedByThread: TThread read FLockedByThread write SetLockedByThread;
      property Datatypes: TDBDataTypeArray read FDatatypes;
      property Favorites: TStringList read FFavorites;
    published
      property Active: Boolean read FActive write SetActive default False;
      property Database: String read FDatabase write SetDatabase;
      property LogPrefix: String read FLogPrefix write FLogPrefix;
      property OnLog: TDBLogEvent read FOnLog write FOnLog;
      property OnConnected: TDBEvent read FOnConnected write FOnConnected;
      property OnDatabaseChanged: TDBEvent read FOnDatabaseChanged write FOnDatabaseChanged;
  end;
  TDBConnectionList = TObjectList<TDBConnection>;


  { TMySQLConnection }

  TMySQLRawResults = Array of PMYSQL_RES;
  TMySQLConnection = class(TDBConnection)
    private
      FHandle: PMYSQL;
      FLastRawResults: TMySQLRawResults;
      FPlink: TPlink;
      procedure SetActive(Value: Boolean); override;
      procedure DoBeforeConnect; override;
      procedure DoAfterConnect; override;
      procedure AssignProc(var Proc: FARPROC; Name: PAnsiChar);
      function GetThreadId: Cardinal; override;
      function GetCharacterSet: String; override;
      procedure SetCharacterSet(CharsetName: String); override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastError: String; override;
      function GetAllDatabases: TStringList; override;
      function GetTableEngines: TStringList; override;
      function GetCollationTable: TDBQuery; override;
      function GetCharsetTable: TDBQuery; override;
      function GetCreateViewCode(Database, Name: String): String;
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); override;
      procedure SetLockedByThread(Value: TThread); override;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function GetLastResults: TDBQueryList; override;
      function GetCreateCode(Database, Schema, Name: String; NodeType: TListNodeType): String; override;
      property LastRawResults: TMySQLRawResults read FLastRawResults;
      function MaxAllowedPacket: Int64; override;
      function ExplainAnalyzer(SQL, DatabaseName: String): Boolean; override;
  end;

  TAdoRawResults = Array of _RecordSet;
  TAdoDBConnection = class(TDBConnection)
    private
      FAdoHandle: TAdoConnection;
      FLastRawResults: TAdoRawResults;
      FLastError: String;
      procedure SetActive(Value: Boolean); override;
      procedure DoAfterConnect; override;
      function GetThreadId: Cardinal; override;
      procedure SetCharacterSet(CharsetName: String); override;
      function GetLastErrorCode: Cardinal; override;
      function GetLastError: String; override;
      function GetAllDatabases: TStringList; override;
      function GetCollationTable: TDBQuery; override;
      function GetCharsetTable: TDBQuery; override;
      function GetInformationSchemaObjects: TStringList; override;
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function GetLastResults: TDBQueryList; override;
      function MaxAllowedPacket: Int64; override;
      property LastRawResults: TAdoRawResults read FLastRawResults;
  end;

  TPQConnectStatus = (CONNECTION_OK, CONNECTION_BAD, CONNECTION_STARTED, CONNECTION_MADE, CONNECTION_AWAITING_RESPONSE, CONNECTION_AUTH_OK, CONNECTION_SETENV, CONNECTION_SSL_STARTUP, CONNECTION_NEEDED);
  PPGconn = Pointer;
  PPGresult = Pointer;
  POid = Integer;
  TPGRawResults = Array of PPGresult;
  TPQerrorfields = (PG_DIAG_SEVERITY, PG_DIAG_SQLSTATE, PG_DIAG_MESSAGE_PRIMARY, PG_DIAG_MESSAGE_DETAIL, PG_DIAG_MESSAGE_HINT, PG_DIAG_STATEMENT_POSITION, PG_DIAG_INTERNAL_POSITION, PG_DIAG_INTERNAL_QUERY, PG_DIAG_CONTEXT, PG_DIAG_SOURCE_FILE, PG_DIAG_SOURCE_LINE, PG_DIAG_SOURCE_FUNCTION);
  TPgConnection = class(TDBConnection)
    private
      FHandle: PPGconn;
      FLastRawResults: TPGRawResults;
      FLastErrorHint: String;
      procedure SetActive(Value: Boolean); override;
      procedure DoBeforeConnect; override;
      function GetThreadId: Cardinal; override;
      procedure SetCharacterSet(CharsetName: String); override;
      procedure AssignProc(var Proc: FARPROC; Name: PAnsiChar);
      function GetLastErrorCode: Cardinal; override;
      function GetLastError: String; override;
      function GetAllDatabases: TStringList; override;
      function GetCharsetTable: TDBQuery; override;
      procedure FetchDbObjects(db: String; var Cache: TDBObjectList); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TDBLogCategory=lcSQL); override;
      function Ping(Reconnect: Boolean): Boolean; override;
      function GetLastResults: TDBQueryList; override;
      function MaxAllowedPacket: Int64; override;
      property LastRawResults: TPGRawResults read FLastRawResults;
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
      FCurrentUpdateRow: TRowData;
      FEof: Boolean;
      FStoreResult: Boolean;
      FColumns: TTableColumnList;
      FKeys: TTableKeyList;
      FForeignKeys: TForeignKeyList;
      FEditingPrepared: Boolean;
      FUpdateData: TUpdateData;
      FDBObject: TDBObject;
      procedure SetRecNo(Value: Int64); virtual; abstract;
      procedure SetColumnOrgNames(Value: TStringList);
      procedure SetDBObject(Value: TDBObject);
      procedure CreateUpdateRow;
      function GetKeyColumns: TStringList;
      function GetWhereClause: String;
      function GridQuery(QueryType, QueryBody: String): String;
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
      function ColAttributes(Column: Integer): TTableColumn;
      function IsNull(Column: Integer): Boolean; overload; virtual; abstract;
      function IsNull(Column: String): Boolean; overload;
      function IsFunction(Column: Integer): Boolean;
      function HasResult: Boolean; virtual; abstract;
      procedure CheckEditable;
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
      property AutoIncrementColumn: Integer read FAutoIncrementColumn;
      property DBObject: TDBObject read FDBObject write SetDBObject;
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

  TPGQuery = class(TDBQuery)
    private
      FCurrentResults: PPGresult;
      FRecNoLocal: Integer;
      FResultList: TPGRawResults;
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
  mysql_warning_count: function(Handle: PMYSQL): Cardinal; stdcall;

  LibPqPath: String = 'libpq.dll';
  LibPqHandle: HMODULE;
  PQconnectdb: function(const ConnInfo: PAnsiChar): PPGconn cdecl;
  PQerrorMessage: function(const Handle: PPGconn): PAnsiChar cdecl;
  PQresultErrorMessage: function(const Result: PPGresult): PAnsiChar cdecl;
  PQresultErrorField: function(const Result: PPGresult; fieldcode: Integer): PAnsiChar;
  PQfinish: procedure(const Handle: PPGconn);
  PQstatus: function(const Handle: PPGconn): TPQConnectStatus cdecl;
  PQsendQuery: function(const Handle: PPGconn; command: PAnsiChar): Integer cdecl;
  PQgetResult: function(const Handle: PPGconn): PPGresult cdecl;
  PQbackendPID: function(const Handle: PPGconn): Integer cdecl;
  PQcmdTuples: function(Result: PPGresult): PAnsiChar; cdecl;
  PQntuples: function(Result: PPGresult): Integer; cdecl;
  PQclear: procedure(Result: PPGresult); cdecl;
  PQnfields: function(Result: PPGresult): Integer; cdecl;
  PQfname: function(const Result: PPGresult; column_number: Integer): PAnsiChar; cdecl;
  PQftype: function(const Result: PPGresult; column_number: Integer): POid; cdecl;
  PQgetvalue: function(const Result: PPGresult; row_number: Integer; column_number: Integer): PAnsiChar; cdecl;
  PQgetlength: function(const Result: PPGresult; row_number: Integer; column_number: Integer): Integer; cdecl;
  PQgetisnull: function(const Result: PPGresult; row_number: Integer; column_number: Integer): Integer; cdecl;

implementation

uses helpers, loginform;


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
    raise EDatabaseError.Create(_('Error creating I/O pipes'));
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
  PlinkParameters, PlinkCmdDisplay: String;
  OutText, ErrorText: String;
  rx: TRegExpr;
  StartupInfo: TStartupInfo;
  ExitCode: LongWord;
  Waited, ReturnedSomethingAt: Integer;
begin
  // Build plink.exe command line
  // plink bob@domain.com -pw myPassw0rd1 -P 22 -i "keyfile.pem" -L 55555:localhost:3306
  PlinkParameters := '-ssh ';
  if FConnection.Parameters.SSHUser <> '' then
    PlinkParameters := PlinkParameters + FConnection.Parameters.SSHUser + '@';
  if FConnection.Parameters.SSHHost <> '' then
    PlinkParameters := PlinkParameters + FConnection.Parameters.SSHHost
  else
    PlinkParameters := PlinkParameters + FConnection.Parameters.Hostname;
  if FConnection.Parameters.SSHPassword <> '' then
    PlinkParameters := PlinkParameters + ' -pw "' + FConnection.Parameters.SSHPassword + '"';
  if FConnection.Parameters.SSHPort > 0 then
    PlinkParameters := PlinkParameters + ' -P ' + IntToStr(FConnection.Parameters.SSHPort);
  if FConnection.Parameters.SSHPrivateKey <> '' then
    PlinkParameters := PlinkParameters + ' -i "' + FConnection.Parameters.SSHPrivateKey + '"';
  PlinkParameters := PlinkParameters + ' -N -L ' + IntToStr(FConnection.Parameters.SSHLocalPort) + ':' + FConnection.Parameters.Hostname + ':' + IntToStr(FConnection.Parameters.Port);
  rx := TRegExpr.Create;
  rx.Expression := '(-pw\s+")[^"]*(")';
  PlinkCmdDisplay := FConnection.Parameters.SSHPlinkExe + ' ' + rx.Replace(PlinkParameters, '${1}******${2}', True);
  FConnection.Log(lcInfo, f_('Attempt to create plink.exe process, waiting %ds for response ...', [FConnection.Parameters.SSHTimeout]));

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
       PChar(FConnection.Parameters.SSHPlinkExe),
       PChar(PlinkParameters),
       nil,
       nil,
       true,
       CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
       nil,
       PChar(GetCurrentDir),
       StartupInfo,
       FProcessInfo) then begin
    raise EDatabaseError.CreateFmt(_('Could not execute PLink: %s'), [CRLF+PlinkCmdDisplay]);
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
      raise EDatabaseError.CreateFmt(_('PLink exited unexpected. Command line was: %s'), [CRLF+PlinkCmdDisplay]);

    OutText := ReadPipe(FOutPipe);
    ErrorText := ReadPipe(FErrorPipe);
    if (OutText <> '') or (ErrorText <> '') then
      ReturnedSomethingAt := Waited;

    if OutText <> '' then begin
      rx.Expression := '^[^\.]+\.';
      if rx.Exec(OutText) then
        MessageDialog('PLink: '+rx.Match[0], OutText, mtInformation, [mbOK])
      else
        MessageDialog('PLink:', OutText, mtInformation, [mbOK]);
    end;

    if ErrorText <> '' then begin
      rx.Expression := '([^\.]+\?)(\s*\(y\/n\s*(,[^\)]+)?\)\s*)$';
      if rx.Exec(ErrorText) then begin
        case MessageDialog(Trim(rx.Match[1]), Copy(ErrorText, 1, Length(ErrorText)-rx.MatchLen[2]), mtConfirmation, [mbYes, mbNo, mbCancel]) of
          mrYes:
            SendText('y');
          mrNo:
            SendText('n');
          mrCancel: begin
            Destroy;
            raise EDatabaseError.Create(_('PLink cancelled'));
          end;
        end;
      end else begin
        MessageDialog('PLink:', ErrorText, mtError, [mbOK]);
      end;
    end;

    // Exit loop after 1s idletime when there was output earlier
    if (ReturnedSomethingAt > 0) and (Waited >= ReturnedSomethingAt+1000) then
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
    raise EDatabaseError.Create(_('Error reading I/O pipes'));

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
  FNetType := TNetType(AppSettings.GetDefaultInt(asNetType));
  FIsFolder := False;
  FHostname := AppSettings.GetDefaultString(asHost);
  FUsername := AppSettings.GetDefaultString(asUser);
  FPassword := '';
  FPort := MakeInt(AppSettings.GetDefaultString(asPort));
  FSSHPlinkExe := AppSettings.ReadString(asPlinkExecutable);
  FSSHPort := AppSettings.GetDefaultInt(asSSHtunnelPort);
  FSSHTimeout := AppSettings.GetDefaultInt(asSSHtunnelTimeout);
  FSSHLocalPort := FPort + 1;
  FSSLPrivateKey := '';
  FSSLCertificate := '';
  FSSLCACertificate := '';
  FStartupScriptFilename := '';
  FFullTableStatus := AppSettings.GetDefaultBool(asFullTableStatus);
  FSessionColor := AppSettings.GetDefaultInt(asTreeBackground);
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
        f_('Please report that on %s', ['http://code.google.com/p/heidisql/issues/detail?id=2958'])
        );
      FNetType := ntMySQL_TCPIP;
    end;
    FHostname := AppSettings.ReadString(asHost);
    FUsername := AppSettings.ReadString(asUser);
    FPassword := decrypt(AppSettings.ReadString(asPassword));
    FLoginPrompt := AppSettings.ReadBool(asLoginPrompt);
    FWindowsAuth := AppSettings.ReadBool(asWindowsAuth);
    FPort := MakeInt(AppSettings.ReadString(asPort));
    FAllDatabases := AppSettings.ReadString(asDatabases);
    FComment := AppSettings.ReadString(asComment);
    FSSHHost := AppSettings.ReadString(asSSHtunnelHost);
    FSSHPort := AppSettings.ReadInt(asSSHtunnelHostPort);
    FSSHUser := AppSettings.ReadString(asSSHtunnelUser);
    FSSHPassword := decrypt(AppSettings.ReadString(asSSHtunnelPassword));
    FSSHTimeout := AppSettings.ReadInt(asSSHtunnelTimeout);
    FSSHPrivateKey := AppSettings.ReadString(asSSHtunnelPrivateKey);
    FSSHLocalPort := AppSettings.ReadInt(asSSHtunnelPort);
    FSSLPrivateKey := AppSettings.ReadString(asSSLKey);
    // Auto-activate SSL for sessions created before UseSSL was introduced:
    FWantSSL := AppSettings.ReadBool(asSSLActive, '', FSSLPrivateKey<>'');
    FSSLCertificate := AppSettings.ReadString(asSSLCert);
    FSSLCACertificate := AppSettings.ReadString(asSSLCA);
    FStartupScriptFilename := AppSettings.ReadString(asStartupScriptFilename);
    FCompressed := AppSettings.ReadBool(asCompressed);
    FQueryTimeout := AppSettings.ReadInt(asQueryTimeout);
    FLocalTimeZone := AppSettings.ReadBool(asLocalTimeZone);
    FFullTableStatus := AppSettings.ReadBool(asFullTableStatus);
    FServerVersion := AppSettings.ReadString(asServerVersionFull);
    DummyDate := 0;
    FLastConnect := StrToDateTimeDef(AppSettings.ReadString(asLastConnect), DummyDate);
    FCounter := AppSettings.ReadInt(asConnectCount);
    AppSettings.ResetPath;
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
    AppSettings.WriteString(asUser, FUsername);
    AppSettings.WriteString(asPassword, encrypt(FPassword));
    AppSettings.WriteBool(asLoginPrompt, FLoginPrompt);
    AppSettings.WriteString(asPort, IntToStr(FPort));
    AppSettings.WriteInt(asNetType, Integer(FNetType));
    AppSettings.WriteBool(asCompressed, FCompressed);
    AppSettings.WriteBool(asLocalTimeZone, FLocalTimeZone);
    AppSettings.WriteInt(asQueryTimeout, FQueryTimeout);
    AppSettings.WriteBool(asFullTableStatus, FFullTableStatus);
    AppSettings.WriteString(asDatabases, FAllDatabases);
    AppSettings.WriteString(asComment, FComment);
    AppSettings.WriteString(asStartupScriptFilename, FStartupScriptFilename);
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
    else
      raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(FNetType)]);
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
    ngPgSQL:
      Result := TPGQuery.Create(AOwner);
    else
      raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(FNetType)]);
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
  else if IsTokudb then
    My := 'TokuDB'
  else if IsInfiniDB then
    My := 'InfiniDB'
  else if IsInfobright then
    My := 'Infobright'
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
    ntPgSQL_TCPIP:
      Result := 'PostgreSQL ('+_('experimental')+')';
  end else case NetType of
    ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel:
      Result := My;
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP:
      Result := 'MS SQL';
    ntPgSQL_TCPIP:
      Result := 'PostgreSQL';
  end;
end;


function TConnectionParameters.GetNetTypeGroup: TNetTypeGroup;
begin
  case FNetType of
    ntMySQL_TCPIP, ntMySQL_NamedPipe, ntMySQL_SSHtunnel:
      Result := ngMySQL;
    ntMSSQL_NamedPipe, ntMSSQL_TCPIP, ntMSSQL_SPX, ntMSSQL_VINES, ntMSSQL_RPC:
      Result := ngMSSQL;
    ntPgSQL_TCPIP:
      Result := ngPgSQL;
    else
      raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(FNetType)]);
  end;
end;


function TConnectionParameters.IsMySQL: Boolean;
begin
  Result := NetTypeGroup = ngMySQL;
end;


function TConnectionParameters.IsMSSQL: Boolean;
begin
  Result := NetTypeGroup = ngMSSQL;
end;


function TConnectionParameters.IsMariaDB: Boolean;
begin
  Result := Pos('-mariadb', LowerCase(ServerVersion)) > 0;
end;


function TConnectionParameters.IsPercona: Boolean;
begin
  Result := Pos('percona server', LowerCase(ServerVersion)) > 0;
end;


function TConnectionParameters.IsTokudb: Boolean;
begin
  Result := Pos('tokudb', LowerCase(ServerVersion)) > 0;
end;


function TConnectionParameters.IsInfiniDB: Boolean;
begin
  Result := Pos('infinidb', LowerCase(ServerVersion)) > 0;
end;


function TConnectionParameters.IsInfobright: Boolean;
begin
  Result := Pos('infobright', LowerCase(ServerVersion)) > 0;
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
      else if IsInfobright then Result := 173;
    end;
    ngMSSQL: Result := 123;
    ngPgSQL: Result := 187;
    else Result := ICONINDEX_SERVER;
  end;
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
  FKeepAliveTimer.Interval := 20000;
  FKeepAliveTimer.OnTimer := KeepAliveTimerEvent;
  FFavorites := TStringList.Create;
end;


constructor TMySQLConnection.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FQuoteChar := '`';
  FQuoteChars := '`"';
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
end;


destructor TDBConnection.Destroy;
begin
  if Active then Active := False;
  ClearCache(True);
  FKeepAliveTimer.Free;
  FFavorites.Free;
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
  //FreeAndNil(FHandle);
  inherited;
end;


function TDBConnection.GetDatatypeByName(var DataType: String; DeleteFromSource: Boolean): TDBDatatype;
var
  i: Integer;
  Match: Boolean;
  rx: TRegExpr;
  Types: String;
begin
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  Match := False;
  for i:=0 to High(FDatatypes) do begin
    Types := FDatatypes[i].Name;
    if FDatatypes[i].Names <> '' then
      Types := Types + '|' + FDatatypes[i].Names;
    rx.Expression := '^('+Types+')\b';
    Match := rx.Exec(Datatype);
    if Match then begin
      if DeleteFromSource then
        Delete(DataType, 1, rx.MatchLen[1]);
      Result := FDatatypes[i];
      break;
    end;
  end;
  rx.Free;
  if (not Match) and (FParameters.NetTypeGroup = ngPgSQL) then
    Result := FDatatypes[0];
end;


procedure TMySQLConnection.AssignProc(var Proc: FARPROC; Name: PAnsiChar);
var
  ClientVersion: String;
begin
  // Map library procedure to internal procedure
  Log(lcDebug, f_('Assign procedure "%s"', [Name]));
  Proc := GetProcAddress(LibMysqlHandle, Name);
  if Proc = nil then begin
    if @mysql_get_client_info = nil then
      mysql_get_client_info := GetProcAddress(LibMysqlHandle, 'mysql_get_client_info');
    ClientVersion := '';
    if @mysql_get_client_info <> nil then
      ClientVersion := ' ('+DecodeApiString(mysql_get_client_info)+')';
    LibMysqlHandle := 0;
    raise EDatabaseError.Create(f_('Your %s is out-dated or somehow incompatible to %s. Please use the one from the installer, or just reinstall %s.', [LibMysqlPath+ClientVersion, APPNAME, APPNAME]));
  end;
end;


procedure TPgConnection.AssignProc(var Proc: FARPROC; Name: PAnsiChar);
begin
  // Map library procedure to internal procedure
  Log(lcDebug, f_('Assign procedure "%s"', [Name]));
  Proc := GetProcAddress(LibPqHandle, Name);
  if Proc = nil then begin
    LibPqHandle := 0;
    Log(lcDebug, f_('Library error in %s: Could not find procedure address for "%s"', [LibPqPath, Name]));
    raise EDatabaseError.Create(f_('Your %s is out-dated or somehow incompatible to %s. Please use the one from the installer, or just reinstall %s.', [LibPqPath, APPNAME, APPNAME]));
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
      mysql_thread_init;
    end else begin
      mysql_thread_end;
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
  CurCharset: String;
  sslca, sslkey, sslcert: PAnsiChar;
  PluginDir: AnsiString;
  Vars, Status: TDBQuery;
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
        if FParameters.WantSSL then begin
          // mysql_ssl_set() wants nil, while PAnsiChar(AnsiString()) is never nil
          sslkey := nil;
          sslcert := nil;
          sslca := nil;
          if FParameters.SSLPrivateKey <> '' then
 	          sslkey := PAnsiChar(AnsiString(FParameters.SSLPrivateKey));
          if FParameters.SSLCertificate <> '' then
 	          sslcert := PAnsiChar(AnsiString(FParameters.SSLCertificate));
          if FParameters.SSLCACertificate <> '' then
 	          sslca := PAnsiChar(AnsiString(FParameters.SSLCACertificate));
          { TODO : Use Cipher and CAPath parameters }
          mysql_ssl_set(FHandle,
            sslkey,
            sslcert,
            sslca,
            nil,
            nil);
          Log(lcInfo, _('SSL parameters successfully set.'));
        end;
      end;

      ntMySQL_NamedPipe: begin
        FinalHost := '.';
        FinalSocket := FParameters.Hostname;
      end;

      ntMySQL_SSHtunnel: begin
        // Create plink.exe process
        FPlink := TPlink.Create(Self);
        FPlink.Connect;
        FinalHost := 'localhost';
        FinalPort := FParameters.SSHLocalPort;
      end;
    end;

    // Gather client options
    ClientFlags := CLIENT_LOCAL_FILES or CLIENT_INTERACTIVE or CLIENT_PROTOCOL_41 or CLIENT_MULTI_STATEMENTS;
    if Parameters.Compressed then
      ClientFlags := ClientFlags or CLIENT_COMPRESS;
    if Parameters.WantSSL then
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
      if FPlink <> nil then
        FPlink.Free;
      raise EDatabaseError.Create(Error);
    end else begin
      FActive := True;
      // Catch late init_connect error by firing mysql_ping(), which detects a broken
      // connection without running into some access violation. See issue #3464.
      Ping(False);
      if not FActive then
        raise EDatabaseError.CreateFmt(_('Connection closed immediately after it was established. '+
          'This is mostly caused by an "%s" server variable which has errors in itself, '+
          'or your user account does not have the required privileges for it to run.'+CRLF+CRLF+
          'You may ask someone with SUPER privileges'+CRLF+
          '* either to fix the "%s" variable,'+CRLF+
          '* or to grant you missing privileges.'),
          ['init_connect', 'init_connect']);
      Log(lcInfo, f_('Connected. Thread-ID: %d', [ThreadId]));
      CharacterSet := 'utf8';
      CurCharset := CharacterSet;
      Log(lcDebug, 'Characterset: '+CurCharset);
      FIsUnicode := CurCharset = 'utf8';
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
      FServerVersionUntouched := DecodeAPIString(mysql_get_server_info(FHandle));
      Vars := GetSessionVariables(False);
      while not Vars.Eof do begin
        if Vars.Col(0) = 'version_compile_os' then
          FServerOS := Vars.Col(1);
        if Vars.Col(0) = 'hostname' then
          FRealHostname := Vars.Col(1);
        if (Vars.Col(0) = 'version') and (Vars.Col(1) <> '') then
          FServerVersionUntouched := Vars.Col(1);
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
    if FPlink <> nil then
      FPlink.Free;
    Log(lcInfo, f_(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
  end;

end;


procedure TAdoDBConnection.SetActive(Value: Boolean);
var
  tmpdb, Error, NetLib, DataSource, QuotedPassword: String;
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
            _('On Wine, you can try to install MDAC:')+CRLF+
            '> wget http://winetricks.org/winetricks'+CRLF+
            '> chmod +x winetricks'+CRLF+
            '> sh winetricks mdac28'+CRLF+
            '> sh winetricks native_mdac');
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
    FAdoHandle.ConnectionString := 'Provider=SQLOLEDB;'+
      'Password='+QuotedPassword+';'+
      'Persist Security Info=True;'+
      'User ID='+Parameters.Username+';'+
      'Network Library='+NetLib+';'+
      'Data Source='+DataSource+';'+
      'Application Name='+AppName+';'
      ;
    if Parameters.WindowsAuth then
      FAdoHandle.ConnectionString := FAdoHandle.ConnectionString + 'Integrated Security=SSPI;';
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
      on E:EOleException do begin
        FLastError := E.Message;
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
    Log(lcInfo, f_(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
  end;
end;


procedure TPgConnection.SetActive(Value: Boolean);
var
  dbname, ConnInfo, Error, tmpdb: String;
begin
  if Value then begin
    DoBeforeConnect;
    // Simon Riggs:
    // "You should connect as "postgres" database by default, with an option to change. Don't use template1"
    dbname := FParameters.AllDatabasesStr;
    if dbname = '' then
      dbname := 'postgres';
    ConnInfo := 'host='''+FParameters.Hostname+''' '+
      'port='''+IntToStr(FParameters.Port)+''' '+
      'user='''+FParameters.Username+''' ' +
      'password='''+FParameters.Password+''' '+
      'dbname='''+dbname+''' '+
      'application_name='''+APPNAME+'''';
    FHandle := PQconnectdb(PAnsiChar(AnsiString(ConnInfo)));
    if PQstatus(FHandle) = CONNECTION_BAD then begin
      Error := LastError;
      Log(lcError, Error);
      FConnectionStarted := 0;
      FHandle := nil;
      raise EDatabaseError.Create(Error);
    end;
    FActive := True;
    FServerVersionUntouched := GetVar('SELECT VERSION()');
    FConnectionStarted := GetTickCount div 1000;
    Log(lcInfo, f_('Connected. Thread-ID: %d', [ThreadId]));
    FIsUnicode := True;
    Query('SET statement_timeout TO '+IntToStr(Parameters.QueryTimeout));
    try
      FServerUptime := StrToIntDef(GetVar('SELECT EXTRACT(EPOCH FROM CURRENT_TIMESTAMP - pg_postmaster_start_time())::INTEGER'), -1);
    except
      FServerUptime := -1;
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
    PQfinish(FHandle);
    FActive := False;
    ClearCache(False);
    FConnectionStarted := 0;
    Log(lcInfo, f_(MsgDisconnect, [FParameters.Hostname, DateTimeToStr(Now)]));
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
    [FParameters.Hostname, FParameters.NetTypeName(FParameters.NetType, True), FParameters.Username, UsingPass]
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
      FSQLSpecifities[spUSEQuery] := 'SET SCHEMA %s';
    end;

  end;

end;


procedure TMySQLConnection.DoBeforeConnect;
begin
  // Init libmysql before actually connecting.
  // Each connection has its own library handle
  if LibMysqlHandle = 0 then begin
    Log(lcDebug, f_('Loading library file %s ...', [LibMysqlPath]));
    LibMysqlHandle := LoadLibrary(PWideChar(LibMysqlPath));
    if LibMysqlHandle = 0 then
      raise EDatabaseError.CreateFmt(_('Cannot find a usable %s. Please launch %s from the directory where you have installed it.'), [LibMysqlPath, ExtractFileName(ParamStr(0))])
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
      AssignProc(@mysql_warning_count, 'mysql_warning_count');
      Log(lcDebug, LibMysqlPath + ' v' + DecodeApiString(mysql_get_client_info) + ' loaded.');
    end;
  end;
  inherited;
end;


procedure TPgConnection.DoBeforeConnect;
begin
  // Init lib before actually connecting.
  // Each connection has its own library handle
  if LibPqHandle = 0 then begin
    Log(lcDebug, f_('Loading library file %s ...', [LibPqPath]));
    LibPqHandle := LoadLibrary(PWideChar(LibPqPath));
    if LibPqHandle = 0 then
      raise EDatabaseError.CreateFmt(_('Cannot find a usable %s. Please launch %s from the directory where you have installed it.'), [LibPqPath, ExtractFileName(ParamStr(0))])
    else begin
      AssignProc(@PQconnectdb, 'PQconnectdb');
      AssignProc(@PQerrorMessage, 'PQerrorMessage');
      AssignProc(@PQresultErrorMessage, 'PQresultErrorMessage');
      AssignProc(@PQresultErrorField, 'PQresultErrorField');
      AssignProc(@PQfinish, 'PQfinish');
      AssignProc(@PQstatus, 'PQstatus');
      AssignProc(@PQsendQuery, 'PQsendQuery');
      AssignProc(@PQgetResult, 'PQgetResult');
      AssignProc(@PQbackendPID, 'PQbackendPID');
      AssignProc(@PQcmdTuples, 'PQcmdTuples');
      AssignProc(@PQntuples, 'PQntuples');
      AssignProc(@PQclear, 'PQclear');
      AssignProc(@PQnfields, 'PQnfields');
      AssignProc(@PQfname, 'PQfname');
      AssignProc(@PQftype, 'PQftype');
      AssignProc(@PQgetvalue, 'PQgetvalue');
      AssignProc(@PQgetlength, 'PQgetlength');
      AssignProc(@PQgetisnull, 'PQgetisnull');

      Log(lcDebug, LibPqPath + ' v??' + ' loaded.');
    end;
  end;
  inherited;
end;


procedure TDBConnection.DoAfterConnect;
begin
  AppSettings.SessionPath := FParameters.SessionPath;
  AppSettings.WriteString(asServerVersionFull, FServerVersionUntouched);
  FParameters.ServerVersion := FServerVersionUntouched;
  if Assigned(FOnConnected) then
    FOnConnected(Self, FDatabase);
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
      if FDatatypes[i].Index in [dtDatetime, dtTime, dtTimestamp] then
        FDatatypes[i].HasLength := True;
    end;
  end;

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
begin
  Log(lcDebug, 'Ping server ...');
  if (FHandle=nil) or (mysql_ping(FHandle) <> 0) then begin
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
      PingStatus := PQsendQuery(FHandle, PAnsiChar(''));
      IsBroken := PingStatus <> 1;
      PingResult := PQgetResult(FHandle);
      while PingResult <> nil do begin
        PQclear(PingResult);
        PingResult := PQgetResult(FHandle);
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
  FResultCount := 0;
  FStatementNum := 1;
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
    FRowsAffected := 0;
    FWarningCount := mysql_warning_count(FHandle);
    FRowsFound := 0;
    TimerStart := GetTickCount;
    QueryResult := mysql_store_result(FHandle);
    FLastQueryNetworkDuration := GetTickCount - TimerStart;

    if (QueryResult = nil) and (mysql_affected_rows(FHandle) = -1) then begin
      // Indicates a late error, e.g. triggered by mysql_store_result(), after selecting a stored
      // function with invalid SQL body. Also SHOW TABLE STATUS on older servers.
      // See http://dev.mysql.com/doc/refman/5.0/en/mysql-affected-rows.html
      //   "An integer greater than zero indicates the number of rows affected or
      //   retrieved. Zero indicates that no records were updated for an UPDATE statement, no rows
      //   matched the WHERE clause in the query or that no query has yet been executed. -1
      //   indicates that the query returned an error or that, for a SELECT query,
      //   mysql_affected_rows() was called prior to calling mysql_store_result()."
      Log(lcError, GetLastError);
      raise EDatabaseError.Create(GetLastError);
    end;

    if QueryResult = nil then
      DetectUSEQuery(SQL);

    while QueryStatus=0 do begin
      if QueryResult <> nil then begin
        // Statement returned a result set
        Inc(FRowsFound, mysql_num_rows(QueryResult));
        if DoStoreResult then begin
          SetLength(FLastRawResults, Length(FLastRawResults)+1);
          FLastRawResults[Length(FLastRawResults)-1] := QueryResult;
        end else begin
          mysql_free_result(QueryResult);
        end;
      end else begin
        // No result, but probably affected rows
        Inc(FRowsAffected, mysql_affected_rows(FHandle));
      end;
      // more results? -1 = no, >0 = error, 0 = yes (keep looping)
      Inc(FStatementNum);
      QueryStatus := mysql_next_result(FHandle);
      if QueryStatus = 0 then
        QueryResult := mysql_store_result(FHandle)
      else if QueryStatus > 0 then begin
        // MySQL stops executing a multi-query when an error occurs. So do we here by raising an exception.
        SetLength(FLastRawResults, 0);
        Log(lcError, GetLastError);
        raise EDatabaseError.Create(GetLastError);
      end;
    end;
    FResultCount := Length(FLastRawResults);

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

    DetectUSEQuery(SQL);
  except
    on E:EOleException do begin
      FLastError := E.Message;
      Log(lcError, GetLastError);
      raise EDatabaseError.Create(GetLastError);
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
  FResultCount := 0;
  FRowsFound := 0;
  FRowsAffected := 0;
  FWarningCount := 0;

  QueryStatus := PQsendQuery(FHandle, PAnsiChar(NativeSQL));

  FLastQueryDuration := GetTickCount - TimerStart;
  FLastQueryNetworkDuration := 0;
  if QueryStatus <> 1 then begin
    Log(lcError, GetLastError);
    raise EDatabaseError.Create(GetLastError);
  end else begin
    FRowsAffected := 0;
    FRowsFound := 0;
    TimerStart := GetTickCount;
    QueryResult := PQgetResult(FHandle);
    FLastQueryNetworkDuration := GetTickCount - TimerStart;

    DetectUSEQuery(SQL);

    while QueryResult <> nil do begin
      if PQnfields(QueryResult) > 0 then begin
        // Statement returned a result set
        Inc(FRowsFound, PQntuples(QueryResult));
        if DoStoreResult then begin
          SetLength(FLastRawResults, Length(FLastRawResults)+1);
          FLastRawResults[Length(FLastRawResults)-1] := QueryResult;
        end else begin
          PQclear(QueryResult);
        end;
      end else begin
        Inc(FRowsAffected, StrToIntDef(String(PQcmdTuples(QueryResult)), 0));
      end;
      if LastError <> '' then begin
        SetLength(FLastRawResults, 0);
        Log(lcError, GetLastError);
        // Clear remaining results, to avoid "another command is already running"
        while QueryResult <> nil do begin
          PQclear(QueryResult);
          QueryResult := PQgetResult(FHandle);
        end;
        raise EDatabaseError.Create(GetLastError);
      end;
      // more results?
      Inc(FStatementNum);
      QueryResult := PQgetResult(FHandle);
    end;
    FResultCount := Length(FLastRawResults);

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


function TPGConnection.GetLastResults: TDBQueryList;
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


function TMySQLConnection.GetCreateCode(Database, Schema, Name: String; NodeType: TListNodeType): String;
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
    else Exception.CreateFmt(_('Unhandled list node type in %s.%s'), [ClassName, 'GetCreateCode']);
  end;
  if NodeType = lntView then
    Result := GetCreateViewCode(Database, Name)
  else
    Result := GetVar('SHOW CREATE '+UpperCase(TmpObj.ObjType)+' '+QuoteIdent(Database)+'.'+QuoteIdent(Name), Column);
  TmpObj.Free;
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
    on E:EDatabaseError do begin
      ViewIS := GetResults('SELECT * FROM INFORMATION_SCHEMA.VIEWS WHERE '+
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
    AlternativeSelectCode := GetVar('SELECT LOAD_FILE(CONCAT(IFNULL(@@GLOBAL.datadir, CONCAT(@@GLOBAL.basedir, '+EscapeString('data/')+')), '+EscapeString(Database+'/'+Name+'.frm')+'))');
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.ModifierG := False;
    rx.Expression := '\nsource\=(.+)\n\w+\=';
    if rx.Exec(AlternativeSelectCode) then begin
      // Put pieces of CREATE VIEW together
      Obj := FindObject(Database, Name);
      ParseViewStructure(Result, Obj, nil,
        Algorithm, Definer, SQLSecurity, CheckOption, SelectCode);
      AlternativeSelectCode := UnescapeString(rx.Match[1]);
      Result := 'CREATE ';
      if Algorithm <> '' then
        Result := Result + 'ALGORITHM='+Uppercase(Algorithm)+' ';
      if Definer <> '' then
        Result := Result + 'DEFINER='+QuoteIdent(Definer, True, '@')+' ';
      Result := Result + 'VIEW '+Obj.QuotedName+' AS '+AlternativeSelectCode+' ';
      // WITH .. CHECK OPTION is already contained in the source
    end;
    rx.Free;
  except
    // Do not raise if that didn't work
    on E:EDatabaseError do;
  end;
end;


function TDBConnection.GetCreateCode(Database, Schema, Name: String; NodeType: TListNodeType): String;
var
  Cols, Keys: TDBQuery;
  ConstraintName, MaxLen: String;
  ColNames: TStringList;
  Rows: TStringList;

  // Return fitting schema clause for queries in IS.TABLES, IS.ROUTINES etc.
  // TODO: Does not work on MSSQL 2000
  function SchemaClauseIS(Prefix: String): String;
  begin
    if Schema <> '' then
      Result := Prefix+'_SCHEMA='+EscapeString(Schema)
    else
      Result := Prefix+'_CATALOG='+EscapeString(Database);
  end;

begin
  case NodeType of
    lntTable: begin
      Result := 'CREATE TABLE '+QuoteIdent(Name)+' (';

      // Retrieve column details from IS
      Cols := GetResults('SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE '+
        SchemaClauseIS('TABLE') +
        ' AND TABLE_NAME='+EscapeString(Name)
        );
      while not Cols.Eof do begin
        Result := Result + CRLF + #9 + QuoteIdent(Cols.Col('COLUMN_NAME')) + ' ' + UpperCase(Cols.Col('DATA_TYPE'));
        if not Cols.IsNull('CHARACTER_MAXIMUM_LENGTH') then begin
          MaxLen := Cols.Col('CHARACTER_MAXIMUM_LENGTH');
          if MaxLen = '-1' then
            Result := Result + '(max)'
          else
            Result := Result + '(' + MaxLen + ')';
        end;
        if Cols.Col('IS_NULLABLE') = 'NO' then
          Result := Result + ' NOT';
        Result := Result + ' NULL';
        Result := Result + ',';
        Cols.Next;
      end;
      Cols.Free;

      // Retrieve primary and unique key details from IS
      Keys := GetResults('SELECT C.CONSTRAINT_NAME, C.CONSTRAINT_TYPE, K.COLUMN_NAME'+
        ' FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS C'+
        ' INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K ON'+
        '   C.CONSTRAINT_NAME = K.CONSTRAINT_NAME'+
        '   AND K.TABLE_NAME='+EscapeString(Name)+
        '   AND '+SchemaClauseIS('K.TABLE')+
        ' WHERE C.CONSTRAINT_TYPE IN ('+EscapeString('PRIMARY KEY')+', '+EscapeString('UNIQUE')+')'+
        ' ORDER BY K.ORDINAL_POSITION');
      ConstraintName := '';
      ColNames := TStringList.Create;
      while not Keys.Eof do begin
        if Keys.Col('CONSTRAINT_NAME') <> ConstraintName then begin
          if ConstraintName <> '' then
            Result := Result + ' (' + ImplodeStr(',', ColNames) + '),';
          ConstraintName := Keys.Col('CONSTRAINT_NAME');
          Result := Result + CRLF + #9 + Keys.Col('CONSTRAINT_TYPE');
          if Pos('KEY', Keys.Col('CONSTRAINT_TYPE')) = 0 then
            Result := Result + ' KEY';
          ColNames.Clear;
        end;
        ColNames.Add(QuoteIdent(Keys.Col('COLUMN_NAME')));
        Keys.Next;
      end;
      if ConstraintName <> '' then
        Result := Result + ' (' + ImplodeStr(',', ColNames) + '),';
      Keys.Free;
      ColNames.Free;

      Delete(Result, Length(Result), 1);
      Result := Result + CRLF + ')';

    end;

    lntView: begin
      Result := GetVar('SELECT VIEW_DEFINITION'+
        ' FROM INFORMATION_SCHEMA.VIEWS'+
        ' WHERE TABLE_NAME='+EscapeString(Name)+
        ' AND '+SchemaClauseIS('TABLE')
        );
      if FParameters.NetTypeGroup = ngPgSQL then
        Result := 'CREATE VIEW ' + QuoteIdent(Name) + ' AS ' + Result;
    end;

    lntFunction: begin
      case Parameters.NetTypeGroup of
        ngMSSQL: begin
          // Tested on MS SQL 8.0 and 11.0
          // See http://www.heidisql.com/forum.php?t=12495
          Rows := GetCol('EXEC sp_helptext '+EscapeString(Name));
          // Do not use Rows.Text, as the rows already include a trailing linefeed
          Result := implodestr('', Rows);
          Rows.Free;
        end;
        else begin
          Result := GetVar('SELECT ROUTINE_DEFINITION'+
            ' FROM INFORMATION_SCHEMA.ROUTINES'+
            ' WHERE ROUTINE_NAME='+EscapeString(Name)+
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
          Rows := GetCol('EXEC sp_helptext '+EscapeString(Name));
          Result := implodestr('', Rows);
          Rows.Free;
        end;
        else begin
          Result := GetVar('SELECT ROUTINE_DEFINITION'+
            ' FROM INFORMATION_SCHEMA.ROUTINES'+
            ' WHERE ROUTINE_NAME='+EscapeString(Name)+
            ' AND ROUTINE_TYPE='+EscapeString('PROCEDURE')+
            ' AND '+SchemaClauseIS('ROUTINE')
            );
        end;
      end;
    end;

  end;

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
    end else begin
      if FParameters.NetTypeGroup = ngPgSQL then
        Value := EscapeString(Value)
      else
        Value := QuoteIdent(Value);
      Query(Format(GetSQLSpecifity(spUSEQuery), [Value]), False);
    end;
    SetObjectNamesInSelectedDB;
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


function TPGConnection.GetThreadId: Cardinal;
begin
  if FThreadId = 0 then begin
    Ping(False);
    if FActive then
      FThreadID := PQbackendPID(FHandle);
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
  Result := DecodeAPIString(mysql_character_set_name(FHandle));
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


procedure TPGConnection.SetCharacterSet(CharsetName: String);
begin
  // Not in use. No charset stuff going on here?
end;


function TMySQLConnection.GetLastErrorCode: Cardinal;
begin
  Result := mysql_errno(FHandle);
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
  Result := Cardinal(PQstatus(FHandle));
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
  if FStatementNum = 1 then
    Result := f_(MsgSQLError, [LastErrorCode, Msg])
  else
    Result := f_(MsgSQLErrorMultiStatements, [LastErrorCode, FStatementNum, Msg]);
end;


function TAdoDBConnection.GetLastError: String;
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


function TPgConnection.GetLastError: String;
begin
  Result := DecodeAPIString(PQerrorMessage(FHandle));
  Result := Trim(Result);
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
    ngMySQL, ngPgSQL: begin
      rx.Expression := '(\d+)\.(\d+)\.(\d+)';
      if rx.Exec(FServerVersionUntouched) then begin
        Result := StrToIntDef(rx.Match[1], 0) *10000 +
          StrToIntDef(rx.Match[2], 0) *100 +
          StrToIntDef(rx.Match[3], 0);
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
      end;
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
    ngMySQL, ngPgSQL: begin
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
    except on E:EDatabaseError do
      FAllDatabases := TStringList.Create;
    end;
    Result := FAllDatabases;
  end;
end;


function TPGConnection.GetAllDatabases: TStringList;
begin
  // In PostgreSQL, we display schemata, not databases.
  // The AllDatabasesStr is used to set the single database name
  if not Assigned(FAllDatabases) then begin
    try
      // Query is.schemata when using schemata, for databases use pg_database
      //FAllDatabases := GetCol('SELECT datname FROM pg_database WHERE datistemplate=FALSE');
      FAllDatabases := GetCol('SELECT '+QuoteIdent('schema_name')+
        ' FROM '+QuoteIdent('information_schema')+'.'+QuoteIdent('schemata')+
        ' ORDER BY '+QuoteIdent('schema_name'));
    except on E:EDatabaseError do
      FAllDatabases := TStringList.Create;
    end;
  end;
  Result := FAllDatabases;
end;


function TDBConnection.RefreshAllDatabases: TStringList;
begin
  FreeAndNil(FAllDatabases);
  Result := AllDatabases;
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
    ngMySQL, ngPgSQL: begin
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

    ngMSSQL: begin

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
    if Parameters.IsMSSQL then
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


function TMySQLConnection.MaxAllowedPacket: Int64;
var
  Vars: TDBQuery;
begin
  Vars := GetSessionVariables(False);
  Result := 0;
  while not Vars.Eof do begin
    if Vars.Col(0) = 'max_allowed_packet' then begin
      Result := MakeInt(Vars.Col(1));
      Break;
    end;
    Vars.Next;
  end;
  if Result = 0 then begin
    Log(lcError, f_('The server did not return a non-zero value for the %s variable. Assuming %s now.', ['max_allowed_packet', FormatByteNumber(Result)]));
    Result := SIZE_MB;
  end;

end;


function TAdoDBConnection.MaxAllowedPacket: Int64;
begin
  // No clue what MS SQL allows
  Result := SIZE_MB;
end;


function TPGConnection.MaxAllowedPacket: Int64;
begin
  // No clue what PostgreSQL allows
  Result := SIZE_MB;
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
  // Return server uptime in seconds. Return -1 if unknown.
  if FServerUptime > 0 then
    Result := FServerUptime + (Integer(GetTickCount div 1000) - FConnectionStarted)
  else
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
  URL := 'http://mariadb.org/explain_analyzer/api/1/?raw_explain='+EncodeURLParam(Raw)+'&client='+APPNAME;
  ShellExec(URL);
end;


function TDBConnection.GetDateTimeValue(Input: String; Datatype: TDBDatatypeIndex): String;
var
  dt: TDateTime;
begin
  // Return date/time string value as expected by server
  case Parameters.NetTypeGroup of
    ngMSSQL: begin
      try
        dt := StrToDateTime(Input);
        case Datatype of
          dtDate:
            Result := SysUtils.FormatDateTime('dd"/"mm"/"yyyy', dt);
          dtTime:
            Result := SysUtils.FormatDateTime('hh":"nn":"ss', dt);
          dtYear:
            Result := SysUtils.FormatDateTime('yyyy', dt);
          dtDatetime:
            Result := SysUtils.FormatDateTime('dd"/"mm"/"yyyy hh":"nn":"ss', dt);
        end;
      except
        on E:EConvertError do
          Result := Input;
      end;
    end;
    else
      Result := Input;
  end;
end;



procedure TDBConnection.ClearCache(IncludeDBObjects: Boolean);
begin
  // Free cached lists and results. Called when the connection was closed and/or destroyed
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
    SetObjectNamesInSelectedDB;
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
      ' FROM '+QuoteIdent('information_schema')+'.'+QuoteIdent('SCHEMATA')+
      ' WHERE '+QuoteIdent('SCHEMA_NAME')+'='+EscapeString(db));
  except
    Cache.FCollation := '';
  end;
  rx := TRegExpr.Create;
  rx.ModifierI := True;

  // Tables and views
  Results := nil;
  try
    if Parameters.FullTableStatus or (UpperCase(db) = 'INFORMATION_SCHEMA') then begin
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
        ' FROM INFORMATION_SCHEMA.TABLES'+
        ' WHERE TABLE_SCHEMA='+EscapeString(db)+' AND TABLE_TYPE IN('+EscapeString('BASE TABLE')+', '+EscapeString('VIEW')+')'
        );
    end;
  except
    on E:EDatabaseError do;
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
    on E:EDatabaseError do;
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
    on E:EDatabaseError do;
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
    Results := GetResults('SHOW EVENTS FROM '+QuoteIdent(db));
  except
    on E:EDatabaseError do;
  end;
  if Assigned(Results) then begin
    while not Results.Eof do begin
      if Results.Col('Db') = db then begin
        Obj := TDBObject.Create(Self);
        Cache.Add(obj);
        Obj.Name := Results.Col('Name');
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
      ' WHERE '+QuoteIdent('type')+' IN ('+EscapeString('P')+', '+EscapeString('U')+', '+EscapeString('V')+', '+EscapeString('TR')+', '+EscapeString('FN')+')');
  except
    on E:EDatabaseError do;
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
      else if tp = 'FN' then
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
  tp: String;
begin
  // Tables, views and procedures
  Results := nil;
  try
    Results := GetResults('SELECT *,'+
      ' pg_table_size(t.TABLE_SCHEMA || ''.'' || t.TABLE_NAME) AS data_length,'+
      ' pg_relation_size(t.TABLE_SCHEMA || ''.'' || t.TABLE_NAME) AS index_length,'+
      ' c.reltuples, obj_description(c.oid) AS comment'+
      ' FROM '+QuoteIdent('information_schema')+'.'+QuoteIdent('tables')+' AS t'+
      ' LEFT JOIN '+QuoteIdent('pg_namespace')+' n ON t.table_schema = n.nspname'+
      ' LEFT JOIN '+QuoteIdent('pg_class')+' c ON n.oid = c.relnamespace AND c.relname=t.table_name'+
      ' WHERE t.'+QuoteIdent('table_schema')+'='+EscapeString(db)  // Use table_schema when using schemata
      );
  except
    on E:EDatabaseError do;
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
      obj.Rows := StrToIntDef(Results.Col('reltuples'), obj.Rows);
      obj.DataLen := StrToIntDef(Results.Col('data_length'), obj.DataLen);
      obj.IndexLen := StrToIntDef(Results.Col('index_length'), obj.IndexLen);
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
end;


procedure TDBConnection.SetObjectNamesInSelectedDB;
var
  i: Integer;
  Objects: TDBObjectList;
  ObjNames: String;
begin
  // Add object names to additional stringlist
  if Assigned(FObjectNamesInSelectedDB) then begin
    ObjNames := '';
    if DbObjectsCached(FDatabase) then begin
      Objects := GetDbObjects(FDatabase);
      // Limit slow highlighter to 1000 table names. See http://www.heidisql.com/forum.php?t=16307
      if Objects.Count < 1000 then begin
        for i:=0 to Objects.Count-1 do
          ObjNames := ObjNames + Objects[i].Name + CRLF;
      end;
    end;
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


function TDBConnection.ExtractIdentifier(var SQL: String): String;
var
  i, LeftPos, RightPos: Integer;
begin
  // Return first identifier from SQL and remove it from the original string
  // Backticks are escaped by a second backtick
  Result := '';

  // Find left start position
  LeftPos := 0;
  for i:=1 to Length(SQL) do begin
    if Pos(SQL[i], FQuoteChars) > 0 then begin
      LeftPos := i+1;
      Break;
    end;
  end;

  // Step forward for each character of the identifier
  i := LeftPos;
  RightPos := LeftPos;
  while i < Length(SQL) do begin
    if Pos(SQL[i], FQuoteChars) > 0 then begin
      if SQL[i+1] = SQL[i] then
        Inc(i)
      else begin
        RightPos := i;
        Break;
      end;
    end;
    Result := Result + SQL[i];
    Inc(i);
  end;

  if RightPos > LeftPos then
    Delete(SQL, 1, RightPos+1);
end;


function TDBConnection.ConnectionInfo: TStringList;
var
  Infos, Val: String;
  rx: TRegExpr;

  function EvalBool(B: Boolean): String;
  begin
    if B then Result := _('Yes') else Result := _('No');
  end;
begin
  Log(lcDebug, 'Get connection details ...');
  Result := TStringList.Create;
  if Assigned(Parameters) then
    Result.Values[_('Host')] := Parameters.Hostname;
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
    case Parameters.NetTypeGroup of
      ngMySQL: begin
        Result.Values[_('Client version (libmysql)')] := DecodeApiString(mysql_get_client_info);
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
          Result.Values[_(rx.Match[1])] := Val;
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
  ColSpec, Quotes: String;
  rx, rxCol: TRegExpr;
  i, LiteralStart: Integer;
  InLiteral: Boolean;
  Col: TTableColumn;
  Key: TTableKey;
  ForeignKey: TForeignKey;
  Collations: TDBQuery;
const
  QuoteReplacement = '{{}}';
begin
  Ping(True);
  if Assigned(Columns) then Columns.Clear;
  if Assigned(Keys) then Keys.Clear;
  if Assigned(ForeignKeys) then ForeignKeys.Clear;
  if CreateTable = '' then
    Exit;
  Collations := CollationTable;
  Quotes := QuoteRegExprMetaChars(FQuoteChars);
  rx := TRegExpr.Create;
  rx.ModifierS := False;
  rx.ModifierM := True;
  rx.Expression := '^\s+(['+Quotes+'].+),?$';
  rxCol := TRegExpr.Create;
  rxCol.ModifierI := True;
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(Columns) then
      break;
    ColSpec := rx.Match[1];

    Col := TTableColumn.Create(Self);
    Columns.Add(Col);
    Col.Name := ExtractIdentifier(ColSpec);
    Col.OldName := Col.Name;
    Col.Status := esUntouched;
    Col.LengthCustomized := False;

    // Datatype
    Col.DataType := GetDatatypeByName(ColSpec, True);
    Col.OldDataType := Col.DataType;

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
    rxCol.Expression := '(NULL|CURRENT_TIMESTAMP(\(\d+\))?)(\s+ON\s+UPDATE\s+CURRENT_TIMESTAMP(\(\d+\))?)?';
    if UpperCase(Copy(ColSpec, 1, 14)) = 'AUTO_INCREMENT' then begin
      Col.DefaultType := cdtAutoInc;
      Col.DefaultText := 'AUTO_INCREMENT';
      Delete(ColSpec, 1, 15);
    end else if UpperCase(Copy(ColSpec, 1, 8)) = 'DEFAULT ' then begin
      Delete(ColSpec, 1, 8);
      if rxCol.Exec(ColSpec) then begin
        if rxCol.Match[1] = 'NULL' then begin
          Col.DefaultType := cdtNull;
          Col.DefaultText := 'NULL';
          if rxCol.Match[3] <> '' then
            Col.DefaultType := cdtNullUpdateTS;
        end else begin
          Col.DefaultType := cdtCurTS;
          Col.DefaultText := 'CURRENT_TIMESTAMP';
          if rxCol.Match[3] <> '' then
            Col.DefaultType := cdtCurTSUpdateTS;
        end;
        Delete(ColSpec, 1, rxCol.MatchLen[0]);
      end else if (ColSpec[1] = '''') or (Copy(ColSpec, 1, 2) = 'b''') then begin
        InLiteral := True;
        LiteralStart := Pos('''', ColSpec)+1;
        for i:=LiteralStart to Length(ColSpec) do begin
          if ColSpec[i] = '''' then
            InLiteral := not InLiteral
          else if not InLiteral then
            break;
        end;
        Col.DefaultType := cdtText;
        Col.DefaultText := Copy(ColSpec, LiteralStart, i-LiteralStart-1);
        // A linefeed needs to display as "\n" but a single quote must not contain a backslash here
        Col.DefaultText := EscapeString(UnescapeString(Col.DefaultText), False, False);
        Col.DefaultText := StringReplace(Col.DefaultText, '\''', '''', [rfReplaceAll]);
        Delete(ColSpec, 1, i);
      end;
    end;

    // Comment
    Col.Comment := ExtractComment(ColSpec);

    if not rx.ExecNext then
      break;
  end;

  // Detect keys
  // PRIMARY KEY (`id`), UNIQUE KEY `id` (`id`), KEY `id_2` (`id`) USING BTREE,
  // KEY `Text` (`Text`(100)), FULLTEXT KEY `Email` (`Email`,`Text`)
  rx.Expression := '^\s+((\w+)\s+)?KEY\s+(['+Quotes+']?([^'+Quotes+']+)['+Quotes+']?\s+)?((USING|TYPE)\s+(\w+)\s+)?\((.+)\)(\s+USING\s+(\w+))?(\s+KEY_BLOCK_SIZE(\s|\=)+\d+)?,?$';
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(Keys) then
      break;
    Key := TTableKey.Create(Self);
    Keys.Add(Key);
    Key.Name := rx.Match[4];
    if Key.Name = '' then Key.Name := rx.Match[2]; // PRIMARY
    Key.Name := StringReplace(Key.Name, QuoteReplacement, FQuoteChar, [rfReplaceAll]);
    Key.OldName := Key.Name;
    Key.IndexType := rx.Match[2];
    Key.OldIndexType := Key.IndexType;
    if rx.Match[6] <> '' then // 5.0 and below show USING ... before column list
      Key.Algorithm := rx.Match[7]
    else
      Key.Algorithm := rx.Match[10];
    if Key.IndexType = '' then Key.IndexType := 'KEY'; // KEY
    Key.Columns := Explode(',', rx.Match[8]);
    for i:=0 to Key.Columns.Count-1 do begin
      rxCol.Expression := '^['+Quotes+']?([^'+Quotes+']+)['+Quotes+']?(\((\d+)\))?$';
      if rxCol.Exec(Key.Columns[i]) then begin
        Key.Columns[i] := rxCol.Match[1];
        Key.SubParts.Add(rxCol.Match[3]);
      end;
      Key.Columns[i] := StringReplace(Key.Columns[i], QuoteReplacement, FQuoteChar, [rfReplaceAll]);
    end;
    if not rx.ExecNext then
      break;
  end;

  // Detect foreign keys
  // CONSTRAINT `FK1` FOREIGN KEY (`which`) REFERENCES `fk1` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
  rx.Expression := '\s+CONSTRAINT\s+['+Quotes+']([^'+Quotes+']+)['+Quotes+']\sFOREIGN KEY\s+\(([^\)]+)\)\s+REFERENCES\s+['+Quotes+']([^\(]+)['+Quotes+']\s\(([^\)]+)\)(\s+ON DELETE (RESTRICT|CASCADE|SET NULL|NO ACTION))?(\s+ON UPDATE (RESTRICT|CASCADE|SET NULL|NO ACTION))?';
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(ForeignKeys) then
      break;
    ForeignKey := TForeignKey.Create(Self);
    ForeignKeys.Add(ForeignKey);
    ForeignKey.KeyName := rx.Match[1];
    ForeignKey.KeyName := StringReplace(ForeignKey.KeyName, QuoteReplacement, FQuoteChar, [rfReplaceAll]);
    ForeignKey.OldKeyName := ForeignKey.KeyName;
    ForeignKey.KeyNameWasCustomized := True;
    ForeignKey.ReferenceTable := StringReplace(rx.Match[3], '`', '', [rfReplaceAll]);
    ForeignKey.ReferenceTable := StringReplace(ForeignKey.ReferenceTable, '"', '', [rfReplaceAll]);
    ForeignKey.ReferenceTable := StringReplace(ForeignKey.ReferenceTable, '[', '', [rfReplaceAll]);
    ForeignKey.ReferenceTable := StringReplace(ForeignKey.ReferenceTable, ']', '', [rfReplaceAll]);
    ForeignKey.ReferenceTable := StringReplace(ForeignKey.ReferenceTable, QuoteReplacement, FQuoteChar, [rfReplaceAll]);
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


procedure TDBConnection.ParseViewStructure(CreateCode: String; DBObj: TDBObject; Columns: TTableColumnList;
  var Algorithm, Definer, SQLSecurity, CheckOption, SelectCode: String);
var
  rx: TRegExpr;
  Col: TTableColumn;
  Results: TDBQuery;
  SchemaClause, DataType: String;
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
    rx.Expression := 'CREATE\s+(OR\s+REPLACE\s+)?'+
      '(ALGORITHM\s*=\s*(\w*)\s*)?'+
      '(DEFINER\s*=\s*(\S+)\s+)?'+
      '(SQL\s+SECURITY\s+\w+\s+)?'+
      'VIEW\s+[^\(]+\s+'+
      '(\([^\)]+\)\s+)?'+
      'AS\s+(.+)(\s+WITH\s+(\w+\s+)?CHECK\s+OPTION\s*)?$';
    if rx.Exec(CreateCode) then begin
      Algorithm := rx.Match[3];
      Definer := DeQuoteIdent(rx.Match[5], '@');
      CheckOption := Trim(rx.Match[10]);
      SelectCode := rx.Match[8];
    end else
      raise Exception.CreateFmt(_('Regular expression did not match the VIEW code in %s: %s'), ['ParseViewStructure()', CRLF+CRLF+CreateCode]);
    rx.Free;
  end;

  if Assigned(Columns) then begin
    Columns.Clear;
    rx := TRegExpr.Create;
    rx.Expression := '(\((.+)\))(\s+unsigned)?(\s+zerofill)?';
    if DBObj.Schema <> '' then
      SchemaClause := 'AND TABLE_SCHEMA='+EscapeString(DBObj.Schema)
    else
      SchemaClause := 'AND '+GetSQLSpecifity(spISTableSchemaCol)+'='+EscapeString(DBObj.Database);
    Results := GetResults('SELECT * '+
      'FROM INFORMATION_SCHEMA.COLUMNS '+
      'WHERE '+
      '  TABLE_NAME='+EscapeString(DBObj.Name)+' '+
      SchemaClause
      );
    while not Results.Eof do begin
      Col := TTableColumn.Create(Self);
      Columns.Add(Col);
      Col.Name := Results.Col('COLUMN_NAME');
      Col.AllowNull := UpperCase(Results.Col('IS_NULLABLE')) = 'YES';
      DataType := Results.Col('DATA_TYPE');
      Col.DataType := GetDatatypeByName(DataType, False);
      if Results.ColExists('COLUMN_TYPE') then begin
        // Use MySQL's proprietary column_type - the only way to get SET and ENUM values
        if rx.Exec(Results.Col('COLUMN_TYPE')) then begin
          Col.LengthSet := rx.Match[2];
          if Col.DataType.Category in [dtcInteger, dtcReal] then begin
            Col.Unsigned := rx.Match[3] <> '';
            Col.ZeroFill := rx.Match[4] <> '';
          end;
        end;
      end else begin
        if not Results.IsNull('CHARACTER_MAXIMUM_LENGTH') then begin
          Col.LengthSet := Results.Col('CHARACTER_MAXIMUM_LENGTH');
        end else if not Results.IsNull('NUMERIC_PRECISION') then begin
          Col.LengthSet := Results.Col('NUMERIC_PRECISION');
          if not Results.IsNull('NUMERIC_SCALE') then
            Col.LengthSet := Col.LengthSet + ',' + Results.Col('NUMERIC_SCALE');
        end;
        if Col.LengthSet = '-1' then
          Col.LengthSet := 'max';
      end;
      Col.Collation := Results.Col('COLLATION_NAME');
      Col.Comment := Results.Col('COLUMN_COMMENT', True);
      Col.DefaultText := Results.Col('COLUMN_DEFAULT');
      if Results.IsNull('COLUMN_DEFAULT') then begin
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
    Param.Datatype := rx.Match[5];
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
  Obj.Comment := ExtractComment(Body);
  Obj.Body := TrimLeft(Body);
  rx.Free;
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
  FRecNo := -1;
  FRecordCount := 0;
  FColumnNames := TStringList.Create;
  FColumnNames.CaseSensitive := True;
  FColumnOrgNames := TStringList.Create;
  FColumnOrgNames.CaseSensitive := True;
  FStoreResult := True;
  FDBObject := nil;
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


destructor TPGQuery.Destroy;
var
  i: Integer;
begin
  if HasResult then for i:=Low(FResultList) to High(FResultList) do
    PQclear(FResultList[i]);
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
        if (Field.flags and AUTO_INCREMENT_FLAG) = AUTO_INCREMENT_FLAG then
          FAutoIncrementColumn := i;
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
            TypeIndex := dtEnum;
          ftDate:
            TypeIndex := dtDate;
          ftTime:
            TypeIndex := dtTime;
          ftDateTime:
            TypeIndex := dtDateTime;
          else
            raise EDatabaseError.CreateFmt(_('Unknown data type for column #%d - %s: %d'), [i, FColumnNames[i], Integer(LastResult.Fields[i].DataType)]);
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
  i, j, NumFields: Integer;
  NumResults: Integer;
  FieldTypeOID: POid;
  LastResult: PPGresult;
  TypeFound: Boolean;
  rx: TRegExpr;
begin
  if UseRawResult = -1 then begin
    Connection.Query(FSQL, FStoreResult);
    UseRawResult := 0;
  end;
  if Connection.ResultCount > UseRawResult then
    LastResult := TPGConnection(Connection).LastRawResults[UseRawResult]
  else
    LastResult := nil;
  if AddResult and (Length(FResultList) = 0) then
    AddResult := False;
  if AddResult then
    NumResults := Length(FResultList)+1
  else begin
    for i:=Low(FResultList) to High(FResultList) do
      PQclear(FResultList[i]);
    NumResults := 1;
    FRecordCount := 0;
    FAutoIncrementColumn := -1;
    FEditingPrepared := False;
  end;
  if LastResult <> nil then begin
    Connection.Log(lcDebug, 'Result #'+IntToStr(NumResults)+' fetched.');
    SetLength(FResultList, NumResults);
    FResultList[NumResults-1] := LastResult;
    FRecordCount := FRecordCount + PQntuples(LastResult);
  end;
  if not AddResult then begin
    if HasResult then begin
      // FCurrentResults is normally done in SetRecNo, but never if result has no rows
      FCurrentResults := LastResult;
      NumFields := PQnfields(LastResult);
      SetLength(FColumnTypes, NumFields);
      SetLength(FColumnLengths, NumFields);
      SetLength(FColumnFlags, NumFields);
      FColumnNames.Clear;
      FColumnOrgNames.Clear;
      rx := TRegExpr.Create;
      for i:=0 to NumFields-1 do begin
        FColumnNames.Add(Connection.DecodeAPIString(PQfname(LastResult, i)));
        FColumnOrgNames.Add(FColumnNames[FColumnNames.Count-1]);
        FieldTypeOID :=  PQftype(LastResult, i);
        TypeFound := False;
        for j:=0 to High(FConnection.Datatypes) do begin
          if FConnection.Datatypes[j].NativeTypes = '' then
            Continue;
          rx.Expression := '\b('+FConnection.Datatypes[j].NativeTypes+')\b';
          if rx.Exec(IntToStr(FieldTypeOID)) then begin
            FColumnTypes[i] := FConnection.Datatypes[j];
            TypeFound := True;
            break;
          end;
        end;
        if not TypeFound then begin
          // Fall back to text type
          FColumnTypes[i] := FConnection.Datatypes[11];
        end;
      end;
      rx.Free;
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
  Row: TRowData;
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
        Inc(NumRows, PQntuples(FResultList[i]));
        if NumRows > Value then begin
          FCurrentResults := FResultList[i];
          FRecNoLocal := PQntuples(FCurrentResults)-(NumRows-Value);
          FCurrentUpdateRow := nil;
          for j:=Low(FColumnLengths) to High(FColumnLengths) do
            FColumnLengths[j] := PQgetlength(FCurrentResults, FRecNoLocal, j);
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
  Field: PMYSQL_FIELD;
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
        Field := mysql_fetch_field_direct(FCurrentResults, column);
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
    Raise EDatabaseError.CreateFmt(_(MsgInvalidColumn), [Column, ColumnCount, RecordCount]);
end;


function TAdoDBQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
begin
  // Catch broken connection
  if not FConnection.Active then begin
    Result := '';
  end else if (Column > -1) and (Column < ColumnCount) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      Result := FCurrentUpdateRow[Column].NewText;
    end else begin
      try
        Result := FCurrentResults.Fields[Column].AsString;
      except
        Result := String(FCurrentResults.Fields[Column].AsAnsiString);
      end;
      if Datatype(Column).Index = dtBit then begin
        if UpperCase(Result) = 'TRUE' then
          Result := '1'
        else
          Result := '0';
      end;
    end;
  end else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt(_(MsgInvalidColumn), [Column, ColumnCount, RecordCount]);
end;


function TPGQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
var
  AnsiStr: AnsiString;
begin
  if (Column > -1) and (Column < ColumnCount) then begin
    if FEditingPrepared and Assigned(FCurrentUpdateRow) then begin
      Result := FCurrentUpdateRow[Column].NewText;
    end else begin
      SetString(AnsiStr, PQgetvalue(FCurrentResults, FRecNoLocal, Column), FColumnLengths[Column]);
      if Datatype(Column).Category in [dtcBinary, dtcSpatial] then
        Result := String(AnsiStr)
      else if Datatype(Column).Index = dtbool then
        if AnsiStr='t' then Result := 'true' else Result := 'false'
      else
        Result := Connection.DecodeAPIString(AnsiStr);
    end;
  end else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt(_(MsgInvalidColumn), [Column, ColumnCount, RecordCount]);
end;


function TDBQuery.Col(ColumnName: String; IgnoreErrors: Boolean=False): String;
var
  idx: Integer;
begin
  idx := ColumnNames.IndexOf(ColumnName);
  if idx = -1 then
    idx := ColumnNames.IndexOf(LowerCase(ColumnName));
  if idx > -1 then
    Result := Col(idx)
  else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt(_('Column "%s" not available.'), [ColumnName]);
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
    raise EDatabaseError.CreateFmt(_('Column #%s not available.'), [IntToStr(Column)]);
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


function TPGQuery.ColIsPrimaryKeyPart(Column: Integer): Boolean;
begin
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


function TPGQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
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
  // Catch broken connection
  if not FConnection.Active then
    Result := False
  else if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsNull
  else
    Result := FCurrentResults.Fields[Column].IsNull;
end;


function TPGQuery.IsNull(Column: Integer): Boolean;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsNull
  else
    Result := PQgetisnull(FCurrentResults, FRecNoLocal, Column) = 1;
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


procedure TDBQuery.PrepareEditing;
var
  CreateCode, Dummy, DB: String;
  DBObjects: TDBObjectList;
  LObj, Obj: TDBObject;
begin
  // Try to fetch column names and keys
  if FEditingPrepared then
    Exit;
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
      if (LObj.NodeType in [lntTable, lntView]) and (LObj.Name = TableName) then begin
        Obj := LObj;
        break;
      end;
    end;
    if Obj = nil then
      raise EDatabaseError.Create(f_('Could not find table or view %s.%s. Please refresh database tree.', [DB, TableName]));
  end;
  CreateCode := Connection.GetCreateCode(Obj.Database, Obj.Schema, Obj.Name, Obj.NodeType);
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
  case Obj.NodeType of
    lntTable:
      Connection.ParseTableStructure(CreateCode, FColumns, FKeys, FForeignKeys);
    lntView:
      Connection.ParseViewStructure(CreateCode, Obj, FColumns, Dummy, Dummy, Dummy, Dummy, Dummy);
  end;  
  FreeAndNil(FUpdateData);
  FUpdateData := TUpdateData.Create(True);
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
      raise EDatabaseError.Create(FormatNumber(Connection.RowsAffected)+' rows deleted when that should have been 1.');
  end;
  if Assigned(FCurrentUpdateRow) then begin
    FUpdateData.Remove(FCurrentUpdateRow);
    FCurrentUpdateRow := nil;
    FRecNo := -1;
  end;
end;


function TDBQuery.InsertRow: Int64;
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
    c.OldIsFunction := False;
    c.OldIsNull := False;
    ColAttr := ColAttributes(i);
    if Assigned(ColAttr) then begin
      c.OldIsNull := ColAttr.DefaultType in [cdtNull, cdtNullUpdateTS, cdtAutoInc];
      if ColAttr.DefaultType in [cdtText, cdtTextUpdateTS] then
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
  if Null then
    FCurrentUpdateRow[Column].NewText := ''
  else
    FCurrentUpdateRow[Column].NewText := NewText;
  FCurrentUpdateRow[Column].Modified := (FCurrentUpdateRow[Column].NewText <> FCurrentUpdateRow[Column].OldText) or
    (FCurrentUpdateRow[Column].NewIsNull <> FCurrentUpdateRow[Column].OldIsNull) or
    (FCurrentUpdateRow[Column].NewIsFunction <> FCurrentUpdateRow[Column].OldIsFunction)
    ;
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
  Row: TRowData;
  Cell: TCellData;
  sqlUpdate, sqlInsertColumns, sqlInsertValues, Val: String;
  RowModified: Boolean;
  ColAttr: TTableColumn;
begin
  Result := True;
  if not FEditingPrepared then
    raise EDatabaseError.Create(_('Internal error: Cannot post modifications before editing was prepared.'));

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
          Val := Cell.NewText;
          if (Datatype(i).Index = dtBit) and FConnection.Parameters.IsMySQL then
            Val := 'b' + Connection.EscapeString(Val);
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
          raise EDatabaseError.Create(FormatNumber(Connection.RowsAffected)+' rows updated when that should have been 1.');
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
      Field := mysql_fetch_field_direct(FCurrentResults, i);
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
      raise EDatabaseError.Create(_('More than one table involved.'));
    if Field.org_table <> '' then begin
      tbl := Field.org_table;
      db := Field.db;
    end;
  end;
  if tbl = '' then
    raise EDatabaseError.Create(_('Could not determine name of table.'))
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


function TPGQuery.TableName: String;
begin
  // TODO
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
    raise EDatabaseError.Create(_(MSG_NOGRIDEDITING));
  // All column names must be present in order to send valid INSERT/UPDATE/DELETE queries
  for i:=0 to KeyCols.Count-1 do begin
    if FColumnOrgNames.IndexOf(KeyCols[i]) = -1 then
      raise EDatabaseError.Create(_(MSG_NOGRIDEDITING));
  end;
  for i:=0 to FColumnOrgNames.Count-1 do begin
    if FColumnOrgNames[i] = '' then
      raise EDatabaseError.CreateFmt(_('Column #%d has an undefined origin: %s'), [i, ColumnNames[i]]);
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
      raise EDatabaseError.CreateFmt(_('Cannot compose WHERE clause - column missing: %s'), [NeededCols[i]]);
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
    Schema := s.Schema;
    Database := s.Database;
    NodeType := s.NodeType;
    GroupType := s.GroupType;
    Created := s.Created;
    Updated := s.Updated;
    Comment := s.Comment;
    Rows := s.Rows;
    Size := s.Size;
    FCreateCode := s.FCreateCode;
    FCreateCodeFetched := s.FCreateCodeFetched;
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
      and (Schema = CompareTo.Schema)
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
  if not FCreateCodeFetched then try
    CreateCode := Connection.GetCreateCode(Database, Schema, Name, NodeType);
  except on E:Exception do
    Connection.Log(lcError, E.Message);
  end;
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
  if FConnection.Parameters.NetTypeGroup = ngPgSQL then
    Result := Connection.QuoteIdent(Schema, AlwaysQuote)
  else
    Result := Connection.QuoteIdent(Database, AlwaysQuote);
end;

function TDBObject.QuotedName(AlwaysQuote: Boolean=True): String;
begin
  Result := '';
  if FConnection.Parameters.IsMSSQL then begin
    if Schema <> '' then
      Result := Result + Connection.QuoteIdent(Schema, AlwaysQuote);
    Result := Result + '.';
  end;
  Result := Result + Connection.QuoteIdent(Name, AlwaysQuote);
end;

function TDBObject.QuotedDbAndTableName(AlwaysQuote: Boolean=True): String;
begin
  Result := QuotedDatabase(AlwaysQuote) + '.' + QuotedName(AlwaysQuote);
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

function TTableColumn.SQLCode(OverrideCollation: String=''): String;
var
  IsVirtual: Boolean;
  Text, TSLen: String;
begin
  Result := FConnection.QuoteIdent(Name) + ' ' +DataType.Name;
  IsVirtual := (Expression <> '') and (Virtuality <> '');
  if (LengthSet <> '') and DataType.HasLength then
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
    Text := esc(DefaultText);
    // Support BIT syntax in MySQL
    if DataType.Index = dtBit then
      Text := 'b'+Text;
    TSLen := '';
    if LengthSet <> '' then
      TSLen := '('+LengthSet+')';
    Result := Result + ' ';
    case DefaultType of
      // cdtNothing:
      cdtText:           Result := Result + 'DEFAULT '+Text;
      cdtTextUpdateTS:   Result := Result + 'DEFAULT '+Text+' ON UPDATE CURRENT_TIMESTAMP'+TSLen;
      cdtNull:           Result := Result + 'DEFAULT NULL';
      cdtNullUpdateTS:   Result := Result + 'DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP'+TSLen;
      cdtCurTS:          Result := Result + 'DEFAULT CURRENT_TIMESTAMP'+TSLen;
      cdtCurTSUpdateTS:  Result := Result + 'DEFAULT CURRENT_TIMESTAMP'+TSLen+' ON UPDATE CURRENT_TIMESTAMP'+TSLen;
      cdtAutoInc:        Result := Result + 'AUTO_INCREMENT';
    end;
    Result := TrimRight(Result); // Remove whitespace for columns without default value
  end;
  if IsVirtual then
    Result := Result + ' AS ('+Expression+') '+Virtuality;
  if Comment <> '' then
    Result := Result + ' COMMENT '+esc(Comment);
  if Collation <> '' then begin
    Result := Result + ' COLLATE ';
    if OverrideCollation <> '' then
      Result := Result + esc(OverrideCollation)
    else
      Result := Result + esc(Collation);
  end;
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

function TTableKey.GetImageIndex: Integer;
begin
  // Detect key icon index for specified index
  if IndexType = PKEY then Result := ICONINDEX_PRIMARYKEY
  else if IndexType = KEY then Result := ICONINDEX_INDEXKEY
  else if IndexType = UKEY then Result := ICONINDEX_UNIQUEKEY
  else if IndexType = FKEY then Result := ICONINDEX_FULLTEXTKEY
  else if IndexType = SKEY then Result := ICONINDEX_SPATIALKEY
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
    else raise EDatabaseError.CreateFmt(_('Unsupported type (%d) in %s.'), [_type, 'mysql_authentication_dialog_ask']);
  end;
  Dialog.Free;
end;


initialization


finalization

// Release libmysql.dll handle
if LibMysqlHandle <> 0 then begin
  FreeLibrary(LibMysqlHandle);
  LibMysqlHandle := 0;
end;


end.
