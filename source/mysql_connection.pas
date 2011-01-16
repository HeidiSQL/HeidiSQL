unit mysql_connection;

interface

uses
  Classes, SysUtils, windows, mysql_api, mysql_structures, SynRegExpr, Contnrs, Generics.Collections, Generics.Defaults,
  DateUtils, Types, ShellApi, Math, Dialogs;

type
  { TDBObjectList and friends }

  TListNodeType = (lntNone, lntDb, lntTable, lntView, lntFunction, lntProcedure, lntTrigger, lntEvent, lntColumn);
  TListNodeTypes = Set of TListNodeType;
  TMySQLConnection = class;
  TDBObject = class(TPersistent)
    private
      FCreateCode: String;
      FCreateCodeFetched: Boolean;
      FConnection: TMySQLConnection;
      function GetObjType: String;
      function GetImageIndex: Integer;
      function GetCreateCode: String;
      procedure SetCreateCode(Value: String);
    public
      Name, Database, Column, Engine, Comment, RowFormat, CreateOptions, Collation: String;
      Created, Updated, LastChecked: TDateTime;
      Rows, Size, Version, AvgRowLen, MaxDataLen, IndexLen, DataLen, DataFree, AutoInc, CheckSum: Int64;
      NodeType: TListNodeType;
      constructor Create(OwnerConnection: TMySQLConnection);
      procedure Assign(Source: TPersistent); override;
      function IsSameAs(CompareTo: TDBObject): Boolean;
      property ObjType: String read GetObjType;
      property ImageIndex: Integer read GetImageIndex;
      property CreateCode: String read GetCreateCode write SetCreateCode;
      property Connection: TMySQLConnection read FConnection;
  end;
  PDBObject = ^TDBObject;
  TDBObjectList = class(TObjectList<TDBObject>)
    private
      FDatabase: String;
      FDataSize: Int64;
      FLastUpdate: TDateTime;
    public
      property Database: String read FDatabase;
      property DataSize: Int64 read FDataSize;
      property LastUpdate: TDateTime read FLastUpdate;
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
      procedure SetStatus(Value: TEditingStatus);
    public
      Name, OldName: String;
      DataType: TDatatype;
      LengthSet: String;
      Unsigned, AllowNull, ZeroFill, LengthCustomized: Boolean;
      DefaultType: TColumnDefaultType;
      DefaultText: String;
      Comment, Charset, Collation: String;
      FStatus: TEditingStatus;
      constructor Create;
      destructor Destroy; override;
      function SQLCode: String;
      property Status: TEditingStatus read FStatus write SetStatus;
  end;
  PTableColumn = ^TTableColumn;
  TTableColumnList = TObjectList<TTableColumn>;

  TTableKey = class(TObject)
    public
      Name, OldName: String;
      IndexType, OldIndexType, Algorithm: String;
      Columns, SubParts: TStringList;
      Modified, Added: Boolean;
      constructor Create;
      destructor Destroy; override;
      procedure Modification(Sender: TObject);
      function SQLCode: String;
  end;
  TTableKeyList = TObjectList<TTableKey>;

  // Helper object to manage foreign keys in a TObjectList
  TForeignKey = class(TObject)
    public
      KeyName, OldKeyName, ReferenceTable, OnUpdate, OnDelete: String;
      Columns, ForeignColumns: TStringList;
      Modified, Added, KeyNameWasCustomized: Boolean;
      constructor Create;
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

  TNetType = (ntTCPIP, ntNamedPipe, ntSSHtunnel);

  TMySQLClientOption = (
    opCompress,             // CLIENT_COMPRESS
    opConnectWithDb,        // CLIENT_CONNECT_WITH_DB
    opFoundRows,            // CLIENT_FOUND_ROWS
    opIgnoreSigpipe,        // CLIENT_IGNORE_SIGPIPE
    opIgnoreSpace,          // CLIENT_IGNORE_SPACE
    opInteractive,          // CLIENT_INTERACTIVE
    opLocalFiles,           // CLIENT_LOCAL_FILES
    opLongFlag,             // CLIENT_LONG_FLAG
    opLongPassword,         // CLIENT_LONG_PASSWORD
    opMultiResults,         // CLIENT_MULTI_RESULTS
    opMultiStatements,      // CLIENT_MULTI_STATEMENTS
    opNoSchema,             // CLIENT_NO_SCHEMA
    opODBC,                 // CLIENT_ODBC
    opProtocol41,           // CLIENT_PROTOCOL_41
    opRememberOptions,      // CLIENT_REMEMBER_OPTIONS
    opReserved,             // CLIENT_RESERVED
    opSecureConnection,     // CLIENT_SECURE_CONNECTION
    opSSL,                  // CLIENT_SSL
    opTransactions          // CLIENT_TRANSACTIONS
    );
  TMySQLClientOptions = set of TMySQLClientOption;

  TConnectionParameters = class(TObject)
    strict private
      FNetType: TNetType;
      FHostname, FUsername, FPassword, FAllDatabases, FStartupScriptFilename,
      FSSLPrivateKey, FSSLCertificate, FSSLCACertificate,
      FSSHHost, FSSHUser, FSSHPassword, FSSHPlinkExe, FSSHPrivateKey: String;
      FPort, FSSHPort, FSSHLocalPort, FSSHTimeout: Integer;
      FOptions: TMySQLClientOptions;
      FLoginPrompt: Boolean;
    public
      constructor Create;
    published
      property NetType: TNetType read FNetType write FNetType;
      property Hostname: String read FHostname write FHostname;
      property Port: Integer read FPort write FPort;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
      property AllDatabases: String read FAllDatabases write FAllDatabases;
      property StartupScriptFilename: String read FStartupScriptFilename write FStartupScriptFilename;
      property Options: TMySQLClientOptions read FOptions write FOptions;
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


  { TMySQLConnection }

  TMySQLLogCategory = (lcInfo, lcSQL, lcUserFiredSQL, lcError, lcDebug);
  TMySQLLogEvent = procedure(Msg: String; Category: TMySQLLogCategory=lcInfo; Connection: TMySQLConnection=nil) of object;
  TMySQLDatabaseEvent = procedure(Connection: TMySQLConnection; Database: String) of object;

  TMySQLQuery = class;
  TMySQLQueryList = TObjectList<TMySQLQuery>;
  TMySQLConnection = class(TComponent)
    private
      FHandle: PMYSQL;
      FActive: Boolean;
      FConnectionStarted: Integer;
      FServerStarted: Integer;
      FSessionName: String;
      FParameters: TConnectionParameters;
      FLoginPromptDone: Boolean;
      FDatabase: String;
      FAllDatabases: TStringList;
      FLogPrefix: String;
      FOnLog: TMySQLLogEvent;
      FOnDatabaseChanged: TMySQLDatabaseEvent;
      FOnDBObjectsCleared: TMySQLDatabaseEvent;
      FRowsFound: Int64;
      FRowsAffected: Int64;
      FServerOS: String;
      FServerVersionUntouched: String;
      FRealHostname: String;
      FLastQueryDuration, FLastQueryNetworkDuration: Cardinal;
      FLastQuerySQL: String;
      FIsUnicode: Boolean;
      FTableEngines: TStringList;
      FTableEngineDefault: String;
      FCollationTable: TMySQLQuery;
      FCharsetTable: TMySQLQuery;
      FInformationSchemaObjects: TStringList;
      FDatabases: TDatabaseList;
      FObjectNamesInSelectedDB: TStrings;
      FPlinkProcInfo: TProcessInformation;
      FLastResults: Array of PMYSQL_RES;
      FResultCount: Integer;
      FCurrentUserHostCombination: String;
      procedure SetActive(Value: Boolean);
      procedure ClosePlink;
      procedure SetDatabase(Value: String);
      function GetThreadId: Cardinal;
      function GetCharacterSet: String;
      procedure SetCharacterSet(CharsetName: String);
      function GetLastError: String;
      function GetServerVersionStr: String;
      function GetServerVersionInt: Integer;
      function GetAllDatabases: TStringList;
      function GetTableEngines: TStringList;
      function GetCollationTable: TMySQLQuery;
      function GetCollationList: TStringList;
      function GetCharsetTable: TMySQLQuery;
      function GetCharsetList: TStringList;
      function GetInformationSchemaObjects: TStringList;
      function GetConnectionUptime: Integer;
      function GetServerUptime: Integer;
      function GetCurrentUserHostCombination: String;
      function DecodeAPIString(a: AnsiString): String;
      procedure Log(Category: TMySQLLogCategory; Msg: String);
      procedure ClearCache(IncludeDBObjects: Boolean);
      procedure SetObjectNamesInSelectedDB;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TMySQLLogCategory=lcSQL): PMYSQL_RES;
      function EscapeString(Text: String; ProcessJokerChars: Boolean=False): String;
      function escChars(const Text: String; EscChar, Char1, Char2, Char3, Char4: Char): String;
      function UnescapeString(Text: String): String;
      function ConvertServerVersion(Version: Integer): String;
      function GetResults(SQL: String): TMySQLQuery;
      function GetCol(SQL: String; Column: Integer=0): TStringList;
      function GetVar(SQL: String; Column: Integer=0): String; overload;
      function GetVar(SQL: String; Column: String): String; overload;
      function Ping: Boolean;
      function RefreshAllDatabases: TStringList;
      function GetDBObjects(db: String; Refresh: Boolean=False): TDBObjectList;
      function DbObjectsCached(db: String): Boolean;
      function ParseDateTime(Str: String): TDateTime;
      function GetKeyColumns(Columns: TTableColumnList; Keys: TTableKeyList): TStringList;
      function ConnectionInfo: TStringList;
      function GetLastResults: TMySQLQueryList;
      procedure ClearDbObjects(db: String);
      procedure ClearAllDbObjects;
      procedure ParseTableStructure(CreateTable: String; Columns: TTableColumnList; Keys: TTableKeyList; ForeignKeys: TForeignKeyList);
      procedure ParseViewStructure(CreateCode, ViewName: String; Columns: TTableColumnList; var Algorithm, Definer, CheckOption, SelectCode: String);
      procedure ParseRoutineStructure(CreateCode: String; Parameters: TRoutineParamList;
        var Deterministic: Boolean; var Definer, Returns, DataAccess, Security, Comment, Body: String);
      property SessionName: String read FSessionName write FSessionName;
      property Parameters: TConnectionParameters read FParameters write FParameters;
      property ThreadId: Cardinal read GetThreadId;
      property ConnectionUptime: Integer read GetConnectionUptime;
      property ServerUptime: Integer read GetServerUptime;
      property CharacterSet: String read GetCharacterSet write SetCharacterSet;
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
      property AllDatabases: TStringList read GetAllDatabases;
      property TableEngines: TStringList read GetTableEngines;
      property TableEngineDefault: String read FTableEngineDefault;
      property CollationTable: TMySQLQuery read GetCollationTable;
      property CollationList: TStringList read GetCollationList;
      property CharsetTable: TMySQLQuery read GetCharsetTable;
      property CharsetList: TStringList read GetCharsetList;
      property InformationSchemaObjects: TStringList read GetInformationSchemaObjects;
      property ObjectNamesInSelectedDB: TStrings read FObjectNamesInSelectedDB write FObjectNamesInSelectedDB;
      property ResultCount: Integer read FResultCount;
      property CurrentUserHostCombination: String read GetCurrentUserHostCombination;
    published
      property Active: Boolean read FActive write SetActive default False;
      property Database: String read FDatabase write SetDatabase;
      property LogPrefix: String read FLogPrefix write FLogPrefix;
      // Events
      property OnLog: TMySQLLogEvent read FOnLog write FOnLog;
      property OnDatabaseChanged: TMySQLDatabaseEvent read FOnDatabaseChanged write FOnDatabaseChanged;
      property OnDBObjectsCleared: TMySQLDatabaseEvent read FOnDBObjectsCleared write FOnDBObjectsCleared;
  end;
  TMySQLConnectionList = TObjectList<TMySQLConnection>;


  { TMySQLQuery }

  TMySQLQuery = class(TComponent)
    private
      FSQL: String;
      FConnection: TMySQLConnection;
      FRecNo,
      FRecordCount: Int64;
      FColumnNames: TStringList;
      FColumnOrgNames: TStringList;
      FColumnTypes: Array of TDatatype;
      FColumnLengths: TIntegerDynArray;
      FColumnFlags: TCardinalDynArray;
      FResultList: Array of PMYSQL_RES;
      FCurrentResults: PMYSQL_RES;
      FCurrentRow: PMYSQL_ROW;
      FCurrentUpdateRow: TRowData;
      FEof: Boolean;
      FStoreResult: Boolean;
      FColumns: TTableColumnList;
      FKeys: TTableKeyList;
      FForeignKeys: TForeignKeyList;
      FEditingPrepared: Boolean;
      FUpdateData: TUpdateData;
      procedure SetSQL(Value: String);
      procedure SetRecNo(Value: Int64);
      procedure SetColumnOrgNames(Value: TStringList);
      procedure CreateUpdateRow;
      function GetKeyColumns: TStringList;
      function GetWhereClause: String;
      function ColAttributes(Column: Integer): TTableColumn;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False; Res: PMYSQL_RES=nil);
      procedure First;
      procedure Next;
      function ColumnCount: Integer;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload;
      function Col(ColumnName: String; IgnoreErrors: Boolean=False): String; overload;
      function BinColAsHex(Column: Integer; IgnoreErrors: Boolean=False): String;
      function DataType(Column: Integer): TDataType;
      function MaxLength(Column: Integer): Int64;
      function ValueList(Column: Integer): TStringList;
      function ColExists(Column: String): Boolean;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean;
      function ColIsUniqueKeyPart(Column: Integer): Boolean;
      function ColIsKeyPart(Column: Integer): Boolean;
      function IsNull(Column: Integer): Boolean; overload;
      function IsNull(Column: String): Boolean; overload;
      function HasResult: Boolean;
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
      function DatabaseName: String;
      function TableName: String;
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
      property SQL: String read FSQL write SetSQL;
      property Connection: TMySQLConnection read FConnection write FConnection;
  end;
  PMySQLQuery = ^TMySQLQuery;

  function QuoteIdent(Identifier: String; AlwaysQuote: Boolean=True; Glue: Char=#0): String;
  function DeQuoteIdent(Identifier: String; Glue: Char=#0): String;

implementation

uses helpers, loginform;



{ TConnectionParameters }

constructor TConnectionParameters.Create;
begin
  FNetType := ntTCPIP;
  FHostname := DEFAULT_HOST;
  FUsername := DEFAULT_USER;
  FPassword := '';
  FPort := DEFAULT_PORT;
  FSSHPort := DEFAULT_SSHPORT;
  FSSHTimeout := DEFAULT_SSHTIMEOUT;
  FSSHLocalPort := FPort + 1;
  FSSLPrivateKey := '';
  FSSLCertificate := '';
  FSSLCACertificate := '';
  FStartupScriptFilename := '';
  FOptions := [opCompress, opLocalFiles, opInteractive, opProtocol41, opMultiStatements];
end;



{ TMySQLConnection }

constructor TMySQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSessionName := 'Unnamed';
  FParameters := TConnectionParameters.Create;
  FRowsFound := 0;
  FRowsAffected := 0;
  FConnectionStarted := 0;
  FLastQueryDuration := 0;
  FLastQueryNetworkDuration := 0;
  FLogPrefix := '';
  FIsUnicode := False;
  FDatabases := TDatabaseList.Create(True);
  FLoginPromptDone := False;
  FCurrentUserHostCombination := '';
end;


destructor TMySQLConnection.Destroy;
begin
  if Active then Active := False;
  FOnDBObjectsCleared := nil;
  ClearCache(True);
  inherited Destroy;
end;


{**
  (Dis-)Connect to/from server
}
procedure TMySQLConnection.SetActive( Value: Boolean );
var
  Connected: PMYSQL;
  ClientFlags, FinalPort: Integer;
  Error, tmpdb, FinalHost, FinalSocket, PlinkCmd, UsernamePrompt, PasswordPrompt: String;
  SSLResult: Byte;
  UsingPass, Protocol, CurCharset: String;
  StartupInfo: TStartupInfo;
  ExitCode: LongWord;
begin
  if Value and (FHandle = nil) then begin
    // Prompt for password on initial connect
    if FParameters.LoginPrompt and (not FLoginPromptDone) then begin
      UsernamePrompt := FParameters.Username;
      PasswordPrompt := FParameters.Password;
      LoginPrompt('Login to '+FParameters.Hostname+':', UsernamePrompt, PasswordPrompt);
      FParameters.Username := UsernamePrompt;
      FParameters.Password := PasswordPrompt;
      FLoginPromptDone := True;
    end;

    // Get handle
    FHandle := mysql_init(nil);

    // Prepare connection
    case FParameters.NetType of
      ntTCPIP: Protocol := 'TCP/IP';
      ntNamedPipe: Protocol := 'named pipe';
      ntSSHtunnel: Protocol := 'SSH tunnel';
    end;
    if FParameters.Password <> '' then UsingPass := 'Yes' else UsingPass := 'No';
    Log(lcInfo, 'Connecting to '+FParameters.Hostname+' via '+Protocol+
      ', username '+FParameters.Username+
      ', using password: '+UsingPass+' ...');

    // Prepare special stuff for SSL and SSH tunnel
    FinalHost := FParameters.Hostname;
    FinalSocket := '';
    FinalPort := FParameters.Port;
    case FParameters.NetType of
      ntTCPIP: begin
        if (FParameters.SSLPrivateKey <> '') and
        (FParameters.SSLCertificate <> '') and
        (FParameters.SSLCACertificate <> '') then begin
          FParameters.Options := FParameters.Options + [opSSL];
          { TODO : Use Cipher and CAPath parameters }
          SSLResult := mysql_ssl_set(
            FHandle,
            PansiChar(AnsiString(FParameters.SSLPrivateKey)),
            PansiChar(AnsiString(FParameters.SSLCertificate)),
            PansiChar(AnsiString(FParameters.SSLCACertificate)),
            {PansiChar(AnsiString(FParameters.CApath))}nil,
            {PansiChar(AnsiString(FParameters.Cipher))}nil);
          if SSLresult <> 0 then
            raise EDatabaseError.CreateFmt('Could not connect using SSL (Error %d)', [SSLresult]);
        end;
      end;

      ntNamedPipe: begin
        FinalHost := '.';
        FinalSocket := FParameters.Hostname;
      end;

      ntSSHtunnel: begin
        // Build plink.exe command line
        // plink bob@domain.com -pw myPassw0rd1 -P 22 -i "keyfile.pem" -L 55555:localhost:3306
        PlinkCmd := FParameters.SSHPlinkExe + ' ';
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
    ClientFlags := 0;
    if opRememberOptions   in FParameters.Options then ClientFlags := ClientFlags or CLIENT_REMEMBER_OPTIONS;
    if opLongPassword      in FParameters.Options then ClientFlags := ClientFlags or CLIENT_LONG_PASSWORD;
    if opFoundRows         in FParameters.Options then ClientFlags := ClientFlags or CLIENT_FOUND_ROWS;
    if opLongFlag          in FParameters.Options then ClientFlags := ClientFlags or CLIENT_LONG_FLAG;
    if opConnectWithDb     in FParameters.Options then ClientFlags := ClientFlags or CLIENT_CONNECT_WITH_DB;
    if opNoSchema          in FParameters.Options then ClientFlags := ClientFlags or CLIENT_NO_SCHEMA;
    if opCompress          in FParameters.Options then ClientFlags := ClientFlags or CLIENT_COMPRESS;
    if opODBC              in FParameters.Options then ClientFlags := ClientFlags or CLIENT_ODBC;
    if opLocalFiles        in FParameters.Options then ClientFlags := ClientFlags or CLIENT_LOCAL_FILES;
    if opIgnoreSpace       in FParameters.Options then ClientFlags := ClientFlags or CLIENT_IGNORE_SPACE;
    if opProtocol41        in FParameters.Options then ClientFlags := ClientFlags or CLIENT_PROTOCOL_41;
    if opInteractive       in FParameters.Options then ClientFlags := ClientFlags or CLIENT_INTERACTIVE;
    if opSSL               in FParameters.Options then ClientFlags := ClientFlags or CLIENT_SSL;
    if opIgnoreSigpipe     in FParameters.Options then ClientFlags := ClientFlags or CLIENT_IGNORE_SIGPIPE;
    if opTransactions      in FParameters.Options then ClientFlags := ClientFlags or CLIENT_TRANSACTIONS;
    if opReserved          in FParameters.Options then ClientFlags := ClientFlags or CLIENT_RESERVED;
    if opSecureConnection  in FParameters.Options then ClientFlags := ClientFlags or CLIENT_SECURE_CONNECTION;
    if opMultiStatements   in FParameters.Options then ClientFlags := ClientFlags or CLIENT_MULTI_STATEMENTS;
    if opMultiResults      in FParameters.Options then ClientFlags := ClientFlags or CLIENT_MULTI_RESULTS;
    if opRememberOptions   in FParameters.Options then ClientFlags := ClientFlags or CLIENT_REMEMBER_OPTIONS;

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
      Log(lcInfo, 'Connected. Thread-ID: '+IntToStr(ThreadId));
      FActive := True;
      CharacterSet := 'utf8';
      CurCharset := CharacterSet;
      Log(lcInfo, 'Characterset: '+CurCharset);
      FIsUnicode := CurCharset = 'utf8';
      FConnectionStarted := GetTickCount div 1000;
      FServerStarted := FConnectionStarted - StrToIntDef(GetVar('SHOW STATUS LIKE ''Uptime''', 1), 1);
      FServerVersionUntouched := DecodeAPIString(mysql_get_server_info(FHandle));
      FServerOS := GetVar('SHOW VARIABLES LIKE ' + EscapeString('version_compile_os'), 1);
      FRealHostname := GetVar('SHOW VARIABLES LIKE ' + EscapeString('hostname'), 1);;
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
    end;
  end

  else if (not Value) and (FHandle <> nil) then begin
    mysql_close(FHandle);
    FActive := False;
    ClearCache(False);
    FConnectionStarted := 0;
    FHandle := nil;
    ClosePlink;
    Log(lcInfo, 'Connection to '+FParameters.Hostname+' closed at '+DateTimeToStr(Now));
  end;

end;


function TMySQLConnection.Ping: Boolean;
begin
  Log(lcDebug, 'Ping server ...');
  if FActive and ((FHandle=nil) or (mysql_ping(FHandle) <> 0)) then
    Active := False;
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
function TMySQLConnection.Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TMySQLLogCategory=lcSQL): PMYSQL_RES;
var
  querystatus: Integer;
  NativeSQL: AnsiString;
  TimerStart: Cardinal;
  NextResult: PMYSQL_RES;
begin
  if not Ping then
    Active := True;
  Log(LogCategory, SQL);
  FLastQuerySQL := SQL;
  if IsUnicode then
    NativeSQL := UTF8Encode(SQL)
  else
    NativeSQL := AnsiString(SQL);
  TimerStart := GetTickCount;
  SetLength(FLastResults, 0);
  FResultCount := 0;
  querystatus := mysql_real_query(FHandle, PAnsiChar(NativeSQL), Length(NativeSQL));
  FLastQueryDuration := GetTickCount - TimerStart;
  FLastQueryNetworkDuration := 0;
  if querystatus <> 0 then begin
    // Most errors will show up here, some others slightly later, after mysql_store_result()
    Log(lcError, GetLastError);
    raise EDatabaseError.Create(GetLastError);
  end else begin
    // We must call mysql_store_result() + mysql_free_result() to unblock the connection
    // See: http://dev.mysql.com/doc/refman/5.0/en/mysql-store-result.html
    FRowsAffected := mysql_affected_rows(FHandle);
    TimerStart := GetTickCount;
    Result := mysql_store_result(FHandle);
    FLastQueryNetworkDuration := GetTickCount - TimerStart;
    if (Result = nil) and (FRowsAffected = -1) then begin
      // Indicates a late error, e.g. triggered by mysql_store_result(), after selecting a stored
      // function with invalid SQL body. Also SHOW TABLE STATUS on older servers.
      Log(lcError, GetLastError);
      raise EDatabaseError.Create(GetLastError);
    end;
    if Result <> nil then begin
      FRowsFound := mysql_num_rows(Result);
      FRowsAffected := 0;
      Log(lcDebug, IntToStr(RowsFound)+' rows found.');

      if DoStoreResult then begin
        SetLength(FLastResults, 1);
        FLastResults[0] := Result;
      end else begin
        mysql_free_result(Result);
        Result := nil;
      end;

      // No support for real multi results yet, throw them away, so mysql_ping() does not crash on the *next* query.
      while mysql_next_result(FHandle) = 0 do begin
        NextResult := mysql_store_result(FHandle);
        if NextResult <> nil then begin
          if DoStoreResult then begin
            SetLength(FLastResults, Length(FLastResults)+1);
            FLastResults[Length(FLastResults)-1] := NextResult;
          end else
            mysql_free_result(NextResult);
        end;
      end;
      FResultCount := Length(FLastResults);

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


function TMySQLConnection.GetLastResults: TMySQLQueryList;
var
  r: TMySQLQuery;
  i: Integer;
begin
  Result := TMySQLQueryList.Create(False);
  for i:=Low(FLastResults) to High(FLastResults) do begin
    r := TMySQLQuery.Create(nil);
    r.Connection := Self;
    r.SQL := FLastQuerySQL;
    r.Execute(True, FLastResults[i]);
    Result.Add(r);
  end;
end;


{**
  Set "Database" property and select that db if connected
}
procedure TMySQLConnection.SetDatabase(Value: String);
begin
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
  Result := mysql_thread_id(FHandle);
end;


{**
  Return currently used character set
}
function TMySQLConnection.GetCharacterSet: String;
begin
  Result := DecodeAPIString(mysql_character_set_name(FHandle));
end;


{**
  Switch character set
}
procedure TMySQLConnection.SetCharacterSet(CharsetName: String);
begin
  mysql_set_character_set(FHandle, PAnsiChar(Utf8Encode(CharsetName)));
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
  Result := Format('SQL Error (%d): %s', [mysql_errno(FHandle), Msg]);
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


function TMySQLConnection.GetServerVersionStr: String;
begin
  Result := ConvertServerVersion(ServerVersionInt);
end;


function TMySQLConnection.GetAllDatabases: TStringList;
var
  i: Integer;
begin
  if not Assigned(FAllDatabases) then begin
    if FParameters.AllDatabases <> '' then begin
      FAllDatabases := TStringList.Create;
      FAllDatabases.Delimiter := ';';
      FAllDatabases.StrictDelimiter := True;
      FAllDatabases.DelimitedText := FParameters.AllDatabases;
      // Trim all and remove empty items
      for i:=FAllDatabases.Count-1 downto 0 do begin
        FAllDatabases[i] := Trim(FAllDatabases[i]);
        if FAllDatabases[i] = '' then
          FAllDatabases.Delete(i);
      end;
    end else
      FAllDatabases := GetCol('SHOW DATABASES');
  end;
  Result := FAllDatabases;
end;


function TMySQLConnection.RefreshAllDatabases: TStringList;
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


function TMySQLConnection.GetResults(SQL: String): TMySQLQuery;
begin
  Result := TMySQLQuery.Create(Self);
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
}
procedure TMySQLConnection.Log(Category: TMySQLLogCategory; Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(FLogPrefix+Msg, Category, Self);
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
function TMySQLConnection.EscapeString(Text: String; ProcessJokerChars: Boolean=false): String;
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
  if not ProcessJokerChars then begin
    // Add surrounding single quotes only for non-LIKE-values
    // because in all cases we're using ProcessLIKEChars we
    // need to add leading and/or trailing joker-chars by hand
    // without being escaped
    Result := Char(#39) + Result + Char(#39);
  end;
end;


{***
 Attempt to do string replacement faster than StringReplace
}
function TMySQLConnection.escChars(const Text: String; EscChar, Char1, Char2, Char3, Char4: Char): String;
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


function TMySQLConnection.UnescapeString(Text: String): String;
begin
  // Return text with MySQL special sequences turned back to normal characters
  Result := StringReplace(Text, '\r', #13, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
  Result := StringReplace(Result, '''''', '''', [rfReplaceAll]);
end;


{**
  Add backticks to identifier
  Todo: Support ANSI style
}
function QuoteIdent(Identifier: String; AlwaysQuote: Boolean=True; Glue: Char=#0): String;
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
      Result := StringReplace(Result, '`', '``', [rfReplaceAll]);
      Result := '`' + Result + '`';
    end;
  end;
end;


function DeQuoteIdent(Identifier: String; Glue: Char=#0): String;
begin
  Result := Identifier;
  if (Result[1] = '`') and (Result[Length(Identifier)] = '`') then
    Result := Copy(Result, 2, Length(Result)-2);
  if Glue <> #0 then
    Result := StringReplace(Result, '`'+Glue+'`', Glue, [rfReplaceAll]);
end;


function TMySQLConnection.GetCol(SQL: String; Column: Integer=0): TStringList;
var
  Results: TMySQLQuery;
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
function TMySQLConnection.GetVar(SQL: String; Column: Integer=0): String;
var
  Results: TMySQLQuery;
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
function TMySQLConnection.GetVar(SQL: String; Column: String): String;
var
  Results: TMySQLQuery;
begin
  Results := GetResults(SQL);
  if Results.RecordCount > 0 then
    Result := Results.Col(Column)
  else
    Result := '';
  FreeAndNil(Results);
end;


function TMySQLConnection.GetTableEngines: TStringList;
var
  ShowEngines, HaveEngines: TMySQLQuery;
  engineName, engineSupport: String;
  PossibleEngines: TStringList;
begin
  // After a disconnect Ping triggers the cached engines to be reset
  Log(lcDebug, 'Fetching list of table engines ...');
  Ping;
  if not Assigned(FTableEngines) then begin
    FTableEngines := TStringList.Create;
    try
      ShowEngines := GetResults('SHOW ENGINES');
      while not ShowEngines.Eof do begin
        engineName := ShowEngines.Col('Engine');
        engineSupport := LowerCase(ShowEngines.Col('Support'));
        // Add to dropdown if supported
        if (engineSupport = 'yes') or (engineSupport = 'default') then
          FTableEngines.Add(engineName);
        // Check if this is the default engine
        if engineSupport = 'default' then
          FTableEngineDefault := engineName;
        ShowEngines.Next;
      end;
    except
      // Ignore errors on old servers and try a fallback:
      // Manually fetch available engine types by analysing have_* options
      // This is for servers below 4.1 or when the SHOW ENGINES statement has
      // failed for some other reason
      HaveEngines := GetResults('SHOW VARIABLES LIKE ''have%''');
      // Add default engines which will not show in a have_* variable:
      FTableEngines.CommaText := 'MyISAM,MRG_MyISAM,HEAP';
      FTableEngineDefault := 'MyISAM';
      // Possible other engines:
      PossibleEngines := TStringList.Create;
      PossibleEngines.CommaText := 'ARCHIVE,BDB,BLACKHOLE,CSV,EXAMPLE,FEDERATED,INNODB,ISAM';
      while not HaveEngines.Eof do begin
        engineName := copy(HaveEngines.Col(0), 6, Length(HaveEngines.Col(0)));
        // Strip additional "_engine" suffix, fx from "have_blackhole_engine"
        if Pos('_', engineName) > 0 then
          engineName := copy(engineName, 0, Pos('_', engineName)-1);
        engineName := UpperCase(engineName);
        // Add engine to list if it's a) in HaveEngineList and b) activated
        if (PossibleEngines.IndexOf(engineName) > -1)
          and (LowerCase(HaveEngines.Col(1)) = 'yes') then
          FTableEngines.Add(engineName);
        HaveEngines.Next;
      end;
    end;
  end;
  Result := FTableEngines;
end;


function TMySQLConnection.GetCollationTable: TMySQLQuery;
begin
  Log(lcDebug, 'Fetching list of collations ...');
  Ping;
  if (not Assigned(FCollationTable)) and (ServerVersionInt >= 40100) then
    FCollationTable := GetResults('SHOW COLLATION');
  if Assigned(FCollationTable) then
    FCollationTable.First;
  Result := FCollationTable;
end;


function TMySQLConnection.GetCollationList: TStringList;
var
  c: TMySQLQuery;
begin
  c := CollationTable;
  Result := TStringList.Create;
  if Assigned(c) then while not c.Eof do begin
    Result.Add(c.Col('Collation'));
    c.Next;
  end;
end;


function TMySQLConnection.GetCharsetTable: TMySQLQuery;
begin
  Log(lcDebug, 'Fetching charset list ...');
  Ping;
  if (not Assigned(FCharsetTable)) and (ServerVersionInt >= 40100) then
    FCharsetTable := GetResults('SHOW CHARSET');
  Result := FCharsetTable;
end;


function TMySQLConnection.GetCharsetList: TStringList;
var
  c: TMySQLQuery;
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


function TMySQLConnection.GetInformationSchemaObjects: TStringList;
begin
  Log(lcDebug, 'Fetching objects in information_schema db ...');
  Ping;
  if not Assigned(FInformationSchemaObjects) then try
    FInformationSchemaObjects := GetCol('SHOW TABLES FROM '+QuoteIdent('information_schema'));
  except
    // Gracefully return an empty list on old servers
    FInformationSchemaObjects := TStringList.Create;
  end;
  Result := FInformationSchemaObjects;
end;


function TMySQLConnection.GetConnectionUptime: Integer;
begin
  // Return seconds since last connect
  if not FActive then
    Result := 0
  else
    Result := Integer(GetTickCount div 1000) - FConnectionStarted;
end;


function TMySQLConnection.GetServerUptime: Integer;
begin
  // Return server uptime in seconds
  Result := Integer(GetTickCount div 1000) - FServerStarted;
end;


function TMySQLConnection.GetCurrentUserHostCombination: String;
begin
  // Return current user@host combination, used by various object editors for DEFINER clauses
  Log(lcDebug, 'Fetching user@host ...');
  Ping;
  if FCurrentUserHostCombination = '' then
    FCurrentUserHostCombination := GetVar('SELECT CURRENT_USER()');
  Result := FCurrentUserHostCombination;
end;


procedure TMySQLConnection.ClearCache(IncludeDBObjects: Boolean);
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
end;


procedure TMySQLConnection.ClearDbObjects(db: String);
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


procedure TMySQLConnection.ClearAllDbObjects;
var
  i: Integer;
begin
  for i:=FDatabases.Count-1 downto 0 do
    ClearDbObjects(FDatabases[i].Database);
end;


function TMySQLConnection.DbObjectsCached(db: String): Boolean;
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


function TMySQLConnection.ParseDateTime(Str: String): TDateTime;
var
  rx: TRegExpr;
begin
  // Parse a MySQL date / time string value into a TDateTime
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
  Results: TMySQLQuery;
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
    Results := nil;
    rx := TRegExpr.Create;
    rx.ModifierI := True;

    // Tables and views
    try
      Results := GetResults('SHOW TABLE STATUS FROM '+QuoteIdent(db));
    except
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
    // Sort list like it get sorted in MainForm.vstCompareNodes
    Result.Sort;
    // Add list of objects in this database to cached list of all databases
    FDatabases.Add(Result);
    SetObjectNamesInSelectedDB;
  end;

end;


procedure TMySQLConnection.SetObjectNamesInSelectedDB;
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


function TMySQLConnection.GetKeyColumns(Columns: TTableColumnList; Keys: TTableKeyList): TStringList;
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


function TMySQLConnection.DecodeAPIString(a: AnsiString): String;
begin
  if IsUnicode then
    Result := Utf8ToString(a)
  else
    Result := String(a);
end;


function TMySQLConnection.ConnectionInfo: TStringList;
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
  Ping;
  Result.Values['Connected'] := EvalBool(FActive);
  if FActive then begin
    Result.Values['Real Hostname'] := FRealHostname;
    Result.Values['Server OS'] := ServerOS;
    Result.Values['Server version'] := FServerVersionUntouched;
    Result.Values['Client version (libmysql)'] := DecodeApiString(mysql_get_client_info);
    Result.Values['Connection port'] := IntToStr(Parameters.Port);
    Result.Values['Compressed protocol'] := EvalBool(opCompress in Parameters.Options);
    Result.Values['Unicode enabled'] := EvalBool(IsUnicode);
    Infos := DecodeApiString(mysql_stat(FHandle));
    rx := TRegExpr.Create;
    rx.ModifierG := False;
    rx.Expression := '(\S.*)\:\s+(\S*)(\s+|$)';
    if rx.Exec(Infos) then while True do begin
      Val := rx.Match[2];
      if LowerCase(rx.Match[1]) = 'uptime' then
        Val := FormatTimeNumber(StrToIntDef(Val, 0))
      else
        Val := FormatNumber(Val);
      Result.Values[rx.Match[1]] := Val;
      if not rx.ExecNext then
        break;
    end;
    rx.Free;
  end;
end;


procedure TMySQLConnection.ParseTableStructure(CreateTable: String; Columns: TTableColumnList; Keys: TTableKeyList; ForeignKeys: TForeignKeyList);
var
  ColSpec: String;
  rx, rxCol: TRegExpr;
  i: Integer;
  InLiteral: Boolean;
  Col: TTableColumn;
  Key: TTableKey;
  ForeignKey: TForeignKey;
  Collations: TMySQLQuery;
begin
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

    Col := TTableColumn.Create;
    Columns.Add(Col);
    Col.Name := rx.Match[1];
    Col.OldName := Col.Name;
    Col.Status := esUntouched;
    Col.LengthCustomized := True;

    // Datatype
    Col.DataType := GetDatatypeByName(UpperCase(rx.Match[2]));

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

    // Collation - probably not present when charset present
    rxCol.Expression := '^COLLATE (\w+)\b\s*';
    if rxCol.Exec(ColSpec) then begin
      Col.Collation := rxCol.Match[1];
      Delete(ColSpec, 1, rxCol.MatchLen[0]);
    end;
    if Col.Collation = '' then begin
      if not Assigned(Collations) then
        Collations := CollationTable;
      if Assigned(Collations) then while not Collations.Eof do begin
        if (Collations.Col('Charset') = Col.Charset) and (Collations.Col('Default') = 'Yes') then begin
          Col.Collation := Collations.Col('Collation');
          break;
        end;
        Collations.Next;
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
    Key := TTableKey.Create;
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
    ForeignKey := TForeignKey.Create;
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


procedure TMySQLConnection.ParseViewStructure(CreateCode, ViewName: String; Columns: TTableColumnList; var Algorithm, Definer, CheckOption, SelectCode: String);
var
  rx: TRegExpr;
  Col: TTableColumn;
  Results: TMySQLQuery;
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
      '(ALGORITHM\s*=\s*(\w+)\s+)?'+
      '(DEFINER\s*=\s*(\S+)\s+)?'+
      '(SQL\s+SECURITY\s+\w+\s+)?'+
      'VIEW\s+(`?(\w[\w\s\-]*)`?\.)?(`?(\w[\w\s\-]*)`?)?\s+'+
      '(\([^\)]\)\s+)?'+
      'AS\s+(.+)(\s+WITH\s+(\w+\s+)?CHECK\s+OPTION\s*)?$';
    if rx.Exec(CreateCode) then begin
      Algorithm := rx.Match[3];
      Definer := DeQuoteIdent(rx.Match[5], '@');
      // When exporting a view we need the db name for the below SHOW COLUMNS query,
      // if the connection is on a different db currently
      DbName := rx.Match[8];
      ViewName := rx.Match[10];
      CheckOption := Trim(rx.Match[14]);
      SelectCode := rx.Match[12];
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
      Col := TTableColumn.Create;
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
      if Col.DataType.Category <> dtcTemporal then
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


procedure TMySQLConnection.ParseRoutineStructure(CreateCode: String; Parameters: TRoutineParamList;
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



{ TMySQLQuery }

constructor TMySQLQuery.Create(AOwner: TComponent);
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


destructor TMySQLQuery.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FColumnNames);
  FreeAndNil(FColumnOrgNames);
  FreeAndNil(FColumns);
  FreeAndNil(FKeys);
  FreeAndNil(FUpdateData);
  SetLength(FColumnFlags, 0);
  SetLength(FColumnLengths, 0);
  SetLength(FColumnTypes, 0);
  if HasResult then for i:=Low(FResultList) to High(FResultList) do
    mysql_free_result(FResultList[i]);
  SetLength(FResultList, 0);
  FSQL := '';
  FRecordCount := 0;
  inherited Destroy;
end;


procedure TMySQLQuery.SetSQL(Value: String);
begin
  FSQL := Value;
end;


procedure TMySQLQuery.Execute(AddResult: Boolean=False; Res: PMYSQL_RES=nil);
var
  i, j, NumFields: Integer;
  NumResults: Int64;
  Field: PMYSQL_FIELD;
  IsBinary: Boolean;
  FLastResult: PMYSQL_RES;
begin
  if Res <> nil then
    FLastResult := Res
  else
    FLastResult := Connection.Query(FSQL, FStoreResult);
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
  if FLastResult <> nil then begin
    Connection.Log(lcDebug, 'Result #'+IntToStr(NumResults)+' fetched.');
    SetLength(FResultList, NumResults);
    FResultList[NumResults-1] := FLastResult;
    FRecordCount := FRecordCount + FLastResult.row_count;
  end;
  if not AddResult then begin
    if HasResult then begin
      // FCurrentResults is normally done in SetRecNo, but never if result has no rows
      FCurrentResults := FLastResult;
      NumFields := mysql_num_fields(FLastResult);
      SetLength(FColumnTypes, NumFields);
      SetLength(FColumnLengths, NumFields);
      SetLength(FColumnFlags, NumFields);
      FColumnNames.Clear;
      FColumnOrgNames.Clear;
      for i:=0 to NumFields-1 do begin
        Field := mysql_fetch_field_direct(FLastResult, i);
        FColumnNames.Add(Connection.DecodeAPIString(Field.name));
        if Connection.ServerVersionInt >= 40100 then
          FColumnOrgNames.Add(Connection.DecodeAPIString(Field.org_name))
        else
          FColumnOrgNames.Add(Connection.DecodeAPIString(Field.name));
        FColumnFlags[i] := Field.flags;
        FColumnTypes[i] := Datatypes[Low(Datatypes)];
        if (Field.flags and ENUM_FLAG) = ENUM_FLAG then
          FColumnTypes[i] := Datatypes[Integer(dtEnum)]
        else if (Field.flags and SET_FLAG) = SET_FLAG then
          FColumnTypes[i] := Datatypes[Integer(dtSet)]
        else for j:=Low(Datatypes) to High(Datatypes) do begin
          if Field._type = Datatypes[j].NativeType then begin
            // Text and Blob types share the same constants (see FIELD_TYPEs in mysql_api)
            // Some function results return binary collation up to the latest versions. Work around
            // that by checking if this field is a real table field
            // See http://bugs.mysql.com/bug.php?id=10201
            if Connection.IsUnicode then
              IsBinary := (Field.charsetnr = COLLATION_BINARY) and (Field.org_table <> '')
            else
              IsBinary := (Field.flags and BINARY_FLAG) = BINARY_FLAG;
            if IsBinary and (Datatypes[j].Category = dtcText) then
              continue;
            FColumnTypes[i] := Datatypes[j];
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


procedure TMySQLQuery.SetColumnOrgNames(Value: TStringList);
begin
  // Retrieve original column names from caller
  FColumnOrgNames.Text := Value.Text;
end;


procedure TMySQLQuery.First;
begin
  RecNo := 0;
end;


procedure TMySQLQuery.Next;
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


function TMySQLQuery.ColumnCount: Integer;
begin
  Result := ColumnNames.Count;
end;


function TMySQLQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
var
  AnsiStr: AnsiString;
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
    end;
  end else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt('Column #%d not available. Query returned %d columns and %d rows.', [Column, ColumnCount, RecordCount]);
end;


function TMySQLQuery.Col(ColumnName: String; IgnoreErrors: Boolean=False): String;
var
  idx: Integer;
begin
  idx := ColumnNames.IndexOf(ColumnName);
  if idx > -1 then
    Result := Col(idx)
  else if not IgnoreErrors then
    Raise EDatabaseError.CreateFmt('Column "%s" not available.', [ColumnName]);
end;


function TMySQLQuery.BinColAsHex(Column: Integer; IgnoreErrors: Boolean=False): String;
var
  BinLen: Integer;
  Ansi: AnsiString;
begin
  // Return a binary column value as hex AnsiString
  Result := Col(Column, IgnoreErrors);
  Ansi := AnsiString(Result);
  BinLen := FColumnLengths[Column];
  SetLength(Result, BinLen*2);
  BinToHex(PAnsiChar(Ansi), PChar(Result), BinLen);
end;


function TMySQLQuery.DataType(Column: Integer): TDataType;
var
  Col: TTableColumn;
begin
  Col := ColAttributes(Column);
  if Assigned(Col) then
    Result := Col.DataType
  else
    Result := FColumnTypes[Column];
end;


function TMySQLQuery.MaxLength(Column: Integer): Int64;
var
  ColAttr: TTableColumn;
begin
  // Return maximum posible length of values in given columns
  // Note: PMYSQL_FIELD.max_length holds the maximum existing value in that column, which is useless here
  Result := MaxInt;
  ColAttr := ColAttributes(Column);
  if Assigned(ColAttr) then begin
    case ColAttr.DataType.Index of
      dtChar, dtVarchar, dtBinary, dtVarBinary: Result := MakeInt(ColAttr.LengthSet);
      dtTinyText, dtTinyBlob: Result := 255;
      dtText, dtBlob: Result := 65535;
      dtMediumText, dtMediumBlob: Result := 16777215;
      dtLongText, dtLongBlob: Result := 4294967295;
    end;
  end;
end;


function TMySQLQuery.ValueList(Column: Integer): TStringList;
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


function TMySQLQuery.ColAttributes(Column: Integer): TTableColumn;
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


function TMySQLQuery.ColExists(Column: String): Boolean;
begin
  Result := (ColumnNames <> nil) and (ColumnNames.IndexOf(Column) > -1);
end;


function TMySQLQuery.ColIsPrimaryKeyPart(Column: Integer): Boolean;
begin
  Result := (FColumnFlags[Column] and PRI_KEY_FLAG) = PRI_KEY_FLAG;
end;


function TMySQLQuery.ColIsUniqueKeyPart(Column: Integer): Boolean;
begin
  Result := (FColumnFlags[Column] and UNIQUE_KEY_FLAG) = UNIQUE_KEY_FLAG;
end;


function TMySQLQuery.ColIsKeyPart(Column: Integer): Boolean;
begin
  Result := (FColumnFlags[Column] and MULTIPLE_KEY_FLAG) = MULTIPLE_KEY_FLAG;
end;


function TMySQLQuery.IsNull(Column: Integer): Boolean;
begin
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then
    Result := FCurrentUpdateRow[Column].NewIsNull
  else
    Result := FCurrentRow[Column] = nil;
end;


function TMySQLQuery.IsNull(Column: String): Boolean;
begin
  Result := IsNull(FColumnNames.IndexOf(Column));
end;


function TMySQLQuery.HasResult: Boolean;
begin
  Result := Length(FResultList) > 0;
end;


procedure TMySQLQuery.PrepareEditing;
var
  Res: TMySQLQuery;
  CreateCode, Dummy: String;
begin
  // Try to fetch column names and keys
  if FEditingPrepared then
    Exit;
  Res := Connection.GetResults('SHOW CREATE TABLE ' + QuotedDbAndTableName);
  CreateCode := Res.Col(1);
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
  // This is probably a VIEW, so column names need to be fetched differently
  if UpperCase(Res.ColumnNames[0]) = 'TABLE' then
    Connection.ParseTableStructure(CreateCode, FColumns, FKeys, FForeignKeys)
  else
    Connection.ParseViewStructure(CreateCode, TableName, FColumns, Dummy, Dummy, Dummy, Dummy);
  FreeAndNil(Res);
  FreeAndNil(FUpdateData);
  FUpdateData := TUpdateData.Create(True);
  FEditingPrepared := True;
end;


function TMySQLQuery.DeleteRow: Boolean;
var
  sql: String;
  IsVirtual: Boolean;
begin
  // Delete current row from result
  PrepareEditing;
  IsVirtual := Assigned(FCurrentUpdateRow) and FCurrentUpdateRow.Inserted;
  if not IsVirtual then begin
    sql := 'DELETE FROM ' + QuotedDbAndTableName + ' WHERE ' + GetWhereClause + ' LIMIT 1';
    Connection.Query(sql);
  end;
  if Assigned(FCurrentUpdateRow) then begin
    FUpdateData.Remove(FCurrentUpdateRow);
    FCurrentUpdateRow := nil;
  end;
  Result := True;
end;


function TMySQLQuery.InsertRow: Cardinal;
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


procedure TMySQLQuery.SetCol(Column: Integer; NewText: String; Null: Boolean);
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


procedure TMySQLQuery.CreateUpdateRow;
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


function TMySQLQuery.EnsureFullRow: Boolean;
var
  i: Integer;
  sql: String;
  Data: TMySQLQuery;
begin
  // Load full column values
  Result := True;
  if not HasFullData then try
    PrepareEditing;
    for i:=0 to FColumnOrgNames.Count-1 do begin
      if sql <> '' then
        sql := sql + ', ';
      sql := sql + QuoteIdent(FColumnOrgNames[i]);
    end;
    Data := Connection.GetResults('SELECT '+sql+' FROM '+QuotedDbAndTableName+' WHERE '+GetWhereClause+' LIMIT 1');
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


function TMySQLQuery.HasFullData: Boolean;
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


function TMySQLQuery.SaveModifications: Boolean;
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
        dtcInteger, dtcReal: Val := Cell.NewText;
        else Val := Connection.EscapeString(Cell.NewText);
      end;
      sqlUpdate := sqlUpdate + QuoteIdent(FColumnOrgNames[i]) + '=' + Val;
      sqlInsertColumns := sqlInsertColumns + QuoteIdent(FColumnOrgNames[i]);
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
      end else
        Connection.Query('UPDATE '+QuotedDbAndTableName+' SET '+sqlUpdate+' WHERE '+GetWhereClause+' LIMIT 1');
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
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;

  end;
end;


procedure TMySQLQuery.DiscardModifications;
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


function TMySQLQuery.Modified(Column: Integer): Boolean;
begin
  Result := False;
  if FEditingPrepared and Assigned(FCurrentUpdateRow) then try
    Result := FCurrentUpdateRow[Column].Modified;
  except
    connection.Log(lcdebug, inttostr(column));
    raise;
  end;
end;


function TMySQLQuery.Modified: Boolean;
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


function TMySQLQuery.Inserted: Boolean;
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


function TMySQLQuery.TableName: String;
var
  Field: PMYSQL_FIELD;
  i: Integer;
  tbl, db: AnsiString;
  Objects: TDBObjectList;
  Obj: TDBObject;
begin
  for i:=0 to ColumnCount-1 do begin
    Field := mysql_fetch_field_direct(FCurrentResults, i);

    if Field.table <> Field.org_table then begin
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
      if tbl <> '' then
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


function TMySQLQuery.QuotedDbAndTableName: String;
var
  db: String;
begin
  // Return `db`.`table` if necessairy, otherwise `table`
  db := DatabaseName;
  if Connection.Database <> db then
    Result := QuoteIdent(db)+'.';
  Result := Result + QuoteIdent(TableName);
end;


function TMySQLQuery.GetKeyColumns: TStringList;
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


procedure TMySQLQuery.CheckEditable;
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


function TMySQLQuery.GetWhereClause: String;
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
    Result := Result + ' ' + QuoteIdent(FColumnOrgNames[j]);
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
        dtcInteger, dtcReal: Result := Result + '=' + ColVal;
        else Result := Result + '=' + Connection.EscapeString(ColVal);
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

constructor TDBObject.Create(OwnerConnection: TMySQLConnection);
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
    lntNone: Result := ICONINDEX_SERVER;

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
  Column: Integer;
begin
  Column := -1;
  case NodeType of
    lntTable, lntView: Column := 1;
    lntFunction, lntProcedure, lntTrigger: Column := 2;
    lntEvent: Column := 3;
    else Exception.Create('Unhandled list node type in '+ClassName+'.GetCreateCode');
  end;
  if not FCreateCodeFetched then try
    FCreateCode := FConnection.GetVar('SHOW CREATE '+UpperCase(ObjType)+' '+QuoteIdent(Database)+'.'+QuoteIdent(Name), Column)
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


{ *** TTableColumn }

constructor TTableColumn.Create;
begin
  inherited Create;
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
begin
  Result := QuoteIdent(Name) + ' ' +DataType.Name;
  if LengthSet <> '' then
    Result := Result + '(' + LengthSet + ')';
  if (DataType.Category in [dtcInteger, dtcReal]) and Unsigned then
    Result := Result + ' UNSIGNED';
  if (DataType.Category in [dtcInteger, dtcReal]) and ZeroFill then
    Result := Result + ' ZEROFILL';
  if not AllowNull then
    Result := Result + ' NOT';
  Result := Result + ' NULL';
  if DefaultType <> cdtNothing then begin
    Result := Result + ' ' + GetColumnDefaultClause(DefaultType, DefaultText);
    Result := TrimRight(Result); // Remove whitespace for columns without default value
  end;
  if Comment <> '' then
    Result := Result + ' COMMENT '+esc(Comment);
  if Collation <> '' then
    Result := Result + ' COLLATE '+esc(Collation);
end;



{ *** TTableKey }

constructor TTableKey.Create;
begin
  inherited Create;
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
    Result := Result + 'INDEX ' + QuoteIdent(Name) + ' ';
  end;
  Result := Result + '(';
  for i:=0 to Columns.Count-1 do begin
    Result := Result + QuoteIdent(Columns[i]);
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

constructor TForeignKey.Create;
begin
  inherited Create;
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
    Result := 'CONSTRAINT '+QuoteIdent(KeyName)+' ';
  Result := Result + 'FOREIGN KEY (';
  for i:=0 to Columns.Count-1 do
    Result := Result + QuoteIdent(Columns[i]) + ', ';
  if Columns.Count > 0 then Delete(Result, Length(Result)-1, 2);
  Result := Result + ') REFERENCES ' + QuoteIdent(ReferenceTable, True, '.') + ' (';
  for i:=0 to ForeignColumns.Count-1 do
    Result := Result + QuoteIdent(ForeignColumns[i]) + ', ';
  if ForeignColumns.Count > 0 then Delete(Result, Length(Result)-1, 2);
  Result := Result + ')';
  if OnUpdate <> '' then
    Result := Result + ' ON UPDATE ' + OnUpdate;
  if OnDelete <> '' then
    Result := Result + ' ON DELETE ' + OnDelete;
end;



end.
