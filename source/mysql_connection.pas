unit mysql_connection;

interface

uses
  Classes, SysUtils, windows, mysql_api, mysql_structures, SynRegExpr, Contnrs, Generics.Collections, Generics.Defaults,
  DateUtils, Types, ShellApi, Math, Dialogs;

type
  { TDBObjectList and friends }

  TListNodeType = (lntNone, lntDb, lntTable, lntView, lntFunction, lntProcedure, lntTrigger, lntEvent, lntColumn);
  TListNodeTypes = Set of TListNodeType;
  TDBObject = class(TPersistent)
    private
      function GetObjType: String;
      function GetImageIndex: Integer;
    public
      Name, Database, Engine, Comment, RowFormat, CreateOptions, Collation: String;
      Created, Updated, LastChecked: TDateTime;
      Rows, Size, Version, AvgRowLen, MaxDataLen, IndexLen, DataLen, DataFree, AutoInc, CheckSum: Int64;
      NodeType: TListNodeType;
      constructor Create;
      procedure Assign(Source: TPersistent); override;
      property ObjType: String read GetObjType;
      property ImageIndex: Integer read GetImageIndex;
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
      Unsigned, AllowNull: Boolean;
      DefaultType: TColumnDefaultType;
      DefaultText: String;
      Comment, Collation: String;
      FStatus: TEditingStatus;
      constructor Create;
      destructor Destroy; override;
      property Status: TEditingStatus read FStatus write SetStatus;
  end;
  PTableColumn = ^TTableColumn;
  TTableColumnList = TObjectList<TTableColumn>;

  TTableKey = class(TObject)
    public
      Name, OldName: String;
      IndexType, Algorithm: String;
      Columns, SubParts: TStringList;
      Modified, Added: Boolean;
      constructor Create;
      destructor Destroy; override;
      procedure Modification(Sender: TObject);
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
      FPort, FSSHPort, FSSHLocalPort: Integer;
      FOptions: TMySQLClientOptions;
    public
      constructor Create;
    published
      property NetType: TNetType read FNetType write FNetType;
      property Hostname: String read FHostname write FHostname;
      property Port: Integer read FPort write FPort;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property AllDatabases: String read FAllDatabases write FAllDatabases;
      property StartupScriptFilename: String read FStartupScriptFilename write FStartupScriptFilename;
      property Options: TMySQLClientOptions read FOptions write FOptions;
      property SSHHost: String read FSSHHost write FSSHHost;
      property SSHPort: Integer read FSSHPort write FSSHPort;
      property SSHUser: String read FSSHUser write FSSHUser;
      property SSHPassword: String read FSSHPassword write FSSHPassword;
      property SSHPrivateKey: String read FSSHPrivateKey write FSSHPrivateKey;
      property SSHLocalPort: Integer read FSSHLocalPort write FSSHLocalPort;
      property SSHPlinkExe: String read FSSHPlinkExe write FSSHPlinkExe;
      property SSLPrivateKey: String read FSSLPrivateKey write FSSLPrivateKey;
      property SSLCertificate: String read FSSLCertificate write FSSLCertificate;
      property SSLCACertificate: String read FSSLCACertificate write FSSLCACertificate;
  end;


  { TMySQLConnection }

  TMySQLLogCategory = (lcInfo, lcSQL, lcUserFiredSQL, lcError, lcDebug);
  TMySQLLogEvent = procedure(Msg: String; Category: TMySQLLogCategory=lcInfo) of object;
  TMySQLDatabaseEvent = procedure(Database: String) of object;

  TMySQLQuery = class;
  TMySQLConnection = class(TComponent)
    private
      FHandle: PMYSQL;
      FActive: Boolean;
      FConnectionStarted: Integer;
      FServerStarted: Integer;
      FParameters: TConnectionParameters;
      FDatabase: String;
      FLogPrefix: String;
      FOnLog: TMySQLLogEvent;
      FOnDatabaseChanged: TMySQLDatabaseEvent;
      FOnDBObjectsCleared: TMySQLDatabaseEvent;
      FRowsFound: Int64;
      FRowsAffected: Int64;
      FServerVersionUntouched: String;
      FLastQueryDuration, FLastQueryNetworkDuration: Cardinal;
      FIsUnicode: Boolean;
      FTableEngines: TStringList;
      FTableEngineDefault: String;
      FCollationTable: TMySQLQuery;
      FCharsetTable: TMySQLQuery;
      FInformationSchemaObjects: TStringList;
      FDatabases: TDatabaseList;
      FObjectNamesInSelectedDB: TStrings;
      FPlinkProcInfo: TProcessInformation;
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
      procedure Log(Category: TMySQLLogCategory; Msg: String);
      procedure ClearCache;
      procedure SetObjectNamesInSelectedDB;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Query(SQL: String; DoStoreResult: Boolean=False; LogCategory: TMySQLLogCategory=lcSQL): PMYSQL_RES;
      function EscapeString(Text: String; ProcessJokerChars: Boolean=False): String;
      function escChars(const Text: String; EscChar, Char1, Char2, Char3, Char4: Char): String;
      function QuoteIdent(Identifier: String): String;
      function DeQuoteIdent(Identifier: String): String;
      function ConvertServerVersion(Version: Integer): String;
      function GetResults(SQL: String): TMySQLQuery;
      function GetCol(SQL: String; Column: Integer=0): TStringList;
      function GetVar(SQL: String; Column: Integer=0): String; overload;
      function GetVar(SQL: String; Column: String): String; overload;
      function Ping: Boolean;
      function GetDBObjects(db: String; Refresh: Boolean=False): TDBObjectList;
      function DbObjectsCached(db: String): Boolean;
      function ParseDateTime(Str: String): TDateTime;
      function GetKeyColumns(Columns: TTableColumnList; Keys: TTableKeyList): TStringList;
      procedure ClearDbObjects(db: String);
      procedure ClearAllDbObjects;
      property Parameters: TConnectionParameters read FParameters write FParameters;
      property ThreadId: Cardinal read GetThreadId;
      property ConnectionUptime: Integer read GetConnectionUptime;
      property ServerUptime: Integer read GetServerUptime;
      property CharacterSet: String read GetCharacterSet write SetCharacterSet;
      property LastError: String read GetLastError;
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
    published
      property Active: Boolean read FActive write SetActive default False;
      property Database: String read FDatabase write SetDatabase;
      property LogPrefix: String read FLogPrefix write FLogPrefix;
      // Events
      property OnLog: TMySQLLogEvent read FOnLog write FOnLog;
      property OnDatabaseChanged: TMySQLDatabaseEvent read FOnDatabaseChanged write FOnDatabaseChanged;
      property OnDBObjectsCleared: TMySQLDatabaseEvent read FOnDBObjectsCleared write FOnDBObjectsCleared;
  end;


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
      FLogCategory: TMySQLLogCategory;
      FStoreResult: Boolean;
      FColumns: TTableColumnList;
      FKeys: TTableKeyList;
      FForeignKeys: TForeignKeyList;
      FEditingPrepared: Boolean;
      FUpdateData: TUpdateData;
      procedure SetSQL(Value: String);
      procedure SetRecNo(Value: Int64);
      procedure SetColumnOrgNames(Value: TStringList);
      procedure PrepareEditing;
      procedure CreateUpdateRow;
      function DatabaseName: String;
      function TableName: String;
      function QuotedDbAndTableName: String;
      function GetKeyColumns: TStringList;
      function GetWhereClause: String;
      function ColAttributes(Column: Integer): TTableColumn;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute(AddResult: Boolean=False);
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
      procedure DiscardModifications;
      property RecNo: Int64 read FRecNo write SetRecNo;
      property Eof: Boolean read FEof;
      property RecordCount: Int64 read FRecordCount;
      property ColumnNames: TStringList read FColumnNames;
      property LogCategory: TMySQLLogCategory read FLogCategory write FLogCategory;
      property StoreResult: Boolean read FStoreResult write FStoreResult;
      property ColumnOrgNames: TStringList read FColumnOrgNames write SetColumnOrgNames;
    published
      property SQL: String read FSQL write SetSQL;
      property Connection: TMySQLConnection read FConnection write FConnection;
  end;


implementation

uses helpers;



{ TConnectionParameters }

constructor TConnectionParameters.Create;
begin
  FNetType := ntTCPIP;
  FHostname := DEFAULT_HOST;
  FUsername := DEFAULT_USER;
  FPassword := '';
  FPort := DEFAULT_PORT;
  FSSHPort := DEFAULT_SSHPORT;
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
  FParameters := TConnectionParameters.Create;
  FRowsFound := 0;
  FRowsAffected := 0;
  FConnectionStarted := 0;
  FLastQueryDuration := 0;
  FLastQueryNetworkDuration := 0;
  FLogPrefix := '';
  FIsUnicode := False;
  FDatabases := TDatabaseList.Create(True);
end;


destructor TMySQLConnection.Destroy;
begin
  if Active then Active := False;
  FOnDBObjectsCleared := nil;
  ClearCache;
  inherited Destroy;
end;


{**
  (Dis-)Connect to/from server
}
procedure TMySQLConnection.SetActive( Value: Boolean );
var
  Connected: PMYSQL;
  ClientFlags, FinalPort: Integer;
  Error, tmpdb, FinalHost, FinalSocket, PlinkCmd: String;
  SSLResult: Byte;
  UsingPass, Protocol, CurCharset: String;
  StartupInfo: TStartupInfo;
  ExitCode: LongWord;
begin
  if Value and (FHandle = nil) then begin
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
        PlinkCmd := PlinkCmd + ' -L ' + IntToStr(FParameters.SSHLocalPort) + ':' + FParameters.Hostname + ':' + IntToStr(FParameters.Port);
        Log(lcInfo, 'Attempt to create plink.exe process ...');
        // Create plink.exe process
        FillChar(FPlinkProcInfo, SizeOf(TProcessInformation), 0);
        FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
        StartupInfo.cb := SizeOf(TStartupInfo);
        if CreateProcess(nil, PChar(PlinkCmd), nil, nil, false,
          CREATE_DEFAULT_ERROR_MODE + NORMAL_PRIORITY_CLASS + CREATE_NO_WINDOW,
          nil, nil, StartupInfo, FPlinkProcInfo) then begin
          WaitForSingleObject(FPlinkProcInfo.hProcess, 1000);
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
      FServerVersionUntouched := Utf8ToString(mysql_get_server_info(FHandle));
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
    FConnectionStarted := 0;
    FHandle := nil;
    ClosePlink;
    Log(lcInfo, 'Connection to '+FParameters.Hostname+' closed at '+DateTimeToStr(Now));
  end;

end;


function TMySQLConnection.Ping: Boolean;
begin
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
  querystatus, i: Integer;
  NativeSQL: AnsiString;
  TimerStart: Cardinal;
  NextResult: PMYSQL_RES;
begin
  if not Ping then
    Active := True;
  Log(LogCategory, SQL);
  if IsUnicode then
    NativeSQL := UTF8Encode(SQL)
  else
    NativeSQL := AnsiString(SQL);
  TimerStart := GetTickCount;
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
      if not DoStoreResult then begin
        mysql_free_result(Result);
        Result := nil;
      end;

      // No support for real multi results yet, throw them away, so mysql_ping() does not crash on the *next* query.
      i := 1;
      while mysql_next_result(FHandle) = 0 do begin
        Inc(i);
        Log(lcDebug, 'Storing and freeing result #'+IntToStr(i)+' from multiple result set ...');
        NextResult := mysql_store_result(FHandle);
        if NextResult <> nil then
          mysql_free_result(NextResult);
      end;

    end else begin
      // Query did not return a result
      FRowsFound := 0;
      Log(lcDebug, IntToStr(RowsAffected)+' rows affected.');
      if UpperCase(Copy(SQL, 1, 3)) = 'USE' then begin
        FDatabase := Trim(Copy(SQL, 4, Length(SQL)-3));
        FDatabase := DeQuoteIdent(FDatabase);
        Log(lcDebug, 'Database "'+FDatabase+'" selected');
        if Assigned(FOnDatabaseChanged) then
          FOnDatabaseChanged(Database);
      end;
    end;
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
        FOnDatabaseChanged(Value);
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
  Result := Utf8ToString(mysql_character_set_name(FHandle));
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
  Msg := Utf8ToString(mysql_error(FHandle));
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
begin
  if FParameters.AllDatabases <> '' then begin
    Result := TStringList.Create;
    Result.Delimiter := ';';
    Result.StrictDelimiter := True;
    Result.DelimitedText := FParameters.AllDatabases;
  end else
    Result := GetCol('SHOW DATABASES');
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
    FOnLog(FLogPrefix+Msg, Category);
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


{**
  Add backticks to identifier
  Todo: Support ANSI style
}
function TMySQLConnection.QuoteIdent(Identifier: String): String;
begin
  Result := StringReplace(Identifier, '`', '``', [rfReplaceAll]);
  Result := '`' + Result + '`';
end;


function TMySQLConnection.DeQuoteIdent(Identifier: String): String;
begin
  Result := Identifier;
  if (Result[1] = '`') and (Result[Length(Identifier)] = '`') then
    Result := Copy(Result, 2, Length(Result)-2);
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
  if not Assigned(FTableEngines) then begin
    FTableEngines := TStringList.Create;
    try
      ShowEngines := GetResults('SHOW ENGINES');
      while not ShowEngines.Eof do begin
        engineName := ShowEngines.Col('Engine');
        engineSupport := LowerCase(ShowEngines.Col('Support'));
        // Add to dropdown if supported
        if engineSupport <> 'no' then
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


procedure TMySQLConnection.ClearCache;
begin
  // Free cached lists and results. Called when the connection was closed and/or destroyed
  FreeAndNil(FCollationTable);
  FreeAndNil(FCharsetTable);
  FreeAndNil(FTableEngines);
  FreeAndNil(FInformationSchemaObjects);
  ClearAllDbObjects;
  FTableEngineDefault := '';
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
        FOnDBObjectsCleared(db);
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
        obj := TDBObject.Create;
        Result.Add(obj);
        obj.Name := Results.Col('Name');
        obj.Database := db;
        obj.Rows := StrToInt64Def(Results.Col('Rows'), -1);
        if Results.IsNull('Data_length') or Results.IsNull('Index_length') then
          Obj.Size := -1
        else begin
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
        Obj.Version := StrToInt64Def(Results.Col('Version', True), -1);
        Obj.AutoInc := StrToInt64Def(Results.Col('Auto_increment'), -1);
        Obj.RowFormat := Results.Col('Row_format');
        Obj.AvgRowLen := StrToInt64Def(Results.Col('Avg_row_length'), -1);
        Obj.MaxDataLen := StrToInt64Def(Results.Col('Max_data_length'), -1);
        Obj.IndexLen := StrToInt64Def(Results.Col('Index_length'), -1);
        Obj.DataLen := StrToInt64Def(Results.Col('Data_length'), -1);
        Obj.DataFree := StrToInt64Def(Results.Col('Data_free'), -1);
        Obj.LastChecked := ParseDateTime(Results.Col('Check_time'));
        Obj.Collation := Results.Col('Collation', True);
        Obj.CheckSum := StrToInt64Def(Results.Col('Checksum', True), -1);
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
        obj := TDBObject.Create;
        Result.Add(obj);
        obj.Name := Results.Col('Name');
        obj.Database := db;
        obj.Rows := -1;
        Obj.Size := -1;
        Obj.NodeType := lntFunction;
        Obj.Created := ParseDateTime(Results.Col('Created'));
        Obj.Updated := ParseDateTime(Results.Col('Modified'));
        Obj.Engine := '';
        Obj.Comment := Results.Col('Comment');
        Obj.Version := -1;
        Obj.AutoInc := -1;
        Obj.RowFormat := '';
        Obj.AvgRowLen := -1;
        Obj.MaxDataLen := -1;
        Obj.IndexLen := -1;
        Obj.DataLen := -1;
        Obj.DataFree := -1;
        Obj.LastChecked := 0;
        Obj.Collation := '';
        Obj.CheckSum := -1;
        Obj.CreateOptions := '';
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
        obj := TDBObject.Create;
        Result.Add(obj);
        obj.Name := Results.Col('Name');
        obj.Database := db;
        obj.Rows := -1;
        Obj.Size := -1;
        Obj.NodeType := lntProcedure;
        Obj.Created := ParseDateTime(Results.Col('Created'));
        Obj.Updated := ParseDateTime(Results.Col('Modified'));
        Obj.Engine := '';
        Obj.Comment := Results.Col('Comment');
        Obj.Version := -1;
        Obj.AutoInc := -1;
        Obj.RowFormat := '';
        Obj.AvgRowLen := -1;
        Obj.MaxDataLen := -1;
        Obj.IndexLen := -1;
        Obj.DataLen := -1;
        Obj.DataFree := -1;
        Obj.LastChecked := 0;
        Obj.Collation := '';
        Obj.CheckSum := -1;
        Obj.CreateOptions := '';
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
        obj := TDBObject.Create;
        Result.Add(obj);
        obj.Name := Results.Col('Trigger');
        obj.Database := db;
        obj.Rows := -1;
        Obj.Size := -1;
        Obj.NodeType := lntTrigger;
        Obj.Created := ParseDateTime(Results.Col('Created'));
        Obj.Updated := 0;
        Obj.Engine := '';
        Obj.Comment := Results.Col('Timing')+' '+Results.Col('Event')+' in table '+QuoteIdent(Results.Col('Table'));
        Obj.Version := -1;
        Obj.AutoInc := -1;
        Obj.RowFormat := '';
        Obj.AvgRowLen := -1;
        Obj.MaxDataLen := -1;
        Obj.IndexLen := -1;
        Obj.DataLen := -1;
        Obj.DataFree := -1;
        Obj.LastChecked := 0;
        Obj.Collation := '';
        Obj.CheckSum := -1;
        Obj.CreateOptions := '';
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
          Obj := TDBObject.Create;
          Result.Add(obj);
          Obj.Name := Results.Col('Name');
          Obj.Database := db;
          Obj.Rows := -1;
          Obj.Size := -1;
          Obj.NodeType := lntEvent;
          Obj.Created := 0;
          Obj.Updated := 0;
          Obj.Engine := '';
          Obj.Comment := '';
          Obj.Version := -1;
          Obj.AutoInc := -1;
          Obj.RowFormat := '';
          Obj.AvgRowLen := -1;
          Obj.MaxDataLen := -1;
          Obj.IndexLen := -1;
          Obj.DataLen := -1;
          Obj.DataFree := -1;
          Obj.LastChecked := 0;
          Obj.Collation := '';
          Obj.CheckSum := -1;
          Obj.CreateOptions := '';
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
  FLogCategory := lcSQL;
end;


destructor TMySQLQuery.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FColumnNames);
  FreeAndNil(FColumnOrgNames);
  FreeAndNil(FColumns);
  FreeAndNil(FKeys);
  SetLength(FColumnFlags, 0);
  SetLength(FColumnLengths, 0);
  if HasResult then for i:=Low(FResultList) to High(FResultList) do
    mysql_free_result(FResultList[i]);
  SetLength(FResultList, 0);
  inherited Destroy;
end;


procedure TMySQLQuery.SetSQL(Value: String);
begin
  FSQL := Value;
end;


procedure TMySQLQuery.Execute(AddResult: Boolean=False);
var
  i, j, NumFields: Integer;
  NumResults: Int64;
  Field: PMYSQL_FIELD;
  IsBinary: Boolean;
  FLastResult: PMYSQL_RES;
begin
  FLastResult := Connection.Query(FSQL, FStoreResult, FLogCategory);
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
        FColumnNames.Add(Utf8ToString(Field.name));
        if Connection.ServerVersionInt >= 40100 then
          FColumnOrgNames.Add(Utf8ToString(Field.org_name))
        else
          FColumnOrgNames.Add(String(Field.name));
        FColumnFlags[i] := Field.flags;
        FColumnTypes[i] := Datatypes[Low(Datatypes)];
        if (Field.flags and ENUM_FLAG) = ENUM_FLAG then
          FColumnTypes[i] := Datatypes[Integer(dtEnum)]
        else if (Field.flags and SET_FLAG) = SET_FLAG then
          FColumnTypes[i] := Datatypes[Integer(dtSet)]
        else for j:=Low(Datatypes) to High(Datatypes) do begin
          if Field._type = Datatypes[j].NativeType then begin
            if Datatypes[j].Index in [dtTinytext, dtText, dtMediumtext, dtLongtext] then begin
              // Text and Blob types share the same constants (see FIELD_TYPEs in mysql_api)
              // Some function results return binary collation up to the latest versions. Work around
              // that by checking if this field is a real table field
              // See http://bugs.mysql.com/bug.php?id=10201
              if Connection.IsUnicode then
                IsBinary := (Field.charsetnr = COLLATION_BINARY) and (Field.org_table <> '')
              else
                IsBinary := (Field.flags and BINARY_FLAG) = BINARY_FLAG;
              if IsBinary then
                continue;
            end;
            FColumnTypes[i] := Datatypes[j];
            break;
          end;
        end;
      end;
      RecNo := 0;
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
  NumRows: Int64;
  Row: TRowData;
  RowFound: Boolean;
begin
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
          if (FRecNo+1 <> Value) or (FCurrentRow = nil) then
            mysql_data_seek(FCurrentResults, FCurrentResults.row_count-(NumRows-Value));
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
      if Connection.IsUnicode and (not (Datatype(Column).Category in [dtcBinary, dtcSpatial])) then
        Result := UTF8ToString(AnsiStr)
      else
        Result := String(AnsiStr);
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
  CreateTable: String;
begin
  // Try to fetch column names and keys
  if FEditingPrepared then
    Exit;
  CreateTable := Connection.GetVar('SHOW CREATE TABLE ' + QuotedDbAndTableName, 1);
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
  ParseTableStructure(CreateTable, FColumns, FKeys, FForeignKeys);
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
  else begin
    FCurrentUpdateRow[Column].NewText := NewText;
    if DataType(Column).Category in [dtcInteger, dtcReal] then
      FCurrentUpdateRow[Column].NewText := UnformatNumber(FCurrentUpdateRow[Column].NewText);
  end;
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
      sql := sql + Connection.QuoteIdent(FColumnOrgNames[i]);
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
      end else
        Connection.Query('UPDATE '+QuotedDbAndTableName+' SET '+sqlUpdate+' WHERE '+GetWhereClause);
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
    if FCurrentUpdateRow.Inserted then
      FUpdateData.Remove(FCurrentUpdateRow)
    else for x:=0 to FCurrentUpdateRow.Count-1 do begin
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
  for i:=0 to ColumnCount-1 do begin
    Field := mysql_fetch_field_direct(FCurrentResults, i);
    if Field.db <> '' then begin
      if Connection.IsUnicode then
        Result := UTF8ToString(Field.db)
      else
        Result := String(Field.db);
      break;
    end;
  end;
end;


function TMySQLQuery.TableName: String;
var
  Field: PMYSQL_FIELD;
  i: Integer;
  tbl, db: AnsiString;
begin
  for i:=0 to ColumnCount-1 do begin
    Field := mysql_fetch_field_direct(FCurrentResults, i);
    if (Field.org_table <> '') and (tbl <> '') and ((tbl <> Field.org_table) or (db <> Field.db)) then
      raise EDatabaseError.Create('More than one table involved.');
    if Field.org_table <> '' then begin
      tbl := Field.org_table;
      db := Field.db;
    end;
  end;
  if tbl = '' then
    raise EDatabaseError.Create('Could not determine name of table.')
  else begin
    if Connection.IsUnicode then
      Result := UTF8ToString(tbl)
    else
      Result := String(tbl);
  end;
end;


function TMySQLQuery.QuotedDbAndTableName: String;
var
  db: String;
begin
  // Return `db`.`table` if necessairy, otherwise `table`
  db := DatabaseName;
  if (Connection.Database <> db) and (db <> '') then
    Result := Connection.QuoteIdent(db)+'.';
  Result := Result + Connection.QuoteIdent(TableName);
end;


function TMySQLQuery.GetKeyColumns: TStringList;
var
  NeededCols: TStringList;
  i: Integer;
begin
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
        dtcInteger, dtcReal: Result := Result + '=' + ColVal;
        else Result := Result + '=' + Connection.EscapeString(ColVal);
      end;
    end;
  end;
end;



{ TDBObjectComparer }

function TDBObjectComparer.Compare(const Left, Right: TDBObject): Integer;
begin
  // Simple sort method for a TDBObjectList
  Result := CompareAnyNode(Left.Name, Right.Name);
end;


{ TDBObject }

constructor TDBObject.Create;
begin
  NodeType := lntNone;
end;


procedure TDBObject.Assign(Source: TPersistent);
var
  s: TDBObject;
begin
  if Source is TDBObject then begin
    s := Source as TDBObject;
    Name := s.Name;
    Database := s.Database;
    NodeType := s.NodeType;
    Created := s.Created;
    Updated := s.Updated;
    Comment := s.Comment;
    Rows := s.Rows;
    Size := s.Size;
  end else
    inherited;
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
    else Result := 'Unknown, should never appear';
  end;
end;

function TDBObject.GetImageIndex: Integer;
begin
  // Detect key icon index for specified db object (table, trigger, ...)
  case NodeType of
    lntTable: Result := ICONINDEX_TABLE;
    lntFunction: Result := ICONINDEX_STOREDFUNCTION;
    lntProcedure: Result := ICONINDEX_STOREDPROCEDURE;
    lntView: Result := ICONINDEX_VIEW;
    lntTrigger: Result := ICONINDEX_TRIGGER;
    lntEvent: Result := ICONINDEX_EVENT;
    else Result := -1;
  end;
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



end.
