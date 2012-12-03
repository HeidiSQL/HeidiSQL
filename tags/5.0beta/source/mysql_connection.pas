unit mysql_connection;

interface

uses
  Classes, SysUtils, windows, mysql_api, mysql_structures, SynRegExpr, Contnrs, Generics.Collections, DateUtils;

type
  { TDBObjectList and friends }

  TListNodeType = (lntNone, lntDb, lntTable, lntView, lntFunction, lntProcedure, lntTrigger, lntColumn);
  TListNodeTypes = Set of TListNodeType;
  TDBObject = class
    Name, Database, Engine, Comment, RowFormat, CreateOptions, Collation, ObjType: String;
    Created, Updated, LastChecked: TDateTime;
    Rows, Size, Version, AvgRowLen, MaxDataLen, IndexLen, DataLen, DataFree, AutoInc, CheckSum: Int64;
    NodeType: TListNodeType;
  end;
  TDBObjectList = TObjectList<TDBObject>;

  {$M+} // Needed to add published properties

  { TConnectionParameters }

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
      FHostname, FSocketname, FUsername, FPassword: String;
      FPort: Integer;
      FOptions: TMySQLClientOptions;
    public
      constructor Create;
    published
      property Hostname: String read FHostname write FHostname;
      property Socketname: String read FSocketname write FSocketname;
      property Port: Integer read FPort write FPort;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property Options: TMySQLClientOptions read FOptions write FOptions;
  end;


  { TMySQLConnection }

  TMySQLLogCategory = (lcInfo, lcSQL, lcError, lcWarning, lcDebug);
  TMySQLLogEvent = procedure(Msg: String; Category: TMySQLLogCategory=lcInfo) of object;
  TMySQLDatabaseChangedEvent = procedure(Database: String) of object;

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
      FOnDatabaseChanged: TMySQLDatabaseChangedEvent;
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
      FDBObjectLists: TStringList;
      procedure SetActive(Value: Boolean);
      procedure SetDatabase(Value: String);
      function GetThreadId: Cardinal;
      function GetCharacterSet: String;
      procedure SetCharacterSet(CharsetName: String);
      function GetLastError: String;
      function GetServerVersionStr: String;
      function GetServerVersionInt: Integer;
      function GetTableEngines: TStringList;
      function GetCollationTable: TMySQLQuery;
      function GetCollationList: TStringList;
      function GetCharsetTable: TMySQLQuery;
      function GetCharsetList: TStringList;
      function GetInformationSchemaObjects: TStringList;
      function GetConnectionUptime: Integer;
      function GetServerUptime: Integer;
      function ParseDateTime(Str: String): TDateTime;
      procedure Log(Category: TMySQLLogCategory; Msg: String);
      procedure ClearCache;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Query(SQL: String; DoStoreResult: Boolean=False): PMYSQL_RES;
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
      procedure ClearDbObjects(db: String='');
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
      property TableEngines: TStringList read GetTableEngines;
      property TableEngineDefault: String read FTableEngineDefault;
      property CollationTable: TMySQLQuery read GetCollationTable;
      property CollationList: TStringList read GetCollationList;
      property CharsetTable: TMySQLQuery read GetCharsetTable;
      property CharsetList: TStringList read GetCharsetList;
      property InformationSchemaObjects: TStringList read GetInformationSchemaObjects;
    published
      property Active: Boolean read FActive write SetActive default False;
      property Database: String read FDatabase write SetDatabase;
      property LogPrefix: String read FLogPrefix write FLogPrefix;
      // Events
      property OnLog: TMySQLLogEvent read FOnLog write FOnLog;
      property OnDatabaseChanged: TMySQLDatabaseChangedEvent read FOnDatabaseChanged write FOnDatabaseChanged;
  end;


  { TMySQLQuery }

  TMySQLQuery = class(TComponent)
    private
      FSQL: String;
      FConnection: TMySQLConnection;
      FRecNo,
      FRecordCount: Int64;
      FColumnNames: TStringList;
      FLastResult: PMYSQL_RES;
      FCurrentRow: PMYSQL_ROW;
      FEof: Boolean;
      FDatatypes: Array of TDatatype;
      procedure SetSQL(Value: String);
      procedure SetRecNo(Value: Int64);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute;
      procedure First;
      procedure Next;
      function ColumnCount: Integer;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): String; overload;
      function Col(ColumnName: String; IgnoreErrors: Boolean=False): String; overload;
      function BinColAsHex(Column: Integer; IgnoreErrors: Boolean=False): String;
      function DataType(Column: Integer): TDataType;
      function ColExists(Column: String): Boolean;
      function ColIsPrimaryKeyPart(Column: Integer): Boolean;
      function IsNull(Column: Integer): Boolean; overload;
      function IsNull(Column: String): Boolean; overload;
      function HasResult: Boolean;
      property RecNo: Int64 read FRecNo write SetRecNo;
      property Eof: Boolean read FEof;
      property RecordCount: Int64 read FRecordCount;
      property ColumnNames: TStringList read FColumnNames;
    published
      property SQL: String read FSQL write SetSQL;
      property Connection: TMySQLConnection read FConnection write FConnection;
  end;


implementation


{ TConnectionParameters }

constructor TConnectionParameters.Create;
begin
  FHostname := DEFAULT_HOST;
  FSocketname := '';
  FUsername := DEFAULT_USER;
  FPassword := '';
  FPort := DEFAULT_PORT;
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
end;


destructor TMySQLConnection.Destroy;
begin
  if Active then Active := False;
  ClearCache;
  inherited Destroy;
end;


{**
  (Dis-)Connect to/from server
}
procedure TMySQLConnection.SetActive( Value: Boolean );
var
  Connected: PMYSQL;
  ClientFlags: Integer;
  Error, tmpdb: String;
  UsingPass, Protocol, CurCharset: String;
begin
  FActive := Value;

  if Value and (FHandle = nil) then begin
    // Get handle
    FHandle := mysql_init(nil);

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

    // Prepare connection
    if FParameters.Hostname = '.' then Protocol := 'named pipe' else Protocol := 'TCP/IP';
    if FParameters.Password <> '' then UsingPass := 'Yes' else UsingPass := 'No';
    Log(lcInfo, 'Connecting to '+FParameters.Hostname+' via '+Protocol+
      ', username '+FParameters.Username+
      ', using password: '+UsingPass+' ...');
    Connected := mysql_real_connect(
      FHandle,
      PAnsiChar(Utf8Encode(FParameters.Hostname)),
      PAnsiChar(Utf8Encode(FParameters.Username)),
      PAnsiChar(Utf8Encode(FParameters.Password)),
      nil,
      FParameters.Port,
      PAnsiChar(Utf8Encode(FParameters.Socketname)),
      ClientFlags
      );
    if Connected = nil then begin
      Error := LastError;
      Log(lcError, Error);
      FActive := False;
      FConnectionStarted := 0;
      FHandle := nil;
      raise Exception.Create(Error);
    end else begin
      Log(lcInfo, 'Connected. Thread-ID: '+IntToStr(ThreadId));
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
    FConnectionStarted := 0;
    FHandle := nil;
    Log(lcInfo, 'Connection to '+FParameters.Hostname+' closed at '+DateTimeToStr(Now));
  end;

end;


function TMySQLConnection.Ping: Boolean;
begin
  if FActive and ((FHandle=nil) or (mysql_ping(FHandle) <> 0)) then begin
    Active := False;
    ClearCache;
  end;
  Result := FActive;
end;


{**
   Executes a query
}
function TMySQLConnection.Query(SQL: String; DoStoreResult: Boolean=False): PMYSQL_RES;
var
  querystatus: Integer;
  NativeSQL: AnsiString;
  TimerStart: Cardinal;
begin
  if not Ping then
    Active := True;
  Log(lcSQL, SQL);
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
    raise Exception.Create(GetLastError);
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
      raise Exception.Create(GetLastError);
    end;
    if Result <> nil then begin
      FRowsFound := mysql_num_rows(Result);
      FRowsAffected := 0;
      Log(lcDebug, IntToStr(RowsFound)+' rows found.');
      if not DoStoreResult then
        mysql_free_result(Result);
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
  ClearDbObjects;
  FTableEngineDefault := '';
end;


procedure TMySQLConnection.ClearDbObjects(db: String='');
var
  i: Integer;
begin
  // Free all cached database object lists, or, if db is passed, only that one
  if not Assigned(FDBObjectLists) then
    Exit;
  if db <> '' then begin
    i := FDBObjectLists.IndexOf(db);
    if i = -1 then
      Exit;
    TDBObjectList(FDBObjectLists.Objects[i]).Free;
    FDBObjectLists.Delete(i);
  end else begin
    for i:=0 to FDBObjectLists.Count-1 do
      TDBObjectList(FDBObjectLists.Objects[i]).Free;
    FDBObjectLists.Clear;
  end;
end;


function TMySQLConnection.DbObjectsCached(db: String): Boolean;
begin
  // Check if a table list is stored in cache
  Result := Assigned(FDBObjectLists) and (FDBObjectLists.IndexOf(db) > -1);
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
begin
  // Cache and return a db's table list
  if Refresh then
    ClearDbObjects(db);
  if DbObjectsCached(db) then
    Result := FDBObjectLists.Objects[FDBObjectLists.IndexOf(db)] as TDBObjectList
  else begin
    Result := TDBObjectList.Create;
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
        else
          Obj.Size := StrToInt64Def(Results.Col('Data_length'), 0) + StrToInt64Def(Results.Col('Index_length'), 0);
        Obj.ObjType := 'TABLE';
        Obj.NodeType := lntTable;
        if Results.IsNull(1) and Results.IsNull(2) then begin // Engine column is NULL for views
          Obj.NodeType := lntView;
          Obj.ObjType := 'VIEW';
        end;
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
        Obj.ObjType := 'FUNCTION';
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
        Obj.ObjType := 'PROCEDURE';
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
        Obj.ObjType := 'TRIGGER';
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

    // Add list of objects in this database to cached list of all databases
    if not Assigned(FDBObjectLists) then
      FDBObjectLists := TStringList.Create;
    FDBObjectLists.AddObject(db, Result);
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
end;


destructor TMySQLQuery.Destroy;
begin
  FreeAndNil(FColumnNames);
  if HasResult then
    mysql_free_result(FLastResult);
  inherited Destroy;
end;


procedure TMySQLQuery.SetSQL(Value: String);
begin
  FSQL := Value;
end;


procedure TMySQLQuery.Execute;
var
  i, j, NumFields: Integer;
  Field: PMYSQL_FIELD;
  IsBinary: Boolean;
begin
  FLastResult := Connection.Query(FSQL, True);
  FRecordCount := Connection.RowsFound;
  if HasResult then begin
    NumFields := mysql_num_fields(FLastResult);
    SetLength(FDatatypes, NumFields);
    for i:=0 to NumFields-1 do begin
      Field := mysql_fetch_field_direct(FLastResult, i);
      FColumnNames.Add(Utf8ToString(Field.name));

      FDatatypes[i] := Datatypes[Low(Datatypes)];
      if (Field.flags and ENUM_FLAG) = ENUM_FLAG then
        FDatatypes[i] := Datatypes[Integer(dtEnum)]
      else if (Field.flags and SET_FLAG) = SET_FLAG then
        FDatatypes[i] := Datatypes[Integer(dtSet)]
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
          FDatatypes[i] := Datatypes[j];
          break;
        end;
      end;
    end;
    RecNo := 0;
  end else
    SetLength(FDatatypes, 0);
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
begin
  if Value >= RecordCount then begin
    FRecNo := RecordCount;
    FEof := True;
  end else begin
    if FRecNo+1 <> Value then
      mysql_data_seek(FLastResult, Value);
    FRecNo := Value;
    FEof := False;
    FCurrentRow := mysql_fetch_row(FLastResult);
  end;
end;


function TMySQLQuery.ColumnCount: Integer;
begin
  Result := ColumnNames.Count;
end;


function TMySQLQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): String;
begin
  if (Column > -1) and (Column < ColumnCount) then begin
    if Connection.IsUnicode then
      Result := UTF8ToString(FCurrentRow[Column])
    else
      Result := String(FCurrentRow[Column]);
  end else if not IgnoreErrors then
    Raise Exception.CreateFmt('Column #%d not available. Query returned %d columns and %d rows.', [Column, ColumnCount, RecordCount]);
end;


function TMySQLQuery.Col(ColumnName: String; IgnoreErrors: Boolean=False): String;
var
  idx: Integer;
begin
  idx := ColumnNames.IndexOf(ColumnName);
  if idx > -1 then
    Result := Col(idx)
  else if not IgnoreErrors then
    Raise Exception.CreateFmt('Column "%s" not available.', [ColumnName]);
end;


function TMySQLQuery.BinColAsHex(Column: Integer; IgnoreErrors: Boolean=False): String;
var
  LengthPointer: PLongInt;
  BinLen: Integer;
begin
  // Return a binary column value as hex AnsiString
  if (Column > -1) and (Column < ColumnCount) then begin
    LengthPointer := mysql_fetch_lengths(FLastResult);
    if LengthPointer <> nil then begin
      BinLen := PInteger(Integer(LengthPointer) + Column * SizeOf(Integer))^;
      SetLength(Result, BinLen*2);
      BinToHex(FCurrentRow[Column], PChar(Result), BinLen);
    end;
  end else if not IgnoreErrors then
    Raise Exception.CreateFmt('Column #%d not available. Query returned %d columns and %d rows.', [Column, ColumnCount, RecordCount]);
end;


function TMySQLQuery.DataType(Column: Integer): TDataType;
begin
  Result := FDatatypes[Column];
end;


function TMySQLQuery.ColExists(Column: String): Boolean;
begin
  Result := (ColumnNames <> nil) and (ColumnNames.IndexOf(Column) > -1);
end;


function TMySQLQuery.ColIsPrimaryKeyPart(Column: Integer): Boolean;
var
  Field: PMYSQL_FIELD;
begin
  if HasResult and (Column < ColumnCount) then begin
    Field := mysql_fetch_field_direct(FLastResult, Column);
    Result := (Field.flags and PRI_KEY_FLAG) = PRI_KEY_FLAG;
  end else
    Result := False;
end;


function TMySQLQuery.IsNull(Column: Integer): Boolean;
begin
  Result := FCurrentRow[Column] = nil;
end;


function TMySQLQuery.IsNull(Column: String): Boolean;
begin
  Result := IsNull(FColumnNames.IndexOf(Column));
end;


function TMySQLQuery.HasResult: Boolean;
begin
  Result := FLastResult <> nil;
end;



end.