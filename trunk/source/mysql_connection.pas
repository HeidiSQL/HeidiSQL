unit mysql_connection;

{$M+} // Needed to add published properties

interface

uses
  Classes, SysUtils, windows, mysql_api, mysql_structures, WideStrings, WideStrUtils, cUnicodeCodecs, SynRegExpr;

type

  { TMySQLConnection }

  TMySQLLogCategory = (lcInfo, lcSQL, lcError, lcWarning, lcDebug);
  TMySQLLogEvent = procedure(Msg: WideString; Category: TMySQLLogCategory=lcInfo) of object;
  TMySQLDatabaseChangedEvent = procedure(Database: WideString) of object;

  TMySQLServerCapability = (
    cpShowEngines,          // SHOW ENGINES
    cpShowTableStatus,      // SHOW TABLE STATUS
    cpShowFullTables,       // SHOW FULL TABLES
    cpShowCreateTable,      // SHOW CREATE TABLE foo
    cpShowCreateDatabase,   // SHOW CREATE DATABASE foo
    cpHelpSystem,           // HELP "foo"
    cpSetNames,             // SET NAMES
    cpCalcFoundRows,        // SELECT SQL_CALC_FOUND_ROWS ...
    cpLoadFile,             // LOAD DATA LOCAL INFILE ...
    cpTableComment,         // CREATE TABLE ... COMMENT = "foo"
    cpFieldComment,         // ALTER TABLE ADD ... COMMENT = "foo"
    cpColumnMoving,         // ALTER TABLE CHANGE ... FIRST|AFTER foo
    cpTruncateTable,        // TRUNCATE TABLE foo
    cpAlterDatabase,        // ALTER DATABASE
    cpRenameDatabase        // RENAME DATABASE
    );
  TMySQLServerCapabilities = set of TMySQLServerCapability;

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

const
  DEFAULT_MYSQLOPTIONS = [opCompress, opLocalFiles, opInteractive, opProtocol41, opMultiStatements];

type
  TMySQLQuery = class;
  TMySQLConnection = class(TComponent)
    private
      FHandle: PMYSQL;
      FActive: Boolean;
      FConnectionStarted: Integer;
      FServerStarted: Integer;
      FHostname: String;
      FSocketname: String;
      FPort: Integer;
      FUsername: String;
      FPassword: String;
      FDatabase: WideString;
      FLogPrefix: WideString;
      FOnLog: TMySQLLogEvent;
      FOnDatabaseChanged: TMySQLDatabaseChangedEvent;
      FOptions: TMySQLClientOptions;
      FCapabilities: TMySQLServerCapabilities;
      FRowsFound: Int64;
      FRowsAffected: Int64;
      FServerVersionUntouched: String;
      FLastQueryDuration, FLastQueryNetworkDuration: Cardinal;
      FIsUnicode: Boolean;
      FTableEngines: TStringList;
      FTableEngineDefault: String;
      FCollationTable: TMySQLQuery;
      FCharsetTable: TMySQLQuery;
      procedure SetActive(Value: Boolean);
      procedure SetDatabase(Value: WideString);
      function GetThreadId: Cardinal;
      function GetCharacterSet: String;
      procedure SetCharacterSet(CharsetName: String);
      function GetLastError: WideString;
      function GetServerVersionStr: String;
      function GetServerVersionInt: Integer;
      function GetTableEngines: TStringList;
      function GetCollationTable: TMySQLQuery;
      function GetCollationList: TStringList;
      function GetCharsetTable: TMySQLQuery;
      function GetCharsetList: TStringList;
      function GetConnectionUptime: Integer;
      function GetServerUptime: Integer;
      procedure Log(Category: TMySQLLogCategory; Msg: WideString);
      procedure DetectCapabilities;
      procedure ClearCache;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Query(SQL: WideString; DoStoreResult: Boolean=False): PMYSQL_RES;
      function EscapeString(Text: WideString; DoQuote: Boolean=True): WideString;
      function QuoteIdent(Identifier: WideString): WideString;
      function DeQuoteIdent(Identifier: WideString): WideString;
      function ConvertServerVersion(Version: Integer): String;
      function GetResults(SQL: WideString): TMySQLQuery;
      function GetCol(SQL: WideString; Column: Integer=0): TWideStringList;
      function GetVar(SQL: WideString; Column: Integer=0): WideString; overload;
      function GetVar(SQL: WideString; Column: WideString): WideString; overload;
      function Ping: Boolean;
      property ThreadId: Cardinal read GetThreadId;
      property ConnectionUptime: Integer read GetConnectionUptime;
      property ServerUptime: Integer read GetServerUptime;
      property CharacterSet: String read GetCharacterSet write SetCharacterSet;
      property LastError: WideString read GetLastError;
      property ServerVersionUntouched: String read FServerVersionUntouched;
      property ServerVersionStr: String read GetServerVersionStr;
      property ServerVersionInt: Integer read GetServerVersionInt;
      property Capabilities: TMySQLServerCapabilities read FCapabilities;
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
    published
      property Active: Boolean read FActive write SetActive default False;
      property Hostname: String read FHostname write FHostname;
      property Socketname: String read FSocketname write FSocketname;
      property Port: Integer read FPort write FPort default MYSQL_PORT;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property Database: WideString read FDatabase write SetDatabase;
      property Options: TMySQLClientOptions read FOptions write FOptions default [opCompress, opLocalFiles, opInteractive, opProtocol41];
      property LogPrefix: WideString read FLogPrefix write FLogPrefix;
      // Events
      property OnLog: TMySQLLogEvent read FOnLog write FOnLog;
      property OnDatabaseChanged: TMySQLDatabaseChangedEvent read FOnDatabaseChanged write FOnDatabaseChanged;
  end;


  { TMySQLQuery }

  TMySQLQuery = class(TComponent)
    private
      FSQL: WideString;
      FConnection: TMySQLConnection;
      FRecNo,
      FRecordCount: Int64;
      FColumnNames: TWideStringList;
      FLastResult: PMYSQL_RES;
      FCurrentRow: PMYSQL_ROW;
      FEof: Boolean;
      FDatatypes: Array of TDatatype;
      procedure SetSQL(Value: WideString);
      procedure SetRecNo(Value: Int64);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Execute;
      procedure First;
      procedure Next;
      function ColumnCount: Integer;
      function Col(Column: Integer; IgnoreErrors: Boolean=False): WideString; overload;
      function Col(ColumnName: WideString; IgnoreErrors: Boolean=False): WideString; overload;
      function DataType(Column: Integer): TDataType;
      function ColExists(Column: WideString): Boolean;
      function IsNull(Column: Integer): Boolean;
      function HasResult: Boolean;
      property RecNo: Int64 read FRecNo write SetRecNo;
      property Eof: Boolean read FEof;
      property RecordCount: Int64 read FRecordCount;
      property ColumnNames: TWideStringList read FColumnNames;
    published
      property SQL: WideString read FSQL write SetSQL;
      property Connection: TMySQLConnection read FConnection write FConnection;
  end;


implementation


{ TMySQLConnection }

constructor TMySQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := DEFAULT_MYSQLOPTIONS;
  FPort := MYSQL_PORT;
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
  Error, tmpdb: WideString;
  UsingPass, Protocol, CurCharset: String;
begin
  FActive := Value;

  if Value and (FHandle = nil) then begin
    // Get handle
    FHandle := mysql_init(nil);

    // Gather client options
    ClientFlags := 0;
    if opRememberOptions   in FOptions then ClientFlags := ClientFlags or CLIENT_REMEMBER_OPTIONS;
    if opLongPassword      in FOptions then ClientFlags := ClientFlags or CLIENT_LONG_PASSWORD;
    if opFoundRows         in FOptions then ClientFlags := ClientFlags or CLIENT_FOUND_ROWS;
    if opLongFlag          in FOptions then ClientFlags := ClientFlags or CLIENT_LONG_FLAG;
    if opConnectWithDb     in FOptions then ClientFlags := ClientFlags or CLIENT_CONNECT_WITH_DB;
    if opNoSchema          in FOptions then ClientFlags := ClientFlags or CLIENT_NO_SCHEMA;
    if opCompress          in FOptions then ClientFlags := ClientFlags or CLIENT_COMPRESS;
    if opODBC              in FOptions then ClientFlags := ClientFlags or CLIENT_ODBC;
    if opLocalFiles        in FOptions then ClientFlags := ClientFlags or CLIENT_LOCAL_FILES;
    if opIgnoreSpace       in FOptions then ClientFlags := ClientFlags or CLIENT_IGNORE_SPACE;
    if opProtocol41        in FOptions then ClientFlags := ClientFlags or CLIENT_PROTOCOL_41;
    if opInteractive       in FOptions then ClientFlags := ClientFlags or CLIENT_INTERACTIVE;
    if opSSL               in FOptions then ClientFlags := ClientFlags or CLIENT_SSL;
    if opIgnoreSigpipe     in FOptions then ClientFlags := ClientFlags or CLIENT_IGNORE_SIGPIPE;
    if opTransactions      in FOptions then ClientFlags := ClientFlags or CLIENT_TRANSACTIONS;
    if opReserved          in FOptions then ClientFlags := ClientFlags or CLIENT_RESERVED;
    if opSecureConnection  in FOptions then ClientFlags := ClientFlags or CLIENT_SECURE_CONNECTION;
    if opMultiStatements   in FOptions then ClientFlags := ClientFlags or CLIENT_MULTI_STATEMENTS;
    if opMultiResults      in FOptions then ClientFlags := ClientFlags or CLIENT_MULTI_RESULTS;
    if opRememberOptions   in FOptions then ClientFlags := ClientFlags or CLIENT_REMEMBER_OPTIONS;

    // Prepare connection
    if FHostname = '.' then Protocol := 'named pipe' else Protocol := 'TCP/IP';
    if Password <> '' then UsingPass := 'Yes' else UsingPass := 'No';
    Log(lcInfo, 'Connecting to '+Hostname+' via '+Protocol+
      ', username '+Username+
      ', using password: '+UsingPass+' ...');
    Connected := mysql_real_connect(
      FHandle,
      PChar(FHostname),
      PChar(FUsername),
      PChar(FPassword),
      nil,
      FPort,
      PChar(FSocketname),
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
      FServerVersionUntouched := mysql_get_server_info(FHandle);
      DetectCapabilities;
      tmpdb := FDatabase;
      FDatabase := '';
      SetDatabase(tmpdb);
    end;
  end

  else if (not Value) and (FHandle <> nil) then begin
    mysql_close(FHandle);
    FConnectionStarted := 0;
    FHandle := nil;
    FCapabilities := [];
    Log(lcInfo, 'Connection to '+FHostname+' closed at '+DateTimeToStr(Now));
  end;

end;


function TMySQLConnection.Ping: Boolean;
begin
  if FActive and (mysql_ping(FHandle) <> 0) then begin
    Active := False;
    ClearCache;
  end;
  Result := FActive;
end;


{**
   Executes a query
}
function TMySQLConnection.Query(SQL: WideString; DoStoreResult: Boolean=False): PMYSQL_RES;
var
  querystatus: Integer;
  NativeSQL: String;
  TimerStart: Cardinal;
begin
  if not Ping then
    Active := True;
  Log(lcSQL, SQL);
  NativeSQL := UTF8Encode(SQL);
  TimerStart := GetTickCount;
  querystatus := mysql_real_query(FHandle, PChar(NativeSQL), Length(NativeSQL));
  FLastQueryDuration := GetTickCount - TimerStart;
  FLastQueryNetworkDuration := 0;
  if querystatus <> 0 then begin
    Log(lcError, GetLastError);
    raise Exception.Create(GetLastError);
  end else begin
    // We must call mysql_store_result() + mysql_free_result() to unblock the connection
    // See: http://dev.mysql.com/doc/refman/5.0/en/mysql-store-result.html 
    FRowsAffected := mysql_affected_rows(FHandle);
    TimerStart := GetTickCount;
    Result := mysql_store_result(FHandle);
    FLastQueryNetworkDuration := GetTickCount - TimerStart;
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
procedure TMySQLConnection.SetDatabase(Value: WideString);
begin
  if (Value = '') or (Value = FDatabase) then
    Exit;
  Query('USE '+QuoteIdent(Value), False);
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
  Result := mysql_character_set_name(FHandle);
end;


{**
  Switch character set
}
procedure TMySQLConnection.SetCharacterSet(CharsetName: String);
begin
  mysql_set_character_set(FHandle, PAnsiChar(CharsetName));
end;


{**
  Return the last error nicely formatted
}
function TMySQLConnection.GetLastError: WideString;
var
  Msg, Additional: WideString;
  rx: TRegExpr;
begin
  Msg := Utf8Decode(mysql_error(FHandle));
  // Find "(errno: 123)" in message and add more meaningful message from perror.exe
  rx := TRegExpr.Create;
  rx.Expression := '.+\(errno\:\s+(\d+)\)';
  if rx.Exec(Msg) then begin
    Additional := MySQLErrorCodes.Values[rx.Match[1]];
    if Additional <> '' then
      Msg := Msg + CRLF + CRLF + Additional;
  end;
  rx.Free;
  Result := WideFormat('SQL Error (%d): %s', [mysql_errno(FHandle), Msg]);
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
    end else if FServerVersionUntouched[i] in ['0'..'9'] then begin
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


function TMySQLConnection.GetResults(SQL: WideString): TMySQLQuery;
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
procedure TMySQLConnection.Log(Category: TMySQLLogCategory; Msg: WideString);
begin
  if Assigned(FOnLog) then
    FOnLog(FLogPrefix+Msg, Category);
end;


{**
  Escapes a string for usage in SQL queries
}
function TMySQLConnection.EscapeString(Text: WideString; DoQuote: Boolean): WideString;
var
  BufferLen: Integer;
  Buffer: PChar;
  NativeText: String;
begin
  BufferLen := Length(Text) * 2 + 1;
  GetMem(Buffer, BufferLen);
  NativeText := UTF8Encode(Text);
  BufferLen := mysql_real_escape_string(FHandle, Buffer, PChar(NativeText), Length(Text));
  SetString(Result, Buffer, BufferLen);
  FreeMem(Buffer);

  if DoQuote then
    Result := '''' + Result + '''';
end;


{**
  Add backticks to identifier
  Todo: Support ANSI style
}
function TMySQLConnection.QuoteIdent(Identifier: WideString): WideString;
begin
  Result := WideStringReplace(Identifier, '`', '``', [rfReplaceAll]);
  Result := '`' + Result + '`';
end;


function TMySQLConnection.DeQuoteIdent(Identifier: WideString): WideString;
begin
  Result := Identifier;
  if (Result[1] = '`') and (Result[Length(Identifier)] = '`') then
    Result := Copy(Result, 2, Length(Result)-2);
end;


{**
  Detect various capabilities of the server
  for easy feature-checks in client-applications.
}
procedure TMySQLConnection.DetectCapabilities;
var
  ver: Integer;
  procedure addCap(c: TMySQLServerCapability; addit: Boolean);
  begin
    if addit then
      Include(FCapabilities, c)
    else
      Exclude(FCapabilities, c);
  end;
begin
  // Avoid calling GetServerVersionInt too often
  ver := ServerVersionInt;

  addCap(cpShowEngines, ver >= 40102);
  addCap(cpShowTableStatus, ver >= 32300);
  addCap(cpShowFullTables, ver >= 50002);
  addCap(cpShowCreateTable, ver >= 32320);
  addCap(cpShowCreateDatabase, ver >= 50002);
  addCap(cpHelpSystem, ver >= 40100);
  addCap(cpSetNames, ver >= 40100);
  addCap(cpCalcFoundRows, ver >= 40000);
  addCap(cpLoadFile, ver >= 32206);
  addCap(cpTableComment, ver >= 32300);
  addCap(cpFieldComment, ver >= 40100);
  addCap(cpColumnMoving, ver >= 40001);
  addCap(cpTruncateTable, ver >= 50003);
  addCap(cpAlterDatabase, ver >= 50002);
  addCap(cpRenameDatabase, ver >= 50107);
end;


function TMySQLConnection.GetCol(SQL: WideString; Column: Integer=0): TWideStringList;
var
  Results: TMySQLQuery;
begin
  Results := GetResults(SQL);
  Result := TWideStringList.Create;
  if Results.RecordCount > 0 then while not Results.Eof do begin
    Result.Add(Results.Col(Column));
    Results.Next;
  end;
  FreeAndNil(Results);
end;


{**
  Get single cell value via SQL query, identified by column number
}
function TMySQLConnection.GetVar(SQL: WideString; Column: Integer=0): WideString;
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
function TMySQLConnection.GetVar(SQL: WideString; Column: WideString): WideString;
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
  if Assigned(FCharsetTable) then
    FCharsetTable.First;
  Result := FCharsetTable;
end;


function TMySQLConnection.GetCharsetList: TStringList;
var
  c: TMySQLQuery;
begin
  c := CharsetTable;
  Result := TStringList.Create;
  if Assigned(c) then while not c.Eof do begin
    Result.Add(c.Col('Description') + ' (' + c.Col('Charset') + ')');
    c.Next;
  end;
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
  FTableEngineDefault := '';
end;



{ TMySQLQuery }

constructor TMySQLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecNo := -1;
  FRecordCount := 0;
  FColumnNames := TWideStringlist.Create;
  FColumnNames.CaseSensitive := True;
end;


destructor TMySQLQuery.Destroy;
begin
  FreeAndNil(FColumnNames);
  if HasResult then
    mysql_free_result(FLastResult);
  inherited Destroy;
end;


procedure TMySQLQuery.SetSQL(Value: WideString);
begin
  FSQL := Value;
end;


procedure TMySQLQuery.Execute;
var
  i, j, NumFields: Integer;
  Field: PMYSQL_FIELD;
  IsBinary, Bug10491: Boolean;
begin
  FLastResult := Connection.Query(FSQL, True);
  FRecordCount := Connection.RowsFound;
  if HasResult then begin
    NumFields := mysql_num_fields(FLastResult);
    SetLength(FDatatypes, NumFields);
    // Workaround for bug 10491 and friends, see http://bugs.mysql.com/10491
    Bug10491 := (Connection.ServerVersionInt < 50046) and
      (Connection.ServerVersionInt >= 40100) and
      (UpperCase(Copy(Trim(FSQL), 1, 4)) = 'SHOW');
    for i:=0 to NumFields-1 do begin
      Field := mysql_fetch_field_direct(FLastResult, i);
      FColumnNames.Add(Utf8Decode(Field.name));

      FDatatypes[i] := Datatypes[Low(Datatypes)];
      if Bug10491 then
        FDatatypes[i] := Datatypes[Integer(dtText)]
      else if (Field.flags and ENUM_FLAG) = ENUM_FLAG then
        FDatatypes[i] := Datatypes[Integer(dtEnum)]
      else if (Field.flags and SET_FLAG) = SET_FLAG then
        FDatatypes[i] := Datatypes[Integer(dtSet)]
      else for j:=Low(Datatypes) to High(Datatypes) do begin
        if Field._type = Datatypes[j].NativeType then begin
          // Text and Blob types share the same constants (see FIELD_TYPEs in mysql_api)
          if Connection.IsUnicode then
            IsBinary := Field.charsetnr = COLLATION_BINARY
          else
            IsBinary := (Field.flags and BINARY_FLAG) = BINARY_FLAG;
          if IsBinary and (Datatypes[j].Category = dtcText) then
            continue;
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


function TMySQLQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): WideString;
var
  LengthPointer: PLongInt;
  BinLen: LongInt;
  Bin: String;
begin
  if (Column > -1) and (Column < ColumnCount) then begin
    if FDatatypes[Column].Category = dtcBinary then begin
      LengthPointer := mysql_fetch_lengths(FLastResult);
      if LengthPointer <> nil then begin
        BinLen := PLongInt(LongInt(LengthPointer) + Column * SizeOf(LongInt))^;
        SetString(Bin, FCurrentRow[Column], BinLen);
        Result := WideString(Bin);
      end;
    end else begin
      if Connection.IsUnicode then
        Result := UTF8StringToWideString(FCurrentRow[Column])
      else
        Result := FCurrentRow[Column];
    end;
  end else if not IgnoreErrors then
    Raise Exception.CreateFmt('Column #%d not available. Query returned %d columns and %d rows.', [Column, ColumnCount, RecordCount]);
end;


function TMySQLQuery.Col(ColumnName: WideString; IgnoreErrors: Boolean=False): WideString;
var
  idx: Integer;
begin
  idx := ColumnNames.IndexOf(ColumnName);
  if idx > -1 then
    Result := Col(idx)
  else if not IgnoreErrors then
    Raise Exception.CreateFmt('Column "%s" not available.', [ColumnName]);
end;


function TMySQLQuery.DataType(Column: Integer): TDataType;
begin
  Result := FDatatypes[Column];
end;


function TMySQLQuery.ColExists(Column: WideString): Boolean;
begin
  Result := (ColumnNames <> nil) and (ColumnNames.IndexOf(Column) > -1);
end;


function TMySQLQuery.IsNull(Column: Integer): Boolean;
begin
  Result := FCurrentRow[Column] = nil;
end;


function TMySQLQuery.HasResult: Boolean;
begin
  Result := FLastResult <> nil;
end;


end.
