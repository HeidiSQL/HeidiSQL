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
      FConnectionStarted: Cardinal;
      FHostname: String;
      FSocketname: String;
      FPort: Integer;
      FUsername: String;
      FPassword: String;
      FDatabase: WideString;
      FOnLog: TMySQLLogEvent;
      FOnDatabaseChanged: TMySQLDatabaseChangedEvent;
      FOptions: TMySQLClientOptions;
      FCapabilities: TMySQLServerCapabilities;
      FRowsFound: Int64;
      FRowsAffected: Int64;
      FServerVersionUntouched: String;
      FLastQueryStart, FLastQueryEnd: Cardinal;
      function GetActive: Boolean;
      procedure SetActive(Value: Boolean);
      procedure SetDatabase(Value: WideString);
      function GetThreadId: Cardinal;
      function GetCharacterSet: String;
      procedure SetCharacterSet(CharsetName: String);
      function GetLastError: WideString;
      function GetServerVersionStr: String;
      function GetServerVersionInt: Integer;
      function GetLastQueryDuration: Cardinal;
      procedure Log(Category: TMySQLLogCategory; Msg: WideString);
      procedure DetectCapabilities;
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
      property ThreadId: Cardinal read GetThreadId;
      property ConnectionStarted: Cardinal read FConnectionStarted;
      property CharacterSet: String read GetCharacterSet write SetCharacterSet;
      property LastError: WideString read GetLastError;
      property ServerVersionUntouched: String read FServerVersionUntouched;
      property ServerVersionStr: String read GetServerVersionStr;
      property ServerVersionInt: Integer read GetServerVersionInt;
      property Capabilities: TMySQLServerCapabilities read FCapabilities;
      property RowsFound: Int64 read FRowsFound;
      property RowsAffected: Int64 read FRowsAffected;
      property LastQueryDuration: Cardinal read GetLastQueryDuration;
    published
      property Active: Boolean read GetActive write SetActive default False;
      property Hostname: String read FHostname write FHostname;
      property Socketname: String read FSocketname write FSocketname;
      property Port: Integer read FPort write FPort default MYSQL_PORT;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property Database: WideString read FDatabase write SetDatabase;
      property Options: TMySQLClientOptions read FOptions write FOptions default [opCompress, opLocalFiles, opInteractive, opProtocol41];
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
  FLastQueryStart := 0;
  FLastQueryEnd := 0;
end;


destructor TMySQLConnection.Destroy;
begin
  if Active then Active := False;
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
  UsingPass, Protocol: String;
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
      Log(lcInfo, 'Characterset: '+CharacterSet);
      FConnectionStarted := GetTickCount;
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


function TMySQLConnection.GetActive: Boolean;
begin
  if FActive and (mysql_ping(FHandle) <> 0) then
    Active := False;
  Result := FActive;
end;


{**
   Executes a query
}
function TMySQLConnection.Query(SQL: WideString; DoStoreResult: Boolean=False): PMYSQL_RES;
var
  querystatus: Integer;
  NativeSQL: String;
begin
  if not Active then
    Active := True;
  Log(lcSQL, SQL);
  NativeSQL := UTF8Encode(SQL);
  FLastQueryStart := GetTickCount;
  querystatus := mysql_real_query(FHandle, PChar(NativeSQL), Length(NativeSQL));
  FLastQueryEnd := GetTickCount;
  if querystatus <> 0 then begin
    Log(lcError, GetLastError);
    raise Exception.Create(GetLastError);
  end else begin
    // We must call mysql_store_result() + mysql_free_result() to unblock the connection
    // See: http://dev.mysql.com/doc/refman/5.0/en/mysql-store-result.html 
    FRowsAffected := mysql_affected_rows(FHandle);
    Result := mysql_store_result(FHandle);
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
  fullversion, v1, v2, v3: String;
begin
  Result := -1;

  dots := 0;
  // Avoid calling GetServerVersionUntouched too often
  fullversion := ServerVersionUntouched;
  v1 := '';
  v2 := '';
  v3 := '';
  for i:=1 to Length(fullversion) do begin
    if fullversion[i] = '.' then begin
      inc(dots);
      // We expect exactly 2 dots.
      if dots > 2 then
        break;
    end else if fullversion[i] in ['0'..'9'] then begin
      if dots = 0 then
        v1 := v1 + fullversion[i]
      else if dots = 1 then
        v2 := v2 + fullversion[i]
      else if dots = 2 then
        v3 := v3 + fullversion[i];
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
    FOnLog(Msg, Category);
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


function TMySQLConnection.GetLastQueryDuration: Cardinal;
begin
  Result := FLastQueryEnd - FLastQueryStart;
end;


function TMySQLConnection.GetCol(SQL: WideString; Column: Integer=0): TWideStringList;
var
  Results: TMySQLQuery;
begin
  try
    Results := GetResults(SQL);
    Result := TWideStringList.Create;
    if Results.RecordCount > 0 then while not Results.Eof do begin
      Result.Add(Results.Col(Column));
      Results.Next;
    end;
  finally
    FreeAndNil(Results);
  end;
end;


{**
  Get single cell value via SQL query, identified by column number
}
function TMySQLConnection.GetVar(SQL: WideString; Column: Integer=0): WideString;
var
  Results: TMySQLQuery;
begin
  try
    Results := GetResults(SQL);
    if Results.RecordCount > 0 then
      Result := Results.Col(Column)
    else
      Result := '';
  finally
    FreeAndNil(Results);
  end;
end;


{**
  Get single cell value via SQL query, identified by column name
}
function TMySQLConnection.GetVar(SQL: WideString; Column: WideString): WideString;
var
  Results: TMySQLQuery;
begin
  try
    Results := GetResults(SQL);
    if Results.RecordCount > 0 then
      Result := Results.Col(Column)
    else
      Result := '';
  finally
    FreeAndNil(Results);
  end;
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
  inherited Destroy;
  FreeAndNil(FColumnNames);
  mysql_free_result(FLastResult);
end;


procedure TMySQLQuery.SetSQL(Value: WideString);
begin
  FSQL := Value;
end;


procedure TMySQLQuery.Execute;
var
  i: Integer;
  Field: PMYSQL_FIELD;
begin
  FLastResult := Connection.Query(FSQL, True);
  FRecordCount := Connection.RowsFound;
  if HasResult then begin
    for i:=0 to mysql_num_fields(FLastResult)-1 do begin
      Field := mysql_fetch_field_direct(FLastResult, i);
      FColumnNames.Add(Utf8Decode(Field.name));
    end;
    RecNo := 0;
  end;
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
    FRecNo := Value;
    FEof := False;
    mysql_data_seek(FLastResult, FRecNo);
    FCurrentRow := mysql_fetch_row(FLastResult);
  end;
end;


function TMySQLQuery.ColumnCount: Integer;
begin
  Result := ColumnNames.Count;
end;


function TMySQLQuery.Col(Column: Integer; IgnoreErrors: Boolean=False): WideString;
begin
  if (Column > -1) and (Column < ColumnCount) then
    Result := UTF8StringToWideString(FCurrentRow[Column])
  else if not IgnoreErrors then
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
var
  i: Integer;
  Field: PMYSQL_FIELD;
begin
  Field := mysql_fetch_field_direct(FLastResult, Column);
  Result := Datatypes[Low(Datatypes)];
  for i:=Low(Datatypes) to High(Datatypes) do begin
    if Field._type = Datatypes[i].NativeType then begin
      Result := Datatypes[i];
      break;
    end;
  end;
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
