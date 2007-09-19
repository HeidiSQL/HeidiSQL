unit mysqldataset;

{$M+} // Needed to add published properties

interface

uses
  Classes, db, SysUtils, windows, mysql_api;

type

  {TMySQLConnection}

  TMySQLLogCategory = (lcStats, lcSQL, lcError, lcInternal);
  TMySQLLogEvent = procedure (Category: TMySQLLogCategory; Msg: String) of object;

  TMySQLCapability = (
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
    cpBackticks,            // `identifier`
    cpAlterDatabase,        // ALTER DATABASE
    cpRenameDatabase        // RENAME DATABASE     
    );
  TMySQLCapabilities = set of TMySQLCapability;

  TMySQLConnectionOption = (
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
  TMySQLConnectionOptions = set of TMySQLConnectionOption;

const
  DEFAULT_MYSQLOPTIONS = [opLocalFiles, opInteractive, opProtocol41];

type
  TMySQLConnection = class(TComponent)
    private
      FHandle: PMYSQL;
      FActive: Boolean;
      FHostname: String;
      FPort: Cardinal;
      FUsername: String;
      FPassword: String;
      FTimeout: Cardinal;
      FDatabase: String;
      FOnLog: TMySQLLogEvent;
      FOptions: TMySQLConnectionOptions;
      FCapabilities: TMySQLCapabilities;
      procedure SetActive( Value: Boolean );
      procedure SetDatabase( Value: String );
      function GetThreadId: Cardinal;
      function GetCharacterSet: String;
      function GetLastError: String;
      function GetServerVersionStr: String;
      function GetServerVersionInt: Integer;
      procedure Log(Category: TMySQLLogCategory; Msg: String);
      procedure DetectCapabilities;
    public
      constructor Create(AOwner: TComponent); override;
      property ThreadId: Cardinal read GetThreadId;
      property Handle: PMYSQL read FHandle;
      property CharacterSet: String read GetCharacterSet;
      function Query( SQL: String ): Longint;
      property LastError: String read GetLastError;
      property ServerVersionStr: String read GetServerVersionStr;
      property ServerVersionInt: Integer read GetServerVersionInt;
      function EscapeString( Text: String; DoQuote: Boolean = True ): String;
      property Capabilities: TMySQLCapabilities read FCapabilities;
      function QuoteIdent( Identifier: String ): String;

    published
      property Active: Boolean read FActive write SetActive default False;
      property Hostname: String read FHostname write FHostname;
      property Port: Cardinal read FPort write FPort default MYSQL_PORT;
      property Username: String read FUsername write FUsername;
      property Password: String read FPassword write FPassword;
      property Timeout: Cardinal read FTimeout write FTimeout default NET_READ_TIMEOUT;
      property Database: String read FDatabase write SetDatabase;
      property Options: TMySQLConnectionOptions read FOptions write FOptions default DEFAULT_MYSQLOPTIONS;

      // Events
      property OnLog: TMySQLLogEvent read FOnLog write FOnLog;
  end;


  {TMySQLQuery}

  TMySQLQuery = class(TDataSet)
    private
      FSQL: TStrings;
      FConnection: TMySQLConnection;
      FRowsAffected: Int64;
      FLastResult: PMYSQL_RES;
      FRecNo: Int64;
      FCurrentRow: PMYSQL_ROW;
      procedure SetQuery(Value: TStrings);
    protected
      function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
      procedure SetFieldData(Field: TField; Buffer: Pointer); override;
      function GetRecordSize: Word; override;
      function GetCanModify: Boolean; override;
      procedure InternalOpen; override;
      procedure InternalClose; override;
      procedure InternalInitFieldDefs; override;
      procedure InternalHandleException; override;
      procedure InternalInitRecord(Buffer: PChar); override;
      function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
      procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
      procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
      procedure InternalSetToRecord(Buffer: PChar); override;
      function IsCursorOpen: Boolean; override;
      procedure InternalFirst; override;
      procedure InternalLast; override;
      procedure InternalEdit; override;
      procedure InternalInsert; override;
      procedure InternalPost; override;
      procedure InternalDelete; override;
      function GetRecNo: Integer; override;
      function GetRecordCount: Integer; override;
      procedure SetRecNo(Value: Integer); override;

    public
      constructor Create(AOwner: TComponent); override;
      procedure ExecSQL;
      property RowsAffected: Int64 read FRowsAffected;
      function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    published
      property SQL: TStrings read FSQL write SetQuery;
      property Connection: TMySQLConnection read FConnection write FConnection;

      property AutoCalcFields;
      property BeforeOpen;
      property AfterOpen;
      property BeforeClose;
      property AfterClose;
      property BeforeRefresh;
      property AfterRefresh;
      property BeforeScroll;
      property AfterScroll;
      property OnCalcFields;
      property OnFilterRecord;
      property Filter;
      property Filtered;
  end;

  procedure Register;

  // Should be removed when this baby is running
  procedure debug(txt: String);


implementation


procedure debug(txt: String);
begin
  txt := 'mds '+txt;
  OutputDebugString(PChar(txt));
end;

procedure Register;
begin
  RegisterComponents('MySQL Dataset', [TMySQLConnection, TMySQLQuery]);
end;



{TMySQLConnection}

constructor TMySQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := DEFAULT_MYSQLOPTIONS;
  FTimeout := NET_READ_TIMEOUT;
  FPort := MYSQL_PORT;
end;


{**
  (Dis-)Connect to/from server
  Parts copied from MySQL-Front or revision 1 of HeidiSQL's childwin.pas
}
procedure TMySQLConnection.SetActive( Value: Boolean );
var
  connected : PMYSQL;
  ClientFlags : Integer;
  error : String;
begin
  FActive := Value;

  if Value and (FHandle = nil) then
  begin
    // Get handle
    FHandle := mysql_init(nil);
    // timeout
    mysql_options(FHandle, MYSQL_OPT_CONNECT_TIMEOUT, @FTimeout);

    // read ini-file
    // mysql_options(FHandle, MYSQL_READ_DEFAULT_FILE, pchar(ExtractFilePath(paramstr(0)) + 'my.ini'));
    // read [Client]-section from ini-file
    mysql_options(FHandle, MYSQL_READ_DEFAULT_GROUP, pchar('Client'));

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

    // Connect
    connected := mysql_real_connect(FHandle,
      pChar(FHostname),
      pChar(FUsername),
      pChar(FPassword),
      nil,
      FPort,
      nil,
      ClientFlags
      );
    if connected = nil then
    begin
      error := LastError;
      Log( lcError, error );
      FActive := False;
      FHandle := nil;
      raise Exception.Create( error );
    end
    else begin
      DetectCapabilities;
      Log( lcStats, 'Connection established with host "'+Hostname+'" on port '+IntToStr(Port)+' as user "'+Username+'"' );
      Log( lcStats, 'Connection-ID: '+IntToStr(ThreadId) );
      Log( lcStats, 'Characterset: '+CharacterSet );
      Log( lcStats, 'Server version: '+ServerVersionStr+' ('+IntToStr(ServerVersionInt)+')' );
      SetDatabase( FDatabase );
    end;
  end

  else if (not Value) and (FHandle <> nil) then
  begin
    mysql_close(FHandle);
    FHandle := nil;
    FCapabilities := [];
    Log( lcStats, 'Connection closed' );
  end;

end;


{**
   Executes a query
}
function TMySQLConnection.Query(SQL: string): Longint;
begin
  if Not FActive then
    SetActive( True );
  Log( lcSQL, Trim(Copy(SQL, 1, 1024)) );
  Result := mysql_real_query(FHandle, pChar(SQL), length(SQL));
  if Result <> 0 then
  begin
    Log( lcError, GetLastError );
    raise Exception.Create(GetLastError);
  end
  else
  begin
    Log( lcStats, IntToStr(mysql_affected_rows( FHandle ))+' rows affected' );
  end;
end;


{**
  Set "Database" property and select that db if connected
}
procedure TMySQLConnection.SetDatabase( Value: String );
var
  res : Integer;
  oldValue : String;
begin
  if Value = '' then
    Exit;

  oldValue := FDatabase;
  FDatabase := Value;

  // Switch to DB if connected.
  // If not connected, SetDatabase() should be called by SetActive()
  if FActive then
  begin
    res := Query( 'USE '+QuoteIdent(Value) );
    if res = 0 then
      Log( lcStats, 'Database "'+Value+'" selected' )
    else begin
      FDatabase := oldValue;
      raise Exception.Create(GetLastError);
    end;
  end;
end;


{**
  Return current thread id
}
function TMySQLConnection.GetThreadId: Cardinal;
begin
  Result := mysql_thread_id( FHandle );
end;


{**
  Return currently used character set
}
function TMySQLConnection.GetCharacterSet: String;
begin
  Result := mysql_character_set_name( FHandle );
end;


{**
  Return the last error nicely formatted
}
function TMySQLConnection.GetLastError: String;
begin
  Result := Format('SQL Error (%d): %s', [mysql_errno(FHandle), mysql_error(FHandle)] );
end;


{**
  Return the untouched server version string
}
function TMySQLConnection.GetServerVersionStr: String;
begin
  Result := mysql_get_server_info(FHandle);
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
  // Avoid calling GetServerVersionStr too often
  fullversion := ServerVersionStr;
  v1 := '';
  v2 := '';
  v3 := '';
  for i := 1 to Length(fullversion) do
  begin
    if fullversion[i] = '.' then
    begin
      inc(dots);
      // We expect exactly 2 dots.
      if dots > 2 then
        break;
    end
    else if fullversion[i] in ['0'..'9'] then
    begin
      if dots = 0 then
        v1 := v1 + fullversion[i]
      else if dots = 1 then
        v2 := v2 + fullversion[i]
      else if dots = 2 then
        v3 := v3 + fullversion[i];
    end
    else // Don't include potential numbers of trailing string
      break;
  end;

  // Concat tokens
  if (Length(v1)>0) and (Length(v2)>0) and (Length(v3)>0) then
  begin
    Result := StrToIntDef(v1, 0) *10000 +
      StrToIntDef(v2, 0) *100 +
      StrToIntDef(v3, 0);
  end;

end;


{**
  Call log event if assigned to object
}
procedure TMySQLConnection.Log(Category: TMySQLLogCategory; Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog( Category, Msg);
end;


{**
  Escapes a string for usage in SQL queries
}
function TMySQLConnection.EscapeString( Text: String; DoQuote: Boolean ): String;
var
  BufferLen: Integer;
  Buffer: PChar;
begin
  BufferLen := Length(Text) * 2 + 1;
  GetMem(Buffer, BufferLen);
  BufferLen := mysql_real_escape_string(FHandle, Buffer, PChar(Text), Length(Text));
  SetString(Result, Buffer, BufferLen);
  FreeMem(Buffer);

  if DoQuote then
    Result := '''' + Result + '''';
end;


{**
  Add backticks to identifier
  Todo: Support ANSI style
}
function TMySQLConnection.QuoteIdent( Identifier: String ): String;
begin
  if cpBackticks in Capabilities then
  begin
    Result := StringReplace(Identifier, '`', '``', [rfReplaceAll]);
    Result := '`' + Result + '`';
  end
  else
    Result := Identifier;
end;


{**
  Detect various capabilities of the server
  for easy feature-checks in client-applications.
}
procedure TMySQLConnection.DetectCapabilities;
var
  ver : Integer;
  procedure addCap( c: TMySQLCapability; addit: Boolean );
  begin
    if addit then
      Include( FCapabilities, c )
    else
      Exclude( FCapabilities, c );
  end;
begin
  // Avoid calling GetServerVersionInt too often
  ver := ServerVersionInt;

  addCap( cpShowEngines, ver >= 40102 );
  addCap( cpShowTableStatus, ver >= 32300 );
  addCap( cpShowFullTables, ver >= 50002 );
  addCap( cpShowCreateTable, ver >= 32320 );
  addCap( cpShowCreateDatabase, ver >= 50002 );
  addCap( cpHelpSystem, ver >= 40100 );
  addCap( cpSetNames, ver >= 40100 );
  addCap( cpCalcFoundRows, ver >= 40000 );
  addCap( cpLoadFile, ver >= 32206 );
  addCap( cpTableComment, ver >= 32300 );
  addCap( cpFieldComment, ver >= 40100 );
  addCap( cpColumnMoving, ver >= 40001 );
  addCap( cpTruncateTable, ver >= 50003 );
  addCap( cpBackticks, ver >= 32300 );
  addCap( cpAlterDatabase, ver >= 50002 );
  addCap( cpRenameDatabase, ver >= 50107 );

end;



{ TMySQLQuery }

constructor TMySQLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FRowsAffected := -1;
  FRecNo := -1;
end;


{**
  Executes a query without handling the resultset
}
procedure TMySQLQuery.ExecSQL;
begin
  FConnection.Query( FSQL.Text );
  FRowsAffected := mysql_affected_rows( FConnection.Handle );
end;


{**
  Set SQL TStringList
}
procedure TMySQLQuery.SetQuery(Value: TStrings);
begin
  if FSQL.Text <> Value.Text then
  begin
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;


{**
  The most important method for a TDataset:
  Navigate to and fetch the current, next or prior row
}
function TMySQLQuery.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := grOK;

  case GetMode of
    gmCurrent:
      begin
        if (RecNo < 0) or (RecNo >= RecordCount) then
          Result := grError
        else
          Result := grOK;
      end;

    gmNext:
      if RecNo >= RecordCount then
        Result := grEOF
      else
      begin
        RecNo := RecNo + 1;
        Result := grOK;
      end;

    gmPrior:
      if RecNo <= 0 then
        Result := grBOF
      else
      begin
        RecNo := RecNo - 1;
        Result := grOK;
      end;
  end;

  if Result = grOK then
  begin
    FCurrentRow := mysql_fetch_row( FLastResult );
    System.Move(
      FCurrentRow,
      Buffer,
      SizeOf(FCurrentRow)
    );
  end;

end;

function TMySQLQuery.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  {
  The Field parameter is the field for which the value needs to be retrieved.
  The Field parameter is only passed for reference and should never be altered by this routine.

  The Buffer parameter is where the field value needs to be copied to.
  Looking at the buffer parameter results in a question that doesn't have an
  obvious answer at first glance. That question is "What size is that buffer
  and what needs to be copied into it?". The only way of determining this is
  by looking at the various TField types in DB.pas and examining their GetValue
  and GetDataSize methods.

  Here is a partial table with some values used in the base dataset we will create later on:
  Field Type Buffer Result
  ftInteger,ftDate,ftTime Integer
  ftBoolean Boolean
  ftDateTime TDateTimeRec
  ftFloat,ftCurrency Double
  ftString PChar

  As we can see, most types map pretty cleanly with the noteable exception of TDateTime
  which requires some translation into a TDateTimeRec.

  GetFieldData function returns True if a value was copied into the buffer by the
  method and False if no value was copied.

  That covers the GetFieldData method.
  }
  FConnection.Log(lcInternal, 'GetFieldData called for RecNo '+inttostr(RecNo)+' field "'+Field.Name+'" ('+inttostr(field.FieldNo)+')');
  if Field.DataType = ftString then
  begin
    System.Move(
      FCurrentRow[Field.FieldNo-1]^,
      //ZEOS: RowAccessor.GetColumnData(ColumnIndex, Result)^,
      Buffer,
      //ZEOS: RowAccessor.GetColumnDataSize(ColumnIndex)
      SizeOf(FCurrentRow[Field.FieldNo-1])
      );
    Result := True;
  end
  else
    Result := False;
end;


{**
  Tell dataset the contents of a field
}
procedure TMySQLQuery.SetFieldData(Field: TField; Buffer: Pointer);
begin
  { SetFieldData is the exact reverse operation
  of GetFieldData. It is passed a buffer with some field value in the buffer that
  must then be copied back into your record buffer.
  }
  if Field.DataType = ftString then
  begin
    System.Move(
      Buffer^,
      FCurrentRow,
      SizeOf(FCurrentRow)
      );
  end;
end;


{**
  ??
}
function TMySQLQuery.GetRecordSize: Word;
begin
  Result := 1;
end;


{**
  Dataset is editable?
  Should be True for simple SELECTs and False for not parsable SELECTs 
}
function TMySQLQuery.GetCanModify: Boolean;
begin
  Result := True;
end;


{**
  Send query and fetch resultset
}
procedure TMySQLQuery.InternalOpen;
begin
  FConnection.Query( FSQL.Text );
  FLastResult := mysql_store_result( FConnection.Handle );
  FieldDefs.Clear;
  FieldDefs.Update; // Calls InternalInitFieldDefs

  if DefaultFields then CreateFields;
  BindFields(True);
end;


{**
  Close resultset
}
procedure TMySQLQuery.InternalClose;
begin
  mysql_free_result(FLastResult);
end;


{**
  Fetch field types of recent resultset
}
procedure TMySQLQuery.InternalInitFieldDefs;
var
  def: TFieldDef;
  i, numfields: Cardinal;
  field: PMYSQL_FIELD;
  fType: TFieldType;

  // Detect signed flag of a field
  function Signed: Boolean;
  begin
    Result := (UNSIGNED_FLAG and field.flags) = 0;
  end;

begin
  numfields := mysql_num_fields(FLastResult);

  for i := 0 to numfields-1 do
  begin
    field := mysql_fetch_field_direct(FLastResult, i);

    // Create a new field
    def := FieldDefs.AddFieldDef;
    def.FieldNo := i;
    def.Name := field.name;

    // Map field type to delphi-types
    // see TFieldType in DB.pas
    case field._type of
      FIELD_TYPE_TINY:
        fType := ftSmallint;

      FIELD_TYPE_YEAR, FIELD_TYPE_SHORT:
        fType := ftInteger;

      FIELD_TYPE_INT24, FIELD_TYPE_LONG:
        begin
          if Signed then
            fType := ftInteger
          else
            fType := ftFloat;
        end;

      FIELD_TYPE_LONGLONG:
        fType := ftString;

      FIELD_TYPE_FLOAT:
        fType := ftFloat;

      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        begin
          if (field.decimals = 0) and (field.length < 11) then
            fType := ftInteger
          else
            fType := ftFloat;
        end;

      FIELD_TYPE_DOUBLE:
        fType := ftFloat;

      FIELD_TYPE_DATE:
        fType := ftString;

      FIELD_TYPE_TIME:
        fType := ftString;

      FIELD_TYPE_DATETIME, FIELD_TYPE_TIMESTAMP:
        fType := ftString;

      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB,
      FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB:
        if (field.flags and BINARY_FLAG) = 0 then
          fType := ftMemo
        else
          fType := ftBlob;

      FIELD_TYPE_BIT:
        fType := ftBlob;

      FIELD_TYPE_VARCHAR:
        fType := ftString;

      FIELD_TYPE_VAR_STRING:
        fType := ftString;

      FIELD_TYPE_STRING:
        fType := ftString;

      FIELD_TYPE_ENUM:
        fType := ftString;

      FIELD_TYPE_SET:
        fType := ftString;

      FIELD_TYPE_NULL:
        // Example: SELECT NULL FROM DUAL
        // Todo: Find out if it is possible to get real data in a
        // TYPE_NULL field, perhaps adjust to binary or some such?
        fType := ftString;

      FIELD_TYPE_GEOMETRY:
        // Todo: Would be nice to show as WKT.
        fType := ftBlob;

      else
        raise Exception.Create('Unknown MySQL data type!'+IntToStr(field._type));
    end;

    def.DataType := fType;
    if fType in [ftString, ftWidestring, ftBytes] then
      def.Size := field.length
    else
      def.Size := 0;

    def.Required := (field.flags and NOT_NULL_FLAG) = NOT_NULL_FLAG;
    def.Precision := field.length;

  end;
end;


procedure TMySQLQuery.InternalHandleException;
begin
  // Application.HandleException(Self); ?
end;


{**
  ??
}
procedure TMySQLQuery.InternalInitRecord(Buffer: PChar);
begin
end;


{**
  ??
}
function TMySQLQuery.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
end;


{**
  ??
}
procedure TMySQLQuery.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
end;


{**
  ??
}
procedure TMySQLQuery.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
end;


{**
  ??
}
procedure TMySQLQuery.InternalSetToRecord(Buffer: PChar);
begin
end;


{**
  ??
}
function TMySQLQuery.IsCursorOpen: Boolean;
begin
  // Result := Handle <> nil; ?
end;


{**
  Called by DataSet.First
}
procedure TMySQLQuery.InternalFirst;
begin
  FRecNo := 0;
  mysql_data_seek( FLastResult, FRecNo );
end;


{**
  Called by DataSet.Last
}
procedure TMySQLQuery.InternalLast;
begin
  FRecNo := mysql_num_rows( FLastResult )-1;
  mysql_data_seek( FLastResult, FRecNo );
end;


{**
  ??
}
procedure TMySQLQuery.InternalEdit;
begin
end;


{**
  Fill default values?
}
procedure TMySQLQuery.InternalInsert;
begin
end;


{**
  Generate UPDATE or INSERT statement?
}
procedure TMySQLQuery.InternalPost;
begin
  inherited;
end;


{**
  Generate DELETE statement?
}
procedure TMySQLQuery.InternalDelete;
begin
end;


{**
  Called by DataSet.RecNo
}
function TMySQLQuery.GetRecNo: Integer;
begin
  Result := FRecNo;
end;


{**
  Called by DataSet.RecordCount
}
function TMySQLQuery.GetRecordCount: Integer;
begin
  Result := mysql_num_rows( FLastResult );
end;


{**
  Navigate to record
}
procedure TMySQLQuery.SetRecNo(Value: Integer);
begin
  if Value > RecordCount then
    Value := RecordCount-1;
  FRecNo := Value;
  mysql_data_seek( FLastResult, FRecNo );
end;



end.
