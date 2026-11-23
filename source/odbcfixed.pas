unit odbcfixed;

// SQLDB TODBCConnection with fixes for long texts, text encoding and exact decimals.
// See extra/odbc.md for the problems in odbcconn as shipped with FPC 3.2.2.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, BufDataset, FmtBCD, odbcconn, odbcsqldyn;

type

  // For units which do not use odbcsqldyn
  TOdbcStmt = SQLHSTMT;

  { TFixedODBCConnection }

  TFixedODBCConnection = class(TODBCConnection)
  private
    FFloat38AsDecimal: Boolean;
  protected
    procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs); override;
    function LoadField(cursor: TSQLCursor; FieldDef: TFieldDef; buffer: pointer;
      out CreateBlob: boolean): boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField;
      cursor: TSQLCursor; ATransaction: TSQLTransaction); override;
  public
    constructor Create(AOwner: TComponent); override;
    // Oracle: unconstrained NUMBER and FLOAT(n) are reported as SQL_FLOAT with 38 digits
    property Float38AsDecimal: Boolean read FFloat38AsDecimal write FFloat38AsDecimal;
  end;

const
  OdbcNullStmt = SQL_NULL_HSTMT;

// Number of result columns of a prepared, not yet executed statement. -1 if the driver cannot tell.
function OdbcPreparedColumnCount(Cursor: TSQLCursor): Integer;
// Statement handle of a prepared cursor, SQL_NULL_HSTMT if none
function OdbcCursorStmt(Cursor: TSQLCursor): SQLHSTMT;
// Execute a statement without result set via SQLExecDirect, without preparing it before.
// Oracle's driver takes :NEW in trigger bodies for a bind variable on SQLPrepare, and fails.
// Returns the number of affected rows, or -1. OnStmt gets the statement handle while it is
// executed, and SQL_NULL_HSTMT before it is freed.
type
  TOdbcStmtEvent = procedure(Stmt: SQLHSTMT) of object;
function OdbcExecDirect(Conn: TODBCConnection; const SQL: String; OnStmt: TOdbcStmtEvent=nil): Int64;
// Readable message from an EODBCException: only the driver messages, without the
// connection string, which contains the password
function OdbcErrorText(const Msg: String): String;
// Names of installed ODBC drivers, filtered by a regular expression
function GetOdbcDriverNames(const FilterRegex: String): TStringList;
// Let the Oracle client talk UTF-8, if the user has not configured NLS_LANG
function PrepareOracleClientEnvironment: String;
// Load the ODBC driver manager before odbcconn does it. On Linux and macOS, FPC loads the
// unversioned libodbc.so / libodbc.dylib, which only exists with the unixODBC -dev package.
procedure LoadOdbcDriverManager;
// True if LoadOdbcDriverManager succeeds, without raising
function OdbcDriverManagerAvailable: Boolean;
// SQLCancel on a statement running in another thread. Not declared by FPC's odbcsql.inc.
function OdbcCancel(Stmt: SQLHSTMT): Boolean;

implementation

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF} RegExpr, dynlibs, TypInfo;

{$IFNDEF WINDOWS}
function libc_setenv(name, value: PChar; overwrite: LongInt): LongInt; cdecl; external 'c' name 'setenv';
{$ENDIF}

type
  // gives access to protected TODBCCursor.FSTMTHandle
  TODBCCursorAccess = class(TODBCCursor);
  // gives access to protected TODBCEnvironment.FENVHandle
  TODBCEnvironmentAccess = class(TODBCEnvironment);

var
  ODBCFormatSettings: TFormatSettings;
  OdbcDriverManagerLoaded: Boolean = False;


constructor TFixedODBCConnection.Create(AOwner: TComponent);
var
  Opt: TConnOption;
  OptName: String;
begin
  inherited Create(AOwner);
  // FPC 3.2.4 sets sqCommitEndsPrepared and sqRollbackEndsPrepared for all connections, so
  // a commit unprepares all prepared queries of the transaction. On a query which is still
  // open, e.g. the data grid's result, UnPrepare raises "Operation cannot be performed on an
  // active dataset". Results are fully fetched (PacketRecords = -1) and ODBC statements
  // survive a commit. By name, as FPC 3.2.2 does not know these options.
  for Opt:=Low(TConnOption) to High(TConnOption) do begin
    OptName := GetEnumName(TypeInfo(TConnOption), Ord(Opt));
    if SameText(OptName, 'sqCommitEndsPrepared') or SameText(OptName, 'sqRollbackEndsPrepared') then
      Exclude(FConnOptions, Opt);
  end;
end;


procedure TFixedODBCConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);
var
  Stmt: SQLHSTMT;
  i: Integer;
  NameLen, DataType, Digits, Nullable: SQLSMALLINT;
  ColSize: SQLULEN;
begin
  inherited AddFieldDefs(cursor, FieldDefs);
  Stmt := TODBCCursorAccess(cursor).FSTMTHandle;
  NameLen := 0;
  DataType := 0;
  Digits := 0;
  Nullable := 0;
  ColSize := 0;
  for i:=0 to FieldDefs.Count-1 do begin
    // inherited truncates ColumnSize to a word - ask the driver again
    if not (SQLDescribeCol(Stmt, i+1, nil, 0, NameLen, DataType, ColSize, Digits, Nullable)
      in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO]) then
      Continue;
    case DataType of
      SQL_CHAR, SQL_VARCHAR, SQL_WCHAR, SQL_WVARCHAR: begin
        if (ColSize = 0) or (ColSize > MaxSmallint) then begin
          FieldDefs[i].DataType := ftWideMemo;
          FieldDefs[i].Size := 0;
        end
        else if DataType = SQL_VARCHAR then
          // fetch via SQL_C_WCHAR, not in the ANSI/client codepage
          FieldDefs[i].DataType := ftWideString
        else if DataType = SQL_CHAR then
          FieldDefs[i].DataType := ftFixedWideChar;
      end;
      SQL_LONGVARCHAR, SQL_WLONGVARCHAR: begin
        // ftMemo would be fetched as raw bytes in server codepage
        FieldDefs[i].DataType := ftWideMemo;
        FieldDefs[i].Size := 0;
      end;
      SQL_BINARY, SQL_VARBINARY: begin
        if (ColSize = 0) or (ColSize > MaxSmallint) then begin
          FieldDefs[i].DataType := ftBlob;
          FieldDefs[i].Size := 0;
        end;
      end;
      SQL_FLOAT: begin
        if FFloat38AsDecimal and (ColSize = 38) then begin
          FieldDefs[i].DataType := ftFmtBCD;
          FieldDefs[i].Precision := 38;
          FieldDefs[i].Size := 0; // scale is variable
        end;
      end;
      SQL_DECIMAL, SQL_NUMERIC: begin
        if ColSize <= MaxFmtBCDFractionSize then begin
          // exact decimal, instead of ftFloat/ftCurrency
          FieldDefs[i].DataType := ftFmtBCD;
          FieldDefs[i].Precision := ColSize;
          FieldDefs[i].Size := Digits; // scale
        end else begin
          // more digits than TBCD holds: keep the exact text
          FieldDefs[i].DataType := ftWideString;
          FieldDefs[i].Size := ColSize + 2; // sign and decimal point
        end;
      end;
    end;
  end;
end;


function TFixedODBCConnection.LoadField(cursor: TSQLCursor; FieldDef: TFieldDef;
  buffer: pointer; out CreateBlob: boolean): boolean;
type
  TWTxtBuffer = array[0..MaxFmtBCDFractionSize+3] of WideChar;
var
  Res: SQLRETURN;
  StrLenOrInd: SQLLEN;
  WTxt: TWTxtBuffer;
  Bcd: TBCD;
begin
  if FieldDef.DataType <> ftFmtBCD then
    Exit(inherited LoadField(cursor, FieldDef, buffer, CreateBlob));
  // Fetch decimals as text, the driver sends "." and no thousand separators.
  // Oracle needs NUM=US in the connection string for that.
  CreateBlob := False;
  StrLenOrInd := 0;
  WTxt := Default(TWTxtBuffer);
  Res := SQLGetData(TODBCCursorAccess(cursor).FSTMTHandle, FieldDef.Index+1, SQL_C_WCHAR,
    @WTxt[0], SizeOf(WTxt), @StrLenOrInd);
  if not (Res in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO]) then
    raise EODBCException.CreateFmt('Could not load decimal data for field "%s"', [FieldDef.Name]);
  Result := StrLenOrInd <> SQL_NULL_DATA;
  if Result then begin
    Bcd := StrToBCD(String(UnicodeString(PWideChar(@WTxt[0]))), ODBCFormatSettings);
    Move(Bcd, buffer^, SizeOf(TBCD));
  end;
end;


procedure TFixedODBCConnection.LoadBlobIntoBuffer(FieldDef: TFieldDef;
  ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction: TSQLTransaction);
const
  ChunkSize = 65536; // bytes, even number
var
  Stmt: SQLHSTMT;
  Res: SQLRETURN;
  StrLenOrInd: SQLLEN;
  Chunk: Pointer;
  Stream: TMemoryStream;
  BytesRead: SQLLEN;
begin
  if FieldDef.DataType <> ftWideMemo then begin
    inherited;
    Exit;
  end;
  // Fetch text as UTF-16 (SQL_C_WCHAR), so the driver converts from any server codepage.
  // Each chunk is terminated by a #0 widechar, which must not be copied.
  Stmt := TODBCCursorAccess(cursor).FSTMTHandle;
  Chunk := GetMem(ChunkSize);
  Stream := TMemoryStream.Create;
  try
    repeat
      StrLenOrInd := 0;
      Res := SQLGetData(Stmt, FieldDef.Index+1, SQL_C_WCHAR, Chunk, ChunkSize, @StrLenOrInd);
      if Res = SQL_NO_DATA then
        Break;
      if not (Res in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO]) then
        raise EODBCException.CreateFmt('Could not load text data for field "%s"', [FieldDef.Name]);
      if StrLenOrInd = SQL_NULL_DATA then
        Break;
      if (StrLenOrInd = SQL_NO_TOTAL) or (StrLenOrInd > ChunkSize - SizeOf(WideChar)) then
        BytesRead := ChunkSize - SizeOf(WideChar)
      else
        BytesRead := StrLenOrInd;
      Stream.Write(Chunk^, BytesRead);
    until Res = SQL_SUCCESS;
    ABlobBuf^.BlobBuffer^.Size := Stream.Size;
    ReAllocMem(ABlobBuf^.BlobBuffer^.Buffer, Stream.Size);
    if Stream.Size > 0 then
      Move(Stream.Memory^, ABlobBuf^.BlobBuffer^.Buffer^, Stream.Size);
  finally
    Stream.Free;
    FreeMem(Chunk);
  end;
end;


function OdbcCursorStmt(Cursor: TSQLCursor): SQLHSTMT;
begin
  if Cursor is TODBCCursor then
    Result := TODBCCursorAccess(Cursor).FSTMTHandle
  else
    Result := SQL_NULL_HSTMT;
end;


function OdbcPreparedColumnCount(Cursor: TSQLCursor): Integer;
var
  ColCount: SQLSMALLINT;
begin
  Result := -1;
  if not (Cursor is TODBCCursor) then
    Exit;
  ColCount := 0;
  if SQLNumResultCols(TODBCCursorAccess(Cursor).FSTMTHandle, ColCount) in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO] then
    Result := ColCount;
end;


// Same as ODBCCheckResult in odbcconn, which is not public
procedure CheckOdbcResult(Res: SQLRETURN; HandleType: SQLSMALLINT; AHandle: SQLHANDLE; const ErrorMsg: String);
type
  TMsgTextBuffer = array[0..1023] of AnsiChar;
var
  NativeError, FirstNativeError: SQLINTEGER;
  TextLength: SQLSMALLINT;
  SqlState, FirstSqlState: String;
  MsgText: TMsgTextBuffer;
  TotalMessage: String;
  RecNumber: SQLSMALLINT;
begin
  if Res in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO] then
    Exit;
  TotalMessage := ErrorMsg + ' ODBC error details:';
  FirstNativeError := 0;
  FirstSqlState := '';
  RecNumber := 1;
  while True do begin
    SqlState := '';
    SetLength(SqlState, 5);
    MsgText := Default(TMsgTextBuffer);
    NativeError := 0;
    TextLength := 0;
    if not (SQLGetDiagRec(HandleType, AHandle, RecNumber, @SqlState[1], NativeError,
      @MsgText[0], SizeOf(MsgText)-1, TextLength) in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO]) then
      Break;
    TotalMessage := TotalMessage + Format(' Record %d: SqlState: %s; NativeError: %d; Message: %s;',
      [RecNumber, SqlState, NativeError, String(PAnsiChar(@MsgText[0]))]);
    if RecNumber = 1 then begin
      FirstNativeError := NativeError;
      FirstSqlState := SqlState;
    end;
    Inc(RecNumber);
  end;
  raise EODBCException.CreateFmt(TotalMessage, [], nil, FirstNativeError, FirstSqlState);
end;


function OdbcExecDirect(Conn: TODBCConnection; const SQL: String; OnStmt: TOdbcStmtEvent=nil): Int64;
var
  Dbc: SQLHDBC;
  Stmt: SQLHSTMT;
  Res: SQLRETURN;
  WSQL: UnicodeString;
  RowCount: SQLLEN;
begin
  Result := -1;
  RowCount := 0;
  Dbc := SQLHDBC(Conn.Handle);
  Stmt := SQL_NULL_HSTMT;
  CheckOdbcResult(SQLAllocHandle(SQL_HANDLE_STMT, Dbc, Stmt),
    SQL_HANDLE_DBC, Dbc, 'Could not allocate ODBC Statement handle.');
  try
    if Assigned(OnStmt) then
      OnStmt(Stmt);
    WSQL := UnicodeString(SQL);
    Res := SQLExecDirectW(Stmt, PWideChar(WSQL), Length(WSQL));
    // SQL_NO_DATA: searched UPDATE or DELETE without affected rows
    if Res <> SQL_NO_DATA then
      CheckOdbcResult(Res, SQL_HANDLE_STMT, Stmt, 'Could not execute statement.');
    if Res = SQL_NO_DATA then
      Result := 0
    else if SQLRowCount(Stmt, RowCount) in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO] then
      Result := RowCount;
  finally
    if Assigned(OnStmt) then
      OnStmt(SQL_NULL_HSTMT);
    SQLFreeHandle(SQL_HANDLE_STMT, Stmt);
  end;
end;


function OdbcErrorText(const Msg: String): String;
const
  MsgMarker = ' Message: ';
  RecordMarker = '; Record ';
var
  Rest, Part: String;
  p: Integer;
  rx: TRegExpr;
begin
  // "Could not execute statement. ODBC error details: LastReturnCode: SQL_ERROR; Record 1: SqlState: 42S02;
  //  NativeError: 942; Message: [Oracle][ODBC][Ora]ORA-00942: table or view does not exist;"
  Result := '';
  rx := TRegExpr.Create;
  try
    // Strip vendor prefixes like [Oracle][ODBC][Ora]
    rx.Expression := '^(\[[^\]]*\])+\s*';
    Rest := Msg;
    p := Pos(MsgMarker, Rest);
    while p > 0 do begin
      Rest := Copy(Rest, p + Length(MsgMarker), MaxInt);
      p := Pos(RecordMarker, Rest);
      if p > 0 then
        Part := Copy(Rest, 1, p-1)
      else
        Part := Rest;
      Part := Trim(Part);
      if (Part <> '') and (Part[Length(Part)] = ';') then
        Delete(Part, Length(Part), 1);
      Part := Trim(rx.Replace(Part, '', False));
      // Snowflake's driver appends a multi-line "error trace:" with locations in its Rust sources
      if Pos('error trace:', LowerCase(Part)) > 0 then
        Part := Trim(Copy(Part, 1, Pos('error trace:', LowerCase(Part))-1));
      if Part <> '' then begin
        if Result <> '' then
          Result := Result + sLineBreak;
        Result := Result + Part;
      end;
      if p = 0 then
        Break;
      Rest := Copy(Rest, p, MaxInt);
      p := Pos(MsgMarker, Rest);
    end;
    if Result = '' then begin
      // No driver message. Remove the connection string anyway.
      rx.Expression := 'connection string\s+"[^"]*"';
      Result := rx.Replace(Msg, 'connection string', False);
    end;
  finally
    rx.Free;
  end;
end;


function GetOdbcDriverNames(const FilterRegex: String): TStringList;
type
  TDescBuffer = array[0..511] of AnsiChar;
  TAttrsBuffer = array[0..2047] of AnsiChar;
var
  Env: TODBCEnvironment;
  Res: SQLRETURN;
  Direction: SQLUSMALLINT;
  Desc: TDescBuffer;
  Attrs: TAttrsBuffer;
  DescLen, AttrLen: SQLSMALLINT;
  DriverName: String;
  rx: TRegExpr;
begin
  Result := TStringList.Create;
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.Expression := FilterRegex;
  Env := nil;
  try
    try
      LoadOdbcDriverManager;
      Env := TODBCEnvironment.Create;
      Direction := SQL_FETCH_FIRST;
      while True do begin
        Desc := Default(TDescBuffer);
        Attrs := Default(TAttrsBuffer);
        Res := SQLDrivers(TODBCEnvironmentAccess(Env).FENVHandle, Direction,
          @Desc[0], SizeOf(Desc), @DescLen, @Attrs[0], SizeOf(Attrs), @AttrLen);
        if not (Res in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO]) then
          Break;
        DriverName := String(PAnsiChar(@Desc[0]));
        if (FilterRegex = '') or rx.Exec(DriverName) then
          Result.Add(DriverName);
        Direction := SQL_FETCH_NEXT;
      end;
    except
      // No driver manager installed. Leave the list empty.
      on E:Exception do;
    end;
  finally
    Env.Free;
    rx.Free;
  end;
end;


function PrepareOracleClientEnvironment: String;
begin
  // Without NLS_LANG, the Oracle client converts all text to an ASCII charset,
  // already in the SQL sent to the server. Must be set before oci.dll is loaded.
  // Territory AMERICA (the default for ".AL32UTF8") also gives "." as decimal separator.
  Result := SysUtils.GetEnvironmentVariable('NLS_LANG');
  if Result = '' then begin
    Result := '.AL32UTF8';
    {$IFDEF WINDOWS}
    Windows.SetEnvironmentVariable('NLS_LANG', PChar(Result));
    {$ELSE}
    // The Oracle client reads it via getenv() from libc
    libc_setenv('NLS_LANG', PChar(Result), 1);
    {$ENDIF}
  end;
end;


procedure LoadOdbcDriverManager;
{$IFNDEF WINDOWS}
const
  // Runtime names of unixODBC: libodbc2 (Debian 12+, Ubuntu 23.04+), libodbc1 (older), Homebrew
  LibNames: array[0..3] of String = ('libodbc.so.2', 'libodbc.so.1', 'libodbc.2.dylib', '');
var
  LibName: String;
  Tried: String;
{$ENDIF}
begin
  if OdbcDriverManagerLoaded then
    Exit;
  {$IFDEF WINDOWS}
  // odbc32.dll is part of Windows, odbcconn loads it
  {$ELSE}
  Tried := '';
  for LibName in LibNames do begin
    try
      // '' = FPC's default libodbc.so / libodbc.dylib
      InitialiseODBC(LibName);
      OdbcDriverManagerLoaded := True;
      Break;
    except
      on E:EInOutError do begin
        if LibName <> '' then
          Tried := Tried + LibName + ', ';
      end;
    end;
  end;
  if not OdbcDriverManagerLoaded then
    raise EInOutError.Create('Can not load the ODBC driver manager. Please install unixODBC (tried '
      + Tried + 'libodbc.' + SharedSuffix + ').');
  {$ENDIF}
end;


function OdbcDriverManagerAvailable: Boolean;
begin
  try
    LoadOdbcDriverManager;
    Result := True;
  except
    on E:EInOutError do
      Result := False;
  end;
end;


type
  TSQLCancel = function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

function OdbcCancel(Stmt: SQLHSTMT): Boolean;
var
  CancelFunc: TSQLCancel;
begin
  Result := False;
  if ODBCLibraryHandle = NilHandle then
    Exit;
  CancelFunc := TSQLCancel(GetProcedureAddress(ODBCLibraryHandle, 'SQLCancel'));
  if Assigned(CancelFunc) then
    Result := CancelFunc(Stmt) in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO];
end;


initialization
  ODBCFormatSettings := DefaultFormatSettings;
  ODBCFormatSettings.DecimalSeparator := '.';
  ODBCFormatSettings.ThousandSeparator := #0;

end.
