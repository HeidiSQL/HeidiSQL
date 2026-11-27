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
    FExecDirect: Boolean;
    FAutoCommit: Boolean;
    // Affected rows of the last statement executed via SQLExecDirect, see Execute
    FDirectCursor: TSQLCursor;
    FDirectRowsAffected: TRowsCount;
  protected
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
    procedure PrepareStatement(cursor: TSQLCursor; ATransaction: TSQLTransaction; buf: string;
      AParams: TParams); override;
    procedure Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams); override;
    function StartDBTransaction(trans: TSQLHandle; AParams: string): boolean; override;
    procedure AddFieldDefsForAllTypes(cursor: TSQLCursor; FieldDefs: TFieldDefs);
    procedure AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs); override;
    function LoadField(cursor: TSQLCursor; FieldDef: TFieldDef; buffer: pointer;
      out CreateBlob: boolean): boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField;
      cursor: TSQLCursor; ATransaction: TSQLTransaction); override;
  public
    constructor Create(AOwner: TComponent); override;
    // Oracle: unconstrained NUMBER and FLOAT(n) are reported as SQL_FLOAT with 38 digits
    property Float38AsDecimal: Boolean read FFloat38AsDecimal write FFloat38AsDecimal;
    // Execute statements without parameters via SQLExecDirect, not SQLPrepare + SQLExecute.
    // MS SQL runs prepared statements in their own scope, so a #temp table created there is
    // gone afterwards. SQLNumResultCols is unknown before execution then.
    property ExecDirect: Boolean read FExecDirect write FExecDirect;
    // SQL_AUTOCOMMIT_ON instead of a transaction. Same as odbcconn's "AUTOCOMMIT" parameter,
    // which also ends up in the connection string, where the driver reports it as invalid.
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;
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
// SQL_ATTR_QUERY_TIMEOUT on a statement before its execution. Expiry raises SQLSTATE HYT00.
function OdbcSetQueryTimeout(Stmt: SQLHSTMT; Seconds: Integer): Boolean;

implementation

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF} RegExpr, dynlibs, TypInfo, Math, LazUTF8;

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

procedure CheckOdbcResult(Res: SQLRETURN; HandleType: SQLSMALLINT; AHandle: SQLHANDLE; const ErrorMsg: String); forward;

type
  TSQLMoreResults = function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

// SQLMoreResults, not declared by FPC's odbcsql.inc
function OdbcMoreResults(Stmt: SQLHSTMT): SQLRETURN;
var
  MoreResultsFunc: TSQLMoreResults;
begin
  Result := SQL_NO_DATA;
  if ODBCLibraryHandle = NilHandle then
    Exit;
  MoreResultsFunc := TSQLMoreResults(GetProcedureAddress(ODBCLibraryHandle, 'SQLMoreResults'));
  if Assigned(MoreResultsFunc) then
    Result := MoreResultsFunc(Stmt);
end;


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


procedure TFixedODBCConnection.PrepareStatement(cursor: TSQLCursor; ATransaction: TSQLTransaction;
  buf: string; AParams: TParams);
var
  ODBCCursor: TODBCCursorAccess;
begin
  if (not FExecDirect) or (Assigned(AParams) and (AParams.Count > 0))
    or (TODBCCursorAccess(cursor).FSchemaType <> stNoSchema) then begin
    inherited;
    Exit;
  end;
  // Statement handle only, the SQL is passed to SQLExecDirect in Execute
  ODBCCursor := TODBCCursorAccess(cursor);
  CheckOdbcResult(SQLAllocHandle(SQL_HANDLE_STMT, SQLHDBC(Handle), ODBCCursor.FSTMTHandle),
    SQL_HANDLE_DBC, SQLHDBC(Handle), 'Could not allocate ODBC Statement handle.');
  ODBCCursor.FPrepared := True;
  ODBCCursor.FQuery := buf;
end;


procedure TFixedODBCConnection.Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
var
  ODBCCursor: TODBCCursorAccess;
  Res: SQLRETURN;
  ColumnCount: SQLSMALLINT;
  RowCount: SQLLEN;
  WSQL: UnicodeString;
begin
  ODBCCursor := TODBCCursorAccess(cursor);
  if (not FExecDirect) or (Assigned(AParams) and (AParams.Count > 0))
    or (ODBCCursor.FSchemaType <> stNoSchema) then begin
    inherited;
    Exit;
  end;
  WSQL := UnicodeString(ODBCCursor.FQuery);
  Res := SQLExecDirectW(ODBCCursor.FSTMTHandle, PWideChar(WSQL), Length(WSQL));
  // SQL_NO_DATA: searched UPDATE or DELETE without affected rows
  if Res <> SQL_NO_DATA then
    CheckOdbcResult(Res, SQL_HANDLE_STMT, ODBCCursor.FSTMTHandle, 'Could not execute statement.');
  // Skip row counts before the first result set with columns, e.g. from an INSERT in a
  // procedure without SET NOCOUNT ON. SQLRowCount is gone after SQLMoreResults, so the
  // counts are summed up here, see RowsAffected.
  FDirectCursor := cursor;
  FDirectRowsAffected := -1;
  while True do begin
    ColumnCount := 0;
    if not (SQLNumResultCols(ODBCCursor.FSTMTHandle, ColumnCount) in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO]) then
      ColumnCount := 0;
    if ColumnCount > 0 then
      Break;
    RowCount := -1;
    if Res = SQL_NO_DATA then
      FDirectRowsAffected := Max(FDirectRowsAffected, 0)
    else if (SQLRowCount(ODBCCursor.FSTMTHandle, RowCount) in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO])
      and (RowCount >= 0) then
      FDirectRowsAffected := Max(FDirectRowsAffected, 0) + RowCount;
    Res := OdbcMoreResults(ODBCCursor.FSTMTHandle);
    if Res = SQL_NO_DATA then
      Break;
    CheckOdbcResult(Res, SQL_HANDLE_STMT, ODBCCursor.FSTMTHandle, 'Could not execute statement.');
  end;
  ODBCCursor.FSelectable := ColumnCount > 0;
end;


function TFixedODBCConnection.RowsAffected(cursor: TSQLCursor): TRowsCount;
begin
  if FExecDirect and (cursor = FDirectCursor) then
    Result := FDirectRowsAffected
  else
    Result := inherited;
end;


function TFixedODBCConnection.StartDBTransaction(trans: TSQLHandle; AParams: string): boolean;
begin
  if not FAutoCommit then
    Exit(inherited);
  CheckOdbcResult(SQLSetConnectAttr(SQLHDBC(Handle), SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER),
    SQL_HANDLE_DBC, SQLHDBC(Handle), 'Could not switch on autocommit.');
  // No transaction to commit or roll back
  Result := False;
end;


const
  // MS SQL specific types, rejected by odbcconn's AddFieldDefs, or (variant) not loadable there
  SQL_SS_VARIANT_ = -150;
  SQL_SS_UDT_ = -151;
  SQL_SS_TIMESTAMPOFFSET_ = -155;

function HasUnsupportedColumns(Stmt: SQLHSTMT): Boolean;
var
  ColCount, NameLen, DataType, Digits, Nullable: SQLSMALLINT;
  ColSize: SQLULEN;
  i: Integer;
begin
  Result := False;
  ColCount := 0;
  if not (SQLNumResultCols(Stmt, ColCount) in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO]) then
    Exit;
  NameLen := 0;
  DataType := 0;
  ColSize := 0;
  Digits := 0;
  Nullable := 0;
  for i:=1 to ColCount do begin
    if (SQLDescribeCol(Stmt, i, nil, 0, NameLen, DataType, ColSize, Digits, Nullable)
      in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO])
      and ((DataType = SQL_SS_VARIANT_) or (DataType = SQL_SS_UDT_) or (DataType = SQL_SS_TIMESTAMPOFFSET_)) then
      Exit(True);
  end;
end;

procedure TFixedODBCConnection.AddFieldDefsForAllTypes(cursor: TSQLCursor; FieldDefs: TFieldDefs);
type
  TNameBuffer = array[0..255] of WideChar;
var
  Stmt: SQLHSTMT;
  ColCount, NameLen, DataType, Digits, Nullable: SQLSMALLINT;
  ColSize: SQLULEN;
  i, FieldSize: Integer;
  ColName: TNameBuffer;
  FieldType: TFieldType;
begin
  // Replacement for inherited AddFieldDefs, used when the result has MS SQL types which it
  // rejects with "unknown or unsupported column type". These are fetched as text (datetimeoffset,
  // sql_variant) or raw bytes (CLR types like geography). Other types as in odbcconn.
  Stmt := TODBCCursorAccess(cursor).FSTMTHandle;
  ColCount := 0;
  CheckOdbcResult(SQLNumResultCols(Stmt, ColCount), SQL_HANDLE_STMT, Stmt,
    'Could not determine number of columns in result set.');
  for i:=1 to ColCount do begin
    ColName := Default(TNameBuffer);
    NameLen := 0;
    DataType := 0;
    ColSize := 0;
    Digits := 0;
    Nullable := 0;
    CheckOdbcResult(SQLDescribeColW(Stmt, i, @ColName[0], Length(ColName), NameLen, DataType, ColSize, Digits, Nullable),
      SQL_HANDLE_STMT, Stmt, Format('Could not get column properties for column %d.', [i]));
    FieldSize := 0;
    case DataType of
      SQL_CHAR, SQL_WCHAR: begin FieldType := ftFixedWideChar; FieldSize := ColSize; end;
      SQL_VARCHAR, SQL_WVARCHAR: begin FieldType := ftWideString; FieldSize := ColSize; end;
      SQL_LONGVARCHAR, SQL_WLONGVARCHAR, SQL_SS_XML: FieldType := ftWideMemo;
      SQL_DECIMAL, SQL_NUMERIC: FieldType := ftFloat; // see below
      SQL_SMALLINT, SQL_TINYINT: FieldType := ftSmallint;
      SQL_INTEGER: FieldType := ftInteger;
      SQL_BIGINT: FieldType := ftLargeint;
      SQL_REAL, SQL_FLOAT, SQL_DOUBLE: FieldType := ftFloat;
      SQL_BIT: FieldType := ftBoolean;
      SQL_BINARY: begin FieldType := ftBytes; FieldSize := ColSize; end;
      SQL_VARBINARY: begin FieldType := ftVarBytes; FieldSize := ColSize; end;
      SQL_LONGVARBINARY, SQL_SS_UDT_: FieldType := ftBlob;
      SQL_TYPE_DATE: FieldType := ftDate;
      SQL_SS_TIME2, SQL_TYPE_TIME: FieldType := ftTime;
      SQL_TYPE_TIMESTAMP: FieldType := ftDateTime;
      SQL_GUID: begin FieldType := ftGuid; FieldSize := 38; end;
      // "2026-11-27 16:03:37.5297149 +01:00"
      SQL_SS_TIMESTAMPOFFSET_: begin FieldType := ftWideString; FieldSize := 34; end;
      SQL_SS_VARIANT_: begin FieldType := ftWideString; FieldSize := 8000; end;
      else begin FieldType := ftWideString; FieldSize := Max(ColSize, 1); end;
    end;
    if (FieldType in [ftWideString, ftFixedWideChar, ftVarBytes]) and ((FieldSize = 0) or (FieldSize > MaxSmallint)) then begin
      // (max) types, handled as blobs. The fix-up loop in AddFieldDefs does the same for texts.
      if FieldType = ftVarBytes then
        FieldType := ftBlob
      else
        FieldType := ftWideMemo;
      FieldSize := 0;
    end;
    if FieldType in [ftWideMemo, ftBlob] then
      FieldSize := 0;
    AddFieldDef(FieldDefs, i, String(UnicodeString(PWideChar(@ColName[0]))), FieldType, FieldSize, -1,
      False, Nullable = SQL_NO_NULLS, False);
  end;
end;


procedure TFixedODBCConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);
var
  Stmt: SQLHSTMT;
  i: Integer;
  NameLen, DataType, Digits, Nullable: SQLSMALLINT;
  ColSize: SQLULEN;
begin
  Stmt := TODBCCursorAccess(cursor).FSTMTHandle;
  if HasUnsupportedColumns(Stmt) then
    AddFieldDefsForAllTypes(cursor, FieldDefs)
  else
    inherited AddFieldDefs(cursor, FieldDefs);
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
    {$IFDEF WINDOWS}
    // Driver messages are fetched via the ANSI SQLGetDiagRec, in the Windows codepage,
    // e.g. German umlauts from the SQL Server driver
    if FindInvalidUTF8Codepoint(PChar(Rest), Length(Rest)) >= 0 then
      Rest := WinCPToUTF8(Rest);
    {$ENDIF}
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


function OdbcSetQueryTimeout(Stmt: SQLHSTMT; Seconds: Integer): Boolean;
begin
  // SQL_QUERY_TIMEOUT = SQL_ATTR_QUERY_TIMEOUT
  Result := SQLSetStmtAttr(Stmt, SQL_QUERY_TIMEOUT, SQLPOINTER(PtrUInt(Seconds)), SQL_IS_UINTEGER)
    in [SQL_SUCCESS, SQL_SUCCESS_WITH_INFO];
end;


initialization
  ODBCFormatSettings := DefaultFormatSettings;
  ODBCFormatSettings.DecimalSeparator := '.';
  ODBCFormatSettings.ThousandSeparator := #0;

end.
