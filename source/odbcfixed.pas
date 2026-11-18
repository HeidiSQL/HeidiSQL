unit odbcfixed;

// SQLDB TODBCConnection with fixes for long texts, text encoding and exact decimals.
// See extra/odbc.md for the problems in odbcconn as shipped with FPC 3.2.2.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, BufDataset, FmtBCD, odbcconn, odbcsqldyn;

type

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
    // Oracle: unconstrained NUMBER and FLOAT(n) are reported as SQL_FLOAT with 38 digits
    property Float38AsDecimal: Boolean read FFloat38AsDecimal write FFloat38AsDecimal;
  end;

// Number of result columns of a prepared, not yet executed statement. -1 if the driver cannot tell.
function OdbcPreparedColumnCount(Cursor: TSQLCursor): Integer;
// Execute a statement without result set via SQLExecDirect, without preparing it before.
// Oracle's driver takes :NEW in trigger bodies for a bind variable on SQLPrepare, and fails.
// Returns the number of affected rows, or -1.
function OdbcExecDirect(Conn: TODBCConnection; const SQL: String): Int64;
// Readable message from an EODBCException: only the driver messages, without the
// connection string, which contains the password
function OdbcErrorText(const Msg: String): String;
// Names of installed ODBC drivers, filtered by a regular expression
function GetOdbcDriverNames(const FilterRegex: String): TStringList;
// Let the Oracle client talk UTF-8, if the user has not configured NLS_LANG
function PrepareOracleClientEnvironment: String;

implementation

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF} RegExpr;

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


procedure TFixedODBCConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);
var
  Stmt: SQLHSTMT;
  i: Integer;
  NameLen, DataType, Digits, Nullable: SQLSMALLINT;
  ColSize: SQLULEN;
begin
  inherited AddFieldDefs(cursor, FieldDefs);
  Stmt := TODBCCursorAccess(cursor).FSTMTHandle;
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
var
  Res: SQLRETURN;
  StrLenOrInd: SQLLEN;
  WTxt: array[0..MaxFmtBCDFractionSize+3] of WideChar;
  Bcd: TBCD;
begin
  if FieldDef.DataType <> ftFmtBCD then
    Exit(inherited LoadField(cursor, FieldDef, buffer, CreateBlob));
  // Fetch decimals as text, the driver sends "." and no thousand separators.
  // Oracle needs NUM=US in the connection string for that.
  CreateBlob := False;
  StrLenOrInd := 0;
  FillChar(WTxt, SizeOf(WTxt), 0);
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
var
  NativeError, FirstNativeError: SQLINTEGER;
  TextLength: SQLSMALLINT;
  SqlState, FirstSqlState: String;
  MsgText: array[0..1023] of AnsiChar;
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
    SetLength(SqlState, 5);
    FillChar(MsgText, SizeOf(MsgText), 0);
    NativeError := 0;
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


function OdbcExecDirect(Conn: TODBCConnection; const SQL: String): Int64;
var
  Dbc: SQLHDBC;
  Stmt: SQLHSTMT;
  Res: SQLRETURN;
  WSQL: UnicodeString;
  RowCount: SQLLEN;
begin
  Result := -1;
  Dbc := SQLHDBC(Conn.Handle);
  Stmt := SQL_NULL_HSTMT;
  CheckOdbcResult(SQLAllocHandle(SQL_HANDLE_STMT, Dbc, Stmt),
    SQL_HANDLE_DBC, Dbc, 'Could not allocate ODBC Statement handle.');
  try
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
var
  Env: TODBCEnvironment;
  Res: SQLRETURN;
  Direction: SQLUSMALLINT;
  Desc: array[0..511] of AnsiChar;
  Attrs: array[0..2047] of AnsiChar;
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
      // Loads the driver manager (odbc32.dll / libodbc)
      Env := TODBCEnvironment.Create;
      Direction := SQL_FETCH_FIRST;
      while True do begin
        FillChar(Desc, SizeOf(Desc), 0);
        FillChar(Attrs, SizeOf(Attrs), 0);
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


initialization
  ODBCFormatSettings := DefaultFormatSettings;
  ODBCFormatSettings.DecimalSeparator := '.';
  ODBCFormatSettings.ThousandSeparator := #0;

end.
