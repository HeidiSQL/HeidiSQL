{
    This file is part of the Free Component Library (FCL)

    MS SQL Server connection using DB-Library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    The Original Code was created by (c) 2010 Ladislav Karrach (Windows)
    for the Free Pascal project.
 **********************************************************************

    MS SQL Server Client Library is required (ntwdblib.dll)
    - or -
    FreeTDS (dblib.dll)
      freetds.conf: (http://www.freetds.org/userguide/freetdsconf.htm)
        [global]
        tds version = 7.1
        client charset = UTF-8
        port = 1433 or instance = ...  (optional)
        dump file = freetds.log        (optional)
        text size = 2147483647         (optional)

    TMSSQLConnection properties:
      HostName - can be specified also as 'servername:port' or 'servername\instance'
                 (SQL Server Browser Service must be running on server to connect to specific instance)
      CharSet - if you use Microsoft DB-Lib and set to 'UTF-8' then char/varchar fields will be UTF8Encoded/Decoded
                if you use FreeTDS DB-Lib then you must compile with iconv support (requires libiconv2.dll) or cast char/varchar to nchar/nvarchar in SELECTs
      Params - "AutoCommit=true" - if you don't want explicitly commit/rollback transactions
               "TextSize=16777216" - set maximum size of text/image data returned
               "ApplicationName=YourAppName" - Set the app name for the connection. MSSQL 2000 and higher only
}
unit MSSQLConn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset,
  dblib;

type

  { TMSSQLConnection }

  TMSSQLConnection = class(TSQLConnection)
  private
    type
      TServerInfo = record
        ServerVersion: string;
        ServerVersionString: string;
        UserName: string;
      end;
    var
      FDBLogin: PLOGINREC;
      FDBProc : PDBPROCESS;
      Ftds    : integer;     // TDS protocol version
      Fstatus : STATUS;      // current result/rows fetch status
      FServerInfo: TServerInfo;
    function CheckError(const Ret: RETCODE): RETCODE;
    procedure Execute(const cmd: string); overload;
    procedure ExecuteDirectSQL(const Query: string);
    procedure CancelQuery;
    procedure GetParameters(cursor: TSQLCursor; AParams: TParams);
    function TranslateFldType(SQLDataType: integer): TFieldType;
    function AutoCommit: boolean;
    function IsSybase: boolean;
  protected
    // Overrides from TSQLConnection
    function GetHandle:pointer; override;
    function GetAsSQLText(Param : TParam) : string; overload; override;
    function GetConnectionCharSet: string; override;
    // - Connect/disconnect
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    // - Handle (de)allocation
    function AllocateCursorHandle:TSQLCursor; override;
    procedure DeAllocateCursorHandle(var cursor:TSQLCursor); override;
    function AllocateTransactionHandle:TSQLHandle; override;
    // - Transaction handling
    function GetTransactionHandle(trans:TSQLHandle):pointer; override;
    function StartDBTransaction(trans:TSQLHandle; AParams:string):boolean; override;
    function Commit(trans:TSQLHandle):boolean; override;
    function Rollback(trans:TSQLHandle):boolean; override;
    procedure CommitRetaining(trans:TSQLHandle); override;
    procedure RollbackRetaining(trans:TSQLHandle); override;
    // - Statement handling
    function StrToStatementType(s : string) : TStatementType; override;
    procedure PrepareStatement(cursor:TSQLCursor; ATransaction:TSQLTransaction; buf:string; AParams:TParams); override;
    procedure UnPrepareStatement(cursor:TSQLCursor); override;
    // - Statement execution
    procedure Execute(cursor:TSQLCursor; ATransaction:TSQLTransaction; AParams:TParams); override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
    function RefreshLastInsertID(Query : TCustomSQLQuery; Field : TField): boolean; override;
    // - Result retrieving
    procedure AddFieldDefs(cursor:TSQLCursor; FieldDefs:TFieldDefs); override;
    function Fetch(cursor:TSQLCursor):boolean; override;
    function LoadField(cursor:TSQLCursor; FieldDef:TFieldDef; buffer:pointer; out CreateBlob : boolean):boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TSQLTransaction); override;
    procedure FreeFldBuffers(cursor:TSQLCursor); override;
    // - UpdateIndexDefs
    procedure UpdateIndexDefs(IndexDefs:TIndexDefs; TableName:string); override;
    // - Schema info
    function GetSchemaInfoSQL(SchemaType:TSchemaType; SchemaObjectName, SchemaObjectPattern:string):string; override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetConnectionInfo(InfoType:TConnInfoType): string; override;
    procedure CreateDB; override;
    procedure DropDB; override;
    //property TDS:integer read Ftds;
  published
    // Redeclare properties from TSQLConnection
    property Password;
    property Transaction;
    property UserName;
    property CharSet;
    property HostName;
    // Redeclare properties from TDatabase
    property Connected;
    property Role;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

  { TSybaseConnection }

  TSybaseConnection = class(TMSSQLConnection)
  public
    constructor Create(AOwner : TComponent); override;
  end;

  { EMSSQLDatabaseError }

  EMSSQLDatabaseError = class(ESQLDatabaseError)
    public
      property DBErrorCode: integer read ErrorCode; deprecated 'Please use ErrorCode instead of DBErrorCode'; // Feb 2014
  end;

  { TMSSQLConnectionDef }

  TMSSQLConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
    Class Function DefaultLibraryName : String; override;
    Class Function LoadFunction : TLibraryLoadFunction; override;
    Class Function UnLoadFunction : TLibraryUnLoadFunction; override;
    Class Function LoadedLibraryName: string; override;
  end;

  { TSybaseConnectionDef }

  TSybaseConnectionDef = Class(TMSSQLConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
  end;


var
   DBLibLibraryName: string = DBLIBDLL;

implementation

uses StrUtils, FmtBCD;

type

  { TDBLibCursor }

  TDBLibCursor = class(TSQLCursor)
  private
    FConnection: TMSSQLConnection;                    // owner connection
    FQuery: string;                                   // :ParamNames converted to $1,$2,..,$n
    FParamReplaceString: string;
  protected
    FRowsAffected: integer;
    function ReplaceParams(AParams: TParams): string; // replaces parameters placeholders $1,$2,..$n in FQuery with supplied values in AParams
    procedure Prepare(const Buf: string; AParams: TParams);
    procedure Execute(AParams: TParams);
    function Fetch: boolean;
    procedure Put(column: integer; out s: string); overload;
  public
    constructor Create(AConnection: TMSSQLConnection); overload;
  end;


const
  SBeginTransaction = 'BEGIN TRANSACTION';
  SAutoCommit = 'AUTOCOMMIT';
  STextSize   = 'TEXTSIZE';
  SAppName    = 'APPLICATIONNAME';


var
  DBErrorStr, DBMsgStr: AnsiString;
  DBErrorNo, DBMsgNo: integer;

function DBErrHandler(dbproc: PDBPROCESS; severity, dberr, oserr:INT; dberrstr, oserrstr:PAnsiChar):INT; cdecl;
begin
  DBErrorStr:=DBErrorStr+LineEnding+dberrstr;
  DBErrorNo :=dberr;
  Result    :=INT_CANCEL;
  // for server messages with severity greater than 10 error handler is also called
end;

function DBMsgHandler(dbproc: PDBPROCESS; msgno: DBINT; msgstate, severity:INT; msgtext, srvname, procname:PAnsiChar; line:DBUSMALLINT):INT; cdecl;
begin
  DBMsgStr:=DBMsgStr+LineEnding+msgtext;
  DBMsgNo :=msgno;
  Result  :=0;
end;


{ TDBLibCursor }

procedure TDBLibCursor.Prepare(const Buf: string; AParams: TParams);
var
  ParamBinding : TParamBinding;
begin
  if assigned(AParams) and (AParams.Count > 0) then
    FQuery := AParams.ParseSQL(Buf, False, sqEscapeSlash in FConnection.ConnOptions, sqEscapeRepeat in FConnection.ConnOptions, psSimulated, ParamBinding, FParamReplaceString)
  else
    FQuery := Buf;
  FPrepared := True;
end;

function TDBLibCursor.ReplaceParams(AParams: TParams): string;
var i: integer;
    ParamNames, ParamValues: array of string;
begin
  if Assigned(AParams) and (AParams.Count > 0) then //taken from mysqlconn, pqconnection
  begin
    setlength(ParamNames, AParams.Count);
    setlength(ParamValues, AParams.Count);
    for i := 0 to AParams.Count -1 do
    begin
      ParamNames[AParams.Count-i-1] := format('%s%d', [FParamReplaceString, AParams[i].Index+1]);
      ParamValues[AParams.Count-i-1] := FConnection.GetAsSQLText(AParams[i]);
    end;
    Result := stringsreplace(FQuery, ParamNames, ParamValues, [rfReplaceAll]);
  end
  else
    Result := FQuery;
end;

procedure TDBLibCursor.Execute(AParams: TParams);
begin
  Fconnection.Execute(Self, nil, AParams);
end;

function TDBLibCursor.Fetch: boolean;
begin
  Result := Fconnection.Fetch(Self);
end;

procedure TDBLibCursor.Put(column: integer; out s: string);
var
  data: PByte;
  datalen: DBINT;
begin
  data := dbdata(Fconnection.FDBProc, column);
  datalen := dbdatlen(Fconnection.FDBProc, column);
  SetString(s, PAnsiChar(data), datalen);
end;

constructor TDBLibCursor.Create(AConnection: TMSSQLConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;


{ TSybaseConnection }

constructor TSybaseConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Ftds := DBTDS_50;
end;


{ TMSSQLConnection }

function TMSSQLConnection.IsSybase: boolean;
begin
  Result := (Ftds=DBTDS_50) or (Ftds=DBTDS_42);
end;

function TMSSQLConnection.CheckError(const Ret: RETCODE): RETCODE;
var E: EMSSQLDatabaseError;
begin
  if (Ret=FAIL) or (DBErrorStr<>'') then
  begin
    // try clear all pending results to allow ROLLBACK and prevent error 10038 "Results pending"
    if assigned(FDBProc) then dbcancel(FDBProc);
    if DBErrorStr = '' then
      case DBErrorNo of
        SYBEFCON: DBErrorStr:='SQL Server connection failed!';
      end;
    E:=EMSSQLDatabaseError.CreateFmt('Error %d : %s'+LineEnding+'%s', [DBErrorNo, DBErrorStr, DBMsgStr], Self, DBErrorNo, '');
    DBErrorStr:='';
    DBMsgStr:='';
    raise E;
  end;
  Result:=Ret;
end;

constructor TMSSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnOptions := [sqSupportEmptyDatabaseName, sqEscapeRepeat, sqImplicitTransaction, sqLastInsertID, sqSequences];
  //FieldNameQuoteChars:=DoubleQuotes; //default
  Ftds := DBTDS_UNKNOWN;
end;

procedure TMSSQLConnection.CreateDB;
begin
  ExecuteDirectSQL('CREATE DATABASE '+DatabaseName);
end;

procedure TMSSQLConnection.DropDB;
begin
  ExecuteDirectSQL('DROP DATABASE '+DatabaseName);
end;

procedure TMSSQLConnection.ExecuteDirectSQL(const Query: string);
var ADatabaseName: string;
begin
  CheckDisConnected;
  ADatabaseName:=DatabaseName;
  DatabaseName:='';
  try
    Open;
    Execute(Query);
  finally
    Close;
    DatabaseName:=ADatabaseName;
  end;
end;

procedure TMSSQLConnection.CancelQuery;
begin
  // Cancel the query currently being retrieved, discarding all pending rows and all remaining resultsets
  if Fstatus = MORE_ROWS then begin
    repeat
      dbcanquery(FDBProc);
    until dbresults(FDBProc) <> SUCCEED;
    Fstatus := NO_MORE_ROWS;
  end;
end;

function TMSSQLConnection.GetHandle: pointer;
begin
  Result:=FDBProc;
end;

function TMSSQLConnection.GetAsSQLText(Param: TParam): string;
  function IsBinary(const s: string): boolean;
  var i: integer;
  begin
    for i:=1 to length(s) do if s[i] < #9 then Exit(true);
    Exit(false);
  end;
  function StrToHex(const s: string): string;
  begin
    setlength(Result, 2*length(s));
    BinToHex(PChar(s), PChar(Result), length(s));
  end;
begin
  if not Param.IsNull then
    case Param.DataType of
      ftBoolean:
        if Param.AsBoolean then
          Result:='1'
        else
          Result:='0';
      ftString, ftFixedChar, ftMemo:
        //if IsBinary(Param.AsString) then
        //  Result := '0x' + StrToHex(Param.AsString)
        //else
        Result := 'N' + inherited GetAsSQLText(Param);
      ftDateTime:
        // ISO 8601 format is unambiguous; is not affected by the SET DATEFORMAT or SET LANGUAGE setting.
        Result := '''' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Param.AsDateTime, FSQLFormatSettings) + '''';
      ftBlob, ftBytes, ftVarBytes:
        Result := '0x' + StrToHex(Param.AsString);
      else
        Result := inherited GetAsSQLText(Param);
    end
  else
    Result:=inherited GetAsSQLText(Param);
end;

function TMSSQLConnection.GetConnectionCharSet: string;
begin
  if CharSet = '' then
    Result := 'utf-8'
  else
    Result := CharSet;
end;

procedure TMSSQLConnection.DoInternalConnect;
const
  DBVERSION: array[boolean] of BYTE = (DBVERSION_73, DBVERSION_100);
  IMPLICIT_TRANSACTIONS_OFF: array[boolean] of shortstring = ('SET IMPLICIT_TRANSACTIONS OFF', 'SET CHAINED OFF');
  ANSI_DEFAULTS_ON: array[boolean] of shortstring = ('SET ANSI_DEFAULTS ON', 'SET QUOTED_IDENTIFIER ON');
  CURSOR_CLOSE_ON_COMMIT_OFF: array[boolean] of shortstring = ('SET CURSOR_CLOSE_ON_COMMIT OFF', 'SET CLOSE ON ENDTRAN OFF');
  VERSION_NUMBER: array[boolean] of shortstring = ('SERVERPROPERTY(''ProductVersion'')', '@@version_number');
  
begin
  // empty DatabaseName=default database defined for login
  inherited DoInternalConnect;

  InitialiseDBLib(DBLibLibraryName);

  if not DBLibInit then
  begin
    dbinit();
    dberrhandle(@DBErrHandler);
    dbmsghandle(@DBMsgHandler);
    DBLibInit:=true;
  end;

  FDBLogin:=dblogin();
  if FDBLogin=nil then DatabaseError('dblogin() failed!');

  // DBVERSION_100 is ATM not implemented by FreeTDS 0.91;
  // set environment variable TDSVER to 5.0: Windows: SET TDSVER=5.0, Unix/Linux: TDSVER=5.0
  // or in freetds.conf: include "tds version=5.0"
  dbsetlversion(FDBLogin, DBVERSION[IsSybase]);

  if UserName = '' then
    dbsetlsecure(FDBLogin)
  else
  begin
    dbsetlname(FDBLogin, PAnsiChar(UserName), DBSETUSER);
    dbsetlname(FDBLogin, PAnsiChar(Password), DBSETPWD);
  end;

  if CharSet = '' then
    dbsetlcharset(FDBLogin, 'UTF-8')
  else
    dbsetlcharset(FDBLogin, PAnsiChar(CharSet));

  if Params.IndexOfName(SAppName) <> -1 then
    dbsetlname(FDBLogin, PAnsiChar(Params.Values[SAppName]), DBSETAPP);

  //dbsetlname(FDBLogin, PAnsiChar(TIMEOUT_IGNORE), DBSET_LOGINTIME);
  dbsetlogintime(10);

  FDBProc := dbopen(FDBLogin, PAnsiChar(HostName));
  if FDBProc=nil then CheckError(FAIL);

  Ftds := dbtds(FDBProc);

  //CheckError( dbsetopt(FDBProc, DBQUOTEDIDENT, '') ); //in FreeTDS executes: "SET QUOTED_IDENTIFIER ON"
  //CheckError( dbsetopt(FDBProc, DBTEXTSIZE,  '2147483647') ); //in FreeTDS: unimplemented, returns FAIL
  //CheckError( dbsetopt(FDBProc, DBTEXTLIMIT, '2147483647') ); //in FreeTDS: unimplemented, returns FAIL, but required by ntwdblib.dll
  //CheckError( dbsqlexec(FDBProc) ); //after setting DBTEXTSIZE option
  //CheckError (dbresults(FDBProc));
  //while dbresults(FDBProc) = SUCCEED do ;

  // Also SQL Server ODBC driver and Microsoft OLE DB Provider for SQL Server set ANSI_DEFAULTS to ON when connecting
  //Execute(ANSI_DEFAULTS_ON[IsSybase]);
  Execute('SET QUOTED_IDENTIFIER ON');

  if Params.IndexOfName(STextSize) <> -1 then
    Execute('SET TEXTSIZE '+Params.Values[STextSize])
  else
    Execute('SET TEXTSIZE 16777216');

  if AutoCommit then
    Execute(IMPLICIT_TRANSACTIONS_OFF[IsSybase]); //set connection to autocommit mode - default

  if DatabaseName <> '' then
    CheckError( dbuse(FDBProc, PAnsiChar(DatabaseName)) );

  with TDBLibCursor.Create(Self) do
  begin
    try
      Prepare(format('SELECT cast(%s as varchar), @@version, user_name()', [VERSION_NUMBER[IsSybase]]), nil);
      Execute(nil);
      while Fetch do
      begin
        Put(1, FServerInfo.ServerVersion);
        Put(2, FServerInfo.ServerVersionString);
        Put(3, FServerInfo.UserName);
      end;
    except
      FServerInfo.ServerVersion:='';
      FServerInfo.ServerVersionString:='';
      FServerInfo.UserName:='';
    end;
    Free;
  end;
end;

procedure TMSSQLConnection.DoInternalDisconnect;
begin
  inherited DoInternalDisconnect;

  dbclose(FDBProc);
  dbfreelogin(FDBLogin);

  ReleaseDBLib;
end;

function TMSSQLConnection.AllocateCursorHandle: TSQLCursor;
begin
   Result:=TDBLibCursor.Create(Self);
end;

procedure TMSSQLConnection.DeAllocateCursorHandle(var cursor: TSQLCursor);
begin
  FreeAndNil(cursor);
end;

function TMSSQLConnection.StrToStatementType(s: string): TStatementType;
begin
  s:=LowerCase(s);
  if s = 'exec' then
    Result:=stExecProcedure
  else
    Result:=inherited StrToStatementType(s);
end;

function TMSSQLConnection.AllocateTransactionHandle: TSQLHandle;
begin
  Result:=nil;
end;

function TMSSQLConnection.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
  Result:=nil;
end;

function TMSSQLConnection.StartDBTransaction(trans: TSQLHandle; AParams: string): boolean;
begin
  Result := not AutoCommit;
  if Result then
    Execute(SBeginTransaction);
end;

function TMSSQLConnection.Commit(trans: TSQLHandle): boolean;
begin
  Execute('COMMIT');
  Result:=true;
end;

function TMSSQLConnection.Rollback(trans: TSQLHandle): boolean;
begin
  Execute('IF @@TRANCOUNT>0 ROLLBACK');
  Result:=true;
end;

procedure TMSSQLConnection.CommitRetaining(trans: TSQLHandle);
begin
  if Commit(trans) then
    Execute(SBeginTransaction);
end;

procedure TMSSQLConnection.RollbackRetaining(trans: TSQLHandle);
begin
  if Rollback(trans) then
    Execute(SBeginTransaction);
end;

function TMSSQLConnection.AutoCommit: boolean;
begin
  Result := StrToBoolDef(Params.Values[SAutoCommit], False);
end;

procedure TMSSQLConnection.PrepareStatement(cursor: TSQLCursor;
   ATransaction: TSQLTransaction; buf: string; AParams: TParams);
begin
  (cursor as TDBLibCursor).Prepare(buf, AParams);
end;

procedure TMSSQLConnection.UnPrepareStatement(cursor: TSQLCursor);
begin
  CancelQuery;
  cursor.FPrepared := False;
end;

procedure TMSSQLConnection.Execute(const cmd: string);
begin
  DBErrorStr:='';
  DBMsgStr  :='';
  CheckError( dbcmd(FDBProc, PAnsiChar(cmd)) );
  CheckError( dbsqlexec(FDBProc) );
  CheckError( dbresults(FDBProc) );
end;

procedure TMSSQLConnection.Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
var c: TDBLibCursor;
    cmd: string;
    res: RETCODE;
begin
  c:=cursor as TDBLibCursor;

  if LogEvent(detParamValue) then
    LogParams(AParams);
  cmd := c.ReplaceParams(AParams);
  if LogEvent(detActualSQL) then
    Log(detActualSQL,Cmd);
  Execute(cmd);

  res := SUCCEED;
  repeat
    c.FSelectable := dbcmdrow(FDBProc)=SUCCEED;
    c.FRowsAffected := dbcount(FDBProc);
    if assigned(dbiscount) and not dbiscount(FDBProc) then
      c.FRowsAffected := -1;

    if not c.FSelectable then  //Sybase stored proc.
    begin
      repeat
        Fstatus := dbnextrow(FDBProc);
      until (Fstatus = NO_MORE_ROWS) or (Fstatus = FAIL);
      res := CheckError( dbresults(FDBProc) );
      // stored procedure information (return status and output parameters)
      // are available only after normal results are processed
      //if res = NO_MORE_RESULTS then GetParameters(cursor, AParams);
    end;
  until c.FSelectable or (res = NO_MORE_RESULTS) or (res = FAIL);

  if res = NO_MORE_RESULTS then
    Fstatus := NO_MORE_ROWS
  else
    Fstatus := MORE_ROWS;
end;

procedure TMSSQLConnection.GetParameters(cursor: TSQLCursor; AParams: TParams);
var Param: TParam;
begin
  // Microsoft SQL Server no more returns OUTPUT parameters as a special result row
  // so we can not use dbret*() functions, but instead we must use dbrpc*() functions
  // only procedure return status number is returned
  if dbhasretstat(FDBProc) = 1 then
    begin
    Param := AParams.FindParam('RETURN_STATUS');
    if not assigned(Param) then
      Param := AParams.CreateParam(ftInteger, 'RETURN_STATUS', ptOutput);
    Param.AsInteger := dbretstatus(FDBProc);
    end;
end;

function TMSSQLConnection.RowsAffected(cursor: TSQLCursor): TRowsCount;
begin
  if assigned(cursor) then
    Result := (cursor as TDBLibCursor).FRowsAffected
  else
    Result := inherited RowsAffected(cursor);
end;

function TMSSQLConnection.RefreshLastInsertID(Query: TCustomSQLQuery; Field: TField): boolean;
var Identity: int64;
begin
  // global variable @@IDENTITY is NUMERIC(38,0)
  Result:=False;
  if dbcmd(FDBProc, 'SELECT @@IDENTITY') = FAIL then Exit;
  if dbsqlexec(FDBProc) = FAIL then Exit;
  if dbresults(FDBProc) = FAIL then Exit;
  if dbnextrow(FDBProc) = FAIL then Exit;
  if dbconvert(FDBProc, dbcoltype(FDBProc,1), dbdata(FDBProc,1), -1, SYBINT8, @Identity, sizeof(Identity)) = -1 then Exit;
  // by default identity columns are ReadOnly
  Field.AsLargeInt := Identity;
  Result:=True;
end;

function TMSSQLConnection.TranslateFldType(SQLDataType: integer): TFieldType;
begin
  case SQLDataType of
    SQLCHAR:             Result:=ftFixedChar;
    SQLVARCHAR:          Result:=ftString;
    SQLINT1:             Result:=ftWord;
    SQLINT2:             Result:=ftSmallInt;
    SQLINT4, SQLINTN:    Result:=ftInteger;
    SYBINT8:             Result:=ftLargeInt;
    SQLFLT4, SQLFLT8,
    SQLFLTN:             Result:=ftFloat;
    SQLMONEY4, SQLMONEY,
    SQLMONEYN:           Result:=ftCurrency;
    SYBMSDATE:           Result:=ftDate;
    SYBMSTIME:           Result:=ftTime;
    SQLDATETIM4, SQLDATETIME,
    SQLDATETIMN,
    SYBMSDATETIME2,
    SYBMSDATETIMEOFFSET: Result:=ftDateTime;
    SYBMSXML,
    SQLTEXT:             Result:=ftMemo;
    SQLIMAGE:            Result:=ftBlob;
    SQLDECIMAL, SQLNUMERIC: Result:=ftBCD;
    SQLBIT:              Result:=ftBoolean;
    SQLBINARY:           Result:=ftBytes;
    SQLVARBINARY:        Result:=ftVarBytes;
    SYBUNIQUE:           Result:=ftGuid;
    SYBVARIANT:          Result:=ftBlob;
  else
    DatabaseErrorFmt('Unsupported SQL DataType %d "%s"', [SQLDataType, dbprtype(SQLDataType)]);
    Result:=ftUnknown;
  end;
end;

procedure TMSSQLConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);
var i, FieldSize: integer;
    FieldName: string;
    FieldType: TFieldType;
    col: DBCOL;
begin
  col.SizeOfStruct:=sizeof(col);
  for i:=1 to dbnumcols(FDBProc) do
  begin
    if dbtablecolinfo(FDBProc, i, @col) = FAIL then continue;
    FieldName := col.Name;
    FieldType := TranslateFldType(col.Typ);
    case FieldType of
      ftString, ftFixedChar:
        begin
        FieldSize := col.MaxLength;
        if FieldSize >= $3FFFFFFF then // varchar(max)
           FieldType := ftMemo;
        end;
      ftBytes, ftVarBytes:
        begin
        FieldSize := col.MaxLength;
        if FieldSize >= $3FFFFFFF then // varbinary(max)
           FieldType := ftBlob;
        end;
      ftBCD:
        begin
        FieldSize := col.Scale;
        if (FieldSize > MaxBCDScale) or (col.Precision-col.Scale > MaxBCDPrecision-MaxBCDScale) then
          FieldType := ftFmtBCD;
        end;
      ftGuid:
        FieldSize := 38;
      else
        FieldSize := 0;
        if col.Identity and (FieldType = ftInteger) then
          FieldType := ftAutoInc;
    end;

    // identity, timestamp and calculated column are not updatable
    AddFieldDef(FieldDefs, i, FieldName, FieldType, FieldSize, col.Precision, True, (col.Null=0) and (not col.Identity), col.Updatable=0);
  end;
end;

function TMSSQLConnection.Fetch(cursor: TSQLCursor): boolean;
begin
  // Compute rows resulting from the COMPUTE clause are not processed
  repeat
    Fstatus := dbnextrow(FDBProc);
    // In case of network failure FAIL is returned
    // Use dbsettime() to specify query timeout, else on Windows TCP KeepAliveTime is used, which defaults to 2 hours 
    Result  := Fstatus=REG_ROW;
  until Result or (Fstatus = NO_MORE_ROWS) or (Fstatus = FAIL);

  if Fstatus = NO_MORE_ROWS then
    while dbresults(FDBProc) <> NO_MORE_RESULTS do // process remaining results if there are any
      repeat
        Fstatus := dbnextrow(FDBProc);
      until (Fstatus = NO_MORE_ROWS) or (Fstatus = FAIL);

  if Fstatus = FAIL then CheckError(FAIL);
end;

function TMSSQLConnection.LoadField(cursor: TSQLCursor; FieldDef: TFieldDef;
   buffer: pointer; out CreateBlob: boolean): boolean;
var i: integer;
    data, dest: PByte;
    datalen, destlen: DBINT;
    srctype, desttype: INT;
    dbdt: DBDATETIME;
    dbdr: DBDATEREC;
    dbdta: DBDATETIMEALL;
    bcdstr: array[0..MaxFmtBCDFractionSize+2] of AnsiChar;
begin
  CreateBlob:=false;
  i:=FieldDef.FieldNo;

  srctype:=dbcoltype(FDBProc,i);
  data:=dbdata(FDBProc,i);
  datalen:=dbdatlen(FDBProc,i);
  Result:=assigned(data) and (datalen>=0);
  if not Result then
    Exit;

  dest:=buffer;
  destlen:=FieldDef.Size;
  case FieldDef.DataType of
    ftString, ftFixedChar:
      begin
      desttype:=SQLCHAR;
      destlen:=FieldDef.Size*FieldDef.CharSize;
      end;
    ftBytes:
      desttype:=SQLBINARY;
    ftVarBytes:
      begin
      PWord(dest)^:=datalen;
      inc(dest, sizeof(Word));
      desttype:=SQLBINARY;
      end;
    ftSmallInt, ftWord:
      begin
      desttype:=SQLINT2;
      destlen:=sizeof(DBSMALLINT); //smallint
      end;
    ftAutoInc,
    ftInteger:
      begin
      desttype:=SQLINT4;
      destlen:=sizeof(DBINT); //integer
      end;
    ftLargeInt:
      begin
      desttype:=SYBINT8;
      destlen:=sizeof(int64);
      end;
    ftCurrency,
    ftFloat:
      begin
      desttype:=SQLFLT8;
      destlen:=sizeof(DBFLT8); //double
      end;
    ftDate, ftTime,
    ftDateTime:
      if srctype in [SYBMSDATE, SYBMSTIME, SYBMSDATETIME2, SYBMSDATETIMEOFFSET] then // dbwillconvert(srctype, SYBMSDATETIME2)
        begin
        dest:=@dbdta;
        desttype:=SYBMSDATETIME2;
        destlen:=sizeof(dbdta);
        end
      else
        begin
        dest:=@dbdt;
        desttype:=SQLDATETIME;
        destlen:=sizeof(dbdt);
        end;
    ftBCD:
      begin
      // FreeTDS 0.91 does not support converting from numeric to money
      //desttype:=SQLMONEY;
      desttype:=SQLFLT8;
      destlen:=sizeof(currency);
      end;
    ftFmtBCD:
      begin
{
      dbnum.precision:=FieldDef.Precision;
      dbnum.scale    :=FieldDef.Size;
      dest:=@dbnum;
      desttype:=SQLNUMERIC;
      destlen:=sizeof(dbnum);
}
      dest:=@bcdstr[0];
      desttype:=SQLCHAR;
      destlen:=sizeof(bcdstr);
      fillchar(bcdstr, destlen, 0); //required when used ntwdblib.dll
      end;
    ftBoolean:
      begin
      desttype:=SQLBIT;
      destlen:=sizeof(WordBool);
      end;
    ftGuid:
      begin
      desttype:=SQLCHAR;
      dest[ 0]:=Ord('{');
      dest[37]:=Ord('}');
      dest[38]:=0; //strings must be null-terminated
      Inc(dest);
      destlen:=36;
      end;
    ftMemo,
    ftBlob:
      begin
      CreateBlob:=true;
      Exit;
      end
  else
    //DatabaseErrorFmt('Tried to load field of unsupported field type %s',[FieldTypeNames[FieldDef.DataType]]);
    Result:=false;
  end;

  dbconvert(FDBProc, srctype, data , datalen, desttype, dest, destlen);

  case FieldDef.DataType of
    ftString, ftFixedChar:
      dest[datalen] := 0; //strings must be null-terminated
    ftDate, ftTime, ftDateTime:
      if desttype = SYBMSDATETIME2 then
        PDateTime(buffer)^ := dbdatetimeallcrack(@dbdta)
      else
      begin
        //detect DBDATEREC version by pre-setting dbdr
        dbdr.millisecond := -1;
        if dbdatecrack(FDBProc, @dbdr, @dbdt) = SUCCEED then
        begin
          if dbdr.millisecond = -1 then
            PDateTime(buffer)^ := composedatetime(
                  encodedate(dbdr.oldyear, dbdr.oldmonth, dbdr.oldday),
                  encodetime(dbdr.oldhour, dbdr.oldminute, dbdr.oldsecond, dbdr.oldmillisecond))
          else
            PDateTime(buffer)^ := composedatetime(
                  encodedate(dbdr.year, dbdr.month, dbdr.day),
                  encodetime(dbdr.hour, dbdr.minute, dbdr.second, dbdr.millisecond));
        end;
      end;
    ftBCD:
      PCurrency(buffer)^ := FloatToCurr(PDouble(buffer)^); //PCurrency(buffer)^ := dbmoneytocurr(buffer);
    ftFmtBCD:
      PBCD(buffer)^:=StrToBCD(bcdstr, FSQLFormatSettings); //PBCD(buffer)^:=dbnumerictobcd(dbnum);
  end;
end;

procedure TMSSQLConnection.LoadBlobIntoBuffer(FieldDef: TFieldDef;
   ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction: TSQLTransaction);
var data: PByte;
    datalen: DBINT;
begin
  // see also LoadField
  data:=dbdata(FDBProc, FieldDef.FieldNo);
  datalen:=dbdatlen(FDBProc, FieldDef.FieldNo);

  ReAllocMem(ABlobBuf^.BlobBuffer^.Buffer, datalen);
  Move(data^, ABlobBuf^.BlobBuffer^.Buffer^, datalen);
  ABlobBuf^.BlobBuffer^.Size := datalen;
end;

procedure TMSSQLConnection.FreeFldBuffers(cursor: TSQLCursor);
begin
  CancelQuery;
  inherited;
end;

procedure TMSSQLConnection.UpdateIndexDefs(IndexDefs: TIndexDefs; TableName: string);
const INDEXES_QUERY: array[boolean] of string=(
      //MS SQL Server; TODO: we can use "execute dbo.sp_helpindex 'TableName'" when Open on Execute will fully work
      'select i.name, i.indid, c.name as col_name,'+
              'indexproperty(i.id, i.name, ''IsUnique''),'+
              'objectproperty(o.id, ''IsPrimaryKey'') '+
      'from sysindexes i '+
            ' join sysindexkeys k on i.id=k.id and i.indid=k.indid '+
            ' join syscolumns c on k.id=c.id and k.colid=c.colid '+
            ' left join sysobjects o on i.name=o.name and i.id=o.parent_obj '+
      'where i.id=object_id(''%s'') '+
      'order by k.indid, k.keyno'
      ,
      //Sybase; http://infocenter.sybase.com/help/index.jsp?topic=/com.sybase.infocenter.help.ase.15.7/title.htm
      'select i.name, i.indid,' +
              'index_col(object_name(i.id),i.indid,c.colid) as col_name,' +
              '(i.status & 2)/2 as IsUnique,' +
              '(i.status & 2048)/2048 as IsPrimaryKey ' +
      'from sysindexes i '+
        ' join syscolumns c on c.id=i.id and c.colid<=i.keycnt-case i.indid when 1 then 0 else 1 end ' +
      'where i.id=object_id(''%s'') '+
        ' and i.indid between 1 and 254 '+ // indid 0 is the table name, 255 is TEXT,IMAGE
      'order by i.indid, c.colid'
      );
var qry : TSQLQuery;
begin
  //if not assigned(Transaction) then
  //  DatabaseError(SErrConnTransactionnSet);

  qry := TSQLQuery.Create(nil);
  qry.Transaction := Transaction;
  qry.Database := Self;
  with qry do
  begin
    ReadOnly := True;
    SQL.Text := format(INDEXES_QUERY[IsSybase], [TableName]);
    Open;
  end;
  while not qry.Eof do with IndexDefs.AddIndexDef do
  begin
    Name := trim(qry.Fields[0].AsString);
    Fields := trim(qry.Fields[2].AsString);
    if qry.Fields[3].AsInteger=1 then Options := Options + [ixUnique];
    if qry.Fields[4].AsInteger=1 then Options := Options + [ixPrimary];
    qry.Next;
    while (Name = trim(qry.Fields[0].AsString)) and (not qry.Eof) do
    begin
      Fields := Fields + ';' + trim(qry.Fields[2].AsString);
      qry.Next;
    end;
  end;
  qry.Close;
  qry.Free;
end;

function TMSSQLConnection.GetSchemaInfoSQL(SchemaType: TSchemaType; SchemaObjectName, SchemaObjectPattern: string): string;
const SCHEMA_QUERY='select id as RECNO, db_name() as CATALOG_NAME, user_name(uid) as SCHEMA_NAME, name as %s '+
                   'from sysobjects '+
                   'where type in (%s) '+
                   'order by name';
begin
  // for simplicity are used only system tables and columns, common to both MS SQL Server and Sybase
  case SchemaType of
    stTables     : Result := format(SCHEMA_QUERY, ['TABLE_NAME, 1 as TABLE_TYPE', '''U''']);
    stSysTables  : Result := format(SCHEMA_QUERY, ['TABLE_NAME, 4 as TABLE_TYPE', '''S''']);
    stProcedures : Result := format(SCHEMA_QUERY, ['PROCEDURE_NAME , case type when ''P'' then 1 else 2 end as PROCEDURE_TYPE', '''P'',''FN'',''IF'',''TF''']);
    stColumns    : Result := 'select colid as RECNO, db_name() as CATALOG_NAME, user_name(uid) as SCHEMA_NAME, o.name as TABLE_NAME,'+
                                    'c.name   as COLUMN_NAME,'+
                                    'colid    as COLUMN_POSITION,'+
                                    '0        as COLUMN_TYPE,'+
                                    'c.type   as COLUMN_DATATYPE,'+
                                    '''''     as COLUMN_TYPENAME,'+
                                    'usertype as COLUMN_SUBTYPE,'+
                                    'prec     as COLUMN_PRECISION,'+
                                    'scale    as COLUMN_SCALE,'+
                                    'length   as COLUMN_LENGTH,'+
                                    'case when c.status&8=8 then 1 else 0 end as COLUMN_NULLABLE '+
                             'from syscolumns c join sysobjects o on c.id=o.id '+
                             'where c.id=object_id(''' + SchemaObjectName + ''') '+
                             'order by colid';
    else           Result := inherited;
  end;
end;

function TMSSQLConnection.GetConnectionInfo(InfoType: TConnInfoType): string;
const
  SERVER_TYPE: array[boolean] of string = ('Microsoft SQL Server', 'ASE'); // product_name returned in TDS login token; same like ODBC SQL_DBMS_NAME
begin
  Result:='';
  try
    InitialiseDBLib(DBLibLibraryName);
    case InfoType of
      citServerType:
        Result:=SERVER_TYPE[IsSybase];
      citServerVersion:
        if Connected then
          Result:=FServerInfo.ServerVersion;
      citServerVersionString:
        if Connected then
          Result:=FServerInfo.ServerVersionString;
      citClientName:
        Result:=TMSSQLConnectionDef.LoadedLibraryName;
    else
      Result:=inherited GetConnectionInfo(InfoType);
    end;
  finally
    ReleaseDBLib;
  end;
end;


{ TMSSQLConnectionDef }

class function TMSSQLConnectionDef.TypeName: String;
begin
   Result:='MSSQLServer';
end;

class function TMSSQLConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
   Result:=TMSSQLConnection;
end;

class function TMSSQLConnectionDef.Description: String;
begin
   Result:='Connect to MS SQL Server via Microsoft client library or via FreeTDS db-lib';
end;

class function TMSSQLConnectionDef.DefaultLibraryName: String;
begin
  Result:=DBLibLibraryName;
end;

class function TMSSQLConnectionDef.LoadFunction: TLibraryLoadFunction;
begin
  Result:=@InitialiseDBLib;
end;

class function TMSSQLConnectionDef.UnLoadFunction: TLibraryUnLoadFunction;
begin
  Result:=@ReleaseDBLib;
end;

class function TMSSQLConnectionDef.LoadedLibraryName: string;
begin
  Result:=DBLibLoadedLibrary;
end;


{ TSybaseConnectionDef }

class function TSybaseConnectionDef.TypeName: String;
begin
  Result:='Sybase';
end;

class function TSybaseConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TSybaseConnection;
end;

class function TSybaseConnectionDef.Description: String;
begin
  Result:='Connect to Sybase SQL Server via FreeTDS db-lib';;
end;


initialization
  RegisterConnection(TMSSQLConnectionDef);
  RegisterConnection(TSybaseConnectionDef);

finalization
  UnRegisterConnection(TMSSQLConnectionDef);
  UnRegisterConnection(TSybaseConnectionDef);

end.

