/// SQLite3 direct access classes to be used with our SynDB architecture
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynDBSQLite3;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.15
  - first public release, corresponding to mORMot Framework 1.15
  - use the SQLite3 engine, as wrapped via the new separated SynSQLite3 unit

  Version 1.16
  - implemented the SQLite3 private encryption using a password (beta feature -
    better not to be used on production)
  - added RowsToSQLite3() functions for direct export of any DB statement rows
    into a SQLite3 database
  - new TSQLDBSQLite3ConnectionProperties.UseMormotCollations property to
    allow SQL table creation statement with or without the mORMot collations

  Version 1.17
  - now allow compilation with Delphi 5
  - now TSQLDBSQLite3Statement.BindDateTime() will store '' when value is 0,
    or a pure date or a pure time if the value is defined as such - by the
    way, it will match SQlite3 expectations of internal date/time functions, as
    defined at http://www.sqlite.org/lang_datefunc.html
  - fixed TSQLDBSQLite3Statement.Step to update CurrentRow and
    TotalRowsRetrieved properties as expected
  - added TSQLDBSQLite3Connection.Synchronous property
  - code refactoring, especially about error handling and ODBC integration

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
  {$ifndef DELPHI5OROLDER}
  Variants,
  {$endif}
  Classes,
  {$ifndef LVCL}
  Contnrs,
  {$endif}
  SynCommons,
  SynSQLite3,
  SynDB;

{ -------------- SQlite3 databese engine native connection  }

type
  /// will implement properties shared by the static SQLite3 engine
  TSQLDBSQLite3ConnectionProperties = class(TSQLDBConnectionProperties)
  private
    fUseMormotCollations: boolean;
    procedure SetUseMormotCollations(const Value: boolean);
  protected
    fUseCache: boolean;
    /// get all table names of this SQLite3 database
    function SQLGetTableNames: RawUTF8; override;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
  public
    /// initialize the properties
    // - only used parameter is aServerName, which should point to the SQLite3
    // database file to be opened (one will be created if none exists)
    // - you can specify an optional password, which will be used to access
    // the database via some custom kind of encryption - not compatible with the
    // official SQLite Encryption Extension module - see
    // @http://www.hwaci.com/sw/sqlite/see.html (beta feature - better not to
    // be used on production - in which the default encryption level is
    // very low)
    // - other parameters (DataBaseName, UserID) are ignored
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    function NewConnection: TSQLDBConnection; override;
    /// convert an ISO-8601 encoded time and date into a date appropriate to
    // be pasted in the SQL request
    // - returns 'YYYY-MM-DD HH24:MI:SS' i.e. change the in-between 'T' into ' '
    function SQLIso8601ToDate(const Iso8601: RawUTF8): RawUTF8; override;
    /// retrieve the column/field layout of a specified table
    // - this overriden method will call PRAGMA table_info()
    // - used e.g. by GetFieldDefinitions
    procedure GetFields(const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray); override;
    /// TRUE if an internal cache of SQL statement should be used
    // - cache will be accessed for TSQLDBSQLite3Connection.NewStatementPrepared
    // method only (since it will cache prepared statements)
    // - default value is TRUE for faster process (e.g. TTestSQLite3ExternalDB
    // regression tests will be two times faster with statement caching)
    // - will cache only statements containing ? parameters or a SELECT with no
    // WHERE clause within
    property UseCache: boolean read fUseCache write fUseCache;
    /// TRUE if you want the SQL creation fields to use mORMot collation
    // - default value is TRUE for use within the mORMot framework, to use
    // dedicated UTF-8 collation and full Unicode support, and Iso8601 handling
    // - when set to FALSE, SQLCreate() method will return standard ASCII
    // SQLite collations for TEXT: it will be
    property UseMormotCollations: boolean read fUseMormotCollations write SetUseMormotCollations;
  end;

  /// implements a direct connection to the SQLite3 engine
  // - an internal statement cache can be used to speed up the process a lot,
  // if NewStatementPrepared method is called (about two times faster with cache
  // enabled, on an in-memory SQLite3 database)
  TSQLDBSQLite3Connection = class(TSQLDBConnection)
  private
    function GetSynchronous: TSQLSynchronousMode;
    procedure SetSynchronous(Value: TSQLSynchronousMode);
  protected
    fDB: TSQLDataBase;
    fCache: TSQLStatementCached;
  public
    /// release internal memory, handles and statement cache
    destructor Destroy; override;
    /// connect to the SQLite3 engine, i.e. create the DB instance
    // - should raise an Exception on error
    procedure Connect; override;
    /// stop connection to the SQLite3 engine, i.e. release the DB instance
    // - should raise an Exception on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; override;
    /// initialize a new SQL query statement for the given connection
    // - this default implementation will call the NewStatement method
    // - but children may override this method to handle statement caching
    // - this method should return nil in case of error, or a prepared statement
    // instance in case of success
    function NewStatementPrepared(const aSQL: RawUTF8;
      ExpectResults: Boolean): TSQLDBStatement; override; 
    /// begin a Transaction for this connection
    // - current implementation do not support nested transaction with those
    // methods: exception will be raised in such case
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// the associated SQLite3 DB instance
    // - assigned to not nil after successfull connection
    property DB: TSQLDataBase read fDB;
    /// query or change the SQlite3 file-based syncrhonization mode, i.e. the
    // way it waits for the data to be flushed on hard drive
    // - default smFull is very slow, but achieve 100% ACID behavior
    // - smNormal is faster, and safe until a catastrophic hardware failure occurs
    // - smOff is the fastest, data should be safe if the application crashes,
    // but database file may be corrupted in case of failure at the wrong time
    property Synchronous: TSQLSynchronousMode read GetSynchronous write SetSynchronous;
  end;

  /// implements a statement using the SQLite3 engine
  TSQLDBSQLite3Statement = class(TSQLDBStatement)
  protected
    fStatement: TSQLRequest;
    fFromCache: boolean;
    fBindShouldStoreValue: boolean;
    fBindValues: TRawUTF8DynArray;
    {{ retrieve the inlined value of a given parameter, e.g. 1 or 'name' }
    function GetParamValueAsText(Param: integer): RawUTF8; override;
    procedure AfterPrepare;
  public
    {{ create a SQLite3 statement instance, from an existing SQLite3 connection
     - the Execute method can be called once per TSQLDBSQLite3Statement instance,
       but you can use the Prepare once followed by several ExecutePrepared methods
     - if the supplied connection is not of TOleDBConnection type, will raise
       an exception }
    constructor Create(aConnection: TSQLDBConnection); override;
    {{ release all associated memory and SQLite3 handles }
    destructor Destroy; override;

    {{ bind a NULL value to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindNull(Param: Integer; IO: TSQLDBParamInOutType=paramIn); override;
    {{ bind an integer value to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure Bind(Param: Integer; Value: Int64;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a double value to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure Bind(Param: Integer; Value: double;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a TDateTime value to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindDateTime(Param: Integer; Value: TDateTime;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a currency value to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindCurrency(Param: Integer; Value: currency;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a UTF-8 encoded string to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindTextU(Param: Integer; const Value: RawUTF8;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a UTF-8 encoded buffer text (#0 ended) to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindTextP(Param: Integer; Value: PUTF8Char;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a UTF-8 encoded string to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindTextS(Param: Integer; const Value: string;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a UTF-8 encoded string to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindTextW(Param: Integer; const Value: WideString;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a Blob buffer to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindBlob(Param: Integer; Data: pointer; Size: integer;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a Blob buffer to a parameter
     - the leftmost SQL parameter has an index of 1 }
    procedure BindBlob(Param: Integer; const Data: RawByteString;
      IO: TSQLDBParamInOutType=paramIn); overload; override;

    {{ Prepare an UTF-8 encoded SQL statement
     - parameters marked as ? will be bound later, before ExecutePrepared call
     - if ExpectResults is TRUE, then Step() and Column*() methods are available
       to retrieve the data rows
     - raise an ESQLDBException on any error }
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: Boolean=false); overload; override;
    {{ Execute a prepared SQL statement
     - parameters marked as ? should have been already bound with Bind*() functions
     - raise an ESQLDBException on any error }
    procedure ExecutePrepared; override;

    {/ After a statement has been prepared via Prepare() + ExecutePrepared() or
       Execute(), this method must be called one or more times to evaluate it
     - you shall call this method before calling any Column*() methods
     - return TRUE on success, with data ready to be retrieved by Column*()
     - return FALSE if no more row is available (e.g. if the SQL statement
      is not a SELECT but an UPDATE or INSERT command)
     - access the first or next row of data from the SQL Statement result:
       if SeekFirst is TRUE, will put the cursor on the first row of results,
       otherwise, it will fetch one row of data, to be called within a loop
     - raise an ESQLite3Exception exception on any error }
    function Step(SeekFirst: boolean=false): boolean; override;
    {{ retrieve a column name of the current Row
     - Columns numeration (i.e. Col value) starts with 0
     - it's up to the implementation to ensure than all column names are unique }
    function ColumnName(Col: integer): RawUTF8; override;
    {{ returns the Column index of a given Column name
     - Columns numeration (i.e. Col value) starts with 0
     - returns -1 if the Column name is not found (via case insensitive search) }
    function ColumnIndex(const aColumnName: RawUTF8): integer; override;
    {{ the Column type of the current Row
     - ftCurrency type should be handled specificaly, for faster process and
     avoid any rounding issue, since currency is a standard OleDB type }
    function ColumnType(Col: integer; FieldSize: PInteger=nil): TSQLDBFieldType; override;
    {{ Reset the previous prepared statement }
    procedure Reset; override;
    {{ returns TRUE if the column contains NULL }
    function ColumnNull(Col: integer): boolean; override;
    {{ return a Column integer value of the current Row, first Col is 0 }
    function ColumnInt(Col: integer): Int64; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDouble(Col: integer): double; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDateTime(Col: integer): TDateTime; override;
    {{ return a Column currency value of the current Row, first Col is 0
     - should retrieve directly the 64 bit Currency content, to avoid
     any rounding/conversion error from floating-point types }
    function ColumnCurrency(Col: integer): currency; override;
    {{ return a Column UTF-8 encoded text value of the current Row, first Col is 0 }
    function ColumnUTF8(Col: integer): RawUTF8; override;
    {{ return a Column as a blob value of the current Row, first Col is 0
    - ColumnBlob() will return the binary content of the field is was not ftBlob,
      e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
      or a direct mapping of the RawUnicode  }
    function ColumnBlob(Col: integer): RawByteString; override;
    {{ append all columns values of the current Row to a JSON stream
     - will use WR.Expand to guess the expected output format
     - fast overriden implementation with no temporary variable
     - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
       format and contains true BLOB data }
    procedure ColumnsToJSON(WR: TJSONWriter); override;
  end;

/// direct export of a DB statement rows into a SQLite3 database
// - the corresponding table will be created within the specified DB connection
// - transaction will be created if WithinTransaction is set to TRUE
function RowsToSQLite3(Connection: TSQLDBSQLite3Connection; const TableName: RawUTF8;
  Rows: TSQLDBStatement; WithinTransaction: boolean=true): integer; overload;

/// direct export of a DB statement rows into a SQLite3 database
// - the corresponding table will be created within the specified DB file
function RowsToSQLite3(const Dest: TFileName; const TableName: RawUTF8;
  Rows: TSQLDBStatement; UseMormotCollations: boolean): integer; overload;


implementation

function RowsToSQLite3(Connection: TSQLDBSQLite3Connection; const TableName: RawUTF8;
  Rows: TSQLDBStatement; WithinTransaction: boolean): integer;
var Fields: TSQLDBColumnPropertyDynArray;
    aTableName, SQL: RawUTF8;
    Ins: TSQLDBStatement;
begin
  result := 0;
  if (Connection=nil) or (Rows=nil) or (Rows.ColumnCount=0) then
    exit;
  if (PosEx('.',TableName)=0) then
    aTablename := TableName else
    aTableName := '`'+TableName+'`';
  if WithinTransaction then
    Connection.StartTransaction; // MUCH faster within a transaction
  try
    Ins := nil;
    try
      while Rows.Step do begin
        // init when first row of data is available
        if result=0 then begin
          SQL := Rows.ColumnsToSQLInsert(aTableName,Fields); // get layout
          Connection.DB.Execute(Connection.Properties.SQLCreate(aTableName,Fields)); // create table
          Ins := Connection.NewStatementPrepared(SQL,false); // prepare INSERT
        end;
        // write row data
        Ins.BindFromRows(Fields,Rows);
        Ins.ExecutePrepared;
        Ins.Reset;
        inc(result);
      end;
      if WithinTransaction then
        Connection.Commit;
    finally
      Ins.Free;
    end;
  except
    on Exception do begin
      if WithinTransaction then
        Connection.Rollback;
      raise;
    end;
  end;
end;

function RowsToSQLite3(const Dest: TFileName; const TableName: RawUTF8;
  Rows: TSQLDBStatement; UseMormotCollations: boolean): integer; overload;
var DB: TSQLDBSQLite3ConnectionProperties;
    Conn: TSQLDBSQLite3Connection;
begin
  result := 0;
  if (Dest='') or (Rows=nil) or (Rows.ColumnCount=0) then
    exit;
  // we do not call DeleteFile(Dest) since DB may be completed on purpose
  DB := TSQLDBSQLite3ConnectionProperties.Create(StringToUTF8(Dest),'','','');
  try
    DB.UseMormotCollations := UseMormotCollations;
    Conn := DB.MainConnection as TSQLDBSQLite3Connection;
    Conn.Connect;
    result := RowsToSQLite3(Conn,TableName,Rows,true);
  finally
    DB.Free;
  end;
end;

{ TSQLDBSQLite3ConnectionProperties }

procedure TSQLDBSQLite3ConnectionProperties.SetUseMormotCollations(const Value: boolean);
const SQLITE3_FIELDS: array[boolean] of TSQLDBFieldTypeDefinition = (
  (' TEXT',' INTEGER',' FLOAT',' FLOAT',' TEXT',' TEXT',' BLOB'),
  (' TEXT COLLATE SYSTEMNOCASE',' INTEGER',' FLOAT',' FLOAT',
   ' TEXT COLLATE ISO8601',' TEXT COLLATE SYSTEMNOCASE',' BLOB'));
begin
  fUseMormotCollations := Value;
  fSQLCreateField := SQLITE3_FIELDS[Value];
end;

constructor TSQLDBSQLite3ConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  fDBMS := dSQLite;
  inherited Create(aServerName,aDatabaseName,aUserID,aPassWord);
  fUseCache := true;
  UseMormotCollations := true;
end;

procedure TSQLDBSQLite3ConnectionProperties.GetFields(
  const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray);
var n, i: integer;
    F: TSQLDBColumnDefine;
    FA: TDynArray;
begin
  FA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Fields,@n);
  FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
  fillchar(F,sizeof(F),0);
  with Execute('PRAGMA table_info('+aTableName+')',[]) do
  while Step do begin
    // cid,name,type,notnull,dflt_value,pk
    F.ColumnName := ColumnUTF8(1);
    F.ColumnTypeNative := ColumnUTF8(2);
    F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative,0);
    FA.Add(F);
  end;
  with Execute('PRAGMA index_list('+aTableName+')',[]) do
    while Step do
      // seq,name,unique
      with Execute('PRAGMA index_info('+ColumnUTF8(1)+')',[]) do
        while Step do begin
          F.ColumnName := ColumnUTF8(2); // seqno,cid,name
          i := FA.Find(F);
          if i>=0 then
            Fields[i].ColumnIndexed := true;
        end;
  SetLength(Fields,n);
end;

procedure TSQLDBSQLite3ConnectionProperties.GetForeignKeys;
begin
  // do nothing (yet)
end;

function TSQLDBSQLite3ConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBSQLite3Connection.Create(self);
end;

function TSQLDBSQLite3ConnectionProperties.SQLGetTableNames: RawUTF8;
begin
  result := SQL_GET_TABLE_NAMES;
end;

function TSQLDBSQLite3ConnectionProperties.SQLIso8601ToDate(const Iso8601: RawUTF8): RawUTF8;
begin
  result := Iso8601;
  if length(result)>10 then
    result[11] := ' '; // 'T' -> ' '
end;


{ TSQLDBSQLite3Connection }

procedure TSQLDBSQLite3Connection.Commit;
begin
  inherited Commit;
  fDB.Commit;
end;

procedure TSQLDBSQLite3Connection.Connect;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter;
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  fDB := TSQLDatabase.Create(UTF8ToString(Properties.ServerName),Properties.PassWord);
  //fDB.SetWalMode(true); // slower INSERT in WAL mode for huge number of rows
end;

destructor TSQLDBSQLite3Connection.Destroy;
begin
  if fCache.Cache<>nil then
  try
    fCache.ReleaseAllDBStatements;
  except
    on Exception do ; // ignore any error in destructor
  end;
  inherited;
end;

procedure TSQLDBSQLite3Connection.Disconnect;
begin
  FreeAndNil(fDB);
end;

function TSQLDBSQLite3Connection.GetSynchronous: TSQLSynchronousMode;
begin
  if IsConnected then
    result := fDB.Synchronous else
    result := smFull;
end;

function TSQLDBSQLite3Connection.IsConnected: boolean;
begin
  result := (self<>nil) and (fDB<>nil);
end;

function TSQLDBSQLite3Connection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBSQLite3Statement.Create(self);
end;

function TSQLDBSQLite3Connection.NewStatementPrepared(const aSQL: RawUTF8;
  ExpectResults: Boolean): TSQLDBStatement;
function IsCachable(P: PUTF8Char): boolean;
var NoWhere: Boolean;
begin // cachable if with ? parameter or SELECT without WHERE clause
  if P<>nil then begin
    while P^=' ' do inc(P);
    NoWhere := IdemPChar(P,'SELECT ');
    if NoWhere or not (IdemPChar(P,'CREATE ') or IdemPChar(P,'ALTER ')) then begin
      result := true;
      while P^<>#0 do begin
        if P^='"' then begin // ignore chars within quotes
          repeat inc(P) until P^ in [#0,'"'];
          if P^=#0 then break;
        end else
        if P^='?' then
          exit else
        if (P^=' ') and IdemPChar(P+1,'WHERE ') then
          NoWhere := false;
        inc(P);
      end;
    end;
    result := NoWhere;
  end else
    result := false;
end;
begin
  if not fProperties.InheritsFrom(TSQLDBSQLite3ConnectionProperties) or
     not TSQLDBSQLite3ConnectionProperties(fProperties).UseCache then begin
    result := inherited NewStatementPrepared(aSQL,ExpectResults);
    exit;
  end;
  if fCache.Cache=nil then begin
    // intialize the internal statement cache
    if not IsConnected then
      Connect; // we need the DB handle
    fCache.Init(fDB.DB);
  end;
  try
    result := NewStatement;
    if IsCachable(Pointer(aSQL)) then
      // with ? parameter or without WHERE clause -> use statement from cache
      with result as TSQLDBSQLite3Statement do begin
        fSQL := aSQL;
        fExpectResults := ExpectResults;
        fFromCache := true; // so fStatement.Close won't be called
        fStatement := fCache.Prepare(aSQL)^; // copy TSQLRequest content
        AfterPrepare;
      end else
      // cache not enabled or no parameter -> do not cache this statement
      result.Prepare(aSQL,ExpectResults);
  except
    on Exception do
      FreeAndNil(result);
  end;
end;

procedure TSQLDBSQLite3Connection.Rollback;
begin
  inherited;
  fDB.RollBack;
end;

procedure TSQLDBSQLite3Connection.SetSynchronous(Value: TSQLSynchronousMode);
begin
  if self=nil then exit;
  if fDB=nil then
    Connect;
  fDB.Synchronous := Value;
end;

procedure TSQLDBSQLite3Connection.StartTransaction;
begin
  inherited;
  fDB.TransactionBegin;
end;


{ TSQLDBSQLite3Statement }

procedure TSQLDBSQLite3Statement.AfterPrepare;
begin
  fColumnCount := fStatement.FieldCount;
  if fBindShouldStoreValue then begin
    fParamCount := fStatement.ParamCount;
    SetLength(fBindValues,fParamCount);
  end;
end;

procedure TSQLDBSQLite3Statement.Bind(Param: Integer; Value: double;
  IO: TSQLDBParamInOutType);
begin
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    fBindValues[Param-1] := DoubleToStr(Value);
  fStatement.Bind(Param,Value);
end;

procedure TSQLDBSQLite3Statement.Bind(Param: Integer; Value: Int64;
  IO: TSQLDBParamInOutType);
begin
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    fBindValues[Param-1] := Int64ToUtf8(Value);
  fStatement.Bind(Param,Value);
end;

procedure TSQLDBSQLite3Statement.BindBlob(Param: Integer; Data: pointer;
  Size: integer; IO: TSQLDBParamInOutType);
begin
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    fBindValues[Param-1] := '*BLOB*';
  fStatement.Bind(Param,Data,Size);
end;

procedure TSQLDBSQLite3Statement.BindBlob(Param: Integer;
  const Data: RawByteString; IO: TSQLDBParamInOutType);
begin
  fStatement.Bind(Param,pointer(Data),length(Data));
end;

procedure TSQLDBSQLite3Statement.BindCurrency(Param: Integer;
  Value: currency; IO: TSQLDBParamInOutType);
begin
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    fBindValues[Param-1] := Curr64ToStr(PInt64(@Value)^);
  fStatement.Bind(Param,Value);
end;

procedure TSQLDBSQLite3Statement.BindDateTime(Param: Integer;
  Value: TDateTime; IO: TSQLDBParamInOutType);
var tmp: RawUTF8;
begin
  tmp := DateTimeToIso8601Text(Value,'T'); // see http://www.sqlite.org/lang_datefunc.html
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    fBindValues[Param-1] := tmp;
  fStatement.Bind(Param,tmp);
end;

procedure TSQLDBSQLite3Statement.BindNull(Param: Integer;
  IO: TSQLDBParamInOutType);
begin
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    fBindValues[Param-1] := 'NULL';
  fStatement.BindNull(Param);
end;

procedure TSQLDBSQLite3Statement.BindTextP(Param: Integer;
  Value: PUTF8Char; IO: TSQLDBParamInOutType);
var Len: integer;
begin
  Len := StrLen(Value);
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    SetString(fBindValues[Param-1],PAnsiChar(Value),Len);
  sqlite3_check(fStatement.RequestDB,
    sqlite3_bind_text(fStatement.Request,Param,pointer(Value),Len,
    SQLITE_TRANSIENT)); // make private copy of the data
end;

procedure TSQLDBSQLite3Statement.BindTextS(Param: Integer;
  const Value: string; IO: TSQLDBParamInOutType);
begin
  BindTextU(Param,StringToUTF8(Value));
end;

procedure TSQLDBSQLite3Statement.BindTextU(Param: Integer;
  const Value: RawUTF8; IO: TSQLDBParamInOutType);
begin
  if fBindShouldStoreValue and (cardinal(Param-1)<cardinal(fParamCount)) then
    fBindValues[Param-1] := Value;
  fStatement.Bind(Param,Value);
end;

procedure TSQLDBSQLite3Statement.BindTextW(Param: Integer;
  const Value: WideString; IO: TSQLDBParamInOutType);
begin
  BindTextU(Param,WideStringToUTF8(Value));
end;

function TSQLDBSQLite3Statement.ColumnBlob(Col: integer): RawByteString;
begin
  result := fStatement.FieldBlob(Col);
end;

function TSQLDBSQLite3Statement.ColumnCurrency(Col: integer): currency;
begin
  result := fStatement.FieldDouble(Col);
end;

function TSQLDBSQLite3Statement.ColumnDateTime(Col: integer): TDateTime;
var Time: Iso8601;
begin
  case ColumnType(Col) of
  ftUTF8:
    result := Iso8601ToDateTime(fStatement.FieldUTF8(Col));
  ftInt64: begin
    Time.Value := fStatement.FieldInt(Col);
    result := Time.ToDateTime;
  end;
  else result := 0;
  end;
end;

function TSQLDBSQLite3Statement.ColumnDouble(Col: integer): double;
begin
  result := fStatement.FieldDouble(Col);
end;

function TSQLDBSQLite3Statement.ColumnIndex(const aColumnName: RawUTF8): integer;
begin
  result := fStatement.FieldIndex(aColumnName);
end;

function TSQLDBSQLite3Statement.ColumnInt(Col: integer): Int64;
begin
  result := fStatement.FieldInt(Col);
end;

function TSQLDBSQLite3Statement.ColumnName(Col: integer): RawUTF8;
begin
  result := fStatement.FieldName(Col);
end;

function TSQLDBSQLite3Statement.ColumnNull(Col: integer): boolean;
begin
  result := fStatement.FieldNull(Col);
end;

procedure TSQLDBSQLite3Statement.ColumnsToJSON(WR: TJSONWriter);
begin
  fStatement.FieldsToJSON(WR);
end;

function TSQLDBSQLite3Statement.ColumnType(Col: integer; FieldSize: PInteger=nil): TSQLDBFieldType;
begin
  case fStatement.FieldType(Col) of
  SQLITE_NULL:    result := ftNull;
  SQLITE_INTEGER: result := ftInt64;
  SQLITE_FLOAT:   result := ftDouble;
  SQLITE_TEXT:    result := ftUTF8;
  SQLITE_BLOB:    result := ftBlob;
  else            result := ftUnknown;
  end;
  if FieldSize<>nil then
    FieldSize^ := 0; // no column size in SQLite3
end;

function TSQLDBSQLite3Statement.ColumnUTF8(Col: integer): RawUTF8;
begin
  result := fStatement.FieldUTF8(Col);
end;

constructor TSQLDBSQLite3Statement.Create(aConnection: TSQLDBConnection);
begin
  if not aConnection.InheritsFrom(TSQLDBSQLite3Connection) then
    raise ESQLDBException.CreateFmt('%s.Create expects a TSQLDBSQLite3Connection',[ClassName]);
  inherited Create(aConnection);
  if sllSQL in SynDBLog.Family.Level then
    fBindShouldStoreValue := true;
end;

destructor TSQLDBSQLite3Statement.Destroy;
begin
  try
    if not fFromCache then
      fStatement.Close; // release statement
  finally
    inherited Destroy;
  end;
end;

procedure TSQLDBSQLite3Statement.ExecutePrepared;
begin
  if fBindShouldStoreValue then
    SynDBLog.Add.Log(sllSQL,SQLWithInlinedParams,self);
  if not fExpectResults then
    // INSERT/UPDATE/DELETE (i.e. not SELECT) -> try to execute directly now
    repeat // Execute all steps of the first statement
    until fStatement.Step<>SQLITE_ROW;
end;

function TSQLDBSQLite3Statement.GetParamValueAsText(Param: integer): RawUTF8;
begin
  if not fBindShouldStoreValue or (cardinal(Param-1)>=cardinal(fParamCount)) then
    result := '' else
    result := fBindValues[Param-1];
end;

procedure TSQLDBSQLite3Statement.Prepare(const aSQL: RawUTF8;
  ExpectResults: Boolean);
begin
  inherited Prepare(aSQL,ExpectResults); // set fSQL + Connect if necessary
  fStatement.Prepare(TSQLDBSQLite3Connection(Connection).fDB.DB,aSQL);
  AfterPrepare;
end;

procedure TSQLDBSQLite3Statement.Reset;
begin
  fStatement.Reset;
  fStatement.BindReset;
end;

function TSQLDBSQLite3Statement.Step(SeekFirst: boolean): boolean;
begin
  if SeekFirst then begin
    fCurrentRow := 0;
    fStatement.Reset;
  end;
  result := fStatement.Step=SQLITE_ROW;
  if result then begin
    inc(fTotalRowsRetrieved);
    inc(fCurrentRow);
  end else
    fCurrentRow := 0;
end;

end.
