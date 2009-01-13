{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          DBLib Statement common functionality           }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcDbLibStatement;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZCompatibility, ZClasses, ZSysUtils, ZCollections,
  ZTokenizer, ZDbcIntfs, ZDbcStatement, ZDbcDbLib, ZPlainDbLibDriver;

type
  {** Implements Generic DBLib Statement. }
  TZDBLibStatement = class(TZAbstractStatement)
  protected
    FSQL: string;
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    FRetrievedResultSet: IZResultSet;
    FRetrievedUpdateCount: Integer;

    procedure InternalExecuteStatement(SQL: WideString); virtual;
    procedure FetchResults; virtual;

  public
    constructor Create(Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    function GetMoreResults: Boolean; override;
    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. With emulation}
  TZDBLibPreparedStatementEmulated = class(TZEmulatedPreparedStatement)
  protected
    function GetEscapeString(Value: WideString): WideString;
    function PrepareSQLParam(ParamIndex: Integer): WideString; override;
    function CreateExecStatement: IZStatement; override;
  public
    constructor Create(Connection: IZConnection; SQL: WideString; Info: TStrings);
    function GetMetaData: IZResultSetMetaData; override;
  end;

  TZDBLibCallableStatement = class(TZAbstractCallableStatement)
  private
    FSQL: string;
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    FLastRowsAffected: Integer;//Workaround for sybase
    FRetrievedResultSet: IZResultSet;
    FRetrievedUpdateCount: Integer;

    procedure FetchResults; virtual;
    procedure FetchRowCount; virtual;

  protected
    procedure SetInParamCount(NewParamCount: Integer); override;

  public
    constructor Create(Connection: IZConnection; ProcName: string; Info: TStrings);

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SqlType: Integer); override;
    function GetMoreResults: Boolean; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

  end;

type
  {** Interface for storing counter. }
  IZUpdateCount = interface(IZInterface)
    ['{03219BB4-E07F-4A50-80CD-291FEA629697}']
    procedure SetCount(Value: Integer);
    function GetCount: Integer;
  end;

  TZUpdateCount = class(TInterfacedObject, IZUpdateCount)
  private
    FCount: Integer;
  public
    constructor Create(ACount: Integer);
    procedure SetCount(Value: Integer); virtual;
    function GetCount: Integer; virtual;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

uses
  ZDbcLogging, ZDbcCachedResultSet, ZDbcDbLibUtils, ZDbcDbLibResultSet,
  ZSybaseToken, ZVariant, WideStrUtils;

constructor TZUpdateCount.Create(ACount: Integer);
begin
  inherited Create;
  FCount := ACount;
end;

procedure TZUpdateCount.SetCount(Value: Integer);
begin
  FCount := Value;
end;

function TZUpdateCount.GetCount: Integer;
begin
  Result := FCount;
end;

{ TZDBLibStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
}
constructor TZDBLibStatement.Create(Connection: IZConnection; Info: TStrings);
begin
  inherited Create(Connection, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  if Assigned(FDBLibConnection) then
    FPLainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  FResults := TZCollection.Create;
end;

destructor TZDBLibStatement.Destroy;
begin
  FResults.Clear;
  inherited Destroy;
end;

{**
  Executes a Statement.
  Used internally to execute statements.

  @param Handle a DBLib connection handle.
  @sql string containing the statements to execute
}
procedure TZDBLibStatement.InternalExecuteStatement(SQL: WideString);
begin
  FHandle := FDBLibConnection.GetConnectionHandle;
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  if FPlainDriver.dbcancel(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);
//This one is to avoid a bug in dblib interface as it drops a single backslash before line end
  if FPlainDriver.GetProtocol = 'mssql' then
    SQL := WideStringReplace(Sql, '\'#13, '\\'#13, [rfReplaceAll]);
//This one is to avoid sybase error: Invalid operator for datatype op: is null type: VOID TYPE
  if FPlainDriver.GetProtocol = 'sybase' then
    SQL := WideStringReplace(Sql, ' AND NULL IS NULL', '', [rfReplaceAll]);
  if FPlainDriver.dbcmd(FHandle, PChar(String(SQL))) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);
  if FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);  
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZDBLibStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZUpdateCount;
begin
  Result := False;
  FRetrievedResultSet := nil;
  FRetrievedUpdateCount := -1;
  if FResults.Count > 0 then
  begin
    try
      Result := FResults.Items[0].QueryInterface(IZResultSet, ResultSet) = 0;
      if Result then
      begin
        FRetrievedResultSet := ResultSet;
        FRetrievedUpdateCount := 0;
      end
      else
      begin
        if FResults.Items[0].QueryInterface(IZUpdateCount, UpdateCount) = 0 then
          FRetrievedUpdateCount := UpdateCount.GetCount;
      end;
      FResults.Delete(0);
    finally
      ResultSet := nil;
      UpdateCount := nil;
    end;
  end;
end;

{**
  Fetches all results and creates a cachedresultset object for each resultset
  and a ZUpdateCount object for each count value.
}
procedure TZDBLibStatement.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
  RowsAffected: Integer;
begin
  FResults.Clear;
//Sybase does not seem to return dbCount at all, so a workaround is made
  RowsAffected := -2;
  while FPlainDriver.dbresults(FHandle) = DBSUCCEED do
  begin
    if FPlainDriver.dbcmdrow(FHandle) = DBSUCCEED then
    begin
      NativeResultSet := TZDBLibResultSet.Create(Self, FSQL);
      NativeResultSet.SetConcurrency(rcReadOnly);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet, FSQL, TZDBLibCachedResolver.Create(Self, NativeResultSet.GetMetaData));
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last; CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResults.Add(CachedResultSet);
    end
    else
    begin
      RowsAffected := FPlainDriver.dbCount(FHandle);
      if RowsAffected > -1 then
        FResults.Add(TZUpdateCount.Create(RowsAffected));
    end;
    FPlainDriver.dbCanQuery(FHandle);
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
  if RowsAffected = -1 then
  begin
    FDBLibConnection.InternalExecuteStatement('select @@rowcount');
    try
      FPlainDriver.dbresults(FHandle);
      NativeResultSet := TZDBLibResultSet.Create(Self, 'select @@rowcount');
      try
        if NativeResultset.Next then
          RowsAffected := NativeResultSet.GetInt(1);
      finally
        NativeResultSet.Close;
      end;
      FResults.Add(TZUpdateCount.Create(RowsAffected));
    finally
      FPlainDriver.dbCancel(FHandle);
    end;
    FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZDBLibStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
begin
  Result := nil;
  FSQL := SQL;
  try
    InternalExecuteStatement(SQL);
    FetchResults;
    repeat
      if GetMoreResults then
        Result := FRetrievedResultSet
      else if FRetrievedUpdateCount = -1 then
        Break;
    until False;
  finally
    FRetrievedResultSet := nil;
  end;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZDBLibStatement.ExecuteUpdate(const SQL: WideString): Integer;
begin
  FSQL := SQL;
  InternalExecuteStatement(SQL);
  FetchResults;
  GetMoreResults;
  Result := FRetrievedUpdateCount;
  FRetrievedResultSet := nil;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
}
function TZDBLibStatement.Execute(const SQL: WideString): Boolean;
begin
  FSQL := SQL;
  InternalExecuteStatement(SQL);
  FetchResults;
  Result := GetMoreResults;
  LastResultSet := FRetrievedResultSet;
  LastUpdateCount := FRetrievedUpdateCount;
  FRetrievedResultSet := nil;
end;

{ TZDBLibPreparedStatementEmulated }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZDBLibPreparedStatementEmulated.Create(Connection: IZConnection;
  SQL: WideString; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  ResultSetType := rtScrollInsensitive;
end;

{**
  Converts an string into escape DBLib format.
  @param Value a regular string.
  @return a string in DBLib escape format.
}
function TZDBLibPreparedStatementEmulated.GetEscapeString(Value: WideString): WideString;
begin
  Result := WideQuotedStr(Value, '''');
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZDBLibPreparedStatementEmulated.PrepareSQLParam(
  ParamIndex: Integer): WideString;
begin
  if InParamCount <= ParamIndex then
    Result := 'NULL'
  else
  begin
    Result := PrepareSQLParameter(InParamValues[ParamIndex],
      InParamTypes[ParamIndex]);
  end;
end;

{**
  Gets the number, types and properties of a <code>ResultSet</code>
  object's columns.
  @return the description of a <code>ResultSet</code> object's columns
}
function TZDBLibPreparedStatementEmulated.GetMetaData: IZResultSetMetaData;
begin
  Result := nil;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZDBLibPreparedStatementEmulated.CreateExecStatement: IZStatement;
begin
  Result := TZDBLibStatement.Create(Connection, Info);
end;

constructor TZDBLibCallableStatement.Create(Connection: IZConnection;
  ProcName: string; Info: TStrings);
begin
  inherited Create(Connection, ProcName, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  if Assigned(FDBLibConnection) then
    FPLainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  FResults := TZCollection.Create;
end;

procedure TZDBLibCallableStatement.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
begin
//Sybase does not seem to return dbCount at all, so a workaround is made
  FLastRowsAffected := -2;
  while FPlainDriver.dbresults(FHandle) = DBSUCCEED do
  begin
    if FPlainDriver.dbcmdrow(FHandle) = DBSUCCEED then
    begin
      NativeResultSet := TZDBLibResultSet.Create(Self, FSQL);
      NativeResultSet.SetConcurrency(rcReadOnly);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet, FSQL, TZDBLibCachedResolver.Create(Self, NativeResultSet.GetMetaData));
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last; CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResults.Add(CachedResultSet);
    end
    else
    begin
      FLastRowsAffected := FPlainDriver.dbCount(FHandle);
      if FLastRowsAffected > -1 then
        FResults.Add(TZUpdateCount.Create(FLastRowsAffected));
    end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
end;

procedure TZDBLibCallableStatement.FetchRowCount;
var
  NativeResultSet: TZDBLibResultSet;
begin
//Sybase does not seem to return dbCount at all, so a workaround is made
  if FLastRowsAffected = -1 then
  begin
    FDBLibConnection.InternalExecuteStatement('select @@rowcount');
    try
      FPlainDriver.dbresults(FHandle);
      NativeResultSet := TZDBLibResultSet.Create(Self, 'select @@rowcount');
      try
        if NativeResultset.Next then
          FLastRowsAffected := NativeResultSet.GetInt(1);
      finally
        NativeResultset.Close;
      end;
      FResults.Add(TZUpdateCount.Create(FLastRowsAffected));
    finally
      FPlainDriver.dbCancel(FHandle);
    end;
    FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
  end;
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZDBLibCallableStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZUpdateCount;
begin
  Result := False;
  FRetrievedResultSet := nil;
  FRetrievedUpdateCount := -1;
  if FResults.Count > 0 then
  begin
    try
      Result := FResults.Items[0].QueryInterface(IZResultSet, ResultSet) = 0;
      if Result then
      begin
        FRetrievedResultSet := ResultSet;
        FRetrievedUpdateCount := 0;
      end
      else
        if FResults.Items[0].QueryInterface(IZUpdateCount, UpdateCount) = 0 then
          FRetrievedUpdateCount := UpdateCount.GetCount;
      FResults.Delete(0);
    finally
      ResultSet := nil;
      UpdateCount := nil;
    end;
  end;
end;

function TZDBLibCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if not ExecutePrepared then
    while not GetMoreResults and (FRetrievedUpdateCount <> -1) do;
  Result := FRetrievedResultSet;
  FRetrievedResultSet := nil;
end;

function TZDBLibCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if ExecutePrepared then
    while GetMoreResults and (FRetrievedUpdateCount = -1) do;
  Result := FRetrievedUpdateCount;
  FRetrievedResultSet := nil;
end;

procedure TZDBLibCallableStatement.RegisterOutParameter(ParameterIndex: Integer;
  SqlType: Integer);
begin
  SetOutParamCount(ParameterIndex);
  OutParamTypes[ParameterIndex - 1] := TZSqlType(SqlType);

  //Count inparams must equal count outparams to correct set paramters
  if InParamCount < ParameterIndex then
    SetInParamCount(ParameterIndex);
end;

function TZDBLibCallableStatement.ExecutePrepared: Boolean;
var
  S: string;
  I, ParamIndex, DatLen: Integer;
  RetParam: Byte;
  DatBoolean: Boolean;
  DatByte: Byte;
  DatShort: SmallInt;
  DatInteger: Integer;
  DatFloat: Single;
  DatDouble: Double;
  DatString: string;
  DatMoney: Currency;
  DatDBDATETIME: DBDATETIME;
  DatBytes: TByteDynArray;
  Temp: TZVariant;
  ParamType: TZSQLType;
begin
  FHandle := FDBLibConnection.GetConnectionHandle;
  S := Trim(Sql);
  if FPLainDriver.dbRPCInit(FHandle, PChar(S), 0) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCInit');

  for I := 1 to InParamCount - 1 do//The 0 parameter is the return value
  begin
    RetParam := 0;
    if OutParamTypes[I] <> stUnknown then
      RetParam := DBRPCRETURN;

    ParamType := InParamTypes[I];
    if ParamType = stUnknown then
      ParamType := OutParamTypes[I];

    if DefVarManager.IsNull(InParamValues[I]) and (InParamTypes[I] <> stUnknown) then
    begin
      FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
        ConvertSqlTypeToDBLibType(InParamTypes[I]), -1, 0, nil)
    end
    else
    begin
      case ParamType of
        stBoolean:
          begin
            DatBoolean := SoftVarManager.GetAsBoolean(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLINT1, -1, -1, @DatBoolean);
          end;
        stByte:
          begin
            DatByte := Byte(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
            SQLINT1, -1, -1, @DatByte);
          end;
        stShort:
          begin
            DatShort := SmallInt(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLINT2, -1, -1, @DatShort);
          end;
        stInteger, stLong:
          begin
            DatInteger := Integer(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLINT4, -1, -1, @DatInteger);
          end;
        stFloat:
          begin
            DatFloat := SoftVarManager.GetAsFloat(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLFLT4, -1, -1, @DatFloat);
          end;
        stDouble, stBigDecimal:
          begin
            DatDouble := SoftVarManager.GetAsFloat(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLFLT8, -1, -1, @DatDouble);
          end;
        stString:
          begin
            DatString := SoftVarManager.GetAsString(InParamValues[I]);
            if DatString = ''then
              DatLen := 1
            else
              DatLen := Length(DatString);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLCHAR, MaxInt, DatLen, PChar(DatString));
          end;
        stDate:
          begin
            DatString := FormatDateTime('yyyymmdd',
              SoftVarManager.GetAsDateTime(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLCHAR, MaxInt, Length(DatString), PChar(DatString));
          end;
        stTime:
          begin
            DatString := FormatDateTime('hh":"mm":"ss":"zzz',
              SoftVarManager.GetAsDateTime(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLCHAR, MaxInt, Length(DatString), PChar(DatString));
          end;
        stTimeStamp:
          begin
            DatString := FormatDateTime('yyyymmdd hh":"mm":"ss":"zzz',
              SoftVarManager.GetAsDateTime(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
              SQLCHAR, MaxInt, Length(DatString), PChar(DatString));
          end;
  //      stBytes,
  //      stUnicodeString,
  //      stAsciiStream,
  //      stUnicodeStream,
  //      stBinaryStream
      else
        FPlainDriver.dbRpcParam(FHandle, nil, 0, SQLCHAR, 0, 0, nil);
    end;
  end;
  end;

  if FPLainDriver.dbRpcExec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCExec');
  FetchResults;
  Result := GetMoreResults;

  if FPLainDriver.dbHasRetStat(FHandle) then
    DefVarManager.SetAsInteger(Temp, FPlainDriver.dbRetStatus(FHandle))
  else Temp := NullVariant;
  OutParamValues[0] := Temp; //set function RETURN_VALUE

  ParamIndex := 1;
  for I := 1 to OutParamCount - 1 do
  begin
    if OutParamTypes[I] = stUnknown then
      Continue;
    if FPlainDriver.dbRetData(FHandle, ParamIndex) = nil then
      Temp := NullVariant
    else
    begin
      case FPLainDriver.dbRetType(FHandle, ParamIndex) of
        SQLCHAR, SQLBINARY:
          begin
            DatLen := FPLainDriver.dbRetLen(FHandle, ParamIndex);
            SetLength(DatBytes, DatLen);
            Move(PChar(FPLainDriver.dbRetData(FHandle, ParamIndex))^,
              DatBytes[0], Length(DatBytes));
            DefVarManager.SetAsString(Temp, BytesToStr(DatBytes));
          end;
        SQLINT1:
          DefVarManager.SetAsInteger(Temp,
            PByte(FPlainDriver.dbRetData(FHandle, ParamIndex))^);
        SQLINT2:
          DefVarManager.SetAsInteger(Temp,
            PSmallInt(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        SQLINT4:
          DefVarManager.SetAsInteger(Temp,
            PInteger(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        SQLFLT4:
          DefVarManager.SetAsFloat(Temp,
            PSingle(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        SQLFLT8:
          DefVarManager.SetAsFloat(Temp,
            PDouble(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        SQLMONEY4:
          begin
            FPlainDriver.dbConvert(FHandle, SQLMONEY4,
              FPlainDriver.dbRetData(FHandle, ParamIndex), 4, SQLMONEY,
              PByte(@DatMoney), 8);
            DefVarManager.SetAsFloat(Temp, DatMoney);
          end;
        SQLMONEY:
          DefVarManager.SetAsFloat(Temp,
            PCurrency(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        SQLDECIMAL:
          begin
            FPLainDriver.dbConvert(FHandle, SQLDECIMAL,
              FPLainDriver.dbRetData(FHandle, ParamIndex),
              FPLainDriver.dbRetLen(FHandle, ParamIndex),
              SQLFLT8, PByte(@DatDouble), 8);
            DefVarManager.SetAsFloat(Temp, DatDouble);
          end;
        SQLNUMERIC:
          begin
            FPLainDriver.dbConvert(FHandle, SQLNUMERIC,
              FPLainDriver.dbRetData(FHandle, ParamIndex),
              FPLainDriver.dbRetLen(FHandle, ParamIndex),
              SQLFLT8, PByte(@DatDouble), 8);
            DefVarManager.SetAsFloat(Temp, DatDouble);
          end;
        SQLDATETIM4:
          begin
            FPLainDriver.dbConvert(FHandle, SQLDATETIM4,
              FPLainDriver.dbRetData(FHandle, ParamIndex), 4,
              SQLDATETIME, PByte(@DatDBDATETIME), 8);
            DefVarManager.SetAsDateTime(Temp,
              DatDBDATETIME.dtdays + 2 + (DatDBDATETIME.dttime / 25920000));
          end;
        SQLDATETIME:
          begin
            DatDBDATETIME := PDBDATETIME(
              FPLainDriver.dbRetData(FHandle, ParamIndex))^;
            DefVarManager.SetAsDateTime(Temp,
              DatDBDATETIME.dtdays + 2 + (DatDBDATETIME.dttime / 25920000));
          end;
        else
          Temp := NullVariant;
      end;
    end;
    OutParamValues[I] := Temp;
    Inc(ParamIndex);
  end;

//Workaround for sybase. the dbCount does not work, so a select @@rowcount is
//made but this cleared the returned output parameters, so this is moved here
//after reading the output parameters 
  FetchRowCount;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol,
    Format('EXEC %s', [SQL]));
end;

procedure TZDBLibCallableStatement.SetInParamCount(NewParamCount: Integer);
begin
  inherited SetInParamCount(NewParamCount);

  if OutParamCount < NewParamCount then
    SetOutParamCount(NewParamCount);
end;

end.

