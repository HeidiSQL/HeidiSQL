{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6Statement;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZDbcInterbase6,
  ZPlainInterbase6, ZDbcInterbase6Utils, ZDbcInterbase6ResultSet,
  ZPlainInterbaseDriver, ZPlainFirebirdInterbaseConstants,
  ZCompatibility, ZDbcLogging, ZVariant, ZMessages;

type

  {** Implements Generic Interbase6 Statement. }
  TZInterbase6Statement = class(TZAbstractStatement)
  private
    FCachedBlob: boolean;
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure CheckInterbase6Error(const Sql: string = '');
  public
    constructor Create(Connection: IZConnection; Info: TStrings);

    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. }
  TZInterbase6PreparedStatement = class(TZAbstractPreparedStatement)
  private
    FCachedBlob: boolean;
    FParamSQLData: IZParamsSQLDA;
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure CheckInterbase6Error(const Sql: string = '');
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);

    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZInterbase6CallableStatement = class(TZAbstractCallableStatement)
  private
    FCachedBlob: boolean;
    FParamSQLData: IZParamsSQLDA;
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure CheckInterbase6Error(const Sql: string = '');
    procedure FetchOutParams(Value: IZResultSQLDA);
    function GetProcedureSql(SelectProc: boolean): string;
    procedure TrimInParameters;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);

    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

implementation

uses ZDbcCachedResultSet, ZSysUtils, ZDbcUtils;

{ TZInterbase6Statement }

{**
   Check interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6Statement.CheckInterbase6Error(const Sql: string = '');
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, lcExecute, SQL);
end;


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6Statement.Create(Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
end;

{**
  Destroys this object and cleanups the memory.
}
{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
{$HINTS OFF}
function TZInterbase6Statement.ExecuteQuery(const SQL: WideString): IZResultSet;
var
  Cursor: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  StmtHandle := nil;

  with FIBConnection do
  begin
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);

    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, SQL, StmtHandle);

//      if not(StatementType in [stSelect, stSelectForUpdate]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
          SQL, StmtHandle, SQLData);

      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, SQLData.GetData);
      CheckInterbase6Error(SQL);

      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;
          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
            @StmtHandle, PChar(Cursor), 0);
          CheckInterbase6Error(SQL);
        end;

        Result := GetCachedResultSet(SQL, Self,
          TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor, SQLData, nil, FCachedBlob));
      end
      else
        raise EZSQLException.Create(SCanNotRetrieveResultSetData);

      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do begin
       FreeStatement(GetPlainDriver, StmtHandle);
       raise;
      end;
    end;
  end;
end;
{$HINTS OFF}

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
{$HINTS OFF}
function TZInterbase6Statement.ExecuteUpdate(const SQL: WideString): Integer;
var
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := -1;
  StmtHandle := nil;
  with FIBConnection do
  begin
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, SQL, StmtHandle);

//      if StatementType in [stExecProc, stSelect, stSelectForUpdate] then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, nil, nil);
      CheckInterbase6Error(SQL);

      case StatementType of
        stCommit, stRollback, stUnknown: Result := -1;
        else begin
          Result := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);
          LastUpdateCount := Result;
        end;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    finally
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;
end;
{$HINTS ON}

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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
{$HINTS OFF}
function TZInterbase6Statement.Execute(const SQL: WideString): Boolean;
var
  Cursor: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  StmtHandle := nil;
  with FIBConnection do
  begin
    try
      Result := False;
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver, GetDBHandle, GetTrHandle,
        GetDialect, SQL, StmtHandle);

      { Check statement type }
//      if not (StatementType in [stExecProc]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      { Create Result SQLData if statement returns result }
      if StatementType = stSelect then
      begin
        SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
        PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
          SQL, StmtHandle, SQLData);
      end;

      { Execute prepared statement }
      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
              @StmtHandle, GetDialect, nil);
      CheckInterbase6Error(Sql);
      { Set updated rows count }
      LastUpdateCount := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);

      case StatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

      { Create ResultSet if possible else free Stateent Handle }
      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;
          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
            @StmtHandle, PChar(Cursor), 0);
          CheckInterbase6Error(SQL);
        end;

        LastResultSet := GetCachedResultSet(SQL, Self,
          TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor,
            SQLData, nil, FCachedBlob));
      end else  begin
        LastResultSet := nil;
        FreeStatement(GetPlainDriver, StmtHandle);
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do begin
       FreeStatement(GetPlainDriver, StmtHandle);
       raise;
      end;
    end;
  end;
end;
{$HINTS ON}

{ TZInterbase6PreparedStatement }

{**
   Check interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6PreparedStatement.CheckInterbase6Error(const Sql: string);
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, lcExecute, SQL);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6PreparedStatement.Create(Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
  with FIBConnection do
    FParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}

function TZInterbase6PreparedStatement.Execute(const SQL: WideString): Boolean;
begin
  Self.SQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
{$HINTS OFF}
function TZInterbase6PreparedStatement.ExecutePrepared: Boolean;
var
  Cursor: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := False;
  StmtHandle := nil;
  with FIBConnection do
  begin
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, SQL, StmtHandle);

//      if not (StatementType in [stExecProc]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      if StatementType in [stSelect, stExecProc] then
      begin
        SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
        PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
          SQL, StmtHandle, SQLData);
      end;

      PrepareParameters(GetPlainDriver, SQL, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, FParamSQLData);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
            GetDialect, FParamSQLData.GetData, nil);
      CheckInterbase6Error(SQL);

      LastUpdateCount := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);

      case StatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

      { Create ResultSet if possible else free Stateent Handle }
      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        Cursor := RandomString(12);
        LastResultSet := GetCachedResultSet(SQL, Self,
          TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor,
            SQLData, nil, FCachedBlob));
      end
      else begin
        LastResultSet := nil;
        FreeStatement(GetPlainDriver, StmtHandle);
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do begin
       FreeStatement(GetPlainDriver, StmtHandle);
       raise;
      end;
    end;
  end;
end;
{$HINTS ON}

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZInterbase6PreparedStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
begin
  Self.SQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$HINTS OFF}
function TZInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  Cursor: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  StmtHandle := nil;
  with FIBConnection do
  begin
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, SQL, StmtHandle);

//      if not(StatementType in [stSelect, stSelectForUpdate]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
        SQL, StmtHandle, SQLData);
      PrepareParameters(GetPlainDriver, SQL, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, FParamSQLData);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
        GetDialect, FParamSQLData.GetData, nil);
      CheckInterbase6Error(SQL);

      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;
          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
            @StmtHandle, PChar(Cursor), 0);
          CheckInterbase6Error(SQL);
        end;  

        Result := GetCachedResultSet(SQL, Self,
          TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor, SQLData, nil, FCachedBlob));
      end
      else
        raise EZSQLException.Create(SCanNotRetrieveResultSetData);

     LastResultSet := Result;
     { Logging SQL Command }
     DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do begin
        FreeStatement(GetPlainDriver, StmtHandle);
        raise;
      end;
    end;
  end;
end;
{$HINTS ON}

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
function TZInterbase6PreparedStatement.ExecuteUpdate(const SQL: WideString): Integer;
begin
  Self.SQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
{$HINTS OFF}
function TZInterbase6PreparedStatement.ExecuteUpdatePrepared: Integer;
var
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := -1;
  StmtHandle := nil;

  with FIBConnection do
  begin
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver, GetDBHandle,
        GetTrHandle, GetDialect, SQL, StmtHandle);

//      if StatementType in [stExecProc, stSelect, stSelectForUpdate] then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      PrepareParameters(GetPlainDriver, SQL, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, FParamSQLData);

      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, FParamSQLData.GetData);
      CheckInterbase6Error(SQL);

      Result := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);
      LastUpdateCount := Result;

      case StatementType of
        stCommit, stRollback, stUnknown: Result := -1;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    finally
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;
end;
{$HINTS ON}


{ TZInterbase6CallableStatement }

{**
   Check interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6CallableStatement.CheckInterbase6Error(const Sql: string);
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, lcExecute, SQL);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6CallableStatement.Create(Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
  with FIBConnection do
    FParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}

function TZInterbase6CallableStatement.Execute(const SQL: WideString): Boolean;
begin
  Self.SQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
{$HINTS OFF}
function TZInterbase6CallableStatement.ExecutePrepared: Boolean;
var
  Cursor, ProcSql: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := False;
  StmtHandle := nil;
  with FIBConnection do
  begin
    TrimInParameters;
    ProcSql := GetProcedureSql(False);
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
    try
      { Prepare statement }
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver, GetDBHandle, GetTrHandle,
        GetDialect, ProcSql, StmtHandle);
      PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
        SQL, StmtHandle, SQLData);
      PrepareParameters(GetPlainDriver, ProcSql, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, FParamSQLData);
      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
            GetDialect, FParamSQLData.GetData, SQLData.GetData);
      CheckInterbase6Error(SQL);

      LastUpdateCount := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);

      case StatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

      { Create ResultSet if possible else free Stateent Handle, ResultSQlData and
        ParamSqlData }
      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        Cursor := RandomString(12);
        LastResultSet := GetCachedResultSet(SQL, Self,
          TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor,
            SQLData, nil, FCachedBlob));
      end else begin
        { Fetch data and fill Output params }
        FetchOutParams(SQLData);
        FreeStatement(GetPlainDriver, StmtHandle);
        LastResultSet := nil;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do begin
       FreeStatement(GetPlainDriver, StmtHandle);
       raise;
      end;
    end;
  end;
end;
{$HINTS ON}

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZInterbase6CallableStatement.ExecuteQuery(
  const SQL: WideString): IZResultSet;
begin
  Self.SQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$HINTS OFF}
function TZInterbase6CallableStatement.ExecuteQueryPrepared: IZResultSet;
var
  Cursor: string;
  ProcSql: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  StmtHandle := nil;

  with FIBConnection do
  begin
    TrimInParameters;
    ProcSql := GetProcedureSql(True);
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, ProcSql, StmtHandle);

//      if not(StatementType in [stSelect, stSelectForUpdate]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
        SQL, StmtHandle, SQLData);
      PrepareParameters(GetPlainDriver, ProcSql, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, FParamSQLData);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
        GetDialect, FParamSQLData.GetData, nil);
      CheckInterbase6Error(ProcSql);

      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;
          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
            @StmtHandle, PChar(Cursor), 0);
          CheckInterbase6Error(ProcSql);
        end;  

        Result := GetCachedResultSet(ProcSql, Self,
          TZInterbase6ResultSet.Create(Self, ProcSql, StmtHandle, Cursor, SQLData, nil, FCachedBlob));
      end;
          
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do begin
        FreeStatement(GetPlainDriver, StmtHandle);
        raise;
      end;
    end;
  end;
end;
{$HINTS ON}

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
function TZInterbase6CallableStatement.ExecuteUpdate(const SQL: WideString): Integer;
begin
  Self.SQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZInterbase6CallableStatement.ExecuteUpdatePrepared: Integer;
var
  ProcSql: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
//  Result := -1;
  StmtHandle := nil;

  with FIBConnection do
  begin
    TrimInParameters;
    ProcSql := GetProcedureSql(False);
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, ProcSql, StmtHandle);

//      if not (StatementType in [stSelect, stSelectForUpdate]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
        SQL, StmtHandle, SQLData);
      PrepareParameters(GetPlainDriver, ProcSql, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, FParamSQLData);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
        GetDialect, FParamSQLData.GetData, SQLData.GetData);
      CheckInterbase6Error(ProcSql);

      Result := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);
      LastUpdateCount := Result;
      { Fetch data and fill Output params }
      FetchOutParams(SQLData);
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do begin
        FreeStatement(GetPlainDriver, StmtHandle);
        raise;
      end;
    end;
  end;
end;

{**
  Set output parameters values from IZResultSQLDA.
  @param Value a IZResultSQLDA object.
}
procedure TZInterbase6CallableStatement.FetchOutParams(
  Value: IZResultSQLDA);
var
  I: Integer;
  Temp: TZVariant;
begin
  SetOutParamCount(Value.GetFieldCount);
  for I := 0 to Value.GetFieldCount-1 do
  begin
    if Value.IsNull(I) then
      DefVarManager.SetNull(Temp)
    else
      case Value.GetFieldSqlType(I) of
      stBoolean:
        DefVarManager.SetAsBoolean(Temp, Value.GetBoolean(I));
      stByte:
        DefVarManager.SetAsInteger(Temp, Value.GetByte(I));
      stShort:
        DefVarManager.SetAsInteger(Temp, Value.GetShort(I));
      stInteger:
        DefVarManager.SetAsInteger(Temp, Value.GetInt(I));
      stLong:
        DefVarManager.SetAsInteger(Temp, Value.GetLong(I));
      stFloat:
        DefVarManager.SetAsFloat(Temp, Value.GetFloat(I));
      stDouble:
        DefVarManager.SetAsFloat(Temp, Value.GetDouble(I));
      stBigDecimal:
        DefVarManager.SetAsFloat(Temp, Value.GetBigDecimal(I));
      stString:
        DefVarManager.SetAsString(Temp, Value.GetString(I));
      stUnicodeString:
//        DefVarManager.SetAsUnicodeString(Temp, Value.GetUnicodeString(I));
        DefVarManager.SetAsUnicodeString(Temp, Value.GetString(I));
      stDate:
        DefVarManager.SetAsDateTime(Temp, Value.GetDate(I));
      stTime:
        DefVarManager.SetAsDateTime(Temp, Value.GetTime(I));
      stTimestamp:
        DefVarManager.SetAsDateTime(Temp, Value.GetTimestamp(I));
    end;
    OutParamValues[I] := Temp;
  end;
end;

{**
   Create sql string for calling stored procedure.
   @param SelectProc indicate use <b>EXECUTE PROCEDURE</b> or
    <b>SELECT</b> staement
   @return a Stored Procedure SQL string 
}
function TZInterbase6CallableStatement.GetProcedureSql(
  SelectProc: boolean): string;

  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    for I := 0 to Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: string;
begin
  InParams := GenerateParamsStr(High(InParamValues) + 1);
  if InParams <> '' then
    InParams := '(' + InParams + ')';

  if SelectProc then
    Result := 'SELECT * FROM ' + SQL + InParams
  else
    Result := 'EXECUTE PROCEDURE ' + SQL + InParams;
end;

{**
   Function remove stUnknown paramters from InParamTypes and InParamValues
}
procedure TZInterbase6CallableStatement.TrimInParameters;
var
  I: integer;
  ParamValues: TZVariantDynArray;
  ParamTypes: TZSQLTypeArray;
  ParamCount: Integer;
begin
  ParamCount := 0;
  SetLength(ParamValues, InParamCount);
  SetLength(ParamTypes, InParamCount);

  for I := 0 to High(InParamTypes) do
  begin
    if InParamTypes[I] = ZDbcIntfs.stUnknown then
     Continue;

    ParamTypes[ParamCount] := InParamTypes[I];
    ParamValues[ParamCount] := InParamValues[I];
    Inc(ParamCount);
  end;
  if ParamCount = InParamCount then
    Exit;
  InParamTypes := ParamTypes;
  InParamValues := ParamValues;
  SetInParamCount(ParamCount);
end;

end.


