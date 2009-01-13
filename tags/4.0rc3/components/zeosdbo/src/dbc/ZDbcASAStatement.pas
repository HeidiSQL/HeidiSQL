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

unit ZDbcASAStatement;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZDbcASA, ZDbcASAUtils,
  ZDbcASAResultSet, ZPlainASADriver, ZCompatibility, ZDbcLogging, ZVariant,
  ZMessages;

type

  {** Implements Generic ASA Statement. }
  TZASAStatement = class(TZAbstractStatement)
  private
    FCachedBlob: Boolean;
    FStmtNum: SmallInt;
    FASAConnection: IZASAConnection;
    FSQLData: IZASASQLDA;
    FMoreResults: Boolean;
  public
    constructor Create(Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    procedure Close; override;
    procedure Cancel; override;
    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
    function GetMoreResults: Boolean; override;
    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. }
  TZASAPreparedStatement = class(TZAbstractPreparedStatement)
  private
    FCachedBlob: boolean;
    FStmtNum: SmallInt;
    FASAConnection: IZASAConnection;
    FParamSQLData: IZASASQLDA;
    FSQLData: IZASASQLDA;
    FMoreResults: Boolean;
    FPrepared: Boolean;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Close; override;
    procedure Cancel; override;
    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
    function GetMoreResults: Boolean; override;
    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZASACallableStatement = class(TZAbstractCallableStatement)
  private
    FCachedBlob: boolean;
    FStmtNum: SmallInt;
    FASAConnection: IZASAConnection;
    FParamSQLData: IZASASQLDA;
    FSQLData: IZASASQLDA;
    FMoreResults: Boolean;
    FPrepared: Boolean;
  protected
    procedure FetchOutParams( Value: IZASASQLDA);
    function GetProcedureSQL: String;
    procedure TrimInParameters;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Close; override;
    procedure Cancel; override;
    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
    function GetMoreResults: Boolean; override;
    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

implementation

uses ZDbcCachedResultSet, ZSysUtils, ZDbcUtils;

{ TZASAStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZASAStatement.Create(Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, Info);

  FASAConnection := Connection as IZASAConnection;
  FetchSize := BlockSize;
  ResultSetConcurrency := rcUpdatable;
  ResultSetType := rtScrollSensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
  CursorName := RandomString(12);
  FSQLData := TZASASQLDA.Create( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, CursorName);
end;

destructor TZASAStatement.Destroy;
begin
  FSQLData := nil;
  inherited;
end;

procedure TZASAStatement.Close;
begin
  if not Closed then
  begin
    FASAConnection.GetPlainDriver.db_close( FASAConnection.GetDBHandle,
      PChar( CursorName));
    Closed := false;
  end;
  if FStmtNum <> 0 then
  begin
    FASAConnection.GetPlainDriver.db_dropstmt( FASAConnection.GetDBHandle, nil,
     nil, @FStmtNum);
    FStmtNum := 0;
  end;
  inherited;
end;

procedure TZASAStatement.Cancel;
begin
  with FASAConnection do
  begin
    GetPlainDriver.db_cancel_request( GetDBHandle);
    ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute);
  end;
end;

function TZASAStatement.GetWarnings: EZSQLWarning;
begin
  Result := inherited GetWarnings;
end;

procedure TZASAStatement.ClearWarnings;
begin
  inherited;
end;

function TZASAStatement.GetMoreResults: Boolean;
var
  SQLData: IZASASQLDA;
begin
  Result := FMoreResults;
  if FMoreResults then
  begin
    with FASAConnection do
    begin
      GetPlainDriver.db_resume( GetDBHandle, PChar( CursorName));
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute);
      if GetDBHandle.sqlcode = SQLE_PROCEDURE_COMPLETE then
        Result := false
      else begin
        SQLData := TZASAResultSet( LastResultSet).SQLData;
        DescribeCursor( FASAConnection, TZASASQLDA( SQLData), CursorName, '');
      end;
    end;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
{$HINTS OFF}
function TZASAStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
var
  Cursor: string;
  CursorOptions: SmallInt;
begin
  Close;

  with FASAConnection do
  begin
    try
      GetPlainDriver.db_prepare_describe( GetDBHandle, nil, @FStmtNum,
        PChar(String(SQL)), FSQLData.GetData, SQL_PREPARE_DESCRIBE_STMTNUM +
        SQL_PREPARE_DESCRIBE_OUTPUT + SQL_PREPARE_DESCRIBE_VARRESULT, 0);
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);

      FMoreResults := GetDBHandle.sqlerrd[2] = 0;
      if not FMoreResults then
      begin
        if FSQLData.GetData^.sqld <= 0 then
          raise EZSQLException.Create( SCanNotRetrieveResultSetData)
        else if ( FSQLData.GetData^.sqld > FSQLData.GetData^.sqln) then
        begin
          FSQLData.AllocateSQLDA( FSQLData.GetData^.sqld);
          GetPlainDriver.db_describe( GetDBHandle, nil, @FStmtNum,
            FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
          ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute,
            SQL);
        end;
        FSQLData.InitFields;
      end;
      if ResultSetConcurrency = rcUpdatable then
        CursorOptions := CUR_OPEN_DECLARE + CUR_UPDATE
      else
        CursorOptions := CUR_OPEN_DECLARE + CUR_READONLY;
      if ResultSetType = rtScrollInsensitive then
        CursorOptions := CursorOptions + CUR_INSENSITIVE;
      Cursor := CursorName;
      GetPlainDriver.db_open( GetDBHandle, PChar( Cursor), nil, @FStmtNum,
        nil, FetchSize, 0, CursorOptions);
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute,
        SQL);
      Closed := false;
      if FMoreResults then
        DescribeCursor( FASAConnection, FSQLData, Cursor, SQL);

      LastUpdateCount := -1;
      Result := GetCachedResultSet( SQL, Self,
        TZASAResultSet.Create( Self, SQL, FStmtNum, Cursor, FSQLData, nil,
        FCachedBlob));

      { Logging SQL Command }
      DriverManager.LogMessage( lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do
      begin
        Self.Close;
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
function TZASAStatement.ExecuteUpdate(const SQL: WideString): Integer;
begin
  Close;
  Result := -1;
  with FASAConnection do
  begin

    GetPlainDriver.db_execute_imm( GetDBHandle, PChar(String(SQL)));
    ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);

    Result := GetDBHandle.sqlErrd[2];
    LastUpdateCount := Result;

    { Logging SQL Command }
    DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
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
function TZASAStatement.Execute(const SQL: WideString): Boolean;
begin
  try
    LastResultSet := ExecuteQuery( SQL);
    Result := true;
  except
    ExecuteUpdate( SQL);
    Result := false;
  end;
end;

{ TZASAPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZASAPreparedStatement.Create(Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FASAConnection := Connection as IZASAConnection;
  FetchSize := BlockSize;
  ResultSetConcurrency := rcUpdatable;
  ResultSetType := rtScrollSensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
  CursorName := RandomString(12);
  FParamSQLData := TZASASQLDA.Create( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, CursorName);
  FSQLData := TZASASQLDA.Create( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, CursorName);
  Prepare( FASAConnection, FSQLData, FParamSQLData, SQL, @FStmtNum, FPrepared,
    FMoreResults);
end;

destructor TZASAPreparedStatement.Destroy;
begin
  FSQLData := nil;
  FParamSQLData := nil;
  inherited;
end;

procedure TZASAPreparedStatement.Close;
begin
  if not Closed then
  begin
    FASAConnection.GetPlainDriver.db_close( FASAConnection.GetDBHandle,
      PChar( CursorName));
    Closed := false;
  end;
  if FStmtNum <> 0 then
  begin
    FASAConnection.GetPlainDriver.db_dropstmt( FASAConnection.GetDBHandle, nil,
     nil, @FStmtNum);
    FStmtNum := 0;
  end;
  inherited;
end;

procedure TZASAPreparedStatement.Cancel;
begin
  with FASAConnection do
  begin
    GetPlainDriver.db_cancel_request( GetDBHandle);
    ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);
  end;
end;

function TZASAPreparedStatement.GetWarnings: EZSQLWarning;
begin
  Result := inherited GetWarnings;
end;

procedure TZASAPreparedStatement.ClearWarnings;
begin
  inherited;
end;

function TZASAPreparedStatement.GetMoreResults: Boolean;
begin
  Result := FMoreResults;
  if FMoreResults then
  begin
    with FASAConnection do
    begin
      GetPlainDriver.db_resume( GetDBHandle, PChar( CursorName));
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute);
      if GetDBHandle.sqlcode = SQLE_PROCEDURE_COMPLETE then
        Result := false
      else
        DescribeCursor( FASAConnection, TZASASQLDA( FSQLData), CursorName, '');
    end;
  end;
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

function TZASAPreparedStatement.Execute(const SQL: WideString): Boolean;
begin
  if Self.SQL <> SQL then
  begin
    Close;
    Self.SQL := SQL;
    Prepare( FASAConnection, FSQLData, FParamSQLData, SQL, @FStmtNum, FPrepared,
      FMoreResults);
  end;
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
function TZASAPreparedStatement.ExecutePrepared: Boolean;
begin
  if FMoreResults or ( FSQLData.GetData.sqld > 0) then
  begin
    LastResultSet := ExecuteQueryPrepared;
    Result := true;
  end else begin
    ExecuteUpdatePrepared;
    Result := false;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZASAPreparedStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
begin
  if Self.SQL <> SQL then
  begin
    Close;
    Self.SQL := SQL;
    Prepare( FASAConnection, FSQLData, FParamSQLData, SQL, @FStmtNum, FPrepared,
      FMoreResults);
  end;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$HINTS OFF}
function TZASAPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  Cursor: string;
  CursorOptions: SmallInt;
begin
  with FASAConnection do
  begin
    PrepareParameters( GetPlainDriver, InParamValues, InParamTypes,
      InParamCount, FParamSQLData);
    if ResultSetConcurrency = rcUpdatable then
      CursorOptions := CUR_OPEN_DECLARE + CUR_UPDATE
    else
      CursorOptions := CUR_OPEN_DECLARE + CUR_READONLY;
    if ResultSetType = rtScrollInsensitive then
      CursorOptions := CursorOptions + CUR_INSENSITIVE;
    Cursor := CursorName;
    GetPlainDriver.db_open( GetDBHandle, PChar( Cursor), nil, @FStmtNum,
      FParamSQLData.GetData, FetchSize, 0, CursorOptions);
    ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute,
      SQL);
    Closed := false;
    try
      if FMoreResults then
        DescribeCursor( FASAConnection, TZASASQLDA( FSQLData), Cursor, '');

      LastUpdateCount := -1;
      Result := GetCachedResultSet( SQL, Self,
        TZASAResultSet.Create( Self, SQL, FStmtNum, Cursor, FSQLData, nil,
        FCachedBlob));

      { Logging SQL Command }
      DriverManager.LogMessage( lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do
      begin
        Self.Close;
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
function TZASAPreparedStatement.ExecuteUpdate(const SQL: WideString): Integer;
begin
  if Self.SQL <> SQL then
  begin
    Close;
    Self.SQL := SQL;
    Prepare( FASAConnection, FSQLData, FParamSQLData, SQL, @FStmtNum, FPrepared,
      FMoreResults);
  end;
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
function TZASAPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  with FASAConnection do
  begin

    PrepareParameters( GetPlainDriver, InParamValues, InParamTypes,
      InParamCount, FParamSQLData);
    GetPlainDriver.db_execute_into( GetDBHandle, nil, nil, @FStmtNum,
      FParamSQLData.GetData, nil);
    if ( GetDBHandle.SqlCode <> -185) or ( FSQLData.GetData^.sqld = 0) then
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);

    Result := GetDBHandle.sqlErrd[2];
    LastUpdateCount := Result;

    { Logging SQL Command }
    DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
  end;
end;
{$HINTS ON}


{ TZASACallableStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Info a statement parameters.
}
constructor TZASACallableStatement.Create(Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FASAConnection := Connection as IZASAConnection;
  FetchSize := BlockSize;
  ResultSetConcurrency := rcUpdatable;
  ResultSetType := rtScrollSensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
  CursorName := RandomString(12);
  FParamSQLData := TZASASQLDA.Create( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, CursorName);
  FSQLData := TZASASQLDA.Create( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, CursorName);
end;

destructor TZASACallableStatement.Destroy;
begin
  FSQLData := nil;
  FParamSQLData := nil;
  inherited;
end;

procedure TZASACallableStatement.Close;
begin
  if not Closed then
  begin
    FASAConnection.GetPlainDriver.db_close( FASAConnection.GetDBHandle,
      PChar( CursorName));
    Closed := false;
  end;
  if FStmtNum <> 0 then
  begin
    FASAConnection.GetPlainDriver.db_dropstmt( FASAConnection.GetDBHandle, nil,
     nil, @FStmtNum);
    FStmtNum := 0;
  end;
  inherited;
end;

procedure TZASACallableStatement.Cancel;
begin
  with FASAConnection do
  begin
    GetPlainDriver.db_cancel_request( GetDBHandle);
    ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);
  end;
end;

function TZASACallableStatement.GetWarnings: EZSQLWarning;
begin
  Result := inherited GetWarnings;
end;

procedure TZASACallableStatement.ClearWarnings;
begin
  inherited;
end;

function TZASACallableStatement.GetMoreResults: Boolean;
begin
  Result := FMoreResults;
  if FMoreResults then
  begin
    with FASAConnection do
    begin
      GetPlainDriver.db_resume( GetDBHandle, PChar( CursorName));
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute);
      if GetDBHandle.sqlcode = SQLE_PROCEDURE_COMPLETE then
        Result := false
      else
        DescribeCursor( FASAConnection, TZASASQLDA( FSQLData), CursorName, '');
    end;
  end;
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

function TZASACallableStatement.Execute(const SQL: WideString): Boolean;
var
  ProcSQL: string;
begin
  TrimInParameters;
  ProcSQL := GetProcedureSQL;
  if not FPrepared or ( Self.SQL <> ProcSQL) then
  begin
    Close;
    Self.SQL := ProcSQL;
    Prepare( FASAConnection, FSQLData, FParamSQLData, Self.SQL, @FStmtNum,
      FPrepared, FMoreResults);
  end;
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
function TZASACallableStatement.ExecutePrepared: Boolean;
begin
  if not FPrepared then
    Result := Execute( SQL)
  else begin
    if FMoreResults or ( ( FSQLData.GetData.sqld > 0) and
      ( FSQLData.GetData.sqlVar[0].sqlInd^ and DT_PROCEDURE_OUT = 0)) then
    begin
      LastResultSet := ExecuteQueryPrepared;
      Result := true;
    end else begin
      ExecuteUpdatePrepared;
      Result := false;
    end;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZASACallableStatement.ExecuteQuery(
  const SQL: WideString): IZResultSet;
var
  ProcSQL: string;
begin
  TrimInParameters;
  ProcSQL := GetProcedureSQL;
  if not FPrepared or ( Self.SQL <> ProcSQL) then
  begin
    Close;
    Self.SQL := ProcSQL;
    Prepare( FASAConnection, FSQLData, FParamSQLData, Self.SQL, @FStmtNum,
      FPrepared, FMoreResults);
  end;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$HINTS OFF}
function TZASACallableStatement.ExecuteQueryPrepared: IZResultSet;
var
  Cursor: string;
  CursorOptions: SmallInt;
begin
  if not FPrepared then
    Result := ExecuteQuery( SQL)
  else begin
    with FASAConnection do
    begin
      PrepareParameters( GetPlainDriver, InParamValues, InParamTypes,
        InParamCount, FParamSQLData);
      if ResultSetConcurrency = rcUpdatable then
        CursorOptions := CUR_OPEN_DECLARE + CUR_UPDATE
      else
        CursorOptions := CUR_OPEN_DECLARE + CUR_READONLY;
      if ResultSetType = rtScrollInsensitive then
        CursorOptions := CursorOptions + CUR_INSENSITIVE;
      Cursor := CursorName;
      GetPlainDriver.db_open( GetDBHandle, PChar( Cursor), nil, @FStmtNum,
        FParamSQLData.GetData, FetchSize, 0, CursorOptions);
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute,
        SQL);
      Closed := false;
      try
        if FMoreResults then
          DescribeCursor( FASAConnection, TZASASQLDA( FSQLData), Cursor, SQL);

        LastUpdateCount := -1;
        Result := GetCachedResultSet( SQL, Self,
          TZASAResultSet.Create( Self, SQL, FStmtNum, Cursor, FSQLData, nil,
          FCachedBlob));

        { Logging SQL Command }
        DriverManager.LogMessage( lcExecute, GetPlainDriver.GetProtocol, SQL);
      except
        on E: Exception do
        begin
          Self.Close;
          raise;
        end;
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
function TZASACallableStatement.ExecuteUpdate(const SQL: WideString): Integer;
var
  ProcSQL: string;
begin
  TrimInParameters;
  ProcSQL := GetProcedureSQL;
  if not FPrepared or ( Self.SQL <> ProcSQL) then
  begin
    Close;
    Self.SQL := ProcSQL;
    Prepare( FASAConnection, FSQLData, FParamSQLData, Self.SQL, @FStmtNum,
      FPrepared, FMoreResults);
  end;
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
function TZASACallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if not FPrepared then
    Result := ExecuteUpdate( SQL)
  else begin
//    Result := -1;
    with FASAConnection do
    begin

      PrepareParameters( GetPlainDriver, InParamValues, InParamTypes,
        InParamCount, FParamSQLData);
      GetPlainDriver.db_execute_into( GetDBHandle, nil, nil, @FStmtNum,
        FParamSQLData.GetData, FSQLData.GetData);
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);

      Result := GetDBHandle.sqlErrd[2];
      LastUpdateCount := Result;
      { Fetch data and fill Output params }
      FetchOutParams( FSQLData);

      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    end;
  end;
end;

{**
  Set output parameters values from IZResultSQLDA.
  @param Value a IZASASQLDA object.
}
procedure TZASACallableStatement.FetchOutParams( Value: IZASASQLDA);
var
  I: Integer;
  L: LongWord;
  Temp: TZVariant;
  TempBlob: IZBlob;
  P: Pointer;
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
        DefVarManager.SetAsUnicodeString(Temp, Value.GetString(I));
      stBytes:
        DefVarManager.SetAsString( Temp, BytesToStr( Value.GetBytes( I)));
      stDate:
        DefVarManager.SetAsDateTime(Temp, Value.GetDate(I));
      stTime:
        DefVarManager.SetAsDateTime(Temp, Value.GetTime(I));
      stTimestamp:
        DefVarManager.SetAsDateTime(Temp, Value.GetTimestamp(I));
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream:
        begin
          GetMem( P, PZASABlobStruct( Value.GetData.sqlvar[I].sqlData).untrunc_len);
          Value.ReadBlobToMem( I, P, L);
          TempBlob := TZASABlob.CreateWithData( P, L);
          DefVarManager.SetAsInterface( Temp, TempBlob);
        end;
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
function TZASACallableStatement.GetProcedureSql: string;

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
  InParams := GenerateParamsStr( High( InParamValues) + 1);
  if InParams <> '' then
    InParams := '(' + InParams + ')';
  Result := 'call ' + SQL + InParams;
end;

{**
   Function remove stUnknown paramters from InParamTypes and InParamValues
}
procedure TZASACallableStatement.TrimInParameters;
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


