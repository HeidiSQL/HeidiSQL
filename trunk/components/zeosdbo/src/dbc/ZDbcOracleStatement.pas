{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcOracleStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging,
  ZPlainOracleDriver, ZCompatibility, ZVariant, ZDbcGenericResolver,
  ZDbcCachedResultSet, ZDbcOracleUtils;

type

  {** Defines a Oracle specific statement. }
  IZOracleStatement = interface(IZStatement)
    ['{8644E5B6-1E0F-493F-B6AC-40D70CCEA13A}']

    function GetStatementHandle: POCIStmt;
  end;

  {** Implements Generic Oracle Statement. }
  TZOracleStatement = class(TZAbstractStatement, IZOracleStatement)
  private
    FPlainDriver: IZOraclePlainDriver;

  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function GetStatementHandle: POCIStmt;
  end;

  {** Implements Prepared SQL Statement. }
  TZOraclePreparedStatement = class(TZAbstractPreparedStatement)
  private
    FPrepared: Boolean;
    FHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FOracleSQL: string;
    FExecStatement: IZStatement;
    FLastStatement: IZStatement;
    FInVars: PZSQLVars;

    procedure SetLastStatement(LastStatement: IZStatement);
    function GetExecStatement: IZStatement;
    function ConvertToOracleSQLQuery(SQL: string): string;

  protected
    property Prepared: Boolean read FPrepared write FPrepared;
    property Handle: POCIStmt read FHandle write FHandle;
    property ErrorHandle: POCIError read FErrorHandle write FErrorHandle;
    property OracleSQL: string read FOracleSQL write FOracleSQL;
    property ExecStatement: IZStatement read FExecStatement write FExecStatement;
    property LastStatement: IZStatement read FLastStatement write SetLastStatement;
    property InVars: PZSQLVars read FInVars write FInVars;

    procedure Prepare; virtual;

  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Close; override;

    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetStatementHandle: POCIStmt;
  end;

implementation

uses
  ZMessages, ZDbcOracle, ZDbcOracleResultSet, ZTokenizer;

{ TZOracleStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZOracleStatement.Create(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZOracleStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
begin
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);

  try
    PrepareOracleStatement(FPlainDriver, SQL, Handle, ErrorHandle);
    Result := CreateOracleResultSet(FPlainDriver, Self, SQL,
      Handle, ErrorHandle);
  except
    FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
    raise;
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
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
function TZOracleStatement.ExecuteUpdate(const SQL: WideString): Integer;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
begin
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);

  try
    PrepareOracleStatement(FPlainDriver, SQL, Handle, ErrorHandle);
    ExecuteOracleStatement(FPlainDriver, Connection, SQL, Handle, ErrorHandle);
    Result := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
  finally
    FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
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
function TZOracleStatement.Execute(const SQL: WideString): Boolean;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
  StatementType: ub2;
begin
  Result := False;
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);

  try
    PrepareOracleStatement(FPlainDriver, SQL, Handle, ErrorHandle);

    StatementType := 0;
    FPlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @StatementType, nil,
      OCI_ATTR_STMT_TYPE, ErrorHandle);

    if StatementType = OCI_STMT_SELECT then
    begin
      LastResultSet := CreateOracleResultSet(FPlainDriver, Self,
        SQL, Handle, ErrorHandle);
      Result := LastResultSet <> nil;
    end
    else
    begin
      ExecuteOracleStatement(FPlainDriver, Connection, SQL,
        Handle, ErrorHandle);
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
    end;
  finally
    if not Result then
    begin
      FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
    end;
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Gets statement handle.
  @return statement handle.
}
function TZOracleStatement.GetStatementHandle: POCIStmt;
begin
  Result := nil;
end;

{ TZOraclePreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZOraclePreparedStatement.Create(
  PlainDriver: IZOraclePlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
  FOracleSQL := ConvertToOracleSQLQuery(SQL);
  FPrepared := False;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOraclePreparedStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Sets a reference to the last statement.
  @param LastStatement the last statement interface.
}
procedure TZOraclePreparedStatement.SetLastStatement(
  LastStatement: IZStatement);
begin
  if FLastStatement <> nil then
    FLastStatement.Close;
  FLastStatement := LastStatement;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZOraclePreparedStatement.GetExecStatement: IZStatement;
begin
  if ExecStatement = nil then
  begin
    ExecStatement := TZOracleStatement.Create(FPlainDriver, Connection, Info);

    ExecStatement.SetMaxFieldSize(GetMaxFieldSize);
    ExecStatement.SetMaxRows(GetMaxRows);
    ExecStatement.SetEscapeProcessing(EscapeProcessing);
    ExecStatement.SetQueryTimeout(GetQueryTimeout);
    ExecStatement.SetCursorName(CursorName);

    ExecStatement.SetFetchDirection(GetFetchDirection);
    ExecStatement.SetFetchSize(GetFetchSize);
    ExecStatement.SetResultSetConcurrency(GetResultSetConcurrency);
    ExecStatement.SetResultSetType(GetResultSetType);
  end;
  Result := ExecStatement;
end;

{**
  Converts an SQL query into Oracle format.
  @param SQL a query with parameters defined with '?'
  @returns a query with parameters in Oracle format ':pN'.
}
function TZOraclePreparedStatement.ConvertToOracleSQLQuery(SQL: string): string;
var
  I, N: Integer;
  Tokens: TStrings;
begin
  if Pos('?', SQL) > 0 then
  begin
    Tokens := Connection.GetDriver.GetTokenizer.
      TokenizeBufferToList(SQL, [toUnifyWhitespaces]);
    try
      Result := '';
      N := 0;
      for I := 0 to Tokens.Count - 1 do
      begin
        if Tokens[I] = '?' then
        begin
          Inc(N);
          Result := Result + ':P' + IntToStr(N);
        end else
          Result := Result + Tokens[I];
      end;
    finally
      Tokens.Free;
    end;
  end else
    Result := SQL;
end;

{**
  Closes this statement and frees all resources.
}
procedure TZOraclePreparedStatement.Close;
begin
  inherited Close;
  if LastStatement <> nil then
  begin
    FLastStatement.Close;
    FLastStatement := nil;
  end;
  FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  FreeOracleSQLVars(FPlainDriver, FInVars);
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
function TZOraclePreparedStatement.Execute(const SQL: WideString): Boolean;
begin
  LastStatement := GetExecStatement;
  Result := LastStatement.Execute(SQL);
  if Result then
    LastResultSet := LastStatement.GetResultSet
  else
    LastUpdateCount := LastStatement.GetUpdateCount;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
begin
  Result := GetExecStatement.ExecuteQuery(SQL);
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
function TZOraclePreparedStatement.ExecuteUpdate(const SQL: WideString): Integer;
begin
  Result := GetExecStatement.ExecuteUpdate(SQL);
  LastUpdateCount := Result;
end;

{**
  Prepares an SQL statement
}
procedure TZOraclePreparedStatement.Prepare;
var
  I: Integer;
  Status: Integer;
  TypeCode: ub2;
  CurrentVar: PZSQLVar;
begin
  if not Prepared then
  begin
    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
    begin
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    end;

    PrepareOracleStatement(FPlainDriver, OracleSQL, Handle, ErrorHandle);
    AllocateOracleSQLVars(FInVars, InParamCount);
    InVars^.ActualNum := InParamCount;

    for I := 0 to InParamCount - 1 do
    begin
      CurrentVar := @FInVars.Variables[I + 1];
      CurrentVar.Handle := nil;

      { Artificially define Oracle internal type. }
      if InParamTypes[I] = stBinaryStream then
        TypeCode := SQLT_BLOB
      else if InParamTypes[I] = stAsciiStream then
        TypeCode := SQLT_CLOB
      else TypeCode := SQLT_STR;

      InitializeOracleVar(FPlainDriver, Connection, CurrentVar,
        InParamTypes[I], TypeCode, 1024);

      Status := FPlainDriver.BindByPos(FHandle, CurrentVar.BindHandle,
        FErrorHandle, I + 1, CurrentVar.Data, CurrentVar.Length,
        CurrentVar.TypeCode, @CurrentVar.Indicator, nil, nil, 0, nil,
        OCI_DEFAULT);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, SQL);
    end;

    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    Prepared := True;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecutePrepared: Boolean;
var
  StatementType: ub2;
begin
  Result := False;

  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, ErrorHandle,
    FInVars, InParamValues);

  StatementType := 0;
  FPlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @StatementType, nil,
    OCI_ATTR_STMT_TYPE, ErrorHandle);

  if StatementType = OCI_STMT_SELECT then
  begin
    { Executes the statement and gets a resultset. }
    LastResultSet := CreateOracleResultSet(FPlainDriver, Self,
      SQL, Handle, ErrorHandle);
    Result := LastResultSet <> nil;
  end
  else
  begin
    { Executes the statement and gets a result. }
    ExecuteOracleStatement(FPlainDriver, Connection, OracleSQL,
      Handle, ErrorHandle);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, OracleSQL);

  { Unloads binded variables with values. }
  UnloadOracleVars(FInVars);

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, ErrorHandle,
    FInVars, InParamValues);

  { Executes the statement and gets a resultset. }
  Result := CreateOracleResultSet(FPlainDriver, Self, SQL,
    Handle, ErrorHandle);

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  { Unloads binded variables with values. }
  UnloadOracleVars(FInVars);
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
function TZOraclePreparedStatement.ExecuteUpdatePrepared: Integer;
var
  StatementType: ub2;
  ResultSet: IZResultSet;
begin
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, ErrorHandle,
    FInVars, InParamValues);

  StatementType := 0;
  FPlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @StatementType, nil,
    OCI_ATTR_STMT_TYPE, ErrorHandle);

  if StatementType = OCI_STMT_SELECT then
  begin
    { Executes the statement and gets a resultset. }
    ResultSet := CreateOracleResultSet(FPlainDriver, Self,
      SQL, Handle, ErrorHandle);
    try
      while ResultSet.Next do;
      LastUpdateCount := ResultSet.GetRow;
    finally
      ResultSet.Close;
    end;
  end
  else
  begin
    { Executes the statement and gets a result. }
    ExecuteOracleStatement(FPlainDriver, Connection, OracleSQL,
      Handle, ErrorHandle);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
  end;
  Result := LastUpdateCount;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, OracleSQL);

  { Unloads binded variables with values. }
  UnloadOracleVars(FInVars);

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Gets statement handle.
  @return statement handle.
}
function TZOraclePreparedStatement.GetStatementHandle: POCIStmt;
begin
  Result := FHandle;
end;

end.
