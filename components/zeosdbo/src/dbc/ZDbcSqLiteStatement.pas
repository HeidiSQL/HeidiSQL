{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZDbcSqLiteStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZPlainSqLiteDriver,
  ZCompatibility, ZDbcLogging, ZVariant;

type

  {** Implements Generic SQLite Statement. }
  TZSQLiteStatement = class(TZAbstractStatement)
  private
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;

    function CreateResultSet(SQL: string; StmtHandle: Psqlite_vm;
      ColumnCount: Integer; ColumnNames: PPChar;
      ColumnValues: PPChar): IZResultSet;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver;
      Connection: IZConnection; Info: TStrings; Handle: Psqlite);

    function ExecuteQuery(SQL: string): IZResultSet; override;
    function ExecuteUpdate(SQL: string): Integer; override;
    function Execute(SQL: string): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. }
  TZSQLitePreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;
  protected
    function CreateExecStatement: IZStatement; override;
    function GetEscapeString(Value: string): string;
    function PrepareSQLParam(ParamIndex: Integer): string; override;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver;
      Connection: IZConnection; SQL: string; Info: TStrings;
      Handle: Psqlite);
  end;

implementation

uses
  ZDbcSqLite, ZDbcSqLiteUtils, ZDbcSqLiteResultSet, ZSqLiteToken, ZSysUtils,
  ZMessages, ZDbcCachedResultSet, ZDbcUtils;

{ TZSQLiteStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native SQLite plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Info a statement parameters.
}
constructor TZSQLiteStatement.Create(PlainDriver: IZSQLitePlainDriver;
  Connection: IZConnection; Info: TStrings; Handle: Psqlite);
begin
  inherited Create(Connection, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZSQLiteStatement.CreateResultSet(SQL: string; StmtHandle: Psqlite_vm;
  ColumnCount: Integer; ColumnNames: PPChar; ColumnValues: PPChar): IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  { Creates a native result set. }
  NativeResultSet := TZSQLiteResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    StmtHandle, ColumnCount, ColumnNames, ColumnValues);
  NativeResultSet.SetConcurrency(rcReadOnly);

  { Creates a cached result set. }
  CachedResolver := TZSQLiteCachedResolver.Create(FPlainDriver, FHandle, Self,
    NativeResultSet.GetMetaData);
  CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
    CachedResolver);

  { Fetches all rows to prevent blocking. }
  CachedResultSet.SetType(rtScrollInsensitive);
  CachedResultSet.Last;
  CachedResultSet.BeforeFirst;
  CachedResultSet.SetConcurrency(GetResultSetConcurrency);

  Result := CachedResultSet;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZSQLiteStatement.ExecuteQuery(SQL: string): IZResultSet;
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
  SQLTail: PChar;
  StmtHandle: Psqlite_vm;
  ColumnCount: Integer;
  ColumnValues: PPChar;
  ColumnNames: PPChar;
begin
  ErrorCode := FPlainDriver.Compile(FHandle, PChar(SQL), SQLTail,
    StmtHandle, ErrorMessage);
  CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  try
    ErrorCode := FPlainDriver.Step(StmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther, 'FETCH');
  except
    FPlainDriver.Finalize(StmtHandle, ErrorMessage);
    raise;
  end;

  Result := CreateResultSet(SQL, StmtHandle, ColumnCount, ColumnNames,
    ColumnValues);
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
function TZSQLiteStatement.ExecuteUpdate(SQL: string): Integer;
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
begin
  ErrorCode := FPlainDriver.Execute(FHandle, PChar(SQL), nil, nil,
    ErrorMessage);
  CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  Result := FPlainDriver.Changes(FHandle);
  LastUpdateCount := Result;
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
function TZSQLiteStatement.Execute(SQL: string): Boolean;
var
  ErrorCode: Integer;
  ErrorMessage: PChar;
  SQLTail: PChar;
  StmtHandle: Psqlite_vm;
  ColumnCount: Integer;
  ColumnValues: PPChar;
  ColumnNames: PPChar;
begin
  ErrorCode := FPlainDriver.Compile(FHandle, PChar(SQL), SQLTail,
    StmtHandle, ErrorMessage);
  CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  try
    ErrorCode := FPlainDriver.Step(StmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther, 'FETCH');
  except
    FPlainDriver.Finalize(StmtHandle, ErrorMessage);
    raise;
  end;

  { Process queries with result sets }
  if ColumnCount <> 0 then
  begin
    Result := True;
    LastResultSet := CreateResultSet(SQL, StmtHandle, ColumnCount, ColumnNames,
      ColumnValues);
  end
  { Processes regular query. }
  else
  begin
    Result := False;
    LastUpdateCount := FPlainDriver.Changes(FHandle);
    ErrorCode := FPlainDriver.Finalize(StmtHandle, ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcOther,
      'Finalize SQLite VM');
  end;
end;

{ TZSQLitePreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native SQLite Plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZSQLitePreparedStatement.Create(PlainDriver: IZSQLitePlainDriver;
  Connection: IZConnection; SQL: string; Info: TStrings; Handle: Psqlite);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZSQLitePreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZSQLiteStatement.Create(FPlainDriver, Connection, Info,FHandle);
end;

{**
  Converts an string into escape SQLite format.
  @param Value a regular string.
  @return a string in SQLite escape format.
}
function TZSQLitePreparedStatement.GetEscapeString(Value: string): string;
begin
  Result := AnsiQuotedStr(Value, '''');
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZSQLitePreparedStatement.PrepareSQLParam(ParamIndex: Integer): string;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value)  then
    Result := 'NULL'
  else begin
    case InParamTypes[ParamIndex] of
      stBoolean:
        if SoftVarManager.GetAsBoolean(Value) then Result := '''Y'''
        else Result := '''N''';
      stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
        Result := SoftVarManager.GetAsString(Value);
      stString, stBytes:
        Result := GetEscapeString(SoftVarManager.GetAsString(Value));
      stDate:
        Result := '''' + FormatDateTime('yyyy-mm-dd',
          SoftVarManager.GetAsDateTime(Value)) + '''';
      stTime:
        Result := '''' + FormatDateTime('hh:mm:ss',
          SoftVarManager.GetAsDateTime(Value)) + '''';
      stTimestamp:
        Result := '''' + FormatDateTime('yyyy-mm-dd hh:mm:ss',
          SoftVarManager.GetAsDateTime(Value)) + '''';
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            if InParamTypes[ParamIndex] = stBinaryStream then
              Result := EncodeString(TempBlob.GetString)
            else Result := GetEscapeString(TempBlob.GetString);
          end else
            Result := 'NULL';
        end;
    end;
  end;
end;

end.
