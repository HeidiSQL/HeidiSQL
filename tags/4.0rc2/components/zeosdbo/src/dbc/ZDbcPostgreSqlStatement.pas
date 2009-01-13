{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging,
  ZPlainPostgreSqlDriver, ZCompatibility, ZVariant, ZDbcGenericResolver,
  ZDbcCachedResultSet, ZDbcPostgreSql;

type

  {** Defines a PostgreSQL specific statement. }
  IZPostgreSQLStatement = interface(IZStatement)
    ['{E4FAFD96-97CC-4247-8ECC-6E0A168FAFE6}']

    function IsOidAsBlob: Boolean;
  end;

  {** Implements Generic PostgreSQL Statement. }
  TZPostgreSQLStatement = class(TZAbstractStatement, IZPostgreSQLStatement)
  private
    FHandle: PZPostgreSQLConnect;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FOidAsBlob: Boolean;
  protected
    function CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; Info: TStrings; Handle: PZPostgreSQLConnect);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function IsOidAsBlob: Boolean;
  end;

  {** Implements Prepared SQL Statement. }
  TZPostgreSQLPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: PZPostgreSQLConnect;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FCharactersetCode : TZPgCharactersetType;
  protected
    function CreateExecStatement: IZStatement; override;
    function PrepareSQLParam(ParamIndex: Integer): WideString; override;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: PZPostgreSQLConnect);
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver, IZCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

implementation

uses
  ZMessages, ZDbcPostgreSqlResultSet, ZPostgreSqlToken,
  ZDbcPostgreSqlUtils;

{ TZPostgreSQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLStatement.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Connection: IZConnection; Info: TStrings; Handle: PZPostgreSQLConnect);
begin
  inherited Create(Connection, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  { Processes connection properties. }
  if Self.Info.Values['oidasblob'] <> '' then
    FOidAsBlob := StrToBoolEx(Self.Info.Values['oidasblob'])
  else
    FOidAsBlob := (Connection as IZPostgreSQLConnection).IsOidAsBlob;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Checks is oid should be treated as Large Object.
  @return <code>True</code> if oid should represent a Large Object.
}
function TZPostgreSQLStatement.IsOidAsBlob: Boolean;
begin
  Result := FOidAsBlob;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLStatement.CreateResultSet(const SQL: string;
  QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZPostgreSQLResultSet.Create(FPlainDriver, Self, SQL,
    FHandle, QueryHandle);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end else
    Result := NativeResultSet;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
begin
  Result := nil;
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, PChar(String(SQL)));
  CheckPostgreSQLError(Connection, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  if QueryHandle <> nil then
    Result := CreateResultSet(SQL, QueryHandle)
  else Result := nil;
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
function TZPostgreSQLStatement.ExecuteUpdate(const SQL: WideString): Integer;
var
  QueryHandle: PZPostgreSQLResult;
begin
  Result := -1;
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, PChar(String(SQL)));
  CheckPostgreSQLError(Connection, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(StrPas(FPlainDriver.GetCommandTuples(QueryHandle)), 0);
    FPlainDriver.Clear(QueryHandle);
  end;

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
function TZPostgreSQLStatement.Execute(const SQL: WideString): Boolean;
var
  QueryHandle: PZPostgreSQLResult;
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, PChar(String(SQL)));
  CheckPostgreSQLError(Connection, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.GetResultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        LastResultSet := CreateResultSet(SQL, QueryHandle);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(StrPas(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(StrPas(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
  end;

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{ TZPostgreSQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLPreparedStatement.Create(
  PlainDriver: IZPostgreSQLPlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings; Handle: PZPostgreSQLConnect);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
  FCharactersetCode := (Connection as IZPostgreSQLConnection).GetCharactersetCode;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZPostgreSQLPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZPostgreSQLStatement.Create(FPlainDriver, Connection, Info, FHandle);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLPreparedStatement.PrepareSQLParam(
  ParamIndex: Integer): WideString;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  TempStream: TStream;
  WriteTempBlob: IZPostgreSQLBlob;
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
        if SoftVarManager.GetAsBoolean(Value) then
          Result := 'TRUE'
        else Result := 'FALSE';
      stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
        Result := SoftVarManager.GetAsString(Value);
      stString, stBytes:
        Result := EncodeString(FCharactersetCode,SoftVarManager.GetAsString(Value));
      stDate:
        Result := Format('''%s''::date',
          [FormatDateTime('yyyy-mm-dd', SoftVarManager.GetAsDateTime(Value))]);
      stTime:
        Result := Format('''%s''::time',
          [FormatDateTime('hh":"mm":"ss', SoftVarManager.GetAsDateTime(Value))]);
      stTimestamp:
        Result := Format('''%s''::timestamp',
          [FormatDateTime('yyyy-mm-dd hh":"mm":"ss',
            SoftVarManager.GetAsDateTime(Value))]);
      stAsciiStream, stUnicodeStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then begin
            Result := EncodeString(TempBlob.GetString)
          end else begin
           Result := 'NULL';
          end;
        end;
      stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            if (GetConnection as IZPostgreSQLConnection).IsOidAsBlob then
            begin
              TempStream := TempBlob.GetStream;
              try
                WriteTempBlob := TZPostgreSQLBlob.Create(FPlainDriver,
                  nil, 0, FHandle, 0);
                WriteTempBlob.SetStream(TempStream);
                WriteTempBlob.WriteBlob;
                Result := IntToStr(WriteTempBlob.GetBlobOid);
              finally
                WriteTempBlob := nil;
                TempStream.Free;
              end;
            end else begin
              result:= FPlainDriver.EncodeBYTEA(TempBlob.GetString,FHandle); // FirmOS
              {
               Result := EncodeString(TempBlob.GetString);
               Result := Copy(Result, 2, Length(Result) - 2);
               Result := EncodeString(Result);
              }
            end;
          end else
            Result := 'NULL';
        end;
    end;
  end;
end;

{ TZPostgreSQLCachedResolver }

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZPostgreSQLCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stBinaryStream, stUnicodeStream]);
end;

end.
