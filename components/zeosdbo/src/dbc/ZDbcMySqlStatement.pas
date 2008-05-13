{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZPlainMySqlDriver,
  ZCompatibility, ZDbcLogging, ZVariant;

type

  {** Represents a MYSQL specific connection interface. }
  IZMySQLStatement = interface (IZStatement)
    ['{A05DB91F-1E40-46C7-BF2E-25D74978AC83}']

    function IsUseResult: Boolean;
  end;


  {** Implements Generic MySQL Statement. }
  TZMySQLStatement = class(TZAbstractStatement, IZMySQLStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult: Boolean;

    function CreateResultSet(const SQL: WideString): IZResultSet;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; Info: TStrings; Handle: PZMySQLConnect);

    function ExecuteQuery(const SQL: WideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: WideString): Integer; override;
    function Execute(const SQL: WideString): Boolean; override;

    function IsUseResult: Boolean;
  end;

  {** Implements Prepared SQL Statement. }
  TZMySQLPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
  protected
    function CreateExecStatement: IZStatement; override;
    function GetEscapeString(const Value: WideString): WideString;
    function PrepareSQLParam(ParamIndex: Integer): WideString; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; const SQL: WideString; Info: TStrings;
      Handle: PZMySQLConnect);
  end;

implementation

uses
  ZDbcMySql, ZDbcMySqlUtils, ZDbcMySqlResultSet, ZMySqlToken, ZSysUtils,
  ZMessages, ZDbcCachedResultSet, ZDbcUtils{$IFNDEF VER130BELOW}, DateUtils{$ENDIF};

{ TZMySQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Info a statement parameters.
}
constructor TZMySQLStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; Info: TStrings; Handle: PZMySQLConnect);
var
  MySQLConnection: IZMySQLConnection;
begin
  inherited Create(Connection, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  MySQLConnection := Connection as IZMySQLConnection;
  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLStatement.CreateResultSet(const SQL: WideString): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQLResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    FUseResult);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
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
function TZMySQLStatement.ExecuteQuery(const SQL: WideString): IZResultSet;
begin
  Result := nil;
  if FPlainDriver.ExecQuery(FHandle, PChar(UTF8Encode(SQL))) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
{$IFDEF ENABLE_MYSQL_DEPRECATED}
    if FPlainDriver.GetClientVersion < 32200 then
      begin
        // ResultSetExists is only useable since mysql 3.22
        if FPlainDriver.GetStatus(FHandle) = MYSQL_STATUS_READY then
          raise EZSQLException.Create(SCanNotOpenResultSet);
      end
    else
{$ENDIF ENABLE_MYSQL_DEPRECATED}
    if not FPlainDriver.ResultSetExists(FHandle) then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    Result := CreateResultSet(SQL);
  end else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
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
function TZMySQLStatement.ExecuteUpdate(const SQL: WideString): Integer;
var
  QueryHandle: PZMySQLResult;
  HasResultset : Boolean;
begin
  Result := -1;
  if FPlainDriver.ExecQuery(FHandle, PChar(UTF8Encode(SQL))) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
{$IFDEF ENABLE_MYSQL_DEPRECATED}
    if FPlainDriver.GetClientVersion < 32200 then
      HasResultSet := FPlainDriver.GetStatus(FHandle) <> MYSQL_STATUS_READY
    else
      HasResultSet := FPlainDriver.ResultSetExists(FHandle);
{$ELSE}
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
{$ENDIF ENABLE_MYSQL_DEPRECATED}
    { Process queries with result sets }
    if HasResultSet then
    begin
      QueryHandle := FPlainDriver.StoreResult(FHandle);
      if QueryHandle <> nil then
      begin
        Result := FPlainDriver.GetRowCount(QueryHandle);
        FPlainDriver.FreeResult(QueryHandle);
      end else
        Result := FPlainDriver.GetAffectedRows(FHandle);
    end
    { Process regular query }
    else Result := FPlainDriver.GetAffectedRows(FHandle);
  end else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
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
function TZMySQLStatement.Execute(const SQL: WideString): Boolean;
var
  HasResultset : Boolean;
begin
  Result := False;
  if FPlainDriver.ExecQuery(FHandle, PChar(UTF8Encode(SQL))) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
{$IFDEF ENABLE_MYSQL_DEPRECATED}
    if FPlainDriver.GetClientVersion < 32200 then
      HasResultSet := FPlainDriver.GetStatus(FHandle) <> MYSQL_STATUS_READY
    else
      HasResultSet := FPlainDriver.ResultSetExists(FHandle);
{$ELSE}
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
{$ENDIF ENABLE_MYSQL_DEPRECATED}
    { Process queries with result sets }
    if HasResultSet then
    begin
      Result := True;
      LastResultSet := CreateResultSet(SQL);
    end
    { Processes regular query. }
    else
    begin
      Result := False;
      LastUpdateCount := FPlainDriver.GetAffectedRows(FHandle);
    end;
  end else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
end;

{ TZMySQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL Plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLPreparedStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; const SQL: WideString; Info: TStrings; Handle: PZMySQLConnect);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZMySQLPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZMySQLStatement.Create(FPlainDriver, Connection, Info,FHandle);
end;

{**
  Converts an string into escape MySQL format.
  @param Value a regular string.
  @return a string in MySQL escape format.
}
function TZMySQLPreparedStatement.GetEscapeString(const Value: WideString): WideString;
var
  Bytes: Integer;
  Input: PChar;
  Buffer: PChar;
  Output: PWideChar;
begin
  // UTF-8 strings are at most 4 bytes per character.
  // Include room for a null terminator.
  Bytes := Length(Value) * 4 + 1;
  GetMem(Input, Bytes);

  // Convert WideString input to UTF-8 PChar.
  Bytes := UnicodeToUtf8(Input, Bytes, PWideChar(Value), Length(Value)) - 1;

  // Make room for escaped UTF-8 string.
  GetMem(Buffer, Bytes * 2 + 1);

  // According to the documentation, mysql_real_escape string
  // expects a byte count as input, but returns a length of
  // characters as output.  This is not true, the function
  // actually returns a byte count as output too.  NUL
  // characters are converted to '\0'.
  Bytes := FPlainDriver.GetEscapeString(FHandle, Buffer, Input, Bytes);

  // Make room for wide version of escaped string.
  GetMem(Output, Length(Value) * 4 + 2);

  // Convert back to WideChars, then compose result.
  Utf8ToUnicode(Output, Length(Value) * 2, Buffer, Bytes + 1);
  Result := '''' + WideString(Output) + '''';

  // Free temporary buffers.
  FreeMem(Input);
  FreeMem(Buffer);
  FreeMem(Output);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZMySQLPreparedStatement.PrepareSQLParam(ParamIndex: Integer): WideString;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;

  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value) then
    if (InParamDefaultValues[ParamIndex] <> '') and
      StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
      Result := InParamDefaultValues[ParamIndex]
    else
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
      stUnicodeString:
        Result := GetEscapeString(SoftVarManager.GetAsUnicodeString(Value));
      stDate:
      begin
        {$IFNDEF VER130BELOW}
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        {$ELSE}
        DecodeDate(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay);
        DecodeTime(SoftVarManager.GetAsDateTime(Value),
          AHour, AMinute, ASecond, AMilliSecond);
        {$ENDIF}
        Result := '''' + Format('%0.4d-%0.2d-%0.2d',
          [AYear, AMonth, ADay]) + '''';
      end;
      stTime:
      begin
        {$IFNDEF VER130BELOW}
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        {$ELSE}
        DecodeDate(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay);
        DecodeTime(SoftVarManager.GetAsDateTime(Value),
          AHour, AMinute, ASecond, AMilliSecond);
        {$ENDIF}
        Result := '''' + Format('%0.2d:%0.2d:%0.2d',
          [AHour, AMinute, ASecond]) + '''';
      end;
      stTimestamp:
      begin
        {$IFNDEF VER130BELOW}
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        {$ELSE}
        DecodeDate(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay);
        DecodeTime(SoftVarManager.GetAsDateTime(Value),
          AHour, AMinute, ASecond, AMilliSecond);
        {$ENDIF}
        Result := '''' + Format('%0.4d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d',
          [AYear, AMonth, ADay, AHour, AMinute, ASecond]) + '''';
      end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
            Result := GetEscapeString(TempBlob.GetString)
          else Result := 'NULL';
        end;
    end;
  end;
end;

end.
