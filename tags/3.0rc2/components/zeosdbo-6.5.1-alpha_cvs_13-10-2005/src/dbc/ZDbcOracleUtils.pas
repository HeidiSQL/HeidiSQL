{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{              Written by Sergey Seroukhov                }
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

unit ZDbcOracleUtils;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZVariant, ZPlainOracleDriver,
  ZDbcLogging, ZCompatibility;

const
  MAX_SQLVAR_LIMIT = 1024;

type
  {** Declares SQL Object }
  TZSQLVar = packed record
    Handle:    POCIHandle;
    Define:    POCIHandle;
    BindHandle: POCIBind;
    Data:      Pointer;
    DupData:   Pointer;
    DataType:  ub2;
    DataSize:  ub2;
    Length:    Integer;
    Precision: Integer;
    Scale:     Integer;
    ColType:   TZSQLType;
    TypeCode:  ub2;
    Indicator: sb2;
    Blob:      IZBlob;
  end;
  PZSQLVar = ^TZSQLVar;

  TZSQLVars = packed record
    AllocNum:  ub4;
    ActualNum: ub4;
    Variables: array[1..MAX_SQLVAR_LIMIT] of TZSQLVar;
  end;
  PZSQLVars = ^TZSQLVars;

{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
}
procedure FreeOracleSQLVars(PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars);

{**
  Allocates in memory and initializes the Oracle variable.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variable an Oracle variable holder.
  @param DataType a DBC data type.
  @param OracleType a correspondent Oracle type.
  @param DataSize a length for string variables.
}
procedure InitializeOracleVar(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Variable: PZSQLVar;
  DataType: TZSQLType; OracleType: ub2; DataSize: Integer);

{**
  Loads Oracle variables binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variables Oracle variable holders.
  @param Values a values to be loaded.
}
procedure LoadOracleVars(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; ErrorHandle: POCIError; Variables: PZSQLVars;
  Values: TZVariantDynArray);

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders.
}
procedure UnloadOracleVars(Variables: PZSQLVars);

{**
  Converts a MySQL native types into ZDBC SQL types.
  @param PlainDriver a native MySQL plain driver.
  @param FieldHandle a handler to field description structure.
  @param FieldFlags field flags.
  @return a SQL undepended type.
}
//function ConvertMySQLHandleToSQLType(PlainDriver: IZMySQLPlainDriver;
//  FieldHandle: PZMySQLField; FieldFlags: Integer): TZSQLType;

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(TypeName: string;
  Size: Integer; Precision: Integer): TZSQLType;

{**
  Converts Oracle internal date into TDateTime
  @param Value a pointer to Oracle internal date.
  @return a decoded TDateTime value.
}
function OraDateToDateTime(Value: PChar): TDateTime;

{**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(PlainDriver: IZOraclePlainDriver;
  ErrorHandle: POCIError; Status: Integer; LogCategory: TZLoggingCategory;
  LogMessage: string);

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; SQL: string; Handle: POCIStmt;
  ErrorHandle: POCIError): IZResultSet;

{**
  Allocates in memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection object.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure AllocateOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError);

{**
  Frees from memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure FreeOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  var Handle: POCIStmt; var ErrorHandle: POCIError);

{**
  Prepares an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure PrepareOracleStatement(PlainDriver: IZOraclePlainDriver;
  SQL: string; Handle: POCIStmt; ErrorHandle: POCIError);

{**
  Executes an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure ExecuteOracleStatement(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; SQL: string; Handle: POCIStmt;
  ErrorHandle: POCIError);

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(PlainDriver: IZOraclePlainDriver;
  Handle: POCIStmt; ErrorHandle: POCIError): ub4;

implementation

uses ZMessages, ZDbcOracle, ZDbcOracleResultSet, ZDbcCachedResultSet,
  ZDbcGenericResolver;

{**
  Calculates size of SQLVars record.
  @param Count a number of variable.
  @returns a record size.
}
function CalculateSQLVarsSize(Count: Integer): Integer;
begin
  Result := SizeOf(TZSQLVars) + Count * SizeOf(TZSQLVar);
end;

{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);
var
  Size: Integer;
begin
  if Variables <> nil then
    FreeMem(Variables);

  Size := CalculateSQLVarsSize(Count);
  GetMem(Variables, Size);
  FillChar(Variables^, Size, 0);
  Variables^.AllocNum := Count;
  Variables^.ActualNum := 0;
end;

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
}
procedure FreeOracleSQLVars(PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars);
var
  I: Integer;
  CurrentVar: PZSQLVar;
begin
  if Variables <> nil then
  begin
    { Frees allocated memory for output variables }
    for I := 1 to Variables.ActualNum do
    begin
      CurrentVar := @Variables.Variables[I];
      if CurrentVar.Data <> nil then
      begin
        if CurrentVar.TypeCode in [SQLT_BLOB, SQLT_CLOB] then
        begin
          PlainDriver.DescriptorFree(PPOCIDescriptor(CurrentVar.Data)^,
            OCI_DTYPE_LOB);
        end
        else if CurrentVar.TypeCode = SQLT_TIMESTAMP then
        begin
          PlainDriver.DescriptorFree(PPOCIDescriptor(CurrentVar.Data)^,
            OCI_DTYPE_TIMESTAMP);
        end;
        FreeMem(CurrentVar.Data);
        CurrentVar.Data := nil;
      end;
    end;

    FreeMem(Variables);
  end;
  Variables := nil;
end;

{**
  Allocates in memory and initializes the Oracle variable.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variable an Oracle variable holder.
  @param DataType a DBC data type.
  @param OracleType a correspondent Oracle type.
  @param DataSize a length for string variables.
}
procedure InitializeOracleVar(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Variable: PZSQLVar;
  DataType: TZSQLType; OracleType: ub2; DataSize: Integer);
var
  Length: Integer;
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  Variable.ColType := DataType;
  Variable.TypeCode := OracleType;
  Variable.DataSize := DataSize;
  Length := 0;
  case Variable.ColType of
    stByte, stShort, stInteger, stLong:
      begin
        Variable.TypeCode := SQLT_INT;
        Length := SizeOf(LongInt);
      end;
    stFloat, stDouble:
      begin
        Variable.TypeCode := SQLT_FLT;
        Length := SizeOf(Double);
      end;
    stDate, stTime, stTimestamp:
      begin
        Variable.TypeCode := SQLT_TIMESTAMP;
        Length := SizeOf(POCIDateTime);
      end;
    stString:
      begin
        Variable.TypeCode := SQLT_STR;
        Length := Variable.DataSize + 1;
      end;
    stAsciiStream, stBinaryStream:
      begin
        if not (Variable.TypeCode in [SQLT_CLOB, SQLT_BLOB]) then
        begin
          if Variable.ColType = stAsciiStream then
            Variable.TypeCode := SQLT_LVC
          else Variable.TypeCode := SQLT_LVB;
          if Variable.DataSize = 0 then
            Length := 128 * 1024 + SizeOf(Integer)
          else Length := Variable.DataSize + SizeOf(Integer);
        end else
          Length := SizeOf(POCILobLocator);
      end;
    stUnknown:
      Exit;
  end;

  Variable.Length := Length;
  GetMem(Variable.Data, Variable.Length);
  if Variable.TypeCode in [SQLT_BLOB, SQLT_CLOB] then
  begin
    PlainDriver.DescriptorAlloc(OracleConnection.GetConnectionHandle,
      PPOCIDescriptor(Variable.Data)^, OCI_DTYPE_LOB, 0, nil);
  end
  else if Variable.TypeCode = SQLT_TIMESTAMP then
  begin
    PlainDriver.DescriptorAlloc(OracleConnection.GetConnectionHandle,
      PPOCIDescriptor(Variable.Data)^, OCI_DTYPE_TIMESTAMP, 0, nil);
  end;
end;

{**
  Loads Oracle variables binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param Variables Oracle variable holders.
  @param Values a values to be loaded.
}
procedure LoadOracleVars(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; ErrorHandle: POCIError; Variables: PZSQLVars;
  Values: TZVariantDynArray);
var
  I: Integer;
  Status: Integer;
  CurrentVar: PZSQLVar;
  TempDate: TDateTime;
  TempBlob: IZBlob;
  WriteTempBlob: IZOracleBlob;
  TempStream: TStream;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  for I := 0 to Variables.ActualNum - 1 do
  begin
    CurrentVar := @Variables.Variables[I + 1];
    CurrentVar.DupData := CurrentVar.Data;
    if DefVarManager.IsNull(Values[I]) then
    begin
      CurrentVar.Indicator := -1;
      CurrentVar.Data := nil;
    end
    else
    begin
      CurrentVar.Indicator := 0;
      case CurrentVar.TypeCode of
        SQLT_INT:
          begin
            PLongInt(CurrentVar.Data)^ :=
              DefVarManager.GetAsInteger(Values[I]);
          end;
        SQLT_FLT:
          begin
            PDouble(CurrentVar.Data)^ :=
              DefVarManager.GetAsFloat(Values[I]);
          end;
        SQLT_STR:
          begin
            StrLCopy(PChar(CurrentVar.Data),
              PChar(DefVarManager.GetAsString(Values[I])), 1024);
          end;
        SQLT_TIMESTAMP:
          begin
            TempDate := DefVarManager.GetAsDateTime(Values[I]);
            DecodeDate(TempDate, Year, Month, Day);
            DecodeTime(TempDate, Hour, Min, Sec, MSec);
            Status := PlainDriver.DateTimeConstruct(
              OracleConnection.GetConnectionHandle,
              ErrorHandle, PPOCIDescriptor(CurrentVar.Data)^,
              Year, Month, Day, Hour, Min, Sec, MSec, nil, 0);
            CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, '');
          end;
        SQLT_BLOB, SQLT_CLOB:
          begin
            TempBlob := DefVarManager.GetAsInterface(Values[I]) as IZBlob;
            TempStream := TempBlob.GetStream;
            try
              WriteTempBlob := TZOracleBlob.Create(PlainDriver,
                nil, 0, Connection, PPOCIDescriptor(CurrentVar.Data)^,
                CurrentVar.ColType);
              WriteTempBlob.SetStream(TempStream);
              WriteTempBlob.CreateBlob;
              WriteTempBlob.WriteBlob;
              CurrentVar.Blob := WriteTempBlob;
            finally
              WriteTempBlob := nil;
              TempStream.Free;
            end;
          end;
      end;
    end;
  end;
end;

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders.
}
procedure UnloadOracleVars(Variables: PZSQLVars);
var
  I: Integer;
  CurrentVar: PZSQLVar;
begin
  for I := 1 to Variables.ActualNum do
  begin
    CurrentVar := @Variables.Variables[I + 1];
    CurrentVar.Blob := nil;
    CurrentVar.Data := CurrentVar.DupData;
  end;
end;

(*

{**
  Converts a MySQL native types into ZDBC SQL types.
  @param PlainDriver a native MySQL plain driver.
  @param FieldHandle a handler to field description structure.
  @param FieldFlags a field flags.
  @return a SQL undepended type.
}
function ConvertMySQLHandleToSQLType(PlainDriver: IZMySQLPlainDriver;
  FieldHandle: PZMySQLField; FieldFlags: Integer): TZSQLType;
begin
  case PlainDriver.GetFieldType(FieldHandle) of
    FIELD_TYPE_TINY:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stByte
        else Result := stShort;
      end;
    FIELD_TYPE_YEAR, FIELD_TYPE_SHORT:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stShort
        else Result := stInteger;
      end;
    FIELD_TYPE_INT24, FIELD_TYPE_LONG:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stInteger
        else Result := stLong;
      end;
    FIELD_TYPE_LONGLONG:
      begin
        if (UNSIGNED_FLAG and FieldFlags) = 0 then
          Result := stLong
        else Result := stBigDecimal;
      end;
    FIELD_TYPE_FLOAT:
      Result := stFloat;
    FIELD_TYPE_DECIMAL:
      begin
        if PlainDriver.GetFieldDecimals(FieldHandle) = 0 then
        begin
          if PlainDriver.GetFieldLength(FieldHandle) < 11 then
            Result := stInteger
          else Result := stLong;
        end else
          Result := stDouble;
      end;
    FIELD_TYPE_DOUBLE:
      Result := stDouble;
    FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
      Result := stDate;
    FIELD_TYPE_TIME:
      Result := stTime;
    FIELD_TYPE_DATETIME, FIELD_TYPE_TIMESTAMP:
      Result := stTimestamp;
    FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB,
    FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB:
      if (FieldFlags and BINARY_FLAG) = 0 then
        Result := stAsciiStream
      else Result := stBinaryStream;
    else
      Result := stString;
  end;
end;
*)

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(TypeName: string;
  Size: Integer; Precision: Integer): TZSQLType;
begin
  TypeName := UpperCase(TypeName);
  Result := stUnknown;

  if (TypeName = 'CHAR') or (TypeName = 'VARCHAR2') then
    Result := stString
  else if (TypeName = 'NCHAR') or (TypeName = 'NVARCHAR2') then
    Result := stUnicodeString
  else if TypeName = 'FLOAT' then
    Result := stDouble
  else if TypeName = 'DATE' then
    Result := stDate
  else if TypeName = 'BLOB' then
    Result := stBinaryStream
  else if (TypeName = 'RAW') or (TypeName = 'LONG RAW') then
    Result := stBinaryStream
  else if TypeName = 'CLOB' then
    Result := stAsciiStream
  else if TypeName = 'NCLOB' then
    Result := stUnicodeStream
  else if TypeName = 'LONG' then
    Result := stAsciiStream
  else if StartsWith(TypeName, 'TIMESTAMP') then
    Result := stTimestamp
  else if TypeName = 'NUMBER' then
  begin
    if Precision = 0 then
    begin
      if Size = 0 then
        Result := stLong
      else if Size <= 2 then
        Result := stByte
      else if Size <= 4 then
        Result := stShort
      else if Size <= 9 then
        Result := stInteger
      else Result := stLong
    end else
      Result := stDouble;
  end;
end;

{**
  Converts Oracle internal date into TDateTime
  @param Value a pointer to Oracle internal date.
  @return a decoded TDateTime value.
}
function OraDateToDateTime(Value: PChar): TDateTime;
type
  TOraDate = array[1..7] of Byte;
  POraDate = ^TOraDate;
var
  Ptr: POraDate;
begin
  Ptr := POraDate(Value);
  Result := EncodeDate((Ptr[1] - 100) * 100 + Ptr[2] - 100, Ptr[3], Ptr[4]) +
    EncodeTime(Ptr[5]-1, Ptr[6]-1, Ptr[7]-1, 0);
end;

{**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(PlainDriver: IZOraclePlainDriver;
  ErrorHandle: POCIError; Status: Integer; LogCategory: TZLoggingCategory;
  LogMessage: string);
var
  ErrorMessage: string;
  ErrorBuffer: array[0..255] of Char;
  ErrorCode: SB4;
begin
  ErrorMessage := '';
  ErrorCode := Status;

  case Status of
    OCI_SUCCESS:
      Exit;
    OCI_SUCCESS_WITH_INFO:
      ErrorMessage := 'OCI_SUCCESS_WITH_INFO';
    OCI_NEED_DATA:
      ErrorMessage := 'OCI_NEED_DATA';
    OCI_NO_DATA:
      ErrorMessage := 'OCI_NO_DATA';
    OCI_ERROR:
      begin
        PlainDriver.ErrorGet(ErrorHandle, 1, nil, ErrorCode, ErrorBuffer, 255,
          OCI_HTYPE_ERROR);
        ErrorMessage := StrPas(ErrorBuffer);
      end;
    OCI_INVALID_HANDLE:
      ErrorMessage := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:
      ErrorMessage := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:
      ErrorMessage := 'OCI_CONTINUE';
  end;

  if (ErrorCode <> OCI_SUCCESS) and (ErrorMessage <> '') then
  begin
    DriverManager.LogError(LogCategory, PlainDriver.GetProtocol, LogMessage,
      ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ErrorMessage]));
  end;
end;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(PlainDriver: IZOraclePlainDriver;
  Statement: IZStatement; SQL: string; Handle: POCIStmt;
  ErrorHandle: POCIError): IZResultSet;
var
  NativeResultSet: TZOracleResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOracleResultSet.Create(PlainDriver, Statement,
    SQL, Handle, ErrorHandle);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (Statement.GetResultSetConcurrency = rcUpdatable)
    or (Statement.GetResultSetType <> rtForwardOnly) then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZGenericCachedResolver.Create(
      Statement, NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end else
    Result := NativeResultSet;
end;

{**
  Allocates in memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection object.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure AllocateOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError);
var
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  ErrorHandle := nil;
  PlainDriver.HandleAlloc(OracleConnection.GetConnectionHandle,
    ErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  Handle := nil;
  PlainDriver.HandleAlloc(OracleConnection.GetConnectionHandle,
    Handle, OCI_HTYPE_STMT, 0, nil);
end;

{**
  Frees from memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure FreeOracleStatementHandles(PlainDriver: IZOraclePlainDriver;
  var Handle: POCIStmt; var ErrorHandle: POCIError);
begin
  if ErrorHandle <> nil then
  begin
    PlainDriver.HandleFree(ErrorHandle, OCI_HTYPE_ERROR);
    ErrorHandle := nil;
  end;
  if Handle <> nil then
  begin
    PlainDriver.HandleFree(Handle, OCI_HTYPE_STMT);
    Handle := nil;
  end;
end;

{**
  Prepares an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure PrepareOracleStatement(PlainDriver: IZOraclePlainDriver;
  SQL: string; Handle: POCIStmt; ErrorHandle: POCIError);
var
  Status: Integer;
begin
  Status := PlainDriver.StmtPrepare(Handle, ErrorHandle, PChar(SQL),
    Length(SQL), OCI_NTV_SYNTAX, OCI_DEFAULT);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcExecute, SQL);
end;

{**
  Executes an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure ExecuteOracleStatement(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; SQL: string; Handle: POCIStmt; ErrorHandle: POCIError);
var
  Status: Integer;
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  Status := PlainDriver.StmtExecute(OracleConnection.GetContextHandle,
    Handle, ErrorHandle, 1, 0, nil, nil, OCI_DEFAULT);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcExecute, SQL);
end;

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(PlainDriver: IZOraclePlainDriver;
  Handle: POCIStmt; ErrorHandle: POCIError): ub4;
begin
  Result := 0;
  PlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @Result, nil,
    OCI_ATTR_ROW_COUNT, ErrorHandle);
end;

end.
