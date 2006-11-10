{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{   Written by Sergey Seroukhov and Sergey Merkuriev      }
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

unit ZDbcMySqlUtils;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZPlainMySqlDriver, ZDbcLogging;

const
  MAXBUF = 65535;

{**
  Converts a MySQL native types into ZDBC SQL types.
  @param PlainDriver a native MySQL plain driver.
  @param FieldHandle a handler to field description structure.
  @param FieldFlags field flags.
  @return a SQL undepended type.
}
function ConvertMySQLHandleToSQLType(PlainDriver: IZMySQLPlainDriver;
  FieldHandle: PZMySQLField; FieldFlags: Integer): TZSQLType;

{**
  Convert string mysql field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertMySQLTypeToSQLType(TypeName, TypeNameFull: string): TZSQLType;

{**
  Converts MySQL Timestamp to TDateTime
  @param Value a timestamp string.
  @return a decoded TDateTime value.
}
function MySQLTimestampToDateTime(Value: string): TDateTime;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(PlainDriver: IZMySQLPlainDriver;
  Handle: PZMySQLConnect; LogCategory: TZLoggingCategory; LogMessage: string);

implementation

uses ZMessages;

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

{**
  Convert string mysql field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertMySQLTypeToSQLType(TypeName, TypeNameFull: string): TZSQLType;
var
  IsUnsigned: Boolean;
  Pos, Len: Integer;
begin
  TypeName := UpperCase(TypeName);
  TypeNameFull := UpperCase(TypeNameFull);
  Result := stUnknown;

  Pos := FirstDelimiter(' ', TypeName);
  if Pos > 0 then
    TypeName := Copy(TypeName, 1, Pos-1);

  if EndsWith(TypeNameFull, 'UNSIGNED') then
    IsUnsigned := True
  else IsUnsigned := False;

  if TypeName = 'TINYINT' then
  begin
    if IsUnsigned then
      Result := stShort
    else Result := stByte;
  end
  else if TypeName = 'YEAR' then
    Result := stShort
  else if TypeName = 'SMALLINT' then
  begin
    if IsUnsigned then
      Result := stInteger
    else Result := stShort;
  end
  else if TypeName = 'MEDIUMINT' then
    Result := stInteger
  else if TypeName = 'INT' then
  begin
    if StartsWith(TypeNameFull, 'INT(10)') or
      StartsWith(TypeNameFull, 'INT(11)') then
    begin
      if IsUnsigned then
        Result := stLong
      else Result := stInteger;
    end else
      Result := stInteger;
  end
  else if TypeName = 'INTEGER' then
  begin
    if IsUnsigned then
      Result := stLong
    else Result := stInteger;
  end
  else if TypeName = 'BIGINT' then
    Result := stLong
  else if TypeName = 'INT24' then
    Result := stLong
  else if TypeName = 'REAL' then
  begin
    if IsUnsigned then
      Result := stDouble
    else Result := stFloat;
  end
  else if TypeName = 'FLOAT' then
  begin
    if IsUnsigned then
      Result := stDouble
    else Result := stFloat;
  end
  else if TypeName = 'DECIMAL' then
  begin
    if EndsWith(TypeNameFull, ',0)') then
    begin
      Len := StrToInt(Copy(TypeNameFull, 9, Length(TypeNameFull) - 11));
      if Len < 10 then
        Result := stInteger
      else Result := stLong;
    end else
      Result := stDouble;
  end
  else if TypeName = 'DOUBLE' then
    Result := stDouble
  else if TypeName = 'CHAR' then
    Result := stString
  else if TypeName = 'VARCHAR' then
    Result := stString
  else if TypeName = 'VARBINARY' then
    Result := stBytes
  else if TypeName = 'BINARY' then
    Result := stBytes
  else if TypeName = 'DATE' then
    Result := stString
  else if TypeName = 'TIME' then
    Result := stString
  else if TypeName = 'TIMESTAMP' then
    Result := stString
  else if TypeName = 'DATETIME' then
    Result := stString
  else if TypeName = 'TINYBLOB' then
    Result := stBinaryStream
  else if TypeName = 'BLOB' then
    Result := stBinaryStream
  else if TypeName = 'MEDIUMBLOB' then
    Result := stBinaryStream
  else if TypeName = 'LONGBLOB' then
    Result := stBinaryStream
  else if TypeName = 'TINYTEXT' then
    Result := stAsciiStream
  else if TypeName = 'TEXT' then
    Result := stAsciiStream
  else if TypeName = 'MEDIUMTEXT' then
    Result := stAsciiStream
  else if TypeName = 'LONGTEXT' then
    Result := stAsciiStream
  else if TypeName = 'ENUM' then
  begin
    if (TypeNameFull = 'ENUM(''Y'',''N'')')
      or (TypeNameFull = 'ENUM(''N'',''Y'')') then
      Result := stBoolean
    else Result := stString;
  end
  else if TypeName = 'SET' then
    Result := stString;
end;

{**
  Converts MySQL Timestamp to TDateTime
  @param Value a timestamp string.
  @return a decoded TDateTime value.
}
function MySQLTimestampToDateTime(Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: Integer;
  StrLength, StrPos: Integer;
begin
  Month := 0;
  Day := 0;
  Hour := 0;
  Min := 0;
  Sec := 0;
  Result := 0;

  StrLength := Length(Value);
  if (StrLength = 14) or (StrLength = 8) then
  begin
    StrPos := 5;
    Year := StrToIntDef(Copy(Value, 1, 4), 0);
  end
  else
  begin
    StrPos := 3;
    Year := StrToIntDef(Copy(Value, 1, 2), 0);
  end;

  if StrLength > 2 then  {Add Month}
  begin
    Month := StrToIntDef(Copy(Value, StrPos, 2), 0);
    if StrLength > 4 then {Add Day}
    begin
      Day := StrToIntDef(Copy(Value, StrPos + 2, 2), 0);
      if StrLength > 6 then {Add Hour}
      begin
        Hour := StrToIntDef(Copy(Value, StrPos + 4, 2), 0);
        if StrLength > 8 then {Add Minute}
        begin
          Min := StrToIntDef(Copy(Value, StrPos + 6, 2), 0);
          if StrLength > 10 then {Add Second}
            Sec := StrToIntDef(Copy(Value, StrPos + 8, 2), 0);
       end;
     end;
   end;
  end;

  if (Year <> 0) and (Month <> 0) and (Day <> 0) then
  begin
    try
      Result := EncodeDate(Year, Month, Day)
    except
    end;
  end;

  try
    Result := Result + EncodeTime(Hour, Min, Sec, 0);
  except
  end;
end;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(PlainDriver: IZMySQLPlainDriver;
  Handle: PZMySQLConnect; LogCategory: TZLoggingCategory; LogMessage: string);
var
  ErrorMessage: string;
  ErrorCode: Integer;
begin
  ErrorMessage := Trim(StrPas(PlainDriver.GetLastError(Handle)));
  ErrorCode := PlainDriver.GetLastErrorCode(Handle);
  if (ErrorCode <> 0) and (ErrorMessage <> '') then
  begin
    DriverManager.LogError(LogCategory, PlainDriver.GetProtocol, LogMessage,
      ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ErrorMessage]));
  end;
end;

end.
