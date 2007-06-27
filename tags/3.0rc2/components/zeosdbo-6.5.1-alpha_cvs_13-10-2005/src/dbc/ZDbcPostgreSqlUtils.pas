{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlUtils;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZDbcIntfs, ZPlainPostgreSqlDriver, ZDbcResultSetMetadata,
  ZDbcPostgreSql, ZDbcLogging;

{**
  Indicate what field type is a number (integer, float and etc.)
  @param  the SQLType field type value
  @result true if field type number
}
function IsNumber(Value: TZSQLType): Boolean;

{**
  Remove qoutes from the string
  @param  the quoted string
  @result the string without qoutes
}
function EscapeQuotes(const Value: string): string;

{**
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param The TypeName is PostgreSQL type name
   @return The ZSQLType type
}
function PostgreSQLToSQLType(Connection: IZPostgreSQLConnection;
  TypeName: string): TZSQLType; overload;

{**
    Another version of PostgreSQLToSQLType()
      - comparing integer should be faster than AnsiString?
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param TypeOid is PostgreSQL type OID
   @return The ZSQLType type
}
function PostgreSQLToSQLType(Connection: IZPostgreSQLConnection;
  TypeOid: Integer): TZSQLType; overload;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeString(Value: string): string;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeString(Value: string): string;

{**
  Checks for possible sql errors.
  @param Connection a reference to database connection to execute Rollback.
  @param PlainDriver a PostgreSQL plain driver.
  @param Handle a PostgreSQL connection reference.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckPostgreSQLError(Connection: IZConnection;
  PlainDriver: IZPostgreSQLPlainDriver;
  Handle: PZPostgreSQLConnect; LogCategory: TZLoggingCategory;
  LogMessage: string);

{**
   Resolve problem with minor version in PostgreSql bettas
   @param Value a minor version string like "4betta2"
   @return a miror version number
}
function GetMinorVersion(Value: string): Word;

implementation

uses ZMessages;

{**
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param The TypeName is PostgreSQL type name
   @return The ZSQLType type
}
function PostgreSQLToSQLType(Connection: IZPostgreSQLConnection;
  TypeName: string): TZSQLType;
begin
  TypeName := LowerCase(TypeName);
  if (TypeName = 'interval') or (TypeName = 'char')
    or (TypeName = 'varchar') then
    Result := stString
  else if TypeName = 'text' then
    Result := stAsciiStream
  else if TypeName = 'oid' then
  begin
    if Connection.IsOidAsBlob() then
      Result := stBinaryStream
    else Result := stInteger;
  end
  else if TypeName = 'name' then
    Result := stString
  else if TypeName = 'cidr' then
    Result := stString
  else if TypeName = 'inet' then
    Result := stString
  else if TypeName = 'macaddr' then
    Result := stString
  else if TypeName = 'int2' then
    Result := stShort
  else if TypeName = 'int4' then
    Result := stInteger
  else if TypeName = 'int8' then
    Result := stLong
  else if TypeName = 'float4' then
    Result := stFloat
  else if (TypeName = 'float8') or (TypeName = 'decimal')
    or (TypeName = 'numeric') then
    Result := stDouble
  else if TypeName = 'money' then
    Result := stDouble
  else if TypeName = 'bool' then
    Result := stBoolean
  else if TypeName = 'date' then
    Result := stDate
  else if TypeName = 'time' then
    Result := stTime
  else if (TypeName = 'datetime') or (TypeName = 'timestamp')
    or (TypeName = 'timestamptz') or (TypeName = 'abstime') then
    Result := stTimestamp
  else if TypeName = 'regproc' then
    Result := stString
  else if TypeName = 'bytea' then
  begin
    if Connection.IsOidAsBlob then
      Result := stBytes
    else Result := stBinaryStream;
  end
  else if TypeName = 'bpchar' then
    Result := stString
  else if (TypeName = 'int2vector') or (TypeName = 'oidvector')
    or (TypeName = '_aclitem') then
    Result := stAsciiStream
  else
    Result := stUnknown;
end;

{**
   Another version of PostgreSQLToSQLType()
     - comparing integer should be faster than AnsiString.
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param TypeOid is PostgreSQL type OID
   @return The ZSQLType type
}
function PostgreSQLToSQLType(Connection: IZPostgreSQLConnection;
  TypeOid: Integer): TZSQLType; overload;
begin
  case TypeOid of
    1186,18,1043: Result := stString; { interval/char/varchar }
    25: Result := stAsciiStream; { text }
    26: { oid }
      begin
        if Connection.IsOidAsBlob() then
          Result := stBinaryStream
        else Result := stInteger;
      end;
    19: Result := stString; { name }
    21: Result := stShort; { int2 }
    23: Result := stInteger; { int4 }
    20: Result := stLong; { int8 }
    700: Result := stFloat; { float4 }
    701,1700: Result := stDouble; { float8/numeric. no 'decimal' any more }
    790: Result := stFloat; { money }
    16: Result := stBoolean; { bool }
    1082: Result := stDate; { date }
    1083: Result := stTime; { time }
    1114,1184,702: Result := stTimestamp; { timestamp,timestamptz/abstime. no 'datetime' any more}
    24: Result := stString; { regproc }
    17: { bytea }
      begin
        if Connection.IsOidAsBlob then
          Result := stBytes
        else Result := stBinaryStream;
      end;
    1042: Result := stString; { bpchar }
    22,30: Result := stAsciiStream; { int2vector/oidvector. no '_aclitem' }
    else
      Result := stUnknown;
  end;
end;

{**
  Indicate what field type is a number (integer, float and etc.)
  @param  the SQLType field type value
  @result true if field type number
}
function IsNumber(Value: TZSQLType): Boolean;
begin
  Result := Value in [stByte, stShort, stInteger, stLong,
    stFloat, stDouble, stBigDecimal];
end;

{**
  Remove qoutes from the string
  @param  the quoted string
  @result the string without qoutes
}
function EscapeQuotes(const Value: string): string;
var
  I: Integer;
  PrevChar, PrevPrevChar: string;
begin
  Result := '';
  PrevChar := ' ';
  PrevPrevChar := ' ';
  for I := 1 to Length(Value) do
  begin
    Result := Result + Value[I];
    if (Value[I] = '''') and ((PrevChar <> '\') or
       (PrevChar = '\') or (PrevPrevChar = '\')) then
      Result := Result + '''';
    PrevPrevChar := PrevChar;
    PrevChar := Value[I];
  end;
end;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeString(Value: string): string;
var
  I: Integer;
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := PChar(Value);
  DestLength := 2;
  for I := 1 to SrcLength do
  begin
    if SrcBuffer^ in [#0, '''', '\'] then
      Inc(DestLength, 4)
    else Inc(DestLength);
    Inc(SrcBuffer);
  end;

  SrcBuffer := PChar(Value);
  SetLength(Result, DestLength);
  DestBuffer := PChar(Result);
  DestBuffer^ := '''';
  Inc(DestBuffer);

  for I := 1 to SrcLength do
  begin
    if SrcBuffer^ in [#0, '''', '\'] then
    begin
      DestBuffer[0] := '\';
      DestBuffer[1] := Chr(Ord('0') + (Byte(SrcBuffer^) shr 6));
      DestBuffer[2] := Chr(Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07));
      DestBuffer[3] := Chr(Ord('0') + (Byte(SrcBuffer^) and $07));
      Inc(DestBuffer, 4);
    end
    else
    begin
      DestBuffer^ := SrcBuffer^;
      Inc(DestBuffer);
    end;
    Inc(SrcBuffer);
  end;
  DestBuffer^ := '''';
end;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeString(Value: string): string;
var
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := PChar(Value);
  SetLength(Result, SrcLength);
  DestLength := 0;
  DestBuffer := PChar(Result);

  while SrcLength > 0 do
  begin
    if SrcBuffer^ = '\' then
    begin
      Inc(SrcBuffer);
      if SrcBuffer^ in ['\', ''''] then
      begin
        DestBuffer^ := SrcBuffer^;
        Inc(SrcBuffer);
        Dec(SrcLength, 2);
      end
      else
      begin
        DestBuffer^ := Chr(((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
          or ((Byte(SrcBuffer[1]) - Ord('0')) shl 3)
          or ((Byte(SrcBuffer[2]) - Ord('0'))));
        Inc(SrcBuffer, 3);
        Dec(SrcLength, 4);
      end;
    end
    else
    begin
      DestBuffer^ := SrcBuffer^;
      Inc(SrcBuffer);
      Dec(SrcLength);
    end;
    Inc(DestBuffer);
    Inc(DestLength);
  end;
  SetLength(Result, DestLength);
end;

{**
  Checks for possible sql errors.
  @param Connection a reference to database connection to execute Rollback.
  @param PlainDriver a PostgreSQL plain driver.
  @param Handle a PostgreSQL connection reference.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckPostgreSQLError(Connection: IZConnection;
  PlainDriver: IZPostgreSQLPlainDriver;
  Handle: PZPostgreSQLConnect; LogCategory: TZLoggingCategory;
  LogMessage: string);
var
  ErrorMessage: string;
begin
  if Assigned(Handle) then
    ErrorMessage := Trim(StrPas(PlainDriver.GetErrorMessage(Handle)))
  else ErrorMessage := '';

  if ErrorMessage <> '' then
  begin
    if Assigned(Connection) and Connection.GetAutoCommit then
      Connection.Rollback;

    DriverManager.LogError(LogCategory, PlainDriver.GetProtocol, LogMessage,
      0, ErrorMessage);
    raise EZSQLException.Create(Format(SSQLError1, [ErrorMessage]));
  end;
end;

{**
   Resolve problem with minor version in PostgreSql bettas
   @param Value a minor version string like "4betta2"
   @return a miror version number
}
function GetMinorVersion(Value: string): Word;
var
  I: integer;
  Temp: string;
begin
  for I := 1 to Length(Value) do
    if Value[I] in ['0'..'9'] then
      Temp := Temp + Value[I]
    else
      Break;
  Result := StrToIntDef(Temp, 0);
end;

end.
