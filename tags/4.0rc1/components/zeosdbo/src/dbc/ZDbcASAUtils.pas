{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           ASA Database Connectivity Classes             }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcASAUtils;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZPlainASADriver, ZDbcLogging,
  ZCompatibility, ZDbcASA, ZDbcStatement, ZVariant;   // SQLTimSt, FmtBCD,

const
  StdVars = 20;
  MinBLOBSize = 256;
  BlockSize = 20;

type
  { Interbase Error Class}
  EZASAConvertError = class(Exception);

  TZASADECLTYPE = record
    sqlType: SmallInt;
    sqlLen : Word;
  end;

  { Base interface for sqlda }
  IZASASQLDA = interface
    ['{7606E8EB-9FC8-4F76-8D91-E23AB96409E1}']
    procedure AllocateSQLDA( NumVars: Word);
    procedure InitFields;
    procedure FreeSQLDA;

    function GetData: PASASQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldName(const Index: Word): string;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): Word;

    procedure UpdateNull(const Index: Integer; Value: boolean);
    procedure UpdateBoolean(const Index: Integer; Value: boolean);
    procedure UpdateByte(const Index: Integer; Value: ShortInt);
    procedure UpdateShort(const Index: Integer; Value: SmallInt);
    procedure UpdateInt(const Index: Integer; Value: Integer);
    procedure UpdateLong(const Index: Integer; Value: Int64);
    procedure UpdateFloat(const Index: Integer; Value: Single);
    procedure UpdateDouble(const Index: Integer; Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; Value: Extended);
    procedure UpdatePChar(const Index: Integer; Value: PChar);
    procedure UpdateString(const Index: Integer; Value: string);
    procedure UpdateBytes(const Index: Integer; Value: TByteDynArray);
    procedure UpdateDate(const Index: Integer; Value: TDateTime);
    procedure UpdateTime(const Index: Integer; Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; Value: TDateTime);
    procedure UpdateValue(const Index: Word; Value: Variant);
    procedure WriteBlob(const Index: Integer; Stream: TStream);

    function IsNull(const Index: Integer): Boolean;
    function IsAssigned(const Index: Integer): Boolean;
    function GetBoolean(const Index: Integer): Boolean;
    function GetByte(const Index: Integer): ShortInt;
    function GetShort(const Index: Integer): SmallInt;
    function GetInt(const Index: Integer): Integer;
    function GetLong(const Index: Integer): Int64;
    function GetFloat(const Index: Integer): Single;
    function GetDouble(const Index: Integer): Double;
    function GetBigDecimal(const Index: Integer): Extended;
    function GetPChar(const Index: Integer): PChar;
    function GetString(const Index: Integer): string;
    function GetBytes(const Index: Integer): TByteDynArray;
    function GetDate(const Index: Integer): TDateTime;
    function GetTime(const Index: Integer): TDateTime;
    function GetTimestamp(const Index: Integer): TDateTime;
    function GetValue(const Index: Word): Variant;

    procedure ReadBlobToMem(const Index: Word; var Buffer: Pointer; var Length: LongWord);
    procedure ReadBlobToStream(const Index: Word; Stream: TStream);
    procedure ReadBlobToString(const Index: Word; var str: string);
    procedure ReadBlobToVariant(const Index: Word; var Value: Variant);
  end;

  { Base class contain core functions to work with sqlda structure
    Can allocate memory for sqlda structure get basic information }
  TZASASQLDA = class (TInterfacedObject, IZASASQLDA)
  private
    FSQLDA: PASASQLDA;
    FPlainDriver: IZASAPlainDriver;
    FHandle: PZASASQLCA;
    FCursorName: String;
    procedure CreateException( Msg: string);
    procedure CheckIndex(const Index: Word);
    procedure CheckRange(const Index: Word);
    procedure SetFieldType(const Index: Word; ASAType: Smallint; Len: LongWord;
      SetDeclType: Boolean = true);
  protected
    FDeclType: array of TZASADECLTYPE;
    procedure ReadBlob(const Index: Word; Buffer: Pointer; Length: LongWord);
  public
    constructor Create(PlainDriver: IZASAPlainDriver; Handle: PZASASQLCA;
      CursorName: String; NumVars: Word = StdVars);
    destructor Destroy; override;

    procedure AllocateSQLDA( NumVars: Word);
    procedure InitFields;
    procedure FreeSQLDA;

    function GetData: PASASQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldName(const Index: Word): string;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): Integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): Word;

    procedure UpdateNull(const Index: Integer; Value: boolean);
    procedure UpdateBoolean(const Index: Integer; Value: boolean);
    procedure UpdateByte(const Index: Integer; Value: ShortInt);
    procedure UpdateShort(const Index: Integer; Value: SmallInt);
    procedure UpdateInt(const Index: Integer; Value: Integer);
    procedure UpdateLong(const Index: Integer; Value: Int64);
    procedure UpdateFloat(const Index: Integer; Value: Single);
    procedure UpdateDouble(const Index: Integer; Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; Value: Extended);
    procedure UpdatePChar(const Index: Integer; Value: PChar);
    procedure UpdateString(const Index: Integer; Value: string);
    procedure UpdateBytes(const Index: Integer; Value: TByteDynArray);
    procedure UpdateDate(const Index: Integer; Value: TDateTime);
    procedure UpdateTime(const Index: Integer; Value: TDateTime);
    procedure UpdateDateTime(const Index: Integer; Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; Value: TDateTime);
    procedure UpdateValue(const Index: Word; Value: Variant);
    procedure WriteBlob(const Index: Integer; Stream: TStream);

    function IsNull(const Index: Integer): Boolean;
    function IsAssigned(const Index: Integer): Boolean;
    function GetBoolean(const Index: Integer): Boolean;
    function GetByte(const Index: Integer): ShortInt;
    function GetShort(const Index: Integer): SmallInt;
    function GetInt(const Index: Integer): Integer;
    function GetLong(const Index: Integer): Int64;
    function GetFloat(const Index: Integer): Single;
    function GetDouble(const Index: Integer): Double;
    function GetBigDecimal(const Index: Integer): Extended;
    function GetPChar(const Index: Integer): PChar;
    function GetString(const Index: Integer): string;
    function GetBytes(const Index: Integer): TByteDynArray;
    function GetDate(const Index: Integer): TDateTime;
    function GetTime(const Index: Integer): TDateTime;
    function GetTimestamp(const Index: Integer): TDateTime;
    function GetValue(const Index: Word): Variant;

    procedure ReadBlobToMem(const Index: Word; var Buffer: Pointer; var Length: LongWord);
    procedure ReadBlobToStream(const Index: Word; Stream: TStream);
    procedure ReadBlobToString(const Index: Word; var str: string);
    procedure ReadBlobToVariant(const Index: Word; var Value: Variant);
  end;

{**
  Converts a ASA native type into ZDBC SQL types.
  @param FieldHandle a handler to field description structure.
  @return a SQL undepended type.
}
function ConvertASATypeToSQLType( SQLType: SmallInt): TZSQLType;

{**
  Converts a ASA native type into String.
  @param SQLType Field of TASASQLVar structure.
  @return type description.
}
function ConvertASATypeToString( SQLType: SmallInt): String;

function ConvertASAJDBCToSqlType( FieldType: SmallInt): TZSQLType;
{
procedure TSQLTimeStampToASADateTime( DT: TSQLTimeStamp; const ASADT: PZASASQLDateTime);
function ASADateTimeToSQLTimeStamp( ASADT: PZASASQLDateTime): TSQLTimeStamp;
}
{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckASAError(PlainDriver: IZASAPlainDriver;
  Handle: PZASASQLCA; LogCategory: TZLoggingCategory; LogMessage: string = '');

function GetCachedResultSet(SQL: string;
  Statement: IZStatement; NativeResultSet: IZResultSet): IZResultSet;

procedure DescribeCursor( FASAConnection: IZASAConnection; FSQLData: IZASASQLDA;
  Cursor, SQL: String);

procedure Prepare( FASAConnection: IZASAConnection; FSQLData, FParamsSQLData: IZASASQLDA;
   const SQL: String; StmtNum: PSmallInt; var FPrepared, FMoreResults: Boolean);

procedure PrepareParameters( PlainDriver: IZASAPlainDriver;
  InParamValues: TZVariantDynArray; InParamTypes: TZSQLTypeArray;
  InParamCount: Integer; ParamSqlData: IZASASQLDA);

function RandomString( Len: integer): string;

implementation

uses {$IFDEF FPC}Variants,{$ELSE}{$IFNDEF VER130BELOW}Variants,{$ENDIF}{$ENDIF} ZMessages, ZDbcCachedResultSet, Math;

{ TZASASQLDA }

procedure TZASASQLDA.CreateException( Msg: string);
begin
  DriverManager.LogError( lcOther, FPlainDriver.GetProtocol, '', -1, Msg);
  raise EZSQLException.Create( Format( SSQLError1, [ Msg]));
end;

{**
   Check range count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZASASQLDA.CheckIndex(const Index: Word);
begin
  Assert( Assigned( FSQLDA), 'SQLDA not initialized.');
  Assert( Index < FSQLDA.sqld, 'Out of Range.');
end;

procedure TZASASQLDA.CheckRange(const Index: Word);
begin
  CheckIndex( Index);
  Assert( Assigned( FSQLDA.sqlVar[ Index].sqlData),
    'No memory for variable in SQLDA.');
end;

procedure TZASASQLDA.SetFieldType(const Index: Word; ASAType: Smallint;
  Len: LongWord; SetDeclType: Boolean = true);
begin
  CheckIndex(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    if ( ASAType and $FFFE = DT_LONGBINARY) or
       ( ASAType and $FFFE = DT_LONGVARCHAR) then
    begin
      if Assigned( sqlData) then
        ReallocMem( sqlData, SizeOf( TZASABlobStruct) + Len)
      else
        GetMem( sqlData, SizeOf( TZASABlobStruct) + Len);
      PZASABlobStruct( sqlData).array_len := Len;
      PZASABlobStruct( sqlData).stored_len := 0;
      PZASABlobStruct( sqlData).untrunc_len := 0;
      PZASABlobStruct( sqlData).arr[0] := #0;
      Inc( Len, SizeOf( TZASABlobStruct));
    end else begin
      if ( ASAType and $FFFE = DT_BINARY) or
         ( ASAType and $FFFE = DT_VARCHAR) then
        Inc( Len, SizeOf( TZASASQLSTRING));
      if Assigned( sqlData) then
        ReallocMem( sqlData, Len)
      else
        GetMem( sqlData, Len);
      if ( ASAType and $FFFE = DT_BINARY) or
         ( ASAType and $FFFE = DT_VARCHAR) then
        PZASASQLSTRING( sqlData).length := 0;
    end;
    sqlType := ASAType;
    sqllen := Len;
    if SetDeclType then
    begin
      FDeclType[Index].sqlType := sqlType;
      FDeclType[Index].sqlLen := sqlLen;
    end;
  end;
end;

constructor TZASASQLDA.Create(PlainDriver: IZASAPlainDriver; Handle: PZASASQLCA;
      CursorName: String; NumVars: Word = StdVars);
begin
  FPlainDriver := PlainDriver;
  FHandle := Handle;
  FCursorName := CursorName;
  AllocateSQLDA( NumVars);
  inherited Create;
end;

destructor TZASASQLDA.Destroy;
begin
  FreeSQLDA;
  inherited;
end;

{**
   Reallocate SQLDA to fields count length
   @param Value the count fields
}
procedure TZASASQLDA.AllocateSQLDA( NumVars: Word);
begin
  FreeSQLDA;
  FSQLDA := FPlainDriver.db_alloc_sqlda( NumVars);
  if not Assigned( FSQLDA) then
    CreateException( 'Not enough memory for SQLDA');
  SetLength( FDeclType, FSQLDA.sqln);
end;

{**
   Allocate memory for SQLVar in SQLDA structure for every
   fields by it length.
}
procedure TZASASQLDA.InitFields;
var
  i: Integer;
begin
  if Assigned( FSQLDA) then
  begin
    for i := 0 to FSQLDA.sqld-1 do
    begin
      FDeclType[i].sqlType := FSQLDA.sqlVar[i].sqlType;
      FDeclType[i].sqlLen := FSQLDA.sqlVar[i].sqlLen;
      case FSQLDA.sqlVar[i].sqlType and $FFFE of
        DT_DATE,
        DT_TIME,
        DT_TIMESTAMP  : begin
                          FSQLDA.sqlVar[i].sqlType := DT_TIMESTAMP_STRUCT +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := SizeOf( TZASASQLDateTime);
                        end;
        DT_DECIMAL    : begin
                          FSQLDA.sqlVar[i].sqlType := DT_DOUBLE +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := SizeOf( Double);
                        end;
        DT_STRING,
        DT_FIXCHAR,
        DT_VARCHAR,
        DT_LONGVARCHAR: if FSQLDA.sqlVar[i].sqlLen < MinBLOBSize then
                          FSQLDA.sqlVar[i].sqlType := DT_VARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001)
                        else begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGVARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
        DT_BINARY,
        DT_LONGBINARY:  if FSQLDA.sqlVar[i].sqlLen < MinBLOBSize then
                          FSQLDA.sqlVar[i].sqlType := DT_BINARY +
                            ( FSQLDA.sqlVar[i].sqlType and $0001)
                        else begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGBINARY +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
      end;
      SetFieldType( i, FSQLDA.sqlVar[i].sqlType, FSQLDA.sqlVar[i].sqlLen, False);
    end;
  end;
end;

{**
   Clear allocated data for SQLDA parameters
}
procedure TZASASQLDA.FreeSQLDA;
var
  i: integer;
begin
  if Assigned( FSQLDA) then
  begin
    for i := 0 to FSQLDA.sqld-1 do
    begin
      if Assigned( FSQLDA.sqlVar[i].sqlData) then
        FreeMem( FSQLDA.sqlVar[i].sqlData);
    end;
    FPlainDriver.db_free_sqlda( FSQLDA);
    FSQLDA := nil;
  end;
  FDeclType := nil;
end;

{**
   Return pointer to SQLDA structure
}
function TZASASQLDA.GetData: PASASQLDA;
begin
  Result := FSQLDA;
end;

{**
   Indicate blob field
   @param Index the index fields
   @return true if blob field overwise false
}
function TZASASQLDA.IsBlob(const Index: Word): boolean;
begin
  Result := GetFieldSqlType( Index) in
    [ stAsciiStream, stUnicodeStream, stBinaryStream];
end;

{**
   Indicate nullable field
   @param Index the index fields
   @return true if field nullable overwise false
}
function TZASASQLDA.IsNullable(const Index: Word): boolean;
begin
  CheckIndex(Index);
  Result := FSQLDA.sqlvar[Index].sqlType and 1 = 1
end;

{**
   Get fields count not allocated.
   @return fields count
}
function TZASASQLDA.GetFieldCount: Integer;
begin
  if Assigned( FSQLDA) then
    Result := FSQLDA.sqld
  else
    Result := 0;
end;

{**
   Return Name for field
   @param Index the index fields
   @return the name
}
function TZASASQLDA.GetFieldName(const Index: Word): string;
begin
  CheckIndex(Index);
  SetString( Result, FSQLDA.sqlvar[Index].sqlname.data,
    FSQLDA.sqlvar[Index].sqlname.length-1);
end;

{**
   Return field index by it name
   @param Index the index fields
   @return the index field
}
function TZASASQLDA.GetFieldIndex(const Name: String): Word;
begin
  for Result := 0 to FSQLDA.sqld - 1 do
    if FSQLDA.sqlvar[Result].sqlname.length = Length(name) then
      if StrLIComp(PChar(@FSQLDA.sqlvar[Result].sqlname.data), PChar(Name),
        Length(name)) = 0 then Exit;
  CreateException( Format( SFieldNotFound1, [name]));
  Result := 0; // satisfy compiler
end;

{**
   Return field length
   @param Index the index fields
   @return the field lenth
}
function TZASASQLDA.GetFieldLength(const Index: Word): Word;
begin
  CheckIndex( Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_DECIMAL then
    Result := FSQLDA.sqlvar[Index].sqlLen
  else
    Result := (FSQLDA.sqlvar[Index].sqlLen and $FF) div 2 + 1;
end;

{**
   Return field scale
   @param Index the index fields
   @return the field scale
}
function TZASASQLDA.GetFieldScale(const Index: Word): integer;
begin
  CheckIndex(Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_DECIMAL then
    Result := 0
  else
    Result := FSQLDA.sqlvar[Index].sqlLen div 256;
end;

{**
   Convert ASA sql type to SQLType
   @param Index the index fields
   @return the SQLType
}
function TZASASQLDA.GetFieldSqlType(const Index: Word): TZSQLType;
begin
  CheckIndex(Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_TIMESTAMP_STRUCT then
    Result := ConvertASATypeToSQLType( FSQLDA.sqlvar[Index].sqlType)
  else
    Result := ConvertASATypeToSQLType( FDeclType[Index].sqlType)
end;

{**
   Set up parameter null value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateNull(const Index: Integer; Value: Boolean);
begin
  CheckIndex( Index);
  with FSQLDA.sqlvar[ Index] do
  begin
    if not Assigned( sqlData) then
      SetFieldType( Index, DT_TINYINT or 1, SizeOf( Byte));
    if Value then
      sqlind^ := -1 //NULL
    else
      sqlind^ :=  0; //NOT NULL
  end;
end;

{**
   Set up parameter Boolean value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateBoolean(const Index: Integer; Value: boolean);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_BIT or 1, SizeOf( Byte));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallint(sqldata)^ := ord(Value);
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := ord(Value);
      DT_FLOAT            : PSingle(sqldata)^ := ord(Value);
      DT_DOUBLE           : PDouble(sqldata)^ := ord(Value);
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length := 1;
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                IntToStr( ord( Value)), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := ord(Value);
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := ord(Value);
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateByte(const Index: Integer; Value: ShortInt);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_TINYINT or 1, SizeOf( Byte));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallint(sqldata)^ := Value;
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := Value;
      DT_FLOAT            : PSingle(sqldata)^ := Value;
      DT_DOUBLE           : PDouble(sqldata)^ := Value;
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length :=
                                Length( IntToStr( Value));
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                IntToStr( Value), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := Value;
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := Value;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter short value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateShort(const Index: Integer; Value: SmallInt);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_SMALLINT or 1, SizeOf( SmallInt));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallInt(sqldata)^ := Value;
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := Value;
      DT_FLOAT            : PSingle(sqldata)^ := Value;
      DT_DOUBLE           : PDouble(sqldata)^ := Value;
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length :=
                                Length( IntToStr( Value));
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                IntToStr( Value), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := Value;
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := Value;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter integer value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateInt(const Index: Integer; Value: Integer);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_INT or 1, SizeOf( Integer));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallint(sqldata)^ := Value;
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := Value;
      DT_FLOAT            : PSingle(sqldata)^ := Value;
      DT_DOUBLE           : PDouble(sqldata)^ := Value;
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length :=
                                Length( IntToStr( Value));
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                IntToStr( Value), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := Value;
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := Value;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Long value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateLong(const Index: integer; Value: Int64);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_BIGINT or 1, SizeOf( Int64));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallint(sqldata)^ := Value;
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := Value;
      DT_FLOAT            : PSingle(sqldata)^ := Value;
      DT_DOUBLE           : PDouble(sqldata)^ := Value;
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length :=
                                Length( IntToStr( Value));
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                IntToStr( Value), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := Value;
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := Value;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Float value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateFloat(const Index: Integer; Value: Single);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_FLOAT or 1, SizeOf( Single));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallint(sqldata)^ := Trunc( Value);
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := Trunc( Value);
      DT_FLOAT            : PSingle(sqldata)^ := Value;
      DT_DOUBLE           : PDouble(sqldata)^ := Value;
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length :=
                                Length( FloatToStr( Value));
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                FloatToStr( Value), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := Trunc( Value);
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := Trunc( Value);
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Double value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateDouble(const Index: Integer; Value: Double);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_DOUBLE or 1, SizeOf( Double));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallint(sqldata)^ := Trunc( Value);
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := Trunc( Value);
      DT_FLOAT            : PSingle(sqldata)^ := Value;
      DT_DOUBLE           : PDouble(sqldata)^ := Value;
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length :=
                                Length( FloatToStr( Value));
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                FloatToStr( Value), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := Trunc( Value);
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := Trunc( Value);
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter BigDecimal value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateBigDecimal(const Index: Integer; Value: Extended);
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_DOUBLE or 1, SizeOf( Double));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_SMALLINT,
      DT_UNSSMALLINT      : PSmallint(sqldata)^ := Trunc( Value);
      DT_INT,
      DT_UNSINT           : PInteger(sqldata)^ := Trunc( Value);
      DT_FLOAT            : PSingle(sqldata)^ := Value;
      DT_DOUBLE           : PDouble(sqldata)^ := Value;
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length :=
                                Length( FloatToStr( Value));
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                FloatToStr( Value), sqllen-3);
                            end;
      DT_TINYINT,
      DT_BIT              : PByte(sqldata)^ := Trunc( Value);
      DT_BIGINT,
      DT_UNSBIGINT        : PInt64(sqldata)^ := Trunc( Value);
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter PChar value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdatePChar(const Index: Integer; Value: PChar);
var
  BlobSize: Integer;
begin
  CheckIndex( Index);
  BlobSize := StrLen( Value);
  if BlobSize < MinBLOBSize then
    SetFieldType( Index, DT_VARCHAR or 1, MinBLOBSize - 1)
  else
    SetFieldType( Index, DT_LONGVARCHAR or 1, BlobSize);
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length := BlobSize;
                              StrLCopy( @PZASASQLSTRING( sqlData).data[0],
                                Value, BlobSize);
                            end;
      DT_LONGVARCHAR      : begin
                              StrLCopy( @PZASABlobStruct( sqlData).arr[0], Value,
                                BlobSize);
                              PZASABlobStruct( sqlData).stored_len := BlobSize;
                              PZASABlobStruct( sqlData).untrunc_len := BlobSize;
                            end;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter String value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateString(const Index: Integer; Value: string);
var
  BlobSize: Integer;
begin
  CheckIndex( Index);
  BlobSize := Length( Value);
  if BlobSize < MinBLOBSize then
    SetFieldType( Index, DT_VARCHAR or 1, MinBLOBSize - 1)
  else
    SetFieldType( Index, DT_LONGVARCHAR or 1, BlobSize);
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_VARCHAR          : begin
                              PZASASQLSTRING( sqlData).length := BlobSize;
                              StrPLCopy( @PZASASQLSTRING( sqlData).data[0],
                                Value, BlobSize);
                            end;
      DT_LONGVARCHAR      : begin
                              StrPLCopy( @PZASABlobStruct( sqlData).arr[0], Value,
                                BlobSize);
                              PZASABlobStruct( sqlData).stored_len := BlobSize;
                              PZASABlobStruct( sqlData).untrunc_len := BlobSize;
                            end;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateBytes(const Index: Integer; Value: TByteDynArray);
var
  BlobSize: Integer;
begin
  CheckIndex( Index);
  BlobSize := Length( Value);
  if BlobSize < MinBLOBSize then
    SetFieldType( Index, DT_BINARY or 1, MinBLOBSize - 1)
  else
    SetFieldType( Index, DT_LONGBINARY or 1, BlobSize);
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_BINARY           : begin
                              PZASASQLSTRING( sqlData).length := BlobSize;
                              Move( Value[0], PZASASQLSTRING( sqlData).data[0], BlobSize);
                            end;
      DT_LONGBINARY       : begin
                              Move( Value[0], PZASABlobStruct( sqlData).arr[0], BlobSize);
                              PZASABlobStruct( sqlData).stored_len := BlobSize;
                              PZASABlobStruct( sqlData).untrunc_len := BlobSize;
                            end;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Date value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateDate(const Index: Integer; Value: TDateTime);
begin
  UpdateDateTime(Index, Value);
  FDeclType[Index].sqlType := DT_DATE;
end;

{**
   Set up parameter Time value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateTime(const Index: Integer; Value: TDateTime);
begin
  UpdateDateTime(Index, Value);
  FDeclType[Index].sqlType := DT_TIME;
end;

{**
   Set up parameter DateTime value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateDateTime(const Index: Integer;
  Value: TDateTime);
var
  y, m, d: word;
  hr, min, sec, msec: word;
begin
  CheckIndex( Index);
  SetFieldType( Index, DT_TIMESTAMP_STRUCT or 1, SizeOf( TZASASQLDateTime));
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_TIMESTAMP_STRUCT : begin
                              DecodeDate( Value, y, m, d);
                              DecodeTime( Value, hr, min, sec, msec);
                              PZASASQLDateTime( sqlData).Year := y;
                              PZASASQLDateTime( sqlData).Month := m - 1;
                              PZASASQLDateTime( sqlData).Day := d;
                              PZASASQLDateTime( sqlData).Hour := hr;
                              PZASASQLDateTime( sqlData).Minute := min;
                              PZASASQLDateTime( sqlData).Second := sec;
                              PZASASQLDateTime( sqlData).MicroSecond :=
                                msec * 10;
                              PZASASQLDateTime( sqlData).Day_of_Week := 0;
                              PZASASQLDateTime( sqlData).Day_of_Year := 0;
                            end;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
  FDeclType[Index].sqlType := DT_TIMESTAMP;
end;

{**
   Set up parameter Timestamp value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateTimestamp(const Index: Integer; Value: TDateTime);
begin
  UpdateDateTime(Index, Value);
end;

{**
   Set up parameter Type value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateValue(const Index: Word; Value: Variant);
begin
  case VarType(Value) of
    varEmpty,
    varNull       : UpdateNull( Index, True);
    varSmallint   : UpdateShort( Index, Value);
    varInteger    : UpdateInt( Index, Value);
    varSingle     : UpdateFloat( Index, Value);
    varDouble     : UpdateDouble( Index, Value);
    varCurrency   : UpdateBigDecimal( Index, Value);
    varDate       : UpdateDateTime( Index, Value);
    varStrArg,
    varString,
    varOleStr     : UpdateString( Index, Value);
    varBoolean    : UpdateBoolean( Index, Value);
    varByte       : UpdateByte( Index, Value);
{$IFDEF COMPILER6_UP}
    varInt64      : UpdateLong( Index, Value);
    varShortInt   : UpdateByte( Index, Value);
    varLongWord   : UpdateInt( Index, Value);
    varWord       : UpdateShort( Index, Value);
{$ENDIF}
  else
    if VarArrayDimCount( Value) = 1 then
    begin
      UpdateBytes( Index, VarArrayLock( Value));
      VarArrayUnlock( Value);
    end else
      CreateException( SUnsupportedParameterType);
  end;
end;

{**
   Write stream to blob field
   @param Index an index field number
   @param Stream the souse data stream
}
procedure TZASASQLDA.WriteBlob(const Index: Integer; Stream: TStream);
var
  BlobSize: Integer;
begin
  CheckIndex( Index);
  Stream.Position := 0;
  BlobSize := Stream.Size;
  SetFieldType( Index, DT_LONGBINARY or 1, BlobSize);
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_LONGVARCHAR,
      DT_LONGBINARY : begin
                        Stream.ReadBuffer( PZASABlobStruct( sqlData).arr[0], BlobSize);
                        Stream.Position := 0;
                        PZASABlobStruct( sqlData).stored_len := BlobSize;
                        PZASABlobStruct( sqlData).untrunc_len := BlobSize;
                      end;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then sqlind^ := 0; // not null
  end;
end;

{**
   Indicate field null
   @param Index the field index
   @return true if fied value NULL overwise false
}
function TZASASQLDA.IsNull(const Index: Integer): Boolean;
begin
  CheckIndex( Index);
  with FSQLDA.sqlvar[Index] do
    Result := Assigned( sqlind) and (sqlind^ < 0);
end;

{**
   Indicate sqldata assigned
   @param Index the field index
   @return true if assigned field data 
}
function TZASASQLDA.IsAssigned(const Index: Integer): Boolean;
begin
  CheckIndex( Index);
  with FSQLDA.sqlvar[Index] do
    Result := Assigned( sqldata);
end;

{**
   Return BigDecimal field value
   @param Index the field index
   @return the field BigDecimal value
}
function TZASASQLDA.GetBigDecimal(const Index: Integer): Extended;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := PSingle(sqldata)^;
      DT_DOUBLE      : Result := PDouble(sqldata)^;
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToFloat( s);
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Boolean field value
   @param Index the field index
   @return the field boolean value
}
function TZASASQLDA.GetBoolean(const Index: Integer): Boolean;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := False;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^ <> 0;
      DT_UNSSMALLINT : Result := PWord(sqldata)^ <> 0;
      DT_INT         : Result := PInteger(sqldata)^ <> 0;
      DT_UNSINT      : Result := PLongWord(sqldata)^ <> 0;
      DT_FLOAT       : Result := PSingle(sqldata)^ <> 0;
      DT_DOUBLE      : Result := PDouble(sqldata)^ <> 0;
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToInt( s) = 1;
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^ <> 0;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^ <> 0;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Byte field value
   @param Index the field index
   @return the field Byte value
}
function TZASASQLDA.GetByte(const Index: Integer): ShortInt;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := Trunc( PSingle(sqldata)^);
      DT_DOUBLE      : Result := Trunc( PDouble(sqldata)^);
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToInt( s);
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Bytes field value
   @param Index the field index
   @return the field Bytes value
}
function TZASASQLDA.GetBytes(const Index: Integer): TByteDynArray;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := nil;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_BINARY           : begin
                              SetLength( Result, PZASASQLSTRING( sqlData).length);
                              Move( PZASASQLSTRING( sqlData).data[0], Result[0], PZASASQLSTRING( sqlData).length);
                            end;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Date field value
   @param Index the field index
   @return the field Date value
}
function TZASASQLDA.GetDate(const Index: Integer): TDateTime;
begin
  Result := Trunc( GetTimestamp( Index));
end;

{**
   Return Double field value
   @param Index the field index
   @return the field Double value
}
function TZASASQLDA.GetDouble(const Index: Integer): Double;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := PSingle(sqldata)^;
      DT_DOUBLE      : Result := PDouble(sqldata)^;
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToFloat( s);
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Float field value
   @param Index the field index
   @return the field Float value
}
function TZASASQLDA.GetFloat(const Index: Integer): Single;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := PSingle(sqldata)^;
      DT_DOUBLE      : Result := PDouble(sqldata)^;
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToFloat( s);
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Integer field value
   @param Index the field index
   @return the field Integer value
}
function TZASASQLDA.GetInt(const Index: Integer): Integer;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := Trunc( PSingle(sqldata)^);
      DT_DOUBLE      : Result := Trunc( PDouble(sqldata)^);
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToInt( s);
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Long field value
   @param Index the field index
   @return the field Long value
}
function TZASASQLDA.GetLong(const Index: Integer): Int64;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := Trunc( PSingle(sqldata)^);
      DT_DOUBLE      : Result := Trunc( PDouble(sqldata)^);
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToInt64( s);
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Pchar field value
   @param Index the field index
   @return the field PChar value
}
function TZASASQLDA.GetPChar(const Index: Integer): PChar;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := nil;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_VARCHAR          : begin
                              GetMem( Result, PZASASQLSTRING( sqlData).length + 1);
                              StrLCopy( Result, @PZASASQLSTRING( sqlData).data[0],
                                PZASASQLSTRING( sqlData).length);
                            end;
    else
      Result := PChar( GetString( Index));
    end;
  end;
end;

{**
   Return String field value
   @param Index the field index
   @return the field String value
}
function TZASASQLDA.GetString(const Index: Integer): string;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := '';
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := IntToStr( PSmallint(sqldata)^);
      DT_UNSSMALLINT : Result := IntToStr( PWord(sqldata)^);
      DT_INT         : Result := IntToStr( PInteger(sqldata)^);
      DT_UNSINT      : Result := IntToStr( PLongWord(sqldata)^);
      DT_FLOAT       : Result := FloatToStr( PSingle(sqldata)^);
      DT_DOUBLE      : Result := FloatToStr( PDouble(sqldata)^);
      DT_VARCHAR     : SetString( Result, PChar( @PZASASQLSTRING( sqlData).data[0]),
                         PZASASQLSTRING( sqlData).length);
      DT_LONGVARCHAR : ReadBlobToString( Index, Result);
      DT_TIMESTAMP_STRUCT : Result := DateToStr( GetTimestamp( Index));
      DT_TINYINT     : Result := IntToStr( PByte(sqldata)^);
      {$IFDEF VER130BELOW}
      DT_BIT         : Result := BoolToStr( ( PByte(sqldata)^ = 1));
      {$ELSE}
      DT_BIT         : Result := BoolToStr( ( PByte(sqldata)^ = 1), True);
      {$ENDIF}
      DT_BIGINT,
      DT_UNSBIGINT   : Result := IntToStr( PInt64(sqldata)^);
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Short field value
   @param Index the field index
   @return the field Short value
}
function TZASASQLDA.GetShort(const Index: Integer): SmallInt;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
//      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := Trunc( PSingle(sqldata)^);
      DT_DOUBLE      : Result := Trunc( PDouble(sqldata)^);
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := StrToInt( s);
                       end;
      DT_TINYINT,
      DT_BIT         : Result := PByte(sqldata)^;
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Time field value
   @param Index the field index
   @return the field Time value
}
function TZASASQLDA.GetTime(const Index: Integer): TDateTime;
begin
  Result := Frac( GetTimestamp( Index));
end;

{**
   Return Timestamp field value
   @param Index the field index
   @return the field Timestamp value
}
function TZASASQLDA.GetTimestamp(const Index: Integer): TDateTime;
begin
  CheckRange( Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Result := 0;
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_TIMESTAMP_STRUCT : begin
                              Result := EncodeDate( PZASASQLDateTime( sqlData).Year,
                              PZASASQLDateTime( sqlData).Month + 1,
                              PZASASQLDateTime( sqlData).Day) +
                              EncodeTime( PZASASQLDateTime( sqlData).Hour,
                              PZASASQLDateTime( sqlData).Minute,
                              PZASASQLDateTime( sqlData).Second,
                              PZASASQLDateTime( sqlData).MicroSecond div 10);
                            end;
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

{**
   Return Variant field value
   @param Index the field index
   @return the field Variant value
}
function TZASASQLDA.GetValue(const Index: Word): Variant;
var
  s: String;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    VarClear(Result);
    if (sqlind^ < 0) then Exit;

    case sqlType and $FFFE of
      DT_SMALLINT    : Result := PSmallint(sqldata)^;
      DT_UNSSMALLINT : Result := PWord(sqldata)^;
      DT_INT         : Result := PInteger(sqldata)^;
//      DT_UNSINT      : Result := PLongWord(sqldata)^;
      DT_FLOAT       : Result := PSingle(sqldata)^;
      DT_DOUBLE      : Result := PDouble(sqldata)^;
      DT_VARCHAR     : begin
                         SetString( s, PChar( @PZASASQLSTRING( sqlData).data[0]),
                           PZASASQLSTRING( sqlData).length);
                         Result := s;
                       end;
      DT_LONGVARCHAR,
      DT_LONGBINARY  : ReadBlobToVariant(Index, Result);
      DT_TIMESTAMP_STRUCT : Result := GetTimeStamp( Index);
      DT_TINYINT     : Result := PByte(sqldata)^;
      DT_BIT         : Result := Boolean( PByte(sqldata)^);
  {$IFDEF COMPILER6_UP}
      DT_BIGINT,
      DT_UNSBIGINT   : Result := PInt64(sqldata)^;
  {$ELSE}
      DT_BIGINT,
      DT_UNSBIGINT   : Result := Integer( PInt64(sqldata)^);
  {$ENDIF}
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
    end;
  end;
end;

procedure TZASASQLDA.ReadBlob(const Index: Word; Buffer: Pointer;
  Length: LongWord);
var
  TempSQLDA: PASASQLDA;
  Offs, Rd: LongWord;
const
  BlockSize = 32700;
begin
  with FSQLDA.sqlvar[Index] do
  begin
    if ( ( sqlType and $FFFE = DT_LONGVARCHAR) or
         ( sqlType and $FFFE = DT_LONGBINARY)) and
       ( PZASABlobStruct( sqlData).array_len > 0) then
    begin
      Assert( PZASABlobStruct( sqlData).array_len = PZASABlobStruct( sqlData).untrunc_len,
        'Blob Record is not correctly initialized');
      if PZASABlobStruct( sqlData).array_len <> Length then
        CreateException( 'Could''nt complete BLOB-Read');
      move( PZASABlobStruct( sqlData).arr[0], PChar( Buffer)[0], PZASABlobStruct( sqlData).array_len);
    end else begin
      TempSQLDA := FPlainDriver.db_alloc_sqlda( 1);
      if not Assigned( TempSQLDA) then
        CreateException( 'Not enough memory for SQLDA');
      try
        with TempSQLDA.sqlvar[ 0] do
        begin
          sqlType := DT_FIXCHAR;
          sqlname.length := 0;
          sqlname.data[0] := #0;
          TempSQLDA.sqld := TempSQLDA.sqln;

          sqlData := Buffer;
          Offs := 0;
          sqllen := Min( BlockSize, Length);
          Rd := 0;

          while True do
          begin
            FPlainDriver.db_get_data( FHandle, PChar( FCursorName), Index + 1, Offs, TempSQLDA);
            CheckASAError( FPlainDriver, FHandle, lcOther);
            if sqlind^ < 0 then
              break;
            Inc( Rd, sqllen);
            if sqlind^ = 0 then
              break;
            Inc( Offs, sqllen);
            Inc( PChar( sqlData), sqllen);
            sqllen := Min( BlockSize, Length-Rd);
          end;
          if Rd <> Length then
            CreateException( 'Could''nt complete BLOB-Read');

          DriverManager.LogMessage( lcExecute, FPlainDriver.GetProtocol,
            Format( 'GET DATA for Column: %s', [ GetFieldName(Index)]));

          FPlainDriver.db_free_sqlda( TempSQLDA);
          TempSQLDA := nil;
        end;
      except
        if Assigned( TempSQLDA) then
          FPlainDriver.db_free_sqlda( TempSQLDA);
        raise;
      end;
    end;
  end;
end;

{**
   Read blob data to Buffer
   @param Index an filed index
   @param Str destination string
}
procedure TZASASQLDA.ReadBlobToMem(const Index: Word; var Buffer: Pointer;
  var Length: LongWord);
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Buffer := nil;
    Length := 0;
    if (sqlind^ < 0) then Exit;

    if ( ( sqlType and $FFFE = DT_LONGVARCHAR) or
         ( sqlType and $FFFE = DT_LONGBINARY)) then
    begin
      Length := PZASABlobStruct( sqlData).untrunc_len;
      GetMem( Buffer, Length);
      ReadBlob( Index, Buffer, Length);
    end else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
  end;
end;

{**
   Read blob data to string
   @param Index an filed index
   @param Str destination string
}
procedure TZASASQLDA.ReadBlobToString(const Index: Word; var Str: string);
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Str := '';
    if (sqlind^ < 0) then Exit;

    if sqlType and $FFFE = DT_LONGVARCHAR then
    begin
      SetLength( Str, PZASABlobStruct( sqlData).untrunc_len);
      ReadBlob( Index, PChar( Str), Length( Str));
    end else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
  end;
end;

{**
   Read blob data to stream
   @param Index an filed index
   @param Stream destination stream object
}
procedure TZASASQLDA.ReadBlobToStream(const Index: Word; Stream: TStream);
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Stream.Size := 0;
    if (sqlind^ < 0) then Exit;

    if ( ( sqlType and $FFFE = DT_LONGVARCHAR) or
         ( sqlType and $FFFE = DT_LONGBINARY)) and
      ( Stream is TMemoryStream) then
    begin
      Stream.Size := PZASABlobStruct( sqlData).untrunc_len;
      ReadBlob( Index, TMemoryStream( Stream).Memory, Stream.Size);
    end else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
  end;
end;

{**
   Read blob data to variant value
   @param Index an filed index
   @param Value destination variant value
}
procedure TZASASQLDA.ReadBlobToVariant(const Index: Word;
  var Value: Variant);
var
  PData: Pointer;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Value := Null;
    if (sqlind^ < 0) then Exit;

    if ( ( sqlType and $FFFE = DT_LONGVARCHAR) or
         ( sqlType and $FFFE = DT_LONGBINARY)) then
    begin
      Value := VarArrayCreate( [ 0, PZASABlobStruct( sqlData).untrunc_len-1], varByte);
      PData := VarArrayLock( Value);
      try
        ReadBlob( Index, PData, PZASABlobStruct( sqlData).untrunc_len);
      finally
        VarArrayUnlock( Value);
      end;
    end else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
  end;
end;

{**
  Converts a ASA native types into ZDBC SQL types.
  @param SQLType Field of TASASQLVar structure.
  @return a SQL undepended type.
}
function ConvertASATypeToSQLType( SQLType: SmallInt): TZSQLType;
begin
  case SQLType and $FFFE of
    DT_NOTYPE:
      Result := stUnknown;
    DT_SMALLINT:
      Result := stShort;
    DT_INT:
      Result := stInteger;
    DT_DECIMAL:
      Result := stDouble; //BCD Felder mom. nicht unterstützt
    DT_FLOAT:
      Result := stFloat;
    DT_DOUBLE:
      Result := stDouble;
    DT_DATE:
      Result := stDate;
    DT_VARIABLE, DT_STRING, DT_FIXCHAR, DT_VARCHAR:
      Result := stString;
    DT_LONGVARCHAR:
      Result := stAsciiStream;
    DT_TIME:
      Result := stTime;
    DT_TIMESTAMP:
      Result := stTimestamp;
    DT_TIMESTAMP_STRUCT:
      Result := stTimestamp;
    DT_BINARY:
      Result := stBytes;
    DT_LONGBINARY:
      Result := stBinaryStream;
    DT_TINYINT:
      Result := stByte;
    DT_BIGINT:
      Result := stLong;
    DT_UNSINT:
      Result := stInteger;
    DT_UNSSMALLINT:
      Result := stShort;
    DT_UNSBIGINT:
      Result := stLong;
    DT_BIT:
      Result := stBoolean;
  else
    Result := stUnknown;
  end;
end;

{**
  Converts a ASA native type into String.
  @param SQLType Field of TASASQLVar structure.
  @return type description.
}
function ConvertASATypeToString( SQLType: SmallInt): String;
begin
  case SQLType and $FFFE of
    DT_SMALLINT:
      Result := 'DT_SMALLINT';
    DT_INT:
      Result := 'DT_INT';
    DT_DECIMAL:
      Result := 'DT_DECIMAL'; //BCD Felder mom. nicht unterstützt
    DT_FLOAT:
      Result := 'DT_FLOAT';
    DT_DOUBLE:
      Result := 'DT_DOUBLE';
    DT_DATE:
      Result := 'DT_DATE';
    DT_VARIABLE:
      Result := 'DT_VARIABLE';
    DT_STRING:
      Result := 'DT_STRING';
    DT_FIXCHAR:
      Result := 'DT_FIXCHAR';
    DT_VARCHAR:
      Result := 'DT_VARCHAR';
    DT_LONGVARCHAR:
      Result := 'DT_LONGVARCHAR';
    DT_TIME:
      Result := 'DT_TIME';
    DT_TIMESTAMP:
      Result := 'DT_TIMESTAMP';
    DT_TIMESTAMP_STRUCT:
      Result := 'DT_TIMESTAMP_STRUCT';
    DT_BINARY:
      Result := 'DT_BINARY';
    DT_LONGBINARY:
      Result := 'DT_LONGBINARY';
    DT_TINYINT:
      Result := 'DT_TINYINT';
    DT_BIGINT:
      Result := 'DT_BIGINT';
    DT_UNSINT:
      Result := 'DT_UNSINT';
    DT_UNSSMALLINT:
      Result := 'DT_UNSSMALLINT';
    DT_UNSBIGINT:
      Result := 'DT_UNSBIGINT';
    DT_BIT:
      Result := 'DT_BIT';
  else
    Result := 'Unknown';
  end;
end;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertASAJDBCToSqlType(FieldType: SmallInt): TZSQLType;
begin
  case FieldType of
    1, 12, -8, -9: Result := stString;
    -7: Result := stBoolean;
    -6: Result := stByte;
    5: Result := stShort;
    4: Result := stInteger;
    -5 : Result := stLong;
    6, 7, 8: Result := stDouble;
    2, 3: Result := stDouble;  //BCD Feld
    11, 93: Result := stTimestamp;
    -1, -10: Result := stAsciiStream;
    -4, -11, 1111: Result := stBinaryStream;
    -3, -2: Result := stBytes;
    92: Result := stTime;
    91: Result := stDate;
  else
    Result := stUnknown;
  end;
end;
{
procedure TSQLTimeStampToASADateTime( DT: TSQLTimeStamp; const ASADT: PZASASQLDateTime);
begin
  ASADT.Year := DT.Year;
  ASADT.Month := DT.Month - 1;
  ASADT.Day := DT.Day;
  ASADT.Hour := DT.Hour;
  ASADT.Minute := DT.Minute;
  ASADT.Second := DT.Second;
  ASADT.MicroSecond := DT.Fractions * 10;
  ASADT.Day_of_Week := 0;
  ASADT.Day_of_Year := 0;
end;

function ASADateTimeToSQLTimeStamp( ASADT: PZASASQLDateTime): TSQLTimeStamp;
begin
  DT.Year := ASADT.Year;
  DT.Month := ASADT.Month + 1;
  DT.Day := ASADT.Day;
  DT.Hour := ASADT.Hour;
  DT.Minute := ASADT.Minute;
  DT.Second := ASADT.Second;
  DT.Fractions := ASADT.MicroSecond div 10;
end;
}
{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckASAError( PlainDriver: IZASAPlainDriver;
  Handle: PZASASQLCA; LogCategory: TZLoggingCategory; LogMessage: string = '');
var
  ErrorBuf: array[0..1024] of Char;
  ErrorMessage: string;
begin
  if Handle.SqlCode < SQLE_NOERROR then
  begin
    ErrorMessage := PlainDriver.sqlError_Message( Handle, ErrorBuf, SizeOf( ErrorBuf));
    //SyntaxError Position in SQLCount
    DriverManager.LogError( LogCategory, PlainDriver.GetProtocol, LogMessage,
      Handle.SqlCode, ErrorMessage);
    raise EZSQLException.CreateWithCode( Handle.SqlCode,
      Format(SSQLError1, [ErrorMessage]));
  end;
end;

{**
  Create CachedResultSet with using TZCachedResultSet and return it.
  @param SQL a sql query command
  @param Statement a zeos statement object
  @param NativeResultSet a native result set
  @return cached ResultSet
}
function GetCachedResultSet(SQL: string;
  Statement: IZStatement; NativeResultSet: IZResultSet): IZResultSet;
var
  CachedResultSet: TZCachedResultSet;
begin
  if (Statement.GetResultSetConcurrency <> rcReadOnly)
    or (Statement.GetResultSetType <> rtForwardOnly) then
  begin
    CachedResultSet := TZCachedResultSet.Create( NativeResultSet, SQL, nil);
    CachedResultSet.SetResolver( TZASACachedResolver.Create(
      Statement, NativeResultSet.GetMetadata));
    CachedResultSet.SetConcurrency( Statement.GetResultSetConcurrency);
    Result := CachedResultSet;
  end else
    Result := NativeResultSet;
end;

procedure DescribeCursor( FASAConnection: IZASAConnection; FSQLData: IZASASQLDA;
  Cursor, SQL: String);
begin
  FSQLData.AllocateSQLDA( StdVars);
  with FASAConnection do
  begin
    GetPlainDriver.db_describe_cursor( GetDBHandle, PChar( Cursor),
      FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
    ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);
    if FSQLData.GetData^.sqld <= 0 then
      raise EZSQLException.Create( SCanNotRetrieveResultSetData)
    else if ( FSQLData.GetData^.sqld > FSQLData.GetData^.sqln) then
    begin
      FSQLData.AllocateSQLDA( FSQLData.GetData^.sqld);
      GetPlainDriver.db_describe_cursor( GetDBHandle, PChar( Cursor),
        FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);
    end;
    FSQLData.InitFields;
  end;
end;

procedure Prepare( FASAConnection: IZASAConnection; FSQLData, FParamsSQLData: IZASASQLDA;
   const SQL: String; StmtNum: PSmallInt; var FPrepared, FMoreResults: Boolean);
begin
  with FASAConnection do
  begin
    if FPrepared then
    begin
      FParamsSQLData.AllocateSQLDA( StdVars);
      FSQLData.AllocateSQLDA( StdVars);
      if StmtNum^ <> 0 then
      begin
        GetPlainDriver.db_dropstmt( GetDBHandle, nil, nil, StmtNum);
        StmtNum^ := 0;
      end;
    end;
    try
      GetPlainDriver.db_prepare_describe( GetDBHandle, nil, StmtNum,
        PChar( SQL), FParamsSQLData.GetData, SQL_PREPARE_DESCRIBE_STMTNUM +
        SQL_PREPARE_DESCRIBE_INPUT + SQL_PREPARE_DESCRIBE_VARRESULT, 0);
      ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, SQL);

      FMoreResults := GetDBHandle.sqlerrd[2] = 0;

      if FParamsSQLData.GetData^.sqld > FParamsSQLData.GetData^.sqln then
      begin
        FParamsSQLData.AllocateSQLDA( FParamsSQLData.GetData^.sqld);
        GetPlainDriver.db_describe( GetDBHandle, nil, StmtNum,
          FParamsSQLData.GetData, SQL_DESCRIBE_INPUT);
        ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute,
          SQL);
      end;

      if not FMoreResults then
      begin
        GetPlainDriver.db_describe( GetDBHandle, nil, StmtNum,
          FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
        ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute,
          SQL);
        if FSQLData.GetData^.sqld > FSQLData.GetData^.sqln then
        begin
          FSQLData.AllocateSQLDA( FSQLData.GetData^.sqld);
          GetPlainDriver.db_describe( GetDBHandle, nil, StmtNum,
            FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
          ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute,
            SQL);
        end;
        FSQLData.InitFields;
      end;

      FPrepared := true;
      { Logging SQL Command }
      DriverManager.LogMessage( lcExecute, GetPlainDriver.GetProtocol,
        'Prepare: '+ SQL);
    except
      on E: Exception do
      begin
        if StmtNum^ <> 0 then
          GetPlainDriver.db_dropstmt( GetDBHandle, nil, nil, StmtNum);
        raise;
      end;
    end;
  end;
end;

procedure PrepareParameters( PlainDriver: IZASAPlainDriver;
  InParamValues: TZVariantDynArray; InParamTypes: TZSQLTypeArray;
  InParamCount: Integer; ParamSqlData: IZASASQLDA);
var
  i: Integer;
  TempBlob: IZBlob;
  TempStream: TStream;
begin
  if InParamCount <> ParamSqlData.GetFieldCount then
    raise EZSQLException.Create( SInvalidInputParameterCount);
  for i := 0 to ParamSqlData.GetFieldCount-1 do
  begin
    if DefVarManager.IsNull( InParamValues[i])then
      ParamSqlData.UpdateNull( i, True)
    else
      case InParamTypes[i] of
        stBoolean:
          ParamSqlData.UpdateBoolean( i,
            SoftVarManager.GetAsBoolean( InParamValues[i]));
        stByte:
          ParamSqlData.UpdateByte( i,
            SoftVarManager.GetAsInteger( InParamValues[i]));
        stShort:
          ParamSqlData.UpdateShort( i,
            SoftVarManager.GetAsInteger( InParamValues[i]));
        stInteger:
          ParamSqlData.UpdateInt( i,
            SoftVarManager.GetAsInteger( InParamValues[i]));
        stLong:
          ParamSqlData.UpdateLong( i,
            SoftVarManager.GetAsInteger( InParamValues[i]));
        stFloat:
          ParamSqlData.UpdateFloat( i,
            SoftVarManager.GetAsFloat( InParamValues[i]));
        stDouble:
          ParamSqlData.UpdateDouble( i,
            SoftVarManager.GetAsFloat( InParamValues[i]));
        stBigDecimal:
          ParamSqlData.UpdateBigDecimal( i,
            SoftVarManager.GetAsFloat( InParamValues[i]));
        stString:
          ParamSqlData.UpdateString( i,
            SoftVarManager.GetAsString( InParamValues[i]));
        stUnicodeString:
          ParamSqlData.UpdateString( i,
            SoftVarManager.GetAsUnicodeString( InParamValues[i]));
        stBytes:
          ParamSqlData.UpdateBytes( i,
            StrToBytes(SoftVarManager.GetAsString( InParamValues[i])));
        stDate:
          ParamSqlData.UpdateDate( i,
            SoftVarManager.GetAsDateTime( InParamValues[i]));
        stTime:
          ParamSqlData.UpdateTime( i,
            SoftVarManager.GetAsDateTime( InParamValues[i]));
        stTimestamp:
          ParamSqlData.UpdateTimestamp( i,
            SoftVarManager.GetAsDateTime( InParamValues[i]));
        stAsciiStream,
        stUnicodeStream,
        stBinaryStream:
          begin
            TempBlob := DefVarManager.GetAsInterface( InParamValues[i]) as IZBlob;
            if not TempBlob.IsEmpty then
            begin
              TempStream := TempBlob.GetStream;
              try
                ParamSqlData.WriteBlob( i, TempStream);
              finally
                TempStream.Free;
              end;
            end;
          end;
        else
          raise EZASAConvertError.Create( SUnsupportedParameterType);
      end;
  end;
end;

{**
   Generate specific length random string and return it
   @param Len a length result string
   @return random string
}
function RandomString( Len: integer): string;
begin
  Result := '';
  while Length( Result) < Len do
    Result := Result + IntToStr( Trunc( Random( High( Integer))));
  if Length( Result) > Len then
    Result := Copy( Result, 1, Len);
end;

end.
