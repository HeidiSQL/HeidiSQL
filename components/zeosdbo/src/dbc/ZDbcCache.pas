{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Caching Classes and Interfaces               }
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

unit ZDbcCache;

interface

{$I ZDbc.inc}


uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  ZCompatibility, Classes, SysUtils, Contnrs, ZClasses, ZDbcIntfs,
  ZDbcResultSet, ZDbcResultSetMetadata, ZVariant;

type

  {** Defines a row status type. }
  TZRowUpdateType = (utUnmodified, utModified, utInserted, utDeleted);
  TZRowUpdateTypes = set of TZRowUpdateType;

  {** Defines a header for row buffer. }
  TZRowBuffer = packed record
    Index: Integer;
    UpdateType: TZRowUpdateType;
    BookmarkFlag: Byte;
    Columns: TByteArray;
  end;
  PZRowBuffer = ^TZRowBuffer;

  {** Implements a column buffer accessor. }
  TZRowAccessor = class(TObject)
  private
    FRowSize: Integer;
    FColumnsSize: Integer;
    FColumnCount: Integer;
    FColumnNames: array of string;
    FColumnCases: array of Boolean;
    FColumnTypes: array of TZSQLType;
    FColumnLengths: array of Integer;
    FColumnOffsets: array of Integer;
    FBuffer: PZRowBuffer;
    FHasBlobs: Boolean;
    FTemp: string;

    function GetColumnSize(ColumnInfo: TZColumnInfo): Integer;
    function GetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer): IZBlob;
    procedure SetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer;
      Value: IZBlob);

  protected
    procedure CheckColumnIndex(ColumnIndex: Integer);
    procedure CheckColumnConvertion(ColumnIndex: Integer; ResultType: TZSQLType);

  public
    constructor Create(ColumnsInfo: TObjectList);
    destructor Destroy; override;

    function AllocBuffer(var Buffer: PZRowBuffer): PZRowBuffer;
    procedure InitBuffer(Buffer: PZRowBuffer);
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure MoveBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure ClearBuffer(Buffer: PZRowBuffer);
    procedure DisposeBuffer(Buffer: PZRowBuffer);

    function CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
      ColumnIndices: TIntegerDynArray; ColumnDirs: TBooleanDynArray): Integer;

    function Alloc: PZRowBuffer;
    procedure Init;
    procedure CopyTo(DestBuffer: PZRowBuffer);
    procedure CopyFrom(SrcBuffer: PZRowBuffer);
    procedure MoveTo(DestBuffer: PZRowBuffer);
    procedure MoveFrom(SrcBuffer: PZRowBuffer);
    procedure CloneTo(DestBuffer: PZRowBuffer);
    procedure CloneFrom(SrcBuffer: PZRowBuffer);
    procedure Clear;
    procedure Dispose;

    function GetColumnData(ColumnIndex: Integer; var IsNull: Boolean): Pointer;
    function GetColumnDataSize(ColumnIndex: Integer): Integer;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPChar(ColumnIndex: Integer; var IsNull: Boolean): PChar;
    function GetString(ColumnIndex: Integer; var IsNull: Boolean): string;
    function GetUnicodeString(ColumnIndex: Integer; var IsNull: Boolean): WideString;
    function GetBoolean(ColumnIndex: Integer; var IsNull: Boolean): Boolean;
    function GetByte(ColumnIndex: Integer; var IsNull: Boolean): ShortInt;
    function GetShort(ColumnIndex: Integer; var IsNull: Boolean): SmallInt;
    function GetInt(ColumnIndex: Integer; var IsNull: Boolean): Integer;
    function GetLong(ColumnIndex: Integer; var IsNull: Boolean): Int64;
    function GetFloat(ColumnIndex: Integer; var IsNull: Boolean): Single;
    function GetDouble(ColumnIndex: Integer; var IsNull: Boolean): Double;
    function GetBigDecimal(ColumnIndex: Integer; var IsNull: Boolean): Extended;
    function GetBytes(ColumnIndex: Integer; var IsNull: Boolean): TByteDynArray;
    function GetDate(ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetTime(ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetTimestamp(ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetAsciiStream(ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetUnicodeStream(ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBinaryStream(ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBlob(ColumnIndex: Integer; var IsNull: Boolean): IZBlob;
    function GetValue(ColumnIndex: Integer): TZVariant;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetNotNull(ColumnIndex: Integer);
    procedure SetNull(ColumnIndex: Integer);
    procedure SetBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure SetByte(ColumnIndex: Integer; Value: ShortInt);
    procedure SetShort(ColumnIndex: Integer; Value: SmallInt);
    procedure SetInt(ColumnIndex: Integer; Value: Integer);
    procedure SetLong(ColumnIndex: Integer; Value: Int64);
    procedure SetFloat(ColumnIndex: Integer; Value: Single);
    procedure SetDouble(ColumnIndex: Integer; Value: Double);
    procedure SetBigDecimal(ColumnIndex: Integer; Value: Extended);
    procedure SetPChar(ColumnIndex: Integer; Value: PChar);
    procedure SetString(ColumnIndex: Integer; Value: string);
    procedure SetUnicodeString(ColumnIndex: Integer; Value: WideString);
    procedure SetBytes(ColumnIndex: Integer; Value: TByteDynArray);
    procedure SetDate(ColumnIndex: Integer; Value: TDateTime);
    procedure SetTime(ColumnIndex: Integer; Value: TDateTime);
    procedure SetTimestamp(ColumnIndex: Integer; Value: TDateTime);
    procedure SetAsciiStream(ColumnIndex: Integer; Value: TStream);
    procedure SetUnicodeStream(ColumnIndex: Integer; Value: TStream);
    procedure SetBinaryStream(ColumnIndex: Integer; Value: TStream);
    procedure SetBlob(ColumnIndex: Integer; Value: IZBlob);
    procedure SetValue(ColumnIndex: Integer; Value: TZVariant);

    property ColumnsSize: Integer read FColumnsSize;
    property RowSize: Integer read FRowSize;
    property RowBuffer: PZRowBuffer read FBuffer write FBuffer;
  end;

const
  RowHeaderSize = SizeOf(TZRowBuffer) - SizeOf(TByteArray);

implementation

uses Math, ZMessages, ZSysUtils, ZDbcUtils, ZCollections;

{ TZRowAccessor }

{**
  Creates this object and assignes the main properties.
  @param ColumnsInfo a collection with column information.
}
constructor TZRowAccessor.Create(ColumnsInfo: TObjectList);
var
  I: Integer;
  Current: TZColumnInfo;
begin
  FBuffer := nil;
  FColumnCount := ColumnsInfo.Count;
  FColumnsSize := 0;
  SetLength(FColumnNames, FColumnCount);
  SetLength(FColumnCases, FColumnCount);
  SetLength(FColumnTypes, FColumnCount);
  SetLength(FColumnLengths, FColumnCount);
  SetLength(FColumnOffsets, FColumnCount);
  FHasBlobs := False;

  for I := 0 to FColumnCount - 1 do
  begin
    Current := TZColumnInfo(ColumnsInfo[I]);
    FColumnNames[I] := Current.ColumnName;
    FColumnCases[I] := Current.CaseSensitive;
    FColumnTypes[I] := Current.ColumnType;
    FColumnLengths[I] := GetColumnSize(Current);
    FColumnOffsets[I] := FColumnsSize;
    Inc(FColumnsSize, FColumnLengths[I] + 1);
    // 32768 is the length of a TByteArray.
    if FColumnsSize > 32767 then begin
      raise Exception.Create('Zeos: Row buffer width exceeded.\nTry using fewer or longer columns in SQL query.');
    end;
    FHasBlobs := FHasBlobs
      or (FColumnTypes[I] in [stAsciiStream, stUnicodeStream, stBinaryStream]);
  end;
  FRowSize := FColumnsSize + RowHeaderSize;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZRowAccessor.Destroy;
begin
  inherited Destroy;
end;

{**
  Checks is the column index correct and row buffer is available.
  @param ColumnIndex an index of column.
}
procedure TZRowAccessor.CheckColumnIndex(ColumnIndex: Integer);
begin
  if not Assigned(FBuffer) then
    raise EZSQLException.Create(SRowBufferIsNotAssigned);

  if (ColumnIndex <= 0) or (ColumnIndex > FColumnCount) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;
end;

{**
  Checks is the column convertion from one type to another type allowed.
  @param ColumnIndex an index of column.
  @param ResultType a requested data type.
  @return <code>true</code> if convertion is allowed or throw exception
    otherwise.
}
procedure TZRowAccessor.CheckColumnConvertion(ColumnIndex: Integer;
  ResultType: TZSQLType);
begin
  if not Assigned(FBuffer) then
    raise EZSQLException.Create(SRowBufferIsNotAssigned);

  if (ColumnIndex <= 0) or (ColumnIndex > FColumnCount) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;

  if not CheckConvertion(FColumnTypes[ColumnIndex - 1], ResultType) then
  begin
    raise EZSQLException.Create(
      Format(SConvertionIsNotPossible, [ColumnIndex,
      DefineColumnTypeName(FColumnTypes[ColumnIndex - 1]),
      DefineColumnTypeName(ResultType)]));
  end;
end;

{**
  Gets a size of column with the specified type.
  @param ColumnInfo a column information struct.
  @return a size for the column with the specified type.
}
function TZRowAccessor.GetColumnSize(ColumnInfo: TZColumnInfo): Integer;
begin
  case ColumnInfo.ColumnType of
    stBoolean:
      Result := SizeOf(WordBool);
    stByte:
      Result := SizeOf(Byte);
    stShort:
      Result := SizeOf(SmallInt);
    stInteger:
      Result := SizeOf(Integer);
    stLong:
      Result := SizeOf(Int64);
    stFloat:
      Result := SizeOf(Single);
    stDouble:
      Result := SizeOf(Double);
    stBigDecimal:
      Result := SizeOf(Extended);
    stString:
      Result := ColumnInfo.Precision + 1;
    stUnicodeString:
      Result := 2 * ColumnInfo.Precision + 2;
    stBytes:
      Result := SizeOf(SmallInt) + ColumnInfo.Precision;
    stDate, stTime, stTimestamp:
      Result := SizeOf(TDateTime);
    stAsciiStream, stUnicodeStream, stBinaryStream:
      Result := SizeOf(Pointer);
    else
      Result := 0;
  end;
end;

{**
  Gets a stream from the specified columns.
  @param Buffer a row buffer.
  @param ColumnIndex an index of the column.
}
function TZRowAccessor.GetBlobObject(Buffer: PZRowBuffer;
  ColumnIndex: Integer): IZBlob;
var
  BlobPtr: PPointer;
  NullPtr: PByte;
begin
  BlobPtr := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := PByte(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  if NullPtr^ = 0 then
    Result := IZBlob(BlobPtr^)
  else Result := nil;
end;

{**
  Sets a blob into the specified columns.
  @param Buffer a row buffer.
  @param ColumnIndex an index of the column.
  @param Value a stream object to be set.
}
procedure TZRowAccessor.SetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer;
  Value: IZBlob);
var
  BlobPtr: PPointer;
  NullPtr: PByte;
begin
  BlobPtr := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := PByte(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  if NullPtr^ = 0 then
    IZBlob(BlobPtr^) := nil
  else BlobPtr^ := nil;

  IZBlob(BlobPtr^) := Value;

  if Value <> nil then
    NullPtr^ := 0
  else NullPtr^ := 1;
end;

{**
  Allocates a new row buffer and sets it into the variable.
  @param Buffer a pointer to row buffer.
  @return a pointer to the allocated buffer.
}
function TZRowAccessor.AllocBuffer(var Buffer: PZRowBuffer): PZRowBuffer;
begin
  GetMem(Buffer, FRowSize);
  InitBuffer(Buffer);
  Result := Buffer;
end;

{**
  Disposes the specified row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.DisposeBuffer(Buffer: PZRowBuffer);
begin
  if Assigned(Buffer) then
  begin
    ClearBuffer(Buffer);
    FreeMem(Buffer);
  end;
end;

{**
  Initializes the row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.InitBuffer(Buffer: PZRowBuffer);
begin
  if Assigned(Buffer) then
    with Buffer^ do
    begin
      Index := 0;
      BookmarkFlag := 0;//bfCurrent;
      UpdateType := utUnmodified;
      FillChar(Columns, FColumnsSize, 1);
    end;
end;

{**
  Copies the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
var
  I: Integer;
begin
  ClearBuffer(DestBuffer);
  with DestBuffer^ do
  begin
    Index := SrcBuffer^.Index;
    UpdateType := SrcBuffer^.UpdateType;
    BookmarkFlag := SrcBuffer^.BookmarkFlag;
    System.Move(SrcBuffer^.Columns, Columns, FColumnsSize);
    if FHasBlobs then
      for I := 0 to FColumnCount - 1 do
      begin
        if (FColumnTypes[I] in [stAsciiStream, stUnicodeStream, stBinaryStream])
          and (Columns[FColumnOffsets[I]] = 0) then
        begin
          Columns[FColumnOffsets[I]] := 1;
          SetBlobObject(DestBuffer, I + 1, GetBlobObject(SrcBuffer, I + 1));
        end;
      end;
  end;
end;

{**
  Moves the row buffer from source to destination row.
  Source buffer is cleaned up after the operation.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.MoveBuffer(SrcBuffer: PZRowBuffer;
  DestBuffer: PZRowBuffer);
begin
  CopyBuffer(SrcBuffer, DestBuffer);
  ClearBuffer(SrcBuffer);
end;

{**
  Clones the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.CloneBuffer(SrcBuffer: PZRowBuffer;
  DestBuffer: PZRowBuffer);
var
  I: Integer;
  Blob: IZBlob;
begin
  ClearBuffer(DestBuffer);
  with DestBuffer^ do
  begin
    Index := SrcBuffer^.Index;
    UpdateType := SrcBuffer^.UpdateType;
    BookmarkFlag := SrcBuffer^.BookmarkFlag;
    System.Move(SrcBuffer^.Columns, Columns, FColumnsSize);
    if FHasBlobs then
      for I := 0 to FColumnCount - 1 do
      begin
        if (FColumnTypes[I] in [stAsciiStream, stUnicodeStream, stBinaryStream])
          and (Columns[FColumnOffsets[I]] = 0) then
        begin
          Columns[FColumnOffsets[I]] := 1;
          Blob := GetBlobObject(SrcBuffer, I + 1);
          if Blob <> nil then
            Blob := Blob.Clone;
          SetBlobObject(DestBuffer, I + 1, Blob);
        end;
      end;
  end;
end;

{**
  Compares fields from two row buffers.
  @param Buffer1 the first row buffer to compare.
  @param Buffer2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZRowAccessor.CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
  ColumnIndices: TIntegerDynArray; ColumnDirs: TBooleanDynArray): Integer;
var
  I: Integer;
  ColumnIndex: Integer;
  Length1, Length2: SmallInt;
  ValuePtr1, ValuePtr2: Pointer;
  Blob1, Blob2: IZBlob;
  BlobEmpty1, BlobEmpty2: Boolean;

  function CompareFloat(Value1, Value2: Extended): Integer;
  begin
    Value1 := Value1 - Value2;
    if Value1 > 0 then
      Result := 1
    else if Value1 < 0 then
      Result := -1
    else Result := 0;
  end;

  function CompareBool(Value1, Value2: Boolean): Integer;
  begin
    if Value1 = Value2 then
      Result := 0
    else if Value1 = True then
      Result := 1
    else Result := -1;
  end;

  function CompareInt64(Value1, Value2: Int64): Integer;
  begin
    Value1 := Value1 - Value2;
    if Value1 > 0 then
      Result := 1
    else if Value1 < 0 then
      Result := -1
    else Result := 0;
  end;

begin
  Result := 0;
  for I := Low(ColumnIndices) to High(ColumnIndices) do
  begin
    ColumnIndex := ColumnIndices[I] - 1;
    { Checks for both Null columns. }
    if (Buffer1.Columns[FColumnOffsets[ColumnIndex]] = 1) and
      (Buffer2.Columns[FColumnOffsets[ColumnIndex]] = 1) then
      Continue;
    { Checks for not-Null and Null columns. }
    if Buffer1.Columns[FColumnOffsets[ColumnIndex]] <>
      Buffer2.Columns[FColumnOffsets[ColumnIndex]] then
    begin
      if not (FColumnTypes[ColumnIndex]
        in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
      begin
        Result := Buffer2.Columns[FColumnOffsets[ColumnIndex]] -
          Buffer1.Columns[FColumnOffsets[ColumnIndex]];
        if not ColumnDirs[I] then
          Result := -Result;
        Break;
      end;
    end;
    { Compares column values. }
    ValuePtr1 := @Buffer1.Columns[FColumnOffsets[ColumnIndex] + 1];
    ValuePtr2 := @Buffer2.Columns[FColumnOffsets[ColumnIndex] + 1];
    case FColumnTypes[ColumnIndex] of
      stByte:
        Result := PShortInt(ValuePtr1)^ - PShortInt(ValuePtr2)^;
      stShort:
        Result := PSmallInt(ValuePtr1)^ - PSmallInt(ValuePtr2)^;
      stInteger:
        Result := PInteger(ValuePtr1)^ - PInteger(ValuePtr2)^;
      stLong:
        Result := CompareInt64(PInt64(ValuePtr1)^, PInt64(ValuePtr2)^);
      stFloat:
        Result := CompareFloat(PSingle(ValuePtr1)^, PSingle(ValuePtr2)^);
      stDouble:
        Result := CompareFloat(PDouble(ValuePtr1)^, PDouble(ValuePtr2)^);
      stBigDecimal:
        Result := CompareFloat(PExtended(ValuePtr1)^, PExtended(ValuePtr2)^);
      stBoolean:
        Result := CompareBool(PWordBool(ValuePtr1)^, PWordBool(ValuePtr2)^);
      stDate, stTime, stTimestamp:
        Result := CompareFloat(PDateTime(ValuePtr1)^, PDateTime(ValuePtr2)^);
      stString:
        Result := Integer(ValuePtr1 <> ValuePtr2);
      stUnicodeString:
      {$IFNDEF VER130BELOW}
        Result := WideCompareStr(PWideChar(ValuePtr1), PWideChar(ValuePtr2));
      {$ELSE}
        Result := AnsiCompareStr(PWideChar(ValuePtr1), PWideChar(ValuePtr2));
      {$ENDIF}
      stBytes:
        begin
          Length1 := PSmallInt(ValuePtr1)^;
          Length2 := PSmallInt(ValuePtr2)^;
          ValuePtr1 := @Buffer1.Columns[FColumnOffsets[ColumnIndex]
            + 1 + SizeOf(SmallInt)];
          ValuePtr2 := @Buffer2.Columns[FColumnOffsets[ColumnIndex]
            + 1 + SizeOf(SmallInt)];
          Result := CompareStr(BufferToStr(ValuePtr1, Length1),
            BufferToStr(ValuePtr2, Length2));
        end;
      stAsciiStream, stBinaryStream, stUnicodeStream:
        begin
          Blob1 := GetBlobObject(Buffer1, ColumnIndex + 1);
          BlobEmpty1 := (Blob1 = nil) or (Blob1.IsEmpty);
          Blob2 := GetBlobObject(Buffer2, ColumnIndex + 1);
          BlobEmpty2 := (Blob2 = nil) or (Blob2.IsEmpty);
          if (BlobEmpty1 = True) and (BlobEmpty2 = True) then
            Continue
          else if (BlobEmpty1 <> BlobEmpty2) then
          begin
            if BlobEmpty1 then
              Result := -1
            else Result := 1;
          end
          else if FColumnTypes[ColumnIndex] = stAsciiStream then
            Result := Integer(Blob1.GetString <> Blob2.GetString)
          else if FColumnTypes[ColumnIndex] = stBinaryStream then
            Result := CompareStr(Blob1.GetString, Blob2.GetString)
          else if FColumnTypes[ColumnIndex] = stAsciiStream then
          {$IFNDEF VER130BELOW}
            Result := WideCompareStr(Blob1.GetUnicodeString, Blob2.GetUnicodeString);
          {$ELSE}
            Result := AnsiCompareStr(Blob1.GetString, Blob2.GetString);
          {$ENDIF}
        end;
    end;
    if Result <> 0 then
    begin
      if not ColumnDirs[I] then
        Result := -Result;
      Break;
    end;
  end;
end;

{**
  Cleans the specified row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.ClearBuffer(Buffer: PZRowBuffer);
var
  I: Integer;
begin
  with Buffer^ do
  begin
    Index := -1;
    UpdateType := utUnmodified;
    BookmarkFlag := 0;
    if FHasBlobs then
      for I := 0 to FColumnCount - 1 do
      begin
        if (FColumnTypes[I] in [stAsciiStream, stUnicodeStream, stBinaryStream])
          and (Columns[FColumnOffsets[I]] = 0) then
          SetBlobObject(Buffer, I + 1, nil);
      end;
    FillChar(Columns, FColumnsSize, 1);
  end;
end;

{**
  Allocates a new row buffer.
  @return a pointer to the allocated buffer.
}
function TZRowAccessor.Alloc: PZRowBuffer;
begin
  Result := AllocBuffer(FBuffer);
end;

{**
  Disposes an associated row buffer.
}
procedure TZRowAccessor.Dispose;
begin
  DisposeBuffer(FBuffer);
  FBuffer := nil;
end;

{**
  Initializes the associated row buffer.
}
procedure TZRowAccessor.Init;
begin
  InitBuffer(FBuffer);
end;

{**
  Copies the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.CopyTo(DestBuffer: PZRowBuffer);
begin
  CopyBuffer(FBuffer, DestBuffer);
end;

{**
  Copies the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.CopyFrom(SrcBuffer: PZRowBuffer);
begin
  CopyBuffer(SrcBuffer, FBuffer);
end;

{**
  Moves the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.MoveTo(DestBuffer: PZRowBuffer);
begin
  MoveBuffer(FBuffer, DestBuffer);
end;

{**
  Moves the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.MoveFrom(SrcBuffer: PZRowBuffer);
begin
  MoveBuffer(SrcBuffer, FBuffer);
end;

{**
  Clones the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.CloneTo(DestBuffer: PZRowBuffer);
begin
  CloneBuffer(FBuffer, DestBuffer);
end;

{**
  Clones the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.CloneFrom(SrcBuffer: PZRowBuffer);
begin
  CloneBuffer(SrcBuffer, FBuffer);
end;

{**
  Cleans the associated row buffer.
}
procedure TZRowAccessor.Clear;
begin
  ClearBuffer(FBuffer);
end;

{**
  Gets a pointer to the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a pointer to the column data buffer.
}
function TZRowAccessor.GetColumnData(ColumnIndex: Integer;
  var IsNull: Boolean): Pointer;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1];
  IsNull := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1;
end;

{**
  Gets a size of the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a size of the column data buffer.
}
function TZRowAccessor.GetColumnDataSize(ColumnIndex: Integer): Integer;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := FColumnLengths[ColumnIndex - 1];
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZRowAccessor.IsNull(ColumnIndex: Integer): Boolean;
var
  TempBlob: IZBlob;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1;
  if not Result and (FColumnTypes[ColumnIndex - 1] in [stAsciiStream,
    stBinaryStream, stUnicodeStream]) then
  begin
    TempBlob := GetBlobObject(FBuffer, ColumnIndex);
    Result := (TempBlob = nil) or TempBlob.IsEmpty;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetPChar(ColumnIndex: Integer;
  var IsNull: Boolean): PChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString:
        Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1];
      else
      begin
        FTemp := GetString(ColumnIndex, IsNull);
        Result := PChar(FTemp);
      end;
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetString(ColumnIndex: Integer;
  var IsNull: Boolean): string;
var
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  TempBytes := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 'True'
        else Result := 'False';
      stByte: Result := IntToStr(GetByte(ColumnIndex, IsNull));
      stShort: Result := IntToStr(GetShort(ColumnIndex, IsNull));
      stInteger: Result := IntToStr(GetInt(ColumnIndex, IsNull));
      stLong: Result := IntToStr(GetLong(ColumnIndex, IsNull));
      stFloat: Result := FloatToSQLStr(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := FloatToSQLStr(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := FloatToSQLStr(GetBigDecimal(ColumnIndex, IsNull));
      stString:
        Result := PChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
      stUnicodeString, stUnicodeStream: Result := GetUnicodeString(ColumnIndex, IsNull);
      stBytes: Result := BytesToStr(GetBytes(ColumnIndex, IsNull));
      stDate: Result := FormatDateTime('yyyy-mm-dd', GetDate(ColumnIndex, IsNull));
      stTime: Result := FormatDateTime('hh:mm:ss', GetTime(ColumnIndex, IsNull));
      stTimestamp:
        Result := FormatDateTime('yyyy-mm-dd hh:mm:ss',
          GetTimestamp(ColumnIndex, IsNull));
      stAsciiStream, stBinaryStream:
        begin
          TempBlob := GetBlobObject(FBuffer, ColumnIndex);
          if (TempBlob <> nil) and not TempBlob.IsEmpty then
            Result := TempBlob.GetString;
        end;
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetUnicodeString(ColumnIndex: Integer;
  var IsNull: Boolean): WideString;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stUnicodeString:
        Result := PWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
      stUnicodeStream:
        begin
          TempBlob := GetBlobObject(FBuffer, ColumnIndex);
          if (TempBlob <> nil) and not TempBlob.IsEmpty then
            Result := TempBlob.GetUnicodeString;
        end;
      else
        Result := GetString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZRowAccessor.GetBoolean(ColumnIndex: Integer;
  var IsNull: Boolean): Boolean;
var
  TempStr: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        Result := PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stByte: Result := GetByte(ColumnIndex, IsNull) <> 0;
      stShort: Result := GetShort(ColumnIndex, IsNull) <> 0;
      stInteger: Result := GetInt(ColumnIndex, IsNull) <> 0;
      stLong: Result := GetLong(ColumnIndex, IsNull) <> 0;
      stFloat: Result := GetFloat(ColumnIndex, IsNull) <> 0;
      stDouble: Result := GetDouble(ColumnIndex, IsNull) <> 0;
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull) <> 0;
      stString, stUnicodeString:
        begin
          TempStr := UpperCase(GetString(ColumnIndex, IsNull));
          Result := (TempStr = 'T') or (TempStr = 'Y') or (TempStr = 'TRUE')
            or (TempStr = 'YES');
        end;
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetByte(ColumnIndex: Integer;
  var IsNull: Boolean): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte:
        Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetShort(ColumnIndex: Integer;
  var IsNull: Boolean): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort:
        Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetInt(ColumnIndex: Integer;
  var IsNull: Boolean): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger:
        Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetLong(ColumnIndex: Integer;
  var IsNull: Boolean): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong:
        Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetFloat(ColumnIndex: Integer;
  var IsNull: Boolean): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat:
        Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stDouble: Result := GetDouble(ColumnIndex, IsNull);
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := SQLStrToFloatDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetDouble(ColumnIndex: Integer;
  var IsNull: Boolean): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := GetFloat(ColumnIndex, IsNull);
      stDouble:
        Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := SQLStrToFloatDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetBigDecimal(ColumnIndex: Integer;
  var IsNull: Boolean): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := GetFloat(ColumnIndex, IsNull);
      stDouble: Result := GetDouble(ColumnIndex, IsNull);
      stBigDecimal:
        Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stString, stUnicodeString:
        Result := SQLStrToFloatDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetBytes(ColumnIndex: Integer;
  var IsNull: Boolean): TByteDynArray;
var
  I: Integer;
  TempShort: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBytes:
        begin
          TempShort := PSmallInt(@FBuffer.Columns[
            FColumnOffsets[ColumnIndex - 1] + 1])^;
          SetLength(Result, TempShort);
          for I := 0 to TempShort - 1 do
          begin
            Result[I] := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]
              + 1 + SizeOf(SmallInt) + I];
          end;
        end;
      else
        Result := StrToBytes(GetString(ColumnIndex, IsNull));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetDate(ColumnIndex: Integer;
  var IsNull: Boolean): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := Int(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
      stString, stUnicodeString:
        Result := Trunc(AnsiSQLDateToDateTime(GetString(ColumnIndex, IsNull)));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetTime(ColumnIndex: Integer;
  var IsNull: Boolean): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := Frac(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
      stString, stUnicodeString:
        Result := Frac(AnsiSQLDateToDateTime(GetString(ColumnIndex, IsNull)));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZRowAccessor.GetTimestamp(ColumnIndex: Integer;
  var IsNull: Boolean): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stString, stUnicodeString:
        Result := AnsiSQLDateToDateTime(GetString(ColumnIndex, IsNull));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a stream of ASCII characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <char>LONGVARCHAR</char> values.
  The JDBC driver will
  do any necessary conversion from the database format into ASCII.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of one-byte ASCII characters; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetAsciiStream(ColumnIndex: Integer;
  var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    Result := TempBlob.GetStream
  else Result := nil;
  IsNull := Result = nil;
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  as a stream of Unicode characters.
  The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large<code>LONGVARCHAR</code>values.  The JDBC driver will
  do any necessary conversion from the database format into Unicode.
  The byte format of the Unicode stream must be Java UTF-8,
  as specified in the Java virtual machine specification.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream in Java UTF-8 byte format; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetUnicodeStream(ColumnIndex: Integer;
  var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    Result := TempBlob.GetStream
  else Result := nil;
  IsNull := Result = nil;
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a binary stream of
  uninterpreted bytes. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARBINARY</code> values.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of uninterpreted bytes;
    if the value is SQL <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetBinaryStream(ColumnIndex: Integer;
  var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) then
    Result := TempBlob.GetStream
  else Result := nil;
  IsNull := Result = nil;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZRowAccessor.GetBlob(ColumnIndex: Integer;
  var IsNull: Boolean): IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  Result := GetBlobObject(FBuffer, ColumnIndex);
  IsNull := Result = nil;
  if Result = nil then
  begin
    Result := TZAbstractBlob.CreateWithStream(nil);
    SetBlobObject(FBuffer, ColumnIndex, Result);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Variant</code> value.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
}
function TZRowAccessor.GetValue(ColumnIndex: Integer): TZVariant;
var
  ValuePtr: Pointer;
  IsNull: Boolean;
begin
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    ValuePtr := @FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1];
    case FColumnTypes[ColumnIndex - 1] of
      stByte:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PShortInt(ValuePtr)^;
        end;
      stShort:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PSmallInt(ValuePtr)^;
        end;
      stInteger:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PInteger(ValuePtr)^;
        end;
      stLong:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PInt64(ValuePtr)^;
        end;
      stFloat:
        begin
          Result.VType := vtFloat;
          Result.VFloat := PSingle(ValuePtr)^;
        end;
      stDouble:
        begin
          Result.VType := vtFloat;
          Result.VFloat := PDouble(ValuePtr)^;
        end;
      stBigDecimal:
        begin
          Result.VType := vtFloat;
          Result.VFloat := PExtended(ValuePtr)^;
        end;
      stBoolean:
        begin
          Result.VType := vtBoolean;
          Result.VBoolean := PWordBool(ValuePtr)^;
        end;
      stDate, stTime, stTimestamp:
        begin
          Result.VType := vtDateTime;
          Result.VDateTime := PDateTime(ValuePtr)^;
        end;
      stString:
        begin
          Result.VType := vtString;
          Result.VString := PChar(ValuePtr);
        end;
      stUnicodeString:
        begin
          Result.VType := vtUnicodeString;
          Result.VUnicodeString := PWideChar(ValuePtr);
        end;
      stBytes, stAsciiStream, stBinaryStream:
        begin
          Result.VType := vtString;
          Result.VString := GetString(ColumnIndex, IsNull);
        end;
      stUnicodeStream:
        begin
          Result.VType := vtUnicodeString;
          Result.VUnicodeString := GetUnicodeString(ColumnIndex, IsNull);
        end;
      else
        Result.VType := vtNull;
    end;
  end else
    Result.VType := vtNull;
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a not nullable column a null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNotNull(ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1)
    and (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    SetBlobObject(FBuffer, ColumnIndex, nil);
  end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
end;

{**
  Gives a nullable column a null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNull(ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0)
    and (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    SetBlobObject(FBuffer, ColumnIndex, nil);
  end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 1;
end;

{**
  Sets the designated column with a <code>boolean</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBoolean(ColumnIndex: Integer; Value: Boolean);
var
  TempInt: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  if Value then TempInt := 1
  else TempInt := 0;

  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stByte: SetByte(ColumnIndex, TempInt);
    stShort: SetShort(ColumnIndex, TempInt);
    stInteger: SetInt(ColumnIndex, TempInt);
    stLong: SetLong(ColumnIndex, TempInt);
    stFloat: SetFloat(ColumnIndex, TempInt);
    stDouble: SetDouble(ColumnIndex, TempInt);
    stBigDecimal: SetBigDecimal(ColumnIndex, TempInt);
    stString, stUnicodeString:
      if Value then SetString(ColumnIndex, 'True')
      else SetString(ColumnIndex, 'False');
  end;
end;

{**
  Sets the designated column with a <code>byte</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetByte(ColumnIndex: Integer;
  Value: ShortInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stShort: SetShort(ColumnIndex, Value);
    stInteger: SetInt(ColumnIndex, Value);
    stLong: SetLong(ColumnIndex, Value);
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString: SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>short</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetShort(ColumnIndex: Integer; Value: SmallInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Value);
    stShort:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stInteger: SetInt(ColumnIndex, Value);
    stLong: SetLong(ColumnIndex, Value);
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with an <code>int</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetInt(ColumnIndex: Integer; Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Value);
    stShort: SetShort(ColumnIndex, Value);
    stInteger:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stLong: SetLong(ColumnIndex, Value);
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>long</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetLong(ColumnIndex: Integer; Value: Int64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Value);
    stShort: SetShort(ColumnIndex, Value);
    stInteger: SetInt(ColumnIndex, Value);
    stLong:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>float</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetFloat(ColumnIndex: Integer; Value: Single);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Trunc(Value));
    stShort: SetShort(ColumnIndex, Trunc(Value));
    stInteger: SetInt(ColumnIndex, Trunc(Value));
    stLong: SetLong(ColumnIndex, Trunc(Value));
    stFloat:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, FloatToSQLStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>double</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetDouble(ColumnIndex: Integer; Value: Double);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Trunc(Value));
    stShort: SetShort(ColumnIndex, Trunc(Value));
    stInteger: SetInt(ColumnIndex, Trunc(Value));
    stLong: SetLong(ColumnIndex, Trunc(Value));
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, FloatToSQLStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBigDecimal(ColumnIndex: Integer; Value: Extended);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Trunc(Value));
    stShort: SetShort(ColumnIndex, Trunc(Value));
    stInteger: SetInt(ColumnIndex, Trunc(Value));
    stLong: SetLong(ColumnIndex, Trunc(Value));
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stString, stUnicodeString:
      SetString(ColumnIndex, FloatToSQLStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetPChar(ColumnIndex: Integer; Value: PChar);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stString:
      begin
        if Value <> nil then
        begin
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
          StrLCopy(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1], Value,
            FColumnLengths[ColumnIndex - 1] - 1);
        end else
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 1;
      end;
    else
      SetString(ColumnIndex, Value);
  end;
end;

{**
  Sets the designated column with a <code>String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetString(ColumnIndex: Integer; Value: string);
var
  TempStr: string;
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      begin
        TempStr := UpperCase(Value);
        SetBoolean(ColumnIndex, (TempStr = 'Y') or (TempStr = 'T')
          or (TempStr = 'YES') or (TempStr = 'TRUE'));
      end;
    stByte: SetByte(ColumnIndex, StrToIntDef(Value, 0));
    stShort: SetShort(ColumnIndex, StrToIntDef(Value, 0));
    stInteger: SetInt(ColumnIndex, StrToIntDef(Value, 0));
    stLong: SetLong(ColumnIndex, StrToIntDef(Value, 0));
    stFloat: SetFloat(ColumnIndex, SQLStrToFloatDef(Value, 0));
    stDouble: SetDouble(ColumnIndex, SQLStrToFloatDef(Value, 0));
    stBigDecimal: SetBigDecimal(ColumnIndex, SQLStrToFloatDef(Value, 0));
    stString:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        StrPLCopy(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1], Value,
          FColumnLengths[ColumnIndex - 1] - 1);
      end;
    stUnicodeString: SetUnicodeString(ColumnIndex, Value);
    stBytes: SetBytes(ColumnIndex, StrToBytes(Value));
    stDate: SetDate(ColumnIndex, AnsiSQLDateToDateTime(Value));
    stTime: SetTime(ColumnIndex, AnsiSQLDateToDateTime(Value));
    stTimestamp: SetTimestamp(ColumnIndex, AnsiSQLDateToDateTime(Value));
    stAsciiStream, stUnicodeStream, stBinaryStream:
      GetBlob(ColumnIndex, IsNull).SetString(Value);
  end;
end;

{**
  Sets the designated column with a <code>WideString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUnicodeString(ColumnIndex: Integer;
  Value: WideString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stUnicodeString:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        Value := System.Copy(Value, 1, FColumnLengths[ColumnIndex - 1] div 2 - 1);

        if Length(Value) > 0 then
          System.Move(PWideString(Value)^, Pointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^, Length(Value) * 2 + 2)
        else
          PWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := #0;
      end;
    else
      SetString(ColumnIndex, Value);
  end;
end;

{**
  Sets the designated column with a <code>byte</code> array value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBytes(ColumnIndex: Integer; Value: TByteDynArray);
var
  I: Integer;
  TempShort: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  if Value <> nil then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBytes:
        begin
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
          TempShort := MinIntValue([High(Value) + 1, FColumnLengths[ColumnIndex - 1]
            - SizeOf(SmallInt)]);
          PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            TempShort;
          for I := 0 to TempShort - 1 do
          begin
            FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1
              + SizeOf(SmallInt) + I] := Value[I];
          end;
        end;
      else
        SetString(ColumnIndex, BytesToStr(Value));
    end;
  end else
    SetNull(ColumnIndex);
end;

{**
  Sets the designated column with a <code>java.sql.Date</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetDate(ColumnIndex: Integer; Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stDate:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          Trunc(Value);
      end;
    stTimestamp: SetTimestamp(ColumnIndex, Trunc(Value));
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('yyyy-mm-dd', Value));
  end;
end;

{**
  Sets the designated column with a <code>java.sql.Time</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetTime(ColumnIndex: Integer; Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stTime:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          Frac(Value);
      end;
    stTimestamp: SetTimestamp(ColumnIndex, Frac(Value));
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('hh:nn:ss', Value));
  end;
end;

{**
  Sets the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetTimestamp(ColumnIndex: Integer; Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stDate: SetDate(ColumnIndex, Value);
    stTime: SetTime(ColumnIndex, Value);
    stTimestamp:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('yyyy-mm-dd hh:nn:ss', Value));
  end;
end;

{**
  Sets the designated column with an ascii stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetAsciiStream(ColumnIndex: Integer; Value: TStream);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the designated column with a binary stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZRowAccessor.SetBinaryStream(ColumnIndex: Integer; Value: TStream);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the designated column with a character stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUnicodeStream(ColumnIndex: Integer;
  Value: TStream);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the blob wrapper object to the specified column.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @param Value a blob wrapper object to be set.
}
procedure TZRowAccessor.SetBlob(ColumnIndex: Integer; Value: IZBlob);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  SetBlobObject(FBuffer, ColumnIndex, Value);
end;

{**
  Sets the designated column with a <code>Variant</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetValue(ColumnIndex: Integer; Value: TZVariant);
begin
  case Value.VType of
    vtNull: SetNull(ColumnIndex);
    vtBoolean: SetBoolean(ColumnIndex, Value.VBoolean);
    vtInteger: SetLong(ColumnIndex, Value.VInteger);
    vtFloat: SetBigDecimal(ColumnIndex, Value.VFloat);
    vtString: SetString(ColumnIndex, Value.VString);
    vtUnicodeString: SetUnicodeString(ColumnIndex, Value.VUnicodeString);
    vtDateTime: SetTimestamp(ColumnIndex, Value.VDateTime);
  end;
end;

end.
