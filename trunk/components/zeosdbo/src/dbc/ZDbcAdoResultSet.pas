{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Ado Resultset common functionality              }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcAdoResultSet;

interface

{$I ZDbc.inc}

uses
{$IFNDEF UNIX}
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$ENDIF}
{$IFNDEF VER130BELOW}
  DateUtils,
{$ENDIF}
  Classes, SysUtils, ZClasses, ZSysUtils, ZCollections, ZDbcIntfs,
  ZDbcGenericResolver, ZDbcCachedResultSet, ZDbcCache, ZDbcResultSet,
  ZDbcResultsetMetadata, ZCompatibility, ZDbcAdo, ZPlainAdoDriver, ZPlainAdo;

type
  {** Implements Ado ResultSet. }
  TZAdoResultSet = class(TZAbstractResultSet)
  private
    AdoColTypeCache: TIntegerDynArray;
    AdoColumnCount: Integer;
    FFirstFetch: Boolean;
  protected
    FAdoRecordSet: ZPlainAdo.RecordSet;
    procedure Open; override;
  public
    constructor Create(Statement: IZStatement; SQL: string;
      AdoRecordSet: ZPlainAdo.RecordSet);
    destructor Destroy; override;

    procedure Close; override;
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function GetRow: Integer; override;
    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): string; override;
    function GetUnicodeString(ColumnIndex: Integer): WideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): ShortInt; override;
    function GetShort(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TByteDynArray; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
  end;

  {** Implements a cached resolver with Ado specific functionality. }
  TZAdoCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FHandle: ZPlainAdo.Command;
    FAutoColumnIndex: Integer;
  public
    constructor Create(Handle: ZPlainAdo.Connection;
      Statement: IZStatement; Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  Math, OleDB, ZMessages, ZDbcUtils, ZDbcAdoUtils;

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param AdoRecordSet a ADO recordset object, the source of the ResultSet.
}
constructor TZAdoResultSet.Create(Statement: IZStatement; SQL: string; AdoRecordSet: ZPlainAdo.RecordSet);
begin
  inherited Create(Statement, SQL, nil);
  FAdoRecordSet := AdoRecordSet;
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAdoResultSet.Destroy;
begin
  Close;
  inherited;
end;

{**
  Opens this recordset and initializes the Column information.
}
procedure TZAdoResultSet.Open;
var
  OleDBRowset: IUnknown;
  OleDBColumnsInfo: IColumnsInfo;
  pcColumns: Cardinal;
  prgInfo, OriginalprgInfo: PDBColumnInfo;
  ppStringsBuffer: PWideChar;
  I: Integer;
  FieldSize: Integer;
  ColumnInfo: TZColumnInfo;
  ColName: string;
  ColType: Integer;
  HasAutoIncProp: Boolean;
  F: ZPlainAdo.Field20;
  S: string;
  J: Integer;
begin
//Check if the current statement can return rows
  if not Assigned(FAdoRecordSet) or (FAdoRecordSet.State = adStateClosed) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  (FAdoRecordSet as ADORecordsetConstruction).Get_Rowset(OleDBRowset);
  OleDBRowset.QueryInterface(IColumnsInfo, OleDBColumnsInfo);

  OleDBColumnsInfo.GetColumnInfo(pcColumns, prgInfo, ppStringsBuffer);
  OriginalprgInfo := prgInfo;

  { Fills the column info }
  ColumnsInfo.Clear;
  AdoColumnCount := FAdoRecordSet.Fields.Count;
  SetLength(AdoColTypeCache, AdoColumnCount);

  HasAutoIncProp := False;
  if AdoColumnCount > 0 then
    for I := 0 to FAdoRecordSet.Fields.Item[0].Properties.Count - 1 do
      if FAdoRecordSet.Fields.Item[0].Properties.Item[I].Name = 'ISAUTOINCREMENT' then
      begin
        HasAutoIncProp := True;
        Break;
      end;

  if Assigned(prgInfo) then
    if prgInfo.iOrdinal = 0 then
      Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));

  for I := 0 to AdoColumnCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;

    F := FAdoRecordSet.Fields.Item[I];
    ColName := F.Name;
    ColType := F.Type_;
    ColumnInfo.ColumnLabel := ColName;
    ColumnInfo.ColumnName := ColName;
    ColumnInfo.ColumnType := ConvertAdoToSqlType(ColType);
    FieldSize := F.DefinedSize;
    if FieldSize < 0 then
      FieldSize := 0;
    ColumnInfo.ColumnDisplaySize := FieldSize;
    ColumnInfo.Precision := FieldSize;
    ColumnInfo.Currency := ColType = adCurrency;
    ColumnInfo.Signed := False;
    S := '';
    for J := 0 to F.Properties.Count - 1 do
      S := S+F.Properties.Item[J].Name + '=' + VarToStr(F.Properties.Item[J].Value) + ', ';
    if HasAutoIncProp then
      ColumnInfo.AutoIncrement := F.Properties.Item['ISAUTOINCREMENT'].Value;
    if ColType in [adTinyInt, adSmallInt, adInteger, adBigInt, adCurrency, adDecimal, adDouble, adNumeric, adSingle] then
      ColumnInfo.Signed := True;

    ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0) and (F.Properties.Item['BASECOLUMNNAME'].Value <> null) and not ColumnInfo.AutoIncrement;
    ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0) or ColumnInfo.AutoIncrement;
    ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
    if (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) <> 0 then
    case ColumnInfo.ColumnType of
      stString: ColumnInfo.ColumnType := stAsciiStream;
      stUnicodeString: ColumnInfo.ColumnType := stUnicodeStream;
    end;

    ColumnsInfo.Add(ColumnInfo);

    AdoColTypeCache[I] := ColType;
    Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));
  end;
  if Assigned(ppStringsBuffer) then ZAdoMalloc.Free(ppStringsBuffer);
  if Assigned(OriginalprgInfo) then ZAdoMalloc.Free(OriginalprgInfo);
  FFirstFetch := True;
  inherited;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  ADO resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAdoResultSet.Close;
begin
  FAdoRecordSet := nil;
  inherited;
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZAdoResultSet.Next: Boolean;
begin
  Result := False;
  if FAdoRecordSet.BOF and FAdoRecordSet.EOF then
    Exit;
  if FAdoRecordSet.BOF then
    FAdoRecordSet.MoveFirst
  else
    if not FAdoRecordSet.EOF and not FFirstFetch then
      FAdoRecordSet.MoveNext;
  FFirstFetch := False;
  Result := not FAdoRecordSet.EOF;
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZAdoResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  if FAdoRecordSet.EOF or FAdoRecordSet.BOF then FAdoRecordSet.MoveFirst;
  if Row > 0 then
    FAdoRecordSet.Move(Row - 1, adBookmarkFirst)
  else
    FAdoRecordSet.Move(Abs(Row) - 1, adBookmarkLast);
  Result := not (FAdoRecordSet.EOF or FAdoRecordSet.BOF);
end;

{**
  Retrieves the current row number.  The first row is number 1, the
  second number 2, and so on.
  @return the current row number; <code>0</code> if there is no current row
}
function TZAdoResultSet.GetRow: Integer;
begin
  if FAdoRecordSet.EOF or FAdoRecordSet.BOF then
    Result := -1
  else
    Result := FAdoRecordSet.AbsolutePosition;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAdoResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  Result := VarIsNull(FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value) or
            VarIsEmpty(FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetString(ColumnIndex: Integer): string;
var
  NL: Integer;
begin
  Result := '';
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  NL := Length(Result);
  while (NL > 0) and (Result[NL] = ' ') do Dec(NL);
  SetLength(Result, NL);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetUnicodeString(ColumnIndex: Integer): WideString;
var
  NL: Integer;
begin
  Result := '';
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  NL := Length(Result);
  while (NL > 0) and (Result[NL] = ' ') do Dec(NL);
  SetLength(Result, NL);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAdoResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  Result := False;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := False;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetByte(ColumnIndex: Integer): ShortInt;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
{$IFDEF VER130BELOW}
    Result := Integer(FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value);
{$ELSE}
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
{$ENDIF}
  except
    Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
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
function TZAdoResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
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
function TZAdoResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
begin
  SetLength(Result, 0);
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
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
function TZAdoResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;
  try
    Result := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  except
    Result := 0;
  end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAdoResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  V: Variant;
  P: Pointer;
begin
  Result := nil;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;

  V := FAdoRecordSet.Fields.Item[ColumnIndex - 1].Value;
  if VarIsStr(V) then
  begin
    Result := TZAbstractBlob.CreateWithStream(nil);
    Result.SetString(V);
  end;
  if VarIsArray(V) then
  begin
    P := VarArrayLock(V);
    try
      Result := TZAbstractBlob.CreateWithData(P, VarArrayHighBound(V, 1));
    finally
      VarArrayUnLock(V);
    end;
  end;
end;


{ TZAdoCachedResolver }

{**
  Creates a Ado specific cached resolver object.
  @param PlainDriver a native Ado plain driver.
  @param Handle a Ado specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZAdoCachedResolver.Create(Handle: ZPlainAdo.Connection;
  Statement: IZStatement; Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FHandle := ZPlainAdo.CoCommand.Create;
  FHandle._Set_ActiveConnection(Handle);
  FHandle.CommandText := 'SELECT @@IDENTITY';
  FHandle.CommandType := adCmdText;

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := 0;
  for I := 1 to Metadata.GetColumnCount do
  begin
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte, stShort, stInteger, stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
  end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZAdoCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Recordset: ZPlainAdo.Recordset;
  RA: OleVariant;
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) and (FAutoColumnIndex > 0)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    Recordset := FHandle.Execute(RA, null, 0);
    if Recordset.RecordCount > 0 then
    {$IFNDEF VER130BELOW}
      NewRowAccessor.SetLong(FAutoColumnIndex, Recordset.Fields.Item[0].Value);
    {$ELSE}
      NewRowAccessor.SetLong(FAutoColumnIndex,
        Integer(Recordset.Fields.Item[0].Value));
    {$ENDIF}
  end;
end;

end.

