{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcASAResultSet;

interface

{$I ZDbc.inc}

uses
  Classes, ZSysUtils, ZCollections, ZDbcIntfs, ZDbcResultSet, ZDbcASA,
  ZPlainASADriver, ZClasses, ZCompatibility, ZDbcResultSetMetadata,
  ZDbcASAUtils, ZMessages, ZVariant;

type

  {** Implements ASA ResultSet. }
  TZASAResultSet = class(TZAbstractResultSet)
  private
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FCursorName: string;
    FStmtNum: SmallInt;
    FSqlData: IZASASQLDA;
    FParamsSqlData: IZASASQLDA;
    FUpdateSqlData: IZASASQLDA;
    FASAConnection: IZASAConnection;
    FInsert: Boolean;
    FUpdate: Boolean;
    FDelete: Boolean;
  protected
    procedure Open; override;
    procedure PrepareUpdateSQLData; virtual;
    function GetFieldValue(ColumnIndex: Integer): Variant;
  public
    constructor Create(Statement: IZStatement; SQL: string;
      var StmtNum: SmallInt; CursorName: string;
      SqlData: IZASASQLDA; ParamsSqlData: IZASASQLDA;
      CachedBlob: boolean);

    function GetCursorName: string; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): string; override;
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

    function Last: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function MoveRelative(Rows: Integer): Boolean; override;
    function Previous: Boolean; override;
    function Next: Boolean; override;

    function RowUpdated: Boolean; override;
    function RowInserted: Boolean; override;
    function RowDeleted: Boolean; override;

    procedure UpdateNull(ColumnIndex: Integer); override;
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean); override;
    procedure UpdateByte(ColumnIndex: Integer; Value: ShortInt); override;
    procedure UpdateShort(ColumnIndex: Integer; Value: SmallInt); override;
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer); override;
    procedure UpdateLong(ColumnIndex: Integer; Value: Int64); override;
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single); override;
    procedure UpdateDouble(ColumnIndex: Integer; Value: Double); override;
    procedure UpdateBigDecimal(ColumnIndex: Integer; Value: Extended); override;
    procedure UpdatePChar(ColumnIndex: Integer; Value: PChar); override;
    procedure UpdateString(ColumnIndex: Integer; const Value: string); override;
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: WideString);
      override;
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TByteDynArray); override;
    procedure UpdateDate(ColumnIndex: Integer; Value: TDateTime); override;
    procedure UpdateTime(ColumnIndex: Integer; Value: TDateTime); override;
    procedure UpdateTimestamp(ColumnIndex: Integer; Value: TDateTime); override;
    procedure UpdateAsciiStream(ColumnIndex: Integer; Value: TStream); override;
    procedure UpdateUnicodeStream(ColumnIndex: Integer; Value: TStream); override;
    procedure UpdateBinaryStream(ColumnIndex: Integer; Value: TStream); override;
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant); override;

    procedure InsertRow; override;
    procedure UpdateRow; override;
    procedure DeleteRow; override;
    procedure RefreshRow; override;
    procedure CancelRowUpdates; override;
    procedure MoveToInsertRow; override;
    procedure MoveToCurrentRow; override;

    property SQLData: IZASASQLDA read FSQLData;
  end;

  {** Implements external blob wrapper object for PostgreSQL. }
  TZASABlob = class(TZAbstractBlob)
  private
    FBlobRead: Boolean;
    FResultSet: TZASAResultSet;
    FColID: Integer;
  protected
    procedure ReadBlob;
  public
    constructor Create( ResultSet: TZASAResultSet; ColID: Integer);
    constructor CreateWithStream(Stream: TStream);
    constructor CreateWithData(Data: Pointer; Size: Integer);

    function IsEmpty: Boolean; override;
    function Clone: IZBlob; override;
    function GetStream: TStream; override;
    function GetString: string; override;
    function GetUnicodeString: WideString; override;
    function GetBytes: TByteDynArray; override;
    property BlobSize;
    property BlobData;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZDbcUtils, SysUtils, Math, ZdbcLogging, ZdbcStatement;

{ TZASAResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZASAResultSet.Create(Statement: IZStatement; SQL: string;
      var StmtNum: SmallInt; CursorName: string;
      SqlData: IZASASQLDA; ParamsSqlData: IZASASQLDA;
      CachedBlob: boolean);
begin
  inherited Create( Statement, SQL, nil);

  FFetchStat := 0;
  FSqlData := SqlData;
  FCursorName := CursorName;
  FCachedBlob := CachedBlob;
  FASAConnection := Statement.GetConnection as IZASAConnection;
  FDelete := False;
  FInsert := False;
  FUpdate := False;

  FParamsSqlData := ParamsSqlData;
  FStmtNum := StmtNum;
  ResultSetType := rtScrollSensitive;
  ResultSetConcurrency := rcUpdatable;

  Open;
end;

{**
   Return field value by it index
   @param the index column 0 first, 1 second ...
   @return the field value as variant type
}
function TZASAResultSet.GetFieldValue(ColumnIndex: Integer): Variant;
begin
  CheckClosed;
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetValue( ColumnIndex - 1)
  else
    Result := FSqlData.GetValue( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
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
function TZASAResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetBigDecimal( ColumnIndex - 1)
  else
    Result := FSqlData.GetBigDecimal( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZASAResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  Blob: TZASABlob;
begin
  Result := nil;
  CheckClosed;
  CheckBlobColumn(ColumnIndex);

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;

  Blob := TZASABlob.Create( Self, ColumnIndex - 1);
  if FCachedBlob then
    Blob.ReadBlob;
  Result := Blob;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZASAResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBoolean);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetBoolean( ColumnIndex - 1)
  else
    Result := FSqlData.GetBoolean( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetByte(ColumnIndex: Integer): ShortInt;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stByte);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetByte( ColumnIndex - 1)
  else
    Result := FSqlData.GetByte( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
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
function TZASAResultSet.GetBytes(
  ColumnIndex: Integer): TByteDynArray;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBytes);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetBytes( ColumnIndex - 1)
  else
    Result := FSqlData.GetBytes( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDate);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetDate( ColumnIndex - 1)
  else
    Result := FSqlData.GetDate( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDouble);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetDouble( ColumnIndex - 1)
  else
    Result := FSqlData.GetDouble( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetFloat( ColumnIndex - 1)
  else
    Result := FSqlData.GetFloat( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stInteger);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetInt( ColumnIndex - 1)
  else
    Result := FSqlData.GetInt( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stLong);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetLong( ColumnIndex - 1)
  else
    Result := FSqlData.GetLong( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stShort);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetShort( ColumnIndex - 1)
  else
    Result := FSqlData.GetShort( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.GetString(ColumnIndex: Integer): string;
begin
  CheckClosed;
  CheckColumnConvertion( ColumnIndex, stString);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetString( ColumnIndex - 1)
  else
    Result := FSqlData.GetString( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTime);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetTime( ColumnIndex - 1)
  else
    Result := FSqlData.GetTime( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
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
function TZASAResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTimestamp);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetTimestamp( ColumnIndex - 1)
  else
    Result := FSqlData.GetTimestamp( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZASAResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.IsNull( ColumnIndex - 1)
  else
    Result := FSqlData.IsNull(ColumnIndex - 1);
end;

function TZASAResultSet.Last: Boolean;
begin
  if LastRowNo <> MaxInt then
    Result := MoveAbsolute( LastRowNo)
  else
    Result := MoveAbsolute( -1);
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
function TZASAResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  if (MaxRows > 0) and (Row >= MaxRows) then
    Exit;

  FASAConnection.GetPlainDriver.db_fetch( FASAConnection.GetDBHandle,
    PChar( FCursorName), CUR_ABSOLUTE, Row, FSqlData.GetData, BlockSize, CUR_FORREGULAR);
  ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, lcOther);

  if FASAConnection.GetDBHandle.sqlCode <> SQLE_NOTFOUND then
  begin
    RowNo := Row;
    Result := True;
    FFetchStat := 0;
    FDelete := False;
    FInsert := False;
    FUpdate := False;
  end else begin
    FFetchStat := FASAConnection.GetDBHandle.sqlerrd[2];
    if FFetchStat > 0 then
      LastRowNo := Max( Row - FFetchStat, 0);
  end;
end;

function TZASAResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := False;
  if (MaxRows > 0) and ( Abs( RowNo) + Rows >= MaxRows) then
    Exit;

  FASAConnection.GetPlainDriver.db_fetch( FASAConnection.GetDBHandle,
    PChar( FCursorName), CUR_RELATIVE, Rows, FSqlData.GetData, BlockSize, CUR_FORREGULAR);
  ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, lcOther);

  if FASAConnection.GetDBHandle.sqlCode <> SQLE_NOTFOUND then
  begin
    if ( RowNo > 0) or ( RowNo + Rows < 0) then
      RowNo := RowNo + Rows;
    Result := True;
    FFetchStat := 0;
    FDelete := False;
    FInsert := False;
    FUpdate := False;
  end else begin
    FFetchStat := FASAConnection.GetDBHandle.sqlerrd[2];
    if ( FFetchStat > 0) and ( RowNo > 0) then
      LastRowNo := Max( RowNo + Rows - FFetchStat, 0);
  end;
end;

function TZASAResultSet.Previous: Boolean;
begin
  Result := MoveRelative( -1);
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
function TZASAResultSet.Next: Boolean;
begin
  Result := MoveRelative( 1);
end;

{**
  Opens this recordset.
}
procedure TZASAResultSet.Open;
var
  i: Integer;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
begin
  if FStmtNum = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
  for i := 0 to FSqlData.GetFieldCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo, FSqlData  do
    begin
      FieldSqlType := GetFieldSqlType(I);
      ColumnName := GetFieldName(I);
//      TableName := GetFieldRelationName(I);
      ColumnLabel := ColumnName;
      ColumnType := FieldSqlType;

      case FieldSqlType of
        stString,
        stUnicodeString: Precision := GetFieldLength(I);
      end;

      ReadOnly := False;

      if IsNullable(I) then
        Nullable := ntNullable
      else
        Nullable := ntNoNulls;
      Nullable := ntNullable;

      Scale := GetFieldScale(I);
      AutoIncrement := False;
      //Signed := False;
      CaseSensitive := False;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;
  LastRowNo := MaxInt;
  inherited Open;
end;

function TZASAResultSet.GetCursorName: string;
begin
  Result := FCursorName;
end;

function TZASAResultSet.RowUpdated: Boolean;
begin
  Result := FUpdate;
end;

function TZASAResultSet.RowInserted: Boolean;
begin
  Result := FInsert;
end;

function TZASAResultSet.RowDeleted: Boolean;
begin
  Result := FDelete;
end;

procedure TZASAResultSet.PrepareUpdateSQLData;
begin
  FUpdate := not FInsert;
  if not Assigned( FUpdateSQLData) then
  begin
    FUpdateSQLData := TZASASQLDA.Create( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, FCursorName, FSQLData.GetFieldCount);
  end else if FUpdateSQLData.GetFieldCount = 0 then
    FUpdateSQLData.AllocateSQLDA( FSQLData.GetFieldCount);
end;

procedure TZASAResultSet.UpdateNull(ColumnIndex: Integer);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateNull( ColumnIndex, True);
end;

procedure TZASAResultSet.UpdateBoolean(ColumnIndex: Integer; Value: Boolean);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBoolean( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateByte(ColumnIndex: Integer; Value: ShortInt);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateByte( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateShort(ColumnIndex: Integer; Value: SmallInt);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateShort( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateInt(ColumnIndex: Integer; Value: Integer);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateInt( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateLong(ColumnIndex: Integer; Value: Int64);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateLong( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateFloat(ColumnIndex: Integer; Value: Single);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateFloat( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateDouble(ColumnIndex: Integer; Value: Double);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateDouble( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateBigDecimal(ColumnIndex: Integer; Value: Extended);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBigDecimal( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdatePChar(ColumnIndex: Integer; Value: PChar);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdatePChar( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateString(ColumnIndex: Integer; const Value: string);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateString( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateUnicodeString(ColumnIndex: Integer; const Value: WideString);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdatePChar( ColumnIndex, PChar( Value));
end;

procedure TZASAResultSet.UpdateBytes(ColumnIndex: Integer; const Value: TByteDynArray);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBytes( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateDate(ColumnIndex: Integer; Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateDate( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateTime(ColumnIndex: Integer; Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateTime( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateTimestamp(ColumnIndex: Integer; Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateTimestamp( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateAsciiStream(ColumnIndex: Integer; Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateUnicodeStream(ColumnIndex: Integer; Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateBinaryStream(ColumnIndex: Integer; Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateValue(ColumnIndex: Integer; const Value: TZVariant);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateValue( ColumnIndex, EncodeVariant( Value));
end;

procedure TZASAResultSet.InsertRow;
begin
  if Assigned( FUpdateSQLData) and FInsert then
  begin
    FASAConnection.GetPlainDriver.db_put_into( FASAConnection.GetDBHandle,
      PChar( FCursorName), FUpdateSQLData.GetData, FSQLData.GetData);
    ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, lcOther, 'Insert row');

    FInsert := false;
    FUpdateSQLData.FreeSQLDA;
  end;
end;

procedure TZASAResultSet.UpdateRow;
begin
  if Assigned( FUpdateSQLData) and FUpdate then
  begin
    FASAConnection.GetPlainDriver.db_update( FASAConnection.GetDBHandle,
      PChar( FCursorName), FUpdateSQLData.GetData);
    ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, lcOther, 'Update row:' + IntToStr( RowNo));

    FUpdate := false;
    FUpdateSQLData.FreeSQLDA;
  end;
end;

procedure TZASAResultSet.DeleteRow;
begin
  FASAConnection.GetPlainDriver.db_delete( FASAConnection.GetDBHandle,
    PChar( FCursorName));
  ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, lcOther, 'Delete row:' + IntToStr( RowNo));

  FDelete := True;
  if LastRowNo <> MaxInt then
    LastRowNo := LastRowNo - FASAConnection.GetDBHandle.sqlerrd[2];
end;

procedure TZASAResultSet.RefreshRow;
begin
  MoveRelative( 0);
end;

procedure TZASAResultSet.CancelRowUpdates;
begin
  FUpdate := false;
  if Assigned( FUpdateSQLData) then
    FUpdateSQLData.FreeSQLDA;
end;

procedure TZASAResultSet.MoveToInsertRow;
begin
  FInsert := true;
end;

procedure TZASAResultSet.MoveToCurrentRow;  
begin
  FInsert := false;
  if Assigned( FUpdateSQLData) then
    FUpdateSQLData.FreeSQLDA;
end;

{ TZASABlob }

function TZASABlob.Clone: IZBlob;
var
  Dt: Pointer;
begin
  Dt := nil;
  if BlobSize > 0 then
  begin
    GetMem( Dt, BlobSize);
    System.Move( BlobData^, Dt^, BlobSize);
  end;
  Result := TZASABlob.CreateWithData( Dt, BlobSize);
end;

{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
constructor TZASABlob.Create( ResultSet: TZASAResultSet; ColID: Integer);
begin
  inherited Create;
  FBlobRead := False;
  FResultSet := ResultSet;
  FColID := ColID;
end;

constructor TZASABlob.CreateWithStream(Stream: TStream);
begin
  inherited CreateWithStream( Stream);
  FBlobRead := true;
end;

constructor TZASABlob.CreateWithData(Data: Pointer; Size: Integer);
begin
  inherited Create;
  BlobData := Data;
  BlobSize := Size;
  Updated := False;
  FBlobRead := true;
end;

function TZASABlob.GetBytes: TByteDynArray;
begin
  ReadBlob;
  Result := inherited GetBytes;
end;

function TZASABlob.GetStream: TStream;
begin
  ReadBlob;
  Result := inherited GetStream;
end;

function TZASABlob.GetString: string;
begin
  ReadBlob;
  Result := inherited GetString;
end;

function TZASABlob.GetUnicodeString: WideString;
begin
  ReadBlob;
  Result := inherited GetUnicodeString;
end;

function TZASABlob.IsEmpty: Boolean;
begin
  ReadBlob;
  Result := inherited IsEmpty;
end;

procedure TZASABlob.ReadBlob;
var
  Size: LongWord;
  Buffer: Pointer;
begin
  if FBlobRead then
   Exit;

  if FResultSet.FInsert or ( FResultSet.FUpdate and FResultSet.FUpdateSQLData.IsAssigned( FColID)) then
    FResultSet.FUpdateSQLData.ReadBlobToMem( FColID, Buffer, Size)
  else
    FResultSet.FSQLData.ReadBlobToMem( FColID, Buffer, Size);
  BlobSize := Size;
  BlobData := Buffer;
  FBlobRead := True;
end;

end.
