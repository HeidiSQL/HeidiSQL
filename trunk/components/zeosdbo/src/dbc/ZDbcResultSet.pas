{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

unit ZDbcResultSet;

interface

{$I ZDbc.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  Classes, SysUtils, Contnrs, ZDbcIntfs, ZClasses, ZCollections, ZSysUtils,
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$IFDEF VER130BELOW}
  {$IFDEF WIN32}
    Comobj,
  {$ENDIF}
{$ENDIF}
  ZCompatibility, ZVariant;

type
  {** Implements Abstract ResultSet. }
  TZAbstractResultSet = class(TInterfacedObject, IZResultSet)
  private
    FTemp: string;
    FRowNo: Integer;
    FLastRowNo: Integer;
    FMaxRows: Integer;
    FClosed: Boolean;
    FFetchDirection: TZFetchDirection;
    FFetchSize: Integer;
    FResultSetType: TZResultSetType;
    FResultSetConcurrency: TZResultSetConcurrency;
    FPostUpdates: TZPostUpdatesMode;
    FLocateUpdates: TZLocateUpdatesMode;
    FColumnsInfo: TObjectList;
    FMetadata: TContainedObject;
    FStatement: IZStatement;
  protected
    LastWasNull: Boolean;

    constructor Create(Statement: IZStatement; SQL: string;
      Metadata: TContainedObject);

    procedure RaiseUnsupportedException;
    procedure RaiseForwardOnlyException;
    procedure RaiseReadOnlyException;
    procedure CheckClosed;
    procedure CheckColumnConvertion(ColumnIndex: Integer; ResultType: TZSQLType);
    procedure CheckBlobColumn(ColumnIndex: Integer);
    procedure Open; virtual;
    function GetColumnIndex(const ColumnName: string): Integer;

    property RowNo: Integer read FRowNo write FRowNo;
    property LastRowNo: Integer read FLastRowNo write FLastRowNo;
    property MaxRows: Integer read FMaxRows write FMaxRows;
    property Closed: Boolean read FClosed write FClosed;
    property FetchDirection: TZFetchDirection
      read FFetchDirection write FFetchDirection;
    property FetchSize: Integer read FFetchSize write FFetchSize;
    property ResultSetType: TZResultSetType
      read FResultSetType write FResultSetType;
    property ResultSetConcurrency: TZResultSetConcurrency
      read FResultSetConcurrency write FResultSetConcurrency;
    property Statement: IZStatement read FStatement;
    property Metadata: TContainedObject read FMetadata write FMetadata;
  public
    destructor Destroy; override;

    procedure SetType(Value: TZResultSetType);
    procedure SetConcurrency(Value: TZResultSetConcurrency);

    function Next: Boolean; virtual;
    procedure Close; virtual;
    function WasNull: Boolean; virtual;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean; virtual;
    function GetPChar(ColumnIndex: Integer): PChar; virtual;
    function GetString(ColumnIndex: Integer): string; virtual;
    function GetUnicodeString(ColumnIndex: Integer): WideString; virtual;
    function GetBoolean(ColumnIndex: Integer): Boolean; virtual;
    function GetByte(ColumnIndex: Integer): ShortInt; virtual;
    function GetShort(ColumnIndex: Integer): SmallInt; virtual;
    function GetInt(ColumnIndex: Integer): Integer; virtual;
    function GetLong(ColumnIndex: Integer): Int64; virtual;
    function GetFloat(ColumnIndex: Integer): Single; virtual;
    function GetDouble(ColumnIndex: Integer): Double; virtual;
    function GetBigDecimal(ColumnIndex: Integer): Extended; virtual;
    function GetBytes(ColumnIndex: Integer): TByteDynArray; virtual;
    function GetDate(ColumnIndex: Integer): TDateTime; virtual;
    function GetTime(ColumnIndex: Integer): TDateTime; virtual;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; virtual;
    function GetAsciiStream(ColumnIndex: Integer): TStream; virtual;
    function GetUnicodeStream(ColumnIndex: Integer): TStream; virtual;
    function GetBinaryStream(ColumnIndex: Integer): TStream; virtual;
    function GetBlob(ColumnIndex: Integer): IZBlob; virtual;
    function GetValue(ColumnIndex: Integer): TZVariant; virtual;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    function IsNullByName(const ColumnName: string): Boolean; virtual;
    function GetPCharByName(const ColumnName: string): PChar; virtual;
    function GetStringByName(const ColumnName: string): string; virtual;
    function GetUnicodeStringByName(const ColumnName: string): WideString; virtual;
    function GetBooleanByName(const ColumnName: string): Boolean; virtual;
    function GetByteByName(const ColumnName: string): ShortInt; virtual;
    function GetShortByName(const ColumnName: string): SmallInt; virtual;
    function GetIntByName(const ColumnName: string): Integer; virtual;
    function GetLongByName(const ColumnName: string): Int64; virtual;
    function GetFloatByName(const ColumnName: string): Single; virtual;
    function GetDoubleByName(const ColumnName: string): Double; virtual;
    function GetBigDecimalByName(const ColumnName: string): Extended; virtual;
    function GetBytesByName(const ColumnName: string): TByteDynArray; virtual;
    function GetDateByName(const ColumnName: string): TDateTime; virtual;
    function GetTimeByName(const ColumnName: string): TDateTime; virtual;
    function GetTimestampByName(const ColumnName: string): TDateTime; virtual;
    function GetAsciiStreamByName(const ColumnName: string): TStream; virtual;
    function GetUnicodeStreamByName(const ColumnName: string): TStream; virtual;
    function GetBinaryStreamByName(const ColumnName: string): TStream; virtual;
    function GetBlobByName(const ColumnName: string): IZBlob; virtual;
    function GetValueByName(const ColumnName: string): TZVariant; virtual;

    //=====================================================================
    // Advanced features:
    //=====================================================================

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;

    function GetCursorName: string; virtual;
    function GetMetaData: IZResultSetMetaData; virtual;
    function FindColumn(const ColumnName: string): Integer; virtual;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function IsBeforeFirst: Boolean; virtual;
    function IsAfterLast: Boolean; virtual;
    function IsFirst: Boolean; virtual;
    function IsLast: Boolean; virtual;
    procedure BeforeFirst; virtual;
    procedure AfterLast; virtual;
    function First: Boolean; virtual;
    function Last: Boolean; virtual;
    function GetRow: Integer; virtual;
    function MoveAbsolute(Row: Integer): Boolean; virtual;
    function MoveRelative(Rows: Integer): Boolean; virtual;
    function Previous: Boolean; virtual;

    //---------------------------------------------------------------------
    // Properties
    //---------------------------------------------------------------------

    procedure SetFetchDirection(Direction: TZFetchDirection); virtual;
    function GetFetchDirection: TZFetchDirection; virtual;

    procedure SetFetchSize(Rows: Integer); virtual;
    function GetFetchSize: Integer; virtual;

    function GetType: TZResultSetType; virtual;
    function GetConcurrency: TZResultSetConcurrency; virtual;

    function GetPostUpdates: TZPostUpdatesMode;
    function GetLocateUpdates: TZLocateUpdatesMode;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean; virtual;
    function RowInserted: Boolean; virtual;
    function RowDeleted: Boolean; virtual;

    procedure UpdateNull(ColumnIndex: Integer); virtual;
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean); virtual;
    procedure UpdateByte(ColumnIndex: Integer; Value: ShortInt); virtual;
    procedure UpdateShort(ColumnIndex: Integer; Value: SmallInt); virtual;
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer); virtual;
    procedure UpdateLong(ColumnIndex: Integer; Value: Int64); virtual;
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single); virtual;
    procedure UpdateDouble(ColumnIndex: Integer; Value: Double); virtual;
    procedure UpdateBigDecimal(ColumnIndex: Integer; Value: Extended); virtual;
    procedure UpdatePChar(ColumnIndex: Integer; Value: PChar); virtual;
    procedure UpdateString(ColumnIndex: Integer; const Value: string); virtual;
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: WideString);
      virtual;
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TByteDynArray); virtual;
    procedure UpdateDate(ColumnIndex: Integer; Value: TDateTime); virtual;
    procedure UpdateTime(ColumnIndex: Integer; Value: TDateTime); virtual;
    procedure UpdateTimestamp(ColumnIndex: Integer; Value: TDateTime); virtual;
    procedure UpdateAsciiStream(ColumnIndex: Integer; Value: TStream); virtual;
    procedure UpdateUnicodeStream(ColumnIndex: Integer; Value: TStream); virtual;
    procedure UpdateBinaryStream(ColumnIndex: Integer; Value: TStream); virtual;
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant); virtual;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    procedure UpdateNullByName(const ColumnName: string); virtual;
    procedure UpdateBooleanByName(const ColumnName: string; Value: Boolean); virtual;
    procedure UpdateByteByName(const ColumnName: string; Value: ShortInt); virtual;
    procedure UpdateShortByName(const ColumnName: string; Value: SmallInt); virtual;
    procedure UpdateIntByName(const ColumnName: string; Value: Integer); virtual;
    procedure UpdateLongByName(const ColumnName: string; Value: Int64); virtual;
    procedure UpdateFloatByName(const ColumnName: string; Value: Single); virtual;
    procedure UpdateDoubleByName(const ColumnName: string; Value: Double); virtual;
    procedure UpdateBigDecimalByName(const ColumnName: string; Value: Extended); virtual;
    procedure UpdatePCharByName(const ColumnName: string; Value: PChar); virtual;
    procedure UpdateStringByName(const ColumnName: string; const Value: string); virtual;
    procedure UpdateUnicodeStringByName(const ColumnName: string; const Value: WideString);
      virtual;
    procedure UpdateBytesByName(const ColumnName: string; const Value: TByteDynArray); virtual;
    procedure UpdateDateByName(const ColumnName: string; Value: TDateTime); virtual;
    procedure UpdateTimeByName(const ColumnName: string; Value: TDateTime); virtual;
    procedure UpdateTimestampByName(const ColumnName: string; Value: TDateTime); virtual;
    procedure UpdateAsciiStreamByName(const ColumnName: string; Value: TStream); virtual;
    procedure UpdateUnicodeStreamByName(const ColumnName: string; Value: TStream); virtual;
    procedure UpdateBinaryStreamByName(const ColumnName: string; Value: TStream); virtual;
    procedure UpdateValueByName(const ColumnName: string; const Value: TZVariant); virtual;

    procedure InsertRow; virtual;
    procedure UpdateRow; virtual;
    procedure DeleteRow; virtual;
    procedure RefreshRow; virtual;
    procedure CancelRowUpdates; virtual;
    procedure MoveToInsertRow; virtual;
    procedure MoveToCurrentRow; virtual;

    function CompareRows(Row1, Row2: Integer; const ColumnIndices: TIntegerDynArray;
      const ColumnDirs: TBooleanDynArray): Integer; virtual;

    function GetStatement: IZStatement; virtual;

    property ColumnsInfo: TObjectList read FColumnsInfo write FColumnsInfo;
  end;

  {** Implements external or internal blob wrapper object. }
  TZAbstractBlob = class(TInterfacedObject, IZBlob)
  private
    FBlobData: Pointer;
    FBlobSize: Integer;
    FUpdated: Boolean;
  protected
    property BlobData: Pointer read FBlobData write FBlobData;
    property BlobSize: Integer read FBlobSize write FBlobSize;
    property Updated: Boolean read FUpdated write FUpdated;
  public
    constructor CreateWithStream(Stream: TStream);
    constructor CreateWithData(Data: Pointer; Size: Integer);
    destructor Destroy; override;

    function IsEmpty: Boolean; virtual;
    function IsUpdated: Boolean; virtual;
    function Length: LongInt; virtual;

    function GetString: string; virtual;
    procedure SetString(const Value: string); virtual;
    function GetUnicodeString: WideString; virtual;
    procedure SetUnicodeString(const Value: WideString); virtual;
    function GetBytes: TByteDynArray; virtual;
    procedure SetBytes(const Value: TByteDynArray); virtual;
    function GetStream: TStream; virtual;
    procedure SetStream(Value: TStream); virtual;

    procedure Clear; virtual;
    function Clone: IZBlob; virtual;
  end;

implementation

uses ZMessages, ZDbcUtils, ZDbcResultSetMetadata;

{ TZAbstractResultSet }

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param Metadata a resultset metadata object.
}
constructor TZAbstractResultSet.Create(Statement: IZStatement; SQL: string;
  Metadata: TContainedObject);
var
  DatabaseMetadata: IZDatabaseMetadata;
begin
  LastWasNull := True;
  FRowNo := 0;
  FLastRowNo := 0;
  FClosed := True;

  if Statement = nil then
  begin
    FResultSetType := rtForwardOnly;
    FResultSetConcurrency := rcReadOnly;
    FPostUpdates := poColumnsAll;
    FLocateUpdates := loWhereAll;
    FMaxRows := 0;
  end
  else
  begin
    FFetchDirection := Statement.GetFetchDirection;
    FFetchSize := Statement.GetFetchSize;
    FResultSetType := Statement.GetResultSetType;
    FResultSetConcurrency := Statement.GetResultSetConcurrency;
    FPostUpdates := Statement.GetPostUpdates;
    FLocateUpdates := Statement.GetLocateUpdates;
    FStatement := Statement;
    FMaxRows := Statement.GetMaxRows;
  end;

  if Metadata = nil then
  begin
    if Statement <> nil then
      DatabaseMetadata := GetStatement.GetConnection.GetMetadata
    else DatabaseMetadata := nil;
    FMetadata := TZAbstractResultSetMetadata.Create(DatabaseMetadata, SQL, Self);
  end else
    FMetadata := Metadata;

  FColumnsInfo := TObjectList.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractResultSet.Destroy;
begin
  if not FClosed then Close;

  if FMetadata <> nil then
    FMetadata.Free;
  FMetadata := nil;
  FStatement := nil;

  FColumnsInfo.Free;
  inherited Destroy;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractResultSet.RaiseUnsupportedException;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
end;

{**
  Raises operation is not allowed in FORWARD ONLY mode exception.
}
procedure TZAbstractResultSet.RaiseForwardOnlyException;
begin
  raise EZSQLException.Create(SOperationIsNotAllowed1);
end;

{**
  Raises operation is not allowed in READ ONLY mode exception.
}
procedure TZAbstractResultSet.RaiseReadOnlyException;
begin
  raise EZSQLException.Create(SOperationIsNotAllowed2);
end;

{**
  Checks if result set is open and operation is allowed.
}
procedure TZAbstractResultSet.CheckClosed;
begin
  if FClosed then
    raise EZSQLException.Create(SOperationIsNotAllowed4);
end;

{**
  Checks is the column convertion from one type to another type allowed.
  @param ColumnIndex an index of column.
  @param ResultType a requested data type.
}
procedure TZAbstractResultSet.CheckColumnConvertion(ColumnIndex: Integer;
  ResultType: TZSQLType);
var
  InitialType: TZSQLType;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  if (Metadata = nil) or (ColumnIndex <= 0)
    or (ColumnIndex > Metadata.GetColumnCount) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;

  InitialType := Metadata.GetColumnType(ColumnIndex);
  if not CheckConvertion(InitialType, ResultType) then
  begin
    raise EZSQLException.Create(
      Format(SConvertionIsNotPossible,
      [ColumnIndex, DefineColumnTypeName(InitialType),
      DefineColumnTypeName(ResultType)]));
  end;
end;

{**
  Checks for blob expected column.
  @param ColumnIndex an index of column.
}
procedure TZAbstractResultSet.CheckBlobColumn(ColumnIndex: Integer);
var
  InitialType: TZSQLType;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  if (Metadata = nil) or (ColumnIndex <= 0)
    or (ColumnIndex > Metadata.GetColumnCount) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;

  InitialType := Metadata.GetColumnType(ColumnIndex);
  if not (InitialType in [stAsciiStream, stBinaryStream, stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(InitialType)]));
  end;
end;

{**
  Set the concurrency mode of this <code>ResultSet</code> object.
  The concurrency used is determined by the
  <code>Statement</code> object that created the result set.

  @param the concurrency type, either <code>CONCUR_READ_ONLY</code>
    or <code>CONCUR_UPDATABLE</code>
}
procedure TZAbstractResultSet.SetConcurrency(Value: TZResultSetConcurrency);
begin
  ResultSetConcurrency := Value;
end;

{**
  Set the type of this <code>ResultSet</code> object.
  The type is determined by the <code>Statement</code> object
  that created the result set.

  @param <code>TYPE_FORWARD_ONLY</code>,
    <code>TYPE_SCROLL_INSENSITIVE</code>,
    or <code>TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractResultSet.SetType(Value: TZResultSetType);
begin
  ResultSetType := Value;
end;

{**
  Opens this recordset.
}
procedure TZAbstractResultSet.Open;
begin
  FClosed := False;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAbstractResultSet.Close;
  var I : integer;
  var FColumnInfo : TZColumnInfo;
begin
  LastWasNull := True;
  FRowNo := 0;
  FLastRowNo := 0;
  FClosed := True;
  for I := FColumnsInfo.Count-1 downto 0 do begin
    FColumnInfo:=TZColumnInfo(FColumnsInfo.Extract(FColumnsInfo.Items[I]));
    FColumnInfo.Free;
  end;
  FColumnsInfo.Clear;
  FStatement := nil;
end;

{**
  Reports whether
  the last column read had a value of SQL <code>NULL</code>.
  Note that you must first call one of the <code>getXXX</code> methods
  on a column to try to read its value and then call
  the method <code>wasNull</code> to see if the value read was
  SQL <code>NULL</code>.

  @return <code>true</code> if the last column value read was SQL
    <code>NULL</code> and <code>false</code> otherwise
}
function TZAbstractResultSet.WasNull: Boolean;
begin
  Result := LastWasNull;
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  Result := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPChar(ColumnIndex: Integer): PChar;
begin
  FTemp := GetString(ColumnIndex);
  Result := PChar(FTemp);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetString(ColumnIndex: Integer): string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetUnicodeString(ColumnIndex: Integer): WideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  Result := '';
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetByte(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
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
function TZAbstractResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := 0;
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
function TZAbstractResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := 0;
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
function TZAbstractResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := 0;
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
function TZAbstractResultSet.GetAsciiStream(ColumnIndex: Integer): TStream;
var
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  Result := nil;
  if not IsNull(ColumnIndex) then
  begin
    Blob := GetBlob(ColumnIndex);
    if Blob <> nil then
      Result := Blob.GetStream;
  end;
  LastWasNull := (Result = nil);
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
function TZAbstractResultSet.GetUnicodeStream(ColumnIndex: Integer): TStream;
var
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  Result := nil;
  if not IsNull(ColumnIndex) then
  begin
    Blob := GetBlob(ColumnIndex);
    if Blob <> nil then
      Result := Blob.GetStream;
  end;
  LastWasNull := (Result = nil);
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
function TZAbstractResultSet.GetBinaryStream(ColumnIndex: Integer): TStream;
var
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  Result := nil;
  if not IsNull(ColumnIndex) then
  begin
    Blob := GetBlob(ColumnIndex);
    if Blob <> nil then
      Result := Blob.GetStream;
  end;
  LastWasNull := (Result = nil);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Result := TZAbstractBlob.CreateWithStream(nil);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Variant</code> object.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Variant</code> object representing the SQL
    any value in the specified column
}
function TZAbstractResultSet.GetValue(ColumnIndex: Integer): TZVariant;
var
  Metadata: TZAbstractResultSetMetadata;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  Metadata := TZAbstractResultSetMetadata(FMetadata);
{$IFNDEF DISABLE_CHECKING}
  if (Metadata = nil) or (ColumnIndex <= 0)
    or (ColumnIndex > Metadata.GetColumnCount) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;
{$ENDIF}

  case Metadata.GetColumnType(ColumnIndex) of
    stBoolean:
      begin
        Result.VType := vtBoolean;
        Result.VBoolean := GetBoolean(ColumnIndex);
      end;
    stByte, stShort, stInteger, stLong:
      begin
        Result.VType := vtInteger;
        Result.VInteger := GetLong(ColumnIndex);
      end;
    stFloat, stDouble, stBigDecimal:
      begin
        Result.VType := vtFloat;
        Result.VFloat := GetBigDecimal(ColumnIndex);
      end;
    stDate, stTime, stTimestamp:
      begin
        Result.VType := vtDateTime;
        Result.VDateTime := GetTimestamp(ColumnIndex);
      end;
    stString, stBytes, stAsciiStream, stBinaryStream:
      begin
        Result.VType := vtString;
        Result.VString := GetString(ColumnIndex);
      end;
    stUnicodeString, stUnicodeStream:
      begin
        Result.VType := vtUnicodeString;
        Result.VUnicodeString := GetUnicodeString(ColumnIndex);
      end;
    else
      Result.VType := vtNull;
  end;

  if WasNull then
    Result.VType := vtNull;
end;

//======================================================================
// Methods for accessing results by column name
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnName the SQL name of the column
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractResultSet.IsNullByName(const ColumnName: string): Boolean;
begin
  Result := IsNull(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PChar</code> in the Delphi programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPCharByName(const ColumnName: string): PChar;
begin
  Result := GetPChar(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetStringByName(const ColumnName: string): string;
begin
  Result := GetString(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Object Pascal programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetUnicodeStringByName(const ColumnName: string):
  WideString;
begin
  Result := GetUnicodeString(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractResultSet.GetBooleanByName(const ColumnName: string): Boolean;
begin
  Result := GetBoolean(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetByteByName(const ColumnName: string): ShortInt;
begin
  Result := GetByte(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetShortByName(const ColumnName: string): SmallInt;
begin
  Result := GetShort(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetIntByName(const ColumnName: string): Integer;
begin
  Result := GetInt(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetLongByName(const ColumnName: string): Int64;
begin
  Result := GetLong(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetFloatByName(const ColumnName: string): Single;
begin
  Result := GetFloat(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetDoubleByName(const ColumnName: string): Double;
begin
  Result := GetDouble(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.math.BigDecimal</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBigDecimalByName(const ColumnName: string): Extended;
begin
  Result := GetBigDecimal(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBytesByName(const ColumnName: string): TByteDynArray;
begin
  Result := GetBytes(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetDateByName(const ColumnName: string): TDateTime;
begin
  Result := GetDate(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>,
    the value returned is <code>null</code>
}
function TZAbstractResultSet.GetTimeByName(const ColumnName: string): TDateTime;
begin
  Result := GetTime(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetTimestampByName(const ColumnName: string): TDateTime;
begin
  Result := GetTimestamp(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a stream of
  ASCII characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARCHAR</code> values.
  The JDBC driver will
  do any necessary conversion from the database format into ASCII.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream. Also, a
  stream may return <code>0</code> when the method <code>available</code>
  is called whether there is data available or not.

  @param columnName the SQL name of the column
  @return a Java input stream that delivers the database column value
    as a stream of one-byte ASCII characters.
    If the value is SQL <code>NULL</code>,
    the value returned is <code>null</code>.
}
function TZAbstractResultSet.GetAsciiStreamByName(const ColumnName: string): TStream;
begin
  Result := GetAsciiStream(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a stream of
  Unicode characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARCHAR</code> values.
  The JDBC driver will
  do any necessary conversion from the database format into Unicode.
  The byte format of the Unicode stream must be Java UTF-8,
  as defined in the Java virtual machine specification.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream. Also, a
  stream may return <code>0</code> when the method <code>available</code>
  is called whether there is data available or not.

  @param columnName the SQL name of the column
  @return a Java input stream that delivers the database column value
    as a stream of two-byte Unicode characters.
    If the value is SQL <code>NULL</code>, the value returned is <code>null</code>.
}
function TZAbstractResultSet.GetUnicodeStreamByName(const ColumnName: string): TStream;
begin
  Result := GetUnicodeStream(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a stream of uninterpreted
  <code>byte</code>s.
  The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARBINARY</code>
  values.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream. Also, a
  stream may return <code>0</code> when the method <code>available</code>
  is called whether there is data available or not.

  @param columnName the SQL name of the column
  @return a Java input stream that delivers the database column value
    as a stream of uninterpreted bytes;
    if the value is SQL <code>NULL</code>, the result is <code>null</code>
}
function TZAbstractResultSet.GetBinaryStreamByName(const ColumnName: string): TStream;
begin
  Result := GetBinaryStream(GetColumnIndex(ColumnName));
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param colName the name of the column from which to retrieve the value
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractResultSet.GetBlobByName(const ColumnName: string): IZBlob;
begin
  Result := GetBlob(GetColumnIndex(ColumnName));
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Variant</code> object.

  @param colName the name of the column from which to retrieve the value
  @return a <code>Blob</code> object representing the SQL <code>Any</code>
    value in the specified column
}
function TZAbstractResultSet.GetValueByName(const ColumnName: string): TZVariant;
begin
  Result := GetValue(GetColumnIndex(ColumnName));
end;

//=====================================================================
// Advanced features:
//=====================================================================

{**
  Returns the first warning reported by calls on this
  <code>ResultSet</code> object.
  Subsequent warnings on this <code>ResultSet</code> object
  will be chained to the <code>SQLWarning</code> object that
  this method returns.

  <P>The warning chain is automatically cleared each time a new
  row is read.

  <P><B>Note:</B> This warning chain only covers warnings caused
  by <code>ResultSet</code> methods.  Any warning caused by
  <code>Statement</code> methods
  (such as reading OUT parameters) will be chained on the
  <code>Statement</code> object.

  @return the first <code>SQLWarning</code> object reported or <code>null</code>
}
function TZAbstractResultSet.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported on this <code>ResultSet</code> object.
  After this method is called, the method <code>getWarnings</code>
  returns <code>null</code> until a new warning is
  reported for this <code>ResultSet</code> object.
}
procedure TZAbstractResultSet.ClearWarnings;
begin
end;

{**
  Gets the name of the SQL cursor used by this <code>ResultSet</code>
  object.

  <P>In SQL, a result table is retrieved through a cursor that is
  named. The current row of a result set can be updated or deleted
  using a positioned update/delete statement that references the
  cursor name. To insure that the cursor has the proper isolation
  level to support update, the cursor's <code>select</code> statement should be
  of the form 'select for update'. If the 'for update' clause is
  omitted, the positioned updates may fail.

  <P>The JDBC API supports this SQL feature by providing the name of the
  SQL cursor used by a <code>ResultSet</code> object.
  The current row of a <code>ResultSet</code> object
  is also the current row of this SQL cursor.

  <P><B>Note:</B> If positioned update is not supported, a
  <code>SQLException</code> is thrown.

  @return the SQL name for this <code>ResultSet</code> object's cursor
}
function TZAbstractResultSet.GetCursorName: string;
begin
  Result := '';
end;

{**
  Retrieves the  number, types and properties of
  this <code>ResultSet</code> object's columns.
  @return the description of this <code>ResultSet</code> object's columns
}
function TZAbstractResultSet.GetMetaData: IZResultSetMetaData;
begin
  Result := TZAbstractResultSetMetadata(FMetadata);
end;

{**
  Maps the given <code>ResultSet</code> column name to its
  <code>ResultSet</code> column index.

  @param columnName the name of the column
  @return the column index of the given column name
}
function TZAbstractResultSet.GetColumnIndex(const ColumnName: string): Integer;
begin
  Result := FindColumn(ColumnName);

  if Result < 1 then
    raise EZSQLException.Create(Format(SColumnWasNotFound, [ColumnName]));
end;

{**
  Maps the given <code>ResultSet</code> column name to its
  <code>ResultSet</code> column index.

  @param columnName the name of the column
  @return the column index of the given column name
}
function TZAbstractResultSet.FindColumn(const ColumnName: string): Integer;
var
  I: Integer;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  Result := 0;

  { Search for case sensitive columns. }
  for I := 1 to Metadata.GetColumnCount do
  begin
    if Metadata.GetColumnLabel(I) = ColumnName then
    begin
      Result := I;
      Exit;
    end;
  end;

  { Search for case insensitive columns. }
  for I := 1 to Metadata.GetColumnCount do
  begin
    if AnsiUpperCase(Metadata.GetColumnLabel(I)) = AnsiUpperCase(ColumnName) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

//---------------------------------------------------------------------
// Traversal/Positioning
//---------------------------------------------------------------------

{**
  Indicates whether the cursor is before the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is before the first row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZAbstractResultSet.IsBeforeFirst: Boolean;
begin
  Result := (FRowNo = 0);
end;

{**
  Indicates whether the cursor is after the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is after the last row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZAbstractResultSet.IsAfterLast: Boolean;
begin
  Result := {(FLastRowNo > 0) and} (FRowNo > FLastRowNo);
end;

{**
  Indicates whether the cursor is on the first row of
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on the first row;
    <code>false</code> otherwise
}
function TZAbstractResultSet.IsFirst: Boolean;
begin
  Result := (FRowNo = 1);
end;

{**
  Indicates whether the cursor is on the last row of
  this <code>ResultSet</code> object.
  Note: Calling the method <code>isLast</code> may be expensive
  because the JDBC driver
  might need to fetch ahead one row in order to determine
  whether the current row is the last row in the result set.

  @return <code>true</code> if the cursor is on the last row;
    <code>false</code> otherwise
}
function TZAbstractResultSet.IsLast: Boolean;
begin
  Result := {(FLastRowNo > 0) and} (FRowNo = FLastRowNo);
end;

{**
  Moves the cursor to the front of
  this <code>ResultSet</code> object, just before the
  first row. This method has no effect if the result set contains no rows.
}
procedure TZAbstractResultSet.BeforeFirst;
begin
  MoveAbsolute(0);
end;

{**
  Moves the cursor to the end of
  this <code>ResultSet</code> object, just after the
  last row. This method has no effect if the result set contains no rows.
}
procedure TZAbstractResultSet.AfterLast;
begin
  Last;
  Next;
end;

{**
  Moves the cursor to the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
  <code>false</code> if there are no rows in the result set
}
function TZAbstractResultSet.First: Boolean;
begin
  Result := MoveAbsolute(1);
end;

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZAbstractResultSet.Last: Boolean;
begin
  Result := MoveAbsolute(FLastRowNo);
end;

{**
  Retrieves the current row number.  The first row is number 1, the
  second number 2, and so on.
  @return the current row number; <code>0</code> if there is no current row
}
function TZAbstractResultSet.GetRow: Integer;
begin
  Result := FRowNo;
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
function TZAbstractResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  RaiseForwardOnlyException;
end;

{**
  Moves the cursor a relative number of rows, either positive or negative.
  Attempting to move beyond the first/last row in the
  result set positions the cursor before/after the
  the first/last row. Calling <code>relative(0)</code> is valid, but does
  not change the cursor position.

  <p>Note: Calling the method <code>relative(1)</code>
  is different from calling the method <code>next()</code>
  because is makes sense to call <code>next()</code> when there
  is no current row,
  for example, when the cursor is positioned before the first row
  or after the last row of the result set.

  @return <code>true</code> if the cursor is on a row;
    <code>false</code> otherwise
}
function TZAbstractResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := MoveAbsolute(FRowNo + Rows);
end;

{**
  Moves the cursor to the previous row in this
  <code>ResultSet</code> object.

  <p><B>Note:</B> Calling the method <code>previous()</code> is not the same as
  calling the method <code>relative(-1)</code> because it
  makes sense to call</code>previous()</code> when there is no current row.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if it is off the result set
}
function TZAbstractResultSet.Previous: Boolean;
begin
  Result := MoveAbsolute(FRowNo - 1);
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
function TZAbstractResultSet.Next: Boolean;
begin
  Result := MoveAbsolute(FRowNo + 1);
end;

//---------------------------------------------------------------------
// Properties
//---------------------------------------------------------------------

{**
  Returns the fetch direction for this
  <code>ResultSet</code> object.
  @return the current fetch direction for this <code>ResultSet</code> object
}
function TZAbstractResultSet.GetFetchDirection: TZFetchDirection;
begin
  Result := FFetchDirection;
end;

{**
  Gives a hint as to the direction in which the rows in this
  <code>ResultSet</code> object will be processed.
  The initial value is determined by the
  <code>Statement</code> object
  that produced this <code>ResultSet</code> object.
  The fetch direction may be changed at any time.
}
procedure TZAbstractResultSet.SetFetchDirection(Direction: TZFetchDirection);
begin
  if Direction <> fdForward then
    RaiseUnsupportedException;
end;

{**
  Returns the fetch size for this
  <code>ResultSet</code> object.
  @return the current fetch size for this <code>ResultSet</code> object
}
function TZAbstractResultSet.GetFetchSize: Integer;
begin
  Result := FFetchSize;
end;

{**
  Gives the JDBC driver a hint as to the number of rows that should
  be fetched from the database when more rows are needed for this
  <code>ResultSet</code> object.
  If the fetch size specified is zero, the JDBC driver
  ignores the value and is free to make its own best guess as to what
  the fetch size should be.  The default value is set by the
  <code>Statement</code> object
  that created the result set.  The fetch size may be changed at any time.

  @param rows the number of rows to fetch
}
procedure TZAbstractResultSet.SetFetchSize(Rows: Integer);
begin
  FFetchSize := Rows;
end;

{**
  Returns the type of this <code>ResultSet</code> object.
  The type is determined by the <code>Statement</code> object
  that created the result set.

  @return <code>TYPE_FORWARD_ONLY</code>,
    <code>TYPE_SCROLL_INSENSITIVE</code>,
    or <code>TYPE_SCROLL_SENSITIVE</code>
}
function TZAbstractResultSet.GetType: TZResultSetType;
begin
  Result := FResultSetType;
end;

{**
  Returns the concurrency mode of this <code>ResultSet</code> object.
  The concurrency used is determined by the
  <code>Statement</code> object that created the result set.

  @return the concurrency type, either <code>CONCUR_READ_ONLY</code>
    or <code>CONCUR_UPDATABLE</code>
}
function TZAbstractResultSet.GetConcurrency: TZResultSetConcurrency;
begin
  Result := FResultSetConcurrency;
end;

{**
  Gets an assigned post locate mode.
  @param the assigned post locate mode.
}
function TZAbstractResultSet.GetLocateUpdates: TZLocateUpdatesMode;
begin
  Result := FLocateUpdates;
end;

function TZAbstractResultSet.GetPostUpdates: TZPostUpdatesMode;
begin
  Result := FPostUpdates;
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Indicates whether the current row has been updated.  The value returned
  depends on whether or not the result set can detect updates.

  @return <code>true</code> if the row has been visibly updated
    by the owner or another, and updates are detected
}
function TZAbstractResultSet.RowUpdated: Boolean;
begin
  Result := False;
end;

{**
  Indicates whether the current row has had an insertion.
  The value returned depends on whether or not this
  <code>ResultSet</code> object can detect visible inserts.

  @return <code>true</code> if a row has had an insertion
    and insertions are detected; <code>false</code> otherwise
}
function TZAbstractResultSet.RowInserted: Boolean;
begin
  Result := False;
end;

{**
  Indicates whether a row has been deleted.  A deleted row may leave
  a visible "hole" in a result set.  This method can be used to
  detect holes in a result set.  The value returned depends on whether
  or not this <code>ResultSet</code> object can detect deletions.

  @return <code>true</code> if a row was deleted and deletions are detected;
    <code>false</code> otherwise
}
function TZAbstractResultSet.RowDeleted: Boolean;
begin
  Result := False;
end;

{**
  Gives a nullable column a null value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code>
  or <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZAbstractResultSet.UpdateNull(ColumnIndex: Integer);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBoolean(ColumnIndex: Integer; Value: Boolean);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateByte(ColumnIndex: Integer;
  Value: ShortInt);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>short</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateShort(ColumnIndex: Integer; Value: SmallInt);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateInt(ColumnIndex: Integer; Value: Integer);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateLong(ColumnIndex: Integer; Value: Int64);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>float</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateFloat(ColumnIndex: Integer; Value: Single);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDouble(ColumnIndex: Integer; Value: Double);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBigDecimal(ColumnIndex: Integer;
  Value: Extended);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePChar(ColumnIndex: Integer; Value: PChar);
begin
  UpdateString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateString(ColumnIndex: Integer; const Value: string);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>WideString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeString(ColumnIndex: Integer;
  const Value: WideString);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>byte</code> array value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBytes(ColumnIndex: Integer;
  const Value: TByteDynArray);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDate(ColumnIndex: Integer; Value: TDateTime);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTime(ColumnIndex: Integer; Value: TDateTime);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimestamp(ColumnIndex: Integer;
  Value: TDateTime);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateAsciiStream(ColumnIndex: Integer;
  Value: TStream);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZAbstractResultSet.UpdateBinaryStream(ColumnIndex: Integer;
  Value: TStream);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStream(ColumnIndex: Integer;
  Value: TStream);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a variant value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateValue(ColumnIndex: Integer;
  const Value: TZVariant);
begin
  case Value.VType of
    vtBoolean: UpdateBoolean(ColumnIndex, Value.VBoolean);
    vtInteger: UpdateLong(ColumnIndex, Value.VInteger);
    vtFloat: UpdateBigDecimal(ColumnIndex, Value.VFloat);
    vtString: UpdateString(ColumnIndex, Value.VString);
    vtDateTime: UpdateTimestamp(ColumnIndex, Value.VDateTime);
    vtUnicodeString: UpdateUnicodeString(ColumnIndex, Value.VUnicodeString);
    else UpdateNull(ColumnIndex);
  end;
end;

{**
  Updates the designated column with a <code>null</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
}
procedure TZAbstractResultSet.UpdateNullByName(const ColumnName: string);
begin
  UpdateNull(GetColumnIndex(ColumnName));
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBooleanByName(const ColumnName: string;
  Value: Boolean);
begin
  UpdateBoolean(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateByteByName(const ColumnName: string;
  Value: ShortInt);
begin
  UpdateByte(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>short</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateShortByName(const ColumnName: string;
  Value: SmallInt);
begin
  UpdateShort(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateIntByName(const ColumnName: string;
  Value: Integer);
begin
  UpdateInt(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateLongByName(const ColumnName: string;
  Value: Int64);
begin
  UpdateLong(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>float	</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateFloatByName(const ColumnName: string;
  Value: Single);
begin
  UpdateFloat(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDoubleByName(const ColumnName: string;
  Value: Double);
begin
  UpdateDouble(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBigDecimalByName(const ColumnName: string;
  Value: Extended);
begin
  UpdateBigDecimal(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePCharByName(const ColumnName: string;
  Value: PChar);
begin
  UpdatePChar(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateStringByName(const ColumnName: string;
  const Value: string);
begin
  UpdateString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>WideString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStringByName(const ColumnName: string;
  const Value: WideString);
begin
  UpdateUnicodeString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  JDBC 2.0

  Updates a column with a byte array value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row, or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or <code>insertRow</code>
  methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBytesByName(const ColumnName: string;
  const Value: TByteDynArray);
begin
  UpdateBytes(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDateByName(const ColumnName: string;
  Value: TDateTime);
begin
  UpdateDate(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimeByName(const ColumnName: string;
  Value: TDateTime);
begin
  UpdateTime(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimestampByName(const ColumnName: string;
  Value: TDateTime);
begin
  UpdateTimestamp(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateAsciiStreamByName(const ColumnName: string;
  Value: TStream);
begin
  UpdateAsciiStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBinaryStreamByName(const ColumnName: string;
  Value: TStream);
begin
  UpdateBinaryStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStreamByName(const ColumnName: string;
  Value: TStream);
begin
  UpdateUnicodeStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>Variant</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateValueByName(const ColumnName: string;
  const Value: TZVariant);
begin
  UpdateValue(GetColumnIndex(ColumnName), Value);
end;

{**
  Inserts the contents of the insert row into this
  <code>ResultSet</code> objaect and into the database.
  The cursor must be on the insert row when this method is called.
}
procedure TZAbstractResultSet.InsertRow;
begin
  RaiseReadOnlyException;
end;

{**
  Updates the underlying database with the new contents of the
  current row of this <code>ResultSet</code> object.
  This method cannot be called when the cursor is on the insert row.
}
procedure TZAbstractResultSet.UpdateRow;
begin
  RaiseReadOnlyException;
end;

{**
  Deletes the current row from this <code>ResultSet</code> object
  and from the underlying database.  This method cannot be called when
  the cursor is on the insert row.
}
procedure TZAbstractResultSet.DeleteRow;
begin
  RaiseReadOnlyException;
end;

{**
  Refreshes the current row with its most recent value in
  the database.  This method cannot be called when
  the cursor is on the insert row.

  <P>The <code>refreshRow</code> method provides a way for an
  application to
  explicitly tell the JDBC driver to refetch a row(s) from the
  database.  An application may want to call <code>refreshRow</code> when
  caching or prefetching is being done by the JDBC driver to
  fetch the latest value of a row from the database.  The JDBC driver
  may actually refresh multiple rows at once if the fetch size is
  greater than one.

  <P> All values are refetched subject to the transaction isolation
  level and cursor sensitivity.  If <code>refreshRow</code> is called after
  calling an <code>updateXXX</code> method, but before calling
  the method <code>updateRow</code>, then the
  updates made to the row are lost.  Calling the method
  <code>refreshRow</code> frequently will likely slow performance.
}
procedure TZAbstractResultSet.RefreshRow;
begin
  RaiseUnsupportedException;
end;

{**
  Cancels the updates made to the current row in this
  <code>ResultSet</code> object.
  This method may be called after calling an
  <code>updateXXX</code> method(s) and before calling
  the method <code>updateRow</code> to roll back
  the updates made to a row.  If no updates have been made or
  <code>updateRow</code> has already been called, this method has no
  effect.
}
procedure TZAbstractResultSet.CancelRowUpdates;
begin
  RaiseReadOnlyException;
end;

{**
  Moves the cursor to the insert row.  The current cursor position is
  remembered while the cursor is positioned on the insert row.

  The insert row is a special row associated with an updatable
  result set.  It is essentially a buffer where a new row may
  be constructed by calling the <code>updateXXX</code> methods prior to
  inserting the row into the result set.

  Only the <code>updateXXX</code>, <code>getXXX</code>,
  and <code>insertRow</code> methods may be
  called when the cursor is on the insert row.  All of the columns in
  a result set must be given a value each time this method is
  called before calling <code>insertRow</code>.
  An <code>updateXXX</code> method must be called before a
  <code>getXXX</code> method can be called on a column value.
}
procedure TZAbstractResultSet.MoveToInsertRow;
begin
  RaiseReadOnlyException;
end;

{**
  Moves the cursor to the remembered cursor position, usually the
  current row.  This method has no effect if the cursor is not on
  the insert row.
}
procedure TZAbstractResultSet.MoveToCurrentRow;
begin
end;

{**
  Compares fields from two row buffers.
  @param Row1 the first row buffer to compare.
  @param Row2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZAbstractResultSet.CompareRows(Row1, Row2: Integer;
  const ColumnIndices: TIntegerDynArray; const ColumnDirs: TBooleanDynArray): Integer;
var
  I: Integer;
  ColumnIndex: Integer;
  SaveRowNo: Integer;
  Value1, Value2: TZVariant;

  function CompareFloat(Value1, Value2: Extended): Integer;
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
  SaveRowNo := RowNo;
  try
    for I := Low(ColumnIndices) to High(ColumnIndices) do
    begin
      ColumnIndex := ColumnIndices[I];

      MoveAbsolute(Row1);
      Value1 := GetValue(ColumnIndex);
      MoveAbsolute(Row2);
      Value2 := GetValue(ColumnIndex);

      { Checks for both Null columns. }
      if (Value1.VType = vtNull) and (Value2.VType = vtNull) then
        Continue;
      { Checks for not-Null and Null columns. }
      if (Value1.VType = vtNull) or (Value2.VType = vtNull) then
      begin
        if Value1.VType <> vtNull then
          Result := 1
        else Result := -1;
        if not ColumnDirs[I] then
          Result := -Result;
        Break;
      end;
      case Value1.VType of
        vtBoolean:
          begin
            if Value1.VBoolean = Value2.VBoolean then
              Result := 0
            else if Value1.VBoolean = True then
              Result := 1
            else Result := -1;
          end;
        vtInteger:
          Result := Value1.VInteger - Value2.VInteger;
        vtFloat:
          Result := CompareFloat(Value1.VFloat, Value2.VFloat);
        vtDateTime:
          Result := CompareFloat(Value1.VDateTime, Value2.VDateTime);
        vtString:
          Result := AnsiCompareStr(Value1.VString, Value2.VString);
        vtUnicodeString:
        {$IFNDEF VER130BELOW}
          Result := WideCompareStr(Value1.VUnicodeString, Value2.VUnicodeString);
        {$ELSE}
          Result := AnsiCompareStr(Value1.VUnicodeString, Value2.VUnicodeString);
        {$ENDIF}
      end;
      if Result <> 0 then
      begin
        if not ColumnDirs[I] then
          Result := -Result;
        Break;
      end;
    end;
  finally
    MoveAbsolute(SaveRowNo);
  end;
end;

{**
  Returns the <code>Statement</code> object that produced this
  <code>ResultSet</code> object.
  If the result set was generated some other way, such as by a
  <code>DatabaseMetaData</code> method, this method returns
  <code>null</code>.

  @return the <code>Statment</code> object that produced
    this <code>ResultSet</code> object or <code>null</code>
    if the result set was produced some other way
}
function TZAbstractResultSet.GetStatement: IZStatement;
begin
  Result := FStatement;
end;

{ TZAbstractBlob }

{**
  Constructs this class and assignes the main properties.
  @param Stream a data string object.
}
constructor TZAbstractBlob.CreateWithStream(Stream: TStream);
begin
  inherited Create;
  FUpdated := False;
  if Assigned(Stream) then
  begin
    FBlobSize := Stream.Size;
    if FBlobSize > 0 then
    begin
      GetMem(FBlobData, FBlobSize);
      Stream.Position := 0;
      Stream.ReadBuffer(FBlobData^, FBlobSize);
    end;
  end
  else
  begin
    FBlobSize := -1;
    FBlobData := nil;
  end;
end;

{**
  Constructs this class and assignes the main properties.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
}
constructor TZAbstractBlob.CreateWithData(Data: Pointer; Size: Integer);
begin
  inherited Create;
  FBlobData := nil;
  FBlobSize := Size;
  if FBlobSize > 0 then
  begin
    GetMem(FBlobData, FBlobSize);
    System.Move(Data^, FBlobData^, FBlobSize);
  end;
  FUpdated := False;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractBlob.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{**
  Clears the content of this blob.
}
procedure TZAbstractBlob.Clear;
begin
  if Assigned(FBlobData) then
    FreeMem(FBlobData);
  FBlobData := nil;
  FBlobSize := -1;
  FUpdated := True;
end;

{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZAbstractBlob.Clone: IZBlob;
begin
  Result := TZAbstractBlob.CreateWithData(FBlobData, FBlobSize);
end;

{**
  Checks if this blob has an empty content.
  @return <code>True</code> if this blob is empty.
}
function TZAbstractBlob.IsEmpty: Boolean;
begin
  Result := FBlobSize < 0;
end;

{**
  Checks if the content of this blob was updated.
  @return <code>True</code> is this blob was updated.
}
function TZAbstractBlob.IsUpdated: Boolean;
begin
  Result := FUpdated;
end;

{**
  Gets the length of the stored data.
  @return the length of the stored data or null if the blob is empty.
}
function TZAbstractBlob.Length: LongInt;
begin
  Result := FBlobSize;
end;

{**
  Gets the string from the stored data.
  @return a string which contains the stored data.
}
function TZAbstractBlob.GetString: string;
begin
  if (FBlobSize > 0) and Assigned(FBlobData) then
    System.SetString(Result, PChar(FBlobData), FBlobSize)
  else Result := '';
end;

{**
  Sets a new string data to this blob content.
  @param Value a new string data.
}
procedure TZAbstractBlob.SetString(const Value: string);
begin
  Clear;
  FBlobSize := System.Length(Value);
  if FBlobSize > 0 then
  begin
    GetMem(FBlobData, FBlobSize);
    System.Move(PChar(Value)^, FBlobData^, FBlobSize);
  end;
  FUpdated := True;
end;

{**
  Gets the wide string from the stored data.
  @return a string which contains the stored data.
}
function TZAbstractBlob.GetUnicodeString: WideString;
begin
  Result := GetString;
end;

{**
  Sets a new string data to this blob content.
  @param Value a new wide string data.
}
procedure TZAbstractBlob.SetUnicodeString(const Value: WideString);
begin
  SetString(Value);
end;

{**
  Gets the byte buffer from the stored data.
  @return a byte buffer which contains the stored data.
}
function TZAbstractBlob.GetBytes: TByteDynArray;
var
  I: Integer;
  TempString: AnsiString;
begin
  if not IsEmpty then
  begin
    TempString := GetString;
    SetLength(Result, System.Length(TempString));
    for I := 0 to System.Length(TempString) - 1 do
      Result[I] := Ord(TempString[I + 1]);
  end else
    Result := nil;
end;

{**
  Sets a new byte buffer to this blob content.
  @param Value a new byte buffer.
}
procedure TZAbstractBlob.SetBytes(const Value: TByteDynArray);
var
  I: Integer;
  TempString: AnsiString;
begin
  if Value <> nil then
  begin
    SetLength(TempString, High(Value) + 1);
    for I := 0 to High(Value) do
      TempString[I + 1] := Chr(Value[I]);
    SetString(TempString);
  end else
    Clear;
  FUpdated := True;
end;

{**
  Gets the associated stream object.
  @return an associated or newly created stream object.
}
function TZAbstractBlob.GetStream: TStream;
begin
  Result := TMemoryStream.Create;
  if (FBlobSize > 0) and Assigned(FBlobData) then
  begin
    Result.Size := FBlobSize;
    System.Move(FBlobData^, TMemoryStream(Result).Memory^, FBlobSize);
  end;
  Result.Position := 0;
end;

{**
  Sets a data from the specified stream into this blob.
  @param Value a stream object to be stored into this blob.
}
procedure TZAbstractBlob.SetStream(Value: TStream);
begin
  Clear;
  if Assigned(Value) then
  begin
    FBlobSize := Value.Size;
    if FBlobSize > 0 then
    begin
      GetMem(FBlobData, FBlobSize);
      Value.Position := 0;
      Value.ReadBuffer(FBlobData^, FBlobSize);
    end;
  end
  else
  begin
    FBlobSize := -1;
    FBlobData := nil;
  end;
  FUpdated := True;
end;

end.
