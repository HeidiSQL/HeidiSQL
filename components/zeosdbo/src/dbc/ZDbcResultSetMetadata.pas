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

unit ZDbcResultSetMetadata;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, Contnrs, ZDbcIntfs, ZClasses, ZCollections,
  ZGenericSqlAnalyser,
{$IFDEF VER130BELOW}
  {$IFDEF WIN32}
    Comobj,
  {$ENDIF}
{$ENDIF}
  ZTokenizer, ZSelectSchema, ZCompatibility, ZDbcResultSet;

type

  {** Implements a column information structure. }
  TZColumnInfo = class(TObject)
  protected
    FAutoIncrement: Boolean;
    FCaseSensitive: Boolean;
    FSearchable: Boolean;
    FCurrency: Boolean;
    FNullable: TZColumnNullableType;
    FSigned: Boolean;
    FColumnDisplaySize: Integer;
    FColumnLabel: string;
    FColumnName: string;
    FSchemaName: string;
    FPrecision: Integer;
    FScale: Integer;
    FTableName: string;
    FCatalogName: string;
    FColumnType: TZSQLType;
    FReadOnly: Boolean;
    FWritable: Boolean;
    FDefinitelyWritable: Boolean;
    FDefaultValue: string;
  public
    constructor Create;
    function GetColumnTypeName: string;

    property AutoIncrement: Boolean read FAutoIncrement write FAutoIncrement;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property Searchable: Boolean read FSearchable write FSearchable;
    property Currency: Boolean read FCurrency write FCurrency;
    property Nullable: TZColumnNullableType read FNullable write FNullable;

    property Signed: Boolean read FSigned write FSigned;
    property ColumnDisplaySize: Integer read FColumnDisplaySize
      write FColumnDisplaySize;
    property ColumnLabel: string read FColumnLabel write FColumnLabel;
    property ColumnName: string read FColumnName write FColumnName;
    property SchemaName: string read FSchemaName write FSchemaName;
    property Precision: Integer read FPrecision write FPrecision;
    property Scale: Integer read FScale write FScale;
    property TableName: string read FTableName write FTableName;
    property CatalogName: string read FCatalogName write FCatalogName;
    property ColumnType: TZSQLType read FColumnType write FColumnType;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Writable: Boolean read FWritable write FWritable;
    property DefinitelyWritable: Boolean read FDefinitelyWritable
      write FDefinitelyWritable;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  {** Implements Abstract ResultSet Metadata. }
  TZAbstractResultSetMetadata = class(TContainedObject, IZResultSetMetaData)
  private
    FLoaded: Boolean;
    FMetadata: IZDatabaseMetadata;
    FColumnsLabels: TStrings;
    FSQL: WideString;
    FTableColumns: TZHashMap;
    FIdentifierConvertor: IZIdentifierConvertor;
    FResultSet: TZAbstractResultSet;
  protected
    procedure LoadColumn(ColumnIndex: Integer; ColumnInfo: TZColumnInfo;
      SelectSchema: IZSelectSchema); virtual;

    function GetTableColumns(TableRef: TZTableRef): IZResultSet;
    function ReadColumnByRef(FieldRef: TZFieldRef;
      ColumnInfo: TZColumnInfo): Boolean;
    function ReadColumnByName(FieldName: string; TableRef: TZTableRef;
      ColumnInfo: TZColumnInfo): Boolean;
    procedure ClearColumn(ColumnInfo: TZColumnInfo);
    procedure LoadColumns;
    procedure ReplaceStarColumns(SelectSchema: IZSelectSchema);

    property MetaData: IZDatabaseMetadata read FMetadata write FMetadata;
    property ColumnsLabels: TStrings read FColumnsLabels write FColumnsLabels;
    property SQL: WideString read FSQL write FSQL;
    property IdentifierConvertor: IZIdentifierConvertor
      read FIdentifierConvertor write FIdentifierConvertor;
    property Loaded: Boolean read FLoaded write FLoaded;
    property ResultSet: TZAbstractResultSet read FResultSet write FResultSet;
  public
    constructor Create(Metadata: IZDatabaseMetadata; SQL: WideString;
      ParentResultSet: TZAbstractResultSet);
    destructor Destroy; override;

    function GetColumnCount: Integer; virtual;
    function IsAutoIncrement(Column: Integer): Boolean; virtual;
    function IsCaseSensitive(Column: Integer): Boolean; virtual;
    function IsSearchable(Column: Integer): Boolean; virtual;
    function IsCurrency(Column: Integer): Boolean; virtual;
    function IsNullable(Column: Integer): TZColumnNullableType; virtual;

    function IsSigned(Column: Integer): Boolean; virtual;
    function GetColumnDisplaySize(Column: Integer): Integer; virtual;
    function GetColumnLabel(Column: Integer): string; virtual;
    function GetColumnName(Column: Integer): string; virtual;
    function GetSchemaName(Column: Integer): string; virtual;
    function GetPrecision(Column: Integer): Integer; virtual;
    function GetScale(Column: Integer): Integer; virtual;
    function GetTableName(Column: Integer): string; virtual;
    function GetCatalogName(Column: Integer): string; virtual;
    function GetColumnType(Column: Integer): TZSQLType; virtual;
    function GetColumnTypeName(Column: Integer): string; virtual;
    function IsReadOnly(Column: Integer): Boolean; virtual;
    function IsWritable(Column: Integer): Boolean; virtual;
    function IsDefinitelyWritable(Column: Integer): Boolean; virtual;
    function GetDefaultValue(Column: Integer): string; virtual;
    function HasDefaultValue(Column: Integer): Boolean; virtual;
  end;

implementation

uses ZVariant, ZDbcUtils, ZDbcMetadata, ZSysUtils;

{ TZColumnInfo }

{**
  Constructs this object and assigns main properties.
}
constructor TZColumnInfo.Create;
begin
  FAutoIncrement := False;
  FCaseSensitive := False;
  FSearchable := False;
  FCurrency := False;
  FNullable := ntNullableUnknown;
  FSigned := False;
  FColumnDisplaySize := 0;
  FColumnLabel := '';
  FColumnName := '';
  FSchemaName := '';
  FPrecision := 0;
  FScale := 0;
  FTableName := '';
  FCatalogName := '';
  FDefaultValue := '';
  FColumnType := stUnknown;
  FReadOnly := True;
  FWritable := False;
  FDefinitelyWritable := False;
end;

{**
  Retrieves the designated column's database-specific type name.
  @return type name used by the database. If the column type is
    a user-defined type, then a fully-qualified type name is returned.
}
function TZColumnInfo.GetColumnTypeName: string;
begin
  Result := DefineColumnTypeName(FColumnType);
end;

{ TZAbstractResultSetMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Metadata a database metadata object.
  @param SQL an SQL query statement.
  @param ColumnsInfo a collection of columns info.
}
constructor TZAbstractResultSetMetadata.Create(Metadata: IZDatabaseMetadata;
  SQL: WideString; ParentResultSet: TZAbstractResultSet);
begin
  inherited Create(ParentResultSet);

  FMetadata := Metadata;
  FSQL := SQL;
  FLoaded := not (FMetadata <> nil);
  FTableColumns := TZHashMap.Create;
  FIdentifierConvertor := TZDefaultIdentifierConvertor.Create(FMetadata);
  FResultSet := ParentResultSet;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractResultSetMetadata.Destroy;
begin
  FIdentifierConvertor := nil;
  FMetadata := nil;
  if Assigned(FTableColumns) then
  begin
    FTableColumns.Clear;
    FTableColumns.Free;
  end;
  FTableColumns := nil;
  if FColumnsLabels <> nil then
    FColumnsLabels.Free;
  inherited Destroy;
end;

{**
  Returns the number of columns in this <code>ResultSet</code> object.
  @return the number of columns
}
function TZAbstractResultSetMetadata.GetColumnCount: Integer;
begin
  Result := FResultSet.ColumnsInfo.Count;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsAutoIncrement(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).AutoIncrement;
end;

{**
  Indicates whether a column's case matters.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsCaseSensitive(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).CaseSensitive;
end;

{**
  Indicates whether the designated column can be used in a where clause.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsSearchable(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).Searchable;
end;

{**
  Indicates whether the designated column is a cash value.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsCurrency(Column: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).Currency;
end;

{**
  Indicates the nullability of values in the designated column.
  @param column the first column is 1, the second is 2, ...
  @return the nullability status of the given column; one of <code>columnNoNulls</code>,
    <code>columnNullable</code> or <code>columnNullableUnknown</code>
}
function TZAbstractResultSetMetadata.IsNullable(
  Column: Integer): TZColumnNullableType;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).Nullable;
end;

{**
  Indicates whether values in the designated column are signed numbers.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsSigned(Column: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).Signed;
end;

{**
  Indicates the designated column's normal maximum width in characters.
  @param column the first column is 1, the second is 2, ...
  @return the normal maximum number of characters allowed as the width
    of the designated column
}
function TZAbstractResultSetMetadata.GetColumnDisplaySize(
  Column: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).ColumnDisplaySize;
end;

{**
  Gets the designated column's suggested title for use in printouts and
  displays.
  @param column the first column is 1, the second is 2, ...
  @return the suggested column title
}
function TZAbstractResultSetMetadata.GetColumnLabel(Column: Integer): string;
var
  I, J, N: Integer;
  ColumnName: string;
  ColumnsInfo: TObjectList;
begin
  { Prepare unique column labels. }
  if FColumnsLabels = nil then
  begin
    ColumnsInfo := FResultSet.ColumnsInfo;
    FColumnsLabels := TStringList.Create;
    for I := 0 to ColumnsInfo.Count - 1 do
    begin
      N := 0;
      ColumnName := TZColumnInfo(ColumnsInfo[I]).ColumnLabel;
      for J := 0 to I - 1 do
      begin
        if TZColumnInfo(ColumnsInfo[J]).ColumnLabel = ColumnName then
          Inc(N);
      end;
      if ColumnName = '' then
        ColumnName := 'Column';
      if N > 0 then
        ColumnName := ColumnName + '_' + IntToStr(N);
      FColumnsLabels.Add(ColumnName);
    end;
  end;

  Result := ColumnsLabels[Column - 1];
end;

{**
  Get the designated column's name.
  @param column the first column is 1, the second is 2, ...
  @return column name
}
function TZAbstractResultSetMetadata.GetColumnName(
  Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).ColumnName;
end;

{**
  Get the designated column's table's schema.
  @param column the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetSchemaName(
  Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).SchemaName;
end;

{**
  Get the designated column's number of decimal digits.
  @param column the first column is 1, the second is 2, ...
  @return precision
}
function TZAbstractResultSetMetadata.GetPrecision(Column: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).Precision;
end;

{**
  Gets the designated column's number of digits to right of the decimal point.
  @param column the first column is 1, the second is 2, ...
  @return scale
}
function TZAbstractResultSetMetadata.GetScale(Column: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).Scale;
end;

{**
  Gets the designated column's table name.
  @param column the first column is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetTableName(Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).TableName;
end;

{**
  Gets the designated column's table's catalog name.
  @param column the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetCatalogName(Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).CatalogName;
end;

{**
  Retrieves the designated column's SQL type.
  @param column the first column is 1, the second is 2, ...
  @return SQL type from java.sql.Types
}
function TZAbstractResultSetMetadata.GetColumnType(Column: Integer): TZSQLType;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).ColumnType;
end;

{**
  Retrieves the designated column's database-specific type name.

  @param column the first column is 1, the second is 2, ...
  @return type name used by the database. If the column type is
    a user-defined type, then a fully-qualified type name is returned.
}
function TZAbstractResultSetMetadata.GetColumnTypeName(Column: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).GetColumnTypeName;
end;

{**
  Indicates whether the designated column is definitely not writable.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsReadOnly(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).ReadOnly;
end;

{**
  Indicates whether it is possible for a write on the designated column to succeed.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsWritable(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).Writable;
end;

{**
  Indicates whether a write on the designated column will definitely succeed.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsDefinitelyWritable(
  Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).DefinitelyWritable;
end;

{**
  Gets a default value for this field.
  @param column the first column is 1, the second is 2, ...
  @return a default value for this field.
}
function TZAbstractResultSetMetadata.GetDefaultValue(
  Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).DefaultValue;
end;

{**
  Finds whether this field has a default value.
  @param column the first column is 1, the second is 2, ...
  @return true if this field has a default value.
}
function TZAbstractResultSetMetadata.HasDefaultValue(
  Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  // '' = NULL / no default value, '''''' = empty string (''), etc.
  Result := TZColumnInfo(FResultSet.ColumnsInfo[Column - 1]).DefaultValue = '';
end;

{**
  Gets a table description result set.
  @param TableRef a table reference object.
  @return a result set with table columns from database metadata.
}
function TZAbstractResultSetMetadata.GetTableColumns(
  TableRef: TZTableRef): IZResultSet;
var
  TableKey: IZAnyValue;
begin
  TableKey := TZAnyValue.CreateWithString(TableRef.FullName);
  if FTableColumns.Get(TableKey) = nil then
  begin
    Result := Metadata.GetColumns(TableRef.Catalog,
      TableRef.Schema, TableRef.Table, '');
    FTableColumns.Put(TableKey, Result);
  end else
    Result := FTableColumns.Get(TableKey) as IZResultSet;
end;

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZAbstractResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
//  ColumnInfo.ReadOnly := True;
//  ColumnInfo.Writable := False;
//  ColumnInfo.DefinitelyWritable := False;
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  ColumnInfo.ColumnName := '';
end;

{**
  Reads a column information from table metadata.
  @param FieldName a name of the field.
  @param TableRef a table reference object.
  @param ColumnInfo a column information object.
  @return <code>True</code> is column was found and read.
}
function TZAbstractResultSetMetadata.ReadColumnByName(FieldName: string;
  TableRef: TZTableRef; ColumnInfo: TZColumnInfo): Boolean;
var
  TableColumns: IZResultSet;
begin
  Result := False;
  TableColumns := GetTableColumns(TableRef);
  { Checks for unexisted table. }
  if not Assigned(TableColumns) then
    Exit;

  { Locates a column row. }
  TableColumns.BeforeFirst;
  while TableColumns.Next do
    if TableColumns.GetString(4) = FieldName then
      Break;
  if TableColumns.IsAfterLast then
  begin
    { Locates a column row with case insensitivity. }
    TableColumns.BeforeFirst;
    while TableColumns.Next do
      if AnsiUpperCase(TableColumns.GetString(4)) = AnsiUpperCase(FieldName) then
        Break;
    if TableColumns.IsAfterLast then
      Exit;
  end;

  { Reads a column information. }
  Result := True;
  ColumnInfo.CatalogName := TableColumns.GetString(1);
  ColumnInfo.SchemaName := TableColumns.GetString(2);
  ColumnInfo.TableName := TableColumns.GetString(3);
  ColumnInfo.ColumnName := FieldName;

//If the returned column information is null then the value assigned during
//the resultset.open will be kept
  if not TableColumns.IsNull(5) then
    ColumnInfo.ColumnType := TZSQLType(TableColumns.GetInt(5));
  if not TableColumns.IsNull(11) then
    ColumnInfo.Nullable := TZColumnNullableType(TableColumns.GetInt(11));
  if not TableColumns.IsNull(19) then
    ColumnInfo.AutoIncrement := TableColumns.GetBoolean(19);
  if not TableColumns.IsNull(20) then
    ColumnInfo.CaseSensitive := TableColumns.GetBoolean(20);
  if not TableColumns.IsNull(21) then
    ColumnInfo.Searchable := TableColumns.GetBoolean(21);
  if not TableColumns.IsNull(22) then
    ColumnInfo.Writable := TableColumns.GetBoolean(22);
  if not TableColumns.IsNull(23) then
    ColumnInfo.DefinitelyWritable := TableColumns.GetBoolean(23);
  if not TableColumns.IsNull(24) then
    ColumnInfo.ReadOnly := TableColumns.GetBoolean(24);
  if not TableColumns.IsNull(13) then
    ColumnInfo.DefaultValue := TableColumns.GetString(13);
end;

{**
  Reads a column information from table metadata.
  @param FieldRef a field reference object.
  @param ColumnInfo a column information object.
  @return <code>True</code> if column was found and read.
}
function TZAbstractResultSetMetadata.ReadColumnByRef(
  FieldRef: TZFieldRef; ColumnInfo: TZColumnInfo): Boolean;
begin
  Result := False;
  ClearColumn(ColumnInfo);
  { Checks for uncompleted field reference. }
  if not Assigned(FieldRef) or not Assigned(FieldRef.TableRef) then
    Exit;
  if not FieldRef.IsField then
    Exit;

  Result := ReadColumnByName(FieldRef.Field, FieldRef.TableRef, ColumnInfo);
end;

{**
  Initializes on single column of the result set.
  @param ColumnIndex a column index in the query.
  @param ColumnInfo a column information object to be initialized.
  @param SelectSchema a schema of the select statement.
}
procedure TZAbstractResultSetMetadata.LoadColumn(ColumnIndex: Integer;
  ColumnInfo: TZColumnInfo; SelectSchema: IZSelectSchema);
var
  I: Integer;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  Found: Boolean;
begin
  { Initializes single columns with specified table. }
  FieldRef := SelectSchema.LinkFieldByIndexAndShortName(
    ColumnIndex, ColumnInfo.ColumnLabel);
  ReadColumnByRef(FieldRef, ColumnInfo);
  if ColumnInfo.ColumnName <> '' then
    Exit;

  { Initializes single columns without specified table. }
  I := 0;
  Found := False;
  while (ColumnInfo.ColumnName = '') and (I < SelectSchema.TableCount)
    and not Found do
  begin
    TableRef := SelectSchema.Tables[I];
    if Assigned(FieldRef) then
      Found := ReadColumnByName(FieldRef.Field, TableRef, ColumnInfo)
    else
      Found := ReadColumnByName(ColumnInfo.ColumnLabel, TableRef, ColumnInfo);
    Inc(I);
  end;
end;

{**
  Replaces '*' columns in the select schema.
  @param SelectSchema a query select schema.
}
procedure TZAbstractResultSetMetadata.ReplaceStarColumns(
  SelectSchema: IZSelectSchema);
var
  I: Integer;
  Current: TZFieldRef;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  ResultSet: IZResultSet;
begin
  I := 0;
  while I < SelectSchema.FieldCount do
  begin
    Current := SelectSchema.Fields[I];
    if (Current.Field = '*') and (Current.TableRef <> nil) then
    begin
      TableRef := Current.TableRef;
      ResultSet := Self.GetTableColumns(TableRef);
      if ResultSet <> nil then
      begin
        ResultSet.BeforeFirst;
        while ResultSet.Next do
        begin
          FieldRef := TZFieldRef.Create(True, TableRef.Catalog, TableRef.Schema,
            TableRef.Table, ResultSet.GetString(4), '', TableRef);
          SelectSchema.InsertField(I, FieldRef);
          Inc(I);
        end;
      end;
      SelectSchema.DeleteField(Current);
      Dec(I);
    end;
    Inc(I);
  end;
end;

{**
  Initializes columns with additional data.
}
procedure TZAbstractResultSetMetadata.LoadColumns;
var
  I: Integer;
  Driver: IZDriver;
  Tokenizer: IZTokenizer;
  StatementAnalyser: IZStatementAnalyser;
  SelectSchema: IZSelectSchema;
  FillByIndices: Boolean;
begin
  { Parses the Select statement and retrieves a schema object. }
  Driver := Metadata.GetConnection.GetDriver;
  Tokenizer := Driver.GetTokenizer;
  StatementAnalyser := Driver.GetStatementAnalyser;
  SelectSchema := StatementAnalyser.DefineSelectSchemaFromQuery(Tokenizer, SQL);
  if Assigned(SelectSchema) then
  begin
    SelectSchema.LinkReferences(IdentifierConvertor);
    ReplaceStarColumns(SelectSchema);
    FillByIndices := SelectSchema.FieldCount = FResultSet.ColumnsInfo.Count;
    for I := 0 to FResultSet.ColumnsInfo.Count - 1 do
    begin
      if FillByIndices then
        LoadColumn(I + 1, TZColumnInfo(FResultSet.ColumnsInfo[I]), SelectSchema)
      else
        LoadColumn(-1, TZColumnInfo(FResultSet.ColumnsInfo[I]), SelectSchema);
    end;
  end;
  Loaded := True;
end;

end.

