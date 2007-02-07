{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Abstract Database Connectivity Classes        }
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

unit ZDbcMetadata;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
{$IFDEF VER130BELOW}
  {$IFDEF WIN32}
    Comobj,
  {$ENDIF}
{$ENDIF}
  Classes, SysUtils, Contnrs, ZSysUtils, ZClasses, ZDbcIntfs, ZTokenizer,
  ZDbcResultSetMetadata, ZDbcCachedResultSet, ZDbcCache, ZCompatibility,
  ZSelectSchema;

const
  procedureColumnUnknown = 0;
  procedureColumnIn = 1;
  procedureColumnInOut = 2;
  procedureColumnOut = 4;
  procedureColumnReturn = 5;
  procedureColumnResult = 3;
  procedureNoNulls = 0;
  procedureNullable = 1;
  procedureNullableUnknown = 2;

type

  {** Defines a metadata resultset column definition. }
  TZMetadataColumnDef = packed record
    Name: string;
    SQLType: TZSQLType;
    Length: Integer
  end;

  {** Defines a dynamic array of metadata column definitions. }
  TZMetadataColumnDefs = array of TZMetadataColumnDef;

  {** Represents a Virtual ResultSet interface. }
  IZVirtualResultSet = interface(IZCachedResultSet)
    ['{D84055AC-BCD5-40CD-B408-6F11AF000C96}']
    procedure SetType(Value: TZResultSetType);
    procedure SetConcurrency(Value: TZResultSetConcurrency);
  end;

  {** Implements Virtual ResultSet. }
  TZVirtualResultSet = class(TZAbstractCachedResultSet, IZVirtualResultSet)
  protected
    procedure CalculateRowDefaults(RowAccessor: TZRowAccessor); override;
    procedure PostRowUpdates(OldRowAccessor, NewRowAccessor: TZRowAccessor);
      override;
  public
    constructor CreateWithStatement(const SQL: string; Statement: IZStatement);
    constructor CreateWithColumns(ColumnsInfo: TObjectList; const SQL: string);
    destructor Destroy; override;
  end;

  {** Implements Abstract Database Metadata. }
  TZAbstractDatabaseMetadata = class(TContainedObject, IZDatabaseMetadata)
  private
    FConnection: Pointer;
    FUrl: string;
    FInfo: TStrings;
    FCachedResultSets: IZHashMap;
  protected
    constructor Create(ParentConnection: IZConnection;
      const Url: string; Info: TStrings);

    { Metadata ResultSets Caching. }
    procedure AddResultSetToCache(const Key: string; ResultSet: IZResultSet);
    function GetResultSetFromCache(const Key: string): IZResultSet;
    function ConstructVirtualResultSet(ColumnsDefs: TZMetadataColumnDefs):
      IZVirtualResultSet;
    function CopyToVirtualResultSet(SrcResultSet: IZResultSet;
      DestResultSet: IZVirtualResultSet): IZVirtualResultSet;
    function CloneCachedResultSet(ResultSet: IZResultSet): IZResultSet;

    property Url: string read FUrl;
    property Info: TStrings read FInfo;
    property CachedResultSets: IZHashMap read FCachedResultSets
      write FCachedResultSets;
  public
    destructor Destroy; override;

    function AllProceduresAreCallable: Boolean; virtual;
    function AllTablesAreSelectable: Boolean; virtual;
    function GetURL: string; virtual;
    function GetUserName: string; virtual;
    function IsReadOnly: Boolean; virtual;
    function NullsAreSortedHigh: Boolean; virtual;
    function NullsAreSortedLow: Boolean; virtual;
    function NullsAreSortedAtStart: Boolean; virtual;
    function NullsAreSortedAtEnd: Boolean; virtual;
    function GetDatabaseProductName: string; virtual;
    function GetDatabaseProductVersion: string; virtual;
    function GetDriverName: string; virtual;
    function GetDriverVersion: string; virtual;
    function GetDriverMajorVersion: Integer; virtual;
    function GetDriverMinorVersion: Integer; virtual;
    function UsesLocalFiles: Boolean; virtual;
    function UsesLocalFilePerTable: Boolean; virtual;
    function SupportsMixedCaseIdentifiers: Boolean; virtual;
    function StoresUpperCaseIdentifiers: Boolean; virtual;
    function StoresLowerCaseIdentifiers: Boolean; virtual;
    function StoresMixedCaseIdentifiers: Boolean; virtual;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; virtual;
    function StoresUpperCaseQuotedIdentifiers: Boolean; virtual;
    function StoresLowerCaseQuotedIdentifiers: Boolean; virtual;
    function StoresMixedCaseQuotedIdentifiers: Boolean; virtual;
    function GetIdentifierQuoteString: string; virtual;
    function GetSQLKeywords: string; virtual;
    function GetNumericFunctions: string; virtual;
    function GetStringFunctions: string; virtual;
    function GetSystemFunctions: string; virtual;
    function GetTimeDateFunctions: string; virtual;
    function GetSearchStringEscape: string; virtual;
    function GetExtraNameCharacters: string; virtual;

    function SupportsAlterTableWithAddColumn: Boolean; virtual;
    function SupportsAlterTableWithDropColumn: Boolean; virtual;
    function SupportsColumnAliasing: Boolean; virtual;
    function NullPlusNonNullIsNull: Boolean; virtual;
    function SupportsConvert: Boolean; virtual;
    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
      Boolean; virtual;
    function SupportsTableCorrelationNames: Boolean; virtual;
    function SupportsDifferentTableCorrelationNames: Boolean; virtual;
    function SupportsExpressionsInOrderBy: Boolean; virtual;
    function SupportsOrderByUnrelated: Boolean; virtual;
    function SupportsGroupBy: Boolean; virtual;
    function SupportsGroupByUnrelated: Boolean; virtual;
    function SupportsGroupByBeyondSelect: Boolean; virtual;
    function SupportsLikeEscapeClause: Boolean; virtual;
    function SupportsMultipleResultSets: Boolean; virtual;
    function SupportsMultipleTransactions: Boolean; virtual;
    function SupportsNonNullableColumns: Boolean; virtual;
    function SupportsMinimumSQLGrammar: Boolean; virtual;
    function SupportsCoreSQLGrammar: Boolean; virtual;
    function SupportsExtendedSQLGrammar: Boolean; virtual;
    function SupportsANSI92EntryLevelSQL: Boolean; virtual;
    function SupportsANSI92IntermediateSQL: Boolean; virtual;
    function SupportsANSI92FullSQL: Boolean; virtual;
    function SupportsIntegrityEnhancementFacility: Boolean; virtual;
    function SupportsOuterJoins: Boolean; virtual;
    function SupportsFullOuterJoins: Boolean; virtual;
    function SupportsLimitedOuterJoins: Boolean; virtual;
    function GetSchemaTerm: string; virtual;
    function GetProcedureTerm: string; virtual;
    function GetCatalogTerm: string; virtual;
    function IsCatalogAtStart: Boolean; virtual;
    function GetCatalogSeparator: string; virtual;
    function SupportsSchemasInDataManipulation: Boolean; virtual;
    function SupportsSchemasInProcedureCalls: Boolean; virtual;
    function SupportsSchemasInTableDefinitions: Boolean; virtual;
    function SupportsSchemasInIndexDefinitions: Boolean; virtual;
    function SupportsSchemasInPrivilegeDefinitions: Boolean; virtual;
    function SupportsCatalogsInDataManipulation: Boolean; virtual;
    function SupportsCatalogsInProcedureCalls: Boolean; virtual;
    function SupportsCatalogsInTableDefinitions: Boolean; virtual;
    function SupportsCatalogsInIndexDefinitions: Boolean; virtual;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean; virtual;
    function SupportsPositionedDelete: Boolean; virtual;
    function SupportsPositionedUpdate: Boolean; virtual;
    function SupportsSelectForUpdate: Boolean; virtual;
    function SupportsStoredProcedures: Boolean; virtual;
    function SupportsSubqueriesInComparisons: Boolean; virtual;
    function SupportsSubqueriesInExists: Boolean; virtual;
    function SupportsSubqueriesInIns: Boolean; virtual;
    function SupportsSubqueriesInQuantifieds: Boolean; virtual;
    function SupportsCorrelatedSubqueries: Boolean; virtual;
    function SupportsUnion: Boolean; virtual;
    function SupportsUnionAll: Boolean;  virtual;
    function SupportsOpenCursorsAcrossCommit: Boolean; virtual;
    function SupportsOpenCursorsAcrossRollback: Boolean; virtual;
    function SupportsOpenStatementsAcrossCommit: Boolean; virtual;
    function SupportsOpenStatementsAcrossRollback: Boolean; virtual;

    function GetMaxBinaryLiteralLength: Integer; virtual;
    function GetMaxCharLiteralLength: Integer; virtual;
    function GetMaxColumnNameLength: Integer; virtual;
    function GetMaxColumnsInGroupBy: Integer; virtual;
    function GetMaxColumnsInIndex: Integer; virtual;
    function GetMaxColumnsInOrderBy: Integer; virtual;
    function GetMaxColumnsInSelect: Integer; virtual;
    function GetMaxColumnsInTable: Integer; virtual;
    function GetMaxConnections: Integer; virtual;
    function GetMaxCursorNameLength: Integer; virtual;
    function GetMaxIndexLength: Integer; virtual;
    function GetMaxSchemaNameLength: Integer; virtual;
    function GetMaxProcedureNameLength: Integer; virtual;
    function GetMaxCatalogNameLength: Integer; virtual;
    function GetMaxRowSize: Integer; virtual;
    function DoesMaxRowSizeIncludeBlobs: Boolean; virtual;
    function GetMaxStatementLength: Integer; virtual;
    function GetMaxStatements: Integer; virtual;
    function GetMaxTableNameLength: Integer; virtual;
    function GetMaxTablesInSelect: Integer; virtual;
    function GetMaxUserNameLength: Integer; virtual;

    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; virtual;
    function SupportsTransactions: Boolean; virtual;
    function SupportsTransactionIsolationLevel(Level: TZTransactIsolationLevel):
      Boolean; virtual;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; virtual;
    function SupportsDataManipulationTransactionsOnly: Boolean; virtual;
    function DataDefinitionCausesTransactionCommit: Boolean; virtual;
    function DataDefinitionIgnoredInTransactions: Boolean; virtual;

    function GetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; virtual;
    function GetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; virtual;

    function GetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; virtual;
    function GetSchemas: IZResultSet; virtual;
    function GetCatalogs: IZResultSet; virtual;
    function GetTableTypes: IZResultSet; virtual;
    function GetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; virtual;
    function GetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; virtual;

    function GetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; virtual;
    function GetBestRowIdentifier(const Catalog: string; const Schema: string;
      const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet; virtual;
    function GetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; virtual;

    function GetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; virtual;
    function GetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; virtual;
    function GetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; virtual;
    function GetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; virtual;

    function GetTypeInfo: IZResultSet; virtual;

    function GetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; virtual;

    function GetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet; virtual;

    function SupportsResultSetType(_Type: TZResultSetType): Boolean; virtual;
    function SupportsResultSetConcurrency(_Type: TZResultSetType;
      Concurrency: TZResultSetConcurrency): Boolean; virtual;
    function SupportsBatchUpdates: Boolean; virtual;

    function GetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; virtual;

    function GetConnection: IZConnection; virtual;

    function GetIdentifierConvertor: IZIdentifierConvertor; virtual;
    procedure ClearCache; overload;virtual;
		procedure ClearCache(const Key: string);overload;virtual;
  end;

  {** Implements a default Case Sensitive/Unsensitive identifier convertor. }
  TZDefaultIdentifierConvertor = class (TZAbstractObject,
    IZIdentifierConvertor)
  private
    FMetadata: IZDatabaseMetadata;
  protected
    property Metadata: IZDatabaseMetadata read FMetadata write FMetadata;

    function IsLowerCase(const Value: string): Boolean;
    function IsUpperCase(const Value: string): Boolean;
    function IsSpecialCase(const Value: string): Boolean;
  public
    constructor Create(Metadata: IZDatabaseMetadata);

    function IsCaseSensitive(const Value: string): Boolean;
    function IsQuoted(const Value: string): Boolean;
    function Quote(const Value: string): string;
    function ExtractQuote(const Value: string): string;
  end;

  function GetTablesMetaDataCacheKey(Const Catalog:String;
      Const SchemaPattern:String;	Const TableNamePattern:String;const Types: TStringDynArray):String;


var
  ProceduresColumnsDynArray: TZMetadataColumnDefs;
  ProceduresColColumnsDynArray: TZMetadataColumnDefs;
  TableColumnsDynArray: TZMetadataColumnDefs;
  SchemaColumnsDynArray: TZMetadataColumnDefs;
  CatalogColumnsDynArray: TZMetadataColumnDefs;
  TableTypeColumnsDynArray: TZMetadataColumnDefs;
  TableColColumnsDynArray: TZMetadataColumnDefs;
  TableColPrivColumnsDynArray: TZMetadataColumnDefs;
  TablePrivColumnsDynArray: TZMetadataColumnDefs;
  BestRowIdentColumnsDynArray: TZMetadataColumnDefs;
  TableColVerColumnsDynArray: TZMetadataColumnDefs;
  PrimaryKeyColumnsDynArray: TZMetadataColumnDefs;
  ImportedKeyColumnsDynArray: TZMetadataColumnDefs;
  ExportedKeyColumnsDynArray: TZMetadataColumnDefs;
  CrossRefColumnsDynArray: TZMetadataColumnDefs;
  TypeInfoColumnsDynArray: TZMetadataColumnDefs;
  IndexInfoColumnsDynArray: TZMetadataColumnDefs;
  SequenceColumnsDynArray: TZMetadataColumnDefs;
  UDTColumnsDynArray: TZMetadataColumnDefs;

implementation

uses ZVariant, ZCollections;

{ TZAbstractDatabaseMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZAbstractDatabaseMetadata.Create(
  ParentConnection: IZConnection; const Url: string; Info: TStrings);
begin
  inherited Create(ParentConnection);
  FConnection := Pointer(ParentConnection);
  FUrl := Url;
  FInfo := Info;
  FCachedResultSets := TZHashMap.Create;
end;

{**  Destroys this object and cleanups the memory.
}
destructor TZAbstractDatabaseMetadata.Destroy;
begin
  FCachedResultSets.Clear;
  FCachedResultSets := nil;
  
  inherited Destroy;
end;

{**
  Retrieves the connection that produced this metadata object.
  @return the connection that produced this metadata object
}
function TZAbstractDatabaseMetadata.GetConnection: IZConnection;
begin
  Result := IZConnection(FConnection);
end;

{**
  Constructs a virtual result set object.
  @param ColumnsDefs an array of column definition objects.
  @return a created result set.
}
function TZAbstractDatabaseMetadata.ConstructVirtualResultSet(
  ColumnsDefs: TZMetadataColumnDefs): IZVirtualResultSet;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  ColumnsInfo: TObjectList;
begin
  ColumnsInfo := TObjectList.Create;
  try
    for I := 0 to High(ColumnsDefs) do
    begin
      ColumnInfo := TZColumnInfo.Create;
      with ColumnInfo do
      begin
        ColumnLabel := ColumnsDefs[I].Name;
        ColumnType := ColumnsDefs[I].SQLType;
        ColumnDisplaySize := ColumnsDefs[I].Length;
        Precision := ColumnsDefs[I].Length;
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;

    Result := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '');
    with Result do
    begin
      SetType(rtScrollInsensitive);
      SetConcurrency(rcUpdatable);
    end;
  finally
    ColumnsInfo.Free;
  end;
end;

{**
  Clears all cached metadata.
}
procedure TZAbstractDatabaseMetadata.ClearCache;
begin
  FCachedResultSets.Clear;
end;

{**
  Clears specific cached metadata.
}
procedure TZAbstractDatabaseMetadata.ClearCache(const Key: string);
var
  TempKey: IZAnyValue;
begin
  TempKey := TZAnyValue.CreateWithString(Key);
  FCachedResultSets.Remove(TempKey);
end;

{**
  Adds resultset to the internal cache.
  @param Key a resultset unique key value.
  @param ResultSet a resultset interface.
}
procedure TZAbstractDatabaseMetadata.AddResultSetToCache(const Key: string;
  ResultSet: IZResultSet);
var
  TempKey: IZAnyValue;
begin
  TempKey := TZAnyValue.CreateWithString(Key);
  FCachedResultSets.Put(TempKey, CloneCachedResultSet(ResultSet));
end;

{**
  Gets a resultset interface from the internal cache by key.
  @param Key a resultset unique key value.
  @returns a cached resultset interface or <code>nil</code> otherwise.
}
function TZAbstractDatabaseMetadata.GetResultSetFromCache(
  const Key: string): IZResultSet;
var
  TempKey: IZAnyValue;
begin
  // Disables retrieving cached results to fix
  // bug #1571441 "Creating a new Field >> changes in this field are not saved."
  exit;
  TempKey := TZAnyValue.CreateWithString(Key);
  Result := FCachedResultSets.Get(TempKey) as IZResultSet;
  if Result <> nil then
    Result := CloneCachedResultSet(Result);
end;

{**
  Copies on result set to another one from the current position.
  @param SrcResultSet a source result set.
  @param DestResultSet a destination result set.
  @returns a destination result set.
}
function TZAbstractDatabaseMetadata.CopyToVirtualResultSet(
  SrcResultSet: IZResultSet; DestResultSet: IZVirtualResultSet):
  IZVirtualResultSet;
var
  I: Integer;
  Metadata: IZResultSetMetadata;
begin
  DestResultSet.SetType(rtScrollInsensitive);
  DestResultSet.SetConcurrency(rcUpdatable);

  Metadata := SrcResultSet.GetMetadata;
  while SrcResultSet.Next do
  begin
    DestResultSet.MoveToInsertRow;
    for I := 1 to Metadata.GetColumnCount do
    begin
      case Metadata.GetColumnType(I) of
        stBoolean:
          DestResultSet.UpdateBoolean(I, SrcResultSet.GetBoolean(I));
        stByte:
          DestResultSet.UpdateByte(I, SrcResultSet.GetByte(I));
        stShort:
          DestResultSet.UpdateShort(I, SrcResultSet.GetShort(I));
        stInteger:
          DestResultSet.UpdateInt(I, SrcResultSet.GetInt(I));
        stLong:
          DestResultSet.UpdateLong(I, SrcResultSet.GetLong(I));
        stFloat:
          DestResultSet.UpdateFloat(I, SrcResultSet.GetFloat(I));
        stDouble:
          DestResultSet.UpdateDouble(I, SrcResultSet.GetDouble(I));
        stBigDecimal:
          DestResultSet.UpdateBigDecimal(I, SrcResultSet.GetBigDecimal(I));
        stString:
          DestResultSet.UpdateString(I, SrcResultSet.GetString(I));
        stUnicodeString:
          DestResultSet.UpdateUnicodeString(I, SrcResultSet.GetUnicodeString(I));
        stBytes:
          DestResultSet.UpdateBytes(I, SrcResultSet.GetBytes(I));
        stDate:
          DestResultSet.UpdateDate(I, SrcResultSet.GetDate(I));
        stTime:
          DestResultSet.UpdateTime(I, SrcResultSet.GetTime(I));
        stTimestamp:
          DestResultSet.UpdateTimestamp(I, SrcResultSet.GetTimestamp(I));
        stAsciiStream,
        stUnicodeStream,
        stBinaryStream:
          DestResultSet.UpdateString(I, SrcResultSet.GetString(I));
      end;
      if SrcResultSet.WasNull then
        DestResultSet.UpdateNull(I);
    end;
    DestResultSet.InsertRow;
  end;

  DestResultSet.BeforeFirst;
  DestResultSet.SetConcurrency(rcReadOnly);
  Result := DestResultSet;
end;

{**
  Clones the cached resultset.
  @param ResultSet the resultset to be cloned.
  @returns the clone of the specified resultset.
}
function TZAbstractDatabaseMetadata.CloneCachedResultSet(
  ResultSet: IZResultSet): IZResultSet;
var
  I: Integer;
  Metadata: IZResultSetMetadata;
  ColumnInfo: TZColumnInfo;
  ColumnsInfo: TObjectList;
begin
  Result := nil;
  Metadata := ResultSet.GetMetadata;
  ColumnsInfo := TObjectList.Create;
  try
    for I := 1 to Metadata.GetColumnCount do
    begin
      ColumnInfo := TZColumnInfo.Create;
      with ColumnInfo do
      begin
        ColumnLabel := Metadata.GetColumnLabel(I);
        ColumnType := Metadata.GetColumnType(I);
        ColumnDisplaySize := Metadata.GetPrecision(I);
        Precision := Metadata.GetPrecision(I);
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;

    ResultSet.BeforeFirst;
    Result := CopyToVirtualResultSet(ResultSet,
      TZVirtualResultSet.CreateWithColumns(ColumnsInfo, ''));
    ResultSet.BeforeFirst;
  finally
    ColumnsInfo.Free;
  end;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  Can all the procedures returned by getProcedures be called by the
  current user?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.AllProceduresAreCallable: Boolean;
begin
  Result := True;
end;

{**
  Can all the tables returned by getTable be SELECTed by the
  current user?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.AllTablesAreSelectable: Boolean;
begin
  Result := True;
end;

{**
  What's the url for this database?
  @return the url or null if it cannot be generated
}
function TZAbstractDatabaseMetadata.GetURL: string;
begin
  Result := FUrl;
end;

{**
  What's our user name as known to the database?
  @return our database user name
}
function TZAbstractDatabaseMetadata.GetUserName: string;
begin
  Result := FInfo.Values['UID'];
  if Result = '' then
    Result := FInfo.Values['username'];
end;

{**
  Is the database in read-only mode?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.IsReadOnly: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted high?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.NullsAreSortedHigh: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted low?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.NullsAreSortedLow: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted at the start regardless of sort order?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.NullsAreSortedAtStart: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted at the end regardless of sort order?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.NullsAreSortedAtEnd: Boolean;
begin
  Result := False;
end;

{**
  What's the name of this database product?
  @return database product name
}
function TZAbstractDatabaseMetadata.GetDatabaseProductName: string;
begin
  Result := '';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZAbstractDatabaseMetadata.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZAbstractDatabaseMetadata.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver';
end;

{**
  What's the version of this JDBC driver?
  @return JDBC driver version
}
function TZAbstractDatabaseMetadata.GetDriverVersion: string;
begin
  Result := Format('%d.%d', [GetDriverMajorVersion, GetDriverMinorVersion]);
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZAbstractDatabaseMetadata.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZAbstractDatabaseMetadata.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database store tables in a local file?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.UsesLocalFiles: Boolean;
begin
  Result := True;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZAbstractDatabaseMetadata.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  What's the string used to quote SQL identifiers?
  This returns a space " " if identifier quoting isn't supported.
  A JDBC Compliant<sup><font size=-2>TM</font></sup>
  driver always uses a double quote character.
  @return the quoting string
}
function TZAbstractDatabaseMetadata.GetIdentifierQuoteString: string;
begin
  Result := '"';
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZAbstractDatabaseMetadata.GetSQLKeywords: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractDatabaseMetadata.GetNumericFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractDatabaseMetadata.GetStringFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractDatabaseMetadata.GetSystemFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZAbstractDatabaseMetadata.GetTimeDateFunctions: string;
begin
  Result := '';
end;

{**
  Gets the string that can be used to escape wildcard characters.
  This is the string that can be used to escape '_' or '%' in
  the string pattern style catalog search parameters.

  <P>The '_' character represents any single character.
  <P>The '%' character represents any sequence of zero or
  more characters.

  @return the string used to escape wildcard characters
}
function TZAbstractDatabaseMetadata.GetSearchStringEscape: string;
begin
  Result := '%';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZAbstractDatabaseMetadata.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Is "ALTER TABLE" with add column supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsAlterTableWithAddColumn: Boolean;
begin
  Result := True;
end;

{**
  Is "ALTER TABLE" with drop column supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsAlterTableWithDropColumn: Boolean;
begin
  Result := True;
end;

{**
  Is column aliasing supported?

  <P>If so, the SQL AS clause can be used to provide names for
  computed columns or to provide alias names for columns as
  required.
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsColumnAliasing: Boolean;
begin
  Result := True;
end;

{**
  Are concatenations between NULL and non-NULL values NULL?
  For SQL-92 compliance, a JDBC technology-enabled driver will
  return <code>true</code>.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.NullPlusNonNullIsNull: Boolean;
begin
  Result := True;
end;

{**
  Is the CONVERT function between SQL types supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsConvert: Boolean;
begin
  Result := False;
end;

{**
  Is CONVERT between the given SQL types supported?
  @param fromType the type to convert from
  @param toType the type to convert to
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Types
}
function TZAbstractDatabaseMetadata.SupportsConvertForTypes(
  FromType: TZSQLType; ToType: TZSQLType): Boolean;
begin
  Result := False;
end;

{**
  Are table correlation names supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsTableCorrelationNames: Boolean;
begin
  Result := True;
end;

{**
  If table correlation names are supported, are they restricted
  to be different from the names of the tables?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsDifferentTableCorrelationNames: Boolean;
begin
  Result := False;
end;

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := False;
end;

{**
  Is the escape character in "LIKE" clauses supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsLikeEscapeClause: Boolean;
begin
  Result := True;
end;

{**
  Are multiple <code>ResultSet</code> from a single execute supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsMultipleResultSets: Boolean;
begin
  Result := True;
end;

{**
  Can we have multiple transactions open at once (on different
  connections)?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsMultipleTransactions: Boolean;
begin
  Result := True;
end;

{**
  Can columns be defined as non-nullable?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsNonNullableColumns: Boolean;
begin
  Result := True;
end;

{**
  Is the ODBC Minimum SQL grammar supported?
  All JDBC Compliant<sup><font size=-2>TM</font></sup> drivers must return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsMinimumSQLGrammar: Boolean;
begin
  Result := True;
end;

{**
  Is the ODBC Core SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsCoreSQLGrammar: Boolean;
begin
  Result := True;
end;

{**
  Is the ODBC Extended SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsExtendedSQLGrammar: Boolean;
begin
  Result := True;
end;

{**
  Is the ANSI92 entry level SQL grammar supported?
  All JDBC Compliant<sup><font size=-2>TM</font></sup> drivers must return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsANSI92EntryLevelSQL: Boolean;
begin
  Result := True;
end;

{**
  Is the ANSI92 intermediate SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsANSI92IntermediateSQL: Boolean;
begin
  Result := True;
end;

{**
  Is the ANSI92 full SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsANSI92FullSQL: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  Is some form of outer join supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsOuterJoins: Boolean;
begin
  Result := True;
end;

{**
  Are full nested outer joins supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsFullOuterJoins: Boolean;
begin
  Result := True;
end;

{**
  Is there limited support for outer joins?  (This will be true
  if supportFullOuterJoins is true.)
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsLimitedOuterJoins: Boolean;
begin
  Result := True;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZAbstractDatabaseMetadata.GetSchemaTerm: string;
begin
  Result := 'Schema';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZAbstractDatabaseMetadata.GetProcedureTerm: string;
begin
  Result := 'Procedure';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZAbstractDatabaseMetadata.GetCatalogTerm: string;
begin
  Result := 'Catalog';
end;

{**
  Does a catalog appear at the start of a qualified table name?
  (Otherwise it appears at the end)
  @return true if it appears at the start
}
function TZAbstractDatabaseMetadata.IsCatalogAtStart: Boolean;
begin
  Result := False;
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZAbstractDatabaseMetadata.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsPositionedDelete: Boolean;
begin
  Result := False;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsPositionedUpdate: Boolean;
begin
  Result := False;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSelectForUpdate: Boolean;
begin
  Result := False;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsStoredProcedures: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSubqueriesInExists: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSubqueriesInIns: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := False;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := False;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsUnion: Boolean;
begin
  Result := False;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsUnionAll: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseMetadata.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseMetadata.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseMetadata.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseMetadata.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := True;
end;

//----------------------------------------------------------------------
// The following group of methods exposes various limitations
// based on the target database with the current driver.
// Unless otherwise specified, a result of zero means there is no
// limit, or the limit is not known.

{**
  How many hex characters can you have in an inline binary literal?
  @return max binary literal length in hex characters;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxCharLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxColumnNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxColumnsInIndex: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxColumnsInTable: Integer;
begin
  Result := 0;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxCursorNameLength: Integer;
begin
  Result := 0;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxSchemaNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxProcedureNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxCatalogNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxRowSize: Integer;
begin
  Result := 0;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxStatementLength: Integer;
begin
  Result := 0;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxTableNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseMetadata.GetMaxUserNameLength: Integer;
begin
  Result := 0;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZAbstractDatabaseMetadata.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZAbstractDatabaseMetadata.SupportsTransactionIsolationLevel(
  Level: TZTransactIsolationLevel): Boolean;
begin
  Result := True;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Gets a description of the stored procedures available in a
  catalog.

  <P>Only procedure descriptions matching the schema and
  procedure name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM, and PROCEDURE_NAME.

  <P>Each procedure description has the the following columns:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
   <LI> reserved for future use
   <LI> reserved for future use
   <LI> reserved for future use
 	<LI><B>REMARKS</B> String => explanatory comment on the procedure
 	<LI><B>PROCEDURE_TYPE</B> short => kind of procedure:
       <UL>
       <LI> procedureResultUnknown - May return a result
       <LI> procedureNoResult - Does not return a result
       <LI> procedureReturnsResult - Returns a result
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @return <code>ResultSet</code> - each row is a procedure description
  @see #getSearchStringEscape
}
function TZAbstractDatabaseMetadata.GetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-procedures:%s:%s:%s',
    [Catalog, SchemaPattern, ProcedureNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ProceduresColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a catalog's stored procedure parameters
  and result columns.

  <P>Only descriptions matching the schema, procedure and
  parameter name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
  if any, is first. Next are the parameter descriptions in call
  order. The column descriptions follow in column number order.

  <P>Each row in the <code>ResultSet</code> is a parameter description or
  column description with the following fields:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
 	<LI><B>COLUMN_NAME</B> String => column/parameter name
 	<LI><B>COLUMN_TYPE</B> Short => kind of column/parameter:
       <UL>
       <LI> procedureColumnUnknown - nobody knows
       <LI> procedureColumnIn - IN parameter
       <LI> procedureColumnInOut - INOUT parameter
       <LI> procedureColumnOut - OUT parameter
       <LI> procedureColumnReturn - procedure return value
       <LI> procedureColumnResult - result column in <code>ResultSet</code>
       </UL>
   <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => SQL type name, for a UDT type the
   type name is fully qualified
 	<LI><B>PRECISION</B> int => precision
 	<LI><B>LENGTH</B> int => length in bytes of data
 	<LI><B>SCALE</B> short => scale
 	<LI><B>RADIX</B> short => radix
 	<LI><B>NULLABLE</B> short => can it contain NULL?
       <UL>
       <LI> procedureNoNulls - does not allow NULL values
       <LI> procedureNullable - allows NULL values
       <LI> procedureNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing parameter/column
   </OL>

  <P><B>Note:</B> Some databases may not return the column
  descriptions for a procedure. Additional columns beyond
  REMARKS can be defined by the database.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row describes a stored procedure parameter or
       column
  @see #getSearchStringEscape
}
function TZAbstractDatabaseMetadata.GetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-procedure-columns:%s:%s:%s:%s',
    [Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ProceduresColColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of tables available in a catalog.

  <P>Only table descriptions matching the catalog, schema, table
  name and type criteria are returned.  They are ordered by
  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.

  <P>Each table description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	<LI><B>REMARKS</B> String => explanatory comment on the table
   </OL>

  <P><B>Note:</B> Some databases may not return information for
  all tables.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param types a list of table types to include; null returns all types
  @return <code>ResultSet</code> - each row is a table description
  @see #getSearchStringEscape
}
function TZAbstractDatabaseMetadata.GetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  Key: string;
begin
  Key := GetTablesMetaDataCacheKey(Catalog,SchemaPattern,TableNamePattern,Types);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets the schema names available in this database.  The results
  are ordered by schema name.

  <P>The schema column is:
   <OL>
 	<LI><B>TABLE_SCHEM</B> String => schema name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  schema name
}
function TZAbstractDatabaseMetadata.GetSchemas: IZResultSet;
var
  Key: string;
begin
  Key := 'get-schemas';

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(SchemaColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets the catalog names available in this database.  The results
  are ordered by catalog name.

  <P>The catalog column is:
   <OL>
 	<LI><B>TABLE_CAT</B> String => catalog name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  catalog name
}
function TZAbstractDatabaseMetadata.GetCatalogs: IZResultSet;
var
  Key: string;
begin
  Key := 'get-catalogs';

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(CatalogColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets the table types available in this database.  The results
  are ordered by table type.

  <P>The table type is:
   <OL>
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZAbstractDatabaseMetadata.GetTableTypes: IZResultSet;
var
  Key: string;
begin
  Key := 'get-table-types';

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);

//    Result.MoveToInsertRow;
//    Result.UpdateString(1, 'TABLE');
//    Result.InsertRow;

    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of table columns available in
  the specified catalog.

  <P>Only column descriptions matching the catalog, schema, table
  and column name criteria are returned.  They are ordered by
  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
 	<LI><B>COLUMN_SIZE</B> int => column size.  For char or date
 	    types this is the maximum number of characters, for numeric or
 	    decimal types this is precision.
 	<LI><B>BUFFER_LENGTH</B> is not used.
 	<LI><B>DECIMAL_DIGITS</B> int => the number of fractional digits
 	<LI><B>NUM_PREC_RADIX</B> int => Radix (typically either 10 or 2)
 	<LI><B>NULLABLE</B> int => is NULL allowed?
       <UL>
       <LI> columnNoNulls - might not allow NULL values
       <LI> columnNullable - definitely allows NULL values
       <LI> columnNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing column (may be null)
  	<LI><B>COLUMN_DEF</B> String => default value (may be null)
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>CHAR_OCTET_LENGTH</B> int => for char types the
        maximum number of bytes in the column
 	<LI><B>ORDINAL_POSITION</B> int	=> index of column in table
       (starting at 1)
 	<LI><B>IS_NULLABLE</B> String => "NO" means column definitely
       does not allow NULL values; "YES" means the column might
       allow NULL values.  An empty string means nobody knows.
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column description
  @see #getSearchStringEscape
}
function TZAbstractDatabaseMetadata.GetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-columns:%s:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of the access rights for a table's columns.

  <P>Only privileges matching the column name criteria are
  returned.  They are ordered by COLUMN_NAME and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column privilege description
  @see #getSearchStringEscape
}
function TZAbstractDatabaseMetadata.GetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-column-privileges:%s:%s:%s:%s',
    [Catalog, Schema, Table, ColumnNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of the access rights for each table available
  in a catalog. Note that a table privilege applies to one or
  more columns in the table. It would be wrong to assume that
  this priviledge applies to all columns (this may be true for
  some systems but is not true for all.)

  <P>Only privileges matching the schema and table name
  criteria are returned.  They are ordered by TABLE_SCHEM,
  TABLE_NAME, and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @return <code>ResultSet</code> - each row is a table privilege description
  @see #getSearchStringEscape
}
function TZAbstractDatabaseMetadata.GetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-table-privileges:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a table's optimal set of columns that
  uniquely identifies a row. They are ordered by SCOPE.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => actual scope of result
       <UL>
       <LI> bestRowTemporary - very temporary, while using row
       <LI> bestRowTransaction - valid for remainder of current transaction
       <LI> bestRowSession - valid for remainder of current session
       </UL>
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => not used
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> bestRowUnknown - may or may not be pseudo column
       <LI> bestRowNotPseudo - is NOT a pseudo column
       <LI> bestRowPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param scope the scope of interest; use same values as SCOPE
  @param nullable include columns that are nullable?
  @return <code>ResultSet</code> - each row is a column description
}
function TZAbstractDatabaseMetadata.GetBestRowIdentifier(const Catalog: string;
  const Schema: string; const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet;
var
  Key: string;
  IndexName: string;
  ColumnNames: TStrings;
begin
  Key := Format('get-best-row-identifier:%s:%s:%s:%d:%s',
    [Catalog, Schema, Table, Scope, BoolToStr(Nullable)]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(BestRowIdentColumnsDynArray);
    ColumnNames := TStringList.Create;
    try
      { Tries primary keys. }
      with GetPrimaryKeys(Catalog, Schema, Table) do
      begin
        while Next do
          ColumnNames.Add(GetStringByName('COLUMN_NAME'));
        Close;
      end;

      { Tries unique indices. }
      if ColumnNames.Count = 0 then
      begin
        with GetIndexInfo(Catalog, Schema, Table, True, False) do
        begin
          IndexName := '';
          while Next do
          begin
            if IndexName = '' then
              IndexName := GetStringByName('INDEX_NAME');
            if GetStringByName('INDEX_NAME') = IndexName then
              ColumnNames.Add(GetStringByName('COLUMN_NAME'));
          end;
          Close;
        end;
      end;

      with GetColumns(Catalog, Schema, Table, '') do
      begin
        while Next do
        begin
          if (ColumnNames.Count <> 0) and (ColumnNames.IndexOf(
            GetStringByName('COLUMN_NAME')) < 0) then
            Continue;
          if (ColumnNames.Count = 0)
            and (TZSQLType(GetIntByName('DATA_TYPE')) in
            [stBytes, stBinaryStream, stAsciiStream, stUnicodeStream]) then
            Continue;

          Result.MoveToInsertRow;
          Result.UpdateInt(1, Ord(sbrSession));
          Result.UpdateString(2, GetStringByName('COLUMN_NAME'));
          Result.UpdateInt(3, GetIntByName('DATA_TYPE'));
          Result.UpdateString(4, GetStringByName('TYPE_NAME'));
          Result.UpdateInt(5, GetIntByName('COLUMN_SIZE'));
          Result.UpdateInt(6, GetIntByName('BUFFER_LENGTH'));
          Result.UpdateInt(7, GetIntByName('DECIMAL_DIGITS'));
          Result.UpdateInt(8, Ord(brNotPseudo));
          Result.InsertRow;
        end;
        Close;
      end;
    finally
      ColumnNames.Free;
    end;

    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a table's columns that are automatically
  updated when any value in a row is updated.  They are
  unordered.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => is not used
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => length of column value in bytes
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> versionColumnUnknown - may or may not be pseudo column
       <LI> versionColumnNotPseudo - is NOT a pseudo column
       <LI> versionColumnPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a column description
  @exception SQLException if a database access error occurs
}
function TZAbstractDatabaseMetadata.GetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-version-columns:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColVerColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a table's primary key columns.  They
  are ordered by COLUMN_NAME.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>KEY_SEQ</B> short => sequence number within primary key
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @exception SQLException if a database access error occurs
}
function TZAbstractDatabaseMetadata.GetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-primary-keys:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(PrimaryKeyColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of the primary key columns that are
  referenced by a table's foreign key columns (the primary keys
  imported by a table).  They are ordered by PKTABLE_CAT,
  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog
       being imported (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema
       being imported (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
       being imported
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
       being imported
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @see #getExportedKeys
}
function TZAbstractDatabaseMetadata.GetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-imported-keys:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ImportedKeyColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of the foreign key columns that reference a
  table's primary key columns (the foreign keys exported by a
  table).  They are ordered by FKTABLE_CAT, FKTABLE_SCHEM,
  FKTABLE_NAME, and KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZAbstractDatabaseMetadata.GetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-exported-keys:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ExportedKeyColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of the foreign key columns in the foreign key
  table that reference the primary key columns of the primary key
  table (describe how one table imports another's key.) This
  should normally return a single foreign key/primary key pair
  (most tables only import a foreign key from a table once.)  They
  are ordered by FKTABLE_CAT, FKTABLE_SCHEM, FKTABLE_NAME, and
  KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param primaryCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param primarySchema a schema name; "" retrieves those
  without a schema
  @param primaryTable the table name that exports the key
  @param foreignCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param foreignSchema a schema name; "" retrieves those
  without a schema
  @param foreignTable the table name that imports the key
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZAbstractDatabaseMetadata.GetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-cross-reference:%s:%s:%s:%s:%s:%s',
    [PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog,
    ForeignSchema, ForeignTable]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(CrossRefColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of all the standard SQL types supported by
  this database. They are ordered by DATA_TYPE and then by how
  closely the data type maps to the corresponding JDBC SQL type.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_NAME</B> String => Type name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>PRECISION</B> int => maximum precision
 	<LI><B>LITERAL_PREFIX</B> String => prefix used to quote a literal
       (may be null)
 	<LI><B>LITERAL_SUFFIX</B> String => suffix used to quote a literal
        (may be null)
 	<LI><B>CREATE_PARAMS</B> String => parameters used in creating
       the type (may be null)
 	<LI><B>NULLABLE</B> short => can you use NULL for this type?
       <UL>
       <LI> typeNoNulls - does not allow NULL values
       <LI> typeNullable - allows NULL values
       <LI> typeNullableUnknown - nullability unknown
       </UL>
 	<LI><B>CASE_SENSITIVE</B> boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> boolean => can it be used for an
       auto-increment value?
 	<LI><B>LOCAL_TYPE_NAME</B> String => localized version of type name
       (may be null)
 	<LI><B>MINIMUM_SCALE</B> short => minimum scale supported
 	<LI><B>MAXIMUM_SCALE</B> short => maximum scale supported
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>NUM_PREC_RADIX</B> int => usually 2 or 10
   </OL>

  @return <code>ResultSet</code> - each row is an SQL type description
}
function TZAbstractDatabaseMetadata.GetTypeInfo: IZResultSet;
var
  Key: string;
begin
  Key := 'get-type-info';

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>NON_UNIQUE</B> boolean => Can index values be non-unique?
       false when TYPE is tableIndexStatistic
 	<LI><B>INDEX_QUALIFIER</B> String => index catalog (may be null);
       null when TYPE is tableIndexStatistic
 	<LI><B>INDEX_NAME</B> String => index name; null when TYPE is
       tableIndexStatistic
 	<LI><B>TYPE</B> short => index type:
       <UL>
       <LI> tableIndexStatistic - this identifies table statistics that are
            returned in conjuction with a table's index descriptions
       <LI> tableIndexClustered - this is a clustered index
       <LI> tableIndexHashed - this is a hashed index
       <LI> tableIndexOther - this is some other style of index
       </UL>
 	<LI><B>ORDINAL_POSITION</B> short => column sequence number
       within index; zero when TYPE is tableIndexStatistic
 	<LI><B>COLUMN_NAME</B> String => column name; null when TYPE is
       tableIndexStatistic
 	<LI><B>ASC_OR_DESC</B> String => column sort sequence, "A" => ascending,
       "D" => descending, may be null if sort sequence is not supported;
       null when TYPE is tableIndexStatistic
 	<LI><B>CARDINALITY</B> int => When TYPE is tableIndexStatistic, then
       this is the number of rows in the table; otherwise, it is the
       number of unique values in the index.
 	<LI><B>PAGES</B> int => When TYPE is  tableIndexStatisic then
       this is the number of pages used for the table, otherwise it
       is the number of pages used for the current index.
 	<LI><B>FILTER_CONDITION</B> String => Filter condition, if any.
       (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param unique when true, return only indices for unique values;
      when false, return indices regardless of whether unique or not
  @param approximate when true, result is allowed to reflect approximate
      or out of data values; when false, results are requested to be
      accurate
  @return <code>ResultSet</code> - each row is an index column description
}
function TZAbstractDatabaseMetadata.GetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-index-info:%s:%s:%s:%s:%s',
    [Catalog, Schema, Table, BoolToStr(Unique), BoolToStr(Approximate)]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.GetSequences(const Catalog, SchemaPattern,
  SequenceNamePattern: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-sequences:%s:%s:%s',
    [Catalog, SchemaPattern, SequenceNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(SequenceColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsResultSetType(
  _Type: TZResultSetType): Boolean;
begin
  Result := True;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseMetadata.SupportsResultSetConcurrency(
  _Type: TZResultSetType; Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := True;
end;

{**
  Indicates whether the driver supports batch updates.
  @return true if the driver supports batch updates; false otherwise
}
function TZAbstractDatabaseMetadata.SupportsBatchUpdates: Boolean;
begin
  Result := True;
end;

{**

  Gets a description of the user-defined types defined in a particular
  schema.  Schema-specific UDTs may have type JAVA_OBJECT, STRUCT,
  or DISTINCT.

  <P>Only types matching the catalog, schema, type name and type
  criteria are returned.  They are ordered by DATA_TYPE, TYPE_SCHEM
  and TYPE_NAME.  The type name parameter may be a fully-qualified
  name.  In this case, the catalog and schemaPattern parameters are
  ignored.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_CAT</B> String => the type's catalog (may be null)
 	<LI><B>TYPE_SCHEM</B> String => type's schema (may be null)
 	<LI><B>TYPE_NAME</B> String => type name
   <LI><B>CLASS_NAME</B> String => Java class name
 	<LI><B>DATA_TYPE</B> String => type value defined in java.sql.Types.
   One of JAVA_OBJECT, STRUCT, or DISTINCT
 	<LI><B>REMARKS</B> String => explanatory comment on the type
   </OL>

  <P><B>Note:</B> If the driver does not support UDTs, an empty
  result set is returned.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param typeNamePattern a type name pattern; may be a fully-qualified name
  @param types a list of user-named types to include (JAVA_OBJECT,
  STRUCT, or DISTINCT); null returns all types
  @return <code>ResultSet</code> - each row is a type description
}
function TZAbstractDatabaseMetadata.GetUDTs(const Catalog: string;
  const SchemaPattern: string; const TypeNamePattern: string;
  const Types: TIntegerDynArray): IZResultSet;
var
  I: Integer;
  Key: string;
begin
  Key := '';
  for I := Low(Types) to High(Types) do
    Key := Key + ':' + IntToStr(Types[I]);
  Key := Format('get-udts:%s:%s:%s%s',
    [Catalog, SchemaPattern, TypeNamePattern, Key]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(UDTColumnsDynArray);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Creates ab identifier convertor object.
  @returns an identifier convertor object.
}
function TZAbstractDatabaseMetadata.GetIdentifierConvertor:
  IZIdentifierConvertor;
begin
  Result := TZDefaultIdentifierConvertor.Create(Self);
end;

{ TZVirtualResultSet }

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
}
constructor TZVirtualResultSet.CreateWithStatement(const SQL: string;
   Statement: IZStatement);
begin
  inherited CreateWithStatement(SQL, Statement);
end;

{**
  Creates this object and assignes the main properties.
  @param ColumnsInfo a columns info for cached rows.
  @param SQL an SQL query string.
}
constructor TZVirtualResultSet.CreateWithColumns(ColumnsInfo: TObjectList;
  const SQL: string);
begin
  inherited CreateWithColumns(ColumnsInfo, SQL);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZVirtualResultSet.Destroy;
begin
  inherited Destroy;
end;

{**
  Calculates column default values..
  @param RowAccessor a row accessor which contains new column values.
}
procedure TZVirtualResultSet.CalculateRowDefaults(RowAccessor: TZRowAccessor);
begin
end;

{**
  Post changes to database server.
  @param OldRowAccessor a row accessor which contains old column values.
  @param NewRowAccessor a row accessor which contains new or updated
    column values.
}
procedure TZVirtualResultSet.PostRowUpdates(OldRowAccessor,
  NewRowAccessor: TZRowAccessor);
begin
end;

{ TZDefaultIdentifierConvertor }

{**
  Constructs this default identifier convertor object.
  @param Metadata a database metadata interface.
}
constructor TZDefaultIdentifierConvertor.Create(
  Metadata: IZDatabaseMetadata);
begin
  inherited Create;
  FMetadata := Metadata;
end;

{**
  Checks is the specified string in lower case.
  @param an identifier string.
  @return <code>True</code> is the identifier string in lower case.
}
function TZDefaultIdentifierConvertor.IsLowerCase(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Value) do
  begin
    if not (Value[I] in ['a'..'z','0'..'9','_']) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

{**
  Checks is the specified string in upper case.
  @param an identifier string.
  @return <code>True</code> is the identifier string in upper case.
}
function TZDefaultIdentifierConvertor.IsUpperCase(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Value) do
  begin
    if not (Value[I] in ['A'..'Z','0'..'9','_']) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

{**
  Checks is the specified string in special case.
  @param an identifier string.
  @return <code>True</code> is the identifier string in mixed case.
}
function TZDefaultIdentifierConvertor.IsSpecialCase(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Value[1] in ['0'..'9']) then
  begin
    Result := True;
    exit;
  end;
  for I := 1 to Length(Value) do
  begin
    if not (Value[I] in ['A'..'Z','a'..'z','0'..'9','_']) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{**
  Checks is the string case sensitive.
  @return <code>True</code> if the string case sensitive.
}
function TZDefaultIdentifierConvertor.IsCaseSensitive(const Value: string): Boolean;
const
  AnsiSQLKeywords = 'insert,update,delete,select,drop,create,from,set,values,'
    + 'where,order,group,by,having,into,as,table,index,primary,key,on,is,null,'
    + 'char,varchar,integer,number';
var
  Keywords: string;
begin
  if Value = '' then
    Result := False
  else if IsSpecialCase(Value) then
    Result := True
  else if IsLowerCase(Value) then
    Result := not Metadata.StoresLowerCaseIdentifiers
  else if IsUpperCase(Value) then
    Result := not Metadata.StoresUpperCaseIdentifiers
  else
    Result := not Metadata.StoresMixedCaseIdentifiers;

  { Checks for reserved keywords. }
  if not Result then
  begin
    Keywords := ',' + AnsiSQLKeywords + ','
      + LowerCase(Metadata.GetSQLKeywords) + ',';
    Result := Pos(',' + LowerCase(Value) + ',', Keywords) > 0;
  end;
end;

{**
  Checks is the string quoted.
  @return <code>True</code> is the string quoted.
}
function TZDefaultIdentifierConvertor.IsQuoted(const Value: string): Boolean;
var
  QuoteDelim: string;
begin
  QuoteDelim := Metadata.GetIdentifierQuoteString;
  Result := (QuoteDelim <> '') and (Value <> '') and (QuoteDelim[1] = Value[1]);
end;

{**
  Extracts the quote from the idenfitier string.
  @param an identifier string.
  @return a extracted and processed string.
}
function TZDefaultIdentifierConvertor.ExtractQuote(const Value: string): string;
begin
  if IsQuoted(Value) then
  begin
    Result := Copy(Value, 2, Length(Value) - 2);
    if not Metadata.StoresMixedCaseQuotedIdentifiers then
    begin
      if Metadata.StoresLowerCaseQuotedIdentifiers then
        Result := LowerCase(Result)
      else if Metadata.StoresUpperCaseQuotedIdentifiers then
        Result := UpperCase(Result);
    end;
  end
  else
  begin
    Result := Value;
    if not Metadata.StoresMixedCaseIdentifiers then
    begin
      if Metadata.StoresLowerCaseIdentifiers then
        Result := LowerCase(Result)
      else if Metadata.StoresUpperCaseIdentifiers then
        Result := UpperCase(Result);
    end;
  end;
end;

{**
  Quotes the identifier string.
  @param an identifier string.
  @return a quoted string.
}
function TZDefaultIdentifierConvertor.Quote(const Value: string): string;
var
  QuoteDelim: string;
begin
  Result := Value;
  QuoteDelim := Metadata.GetIdentifierQuoteString;
  if Length(QuoteDelim) > 1 then
    Result := QuoteDelim[1] + Result + QuoteDelim[2]
  else if Length(QuoteDelim) = 1 then
    Result := QuoteDelim[1] + Result + QuoteDelim[1];
end;

{**
  rerurns cache key for get tables metadata entry
  @param Catalog catalog name
  @param SchemaPattern schema pattern
  @param TableNamePattern table name pattern
  @param Types table types
  @return the cache key string
}
function GetTablesMetaDataCacheKey(Const Catalog:String;
      Const SchemaPattern:String;	Const TableNamePattern:String;const Types: TStringDynArray):String;
Var I : Integer;
    Key :  String;
begin
  for I := Low(Types) to High(Types) do
    Key := Key + ':' + Types[I];

  Result:= Format('get-tables:%s:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern, Key]);
end;


const
  ProceduresColumnCount = 8;
  ProceduresColumns: array[1..ProceduresColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'PROCEDURE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_NAME'; SQLType: stString; Length: 255),
    (Name: 'RESERVED1'; SQLType: stString; Length: 255),
    (Name: 'RESERVED2'; SQLType: stString; Length: 255),
    (Name: 'RESERVED3'; SQLType: stString; Length: 255),
    (Name: 'REMARKS'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_TYPE'; SQLType: stShort; Length: 0)
  );

  ProceduresColColumnCount = 13;
  ProceduresColColumns: array[1..ProceduresColColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'PROCEDURE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_TYPE'; SQLType: stShort; Length: 0),
    (Name: 'DATA_TYPE'; SQLType: stShort; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PRECISION'; SQLType: stInteger; Length: 0),
    (Name: 'LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'SCALE'; SQLType: stShort; Length: 0),
    (Name: 'RADIX'; SQLType: stShort; Length: 0),
    (Name: 'NULLABLE'; SQLType: stShort; Length: 0),
    (Name: 'REMARKS'; SQLType: stString; Length: 255)
  );

  TableColumnCount = 5;
  TableColumns: array[1..TableColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'TABLE_TYPE'; SQLType: stString; Length: 255),
    (Name: 'REMARKS'; SQLType: stString; Length: 255)
  );

  SchemaColumnCount = 1;
  SchemaColumns: array[1..SchemaColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255)
  );

  CatalogColumnCount = 1;
  CatalogColumns: array[1..CatalogColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255)
  );

  TableTypeColumnCount = 1;
  TableTypeColumns: array[1..TableTypeColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_TYPE'; SQLType: stString; Length: 255)
  );

  TableColColumnCount = 24;
  TableColColumns: array[1..TableColColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stShort; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_SIZE'; SQLType: stInteger; Length: 0),
    (Name: 'BUFFER_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'DECIMAL_DIGITS'; SQLType: stInteger; Length: 0),
    (Name: 'NUM_PREC_RADIX'; SQLType: stInteger; Length: 0),
    (Name: 'NULLABLE'; SQLType: stInteger; Length: 0),
    (Name: 'REMARKS'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_DEF'; SQLType: stString; Length: 255),
    (Name: 'SQL_DATA_TYPE'; SQLType: stInteger; Length: 0),
    (Name: 'SQL_DATETIME_SUB'; SQLType: stInteger; Length: 0),
    (Name: 'CHAR_OCTET_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'ORDINAL_POSITION'; SQLType: stInteger; Length: 0),
    (Name: 'IS_NULLABLE'; SQLType: stString; Length: 255),
    (Name: 'AUTO_INCREMENT'; SQLType: stBoolean; Length: 0),
    (Name: 'CASE_SENSITIVE'; SQLType: stBoolean; Length: 0),
    (Name: 'SEARCHABLE'; SQLType: stBoolean; Length: 0),
    (Name: 'WRITABLE'; SQLType: stBoolean; Length: 0),
    (Name: 'DEFINITELYWRITABLE'; SQLType: stBoolean; Length: 0),
    (Name: 'READONLY'; SQLType: stBoolean; Length: 0)
  );

  TableColPrivColumnCount = 8;
  TableColPrivColumns: array[1..TableColPrivColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'GRANTOR'; SQLType: stString; Length: 255),
    (Name: 'GRANTEE'; SQLType: stString; Length: 255),
    (Name: 'PRIVILEGE'; SQLType: stString; Length: 255),
    (Name: 'IS_GRANTABLE'; SQLType: stString; Length: 255)
  );

  TablePrivColumnCount = 7;
  TablePrivColumns: array[1..TablePrivColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'GRANTOR'; SQLType: stString; Length: 255),
    (Name: 'GRANTEE'; SQLType: stString; Length: 255),
    (Name: 'PRIVILEGE'; SQLType: stString; Length: 255),
    (Name: 'IS_GRANTABLE'; SQLType: stString; Length: 255)
  );

  BestRowIdentColumnCount = 8;
  BestRowIdentColumns: array[1..BestRowIdentColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'SCOPE'; SQLType: stShort; Length: 0),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stShort; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_SIZE'; SQLType: stInteger; Length: 0),
    (Name: 'BUFFER_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'DECIMAL_DIGITS'; SQLType: stShort; Length: 0),
    (Name: 'PSEUDO_COLUMN'; SQLType: stShort; Length: 0)
  );

  TableColVerColumnCount = 8;
  TableColVerColumns: array[1..TableColVerColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'SCOPE'; SQLType: stShort; Length: 0),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stShort; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_SIZE'; SQLType: stInteger; Length: 0),
    (Name: 'BUFFER_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'DECIMAL_DIGITS'; SQLType: stShort; Length: 0),
    (Name: 'PSEUDO_COLUMN'; SQLType: stShort; Length: 0)
  );

  PrimaryKeyColumnCount = 6;
  PrimaryKeyColumns: array[1..PrimaryKeyColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stShort; Length: 0),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255)
  );

  ImportedKeyColumnCount = 14;
  ImportedKeyColumns: array[1..ImportedKeyColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'PKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stShort; Length: 0),
    (Name: 'UPDATE_RULE'; SQLType: stShort; Length: 0),
    (Name: 'DELETE_RULE'; SQLType: stShort; Length: 0),
    (Name: 'FK_NAME'; SQLType: stString; Length: 255),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255),
    (Name: 'DEFERRABILITY'; SQLType: stShort; Length: 0)
  );

  ExportedKeyColumnCount = 14;
  ExportedKeyColumns: array[1..ExportedKeyColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'PKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stShort; Length: 0),
    (Name: 'UPDATE_RULE'; SQLType: stShort; Length: 0),
    (Name: 'DELETE_RULE'; SQLType: stShort; Length: 0),
    (Name: 'FK_NAME'; SQLType: stString; Length: 255),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255),
    (Name: 'DEFERRABILITY'; SQLType: stShort; Length: 0)
  );

  CrossRefColumnCount = 14;
  CrossRefColumns: array[1..CrossRefColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'PKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stShort; Length: 0),
    (Name: 'UPDATE_RULE'; SQLType: stShort; Length: 0),
    (Name: 'DELETE_RULE'; SQLType: stShort; Length: 0),
    (Name: 'FK_NAME'; SQLType: stString; Length: 255),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255),
    (Name: 'DEFERRABILITY'; SQLType: stShort; Length: 0)
  );

  TypeInfoColumnCount = 18;
  TypeInfoColumns: array[1..TypeInfoColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stShort; Length: 0),
    (Name: 'PRECISION'; SQLType: stInteger; Length: 0),
    (Name: 'LITERAL_PREFIX'; SQLType: stString; Length: 255),
    (Name: 'LITERAL_SUFFIX'; SQLType: stString; Length: 255),
    (Name: 'CREATE_PARAMS'; SQLType: stString; Length: 255),
    (Name: 'NULLABLE'; SQLType: stShort; Length: 0),
    (Name: 'CASE_SENSITIVE'; SQLType: stBoolean; Length: 0),
    (Name: 'SEARCHABLE'; SQLType: stShort; Length: 0),
    (Name: 'UNSIGNED_ATTRIBUTE'; SQLType: stBoolean; Length: 0),
    (Name: 'FIXED_PREC_SCALE'; SQLType: stBoolean; Length: 0),
    (Name: 'AUTO_INCREMENT'; SQLType: stBoolean; Length: 0),
    (Name: 'LOCAL_TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'MINIMUM_SCALE'; SQLType: stShort; Length: 0),
    (Name: 'MAXIMUM_SCALE'; SQLType: stShort; Length: 0),
    (Name: 'SQL_DATA_TYPE'; SQLType: stInteger; Length: 0),
    (Name: 'SQL_DATETIME_SUB'; SQLType: stInteger; Length: 0),
    (Name: 'NUM_PREC_RADIX'; SQLType: stInteger; Length: 0)
  );

  IndexInfoColumnCount = 13;
  IndexInfoColumns: array[1..IndexInfoColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'NON_UNIQUE'; SQLType: stBoolean; Length: 0),
    (Name: 'INDEX_QUALIFIER'; SQLType: stString; Length: 255),
    (Name: 'INDEX_NAME'; SQLType: stString; Length: 255),
    (Name: 'TYPE'; SQLType: stShort; Length: 0),
    (Name: 'ORDINAL_POSITION'; SQLType: stShort; Length: 0),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'ASC_OR_DESC'; SQLType: stString; Length: 255),
    (Name: 'CARDINALITY'; SQLType: stInteger; Length: 0),
    (Name: 'PAGES'; SQLType: stInteger; Length: 0),
    (Name: 'FILTER_CONDITION'; SQLType: stString; Length: 255)
  );

  SequenceColumnCount = 3;
  SequenceColumns: array[1..SequenceColumnCount]
    of TZMetadataColumnDef = (
    (Name: 'SEQUENCE_CAT'; SQLType: stString; Length: 255),
    (Name: 'SEQUENCE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'SEQUENCE_NAME'; SQLType: stString; Length: 255)
  );

  UDTColumnCount = 6;
  UDTColumns: array[1..UDTColumnCount]
    of TZMetadataColumnDef =(
    (Name: 'TYPE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TYPE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'CLASS_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stShort; Length: 0),
    (Name: 'REMARKS'; SQLType: stString; Length: 255)
  );

var
  I: Integer;

initialization
  SetLength(ProceduresColumnsDynArray, ProceduresColumnCount);
  for I := 1 to ProceduresColumnCount do
    ProceduresColumnsDynArray[I - 1] := ProceduresColumns[I];

  SetLength(ProceduresColColumnsDynArray, ProceduresColColumnCount);
  for I := 1 to ProceduresColColumnCount do
    ProceduresColColumnsDynArray[I - 1] := ProceduresColColumns[I];

  SetLength(TableColumnsDynArray, TableColumnCount);
  for I := 1 to TableColumnCount do
    TableColumnsDynArray[I - 1] := TableColumns[I];

  SetLength(SchemaColumnsDynArray, SchemaColumnCount);
  for I := 1 to SchemaColumnCount do
    SchemaColumnsDynArray[I - 1] := SchemaColumns[I];

  SetLength(CatalogColumnsDynArray, CatalogColumnCount);
  for I := 1 to CatalogColumnCount do
    CatalogColumnsDynArray[I - 1] := CatalogColumns[I];

  SetLength(TableTypeColumnsDynArray, TableTypeColumnCount);
  for I := 1 to TableTypeColumnCount do
    TableTypeColumnsDynArray[I - 1] := TableTypeColumns[I];

  SetLength(TableColColumnsDynArray, TableColColumnCount);
  for I := 1 to TableColColumnCount do
    TableColColumnsDynArray[I - 1] := TableColColumns[I];

  SetLength(TableColPrivColumnsDynArray, TableColPrivColumnCount);
  for I := 1 to TableColPrivColumnCount do
    TableColPrivColumnsDynArray[I - 1] := TableColPrivColumns[I];

  SetLength(TablePrivColumnsDynArray, TablePrivColumnCount);
  for I := 1 to TablePrivColumnCount do
    TablePrivColumnsDynArray[I - 1] := TablePrivColumns[I];

  SetLength(BestRowIdentColumnsDynArray, BestRowIdentColumnCount);
  for I := 1 to BestRowIdentColumnCount do
    BestRowIdentColumnsDynArray[I - 1] := BestRowIdentColumns[I];

  SetLength(TableColVerColumnsDynArray, TableColVerColumnCount);
  for I := 1 to TableColVerColumnCount do
    TableColVerColumnsDynArray[I - 1] := TableColVerColumns[I];

  SetLength(PrimaryKeyColumnsDynArray, PrimaryKeyColumnCount);
  for I := 1 to PrimaryKeyColumnCount do
    PrimaryKeyColumnsDynArray[I - 1] := PrimaryKeyColumns[I];

  SetLength(ImportedKeyColumnsDynArray, ImportedKeyColumnCount);
  for I := 1 to ImportedKeyColumnCount do
    ImportedKeyColumnsDynArray[I - 1] := ImportedKeyColumns[I];

  SetLength(ExportedKeyColumnsDynArray, ExportedKeyColumnCount);
  for I := 1 to ExportedKeyColumnCount do
    ExportedKeyColumnsDynArray[I - 1] := ExportedKeyColumns[I];

  SetLength(CrossRefColumnsDynArray, CrossRefColumnCount);
  for I := 1 to CrossRefColumnCount do
    CrossRefColumnsDynArray[I - 1] := CrossRefColumns[I];

  SetLength(TypeInfoColumnsDynArray, TypeInfoColumnCount);
  for I := 1 to TypeInfoColumnCount do
    TypeInfoColumnsDynArray[I - 1] := TypeInfoColumns[I];

  SetLength(IndexInfoColumnsDynArray, IndexInfoColumnCount);
  for I := 1 to IndexInfoColumnCount do
    IndexInfoColumnsDynArray[I - 1] := IndexInfoColumns[I];

  SetLength(SequenceColumnsDynArray, SequenceColumnCount);
  for I := 1 to SequenceColumnCount do
    SequenceColumnsDynArray[I - 1] := SequenceColumns[I];

  SetLength(UDTColumnsDynArray, UDTColumnCount);
  for I := 1 to UDTColumnCount do
    UDTColumnsDynArray[I - 1] := UDTColumns[I];
end.
