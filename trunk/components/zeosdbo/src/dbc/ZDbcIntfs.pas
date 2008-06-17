{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Database Connectivity Interfaces              }
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

unit ZDbcIntfs;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, SysUtils, ZClasses, ZCollections, ZSysUtils, ZCompatibility,
  ZTokenizer, ZSelectSchema, ZGenericSqlAnalyser, ZDbcLogging, ZVariant;

const
  { Constatnts from JDBC DatabaseMetadata }
  TypeSearchable           = 3;
  ProcedureReturnsResult   = 2;

// Exceptions
type

  {** Abstract SQL exception. }
  EZSQLThrowable = class(Exception)
  private
    FErrorCode: Integer;
    FStatusCode: String;
  public
    constructor Create(const Msg: string);
    constructor CreateWithCode(const ErrorCode: Integer; const Msg: string);
    constructor CreateWithStatus(const StatusCode: String; const Msg: string);
    constructor CreateClone(const E:EZSQLThrowable);

    property ErrorCode: Integer read FErrorCode;
    property StatusCode: string read FStatuscode; // The "String" Errocode // FirmOS
  end;

  {** Generic SQL exception. }
  EZSQLException = class(EZSQLThrowable);

  {** Generic SQL warning. }
  EZSQLWarning = class(EZSQLThrowable);

// Data types
type
  {** Defines supported SQL types. }
  TZSQLType = (stUnknown, stBoolean, stByte, stShort, stInteger, stLong, stFloat,
    stDouble, stBigDecimal, stString, stUnicodeString, stBytes, stDate, stTime,
    stTimestamp, stAsciiStream, stUnicodeStream, stBinaryStream);

  {** Defines a transaction isolation level. }
  TZTransactIsolationLevel = (tiNone, tiReadUncommitted, tiReadCommitted,
    tiRepeatableRead, tiSerializable);

  {** Defines a resultset fetch direction. }
  TZFetchDirection = (fdForward, fdReverse, fdUnknown);

  {** Defines a type of result set. }
  TZResultSetType = (rtForwardOnly, rtScrollInsensitive, rtScrollSensitive);

  {** Defines a result set concurrency type. }
  TZResultSetConcurrency = (rcReadOnly, rcUpdatable);

  {** Defines a nullable type for the column. }
  TZColumnNullableType = (ntNoNulls, ntNullable, ntNullableUnknown);

  {** Defines a result type for the procedures. }
  TZProcedureResultType = (prtUnknown, prtNoResult, prtReturnsResult);

  {** Defines a column type for the procedures. }
  TZProcedureColumnType = (pctUnknown, pctIn, pctInOut, pctOut, pctReturn,
    pctResultSet);

  {** Defines a best row identifier. }
  TZBestRowIdentifier = (brUnknown, brNotPseudo, brPseudo);

  {** Defines a scope best row identifier. }
  TZScopeBestRowIdentifier = (sbrTemporary, sbrTransaction, sbrSession);

  {** Defines a version column. }
  TZVersionColumn = (vcUnknown, vcNotPseudo, vcPseudo);

  {**  }
  TZImportedKey = (ikCascade, ikRestrict, ikSetNull, ikNoAction, ikSetDefault,
    ikInitiallyDeferred, ikInitiallyImmediate, ikNotDeferrable);

  TZTableIndex = (tiStatistic, tiClustered, tiHashed, tiOther);

  {** Defines a post update mode. }
  TZPostUpdatesMode = (poColumnsAll, poColumnsChanged);

  {** Defines a locate mode. }
  TZLocateUpdatesMode = (loWhereAll, loWhereChanged, loWhereKeyOnly);

// Interfaces
type

  // Forward declarations
  IZDriverManager = interface;
  IZDriver = interface;
  IZConnection = interface;
  IZDatabaseMetadata = interface;
  IZStatement = interface;
  IZPreparedStatement = interface;
  IZCallableStatement = interface;
  IZResultSet = interface;
  IZResultSetMetadata = interface;
  IZBlob = interface;
  IZNotification = interface;
  IZSequence = interface;

  {** Driver Manager interface. }
  IZDriverManager = interface(IZInterface)
    ['{8874B9AA-068A-4C0C-AE75-9DB1EA9E3720}']

    function GetConnection(const Url: string): IZConnection;
    function GetConnectionWithParams(const Url: string; Info: TStrings): IZConnection;
    function GetConnectionWithLogin(const Url: string; const User: string;
      const Password: string): IZConnection;

    function GetDriver(const Url: string): IZDriver;
    procedure RegisterDriver(Driver: IZDriver);
    procedure DeregisterDriver(Driver: IZDriver);

    function GetDrivers: IZCollection;

    function GetLoginTimeout: Integer;
    procedure SetLoginTimeout(Seconds: Integer);

    procedure AddLoggingListener(Listener: IZLoggingListener);
    procedure RemoveLoggingListener(Listener: IZLoggingListener);

    procedure LogMessage(Category: TZLoggingCategory; const Protocol: string;
      const Msg: WideString);
    procedure LogError(Category: TZLoggingCategory; const Protocol: string;
      const Msg: WideString; ErrorCode: Integer; const Error: WideString);
  end;

  {** Database Driver interface. }
  IZDriver = interface(IZInterface)
    ['{2157710E-FBD8-417C-8541-753B585332E2}']

    function GetSupportedProtocols: TStringDynArray;
    function Connect(const Url: string; Info: TStrings): IZConnection;
    function AcceptsURL(const Url: string): Boolean;

    function GetPropertyInfo(const Url: string; Info: TStrings): TStrings;
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetSubVersion: Integer;
    function GetTokenizer: IZTokenizer;
    function GetStatementAnalyser: IZStatementAnalyser;
  end;

  {** Database Connection interface. }
  IZConnection = interface(IZInterface)
    ['{8EEBBD1A-56D1-4EC0-B3BD-42B60591457F}']

    function CreateStatement: IZStatement;
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    function PrepareCall(const SQL: string): IZCallableStatement;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const SQL: string; Info: TStrings):
      IZCallableStatement;

    function CreateNotification(const Event: string): IZNotification;
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;

    function NativeSQL(const SQL: string): string;

    procedure SetAutoCommit(Value: Boolean);
    function GetAutoCommit: Boolean;

    procedure Commit;
    procedure Rollback;

    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const transactionid: string);
    procedure CommitPrepared(const transactionid: string);
    procedure RollbackPrepared(const transactionid: string);


    //Ping Server Support (firmos) 27032006

    function PingServer: Integer;


    procedure Open;
    procedure Close;
    function IsClosed: Boolean;
    function Ping: Boolean;
    function GetAffectedRowsFromLastPost: Int64;
    function GetThreadId: Cardinal;
    function GetEscapeString(const Value: string): string;

    function GetDriver: IZDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;
    function GetClientVersion: Integer;
    function GetHostVersion: Integer;

    procedure SetReadOnly(Value: Boolean);
    function IsReadOnly: Boolean;

    procedure SetCatalog(const Value: string);
    function GetCatalog: string;

    procedure SetTransactionIsolation(Value: TZTransactIsolationLevel);
    function GetTransactionIsolation: TZTransactIsolationLevel;

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;
  end;

  {** Database metadata interface. }
  IZDatabaseMetadata = interface(IZInterface)
    ['{FE331C2D-0664-464E-A981-B4F65B85D1A8}']

    function AllProceduresAreCallable: Boolean;
    function AllTablesAreSelectable: Boolean;
    function GetURL: string;
    function GetUserName: string;
    function IsReadOnly: Boolean;
    function NullsAreSortedHigh: Boolean;
    function NullsAreSortedLow: Boolean;
    function NullsAreSortedAtStart: Boolean;
    function NullsAreSortedAtEnd: Boolean;
    function GetDatabaseProductName: string;
    function GetDatabaseProductVersion: string;
    function GetDriverName: string;
    function GetDriverVersion: string;
    function GetDriverMajorVersion: Integer;
    function GetDriverMinorVersion: Integer;
    function UsesLocalFiles: Boolean;
    function UsesLocalFilePerTable: Boolean;
    function SupportsMixedCaseIdentifiers: Boolean;
    function StoresUpperCaseIdentifiers: Boolean;
    function StoresLowerCaseIdentifiers: Boolean;
    function StoresMixedCaseIdentifiers: Boolean;
    function SupportsMixedCaseQuotedIdentifiers: Boolean;
    function StoresUpperCaseQuotedIdentifiers: Boolean;
    function StoresLowerCaseQuotedIdentifiers: Boolean;
    function StoresMixedCaseQuotedIdentifiers: Boolean;
    function GetIdentifierQuoteString: string;
    function GetSQLKeywords: string;
    function GetNumericFunctions: string;
    function GetStringFunctions: string;
    function GetSystemFunctions: string;
    function GetTimeDateFunctions: string;
    function GetSearchStringEscape: string;
    function GetExtraNameCharacters: string;

    function SupportsAlterTableWithAddColumn: Boolean;
    function SupportsAlterTableWithDropColumn: Boolean;
    function SupportsColumnAliasing: Boolean;
    function NullPlusNonNullIsNull: Boolean;
    function SupportsConvert: Boolean;
    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
      Boolean;
    function SupportsTableCorrelationNames: Boolean;
    function SupportsDifferentTableCorrelationNames: Boolean;
    function SupportsExpressionsInOrderBy: Boolean;
    function SupportsOrderByUnrelated: Boolean;
    function SupportsGroupBy: Boolean;
    function SupportsGroupByUnrelated: Boolean;
    function SupportsGroupByBeyondSelect: Boolean;
    function SupportsLikeEscapeClause: Boolean;
    function SupportsMultipleResultSets: Boolean;
    function SupportsMultipleTransactions: Boolean;
    function SupportsNonNullableColumns: Boolean;
    function SupportsMinimumSQLGrammar: Boolean;
    function SupportsCoreSQLGrammar: Boolean;
    function SupportsExtendedSQLGrammar: Boolean;
    function SupportsANSI92EntryLevelSQL: Boolean;
    function SupportsANSI92IntermediateSQL: Boolean;
    function SupportsANSI92FullSQL: Boolean;
    function SupportsIntegrityEnhancementFacility: Boolean;
    function SupportsOuterJoins: Boolean;
    function SupportsFullOuterJoins: Boolean;
    function SupportsLimitedOuterJoins: Boolean;
    function GetSchemaTerm: string;
    function GetProcedureTerm: string;
    function GetCatalogTerm: string;
    function IsCatalogAtStart: Boolean;
    function GetCatalogSeparator: string;
    function SupportsSchemasInDataManipulation: Boolean;
    function SupportsSchemasInProcedureCalls: Boolean;
    function SupportsSchemasInTableDefinitions: Boolean;
    function SupportsSchemasInIndexDefinitions: Boolean;
    function SupportsSchemasInPrivilegeDefinitions: Boolean;
    function SupportsCatalogsInDataManipulation: Boolean;
    function SupportsCatalogsInProcedureCalls: Boolean;
    function SupportsCatalogsInTableDefinitions: Boolean;
    function SupportsCatalogsInIndexDefinitions: Boolean;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean;
    function SupportsPositionedDelete: Boolean;
    function SupportsPositionedUpdate: Boolean;
    function SupportsSelectForUpdate: Boolean;
    function SupportsStoredProcedures: Boolean;
    function SupportsSubqueriesInComparisons: Boolean;
    function SupportsSubqueriesInExists: Boolean;
    function SupportsSubqueriesInIns: Boolean;
    function SupportsSubqueriesInQuantifieds: Boolean;
    function SupportsCorrelatedSubqueries: Boolean;
    function SupportsUnion: Boolean;
    function SupportsUnionAll: Boolean;
    function SupportsOpenCursorsAcrossCommit: Boolean;
    function SupportsOpenCursorsAcrossRollback: Boolean;
    function SupportsOpenStatementsAcrossCommit: Boolean;
    function SupportsOpenStatementsAcrossRollback: Boolean;

    function GetMaxBinaryLiteralLength: Integer;
    function GetMaxCharLiteralLength: Integer;
    function GetMaxColumnNameLength: Integer;
    function GetMaxColumnsInGroupBy: Integer;
    function GetMaxColumnsInIndex: Integer;
    function GetMaxColumnsInOrderBy: Integer;
    function GetMaxColumnsInSelect: Integer;
    function GetMaxColumnsInTable: Integer;
    function GetMaxConnections: Integer;
    function GetMaxCursorNameLength: Integer;
    function GetMaxIndexLength: Integer;
    function GetMaxSchemaNameLength: Integer;
    function GetMaxProcedureNameLength: Integer;
    function GetMaxCatalogNameLength: Integer;
    function GetMaxRowSize: Integer;
    function DoesMaxRowSizeIncludeBlobs: Boolean;
    function GetMaxStatementLength: Integer;
    function GetMaxStatements: Integer;
    function GetMaxTableNameLength: Integer;
    function GetMaxTablesInSelect: Integer;
    function GetMaxUserNameLength: Integer;

    function GetDefaultTransactionIsolation: TZTransactIsolationLevel;
    function SupportsTransactions: Boolean;
    function SupportsTransactionIsolationLevel(Level: TZTransactIsolationLevel):
      Boolean;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
    function SupportsDataManipulationTransactionsOnly: Boolean;
    function DataDefinitionCausesTransactionCommit: Boolean;
    function DataDefinitionIgnoredInTransactions: Boolean;

    function GetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet;
    function GetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string): IZResultSet;

    function GetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet;
    function GetSchemas: IZResultSet;
    function GetCatalogs: IZResultSet;
    function GetTableTypes: IZResultSet;
    function GetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet;
    function GetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet;

    function GetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet;
    function GetBestRowIdentifier(const Catalog: string; const Schema: string;
      const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet;
    function GetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;

    function GetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet;

    function GetTypeInfo: IZResultSet;

    function GetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet;

    function GetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet;

    function SupportsResultSetType(_Type: TZResultSetType): Boolean;
    function SupportsResultSetConcurrency(_Type: TZResultSetType;
      Concurrency: TZResultSetConcurrency): Boolean;
    function SupportsBatchUpdates: Boolean;

    function GetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet;

    function GetConnection: IZConnection;
    function GetIdentifierConvertor: IZIdentifierConvertor;

    procedure ClearCache;overload;
		procedure ClearCache(const Key: string);overload;
  end;

  {** Generic SQL statement interface. }
  IZStatement = interface(IZInterface)
    ['{22CEFA7E-6A6D-48EC-BB9B-EE66056E90F1}']

    function ExecuteQuery(const SQL: WideString): IZResultSet;
    function ExecuteUpdate(const SQL: WideString): Integer;
    procedure Close;

    function GetMaxFieldSize: Integer;
    procedure SetMaxFieldSize(Value: Integer);
    function GetMaxRows: Integer;
    procedure SetMaxRows(Value: Integer);
    procedure SetEscapeProcessing(Value: Boolean);
    function GetQueryTimeout: Integer;
    procedure SetQueryTimeout(Value: Integer);
    procedure Cancel;
    procedure SetCursorName(const Value: string);

    function Execute(const SQL: WideString): Boolean;
    function GetResultSet: IZResultSet;
    function GetUpdateCount: Integer;
    function GetMoreResults: Boolean;

    procedure SetFetchDirection(Value: TZFetchDirection);
    function GetFetchDirection: TZFetchDirection;
    procedure SetFetchSize(Value: Integer);
    function GetFetchSize: Integer;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency);
    function GetResultSetConcurrency: TZResultSetConcurrency;
    procedure SetResultSetType(Value: TZResultSetType);
    function GetResultSetType: TZResultSetType;

    procedure SetPostUpdates(Value: TZPostUpdatesMode);
    function GetPostUpdates: TZPostUpdatesMode;
    procedure SetLocateUpdates(Value: TZLocateUpdatesMode);
    function GetLocateUpdates: TZLocateUpdatesMode;

    procedure AddBatch(const SQL: string);
    procedure ClearBatch;
    function ExecuteBatch: TIntegerDynArray;

    function GetConnection: IZConnection;
    function GetParameters: TStrings;

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;
  end;

  {** Prepared SQL statement interface. }
  IZPreparedStatement = interface(IZStatement)
    ['{990B8477-AF11-4090-8821-5B7AFEA9DD70}']

    function ExecuteQueryPrepared: IZResultSet;
    function ExecuteUpdatePrepared: Integer;
    function ExecutePrepared: Boolean;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string);

    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    procedure SetByte(ParameterIndex: Integer; Value: ShortInt);
    procedure SetShort(ParameterIndex: Integer; Value: SmallInt);
    procedure SetInt(ParameterIndex: Integer; Value: Integer);
    procedure SetLong(ParameterIndex: Integer; Value: Int64);
    procedure SetFloat(ParameterIndex: Integer; Value: Single);
    procedure SetDouble(ParameterIndex: Integer; Value: Double);
    procedure SetBigDecimal(ParameterIndex: Integer; Value: Extended);
    procedure SetPChar(ParameterIndex: Integer; Value: PChar);
    procedure SetString(ParameterIndex: Integer; const Value: string);
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: WideString);
    procedure SetBytes(ParameterIndex: Integer; const Value: TByteDynArray);
    procedure SetDate(ParameterIndex: Integer; Value: TDateTime);
    procedure SetTime(ParameterIndex: Integer; Value: TDateTime);
    procedure SetTimestamp(ParameterIndex: Integer; Value: TDateTime);
    procedure SetAsciiStream(ParameterIndex: Integer; Value: TStream);
    procedure SetUnicodeStream(ParameterIndex: Integer; Value: TStream);
    procedure SetBinaryStream(ParameterIndex: Integer; Value: TStream);
    procedure SetBlob(ParameterIndex: Integer; SQLType: TZSQLType;
      Value: IZBlob);
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant);

    procedure ClearParameters;

    procedure AddBatchPrepared;
    function GetMetadata: IZResultSetMetadata;
  end;

  {** Callable SQL statement interface. }
  IZCallableStatement = interface(IZPreparedStatement)
    ['{E6FA6C18-C764-4C05-8FCB-0582BDD1EF40}']

    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer);
    function WasNull: Boolean;

    function IsNull(ParameterIndex: Integer): Boolean;
    function GetPChar(ParameterIndex: Integer): PChar;
    function GetString(ParameterIndex: Integer): string;
    function GetUnicodeString(ParameterIndex: Integer): WideString;
    function GetBoolean(ParameterIndex: Integer): Boolean;
    function GetByte(ParameterIndex: Integer): ShortInt;
    function GetShort(ParameterIndex: Integer): SmallInt;
    function GetInt(ParameterIndex: Integer): Integer;
    function GetLong(ParameterIndex: Integer): Int64;
    function GetFloat(ParameterIndex: Integer): Single;
    function GetDouble(ParameterIndex: Integer): Double;
    function GetBigDecimal(ParameterIndex: Integer): Extended;
    function GetBytes(ParameterIndex: Integer): TByteDynArray;
    function GetDate(ParameterIndex: Integer): TDateTime;
    function GetTime(ParameterIndex: Integer): TDateTime;
    function GetTimestamp(ParameterIndex: Integer): TDateTime;
    function GetValue(ParameterIndex: Integer): TZVariant;
  end;

  {** Rows returned by SQL query. }
  IZResultSet = interface(IZInterface)
    ['{8F4C4D10-2425-409E-96A9-7142007CC1B2}']

    function Next: Boolean;
    procedure Close;
    function WasNull: Boolean;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPChar(ColumnIndex: Integer): PChar;
    function GetString(ColumnIndex: Integer): string;
    function GetUnicodeString(ColumnIndex: Integer): WideString;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetByte(ColumnIndex: Integer): ShortInt;
    function GetShort(ColumnIndex: Integer): SmallInt;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetBigDecimal(ColumnIndex: Integer): Extended;
    function GetBytes(ColumnIndex: Integer): TByteDynArray;
    function GetDate(ColumnIndex: Integer): TDateTime;
    function GetTime(ColumnIndex: Integer): TDateTime;
    function GetTimestamp(ColumnIndex: Integer): TDateTime;
    function GetAsciiStream(ColumnIndex: Integer): TStream;
    function GetUnicodeStream(ColumnIndex: Integer): TStream;
    function GetBinaryStream(ColumnIndex: Integer): TStream;
    function GetBlob(ColumnIndex: Integer): IZBlob;
    function GetValue(ColumnIndex: Integer): TZVariant;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    function IsNullByName(const ColumnName: string): Boolean;
    function GetPCharByName(const ColumnName: string): PChar;
    function GetStringByName(const ColumnName: string): string;
    function GetUnicodeStringByName(const ColumnName: string): WideString;
    function GetBooleanByName(const ColumnName: string): Boolean;
    function GetByteByName(const ColumnName: string): ShortInt;
    function GetShortByName(const ColumnName: string): SmallInt;
    function GetIntByName(const ColumnName: string): Integer;
    function GetLongByName(const ColumnName: string): Int64;
    function GetFloatByName(const ColumnName: string): Single;
    function GetDoubleByName(const ColumnName: string): Double;
    function GetBigDecimalByName(const ColumnName: string): Extended;
    function GetBytesByName(const ColumnName: string): TByteDynArray;
    function GetDateByName(const ColumnName: string): TDateTime;
    function GetTimeByName(const ColumnName: string): TDateTime;
    function GetTimestampByName(const ColumnName: string): TDateTime;
    function GetAsciiStreamByName(const ColumnName: string): TStream;
    function GetUnicodeStreamByName(const ColumnName: string): TStream;
    function GetBinaryStreamByName(const ColumnName: string): TStream;
    function GetBlobByName(const ColumnName: string): IZBlob;
    function GetValueByName(const ColumnName: string): TZVariant;

    //=====================================================================
    // Advanced features:
    //=====================================================================

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;

    function GetCursorName: string;
    function GetMetadata: IZResultSetMetadata;
    function FindColumn(const ColumnName: string): Integer;
    
    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function IsBeforeFirst: Boolean;
    function IsAfterLast: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    procedure BeforeFirst;
    procedure AfterLast;
    function First: Boolean;
    function Last: Boolean;
    function GetRow: Integer;
    function MoveAbsolute(Row: Integer): Boolean;
    function MoveRelative(Rows: Integer): Boolean;
    function Previous: Boolean;

    //---------------------------------------------------------------------
    // Properties
    //---------------------------------------------------------------------

    procedure SetFetchDirection(Value: TZFetchDirection);
    function GetFetchDirection: TZFetchDirection;

    procedure SetFetchSize(Value: Integer);
    function GetFetchSize: Integer;

    function GetType: TZResultSetType;
    function GetConcurrency: TZResultSetConcurrency;

    function GetPostUpdates: TZPostUpdatesMode;
    function GetLocateUpdates: TZLocateUpdatesMode;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean;
    function RowInserted: Boolean;
    function RowDeleted: Boolean;

    procedure UpdateNull(ColumnIndex: Integer);
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure UpdateByte(ColumnIndex: Integer; Value: ShortInt);
    procedure UpdateShort(ColumnIndex: Integer; Value: SmallInt);
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer);
    procedure UpdateLong(ColumnIndex: Integer; Value: Int64);
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single);
    procedure UpdateDouble(ColumnIndex: Integer; Value: Double);
    procedure UpdateBigDecimal(ColumnIndex: Integer; Value: Extended);
    procedure UpdatePChar(ColumnIndex: Integer; Value: PChar);
    procedure UpdateString(ColumnIndex: Integer; const Value: string);
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: WideString);
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TByteDynArray);
    procedure UpdateDate(ColumnIndex: Integer; Value: TDateTime);
    procedure UpdateTime(ColumnIndex: Integer; Value: TDateTime);
    procedure UpdateTimestamp(ColumnIndex: Integer; Value: TDateTime);
    procedure UpdateAsciiStream(ColumnIndex: Integer; Value: TStream);
    procedure UpdateUnicodeStream(ColumnIndex: Integer; Value: TStream);
    procedure UpdateBinaryStream(ColumnIndex: Integer; Value: TStream);
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant);

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    procedure UpdateNullByName(const ColumnName: string);
    procedure UpdateBooleanByName(const ColumnName: string; Value: Boolean);
    procedure UpdateByteByName(const ColumnName: string; Value: ShortInt);
    procedure UpdateShortByName(const ColumnName: string; Value: SmallInt);
    procedure UpdateIntByName(const ColumnName: string; Value: Integer);
    procedure UpdateLongByName(const ColumnName: string; Value: Int64);
    procedure UpdateFloatByName(const ColumnName: string; Value: Single);
    procedure UpdateDoubleByName(const ColumnName: string; Value: Double);
    procedure UpdateBigDecimalByName(const ColumnName: string; Value: Extended);
    procedure UpdatePCharByName(const ColumnName: string; Value: PChar);
    procedure UpdateStringByName(const ColumnName: string; const Value: string);
    procedure UpdateUnicodeStringByName(const ColumnName: string; const Value: WideString);
    procedure UpdateBytesByName(const ColumnName: string; const Value: TByteDynArray);
    procedure UpdateDateByName(const ColumnName: string; Value: TDateTime);
    procedure UpdateTimeByName(const ColumnName: string; Value: TDateTime);
    procedure UpdateTimestampByName(const ColumnName: string; Value: TDateTime);
    procedure UpdateAsciiStreamByName(const ColumnName: string; Value: TStream);
    procedure UpdateUnicodeStreamByName(const ColumnName: string; Value: TStream);
    procedure UpdateBinaryStreamByName(const ColumnName: string; Value: TStream);
    procedure UpdateValueByName(const ColumnName: string; const Value: TZVariant);

    procedure InsertRow;
    procedure UpdateRow;
    procedure DeleteRow;
    procedure RefreshRow;
    procedure CancelRowUpdates;
    procedure MoveToInsertRow;
    procedure MoveToCurrentRow;
//    procedure MoveToSearchRow;

//    function Search(CaseInsensitive, PartialKey: Boolean): Boolean;
//    function Compare(Row: Integer; CaseInsensitive, PartialKey: Boolean):
//      Boolean;

    function CompareRows(Row1, Row2: Integer; const ColumnIndices: TIntegerDynArray;
      const ColumnDirs: TBooleanDynArray): Integer;

    function GetStatement: IZStatement;
  end;

  {** ResultSet metadata interface. }
  IZResultSetMetadata = interface(IZInterface)
    ['{47CA2144-2EA7-42C4-8444-F5154369B2D7}']

    function GetColumnCount: Integer;
    function IsAutoIncrement(Column: Integer): Boolean;
    function IsCaseSensitive(Column: Integer): Boolean;
    function IsSearchable(Column: Integer): Boolean;
    function IsCurrency(Column: Integer): Boolean;
    function IsNullable(Column: Integer): TZColumnNullableType;

    function IsSigned(Column: Integer): Boolean;
    function GetColumnDisplaySize(Column: Integer): Integer;
    function GetColumnLabel(Column: Integer): string;
    function GetColumnName(Column: Integer): string;
    function GetSchemaName(Column: Integer): string;
    function GetPrecision(Column: Integer): Integer;
    function GetScale(Column: Integer): Integer;
    function GetTableName(Column: Integer): string;
    function GetCatalogName(Column: Integer): string;
    function GetColumnType(Column: Integer): TZSQLType;
    function GetColumnTypeName(Column: Integer): string;
    function IsReadOnly(Column: Integer): Boolean;
    function IsWritable(Column: Integer): Boolean;
    function IsDefinitelyWritable(Column: Integer): Boolean;
    function GetDefaultValue(Column: Integer): string;
    function HasDefaultValue(Column: Integer): Boolean;
  end;

  {** External or internal blob wrapper object. }
  IZBlob = interface(IZInterface)
    ['{47D209F1-D065-49DD-A156-EFD1E523F6BF}']

    function IsEmpty: Boolean;
    function IsUpdated: Boolean;
    function Length: LongInt;

    function GetString: string;
    procedure SetString(const Value: string);
    function GetUnicodeString: WideString;
    procedure SetUnicodeString(const Value: WideString);
    function GetBytes: TByteDynArray;
    procedure SetBytes(const Value: TByteDynArray);
    function GetStream: TStream;
    procedure SetStream(Value: TStream);

    procedure Clear;
    function Clone: IZBlob;
  end;

  {** Database notification interface. }
  IZNotification = interface(IZInterface)
    ['{BF785C71-EBE9-4145-8DAE-40674E45EF6F}']

    function GetEvent: string;
    procedure Listen;
    procedure Unlisten;
    procedure DoNotify;
    function CheckEvents: string;

    function GetConnection: IZConnection;
  end;

  {** Database sequence generator interface. }
  IZSequence = interface(IZInterface)
    ['{A9A54FE5-0DBE-492F-8DA6-04AC5FCE779C}']
    function  GetName: string;
    function  GetBlockSize: Integer;
    procedure SetName(const Value: string);
    procedure SetBlockSize(const Value: Integer);
    function  GetCurrentValue: Int64;
    function  GetNextValue: Int64;
    function  GetCurrentValueSQL: string;
    function  GetNextValueSQL: string;
    function  GetConnection: IZConnection;
  end;

var
  {** The common driver manager object. }
  DriverManager: IZDriverManager;

implementation

uses ZMessages;

type
  {** Driver Manager interface. }
  TZDriverManager = class(TInterfacedObject, IZDriverManager)
  private
    FDrivers: IZCollection;
    FLoginTimeout: Integer;
    FLoggingListeners: IZCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function GetConnection(const Url: string): IZConnection;
    function GetConnectionWithParams(const Url: string; Info: TStrings): IZConnection;
    function GetConnectionWithLogin(const Url: string; const User: string;
      const Password: string): IZConnection;

    function GetDriver(const Url: string): IZDriver;
    procedure RegisterDriver(Driver: IZDriver);
    procedure DeregisterDriver(Driver: IZDriver);

    function GetDrivers: IZCollection;

    function GetLoginTimeout: Integer;
    procedure SetLoginTimeout(Value: Integer);

    procedure AddLoggingListener(Listener: IZLoggingListener);
    procedure RemoveLoggingListener(Listener: IZLoggingListener);

    procedure LogMessage(Category: TZLoggingCategory; const Protocol: string;
      const Msg: WideString);
    procedure LogError(Category: TZLoggingCategory; const Protocol: string;
      const Msg: WideString; ErrorCode: Integer; const Error: WideString);
  end;

  {** Stores information about registered drivers *}
  TZDriverInfo = class(TInterfacedObject, IZInterface)
  private
    FDriver: IZDriver;
  public
    property Driver: IZDriver read FDriver write FDriver;
  end;

{ TZDriverManager }

{**
  Constructs this object with default properties.
}
constructor TZDriverManager.Create;
begin
  FDrivers := TZCollection.Create;
  FLoginTimeout := 0;
  FLoggingListeners := TZCollection.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZDriverManager.Destroy;
begin
  FDrivers := nil;
  FLoggingListeners := nil;
  inherited Destroy;
end;

{**
  Gets a collection of registered drivers.
  @return an unmodifiable collection with registered drivers.
}
function TZDriverManager.GetDrivers: IZCollection;
begin
  Result := TZUnmodifiableCollection.Create(FDrivers);
end;

{**
  Gets a login timeout value.
  @return a login timeout.
}
function TZDriverManager.GetLoginTimeout: Integer;
begin
  Result := FLoginTimeout;
end;

{**
  Sets a new login timeout value.
  @param Seconds a new login timeout in seconds.
}
procedure TZDriverManager.SetLoginTimeout(Value: Integer);
begin
  FLoginTimeout := Value;
end;

{**
  Registers a driver for specific database.
  @param Driver a driver to be registered.
}
procedure TZDriverManager.RegisterDriver(Driver: IZDriver);
begin
  if not FDrivers.Contains(Driver) then
    FDrivers.Add(Driver);
end;

{**
  Unregisters a driver for specific database.
  @param Driver a driver to be unregistered.
}
procedure TZDriverManager.DeregisterDriver(Driver: IZDriver);
begin
  FDrivers.Remove(Driver);
end;

{**
  Gets a driver which accepts the specified url.
  @param Url a database connection url.
  @return a found driver or <code>null</code> otherwise.
}
function TZDriverManager.GetDriver(const Url: string): IZDriver;
var
  I: Integer;
  Current: IZDriver;
begin
  Result := nil;
  for I := 0 to FDrivers.Count - 1 do
  begin
    Current := FDrivers[I] as IZDriver;
    if Current.AcceptsURL(Url) then
    begin
      Result := Current;
      Break;
    end;
  end;
end;

{**
  Locates a required driver and opens a connection to the specified database.
  @param Url a database connection Url.
  @param Info an extra connection parameters.
  @return an opened connection.
}
function TZDriverManager.GetConnectionWithParams(const Url: string; Info: TStrings):
  IZConnection;
var
  Driver: IZDriver;
begin
  Driver := GetDriver(Url);
  if Driver = nil then
    raise EZSQLException.Create(SDriverWasNotFound);
  Result := Driver.Connect(Url, Info);
end;

{**
  Locates a required driver and opens a connection to the specified database.
  @param Url a database connection Url.
  @param User a user's name.
  @param Password a user's password.
  @return an opened connection.
}
function TZDriverManager.GetConnectionWithLogin(const Url: string; const User: string;
  const Password: string): IZConnection;
var
  Info: TStrings;
begin
  Info := TStringList.Create;
  try
    Info.Add('username=' + User);
    Info.Add('password=' + Password);
    Result := GetConnectionWithParams(Url, Info);
  finally
    Info.Free;
  end;
end;

{**
  Locates a required driver and opens a connection to the specified database.
  @param Url a database connection Url.
  @return an opened connection.
}
function TZDriverManager.GetConnection(const Url: string): IZConnection;
begin
  Result := GetConnectionWithParams(Url, nil);
end;

{**
  Adds a logging listener to log SQL events.
  @param Listener a logging interface to be added.
}
procedure TZDriverManager.AddLoggingListener(Listener: IZLoggingListener);
begin
  FLoggingListeners.Add(Listener);
end;

{**
  Removes a logging listener from the list.
  @param Listener a logging interface to be removed.
}
procedure TZDriverManager.RemoveLoggingListener(Listener: IZLoggingListener);
begin
  FLoggingListeners.Remove(Listener);
end;

{**
  Logs a message about event with error result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
  @param ErrorCode an error code.
  @param Error an error message.
}
procedure TZDriverManager.LogError(Category: TZLoggingCategory;
  const Protocol: string; const Msg: WideString; ErrorCode: Integer; const Error: WideString);
var
  I: Integer;
  Listener: IZLoggingListener;
  Event: TZLoggingEvent;
begin
  if FLoggingListeners.Count = 0 then Exit;
  Event := TZLoggingEvent.Create(Category, Protocol, Msg, ErrorCode, Error);
  try
    for I := 0 to FLoggingListeners.Count - 1 do
    begin
      Listener := FLoggingListeners[I] as IZLoggingListener;
      try
        Listener.LogEvent(Event);
      except
      end;
    end;
  finally
    Event.Destroy;
  end;
end;

{**
  Logs a message about event with normal result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZDriverManager.LogMessage(Category: TZLoggingCategory;
  const Protocol: string; const Msg: WideString);
begin
  if FLoggingListeners.Count = 0 then Exit;
  LogError(Category, Protocol, Msg, 0, '');
end;

{ EZSQLThrowable }

{**
  Creates an exception with message string.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateClone(const E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  FErrorCode:=E.ErrorCode;
  FStatusCode:=E.Statuscode;
end;

constructor EZSQLThrowable.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := -1;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
  @param ErrorCode a native server error code.
}
constructor EZSQLThrowable.CreateWithCode(const ErrorCode: Integer;
  const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

constructor EZSQLThrowable.CreateWithStatus(const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FStatusCode := StatusCode;
end;

initialization
  DriverManager := TZDriverManager.Create;
finalization
  DriverManager := nil;
end.

