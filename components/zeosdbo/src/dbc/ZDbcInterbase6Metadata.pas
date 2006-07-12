{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Merkuriev                  }
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

unit ZDbcInterbase6Metadata;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata, ZCompatibility,
  ZDbcConnection, ZDbcInterbase6;

type

  {** Implements Interbase6 Database Metadata. }
  TZInterbase6DatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    FServerVersion: string;
    FIBConnection: TZInterbase6Connection;
    function StripEscape(const Pattern: string): string;
    function HasNoWildcards(const Pattern: string): boolean;
    function GetPrivilege(Privilege: string): string;
    function ConstructNameCondition(Pattern: string; Column: string): string;
    function GetServerVersion: string;
  public
    constructor Create(Connection: TZAbstractConnection; Url: string; Info: TStrings);
    destructor Destroy; override;

    property ServerVersion: string read GetServerVersion write FServerVersion;

    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
    function UsesLocalFilePerTable: Boolean; override;
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function StoresUpperCaseIdentifiers: Boolean; override;
    function StoresLowerCaseIdentifiers: Boolean; override;
    function StoresMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
    function StoresUpperCaseQuotedIdentifiers: Boolean; override;
    function StoresLowerCaseQuotedIdentifiers: Boolean; override;
    function StoresMixedCaseQuotedIdentifiers: Boolean; override;
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;

    function SupportsExpressionsInOrderBy: Boolean; override;
    function SupportsOrderByUnrelated: Boolean; override;
    function SupportsGroupBy: Boolean; override;
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
    function SupportsIntegrityEnhancementFacility: Boolean; override;
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
    function GetCatalogSeparator: string; override;
    function SupportsSchemasInDataManipulation: Boolean; override;
    function SupportsSchemasInProcedureCalls: Boolean; override;
    function SupportsSchemasInTableDefinitions: Boolean; override;
    function SupportsSchemasInIndexDefinitions: Boolean; override;
    function SupportsSchemasInPrivilegeDefinitions: Boolean; override;
    function SupportsCatalogsInDataManipulation: Boolean; override;
    function SupportsCatalogsInProcedureCalls: Boolean; override;
    function SupportsCatalogsInTableDefinitions: Boolean; override;
    function SupportsCatalogsInIndexDefinitions: Boolean; override;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean; override;
    function SupportsPositionedDelete: Boolean; override;
    function SupportsPositionedUpdate: Boolean; override;
    function SupportsSelectForUpdate: Boolean; override;
    function SupportsStoredProcedures: Boolean; override;
    function SupportsSubqueriesInComparisons: Boolean; override;
    function SupportsSubqueriesInExists: Boolean; override;
    function SupportsSubqueriesInIns: Boolean; override;
    function SupportsSubqueriesInQuantifieds: Boolean; override;
    function SupportsCorrelatedSubqueries: Boolean; override;
    function SupportsUnion: Boolean; override;
    function SupportsUnionAll: Boolean;  override;
    function SupportsOpenCursorsAcrossCommit: Boolean; override;
    function SupportsOpenCursorsAcrossRollback: Boolean; override;
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;

    function GetMaxBinaryLiteralLength: Integer; override;
    function GetMaxCharLiteralLength: Integer; override;
    function GetMaxColumnNameLength: Integer; override;
    function GetMaxColumnsInGroupBy: Integer; override;
    function GetMaxColumnsInIndex: Integer; override;
    function GetMaxColumnsInOrderBy: Integer; override;
    function GetMaxColumnsInSelect: Integer; override;
    function GetMaxColumnsInTable: Integer; override;
    function GetMaxConnections: Integer; override;
    function GetMaxCursorNameLength: Integer; override;
    function GetMaxIndexLength: Integer; override;
    function GetMaxSchemaNameLength: Integer; override;
    function GetMaxProcedureNameLength: Integer; override;
    function GetMaxCatalogNameLength: Integer; override;
    function GetMaxRowSize: Integer; override;
    function DoesMaxRowSizeIncludeBlobs: Boolean; override;
    function GetMaxStatementLength: Integer; override;
    function GetMaxStatements: Integer; override;
    function GetMaxTableNameLength: Integer; override;
    function GetMaxTablesInSelect: Integer; override;
    function GetMaxUserNameLength: Integer; override;

    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
    function SupportsTransactions: Boolean; override;
    function SupportsTransactionIsolationLevel(Level: TZTransactIsolationLevel):
      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function DataDefinitionCausesTransactionCommit: Boolean; override;
    function DataDefinitionIgnoredInTransactions: Boolean; override;

    function GetProcedures(Catalog: string; SchemaPattern: string;
      ProcedureNamePattern: string): IZResultSet; override;
    function GetProcedureColumns(Catalog: string; SchemaPattern: string;
      ProcedureNamePattern: string; ColumnNamePattern: string):
      IZResultSet; override;

    function GetTables(Catalog: string; SchemaPattern: string;
      TableNamePattern: string; Types: TStringDynArray): IZResultSet; override;
    function GetSchemas: IZResultSet; override;
    function GetCatalogs: IZResultSet; override;
    function GetTableTypes: IZResultSet; override;
    function GetColumns(Catalog: string; SchemaPattern: string;
      TableNamePattern: string; ColumnNamePattern: string): IZResultSet; override;
    function GetColumnPrivileges(Catalog: string; Schema: string;
      Table: string; ColumnNamePattern: string): IZResultSet; override;

    function GetTablePrivileges(Catalog: string; SchemaPattern: string;
      TableNamePattern: string): IZResultSet; override;
    function GetVersionColumns(Catalog: string; Schema: string;
      Table: string): IZResultSet; override;

    function GetPrimaryKeys(Catalog: string; Schema: string;
      Table: string): IZResultSet; override;
    function GetImportedKeys(Catalog: string; Schema: string;
      Table: string): IZResultSet; override;
    function GetExportedKeys(Catalog: string; Schema: string;
      Table: string): IZResultSet; override;
    function GetCrossReference(PrimaryCatalog: string; PrimarySchema: string;
      PrimaryTable: string; ForeignCatalog: string; ForeignSchema: string;
      ForeignTable: string): IZResultSet; override;

    function GetTypeInfo: IZResultSet; override;

    function GetIndexInfo(Catalog: string; Schema: string; Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;

    function GetSequences(Catalog: string; SchemaPattern: string;
      SequenceNamePattern: string): IZResultSet; override;

    function SupportsResultSetType(_Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(_Type: TZResultSetType;
      Concurrency: TZResultSetConcurrency): Boolean; override;

    function GetUDTs(Catalog: string; SchemaPattern: string;
      TypeNamePattern: string; Types: TIntegerDynArray): IZResultSet; override;
  end;

implementation

uses ZMessages, ZDbcInterbase6Utils;

{ TZInterbase6DatabaseMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZInterbase6DatabaseMetadata.Create(Connection: TZAbstractConnection;
  Url: string; Info: TStrings);
begin
  inherited Create(Connection, Url, Info);
  FIBConnection := Connection as TZInterbase6Connection;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZInterbase6DatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZInterbase6DatabaseMetadata.GetDatabaseProductName: string;
begin
  Result := 'Interbase/Firebird';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZInterbase6DatabaseMetadata.GetDatabaseProductVersion: string;
begin
  Result := '6.0+';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZInterbase6DatabaseMetadata.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Interbase and Firebird';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZInterbase6DatabaseMetadata.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZInterbase6DatabaseMetadata.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZInterbase6DatabaseMetadata.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZInterbase6DatabaseMetadata.GetSQLKeywords: string;
begin
  Result := 'ACTIVE,AFTER,ASCENDING,BASE_NAME,BEFORE,BLOB,' +
    'CACHE,CHECK_POINT_LENGTH,COMPUTED,CONDITIONAL,CONTAINING,' +
    'CSTRING,DATABASE,RDB$DB_KEY,DEBUG,DESCENDING,DO,ENTRY_POINT,' +
    'EXIT,FILE,FILTER,FUNCTION,GDSCODE,GENERATOR,GEN_ID,' +
    'GROUP_COMMIT_WAIT_TIME,IF,INACTIVE,INPUT_TYPE,INDEX,' +
    'LOGFILE,LOG_BUFFER_SIZE,MANUAL,MAXIMUM_SEGMENT,MERGE, MESSAGE,' +
    'MODULE_NAME,NCHAR,NUM_LOG_BUFFERS,OUTPUT_TYPE,OVERFLOW,PAGE,' +
    'PAGES,PAGE_SIZE,PARAMETER,PASSWORD,PLAN,POST_EVENT,PROTECTED,' +
    'RAW_PARTITIONS,RESERV,RESERVING,RETAIN,RETURNING_VALUES,RETURNS,' +
    'SEGMENT,SHADOW,SHARED,SINGULAR,SNAPSHOT,SORT,STABILITY,STARTS,' +
    'STARTING,STATISTICS,SUB_TYPE,SUSPEND,TRIGGER,VARIABLE,RECORD_VERSION,' +
    'WAIT,WHILE,WORK';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseMetadata.GetNumericFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseMetadata.GetStringFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseMetadata.GetSystemFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZInterbase6DatabaseMetadata.GetTimeDateFunctions: string;
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
function TZInterbase6DatabaseMetadata.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZInterbase6DatabaseMetadata.GetExtraNameCharacters: string;
begin
  Result := '$';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := False;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZInterbase6DatabaseMetadata.GetSchemaTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZInterbase6DatabaseMetadata.GetProcedureTerm: string;
begin
  Result := 'PROCEDURE';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZInterbase6DatabaseMetadata.GetCatalogTerm: string;
begin
  Result := '';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZInterbase6DatabaseMetadata.GetCatalogSeparator: string;
begin
  Result := '';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsPositionedDelete: Boolean;
begin
  Result := True;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsPositionedUpdate: Boolean;
begin
  Result := True;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSubqueriesInIns: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseMetadata.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseMetadata.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseMetadata.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseMetadata.SupportsOpenStatementsAcrossRollback: Boolean;
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
function TZInterbase6DatabaseMetadata.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxCharLiteralLength: Integer;
begin
  Result := 1024;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxColumnNameLength: Integer;
begin
  Result := 31;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxColumnsInSelect: Integer;
begin
  Result := 32767;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxColumnsInTable: Integer;
begin
  Result := 32767;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxCursorNameLength: Integer;
begin
  Result := 31;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxIndexLength: Integer;
begin
  Result := 198;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxSchemaNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxProcedureNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxCatalogNameLength: Integer;
begin
  Result := 27;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxRowSize: Integer;
begin
  Result := 32664;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := False;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxStatementLength: Integer;
begin
  Result := 640;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxTableNameLength: Integer;
begin
  Result := 31;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxTablesInSelect: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseMetadata.GetMaxUserNameLength: Integer;
begin
  Result := 31;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZInterbase6DatabaseMetadata.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiSerializable;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZInterbase6DatabaseMetadata.SupportsTransactionIsolationLevel(
  Level: TZTransactIsolationLevel): Boolean;
begin
  case Level of
    tiRepeatableRead, tiReadCommitted, tiSerializable: Result := True;
    tiReadUncommitted: Result := False;
    tiNone: Result := False; //MAY BE FIX IT
    else
      Result := False;
  end;    
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.DataDefinitionIgnoredInTransactions: Boolean;
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
function TZInterbase6DatabaseMetadata.GetProcedures(Catalog: string;
  SchemaPattern: string; ProcedureNamePattern: string): IZResultSet;
var
  SQL: string;
  Key: string;
begin
  Key := Format('get-procedures:%s:%s:%s',
    [Catalog, SchemaPattern, ProcedureNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ProceduresColumnsDynArray);

    ProcedureNamePattern := ConstructNameCondition(ProcedureNamePattern,
      'RDB$PROCEDURE_NAME');
    SQL := 'SELECT RDB$PROCEDURE_NAME, RDB$PROCEDURE_OUTPUTS,'
      + ' RDB$DESCRIPTION FROM RDB$PROCEDURES';
    if ProcedureNamePattern <> '' then
      SQL := SQL + ' WHERE ' + ProcedureNamePattern;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateNull(2);
        Result.UpdateString(3,
          GetStringByName('RDB$PROCEDURE_NAME'));
        Result.UpdateNull(4);
        Result.UpdateNull(5);
        Result.UpdateNull(6);
        Result.UpdateString(7,
          GetStringByName('RDB$DESCRIPTION'));
        if IsNullByName('RDB$PROCEDURE_OUTPUTS') then
          Result.UpdateInt(8, Ord(prtNoResult))
        else Result.UpdateInt(8, Ord(prtReturnsResult));
        Result.InsertRow;
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetProcedureColumns(Catalog: string;
  SchemaPattern: string; ProcedureNamePattern: string;
  ColumnNamePattern: string): IZResultSet;
var
  Key: string;
  SQL, Where: string;
  TypeName, SubTypeName: Integer;
begin
  Key := Format('get-procedure-columns:%s:%s:%s:%s',
    [Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ProceduresColColumnsDynArray);

    ProcedureNamePattern := ConstructNameCondition(ProcedureNamePattern,
      'P.RDB$PROCEDURE_NAME');
    ColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
      'PP.RDB$PARAMETER_NAME');

    if StrPos(PChar(ServerVersion), 'Interbase 5') <> nil then
    begin
      SQL := ' SELECT P.RDB$PROCEDURE_NAME, PP.RDB$PARAMETER_NAME,'
        + ' PP.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE,'
        + ' F.RDB$FIELD_SCALE, F.RDB$FIELD_LENGTH, F.RDB$NULL_FLAG,'
        + ' PP.RDB$DESCRIPTION, F.RDB$FIELD_SCALE as RDB$FIELD_PRECISION,'
        + ' F.RDB$NULL_FLAG FROM RDB$PROCEDURES P'
        + ' JOIN RDB$PROCEDURE_PARAMETERS PP ON P.RDB$PROCEDURE_NAME'
        + '=PP.RDB$PROCEDURE_NAME JOIN RDB$FIELDS F ON PP.RDB$FIELD_SOURCE'
        + '=F.RDB$FIELD_NAME ';

      Where := ProcedureNamePattern;
      if ColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := ColumnNamePattern
        else Where := Where + ' AND ' + ColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY  P.RDB$PROCEDURE_NAME,'
        + ' PP.RDB$PARAMETER_TYPE desc, PP.RDB$PARAMETER_NUMBER';
    end
    else
    begin
      SQL := ' SELECT P.RDB$PROCEDURE_NAME, PP.RDB$PARAMETER_NAME,'
        + ' PP.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE,'
        + ' F.RDB$FIELD_SCALE, F.RDB$FIELD_LENGTH, F.RDB$NULL_FLAG,'
        + ' PP.RDB$DESCRIPTION, F.RDB$FIELD_PRECISION, F.RDB$NULL_FLAG '
        + ' FROM RDB$PROCEDURES P JOIN RDB$PROCEDURE_PARAMETERS PP ON'
        + ' P.RDB$PROCEDURE_NAME = PP.RDB$PROCEDURE_NAME '
        + ' JOIN RDB$FIELDS F ON PP.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ';

      Where := ProcedureNamePattern;
      if ColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := ColumnNamePattern
        else Where := Where + ' AND ' + ColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY  P.RDB$PROCEDURE_NAME,'
        + ' PP.RDB$PARAMETER_TYPE desc, PP.RDB$PARAMETER_NUMBER';
    end;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        TypeName := GetIntByName('RDB$FIELD_TYPE');
        SubTypeName := GetIntByName('RDB$FIELD_SUB_TYPE');
        //FieldScale := GetIntByName('RDB$FIELD_SCALE');

        Result.MoveToInsertRow;
        Result.UpdateNull(1);    //PROCEDURE_CAT
        Result.UpdateNull(2);    //PROCEDURE_SCHEM
        Result.UpdateString(3,
          GetStringByName('RDB$PROCEDURE_NAME'));    //TABLE_NAME
        Result.UpdateString(4,
          GetStringByName('RDB$PARAMETER_NAME'));    //COLUMN_NAME
        case GetIntByName('RDB$PARAMETER_TYPE') of
          0: Result.UpdateInt(5, 1);//ptInput
          1: Result.UpdateInt(5, 4);//ptResult
          else Result.UpdateInt(5, 0);//ptUnknown
        end;

        Result.UpdateInt(6,
          Ord(ConvertInterbase6ToSqlType(TypeName, SubTypeName))); //DATA_TYPE
        Result.UpdateString(7,
          GetStringByName('RDB$FIELD_TYPE'));    //TYPE_NAME
        Result.UpdateInt(10,
          GetIntByName('RDB$FIELD_PRECISION'));
        Result.UpdateNull(9);    //BUFFER_LENGTH
        Result.UpdateInt(10,
          GetIntByName('RDB$FIELD_SCALE'));
        Result.UpdateInt(11, 10);
        Result.UpdateInt(12,
          GetIntByName('RDB$NULL_FLAG'));
        Result.UpdateString(12,
          GetStringByName('RDB$FIELD_PRECISION'));
        Result.InsertRow;
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetTables(Catalog: string;
  SchemaPattern: string; TableNamePattern: string;
  Types: TStringDynArray): IZResultSet;
var
  Key: string;
  SQL, TableType: string;
  BLR: IZBlob;
  I, SystemFlag, ViewContext: Integer;
begin
  Key := '';
  for I := Low(Types) to High(Types) do
    Key := Key + ':' + Types[I];

  Key := Format('get-tables:%s:%s:%s%s',
    [Catalog, SchemaPattern, TableNamePattern, Key]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColumnsDynArray);

    TableNamePattern := ConstructNameCondition(TableNamePattern,
      'a.RDB$RELATION_NAME');
    SQL := 'SELECT DISTINCT a.RDB$RELATION_NAME, b.RDB$SYSTEM_FLAG,'
      + ' b.RDB$VIEW_CONTEXT, a.RDB$VIEW_SOURCE FROM RDB$RELATIONS a'
      + ' JOIN RDB$RELATION_FIELDS b ON a.RDB$RELATION_NAME'
      + '=b.RDB$RELATION_NAME';

    if TableNamePattern <> '' then
      SQL := SQL + ' WHERE ' + TableNamePattern;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        SystemFlag := GetIntByName('RDB$SYSTEM_FLAG');
        ViewContext := GetIntByName('RDB$VIEW_CONTEXT');

        if SystemFlag = 0 then
        begin
          if ViewContext = 0 then
          BLR := GetBlobByName('RDB$VIEW_SOURCE');
          if BLR.IsEmpty then
            TableType := 'TABLE'
          else TableType := 'VIEW';
        end else
          TableType := 'SYSTEM TABLE';

        if Length(Types) = 0 then
        begin
          Result.MoveToInsertRow;
          Result.UpdateNull(1);
          Result.UpdateNull(2);
          Result.UpdateString(3, GetStringByName('RDB$RELATION_NAME'));
          Result.UpdateString(4, TableType);
          Result.UpdateNull(5);
          Result.InsertRow;
        end
        else begin
          for I := 0 to High(Types) do
          begin
            if Types[I] = TableType then
            begin
              Result.MoveToInsertRow;
              Result.UpdateNull(1);
              Result.UpdateNull(2);
              Result.UpdateString(3, GetStringByName('RDB$RELATION_NAME'));
              Result.UpdateString(4, TableType);
              Result.UpdateNull(5);
              Result.InsertRow;
            end;
          end;
        end;
            
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetSchemas: IZResultSet;
begin
  Result := inherited GetSchemas;
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
function TZInterbase6DatabaseMetadata.GetCatalogs: IZResultSet;
begin
  Result := inherited GetCatalogs;
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
function TZInterbase6DatabaseMetadata.GetTableTypes: IZResultSet;
const
  TablesTypes: array [0..2] of string = ('TABLE', 'VIEW', 'SYSTEM TABLE');
var
  I: Integer;
  Key: string;
begin
  Key := 'get-table-types';

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);

    for I := 0 to 2 do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(1, TablesTypes[I]);
      Result.InsertRow;
    end;

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
function TZInterbase6DatabaseMetadata.GetColumns(Catalog: string;
  SchemaPattern: string; TableNamePattern: string;
  ColumnNamePattern: string): IZResultSet;
var
  Key: string;
  SQL, Where, ColumnName, DefaultValue: string;
  TypeName, SubTypeName, FieldScale: integer;
begin
  Key := Format('get-columns:%s:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColColumnsDynArray);

    TableNamePattern := ConstructNameCondition(TableNamePattern,
      'a.RDB$RELATION_NAME');
    ColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
      'a.RDB$FIELD_NAME');

    if StrPos(PChar(ServerVersion), 'Interbase 5') <> nil then
    begin
      SQL := 'SELECT a.RDB$RELATION_NAME, a.RDB$FIELD_NAME, a.RDB$FIELD_POSITION,'
        + ' a.RDB$NULL_FLAG, b. RDB$FIELD_LENGTH, b.RDB$FIELD_SCALE,'
        + ' c.RDB$TYPE_NAME, b.RDB$FIELD_TYPE, b.RDB$FIELD_SUB_TYPE,'
        + ' b.RDB$DESCRIPTION, b.RDB$CHARACTER_LENGTH, b.RDB$FIELD_SCALE'
        + ' as RDB$FIELD_PRECISION, a.RDB$DEFAULT_SOURCE, b.RDB$DEFAULT_SOURCE'
        + ' as RDB$DEFAULT_SOURCE_DOMAIN FROM RDB$RELATION_FIELDS a'
        + ' JOIN RDB$FIELDS b ON (b.RDB$FIELD_NAME = a.RDB$FIELD_SOURCE)'
        + ' LEFT JOIN RDB$TYPES c ON b.RDB$FIELD_TYPE = c.RDB$TYPE'
        + ' and c.RDB$FIELD_NAME = ''RDB$FIELD_TYPE''';

      Where := TableNamePattern;
      if ColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := ColumnNamePattern
        else Where := Where + ' AND ' + ColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$FIELD_POSITION';
    end
    else
    begin
      SQL := ' SELECT a.RDB$RELATION_NAME, a.RDB$FIELD_NAME, a.RDB$FIELD_POSITION,'
        + ' a.RDB$NULL_FLAG, a.RDB$DEFAULT_VALUE, b. RDB$FIELD_LENGTH,'
        + ' b.RDB$FIELD_SCALE, c.RDB$TYPE_NAME, b.RDB$FIELD_TYPE,'
        + ' b.RDB$FIELD_SUB_TYPE, b.RDB$DESCRIPTION, b.RDB$CHARACTER_LENGTH,'
        + ' b.RDB$FIELD_PRECISION, a.RDB$DEFAULT_SOURCE, b.RDB$DEFAULT_SOURCE'
        + ' as RDB$DEFAULT_SOURCE_DOMAIN FROM RDB$RELATION_FIELDS a'
        + ' JOIN RDB$FIELDS b ON (b.RDB$FIELD_NAME = a.RDB$FIELD_SOURCE)'
        + ' LEFT JOIN RDB$TYPES c ON (b.RDB$FIELD_TYPE = c.RDB$TYPE'
        + ' and c.RDB$FIELD_NAME = ''RDB$FIELD_TYPE'')';

      Where := TableNamePattern;
      if ColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := ColumnNamePattern
        else Where := Where + ' AND ' + ColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$FIELD_POSITION';
    end;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        TypeName := GetIntByName('RDB$FIELD_TYPE');
        SubTypeName := GetIntByName('RDB$FIELD_SUB_TYPE');
        FieldScale := GetIntByName('RDB$FIELD_SCALE');
        ColumnName := GetStringByName('RDB$FIELD_NAME');

        DefaultValue := GetStringByName('RDB$DEFAULT_SOURCE');
        if DefaultValue = '' then
          DefaultValue := GetStringByName('RDB$DEFAULT_SOURCE_DOMAIN');
        if StartsWith(UpperCase(DefaultValue), 'DEFAULT') then
        begin
          DefaultValue := Trim(Copy(DefaultValue,
            Length('DEFAULT') + 1, Length(DefaultValue)));
        end;

        Result.MoveToInsertRow;
        Result.UpdateNull(1);    //TABLE_CAT
        Result.UpdateNull(2);    //TABLE_SCHEM
        Result.UpdateString(3,
          GetStringByName('RDB$RELATION_NAME'));    //TABLE_NAME
        Result.UpdateString(4, ColumnName);    //COLUMN_NAME
        Result.UpdateInt(5,
          Ord(ConvertInterbase6ToSqlType(TypeName, SubTypeName))); //DATA_TYPE
        Result.UpdateString(6,
          GetStringByName('RDB$TYPE_NAME'));    //TYPE_NAME
        Result.UpdateInt(7,
          GetIntByName('RDB$FIELD_LENGTH'));    //COLUMN_SIZE
        Result.UpdateNull(8);    //BUFFER_LENGTH

        if FieldScale < 0 then
          Result.UpdateInt(9, -1 * FieldScale)    //DECIMAL_DIGITS
        else Result.UpdateInt(9, 0);    //DECIMAL_DIGITS

        Result.UpdateInt(10, 10);   //NUM_PREC_RADIX

        if GetIntByName('RDB$NULL_FLAG') <> 0 then
          Result.UpdateInt(11, Ord(ntNoNulls))   //NULLABLE
        else Result.UpdateInt(11, Ord(ntNullable));

        Result.UpdateString(12,
          GetStringByName('RDB$DESCRIPTION'));   //REMARKS
        Result.UpdateString(13, DefaultValue);   //COLUMN_DEF
        Result.UpdateNull(14);   //SQL_DATA_TYPE
        Result.UpdateNull(15);   //SQL_DATETIME_SUB
        Result.UpdateInt(16,
          GetInt(7));   //CHAR_OCTET_LENGTH
        Result.UpdateInt(17,
          GetIntByName('RDB$FIELD_POSITION') + 1);   //ORDINAL_POSITION

        if IsNullByName('RDB$NULL_FLAG') then
          Result.UpdateString(18, 'YES')   //IS_NULLABLE
        else Result.UpdateString(18, 'NO');   //IS_NULLABLE

        Result.UpdateNullByName('AUTO_INCREMENT');

        if CompareStr(ColumnName, UpperCase(ColumnName)) = 0 then
          Result.UpdateBooleanByName('CASE_SENSITIVE', False)
        else
          Result.UpdateBooleanByName('CASE_SENSITIVE', True);

        Result.UpdateBooleanByName('SEARCHABLE', True);
        Result.UpdateBooleanByName('WRITABLE', True);
        Result.UpdateBooleanByName('DEFINITELYWRITABLE', True);
        Result.UpdateBooleanByName('READONLY', False);

        Result.InsertRow;
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetColumnPrivileges(Catalog: string;
  Schema: string; Table: string; ColumnNamePattern: string): IZResultSet;
var
  SQL: string;
  Key: string;
  TableName, FieldName, Privilege: string;
  Grantor, Grantee, Grantable: string;
begin
  Key := Format('get-column-privileges:%s:%s:%s:%s',
    [Catalog, Schema, Table, ColumnNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);

    Table := ConstructNameCondition(Table, 'a.RDB$RELATION_NAME');
    ColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
      'a.RDB$FIELD_NAME');

    SQL := 'SELECT a.RDB$USER, a.RDB$GRANTOR, a.RDB$PRIVILEGE,'
      + ' a.RDB$GRANT_OPTION, a.RDB$RELATION_NAME, a.RDB$FIELD_NAME '
      + ' FROM RDB$USER_PRIVILEGES a, RDB$TYPES b '
      + ' WHERE a.RDB$OBJECT_TYPE = b.RDB$TYPE AND ';
    if Table <> '' then
      SQL := SQL + Table + ' AND ';
    if ColumnNamePattern <> '' then
      SQL := SQL + ColumnNamePattern + ' AND ';
    SQL := SQL + ' b.RDB$TYPE_NAME IN (''RELATION'', ''VIEW'','
      + ' ''COMPUTED_FIELD'', ''FIELD'' ) AND b.RDB$FIELD_NAME'
      + '=''RDB$OBJECT_TYPE'' ORDER BY a.RDB$FIELD_NAME, a.RDB$PRIVILEGE  ' ;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        TableName := GetStringByName('RDB$RELATION_NAME');
        FieldName := GetStringByName('RDB$FIELD_NAME');
        Privilege := GetPrivilege(GetStringByName('RDB$PRIVILEGE'));
        Grantor := GetStringByName('RDB$GRANTOR');
        Grantee := GetStringByName('RDB$USER');

        if Grantor = Grantee then
          Grantable := 'YES'
        else Grantable := 'NO';
        if FieldName = '' then
        begin
          SQL := 'SELECT RDB$FIELD_NAME FROM RDB$RELATION_FIELDS '
            + ' WHERE RDB$RELATION_NAME = ''' + TableName + ''' AND '
            + ' RDB$FIELD_NAME = ''' + ColumnNamePattern + ''' AND ';
          with GetConnection.CreateStatement.ExecuteQuery(SQL) do
          begin
            while Next do
            begin
              Result.MoveToInsertRow;
              Result.UpdateNull(1);
              Result.UpdateNull(2);
              Result.UpdateString(3, TableName);
              Result.UpdateString(4, GetString(1));
              Result.UpdateString(5, Grantor);
              Result.UpdateString(6, Grantee);
              Result.UpdateString(7, Privilege);
              Result.UpdateString(8, Grantable);
              Result.InsertRow;
            end;
            Close;
          end;
        end
        else
        begin
          Result.MoveToInsertRow;
          Result.UpdateNull(1);
          Result.UpdateNull(2);
          Result.UpdateString(3, TableName);
          Result.UpdateString(4, FieldName);
          Result.UpdateString(5, Grantor);
          Result.UpdateString(6, Grantee);
          Result.UpdateString(7, Privilege);
          Result.UpdateString(8, Grantable);
          Result.InsertRow;
        end;
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetTablePrivileges(Catalog: string;
  SchemaPattern: string; TableNamePattern: string): IZResultSet;
var
  SQL: string;
  Key: string;
  TableName, Privilege, Grantor: string;
  Grantee, Grantable: string;
begin
  Key := Format('get-table-privileges:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);

    TableNamePattern := ConstructNameCondition(TableNamePattern,
      'a.RDB$RELATION_NAME');

    SQL := 'SELECT a.RDB$USER, a.RDB$GRANTOR, a.RDB$PRIVILEGE,'
      + ' a.RDB$GRANT_OPTION, a.RDB$RELATION_NAME FROM RDB$USER_PRIVILEGES a,'
      + ' RDB$TYPES b WHERE a.RDB$OBJECT_TYPE = b.RDB$TYPE AND '
      + ' b.RDB$TYPE_NAME IN (''RELATION'', ''VIEW'', ''COMPUTED_FIELD'','
      + ' ''FIELD'' ) AND a.RDB$FIELD_NAME IS NULL ';
    if TableNamePattern <> '' then
      SQL := SQL + ' AND ' + TableNamePattern;
    SQL := SQL + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$PRIVILEGE';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        TableName := GetStringByName('RDB$RELATION_NAME');
        Privilege := GetPrivilege(GetStringByName('RDB$PRIVILEGE'));
        Grantor := GetStringByName('RDB$GRANTOR');
        Grantee := GetStringByName('RDB$USER');

        if Grantor = Grantee then
          Grantable := 'YES'
        else Grantable := 'NO';

        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateNull(2);
        Result.UpdateString(3, TableName);
        Result.UpdateString(4, Grantor);
        Result.UpdateString(5, Grantee);
        Result.UpdateString(6, Privilege);
        Result.UpdateString(7, Grantable);
        Result.InsertRow;
      end;
      Close;
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
function TZInterbase6DatabaseMetadata.GetVersionColumns(Catalog: string;
  Schema: string; Table: string): IZResultSet;
var
  Key: string;
begin
  Key := Format('get-version-columns:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TableColVerColumnsDynArray);

    Result.MoveToInsertRow;
    Result.UpdateNull(1);
    Result.UpdateString(2, 'ctid');
    //  Result.UpdateInt(3, GetSQLType('tid')); //FIX IT
    Result.UpdateString(4, 'tid');
    Result.UpdateNull(5);
    Result.UpdateNull(6);
    Result.UpdateNull(7);
    Result.UpdateInt(4, Ord(vcPseudo));
    Result.InsertRow;

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
function TZInterbase6DatabaseMetadata.GetPrimaryKeys(Catalog: string;
  Schema: string; Table: string): IZResultSet;
var
  SQL, Key: string;
begin
  Key := Format('get-primary-keys:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Table := ConstructNameCondition(Table, 'a.RDB$RELATION_NAME');
    SQL := ' SELECT null as TABLE_CAT, null as TABLE_SCHEM,'
      + ' a.RDB$RELATION_NAME as TABLE_NAME, b.RDB$FIELD_NAME as COLUMN_NAME,'
      + ' b.RDB$FIELD_POSITION+1 as KEY_SEQ, a.RDB$INDEX_NAME as PK_NAME'
      + ' FROM RDB$RELATION_CONSTRAINTS a JOIN RDB$INDEX_SEGMENTS b ON'
      + ' (a.RDB$INDEX_NAME = b.RDB$INDEX_NAME)'
      + ' WHERE  RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''';
    if Table <> '' then
      SQL := SQL + ' AND ' + Table;
    SQL := SQL + ' ORDER BY a.RDB$RELATION_NAME, b.RDB$FIELD_NAME';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(PrimaryKeyColumnsDynArray));
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
function TZInterbase6DatabaseMetadata.GetImportedKeys(Catalog: string;
  Schema: string; Table: string): IZResultSet;
var
  Key, SQL: string;
begin
  Key := Format('get-imported-keys:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ImportedKeyColumnsDynArray);

    Table := ConstructNameCondition(Table, 'RELC_FOR.RDB$RELATION_NAME');
    SQL := 'SELECT RELC_PRIM.RDB$RELATION_NAME, '    // 1 prim.RDB$ key table name
      + ' IS_PRIM.RDB$FIELD_NAME, '         // 2 prim.RDB$ key column name
      + ' RELC_FOR.RDB$RELATION_NAME, '     // 3 foreign key table name
      + ' IS_FOR.RDB$FIELD_NAME, '          // 4 foreign key column name
      + ' IS_FOR.RDB$FIELD_POSITION, '      // 5 key sequence
      + ' REFC_PRIM.RDB$UPDATE_RULE, '      // 6
      + ' REFC_PRIM.RDB$DELETE_RULE, '      // 7
      + ' RELC_FOR.RDB$CONSTRAINT_NAME, '   // 8 foreign key constraint name
      + ' RELC_PRIM.RDB$CONSTRAINT_NAME '   // 9 primary key constraint name
      + ' FROM RDB$RELATION_CONSTRAINTS RELC_FOR, RDB$REF_CONSTRAINTS REFC_FOR, '
      + ' RDB$RELATION_CONSTRAINTS RELC_PRIM, RDB$REF_CONSTRAINTS REFC_PRIM, '
      + ' RDB$INDEX_SEGMENTS IS_PRIM,  RDB$INDEX_SEGMENTS IS_FOR '
      + ' WHERE RELC_FOR.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ';
     if Table <> '' then
       SQL := SQL + Table + ' AND ';
     SQL := SQL + ' RELC_FOR.RDB$CONSTRAINT_NAME=REFC_FOR.RDB$CONSTRAINT_NAME'
       + ' and REFC_FOR.RDB$CONST_NAME_UQ = RELC_PRIM.RDB$CONSTRAINT_NAME and '
       + ' RELC_PRIM.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and ' // useful check, anyay
       + ' RELC_PRIM.RDB$INDEX_NAME = IS_PRIM.RDB$INDEX_NAME and '
       + ' IS_FOR.RDB$INDEX_NAME = RELC_FOR.RDB$INDEX_NAME   and '
       + ' IS_PRIM.RDB$FIELD_POSITION = IS_FOR.RDB$FIELD_POSITION  and '
       + ' REFC_PRIM.RDB$CONSTRAINT_NAME = RELC_FOR.RDB$CONSTRAINT_NAME '
       + ' ORDER BY RELC_PRIM.RDB$RELATION_NAME, IS_FOR.RDB$FIELD_POSITION ';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNullByName('PKTABLE_CAT');
        Result.UpdateNullByName('PKTABLE_SCHEM');
        Result.UpdateStringByName('PKTABLE_NAME', GetString(1));
        Result.UpdateStringByName('PKCOLUMN_NAME', GetString(2));
        Result.UpdateNullByName('FKTABLE_CAT');
        Result.UpdateNullByName('FKTABLE_SCHEM');
        Result.UpdateStringByName('FKTABLE_NAME', GetString(3));
        Result.UpdateStringByName('FKCOLUMN_NAME', GetString(4));
        Result.UpdateIntByName('KEY_SEQ', GetInt(5) + 1);

        if GetString(6) = 'RESTRICT' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikRestrict))
        else if GetString(6) = 'NO ACTION' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikNoAction))
        else if GetString(6) = 'SET DEFAULT' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikSetDefault))
        else if GetString(6) = 'CASCADE' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikCascade))
        else if GetString(6) = 'SET NULL' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikSetNull));

        if GetString(7) = 'RESTRICT' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikRestrict))
        else if GetString(7) = 'NO ACTION' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikNoAction))
        else if GetString(7) = 'SET DEFAULT' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikSetDefault))
        else if GetString(7) = 'CASCADE' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikCascade))
        else if GetString(7) = 'SET NULL' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikSetNull));

        Result.UpdateString(3, GetString(1));
        Result.UpdateStringByName('FK_NAME', GetString(8));
        Result.UpdateStringByName('PK_NAME', GetString(9));
        Result.UpdateNullByName('DEFERRABILITY');
        Result.InsertRow;
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetExportedKeys(Catalog: string;
  Schema: string; Table: string): IZResultSet;
var
  SQL, Key: string;
begin
  Key := Format('get-exported-keys:%s:%s:%s', [Catalog, Schema, Table]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(ExportedKeyColumnsDynArray);

    Table := ConstructNameCondition(Table, 'RC_PRIM.RDB$RELATION_NAME');
    SQL := ' SELECT RC_PRIM.RDB$RELATION_NAME, ' // prim.RDB$ key table name
      + ' IS_PRIM.RDB$FIELD_NAME, '       // prim.RDB$ key column name
      + ' RC_FOR.RDB$RELATION_NAME, '     // foreign key table name
      + ' IS_FOR.RDB$FIELD_NAME, '        // foreign key column name
      + ' IS_FOR.RDB$FIELD_POSITION, '    // key sequence
      + ' REFC_PRIM.RDB$UPDATE_RULE, '    // if update or delete rule is null, interpret as RESTRICT
      + ' REFC_PRIM.RDB$DELETE_RULE, '
      + ' RC_FOR.RDB$CONSTRAINT_NAME, '   // foreign key constraint name
      + ' RC_PRIM.RDB$CONSTRAINT_NAME '  // primary key constraint name
      + ' FROM RDB$RELATION_CONSTRAINTS RC_FOR, RDB$REF_CONSTRAINTS REFC_FOR, '
      + ' RDB$RELATION_CONSTRAINTS RC_PRIM, RDB$REF_CONSTRAINTS REFC_PRIM, '
      + ' RDB$INDEX_SEGMENTS IS_PRIM, RDB$INDEX_SEGMENTS IS_FOR '
      + ' WHERE RC_PRIM.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and ';
    if Table <> '' then
      SQL := SQL + Table + ' AND ';
    SQL := SQL + ' REFC_FOR.RDB$CONST_NAME_UQ = RC_PRIM.RDB$CONSTRAINT_NAME'
      + ' and RC_FOR.RDB$CONSTRAINT_NAME = REFC_FOR.RDB$CONSTRAINT_NAME and '
      + ' RC_FOR.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' and '// useful check, anyay
      + ' RC_PRIM.RDB$INDEX_NAME = IS_PRIM.RDB$INDEX_NAME and '
      + ' IS_FOR.RDB$INDEX_NAME = RC_FOR.RDB$INDEX_NAME   and '
      + ' IS_PRIM.RDB$FIELD_POSITION = IS_FOR.RDB$FIELD_POSITION  and '
      + ' REFC_PRIM.RDB$CONSTRAINT_NAME = RC_FOR.RDB$CONSTRAINT_NAME '
      + ' ORDER BY RC_FOR.RDB$RELATION_NAME, IS_FOR.RDB$FIELD_POSITION ';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNullByName('PKTABLE_CAT');
        Result.UpdateNullByName('PKTABLE_SCHEM');
        Result.UpdateStringByName('PKTABLE_NAME', GetString(1));
        Result.UpdateStringByName('PKCOLUMN_NAME', GetString(2));
        Result.UpdateNullByName('FKTABLE_CAT');
        Result.UpdateNullByName('FKTABLE_SCHEM');
        Result.UpdateStringByName('FKTABLE_NAME', GetString(3));
        Result.UpdateStringByName('FKCOLUMN_NAME', GetString(4));
        Result.UpdateIntByName('KEY_SEQ', GetInt(5) + 1);

        if GetString(6) = 'RESTRICT' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikRestrict))
        else if GetString(6) = 'NO ACTION' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikNoAction))
        else if GetString(6) = 'SET DEFAULT' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikSetDefault))
        else if GetString(6) = 'CASCADE' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikCascade))
        else if GetString(6) = 'SET NULL' then
          Result.UpdateIntByName('UPDATE_RULE', Ord(ikSetNull));

        if GetString(7) = 'RESTRICT' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikRestrict))
        else if GetString(7) = 'NO ACTION' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikNoAction))
        else if GetString(7) = 'SET DEFAULT' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikSetDefault))
        else if GetString(7) = 'CASCADE' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikCascade))
        else if GetString(7) = 'SET NULL' then
          Result.UpdateIntByName('DELETE_RULE', Ord(ikSetNull));

        Result.UpdateString(3, GetString(1));
        Result.UpdateStringByName('FK_NAME', GetString(8));
        Result.UpdateStringByName('PK_NAME', GetString(9));
        Result.UpdateNullByName('DEFERRABILITY');
        Result.InsertRow;
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetCrossReference(PrimaryCatalog: string;
  PrimarySchema: string; PrimaryTable: string; ForeignCatalog: string;
  ForeignSchema: string; ForeignTable: string): IZResultSet;
begin
  Result := inherited GetCrossReference(PrimaryCatalog, PrimarySchema,
    PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable);
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
function TZInterbase6DatabaseMetadata.GetTypeInfo: IZResultSet;
var
  SQL, Key: string;
begin
  Key := 'get-type-info';

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);

    SQL := ' SELECT RDB$TYPE, RDB$TYPE_NAME FROM RDB$TYPES ' +
      ' WHERE RDB$FIELD_NAME = ''RDB$FIELD_TYPE'' ';
    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(1, GetString(2));
        Result.UpdateInt(2, Ord(ConvertInterbase6ToSqlType(GetInt(1), 0)));
        Result.UpdateInt(3, 9);
        Result.UpdateInt(7, Ord(ntNoNulls));
        Result.UpdateBoolean(8, false);
        Result.UpdateBoolean(9, false);
        Result.UpdateBoolean(11, false);
        Result.UpdateBoolean(12, false);
        Result.UpdateInt(18, 10);
        Result.InsertRow;
      end;
      Close;
    end;

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
function TZInterbase6DatabaseMetadata.GetIndexInfo(Catalog: string;
  Schema: string; Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  SQL, Key: string;
begin
  Key := Format('get-index-info:%s:%s:%s:%s:%s',
    [Catalog, Schema, Table, BoolToStr(Unique), BoolToStr(Approximate)]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);

    SQL :=  ' SELECT I.RDB$RELATION_NAME, I.RDB$UNIQUE_FLAG, I.RDB$INDEX_NAME,'
      + ' ISGMT.RDB$FIELD_POSITION,	ISGMT.RDB$FIELD_NAME, I.RDB$INDEX_TYPE,'
      + ' I.RDB$SEGMENT_COUNT, COUNT (DISTINCT P.RDB$PAGE_NUMBER) '
      + ' FROM RDB$INDICES I JOIN RDB$INDEX_SEGMENTS ISGMT ON'
      + ' I.RDB$INDEX_NAME = ISGMT.RDB$INDEX_NAME JOIN RDB$RELATIONS R ON'
      + ' (R.RDB$RELATION_NAME = I.RDB$RELATION_NAME) JOIN RDB$PAGES P ON'
      + ' (P.RDB$RELATION_ID = R.RDB$RELATION_ID AND P.RDB$PAGE_TYPE = 7'
      + ' OR P.RDB$PAGE_TYPE = 6) WHERE ';
    if Unique then
      SQL := SQL + ' I.RDB$UNIQUE_FLAG = 1 AND ';
    SQL := SQL + ' I.RDB$RELATION_NAME = ''' + Table + ''' GROUP BY '
      + ' I.RDB$INDEX_NAME, I.RDB$RELATION_NAME, I.RDB$UNIQUE_FLAG, '
      + ' ISGMT.RDB$FIELD_POSITION, ISGMT.RDB$FIELD_NAME, I.RDB$INDEX_TYPE, '
      + ' I.RDB$SEGMENT_COUNT ORDER BY 2,3,4';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNullByName('TABLE_CAT');
        Result.UpdateNullByName('TABLE_SCHEM');
        Result.UpdateStringByName('TABLE_NAME',
          GetStringByName('RDB$RELATION_NAME'));
        Result.UpdateBooleanByName('NON_UNIQUE',
          not GetBooleanByName('RDB$UNIQUE_FLAG'));
        Result.UpdateNullByName('INDEX_QUALIFIER');
        Result.UpdateStringByName('INDEX_NAME',
          GetStringByName('RDB$INDEX_NAME'));
        Result.UpdateIntByName('TYPE', Ord(ntNoNulls));
        Result.UpdateIntByName('ORDINAL_POSITION',
          GetIntByName('RDB$FIELD_POSITION') + 1);
        Result.UpdateStringByName('COLUMN_NAME',
          GetStringByName('RDB$FIELD_NAME'));
        Result.UpdateNullByName('ASC_OR_DESC');
        Result.UpdateNullByName('CARDINALITY');
        Result.UpdateIntByName('PAGES',
          GetIntByName('RDB$SEGMENT_COUNT'));
        Result.UpdateNullByName('FILTER_CONDITION');
        Result.InsertRow;
      end;
      Close;
    end;

    AddResultSetToCache(Key, Result);
  end;
end;

function TZInterbase6DatabaseMetadata.GetSequences(Catalog, SchemaPattern,
  SequenceNamePattern: string): IZResultSet;
var
  Key: string;
  SQL: string;
begin
  Key := Format('get-sequences:%s:%s:%s',
    [Catalog, SchemaPattern, SequenceNamePattern]);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := ConstructVirtualResultSet(SequenceColumnsDynArray);

    SequenceNamePattern := ConstructNameCondition(SequenceNamePattern,
      'RDB$GENERATOR_NAME');

    SQL := ' SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS ' +
      'WHERE RDB$SYSTEM_FLAG IS NULL';

    if SequenceNamePattern <> '' then
      SQL := SQL + ' AND ' + SequenceNamePattern;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateNull(2);
        Result.UpdateString(3, GetStringByName('RDB$GENERATOR_NAME'));
        Result.InsertRow;
      end;
      Close;
    end;
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsResultSetType(
  _Type: TZResultSetType): Boolean;
begin
  Result := _Type = rtScrollInsensitive;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseMetadata.SupportsResultSetConcurrency(
  _Type: TZResultSetType; Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtScrollInsensitive) and (Concurrency = rcReadOnly);
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
function TZInterbase6DatabaseMetadata.GetUDTs(Catalog: string;
  SchemaPattern: string; TypeNamePattern: string;
  Types: TIntegerDynArray): IZResultSet;
begin
  Result := inherited GetUDTs(Catalog, SchemaPattern, TypeNamePattern, Types);
end;

{**
  Gets a privilege name.
  @param  Interbase privilege name
  @returns a JDBC privilege name.
}
function TZInterbase6DatabaseMetadata.GetPrivilege(Privilege: string): string;
begin
  if Privilege = 'S' then
    Result := 'SELECT'
  else if Privilege = 'I' then
    Result := 'INSERT'
  else if Privilege = 'U' then
    Result := 'UPDATE'
  else if Privilege = 'D' then
    Result := 'DELETE'
  else if Privilege = 'R' then
    Result := 'REFERENCE'
  else Result := '';
end;

{**
   Takes a name patternand column name and retuen an appropriate SQL clause
    @param Pattern a sql pattren
    @parma Column a sql column name
    @return processed string for query
}
function TZInterbase6DatabaseMetadata.ConstructNameCondition(
  Pattern, Column: string): string;
const
  Spaces = '';
var
  StrippedPattern: string;
begin
  if (Length(Pattern) > 2 * 31) then
    raise EZSQLException.Create(SPattern2Long);

  if (Pattern = '%') or (Pattern = '') then Exit;

  if HasNoWildcards(Pattern) then
  begin
    StrippedPattern := StripEscape(Pattern);
    Result := Format('%s = ''%s''', [Column, StrippedPattern]);
  end
  else
  begin
    StrippedPattern := StripEscape(Pattern);
    Result := Format('%s || ''%s'' like ''%s%s%%''',
      [Column, Spaces, StrippedPattern, Spaces]);
  end;
end;

{**
   Check what pattern do not contain wildcards
   @param Pattern a sql pattern
   @return if pattern contain wildcards return true otherwise false
}
function TZInterbase6DatabaseMetadata.HasNoWildcards(
  const Pattern: string): Boolean;
var
  I: Integer;
  PreviousChar: string[1];
  PreviousCharWasEscape: Boolean;
begin
  Result := False;
  PreviousChar := '';
  PreviousCharWasEscape := False;
  for I := 1 to Length(Pattern) do
  begin
    if not PreviousCharWasEscape and
       ((Pattern[I] = '_') or (Pattern[I] = '%')) then
     Exit;

    PreviousCharWasEscape := (Pattern[I] = '\') and (PreviousChar = '\');
    PreviousChar := Pattern[I];
  end;
  Result := True;
end;

{**
   Remove escapes from pattren string
   @param Pattern a sql pattern
   @return string without escapes
}
function TZInterbase6DatabaseMetadata.StripEscape(
  const Pattern: string): string;
var
  I: Integer;
  PreviousChar: string[1];
begin
  PreviousChar := '';
  for I := 1 to Length(Pattern) do
  begin
    if (Pattern <> '\') and (PreviousChar <> '\') then
    begin
      Result := Result + Pattern[I];
      PreviousChar := Pattern[I];
    end;
  end;
end;

{**
  Gets the version of the server.
  @returns the version of the server.
}
function TZInterbase6DatabaseMetadata.GetServerVersion: string;
begin
  if FServerVersion = '' then
  begin
    FServerVersion := GetVersion(FIBConnection.GetPlainDriver,
      FIBConnection.GetDBHandle);
  end;
  Result := FServerVersion;
end;

end.

