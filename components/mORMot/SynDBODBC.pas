/// ODBC 3.x library direct access classes to be used with our SynDB architecture
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynDBODBC;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.16
  - first public release, corresponding to mORMot Framework 1.16

  Version 1.17
  - initial working code, tested with ODBC Oracle provider

  TODO:
  - implement array binding of parameters
    http://msdn.microsoft.com/en-us/library/windows/desktop/ms709287
  - implement row-wise binding when all columns are inlined 
    http://msdn.microsoft.com/en-us/library/windows/desktop/ms711730

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
{$ifndef DELPHI5OROLDER}
  Variants,
{$endif}
  Classes,
  Contnrs,
  SynCommons,
  SynDB;


{ -------------- TODBC* classes and types implementing an ODBC library connection  }

type
  /// generic Exception type, generated for ODBC connection
  EODBCException = class(ESQLDBException);

  /// will implement properties shared by the ODBC library
  TODBCConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    /// this overriden method will retrieve the kind of DBMS from the main connection 
    function GetDBMS: TSQLDBDefinition; override;
  public
    /// initialize the connection properties
    // - will raise an exception if the ODBC library is not available
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overriden method will create an TODBCConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// get all table names
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined
    procedure GetTableNames(var Tables: TRawUTF8DynArray); override;
    /// retrieve the column/field layout of a specified table
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined
    procedure GetFields(const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray); override;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
  end;

  /// implements a direct connection to the ODBC library
  TODBCConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fODBCProperties: TODBCConnectionProperties;
    fEnv: pointer;
    fDbc: pointer;
    fDBMS: TSQLDBDefinition;  
    fDBMSName, fDBMSVersion: RawUTF8;
  public
    /// connect to a specified ODBC database
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the ODBC library, i.e. create the DB instance
    // - should raise an Exception on error
    procedure Connect; override;
    /// stop connection to the ODBC library, i.e. release the DB instance
    // - should raise an Exception on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;

    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    // - current implementation do not support nested transaction with those
    // methods: exception will be raised in such case
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;

    /// the remote DBMS type, as retrieved at ODBC connection opening
    property DBMS: TSQLDBDefinition read fDBMS;
  published
    /// the remote DBMS name, as retrieved at ODBC connection opening
    property DBMSName: RawUTF8 read fDBMSName;
    /// the remote DBMS version, as retrieved at ODBC connection opening
    property DBMSVersion: RawUTF8 read fDBMSVersion;
  end;

  /// implements a statement using a ODBC connection
  TODBCStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fStatement: pointer;
    fColTmp: pointer;
    fColTmpLen: cardinal;
    fGetColIndicator: PtrInt; // as PtrInt=SqlLen
    fGetColStatus: SmallInt; // as SqlReturn
    fSQLW: RawUnicode;
    procedure AllocStatement;
    procedure BindColumns;
    function GetCol(Col: integer; ExpectedType: TSQLDBFieldType): TSQLDBStatementGetCol;
    function GetUpdateCount: integer; override;
  public
    {{ create a ODBC statement instance, from an existing ODBC connection
     - the Execute method can be called once per TODBCStatement instance,
       but you can use the Prepare once followed by several ExecutePrepared methods
     - if the supplied connection is not of TOleDBConnection type, will raise
       an exception }
    constructor Create(aConnection: TSQLDBConnection); override;
    {{ release all associated memory and ODBC handles }
    destructor Destroy; override;

    {{ Prepare an UTF-8 encoded SQL statement
     - parameters marked as ? will be bound later, before ExecutePrepared call
     - if ExpectResults is TRUE, then Step() and Column*() methods are available
       to retrieve the data rows
     - raise an ESQLDBException on any error }
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: Boolean=false); overload; override;
    {{ Execute a prepared SQL statement
     - parameters marked as ? should have been already bound with Bind*() functions
     - raise an ESQLDBException on any error }
    procedure ExecutePrepared; override;

    {/ After a statement has been prepared via Prepare() + ExecutePrepared() or
       Execute(), this method must be called one or more times to evaluate it
     - you shall call this method before calling any Column*() methods
     - return TRUE on success, with data ready to be retrieved by Column*()
     - return FALSE if no more row is available (e.g. if the SQL statement
      is not a SELECT but an UPDATE or INSERT command)
     - access the first or next row of data from the SQL Statement result:
       if SeekFirst is TRUE, will put the cursor on the first row of results,
       otherwise, it will fetch one row of data, to be called within a loop
     - raise an EODBCException exception on any error }
    function Step(SeekFirst: boolean=false): boolean; override;
    {{ returns TRUE if the column contains NULL }
    function ColumnNull(Col: integer): boolean; override;
    {{ return a Column integer value of the current Row, first Col is 0 }
    function ColumnInt(Col: integer): Int64; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDouble(Col: integer): double; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDateTime(Col: integer): TDateTime; override;
    {{ return a Column currency value of the current Row, first Col is 0
     - should retrieve directly the 64 bit Currency content, to avoid
     any rounding/conversion error from floating-point types }
    function ColumnCurrency(Col: integer): currency; override;
    {{ return a Column UTF-8 encoded text value of the current Row, first Col is 0 }
    function ColumnUTF8(Col: integer): RawUTF8; override;
    {{ return a Column as a blob value of the current Row, first Col is 0
    - ColumnBlob() will return the binary content of the field is was not ftBlob,
      e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
      or a direct mapping of the RawUnicode  }
    function ColumnBlob(Col: integer): RawByteString; override;
    {{ append all columns values of the current Row to a JSON stream
     - will use WR.Expand to guess the expected output format
     - fast overriden implementation with no temporary variable
     - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
       format and contains true BLOB data }
    procedure ColumnsToJSON(WR: TJSONWriter); override;
  end;


implementation


{ -------------- ODBC library interfaces, constants and types }

const
  SQL_NULL_DATA = -1;
  SQL_DATA_AT_EXEC = -2;
  SQL_NO_TOTAL = -4;

  // return values from functions
  SQL_SUCCESS = 0;
  SQL_SUCCESS_WITH_INFO = 1;

  SQL_NO_DATA = 100;

  SQL_PARAM_TYPE_UNKNOWN = 0;
  SQL_PARAM_INPUT = 1;
  SQL_PARAM_INPUT_OUTPUT = 2;
  SQL_RESULT_COL = 3;
  SQL_PARAM_OUTPUT = 4;
  SQL_RETURN_VALUE = 5;
  SQL_PARAM_DATA_AVAILABLE = 101;

  SQL_ERROR = (-1);
  SQL_INVALID_HANDLE = (-2);

  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;

  // flags for null-terminated string
  SQL_NTS = (-3);
  SQL_NTSL = (-3);

  // maximum message length
  SQL_MAX_MESSAGE_LENGTH = 512;

  // date/time length constants
  SQL_DATE_LEN = 10;
  // add P+1 if precision is nonzero
  SQL_TIME_LEN = 8;
  // add P+1 if precision is nonzero
  SQL_TIMESTAMP_LEN = 19;

  // handle type identifiers
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

  // env attribute
  SQL_ATTR_ODBC_VERSION = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH = 202;
  SQL_ATTR_OUTPUT_NTS = 10001;
  SQL_OV_ODBC3 = pointer(3);

  // values for SQLStatistics()
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL = 1;
  SQL_QUICK = 0;
  SQL_ENSURE = 1;

  // connection attributes
  SQL_ACCESS_MODE = 101;
  SQL_AUTOCOMMIT = 102;
  SQL_LOGIN_TIMEOUT = 103;
  SQL_OPT_TRACE = 104;
  SQL_OPT_TRACEFILE = 105;
  SQL_TRANSLATE_DLL = 106;
  SQL_TRANSLATE_OPTION = 107;
  SQL_TXN_ISOLATION = 108;
  SQL_CURRENT_QUALIFIER = 109;
  SQL_ODBC_CURSORS = 110;
  SQL_QUIET_MODE = 111;
  SQL_PACKET_SIZE = 112;
  SQL_ATTR_AUTO_IPD = 10001;
  SQL_ATTR_METADATA_ID = 10014;

  // statement attributes
  SQL_ATTR_APP_ROW_DESC = 10010;
  SQL_ATTR_APP_PARAM_DESC = 10011;
  SQL_ATTR_IMP_ROW_DESC = 10012;
  SQL_ATTR_IMP_PARAM_DESC = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE = (-1);
  SQL_ATTR_CURSOR_SENSITIVITY = (-2);

  // SQL_ATTR_CURSOR_SCROLLABLE values
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE = 1;

	// SQL_AUTOCOMMIT options
  SQL_AUTOCOMMIT_OFF = pointer(0);
  SQL_AUTOCOMMIT_ON = pointer(1);

  // identifiers of fields in the SQL descriptor
  SQL_DESC_COUNT = 1001;
  SQL_DESC_TYPE = 1002;
  SQL_DESC_LENGTH = 1003;
  SQL_DESC_OCTET_LENGTH_PTR = 1004;
  SQL_DESC_PRECISION = 1005;
  SQL_DESC_SCALE = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE = 1008;
  SQL_DESC_INDICATOR_PTR = 1009;
  SQL_DESC_DATA_PTR = 1010;
  SQL_DESC_NAME = 1011;
  SQL_DESC_UNNAMED = 1012;
  SQL_DESC_OCTET_LENGTH = 1013;
  SQL_DESC_ALLOC_TYPE = 1099;

  // identifiers of fields in the diagnostics area
  SQL_DIAG_RETURNCODE = 1;
  SQL_DIAG_NUMBER = 2;
  SQL_DIAG_ROW_COUNT = 3;
  SQL_DIAG_SQLSTATE = 4;
  SQL_DIAG_NATIVE = 5;
  SQL_DIAG_MESSAGE_TEXT = 6;
  SQL_DIAG_DYNAMIC_FUNCTION = 7;
  SQL_DIAG_CLASS_ORIGIN = 8;
  SQL_DIAG_SUBCLASS_ORIGIN = 9;
  SQL_DIAG_CONNECTION_NAME = 10;
  SQL_DIAG_SERVER_NAME = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

  // SQL data type codes
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR = 1;
  SQL_NUMERIC = 2;
  SQL_DECIMAL = 3;
  SQL_INTEGER = 4;
  SQL_SMALLINT = 5;
  SQL_FLOAT = 6;
  SQL_REAL = 7;
  SQL_DOUBLE = 8;
  SQL_DATETIME = 9;
  SQL_DATE = 9;
  SQL_INTERVAL = 10;
  SQL_TIME = 10;
  SQL_TIMESTAMP = 11;
  SQL_VARCHAR = 12;
  SQL_LONGVARCHAR = -1;
  SQL_BINARY = -2;
  SQL_VARBINARY = -3;
  SQL_LONGVARBINARY = -4;
  SQL_BIGINT = -5;
  SQL_TINYINT = -6;
  SQL_BIT = -7;
  SQL_WCHAR = -8;
  SQL_WVARCHAR = -9;
  SQL_WLONGVARCHAR = -10;
  SQL_GUID = -11;

  // One-parameter shortcuts for date/time data types
  SQL_TYPE_DATE = 91;
  SQL_TYPE_TIME = 92;
  SQL_TYPE_TIMESTAMP = 93;

  // C datatype to SQL datatype mapping
  SQL_C_CHAR = SQL_CHAR;
  SQL_C_WCHAR = SQL_WCHAR;
  SQL_C_LONG = SQL_INTEGER;
  SQL_C_SHORT = SQL_SMALLINT;
  SQL_C_FLOAT = SQL_REAL;
  SQL_C_DOUBLE = SQL_DOUBLE;
  SQL_C_NUMERIC = SQL_NUMERIC;
  SQL_C_DEFAULT = 99;
  SQL_SIGNED_OFFSET = (-20);
  SQL_UNSIGNED_OFFSET = (-22);
  SQL_C_DATE = SQL_DATE;
  SQL_C_TIME = SQL_TIME;
  SQL_C_TIMESTAMP = SQL_TIMESTAMP;
  SQL_C_TYPE_DATE = SQL_TYPE_DATE;
  SQL_C_TYPE_TIME = SQL_TYPE_TIME;
  SQL_C_TYPE_TIMESTAMP = SQL_TYPE_TIMESTAMP;
  SQL_C_BINARY = SQL_BINARY;
  SQL_C_BIT = SQL_BIT;
  SQL_C_SBIGINT = (SQL_BIGINT+SQL_SIGNED_OFFSET);
  SQL_C_UBIGINT = (SQL_BIGINT+SQL_UNSIGNED_OFFSET);
  SQL_C_TINYINT = SQL_TINYINT;
  SQL_C_SLONG = (SQL_C_LONG+SQL_SIGNED_OFFSET);
  SQL_C_SSHORT = (SQL_C_SHORT+SQL_SIGNED_OFFSET);
  SQL_C_STINYINT = (SQL_TINYINT+SQL_SIGNED_OFFSET);
  SQL_C_ULONG = (SQL_C_LONG+SQL_UNSIGNED_OFFSET);
  SQL_C_USHORT = (SQL_C_SHORT+SQL_UNSIGNED_OFFSET);
  SQL_C_UTINYINT = (SQL_TINYINT+SQL_UNSIGNED_OFFSET);

  // Statement attribute values for cursor sensitivity
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE = 2;

  // GetTypeInfo() request for all data types
  SQL_ALL_TYPES = 0;

  // Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()
  SQL_DEFAULT = 99;

  // SQLSQLLEN GetData() code indicating that the application row descriptor
  // specifies the data type
  SQL_ARD_TYPE = (-99);

  SQL_APD_TYPE = (-100);

  // SQL date/time type subcodes
  SQL_CODE_DATE = 1;
  SQL_CODE_TIME = 2;
  SQL_CODE_TIMESTAMP = 3;

  // CLI option values
  SQL_FALSE = 0;
  SQL_TRUE = 1;

  // values of NULLABLE field in descriptor
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;

  // Value returned by SQLGetTypeInfo() to denote that it is
  // not known whether or not a data type supports null values.
  SQL_NULLABLE_UNKNOWN = 2;

  // Values returned by SQLGetTypeInfo() to show WHERE clause supported
  SQL_PRED_NONE = 0;
  SQL_PRED_CHAR = 1;
  SQL_PRED_BASIC = 2;

  // values of UNNAMED field in descriptor
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

  // values of ALLOC_TYPE field in descriptor
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

  // FreeStmt() options
  SQL_CLOSE = 0;
  SQL_DROP = 1;
  SQL_UNBIND = 2;
  SQL_RESET_PARAMS = 3;

  // Codes used for FetchOrientation in SQLFetchScroll() and SQLDataSources()
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

  // Other codes used for FetchOrientation in SQLFetchScroll()
  SQL_FETCH_LAST = 3;
  SQL_FETCH_PRIOR = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;

  // SQLEndTran() options
  SQL_COMMIT = 0;
  SQL_ROLLBACK = 1;

  // null handles returned by SQLAllocHandle()
  SQL_NULL_HENV = 0;
  SQL_NULL_HDBC = 0;
  SQL_NULL_HSTMT = 0;
  SQL_NULL_HDESC = 0;

  // null handle used in place of parent handle when allocating HENV
  SQL_NULL_HANDLE = nil;

// Information requested by SQLGetInfo()
  SQL_MAX_DRIVER_CONNECTIONS = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME = 2;
  SQL_FETCH_DIRECTION = 8;
  SQL_SERVER_NAME = 13;
  SQL_SEARCH_PATTERN_ESCAPE = 14;
  SQL_DBMS_NAME = 17;
  SQL_DBMS_VER = 18;
  SQL_ACCESSIBLE_TABLES = 19;
  SQL_ACCESSIBLE_PROCEDURES = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR = 23;
  SQL_DATA_SOURCE_READ_ONLY = 25;
  SQL_DEFAULT_TXN_ISOLATION = 26;
  SQL_IDENTIFIER_CASE = 28;
  SQL_IDENTIFIER_QUOTE_CHAR = 29;
  SQL_MAX_COLUMN_NAME_LEN = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN = 35;
  SQL_SCROLL_CONCURRENCY = 43;
  SQL_TXN_CAPABLE = 46;
  SQL_TRANSACTION_CAPABLE = SQL_TXN_CAPABLE;
  SQL_USER_NAME = 47;
  SQL_TXN_ISOLATION_OPTION = 72;
  SQL_TRANSACTION_ISOLATION_OPTION = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY = 73;
  SQL_GETDATA_EXTENSIONS = 81;
  SQL_NULL_COLLATION = 85;
  SQL_ALTER_TABLE = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
  SQL_SPECIAL_CHARACTERS = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE = 101;
  SQL_MAX_INDEX_SIZE = 102;
  SQL_MAXIMUM_INDEX_SIZE = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE = 104;
  SQL_MAXIMUM_ROW_SIZE = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES = 115;
  SQL_OUTER_JOIN_CAPABILITIES = SQL_OJ_CAPABILITIES;


type
  SqlSmallint = Smallint;
  SqlDate = Byte;
  SqlTime = Byte;
  SqlDecimal = Byte;
  SqlDouble = Double;
  SqlFloat = Double;
  SqlInteger = integer;
  SqlUInteger = cardinal;
  SqlNumeric = Byte;
  SqlPointer = Pointer;
  SqlReal = Single;
  SqlUSmallint = Word;
  SqlTimestamp = Byte;
  SqlVarchar = Byte;
  PSqlSmallint = ^SqlSmallint;
  PSqlInteger = ^SqlInteger;

  SqlReturn = SqlSmallint;
  SqlLen = PtrInt;
  SqlULen = PtrUInt;
  {$ifdef CPU64}
  SqlSetPosIRow = PtrUInt;
  {$else}
  SqlSetPosIRow = Word;
  {$endif}
  PSqlLen = ^SqlLen;

  SqlHandle = Pointer;
  SqlHEnv = SqlHandle;
  SqlHDbc = SqlHandle;
  SqlHStmt = SqlHandle;
  SqlHDesc = SqlHandle;

  {$A-}
  /// memory structure used to store SQL_C_TYPE_TIMESTAMP values
  SQL_TIMESTAMP_STRUCT = {$ifndef UNICODE}object{$else}record{$endif}
    Year:     SqlSmallint;
    Month:    SqlUSmallint;
    Day:      SqlUSmallint;
    Hour:     SqlUSmallint;
    Minute:   SqlUSmallint;
    Second:   SqlUSmallint;
    Fraction: SqlUInteger;
    /// convert an ODBC date and time into Delphi TDateTime
    // - depending on the original column data type specified, it will return
    // either a TDate (for SQL_TYPE_DATE), either a TTime (for SQL_TYPE_TIME),
    // either a TDateTime content (for SQL_TYPE_TIMESTAMP)
    function ToDateTime(DataType: SqlSmallint=SQL_TYPE_TIMESTAMP): TDateTime;
    /// convert an ODBC date and time into its textual expanded ISO-8601
    // - will fill up to 21 characters, including double quotes
    // - depending on the column data type specified, it will return either an
    // ISO-8601 date (for SQL_TYPE_DATE), either a time (for SQL_TYPE_TIME),
    // either a full date+time ISO-8601 content (for SQL_TYPE_TIMESTAMP)
    function ToIso8601(Dest: PUTF8Char; DataType: SqlSmallint): integer;
    /// convert a TDateTime into ODBC date or timestamp
    // - returns the corresponding C type, i.e. either SQL_C_TYPE_DATE,
    // either SQL_C_TYPE_TIMESTAMP
    function From(DateTime: TDateTime): SqlSmallint;
  end;
  SQL_TIME_STRUCT	= record
    Hour:     SqlUSmallint;
    Minute:   SqlUSmallint;
    Second:   SqlUSmallint;
  end;
  SQL_DATE_STRUCT	= record
    year:	SQLSMALLINT;
    month:	SQLUSMALLINT;
    day:	SQLUSMALLINT;
  end;
  {$A+}
  PSQL_TIMESTAMP_STRUCT = ^SQL_TIMESTAMP_STRUCT;

  /// direct access to the ODBC library
  // - this wrapper will initialize both Ansi and Wide versions of the ODBC
  // driver functions, and will work with 32 bit and 64 bit version of the
  // interfaces, on Windows or POSIX platforms
  // - within this unit, we will only use Wide version, and UTF-8 conversion
  TODBCLib = class(TSQLDBLib)
  protected
    procedure HandleError(Status: SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle;
      InfoRaiseException: Boolean; LogLevelNoRaise: TSynLogInfo);
  public
    AllocEnv: function (var EnvironmentHandle: SqlHEnv): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    AllocHandle: function(HandleType: SqlSmallint; InputHandle: SqlHandle;
      var OutputHandle: SqlHandle): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    AllocStmt: function(ConnectionHandle: SqlHDbc; var StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    BindCol: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      TargetType: SqlSmallint; TargetValue: SqlPointer;
      BufferLength: SqlLen; StrLen_or_Ind: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    BindParameter: function (StatementHandle: SqlHStmt; ParameterNumber: SqlUSmallint;
      InputOutputType, ValueType, ParameterType: SqlSmallint; ColumnSize: SqlULen;
      DecimalDigits: SqlSmallint; ParameterValue: SqlPointer; BufferLength: SqlLen;
      var StrLen_or_Ind: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Cancel: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    CloseCursor: function(StatementHandle: SqlHStmt): SqlReturn;
     {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColAttributeA: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      FieldIdentifier: SqlUSmallint; CharacterAttribute: PAnsiChar;
      BufferLength: SqlSmallint; StringLength: PSqlSmallint; NumericAttributePtr: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColAttributeW: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      FieldIdentifier: SqlUSmallint; CharacterAttribute: PWideChar;
      BufferLength: SqlSmallint; StringLength: PSqlSmallint; NumericAttributePtr: PSqlLen): SqlReturn;
       {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColumnsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      TableName: PAnsiChar;   NameLength3: SqlSmallint;
      ColumnName: PAnsiChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColumnsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      TableName: PWideChar;   NameLength3: SqlSmallint;
      ColumnName: PWideChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    StatisticsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      TableName: PAnsiChar;   NameLength3: SqlSmallint;
      Unique, Reserved: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    StatisticsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      TableName: PWideChar;   NameLength3: SqlSmallint;
      Unique, Reserved: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ConnectA: function(ConnectionHandle: SqlHDbc;
      ServerName: PAnsiChar; NameLength1: SqlSmallint;
      UserName: PAnsiChar; NameLength2: SqlSmallint;
      Authentication: PAnsiChar; NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ConnectW: function(ConnectionHandle: SqlHDbc;
      ServerName: PWideChar; NameLength1: SqlSmallint;
      UserName: PWideChar; NameLength2: SqlSmallint;
      Authentication: PWideChar; NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    CopyDesc: function(SourceDescHandle, TargetDescHandle: SqlHDesc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DataSourcesA: function(EnvironmentHandle: SqlHEnv; Direction: SqlUSmallint;
      ServerName: PAnsiChar;  BufferLength1: SqlSmallint; var NameLength1: SqlSmallint;
      Description: PAnsiChar; BufferLength2: SqlSmallint; var NameLength2: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DataSourcesW: function(EnvironmentHandle: SqlHEnv; Direction: SqlUSmallint;
      ServerName: PWideChar;  BufferLength1: SqlSmallint; var NameLength1: SqlSmallint;
      Description: PWideChar; BufferLength2: SqlSmallint; var NameLength2: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DescribeColA: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      ColumnName: PAnsiChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint;
      var DataType: SqlSmallint; var ColumnSize: SqlULen; var DecimalDigits: SqlSmallint;
      var Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DescribeColW: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      ColumnName: PWideChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint;
      var DataType: SqlSmallint; var ColumnSize: SqlULen; var DecimalDigits: SqlSmallint;
      var Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Disconnect: function(ConnectionHandle: SqlHDbc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    EndTran: function(HandleType: SqlSmallint; Handle: SqlHandle;
      CompletionType: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ErrorA: function(EnvironmentHandle: SqlHEnv; ConnectionHandle: SqlHDbc; StatementHandle: SqlHStmt;
      Sqlstate: PAnsiChar; var NativeError: SqlInteger;
      MessageText: PAnsiChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ErrorW: function(EnvironmentHandle: SqlHEnv; ConnectionHandle: SqlHDbc; StatementHandle: SqlHStmt;
      Sqlstate: PWideChar; var NativeError: SqlInteger;
      MessageText: PWideChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ExecDirectA: function(StatementHandle: SqlHStmt;
      StatementText: PAnsiChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ExecDirectW: function(StatementHandle: SqlHStmt;
      StatementText: PWideChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Execute: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Fetch: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FetchScroll: function(StatementHandle: SqlHStmt;
      FetchOrientation: SqlSmallint; FetchOffset: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeConnect: function(ConnectionHandle: SqlHDbc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeEnv: function(EnvironmentHandle: SqlHEnv): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeHandle: function(HandleType: SqlSmallint; Handle: SqlHandle): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeStmt: function(StatementHandle: SqlHStmt; Option: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetConnectAttrA: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; BufferLength: SqlInteger; pStringLength: pSqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetConnectAttrW: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; BufferLength: SqlInteger; pStringLength: pSqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetCursorNameA: function(StatementHandle: SqlHStmt;
      CursorName: PAnsiChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetCursorNameW: function(StatementHandle: SqlHStmt;
      CursorName: PWideChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetData: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      TargetType: SqlSmallint; TargetValue: SqlPointer; BufferLength: SqlLen;
      StrLen_or_Ind: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescFieldA: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      FieldIdentifier: SqlSmallint; Value: SqlPointer; BufferLength: SqlInteger;
      var StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescFieldW: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      FieldIdentifier: SqlSmallint; Value: SqlPointer; BufferLength: SqlInteger;
      var StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescRecA: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      Name: PAnsiChar; BufferLength: SqlSmallint; var StringLength: SqlSmallint;
      var _Type, SubType: SqlSmallint; var Length: SqlLen;
      var Precision, Scale, Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescRecW: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      Name: PWideChar; BufferLength: SqlSmallint; var StringLength: SqlSmallint;
      var _Type, SubType: SqlSmallint; var Length: SqlLen;
      var Precision, Scale, Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagFieldA: function(HandleType: SqlSmallint; Handle: SqlHandle;
      RecNumber, DiagIdentifier: SqlSmallint;
      DiagInfo: SqlPointer; BufferLength: SqlSmallint; var StringLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagFieldW: function(HandleType: SqlSmallint; Handle: SqlHandle;
      RecNumber, DiagIdentifier: SqlSmallint;
      DiagInfo: SqlPointer; BufferLength: SqlSmallint; var StringLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagRecA: function(HandleType: SqlSmallint; Handle: SqlHandle; RecNumber: SqlSmallint;
      Sqlstate: PAnsiChar; var NativeError: SqlInteger;
      MessageText: PAnsiChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagRecW: function(HandleType: SqlSmallint; Handle: SqlHandle; RecNumber: SqlSmallint;
      Sqlstate: PWideChar; var NativeError: SqlInteger;
      MessageText: PWideChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    PrepareA: function(StatementHandle: SqlHStmt;
      StatementText: PAnsiChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    PrepareW: function(StatementHandle: SqlHStmt;
      StatementText: PWideChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    RowCount: function(StatementHandle: SqlHStmt; var RowCount: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    NumResultCols: function(StatementHandle: SqlHStmt; var ColumnCount: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetInfoA: function(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      InfoValuePtr: SqlPointer; BufferLength: SqlSmallint; StringLengthPtr: PSqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetInfoW: function(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      InfoValuePtr: SqlPointer; BufferLength: SqlSmallint; StringLengthPtr: PSqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetStmtAttrA: function(StatementHandle: SqlHStmt; Attribute: SqlInteger;
      Value: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetStmtAttrW: function(StatementHandle: SqlHStmt; Attribute: SqlInteger;
      Value: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetEnvAttr: function(EnvironmentHandle: SqlHEnv; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetConnectAttrA: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetConnectAttrW: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    TablesA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar; NameLength2: SqlSmallint;
      TableName: PAnsiChar; NameLength3: SqlSmallint;
      TableType: PAnsiChar; NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    TablesW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar; NameLength2: SqlSmallint;
      TableName: PWideChar; NameLength3: SqlSmallint;
      TableType: PWideChar; NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ForeignKeysA: function(StatementHandle: SqlHStmt;
      PKCatalogName: PAnsiChar; NameLength1: SqlSmallint;
      PKSchemaName: PAnsiChar; NameLength2: SqlSmallint;
      PKTableName: PAnsiChar; NameLength3: SqlSmallint;
      FKCatalogName: PAnsiChar; NameLength4: SqlSmallint;
      FKSchemaName: PAnsiChar; NameLength5: SqlSmallint;
      FKTableName: PAnsiChar; NameLength6: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ForeignKeysW: function(StatementHandle: SqlHStmt;
      PKCatalogName: PWideChar; NameLength1: SqlSmallint;
      PKSchemaName: PWideChar; NameLength2: SqlSmallint;
      PKTableName: PWideChar; NameLength3: SqlSmallint;
      FKCatalogName: PWideChar; NameLength4: SqlSmallint;
      FKSchemaName: PWideChar; NameLength5: SqlSmallint;
      FKTableName: PWideChar; NameLength6: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
  public
    /// load the ODBC library
    // - and retrieve all SQL*() addresses for ODBC_ENTRIES[] items
    constructor Create;
    /// raise an exception on error
    procedure Check(Status: SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle; 
      InfoRaiseException: Boolean=false; LogLevelNoRaise: TSynLogInfo=sllNone);
      {$ifdef HASINLINE} inline; {$endif}
  end;

const
  ODBC_ENTRIES: array[0..60] of PChar =
    ('SQLAllocEnv','SQLAllocHandle','SQLAllocStmt',
     'SQLBindCol','SQLBindParameter','SQLCancel','SQLCloseCursor',
     'SQLColAttribute','SQLColAttributeW','SQLColumns','SQLColumnsW',
     'SQLStatistics','SQLStatisticsW','SQLConnect','SQLConnectW',
     'SQLCopyDesc','SQLDataSources','SQLDataSourcesW',
     'SQLDescribeCol','SQLDescribeColW','SQLDisconnect','SQLEndTran',
     'SQLError','SQLErrorW','SQLExecDirect','SQLExecDirectW','SQLExecute',
     'SQLFetch','SQLFetchScroll','SQLFreeConnect','SQLFreeEnv','SQLFreeHandle',
     'SQLFreeStmt','SQLGetConnectAttr','SQLGetConnectAttrW',
     'SQLGetCursorName','SQLGetCursorNameW','SQLGetData',
     'SQLGetDescField','SQLGetDescFieldW','SQLGetDescRec','SQLGetDescRecW',
     'SQLGetDiagField','SQLGetDiagFieldW','SQLGetDiagRec','SQLGetDiagRecW',
     'SQLPrepare','SQLPrepareW','SQLRowCount','SQLNumResultCols',
     'SQLGetInfo','SQLGetInfoW','SQLSetStmtAttr','SQLSetStmtAttrW','SQLSetEnvAttr',
     'SQLSetConnectAttr','SQLSetConnectAttrW','SQLTables','SQLTablesW',
     'SQLForeignKeys','SQLForeignKeysW');

var
  ODBC: TODBCLib = nil;


{ TODBCConnection }

procedure TODBCConnection.Connect;
var Log: ISynLog;
    Len: SqlSmallint;
    Info: array[byte] of WideChar;
begin
  Log := SynDBLog.Enter;
  Disconnect; // force fDbc=nil
  if fEnv=nil then
    if (ODBC=nil) or (ODBC.AllocHandle(SQL_HANDLE_ENV,SQL_NULL_HANDLE,fEnv)=SQL_ERROR) then
      raise EODBCException.CreateFmt('%s: Unable to allocate an environment handle',[ClassName]);
  with ODBC do
  try
    Check(SetEnvAttr(fEnv,SQL_ATTR_ODBC_VERSION,SQL_OV_ODBC3,0),SQL_HANDLE_ENV,fEnv);
    Check(AllocHandle(SQL_HANDLE_DBC,fEnv,fDbc),SQL_HANDLE_ENV,fEnv);
    with fODBCProperties do
      Check(ConnectA(fDbc,pointer(fServerName),length(fServerName),
        pointer(fUserID),length(fUserID),pointer(fPassWord),length(fPassWord)),
        SQL_HANDLE_DBC,fDbc);
    Check(GetInfoW(fDbc,SQL_DBMS_NAME,@Info,sizeof(Info)shr 1,@Len),SQL_HANDLE_DBC,fDbc);
    fDBMSName := RawUnicodeToUtf8(Info,Len shr 1);
    fDBMS := dDefault;
    if fDBMSName<>'' then
      if IdemPChar(pointer(fDBMSName),'ORACLE') then
        fDBMS := dOracle else
      if IdemPChar(pointer(fDBMSName),'MICROSOFT SQL') then
        fDBMS := dMSSQL else
      if IdemPChar(pointer(fDBMSName),'ACCESS') then
        fDBMS := dJet else
      if IdemPChar(pointer(fDBMSName),'MYSQL') then
        fDBMS := dMySQL else
      if IdemPChar(pointer(fDBMSName),'SQLITE') then
        fDBMS := dSQLite;
    Check(GetInfoW(fDbc,SQL_DBMS_VER,@Info,sizeof(Info)shr 1,@Len),SQL_HANDLE_DBC,fDbc);
    fDBMSVersion := RawUnicodeToUtf8(Info,Len shr 1);
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      self.Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

constructor TODBCConnection.Create(aProperties: TSQLDBConnectionProperties);
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  if not aProperties.InheritsFrom(TODBCConnectionProperties) then
    raise EODBCException.CreateFmt('Invalid %s.Create',[ClassName]);
  Log.Log(sllDB,aProperties);
  fODBCProperties := TODBCConnectionProperties(aProperties);
  inherited;
end;

destructor TODBCConnection.Destroy;
begin
  inherited Destroy;
  if (ODBC<>nil) and (fEnv<>nil) then
    ODBC.FreeHandle(SQL_HANDLE_ENV,fEnv);
end;

procedure TODBCConnection.Disconnect;
begin
  if (ODBC<>nil) and (fDbc<>nil) then
  with ODBC do begin
    SynDBLog.Enter(self);
    Disconnect(fDbc);
    FreeHandle(SQL_HANDLE_DBC,fDbc);
    fDbc := nil;
  end;
end;

function TODBCConnection.IsConnected: boolean;
begin
  result := fDbc<>nil;
end;

function TODBCConnection.NewStatement: TSQLDBStatement;
begin
  result := TODBCStatement.Create(self);
end;

procedure TODBCConnection.Commit;
begin
  inherited Commit;
  with ODBC do begin
    Check(EndTran(SQL_HANDLE_DBC,fDBc,SQL_COMMIT),SQL_HANDLE_DBC,fDBc);
    Check(SetConnectAttrW(fDBc,SQL_AUTOCOMMIT,SQL_AUTOCOMMIT_ON,0),
      SQL_HANDLE_DBC,fDBc); // back to default AUTO COMMIT ON mode
  end;
end;

procedure TODBCConnection.Rollback;
begin
  inherited RollBack;
  with ODBC do begin
    Check(EndTran(SQL_HANDLE_DBC,fDBc,SQL_ROLLBACK),SQL_HANDLE_DBC,fDBc);
    Check(SetConnectAttrW(fDBc,SQL_AUTOCOMMIT,SQL_AUTOCOMMIT_ON,0),
      SQL_HANDLE_DBC,fDBc); // back to default AUTO COMMIT ON mode
  end;
end;

procedure TODBCConnection.StartTransaction;
begin
  if TransactionCount>0 then
    raise EODBCException.Create('TODBCConnection do not provide nested transactions');
  inherited StartTransaction;
  ODBC.Check(ODBC.SetConnectAttrW(fDBc,SQL_AUTOCOMMIT,SQL_AUTOCOMMIT_OFF,0),
    SQL_HANDLE_DBC,fDBc);
end;


{ TODBCStatement }

procedure TODBCStatement.AllocStatement;
var hDbc: SqlHDbc;
begin
  if fStatement<>nil then
    raise EODBCException.Create('AllocStatement called twice');
  fCurrentRow := 0;
  if not fConnection.Connected then
    fConnection.Connect;
  hDbc := (fConnection as TODBCConnection).fDbc;
  with ODBC do 
    Check(AllocHandle(SQL_HANDLE_STMT,hDBC,fStatement),SQL_HANDLE_DBC,hDBC);
end;

function ODBCColumnToFieldType(DataType, ColumnPrecision, ColumnScale: integer): TSQLDBFieldType;
begin // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob
  case DataType of
    SQL_DECIMAL, SQL_NUMERIC, SQL_FLOAT: begin
      result := ftDouble;
      if ColumnPrecision=10 then
        case ColumnScale of
        0:    result := ftInt64;
        1..4: result := ftCurrency;
        end;
    end;
    SQL_REAL, SQL_DOUBLE:
      result := ftDouble;
    SQL_SMALLINT, SQL_INTEGER, SQL_TINYINT, SQL_BIT, SQL_BIGINT:
      result := ftInt64;
    SQL_BINARY, SQL_VARBINARY, SQL_LONGVARBINARY:
      result := ftBlob;
    SQL_TIME, SQL_DATETIME,
    SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TYPE_TIMESTAMP:
      result := ftDate;
    else // all other types will be converted to text
      result := ftUtf8;
  end;
end;

const
  /// internal mapping to handled GetData() type for Column*() methods
  // - numerical values (integer or floating-point) are converted to SQL_C_CHAR
  // - date/time to SQL_C_TYPE_TIMESTAMP object
  // - text columns to SQL_C_WCHAR (for proper UTF-8 data retrieval)
  // - BLOB columns to SQL_C_BINARY
  ODBC_TYPE_TO: array[TSQLDBFieldType] of ShortInt = (
   SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR,
   SQL_C_TYPE_TIMESTAMP, SQL_C_WCHAR, SQL_C_BINARY);
   // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob

procedure TODBCStatement.BindColumns;
var nCols, NameLength, DataType, DecimalDigits, Nullable: SqlSmallint;
    ColumnSize, siz: SqlULen;
    c: integer;
    Name: array[byte] of WideChar;
begin
  if (fColumnCount>0) or (fColTmp<>nil) then
    raise EODBCException.Create('TODBCStatement.BindColumns twice');
  with ODBC do begin
    Check(NumResultCols(fStatement,nCols),SQL_HANDLE_STMT,fStatement);
    for c := 1 to nCols do begin
      Check(DescribeColW(fStatement,c,Name,256,NameLength,DataType,ColumnSize,
        DecimalDigits,Nullable),SQL_HANDLE_STMT,fStatement);
      with PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(
         RawUnicodeToUtf8(Name,NameLength)))^ do begin
        ColumnValueInlined := true;
        ColumnValueDBType := DataType;
        if ColumnSize>65535 then
          ColumnSize := 0; // avoid out of memory error for BLOBs
        ColumnValueDBSize := ColumnSize;
        ColumnNonNullable := (Nullable=SQL_NO_NULLS);
        ColumnType := ODBCColumnToFieldType(DataType,10,DecimalDigits);
        if ColumnType=ftUTF8 then
          siz := ColumnSize*2+2 else // guess max size as WideChar buffer
          siz := ColumnSize;
        if siz>fColTmpLen then
          fColTmpLen := siz;
      end;
    end;
    assert(fColumnCount=nCols);
    if fColTmpLen<63 then
      fColTmpLen := 64; /// ODBC never truncates fixed-length data: ensure enough
    GetMem(fColTmp,fColTmpLen);
  end;
end;

function TODBCStatement.GetCol(Col: integer; ExpectedType: TSQLDBFieldType): TSQLDBStatementGetCol;
const MINIMUM_CHUNK_SIZE = 65536;
begin // colNull, colWrongType, colTmpUsed, colTmpUsedTruncated
  CheckCol(Col); // check Col value
  if not Assigned(fStatement) or (fColumnCount=0) or (fColTmp=nil) then
    raise EODBCException.Create('TODBCStatement.Column*() with no prior Execute');
  with fColumns[Col] do
    if ColumnValueState=colNull then
      result := colNull else begin
    if (ColumnValueState=colTmpUsedTruncated) and (fColTmpLen<MINIMUM_CHUNK_SIZE) then begin
      FreeMem(fColTmp);
      GetMem(fColTmp,MINIMUM_CHUNK_SIZE);
      fColTmpLen := MINIMUM_CHUNK_SIZE;
    end;
    if ColumnValueState<>colWrongType then
      fGetColStatus := ODBC.GetData(fStatement,Col+1,ODBC_TYPE_TO[ColumnType],
        fColTmp,fColTmpLen,@fGetColIndicator);
    if ColumnType<>ExpectedType then begin
      if fGetColIndicator=SQL_NULL_DATA then
        result := colNull else
        result := colWrongType;
    end else begin
      if fGetColStatus<>SQL_SUCCESS then 
        if (fGetColStatus=SQL_SUCCESS_WITH_INFO) and
           (ColumnType in FIXEDLENGTH_SQLDBFIELDTYPE) then
          fGetColStatus := SQL_SUCCESS else // allow rounding problem
          ODBC.HandleError(fGetColStatus,SQL_HANDLE_STMT,fStatement,false,sllNone);
      case fGetColIndicator of
      SQL_NULL_DATA:
        result := colNull;
      SQL_NO_TOTAL:
        if ColumnType in [ftInt64, ftDouble, ftCurrency, ftDate] then
          result := colTmpUsed else
          raise EODBCException.CreateFmt('"%s" column returned no size',[ColumnName]);
      else
        if fGetColIndicator<0 then
          raise EODBCException.CreateFmt('"%s" column returned invalid size',[ColumnName]) else
          if fGetColStatus=SQL_SUCCESS then
            result := colTmpUsed else
            result := colTmpUsedTruncated;
      end;
    end;
    ColumnValueState := result;
  end;
end;

function TODBCStatement.ColumnBlob(Col: integer): RawByteString;
var res: TSQLDBStatementGetCol;
    offs: integer;
begin
  res := GetCol(Col,ftBlob);
  case res of
    colNull:      result := '';
    colWrongType: ColumnToTypedValue(Col,ftBlob,result);
    else begin
      SetString(result,PAnsiChar(fColTmp),fGetColIndicator);
      offs := 0;
      while res=colTmpUsedTruncated do begin
        inc(offs,fGetColIndicator);
        res := GetCol(Col,ftBlob); // get next chunk of data
        if fGetColIndicator<=0 then
          break;
        SetLength(result,offs+fGetColIndicator);
        move(fColTmp^,PByteArray(result)^[offs],fGetColIndicator);
      end;
    end;
  end;
end;

function TODBCStatement.ColumnUTF8(Col: integer): RawUTF8;
var res: TSQLDBStatementGetCol;
begin
  res := GetCol(Col,ftUTF8);
  case res of
    colNull:      result := '';
    colWrongType: ColumnToTypedValue(Col,ftUTF8,result);
    else begin
      result := RawUnicodeToUtf8(fColTmp,fGetColIndicator shr 1);
      while res=colTmpUsedTruncated do begin
        res := GetCol(Col,ftUTF8); // get next chunk of Unicode text
        if fGetColIndicator<=0 then
          break;
        result := result+RawUnicodeToUtf8(fColTmp,fGetColIndicator shr 1);
      end;
    end;
  end;
end;

function TODBCStatement.ColumnCurrency(Col: integer): currency;
begin
  case GetCol(Col,ftCurrency) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftCurrency,result);
    else PInt64(@result)^ := StrToCurr64(fColTmp); // encoded as SQL_C_CHAR
  end;
end;

function TODBCStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  case GetCol(Col,ftDate) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftDate,result);
    else result := PSQL_TIMESTAMP_STRUCT(fColTmp)^.ToDateTime(
      fColumns[Col].ColumnValueDBType);
  end;
end;

function TODBCStatement.ColumnDouble(Col: integer): double;
begin
  case GetCol(Col,ftDouble) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftDouble,result);
    else result := GetExtended(fColTmp); // encoded as SQL_C_CHAR
  end;
end;

function TODBCStatement.ColumnInt(Col: integer): Int64;
begin
  case GetCol(Col,ftInt64) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftInt64,result);
    else SetInt64(fColTmp,result); // encoded as SQL_C_CHAR
  end;
end;

function TODBCStatement.ColumnNull(Col: integer): boolean;
begin // ftUnknown will check for NULL but never returns data but colWrongType
  result := GetCol(Col,ftUnknown)=colNull;
end;

procedure TODBCStatement.ColumnsToJSON(WR: TJSONWriter);
var res: TSQLDBStatementGetCol;
    col, offs: integer;
    tmp: array[0..31] of AnsiChar;
    blob: RawByteString;
begin
  if not Assigned(fStatement) or (CurrentRow<=0) then
    raise EODBCException.Create('TODBCStatement.ColumnsToJSON() with no prior Step');
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do // fast direct conversion from OleDB buffer
  with fColumns[col] do begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    res := GetCol(Col,ColumnType);
    if res=colNull then
      WR.AddShort('null') else
      case ColumnType of
      ftInt64:
        WR.AddNoJSONEscape(fColTmp);  // already as SQL_C_CHAR
      ftDouble, ftCurrency:
        WR.AddFloatStr(fColTmp);      // already as SQL_C_CHAR
      ftDate:
        WR.AddNoJSONEscape(@tmp,
          PSQL_TIMESTAMP_STRUCT(fColTmp)^.ToIso8601(tmp,ColumnValueDBType));
      ftUTF8: begin
        WR.Add('"');
        if fGetColIndicator>1 then begin
          WR.AddJSONEscapeW(fColTmp,fGetColIndicator shr 1);
          while res=colTmpUsedTruncated do begin
            res := GetCol(col,ftUTF8);  // get next chunk of Unicode text
            if fGetColIndicator<=0 then
              break;
            WR.AddJSONEscapeW(fColTmp,fGetColIndicator shr 1);
          end;
        end;
        WR.Add('"');
      end;
      ftBlob: begin
        SetString(blob,PAnsiChar(fColTmp),fGetColIndicator);
        offs := 0;
        while res=colTmpUsedTruncated do begin
          inc(offs,fGetColIndicator);
          res := GetCol(Col,ftBlob); // get next chunk of data
          if fGetColIndicator<=0 then
            break;
          SetLength(blob,offs+fGetColIndicator);
          move(fColTmp^,PByteArray(blob)^[offs],fGetColIndicator);
        end;
        WR.WrBase64(pointer(blob),length(blob),true);
      end;
      else assert(false);
    end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

constructor TODBCStatement.Create(aConnection: TSQLDBConnection);
begin
  if not aConnection.InheritsFrom(TODBCConnection) then
    raise ESQLDBException.CreateFmt('%s.Create expects a TODBCConnection',[ClassName]);
  inherited Create(aConnection);
end;

destructor TODBCStatement.Destroy;
begin
  try
    if fStatement<>nil then begin
      ODBC.FreeStmt(fStatement,SQL_CLOSE);
      ODBC.FreeHandle(SQL_HANDLE_STMT,fStatement);
    end;
  finally
    if fColTmp<>nil then
      FreeMem(fColTmp);
    inherited Destroy;
  end;
end;

procedure TODBCStatement.ExecutePrepared;
const
  ODBC_IOTYPE_TO_PARAM: array[TSQLDBParamInOutType] of ShortInt = (
    SQL_PARAM_INPUT, SQL_PARAM_OUTPUT, SQL_PARAM_INPUT_OUTPUT);
var p: integer;
    InputOutputType, ValueType: SqlSmallint;
    ColumnSize: SqlULen;
    ParameterValue: SqlPointer;
    StrLen_or_Ind: SqlLen;
begin
  if fStatement=nil then
    raise EODBCException.Create('ExecutePrepared called without previous Prepare');
  if (fSQL<>'') and (sllSQL in SynDBLog.Family.Level) then
    SynDBLog.Add.Log(sllSQL,SQLWithInlinedParams,self);
  try
    // 1. bind parameters
    if fParamCount>0 then
      for p := 0 to fParamCount-1 do
      with fParams[p] do begin
        StrLen_or_Ind := SQL_NTS;
        ParameterValue := nil;
        ValueType := ODBC_TYPE_TO[VType];
        InputOutputType := ODBC_IOTYPE_TO_PARAM[VInOut];
        case VType of
        ftNull:
          StrLen_or_Ind := SQL_NULL_DATA;
        ftInt64:
          if VInOut=paramIn then
            VData := Int64ToUTF8(VInt64) else begin
            ValueType := SQL_C_SBIGINT;
            ParameterValue := pointer(@VInt64);
          end;
        ftDouble: begin
          ValueType := SQL_C_DOUBLE;
          ParameterValue := pointer(@VInt64);
        end;
        ftCurrency:
          if VInOut=paramIn then
            VData := Curr64ToStr(VInt64) else begin
            ValueType := SQL_C_DOUBLE;
            PDouble(@VInt64)^ := PCurrency(@VInt64)^;
            ParameterValue := pointer(@VInt64);
          end;
        ftDate: begin
          SetLength(VData,sizeof(SQL_TIMESTAMP_STRUCT));
          ValueType := PSQL_TIMESTAMP_STRUCT(VData)^.From(PDateTime(@VInt64)^);
        end;
        ftUTF8:
          VData := Utf8DecodeToRawUnicodeUI(VData);
        ftBlob:
          StrLen_or_Ind := length(VData);
        else
          raise EODBCException.CreateFmt('Invalid bound parameter #%d',[p+1]);
        end;
        if ParameterValue=nil then begin
          ParameterValue := pointer(VData);
          ColumnSize := length(VData);
          if (ValueType=SQL_C_CHAR) or (ValueType=SQL_C_WCHAR) then
            inc(ColumnSize);
        end else
          ColumnSize := SizeOf(Int64);
        ODBC.Check(ODBC.BindParameter(fStatement, p+1, InputOutputType,
          ValueType, ValueType, ColumnSize, 0, ParameterValue, ColumnSize,
          StrLen_or_Ind), SQL_HANDLE_STMT,fStatement);
      end;
    // 2. execute prepared statement
    ODBC.Check(ODBC.Execute(fStatement),SQL_HANDLE_STMT,fStatement);
  finally
    // 3. release and/or retrieve OUT bound parameters
    for p := 0 to fParamCount-1 do
    with fParams[p] do
    case VType of
      ftCurrency:
        if VInOut<>paramIn then
          PCurrency(@VInt64)^ := PDouble(@VInt64)^;
      ftDate:
        if VInOut<>paramIn then
          PDateTime(@VInt64)^ := PSQL_TIMESTAMP_STRUCT(VData)^.ToDateTime;
      ftUTF8:
        VData := RawUnicodeToUtf8(pointer(VData),StrLenW(pointer(VData)));
    end;
  end;
end;

function TODBCStatement.GetUpdateCount: integer;
var RowCount: SqlLen;
begin
  if (fStatement<>nil) and not fExpectResults then
    ODBC.Check(ODBC.RowCount(fStatement,RowCount),SQL_HANDLE_STMT,fStatement) else
    RowCount := 0;
  result := RowCount;
end;

procedure TODBCStatement.Prepare(const aSQL: RawUTF8; ExpectResults: Boolean);
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  if (fStatement<>nil) or (fColumnCount>0) then
    raise EODBCException.CreateFmt('%s.Prepare should be called only once',[ClassName]);
  // 1. process SQL
  inherited Prepare(aSQL,ExpectResults); // set fSQL + Connect if necessary
  fSQLW := Utf8DecodeToRawUnicode(fSQL);
  // 2. prepare statement and bind result columns (if any)
  AllocStatement;
  try
    ODBC.Check(ODBC.PrepareW(fStatement,pointer(fSQLW),length(fSQLW) shr 1),
      SQL_HANDLE_STMT,fStatement);
    if fExpectResults then
      BindColumns;
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      ODBC.FreeHandle(SQL_HANDLE_STMT,fStatement);
      fStatement := nil;
      raise;
    end;
  end;
end;

function TODBCStatement.Step(SeekFirst: boolean): boolean;
const CMD: array[boolean] of smallint = (SQL_FETCH_NEXT,SQL_FETCH_FIRST);
var status: SqlReturn;
    i, sav: integer;
begin
  result := false;
  sav := fCurrentRow;
  fCurrentRow := 0;
  if not Assigned(fStatement) or (fColumnCount=0) then
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  for i := 0 to fColumnCount-1 do
    fColumns[i].ColumnValueState := colNone;
  with ODBC do begin
    status := FetchScroll(fStatement,CMD[SeekFirst],0);
    case status of
    SQL_NO_DATA:
      exit; // no more data
    SQL_SUCCESS, SQL_SUCCESS_WITH_INFO: begin // ignore WITH_INFO messages
      fCurrentRow := sav+1;
      inc(fTotalRowsRetrieved);
      result := true; // mark data available for Column*() methods
    end;
    else HandleError(status,SQL_HANDLE_STMT,fStatement,false,sllNone);
    end;
  end;
end;


{ TODBCLib }

procedure TODBCLib.Check(Status: SqlReturn; HandleType: SqlSmallint;
  Handle: SqlHandle; InfoRaiseException: Boolean=false; LogLevelNoRaise: TSynLogInfo=sllNone);
begin
  if Status<>SQL_SUCCESS then
    HandleError(Status,HandleType,Handle,InfoRaiseException,LogLevelNoRaise);
end;

constructor TODBCLib.Create;
var P: PPointer;
    i: integer;
begin
  fHandle := SafeLoadLibrary('odbc32.dll');
  if fHandle=0 then
    raise EODBCException.Create('Unable to find ODBC Client Interface (odbc32.dll)');
  P := @@AllocEnv;
  for i := 0 to High(ODBC_ENTRIES) do begin
    P^ := GetProcAddress(fHandle,ODBC_ENTRIES[i]);
    if P^=nil then begin
      FreeLibrary(fHandle);
      fHandle := 0;
      raise EODBCException.CreateFmt('Invalid odbc32.dll: missing %s',[ODBC_ENTRIES[i]]);
    end;
    inc(P);
  end;
end;

procedure TODBCLib.HandleError(Status: SqlReturn; HandleType: SqlSmallint;
  Handle: SqlHandle; InfoRaiseException: Boolean; LogLevelNoRaise: TSynLogInfo);
const FMT: PUTF8Char = '%[%] % (%)'#13#10;
var Sqlstate: array[0..6] of WideChar;
    MessageText: array[0..1023] of WideChar;
    RecNum, NativeError: SqlInteger;
    TextLength: SqlSmallint;
    msg: RawUTF8;
begin
  if (Handle=nil) or (Status=SQL_INVALID_HANDLE) then
    msg := 'Invalid handle' else begin
    RecNum := 1;
    while ODBC.GetDiagRecW(HandleType,Handle,RecNum,
       Sqlstate,NativeError,MessageText,1024,TextLength) and (not 1)=0 do begin
      while (textlength>0) and (MessageText[textlength-1]<' ') do begin
        dec(textlength);
        MessageText[textlength] := #0; // trim #13/#10 right of MessageText
      end;
      msg := FormatUTF8(FMT,[msg,Sqlstate,MessageText,NativeError]);
      inc(RecNum);
    end;
    if msg='' then
      msg := 'Unspecified error';
    if (Status=SQL_SUCCESS_WITH_INFO) and not InfoRaiseException then
      LogLevelNoRaise := sllInfo;
  end;
  if LogLevelNoRaise<>sllNone then
    SynDBLog.Add.Log(LogLevelNoRaise,msg) else
    raise EODBCException.Create(string(msg));
end;


{ TODBCConnectionProperties }

constructor TODBCConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  if ODBC=nil then begin
    ODBC := TODBCLib.Create;
    GarbageCollector.Add(ODBC);
  end;
  inherited;
end;

function TODBCConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TODBCConnection.Create(self);
end;

procedure TODBCConnectionProperties.GetFields(const aTableName: RawUTF8;
  var Fields: TSQLDBColumnDefineDynArray);
var Schema, Table: RawUTF8;
    F: TSQLDBColumnDefine;
    i,n: integer;
    status: SqlReturn;
    FA: TDynArray;
begin
  inherited; // first try from SQL, if any (faster)
  if Fields<>nil then
    exit; // already retrieved directly from engine
  Split(aTableName,'.',Schema,Table);
  if Table='' then begin
    Table := Schema;
    Schema := '%';
  end;
  Table := UpperCase(Table);
  Schema := UpperCase(Schema);
  try
    // get column definitions
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      status := ODBC.ColumnsA(fStatement,nil,0,pointer(Schema),SQL_NTS,
        pointer(Table),SQL_NTS,nil,0);
      if status<>SQL_SUCCESS then // e.g. driver does not support schema
        status := ODBC.ColumnsA(fStatement,nil,0,nil,0,pointer(Table),SQL_NTS,nil,0);
      ODBC.Check(status,SQL_HANDLE_STMT,fStatement);
      BindColumns;
      FA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Fields,@n);
      fillchar(F,sizeof(F),0);
      while Step do begin
        F.ColumnName := ColumnUTF8(3);
        F.ColumnLength := ColumnInt(6);
        F.ColumnPrecision := ColumnInt(9);
        F.ColumnScale := ColumnInt(8);
        F.ColumnTypeNative := ColumnUTF8(5);
        F.ColumnType:= ODBCColumnToFieldType(ColumnInt(4),
          F.ColumnPrecision,F.ColumnScale);
        FA.Add(F);
      end;
      SetLength(Fields,n);
    finally
      Free; // TODBCStatement release
    end;
    // get indexes
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.StatisticsA(fStatement,nil,0,pointer(Schema),SQL_NTS,
        pointer(Table),SQL_NTS,SQL_INDEX_ALL,SQL_QUICK);
      if status<>SQL_SUCCESS then // e.g. driver does not support schema
        ODBC.StatisticsA(fStatement,nil,0,nil,0,pointer(Table),SQL_NTS,SQL_INDEX_ALL,SQL_QUICK);
      ODBC.Check(status,SQL_HANDLE_STMT,fStatement);
      BindColumns;
      while Step do begin
        F.ColumnName := Trim(ColumnUTF8(8));
        if F.ColumnName<>'' then
        for i := 0 to n-1 do
          with Fields[i] do
          if IdemPropNameU(ColumnName,F.ColumnName) then begin
            ColumnIndexed := true;
            break;
          end;
      end;
    finally
      Free; // TODBCStatement release
    end;
  except
    on Exception do
      SetLength(Fields,0);
  end;
end;

procedure TODBCConnectionProperties.GetTableNames(var Tables: TRawUTF8DynArray);
var n: integer;
    schema, tablename: RawUTF8;
begin
  inherited; // first try from SQL, if any (faster)
  if Tables<>nil then
    exit; // already retrieved directly from engine
  try
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(ODBC.TablesA(fStatement,nil,0,nil,0,nil,0,'TABLE',SQL_NTS),SQL_HANDLE_STMT,fStatement);
      BindColumns;
      n := 0;
      while Step do begin
        schema := ColumnUTF8(1);
        tablename := ColumnUTF8(2);
        if schema<>'' then
          tablename := schema+'.'+tablename;
        AddSortedRawUTF8(Tables,n,tablename);
      end;
      SetLength(Tables,n);
    finally
      Free; // TODBCStatement release
    end;
  except
    on Exception do
      SetLength(Tables,0);
  end;
end;

procedure TODBCConnectionProperties.GetForeignKeys;
begin
  try
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(ODBC.ForeignKeysA(fStatement,nil,0,nil,0,nil,0,nil,0,nil,0,'%',SQL_NTS),
        SQL_HANDLE_STMT,fStatement);
      BindColumns;
      while Step do 
        fForeignKeys.Add(
          ColumnUTF8(5)+'.'+ColumnUTF8(6)+'.'+ColumnUTF8(7),
          ColumnUTF8(1)+'.'+ColumnUTF8(2)+'.'+ColumnUTF8(3));
    finally
      Free; // TODBCStatement release
    end;
  except
    on Exception do ; // just ignore errors here
  end;
end;

function TODBCConnectionProperties.GetDBMS: TSQLDBDefinition;
begin
  if fDBMS=dUnknown then
    with MainConnection as TODBCConnection do begin
      if not IsConnected then
        Connect; // retrieve DBMS property
      self.fDBMS := DBMS;
    end;
  result := fDBMS;
end;


{ SQL_TIMESTAMP_STRUCT }

function SQL_TIMESTAMP_STRUCT.From(DateTime: TDateTime): SqlSmallint;
var Y,MS: word;
begin
  DecodeDate(DateTime,Y,Month,Day);
  if frac(DateTime)=0 then
    PInt64(@Hour)^ := 0 else
    DecodeTime(DateTime,Hour,Minute,Second,MS);
  Year := Y;
  Fraction := 0;
  if PInt64(@Hour)^=0 then
    result := SQL_C_TYPE_DATE else
    result := SQL_C_TYPE_TIMESTAMP;
end;

function SQL_TIMESTAMP_STRUCT.ToDateTime(DataType: SqlSmallint=SQL_TYPE_TIMESTAMP): TDateTime;
var time: TDateTime;
begin
  if DataType=SQL_TYPE_TIME then
    result := 0 else
    result := EncodeDate(Year,Month,Day);
  if (DataType<>SQL_TYPE_DATE) and (PInt64(@Hour)^<>0) and
     TryEncodeTime(Hour,Minute,Second,0,time) then
    result := result+time;
end;

function SQL_TIMESTAMP_STRUCT.ToIso8601(Dest: PUTF8Char; DataType: SqlSmallint): integer;
begin
  Dest^ := '"';
  inc(Dest);
  if DataType<>SQL_TYPE_TIME then begin
    DateToIso8601PChar(Dest,true,Year,Month,Day);
    inc(Dest,10);
  end;
  if (DataType<>SQL_TYPE_DATE) and (PInt64(@Hour)^<>0) and (Hour<24) and
     (Minute<60) and (Second<60) then begin
    TimeToIso8601PChar(Dest,true,Hour,Minute,Second,'T');
    inc(Dest,9);
    result := 21; // we use 'T' as TTextWriter.AddDateTime
  end else
    result := 12; // only date
  Dest^ := '"';
end;

end.
