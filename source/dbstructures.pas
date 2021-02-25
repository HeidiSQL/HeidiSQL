unit dbstructures;

// -------------------------------------
// Server constants, variables and data types
// -------------------------------------

interface

uses
  Classes, Graphics, Windows, SysUtils, gnugettext, Vcl.Forms;



const
  // Used in TMysqlFunction
  SQL_VERSION_ANSI = -1;

  // General declarations
  MYSQL_ERRMSG_SIZE = 512;
  SQLSTATE_LENGTH = 5;
  SCRAMBLE_LENGTH = 20;
  MYSQL_PORT = 3306;
  LOCAL_HOST = 'localhost';
  NAME_LEN = 64;
  PROTOCOL_VERSION = 10;
  FRM_VER = 6;

  // Field's flags
  NOT_NULL_FLAG = 1;
  PRI_KEY_FLAG = 2;
  UNIQUE_KEY_FLAG = 4;
  MULTIPLE_KEY_FLAG = 8;
  BLOB_FLAG = 16;
  UNSIGNED_FLAG = 32;
  ZEROFILL_FLAG = 64;
  BINARY_FLAG = 128;
  ENUM_FLAG = 256;
  AUTO_INCREMENT_FLAG = 512;
  TIMESTAMP_FLAG = 1024;
  SET_FLAG = 2048;
  NUM_FLAG = 32768;
  PART_KEY_FLAG = 16384;
  GROUP_FLAG = 32768;
  UNIQUE_FLAG = 65536;
  BINCMP_FLAG = 131072;

  // Client connection options
  CLIENT_LONG_PASSWORD = 1;
  CLIENT_FOUND_ROWS = 2; // Found instead of affected rows
  CLIENT_LONG_FLAG = 4;
  CLIENT_CONNECT_WITH_DB = 8;
  CLIENT_NO_SCHEMA = 16; // Don't allow database.table.column
  CLIENT_COMPRESS = 32;
  CLIENT_ODBC = 64;
  CLIENT_LOCAL_FILES = 128;
  CLIENT_IGNORE_SPACE = 256; // Ignore spaces before '('
  CLIENT_PROTOCOL_41 = 512;
  CLIENT_INTERACTIVE = 1024;
  CLIENT_SSL = 2048; // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE = 4096;
  CLIENT_TRANSACTIONS = 8192;
  CLIENT_RESERVED = 16384;
  CLIENT_SECURE_CONNECTION = 32768;
  CLIENT_MULTI_STATEMENTS = 65536;
  CLIENT_MULTI_RESULTS = 131072;
  CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS = 4194304;
  CLIENT_SSL_VERIFY_SERVER_CERT = 67108864;
  CLIENT_REMEMBER_OPTIONS = 134217728;

  COLLATION_BINARY = 63;
  // Equivalent to COLLATION_BINARY, this is what a new driver returns when connected to a pre-4.1 server.
  COLLATION_NONE =  0;

  { SQLite Result Codes
    result code definitions

    Many SQLite functions return an integer result code from the set shown
    here in order to indicate success or failure.

    New error codes may be added in future versions of SQLite.

    See also: [extended result code definitions]
  }
  SQLITE_OK           = 0;   // Successful result
  // beginning-of-error-codes
  SQLITE_ERROR        = 1;   // Generic error
  SQLITE_INTERNAL     = 2;   // Internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite3_interrupt()*/
  SQLITE_IOERR        = 10;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT      = 11;  // The database disk image is malformed
  SQLITE_NOTFOUND     = 12;  // Unknown opcode in sqlite3_file_control()
  SQLITE_FULL         = 13;  // Insertion failed because database is full
  SQLITE_CANTOPEN     = 14;  // Unable to open the database file
  SQLITE_PROTOCOL     = 15;  // Database lock protocol error
  SQLITE_EMPTY        = 16;  // Internal use only
  SQLITE_SCHEMA       = 17;  // The database schema changed
  SQLITE_TOOBIG       = 18;  // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT   = 19;  // Abort due to constraint violation
  SQLITE_MISMATCH     = 20;  // Data type mismatch
  SQLITE_MISUSE       = 21;  // Library used incorrectly
  SQLITE_NOLFS        = 22;  // Uses OS features not supported on host
  SQLITE_AUTH         = 23;  // Authorization denied
  SQLITE_FORMAT       = 24;  // Not used
  SQLITE_RANGE        = 25;  // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB       = 26;  // File opened that is not a database file
  SQLITE_NOTICE       = 27;  // Notifications from sqlite3_log()
  SQLITE_WARNING      = 28;  // Warnings from sqlite3_log()
  SQLITE_ROW          = 100; // sqlite3_step() has another row ready
  SQLITE_DONE         = 101; // sqlite3_step() has finished executing

  { SQLite Flags
    These constants define various flags that can be passed into
    "prepFlags" parameter of the [sqlite3_prepare_v3()] and
    [sqlite3_prepare16_v3()] interfaces.

    New flags may be added in future releases of SQLite.
  }
  SQLITE_PREPARE_PERSISTENT = $01; // prepared statement will be retained for a long time and probably reused many times
  SQLITE_PREPARE_NORMALIZE  = $02; // no-op
  SQLITE_PREPARE_NO_VTAB    = $04; // return an error (error code SQLITE_ERROR) if the statement uses any virtual tables


  { SQLite Fundamental Datatypes
    Every value in SQLite has one of five fundamental datatypes:
      64-bit signed integer
      64-bit IEEE floating point number
      string
      BLOB
      NULL
  }
  SQLITE_INTEGER  = 1;
  SQLITE_FLOAT    = 2;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;
  SQLITE_TEXT     = 3;
  SQLITE3_TEXT    = 3;

  { CAPI3REF: Database Connection Configuration Options
    These constants are the available integer configuration options that
    can be passed as the second argument to the [sqlite3_db_config()] interface.
  }
  SQLITE_DBCONFIG_MAINDBNAME            = 1000; // const char*
  SQLITE_DBCONFIG_LOOKASIDE             = 1001; // void* int int
  SQLITE_DBCONFIG_ENABLE_FKEY           = 1002; // int int*
  SQLITE_DBCONFIG_ENABLE_TRIGGER        = 1003; // int int*
  SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = 1004; // int int*
  SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005; // int int*
  SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE      = 1006; // int int*
  SQLITE_DBCONFIG_ENABLE_QPSG           = 1007; // int int*
  SQLITE_DBCONFIG_TRIGGER_EQP           = 1008; // int int*
  SQLITE_DBCONFIG_RESET_DATABASE        = 1009; // int int*
  SQLITE_DBCONFIG_DEFENSIVE             = 1010; // int int*
  SQLITE_DBCONFIG_WRITABLE_SCHEMA       = 1011; // int int*
  SQLITE_DBCONFIG_LEGACY_ALTER_TABLE    = 1012; // int int*
  SQLITE_DBCONFIG_DQS_DML               = 1013; // int int*
  SQLITE_DBCONFIG_DQS_DDL               = 1014; // int int*
  SQLITE_DBCONFIG_ENABLE_VIEW           = 1015; // int int*
  SQLITE_DBCONFIG_MAX                   = 1015; // Largest DBCONFIG



type
  PUSED_MEM=^USED_MEM;
  USED_MEM = packed record
    next:       PUSED_MEM;
    left:       Integer;
    size:       Integer;
  end;

  PERR_PROC = ^ERR_PROC;
  ERR_PROC = procedure;

  PMEM_ROOT = ^MEM_ROOT;
  MEM_ROOT = packed record
    free:              PUSED_MEM;
    used:              PUSED_MEM;
    pre_alloc:         PUSED_MEM;
    min_malloc:        Integer;
    block_size:        Integer;
    block_num:         Integer;
    first_block_usage: Integer;
    error_handler:     PERR_PROC;
  end;

  NET = record
    vio:              Pointer;
    buff:             PAnsiChar;
    buff_end:         PAnsiChar;
    write_pos:        PAnsiChar;
    read_pos:         PAnsiChar;
    fd:               Integer;
    max_packet:       Cardinal;
    max_packet_size:  Cardinal;
    pkt_nr:           Cardinal;
    compress_pkt_nr:  Cardinal;
    write_timeout:    Cardinal;
    read_timeout:     Cardinal;
    retry_count:      Cardinal;
    fcntl:            Integer;
    compress:         Byte;
    remain_in_buf:    LongInt;
    length:           LongInt;
    buf_length:       LongInt;
    where_b:          LongInt;
    return_status:    Pointer;
    reading_or_writing: Char;
    save_char:        Char;
    no_send_ok:       Byte;
    last_error:       array[1..MYSQL_ERRMSG_SIZE] of Char;
    sqlstate:         array[1..SQLSTATE_LENGTH + 1] of Char;
    last_errno:       Cardinal;
    error:            Char;
    query_cache_query: Pointer;
    report_error:     Byte;
    return_errno:     Byte;
  end;

  PMYSQL_FIELD = ^MYSQL_FIELD;
  MYSQL_FIELD = record
    name:             PAnsiChar;   // Name of column
    org_name:         PAnsiChar;   // Name of original column (added after 3.23.58)
    table:            PAnsiChar;   // Table of column if column was a field
    org_table:        PAnsiChar;   // Name of original table (added after 3.23.58
    db:               PAnsiChar;   // table schema (added after 3.23.58)
    catalog:	        PAnsiChar;   // table catalog (added after 3.23.58)
    def:              PAnsiChar;   // Default value (set by mysql_list_fields)
    length:           LongInt;     // Width of column
    max_length:       LongInt;     // Max width of selected set
    // added after 3.23.58
    name_length:      Cardinal;
    org_name_length:  Cardinal;
    table_length:     Cardinal;
    org_table_length: Cardinal;
    db_length:        Cardinal;
    catalog_length:   Cardinal;
    def_length:       Cardinal;
    //***********************
    flags:            Cardinal;    // Div flags
    decimals:         Cardinal;    // Number of decimals in field
    charsetnr:        Cardinal;    // char set number (added in 4.1)
    _type:            Cardinal;    // Type of field. Se mysql_com.h for types
  end;

  MYSQL_ROW = array[0..$ffff] of PAnsiChar;
  PMYSQL_ROW = ^MYSQL_ROW;

  PMYSQL_ROWS = ^MYSQL_ROWS;
  MYSQL_ROWS = record
    next:       PMYSQL_ROWS;
    data:       PMYSQL_ROW;
  end;

  MYSQL_DATA = record
    Rows:       Int64;
    Fields:     Cardinal;
    Data:       PMYSQL_ROWS;
    Alloc:      MEM_ROOT;
  end;
  PMYSQL_DATA = ^MYSQL_DATA;

  PMYSQL = ^MYSQL;
  MYSQL = record
    _net:            NET;
    connector_fd:    Pointer;
    host:            PAnsiChar;
    user:            PAnsiChar;
    passwd:          PAnsiChar;
    unix_socket:     PAnsiChar;
    server_version:  PAnsiChar;
    host_info:       PAnsiChar;
    info:            PAnsiChar;
    db:              PAnsiChar;
    charset:         PAnsiChar;
    fields:          PMYSQL_FIELD;
    field_alloc:     MEM_ROOT;
    affected_rows:   Int64;
    insert_id:       Int64;
    extra_info:      Int64;
    thread_id:       LongInt;
    packet_length:   LongInt;
    port:            Cardinal;
    client_flag:     LongInt;
    server_capabilities: LongInt;
    protocol_version: Cardinal;
    field_count:     Cardinal;
    server_status:   Cardinal;
    server_language: Cardinal;
    warning_count:   Cardinal;
    options:         Cardinal;
    status:          Byte;
    free_me:         Byte;
    reconnect:       Byte;
    scramble:        array[1..SCRAMBLE_LENGTH+1] of Char;
    rpl_pivot:       Byte;
    master:          PMYSQL;
    next_slave:      PMYSQL;
    last_used_slave: PMYSQL;
    last_used_con:   PMYSQL;
    stmts:           Pointer;
    methods:         Pointer;
    thd:             Pointer;
    unbuffered_fetch_owner: PByte;
  end;

  MYSQL_RES = record
    row_count:       Int64;
    field_count, current_field:     Integer;
    fields:          PMYSQL_FIELD;
    data:            PMYSQL_DATA;
    data_cursor:     PMYSQL_ROWS;
    field_alloc:     MEM_ROOT;
    row:             PMYSQL_ROW;     // If unbuffered read
    current_row:     PMYSQL_ROW;     // buffer to current row
    lengths:         PLongInt;       // column lengths of current row
    handle:          PMYSQL;         // for unbuffered reads
    eof:             Byte;           // Used my mysql_fetch_row
    is_ps:           Byte;
  end;
  PMYSQL_RES = ^MYSQL_RES;

  TMySQLOption = (
    MYSQL_OPT_CONNECT_TIMEOUT,
    MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE,
    MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR,
    MYSQL_SET_CHARSET_NAME,
    MYSQL_OPT_LOCAL_INFILE,
    MYSQL_OPT_PROTOCOL,
    MYSQL_SHARED_MEMORY_BASE_NAME,
    MYSQL_OPT_READ_TIMEOUT,
    MYSQL_OPT_WRITE_TIMEOUT,
    MYSQL_OPT_USE_RESULT,
    MYSQL_OPT_USE_REMOTE_CONNECTION,
    MYSQL_OPT_USE_EMBEDDED_CONNECTION,
    MYSQL_OPT_GUESS_CONNECTION,
    MYSQL_SET_CLIENT_IP,
    MYSQL_SECURE_AUTH,
    MYSQL_REPORT_DATA_TRUNCATION,
    MYSQL_OPT_RECONNECT,
    MYSQL_OPT_SSL_VERIFY_SERVER_CERT,
    MYSQL_PLUGIN_DIR,
    MYSQL_DEFAULT_AUTH,
    MYSQL_OPT_BIND,
    MYSQL_OPT_SSL_KEY,
    MYSQL_OPT_SSL_CERT,
    MYSQL_OPT_SSL_CA,
    MYSQL_OPT_SSL_CAPATH,
    MYSQL_OPT_SSL_CIPHER,
    MYSQL_OPT_SSL_CRL,
    MYSQL_OPT_SSL_CRLPATH,
    // Connection attribute options
    MYSQL_OPT_CONNECT_ATTR_RESET,
    MYSQL_OPT_CONNECT_ATTR_ADD,
    MYSQL_OPT_CONNECT_ATTR_DELETE,
    MYSQL_SERVER_PUBLIC_KEY,
    MYSQL_ENABLE_CLEARTEXT_PLUGIN,
    MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS,
    MYSQL_OPT_SSL_ENFORCE,
    MYSQL_OPT_MAX_ALLOWED_PACKET,
    MYSQL_OPT_NET_BUFFER_LENGTH,
    MYSQL_OPT_TLS_VERSION,
    MYSQL_OPT_SSL_MODE,
    MYSQL_OPT_GET_SERVER_PUBLIC_KEY,

    // MariaDB specific
    MYSQL_PROGRESS_CALLBACK=5999,
    MYSQL_OPT_NONBLOCK,
    // MariaDB Connector/C specific
    MYSQL_DATABASE_DRIVER=7000,
    MARIADB_OPT_SSL_FP,                // deprecated, use MARIADB_OPT_TLS_PEER_FP instead
    MARIADB_OPT_SSL_FP_LIST,           // deprecated, use MARIADB_OPT_TLS_PEER_FP_LIST instead
    MARIADB_OPT_TLS_PASSPHRASE,        // passphrase for encrypted certificates
    MARIADB_OPT_TLS_CIPHER_STRENGTH,
    MARIADB_OPT_TLS_VERSION,
    MARIADB_OPT_TLS_PEER_FP,           // single finger print for server certificate verification
    MARIADB_OPT_TLS_PEER_FP_LIST,      // finger print white list for server certificate verification
    MARIADB_OPT_CONNECTION_READ_ONLY,
    MYSQL_OPT_CONNECT_ATTRS,           // for mysql_get_optionv
    MARIADB_OPT_USERDATA,
    MARIADB_OPT_CONNECTION_HANDLER,
    MARIADB_OPT_PORT,
    MARIADB_OPT_UNIXSOCKET,
    MARIADB_OPT_PASSWORD,
    MARIADB_OPT_HOST,
    MARIADB_OPT_USER,
    MARIADB_OPT_SCHEMA,
    MARIADB_OPT_DEBUG,
    MARIADB_OPT_FOUND_ROWS,
    MARIADB_OPT_MULTI_RESULTS,
    MARIADB_OPT_MULTI_STATEMENTS,
    MARIADB_OPT_INTERACTIVE,
    MARIADB_OPT_PROXY_HEADER
    );

  // MySQL data types
  TDBDatatypeIndex = (dtTinyint, dtSmallint, dtMediumint, dtInt, dtBigint, dtSerial, dtBigSerial,
    dtFloat, dtDouble, dtDecimal, dtNumeric, dtReal, dtDoublePrecision, dtMoney, dtSmallmoney,
    dtDate, dtTime, dtYear, dtDatetime, dtDatetime2, dtDatetimeOffset, dtSmalldatetime, dtTimestamp, dtInterval,
    dtChar, dtNchar, dtVarchar, dtNvarchar, dtTinytext, dtText, dtNtext, dtMediumtext, dtLongtext,
    dtJson, dtJsonB, dtCidr, dtInet, dtMacaddr,
    dtBinary, dtVarbinary, dtTinyblob, dtBlob, dtMediumblob, dtLongblob, dtImage,
    dtEnum, dtSet, dtBit, dtVarBit, dtBool, dtRegClass, dtRegProc, dtUnknown,
    dtCursor, dtSqlvariant, dtTable, dtUniqueidentifier, dtHierarchyid, dtXML,
    dtPoint, dtLinestring, dtLineSegment, dtPolygon, dtGeometry, dtBox, dtPath, dtCircle, dtMultipoint, dtMultilinestring, dtMultipolygon, dtGeometrycollection
    );

  // MySQL data type categorization
  TDBDatatypeCategoryIndex = (dtcInteger, dtcReal, dtcText, dtcBinary, dtcTemporal, dtcSpatial, dtcOther);

  // MySQL native column type constants. See include/mysql.h.pp in the server code
  TMySQLType = (mytDecimal, mytTiny, mytShort, mytLong, mytFloat, mytDouble, mytNull, mytTimestamp,
    mytLonglong, mytInt24, mytDate, mytTime, mytDatetime, mytYear, mytNewdate, mytVarchar,
    mytBit, mytTimestamp2, mytDatetime2, mytTime2, mytJson=245, mytNewdecimal, mytEnum, mytSet, mytTinyblob,
    mytMediumblob, mytLongblob, mytBlob, mytVarstring, mytString, mytGeometry);
  // MySQL data type structure
  TDBDatatype = record
    Index:           TDBDatatypeIndex;
    NativeType:      TMySQLType; // See above
    NativeTypes:     String; // Same as above, but for multiple postgresql oid's
    Name:            String;
    Names:           String;
    Description:     String;
    HasLength:       Boolean; // Can have Length- or Set-attribute?
    RequiresLength:  Boolean; // Must have a Length- or Set-attribute?
    MaxSize:         Int64;
    DefaultSize:     Int64;   // TEXT and BLOB allow custom length, but we want to leave the default max length away from ALTER TABLE's
    HasBinary:       Boolean; // Can be binary?
    HasDefault:      Boolean; // Can have a default value?
    LoadPart:        Boolean; // Select per SUBSTR() or LEFT()
    DefLengthSet:    String;  // Should be set for types which require a length/set
    Format:          String;  // Used for date/time values when displaying and generating queries
    ValueMustMatch:  String;
    Category:        TDBDatatypeCategoryIndex;
  end;

  // MySQL data type category structure
  TDBDatatypeCategory = record
    Index:           TDBDatatypeCategoryIndex;
    Name:            String;
    Color:           TColor;
    NullColor:       TColor;
  end;

  // MySQL functions structure
  TMySQLFunction = record
    Name:         String;
    Declaration:  String;
    Category:     String;
    Version:      Integer; // Minimum MySQL version where function is available
    Description:  String;
  end;

  // PostgreSQL structures
  TPQConnectStatus = (CONNECTION_OK, CONNECTION_BAD, CONNECTION_STARTED, CONNECTION_MADE, CONNECTION_AWAITING_RESPONSE, CONNECTION_AUTH_OK, CONNECTION_SETENV, CONNECTION_SSL_STARTUP, CONNECTION_NEEDED);
  PPGconn = Pointer;
  PPGresult = Pointer;
  POid = Cardinal;

  // SQLite structures
  Psqlite3 = Pointer;
  Psqlite3_stmt = Pointer;

  // Server variables
  TVarScope = (vsGlobal, vsSession, vsBoth);
  TServerVariable = record
    Name: String;
    IsDynamic: Boolean;
    VarScope: TVarScope;
    EnumValues: String;
  end;

  // Custom exception class for any connection or database related error
  EDbError = class(Exception)
    public
      ErrorCode: Cardinal;
      constructor Create(const Msg: string; const ErrorCode: Cardinal=0);
  end;

  // DLL loading
  TDbLib = class(TObject)
    const
      LIB_PROC_ERROR: Cardinal = 1000;
    private
      FDllFile: String;
      FHandle: HMODULE;
      procedure AssignProc(var Proc: FARPROC; Name: PAnsiChar; Mandantory: Boolean=True);
      procedure AssignProcedures; virtual; abstract;
    public
      property Handle: HMODULE read FHandle;
      property DllFile: String read FDllFile;
      constructor Create(DllFile: String);
      destructor Destroy; override;
  end;
  TMySQLLib = class(TDbLib)
    mysql_affected_rows: function(Handle: PMYSQL): Int64; stdcall;
    mysql_character_set_name: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_close: procedure(Handle: PMYSQL); stdcall;
    mysql_data_seek: procedure(Result: PMYSQL_RES; Offset: Int64); stdcall;
    mysql_errno: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_error: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_fetch_field_direct: function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD; stdcall;
    mysql_fetch_field: function(Result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
    mysql_fetch_lengths: function(Result: PMYSQL_RES): PLongInt; stdcall;
    mysql_fetch_row: function(Result: PMYSQL_RES): PMYSQL_ROW; stdcall;
    mysql_free_result: procedure(Result: PMYSQL_RES); stdcall;
    mysql_get_client_info: function: PAnsiChar; stdcall;
    mysql_get_server_info: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_init: function(Handle: PMYSQL): PMYSQL; stdcall;
    mysql_num_fields: function(Result: PMYSQL_RES): Integer; stdcall;
    mysql_num_rows: function(Result: PMYSQL_RES): Int64; stdcall;
    mysql_options: function(Handle: PMYSQL; Option: Integer; arg: PAnsiChar): Integer; stdcall;
    mysql_optionsv: function(Handle: PMYSQL; Option: Integer; arg, val: PAnsiChar): Integer; stdcall;
    mysql_ping: function(Handle: PMYSQL): Integer; stdcall;
    mysql_real_connect: function(Handle: PMYSQL; const Host, User, Passwd, Db: PAnsiChar; Port: Cardinal; const UnixSocket: PAnsiChar; ClientFlag: Cardinal): PMYSQL; stdcall;
    mysql_real_query: function(Handle: PMYSQL; const Query: PAnsiChar; Length: Cardinal): Integer; stdcall;
    mysql_ssl_set: function(Handle: PMYSQL; const key, cert, CA, CApath, cipher: PAnsiChar): Byte; stdcall;
    mysql_stat: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_store_result: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    mysql_thread_id: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_next_result: function(Handle: PMYSQL): Integer; stdcall;
    mysql_set_character_set: function(Handle: PMYSQL; csname: PAnsiChar): Integer; stdcall;
    mysql_thread_init: function: Byte; stdcall;
    mysql_thread_end: procedure; stdcall;
    mysql_warning_count: function(Handle: PMYSQL): Cardinal; stdcall;
    private
      procedure AssignProcedures; override;
  end;
  TPostgreSQLLib = class(TDbLib)
    PQconnectdb: function(const ConnInfo: PAnsiChar): PPGconn cdecl;
    PQerrorMessage: function(const Handle: PPGconn): PAnsiChar cdecl;
    PQresultErrorMessage: function(const Result: PPGresult): PAnsiChar cdecl;
    PQresultErrorField: function(const Result: PPGresult; fieldcode: Integer): PAnsiChar;
    PQfinish: procedure(const Handle: PPGconn);
    PQstatus: function(const Handle: PPGconn): TPQConnectStatus cdecl;
    PQsendQuery: function(const Handle: PPGconn; command: PAnsiChar): Integer cdecl;
    PQgetResult: function(const Handle: PPGconn): PPGresult cdecl;
    PQbackendPID: function(const Handle: PPGconn): Integer cdecl;
    PQcmdTuples: function(Result: PPGresult): PAnsiChar; cdecl;
    PQntuples: function(Result: PPGresult): Integer; cdecl;
    PQclear: procedure(Result: PPGresult); cdecl;
    PQnfields: function(Result: PPGresult): Integer; cdecl;
    PQfname: function(const Result: PPGresult; column_number: Integer): PAnsiChar; cdecl;
    PQftype: function(const Result: PPGresult; column_number: Integer): POid; cdecl;
    PQftable: function(const Result: PPGresult; column_number: Integer): POid; cdecl;
    PQgetvalue: function(const Result: PPGresult; row_number: Integer; column_number: Integer): PAnsiChar; cdecl;
    PQgetlength: function(const Result: PPGresult; row_number: Integer; column_number: Integer): Integer; cdecl;
    PQgetisnull: function(const Result: PPGresult; row_number: Integer; column_number: Integer): Integer; cdecl;
    PQlibVersion: function(): Integer; cdecl;
    private
      procedure AssignProcedures; override;
  end;
  TSQLiteCollationNeededCallback = procedure(userData: Pointer; ppDb:Psqlite3; eTextRep: Integer; zName: PAnsiChar); cdecl;
  TSQLiteCollation = function(userData: Pointer; lenA: Integer; strA: PAnsiChar; lenB: Integer; strB: PAnsiChar): Integer; cdecl;

  TSQLiteLib = class(TDbLib)
    sqlite3_open: function(const filename: PAnsiChar; var ppDb: Psqlite3): Integer; cdecl;
    sqlite3_libversion: function(): PAnsiChar; cdecl;
    sqlite3_close: function(ppDb: Psqlite3): Integer; cdecl;
    sqlite3_db_config: function (ppDb: Psqlite3; op: Integer): Integer; cdecl varargs;
    sqlite3_enable_load_extension: function(ppDb: Psqlite3; onoff: Integer): Integer; cdecl;
    sqlite3_errmsg: function(ppDb: Psqlite3): PAnsiChar; cdecl;
    sqlite3_errcode: function(ppDb: Psqlite3): Integer; cdecl;
    sqlite3_prepare_v2: function(ppDb: Psqlite3; zSql: PAnsiChar; nByte: Integer; var ppStmt: Psqlite3_stmt; var pzTail: PAnsiChar): Integer; cdecl;
    sqlite3_prepare_v3: function(ppDb: Psqlite3; zSql: PAnsiChar; nByte: Integer; prepFlags: Cardinal; var ppStmt: Psqlite3_stmt; var pzTail: PAnsiChar): Integer; cdecl;
    sqlite3_exec: function(ppDb: Psqlite3; sql: PAnsiChar; callback: Integer; callvack_arg: Pointer; errmsg: PAnsiChar): Integer; cdecl;
    sqlite3_finalize: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_step: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_reset: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_total_changes: function(ppDb: Psqlite3): Integer; cdecl;
    sqlite3_column_text: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_count: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_column_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_decltype: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_database_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_table_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_origin_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_type: function(pStmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
    sqlite3_next_stmt: function(ppDb: Psqlite3; pStmt: Psqlite3_stmt): Psqlite3_stmt; cdecl;
    sqlite3_table_column_metadata: function(ppDb: Psqlite3;
      zDbName, zTableName, zColumnName: PAnsiChar;
      var pzDataType, pzCollSeq: PAnsiChar; var pNotNull, pPrimaryKey, pAutoinc: Integer
      ): Integer; cdecl;
    sqlite3_collation_needed: function(ppDb: Psqlite3; userData: Pointer; Func: TSQLiteCollationNeededCallback): Integer; cdecl;
    sqlite3_create_collation: function(ppDb: Psqlite3; const zName: PAnsiChar; eTextRep: Integer; pArg: Pointer; xCompare: TSQLiteCollation): Integer; cdecl;
    private
      procedure AssignProcedures; override;
  end;

var
  MySQLKeywords: TStringList;
  MySQLErrorCodes: TStringList;

  // MySQL data type categories
  DatatypeCategories: array[TDBDatatypeCategoryIndex] of TDBDatatypeCategory = (
    (
      Index:           dtcInteger;
      Name:            'Integer'
    ),
    (
      Index:           dtcReal;
      Name:            'Real'
    ),
    (
      Index:           dtcText;
      Name:            'Text'
    ),
    (
      Index:           dtcBinary;
      Name:            'Binary'
    ),
    (
      Index:           dtcTemporal;
      Name:            'Temporal (time)'
    ),
    (
      Index:           dtcSpatial;
      Name:            'Spatial (geometry)'
    ),
    (
      Index:           dtcOther;
      Name:            'Other'
    )
  );

  // MySQL Data Type List and Properties
  MySQLDatatypes: array [0..37] of TDBDatatype =
  (
    (
      Index:           dtUnknown;
      NativeTypes:     '99999';
      Name:            'UNKNOWN';
      Description:     'Unknown data type';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtTinyint;
      NativeType:      mytTiny;
      Name:            'TINYINT';
      Description:     'TINYINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A very small integer. The signed range is -128 to 127. ' +
        'The unsigned range is 0 to 255.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         127;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtSmallint;
      NativeType:      mytShort;
      Name:            'SMALLINT';
      Description:     'SMALLINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A small integer. The signed range is -32768 to 32767. ' +
        'The unsigned range is 0 to 65535.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         32767;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtMediumint;
      NativeType:      mytInt24;
      Name:            'MEDIUMINT';
      Description:     'MEDIUMINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A medium-sized integer. The signed range is -8388608 to 8388607. ' +
        'The unsigned range is 0 to 16777215.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         8388607;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtInt;
      NativeType:      mytLong;
      Name:            'INT';
      Description:     'INT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A normal-size integer. The signed range is -2147483648 to 2147483647. ' +
        'The unsigned range is 0 to 4294967295.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         2147483647;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtBigint;
      NativeType:      mytLonglong;
      Name:            'BIGINT';
      Description:     'BIGINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A large integer. The signed range is -9223372036854775808 to ' +
        '9223372036854775807. The unsigned range is 0 to 18446744073709551615.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         9223372036854775807;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtFloat;
      NativeType:      mytFloat;
      Name:            'FLOAT';
      Description:     'FLOAT[(M,D)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A small (single-precision) floating-point number. Allowable values are '+
        '-3.402823466E+38 to -1.175494351E-38, 0, and 1.175494351E-38 to '+
        '3.402823466E+38. These are the theoretical limits, based on the IEEE '+
        'standard. The actual range might be slightly smaller depending on your '+
        'hardware or operating system.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtDouble;
      NativeType:      mytDouble;
      Name:            'DOUBLE';
      Description:     'DOUBLE[(M,D)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A normal-size (double-precision) floating-point number. Allowable ' +
        'values are -1.7976931348623157E+308 to -2.2250738585072014E-308, 0, and ' +
        '2.2250738585072014E-308 to 1.7976931348623157E+308. These are the ' +
        'theoretical limits, based on the IEEE standard. The actual range might ' +
        'be slightly smaller depending on your hardware or operating system.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtDecimal;
      NativeType:      mytNewdecimal;
      Name:            'DECIMAL';
      Description:     'DECIMAL[(M[,D])] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A packed "exact" fixed-point number. M is the total number of digits ' +
        '(the precision) and D is the number of digits after the decimal point ' +
        '(the scale). The decimal point and (for negative numbers) the "-" sign ' +
        'are not counted in M. If D is 0, values have no decimal point or ' +
        'fractional part. The maximum number of digits (M) for DECIMAL is 65. ' +
        'The maximum number of supported decimals (D) is 30. If D is omitted, ' +
        'the default is 0. If M is omitted, the default is 10.';
      HasLength:       True;
      RequiresLength:  True;
      MaxSize:         9223372036854775807;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '20,6';
      Category:        dtcReal;
    ),
    (
      Index:           dtDate;
      NativeType:      mytDate;
      Name:            'DATE';
      Description:     'DATE' + sLineBreak +
        'A date. The supported range is ''1000-01-01'' to ''9999-12-31''. MySQL ' +
        'displays DATE values in ''YYYY-MM-DD'' format, but allows assignment of ' +
        'values to DATE columns using either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtTime;
      NativeType:      mytTime;
      Name:            'TIME';
      Description:     'TIME' + sLineBreak +
        'A time. The range is ''-838:59:59'' to ''838:59:59''. MySQL displays TIME ' +
        'values in ''HH:MM:SS'' format, but allows assignment of values to TIME ' +
        'columns using either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtYear;
      NativeType:      mytYear;
      Name:            'YEAR';
      Description:     'YEAR[(2|4)]' + sLineBreak +
        'A year in two-digit or four-digit format. The default is four-digit ' +
        'format. In four-digit format, the allowable values are 1901 to 2155, ' +
        'and 0000. In two-digit format, the allowable values are 70 to 69, ' +
        'representing years from 1970 to 2069. MySQL displays YEAR values in ' +
        'YYYY format, but allows you to assign values to YEAR columns using ' +
        'either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDatetime;
      NativeType:      mytDatetime;
      Name:            'DATETIME';
      Description:     'DATETIME' + sLineBreak +
        'A date and time combination. The supported range is ''1000-01-01 ' +
        '00:00:00'' to ''9999-12-31 23:59:59''. MySQL displays DATETIME values in ' +
        '''YYYY-MM-DD HH:MM:SS'' format, but allows assignment of values to ' +
        'DATETIME columns using either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtTimestamp;
      NativeType:      mytTimestamp;
      Name:            'TIMESTAMP';
      Description:     'TIMESTAMP' + sLineBreak +
        'A timestamp. The range is ''1970-01-01 00:00:01'' UTC to ''2038-01-09 ' +
        '03:14:07'' UTC. TIMESTAMP values are stored as the number of seconds ' +
        'since the epoch (''1970-01-01 00:00:00'' UTC). A TIMESTAMP cannot ' +
        'represent the value ''1970-01-01 00:00:00'' because that is equivalent to ' +
        '0 seconds from the epoch and the value 0 is reserved for representing ' +
        '''0000-00-00 00:00:00'', the "zero" TIMESTAMP value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtVarchar;
      NativeType:      mytVarstring;
      Name:            'VARCHAR';
      Description:     'VARCHAR(M)' + sLineBreak +
        'A variable-length string. M represents the maximum column length in ' +
        'characters. The range of M is 0 to 65,535. The effective maximum length ' +
        'of a VARCHAR is subject to the maximum row size (65,535 bytes, which is ' +
        'shared among all columns) and the character set used. For example, utf8 ' +
        'characters can require up to three bytes per character, so a VARCHAR ' +
        'column that uses the utf8 character set can be declared to be a maximum ' +
        'of 21,844 characters. ' + sLineBreak + sLineBreak +
        '*Note*: MySQL 5.1 follows the standard SQL specification, and does not ' +
        'remove trailing spaces from VARCHAR values.';
      HasLength:       True;
      RequiresLength:  True;
      MaxSize:         255;
      HasBinary:       True; // MySQL-Help says the opposite but it's valid for older versions at least.
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtChar;
      NativeType:      mytString;
      Name:            'CHAR';
      Description:     'CHAR[(M)]' + sLineBreak +
        'A fixed-length string that is always right-padded with spaces to the ' +
        'specified length when stored. M represents the column length in ' +
        'characters. The range of M is 0 to 255. If M is omitted, the length is 1.' + sLineBreak + sLineBreak +
        '*Note*: Trailing spaces are removed when CHAR values are retrieved ' +
        'unless the PAD_CHAR_TO_FULL_LENGTH SQL mode is enabled.';
      HasLength:       True;
      RequiresLength:  True;
      MaxSize:         255;
      HasBinary:       True;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtTinytext;
      NativeType:      mytTinyblob;
      Name:            'TINYTEXT';
      Description:     'TINYTEXT' + sLineBreak +
        'A TEXT column with a maximum length of 255 (2^8 - 1) characters. The ' +
        'effective maximum length is less if the value contains multi-byte ' +
        'characters. Each TINYTEXT value is stored using a one-byte length ' +
        'prefix that indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      MaxSize:         255;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dtText;
      NativeType:      mytBlob;
      Name:            'TEXT';
      Description:     'TEXT[(M)]' + sLineBreak +
        'A TEXT column with a maximum length of 65,535 (2^16 - 1) characters. The ' +
        'effective maximum length is less if the value contains multi-byte ' +
        'characters. Each TEXT value is stored using a two-byte length prefix ' +
        'that indicates the number of bytes in the value. ' + sLineBreak +
        'An optional length M can be given for this type. If this is done, MySQL ' +
        'creates the column as the smallest TEXT type large enough to hold ' +
        'values M characters long.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         65535;
      DefaultSize:     65535;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtMediumtext;
      NativeType:      mytMediumblob;
      Name:            'MEDIUMTEXT';
      Description:     'MEDIUMTEXT' + sLineBreak +
        'A TEXT column with a maximum length of 16,777,215 (2^24 - 1) characters. ' +
        'The effective maximum length is less if the value contains multi-byte ' +
        'characters. Each MEDIUMTEXT value is stored using a three-byte length ' +
        'prefix that indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtLongtext;
      NativeType:      mytLongblob;
      Name:            'LONGTEXT';
      Description:     'LONGTEXT' + sLineBreak +
        'A TEXT column with a maximum length of 4,294,967,295 or 4GB (2^32 - 1) ' +
        'characters. The effective maximum length is less if the value contains ' +
        'multi-byte characters. The effective maximum length of LONGTEXT columns ' +
        'also depends on the configured maximum packet size in the client/server ' +
        'protocol and available memory. Each LONGTEXT value is stored using a ' +
        'four-byte length prefix that indicates the number of bytes in the ' +
        'value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtJson;
      NativeType:      mytJson;
      Name:            'JSON';
      Description:     'JSON' + sLineBreak +
        'Documents stored in JSON columns are converted to an internal format that '+
        'permits quick read access to document elements. When the server later must '+
        'read a JSON value stored in this binary format, the value need not be parsed '+
        'from a text representation. The binary format is structured to enable the '+
        'server to look up subobjects or nested values directly by key or array index '+
        'without reading all values before or after them in the document.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dtBinary;
      NativeType:      mytString;
      Name:            'BINARY';
      Description:     'BINARY(M)' + sLineBreak +
        'The BINARY type is similar to the CHAR type, but stores binary byte ' +
        'strings rather than non-binary character strings. M represents the ' +
        'column length in bytes.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '50';
      Category:        dtcBinary;
    ),
    (
      Index:           dtVarbinary;
      NativeType:      mytVarstring;
      Name:            'VARBINARY';
      Description:     'VARBINARY(M)' + sLineBreak +
        'The VARBINARY type is similar to the VARCHAR type, but stores binary ' +
        'byte strings rather than non-binary character strings. M represents the ' +
        'maximum column length in bytes.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcBinary;
    ),
    (
      Index:           dtTinyblob;
      NativeType:      mytTinyblob;
      Name:           'TINYBLOB';
      Description:     'TINYBLOB' + sLineBreak +
        'A BLOB column with a maximum length of 255 (2^8 - 1) bytes. Each ' +
        'TINYBLOB value is stored using a one-byte length prefix that indicates ' +
        'the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dtBlob;
      NativeType:      mytBlob;
      Name:            'BLOB';
      Description:     'BLOB[(M)]' + sLineBreak +
        'A BLOB column with a maximum length of 65,535 (2^16 - 1) bytes. Each ' +
        'BLOB value is stored using a two-byte length prefix that indicates the ' +
        'number of bytes in the value. ' + sLineBreak +
        'An optional length M can be given for this type. If this is done, MySQL ' +
        'creates the column as the smallest BLOB type large enough to hold ' +
        'values M bytes long.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         65535;
      DefaultSize:     65535;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dtMediumblob;
      NativeType:      mytMediumblob;
      Name:            'MEDIUMBLOB';
      Description:     'MEDIUMBLOB' + sLineBreak +
        'A BLOB column with a maximum length of 16,777,215 (2^24 - 1) bytes. Each ' +
        'MEDIUMBLOB value is stored using a three-byte length prefix that ' +
        'indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dtLongblob;
      NativeType:      mytLongblob;
      Name:            'LONGBLOB';
      Description:     'LONGBLOB' + sLineBreak +
        'A BLOB column with a maximum length of 4,294,967,295 or 4GB (2^32 - 1) ' +
        'bytes. The effective maximum length of LONGBLOB columns depends on the ' +
        'configured maximum packet size in the client/server protocol and ' +
        'available memory. Each LONGBLOB value is stored using a four-byte ' +
        'length prefix that indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dtEnum;
      NativeType:      mytEnum;
      Name:            'ENUM';
      Description:     'ENUM(''value1'',''value2'',...)' + sLineBreak +
        'An enumeration. A string object that can have only one value, chosen ' +
        'from the list of values ''value1'', ''value2'', ..., NULL or the special '''' ' +
        'error value. An ENUM column can have a maximum of 65,535 distinct ' +
        'values. ENUM values are represented internally as integers.';
      HasLength:       True; // Obviously this is not meant as "length", but as "set of values"
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Y'',''N''';
      Category:        dtcOther;
    ),
    (
      Index:           dtSet;
      NativeType:      mytSet;
      Name:            'SET';
      Description:     'SET(''value1'',''value2'',...)' + sLineBreak +
        'A set. A string object that can have zero or more values, each of which ' +
        'must be chosen from the list of values ''value1'', ''value2'', ... A SET ' +
        'column can have a maximum of 64 members. SET values are represented ' +
        'internally as integers.';
      HasLength:       True; // Same as for ENUM
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Value A'',''Value B''';
      Category:        dtcOther;
    ),
    (
      Index:           dtBit;
      NativeType:      mytBit;
      Name:            'BIT';
      Description:     'BIT[(M)]' + sLineBreak +
        'A bit-field type. M indicates the number of bits per value, from 1 to ' +
        '64. The default is 1 if M is omitted.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtPoint;
      NativeType:      mytGeometry;
      Name:            'POINT';
      Description:     'POINT(x,y)' + sLineBreak +
        'Constructs a WKB Point using its coordinates.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtLinestring;
      NativeType:      mytGeometry;
      Name:            'LINESTRING';
      Description:     'LINESTRING(pt1,pt2,...)' + sLineBreak +
        'Constructs a WKB LineString value from a number of WKB Point arguments. ' +
        'If any argument is not a WKB Point, the return value is NULL. If the ' +
        'number of Point arguments is less than two, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtPolygon;
      NativeType:      mytGeometry;
      Name:            'POLYGON';
      Description:     'POLYGON(ls1,ls2,...)' + sLineBreak +
        'Constructs a WKB Polygon value from a number of WKB LineString ' +
        'arguments. If any argument does not represent the WKB of a LinearRing ' +
        '(that is, not a closed and simple LineString) the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtGeometry;
      NativeType:      mytGeometry;
      Name:            'GEOMETRY';
      Description:     '';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtMultipoint;
      NativeType:      mytGeometry;
      Name:            'MULTIPOINT';
      Description:     'MULTIPOINT(pt1,pt2,...)' + sLineBreak +
        'Constructs a WKB MultiPoint value using WKB Point arguments. If any ' +
        'argument is not a WKB Point, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtMultilinestring;
      NativeType:      mytGeometry;
      Name:            'MULTILINESTRING';
      Description:     'MULTILINESTRING(ls1,ls2,...)' + sLineBreak +
        'Constructs a WKB MultiLineString value using WKB LineString arguments. ' +
        'If any argument is not a WKB LineString, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtMultipolygon;
      NativeType:      mytGeometry;
      Name:            'MULTIPOLYGON';
      Description:     'MULTIPOLYGON(poly1,poly2,...)' + sLineBreak +
        'Constructs a WKB MultiPolygon value from a set of WKB Polygon ' +
        'arguments. If any argument is not a WKB Polygon, the return value is ' +
        'NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtGeometrycollection;
      NativeType:      mytGeometry;
      Name:            'GEOMETRYCOLLECTION';
      Description:     'GEOMETRYCOLLECTION(g1,g2,...)' + sLineBreak +
        'Constructs a WKB GeometryCollection. If any argument is not a ' +
        'well-formed WKB representation of a geometry, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    )

  );

  MSSQLDatatypes: array [0..33] of TDBDatatype =
  (
    (
      Index:           dtUnknown;
      NativeTypes:     '99999';
      Name:            'UNKNOWN';
      Description:     'Unknown data type';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index: dtTinyint;
      Name:            'TINYINT';
      Description:     'Integer data from 0 through 255.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtSmallint;
      Name:            'SMALLINT';
      Description:     'Integer data from -2^15 (-32,768) through 2^15 - 1 (32,767).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtInt;
      Name:            'INT';
      Description:     'Integer (whole number) data from -2^31 (-2,147,483,648) through 2^31 - 1 (2,147,483,647).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtBigint;
      Name:            'BIGINT';
      Description:     'Integer (whole number) data from -2^63 (-9,223,372,036,854,775,808) through 2^63-1 (9,223,372,036,854,775,807).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtBit;
      Name:            'BIT';
      Description:     '0 or 1';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtDecimal;
      Name:            'DECIMAL';
      Description:     'Fixed precision and scale numeric data from -10^38 +1 through 10^38 1.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '10,0';
      Category:        dtcReal;
    ),
    (
      Index:           dtNumeric;
      Name:            'NUMERIC';
      Description:     'Functionally equivalent to decimal.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '10,0';
      Category:        dtcReal;
    ),
    (
      Index:           dtMoney;
      Name:            'MONEY';
      Description:     'Monetary data values from -2^63 (-922,337,203,685,477.5808) through 2^63 - 1 (+922,337,203,685,477.5807), with accuracy to a ten-thousandth of a monetary unit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtSmallmoney;
      Name:            'SMALLMONEY';
      Description:     'Monetary data values from -214,748.3648 through +214,748.3647, with accuracy to a ten-thousandth of a monetary unit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtFloat;
      Name:            'FLOAT';
      Description:     'Floating precision number data with the following valid values: -1.79E + 308 through -2.23E - 308, 0 and 2.23E + 308 through 1.79E + 308.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtReal;
      Name:            'REAL';
      Description:     'Floating precision number data with the following valid values: -3.40E + 38 through -1.18E - 38, 0 and 1.18E - 38 through 3.40E + 38.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtTime;
      Name:            'TIME';
      Description:     'The time data type stores time values only, based on a 24-hour clock. '+
        'The time data type has a range of 00:00:00.0000000 through 23:59:59.9999999 with an '+
        'accuracy of 100 nanoseconds. The default value is 00:00:00.0000000 (midnight). The '+
        'time data type supports user-defined fractional second precision, and the storage '+
        'size varies from 3 to 6 bytes, based on the precision specified.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDate;
      Name:            'DATE';
      Description:     'The date data type has a range of January 1, 01 through December 31, '+
        '9999 with an accuracy of 1 day. The default value is January 1, 1900. The storage size '+
        'is 3 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDatetime;
      Name:            'DATETIME';
      Description:     'Date and time data from January 1, 1753, through December 31, 9999, with an accuracy of three-hundredths of a second, or 3.33 milliseconds.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDatetime2;
      Name:            'DATETIME2';
      Description:     'Date and time data from January 1,1 AD through December 31, 9999 AD, with an accuracy of three-hundredths of a second, or 3.33 milliseconds.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzzzzzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDatetimeOffset;
      Name:            'DATETIMEOFFSET';
      Description:     'Defines a date that is combined with a time of a day that has time zone awareness and is based on a 24-hour clock.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzzzzzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtSmalldatetime;
      Name:            'SMALLDATETIME';
      Description:     'Date and time data from January 1, 1900, through June 6, 2079, with an accuracy of one minute.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtTimestamp;
      Name:            'TIMESTAMP';
      Description:     'A database-wide unique number that gets updated every time a row gets updated.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtChar;
      Name:            'CHAR';
      Description:     'Fixed-length non-Unicode character data with a maximum length of 8,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtVarchar;
      Name:            'VARCHAR';
      Description:     'Variable-length non-Unicode data with a maximum of 8,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtText;
      Name:            'TEXT';
      Description:     'Variable-length non-Unicode data with a maximum length of 2^31 - 1 (2,147,483,647) characters.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtNchar;
      Name:            'NCHAR';
      Description:     'Fixed-length Unicode data with a maximum length of 4,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtNvarchar;
      Name:            'NVARCHAR';
      Description:     'Variable-length Unicode data with a maximum length of 4,000 characters. sysname is a system-supplied user-defined data type that is functionally equivalent to nvarchar(128) and is used to reference database object names.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtNtext;
      Name:            'NTEXT';
      Description:     'Variable-length Unicode data with a maximum length of 2^30 - 1 (1,073,741,823) characters.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtBinary;
      Name:            'BINARY';
      Description:     'Fixed-length binary data with a maximum length of 8,000 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dtVarbinary;
      Name:            'VARBINARY';
      Description:     'Variable-length binary data with a maximum length of 8,000 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dtImage;
      Name:            'IMAGE';
      Description:     'Variable-length binary data with a maximum length of 2^31 - 1 (2,147,483,647) bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dtCursor;
      Name:            'CURSOR';
      Description:     'A reference to a cursor.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtSqlvariant;
      Name:            'SQL_VARIANT';
      Description:     'A data type that stores values of various SQL Server-supported data types, except text, ntext, timestamp, and sql_variant.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtTable;
      Name:            'TABLE';
      Description:     'A special data type used to store a result set for later processing .';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtUniqueidentifier;
      Name:            'UNIQUEIDENTIFIER';
      Description:     'A globally unique identifier (GUID).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtHierarchyid;
      Name:            'HIERARCHYID';
      Description:     'Represents a position in a hierarchy.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtXML;
      Name:            'XML';
      Description:     'Lets you store XML documents and fragments.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    )
  );

  PostgreSQLDatatypes: Array[0..37] of TDBDatatype =
  (
    (
      Index:           dtUnknown;
      NativeTypes:     '99999';
      Name:            'UNKNOWN';
      Description:     'Unknown data type';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtSmallint;
      NativeTypes:     '21';
      Name:            'SMALLINT';
      Names:           'smallint|int2';
      Description:     'Small-range integer. Range: -32768 to +32767. Storage Size: 2 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\d{1,5}$';
      Category:        dtcInteger;
    ),
    (
      Index:           dtInt;
      // 26 = oid, 28 = xid
      NativeTypes:     '23|26|28';
      Name:            'INTEGER';
      Names:           'integer|int4|int|oid|xid';
      Description:     'Typical choice for integer. Range: -2147483648 to +2147483647. Storage Size: 4 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\d{1,10}$';
      Category:        dtcInteger;
    ),
    (
      Index:           dtBigint;
      NativeTypes:     '20';
      Name:            'BIGINT';
      Names:           'bigint|int8';
      Description:     'Large-range integer. Range: -9223372036854775808 to 9223372036854775807. Storage Size: 8 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\d{1,19}$';
      Category:        dtcInteger;
    ),
    (
      Index:           dtSerial;
      Name:            'SERIAL';
      Names:           'serial|serial4';
      Description:     'Autoincrementing integer. Range: 1 to 2147483647. Storage Size: 4 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtBigSerial;
      Name:            'BIGSERIAL';
      Names:           'bigserial|serial8';
      Description:     'Large autoincrementing integer. Range: 1 to 9223372036854775807. Storage Size: 8 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtVarBit;
      NativeTypes:     '1562';
      Name:            'BIT VARYING';
      Names:           'bit varying|varbit';
      Description:     'Variable-length bit string.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtBit;
      NativeTypes:     '1560';
      Name:            'BIT';
      Names:           'bit';
      Description:     'Fixed-length bit string.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtNumeric;
      NativeTypes:     '1700';
      Name:            'NUMERIC';
      Names:           'numeric|float8|decimal';
      Description:     'User-specified precision, exact. Range: no limit. Storage Size: variable.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtReal;
      NativeTypes:     '700';
      Name:            'REAL';
      Names:           'real|float4';
      Description:     'Variable-precision, inexact. Range: 6 decimal digits precision. Storage Size: 4 Bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtDoublePrecision;
      NativeTypes:     '701|1700';
      Name:            'DOUBLE PRECISION';
      Names:           'double precision|float8';
      Description:     'Variable-precision, inexact. Range: 15 decimal digits precision. Storage Size: 8 Bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtMoney;
      NativeTypes:     '790';
      Name:            'MONEY';
      Description:     'Currency amount. Range: -92233720368547758.08 to +92233720368547758.07. Storage Size: 8 Bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtChar;
      NativeTypes:     '18|1042';
      Name:            'CHAR';
      Names:           'CHARACTER';
      Description:     'Fixed-length, blank padded.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtVarchar;
      NativeTypes:     '18|19|24|1043|1043';
      Name:            'VARCHAR';
      Names:           'char|bpchar|varchar|name|enum|character varying';
      Description:     'Variable-length with limit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtText;
      NativeTypes:     '25|22|30|143|629|651|719|791|1000|1028|1040|1041|1115|1182|1183|1185|1187|1231|1263|1270|1561|1563|2201|2207|2211|2949|2951|3643|3644|3645|3735|3770';
      Name:            'TEXT';
      Names:           'text|int2vector|oidvector|bool';
      Description:     'Variable unlimited length.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtCidr;
      NativeTypes:     '650';
      Name:            'CIDR';
      Names:           'cidr';
      Description:     'IPv4 and IPv6 networks. Storage size: 7 or 19 bytes';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dtInet;
      NativeTypes:     '869';
      Name:            'INET';
      Names:           'inet';
      Description:     'IPv4 and IPv6 hosts and networks. Storage size: 7 or 19 bytes';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dtMacaddr;
      NativeTypes:     '829';
      Name:            'MACADDR';
      Names:           'macaddr';
      Description:     'MAC addresses. Storage size: 6 bytes';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dtDate;
      NativeTypes:     '1082';
      Name:            'DATE';
      Description:     'Calendar date (year, month, day).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtTime;
      NativeTypes:     '1083';
      Name:            'TIME';
      Description:     'Time of day.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDatetime;
      NativeTypes:     '1082|1114|702';
      Name:            'TIMESTAMP';
      Names:           'timestamp|datetime|abstime|timestamp without time zone';
      Description:     'Date and time without timezone, e.g. "2020-06-27 16:24:41".';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDatetime2;
      NativeTypes:     '1184';
      Name:            'TIMESTAMPTZ';
      Names:           'timestamptz|timestamp with time zone';
      Description:     'Date and time with time zone, e.g. "2020-06-27 16:24:41+02".';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDate;
      NativeTypes:     '1082';
      Name:            'DATE';
      Description:     'Calendar date (year, month, day).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtInterval;
      NativeTypes:     '1186';
      Name:            'INTERVAL';
      Description:     'time interval	from -178000000 years to 178000000 years';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dtBlob;
      NativeTypes:     '17';
      Name:            'BYTEA';
      Description:     'Binary data ("byte array").';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dtPoint;
      NativeTypes:     '600';
      Name:            'POINT';
      Description:     'Point on a plane (x,y). Storage size: 16 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtLinestring;
      NativeTypes:     '628';
      Name:            'LINE';
      Description:     'Infinite line ((x1,y1),(x2,y2)). Storage size: 32 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtLineSegment;
      NativeTypes:     '601';
      Name:            'LSEG';
      Description:     'Finite line segment ((x1,y1),(x2,y2)). Storage size: 32 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtBox;
      NativeTypes:     '603';
      Name:            'BOX';
      Description:     'Rectangular box ((x1,y1),(x2,y2)). Storage size: 32 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtPath;
      NativeTypes:     '602';
      Name:            'PATH';
      Description:     'Closed path (similar to polygon) ((x1,y1),...). Storage size: 16+16n bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtPolygon;
      NativeTypes:     '604';
      Name:            'POLYGON';
      Description:     'Closed path (similar to polygon) ((x1,y1),...). Storage size: 40+16n bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtCircle;
      NativeTypes:     '718';
      Name:            'CIRCLE';
      Description:     'Circle <(x,y),r> (center point and radius). Storage size: 24 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dtBool;
      NativeTypes:     '16';
      Name:            'BOOLEAN';
      Names:           'boolean|bool';
      Description:     'State of true or false. Storage size: 1 byte.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^(true|false)$';
      Category:        dtcOther;
    ),
    (
      Index:           dtRegClass;
      NativeTypes:     '2205';
      Name:            'REGCLASS';
      Names:           'regclass';
      Description:     'Relation name';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtRegProc;
      NativeTypes:     '24';
      Name:            'REGPROC';
      Names:           'regproc|regprocedure';
      Description:     'Function name';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtJson;
      NativeTypes:     '114';
      Name:            'JSON';
      Names:           'json';
      Description:     'JavaScript Object Notation data';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dtJsonB;
      NativeTypes:     '3802';
      Name:            'JSONB';
      Names:           'jsonb';
      Description:     'JavaScript Object Notation data in a binary form';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dtUniqueidentifier;
      NativeTypes:     '2950';
      Name:            'UUID';
      Names:           'uuid';
      Description:     'The data type uuid stores Universally Unique Identifiers (UUID) as defined by RFC 4122, ISO/IEC 9834-8:2005, and related standards.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\{?[a-f0-9]{8}-?[a-f0-9]{4}-?[a-f0-9]{4}-?[a-f0-9]{4}-?[a-f0-9]{12}\}?$';
      Category:        dtcText;
    )
  );

  SQLiteDatatypes: Array[0..14] of TDBDatatype =
  (
    (
      Index:           dtUnknown;
      Name:            'UNKNOWN';
      Description:     'Unknown data type';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dtTinyint;
      Name:            'TINYINT';
      Names:           'INT2|BOOLEAN|BOOL';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtInt;
      Name:            'INTEGER';
      Names:           'INT|MEDIUMINT|INT8';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtBigint;
      Name:            'BIGINT';
      Names:           'UNSIGNED BIG INT';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dtChar;
      Name:            'CHAR';
      Names:           'CHARACTER|CHAR|NCHAR|NATIVE CHARACTER';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtVarchar;
      Name:            'VARCHAR';
      Names:           'VARCHAR|VARYING CHARACTER|NVARCHAR|CHARACTER|CHAR|NCHAR|NATIVE CHARACTER';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dtText;
      Name:            'TEXT';
      Names:           'CLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dtUniqueidentifier;
      Name:            'UNIQUEIDENTIFIER';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dtBlob;
      Name:            'BLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dtReal;
      Name:            'REAL';
      Names:           'REAL|NUMERIC|DOUBLE|DOUBLE PRECISION|FLOAT|DECIMAL';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dtDate;
      Name:            'DATE';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dtTime;
      Name:            'TIME';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dtDatetime;
      Name:            'DATETIME';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dtEnum;
      NativeType:      mytEnum;
      Name:            'ENUM';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Y'',''N''';
      Category:        dtcOther;
    ),
    (
      Index:           dtSet;
      NativeType:      mytSet;
      Name:            'SET';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Value A'',''Value B''';
      Category:        dtcOther;
    )
  );


  MySqlFunctions: Array [0..375] of TMysqlFunction =
  (
    (
      Name:         'BIT_COUNT';
      Declaration:  '(N)';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of bits that are set in the argument N.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT BIT_COUNT(29), BIT_COUNT(b''101010'');'+sLineBreak
        +'+---------------+----------------------+'+sLineBreak
        +'| BIT_COUNT(29) | BIT_COUNT(b''101010'') |'+sLineBreak
        +'+---------------+----------------------+'+sLineBreak
        +'| 4 | 3 |'+sLineBreak
        +'+---------------+----------------------+'
    ),

    (
      Name:         'COALESCE';
      Declaration:  '(value,...)';
      Category:     'Comparison Operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the first non-NULL value in the list, or NULL if'+sLineBreak
        +'there are no'+sLineBreak
        +'non-NULL values. At least one parameter must be passed.'+sLineBreak
        +' '+sLineBreak
        +'See also NULL Values in MariaDB.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT COALESCE(NULL,1);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| COALESCE(NULL,1) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT COALESCE(NULL,NULL,NULL);'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| COALESCE(NULL,NULL,NULL) |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +' '+sLineBreak
        +'When two arguments are given, COALESCE() is the same as'+sLineBreak
        +'IFNULL():'+sLineBreak
        +' '+sLineBreak
        +'SET @a=NULL, @b=1;'+sLineBreak
        +' '+sLineBreak
        +'SELECT COALESCE(@a, @b), IFNULL(@a, @b);'+sLineBreak
        +'+------------------+----------------+'+sLineBreak
        +'| COALESCE(@a, @b) | IFNULL(@a, @b) |'+sLineBreak
        +'+------------------+----------------+'+sLineBreak
        +'| 1 | 1 |'+sLineBreak
        +'+------------------+----------------+'+sLineBreak
        +' '+sLineBreak
        +'Hex type confusion:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (a INT, b VARCHAR(10));'+sLineBreak
        +'INSERT INTO t1 VALUES (0x31, 0x61),(COALESCE(0x31),'+sLineBreak
        +'COALESCE(0x61));'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+'+sLineBreak
        +'| a | b |'+sLineBreak
        +'+------+------+'+sLineBreak
        +'| 49 | a |'+sLineBreak
        +'| 1 | a |'+sLineBreak
        +'+------+------+'+sLineBreak
        +' '+sLineBreak
        +'The reason for the differing results above is that when 0x31'+sLineBreak
        +'is inserted directly to the column, it''s treated as a'+sLineBreak
        +'number (see Hexadecimal Literals), while when 0x31 is passed'+sLineBreak
        +'to COALESCE(), it''s treated as a string, because:'+sLineBreak
        +'HEX values have a string data type by default.'+sLineBreak
        +'COALESCE() has the same data type as the argument.'
    ),

    (
      Name:         'GREATEST';
      Declaration:  '(value1,value2,...)';
      Category:     'Comparison Operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'With two or more arguments, returns the largest'+sLineBreak
        +'(maximum-valued)'+sLineBreak
        +'argument. The arguments are compared using the same rules as'+sLineBreak
        +'for'+sLineBreak
        +'LEAST().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT GREATEST(2,0);'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| GREATEST(2,0) |'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT GREATEST(34.0,3.0,5.0,767.0);'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| GREATEST(34.0,3.0,5.0,767.0) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| 767.0 |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT GREATEST(''B'',''A'',''C'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| GREATEST(''B'',''A'',''C'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| C |'+sLineBreak
        +'+-----------------------+'
    ),

    (
      Name:         'INTERVAL';
      Declaration:  '(N,N1,N2,N3,...)';
      Category:     'Comparison Operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the index of the last argument that is less than the'+sLineBreak
        +'first argument or is NULL. '+sLineBreak
        +' '+sLineBreak
        +'Returns 0 if N < N1, 1 if N < N2, 2 if N < N3 and so on or'+sLineBreak
        +'-1 if N is NULL. All'+sLineBreak
        +'arguments are treated as integers. It is required that N1 <'+sLineBreak
        +'N2 < N3'
    ),

    (
      Name:         'ISNULL';
      Declaration:  '(expr)';
      Category:     'Comparison Operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'If expr is NULL, ISNULL() returns 1, otherwise it returns 0.'+sLineBreak
        +' '+sLineBreak
        +'See also NULL Values in MariaDB.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ISNULL(1+1);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ISNULL(1+1) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ISNULL(1/0);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ISNULL(1/0) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------+'
    ),

    (
      Name:         'LEAST';
      Declaration:  '(value1,value2,...)';
      Category:     'Comparison Operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'With two or more arguments, returns the smallest'+sLineBreak
        +'(minimum-valued)'+sLineBreak
        +'argument. The arguments are compared using the following'+sLineBreak
        +'rules:'+sLineBreak
        +'If the return value is used in an INTEGER context or all'+sLineBreak
        +'arguments are integer-valued, they are compared as integers.'+sLineBreak
        +'If the return value is used in a REAL context or all'+sLineBreak
        +'arguments are real-valued, they are compared as reals.'+sLineBreak
        +'If any argument is a case-sensitive string, the arguments'+sLineBreak
        +'are compared as case-sensitive strings.'+sLineBreak
        +'In all other cases, the arguments are compared as'+sLineBreak
        +'case-insensitive strings.'+sLineBreak
        +' '+sLineBreak
        +'LEAST() returns NULL if any argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LEAST(2,0);'+sLineBreak
        +'+------------+'+sLineBreak
        +'| LEAST(2,0) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LEAST(34.0,3.0,5.0,767.0);'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| LEAST(34.0,3.0,5.0,767.0) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 3.0 |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LEAST(''B'',''A'',''C'');'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| LEAST(''B'',''A'',''C'') |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| A |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'IFNULL';
      Declaration:  '(expr1,expr2)';
      Category:     'Control Flow Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If expr1 is not NULL, IFNULL() returns expr1; otherwise it'+sLineBreak
        +'returns'+sLineBreak
        +'expr2. IFNULL() returns a numeric or string value, depending'+sLineBreak
        +'on the'+sLineBreak
        +'context in which it is used.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT IFNULL(1,0); '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| IFNULL(1,0) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IFNULL(NULL,10);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| IFNULL(NULL,10) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 10 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IFNULL(1/0,10);'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| IFNULL(1/0,10) |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| 10.0000 |'+sLineBreak
        +'+----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IFNULL(1/0,''yes'');'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| IFNULL(1/0,''yes'') |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| yes |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'NULLIF';
      Declaration:  '(expr1,expr2)';
      Category:     'Control Flow Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns NULL if expr1 = expr2 is true, otherwise returns'+sLineBreak
        +'expr1. This is'+sLineBreak
        +'the same as CASE WHEN expr1 = expr2 THEN NULL ELSE expr1'+sLineBreak
        +'END.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT NULLIF(1,1);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| NULLIF(1,1) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT NULLIF(1,2);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| NULLIF(1,2) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------+'
    ),

    (
      Name:         'BINARY';
      Declaration:  '(M)';
      Category:     'Data Types';
      Version:      SQL_VERSION_ANSI;
      Description:  'The BINARY type is similar to the CHAR type, but stores'+sLineBreak
        +'binary'+sLineBreak
        +'byte strings rather than non-binary character strings. M'+sLineBreak
        +'represents the'+sLineBreak
        +'column length in bytes.'+sLineBreak
        +' '+sLineBreak
        +'It contains no character set, and comparison and sorting are'+sLineBreak
        +'based on the numeric value of the bytes.'+sLineBreak
        +' '+sLineBreak
        +'If the maximum length is exceeded, and SQL strict mode is'+sLineBreak
        +'not enabled , the extra characters will be dropped with a'+sLineBreak
        +'warning. If strict mode is enabled, an error will occur.'+sLineBreak
        +' '+sLineBreak
        +'BINARY values are right-padded with 0x00 (the zero byte) to'+sLineBreak
        +'the specified length when inserted. The padding is not'+sLineBreak
        +'removed on select, so this needs to be taken into account'+sLineBreak
        +'when sorting and comparing, where all bytes are significant.'+sLineBreak
        +'The zero byte, 0x00 is less than a space for comparison'+sLineBreak
        +'purposes.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Inserting too many characters, first with strict mode off,'+sLineBreak
        +'then with it on:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE bins (a BINARY(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO bins VALUES(''12345678901'');'+sLineBreak
        +'Query OK, 1 row affected, 1 warning (0.04 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM bins;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 1234567890 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SET sql_mode=''STRICT_ALL_TABLES'';'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO bins VALUES(''12345678901'');'+sLineBreak
        +'ERROR 1406 (22001): Data too long for column ''a'' at row 1'+sLineBreak
        +' '+sLineBreak
        +'Sorting is performed with the byte value:'+sLineBreak
        +' '+sLineBreak
        +'TRUNCATE bins;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO bins VALUES(''A''),(''B''),(''a''),(''b'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM bins ORDER BY a;'+sLineBreak
        +' '+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------+'+sLineBreak
        +'| A |'+sLineBreak
        +'| B |'+sLineBreak
        +'| a |'+sLineBreak
        +'| b |'+sLineBreak
        +'+------+'+sLineBreak
        +' '+sLineBreak
        +'Using CAST to sort as a CHAR instead:'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM bins ORDER BY CAST(a AS CHAR);'+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'| A |'+sLineBreak
        +'| b |'+sLineBreak
        +'| B |'+sLineBreak
        +'+------+'+sLineBreak
        +' '+sLineBreak
        +'The field is a BINARY(10), so padding of two ''\0''s are'+sLineBreak
        +'inserted, causing comparisons that don''t take this into'+sLineBreak
        +'account to fail:'+sLineBreak
        +' '+sLineBreak
        +'TRUNCATE bins;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO bins VALUES(''12345678'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT a = ''12345678'', a = ''12345678\0\0'' from bins;'+sLineBreak
        +' '+sLineBreak
        +'+----------------+--------------------+'+sLineBreak
        +'| a = ''12345678'' | a = ''12345678\0\0'' |'+sLineBreak
        +'+----------------+--------------------+'+sLineBreak
        +'| 0 | 1 |'+sLineBreak
        +'+----------------+--------------------+'
    ),

    (
      Name:         'ENUM';
      Declaration:  '(''value1'',''value2'',...)';
      Category:     'Data Types';
      Version:      SQL_VERSION_ANSI;
      Description:  'An enumeration. A string object that can have only one'+sLineBreak
        +'value, chosen'+sLineBreak
        +'from the list of values ''value1'', ''value2'', ..., NULL or'+sLineBreak
        +'the special '+sLineBreak
        +''''' error value. In theory, an ENUM column can have a'+sLineBreak
        +'maximum of 65,535 distinct'+sLineBreak
        +'values; in practice, the real maximum depends on many'+sLineBreak
        +'factors. ENUM values are represented internally as integers.'+sLineBreak
        +' '+sLineBreak
        +'Trailing spaces are automatically stripped from ENUM values'+sLineBreak
        +'on table creation.'+sLineBreak
        +' '+sLineBreak
        +'ENUMs require relatively little storage space compared to'+sLineBreak
        +'strings, either one or two bytes depending on the number of'+sLineBreak
        +'enumeration values.'+sLineBreak
        +' '+sLineBreak
        +'NULL and empty values'+sLineBreak
        +' '+sLineBreak
        +'An ENUM can also contain NULL and empty values. If the ENUM'+sLineBreak
        +'column is declared to permit NULL values, NULL becomes a'+sLineBreak
        +'valid value, as well as the default value (see below). If'+sLineBreak
        +'strict SQL Mode is not enabled, and an invalid value is'+sLineBreak
        +'inserted into an ENUM, a special empty string, with an index'+sLineBreak
        +'value of zero (see Numeric index, below), is inserted, with'+sLineBreak
        +'a warning. This may be confusing, because the empty string'+sLineBreak
        +'is also a possible value, and the only difference if that in'+sLineBreak
        +'this case its index is not 0. Inserting will fail with an'+sLineBreak
        +'error if strict mode is active.'+sLineBreak
        +' '+sLineBreak
        +'If a DEFAULT clause is missing, the default value will be:'+sLineBreak
        +'NULL is the column is nullable;'+sLineBreak
        +'otherwise, the first value in the enumaration.'+sLineBreak
        +' '+sLineBreak
        +'Numeric index'+sLineBreak
        +' '+sLineBreak
        +'ENUM values are indexed numerically in the order they are'+sLineBreak
        +'defined, and sorting will be performed in this numeric'+sLineBreak
        +'order. We suggest not using ENUM to store numerals, as there'+sLineBreak
        +'is little to no storage space benefit, and it is easy to'+sLineBreak
        +'confuse the enum integer with the enum numeral value by'+sLineBreak
        +'leaving out the quotes.'+sLineBreak
        +' '+sLineBreak
        +'An ENUM defined as ENUM(''apple'',''orange'',''pear'') would'+sLineBreak
        +'have the following index values:'+sLineBreak
        +' '+sLineBreak
        +'Index | Value | '+sLineBreak
        +' '+sLineBreak
        +'NULL | NULL | '+sLineBreak
        +' '+sLineBreak
        +'0 | '''' | '+sLineBreak
        +' '+sLineBreak
        +'1 | ''apple'' | '+sLineBreak
        +' '+sLineBreak
        +'2 | ''orange'' | '+sLineBreak
        +' '+sLineBreak
        +'3 | ''pear'' | '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE fruits ('+sLineBreak
        +' id INT NOT NULL auto_increment PRIMARY KEY,'+sLineBreak
        +' fruit ENUM(''apple'',''orange'',''pear''),'+sLineBreak
        +' bushels INT);'+sLineBreak
        +' '+sLineBreak
        +'DESCRIBE fruits;'+sLineBreak
        +' '+sLineBreak
        +'+---------+-------------------------------+------+-----+---------+----------------+'+sLineBreak
        +'| Field | Type | Null | Key | Default | Extra |'+sLineBreak
        +'+---------+-------------------------------+------+-----+---------+----------------+'+sLineBreak
        +'| id | int(11) | NO | PRI | NULL | auto_increment |'+sLineBreak
        +'| fruit | enum(''apple'',''orange'',''pear'') | YES | | NULL'+sLineBreak
        +'| |'+sLineBreak
        +'| bushels | int(11) | YES | | NULL | |'+sLineBreak
        +'+---------+-------------------------------+------+-----+---------+----------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO fruits'+sLineBreak
        +' (fruit,bushels) VALUES'+sLineBreak
        +' (''pear'',20),'+sLineBreak
        +' (''apple'',100),'+sLineBreak
        +' (''orange'',25);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO fruits'+sLineBreak
        +' (fruit,bushels) VALUES'+sLineBreak
        +' (''avocado'',10);'+sLineBreak
        +'ERROR 1265 (01000): Data truncated for column ''fruit'' at'+sLineBreak
        +'row 1'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM fruits;'+sLineBreak
        +' '+sLineBreak
        +'+----+--------+---------+'+sLineBreak
        +'| id | fruit | bushels |'+sLineBreak
        +'+----+--------+---------+'+sLineBreak
        +'| 1 | pear | 20 |'+sLineBreak
        +'| 2 | apple | 100 |'+sLineBreak
        +'| 3 | orange | 25 |'+sLineBreak
        +'+----+--------+---------+'+sLineBreak
        +' '+sLineBreak
        +'Selecting by numeric index:'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM fruits WHERE fruit=2;'+sLineBreak
        +' '+sLineBreak
        +'+----+--------+---------+'+sLineBreak
        +'| id | fruit | bushels |'+sLineBreak
        +'+----+--------+---------+'+sLineBreak
        +'| 3 | orange | 25 |'+sLineBreak
        +'+----+--------+---------+'+sLineBreak
        +' '+sLineBreak
        +'Sorting is according to the index value:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE enums (a ENUM(''2'',''1''));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO enums VALUES (''1''),(''2'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM enums ORDER BY a ASC;'+sLineBreak
        +' '+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------+'+sLineBreak
        +' '+sLineBreak
        +'It''s easy to get confused between returning the enum'+sLineBreak
        +'integer with the stored value, so we don''t suggest using'+sLineBreak
        +'ENUM to store numerals. The first example returns the 1st'+sLineBreak
        +'indexed field (''2'' has an index value of 1, as it''s'+sLineBreak
        +'defined first), while the second example returns the string'+sLineBreak
        +'value ''1''.'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM enums WHERE a=1;'+sLineBreak
        +' '+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM enums WHERE a=''1'';'+sLineBreak
        +' '+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------+'
    ),

    (
      Name:         'VARBINARY';
      Declaration:  '(M)';
      Category:     'Data Types';
      Version:      SQL_VERSION_ANSI;
      Description:  'The VARBINARY type is similar to the VARCHAR type, but'+sLineBreak
        +'stores binary byte strings rather than non-binary character'+sLineBreak
        +'strings. M represents the maximum column length in bytes. '+sLineBreak
        +' '+sLineBreak
        +'It contains no character set, and comparison and sorting are'+sLineBreak
        +'based on the numeric value of the bytes.'+sLineBreak
        +' '+sLineBreak
        +'If the maximum length is exceeded, and SQL strict mode is'+sLineBreak
        +'not enabled , the extra characters will be dropped with a'+sLineBreak
        +'warning. If strict mode is enabled, an error will occur.'+sLineBreak
        +' '+sLineBreak
        +'Unlike BINARY values, VARBINARYs are not right-padded when'+sLineBreak
        +'inserting.'+sLineBreak
        +' '+sLineBreak
        +'Oracle Mode'+sLineBreak
        +' '+sLineBreak
        +'In Oracle mode from MariaDB 10.3, RAW is a synonym for'+sLineBreak
        +'VARBINARY.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Inserting too many characters, first with strict mode off,'+sLineBreak
        +'then with it on:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE varbins (a VARBINARY(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO varbins VALUES(''12345678901'');'+sLineBreak
        +'Query OK, 1 row affected, 1 warning (0.04 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM varbins;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 1234567890 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SET sql_mode=''STRICT_ALL_TABLES'';'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO varbins VALUES(''12345678901'');'+sLineBreak
        +'ERROR 1406 (22001): Data too long for column ''a'' at row 1'+sLineBreak
        +' '+sLineBreak
        +'Sorting is performed with the byte value:'+sLineBreak
        +' '+sLineBreak
        +'TRUNCATE varbins;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO varbins VALUES(''A''),(''B''),(''a''),(''b'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM varbins ORDER BY a;'+sLineBreak
        +' '+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------+'+sLineBreak
        +'| A |'+sLineBreak
        +'| B |'+sLineBreak
        +'| a |'+sLineBreak
        +'| b |'+sLineBreak
        +'+------+'+sLineBreak
        +' '+sLineBreak
        +'Using CAST to sort as a CHAR instead:'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM varbins ORDER BY CAST(a AS CHAR);'+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------+'+sLineBreak
        +'| a |'+sLineBreak
        +'| A |'+sLineBreak
        +'| b |'+sLineBreak
        +'| B |'+sLineBreak
        +'+------+'
    ),

    (
      Name:         'ADDDATE';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'When invoked with the INTERVAL form of the second argument,'+sLineBreak
        +'ADDDATE()'+sLineBreak
        +'is a synonym for DATE_ADD(). The related function'+sLineBreak
        +'SUBDATE() is a synonym for DATE_SUB(). For'+sLineBreak
        +'information on the INTERVAL unit argument, see the'+sLineBreak
        +'discussion for'+sLineBreak
        +'DATE_ADD().'+sLineBreak
        +' '+sLineBreak
        +'When invoked with the days form of the second argument,'+sLineBreak
        +'MariaDB treats it as an'+sLineBreak
        +'integer number of days to be added to expr.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATE_ADD(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| DATE_ADD(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| 2008-02-02 |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ADDDATE(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| ADDDATE(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| 2008-02-02 |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ADDDATE(''2008-01-02'', 31);'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| ADDDATE(''2008-01-02'', 31) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 2008-02-02 |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, ADDDATE(d, 10) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| d | ADDDATE(d, 10) |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-02-09 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-25 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-05-01 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-11-09 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-02-09 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-10-17 11:19:34 |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, ADDDATE(d, INTERVAL 10 HOUR) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+------------------------------+'+sLineBreak
        +'| d | ADDDATE(d, INTERVAL 10 HOUR) |'+sLineBreak
        +'+---------------------+------------------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-01-31 07:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-15 16:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-04-21 22:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-10-30 16:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-01-31 00:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-10-07 21:19:34 |'+sLineBreak
        +'+---------------------+------------------------------+'
    ),

    (
      Name:         'ADDDATE';
      Declaration:  '(expr,days)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'When invoked with the INTERVAL form of the second argument,'+sLineBreak
        +'ADDDATE()'+sLineBreak
        +'is a synonym for DATE_ADD(). The related function'+sLineBreak
        +'SUBDATE() is a synonym for DATE_SUB(). For'+sLineBreak
        +'information on the INTERVAL unit argument, see the'+sLineBreak
        +'discussion for'+sLineBreak
        +'DATE_ADD().'+sLineBreak
        +' '+sLineBreak
        +'When invoked with the days form of the second argument,'+sLineBreak
        +'MariaDB treats it as an'+sLineBreak
        +'integer number of days to be added to expr.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATE_ADD(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| DATE_ADD(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| 2008-02-02 |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ADDDATE(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| ADDDATE(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| 2008-02-02 |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ADDDATE(''2008-01-02'', 31);'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| ADDDATE(''2008-01-02'', 31) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 2008-02-02 |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, ADDDATE(d, 10) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| d | ADDDATE(d, 10) |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-02-09 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-25 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-05-01 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-11-09 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-02-09 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-10-17 11:19:34 |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, ADDDATE(d, INTERVAL 10 HOUR) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+------------------------------+'+sLineBreak
        +'| d | ADDDATE(d, INTERVAL 10 HOUR) |'+sLineBreak
        +'+---------------------+------------------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-01-31 07:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-15 16:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-04-21 22:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-10-30 16:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-01-31 00:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-10-07 21:19:34 |'+sLineBreak
        +'+---------------------+------------------------------+'
    ),

    (
      Name:         'ADDTIME';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'ADDTIME() adds expr2 to expr1 and returns the result. expr1'+sLineBreak
        +'is a time'+sLineBreak
        +'or datetime expression, and expr2 is a time expression.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ADDTIME(''2007-12-31 23:59:59.999999'', ''1'+sLineBreak
        +'1:1:1.000002'');'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| ADDTIME(''2007-12-31 23:59:59.999999'', ''1'+sLineBreak
        +'1:1:1.000002'') |'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| 2008-01-02 01:01:01.000001 |'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ADDTIME(''01:00:00.999999'', ''02:00:00.999998'');'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| ADDTIME(''01:00:00.999999'', ''02:00:00.999998'') |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| 03:00:01.999997 |'+sLineBreak
        +'+-----------------------------------------------+'
    ),

    (
      Name:         'CONVERT_TZ';
      Declaration:  '(dt,from_tz,to_tz)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CONVERT_TZ() converts a datetime value dt from the time zone'+sLineBreak
        +'given by from_tz to the time zone given by to_tz and returns'+sLineBreak
        +'the resulting value.'+sLineBreak
        +' '+sLineBreak
        +'In order to use named time zones, such as GMT, MET or'+sLineBreak
        +'Africa/Johannesburg, the time_zone tables must be loaded'+sLineBreak
        +'(see mysql_tzinfo_to_sql).'+sLineBreak
        +' '+sLineBreak
        +'No conversion will take place if the value falls outside of'+sLineBreak
        +'the supported TIMESTAMP range (''1970-01-01 00:00:01'' to'+sLineBreak
        +'''2038-01-19 05:14:07'' UTC) when converted from from_tz to'+sLineBreak
        +'UTC.'+sLineBreak
        +' '+sLineBreak
        +'This function returns NULL if the arguments are invalid (or'+sLineBreak
        +'named time zones have not been loaded).'+sLineBreak
        +' '+sLineBreak
        +'See time zones for more information.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CONVERT_TZ(''2016-01-01'+sLineBreak
        +'12:00:00'',''+00:00'',''+10:00'');'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| CONVERT_TZ(''2016-01-01 12:00:00'',''+00:00'',''+10:00'')'+sLineBreak
        +'|'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| 2016-01-01 22:00:00 |'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Using named time zones (with the time zone tables loaded):'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONVERT_TZ(''2016-01-01'+sLineBreak
        +'12:00:00'',''GMT'',''Africa/Johannesburg'');'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +'| CONVERT_TZ(''2016-01-01'+sLineBreak
        +'12:00:00'',''GMT'',''Africa/Johannesburg'') |'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +'| 2016-01-01 14:00:00 |'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'The value is out of the TIMESTAMP range, so no conversion'+sLineBreak
        +'takes place:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONVERT_TZ(''1969-12-31'+sLineBreak
        +'22:00:00'',''+00:00'',''+10:00'');'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| CONVERT_TZ(''1969-12-31 22:00:00'',''+00:00'',''+10:00'')'+sLineBreak
        +'|'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| 1969-12-31 22:00:00 |'+sLineBreak
        +'+-----------------------------------------------------+'
    ),

    (
      Name:         'CURDATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current date as a value in ''YYYY-MM-DD'' or'+sLineBreak
        +'YYYYMMDD'+sLineBreak
        +'format, depending on whether the function is used in a'+sLineBreak
        +'string or'+sLineBreak
        +'numeric context.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CURDATE();'+sLineBreak
        +'+------------+'+sLineBreak
        +'| CURDATE() |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 2019-03-05 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'In a numeric context (note this is not performing date'+sLineBreak
        +'calculations):'+sLineBreak
        +' '+sLineBreak
        +'SELECT CURDATE() +0;'+sLineBreak
        +' '+sLineBreak
        +'+--------------+'+sLineBreak
        +'| CURDATE() +0 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 20190305 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'Data calculation:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CURDATE() - INTERVAL 5 DAY;'+sLineBreak
        +' '+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| CURDATE() - INTERVAL 5 DAY |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| 2019-02-28 |'+sLineBreak
        +'+----------------------------+'
    ),

    (
      Name:         'CURTIME';
      Declaration:  '([precision])';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current time as a value in ''HH:MM:SS'' or'+sLineBreak
        +'HHMMSS.uuuuuu format, depending on whether the function is'+sLineBreak
        +'used in a string or numeric context. The value is expressed'+sLineBreak
        +'in the current time zone.'+sLineBreak
        +' '+sLineBreak
        +'The optional precision determines the microsecond precision.'+sLineBreak
        +'See Microseconds in MariaDB.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CURTIME();'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| CURTIME() |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| 12:45:39 |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CURTIME() + 0;'+sLineBreak
        +' '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| CURTIME() + 0 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 124545.000000 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'With precision:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CURTIME(2);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| CURTIME(2) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 09:49:08.09 |'+sLineBreak
        +'+-------------+'
    ),

    // Added by hand, for https://github.com/HeidiSQL/HeidiSQL/issues/74#issuecomment-559321533
    // and again, for https://www.heidisql.com/forum.php?t=37278
    (
      Name:         'CURRENT_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CURRENT_TIMESTAMP and CURRENT_TIMESTAMP() are synonyms for NOW()'
    ),

    (
      Name:         'DATEDIFF';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'DATEDIFF() returns (expr1 ֠expr2) expressed'+sLineBreak
        +'as a value in days from one date to the other. expr1 and'+sLineBreak
        +'expr2 are date'+sLineBreak
        +'or date-and-time expressions. Only the date parts of the'+sLineBreak
        +'values are used in the'+sLineBreak
        +'calculation.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATEDIFF(''2007-12-31 23:59:59'',''2007-12-30'');'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| DATEDIFF(''2007-12-31 23:59:59'',''2007-12-30'') |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATEDIFF(''2010-11-30 23:59:59'',''2010-12-31'');'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| DATEDIFF(''2010-11-30 23:59:59'',''2010-12-31'') |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| -31 |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT NOW();'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| NOW() |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2011-05-23 10:56:05 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, DATEDIFF(NOW(),d) FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+-------------------+'+sLineBreak
        +'| d | DATEDIFF(NOW(),d) |'+sLineBreak
        +'+---------------------+-------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 1574 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 10082 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 32 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | -160 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 113 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2419 |'+sLineBreak
        +'+---------------------+-------------------+'
    ),

    (
      Name:         'DATE_ADD';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Performs date arithmetic. The date argument specifies the'+sLineBreak
        +'starting date or datetime value. expr is an expression'+sLineBreak
        +'specifying the'+sLineBreak
        +'interval value to be added or subtracted from the starting'+sLineBreak
        +'date. expr is a'+sLineBreak
        +'string; it may start with a "-" for negative intervals.'+sLineBreak
        +'unit is a'+sLineBreak
        +'keyword indicating the units in which the expression should'+sLineBreak
        +'be interpreted. See Date and Time Units for a complete list'+sLineBreak
        +'of permitted units. '+sLineBreak
        +' '+sLineBreak
        +'See also DATE_SUB().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ''2008-12-31 23:59:59'' + INTERVAL 1 SECOND;'+sLineBreak
        +' '+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| ''2008-12-31 23:59:59'' + INTERVAL 1 SECOND |'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| 2009-01-01 00:00:00 |'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT INTERVAL 1 DAY + ''2008-12-31'';'+sLineBreak
        +' '+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| INTERVAL 1 DAY + ''2008-12-31'' |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 2009-01-01 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ''2005-01-01'' - INTERVAL 1 SECOND;'+sLineBreak
        +' '+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| ''2005-01-01'' - INTERVAL 1 SECOND |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| 2004-12-31 23:59:59 |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_ADD(''2000-12-31 23:59:59'', INTERVAL 1 SECOND);'+sLineBreak
        +'+----------------------------------------------------+'+sLineBreak
        +'| DATE_ADD(''2000-12-31 23:59:59'', INTERVAL 1 SECOND) |'+sLineBreak
        +'+----------------------------------------------------+'+sLineBreak
        +'| 2001-01-01 00:00:00 |'+sLineBreak
        +'+----------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_ADD(''2010-12-31 23:59:59'', INTERVAL 1 DAY);'+sLineBreak
        +'+-------------------------------------------------+'+sLineBreak
        +'| DATE_ADD(''2010-12-31 23:59:59'', INTERVAL 1 DAY) |'+sLineBreak
        +'+-------------------------------------------------+'+sLineBreak
        +'| 2011-01-01 23:59:59 |'+sLineBreak
        +'+-------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_ADD(''2100-12-31 23:59:59'', INTERVAL ''1:1'''+sLineBreak
        +'MINUTE_SECOND);'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +'| DATE_ADD(''2100-12-31 23:59:59'', INTERVAL ''1:1'''+sLineBreak
        +'MINUTE_SECOND) |'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +'| 2101-01-01 00:01:00 |'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_ADD(''1900-01-01 00:00:00'', INTERVAL ''-1 10'''+sLineBreak
        +'DAY_HOUR);'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| DATE_ADD(''1900-01-01 00:00:00'', INTERVAL ''-1 10'''+sLineBreak
        +'DAY_HOUR) |'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| 1899-12-30 14:00:00 |'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_ADD(''1992-12-31 23:59:59.000002'', INTERVAL'+sLineBreak
        +'''1.999999'' SECOND_MICROSECOND);'+sLineBreak
        +'+--------------------------------------------------------------------------------+'+sLineBreak
        +'| DATE_ADD(''1992-12-31 23:59:59.000002'', INTERVAL'+sLineBreak
        +'''1.999999'' SECOND_MICROSECOND) |'+sLineBreak
        +'+--------------------------------------------------------------------------------+'+sLineBreak
        +'| 1993-01-01 00:00:01.000001 |'+sLineBreak
        +'+--------------------------------------------------------------------------------+'
    ),

    (
      Name:         'DATE_FORMAT';
      Declaration:  '(date, format[, locale])';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Formats the date value according to the format string. '+sLineBreak
        +' '+sLineBreak
        +'The language used for the names is controlled by the value'+sLineBreak
        +'of the lc_time_names system variable. See server locale for'+sLineBreak
        +'more on the supported locales.'+sLineBreak
        +' '+sLineBreak
        +'The options that can be used by DATE_FORMAT(), as well as'+sLineBreak
        +'its inverse STR_TO_DATE() and the FROM_UNIXTIME() function,'+sLineBreak
        +'are:'+sLineBreak
        +' '+sLineBreak
        +'Option | Description | '+sLineBreak
        +' '+sLineBreak
        +'%a | Short weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%b | Short form month name in current locale. For locale'+sLineBreak
        +'en_US this is one of:'+sLineBreak
        +'Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov or Dec. | '+sLineBreak
        +' '+sLineBreak
        +'%c | Month with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%D | Day with English suffix ''th'', ''nd'', ''st'' or'+sLineBreak
        +'''rd''''. (1st, 2nd, 3rd...). | '+sLineBreak
        +' '+sLineBreak
        +'%d | Day with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%e | Day with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%f | Sub seconds 6 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%H | Hour with 2 digits between 00-23. | '+sLineBreak
        +' '+sLineBreak
        +'%h | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%I | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%i | Minute with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%j | Day of the year (001-366) | '+sLineBreak
        +' '+sLineBreak
        +'%k | Hour with 1 digits between 0-23. | '+sLineBreak
        +' '+sLineBreak
        +'%l | Hour with 1 digits between 1-12. | '+sLineBreak
        +' '+sLineBreak
        +'%M | Full month name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%m | Month with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%p | AM/PM according to current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%r | Time in 12 hour format, followed by AM/PM. Short for'+sLineBreak
        +'''%I:%i:%S %p''. | '+sLineBreak
        +' '+sLineBreak
        +'%S | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%s | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%T | Time in 24 hour format. Short for ''%H:%i:%S''. | '+sLineBreak
        +' '+sLineBreak
        +'%U | Week number (00-53), when first day of the week is'+sLineBreak
        +'Sunday. | '+sLineBreak
        +' '+sLineBreak
        +'%u | Week number (00-53), when first day of the week is'+sLineBreak
        +'Monday. | '+sLineBreak
        +' '+sLineBreak
        +'%V | Week number (01-53), when first day of the week is'+sLineBreak
        +'Sunday. Used with %X. | '+sLineBreak
        +' '+sLineBreak
        +'%v | Week number (01-53), when first day of the week is'+sLineBreak
        +'Monday. Used with %x. | '+sLineBreak
        +' '+sLineBreak
        +'%W | Full weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%w | Day of the week. 0 = Sunday, 6 = Saturday. | '+sLineBreak
        +' '+sLineBreak
        +'%X | Year with 4 digits when first day of the week is'+sLineBreak
        +'Sunday. Used with %V. | '+sLineBreak
        +' '+sLineBreak
        +'%x | Year with 4 digits when first day of the week is'+sLineBreak
        +'Monday. Used with %v. | '+sLineBreak
        +' '+sLineBreak
        +'%Y | Year with 4 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%y | Year with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%# | For str_to_date(), skip all numbers. | '+sLineBreak
        +' '+sLineBreak
        +'%. | For str_to_date(), skip all punctation characters. | '+sLineBreak
        +' '+sLineBreak
        +'%@ | For str_to_date(), skip all alpha characters. | '+sLineBreak
        +' '+sLineBreak
        +'%% | A literal % character. | '+sLineBreak
        +' '+sLineBreak
        +'To get a date in one of the standard formats, GET_FORMAT()'+sLineBreak
        +'can be used.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATE_FORMAT(''2009-10-04 22:23:00'', ''%W %M %Y'');'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''2009-10-04 22:23:00'', ''%W %M %Y'') |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| Sunday October 2009 |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_FORMAT(''2007-10-04 22:23:00'', ''%H:%i:%s'');'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''2007-10-04 22:23:00'', ''%H:%i:%s'') |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| 22:23:00 |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_FORMAT(''1900-10-04 22:23:00'', ''%D %y %a %d %m'+sLineBreak
        +'%b %j'');'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''1900-10-04 22:23:00'', ''%D %y %a %d %m %b'+sLineBreak
        +'%j'') |'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| 4th 00 Thu 04 10 Oct 277 |'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_FORMAT(''1997-10-04 22:23:00'', ''%H %k %I %r %T'+sLineBreak
        +'%S %w'');'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''1997-10-04 22:23:00'', ''%H %k %I %r %T %S'+sLineBreak
        +'%w'') |'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| 22 22 10 10:23:00 PM 22:23:00 00 6 |'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_FORMAT(''1999-01-01'', ''%X %V'');'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''1999-01-01'', ''%X %V'') |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| 1998 52 |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_FORMAT(''2006-06-00'', ''%d'');'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''2006-06-00'', ''%d'') |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| 00 |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Optionally, the locale can be explicitly specified as the'+sLineBreak
        +'third DATE_FORMAT() argument. Doing so makes the function'+sLineBreak
        +'independent from the session settings, and the three'+sLineBreak
        +'argument version of DATE_FORMAT() can be used in virtual'+sLineBreak
        +'indexed and persistent generated-columns:'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_FORMAT(''2006-01-01'', ''%W'', ''el_GR'');'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''2006-01-01'', ''%W'', ''el_GR'') |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| ??????? |'+sLineBreak
        +'+------------------------------------------+'
    ),

    (
      Name:         'DATE_SUB';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Performs date arithmetic. The date argument specifies the'+sLineBreak
        +'starting date or datetime value. expr is an expression'+sLineBreak
        +'specifying the'+sLineBreak
        +'interval value to be added or subtracted from the starting'+sLineBreak
        +'date. expr is a'+sLineBreak
        +'string; it may start with a "-" for negative intervals.'+sLineBreak
        +'unit is a'+sLineBreak
        +'keyword indicating the units in which the expression should'+sLineBreak
        +'be interpreted. See Date and Time Units for a complete list'+sLineBreak
        +'of permitted units. '+sLineBreak
        +' '+sLineBreak
        +'See also DATE_ADD().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATE_SUB(''1998-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| DATE_SUB(''1998-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| 1997-12-02 |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_SUB(''2005-01-01 00:00:00'', INTERVAL ''1'+sLineBreak
        +'1:1:1'' DAY_SECOND);'+sLineBreak
        +'+----------------------------------------------------------------+'+sLineBreak
        +'| DATE_SUB(''2005-01-01 00:00:00'', INTERVAL ''1 1:1:1'''+sLineBreak
        +'DAY_SECOND) |'+sLineBreak
        +'+----------------------------------------------------------------+'+sLineBreak
        +'| 2004-12-30 22:58:59 |'+sLineBreak
        +'+----------------------------------------------------------------+'
    ),

    (
      Name:         'DAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'DAY() is a synonym for DAYOFMONTH().'
    ),

    (
      Name:         'DAYNAME';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the name of the weekday for date. The language used'+sLineBreak
        +'for the name is controlled by the value'+sLineBreak
        +'of the lc_time_names system variable. See server locale for'+sLineBreak
        +'more on the supported locales.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DAYNAME(''2007-02-03'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| DAYNAME(''2007-02-03'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| Saturday |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, DAYNAME(d) FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+------------+'+sLineBreak
        +'| d | DAYNAME(d) |'+sLineBreak
        +'+---------------------+------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | Tuesday |'+sLineBreak
        +'| 1983-10-15 06:42:51 | Saturday |'+sLineBreak
        +'| 2011-04-21 12:34:56 | Thursday |'+sLineBreak
        +'| 2011-10-30 06:31:41 | Sunday |'+sLineBreak
        +'| 2011-01-30 14:03:25 | Sunday |'+sLineBreak
        +'| 2004-10-07 11:19:34 | Thursday |'+sLineBreak
        +'+---------------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'Changing the locale:'+sLineBreak
        +' '+sLineBreak
        +'SET lc_time_names = ''fr_CA'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT DAYNAME(''2013-04-01'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| DAYNAME(''2013-04-01'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| lundi |'+sLineBreak
        +'+-----------------------+'
    ),

    (
      Name:         'DAYOFMONTH';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the day of the month for date, in the range 1 to 31,'+sLineBreak
        +'or 0'+sLineBreak
        +'for dates such as ''0000-00-00'' or ''2008-00-00'' which'+sLineBreak
        +'have a zero day'+sLineBreak
        +'part.'+sLineBreak
        +' '+sLineBreak
        +'DAY() is a synonym.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DAYOFMONTH(''2007-02-03'');'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| DAYOFMONTH(''2007-02-03'') |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d FROM t1 where DAYOFMONTH(d) = 30;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 |'+sLineBreak
        +'| 2011-10-30 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'DAYOFWEEK';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the day of the week index for the date (1 = Sunday,'+sLineBreak
        +'2 = Monday, ..., 7 ='+sLineBreak
        +'Saturday). These index values correspond to the ODBC'+sLineBreak
        +'standard.'+sLineBreak
        +' '+sLineBreak
        +'This contrasts with WEEKDAY() which follows a different'+sLineBreak
        +'index numbering'+sLineBreak
        +'(0 = Monday, 1 = Tuesday, ... 6 = Sunday).'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DAYOFWEEK(''2007-02-03'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| DAYOFWEEK(''2007-02-03'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 7 |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, DAYNAME(d), DAYOFWEEK(d), WEEKDAY(d) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+------------+--------------+------------+'+sLineBreak
        +'| d | DAYNAME(d) | DAYOFWEEK(d) | WEEKDAY(d) |'+sLineBreak
        +'+---------------------+------------+--------------+------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | Tuesday | 3 | 1 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | Saturday | 7 | 5 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | Thursday | 5 | 3 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | Sunday | 1 | 6 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | Sunday | 1 | 6 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | Thursday | 5 | 3 |'+sLineBreak
        +'+---------------------+------------+--------------+------------+'
    ),

    (
      Name:         'DAYOFYEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the day of the year for date, in the range 1 to 366.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DAYOFYEAR(''2018-02-16'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| DAYOFYEAR(''2018-02-16'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 47 |'+sLineBreak
        +'+-------------------------+'
    ),

    (
      Name:         'EXTRACT';
      Declaration:  '(unit FROM date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The EXTRACT() function extracts the required unit from the'+sLineBreak
        +'date. See Date and Time Units for a complete list of'+sLineBreak
        +'permitted units.'+sLineBreak
        +' '+sLineBreak
        +'In MariaDB 10.0.7 and MariaDB 5.5.35, EXTRACT (HOUR FROM'+sLineBreak
        +'...) was changed to return a value from 0 to 23, adhering to'+sLineBreak
        +'the SQL standard. Until MariaDB 10.0.6 and MariaDB 5.5.34,'+sLineBreak
        +'and in all versions of MySQL at least as of MySQL 5.7, it'+sLineBreak
        +'could return a value > 23. HOUR() is not a standard'+sLineBreak
        +'function, so continues to adhere to the old behaviour'+sLineBreak
        +'inherited from MySQL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT EXTRACT(YEAR FROM ''2009-07-02'');'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| EXTRACT(YEAR FROM ''2009-07-02'') |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| 2009 |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXTRACT(YEAR_MONTH FROM ''2009-07-02 01:02:03'');'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| EXTRACT(YEAR_MONTH FROM ''2009-07-02 01:02:03'') |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| 200907 |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXTRACT(DAY_MINUTE FROM ''2009-07-02 01:02:03'');'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| EXTRACT(DAY_MINUTE FROM ''2009-07-02 01:02:03'') |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| 20102 |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXTRACT(MICROSECOND FROM ''2003-01-02'+sLineBreak
        +'10:30:00.000123'');'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +'| EXTRACT(MICROSECOND FROM ''2003-01-02 10:30:00.000123'') |'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +'| 123 |'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.0.7 and MariaDB 5.5.35, EXTRACT (HOUR'+sLineBreak
        +'FROM...) returns a value from 0 to 23, as per the SQL'+sLineBreak
        +'standard. HOUR is not a standard function, so continues to'+sLineBreak
        +'adhere to the old behaviour inherited from MySQL.'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXTRACT(HOUR FROM ''26:30:00''), HOUR(''26:30:00'');'+sLineBreak
        +'+-------------------------------+------------------+'+sLineBreak
        +'| EXTRACT(HOUR FROM ''26:30:00'') | HOUR(''26:30:00'') |'+sLineBreak
        +'+-------------------------------+------------------+'+sLineBreak
        +'| 2 | 26 |'+sLineBreak
        +'+-------------------------------+------------------+'
    ),

    (
      Name:         'FROM_DAYS';
      Declaration:  '(N)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a day number N, returns a DATE value. The day count is'+sLineBreak
        +'based on the number of days from the start of the standard'+sLineBreak
        +'calendar (0000-00-00). '+sLineBreak
        +' '+sLineBreak
        +'The function is not designed for use with dates before the'+sLineBreak
        +'advent of the Gregorian calendar in October 1582. Results'+sLineBreak
        +'will not be reliable since it doesn''t account for the lost'+sLineBreak
        +'days when the calendar changed from the Julian calendar.'+sLineBreak
        +' '+sLineBreak
        +'This is the converse of the TO_DAYS() function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT FROM_DAYS(730669);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| FROM_DAYS(730669) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 2000-07-03 |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'FROM_UNIXTIME';
      Declaration:  '(unix_timestamp)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a representation of the unix_timestamp argument as a'+sLineBreak
        +'value in'+sLineBreak
        +'''YYYY-MM-DD HH:MM:SS'' or YYYYMMDDHHMMSS.uuuuuu format,'+sLineBreak
        +'depending on'+sLineBreak
        +'whether the function is used in a string or numeric context.'+sLineBreak
        +'The value'+sLineBreak
        +'is expressed in the current time zone. unix_timestamp is an'+sLineBreak
        +'internal'+sLineBreak
        +'timestamp value such as is produced by the UNIX_TIMESTAMP()'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'If format is given, the result is formatted according to the'+sLineBreak
        +'format'+sLineBreak
        +'string, which is used the same way as listed in the entry'+sLineBreak
        +'for the'+sLineBreak
        +'DATE_FORMAT() function.'+sLineBreak
        +' '+sLineBreak
        +'Timestamps in MariaDB have a maximum value of 2147483647,'+sLineBreak
        +'equivalent to 2038-01-19 05:14:07. This is due to the'+sLineBreak
        +'underlying 32-bit limitation. Using the function on a'+sLineBreak
        +'timestamp beyond this will result in NULL being returned.'+sLineBreak
        +'Use DATETIME as a storage type if you require dates beyond'+sLineBreak
        +'this.'+sLineBreak
        +' '+sLineBreak
        +'The options that can be used by FROM_UNIXTIME(), as well as'+sLineBreak
        +'DATE_FORMAT() and STR_TO_DATE(), are:'+sLineBreak
        +' '+sLineBreak
        +'Option | Description | '+sLineBreak
        +' '+sLineBreak
        +'%a | Short weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%b | Short form month name in current locale. For locale'+sLineBreak
        +'en_US this is one of:'+sLineBreak
        +'Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov or Dec. | '+sLineBreak
        +' '+sLineBreak
        +'%c | Month with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%D | Day with English suffix ''th'', ''nd'', ''st'' or'+sLineBreak
        +'''rd''''. (1st, 2nd, 3rd...). | '+sLineBreak
        +' '+sLineBreak
        +'%d | Day with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%e | Day with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%f | Sub seconds 6 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%H | Hour with 2 digits between 00-23. | '+sLineBreak
        +' '+sLineBreak
        +'%h | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%I | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%i | Minute with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%j | Day of the year (001-366) | '+sLineBreak
        +' '+sLineBreak
        +'%k | Hour with 1 digits between 0-23. | '+sLineBreak
        +' '+sLineBreak
        +'%l | Hour with 1 digits between 1-12. | '+sLineBreak
        +' '+sLineBreak
        +'%M | Full month name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%m | Month with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%p | AM/PM according to current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%r | Time in 12 hour format, followed by AM/PM. Short for'+sLineBreak
        +'''%I:%i:%S %p''. | '+sLineBreak
        +' '+sLineBreak
        +'%S | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%s | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%T | Time in 24 hour format. Short for ''%H:%i:%S''. | '+sLineBreak
        +' '+sLineBreak
        +'%U | Week number (00-53), when first day of the week is'+sLineBreak
        +'Sunday. | '+sLineBreak
        +' '+sLineBreak
        +'%u | Week number (00-53), when first day of the week is'+sLineBreak
        +'Monday. | '+sLineBreak
        +' '+sLineBreak
        +'%V | Week number (01-53), when first day of the week is'+sLineBreak
        +'Sunday. Used with %X. | '+sLineBreak
        +' '+sLineBreak
        +'%v | Week number (01-53), when first day of the week is'+sLineBreak
        +'Monday. Used with %x. | '+sLineBreak
        +' '+sLineBreak
        +'%W | Full weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%w | Day of the week. 0 = Sunday, 1 = Saturday. | '+sLineBreak
        +' '+sLineBreak
        +'%X | Year with 4 digits when first day of the week is'+sLineBreak
        +'Sunday. Used with %V. | '+sLineBreak
        +' '+sLineBreak
        +'%x | Year with 4 digits when first day of the week is'+sLineBreak
        +'Sunday. Used with %v. | '+sLineBreak
        +' '+sLineBreak
        +'%Y | Year with 4 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%y | Year with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%# | For str_to_date(), skip all numbers. | '+sLineBreak
        +' '+sLineBreak
        +'%. | For str_to_date(), skip all punctation characters. | '+sLineBreak
        +' '+sLineBreak
        +'%@ | For str_to_date(), skip all alpha characters. | '+sLineBreak
        +' '+sLineBreak
        +'%% | A literal % character. | '+sLineBreak
        +' '+sLineBreak
        +'Performance Considerations'+sLineBreak
        +' '+sLineBreak
        +'If your session time zone is set to SYSTEM (the default),'+sLineBreak
        +'FROM_UNIXTIME() will call the OS function to convert the'+sLineBreak
        +'data using the system time zone. At least on Linux, the'+sLineBreak
        +'corresponding function (localtime_r) uses a global mutex'+sLineBreak
        +'inside glibc that can cause contention under high concurrent'+sLineBreak
        +'load.'+sLineBreak
        +' '+sLineBreak
        +'Set your time zone to a named time zone to avoid this issue.'+sLineBreak
        +'See mysql time zone tables for details on how to do this.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT FROM_UNIXTIME(1196440219);'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(1196440219) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 2007-11-30 11:30:19 |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(1196440219) + 0;'+sLineBreak
        +' '+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(1196440219) + 0 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 20071130113019.000000 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(UNIX_TIMESTAMP(), ''%Y %D %M %h:%i:%s'+sLineBreak
        +'%x'');'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(UNIX_TIMESTAMP(), ''%Y %D %M %h:%i:%s %x'')'+sLineBreak
        +'|'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| 2010 27th March 01:03:47 2010 |'+sLineBreak
        +'+---------------------------------------------------------+'
    ),

    (
      Name:         'FROM_UNIXTIME';
      Declaration:  '(unix_timestamp,format)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a representation of the unix_timestamp argument as a'+sLineBreak
        +'value in'+sLineBreak
        +'''YYYY-MM-DD HH:MM:SS'' or YYYYMMDDHHMMSS.uuuuuu format,'+sLineBreak
        +'depending on'+sLineBreak
        +'whether the function is used in a string or numeric context.'+sLineBreak
        +'The value'+sLineBreak
        +'is expressed in the current time zone. unix_timestamp is an'+sLineBreak
        +'internal'+sLineBreak
        +'timestamp value such as is produced by the UNIX_TIMESTAMP()'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'If format is given, the result is formatted according to the'+sLineBreak
        +'format'+sLineBreak
        +'string, which is used the same way as listed in the entry'+sLineBreak
        +'for the'+sLineBreak
        +'DATE_FORMAT() function.'+sLineBreak
        +' '+sLineBreak
        +'Timestamps in MariaDB have a maximum value of 2147483647,'+sLineBreak
        +'equivalent to 2038-01-19 05:14:07. This is due to the'+sLineBreak
        +'underlying 32-bit limitation. Using the function on a'+sLineBreak
        +'timestamp beyond this will result in NULL being returned.'+sLineBreak
        +'Use DATETIME as a storage type if you require dates beyond'+sLineBreak
        +'this.'+sLineBreak
        +' '+sLineBreak
        +'The options that can be used by FROM_UNIXTIME(), as well as'+sLineBreak
        +'DATE_FORMAT() and STR_TO_DATE(), are:'+sLineBreak
        +' '+sLineBreak
        +'Option | Description | '+sLineBreak
        +' '+sLineBreak
        +'%a | Short weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%b | Short form month name in current locale. For locale'+sLineBreak
        +'en_US this is one of:'+sLineBreak
        +'Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov or Dec. | '+sLineBreak
        +' '+sLineBreak
        +'%c | Month with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%D | Day with English suffix ''th'', ''nd'', ''st'' or'+sLineBreak
        +'''rd''''. (1st, 2nd, 3rd...). | '+sLineBreak
        +' '+sLineBreak
        +'%d | Day with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%e | Day with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%f | Sub seconds 6 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%H | Hour with 2 digits between 00-23. | '+sLineBreak
        +' '+sLineBreak
        +'%h | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%I | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%i | Minute with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%j | Day of the year (001-366) | '+sLineBreak
        +' '+sLineBreak
        +'%k | Hour with 1 digits between 0-23. | '+sLineBreak
        +' '+sLineBreak
        +'%l | Hour with 1 digits between 1-12. | '+sLineBreak
        +' '+sLineBreak
        +'%M | Full month name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%m | Month with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%p | AM/PM according to current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%r | Time in 12 hour format, followed by AM/PM. Short for'+sLineBreak
        +'''%I:%i:%S %p''. | '+sLineBreak
        +' '+sLineBreak
        +'%S | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%s | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%T | Time in 24 hour format. Short for ''%H:%i:%S''. | '+sLineBreak
        +' '+sLineBreak
        +'%U | Week number (00-53), when first day of the week is'+sLineBreak
        +'Sunday. | '+sLineBreak
        +' '+sLineBreak
        +'%u | Week number (00-53), when first day of the week is'+sLineBreak
        +'Monday. | '+sLineBreak
        +' '+sLineBreak
        +'%V | Week number (01-53), when first day of the week is'+sLineBreak
        +'Sunday. Used with %X. | '+sLineBreak
        +' '+sLineBreak
        +'%v | Week number (01-53), when first day of the week is'+sLineBreak
        +'Monday. Used with %x. | '+sLineBreak
        +' '+sLineBreak
        +'%W | Full weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%w | Day of the week. 0 = Sunday, 1 = Saturday. | '+sLineBreak
        +' '+sLineBreak
        +'%X | Year with 4 digits when first day of the week is'+sLineBreak
        +'Sunday. Used with %V. | '+sLineBreak
        +' '+sLineBreak
        +'%x | Year with 4 digits when first day of the week is'+sLineBreak
        +'Sunday. Used with %v. | '+sLineBreak
        +' '+sLineBreak
        +'%Y | Year with 4 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%y | Year with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%# | For str_to_date(), skip all numbers. | '+sLineBreak
        +' '+sLineBreak
        +'%. | For str_to_date(), skip all punctation characters. | '+sLineBreak
        +' '+sLineBreak
        +'%@ | For str_to_date(), skip all alpha characters. | '+sLineBreak
        +' '+sLineBreak
        +'%% | A literal % character. | '+sLineBreak
        +' '+sLineBreak
        +'Performance Considerations'+sLineBreak
        +' '+sLineBreak
        +'If your session time zone is set to SYSTEM (the default),'+sLineBreak
        +'FROM_UNIXTIME() will call the OS function to convert the'+sLineBreak
        +'data using the system time zone. At least on Linux, the'+sLineBreak
        +'corresponding function (localtime_r) uses a global mutex'+sLineBreak
        +'inside glibc that can cause contention under high concurrent'+sLineBreak
        +'load.'+sLineBreak
        +' '+sLineBreak
        +'Set your time zone to a named time zone to avoid this issue.'+sLineBreak
        +'See mysql time zone tables for details on how to do this.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT FROM_UNIXTIME(1196440219);'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(1196440219) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 2007-11-30 11:30:19 |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(1196440219) + 0;'+sLineBreak
        +' '+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(1196440219) + 0 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 20071130113019.000000 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(UNIX_TIMESTAMP(), ''%Y %D %M %h:%i:%s'+sLineBreak
        +'%x'');'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(UNIX_TIMESTAMP(), ''%Y %D %M %h:%i:%s %x'')'+sLineBreak
        +'|'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| 2010 27th March 01:03:47 2010 |'+sLineBreak
        +'+---------------------------------------------------------+'
    ),

    (
      Name:         'GET_FORMAT';
      Declaration:  '({DATE|DATETIME|TIME}, {''EUR''|''USA''|''JIS''|''ISO''|''INTERNAL''})';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a format string. This function is useful in'+sLineBreak
        +'combination with'+sLineBreak
        +'the DATE_FORMAT() and the STR_TO_DATE() functions.'+sLineBreak
        +' '+sLineBreak
        +'Possible result formats are:'+sLineBreak
        +' '+sLineBreak
        +'Function Call | Result Format | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATE,''EUR'') | ''%d.%m.%Y'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATE,''USA'') | ''%m.%d.%Y'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATE,''JIS'') | ''%Y-%m-%d'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATE,''ISO'') | ''%Y-%m-%d'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATE,''INTERNAL'') | ''%Y%m%d'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATETIME,''EUR'') | ''%Y-%m-%d %H.%i.%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATETIME,''USA'') | ''%Y-%m-%d %H.%i.%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATETIME,''JIS'') | ''%Y-%m-%d %H:%i:%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATETIME,''ISO'') | ''%Y-%m-%d %H:%i:%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(DATETIME,''INTERNAL'') | ''%Y%m%d%H%i%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(TIME,''EUR'') | ''%H.%i.%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(TIME,''USA'') | ''%h:%i:%s %p'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(TIME,''JIS'') | ''%H:%i:%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(TIME,''ISO'') | ''%H:%i:%s'' | '+sLineBreak
        +' '+sLineBreak
        +'GET_FORMAT(TIME,''INTERNAL'') | ''%H%i%s'' | '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Obtaining the string matching to the standard European date'+sLineBreak
        +'format:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_FORMAT(DATE, ''EUR'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| GET_FORMAT(DATE, ''EUR'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| %d.%m.%Y |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Using the same string to format a date:'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATE_FORMAT(''2003-10-03'',GET_FORMAT(DATE,''EUR''));'+sLineBreak
        +'+--------------------------------------------------+'+sLineBreak
        +'| DATE_FORMAT(''2003-10-03'',GET_FORMAT(DATE,''EUR'')) |'+sLineBreak
        +'+--------------------------------------------------+'+sLineBreak
        +'| 03.10.2003 |'+sLineBreak
        +'+--------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT STR_TO_DATE(''10.31.2003'',GET_FORMAT(DATE,''USA''));'+sLineBreak
        +'+--------------------------------------------------+'+sLineBreak
        +'| STR_TO_DATE(''10.31.2003'',GET_FORMAT(DATE,''USA'')) |'+sLineBreak
        +'+--------------------------------------------------+'+sLineBreak
        +'| 2003-10-31 |'+sLineBreak
        +'+--------------------------------------------------+'
    ),

    (
      Name:         'HOUR';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the hour for time. The range of the return value is'+sLineBreak
        +'0 to 23'+sLineBreak
        +'for time-of-day values. However, the range of TIME values'+sLineBreak
        +'actually is'+sLineBreak
        +'much larger, so HOUR can return values greater than 23.'+sLineBreak
        +' '+sLineBreak
        +'The return value is always positive, even if a negative TIME'+sLineBreak
        +'value is provided.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT HOUR(''10:05:03'');'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| HOUR(''10:05:03'') |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 10 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HOUR(''272:59:59'');'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| HOUR(''272:59:59'') |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 272 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'Difference between EXTRACT (HOUR FROM ...) (>= MariaDB'+sLineBreak
        +'10.0.7 and MariaDB 5.5.35) and HOUR:'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXTRACT(HOUR FROM ''26:30:00''), HOUR(''26:30:00'');'+sLineBreak
        +'+-------------------------------+------------------+'+sLineBreak
        +'| EXTRACT(HOUR FROM ''26:30:00'') | HOUR(''26:30:00'') |'+sLineBreak
        +'+-------------------------------+------------------+'+sLineBreak
        +'| 2 | 26 |'+sLineBreak
        +'+-------------------------------+------------------+'
    ),

    (
      Name:         'LAST_DAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Takes a date or datetime value and returns the corresponding'+sLineBreak
        +'value for'+sLineBreak
        +'the last day of the month. Returns NULL if the argument is'+sLineBreak
        +'invalid.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LAST_DAY(''2003-02-05'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| LAST_DAY(''2003-02-05'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 2003-02-28 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_DAY(''2004-02-05'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| LAST_DAY(''2004-02-05'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 2004-02-29 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_DAY(''2004-01-01 01:01:01'');'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| LAST_DAY(''2004-01-01 01:01:01'') |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| 2004-01-31 |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_DAY(''2003-03-32'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| LAST_DAY(''2003-03-32'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'1 row in set, 1 warning (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'Warning (Code 1292): Incorrect datetime value:'+sLineBreak
        +'''2003-03-32'''
    ),

    (
      Name:         'MAKEDATE';
      Declaration:  '(year,dayofyear)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a date, given year and day-of-year values. dayofyear'+sLineBreak
        +'must be'+sLineBreak
        +'greater than 0 or the result is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MAKEDATE(2011,31), MAKEDATE(2011,32);'+sLineBreak
        +'+-------------------+-------------------+'+sLineBreak
        +'| MAKEDATE(2011,31) | MAKEDATE(2011,32) |'+sLineBreak
        +'+-------------------+-------------------+'+sLineBreak
        +'| 2011-01-31 | 2011-02-01 |'+sLineBreak
        +'+-------------------+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAKEDATE(2011,365), MAKEDATE(2014,365);'+sLineBreak
        +'+--------------------+--------------------+'+sLineBreak
        +'| MAKEDATE(2011,365) | MAKEDATE(2014,365) |'+sLineBreak
        +'+--------------------+--------------------+'+sLineBreak
        +'| 2011-12-31 | 2014-12-31 |'+sLineBreak
        +'+--------------------+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAKEDATE(2011,0);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| MAKEDATE(2011,0) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'MAKETIME';
      Declaration:  '(hour,minute,second)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a time value calculated from the hour, minute, and'+sLineBreak
        +'second arguments.'+sLineBreak
        +' '+sLineBreak
        +'If minute or second are out of the range 0 to 60, NULL is'+sLineBreak
        +'returned. The hour can be in the range -838 to 838, outside'+sLineBreak
        +'of which the value is truncated with a warning.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MAKETIME(13,57,33);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| MAKETIME(13,57,33) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 13:57:33 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAKETIME(-13,57,33);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| MAKETIME(-13,57,33) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| -13:57:33 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAKETIME(13,67,33);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| MAKETIME(13,67,33) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAKETIME(-1000,57,33);'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| MAKETIME(-1000,57,33) |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| -838:59:59 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'1 row in set, 1 warning (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'SHOW WARNINGS;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------+-----------------------------------------------+'+sLineBreak
        +'| Level | Code | Message |'+sLineBreak
        +'+---------+------+-----------------------------------------------+'+sLineBreak
        +'| Warning | 1292 | Truncated incorrect time value:'+sLineBreak
        +'''-1000:57:33'' |'+sLineBreak
        +'+---------+------+-----------------------------------------------+'
    ),

    (
      Name:         'MICROSECOND';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the microseconds from the time or datetime'+sLineBreak
        +'expression expr as a number in the range from 0 to 999999.'+sLineBreak
        +' '+sLineBreak
        +'If expr is a time with no microseconds, zero is returned,'+sLineBreak
        +'while if expr is a date with no time, zero with a warning is'+sLineBreak
        +'returned.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MICROSECOND(''12:00:00.123456'');'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| MICROSECOND(''12:00:00.123456'') |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| 123456 |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MICROSECOND(''2009-12-31 23:59:59.000010'');'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| MICROSECOND(''2009-12-31 23:59:59.000010'') |'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| 10 |'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MICROSECOND(''2013-08-07 12:13:14'');'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| MICROSECOND(''2013-08-07 12:13:14'') |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MICROSECOND(''2013-08-07'');'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| MICROSECOND(''2013-08-07'') |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'1 row in set, 1 warning (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'SHOW WARNINGS;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------+----------------------------------------------+'+sLineBreak
        +'| Level | Code | Message |'+sLineBreak
        +'+---------+------+----------------------------------------------+'+sLineBreak
        +'| Warning | 1292 | Truncated incorrect time value:'+sLineBreak
        +'''2013-08-07'' |'+sLineBreak
        +'+---------+------+----------------------------------------------+'
    ),

    (
      Name:         'MINUTE';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the minute for time, in the range 0 to 59. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MINUTE(''2013-08-03 11:04:03'');'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| MINUTE(''2013-08-03 11:04:03'') |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +' '+sLineBreak
        +' SELECT MINUTE (''23:12:50'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| MINUTE (''23:12:50'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 12 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'MONTH';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the month for date in the range 1 to 12 for January'+sLineBreak
        +'to'+sLineBreak
        +'December, or 0 for dates such as ''0000-00-00'' or'+sLineBreak
        +'''2008-00-00'' that'+sLineBreak
        +'have a zero month part.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MONTH(''2019-01-03'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| MONTH(''2019-01-03'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MONTH(''2019-00-03'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| MONTH(''2019-00-03'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'MONTHNAME';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the full name of the month for date. The language'+sLineBreak
        +'used for the name is controlled by the value of the'+sLineBreak
        +'lc_time_names system variable. See server locale for more on'+sLineBreak
        +'the supported locales.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MONTHNAME(''2019-02-03'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| MONTHNAME(''2019-02-03'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| February |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Changing the locale:'+sLineBreak
        +' '+sLineBreak
        +'SET lc_time_names = ''fr_CA'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT MONTHNAME(''2019-05-21'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| MONTHNAME(''2019-05-21'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| mai |'+sLineBreak
        +'+-------------------------+'
    ),

    (
      Name:         'NOW';
      Declaration:  '([precision])';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current date and time as a value in ''YYYY-MM-DD'+sLineBreak
        +'HH:MM:SS'''+sLineBreak
        +'or YYYYMMDDHHMMSS.uuuuuu format, depending on whether the'+sLineBreak
        +'function is'+sLineBreak
        +'used in a string or numeric context. The value is expressed'+sLineBreak
        +'in the'+sLineBreak
        +'current time zone.'+sLineBreak
        +' '+sLineBreak
        +'The optional precision determines the microsecond precision.'+sLineBreak
        +'See Microseconds in MariaDB.'+sLineBreak
        +' '+sLineBreak
        +'NOW() (or its synonyms) can be used as the default value for'+sLineBreak
        +'TIMESTAMP columns as well as, since MariaDB 10.0.1, DATETIME'+sLineBreak
        +'columns. Before MariaDB 10.0.1, it was only possible for a'+sLineBreak
        +'single TIMESTAMP column per table to contain the'+sLineBreak
        +'CURRENT_TIMESTAMP as its default.'+sLineBreak
        +' '+sLineBreak
        +'When displayed in the INFORMATION_SCHEMA.COLUMNS table, a'+sLineBreak
        +'default CURRENT TIMESTAMP is displayed as CURRENT_TIMESTAMP'+sLineBreak
        +'up until MariaDB 10.2.2, and as current_timestamp() from'+sLineBreak
        +'MariaDB 10.2.3, due to to MariaDB 10.2 accepting expressions'+sLineBreak
        +'in the DEFAULT clause.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT NOW();'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| NOW() |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2010-03-27 13:13:25 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT NOW() + 0;'+sLineBreak
        +' '+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| NOW() + 0 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 20100327131329.000000 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'With precision:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CURRENT_TIMESTAMP(2);'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| CURRENT_TIMESTAMP(2) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 2018-07-10 09:47:26.24 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Used as a default TIMESTAMP:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t (createdTS TIMESTAMP NOT NULL DEFAULT'+sLineBreak
        +'CURRENT_TIMESTAMP);'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2:'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE'+sLineBreak
        +'TABLE_SCHEMA=''test'''+sLineBreak
        +' AND COLUMN_NAME LIKE ''%ts%''\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +' TABLE_CATALOG: def'+sLineBreak
        +' TABLE_SCHEMA: test'+sLineBreak
        +' TABLE_NAME: t'+sLineBreak
        +' COLUMN_NAME: ts'+sLineBreak
        +' ORDINAL_POSITION: 1'+sLineBreak
        +' COLUMN_DEFAULT: current_timestamp()'+sLineBreak
        +'...'
    ),

    (
      Name:         'PERIOD_ADD';
      Declaration:  '(P,N)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Adds N months to period P. P is in the format YYMM or'+sLineBreak
        +'YYYYMM, and is not a date value. If P contains a two-digit'+sLineBreak
        +'year, values from 00 to 69 are converted to from 2000 to'+sLineBreak
        +'2069, while values from 70 are converted to 1970 upwards.'+sLineBreak
        +' '+sLineBreak
        +'Returns a value in the format YYYYMM.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT PERIOD_ADD(200801,2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| PERIOD_ADD(200801,2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 200803 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT PERIOD_ADD(6910,2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| PERIOD_ADD(6910,2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 206912 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT PERIOD_ADD(7010,2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| PERIOD_ADD(7010,2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 197012 |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'PERIOD_DIFF';
      Declaration:  '(P1,P2)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of months between periods P1 and P2. P1'+sLineBreak
        +'and P2 '+sLineBreak
        +'can be in the format YYMM or YYYYMM, and are not date'+sLineBreak
        +'values.'+sLineBreak
        +' '+sLineBreak
        +'If P1 or P2 contains a two-digit year, values from 00 to 69'+sLineBreak
        +'are converted to from 2000 to 2069, while values from 70 are'+sLineBreak
        +'converted to 1970 upwards.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT PERIOD_DIFF(200802,200703);'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| PERIOD_DIFF(200802,200703) |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| 11 |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT PERIOD_DIFF(6902,6803);'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| PERIOD_DIFF(6902,6803) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 11 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT PERIOD_DIFF(7002,6803);'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| PERIOD_DIFF(7002,6803) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| -1177 |'+sLineBreak
        +'+------------------------+'
    ),

    (
      Name:         'QUARTER';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the quarter of the year for date, in the range 1 to'+sLineBreak
        +'4. Returns 0 if month contains a zero value, or NULL if the'+sLineBreak
        +'given value is not otherwise a valid date (zero values are'+sLineBreak
        +'accepted).'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT QUARTER(''2008-04-01'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| QUARTER(''2008-04-01'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT QUARTER(''2019-00-01'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| QUARTER(''2019-00-01'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------------+'
    ),

    (
      Name:         'SECOND';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the second for a given time (which can include'+sLineBreak
        +'microseconds), in the range 0 to 59, or NULL if not given a'+sLineBreak
        +'valid time value.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SECOND(''10:05:03'');'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| SECOND(''10:05:03'') |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SECOND(''10:05:01.999999'');'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| SECOND(''10:05:01.999999'') |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------------------------+'
    ),

    (
      Name:         'SEC_TO_TIME';
      Declaration:  '(seconds)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the seconds argument, converted to hours, minutes,'+sLineBreak
        +'and'+sLineBreak
        +'seconds, as a TIME value. The range of the result is'+sLineBreak
        +'constrained to'+sLineBreak
        +'that of the TIME data type. A warning occurs if the argument'+sLineBreak
        +'corresponds to a value outside that range.'+sLineBreak
        +' '+sLineBreak
        +'The time will be returned in the format hh:mm:ss, or hhmmss'+sLineBreak
        +'if used in a numeric calculation.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SEC_TO_TIME(12414);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| SEC_TO_TIME(12414) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 03:26:54 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SEC_TO_TIME(12414)+0;'+sLineBreak
        +' '+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| SEC_TO_TIME(12414)+0 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 32654 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SEC_TO_TIME(9999999);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| SEC_TO_TIME(9999999) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 838:59:59 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'1 row in set, 1 warning (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'SHOW WARNINGS;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------+-------------------------------------------+'+sLineBreak
        +'| Level | Code | Message |'+sLineBreak
        +'+---------+------+-------------------------------------------+'+sLineBreak
        +'| Warning | 1292 | Truncated incorrect time value:'+sLineBreak
        +'''9999999'' |'+sLineBreak
        +'+---------+------+-------------------------------------------+'
    ),

    (
      Name:         'STR_TO_DATE';
      Declaration:  '(str,format)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This is the inverse of the DATE_FORMAT() function. It takes'+sLineBreak
        +'a string str and a format string format. STR_TO_DATE()'+sLineBreak
        +'returns a'+sLineBreak
        +'DATETIME value if the format string contains both date and'+sLineBreak
        +'time parts, or a'+sLineBreak
        +'DATE or TIME value if the string contains only date or time'+sLineBreak
        +'parts.'+sLineBreak
        +' '+sLineBreak
        +'The date, time, or datetime values contained in str should'+sLineBreak
        +'be given in the format indicated by format. If str contains'+sLineBreak
        +'an illegal date, time, or datetime value, STR_TO_DATE()'+sLineBreak
        +'returns NULL. An illegal value also produces a warning.'+sLineBreak
        +' '+sLineBreak
        +'The options that can be used by STR_TO_DATE(), as well as'+sLineBreak
        +'its inverse DATE_FORMAT() and the FROM_UNIXTIME() function,'+sLineBreak
        +'are:'+sLineBreak
        +' '+sLineBreak
        +'Option | Description | '+sLineBreak
        +' '+sLineBreak
        +'%a | Short weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%b | Short form month name in current locale. For locale'+sLineBreak
        +'en_US this is one of:'+sLineBreak
        +'Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov or Dec. | '+sLineBreak
        +' '+sLineBreak
        +'%c | Month with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%D | Day with English suffix ''th'', ''nd'', ''st'' or'+sLineBreak
        +'''rd''''. (1st, 2nd, 3rd...). | '+sLineBreak
        +' '+sLineBreak
        +'%d | Day with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%e | Day with 1 or 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%f | Sub seconds 6 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%H | Hour with 2 digits between 00-23. | '+sLineBreak
        +' '+sLineBreak
        +'%h | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%I | Hour with 2 digits between 01-12. | '+sLineBreak
        +' '+sLineBreak
        +'%i | Minute with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%j | Day of the year (001-366) | '+sLineBreak
        +' '+sLineBreak
        +'%k | Hour with 1 digits between 0-23. | '+sLineBreak
        +' '+sLineBreak
        +'%l | Hour with 1 digits between 1-12. | '+sLineBreak
        +' '+sLineBreak
        +'%M | Full month name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%m | Month with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%p | AM/PM according to current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%r | Time in 12 hour format, followed by AM/PM. Short for'+sLineBreak
        +'''%I:%i:%S %p''. | '+sLineBreak
        +' '+sLineBreak
        +'%S | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%s | Seconds with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%T | Time in 24 hour format. Short for ''%H:%i:%S''. | '+sLineBreak
        +' '+sLineBreak
        +'%U | Week number (00-53), when first day of the week is'+sLineBreak
        +'Sunday. | '+sLineBreak
        +' '+sLineBreak
        +'%u | Week number (00-53), when first day of the week is'+sLineBreak
        +'Monday. | '+sLineBreak
        +' '+sLineBreak
        +'%V | Week number (01-53), when first day of the week is'+sLineBreak
        +'Sunday. Used with %X. | '+sLineBreak
        +' '+sLineBreak
        +'%v | Week number (01-53), when first day of the week is'+sLineBreak
        +'Monday. Used with %x. | '+sLineBreak
        +' '+sLineBreak
        +'%W | Full weekday name in current locale (Variable'+sLineBreak
        +'lc_time_names). | '+sLineBreak
        +' '+sLineBreak
        +'%w | Day of the week. 0 = Sunday, 6 = Saturday. | '+sLineBreak
        +' '+sLineBreak
        +'%X | Year with 4 digits when first day of the week is'+sLineBreak
        +'Sunday. Used with %V. | '+sLineBreak
        +' '+sLineBreak
        +'%x | Year with 4 digits when first day of the week is'+sLineBreak
        +'Monday. Used with %v. | '+sLineBreak
        +' '+sLineBreak
        +'%Y | Year with 4 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%y | Year with 2 digits. | '+sLineBreak
        +' '+sLineBreak
        +'%# | For str_to_date(), skip all numbers. | '+sLineBreak
        +' '+sLineBreak
        +'%. | For str_to_date(), skip all punctation characters. | '+sLineBreak
        +' '+sLineBreak
        +'%@ | For str_to_date(), skip all alpha characters. | '+sLineBreak
        +' '+sLineBreak
        +'%% | A literal % character. | '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT STR_TO_DATE(''Wednesday, June 2, 2014'', ''%W, %M %e,'+sLineBreak
        +'%Y'');'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| STR_TO_DATE(''Wednesday, June 2, 2014'', ''%W, %M %e,'+sLineBreak
        +'%Y'') |'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +'| 2014-06-02 |'+sLineBreak
        +'+---------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT STR_TO_DATE(''Wednesday23423, June 2, 2014'', ''%W,'+sLineBreak
        +'%M %e, %Y'');'+sLineBreak
        +'+--------------------------------------------------------------+'+sLineBreak
        +'| STR_TO_DATE(''Wednesday23423, June 2, 2014'', ''%W, %M %e,'+sLineBreak
        +'%Y'') |'+sLineBreak
        +'+--------------------------------------------------------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+--------------------------------------------------------------+'+sLineBreak
        +'1 row in set, 1 warning (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'SHOW WARNINGS;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------+-----------------------------------------------------------------------------------+'+sLineBreak
        +'| Level | Code | Message |'+sLineBreak
        +'+---------+------+-----------------------------------------------------------------------------------+'+sLineBreak
        +'| Warning | 1411 | Incorrect datetime value:'+sLineBreak
        +'''Wednesday23423, June 2, 2014'' for function str_to_date |'+sLineBreak
        +'+---------+------+-----------------------------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT STR_TO_DATE(''Wednesday23423, June 2, 2014'', ''%W%#,'+sLineBreak
        +'%M %e, %Y'');'+sLineBreak
        +'+----------------------------------------------------------------+'+sLineBreak
        +'| STR_TO_DATE(''Wednesday23423, June 2, 2014'', ''%W%#, %M'+sLineBreak
        +'%e, %Y'') |'+sLineBreak
        +'+----------------------------------------------------------------+'+sLineBreak
        +'| 2014-06-02 |'+sLineBreak
        +'+----------------------------------------------------------------+'
    ),

    (
      Name:         'SUBDATE';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'When invoked with the INTERVAL form of the second argument,'+sLineBreak
        +'SUBDATE()'+sLineBreak
        +'is a synonym for DATE_SUB(). See Date and Time Units for a'+sLineBreak
        +'complete list of permitted units. '+sLineBreak
        +' '+sLineBreak
        +'The second form allows the use of an integer value for days.'+sLineBreak
        +'In such'+sLineBreak
        +'cases, it is interpreted as the number of days to be'+sLineBreak
        +'subtracted from'+sLineBreak
        +'the date or datetime expression expr.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATE_SUB(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| DATE_SUB(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| 2007-12-02 |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBDATE(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| SUBDATE(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| 2007-12-02 |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBDATE(''2008-01-02 12:00:00'', 31);'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| SUBDATE(''2008-01-02 12:00:00'', 31) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| 2007-12-02 12:00:00 |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, SUBDATE(d, 10) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| d | SUBDATE(d, 10) |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-01-20 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-05 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-04-11 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-10-20 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-01-20 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-09-27 11:19:34 |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, SUBDATE(d, INTERVAL 10 MINUTE) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+--------------------------------+'+sLineBreak
        +'| d | SUBDATE(d, INTERVAL 10 MINUTE) |'+sLineBreak
        +'+---------------------+--------------------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-01-30 21:21:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-15 06:32:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-04-21 12:24:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-10-30 06:21:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-01-30 13:53:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-10-07 11:09:34 |'+sLineBreak
        +'+---------------------+--------------------------------+'
    ),

    (
      Name:         'SUBDATE';
      Declaration:  '(expr,days)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'When invoked with the INTERVAL form of the second argument,'+sLineBreak
        +'SUBDATE()'+sLineBreak
        +'is a synonym for DATE_SUB(). See Date and Time Units for a'+sLineBreak
        +'complete list of permitted units. '+sLineBreak
        +' '+sLineBreak
        +'The second form allows the use of an integer value for days.'+sLineBreak
        +'In such'+sLineBreak
        +'cases, it is interpreted as the number of days to be'+sLineBreak
        +'subtracted from'+sLineBreak
        +'the date or datetime expression expr.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATE_SUB(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| DATE_SUB(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| 2007-12-02 |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBDATE(''2008-01-02'', INTERVAL 31 DAY);'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| SUBDATE(''2008-01-02'', INTERVAL 31 DAY) |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| 2007-12-02 |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBDATE(''2008-01-02 12:00:00'', 31);'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| SUBDATE(''2008-01-02 12:00:00'', 31) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| 2007-12-02 12:00:00 |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, SUBDATE(d, 10) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| d | SUBDATE(d, 10) |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-01-20 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-05 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-04-11 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-10-20 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-01-20 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-09-27 11:19:34 |'+sLineBreak
        +'+---------------------+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, SUBDATE(d, INTERVAL 10 MINUTE) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+--------------------------------+'+sLineBreak
        +'| d | SUBDATE(d, INTERVAL 10 MINUTE) |'+sLineBreak
        +'+---------------------+--------------------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 2007-01-30 21:21:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 1983-10-15 06:32:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 2011-04-21 12:24:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 2011-10-30 06:21:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 2011-01-30 13:53:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 2004-10-07 11:09:34 |'+sLineBreak
        +'+---------------------+--------------------------------+'
    ),

    (
      Name:         'SUBTIME';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'SUBTIME() returns expr1 - expr2 expressed as a value in the'+sLineBreak
        +'same'+sLineBreak
        +'format as expr1. expr1 is a time or datetime expression, and'+sLineBreak
        +'expr2 is'+sLineBreak
        +'a time expression.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SUBTIME(''2007-12-31 23:59:59.999999'',''1'+sLineBreak
        +'1:1:1.000002'');'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +'| SUBTIME(''2007-12-31 23:59:59.999999'',''1 1:1:1.000002'')'+sLineBreak
        +'|'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +'| 2007-12-30 22:58:58.999997 |'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBTIME(''01:00:00.999999'', ''02:00:00.999998'');'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| SUBTIME(''01:00:00.999999'', ''02:00:00.999998'') |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| -00:59:59.999999 |'+sLineBreak
        +'+-----------------------------------------------+'
    ),

    (
      Name:         'SYSDATE';
      Declaration:  '([precision])';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current date and time as a value in ''YYYY-MM-DD'+sLineBreak
        +'HH:MM:SS'''+sLineBreak
        +'or YYYYMMDDHHMMSS.uuuuuu format, depending on whether the'+sLineBreak
        +'function is'+sLineBreak
        +'used in a string or numeric context.'+sLineBreak
        +' '+sLineBreak
        +'The optional precision determines the microsecond precision.'+sLineBreak
        +'See Microseconds in MariaDB.'+sLineBreak
        +' '+sLineBreak
        +'SYSDATE() returns the time at which it executes. This'+sLineBreak
        +'differs from the'+sLineBreak
        +'behavior for NOW(), which returns a constant time that'+sLineBreak
        +'indicates the'+sLineBreak
        +'time at which the statement began to execute. (Within a'+sLineBreak
        +'stored routine'+sLineBreak
        +'or trigger, NOW() returns the time at which the routine or'+sLineBreak
        +'triggering'+sLineBreak
        +'statement began to execute.)'+sLineBreak
        +' '+sLineBreak
        +'In addition, changing the timestamp system variable with a'+sLineBreak
        +'SET timestamp statement affects the value returned by'+sLineBreak
        +'NOW() but not by SYSDATE(). This means that timestamp'+sLineBreak
        +'settings in the'+sLineBreak
        +'binary log have no effect on invocations of SYSDATE().'+sLineBreak
        +' '+sLineBreak
        +'Because SYSDATE() can return different values even within'+sLineBreak
        +'the same'+sLineBreak
        +'statement, and is not affected by SET TIMESTAMP, it is'+sLineBreak
        +'non-deterministic and therefore unsafe for replication if'+sLineBreak
        +'statement-based binary logging is used. If that is a'+sLineBreak
        +'problem, you can'+sLineBreak
        +'use row-based logging, or start the server with the mysqld'+sLineBreak
        +'option --sysdate-is-now to cause SYSDATE() to be an alias'+sLineBreak
        +'for NOW(). The non-deterministic nature of SYSDATE() also'+sLineBreak
        +'means that indexes cannot be used for evaluating expressions'+sLineBreak
        +'that refer to it, and that statements using the SYSDATE()'+sLineBreak
        +'function are unsafe for statement-based replication.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Difference between NOW() and SYSDATE():'+sLineBreak
        +' '+sLineBreak
        +'SELECT NOW(), SLEEP(2), NOW();'+sLineBreak
        +'+---------------------+----------+---------------------+'+sLineBreak
        +'| NOW() | SLEEP(2) | NOW() |'+sLineBreak
        +'+---------------------+----------+---------------------+'+sLineBreak
        +'| 2010-03-27 13:23:40 | 0 | 2010-03-27 13:23:40 |'+sLineBreak
        +'+---------------------+----------+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SYSDATE(), SLEEP(2), SYSDATE();'+sLineBreak
        +'+---------------------+----------+---------------------+'+sLineBreak
        +'| SYSDATE() | SLEEP(2) | SYSDATE() |'+sLineBreak
        +'+---------------------+----------+---------------------+'+sLineBreak
        +'| 2010-03-27 13:23:52 | 0 | 2010-03-27 13:23:54 |'+sLineBreak
        +'+---------------------+----------+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'With precision:'+sLineBreak
        +' '+sLineBreak
        +'SELECT SYSDATE(4);'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| SYSDATE(4) |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| 2018-07-10 10:17:13.1689 |'+sLineBreak
        +'+--------------------------+'
    ),

    (
      Name:         'TIMEDIFF';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'TIMEDIFF() returns expr1 - expr2 expressed as a time value.'+sLineBreak
        +'expr1 and'+sLineBreak
        +'expr2 are time or date-and-time expressions, but both must'+sLineBreak
        +'be of the'+sLineBreak
        +'same type.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TIMEDIFF(''2000:01:01 00:00:00'', ''2000:01:01'+sLineBreak
        +'00:00:00.000001'');'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +'| TIMEDIFF(''2000:01:01 00:00:00'', ''2000:01:01'+sLineBreak
        +'00:00:00.000001'') |'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +'| -00:00:00.000001 |'+sLineBreak
        +'+---------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TIMEDIFF(''2008-12-31 23:59:59.000001'', ''2008-12-30'+sLineBreak
        +'01:01:01.000002'');'+sLineBreak
        +'+----------------------------------------------------------------------+'+sLineBreak
        +'| TIMEDIFF(''2008-12-31 23:59:59.000001'', ''2008-12-30'+sLineBreak
        +'01:01:01.000002'') |'+sLineBreak
        +'+----------------------------------------------------------------------+'+sLineBreak
        +'| 46:58:57.999999 |'+sLineBreak
        +'+----------------------------------------------------------------------+'
    ),

    (
      Name:         'TIMESTAMPADD';
      Declaration:  '(unit,interval,datetime_expr)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Adds the integer expression interval to the date or datetime'+sLineBreak
        +'expression datetime_expr. The unit for interval is given by'+sLineBreak
        +'the unit'+sLineBreak
        +'argument, which should be one of the following values:'+sLineBreak
        +'MICROSECOND, SECOND, MINUTE, HOUR, DAY, WEEK, MONTH,'+sLineBreak
        +'QUARTER, or YEAR.'+sLineBreak
        +' '+sLineBreak
        +'The unit value may be specified using one of keywords as'+sLineBreak
        +'shown, or'+sLineBreak
        +'with a prefix of SQL_TSI_. For example, DAY and SQL_TSI_DAY'+sLineBreak
        +'both are'+sLineBreak
        +'legal.'+sLineBreak
        +' '+sLineBreak
        +'Before MariaDB 5.5, FRAC_SECOND was permitted as a synonym'+sLineBreak
        +'for MICROSECOND.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TIMESTAMPADD(MINUTE,1,''2003-01-02'');'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| TIMESTAMPADD(MINUTE,1,''2003-01-02'') |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| 2003-01-02 00:01:00 |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TIMESTAMPADD(WEEK,1,''2003-01-02'');'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| TIMESTAMPADD(WEEK,1,''2003-01-02'') |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| 2003-01-09 |'+sLineBreak
        +'+-----------------------------------+'
    ),

    (
      Name:         'TIMESTAMPDIFF';
      Declaration:  '(unit,datetime_expr1,datetime_expr2)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns datetime_expr2 - datetime_expr1, where'+sLineBreak
        +'datetime_expr1 and'+sLineBreak
        +'datetime_expr2 are date or datetime expressions. One'+sLineBreak
        +'expression may be'+sLineBreak
        +'a date and the other a datetime; a date value is treated as'+sLineBreak
        +'a datetime'+sLineBreak
        +'having the time part ''00:00:00'' where necessary. The unit'+sLineBreak
        +'for the'+sLineBreak
        +'result (an integer) is given by the unit argument. The legal'+sLineBreak
        +'values'+sLineBreak
        +'for unit are the same as those listed in the description of'+sLineBreak
        +'the'+sLineBreak
        +'TIMESTAMPADD() function, i.e MICROSECOND, SECOND, MINUTE,'+sLineBreak
        +'HOUR, DAY, WEEK, MONTH, QUARTER, or YEAR.'+sLineBreak
        +' '+sLineBreak
        +'TIMESTAMPDIFF can also be used to calculate age.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TIMESTAMPDIFF(MONTH,''2003-02-01'',''2003-05-01'');'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| TIMESTAMPDIFF(MONTH,''2003-02-01'',''2003-05-01'') |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TIMESTAMPDIFF(YEAR,''2002-05-01'',''2001-01-01'');'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| TIMESTAMPDIFF(YEAR,''2002-05-01'',''2001-01-01'') |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| -1 |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TIMESTAMPDIFF(MINUTE,''2003-02-01'',''2003-05-01'+sLineBreak
        +'12:05:55'');'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +'| TIMESTAMPDIFF(MINUTE,''2003-02-01'',''2003-05-01'+sLineBreak
        +'12:05:55'') |'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +'| 128885 |'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Calculating age:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CURDATE();'+sLineBreak
        +'+------------+'+sLineBreak
        +'| CURDATE() |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 2019-05-27 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TIMESTAMPDIFF(YEAR, ''1971-06-06'', CURDATE()) AS'+sLineBreak
        +'age;'+sLineBreak
        +' '+sLineBreak
        +'+------+'+sLineBreak
        +'| age |'+sLineBreak
        +'+------+'+sLineBreak
        +'| 47 |'+sLineBreak
        +'+------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TIMESTAMPDIFF(YEAR, ''1971-05-06'', CURDATE()) AS'+sLineBreak
        +'age;'+sLineBreak
        +' '+sLineBreak
        +'+------+'+sLineBreak
        +'| age |'+sLineBreak
        +'+------+'+sLineBreak
        +'| 48 |'+sLineBreak
        +'+------+'+sLineBreak
        +' '+sLineBreak
        +'Age as of 2014-08-02:'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, date_of_birth,'+sLineBreak
        +'TIMESTAMPDIFF(YEAR,date_of_birth,''2014-08-02'') AS age '+sLineBreak
        +' FROM student_details;'+sLineBreak
        +' '+sLineBreak
        +'+---------+---------------+------+'+sLineBreak
        +'| name | date_of_birth | age |'+sLineBreak
        +'+---------+---------------+------+'+sLineBreak
        +'| Chun | 1993-12-31 | 20 |'+sLineBreak
        +'| Esben | 1946-01-01 | 68 |'+sLineBreak
        +'| Kaolin | 1996-07-16 | 18 |'+sLineBreak
        +'| Tatiana | 1988-04-13 | 26 |'+sLineBreak
        +'+---------+---------------+------+'
    ),

    (
      Name:         'TIME_FORMAT';
      Declaration:  '(time,format)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This is used like the DATE_FORMAT() function, but the format'+sLineBreak
        +'string'+sLineBreak
        +'may contain format specifiers only for hours, minutes, and'+sLineBreak
        +'seconds.'+sLineBreak
        +'Other specifiers produce a NULL value or 0.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TIME_FORMAT(''100:00:00'', ''%H %k %h %I %l'');'+sLineBreak
        +'+--------------------------------------------+'+sLineBreak
        +'| TIME_FORMAT(''100:00:00'', ''%H %k %h %I %l'') |'+sLineBreak
        +'+--------------------------------------------+'+sLineBreak
        +'| 100 100 04 04 4 |'+sLineBreak
        +'+--------------------------------------------+'
    ),

    (
      Name:         'TIME_TO_SEC';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the time argument, converted to seconds.'+sLineBreak
        +' '+sLineBreak
        +'The value returned by TIME_TO_SEC is of type DOUBLE. Before'+sLineBreak
        +'MariaDB 5.3 (and MySQL 5.6), the type was INT. See'+sLineBreak
        +'Microseconds in MariaDB.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TIME_TO_SEC(''22:23:00'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| TIME_TO_SEC(''22:23:00'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 80580 |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TIME_TO_SEC(''00:39:38'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| TIME_TO_SEC(''00:39:38'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 2378 |'+sLineBreak
        +'+-------------------------+'
    ),

    (
      Name:         'TO_DAYS';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a date date, returns the number of days since the'+sLineBreak
        +'start of the current calendar (0000-00-00).'+sLineBreak
        +' '+sLineBreak
        +'The function is not designed for use with dates before the'+sLineBreak
        +'advent of the Gregorian calendar in October 1582. Results'+sLineBreak
        +'will not be reliable since it doesn''t account for the lost'+sLineBreak
        +'days when the calendar changed from the Julian calendar.'+sLineBreak
        +' '+sLineBreak
        +'This is the converse of the FROM_DAYS() function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TO_DAYS(''2007-10-07'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| TO_DAYS(''2007-10-07'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 733321 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TO_DAYS(''0000-01-01'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| TO_DAYS(''0000-01-01'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TO_DAYS(950501);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| TO_DAYS(950501) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 728779 |'+sLineBreak
        +'+-----------------+'
    ),

    (
      Name:         'TO_SECONDS';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of seconds from year 0 till expr, or NULL'+sLineBreak
        +'if expr is not a valid date or datetime.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TO_SECONDS(''2013-06-13'');'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| TO_SECONDS(''2013-06-13'') |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| 63538300800 |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TO_SECONDS(''2013-06-13 21:45:13'');'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| TO_SECONDS(''2013-06-13 21:45:13'') |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| 63538379113 |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TO_SECONDS(NOW());'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| TO_SECONDS(NOW()) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 63543530875 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TO_SECONDS(20130513);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| TO_SECONDS(20130513) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 63535622400 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'1 row in set (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT TO_SECONDS(130513);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| TO_SECONDS(130513) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 63535622400 |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'UNIX_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If called with no argument, returns a Unix timestamp'+sLineBreak
        +'(seconds since'+sLineBreak
        +'''1970-01-01 00:00:00'' UTC) as an unsigned integer. If'+sLineBreak
        +'UNIX_TIMESTAMP()'+sLineBreak
        +'is called with a date argument, it returns the value of the'+sLineBreak
        +'argument as seconds'+sLineBreak
        +'since ''1970-01-01 00:00:00'' UTC. date may be a DATE'+sLineBreak
        +'string, a'+sLineBreak
        +'DATETIME string, a TIMESTAMP, or a number in'+sLineBreak
        +'the format YYMMDD or YYYYMMDD. The server interprets date as'+sLineBreak
        +'a value in the'+sLineBreak
        +'current time zone and converts it to an internal value in'+sLineBreak
        +'UTC. Clients can set'+sLineBreak
        +'their time zone as described in time zones.'+sLineBreak
        +' '+sLineBreak
        +'The inverse function of UNIX_TIMESTAMP() is FROM_UNIXTIME()'+sLineBreak
        +' '+sLineBreak
        +'UNIX_TIMESTAMP() supports microseconds.'+sLineBreak
        +' '+sLineBreak
        +'Timestamps in MariaDB have a maximum value of 2147483647,'+sLineBreak
        +'equivalent to 2038-01-19 05:14:07. This is due to the'+sLineBreak
        +'underlying 32-bit limitation. Using the function on a date'+sLineBreak
        +'beyond this will result in NULL being returned. Use DATETIME'+sLineBreak
        +'as a storage type if you require dates beyond this.'+sLineBreak
        +' '+sLineBreak
        +'Error Handling'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL for wrong arguments to UNIX_TIMESTAMP(). In'+sLineBreak
        +'MySQL and MariaDB before 5.3 wrong arguments to'+sLineBreak
        +'UNIX_TIMESTAMP() returned 0. '+sLineBreak
        +' '+sLineBreak
        +'Compatibility'+sLineBreak
        +' '+sLineBreak
        +'As you can see in the examples above,'+sLineBreak
        +'UNIX_TIMESTAMP(constant-date-string) returns a timestamp'+sLineBreak
        +'with 6 decimals while MariaDB 5.2 and before returns it'+sLineBreak
        +'without decimals. This can cause a problem if you are using'+sLineBreak
        +'UNIX_TIMESTAMP() as a partitioning function. You can fix'+sLineBreak
        +'this by using FLOOR(UNIX_TIMESTAMP(..)) or changing the date'+sLineBreak
        +'string to a date number, like 20080101000000. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT UNIX_TIMESTAMP();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| UNIX_TIMESTAMP() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1269711082 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT UNIX_TIMESTAMP(''2007-11-30 10:30:19'');'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| UNIX_TIMESTAMP(''2007-11-30 10:30:19'') |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| 1196436619.000000 |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT UNIX_TIMESTAMP("2007-11-30 10:30:19.123456");'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| unix_timestamp("2007-11-30 10:30:19.123456") |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| 1196411419.123456 |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(UNIX_TIMESTAMP(''2007-11-30'+sLineBreak
        +'10:30:19''));'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(UNIX_TIMESTAMP(''2007-11-30 10:30:19'')) |'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +'| 2007-11-30 10:30:19.000000 |'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(FLOOR(UNIX_TIMESTAMP(''2007-11-30'+sLineBreak
        +'10:30:19'')));'+sLineBreak
        +'+-------------------------------------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(FLOOR(UNIX_TIMESTAMP(''2007-11-30'+sLineBreak
        +'10:30:19''))) |'+sLineBreak
        +'+-------------------------------------------------------------+'+sLineBreak
        +'| 2007-11-30 10:30:19 |'+sLineBreak
        +'+-------------------------------------------------------------+'
    ),

    (
      Name:         'UNIX_TIMESTAMP';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If called with no argument, returns a Unix timestamp'+sLineBreak
        +'(seconds since'+sLineBreak
        +'''1970-01-01 00:00:00'' UTC) as an unsigned integer. If'+sLineBreak
        +'UNIX_TIMESTAMP()'+sLineBreak
        +'is called with a date argument, it returns the value of the'+sLineBreak
        +'argument as seconds'+sLineBreak
        +'since ''1970-01-01 00:00:00'' UTC. date may be a DATE'+sLineBreak
        +'string, a'+sLineBreak
        +'DATETIME string, a TIMESTAMP, or a number in'+sLineBreak
        +'the format YYMMDD or YYYYMMDD. The server interprets date as'+sLineBreak
        +'a value in the'+sLineBreak
        +'current time zone and converts it to an internal value in'+sLineBreak
        +'UTC. Clients can set'+sLineBreak
        +'their time zone as described in time zones.'+sLineBreak
        +' '+sLineBreak
        +'The inverse function of UNIX_TIMESTAMP() is FROM_UNIXTIME()'+sLineBreak
        +' '+sLineBreak
        +'UNIX_TIMESTAMP() supports microseconds.'+sLineBreak
        +' '+sLineBreak
        +'Timestamps in MariaDB have a maximum value of 2147483647,'+sLineBreak
        +'equivalent to 2038-01-19 05:14:07. This is due to the'+sLineBreak
        +'underlying 32-bit limitation. Using the function on a date'+sLineBreak
        +'beyond this will result in NULL being returned. Use DATETIME'+sLineBreak
        +'as a storage type if you require dates beyond this.'+sLineBreak
        +' '+sLineBreak
        +'Error Handling'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL for wrong arguments to UNIX_TIMESTAMP(). In'+sLineBreak
        +'MySQL and MariaDB before 5.3 wrong arguments to'+sLineBreak
        +'UNIX_TIMESTAMP() returned 0. '+sLineBreak
        +' '+sLineBreak
        +'Compatibility'+sLineBreak
        +' '+sLineBreak
        +'As you can see in the examples above,'+sLineBreak
        +'UNIX_TIMESTAMP(constant-date-string) returns a timestamp'+sLineBreak
        +'with 6 decimals while MariaDB 5.2 and before returns it'+sLineBreak
        +'without decimals. This can cause a problem if you are using'+sLineBreak
        +'UNIX_TIMESTAMP() as a partitioning function. You can fix'+sLineBreak
        +'this by using FLOOR(UNIX_TIMESTAMP(..)) or changing the date'+sLineBreak
        +'string to a date number, like 20080101000000. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT UNIX_TIMESTAMP();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| UNIX_TIMESTAMP() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1269711082 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT UNIX_TIMESTAMP(''2007-11-30 10:30:19'');'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| UNIX_TIMESTAMP(''2007-11-30 10:30:19'') |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| 1196436619.000000 |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT UNIX_TIMESTAMP("2007-11-30 10:30:19.123456");'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| unix_timestamp("2007-11-30 10:30:19.123456") |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| 1196411419.123456 |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(UNIX_TIMESTAMP(''2007-11-30'+sLineBreak
        +'10:30:19''));'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(UNIX_TIMESTAMP(''2007-11-30 10:30:19'')) |'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +'| 2007-11-30 10:30:19.000000 |'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_UNIXTIME(FLOOR(UNIX_TIMESTAMP(''2007-11-30'+sLineBreak
        +'10:30:19'')));'+sLineBreak
        +'+-------------------------------------------------------------+'+sLineBreak
        +'| FROM_UNIXTIME(FLOOR(UNIX_TIMESTAMP(''2007-11-30'+sLineBreak
        +'10:30:19''))) |'+sLineBreak
        +'+-------------------------------------------------------------+'+sLineBreak
        +'| 2007-11-30 10:30:19 |'+sLineBreak
        +'+-------------------------------------------------------------+'
    ),

    (
      Name:         'WEEK';
      Declaration:  '(date[,mode])';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function returns the week number for date. The'+sLineBreak
        +'two-argument form of'+sLineBreak
        +'WEEK() allows you to specify whether the week starts on'+sLineBreak
        +'Sunday or Monday'+sLineBreak
        +'and whether the return value should be in the range from 0'+sLineBreak
        +'to 53 or from 1 to'+sLineBreak
        +'53. If the mode argument is omitted, the value of the'+sLineBreak
        +'default_week_format system variable is used.'+sLineBreak
        +' '+sLineBreak
        +'Modes'+sLineBreak
        +' '+sLineBreak
        +'Mode | 1st day of week | Range | Week 1 is the 1st week with'+sLineBreak
        +'| '+sLineBreak
        +' '+sLineBreak
        +'0 | Sunday | 0-53 | a Sunday in this year | '+sLineBreak
        +' '+sLineBreak
        +'1 | Monday | 0-53 | more than 3 days this year | '+sLineBreak
        +' '+sLineBreak
        +'2 | Sunday | 1-53 | a Sunday in this year | '+sLineBreak
        +' '+sLineBreak
        +'3 | Monday | 1-53 | more than 3 days this year | '+sLineBreak
        +' '+sLineBreak
        +'4 | Sunday | 0-53 | more than 3 days this year | '+sLineBreak
        +' '+sLineBreak
        +'5 | Monday | 0-53 | a Monday in this year | '+sLineBreak
        +' '+sLineBreak
        +'6 | Sunday | 1-53 | more than 3 days this year | '+sLineBreak
        +' '+sLineBreak
        +'7 | Monday | 1-53 | a Monday in this year | '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT WEEK(''2008-02-20'');'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| WEEK(''2008-02-20'') |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 7 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT WEEK(''2008-02-20'',0);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| WEEK(''2008-02-20'',0) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 7 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT WEEK(''2008-02-20'',1);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| WEEK(''2008-02-20'',1) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 8 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT WEEK(''2008-12-31'',0);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| WEEK(''2008-12-31'',0) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 52 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT WEEK(''2008-12-31'',1);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| WEEK(''2008-12-31'',1) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 53 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, WEEK(d,0), WEEK(d,1) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+-----------+-----------+'+sLineBreak
        +'| d | WEEK(d,0) | WEEK(d,1) |'+sLineBreak
        +'+---------------------+-----------+-----------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 4 | 5 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 41 | 41 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 16 | 16 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 44 | 43 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 5 | 4 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 40 | 41 |'+sLineBreak
        +'+---------------------+-----------+-----------+'
    ),

    (
      Name:         'WEEKDAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the weekday index for date '+sLineBreak
        +'(0 = Monday, 1 = Tuesday, ... 6 = Sunday).'+sLineBreak
        +' '+sLineBreak
        +'This contrasts with DAYOFWEEK() which follows the ODBC'+sLineBreak
        +'standard'+sLineBreak
        +'(1 = Sunday, 2 = Monday, ..., 7 = Saturday).'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT WEEKDAY(''2008-02-03 22:23:00'');'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| WEEKDAY(''2008-02-03 22:23:00'') |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| 6 |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT WEEKDAY(''2007-11-06'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| WEEKDAY(''2007-11-06'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT d FROM t1 where WEEKDAY(d) = 6;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2011-10-30 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'WEEKOFYEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the calendar week of the date as a number in the'+sLineBreak
        +'range from 1'+sLineBreak
        +'to 53. WEEKOFYEAR() is a compatibility function that is'+sLineBreak
        +'equivalent to'+sLineBreak
        +'WEEK(date,3).'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT WEEKOFYEAR(''2008-02-20'');'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| WEEKOFYEAR(''2008-02-20'') |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| 8 |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +' select * from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT d, WEEKOFYEAR(d), WEEK(d,3) from t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+---------------+-----------+'+sLineBreak
        +'| d | WEEKOFYEAR(d) | WEEK(d,3) |'+sLineBreak
        +'+---------------------+---------------+-----------+'+sLineBreak
        +'| 2007-01-30 21:31:07 | 5 | 5 |'+sLineBreak
        +'| 1983-10-15 06:42:51 | 41 | 41 |'+sLineBreak
        +'| 2011-04-21 12:34:56 | 16 | 16 |'+sLineBreak
        +'| 2011-10-30 06:31:41 | 43 | 43 |'+sLineBreak
        +'| 2011-01-30 14:03:25 | 4 | 4 |'+sLineBreak
        +'| 2004-10-07 11:19:34 | 41 | 41 |'+sLineBreak
        +'+---------------------+---------------+-----------+'
    ),

    (
      Name:         'YEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the year for the given date, in the range 1000 to'+sLineBreak
        +'9999, or 0 for the'+sLineBreak
        +'"zero" date.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t1 WHERE YEAR(d) = 2011;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2011-04-21 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT YEAR(''1987-01-01'');'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| YEAR(''1987-01-01'') |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 1987 |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'YEARWEEK';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns year and week for a date. The mode argument works'+sLineBreak
        +'exactly like the mode'+sLineBreak
        +'argument to WEEK(). The year in the result may be different'+sLineBreak
        +'from the'+sLineBreak
        +'year in the date argument for the first and the last week of'+sLineBreak
        +'the year.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT YEARWEEK(''1987-01-01'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| YEARWEEK(''1987-01-01'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 198652 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'6 rows in set (0.02 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT YEARWEEK(d) FROM t1 WHERE YEAR(d) = 2011;'+sLineBreak
        +' '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| YEARWEEK(d) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 201116 |'+sLineBreak
        +'| 201144 |'+sLineBreak
        +'| 201105 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'3 rows in set (0.03 sec)'
    ),

    (
      Name:         'YEARWEEK';
      Declaration:  '(date,mode)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns year and week for a date. The mode argument works'+sLineBreak
        +'exactly like the mode'+sLineBreak
        +'argument to WEEK(). The year in the result may be different'+sLineBreak
        +'from the'+sLineBreak
        +'year in the date argument for the first and the last week of'+sLineBreak
        +'the year.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT YEARWEEK(''1987-01-01'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| YEARWEEK(''1987-01-01'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 198652 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 (d DATETIME);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +' ("2007-01-30 21:31:07"),'+sLineBreak
        +' ("1983-10-15 06:42:51"),'+sLineBreak
        +' ("2011-04-21 12:34:56"),'+sLineBreak
        +' ("2011-10-30 06:31:41"),'+sLineBreak
        +' ("2011-01-30 14:03:25"),'+sLineBreak
        +' ("2004-10-07 11:19:34");'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 2007-01-30 21:31:07 |'+sLineBreak
        +'| 1983-10-15 06:42:51 |'+sLineBreak
        +'| 2011-04-21 12:34:56 |'+sLineBreak
        +'| 2011-10-30 06:31:41 |'+sLineBreak
        +'| 2011-01-30 14:03:25 |'+sLineBreak
        +'| 2004-10-07 11:19:34 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'6 rows in set (0.02 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT YEARWEEK(d) FROM t1 WHERE YEAR(d) = 2011;'+sLineBreak
        +' '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| YEARWEEK(d) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 201116 |'+sLineBreak
        +'| 201144 |'+sLineBreak
        +'| 201105 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'3 rows in set (0.03 sec)'
    ),

    (
      Name:         'COLUMN_ADD';
      Declaration:  '(dyncol_blob, column_nr, value [as type], [column_nr, value [as type]]...)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Adds or updates dynamic columns.'+sLineBreak
        +'dyncol_blob must be either a valid dynamic columns blob (for'+sLineBreak
        +'example, COLUMN_CREATE returns such blob), or an empty'+sLineBreak
        +'string.'+sLineBreak
        +'column_name specifies the name of the column to be added. If'+sLineBreak
        +'dyncol_blob already has a column with this name, it will be'+sLineBreak
        +'overwritten.'+sLineBreak
        +'value specifies the new value for the column. Passing a NULL'+sLineBreak
        +'value will cause the column to be deleted.'+sLineBreak
        +'as type is optional. See #datatypes section for a discussion'+sLineBreak
        +'about types.'+sLineBreak
        +' '+sLineBreak
        +'The return value is a dynamic column blob after the'+sLineBreak
        +'modifications.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'-- MariaDB 5.3+:'+sLineBreak
        +'UPDATE tbl SET dyncol_blob=COLUMN_ADD(dyncol_blob, 1'+sLineBreak
        +'/*column id*/, "value") WHERE id=1;'+sLineBreak
        +' '+sLineBreak
        +'-- MariaDB 10.0.1+:'+sLineBreak
        +'UPDATE t1 SET dyncol_blob=COLUMN_ADD(dyncol_blob,'+sLineBreak
        +'"column_name", "value") WHERE id=1;'+sLineBreak
        +' '+sLineBreak
        +'Note: COLUMN_ADD() is a regular function (just like'+sLineBreak
        +'CONCAT()), hence, in order to update the value in the table'+sLineBreak
        +'you have to use the UPDATE ... SET'+sLineBreak
        +'dynamic_col=COLUMN_ADD(dynamic_col,'+sLineBreak
        +'....)  pattern.'
    ),

    (
      Name:         'COLUMN_ADD';
      Declaration:  '(dyncol_blob, column_name, value [as type], [column_name, value [as type]]...)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Adds or updates dynamic columns.'+sLineBreak
        +'dyncol_blob must be either a valid dynamic columns blob (for'+sLineBreak
        +'example, COLUMN_CREATE returns such blob), or an empty'+sLineBreak
        +'string.'+sLineBreak
        +'column_name specifies the name of the column to be added. If'+sLineBreak
        +'dyncol_blob already has a column with this name, it will be'+sLineBreak
        +'overwritten.'+sLineBreak
        +'value specifies the new value for the column. Passing a NULL'+sLineBreak
        +'value will cause the column to be deleted.'+sLineBreak
        +'as type is optional. See #datatypes section for a discussion'+sLineBreak
        +'about types.'+sLineBreak
        +' '+sLineBreak
        +'The return value is a dynamic column blob after the'+sLineBreak
        +'modifications.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'-- MariaDB 5.3+:'+sLineBreak
        +'UPDATE tbl SET dyncol_blob=COLUMN_ADD(dyncol_blob, 1'+sLineBreak
        +'/*column id*/, "value") WHERE id=1;'+sLineBreak
        +' '+sLineBreak
        +'-- MariaDB 10.0.1+:'+sLineBreak
        +'UPDATE t1 SET dyncol_blob=COLUMN_ADD(dyncol_blob,'+sLineBreak
        +'"column_name", "value") WHERE id=1;'+sLineBreak
        +' '+sLineBreak
        +'Note: COLUMN_ADD() is a regular function (just like'+sLineBreak
        +'CONCAT()), hence, in order to update the value in the table'+sLineBreak
        +'you have to use the UPDATE ... SET'+sLineBreak
        +'dynamic_col=COLUMN_ADD(dynamic_col,'+sLineBreak
        +'....)  pattern.'
    ),

    (
      Name:         'COLUMN_CHECK';
      Declaration:  '(dyncol_blob)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Check if dyncol_blob is a valid packed dynamic columns blob.'+sLineBreak
        +'Return value of 1 means the blob is valid, return value of 0'+sLineBreak
        +'means it is not.'+sLineBreak
        +' '+sLineBreak
        +'Rationale:'+sLineBreak
        +'Normally, one works with valid dynamic column blobs.'+sLineBreak
        +'Functions like COLUMN_CREATE, COLUMN_ADD, COLUMN_DELETE'+sLineBreak
        +'always return valid dynamic column blobs. However, if a'+sLineBreak
        +'dynamic column blob is accidentally truncated, or transcoded'+sLineBreak
        +'from one character set to another, it will be corrupted.'+sLineBreak
        +'This function can be used to check if a value in a blob'+sLineBreak
        +'field is a valid dynamic column blob.'
    ),

    (
      Name:         'COLUMN_CREATE';
      Declaration:  '(column_nr, value [as type], [column_nr, value [as type]]...)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a dynamic columns blob that stores the specified'+sLineBreak
        +'columns with values.'+sLineBreak
        +' '+sLineBreak
        +'The return value is suitable for '+sLineBreak
        +'storing in a table'+sLineBreak
        +'further modification with other dynamic columns functions'+sLineBreak
        +' '+sLineBreak
        +'The as type part allows one to specify the value type. In'+sLineBreak
        +'most cases,'+sLineBreak
        +'this is redundant because MariaDB will be able to deduce the'+sLineBreak
        +'type of the'+sLineBreak
        +'value. Explicit type specification may be needed when the'+sLineBreak
        +'type of the value is'+sLineBreak
        +'not apparent. For example, a literal ''2012-12-01'' has a'+sLineBreak
        +'CHAR type by'+sLineBreak
        +'default, one will need to specify ''2012-12-01'' AS DATE to'+sLineBreak
        +'have it stored as'+sLineBreak
        +'a date. See Dynamic Columns:Datatypes for further details.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'-- MariaDB 5.3+:'+sLineBreak
        +'INSERT INTO tbl SET dyncol_blob=COLUMN_CREATE(1 /*column'+sLineBreak
        +'id*/, "value");'+sLineBreak
        +'-- MariaDB 10.0.1+:'+sLineBreak
        +'INSERT INTO tbl SET'+sLineBreak
        +'dyncol_blob=COLUMN_CREATE("column_name", "value");'
    ),

    (
      Name:         'COLUMN_CREATE';
      Declaration:  '(column_name, value [as type], [column_name, value [as type]]...)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a dynamic columns blob that stores the specified'+sLineBreak
        +'columns with values.'+sLineBreak
        +' '+sLineBreak
        +'The return value is suitable for '+sLineBreak
        +'storing in a table'+sLineBreak
        +'further modification with other dynamic columns functions'+sLineBreak
        +' '+sLineBreak
        +'The as type part allows one to specify the value type. In'+sLineBreak
        +'most cases,'+sLineBreak
        +'this is redundant because MariaDB will be able to deduce the'+sLineBreak
        +'type of the'+sLineBreak
        +'value. Explicit type specification may be needed when the'+sLineBreak
        +'type of the value is'+sLineBreak
        +'not apparent. For example, a literal ''2012-12-01'' has a'+sLineBreak
        +'CHAR type by'+sLineBreak
        +'default, one will need to specify ''2012-12-01'' AS DATE to'+sLineBreak
        +'have it stored as'+sLineBreak
        +'a date. See Dynamic Columns:Datatypes for further details.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'-- MariaDB 5.3+:'+sLineBreak
        +'INSERT INTO tbl SET dyncol_blob=COLUMN_CREATE(1 /*column'+sLineBreak
        +'id*/, "value");'+sLineBreak
        +'-- MariaDB 10.0.1+:'+sLineBreak
        +'INSERT INTO tbl SET'+sLineBreak
        +'dyncol_blob=COLUMN_CREATE("column_name", "value");'
    ),

    (
      Name:         'COLUMN_DELETE';
      Declaration:  '(dyncol_blob, column_nr, column_nr...)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Deletes a dynamic column with the specified name. Multiple'+sLineBreak
        +'names can be given. The return value is a dynamic column'+sLineBreak
        +'blob after the modification.'
    ),

    (
      Name:         'COLUMN_DELETE';
      Declaration:  '(dyncol_blob, column_name, column_name...)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Deletes a dynamic column with the specified name. Multiple'+sLineBreak
        +'names can be given. The return value is a dynamic column'+sLineBreak
        +'blob after the modification.'
    ),

    (
      Name:         'COLUMN_EXISTS';
      Declaration:  '(dyncol_blob, column_nr)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Checks if a column with name column_name exists in'+sLineBreak
        +'dyncol_blob. If yes, return 1, otherwise return 0. See'+sLineBreak
        +'dynamic columns for more information.'
    ),

    (
      Name:         'COLUMN_EXISTS';
      Declaration:  '(dyncol_blob, column_name)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Checks if a column with name column_name exists in'+sLineBreak
        +'dyncol_blob. If yes, return 1, otherwise return 0. See'+sLineBreak
        +'dynamic columns for more information.'
    ),

    (
      Name:         'COLUMN_GET';
      Declaration:  '(dyncol_blob, column_nr as type)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Gets the value of a dynamic column by its name. If no column'+sLineBreak
        +'with the given name exists, NULL will be returned.'+sLineBreak
        +' '+sLineBreak
        +'column_name as type requires that one specify the datatype'+sLineBreak
        +'of the dynamic column they are reading. '+sLineBreak
        +' '+sLineBreak
        +'This may seem counter-intuitive: why would one need to'+sLineBreak
        +'specify which datatype they''re retrieving? Can''t the'+sLineBreak
        +'dynamic columns system figure the datatype from the data'+sLineBreak
        +'being stored?'+sLineBreak
        +' '+sLineBreak
        +'The answer is: SQL is a statically-typed language. The SQL'+sLineBreak
        +'interpreter needs to know the datatypes of all expressions'+sLineBreak
        +'before the query is run (for example, when one is using'+sLineBreak
        +'prepared statements and runs "select COLUMN_GET(...)", the'+sLineBreak
        +'prepared statement API requires the server to inform the'+sLineBreak
        +'client about the datatype of the column being read before'+sLineBreak
        +'the query is executed and the server can see what datatype'+sLineBreak
        +'the column actually has).'+sLineBreak
        +' '+sLineBreak
        +'A note about lengths'+sLineBreak
        +' '+sLineBreak
        +'If you''re running queries like:'+sLineBreak
        +' '+sLineBreak
        +'SELECT COLUMN_GET(blob, ''colname'' as CHAR) ...'+sLineBreak
        +' '+sLineBreak
        +'without specifying a maximum length (i.e. using #as CHAR#,'+sLineBreak
        +'not as CHAR(n)), MariaDB will report the maximum length of'+sLineBreak
        +'the resultset column to be 53,6870,911 for MariaDB'+sLineBreak
        +'5.3-10.0.0 and 16,777,216 for MariaDB 10.0.1+. This may'+sLineBreak
        +'cause excessive memory usage in some client libraries,'+sLineBreak
        +'because they try to pre-allocate a buffer of maximum'+sLineBreak
        +'resultset width. To avoid this problem, use CHAR(n) whenever'+sLineBreak
        +'you''re using COLUMN_GET in the select list.'+sLineBreak
        +' '+sLineBreak
        +'See Dynamic Columns:Datatypes for more information about'+sLineBreak
        +'datatypes.'
    ),

    (
      Name:         'COLUMN_GET';
      Declaration:  '(dyncol_blob, column_name as type)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Gets the value of a dynamic column by its name. If no column'+sLineBreak
        +'with the given name exists, NULL will be returned.'+sLineBreak
        +' '+sLineBreak
        +'column_name as type requires that one specify the datatype'+sLineBreak
        +'of the dynamic column they are reading. '+sLineBreak
        +' '+sLineBreak
        +'This may seem counter-intuitive: why would one need to'+sLineBreak
        +'specify which datatype they''re retrieving? Can''t the'+sLineBreak
        +'dynamic columns system figure the datatype from the data'+sLineBreak
        +'being stored?'+sLineBreak
        +' '+sLineBreak
        +'The answer is: SQL is a statically-typed language. The SQL'+sLineBreak
        +'interpreter needs to know the datatypes of all expressions'+sLineBreak
        +'before the query is run (for example, when one is using'+sLineBreak
        +'prepared statements and runs "select COLUMN_GET(...)", the'+sLineBreak
        +'prepared statement API requires the server to inform the'+sLineBreak
        +'client about the datatype of the column being read before'+sLineBreak
        +'the query is executed and the server can see what datatype'+sLineBreak
        +'the column actually has).'+sLineBreak
        +' '+sLineBreak
        +'A note about lengths'+sLineBreak
        +' '+sLineBreak
        +'If you''re running queries like:'+sLineBreak
        +' '+sLineBreak
        +'SELECT COLUMN_GET(blob, ''colname'' as CHAR) ...'+sLineBreak
        +' '+sLineBreak
        +'without specifying a maximum length (i.e. using #as CHAR#,'+sLineBreak
        +'not as CHAR(n)), MariaDB will report the maximum length of'+sLineBreak
        +'the resultset column to be 53,6870,911 for MariaDB'+sLineBreak
        +'5.3-10.0.0 and 16,777,216 for MariaDB 10.0.1+. This may'+sLineBreak
        +'cause excessive memory usage in some client libraries,'+sLineBreak
        +'because they try to pre-allocate a buffer of maximum'+sLineBreak
        +'resultset width. To avoid this problem, use CHAR(n) whenever'+sLineBreak
        +'you''re using COLUMN_GET in the select list.'+sLineBreak
        +' '+sLineBreak
        +'See Dynamic Columns:Datatypes for more information about'+sLineBreak
        +'datatypes.'
    ),

    (
      Name:         'COLUMN_JSON';
      Declaration:  '(dyncol_blob)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a JSON representation of data in dyncol_blob. Can'+sLineBreak
        +'also be used to display nested columns. See dynamic columns'+sLineBreak
        +'for more information.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'select item_name, COLUMN_JSON(dynamic_cols) from assets;'+sLineBreak
        +'+-----------------+----------------------------------------+'+sLineBreak
        +'| item_name | COLUMN_JSON(dynamic_cols) |'+sLineBreak
        +'+-----------------+----------------------------------------+'+sLineBreak
        +'| MariaDB T-shirt | {"size":"XL","color":"blue"} |'+sLineBreak
        +'| Thinkpad Laptop | {"color":"black","warranty":"3'+sLineBreak
        +'years"} |'+sLineBreak
        +'+-----------------+----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Limitation: COLUMN_JSON will decode nested dynamic columns'+sLineBreak
        +'at a nesting level of not more than 10 levels deep. Dynamic'+sLineBreak
        +'columns that are nested deeper than 10 levels will be shown'+sLineBreak
        +'as BINARY string, without encoding.'
    ),

    (
      Name:         'COLUMN_LIST';
      Declaration:  '(dyncol_blob)';
      Category:     'Dynamic Column Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Since MariaDB 10.0.1, this function returns a'+sLineBreak
        +'comma-separated list of column names. The names are quoted'+sLineBreak
        +'with backticks.'+sLineBreak
        +' '+sLineBreak
        +'Before MariaDB 10.0.1, it returned a comma-separated list of'+sLineBreak
        +'column numbers, not names.'+sLineBreak
        +' '+sLineBreak
        +'See dynamic columns for more information.'
    ),

    (
      Name:         'AES_DECRYPT';
      Declaration:  '(crypt_str,key_str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function allows decryption of data using the official'+sLineBreak
        +'AES'+sLineBreak
        +'(Advanced Encryption Standard) algorithm. For more'+sLineBreak
        +'information, see'+sLineBreak
        +'the description of AES_ENCRYPT().'
    ),

    (
      Name:         'AES_ENCRYPT';
      Declaration:  '(str,key_str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'AES_ENCRYPT() and AES_DECRYPT() allow encryption and'+sLineBreak
        +'decryption of'+sLineBreak
        +'data using the official AES (Advanced Encryption Standard)'+sLineBreak
        +'algorithm,'+sLineBreak
        +'previously known as "Rijndael." Encoding with a 128-bit'+sLineBreak
        +'key length is'+sLineBreak
        +'used, but you can extend it up to 256 bits by modifying the'+sLineBreak
        +'source. We'+sLineBreak
        +'chose 128 bits because it is much faster and it is secure'+sLineBreak
        +'enough for'+sLineBreak
        +'most purposes.'+sLineBreak
        +' '+sLineBreak
        +'AES_ENCRYPT() encrypts a string str using the key key_str,'+sLineBreak
        +'and returns a binary string.'+sLineBreak
        +' '+sLineBreak
        +'AES_DECRYPT() decrypts the encrypted string and returns the'+sLineBreak
        +'original'+sLineBreak
        +'string.'+sLineBreak
        +' '+sLineBreak
        +'The input arguments may be any length. If either argument is'+sLineBreak
        +'NULL, the result of this function is also NULL.'+sLineBreak
        +' '+sLineBreak
        +'Because AES is a block-level algorithm, padding is used to'+sLineBreak
        +'encode'+sLineBreak
        +'uneven length strings and so the result string length may be'+sLineBreak
        +'calculated using this formula:'+sLineBreak
        +' '+sLineBreak
        +'16 x (trunc(string_length / 16) + 1)'+sLineBreak
        +' '+sLineBreak
        +'If AES_DECRYPT() detects invalid data or incorrect padding,'+sLineBreak
        +'it returns'+sLineBreak
        +'NULL. However, it is possible for AES_DECRYPT() to return a'+sLineBreak
        +'non-NULL'+sLineBreak
        +'value (possibly garbage) if the input data or the key is'+sLineBreak
        +'invalid.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'INSERT INTO t VALUES'+sLineBreak
        +'(AES_ENCRYPT(''text'',SHA2(''password'',512)));'
    ),

    (
      Name:         'COMPRESS';
      Declaration:  '(string_to_compress)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Compresses a string and returns the result as a binary'+sLineBreak
        +'string. This'+sLineBreak
        +'function requires MariaDB to have been compiled with a'+sLineBreak
        +'compression'+sLineBreak
        +'library such as zlib. Otherwise, the return value is always'+sLineBreak
        +'NULL. The'+sLineBreak
        +'compressed string can be uncompressed with UNCOMPRESS().'+sLineBreak
        +' '+sLineBreak
        +'The have_compress server system variable indicates whether a'+sLineBreak
        +'compression library is present. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LENGTH(COMPRESS(REPEAT(''a'',1000)));'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| LENGTH(COMPRESS(REPEAT(''a'',1000))) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| 21 |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LENGTH(COMPRESS(''''));'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| LENGTH(COMPRESS('''')) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LENGTH(COMPRESS(''a''));'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| LENGTH(COMPRESS(''a'')) |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 13 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LENGTH(COMPRESS(REPEAT(''a'',16)));'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| LENGTH(COMPRESS(REPEAT(''a'',16))) |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| 15 |'+sLineBreak
        +'+----------------------------------+'
    ),

    (
      Name:         'DECODE';
      Declaration:  '(crypt_str,pass_str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Decrypts the encrypted string crypt_str using pass_str as'+sLineBreak
        +'the'+sLineBreak
        +'password. crypt_str should be a string returned from'+sLineBreak
        +'ENCODE(). The resulting string will be the original string'+sLineBreak
        +'only if pass_str is the same.'
    ),

    (
      Name:         'DES_DECRYPT';
      Declaration:  '(crypt_str[,key_str])';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Decrypts a string encrypted with DES_ENCRYPT(). If an error'+sLineBreak
        +'occurs,'+sLineBreak
        +'this function returns NULL.'+sLineBreak
        +' '+sLineBreak
        +'This function works only if MariaDB has been configured with'+sLineBreak
        +'TLS'+sLineBreak
        +'support.'+sLineBreak
        +' '+sLineBreak
        +'If no key_str argument is given, DES_DECRYPT() examines the'+sLineBreak
        +'first byte'+sLineBreak
        +'of the encrypted string to determine the DES key number that'+sLineBreak
        +'was used'+sLineBreak
        +'to encrypt the original string, and then reads the key from'+sLineBreak
        +'the DES'+sLineBreak
        +'key file to decrypt the message. For this to work, the user'+sLineBreak
        +'must have'+sLineBreak
        +'the SUPER privilege. The key file can be specified with the'+sLineBreak
        +'--des-key-file server option.'+sLineBreak
        +' '+sLineBreak
        +'If you pass this function a key_str argument, that string is'+sLineBreak
        +'used as'+sLineBreak
        +'the key for decrypting the message.'+sLineBreak
        +' '+sLineBreak
        +'If the crypt_str argument does not appear to be an encrypted'+sLineBreak
        +'string,'+sLineBreak
        +'MariaDB returns the given crypt_str.'
    ),

    (
      Name:         'DES_ENCRYPT';
      Declaration:  '(str[,{key_num|key_str}])';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Encrypts the string with the given key using the Triple-DES'+sLineBreak
        +'algorithm.'+sLineBreak
        +' '+sLineBreak
        +'This function works only if MariaDB has been configured with'+sLineBreak
        +'TLS support.'+sLineBreak
        +' '+sLineBreak
        +'The encryption key to use is chosen based on the second'+sLineBreak
        +'argument to'+sLineBreak
        +'DES_ENCRYPT(), if one was given. With no argument, the first'+sLineBreak
        +'key from'+sLineBreak
        +'the DES key file is used. With a key_num argument, the given'+sLineBreak
        +'key '+sLineBreak
        +'number (0-9) from the DES key file is used. With a key_str'+sLineBreak
        +'argument,'+sLineBreak
        +'the given key string is used to encrypt str. '+sLineBreak
        +' '+sLineBreak
        +'The key file can be specified with the --des-key-file server'+sLineBreak
        +'option.'+sLineBreak
        +' '+sLineBreak
        +'The return string is a binary string where the first'+sLineBreak
        +'character is '+sLineBreak
        +'CHAR(128 | key_num). If an error occurs, DES_ENCRYPT()'+sLineBreak
        +'returns NULL.'+sLineBreak
        +' '+sLineBreak
        +'The 128 is added to make it easier to recognize an encrypted'+sLineBreak
        +'key. If'+sLineBreak
        +'you use a string key, key_num is 127.'+sLineBreak
        +' '+sLineBreak
        +'The string length for the result is given by this formula:'+sLineBreak
        +' '+sLineBreak
        +'new_len = orig_len + (8 - (orig_len % 8)) + 1'+sLineBreak
        +' '+sLineBreak
        +'Each line in the DES key file has the following format:'+sLineBreak
        +' '+sLineBreak
        +'key_num des_key_str'+sLineBreak
        +' '+sLineBreak
        +'Each key_num value must be a number in the range from 0 to'+sLineBreak
        +'9. Lines in'+sLineBreak
        +'the file may be in any order. des_key_str is the string that'+sLineBreak
        +'is used'+sLineBreak
        +'to encrypt the message. There should be at least one space'+sLineBreak
        +'between the'+sLineBreak
        +'number and the key. The first key is the default key that is'+sLineBreak
        +'used if'+sLineBreak
        +'you do not specify any key argument to DES_ENCRYPT().'+sLineBreak
        +' '+sLineBreak
        +'You can tell MariaDB to read new key values from the key'+sLineBreak
        +'file with the'+sLineBreak
        +'FLUSH DES_KEY_FILE statement. This requires the RELOAD'+sLineBreak
        +'privilege.'+sLineBreak
        +' '+sLineBreak
        +'One benefit of having a set of default keys is that it gives'+sLineBreak
        +'applications a way to check for the existence of encrypted'+sLineBreak
        +'column'+sLineBreak
        +'values, without giving the end user the right to decrypt'+sLineBreak
        +'those values.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT customer_address FROM customer_table '+sLineBreak
        +' WHERE crypted_credit_card ='+sLineBreak
        +'DES_ENCRYPT(''credit_card_number'');'
    ),

    (
      Name:         'ENCODE';
      Declaration:  '(str,pass_str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'ENCODE is not considered cryptographically secure, and'+sLineBreak
        +'should not be used for password encryption.'+sLineBreak
        +' '+sLineBreak
        +'Encrypt str using pass_str as the password. To decrypt the'+sLineBreak
        +'result, use'+sLineBreak
        +'DECODE().'+sLineBreak
        +' '+sLineBreak
        +'The result is a binary string of the same length as str.'+sLineBreak
        +' '+sLineBreak
        +'The strength of the encryption is based on how good the'+sLineBreak
        +'random generator is. '+sLineBreak
        +' '+sLineBreak
        +'It is not recommended to rely on the encryption performed by'+sLineBreak
        +'the ENCODE function. Using a salt value (changed when a'+sLineBreak
        +'password is updated) will improve matters somewhat, but for'+sLineBreak
        +'storing passwords, consider a more cryptographically secure'+sLineBreak
        +'function, such as SHA2().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'ENCODE(''not so secret text'','+sLineBreak
        +'CONCAT(''random_salt'',''password''))'
    ),

    (
      Name:         'ENCRYPT';
      Declaration:  '(str[,salt])';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Encrypts a string using the Unix crypt() system call,'+sLineBreak
        +'returning an encrypted binary string. The salt argument'+sLineBreak
        +'should be a string with at least two characters or the'+sLineBreak
        +'returned result will be NULL. If no salt argument is given,'+sLineBreak
        +'a random value of sufficient length is used.'+sLineBreak
        +' '+sLineBreak
        +'It is not recommended to use ENCRYPT() with utf16, utf32 or'+sLineBreak
        +'ucs2 multi-byte character sets because the crypt() system'+sLineBreak
        +'call expects a string terminated with a zero byte.'+sLineBreak
        +' '+sLineBreak
        +'Note that the underlying crypt() system call may have some'+sLineBreak
        +'limitations, such as ignoring all but the first eight'+sLineBreak
        +'characters.'+sLineBreak
        +' '+sLineBreak
        +'If the have_crypt system variable is set to NO (because the'+sLineBreak
        +'crypt() system call is not available), the ENCRYPT function'+sLineBreak
        +'will always return NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ENCRYPT(''encrypt me'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| ENCRYPT(''encrypt me'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 4I5BsEx0lqTDk |'+sLineBreak
        +'+-----------------------+'
    ),

    (
      Name:         'MD5';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Calculates an MD5 128-bit checksum for the string. '+sLineBreak
        +' '+sLineBreak
        +'The return value is a 32-hex digit string, and as of MariaDB'+sLineBreak
        +'5.5, is a nonbinary string in the connection character set'+sLineBreak
        +'and collation, determined by the values of the'+sLineBreak
        +'character_set_connection and collation_connection system'+sLineBreak
        +'variables. Before 5.5, the return value was a binary string.'+sLineBreak
        +' '+sLineBreak
        +'NULL is returned if the argument was NULL. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MD5(''testing'');'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| MD5(''testing'') |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| ae2b1fca515949e5d54fb22b8ed95575 |'+sLineBreak
        +'+----------------------------------+'
    ),

    (
      Name:         'OLD_PASSWORD';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'OLD_PASSWORD() was added to MySQL when the implementation of'+sLineBreak
        +''+sLineBreak
        +'PASSWORD() was changed to improve security. OLD_PASSWORD()'+sLineBreak
        +'returns the'+sLineBreak
        +'value of the old (pre-MySQL 4.1) implementation of'+sLineBreak
        +'PASSWORD() as a'+sLineBreak
        +'string, and is intended to permit you to reset passwords for'+sLineBreak
        +'any'+sLineBreak
        +'pre-4.1 clients that need to connect to a more recent MySQL'+sLineBreak
        +'server version, or any version of MariaDB,'+sLineBreak
        +'without locking them out.'+sLineBreak
        +' '+sLineBreak
        +'As of MariaDB 5.5, the return value is a nonbinary string in'+sLineBreak
        +'the connection character set and collation, determined by'+sLineBreak
        +'the values of the character_set_connection and'+sLineBreak
        +'collation_connection system variables. Before 5.5, the'+sLineBreak
        +'return value was a binary string.'+sLineBreak
        +' '+sLineBreak
        +'The return value is 16 bytes in length, or NULL if the'+sLineBreak
        +'argument was NULL.'
    ),

    (
      Name:         'PASSWORD';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The PASSWORD() function is used for hashing passwords for'+sLineBreak
        +'use in authentication by the MariaDB server. It is not'+sLineBreak
        +'intended for use in other applications.'+sLineBreak
        +' '+sLineBreak
        +'Calculates and returns a hashed password string from the'+sLineBreak
        +'plaintext password str. Returns an empty string (>= MariaDB'+sLineBreak
        +'10.0.4) or NULL ('
    ),

    (
      Name:         'SHA1';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Calculates an SHA-1 160-bit checksum for the string str, as'+sLineBreak
        +'described in'+sLineBreak
        +'RFC 3174 (Secure Hash Algorithm).'+sLineBreak
        +' '+sLineBreak
        +'The value is returned as a string of 40 hex digits, or NULL'+sLineBreak
        +'if the argument was NULL. As of MariaDB 5.5, the return'+sLineBreak
        +'value is a nonbinary string in the connection character set'+sLineBreak
        +'and collation, determined by the values of the'+sLineBreak
        +'character_set_connection and collation_connection system'+sLineBreak
        +'variables. Before 5.5, the return value was a binary string.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SHA1(''some boring text'');'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| SHA1(''some boring text'') |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| af969fc2085b1bb6d31e517d5c456def5cdd7093 |'+sLineBreak
        +'+------------------------------------------+'
    ),

    (
      Name:         'SHA2';
      Declaration:  '(str,hash_len)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a string str, calculates an SHA-2 checksum, which is'+sLineBreak
        +'considered more cryptographically secure than its SHA-1'+sLineBreak
        +'equivalent. The SHA-2 family includes SHA-224, SHA-256,'+sLineBreak
        +'SHA-384, and SHA-512, and the hash_len must correspond to'+sLineBreak
        +'one of these, i.e. 224, 256, 384 or 512. 0 is equivalent to'+sLineBreak
        +'256.'+sLineBreak
        +' '+sLineBreak
        +'The return value is a nonbinary string in the connection'+sLineBreak
        +'character set and collation, determined by the values of the'+sLineBreak
        +'character_set_connection and collation_connection system'+sLineBreak
        +'variables. '+sLineBreak
        +' '+sLineBreak
        +'NULL is returned if the hash length is not valid, or the'+sLineBreak
        +'string str is NULL.'+sLineBreak
        +' '+sLineBreak
        +'SHA2 will only work if MariaDB was has been configured with'+sLineBreak
        +'TLS support. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SHA2(''Maria'',224);'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +'| SHA2(''Maria'',224) |'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +'| 6cc67add32286412efcab9d0e1675a43a5c2ef3cec8879f81516ff83 |'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SHA2(''Maria'',256);'+sLineBreak
        +'+------------------------------------------------------------------+'+sLineBreak
        +'| SHA2(''Maria'',256) |'+sLineBreak
        +'+------------------------------------------------------------------+'+sLineBreak
        +'|'+sLineBreak
        +'9ff18ebe7449349f358e3af0b57cf7a032c1c6b2272cb2656ff85eb112232f16'+sLineBreak
        +'|'+sLineBreak
        +'+------------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SHA2(''Maria'',0);'+sLineBreak
        +'+------------------------------------------------------------------+'+sLineBreak
        +'| SHA2(''Maria'',0) |'+sLineBreak
        +'+------------------------------------------------------------------+'+sLineBreak
        +'|'+sLineBreak
        +'9ff18ebe7449349f358e3af0b57cf7a032c1c6b2272cb2656ff85eb112232f16'+sLineBreak
        +'|'+sLineBreak
        +'+------------------------------------------------------------------+'
    ),

    (
      Name:         'UNCOMPRESS';
      Declaration:  '(string_to_uncompress)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Uncompresses a string compressed by the COMPRESS() function.'+sLineBreak
        +'If the'+sLineBreak
        +'argument is not a compressed value, the result is NULL. This'+sLineBreak
        +'function'+sLineBreak
        +'requires MariaDB to have been compiled with a compression'+sLineBreak
        +'library such'+sLineBreak
        +'as zlib. Otherwise, the return value is always NULL. The'+sLineBreak
        +'have_compress server system variable indicates whether a'+sLineBreak
        +'compression library is present. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT UNCOMPRESS(COMPRESS(''a string''));'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| UNCOMPRESS(COMPRESS(''a string'')) |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| a string |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT UNCOMPRESS(''a string'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| UNCOMPRESS(''a string'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+------------------------+'
    ),

    (
      Name:         'UNCOMPRESSED_LENGTH';
      Declaration:  '(compressed_string)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length that the compressed string had before'+sLineBreak
        +'being'+sLineBreak
        +'compressed with COMPRESS().'+sLineBreak
        +' '+sLineBreak
        +'UNCOMPRESSED_LENGTH() returns NULL or an incorrect result if'+sLineBreak
        +'the string is not compressed.'+sLineBreak
        +' '+sLineBreak
        +'Until MariaDB 10.3.1, returns MYSQL_TYPE_LONGLONG, or'+sLineBreak
        +'bigint(10), in all cases. From MariaDB 10.3.1, returns'+sLineBreak
        +'MYSQL_TYPE_LONG, or int(10), when the result would fit'+sLineBreak
        +'within 32-bits.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT UNCOMPRESSED_LENGTH(COMPRESS(REPEAT(''a'',30)));'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| UNCOMPRESSED_LENGTH(COMPRESS(REPEAT(''a'',30))) |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| 30 |'+sLineBreak
        +'+-----------------------------------------------+'
    ),

    (
      Name:         'AVG';
      Declaration:  '([DISTINCT] expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the average value of expr. The DISTINCT option can'+sLineBreak
        +'be used to return the average of the distinct values of'+sLineBreak
        +'expr. NULL values are ignored. It is an aggregate function,'+sLineBreak
        +'and so can be used with the GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'AVG() returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.0, AVG() can be used as a window function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE sales (sales_value INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO sales VALUES(10),(20),(20),(40);'+sLineBreak
        +' '+sLineBreak
        +'SELECT AVG(sales_value) FROM sales;'+sLineBreak
        +' '+sLineBreak
        +'+------------------+'+sLineBreak
        +'| AVG(sales_value) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 22.5000 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT AVG(DISTINCT(sales_value)) FROM sales;'+sLineBreak
        +' '+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| AVG(DISTINCT(sales_value)) |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| 23.3333 |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Commonly, AVG() is used with a GROUP BY clause:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student (name CHAR(10), test CHAR(10), score'+sLineBreak
        +'TINYINT); '+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87), (''Tatiana'', ''Tuning'', 83);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, AVG(score) FROM student GROUP BY name;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| name | AVG(score) |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| Chun | 74.0000 |'+sLineBreak
        +'| Esben | 37.0000 |'+sLineBreak
        +'| Kaolin | 72.0000 |'+sLineBreak
        +'| Tatiana | 85.0000 |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +' '+sLineBreak
        +'Be careful to avoid this common mistake, not grouping'+sLineBreak
        +'correctly and returning mismatched data: '+sLineBreak
        +' '+sLineBreak
        +'SELECT name,test,AVG(score) FROM student;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +'| name | test | MIN(score) |'+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +'| Chun | SQL | 31 |'+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student_test (name CHAR(10), test CHAR(10),'+sLineBreak
        +'score TINYINT); '+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87), (''Tatiana'', ''Tuning'', 83);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, AVG(score) OVER (PARTITION BY'+sLineBreak
        +'test) '+sLineBreak
        +' AS average_by_test FROM student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+-----------------+'+sLineBreak
        +'| name | test | score | average_by_test |'+sLineBreak
        +'+---------+--------+-------+-----------------+'+sLineBreak
        +'| Chun | SQL | 75 | 65.2500 |'+sLineBreak
        +'| Chun | Tuning | 73 | 68.7500 |'+sLineBreak
        +'| Esben | SQL | 43 | 65.2500 |'+sLineBreak
        +'| Esben | Tuning | 31 | 68.7500 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 65.2500 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 68.7500 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 65.2500 |'+sLineBreak
        +'| Tatiana | Tuning | 83 | 68.7500 |'+sLineBreak
        +'+---------+--------+-------+-----------------+'
    ),

    (
      Name:         'BIT_AND';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the bitwise AND of all bits in expr. The calculation'+sLineBreak
        +'is performed with 64-bit (BIGINT) precision. It is an'+sLineBreak
        +'aggregate function, and so can be used with the GROUP BY'+sLineBreak
        +'clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.0, BIT_AND() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE vals (x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO vals VALUES(111),(110),(100);'+sLineBreak
        +' '+sLineBreak
        +'SELECT BIT_AND(x), BIT_OR(x), BIT_XOR(x) FROM vals;'+sLineBreak
        +' '+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +'| BIT_AND(x) | BIT_OR(x) | BIT_XOR(x) |'+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +'| 100 | 111 | 101 |'+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE vals2 (category VARCHAR(1), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO vals2 VALUES'+sLineBreak
        +' (''a'',111),(''a'',110),(''a'',100),'+sLineBreak
        +' (''b'',''000''),(''b'',001),(''b'',011);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, BIT_AND(x), BIT_OR(x), BIT_XOR(x) '+sLineBreak
        +' FROM vals GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+------------+-----------+------------+'+sLineBreak
        +'| category | BIT_AND(x) | BIT_OR(x) | BIT_XOR(x) |'+sLineBreak
        +'+----------+------------+-----------+------------+'+sLineBreak
        +'| a | 100 | 111 | 101 |'+sLineBreak
        +'| b | 0 | 11 | 10 |'+sLineBreak
        +'+----------+------------+-----------+------------+'
    ),

    (
      Name:         'BIT_OR';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the bitwise OR of all bits in expr. The calculation'+sLineBreak
        +'is performed with 64-bit (BIGINT) precision. It is an'+sLineBreak
        +'aggregate function, and so can be used with the GROUP BY'+sLineBreak
        +'clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.0, BIT_OR can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE vals (x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO vals VALUES(111),(110),(100);'+sLineBreak
        +' '+sLineBreak
        +'SELECT BIT_AND(x), BIT_OR(x), BIT_XOR(x) FROM vals;'+sLineBreak
        +' '+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +'| BIT_AND(x) | BIT_OR(x) | BIT_XOR(x) |'+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +'| 100 | 111 | 101 |'+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE vals2 (category VARCHAR(1), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO vals2 VALUES'+sLineBreak
        +' (''a'',111),(''a'',110),(''a'',100),'+sLineBreak
        +' (''b'',''000''),(''b'',001),(''b'',011);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, BIT_AND(x), BIT_OR(x), BIT_XOR(x) '+sLineBreak
        +' FROM vals GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+------------+-----------+------------+'+sLineBreak
        +'| category | BIT_AND(x) | BIT_OR(x) | BIT_XOR(x) |'+sLineBreak
        +'+----------+------------+-----------+------------+'+sLineBreak
        +'| a | 100 | 111 | 101 |'+sLineBreak
        +'| b | 0 | 11 | 10 |'+sLineBreak
        +'+----------+------------+-----------+------------+'
    ),

    (
      Name:         'BIT_XOR';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the bitwise XOR of all bits in expr. The calculation'+sLineBreak
        +'is performed with 64-bit (BIGINT) precision. It is an'+sLineBreak
        +'aggregate function, and so can be used with the GROUP BY'+sLineBreak
        +'clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.0, BIT_XOR() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE vals (x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO vals VALUES(111),(110),(100);'+sLineBreak
        +' '+sLineBreak
        +'SELECT BIT_AND(x), BIT_OR(x), BIT_XOR(x) FROM vals;'+sLineBreak
        +' '+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +'| BIT_AND(x) | BIT_OR(x) | BIT_XOR(x) |'+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +'| 100 | 111 | 101 |'+sLineBreak
        +'+------------+-----------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE vals2 (category VARCHAR(1), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO vals2 VALUES'+sLineBreak
        +' (''a'',111),(''a'',110),(''a'',100),'+sLineBreak
        +' (''b'',''000''),(''b'',001),(''b'',011);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, BIT_AND(x), BIT_OR(x), BIT_XOR(x) '+sLineBreak
        +' FROM vals GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+------------+-----------+------------+'+sLineBreak
        +'| category | BIT_AND(x) | BIT_OR(x) | BIT_XOR(x) |'+sLineBreak
        +'+----------+------------+-----------+------------+'+sLineBreak
        +'| a | 100 | 111 | 101 |'+sLineBreak
        +'| b | 0 | 11 | 10 |'+sLineBreak
        +'+----------+------------+-----------+------------+'
    ),

    (
      Name:         'COUNT';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a count of the number of non-NULL values of expr in'+sLineBreak
        +'the rows retrieved by a SELECT statement. The result is a'+sLineBreak
        +'BIGINT value. It is an aggregate function, and so can be'+sLineBreak
        +'used with the GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'COUNT(*) counts the total number of rows in a table.'+sLineBreak
        +' '+sLineBreak
        +'COUNT() returns 0 if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.0, COUNT() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE student (name CHAR(10), test CHAR(10), score'+sLineBreak
        +'TINYINT); '+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87), (''Tatiana'', ''Tuning'', 83);'+sLineBreak
        +' '+sLineBreak
        +'SELECT COUNT(*) FROM student;'+sLineBreak
        +' '+sLineBreak
        +'+----------+'+sLineBreak
        +'| COUNT(*) |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| 8 |'+sLineBreak
        +'+----------+'+sLineBreak
        +' '+sLineBreak
        +'COUNT(DISTINCT) example:'+sLineBreak
        +' '+sLineBreak
        +'SELECT COUNT(DISTINCT (name)) FROM student;'+sLineBreak
        +' '+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| COUNT(DISTINCT (name)) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, COUNT(score) OVER (PARTITION BY'+sLineBreak
        +'name) '+sLineBreak
        +' AS tests_written FROM student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+---------------+'+sLineBreak
        +'| name | test | score | tests_written |'+sLineBreak
        +'+---------+--------+-------+---------------+'+sLineBreak
        +'| Chun | SQL | 75 | 2 |'+sLineBreak
        +'| Chun | Tuning | 73 | 2 |'+sLineBreak
        +'| Esben | SQL | 43 | 2 |'+sLineBreak
        +'| Esben | Tuning | 31 | 2 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 2 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 2 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 1 |'+sLineBreak
        +'+---------+--------+-------+---------------+'
    ),

    (
      Name:         'GROUP_CONCAT';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function returns a string result with the concatenated'+sLineBreak
        +'non-NULL'+sLineBreak
        +'values from a group. It returns NULL if there are no'+sLineBreak
        +'non-NULL values.'+sLineBreak
        +' '+sLineBreak
        +'The maximum returned length in bytes is determined by the'+sLineBreak
        +'group_concat_max_len server system variable, which defaults'+sLineBreak
        +'to 1M (>= MariaDB 10.2.4) or 1K ('
    ),

    (
      Name:         'MAX';
      Declaration:  '([DISTINCT] expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the largest, or maximum, value of expr. MAX() can'+sLineBreak
        +'also take a string'+sLineBreak
        +'argument in which case it returns the maximum string value.'+sLineBreak
        +'The DISTINCT'+sLineBreak
        +'keyword can be used to find the maximum of the distinct'+sLineBreak
        +'values of expr,'+sLineBreak
        +'however, this produces the same result as omitting DISTINCT.'+sLineBreak
        +' '+sLineBreak
        +'Note that SET and ENUM fields are currently compared by'+sLineBreak
        +'their string value rather than their relative position in'+sLineBreak
        +'the set, so MAX() may produce a different highest result'+sLineBreak
        +'than ORDER BY DESC.'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, MAX() can be used as a window function.'+sLineBreak
        +' '+sLineBreak
        +'MAX() returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE student (name CHAR(10), test CHAR(10), score'+sLineBreak
        +'TINYINT); '+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87), (''Tatiana'', ''Tuning'', 83);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, MAX(score) FROM student GROUP BY name;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| name | MAX(score) |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| Chun | 75 |'+sLineBreak
        +'| Esben | 43 |'+sLineBreak
        +'| Kaolin | 88 |'+sLineBreak
        +'| Tatiana | 87 |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +' '+sLineBreak
        +'MAX string:'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAX(name) FROM student;'+sLineBreak
        +' '+sLineBreak
        +'+-----------+'+sLineBreak
        +'| MAX(name) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| Tatiana |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'Be careful to avoid this common mistake, not grouping'+sLineBreak
        +'correctly and returning mismatched data: '+sLineBreak
        +' '+sLineBreak
        +'SELECT name,test,MAX(SCORE) FROM student;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +'| name | test | MAX(SCORE) |'+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +'| Chun | SQL | 88 |'+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +' '+sLineBreak
        +'Difference between ORDER BY DESC and MAX():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student2(name CHAR(10),grade'+sLineBreak
        +'ENUM(''b'',''c'',''a''));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student2'+sLineBreak
        +'VALUES(''Chun'',''b''),(''Esben'',''c''),(''Kaolin'',''a'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAX(grade) FROM student2;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| MAX(grade) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| c |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT grade FROM student2 ORDER BY grade DESC LIMIT 1;'+sLineBreak
        +' '+sLineBreak
        +'+-------+'+sLineBreak
        +'| grade |'+sLineBreak
        +'+-------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+-------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, MAX(score) '+sLineBreak
        +' OVER (PARTITION BY name) AS highest_score FROM'+sLineBreak
        +'student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+---------------+'+sLineBreak
        +'| name | test | score | highest_score |'+sLineBreak
        +'+---------+--------+-------+---------------+'+sLineBreak
        +'| Chun | SQL | 75 | 75 |'+sLineBreak
        +'| Chun | Tuning | 73 | 75 |'+sLineBreak
        +'| Esben | SQL | 43 | 43 |'+sLineBreak
        +'| Esben | Tuning | 31 | 43 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 88 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 88 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 87 |'+sLineBreak
        +'+---------+--------+-------+---------------+'
    ),

    (
      Name:         'MIN';
      Declaration:  '([DISTINCT] expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the minimum value of expr. MIN() may take a string'+sLineBreak
        +'argument, in which case it returns the minimum string value.'+sLineBreak
        +'The DISTINCT'+sLineBreak
        +'keyword can be used to find the minimum of the distinct'+sLineBreak
        +'values of expr,'+sLineBreak
        +'however, this produces the same result as omitting DISTINCT.'+sLineBreak
        +' '+sLineBreak
        +'Note that SET and ENUM fields are currently compared by'+sLineBreak
        +'their string value rather than their relative position in'+sLineBreak
        +'the set, so MIN() may produce a different lowest result than'+sLineBreak
        +'ORDER BY ASC.'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, MIN() can be used as a window function.'+sLineBreak
        +' '+sLineBreak
        +'MIN() returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE student (name CHAR(10), test CHAR(10), score'+sLineBreak
        +'TINYINT); '+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87), (''Tatiana'', ''Tuning'', 83);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, MIN(score) FROM student GROUP BY name;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| name | MIN(score) |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| Chun | 73 |'+sLineBreak
        +'| Esben | 31 |'+sLineBreak
        +'| Kaolin | 56 |'+sLineBreak
        +'| Tatiana | 83 |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +' '+sLineBreak
        +'MIN() with a string:'+sLineBreak
        +' '+sLineBreak
        +'SELECT MIN(name) FROM student;'+sLineBreak
        +' '+sLineBreak
        +'+-----------+'+sLineBreak
        +'| MIN(name) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| Chun |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'Be careful to avoid this common mistake, not grouping'+sLineBreak
        +'correctly and returning mismatched data: '+sLineBreak
        +' '+sLineBreak
        +'SELECT name,test,MIN(score) FROM student;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +'| name | test | MIN(score) |'+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +'| Chun | SQL | 31 |'+sLineBreak
        +'+------+------+------------+'+sLineBreak
        +' '+sLineBreak
        +'Difference between ORDER BY ASC and MIN():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student2(name CHAR(10),grade'+sLineBreak
        +'ENUM(''b'',''c'',''a''));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student2'+sLineBreak
        +'VALUES(''Chun'',''b''),(''Esben'',''c''),(''Kaolin'',''a'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT MIN(grade) FROM student2;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| MIN(grade) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT grade FROM student2 ORDER BY grade ASC LIMIT 1;'+sLineBreak
        +' '+sLineBreak
        +'+-------+'+sLineBreak
        +'| grade |'+sLineBreak
        +'+-------+'+sLineBreak
        +'| b |'+sLineBreak
        +'+-------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, MIN(score) '+sLineBreak
        +' OVER (PARTITION BY name) AS lowest_score FROM student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+--------------+'+sLineBreak
        +'| name | test | score | lowest_score |'+sLineBreak
        +'+---------+--------+-------+--------------+'+sLineBreak
        +'| Chun | SQL | 75 | 73 |'+sLineBreak
        +'| Chun | Tuning | 73 | 73 |'+sLineBreak
        +'| Esben | SQL | 43 | 31 |'+sLineBreak
        +'| Esben | Tuning | 31 | 31 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 56 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 56 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 87 |'+sLineBreak
        +'+---------+--------+-------+--------------+'
    ),

    (
      Name:         'STD';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the population standard deviation of expr. This is'+sLineBreak
        +'an extension'+sLineBreak
        +'to standard SQL. The standard SQL function STDDEV_POP() can'+sLineBreak
        +'be used instead. '+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, STD() can be used as a window function.'+sLineBreak
        +' '+sLineBreak
        +'This function returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE stats (category VARCHAR(2), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO stats VALUES '+sLineBreak
        +' (''a'',1),(''a'',2),(''a'',3),'+sLineBreak
        +' (''b'',11),(''b'',12),(''b'',20),(''b'',30),(''b'',60);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, STDDEV_POP(x), STDDEV_SAMP(x), VAR_POP(x) '+sLineBreak
        +' FROM stats GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| category | STDDEV_POP(x) | STDDEV_SAMP(x) | VAR_POP(x) |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| a | 0.8165 | 1.0000 | 0.6667 |'+sLineBreak
        +'| b | 18.0400 | 20.1693 | 325.4400 |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, STDDEV_POP(score) '+sLineBreak
        +' OVER (PARTITION BY test) AS stddev_results FROM'+sLineBreak
        +'student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+----------------+'+sLineBreak
        +'| name | test | score | stddev_results |'+sLineBreak
        +'+---------+--------+-------+----------------+'+sLineBreak
        +'| Chun | SQL | 75 | 16.9466 |'+sLineBreak
        +'| Chun | Tuning | 73 | 24.1247 |'+sLineBreak
        +'| Esben | SQL | 43 | 16.9466 |'+sLineBreak
        +'| Esben | Tuning | 31 | 24.1247 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 16.9466 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 24.1247 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 16.9466 |'+sLineBreak
        +'+---------+--------+-------+----------------+'
    ),

    (
      Name:         'STDDEV';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the population standard deviation of expr. This'+sLineBreak
        +'function is'+sLineBreak
        +'provided for compatibility with Oracle. The standard SQL'+sLineBreak
        +'function'+sLineBreak
        +'STDDEV_POP() can be used instead.'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, STDDEV() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'This function returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE stats (category VARCHAR(2), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO stats VALUES '+sLineBreak
        +' (''a'',1),(''a'',2),(''a'',3),'+sLineBreak
        +' (''b'',11),(''b'',12),(''b'',20),(''b'',30),(''b'',60);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, STDDEV_POP(x), STDDEV_SAMP(x), VAR_POP(x) '+sLineBreak
        +' FROM stats GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| category | STDDEV_POP(x) | STDDEV_SAMP(x) | VAR_POP(x) |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| a | 0.8165 | 1.0000 | 0.6667 |'+sLineBreak
        +'| b | 18.0400 | 20.1693 | 325.4400 |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, STDDEV_POP(score) '+sLineBreak
        +' OVER (PARTITION BY test) AS stddev_results FROM'+sLineBreak
        +'student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+----------------+'+sLineBreak
        +'| name | test | score | stddev_results |'+sLineBreak
        +'+---------+--------+-------+----------------+'+sLineBreak
        +'| Chun | SQL | 75 | 16.9466 |'+sLineBreak
        +'| Chun | Tuning | 73 | 24.1247 |'+sLineBreak
        +'| Esben | SQL | 43 | 16.9466 |'+sLineBreak
        +'| Esben | Tuning | 31 | 24.1247 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 16.9466 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 24.1247 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 16.9466 |'+sLineBreak
        +'+---------+--------+-------+----------------+'
    ),

    (
      Name:         'STDDEV_POP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the population standard deviation of expr (the'+sLineBreak
        +'square root of'+sLineBreak
        +'VAR_POP()). You can also use STD() or'+sLineBreak
        +'STDDEV(), which are equivalent but not standard SQL.'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, STDDEV_POP() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'STDDEV_POP() returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE stats (category VARCHAR(2), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO stats VALUES '+sLineBreak
        +' (''a'',1),(''a'',2),(''a'',3),'+sLineBreak
        +' (''b'',11),(''b'',12),(''b'',20),(''b'',30),(''b'',60);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, STDDEV_POP(x), STDDEV_SAMP(x), VAR_POP(x) '+sLineBreak
        +' FROM stats GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| category | STDDEV_POP(x) | STDDEV_SAMP(x) | VAR_POP(x) |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| a | 0.8165 | 1.0000 | 0.6667 |'+sLineBreak
        +'| b | 18.0400 | 20.1693 | 325.4400 |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, STDDEV_POP(score) '+sLineBreak
        +' OVER (PARTITION BY test) AS stddev_results FROM'+sLineBreak
        +'student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+----------------+'+sLineBreak
        +'| name | test | score | stddev_results |'+sLineBreak
        +'+---------+--------+-------+----------------+'+sLineBreak
        +'| Chun | SQL | 75 | 16.9466 |'+sLineBreak
        +'| Chun | Tuning | 73 | 24.1247 |'+sLineBreak
        +'| Esben | SQL | 43 | 16.9466 |'+sLineBreak
        +'| Esben | Tuning | 31 | 24.1247 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 16.9466 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 24.1247 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 16.9466 |'+sLineBreak
        +'+---------+--------+-------+----------------+'
    ),

    (
      Name:         'STDDEV_SAMP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sample standard deviation of expr (the square'+sLineBreak
        +'root of VAR_SAMP()).'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, STDDEV_SAMP() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'STDDEV_SAMP() returns NULL if there were no matching rows.'
    ),

    (
      Name:         'SUM';
      Declaration:  '([DISTINCT] expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sum of expr. If the return set has no rows,'+sLineBreak
        +'SUM() returns'+sLineBreak
        +'NULL. The DISTINCT keyword can be used to sum only the'+sLineBreak
        +'distinct values'+sLineBreak
        +'of expr.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.0, SUM() can be used as a window function,'+sLineBreak
        +'although not with the DISTINCT specifier.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE sales (sales_value INT);'+sLineBreak
        +'INSERT INTO sales VALUES(10),(20),(20),(40);'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUM(sales_value) FROM sales;'+sLineBreak
        +' '+sLineBreak
        +'+------------------+'+sLineBreak
        +'| SUM(sales_value) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 90 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUM(DISTINCT(sales_value)) FROM sales;'+sLineBreak
        +' '+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| SUM(DISTINCT(sales_value)) |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| 70 |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Commonly, SUM is used with a GROUP BY clause:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE sales (name CHAR(10), month CHAR(10), units'+sLineBreak
        +'INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO sales VALUES '+sLineBreak
        +' (''Chun'', ''Jan'', 75), (''Chun'', ''Feb'', 73),'+sLineBreak
        +' (''Esben'', ''Jan'', 43), (''Esben'', ''Feb'', 31),'+sLineBreak
        +' (''Kaolin'', ''Jan'', 56), (''Kaolin'', ''Feb'', 88),'+sLineBreak
        +' (''Tatiana'', ''Jan'', 87), (''Tatiana'', ''Feb'', 83);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, SUM(units) FROM sales GROUP BY name;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| name | SUM(units) |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +'| Chun | 148 |'+sLineBreak
        +'| Esben | 74 |'+sLineBreak
        +'| Kaolin | 144 |'+sLineBreak
        +'| Tatiana | 170 |'+sLineBreak
        +'+---------+------------+'+sLineBreak
        +' '+sLineBreak
        +'The GROUP BY clause is required when using an aggregate'+sLineBreak
        +'function along with regular column data, otherwise the'+sLineBreak
        +'result will be a mismatch, as in the following common type'+sLineBreak
        +'of mistake:'+sLineBreak
        +' '+sLineBreak
        +'SELECT name,SUM(units) FROM sales'+sLineBreak
        +';'+sLineBreak
        +'+------+------------+'+sLineBreak
        +'| name | SUM(units) |'+sLineBreak
        +'+------+------------+'+sLineBreak
        +'| Chun | 536 |'+sLineBreak
        +'+------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, SUM(score) OVER (PARTITION BY'+sLineBreak
        +'name) AS total_score FROM student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+-------------+'+sLineBreak
        +'| name | test | score | total_score |'+sLineBreak
        +'+---------+--------+-------+-------------+'+sLineBreak
        +'| Chun | SQL | 75 | 148 |'+sLineBreak
        +'| Chun | Tuning | 73 | 148 |'+sLineBreak
        +'| Esben | SQL | 43 | 74 |'+sLineBreak
        +'| Esben | Tuning | 31 | 74 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 144 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 144 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 87 |'+sLineBreak
        +'+---------+--------+-------+-------------+'
    ),

    (
      Name:         'VARIANCE';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the population standard variance of expr. This is an'+sLineBreak
        +'extension to'+sLineBreak
        +'standard SQL. The standard SQL function VAR_POP() can be'+sLineBreak
        +'used'+sLineBreak
        +'instead.'+sLineBreak
        +' '+sLineBreak
        +'Variance is calculated by'+sLineBreak
        +'working out the mean for the set'+sLineBreak
        +'for each number, subtracting the mean and squaring the'+sLineBreak
        +'result'+sLineBreak
        +'calculate the average of the resulting differences'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, VARIANCE() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'VARIANCE() returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE v(i tinyint);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO v VALUES(101),(99);'+sLineBreak
        +' '+sLineBreak
        +'SELECT VARIANCE(i) FROM v;'+sLineBreak
        +' '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| VARIANCE(i) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 1.0000 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO v VALUES(120),(80);'+sLineBreak
        +' '+sLineBreak
        +'SELECT VARIANCE(i) FROM v;'+sLineBreak
        +' '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| VARIANCE(i) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 200.5000 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE stats (category VARCHAR(2), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO stats VALUES '+sLineBreak
        +' (''a'',1),(''a'',2),(''a'',3),'+sLineBreak
        +' (''b'',11),(''b'',12),(''b'',20),(''b'',30),(''b'',60);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, STDDEV_POP(x), STDDEV_SAMP(x), VAR_POP(x) '+sLineBreak
        +' FROM stats GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| category | STDDEV_POP(x) | STDDEV_SAMP(x) | VAR_POP(x) |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| a | 0.8165 | 1.0000 | 0.6667 |'+sLineBreak
        +'| b | 18.0400 | 20.1693 | 325.4400 |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, VAR_POP(score) '+sLineBreak
        +' OVER (PARTITION BY test) AS variance_results FROM'+sLineBreak
        +'student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+------------------+'+sLineBreak
        +'| name | test | score | variance_results |'+sLineBreak
        +'+---------+--------+-------+------------------+'+sLineBreak
        +'| Chun | SQL | 75 | 287.1875 |'+sLineBreak
        +'| Chun | Tuning | 73 | 582.0000 |'+sLineBreak
        +'| Esben | SQL | 43 | 287.1875 |'+sLineBreak
        +'| Esben | Tuning | 31 | 582.0000 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 287.1875 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 582.0000 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 287.1875 |'+sLineBreak
        +'+---------+--------+-------+------------------+'
    ),

    (
      Name:         'VAR_POP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the population standard variance of expr. It'+sLineBreak
        +'considers rows as'+sLineBreak
        +'the whole population, not as a sample, so it has the number'+sLineBreak
        +'of rows as'+sLineBreak
        +'the denominator. You can also use VARIANCE(), which is'+sLineBreak
        +'equivalent but'+sLineBreak
        +'is not standard SQL.'+sLineBreak
        +' '+sLineBreak
        +'Variance is calculated by'+sLineBreak
        +'working out the mean for the set'+sLineBreak
        +'for each number, subtracting the mean and squaring the'+sLineBreak
        +'result'+sLineBreak
        +'calculate the average of the resulting differences'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, VAR_POP() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'VAR_POP() returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE v(i tinyint);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO v VALUES(101),(99);'+sLineBreak
        +' '+sLineBreak
        +'SELECT VAR_POP(i) FROM v;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| VAR_POP(i) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 1.0000 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO v VALUES(120),(80);'+sLineBreak
        +' '+sLineBreak
        +'SELECT VAR_POP(i) FROM v;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| VAR_POP(i) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 200.5000 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE stats (category VARCHAR(2), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO stats VALUES '+sLineBreak
        +' (''a'',1),(''a'',2),(''a'',3),'+sLineBreak
        +' (''b'',11),(''b'',12),(''b'',20),(''b'',30),(''b'',60);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, STDDEV_POP(x), STDDEV_SAMP(x), VAR_POP(x) '+sLineBreak
        +' FROM stats GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| category | STDDEV_POP(x) | STDDEV_SAMP(x) | VAR_POP(x) |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| a | 0.8165 | 1.0000 | 0.6667 |'+sLineBreak
        +'| b | 18.0400 | 20.1693 | 325.4400 |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, VAR_POP(score) '+sLineBreak
        +' OVER (PARTITION BY test) AS variance_results FROM'+sLineBreak
        +'student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+------------------+'+sLineBreak
        +'| name | test | score | variance_results |'+sLineBreak
        +'+---------+--------+-------+------------------+'+sLineBreak
        +'| Chun | SQL | 75 | 287.1875 |'+sLineBreak
        +'| Chun | Tuning | 73 | 582.0000 |'+sLineBreak
        +'| Esben | SQL | 43 | 287.1875 |'+sLineBreak
        +'| Esben | Tuning | 31 | 582.0000 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 287.1875 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 582.0000 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 287.1875 |'+sLineBreak
        +'+---------+--------+-------+------------------+'
    ),

    (
      Name:         'VAR_SAMP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sample variance of expr. That is, the'+sLineBreak
        +'denominator is the number of rows minus one.'+sLineBreak
        +' '+sLineBreak
        +'It is an aggregate function, and so can be used with the'+sLineBreak
        +'GROUP BY clause.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.2, VAR_SAMP() can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'VAR_SAMP() returns NULL if there were no matching rows.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'As an aggregate function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE stats (category VARCHAR(2), x INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO stats VALUES '+sLineBreak
        +' (''a'',1),(''a'',2),(''a'',3),'+sLineBreak
        +' (''b'',11),(''b'',12),(''b'',20),(''b'',30),(''b'',60);'+sLineBreak
        +' '+sLineBreak
        +'SELECT category, STDDEV_POP(x), STDDEV_SAMP(x), VAR_POP(x) '+sLineBreak
        +' FROM stats GROUP BY category;'+sLineBreak
        +' '+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| category | STDDEV_POP(x) | STDDEV_SAMP(x) | VAR_POP(x) |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +'| a | 0.8165 | 1.0000 | 0.6667 |'+sLineBreak
        +'| b | 18.0400 | 20.1693 | 325.4400 |'+sLineBreak
        +'+----------+---------------+----------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE student_test (name CHAR(10), test'+sLineBreak
        +'CHAR(10), score TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student_test VALUES '+sLineBreak
        +' (''Chun'', ''SQL'', 75), (''Chun'', ''Tuning'', 73), '+sLineBreak
        +' (''Esben'', ''SQL'', 43), (''Esben'', ''Tuning'', 31), '+sLineBreak
        +' (''Kaolin'', ''SQL'', 56), (''Kaolin'', ''Tuning'', 88), '+sLineBreak
        +' (''Tatiana'', ''SQL'', 87);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, test, score, VAR_SAMP(score) '+sLineBreak
        +' OVER (PARTITION BY test) AS variance_results FROM'+sLineBreak
        +'student_test;'+sLineBreak
        +' '+sLineBreak
        +'+---------+--------+-------+------------------+'+sLineBreak
        +'| name | test | score | variance_results |'+sLineBreak
        +'+---------+--------+-------+------------------+'+sLineBreak
        +'| Chun | SQL | 75 | 382.9167 |'+sLineBreak
        +'| Chun | Tuning | 73 | 873.0000 |'+sLineBreak
        +'| Esben | SQL | 43 | 382.9167 |'+sLineBreak
        +'| Esben | Tuning | 31 | 873.0000 |'+sLineBreak
        +'| Kaolin | SQL | 56 | 382.9167 |'+sLineBreak
        +'| Kaolin | Tuning | 88 | 873.0000 |'+sLineBreak
        +'| Tatiana | SQL | 87 | 382.9167 |'+sLineBreak
        +'+---------+--------+-------+------------------+'
    ),

    (
      Name:         'GEOMETRYCOLLECTION';
      Declaration:  '(g1,g2,...)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB GeometryCollection. If any argument is not'+sLineBreak
        +'a well-formed WKB representation of a geometry, the return'+sLineBreak
        +'value is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE gis_geometrycollection (g GEOMETRYCOLLECTION);'+sLineBreak
        +'SHOW FIELDS FROM gis_geometrycollection;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO gis_geometrycollection VALUES'+sLineBreak
        +' (GeomCollFromText(''GEOMETRYCOLLECTION(POINT(0 0),'+sLineBreak
        +'LINESTRING(0 0,10 10))'')),'+sLineBreak
        +' (GeometryFromWKB(AsWKB(GeometryCollection(Point(44, 6),'+sLineBreak
        +'LineString(Point(3, 6), Point(7, 9)))))),'+sLineBreak
        +' (GeomFromText(''GeometryCollection()'')),'+sLineBreak
        +' (GeomFromText(''GeometryCollection EMPTY''));'
    ),

    (
      Name:         'LINESTRING';
      Declaration:  '(pt1,pt2,...)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB LineString value from a number of WKB Point'+sLineBreak
        +'arguments. If any argument is not a WKB Point, the return'+sLineBreak
        +'value is'+sLineBreak
        +'NULL. If the number of Point arguments is less than two, the'+sLineBreak
        +'return value is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @ls = ''LineString(1 1,2 2,3 3)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT AsText(EndPoint(GeomFromText(@ls)));'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| AsText(EndPoint(GeomFromText(@ls))) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| POINT(3 3) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE gis_line (g LINESTRING);'+sLineBreak
        +'INSERT INTO gis_line VALUES'+sLineBreak
        +' (LineFromText(''LINESTRING(0 0,0 10,10 0)'')),'+sLineBreak
        +' (LineStringFromText(''LINESTRING(10 10,20 10,20 20,10 20,10'+sLineBreak
        +'10)'')),'+sLineBreak
        +' (LineStringFromWKB(AsWKB(LineString(Point(10, 10),'+sLineBreak
        +'Point(40, 10)))));'
    ),

    (
      Name:         'MULTILINESTRING';
      Declaration:  '(ls1,ls2,...)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB MultiLineString value using WKB LineString'+sLineBreak
        +'arguments. If any argument is not a WKB LineString, the'+sLineBreak
        +'return value is'+sLineBreak
        +'NULL.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE gis_multi_line (g MULTILINESTRING);'+sLineBreak
        +'INSERT INTO gis_multi_line VALUES'+sLineBreak
        +' (MultiLineStringFromText(''MULTILINESTRING((10 48,10 21,10'+sLineBreak
        +'0),(16 0,16 23,16 48))'')),'+sLineBreak
        +' (MLineFromText(''MULTILINESTRING((10 48,10 21,10 0))'')),'+sLineBreak
        +' (MLineFromWKB(AsWKB(MultiLineString(LineString(Point(1, 2),'+sLineBreak
        +'Point(3, 5)), LineString(Point(2, 5),Point(5, 8),Point(21,'+sLineBreak
        +'7))))));'
    ),

    (
      Name:         'MULTIPOINT';
      Declaration:  '(pt1,pt2,...)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB MultiPoint value using WKB Point arguments.'+sLineBreak
        +'If any argument is not a WKB Point, the return value is'+sLineBreak
        +'NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_GEOMFROMTEXT(''MultiPoint( 1 1, 2 2, 5 3, 7 2, 9'+sLineBreak
        +'3, 8 4, 6 6, 6 9, 4 9, 1 5 )'');'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE gis_multi_point (g MULTIPOINT);'+sLineBreak
        +'INSERT INTO gis_multi_point VALUES'+sLineBreak
        +' (MultiPointFromText(''MULTIPOINT(0 0,10 10,10 20,20'+sLineBreak
        +'20)'')),'+sLineBreak
        +' (MPointFromText(''MULTIPOINT(1 1,11 11,11 21,21 21)'')),'+sLineBreak
        +' (MPointFromWKB(AsWKB(MultiPoint(Point(3, 6), Point(4,'+sLineBreak
        +'10)))));'
    ),

    (
      Name:         'MULTIPOLYGON';
      Declaration:  '(poly1,poly2,...)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB MultiPolygon value from a set of WKB'+sLineBreak
        +'Polygon arguments. If any argument is not a WKB Polygon, the'+sLineBreak
        +'return value is NULL.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE gis_multi_polygon (g MULTIPOLYGON);'+sLineBreak
        +'INSERT INTO gis_multi_polygon VALUES'+sLineBreak
        +' (MultiPolygonFromText(''MULTIPOLYGON(((28 26,28 0,84 0,84'+sLineBreak
        +'42,28 26),(52 18,66 23,73 9,48 6,52 18)),((59 18,67 18,67'+sLineBreak
        +'13,59 13,59 18)))'')),'+sLineBreak
        +' (MPolyFromText(''MULTIPOLYGON(((28 26,28 0,84 0,84 42,28'+sLineBreak
        +'26),(52 18,66 23,73 9,48 6,52 18)),((59 18,67 18,67 13,59'+sLineBreak
        +'13,59 18)))'')),'+sLineBreak
        +' (MPolyFromWKB(AsWKB(MultiPolygon(Polygon(LineString(Point(0,'+sLineBreak
        +'3), Point(3, 3), Point(3, 0), Point(0, 3)))))));'
    ),

    (
      Name:         'POINT';
      Declaration:  '(x,y)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB Point using the given coordinates.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_GEOMFROMTEXT(''Point(1 1)'');'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE gis_point (g POINT);'+sLineBreak
        +'INSERT INTO gis_point VALUES'+sLineBreak
        +' (PointFromText(''POINT(10 10)'')),'+sLineBreak
        +' (PointFromText(''POINT(20 10)'')),'+sLineBreak
        +' (PointFromText(''POINT(20 20)'')),'+sLineBreak
        +' (PointFromWKB(AsWKB(PointFromText(''POINT(10 20)''))));'
    ),

    (
      Name:         'POLYGON';
      Declaration:  '(ls1,ls2,...)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB Polygon value from a number of WKB'+sLineBreak
        +'LineString'+sLineBreak
        +'arguments. If any argument does not represent the WKB of a'+sLineBreak
        +'LinearRing (that is,'+sLineBreak
        +'not a closed and simple LineString) the return value is'+sLineBreak
        +'NULL.'+sLineBreak
        +' '+sLineBreak
        +'Note that according to the OpenGIS standard, a POLYGON'+sLineBreak
        +'should have exactly one ExteriorRing and all other rings'+sLineBreak
        +'should lie within that ExteriorRing and thus be the'+sLineBreak
        +'InteriorRings. Practically, however, some systems, including'+sLineBreak
        +'MariaDB''s, permit polygons to have several'+sLineBreak
        +'''ExteriorRings''. In the case of there being multiple,'+sLineBreak
        +'non-overlapping exterior rings ST_NUMINTERIORRINGS() will'+sLineBreak
        +'return 1.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_GEOMFROMTEXT(''POLYGON((1 1,1 5,4 9,6 9,9 3,7'+sLineBreak
        +'2,1 1))'');'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE gis_polygon (g POLYGON);'+sLineBreak
        +'INSERT INTO gis_polygon VALUES'+sLineBreak
        +' (PolygonFromText(''POLYGON((10 10,20 10,20 20,10 20,10'+sLineBreak
        +'10))'')),'+sLineBreak
        +' (PolyFromText(''POLYGON((0 0,50 0,50 50,0 50,0 0), (10'+sLineBreak
        +'10,20 10,20 20,10 20,10 10))'')),'+sLineBreak
        +' (PolyFromWKB(AsWKB(Polygon(LineString(Point(0, 0),'+sLineBreak
        +'Point(30, 0), Point(30, 30), Point(0, 0))))));'+sLineBreak
        +' '+sLineBreak
        +'Non-overlapping ''polygon'':'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_NumInteriorRings(ST_PolyFromText(''POLYGON((0 0,10'+sLineBreak
        +'0,10 10,0 10,0 0),'+sLineBreak
        +' (-1 -1,-5 -1,-5 -5,-1 -5,-1 -1))'')) AS NumInteriorRings;'+sLineBreak
        +' '+sLineBreak
        +'+------------------+'+sLineBreak
        +'| NumInteriorRings |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'ST_BUFFER';
      Declaration:  '(g1,r)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a geometry that represents all points whose distance'+sLineBreak
        +'from geometry g1 is less than or equal to distance, or'+sLineBreak
        +'radius, r.'+sLineBreak
        +' '+sLineBreak
        +'Uses for this function could include creating for example a'+sLineBreak
        +'new geometry representing a buffer zone around an island.'+sLineBreak
        +' '+sLineBreak
        +'BUFFER() is a synonym.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Determining whether a point is within a buffer zone:'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POLYGON((10 10, 10 20, 20 20, 20'+sLineBreak
        +'10, 10 10))'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''POINT(8 8)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_WITHIN(@g2,ST_BUFFER(@g1,5));'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| ST_WITHIN(@g2,ST_BUFFER(@g1,5)) |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_WITHIN(@g2,ST_BUFFER(@g1,1));'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| ST_WITHIN(@g2,ST_BUFFER(@g1,1)) |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------------------+'
    ),

    (
      Name:         'ST_CONVEXHULL';
      Declaration:  '(g)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a geometry, returns a geometry that is the minimum'+sLineBreak
        +'convex geometry enclosing all geometries within the set.'+sLineBreak
        +'Returns NULL if the geometry value is NULL or an empty'+sLineBreak
        +'value.'+sLineBreak
        +' '+sLineBreak
        +'ST_ConvexHull() and ConvexHull() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The ConvexHull of a single point is simply the single point:'+sLineBreak
        +' '+sLineBreak
        +'SET @g = ST_GEOMFROMTEXT(''Point(0 0)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_ASTEXT(ST_CONVEXHULL(@g));'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| ST_ASTEXT(ST_CONVEXHULL(@g)) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| POINT(0 0) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g = ST_GEOMFROMTEXT(''MultiPoint(0 0, 1 2, 2 3)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_ASTEXT(ST_CONVEXHULL(@g));'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| ST_ASTEXT(ST_CONVEXHULL(@g)) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| POLYGON((0 0,1 2,2 3,0 0)) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g = ST_GEOMFROMTEXT(''MultiPoint( 1 1, 2 2, 5 3, 7 2, 9'+sLineBreak
        +'3, 8 4, 6 6, 6 9, 4 9, 1 5 )'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_ASTEXT(ST_CONVEXHULL(@g));'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| ST_ASTEXT(ST_CONVEXHULL(@g)) |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| POLYGON((1 1,1 5,4 9,6 9,9 3,7 2,1 1)) |'+sLineBreak
        +'+----------------------------------------+'
    ),

    (
      Name:         'ST_INTERSECTION';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a geometry that is the intersection, or shared'+sLineBreak
        +'portion, of geometry g1 and geometry g2.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(2 1)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(2 1, 0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASTEXT(ST_INTERSECTION(@g1,@g2));'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| ASTEXT(ST_INTERSECTION(@g1,@g2)) |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| POINT(2 1) |'+sLineBreak
        +'+----------------------------------+'
    ),

    (
      Name:         'ST_POINTONSURFACE';
      Declaration:  '(g)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a geometry, returns a POINT guaranteed to intersect a'+sLineBreak
        +'surface. However, see MDEV-7514.'+sLineBreak
        +' '+sLineBreak
        +'ST_PointOnSurface() and PointOnSurface() are synonyms.'
    ),

    (
      Name:         'ST_SYMDIFFERENCE';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a geometry that represents the portions of geometry'+sLineBreak
        +'g1 and geometry g2 that don''t intersect.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''LINESTRING(10 20, 10 40)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(10 15, 10 25)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASTEXT(ST_SYMDIFFERENCE(@g1,@g2));'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| ASTEXT(ST_SYMDIFFERENCE(@g1,@g2)) |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| MULTILINESTRING((10 15,10 20),(10 25,10 40)) |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GeomFromText(''LINESTRING(10 20, 10 41)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASTEXT(ST_SYMDIFFERENCE(@g1,@g2));'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| ASTEXT(ST_SYMDIFFERENCE(@g1,@g2)) |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| LINESTRING(10 40,10 41) |'+sLineBreak
        +'+-----------------------------------+'
    ),

    (
      Name:         'ST_UNION';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Constructors';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a geometry that is the union of the geometry g1 and'+sLineBreak
        +'geometry g2.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GEOMFROMTEXT(''POINT (0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = GEOMFROMTEXT(''POINT (2 0)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASTEXT(ST_UNION(@g1,@g2));'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| ASTEXT(ST_UNION(@g1,@g2)) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| MULTIPOINT(2 0,0 2) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = GEOMFROMTEXT(''POLYGON((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = GEOMFROMTEXT(''POLYGON((2 2,4 2,4 4,2 4,2 2))'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASTEXT(ST_UNION(@g1,@g2));'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| ASTEXT(ST_UNION(@g1,@g2)) |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| POLYGON((0 0,0 3,2 3,2 4,4 4,4 2,3 2,3 0,0 0)) |'+sLineBreak
        +'+------------------------------------------------+'
    ),

    (
      Name:         'ST_BOUNDARY';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a geometry that is the closure of the combinatorial'+sLineBreak
        +'boundary of the geometry value g.'+sLineBreak
        +' '+sLineBreak
        +'BOUNDARY() is a synonym.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ST_AsText(ST_Boundary(ST_GeomFromText(''LINESTRING(3'+sLineBreak
        +'3,0 0, -3 3)'')));'+sLineBreak
        +'+----------------------------------------------------------------------+'+sLineBreak
        +'| ST_AsText(ST_Boundary(ST_GeomFromText(''LINESTRING(3 3,0'+sLineBreak
        +'0, -3 3)''))) |'+sLineBreak
        +'+----------------------------------------------------------------------+'+sLineBreak
        +'| MULTIPOINT(3 3,-3 3) |'+sLineBreak
        +'+----------------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_Boundary(ST_GeomFromText(''POLYGON((3'+sLineBreak
        +'3,0 0, -3 3, 3 3))'')));'+sLineBreak
        +'+--------------------------------------------------------------------------+'+sLineBreak
        +'| ST_AsText(ST_Boundary(ST_GeomFromText(''POLYGON((3 3,0 0,'+sLineBreak
        +'-3 3, 3 3))''))) |'+sLineBreak
        +'+--------------------------------------------------------------------------+'+sLineBreak
        +'| LINESTRING(3 3,0 0,-3 3,3 3) |'+sLineBreak
        +'+--------------------------------------------------------------------------+'
    ),

    (
      Name:         'ST_DIMENSION';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the inherent dimension of the geometry value g. The'+sLineBreak
        +'result can'+sLineBreak
        +'be'+sLineBreak
        +' '+sLineBreak
        +'Dimension | Definition | '+sLineBreak
        +' '+sLineBreak
        +' -1 | empty geometry | '+sLineBreak
        +' '+sLineBreak
        +' 0 | geometry with no length or area | '+sLineBreak
        +' '+sLineBreak
        +' 1 | geometry with no area but nonzero length | '+sLineBreak
        +' '+sLineBreak
        +' 2 | geometry with nonzero area | '+sLineBreak
        +' '+sLineBreak
        +'ST_Dimension() and Dimension() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT Dimension(GeomFromText(''LineString(1 1,2 2)''));'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| Dimension(GeomFromText(''LineString(1 1,2 2)'')) |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------------------------------------+'
    ),

    (
      Name:         'ST_ENVELOPE';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the Minimum Bounding Rectangle (MBR) for the'+sLineBreak
        +'geometry value g. The result is returned as a Polygon value.'+sLineBreak
        +' '+sLineBreak
        +'The polygon is defined by the corner points of the bounding'+sLineBreak
        +'box:'+sLineBreak
        +' '+sLineBreak
        +'POLYGON((MINX MINY, MAXX MINY, MAXX MAXY, MINX MAXY, MINX'+sLineBreak
        +'MINY))'+sLineBreak
        +' '+sLineBreak
        +'ST_ENVELOPE() and ENVELOPE() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT AsText(ST_ENVELOPE(GeomFromText(''LineString(1 1,4'+sLineBreak
        +'4)'')));'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +'| AsText(ST_ENVELOPE(GeomFromText(''LineString(1 1,4 4)'')))'+sLineBreak
        +'|'+sLineBreak
        +'+----------------------------------------------------------+'+sLineBreak
        +'| POLYGON((1 1,4 1,4 4,1 4,1 1)) |'+sLineBreak
        +'+----------------------------------------------------------+'
    ),

    (
      Name:         'ST_GEOMETRYN';
      Declaration:  '(gc,N)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the N-th geometry in the GeometryCollection gc.'+sLineBreak
        +'Geometries are numbered beginning with 1.'+sLineBreak
        +' '+sLineBreak
        +'ST_GeometryN() and GeometryN() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @gc = ''GeometryCollection(Point(1 1),LineString(12 14,'+sLineBreak
        +'9 11))'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT AsText(GeometryN(GeomFromText(@gc),1));'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| AsText(GeometryN(GeomFromText(@gc),1)) |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| POINT(1 1) |'+sLineBreak
        +'+----------------------------------------+'
    ),

    (
      Name:         'ST_GEOMETRYTYPE';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns as a string the name of the geometry type of which'+sLineBreak
        +'the'+sLineBreak
        +'geometry instance g is a member. The name corresponds to one'+sLineBreak
        +'of the'+sLineBreak
        +'instantiable Geometry subclasses.'+sLineBreak
        +' '+sLineBreak
        +'ST_GeometryType() and GeometryType() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT GeometryType(GeomFromText(''POINT(1 1)''));'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| GeometryType(GeomFromText(''POINT(1 1)'')) |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| POINT |'+sLineBreak
        +'+------------------------------------------+'
    ),

    (
      Name:         'ST_ISCLOSED';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if a given LINESTRING''s start and end points are'+sLineBreak
        +'the same, or 0 if they are not the same. Before MariaDB'+sLineBreak
        +'10.1.5, returns NULL if not given a LINESTRING. After'+sLineBreak
        +'MariaDB 10.1.5, returns -1.'+sLineBreak
        +' '+sLineBreak
        +'ST_IsClosed() and IsClosed() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @ls = ''LineString(0 0, 0 4, 4 4, 0 0)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_ISCLOSED(GEOMFROMTEXT(@ls));'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| ST_ISCLOSED(GEOMFROMTEXT(@ls)) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @ls = ''LineString(0 0, 0 4, 4 4, 0 1)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_ISCLOSED(GEOMFROMTEXT(@ls));'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| ST_ISCLOSED(GEOMFROMTEXT(@ls)) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+--------------------------------+'
    ),

    (
      Name:         'ST_ISEMPTY';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'IsEmpty is a function defined by the OpenGIS specification,'+sLineBreak
        +'but is not fully implemented by MariaDB or MySQL. '+sLineBreak
        +' '+sLineBreak
        +'Since MariaDB and MySQL do not support GIS EMPTY values such'+sLineBreak
        +'as POINT EMPTY, as implemented it simply returns 1 if the'+sLineBreak
        +'geometry value g is invalid, 0 if it is valid, and NULL if'+sLineBreak
        +'the argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +'ST_IsEmpty() and IsEmpty() are synonyms.'
    ),

    (
      Name:         'ST_IsRing';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns true if a given LINESTRING is a ring, that is, both'+sLineBreak
        +'ST_IsClosed and ST_IsSimple. A simple curve does not pass'+sLineBreak
        +'through the same point more than once. However, see'+sLineBreak
        +'MDEV-7510.'+sLineBreak
        +' '+sLineBreak
        +'St_IsRing() and IsRing() are synonyms.'
    ),

    (
      Name:         'ST_IsSimple';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns true if the given Geometry has no anomalous'+sLineBreak
        +'geometric points, false if it does, or NULL if given a NULL'+sLineBreak
        +'value.'+sLineBreak
        +' '+sLineBreak
        +'ST_IsSimple() and IsSimple() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'A POINT is always simple.'+sLineBreak
        +' '+sLineBreak
        +'SET @g = ''Point(1 2)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_ISSIMPLE(GEOMFROMTEXT(@g));'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| ST_ISSIMPLE(GEOMFROMTEXT(@g)) |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------------------------+'
    ),

    (
      Name:         'ST_NUMGEOMETRIES';
      Declaration:  '(gc)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of geometries in the GeometryCollection'+sLineBreak
        +'gc.'+sLineBreak
        +' '+sLineBreak
        +'ST_NumGeometries() and NumGeometries() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @gc = ''GeometryCollection(Point(1 1),LineString(2 2, 3'+sLineBreak
        +'3))'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT NUMGEOMETRIES(GeomFromText(@gc));'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| NUMGEOMETRIES(GeomFromText(@gc)) |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+----------------------------------+'
    ),

    (
      Name:         'ST_RELATE';
      Declaration:  '(g1, g2, i)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns true if Geometry g1 is spatially related to'+sLineBreak
        +'Geometryg2 by testing for intersections between the'+sLineBreak
        +'interior, boundary and exterior of the two geometries as'+sLineBreak
        +'specified by the values in intersection matrix pattern i.'
    ),

    (
      Name:         'ST_SRID';
      Declaration:  '(g)';
      Category:     'Geometry Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns an integer indicating the Spatial Reference System'+sLineBreak
        +'ID for the'+sLineBreak
        +'geometry value g.'+sLineBreak
        +' '+sLineBreak
        +'In MariaDB, the SRID value is just an integer associated'+sLineBreak
        +'with the'+sLineBreak
        +'geometry value. All calculations are done assuming Euclidean'+sLineBreak
        +'(planar)'+sLineBreak
        +'geometry.'+sLineBreak
        +' '+sLineBreak
        +'ST_SRID() and SRID() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SRID(GeomFromText(''LineString(1 1,2 2)'',101));'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| SRID(GeomFromText(''LineString(1 1,2 2)'',101)) |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| 101 |'+sLineBreak
        +'+-----------------------------------------------+'
    ),

    (
      Name:         'CONTAINS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether a geometry g1 completely'+sLineBreak
        +'contains geometry g2. CONTAINS() is based on the original'+sLineBreak
        +'MySQL implementation and uses object bounding rectangles,'+sLineBreak
        +'while ST_CONTAINS() uses object shapes. '+sLineBreak
        +' '+sLineBreak
        +'This tests the opposite relationship to Within().'
    ),

    (
      Name:         'CROSSES';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if g1 spatially crosses g2. Returns NULL if g1 is'+sLineBreak
        +'a Polygon or a MultiPolygon, or if g2 is a'+sLineBreak
        +'Point or a MultiPoint. Otherwise, returns 0.'+sLineBreak
        +' '+sLineBreak
        +'The term spatially crosses denotes a spatial relation'+sLineBreak
        +'between two'+sLineBreak
        +'given geometries that has the following properties:'+sLineBreak
        +'The two geometries intersect'+sLineBreak
        +'Their intersection results in a geometry that has a'+sLineBreak
        +'dimension that is one'+sLineBreak
        +' less than the maximum dimension of the two given geometries'+sLineBreak
        +'Their intersection is not equal to either of the two given'+sLineBreak
        +'geometries'+sLineBreak
        +' '+sLineBreak
        +'CROSSES() is based on the original MySQL implementation, and'+sLineBreak
        +'uses object bounding rectangles, while ST_CROSSES() uses'+sLineBreak
        +'object shapes.'
    ),

    (
      Name:         'DISJOINT';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 is spatially disjoint'+sLineBreak
        +'from'+sLineBreak
        +'(does not intersect) g2.'+sLineBreak
        +' '+sLineBreak
        +'DISJOINT() tests the opposite relationship to INTERSECTS().'+sLineBreak
        +' '+sLineBreak
        +'DISJOINT() is based on the original MySQL implementation and'+sLineBreak
        +'uses object bounding rectangles, while ST_DISJOINT() uses'+sLineBreak
        +'object shapes.'
    ),

    (
      Name:         'EQUALS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 is spatially equal to'+sLineBreak
        +'g2.'+sLineBreak
        +' '+sLineBreak
        +'EQUALS() is based on the original MySQL implementation and'+sLineBreak
        +'uses object bounding rectangles, while ST_EQUALS() uses'+sLineBreak
        +'object shapes.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.2.3, MBREQUALS is a synonym for Equals.'
    ),

    (
      Name:         'INTERSECTS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether geometry g1 spatially'+sLineBreak
        +'intersects geometry g2.'+sLineBreak
        +' '+sLineBreak
        +'INTERSECTS() is based on the original MySQL implementation'+sLineBreak
        +'and uses object bounding rectangles, while ST_INTERSECTS()'+sLineBreak
        +'uses object shapes.'+sLineBreak
        +' '+sLineBreak
        +'INTERSECTS() tests the opposite relationship to DISJOINT().'
    ),

    (
      Name:         'OVERLAPS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 spatially overlaps g2.'+sLineBreak
        +'The term spatially overlaps is used if two geometries'+sLineBreak
        +'intersect and their'+sLineBreak
        +'intersection results in a geometry of the same dimension but'+sLineBreak
        +'not equal to'+sLineBreak
        +'either of the given geometries.'+sLineBreak
        +' '+sLineBreak
        +'OVERLAPS() is based on the original MySQL implementation and'+sLineBreak
        +'uses object bounding rectangles, while ST_OVERLAPS() uses'+sLineBreak
        +'object shapes.'
    ),

    (
      Name:         'ST_CONTAINS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether a geometry g1 completely'+sLineBreak
        +'contains geometry g2.'+sLineBreak
        +' '+sLineBreak
        +'ST_CONTAINS() uses object shapes, while CONTAINS(), based on'+sLineBreak
        +'the original MySQL implementation, uses object bounding'+sLineBreak
        +'rectangles.'+sLineBreak
        +' '+sLineBreak
        +'ST_CONTAINS tests the opposite relationship to ST_WITHIN().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POLYGON((175 150, 20 40, 50 60,'+sLineBreak
        +'125 100, 175 150))'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''POINT(174 149)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_CONTAINS(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| ST_CONTAINS(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''POINT(175 151)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_CONTAINS(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| ST_CONTAINS(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'
    ),

    (
      Name:         'ST_CROSSES';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if geometry g1 spatially crosses geometry g2.'+sLineBreak
        +'Returns NULL if g1 is a Polygon or a MultiPolygon, or if g2'+sLineBreak
        +'is a'+sLineBreak
        +'Point or a MultiPoint. Otherwise, returns 0.'+sLineBreak
        +' '+sLineBreak
        +'The term spatially crosses denotes a spatial relation'+sLineBreak
        +'between two'+sLineBreak
        +'given geometries that has the following properties:'+sLineBreak
        +'The two geometries intersect'+sLineBreak
        +'Their intersection results in a geometry that has a'+sLineBreak
        +'dimension that is one'+sLineBreak
        +' less than the maximum dimension of the two given geometries'+sLineBreak
        +'Their intersection is not equal to either of the two given'+sLineBreak
        +'geometries'+sLineBreak
        +' '+sLineBreak
        +'ST_CROSSES() uses object shapes, while CROSSES(), based on'+sLineBreak
        +'the original MySQL implementation, uses object bounding'+sLineBreak
        +'rectangles.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''LINESTRING(174 149, 176 151)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''POLYGON((175 150, 20 40, 50 60,'+sLineBreak
        +'125 100, 175 150))'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_CROSSES(@g1,@g2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| ST_CROSSES(@g1,@g2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''LINESTRING(176 149, 176 151)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_CROSSES(@g1,@g2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| ST_CROSSES(@g1,@g2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'ST_DIFFERENCE';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a geometry representing the point set difference of'+sLineBreak
        +'the given geometry values.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = POINT(10,10), @g2 = POINT(20,20);'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_Difference(@g1, @g2));'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| ST_AsText(ST_Difference(@g1, @g2)) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| POINT(10 10) |'+sLineBreak
        +'+------------------------------------+'
    ),

    (
      Name:         'ST_DISJOINT';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether geometry g1 is spatially'+sLineBreak
        +'disjoint from'+sLineBreak
        +'(does not intersect with) geometry g2.'+sLineBreak
        +' '+sLineBreak
        +'ST_DISJOINT() uses object shapes, while DISJOINT(), based on'+sLineBreak
        +'the original MySQL implementation, uses object bounding'+sLineBreak
        +'rectangles.'+sLineBreak
        +' '+sLineBreak
        +'ST_DISJOINT() tests the opposite relationship to'+sLineBreak
        +'ST_INTERSECTS().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(0 0)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(2 0, 0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_DISJOINT(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| ST_DISJOINT(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(0 0, 0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_DISJOINT(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| ST_DISJOINT(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'
    ),

    (
      Name:         'ST_DISTANCE';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the distance between two geometries, or null if not'+sLineBreak
        +'given valid inputs.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_Distance(POINT(1,2),POINT(2,2));'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| ST_Distance(POINT(1,2),POINT(2,2)) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------------------------+'
    ),

    (
      Name:         'ST_EQUALS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether geometry g1 is spatially'+sLineBreak
        +'equal to geometry g2.'+sLineBreak
        +' '+sLineBreak
        +'ST_EQUALS() uses object shapes, while EQUALS(), based on the'+sLineBreak
        +'original MySQL implementation, uses object bounding'+sLineBreak
        +'rectangles.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''LINESTRING(174 149, 176 151)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(176 151, 174 149)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_EQUALS(@g1,@g2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| ST_EQUALS(@g1,@g2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(2 0)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_EQUALS(@g1,@g2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| ST_EQUALS(@g1,@g2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'ST_INTERSECTS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether geometry g1 spatially'+sLineBreak
        +'intersects geometry g2.'+sLineBreak
        +' '+sLineBreak
        +'ST_INTERSECTS() uses object shapes, while INTERSECTS(),'+sLineBreak
        +'based on the original MySQL implementation, uses object'+sLineBreak
        +'bounding rectangles.'+sLineBreak
        +' '+sLineBreak
        +'ST_INTERSECTS() tests the opposite relationship to'+sLineBreak
        +'ST_DISJOINT().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(0 0)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(0 0, 0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_INTERSECTS(@g1,@g2);'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| ST_INTERSECTS(@g1,@g2) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(2 0, 0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_INTERSECTS(@g1,@g2);'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| ST_INTERSECTS(@g1,@g2) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------------+'
    ),

    (
      Name:         'ST_LENGTH';
      Declaration:  '(ls)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns as a double-precision number the length of the'+sLineBreak
        +'LineString value ls in its associated spatial reference.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @ls = ''LineString(1 1,2 2,3 3)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_LENGTH(ST_GeomFromText(@ls));'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| ST_LENGTH(ST_GeomFromText(@ls)) |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| 2.82842712474619 |'+sLineBreak
        +'+---------------------------------+'
    ),

    (
      Name:         'ST_OVERLAPS';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether geometry g1 spatially'+sLineBreak
        +'overlaps geometry g2.'+sLineBreak
        +' '+sLineBreak
        +'The term spatially overlaps is used if two geometries'+sLineBreak
        +'intersect and their'+sLineBreak
        +'intersection results in a geometry of the same dimension but'+sLineBreak
        +'not equal to'+sLineBreak
        +'either of the given geometries.'+sLineBreak
        +' '+sLineBreak
        +'ST_OVERLAPS() uses object shapes, while OVERLAPS(), based on'+sLineBreak
        +'the original MySQL implementation, uses object bounding'+sLineBreak
        +'rectangles.'
    ),

    (
      Name:         'ST_TOUCHES';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether geometry g1 spatially'+sLineBreak
        +'touches geometry g2. Two geometries spatially touch if the'+sLineBreak
        +'interiors of the geometries do not intersect,'+sLineBreak
        +'but the boundary of one of the geometries intersects either'+sLineBreak
        +'the boundary or the'+sLineBreak
        +'interior of the other.'+sLineBreak
        +' '+sLineBreak
        +'ST_TOUCHES() uses object shapes, while TOUCHES(), based on'+sLineBreak
        +'the original MySQL implementation, uses object bounding'+sLineBreak
        +'rectangles.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(2 0)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''LINESTRING(2 0, 0 2)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_TOUCHES(@g1,@g2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| ST_TOUCHES(@g1,@g2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(2 1)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_TOUCHES(@g1,@g2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| ST_TOUCHES(@g1,@g2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'ST_WITHIN';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether geometry g1 is spatially'+sLineBreak
        +'within geometry g2.'+sLineBreak
        +' '+sLineBreak
        +'This tests the opposite relationship as ST_CONTAINS().'+sLineBreak
        +' '+sLineBreak
        +'ST_WITHIN() uses object shapes, while WITHIN(), based on the'+sLineBreak
        +'original MySQL implementation, uses object bounding'+sLineBreak
        +'rectangles.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(174 149)'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = ST_GEOMFROMTEXT(''POLYGON((175 150, 20 40, 50 60,'+sLineBreak
        +'125 100, 175 150))'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_WITHIN(@g1,@g2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| ST_WITHIN(@g1,@g2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = ST_GEOMFROMTEXT(''POINT(176 151)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_WITHIN(@g1,@g2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| ST_WITHIN(@g1,@g2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'TOUCHES';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 spatially touches g2.'+sLineBreak
        +'Two'+sLineBreak
        +'geometries spatially touch if the interiors of the'+sLineBreak
        +'geometries do not intersect,'+sLineBreak
        +'but the boundary of one of the geometries intersects either'+sLineBreak
        +'the boundary or the'+sLineBreak
        +'interior of the other.'+sLineBreak
        +' '+sLineBreak
        +'TOUCHES() is based on the original MySQL implementation and'+sLineBreak
        +'uses object bounding rectangles, while ST_TOUCHES() uses'+sLineBreak
        +'object shapes.'
    ),

    (
      Name:         'WITHIN';
      Declaration:  '(g1,g2)';
      Category:     'Geometry Relations';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 is spatially within'+sLineBreak
        +'g2.'+sLineBreak
        +'This tests the opposite relationship as Contains().'+sLineBreak
        +' '+sLineBreak
        +'WITHIN() is based on the original MySQL implementation, and'+sLineBreak
        +'uses object bounding rectangles, while ST_WITHIN() uses'+sLineBreak
        +'object shapes.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GEOMFROMTEXT(''POINT(174 149)'');'+sLineBreak
        +'SET @g2 = GEOMFROMTEXT(''POINT(176 151)'');'+sLineBreak
        +'SET @g3 = GEOMFROMTEXT(''POLYGON((175 150, 20 40, 50 60, 125'+sLineBreak
        +'100, 175 150))'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT within(@g1,@g3);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| within(@g1,@g3) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT within(@g2,@g3);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| within(@g2,@g3) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------+'
    ),

    (
      Name:         'BENCHMARK';
      Declaration:  '(count,expr)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The BENCHMARK() function executes the expression expr'+sLineBreak
        +'repeatedly count'+sLineBreak
        +'times. It may be used to time how quickly MariaDB processes'+sLineBreak
        +'the'+sLineBreak
        +'expression. The result value is always 0. The intended use'+sLineBreak
        +'is from'+sLineBreak
        +'within the mysql client, which reports query execution'+sLineBreak
        +'times.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT BENCHMARK(1000000,ENCODE(''hello'',''goodbye''));'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| BENCHMARK(1000000,ENCODE(''hello'',''goodbye'')) |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'1 row in set (0.21 sec)'
    ),

    (
      Name:         'BINLOG_GTID_POS';
      Declaration:  '(binlog_filename,binlog_offset)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The BINLOG_GTID_POS() function takes as input an old-style'+sLineBreak
        +'binary log position in the form of a file name and a file'+sLineBreak
        +'offset. It looks up the position in the current binlog, and'+sLineBreak
        +'returns a string representation of the corresponding GTID'+sLineBreak
        +'position. If the position is not found in the current'+sLineBreak
        +'binlog, NULL is returned.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT BINLOG_GTID_POS("master-bin.000001", 600);'
    ),

    (
      Name:         'CHARSET';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the character set of the string argument. If str is'+sLineBreak
        +'not a string, it is considered as a binary string (so the'+sLineBreak
        +'function returns ''binary''). This applies to NULL, too. The'+sLineBreak
        +'return value is a string in the utf8 character set.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CHARSET(''abc'');'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| CHARSET(''abc'') |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| latin1 |'+sLineBreak
        +'+----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CHARSET(CONVERT(''abc'' USING utf8));'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| CHARSET(CONVERT(''abc'' USING utf8)) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| utf8 |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CHARSET(USER());'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| CHARSET(USER()) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| utf8 |'+sLineBreak
        +'+-----------------+'
    ),

    (
      Name:         'COERCIBILITY';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the collation coercibility value of the string'+sLineBreak
        +'argument. Coercibility defines what will be converted to'+sLineBreak
        +'what in case of collation conflict, with an expression with'+sLineBreak
        +'higher coercibility being converted to the collation of an'+sLineBreak
        +'expression with lower coercibility.'+sLineBreak
        +' '+sLineBreak
        +'Coercibility | Description | Example | '+sLineBreak
        +' '+sLineBreak
        +'0 | Explicit | Value using a COLLATE clause | '+sLineBreak
        +' '+sLineBreak
        +'1 | No collation | Concatenated strings using different'+sLineBreak
        +'collations | '+sLineBreak
        +' '+sLineBreak
        +'2 | Implicit | Column value | '+sLineBreak
        +' '+sLineBreak
        +'3 | Constant | USER() return value | '+sLineBreak
        +' '+sLineBreak
        +'4 | Coercible | Literal string | '+sLineBreak
        +' '+sLineBreak
        +'5 | Ignorable | NULL or derived from NULL | '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT COERCIBILITY(''abc'' COLLATE latin1_swedish_ci);'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| COERCIBILITY(''abc'' COLLATE latin1_swedish_ci) |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT COERCIBILITY(USER());'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| COERCIBILITY(USER()) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT COERCIBILITY(''abc'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| COERCIBILITY(''abc'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'COLLATION';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the collation of the string argument. If str is not'+sLineBreak
        +'a string, it is considered as a binary string (so the'+sLineBreak
        +'function returns ''binary''). This applies to NULL, too. The'+sLineBreak
        +'return value is a string in the utf8 character set.'+sLineBreak
        +' '+sLineBreak
        +'See Character Sets and Collations.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT COLLATION(''abc'');'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| COLLATION(''abc'') |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| latin1_swedish_ci |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT COLLATION(_utf8''abc'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| COLLATION(_utf8''abc'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| utf8_general_ci |'+sLineBreak
        +'+-----------------------+'
    ),

    (
      Name:         'CONNECTION_ID';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the connection ID (thread ID) for the connection.'+sLineBreak
        +'Every'+sLineBreak
        +'thread (including events) has an ID that is unique among the'+sLineBreak
        +'set of currently'+sLineBreak
        +'connected clients.'+sLineBreak
        +' '+sLineBreak
        +'Until MariaDB 10.3.1, returns MYSQL_TYPE_LONGLONG, or'+sLineBreak
        +'bigint(10), in all cases. From MariaDB 10.3.1, returns'+sLineBreak
        +'MYSQL_TYPE_LONG, or int(10), when the result would fit'+sLineBreak
        +'within 32-bits.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CONNECTION_ID();'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| CONNECTION_ID() |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+-----------------+'
    ),

    (
      Name:         'DATABASE';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the default (current) database name as a string in'+sLineBreak
        +'the utf8 character set. If there is no default database,'+sLineBreak
        +'DATABASE() returns NULL. Within a stored routine, the'+sLineBreak
        +'default database is the database that the routine is'+sLineBreak
        +'associated with, which is not necessarily the same as the'+sLineBreak
        +'database that is the default in the calling context.'+sLineBreak
        +' '+sLineBreak
        +'SCHEMA() is a synonym for DATABASE().'+sLineBreak
        +' '+sLineBreak
        +'To select a default database, the USE statement can be run.'+sLineBreak
        +'Another way to set the default database is specifying its'+sLineBreak
        +'name at mysql command line client startup.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DATABASE();'+sLineBreak
        +'+------------+'+sLineBreak
        +'| DATABASE() |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'USE test;'+sLineBreak
        +' '+sLineBreak
        +'Database changed'+sLineBreak
        +' '+sLineBreak
        +'SELECT DATABASE();'+sLineBreak
        +'+------------+'+sLineBreak
        +'| DATABASE() |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| test |'+sLineBreak
        +'+------------+'
    ),

    (
      Name:         'DECODE_HISTOGRAM';
      Declaration:  '(hist_type,histogram)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string of comma separated numeric values'+sLineBreak
        +'corresponding to a probability distribution represented by'+sLineBreak
        +'the histogram of type hist_type (SINGLE_PREC_HB or'+sLineBreak
        +'DOUBLE_PREC_HB). The hist_type and histogram would be'+sLineBreak
        +'commonly used from the mysql.column_stats table.'+sLineBreak
        +' '+sLineBreak
        +'See Histogram Based Statistics for details.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE origin ('+sLineBreak
        +' i INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,'+sLineBreak
        +' v INT UNSIGNED NOT NULL'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO origin(v) VALUES '+sLineBreak
        +' (1),(2),(3),(4),(5),(10),(20),'+sLineBreak
        +' (30),(40),(50),(60),(70),(80),'+sLineBreak
        +' (90),(100),(200),(400),(800);'+sLineBreak
        +' '+sLineBreak
        +'SET histogram_size=10,histogram_type=SINGLE_PREC_HB;'+sLineBreak
        +' '+sLineBreak
        +'ANALYZE TABLE origin PERSISTENT FOR ALL;'+sLineBreak
        +' '+sLineBreak
        +'+-------------+---------+----------+-----------------------------------------+'+sLineBreak
        +'| Table | Op | Msg_type | Msg_text |'+sLineBreak
        +'+-------------+---------+----------+-----------------------------------------+'+sLineBreak
        +'| test.origin | analyze | status | Engine-independent'+sLineBreak
        +'statistics collected |'+sLineBreak
        +'| test.origin | analyze | status | OK |'+sLineBreak
        +'+-------------+---------+----------+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT db_name,table_name,column_name,hist_type,'+sLineBreak
        +' hex(histogram),decode_histogram(hist_type,histogram) '+sLineBreak
        +' FROM mysql.column_stats WHERE db_name=''test'' and'+sLineBreak
        +'table_name=''origin'';'+sLineBreak
        +' '+sLineBreak
        +'+---------+------------+-------------+----------------+----------------------+-------------------------------------------------------------------+'+sLineBreak
        +'| db_name | table_name | column_name | hist_type |'+sLineBreak
        +'hex(histogram) | decode_histogram(hist_type,histogram) |'+sLineBreak
        +'+---------+------------+-------------+----------------+----------------------+-------------------------------------------------------------------+'+sLineBreak
        +'| test | origin | i | SINGLE_PREC_HB | 0F2D3C5A7887A5C3D2F0'+sLineBreak
        +'|'+sLineBreak
        +'0.059,0.118,0.059,0.118,0.118,0.059,0.118,0.118,0.059,0.118,0.059'+sLineBreak
        +'|'+sLineBreak
        +'| test | origin | v | SINGLE_PREC_HB | 000001060C0F161C1F7F'+sLineBreak
        +'|'+sLineBreak
        +'0.000,0.000,0.004,0.020,0.024,0.012,0.027,0.024,0.012,0.376,0.502'+sLineBreak
        +'|'+sLineBreak
        +'+---------+------------+-------------+----------------+----------------------+-------------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET histogram_size=20,histogram_type=DOUBLE_PREC_HB;'+sLineBreak
        +' '+sLineBreak
        +'ANALYZE TABLE origin PERSISTENT FOR ALL;'+sLineBreak
        +' '+sLineBreak
        +'+-------------+---------+----------+-----------------------------------------+'+sLineBreak
        +'| Table | Op | Msg_type | Msg_text |'+sLineBreak
        +'+-------------+---------+----------+-----------------------------------------+'+sLineBreak
        +'| test.origin | analyze | status | Engine-independent'+sLineBreak
        +'statistics collected |'+sLineBreak
        +'| test.origin | analyze | status | OK |'+sLineBreak
        +'+-------------+---------+----------+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT db_name,table_name,column_name,'+sLineBreak
        +' hist_type,hex(histogram),decode_histogram(hist_type,histogram)'+sLineBreak
        +''+sLineBreak
        +' FROM mysql.column_stats WHERE db_name=''test'' and'+sLineBreak
        +'table_name=''origin'';'+sLineBreak
        +' '+sLineBreak
        +'+---------+------------+-------------+----------------+------------------------------------------+-----------------------------------------------------------------------------------------+'+sLineBreak
        +'| db_name | table_name | column_name | hist_type |'+sLineBreak
        +'hex(histogram) | decode_histogram(hist_type,histogram) |'+sLineBreak
        +'+---------+------------+-------------+----------------+------------------------------------------+-----------------------------------------------------------------------------------------+'+sLineBreak
        +'| test | origin | i | DOUBLE_PREC_HB |'+sLineBreak
        +'0F0F2D2D3C3C5A5A78788787A5A5C3C3D2D2F0F0 |'+sLineBreak
        +'0.05882,0.11765,0.05882,0.11765,0.11765,0.05882,0.11765,0.11765,0.05882,0.11765,0.05882'+sLineBreak
        +'|'+sLineBreak
        +'| test | origin | v | DOUBLE_PREC_HB |'+sLineBreak
        +'5200F600480116067E0CB30F1B16831CB81FD67F |'+sLineBreak
        +'0.00125,0.00250,0.00125,0.01877,0.02502,0.01253,0.02502,0.02502,0.01253,0.37546,0.50063'+sLineBreak
        +'|'
    ),

    (
      Name:         'DEFAULT';
      Declaration:  '(col_name)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the default value for a table column. If the column'+sLineBreak
        +'has no default value, NULL is returned.'+sLineBreak
        +'For integer columns using AUTO_INCREMENT, 0 is returned.'+sLineBreak
        +' '+sLineBreak
        +'When using DEFAULT as a value to set in an INSERT or UPDATE'+sLineBreak
        +'statement, you can use the bare keyword DEFAULT without the'+sLineBreak
        +'parentheses and argument to'+sLineBreak
        +'refer to the column in context. You can only use DEFAULT as'+sLineBreak
        +'a bare keyword if you are using it'+sLineBreak
        +'alone without a surrounding expression or function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Select only non-default values for a column:'+sLineBreak
        +' '+sLineBreak
        +'SELECT i FROM t WHERE i != DEFAULT(i);'+sLineBreak
        +' '+sLineBreak
        +'Update values to be one greater than the default value:'+sLineBreak
        +' '+sLineBreak
        +'UPDATE t SET i = DEFAULT(i)+1 WHERE i'
    ),

    (
      Name:         'FOUND_ROWS';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'A SELECT statement may include a LIMIT clause to restrict'+sLineBreak
        +'the number'+sLineBreak
        +'of rows the server returns to the client. In some cases, it'+sLineBreak
        +'is'+sLineBreak
        +'desirable to know how many rows the statement would have'+sLineBreak
        +'returned'+sLineBreak
        +'without the LIMIT, but without running the statement again.'+sLineBreak
        +'To obtain'+sLineBreak
        +'this row count, include a SQL_CALC_FOUND_ROWS option in the'+sLineBreak
        +'SELECT'+sLineBreak
        +'statement, and then invoke FOUND_ROWS() afterwards.'+sLineBreak
        +' '+sLineBreak
        +'You can also use FOUND_ROWS() to obtain the number of rows'+sLineBreak
        +'returned by a SELECT which does not contain a LIMIT clause.'+sLineBreak
        +'In this case you don''t need to use the SQL_CALC_FOUND_ROWS'+sLineBreak
        +'option. This can be useful for example in a stored'+sLineBreak
        +'procedure.'+sLineBreak
        +' '+sLineBreak
        +'Also, this function works with some other statements which'+sLineBreak
        +'return a resultset, including SHOW, DESC and HELP. For'+sLineBreak
        +'DELETE ... RETURNING you should use ROW_COUNT(). It also'+sLineBreak
        +'works as a prepared statement, or after executing a prepared'+sLineBreak
        +'statement.'+sLineBreak
        +' '+sLineBreak
        +'Statements which don''t return any results don''t affect'+sLineBreak
        +'FOUND_ROWS() - the previous value will still be returned.'+sLineBreak
        +' '+sLineBreak
        +'Warning: When used after a CALL statement, this function'+sLineBreak
        +'returns the number of rows selected by the last query in the'+sLineBreak
        +'procedure, not by the whole procedure.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the FOUND_ROWS() function are not safe for'+sLineBreak
        +'replication.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SHOW ENGINES;'+sLineBreak
        +' '+sLineBreak
        +'+--------------------+---------+----------------------------------------------------------------+--------------+------+------------+'+sLineBreak
        +'| Engine | Support | Comment | Transactions | XA |'+sLineBreak
        +'Savepoints |'+sLineBreak
        +'+--------------------+---------+----------------------------------------------------------------+--------------+------+------------+'+sLineBreak
        +'| InnoDB | DEFAULT | Supports transactions, row-level'+sLineBreak
        +'locking, and foreign keys | YES | YES | YES |'+sLineBreak
        +'...'+sLineBreak
        +'| SPHINX | YES | Sphinx storage engine | NO | NO | NO |'+sLineBreak
        +'+--------------------+---------+----------------------------------------------------------------+--------------+------+------------+'+sLineBreak
        +'11 rows in set (0.01 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT FOUND_ROWS();'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| FOUND_ROWS() |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 11 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SQL_CALC_FOUND_ROWS * FROM tbl_name WHERE id > 100'+sLineBreak
        +'LIMIT 10;'+sLineBreak
        +' '+sLineBreak
        +'SELECT FOUND_ROWS();'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| FOUND_ROWS() |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 23 |'+sLineBreak
        +'+--------------+'
    ),

    (
      Name:         'LAST_INSERT_ID';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LAST_INSERT_ID() (no arguments) returns'+sLineBreak
        +'the first automatically generated value successfully'+sLineBreak
        +'inserted for an'+sLineBreak
        +'AUTO_INCREMENT column as a result of the most recently'+sLineBreak
        +'executed INSERT'+sLineBreak
        +'statement. The value of LAST_INSERT_ID() remains unchanged'+sLineBreak
        +'if no rows'+sLineBreak
        +'are successfully inserted.'+sLineBreak
        +' '+sLineBreak
        +'If one gives an argument to LAST_INSERT_ID(), then it will'+sLineBreak
        +'return the value of the expression and'+sLineBreak
        +'the next call to LAST_INSERT_ID() will return the same'+sLineBreak
        +'value. The value will also be sent to the client'+sLineBreak
        +'and can be accessed by the mysql_insert_id function.'+sLineBreak
        +' '+sLineBreak
        +'For example, after inserting a row that generates an'+sLineBreak
        +'AUTO_INCREMENT'+sLineBreak
        +'value, you can get the value like this:'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 9 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'You can also use LAST_INSERT_ID() to delete the last'+sLineBreak
        +'inserted row:'+sLineBreak
        +' '+sLineBreak
        +'DELETE FROM product WHERE id = LAST_INSERT_ID();'+sLineBreak
        +' '+sLineBreak
        +'If no rows were successfully inserted, LAST_INSERT_ID()'+sLineBreak
        +'returns 0.'+sLineBreak
        +' '+sLineBreak
        +'The value of LAST_INSERT_ID() will be consistent across all'+sLineBreak
        +'versions'+sLineBreak
        +'if all rows in the INSERT or UPDATE statement were'+sLineBreak
        +'successful.'+sLineBreak
        +' '+sLineBreak
        +'The currently executing statement does not affect the value'+sLineBreak
        +'of'+sLineBreak
        +'LAST_INSERT_ID(). Suppose that you generate an'+sLineBreak
        +'AUTO_INCREMENT value'+sLineBreak
        +'with one statement, and then refer to LAST_INSERT_ID() in a'+sLineBreak
        +'multiple-row INSERT statement that inserts rows into a table'+sLineBreak
        +'with its'+sLineBreak
        +'own AUTO_INCREMENT column. The value of LAST_INSERT_ID()'+sLineBreak
        +'will remain'+sLineBreak
        +'stable in the second statement; its value for the second and'+sLineBreak
        +'later'+sLineBreak
        +'rows is not affected by the earlier row insertions.'+sLineBreak
        +'(However, if you'+sLineBreak
        +'mix references to LAST_INSERT_ID() and LAST_INSERT_ID(expr),'+sLineBreak
        +'the'+sLineBreak
        +'effect is undefined.)'+sLineBreak
        +' '+sLineBreak
        +'If the previous statement returned an error, the value of'+sLineBreak
        +'LAST_INSERT_ID() is undefined. For transactional tables, if'+sLineBreak
        +'the'+sLineBreak
        +'statement is rolled back due to an error, the value of'+sLineBreak
        +'LAST_INSERT_ID() is left undefined. For manual ROLLBACK, the'+sLineBreak
        +'value of'+sLineBreak
        +'LAST_INSERT_ID() is not restored to that before the'+sLineBreak
        +'transaction; it'+sLineBreak
        +'remains as it was at the point of the ROLLBACK.'+sLineBreak
        +' '+sLineBreak
        +'Within the body of a stored routine (procedure or function)'+sLineBreak
        +'or a'+sLineBreak
        +'trigger, the value of LAST_INSERT_ID() changes the same way'+sLineBreak
        +'as for'+sLineBreak
        +'statements executed outside the body of these kinds of'+sLineBreak
        +'objects. The'+sLineBreak
        +'effect of a stored routine or trigger upon the value of'+sLineBreak
        +'LAST_INSERT_ID() that is seen by following statements'+sLineBreak
        +'depends on the'+sLineBreak
        +'kind of routine:'+sLineBreak
        +'If a stored procedure executes statements that change the'+sLineBreak
        +'value of LAST_INSERT_ID(), the new value will be seen by'+sLineBreak
        +'statements that follow the procedure call.'+sLineBreak
        +' '+sLineBreak
        +'For stored functions and triggers that change the value, the'+sLineBreak
        +'value is restored when the function or trigger ends, so'+sLineBreak
        +'following statements will not see a changed value.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t ('+sLineBreak
        +' id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY, '+sLineBreak
        +' f VARCHAR(1)) '+sLineBreak
        +'ENGINE = InnoDB;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''a'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''b'');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''c'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''d''),(''e'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+'+sLineBreak
        +'| id | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +'| 1 | a |'+sLineBreak
        +'| 2 | b |'+sLineBreak
        +'| 3 | c |'+sLineBreak
        +'| 4 | d |'+sLineBreak
        +'| 5 | e |'+sLineBreak
        +'+----+------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID(12);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| LAST_INSERT_ID(12) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 12 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 12 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''f'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 6 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+'+sLineBreak
        +'| id | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +'| 1 | a |'+sLineBreak
        +'| 2 | b |'+sLineBreak
        +'| 3 | c |'+sLineBreak
        +'| 4 | d |'+sLineBreak
        +'| 5 | e |'+sLineBreak
        +'| 6 | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID(12);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| LAST_INSERT_ID(12) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 12 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''g'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+'+sLineBreak
        +'| id | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +'| 1 | a |'+sLineBreak
        +'| 2 | b |'+sLineBreak
        +'| 3 | c |'+sLineBreak
        +'| 4 | d |'+sLineBreak
        +'| 5 | e |'+sLineBreak
        +'| 6 | f |'+sLineBreak
        +'| 7 | g |'+sLineBreak
        +'+----+------+'
    ),

    (
      Name:         'LAST_INSERT_ID';
      Declaration:  '(expr)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LAST_INSERT_ID() (no arguments) returns'+sLineBreak
        +'the first automatically generated value successfully'+sLineBreak
        +'inserted for an'+sLineBreak
        +'AUTO_INCREMENT column as a result of the most recently'+sLineBreak
        +'executed INSERT'+sLineBreak
        +'statement. The value of LAST_INSERT_ID() remains unchanged'+sLineBreak
        +'if no rows'+sLineBreak
        +'are successfully inserted.'+sLineBreak
        +' '+sLineBreak
        +'If one gives an argument to LAST_INSERT_ID(), then it will'+sLineBreak
        +'return the value of the expression and'+sLineBreak
        +'the next call to LAST_INSERT_ID() will return the same'+sLineBreak
        +'value. The value will also be sent to the client'+sLineBreak
        +'and can be accessed by the mysql_insert_id function.'+sLineBreak
        +' '+sLineBreak
        +'For example, after inserting a row that generates an'+sLineBreak
        +'AUTO_INCREMENT'+sLineBreak
        +'value, you can get the value like this:'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 9 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'You can also use LAST_INSERT_ID() to delete the last'+sLineBreak
        +'inserted row:'+sLineBreak
        +' '+sLineBreak
        +'DELETE FROM product WHERE id = LAST_INSERT_ID();'+sLineBreak
        +' '+sLineBreak
        +'If no rows were successfully inserted, LAST_INSERT_ID()'+sLineBreak
        +'returns 0.'+sLineBreak
        +' '+sLineBreak
        +'The value of LAST_INSERT_ID() will be consistent across all'+sLineBreak
        +'versions'+sLineBreak
        +'if all rows in the INSERT or UPDATE statement were'+sLineBreak
        +'successful.'+sLineBreak
        +' '+sLineBreak
        +'The currently executing statement does not affect the value'+sLineBreak
        +'of'+sLineBreak
        +'LAST_INSERT_ID(). Suppose that you generate an'+sLineBreak
        +'AUTO_INCREMENT value'+sLineBreak
        +'with one statement, and then refer to LAST_INSERT_ID() in a'+sLineBreak
        +'multiple-row INSERT statement that inserts rows into a table'+sLineBreak
        +'with its'+sLineBreak
        +'own AUTO_INCREMENT column. The value of LAST_INSERT_ID()'+sLineBreak
        +'will remain'+sLineBreak
        +'stable in the second statement; its value for the second and'+sLineBreak
        +'later'+sLineBreak
        +'rows is not affected by the earlier row insertions.'+sLineBreak
        +'(However, if you'+sLineBreak
        +'mix references to LAST_INSERT_ID() and LAST_INSERT_ID(expr),'+sLineBreak
        +'the'+sLineBreak
        +'effect is undefined.)'+sLineBreak
        +' '+sLineBreak
        +'If the previous statement returned an error, the value of'+sLineBreak
        +'LAST_INSERT_ID() is undefined. For transactional tables, if'+sLineBreak
        +'the'+sLineBreak
        +'statement is rolled back due to an error, the value of'+sLineBreak
        +'LAST_INSERT_ID() is left undefined. For manual ROLLBACK, the'+sLineBreak
        +'value of'+sLineBreak
        +'LAST_INSERT_ID() is not restored to that before the'+sLineBreak
        +'transaction; it'+sLineBreak
        +'remains as it was at the point of the ROLLBACK.'+sLineBreak
        +' '+sLineBreak
        +'Within the body of a stored routine (procedure or function)'+sLineBreak
        +'or a'+sLineBreak
        +'trigger, the value of LAST_INSERT_ID() changes the same way'+sLineBreak
        +'as for'+sLineBreak
        +'statements executed outside the body of these kinds of'+sLineBreak
        +'objects. The'+sLineBreak
        +'effect of a stored routine or trigger upon the value of'+sLineBreak
        +'LAST_INSERT_ID() that is seen by following statements'+sLineBreak
        +'depends on the'+sLineBreak
        +'kind of routine:'+sLineBreak
        +'If a stored procedure executes statements that change the'+sLineBreak
        +'value of LAST_INSERT_ID(), the new value will be seen by'+sLineBreak
        +'statements that follow the procedure call.'+sLineBreak
        +' '+sLineBreak
        +'For stored functions and triggers that change the value, the'+sLineBreak
        +'value is restored when the function or trigger ends, so'+sLineBreak
        +'following statements will not see a changed value.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t ('+sLineBreak
        +' id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY, '+sLineBreak
        +' f VARCHAR(1)) '+sLineBreak
        +'ENGINE = InnoDB;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''a'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''b'');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''c'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''d''),(''e'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+'+sLineBreak
        +'| id | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +'| 1 | a |'+sLineBreak
        +'| 2 | b |'+sLineBreak
        +'| 3 | c |'+sLineBreak
        +'| 4 | d |'+sLineBreak
        +'| 5 | e |'+sLineBreak
        +'+----+------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID(12);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| LAST_INSERT_ID(12) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 12 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 12 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''f'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID();'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LAST_INSERT_ID() |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 6 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+'+sLineBreak
        +'| id | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +'| 1 | a |'+sLineBreak
        +'| 2 | b |'+sLineBreak
        +'| 3 | c |'+sLineBreak
        +'| 4 | d |'+sLineBreak
        +'| 5 | e |'+sLineBreak
        +'| 6 | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LAST_INSERT_ID(12);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| LAST_INSERT_ID(12) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 12 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t(f) VALUES(''g'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM t;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+'+sLineBreak
        +'| id | f |'+sLineBreak
        +'+----+------+'+sLineBreak
        +'| 1 | a |'+sLineBreak
        +'| 2 | b |'+sLineBreak
        +'| 3 | c |'+sLineBreak
        +'| 4 | d |'+sLineBreak
        +'| 5 | e |'+sLineBreak
        +'| 6 | f |'+sLineBreak
        +'| 7 | g |'+sLineBreak
        +'+----+------+'
    ),

    (
      Name:         'LAST_VALUE';
      Declaration:  '(expr,[expr,...])';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LAST_VALUE() evaluates all expressions and returns the last.'+sLineBreak
        +' '+sLineBreak
        +'This is useful together with setting user variables to a'+sLineBreak
        +'value with @var:=expr, for example when you want to get data'+sLineBreak
        +'of rows updated/deleted without having to do two queries'+sLineBreak
        +'against the table.'+sLineBreak
        +' '+sLineBreak
        +'Since MariaDB 10.2.2, LAST_VALUE can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if no last value exists.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t1 (a int, b int);'+sLineBreak
        +'INSERT INTO t1 VALUES(1,10),(2,20);'+sLineBreak
        +'DELETE FROM t1 WHERE a=1 AND last_value(@a:=a,@b:=b,1);'+sLineBreak
        +'SELECT @a,@b;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+'+sLineBreak
        +'| @a | @b |'+sLineBreak
        +'+------+------+'+sLineBreak
        +'| 1 | 10 |'+sLineBreak
        +'+------+------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 ('+sLineBreak
        +' pk int primary key,'+sLineBreak
        +' a int,'+sLineBreak
        +' b int,'+sLineBreak
        +' c char(10),'+sLineBreak
        +' d decimal(10, 3),'+sLineBreak
        +' e real'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'( 1, 0, 1, ''one'', 0.1, 0.001),'+sLineBreak
        +'( 2, 0, 2, ''two'', 0.2, 0.002),'+sLineBreak
        +'( 3, 0, 3, ''three'', 0.3, 0.003),'+sLineBreak
        +'( 4, 1, 2, ''three'', 0.4, 0.004),'+sLineBreak
        +'( 5, 1, 1, ''two'', 0.5, 0.005),'+sLineBreak
        +'( 6, 1, 1, ''one'', 0.6, 0.006),'+sLineBreak
        +'( 7, 2, NULL, ''n_one'', 0.5, 0.007),'+sLineBreak
        +'( 8, 2, 1, ''n_two'', NULL, 0.008),'+sLineBreak
        +'( 9, 2, 2, NULL, 0.7, 0.009),'+sLineBreak
        +'(10, 2, 0, ''n_four'', 0.8, 0.010),'+sLineBreak
        +'(11, 2, 10, NULL, 0.9, NULL);'+sLineBreak
        +' '+sLineBreak
        +'SELECT pk, FIRST_VALUE(pk) OVER (ORDER BY pk) AS first_asc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk) AS last_asc,'+sLineBreak
        +' FIRST_VALUE(pk) OVER (ORDER BY pk DESC) AS first_desc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk DESC) AS last_desc'+sLineBreak
        +'FROM t1'+sLineBreak
        +'ORDER BY pk DESC;'+sLineBreak
        +' '+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| pk | first_asc | last_asc | first_desc | last_desc |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| 11 | 1 | 11 | 11 | 11 |'+sLineBreak
        +'| 10 | 1 | 10 | 11 | 10 |'+sLineBreak
        +'| 9 | 1 | 9 | 11 | 9 |'+sLineBreak
        +'| 8 | 1 | 8 | 11 | 8 |'+sLineBreak
        +'| 7 | 1 | 7 | 11 | 7 |'+sLineBreak
        +'| 6 | 1 | 6 | 11 | 6 |'+sLineBreak
        +'| 5 | 1 | 5 | 11 | 5 |'+sLineBreak
        +'| 4 | 1 | 4 | 11 | 4 |'+sLineBreak
        +'| 3 | 1 | 3 | 11 | 3 |'+sLineBreak
        +'| 2 | 1 | 2 | 11 | 2 |'+sLineBreak
        +'| 1 | 1 | 1 | 11 | 1 |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE t1 (i int);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'(1),(2),(3),(4),(5),(6),(7),(8),(9),(10);'+sLineBreak
        +' '+sLineBreak
        +'SELECT i,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW'+sLineBreak
        +'and 1 FOLLOWING) AS f_1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW and'+sLineBreak
        +'1 FOLLOWING) AS l_1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING'+sLineBreak
        +'AND 1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING AND'+sLineBreak
        +'1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING'+sLineBreak
        +'AND 1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND'+sLineBreak
        +'1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING'+sLineBreak
        +'AND 2 FOLLOWING) AS f_1f2f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING AND'+sLineBreak
        +'2 FOLLOWING) AS f_1f2f'+sLineBreak
        +'FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| i | f_1f | l_1f | f_1p1f | f_1p1f | f_2p1p | f_2p1p |'+sLineBreak
        +'f_1f2f | f_1f2f |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| 1 | 1 | 2 | 1 | 2 | NULL | NULL | 2 | 3 |'+sLineBreak
        +'| 2 | 2 | 3 | 1 | 3 | 1 | 1 | 3 | 4 |'+sLineBreak
        +'| 3 | 3 | 4 | 2 | 4 | 1 | 2 | 4 | 5 |'+sLineBreak
        +'| 4 | 4 | 5 | 3 | 5 | 2 | 3 | 5 | 6 |'+sLineBreak
        +'| 5 | 5 | 6 | 4 | 6 | 3 | 4 | 6 | 7 |'+sLineBreak
        +'| 6 | 6 | 7 | 5 | 7 | 4 | 5 | 7 | 8 |'+sLineBreak
        +'| 7 | 7 | 8 | 6 | 8 | 5 | 6 | 8 | 9 |'+sLineBreak
        +'| 8 | 8 | 9 | 7 | 9 | 6 | 7 | 9 | 10 |'+sLineBreak
        +'| 9 | 9 | 10 | 8 | 10 | 7 | 8 | 10 | 10 |'+sLineBreak
        +'| 10 | 10 | 10 | 9 | 10 | 8 | 9 | NULL | NULL |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'
    ),

    (
      Name:         'LAST_VALUE';
      Declaration:  '(expr)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LAST_VALUE() evaluates all expressions and returns the last.'+sLineBreak
        +' '+sLineBreak
        +'This is useful together with setting user variables to a'+sLineBreak
        +'value with @var:=expr, for example when you want to get data'+sLineBreak
        +'of rows updated/deleted without having to do two queries'+sLineBreak
        +'against the table.'+sLineBreak
        +' '+sLineBreak
        +'Since MariaDB 10.2.2, LAST_VALUE can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if no last value exists.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t1 (a int, b int);'+sLineBreak
        +'INSERT INTO t1 VALUES(1,10),(2,20);'+sLineBreak
        +'DELETE FROM t1 WHERE a=1 AND last_value(@a:=a,@b:=b,1);'+sLineBreak
        +'SELECT @a,@b;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+'+sLineBreak
        +'| @a | @b |'+sLineBreak
        +'+------+------+'+sLineBreak
        +'| 1 | 10 |'+sLineBreak
        +'+------+------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 ('+sLineBreak
        +' pk int primary key,'+sLineBreak
        +' a int,'+sLineBreak
        +' b int,'+sLineBreak
        +' c char(10),'+sLineBreak
        +' d decimal(10, 3),'+sLineBreak
        +' e real'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'( 1, 0, 1, ''one'', 0.1, 0.001),'+sLineBreak
        +'( 2, 0, 2, ''two'', 0.2, 0.002),'+sLineBreak
        +'( 3, 0, 3, ''three'', 0.3, 0.003),'+sLineBreak
        +'( 4, 1, 2, ''three'', 0.4, 0.004),'+sLineBreak
        +'( 5, 1, 1, ''two'', 0.5, 0.005),'+sLineBreak
        +'( 6, 1, 1, ''one'', 0.6, 0.006),'+sLineBreak
        +'( 7, 2, NULL, ''n_one'', 0.5, 0.007),'+sLineBreak
        +'( 8, 2, 1, ''n_two'', NULL, 0.008),'+sLineBreak
        +'( 9, 2, 2, NULL, 0.7, 0.009),'+sLineBreak
        +'(10, 2, 0, ''n_four'', 0.8, 0.010),'+sLineBreak
        +'(11, 2, 10, NULL, 0.9, NULL);'+sLineBreak
        +' '+sLineBreak
        +'SELECT pk, FIRST_VALUE(pk) OVER (ORDER BY pk) AS first_asc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk) AS last_asc,'+sLineBreak
        +' FIRST_VALUE(pk) OVER (ORDER BY pk DESC) AS first_desc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk DESC) AS last_desc'+sLineBreak
        +'FROM t1'+sLineBreak
        +'ORDER BY pk DESC;'+sLineBreak
        +' '+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| pk | first_asc | last_asc | first_desc | last_desc |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| 11 | 1 | 11 | 11 | 11 |'+sLineBreak
        +'| 10 | 1 | 10 | 11 | 10 |'+sLineBreak
        +'| 9 | 1 | 9 | 11 | 9 |'+sLineBreak
        +'| 8 | 1 | 8 | 11 | 8 |'+sLineBreak
        +'| 7 | 1 | 7 | 11 | 7 |'+sLineBreak
        +'| 6 | 1 | 6 | 11 | 6 |'+sLineBreak
        +'| 5 | 1 | 5 | 11 | 5 |'+sLineBreak
        +'| 4 | 1 | 4 | 11 | 4 |'+sLineBreak
        +'| 3 | 1 | 3 | 11 | 3 |'+sLineBreak
        +'| 2 | 1 | 2 | 11 | 2 |'+sLineBreak
        +'| 1 | 1 | 1 | 11 | 1 |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE t1 (i int);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'(1),(2),(3),(4),(5),(6),(7),(8),(9),(10);'+sLineBreak
        +' '+sLineBreak
        +'SELECT i,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW'+sLineBreak
        +'and 1 FOLLOWING) AS f_1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW and'+sLineBreak
        +'1 FOLLOWING) AS l_1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING'+sLineBreak
        +'AND 1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING AND'+sLineBreak
        +'1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING'+sLineBreak
        +'AND 1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND'+sLineBreak
        +'1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING'+sLineBreak
        +'AND 2 FOLLOWING) AS f_1f2f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING AND'+sLineBreak
        +'2 FOLLOWING) AS f_1f2f'+sLineBreak
        +'FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| i | f_1f | l_1f | f_1p1f | f_1p1f | f_2p1p | f_2p1p |'+sLineBreak
        +'f_1f2f | f_1f2f |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| 1 | 1 | 2 | 1 | 2 | NULL | NULL | 2 | 3 |'+sLineBreak
        +'| 2 | 2 | 3 | 1 | 3 | 1 | 1 | 3 | 4 |'+sLineBreak
        +'| 3 | 3 | 4 | 2 | 4 | 1 | 2 | 4 | 5 |'+sLineBreak
        +'| 4 | 4 | 5 | 3 | 5 | 2 | 3 | 5 | 6 |'+sLineBreak
        +'| 5 | 5 | 6 | 4 | 6 | 3 | 4 | 6 | 7 |'+sLineBreak
        +'| 6 | 6 | 7 | 5 | 7 | 4 | 5 | 7 | 8 |'+sLineBreak
        +'| 7 | 7 | 8 | 6 | 8 | 5 | 6 | 8 | 9 |'+sLineBreak
        +'| 8 | 8 | 9 | 7 | 9 | 6 | 7 | 9 | 10 |'+sLineBreak
        +'| 9 | 9 | 10 | 8 | 10 | 7 | 8 | 10 | 10 |'+sLineBreak
        +'| 10 | 10 | 10 | 9 | 10 | 8 | 9 | NULL | NULL |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'
    ),

    (
      Name:         'LAST_VALUE';
      Declaration:  '(  [ PARTITION BY partition_expression ]  [ ORDER BY order_list ] )';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LAST_VALUE() evaluates all expressions and returns the last.'+sLineBreak
        +' '+sLineBreak
        +'This is useful together with setting user variables to a'+sLineBreak
        +'value with @var:=expr, for example when you want to get data'+sLineBreak
        +'of rows updated/deleted without having to do two queries'+sLineBreak
        +'against the table.'+sLineBreak
        +' '+sLineBreak
        +'Since MariaDB 10.2.2, LAST_VALUE can be used as a window'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if no last value exists.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t1 (a int, b int);'+sLineBreak
        +'INSERT INTO t1 VALUES(1,10),(2,20);'+sLineBreak
        +'DELETE FROM t1 WHERE a=1 AND last_value(@a:=a,@b:=b,1);'+sLineBreak
        +'SELECT @a,@b;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+'+sLineBreak
        +'| @a | @b |'+sLineBreak
        +'+------+------+'+sLineBreak
        +'| 1 | 10 |'+sLineBreak
        +'+------+------+'+sLineBreak
        +' '+sLineBreak
        +'As a window function:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE t1 ('+sLineBreak
        +' pk int primary key,'+sLineBreak
        +' a int,'+sLineBreak
        +' b int,'+sLineBreak
        +' c char(10),'+sLineBreak
        +' d decimal(10, 3),'+sLineBreak
        +' e real'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'( 1, 0, 1, ''one'', 0.1, 0.001),'+sLineBreak
        +'( 2, 0, 2, ''two'', 0.2, 0.002),'+sLineBreak
        +'( 3, 0, 3, ''three'', 0.3, 0.003),'+sLineBreak
        +'( 4, 1, 2, ''three'', 0.4, 0.004),'+sLineBreak
        +'( 5, 1, 1, ''two'', 0.5, 0.005),'+sLineBreak
        +'( 6, 1, 1, ''one'', 0.6, 0.006),'+sLineBreak
        +'( 7, 2, NULL, ''n_one'', 0.5, 0.007),'+sLineBreak
        +'( 8, 2, 1, ''n_two'', NULL, 0.008),'+sLineBreak
        +'( 9, 2, 2, NULL, 0.7, 0.009),'+sLineBreak
        +'(10, 2, 0, ''n_four'', 0.8, 0.010),'+sLineBreak
        +'(11, 2, 10, NULL, 0.9, NULL);'+sLineBreak
        +' '+sLineBreak
        +'SELECT pk, FIRST_VALUE(pk) OVER (ORDER BY pk) AS first_asc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk) AS last_asc,'+sLineBreak
        +' FIRST_VALUE(pk) OVER (ORDER BY pk DESC) AS first_desc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk DESC) AS last_desc'+sLineBreak
        +'FROM t1'+sLineBreak
        +'ORDER BY pk DESC;'+sLineBreak
        +' '+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| pk | first_asc | last_asc | first_desc | last_desc |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| 11 | 1 | 11 | 11 | 11 |'+sLineBreak
        +'| 10 | 1 | 10 | 11 | 10 |'+sLineBreak
        +'| 9 | 1 | 9 | 11 | 9 |'+sLineBreak
        +'| 8 | 1 | 8 | 11 | 8 |'+sLineBreak
        +'| 7 | 1 | 7 | 11 | 7 |'+sLineBreak
        +'| 6 | 1 | 6 | 11 | 6 |'+sLineBreak
        +'| 5 | 1 | 5 | 11 | 5 |'+sLineBreak
        +'| 4 | 1 | 4 | 11 | 4 |'+sLineBreak
        +'| 3 | 1 | 3 | 11 | 3 |'+sLineBreak
        +'| 2 | 1 | 2 | 11 | 2 |'+sLineBreak
        +'| 1 | 1 | 1 | 11 | 1 |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE t1 (i int);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'(1),(2),(3),(4),(5),(6),(7),(8),(9),(10);'+sLineBreak
        +' '+sLineBreak
        +'SELECT i,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW'+sLineBreak
        +'and 1 FOLLOWING) AS f_1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW and'+sLineBreak
        +'1 FOLLOWING) AS l_1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING'+sLineBreak
        +'AND 1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING AND'+sLineBreak
        +'1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING'+sLineBreak
        +'AND 1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND'+sLineBreak
        +'1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING'+sLineBreak
        +'AND 2 FOLLOWING) AS f_1f2f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING AND'+sLineBreak
        +'2 FOLLOWING) AS f_1f2f'+sLineBreak
        +'FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| i | f_1f | l_1f | f_1p1f | f_1p1f | f_2p1p | f_2p1p |'+sLineBreak
        +'f_1f2f | f_1f2f |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| 1 | 1 | 2 | 1 | 2 | NULL | NULL | 2 | 3 |'+sLineBreak
        +'| 2 | 2 | 3 | 1 | 3 | 1 | 1 | 3 | 4 |'+sLineBreak
        +'| 3 | 3 | 4 | 2 | 4 | 1 | 2 | 4 | 5 |'+sLineBreak
        +'| 4 | 4 | 5 | 3 | 5 | 2 | 3 | 5 | 6 |'+sLineBreak
        +'| 5 | 5 | 6 | 4 | 6 | 3 | 4 | 6 | 7 |'+sLineBreak
        +'| 6 | 6 | 7 | 5 | 7 | 4 | 5 | 7 | 8 |'+sLineBreak
        +'| 7 | 7 | 8 | 6 | 8 | 5 | 6 | 8 | 9 |'+sLineBreak
        +'| 8 | 8 | 9 | 7 | 9 | 6 | 7 | 9 | 10 |'+sLineBreak
        +'| 9 | 9 | 10 | 8 | 10 | 7 | 8 | 10 | 10 |'+sLineBreak
        +'| 10 | 10 | 10 | 9 | 10 | 8 | 9 | NULL | NULL |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'
    ),

    (
      Name:         'ROW_COUNT';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'ROW_COUNT() returns the number of rows updated, inserted or'+sLineBreak
        +'deleted'+sLineBreak
        +'by the preceding statement. This is the same as the row'+sLineBreak
        +'count that the'+sLineBreak
        +'mysql client displays and the value from the'+sLineBreak
        +'mysql_affected_rows() C'+sLineBreak
        +'API function.'+sLineBreak
        +' '+sLineBreak
        +'Generally:'+sLineBreak
        +'For statements which return a result set (such as SELECT,'+sLineBreak
        +'SHOW, DESC or HELP), returns -1, even when the result set is'+sLineBreak
        +'empty. This is also true for administrative statements, such'+sLineBreak
        +'as OPTIMIZE.'+sLineBreak
        +'For DML statements other than SELECT and for ALTER TABLE,'+sLineBreak
        +'returns the number of affected rows.'+sLineBreak
        +'For DDL statements (including TRUNCATE) and for other'+sLineBreak
        +'statements which don''t return any result set (such as USE,'+sLineBreak
        +'DO, SIGNAL or DEALLOCATE PREPARE), returns 0.'+sLineBreak
        +' '+sLineBreak
        +'For UPDATE, affected rows is by default the number of rows'+sLineBreak
        +'that were actually changed. If the CLIENT_FOUND_ROWS flag to'+sLineBreak
        +'mysql_real_connect() is specified when connecting to mysqld,'+sLineBreak
        +'affected rows is instead the number of rows matched by the'+sLineBreak
        +'WHERE clause. '+sLineBreak
        +' '+sLineBreak
        +'For REPLACE, deleted rows are also counted. So, if REPLACE'+sLineBreak
        +'deletes a row and adds a new row, ROW_COUNT() returns 2.'+sLineBreak
        +' '+sLineBreak
        +'For INSERT ... ON DUPLICATE KEY, updated rows are counted'+sLineBreak
        +'twice. So, if INSERT adds a new rows and modifies another'+sLineBreak
        +'row, ROW_COUNT() returns 3.'+sLineBreak
        +' '+sLineBreak
        +'ROW_COUNT() does not take into account rows that are not'+sLineBreak
        +'directly deleted/updated by the last statement. This means'+sLineBreak
        +'that rows deleted by foreign keys or triggers are not'+sLineBreak
        +'counted.'+sLineBreak
        +' '+sLineBreak
        +'Warning: You can use ROW_COUNT() with prepared statements,'+sLineBreak
        +'but you need to call it after EXECUTE, not after DEALLOCATE'+sLineBreak
        +'PREPARE, because the row count for allocate prepare is'+sLineBreak
        +'always 0.'+sLineBreak
        +' '+sLineBreak
        +'Warning: When used after a CALL statement, this function'+sLineBreak
        +'returns the number of rows affected by the last statement in'+sLineBreak
        +'the procedure, not by the whole procedure.'+sLineBreak
        +' '+sLineBreak
        +'Warning: After INSERT DELAYED, ROW_COUNT() returns the'+sLineBreak
        +'number of the rows you tried to insert, not the number of'+sLineBreak
        +'the successful writes.'+sLineBreak
        +' '+sLineBreak
        +'This information can also be found in the diagnostics area.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the ROW_COUNT() function are not safe for'+sLineBreak
        +'replication.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t (A INT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t VALUES(1),(2),(3);'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROW_COUNT();'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ROW_COUNT() |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'DELETE FROM t WHERE A IN(1,2);'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROW_COUNT(); '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ROW_COUNT() |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'Example with prepared statements:'+sLineBreak
        +' '+sLineBreak
        +'SET @q = ''INSERT INTO t VALUES(1),(2),(3);'';'+sLineBreak
        +' '+sLineBreak
        +'PREPARE stmt FROM @q;'+sLineBreak
        +' '+sLineBreak
        +'EXECUTE stmt;'+sLineBreak
        +' '+sLineBreak
        +'Query OK, 3 rows affected (0.39 sec)'+sLineBreak
        +'Records: 3 Duplicates: 0 Warnings: 0'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROW_COUNT();'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ROW_COUNT() |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+-------------+'
    ),

    (
      Name:         'SCHEMA';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function is a synonym for DATABASE().'
    ),

    (
      Name:         'SESSION_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'SESSION_USER() is a synonym for USER().'
    ),

    (
      Name:         'SYSTEM_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'SYSTEM_USER() is a synonym for USER().'
    ),

    (
      Name:         'USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current MariaDB user name and host name, given'+sLineBreak
        +'when authenticating to MariaDB, as a string in the utf8'+sLineBreak
        +'character set.'+sLineBreak
        +' '+sLineBreak
        +'Note that the value of USER() may differ from the value of'+sLineBreak
        +'CURRENT_USER(), which is the user used to authenticate the'+sLineBreak
        +'current client. '+sLineBreak
        +'CURRENT_ROLE() returns the current active role.'+sLineBreak
        +' '+sLineBreak
        +'SYSTEM_USER() and SESSION_USER are synonyms for USER().'+sLineBreak
        +' '+sLineBreak
        +'Statements using the USER() function or one of its synonyms'+sLineBreak
        +'are not safe for statement level replication.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'shell> mysql --user="anonymous"'+sLineBreak
        +' '+sLineBreak
        +'MariaDB [(none)]> select user(),current_user();'+sLineBreak
        +'+---------------------+----------------+'+sLineBreak
        +'| user() | current_user() |'+sLineBreak
        +'+---------------------+----------------+'+sLineBreak
        +'| anonymous@localhost | @localhost |'+sLineBreak
        +'+---------------------+----------------+'
    ),

    (
      Name:         'VERSION';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string that indicates the MariaDB server version.'+sLineBreak
        +'The string'+sLineBreak
        +'uses the utf8 character set.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT VERSION();'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| VERSION() |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| 10.0.3-MariaDB-1~precise-log |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'The VERSION() string may have one or more of the following'+sLineBreak
        +'suffixes:'+sLineBreak
        +' '+sLineBreak
        +'Suffix | Description | '+sLineBreak
        +' '+sLineBreak
        +'-embedded | The server is an embedded server (libmysqld). | '+sLineBreak
        +' '+sLineBreak
        +'-log | General logging, slow logging or binary (replication)'+sLineBreak
        +'logging is enabled. | '+sLineBreak
        +' '+sLineBreak
        +'-debug | The server is compiled for debugging. | '+sLineBreak
        +' '+sLineBreak
        +'-valgrind | The server is compiled to be instrumented with'+sLineBreak
        +'valgrind. | '+sLineBreak
        +' '+sLineBreak
        +'Changing the Version String'+sLineBreak
        +' '+sLineBreak
        +'Some old legacy code may break because they are parsing the'+sLineBreak
        +'VERSION string and expecting a MySQL string or a simple'+sLineBreak
        +'version'+sLineBreak
        +'string like Joomla til API17, see MDEV-7780.'+sLineBreak
        +' '+sLineBreak
        +'In MariaDB 10.2 one can fool these applications by setting'+sLineBreak
        +'the version string from the command line or the my.cnf files'+sLineBreak
        +'with --version=....'
    ),

    (
      Name:         'JSON_ARRAY';
      Declaration:  '([value[, value2] ...])';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a JSON array containing the listed values. The list'+sLineBreak
        +'can be empty.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SELECT Json_Array(56, 3.1416, ''My name is "Foo"'', NULL);'+sLineBreak
        +'+--------------------------------------------------+'+sLineBreak
        +'| Json_Array(56, 3.1416, ''My name is "Foo"'', NULL) |'+sLineBreak
        +'+--------------------------------------------------+'+sLineBreak
        +'| [56, 3.1416, "My name is \"Foo\"", null] |'+sLineBreak
        +'+--------------------------------------------------+'
    ),

    (
      Name:         'JSON_ARRAY_APPEND';
      Declaration:  '(json_doc, path, value[, path, value] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Appends values to the end of the specified arrays within a'+sLineBreak
        +'JSON document, returning the result, or NULL if any of the'+sLineBreak
        +'arguments are NULL.'+sLineBreak
        +' '+sLineBreak
        +'Evaluation is performed from left to right, with the'+sLineBreak
        +'resulting document from the previous pair becoming the new'+sLineBreak
        +'value against which the next pair is evaluated.'+sLineBreak
        +' '+sLineBreak
        +'If the json_doc is not a valid JSON document, or if any of'+sLineBreak
        +'the paths are not valid, or contain a * or ** wildcard, an'+sLineBreak
        +'error is returned.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @json = ''[1, 2, [3, 4]]'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_APPEND(@json, ''$[0]'', 5)'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| JSON_ARRAY_APPEND(@json, ''$[0]'', 5) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| [[1, 5], 2, [3, 4]] |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_APPEND(@json, ''$[1]'', 6);'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| JSON_ARRAY_APPEND(@json, ''$[1]'', 6) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| [1, [2, 6], [3, 4]] |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_APPEND(@json, ''$[1]'', 6, ''$[2]'', 7);'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| JSON_ARRAY_APPEND(@json, ''$[1]'', 6, ''$[2]'', 7) |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| [1, [2, 6], [3, 4, 7]] |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_APPEND(@json, ''$'', 5);'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| JSON_ARRAY_APPEND(@json, ''$'', 5) |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| [1, 2, [3, 4], 5] |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @json = ''{"A": 1, "B": [2], "C": [3, 4]}'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_APPEND(@json, ''$.B'', 5);'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| JSON_ARRAY_APPEND(@json, ''$.B'', 5) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| {"A": 1, "B": [2, 5], "C": [3, 4]} |'+sLineBreak
        +'+------------------------------------+'
    ),

    (
      Name:         'JSON_ARRAY_INSERT';
      Declaration:  '(json_doc, path, value[, path, value] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Inserts a value into a JSON document, returning the modified'+sLineBreak
        +'document, or NULL if any of the arguments are NULL.'+sLineBreak
        +' '+sLineBreak
        +'Evaluation is performed from left to right, with the'+sLineBreak
        +'resulting document from the previous pair becoming the new'+sLineBreak
        +'value against which the next pair is evaluated.'+sLineBreak
        +' '+sLineBreak
        +'If the json_doc is not a valid JSON document, or if any of'+sLineBreak
        +'the paths are not valid, or contain a * or ** wildcard, an'+sLineBreak
        +'error is returned.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @json = ''[1, 2, [3, 4]]'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_INSERT(@json, ''$[0]'', 5);'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| JSON_ARRAY_INSERT(@json, ''$[0]'', 5) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| [5, 1, 2, [3, 4]] |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_INSERT(@json, ''$[1]'', 6);'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| JSON_ARRAY_INSERT(@json, ''$[1]'', 6) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| [1, 6, 2, [3, 4]] |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_ARRAY_INSERT(@json, ''$[1]'', 6, ''$[2]'', 7);'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| JSON_ARRAY_INSERT(@json, ''$[1]'', 6, ''$[2]'', 7) |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| [1, 6, 7, 2, [3, 4]] |'+sLineBreak
        +'+------------------------------------------------+'
    ),

    (
      Name:         'JSON_COMPACT';
      Declaration:  '(json_doc)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Removes all unnecessary spaces so the json document is as'+sLineBreak
        +'short as possible.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @j = ''{ "A": 1, "B": [2, 3]}'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_COMPACT(@j), @j;'+sLineBreak
        +'+-------------------+------------------------+'+sLineBreak
        +'| JSON_COMPACT(@j) | @j |'+sLineBreak
        +'+-------------------+------------------------+'+sLineBreak
        +'| {"A":1,"B":[2,3]} | { "A": 1, "B": [2, 3]} |'+sLineBreak
        +'+-------------------+------------------------+'
    ),

    (
      Name:         'JSON_CONTAINS';
      Declaration:  '(json_doc, val[, path])';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns whether or not the specified value is found in the'+sLineBreak
        +'given JSON document or, optionally, at the specified path'+sLineBreak
        +'within the document. Returns 1 if it does, 0 if not and NULL'+sLineBreak
        +'if any of the arguments are null. An error occurs if the'+sLineBreak
        +'document or path is not valid, or contains the * or **'+sLineBreak
        +'wildcards.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @json = ''{"A": 0, "B": {"C": 1}, "D": 2}'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_CONTAINS(@json, ''2'', ''$.A'');'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| JSON_CONTAINS(@json, ''2'', ''$.A'') |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_CONTAINS(@json, ''2'', ''$.D'');'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| JSON_CONTAINS(@json, ''2'', ''$.D'') |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_CONTAINS(@json, ''{"C": 1}'', ''$.A'');'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| JSON_CONTAINS(@json, ''{"C": 1}'', ''$.A'') |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_CONTAINS(@json, ''{"C": 1}'', ''$.B'');'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| JSON_CONTAINS(@json, ''{"C": 1}'', ''$.B'') |'+sLineBreak
        +'+-----------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------------------------+'
    ),

    (
      Name:         'JSON_CONTAINS_PATH';
      Declaration:  '(json_doc, return_arg, path[, path] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Indicates whether the given JSON document contains data at'+sLineBreak
        +'the specified path or paths. Returns 1 if it does, 0 if not'+sLineBreak
        +'and NULL if any of the arguments are null.'+sLineBreak
        +' '+sLineBreak
        +'The return_arg can be one or all:'+sLineBreak
        +'one - Returns 1 if at least one path exists within the JSON'+sLineBreak
        +'document. '+sLineBreak
        +'all - Returns 1 only if all paths exist within the JSON'+sLineBreak
        +'document.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @json = ''{"A": 1, "B": [2], "C": [3, 4]}'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_CONTAINS_PATH(@json, ''one'', ''$.A'', ''$.D'');'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| JSON_CONTAINS_PATH(@json, ''one'', ''$.A'', ''$.D'') |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'1 row in set (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_CONTAINS_PATH(@json, ''all'', ''$.A'', ''$.D'');'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| JSON_CONTAINS_PATH(@json, ''all'', ''$.A'', ''$.D'') |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------------------------------------+'
    ),

    (
      Name:         'JSON_DEPTH';
      Declaration:  '(json_doc)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the maximum depth of the given JSON document, or'+sLineBreak
        +'NULL if the argument is null. An error will occur if the'+sLineBreak
        +'argument is an invalid JSON document.'+sLineBreak
        +'Scalar values or empty arrays or objects have a depth of 1.'+sLineBreak
        +'Arrays or objects that are not empty but contain only'+sLineBreak
        +'elements or member values of depth 1 will have a depth of 2.'+sLineBreak
        +'In other cases, the depth will be greater than 2.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_DEPTH(''[]''), JSON_DEPTH(''true''),'+sLineBreak
        +'JSON_DEPTH(''{}'');'+sLineBreak
        +'+------------------+--------------------+------------------+'+sLineBreak
        +'| JSON_DEPTH(''[]'') | JSON_DEPTH(''true'') |'+sLineBreak
        +'JSON_DEPTH(''{}'') |'+sLineBreak
        +'+------------------+--------------------+------------------+'+sLineBreak
        +'| 1 | 1 | 1 |'+sLineBreak
        +'+------------------+--------------------+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_DEPTH(''[1, 2, 3]''), JSON_DEPTH(''[[], {},'+sLineBreak
        +'[]]'');'+sLineBreak
        +'+-------------------------+----------------------------+'+sLineBreak
        +'| JSON_DEPTH(''[1, 2, 3]'') | JSON_DEPTH(''[[], {}, []]'') |'+sLineBreak
        +'+-------------------------+----------------------------+'+sLineBreak
        +'| 2 | 2 |'+sLineBreak
        +'+-------------------------+----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_DEPTH(''[1, 2, [3, 4, 5, 6], 7]'');'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| JSON_DEPTH(''[1, 2, [3, 4, 5, 6], 7]'') |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+---------------------------------------+'
    ),

    (
      Name:         'JSON_DETAILED';
      Declaration:  '(json_doc[, tab_size])';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Represents JSON in the most understandable way emphasizing'+sLineBreak
        +'nested structures.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @j = ''{ "A":1,"B":[2,3]}'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT @j;'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| @j |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| { "A":1,"B":[2,3]} |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_DETAILED(@j);'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| JSON_DETAILED(@j) |'+sLineBreak
        +'+------------------------------------------------------------+'+sLineBreak
        +'| {'+sLineBreak
        +' "A": 1,'+sLineBreak
        +' "B": '+sLineBreak
        +' ['+sLineBreak
        +' 2,'+sLineBreak
        +' 3'+sLineBreak
        +' ]'+sLineBreak
        +'} |'+sLineBreak
        +'+------------------------------------------------------------+'
    ),

    (
      Name:         'JSON_EXTRACT';
      Declaration:  '(json_doc, path[, path] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Extracts data from a JSON document. The extracted data is'+sLineBreak
        +'selected from the parts matching the path arguments. Returns'+sLineBreak
        +'all matched values; either as a single matched value, or, if'+sLineBreak
        +'the arguments could return multiple values, a result'+sLineBreak
        +'autowrapped as an array in the matching order.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if no paths match or if any of the arguments'+sLineBreak
        +'are NULL. '+sLineBreak
        +' '+sLineBreak
        +'An error will occur if any path argument is not a valid'+sLineBreak
        +'path, or if the json_doc argument is not a valid JSON'+sLineBreak
        +'document.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @json = ''[1, 2, [3, 4]]'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_EXTRACT(@json, ''$[1]'');'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| JSON_EXTRACT(@json, ''$[1]'') |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_EXTRACT(@json, ''$[2]'');'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| JSON_EXTRACT(@json, ''$[2]'') |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| [3, 4] |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_EXTRACT(@json, ''$[2][1]'');'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| JSON_EXTRACT(@json, ''$[2][1]'') |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+--------------------------------+'
    ),

    (
      Name:         'JSON_INSERT';
      Declaration:  '(json_doc, path, val[, path, val] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Inserts data into a JSON document, returning the resulting'+sLineBreak
        +'document or NULL if any argument is null. '+sLineBreak
        +' '+sLineBreak
        +'An error will occur if the JSON document is not invalid, or'+sLineBreak
        +'if any of the paths are invalid or contain a * or **'+sLineBreak
        +'wildcard.'+sLineBreak
        +' '+sLineBreak
        +'JSON_INSERT can only insert data while JSON_REPLACE can only'+sLineBreak
        +'update. JSON_SET can update or insert data. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @json = ''{ "A": 0, "B": [1, 2]}'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_INSERT(@json, ''$.C'', ''[3, 4]'');'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| JSON_INSERT(@json, ''$.C'', ''[3, 4]'') |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| { "A": 0, "B": [1, 2], "C":"[3, 4]"} |'+sLineBreak
        +'+--------------------------------------+'
    ),

    (
      Name:         'JSON_KEYS';
      Declaration:  '(json_doc[, path])';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the keys as a JSON array from the top-level value of'+sLineBreak
        +'a JSON object or, if the optional path argument is provided,'+sLineBreak
        +'the top-level keys from the path. '+sLineBreak
        +' '+sLineBreak
        +'Excludes keys from nested sub-objects in the top level'+sLineBreak
        +'value. The resulting array will be empty if the selected'+sLineBreak
        +'object is empty.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if any of the arguments are null, a given path'+sLineBreak
        +'does not locate an object, or if the json_doc argument is'+sLineBreak
        +'not an object.'+sLineBreak
        +' '+sLineBreak
        +'An error will occur if JSON document is invalid, the path is'+sLineBreak
        +'invalid or if the path contains a * or ** wildcard.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_KEYS(''{"A": 1, "B": {"C": 2}}'');'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| JSON_KEYS(''{"A": 1, "B": {"C": 2}}'') |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| ["A", "B"] |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_KEYS(''{"A": 1, "B": 2, "C": {"D":'+sLineBreak
        +'3}}'', ''$.C'');'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| JSON_KEYS(''{"A": 1, "B": 2, "C": {"D": 3}}'','+sLineBreak
        +'''$.C'') |'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| ["D"] |'+sLineBreak
        +'+-----------------------------------------------------+'
    ),

    (
      Name:         'JSON_LENGTH';
      Declaration:  '(json_doc[, path])';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length of a JSON document, or, if the optional'+sLineBreak
        +'path argument is given, the length of the value within the'+sLineBreak
        +'document specified by the path. '+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if any of the arguments argument are null or'+sLineBreak
        +'the path argument does not identify a value in the document.'+sLineBreak
        +''+sLineBreak
        +' '+sLineBreak
        +'An error will occur if the JSON document is invalid, the'+sLineBreak
        +'path is invalid or if the path contains a * or ** wildcard.'+sLineBreak
        +' '+sLineBreak
        +'Length will be determined as follow:'+sLineBreak
        +'A scalar''s length is always 1.'+sLineBreak
        +'If an array, the number of elements in the array.'+sLineBreak
        +'If an object, the number of members in the object.'+sLineBreak
        +' '+sLineBreak
        +'The length of nested arrays or objects are not counted.'
    ),

    (
      Name:         'JSON_LOOSE';
      Declaration:  '(json_doc)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Adds spaces to a JSON document to make it look more'+sLineBreak
        +'readable.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @j = ''{ "A":1,"B":[2,3]}'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_LOOSE(@j), @j;'+sLineBreak
        +'+-----------------------+--------------------+'+sLineBreak
        +'| JSON_LOOSE(@j) | @j |'+sLineBreak
        +'+-----------------------+--------------------+'+sLineBreak
        +'| {"A": 1, "B": [2, 3]} | { "A":1,"B":[2,3]} |'+sLineBreak
        +'+-----------------------+--------------------+'
    ),

    (
      Name:         'JSON_MERGE';
      Declaration:  '(json_doc, json_doc[, json_doc] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Merges the given JSON documents.'+sLineBreak
        +' '+sLineBreak
        +'Returns the merged result,or NULL if any argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +'An error occurs if any of the arguments are not valid JSON'+sLineBreak
        +'documents.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @json1 = ''[1, 2]'';'+sLineBreak
        +' '+sLineBreak
        +'SET @json2 = ''[3, 4]'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_MERGE(@json1,@json2);'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| JSON_MERGE(@json1,@json2) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| [1, 2, 3, 4] |'+sLineBreak
        +'+---------------------------+'
    ),

    (
      Name:         'JSON_OBJECT';
      Declaration:  '([key, value[, key, value] ...])';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a JSON object containing the given key/value pairs.'+sLineBreak
        +'The key/value list can be empty.'+sLineBreak
        +' '+sLineBreak
        +'An error will occur if there are an odd number of arguments,'+sLineBreak
        +'or any key name is NULL.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_OBJECT("id", 1, "name", "Monty");'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| JSON_OBJECT("id", 1, "name", "Monty") |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| {"id": 1, "name": "Monty"} |'+sLineBreak
        +'+---------------------------------------+'
    ),

    (
      Name:         'JSON_QUERY';
      Declaration:  '(json_doc, path)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a JSON document, returns an object or array specified'+sLineBreak
        +'by the path. Returns NULL if not given a valid JSON'+sLineBreak
        +'document, or if there is no match.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'select json_query(''{"key1":{"a":1, "b":[1,2]}}'','+sLineBreak
        +'''$.key1'');'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| json_query(''{"key1":{"a":1, "b":[1,2]}}'','+sLineBreak
        +'''$.key1'') |'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| {"a":1, "b":[1,2]} |'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'select json_query(''{"key1":123, "key1": [1,2,3]}'','+sLineBreak
        +'''$.key1'');'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| json_query(''{"key1":123, "key1": [1,2,3]}'','+sLineBreak
        +'''$.key1'') |'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| [1,2,3] |'+sLineBreak
        +'+-------------------------------------------------------+'
    ),

    (
      Name:         'JSON_QUOTE';
      Declaration:  '(json_value)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Quotes a string as a JSON value, usually for producing valid'+sLineBreak
        +'JSON string literals for inclusion in JSON documents. Wraps'+sLineBreak
        +'the string with double quote characters and escapes interior'+sLineBreak
        +'quotes and other special characters, returning a utf8mb4'+sLineBreak
        +'string. '+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if the argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_QUOTE(''A''), JSON_QUOTE("B"),'+sLineBreak
        +'JSON_QUOTE(''"C"'');'+sLineBreak
        +'+-----------------+-----------------+-------------------+'+sLineBreak
        +'| JSON_QUOTE(''A'') | JSON_QUOTE("B") |'+sLineBreak
        +'JSON_QUOTE(''"C"'') |'+sLineBreak
        +'+-----------------+-----------------+-------------------+'+sLineBreak
        +'| "A" | "B" | "\"C\"" |'+sLineBreak
        +'+-----------------+-----------------+-------------------+'
    ),

    (
      Name:         'JSON_REMOVE';
      Declaration:  '(json_doc, path[, path] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Removes data from a JSON document returning the result, or'+sLineBreak
        +'NULL if any of the arguments are null. If the element does'+sLineBreak
        +'not exist in the document, no changes are made.'+sLineBreak
        +' '+sLineBreak
        +'An error will occur if JSON document is invalid, the path is'+sLineBreak
        +'invalid or if the path contains a * or ** wildcard.'+sLineBreak
        +' '+sLineBreak
        +'Path arguments are evaluated from left to right, with the'+sLineBreak
        +'result from the earlier evaluation being used as the value'+sLineBreak
        +'for the next.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_REMOVE(''{"A": 1, "B": 2, "C": {"D":'+sLineBreak
        +'3}}'', ''$.C'');'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| JSON_REMOVE(''{"A": 1, "B": 2, "C": {"D": 3}}'','+sLineBreak
        +'''$.C'') |'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| {"A": 1, "B": 2} |'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_REMOVE(''["A", "B", ["C", "D"],'+sLineBreak
        +'"E"]'', ''$[1]'');'+sLineBreak
        +'+----------------------------------------------------+'+sLineBreak
        +'| JSON_REMOVE(''["A", "B", ["C", "D"], "E"]'','+sLineBreak
        +'''$[1]'') |'+sLineBreak
        +'+----------------------------------------------------+'+sLineBreak
        +'| ["A", ["C", "D"], "E"] |'+sLineBreak
        +'+----------------------------------------------------+'
    ),

    (
      Name:         'JSON_REPLACE';
      Declaration:  '(json_doc, path, val[, path, val] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Replaces existing values in a JSON document, returning the'+sLineBreak
        +'result, or NULL if any of the arguments are NULL. '+sLineBreak
        +' '+sLineBreak
        +'An error will occur if the JSON document is invalid, the'+sLineBreak
        +'path is invalid or if the path contains a * or ** wildcard.'+sLineBreak
        +' '+sLineBreak
        +'Paths and values are evaluated from left to right, with the'+sLineBreak
        +'result from the earlier evaluation being used as the value'+sLineBreak
        +'for the next.'+sLineBreak
        +' '+sLineBreak
        +'JSON_REPLACE can only update data, while JSON_INSERT can'+sLineBreak
        +'only insert. JSON_SET can update or insert data. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_REPLACE(''{ "A": 1, "B": [2, 3]}'','+sLineBreak
        +'''$.B[1]'', 4);'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| JSON_REPLACE(''{ "A": 1, "B": [2, 3]}'', ''$.B[1]'','+sLineBreak
        +'4) |'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| { "A": 1, "B": [2, 4]} |'+sLineBreak
        +'+-----------------------------------------------------+'
    ),

    (
      Name:         'JSON_SEARCH';
      Declaration:  '(json_doc, return_arg, search_str[, escape_char[, path] ...])';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the path to the given string within a JSON document,'+sLineBreak
        +'or NULL if any of json_doc, search_str or a path argument is'+sLineBreak
        +'NULL; if the search string is not found, or if no path'+sLineBreak
        +'exists within the document. '+sLineBreak
        +' '+sLineBreak
        +'A warning will occur if the JSON document is not valid, any'+sLineBreak
        +'of the path arguments are not valid, if return_arg is'+sLineBreak
        +'neither one nor all, or if the escape character is not a'+sLineBreak
        +'constant. NULL will be returned.'+sLineBreak
        +' '+sLineBreak
        +'return_arg can be one of two values:'+sLineBreak
        +'''one: Terminates after finding the first match, so will'+sLineBreak
        +'return one path string. If there is more than one match, it'+sLineBreak
        +'is undefined which is considered first.'+sLineBreak
        +'all: Returns all matching path strings, without duplicates.'+sLineBreak
        +'Multiple strings are autowrapped as an array. The order is'+sLineBreak
        +'undefined.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @json = ''["A", [{"B": "1"}], {"C":"AB"},'+sLineBreak
        +'{"D":"BC"}]'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_SEARCH(@json, ''one'', ''AB'');'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| JSON_SEARCH(@json, ''one'', ''AB'') |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| "$[2].C" |'+sLineBreak
        +'+---------------------------------+'
    ),

    (
      Name:         'JSON_SET';
      Declaration:  '(json_doc, path, val[, path, val] ...)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Updates or inserts data into a JSON document, returning the'+sLineBreak
        +'result, or NULL if any of the arguments are NULL or the'+sLineBreak
        +'optional path fails to find an object.'+sLineBreak
        +' '+sLineBreak
        +'An error will occur if the JSON document is invalid, the'+sLineBreak
        +'path is invalid or if the path contains a * or  wildcard.'+sLineBreak
        +' '+sLineBreak
        +'JSON_SET can update or insert data, while JSON_REPLACE can'+sLineBreak
        +'only update, and JSON_INSERT only insert.'
    ),

    (
      Name:         'JSON_TYPE';
      Declaration:  '(json_val)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the type of a JSON value, or NULL if the argument is'+sLineBreak
        +'null.'+sLineBreak
        +' '+sLineBreak
        +'An error will occur if the argument is an invalid JSON'+sLineBreak
        +'value.'+sLineBreak
        +' '+sLineBreak
        +'The following is a complete list of the possible return'+sLineBreak
        +'types:'+sLineBreak
        +' '+sLineBreak
        +'Return type | Value | '+sLineBreak
        +' '+sLineBreak
        +'ARRAY | JSON array | '+sLineBreak
        +' '+sLineBreak
        +'BIT | MariaDB BIT scalar | '+sLineBreak
        +' '+sLineBreak
        +'BLOB | MariaDB binary types (BINARY, VARBINARY or BLOB) | '+sLineBreak
        +' '+sLineBreak
        +'BOOLEAN | JSON true/false literals | '+sLineBreak
        +' '+sLineBreak
        +'DATE | MariaDB DATE scalar | '+sLineBreak
        +' '+sLineBreak
        +'DATETIME | MariaDB DATETIME or TIMESTAMP scalar | '+sLineBreak
        +' '+sLineBreak
        +'DECIMAL | MariaDB DECIMAL or NUMERIC scalar | '+sLineBreak
        +' '+sLineBreak
        +'DOUBLE | MariaDB DOUBLE FLOAT scalar | '+sLineBreak
        +' '+sLineBreak
        +'INTEGER | MariaDB integer types (TINYINT, SMALLINT,'+sLineBreak
        +'MEDIUMINT, INT or BIGINT) | '+sLineBreak
        +' '+sLineBreak
        +'NULL | JSON null literal or NULL argument | '+sLineBreak
        +' '+sLineBreak
        +'OBJECT | JSON object | '+sLineBreak
        +' '+sLineBreak
        +'OPAQUE | Any valid JSON value that is not one of the other'+sLineBreak
        +'types. | '+sLineBreak
        +' '+sLineBreak
        +'STRING | MariaDB character types (CHAR, VARCHAR, TEXT, ENUM'+sLineBreak
        +'or SET) | '+sLineBreak
        +' '+sLineBreak
        +'TIME | MariaDB TIME scalar | '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_TYPE(''{"A": 1, "B": 2, "C": 3}'');'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| JSON_TYPE(''{"A": 1, "B": 2, "C": 3}'') |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| OBJECT |'+sLineBreak
        +'+---------------------------------------+'
    ),

    (
      Name:         'JSON_UNQUOTE';
      Declaration:  '(val)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Unquotes a JSON value, returning a string, or NULL if the'+sLineBreak
        +'argument is null. '+sLineBreak
        +' '+sLineBreak
        +'An error will occur if the given value begins and ends with'+sLineBreak
        +'double quotes and is an invalid JSON string literal.'+sLineBreak
        +' '+sLineBreak
        +'Certain character sequences have special meanings within a'+sLineBreak
        +'string. Usually, a backspace is ignored, but the escape'+sLineBreak
        +'sequences in the table below are recognised by MariaDB,'+sLineBreak
        +'unless the SQL Mode is set to NO_BACKSLASH_ESCAPES SQL.'+sLineBreak
        +' '+sLineBreak
        +'Escape sequence | Character | '+sLineBreak
        +' '+sLineBreak
        +'\" | Double quote (") | '+sLineBreak
        +' '+sLineBreak
        +'\b | Backspace | '+sLineBreak
        +' '+sLineBreak
        +'\f | Formfeed | '+sLineBreak
        +' '+sLineBreak
        +'\n | Newline (linefeed) | '+sLineBreak
        +' '+sLineBreak
        +'\r | Carriage return | '+sLineBreak
        +' '+sLineBreak
        +'\t | Tab | '+sLineBreak
        +' '+sLineBreak
        +'\\ | Backslash (\) | '+sLineBreak
        +' '+sLineBreak
        +'\uXXXX | UTF-8 bytes for Unicode value XXXX | '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_UNQUOTE(''"Monty"'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| JSON_UNQUOTE(''"Monty"'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| Monty |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'With the default SQL Mode:'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_UNQUOTE(''Si\bng\ting'');'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| JSON_UNQUOTE(''Si\bng\ting'') |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| Sng ing |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Setting NO_BACKSLASH_ESCAPES:'+sLineBreak
        +' '+sLineBreak
        +'SET @@sql_mode = ''NO_BACKSLASH_ESCAPES'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_UNQUOTE(''Si\bng\ting'');'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| JSON_UNQUOTE(''Si\bng\ting'') |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| Si\bng\ting |'+sLineBreak
        +'+-----------------------------+'
    ),

    (
      Name:         'JSON_VALID';
      Declaration:  '(value)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Indicates whether the given value is a valid JSON document'+sLineBreak
        +'or not. Returns 1 if valid, 0 if not, and NULL if the'+sLineBreak
        +'argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.4.3, the JSON_VALID function is'+sLineBreak
        +'automatically used as a CHECK constraint for the JSON data'+sLineBreak
        +'type alias in order to ensure that a valid json document is'+sLineBreak
        +'inserted. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT JSON_VALID(''{"id": 1, "name": "Monty"}'');'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| JSON_VALID(''{"id": 1, "name": "Monty"}'') |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT JSON_VALID(''{"id": 1, "name": "Monty",'+sLineBreak
        +'"oddfield"}'');'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +'| JSON_VALID(''{"id": 1, "name": "Monty",'+sLineBreak
        +'"oddfield"}'') |'+sLineBreak
        +'+------------------------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------------------------------------------+'
    ),

    (
      Name:         'JSON_VALUE';
      Declaration:  '(json_doc, path)';
      Category:     'JSON Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a JSON document, returns the scalar specified by the'+sLineBreak
        +'path. Returns NULL if not given a valid JSON document, or if'+sLineBreak
        +'there is no match.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'select json_value(''{"key1":123}'', ''$.key1'');'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| json_value(''{"key1":123}'', ''$.key1'') |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| 123 |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'select json_value(''{"key1": [1,2,3], "key1":123}'','+sLineBreak
        +'''$.key1'');'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| json_value(''{"key1": [1,2,3], "key1":123}'','+sLineBreak
        +'''$.key1'') |'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| 123 |'+sLineBreak
        +'+-------------------------------------------------------+'
    ),

    (
      Name:         'GLENGTH';
      Declaration:  '(ls)';
      Category:     'LineString Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns as a double-precision number the length of the'+sLineBreak
        +'LineString value ls in its associated spatial reference.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @ls = ''LineString(1 1,2 2,3 3)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT GLength(GeomFromText(@ls));'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| GLength(GeomFromText(@ls)) |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| 2.82842712474619 |'+sLineBreak
        +'+----------------------------+'
    ),

    (
      Name:         'ST_ENDPOINT';
      Declaration:  '(ls)';
      Category:     'LineString Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the Point that is the endpoint of the'+sLineBreak
        +'LineString value ls.'+sLineBreak
        +' '+sLineBreak
        +'ST_EndPoint() and EndPoint() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @ls = ''LineString(1 1,2 2,3 3)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT AsText(EndPoint(GeomFromText(@ls)));'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| AsText(EndPoint(GeomFromText(@ls))) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| POINT(3 3) |'+sLineBreak
        +'+-------------------------------------+'
    ),

    (
      Name:         'ST_NUMPOINTS';
      Declaration:  '(ls)';
      Category:     'LineString Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of Point objects in the LineString'+sLineBreak
        +'value ls.'+sLineBreak
        +' '+sLineBreak
        +'ST_NumPoints() and NumPoints() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @ls = ''LineString(1 1,2 2,3 3)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT NumPoints(GeomFromText(@ls));'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| NumPoints(GeomFromText(@ls)) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| 3 |'+sLineBreak
        +'+------------------------------+'
    ),

    (
      Name:         'ST_POINTN';
      Declaration:  '(ls,N)';
      Category:     'LineString Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the N-th Point in the LineString value ls.'+sLineBreak
        +'Points are numbered beginning with 1.'+sLineBreak
        +' '+sLineBreak
        +'ST_PointN() and PointN() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @ls = ''LineString(1 1,2 2,3 3)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT AsText(PointN(GeomFromText(@ls),2));'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| AsText(PointN(GeomFromText(@ls),2)) |'+sLineBreak
        +'+-------------------------------------+'+sLineBreak
        +'| POINT(2 2) |'+sLineBreak
        +'+-------------------------------------+'
    ),

    (
      Name:         'MBRContains';
      Declaration:  '(g1,g2)';
      Category:     'MBR';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding'+sLineBreak
        +'Rectangle of'+sLineBreak
        +'g1 contains the Minimum Bounding Rectangle of g2. This tests'+sLineBreak
        +'the'+sLineBreak
        +'opposite relationship as MBRWithin().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +' '+sLineBreak
        +'SET @g2 = GeomFromText(''Point(1 1)'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT MBRContains(@g1,@g2), MBRContains(@g2,@g1);'+sLineBreak
        +'+----------------------+----------------------+'+sLineBreak
        +'| MBRContains(@g1,@g2) | MBRContains(@g2,@g1) |'+sLineBreak
        +'+----------------------+----------------------+'+sLineBreak
        +'| 1 | 0 |'+sLineBreak
        +'+----------------------+----------------------+'
    ),

    (
      Name:         'MBRDisjoint';
      Declaration:  '(g1,g2)';
      Category:     'MBR';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding'+sLineBreak
        +'Rectangles of the two geometries g1 and g2 are disjoint. Two'+sLineBreak
        +'geometries are disjoint if they do not intersect, that is'+sLineBreak
        +'touch or overlap.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((4 4,4 7,7 7,7 4,4 4))'');'+sLineBreak
        +'SELECTmbrdisjoint(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| mbrdisjoint(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((3 3,3 6,6 6,6 3,3 3))'');'+sLineBreak
        +'SELECT mbrdisjoint(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| mbrdisjoint(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'
    ),

    (
      Name:         'MBREqual';
      Declaration:  '(g1,g2)';
      Category:     'MBR';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding'+sLineBreak
        +'Rectangles of'+sLineBreak
        +'the two geometries g1 and g2 are the same.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1=GEOMFROMTEXT(''LINESTRING(0 0, 1 2)'');'+sLineBreak
        +'SET @g2=GEOMFROMTEXT(''POLYGON((0 0, 0 2, 1 2, 1 0, 0'+sLineBreak
        +'0))'');'+sLineBreak
        +'SELECT MbrEqual(@g1,@g2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| MbrEqual(@g1,@g2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1=GEOMFROMTEXT(''LINESTRING(0 0, 1 3)'');'+sLineBreak
        +'SET @g2=GEOMFROMTEXT(''POLYGON((0 0, 0 2, 1 4, 1 0, 0'+sLineBreak
        +'0))'');'+sLineBreak
        +'SELECT MbrEqual(@g1,@g2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| MbrEqual(@g1,@g2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'MBRIntersects';
      Declaration:  '(g1,g2)';
      Category:     'MBR';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding'+sLineBreak
        +'Rectangles of the two geometries g1 and g2 intersect.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((3 3,3 6,6 6,6 3,3 3))'');'+sLineBreak
        +'SELECT mbrintersects(@g1,@g2);'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| mbrintersects(@g1,@g2) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((4 4,4 7,7 7,7 4,4 4))'');'+sLineBreak
        +'SELECT mbrintersects(@g1,@g2);'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| mbrintersects(@g1,@g2) |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------------+'
    ),

    (
      Name:         'MBROverlaps';
      Declaration:  '(g1,g2)';
      Category:     'MBR';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding'+sLineBreak
        +'Rectangles of'+sLineBreak
        +'the two geometries g1 and g2 overlap. The term spatially'+sLineBreak
        +'overlaps is'+sLineBreak
        +'used if two geometries intersect and their intersection'+sLineBreak
        +'results in a'+sLineBreak
        +'geometry of the same dimension but not equal to either of'+sLineBreak
        +'the given'+sLineBreak
        +'geometries.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((4 4,4 7,7 7,7 4,4 4))'');'+sLineBreak
        +'SELECT mbroverlaps(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| mbroverlaps(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((3 3,3 6,6 6,6 3,3 3))'');'+sLineBreak
        +'SELECT mbroverlaps(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| mbroverlaps(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 4,4 4,4 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((3 3,3 6,6 6,6 3,3 3))'');'+sLineBreak
        +'SELECT mbroverlaps(@g1,@g2);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| mbroverlaps(@g1,@g2) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'
    ),

    (
      Name:         'MBRTouches';
      Declaration:  '(g1,g2)';
      Category:     'MBR';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding'+sLineBreak
        +'Rectangles of'+sLineBreak
        +'the two geometries g1 and g2 touch. Two geometries spatially'+sLineBreak
        +'touch if'+sLineBreak
        +'the interiors of the geometries do not intersect, but the'+sLineBreak
        +'boundary of'+sLineBreak
        +'one of the geometries intersects either the boundary or the'+sLineBreak
        +'interior'+sLineBreak
        +'of the other.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((4 4,4 7,7 7,7 4,4 4))'');'+sLineBreak
        +'SELECT mbrtouches(@g1,@g2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| mbrtouches(@g1,@g2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((3 3,3 6,6 6,6 3,3 3))'');'+sLineBreak
        +'SELECT mbrtouches(@g1,@g2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| mbrtouches(@g1,@g2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 4,4 4,4 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((3 3,3 6,6 6,6 3,3 3))'');'+sLineBreak
        +'SELECT mbrtouches(@g1,@g2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| mbrtouches(@g1,@g2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'MBRWithin';
      Declaration:  '(g1,g2)';
      Category:     'MBR';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding'+sLineBreak
        +'Rectangle of'+sLineBreak
        +'g1 is within the Minimum Bounding Rectangle of g2. This'+sLineBreak
        +'tests the'+sLineBreak
        +'opposite relationship as MBRContains().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g1 = GeomFromText(''Polygon((0 0,0 3,3 3,3 0,0 0))'');'+sLineBreak
        +'SET @g2 = GeomFromText(''Polygon((0 0,0 5,5 5,5 0,0 0))'');'+sLineBreak
        +'SELECT MBRWithin(@g1,@g2), MBRWithin(@g2,@g1);'+sLineBreak
        +'+--------------------+--------------------+'+sLineBreak
        +'| MBRWithin(@g1,@g2) | MBRWithin(@g2,@g1) |'+sLineBreak
        +'+--------------------+--------------------+'+sLineBreak
        +'| 1 | 0 |'+sLineBreak
        +'+--------------------+--------------------+'
    ),

    (
      Name:         'GET_LOCK';
      Declaration:  '(str,timeout)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Tries to obtain a lock with a name given by the string str,'+sLineBreak
        +'using a timeout of timeout seconds. Returns 1 if the lock'+sLineBreak
        +'was obtained successfully, 0 if the attempt timed out (for'+sLineBreak
        +'example, because another client has previously locked the'+sLineBreak
        +'name), or NULL if an error occurred (such as running out of'+sLineBreak
        +'memory or the thread was killed with mysqladmin kill).'+sLineBreak
        +' '+sLineBreak
        +'A lock is released with RELEASE_LOCK(), when the connection'+sLineBreak
        +'terminates (either normally or abnormally), or before'+sLineBreak
        +'MariaDB 10.0.2, when the connection executes another'+sLineBreak
        +'GET_LOCK statement. From MariaDB 10.0.2, a connection can'+sLineBreak
        +'hold multiple locks at the same time, so a lock that is no'+sLineBreak
        +'longer needed needs to be explicitly released.'+sLineBreak
        +' '+sLineBreak
        +'The IS_FREE_LOCK function returns whether a specified lock a'+sLineBreak
        +'free or not, and the IS_USED_LOCK whether the function is in'+sLineBreak
        +'use or not.'+sLineBreak
        +' '+sLineBreak
        +'Locks obtained with GET_LOCK() do not interact with'+sLineBreak
        +'transactions. That is, committing a transaction does not'+sLineBreak
        +'release any such locks obtained during the transaction.'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.0.2, it is also possible to recursively set'+sLineBreak
        +'the same lock. If a lock with the same name is set n times,'+sLineBreak
        +'it needs to be released n times as well. '+sLineBreak
        +' '+sLineBreak
        +'str is case insensitive for GET_LOCK() and related'+sLineBreak
        +'functions. If str is an empty string or NULL, GET_LOCK()'+sLineBreak
        +'returns NULL and does nothing. From MariaDB 10.2.2, timeout'+sLineBreak
        +'supports microseconds. Before then, it was rounded to the'+sLineBreak
        +'closest integer.'+sLineBreak
        +' '+sLineBreak
        +'If the metadata_lock_info plugin is installed, locks'+sLineBreak
        +'acquired with this function are visible in the Information'+sLineBreak
        +'Schema METADATA_LOCK_INFO table.'+sLineBreak
        +' '+sLineBreak
        +'This function can be used to implement application locks or'+sLineBreak
        +'to simulate record locks. Names are locked on a server-wide'+sLineBreak
        +'basis. If a name has been locked by one client, GET_LOCK()'+sLineBreak
        +'blocks any request by another client for a lock with the'+sLineBreak
        +'same name. This allows clients that agree on a given lock'+sLineBreak
        +'name to use the name to perform cooperative advisory'+sLineBreak
        +'locking. But be aware that it also allows a client that is'+sLineBreak
        +'not among the set of cooperating clients to lock a name,'+sLineBreak
        +'either inadvertently or deliberately, and thus prevent any'+sLineBreak
        +'of the cooperating clients from locking that name. One way'+sLineBreak
        +'to reduce the likelihood of this is to use lock names that'+sLineBreak
        +'are database-specific or application-specific. For example,'+sLineBreak
        +'use lock names of the form db_name.str or app_name.str.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the GET_LOCK() function are not safe for'+sLineBreak
        +'replication.'+sLineBreak
        +' '+sLineBreak
        +'The patch to permit multiple locks was contributed by'+sLineBreak
        +'Konstantin "Kostja" Osipov (MDEV-3917).'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT GET_LOCK(''lock1'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock1'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IS_FREE_LOCK(''lock1''), IS_USED_LOCK(''lock1'');'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| IS_FREE_LOCK(''lock1'') | IS_USED_LOCK(''lock1'') |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| 0 | 46 |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IS_FREE_LOCK(''lock2''), IS_USED_LOCK(''lock2'');'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| IS_FREE_LOCK(''lock2'') | IS_USED_LOCK(''lock2'') |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| 1 | NULL |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.0.2, multiple locks can be held:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock2'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock2'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IS_FREE_LOCK(''lock1''), IS_FREE_LOCK(''lock2'');'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| IS_FREE_LOCK(''lock1'') | IS_FREE_LOCK(''lock2'') |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| 0 | 0 |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RELEASE_LOCK(''lock1''), RELEASE_LOCK(''lock2'');'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| RELEASE_LOCK(''lock1'') | RELEASE_LOCK(''lock2'') |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| 1 | 1 |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Before MariaDB 10.0.2, a connection could only hold a single'+sLineBreak
        +'lock:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock2'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock2'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IS_FREE_LOCK(''lock1''), IS_FREE_LOCK(''lock2'');'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| IS_FREE_LOCK(''lock1'') | IS_FREE_LOCK(''lock2'') |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| 1 | 0 |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RELEASE_LOCK(''lock1''), RELEASE_LOCK(''lock2'');'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| RELEASE_LOCK(''lock1'') | RELEASE_LOCK(''lock2'') |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +'| NULL | 1 |'+sLineBreak
        +'+-----------------------+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.0.2, it is possible to hold the same lock'+sLineBreak
        +'recursively. This example is viewed using the'+sLineBreak
        +'metadata_lock_info plugin:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock3'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock3'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock3'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock3'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM INFORMATION_SCHEMA.METADATA_LOCK_INFO;'+sLineBreak
        +' '+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| THREAD_ID | LOCK_MODE | LOCK_DURATION | LOCK_TYPE |'+sLineBreak
        +'TABLE_SCHEMA | TABLE_NAME |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| 46 | MDL_SHARED_NO_WRITE | NULL | User lock | lock3 | |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RELEASE_LOCK(''lock3'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| RELEASE_LOCK(''lock3'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM INFORMATION_SCHEMA.METADATA_LOCK_INFO;'+sLineBreak
        +' '+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| THREAD_ID | LOCK_MODE | LOCK_DURATION | LOCK_TYPE |'+sLineBreak
        +'TABLE_SCHEMA | TABLE_NAME |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| 46 | MDL_SHARED_NO_WRITE | NULL | User lock | lock3 | |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RELEASE_LOCK(''lock3'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| RELEASE_LOCK(''lock3'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM INFORMATION_SCHEMA.METADATA_LOCK_INFO;'+sLineBreak
        +' '+sLineBreak
        +'Empty set (0.000 sec)'+sLineBreak
        +' '+sLineBreak
        +'Timeout example: Connection 1:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock4'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock4'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Connection 2:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock4'',10);'+sLineBreak
        +' '+sLineBreak
        +'After 10 seconds...'+sLineBreak
        +' '+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock4'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Deadlocks are automatically detected and resolved.'+sLineBreak
        +'Connection 1:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock5'',10); '+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock5'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Connection 2:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock6'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock6'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Connection 1:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock6'',10); '+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock6'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Connection 2:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock5'',10);'+sLineBreak
        +'ERROR 1213 (40001): Deadlock found when trying to get lock;'+sLineBreak
        +' try restarting transaction'
    ),

    (
      Name:         'INET6_ATON';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given an IPv6 or IPv4 network address as a string, returns a'+sLineBreak
        +'binary string that represents the numeric value of the'+sLineBreak
        +'address.'+sLineBreak
        +' '+sLineBreak
        +'No trailing zone ID''s or traling network masks are'+sLineBreak
        +'permitted. For IPv4 addresses, or IPv6 addresses with IPv4'+sLineBreak
        +'address parts, no classful addresses or trailing port'+sLineBreak
        +'numbers are permitted and octal numbers are not supported.'+sLineBreak
        +' '+sLineBreak
        +'The returned binary string will be VARBINARY(16) or'+sLineBreak
        +'VARBINARY(4) for IPv6 and IPv4 addresses respectively.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if the argument is not understood.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT HEX(INET6_ATON(''10.0.1.1''));'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| HEX(INET6_ATON(''10.0.1.1'')) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| 0A000101 |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(INET6_ATON(''48f3::d432:1431:ba23:846f''));'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| HEX(INET6_ATON(''48f3::d432:1431:ba23:846f'')) |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| 48F3000000000000D4321431BA23846F |'+sLineBreak
        +'+----------------------------------------------+'
    ),

    (
      Name:         'INET6_NTOA';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given an IPv6 or IPv4 network address as a numeric binary'+sLineBreak
        +'string, returns the address as a nonbinary string in the'+sLineBreak
        +'connection character set.'+sLineBreak
        +' '+sLineBreak
        +'The return string is lowercase, and is platform independent,'+sLineBreak
        +'since it does not use functions specific to the operating'+sLineBreak
        +'system. It has a maximum length of 39 characters.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if the argument is not understood.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT INET6_NTOA(UNHEX(''0A000101''));'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| INET6_NTOA(UNHEX(''0A000101'')) |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 10.0.1.1 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT'+sLineBreak
        +'INET6_NTOA(UNHEX(''48F3000000000000D4321431BA23846F''));'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| INET6_NTOA(UNHEX(''48F3000000000000D4321431BA23846F'')) |'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| 48f3::d432:1431:ba23:846f |'+sLineBreak
        +'+-------------------------------------------------------+'
    ),

    (
      Name:         'INET_ATON';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given the dotted-quad representation of an IPv4 network'+sLineBreak
        +'address as a string,'+sLineBreak
        +'returns an integer that represents the numeric value of the'+sLineBreak
        +'address.'+sLineBreak
        +'Addresses may be 4- or 8-byte addresses.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if the argument is not understood.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT INET_ATON(''192.168.1.1'');'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| INET_ATON(''192.168.1.1'') |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| 3232235777 |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +' '+sLineBreak
        +'This is calculated as follows: 192 x 2563 + 168 x 256 2 + 1'+sLineBreak
        +'x 256 + 1'
    ),

    (
      Name:         'INET_NTOA';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a numeric IPv4 network address in network byte order'+sLineBreak
        +'(4 or 8 byte),'+sLineBreak
        +'returns the dotted-quad representation of the address as a'+sLineBreak
        +'string.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT INET_NTOA(3232235777);'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| INET_NTOA(3232235777) |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 192.168.1.1 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'192.168.1.1 corresponds to 3232235777 since 192 x 2563 + 168'+sLineBreak
        +'x 256 2 + 1 x 256 + 1 = 3232235777'
    ),

    (
      Name:         'IS_FREE_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Checks whether the lock named str is free to use (that is,'+sLineBreak
        +'not locked).'+sLineBreak
        +'Returns 1 if the lock is free (no one is using the lock),'+sLineBreak
        +' 0 if the lock is in use, and NULL if an'+sLineBreak
        +'error occurs (such as an incorrect argument, like an empty'+sLineBreak
        +'string or NULL). str is case insensitive.'+sLineBreak
        +' '+sLineBreak
        +'If the metadata_lock_info plugin is installed, the'+sLineBreak
        +'Information Schema metadata_lock_info table contains'+sLineBreak
        +'information about locks of this kind (as well as metadata'+sLineBreak
        +'locks).'+sLineBreak
        +' '+sLineBreak
        +'Statements using the IS_FREE_LOCK() function are not safe'+sLineBreak
        +'for replication.'
    ),

    (
      Name:         'IS_IPV4';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If the expression is a valid IPv4 address, returns 1,'+sLineBreak
        +'otherwise returns 0.'+sLineBreak
        +' '+sLineBreak
        +'IS_IPV4() is stricter than INET_ATON(), but as strict as'+sLineBreak
        +'INET6_ATON(), in determining the validity of an IPv4'+sLineBreak
        +'address. This implies that if IS_IPV4 returns 1, the same'+sLineBreak
        +'expression will always return a non-NULL result when passed'+sLineBreak
        +'to INET_ATON(), but that the reverse may not apply.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT IS_IPV4(''1110.0.1.1'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| IS_IPV4(''1110.0.1.1'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IS_IPV4(''48f3::d432:1431:ba23:846f'');'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| IS_IPV4(''48f3::d432:1431:ba23:846f'') |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+--------------------------------------+'
    ),

    (
      Name:         'IS_IPV4_COMPAT';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if a given numeric binary string IPv6 address,'+sLineBreak
        +'such as returned by INET6_ATON(), is IPv4-compatible,'+sLineBreak
        +'otherwise returns 0. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT IS_IPV4_COMPAT(INET6_ATON(''::10.0.1.1''));'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| IS_IPV4_COMPAT(INET6_ATON(''::10.0.1.1'')) |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT'+sLineBreak
        +'IS_IPV4_COMPAT(INET6_ATON(''::48f3::d432:1431:ba23:846f''));'+sLineBreak
        +'+-----------------------------------------------------------+'+sLineBreak
        +'|'+sLineBreak
        +'IS_IPV4_COMPAT(INET6_ATON(''::48f3::d432:1431:ba23:846f''))'+sLineBreak
        +'|'+sLineBreak
        +'+-----------------------------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------------------------------------------------+'
    ),

    (
      Name:         'IS_IPV4_MAPPED';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if a given a numeric binary string IPv6 address,'+sLineBreak
        +'such as returned by INET6_ATON(), is a valid IPv4-mapped'+sLineBreak
        +'address, otherwise returns 0.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT IS_IPV4_MAPPED(INET6_ATON(''::10.0.1.1''));'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| IS_IPV4_MAPPED(INET6_ATON(''::10.0.1.1'')) |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT IS_IPV4_MAPPED(INET6_ATON(''::ffff:10.0.1.1''));'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| IS_IPV4_MAPPED(INET6_ATON(''::ffff:10.0.1.1'')) |'+sLineBreak
        +'+-----------------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------------------------------+'
    ),

    (
      Name:         'IS_IPV6';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if the expression is a valid IPv6 address'+sLineBreak
        +'specified as a string, otherwise returns 0. Does not'+sLineBreak
        +'consider IPv4 addresses to be valid IPv6 addresses.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +' SELECT IS_IPV6(''48f3::d432:1431:ba23:846f'');'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| IS_IPV6(''48f3::d432:1431:ba23:846f'') |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'1 row in set (0.02 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT IS_IPV6(''10.0.1.1'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| IS_IPV6(''10.0.1.1'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'IS_USED_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Checks whether the lock named str is in use (that is,'+sLineBreak
        +'locked). If so,'+sLineBreak
        +'it returns the connection identifier of the client that'+sLineBreak
        +'holds the'+sLineBreak
        +'lock. Otherwise, it returns NULL. str is case insensitive.'+sLineBreak
        +' '+sLineBreak
        +'If the metadata_lock_info plugin is installed, the'+sLineBreak
        +'Information Schema metadata_lock_info table contains'+sLineBreak
        +'information about locks of this kind (as well as metadata'+sLineBreak
        +'locks).'+sLineBreak
        +' '+sLineBreak
        +'Statements using the IS_USED_LOCK() function are not safe'+sLineBreak
        +'for replication.'
    ),

    (
      Name:         'MASTER_GTID_WAIT';
      Declaration:  '(gtid-list[, timeout)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function takes a string containing a comma-separated'+sLineBreak
        +'list of global transaction id''s'+sLineBreak
        +'(similar to the value of, for example, gtid_binlog_pos). It'+sLineBreak
        +'waits until the value of gtid_slave_pos has the same or'+sLineBreak
        +'higher seq_no within all replication domains specified in'+sLineBreak
        +'the gtid-list; in other words, it waits until the slave has'+sLineBreak
        +'reached the specified GTID position.'+sLineBreak
        +' '+sLineBreak
        +'An optional second argument gives a timeout in seconds. If'+sLineBreak
        +'the timeout'+sLineBreak
        +'expires before the specified GTID position is reached, then'+sLineBreak
        +'the function'+sLineBreak
        +'returns -1. Passing NULL or a negative number for the'+sLineBreak
        +'timeout means no timeout, and the function will wait'+sLineBreak
        +'indefinitely.'+sLineBreak
        +' '+sLineBreak
        +' If the wait completes without a timeout, 0 is returned.'+sLineBreak
        +'Passing NULL for the'+sLineBreak
        +' gtid-list makes the function return NULL immediately,'+sLineBreak
        +'without waiting.'+sLineBreak
        +' '+sLineBreak
        +'The gtid-list may be the empty string, in which case'+sLineBreak
        +'MASTER_GTID_WAIT()'+sLineBreak
        +'returns immediately. If the gtid-list contains fewer domains'+sLineBreak
        +'than'+sLineBreak
        +'gtid_slave_pos, then only those domains are waited upon. If'+sLineBreak
        +'gtid-list'+sLineBreak
        +'contains a domain that is not present in @@gtid_slave_pos,'+sLineBreak
        +'then'+sLineBreak
        +'MASTER_GTID_WAIT() will wait until an event containing such'+sLineBreak
        +'domain_id arrives'+sLineBreak
        +'on the slave (or until timed out or killed).'+sLineBreak
        +' '+sLineBreak
        +'MASTER_GTID_WAIT() can be useful to ensure that a slave has'+sLineBreak
        +'caught up to'+sLineBreak
        +'a master. Simply take the value of gtid_binlog_pos on the'+sLineBreak
        +'master, and use it in a MASTER_GTID_WAIT() call on the'+sLineBreak
        +'slave; when the call completes, the slave'+sLineBreak
        +'will have caught up with that master position.'+sLineBreak
        +' '+sLineBreak
        +'MASTER_GTID_WAIT() can also be used in client applications'+sLineBreak
        +'together with the'+sLineBreak
        +'last_gtid session variable. This is useful in a'+sLineBreak
        +'read-scaleout replication setup, where the application'+sLineBreak
        +'writes to a single master but divides the'+sLineBreak
        +'reads out to a number of slaves to distribute the load. In'+sLineBreak
        +'such a setup, there'+sLineBreak
        +'is a risk that an application could first do an update on'+sLineBreak
        +'the master, and then'+sLineBreak
        +'a bit later do a read on a slave, and if the slave is not'+sLineBreak
        +'fast enough, the'+sLineBreak
        +'data read from the slave might not include the update just'+sLineBreak
        +'made, possibly'+sLineBreak
        +'confusing the application and/or the end-user. One way to'+sLineBreak
        +'avoid this is to'+sLineBreak
        +'request the value of last_gtid on the master just after the'+sLineBreak
        +'update. Then'+sLineBreak
        +'before doing the read on the slave, do a MASTER_GTID_WAIT()'+sLineBreak
        +'on the value'+sLineBreak
        +'obtained from the master; this will ensure that the read is'+sLineBreak
        +'not performed'+sLineBreak
        +'until the slave has replicated sufficiently far for the'+sLineBreak
        +'update to have become'+sLineBreak
        +'visible.'+sLineBreak
        +' '+sLineBreak
        +'Note that MASTER_GTID_WAIT() can be used even if the slave'+sLineBreak
        +'is configured not'+sLineBreak
        +'to use GTID for connections (CHANGE MASTER TO'+sLineBreak
        +'master_use_gtid=no). This is'+sLineBreak
        +'because from MariaDB 10, GTIDs are always logged on the'+sLineBreak
        +'master server, and'+sLineBreak
        +'always recorded on the slave servers.'+sLineBreak
        +' '+sLineBreak
        +'Differences to MASTER_POS_WAIT()'+sLineBreak
        +' '+sLineBreak
        +'MASTER_GTID_WAIT() is global; it waits for any master'+sLineBreak
        +'connection to reach'+sLineBreak
        +' the specified GTID position. MASTER_POS_WAIT() works only'+sLineBreak
        +'against a'+sLineBreak
        +' specific connection. This also means that while'+sLineBreak
        +'MASTER_POS_WAIT() aborts if'+sLineBreak
        +' its master connection is terminated with STOP SLAVE or due'+sLineBreak
        +'to an error,'+sLineBreak
        +' MASTER_GTID_WAIT() continues to wait while slaves are'+sLineBreak
        +'stopped.'+sLineBreak
        +' '+sLineBreak
        +'MASTER_GTID_WAIT() can take its timeout as a floating-point'+sLineBreak
        +'value, so a'+sLineBreak
        +' timeout in fractional seconds is supported, eg.'+sLineBreak
        +'MASTER_GTID_WAIT("0-1-100",'+sLineBreak
        +' 0.5). (The minimum wait is one microsecond, 0.000001'+sLineBreak
        +'seconds).'+sLineBreak
        +' '+sLineBreak
        +'MASTER_GTID_WAIT() allows one to specify a timeout of zero'+sLineBreak
        +'in order to do a'+sLineBreak
        +' non-blocking check to see if the slaves have progressed to'+sLineBreak
        +'a specific GTID position'+sLineBreak
        +' (MASTER_POS_WAIT() takes a zero timeout as meaning an'+sLineBreak
        +'infinite wait). To do'+sLineBreak
        +' an infinite MASTER_GTID_WAIT(), specify a negative timeout,'+sLineBreak
        +'or omit the'+sLineBreak
        +' timeout argument.'+sLineBreak
        +' '+sLineBreak
        +'MASTER_GTID_WAIT() does not return the number of events'+sLineBreak
        +'executed since the'+sLineBreak
        +' wait started, nor does it return NULL if a slave thread is'+sLineBreak
        +'stopped. It'+sLineBreak
        +' always returns either 0 for successful wait completed, or'+sLineBreak
        +'-1 for timeout'+sLineBreak
        +' reached (or NULL if the specified gtid-pos is NULL).'+sLineBreak
        +' '+sLineBreak
        +'Since MASTER_GTID_WAIT() looks only at the seq_no part of'+sLineBreak
        +'the GTIDs, not the'+sLineBreak
        +'server_id, care is needed if a slave becomes diverged from'+sLineBreak
        +'another server so'+sLineBreak
        +'that two different GTIDs with the same seq_no (in the same'+sLineBreak
        +'domain) arrive at'+sLineBreak
        +'the same server. This situation is in any case best avoided;'+sLineBreak
        +'setting'+sLineBreak
        +'gtid_strict_mode is recommended, as this will prevent any'+sLineBreak
        +'such out-of-order sequence numbers from ever being'+sLineBreak
        +'replicated on a slave.'
    ),

    (
      Name:         'MASTER_POS_WAIT';
      Declaration:  '(log_name,log_pos[,timeout,["connection_name"]])';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function is useful in replication for controlling'+sLineBreak
        +'master/slave synchronization. It blocks until the slave has'+sLineBreak
        +'read and applied all updates up to the specified position'+sLineBreak
        +'(log_name,log_pos) in the master log. The return value is'+sLineBreak
        +'the number of log events the slave had to wait for to'+sLineBreak
        +'advance to the specified position. The function returns NULL'+sLineBreak
        +'if'+sLineBreak
        +'the slave SQL thread is not started, the slave''s master'+sLineBreak
        +'information is not'+sLineBreak
        +'initialized, the arguments are incorrect, or an error'+sLineBreak
        +'occurs. It returns -1 if'+sLineBreak
        +'the timeout has been exceeded. If the slave SQL thread stops'+sLineBreak
        +'while'+sLineBreak
        +' MASTER_POS_WAIT() is waiting, the function returns NULL. If'+sLineBreak
        +'the slave is past the specified position, the function'+sLineBreak
        +'returns immediately.'+sLineBreak
        +' '+sLineBreak
        +'If a timeout value is specified, MASTER_POS_WAIT() stops'+sLineBreak
        +'waiting when timeout seconds have elapsed. timeout must be'+sLineBreak
        +'greater than 0; a'+sLineBreak
        +'zero or negative timeout means no timeout.'+sLineBreak
        +' '+sLineBreak
        +'The connection_name is used when you are using'+sLineBreak
        +'multi-source-replication. If you don''t specify it, it''s'+sLineBreak
        +'set to the value of the default_master_connection system'+sLineBreak
        +'variable.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the MASTER_POS_WAIT() function are not safe'+sLineBreak
        +'for replication.'
    ),

    (
      Name:         'NAME_CONST';
      Declaration:  '(name,value)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the given value. When used to produce a result set'+sLineBreak
        +'column,'+sLineBreak
        +' NAME_CONST() causes the column to have the given name. The'+sLineBreak
        +'arguments should be constants.'+sLineBreak
        +' '+sLineBreak
        +'This function is used internally when replicating stored'+sLineBreak
        +'procedures. It makes little sense to use it explicitly in'+sLineBreak
        +'SQL statements, and it was not supposed to be used like'+sLineBreak
        +'that.'+sLineBreak
        +' '+sLineBreak
        +'SELECT NAME_CONST(''myname'', 14);'+sLineBreak
        +'+--------+'+sLineBreak
        +'| myname |'+sLineBreak
        +'+--------+'+sLineBreak
        +'| 14 |'+sLineBreak
        +'+--------+'
    ),

    (
      Name:         'RELEASE_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Releases the lock named by the string str that was obtained'+sLineBreak
        +'with GET_LOCK(). Returns 1 if the lock was released, 0 if'+sLineBreak
        +'the lock was not established by this thread (in which case'+sLineBreak
        +'the lock is not'+sLineBreak
        +'released), and NULL if the named lock did not exist. The'+sLineBreak
        +'lock does not exist if it was never obtained by a call to'+sLineBreak
        +'GET_LOCK() or if it has previously been released.'+sLineBreak
        +' '+sLineBreak
        +'MariaDB until 10.0.1'+sLineBreak
        +' '+sLineBreak
        +'Before 10.0.2, GET_LOCK() released the existing lock, if'+sLineBreak
        +'any. Since 10.0.2 this does not happen, because multiple'+sLineBreak
        +'locks are allowed.'+sLineBreak
        +' '+sLineBreak
        +'str is case insensitive. If str is an empty string or NULL,'+sLineBreak
        +'RELEASE_LOCK() returns NULL and does nothing.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the RELEASE_LOCK() function are not safe'+sLineBreak
        +'for replication.'+sLineBreak
        +' '+sLineBreak
        +'The DO statement is convenient to use with RELEASE_LOCK().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Connection1:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock1'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock1'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Connection 2:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock2'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock2'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'Connection 1:'+sLineBreak
        +' '+sLineBreak
        +'SELECT RELEASE_LOCK(''lock1''), RELEASE_LOCK(''lock2''),'+sLineBreak
        +'RELEASE_LOCK(''lock3'');'+sLineBreak
        +'+-----------------------+-----------------------+-----------------------+'+sLineBreak
        +'| RELEASE_LOCK(''lock1'') | RELEASE_LOCK(''lock2'') |'+sLineBreak
        +'RELEASE_LOCK(''lock3'') |'+sLineBreak
        +'+-----------------------+-----------------------+-----------------------+'+sLineBreak
        +'| 1 | 0 | NULL |'+sLineBreak
        +'+-----------------------+-----------------------+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.0.2, it is possible to hold the same lock'+sLineBreak
        +'recursively. This example is viewed using the'+sLineBreak
        +'metadata_lock_info plugin:'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock3'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock3'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT GET_LOCK(''lock3'',10);'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| GET_LOCK(''lock3'',10) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM INFORMATION_SCHEMA.METADATA_LOCK_INFO;'+sLineBreak
        +' '+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| THREAD_ID | LOCK_MODE | LOCK_DURATION | LOCK_TYPE |'+sLineBreak
        +'TABLE_SCHEMA | TABLE_NAME |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| 46 | MDL_SHARED_NO_WRITE | NULL | User lock | lock3 | |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RELEASE_LOCK(''lock3'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| RELEASE_LOCK(''lock3'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM INFORMATION_SCHEMA.METADATA_LOCK_INFO;'+sLineBreak
        +' '+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| THREAD_ID | LOCK_MODE | LOCK_DURATION | LOCK_TYPE |'+sLineBreak
        +'TABLE_SCHEMA | TABLE_NAME |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +'| 46 | MDL_SHARED_NO_WRITE | NULL | User lock | lock3 | |'+sLineBreak
        +'+-----------+---------------------+---------------+-----------+--------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RELEASE_LOCK(''lock3'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| RELEASE_LOCK(''lock3'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM INFORMATION_SCHEMA.METADATA_LOCK_INFO;'+sLineBreak
        +' '+sLineBreak
        +'Empty set (0.000 sec)'
    ),

    (
      Name:         'SLEEP';
      Declaration:  '(duration)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Sleeps (pauses) for the number of seconds given by the'+sLineBreak
        +'duration argument, then'+sLineBreak
        +'returns 0. If SLEEP() is interrupted, it'+sLineBreak
        +'returns 1. The duration may have a fractional part given in'+sLineBreak
        +'microseconds.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the SLEEP() function are not safe for'+sLineBreak
        +'replication.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SELECT SLEEP(5.5);'+sLineBreak
        +'+------------+'+sLineBreak
        +'| SLEEP(5.5) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------+'+sLineBreak
        +'1 row in set (5.50 sec)'
    ),

    (
      Name:         'UUID';
      Declaration:  '()';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a Universal Unique Identifier (UUID) generated'+sLineBreak
        +'according to "DCE 1.1:'+sLineBreak
        +'Remote Procedure Call" (Appendix A) CAE (Common'+sLineBreak
        +'Applications Environment)'+sLineBreak
        +'Specifications published by The Open Group in October'+sLineBreak
        +'1997 '+sLineBreak
        +'(Document Number C706).'+sLineBreak
        +' '+sLineBreak
        +'A UUID is designed as a number that is globally unique in'+sLineBreak
        +'space and time. Two'+sLineBreak
        +'calls to UUID() are expected to generate two different'+sLineBreak
        +'values, even if these calls are performed on two separate'+sLineBreak
        +'computers that are'+sLineBreak
        +'not connected to each other.'+sLineBreak
        +' '+sLineBreak
        +'A UUID is a 128-bit number represented by a utf8 string of'+sLineBreak
        +'five'+sLineBreak
        +'hexadecimal numbers in aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'+sLineBreak
        +'format:'+sLineBreak
        +'The first three numbers are generated from a timestamp.'+sLineBreak
        +'The fourth number preserves temporal uniqueness in case the'+sLineBreak
        +'timestamp value'+sLineBreak
        +' loses monotonicity (for example, due to daylight saving'+sLineBreak
        +'time).'+sLineBreak
        +'The fifth number is an IEEE 802 node number that provides'+sLineBreak
        +'spatial uniqueness.'+sLineBreak
        +' A random number is substituted if the latter is not'+sLineBreak
        +'available (for example,'+sLineBreak
        +' because the host computer has no Ethernet card, or we do'+sLineBreak
        +'not know how to find'+sLineBreak
        +' the hardware address of an interface on your operating'+sLineBreak
        +'system). In this case,'+sLineBreak
        +' spatial uniqueness cannot be guaranteed. Nevertheless, a'+sLineBreak
        +'collision should'+sLineBreak
        +' have very low probability.'+sLineBreak
        +' '+sLineBreak
        +'Currently, the MAC address of an interface is taken into'+sLineBreak
        +'account only on FreeBSD and Linux. On other operating'+sLineBreak
        +'systems, MariaDB uses a randomly generated 48-bit number.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the UUID() function are not safe for'+sLineBreak
        +'replication.'+sLineBreak
        +' '+sLineBreak
        +'UUID() results are intended to be unique, but cannot always'+sLineBreak
        +'be relied upon to unpredictable and unguessable, so should'+sLineBreak
        +'not be relied upon for these purposes.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT UUID();'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| UUID() |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| cd41294a-afb0-11df-bc9b-00241dd75637 |'+sLineBreak
        +'+--------------------------------------+'
    ),

    (
      Name:         'UUID_SHORT';
      Declaration:  '()';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a "short" universal identifier as a 64-bit'+sLineBreak
        +'unsigned integer (rather'+sLineBreak
        +'than a string-form 128-bit identifier as returned by the'+sLineBreak
        +'UUID() function).'+sLineBreak
        +' '+sLineBreak
        +'The value of UUID_SHORT() is guaranteed to be unique if the'+sLineBreak
        +'following conditions hold:'+sLineBreak
        +'The server_id of the current host is unique among your set'+sLineBreak
        +'of master and'+sLineBreak
        +' slave servers'+sLineBreak
        +'server_id is between 0 and 255'+sLineBreak
        +'You don''t set back your system time for your server between'+sLineBreak
        +'mysqld restarts'+sLineBreak
        +'You do not invoke UUID_SHORT() on average more than 16'+sLineBreak
        +' million times per second between mysqld restarts'+sLineBreak
        +' '+sLineBreak
        +'The UUID_SHORT() return value is constructed this way:'+sLineBreak
        +' '+sLineBreak
        +' (server_id & 255)'
    ),

    (
      Name:         'ABS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the absolute (non-negative) value of X. If X is not'+sLineBreak
        +'a number, it is converted to a numeric type.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ABS(42);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| ABS(42) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 42 |'+sLineBreak
        +'+---------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ABS(-42);'+sLineBreak
        +'+----------+'+sLineBreak
        +'| ABS(-42) |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| 42 |'+sLineBreak
        +'+----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ABS(DATE ''1994-01-01'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| ABS(DATE ''1994-01-01'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 19940101 |'+sLineBreak
        +'+------------------------+'
    ),

    (
      Name:         'ACOS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the arc cosine of X, that is, the value whose cosine'+sLineBreak
        +'is X.'+sLineBreak
        +'Returns NULL if X is not in the range -1 to 1.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ACOS(1);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| ACOS(1) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ACOS(1.0001);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| ACOS(1.0001) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ACOS(0);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| ACOS(0) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1.5707963267949 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ACOS(0.234);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| ACOS(0.234) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1.33460644244679 |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'ASIN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the arc sine of X, that is, the value whose sine is'+sLineBreak
        +'X. Returns'+sLineBreak
        +'NULL if X is not in the range -1 to 1.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ASIN(0.2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| ASIN(0.2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 0.2013579207903308 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASIN(''foo'');'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ASIN(''foo'') |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SHOW WARNINGS;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------+-----------------------------------------+'+sLineBreak
        +'| Level | Code | Message |'+sLineBreak
        +'+---------+------+-----------------------------------------+'+sLineBreak
        +'| Warning | 1292 | Truncated incorrect DOUBLE value: ''foo'''+sLineBreak
        +'|'+sLineBreak
        +'+---------+------+-----------------------------------------+'
    ),

    (
      Name:         'ATAN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the arc tangent of X, that is, the value whose'+sLineBreak
        +'tangent is X.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ATAN(2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| ATAN(2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 1.1071487177940904 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ATAN(-2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| ATAN(-2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| -1.1071487177940904 |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'CEIL';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CEIL() is a synonym for CEILING().'
    ),

    (
      Name:         'CEILING';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the smallest integer value not less than X.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CEILING(1.23);'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| CEILING(1.23) |'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CEILING(-1.23);'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| CEILING(-1.23) |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| -1 |'+sLineBreak
        +'+----------------+'
    ),

    (
      Name:         'CONV';
      Declaration:  '(N,from_base,to_base)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Converts numbers between different number bases. Returns a'+sLineBreak
        +'string'+sLineBreak
        +'representation of the number N, converted from base'+sLineBreak
        +'from_base'+sLineBreak
        +'to base to_base.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if any argument is NULL, or if the second or'+sLineBreak
        +'third argument are not in the allowed range.'+sLineBreak
        +' '+sLineBreak
        +'The argument N is interpreted as an integer, but may be'+sLineBreak
        +'specified as an'+sLineBreak
        +'integer or a string. The minimum base is 2 and the maximum'+sLineBreak
        +'base is 36. If'+sLineBreak
        +'to_base is a negative number, N is regarded as a signed'+sLineBreak
        +'number.'+sLineBreak
        +'Otherwise, N is treated as unsigned. CONV() works with'+sLineBreak
        +'64-bit'+sLineBreak
        +'precision.'+sLineBreak
        +' '+sLineBreak
        +'Some shortcuts for this function are also available: BIN(),'+sLineBreak
        +'OCT(), HEX(), UNHEX(). Also, MariaDB allows binary literal'+sLineBreak
        +'values and hexadecimal literal values.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CONV(''a'',16,2);'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| CONV(''a'',16,2) |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| 1010 |'+sLineBreak
        +'+----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONV(''6E'',18,8);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| CONV(''6E'',18,8) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 172 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONV(-17,10,-18);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| CONV(-17,10,-18) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| -H |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONV(12+''10''+''10''+0xa,10,10);'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| CONV(12+''10''+''10''+0xa,10,10) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| 42 |'+sLineBreak
        +'+------------------------------+'
    ),

    (
      Name:         'COS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the cosine of X, where X is given in radians.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT COS(PI());'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| COS(PI()) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| -1 |'+sLineBreak
        +'+-----------'
    ),

    (
      Name:         'COT';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the cotangent of X.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT COT(42);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| COT(42) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 0.4364167060752729 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT COT(12);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| COT(12) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| -1.5726734063976893 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT COT(0);'+sLineBreak
        +'ERROR 1690 (22003): DOUBLE value is out of range in'+sLineBreak
        +'''cot(0)'''
    ),

    (
      Name:         'CRC32';
      Declaration:  '(expr)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Computes a cyclic redundancy check value and returns a'+sLineBreak
        +'32-bit unsigned'+sLineBreak
        +'value. The result is NULL if the argument is NULL. The'+sLineBreak
        +'argument is'+sLineBreak
        +'expected to be a string and (if possible) is treated as one'+sLineBreak
        +'if it is'+sLineBreak
        +'not.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CRC32(''MariaDB'');'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| CRC32(''MariaDB'') |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 4227209140 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CRC32(''mariadb'');'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| CRC32(''mariadb'') |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 2594253378 |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'DEGREES';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the argument X, converted from radians to degrees.'+sLineBreak
        +' '+sLineBreak
        +'This is the converse of the RADIANS() function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT DEGREES(PI());'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| DEGREES(PI()) |'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 180 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DEGREES(PI() / 2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| DEGREES(PI() / 2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 90 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT DEGREES(45);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| DEGREES(45) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 2578.3100780887 |'+sLineBreak
        +'+-----------------+'
    ),

    (
      Name:         'EXP';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the value of e (the base of natural logarithms)'+sLineBreak
        +'raised to the'+sLineBreak
        +'power of X. The inverse of this function is LOG() (using a'+sLineBreak
        +'single'+sLineBreak
        +'argument only) or LN().'+sLineBreak
        +' '+sLineBreak
        +'If X is NULL, this function returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT EXP(2);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| EXP(2) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 7.38905609893065 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXP(-2);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| EXP(-2) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 0.1353352832366127 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXP(0);'+sLineBreak
        +'+--------+'+sLineBreak
        +'| EXP(0) |'+sLineBreak
        +'+--------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXP(NULL);'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| EXP(NULL) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+-----------+'
    ),

    (
      Name:         'FLOOR';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the largest integer value not greater than X.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT FLOOR(1.23);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| FLOOR(1.23) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FLOOR(-1.23);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| FLOOR(-1.23) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| -2 |'+sLineBreak
        +'+--------------+'
    ),

    (
      Name:         'LN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the natural logarithm of X; that is, the base-e'+sLineBreak
        +'logarithm of X.'+sLineBreak
        +'If X is less than or equal to 0, or NULL, then NULL is'+sLineBreak
        +'returned.'+sLineBreak
        +' '+sLineBreak
        +'The inverse of this function is EXP().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LN(2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| LN(2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 0.693147180559945 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LN(-2);'+sLineBreak
        +'+--------+'+sLineBreak
        +'| LN(-2) |'+sLineBreak
        +'+--------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+--------+'
    ),

    (
      Name:         'LOG';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If called with one parameter, this function returns the'+sLineBreak
        +'natural'+sLineBreak
        +'logarithm of X. If X is less than or equal to 0, then NULL'+sLineBreak
        +'is'+sLineBreak
        +'returned.'+sLineBreak
        +' '+sLineBreak
        +'If called with two parameters, it returns the logarithm of X'+sLineBreak
        +'to the base B. If B is'
    ),

    (
      Name:         'LOG';
      Declaration:  '(B,X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If called with one parameter, this function returns the'+sLineBreak
        +'natural'+sLineBreak
        +'logarithm of X. If X is less than or equal to 0, then NULL'+sLineBreak
        +'is'+sLineBreak
        +'returned.'+sLineBreak
        +' '+sLineBreak
        +'If called with two parameters, it returns the logarithm of X'+sLineBreak
        +'to the base B. If B is'
    ),

    (
      Name:         'LOG10';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the base-10 logarithm of X.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LOG10(2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| LOG10(2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 0.301029995663981 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOG10(100);'+sLineBreak
        +'+------------+'+sLineBreak
        +'| LOG10(100) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOG10(-100);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| LOG10(-100) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+-------------+'
    ),

    (
      Name:         'LOG2';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the base-2 logarithm of X.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LOG2(4398046511104);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| LOG2(4398046511104) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| 42 |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOG2(65536);'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| LOG2(65536) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 16 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOG2(-100);'+sLineBreak
        +'+------------+'+sLineBreak
        +'| LOG2(-100) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+------------+'
    ),

    (
      Name:         'MOD';
      Declaration:  '(N,M)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Modulo operation. Returns the remainder of N divided by M.'+sLineBreak
        +'See also Modulo Operator.'+sLineBreak
        +' '+sLineBreak
        +'If the ERROR_ON_DIVISION_BY_ZERO SQL_MODE is used, any'+sLineBreak
        +'number modulus zero produces an error. Otherwise, it returns'+sLineBreak
        +'NULL.'+sLineBreak
        +' '+sLineBreak
        +'The integer part of a division can be obtained using DIV.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT 1042 % 50;'+sLineBreak
        +' '+sLineBreak
        +'+-----------+'+sLineBreak
        +'| 1042 % 50 |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| 42 |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MOD(234, 10);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| MOD(234, 10) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT 253 % 7;'+sLineBreak
        +' '+sLineBreak
        +'+---------+'+sLineBreak
        +'| 253 % 7 |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MOD(29,9);'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| MOD(29,9) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT 29 MOD 9;'+sLineBreak
        +' '+sLineBreak
        +'+----------+'+sLineBreak
        +'| 29 MOD 9 |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+----------+'
    ),

    (
      Name:         'OCT';
      Declaration:  '(N)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string representation of the octal value of N,'+sLineBreak
        +'where N is a longlong (BIGINT) number. This is equivalent to'+sLineBreak
        +'CONV(N,10,8). Returns NULL if N is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT OCT(34);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| OCT(34) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 42 |'+sLineBreak
        +'+---------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT OCT(12);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| OCT(12) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 14 |'+sLineBreak
        +'+---------+'
    ),

    (
      Name:         'PI';
      Declaration:  '()';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the value of ? (pi). The default number of decimal'+sLineBreak
        +'places'+sLineBreak
        +'displayed is six, but MariaDB uses the full double-precision'+sLineBreak
        +'value'+sLineBreak
        +'internally.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT PI();'+sLineBreak
        +'+----------+'+sLineBreak
        +'| PI() |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| 3.141593 |'+sLineBreak
        +'+----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT PI()+0.0000000000000000000000;'+sLineBreak
        +' '+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| PI()+0.0000000000000000000000 |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 3.1415926535897931159980 |'+sLineBreak
        +'+-------------------------------+'
    ),

    (
      Name:         'POW';
      Declaration:  '(X,Y)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the value of X raised to the power of Y.'+sLineBreak
        +' '+sLineBreak
        +'POWER() is a synonym.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT POW(2,3);'+sLineBreak
        +'+----------+'+sLineBreak
        +'| POW(2,3) |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| 8 |'+sLineBreak
        +'+----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT POW(2,-2);'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| POW(2,-2) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| 0.25 |'+sLineBreak
        +'+-----------+'
    ),

    (
      Name:         'POWER';
      Declaration:  '(X,Y)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This is a synonym for POW(), which returns the value of X'+sLineBreak
        +'raised to the power of Y.'
    ),

    (
      Name:         'RADIANS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the argument X, converted from degrees to radians.'+sLineBreak
        +'Note that'+sLineBreak
        +'? radians equals 180 degrees. '+sLineBreak
        +' '+sLineBreak
        +'This is the converse of the DEGREES() function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT RADIANS(45);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| RADIANS(45) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 0.785398163397448 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RADIANS(90);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| RADIANS(90) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1.5707963267949 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RADIANS(PI());'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| RADIANS(PI()) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 0.0548311355616075 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RADIANS(180);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| RADIANS(180) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 3.14159265358979 |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'RAND';
      Declaration:  '()';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a random DOUBLE precision floating point value v in'+sLineBreak
        +'the range 0'
    ),

    (
      Name:         'RAND';
      Declaration:  '(N)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a random DOUBLE precision floating point value v in'+sLineBreak
        +'the range 0'
    ),

    (
      Name:         'ROUND';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Rounds the argument X to D decimal places. The rounding'+sLineBreak
        +'algorithm'+sLineBreak
        +'depends on the data type of X. D defaults to 0 if not'+sLineBreak
        +'specified.'+sLineBreak
        +'D can be negative to cause D digits left of the decimal'+sLineBreak
        +'point of the'+sLineBreak
        +'value X to become zero.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ROUND(-1.23);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| ROUND(-1.23) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| -1 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(-1.58);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| ROUND(-1.58) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| -2 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(1.58); '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ROUND(1.58) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(1.298, 1);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| ROUND(1.298, 1) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1.3 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(1.298, 0);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| ROUND(1.298, 0) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(23.298, -1);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ROUND(23.298, -1) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 20 |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'ROUND';
      Declaration:  '(X,D)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Rounds the argument X to D decimal places. The rounding'+sLineBreak
        +'algorithm'+sLineBreak
        +'depends on the data type of X. D defaults to 0 if not'+sLineBreak
        +'specified.'+sLineBreak
        +'D can be negative to cause D digits left of the decimal'+sLineBreak
        +'point of the'+sLineBreak
        +'value X to become zero.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ROUND(-1.23);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| ROUND(-1.23) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| -1 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(-1.58);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| ROUND(-1.58) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| -2 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(1.58); '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| ROUND(1.58) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(1.298, 1);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| ROUND(1.298, 1) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1.3 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(1.298, 0);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| ROUND(1.298, 0) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(23.298, -1);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ROUND(23.298, -1) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 20 |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'SIGN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sign of the argument as -1, 0, or 1, depending'+sLineBreak
        +'on whether'+sLineBreak
        +'X is negative, zero, or positive.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SIGN(-32);'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| SIGN(-32) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| -1 |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SIGN(0);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| SIGN(0) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SIGN(234);'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| SIGN(234) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-----------+'
    ),

    (
      Name:         'SIN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sine of X, where X is given in radians.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SIN(1.5707963267948966);'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| SIN(1.5707963267948966) |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SIN(PI());'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| SIN(PI()) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 1.22460635382238e-16 |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ROUND(SIN(PI()));'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| ROUND(SIN(PI())) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'SQRT';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the square root of X. If X is negative, NULL is'+sLineBreak
        +'returned.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SQRT(4);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| SQRT(4) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+---------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SQRT(20);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| SQRT(20) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 4.47213595499958 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SQRT(-16);'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| SQRT(-16) |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SQRT(1764);'+sLineBreak
        +'+------------+'+sLineBreak
        +'| SQRT(1764) |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 42 |'+sLineBreak
        +'+------------+'
    ),

    (
      Name:         'TAN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the tangent of X, where X is given in radians.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TAN(0.7853981633974483);'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| TAN(0.7853981633974483) |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 0.9999999999999999 |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TAN(PI());'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| TAN(PI()) |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| -1.22460635382238e-16 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TAN(PI()+1);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| TAN(PI()+1) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| 1.5574077246549 |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TAN(RADIANS(PI()));'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| TAN(RADIANS(PI())) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 0.0548861508080033 |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'TRUNCATE';
      Declaration:  '(X,D)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number X, truncated to D decimal places. If D is'+sLineBreak
        +'0, the'+sLineBreak
        +'result has no decimal point or fractional part. D can be'+sLineBreak
        +'negative to'+sLineBreak
        +'cause D digits left of the decimal point of the value X to'+sLineBreak
        +'become'+sLineBreak
        +'zero.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TRUNCATE(1.223,1);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| TRUNCATE(1.223,1) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 1.2 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRUNCATE(1.999,1);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| TRUNCATE(1.999,1) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 1.9 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRUNCATE(1.999,0); '+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| TRUNCATE(1.999,0) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRUNCATE(-1.999,1);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| TRUNCATE(-1.999,1) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| -1.9 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRUNCATE(122,-2);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| TRUNCATE(122,-2) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 100 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRUNCATE(10.28*100,0);'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| TRUNCATE(10.28*100,0) |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 1028 |'+sLineBreak
        +'+-----------------------+'
    ),

    (
      Name:         'ST_X';
      Declaration:  '(p)';
      Category:     'Point Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the X-coordinate value for the point p as a'+sLineBreak
        +'double-precision number.'+sLineBreak
        +' '+sLineBreak
        +'ST_X() and X() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @pt = ''Point(56.7 53.34)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT X(GeomFromText(@pt));'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| X(GeomFromText(@pt)) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 56.7 |'+sLineBreak
        +'+----------------------+'
    ),

    (
      Name:         'ST_Y';
      Declaration:  '(p)';
      Category:     'Point Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the Y-coordinate value for the point p as a'+sLineBreak
        +'double-precision number.'+sLineBreak
        +' '+sLineBreak
        +'ST_Y() and Y() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @pt = ''Point(56.7 53.34)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT Y(GeomFromText(@pt));'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| Y(GeomFromText(@pt)) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 53.34 |'+sLineBreak
        +'+----------------------+'
    ),

    (
      Name:         'ST_AREA';
      Declaration:  '(poly)';
      Category:     'Polygon Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns as a double-precision number the area of the Polygon'+sLineBreak
        +'value poly, as measured in its spatial reference system.'+sLineBreak
        +' '+sLineBreak
        +'ST_Area() and Area() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @poly = ''Polygon((0 0,0 3,3 0,0 0),(1 1,1 2,2 1,1'+sLineBreak
        +'1))'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT Area(GeomFromText(@poly));'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| Area(GeomFromText(@poly)) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+---------------------------+'
    ),

    (
      Name:         'ST_CENTROID';
      Declaration:  '(mpoly)';
      Category:     'Polygon Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a point reflecting the mathematical centroid'+sLineBreak
        +'(geometric center) for the MultiPolygon mpoly. The resulting'+sLineBreak
        +'point will not necessarily be on the MultiPolygon. '+sLineBreak
        +' '+sLineBreak
        +'ST_Centroid() and Centroid() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @poly = ST_GeomFromText(''POLYGON((0 0,20 0,20 20,0 20,0'+sLineBreak
        +'0))'');'+sLineBreak
        +'SELECT ST_AsText(ST_Centroid(@poly)) AS center;'+sLineBreak
        +' '+sLineBreak
        +'+--------------+'+sLineBreak
        +'| center |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| POINT(10 10) |'+sLineBreak
        +'+--------------+'
    ),

    (
      Name:         'ST_ExteriorRing';
      Declaration:  '(poly)';
      Category:     'Polygon Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the exterior ring of the Polygon value poly as a'+sLineBreak
        +'LineString.'+sLineBreak
        +' '+sLineBreak
        +'ST_ExteriorRing() and ExteriorRing() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @poly = ''Polygon((0 0,0 3,3 3,3 0,0 0),(1 1,1 2,2 2,2'+sLineBreak
        +'1,1 1))'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT AsText(ExteriorRing(GeomFromText(@poly)));'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| AsText(ExteriorRing(GeomFromText(@poly))) |'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| LINESTRING(0 0,0 3,3 3,3 0,0 0) |'+sLineBreak
        +'+-------------------------------------------+'
    ),

    (
      Name:         'ST_InteriorRingN';
      Declaration:  '(poly,N)';
      Category:     'Polygon Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the N-th interior ring for the Polygon value poly as'+sLineBreak
        +'a LineString. Rings are numbered beginning with 1.'+sLineBreak
        +' '+sLineBreak
        +'ST_InteriorRingN() and InteriorRingN() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @poly = ''Polygon((0 0,0 3,3 3,3 0,0 0),(1 1,1 2,2 2,2'+sLineBreak
        +'1,1 1))'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT AsText(InteriorRingN(GeomFromText(@poly),1));'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| AsText(InteriorRingN(GeomFromText(@poly),1)) |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| LINESTRING(1 1,1 2,2 2,2 1,1 1) |'+sLineBreak
        +'+----------------------------------------------+'
    ),

    (
      Name:         'ST_NumInteriorRings';
      Declaration:  '(poly)';
      Category:     'Polygon Properties';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns an integer containing the number of interior rings'+sLineBreak
        +'in the Polygon value poly.'+sLineBreak
        +' '+sLineBreak
        +'Note that according the the OpenGIS standard, a POLYGON'+sLineBreak
        +'should have exactly one ExteriorRing and all other rings'+sLineBreak
        +'should lie within that ExteriorRing and thus be the'+sLineBreak
        +'InteriorRings. Practically, however, some systems, including'+sLineBreak
        +'MariaDB''s, permit polygons to have several'+sLineBreak
        +'''ExteriorRings''. In the case of there being multiple,'+sLineBreak
        +'non-overlapping exterior rings ST_NumInteriorRings() will'+sLineBreak
        +'return 1.'+sLineBreak
        +' '+sLineBreak
        +'ST_NumInteriorRings() and NumInteriorRings() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @poly = ''Polygon((0 0,0 3,3 3,3 0,0 0),(1 1,1 2,2 2,2'+sLineBreak
        +'1,1 1))'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT NumInteriorRings(GeomFromText(@poly));'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| NumInteriorRings(GeomFromText(@poly)) |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+---------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Non-overlapping ''polygon'':'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_NumInteriorRings(ST_PolyFromText(''POLYGON((0 0,10'+sLineBreak
        +'0,10 10,0 10,0 0),'+sLineBreak
        +' (-1 -1,-5 -1,-5 -5,-1 -5,-1 -1))'')) AS NumInteriorRings;'+sLineBreak
        +' '+sLineBreak
        +'+------------------+'+sLineBreak
        +'| NumInteriorRings |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'SPIDER_BG_DIRECT_SQL';
      Declaration:  '(''sql'', ''tmp_table_list'', ''parameters'')';
      Category:     'Spider Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Executes the given SQL statement in the background on the'+sLineBreak
        +'remote server, as defined in the parameters listing. If the'+sLineBreak
        +'query returns a result-set, it sttores the results in the'+sLineBreak
        +'given temporary table. When the given SQL statement executes'+sLineBreak
        +'successfully, this function returns the number of called'+sLineBreak
        +'UDF''s. It returns 0 when the given SQL statement fails.'+sLineBreak
        +' '+sLineBreak
        +'This function is a UDF installed with the Spider storage'+sLineBreak
        +'engine.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SPIDER_BG_DIRECT_SQL(''SELECT * FROM example_table'','+sLineBreak
        +''''', '+sLineBreak
        +' ''srv "node1", port "8607"'') AS "Direct Query";'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| Direct Query | '+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'Parameters'+sLineBreak
        +' '+sLineBreak
        +'error_rw_mode'+sLineBreak
        +' '+sLineBreak
        +'Description: Returns empty results on network error.'+sLineBreak
        +'0 : Return error on getting network error.'+sLineBreak
        +'1: Return 0 records on getting network error.'+sLineBreak
        +' '+sLineBreak
        +'Default Table Value: 0'+sLineBreak
        +'DSN Parameter Name: erwm'
    ),

    (
      Name:         'SPIDER_COPY_TABLES';
      Declaration:  '(spider_table_name,   source_link_id, destination_link_id_list [,parameters])';
      Category:     'Spider Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'A UDF installed with the Spider Storage Engine, this'+sLineBreak
        +'function copies table data from source_link_id to'+sLineBreak
        +'destination_link_id_list. The service does not need to be'+sLineBreak
        +'stopped in order to copy.'+sLineBreak
        +' '+sLineBreak
        +'If the Spider table is partitioned, the name must be of the'+sLineBreak
        +'format table_name#P#partition_name. The partition name can'+sLineBreak
        +'be viewed in the mysql.spider_tables table, for example:'+sLineBreak
        +' '+sLineBreak
        +'SELECT table_name FROM mysql.spider_tables;'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| table_name |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| spt_a#P#pt1 |'+sLineBreak
        +'| spt_a#P#pt2 |'+sLineBreak
        +'| spt_a#P#pt3 |'+sLineBreak
        +'+-------------+'+sLineBreak
        +' '+sLineBreak
        +'Returns 1 if the data was copied successfully, or 0 if'+sLineBreak
        +'copying the data failed.'
    ),

    (
      Name:         'SPIDER_DIRECT_SQL';
      Declaration:  '(''sql'', ''tmp_table_list'', ''parameters'')';
      Category:     'Spider Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'A UDF installed with the Spider Storage Engine, this'+sLineBreak
        +'function is used to execute the SQL string sql on the remote'+sLineBreak
        +'server, as defined in parameters. If any resultsets are'+sLineBreak
        +'returned, they are stored in the tmp_table_list.'+sLineBreak
        +' '+sLineBreak
        +'The function returns 1 if the SQL executes successfully, or'+sLineBreak
        +'0 if it fails.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SPIDER_DIRECT_SQL(''SELECT * FROM s'', '''', ''srv'+sLineBreak
        +'"node1", port "8607"'');'+sLineBreak
        +'+----------------------------------------------------------------------+'+sLineBreak
        +'| SPIDER_DIRECT_SQL(''SELECT * FROM s'', '''', ''srv'+sLineBreak
        +'"node1", port "8607"'') |'+sLineBreak
        +'+----------------------------------------------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+----------------------------------------------------------------------+'
    ),

    (
      Name:         'SPIDER_FLUSH_TABLE_MON_CACHE';
      Declaration:  '()';
      Category:     'Spider Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'A UDF installed with the Spider Storage Engine, this'+sLineBreak
        +'function is used for refreshing monitoring server'+sLineBreak
        +'information. It returns a value of 1.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SPIDER_FLUSH_TABLE_MON_CACHE();'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SPIDER_FLUSH_TABLE_MON_CACHE() |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------------------------------+'
    ),

    (
      Name:         'ASCII';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the numeric ASCII value of the leftmost character of'+sLineBreak
        +'the string argument. Returns 0 if the given string is empty'+sLineBreak
        +'and NULL if it is NULL.'+sLineBreak
        +' '+sLineBreak
        +'ASCII() works for 8-bit characters.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ASCII(9);'+sLineBreak
        +'+----------+'+sLineBreak
        +'| ASCII(9) |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| 57 |'+sLineBreak
        +'+----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASCII(''9'');'+sLineBreak
        +'+------------+'+sLineBreak
        +'| ASCII(''9'') |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| 57 |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ASCII(''abc'');'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| ASCII(''abc'') |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 97 |'+sLineBreak
        +'+--------------+'
    ),

    (
      Name:         'BIN';
      Declaration:  '(N)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string representation of the binary value of the'+sLineBreak
        +'given longlong (that is, BIGINT) number. This is equivalent'+sLineBreak
        +'to CONV(N,10,2). The argument should be positive. If it is a'+sLineBreak
        +'FLOAT, it will be truncated. Returns NULL if the argument is'+sLineBreak
        +'NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT BIN(12);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| BIN(12) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| 1100 |'+sLineBreak
        +'+---------+'
    ),

    (
      Name:         'BIT_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length of the given string argument in bits. If'+sLineBreak
        +'the argument is not a string, it will be converted to'+sLineBreak
        +'string. If the argument is NULL, it returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT BIT_LENGTH(''text'');'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| BIT_LENGTH(''text'') |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 32 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT BIT_LENGTH('''');'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| BIT_LENGTH('''') |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------+'+sLineBreak
        +' '+sLineBreak
        +'Compatibility'+sLineBreak
        +' '+sLineBreak
        +'PostgreSQL and Sybase support BIT_LENGTH().'
    ),

    (
      Name:         'CAST';
      Declaration:  '(expr AS type)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The CAST() function takes a value of one type and produces a'+sLineBreak
        +'value of another type, similar to the CONVERT() function.'+sLineBreak
        +'For more information, see the description of CONVERT(). '+sLineBreak
        +' '+sLineBreak
        +'The main difference between the CAST() and CONVERT() is that'+sLineBreak
        +'CONVERT(expr,type) is ODBC syntax while CAST(expr as type)'+sLineBreak
        +'and CONVERT(... USING ...) are SQL92 syntax.'+sLineBreak
        +' '+sLineBreak
        +'In MariaDB 10.4 and later, you can use the CAST() function'+sLineBreak
        +'with the INTERVAL keyword.'+sLineBreak
        +' '+sLineBreak
        +'Until MariaDB 5.5.31, X''HHHH'', the standard SQL syntax for'+sLineBreak
        +'binary string literals, erroneously worked in the same way'+sLineBreak
        +'as 0xHHHH. In 5.5.31 it was intentionally changed to behave'+sLineBreak
        +'as a string in all contexts (and never as a number).'+sLineBreak
        +' '+sLineBreak
        +'This introduces an incompatibility with previous versions of'+sLineBreak
        +'MariaDB, and all versions of MySQL (see the example below). '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'Simple casts:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CAST("abc" AS BINARY);'+sLineBreak
        +'SELECT CAST("1" AS UNSIGNED INTEGER);'+sLineBreak
        +'SELECT CAST(123 AS CHAR CHARACTER SET utf8)'+sLineBreak
        +' '+sLineBreak
        +'Note that when one casts to CHAR without specifying the'+sLineBreak
        +'character set, the collation_connection character set'+sLineBreak
        +'collation will be used. When used with CHAR CHARACTER SET,'+sLineBreak
        +'the default collation for that character set will be used.'+sLineBreak
        +' '+sLineBreak
        +'SELECT COLLATION(CAST(123 AS CHAR));'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| COLLATION(CAST(123 AS CHAR)) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| latin1_swedish_ci |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT COLLATION(CAST(123 AS CHAR CHARACTER SET utf8));'+sLineBreak
        +'+-------------------------------------------------+'+sLineBreak
        +'| COLLATION(CAST(123 AS CHAR CHARACTER SET utf8)) |'+sLineBreak
        +'+-------------------------------------------------+'+sLineBreak
        +'| utf8_general_ci |'+sLineBreak
        +'+-------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'If you also want to change the collation, you have to use'+sLineBreak
        +'the COLLATE operator:'+sLineBreak
        +' '+sLineBreak
        +'SELECT COLLATION(CAST(123 AS CHAR CHARACTER SET utf8) '+sLineBreak
        +' COLLATE utf8_unicode_ci);'+sLineBreak
        +'+-------------------------------------------------------------------------+'+sLineBreak
        +'| COLLATION(CAST(123 AS CHAR CHARACTER SET utf8) COLLATE'+sLineBreak
        +'utf8_unicode_ci) |'+sLineBreak
        +'+-------------------------------------------------------------------------+'+sLineBreak
        +'| utf8_unicode_ci |'+sLineBreak
        +'+-------------------------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Using CAST() to order an ENUM field as a CHAR rather than'+sLineBreak
        +'the internal numerical value:'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE enum_list (enum_field enum(''c'',''a'',''b''));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO enum_list (enum_field) '+sLineBreak
        +'VALUES(''c''),(''a''),(''c''),(''b'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM enum_list '+sLineBreak
        +'ORDER BY enum_field;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| enum_field |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| c |'+sLineBreak
        +'| c |'+sLineBreak
        +'| a |'+sLineBreak
        +'| b |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT * FROM enum_list '+sLineBreak
        +'ORDER BY CAST(enum_field AS CHAR);'+sLineBreak
        +'+------------+'+sLineBreak
        +'| enum_field |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| a |'+sLineBreak
        +'| b |'+sLineBreak
        +'| c |'+sLineBreak
        +'| c |'+sLineBreak
        +'+------------+'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 5.5.31, the following will trigger warnings,'+sLineBreak
        +'since x''aa'' and ''X''aa'' no longer behave as a number.'+sLineBreak
        +'Previously, and in all versions of MySQL, no warnings are'+sLineBreak
        +'triggered since they did erroneously behave as a number:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CAST(0xAA AS UNSIGNED), CAST(x''aa'' AS UNSIGNED),'+sLineBreak
        +'CAST(X''aa'' AS UNSIGNED);'+sLineBreak
        +'+------------------------+-------------------------+-------------------------+'+sLineBreak
        +'| CAST(0xAA AS UNSIGNED) | CAST(x''aa'' AS UNSIGNED) |'+sLineBreak
        +'CAST(X''aa'' AS UNSIGNED) |'+sLineBreak
        +'+------------------------+-------------------------+-------------------------+'+sLineBreak
        +'| 170 | 0 | 0 |'+sLineBreak
        +'+------------------------+-------------------------+-------------------------+'+sLineBreak
        +'1 row in set, 2 warnings (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'Warning (Code 1292): Truncated incorrect INTEGER value:'+sLineBreak
        +'''\xAA'''+sLineBreak
        +'Warning (Code 1292): Truncated incorrect INTEGER value:'+sLineBreak
        +'''\xAA'''+sLineBreak
        +' '+sLineBreak
        +'Casting to intervals:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CAST(2019-01-04 INTERVAL AS DAY_SECOND(2)) AS'+sLineBreak
        +'"Cast";'+sLineBreak
        +' '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| Cast |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| 00:20:17.00 |'+sLineBreak
        +'+-------------+'
    ),

    (
      Name:         'CHARACTER_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CHARACTER_LENGTH() is a synonym for CHAR_LENGTH().'
    ),

    (
      Name:         'CHAR_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length of the given string argument, measured in'+sLineBreak
        +'characters. A multi-byte character counts as a single'+sLineBreak
        +'character. This means that for a string containing five'+sLineBreak
        +'two-byte characters, LENGTH() (or OCTET_LENGTH() in Oracle'+sLineBreak
        +'mode) returns 10, whereas CHAR_LENGTH() returns 5. If the'+sLineBreak
        +'argument is NULL, it returns NULL. '+sLineBreak
        +' '+sLineBreak
        +'If the argument is not a string value, it is converted into'+sLineBreak
        +'a string.'+sLineBreak
        +' '+sLineBreak
        +'It is synonymous with the CHARACTER_LENGTH() function.'+sLineBreak
        +' '+sLineBreak
        +'Until MariaDB 10.3.1, returns MYSQL_TYPE_LONGLONG, or'+sLineBreak
        +'bigint(10), in all cases. From MariaDB 10.3.1, returns'+sLineBreak
        +'MYSQL_TYPE_LONG, or int(10), when the result would fit'+sLineBreak
        +'within 32-bits.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CHAR_LENGTH(''MariaDB'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| CHAR_LENGTH(''MariaDB'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 7 |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CHAR_LENGTH(''?'');'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| CHAR_LENGTH(''?'') |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'CHR';
      Declaration:  '(N)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CHR() interprets each argument N as an integer and returns a'+sLineBreak
        +'VARCHAR(1) string consisting of the character given by the'+sLineBreak
        +'code values of the integer. The character set and collation'+sLineBreak
        +'of the string are set according to the values of the'+sLineBreak
        +'character_set_database and collation_database system'+sLineBreak
        +'variables.'+sLineBreak
        +' '+sLineBreak
        +'CHR() is similar to the CHAR() function, but only accepts a'+sLineBreak
        +'single argument.'+sLineBreak
        +' '+sLineBreak
        +'CHR() is available in all sql_modes.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CHR(67);'+sLineBreak
        +'+---------+'+sLineBreak
        +'| CHR(67) |'+sLineBreak
        +'+---------+'+sLineBreak
        +'| C |'+sLineBreak
        +'+---------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CHR(''67'');'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| CHR(''67'') |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| C |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CHR(''C'');'+sLineBreak
        +'+----------+'+sLineBreak
        +'| CHR(''C'') |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| |'+sLineBreak
        +'+----------+'+sLineBreak
        +'1 row in set, 1 warning (0.000 sec)'+sLineBreak
        +' '+sLineBreak
        +'SHOW WARNINGS;'+sLineBreak
        +' '+sLineBreak
        +'+---------+------+----------------------------------------+'+sLineBreak
        +'| Level | Code | Message |'+sLineBreak
        +'+---------+------+----------------------------------------+'+sLineBreak
        +'| Warning | 1292 | Truncated incorrect INTEGER value: ''C'''+sLineBreak
        +'|'+sLineBreak
        +'+---------+------+----------------------------------------+'
    ),

    (
      Name:         'CONCAT';
      Declaration:  '(str1,str2,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string that results from concatenating the'+sLineBreak
        +'arguments. May have one or more arguments. If all arguments'+sLineBreak
        +'are non-binary strings, the result is a non-binary string.'+sLineBreak
        +'If the arguments include any binary strings, the result is a'+sLineBreak
        +'binary string. A numeric argument is converted to its'+sLineBreak
        +'equivalent binary string form; if you want to avoid that,'+sLineBreak
        +'you can use an explicit type cast, as in this example:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT(CAST(int_col AS CHAR), char_col);'+sLineBreak
        +' '+sLineBreak
        +'CONCAT() returns NULL if any argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +'A NULL parameter hides all information contained in other'+sLineBreak
        +'parameters from the result. Sometimes this is not desirable;'+sLineBreak
        +'to avoid this, you can:'+sLineBreak
        +'Use the CONCAT_WS() function with an empty separator,'+sLineBreak
        +'because that function is NULL-safe.'+sLineBreak
        +'Use IFNULL() to turn NULLs into empty strings.'+sLineBreak
        +' '+sLineBreak
        +'Oracle Mode'+sLineBreak
        +' '+sLineBreak
        +'In Oracle mode from MariaDB 10.3, CONCAT ignores NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CONCAT(''Ma'', ''ria'', ''DB'');'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| CONCAT(''Ma'', ''ria'', ''DB'') |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| MariaDB |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT(''Ma'', ''ria'', NULL, ''DB'');'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| CONCAT(''Ma'', ''ria'', NULL, ''DB'') |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT(42.0);'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| CONCAT(42.0) |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 42.0 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'Using IFNULL() to handle NULLs:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT(''The value of @v is: '', IFNULL(@v, ''''));'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| CONCAT(''The value of @v is: '', IFNULL(@v, '''')) |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +'| The value of @v is: |'+sLineBreak
        +'+------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'In Oracle mode, from MariaDB 10.3:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT(''Ma'', ''ria'', NULL, ''DB'');'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| CONCAT(''Ma'', ''ria'', NULL, ''DB'') |'+sLineBreak
        +'+---------------------------------+'+sLineBreak
        +'| MariaDB |'+sLineBreak
        +'+---------------------------------+'
    ),

    (
      Name:         'CONCAT_WS';
      Declaration:  '(separator,str1,str2,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CONCAT_WS() stands for Concatenate With Separator and is a'+sLineBreak
        +'special form of CONCAT(). The first argument is the'+sLineBreak
        +'separator for the rest of the arguments. The separator is'+sLineBreak
        +'added between the strings to be concatenated. The separator'+sLineBreak
        +'can be a string, as can the rest of the arguments.'+sLineBreak
        +' '+sLineBreak
        +'If the separator is NULL, the result is NULL; all other NULL'+sLineBreak
        +'values are skipped. This makes CONCAT_WS() suitable when you'+sLineBreak
        +'want to concatenate some values and avoid losing all'+sLineBreak
        +'information if one of them is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT CONCAT_WS('','',''First name'',''Second name'',''Last'+sLineBreak
        +'Name'');'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| CONCAT_WS('','',''First name'',''Second name'',''Last'+sLineBreak
        +'Name'') |'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +'| First name,Second name,Last Name |'+sLineBreak
        +'+-------------------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT_WS(''-'',''Floor'',NULL,''Room'');'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| CONCAT_WS(''-'',''Floor'',NULL,''Room'') |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| Floor-Room |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'In some cases, remember to include a space in the separator'+sLineBreak
        +'string:'+sLineBreak
        +' '+sLineBreak
        +'SET @a = ''gnu'', @b = ''penguin'', @c = ''sea lion'';'+sLineBreak
        +' '+sLineBreak
        +'Query OK, 0 rows affected (0.00 sec)'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT_WS('', '', @a, @b, @c);'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| CONCAT_WS('', '', @a, @b, @c) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| gnu, penguin, sea lion |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Using CONCAT_WS() to handle NULLs:'+sLineBreak
        +' '+sLineBreak
        +'SET @a = ''a'', @b = NULL, @c = ''c'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONCAT_WS('''', @a, @b, @c);'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| CONCAT_WS('''', @a, @b, @c) |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| ac |'+sLineBreak
        +'+---------------------------+'
    ),

    (
      Name:         'CONVERT';
      Declaration:  '(expr,type)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The CONVERT() and CAST() functions take a value of one type'+sLineBreak
        +'and produce a value of another type.'+sLineBreak
        +' '+sLineBreak
        +'The type can be one of the following values:'+sLineBreak
        +'BINARY'+sLineBreak
        +'CHAR'+sLineBreak
        +'DATE'+sLineBreak
        +'DATETIME '+sLineBreak
        +'DECIMAL[(M[,D])]'+sLineBreak
        +'DOUBLE '+sLineBreak
        +'FLOAT נFrom MariaDB 10.4.5'+sLineBreak
        +'INTEGER '+sLineBreak
        +'Short for SIGNED INTEGER'+sLineBreak
        +' '+sLineBreak
        +'SIGNED [INTEGER]'+sLineBreak
        +'TIME '+sLineBreak
        +'UNSIGNED [INTEGER]'+sLineBreak
        +' '+sLineBreak
        +'Note that in MariaDB, INT and INTEGER are the same thing.'+sLineBreak
        +' '+sLineBreak
        +'BINARY produces a string with the BINARY data type. If the'+sLineBreak
        +'optional length is given, BINARY(N) causes the cast to use'+sLineBreak
        +'no more than N bytes of the argument. Values shorter than'+sLineBreak
        +'the given number in bytes are padded with 0x00 bytes to make'+sLineBreak
        +'them equal the length value.'+sLineBreak
        +' '+sLineBreak
        +'CHAR(N) causes the cast to use no more than the number of'+sLineBreak
        +'characters given in the argument.'+sLineBreak
        +' '+sLineBreak
        +'The main difference between the CAST() and CONVERT() is that'+sLineBreak
        +'CONVERT(expr,type) is ODBC syntax while CAST(expr as type)'+sLineBreak
        +'and CONVERT(... USING ...) are SQL92 syntax.'+sLineBreak
        +' '+sLineBreak
        +'CONVERT() with USING is used to convert data between'+sLineBreak
        +'different character sets. In MariaDB, transcoding names are'+sLineBreak
        +'the same as the'+sLineBreak
        +'corresponding character set names. For example, this'+sLineBreak
        +'statement'+sLineBreak
        +'converts the string ''abc'' in the default character set to'+sLineBreak
        +'the'+sLineBreak
        +'corresponding string in the utf8 character set:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONVERT(''abc'' USING utf8);'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT enum_col FROM tbl_name '+sLineBreak
        +'ORDER BY CAST(enum_col AS CHAR);'+sLineBreak
        +' '+sLineBreak
        +'Converting a BINARY to string to permit the LOWER function'+sLineBreak
        +'to work:'+sLineBreak
        +' '+sLineBreak
        +'SET @x = ''AardVark'';'+sLineBreak
        +' '+sLineBreak
        +'SET @x = BINARY ''AardVark'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOWER(@x), LOWER(CONVERT (@x USING latin1));'+sLineBreak
        +'+-----------+----------------------------------+'+sLineBreak
        +'| LOWER(@x) | LOWER(CONVERT (@x USING latin1)) |'+sLineBreak
        +'+-----------+----------------------------------+'+sLineBreak
        +'| AardVark | aardvark |'+sLineBreak
        +'+-----------+----------------------------------+'
    ),

    (
      Name:         'CONVERT';
      Declaration:  '(expr USING transcoding_name)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The CONVERT() and CAST() functions take a value of one type'+sLineBreak
        +'and produce a value of another type.'+sLineBreak
        +' '+sLineBreak
        +'The type can be one of the following values:'+sLineBreak
        +'BINARY'+sLineBreak
        +'CHAR'+sLineBreak
        +'DATE'+sLineBreak
        +'DATETIME '+sLineBreak
        +'DECIMAL[(M[,D])]'+sLineBreak
        +'DOUBLE '+sLineBreak
        +'FLOAT נFrom MariaDB 10.4.5'+sLineBreak
        +'INTEGER '+sLineBreak
        +'Short for SIGNED INTEGER'+sLineBreak
        +' '+sLineBreak
        +'SIGNED [INTEGER]'+sLineBreak
        +'TIME '+sLineBreak
        +'UNSIGNED [INTEGER]'+sLineBreak
        +' '+sLineBreak
        +'Note that in MariaDB, INT and INTEGER are the same thing.'+sLineBreak
        +' '+sLineBreak
        +'BINARY produces a string with the BINARY data type. If the'+sLineBreak
        +'optional length is given, BINARY(N) causes the cast to use'+sLineBreak
        +'no more than N bytes of the argument. Values shorter than'+sLineBreak
        +'the given number in bytes are padded with 0x00 bytes to make'+sLineBreak
        +'them equal the length value.'+sLineBreak
        +' '+sLineBreak
        +'CHAR(N) causes the cast to use no more than the number of'+sLineBreak
        +'characters given in the argument.'+sLineBreak
        +' '+sLineBreak
        +'The main difference between the CAST() and CONVERT() is that'+sLineBreak
        +'CONVERT(expr,type) is ODBC syntax while CAST(expr as type)'+sLineBreak
        +'and CONVERT(... USING ...) are SQL92 syntax.'+sLineBreak
        +' '+sLineBreak
        +'CONVERT() with USING is used to convert data between'+sLineBreak
        +'different character sets. In MariaDB, transcoding names are'+sLineBreak
        +'the same as the'+sLineBreak
        +'corresponding character set names. For example, this'+sLineBreak
        +'statement'+sLineBreak
        +'converts the string ''abc'' in the default character set to'+sLineBreak
        +'the'+sLineBreak
        +'corresponding string in the utf8 character set:'+sLineBreak
        +' '+sLineBreak
        +'SELECT CONVERT(''abc'' USING utf8);'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT enum_col FROM tbl_name '+sLineBreak
        +'ORDER BY CAST(enum_col AS CHAR);'+sLineBreak
        +' '+sLineBreak
        +'Converting a BINARY to string to permit the LOWER function'+sLineBreak
        +'to work:'+sLineBreak
        +' '+sLineBreak
        +'SET @x = ''AardVark'';'+sLineBreak
        +' '+sLineBreak
        +'SET @x = BINARY ''AardVark'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOWER(@x), LOWER(CONVERT (@x USING latin1));'+sLineBreak
        +'+-----------+----------------------------------+'+sLineBreak
        +'| LOWER(@x) | LOWER(CONVERT (@x USING latin1)) |'+sLineBreak
        +'+-----------+----------------------------------+'+sLineBreak
        +'| AardVark | aardvark |'+sLineBreak
        +'+-----------+----------------------------------+'
    ),

    (
      Name:         'ELT';
      Declaration:  '(N, str1[, str2, str3,...])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Takes a numeric argument and a series of string arguments.'+sLineBreak
        +'Returns the string that corresponds to the given numeric'+sLineBreak
        +'position. For instance, it returns str1 if N is 1, str2 if N'+sLineBreak
        +'is 2, and so on. If the numeric argument is a FLOAT, MariaDB'+sLineBreak
        +'rounds it to the nearest INTEGER. If the numeric argument is'+sLineBreak
        +'less than 1, greater than the total number of arguments, or'+sLineBreak
        +'not a number, ELT() returns NULL. It must have at least two'+sLineBreak
        +'arguments.'+sLineBreak
        +' '+sLineBreak
        +'It is complementary to the FIELD() function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ELT(1, ''ej'', ''Heja'', ''hej'', ''foo'');'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| ELT(1, ''ej'', ''Heja'', ''hej'', ''foo'') |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| ej |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT ELT(4, ''ej'', ''Heja'', ''hej'', ''foo'');'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| ELT(4, ''ej'', ''Heja'', ''hej'', ''foo'') |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| foo |'+sLineBreak
        +'+------------------------------------+'
    ),

    (
      Name:         'EXPORT_SET';
      Declaration:  '(bits, on, off[, separator[, number_of_bits]])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Takes a minimum of three arguments. Returns a string where'+sLineBreak
        +'each bit in the given bits argument is returned, with the'+sLineBreak
        +'string values given for on and off. '+sLineBreak
        +' '+sLineBreak
        +'Bits are examined from right to left, (from low-order to'+sLineBreak
        +'high-order bits). Strings are added to the result from left'+sLineBreak
        +'to right, separated by a separator string (defaults as'+sLineBreak
        +''',''). You can optionally limit the number of bits the'+sLineBreak
        +'EXPORT_SET() function examines using the number_of_bits'+sLineBreak
        +'option. '+sLineBreak
        +' '+sLineBreak
        +'If any of the arguments are set as NULL, the function'+sLineBreak
        +'returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT EXPORT_SET(5,''Y'',''N'','','',4);'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| EXPORT_SET(5,''Y'',''N'','','',4) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| Y,N,Y,N |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXPORT_SET(6,''1'',''0'','','',10);'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| EXPORT_SET(6,''1'',''0'','','',10) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| 0,1,1,0,0,0,0,0,0,0 |'+sLineBreak
        +'+------------------------------+'
    ),

    (
      Name:         'EXTRACTVALUE';
      Declaration:  '(xml_frag, xpath_expr)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The EXTRACTVALUE() function takes two string arguments: a'+sLineBreak
        +'fragment of XML markup and an XPath expression, (also known'+sLineBreak
        +'as a locator). It returns the text (That is, CDDATA), of the'+sLineBreak
        +'first text node which is a child of the element or elements'+sLineBreak
        +'matching the XPath expression. '+sLineBreak
        +' '+sLineBreak
        +'In cases where a valid XPath expression does not match any'+sLineBreak
        +'text nodes in a valid XML fragment, (including the implicit'+sLineBreak
        +'/text() expression), the EXTRACTVALUE() function returns an'+sLineBreak
        +'empty string.'+sLineBreak
        +' '+sLineBreak
        +'Invalid Arguments'+sLineBreak
        +' '+sLineBreak
        +'When either the XML fragment or the XPath expression is'+sLineBreak
        +'NULL, the EXTRACTVALUE() function returns NULL. When the XML'+sLineBreak
        +'fragment is invalid, it raises a warning Code 1525:'+sLineBreak
        +' '+sLineBreak
        +'Warning (Code 1525): Incorrect XML value: ''parse error at'+sLineBreak
        +'line 1 pos 11: unexpected END-OF-INPUT'''+sLineBreak
        +' '+sLineBreak
        +'When the XPath value is invalid, it generates an Error 1105:'+sLineBreak
        +' '+sLineBreak
        +'ERROR 1105 (HY000): XPATH syntax error: '')'''+sLineBreak
        +' '+sLineBreak
        +'Explicit text() Expressions'+sLineBreak
        +' '+sLineBreak
        +'This function is the equivalent of performing a match using'+sLineBreak
        +'the XPath expression after appending /text(). In other'+sLineBreak
        +'words:'+sLineBreak
        +' '+sLineBreak
        +'SELECT'+sLineBreak
        +' EXTRACTVALUE(''example'', ''/cases/case'') AS ''Base'+sLineBreak
        +'Example'','+sLineBreak
        +' EXTRACTVALUE(''example'', ''/cases/case/text()'') AS'+sLineBreak
        +'''text() Example'';'+sLineBreak
        +' '+sLineBreak
        +'+--------------+----------------+'+sLineBreak
        +'| Base Example | text() Example |'+sLineBreak
        +'+--------------+----------------+'+sLineBreak
        +'| example | example |'+sLineBreak
        +'+--------------+----------------+'+sLineBreak
        +' '+sLineBreak
        +'Count Matches'+sLineBreak
        +' '+sLineBreak
        +'When EXTRACTVALUE() returns multiple matches, it returns the'+sLineBreak
        +'content of the first child text node of each matching'+sLineBreak
        +'element, in the matched order, as a single, space-delimited'+sLineBreak
        +'string.'+sLineBreak
        +' '+sLineBreak
        +'By design, the EXTRACTVALUE() function makes no distinction'+sLineBreak
        +'between a match on an empty element and no match at all. If'+sLineBreak
        +'you need to determine whether no matching element was found'+sLineBreak
        +'in the XML fragment or if an element was found that'+sLineBreak
        +'contained no child text nodes, use the XPath count()'+sLineBreak
        +'function. '+sLineBreak
        +' '+sLineBreak
        +'For instance, when looking for a value that exists, but'+sLineBreak
        +'contains no child text nodes, you would get a count of the'+sLineBreak
        +'number of matching instances:'+sLineBreak
        +' '+sLineBreak
        +'SELECT'+sLineBreak
        +' EXTRACTVALUE('''', ''/cases/case'') AS ''Empty Example'','+sLineBreak
        +' EXTRACTVALUE('''', ''/cases/case/count()'') AS ''count()'+sLineBreak
        +'Example'';'+sLineBreak
        +' '+sLineBreak
        +'+---------------+-----------------+'+sLineBreak
        +'| Empty Example | count() Example |'+sLineBreak
        +'+---------------+-----------------+'+sLineBreak
        +'| | 1 |'+sLineBreak
        +'+---------------+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'Alternatively, when looking for a value that doesn''t exist,'+sLineBreak
        +'count() returns 0.'+sLineBreak
        +' '+sLineBreak
        +'SELECT'+sLineBreak
        +' EXTRACTVALUE('''', ''/cases/person'') AS ''No Match'+sLineBreak
        +'Example'','+sLineBreak
        +' EXTRACTVALUE('''', ''/cases/person/count()'') AS ''count()'+sLineBreak
        +'Example'';'+sLineBreak
        +' '+sLineBreak
        +'+------------------+-----------------+'+sLineBreak
        +'| No Match Example | count() Example |'+sLineBreak
        +'+------------------+-----------------+'+sLineBreak
        +'| | 0|'+sLineBreak
        +'+------------------+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'Matches'+sLineBreak
        +' '+sLineBreak
        +'Important: The EXTRACTVALUE() function only returns CDDATA.'+sLineBreak
        +'It does not return tags that the element might contain or'+sLineBreak
        +'the text that these child elements contain.'+sLineBreak
        +' '+sLineBreak
        +'SELECT EXTRACTVALUE(''Personx@example.com'', ''/cases'') AS'+sLineBreak
        +'Case;'+sLineBreak
        +' '+sLineBreak
        +'+--------+'+sLineBreak
        +'| Case |'+sLineBreak
        +'+--------+'+sLineBreak
        +'| Person |'+sLineBreak
        +'+--------+'+sLineBreak
        +' '+sLineBreak
        +'Note, in the above example, while the XPath expression'+sLineBreak
        +'matches to the parent  instance, it does not return the'+sLineBreak
        +'contained  tag or its content.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT'+sLineBreak
        +' ExtractValue(''cccddd'', ''/a'') AS val1,'+sLineBreak
        +' ExtractValue(''cccddd'', ''/a/b'') AS val2,'+sLineBreak
        +' ExtractValue(''cccddd'', ''//b'') AS val3,'+sLineBreak
        +' ExtractValue(''cccddd'', ''/b'') AS val4,'+sLineBreak
        +' ExtractValue(''cccdddeee'', ''//b'') AS val5;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------+------+---------+'+sLineBreak
        +'| val1 | val2 | val3 | val4 | val5 |'+sLineBreak
        +'+------+------+------+------+---------+'+sLineBreak
        +'| ccc | ddd | ddd | | ddd eee |'+sLineBreak
        +'+------+------+------+------+---------+'
    ),

    (
      Name:         'FIELD';
      Declaration:  '(pattern, str1[,str2,...])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the index position of the string or number matching'+sLineBreak
        +'the given pattern. Returns 0 in the event that none of the'+sLineBreak
        +'arguments match the pattern. Raises an Error 1582 if not'+sLineBreak
        +'given at least two arguments.'+sLineBreak
        +' '+sLineBreak
        +'When all arguments given to the FIELD() function are'+sLineBreak
        +'strings, they are treated as case-insensitive. When all the'+sLineBreak
        +'arguments are numbers, they are treated as numbers.'+sLineBreak
        +'Otherwise, they are treated as doubles. '+sLineBreak
        +' '+sLineBreak
        +'If the given pattern occurs more than once, the FIELD()'+sLineBreak
        +'function only returns the index of the first instance. If'+sLineBreak
        +'the given pattern is NULL, the function returns 0, as a NULL'+sLineBreak
        +'pattern always fails to match.'+sLineBreak
        +' '+sLineBreak
        +'This function is complementary to the ELT() function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT FIELD(''ej'', ''Hej'', ''ej'', ''Heja'', ''hej'','+sLineBreak
        +'''foo'') '+sLineBreak
        +' AS ''Field Results'';'+sLineBreak
        +' '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| Field Results | '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FIELD(''fo'', ''Hej'', ''ej'', ''Heja'', ''hej'','+sLineBreak
        +'''foo'')'+sLineBreak
        +' AS ''Field Results'';'+sLineBreak
        +' '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| Field Results | '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FIELD(1, 2, 3, 4, 5, 1) AS ''Field Results'';'+sLineBreak
        +' '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| Field Results |'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 5 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FIELD(NULL, 2, 3) AS ''Field Results'';'+sLineBreak
        +' '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| Field Results |'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+---------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FIELD(''fail'') AS ''Field Results'';'+sLineBreak
        +' '+sLineBreak
        +'Error 1582 (42000): Incorrect parameter count in call'+sLineBreak
        +'to native function ''field'''
    ),

    (
      Name:         'FIND_IN_SET';
      Declaration:  '(pattern, strlist)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the index position where the given pattern occurs in'+sLineBreak
        +'a string list. The first argument is the pattern you want to'+sLineBreak
        +'search for. The second argument is a string containing'+sLineBreak
        +'comma-separated variables. If the second argument is of the'+sLineBreak
        +'SET data-type, the function is optimized to use bit'+sLineBreak
        +'arithmetic.'+sLineBreak
        +' '+sLineBreak
        +'If the pattern does not occur in the string list or if the'+sLineBreak
        +'string list is an empty string, the function returns 0. If'+sLineBreak
        +'either argument is NULL, the function returns NULL. The'+sLineBreak
        +'function does not return the correct result if the pattern'+sLineBreak
        +'contains a comma (",") character.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT FIND_IN_SET(''b'',''a,b,c,d'') AS "Found Results";'+sLineBreak
        +' '+sLineBreak
        +'+---------------+'+sLineBreak
        +'| Found Results |'+sLineBreak
        +'+---------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+---------------+'
    ),

    (
      Name:         'FORMAT';
      Declaration:  '(num, decimal_position[, locale])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Formats the given number for display as a string, adding'+sLineBreak
        +'separators to appropriate position and rounding the results'+sLineBreak
        +'to the given decimal position. For instance, it would format'+sLineBreak
        +'15233.345 to 15,233.35.'+sLineBreak
        +' '+sLineBreak
        +'If the given decimal position is 0, it rounds to return no'+sLineBreak
        +'decimal point or fractional part. You can optionally specify'+sLineBreak
        +'a locale value to format numbers to the pattern appropriate'+sLineBreak
        +'for the given region.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT FORMAT(1234567890.09876543210, 4) AS ''Format'';'+sLineBreak
        +' '+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| Format |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 1,234,567,890.0988 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FORMAT(1234567.89, 4) AS ''Format'';'+sLineBreak
        +' '+sLineBreak
        +'+----------------+'+sLineBreak
        +'| Format |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| 1,234,567.8900 |'+sLineBreak
        +'+----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FORMAT(1234567.89, 0) AS ''Format'';'+sLineBreak
        +' '+sLineBreak
        +'+-----------+'+sLineBreak
        +'| Format |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| 1,234,568 |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FORMAT(123456789,2,''rm_CH'') AS ''Format'';'+sLineBreak
        +' '+sLineBreak
        +'+----------------+'+sLineBreak
        +'| Format |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| 123''456''789,00 |'+sLineBreak
        +'+----------------+'
    ),

    (
      Name:         'FROM_BASE64';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Decodes the given base-64 encode string, returning the'+sLineBreak
        +'result as a binary string. Returns NULL if the given string'+sLineBreak
        +'is NULL or if it''s invalid.'+sLineBreak
        +' '+sLineBreak
        +'It is the reverse of the TO_BASE64 function.'+sLineBreak
        +' '+sLineBreak
        +'There are numerous methods to base-64 encode a string.'+sLineBreak
        +'MariaDB uses the following:'+sLineBreak
        +'It encodes alphabet value 64 as ''+''.'+sLineBreak
        +'It encodes alphabet value 63 as ''/''.'+sLineBreak
        +'It codes output in groups of four printable characters. Each'+sLineBreak
        +'three byte of data encoded uses four characters. If the'+sLineBreak
        +'final group is incomplete, it pads the difference with the'+sLineBreak
        +'''='' character.'+sLineBreak
        +'It divides long output, adding a new line very 76'+sLineBreak
        +'characters.'+sLineBreak
        +'In decoding, it recognizes and ignores newlines, carriage'+sLineBreak
        +'returns, tabs and space whitespace characters.'+sLineBreak
        +' '+sLineBreak
        +'SELECT TO_BASE64(''Maria'') AS ''Input'';'+sLineBreak
        +' '+sLineBreak
        +'+-----------+'+sLineBreak
        +'| Input |'+sLineBreak
        +'+-----------+'+sLineBreak
        +'| TWFyaWE= |'+sLineBreak
        +'+-----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT FROM_BASE64(''TWFyaWE='') AS ''Output'';'+sLineBreak
        +' '+sLineBreak
        +'+--------+'+sLineBreak
        +'| Output |'+sLineBreak
        +'+--------+'+sLineBreak
        +'| Maria |'+sLineBreak
        +'+--------+'
    ),

    (
      Name:         'HEX';
      Declaration:  '(N_or_S)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If N_or_S is a number, returns a string representation of'+sLineBreak
        +'the hexadecimal'+sLineBreak
        +'value of N, where N is a longlong (BIGINT) number. This is'+sLineBreak
        +'equivalent to CONV(N,10,16).'+sLineBreak
        +' '+sLineBreak
        +'If N_or_S is a string, returns a hexadecimal string'+sLineBreak
        +'representation of'+sLineBreak
        +'N_or_S where each byte of each character in N_or_S is'+sLineBreak
        +'converted to two hexadecimal'+sLineBreak
        +'digits. If N_or_S is NULL, returns NULL. The inverse of this'+sLineBreak
        +'operation is performed by the UNHEX()'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT HEX(255);'+sLineBreak
        +'+----------+'+sLineBreak
        +'| HEX(255) |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| FF |'+sLineBreak
        +'+----------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT 0x4D617269614442;'+sLineBreak
        +' '+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 0x4D617269614442 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| MariaDB |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(''MariaDB'');'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| HEX(''MariaDB'') |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| 4D617269614442 |'+sLineBreak
        +'+----------------+'
    ),

    (
      Name:         'INSTR';
      Declaration:  '(str,substr)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the position of the first occurrence of substring'+sLineBreak
        +'substr in'+sLineBreak
        +'string str. This is the same as the two-argument form of'+sLineBreak
        +'LOCATE(),'+sLineBreak
        +'except that the order of the arguments is reversed.'+sLineBreak
        +' '+sLineBreak
        +'INSTR() performs a case-insensitive search.'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT INSTR(''foobarbar'', ''bar'');'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| INSTR(''foobarbar'', ''bar'') |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+---------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT INSTR(''My'', ''Maria'');'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| INSTR(''My'', ''Maria'') |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+----------------------+'
    ),

    (
      Name:         'LCASE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LCASE() is a synonym for LOWER().'
    ),

    (
      Name:         'LEFT';
      Declaration:  '(str,len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the leftmost len characters from the string str, or'+sLineBreak
        +'NULL if'+sLineBreak
        +'any argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LEFT(''MariaDB'', 5);'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| LEFT(''MariaDB'', 5) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| Maria |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length of the string str, measured in bytes. A'+sLineBreak
        +'multi-byte'+sLineBreak
        +'character counts as multiple bytes. This means that for a'+sLineBreak
        +'string'+sLineBreak
        +'containing five two-byte characters, LENGTH() returns 10,'+sLineBreak
        +'whereas'+sLineBreak
        +'CHAR_LENGTH() returns 5. '+sLineBreak
        +' '+sLineBreak
        +'If str is not a string value, it is converted into a string.'+sLineBreak
        +'If str is NULL, the function returns NULL.'+sLineBreak
        +' '+sLineBreak
        +'Until MariaDB 10.3.1, returns MYSQL_TYPE_LONGLONG, or'+sLineBreak
        +'bigint(10), in all cases. From MariaDB 10.3.1, returns'+sLineBreak
        +'MYSQL_TYPE_LONG, or int(10), when the result would fit'+sLineBreak
        +'within 32-bits.'+sLineBreak
        +' '+sLineBreak
        +'Oracle Mode'+sLineBreak
        +' '+sLineBreak
        +'When running Oracle mode from MariaDB 10.3, LENGTH() is a'+sLineBreak
        +'synonym for CHAR_LENGTH().'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LENGTH(''MariaDB'');'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| LENGTH(''MariaDB'') |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| 7 |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LENGTH(''?'');'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| LENGTH(''?'') |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 2 |'+sLineBreak
        +'+--------------+'+sLineBreak
        +' '+sLineBreak
        +'In Oracle mode from MariaDB 10.3:'+sLineBreak
        +' '+sLineBreak
        +'SELECT LENGTH(''?'');'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| LENGTH(''?'') |'+sLineBreak
        +'+--------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+--------------+'
    ),

    (
      Name:         'LENGTHB';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LENGTHB() is a synonym for LENGTH().'
    ),

    (
      Name:         'LOAD_FILE';
      Declaration:  '(file_name)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Reads the file and returns the file contents as a string. To'+sLineBreak
        +'use this function, the file must be located on the server'+sLineBreak
        +'host, you must specify the full path name to the file, and'+sLineBreak
        +'you must have the FILE privilege. The file must be readable'+sLineBreak
        +'by all and it must be less than the size, in bytes, of the'+sLineBreak
        +'max_allowed_packet system variable. If the secure_file_priv'+sLineBreak
        +'system variable is set to a non-empty directory name, the'+sLineBreak
        +'file to be loaded must be located in that directory.'+sLineBreak
        +' '+sLineBreak
        +'If the file does not exist or cannot be read because one of'+sLineBreak
        +'the preceding conditions is not satisfied, the function'+sLineBreak
        +'returns NULL.'+sLineBreak
        +' '+sLineBreak
        +'Since MariaDB 5.1, the character_set_filesystem system'+sLineBreak
        +'variable has controlled interpretation of file names that'+sLineBreak
        +'are given as literal strings.'+sLineBreak
        +' '+sLineBreak
        +'Statements using the LOAD_FILE() function are not safe for'+sLineBreak
        +'statement based replication. This is because the slave will'+sLineBreak
        +'execute the LOAD_FILE() command itself. If the file doesn''t'+sLineBreak
        +'exist on the slave, the function will return NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'UPDATE t SET blob_col=LOAD_FILE(''/tmp/picture'') WHERE'+sLineBreak
        +'id=1;'
    ),

    (
      Name:         'LOCATE';
      Declaration:  '(substr,str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The first syntax returns the position of the first'+sLineBreak
        +'occurrence of'+sLineBreak
        +'substring substr in string str. The second syntax returns'+sLineBreak
        +'the position'+sLineBreak
        +'of the first occurrence of substring substr in string str,'+sLineBreak
        +'starting at'+sLineBreak
        +'position pos. Returns 0 if substr is not in str.'+sLineBreak
        +' '+sLineBreak
        +'LOCATE() performs a case-insensitive search.'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +'INSTR() is a synonym of LOCATE() without the third argument.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LOCATE(''bar'', ''foobarbar'');'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| LOCATE(''bar'', ''foobarbar'') |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOCATE(''My'', ''Maria'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| LOCATE(''My'', ''Maria'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOCATE(''bar'', ''foobarbar'', 5);'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| LOCATE(''bar'', ''foobarbar'', 5) |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 7 |'+sLineBreak
        +'+-------------------------------+'
    ),

    (
      Name:         'LOCATE';
      Declaration:  '(substr,str,pos)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The first syntax returns the position of the first'+sLineBreak
        +'occurrence of'+sLineBreak
        +'substring substr in string str. The second syntax returns'+sLineBreak
        +'the position'+sLineBreak
        +'of the first occurrence of substring substr in string str,'+sLineBreak
        +'starting at'+sLineBreak
        +'position pos. Returns 0 if substr is not in str.'+sLineBreak
        +' '+sLineBreak
        +'LOCATE() performs a case-insensitive search.'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +'INSTR() is a synonym of LOCATE() without the third argument.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LOCATE(''bar'', ''foobarbar'');'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| LOCATE(''bar'', ''foobarbar'') |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +'| 4 |'+sLineBreak
        +'+----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOCATE(''My'', ''Maria'');'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| LOCATE(''My'', ''Maria'') |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+-----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOCATE(''bar'', ''foobarbar'', 5);'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| LOCATE(''bar'', ''foobarbar'', 5) |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| 7 |'+sLineBreak
        +'+-------------------------------+'
    ),

    (
      Name:         'LOWER';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all characters changed to'+sLineBreak
        +'lowercase'+sLineBreak
        +'according to the current character set mapping. The default'+sLineBreak
        +'is latin1'+sLineBreak
        +'(cp1252 West European).'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +' SELECT LOWER(''QUADRATICALLY'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| LOWER(''QUADRATICALLY'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| quadratically |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +' '+sLineBreak
        +'LOWER() (and UPPER()) are ineffective when applied to binary'+sLineBreak
        +'strings (BINARY, VARBINARY, BLOB). '+sLineBreak
        +'To perform lettercase conversion, CONVERT the string to a'+sLineBreak
        +'non-binary string:'+sLineBreak
        +' '+sLineBreak
        +'SET @str = BINARY ''North Carolina'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT LOWER(@str), LOWER(CONVERT(@str USING latin1));'+sLineBreak
        +'+----------------+-----------------------------------+'+sLineBreak
        +'| LOWER(@str) | LOWER(CONVERT(@str USING latin1)) |'+sLineBreak
        +'+----------------+-----------------------------------+'+sLineBreak
        +'| North Carolina | north carolina |'+sLineBreak
        +'+----------------+-----------------------------------+'
    ),

    (
      Name:         'LPAD';
      Declaration:  '(str, len [,padstr])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str, left-padded with the string padstr'+sLineBreak
        +'to a length'+sLineBreak
        +'of len characters. If str is longer than len, the return'+sLineBreak
        +'value is'+sLineBreak
        +'shortened to len characters. If padstr is omitted, the LPAD'+sLineBreak
        +'function pads spaces.'+sLineBreak
        +' '+sLineBreak
        +'Prior to MariaDB 10.3.1, the padstr parameter was mandatory.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if given a NULL argument. If the result is'+sLineBreak
        +'empty (zero length), returns either an empty string or, from'+sLineBreak
        +'MariaDB 10.3.6 with SQL_MODE=Oracle, NULL.'+sLineBreak
        +' '+sLineBreak
        +'The Oracle mode version of the function can be accessed'+sLineBreak
        +'outside of Oracle mode by using LPAD_ORACLE as the function'+sLineBreak
        +'name.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT LPAD(''hello'',10,''.'');'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| LPAD(''hello'',10,''.'') |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| .....hello |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT LPAD(''hello'',2,''.'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| LPAD(''hello'',2,''.'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| he |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.3.1, with the pad string defaulting to'+sLineBreak
        +'space.'+sLineBreak
        +' '+sLineBreak
        +'SELECT LPAD(''hello'',10);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| LPAD(''hello'',10) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| hello |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode version from MariaDB 10.3.6:'+sLineBreak
        +' '+sLineBreak
        +'SELECT LPAD('''',0),LPAD_ORACLE('''',0);'+sLineBreak
        +'+------------+-------------------+'+sLineBreak
        +'| LPAD('''',0) | LPAD_ORACLE('''',0) |'+sLineBreak
        +'+------------+-------------------+'+sLineBreak
        +'| | NULL |'+sLineBreak
        +'+------------+-------------------+'
    ),

    (
      Name:         'LTRIM';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with leading space characters'+sLineBreak
        +'removed.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if given a NULL argument. If the result is'+sLineBreak
        +'empty, returns either an empty string, or, from MariaDB'+sLineBreak
        +'10.3.6 with SQL_MODE=Oracle, NULL.'+sLineBreak
        +' '+sLineBreak
        +'The Oracle mode version of the function can be accessed'+sLineBreak
        +'outside of Oracle mode by using LTRIM_ORACLE as the function'+sLineBreak
        +'name.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT QUOTE(LTRIM('' MariaDB ''));'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| QUOTE(LTRIM('' MariaDB '')) |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| ''MariaDB '' |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode version from MariaDB 10.3.6:'+sLineBreak
        +' '+sLineBreak
        +'SELECT LTRIM(''''),LTRIM_ORACLE('''');'+sLineBreak
        +'+-----------+------------------+'+sLineBreak
        +'| LTRIM('''') | LTRIM_ORACLE('''') |'+sLineBreak
        +'+-----------+------------------+'+sLineBreak
        +'| | NULL |'+sLineBreak
        +'+-----------+------------------+'
    ),

    (
      Name:         'MAKE_SET';
      Declaration:  '(bits,str1,str2,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a set value (a string containing substrings'+sLineBreak
        +'separated by ","'+sLineBreak
        +'characters) consisting of the strings that have the'+sLineBreak
        +'corresponding bit'+sLineBreak
        +'in bits set. str1 corresponds to bit 0, str2 to bit 1, and'+sLineBreak
        +'so on. NULL'+sLineBreak
        +'values in str1, str2, ... are not appended to the result.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MAKE_SET(1,''a'',''b'',''c'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| MAKE_SET(1,''a'',''b'',''c'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| a |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAKE_SET(1 | 4,''hello'',''nice'',''world'');'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| MAKE_SET(1 | 4,''hello'',''nice'',''world'') |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| hello,world |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MAKE_SET(1 | 4,''hello'',''nice'',NULL,''world'');'+sLineBreak
        +'+---------------------------------------------+'+sLineBreak
        +'| MAKE_SET(1 | 4,''hello'',''nice'',NULL,''world'') |'+sLineBreak
        +'+---------------------------------------------+'+sLineBreak
        +'| hello |'+sLineBreak
        +'+---------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT QUOTE(MAKE_SET(0,''a'',''b'',''c''));'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| QUOTE(MAKE_SET(0,''a'',''b'',''c'')) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| '''' |'+sLineBreak
        +'+--------------------------------+'
    ),

    (
      Name:         'MID';
      Declaration:  '(str,pos,len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'MID(str,pos,len) is a synonym for SUBSTRING(str,pos,len).'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT MID(''abcd'',4,1);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| MID(''abcd'',4,1) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| d |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT MID(''abcd'',2,2);'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| MID(''abcd'',2,2) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| bc |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'A negative starting position:'+sLineBreak
        +' '+sLineBreak
        +'SELECT MID(''abcd'',-2,4);'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| MID(''abcd'',-2,4) |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| cd |'+sLineBreak
        +'+------------------+'
    ),

    (
      Name:         'OCTET_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'OCTET_LENGTH() is normally a synonym for LENGTH(). When'+sLineBreak
        +'running Oracle mode from MariaDB 10.3, they are not'+sLineBreak
        +'synonyms, but OCTET_LENGTH() behaves as LENGTH() would when'+sLineBreak
        +'not in Oracle mode.'
    ),

    (
      Name:         'ORD';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If the leftmost character of the string str is a multi-byte'+sLineBreak
        +'character,'+sLineBreak
        +'returns the code for that character, calculated from the'+sLineBreak
        +'numeric'+sLineBreak
        +'values of its constituent bytes using this formula:'+sLineBreak
        +' '+sLineBreak
        +' (1st byte code)'+sLineBreak
        +'+ (2nd byte code x 256)'+sLineBreak
        +'+ (3rd byte code x 256 x 256) ...'+sLineBreak
        +' '+sLineBreak
        +'If the leftmost character is not a multi-byte character,'+sLineBreak
        +'ORD() returns'+sLineBreak
        +'the same value as the ASCII() function.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT ORD(''2'');'+sLineBreak
        +'+----------+'+sLineBreak
        +'| ORD(''2'') |'+sLineBreak
        +'+----------+'+sLineBreak
        +'| 50 |'+sLineBreak
        +'+----------+'
    ),

    (
      Name:         'POSITION';
      Declaration:  '(substr IN str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'POSITION(substr IN str) is a synonym for LOCATE(substr,str).'+sLineBreak
        +' '+sLineBreak
        +'It''s part of ODBC 3.0.'
    ),

    (
      Name:         'QUOTE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Quotes a string to produce a result that can be used as a'+sLineBreak
        +'properly escaped data'+sLineBreak
        +'value in an SQL statement. The string is returned enclosed'+sLineBreak
        +'by single quotes and'+sLineBreak
        +'with each instance of single quote ("''"), backslash'+sLineBreak
        +'("\"),'+sLineBreak
        +'ASCII NUL, and Control-Z preceded by a backslash. If the'+sLineBreak
        +'argument'+sLineBreak
        +'is NULL, the return value is the word "NULL" without'+sLineBreak
        +'enclosing single'+sLineBreak
        +'quotes.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT QUOTE("Don''t!");'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| QUOTE("Don''t!") |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| ''Don\''t!'' |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT QUOTE(NULL); '+sLineBreak
        +'+-------------+'+sLineBreak
        +'| QUOTE(NULL) |'+sLineBreak
        +'+-------------+'+sLineBreak
        +'| NULL |'+sLineBreak
        +'+-------------+'
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(subject, pattern)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(i.e. not in bytes)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(?i)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(?-i)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(''abc'',''b'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(''abc'',''x'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(''BJגN'',''N'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(BINARY ''BJגN'',''N'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(''ABC'',''b'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(''ABC'' COLLATE utf8_bin,''b'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(BINARY''ABC'',''b'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(''ABC'',''(?-i)b'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_INSTR';
      Declaration:  '(''ABC'' COLLATE utf8_bin,''(?i)b'')';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    (
      Name:         'REGEXP_REPLACE';
      Declaration:  '(subject, pattern, replace)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'REGEXP_REPLACE returns the string subject with all'+sLineBreak
        +'occurrences of the regular expression pattern replaced by'+sLineBreak
        +'the string replace. If no occurrences are found, then'+sLineBreak
        +'subject is returned as is.'+sLineBreak
        +' '+sLineBreak
        +'The replace string can have backreferences to the'+sLineBreak
        +'subexpressions in the form \N, where N is a number from 1'+sLineBreak
        +'to 9.'+sLineBreak
        +' '+sLineBreak
        +'The function follows the case sensitivity rules of the'+sLineBreak
        +'effective collation. Matching is performed case'+sLineBreak
        +'insensitively for case insensitive collations, and case'+sLineBreak
        +'sensitively for case sensitive collations and for binary'+sLineBreak
        +'data.'+sLineBreak
        +' '+sLineBreak
        +'The collation case sensitivity can be overwritten using the'+sLineBreak
        +'(?i) and (?-i) PCRE flags.'+sLineBreak
        +' '+sLineBreak
        +'MariaDB 10.0.5 switched to the PCRE regular expression'+sLineBreak
        +'library for enhanced regular expression performance, and'+sLineBreak
        +'REGEXP_REPLACE was introduced as part of this enhancement.'+sLineBreak
        +' '+sLineBreak
        +'MariaDB 10.0.11 introduced the default_regex_flags variable'+sLineBreak
        +'to address the remaining compatibilities between PCRE and'+sLineBreak
        +'the old regex library. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT REGEXP_REPLACE(''ab12cd'',''[0-9]'','''') AS'+sLineBreak
        +'remove_digits;'+sLineBreak
        +'-> abcd'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_REPLACE(''titlebody'', '''','' '')'+sLineBreak
        +'AS strip_html;'+sLineBreak
        +'-> title body'+sLineBreak
        +' '+sLineBreak
        +'Backreferences to the subexpressions in the form \N, where'+sLineBreak
        +'N is a number from 1 to 9:'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_REPLACE(''James Bond'',''^(.*)'+sLineBreak
        +'(.*)$'',''\\2, \\1'') AS reorder_name;'+sLineBreak
        +'-> Bond, James'+sLineBreak
        +' '+sLineBreak
        +'Case insensitive and case sensitive matches:'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_REPLACE(''ABC'',''b'',''-'') AS'+sLineBreak
        +'case_insensitive;'+sLineBreak
        +'-> A-C'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_REPLACE(''ABC'' COLLATE utf8_bin,''b'',''-'')'+sLineBreak
        +'AS case_sensitive;'+sLineBreak
        +'-> ABC'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_REPLACE(BINARY ''ABC'',''b'',''-'') AS'+sLineBreak
        +'binary_data;'+sLineBreak
        +'-> ABC'+sLineBreak
        +' '+sLineBreak
        +'Overwriting the collation case sensitivity using the (?i)'+sLineBreak
        +'and (?-i) PCRE flags.'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_REPLACE(''ABC'',''(?-i)b'',''-'') AS'+sLineBreak
        +'force_case_sensitive;'+sLineBreak
        +'-> ABC'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_REPLACE(BINARY ''ABC'',''(?i)b'',''-'') AS'+sLineBreak
        +'force_case_insensitive;'+sLineBreak
        +'-> A-C'
    ),

    (
      Name:         'REGEXP_SUBSTR';
      Declaration:  '(subject,pattern)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the part of the string subject that matches the'+sLineBreak
        +'regular expression pattern, or an empty string if pattern'+sLineBreak
        +'was not found.'+sLineBreak
        +' '+sLineBreak
        +'The function follows the case sensitivity rules of the'+sLineBreak
        +'effective collation. Matching is performed case'+sLineBreak
        +'insensitively for case insensitive collations, and case'+sLineBreak
        +'sensitively for case sensitive collations and for binary'+sLineBreak
        +'data.'+sLineBreak
        +' '+sLineBreak
        +'The collation case sensitivity can be overwritten using the'+sLineBreak
        +'(?i) and (?-i) PCRE flags.'+sLineBreak
        +' '+sLineBreak
        +'MariaDB 10.0.5 switched to the PCRE regular expression'+sLineBreak
        +'library for enhanced regular expression performance, and'+sLineBreak
        +'REGEXP_SUBSTR was introduced as part of this enhancement.'+sLineBreak
        +' '+sLineBreak
        +'MariaDB 10.0.11 introduced the default_regex_flags variable'+sLineBreak
        +'to address the remaining compatibilities between PCRE and'+sLineBreak
        +'the old regex library. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT REGEXP_SUBSTR(''ab12cd'',''[0-9]+'');'+sLineBreak
        +'-> 12'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_SUBSTR('+sLineBreak
        +' ''See https://mariadb.org/en/foundation/ for details'','+sLineBreak
        +' ''https?://[^/]*'');'+sLineBreak
        +'-> https://mariadb.org'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_SUBSTR(''ABC'',''b'');'+sLineBreak
        +'-> B'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_SUBSTR(''ABC'' COLLATE utf8_bin,''b'');'+sLineBreak
        +'->'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_SUBSTR(BINARY''ABC'',''b'');'+sLineBreak
        +'->'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_SUBSTR(''ABC'',''(?i)b'');'+sLineBreak
        +'-> B'+sLineBreak
        +' '+sLineBreak
        +'SELECT REGEXP_SUBSTR(''ABC'' COLLATE utf8_bin,''(?+i)b'');'+sLineBreak
        +'-> B'
    ),

    (
      Name:         'REVERSE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with the order of the characters'+sLineBreak
        +'reversed.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT REVERSE(''desserts'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| REVERSE(''desserts'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| stressed |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'RIGHT';
      Declaration:  '(str,len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the rightmost len characters from the string str, or'+sLineBreak
        +'NULL if'+sLineBreak
        +'any argument is NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT RIGHT(''MariaDB'', 2);'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| RIGHT(''MariaDB'', 2) |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| DB |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'RPAD';
      Declaration:  '(str, len [, padstr])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str, right-padded with the string padstr'+sLineBreak
        +'to a'+sLineBreak
        +'length of len characters. If str is longer than len, the'+sLineBreak
        +'return value'+sLineBreak
        +'is shortened to len characters. If padstr is omitted, the'+sLineBreak
        +'RPAD function pads spaces.'+sLineBreak
        +' '+sLineBreak
        +'Prior to MariaDB 10.3.1, the padstr parameter was mandatory.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if given a NULL argument. If the result is'+sLineBreak
        +'empty (a length of zero), returns either an empty string,'+sLineBreak
        +'or, from MariaDB 10.3.6 with SQL_MODE=Oracle, NULL.'+sLineBreak
        +' '+sLineBreak
        +'The Oracle mode version of the function can be accessed'+sLineBreak
        +'outside of Oracle mode by using RPAD_ORACLE as the function'+sLineBreak
        +'name.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT RPAD(''hello'',10,''.'');'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| RPAD(''hello'',10,''.'') |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| hello..... |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT RPAD(''hello'',2,''.'');'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| RPAD(''hello'',2,''.'') |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| he |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +' '+sLineBreak
        +'From MariaDB 10.3.1, with the pad string defaulting to'+sLineBreak
        +'space.'+sLineBreak
        +' '+sLineBreak
        +'SELECT RPAD(''hello'',30);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| RPAD(''hello'',30) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| hello |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode version from MariaDB 10.3.6:'+sLineBreak
        +' '+sLineBreak
        +'SELECT RPAD('''',0),RPAD_ORACLE('''',0);'+sLineBreak
        +'+------------+-------------------+'+sLineBreak
        +'| RPAD('''',0) | RPAD_ORACLE('''',0) |'+sLineBreak
        +'+------------+-------------------+'+sLineBreak
        +'| | NULL |'+sLineBreak
        +'+------------+-------------------+'
    ),

    (
      Name:         'RTRIM';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with trailing space characters'+sLineBreak
        +'removed.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if given a NULL argument. If the result is'+sLineBreak
        +'empty, returns either an empty string, or, from MariaDB'+sLineBreak
        +'10.3.6 with SQL_MODE=Oracle, NULL.'+sLineBreak
        +' '+sLineBreak
        +'The Oracle mode version of the function can be accessed'+sLineBreak
        +'outside of Oracle mode by using RTRIM_ORACLE as the function'+sLineBreak
        +'name.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT QUOTE(RTRIM(''MariaDB ''));'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| QUOTE(RTRIM(''MariaDB '')) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| ''MariaDB'' |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode version from MariaDB 10.3.6:'+sLineBreak
        +' '+sLineBreak
        +'SELECT RTRIM(''''),RTRIM_ORACLE('''');'+sLineBreak
        +'+-----------+------------------+'+sLineBreak
        +'| RTRIM('''') | RTRIM_ORACLE('''') |'+sLineBreak
        +'+-----------+------------------+'+sLineBreak
        +'| | NULL |'+sLineBreak
        +'+-----------+------------------+'
    ),

    (
      Name:         'SOUNDEX';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a soundex string from str. Two strings that sound'+sLineBreak
        +'almost the'+sLineBreak
        +'same should have identical soundex strings. A standard'+sLineBreak
        +'soundex string is four'+sLineBreak
        +'characters long, but the SOUNDEX() function returns an'+sLineBreak
        +'arbitrarily long'+sLineBreak
        +'string. You can use SUBSTRING() on the result to get a'+sLineBreak
        +'standard soundex'+sLineBreak
        +'string. All non-alphabetic characters in str are ignored.'+sLineBreak
        +'All'+sLineBreak
        +'international alphabetic characters outside the A-Z range'+sLineBreak
        +'are treated as'+sLineBreak
        +'vowels.'+sLineBreak
        +' '+sLineBreak
        +'Important: When using SOUNDEX(), you should be aware of the'+sLineBreak
        +'following limitations:'+sLineBreak
        +'This function, as currently implemented, is intended to work'+sLineBreak
        +'well with'+sLineBreak
        +' strings that are in the English language only. Strings in'+sLineBreak
        +'other languages may'+sLineBreak
        +' not produce reliable results.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SOUNDEX(''Hello'');'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| SOUNDEX(''Hello'') |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| H400 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SOUNDEX(''MariaDB'');'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| SOUNDEX(''MariaDB'') |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| M631 |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SOUNDEX(''Knowledgebase'');'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| SOUNDEX(''Knowledgebase'') |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +'| K543212 |'+sLineBreak
        +'+--------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT givenname, surname FROM users WHERE'+sLineBreak
        +'SOUNDEX(givenname) = SOUNDEX("robert");'+sLineBreak
        +'+-----------+---------+'+sLineBreak
        +'| givenname | surname |'+sLineBreak
        +'+-----------+---------+'+sLineBreak
        +'| Roberto | Castro |'+sLineBreak
        +'+-----------+---------+'
    ),

    (
      Name:         'SPACE';
      Declaration:  '(N)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string consisting of N space characters. If N is'+sLineBreak
        +'NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT QUOTE(SPACE(6));'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| QUOTE(SPACE(6)) |'+sLineBreak
        +'+-----------------+'+sLineBreak
        +'| '' '' |'+sLineBreak
        +'+-----------------+'
    ),

    (
      Name:         'STRCMP';
      Declaration:  '(expr1,expr2)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'STRCMP() returns 0 if the strings are the same, -1 if the'+sLineBreak
        +'first'+sLineBreak
        +'argument is smaller than the second according to the current'+sLineBreak
        +'sort order,'+sLineBreak
        +'and 1 otherwise.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT STRCMP(''text'', ''text2'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| STRCMP(''text'', ''text2'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| -1 |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT STRCMP(''text2'', ''text'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| STRCMP(''text2'', ''text'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 1 |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT STRCMP(''text'', ''text'');'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| STRCMP(''text'', ''text'') |'+sLineBreak
        +'+------------------------+'+sLineBreak
        +'| 0 |'+sLineBreak
        +'+------------------------+'
    ),

    (
      Name:         'SUBSTRING';
      Declaration:  '(str,pos)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The forms without a len argument return a substring from'+sLineBreak
        +'string str starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms with a len argument return a substring len'+sLineBreak
        +'characters long from string str, starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms that use FROM are standard SQL syntax.'+sLineBreak
        +' '+sLineBreak
        +'It is also possible to use a negative value for pos. In this'+sLineBreak
        +'case, the beginning of the substring is pos characters from'+sLineBreak
        +'the end of the string, rather than the beginning. A negative'+sLineBreak
        +'value may be used for pos in any of the forms of this'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'By default, the position of the first character in the'+sLineBreak
        +'string from which the substring is to be extracted is'+sLineBreak
        +'reckoned as 1. For Oracle-compatibility, from MariaDB'+sLineBreak
        +'10.3.3, when sql_mode is set to ''oracle'', position zero is'+sLineBreak
        +'treated as position 1 (although the first character is still'+sLineBreak
        +'reckoned as 1).'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',5);'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',5) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| ledgebase |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''MariaDB'' FROM 6);'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| SUBSTRING(''MariaDB'' FROM 6) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| DB |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',3,7);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',3,7) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| owledge |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -4);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -4) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| base |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -8, 4);'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -8, 4) |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'' FROM -8 FOR 4);'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'' FROM -8 FOR 4) |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode from MariaDB 10.3.3:'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET sql_mode=''oracle'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| abc |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'SUBSTRING';
      Declaration:  '(str FROM pos)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The forms without a len argument return a substring from'+sLineBreak
        +'string str starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms with a len argument return a substring len'+sLineBreak
        +'characters long from string str, starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms that use FROM are standard SQL syntax.'+sLineBreak
        +' '+sLineBreak
        +'It is also possible to use a negative value for pos. In this'+sLineBreak
        +'case, the beginning of the substring is pos characters from'+sLineBreak
        +'the end of the string, rather than the beginning. A negative'+sLineBreak
        +'value may be used for pos in any of the forms of this'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'By default, the position of the first character in the'+sLineBreak
        +'string from which the substring is to be extracted is'+sLineBreak
        +'reckoned as 1. For Oracle-compatibility, from MariaDB'+sLineBreak
        +'10.3.3, when sql_mode is set to ''oracle'', position zero is'+sLineBreak
        +'treated as position 1 (although the first character is still'+sLineBreak
        +'reckoned as 1).'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',5);'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',5) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| ledgebase |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''MariaDB'' FROM 6);'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| SUBSTRING(''MariaDB'' FROM 6) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| DB |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',3,7);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',3,7) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| owledge |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -4);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -4) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| base |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -8, 4);'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -8, 4) |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'' FROM -8 FOR 4);'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'' FROM -8 FOR 4) |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode from MariaDB 10.3.3:'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET sql_mode=''oracle'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| abc |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'SUBSTRING';
      Declaration:  '(str,pos,len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The forms without a len argument return a substring from'+sLineBreak
        +'string str starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms with a len argument return a substring len'+sLineBreak
        +'characters long from string str, starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms that use FROM are standard SQL syntax.'+sLineBreak
        +' '+sLineBreak
        +'It is also possible to use a negative value for pos. In this'+sLineBreak
        +'case, the beginning of the substring is pos characters from'+sLineBreak
        +'the end of the string, rather than the beginning. A negative'+sLineBreak
        +'value may be used for pos in any of the forms of this'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'By default, the position of the first character in the'+sLineBreak
        +'string from which the substring is to be extracted is'+sLineBreak
        +'reckoned as 1. For Oracle-compatibility, from MariaDB'+sLineBreak
        +'10.3.3, when sql_mode is set to ''oracle'', position zero is'+sLineBreak
        +'treated as position 1 (although the first character is still'+sLineBreak
        +'reckoned as 1).'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',5);'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',5) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| ledgebase |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''MariaDB'' FROM 6);'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| SUBSTRING(''MariaDB'' FROM 6) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| DB |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',3,7);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',3,7) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| owledge |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -4);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -4) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| base |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -8, 4);'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -8, 4) |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'' FROM -8 FOR 4);'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'' FROM -8 FOR 4) |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode from MariaDB 10.3.3:'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET sql_mode=''oracle'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| abc |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'SUBSTRING';
      Declaration:  '(str FROM pos FOR len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The forms without a len argument return a substring from'+sLineBreak
        +'string str starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms with a len argument return a substring len'+sLineBreak
        +'characters long from string str, starting at position pos.'+sLineBreak
        +' '+sLineBreak
        +'The forms that use FROM are standard SQL syntax.'+sLineBreak
        +' '+sLineBreak
        +'It is also possible to use a negative value for pos. In this'+sLineBreak
        +'case, the beginning of the substring is pos characters from'+sLineBreak
        +'the end of the string, rather than the beginning. A negative'+sLineBreak
        +'value may be used for pos in any of the forms of this'+sLineBreak
        +'function.'+sLineBreak
        +' '+sLineBreak
        +'By default, the position of the first character in the'+sLineBreak
        +'string from which the substring is to be extracted is'+sLineBreak
        +'reckoned as 1. For Oracle-compatibility, from MariaDB'+sLineBreak
        +'10.3.3, when sql_mode is set to ''oracle'', position zero is'+sLineBreak
        +'treated as position 1 (although the first character is still'+sLineBreak
        +'reckoned as 1).'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',5);'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',5) |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +'| ledgebase |'+sLineBreak
        +'+------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''MariaDB'' FROM 6);'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| SUBSTRING(''MariaDB'' FROM 6) |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +'| DB |'+sLineBreak
        +'+-----------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'',3,7);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'',3,7) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| owledge |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -4);'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -4) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| base |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'', -8, 4);'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'', -8, 4) |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+-----------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING(''Knowledgebase'' FROM -8 FOR 4);'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| SUBSTRING(''Knowledgebase'' FROM -8 FOR 4) |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +'| edge |'+sLineBreak
        +'+------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode from MariaDB 10.3.3:'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SET sql_mode=''oracle'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',0,3);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',0,3) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| abc |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTR(''abc'',1,2);'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| SUBSTR(''abc'',1,2) |'+sLineBreak
        +'+-------------------+'+sLineBreak
        +'| ab |'+sLineBreak
        +'+-------------------+'
    ),

    (
      Name:         'SUBSTRING_INDEX';
      Declaration:  '(str,delim,count)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the substring from string str before count'+sLineBreak
        +'occurrences of the'+sLineBreak
        +'delimiter delim. If count is positive, everything to the'+sLineBreak
        +'left'+sLineBreak
        +'of the final delimiter (counting from the left) is returned.'+sLineBreak
        +'If count'+sLineBreak
        +'is negative, everything to the right of the final delimiter'+sLineBreak
        +'(counting from the'+sLineBreak
        +'right) is returned. SUBSTRING_INDEX() performs a'+sLineBreak
        +'case-sensitive match when'+sLineBreak
        +'searching for delim.'+sLineBreak
        +' '+sLineBreak
        +'If any argument is NULL, returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT SUBSTRING_INDEX(''www.mariadb.org'', ''.'', 2);'+sLineBreak
        +'+--------------------------------------------+'+sLineBreak
        +'| SUBSTRING_INDEX(''www.mariadb.org'', ''.'', 2) |'+sLineBreak
        +'+--------------------------------------------+'+sLineBreak
        +'| www.mariadb |'+sLineBreak
        +'+--------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT SUBSTRING_INDEX(''www.mariadb.org'', ''.'', -2);'+sLineBreak
        +'+---------------------------------------------+'+sLineBreak
        +'| SUBSTRING_INDEX(''www.mariadb.org'', ''.'', -2) |'+sLineBreak
        +'+---------------------------------------------+'+sLineBreak
        +'| mariadb.org |'+sLineBreak
        +'+---------------------------------------------+'
    ),

    (
      Name:         'TO_BASE64';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Converts the string argument str to its base-64 encoded'+sLineBreak
        +'form, returning the result as a character string in the'+sLineBreak
        +'connection character set and collation.'+sLineBreak
        +' '+sLineBreak
        +'The argument str will be converted to string first if it is'+sLineBreak
        +'not a string. A NULL argument will return a NULL result.'+sLineBreak
        +' '+sLineBreak
        +'The reverse function, FROM_BASE64(), decodes an encoded'+sLineBreak
        +'base-64 string.'+sLineBreak
        +' '+sLineBreak
        +'There are a numerous different methods to base-64 encode a'+sLineBreak
        +'string. The following are used by MariaDB and MySQL:'+sLineBreak
        +'Alphabet value 64 is encoded as ''+''.'+sLineBreak
        +'Alphabet value 63 is encoded as ''/''.'+sLineBreak
        +'Encoding output is made up of groups of four printable'+sLineBreak
        +'characters, with each three bytes of data encoded using four'+sLineBreak
        +'characters. If the final group is not complete, it is padded'+sLineBreak
        +'with ''='' characters to make up a length of four.'+sLineBreak
        +'To divide long output, a newline is added after every 76'+sLineBreak
        +'characters.'+sLineBreak
        +'Decoding will recognize and ignore newlines, carriage'+sLineBreak
        +'returns, tabs, and spaces. '+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TO_BASE64(''Maria'');'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| TO_BASE64(''Maria'') |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| TWFyaWE= |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'TRIM';
      Declaration:  '([{BOTH | LEADING | TRAILING} [remstr] FROM] str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all remstr prefixes or suffixes'+sLineBreak
        +'removed. If none of the specifiers BOTH, LEADING, or'+sLineBreak
        +'TRAILING is given, BOTH is assumed. remstr is optional and,'+sLineBreak
        +'if not specified, spaces are removed.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if given a NULL argument. If the result is'+sLineBreak
        +'empty, returns either an empty string, or, from MariaDB'+sLineBreak
        +'10.3.6 with SQL_MODE=Oracle, NULL.'+sLineBreak
        +' '+sLineBreak
        +'The Oracle mode version of the function can be accessed'+sLineBreak
        +'outside of Oracle mode by using TRIM_ORACLE as the function'+sLineBreak
        +'name.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TRIM('' bar '')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM('' bar ''): bar'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(LEADING ''x'' FROM ''xxxbarxxx'')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM(LEADING ''x'' FROM ''xxxbarxxx''): barxxx'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(BOTH ''x'' FROM ''xxxbarxxx'')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM(BOTH ''x'' FROM ''xxxbarxxx''): bar'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(TRAILING ''xyz'' FROM ''barxxyz'')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM(TRAILING ''xyz'' FROM ''barxxyz''): barx'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode version from MariaDB 10.3.6:'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(''''),TRIM_ORACLE('''');'+sLineBreak
        +'+----------+-----------------+'+sLineBreak
        +'| TRIM('''') | TRIM_ORACLE('''') |'+sLineBreak
        +'+----------+-----------------+'+sLineBreak
        +'| | NULL |'+sLineBreak
        +'+----------+-----------------+'
    ),

    (
      Name:         'TRIM';
      Declaration:  '([remstr FROM] str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all remstr prefixes or suffixes'+sLineBreak
        +'removed. If none of the specifiers BOTH, LEADING, or'+sLineBreak
        +'TRAILING is given, BOTH is assumed. remstr is optional and,'+sLineBreak
        +'if not specified, spaces are removed.'+sLineBreak
        +' '+sLineBreak
        +'Returns NULL if given a NULL argument. If the result is'+sLineBreak
        +'empty, returns either an empty string, or, from MariaDB'+sLineBreak
        +'10.3.6 with SQL_MODE=Oracle, NULL.'+sLineBreak
        +' '+sLineBreak
        +'The Oracle mode version of the function can be accessed'+sLineBreak
        +'outside of Oracle mode by using TRIM_ORACLE as the function'+sLineBreak
        +'name.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT TRIM('' bar '')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM('' bar ''): bar'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(LEADING ''x'' FROM ''xxxbarxxx'')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM(LEADING ''x'' FROM ''xxxbarxxx''): barxxx'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(BOTH ''x'' FROM ''xxxbarxxx'')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM(BOTH ''x'' FROM ''xxxbarxxx''): bar'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(TRAILING ''xyz'' FROM ''barxxyz'')\G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'TRIM(TRAILING ''xyz'' FROM ''barxxyz''): barx'+sLineBreak
        +' '+sLineBreak
        +'Oracle mode version from MariaDB 10.3.6:'+sLineBreak
        +' '+sLineBreak
        +'SELECT TRIM(''''),TRIM_ORACLE('''');'+sLineBreak
        +'+----------+-----------------+'+sLineBreak
        +'| TRIM('''') | TRIM_ORACLE('''') |'+sLineBreak
        +'+----------+-----------------+'+sLineBreak
        +'| | NULL |'+sLineBreak
        +'+----------+-----------------+'
    ),

    (
      Name:         'UCASE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'UCASE() is a synonym for UPPER().'
    ),

    (
      Name:         'UNHEX';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Performs the inverse operation of HEX(str). That is, it'+sLineBreak
        +'interprets'+sLineBreak
        +'each pair of hexadecimal digits in the argument as a number'+sLineBreak
        +'and'+sLineBreak
        +'converts it to the character represented by the number. The'+sLineBreak
        +'resulting'+sLineBreak
        +'characters are returned as a binary string.'+sLineBreak
        +' '+sLineBreak
        +'If str is NULL, UNHEX() returns NULL.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT HEX(''MariaDB'');'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| HEX(''MariaDB'') |'+sLineBreak
        +'+----------------+'+sLineBreak
        +'| 4D617269614442 |'+sLineBreak
        +'+----------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT UNHEX(''4D617269614442'');'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| UNHEX(''4D617269614442'') |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| MariaDB |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT 0x4D617269614442;'+sLineBreak
        +' '+sLineBreak
        +'+------------------+'+sLineBreak
        +'| 0x4D617269614442 |'+sLineBreak
        +'+------------------+'+sLineBreak
        +'| MariaDB |'+sLineBreak
        +'+------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT UNHEX(HEX(''string''));'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| UNHEX(HEX(''string'')) |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +'| string |'+sLineBreak
        +'+----------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(UNHEX(''1267''));'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| HEX(UNHEX(''1267'')) |'+sLineBreak
        +'+--------------------+'+sLineBreak
        +'| 1267 |'+sLineBreak
        +'+--------------------+'
    ),

    (
      Name:         'UPDATEXML';
      Declaration:  '(xml_target, xpath_expr, new_xml)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function replaces a single portion of a given fragment'+sLineBreak
        +'of XML markup'+sLineBreak
        +'xml_target with a new XML fragment new_xml, and then returns'+sLineBreak
        +'the'+sLineBreak
        +'changed XML. The portion of xml_target that is replaced'+sLineBreak
        +'matches an XPath'+sLineBreak
        +'expression xpath_expr supplied by the user. If no expression'+sLineBreak
        +'matching'+sLineBreak
        +'xpath_expr is found, or if multiple matches are found, the'+sLineBreak
        +'function returns'+sLineBreak
        +'the original xml_target XML fragment. All three arguments'+sLineBreak
        +'should be'+sLineBreak
        +'strings.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SELECT'+sLineBreak
        +' UpdateXML(''ccc'', ''/a'', ''fff'') AS val1,'+sLineBreak
        +' UpdateXML(''ccc'', ''/b'', ''fff'') AS val2,'+sLineBreak
        +' UpdateXML(''ccc'', ''//b'', ''fff'') AS val3,'+sLineBreak
        +' UpdateXML(''ccc'', ''/a/d'', ''fff'') AS val4,'+sLineBreak
        +' UpdateXML(''ccc'', ''/a/d'', ''fff'') AS val5'+sLineBreak
        +' \G'+sLineBreak
        +'*************************** 1. row'+sLineBreak
        +'***************************'+sLineBreak
        +'val1: fff'+sLineBreak
        +'val2: ccc'+sLineBreak
        +'val3: fff'+sLineBreak
        +'val4: cccfff'+sLineBreak
        +'val5: ccc'+sLineBreak
        +'1 row in set (0.00 sec)'
    ),

    (
      Name:         'UPPER';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all characters changed to'+sLineBreak
        +'uppercase'+sLineBreak
        +'according to the current character set mapping. The default'+sLineBreak
        +'is latin1'+sLineBreak
        +'(cp1252 West European).'+sLineBreak
        +' '+sLineBreak
        +'SELECT UPPER(surname), givenname FROM users ORDER BY'+sLineBreak
        +'surname;'+sLineBreak
        +' '+sLineBreak
        +'+----------------+------------+'+sLineBreak
        +'| UPPER(surname) | givenname |'+sLineBreak
        +'+----------------+------------+'+sLineBreak
        +'| ABEL | Jacinto |'+sLineBreak
        +'| CASTRO | Robert |'+sLineBreak
        +'| COSTA | Phestos |'+sLineBreak
        +'| MOSCHELLA | Hippolytos |'+sLineBreak
        +'+----------------+------------+'+sLineBreak
        +' '+sLineBreak
        +'UPPER() is ineffective when applied to binary strings'+sLineBreak
        +'(BINARY,'+sLineBreak
        +'VARBINARY, BLOB). The description of '+sLineBreak
        +'LOWER() shows how to'+sLineBreak
        +'perform lettercase conversion of binary strings.'
    ),

    (
      Name:         'WEIGHT_STRING';
      Declaration:  '(str [AS {CHAR|BINARY}(N)] [LEVEL levels] [flags])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a binary string representing the string''s sorting'+sLineBreak
        +'and comparison value. A string with a lower result means'+sLineBreak
        +'that for sorting purposes the string appears before a string'+sLineBreak
        +'with a higher result.'+sLineBreak
        +' '+sLineBreak
        +'WEIGHT_STRING() is particularly useful when adding new'+sLineBreak
        +'collations, for testing purposes.'+sLineBreak
        +' '+sLineBreak
        +'If str is a non-binary string (CHAR, VARCHAR or TEXT),'+sLineBreak
        +'WEIGHT_STRING returns the string''s collation weight. If str'+sLineBreak
        +'is a binary string (BINARY, VARBINARY or BLOB), the return'+sLineBreak
        +'value is simply the input value, since the weight for each'+sLineBreak
        +'byte in a binary string is the byte value.'+sLineBreak
        +' '+sLineBreak
        +'WEIGHT_STRING() returns NULL if given a NULL input. '+sLineBreak
        +' '+sLineBreak
        +'The optional AS clause permits casting the input string to a'+sLineBreak
        +'binary or non-binary string, as well as to a particular'+sLineBreak
        +'length.'+sLineBreak
        +' '+sLineBreak
        +'AS BINARY(N) measures the length in bytes rather than'+sLineBreak
        +'characters, and right pads with 0x00 bytes to the desired'+sLineBreak
        +'length. '+sLineBreak
        +' '+sLineBreak
        +'AS CHAR(N) measures the length in characters, and right pads'+sLineBreak
        +'with spaces to the desired length.'+sLineBreak
        +' '+sLineBreak
        +'N has a minimum value of 1, and if it is less than the'+sLineBreak
        +'length of the input string, the string is truncated without'+sLineBreak
        +'warning.'+sLineBreak
        +' '+sLineBreak
        +'The optional LEVEL clause specifies that the return value'+sLineBreak
        +'should contain weights for specific collation levels. The'+sLineBreak
        +'levels specifier can either be a single integer, a'+sLineBreak
        +'comma-separated list of integers, or a range of integers'+sLineBreak
        +'separated by a dash (whitespace is ignored). Integers can'+sLineBreak
        +'range from 1 to a maximum of 6, dependent on the collation,'+sLineBreak
        +'and need to be listed in ascending order.'+sLineBreak
        +' '+sLineBreak
        +'If the LEVEL clause is no provided, a default of 1 to the'+sLineBreak
        +'maximum for the collation is assumed.'+sLineBreak
        +' '+sLineBreak
        +'If the LEVEL is specified without using a range, an optional'+sLineBreak
        +'modifier is permitted.'+sLineBreak
        +' '+sLineBreak
        +'ASC, the default, returns the weights without any'+sLineBreak
        +'modification.'+sLineBreak
        +' '+sLineBreak
        +'DESC returns bitwise-inverted weights.'+sLineBreak
        +' '+sLineBreak
        +'REVERSE returns the weights in reverse order.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The examples below use the HEX() function to represent'+sLineBreak
        +'non-printable results in hexadecimal format.'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(WEIGHT_STRING(''x''));'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| HEX(WEIGHT_STRING(''x'')) |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +'| 0058 |'+sLineBreak
        +'+-------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(WEIGHT_STRING(''x'' AS BINARY(4)));'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| HEX(WEIGHT_STRING(''x'' AS BINARY(4))) |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| 78000000 |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(WEIGHT_STRING(''x'' AS CHAR(4)));'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| HEX(WEIGHT_STRING(''x'' AS CHAR(4))) |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +'| 0058002000200020 |'+sLineBreak
        +'+------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(WEIGHT_STRING(0xaa22ee LEVEL 1));'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| HEX(WEIGHT_STRING(0xaa22ee LEVEL 1)) |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +'| AA22EE |'+sLineBreak
        +'+--------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(WEIGHT_STRING(0xaa22ee LEVEL 1 DESC));'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| HEX(WEIGHT_STRING(0xaa22ee LEVEL 1 DESC)) |'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +'| 55DD11 |'+sLineBreak
        +'+-------------------------------------------+'+sLineBreak
        +' '+sLineBreak
        +'SELECT HEX(WEIGHT_STRING(0xaa22ee LEVEL 1 REVERSE));'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| HEX(WEIGHT_STRING(0xaa22ee LEVEL 1 REVERSE)) |'+sLineBreak
        +'+----------------------------------------------+'+sLineBreak
        +'| EE22AA |'+sLineBreak
        +'+----------------------------------------------+'
    ),

    (
      Name:         'CUME_DIST';
      Declaration:  '()';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CUME_DIST() is a window function that returns the cumulative'+sLineBreak
        +'distribution of a given row. The following formula is used'+sLineBreak
        +'to calculate the value:'+sLineBreak
        +' '+sLineBreak
        +'(number of rows'
    ),

    (
      Name:         'CUME_DIST';
      Declaration:  '(   [ PARTITION BY partition_expression ]   [ ORDER BY order_list ] )';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CUME_DIST() is a window function that returns the cumulative'+sLineBreak
        +'distribution of a given row. The following formula is used'+sLineBreak
        +'to calculate the value:'+sLineBreak
        +' '+sLineBreak
        +'(number of rows'
    ),

    (
      Name:         'DENSE_RANK';
      Declaration:  '()';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'DENSE_RANK() is a window function that displays the number'+sLineBreak
        +'of a given row, starting at one and following the ORDER BY'+sLineBreak
        +'sequence of the window function, with identical values'+sLineBreak
        +'receiving the same result. Unlike the RANK() function, there'+sLineBreak
        +'are no skipped values if the preceding results are'+sLineBreak
        +'identical. It is also similar to the ROW_NUMBER() function'+sLineBreak
        +'except that in that function, identical values will receive'+sLineBreak
        +'a different row number for each result.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The distinction between DENSE_RANK(), RANK() and'+sLineBreak
        +'ROW_NUMBER():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student(course VARCHAR(10), mark int, name'+sLineBreak
        +'varchar(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Maths'', 60, ''Thulile''),'+sLineBreak
        +' (''Maths'', 60, ''Pritha''),'+sLineBreak
        +' (''Maths'', 70, ''Voitto''),'+sLineBreak
        +' (''Maths'', 55, ''Chun''),'+sLineBreak
        +' (''Biology'', 60, ''Bilal''),'+sLineBreak
        +' (''Biology'', 70, ''Roger'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT '+sLineBreak
        +' RANK() OVER (PARTITION BY course ORDER BY mark DESC) AS'+sLineBreak
        +'rank, '+sLineBreak
        +' DENSE_RANK() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS dense_rank, '+sLineBreak
        +' ROW_NUMBER() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS row_num, '+sLineBreak
        +' course, mark, name '+sLineBreak
        +'FROM student ORDER BY course, mark DESC;'+sLineBreak
        +' '+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| rank | dense_rank | row_num | course | mark | name |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| 1 | 1 | 1 | Biology | 70 | Roger |'+sLineBreak
        +'| 2 | 2 | 2 | Biology | 60 | Bilal |'+sLineBreak
        +'| 1 | 1 | 1 | Maths | 70 | Voitto |'+sLineBreak
        +'| 2 | 2 | 2 | Maths | 60 | Thulile |'+sLineBreak
        +'| 2 | 2 | 3 | Maths | 60 | Pritha |'+sLineBreak
        +'| 4 | 3 | 4 | Maths | 55 | Chun |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'
    ),

    (
      Name:         'DENSE_RANK';
      Declaration:  '(  [ PARTITION BY partition_expression ]  [ ORDER BY order_list ] )';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'DENSE_RANK() is a window function that displays the number'+sLineBreak
        +'of a given row, starting at one and following the ORDER BY'+sLineBreak
        +'sequence of the window function, with identical values'+sLineBreak
        +'receiving the same result. Unlike the RANK() function, there'+sLineBreak
        +'are no skipped values if the preceding results are'+sLineBreak
        +'identical. It is also similar to the ROW_NUMBER() function'+sLineBreak
        +'except that in that function, identical values will receive'+sLineBreak
        +'a different row number for each result.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The distinction between DENSE_RANK(), RANK() and'+sLineBreak
        +'ROW_NUMBER():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student(course VARCHAR(10), mark int, name'+sLineBreak
        +'varchar(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Maths'', 60, ''Thulile''),'+sLineBreak
        +' (''Maths'', 60, ''Pritha''),'+sLineBreak
        +' (''Maths'', 70, ''Voitto''),'+sLineBreak
        +' (''Maths'', 55, ''Chun''),'+sLineBreak
        +' (''Biology'', 60, ''Bilal''),'+sLineBreak
        +' (''Biology'', 70, ''Roger'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT '+sLineBreak
        +' RANK() OVER (PARTITION BY course ORDER BY mark DESC) AS'+sLineBreak
        +'rank, '+sLineBreak
        +' DENSE_RANK() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS dense_rank, '+sLineBreak
        +' ROW_NUMBER() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS row_num, '+sLineBreak
        +' course, mark, name '+sLineBreak
        +'FROM student ORDER BY course, mark DESC;'+sLineBreak
        +' '+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| rank | dense_rank | row_num | course | mark | name |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| 1 | 1 | 1 | Biology | 70 | Roger |'+sLineBreak
        +'| 2 | 2 | 2 | Biology | 60 | Bilal |'+sLineBreak
        +'| 1 | 1 | 1 | Maths | 70 | Voitto |'+sLineBreak
        +'| 2 | 2 | 2 | Maths | 60 | Thulile |'+sLineBreak
        +'| 2 | 2 | 3 | Maths | 60 | Pritha |'+sLineBreak
        +'| 4 | 3 | 4 | Maths | 55 | Chun |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'
    ),

    (
      Name:         'FIRST_VALUE';
      Declaration:  '(expr)';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'FIRST_VALUE returns the first result from an ordered set, or'+sLineBreak
        +'NULL if no such result exists.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t1 ('+sLineBreak
        +' pk int primary key,'+sLineBreak
        +' a int,'+sLineBreak
        +' b int,'+sLineBreak
        +' c char(10),'+sLineBreak
        +' d decimal(10, 3),'+sLineBreak
        +' e real'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'( 1, 0, 1, ''one'', 0.1, 0.001),'+sLineBreak
        +'( 2, 0, 2, ''two'', 0.2, 0.002),'+sLineBreak
        +'( 3, 0, 3, ''three'', 0.3, 0.003),'+sLineBreak
        +'( 4, 1, 2, ''three'', 0.4, 0.004),'+sLineBreak
        +'( 5, 1, 1, ''two'', 0.5, 0.005),'+sLineBreak
        +'( 6, 1, 1, ''one'', 0.6, 0.006),'+sLineBreak
        +'( 7, 2, NULL, ''n_one'', 0.5, 0.007),'+sLineBreak
        +'( 8, 2, 1, ''n_two'', NULL, 0.008),'+sLineBreak
        +'( 9, 2, 2, NULL, 0.7, 0.009),'+sLineBreak
        +'(10, 2, 0, ''n_four'', 0.8, 0.010),'+sLineBreak
        +'(11, 2, 10, NULL, 0.9, NULL);'+sLineBreak
        +' '+sLineBreak
        +'SELECT pk, FIRST_VALUE(pk) OVER (ORDER BY pk) AS first_asc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk) AS last_asc,'+sLineBreak
        +' FIRST_VALUE(pk) OVER (ORDER BY pk DESC) AS first_desc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk DESC) AS last_desc'+sLineBreak
        +'FROM t1'+sLineBreak
        +'ORDER BY pk DESC;'+sLineBreak
        +' '+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| pk | first_asc | last_asc | first_desc | last_desc |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| 11 | 1 | 11 | 11 | 11 |'+sLineBreak
        +'| 10 | 1 | 10 | 11 | 10 |'+sLineBreak
        +'| 9 | 1 | 9 | 11 | 9 |'+sLineBreak
        +'| 8 | 1 | 8 | 11 | 8 |'+sLineBreak
        +'| 7 | 1 | 7 | 11 | 7 |'+sLineBreak
        +'| 6 | 1 | 6 | 11 | 6 |'+sLineBreak
        +'| 5 | 1 | 5 | 11 | 5 |'+sLineBreak
        +'| 4 | 1 | 4 | 11 | 4 |'+sLineBreak
        +'| 3 | 1 | 3 | 11 | 3 |'+sLineBreak
        +'| 2 | 1 | 2 | 11 | 2 |'+sLineBreak
        +'| 1 | 1 | 1 | 11 | 1 |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE t1 (i int);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'(1),(2),(3),(4),(5),(6),(7),(8),(9),(10);'+sLineBreak
        +' '+sLineBreak
        +'SELECT i,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW'+sLineBreak
        +'and 1 FOLLOWING) AS f_1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW and'+sLineBreak
        +'1 FOLLOWING) AS l_1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING'+sLineBreak
        +'AND 1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING AND'+sLineBreak
        +'1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING'+sLineBreak
        +'AND 1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND'+sLineBreak
        +'1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING'+sLineBreak
        +'AND 2 FOLLOWING) AS f_1f2f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING AND'+sLineBreak
        +'2 FOLLOWING) AS f_1f2f'+sLineBreak
        +'FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| i | f_1f | l_1f | f_1p1f | f_1p1f | f_2p1p | f_2p1p |'+sLineBreak
        +'f_1f2f | f_1f2f |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| 1 | 1 | 2 | 1 | 2 | NULL | NULL | 2 | 3 |'+sLineBreak
        +'| 2 | 2 | 3 | 1 | 3 | 1 | 1 | 3 | 4 |'+sLineBreak
        +'| 3 | 3 | 4 | 2 | 4 | 1 | 2 | 4 | 5 |'+sLineBreak
        +'| 4 | 4 | 5 | 3 | 5 | 2 | 3 | 5 | 6 |'+sLineBreak
        +'| 5 | 5 | 6 | 4 | 6 | 3 | 4 | 6 | 7 |'+sLineBreak
        +'| 6 | 6 | 7 | 5 | 7 | 4 | 5 | 7 | 8 |'+sLineBreak
        +'| 7 | 7 | 8 | 6 | 8 | 5 | 6 | 8 | 9 |'+sLineBreak
        +'| 8 | 8 | 9 | 7 | 9 | 6 | 7 | 9 | 10 |'+sLineBreak
        +'| 9 | 9 | 10 | 8 | 10 | 7 | 8 | 10 | 10 |'+sLineBreak
        +'| 10 | 10 | 10 | 9 | 10 | 8 | 9 | NULL | NULL |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'
    ),

    (
      Name:         'FIRST_VALUE';
      Declaration:  '(  [ PARTITION BY partition_expression ]  [ ORDER BY order_list ] )';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'FIRST_VALUE returns the first result from an ordered set, or'+sLineBreak
        +'NULL if no such result exists.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE t1 ('+sLineBreak
        +' pk int primary key,'+sLineBreak
        +' a int,'+sLineBreak
        +' b int,'+sLineBreak
        +' c char(10),'+sLineBreak
        +' d decimal(10, 3),'+sLineBreak
        +' e real'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'( 1, 0, 1, ''one'', 0.1, 0.001),'+sLineBreak
        +'( 2, 0, 2, ''two'', 0.2, 0.002),'+sLineBreak
        +'( 3, 0, 3, ''three'', 0.3, 0.003),'+sLineBreak
        +'( 4, 1, 2, ''three'', 0.4, 0.004),'+sLineBreak
        +'( 5, 1, 1, ''two'', 0.5, 0.005),'+sLineBreak
        +'( 6, 1, 1, ''one'', 0.6, 0.006),'+sLineBreak
        +'( 7, 2, NULL, ''n_one'', 0.5, 0.007),'+sLineBreak
        +'( 8, 2, 1, ''n_two'', NULL, 0.008),'+sLineBreak
        +'( 9, 2, 2, NULL, 0.7, 0.009),'+sLineBreak
        +'(10, 2, 0, ''n_four'', 0.8, 0.010),'+sLineBreak
        +'(11, 2, 10, NULL, 0.9, NULL);'+sLineBreak
        +' '+sLineBreak
        +'SELECT pk, FIRST_VALUE(pk) OVER (ORDER BY pk) AS first_asc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk) AS last_asc,'+sLineBreak
        +' FIRST_VALUE(pk) OVER (ORDER BY pk DESC) AS first_desc,'+sLineBreak
        +' LAST_VALUE(pk) OVER (ORDER BY pk DESC) AS last_desc'+sLineBreak
        +'FROM t1'+sLineBreak
        +'ORDER BY pk DESC;'+sLineBreak
        +' '+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| pk | first_asc | last_asc | first_desc | last_desc |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +'| 11 | 1 | 11 | 11 | 11 |'+sLineBreak
        +'| 10 | 1 | 10 | 11 | 10 |'+sLineBreak
        +'| 9 | 1 | 9 | 11 | 9 |'+sLineBreak
        +'| 8 | 1 | 8 | 11 | 8 |'+sLineBreak
        +'| 7 | 1 | 7 | 11 | 7 |'+sLineBreak
        +'| 6 | 1 | 6 | 11 | 6 |'+sLineBreak
        +'| 5 | 1 | 5 | 11 | 5 |'+sLineBreak
        +'| 4 | 1 | 4 | 11 | 4 |'+sLineBreak
        +'| 3 | 1 | 3 | 11 | 3 |'+sLineBreak
        +'| 2 | 1 | 2 | 11 | 2 |'+sLineBreak
        +'| 1 | 1 | 1 | 11 | 1 |'+sLineBreak
        +'+----+-----------+----------+------------+-----------+'+sLineBreak
        +' '+sLineBreak
        +'CREATE OR REPLACE TABLE t1 (i int);'+sLineBreak
        +'INSERT INTO t1 VALUES'+sLineBreak
        +'(1),(2),(3),(4),(5),(6),(7),(8),(9),(10);'+sLineBreak
        +' '+sLineBreak
        +'SELECT i,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW'+sLineBreak
        +'and 1 FOLLOWING) AS f_1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW and'+sLineBreak
        +'1 FOLLOWING) AS l_1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING'+sLineBreak
        +'AND 1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING AND'+sLineBreak
        +'1 FOLLOWING) AS f_1p1f,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING'+sLineBreak
        +'AND 1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND'+sLineBreak
        +'1 PRECEDING) AS f_2p1p,'+sLineBreak
        +' FIRST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING'+sLineBreak
        +'AND 2 FOLLOWING) AS f_1f2f,'+sLineBreak
        +' LAST_VALUE(i) OVER (ORDER BY i ROWS BETWEEN 1 FOLLOWING AND'+sLineBreak
        +'2 FOLLOWING) AS f_1f2f'+sLineBreak
        +'FROM t1;'+sLineBreak
        +' '+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| i | f_1f | l_1f | f_1p1f | f_1p1f | f_2p1p | f_2p1p |'+sLineBreak
        +'f_1f2f | f_1f2f |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'+sLineBreak
        +'| 1 | 1 | 2 | 1 | 2 | NULL | NULL | 2 | 3 |'+sLineBreak
        +'| 2 | 2 | 3 | 1 | 3 | 1 | 1 | 3 | 4 |'+sLineBreak
        +'| 3 | 3 | 4 | 2 | 4 | 1 | 2 | 4 | 5 |'+sLineBreak
        +'| 4 | 4 | 5 | 3 | 5 | 2 | 3 | 5 | 6 |'+sLineBreak
        +'| 5 | 5 | 6 | 4 | 6 | 3 | 4 | 6 | 7 |'+sLineBreak
        +'| 6 | 6 | 7 | 5 | 7 | 4 | 5 | 7 | 8 |'+sLineBreak
        +'| 7 | 7 | 8 | 6 | 8 | 5 | 6 | 8 | 9 |'+sLineBreak
        +'| 8 | 8 | 9 | 7 | 9 | 6 | 7 | 9 | 10 |'+sLineBreak
        +'| 9 | 9 | 10 | 8 | 10 | 7 | 8 | 10 | 10 |'+sLineBreak
        +'| 10 | 10 | 10 | 9 | 10 | 8 | 9 | NULL | NULL |'+sLineBreak
        +'+------+------+------+--------+--------+--------+--------+--------+--------+'
    ),

    (
      Name:         'MEDIAN';
      Declaration:  '(median expression)';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'MEDIAN() is a window function that returns the median value'+sLineBreak
        +'of a range of values.'+sLineBreak
        +' '+sLineBreak
        +'It is a specific case of PERCENTILE_CONT, with an argument'+sLineBreak
        +'of 0.5 and the ORDER BY column the one in MEDIAN''s'+sLineBreak
        +'argument. '+sLineBreak
        +' '+sLineBreak
        +'MEDIAN() OVER ( [ PARTITION BY partition_expression] )'+sLineBreak
        +' '+sLineBreak
        +'Is equivalent to:'+sLineBreak
        +' '+sLineBreak
        +'PERCENTILE_CONT(0.5) WITHIN '+sLineBreak
        +' GROUP (ORDER BY ) OVER ( [ PARTITION BY'+sLineBreak
        +'partition_expression ])'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE book_rating (name CHAR(30), star_rating'+sLineBreak
        +'TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lord of the Ladybirds'','+sLineBreak
        +'5);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lord of the Ladybirds'','+sLineBreak
        +'3);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lady of the Flies'', 1);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lady of the Flies'', 2);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lady of the Flies'', 5);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, median(star_rating) OVER (PARTITION BY name)'+sLineBreak
        +'FROM book_rating;'+sLineBreak
        +' '+sLineBreak
        +'+-----------------------+----------------------------------------------+'+sLineBreak
        +'| name | median(star_rating) OVER (PARTITION BY name) |'+sLineBreak
        +'+-----------------------+----------------------------------------------+'+sLineBreak
        +'| Lord of the Ladybirds | 4.0000000000 |'+sLineBreak
        +'| Lord of the Ladybirds | 4.0000000000 |'+sLineBreak
        +'| Lady of the Flies | 2.0000000000 |'+sLineBreak
        +'| Lady of the Flies | 2.0000000000 |'+sLineBreak
        +'| Lady of the Flies | 2.0000000000 |'+sLineBreak
        +'+-----------------------+----------------------------------------------+'
    ),

    (
      Name:         'MEDIAN';
      Declaration:  '(  [ PARTITION BY partition_expression ]  )';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'MEDIAN() is a window function that returns the median value'+sLineBreak
        +'of a range of values.'+sLineBreak
        +' '+sLineBreak
        +'It is a specific case of PERCENTILE_CONT, with an argument'+sLineBreak
        +'of 0.5 and the ORDER BY column the one in MEDIAN''s'+sLineBreak
        +'argument. '+sLineBreak
        +' '+sLineBreak
        +'MEDIAN() OVER ( [ PARTITION BY partition_expression] )'+sLineBreak
        +' '+sLineBreak
        +'Is equivalent to:'+sLineBreak
        +' '+sLineBreak
        +'PERCENTILE_CONT(0.5) WITHIN '+sLineBreak
        +' GROUP (ORDER BY ) OVER ( [ PARTITION BY'+sLineBreak
        +'partition_expression ])'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE book_rating (name CHAR(30), star_rating'+sLineBreak
        +'TINYINT);'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lord of the Ladybirds'','+sLineBreak
        +'5);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lord of the Ladybirds'','+sLineBreak
        +'3);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lady of the Flies'', 1);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lady of the Flies'', 2);'+sLineBreak
        +'INSERT INTO book_rating VALUES (''Lady of the Flies'', 5);'+sLineBreak
        +' '+sLineBreak
        +'SELECT name, median(star_rating) OVER (PARTITION BY name)'+sLineBreak
        +'FROM book_rating;'+sLineBreak
        +' '+sLineBreak
        +'+-----------------------+----------------------------------------------+'+sLineBreak
        +'| name | median(star_rating) OVER (PARTITION BY name) |'+sLineBreak
        +'+-----------------------+----------------------------------------------+'+sLineBreak
        +'| Lord of the Ladybirds | 4.0000000000 |'+sLineBreak
        +'| Lord of the Ladybirds | 4.0000000000 |'+sLineBreak
        +'| Lady of the Flies | 2.0000000000 |'+sLineBreak
        +'| Lady of the Flies | 2.0000000000 |'+sLineBreak
        +'| Lady of the Flies | 2.0000000000 |'+sLineBreak
        +'+-----------------------+----------------------------------------------+'
    ),

    (
      Name:         'PERCENT_RANK';
      Declaration:  '()';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'PERCENT_RANK() is a window function that returns the'+sLineBreak
        +'relative percent rank of a given row. The following formula'+sLineBreak
        +'is used to calculate the percent rank:'+sLineBreak
        +' '+sLineBreak
        +'(rank - 1) / (number of rows in the window or partition - 1)'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'create table t1 ('+sLineBreak
        +' pk int primary key,'+sLineBreak
        +' a int,'+sLineBreak
        +' b int'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'insert into t1 values'+sLineBreak
        +'( 1 , 0, 10),'+sLineBreak
        +'( 2 , 0, 10),'+sLineBreak
        +'( 3 , 1, 10),'+sLineBreak
        +'( 4 , 1, 10),'+sLineBreak
        +'( 8 , 2, 10),'+sLineBreak
        +'( 5 , 2, 20),'+sLineBreak
        +'( 6 , 2, 20),'+sLineBreak
        +'( 7 , 2, 20),'+sLineBreak
        +'( 9 , 4, 20),'+sLineBreak
        +'(10 , 4, 20);'+sLineBreak
        +' '+sLineBreak
        +'select pk, a, b,'+sLineBreak
        +' rank() over (order by a) as rank,'+sLineBreak
        +' percent_rank() over (order by a) as pct_rank,'+sLineBreak
        +' cume_dist() over (order by a) as cume_dist'+sLineBreak
        +'from t1;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+------+------+--------------+--------------+'+sLineBreak
        +'| pk | a | b | rank | pct_rank | cume_dist |'+sLineBreak
        +'+----+------+------+------+--------------+--------------+'+sLineBreak
        +'| 1 | 0 | 10 | 1 | 0.0000000000 | 0.2000000000 |'+sLineBreak
        +'| 2 | 0 | 10 | 1 | 0.0000000000 | 0.2000000000 |'+sLineBreak
        +'| 3 | 1 | 10 | 3 | 0.2222222222 | 0.4000000000 |'+sLineBreak
        +'| 4 | 1 | 10 | 3 | 0.2222222222 | 0.4000000000 |'+sLineBreak
        +'| 5 | 2 | 20 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 6 | 2 | 20 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 7 | 2 | 20 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 8 | 2 | 10 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 9 | 4 | 20 | 9 | 0.8888888889 | 1.0000000000 |'+sLineBreak
        +'| 10 | 4 | 20 | 9 | 0.8888888889 | 1.0000000000 |'+sLineBreak
        +'+----+------+------+------+--------------+--------------+'+sLineBreak
        +' '+sLineBreak
        +'select pk, a, b,'+sLineBreak
        +' percent_rank() over (order by pk) as pct_rank,'+sLineBreak
        +' cume_dist() over (order by pk) as cume_dist'+sLineBreak
        +'from t1 order by pk;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| pk | a | b | pct_rank | cume_dist |'+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| 1 | 0 | 10 | 0.0000000000 | 0.1000000000 |'+sLineBreak
        +'| 2 | 0 | 10 | 0.1111111111 | 0.2000000000 |'+sLineBreak
        +'| 3 | 1 | 10 | 0.2222222222 | 0.3000000000 |'+sLineBreak
        +'| 4 | 1 | 10 | 0.3333333333 | 0.4000000000 |'+sLineBreak
        +'| 5 | 2 | 20 | 0.4444444444 | 0.5000000000 |'+sLineBreak
        +'| 6 | 2 | 20 | 0.5555555556 | 0.6000000000 |'+sLineBreak
        +'| 7 | 2 | 20 | 0.6666666667 | 0.7000000000 |'+sLineBreak
        +'| 8 | 2 | 10 | 0.7777777778 | 0.8000000000 |'+sLineBreak
        +'| 9 | 4 | 20 | 0.8888888889 | 0.9000000000 |'+sLineBreak
        +'| 10 | 4 | 20 | 1.0000000000 | 1.0000000000 |'+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +' '+sLineBreak
        +'select pk, a, b,'+sLineBreak
        +' percent_rank() over (partition by a order by a) as'+sLineBreak
        +'pct_rank,'+sLineBreak
        +' cume_dist() over (partition by a order by a) as cume_dist'+sLineBreak
        +'from t1;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| pk | a | b | pct_rank | cume_dist |'+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| 1 | 0 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 2 | 0 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 3 | 1 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 4 | 1 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 5 | 2 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 6 | 2 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 7 | 2 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 8 | 2 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 9 | 4 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 10 | 4 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'+----+------+------+--------------+--------------+'
    ),

    (
      Name:         'PERCENT_RANK';
      Declaration:  '(  [ PARTITION BY partition_expression ]   [ ORDER BY order_list ] )';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'PERCENT_RANK() is a window function that returns the'+sLineBreak
        +'relative percent rank of a given row. The following formula'+sLineBreak
        +'is used to calculate the percent rank:'+sLineBreak
        +' '+sLineBreak
        +'(rank - 1) / (number of rows in the window or partition - 1)'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'create table t1 ('+sLineBreak
        +' pk int primary key,'+sLineBreak
        +' a int,'+sLineBreak
        +' b int'+sLineBreak
        +');'+sLineBreak
        +' '+sLineBreak
        +'insert into t1 values'+sLineBreak
        +'( 1 , 0, 10),'+sLineBreak
        +'( 2 , 0, 10),'+sLineBreak
        +'( 3 , 1, 10),'+sLineBreak
        +'( 4 , 1, 10),'+sLineBreak
        +'( 8 , 2, 10),'+sLineBreak
        +'( 5 , 2, 20),'+sLineBreak
        +'( 6 , 2, 20),'+sLineBreak
        +'( 7 , 2, 20),'+sLineBreak
        +'( 9 , 4, 20),'+sLineBreak
        +'(10 , 4, 20);'+sLineBreak
        +' '+sLineBreak
        +'select pk, a, b,'+sLineBreak
        +' rank() over (order by a) as rank,'+sLineBreak
        +' percent_rank() over (order by a) as pct_rank,'+sLineBreak
        +' cume_dist() over (order by a) as cume_dist'+sLineBreak
        +'from t1;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+------+------+--------------+--------------+'+sLineBreak
        +'| pk | a | b | rank | pct_rank | cume_dist |'+sLineBreak
        +'+----+------+------+------+--------------+--------------+'+sLineBreak
        +'| 1 | 0 | 10 | 1 | 0.0000000000 | 0.2000000000 |'+sLineBreak
        +'| 2 | 0 | 10 | 1 | 0.0000000000 | 0.2000000000 |'+sLineBreak
        +'| 3 | 1 | 10 | 3 | 0.2222222222 | 0.4000000000 |'+sLineBreak
        +'| 4 | 1 | 10 | 3 | 0.2222222222 | 0.4000000000 |'+sLineBreak
        +'| 5 | 2 | 20 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 6 | 2 | 20 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 7 | 2 | 20 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 8 | 2 | 10 | 5 | 0.4444444444 | 0.8000000000 |'+sLineBreak
        +'| 9 | 4 | 20 | 9 | 0.8888888889 | 1.0000000000 |'+sLineBreak
        +'| 10 | 4 | 20 | 9 | 0.8888888889 | 1.0000000000 |'+sLineBreak
        +'+----+------+------+------+--------------+--------------+'+sLineBreak
        +' '+sLineBreak
        +'select pk, a, b,'+sLineBreak
        +' percent_rank() over (order by pk) as pct_rank,'+sLineBreak
        +' cume_dist() over (order by pk) as cume_dist'+sLineBreak
        +'from t1 order by pk;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| pk | a | b | pct_rank | cume_dist |'+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| 1 | 0 | 10 | 0.0000000000 | 0.1000000000 |'+sLineBreak
        +'| 2 | 0 | 10 | 0.1111111111 | 0.2000000000 |'+sLineBreak
        +'| 3 | 1 | 10 | 0.2222222222 | 0.3000000000 |'+sLineBreak
        +'| 4 | 1 | 10 | 0.3333333333 | 0.4000000000 |'+sLineBreak
        +'| 5 | 2 | 20 | 0.4444444444 | 0.5000000000 |'+sLineBreak
        +'| 6 | 2 | 20 | 0.5555555556 | 0.6000000000 |'+sLineBreak
        +'| 7 | 2 | 20 | 0.6666666667 | 0.7000000000 |'+sLineBreak
        +'| 8 | 2 | 10 | 0.7777777778 | 0.8000000000 |'+sLineBreak
        +'| 9 | 4 | 20 | 0.8888888889 | 0.9000000000 |'+sLineBreak
        +'| 10 | 4 | 20 | 1.0000000000 | 1.0000000000 |'+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +' '+sLineBreak
        +'select pk, a, b,'+sLineBreak
        +' percent_rank() over (partition by a order by a) as'+sLineBreak
        +'pct_rank,'+sLineBreak
        +' cume_dist() over (partition by a order by a) as cume_dist'+sLineBreak
        +'from t1;'+sLineBreak
        +' '+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| pk | a | b | pct_rank | cume_dist |'+sLineBreak
        +'+----+------+------+--------------+--------------+'+sLineBreak
        +'| 1 | 0 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 2 | 0 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 3 | 1 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 4 | 1 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 5 | 2 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 6 | 2 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 7 | 2 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 8 | 2 | 10 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 9 | 4 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'| 10 | 4 | 20 | 0.0000000000 | 1.0000000000 |'+sLineBreak
        +'+----+------+------+--------------+--------------+'
    ),

    (
      Name:         'RANK';
      Declaration:  '()';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'RANK() is a window function that displays the number of a'+sLineBreak
        +'given row, starting at one and following the ORDER BY'+sLineBreak
        +'sequence of the window function, with identical values'+sLineBreak
        +'receiving the same result. It is similar to the ROW_NUMBER()'+sLineBreak
        +'function except that in that function, identical values will'+sLineBreak
        +'receive a different row number for each result.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The distinction between DENSE_RANK(), RANK() and'+sLineBreak
        +'ROW_NUMBER():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student(course VARCHAR(10), mark int, name'+sLineBreak
        +'varchar(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Maths'', 60, ''Thulile''),'+sLineBreak
        +' (''Maths'', 60, ''Pritha''),'+sLineBreak
        +' (''Maths'', 70, ''Voitto''),'+sLineBreak
        +' (''Maths'', 55, ''Chun''),'+sLineBreak
        +' (''Biology'', 60, ''Bilal''),'+sLineBreak
        +' (''Biology'', 70, ''Roger'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT '+sLineBreak
        +' RANK() OVER (PARTITION BY course ORDER BY mark DESC) AS'+sLineBreak
        +'rank, '+sLineBreak
        +' DENSE_RANK() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS dense_rank, '+sLineBreak
        +' ROW_NUMBER() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS row_num, '+sLineBreak
        +' course, mark, name '+sLineBreak
        +'FROM student ORDER BY course, mark DESC;'+sLineBreak
        +' '+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| rank | dense_rank | row_num | course | mark | name |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| 1 | 1 | 1 | Biology | 70 | Roger |'+sLineBreak
        +'| 2 | 2 | 2 | Biology | 60 | Bilal |'+sLineBreak
        +'| 1 | 1 | 1 | Maths | 70 | Voitto |'+sLineBreak
        +'| 2 | 2 | 2 | Maths | 60 | Thulile |'+sLineBreak
        +'| 2 | 2 | 3 | Maths | 60 | Pritha |'+sLineBreak
        +'| 4 | 3 | 4 | Maths | 55 | Chun |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'
    ),

    (
      Name:         'RANK';
      Declaration:  '(  [ PARTITION BY partition_expression ]  [ ORDER BY order_list ] )';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'RANK() is a window function that displays the number of a'+sLineBreak
        +'given row, starting at one and following the ORDER BY'+sLineBreak
        +'sequence of the window function, with identical values'+sLineBreak
        +'receiving the same result. It is similar to the ROW_NUMBER()'+sLineBreak
        +'function except that in that function, identical values will'+sLineBreak
        +'receive a different row number for each result.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The distinction between DENSE_RANK(), RANK() and'+sLineBreak
        +'ROW_NUMBER():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student(course VARCHAR(10), mark int, name'+sLineBreak
        +'varchar(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Maths'', 60, ''Thulile''),'+sLineBreak
        +' (''Maths'', 60, ''Pritha''),'+sLineBreak
        +' (''Maths'', 70, ''Voitto''),'+sLineBreak
        +' (''Maths'', 55, ''Chun''),'+sLineBreak
        +' (''Biology'', 60, ''Bilal''),'+sLineBreak
        +' (''Biology'', 70, ''Roger'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT '+sLineBreak
        +' RANK() OVER (PARTITION BY course ORDER BY mark DESC) AS'+sLineBreak
        +'rank, '+sLineBreak
        +' DENSE_RANK() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS dense_rank, '+sLineBreak
        +' ROW_NUMBER() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS row_num, '+sLineBreak
        +' course, mark, name '+sLineBreak
        +'FROM student ORDER BY course, mark DESC;'+sLineBreak
        +' '+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| rank | dense_rank | row_num | course | mark | name |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| 1 | 1 | 1 | Biology | 70 | Roger |'+sLineBreak
        +'| 2 | 2 | 2 | Biology | 60 | Bilal |'+sLineBreak
        +'| 1 | 1 | 1 | Maths | 70 | Voitto |'+sLineBreak
        +'| 2 | 2 | 2 | Maths | 60 | Thulile |'+sLineBreak
        +'| 2 | 2 | 3 | Maths | 60 | Pritha |'+sLineBreak
        +'| 4 | 3 | 4 | Maths | 55 | Chun |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'
    ),

    (
      Name:         'ROW_NUMBER';
      Declaration:  '()';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'ROW_NUMBER() is a window function that displays the number'+sLineBreak
        +'of a given row, starting at one and following the ORDER BY'+sLineBreak
        +'sequence of the window function, with identical values'+sLineBreak
        +'receiving different row numbers. It is similar to the RANK()'+sLineBreak
        +'and DENSE_RANK() functions except that in that function,'+sLineBreak
        +'identical values will receive the same rank for each result.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The distinction between DENSE_RANK(), RANK() and'+sLineBreak
        +'ROW_NUMBER():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student(course VARCHAR(10), mark int, name'+sLineBreak
        +'varchar(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Maths'', 60, ''Thulile''),'+sLineBreak
        +' (''Maths'', 60, ''Pritha''),'+sLineBreak
        +' (''Maths'', 70, ''Voitto''),'+sLineBreak
        +' (''Maths'', 55, ''Chun''),'+sLineBreak
        +' (''Biology'', 60, ''Bilal''),'+sLineBreak
        +' (''Biology'', 70, ''Roger'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT '+sLineBreak
        +' RANK() OVER (PARTITION BY course ORDER BY mark DESC) AS'+sLineBreak
        +'rank, '+sLineBreak
        +' DENSE_RANK() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS dense_rank, '+sLineBreak
        +' ROW_NUMBER() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS row_num, '+sLineBreak
        +' course, mark, name '+sLineBreak
        +'FROM student ORDER BY course, mark DESC;'+sLineBreak
        +' '+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| rank | dense_rank | row_num | course | mark | name |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| 1 | 1 | 1 | Biology | 70 | Roger |'+sLineBreak
        +'| 2 | 2 | 2 | Biology | 60 | Bilal |'+sLineBreak
        +'| 1 | 1 | 1 | Maths | 70 | Voitto |'+sLineBreak
        +'| 2 | 2 | 2 | Maths | 60 | Thulile |'+sLineBreak
        +'| 2 | 2 | 3 | Maths | 60 | Pritha |'+sLineBreak
        +'| 4 | 3 | 4 | Maths | 55 | Chun |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'
    ),

    (
      Name:         'ROW_NUMBER';
      Declaration:  '(  [ PARTITION BY partition_expression ]  [ ORDER BY order_list ] )';
      Category:     'Window Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'ROW_NUMBER() is a window function that displays the number'+sLineBreak
        +'of a given row, starting at one and following the ORDER BY'+sLineBreak
        +'sequence of the window function, with identical values'+sLineBreak
        +'receiving different row numbers. It is similar to the RANK()'+sLineBreak
        +'and DENSE_RANK() functions except that in that function,'+sLineBreak
        +'identical values will receive the same rank for each result.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'The distinction between DENSE_RANK(), RANK() and'+sLineBreak
        +'ROW_NUMBER():'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE student(course VARCHAR(10), mark int, name'+sLineBreak
        +'varchar(10));'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO student VALUES '+sLineBreak
        +' (''Maths'', 60, ''Thulile''),'+sLineBreak
        +' (''Maths'', 60, ''Pritha''),'+sLineBreak
        +' (''Maths'', 70, ''Voitto''),'+sLineBreak
        +' (''Maths'', 55, ''Chun''),'+sLineBreak
        +' (''Biology'', 60, ''Bilal''),'+sLineBreak
        +' (''Biology'', 70, ''Roger'');'+sLineBreak
        +' '+sLineBreak
        +'SELECT '+sLineBreak
        +' RANK() OVER (PARTITION BY course ORDER BY mark DESC) AS'+sLineBreak
        +'rank, '+sLineBreak
        +' DENSE_RANK() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS dense_rank, '+sLineBreak
        +' ROW_NUMBER() OVER (PARTITION BY course ORDER BY mark DESC)'+sLineBreak
        +'AS row_num, '+sLineBreak
        +' course, mark, name '+sLineBreak
        +'FROM student ORDER BY course, mark DESC;'+sLineBreak
        +' '+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| rank | dense_rank | row_num | course | mark | name |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'+sLineBreak
        +'| 1 | 1 | 1 | Biology | 70 | Roger |'+sLineBreak
        +'| 2 | 2 | 2 | Biology | 60 | Bilal |'+sLineBreak
        +'| 1 | 1 | 1 | Maths | 70 | Voitto |'+sLineBreak
        +'| 2 | 2 | 2 | Maths | 60 | Thulile |'+sLineBreak
        +'| 2 | 2 | 3 | Maths | 60 | Pritha |'+sLineBreak
        +'| 4 | 3 | 4 | Maths | 55 | Chun |'+sLineBreak
        +'+------+------------+---------+---------+------+---------+'
    ),

    (
      Name:         'MLineFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTILINESTRING value using its WKB'+sLineBreak
        +'representation and SRID.'+sLineBreak
        +' '+sLineBreak
        +'MLineFromWKB() and MultiLineStringFromWKB() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_AsBinary(MLineFromText(''MULTILINESTRING((10'+sLineBreak
        +'48,10 21,10 0),(16 0,16 23,16 48))''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(MLineFromWKB(@g));'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +'| ST_AsText(MLineFromWKB(@g)) |'+sLineBreak
        +'+--------------------------------------------------------+'+sLineBreak
        +'| MULTILINESTRING((10 48,10 21,10 0),(16 0,16 23,16 48)) |'+sLineBreak
        +'+--------------------------------------------------------+'
    ),

    (
      Name:         'MPointFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOINT value using its WKB representation'+sLineBreak
        +'and SRID.'+sLineBreak
        +' '+sLineBreak
        +'MPointFromWKB() and MultiPointFromWKB() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_AsBinary(MPointFromText(''MultiPoint( 1 1, 2 2,'+sLineBreak
        +'5 3, 7 2, 9 3, 8 4, 6 6, 6 9, 4 9, 1 5 )''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(MPointFromWKB(@g));'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| ST_AsText(MPointFromWKB(@g)) |'+sLineBreak
        +'+-----------------------------------------------------+'+sLineBreak
        +'| MULTIPOINT(1 1,2 2,5 3,7 2,9 3,8 4,6 6,6 9,4 9,1 5) |'+sLineBreak
        +'+-----------------------------------------------------+'
    ),

    (
      Name:         'MPolyFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOLYGON value using its WKB representation'+sLineBreak
        +'and SRID.'+sLineBreak
        +' '+sLineBreak
        +'MPolyFromWKB() and MultiPolygonFromWKB() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_AsBinary(MPointFromText(''MULTIPOLYGON(((28'+sLineBreak
        +'26,28 0,84 0,84 42,28 26),(52 18,66 23,73 9,48 6,52'+sLineBreak
        +'18)),((59 18,67 18,67 13,59 13,59 18)))''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(MPolyFromWKB(@g));'+sLineBreak
        +'+---------------------------------------------------------------------------------------------------------------+'+sLineBreak
        +'| ST_AsText(MPolyFromWKB(@g)) |'+sLineBreak
        +'+---------------------------------------------------------------------------------------------------------------+'+sLineBreak
        +'| MULTIPOLYGON(((28 26,28 0,84 0,84 42,28 26),(52 18,66'+sLineBreak
        +'23,73 9,48 6,52 18)),((59 18,67 18,67 13,59 13,59 18))) |'+sLineBreak
        +'+---------------------------------------------------------------------------------------------------------------+'
    ),

    (
      Name:         'ST_AsBinary';
      Declaration:  '(g)';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Converts a value in internal geometry format to its WKB'+sLineBreak
        +'representation and returns the binary result.'+sLineBreak
        +' '+sLineBreak
        +'ST_AsBinary(), AsBinary(), ST_AsWKB() and AsWKB() are'+sLineBreak
        +'synonyms,'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @poly = ST_GeomFromText(''POLYGON((0 0,0 1,1 1,1 0,0'+sLineBreak
        +'0))'');'+sLineBreak
        +'SELECT ST_AsBinary(@poly);'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_GeomFromWKB(ST_AsWKB(@poly)));'+sLineBreak
        +'+--------------------------------------------+'+sLineBreak
        +'| ST_AsText(ST_GeomFromWKB(ST_AsWKB(@poly))) |'+sLineBreak
        +'+--------------------------------------------+'+sLineBreak
        +'| POLYGON((0 0,0 1,1 1,1 0,0 0)) |'+sLineBreak
        +'+--------------------------------------------+'
    ),

    (
      Name:         'ST_GeomCollFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a GEOMETRYCOLLECTION value using its WKB'+sLineBreak
        +'representation and SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_GeomCollFromWKB(), ST_GeometryCollectionFromWKB(),'+sLineBreak
        +'GeomCollFromWKB() and GeometryCollectionFromWKB() are'+sLineBreak
        +'synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g ='+sLineBreak
        +'ST_AsBinary(ST_GeomFromText(''GEOMETRYCOLLECTION(POLYGON((5'+sLineBreak
        +'5,10 5,10 10,5 5)),POINT(10 10))''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_GeomCollFromWKB(@g));'+sLineBreak
        +'+----------------------------------------------------------------+'+sLineBreak
        +'| ST_AsText(ST_GeomCollFromWKB(@g)) |'+sLineBreak
        +'+----------------------------------------------------------------+'+sLineBreak
        +'| GEOMETRYCOLLECTION(POLYGON((5 5,10 5,10 10,5 5)),POINT(10'+sLineBreak
        +'10)) |'+sLineBreak
        +'+----------------------------------------------------------------+'
    ),

    (
      Name:         'ST_GeomFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a geometry value of any type using its WKB'+sLineBreak
        +'representation and SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_GeomFromWKB(), ST_GeometryFromWKB(), GeomFromWKB() and'+sLineBreak
        +'GeometryFromWKB() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_AsBinary(ST_LineFromText(''LINESTRING(0 4, 4'+sLineBreak
        +'6)''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_GeomFromWKB(@g));'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| ST_AsText(ST_GeomFromWKB(@g)) |'+sLineBreak
        +'+-------------------------------+'+sLineBreak
        +'| LINESTRING(0 4,4 6) |'+sLineBreak
        +'+-------------------------------+'
    ),

    (
      Name:         'ST_LineFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a LINESTRING value using its WKB representation'+sLineBreak
        +'and SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_LineFromWKB(), LineFromWKB(), ST_LineStringFromWKB(), and'+sLineBreak
        +'LineStringFromWKB() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_AsBinary(ST_LineFromText(''LineString(0 4,4'+sLineBreak
        +'6)''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_LineFromWKB(@g)) AS l;'+sLineBreak
        +' '+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| l |'+sLineBreak
        +'+---------------------+'+sLineBreak
        +'| LINESTRING(0 4,4 6) |'+sLineBreak
        +'+---------------------+'
    ),

    (
      Name:         'ST_PointFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POINT value using its WKB representation and'+sLineBreak
        +'SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_PointFromWKB() and PointFromWKB() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_AsBinary(ST_PointFromText(''POINT(0 4)''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_PointFromWKB(@g)) AS p;'+sLineBreak
        +' '+sLineBreak
        +'+------------+'+sLineBreak
        +'| p |'+sLineBreak
        +'+------------+'+sLineBreak
        +'| POINT(0 4) |'+sLineBreak
        +'+------------+'
    ),

    (
      Name:         'ST_PolyFromWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'WKB';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POLYGON value using its WKB representation and'+sLineBreak
        +'SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_PolyFromWKB(), ST_PolygonFromWKB(), PolyFromWKB() and'+sLineBreak
        +'PolygonFromWKB() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ST_AsBinary(ST_PolyFromText(''POLYGON((1 1,1 5,4'+sLineBreak
        +'9,6 9,9 3,7 2,1 1))''));'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_PolyFromWKB(@g)) AS p;'+sLineBreak
        +' '+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| p |'+sLineBreak
        +'+----------------------------------------+'+sLineBreak
        +'| POLYGON((1 1,1 5,4 9,6 9,9 3,7 2,1 1)) |'+sLineBreak
        +'+----------------------------------------+'
    ),

    (
      Name:         'MLineFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTILINESTRING value using its WKT'+sLineBreak
        +'representation and SRID.'+sLineBreak
        +' '+sLineBreak
        +'MLineFromText() and MultiLineStringFromText() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE gis_multi_line (g MULTILINESTRING);'+sLineBreak
        +'SHOW FIELDS FROM gis_multi_line;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO gis_multi_line VALUES'+sLineBreak
        +' (MultiLineStringFromText(''MULTILINESTRING((10 48,10 21,10'+sLineBreak
        +'0),(16 0,16 23,16 48))'')),'+sLineBreak
        +' (MLineFromText(''MULTILINESTRING((10 48,10 21,10 0))'')),'+sLineBreak
        +' (MLineFromWKB(AsWKB(MultiLineString(LineString(Point(1, 2),'+sLineBreak
        +'Point(3, 5)), LineString(Point(2, 5), Point(5, 8), Point(21,'+sLineBreak
        +'7))))));'
    ),

    (
      Name:         'MPointFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOINT value using its WKT representation'+sLineBreak
        +'and SRID.'+sLineBreak
        +' '+sLineBreak
        +'MPointFromText() and MultiPointFromText() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE gis_multi_point (g MULTIPOINT);'+sLineBreak
        +'SHOW FIELDS FROM gis_multi_point;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO gis_multi_point VALUES'+sLineBreak
        +' (MultiPointFromText(''MULTIPOINT(0 0,10 10,10 20,20'+sLineBreak
        +'20)'')),'+sLineBreak
        +' (MPointFromText(''MULTIPOINT(1 1,11 11,11 21,21 21)'')),'+sLineBreak
        +' (MPointFromWKB(AsWKB(MultiPoint(Point(3, 6), Point(4,'+sLineBreak
        +'10)))));'
    ),

    (
      Name:         'MPolyFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOLYGON value using its WKT representation'+sLineBreak
        +'and SRID.'+sLineBreak
        +' '+sLineBreak
        +'MPolyFromText() and MultiPolygonFromText() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE gis_multi_polygon (g MULTIPOLYGON);'+sLineBreak
        +'SHOW FIELDS FROM gis_multi_polygon;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO gis_multi_polygon VALUES'+sLineBreak
        +' (MultiPolygonFromText(''MULTIPOLYGON(((28 26,28 0,84 0,84'+sLineBreak
        +'42,28 26),(52 18,66 23,73 9,48 6,52 18)),((59 18,67 18,67'+sLineBreak
        +'13,59 13,59 18)))'')),'+sLineBreak
        +' (MPolyFromText(''MULTIPOLYGON(((28 26,28 0,84 0,84 42,28'+sLineBreak
        +'26),(52 18,66 23,73 9,48 6,52 18)),((59 18,67 18,67 13,59'+sLineBreak
        +'13,59 18)))'')),'+sLineBreak
        +' (MPolyFromWKB(AsWKB(MultiPolygon(Polygon(LineString(Point(0,'+sLineBreak
        +'3), Point(3, 3), Point(3, 0), Point(0, 3)))))));'
    ),

    (
      Name:         'ST_AsText';
      Declaration:  '(g)';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Converts a value in internal geometry format to its WKT'+sLineBreak
        +'representation and returns the string result.'+sLineBreak
        +' '+sLineBreak
        +'ST_AsText(), AsText(), ST_AsWKT() and AsWKT() are all'+sLineBreak
        +'synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'SET @g = ''LineString(1 1,4 4,6 6)'';'+sLineBreak
        +' '+sLineBreak
        +'SELECT ST_AsText(ST_GeomFromText(@g));'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| ST_AsText(ST_GeomFromText(@g)) |'+sLineBreak
        +'+--------------------------------+'+sLineBreak
        +'| LINESTRING(1 1,4 4,6 6) |'+sLineBreak
        +'+--------------------------------+'
    ),

    (
      Name:         'ST_GeomCollFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a GEOMETRYCOLLECTION value using its WKT '+sLineBreak
        +'representation and SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_GeomCollFromText(), ST_GeometryCollectionFromText(),'+sLineBreak
        +'GeomCollFromText() and GeometryCollectionFromText() are all'+sLineBreak
        +'synonyms.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'CREATE TABLE gis_geometrycollection (g GEOMETRYCOLLECTION);'+sLineBreak
        +'SHOW FIELDS FROM gis_geometrycollection;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO gis_geometrycollection VALUES'+sLineBreak
        +' (GeomCollFromText(''GEOMETRYCOLLECTION(POINT(0 0),'+sLineBreak
        +'LINESTRING(0 0,10 10))'')),'+sLineBreak
        +' (GeometryFromWKB(AsWKB(GeometryCollection(Point(44, 6),'+sLineBreak
        +'LineString(Point(3, 6), Point(7, 9)))))),'+sLineBreak
        +' (GeomFromText(''GeometryCollection()'')),'+sLineBreak
        +' (GeomFromText(''GeometryCollection EMPTY''));'
    ),

    (
      Name:         'ST_GeomFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a geometry value of any type using its WKT'+sLineBreak
        +'representation and SRID.'+sLineBreak
        +' '+sLineBreak
        +'GeomFromText(), GeometryFromText(), ST_GeomFromText() and'+sLineBreak
        +'ST_GeometryFromText() are all synonyms.'+sLineBreak
        +' '+sLineBreak
        +'Example'+sLineBreak
        +' '+sLineBreak
        +'SET @g = ST_GEOMFROMTEXT(''POLYGON((1 1,1 5,4 9,6 9,9 3,7'+sLineBreak
        +'2,1 1))'');'
    ),

    (
      Name:         'ST_LineFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a LINESTRING value using its WKT representation'+sLineBreak
        +'and SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_LineFromText(), ST_LineStringFromText(),'+sLineBreak
        +'ST_LineFromText() and ST_LineStringFromText() are all'+sLineBreak
        +'synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE gis_line (g LINESTRING);'+sLineBreak
        +'SHOW FIELDS FROM gis_line;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO gis_line VALUES'+sLineBreak
        +' (LineFromText(''LINESTRING(0 0,0 10,10 0)'')),'+sLineBreak
        +' (LineStringFromText(''LINESTRING(10 10,20 10,20 20,10 20,10'+sLineBreak
        +'10)'')),'+sLineBreak
        +' (LineStringFromWKB(AsWKB(LineString(Point(10, 10),'+sLineBreak
        +'Point(40, 10)))));'
    ),

    (
      Name:         'ST_PointFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POINT value using its WKT representation and'+sLineBreak
        +'SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_PointFromText() and PointFromText() are synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE gis_point (g POINT);'+sLineBreak
        +'SHOW FIELDS FROM gis_point;'+sLineBreak
        +' '+sLineBreak
        +'INSERT INTO gis_point VALUES'+sLineBreak
        +' (PointFromText(''POINT(10 10)'')),'+sLineBreak
        +' (PointFromText(''POINT(20 10)'')),'+sLineBreak
        +' (PointFromText(''POINT(20 20)'')),'+sLineBreak
        +' (PointFromWKB(AsWKB(PointFromText(''POINT(10 20)''))));'
    ),

    (
      Name:         'ST_PolyFromText';
      Declaration:  '(wkt[,srid])';
      Category:     'WKT';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POLYGON value using its WKT representation and'+sLineBreak
        +'SRID.'+sLineBreak
        +' '+sLineBreak
        +'ST_PolyFromText(), ST_PolygonFromText(), PolyFromText() and'+sLineBreak
        +'ST_PolygonFromText() are all synonyms.'+sLineBreak
        +' '+sLineBreak
        +''+sLineBreak
        +'CREATE TABLE gis_polygon (g POLYGON);'+sLineBreak
        +'INSERT INTO gis_polygon VALUES'+sLineBreak
        +' (PolygonFromText(''POLYGON((10 10,20 10,20 20,10 20,10'+sLineBreak
        +'10))'')),'+sLineBreak
        +' (PolyFromText(''POLYGON((0 0,50 0,50 50,0 50,0 0), (10'+sLineBreak
        +'10,20 10,20 20,10 20,10 10))''));'
    )

  );


  MySQLVariables: array [0..417] of TServerVariable =
  (
    (
      Name: 'auto_increment_increment';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'auto_increment_offset';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'autocommit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'automatic_sp_privileges';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'back_log';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'basedir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'big_tables';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'binlog_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'binlog_checksum';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'binlog_direct_non_transactional_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'binlog_format';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'ROW,STATEMENT,MIXED';
    ),
    (
      Name: 'binlog_row_image';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FULL,MINIMAL,NOBLOB';
    ),
    (
      Name: 'binlog_stmt_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'bulk_insert_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_client';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_connection';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_database[a]';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_filesystem';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_results';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_server';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_system';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'character_sets_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'collation_connection';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'collation_database[b]';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'collation_server';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'completion_type';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'NO_CHAIN,CHAIN,RELEASE,0,1,2';
    ),
    (
      Name: 'concurrent_insert';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'NEVER,AUTO,ALWAYS,0,1,2';
    ),
    (
      Name: 'connect_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'datadir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'date_format';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'datetime_format';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'debug';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'debug_sync';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'default_storage_engine';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FEDERATED,MRG_MYISAM,MyISAM,BLACKHOLE,CSV,MEMORY,ARCHIVE,InnoDB';
    ),
    (
      Name: 'default_tmp_storage_engine';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FEDERATED,MRG_MYISAM,MyISAM,BLACKHOLE,CSV,MEMORY,ARCHIVE,InnoDB';
    ),
    (
      Name: 'default_week_format';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'delay_key_write';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'ON,OFF,ALL';
    ),
    (
      Name: 'delayed_insert_limit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'delayed_insert_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'delayed_queue_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'disable_gtid_unsafe_statements';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'div_precision_increment';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'end_markers_in_json';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'engine_condition_pushdown';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'eq_range_index_dive_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'error_count';
      IsDynamic: False;
      VarScope: vsSession;
    ),
    (
      Name: 'event_scheduler';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'ON,OFF,DISABLED';
    ),
    (
      Name: 'expire_logs_days';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'external_user';
      IsDynamic: False;
      VarScope: vsSession;
    ),
    (
      Name: 'flush';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'flush_time';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'foreign_key_checks';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'ft_boolean_syntax';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_max_word_len';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_min_word_len';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_query_expansion_limit';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_stopword_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'general_log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'general_log_file';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'group_concat_max_len';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'gtid_done';
      IsDynamic: False;
      VarScope: vsBoth;
    ),
    (
      Name: 'gtid_lost';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'gtid_mode';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'gtid_mode';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'gtid_next';
      IsDynamic: True;
      VarScope: vsSession;
      EnumValues: 'AUTOMATIC,ANONYMOUS';
    ),
    (
      Name: 'gtid_owned';
      IsDynamic: False;
      VarScope: vsBoth;
    ),
    (
      Name: 'have_compress';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_crypt';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_csv';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_dynamic_loading';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_geometry';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_innodb';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_ndbcluster';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_openssl';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_partitioning';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_profiling';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_query_cache';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_rtree_keys';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_ssl';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_symlink';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'host_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'hostname';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'identity';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'ignore_builtin_innodb';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'init_connect';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'init_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'init_slave';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_adaptive_flushing';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_adaptive_hash_index';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_adaptive_max_sleep_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_additional_mem_pool_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_analyze_is_persistent';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_api_enable_binlog';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_api_enable_mdl';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_api_trx_level';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_autoextend_increment';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_autoinc_lock_mode';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_dump_at_shutdown';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_dump_now';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_filename';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_load_abort';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_load_at_startup';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_load_now';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_change_buffer_max_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_change_buffering';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'INSERTS,DELETES,PURGES,CHANGES,ALL,NONE';
    ),
    (
      Name: 'innodb_checksum_algorithm';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'INNODB,CRC32,NONE,STRICT_INNODB,STRICT_CRC32,STRICT_NONE';
    ),
    (
      Name: 'innodb_checksums';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_commit_concurrency';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_concurrency_tickets';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_data_file_path';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_data_home_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_doublewrite';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_fast_shutdown';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_format';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_format_check';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_format_max';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_per_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_flush_log_at_trx_commit';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: '0,1,2';
    ),
    (
      Name: 'innodb_flush_method';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_flush_neighbors';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_force_load_corrupted';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_force_recovery';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_aux_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_enable_stopword';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_max_token_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_min_token_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_num_word_optimize';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_server_stopword_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_sort_pll_degree';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_user_stopword_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_io_capacity';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_large_prefix';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_lock_wait_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_locks_unsafe_for_binlog';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_buffer_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_file_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_files_in_group';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_group_home_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_lru_scan_depth';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_max_dirty_pages_pct';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_max_purge_lag';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_mirrored_log_groups';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_disable';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_enable';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_reset';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_reset_all';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_old_blocks_pct';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_old_blocks_time';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_open_files';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_optimize_fulltext_only';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_page_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_print_all_deadlocks';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_purge_batch_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_purge_threads';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_random_read_ahead';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_read_ahead_threshold';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_read_io_threads';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_replication_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_rollback_on_timeout';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_rollback_segments';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_sort_buffer_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_spin_wait_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_method';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'NULLS_EQUAL,NULLS_UNEQUAL,NULLS_IGNORED';
    ),
    (
      Name: 'innodb_stats_on_metadata';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_persistent_sample_pages';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_sample_pages';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_transient_sample_pages';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_strict_mode';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_support_xa';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_sync_array_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_sync_spin_loops';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_table_locks';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_thread_concurrency';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_thread_sleep_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_undo_directory';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_undo_logs';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_undo_tablespaces';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_use_native_aio';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_use_sys_malloc';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_version';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_write_io_threads';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'insert_id';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'interactive_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'join_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'keep_files_on_create';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'key_buffer_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'key_cache_age_threshold';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'key_cache_block_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'key_cache_division_limit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'language';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'large_files_support';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'large_page_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'large_pages';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'last_insert_id';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'lc_messages';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'lc_messages_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'lc_time_names';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'license';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'local_infile';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'lock_wait_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'locked_in_memory';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_bin';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_bin_basename';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_error';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_output';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_queries_not_using_indexes';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_slave_updates';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_slow_queries';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_throttle_queries_not_using_indexes';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_warnings';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'long_query_time';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'low_priority_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'lower_case_file_system';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'lower_case_table_names';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'master_info_repository';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'master_verify_checksum';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_allowed_packet';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_binlog_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_binlog_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_binlog_stmt_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_connect_errors';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_connections';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_delayed_threads';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_error_count';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_heap_table_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_insert_delayed_threads';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_join_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_length_for_sort_data';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_prepared_stmt_count';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_relay_log_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_seeks_for_key';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_sort_length';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_sp_recursion_depth';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_tmp_tables';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_user_connections';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_write_lock_count';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'memlock';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'metadata_locks_cache_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_data_pointer_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_max_sort_file_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_mmap_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_recover_options';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_repair_threads';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'myisam_sort_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'myisam_stats_method';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'NULLS_EQUAL,NULLS_UNEQUAL,NULLS_IGNORED';
    ),
    (
      Name: 'myisam_use_mmap';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'named_pipe';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'net_buffer_length';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'net_read_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'net_retry_count';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'net_write_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'new';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'old';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'old_alter_table';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'old_passwords';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'open_files_limit';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'optimizer_join_cache_level';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_prune_level';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_search_depth';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_switch';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_features';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_max_mem_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_offset';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'have_partitioning';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_accounts_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_digests_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_stages_history_long_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_stages_history_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_statements_history_long_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_statements_history_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_waits_history_long_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_waits_history_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_hosts_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_cond_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_cond_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_file_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_file_handles';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_file_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_mutex_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_mutex_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_rwlock_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_rwlock_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_socket_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_socket_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_stage_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_statement_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_table_handles';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_table_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_thread_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_thread_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_setup_actors_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_setup_objects_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_users_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'pid_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'plugin_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'port';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'preload_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'profiling';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'profiling_history_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'protocol_version';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'proxy_user';
      IsDynamic: False;
      VarScope: vsSession;
    ),
    (
      Name: 'pseudo_thread_id';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'query_alloc_block_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'query_cache_limit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'query_cache_min_res_unit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'query_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'query_cache_type';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: '0,1,2';
    ),
    (
      Name: 'query_cache_wlock_invalidate';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'query_prealloc_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'rand_seed1';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'rand_seed2';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'range_alloc_block_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'read_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'read_only';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'read_rnd_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'relay_log_basename';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_index';
      IsDynamic: False;
      VarScope: vsBoth;
    ),
    (
      Name: 'relay_log_index';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_info_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_info_repository';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_purge';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_recovery';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_space_limit';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_host';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_password';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_port';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_user';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_enabled';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_trace_level';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_wait_no_slave';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_slave_enabled';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_slave_trace_level';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'secure_auth';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'secure_file_priv';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'server_id';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'server_uuid';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'shared_memory';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'shared_memory_base_name';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_external_locking';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_name_resolve';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_networking';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_show_database';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_compressed_protocol';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_exec_mode';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'IDEMPOTENT,STRICT';
    ),
    (
      Name: 'slave_load_tmpdir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_net_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_parallel_workers';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_skip_errors';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_sql_verify_checksum';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_transaction_retries';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_type_conversions';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slow_launch_time';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slow_query_log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slow_query_log_file';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'socket';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sort_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_auto_is_null';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_big_selects';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_big_tables';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_buffer_result';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_log_bin';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_log_off';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_low_priority_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_max_join_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_mode';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_notes';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_quote_show_create';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_safe_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_select_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_slave_skip_counter';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sql_warnings';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'ssl_ca';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_capath';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_cert';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_cipher';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_crl';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_crlpath';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_key';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'storage_engine';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FEDERATED,MRG_MYISAM,MyISAM,BLACKHOLE,CSV,MEMORY,ARCHIVE,InnoDB';
    ),
    (
      Name: 'stored_program_cache';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_binlog';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_frm';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_master_info';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_relay_log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_relay_log_info';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'system_time_zone';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'table_definition_cache';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'table_open_cache';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_concurrency';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_handling';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_stack';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'time_format';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'time_zone';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'timed_mutexes';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'timestamp';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'tmp_table_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'tmpdir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'transaction_alloc_block_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'transaction_prealloc_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'tx_isolation';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'READ-UNCOMMITTED,READ-COMMITTED,REPEATABLE-READ,SERIALIZABLE';
    ),
    (
      Name: 'tx_read_only';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'unique_checks';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'updatable_views_with_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'version';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'version_comment';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'version_compile_machine';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'version_compile_os';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'wait_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'warning_count';
      IsDynamic: False;
      VarScope: vsSession;
    )

  );



  function GetFunctionCategories: TStringList;

implementation

uses apphelpers;

function GetFunctionCategories: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i:=0 to Length(MySqlFunctions)-1 do
  begin
    if Result.IndexOf( MySqlFunctions[i].Category ) = -1 then
    begin
      Result.Add( MySqlFunctions[i].Category );
    end;
  end;
  Result.Sort;
end;



{ EDbError }

constructor EDbError.Create(const Msg: string; const ErrorCode: Cardinal=0);
begin
  Self.ErrorCode := ErrorCode;
  inherited Create(Msg);
end;



{ TDbLib }

constructor TDbLib.Create(DllFile: String);
var
  msg: String;
begin
  // Load DLL as is (with or without path)
  inherited Create;
  FDllFile := DllFile;
  if not FileExists(FDllFile) then begin
    msg := f_('File does not exist: %s', [FDllFile]) +
      sLineBreak + sLineBreak +
      f_('Please launch %s from the directory where you have installed it. Or just reinstall %s.', [APPNAME, APPNAME]
      );
    Raise EdbError.Create(msg);
  end;

  FHandle := LoadLibrary(PWideChar(FDllFile));
  if FHandle = 0 then begin
    msg := f_('Library %s could not be loaded. Please select a different one.',
      [ExtractFileName(FDllFile)]
      );
    if Windows.GetLastError <> 0 then
      msg := msg + sLineBreak + sLineBreak + f_('Internal error %d: %s', [Windows.GetLastError, SysErrorMessage(Windows.GetLastError)]);
    Raise EDbError.Create(msg);
  end;

  // Dll was loaded, now initialize required procedures
  AssignProcedures;
end;


destructor TDbLib.Destroy;
begin
  if FHandle <> 0 then begin
    FreeLibrary(FHandle);
    FHandle := 0;
  end;
  inherited;
end;


procedure TDbLib.AssignProc(var Proc: FARPROC; Name: PAnsiChar; Mandantory: Boolean=True);
var
  msg: String;
begin
  // Map library procedure to internal procedure
  Proc := GetProcAddress(FHandle, Name);
  if Proc = nil then begin
    if Mandantory then begin
      msg := f_('Library error in %s: Could not find procedure address for "%s"',
        [ExtractFileName(FDllFile), Name]
        );
      if Windows.GetLastError <> 0 then
        msg := msg + sLineBreak + sLineBreak + f_('Internal error %d: %s', [Windows.GetLastError, SysErrorMessage(Windows.GetLastError)]);
      Raise EDbError.Create(msg, LIB_PROC_ERROR);
    end;
  end;
end;


procedure TMySQLLib.AssignProcedures;
begin
  AssignProc(@mysql_affected_rows, 'mysql_affected_rows');
  AssignProc(@mysql_character_set_name, 'mysql_character_set_name');
  AssignProc(@mysql_close, 'mysql_close');
  AssignProc(@mysql_data_seek, 'mysql_data_seek');
  AssignProc(@mysql_errno, 'mysql_errno');
  AssignProc(@mysql_error, 'mysql_error');
  AssignProc(@mysql_fetch_field_direct, 'mysql_fetch_field_direct');
  AssignProc(@mysql_fetch_field, 'mysql_fetch_field');
  AssignProc(@mysql_fetch_lengths, 'mysql_fetch_lengths');
  AssignProc(@mysql_fetch_row, 'mysql_fetch_row');
  AssignProc(@mysql_free_result, 'mysql_free_result');
  AssignProc(@mysql_get_client_info, 'mysql_get_client_info');
  AssignProc(@mysql_get_server_info, 'mysql_get_server_info');
  AssignProc(@mysql_init, 'mysql_init');
  AssignProc(@mysql_num_fields, 'mysql_num_fields');
  AssignProc(@mysql_num_rows, 'mysql_num_rows');
  AssignProc(@mysql_ping, 'mysql_ping');
  AssignProc(@mysql_options, 'mysql_options');
  AssignProc(@mysql_optionsv, 'mysql_optionsv', False);
  AssignProc(@mysql_real_connect, 'mysql_real_connect');
  AssignProc(@mysql_real_query, 'mysql_real_query');
  AssignProc(@mysql_ssl_set, 'mysql_ssl_set');
  AssignProc(@mysql_stat, 'mysql_stat');
  AssignProc(@mysql_store_result, 'mysql_store_result');
  AssignProc(@mysql_thread_id, 'mysql_thread_id');
  AssignProc(@mysql_next_result, 'mysql_next_result');
  AssignProc(@mysql_set_character_set, 'mysql_set_character_set');
  AssignProc(@mysql_thread_init, 'mysql_thread_init');
  AssignProc(@mysql_thread_end, 'mysql_thread_end');
  AssignProc(@mysql_warning_count, 'mysql_warning_count');
end;


procedure TPostgreSQLLib.AssignProcedures;
begin
  AssignProc(@PQconnectdb, 'PQconnectdb');
  AssignProc(@PQerrorMessage, 'PQerrorMessage');
  AssignProc(@PQresultErrorMessage, 'PQresultErrorMessage');
  AssignProc(@PQresultErrorField, 'PQresultErrorField');
  AssignProc(@PQfinish, 'PQfinish');
  AssignProc(@PQstatus, 'PQstatus');
  AssignProc(@PQsendQuery, 'PQsendQuery');
  AssignProc(@PQgetResult, 'PQgetResult');
  AssignProc(@PQbackendPID, 'PQbackendPID');
  AssignProc(@PQcmdTuples, 'PQcmdTuples');
  AssignProc(@PQntuples, 'PQntuples');
  AssignProc(@PQclear, 'PQclear');
  AssignProc(@PQnfields, 'PQnfields');
  AssignProc(@PQfname, 'PQfname');
  AssignProc(@PQftype, 'PQftype');
  AssignProc(@PQftable, 'PQftable');
  AssignProc(@PQgetvalue, 'PQgetvalue');
  AssignProc(@PQgetlength, 'PQgetlength');
  AssignProc(@PQgetisnull, 'PQgetisnull');
  AssignProc(@PQlibVersion, 'PQlibVersion');
end;


procedure TSQLiteLib.AssignProcedures;
begin
  AssignProc(@sqlite3_open, 'sqlite3_open');
  AssignProc(@sqlite3_libversion, 'sqlite3_libversion');
  AssignProc(@sqlite3_close, 'sqlite3_close');
  AssignProc(@sqlite3_db_config, 'sqlite3_db_config');
  AssignProc(@sqlite3_enable_load_extension, 'sqlite3_enable_load_extension');
  AssignProc(@sqlite3_errmsg, 'sqlite3_errmsg');
  AssignProc(@sqlite3_errcode, 'sqlite3_errcode');
  AssignProc(@sqlite3_prepare_v2, 'sqlite3_prepare_v2');
  AssignProc(@sqlite3_prepare_v3, 'sqlite3_prepare_v3');
  AssignProc(@sqlite3_exec, 'sqlite3_exec');
  AssignProc(@sqlite3_finalize, 'sqlite3_finalize');
  AssignProc(@sqlite3_step, 'sqlite3_step');
  AssignProc(@sqlite3_reset, 'sqlite3_reset');
  AssignProc(@sqlite3_total_changes, 'sqlite3_total_changes');
  AssignProc(@sqlite3_column_text, 'sqlite3_column_text');
  AssignProc(@sqlite3_column_count, 'sqlite3_column_count');
  AssignProc(@sqlite3_column_name, 'sqlite3_column_name');
  AssignProc(@sqlite3_column_decltype, 'sqlite3_column_decltype');
  AssignProc(@sqlite3_column_database_name, 'sqlite3_column_database_name');
  AssignProc(@sqlite3_column_table_name, 'sqlite3_column_table_name');
  AssignProc(@sqlite3_column_origin_name, 'sqlite3_column_origin_name');
  AssignProc(@sqlite3_column_type, 'sqlite3_column_type');
  AssignProc(@sqlite3_next_stmt, 'sqlite3_next_stmt');
  AssignProc(@sqlite3_table_column_metadata, 'sqlite3_table_column_metadata');
  AssignProc(@sqlite3_collation_needed, 'sqlite3_collation_needed');
  AssignProc(@sqlite3_create_collation, 'sqlite3_create_collation');
end;



initialization

// Keywords copied from SynHighligherSQL
MySQLKeywords := TStringList.Create;
MySQLKeywords.CommaText := 'ACCESSIBLE,ACTION,ADD,AFTER,AGAINST,AGGREGATE,ALGORITHM,ALL,ALTER,ANALYZE,AND,ANY,AS,' +
  'ASC,ASENSITIVE,AT,AUTO_INCREMENT,AVG_ROW_LENGTH,BACKUP,BEFORE,BEGIN,BENCHMARK,BETWEEN,BINLOG,BIT,' +
  'BOOL,BOTH,BY,CACHE,CALL,CASCADE,CASCADED,CASE,CHANGE,CHARACTER,CHARSET,CHECK,' +
  'CHECKSUM,CLIENT,COLLATE,COLLATION,COLUMN,COLUMNS,COMMENT,COMMIT,' +
  'COMMITTED,COMPLETION,CONCURRENT,CONNECTION,CONSISTENT,CONSTRAINT,' +
  'CONVERT,CONTAINS,CONTENTS,CREATE,CROSS,DATA,DATABASE,DATABASES,DAY_HOUR,' +
  'DAY_MICROSECOND,DAY_MINUTE,DAY_SECOND,DEALLOCATE,DEC,DEFAULT,DEFINER,DELAYED,DELAY_KEY_WRITE,DELETE,DESC,' +
  'DETERMINISTIC,DIRECTORY,DISABLE,DISCARD,DESCRIBE,DISTINCT,DISTINCTROW,' +
  'DIV,DROP,DUAL,DUMPFILE,DUPLICATE,EACH,ELSE,ELSEIF,ENABLE,ENCLOSED,END,ENDS,' +
  'ENGINE,ENGINES,ESCAPE,ESCAPED,ERRORS,EVENT,EVENTS,EVERY,EXECUTE,EXISTS,' +
  'EXPANSION,EXPLAIN,FALSE,FIELDS,FILE,FIRST,FLOAT4,FLOAT8,FLUSH,FOR,FORCE,FOREIGN,FROM,' +
  'FULL,FULLTEXT,FUNCTION,FUNCTIONS,GLOBAL,GRANT,GRANTS,GROUP,HAVING,HELP,' +
  'HIGH_PRIORITY,HOSTS,HOUR_MICROSECOND,HOUR_MINUTE,HOUR_SECOND,IDENTIFIED,IGNORE,IGNORE_SERVER_IDS,INDEX,INFILE,INNER,INOUT,INSENSITIVE,INSERT,' +
  'INSERT_METHOD,INSTALL,INT1,INT2,INT3,INT4,INT8,INTEGER,INTO,IO_THREAD,IS,' +
  'ISOLATION,INVOKER,JOIN,KEY,KEYS,KILL,LAST,LEADING,LEAVES,LEVEL,LESS,' +
  'LIKE,LIMIT,LINEAR,LINES,LIST,LOAD,LOCAL,LOCK,LOGS,LONG,LOW_PRIORITY,' +
  'MASTER,MASTER_HOST,MASTER_HEARTBEAT_PERIOD,MASTER_LOG_FILE,MASTER_LOG_POS,MASTER_CONNECT_RETRY,' +
  'MASTER_PASSWORD,MASTER_PORT,MASTER_SSL,MASTER_SSL_CA,MASTER_SSL_CAPATH,' +
  'MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_KEY,MASTER_SSL_VERIFY_SERVER_CERT,MASTER_USER,MATCH,' +
  'MAX_ROWS,MAXVALUE,MIDDLEINT,MIN_ROWS,MINUTE_MICROSECOND,MINUTE_SECOND,MOD,MODE,MODIFY,MODIFIES,NAMES,' +
  'NATURAL,NEW,NO,NODEGROUP,NOT,NO_WRITE_TO_BINLOG,NULL,NUMERIC,OJ,OFFSET,OLD,ON,OPTIMIZE,OPTION,' +
  'OPTIONALLY,OPEN,OR,ORDER,OUT,OUTER,OUTFILE,PACK_KEYS,PARTIAL,PARTITION,' +
  'PARTITIONS,PERSISTENT,PLUGIN,PLUGINS,PRECISION,PREPARE,PRESERVE,PRIMARY,PRIVILEGES,PROCEDURE,' +
  'PROCESS,PROCESSLIST,PURGE,QUERY,RAID_CHUNKS,RAID_CHUNKSIZE,RAID_TYPE,RANGE,' +
  'READ,READS,READ_WRITE,REAL,REBUILD,REFERENCES,REGEXP,RELAY_LOG_FILE,RELAY_LOG_POS,RELEASE,RELOAD,' +
  'RENAME,REORGANIZE,REPAIR,REPEATABLE,REPLACE,REPLICATION,REQUIRE,RESIGNAL,RESTRICT,RESET,' +
  'RESTORE,RETURN,RETURNS,REVOKE,RLIKE,ROLLBACK,ROLLUP,ROUTINE,ROW,' +
  'ROW_FORMAT,ROWS,SAVEPOINT,SCHEDULE,SCHEMA,SCHEMAS,SECOND_MICROSECOND,SECURITY,SELECT,' +
  'SENSITIVE,SEPARATOR,SERIALIZABLE,SESSION,SET,SHARE,SHOW,SHUTDOWN,SIGNAL,SIMPLE,SLAVE,SNAPSHOT,SOME,' +
  'SONAME,SPECIFIC,SQL,SQLEXCEPTION,SQLSTATE,SQLWARNING,SQL_BIG_RESULT,SQL_BUFFER_RESULT,SQL_CACHE,' +
  'SQL_CALC_FOUND_ROWS,SQL_NO_CACHE,SQL_SMALL_RESULT,SPATIAL,SQL_THREAD,SSL,START,' +
  'STARTING,STARTS,STATUS,STOP,STORAGE,STRAIGHT_JOIN,SUBPARTITION,' +
  'SUBPARTITIONS,SUPER,TABLE,TABLES,TABLESPACE,TEMPORARY,TERMINATED,THAN,' +
  'THEN,TO,TRAILING,TRANSACTION,TRIGGER,TRIGGERS,TRUE,TYPE,UNCOMMITTED,UNDO,' +
  'UNINSTALL,UNIQUE,UNLOCK,UNSIGNED,UPDATE,UPGRADE,UNION,USAGE,USE,USING,VALUES,VARCHARACTER,' +
  'VARIABLES,VARYING,VIEW,VIRTUAL,WARNINGS,WHEN,WHERE,WITH,WORK,WRITE,XOR,YEAR_MONTH,ZEROFILL,'
  // SQL Plus commands:
  + 'CLOSE,CONDITION,CONTINUE,CURSOR,DECLARE,DO,EXIT,FETCH,FOUND,GOTO,' +
  'HANDLER,ITERATE,LANGUAGE,LEAVE,LOOP,UNTIL,WHILE';

// Error codes copied from perror.exe
MySQLErrorCodes := Explode(',', '0=No error,'+
  '1=Operation not permitted,'+
  '2=No such file or directory,'+
  '3=No such process,'+
  '4=Interrupted function call,'+
  '5=Input/output error,'+
  '6=No such device or address,'+
  '7=Arg list too long,'+
  '8=Exec format error,'+
  '9=Bad file descriptor,'+
  '10=No child processes,'+
  '11=Resource temporarily unavailable,'+
  '12=Not enough space,'+
  '13=Permission denied,'+
  '14=Bad address,'+
  '16=Resource device,'+
  '17=File exists,'+
  '18=Improper link,'+
  '19=No such device,'+
  '20=Not a directory,'+
  '21=Is a directory,'+
  '22=Invalid argument,'+
  '23=Too many open files in system,'+
  '24=Too many open files,'+
  '25=Inappropriate I/O control operation,'+
  '27=File too large,'+
  '28=No space left on device,'+
  '29=Invalid seek,'+
  '30=Read-only file system,'+
  '31=Too many links,'+
  '32=Broken pipe,'+
  '33=Domain error,'+
  '34=Result too large,'+
  '36=Resource deadlock avoided,'+
  '38=Filename too long,'+
  '39=No locks available,'+
  '40=Function not implemented,'+
  '41=Directory not empty,'+
  '42=Illegal byte sequence,'+
  '120=Didn''t find key on read or update,'+
  '121=Duplicate key on write or update,'+
  '123=Someone has changed the row since it was read (while the table was locked to prevent it),'+
  '124=Wrong index given to function,'+
  '126=Index file is crashed,'+
  '127=Record-file is crashed,'+
  '128=Out of memory,'+
  '130=Incorrect file format,'+
  '131=Command not supported by database,'+
  '132=Old database file,'+
  '133=No record read before update,'+
  '134=Record was already deleted (or record file crashed),'+
  '135=No more room in record file,'+
  '136=No more room in index file,'+
  '137=No more records (read after end of file),'+
  '138=Unsupported extension used for table,'+
  '139=Too big row,'+
  '140=Wrong create options,'+
  '141=Duplicate unique key or constraint on write or update,'+
  '142=Unknown character set used,'+
  '143=Conflicting table definitions in sub-tables of MERGE table,'+
  '144=Table is crashed and last repair failed,'+
  '145=Table was marked as crashed and should be repaired,'+
  '146=Lock timed out; Retry transaction,'+
  '147=Lock table is full;  Restart program with a larger locktable,'+
  '148=Updates are not allowed under a read only transactions,'+
  '149=Lock deadlock; Retry transaction,'+
  '150=Foreign key constraint is incorrectly formed,'+
  '151=Cannot add a child row,'+
  '152=Cannot delete a parent row');



end.
