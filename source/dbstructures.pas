unit dbstructures;

// -------------------------------------
// Server constants, variables and data types
// -------------------------------------

interface

uses
  Classes, Graphics, Windows, SysUtils, gnugettext, Vcl.Forms;



const
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
  TDBDatatypeIndex = (dbdtTinyint, dbdtSmallint, dbdtMediumint, dbdtInt, dbdtUint, dbdtBigint, dbdtSerial, dbdtBigSerial,
    dbdtFloat, dbdtDouble, dbdtDecimal, dbdtNumeric, dbdtReal, dbdtDoublePrecision, dbdtMoney, dbdtSmallmoney,
    dbdtDate, dbdtTime, dbdtYear, dbdtDatetime, dbdtDatetime2, dbdtDatetimeOffset, dbdtSmalldatetime, dbdtTimestamp, dbdtInterval,
    dbdtChar, dbdtNchar, dbdtVarchar, dbdtNvarchar, dbdtTinytext, dbdtText, dbdtNtext, dbdtMediumtext, dbdtLongtext,
    dbdtJson, dbdtJsonB, dbdtCidr, dbdtInet, dbdtMacaddr,
    dbdtBinary, dbdtVarbinary, dbdtTinyblob, dbdtBlob, dbdtMediumblob, dbdtLongblob, dbdtImage,
    dbdtEnum, dbdtSet, dbdtBit, dbdtVarBit, dbdtBool, dbdtRegClass, dbdtRegProc, dbdtUnknown,
    dbdtCursor, dbdtSqlvariant, dbdtTable, dbdtUniqueidentifier, dbdtHierarchyid, dbdtXML,
    dbdtPoint, dbdtLinestring, dbdtLineSegment, dbdtPolygon, dbdtGeometry, dbdtBox, dbdtPath, dbdtCircle, dbdtMultipoint, dbdtMultilinestring, dbdtMultipolygon, dbdtGeometrycollection
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
      Index:           dbdtUnknown;
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
      Index:           dbdtTinyint;
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
      Index:           dbdtSmallint;
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
      Index:           dbdtMediumint;
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
      Index:           dbdtInt;
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
      Index:           dbdtBigint;
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
      Index:           dbdtFloat;
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
      Index:           dbdtDouble;
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
      Index:           dbdtDecimal;
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
      Index:           dbdtDate;
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
      Index:           dbdtTime;
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
      Index:           dbdtYear;
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
      Index:           dbdtDatetime;
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
      Index:           dbdtTimestamp;
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
      Index:           dbdtVarchar;
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
      Index:           dbdtChar;
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
      Index:           dbdtTinytext;
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
      Index:           dbdtText;
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
      Index:           dbdtMediumtext;
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
      Index:           dbdtLongtext;
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
      Index:           dbdtJson;
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
      Index:           dbdtBinary;
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
      Index:           dbdtVarbinary;
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
      Index:           dbdtTinyblob;
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
      Index:           dbdtBlob;
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
      Index:           dbdtMediumblob;
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
      Index:           dbdtLongblob;
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
      Index:           dbdtEnum;
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
      Index:           dbdtSet;
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
      Index:           dbdtBit;
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
      Index:           dbdtPoint;
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
      Index:           dbdtLinestring;
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
      Index:           dbdtPolygon;
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
      Index:           dbdtGeometry;
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
      Index:           dbdtMultipoint;
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
      Index:           dbdtMultilinestring;
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
      Index:           dbdtMultipolygon;
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
      Index:           dbdtGeometrycollection;
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
      Index:           dbdtUnknown;
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
      Index: dbdtTinyint;
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
      Index:           dbdtSmallint;
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
      Index:           dbdtInt;
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
      Index:           dbdtBigint;
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
      Index:           dbdtBit;
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
      Index:           dbdtDecimal;
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
      Index:           dbdtNumeric;
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
      Index:           dbdtMoney;
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
      Index:           dbdtSmallmoney;
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
      Index:           dbdtFloat;
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
      Index:           dbdtReal;
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
      Index:           dbdtTime;
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
      Index:           dbdtDate;
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
      Index:           dbdtDatetime;
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
      Index:           dbdtDatetime2;
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
      Index:           dbdtDatetimeOffset;
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
      Index:           dbdtSmalldatetime;
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
      Index:           dbdtTimestamp;
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
      Index:           dbdtChar;
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
      Index:           dbdtVarchar;
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
      Index:           dbdtText;
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
      Index:           dbdtNchar;
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
      Index:           dbdtNvarchar;
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
      Index:           dbdtNtext;
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
      Index:           dbdtBinary;
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
      Index:           dbdtVarbinary;
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
      Index:           dbdtImage;
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
      Index:           dbdtCursor;
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
      Index:           dbdtSqlvariant;
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
      Index:           dbdtTable;
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
      Index:           dbdtUniqueidentifier;
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
      Index:           dbdtHierarchyid;
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
      Index:           dbdtXML;
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
      Index:           dbdtUnknown;
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
      Index:           dbdtSmallint;
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
      Index:           dbdtInt;
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
      Index:           dbdtBigint;
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
      Index:           dbdtSerial;
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
      Index:           dbdtBigSerial;
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
      Index:           dbdtVarBit;
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
      Index:           dbdtBit;
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
      Index:           dbdtNumeric;
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
      Index:           dbdtReal;
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
      Index:           dbdtDoublePrecision;
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
      Index:           dbdtMoney;
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
      Index:           dbdtChar;
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
      Index:           dbdtVarchar;
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
      Index:           dbdtText;
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
      Index:           dbdtCidr;
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
      Index:           dbdtInet;
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
      Index:           dbdtMacaddr;
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
      Index:           dbdtDate;
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
      Index:           dbdtTime;
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
      Index:           dbdtDatetime;
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
      Index:           dbdtDatetime2;
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
      Index:           dbdtDate;
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
      Index:           dbdtInterval;
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
      Index:           dbdtBlob;
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
      Index:           dbdtPoint;
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
      Index:           dbdtLinestring;
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
      Index:           dbdtLineSegment;
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
      Index:           dbdtBox;
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
      Index:           dbdtPath;
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
      Index:           dbdtPolygon;
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
      Index:           dbdtCircle;
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
      Index:           dbdtBool;
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
      Index:           dbdtRegClass;
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
      Index:           dbdtRegProc;
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
      Index:           dbdtJson;
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
      Index:           dbdtJsonB;
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
      Index:           dbdtUniqueidentifier;
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

  SQLiteDatatypes: Array[0..15] of TDBDatatype =
  (
    (
      Index:           dbdtUnknown;
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
      Index:           dbdtTinyint;
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
      Index:           dbdtInt;
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
      Index:           dbdtUint;
      Name:            'UINT';
      Names:           'UINT';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBigint;
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
      Index:           dbdtChar;
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
      Index:           dbdtVarchar;
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
      Index:           dbdtText;
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
      Index:           dbdtUniqueidentifier;
      Name:            'UNIQUEIDENTIFIER';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBlob;
      Name:            'BLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtReal;
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
      Index:           dbdtDate;
      Name:            'DATE';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTime;
      Name:            'TIME';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime;
      Name:            'DATETIME';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtEnum;
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
      Index:           dbdtSet;
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





implementation

uses apphelpers;



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
