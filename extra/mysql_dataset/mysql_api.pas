{**
  Interface to libmysql.dll

  Copyright-parts:
    MySQL (mysql.h, mysql_com.h)
    Zeos Database Objects (ZPlainMySQL5.pas)
    Mathias Fichtner (mysql.pas)
}

unit mysql_api;

interface

uses
  Windows;  // Needed for some type definitions


const

  { General Declarations }
  MYSQL_ERRMSG_SIZE    = 512;
  SQLSTATE_LENGTH      = 5;
  SCRAMBLE_LENGTH      = 20;

  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';
  NAME_LEN             = 64;
  PROTOCOL_VERSION     = 10;
  FRM_VER              = 6;

  { Field's flags }
  NOT_NULL_FLAG          = 1;     { Field can't be NULL }
  PRI_KEY_FLAG           = 2;     { Field is part of a primary key }
  UNIQUE_KEY_FLAG        = 4;     { Field is part of a unique key }
  MULTIPLE_KEY_FLAG      = 8;     { Field is part of a key }
  BLOB_FLAG              = 16;    { Field is a blob }
  UNSIGNED_FLAG          = 32;    { Field is unsigned }
  ZEROFILL_FLAG          = 64;    { Field is zerofill }
  BINARY_FLAG            = 128;   { Field is binary }
  ENUM_FLAG              = 256;   { Field is an enum }
  AUTO_INCREMENT_FLAG    = 512;   { Field is a autoincrement field }
  TIMESTAMP_FLAG         = 1024;  { Field is a timestamp }
  SET_FLAG               = 2048;  { Field is a set }
  NUM_FLAG               = 32768; { Field is num (for clients) }
  PART_KEY_FLAG	         = 16384; { Intern; Part of some key }
  GROUP_FLAG	           = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }
  BINCMP_FLAG            = 131072; { Intern: Used by sql_yacc }

  { Server Administration Refresh Options }
  REFRESH_GRANT	           = 1;     { Refresh grant tables }
  REFRESH_LOG		           = 2;     { Start on new log file }
  REFRESH_TABLES	         = 4;     { close all tables }
  REFRESH_HOSTS	           = 8;     { Flush host cache }
  REFRESH_STATUS           = 16;    { Flush status variables }
  REFRESH_THREADS          = 32;    { Flush status variables }
  REFRESH_SLAVE            = 64;    { Reset master info abd restat slave thread }
  REFRESH_MASTER           = 128;   { Remove all bin logs in the index and truncate the index }
  REFRESH_READ_LOCK        = 16384; { Lock tables for read }
  REFRESH_FAST		         = 32768; { Intern flag }
  REFRESH_QUERY_CACHE	     = 65536;
  REFRESH_QUERY_CACHE_FREE = 131072; { Pack query cache }
  REFRESH_DES_KEY_FILE	   = 262144;
  REFRESH_USER_RESOURCES   = 524288;

  { Client Connection Options }
  CLIENT_LONG_PASSWORD	    = 1;	 { new more secure passwords }
  CLIENT_FOUND_ROWS	        = 2;	 { Found instead of affected rows }
  CLIENT_LONG_FLAG	        = 4;	 { Get all column flags }
  CLIENT_CONNECT_WITH_DB    = 8;	 { One can specify db on connect }
  CLIENT_NO_SCHEMA	        = 16;	 { Don't allow database.table.column }
  CLIENT_COMPRESS	          = 32;	 { Can use compression protcol }
  CLIENT_ODBC		            = 64;    { Odbc client }
  CLIENT_LOCAL_FILES	      = 128;   { Can use LOAD DATA LOCAL }
  CLIENT_IGNORE_SPACE	      = 256;   { Ignore spaces before '(' }
  CLIENT_PROTOCOL_41	      = 512;   { New 4.1 protocol }
  CLIENT_INTERACTIVE	      = 1024;  { This is an interactive client }
  CLIENT_SSL                = 2048;  { Switch to SSL after handshake }
  CLIENT_IGNORE_SIGPIPE     = 4096;    { IGNORE sigpipes }
  CLIENT_TRANSACTIONS	      = 8192;    { Client knows about transactions }
  CLIENT_RESERVED           = 16384;    { Old flag for 4.1 protocol  }
  CLIENT_SECURE_CONNECTION  = 32768;    { New 4.1 authentication }
  CLIENT_MULTI_STATEMENTS   = 65536;   { Enable/disable multi-stmt support }
  CLIENT_MULTI_RESULTS      = 131072;   { Enable/disable multi-results }
  CLIENT_SSL_VERIFY_SERVER_CERT = 67108864;
  CLIENT_REMEMBER_OPTIONS   = 134217728;

  SERVER_STATUS_IN_TRANS          = 1;   {Transaction has started}
  SERVER_STATUS_AUTOCOMMIT        = 2;   {Server in Autocommit Mode}
  SERVER_STATUS_MORE_RESULTS      = 4;   {More results on server}
  SERVER_MORE_RESULTS_EXISTS      = 8;   {Multiple query, next query exists}
  SERVER_QUERY_NO_GOOD_INDEX_USED = 16;
  SERVER_QUERY_NO_INDEX_USED      = 32;
  SERVER_STATUS_DB_DROPPED        = 256; {A database was dropped}

  NET_READ_TIMEOUT          = 30;        {timeout on read}
  NET_WRITE_TIMEOUT         = 60;        {timeout on write}
  NET_WAIT_TIMEOUT          = 28800;     {wait for new query}

  { Net type }
  NET_TYPE_TCPIP     = 0;
  NET_TYPE_SOCKET    = 1;
  NET_TYPE_NAMEDPIPE = 2;

  {THD: Killable}
  MYSQL_SHUTDOWN_KILLABLE_CONNECT    = 1;
  MYSQL_SHUTDOWN_KILLABLE_TRANS      = 2;
  MYSQL_SHUTDOWN_KILLABLE_LOCK_TABLE = 4;
  MYSQL_SHUTDOWN_KILLABLE_UPDATE     = 8;

  { Enum Field Types }
  FIELD_TYPE_DECIMAL     = 0;
  FIELD_TYPE_TINY        = 1;
  FIELD_TYPE_SHORT       = 2;
  FIELD_TYPE_LONG        = 3;
  FIELD_TYPE_FLOAT       = 4;
  FIELD_TYPE_DOUBLE      = 5;
  FIELD_TYPE_NULL        = 6;
  FIELD_TYPE_TIMESTAMP   = 7;
  FIELD_TYPE_LONGLONG    = 8;
  FIELD_TYPE_INT24       = 9;
  FIELD_TYPE_DATE        = 10;
  FIELD_TYPE_TIME        = 11;
  FIELD_TYPE_DATETIME    = 12;
  FIELD_TYPE_YEAR        = 13;
  FIELD_TYPE_NEWDATE     = 14;
  FIELD_TYPE_VARCHAR     = 15;
  FIELD_TYPE_BIT         = 16;
  FIELD_TYPE_NEWDECIMAL  = 246;
  FIELD_TYPE_ENUM        = 247;
  FIELD_TYPE_SET         = 248;
  FIELD_TYPE_TINY_BLOB   = 249;
  FIELD_TYPE_MEDIUM_BLOB = 250;
  FIELD_TYPE_LONG_BLOB   = 251;
  FIELD_TYPE_BLOB        = 252;
  FIELD_TYPE_VAR_STRING  = 253;
  FIELD_TYPE_STRING      = 254;
  FIELD_TYPE_GEOMETRY    = 255;

  { For Compatibility }
  FIELD_TYPE_CHAR      = FIELD_TYPE_TINY;
  FIELD_TYPE_INTERVAL  = FIELD_TYPE_ENUM;

  MAX_MYSQL_MANAGER_ERR = 256;
  MAX_MYSQL_MANAGER_MSG = 256;

  MANAGER_OK           = 200;
  MANAGER_INFO         = 250;
  MANAGER_ACCESS       = 401;
  MANAGER_CLIENT_ERR   = 450;
  MANAGER_INTERNAL_ERR = 500;


type

  TMySQLStmtAttrType = (
    STMT_ATTR_UPDATE_MAX_LENGTH,
    STMT_ATTR_CURSOR_TYPE,
    STMT_ATTR_PREFETCH_ROWS
  );

  TMysqlShutdownLevel = (
    SHUTDOWN_DEFAULT = 0,
    SHUTDOWN_WAIT_CONNECTIONS = MYSQL_SHUTDOWN_KILLABLE_CONNECT,
    SHUTDOWN_WAIT_TRANSACTIONS = MYSQL_SHUTDOWN_KILLABLE_TRANS,
    SHUTDOWN_WAIT_UPDATES = MYSQL_SHUTDOWN_KILLABLE_UPDATE,
    SHUTDOWN_WAIT_ALL_BUFFERS = (MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1),
    SHUTDOWN_WAIT_CRITICAL_BUFFERS,
    KILL_QUERY = 254,
    KILL_CONNECTION = 255
  );

  TMySQLStatus = (
    MYSQL_STATUS_READY,
    MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

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
    MYSQL_SECURE_AUTH
  );

  TMySQLRplType = (
    MYSQL_RPL_MASTER,
    MYSQL_RPL_SLAVE,
    MYSQL_RPL_ADMIN
  );

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

  PST_MYSQL_OPTIONS = ^ST_MYSQL_OPTIONS;
  ST_MYSQL_OPTIONS = record
    connect_timeout:          Cardinal;
    read_timeout:             Cardinal;
    write_timeout:            Cardinal;
    port:                     Cardinal;
    protocol:                 Cardinal;
    client_flag:              LongInt;
    host:                     PChar;
    user:                     PChar;
    password:                 PChar;
    unix_socket:              PChar;
    db:                       PChar;
    init_commands:            Pointer;
    my_cnf_file:              PChar;
    my_cnf_group:             PChar;
    charset_dir:              PChar;
    charset_name:             PChar;
    ssl_key:                  PChar;
    ssl_cert:                 PChar;
    ssl_ca:                   PChar;
    ssl_capath:               PChar;
    ssl_cipher:               PChar;
    shared_memory_base_name:  PChar;
    max_allowed_packet:       LongInt;
    use_ssl:                  Byte;
    compress:                 Byte;
    named_pipe:               Byte;
    rpl_probe:                Byte;
    rpl_parse:                Byte;
    no_master_reads:          Byte;
    separate_thread:          Byte;
    methods_to_use:           Byte;
    client_ip:                PChar;
    secure_auth:              Byte;
    local_infile_init:        Pointer;
    local_infile_read:        Pointer;
    local_infile_end:         Pointer;
    local_infile_error:       Pointer;
    local_infile_userdata:    Pointer;
  end;

  PLIST = ^LIST;
  LIST = record
    prev:       PLIST;
    next:       PLIST;
    data:       Pointer;
  end;

  MYSQL_FIELD_OFFSET = Cardinal;

  NET = record
    vio:              Pointer;
    buff:             PChar;
    buff_end:         PChar;
    write_pos:        PChar;
    read_pos:         PChar;
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
    name:             PChar;   // Name of column
    org_name:         PChar;   // Original column name, if an alias
    table:            PChar;   // Table of column if column was a field
    org_table:        PChar;   // Org table name if table was an alias
    db:               PChar;   // Database for table
    catalog:	      PChar;   // Catalog for table
    def:              PChar;   // Default value (set by mysql_list_fields)
    length:           LongInt; // Width of column
    max_length:       LongInt; // Max width of selected set
    name_length:      Cardinal;
    org_name_length:  Cardinal;
    table_length:     Cardinal;
    org_table_length: Cardinal;
    db_length:        Cardinal;
    catalog_length:   Cardinal;
    def_length:       Cardinal;
    flags:            Cardinal; // Div flags
    decimals:         Cardinal; // Number of decimals in field
    charsetnr:        Cardinal; // Character set
    _type:            Cardinal; // Type of field. Se mysql_com.h for types
  end;

  MYSQL_ROW = array[0..$ffff] of PChar;
  PMYSQL_ROW = ^MYSQL_ROW;

  PMYSQL_ROWS = ^MYSQL_ROWS;
  MYSQL_ROWS = record
    next:       PMYSQL_ROWS;
    data:       PMYSQL_ROW;
  end;

  MYSQL_ROW_OFFSET = PMYSQL_ROWS;

  MYSQL_DATA = record
    Rows:       Int64;
    Fields:     Cardinal;
    Data:       PMYSQL_ROWS;
    Alloc:      MEM_ROOT;
  end;
  PMYSQL_DATA = ^MYSQL_DATA;

  PMY_CHARSET_INFO = ^MY_CHARSET_INFO;
  MY_CHARSET_INFO = record
    number:         Cardinal;
    state:          Cardinal;
    csname:         PChar;
    name:           PChar;
    comment:        PChar;
    dir:            PChar;
    mbminlen:       Cardinal;
    mbmaxlen:       Cardinal;
  end;

  PMYSQL = ^MYSQL;
  MYSQL = record
    _net:            NET;
    connector_fd:    Pointer;
    host:            PChar;
    user:            PChar;
    passwd:          PChar;
    unix_socket:     PChar;
    server_version:  PChar;
    host_info:       PChar;
    info:            PChar;
    db:              PChar;
    charset:         PChar;
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
    options:         TMySQLOption;
    status:          Byte;
    free_me:         Byte;
    reconnect:       Byte;
    scramble:   array[1..SCRAMBLE_LENGTH+1] of Char;
    rpl_pivot:       Byte;
    master:          PMYSQL;
    next_slave:      PMYSQL;
    last_used_slave: PMYSQL;
    last_used_con:   PMYSQL;
    stmts:           Pointer; //PLIST;            {list of all statements }
    methods:         Pointer;  //PMYSQL_METHODS;
    thd:             Pointer;
    unbuffered_fetch_owner: PByte;
  end;

  MYSQL_RES = record
    row_count:       Int64;
    fields:          PMYSQL_FIELD;
    data:            PMYSQL_DATA;
    data_cursor:     PMYSQL_ROWS;
    lengths:         PLongInt;
    handle:          PMYSQL;
    field_alloc:     MEM_ROOT;
    field_count:     Integer;
    current_field:   Integer;
    row:             PMYSQL_ROW;
    current_row:     PMYSQL_ROW;
    eof:             Byte;
    unbuffered_fetch_cancelled: Byte;
    methods:         PChar;
  end;
  PMYSQL_RES = ^MYSQL_RES;

  PREP_STMT_STATE=(
    MY_ST_UNKNOWN,
    MY_ST_PREPARE,
    MY_ST_EXECUTE);

  PMYSQL_BIND = ^MYSQL_BIND;
  MYSQL_BIND = record
    length:           PLongInt;
    is_null:          PByte;
    buffer:           PChar;
    buffer_type:      Cardinal;
    buffer_length:    LongInt;
    inter_buffer:     PByte;
    offset:           LongInt;
    internal_length:  LongInt;
    param_number:     Cardinal;
    long_data_used:   Byte;
    binary_data:      Byte;
    null_field:       Byte;
    internal_is_null: Byte;
    store_param_func: procedure(_net: NET; param: PMYSQL_BIND);
    fetch_result:     procedure(param: PMYSQL_BIND; row: PMYSQL_ROW);
  end;

  PMYSQL_STMT = ^MYSQL_STMT;
  MYSQL_STMT = record
    handle:               PMYSQL;
    params:               PMYSQL_BIND;
    result:               PMYSQL_RES;
    bind:                 PMYSQL_BIND;
    fields:               PMYSQL_FIELD;
    list:                 LIST;
    current_row:          PByte;
    last_fetched_buffer:  PByte;
    query:                PChar;
    mem_root:             MEM_ROOT;
    last_fetched_column:  Int64;
    stmt_id:              LongInt;
    last_errno:           Cardinal;
    param_count:          Cardinal;
    field_count:          Cardinal;
    state:                PREP_STMT_STATE;
    last_error:           array[1..MYSQL_ERRMSG_SIZE] of Char;
    sqlstate:             array[1..SQLSTATE_LENGTH + 1] of Char;
    long_alloced:         Byte;
    send_types_to_server: Byte;
    param_buffers:        Byte;
    res_buffers:          Byte;
    result_buffered:      Byte;
  end;

  PMYSQL_MANAGER = ^MYSQL_MANAGER;
  MYSQL_MANAGER = record
    _net:               NET;
    host:               PChar;
    user:               PChar;
    passwd:             PChar;
    port:               Cardinal;
    free_me:            Byte;
    eof:                Byte;
    cmd_status:         Integer;
    last_errno:         Integer;
    net_buf:            PChar;
    net_buf_pos:        PChar;
    net_data_end:       PChar;
    net_buf_size:       Integer;
    last_error:         array[1..MAX_MYSQL_MANAGER_ERR] of Char;
  end;

  { Options for mysql_set_option }
  TMySqlSetOption = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF
  );


var
  mysql_affected_rows          : function(Handle: PMYSQL): Int64; stdcall;
  mysql_character_set_name     : function(Handle: PMYSQL): PChar; stdcall;
  mysql_close                  : procedure(Handle: PMYSQL); stdcall;
  mysql_connect                : function(Handle: PMYSQL; const Host, User, Passwd: PChar): PMYSQL; stdcall;
  mysql_create_db              : function(Handle: PMYSQL; const Db: PChar): Integer; stdcall;
  mysql_data_seek              : procedure(Result: PMYSQL_RES; Offset: Int64); stdcall;
  mysql_debug                  : procedure(Debug: PChar); stdcall;
  mysql_drop_db                : function(Handle: PMYSQL; const Db: PChar): Integer; stdcall;
  mysql_dump_debug_info        : function(Handle: PMYSQL): Integer; stdcall;
  mysql_eof                    : function(Result: PMYSQL_RES): Byte; stdcall;
  mysql_errno                  : function(Handle: PMYSQL): Cardinal; stdcall;
  mysql_error                  : function(Handle: PMYSQL): PChar; stdcall;
  mysql_escape_string          : function(PTo, PFrom: PChar; Len: Cardinal): Cardinal; stdcall;
  mysql_fetch_field            : function(Result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
  mysql_fetch_field_direct     : function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD; stdcall;
  mysql_fetch_fields           : function(Result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
  mysql_fetch_lengths          : function(Result: PMYSQL_RES): PLongInt; stdcall;
  mysql_fetch_row              : function(Result: PMYSQL_RES): PMYSQL_ROW; stdcall;
  mysql_field_seek             : function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; stdcall;
  mysql_field_tell             : function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET; stdcall;
  mysql_free_result            : procedure(Result: PMYSQL_RES); stdcall;
  mysql_get_client_info        : function: PChar; stdcall;
  mysql_get_host_info          : function(Handle: PMYSQL): PChar; stdcall;
  mysql_get_proto_info         : function(Handle: PMYSQL): Cardinal; stdcall;
  mysql_get_server_info        : function(Handle: PMYSQL): PChar; stdcall;
  mysql_info                   : function(Handle: PMYSQL): PChar; stdcall;
  mysql_init                   : function(Handle: PMYSQL): PMYSQL; stdcall;
  mysql_insert_id              : function(Handle: PMYSQL): Int64; stdcall;
  mysql_kill                   : function(Handle: PMYSQL; Pid: LongInt): Integer; stdcall;
  mysql_list_dbs               : function(Handle: PMYSQL; Wild: PChar): PMYSQL_RES; stdcall;
  mysql_list_fields            : function(Handle: PMYSQL; const Table, Wild: PChar): PMYSQL_RES; stdcall;
  mysql_list_processes         : function(Handle: PMYSQL): PMYSQL_RES; stdcall;
  mysql_list_tables            : function(Handle: PMYSQL; const Wild: PChar): PMYSQL_RES; stdcall;
  mysql_num_fields             : function(Result: PMYSQL_RES): Integer; stdcall;
  mysql_num_rows               : function(Result: PMYSQL_RES): Int64; stdcall;
  mysql_options                : function(Handle: PMYSQL; Option: TMySQLOption; const Arg: PChar): Integer; stdcall;
  mysql_ping                   : function(Handle: PMYSQL): Integer; stdcall;
  mysql_query                  : function(Handle: PMYSQL; const Query: PChar): Integer; stdcall;
  mysql_real_connect           : function(Handle: PMYSQL; const Host, User, Passwd, Db: PChar;
                                          Port: Cardinal; const UnixSocket: PChar; ClientFlag: Cardinal): PMYSQL; stdcall;
  mysql_real_escape_string     : function(Handle: PMYSQL; PTo: PChar; const PFrom: PChar; length: Cardinal): Cardinal; stdcall;
  mysql_real_query             : function(Handle: PMYSQL; const Query: PChar; Length: Cardinal): Integer; stdcall;
  mysql_refresh                : function(Handle: PMYSQL; Options: Cardinal): Integer; stdcall;
  mysql_row_seek               : function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS; stdcall;
  mysql_row_tell               : function(Result: PMYSQL_RES): PMYSQL_ROWS; stdcall;
  mysql_select_db              : function(Handle: PMYSQL; const Db: PChar): Integer; stdcall;
  mysql_ssl_set                : function(Handle: PMYSQL; const key, cert, CA, CApath, cipher:
                                          PChar): Byte; stdcall;
  mysql_stat                   : function(Handle: PMYSQL): PChar; stdcall;
  mysql_store_result           : function(Handle: PMYSQL): PMYSQL_RES; stdcall;
  mysql_thread_id              : function(Handle: PMYSQL): Cardinal; stdcall;
  mysql_use_result             : function(Handle: PMYSQL): PMYSQL_RES; stdcall;

  { Set up and bring down a thread; these function should be called for each thread in an application which
  opens at least one MySQL connection.  All uses of the connection(s) should be between these function calls. }
  my_init                      : procedure; stdcall;
  mysql_thread_init            : function: Byte; stdcall;
  mysql_thread_end             : procedure; stdcall;
  mysql_thread_safe            : function: Cardinal; stdcall;

  { Set up and bring down the server; to ensure that applications will work when linked against either the
    standard client library or the embedded server library, these functions should be called. }
  mysql_server_init            : function(Argc: Integer; Argv, Groups: Pointer): Integer; stdcall;
  mysql_server_end             : procedure; stdcall;

  mysql_change_user            : function(mysql: PMYSQL; const user: PChar; const passwd: PChar; const db: PChar): Byte;
  mysql_field_count            : function(Handle: PMYSQL): Cardinal; stdcall;
  mysql_get_client_version     : function: Cardinal; stdcall;
  mysql_send_query             : function(mysql: PMYSQL; const query: PChar; length: Cardinal): Integer; stdcall;
  mysql_read_query_result      : function(mysql: PMYSQL): Integer; stdcall;

  { Perform query on master }
  mysql_master_query           : function(mysql: PMYSQL; const query: PChar; length: Cardinal): Byte; stdcall;
  mysql_master_send_query      : function(mysql: PMYSQL; const query: PChar; length: Cardinal): Byte; stdcall;

  { Perform query on slave }
  mysql_slave_query            : function(mysql: PMYSQL; const query: PChar; length: Cardinal): Byte; stdcall;
  mysql_slave_send_query       : function(mysql: PMYSQL; const query: PChar; length: Cardinal): Byte; stdcall;

  { Enable/disable parsing of all queries to decide
    if they go on master or slave }
  mysql_enable_rpl_parse       : procedure(mysql: PMYSQL); stdcall;
  mysql_disable_rpl_parse      : procedure(mysql: PMYSQL); stdcall;

  { Get the value of the parse flag }
  mysql_rpl_parse_enabled      : function(mysql: PMYSQL): Integer; stdcall;

  { Enable/disable reads from master }
  mysql_enable_reads_from_master : procedure(mysql: PMYSQL); stdcall;
  mysql_disable_reads_from_master : procedure(mysql: PMYSQL); stdcall;

  { Get the value of the master read flag }
  mysql_reads_from_master_enabled : function(mysql: PMYSQL): Byte; stdcall;

  mysql_rpl_query_type         : function(const query: PChar; len: Integer): TMySqlRplType; stdcall;

  { Discover the master and its slaves }
  mysql_rpl_probe              : function(mysql: PMYSQL): Byte; stdcall;

  { Set the master, close/free the old one, if it is not a pivot }
  mysql_set_master             : function(mysql: PMYSQL; const host: PChar; port: Cardinal;
                                          const user: PChar; const passwd: PChar): Integer; stdcall;
  mysql_add_slave              : function(mysql: PMYSQL; const host: PChar; port: Cardinal;
                                          const user: PChar; const passwd: PChar): Integer; stdcall;

  mysql_manager_init           : function(con: PMYSQL_MANAGER): PMYSQL_MANAGER; stdcall;
  mysql_manager_connect        : function(con: PMYSQL_MANAGER; const host: PChar; const user: PChar;
                                          const passwd: PChar; port: Cardinal): PMYSQL_MANAGER; stdcall;
  mysql_manager_close          : procedure(con: PMYSQL_MANAGER); stdcall;
  mysql_manager_command        : function(con: PMYSQL_MANAGER; const cmd: PChar; cmd_len: Integer): Integer; stdcall;
  mysql_manager_fetch_line     : function(con: PMYSQL_MANAGER; res_buf: PChar; res_buf_size: Integer): Integer; stdcall;

  mysql_autocommit             : function(Handle: PMYSQL; const mode: Byte): Byte; stdcall;
  mysql_commit                 : function(Handle: PMYSQL): Byte; stdcall;
  mysql_get_server_version     : function(Handle: PMYSQL): Cardinal; stdcall;
  mysql_hex_string             : function(PTo, PFrom: Pchar; Len: Cardinal): Cardinal; stdcall;
  mysql_more_results           : function(Handle: PMYSQL): Byte; stdcall;
  mysql_next_result            : function(Handle: PMYSQL): Integer; stdcall;
  mysql_rollback               : function(Handle: PMYSQL): Byte; stdcall;
  mysql_set_character_set      : function(Handle: PMYSQL; csname: PChar): Integer; stdcall;
  mysql_set_server_option      : function(Handle: PMYSQL; Option: TMysqlSetOption): Integer; stdcall;
  mysql_shutdown               : function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer;
  mysql_sqlstate               : function(Handle: PMYSQL): PChar; stdcall;
  mysql_warning_count          : function(Handle: PMYSQL): Cardinal; stdcall;

  mysql_stmt_affected_rows     : function(stmt: PMYSQL_STMT): Int64; stdcall;
  mysql_stmt_attr_get          : function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;arg: PChar): Integer; stdcall;
  mysql_stmt_attr_set          : function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                          const arg: PChar): Integer; stdcall;
  mysql_stmt_bind_param        : function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND): Byte; stdcall;
  mysql_stmt_bind_result       : function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND): Byte; stdcall;
  mysql_stmt_close             : function(stmt: PMYSQL_STMT): Byte; stdcall;
  mysql_stmt_data_seek         : procedure(stmt: PMYSQL_STMT; offset: Int64); stdcall;
  mysql_stmt_errno             : function(stmt: PMYSQL_STMT): Cardinal; stdcall;
  mysql_stmt_error             : function(stmt: PMYSQL_STMT): PChar; stdcall;
  mysql_stmt_execute           : function(stmt: PMYSQL_STMT): Integer; stdcall;
  mysql_stmt_fetch             : function(stmt: PMYSQL_STMT): Integer; stdcall;
  mysql_stmt_fetch_column      : function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND; column: Cardinal;
                                          offset: Cardinal): Integer; stdcall;
  mysql_stmt_field_count       : function(stmt: PMYSQL_STMT): Cardinal; stdcall;
  mysql_stmt_free_result       : function(stmt: PMYSQL_STMT): Byte; stdcall;
  mysql_stmt_init              : function(Handle: PMYSQL): PMYSQL_STMT; stdcall;
  mysql_stmt_insert_id         : function(stmt: PMYSQL_STMT): Int64; stdcall;
  mysql_stmt_num_rows          : function(stmt: PMYSQL_STMT): Int64; stdcall;
  mysql_stmt_param_count       : function(stmt: PMYSQL_STMT): Cardinal; stdcall;
  mysql_stmt_param_metadata    : function(stmt: PMYSQL_STMT): PMYSQL_RES; stdcall;
  mysql_stmt_prepare           : function(stmt: PMYSQL_STMT; const query: PChar; length: Cardinal): Integer; stdcall;
  mysql_stmt_reset             : function(stmt: PMYSQL_STMT): Byte; stdcall;
  mysql_stmt_result_metadata   : function(stmt: PMYSQL_STMT): PMYSQL_RES; stdcall;
  mysql_stmt_row_seek          : function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS; stdcall;
  mysql_stmt_row_tell          : function(stmt: PMYSQL_STMT): PMYSQL_ROWS; stdcall;
  mysql_stmt_send_long_data    : function(stmt: PMYSQL_STMT; parameter_number: Cardinal; const
                                  data: PChar; length: Cardinal): Byte; stdcall;
  mysql_stmt_sqlstate          : function(stmt: PMYSQL_STMT): PChar; stdcall;
  mysql_stmt_store_result      : function(stmt: PMYSQL_STMT): Integer; stdcall;

  mysql_get_character_set_info : procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO); stdcall;



// Status codes for libmySQL.dll

const
  LIBMYSQL_UNDEFINED = 0;     // libmysql_load() has not yet been called
  LIBMYSQL_MISSING = 1;       // No suitable DLL could be located
  LIBMYSQL_INCOMPATIBLE = 2;  // A DLL was found but it is not compatible
  LIBMYSQL_READY = 3;         // The DLL was loaded successfully

var
  libmysql_handle: HMODULE = 0;
  libmysql_status: byte = LIBMYSQL_UNDEFINED;

function libmysql_load: Byte;
procedure libmysql_free;


implementation


{**
  Loads the library module.
}
function libmysql_load: Byte;

  procedure assign_proc(var proc: FARPROC; name: pChar);
  begin
    proc := GetProcAddress(libmysql_handle, name);
    if proc = nil then libmysql_status := LIBMYSQL_INCOMPATIBLE;
  end;

begin
  libmysql_free;
  libmysql_handle := LoadLibrary('libmysql.dll');
  if libmysql_handle = 0 then
    libmysql_status := LIBMYSQL_MISSING
  else begin
    libmysql_status := LIBMYSQL_READY;
    assign_proc(@mysql_affected_rows, 'mysql_affected_rows');
    assign_proc(@mysql_character_set_name, 'mysql_character_set_name');
    assign_proc(@mysql_close, 'mysql_close');
    assign_proc(@mysql_connect, 'mysql_connect');
    assign_proc(@mysql_create_db, 'mysql_create_db');
    assign_proc(@mysql_data_seek, 'mysql_data_seek');
    assign_proc(@mysql_debug, 'mysql_debug');
    assign_proc(@mysql_drop_db, 'mysql_drop_db');
    assign_proc(@mysql_dump_debug_info, 'mysql_dump_debug_info');
    assign_proc(@mysql_eof, 'mysql_eof');
    assign_proc(@mysql_errno, 'mysql_errno');
    assign_proc(@mysql_error, 'mysql_error');
    assign_proc(@mysql_escape_string, 'mysql_escape_string');
    assign_proc(@mysql_fetch_field, 'mysql_fetch_field');
    assign_proc(@mysql_fetch_field_direct, 'mysql_fetch_field_direct');
    assign_proc(@mysql_fetch_fields, 'mysql_fetch_fields');
    assign_proc(@mysql_fetch_lengths, 'mysql_fetch_lengths');
    assign_proc(@mysql_fetch_row, 'mysql_fetch_row');
    assign_proc(@mysql_field_seek, 'mysql_field_seek');
    assign_proc(@mysql_field_tell, 'mysql_field_tell');
    assign_proc(@mysql_free_result, 'mysql_free_result');
    assign_proc(@mysql_get_client_info, 'mysql_get_client_info');
    assign_proc(@mysql_get_host_info, 'mysql_get_host_info');
    assign_proc(@mysql_get_proto_info, 'mysql_get_proto_info');
    assign_proc(@mysql_get_server_info, 'mysql_get_server_info');
    assign_proc(@mysql_info, 'mysql_info');
    assign_proc(@mysql_init, 'mysql_init');
    assign_proc(@mysql_insert_id, 'mysql_insert_id');
    assign_proc(@mysql_kill, 'mysql_kill');
    assign_proc(@mysql_list_dbs, 'mysql_list_dbs');
    assign_proc(@mysql_list_fields, 'mysql_list_fields');
    assign_proc(@mysql_list_processes, 'mysql_list_processes');
    assign_proc(@mysql_list_tables, 'mysql_list_tables');
    assign_proc(@mysql_num_fields, 'mysql_num_fields');
    assign_proc(@mysql_num_rows, 'mysql_num_rows');
    assign_proc(@mysql_options, 'mysql_options');
    assign_proc(@mysql_ping, 'mysql_ping');
    assign_proc(@mysql_query, 'mysql_query');
    assign_proc(@mysql_real_connect, 'mysql_real_connect');
    assign_proc(@mysql_real_escape_string, 'mysql_real_escape_string');
    assign_proc(@mysql_real_query, 'mysql_real_query');
    assign_proc(@mysql_refresh, 'mysql_refresh');
    assign_proc(@mysql_row_seek, 'mysql_row_seek');
    assign_proc(@mysql_row_tell, 'mysql_row_tell');
    assign_proc(@mysql_select_db, 'mysql_select_db');
    assign_proc(@mysql_shutdown, 'mysql_shutdown');
    assign_proc(@mysql_ssl_set, 'mysql_ssl_set');
    assign_proc(@mysql_stat, 'mysql_stat');
    assign_proc(@mysql_store_result, 'mysql_store_result');
    assign_proc(@mysql_thread_id, 'mysql_thread_id');
    assign_proc(@mysql_use_result, 'mysql_use_result');
    assign_proc(@my_init, 'my_init');
    assign_proc(@mysql_thread_init, 'mysql_thread_init');
    assign_proc(@mysql_thread_end, 'mysql_thread_end');
    assign_proc(@mysql_thread_safe, 'mysql_thread_safe');
    assign_proc(@mysql_server_init, 'mysql_server_init');
    assign_proc(@mysql_server_end, 'mysql_server_end');
    assign_proc(@mysql_change_user, 'mysql_change_user');
    assign_proc(@mysql_field_count, 'mysql_field_count');
    assign_proc(@mysql_get_client_version, 'mysql_get_client_version');

    assign_proc(@mysql_send_query, 'mysql_send_query');
    assign_proc(@mysql_read_query_result, 'mysql_read_query_result');
    assign_proc(@mysql_master_query, 'mysql_master_query');
    assign_proc(@mysql_master_send_query, 'mysql_master_send_query');
    assign_proc(@mysql_slave_query, 'mysql_slave_query');
    assign_proc(@mysql_slave_send_query, 'mysql_slave_send_query');
    assign_proc(@mysql_enable_rpl_parse, 'mysql_enable_rpl_parse');
    assign_proc(@mysql_disable_rpl_parse, 'mysql_disable_rpl_parse');
    assign_proc(@mysql_rpl_parse_enabled, 'mysql_rpl_parse_enabled');
    assign_proc(@mysql_enable_reads_from_master, 'mysql_enable_reads_from_master');
    assign_proc(@mysql_disable_reads_from_master, 'mysql_disable_reads_from_master');
    assign_proc(@mysql_reads_from_master_enabled, 'mysql_reads_from_master_enabled');
    assign_proc(@mysql_rpl_query_type, 'mysql_rpl_query_type');
    assign_proc(@mysql_rpl_probe, 'mysql_rpl_probe');
    assign_proc(@mysql_set_master, 'mysql_set_master');
    assign_proc(@mysql_add_slave, 'mysql_add_slave');
    assign_proc(@mysql_manager_init, 'mysql_manager_init');
    assign_proc(@mysql_manager_connect, 'mysql_manager_connect');
    assign_proc(@mysql_manager_close, 'mysql_manager_close');
    assign_proc(@mysql_manager_command, 'mysql_manager_command');
    assign_proc(@mysql_manager_fetch_line, 'mysql_manager_fetch_line');
    assign_proc(@mysql_autocommit, 'mysql_autocommit');
    assign_proc(@mysql_commit, 'mysql_commit');
    assign_proc(@mysql_get_server_version, 'mysql_get_server_version');
    assign_proc(@mysql_hex_string, 'mysql_hex_string');
    assign_proc(@mysql_more_results, 'mysql_more_results');
    assign_proc(@mysql_next_result, 'mysql_next_result');
    assign_proc(@mysql_rollback, 'mysql_rollback');
    assign_proc(@mysql_set_character_set, 'mysql_set_character_set');
    assign_proc(@mysql_set_server_option, 'mysql_set_server_option');
    assign_proc(@mysql_sqlstate, 'mysql_sqlstate');
    assign_proc(@mysql_warning_count, 'mysql_warning_count');

    {API for PREPARED STATEMENTS}
    assign_proc(@mysql_stmt_affected_rows, 'mysql_stmt_affected_rows');
    assign_proc(@mysql_stmt_attr_get, 'mysql_stmt_attr_get');
    assign_proc(@mysql_stmt_attr_set, 'mysql_stmt_attr_set');
    assign_proc(@mysql_stmt_bind_param, 'mysql_stmt_bind_param');
    assign_proc(@mysql_stmt_bind_result, 'mysql_stmt_bind_result');
    assign_proc(@mysql_stmt_close, 'mysql_stmt_close');
    assign_proc(@mysql_stmt_data_seek, 'mysql_stmt_data_seek');
    assign_proc(@mysql_stmt_errno, 'mysql_stmt_errno');
    assign_proc(@mysql_stmt_error, 'mysql_stmt_error');
    assign_proc(@mysql_stmt_execute, 'mysql_stmt_execute');
    assign_proc(@mysql_stmt_fetch, 'mysql_stmt_fetch');
    assign_proc(@mysql_stmt_fetch_column, 'mysql_stmt_fetch_column');
    assign_proc(@mysql_stmt_field_count, 'mysql_stmt_field_count');
    assign_proc(@mysql_stmt_free_result, 'mysql_stmt_free_result');
    assign_proc(@mysql_stmt_init, 'mysql_stmt_init');
    assign_proc(@mysql_stmt_insert_id, 'mysql_stmt_insert_id');
    assign_proc(@mysql_stmt_num_rows, 'mysql_stmt_num_rows');
    assign_proc(@mysql_stmt_param_count, 'mysql_stmt_param_count');
    assign_proc(@mysql_stmt_param_metadata, 'mysql_stmt_param_metadata');
    assign_proc(@mysql_stmt_prepare, 'mysql_stmt_prepare');
    assign_proc(@mysql_stmt_reset, 'mysql_stmt_reset');
    assign_proc(@mysql_stmt_result_metadata, 'mysql_stmt_result_metadata');
    assign_proc(@mysql_stmt_row_seek, 'mysql_stmt_row_seek');
    assign_proc(@mysql_stmt_row_tell, 'mysql_stmt_row_tell');
    assign_proc(@mysql_stmt_send_long_data, 'mysql_stmt_send_long_data');
    assign_proc(@mysql_stmt_sqlstate, 'mysql_stmt_sqlstate');
    assign_proc(@mysql_stmt_store_result, 'mysql_stmt_store_result');

    assign_proc(@mysql_get_character_set_info, 'mysql_get_character_set_info');
  end; 
  Result := libmysql_status; 
end;


{**
  Unloads the library module
}
procedure libmysql_free;
begin
  if libmysql_handle <> 0 then FreeLibrary(libmysql_handle);
  libmysql_handle := 0;
  libmysql_status := LIBMYSQL_UNDEFINED;
end;


initialization


begin
  libmysql_load;
end;


finalization


begin
  libmysql_free;
end;

end.
