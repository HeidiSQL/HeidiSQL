// -----------------------------------------------------------------------------------------------
//
//                    MySQL Client API for Borland Delphi (version 4 and above)
//
//                           Pascal Interface Unit for libmySQL.dll, the
//                        Client Library for MySQL AB's SQL Database Server
//
//                  This is a literal translation of relevant parts of MySQL AB's
//                    C header files, mysql.h, mysql_com.h, and mysql_version.h
//
//                            Copyright (c) 1999-2002 Matthias Fichtner
//                           (see license.txt for licensing information)
//
// -----------------------------------------------------------------------------------------------
//                       See mysql.h for MySQL AB's copyright and GPL notice
// -----------------------------------------------------------------------------------------------
//
//       17-Aug-1999  mf  Translated mysql.h                             MySQL 3.22.24
//       19-Aug-1999  mf  Corrected some type definitions                MySQL 3.22.24
//       20-Aug-1999  mf  Finished debugging the unit                    MySQL 3.22.24
//       18-Sep-1999  mf  Code maintenance for release 3.22.26a          MySQL 3.22.26a
//       22-Oct-1999  mf  Code maintenance for release 3.22.28           MySQL 3.22.28
//       02-Jan-2000  mf  Code maintenance for release 3.22.29           MySQL 3.22.29
//       21-Jan-2000  mf  Code maintenance for release 3.22.30           MySQL 3.22.30
//       07-Feb-2000  mf  Code maintenance for release 3.22.31           MySQL 3.22.31
//       16-Feb-2000  mf  Code maintenance for release 3.22.32           MySQL 3.22.32
//       13-Aug-2000  mf  Code maintenance for release 3.22.34           MySQL 3.22.34
//       14-Aug-2000  mf  Reworked entire unit for first 3.23 release    MySQL 3.23.19-beta
//       14-Aug-2000  mf  Added mysql_character_set_name()               MySQL 3.23.22-beta
//       11-Sep-2000  mf  Added IS_NUM_FIELD and INTERNAL_NUM_FIELD      MySQL 3.23.24-beta
//       08-Oct-2000  mf  Modified TMEM_ROOT, enum_server_command,       MySQL 3.23.25-beta
//                        and INTERNAL_NUM_FIELD
//       01-Nov-2000  mf  Code maintenance for release 3.23.27           MySQL 3.23.27-beta
//       25-Nov-2000  mf  Code maintenance for release 3.23.28           MySQL 3.23.28-gamma
//       05-Jan-2001  mf  Code maintenance for release 3.23.30           MySQL 3.23.30-gamma
//       19-Jan-2001  mf  Code maintenance for release 3.23.31           MySQL 3.23.31
//       11-Mar-2001  mf  Added functions mysql_real_send_query(),       MySQL 3.23.33
//                        mysql_send_query(), and mysql_reap_query()
//       28-Mai-2001  mf  Modified mysql_send_query(), removed           MySQL 3.23.38
//                        mysql_real_send_query(), mysql_reap_query(),
//                        added mysql_read_query_result(), and fixed
//                        CLIENT_TRANSACTIONS
//       07-Aug-2001  mf  Code maintenance for release 3.23.40           MySQL 3.23.40
//       23-Sep-2001  mf  Code maintenance for release 3.23.42           MySQL 3.23.42
//       29-Jan-2002  mf  Added libmysql_load(), libmysql_free(),        MySQL 3.23.47
//                        libmysql_status and LIBMYSQL_ constants
//                        for dynamic loading of libmySQL.dll
//       11-Mar-2002  mf  Added MYSQL_OPT_LOCAL_INFILE to mysql_option   MySQL 3.23.49
//
// -----------------------------------------------------------------------------------------------
//
//                   Latest releases of mysql.pas are made available through the
//                   distribution site at: http://www.fichtner.net/delphi/mysql/
//
//                  Please send questions, bug reports, and suggestions regarding
//                  mysql.pas to Matthias Fichtner <mfichtner@fichtner-meyer.com>
//
//                      See readme.txt for an introduction and documentation.
//                    See license.txt for licensing information and disclaimer.
//
// -----------------------------------------------------------------------------------------------
//                     This unit is provided "as is". Use it at your own risk.
// -----------------------------------------------------------------------------------------------

unit mysql;

// -----------------------------------------------------------------------------------------------
INTERFACE
// -----------------------------------------------------------------------------------------------

uses
  Windows,  // Needed for some type definitions
  Winsock;  // Needed for some type definitions

// ----------------
// From mysql.h ...
// ----------------

type
  my_bool = byte;
  gptr = pChar;

type
  PUSED_MEM = ^TUSED_MEM;  // struct for once_alloc
  TUSED_MEM = record
    next: PUSED_MEM;       // Next block in use
    left: longword;        // memory left in block
    size: longword;        // size of block
  end;

type
  error_proc = procedure;

type
  PMEM_ROOT = ^TMEM_ROOT;
  TMEM_ROOT = record
    free: PUSED_MEM;
    used: PUSED_MEM;
    pre_alloc: PUSED_MEM;
    min_malloc: longword;
    block_size: longword;
    error_handler: error_proc;
  end;

type
  my_socket = TSocket;

// --------------------
// From mysql_com.h ...
// --------------------

const
  NAME_LEN = 64;               // Field/table name length
  HOSTNAME_LENGTH = 60;
  USERNAME_LENGTH = 16;
  SERVER_VERSION_LENGTH = 60;

  LOCAL_HOST = 'localhost';
  LOCAL_HOST_NAMEDPIPE = '.';

  MYSQL_NAMEDPIPE = 'MySQL';
  MYSQL_SERVICENAME = 'MySql';

type
  enum_server_command = (
    COM_SLEEP, COM_QUIT, COM_INIT_DB, COM_QUERY,
    COM_FIELD_LIST, COM_CREATE_DB, COM_DROP_DB, COM_REFRESH,
    COM_SHUTDOWN, COM_STATISTICS,
    COM_PROCESS_INFO, COM_CONNECT, COM_PROCESS_KILL,
    COM_DEBUG, COM_PING, COM_TIME, COM_DELAYED_INSERT,
    COM_CHANGE_USER, COM_BINLOG_DUMP,
    COM_TABLE_DUMP, COM_CONNECT_OUT
  );

const
  NOT_NULL_FLAG = 1;      // Field can't be NULL
  PRI_KEY_FLAG = 2;       // Field is part of a primary key
  UNIQUE_KEY_FLAG = 4;    // Field is part of a unique key
  MULTIPLE_KEY_FLAG = 8;  // Field is part of a key
  BLOB_FLAG = 16;         // Field is a blob
  UNSIGNED_FLAG = 32;     // Field is unsigned
  ZEROFILL_FLAG = 64;     // Field is zerofill
  BINARY_FLAG = 128;

  // The following are only sent to new clients

  ENUM_FLAG = 256;            // field is an enum
  AUTO_INCREMENT_FLAG = 512;  // field is a autoincrement field
  TIMESTAMP_FLAG = 1024;      // Field is a timestamp
  SET_FLAG = 2048;            // field is a set
  NUM_FLAG = 32768;           // Field is num (for clients)
  PART_KEY_FLAG = 16384;      // Intern; Part of some key
  GROUP_FLAG = 32768;         // Intern: Group field
  UNIQUE_FLAG = 65536;        // Intern: Used by sql_yacc

  REFRESH_GRANT = 1;     // Refresh grant tables
  REFRESH_LOG = 2;       // Start on new log file
  REFRESH_TABLES = 4;    // close all tables
  REFRESH_HOSTS = 8;     // Flush host cache
  REFRESH_STATUS = 16;   // Flush status variables
  REFRESH_THREADS = 32;  // Flush status variables
  REFRESH_SLAVE = 64;    // Reset master info and restart slave
                         // thread
  REFRESH_MASTER = 128;  // Remove all bin logs in the index
                         // and truncate the index

  // The following can't be set with mysql_refresh()

  REFRESH_READ_LOCK = 16384;  // Lock tables for read
  REFRESH_FAST = 32768;       // Intern flag

  CLIENT_LONG_PASSWORD = 1;      // new more secure passwords
  CLIENT_FOUND_ROWS = 2;         // Found instead of affected rows
  CLIENT_LONG_FLAG = 4;          // Get all column flags
  CLIENT_CONNECT_WITH_DB = 8;    // One can specify db on connect
  CLIENT_NO_SCHEMA = 16;         // Don't allow database.table.column
  CLIENT_COMPRESS = 32;          // Can use compression protcol
  CLIENT_ODBC = 64;              // Odbc client
  CLIENT_LOCAL_FILES = 128;      // Can use LOAD DATA LOCAL
  CLIENT_IGNORE_SPACE = 256;     // Ignore spaces before '('
  CLIENT_INTERACTIVE = 1024;     // This is an interactive client
  CLIENT_SSL = 2048;             // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE = 4096;  // IGNORE sigpipes
  CLIENT_TRANSACTIONS = 8192;    // Client knows about transactions

  SERVER_STATUS_IN_TRANS = 1;    // Transaction has started
  SERVER_STATUS_AUTOCOMMIT = 2;  // Server in auto_commit mode

  MYSQL_ERRMSG_SIZE = 200;
  NET_READ_TIMEOUT = 30;       // Timeout on read
  NET_WRITE_TIMEOUT = 60;      // Timeout on write
  NET_WAIT_TIMEOUT = 8*60*60;  // Wait for new query

type
  PVio = ^TVio;
  TVio = record
  end;

type
  PNET = ^TNET;
  TNET = record
    vio: PVio;
    fd: my_socket;
    fcntl: longint;
    buff, buff_end, write_pos, read_pos: pByte;
    last_error: array [0..MYSQL_ERRMSG_SIZE - 1] of char;
    last_errno, max_packet, timeout, pkt_nr: longword;
    error: byte;
    return_errno, compress: my_bool;
    no_send_ok: my_bool;  // needed if we are doing several
      // queries in one command ( as in LOAD TABLE ... FROM MASTER ),
      // and do not want to confuse the client with OK at the wrong time
    remain_in_buf, length, buf_length, where_b: longword;
    return_status: pLongword;
    reading_or_writing: byte;
    save_char: char;
  end;

const
  packet_error: longword = $ffffffff;

const
  FIELD_TYPE_DECIMAL = 0;
  FIELD_TYPE_TINY = 1;
  FIELD_TYPE_SHORT = 2;
  FIELD_TYPE_LONG = 3;
  FIELD_TYPE_FLOAT = 4;
  FIELD_TYPE_DOUBLE = 5;
  FIELD_TYPE_NULL = 6;
  FIELD_TYPE_TIMESTAMP = 7;
  FIELD_TYPE_LONGLONG = 8;
  FIELD_TYPE_INT24 = 9;
  FIELD_TYPE_DATE = 10;
  FIELD_TYPE_TIME = 11;
  FIELD_TYPE_DATETIME = 12;
  FIELD_TYPE_YEAR = 13;
  FIELD_TYPE_NEWDATE = 14;
  FIELD_TYPE_ENUM = 247;
  FIELD_TYPE_SET = 248;
  FIELD_TYPE_TINY_BLOB = 249;
  FIELD_TYPE_MEDIUM_BLOB = 250;
  FIELD_TYPE_LONG_BLOB = 251;
  FIELD_TYPE_BLOB = 252;
  FIELD_TYPE_VAR_STRING = 253;
  FIELD_TYPE_STRING = 254;

const
  FIELD_TYPE_CHAR = FIELD_TYPE_TINY;      // For compability
  FIELD_TYPE_INTERVAL = FIELD_TYPE_ENUM;  // For compability

type
  enum_field_types = FIELD_TYPE_DECIMAL..FIELD_TYPE_STRING;

// ------------------------
// From mysql_version.h ...
// ------------------------

const
  PROTOCOL_VERSION = 10;
  MYSQL_SERVER_VERSION = '3.23.49';
  MYSQL_SERVER_SUFFIX = '';
  FRM_VER = 6;
  MYSQL_VERSION_ID = 32349;
  MYSQL_PORT = 3306;
  MYSQL_UNIX_ADDR = '/tmp/mysql.sock';

// ----------------
// From mysql.h ...
// ----------------

function IS_PRI_KEY(n: longword): boolean;
function IS_NOT_NULL(n: longword): boolean;
function IS_BLOB(n: longword): boolean;
function IS_NUM(t: longword): boolean;

type
  PMYSQL_FIELD = ^TMYSQL_FIELD;
  TMYSQL_FIELD = record
    name: pChar;              // Name of column
    table: pChar;             // Table of column if column was a field
    def: pChar;               // Default value (set by mysql_list_fields)
    _type: enum_field_types;  // Type of field. Se mysql_com.h for types
    length: longword;         // Width of column
    max_length: longword;     // Max width of selected set
    flags: longword;          // Div flags
    decimals: longword;       // Number of decimals in field
  end;

function IS_NUM_FIELD(f: PMYSQL_FIELD): boolean;
function INTERNAL_NUM_FIELD(f: PMYSQL_FIELD): boolean;

type
  PMYSQL_ROW = ^TMYSQL_ROW;  // return data as array of strings
  TMYSQL_ROW = array[0..MaxInt div SizeOf(pChar) - 1] of pChar;

type
  MYSQL_FIELD_OFFSET = longword;  // offset to current field

type
  my_ulonglong = int64;

const
  MYSQL_COUNT_ERROR: my_ulonglong = not 0;

type
  PMYSQL_ROWS = ^TMYSQL_ROWS;
  TMYSQL_ROWS = record
    next: PMYSQL_ROWS;  // list of rows
    data: PMYSQL_ROW;
  end;

type
  MYSQL_ROW_OFFSET = PMYSQL_ROWS;  // offset to current row

type
  PMYSQL_DATA = ^TMYSQL_DATA;
  TMYSQL_DATA = record
    rows: my_ulonglong;
    fields: longword;
    data: PMYSQL_ROWS;
    alloc: TMEM_ROOT;
  end;

type
  PMYSQL_OPTIONS = ^TMYSQL_OPTIONS;
  TMYSQL_OPTIONS = record
    connect_timeout, client_flag: longword;
    compress, named_pipe: my_bool;
    port: longword;
    host, init_command, user, password, unix_socket, db: pChar;
    my_cnf_file, my_cnf_group, charset_dir, charset_name: pChar;
    use_ssl: my_bool;   // if to use SSL or not
    ssl_key: pChar;     // PEM key file
    ssl_cert: pChar;    // PEM cert file
    ssl_ca: pChar;      // PEM CA file
    ssl_capath: pChar;  // PEM directory of CA-s?
  end;

type
  mysql_option = (
    MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE, MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME,
    MYSQL_OPT_LOCAL_INFILE
  );

type
  mysql_status = (
    MYSQL_STATUS_READY, MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

type
  PMYSQL_FIELDS = ^TMYSQL_FIELDS;
  TMYSQL_FIELDS = array[0..MaxInt div SizeOf(TMYSQL_FIELD) - 1] of TMYSQL_FIELD;

type
  PCHARSET_INFO = ^TCHARSET_INFO;
  TCHARSET_INFO = record
    // Omitted: Structure not necessarily needed.
    // Definition of struct charset_info_st can be
    // found in include/m_ctype.h
  end;

type
  PMYSQL = ^TMYSQL;
  TMYSQL = record
    net: TNET;                    // Communication parameters
    connector_fd: gptr;           // ConnectorFd for SSL
    host, user, passwd, unix_socket, server_version, host_info, info, db: pChar;
    port, client_flag, server_capabilities: longword;
    protocol_version: longword;
    field_count: longword;
    server_status: longword;
    thread_id: longword;          // Id for connection in server
    affected_rows: my_ulonglong;
    insert_id: my_ulonglong;      // id if insert on table with NEXTNR
    extra_info: my_ulonglong;     // Used by mysqlshow
    packet_length: longword;
    status: mysql_status;
    fields: PMYSQL_FIELDS;
    field_alloc: TMEM_ROOT;
    free_me: my_bool;             // If free in mysql_close
    reconnect: my_bool;           // set to 1 if automatic reconnect
    options: TMYSQL_OPTIONS;
    scramble_buff: array [0..8] of char;
    charset: PCHARSET_INFO;
    server_language: longword;
  end;

type
  PMYSQL_RES = ^TMYSQL_RES;
  TMYSQL_RES = record
    row_count: my_ulonglong;
    field_count, current_field: longword;
    fields: PMYSQL_FIELDS;
    data: PMYSQL_DATA;
    data_cursor: PMYSQL_ROWS;
    field_alloc: TMEM_ROOT;
    row: PMYSQL_ROW;          // If unbuffered read
    current_row: PMYSQL_ROW;  // buffer to current row
    lengths: pLongword;       // column lengths of current row
    handle: PMYSQL;           // for unbuffered reads
    eof: my_bool;             // Used my mysql_fetch_row
  end;

// Functions to get information from the MYSQL and MYSQL_RES structures
// Should definitely be used if one uses shared libraries

var
  mysql_num_rows: function(res: PMYSQL_RES): my_ulonglong; stdcall;
  mysql_num_fields: function(res: PMYSQL_RES): longword; stdcall;
  mysql_eof: function(res: PMYSQL_RES): my_bool; stdcall;
  mysql_fetch_field_direct: function(res: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD; stdcall;
  mysql_fetch_fields: function(res: PMYSQL_RES): PMYSQL_FIELDS; stdcall;
  mysql_row_tell: function(res: PMYSQL_RES): PMYSQL_ROWS; stdcall;
  mysql_field_tell: function(res: PMYSQL_RES): longword; stdcall;

var
  mysql_field_count: function(_mysql: PMYSQL): longword; stdcall;
  mysql_affected_rows: function(_mysql: PMYSQL): my_ulonglong; stdcall;
  mysql_insert_id: function(_mysql: PMYSQL): my_ulonglong; stdcall;
  mysql_errno: function(_mysql: PMYSQL): longword; stdcall;
  mysql_error: function(_mysql: PMYSQL): pChar; stdcall;
  mysql_info: function(_mysql: PMYSQL): pChar; stdcall;
  mysql_thread_id: function(_mysql: PMYSQL): longword; stdcall;
  mysql_character_set_name: function(_mysql: PMYSQL): pChar; stdcall;

type
  PMYSQL_LENGTHS = ^TMYSQL_LENGTHS;
  TMYSQL_LENGTHS = array[0..MaxInt div SizeOf(longword) - 1] of longword;

type
  extend_buffer_func = function(void: pointer; _to: pChar; length: pLongword): pChar;

var
  mysql_init: function(_mysql: PMYSQL): PMYSQL; stdcall;
  {$IFDEF HAVE_OPENSSL}
  mysql_ssl_set: function(_mysql: PMYSQL; const key, cert, ca, capath: pChar): longint; stdcall;
  mysql_ssl_cipher: function(_mysql: PMYSQL): pChar; stdcall;
  mysql_ssl_clear: function(_mysql: PMYSQL): longint; stdcall;
  {$ENDIF} // HAVE_OPENSSL
  mysql_connect: function(_mysql: PMYSQL; const host, user, passwd: pChar): PMYSQL; stdcall;
  mysql_change_user: function(_mysql: PMYSQL; const user, passwd, db: pChar): my_bool; stdcall;
  mysql_real_connect: function(_mysql: PMYSQL; const host, user, passwd, db: pChar; port: longword; const unix_socket: pChar; clientflag: longword): PMYSQL; stdcall;
  mysql_close: procedure(sock: PMYSQL); stdcall;
  mysql_select_db: function(_mysql: PMYSQL; const db: pChar): longint; stdcall;
  mysql_query: function(_mysql: PMYSQL; const q: pChar): longint; stdcall;
  mysql_send_query: function(_mysql: PMYSQL; const q: pChar; length: longword): longint; stdcall;
  mysql_read_query_result: function(_mysql: PMYSQL): longint; stdcall;
  mysql_real_query: function(_mysql: PMYSQL; const q: pChar; length: longword): longint; stdcall;
  mysql_create_db: function(_mysql: PMYSQL; const DB: pChar): longint; stdcall;
  mysql_drop_db: function(_mysql: PMYSQL; const DB: pChar): longint; stdcall;
  mysql_shutdown: function(_mysql: PMYSQL): longint; stdcall;
  mysql_dump_debug_info: function(_mysql: PMYSQL): longint; stdcall;
  mysql_refresh: function(_mysql: PMYSQL; refresh_options: longword): longint; stdcall;
  mysql_kill: function(_mysql: PMYSQL; pid: longword): longint; stdcall;
  mysql_ping: function(_mysql: PMYSQL): longint; stdcall;
  mysql_stat: function(_mysql: PMYSQL): pChar; stdcall;
  mysql_get_server_info: function(_mysql: PMYSQL): pChar; stdcall;
  mysql_get_client_info: function: pChar; stdcall;
  mysql_get_host_info: function(_mysql: PMYSQL): pChar; stdcall;
  mysql_get_proto_info: function(_mysql: PMYSQL): longword; stdcall;
  mysql_list_dbs: function(_mysql: PMYSQL; const wild: pChar): PMYSQL_RES; stdcall;
  mysql_list_tables: function(_mysql: PMYSQL; const wild: pChar): PMYSQL_RES; stdcall;
  mysql_list_fields: function(_mysql: PMYSQL; const table, wild: pChar): PMYSQL_RES; stdcall;
  mysql_list_processes: function(_mysql: PMYSQL): PMYSQL_RES; stdcall;
  mysql_store_result: function(_mysql: PMYSQL): PMYSQL_RES; stdcall;
  mysql_use_result: function(_mysql: PMYSQL): PMYSQL_RES; stdcall;
  mysql_options: function(_mysql: PMYSQL; option: mysql_option; const arg: pChar): longint; stdcall;
  mysql_free_result: procedure(result: PMYSQL_RES); stdcall;
  mysql_data_seek: procedure(result: PMYSQL_RES; offset: my_ulonglong); stdcall;
  mysql_row_seek: function(result: PMYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; stdcall;
  mysql_field_seek: function(result: PMYSQL_RES; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; stdcall;
  mysql_fetch_row: function(result: PMYSQL_RES): PMYSQL_ROW; stdcall;
  mysql_fetch_lengths: function(result: PMYSQL_RES): PMYSQL_LENGTHS; stdcall;
  mysql_fetch_field: function(result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
  mysql_escape_string: function(_to: pChar; const from: pChar; from_length: longword): longword; stdcall;
  mysql_real_escape_string: function(_mysql: PMYSQL; _to: pChar; const from: pChar; length: longword): longword; stdcall;
  mysql_debug: procedure(const debug: pChar); stdcall;
  mysql_odbc_escape_string: function(_mysql: PMYSQL; _to: pChar; to_length: longword; const from: pChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): pChar; stdcall;
  myodbc_remove_escape: procedure(_mysql: PMYSQL; name: pChar); stdcall;
  mysql_thread_safe: function: longword; stdcall;

function mysql_reload(_mysql: PMySQL): longint;

// Status codes for libmySQL.dll

const
  LIBMYSQL_UNDEFINED = 0;     // libmysql_load() has not yet been called
  LIBMYSQL_MISSING = 1;       // No suitable DLL could be located
  LIBMYSQL_INCOMPATIBLE = 2;  // A DLL was found but it is not compatible
  LIBMYSQL_READY = 3;         // The DLL was loaded successfully

var
  libmysql_handle: HMODULE = 0;
  libmysql_status: byte = LIBMYSQL_UNDEFINED;

function libmysql_load(name: pChar): byte;
procedure libmysql_free;

// -----------------------------------------------------------------------------------------------
IMPLEMENTATION
// -----------------------------------------------------------------------------------------------

function IS_PRI_KEY(n: longword): boolean;
begin
  Result := (n and PRI_KEY_FLAG) = PRI_KEY_FLAG;
end;

function IS_NOT_NULL(n: longword): boolean;
begin
  Result := (n and NOT_NULL_FLAG) = NOT_NULL_FLAG;
end;

function IS_BLOB(n: longword): boolean;
begin
  Result := (n and BLOB_FLAG) = BLOB_FLAG;
end;

function IS_NUM(t: longword): boolean;
begin
  Result := (t <= FIELD_TYPE_INT24) or (t = FIELD_TYPE_YEAR);
end;

function IS_NUM_FIELD(f: PMYSQL_FIELD): boolean;
begin
  Result := (f.flags and NUM_FLAG) = NUM_FLAG;
end;

function INTERNAL_NUM_FIELD(f: PMYSQL_FIELD): boolean;
begin
  Result := (((f._type <= FIELD_TYPE_INT24) and ((f._type <> FIELD_TYPE_TIMESTAMP) or (f.length = 14) or (f.length = 8))) or (f._type = FIELD_TYPE_YEAR));
end;

function mysql_reload(_mysql: PMYSQL): longint;
begin
  Result := mysql_refresh(_mysql, REFRESH_GRANT);
end;

function libmysql_load(name: pChar): byte;

  procedure assign_proc(var proc: FARPROC; name: pChar);
  begin
    proc := GetProcAddress(libmysql_handle, name);
    if proc = nil then libmysql_status := LIBMYSQL_INCOMPATIBLE;
  end;

begin
  libmysql_free;
  if name = nil then name := 'libmysql.dll';
  libmysql_handle := LoadLibrary(name);
  if libmysql_handle = 0 then libmysql_status := LIBMYSQL_MISSING
  else begin
    libmysql_status := LIBMYSQL_READY;
    assign_proc(@mysql_num_rows, 'mysql_num_rows');
    assign_proc(@mysql_num_fields, 'mysql_num_fields');
    assign_proc(@mysql_eof, 'mysql_eof');
    assign_proc(@mysql_fetch_field_direct, 'mysql_fetch_field_direct');
    assign_proc(@mysql_fetch_fields, 'mysql_fetch_fields');
    assign_proc(@mysql_row_tell, 'mysql_row_tell');
    assign_proc(@mysql_field_tell, 'mysql_field_tell');
    assign_proc(@mysql_field_count, 'mysql_field_count');
    assign_proc(@mysql_affected_rows, 'mysql_affected_rows');
    assign_proc(@mysql_insert_id, 'mysql_insert_id');
    assign_proc(@mysql_errno, 'mysql_errno');
    assign_proc(@mysql_error, 'mysql_error');
    assign_proc(@mysql_info, 'mysql_info');
    assign_proc(@mysql_thread_id, 'mysql_thread_id');
    assign_proc(@mysql_character_set_name, 'mysql_character_set_name');
    assign_proc(@mysql_init, 'mysql_init');
    {$IFDEF HAVE_OPENSSL}
    assign_proc(@mysql_ssl_set, 'mysql_ssl_set');
    assign_proc(@mysql_ssl_cipher, 'mysql_ssl_cipher');
    assign_proc(@mysql_ssl_clear, 'mysql_ssl_clear');
    {$ENDIF} // HAVE_OPENSSL
    assign_proc(@mysql_connect, 'mysql_connect');
    assign_proc(@mysql_change_user, 'mysql_change_user');
    assign_proc(@mysql_real_connect, 'mysql_real_connect');
    assign_proc(@mysql_close, 'mysql_close');
    assign_proc(@mysql_select_db, 'mysql_select_db');
    assign_proc(@mysql_query, 'mysql_query');
    assign_proc(@mysql_send_query, 'mysql_send_query');
    assign_proc(@mysql_read_query_result, 'mysql_read_query_result');
    assign_proc(@mysql_real_query, 'mysql_real_query');
    assign_proc(@mysql_create_db, 'mysql_create_db');
    assign_proc(@mysql_drop_db, 'mysql_drop_db');
    assign_proc(@mysql_shutdown, 'mysql_shutdown');
    assign_proc(@mysql_dump_debug_info, 'mysql_dump_debug_info');
    assign_proc(@mysql_refresh, 'mysql_refresh');
    assign_proc(@mysql_kill, 'mysql_kill');
    assign_proc(@mysql_ping, 'mysql_ping');
    assign_proc(@mysql_stat, 'mysql_stat');
    assign_proc(@mysql_get_server_info, 'mysql_get_server_info');
    assign_proc(@mysql_get_client_info, 'mysql_get_client_info');
    assign_proc(@mysql_get_host_info, 'mysql_get_host_info');
    assign_proc(@mysql_get_proto_info, 'mysql_get_proto_info');
    assign_proc(@mysql_list_dbs, 'mysql_list_dbs');
    assign_proc(@mysql_list_tables, 'mysql_list_tables');
    assign_proc(@mysql_list_fields, 'mysql_list_fields');
    assign_proc(@mysql_list_processes, 'mysql_list_processes');
    assign_proc(@mysql_store_result, 'mysql_store_result');
    assign_proc(@mysql_use_result, 'mysql_use_result');
    assign_proc(@mysql_options, 'mysql_options');
    assign_proc(@mysql_free_result, 'mysql_free_result');
    assign_proc(@mysql_data_seek, 'mysql_data_seek');
    assign_proc(@mysql_row_seek, 'mysql_row_seek');
    assign_proc(@mysql_field_seek, 'mysql_field_seek');
    assign_proc(@mysql_fetch_row, 'mysql_fetch_row');
    assign_proc(@mysql_fetch_lengths, 'mysql_fetch_lengths');
    assign_proc(@mysql_fetch_field, 'mysql_fetch_field');
    assign_proc(@mysql_escape_string, 'mysql_escape_string');
    assign_proc(@mysql_real_escape_string, 'mysql_real_escape_string');
    assign_proc(@mysql_debug, 'mysql_debug');
    assign_proc(@mysql_odbc_escape_string, 'mysql_odbc_escape_string');
    assign_proc(@myodbc_remove_escape, 'myodbc_remove_escape');
    assign_proc(@mysql_thread_safe, 'mysql_thread_safe');
  end;
  Result := libmysql_status;
end;

procedure libmysql_free;
begin
  if libmysql_handle <> 0 then FreeLibrary(libmysql_handle);
  libmysql_handle := 0;
  libmysql_status := LIBMYSQL_UNDEFINED;
end;

// -----------------------------------------------------------------------------------------------
INITIALIZATION
// -----------------------------------------------------------------------------------------------

begin
  {$IFNDEF DONT_LOAD_DLL}
  libmysql_load(nil);
  {$ENDIF} // DONT_LOAD_DLL
end;

// -----------------------------------------------------------------------------------------------
FINALIZATION
// -----------------------------------------------------------------------------------------------

begin
  libmysql_free;
end;

end.

