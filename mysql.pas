// tabs = 2
// -----------------------------------------------------------------------------------------------
//
//                             MySQL Client API for Delphi 4 and later
//
//                    Delphi Interface Unit for the libmySQL.dll Client Library
//                               for MySQL AB's SQL Database Server
//
//               Translation of MySQL AB's mysql.h, mysql_com.h and mysql_version.h
//
//                     Copyright © 1999 - 2001 Medienagentur Fichtner & Meyer
//                             Header Translation by Matthias Fichtner
//
// -----------------------------------------------------------------------------------------------
//                      See mysql.h for MySQL AB's copyright and GPL notice!
// -----------------------------------------------------------------------------------------------
//
//     17-Aug-1999  mf  Translated mysql.h                           MySQL-Win32 3.22.24
//     19-Aug-1999  mf  Corrected some type definitions              MySQL-Win32 3.22.24
//     20-Aug-1999  mf  Finished debugging the unit                  MySQL-Win32 3.22.24
//     18-Sep-1999  mf  Code maintenance for release 3.22.26a        MySQL-Win32 3.22.26a
//     22-Oct-1999  mf  Code maintenance for release 3.22.28         MySQL-Win32 3.22.28
//     02-Jan-2000  mf  Code maintenance for release 3.22.29         MySQL-Win32 3.22.29
//     21-Jan-2000  mf  Code maintenance for release 3.22.30         MySQL-Win32 3.22.30
//     07-Feb-2000  mf  Code maintenance for release 3.22.31         MySQL-Win32 3.22.31
//     16-Feb-2000  mf  Code maintenance for release 3.22.32         MySQL-Win32 3.22.32
//     13-Aug-2000  mf  Code maintenance for release 3.22.34         MySQL-Win32 3.22.34
//     14-Aug-2000  mf  Reworked entire unit for first 3.23 release  MySQL-Win32 3.23.19-beta
//     14-Aug-2000  mf  Added mysql_character_set_name()             MySQL-Win32 3.23.22-beta
//     11-Sep-2000  mf  Added IS_NUM_FIELD and INTERNAL_NUM_FIELD    MySQL-Win32 3.23.24-beta
//                      (definition of new constant NUM_FLAG seems
//                      to be buggy - should be 4096 or 8192)
//     08-Oct-2000  mf  Modified TMEM_ROOT, enum_server_command,     MySQL-Win32 3.23.25-beta
//                      and INTERNAL_NUM_FIELD
//     01-Nov-2000  mf  Code maintenance for release 3.23.27         MySQL-Win32 3.23.27-beta
//     25-Nov-2000  mf  Code maintenance for release 3.23.28         MySQL-Win32 3.23.28-gamma
//     05-Jan-2001  mf  Code maintenance for release 3.23.30         MySQL-Win32 3.23.30-gamma
//     19-Jan-2001  mf  Code maintenance for release 3.23.31         MySQL-Win32 3.23.31
//     11-Mar-2001  mf  Added functions mysql_real_send_query(),     MySQL-Win32 3.23.33
//                      mysql_send_query(), and mysql_reap_query()
//
// -----------------------------------------------------------------------------------------------
//                  The latest release of mysql.pas will always be available from
//                 the distribution site at: http://www.fichtner.net/delphi/mysql/
// -----------------------------------------------------------------------------------------------
//                       Please send questions, bug reports and suggestions
//                      regarding this code to: mfichtner@fichtner-meyer.com
// -----------------------------------------------------------------------------------------------
//                        This code is provided "as is" without express or
//                     implied warranty of any kind. Use it at your own risk.
// -----------------------------------------------------------------------------------------------

unit mysql;

// -----------------------------------------------------------------------------------------------
INTERFACE
// -----------------------------------------------------------------------------------------------

uses
	Windows,  // Needed for some type definitions
	Winsock;  // Needed for some type definitions

// ----------------
// from mysql.h ...
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
// from mysql_com.h ...
// --------------------

const
	NAME_LEN = 64;                // Field/table name length
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
	CLIENT_TRANSACTIONS = 8196;    // Client knows about transactions

	SERVER_STATUS_IN_TRANS = 1;	   // Transaction has started
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
// from mysql_version.h ...
// ------------------------

const
	PROTOCOL_VERSION = 10;
	MYSQL_SERVER_VERSION = '3.23.33';
	MYSQL_SERVER_SUFFIX = '';
	FRM_VER = 6;
	MYSQL_VERSION_ID = 32333;
	MYSQL_PORT = 3306;
	MYSQL_UNIX_ADDR = '/tmp/mysql.sock';

// ----------------
// from mysql.h ...
// ----------------

function IS_PRI_KEY(n: longword): boolean;
function IS_NOT_NULL(n: longword): boolean;
function IS_BLOB(n: longword): boolean;
function IS_NUM(t: longword): boolean;

type
	PMYSQL_FIELD = ^TMYSQL_FIELD;
	TMYSQL_FIELD  = record
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
		MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME
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

const
	DLL = 'libmySQL.dll';

// Functions to get information from the MYSQL and MYSQL_RES structures
// Should definitely be used if one uses shared libraries

function mysql_num_rows(res: PMYSQL_RES): my_ulonglong; stdcall; external DLL;
function mysql_num_fields(res: PMYSQL_RES): longword; stdcall; external DLL;
function mysql_eof(res: PMYSQL_RES): my_bool; stdcall; external DLL;
function mysql_fetch_field_direct(res: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD; stdcall; external DLL;
function mysql_fetch_fields(res: PMYSQL_RES): PMYSQL_FIELDS; stdcall; external DLL;
function mysql_row_tell(res: PMYSQL_RES): PMYSQL_ROWS; stdcall; external DLL;
function mysql_field_tell(res: PMYSQL_RES): longword; stdcall; external DLL;

function mysql_field_count(_mysql: PMYSQL): longword; stdcall; external DLL;
function mysql_affected_rows(_mysql: PMYSQL): my_ulonglong; stdcall; external DLL;
function mysql_insert_id(_mysql: PMYSQL): my_ulonglong; stdcall; external DLL;
function mysql_errno(_mysql: PMYSQL): longword; stdcall; external DLL;
function mysql_error(_mysql: PMYSQL): pChar; stdcall; external DLL;
function mysql_info(_mysql: PMYSQL): pChar; stdcall; external DLL;
function mysql_thread_id(_mysql: PMYSQL): longword; stdcall; external DLL;
function mysql_character_set_name(_mysql: PMYSQL): pChar; stdcall; external DLL;

type
	PMYSQL_LENGTHS = ^TMYSQL_LENGTHS;
	TMYSQL_LENGTHS = array[0..MaxInt div SizeOf(longword) - 1] of longword;

type
	extend_buffer_func = function(void: pointer; _to: pChar; length: pLongword): pChar;

function mysql_init(_mysql: PMYSQL): PMYSQL; stdcall; external DLL;
{$IFDEF HAVE_OPENSSL}
function mysql_ssl_set(_mysql: PMYSQL; const key, cert, ca, capath: pChar): longint; stdcall; external DLL;
function mysql_ssl_cipher(_mysql: PMYSQL): pChar; stdcall; external DLL;
function mysql_ssl_clear(_mysql: PMYSQL): longint; stdcall; external DLL;
{$ENDIF} // HAVE_OPENSSL
function mysql_connect(_mysql: PMYSQL; const host, user, passwd: pChar): PMYSQL; stdcall; external DLL;
function mysql_change_user(_mysql: PMYSQL; const user, passwd, db: pChar): my_bool; stdcall; external DLL;
function mysql_real_connect(_mysql: PMYSQL; const host, user, passwd, db: pChar; port: longword; const unix_socket: pChar; clientflag: longword): PMYSQL; stdcall; external DLL;
procedure mysql_close(sock: PMYSQL); stdcall; external DLL;
function mysql_select_db(_mysql: PMYSQL; const db: pChar): longint; stdcall; external DLL;
function mysql_query(_mysql: PMYSQL; const q: pChar): longint; stdcall; external DLL;
function mysql_send_query(_mysql: PMYSQL; const q: pChar): longint; stdcall; external DLL;
function mysql_reap_query(_mysql: PMYSQL): longint; stdcall; external DLL;
function mysql_real_query(_mysql: PMYSQL; const q: pChar; length: longword): longint; stdcall; external DLL;
function mysql_real_send_query(_mysql: PMYSQL; const q: pChar; len: longword): longint; stdcall; external DLL;
function mysql_create_db(_mysql: PMYSQL; const DB: pChar): longint; stdcall; external DLL;
function mysql_drop_db(_mysql: PMYSQL; const DB: pChar): longint; stdcall; external DLL;
function mysql_shutdown(_mysql: PMYSQL): longint; stdcall; external DLL;
function mysql_dump_debug_info(_mysql: PMYSQL): longint; stdcall; external DLL;
function mysql_refresh(_mysql: PMYSQL; refresh_options: longword): longint; stdcall; external DLL;
function mysql_kill(_mysql: PMYSQL; pid: longword): longint; stdcall; external DLL;
function mysql_ping(_mysql: PMYSQL): longint; stdcall; external DLL;
function mysql_stat(_mysql: PMYSQL): pChar; stdcall; external DLL;
function mysql_get_server_info(_mysql: PMYSQL): pChar; stdcall; external DLL;
function mysql_get_client_info: pChar; stdcall; external DLL;
function mysql_get_host_info(_mysql: PMYSQL): pChar; stdcall; external DLL;
function mysql_get_proto_info(_mysql: PMYSQL): longword; stdcall; external DLL;
function mysql_list_dbs(_mysql: PMYSQL; const wild: pChar): PMYSQL_RES; stdcall; external DLL;
function mysql_list_tables(_mysql: PMYSQL; const wild: pChar): PMYSQL_RES; stdcall; external DLL;
function mysql_list_fields(_mysql: PMYSQL; const table, wild: pChar): PMYSQL_RES; stdcall; external DLL;
function mysql_list_processes(_mysql: PMYSQL): PMYSQL_RES; stdcall; external DLL;
function mysql_store_result(_mysql: PMYSQL): PMYSQL_RES; stdcall; external DLL;
function mysql_use_result(_mysql: PMYSQL): PMYSQL_RES; stdcall; external DLL;
function mysql_options(_mysql: PMYSQL; option: mysql_option; const arg: pChar): longint; stdcall; external DLL;
procedure mysql_free_result(result: PMYSQL_RES); stdcall; external DLL;
procedure mysql_data_seek(result: PMYSQL_RES; offset: my_ulonglong); stdcall; external DLL;
function mysql_row_seek(result: PMYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; stdcall; external DLL;
function mysql_field_seek(result: PMYSQL_RES; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; stdcall; external DLL;
function mysql_fetch_row(result: PMYSQL_RES): PMYSQL_ROW; stdcall; external DLL;
function mysql_fetch_lengths(result: PMYSQL_RES): PMYSQL_LENGTHS; stdcall; external DLL;
function mysql_fetch_field(result: PMYSQL_RES): PMYSQL_FIELD; stdcall; external DLL;
function mysql_escape_string(_to: pChar; const from: pChar; from_length: longword): longword; stdcall; external DLL;
function mysql_real_escape_string(_mysql: PMYSQL; _to: pChar; const from: pChar; length: longword): longword; stdcall; external DLL;
procedure mysql_debug(const debug: pChar); stdcall; external DLL;
function mysql_odbc_escape_string(_mysql: PMYSQL; _to: pChar; to_length: longword; const from: pChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): pChar; stdcall; external DLL;
procedure myodbc_remove_escape(_mysql: PMYSQL; name: pChar); stdcall; external DLL;
function mysql_thread_safe: longword; stdcall; external DLL;

function mysql_reload(_mysql: PMySQL): longint;

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

end.

