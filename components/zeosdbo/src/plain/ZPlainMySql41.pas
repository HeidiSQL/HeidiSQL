{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to libmysql.dll           }
{                     Version 4.1                         }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
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

unit ZPlainMySql41;

interface

{$I ZPlain.inc}

{$J+}
{$Z4}
{$IFNDEF VER130BELOW}
{$A8}
{$ENDIF}

uses Classes, ZPlainLoader, ZCompatibility;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS1_DLL_LOCATION = 'libmysql41.dll';
{$IFNDEF STRICT_DLL_LOADING}
  WINDOWS2_DLL_LOCATION = 'libmysql.dll';
{$ENDIF}
  LINUX_DLL_LOCATION = 'libmysqlclient.so';

{ General Declarations }
  MYSQL_ERRMSG_SIZE    = 512;
  SQLSTATE_LENGTH      = 5;
  SCRAMBLE_LENGTH      = 20;

  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';
  NAME_LEN             = 64;
  PROTOCOL_VERSION     = 10;
  FRM_VER              = 6;

{ Enum Field Types }
  FIELD_TYPE_DECIMAL   = 0;
  FIELD_TYPE_TINY      = 1;
  FIELD_TYPE_SHORT     = 2;
  FIELD_TYPE_LONG      = 3;
  FIELD_TYPE_FLOAT     = 4;
  FIELD_TYPE_DOUBLE    = 5;
  FIELD_TYPE_NULL      = 6;
  FIELD_TYPE_TIMESTAMP = 7;
  FIELD_TYPE_LONGLONG  = 8;
  FIELD_TYPE_INT24     = 9;
  FIELD_TYPE_DATE      = 10;
  FIELD_TYPE_TIME      = 11;
  FIELD_TYPE_DATETIME  = 12;
  FIELD_TYPE_YEAR      = 13;
  FIELD_TYPE_NEWDATE   = 14;
  FIELD_TYPE_ENUM      = 247;
  FIELD_TYPE_SET       = 248;
  FIELD_TYPE_TINY_BLOB = 249;
  FIELD_TYPE_MEDIUM_BLOB = 250;
  FIELD_TYPE_LONG_BLOB = 251;
  FIELD_TYPE_BLOB      = 252;
  FIELD_TYPE_VAR_STRING = 253;
  FIELD_TYPE_STRING    = 254;
  FIELD_TYPE_GEOMETRY  = 255;

{ For Compatibility }
  FIELD_TYPE_CHAR      = FIELD_TYPE_TINY;
  FIELD_TYPE_INTERVAL  = FIELD_TYPE_ENUM;

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
  GROUP_FLAG	         = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }

{ Server Administration Refresh Options }
  REFRESH_GRANT	         = 1;     { Refresh grant tables }
  REFRESH_LOG		 = 2;     { Start on new log file }
  REFRESH_TABLES	 = 4;     { close all tables }
  REFRESH_HOSTS	         = 8;     { Flush host cache }
  REFRESH_STATUS         = 16;    { Flush status variables }
  REFRESH_THREADS        = 32;    { Flush status variables }
  REFRESH_SLAVE          = 64;    { Reset master info abd restat slave thread }
  REFRESH_MASTER         = 128;   { Remove all bin logs in the index and truncate the index }
  REFRESH_READ_LOCK      = 16384; { Lock tables for read }
  REFRESH_FAST		 = 32768; { Intern flag }
  REFRESH_QUERY_CACHE	 = 65536;
  REFRESH_QUERY_CACHE_FREE = $20000; { Pack query cache }
  REFRESH_DES_KEY_FILE	 = $40000;
  REFRESH_USER_RESOURCES = $80000;

{ Client Connection Options }
  _CLIENT_LONG_PASSWORD	  = 1;	  { new more secure passwords }
  _CLIENT_FOUND_ROWS	  = 2;	  { Found instead of affected rows }
  _CLIENT_LONG_FLAG	  = 4;	  { Get all column flags }
  _CLIENT_CONNECT_WITH_DB = 8;	  { One can specify db on connect }
  _CLIENT_NO_SCHEMA	  = 16;	  { Don't allow database.table.column }
  _CLIENT_COMPRESS	  = 32;	  { Can use compression protcol }
  _CLIENT_ODBC		  = 64;	  { Odbc client }
  _CLIENT_LOCAL_FILES	  = 128;  { Can use LOAD DATA LOCAL }
  _CLIENT_IGNORE_SPACE	  = 256;  { Ignore spaces before '(' }
  _CLIENT_PROTOCOL_41	  = 512;  { New 4.1 protocol }
  _CLIENT_INTERACTIVE	  = 1024; { This is an interactive client }
  _CLIENT_SSL             = 2048; { Switch to SSL after handshake }
  _CLIENT_IGNORE_SIGPIPE  = 4096; { IGNORE sigpipes }
  _CLIENT_TRANSACTIONS	  = 8192; { Client knows about transactions }
  _CLIENT_RESERVED        = 16384; { Old flag for 4.1 protocol  }
  _CLIENT_SECURE_CONNECTION = 32768; { New 4.1 authentication }
  _CLIENT_MULTI_STATEMENTS = 65536; { Enable/disable multi-stmt support }
  _CLIENT_MULTI_RESULTS   = 131072; { Enable/disable multi-results }
  _CLIENT_REMEMBER_OPTIONS = 2147483648; {Enable/disable multi-results }

{ Net type }
  NET_TYPE_TCPIP = 0;
  NET_TYPE_SOCKET = 1;
  NET_TYPE_NAMEDPIPE = 2;

  MAX_MYSQL_MANAGER_ERR = 256;
  MAX_MYSQL_MANAGER_MSG = 256;

  MANAGER_OK           = 200;
  MANAGER_INFO         = 250;
  MANAGER_ACCESS       = 401;
  MANAGER_CLIENT_ERR   = 450;
  MANAGER_INTERNAL_ERR = 500;

{ ****************** Plain API Types definition ***************** }

type
  TClientCapabilities = (
    CLIENT_LONG_PASSWORD,
    CLIENT_FOUND_ROWS,
    CLIENT_LONG_FLAG,
    CLIENT_CONNECT_WITH_DB,
    CLIENT_NO_SCHEMA,
    CLIENT_COMPRESS,
    CLIENT_ODBC,
    CLIENT_LOCAL_FILES,
    CLIENT_IGNORE_SPACE
  );

  TSetClientCapabilities = set of TClientCapabilities;

  TRefreshOptions = (
    _REFRESH_GRANT,
    _REFRESH_LOG,
    _REFRESH_TABLES,
    _REFRESH_HOSTS,
    _REFRESH_FAST
  );
  TSetRefreshOptions = set of TRefreshOptions;

  TMySqlStatus = (
    MYSQL_STATUS_READY,
    MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

  TMySqlOption = (
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

  TMySqlRplType = (
    MYSQL_RPL_MASTER,
    MYSQL_RPL_SLAVE,
    MYSQL_RPL_ADMIN
  );

  TMySqlServerCommand = (
    COM_SLEEP,
    COM_QUIT,
    COM_INIT_DB,
    COM_QUERY,
    COM_FIELD_LIST,
    COM_CREATE_DB,
    COM_DROP_DB,
    COM_REFRESH,
    COM_SHUTDOWN,
    COM_STATISTICS,
    COM_PROCESS_INFO,
    COM_CONNECT,
    COM_PROCESS_KILL,
    COM_DEBUG,
    COM_PING,
    COM_TIME,
    COM_DELAYED_INSERT,
    COM_CHANGE_USER,
    COM_BINLOG_DUMP,
    COM_TABLE_DUMP,
    COM_CONNECT_OUT,
    COM_REGISTER_SLAVE,
    COM_PREPARE,
    COM_EXECUTE,
    COM_LONG_DATA,
    COM_CLOSE_STMT,
    COM_RESET_STMT,
    COM_SET_OPTION,
    COM_END
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
    free:          PUSED_MEM;
    used:          PUSED_MEM;
    pre_alloc:     PUSED_MEM;
    min_malloc:    Integer;
    block_size:    Integer;
    block_num:     Integer;
    first_block_usage: Integer;
    error_handler: PERR_PROC;
  end;

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

  MYSQL_FIELD_OFFSET = Cardinal;

  MYSQL_ROW = array[00..$ff] of PChar;
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

  PMYSQL_OPTIONS = ^_MYSQL_OPTIONS;
  _MYSQL_OPTIONS = record
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
    methods_to_use:           TMySqlOption;
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

  PMYSQL_METHODS =  ^MYSQL_METHODS;

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
    options:         _mysql_options;
    status:          TMySqlStatus;
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
    methods:         PMYSQL_METHODS;
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

  MYSQL_METHODS = record
    read_query_result: function(handle: PMYSQL): Byte;
    advanced_command:  function(handle: PMYSQL; command: TMySqlServerCommand;
      header: PChar; header_length: LongInt; const arg: PChar;
      arg_length: LongInt; skip_check: Byte): Byte;
    read_rows: function( handle: PMYSQL; mysql_fields: PMYSQL_FIELD;
      fields: Cardinal): PMYSQL_DATA;
    use_result: function(handle: PMYSQL): PMYSQL_RES;
    fetch_lengths: procedure(_to: PLongInt; column: MYSQL_ROW;
      field_count: Cardinal);
    list_fields: function(handle: PMYSQL): PMYSQL_FIELD;
    read_prepare_result: function(handle: PMYSQL; stmt: PMYSQL_STMT): Byte;
    stmt_execute: function(stmt: PMYSQL_STMT): Integer;
    read_binary_rows: function(stmt: PMYSQL_STMT): PMYSQL_DATA;
    unbuffered_fetch: function(handle: PMYSQL; row: PMYSQL_ROW): Integer;
    free_embedded_thd: procedure(handle: PMYSQL);
    read_statisticd: function(handle: PMYSQL): PChar;
  end;

  TModifyType = (MODIFY_INSERT, MODIFY_UPDATE, MODIFY_DELETE);
  TQuoteOptions = (QUOTE_STRIP_CR,QUOTE_STRIP_LF);
  TQuoteOptionsSet = set of TQuoteOptions;

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
  PMYSQL_MANAGER = ^MYSQL_MANAGER;

  { Options for mysql_set_option }
  TMySqlSetOption = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF
  );
  
{ ************** Plain API Function types definition ************* }

  {
    Set up and bring down the server; to ensure that applications will
    work when linked against either the standard client library or the
    embedded server library, these functions should be called.
  }
  Tmysql_server_init = function(Argc: Integer; Argv, Groups: Pointer): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_server_end = procedure;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set up and bring down a thread; these function should be called
    for each thread in an application which opens at least one MySQL
    connection.  All uses of the connection(s) should be between these
    function calls. }
  Tmysql_thread_init = function: Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_thread_end = procedure;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Functions to get information from the MYSQL and MYSQL_RES structures
    Should definitely be used if one uses shared libraries. }
  Tmysql_num_rows = function(Result: PMYSQL_RES): Int64;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_num_fields = function(Result: PMYSQL_RES): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_eof = function(Result: PMYSQL_RES): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_fetch_field_direct = function(Result: PMYSQL_RES;
    FieldNo: Cardinal): PMYSQL_FIELD;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_fetch_fields = function(Result: PMYSQL_RES): PMYSQL_FIELD;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_row_tell = function(Result: PMYSQL_RES): MYSQL_ROW_OFFSET;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_field_tell = function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_field_count = function(Handle: PMYSQL): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_affected_rows = function(Handle: PMYSQL): Int64;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_insert_id = function(Handle: PMYSQL): Int64;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_errno = function(Handle: PMYSQL): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_error = function(Handle: PMYSQL): PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_sqlstate = function(Handle: PMYSQL): PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_warning_count = function(Handle: PMYSQL): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_info = function(Handle: PMYSQL): PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_thread_id = function(Handle: PMYSQL): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_character_set_name = function(Handle: PMYSQL): PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_init = function(Handle: PMYSQL): PMYSQL;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_ssl_set = function(mysql: PMYSQL; const key: PChar; const cert: PChar;
    const ca: PChar; const capath: PChar; const cipher: PChar): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_change_user = function(mysql: PMYSQL; const user: PChar;
    const passwd: PChar; const db: PChar): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_real_connect = function(Handle: PMYSQL;
    const Host, User, Passwd, Db: PChar; Port: Cardinal;
    UnixSocket: PChar; ClientFlag: Cardinal): PMYSQL;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_select_db = function(Handle: PMYSQL; const Db: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_query = function(Handle: PMYSQL; const Query: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_real_query = function(Handle: PMYSQL; const Query: PChar;
    Len: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_store_result = function(Handle: PMYSQL): PMYSQL_RES;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_use_result = function(Handle: PMYSQL): PMYSQL_RES;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Perform query on master }
  Tmysql_master_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_master_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Perform query on slave }
  Tmysql_slave_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_slave_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Enable/disable parsing of all queries to decide
    if they go on master or slave }
  Tmysql_enable_rpl_parse = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_disable_rpl_parse = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Get the value of the parse flag }
  Tmysql_rpl_parse_enabled = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Enable/disable reads from master }
  Tmysql_enable_reads_from_master = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_disable_reads_from_master = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Get the value of the master read flag }
  Tmysql_reads_from_master_enabled = function(mysql: PMYSQL): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_rpl_query_type = function(const query: PChar; len: Integer):
    TMySqlRplType; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Discover the master and its slaves }
  Tmysql_rpl_probe = function(mysql: PMYSQL): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set the master, close/free the old one, if it is not a pivot }
  Tmysql_set_master = function(mysql: PMYSQL; const host: PChar;
    port: Cardinal; const user: PChar; const passwd: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_add_slave = function(mysql: PMYSQL; const host: PChar;
    port: Cardinal; const user: PChar; const passwd: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_shutdown = function(Handle: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_dump_debug_info = function(Handle: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_refresh = function(Handle: PMYSQL; Options: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_kill = function(Handle: PMYSQL; Pid: LongInt): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_set_server_option = function(Handle: PMYSQL;
    Option: TMySqlSetOption): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_ping = function(Handle: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_stat = function(Handle: PMYSQL): PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_get_server_info = function(Handle: PMYSQL): PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_get_client_info = function: PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_get_client_version = function: Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_get_host_info = function(Handle: PMYSQL): PChar;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_get_server_version = function(Handle: PMYSQL): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_get_proto_info = function(Handle: PMYSQL): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_list_dbs = function(Handle: PMYSQL; Wild: PChar): PMYSQL_RES;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_list_tables = function(Handle: PMYSQL; const Wild: PChar): PMYSQL_RES;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_list_fields = function(Handle: PMYSQL; const Table, Wild: PChar):
    PMYSQL_RES; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_list_processes = function(Handle: PMYSQL): PMYSQL_RES;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_options = function(Handle: PMYSQL; Option: TMySqlOption;
    const Arg: PChar): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_free_result = procedure(Result: PMYSQL_RES);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_data_seek = procedure(Result: PMYSQL_RES; Offset: Int64);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_row_seek = function(Result: PMYSQL_RES; Row: MYSQL_ROW_OFFSET):
    MYSQL_ROW_OFFSET; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_field_seek = function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET):
    MYSQL_FIELD_OFFSET; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_fetch_row = function(Result: PMYSQL_RES): PMYSQL_ROW;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_fetch_lengths = function(Result: PMYSQL_RES): PLongInt;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_fetch_field = function(Result: PMYSQL_RES): PMYSQL_FIELD;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_escape_string = function(PTo, PFrom: PChar; Len: Cardinal): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_real_escape_string = function(mysql: PMYSQL; toStr: PChar;
    const fromStr: PChar; length: Cardinal): Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_debug = procedure(Debug: PChar);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_thread_safe = function: Cardinal;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_init = function(con: PMYSQL_MANAGER): PMYSQL_MANAGER;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_connect = function(con: PMYSQL_MANAGER; const host: PChar;
    const user: PChar; const passwd: PChar; port: Cardinal): PMYSQL_MANAGER;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_close = procedure(con: PMYSQL_MANAGER);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_command = function(con: PMYSQL_MANAGER; const cmd: PChar;
    cmd_len: Integer): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_fetch_line = function(con: PMYSQL_MANAGER; res_buf: PChar;
    res_buf_size: Integer): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_read_query_result = function(mysql: PMYSQL): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_connect = function(Handle: PMYSQL; const Host, User, Passwd: PChar):
    PMYSQL; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_create_db = function(Handle: PMYSQL; const Db: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_drop_db = function(Handle: PMYSQL; const Db: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_close = procedure(Handle: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

var

{ ************* Plain API Function variables definition ************ }

  mysql_debug:          Tmysql_debug;
  mysql_dump_debug_info: Tmysql_dump_debug_info;
  mysql_server_init:     Tmysql_server_init;
  mysql_server_end:     Tmysql_server_end;
  mysql_init:           Tmysql_init;
  mysql_connect:        Tmysql_connect;
  mysql_real_connect:   Tmysql_real_connect;
  mysql_close:          Tmysql_close;
  mysql_select_db:      Tmysql_select_db;
  mysql_create_db:      Tmysql_create_db;
  mysql_drop_db:        Tmysql_drop_db;
  mysql_query:          Tmysql_query;
  mysql_real_query:     Tmysql_real_query;
  mysql_shutdown:       Tmysql_shutdown;
  mysql_refresh:        Tmysql_refresh;
  mysql_kill:           Tmysql_kill;
  mysql_ping:           Tmysql_ping;
  mysql_stat:           Tmysql_stat;
  mysql_options:        Tmysql_options;
  mysql_escape_string:  Tmysql_escape_string;
  mysql_get_server_info: Tmysql_get_server_info;
  mysql_get_client_info: Tmysql_get_client_info;
  mysql_get_host_info:  Tmysql_get_host_info;
  mysql_get_proto_info: Tmysql_get_proto_info;
  mysql_list_dbs:       Tmysql_list_dbs;
  mysql_list_tables:    Tmysql_list_tables;
  mysql_list_fields:    Tmysql_list_fields;
  mysql_list_processes: Tmysql_list_processes;
  mysql_data_seek:      Tmysql_data_seek;
  mysql_row_seek:       Tmysql_row_seek;
  mysql_field_seek:     Tmysql_field_seek;
  mysql_fetch_row:      Tmysql_fetch_row;
  mysql_fetch_lengths:  Tmysql_fetch_lengths;
  mysql_fetch_field:    Tmysql_fetch_field;
  mysql_store_result:   Tmysql_store_result;
  mysql_use_result:     Tmysql_use_result;
  mysql_free_result:    Tmysql_free_result;

  { New functions in MySQL 4.0 }

  mysql_thread_init:    Tmysql_thread_init;
  mysql_thread_end:     Tmysql_thread_end;
  mysql_ssl_set:        Tmysql_ssl_set;
  mysql_change_user:    Tmysql_change_user;
  mysql_send_query:     Tmysql_send_query;
  mysql_read_query_result: Tmysql_read_query_result;
  mysql_master_query:   Tmysql_master_query;
  mysql_master_send_query: Tmysql_master_send_query;
  mysql_slave_query:    Tmysql_slave_query;
  mysql_slave_send_query: Tmysql_slave_send_query;
  mysql_enable_rpl_parse: Tmysql_enable_rpl_parse;
  mysql_disable_rpl_parse: Tmysql_disable_rpl_parse;
  mysql_rpl_parse_enabled: Tmysql_rpl_parse_enabled;
  mysql_enable_reads_from_master: Tmysql_enable_reads_from_master;
  mysql_disable_reads_from_master: Tmysql_disable_reads_from_master;
  mysql_reads_from_master_enabled: Tmysql_reads_from_master_enabled;
  mysql_rpl_query_type: Tmysql_rpl_query_type;
  mysql_rpl_probe:      Tmysql_rpl_probe;
  mysql_set_master:     Tmysql_set_master;
  mysql_add_slave:      Tmysql_add_slave;
  mysql_real_escape_string: Tmysql_real_escape_string;
  mysql_thread_safe:    Tmysql_thread_safe;
  mysql_manager_init:   Tmysql_manager_init;
  mysql_manager_connect: Tmysql_manager_connect;
  mysql_manager_close:  Tmysql_manager_close;
  mysql_manager_command: Tmysql_manager_command;
  mysql_manager_fetch_line: Tmysql_manager_fetch_line;
  mysql_num_rows:       Tmysql_num_rows;
  mysql_num_fields:     Tmysql_num_fields;
  mysql_eof:            Tmysql_eof;
  mysql_fetch_field_direct: Tmysql_fetch_field_direct;
  mysql_fetch_fields:   Tmysql_fetch_fields;
  mysql_row_tell:       Tmysql_row_tell;
  mysql_field_tell:     Tmysql_field_tell;
  mysql_field_count:    Tmysql_field_count;
  mysql_affected_rows:   Tmysql_affected_rows;
  mysql_insert_id:      Tmysql_insert_id;
  mysql_errno:          Tmysql_errno;
  mysql_error:          Tmysql_error;
  mysql_sqlstate:       Tmysql_sqlstate;
  mysql_warning_count:  Tmysql_warning_count;
  mysql_info:           Tmysql_info;
  mysql_thread_id:      Tmysql_thread_id;
  mysql_character_set_name: Tmysql_character_set_name;
  mysql_set_server_option: Tmysql_set_server_option;
  mysql_get_client_version: Tmysql_get_client_version;
  mysql_get_server_version: Tmysql_get_server_version;

var
  LibraryLoader: TZNativeLibraryLoader;

implementation

type
  {** Implements a loader for MySQL native library. }
  TZMySQLNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
  end;

{ TZMySQLNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZMySQLNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

  @mysql_server_init     := GetAddress('mysql_server_init');
  @mysql_server_end      := GetAddress('mysql_server_end');
  @mysql_thread_init     := GetAddress('mysql_thread_init');
  @mysql_thread_end      := GetAddress('mysql_thread_end');
  @mysql_num_rows        := GetAddress('mysql_num_rows');
  @mysql_num_fields      := GetAddress('mysql_num_fields');
  @mysql_eof             := GetAddress('mysql_eof');
  @mysql_fetch_field_direct := GetAddress('mysql_fetch_field_direct');
  @mysql_fetch_fields    := GetAddress('mysql_fetch_fields');
  @mysql_row_tell        := GetAddress('mysql_row_tell');
  @mysql_field_tell      := GetAddress('mysql_field_tell');
  @mysql_field_count     := GetAddress('mysql_field_count');
  @mysql_affected_rows   := GetAddress('mysql_affected_rows');
  @mysql_insert_id       := GetAddress('mysql_insert_id');
  @mysql_errno           := GetAddress('mysql_errno');
  @mysql_error           := GetAddress('mysql_error');
  @mysql_sqlstate        := GetAddress('mysql_sqlstate');
  @mysql_warning_count   := GetAddress('mysql_warning_count');
  @mysql_info            := GetAddress('mysql_info');
  @mysql_thread_id       := GetAddress('mysql_thread_id');
  @mysql_character_set_name := GetAddress('mysql_character_set_name');
  @mysql_init            := GetAddress('mysql_init');
  @mysql_ssl_set         := GetAddress('mysql_ssl_set');
  @mysql_change_user     := GetAddress('mysql_change_user');
  @mysql_real_connect    := GetAddress('mysql_real_connect');
  @mysql_select_db       := GetAddress('mysql_select_db');
  @mysql_query           := GetAddress('mysql_query');
  @mysql_send_query      := GetAddress('mysql_send_query');
  @mysql_real_query      := GetAddress('mysql_real_query');
  @mysql_store_result    := GetAddress('mysql_store_result');
  @mysql_use_result      := GetAddress('mysql_use_result');
  @mysql_master_query    := GetAddress('mysql_master_query');
  @mysql_master_send_query := GetAddress('mysql_master_send_query');
  @mysql_slave_query     := GetAddress('mysql_slave_query');
  @mysql_slave_send_query := GetAddress('mysql_slave_send_query');
  @mysql_enable_rpl_parse := GetAddress('mysql_enable_rpl_parse');
  @mysql_disable_rpl_parse := GetAddress('mysql_disable_rpl_parse');
  @mysql_rpl_parse_enabled := GetAddress('mysql_rpl_parse_enabled');
  @mysql_enable_reads_from_master :=
    GetAddress('mysql_enable_reads_from_master');
  @mysql_disable_reads_from_master :=
    GetAddress('mysql_disable_reads_from_master');
  @mysql_reads_from_master_enabled :=
    GetAddress('mysql_reads_from_master_enabled');
  @mysql_rpl_query_type  := GetAddress('mysql_rpl_query_type');
  @mysql_shutdown        := GetAddress('mysql_shutdown');
  @mysql_dump_debug_info := GetAddress('mysql_dump_debug_info');
  @mysql_refresh         := GetAddress('mysql_refresh');
  @mysql_kill            := GetAddress('mysql_kill');
  @mysql_set_server_option := GetAddress('mysql_set_server_option');
  @mysql_ping            := GetAddress('mysql_ping');
  @mysql_stat            := GetAddress('mysql_stat');
  @mysql_get_server_info := GetAddress('mysql_get_server_info');
  @mysql_get_client_info := GetAddress('mysql_get_client_info');
  @mysql_get_client_version := GetAddress('mysql_get_client_version');
  @mysql_get_host_info   := GetAddress('mysql_get_host_info');
  @mysql_get_server_version := GetAddress('mysql_get_server_version');
  @mysql_get_proto_info  := GetAddress('mysql_get_proto_info');
  @mysql_list_fields     := GetAddress('mysql_list_fields');
  @mysql_list_processes  := GetAddress('mysql_list_processes');
  @mysql_list_dbs        := GetAddress('mysql_list_dbs');
  @mysql_list_tables     := GetAddress('mysql_list_tables');
  @mysql_options         := GetAddress('mysql_options');
  @mysql_free_result     := GetAddress('mysql_free_result');
  @mysql_data_seek       := GetAddress('mysql_data_seek');
  @mysql_row_seek        := GetAddress('mysql_row_seek');
  @mysql_field_seek      := GetAddress('mysql_field_seek');
  @mysql_fetch_row       := GetAddress('mysql_fetch_row');
  @mysql_fetch_lengths   := GetAddress('mysql_fetch_lengths');
  @mysql_fetch_field     := GetAddress('mysql_fetch_field');
  @mysql_debug           := GetAddress('mysql_debug');
  @mysql_connect         := GetAddress('mysql_connect');
  @mysql_close           := GetAddress('mysql_close');
  @mysql_create_db       := GetAddress('mysql_create_db');
  @mysql_drop_db         := GetAddress('mysql_drop_db');
  @mysql_escape_string   := GetAddress('mysql_escape_string');
  @mysql_read_query_result := GetAddress('mysql_read_query_result');
  @mysql_rpl_probe       := GetAddress('mysql_rpl_probe');
  @mysql_set_master      := GetAddress('mysql_set_master');
  @mysql_add_slave       := GetAddress('mysql_add_slave');
  @mysql_real_escape_string := GetAddress('mysql_real_escape_string');
  @mysql_thread_safe     := GetAddress('mysql_thread_safe');
  @mysql_manager_init    := GetAddress('mysql_manager_init');
  @mysql_manager_connect := GetAddress('mysql_manager_connect');
  @mysql_manager_close   := GetAddress('mysql_manager_close');
  @mysql_manager_command := GetAddress('mysql_manager_command');
  @mysql_manager_fetch_line := GetAddress('mysql_manager_fetch_line');
end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [WINDOWS1_DLL_LOCATION
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
    , WINDOWS2_DLL_LOCATION
{$ENDIF}
    ]);
{$ELSE}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
end.

