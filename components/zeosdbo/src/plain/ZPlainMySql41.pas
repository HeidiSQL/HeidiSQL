{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to libmysql.dll           }
{                     Version 4.1                         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{                                                         }
{    Copyright (c) 2006 John Marino, www.synsport.com     }
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

unit ZPlainMySql41;

interface

{$I ZPlain.inc}
{$DEFINE MYSQL_41_API}

{$J+}
{$Z4}
{$IFNDEF VER130BELOW}
{$A8}
{$ENDIF}

uses Classes, ZPlainLoader, ZCompatibility, ZPlainMySqlConstants;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS1_DLL_LOCATION = 'libmysql41.dll';
  WINDOWS1_DLL_LOCATION_EMBEDDED = 'libmysqld41.dll';
  LINUX1_DLL_LOCATION = 'libmysqlclient.so.14';

{ General Declarations }
//  PROTOCOL_VERSION     = 10;
//  FRM_VER              = 6;

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
  FIELD_TYPE_VARCHAR   = 15; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_BIT       = 16; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_NEWDECIMAL = 246; //<--ADDED by fduenas 20-06-2006
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
{$IFDEF ENABLE_MYSQL_DEPRECATED}
 // This Structure Changes from Time to Time, so avoid using it !
 // Used by old GetStatus ,function necessary for mysql3.20
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
{$ELSE}
  MYSQL  = pointer;
{$ENDIF ENABLE_MYSQL_DEPRECATED}

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

{ ************** Plain API Function types definition ************* }
{$DEFINE LOAD_MYSQL_API_FUNC}
{$I ZPlainMysql.inc}
{$UNDEF LOAD_MYSQL_API_FUNC}

{ ************** Collection of Plain API Function types definition ************* }
MYSQL41_API = record
{$DEFINE MYSQL_API_VAR}
{$I ZPlainMysql.inc}
{$UNDEF MYSQL_API_VAR}
END;

type
  {** Implements a loader for MySQL native library. }
  TZMySQLNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    api_rec : MYSQL41_API;
    destructor Destroy; override;
    function Load: Boolean; override;
  end;

var

  LibraryLoader: TZMySQLNativeLibraryLoader;
  LibraryLoaderEmbedded: TZMySQLNativeLibraryLoader;

implementation

{ TZMySQLNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZMySQLNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

{ ************** Load adresses of API Functions ************* }
{$DEFINE LOAD_MYSQL_API_REC}
{$I ZPlainMysql.inc}
{$UNDEF LOAD_MYSQL_API_REC}
end;

{**
  Destroys the library and cleanups the memory.
}
destructor TZMySQLNativeLibraryLoader.Destroy;
begin
  if (Loaded) and (@api_rec.mysql_server_end <> nil) then
    api_rec.mysql_server_end;
  inherited Destroy;
end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [
{$IFDEF MYSQL_LOAD_STRICT_DLL_LAST}
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
    WINDOWS2_DLL_LOCATION,
{$ENDIF}
{$ENDIF}
    WINDOWS1_DLL_LOCATION
{$IFNDEF MYSQL_LOAD_STRICT_DLL_LAST}
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
    , WINDOWS2_DLL_LOCATION
{$ENDIF}
{$ENDIF}
    ]);
  LibraryLoaderEmbedded := TZMySQLNativeLibraryLoader.Create(
    [WINDOWS1_DLL_LOCATION_EMBEDDED
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
    , WINDOWS2_DLL_LOCATION_EMBEDDED
{$ENDIF}
    ]);
{$ELSE}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [LINUX1_DLL_LOCATION,LINUX2_DLL_LOCATION]);
  LibraryLoaderEmbedded := TZMySQLNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION_EMBEDDED]);
{$ENDIF}
{$UNDEF MYSQL_41_API}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
  if Assigned(LibraryLoaderEmbedded) then
    LibraryLoaderEmbedded.Free;
end.
