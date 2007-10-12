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

unit ZPlainMysqlConstants;

interface

{$I ZPlain.inc}

uses 
   ZCompatibility; 
 
const
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
  WINDOWS2_DLL_LOCATION = 'libmysql.dll';
  WINDOWS2_DLL_LOCATION_EMBEDDED = 'libmysqld.dll';
{$ENDIF}
  LINUX2_DLL_LOCATION = 'libmysqlclient.so';
  LINUX_DLL_LOCATION_EMBEDDED = 'libmysqld.so';

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
  GROUP_FLAG	         = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }
  BINCMP_FLAG            = $20000; { Intern: Used by sql_yacc }

{ Server Administration Refresh Options }
  REFRESH_GRANT	           = 1;     { Refresh grant tables }
  REFRESH_LOG		       = 2;     { Start on new log file }
  REFRESH_TABLES	       = 4;     { close all tables }
  REFRESH_HOSTS	           = 8;     { Flush host cache }
  REFRESH_STATUS           = 16;    { Flush status variables }
  REFRESH_THREADS          = 32;    { Flush status variables }
  REFRESH_SLAVE            = 64;    { Reset master info abd restat slave thread }
  REFRESH_MASTER           = 128;   { Remove all bin logs in the index and truncate the index }
  REFRESH_READ_LOCK        = 16384; { Lock tables for read }
  REFRESH_FAST		       = 32768; { Intern flag }
  REFRESH_QUERY_CACHE	   = 65536;
  REFRESH_QUERY_CACHE_FREE = $20000; { Pack query cache }
  REFRESH_DES_KEY_FILE	   = $40000;
  REFRESH_USER_RESOURCES   = $80000;

{ Client Connection Options }
  _CLIENT_LONG_PASSWORD	    = 1;	 { new more secure passwords }
  _CLIENT_FOUND_ROWS	    = 2;	 { Found instead of affected rows }
  _CLIENT_LONG_FLAG	        = 4;	 { Get all column flags }
  _CLIENT_CONNECT_WITH_DB   = 8;	 { One can specify db on connect }
  _CLIENT_NO_SCHEMA	        = 16;	 { Don't allow database.table.column }
  _CLIENT_COMPRESS	        = 32;	 { Can use compression protcol }
  _CLIENT_ODBC		        = 64;    { Odbc client }
  _CLIENT_LOCAL_FILES	    = 128;   { Can use LOAD DATA LOCAL }
  _CLIENT_IGNORE_SPACE	    = 256;   { Ignore spaces before '(' }
  _CLIENT_PROTOCOL_41	    = 512;   { New 4.1 protocol }
  _CLIENT_INTERACTIVE	    = 1024;  { This is an interactive client }
  _CLIENT_SSL               = 2048;  { Switch to SSL after handshake }
  _CLIENT_IGNORE_SIGPIPE    = $1000;    { IGNORE sigpipes }
  _CLIENT_TRANSACTIONS	    = $2000;    { Client knows about transactions }
  _CLIENT_RESERVED          = $4000;    { Old flag for 4.1 protocol  }
  _CLIENT_SECURE_CONNECTION = $8000;    { New 4.1 authentication }
  _CLIENT_MULTI_STATEMENTS  = $10000;   { Enable/disable multi-stmt support }
  _CLIENT_MULTI_RESULTS     = $20000;   { Enable/disable multi-results }
  _CLIENT_REMEMBER_OPTIONS  = $8000000; {Enable/disable multi-results }

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

type
    PZMySQLConnect = Pointer;
    PZMySQLResult = Pointer;
    PZMySQLRow = Pointer;
    PZMySQLField = Pointer;
    PZMySQLRowOffset = Pointer;
    PZMySqlPrepStmt = Pointer;
    PZMysqlBindArray = Pointer;

{    TMySqlServerCommand = (
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
    COM_STMT_FETCH,
    COM_END
  );
}
{ Enum Field Types }
    TMysqlFieldTypes = (
        MYSQL_TYPE_DECIMAL,
        MYSQL_TYPE_TINY,         {BIND}
        MYSQL_TYPE_SHORT,        {BIND}
        MYSQL_TYPE_LONG,         {BIND}
        MYSQL_TYPE_FLOAT,        {BIND}
        MYSQL_TYPE_DOUBLE,       {BIND}
        MYSQL_TYPE_NULL,
        MYSQL_TYPE_TIMESTAMP,    {BIND}
        MYSQL_TYPE_LONGLONG,     {BIND}
        MYSQL_TYPE_INT24,
        MYSQL_TYPE_DATE,         {BIND}
        MYSQL_TYPE_TIME,         {BIND}
        MYSQL_TYPE_DATETIME,     {BIND}
        MYSQL_TYPE_YEAR,
        MYSQL_TYPE_NEWDATE,
        MYSQL_TYPE_VARCHAR, //<--ADDED by fduenas 20-06-2006
        MYSQL_TYPE_BIT,     //<--ADDED by fduenas 20-06-2006
        MYSQL_TYPE_NEWDECIMAL, 
        MYSQL_TYPE_ENUM,
        MYSQL_TYPE_SET,
        MYSQL_TYPE_TINY_BLOB,    {BIND}
        MYSQL_TYPE_MEDIUM_BLOB,  {BIND}
        MYSQL_TYPE_LONG_BLOB,    {BIND}
        MYSQL_TYPE_BLOB,         {BIND}
        MYSQL_TYPE_VAR_STRING,   {BIND}
        MYSQL_TYPE_STRING,       {BIND}
        MYSQL_TYPE_GEOMETRY
    );

  TMysqlSetOption = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF
  );

  TMysqlStmtAttrType = (
    STMT_ATTR_UPDATE_MAX_LENGTH,
    STMT_ATTR_CURSOR_TYPE,
    STMT_ATTR_PREFETCH_ROWS
  );

  TMysqlShutdownLevel = (
    SHUTDOWN_DEFAULT{$IFNDEF VER130} = 0{$ENDIF},
    SHUTDOWN_WAIT_CONNECTIONS{$IFNDEF VER130} = MYSQL_SHUTDOWN_KILLABLE_CONNECT{$ENDIF},
    SHUTDOWN_WAIT_TRANSACTIONS{$IFNDEF VER130} = MYSQL_SHUTDOWN_KILLABLE_TRANS{$ENDIF},
    SHUTDOWN_WAIT_UPDATES{$IFNDEF VER130} = MYSQL_SHUTDOWN_KILLABLE_UPDATE{$ENDIF},
    SHUTDOWN_WAIT_ALL_BUFFERS{$IFNDEF VER130} = (MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1){$ENDIF},
    SHUTDOWN_WAIT_CRITICAL_BUFFERS,
    KILL_QUERY{$IFNDEF VER130} = 254{$ENDIF},
    KILL_CONNECTION{$IFNDEF VER130} = 255{$ENDIF}
  );

{  TMySqlOption = (
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
    MYSQL_OPT_RECONNECT
  );

  TMySqlStatus = (
    MYSQL_STATUS_READY,
    MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

  TMySqlRplType = (
    MYSQL_RPL_MASTER,
    MYSQL_RPL_SLAVE,
    MYSQL_RPL_ADMIN
  );
}
  TMySqlProtocolType = (
    MYSQL_PROTOCOL_DEFAULT,
    MYSQL_PROTOCOL_TCP,
    MYSQL_PROTOCOL_SOCKET,
    MYSQL_PROTOCOL_PIPE,
    MYSQL_PROTOCOL_MEMORY
  );

  TMysqlStmtState = (
    MYSQL_STMT_INIT_DONE{$IFNDEF VER130} = 1{$ENDIF},
    MYSQL_STMT_PREPARE_DONE,
    MYSQL_STMT_EXECUTE_DONE,
    MYSQL_STMT_FETCH_DONE
  );

  MYSQL_TIME = record
    year:                Cardinal;
    month:               Cardinal;
    day:                 Cardinal;
    hour:                Cardinal;
    minute:              Cardinal;
    second:              Cardinal;
    neg:                 Byte;
    second_part:         Int64;
  end;

{  PUSED_MEM=^USED_MEM;
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

  PMYSQL_OPTIONS = ^ST_MYSQL_OPTIONS;
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
    methods_to_use:           TMySqlOption;
    client_ip:                PChar;
    secure_auth:              Byte;
    local_infile_init:        Pointer;
    local_infile_read:        Pointer;
    local_infile_end:         Pointer;
    local_infile_error:       Pointer;
    local_infile_userdata:    Pointer;
  end;
}
  PLIST = ^LIST;
  LIST = record
    prev:       PLIST;
    next:       PLIST;
    data:       Pointer;
  end;

  MYSQL_FIELD_OFFSET = Cardinal;

//  PMYSQL_BIND = ^MYSQL_BIND;
//  MYSQL_BIND =  record
//    length:            {$IFNDEF VER130}PLongWord{$ELSE}^Cardinal{$ENDIF};
//    is_null:           {$IFNDEF VER130}PByte{$ELSE}^Byte{$ENDIF};
//    buffer:            PChar;
//    buffer_type:       TMysqlFieldTypes;
//    buffer_length:     LongWord;

    {all but is_unsigned is used internally by mysql server}
//    inter_buffer:      {$IFNDEF VER130}PByte{$ELSE}^Byte{$ENDIF};
//    offset:            LongWord;
//    internal_length:   LongWord;
//    param_number:      Cardinal;
//    pack_length:       Cardinal;
//    is_unsigned:       Byte;
//    long_data_used:    Byte;
//    internal_is_null:  Byte;
//    store_param_funct: Pointer;  {procedure: (NET *net, struct st_mysql_bind *param)}
//    fetch_result:      Pointer;  {prcoedure: (struct st_mysql_bind *, unsigned char **row)}
//    skip_result:       Pointer;  {(struct st_mysql_bind *, MYSQL_FIELD *, unsigned char **row)}
//  end;

  PMYSQL_BIND2 = ^MYSQL_BIND2;
  MYSQL_BIND2 =  record
    length:            PLongInt; 
    is_null:           PByte; 
    buffer:            PChar;
    error:             PByte; 
    buffer_type:       TMysqlFieldTypes;
    buffer_length:     LongInt;
    row_ptr:           PByte; 
    offset:            LongInt;
    length_value:      LongInt;
    param_number:      Cardinal;
    pack_length:       Cardinal;
    error_value:       Byte;
    is_unsigned:       Byte;
    long_data_used:    Byte;
    is_null_value:     Byte;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
  end;

const
  EMBEDDED_DEFAULT_DATA_DIR = {$IFDEF WIN32}
                               '.\data\'
                              {$ELSE} './data/'
                              {$ENDIF};
  SERVER_ARGUMENTS_KEY_PREFIX = 'ServerArgument';
  SERVER_GROUPS : array [0..2] of PChar = ('embedded'#0, 'server'#0, nil);

  DEFAULT_PARAMS : array [0..2] of PChar = ('not_used'#0,
                                            '--datadir='+EMBEDDED_DEFAULT_DATA_DIR+#0,
                                            '--set-variable=key_buffer_size=32M'#0);

implementation


end.
