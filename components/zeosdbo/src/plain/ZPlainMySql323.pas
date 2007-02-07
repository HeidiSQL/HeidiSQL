{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to libmysql.dll           }
{                     Version 3.23                        }
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

unit ZPlainMySql323;

interface

{$I ZPlain.inc}
{$DEFINE MYSQL_323_API}

{$J+}

uses Classes, ZPlainLoader, ZCompatibility, ZPlainMySqlConstants;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS1_DLL_LOCATION = 'libmysql323.dll';
  LINUX1_DLL_LOCATION = 'libmysqlclient.so.10';

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
    MYSQL_SET_CHARSET_NAME
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
    error_handler: PERR_PROC;
  end;

  NET = packed record
    hPipe:         Pointer;
    fd:            Integer;
    fcntl:         Integer;
    buff:          PChar;
    buff_end:      PChar;
    write_pos:     PChar;
    read_pos:      PChar;
    last_error:    array[01..MYSQL_ERRMSG_SIZE] of Char;
    last_errno:    Integer;
    max_packet:    Integer;
    timeout:       Integer;
    pkt_nr:        Integer;
    error:         Char;
    return_errno:  Byte;
    compress:      Byte;
    no_send_ok:    Byte;
    remain_in_buf: LongInt;
    length:        LongInt;
    buf_length:    LongInt;
    where_b:       LongInt;
    return_status: Pointer;
    reading_or_writing: Char;
    save_char:     Char;
  end;

  PMYSQL_FIELD = ^MYSQL_FIELD;
  MYSQL_FIELD = record
    name:       PChar;
    table:      PChar;
    def:        PChar;
    _type:      Byte;
    length:     Integer;
    max_length: Integer;
    flags:      Integer;
    decimals:   Integer;
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
    connect_timeout: Integer;
    clientFlag:      Integer;
    compress:        Byte;
    named_pipe:      Byte;
    port:            Integer;
    host:            PChar;
    init_command:    PChar;
    user:            PChar;
    password:        PChar;
    unix_socket:     PChar;
    db:              PChar;
    my_cnf_file:     PChar;
    my_cnf_group:    PChar;
    charset_dir:     PChar;
    charset_name:    PChar;
    use_ssl:         Byte;
    ssl_key:         PChar;
    ssl_cert:        PChar;
    ssl_ca:          PChar;
    ssl_capath:      PChar;
  end;

  PMYSQL = ^MYSQL;
  MYSQL = record
    _net:            NET;
    connector_fd:    PChar;
    host:            PChar;
    user:            PChar;
    passwd:          PChar;
    unix_socket:     PChar;
    server_version:  PChar;
    host_info:       PChar;
    info:            PChar;
    db:              PChar;
    port:            Integer;
    client_flag:     Integer;
    server_capabilities: Integer;
    protocol_version: Integer;
    field_count:     Integer;
    server_status:   Integer;
    thread_id:       LongInt;
    affected_rows:   Int64;
    insert_id:       Int64;
    extra_info:      Int64;
    packet_length:   LongInt;
    status:          Tmysqlstatus;
    fields:          PMYSQL_FIELD;
    field_alloc:     MEM_ROOT;
    free_me, reconnect: Byte;
    options:         _mysql_options;
    scramble_buff:   array[0..8] of Char;
    charset:         PChar;
    server_language: Integer;
  end;

  MYSQL_RES = packed record
    row_count:       Int64;
    field_count:     Integer;
    current_field:   Integer;
    fields:          PMYSQL_FIELD;
    data:            PMYSQL_DATA;
    data_cursor:     PMYSQL_ROWS;
    field_alloc:     MEM_ROOT;
    row:             PMYSQL_ROW;
    current_row:     PMYSQL_ROW;
    lengths:         PLongInt;
    handle:          PMYSQL;
    eof:             Byte;
  end;
  PMYSQL_RES = ^MYSQL_RES;

  TModifyType = (MODIFY_INSERT, MODIFY_UPDATE, MODIFY_DELETE);
  TQuoteOptions = (QUOTE_STRIP_CR,QUOTE_STRIP_LF);
  TQuoteOptionsSet = set of TQuoteOptions;

{ ************** Plain API Function types definition ************* }
{$DEFINE LOAD_MYSQL_API_FUNC}
{$I ZPlainMysql.inc}
{$UNDEF LOAD_MYSQL_API_FUNC}

{ ************** Collection of Plain API Function types definition ************* }
MYSQL323_API = record
{$DEFINE MYSQL_API_VAR}
{$I ZPlainMysql.inc}
{$UNDEF MYSQL_API_VAR}
END;

type
  {** Implements a loader for MySQL native library. }
  TZMySQLNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    api_rec : MYSQL323_API;
    destructor Destroy; override;
    function Load: Boolean; override;
  end;

var

  LibraryLoader: TZMySQLNativeLibraryLoader;

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
{$ELSE}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [LINUX1_DLL_LOCATION,LINUX2_DLL_LOCATION]);
{$ENDIF}
{$UNDEF MYSQL_323_API}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
end.
