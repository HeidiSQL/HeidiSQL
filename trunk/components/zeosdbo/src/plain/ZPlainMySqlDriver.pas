{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for MySQL              }
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

unit ZPlainMySqlDriver;

interface

{$I ZPlain.inc}

uses Classes, ZClasses, ZPlainDriver, ZCompatibility, ZPlainMysqlConstants,
     {$IFDEF ENABLE_MYSQL_DEPRECATED} ZPlainMySql320, ZPlainMySql323, ZPlainMySql40,{$ENDIF}
     ZPlainMySql41, ZPlainMySql5;

const

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
  _CLIENT_CHANGE_USER     = 512;  { Support the mysql_change_user() }
  _CLIENT_INTERACTIVE     = 1024; { This is an interactive client }
  _CLIENT_SSL             = 2048; { Switch to SSL after handshake }
  _CLIENT_IGNORE_SIGPIPE  = 4096; { IGNORE sigpipes }
  _CLIENT_TRANSACTIONS    = 8196; { Client knows about transactions }
  _CLIENT_RESERVED        = 16384; { Old flag for 4.1 protocol  }
  _CLIENT_SECURE_CONNECTION = 32768; { New 4.1 authentication }
  _CLIENT_MULTI_STATEMENTS = 65536; { Enable/disable multi-stmt support }
  _CLIENT_MULTI_RESULTS   = 131072; { Enable/disable multi-results }
  _CLIENT_REMEMBER_OPTIONS = 2147483648; {Enable/disable multi-results }

type
  PZMySQLConnect = Pointer;
  PZMySQLResult = Pointer;
  PZMySQLRow = Pointer;
  PZMySQLField = Pointer;
  PZMySQLRowOffset = Pointer;

  TZMySQLOption = (
    MYSQL_OPT_CONNECT_TIMEOUT,
    MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE,
    MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR,
    MYSQL_SET_CHARSET_NAME
  );

  TZMySQLStatus = (
    MYSQL_STATUS_READY,
    MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

  {** Represents a generic interface to MySQL native API. }
  IZMySQLPlainDriver = interface (IZPlainDriver)
    ['{D1CB3F6C-72A1-4125-873F-791202ACC5F0}']
    function GetDescription: string;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    procedure Despose(var Handle: PZMySQLConnect);

    function GetAffectedRows(Handle: PZMySQLConnect): Int64;
    // char_set_name
    procedure SetCharacterSet(Handle: PZMySQLConnect; const CharSet: PChar);
    procedure Close(Handle: PZMySQLConnect);
    function Connect(Handle: PZMySQLConnect; const Host, User, Password: PChar): PZMySQLConnect;
    function CreateDatabase(Handle: PZMySQLConnect; const Database: PChar): Integer;
    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    procedure Debug(Debug: PChar);
    function DropDatabase(Handle: PZMySQLConnect; const Database: PChar): Integer;
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    // eof
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;
    function FetchField(Res: PZMySQLResult): PZMySQLField;
    // fetch_field_direct
    // fetch_fields
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    // field_tell
    procedure FreeResult(Res: PZMySQLResult);
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    // info
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function GetListDatabases(Handle: PZMySQLConnect; Wild: PChar): PZMySQLResult;
    function GetListFields(Handle: PZMySQLConnect; const Table, Wild: PChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect; const Wild: PChar): PZMySQLResult;
    // num_fields
    function GetNumRows(Res: PZMySQLResult): Int64;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption; const Arg: PChar): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;
    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function RealConnect(Handle: PZMySQLConnect; const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    // real_escape_string
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar; Length: Integer): Integer;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // row_tell
    function SelectDatabase(Handle: PZMySQLConnect; const Database: PChar): Integer;
    //ssl_set
    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;

    // my_init
    // thread_init
    // thread_end
    // thread_safe

    // server_init
    // server_end

    // change_user
    // field_count
    // function GetClientVersion: AnsiString; 

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!

    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    //function GetServerVersion (Handle: PZMySQLConnect): AnsiString;
    // hex_string
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    // set_character_set
    // set_server_option
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;
    // warning_count

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    // stmt_attr_get
    // stmt_attr_set
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    // stmt_data_seek
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    // stmt_fetch_column
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    // stmt_free_result
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    // stmt_param_metadata
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
    // stmt_reset
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    // stmt_row_seek
    // stmt_row_tell
    // stmt_send_long_data
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    // get_character_set_info

    {non API functions}
    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
{$IFDEF ENABLE_MYSQL_DEPRECATED}
    function GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
{$ENDIF ENABLE_MYSQL_DEPRECATED}
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
    procedure BuildArguments(Options: TStrings);

{
    function GetProtocol: string; virtual;
    function GetDescription: string; virtual;
    procedure Initialize; virtual;

}
  end;

{$IFDEF ENABLE_MYSQL_DEPRECATED}

  {** Implements a driver for MySQL 3.20 }

  TZMySQL320PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZMySQLPlainDriver)
  protected
    MYSQL_API : ZPlainMysql320.mysql320_API;
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PChar; Port: Cardinal;
      UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;

    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PChar): Integer;
    function GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;

    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(Handle: PZMySQLConnect;
      Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;
      const Wild: PChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(Handle: PZMySQLConnect;
      const Table, Wild: PChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;

    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
    procedure BuildArguments(Options: TStrings);
  end;

  {** Implements a driver for MySQL 3.23 }
  TZMySQL323PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZMySQLPlainDriver)
  protected
    MYSQL_API : ZPlainMysql323.mysql323_API;
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PChar; Port: Cardinal;
      UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PChar): Integer;
    function GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;

    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(Handle: PZMySQLConnect;
      Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;
      const Wild: PChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(Handle: PZMySQLConnect;
      const Table, Wild: PChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;

    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
    procedure BuildArguments(Options: TStrings);
  end;

  {** Implements a driver for MySQL 4.0 }
  TZMySQL40PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZMySQLPlainDriver)
  protected
    MYSQL_API : ZPlainMysql40.mysql40_API;
  public
    constructor Create;

    function GetProtocol: string; virtual;
    function GetDescription: string; virtual;
    procedure Initialize; virtual;

    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; virtual;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PChar; Port: Cardinal;
      UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PChar): Integer;
    function GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;

    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(Handle: PZMySQLConnect;
      Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;
      const Wild: PChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(Handle: PZMySQLConnect;
      const Table, Wild: PChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;

    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
    procedure BuildArguments(Options: TStrings); virtual;
  end;

  TZMySQLD40PlainDriver = class (TZMySQL40PlainDriver)
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure Initialize; override;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; override;
    procedure BuildArguments(Options: TStrings); override;
  end;
{$ENDIF ENABLE_MYSQL_DEPRECATED}

  {** Implements a driver for MySQL 4.1 }
  TZMySQL41PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZMySQLPlainDriver)
  protected
    MYSQL_API : ZPlainMysql41.mysql41_API;
  public
    constructor Create;

    function GetProtocol: string; virtual;
    function GetDescription: string; virtual;
    procedure Initialize; virtual;

    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; virtual;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PChar; Port: Cardinal;
      UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PChar): Integer;
    function GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;

    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(Handle: PZMySQLConnect;
      Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;
      const Wild: PChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(Handle: PZMySQLConnect;
      const Table, Wild: PChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;

    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    procedure SetCharacterSet(Handle: PZMySQLConnect; const CharSet: PChar);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
{$IFDEF ENABLE_MYSQL_DEPRECATED}
    function GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
{$ENDIF ENABLE_MYSQL_DEPRECATED}
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
    procedure BuildArguments(Options: TStrings); virtual;
  end;

  {** Implements a driver for MySQL 4.1 }
  TZMySQLD41PlainDriver = class (TZMySQL41PlainDriver)
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure Initialize; override;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; override;
    procedure BuildArguments(Options: TStrings); override;
  end;

  TZMySQL5PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZMySQLPlainDriver)
  protected
    MYSQL_API : ZPlainMysql5.mysql5_API;
  public
    constructor Create;

    function GetProtocol: string; virtual;
    function GetDescription: string; virtual;
    procedure Initialize; virtual; 

    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; virtual;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PChar; Port: Cardinal;
      UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    procedure SetCharacterSet(Handle: PZMySQLConnect; const CharSet: PChar);
    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PChar): Integer;
    function GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;

    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(Handle: PZMySQLConnect;
      Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;
      const Wild: PChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(Handle: PZMySQLConnect;
      const Table, Wild: PChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;

    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
{$IFDEF ENABLE_MYSQL_DEPRECATED}
    function GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
{$ENDIF ENABLE_MYSQL_DEPRECATED}
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
    procedure BuildArguments(Options: TStrings); virtual;
  end;

  TZMySQLD5PlainDriver = class (TZMySQL5PlainDriver)
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure Initialize; override;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; override;
    procedure BuildArguments(Options: TStrings); override;
  end;

implementation
uses SysUtils, ZMessages;

var
  ServerArgs: array of PChar;
  ServerArgsLen: Integer;

procedure BuildServerArguments(Options: TStrings);
var
  TmpList: TStringList;
  i: Integer;
begin
  TmpList := TStringList.Create;
  try
    TmpList.Add(ParamStr(0));
    for i := 0 to Options.Count - 1 do
      if SameText(SERVER_ARGUMENTS_KEY_PREFIX,
                  Copy(Options.Names[i], 1,
                       Length(SERVER_ARGUMENTS_KEY_PREFIX))) then
{$IFDEF VER140BELOW} 
        TmpList.Add(Options.Values[Options.Names[i]]); 
{$ELSE} 
        TmpList.Add(Options.ValueFromIndex[i]); 
{$ENDIF}
    //Check if DataDir is specified, if not, then add it to the Arguments List
    If TmpList.Values['--datadir'] = '' then
       TmpList.Add('--datadir='+EMBEDDED_DEFAULT_DATA_DIR);
    ServerArgsLen := TmpList.Count;
    SetLength(ServerArgs, ServerArgsLen);
    for i := 0 to ServerArgsLen - 1 do
      ServerArgs[i] := StrNew(PChar(TmpList[i]));
  finally
    TmpList.Free;
  end;
end;

{$IFDEF ENABLE_MYSQL_DEPRECATED}
{ TZMySQL320PlainDriver }

constructor TZMySQL320PlainDriver.Create;
begin
end;

function TZMySQL320PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-3.20';
end;

function TZMySQL320PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 3.20+';
end;

procedure TZMySQL320PlainDriver.Initialize;
begin
  ZPlainMySql320.LibraryLoader.LoadIfNeeded;
  MYSQL_API := ZPlainMySql320.LibraryLoader.api_rec;
end;

procedure TZMySQL320PlainDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQL320PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL320PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQL320PlainDriver.Debug(Debug: PChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQL320PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQL320PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQL320PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQL320PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL320PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQL320PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQL320PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQL320PlainDriver.FreeResult(Res: PZMySQLResult);
begin
 MYSQL_API.mysql_free_result(Res);
end;

function TZMySQL320PlainDriver.GetAffectedRows(
  Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQL320PlainDriver.GetClientInfo: PChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQL320PlainDriver.GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQL320PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQL320PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL320PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL320PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQL320PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQL320PlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
Begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
End;

function TZMySQL320PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQL320PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQL320PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQL320PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQL320PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(1, nil, nil);
  Handle := AllocMem(SizeOf(ZPlainMySql320.MYSQL));
  Result := MYSQL_API.mysql_init(Handle);
end;

function TZMySQL320PlainDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(ZPlainMySql320.PMYSQL(Handle));
end;

procedure TZMySQL320PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
  if Handle <> nil then
    FreeMem(Handle);
  Handle := nil;
end;

function TZMySQL320PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQL320PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQL320PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL320PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQL320PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQL320PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQL320PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQL320PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQL320PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,
    ZPlainMySql320.Tmysqloption(Option), Arg);
end;

function TZMySQL320PlainDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle);
end;

function TZMySQL320PlainDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    query: AnsiString;
    testResult: Integer;
begin
    if (mode = True) then
        query := 'AUTOCOMMIT=1'
    else
        query := 'AUTOCOMMIT=0';
    testResult := MYSQL_API.mysql_query(ZPlainMySql320.PMYSQL(Handle),pchar(query));
    Result := (testResult = 0);
end;

function TZMySQL320PlainDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    testResult: Integer;
begin
    testResult := MYSQL_API.mysql_query(ZPlainMySql320.PMYSQL(Handle),'COMMIT');
    Result := (testResult = 0);
end;

function TZMySQL320PlainDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_more_results']);
    result := false;
End;

function TZMySQL320PlainDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_next_rowset']);
    result := 1;
End;

function TZMySQL320PlainDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    testResult: Integer;
begin
    testResult := MYSQL_API.mysql_query(ZPlainMySql320.PMYSQL(Handle),'ROLLBACK');
    Result := (testResult = 0);
end;

function TZMySQL320PlainDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_sqlstate']);
    Result := '?????';  {mysql 3.20 doesn't support this}
end;

function TZMySQL320PlainDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_affected_rows']);
    result := 0;
End;

function TZMySQL320PlainDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_bind_parameters']);
    result := 1;
End;

function TZMySQL320PlainDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_bind_result']);
    result := 1;
End;

function TZMySQL320PlainDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_close']);
    result := nil;
end;

function TZMySQL320PlainDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_errno']);
    result :=  0;
End;

function TZMySQL320PlainDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_error'] );
    result := '';
end;

function TZMySQL320PlainDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_execute']);
    result := 1;
End;

function TZMySQL320PlainDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_fetch']);
    result := 1;
End;

function TZMySQL320PlainDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_field_count']);
    result := 0;
End;

function TZMySQL320PlainDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_init']);
    result := nil;
end;

function TZMySQL320PlainDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_insert_id']);
    result := 0;
End;

function TZMySQL320PlainDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_num_rows']);
    result := 0;
End;

function TZMySQL320PlainDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_param_count']);
    result := 0;
End;

function TZMySQL320PlainDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_prepare']);
    Result := 1;
end;

function TZMySQL320PlainDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_result_metadata']);
    result := nil;
End;

function TZMySQL320PlainDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PChar;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_sql_state']);
    result := '?????';
end;

function TZMySQL320PlainDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_store_result']);
    result := 1;
End;


function TZMySQL320PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_store_result(Handle);
end;

function TZMySQL320PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQL320PlainDriver.GetLastError(Handle: PZMySQLConnect): PChar;
begin
  Result := @(ZPlainMySql320.PMYSQL(Handle)^._net.last_error);
end;

function TZMySQL320PlainDriver.GetFieldType(Field: PZMySQLField): Byte;
begin
  Result := ZPlainMySql320.PMYSQL_FIELD(Field)^._type;
end;

function TZMySQL320PlainDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql320.PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQL320PlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := ZPlainMySql320.PMYSQL_RES(Res).row_count;
end;

function TZMySQL320PlainDriver.GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
begin
  Result := TZMySQLStatus(ZPlainMySql320.PMYSQL(Handle).status);
end;

function TZMySQL320PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
  Exception.CreateFmt (SUnsupportedByDriver, ['ResultSetExists']);
  result := False;
end;

function TZMySQL320PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := ZPlainMySql320.PMYSQL_RES(Res).field_count;
end;

function TZMySQL320PlainDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql320.PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQL320PlainDriver.GetFieldLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql320.PMYSQL_FIELD(Field)^.length;
end;

function TZMySQL320PlainDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql320.PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQL320PlainDriver.GetFieldName(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql320.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL320PlainDriver.GetFieldTable(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql320.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL320PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PChar;
begin
  Result := ZPlainMySql320.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL320PlainDriver.GetLastErrorCode(
  Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql320.PMYSQL(Handle)._net.last_errno;
end;

function TZMySQL320PlainDriver.GetClientVersion: Integer;
begin
 Result := 32000;
end;

function TZMySQL320PlainDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := 32000;
end;

procedure TZMySQL320PlainDriver.BuildArguments(Options: TStrings);
begin

end;

{ TZMySQL323PlainDriver }

constructor TZMySQL323PlainDriver.Create;
begin
end;

function TZMySQL323PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-3.23';
end;

function TZMySQL323PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 3.23+';
end;

procedure TZMySQL323PlainDriver.Initialize;
begin
  ZPlainMySql323.LibraryLoader.LoadIfNeeded;
  MYSQL_API := ZPlainMySql323.LibraryLoader.api_rec;
end;

procedure TZMySQL323PlainDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQL323PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL323PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQL323PlainDriver.Debug(Debug: PChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQL323PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQL323PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQL323PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQL323PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL323PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQL323PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQL323PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQL323PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  MYSQL_API.mysql_free_result(Res);
end;

function TZMySQL323PlainDriver.GetAffectedRows(
  Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQL323PlainDriver.GetClientInfo: PChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQL323PlainDriver.GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQL323PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQL323PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL323PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL323PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQL323PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQL323PlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
Begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
End;

function TZMySQL323PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQL323PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQL323PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQL323PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQL323PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(1, nil, nil);
  Handle := AllocMem(SizeOf(ZPlainMySql323.MYSQL));
  Result := MYSQL_API.mysql_init(Handle);
end;

function TZMySQL323PlainDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(ZPlainMySql323.PMYSQL(Handle));
end;

procedure TZMySQL323PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
  if Handle <> nil then
    FreeMem(Handle);
  Handle := nil;
end;

function TZMySQL323PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQL323PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQL323PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL323PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQL323PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQL323PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQL323PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQL323PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQL323PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,
    ZPlainMySql323.Tmysqloption(Option), Arg);
end;

function TZMySQL323PlainDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle);
end;

function TZMySQL323PlainDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    query: AnsiString;
    testResult: Integer;
begin
    if (mode = True) then
        query := 'AUTOCOMMIT=1'
    else
        query := 'AUTOCOMMIT=0';
    testResult := MYSQL_API.mysql_query(ZPlainMySql323.PMYSQL(Handle),pchar(query));
    Result := (testResult = 0);
end;

function TZMySQL323PlainDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    testResult: Integer;
begin
    testResult := MYSQL_API.mysql_query(ZPlainMySql323.PMYSQL(Handle),'COMMIT');
    Result := (testResult = 0);
end;

function TZMySQL323PlainDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_more_results']);
    result := false;
End;

function TZMySQL323PlainDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_next_rowset']);
    result := 1;
End;

function TZMySQL323PlainDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    testResult: Integer;
begin
    testResult := MYSQL_API.mysql_query(ZPlainMySql323.PMYSQL(Handle),'ROLLBACK');
    Result := (testResult = 0);
end;

function TZMySQL323PlainDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_sqlstate']);
    Result := '?????';  {mysql 3.23 doesn't support this}
end;

function TZMySQL323PlainDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_affected_rows']);
    result := 0;
End;

function TZMySQL323PlainDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_bind_parameters']);
    result := 1;
End;

function TZMySQL323PlainDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_bind_result']);
    result := 1;
End;

function TZMySQL323PlainDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_close']);
    result := nil;
end;

function TZMySQL323PlainDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_errno']);
    result :=  0;
End;

function TZMySQL323PlainDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_error'] );
    result := '';
end;

function TZMySQL323PlainDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_execute']);
    result := 1;
End;

function TZMySQL323PlainDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_fetch']);
    result := 1;
End;

function TZMySQL323PlainDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_field_count']);
    result := 0;
End;

function TZMySQL323PlainDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_init']);
    result := nil;
end;

function TZMySQL323PlainDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_insert_id']);
    result := 0;
End;

function TZMySQL323PlainDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_num_rows']);
    result := 0;
End;

function TZMySQL323PlainDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_param_count']);
    result := 0;
End;

function TZMySQL323PlainDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_prepare']);
    Result := 1;
end;

function TZMySQL323PlainDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_result_metadata']);
    result := nil;
End;

function TZMySQL323PlainDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PChar;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_sql_state']);
    result := '?????';
end;

function TZMySQL323PlainDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_store_result']);
    result := 1;
End;

function TZMySQL323PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_store_result(Handle);
end;

function TZMySQL323PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQL323PlainDriver.GetLastError(Handle: PZMySQLConnect): PChar;
begin
  Result := @(ZPlainMySql320.PMYSQL(Handle)^._net.last_error);
end;

function TZMySQL323PlainDriver.GetFieldType(Field: PZMySQLField): Byte;
begin
  Result := ZPlainMySql323.PMYSQL_FIELD(Field)^._type;
end;

function TZMySQL323PlainDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql323.PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQL323PlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := ZPlainMySql323.PMYSQL_RES(Res).row_count;
end;

function TZMySQL323PlainDriver.GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
begin
  Result := TZMySQLStatus(ZPlainMySql323.PMYSQL(Handle).status);
end;

function TZMySQL323PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := MYSQL_API.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQL323PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := ZPlainMySql323.PMYSQL_RES(Res).field_count;
end;

function TZMySQL323PlainDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql323.PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQL323PlainDriver.GetFieldLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql323.PMYSQL_FIELD(Field)^.length;
end;

function TZMySQL323PlainDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql323.PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQL323PlainDriver.GetFieldName(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql323.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL323PlainDriver.GetFieldTable(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql323.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL323PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PChar;
begin
  Result := ZPlainMySql323.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL323PlainDriver.GetLastErrorCode(
  Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql323.PMYSQL(Handle)._net.last_errno;
end;

function TZMySQL323PlainDriver.GetClientVersion: Integer;
begin
 Result := 32300;
end;

function TZMySQL323PlainDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := 32300;
end;

procedure TZMySQL323PlainDriver.BuildArguments(Options: TStrings);
begin

end;

{ TZMySQL40PlainDriver }

constructor TZMySQL40PlainDriver.Create;
begin
end;

function TZMySQL40PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-4.0';
end;

function TZMySQL40PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 4.0+';
end;

procedure TZMySQL40PlainDriver.Initialize;
begin
  ZPlainMySql40.LibraryLoader.LoadIfNeeded;
  MYSQL_API := ZPlainMySql40.LibraryLoader.api_rec;
end;

procedure TZMySQL40PlainDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQL40PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL40PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQL40PlainDriver.Debug(Debug: PChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQL40PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQL40PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQL40PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQL40PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL40PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQL40PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQL40PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQL40PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  MYSQL_API.mysql_free_result(Res);
end;

function TZMySQL40PlainDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQL40PlainDriver.GetClientInfo: PChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQL40PlainDriver.GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQL40PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQL40PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL40PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL40PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQL40PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQL40PlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
Begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
End;

function TZMySQL40PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQL40PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQL40PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQL40PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQL40PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(1, nil, nil);
  Handle := AllocMem(SizeOf(ZPlainMySql40.mysql));
  Result := MYSQL_API.mysql_init(Handle);
end;

function TZMySQL40PlainDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(ZPlainMySql40.PMYSQL(Handle));
end;

procedure TZMySQL40PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
  if Handle <> nil then
    FreeMem(Handle);
  Handle := nil;
end;

function TZMySQL40PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQL40PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQL40PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL40PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQL40PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQL40PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQL40PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQL40PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQL40PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,
    ZPlainMySql40.TMySqlOption(Option), Arg);
end;

function TZMySQL40PlainDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle);
end;

function TZMySQL40PlainDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    query: AnsiString;
    testResult: Integer;
begin
    if (mode = True) then
        query := 'AUTOCOMMIT=1'
    else
        query := 'AUTOCOMMIT=0';
    testResult := MYSQL_API.mysql_query(ZPlainMySql40.PMYSQL(Handle),pchar(query));
    Result := (testResult = 0);
end;

function TZMySQL40PlainDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    testResult: Integer;
begin
    testResult := MYSQL_API.mysql_query(ZPlainMySql40.PMYSQL(Handle),'COMMIT');
    Result := (testResult = 0);
end;

function TZMySQL40PlainDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_more_results']);
    result := false;
End;

function TZMySQL40PlainDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_next_rowset']);
    result := 1;
End;

function TZMySQL40PlainDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    testResult: Integer;
begin
    testResult := MYSQL_API.mysql_query(ZPlainMySql40.PMYSQL(Handle),'ROLLBACK');
    Result := (testResult = 0);
end;

function TZMySQL40PlainDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_sqlstate']);
    Result := '?????';  {mysql 4.0 doesn't support this}
end;

function TZMySQL40PlainDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_affected_rows']);
    result := 0;
End;

function TZMySQL40PlainDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_bind_parameters']);
    result := 1;
End;

function TZMySQL40PlainDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_bind_result']);
    result := 1;
End;

function TZMySQL40PlainDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_close']);
    result := nil;
end;

function TZMySQL40PlainDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_errno']);
    result :=  0;
End;

function TZMySQL40PlainDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_error'] );
    result := '';
end;

function TZMySQL40PlainDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_execute']);
    result := 1;
End;

function TZMySQL40PlainDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_fetch']);
    result := 1;
End;

function TZMySQL40PlainDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_field_count']);
    result := 0;
End;

function TZMySQL40PlainDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_init']);
    result := nil;
end;

function TZMySQL40PlainDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_insert_id']);
    result := 0;
End;

function TZMySQL40PlainDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_num_rows']);
    result := 0;
End;

function TZMySQL40PlainDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_param_count']);
    result := 0;
End;

function TZMySQL40PlainDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_prepare']);
    Result := 1;
end;

function TZMySQL40PlainDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_result_metadata']);
    result := nil;
End;

function TZMySQL40PlainDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PChar;
begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_sql_state']);
    result := '?????';
end;

function TZMySQL40PlainDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
Begin
    Exception.CreateFmt (SUnsupportedByDriver, ['mysql_stmt_store_result']);
    result := 1;
End;

function TZMySQL40PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_store_result(Handle);
end;

function TZMySQL40PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQL40PlainDriver.GetLastError(Handle: PZMySQLConnect): PChar;
begin
  Result := @(ZPlainMySql40.PMYSQL(Handle)^._net.last_error);
end;

function TZMySQL40PlainDriver.GetFieldType(Field: PZMySQLField): Byte;
begin
  Result := ZPlainMySql40.PMYSQL_FIELD(Field)^._type;
end;

function TZMySQL40PlainDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql40.PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQL40PlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := ZPlainMySql40.PMYSQL_RES(Res).row_count;
end;

function TZMySQL40PlainDriver.GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
begin
  Result := TZMySQLStatus(ZPlainMySql40.PMYSQL(Handle).status);
end;

function TZMySQL40PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := MYSQL_API.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQL40PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := ZPlainMySql40.PMYSQL_RES(Res).field_count;
end;

function TZMySQL40PlainDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql40.PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQL40PlainDriver.GetFieldLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql40.PMYSQL_FIELD(Field)^.length;
end;

function TZMySQL40PlainDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql40.PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQL40PlainDriver.GetFieldName(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql40.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL40PlainDriver.GetFieldTable(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql40.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL40PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PChar;
begin
  Result := ZPlainMySql40.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL40PlainDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql40.PMYSQL(Handle)._net.last_errno;
end;

function TZMySQL40PlainDriver.GetClientVersion: Integer;
begin
 Result := 40000;
end;

function TZMySQL40PlainDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := 40000;
end;

procedure TZMySQL40PlainDriver.BuildArguments(Options: TStrings);
begin

end;

{ TZMySQLD40PlainDriver }

function TZMySQLD40PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-4.0';
end;

function TZMySQLD40PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 4.0+';
end;

function TZMySQLD40PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS);
//    MYSQL_API.mysql_server_init(3, @DEFAULT_PARAMS, @SERVER_GROUPS);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

procedure TZMySQLD40PlainDriver.Initialize;
begin
  ZPlainMySql40.LibraryLoaderEmbedded.LoadIfNeeded;
  MYSQL_API := ZPlainMySql40.LibraryLoaderEmbedded.api_rec;
end;

procedure TZMySQLD40PlainDriver.BuildArguments(Options: TStrings);
begin
  BuildServerArguments(Options);
end;

{$ENDIF ENABLE_MYSQL_DEPRECATED}

{ TZMySQL41PlainDriver }

constructor TZMySQL41PlainDriver.Create;
begin
end;

function TZMySQL41PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-4.1';
end;

function TZMySQL41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 4.1+';
end;

procedure TZMySQL41PlainDriver.Initialize;
begin
  ZPlainMySql41.LibraryLoader.LoadIfNeeded;
  MYSQL_API := ZPlainMySql41.LibraryLoader.api_rec;
end;

procedure TZMySQL41PlainDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQL41PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL41PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQL41PlainDriver.Debug(Debug: PChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQL41PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQL41PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQL41PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQL41PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL41PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQL41PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQL41PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQL41PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  MYSQL_API.mysql_free_result(Res);
end;

procedure TZMySQL41PlainDriver.SetCharacterSet(Handle: PZMySQLConnect; const CharSet: PChar);
begin
  MYSQL_API.mysql_set_character_set(Handle, CharSet);
end;

function TZMySQL41PlainDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQL41PlainDriver.GetClientInfo: PChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQL41PlainDriver.GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQL41PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQL41PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL41PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL41PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQL41PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQL41PlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
Begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
End;

function TZMySQL41PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQL41PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQL41PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQL41PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQL41PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(1, nil, nil);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

function TZMySQL41PlainDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(ZPlainMySql41.PMYSQL(Handle));
end;

procedure TZMySQL41PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
  Handle := nil;
end;

function TZMySQL41PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQL41PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQL41PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL41PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQL41PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQL41PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQL41PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQL41PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQL41PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,
    ZPlainMySql41.TMySqlOption(Option), Arg);
end;

function TZMySQL41PlainDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle,shutdown_level);
end;

function TZMySQL41PlainDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    my_bool, my_mode: Byte;
begin
    if (mode = True) then
        my_mode := 1
    else
        my_mode := 0;
    my_bool := MYSQL_API.mysql_autocommit(ZPlainMySql41.PMYSQL(Handle), my_mode);
    Result := (my_bool = 0);
end;

function TZMySQL41PlainDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_commit(ZPlainMySql41.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL41PlainDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
Begin
    my_bool :=  MYSQL_API.mysql_more_results (ZPlainMySql41.PMYSQL(Handle));
    if (my_bool = 0) then
        Result := False
    else
        Result := True;
End;

function TZMySQL41PlainDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
Begin
    Result := MYSQL_API.mysql_next_result (ZPlainMySql41.PMYSQL(Handle));
End;

function TZMySQL41PlainDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_rollback(ZPlainMySql41.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL41PlainDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Result := MYSQL_API.mysql_sqlstate (ZPlainMySql41.PMYSQL(Handle));
end;

function TZMySQL41PlainDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Result :=  MYSQL_API.mysql_stmt_affected_rows (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Result := MYSQL_API.mysql_stmt_bind_param (ZPlainMySql41.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
End;

function TZMySQL41PlainDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Result := MYSQL_API.mysql_stmt_bind_result (ZPlainMySql41.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
End;

function TZMySQL41PlainDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_stmt_close(ZPlainMySql41.PMYSQL_STMT(PrepStmtHandle));
    if (my_bool = 0) then
        Result := nil
    else
        Result := PrepStmtHandle;
end;

function TZMySQL41PlainDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
Begin
    Result := MYSQL_API.mysql_stmt_errno(ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Result := MYSQL_API.mysql_stmt_error(ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_execute (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_fetch (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_field_count(ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Result := MYSQL_API.mysql_stmt_init(ZPlainMySql41.PMYSQL(Handle));
end;

function TZMySQL41PlainDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
Begin
    Result := MYSQL_API.mysql_stmt_insert_id (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    if (Handle = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_stmt_num_rows (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
Begin
    Result := MYSQL_API.mysql_stmt_param_count (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
begin
    Result := MYSQL_API.mysql_stmt_prepare(ZPlainMySql41.PMYSQL_STMT(PrepStmtHandle), Query, Length);
end;

function TZMySQL41PlainDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
Begin
    Result := MYSQL_API.mysql_stmt_result_metadata (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PChar;
begin
    Result := MYSQL_API.mysql_stmt_sqlstate (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_store_result (ZPlainMySql41.PMYSQL_STMT(Handle));
End;

function TZMySQL41PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := nil;
  // Try and locate a result which has rows.
  repeat
    if Result <> nil then MYSQL_API.mysql_free_result(Result);
    Result := MYSQL_API.mysql_store_result(Handle);
  until (GetRowCount(Result) > 0) or (MYSQL_API.mysql_next_result(Handle) <> 0);
  while MYSQL_API.mysql_next_result(Handle) = 0 do begin
    // Discard remaining results (empty the driver).
    MYSQL_API.mysql_free_result(MYSQL_API.mysql_store_result(Handle));
  end;
end;

function TZMySQL41PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQL41PlainDriver.GetLastError(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_error(Handle);
end;

function TZMySQL41PlainDriver.GetFieldType(Field: PZMySQLField): Byte;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^._type;
end;

function TZMySQL41PlainDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQL41PlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := ZPlainMySql41.PMYSQL_RES(Res).row_count;
end;

{$IFDEF ENABLE_MYSQL_DEPRECATED}
function TZMySQL41PlainDriver.GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
begin
  Result := TZMySQLStatus(ZPlainMySql41.PMYSQL(Handle).status);
end;
{$ENDIF ENABLE_MYSQL_DEPRECATED}

function TZMySQL41PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := MYSQL_API.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQL41PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_RES(Res).field_count;
end;

function TZMySQL41PlainDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQL41PlainDriver.GetFieldLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.length;
end;

function TZMySQL41PlainDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQL41PlainDriver.GetFieldName(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL41PlainDriver.GetFieldTable(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL41PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PChar;
begin
  Result := ZPlainMySql41.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL41PlainDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_errno(ZPlainMySql41.PMYSQL(Handle));
end;

function TZMySQL41PlainDriver.GetClientVersion: Integer;
begin
 Result := MYSQL_API.mysql_get_client_version;
end;

function TZMySQL41PlainDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := MYSQL_API.mysql_get_server_version(Handle);
end;

procedure TZMySQL41PlainDriver.BuildArguments(Options: TStrings);
begin

end;

{ TZMySQLD41PlainDriver }

function TZMySQLD41PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-4.1';
end;

function TZMySQLD41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 4.1+';
end;

function TZMySQLD41PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS);
//    MYSQL_API.mysql_server_init(3, @DEFAULT_PARAMS, @SERVER_GROUPS);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

procedure TZMySQLD41PlainDriver.Initialize;
begin
  ZPlainMySql41.LibraryLoaderEmbedded.LoadIfNeeded;
  MYSQL_API := ZPlainMySql41.LibraryLoaderEmbedded.api_rec;
end;

procedure TZMySQLD41PlainDriver.BuildArguments(Options: TStrings);
begin
  BuildServerArguments(Options);
end;

{ TZMySQL5PlainDriver }

constructor TZMySQL5PlainDriver.Create;
begin
end;

function TZMySQL5PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-5';
end;

function TZMySQL5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 5.0+';
end;

procedure TZMySQL5PlainDriver.Initialize;
begin
  ZPlainMySql5.LibraryLoader.LoadIfNeeded;
  MYSQL_API := ZPlainMySql5.LibraryLoader.api_rec;
end;

procedure TZMySQL5PlainDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQL5PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL5PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQL5PlainDriver.Debug(Debug: PChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQL5PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQL5PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQL5PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQL5PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL5PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQL5PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQL5PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  if Res = nil then Result := nil
  else Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQL5PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  MYSQL_API.mysql_free_result(Res);
end;

procedure TZMySQL5PlainDriver.SetCharacterSet(Handle: PZMySQLConnect; const CharSet: PChar);
begin
  MYSQL_API.mysql_set_character_set(Handle, CharSet);
end;

function TZMySQL5PlainDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQL5PlainDriver.GetClientInfo: PChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQL5PlainDriver.GetEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQL5PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQL5PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL5PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL5PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQL5PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQL5PlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
Begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
End;

function TZMySQL5PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQL5PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQL5PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQL5PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQL5PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(1, nil, nil);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

function TZMySQL5PlainDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(ZPlainMySql5.PMYSQL(Handle));
end;

procedure TZMySQL5PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
  Handle := nil;
end;

function TZMySQL5PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQL5PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQL5PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL5PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQL5PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQL5PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQL5PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQL5PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQL5PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,
    ZPlainMySql5.TMySqlOption(Option), Arg);
end;

function TZMySQL5PlainDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle,shutdown_level);
end;

function TZMySQL5PlainDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    my_bool, my_mode: Byte;
begin
    if (mode = True) then
        my_mode := 1
    else
        my_mode := 0;
    my_bool := MYSQL_API.mysql_autocommit(ZPlainMySql5.PMYSQL(Handle), my_mode);
    Result := (my_bool = 0);
end;

function TZMySQL5PlainDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_commit(ZPlainMySql5.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL5PlainDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
Begin
    my_bool :=  MYSQL_API.mysql_more_results (ZPlainMySql5.PMYSQL(Handle));
    if (my_bool = 0) then
        Result := False
    else
        Result := True;
End;

function TZMySQL5PlainDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
Begin
    Result := MYSQL_API.mysql_next_result (ZPlainMySql5.PMYSQL(Handle));
End;

function TZMySQL5PlainDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_rollback(ZPlainMySql5.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL5PlainDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Result := MYSQL_API.mysql_sqlstate (ZPlainMySql5.PMYSQL(Handle));
end;

function TZMySQL5PlainDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    Result :=  MYSQL_API.mysql_stmt_affected_rows (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Result := MYSQL_API.mysql_stmt_bind_param (ZPlainMySql5.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
End;

function TZMySQL5PlainDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
Begin
    Result := MYSQL_API.mysql_stmt_bind_result (ZPlainMySql5.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
End;

function TZMySQL5PlainDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_stmt_close(ZPlainMySql5.PMYSQL_STMT(PrepStmtHandle));
    if (my_bool = 0) then
        Result := nil
    else
        Result := PrepStmtHandle;
end;

function TZMySQL5PlainDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
Begin
    Result := MYSQL_API.mysql_stmt_errno(ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Result := MYSQL_API.mysql_stmt_error(ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_execute (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_fetch (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_field_count(ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Result := MYSQL_API.mysql_stmt_init(ZPlainMySql5.PMYSQL(Handle));
end;

function TZMySQL5PlainDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
Begin
    Result := MYSQL_API.mysql_stmt_insert_id (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
Begin
    if (Handle = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_stmt_num_rows (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
Begin
    Result := MYSQL_API.mysql_stmt_param_count (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PChar; Length: Integer): Integer;
begin
    Result := MYSQL_API.mysql_stmt_prepare(ZPlainMySql5.PMYSQL_STMT(PrepStmtHandle), Query, Length);
end;

function TZMySQL5PlainDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
Begin
    Result := MYSQL_API.mysql_stmt_result_metadata (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PChar;
begin
    Result := MYSQL_API.mysql_stmt_sqlstate (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
Begin
    Result := MYSQL_API.mysql_stmt_store_result (ZPlainMySql5.PMYSQL_STMT(Handle));
End;

function TZMySQL5PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_store_result(Handle);
end;

function TZMySQL5PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQL5PlainDriver.GetLastError(Handle: PZMySQLConnect): PChar;
begin
  Result := MYSQL_API.mysql_error(Handle);
end;

function TZMySQL5PlainDriver.GetFieldType(Field: PZMySQLField): Byte;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^._type;
end;

function TZMySQL5PlainDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQL5PlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := ZPlainMySql5.PMYSQL_RES(Res).row_count;
end;

{$IFDEF ENABLE_MYSQL_DEPRECATED}
function TZMySQL5PlainDriver.GetStatus(Handle: PZMySQLConnect): TZMySQLStatus;
begin
  Result := TZMySQLStatus(ZPlainMySql5.PMYSQL(Handle).status);
end;
{$ENDIF ENABLE_MYSQL_DEPRECATED}

function TZMySQL5PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := MYSQL_API.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQL5PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_RES(Res).field_count;
end;

function TZMySQL5PlainDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQL5PlainDriver.GetFieldLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.length;
end;

function TZMySQL5PlainDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQL5PlainDriver.GetFieldName(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL5PlainDriver.GetFieldTable(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL5PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PChar;
begin
  Result := ZPlainMySql5.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL5PlainDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_errno(ZPlainMySql5.PMYSQL(Handle));
end;

function TZMySQL5PlainDriver.GetClientVersion: Integer;
begin
 Result := MYSQL_API.mysql_get_client_version;
end;

function TZMySQL5PlainDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := MYSQL_API.mysql_get_server_version(Handle);
end;

procedure TZMySQL5PlainDriver.BuildArguments(Options: TStrings);
begin

end;

{ TZMySQLD5PlainDriver }

function TZMySQLD5PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-5';
end;

function TZMySQLD5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 5+';
end;

function TZMySQLD5PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS);
//    MYSQL_API.mysql_server_init(3, @DEFAULT_PARAMS, @SERVER_GROUPS);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

procedure TZMySQLD5PlainDriver.Initialize;
begin
  ZPlainMySql5.LibraryLoaderEmbedded.LoadIfNeeded;
  MYSQL_API := ZPlainMySql5.LibraryLoaderEmbedded.api_rec;
end;

procedure TZMySQLD5PlainDriver.BuildArguments(Options: TStrings);
begin
  BuildServerArguments(Options);
end;

end.

