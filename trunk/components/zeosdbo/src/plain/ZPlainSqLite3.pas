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

unit ZPlainSqLite3;

interface

{$I ZPlain.inc}

{$J+}
{$Z4}

uses Classes, ZPlainLoader, ZCompatibility;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS_DLL_LOCATION = 'sqlite3.dll';
  LINUX_DLL_LOCATION = 'libsqlite.so';

  SQLITE_ISO8859   = 1;
  MASTER_NAME      = 'sqlite_master';
  TEMP_MASTER_NAME = 'sqlite_temp_master';
  SQLITE_VERSION   = '3.2.7';

  { Return values for sqlite_exec() and sqlite_step() }
  SQLITE_OK           = 0;   // Successful result
  SQLITE_ERROR        = 1;   // SQL error or missing database
  SQLITE_INTERNAL     = 2;   // An internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  _SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite_interrupt()
  SQLITE_IOERR        = 10;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT      = 11;  // The database disk image is malformed
  SQLITE_NOTFOUND     = 12;  // (Internal Only) Table or record not found
  SQLITE_FULL         = 13;  // Insertion failed because database is full
  SQLITE_CANTOPEN     = 14;  // Unable to open the database file
  SQLITE_PROTOCOL     = 15;  // Database lock protocol error
  SQLITE_EMPTY        = 16;  // (Internal Only) Database table is empty
  SQLITE_SCHEMA       = 17;  // The database schema changed
  SQLITE_TOOBIG       = 18;  // Too much data for one row of a table
  SQLITE_CONSTRAINT   = 19;  // Abort due to contraint violation
  SQLITE_MISMATCH     = 20;  // Data type mismatch
  SQLITE_MISUSE       = 21;  // Library used incorrectly
  SQLITE_NOLFS        = 22;  // Uses OS features not supported on host
  SQLITE_AUTH         = 23;  // Authorization denied
  SQLITE_FORMAT       = 24;  // Auxiliary database format error
  SQLITE_RANGE        = 25;  // 2nd parameter to sqlite_bind out of range
  SQLITE_NOTADB       = 26;  // File opened that is not a database file
  SQLITE_ROW          = 100;  // sqlite_step() has another row ready
  SQLITE_DONE         = 101;  // sqlite_step() has finished executing

  SQLITE_NUMERIC      = -1;
  SQLITE_TEXT         = -2;
  SQLITE_ARGS         = -3;

  {
    The second parameter to the access authorization function above will
    be one of the values below.  These values signify what kind of operation
    is to be authorized.  The 3rd and 4th parameters to the authorization
    function will be parameters or NULL depending on which of the following
    codes is used as the second parameter.  The 5th parameter is the name
    of the database ("main", "temp", etc.) if applicable.  The 6th parameter
    is the name of the inner-most trigger or view that is responsible for
    the access attempt or NULL if this access attempt is directly from
    input SQL code.

                                             Arg-3           Arg-4
  }
  SQLITE_COPY                  = 0;  // Table Name      File Name
  SQLITE_CREATE_INDEX          = 1;  // Index Name      Table Name
  SQLITE_CREATE_TABLE          = 2;  // Table Name      NULL
  SQLITE_CREATE_TEMP_INDEX     = 3;  // Index Name      Table Name
  SQLITE_CREATE_TEMP_TABLE     = 4;  // Table Name      NULL
  SQLITE_CREATE_TEMP_TRIGGER   = 5;  // Trigger Name    Table Name
  SQLITE_CREATE_TEMP_VIEW      = 6;  // View Name       NULL
  SQLITE_CREATE_TRIGGER        = 7;  // Trigger Name    Table Name
  SQLITE_CREATE_VIEW           = 8;  // View Name       NULL
  SQLITE_DELETE                = 9;  // Table Name      NULL
  SQLITE_DROP_INDEX            = 10; // Index Name      Table Name
  SQLITE_DROP_TABLE            = 11; // Table Name      NULL
  SQLITE_DROP_TEMP_INDEX       = 12; // Index Name      Table Name
  SQLITE_DROP_TEMP_TABLE       = 13; // Table Name      NULL
  SQLITE_DROP_TEMP_TRIGGER     = 14; // Trigger Name    Table Name
  SQLITE_DROP_TEMP_VIEW        = 15; // View Name       NULL
  SQLITE_DROP_TRIGGER          = 16; // Trigger Name    Table Name
  SQLITE_DROP_VIEW             = 17; // View Name       NULL
  SQLITE_INSERT                = 18; // Table Name      NULL
  SQLITE_PRAGMA                = 19; // Pragma Name     1st arg or NULL
  SQLITE_READ                  = 20; // Table Name      Column Name
  SQLITE_SELECT                = 21; // NULL            NULL
  SQLITE_TRANSACTION           = 22; // NULL            NULL
  SQLITE_UPDATE                = 23; // Table Name      Column Name

  { The return value of the authorization function should be one of the
    following constants: }
  SQLITE_DENY    = 1;   // Abort the SQL statement with an error
  SQLITE_IGNORE = 2;   // Don't allow access, but don't generate an error

type
  Psqlite = Pointer;
  Psqlite_func = Pointer;
  Psqlite_vm = Pointer;

{ ************** Plain API Function types definition ************* }

  Tsqlite_callback = function(p1: Pointer; p2: Integer; var p3: PChar;
    var p4: PChar): Integer; cdecl;
  Tsqlite_simple_callback = function(p1: Pointer): Integer; cdecl;
  Tsqlite_simple_callback0 = function(p1: Pointer): Pointer; cdecl;
  Tsqlite_busy_callback = function(p1: Pointer; const p2: PChar;
    p3: Integer): Integer; cdecl;

  Tsqlite_function_callback = procedure(p1: Psqlite_func; p2: Integer;
    const p3: PPChar); cdecl;
  Tsqlite_finalize_callback = procedure(p1: Psqlite_func); cdecl;
  Tsqlite_auth_callback = function(p1: Pointer; p2: Integer; const p3: PChar;
    const p4: PChar; const p5: PChar; const p6: PChar): Integer; cdecl;
  Tsqlite_trace_callback = procedure(p1: Pointer; const p2: PChar); cdecl;

  Tsqlite_open = function(const filename: PChar;var Qsqlite: Psqlite):
    Integer; cdecl;
  Tsqlite_close = procedure(db: Psqlite); cdecl;
  Tsqlite_column_count = function(db: Psqlite): Integer; cdecl;
  Tsqlite_column_bytes = function(db: Psqlite; iCol: Integer): Pchar; cdecl;
  Tsqlite_column_name = function(db: Psqlite; iCol: Integer): Pchar; cdecl;
  Tsqlite_column_decltype = function(db: Psqlite; iCol: Integer): Pchar; cdecl;
  Tsqlite_exec = function(db: Psqlite; const sql: PChar;
    sqlite_callback: Tsqlite_callback; arg: Pointer;
    var errmsg: PChar): Integer; cdecl;
  Tsqlite_errmsg = function(db: Psqlite): PChar; cdecl;
  Tsqlite_last_insert_rowid = function(db: Psqlite): Integer; cdecl;
  Tsqlite_changes = function(db: Psqlite): Integer; cdecl;
  Tsqlite_last_statement_changes = function(db: Psqlite): Integer; cdecl;
  Tsqlite_error_string = function(code: Integer): PChar; cdecl;
  Tsqlite_interrupt = procedure(db: Psqlite); cdecl;
  Tsqlite_complete = function(const sql: PChar): Integer; cdecl;
  Tsqlite_busy_handler = procedure(db: Psqlite;
    callback: Tsqlite_busy_callback; ptr: Pointer); cdecl;
  Tsqlite_busy_timeout = procedure(db: Psqlite; ms: Integer); cdecl;
  Tsqlite_get_table = function(db: Psqlite; const sql: PChar;
    var resultp: PPChar; var nrow: Integer; var ncolumn: Integer;
    var errmsg: PChar): Integer; cdecl;
  Tsqlite_free_table = procedure(var result: PChar); cdecl;
  Tsqlite_freemem = procedure(ptr: Pointer); cdecl;
  Tsqlite_libversion = function: PChar; cdecl;
  Tsqlite_libencoding = function: PChar; cdecl;

  Tsqlite_create_function = function(db: Psqlite; const zName: PChar;
    nArg: Integer; callback: Tsqlite_function_callback;
    pUserData: Pointer): Integer; cdecl;
  Tsqlite_create_aggregate = function(db: Psqlite; const zName: PChar;
    nArg: Integer; callback: Tsqlite_function_callback;
    finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer; cdecl;
  Tsqlite_function_type = function(db: Psqlite; const zName: PChar;
    datatype: Integer): Integer; cdecl;
  Tsqlite_set_result_string = function(func: Psqlite_func; const arg: PChar;
    len: Integer; UN: Tsqlite_simple_callback): PChar; cdecl;
  Tsqlite_set_result_int = procedure(func: Psqlite_func; arg: Integer); cdecl;
  Tsqlite_set_result_double = procedure(func: Psqlite_func; arg: Double); cdecl;
  Tsqlite_set_result_error = procedure(func: Psqlite_func; const arg: PChar;
    len: Integer); cdecl;
  Tsqlite_user_data = function(func: Psqlite_func): Pointer; cdecl;
  Tsqlite_aggregate_context = function(func: Psqlite_func;
    nBytes: Integer): Pointer; cdecl;
  Tsqlite_aggregate_count = function(func: Psqlite_func): Integer; cdecl;

  Tsqlite_set_authorizer = function(db: Psqlite;
    callback: Tsqlite_auth_callback; pUserData: Pointer): Integer; cdecl;
  Tsqlite_trace = function(db: Psqlite; callback: Tsqlite_trace_callback;
    ptr: Pointer): Pointer; cdecl;

  Tsqlite_compile = function(db: Psqlite; const zSql: PChar; nBytes: Integer;
     var ppVm: Psqlite_vm; pzTail: PChar): Integer; cdecl;
  Tsqlite_step = function(pVm: Psqlite_vm): Integer; cdecl;
  Tsqlite_finalize = function(vm: Psqlite_vm): Integer; cdecl;
  Tsqlite_reset = function(vm: Psqlite_vm): Integer; cdecl;
  Tsqlite_bind = function(vm: Psqlite_vm; idx: Integer; const value: PChar;
    len: Integer; copy: Integer): Integer; cdecl;
{  Tsqlite_bind_double = function(vm: Psqlite_vm; idx: Integer; const value: PChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_int = function(vm: Psqlite_vm; idx: Integer; const value: PChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_int64 = function(vm: Psqlite_vm; idx: Integer; const value: PChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_null = function(vm: Psqlite_vm; idx: Integer; const value: PChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_text = function(vm: Psqlite_vm; idx: Integer; const value: PChar;
    len: Integer; copy: Integer): Integer; cdecl;}

  Tsqlite_progress_handler = procedure(db: Psqlite; p1: Integer;
    callback: Tsqlite_simple_callback; ptr: Pointer); cdecl;
  Tsqlite_commit_hook = function(db: Psqlite; callback: Tsqlite_simple_callback;
    ptr: Pointer): Pointer; cdecl;

  Tsqlite_open_encrypted = function(const zFilename: PChar;
    const pKey: PChar; nKey: Integer; var pErrcode: Integer;
    var pzErrmsg: PChar): Psqlite; cdecl;
  Tsqlite_rekey = function(db: Psqlite; const pKey: Pointer;
    nKey: Integer): Integer; cdecl;

var

{ ************* Plain API Function variables definition ************ }

  sqlite_open: Tsqlite_open;
  sqlite_close: Tsqlite_close;
  sqlite_column_count: Tsqlite_column_count;
  sqlite_column_bytes: Tsqlite_column_bytes;
  sqlite_column_name: Tsqlite_column_name;
  sqlite_column_decltype: Tsqlite_column_decltype;
  sqlite_exec: Tsqlite_exec;
  sqlite_errmsg: Tsqlite_errmsg;
  sqlite_last_insert_rowid: Tsqlite_last_insert_rowid;
  sqlite_changes: Tsqlite_changes;
  sqlite_last_statement_changes: Tsqlite_last_statement_changes;
  sqlite_error_string: Tsqlite_error_string;
  sqlite_interrupt: Tsqlite_interrupt;
  sqlite_complete: Tsqlite_complete;
  sqlite_busy_handler: Tsqlite_busy_handler;
  sqlite_busy_timeout: Tsqlite_busy_timeout;
  sqlite_get_table: Tsqlite_get_table;
  sqlite_free_table: Tsqlite_free_table;
  sqlite_freemem: Tsqlite_freemem;
  sqlite_libversion: Tsqlite_libversion;
  sqlite_libencoding: Tsqlite_libencoding;
  sqlite_create_function: Tsqlite_create_function;
  sqlite_create_aggregate: Tsqlite_create_aggregate;
  sqlite_function_type: Tsqlite_function_type;
  sqlite_set_result_string: Tsqlite_set_result_string;
  sqlite_set_result_int: Tsqlite_set_result_int;
  sqlite_set_result_double: Tsqlite_set_result_double;
  sqlite_set_result_error: Tsqlite_set_result_error;
  sqlite_user_data: Tsqlite_user_data;
  sqlite_aggregate_context: Tsqlite_aggregate_context;
  sqlite_aggregate_count: Tsqlite_aggregate_count;
  sqlite_set_authorizer: Tsqlite_set_authorizer;
  sqlite_trace: Tsqlite_trace;
  sqlite_compile: Tsqlite_compile;
  sqlite_step: Tsqlite_step;
  sqlite_finalize: Tsqlite_finalize;
  sqlite_reset: Tsqlite_reset;
  sqlite_bind: Tsqlite_bind;
  sqlite_progress_handler: Tsqlite_progress_handler;
  sqlite_commit_hook: Tsqlite_commit_hook;
  sqlite_open_encrypted: Tsqlite_open_encrypted;
  sqlite_rekey: Tsqlite_rekey;

var
  LibraryLoader: TZNativeLibraryLoader;

implementation

type
  {** Implements a loader for SQLite native library. }
  TZSQLiteNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
  end;

{ TZSQLiteNativeLibraryLoader }

{**           
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZSQLiteNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

{!}        @sqlite_open                   := GetAddress('sqlite3_open');
{!}        @sqlite_close                  := GetAddress('sqlite3_close');
{!}        @sqlite_column_count           := GetAddress('sqlite3_column_count');
{!}        @sqlite_column_bytes           := GetAddress('sqlite3_column_text');
{!}        @sqlite_column_name            := GetAddress('sqlite3_column_name');
{!}        @sqlite_column_decltype        := GetAddress('sqlite3_column_decltype');
{!}        @sqlite_exec                   := GetAddress('sqlite3_exec');
{!}        @sqlite_last_insert_rowid      := GetAddress('sqlite3_last_insert_rowid');
{!}        @sqlite_changes                := GetAddress('sqlite3_changes');
{!}        @sqlite_errmsg                 := GetAddress('sqlite3_errmsg');
{ ?-       @sqlite_last_statement_changes := GetAddress('sqlite3_last_statement_changes');}
{!}        @sqlite_interrupt              := GetAddress('sqlite3_interrupt');
{!}        @sqlite_complete               := GetAddress('sqlite3_complete');
{!}        @sqlite_busy_handler           := GetAddress('sqlite3_busy_handler');
{!}        @sqlite_busy_timeout           := GetAddress('sqlite3_busy_timeout');
{!}        @sqlite_get_table              := GetAddress('sqlite3_get_table');
{!}        @sqlite_free_table             := GetAddress('sqlite3_free_table');
{!}        @sqlite_freemem                := GetAddress('sqlite3_free');
{!}        @sqlite_libversion             := GetAddress('sqlite3_libversion');
{ ?-       @sqlite_libencoding            := GetAddress('sqlite3_libencoding');
{ ?-       @sqlite_create_function        := GetAddress('sqlite3_create_function');}
{ ?-       @sqlite_create_aggregate       := GetAddress('sqlite3_create_collation');}
{ ?-       @sqlite_function_type          := GetAddress('sqlite3_function_type');
{!}        @sqlite_set_result_string      := GetAddress('sqlite3_result_string');
{!}        @sqlite_set_result_int         := GetAddress('sqlite3_result_int');
{!}        @sqlite_set_result_double      := GetAddress('sqlite3_result_double');
{!}        @sqlite_set_result_error       := GetAddress('sqlite3_result_error');
{!}        @sqlite_user_data              := GetAddress('sqlite3_user_data');
{!}        @sqlite_aggregate_context      := GetAddress('sqlite3_aggregate_context');
{!}        @sqlite_aggregate_count        := GetAddress('sqlite3_aggregate_count');
{!}        @sqlite_set_authorizer         := GetAddress('sqlite3_set_authorizer');
{!}        @sqlite_trace                  := GetAddress('sqlite3_trace');
{!}        @sqlite_compile                := GetAddress('sqlite3_prepare');
{!}        @sqlite_step                   := GetAddress('sqlite3_step');
{!}        @sqlite_finalize               := GetAddress('sqlite3_finalize');
{!}        @sqlite_reset                  := GetAddress('sqlite3_reset');
{ ?-       @sqlite_bind                   := GetAddress('sqlite3_bind');}
{!}        @sqlite_progress_handler       := GetAddress('sqlite3_progress_handler');
{!}        @sqlite_commit_hook            := GetAddress('sqlite3_commit_hook');
{ ?-       @sqlite_open_encrypted         := GetAddress('sqlite3_open_encrypted');}
{ ?-       @sqlite_rekey                  := GetAddress('sqlite3_rekey');}

end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZSQLiteNativeLibraryLoader.Create(
    [WINDOWS_DLL_LOCATION]);
{$ELSE}
  LibraryLoader := TZSQLiteNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
end.

