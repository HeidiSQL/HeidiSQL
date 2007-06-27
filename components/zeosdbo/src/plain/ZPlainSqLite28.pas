{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to sqlite.dll             }
{                     Version 2.8                         }
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

unit ZPlainSqLite28;

interface

{$I ZPlain.inc}

{$J+}
{$Z4}

uses Classes, ZPlainLoader, ZCompatibility;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS_DLL_LOCATION = 'sqlite.dll';
  LINUX_DLL_LOCATION = 'libsqlite.so';

  SQLITE_ISO8859   = 1;
  MASTER_NAME      = 'sqlite_master';
  TEMP_MASTER_NAME = 'sqlite_temp_master';
  SQLITE_VERSION   = '2.8.13';

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
  SQLITE_ATTACH                = 24; // Filename        NULL
  SQLITE_DETACH                = 25; // Database Name   NULL

  { The return value of the authorization function should be one of the
    following constants: }
  SQLITE_DENY    = 1;   // Abort the SQL statement with an error
  SQLITE_IGNORE = 2;   // Don't allow access, but don't generate an error

type
  Psqlite = Pointer;
  Psqlite_func = Pointer;
  Psqlite_vm = Pointer;

{ ************** Plain API Function types definition ************* }

  Tsqlite_simple_callback = function(p1: Pointer): Integer; cdecl;
  Tsqlite_callback = function(p1: Pointer; p2: Integer; var p3: PChar;
    var p4: PChar): Integer; cdecl;
  Tsqlite_busy_callback = function(p1: Pointer; const p2: PChar;
    p3: Integer): Integer; cdecl;
  Tsqlite_function_callback = procedure(p1: Psqlite_func; p2: Integer;
    const p3: PPChar); cdecl;
  Tsqlite_finalize_callback = procedure(p1: Psqlite_func); cdecl;
  Tsqlite_auth_callback = function(p1: Pointer; p2: Integer; const p3: PChar;
    const p4: PChar; const p5: PChar; const p6: PChar): Integer; cdecl;
  Tsqlite_trace_callback = procedure(p1: Pointer; const p2: PChar); cdecl;

  Tsqlite_open = function(const filename: PChar; mode: Integer;
    var errmsg: PChar): Psqlite; cdecl;
  Tsqlite_close = procedure(db: Psqlite); cdecl;
  Tsqlite_exec = function(db: Psqlite; const sql: PChar;
    sqlite_callback: Tsqlite_callback; arg: Pointer;
    var errmsg: PChar): Integer; cdecl;
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
    len: Integer): PChar; cdecl;
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

  Tsqlite_compile = function(db: Psqlite; const zSql: PChar;
    var pzTail: PChar; var ppVm: Psqlite_vm;
    var pzErrmsg: PChar): Integer; cdecl;
  Tsqlite_step = function(pVm: Psqlite_vm; var pN: Integer;
    var pazValue: PPChar; var pazColName: PPChar): Integer; cdecl;
  Tsqlite_finalize = function(vm: Psqlite_vm;
    var pzErrMsg: PChar): Integer; cdecl;
  Tsqlite_reset = function(vm: Psqlite_vm;
    var pzErrMsg: PChar): Integer; cdecl;
  Tsqlite_bind = function(vm: Psqlite_vm; idx: Integer; const value: PChar;
    len: Integer; copy: Integer): Integer; cdecl;

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
  sqlite_exec: Tsqlite_exec;
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

  @sqlite_open                   := GetAddress('sqlite_open');
  @sqlite_close                  := GetAddress('sqlite_close');
  @sqlite_exec                   := GetAddress('sqlite_exec');
  @sqlite_last_insert_rowid      := GetAddress('sqlite_last_insert_rowid');
  @sqlite_changes                := GetAddress('sqlite_changes');
  @sqlite_last_statement_changes := GetAddress('sqlite_last_statement_changes');
  @sqlite_error_string           := GetAddress('sqlite_error_string');
  @sqlite_interrupt              := GetAddress('sqlite_interrupt');
  @sqlite_complete               := GetAddress('sqlite_complete');
  @sqlite_busy_handler           := GetAddress('sqlite_busy_handler');
  @sqlite_busy_timeout           := GetAddress('sqlite_busy_timeout');
  @sqlite_get_table              := GetAddress('sqlite_get_table');
  @sqlite_free_table             := GetAddress('sqlite_free_table');
  @sqlite_freemem                := GetAddress('sqlite_freemem');
  @sqlite_libversion             := GetAddress('sqlite_libversion');
  @sqlite_libencoding            := GetAddress('sqlite_libencoding');
  @sqlite_create_function        := GetAddress('sqlite_create_function');
  @sqlite_create_aggregate       := GetAddress('sqlite_create_aggregate');
  @sqlite_function_type          := GetAddress('sqlite_function_type');
  @sqlite_set_result_string      := GetAddress('sqlite_set_result_string');
  @sqlite_set_result_int         := GetAddress('sqlite_set_result_int');
  @sqlite_set_result_double      := GetAddress('sqlite_set_result_double');
  @sqlite_set_result_error       := GetAddress('sqlite_set_result_error');
  @sqlite_user_data              := GetAddress('sqlite_user_data');
  @sqlite_aggregate_context      := GetAddress('sqlite_aggregate_context');
  @sqlite_aggregate_count        := GetAddress('sqlite_aggregate_count');
  @sqlite_set_authorizer         := GetAddress('sqlite_set_authorizer');
  @sqlite_trace                  := GetAddress('sqlite_trace');
  @sqlite_compile                := GetAddress('sqlite_compile');
  @sqlite_step                   := GetAddress('sqlite_step');
  @sqlite_finalize               := GetAddress('sqlite_finalize');
  @sqlite_reset                  := GetAddress('sqlite_reset');
  @sqlite_bind                   := GetAddress('sqlite_bind');
  @sqlite_progress_handler       := GetAddress('sqlite_progress_handler');
  @sqlite_commit_hook            := GetAddress('sqlite_commit_hook');
  @sqlite_open_encrypted         := GetAddress('sqlite_open_encrypted');
  @sqlite_rekey                  := GetAddress('sqlite_rekey');
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

