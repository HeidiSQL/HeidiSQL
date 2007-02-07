{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for SQLite             }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZPlainSqLiteDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZCompatibility, ZPlainDriver, ZPlainSqLite28, ZPlainSqLite3,
     SysUtils;

const
  MASTER_NAME      = 'sqlite_master';
  TEMP_MASTER_NAME = 'sqlite_temp_master';

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

type

  {** Represents a generic interface to SQLite native API. }
  IZSQLitePlainDriver = interface (IZPlainDriver)
    ['{B931C952-3076-4ECB-9630-D900E8DB9869}']

    function Open(const filename: PChar; mode: Integer;
      var errmsg: PChar): Psqlite;
    procedure Close(db: Psqlite);
    function Execute(db: Psqlite; const sql: PChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PChar): Integer;
    function LastInsertRowId(db: Psqlite): Integer;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(code: Integer): PChar;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    function GetTable(db: Psqlite; const sql: PChar; var resultp: PPChar;
      var nrow: Integer; var ncolumn: Integer; var errmsg: PChar): Integer;
    procedure FreeTable(var result: PChar);
    procedure FreeMem(ptr: Pointer);
    function LibVersion: PChar;
    function LibEncoding: PChar;

    function CreateFunction(db: Psqlite; const zName: PChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      pUserData: Pointer): Integer;
    function CreateAggregate(db: Psqlite; const zName: PChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
    function FunctionType(db: Psqlite; const zName: PChar;
      datatype: Integer): Integer;
    function SetResultString(func: Psqlite_func; const arg: PChar;
      len: Integer): PChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    function Compile(db: Psqlite; const zSql: PChar;nBytes: Integer;
      var pzTail: PChar; var ppVm: Psqlite_vm; var pzErrmsg: PChar): Integer;
    function Step(pVm: Psqlite_vm; var pN: Integer; var pazValue: PPChar;
      var pazColName: PPChar): Integer;
    function Finalize(vm: Psqlite_vm; var pzErrMsg: PChar): Integer;
    function Reset(vm: Psqlite_vm; var pzErrMsg: PChar): Integer;
    function Bind(vm: Psqlite_vm; idx: Integer; const value: PChar;
      len: Integer; copy: Integer): Integer;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function OpenEncrypted(const zFilename: PChar; const pKey: PChar;
      nKey: Integer; var pErrcode: Integer; var pzErrmsg: PChar): Psqlite;
    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
  end;

  {** Implements a driver for SQLite 2.8 }
  TZSQLite28PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZSQLitePlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function Open(const filename: PChar; mode: Integer;
      var errmsg: PChar): Psqlite;
    procedure Close(db: Psqlite);
    function Execute(db: Psqlite; const sql: PChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PChar): Integer;
    function LastInsertRowId(db: Psqlite): Integer;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(code: Integer): PChar;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    function GetTable(db: Psqlite; const sql: PChar; var resultp: PPChar;
      var nrow: Integer; var ncolumn: Integer; var errmsg: PChar): Integer;
    procedure FreeTable(var result: PChar);
    procedure FreeMem(ptr: Pointer);
    function LibVersion: PChar;
    function LibEncoding: PChar;

    function CreateFunction(db: Psqlite; const zName: PChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      pUserData: Pointer): Integer;
    function CreateAggregate(db: Psqlite; const zName: PChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
    function FunctionType(db: Psqlite; const zName: PChar;
      datatype: Integer): Integer;
    function SetResultString(func: Psqlite_func; const arg: PChar;
      len: Integer): PChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    function Compile(db: Psqlite; const zSql: PChar;
      nBytes: Integer;var pzTail: PChar;
      var ppVm: Psqlite_vm; var pzErrmsg: PChar): Integer;
    function Step(pVm: Psqlite_vm; var pN: Integer; var pazValue: PPChar;
      var pazColName: PPChar): Integer;
    function Finalize(vm: Psqlite_vm; var pzErrMsg: PChar): Integer;
    function Reset(vm: Psqlite_vm; var pzErrMsg: PChar): Integer;
    function Bind(vm: Psqlite_vm; idx: Integer; const value: PChar;
      len: Integer; copy: Integer): Integer;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function OpenEncrypted(const zFilename: PChar; const pKey: PChar;
      nKey: Integer; var pErrcode: Integer; var pzErrmsg: PChar): Psqlite;
    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
  end;

  {** Implements a driver for SQLite 3 }
  TZSQLite3PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZSQLitePlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function Open(const filename: PChar; mode: Integer;
      var errmsg: PChar): Psqlite;
    procedure Close(db: Psqlite);
    function Execute(db: Psqlite; const sql: PChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PChar): Integer;
    function LastInsertRowId(db: Psqlite): Integer;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(code: Integer): PChar;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    function GetTable(db: Psqlite; const sql: PChar; var resultp: PPChar;
      var nrow: Integer; var ncolumn: Integer; var errmsg: PChar): Integer;
    procedure FreeTable(var result: PChar);
    procedure FreeMem(ptr: Pointer);
    function LibVersion: PChar;
    function LibEncoding: PChar;

    function CreateFunction(db: Psqlite; const zName: PChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      pUserData: Pointer): Integer;
    function CreateAggregate(db: Psqlite; const zName: PChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
    function FunctionType(db: Psqlite; const zName: PChar;
      datatype: Integer): Integer;
    function SetResultString(func: Psqlite_func; const arg: PChar;
      len: Integer): PChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    function Compile(db: Psqlite; const zSql: PChar;
      nBytes: Integer;var pzTail: PChar; var ppVm: Psqlite_vm;
      var pzErrmsg: PChar): Integer;
    function Step(pVm: Psqlite_vm; var pN: Integer; var pazValue: PPChar;
      var pazColName: PPChar): Integer;
    function Finalize(vm: Psqlite_vm; var pzErrMsg: PChar): Integer;
    function Reset(vm: Psqlite_vm; var pzErrMsg: PChar): Integer;
    function Bind(vm: Psqlite_vm; idx: Integer; const value: PChar;
      len: Integer; copy: Integer): Integer;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function OpenEncrypted(const zFilename: PChar; const pKey: PChar;
      nKey: Integer; var pErrcode: Integer; var pzErrmsg: PChar): Psqlite;
    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
  end;

implementation

{ TZSQLite28PlainDriver }

constructor TZSQLite28PlainDriver.Create;
begin
end;

function TZSQLite28PlainDriver.GetProtocol: string;
begin
  Result := 'sqlite-2.8';
end;

function TZSQLite28PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for SQLite 2.8';
end;

procedure TZSQLite28PlainDriver.Initialize;
begin
  ZPlainSQLite28.LibraryLoader.LoadIfNeeded;
end;

function TZSQLite28PlainDriver.AggregateContext(func: Psqlite_func;
  nBytes: Integer): Pointer;
begin
  Result := ZPlainSqLite28.sqlite_aggregate_context(func, nBytes);
end;

function TZSQLite28PlainDriver.AggregateCount(func: Psqlite_func): Integer;
begin
  Result := ZPlainSqLite28.sqlite_aggregate_count(func);
end;

function TZSQLite28PlainDriver.Bind(vm: Psqlite_vm; idx: Integer;
  const value: PChar; len, copy: Integer): Integer;
begin
  Result := ZPlainSqLite28.sqlite_bind(vm, idx, value, len, copy);
end;

procedure TZSQLite28PlainDriver.BusyHandler(db: Psqlite;
  callback: Tsqlite_busy_callback; ptr: Pointer);
begin
  ZPlainSqLite28.sqlite_busy_handler(db, callback, ptr);
end;

procedure TZSQLite28PlainDriver.BusyTimeout(db: Psqlite; ms: Integer);
begin
  ZPlainSqLite28.sqlite_busy_timeout(db, ms);
end;

function TZSQLite28PlainDriver.Changes(db: Psqlite): Integer;
begin
  Result := ZPlainSqLite28.sqlite_changes(db);
end;

function TZSQLite28PlainDriver.CommitHook(db: Psqlite;
  callback: Tsqlite_simple_callback; ptr: Pointer): Pointer;
begin
  Result := ZPlainSqLite28.sqlite_commit_hook(db, callback, ptr);
end;

function TZSQLite28PlainDriver.Compile(db: Psqlite; const zSql: PChar;
      nBytes: Integer;var pzTail: PChar;
      var ppVm: Psqlite_vm; var pzErrmsg: PChar): Integer;
begin
  Result := ZPlainSqLite28.sqlite_compile(db, zSql, pzTail, ppVm, pzErrmsg);
end;

function TZSQLite28PlainDriver.Complete(const sql: PChar): Integer;
begin
  Result := ZPlainSqLite28.sqlite_complete(sql);
end;

function TZSQLite28PlainDriver.CreateAggregate(db: Psqlite;
  const zName: PChar; nArg: Integer; callback: Tsqlite_function_callback;
  finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
begin
  Result := ZPlainSqLite28.sqlite_create_aggregate(db, zName, nArg, callback,
    finalize, pUserData);
end;

function TZSQLite28PlainDriver.CreateFunction(db: Psqlite;
  const zName: PChar; nArg: Integer; callback: Tsqlite_function_callback;
  pUserData: Pointer): Integer;
begin
  Result := ZPlainSqLite28.sqlite_create_function(db, zName, nArg, callback,
    pUserData);
end;

function TZSQLite28PlainDriver.ErrorString(code: Integer): PChar;
begin
  Result := ZPlainSqLite28.sqlite_error_string(code);
end;

function TZSQLite28PlainDriver.Execute(db: Psqlite; const sql: PChar;
  sqlite_callback: Tsqlite_callback; arg: Pointer;
  var errmsg: PChar): Integer;
begin
  Result := ZPlainSqLite28.sqlite_exec(db, sql, sqlite_callback, arg, errmsg);
end;

function TZSQLite28PlainDriver.Finalize(vm: Psqlite_vm;
  var pzErrMsg: PChar): Integer;
begin
  Result := ZPlainSqLite28.sqlite_finalize(vm, pzErrMsg);
end;

procedure TZSQLite28PlainDriver.FreeMem(ptr: Pointer);
begin
  ZPlainSqLite28.sqlite_freemem(ptr);
end;

procedure TZSQLite28PlainDriver.FreeTable(var result: PChar);
begin
  ZPlainSqLite28.sqlite_free_table(result);
end;

function TZSQLite28PlainDriver.FunctionType(db: Psqlite;
  const zName: PChar; datatype: Integer): Integer;
begin
  Result := ZPlainSqLite28.sqlite_function_type(db, zName, datatype);
end;

function TZSQLite28PlainDriver.GetTable(db: Psqlite; const sql: PChar;
  var resultp: PPChar; var nrow, ncolumn: Integer;
  var errmsg: PChar): Integer;
begin
  Result := ZPlainSqLite28.sqlite_get_table(db, sql, resultp, nrow, ncolumn,
    errmsg);
end;

procedure TZSQLite28PlainDriver.Interrupt(db: Psqlite);
begin
  ZPlainSqLite28.sqlite_interrupt(db);
end;

function TZSQLite28PlainDriver.LastInsertRowId(db: Psqlite): Integer;
begin
  Result := ZPlainSqLite28.sqlite_last_insert_rowid(db);
end;

function TZSQLite28PlainDriver.LastStatementChanges(db: Psqlite): Integer;
begin
  Result := ZPlainSqLite28.sqlite_last_statement_changes(db);
end;

function TZSQLite28PlainDriver.LibEncoding: PChar;
begin
  Result := ZPlainSqLite28.sqlite_libencoding;
end;

function TZSQLite28PlainDriver.LibVersion: PChar;
begin
  Result := ZPlainSqLite28.sqlite_libversion;
end;

function TZSQLite28PlainDriver.Open(const filename: PChar; mode: Integer;
  var errmsg: PChar): Psqlite;
begin
  Result := ZPlainSqLite28.sqlite_open(filename, mode, errmsg);
end;

function TZSQLite28PlainDriver.OpenEncrypted(const zFilename, pKey: PChar;
  nKey: Integer; var pErrcode: Integer; var pzErrmsg: PChar): Psqlite;
begin
  Result := ZPlainSqLite28.sqlite_open_encrypted(zFilename, pKey, nKey,
    pErrcode, pzErrmsg);
end;

procedure TZSQLite28PlainDriver.ProgressHandler(db: Psqlite; p1: Integer;
  callback: Tsqlite_simple_callback; ptr: Pointer);
begin
  ZPlainSqLite28.sqlite_progress_handler(db, p1, callback, ptr);
end;

function TZSQLite28PlainDriver.ReKey(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  Result := ZPlainSqLite28.sqlite_rekey(db, pKey, nKey);
end;

function TZSQLite28PlainDriver.Reset(vm: Psqlite_vm;
  var pzErrMsg: PChar): Integer;
begin
  Result := ZPlainSqLite28.sqlite_reset(vm, pzErrMsg);
end;

function TZSQLite28PlainDriver.SetAuthorizer(db: Psqlite;
  callback: Tsqlite_auth_callback; pUserData: Pointer): Integer;
begin
  Result := ZPlainSqLite28.sqlite_set_authorizer(db, callback, pUserData);
end;

procedure TZSQLite28PlainDriver.SetResultDouble(func: Psqlite_func;
  arg: Double);
begin
  ZPlainSqLite28.sqlite_set_result_double(func, arg);
end;

procedure TZSQLite28PlainDriver.SetResultError(func: Psqlite_func;
  const arg: PChar; len: Integer);
begin
  ZPlainSqLite28.sqlite_set_result_error(func, arg, len);
end;

procedure TZSQLite28PlainDriver.SetResultInt(func: Psqlite_func;
  arg: Integer);
begin
  ZPlainSqLite28.sqlite_set_result_int(func, arg);
end;

function TZSQLite28PlainDriver.SetResultString(func: Psqlite_func;
  const arg: PChar; len: Integer): PChar;
begin
  Result := ZPlainSqLite28.sqlite_set_result_string(func, arg, len);
end;

function TZSQLite28PlainDriver.Step(pVm: Psqlite_vm; var pN: Integer;
  var pazValue, pazColName: PPChar): Integer;
begin
  Result := ZPlainSqLite28.sqlite_step(pVm, pN, pazValue, pazColName);
end;

function TZSQLite28PlainDriver.Trace(db: Psqlite;
  callback: Tsqlite_trace_callback; ptr: Pointer): Pointer;
begin
  Result := ZPlainSqLite28.sqlite_trace(db, callback, ptr);
end;

function TZSQLite28PlainDriver.UserData(func: Psqlite_func): Pointer;
begin
  Result := ZPlainSqLite28.sqlite_user_data(func);
end;

procedure TZSQLite28PlainDriver.Close(db: Psqlite);
begin
  ZPlainSqLite28.sqlite_close(db);
end;

{ TZSQLite3PlainDriver }

constructor TZSQLite3PlainDriver.Create;
begin
end;

function TZSQLite3PlainDriver.GetProtocol: string;
begin
  Result := 'sqlite-3';
end;

function TZSQLite3PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for SQLite 3';
end;

procedure TZSQLite3PlainDriver.Initialize;
begin
  ZPlainSQLite3.LibraryLoader.LoadIfNeeded;
end;

function TZSQLite3PlainDriver.AggregateContext(func: Psqlite_func;
  nBytes: Integer): Pointer;
begin
  Result := ZPlainSqLite3.sqlite_aggregate_context(func, nBytes);
end;

function TZSQLite3PlainDriver.AggregateCount(func: Psqlite_func): Integer;
begin
  Result := ZPlainSqLite3.sqlite_aggregate_count(func);
end;

function TZSQLite3PlainDriver.Bind(vm: Psqlite_vm; idx: Integer;
  const value: PChar; len, copy: Integer): Integer;
begin
  Result := ZPlainSqLite3.sqlite_bind(vm, idx, value, len, copy);
end;

procedure TZSQLite3PlainDriver.BusyHandler(db: Psqlite;
  callback: Tsqlite_busy_callback; ptr: Pointer);
begin
  ZPlainSqLite3.sqlite_busy_handler(db, callback, ptr);
end;

procedure TZSQLite3PlainDriver.BusyTimeout(db: Psqlite; ms: Integer);
begin
  ZPlainSqLite3.sqlite_busy_timeout(db, ms);
end;

function TZSQLite3PlainDriver.Changes(db: Psqlite): Integer;
begin
  Result := ZPlainSqLite3.sqlite_changes(db);
end;

function TZSQLite3PlainDriver.CommitHook(db: Psqlite;
  callback: Tsqlite_simple_callback; ptr: Pointer): Pointer;
begin
  Result := ZPlainSqLite3.sqlite_commit_hook(db, callback, ptr);
end;

function TZSQLite3PlainDriver.Compile(db: Psqlite; const zSql: PChar;
  nBytes: Integer;var pzTail: PChar;
  var ppVm: Psqlite_vm; var pzErrmsg: PChar): Integer;

begin
  Result := ZPlainSqLite3.sqlite_compile(db, zSql, -1, ppVm, nil);
  pzErrmsg := nil;
end;

function TZSQLite3PlainDriver.Complete(const sql: PChar): Integer;
begin
  Result := ZPlainSqLite3.sqlite_complete(sql);
end;

function TZSQLite3PlainDriver.CreateAggregate(db: Psqlite;
  const zName: PChar; nArg: Integer; callback: Tsqlite_function_callback;
  finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLite3PlainDriver.CreateFunction(db: Psqlite;
  const zName: PChar; nArg: Integer; callback: Tsqlite_function_callback;
  pUserData: Pointer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLite3PlainDriver.ErrorString(code: Integer): PChar;
begin
  Case code of
    SQLITE_OK:         Result := 'not an error';
    SQLITE_ERROR:      Result := 'SQL logic error or missing database';
    SQLITE_INTERNAL:   Result := 'internal SQLite implementation flaw';
    SQLITE_PERM:       Result := 'access permission denied';
    SQLITE_ABORT:      Result := 'callback requested query abort';
    SQLITE_BUSY:       Result := 'database is locked';
    SQLITE_LOCKED:     Result := 'database table is locked';
    SQLITE_NOMEM:      Result := 'out of memory';
    SQLITE_READONLY:   Result := 'attempt to write a readonly database';
    _SQLITE_INTERRUPT:  Result := 'interrupted';
    SQLITE_IOERR:      Result := 'disk I/O error';
    SQLITE_CORRUPT:    Result := 'database disk image is malformed';
    SQLITE_NOTFOUND:   Result := 'table or record not found';
    SQLITE_FULL:       Result := 'database is full';
    SQLITE_CANTOPEN:   Result := 'unable to open database file';
    SQLITE_PROTOCOL:   Result := 'database locking protocol failure';
    SQLITE_EMPTY:      Result := 'table contains no data';
    SQLITE_SCHEMA:     Result := 'database schema has changed';
    SQLITE_TOOBIG:     Result := 'too much data for one table row';
    SQLITE_CONSTRAINT: Result := 'constraint failed';
    SQLITE_MISMATCH:   Result := 'datatype mismatch';
    SQLITE_MISUSE:     Result := 'library routine called out of sequence';
    SQLITE_NOLFS:      Result := 'kernel lacks large file support';
    SQLITE_AUTH:       Result := 'authorization denied';
    SQLITE_FORMAT:     Result := 'auxiliary database format error';
    SQLITE_RANGE:      Result := 'bind index out of range';
    SQLITE_NOTADB:     Result := 'file is encrypted or is not a database';
    else                    Result := 'unknown error';
  end;
end;

function TZSQLite3PlainDriver.Execute(db: Psqlite; const sql: PChar;
  sqlite_callback: Tsqlite_callback; arg: Pointer;
  var errmsg: PChar): Integer;
begin
  errmsg:= nil;
  Result := ZPlainSqLite3.sqlite_exec(db, sql, sqlite_callback, arg, errmsg);
end;

function TZSQLite3PlainDriver.Finalize(vm: Psqlite_vm;
  var pzErrMsg: PChar): Integer;
begin
  Result:= ZPlainSqLite3.sqlite_finalize(vm); pzErrMsg:=nil;
end;

procedure TZSQLite3PlainDriver.FreeMem(ptr: Pointer);
begin
  ZPlainSqLite3.sqlite_freemem(ptr);
end;

procedure TZSQLite3PlainDriver.FreeTable(var result: PChar);
begin
  ZPlainSqLite3.sqlite_free_table(result);
end;

function TZSQLite3PlainDriver.FunctionType(db: Psqlite;
  const zName: PChar; datatype: Integer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLite3PlainDriver.GetTable(db: Psqlite; const sql: PChar;
  var resultp: PPChar; var nrow, ncolumn: Integer;
  var errmsg: PChar): Integer;
begin
  Result := ZPlainSqLite3.sqlite_get_table(db, sql, resultp, nrow, ncolumn,
    errmsg);
end;

procedure TZSQLite3PlainDriver.Interrupt(db: Psqlite);
begin
  ZPlainSqLite3.sqlite_interrupt(db);
end;

function TZSQLite3PlainDriver.LastInsertRowId(db: Psqlite): Integer;
begin
  Result := ZPlainSqLite3.sqlite_last_insert_rowid(db);
end;

function TZSQLite3PlainDriver.LastStatementChanges(db: Psqlite): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLite3PlainDriver.LibEncoding: PChar;
begin
  Result := nil;
end;

function TZSQLite3PlainDriver.LibVersion: PChar;
begin
  Result := ZPlainSqLite3.sqlite_libversion;
end;

function TZSQLite3PlainDriver.Open(const filename: PChar; mode: Integer;
  var errmsg: PChar): Psqlite;
var
  Result0: Psqlite;
begin
  Result0:= nil;
  ZPlainSqLite3.sqlite_open(filename, Result0);
  Result := Result0;
end;

function TZSQLite3PlainDriver.OpenEncrypted(const zFilename, pKey: PChar;
  nKey: Integer; var pErrcode: Integer; var pzErrmsg: PChar): Psqlite;
begin
  pErrcode := SQLITE_MISUSE;
  pzErrmsg := 'function is not used in the current version of the library';
  Result:= nil;
end;

procedure TZSQLite3PlainDriver.ProgressHandler(db: Psqlite; p1: Integer;
  callback: Tsqlite_simple_callback; ptr: Pointer);
begin
  ZPlainSqLite3.sqlite_progress_handler(db, p1, callback, ptr);
end;

function TZSQLite3PlainDriver.ReKey(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLite3PlainDriver.Reset(vm: Psqlite_vm;
  var pzErrMsg: PChar): Integer;
begin
  Result := ZPlainSqLite3.sqlite_reset(vm);
  pzErrMsg := nil;
end;

function TZSQLite3PlainDriver.SetAuthorizer(db: Psqlite;
  callback: Tsqlite_auth_callback; pUserData: Pointer): Integer;
begin
  Result := ZPlainSqLite3.sqlite_set_authorizer(db, callback, pUserData);
end;

procedure TZSQLite3PlainDriver.SetResultDouble(func: Psqlite_func;
  arg: Double);
begin
  ZPlainSqLite3.sqlite_set_result_double(func, arg);
end;

procedure TZSQLite3PlainDriver.SetResultError(func: Psqlite_func;
  const arg: PChar; len: Integer);
begin
  ZPlainSqLite3.sqlite_set_result_error(func, arg, len);
end;

procedure TZSQLite3PlainDriver.SetResultInt(func: Psqlite_func;
  arg: Integer);
begin
  ZPlainSqLite3.sqlite_set_result_int(func, arg);
end;

function TZSQLite3PlainDriver.SetResultString(func: Psqlite_func;
  const arg: PChar; len: Integer): PChar;
begin
  Result := ZPlainSqLite3.sqlite_set_result_string(func, arg, len, nil);
end;

function TZSQLite3PlainDriver.Step(pVm: Psqlite_vm; var pN: Integer;
  var pazValue, pazColName: PPChar): Integer;
var i: Integer;
    val,cname,ctype: PChar;
    pazValue0, pazColName0, pazColType: PPChar;
begin
  pazValue0 := nil; // satisfy compiler
  Result := ZPlainSqLite3.sqlite_step(pVm);
  if (Result=SQLITE_ROW) or (Result=SQLITE_DONE) then begin
    pN:= ZPlainSqLite3.sqlite_column_count(pVm);
    if  Result=SQLITE_ROW then begin
      pazValue:= AllocMem(SizeOf(PPChar)*pN+1);
      pazValue0:= pazValue;
    end;
    pazColName:= AllocMem(SizeOf(PPChar)*pN*2+1);
    pazColName0:= pazColName;
    pazColType:= pazColName;

    Inc(pazColType, pN);
//    for i:=1 to pN do
//      inc(pazColType);
    for i:=0 to pN-1 do begin
      if Result = SQLITE_ROW then
      begin
        val  := ZPlainSqLite3.sqlite_column_bytes(pVm, i);
        cname:= ZPlainSqLite3.sqlite_column_name(pVm, i);
        ctype:= ZPlainSqLite3.sqlite_column_decltype(pVm, i);
        pazValue0^  := val; inc(pazValue0);
      end
      else
      begin
        cname:= ZPlainSqLite3.sqlite_column_name(pVm, i);
        ctype:= ZPlainSqLite3.sqlite_column_decltype(pVm, i);
      end;
      pazColName0^:= cname;
      pazColType^ := ctype;
      inc(pazColName0);inc(pazColType);
    end;
    if  Result=SQLITE_ROW then pazValue0^:= nil;
    pazColType^:= nil;
    if Result=SQLITE_DONE then pazValue:= nil;
  end;
end;

function TZSQLite3PlainDriver.Trace(db: Psqlite;
  callback: Tsqlite_trace_callback; ptr: Pointer): Pointer;
begin
  Result := ZPlainSqLite3.sqlite_trace(db, callback, ptr);
end;

function TZSQLite3PlainDriver.UserData(func: Psqlite_func): Pointer;
begin
  Result := ZPlainSqLite3.sqlite_user_data(func);
end;

procedure TZSQLite3PlainDriver.Close(db: Psqlite);
begin
  ZPlainSqLite3.sqlite_close(db);
end;

end.

