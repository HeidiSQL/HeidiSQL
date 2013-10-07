/// SQLite3 embedded Database engine direct access
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynSQLite3;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


       SQLite3 3.7.14 database engine
      ********************************

     Brand new SQLite3 library to be used with Delphi
    - FLEXIBLE: in process, local or remote access (JSON RESTFUL HTTP server)
    - STANDARD: full UTF-8 and Unicode, SQLite3 engine (enhanced but not hacked)
    - SECURE: tested, multi-thread oriented, atomic commit, encryption ready
    - SIMPLE: staticaly linked into a single Delphi unit (no external dll required)
    - LIGHT: use native classes, not TDataSet nor TDataSource
    - SMART: queries share a JSON-based memory cache for immediate response
    - FAST: tuned pascal and i386 assembler code with use of FastMM4/SynScaleMM
    - FREE: full source code provided, with permissive licence

    - unitary tested with provided regression tests
    - includes RTREE extension for doing very fast range queries
    - can include FTS3 full text search engine (MATCH operator) after sqlite3.c
      recompile (by default, FTS3 is not compiled, saving more than 50KB of code)
    - uses only newest API (sqlite3_prepare_v2) and follow SQLite3 official documentation
    - uses purely UTF-8 encoded strings: Ansi/Unicode conversion routines included,
      Delphi 2009 ready (but Unicode works very well with older Delphi versions)
    - optional on the fly fast encryption of the data on disk
    - use an optional and efficient caching mechanism (TSynCache based) for the
      most used SELECT statements, using our TSQLTableJSON as fast data source
    - record retrieval from its ID is speed up via SQL statement preparation
    - uses ISO 8601:2004 format to properly handle date/time values in TEXT field
    - can be easily updated from official SQLite3 source code (see comments in
      the source code of this unit)
    - compiled without thread mutex: the caller has to be thread-safe aware
      (this is faster on most configuration, since mutex has to be acquired once):
      low level sqlite3_*() functions are not thread-safe, as TSQLRequest and
      TSQLBlobStream which just wrap them; but TSQLDataBase is thread-safe, as
      mORMot's TSQLTableDB/TSQLRestServerDB/TSQLRestClientDB which use TSQLDataBase
    - compiled with SQLITE_OMIT_SHARED_CACHE define
    - compatible with our LVCL 'Very LIGHT VCL routines' framework
      for building light but fast GUI servers software

  Initial version: 2008 March, by Arnaud Bouchez - as SQLite3.pas

  Version 1.15
  - first public release, corresponding to mORMot Framework 1.15
  - new unit extracting the SQLite3 wrapper from the previous SQLite3 unit:
    this unit can therefore be used with our SynDB classes (via SynDBSQLite3),
    without SQLite3Commons overhead (and features)
  - added TSQLRequest.BindNull method and associated sqlite3_bind_null function
  - fixed issue with TSQLDataBase with UseCache=false
  - new TSQLStatementCached object, for caching of prepared SQLite3 statements
  - TSQLDatabase constructors now accepts an optional Password parameter,
    associated to the supplied file name, in order to use database encryption

  Version 1.16
  - updated SQLite3 engine to version 3.7.12.1
  - unit now includes FTS3/FTS4 by default (i.e. INCLUDE_FTS3 conditional is
    set in both SQLite3.pas and SynSQLite3.pas units)
  - added sqlite3_changes() and sqlite3_total_changes() function prototypes
  - new TSQLDataBase.LastChangeCount method (wrapper around sqlite3_changes)
  - new IsSQLite3FileEncrypted() function
  - new TSQLRequest.FieldBlobToStream and Bind(TCustomMemoryStream) methods
  - new parameter in TSQLDataBase.ExecuteJSON, LockJSON, UnLockJSON methods,
    for an optional integer pointer, to return the count of row data
  - added an optional behavior parameter to TSQLDataBase.TransactionBegin method
  - reintroduce TSQLDataBaseSQLFunction.Create() constructor, and added some
    TSQLDataBase.RegisterSQLFunction() overloaded methods
  - fixed issue in TSQLRequest.Reset() which was triggered an error about the
    latest statement execution
  - fixed potential issue after TSQLStatementCached.ReleaseAllDBStatements
  - fixed rounding issue when exporting DOUBLE columns into JSON
  - fixed issue of unraised exception in TSQLRequest.PrepareNext
  - TSQLRequest.Execute(JSON: TStream) now allows field names at least, even
    with no data (as expected by TSQLRestClientURI.UpdateFromServer)
  - renamed ESQLException into ESQLite3Exception
  - engine is now compiled including tracing within the FTS3 extension - added
    sqlite3_trace() function prototype to register your own tracing callback

  Version 1.17
  - updated SQLite3 engine to version 3.7.14
  - allow compilation with Delphi 5
  - added TSQLDataBase.CacheFlush method (needed by SQLite3DB)
  - added TSQLDataBase.Synchronous and TSQLDataBase.WALMode properties
  - added TSQLDataBase.ExecuteNoException() overloaded methods
  - fixed ticket [8dc4d49ea9] in TSQLDataBase.GetFieldNames()about result array
    truncated to 64

    Todo:
    - port to systems other than Delphi+Win32 (use external DLL?)
}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
  Classes,
{$ifndef LVCL}
  Contnrs,
{$endif}
  SynCommons;

{.$define ENHANCEDRTL}
{ define this if you DID install our Enhanced Runtime library or LVCL
  - it's better to define this globaly in the Project/Options window }

{$ifdef UNICODE}
  {$undef ENHANCEDRTL} // Delphi 2009.. don't have our Enhanced Runtime library
{$endif}

{$define INCLUDE_FTS3}
{ define this if you want to include the FTS3/FTS4 feature into the library
  - FTS3 is an SQLite module implementing full-text search
  - will include also FTS4 extension module since 3.7.4
  - see http://www.sqlite.org/fts3.html for documentation
  - is defined by default, but can be unset to save about 50 KB of code size
  - should be defined for both SynSQLite3 and SQLite3 units }

{$ifdef INCLUDE_FTS3}
  {$define INCLUDE_TRACE} 
  { define this is you want to include the TRACE feature into the library
   - our C source code custom header will define SQLITE_OMIT_TRACE if FTS3/FST4
   is not defined }
{$endif}

{.$define USEFASTCALL}
{ use the fastcall calling convention to access the SQLite3 library
  - BCC32 -pr fastcall (=Delphi resgister) is buggy, don't know why
   (because of issues with BCC32 itself, or some obfuscated calls in source?)
  - should be defined for both SynSQLite3 and SQLite3 units }


{ ************ direct access to sqlite3.c / sqlite3.obj consts and functions }

type
  /// internaly store the SQLite3 database handle
  TSQLite3DB = type PtrUInt;

  {/ internaly store the SQLite3 statement handle
   - This object is variously known as a "prepared statement" or a "compiled
   SQL statement" or simply as a "statement".
   - Create the object using sqlite3_prepare_v2() or a related function.
   - Bind values to host parameters using the sqlite3_bind_*() interfaces.
   - Run the SQL by calling sqlite3_step() one or more times.
   - Reset the statement using sqlite3_reset() then go back to "Bind" step.
     Do this zero or more times.
   - Destroy the object using sqlite3_finalize(). }
  TSQLite3Statement = type PtrUInt;

  /// internaly store the SQLite3 blob handle
  TSQLite3Blob = type PtrUInt;

  /// internaly store a SQLite3 Dynamically Typed Value Object
  // - SQLite uses the sqlite3_value object to represent all values that
  // can be stored in a database table, which are mapped to this TSQLite3Value type
  // - SQLite uses dynamic typing for the values it stores
  // - Values stored in sqlite3_value objects can be integers, floating point
  // values, strings, BLOBs, or NULL
  TSQLite3Value = type PtrUInt;

  /// internal store a SQLite3 Function Context Object
  // - The context in which an SQL function executes is stored in an sqlite3_context
  // object, which is mapped to this TSQLite3FunctionContext type
  // - A pointer to an sqlite3_context object is always first parameter to
  // application-defined SQL functions, i.e. a TSQLFunctionFunc prototype
  TSQLite3FunctionContext = type PtrUInt;

  /// internaly store any array of  SQLite3 value
  TSQLite3ValueArray = array[0..63] of TSQLite3Value;

const
  {/ internal SQLite3 type as Integer }
  SQLITE_INTEGER = 1;
  {/ internal SQLite3 type as Floating point value }
  SQLITE_FLOAT = 2;
  {/ internal SQLite3 type as Text }
  SQLITE_TEXT = 3;
  {/ internal SQLite3 type as Blob }
  SQLITE_BLOB = 4;
  {/ internal SQLite3 type as NULL }
  SQLITE_NULL = 5;

  {/ text is UTF-8 encoded }
  SQLITE_UTF8     = 1;
  {/ text is UTF-16 LE encoded }
  SQLITE_UTF16LE  = 2;
  {/ text is UTF-16 BE encoded }
  SQLITE_UTF16BE  = 3;
  {/ text is UTF-16 encoded, using the system native byte order }
  SQLITE_UTF16    = 4;
  {/ sqlite3_create_function_v2 don't care about text encoding }
  SQLITE_ANY      = 5;
  {/ used by sqlite3_create_collation() only }
  SQLITE_UTF16_ALIGNED = 8;


  {/ sqlite_exec() return code: no error occured }
  SQLITE_OK = 0;
  {/ sqlite_exec() return code: SQL error or missing database - legacy generic code }
  SQLITE_ERROR = 1;
  {/ sqlite_exec() return code: An internal logic error in SQLite  }
  SQLITE_INTERNAL = 2;
  {/ sqlite_exec() return code: Access permission denied  }
  SQLITE_PERM = 3;
  {/ sqlite_exec() return code: Callback routine requested an abort  }
  SQLITE_ABORT = 4;
  {/ sqlite_exec() return code: The database file is locked  }
  SQLITE_BUSY = 5;
  {/ sqlite_exec() return code: A table in the database is locked  }
  SQLITE_LOCKED = 6;
  {/ sqlite_exec() return code: A malloc() failed  }
  SQLITE_NOMEM = 7;
  {/ sqlite_exec() return code: Attempt to write a readonly database  }
  SQLITE_READONLY = 8;
  {/ sqlite_exec() return code: Operation terminated by sqlite3_interrupt() }
  SQLITE_INTERRUPT = 9;
  {/ sqlite_exec() return code: Some kind of disk I/O error occurred  }
  SQLITE_IOERR = 10;
  {/ sqlite_exec() return code: The database disk image is malformed  }
  SQLITE_CORRUPT = 11;
  {/ sqlite_exec() return code: (Internal Only) Table or record not found  }
  SQLITE_NOTFOUND = 12;
  {/ sqlite_exec() return code: Insertion failed because database is full  }
  SQLITE_FULL = 13;
  {/ sqlite_exec() return code: Unable to open the database file  }
  SQLITE_CANTOPEN = 14;
  {/ sqlite_exec() return code: (Internal Only) Database lock protocol error  }
  SQLITE_PROTOCOL = 15;
  {/ sqlite_exec() return code: Database is empty  }
  SQLITE_EMPTY = 16;
  {/ sqlite_exec() return code: The database schema changed, and unable to be recompiled }
  SQLITE_SCHEMA = 17;
  {/ sqlite_exec() return code: Too much data for one row of a table  }
  SQLITE_TOOBIG = 18;
  {/ sqlite_exec() return code: Abort due to contraint violation  }
  SQLITE_CONSTRAINT = 19;
  {/ sqlite_exec() return code: Data type mismatch  }
  SQLITE_MISMATCH = 20;
  {/ sqlite_exec() return code: Library used incorrectly  }
  SQLITE_MISUSE = 21;
  {/ sqlite_exec() return code: Uses OS features not supported on host  }
  SQLITE_NOLFS = 22;
  {/ sqlite_exec() return code: Authorization denied  }
  SQLITE_AUTH = 23;
  {/ sqlite_exec() return code: Auxiliary database format error  }
  SQLITE_FORMAT = 24;
  {/ sqlite_exec() return code: 2nd parameter to sqlite3_bind out of range  }
  SQLITE_RANGE = 25;
  {/ sqlite_exec() return code: File opened that is not a database file  }
  SQLITE_NOTADB = 26;

  {/ sqlite3_step() return code: another result row is ready  }
  SQLITE_ROW = 100;
  {/ sqlite3_step() return code: has finished executing  }
  SQLITE_DONE = 101;

  /// DestroyPtr set to SQLITE_STATIC if data is constant and will never change
  // - SQLite assumes that the text or BLOB result is in constant space and
  // does not copy the content of the parameter nor call a destructor on the
  // content when it has finished using that result
  SQLITE_STATIC = pointer(0);
  /// DestroyPtr set to SQLITE_TRANSIENT for SQLite3 to make a private copy of
  // the data into space obtained from from sqlite3_malloc() before it returns
  // - this is the default behavior in our framework
  SQLITE_TRANSIENT = pointer(-1);



{/ initialize the SQLite3 database code
  - automaticaly called by the initialization block of this unit
  - so sqlite3.c is compiled with SQLITE_OMIT_AUTOINIT defined }
function sqlite3_initialize: integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ shutdown the SQLite3 database core
  - automaticaly called by the finalization block of this unit }
function sqlite3_shutdown: integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;


{/ Open a SQLite3 database filename, creating a DB handle
  - filename must be UTF-8 encoded (filenames containing international
    characters must be converted to UTF-8 prior to passing them)
  - allocate a sqlite3 object, and return its handle in DB
  - return SQLITE_OK on success
  - an error code (see SQLITE_* const) is returned otherwize - sqlite3_errmsg()
    can be used to obtain an English language description of the error
 - Whatever or not an error occurs when it is opened, resources associated with
   the database connection handle should be released by passing it to
   sqlite3_close() when it is no longer required }
function sqlite3_open(filename: PUTF8Char; var DB: TSQLite3DB): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;


type
  /// type for a custom destructor for the text or BLOB content
  // - set to @sqlite3InternalFree if a Value must be released via Freemem()
  // - set to @sqlite3InternalFreeObject if a vale must be released via
  // TObject(p).Free
  TSQLDestroyPtr = procedure(p: pointer); {$ifndef USEFASTCALL}cdecl;{$endif}

  /// SQLite3 collation (i.e. sort and comparaison) function prototype
  // - this function MUST use s1Len and s2Len parameters during the comparaison:
  // s1 and s2 are not zero-terminated
  // - used by sqlite3_create_collation low-level function 
  TSQLCollateFunc = function(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; {$ifndef USEFASTCALL}cdecl;{$endif}

  /// SQLite3 user function or aggregate callback prototype
  // - argc is the number of supplied parameters, which are available in argv[]
  // (you can call ErrorWrongNumberOfArgs(Context) in case of unexpected number) 
  // - use sqlite3_value_*(argv[*]) functions to retrieve a parameter value
  // - then set the result using sqlite3_result_*(Context,*) functions
  TSQLFunctionFunc = procedure(Context: TSQLite3FunctionContext;
    argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}

  /// SQLite3 user final aggregate callback prototype
  TSQLFunctionFinal = procedure(Context: TSQLite3FunctionContext); {$ifndef USEFASTCALL}cdecl;{$endif}

  {/ SQLite3 callback prototype to handle SQLITE_BUSY errors
  - The first argument to the busy handler is a copy of the user pointer which
    is the third argument to sqlite3_busy_handler().
  - The second argument to the busy handler callback is the number of times
    that the busy handler has been invoked for this locking event.
  - If the busy callback returns 0, then no additional attempts are made to
    access the database and SQLITE_BUSY or SQLITE_IOERR_BLOCKED is returned.
  - If the callback returns non-zero, then another attempt is made to open
    the database for reading and the cycle repeats. }
  TSQLBusyHandler = function(user: pointer; count: integer): integer;
     {$ifndef USEFASTCALL}cdecl;{$endif}

  {$A-}
  PFTSMatchInfo = ^TFTSMatchInfo;
  /// map the matchinfo function returned BLOB value
  // - i.e. the default 'pcx' layout, for both FTS3 and FTS4
  // - see http://www.sqlite.org/fts3.html#matchinfo
  // - used for the FTS3/FTS4 ranking of results by TSQLRest.FTSMatch method
  // and the internal RANK() function as proposed in
  // http://www.sqlite.org/fts3.html#appendix_a
  TFTSMatchInfo = record
    nPhrase: integer;
    nCol: integer;
    hits: array[1..9] of record
      this_row: integer;
      all_rows: integer;
      docs_with_hits: integer;
    end;
  end;
  {$A+}


{/ Function creation routine used to add SQL functions or aggregates or to redefine
   the behavior of existing SQL functions or aggregates
  - The first parameter is the database connection to which the SQL function is
    to be added. If an application uses more than one database connection then
    application-defined SQL functions must be added to each database connection
    separately.
  - The second parameter is the name of the SQL function to be created or redefined.
    The length of the name is limited to 255 bytes in a UTF-8 representation,
    exclusive of the zero-terminator. Note that the name length limit is in
    UTF-8 bytes, not characters nor UTF-16 bytes. Any attempt to create a
    function with a longer name will result in SQLITE_MISUSE being returned.
  - The third parameter (nArg) is the number of arguments that the SQL function
    or aggregate takes. If the third parameter is less than -1 or greater than
    127 then the behavior is undefined.
  - The fourth parameter, eTextRep, specifies what text encoding this SQL
    function prefers for its parameters. Every SQL function implementation must
    be able to work with UTF-8, UTF-16le, or UTF-16be. But some implementations
    may be more efficient with one encoding than another. When multiple
    implementations of the same function are available, SQLite will pick the one
    that involves the least amount of data conversion. If there is only a single
    implementation which does not care what text encoding is used, then the
    fourth argument should be SQLITE_ANY.
  - The fifth parameter, pApp, is an arbitrary pointer. The implementation of the
    function can gain access to this pointer using sqlite3_user_data().
  - The seventh, eighth and ninth parameters, xFunc, xStep and xFinal, are
    pointers to C-language functions that implement the SQL function or aggregate.
    A scalar SQL function requires an implementation of the xFunc callback only;
    nil pointers must be passed as the xStep and xFinal parameters. An aggregate
    SQL function requires an implementation of xStep and xFinal and nil pointer
    must be passed for xFunc. To delete an existing SQL function or aggregate,
    pass nil pointers for all three function callbacks.
  - If the tenth parameter is not NULL, then it is invoked when the function is
    deleted, either by being overloaded or when the database connection closes.
    When the destructure callback of the tenth parameter is invoked, it is
    passed a single argument which is a copy of the pointer which was the fifth
    parameter to sqlite3_create_function_v2().
  - It is permitted to register multiple implementations of the same functions
    with the same name but with either differing numbers of arguments or
    differing preferred text encodings. SQLite will use the implementation
    that most closely matches the way in which the SQL function is used. }
function sqlite3_create_function_v2(DB: TSQLite3DB; FunctionName: PUTF8Char;
  nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSQLFunctionFunc;
  xFinal: TSQLFunctionFinal; xDestroy: TSQLDestroyPtr): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Define New Collating Sequences
  - add new collation sequences to the database connection specified
  - collation name is to be used in CREATE TABLE t1 (a COLLATE CollationName);
   or in SELECT * FROM t1 ORDER BY c COLLATE CollationName;
  - StringEncoding is either SQLITE_UTF8 either SQLITE_UTF16
  - TSQLDataBase.Create add WIN32CASE, WIN32NOCASE and ISO8601 collations }
function sqlite3_create_collation(DB: TSQLite3DB; CollationName: PUTF8Char;
  StringEncoding: integer; CollateParam: pointer; cmp: TSQLCollateFunc): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Destructor for the sqlite3 object, which handle is DB
  - Applications should finalize all prepared statements and close all BLOB handles
    associated with the sqlite3 object prior to attempting to close the object
    (sqlite3_next_stmt() interface can be used for this task)
  - if invoked while a transaction is open, the transaction is automatically rolled back }
function sqlite3_close(DB: TSQLite3DB): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Return the version of the SQLite database engine, in ascii format
  - currently returns '3.7.14' }
function sqlite3_libversion: PUTF8Char; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Returns English-language text that describes an error,
   using UTF-8 encoding (which, with English text, is the same as Ansi).
  - Memory to hold the error message string is managed internally.
     The application does not need to worry about freeing the result.
     However, the error string might be overwritten or deallocated by
     subsequent calls to other SQLite interface functions. }
function sqlite3_errmsg(DB: TSQLite3DB): PAnsiChar; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Returns the rowid of the most recent successful INSERT into the database }
function sqlite3_last_insert_rowid(DB: TSQLite3DB): Int64; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Set A Busy Timeout
 - This routine sets a busy handler that sleeps for a specified amount of time
  when a table is locked. The handler will sleep multiple times until at least
  "ms" milliseconds of sleeping have accumulated. After at least "ms" milliseconds
  of sleeping, the handler returns 0 which causes sqlite3_step() to return
  SQLITE_BUSY or SQLITE_IOERR_BLOCKED.
 - Calling this routine with an argument less than or equal to zero turns off
  all busy handlers.
 -  There can only be a single busy handler for a particular database connection
  any given moment. If another busy handler was defined (using
  sqlite3_busy_handler()) prior to calling this routine, that other busy handler
  is cleared. }
function sqlite3_busy_timeout(DB: TSQLite3DB; Milliseconds: integer): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Register A Callback To Handle SQLITE_BUSY Errors
 - This routine sets a callback function that might be invoked whenever an
  attempt is made to open a database table that another thread or process has locked.
 - If the busy callback is NULL, then SQLITE_BUSY or SQLITE_IOERR_BLOCKED is
  returned immediately upon encountering the lock. If the busy callback is not
  NULL, then the callback might be invoked with two arguments.
 - The default busy callback is NULL. }
function sqlite3_busy_handler(DB: TSQLite3DB;
  CallbackPtr: TSQLBusyHandler; user: Pointer): integer;  {$ifndef USEFASTCALL}cdecl;{$endif} external;
                  
{/ Convenience Routines For Running Queries
  - fill Table with all Row/Col for the SQL query
  - use sqlite3_free_table() to release memory }
function sqlite3_get_table(DB: TSQLite3DB; SQL: PUTF8Char; var Table: PPUTF8CharArray;
  var ResultRow, ResultCol: integer; var Error: PUTF8Char): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ release memory allocated for a sqlite3_get_table() result }
procedure sqlite3_free_table(Table: PPUTF8CharArray); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ One-Step Query Execution Interface }
function sqlite3_exec(DB: TSQLite3DB; SQL: PUTF8Char; CallBack, Args: pointer; Error: PUTF8Char): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Compile a SQL query into byte-code
  - SQL must contains an UTF8-encoded null-terminated string query
  - SQL_bytes contains -1 (to stop at the null char) or the number of bytes in
   the input string, including the null terminator
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - S will contain an handle of the resulting statement (an opaque sqlite3_stmt
   object) on success, or will 0 on error - the calling procedure is responsible
   for deleting the compiled SQL statement using sqlite3_finalize() after it has
   finished with it
  - in this "v2" interface, the prepared statement that is returned contains a
   copy of the original SQL text
  - this routine only compiles the first statement in SQL, so SQLtail is left pointing
   to what remains uncompiled }
function sqlite3_prepare_v2(DB: TSQLite3DB; SQL: PUTF8Char; SQL_bytes: integer;
  var S: TSQLite3Statement; var SQLtail: PUTF8Char): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Delete a previously prepared statement
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - this routine can be called at any point during the execution of the prepared
   statement. If the virtual machine has not completed execution when this routine
   is called, that is like encountering an error or an interrupt. Incomplete updates
   may be rolled back and transactions canceled, depending on the circumstances,
   and the error code returned will be SQLITE_ABORT }
function sqlite3_finalize(S: TSQLite3Statement): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Find the next prepared statement
  - this interface returns a handle to the next prepared statement after S,
   associated with the database connection DB.
  - if S is 0 then this interface returns a pointer to the first prepared
   statement associated with the database connection DB.
  - if no prepared statement satisfies the conditions of this routine, it returns 0 }
function sqlite3_next_stmt(DB: TSQLite3DB; S: TSQLite3Statement): TSQLite3Statement; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Reset a prepared statement object back to its initial state, ready to be re-Prepared
  - if the most recent call to sqlite3_step(S) returned SQLITE_ROW or SQLITE_DONE,
   or if sqlite3_step(S) has never before been called with S, then sqlite3_reset(S)
   returns SQLITE_OK.
  - return an appropriate error code if the most recent call to sqlite3_step(S) failed
  - any SQL statement variables that had values bound to them using the sqlite3_bind_*()
   API retain their values. Use sqlite3_clear_bindings() to reset the bindings. }
function sqlite3_reset(S: TSQLite3Statement): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/  returns true (non-zero) if and only if the prepared statement X
  makes no direct changes to the content of the database file
 - Transaction control statements such as BEGIN, COMMIT, ROLLBACK, SAVEPOINT,
   and RELEASE cause sqlite3_stmt_readonly() to return true, since the statements
   themselves do not actually modify the database but rather they control the
   timing of when other statements modify the database. The ATTACH and DETACH
   statements also cause sqlite3_stmt_readonly() to return true since, while
   those statements change the configuration of a database connection, they
   do not make changes to the content of the database files on disk. }
function sqlite3_stmt_readonly(S: TSQLite3Statement): boolean; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Evaluate An SQL Statement, returning a result status:
  - SQLITE_BUSY means that the database engine was unable to acquire the database
   locks it needs to do its job. If the statement is a COMMIT or occurs outside of
   an explicit transaction, then you can retry the statement. If the statement
   is not a COMMIT and occurs within a explicit transaction then you should
   rollback the transaction before continuing.
  - SQLITE_DONE means that the statement has finished executing successfully.
   sqlite3_step() should not be called again on this virtual machine without
   first calling sqlite3_reset() to reset the virtual machine state back.
  - SQLITE_ROW is returned each time a new row of data is ready for processing by
   the caller. The values may be accessed using the column access functions below.
   sqlite3_step() has to be called again to retrieve the next row of data.
  - SQLITE_MISUSE means that the this routine was called inappropriately. Perhaps
   it was called on a prepared statement that has already been finalized or on
   one that had previously returned SQLITE_ERROR or SQLITE_DONE. Or it could be
   the case that the same database connection is being used by two or more threads
   at the same moment in time.
  - SQLITE_SCHEMA means that the database schema changes, and the SQL statement
   has been recompiled and run again, but the schame changed in a way that makes
   the statement no longer valid, as a fatal error.
  - another specific error code is returned on fatal error }
function sqlite3_step(S: TSQLite3Statement): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ number of columns in the result set for the statement }
function sqlite3_column_count(S: TSQLite3Statement): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ datatype code for the initial data type of a result column
  - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
   SQLITE_BLOB or SQLITE_NULL
  - S is the SQL statement, after sqlite3_step(S) returned SQLITE_ROW
  - Col is the column number, indexed from 0 to sqlite3_column_count(S)-1
  - must be called before any sqlite3_column_*() statement, which may result in
   an implicit type conversion: in this case, value is undefined }
function sqlite3_column_type(S: TSQLite3Statement; Col: integer): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ returns a zero-terminated UTF-8 string containing the declared datatype of a result column }
function sqlite3_column_decltype(S: TSQLite3Statement; Col: integer): PAnsiChar; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ returns the name of a result column as a zero-terminated UTF-8 string }
function sqlite3_column_name(S: TSQLite3Statement; Col: integer): PUTF8Char; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ number of bytes for a BLOB or UTF-8 string result
  - S is the SQL statement, after sqlite3_step(S) returned SQLITE_ROW
  - Col is the column number, indexed from 0 to sqlite3_column_count(S)-1
  - an implicit conversion into UTF-8 text is made for a numeric value or
    UTF-16 column: you must call sqlite3_column_text() or sqlite3_column_blob()
    before calling sqlite3_column_bytes() to perform the conversion itself }
function sqlite3_column_bytes(S: TSQLite3Statement; Col: integer): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ get the value handle of the Col column in the current row of prepared statement S
  - this handle represent a sqlite3_value object
  - this handle can then be accessed with any sqlite3_value_*() function below }
function sqlite3_column_value(S: TSQLite3Statement; Col: integer): TSQLite3Value; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts the Col column in the current row prepared statement S
  into a floating point value and returns a copy of that value
  - NULL is converted into 0.0
  - INTEGER is converted into corresponding floating point value
  - TEXT or BLOB is converted from all correct ASCII numbers with 0.0 as default }
function sqlite3_column_double(S: TSQLite3Statement; Col: integer): double; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts the Col column in the current row prepared statement S
  into a 32 bit integer value and returns a copy of that value
  - NULL is converted into 0
  - FLOAT is truncated into corresponding integer value
  - TEXT or BLOB is converted from all correct ASCII numbers with 0 as default }
function sqlite3_column_int(S: TSQLite3Statement; Col: integer): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts the Col column in the current row prepared statement S
  into a 64 bit integer value and returns a copy of that value
  - NULL is converted into 0
  - FLOAT is truncated into corresponding integer value
  - TEXT or BLOB is converted from all correct ASCII numbers with 0 as default }
function sqlite3_column_int64(S: TSQLite3Statement; Col: integer): int64; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts the Col column in the current row prepared statement S
  into a zero-terminated UTF-8 string and returns a pointer to that string
  - NULL is converted into nil
  - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
  - TEXT is returned directly (with UTF-16 -> UTF-8 encoding if necessary)
  - BLOB add a zero terminator if needed }
function sqlite3_column_text(S: TSQLite3Statement; Col: integer): PUTF8Char; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts the Col column in the current row of prepared statement S
  into a BLOB and then returns a pointer to the converted value
  - NULL is converted into nil
  - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
  - TEXT and BLOB are returned directly }
function sqlite3_column_blob(S: TSQLite3Statement; Col: integer): PAnsiChar; {$ifndef USEFASTCALL}cdecl;{$endif} external;


{/ datatype code for a sqlite3_value object, specified by its handle
  - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
   SQLITE_BLOB or SQLITE_NULL
  - must be called before any sqlite3_value_*() statement, which may result in
   an implicit type conversion: in this case, value is undefined }
function sqlite3_value_type(Value: TSQLite3Value): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ attempts to apply numeric affinity to the value
  - This means that an attempt is made to convert the value to an integer or
  floating point. If such a conversion is possible without loss of information
  (in other words, if the value is a string that looks like a number) then the
  conversion is performed. Otherwise no conversion occurs. The datatype after
  conversion is returned.
  - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
   SQLITE_BLOB or SQLITE_NULL }
function sqlite3_value_numeric_type(Value: TSQLite3Value): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ number of bytes for a sqlite3_value object, specified by its handle
  - used after a call to sqlite3_value_text() or sqlite3_value_blob()
  to determine buffer size (in bytes) }
function sqlite3_value_bytes(Value: TSQLite3Value): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts a sqlite3_value object, specified by its handle,
  into a floating point value and returns a copy of that value }
function sqlite3_value_double(Value: TSQLite3Value): double; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts a sqlite3_value object, specified by its handle,
  into an integer value and returns a copy of that value }
function sqlite3_value_int64(Value: TSQLite3Value): Int64; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts a sqlite3_value object, specified by its handle,
  into an UTF-8 encoded string, and returns a copy of that value }
function sqlite3_value_text(Value: TSQLite3Value): PUTF8Char; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ converts a sqlite3_value object, specified by its handle,
  into a blob memory, and returns a copy of that value }
function sqlite3_value_blob(Value: TSQLite3Value): pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external;


{/ sets the return value of the application-defined function to be NULL}
procedure sqlite3_result_null(Context: TSQLite3FunctionContext); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ sets the return value of the application-defined function to be the 64-bit
   signed integer value given in the 2nd argument }
procedure sqlite3_result_int64(Context: TSQLite3FunctionContext; Value: Int64); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ sets the result from an application-defined function to be a floating point
  value specified by its 2nd argument }
procedure sqlite3_result_double(Context: TSQLite3FunctionContext; Value: double); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ sets the result from an application-defined function to be the BLOB
  - content is pointed to by the Value and which is Value_bytes bytes long
  - set DestroyPtr as SQLITE_STATIC (nil) for static binding
  - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    copy of the data (this is the prefered way in our Framework)
  - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    or to @sqlite3InternalFreeObject if Value must be released via a Free method }
procedure sqlite3_result_blob(Context: TSQLite3FunctionContext; Value: Pointer;
  Value_bytes: Integer=0; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ an internal function which calls Freemem(p)
  - can be used to free some PUTF8Char pointer allocated by Delphi Getmem() }
procedure sqlite3InternalFree(p: pointer); {$ifndef USEFASTCALL}cdecl;{$endif}

{/ an internal function which calls TObject(p).Free
  - can be used to free some Delphi class instance }
procedure sqlite3InternalFreeObject(p: pointer); {$ifndef USEFASTCALL}cdecl;{$endif}

{/ sets the return value of the application-defined function to be a text string
  which is represented as UTF-8
  - if Value_bytes is negative, then SQLite takes result text from the Value
    parameter through the first zero character
  - if Value_bytes is non-negative, then as many bytes (NOT characters: this
    parameter must include the #0 terminator) of the text pointed to by the
    Value parameter are taken as the application-defined function result
  - set DestroyPtr as SQLITE_STATIC (nil) for static binding
  - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    copy of the data (this is the prefered way in our Framework)
  - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    or to @sqlite3InternalFreeObject if Value must be released via a Free method }
procedure sqlite3_result_text(Context: TSQLite3FunctionContext; Value: PUTF8Char;
  Value_bytes: Integer=-1; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ sets the result of the application-defined function to be a copy the unprotected
  sqlite3_value object specified by the 2nd parameter
  - The sqlite3_result_value() interface makes a copy of the sqlite3_value so
  that the sqlite3_value specified in the parameter may change or be deallocated
  after sqlite3_result_value() returns without harm }
procedure sqlite3_result_value(Context: TSQLite3FunctionContext; Value: TSQLite3Value); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ cause the implemented SQL function to throw an exception
 - SQLite interprets the error message string from sqlite3_result_error() as UTF-8
 - if MsgLen is negative, Msg must be #0 ended, or MsgLen must tell the numnber of
   characters in the Msg UTF-8 buffer } 
procedure sqlite3_result_error(Context: TSQLite3FunctionContext; Msg: PUTF8Char; MsgLen: integer=-1); {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ wrapper around sqlite3_result_error() to be called if wrong number of arguments }
procedure ErrorWrongNumberOfArgs(Context: TSQLite3FunctionContext);

{/ returns a copy of the pointer that was the pUserData parameter (the 5th
  parameter) of the sqlite3_create_function_v2() routine that originally
  registered the application defined function
  - This routine must be called from the same thread in which the
    application-defined function is running }
function sqlite3_user_data(Context: TSQLite3FunctionContext): pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ returns a copy of the pointer to the database connection (the 1st parameter)
  of the sqlite3_create_function_v2() routine that originally registered the
  application defined function }
function sqlite3_context_db_handle(Context: TSQLite3FunctionContext): TSQLite3DB; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Implementations of aggregate SQL functions use this routine to allocate
 memory for storing their state.
 - The first time the sqlite3_aggregate_context(C,N) routine is called for a
 particular aggregate function, SQLite allocates N of memory, zeroes out that
 memory, and returns a pointer to the new memory. On second and subsequent calls
 to sqlite3_aggregate_context() for the same aggregate function instance, the
 same buffer is returned. Sqlite3_aggregate_context() is normally called once
 for each invocation of the xStep callback and then one last time when the
 xFinal callback is invoked. When no rows match an aggregate query, the xStep()
 callback of the aggregate function implementation is never called and xFinal()
 is called exactly once. In those cases, sqlite3_aggregate_context() might be
 called for the first time from within xFinal().
 - The sqlite3_aggregate_context(C,N) routine returns a NULL pointer if N is
 less than or equal to zero or if a memory allocate error occurs.
 - The amount of space allocated by sqlite3_aggregate_context(C,N) is
 determined by the N parameter on first successful call. Changing the value
 of N in subsequent call to sqlite3_aggregate_context() within the same
 aggregate function instance will not resize the memory allocation.
 - SQLite automatically frees the memory allocated by sqlite3_aggregate_context()
 when the aggregate query concludes. }
function sqlite3_aggregate_context(Context: TSQLite3FunctionContext;
   nBytes: integer): pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external;


{/ Bind a Text Value to a parameter of a prepared statement
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - S is a statement prepared by a previous call to sqlite3_prepare_v2()
  - Param is the index of the SQL parameter to be set. The leftmost SQL parameter
   has an index of 1.
  - Text must contains an UTF8-encoded null-terminated string query
  - Text_bytes contains -1 (to stop at the null char) or the number of chars
    in the input string, excluding the null terminator
  - set DestroyPtr as SQLITE_STATIC (nil) for static binding
  - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    copy of the data (this is the prefered way in our Framework)
  - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem() }
function sqlite3_bind_text(S: TSQLite3Statement; Param: integer;
  Text: PUTF8Char; Text_bytes: integer=-1; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;
  // note that the official SQLite3 documentation could lead into misunderstanding:
  // Text_bytes must EXCLUDE the null terminator, otherwise a #0 is appended to
  // all column values

{/ Bind a Blob Value to a parameter of a prepared statement
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - S is a statement prepared by a previous call to sqlite3_prepare_v2()
  - Param is the index of the SQL parameter to be set (leftmost=1)
  - Buf must point to a memory buffer of Buf_bytes bytes
  - Buf_bytes contains the number of bytes in Buf
  - set DestroyPtr as SQLITE_STATIC (nil) for static binding
  - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    copy of the data (this is the prefered way in our Framework)
  - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem() }
function sqlite3_bind_blob(S: TSQLite3Statement; Param: integer; Buf: pointer; Buf_bytes: integer;
  DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ bind a ZeroBlob buffer to a parameter
  - uses a fixed amount of memory (just an integer to hold its size) while
   it is being processed. Zeroblobs are intended to serve as placeholders
   for BLOBs whose content is later written using incremental BLOB I/O routines.
  - a negative value for the Size parameter results in a zero-length BLOB
  - the leftmost SQL parameter has an index of 1, but ?NNN may override it }
function sqlite3_bind_zeroblob(S: TSQLite3Statement; Param: integer; Size: integer): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Bind a floating point Value to a parameter of a prepared statement
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - S is a statement prepared by a previous call to sqlite3_prepare_v2()
  - Param is the index of the SQL parameter to be set (leftmost=1)
  - Value is the floating point number to bind }
function sqlite3_bind_double(S: TSQLite3Statement; Param: integer; Value: double): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Bind a 32 bits Integer Value to a parameter of a prepared statement
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - S is a statement prepared by a previous call to sqlite3_prepare_v2()
  - Param is the index of the SQL parameter to be set (leftmost=1)
  - Value is the 32 bits Integer to bind }
function sqlite3_bind_Int(S: TSQLite3Statement; Param: integer; Value: integer): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Bind a 64 bits Integer Value to a parameter of a prepared statement
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - S is a statement prepared by a previous call to sqlite3_prepare_v2()
  - Param is the index of the SQL parameter to be set (leftmost=1)
  - Value is the 64 bits Integer to bind }
function sqlite3_bind_Int64(S: TSQLite3Statement; Param: integer; Value: Int64): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Bind a NULL Value to a parameter of a prepared statement
  - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3_errmsg()
  - S is a statement prepared by a previous call to sqlite3_prepare_v2()
  - Param is the index of the SQL parameter to be set (leftmost=1) }
function sqlite3_bind_null(S: TSQLite3Statement; Param: integer): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Reset All Bindings On A Prepared Statement }
function sqlite3_clear_bindings(S: TSQLite3Statement): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Number Of SQL Parameters for a prepared statement
 - returns the index of the largest (rightmost) parameter. For all forms
  except ?NNN, this will correspond to the number of unique parameters.
  If parameters of the ?NNN are used, there may be gaps in the list. }
function sqlite3_bind_parameter_count(S: TSQLite3Statement): integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;


{/ Open a BLOB For Incremental I/O
  - returns a BLOB handle for row RowID, column ColumnName, table TableName
    in database DBName; in other words, the same BLOB that would be selected by:
  ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID; }
function sqlite3_blob_open(DB: TSQLite3DB; DBName, TableName, ColumnName: PUTF8Char;
  RowID: Int64; Flags: Integer; var Blob: TSQLite3Blob): Integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Close A BLOB Handle }
function sqlite3_blob_close(Blob: TSQLite3Blob): Integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Read Data From a BLOB Incrementally }
function sqlite3_blob_read(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Write Data To a BLOB Incrementally }
function sqlite3_blob_write(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Return The Size Of An Open BLOB }
function sqlite3_blob_bytes(Blob: TSQLite3Blob): Integer; {$ifndef USEFASTCALL}cdecl;{$endif} external;


const
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;

type
  PSQLite3Module = ^TSQLite3Module;
  PSQLite3VTab = ^TSQLite3VTab;
  PSQLite3VTabCursor = ^TSQLite3VTabCursor;

  /// records WHERE clause constraints of the form "column OP expr"
  // - Where "column" is a column in the virtual table, OP is an operator like
  // "=" or "<", and EXPR is an arbitrary expression
  // - So, for example, if the WHERE clause contained a term like this:
  // $ a = 5
  // Then one of the constraints would be on the "a" column with operator "="
  // and an expression of "5"
  // - For example, if the WHERE clause contained something like this:
  // $  x BETWEEN 10 AND 100 AND 999>y
  // The query optimizer might translate this into three separate constraints:
  // ! x >= 10
  // ! x <= 100
  // ! y < 999
  TSQLite3IndexConstraint = record
    /// Column on left-hand side of constraint
    // - The first column of the virtual table is column 0
    // - The ROWID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    iColumn: Integer;
    /// Constraint operator
    // - OP is =, <, <=, >, or >= using one of the SQLITE_INDEX_CONSTRAINT_* values
    op: byte;
    /// True if this constraint is usable
    // - The aConstraint[] array contains information about all constraints that
    // apply to the virtual table. But some of the constraints might not be usable
    // because of the way tables are ordered in a join. The xBestIndex method
    // must therefore only consider constraints that have a usable flag which is
    // true, and just ignore contraints with usable set to false
    usable: boolean;
    /// Used internally - xBestIndex() should ignore this field
    iTermOffset: Integer;
  end;
  PSQLite3IndexConstraintArray = ^TSQLite3IndexConstraintArray;
  TSQLite3IndexConstraintArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraint)-1] of TSQLite3IndexConstraint;

  /// ORDER BY clause, one item per column
  TSQLite3IndexOrderBy = record
    /// Column number
    // - The first column of the virtual table is column 0
    // - The ROWID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    iColumn: Integer;
    /// True for DESC.  False for ASC.
    desc: boolean;
  end;
  PSQLite3IndexOrderByArray = ^TSQLite3IndexOrderByArray;
  TSQLite3IndexOrderByArray = array[0..MaxInt div SizeOf(TSQLite3IndexOrderBy)-1] of TSQLite3IndexOrderBy;

  /// define what information is to be passed to xFilter() for a given WHERE
  // clause constraint of the form "column OP expr"
  TSQLite3IndexConstraintUsage = record
    /// If argvIndex>0 then the right-hand side of the corresponding
    // aConstraint[] is evaluated and becomes the argvIndex-th entry in argv
    // - Exactly one entry should be set to 1, another to 2, another to 3, and
    // so forth up to as many or as few as the xBestIndex() method wants.
    // - The EXPR of the corresponding constraints will then be passed in as
    // the argv[] parameters to xFilter()
    // - For example, if the aConstraint[3].argvIndex is set to 1, then when
    // xFilter() is called, the argv[0] passed to xFilter will have the EXPR
    // value of the aConstraint[3] constraint.
    argvIndex: Integer;
    /// If omit is true, then the constraint is assumed to be fully handled
    // by the virtual table and is not checked again by SQLite
    // - By default, the SQLite core double checks all constraints on each
    // row of the virtual table that it receives. If such a check is redundant,
    // xBestFilter() method can suppress that double-check by setting this field 
    omit: boolean;
  end;
  PSQLite3IndexConstraintUsageArray = ^TSQLite3IndexConstraintUsageArray;
  TSQLite3IndexConstraintUsageArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraintUsage) - 1] of TSQLite3IndexConstraintUsage;

  {/ Structure used as part of the virtual table interface to pass information
    into and receive the reply from the xBestIndex() method of a virtual table module
   - Outputs fields will be passed as parameter to the xFilter() method, and
     will be initialized to zero by SQLite
   - For instance, xBestIndex() method fills the idxNum and idxStr fields with
     information that communicates an indexing strategy to the xFilter method.
     The information in idxNum and idxStr is arbitrary as far as the SQLite core
     is concerned. The SQLite core just copies the information through to the
     xFilter() method. Any desired meaning can be assigned to idxNum and idxStr
     as long as xBestIndex() and xFilter() agree on what that meaning is.
     Use the SetInfo() method of this object in order to make a temporary copy
     of any needed data. }
  TSQLite3IndexInfo = record
    /// input: Number of entries in aConstraint array
    nConstraint: Integer;
    /// input: List of WHERE clause constraints of the form "column OP expr"
    aConstraint: PSQLite3IndexConstraintArray;
    /// input: Number of terms in the aOrderBy array
    nOrderBy: Integer;
    /// input: List of ORDER BY clause, one per column
    aOrderBy: PSQLite3IndexOrderByArray;
    /// output: filled by xBestIndex() method with information about what
    // parameters to pass to xFilter() method
    // - has the same number of items than the aConstraint[] array
    // - should set the aConstraintUsage[].argvIndex to have the corresponding
    // argument in xFilter() argc/argv[] expression list
    aConstraintUsage: PSQLite3IndexConstraintUsageArray;
    /// output: Number used to identify the index
    idxNum: Integer;
    /// output: String, possibly obtained from sqlite3_malloc()
    // - may contain any variable-length data or class/record content, as
    // necessary
    idxStr: PAnsiChar;
    /// output: Free idxStr using sqlite3_free() if true (=1)
    needToFreeIdxStr: Integer;
    /// output: True (=1) if output is already ordered
    // - i.e. if the virtual table will output rows in the order specified
    // by the ORDER BY clause
    // - if False (=0), will indicate to the SQLite core that it will need to
    // do a separate sorting pass over the data after it comes out
    // of the virtual table
    orderByConsumed: Integer;
    /// output: Estimated cost of using this index
    // - Should be set to the estimated number of disk access operations
    // required to execute this query against the virtual table
    // - The SQLite core will often call xBestIndex() multiple times with
    // different constraints, obtain multiple cost estimates, then choose the
    // query plan that gives the lowest estimate
    estimatedCost: Double;
  end;

  {/ Virtual Table Instance Object
  - Every virtual table module implementation uses a subclass of this object
    to describe a particular instance of the virtual table.
  - Each subclass will be tailored to the specific needs of the module
    implementation. The purpose of this superclass is to define certain fields
    that are common to all module implementations. This structure therefore
    contains a pInstance field, which will be used to store a class instance
    handling the virtual table as a pure Delphi class: the TSQLVirtualTableModule
    class will use it internaly }
  TSQLite3VTab = record
    /// The module for this virtual table
    pModule: PSQLite3Module;
    /// no longer used
    nRef: Integer;
    /// Error message from sqlite3_mprintf()
    // - Virtual tables methods can set an error message by assigning a string
    // obtained from sqlite3_mprintf() to zErrMsg.
    // - The method should take care that any prior string is freed by a call
    // to sqlite3_free() prior to assigning a new string to zErrMsg.
    // - After the error message is delivered up to the client application,
    // the string will be automatically freed by sqlite3_free() and the zErrMsg
    // field will be zeroed.
    zErrMsg: PAnsiChar;
    /// this will be used to store a Delphi class instance handling the Virtual Table
    pInstance: TObject;
  end;

  {/ Virtual Table Cursor Object
   - Every virtual table module implementation uses a subclass of the following
     structure to describe cursors that point into the virtual table and are
     used to loop through the virtual table.
   - Cursors are created using the xOpen method of the module and are destroyed
     by the xClose method. Cursors are used by the xFilter, xNext, xEof, xColumn,
     and xRowid methods of the module.
   - Each module implementation will define the content of a cursor structure
     to suit its own needs.
   - This superclass exists in order to define fields of the cursor that are
     common to all implementationsThis structure therefore contains a pInstance
     field, which will be used to store a class instance handling the virtual
     table as a pure Delphi class: the TSQLVirtualTableModule class will use
     it internaly }
  TSQLite3VTabCursor = record
    /// Virtual table of this cursor
    pVtab: PSQLite3VTab;
    /// this will be used to store a Delphi class instance handling the cursor
    pInstance: TObject;
  end;

  {/ defines a module object used to implement a virtual table.
   - Think of a module as a class from which one can construct multiple virtual
   tables having similar properties. For example, one might have a module that
   provides read-only access to comma-separated-value (CSV) files on disk.
   That one module can then be used to create several virtual tables where each
   virtual table refers to a different CSV file.
   - The module structure contains methods that are invoked by SQLite to perform
   various actions on the virtual table such as creating new instances of a
   virtual table or destroying old ones, reading and writing data, searching
   for and deleting, updating, or inserting rows. }
  TSQLite3Module = record
    {/ defines the particular edition of the module table structure
    - Currently, handled iVersion is 2, but in future releases of SQLite the
      module structure definition might be extended with additional methods and
      in that case the iVersion value will be increased }
    iVersion: Integer;
    {/ called to create a new instance of a virtual table in response to a
      CREATE VIRTUAL TABLE statement
    - The job of this method is to construct the new virtual table object (an
      PSQLite3VTab object) and return a pointer to it in ppVTab
    - The DB parameter is a pointer to the SQLite database connection that is
      executing the CREATE VIRTUAL TABLE statement
    - The pAux argument is the copy of the client data pointer that was the
      fourth argument to the sqlite3_create_module_v2() call that registered
      the virtual table module
    - The argv parameter is an array of argc pointers to null terminated strings
    - The first string, argv[0], is the name of the module being invoked. The
      module name is the name provided as the second argument to sqlite3_create_module()
      and as the argument to the USING clause of the CREATE VIRTUAL TABLE
      statement that is running.
    - The second, argv[1], is the name of the database in which the new virtual
      table is being created. The database name is "main" for the primary
      database, or "temp" for TEMP database, or the name given at the end of
      the ATTACH statement for attached databases.
    - The third element of the array, argv[2], is the name of the new virtual
      table, as specified following the TABLE keyword in the CREATE VIRTUAL
      TABLE statement
    - If present, the fourth and subsequent strings in the argv[] array report
      the arguments to the module name in the CREATE VIRTUAL TABLE statement
    - As part of the task of creating a new PSQLite3VTab structure, this method
      must invoke sqlite3_declare_vtab() to tell the SQLite core about the
      columns and datatypes in the virtual table }
    xCreate: function(DB: TSQLite3DB; pAux: Pointer;
      argc: Integer; const argv: PPUTF8CharArray;
      var ppVTab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ xConnect is called to establish a new connection to an existing virtual table,
       whereas xCreate is called to create a new virtual table from scratch
    - It has the same parameters and constructs a new PSQLite3VTab structure
    - xCreate and xConnect methods are only different when the virtual table
     has some kind of backing store that must be initialized the first time the
     virtual table is created. The xCreate method creates and initializes the
     backing store. The xConnect method just connects to an existing backing store. }
    xConnect: function(DB: TSQLite3DB; pAux: Pointer;
      argc: Integer; const argv: PPUTF8CharArray;
      var ppVTab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Used to determine the best way to access the virtual table
    - The pInfo parameter is used for input and output parameters
    - The SQLite core calls the xBestIndex() method when it is compiling a query
     that involves a virtual table. In other words, SQLite calls this method when
     it is running sqlite3_prepare() or the equivalent.
    - By calling this method, the SQLite core is saying to the virtual table
     that it needs to access some subset of the rows in the virtual table and
     it wants to know the most efficient way to do that access. The xBestIndex
     method replies with information that the SQLite core can then use to
     conduct an efficient search of the virtual table, via the xFilter() method.
    - While compiling a single SQL query, the SQLite core might call xBestIndex
     multiple times with different settings in pInfo. The SQLite
     core will then select the combination that appears to give the best performance.
    - The information in the pInfo structure is ephemeral and may be overwritten
     or deallocated as soon as the xBestIndex() method returns. If the
     xBestIndex() method needs to remember any part of the pInfo structure,
     it should make a copy. Care must be taken to store the copy in a place
     where it will be deallocated, such as in the idxStr field with
     needToFreeIdxStr set to 1. }
    xBestIndex: function(var pVTab: TSQLite3VTab; var pInfo: TSQLite3IndexInfo): Integer;
      {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Releases a connection to a virtual table
    - Only the pVTab object is destroyed. The virtual table is not destroyed and
      any backing store associated with the virtual table persists. This method
      undoes the work of xConnect.  }
    xDisconnect: function(pVTab: PSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Releases a connection to a virtual table, just like the xDisconnect method,
      and it also destroys the underlying table implementation.
    - This method undoes the work of xCreate
    - The xDisconnect method is called whenever a database connection that uses
      a virtual table is closed. The xDestroy method is only called when a
      DROP TABLE statement is executed against the virtual table. }
    xDestroy: function(pVTab: PSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Creates a new cursor used for accessing (read and/or writing) a virtual table
    - A successful invocation of this method will allocate the memory for the
      TPSQLite3VTabCursor (or a subclass), initialize the new object, and
      make ppCursor point to the new object. The successful call then returns SQLITE_OK.
    - For every successful call to this method, the SQLite core will later
     invoke the xClose method to destroy the allocated cursor.
    - The xOpen method need not initialize the pVtab field of the ppCursor structure.
      The SQLite core will take care of that chore automatically.
    - A virtual table implementation must be able to support an arbitrary number
      of simultaneously open cursors.
    - When initially opened, the cursor is in an undefined state. The SQLite core
      will invoke the xFilter method on the cursor prior to any attempt to
      position or read from the cursor. }
    xOpen: function(var pVTab: TSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer;
      {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Closes a cursor previously opened by xOpen
    - The SQLite core will always call xClose once for each cursor opened using xOpen.
    - This method must release all resources allocated by the corresponding xOpen call.
    - The routine will not be called again even if it returns an error. The
     SQLite core will not use the pVtabCursor again after it has been closed. }
    xClose: function(pVtabCursor: PSQLite3VTabCursor): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Begins a search of a virtual table
    - The first argument is a cursor opened by xOpen.
    - The next two arguments define a particular search index previously chosen
      by xBestIndex(). The specific meanings of idxNum and idxStr are unimportant
      as long as xFilter() and xBestIndex() agree on what that meaning is.
    - The xBestIndex() function may have requested the values of certain
      expressions using the aConstraintUsage[].argvIndex values of its pInfo
      structure. Those values are passed to xFilter() using the argc and argv
      parameters.
    - If the virtual table contains one or more rows that match the search criteria,
      then the cursor must be left point at the first row. Subsequent calls to
      xEof must return false (zero). If there are no rows match, then the cursor
      must be left in a state that will cause the xEof to return true (non-zero).
      The SQLite engine will use the xColumn and xRowid methods to access that row content.
      The xNext method will be used to advance to the next row.
    - This method must return SQLITE_OK if successful, or an sqlite error code
      if an error occurs.}
    xFilter: function(var pVtabCursor: TSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar;
      argc: Integer; var argv: TSQLite3ValueArray): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Advances a virtual table cursor to the next row of a result set initiated by xFilter
    - If the cursor is already pointing at the last row when this routine is called,
      then the cursor no longer points to valid data and a subsequent call to the
      xEof method must return true (non-zero).
    - If the cursor is successfully advanced to another row of content, then
      subsequent calls to xEof must return false (zero).
    - This method must return SQLITE_OK if successful, or an sqlite error code
      if an error occurs. }
    xNext: function(var pVtabCursor: TSQLite3VTabCursor): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Checks if cursor reached end of rows
    - Must return false (zero) if the specified cursor currently points to a
      valid row of data, or true (non-zero) otherwise }
    xEof: function(var pVtabCursor: TSQLite3VTabCursor): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ The SQLite core invokes this method in order to find the value for the
      N-th column of the current row
    - N is zero-based so the first column is numbered 0.
    - The xColumn method may return its result back to SQLite using one of the
      standard sqlite3_result_*() functions with the specified sContext
    - If the xColumn method implementation calls none of the sqlite3_result_*()
      functions, then the value of the column defaults to an SQL NULL.
    - The xColumn method must return SQLITE_OK on success.
    - To raise an error, the xColumn method should use one of the result_text()
      methods to set the error message text, then return an appropriate error code. }
    xColumn: function(var pVtabCursor: TSQLite3VTabCursor; sContext: TSQLite3FunctionContext;
      N: Integer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Should fill pRowid with the rowid of row that the virtual table cursor
      pVtabCursor is currently pointing at }
    xRowid: function(var pVtabCursor: TSQLite3VTabCursor; var pRowid: Int64): Integer;
      {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Makes a change to a virtual table content (insert/delete/update)
    - The nArg parameter specifies the number of entries in the ppArg[] array
    - The value of nArg will be 1 for a pure delete operation or N+2 for an
      insert or replace or update where N is the number of columns in the table
      (including any hidden columns)
    - The ppArg[0] parameter is the rowid of a row in the virtual table to be deleted.
      If ppArg[0] is an SQL NULL, then no deletion occurs
    - The ppArg[1] parameter is the rowid of a new row to be inserted into the
      virtual table. If ppArg[1] is an SQL NULL, then the implementation must
      choose a rowid for the newly inserted row. Subsequent ppArg[] entries
      contain values of the columns of the virtual table, in the order that
      the columns were declared. The number of columns will match the table
      declaration that the xConnect or xCreate method made using the
      sqlite3_declare_vtab() call. All hidden columns are included.
    - When doing an insert without a rowid (nArg>1, ppArg[1] is an SQL NULL),
      the implementation must set pRowid to the rowid of the newly inserted row;
      this will become the value returned by the sqlite3_last_insert_rowid()
      function. Setting this value in all the other cases is a harmless no-op;
      the SQLite engine ignores the pRowid return value if nArg=1 or ppArg[1]
      is not an SQL NULL.
    - Each call to xUpdate() will fall into one of cases shown below. Note
      that references to ppArg[i] mean the SQL value held within the ppArg[i]
      object, not the ppArg[i] object itself:
      $ nArg = 1
      The single row with rowid equal to ppArg[0] is deleted. No insert occurs.
      $ nArg > 1
      $ ppArg[0] = NULL
      A new row is inserted with a rowid ppArg[1] and column values in ppArg[2]
      and following. If ppArg[1] is an SQL NULL, the a new unique rowid is
      generated automatically.
      $ nArg > 1
      $ ppArg[0] <> NULL
      $ ppArg[0] = ppArg[1]
      The row with rowid ppArg[0] is updated with new values in ppArg[2] and
      following parameters.
      $ nArg > 1
      $ ppArg[0] <> NULL
      $ ppArg[0] <> ppArg[1]
      The row with rowid ppArg[0] is updated with rowid ppArg[1] and new values
      in ppArg[2] and following parameters. This will occur when an SQL statement
      updates a rowid, as in the statement:
      $ UPDATE table SET rowid=rowid+1 WHERE ...;
    - The xUpdate() method must return SQLITE_OK if and only if it is successful.
      If a failure occurs, the xUpdate() must return an appropriate error code.
      On a failure, the pVTab.zErrMsg element may optionally be replaced with
      a custom error message text.
    - If the xUpdate() method violates some constraint of the virtual table
      (including, but not limited to, attempting to store a value of the
      wrong datatype, attempting to store a value that is too large or too small,
      or attempting to change a read-only value) then the xUpdate() must fail
      with an appropriate error code.
    - There might be one or more TSQLite3VTabCursor objects open and in use on
      the virtual table instance and perhaps even on the row of the virtual
      table when the xUpdate() method is invoked. The implementation of xUpdate()
      must be prepared for attempts to delete or modify rows of the table out
      from other existing cursors. If the virtual table cannot accommodate such
      changes, the xUpdate() method must return an error code. }
    xUpdate: function(var pVTab: TSQLite3VTab;
      nArg: Integer; var ppArg: TSQLite3ValueArray;
      var pRowid: Int64): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Begins a transaction on a virtual table
    - This method is always followed by one call to either the xCommit or
      xRollback method.
    - Virtual table transactions do not nest, so the xBegin method will not be
      invoked more than once on a single virtual table without an intervening
      call to either xCommit or xRollback. For nested transactions, use
      xSavepoint, xRelease and xRollBackTo methods.
    - Multiple calls to other methods can and likely will occur in between the
      xBegin and the corresponding xCommit or xRollback. }
    xBegin: function(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Signals the start of a two-phase commit on a virtual table
    - This method is only invoked after call to the xBegin method and prior
      to an xCommit or xRollback.
    - In order to implement two-phase commit, the xSync method on all virtual
      tables is invoked prior to invoking the xCommit method on any virtual table.
    - If any of the xSync methods fail, the entire transaction is rolled back. }
    xSync: function(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Causes a virtual table transaction to commit }
    xCommit: function(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Causes a virtual table transaction to rollback }
    xRollback: function(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Called during sqlite3_prepare() to give the virtual table implementation
      an opportunity to overload SQL functions
    - When a function uses a column from a virtual table as its first argument,
      this method is called to see if the virtual table would like to overload
      the function. The first three parameters are inputs: the virtual table,
      the number of arguments to the function, and the name of the function.
      If no overloading is desired, this method returns 0. To overload the
      function, this method writes the new function implementation into pxFunc
      and writes user data into ppArg and returns 1.
    - Note that infix functions (LIKE, GLOB, REGEXP, and MATCH) reverse the
      order of their arguments. So "like(A,B)" is equivalent to "B like A".
      For the form "B like A" the B term is considered the first argument to the
      function. But for "like(A,B)" the A term is considered the first argument.
    - The function pointer returned by this routine must be valid for the
      lifetime of the pVTab object given in the first parameter. }
    xFindFunction: function(var pVTab: TSQLite3VTab; nArg: Integer; const zName: PAnsiChar;
      var pxFunc: TSQLFunctionFunc; var ppArg: Pointer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Provides notification that the virtual table implementation that the
      virtual table will be given a new name
    - If this method returns SQLITE_OK then SQLite renames the table.
    - If this method returns an error code then the renaming is prevented. }
    xRename: function(var pVTab: TSQLite3VTab; const zNew: PAnsiChar): Integer;
       {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Starts a new transaction with the virtual table
    - SAVEPOINTs are a method of creating transactions, similar to BEGIN and
      COMMIT, except that the SAVEPOINT and RELEASE commands are named and
      may be nested. See @http://www.sqlite.org/lang_savepoint.html
    - iSavepoint parameter indicates the unique name of the SAVEPOINT }
    xSavepoint: function(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer;
       {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Merges a transaction into its parent transaction, so that the specified
      transaction and its parent become the same transaction
    - Causes all savepoints back to and including the most recent savepoint
      with a matching identifier to be removed from the transaction stack
    - Some people view RELEASE as the equivalent of COMMIT for a SAVEPOINT.
      This is an acceptable point of view as long as one remembers that the
      changes committed by an inner transaction might later be undone by a
      rollback in an outer transaction.
    - iSavepoint parameter indicates the unique name of the SAVEPOINT }
    xRelease: function(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer;
       {$ifndef USEFASTCALL}cdecl;{$endif}
    {/ Reverts the state of the virtual table content back to what it was just
      after the corresponding SAVEPOINT
    - iSavepoint parameter indicates the unique name of the SAVEPOINT }
    xRollbackTo: function(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer;
       {$ifndef USEFASTCALL}cdecl;{$endif}
  end;

{/ Used to register a new virtual table module name
  - The module name is registered on the database connection specified by the
    first DB parameter.
  - The name of the module is given by the second parameter.
  - The third parameter is a pointer to the implementation of the virtual table
    module.
  - The fourth parameter is an arbitrary client data pointer that is passed
    through into the xCreate and xConnect methods of the virtual table module
    when a new virtual table is be being created or reinitialized.
  - The fifth parameter can be used to specify a custom destructor for the
    pClientData buffer }
function sqlite3_create_module_v2(DB: TSQLite3DB; const zName: PAnsiChar;
  var p: TSQLite3Module; pClientData: Pointer; xDestroy: TSQLDestroyPtr): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Declare the Schema of a virtual table
  - The xCreate() and xConnect() methods of a virtual table module call this
  interface to declare the format (the names and datatypes of the columns) of
  the virtual tables they implement. The string can be deallocated and/or reused
  as soon as the sqlite3_declare_vtab() routine returns.
  - If a column datatype contains the special keyword "HIDDEN" (in any
  combination of upper and lower case letters) then that keyword it is omitted
  from the column datatype name and the column is marked as a hidden column
  internally. A hidden column differs from a normal column in three respects:
  1. Hidden columns are not listed in the dataset returned by "PRAGMA table_info",
  2. Hidden columns are not included in the expansion of a "*" expression in
  the result set of a SELECT, and 3. Hidden columns are not included in the
  implicit column-list used by an INSERT statement that lacks an explicit
  column-list. }
function sqlite3_declare_vtab(DB: TSQLite3DB; const zSQL: PAnsiChar): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

type
  {/ Compile-Time Authorization Callback prototype
  - The authorizer callback is invoked as SQL statements are being compiled by
    sqlite3_prepare2() e.g.
  - The authorizer callback should return SQLITE_OK to allow the action,
    SQLITE_IGNORE to disallow the specific action but allow the SQL statement
    to continue to be compiled, or SQLITE_DENY to cause the entire SQL statement
    to be rejected with an error.
  - If the authorizer callback returns any value other than SQLITE_IGNORE,
    SQLITE_OK, or SQLITE_DENY then the sqlite3_prepare_v2() or equivalent call
    that triggered the authorizer will fail with an error message.
  - The first pUserData parameter to the authorizer callback is a copy of the
    third parameter to the sqlite3_set_authorizer() interface
  - The second parameter to the callback is an integer action code that
    specifies the particular action to be authorized:
  - The third through sixth parameters to the callback are zero-terminated
    strings that contain additional details about the action to be authorized.
  - Here is a list of handled code constant, and their associated zTab / zCol
    parameters:
    !    const                       zTab            zCol
    $ SQLITE_CREATE_INDEX          Index Name      Table Name
    $ SQLITE_CREATE_TABLE          Table Name      nil
    $ SQLITE_CREATE_TEMP_INDEX     Index Name      Table Name
    $ SQLITE_CREATE_TEMP_TABLE     Table Name      nil
    $ SQLITE_CREATE_TEMP_TRIGGER   Trigger Name    Table Name
    $ SQLITE_CREATE_TEMP_VIEW      View Name       nil
    $ SQLITE_CREATE_TRIGGER        Trigger Name    Table Name
    $ SQLITE_CREATE_VIEW           View Name       nil
    $ SQLITE_DELETE                Table Name      nil
    $ SQLITE_DROP_INDEX            Index Name      Table Name
    $ SQLITE_DROP_TABLE            Table Name      nil
    $ SQLITE_DROP_TEMP_INDEX       Index Name      Table Name
    $ SQLITE_DROP_TEMP_TABLE       Table Name      nil
    $ SQLITE_DROP_TEMP_TRIGGER     Trigger Name    Table Name
    $ SQLITE_DROP_TEMP_VIEW        View Name       nil
    $ SQLITE_DROP_TRIGGER          Trigger Name    Table Name
    $ SQLITE_DROP_VIEW             View Name       nil
    $ SQLITE_INSERT                Table Name      nil
    $ SQLITE_PRAGMA                Pragma Name     1st arg or nil
    $ SQLITE_READ                  Table Name      Column Name
    $ SQLITE_SELECT                nil             nil
    $ SQLITE_TRANSACTION           Operation       nil
    $ SQLITE_UPDATE                Table Name      Column Name
    $ SQLITE_ATTACH                Filename        nil
    $ SQLITE_DETACH                Database Name   nil
    $ SQLITE_ALTER_TABLE           Database Name   Table Name
    $ SQLITE_REINDEX               Index Name      nil
    $ SQLITE_ANALYZE               Table Name      nil
    $ SQLITE_CREATE_VTABLE         Table Name      Module Name
    $ SQLITE_DROP_VTABLE           Table Name      Module Name
    $ SQLITE_FUNCTION              nil             Function Name
    $ SQLITE_SAVEPOINT             Operation       Savepoint Name
   - The 5th parameter to the authorizer callback is the name of the database
     ('main', 'temp', etc.) if applicable.
   - The 6th parameter to the authorizer callback is the name of the inner-most
     trigger or view that is responsible for the access attempt or nil if this
     access attempt is directly from top-level SQL code. }
  TSQLAuthorizerCallback = function(pUserData: Pointer; code: Integer;
    const zTab, zCol, zDb, zAuthContext: PAnsiChar): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}

{/ Registers an authorizer callback to a specified DB connection
  - Only a single authorizer can be in place on a database connection at a time
  - Each call to sqlite3_set_authorizer overrides the previous call
  - Disable the authorizer by installing a nil callback
  - The authorizer is disabled by default }
function sqlite3_set_authorizer(DB: TSQLite3DB; xAuth: TSQLAuthorizerCallback;
  pUserData: Pointer): Integer;   {$ifndef USEFASTCALL}cdecl;{$endif} external;

type
  {/ Callback function invoked when a row is updated, inserted or deleted,
    after sqlite3_update_hook() registration
  - The first pUpdateArg argument is a copy of the third argument to
    sqlite3_update_hook().
  - The second op argument is one of SQLITE_INSERT, SQLITE_DELETE, or SQLITE_UPDATE,
    depending on the operation that caused the callback to be invoked.
  - The third and fourth zDB / zTbl arguments contain pointers to the database
    and table name containing the affected row.
  - The final iRowID parameter is the rowid of the row. In the case of an update,
    this is the rowid after the update takes place.
  - The update hook implementation must not do anything that will modify the
    database connection that invoked the update hook. Any actions to modify the
    database connection must be deferred until after the completion of the
    sqlite3_step() call that triggered the update hook. Note that
    sqlite3_prepare_v2() and sqlite3_step() both modify their database
    connections for the meaning of "modify" in this paragraph. }
  TSQLUpdateCallback = procedure(pUpdateArg: Pointer; op: Integer;
    const zDb, zTbl: PUTF8Char; iRowID: Int64); {$ifndef USEFASTCALL}cdecl;{$endif}

{/ Register Data Change Notification Callbacks
 - The sqlite3_update_hook() interface registers a callback function with
   the database connection identified by the first argument to be invoked
   whenever a row is updated, inserted or deleted.
 - Any callback set by a previous call to this function for the same
   database connection is overridden.
 - sqlite3_update_hook(D,C,P) function returns the P argument from the
   previous call on the same database connection D, or nil for the first
   call on database connection D.
 - The update hook is not invoked when internal system tables are modified
   (i.e. sqlite_master and sqlite_sequence).
 - In the current implementation, the update hook is not invoked when
   duplication rows are deleted because of an ON CONFLICT REPLACE clause.
   Nor is the update hook invoked when rows are deleted using the truncate
   optimization. The exceptions defined in this paragraph might change in
   a future release of SQLite.
 - Note that you should also trace COMMIT and ROLLBACK commands (calling
   sqlite3_commit_hook() and sqlite3_rollback_hook() functions) if you want to
   ensure that the notified update was not canceled by a later Rollback. }
function sqlite3_update_hook(DB: TSQLite3DB; xCallback: TSQLUpdateCallback;
  pArg: pointer): pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

type
  {/ Commit And Rollback Notification Callback function after
    sqlite3_commit_hook() or sqlite3_rollback_hook() registration
  - The callback implementation must not do anything that will modify the
    database connection that invoked the callback. Any actions to modify the
    database connection must be deferred until after the completion of the
    sqlite3_step() call that triggered the commit or rollback hook in the
    first place. Note that sqlite3_prepare_v2() and sqlite3_step() both modify
    their database connections for the meaning of "modify" in this paragraph.
  - When the commit hook callback routine returns zero, the COMMIT operation
    is allowed to continue normally. If the commit hook returns non-zero, then
    the COMMIT is converted into a ROLLBACK. The rollback hook is invoked on
    a rollback that results from a commit hook returning non-zero, just as
    it would be with any other rollback.
  - For the purposes of this API, a transaction is said to have been rolled
    back if an explicit "ROLLBACK" statement is executed, or an error or
    constraint causes an implicit rollback to occur. The rollback callback
    is not invoked if a transaction is automatically rolled back because the
    database connection is closed. }
  TSQLCommitCallback = function(pArg: Pointer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}

{/ Register Commit Notification Callbacks
 - The sqlite3_commit_hook() interface registers a callback function to be
   invoked whenever a transaction is committed.
 - Any callback set by a previous call to sqlite3_commit_hook() for the same
  database connection is overridden.
 - Registering a nil function disables the Commit callback.
 - The sqlite3_commit_hook(D,C,P) function returns the P argument from the
   previous call of the same function on the same database connection D, or nil
   for the first call for each function on D. }
function sqlite3_commit_hook(DB: TSQLite3DB; xCallback: TSQLCommitCallback;
  pArg: Pointer): Pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Register Rollback Notification Callbacks
 - The sqlite3_rollback_hook() interface registers a callback function to be
   invoked whenever a transaction is rolled back.
 - Any callback set by a previous call to sqlite3_rollback_hook() for the same
   database connection is overridden.
 - Registering a nil function disables the Rollback callback.
 - The sqlite3_rollback_hook(D,C,P) function returns the P argument from the
   previous call of the same function on the same database connection D, or nil
   for the first call for each function on D. }
function sqlite3_rollback_hook(DB: TSQLite3DB;  xCallback: TSQLCommitCallback;
  pArg: Pointer): Pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external;


{$ifdef INCLUDE_TRACE}

type
  {/ Callback function registered by sqlite3_trace()
   - this procedure will be invoked at various times when an SQL statement is
    being run by sqlite3_step() }
  TSQLTraceCallback = procedure(TraceArg: Pointer; Trace: PUTF8Char);
    {$ifndef USEFASTCALL}cdecl;{$endif}

{/ Register callback function that can be used for tracing the execution of
  SQL statements
  - The callback function registered by sqlite3_trace() is invoked at various
  times when an SQL statement is being run by sqlite3_step(). The sqlite3_trace()
  callback is invoked with a UTF-8 rendering of the SQL statement text as the
  statement first begins executing. Additional sqlite3_trace() callbacks might
  occur as each triggered subprogram is entered. The callbacks for triggers
  contain a UTF-8 SQL comment that identifies the trigger. }
function sqlite3_trace(DB: TSQLite3DB; tCallback: TSQLTraceCallback;
  aUserData: Pointer): Pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external;

{$endif INCLUDE_TRACE}

(* this function is considered experimental and is subject to change in future
  versions of SQLite -> not declared yet (you should better use our generic
  profiling feature as implemented with SynCommons.TSynLog class)
type
  TSQLProfileCallback = procedure(ProfileArg: Pointer; Profile: PUTF8Char;
    ElapsedNS: Int64); {$ifndef USEFASTCALL}cdecl;{$endif}

function sqlite3_profile(DB: TSQLite3DB; pCallback: TSQLProfileCallback;
  aUserData: Pointer): Pointer; {$ifndef USEFASTCALL}cdecl;{$endif} external; *)

  
{/ Count The Number Of Rows Modified
 - This function returns the number of database rows that were changed or
 inserted or deleted by the most recently completed SQL statement on the
 database connection specified by the first parameter. Only changes that
 are directly specified by the INSERT, UPDATE, or DELETE statement are counted.
 Auxiliary changes caused by triggers or foreign key actions are not counted.
 Use the sqlite3_total_changes() function to find the total number of changes
 including changes caused by triggers and foreign key actions.
 - If a separate thread makes changes on the same database connection while
 sqlite3_changes() is running then the value returned is unpredictable and not
 meaningful. }
function sqlite3_changes(DB: TSQLite3DB): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Total Number Of Rows Modified
 - This function returns the number of row changes caused by INSERT, UPDATE or
 DELETE statements since the database connection was opened. The count returned
 by sqlite3_total_changes() includes all changes from all trigger contexts and
 changes made by foreign key actions. However, the count does not include
 changes used to implement REPLACE constraints, do rollbacks or ABORT
 processing, or DROP TABLE processing. The count does not include rows of
 views that fire an INSTEAD OF trigger, though if the INSTEAD OF trigger makes
 changes of its own, those changes are counted. The sqlite3_total_changes()
 function counts the changes as soon as the statement that makes them is
 completed (when the statement handle is passed to sqlite3_reset()
 or sqlite3_finalize()).
 - If a separate thread makes changes on the same database connection while
 sqlite3_total_changes() is running then the value returned is unpredictable and not
 meaningful. }
function sqlite3_total_changes(DB: TSQLite3DB): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

const
  SQLITE_DENY   = 1;
  SQLITE_IGNORE = 2;

const
  SQLITE_CREATE_INDEX        = 1;
  SQLITE_CREATE_TABLE        = 2;
  SQLITE_CREATE_TEMP_INDEX   = 3;
  SQLITE_CREATE_TEMP_TABLE   = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW    = 6;
  SQLITE_CREATE_TRIGGER      = 7;
  SQLITE_CREATE_VIEW         = 8;
  SQLITE_DELETE              = 9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              = 23;
  SQLITE_ATTACH              = 24;
  SQLITE_DETACH              = 25;
  SQLITE_ALTER_TABLE         = 26;
  SQLITE_REINDEX             = 27;
  SQLITE_ANALYZE             = 28;
  SQLITE_CREATE_VTABLE       = 29;
  SQLITE_DROP_VTABLE         = 30;
  SQLITE_FUNCTION            = 31;
  SQLITE_SAVEPOINT           = 32;
  SQLITE_COPY                = 0;


const
  /// SQL statement to get all tables names in the current database file
  // (taken from official SQLite3 documentation)
  SQL_GET_TABLE_NAMES =
    'SELECT name FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%'';';

type
  /// custom SQLite3 dedicated Exception type
  ESQLite3Exception = class(ESynException)
  public
    /// the DB which raised this exception
    DB: TSQLite3DB;
    /// the corresponding error code
    ErrorCode: integer;
    /// create the exception, getting the message from DB
    constructor Create(aDB: TSQLite3DB; aErrorCode: integer); reintroduce; overload;
    /// create the exception, getting the message from caller
    constructor Create(const aMessage: string; aErrorCode: integer); reintroduce; overload;
  end;

{/ test the result state of a sqlite3_*() function
  - raise a ESQLite3Exception if the result state is an error
  - return the result state otherwize (SQLITE_OK,SQLITE_ROW,SQLITE_DONE e.g.) }
function sqlite3_check(DB: TSQLite3DB; aResult: integer): integer;

{/ Returns a pointer to a block of memory at least N bytes in length
 - should call native malloc() function, i.e. GetMem() in this unit }
function sqlite3_malloc(n: Integer): Pointer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Attempts to resize a prior memory allocation
 - should call native realloc() function, i.e. ReallocMem() in this unit }
function sqlite3_realloc(pOld: Pointer; n: Integer): Pointer;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

{/ Releases memory previously returned by sqlite3_malloc() or sqlite3_realloc()
 - should call native free() function, i.e. FreeMem() in this unit }
procedure sqlite3_free(p: Pointer);
  {$ifndef USEFASTCALL}cdecl;{$endif} external;

/// Returns the number of bytes of memory currently outstanding (malloced but not freed)
function sqlite3_memory_used: Int64;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;
  
/// Returns the maximum value of sqlite3_memory_used() since the high-water mark
// was last reset
function sqlite3_memory_highwater(resetFlag: Integer): Int64;
  {$ifndef USEFASTCALL}cdecl;{$endif} external;


{ ************ objects for high-level access SQLite3 database engine }

type
  /// available file-level write access wait mode of the SQLite3 engine
  // - when synchronous is smFull (which is the default setting), the SQLite
  // database engine will use the xSync method of the VFS to ensure that all
  // content is safely written to the disk surface prior to continuing. This
  // ensures that an operating system crash or power failure will not corrupt
  // the database. FULL synchronous is very safe, but it is also slower.
  // - when synchronous is smNormal, the SQLite database engine will still
  // sync at the most critical moments, but less often than in FULL mode. There
  // is a very small (though non-zero) chance that a power failure at just the
  // wrong time could corrupt the database in NORMAL mode. But in practice,
  // you are more likely to suffer a catastrophic disk failure or some other
  // unrecoverable hardware fault.
  // - when synchronous is smOff, SQLite continues without syncing as soon as
  // it has handed data off to the operating system. If the application running
  // SQLite crashes, the data will be safe, but the database might become
  // corrupted if the operating system crashes or the computer loses power
  // before that data has been written to the disk surface. On the other hand,
  // some operations are as much as 50 or more times faster with synchronous OFF.
  TSQLSynchronousMode = (smOff, smNormal, smFull);

  TSQLDatabase = class;

  TSQLBlobStream = class;

  PSQLRequest = ^TSQLRequest;

  /// wrapper to a SQLite3 request
  TSQLRequest = {$ifndef UNICODE}object{$else}record{$endif}
  private
    fDB: TSQLite3DB;
    fRequest: TSQLite3Statement;
    fNextSQL: PUTF8Char;
    fFieldCount: integer;
    function GetReadOnly: Boolean;
    function GetParamCount: integer;

  // 1. general request process
  public
    {/ Prepare a UTF-8 encoded SQL statement
     - compile the SQL into byte-code
     - parameters ? ?NNN :VV @VV $VV can be bound with Bind*() functions below
     - raise an ESQLite3Exception on any error }
    function Prepare(DB: TSQLite3DB; const SQL: RawUTF8): integer;
    {/ Prepare a WinAnsi SQL statement
     - behave the same as Prepare() }
    function PrepareAnsi(DB: TSQLite3DB; const SQL: WinAnsiString): integer;
    {/ Prepare the next SQL command initialized in previous Prepare()
     - raise an ESQLite3Exception on any error }
    function PrepareNext: integer;
    {/ Evaluate An SQL Statement, returning the sqlite3_step() result status:
     - return SQLITE_ROW on success, with data ready to be retrieved via the
      Field*() methods
     - return SQLITE_DONE if the SQL commands were executed
     - raise an ESQLite3Exception on any error }
    function Step: integer;
    {/ Reset A Prepared Statement Object
     - reset a prepared statement object back to its initial state,
      ready to be re-executed.
     - any SQL statement variables that had values bound to them using the Bind*()
      function below retain their values. Use BindReset() to reset the bindings
     - return SQLITE_OK on success, or the previous Step error code }
    function Reset: integer;
    {/ Execute all SQL statements already prepared by a call to Prepare()
     - the statement is closed
     - raise an ESQLite3Exception on any error }
    procedure ExecuteAll; overload;
    {/ Execute all SQL statements in the aSQL UTF-8 encoded string
     - internaly call Prepare() then Step then PrepareNext until end of aSQL
     - Close is always called internaly
     - raise an ESQLite3Exception on any error }
    procedure ExecuteAll(aDB: TSQLite3DB; const aSQL: RawUTF8); overload;
    {/ Execute one SQL statement already prepared by a call to Prepare()
     - the statement is closed
     - raise an ESQLite3Exception on any error }
    procedure Execute; overload;
    {/ Execute one SQL statement in the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL: call Prepare() then Step once
     - Close is always called internaly
     - raise an ESQLite3Exception on any error }
    procedure Execute(aDB: TSQLite3DB; const aSQL: RawUTF8); overload;
    {/ Execute a SQL statement which return integers from the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get (at least) one field/column result of INTEGER
     - return result as a dynamic array of Int64 in ID
     - return count of row in integer function result (may be < length(ID))
     - raise an ESQLite3Exception on any error }
    function Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var ID: TInt64DynArray): integer; overload;
    {/ Execute a SQL statement which return one integer from the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get (at least) one field/column result of INTEGER
     - return result as an unique Int64 in ID
     - raise an ESQLite3Exception on any error }
    procedure Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out ID: Int64); overload;
    {/ Execute a SQL statement which return one TEXT value from the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get (at least) one field/column result of TEXT
     - raise an ESQLite3Exception on any error }
    procedure Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out Value: RawUTF8); overload;
    {/ Execute a SQL statement which return TEXT from the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get (at least) one field/column result of TEXT
     - return result as a dynamic array of RawUTF8 in ID
     - return count of row in integer function result (may be < length(ID))
     - raise an ESQLite3Exception on any error }
    function Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer; overload;
    /// Execute one SQL statement which return the results in JSON format
    // - JSON format is more compact than XML and well supported
    // - Execute the first statement in aSQL
    // - if SQL is '', the statement should have been prepared, reset and bound if necessary
    // - raise an ESQLite3Exception on any error
    // - JSON data is added to TStream, with UTF-8 encoding
    // - if Expand is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expand is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data (no conversion into TEXT, as with
    // TSQLTableDB) - so will work for sftBlob, sftBlobDynArray and sftBlobRecord
    // - returns the number of data rows added to JSON (excluding the headers)
    function Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; JSON: TStream;
      Expand: boolean=false): PtrInt; overload;
    /// Execute one SQL statement which return the results in JSON format
    // - use internaly Execute() above with a TRawByteStringStream, and return a string
    // - BLOB field value is saved as Base64, e.g. '"\uFFF0base64encodedbinary"'
    // - returns the number of data rows added to JSON (excluding the headers)
    // in the integer variable mapped by aResultCount (if any)
    // - if any error occurs, the ESQLite3Exception is handled and '' is returned
    function ExecuteJSON(aDB: TSQLite3DB; const aSQL: RawUTF8; Expand: boolean=false;
      aResultCount: PPtrInt=nil): RawUTF8;
    {/ Execute all SQL statements in the aSQL UTF-8 encoded string, results will
      be written as ANSI text in OutFile }
    procedure ExecuteDebug(aDB: TSQLite3DB; const aSQL: RawUTF8; var OutFile: Text);
    {/ close the Request handle
     - call it even if an ESQLite3Exception has been raised }
    procedure Close;

    {/ read-only access to the Request (SQLite3 statement) handle }
    property Request: TSQLite3Statement read fRequest;
    {/ read-only access to the SQLite3 database handle }
    property RequestDB: TSQLite3DB read fDB;
    {/ returns true if the current prepared statement makes no direct changes
      to the content of the database file
     - Transaction control statements such as BEGIN, COMMIT, ROLLBACK, SAVEPOINT,
     and RELEASE cause this property to return true, since the statements
     themselves do not actually modify the database but rather they control the
     timing of when other statements modify the database. The ATTACH and DETACH
     statements also cause this property to return true since, while
     those statements change the configuration of a database connection, they
     do not make changes to the content of the database files on disk. }
    property IsReadOnly: Boolean read GetReadOnly;

  // 2. Bind parameters to a SQL query (for the last prepared statement)
  public
    {/ Reset All Bindings On A Prepared Statement
     - Contrary to the intuition of many, Reset() does not reset the bindings
      on a prepared statement. Use this routine to reset all host parameter }
    procedure BindReset;
    {/ bind a NULL value to a parameter
     - the leftmost SQL parameter has an index of 1, but ?NNN may override it
     - raise an ESQLite3Exception on any error }
    procedure BindNull(Param: Integer); 
    {/ bind an integer value to a parameter
     - the leftmost SQL parameter has an index of 1, but ?NNN may override it
     - raise an ESQLite3Exception on any error }
    procedure Bind(Param: Integer; Value: Int64); overload;
    {/ bind a double value to a parameter
     - the leftmost SQL parameter has an index of 1, but ?NNN may override it
     - raise an ESQLite3Exception on any error }
    procedure Bind(Param: Integer; Value: double); overload;
    {/ bind a UTF-8 encoded string to a parameter
     - the leftmost SQL parameter has an index of 1, but ?NNN may override it
     - raise an ESQLite3Exception on any error }
    procedure Bind(Param: Integer; const Value: RawUTF8); overload;
    {/ bind a Blob buffer to a parameter
     - the leftmost SQL parameter has an index of 1, but ?NNN may override it
     - raise an ESQLite3Exception on any error }
    procedure Bind(Param: Integer; Data: pointer; Size: integer); overload;
    {/ bind a Blob TCustomMemoryStream buffer to a parameter
     - the leftmost SQL parameter has an index of 1, but ?NNN may override it
     - raise an ESQLite3Exception on any error }
    procedure Bind(Param: Integer; Data: TCustomMemoryStream); overload;
    {/ bind a ZeroBlob buffer to a parameter
     - uses a fixed amount of memory (just an integer to hold its size) while
      it is being processed. Zeroblobs are intended to serve as placeholders
      for BLOBs whose content is later written using incremental BLOB I/O routines
      (as with TSQLBlobStream created from TSQLDataBase.Blob() e.g.).
     - a negative value for the Size parameter results in a zero-length BLOB
     - the leftmost SQL parameter has an index of 1, but ?NNN may override it
     - raise an ESQLite3Exception on any error }
    procedure BindZero(Param: Integer; Size: integer);

  // 3. Field attributes after a sucessfull Step() (returned SQLITE_ROW)
  public
    {/ the field name of the current ROW  }
    function FieldName(Col: integer): RawUTF8;
    {/ the field index matching this name
      - return -1 if not found }
    function FieldIndex(const aColumnName: RawUTF8): integer;
    {/ return the field as a sqlite3_value object handle, first Col is 0 }
    function FieldValue(Col: integer): TSQLite3Value;
    {/ return a field integer value, first Col is 0 }
    function FieldInt(Col: integer): Int64;
    {/ return a field floating point value, first Col is 0 }
    function FieldDouble(Col: integer): double;
    {/ return a field UTF-8 encoded text value, first Col is 0 }
    function FieldUTF8(Col: integer): RawUTF8;
    {/ return a field as Win-Ansi (i.e. code page 1252) encoded text value, first Col is 0 }
    function FieldA(Col: integer): WinAnsiString;
    {/ return a field RawUnicode encoded text value, first Col is 0 }
    function FieldW(Col: integer): RawUnicode;
    {/ return a field as a blob value (RawByteString/TSQLRawBlob is an AnsiString),
      first Col is 0 }
    function FieldBlob(Col: integer): RawByteString;
    {/ return a field as a TStream blob value, first Col is 0
     - caller shall release the returned TStream instance }
    function FieldBlobToStream(Col: integer): TStream;
    {/ return TRUE if the column value is NULL, first Col is 0 }
    function FieldNull(Col: Integer): Boolean;
    {/ return the field type of this column
     - retrieve the "SQLite3" column type as returned by sqlite3_column_type -
       i.e. SQLITE_NULL, SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT, or SQLITE_BLOB  }
    function FieldType(Col: Integer): integer;
    {/ append all columns values of the current Row to a JSON stream
     - will use WR.Expand to guess the expected output format
     - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
       format and contains true BLOB data }
    procedure FieldsToJSON(WR: TJSONWriter);
    {/ the column/field count of the current ROW
     - fields numerotation starts with 0 }
    property FieldCount: integer read fFieldCount;
    {/ the bound parameters count }
    property ParamCount: integer read GetParamCount;
  end;

  /// used to retrieve a prepared statement
  TSQLStatementCache = record
    /// associated SQL statement
    StatementSQL: RawUTF8;
    /// associated prepared statement, ready to be executed after binding
    Statement: TSQLRequest;
  end;
  /// used to store all prepared statement
  TSQLStatementCacheDynArray = array of TSQLStatementCache;

  /// handle a cache of prepared statements
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef UNICODE}
  TSQLStatementCached = record
  {$else}
  TSQLStatementCached = object
  {$endif}
    /// prepared statements with parameters for faster SQLite3 execution
    // - works for SQL code with ? internal parameters
    Cache: TSQLStatementCacheDynArray;
    /// current number of items in the Cache[] array
    Count: integer;
    /// hashing wrapper associated to the Cache[] array
    Caches: TDynArrayHashed;
    /// the associated SQLite3 database instance
    DB: TSQLite3DB;
    /// intialize the cache
    procedure Init(aDB: TSQLite3DB);
    /// add or retrieve a generic SQL (with ? parameters) statement from cache
    function Prepare(const GenericSQL: RaWUTF8): PSQLRequest;
    // used internaly to release all prepared statements from Cache[]
    procedure ReleaseAllDBStatements;
  end;

  /// those classes can be used to define custom SQL functions inside a TSQLDataBase
  TSQLDataBaseSQLFunction = class
  protected
    fInternalFunction: TSQLFunctionFunc;
    fSQLName: RawUTF8;
    fFunctionParametersCount: integer;
    function CreateFunction(DB: TSQLite3DB): Integer; virtual;
  public
    /// initialize the corresponding SQL function
    // - expects at least the low-level TSQLFunctionFunc implementation (in
    // sqlite3_create_function_v2() format) and the number of expected parameters
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    constructor Create(aFunction: TSQLFunctionFunc; aFunctionParametersCount: Integer;
      const aFunctionName: RawUTF8=''); reintroduce;
    /// the internal function prototype
    // - ready to be assigned to sqlite3_create_function_v2() xFunc parameter
    property InternalFunction: TSQLFunctionFunc read fInternalFunction;
    /// the SQL function name, as called from the SQL statement
    // - the same function name may be registered several times with a diverse
    // number of parameters (e.g. to implement optional parameters)
    property FunctionName: RawUTF8 read fSQLName;
    /// the number of parameters expected by the SQL function
    property FunctionParametersCount: integer read fFunctionParametersCount;
  end;

  /// to be used to define custom SQL functions for dynamic arrays BLOB search
  TSQLDataBaseSQLFunctionDynArray = class(TSQLDataBaseSQLFunction)
  protected
    fDummyDynArray: TDynArray;
    fDummyDynArrayValue: pointer;
  public
    /// initialize the corresponding SQL function
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    // - the SQL function will expect two parameters: the first is the BLOB
    // field content, and the 2nd is the array element to search (set with
    // TDynArray.ElemSave() or with BinToBase64WithMagic(aDynArray.ElemSave())
    // if called via a Client and a JSON prepared parameter)
    // - you should better use the already existing faster SQL functions
    // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains() if possible
    // (this implementation will allocate each dynamic array into memory before
    // comparison, and will be therefore slower than those optimized versions)   
    constructor Create(aTypeInfo: pointer; aCompare: TDynArraySortCompare;
      const aFunctionName: RawUTF8=''); reintroduce;
  end;

  /// Stored Procedure prototype, used by TSQLDataBase.Execute() below
  // - called for every row of a Statement
  // - the implementation may update the database directly by using a
  // local or shared TSQLRequest
  // - the TSQLRequest may be shared and prepared before the call for even
  // faster access than with a local TSQLRequest 
  // - no TSQLDataBase or higher levels objects can be used inside this method,
  // since all locking and try..finally protection is outside it
  // - can optionnaly trigger a ESQLite3Exception on any error
  TOnSQLStoredProc = procedure(Statement: TSQLRequest) of object;

  {/ TSQLDataBase.TransactionBegin can be deferred, immediate, or exclusive
   - tbDeferred means that no locks are acquired on the database until the
   database is first accessed. Thus with a deferred transaction, the BEGIN
   statement itself does nothing to the filesystem. Locks are not acquired
   until the first read or write operation. The first read operation against
   a database creates a SHARED lock and the first write operation creates a
   RESERVED lock. Because the acquisition of locks is deferred until they are
   needed, it is possible that another thread or process could create a
   separate transaction and write to the database after the BEGIN on the
   current thread has executed.
   - If the transaction is tbImmediate, then RESERVED locks are acquired
   on all databases as soon as the BEGIN command is executed, without waiting
   for the database to be used. After a BEGIN IMMEDIATE, no other database
   connection will be able to write to the database or do a BEGIN IMMEDIATE
   or BEGIN EXCLUSIVE. Other processes can continue to read from the database,
   however.
   - A tbExclusive transaction causes EXCLUSIVE locks to be acquired on all
   databases. After a BEGIN EXCLUSIVE, no other database connection except
   for read_uncommitted connections will be able to read the database and
   no other connection without exception will be able to write the database
   until the transaction is complete. }
  TSQLDataBaseTransactionBehaviour = (
    tbDeferred,
    tbImmediate,
    tbExclusive);

  /// simple wrapper for direct SQLite3 database manipulation
  // - embed the SQLite3 database calls into a common object
  // - thread-safe call of all SQLite3 queries (SQLITE_THREADSAFE 0 in sqlite.c)
  // - can cache last results for SELECT statements, if property UseCache is true:
  //  this can speed up most read queries, for web server or client UI e.g.
  TSQLDataBase = class
  private
    fDB: TSQLite3DB;
    fFileName: TFileName;
    fCypherBlock: RawByteString;
    fTransactionActive: boolean;
    fLock: TRTLCriticalSection;
    /// if not nil, cache is used - see UseCache property
    fCache: TSynCache;
    fInternalState: PCardinal;
    fBusyTimeout: Integer;
    fLog: TSynLog;
    /// store TSQLDataBaseSQLFunction instances
    fSQLFunctions: TObjectList;
    function GetUseCache: boolean;
    procedure SetUseCache(const Value: boolean);
    procedure SetBusyTimeout(const ms: Integer);
    function GetUserVersion: cardinal;
    procedure SetUserVersion(const Value: cardinal);
    procedure SetWALMode(Value: Boolean);
    function GetWALMode: boolean;
    procedure SetSynchronous(const Value: TSQLSynchronousMode);
    function GetSynchronous: TSQLSynchronousMode;
  public
    /// enter the TRTLCriticalSection: called before any DB access
    // - provide the SQL statement about to be executed: handle proper caching
    // - is the SQL statement is void, assume a SELECT statement (no cache flush)
    procedure Lock(const aSQL: RawUTF8);
    /// leave the TRTLCriticalSection: called after any DB access
    procedure UnLock;
    /// enter the TRTLCriticalSection: called before any DB access
    // - provide the SQL statement about to be executed: handle proper caching
    // - if this SQL statement has an already cached JSON response, return it and
    // don't enter the TRTLCriticalSection: no UnLockJSON() call is necessary
    // - if this SQL statement is not a SELECT, cache is flushed and
    // the next call to UnLockJSON() won't add any value to the cache since
    // this statement is not a SELECT and doesn't have to be cached!
    // - if aResultCount does map to an integer variable, it will be filled
    // with the returned row count of data (excluding field names) in the result
    function LockJSON(const aSQL: RawUTF8; aResultCount: PPtrInt): RawUTF8;
    /// leave the TRTLCriticalSection: called after any DB access
    // - caller must provide the JSON result for the SQL statement previously set
    //  by LockJSON()
    // - do proper caching of the JSON response for this SQL statement
    procedure UnLockJSON(const aJSONResult: RawUTF8; aResultCount: PtrInt);
    /// (re)open the database from file fFileName
    // - TSQLDatabase.Create already opens the database: this method is to be
    // used only on particular cases, e.g. to close temporary a DB file and
    // allow making a backup on its content
    function DBOpen: integer;
    /// close the opened database
    // - TSQLDatabase.Destroy already closes the database: this method is to be
    // used only on particular cases, e.g. to close temporary a DB file and
    // allow making a backup on its content
    procedure DBClose;
  public
    {/ open a SQLite3 database file
     - open an existing database file or create a new one if no file exists
     - if specified, the password will be used to cypher this file on disk
       (the main SQLite3 database file is encrypted, not the wal file during run)
     - SYSTEMNOCASE collation is added (our custom fast UTF-8 case insensitive compare,
       which is used also in the SQLite3UI unit for coherency and efficiency)
     - ISO8601 collation is added (TDateTime stored as ISO-8601 encoded TEXT)
     - WIN32CASE and WIN32NOCASE collations are added (use slow but accurate Win32 CompareW)
     - some additional SQl functions are registered: MOD, SOUNDEX/SOUNDEXFR/SOUNDEXES,
       RANK, CONCAT
     - initialize a TRTLCriticalSection to ensure that all access to the database is atomic
     - raise an ESQLite3Exception on any error }
    constructor Create(const aFileName: TFileName; const aPassword: RawUTF8='');
    {/ close a database and free its memory and context
      - if TransactionBegin was called but not commited, a RollBack is performed }
    destructor Destroy; override;
    {/ Execute all SQL statements in aSQL UTF-8 encoded string
     - can be prepared with TransactionBegin()
     - raise an ESQLite3Exception on any error }
    procedure ExecuteAll(const aSQL: RawUTF8);
    {/ Execute one SQL statements in aSQL UTF-8 encoded string
     - can be prepared with TransactionBegin()
     - raise an ESQLite3Exception on any error }
    procedure Execute(const aSQL: RawUTF8); overload;
    {/ Execute one SQL statement which return integers from the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get a one field/column result of INTEGER
     - return result as a dynamic array of RawUTF8, as TEXT result
     - return count of row in integer function result (may be < length(ID))
     - raise an ESQLite3Exception on any error }
    function Execute(const aSQL: RawUTF8; var ID: TInt64DynArray): integer; overload;
    {/ Execute one SQL statement returning TEXT from the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get (at least) one field/column result of TEXT
     - return result as a dynamic array of RawUTF8 in ID
     - return count of row in integer function result (may be < length(ID))
     - raise an ESQLite3Exception on any error }
    function Execute(const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer; overload;
    {/ Execute one SQL statement which returns one integer from the aSQL UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get a one field/column result of INTEGER
     - raise an ESQLite3Exception on any error }
    procedure Execute(const aSQL: RawUTF8; out ID: Int64); overload;
    {/ Execute one SQL statement which returns one UTF-8 encoded string value
     - Execute the first statement in aSQL
     - this statement must get a one field/column result of TEXT
     - raise an ESQLite3Exception on any error }
    procedure Execute(const aSQL: RawUTF8; out ID: RawUTF8); overload;
    {/ Execute one SQL statements in aSQL UTF-8 encoded string
     - can be prepared with TransactionBegin()
     - raise no Exception on error, but returns FALSE in such case }
    function ExecuteNoException(const aSQL: RawUTF8): boolean; overload;
    {/ Seamless execution of a SQL statement which returns one integer
     - Execute the first statement in aSQL
     - this statement must get a one field/column result of INTEGER
     - returns 0 on any error }
    procedure ExecuteNoException(const aSQL: RawUTF8; out ID: Int64); overload;
    {/ Seamless execution of a SQL statement which returns one UTF-8 encoded string
     - Execute the first statement in aSQL
     - this statement must get a one field/column result of TEXT
     - returns '' on any error }
    procedure ExecuteNoException(const aSQL: RawUTF8; out ID: RawUTF8); overload;
    /// Execute one SQL statement returning its results in JSON format
    // - the BLOB data is encoded as '"\uFFF0base64encodedbinary"'
    function ExecuteJSON(const aSQL: RawUTF8; Expand: boolean=false; aResultCount: PPtrInt=nil): RawUTF8;
    {/ begin a transaction
     - Execute SQL statements with Execute() procedure below
     - must be ended with Commit on success
     - must be aborted with Rollback after an ESQLite3Exception raised
    - The default transaction behavior is tbDeferred }
    procedure TransactionBegin(aBehavior: TSQLDataBaseTransactionBehaviour = tbDeferred);
    {/ end a transaction: write all Execute() statements to the disk }
    procedure Commit;
    {/ abort a transaction: restore the previous state of the database }
    procedure RollBack;
    {/ return the last Insert Rowid }
    function LastInsertRowID: Int64;
    {/ count the number of rows modified by the last SQL statement
     - this method returns the number of database rows that were changed or
     inserted or deleted by the most recently completed SQL statement on the
     database connection specified by the first parameter. Only changes that
     are directly specified by the INSERT, UPDATE, or DELETE statement are counted.
     - wrapper around the sqlite3_changes() low-level function }
    function LastChangeCount: integer;

    {/ get all table names contained in this database file }
    procedure GetTableNames(var Names: TRawUTF8DynArray);
    {/ get all field names for a specified Table }
    procedure GetFieldNames(var Names: TRawUTF8DynArray; const TableName: RawUTF8);
    /// add a SQL custom function to the SQLite3 database engine
    // - the supplied aFunction instance will be used globally and freed
    // by TSQLDataBase.Destroy destructor
    // - will do nothing if the same function name and parameters count have
    // already been registered (you can register then same function name with
    // several numbers of parameters)
    // - you may use the overloaded function, which is a wrapper around:
    // ! Demo.RegisterSQLFunction(
    // !   TSQLDataBaseSQLFunction.Create(InternalSQLFunctionCharIndex,2,'CharIndex'));
    procedure RegisterSQLFunction(aFunction: TSQLDataBaseSQLFunction); overload;
    /// add a SQL custom function to the SQLite3 database engine
    // - will do nothing if the same function name and parameters count have
    // already been registered (you can register then same function name with
    // several numbers of parameters)
    // - typical use may be:
    // ! Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,2,'CharIndex');
    procedure RegisterSQLFunction(aFunction: TSQLFunctionFunc;
      aFunctionParametersCount: Integer; const aFunctionName: RawUTF8); overload;
    /// add a SQL custom function for a dynamic array to the database 
    // - the resulting SQL function will expect two parameters: the first is the
    // BLOB field content, and the 2nd is the array element to search (as set with
    // TDynArray.ElemSave() or with BinToBase64WithMagic(aDynArray.ElemSave())
    // if called via a Client and a JSON prepared parameter)
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    // - you should better use the already existing faster SQL functions
    // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains() if possible
    // (this implementation will allocate each dynamic array into memory before
    // comparison, and will be therefore slower than those optimized versions -
    // but it will be always faster than Client-Server query, in all cases)   
    procedure RegisterSQLFunction(aDynArrayTypeInfo: pointer; aCompare: TDynArraySortCompare;
      const aFunctionName: RawUTF8=''); overload;

    {/ open a BLOB incrementally for read[/write] access
     - find a BLOB located in row RowID, column ColumnName, table TableName
      in database DBName; in other words, the same BLOB that would be selected by:
      ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID;
     - use after a TSQLRequest.BindZero() to reserve Blob memory
     - if RowID=-1, then the last inserted RowID is used
     - will raise an ESQLite3Exception on any error }
    function Blob(const DBName, TableName, ColumnName: RawUTF8;
      RowID: Int64=-1; ReadWrite: boolean=false): TSQLBlobStream;
    {/ backup of the opened Database into an external file name
     - don't use the experimental SQLite Online Backup API
     - database is closed, VACCUUMed, copied, then reopened: it's very fast  }
    function Backup(const BackupFileName: TFileName): boolean;
    /// flush the internal SQL-based JSON cache content
    // - to be called when the regular Lock/LockJSON methods are not called,
    // e.g. with external tables as defined in SQLite3DB unit
    // - will also increment the global InternalState property value (if set) 
    procedure CacheFlush;

    {/ read-only access to the SQLite3 database handle }
    property DB: TSQLite3DB read fDB;
    {/ access to the log instance associated with this SQLite3 database engine }
    property Log: TSynLog read fLog;
    {/ read-only access to the SQLite3 database filename opened }
    property FileName: TFileName read fFileName;
    /// if this property is set, all ExecuteJSON() responses will be cached
    // - cache is flushed on any write access to the DB (any not SELECT statement)
    // - cache is consistent only if ExecuteJSON() Expand parameter is constant
    // - cache is used by TSQLDataBase.ExecuteJSON() and TSQLTableDB.Create()
    property UseCache: boolean read GetUseCache write SetUseCache;
    /// access to the internal JSON cache, used by ExecuteJSON() method
    // - see UseCache property and CacheFlush method
    property Cache: TSynCache read fCache;
    /// this integer pointer (if not nil) is incremented when any SQL statement
    // changes the database contents (i.e. any not SELECT statement)
    // - this pointer is thread-safe updated, inside a critical section
    property InternalState: PCardinal read fInternalState write fInternalState;
    /// return TRUE if a Transaction begun
    property TransactionActive: boolean read fTransactionActive;
    /// sets a busy handler that sleeps for a specified amount of time
    // (in milliseconds) when a table is locked, before returning an error
    property BusyTimeout: Integer read fBusyTimeout write SetBusyTimeout;
    {/ query or change the Write-Ahead Logging mode for the database
      - beginning with version 3.7 of the SQLite3 engine, a new "Write-Ahead Log"
        option (hereafter referred to as "WAL") is optionaly available
      - WAL might be very slightly slower (perhaps 1% or 2% slower) than the
        traditional rollback-journal approach in applications that do mostly reads
        and seldom write; but WAL provides more concurrency as readers do not block
        writers and a writer does not block readers. Reading and writing can
        proceed concurrently. With our SQLite3 framework, it's not needed.
      - by default, this option is not set: only implement if you really need it,
        but our SQlite3 framework use locked access to the databse, so there
        should be no benefit of WAL for the framework; but if you call
        directly TSQLDatabase instances in your code, it may be useful to you }
    property WALMode: Boolean read GetWALMode write SetWALMode;
    /// query or change the SQlite3 file-based syncrhonization mode, i.e. the
    // way it waits for the data to be flushed on hard drive
    // - default smFull is very slow, but achieve 100% ACID behavior
    // - smNormal is faster, and safe until a catastrophic hardware failure occurs
    // - smOff is the fastest, data should be safe if the application crashes,
    // but database file may be corrupted in case of failure at the wrong time
    property Synchronous: TSQLSynchronousMode read GetSynchronous write SetSynchronous;
    {/ retrieve or set the user_version stored in the SQLite3 database file
      - user-version is a 32-bit signed integer stored in the database header
      - it can be used to change the database in case of format upgrade (e.g.
        refresh some hand-made triggers) }
    property user_version: cardinal read GetUserVersion write SetUserVersion;
  end;

  /// used to read or write a BLOB Incrementaly
  // - data is read/written directly from/to the SQLite3 BTree
  // - data can be written after a TSQLRequest.BindZero() call to reserve memory
  // - this TStream has a fixed size, but Position property can be used to rewind
  TSQLBlobStream = class(TStream)
  protected
    fBlob: TSQLite3Blob;
    fDB: TSQLite3DB;
    fSize,
    fPosition: longint;
    fWritable: boolean;
  public
    {/ Opens a BLOB located in row RowID, column ColumnName, table TableName
    in database DBName; in other words, the same BLOB that would be selected by:
    ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID; }
    constructor Create(aDB: TSQLite3DB; const DBName, TableName,
      ColumnName: RawUTF8; RowID: Int64; ReadWrite: boolean);
    {/ release the BLOB object }
    destructor Destroy; override;
    {/ read Count bytes from the opened BLOB in Buffer }
    function Read(var Buffer; Count: Longint): Longint; override;
    {/ write is allowed for in-place replacement (resizing is not allowed)
     - Create() must have been called with ReadWrite=true }
    function Write(const Buffer; Count: Longint): Longint; override;
    {/ change the current read position }
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {/ read-only access to the BLOB object handle }
    property Handle: TSQLite3Blob read fBlob;
  end;

{$ifdef WITHLOG}
var
  /// the TSynLog class used for logging for all our SynSQlite3 related functions
  // - you may override it with TSQLLog, if available from SQLite3Commons
  // - since not all exceptions are handled specificaly by this unit, you
  // may better use a common TSynLog class for the whole application or module
  SynSQLite3Log: TSynLogClass = TSynLog;
{$endif}


const
  /// encryption XOR mask table size (in bytes)
  // - must be a power of 2
  // - bigger encryption table makes stronger encryption, but use more memory
  // - it's faster when the mask table can stay in the CPU L1 cache
  // - default size is therefore 16KB
  SQLEncryptTableSize = $4000;

var
  /// in order to allow file encryption on disk, initialize this pointer
  // with SQLEncryptTableSize bytes of XOR tables
  // - you can use fixed or custom (SHA+AES) generated table
  // - using a fixed XOR table is very fast and provides strong enough encryption
  // - the first page (first 1024 bytes) is not encrypted, since its content
  // (mostly zero) can be used to easily guess the beginning of the key
  // - if the key is not correct, a ESQLite3Exception will be raised with
  // 'database disk image is malformed' (ErrorCode=SQLITE_CORRUPT)
  // - this table is common to ALL files accessed by the database engine: you
  // have maintain several XOR mask arrays, and set SQLEncryptTable before any
  // sqlite3*() call, to mix passowords or crypted and uncrypted databases
  // (see ChangeSQLEncryptTablePassWord() for multiple SQLEncryptTable use)
  // - please note that this encryption is compatible only with SQlite3 files
  // using the default page size of 1024
  SQLEncryptTable: PByteArray = nil;

{type
  /// this function prototype is called for on-the-fly encryption of a 1KB page
  // - p is a buffer containing the 1024 bytes of data to be encrypted
  // - PageIndex is the 1024 bytes length page number
  // - Encrypt is true to cypher, false to uncypher
  TSQLEncryptFunc: procedure(p: pByte; PageIndex: cardinal; Encrypt: boolean);

var
  /// in order to allow file encryption on disk, initialize this pointer
  // with a TSQLEncryptFunc procedure for per page encryption
  // - use either SQLEncryptFunc, either SQLEncryptTable
  // - SQLEncryptFunc has a page orientation, SQLEncryptTable a byte-orientation
  SQLEncryptFunc: TSQLEncryptFunc;}

/// you can use this simple (and strong enough) procedure for easy SQL encryption
// - usage is global for ALL SQLite3 databases access
// - specify an ascii or binary password
// - a buffer is allocated and initialized in SQLEncryptTable
// - call with Password='' to end up encryption
// - you may use instead SynCrypto unit for more secure SHA-256 and AES-256 algos
// - please note that this encryption is compatible only with SQlite3 files
// using the default page size of 1024
procedure CreateSQLEncryptTable(const PassWord: RawUTF8);

/// use this procedure to change the password for an existing SQLite3 database file
// - conversion is done in-place, therefore this procedure can handle very big files
// - the OldPassWord must be correct, otherwize the resulting file will be corrupted
// - any password can be '' to mark no encryption
// - you may use instead SynCrypto unit for more secure SHA-256 and AES-256 algos
// - please note that this encryption is compatible only with SQlite3 files
// using the default page size of 1024
procedure ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUTF8);

/// check from the file beginning if sounds like a valid SQLite3 file
// - since encryption starts only with the 2nd page, this function will
// return true if a database file is encrypted or not
function IsSQLite3File(const FileName: TFileName): boolean;

/// check if sounds like an encrypted SQLite3 file
// - this will check the 2nd file page beginning to be a valid B-TREE page
// - in some cases, may return false negatives (depending on the password used)
function IsSQLite3FileEncrypted(const FileName: TFileName): boolean;


implementation



{ ************ direct access to sqlite3.c / sqlite3.obj consts and functions }
{
  Code below will link all database engine, from amalgamation source file:

 - compiled with free Borland C++ compiler 5.5.1 from the command line:
     \dev\bcc\bin\bcc32 -6 -O2 -c -d -u- sqlite3.c
 - FastCall use must be set with defining USEFASTCALL above, and
     int __cdecl fts3CompareElemByTerm(const void *lhs, const void *rhs)
     \dev\bcc\bin\bcc32 -6 -O2 -c -d -pr -u- sqlite3.c
 - the following defines must be added in the beginning of the sqlite3.c file:

//#define SQLITE_ENABLE_FTS3
//  this unit is FTS3-ready, but not compiled with it by default
//  if you don't use FTS3, dont define this conditional: you'll spare 50KB of code
//  this conditional is defined at compile time, in order to create sqlite3fts3.obj
#define SQLITE_DEFAULT_MEMSTATUS 0
//  don't need any debug here
#define SQLITE_THREADSAFE 2
//  assuming multi-thread safety is made by caller - in our framework, there is
// only one thread using the database connection at the same time, but there could
// be multiple database connection at the same time (previous was 0 could be unsafe)
#define SQLITE_OMIT_SHARED_CACHE 1
// no need of shared cache in a threadsafe calling model
#define SQLITE_OMIT_AUTOINIT 1
//  sqlite3_initialize() is done in initialization section below -> no AUTOINIT
#define SQLITE_OMIT_DEPRECATED 1
//  spare some code size - is now defined only if compiled without FTS3/FTS4
#ifndef SQLITE_ENABLE_FTS3
  #define SQLITE_OMIT_TRACE 1
#endif
// we don't need sqlite3_profile() and sqlite3_trace() interfaces
#define SQLITE_OMIT_LOAD_EXTENSION 1
// we don't need extension in an embedded engine
#define SQLITE_OMIT_COMPILEOPTION_DIAGS 1
// we don't need Compilation Options Diagnostics in our embedded engine
#define SQLITE_OMIT_PROGRESS_CALLBACK 1
// we don't need sqlite3_progress_handler() API function
#define SQLITE_ENABLE_RTREE 1
// the RTREE extension is now (from v.1.8/3.7) compiled into the engine
//#define SQLITE_OMIT_LOOKASIDE
// even if we use FastMM4/SynScaleMM, LookAside seems mandatory in c source
#define SQLITE_WITHOUT_MSIZE
// _msize() is not available (nor needed) with FastMM4 memory manager

and, in the sqlite3.c source file, the following functions are made external
in order to allow our proprietary but simple and efficient encryption system:

extern int winRead(
  sqlite3_file *id,          /* File to read from */
  void *pBuf,                /* Write content into this buffer */
  int amt,                   /* Number of bytes to read */
  sqlite3_int64 offset       /* Begin reading at this offset */
);

extern int winWrite(
  sqlite3_file *id,         /* File to write into */
  const void *pBuf,         /* The bytes to be written */
  int amt,                  /* Number of bytes to write */
  sqlite3_int64 offset      /* Offset into the file to begin writing at */
);

}

{$ifdef INCLUDE_FTS3}
{$L sqlite3fts3.obj}   // link SQlite3 database engine with FTS3/FTS4 + TRACE
{$else}
{$L sqlite3.obj}       // link SQlite3 database engine
{$endif}


// we then implement all needed Borland C++ runtime functions in pure pascal:

function _ftol: Int64;
// Borland C++ float to integer (Int64) conversion
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

function _ftoul: Int64;
// Borland C++ float to integer (Int64) conversion
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

function malloc(size: cardinal): Pointer; cdecl; { always cdecl }
// the SQLite3 database engine will use the FastMM4/SynScaleMM fast heap manager
begin
  GetMem(Result, size);
end;

procedure free(P: Pointer); cdecl; { always cdecl }
// the SQLite3 database engine will use the FastMM4 very fast heap manager
begin
  FreeMem(P);
end;

function realloc(P: Pointer; Size: Integer): Pointer; cdecl; { always cdecl }
// the SQLite3 database engine will use the FastMM4/SynScaleMM very fast heap manager
begin
  result := P;
  ReallocMem(result,Size);
end;

function memset(P: Pointer; B: Integer; count: Integer): pointer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  result := P;
  FillChar(P^, count, B);
end;

procedure memmove(dest, source: pointer; count: Integer); cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  Move(source^, dest^, count); // move() is overlapping-friendly
end;

procedure memcpy(dest, source: Pointer; count: Integer); cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  Move(source^, dest^, count);
end;

function atol(P: pointer): integer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  result := GetInteger(P);
end;

var __turbofloat: word; { not used, but must be present for linking }

// Borland C++ and Delphi share the same low level Int64 _ll*() functions:

procedure _lldiv;
asm
  jmp System.@_lldiv
end;

procedure _lludiv;
asm
  jmp System.@_lludiv
end;

procedure _llmod;
asm
  jmp System.@_llmod
end;

procedure _llmul;
asm
  jmp System.@_llmul
end;

procedure _llumod;
asm
  jmp System.@_llumod
end;

procedure _llshl;
asm
  jmp System.@_llshl
end;

procedure _llshr;
asm
{$ifndef ENHANCEDRTL} // need this code for Borland/CodeGear default System.pas
  shrd    eax, edx, cl
  sar     edx, cl
  cmp     cl, 32
  jl      @@Done
  cmp     cl, 64
  jge     @@RetSign
  mov     eax, edx
  sar     edx, 31
  ret
@@RetSign:
  sar     edx, 31
  mov     eax, edx
@@Done:
{$else}
  // our customized System.pas didn't forget to put _llshr in its interface :)
  jmp System.@_llshr
{$endif}
end;

procedure _llushr;
asm
  jmp System.@_llushr
end;

function strlen(p: PAnsiChar): integer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin // called only by some obscure FTS3 functions (normal code use dedicated functions)
  result := SynCommons.StrLen(pointer(p));
end;

function memcmp(p1, p2: pByte; Size: integer): integer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  if (p1<>p2) and (Size<>0) then
    if p1<>nil then
      if p2<>nil then begin
        repeat
          if p1^<>p2^ then begin
            result := p1^-p2^;
            exit;
          end;
          dec(Size);
          inc(p1);
          inc(p2);
        until Size=0;
        result := 0;
      end else
      result := 1 else
    result := -1 else
  result := 0;
end;

function strncmp(p1, p2: PByte; Size: integer): integer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
var i: integer;
begin
  for i := 1 to Size do begin
    result := p1^-p2^;
    if (result<>0) or (p1^=0) then
      exit;
    inc(p1);
    inc(p2);
  end;
  result := 0;
end;

// qsort() is used if SQLITE_ENABLE_FTS3 is defined
type // this function type is defined for calling termDataCmp() in sqlite3.c 
  qsort_compare_func = function(P1,P2: pointer): integer; cdecl; { always cdecl }

procedure QuickSort4(base: PPointerArray; L, R: Integer; comparF: qsort_compare_func);
var I, J, P: Integer;
    PP, C: PAnsiChar;
begin
  repeat // from SQLite (FTS), With=sizeof(PAnsiChar) AFAIK
    I := L;
    J := R;
    P := (L+R) shr 1;
    repeat
      PP := @base[P];
      while comparF(@base[I],PP)<0 do
        inc(I);
      while comparF(@base[J],PP)>0 do
        dec(J);
      if I<=J then begin
        C := base[I];
        base[I] := base[J];
        base[J] := C; // fast memory exchange
        if P=I then P := J else if P=J then P := I;
        inc(I);
        dec(J);
      end;
    until I>J;
    if L<J then
      QuickSort4(base, L, J, comparF);
    L := I;
  until I>=R;
end;

procedure QuickSort(baseP: PAnsiChar; Width: integer; L, R: Integer; comparF: qsort_compare_func);
// code below is very fast and optimized
  procedure Exchg(P1,P2: PAnsiChar; Size: integer);
  var B: AnsiChar;
      i: integer;
  begin
    for i := 0 to Size-1 do begin
      B := P1[i];
      P1[i] := P2[i];
      P2[i] := B;
    end;
  end;
var I, J, P: Integer;
    PP, C: PAnsiChar;
begin
  repeat // generic sorting algorithm
    I := L;
    J := R;
    P := (L+R) shr 1;
    repeat
      PP := baseP+P*Width; // compute PP at every loop, since P may change
      C := baseP+I*Width;
      while comparF(C,PP)<0 do begin
        inc(I);
        inc(C,width); // avoid slower multiplication in loop
      end;
      C := baseP+J*Width;
      while comparF(C,PP)>0 do begin
        dec(J);
        dec(C,width); // avoid slower multiplication in loop
      end;
      if I<=J then begin
        Exchg(baseP+I*Width,baseP+J*Width,Width); // fast memory exchange
        if P=I then P := J else if P=J then P := I;
        inc(I);
        dec(J);
      end;
    until I>J;
    if L<J then
      QuickSort(baseP, Width, L, J, comparF);
    L := I;
  until I>=R;
end;

procedure qsort(baseP: pointer; NElem, Width: integer; comparF: qsort_compare_func);
  cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
begin
  if (cardinal(NElem)>1) and (Width>0) then
    if Width=sizeof(pointer) then
      QuickSort4(baseP, 0, NElem-1, comparF) else
      QuickSort(baseP, Width, 0, NElem-1, comparF);
end;

var
  { as standard C library documentation states:
  Statically allocated buffer, shared by the functions gmtime() and localtime().
  Each call of these functions overwrites the content of this structure.
  -> since timing is not thread-dependent, it's OK to share this buffer :) }
  atm: packed record
    tm_sec: Integer;            { Seconds.      [0-60] (1 leap second) }
    tm_min: Integer;            { Minutes.      [0-59]  }
    tm_hour: Integer;           { Hours.        [0-23]  }
    tm_mday: Integer;           { Day.          [1-31]  }
    tm_mon: Integer;            { Month.        [0-11]  }
    tm_year: Integer;           { Year          - 1900. }
    tm_wday: Integer;           { Day of week.  [0-6]   }
    tm_yday: Integer;           { Days in year. [0-365] }
    tm_isdst: Integer;          { DST.          [-1/0/1]}
    __tm_gmtoff: Integer;       { Seconds east of UTC.  }
    __tm_zone: ^Char;           { Timezone abbreviation.}
  end;

function localtime(t: PCardinal): pointer; cdecl; { always cdecl }
// a fast full pascal version of the standard C library function
var uTm: TFileTime;
    lTm: TFileTime;
    S: TSystemTime;
begin
  Int64(uTm) := (Int64(t^) + 11644473600)*10000000; // unix time to dos file time
  FileTimeToLocalFileTime(uTM,lTM);
  FileTimeToSystemTime(lTM,S);
  with atm do begin
    tm_sec := S.wSecond;
    tm_min := S.wMinute;
    tm_hour := S.wHour;
    tm_mday := S.wDay;
    tm_mon := S.wMonth-1;
    tm_year := S.wYear-1900;
    tm_wday := S.wDayOfWeek;
  end;
  result := @atm;
end;

var
  /// local encryption XOR table (size SQLEncryptTableSize=16KB) created
  // by the CreateSQLEncryptTableBytes() procedure below
  CreateSQLEncryptTablePtr: RawByteString;

procedure CreateSQLEncryptTableBytes(const PassWord: RawUTF8; Table: PByteArray);
// very fast table (private key) computation from a given password
// - use a simple prime-based random generator, strong enough for common use
// - execution speed and code size was the goal here
// - use our SynCrypto for SHA-256 and AES-256 for most secure encryption 
var i, j, k, L: integer;
begin
  L := length(Password)-1;
  j := 0;
  k := integer(L*ord(Password[1]))+134775813; // initial value, prime number derivated
  for i := 0 to SQLEncryptTableSize-1 do begin
    Table^[i] := (ord(PassWord[j+1])) xor byte(k);
    k := Integer(k*3+i); // fast prime-based pseudo random generator
    if j=L then
      j := 0 else
      inc(j);
  end;
end;

procedure CreateSQLEncryptTable(const PassWord: RawUTF8);
// you can use this simple (and strong enough) procedure for easy SQL encryption
begin
  if Password='' then
    SQLEncryptTable := nil else begin
    if CreateSQLEncryptTablePtr='' then
      SetLength(CreateSQLEncryptTablePtr,SQLEncryptTableSize);
    CreateSQLEncryptTableBytes(PassWord,pointer(CreateSQLEncryptTablePtr));
    SQLEncryptTable := pointer(CreateSQLEncryptTablePtr);
  end;
end;

procedure XorOffset(p: pByte; Index, Count: cardinal; SQLEncryptTable: PByteArray);
// XorOffset: fast and simple Cypher using Index (= offset in file):
procedure Xor64(PI, P: PPtrIntArray; Count: cardinal); // fast xor
{$ifdef PUREPASCAL}
var i: cardinal;
begin
  for i := 0 to (Count div sizeof(PtrInt))-1 do
    P^[i] := P^[i] xor PI^[i]; // this will compile fine for 64 bit CPU
end;
{$else}
asm // eax=PI edx=P ecx=bytes count
  push ebx
  push esi
  shr ecx,3 // 64 bits = 8 bytes per loop
  jz @z
@1:
  mov ebx,[eax]    // fast CPU-pipelined optimized loop
  mov esi,[eax+4]
  xor [edx],ebx
  xor [edx+4],esi
  dec ecx
  lea eax,[eax+8]
  lea edx,[edx+8]
  jnz @1
@z:
  pop esi
  pop ebx
end;
{$endif}
var i, Len, L: integer;
begin
  if Count>0 then
  repeat
    Index := Index and (SQLEncryptTableSize-1);
    Len := SQLEncryptTableSize-Index;
    if cardinal(Len)>cardinal(Count) then
      Len := Count;
    Xor64(@SQLEncryptTable^[Index],pointer(p),Len); // xor 8 bytes per loop
    L := Len and (-8); // -8=$FFFFFFF8
    inc(p,L);
    inc(Index,L);
    for i := 1 to (Len and 7) do begin // xor 0..7 remaining bytes
      p^ := p^ xor SQLEncryptTable^[Index];
      inc(p); inc(Index);
    end;
    Dec(Count,Len);
  until Count=0;
end;

procedure ChangeSQLEncryptTablePassWord(const FileName: TFileName;
  const OldPassWord, NewPassword: RawUTF8);
var F: THandle;
    R: integer;
    Buf: array[word] of byte; // temp buffer for read/write (64KB is enough)
    Size, Posi: Int64Rec;
    OldP, NewP: array[0..SQLEncryptTableSize-1] of byte; // 2x16KB tables
begin
  if OldPassword=NewPassword then
    exit;
  F := FileOpen(FileName,fmOpenReadWrite);
  if F=INVALID_HANDLE_VALUE then
    exit;
  Size.Lo := GetFileSize(F,@Size.Hi);
  if (Size.Lo<=1024) and (Size.Hi=0) then begin
    FileClose(F); // file is to small to be modified
    exit;
  end;
  if OldPassword<>'' then
    CreateSQLEncryptTableBytes(OldPassWord,@OldP);
  if NewPassword<>'' then
    CreateSQLEncryptTableBytes(NewPassWord,@NewP);
  Int64(Posi) := 1024; // don't change first page, which is uncrypted
  SetFilePointer(F,1024,nil,FILE_BEGIN); // move to first page after 1024
  while Int64(Posi)<Int64(Size) do begin
    R := FileRead(F,Buf,sizeof(Buf)); // read buffer
    if R<0 then
      break; // stop on any read error
    if OldPassword<>'' then
      XorOffset(@Buf,Posi.Lo,R,@OldP); // uncrypt with old key
    if NewPassword<>'' then
      XorOffset(@Buf,Posi.Lo,R,@NewP); // crypt with new key
    SetFilePointer(F,Posi.Lo,@Posi.Hi,FILE_BEGIN); // rewind
    FileWrite(F,Buf,R); // update buffer
    inc(Int64(Posi),cardinal(R));
  end;
  FileClose(F);
end;

// we override default WinRead() and WinWrite() functions below, in order
// to add our proprietary (but efficient) encryption engine
// - should be modified to match other Operating Systems than Windows

type
{$ifndef DELPHI5OROLDER} // Delphi 5 is already aligning records by 4 bytes
{$A4} // bcc32 default alignment is 4 bytes
{$endif}
  TSQLFile = record // called winFile (expand sqlite3_file) in sqlite3.c
    pMethods: pointer;     // sqlite3_io_methods_ptr
    pVfs: pointer;         // The VFS used to open this file (new in version 3.7)
    h: THandle;            // Handle for accessing the file
    bulk: cardinal;        // lockType+sharedLockByte are word-aligned
    lastErrno: cardinal;   // The Windows errno from the last I/O error
    // asm code generated from c is [esi+16] for lastErrNo -> OK
  end;
  // those structures are used to retrieve the Windows file handle
  TSQLPager = record
    pVfs: pointer;
    exclusiveMode, journalMode, useJournal, noSync, fullSync,
    ckptSyncFlags, syncFlags, tempFile, readOnly, memDb: byte;
    eState, eLock, changeCountDone, setMaster, doNotSpill, doNotSyncSpill,
    subjInMemory: Byte;
    dbSize, dbOrigSize, dbFileSize, dbHintSize, errCode, nRec, cksumInit,
    nSubRec: cardinal;
    pInJournal: pointer;
    fd: ^TSQLFile; // File descriptor for database
    jfd: ^TSQLFile; // File descriptor for main journal
    sjfd: ^TSQLFile; // File descriptor for sub-journal
  end;
  TSQLBtShared = record
    pPager: ^TSQLPager;
  end;
  TSQLBTree = record
    db: TSQLite3DB;
    pBt: ^TSQLBtShared;
  end;
  PSQLBTree = ^TSQLBTree;
  TSQLDBOneStruct = record
    zName: PAnsiChar;
    Btree: PSQLBTree;
  end;
  // will map TSQLite3DB
  PSQLDBStruct = ^TSQLDBStruct;
  TSQLDBStruct = record
    pVfs, pVdbe, pDfltColl, mutex: pointer;
    DB0: ^TSQLDBOneStruct;
    nDb: integer;  // Number of backends currently in use
  end;
{$A+}
  // used to store all currently per-database encryption tables
  TSQLCypher = record
    Handle: THandle;
    CypherBuf: RawByteString;
  end;
  TSQLCypherDynArray = array of TSQLCypher;

var
  Cyphers: TSQLCypherDynArray;
  CypherCount: integer;
  Cypher: TDynArray;

function WinWrite(var F: TSQLFile; buf: PByte; buflen: integer; off: Int64): integer; {$ifndef USEFASTCALL}cdecl;{$endif}
// Write data from a buffer into a file.  Return SQLITE_OK on success
// or some other error code on failure
var n, i: integer;
    b: PByte;
    offset: Int64Rec;
    aSQLEncryptTable: pointer;
label err;
begin
  //SynSQLite3Log.Add.Log(sllCustom2,'WinWrite % off=% len=%',[F.h,off,buflen]);
  offset.Lo := Int64Rec(off).Lo;
  offset.Hi := Int64Rec(off).Hi and $7fffffff; // offset must be positive (u64)
  result := SetFilePointer(F.h,offset.Lo,@offset.Hi,FILE_BEGIN);
  if result=-1 then begin
    result := GetLastError;
    if result<>NO_ERROR then begin
      F.lastErrno := result;
      result := SQLITE_FULL;
      exit;
    end;
  end;
  aSQLEncryptTable := SQLEncryptTable;
  if Cyphers<>nil then
  for i := 0 to CypherCount-1 do // (a bit) faster than Cypher.Find(F.h)
    if Cyphers[i].Handle=F.h then begin
      aSQLEncryptTable := Pointer(Cyphers[i].CypherBuf);
      break;
    end;
  if (aSQLEncryptTable<>nil) and ((offset.Lo>=1024) or (offset.Hi<>0)) then begin
    b := buf;
    XorOffset(b,offset.Lo,buflen,aSQLEncryptTable); // crypt buf content after first page
  end else
    b := nil; // mark no encryption
  n := buflen;
  while n>0 do begin
    if not WriteFile(F.h,buf^,n,cardinal(result),nil) then begin
err:  F.lastErrno := GetLastError;
      result := SQLITE_FULL;
      if b<>nil then // restore buf content
        XorOffset(b,offset.Lo,buflen,aSQLEncryptTable);
      exit;
    end;
    if result=0 then break;
    dec(n,result);
    inc(buf,result);
  end;
  if n>result then
    goto err;
  result := SQLITE_OK;
  if b<>nil then // restore buf content
    XorOffset(b,offset.Lo,buflen,aSQLEncryptTable);
end;

const
  SQLITE_IOERR_READ       = $010A;
  SQLITE_IOERR_SHORT_READ = $020A;

function SetFilePointerEx(hFile: THandle; liDistanceToMove: TLargeInteger;
  const lpNewFilePointer: PLargeInteger; dwMoveMethod: DWORD): BOOL; stdcall;
  external kernel32 name 'SetFilePointerEx'; // not defined with older Delphi
  
function WinRead(var F: TSQLFile; buf: PByte; buflen: integer; off: Int64): integer; {$ifndef USEFASTCALL}cdecl;{$endif}
// Read data from a file into a buffer.  Return SQLITE_OK on success
// or some other error code on failure
var offset: Int64Rec;
    aSQLEncryptTable: PByteArray;
    i: integer;
begin
  //SynSQLite3Log.Add.Log(sllCustom2,'WinRead % off=% len=%',[F.h,off,buflen]);
  offset.Lo := Int64Rec(off).Lo;
  offset.Hi := Int64Rec(off).Hi and $7fffffff; // offset must be positive (u64)
  result := SetFilePointer(F.h,offset.Lo,@offset.Hi,FILE_BEGIN);
  if result=-1 then begin
    result := GetLastError;
    if result<>NO_ERROR then begin
      F.lastErrno := result;
      result := SQLITE_FULL;
      exit;
    end;
  end;
  if not ReadFile(F.h,buf^,buflen,cardinal(result),nil) then begin
    F.lastErrno := GetLastError;
    result := SQLITE_IOERR_READ;
    exit;
  end;
  aSQLEncryptTable := SQLEncryptTable;
  if Cyphers<>nil then
  for i := 0 to CypherCount-1 do // (a bit) faster than Cypher.Find(F.h)
    if Cyphers[i].Handle=F.h then begin
      aSQLEncryptTable := Pointer(Cyphers[i].CypherBuf);
      break;
    end;
  if (aSQLEncryptTable<>nil) and ((offset.Lo>=1024) or (offset.Hi<>0)) then
    XorOffset(buf,offset.Lo,result,aSQLEncryptTable); // uncrypt after first page
  dec(buflen,result);
  if buflen>0 then begin // remaining bytes are set to 0
    inc(buf,result);
    fillchar(buf^,buflen,0);
    result := SQLITE_IOERR_SHORT_READ;
  end else
    result := SQLITE_OK;
end;

function IsSQLite3File(const FileName: TFileName): boolean;
var F: THandle;
    Header: array[0..15] of AnsiChar;
begin
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    result := false else begin
    result := (FileRead(F,Header,sizeof(Header))=SizeOf(Header)) and
      (Header='SQLite format 3');
    FileClose(F);
  end;
end;

function IsSQLite3FileEncrypted(const FileName: TFileName): boolean;
var F: THandle;
    Header: array[0..2047] of AnsiChar;
begin
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    result := false else begin
    result := (FileRead(F,Header,sizeof(Header))=SizeOf(Header)) and
      (Header='SQLite format 3') and not(Header[1024] in [#5,#10,#13]);
    // B-tree leaf Type to be either 5 (interior) 10 (index) or 13 (table)
    FileClose(F);
  end;
end;


// ************ objects to access SQLite3 database engine

resourcestring
  sErrorSQLite3NoDB = 'Invalid SQlite3 database handle (%d)';


{ TSQLDataBase }

function TSQLDataBase.Blob(const DBName, TableName, ColumnName: RawUTF8;
  RowID: Int64; ReadWrite: boolean): TSQLBlobStream;
begin
  if self=nil then begin
    result := nil;
    exit; // avoid GPF in case of call from a static-only server
  end;
  if RowID<0 then
    RowID := LastInsertRowID;
  Lock('');
  try
    result := TSQLBlobStream.Create(DB,DBName,TableName,ColumnName,RowID,ReadWrite);
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.Rollback;
begin
  if (self=nil) or not fTransactionActive then
    exit;
  Execute('ROLLBACK TRANSACTION;');
  fTransactionActive := false;
end;

procedure TSQLDataBase.TransactionBegin(aBehavior: TSQLDataBaseTransactionBehaviour = tbDeferred);
const
  TBTOKENS: array[TSQLDataBaseTransactionBehaviour] of RawUTF8 = (
    '', 'IMMEDIATE ', 'EXCLUSIVE '); // see http://www.sqlite.org/lang_transaction.html
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  if fTransactionActive then begin
    Execute('ROLLBACK TRANSACTION;');
    fTransactionActive := false;
  end;
  Execute('BEGIN '+TBTOKENS[aBehavior]+'TRANSACTION;');
  fTransactionActive := true;
end;

procedure TSQLDataBase.Commit;
begin
  if (Self<>nil) and fTransactionActive then begin
    Execute('COMMIT TRANSACTION;');
    fTransactionActive := false;
  end;
end;

{ from WladiD about all collation functions:
  If a field with your custom collate ISO8601 is empty '' (not NULL),
  then SQLite calls the registered collate function with s1len=0 or s2len=0,
  but the pointers s1 or s2 map to the string of the previous call }

function Utf16SQLCompCase(CollateParam: pointer; s1Len: integer; S1: pointer;
    s2Len: integer; S2: pointer) : integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if s1Len=0 then // see WladiD note above
    s1 := nil;
  if s2Len=0 then
    s2 := nil;
  result := CompareStringW(GetThreadLocale, 0, S1, S1len, S2, S2Len) - 2;
end;

function Utf16SQLCompNoCase(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if s1Len=0 then // see WladiD note above
    s1 := nil;
  if s2Len=0 then
    s2 := nil;
  result := CompareStringW(GetThreadLocale, NORM_IGNORECASE, S1, S1len, S2, S2Len) - 2;
end;

function Utf8SQLCompNoCase(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if (s1Len=0) and (s2Len=0) then // see WladiD note above
    result := 0 else
    result := UTF8ILComp(s1,s2,s1Len,s2Len); // properly handles individual s?Len=0
end;

function Utf8SQLDateTime(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; {$ifndef USEFASTCALL}cdecl;{$endif}
var V1,V2: Int64; // faster than Iso8601ToDateTimePChar: uses integer math
begin
  if s1Len=0 then // see WladiD note above
    s1 := nil;
  if s2Len=0 then
    s2 := nil;
  if s1=s2 then begin
    result := 0;
    exit;
  end;
  V1 := Iso8601ToSecondsPUTF8Char(s1,s1Len);
  V2 := Iso8601ToSecondsPUTF8Char(s2,s2Len);
  if (V1=0) or (V2=0) then // any invalid date -> compare as UTF-8 strings
    result := UTF8ILComp(s1,s2,s1Len,s2Len) else
    if V1<V2 then
      result := -1 else
      if V1=V2 then
        result := 0 else
        result := +1;
end;

procedure ErrorWrongNumberOfArgs(Context: TSQLite3FunctionContext);
begin
  sqlite3_result_error(Context, 'wrong number of arguments');
end;

procedure InternalSoundex(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if argc=1 then
    sqlite3_result_int64(Context, SoundExUTF8(sqlite3_value_text(argv[0]))) else
    ErrorWrongNumberOfArgs(Context);
end;

procedure InternalSoundexFr(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if argc=1 then
    sqlite3_result_int64(Context, SoundExUTF8(sqlite3_value_text(argv[0]),nil,sndxFrench)) else
    ErrorWrongNumberOfArgs(Context);
end;

procedure InternalSoundexEs(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if argc=1 then
    sqlite3_result_int64(Context, SoundExUTF8(sqlite3_value_text(argv[0]),nil,sndxSpanish)) else
    ErrorWrongNumberOfArgs(Context);
end;

procedure InternalMod(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
var A1, A2: Int64;
begin // implements the MOD() function, just like Oracle and others
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  A1 := sqlite3_value_int64(argv[0]);
  A2 := sqlite3_value_int64(argv[1]);
  if A2=0 then // avoid computation exception, returns NULL
    sqlite3_result_null(Context) else
    sqlite3_result_int64(Context, A1 mod A2);
end;

procedure InternalRank(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
// supplies the same "RANK" internal function as proposed in
// http://www.sqlite.org/fts3.html#appendix_a
var MI: PFTSMatchInfo;
    p,c: integer;
    score: Double;
begin
  if argc>=1 then begin
    MI := sqlite3_value_blob(argv[0]);
    if argc=MI^.nCol+1 then begin
      score := 0;
      for p := 1 to MI^.nPhrase do
        for c := 1 to MI^.nCol do
        with MI^.hits[c] do 
          if this_row>0 then
            score := score+(this_row/all_rows)*sqlite3_value_double(argv[c]);
      sqlite3_result_double(Context,score);
      exit; // success: don't call sqlite3_result_error()
    end;
  end;
  ErrorWrongNumberOfArgs(Context);
end;

procedure sqlite3InternalFree(p: pointer); {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  Freemem(p);
end;

procedure sqlite3InternalFreeObject(p: pointer); {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  TObject(p).Free;
end;

// supplies a CONCAT() function to process fast string concatenation
type
  PConcatRec = ^TConcatRec;
  TConcatRec = record
    result: PUTF8Char;
    resultlen: PtrInt;
  end;

procedure InternalConcatStep(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
var sep, txt: PUTF8Char;
    seplen, txtlen: PtrInt;
begin
  if argc=2 then
    with PConcatRec(sqlite3_aggregate_context(Context,sizeof(TConcatRec)))^ do begin
      // +1 below for adding a final #0
      txt := sqlite3_value_text(argv[0]);
      txtlen := SynCommons.strlen(txt);
      if result=nil then
        GetMem(result,txtlen+1)
      else begin
        sep := sqlite3_value_text(argv[1]);
        seplen := SynCommons.strlen(sep);
        ReallocMem(result,resultlen+txtlen+seplen+1);
        Move(sep^,result[resultlen],seplen);
        inc(resultlen,seplen);
      end;
      Move(txt^,result[resultlen],txtlen+1);
      inc(resultlen,txtlen);
    end else
    ErrorWrongNumberOfArgs(Context);
end;

procedure InternalConcatFinal(Context: TSQLite3FunctionContext); {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  with PConcatRec(sqlite3_aggregate_context(Context,sizeof(TConcatRec)))^ do
    // sqlite3InternalFree will call Freemem(PConcatRec()^.result)
    sqlite3_result_text(Context,result,resultlen+1,sqlite3InternalFree);
end;

procedure InternalIntegerDynArray(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
var Blob: pointer;
    PI: PIntegerArray;
    Count: integer;
begin // SQL function: IntegerDynArrayContains(BlobField,10) returning a boolean
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  Blob := sqlite3_value_blob(argv[0]);
  if Blob<>nil then begin
    PI := IntegerDynArrayLoadFrom(Blob,Count); // fast map into in-memory array
    if not IntegerScanExists(pointer(PI),Count,sqlite3_value_int64(argv[1])) then
      Blob := nil;
  end;
  sqlite3_result_int64(Context,Int64(Blob<>nil));
end;

procedure InternalSimpleInt64DynArray(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
var Blob: pointer;
    Count, ElemSize, i: integer;
    V: Int64;
begin // Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
  // for currency, expect I64 value = aCurrency*10000 = PInt64(@aCurrency)^
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  Blob := sqlite3_value_blob(argv[0]);
  if Blob<>nil then begin // search into direct in-memory mapping (no allocation)
    Blob := SimpleDynArrayLoadFrom(Blob,sqlite3_user_data(Context),Count,ElemSize);
    if Blob<>nil then begin
      V := sqlite3_value_int64(argv[1]);
      sqlite3_result_int64(Context,Int64(true));
      case ElemSize of
        1: for i := 0 to Count-1 do if PByteArray(Blob)^[i]=byte(V) then exit;
        2: for i := 0 to Count-1 do if PWordArray(Blob)^[i]=word(V) then exit;
        4: if IntegerScanExists(Blob,Count,cardinal(V)) then exit;
        8: for i := 0 to Count-1 do if PInt64Array(Blob)^[i]=V then exit;
      end;
    end;
  end;
  sqlite3_result_int64(Context,Int64(false));
end;

procedure InternalRawUTF8DynArray(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
var Blob: pointer;
    Value: PUTF8Char;
begin // SQL function: RawUTF8DynArrayContainsCase/NoCase(BlobField,'Text') returning a boolean
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  Blob := sqlite3_value_blob(argv[0]);
  if Blob<>nil then begin
    Value := sqlite3_value_text(argv[1]);
    if RawUTF8DynArrayLoadFromContains(Blob,Value,SynCommons.StrLen(Value),
       sqlite3_user_data(Context)=nil)<0 then
      Blob := nil;
  end;
  sqlite3_result_int64(Context,Int64(Blob<>nil));
end;

constructor TSQLDataBase.Create(const aFileName: TFileName; const aPassword: RawUTF8='');
var result: integer;
begin
  {$ifdef WITHLOG}
  fLog := SynSQLite3Log.Family.SynLog; // leave fLog=nil if no Logging wanted
  {$endif}
  InitializeCriticalSection(fLock);
  fFileName := aFileName;
  if (aPassword<>'') and (aFileName<>':memory:') and (aFileName<>'') then begin
    SetLength(fCypherBlock,SQLEncryptTableSize);
    CreateSQLEncryptTableBytes(aPassword,pointer(fCypherBlock));
  end;
  fSQLFunctions := TObjectList.Create;
  result := DBOpen;
  if result<>SQLITE_OK then
    raise ESQLite3Exception.Create(fDB,result);
end;

destructor TSQLDataBase.Destroy;
{$ifndef INCLUDE_FTS3}
var S: TSQLite3Statement;
{$endif}
begin
  if DB<>0 then
  try
    Rollback; // any unfinished transaction is rollbacked
  finally
    {$ifndef INCLUDE_FTS3}
    { Applications should finalize all prepared statements and close all BLOB handles
      associated with the sqlite3 object prior to attempting to close the object }
    repeat
      S := sqlite3_next_stmt(DB,0); // 0: get first prepared statement for DB
      if S=0 then
        break;
      // if code was correctly protected with try/finally, as in
      // TSQLDataBase.Execute() and TSQLRequest.Execute(), we should never go here
      // -> BUT it seems that the FTS3 leaves some statements open at closing
      // assert(false,FileName); // debug purpose, but not FTS3 ready
    until not (sqlite3_finalize(S) in [SQLITE_OK,SQLITE_ABORT]);
    { BUT the problem is that if you use FTS3, the statements will be released
      twice (i.e. one time above and next time in sqlite3_close below),
      so some GPF will occur :(
    -> we don't release any statement in case of FTS3 usage, and rely on our
      framework, which protects all SQL statements with try..finally clauses }
    {$endif INCLUDE_FTS3}
    DBClose;
  end;
  DeleteCriticalSection(fLock);
  fCache.Free;
  fSQLFunctions.Free;
  inherited;
end;

procedure TSQLDataBase.ExecuteAll(const aSQL: RawUTF8);
var R: TSQLRequest;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
{$ifdef WITHLOG}
  Log := SynSQLite3Log.Enter(self);
  Log.Log(sllSQL,aSQL);
{$endif}
  Lock('ALTER'); // don't trust aSQL -> assume modify -> inc(InternalState^)
  try
    R.ExecuteAll(DB,aSQl);
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.Execute(const aSQL: RawUTF8);
var R: TSQLRequest;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
{$ifdef WITHLOG}
  Log := SynSQLite3Log.Enter(self);
  Log.Log(sllSQL,aSQL);
{$endif}
  Lock(aSQL); // run one statement -> we can trust isSelect()
  try
    R.Execute(DB,aSQL);
  finally
    UnLock;
  end;
end;

function TSQLDataBase.Execute(const aSQL: RawUTF8; var ID: TInt64DynArray): integer;
var R: TSQLRequest;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
  if self=nil then begin
    result := 0;
    exit; // avoid GPF in case of call from a static-only server
  end;
{$ifdef WITHLOG}
  Log := SynSQLite3Log.Enter(self);
  Log.Log(sllSQL,aSQL);
{$endif}
  Lock(aSQL);
  try
    result := R.Execute(DB,aSQL,ID);
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.Execute(const aSQL: RawUTF8; out ID: Int64);
var R: TSQLRequest;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
{$ifdef WITHLOG}
  Log := SynSQLite3Log.Enter(self);
  Log.Log(sllSQL,aSQL);
{$endif}
  Lock(aSQL);
  try
    R.Execute(DB,aSQL,ID);
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.Execute(const aSQL: RawUTF8; out ID: RawUTF8);
var R: TSQLRequest;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
{$ifdef WITHLOG}
  Log := SynSQLite3Log.Enter(self);
  Log.Log(sllSQL,aSQL);
{$endif}
  Lock(aSQL);
  try
    R.Execute(DB,aSQL,ID);
  finally
    UnLock;
  end;
end;

function TSQLDataBase.ExecuteNoException(const aSQL: RawUTF8): boolean;
begin
  try
    Execute(aSQL);
    result := true;
  except
    result := false;
  end;
end;

procedure TSQLDataBase.ExecuteNoException(const aSQL: RawUTF8; out ID: Int64);
begin
  try
    Execute(aSQL,ID);
  except
    ID := 0;
  end;
end;

procedure TSQLDataBase.ExecuteNoException(const aSQL: RawUTF8; out ID: RawUTF8);
begin
  try
    Execute(aSQL,ID);
  except
    ID := '';
  end;
end;

function TSQLDataBase.ExecuteJSON(const aSQL: RawUTF8; Expand: boolean=false;
  aResultCount: PPtrInt=nil): RawUTF8;
var R: TSQLRequest;
    Count: PtrInt;
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
{$ifdef WITHLOG}
  SynSQLite3Log.Enter(self);
{$endif}
  result := LockJSON(aSQL,aResultCount); // lock and try getting the request from the cache
  if result='' then // only Execute the DB request if not got from cache
  try
    result := R.ExecuteJSON(DB,aSQL,Expand,@Count);
    if aResultCount<>nil then
      aResultCount^ := Count;
  finally
    UnLockJSON(result,Count);
  end;
end;

function TSQLDataBase.Execute(const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer;
var R: TSQLRequest;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
  if self=nil then begin
    result := 0;
    exit; // avoid GPF in case of call from a static-only server
  end;
{$ifdef WITHLOG}
  Log := SynSQLite3Log.Enter(self);
  Log.Log(sllSQL,aSQL);
{$endif}
  Lock(aSQL);
  try
    result := R.Execute(DB,aSQL,Values);
  finally
    UnLock;
  end;
end;

function TSQLDataBase.LastInsertRowID: Int64;
begin
  if (self=nil) or (DB=0) then
    result := 0 else
    try
      Lock('');
      result := sqlite3_last_insert_rowid(DB);
      {$ifdef WITHLOG}
      {$ifdef DELPHI5OROLDER}
      fLog.Log(sllDB,'LastInsertRowID='+Int64ToUTF8(result),self);
      {$else}
      fLog.Log(sllDB,'LastInsertRowID=%',result,self);
      {$endif}
      {$endif}
    finally
      UnLock;
    end;
end;

function TSQLDataBase.LastChangeCount: integer;
begin
  if (self=nil) or (DB=0) then
    result := 0 else
    try
      Lock('');
      result := sqlite3_changes(DB);
      {$ifdef WITHLOG}
      {$ifdef DELPHI5OROLDER}
      fLog.Log(sllDB,'LastChangeCount='+Int64ToUTF8(result),self);
      {$else}
      fLog.Log(sllDB,'LastChangeCount=%',result,self);
      {$endif}
      {$endif}
    finally
      UnLock;
    end;
end;

procedure TSQLDataBase.GetTableNames(var Names: TRawUTF8DynArray);
begin // SQL statement taken from official SQLite3 FAQ
  SetLength(Names,Execute(SQL_GET_TABLE_NAMES,Names));
  {$ifdef WITHLOG}
  {$ifndef DELPHI5OROLDER}
  fLog.Log(sllDebug,'TableNames',TypeInfo(TRawUTF8DynArray),Names,self);
  {$endif}
  {$endif}
end;

procedure TSQLDataBase.GetFieldNames(var Names: TRawUTF8DynArray; const TableName: RawUTF8);
var R: TSQLRequest;
    n: integer;
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  Lock('');
  try
    try
      R.Prepare(fDB,'PRAGMA table_info('+TableName+');'); // ESQLite3Exception
      n := 0;
      repeat
        if R.Step<>SQLITE_ROW then break;
        if n=length(Names) then
          SetLength(Names,n+MAX_SQLFIELDS);
        Names[n] := sqlite3_column_text(R.Request,1); // cid,name,type,notnull,dflt_value,pk
        inc(n);
      until false;
      SetLength(Names,n);
    finally
      R.Close;
    end;
  finally
    UnLock;
  end;
end;

function TSQLDataBase.GetUseCache: boolean;
begin
  result := (Self<>nil) and (fCache<>nil);
end;

procedure TSQLDataBase.SetUseCache(const Value: boolean);
begin
  if self<>nil then
    if Value<>UseCache then
      if Value then
        fCache := TSynCache.Create else
        FreeAndNil(fCache);
end;

procedure TSQLDataBase.Lock(const aSQL: RawUTF8);
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  EnterCriticalSection(fLock); // on non-concurent calls, this API is very fast
  if not isSelect(pointer(aSQL)) then
    // UPDATE, INSERT or any non SELECT
    CacheFlush;
end;

procedure TSQLDataBase.UnLock;
begin
  if self<>nil then
    LeaveCriticalSection(fLock); // on non-concurent calls, this API is very fast
end;

function TSQLDataBase.LockJSON(const aSQL: RawUTF8; aResultCount: PPtrInt): RawUTF8;
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  EnterCriticalSection(fLock); // cache access is also protected by fLock
  if isSelect(pointer(aSQL)) then begin
    result := fCache.Find(aSQL,aResultCount); // try to get JSON result from cache
    if result<>'' then begin
      {$ifdef WITHLOG}
      fLog.Log(sllSQL,aSQL,self);
      fLog.Log(sllCache,'from cache',self);
      fLog.Log(sllResult,result,self);
      {$endif}
      LeaveCriticalSection(fLock); // found in cache -> leave critical section
    end
    {$ifdef WITHLOG}
    else
      fLog.Log(sllCache,'not in cache',self);
    {$endif}
  end else begin
    // UPDATE, INSERT or any non SELECT statement
    CacheFlush;
    result := '';
  end;
end;

procedure TSQLDataBase.UnLockJSON(const aJSONResult: RawUTF8; aResultCount: PtrInt);
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  {$ifdef WITHLOG}
  fLog.Log(sllResult,aJSONResult,self);
  {$endif}
  fCache.Add(aJSONResult,aResultCount); // if a reset was made just before, Add() does nothing
  UnLock; // leave fLock
end;

function TSQLDataBase.Backup(const BackupFileName: TFileName): boolean;
begin
  fLog.Enter(self);
  if self=nil then begin
    result := false;
    exit; // avoid GPF in case of call from a static-only server
  end;
  Rollback; // any unfinished transaction is rollbacked
  Execute('VACUUM;');
  Lock(''); // lock read-only the data base
  try
    try
      fLog.Log(sllTrace,'close',self);
      DBClose;
      fLog.Log(sllTrace,'copy file',self);
      result := CopyFile(pointer(fFileName),pointer(BackupFileName),false);
    finally
      fLog.Log(sllTrace,'reopen',self);
      DBOpen;
    end;
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.DBClose;
var i: integer;
begin
  if (self=nil) or (fDB=0) then
    exit;
  fLog.Enter;
  if Cyphers<>nil then
    i := Cypher.Find(PSQLDBStruct(fDB)^.DB0^.Btree^.pBt^.pPager^.fd^.h) else
    i := -1;
  sqlite3_close(fDB);
  if i>=0 then begin
    Cypher.Delete(i); // do it after file closing
    {$ifdef WITHLOG}
    fLog.Log(sllDB,'end of encryption');
    {$endif}
  end;
  fDB := 0;
end;

function TSQLDataBase.DBOpen: integer;
var utf8: RawUTF8;
    i: integer;
    Cyph: TSQLCypher;
begin
  fLog.Enter;
  utf8 := StringToUTF8(fFileName);
  result := sqlite3_open(pointer(utf8),fDB);
  if result<>SQLITE_OK then begin
    {$ifdef WITHLOG}
    fLog.Log(sllError,'open("'+utf8+'") failed',self);
    {$endif}
    sqlite3_close(fDB); // should always be closed, even on failure
    fDB := 0;
    exit;
  end;
  Cyph.Handle := PSQLDBStruct(fDB)^.DB0^.Btree^.pBt^.pPager^.fd^.h;
  //fLog.Log(sllDB,'open("%") with handle=%',[utf8,Cyph.Handle],self);
  if fCypherBlock<>'' then begin
    if Cyphers=nil then begin
      Cypher.Init(TypeInfo(TSQLCypherDynArray),Cyphers,@CypherCount);
      Cypher.Compare := SortDynArrayInteger;
    end;
    i := Cypher.Find(Cyph.Handle);
    if i>=0 then begin
      {$ifdef WITHLOG}
      fLog.Log(sllError,'Handle reused for '+utf8);
      {$endif}
    end else begin
      Cyph.CypherBuf := fCypherBlock;
      Cypher.Add(Cyph);
      {$ifdef WITHLOG}
      fLog.Log(sllDB,'encryption enabled');
      {$endif}
    end;
  end;
  // the SQLite3 standard NOCASE collation is used for AnsiString and is very fast
  // our custom fast UTF-8 case insensitive compare, using NormToUpper[] for all 8 bits values
  sqlite3_create_collation(DB,'SYSTEMNOCASE',SQLITE_UTF8,nil,Utf8SQLCompNoCase);
  // our custom fast ISO-8601 date time encoded
  sqlite3_create_collation(DB,'ISO8601',SQLITE_UTF8,nil,Utf8SQLDateTime);
  // two slow but alwyas accurate compare, using the Win32 Unicode API
  sqlite3_create_collation(DB,'WIN32CASE',SQLITE_UTF16,nil,Utf16SQLCompCase);
  sqlite3_create_collation(DB,'WIN32NOCASE',SQLITE_UTF16,nil,Utf16SQLCompNoCase);
  // register the MOD() user function, similar to the standard % operator
  sqlite3_create_function_v2(DB,'MOD',2,SQLITE_ANY,nil,InternalMod,nil,nil,nil);
  // some user functions
  sqlite3_create_function_v2(DB,'SOUNDEX',1,SQLITE_UTF8,nil,InternalSoundex,nil,nil,nil);
  sqlite3_create_function_v2(DB,'SOUNDEXFR',1,SQLITE_UTF8,nil,InternalSoundexFr,nil,nil,nil);
  sqlite3_create_function_v2(DB,'SOUNDEXES',1,SQLITE_UTF8,nil,InternalSoundexEs,nil,nil,nil);
  // rank() function as proposed in http://www.sqlite.org/fts3.html#appendix_a
  sqlite3_create_function_v2(DB,'RANK',-1,SQLITE_ANY,nil,InternalRank,nil,nil,nil);
  // CONCAT() function to process fast string concatenation
  sqlite3_create_function_v2(DB,'CONCAT',2,SQLITE_UTF8,nil,nil,
    InternalConcatStep,InternalConcatFinal,nil);
  // functions to handle some standard dynamic array BLOB content in SQL
  // IntegerDynArrayContains(BlobField,10) returning a boolean
  sqlite3_create_function_v2(DB,'INTEGERDYNARRAYCONTAINS',2,SQLITE_ANY,nil,
    InternalIntegerDynArray,nil,nil,nil);
  // Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
  sqlite3_create_function_v2(DB,'BYTEDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TByteDynArray),InternalSimpleInt64DynArray,nil,nil,nil);
  sqlite3_create_function_v2(DB,'WORDDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TWordDynArray),InternalSimpleInt64DynArray,nil,nil,nil);
  sqlite3_create_function_v2(DB,'CARDINALDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TCardinalDynArray),InternalSimpleInt64DynArray,nil,nil,nil);
  sqlite3_create_function_v2(DB,'INT64DYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TInt64DynArray),InternalSimpleInt64DynArray,nil,nil,nil);
  sqlite3_create_function_v2(DB,'CURRENCYDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TInt64DynArray),InternalSimpleInt64DynArray,nil,nil,nil);
  // RawUTF8DynArrayContainsCase/NoCase(BlobField,'Text') returning a boolean
  sqlite3_create_function_v2(DB,'RAWUTF8DYNARRAYCONTAINSCASE',2,SQLITE_ANY,nil,
    InternalRawUTF8DynArray,nil,nil,nil);
  sqlite3_create_function_v2(DB,'RAWUTF8DYNARRAYCONTAINSNOCASE',2,SQLITE_ANY,
    @UTF8ILComp,InternalRawUTF8DynArray,nil,nil,nil);
  // reallocate all TSQLDataBaseSQLFunction for re-Open (TSQLRestServerDB.Backup)
  for i := 0 to fSQLFunctions.Count-1 do
    TSQLDataBaseSQLFunction(fSQLFunctions.List[i]).CreateFunction(DB);
end;


function TSQLDataBase.GetUserVersion: cardinal;
var tmp: Int64;
begin
  ExecuteNoException('PRAGMA user_version',tmp);
  result := tmp;
end;

procedure TSQLDataBase.SetUserVersion(const Value: cardinal);
begin
  ExecuteNoException('PRAGMA user_version='+Int32ToUTF8(Value));
end;

procedure TSQLDataBase.SetSynchronous(const Value: TSQLSynchronousMode);
begin
  ExecuteNoException('PRAGMA synchronous='+Int32ToUTF8(ord(Value)));
end;

function TSQLDataBase.GetSynchronous: TSQLSynchronousMode;
var tmp: Int64;
begin
  ExecuteNoException('PRAGMA synchronous ',tmp);
  result := TSQLSynchronousMode(tmp);
end;

procedure TSQLDataBase.SetWALMode(Value: Boolean);
const CMD: array[boolean] of RawUTF8 = ('DELETE;','WAL;');
begin
  ExecuteNoException('PRAGMA journal_mode='+CMD[value]);
end;

function TSQLDataBase.GetWALMode: boolean;
var tmp: RawUTF8;
begin
  ExecuteNoException('PRAGMA journal_mode',tmp);
  result := IdemPropNameU(tmp,'wal');
end;

procedure TSQLDataBase.SetBusyTimeout(const ms: Integer);
begin
  if self=nil then
    exit;
  {$ifdef WITHLOG}
  {$ifdef DELPHI5OROLDER}
  fLog.Log(sllDB,'SetBusyTimeout='+Int32ToUTF8(ms),self);
  {$else}
  fLog.Log(sllDB,'SetBusyTimeout=%',ms,self);
  {$endif}
  {$endif}
  sqlite3_busy_timeout(DB,ms);
  fBusyTimeout := ms;
end;

procedure TSQLDataBase.CacheFlush;
begin
  if self=nil then
    exit;
  if InternalState<>nil then
    inc(InternalState^);
  if fCache.Reset then
  {$ifdef WITHLOG}
    fLog.Log(sllCache,'cache flushed',self);
  {$endif}
end;

procedure TSQLDataBase.RegisterSQLFunction(aFunction: TSQLDataBaseSQLFunction);
var i: integer;
begin
  if (self=nil) or (aFunction=nil) then
    exit;
  for i := 0 to fSQLFunctions.Count-1 do
    with TSQLDataBaseSQLFunction(fSQLFunctions.List[i]) do
    if (FunctionParametersCount=aFunction.FunctionParametersCount) and
       IdemPropNameU(FunctionName,aFunction.FunctionName) then begin
      aFunction.Free;
      exit; // already registered with the same name and parameters count
    end;
  {$ifdef WITHLOG}
  fLog.Log(sllDB,'RegisterSQLFunction '+aFunction.FunctionName,self);
  {$endif}
  fSQLFunctions.Add(aFunction);
  if DB<>0 then
    // DB already opened -> register this custom function
    aFunction.CreateFunction(DB);
end;

procedure TSQLDataBase.RegisterSQLFunction(aDynArrayTypeInfo: pointer;
  aCompare: TDynArraySortCompare; const aFunctionName: RawUTF8);
begin
  RegisterSQLFunction(
    TSQLDataBaseSQLFunctionDynArray.Create(aDynArrayTypeInfo,aCompare,aFunctionName));
end;

procedure TSQLDataBase.RegisterSQLFunction(aFunction: TSQLFunctionFunc;
  aFunctionParametersCount: Integer; const aFunctionName: RawUTF8);
begin
  RegisterSQLFunction(TSQLDataBaseSQLFunction.Create(aFunction,aFunctionParametersCount,aFunctionName));
end;


{ TSQLRequest }

procedure TSQLRequest.Bind(Param: Integer; Value: Int64);
begin
  sqlite3_check(RequestDB,sqlite3_bind_Int64(Request,Param,Value));
end;

procedure TSQLRequest.Bind(Param: Integer; Value: double);
begin
  sqlite3_check(RequestDB,sqlite3_bind_double(Request,Param,Value));
end;

procedure TSQLRequest.Bind(Param: Integer; const Value: RawUTF8);
begin
  // note that the official SQLite3 documentation could lead into misunderstanding:
  // Text_bytes must EXCLUDE the null terminator, otherwise a #0 is appended to
  // all column values -> so length(Value) is needed here
  sqlite3_check(RequestDB,sqlite3_bind_text(Request,Param,pointer(Value),
    length(Value),SQLITE_TRANSIENT)); // make private copy of the data
end;

procedure TSQLRequest.Bind(Param: Integer; Data: pointer; Size: integer);
begin
  sqlite3_check(RequestDB,sqlite3_bind_blob(Request,Param,Data,Size,
    SQLITE_TRANSIENT)); // make private copy of the data
end;

procedure TSQLRequest.Bind(Param: Integer; Data: TCustomMemoryStream);
begin
  Bind(Param,Data.Memory,Data.Size);
end;

procedure TSQLRequest.BindNull(Param: Integer);
begin
  sqlite3_check(RequestDB,sqlite3_bind_null(Request,Param));
end;

procedure TSQLRequest.BindReset;
begin
  if Request<>0 then
    sqlite3_clear_bindings(Request);
end;

procedure TSQLRequest.BindZero(Param, Size: integer);
begin
  sqlite3_check(RequestDB,sqlite3_bind_zeroblob(Request,Param,Size));
end;

procedure TSQLRequest.Close;
begin
  if Request=0 then
    exit;
  sqlite3_finalize(Request);
  fRequest := 0;
  fFieldCount := 0;
end;

procedure TSQLRequest.ExecuteAll;
begin
  if RequestDB=0 then
    raise ESQLite3Exception.Create(0,SQLITE_CANTOPEN);
  try
    repeat
      repeat
      until Step<>SQLITE_ROW; // all steps of this statement
    until PrepareNext=SQLITE_DONE; // all statements
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.Execute;
begin
  if RequestDB=0 then
    raise ESQLite3Exception.Create(0,SQLITE_CANTOPEN);
  try
    repeat
    until Step<>SQLITE_ROW; // Execute all steps of the first statement
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.ExecuteAll(aDB: TSQLite3DB; const aSQL: RawUTF8);
begin
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    ExecuteAll;
  finally
    Close; // always release statement, even if done normaly in EngineExecuteAll
  end;
end;

procedure TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8);
begin
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    Execute;
  finally
    Close; // always release statement, even if done normaly in Execute
  end;
end;

function TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var ID: TInt64DynArray): integer;
var LID, Res: integer;
begin
  result := 0;
  LID := length(ID);
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    repeat
      res := Step;
      if res=SQLITE_ROW then begin
        if result>=LID then begin
          inc(LID,256);
          SetLength(ID,LID);
        end;
        ID[result] := sqlite3_column_int64(Request,0); // get first column value
        inc(result);
      end;
    until res=SQLITE_DONE;
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out ID: Int64);
begin
  ID := 0;
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    if Step=SQLITE_ROW then
      ID := sqlite3_column_int64(Request,0); // get first column value
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out Value: RawUTF8);
begin
  Value := '';
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    if Step=SQLITE_ROW then
      Value := sqlite3_column_text(Request,0); // get first column value
  finally
    Close; // always release statement
  end;
end;

function TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer;
var LValues, Res: integer;
begin
  result := 0;
  LValues := length(Values);
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    repeat
      res := Step;
      if res=SQLITE_ROW then begin
        if result>=LValues then begin
          if LValues<256 then
            inc(LValues,16) else
            inc(LValues,256);
          SetLength(Values,LValues);
        end;
        Values[result] := sqlite3_column_text(Request,0); // get first column value
        inc(result);
      end;
    until res=SQLITE_DONE;
  finally
    Close; // always release statement
  end;
end;

function TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; JSON: TStream;
  Expand: boolean=false): PtrInt;
// expand=true: [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
// expand=false: { "FieldCount":2,"Values":["col1","col2",val11,"val12",val21,..] }
var i: integer;
    W: TJSONWriter;
begin
  result := 0;
  W := TJSONWriter.Create(JSON,Expand,false);
  try
    // prepare the SQL request
    if aSQL<>'' then // if not already prepared, reset and bound by caller
      Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount<=0 then begin
      W.CancelAllVoid;
      exit;
    end;
    // get col names and types
    SetLength(W.ColNames,FieldCount);
    for i := 0 to FieldCount-1 do
      W.ColNames[i] := sqlite3_column_name(Request,i);
    W.AddColumns; // write or init field names for appropriate JSON Expand
    if Expand then
      W.Add('[');
    // write rows data
    repeat
      case Step of
      SQLITE_ROW: begin
        inc(result);
        FieldsToJSON(W);
        W.Add(',');
      end;
      SQLITE_DONE:
        break;
      end;
    until false;
    if (result=0) and W.Expand then begin
      // we want the field names at least, even with no data: we allow RowCount=0
      Expand := false; //  {"FieldCount":2,"Values":["col1","col2"]}
      W.Expand := false;
      W.CancelAll;
      for i := 0 to FieldCount-1 do
        W.ColNames[i] := sqlite3_column_name(Request,i);
      W.AddColumns;
    end;
    W.CancelLastComma; // cancel last ','
    W.Add(']');
    if not Expand then
      W.Add('}');
    W.Add(#10);
    W.Flush;
  finally
    try
      if aSQL<>'' then
        Close; // always release statement (if not prepared and closed by caller)
    finally
      W.Free;
    end;
  end;
end;

procedure TSQLRequest.ExecuteDebug(aDB: TSQLite3DB; const aSQL: RawUTF8; var OutFile: Text);
var Res, i, n: integer;
begin
  {$I-}
  writeln;
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    repeat
      repeat
        Res := Step;
        if Res=SQLITE_ROW then begin
          n := FieldCount-1;
          for i := 0 to n do begin
            write(OutFile,FieldA(i));
            if i<n then
              write(OutFile,'|');
          end;
          writeln(OutFile);
        end;
      until Res=SQLITE_DONE;
    until PrepareNext=SQLITE_DONE;
  finally
    {$I+}ioresult;
    Close; // always release statement
  end;
end;

function TSQLRequest.ExecuteJSON(aDB: TSQLite3DB; const aSQL: RawUTF8;
  Expand: boolean=false; aResultCount: PPtrInt=nil): RawUTF8;
var Stream: TRawByteStringStream;
    RowCount: PtrInt;
begin
  Stream := TRawByteStringStream.Create;
  try
    try
      RowCount := Execute(aDB,aSQL,Stream,Expand); // create JSON data in Stream
      if aResultCount<>nil then
        aResultCount^ := RowCount;
      result := Stream.DataString;
    except
      on ESQLite3Exception do
        result := '';
    end;
    // Close has been called in Execute() above since aSQL<>''
  finally
    Stream.Free;
  end;
end;

function TSQLRequest.FieldA(Col: integer): WinAnsiString;
var P: PUTF8Char;
    L,L2: integer;
begin
  result := '';
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  P := sqlite3_column_text(Request,Col);
  L := SynCommons.StrLen(P); // faster than sqlite3_column_bytes(Request,Col)
  if L>0 then begin
    SetLength(result,L);
    L2 := UTF8ToWinPChar(pointer(result),P,L);
    if L2<>L then
      SetLength(result,L2);
  end;
end;

function TSQLRequest.FieldBlob(Col: integer): RawByteString;
var P: PAnsiChar;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  P := sqlite3_column_blob(Request,Col);
  SetString(result,P,sqlite3_column_bytes(Request,Col));
end;

function TSQLRequest.FieldBlobToStream(Col: integer): TStream;
begin
  result := TRawByteStringStream.Create(FieldBlob(Col));
end;

function TSQLRequest.FieldDouble(Col: integer): double;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  result := sqlite3_column_double(Request,Col);
end;

function TSQLRequest.FieldInt(Col: integer): Int64;
begin // internaly, SQLite always uses Int64 -> pure Integer function is useless
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  result := sqlite3_column_int64(Request,Col);
end;

function TSQLRequest.FieldName(Col: integer): RawUTF8;
var P: PUTF8Char;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  P := sqlite3_column_name(Request,Col);
  SetString(result,P,SynCommons.StrLen(P));
end;

function TSQLRequest.FieldIndex(const aColumnName: RawUTF8): integer;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE);
  for result := 0 to FieldCount-1 do
    if StrIComp(pointer(aColumnName),sqlite3_column_name(Request,result))=0 then
      exit;
  result := -1; // not found
end;

function TSQLRequest.FieldNull(Col: Integer): Boolean;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  result := sqlite3_column_type(Request,Col)=SQLITE_NULL;
end;

function TSQLRequest.FieldType(Col: Integer): integer;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  result := sqlite3_column_type(Request,Col);
end;

function TSQLRequest.FieldUTF8(Col: integer): RawUTF8;
var P: PUTF8Char;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  P := pointer(sqlite3_column_text(Request,Col));
  SetString(result,P,SynCommons.StrLen(P));
end;

function TSQLRequest.FieldValue(Col: integer): TSQLite3Value;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  result := sqlite3_column_value(Request,Col);
end;

function TSQLRequest.FieldW(Col: integer): RawUnicode;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE);
  result := Utf8DecodeToRawUnicode(sqlite3_column_text(Request,Col),0);
end;

function TSQLRequest.Prepare(DB: TSQLite3DB; const SQL: RawUTF8): integer;
begin
  fDB := DB;
  fRequest := 0;
  if DB=0 then
    raise ESQLite3Exception.Create(DB,SQLITE_CANTOPEN);
  result := sqlite3_prepare_v2(RequestDB, pointer(SQL), length(SQL)+1, fRequest, fNextSQL);
  while (result=SQLITE_OK) and (Request=0) do // comment or white-space
    result := sqlite3_prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
  fFieldCount := sqlite3_column_count(fRequest);
  sqlite3_check(RequestDB,result);
end;

function TSQLRequest.PrepareAnsi(DB: TSQLite3DB; const SQL: WinAnsiString): integer;
begin
  result := Prepare(DB,WinAnsiToUtf8(SQL));
end;

function TSQLRequest.PrepareNext: integer;
begin
  if (Request=0) or (fNextSQL^=#0) then
    result := SQLITE_DONE else begin
    Close; // free previous statement
    result := sqlite3_prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
    while (result=SQLITE_OK) and (Request=0) and (fNextSQL^<>#0) do
      // comment or white-space -> ignore
      result := sqlite3_prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
    fFieldCount := sqlite3_column_count(fRequest);
    sqlite3_check(RequestDB,result);
    if Request=0 then
      result := SQLITE_DONE; // nothing more to add
  end;
end;

function TSQLRequest.Reset: integer;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE);
  result := sqlite3_reset(Request); // no check here since it was PREVIOUS state
end;

function TSQLRequest.Step: integer;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE);
  result := sqlite3_check(RequestDB,sqlite3_step(Request));
end;

function TSQLRequest.GetReadOnly: Boolean;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE);
  result := sqlite3_stmt_readonly(Request);
end;

procedure TSQLRequest.FieldsToJSON(WR: TJSONWriter);
var i: integer;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE);
  if WR.Expand then
    WR.Add('{');
  for i := 0 to FieldCount-1 do begin
    if WR.Expand then
      WR.AddString(WR.ColNames[i]); // '"'+ColNames[]+'":'
    case sqlite3_column_type(Request,i) of // fast evaluation: type may vary
      SQLITE_BLOB: 
        WR.WrBase64(sqlite3_column_blob(Request,i),
          sqlite3_column_bytes(Request,i),true); // withMagic=true
      SQLITE_NULL:
        WR.AddNoJSONEscape(PAnsiChar('null'),4); // returned also for ""
      SQLITE_INTEGER:
        WR.Add(sqlite3_column_int64(Request,i));
      SQLITE_FLOAT:
        WR.Add(sqlite3_column_double(Request,i));
      SQLITE_TEXT: begin
        WR.Add('"');
        WR.AddJSONEscape(sqlite3_column_text(Request,i),0);
        WR.Add('"');
       end;
    end; // case ColTypes[]
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

function TSQLRequest.GetParamCount: integer;
begin
  if Request=0 then
    result := 0 else
    result := sqlite3_bind_parameter_count(Request);
end;


{ ESQLite3Exception }

constructor ESQLite3Exception.Create(aDB: TSQLite3DB; aErrorCode: integer);
begin
  if aDB=0 then
    CreateFmt(sErrorSQLite3NoDB,[aErrorCode]) else
    Create(string(sqlite3_errmsg(aDB)),aErrorCode);
  DB := aDB;
end;

constructor ESQLite3Exception.Create(const aMessage: string; aErrorCode: integer);
begin
  ErrorCode := aErrorCode;
  Create(aMessage);
end;

function sqlite3_check(DB: TSQLite3DB; aResult: integer): integer;
begin
  if (DB=0) or (aResult in [SQLITE_ERROR..SQLITE_ROW-1]) then // possible error codes
    raise ESQLite3Exception.Create(DB,aResult);
  result := aResult;
end;


{ TSQLBlobStream }

constructor TSQLBlobStream.Create(aDB: TSQLite3DB; const DBName, TableName,
  ColumnName: RawUTF8; RowID: Int64; ReadWrite: boolean);
begin
  fDB := aDB;
  fWritable := ReadWrite;
  sqlite3_check(aDB,sqlite3_blob_open(aDB,pointer(DBName),pointer(TableName),pointer(ColumnName),
    RowID,integer(ReadWrite),fBlob));
  fSize := sqlite3_blob_bytes(fBlob);
end;

destructor TSQLBlobStream.Destroy;
begin
  sqlite3_blob_close(fBlob);
  inherited;
end;

function TSQLBlobStream.Read(var Buffer; Count: Integer): Longint;
begin
  result := fSize-fPosition; // bytes available left
  if Count<result then // read only inside the Blob size
    result := Count;
  if result<>0 then begin
    sqlite3_check(fDB,sqlite3_blob_read(fBlob,Buffer,result,fPosition));
    inc(fPosition,result);
  end;
end;

function TSQLBlobStream.Seek(Offset: Integer; Origin: word): Longint;
begin
  case Origin of
    soFromBeginning: fPosition := Offset;
    soFromCurrent: Inc(fPosition, Offset);
    soFromEnd: fPosition := fSize + Offset;
  end;
  if fPosition>fSize then
    fPosition := fSize;
  Result := fPosition;
end;

function TSQLBlobStream.Write(const Buffer; Count: Integer): Longint;
begin
  result := fSize-fPosition; // bytes available left
  if Count<result then
    result := Count; // write only inside the Blob size
  if result<>0 then begin
    sqlite3_check(fDB,sqlite3_blob_write(fBlob,Buffer,result,fPosition));
    inc(fPosition,result);
  end;
end;


{ TSQLDataBaseSQLFunction }

constructor TSQLDataBaseSQLFunction.Create(aFunction: TSQLFunctionFunc;
  aFunctionParametersCount: Integer; const aFunctionName: RawUTF8);
begin
  fInternalFunction := aFunction;
  fFunctionParametersCount := aFunctionParametersCount;
  if aFunctionName='' then
    fSQLName := RawUTF8(copy(ClassName,2,maxInt)) else
    fSQLName := aFunctionName;
end;

function TSQLDataBaseSQLFunction.CreateFunction(DB: TSQLite3DB): Integer;
begin
  if self<>nil then begin
    result := sqlite3_create_function_v2(DB,pointer(fSQLName),
      FunctionParametersCount,SQLITE_ANY,self,fInternalFunction,nil,nil,nil);
    {$ifdef WITHLOG}
    if result<>SQLITE_OK then 
      SynSQLite3Log.Add.Log(sllError,'register SQL function failed: '+FunctionName,self);
    {$endif}
  end else
    result := SQLITE_ERROR;
end;


{ TSQLDataBaseSQLFunctionDynArray }

procedure InternalSQLFunctionDynArrayBlob(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
var DynArray, Elem: pointer;
    Func: TSQLDataBaseSQLFunctionDynArray;
begin
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  DynArray := sqlite3_value_blob(argv[0]);
  Elem := sqlite3_value_blob(argv[1]); 
  Func := sqlite3_user_data(Context);
  if (DynArray<>nil) and (Elem<>nil) and (Func<>nil) then
  with Func.fDummyDynArray do
  try
    LoadFrom(DynArray); // temporary allocate all dynamic array content
    try
      if ElemLoadFind(Elem)<0 then
        DynArray := nil;
    finally
      Clear; // release temporary array content in fDummyDynArrayValue
    end;
  except
    on Exception do begin
      sqlite3_result_error(Context,'Invalid BLOB content');
      exit;
    end;
  end else
    DynArray := nil;
  sqlite3_result_int64(Context,Int64(DynArray<>nil));
end;

constructor TSQLDataBaseSQLFunctionDynArray.Create(aTypeInfo: pointer;
  aCompare: TDynArraySortCompare; const aFunctionName: RawUTF8);
begin
  fDummyDynArray.Init(aTypeInfo,fDummyDynArrayValue);
  fDummyDynArray.Compare := aCompare;
  inherited Create(InternalSQLFunctionDynArrayBlob,2,aFunctionName);
end;


{ TSQLStatementCached }

procedure TSQLStatementCached.Init(aDB: TSQLite3DB);
begin
  Caches.Init(TypeInfo(TSQLStatementCacheDynArray),Cache,nil,nil,nil,@Count);
  DB := aDB;
end;

function TSQLStatementCached.Prepare(const GenericSQL: RaWUTF8): PSQLRequest;
var added: boolean;
begin
  with Cache[Caches.FindHashedForAdding(GenericSQL,added)] do begin
    if added then begin
      StatementSQL := GenericSQL;
      Statement.Prepare(DB,GenericSQL);
    end else begin
      Statement.BindReset;
      Statement.Reset;
    end;
    result := @Statement;
  end;
end;

procedure TSQLStatementCached.ReleaseAllDBStatements;
var i: integer;
begin
  for i := 0 to Count-1 do
    Cache[i].Statement.Close; // close prepared statement
  Caches.Clear;
  Caches.ReHash; // need to refresh all hashs
end;
    

initialization
  sqlite3_initialize; // so sqlite3.c is compiled with SQLITE_OMIT_AUTOINIT defined

finalization
  sqlite3_shutdown;
end.
