/// SQLite3 embedded Database engine used as the kernel of mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3;

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

  The Original Code is Synopse SQLite3/mORMot database framework.

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


    Initial version: 2008 March, by Arnaud Bouchez

    Version 1.3 - January 22, 2010
    - allow backup directly into a custom file (not only a custom directory)
    - on-the-fly Restore of the database from a compressed backup file
    - some small fixes and multi-compiler enhancements
    - compiler conditional renamed ENHANCEDRTL instead of ENHANCEDTRTL

    Version 1.4 - February 08, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.5 - March 10, 2010
    - updated engine to version 3.6.23
    - included FTS3 embedded sqlite3fts3.obj version

    Version 1.6
    - SQLite3 database layer updated to version 3.6.23.1

    Version 1.7
    - alter table with newly added fields to a TSQLRecord

    Version 1.8
    - includes Unitary Testing class and functions
    - database update engine to version 3.7.0 (main new feature is WAL)
    - SetWALMode() method for enabling Write-Ahead Logging for the database
    - the RTREE extension is now compiled by default into the engine
    - new tests added (mostly relative to the new functions or classes)

    Version 1.9
    - database engine updated to 3.7.1
    - fix issue in TSQLRestServerDB.CreateMissingTables when an exception occured
    - handle now RowID as a valid alias to the ID field (needed for TSQLRecordFTS3)
    - handle TSQLRecordFTS3 record, for using FTS3 virtual tables,
      i.e. implementing full-text search (including dedicated regression tests)

    Version 1.9.1
    - update engine to version 3.7.2: an obscure but very old bug makes
      SQLite authors recommend to use 3.7.2 for all new development.
      Upgrading from all prior SQLite versions is also recommended.

    Version 1.9.2
    - fixed a potential GPF in TSQLRestClientDB.Destroy

    Version 1.10
    - code modifications to compile with Delphi 6 compiler (Delphi 5 failed due
      to some obscure compiler bugs in SynCrypto.pas)
    - update SQLite3 engine to version 3.7.3

    Version 1.11
    - update SQLite3 engine to version 3.7.4
    - introduces new TSQLite3DB, TSQLite3Statement, TSQLite3Blob, TSQLite3Value
      and TSQLite3FunctionContext types to clarify SQLite3 internal handle usage
    - new sqlite3_busy_timeout and sqlite3_busy_handler low-level functions
    - new TSQLDataBase.BusyTimeout property, to set the database timeout
      value in milliseconds
    - now handles User Defined Functions, via sqlite3_create_function_v2 and
      corresponding sqlite3_result_* functions: as sample, the MOD() function
      is defined in any database opened via TSQLDataBase (it was needed
      to have compatibility with Oracle/MySQL/MSSQL/PostGreSQL engines)
    - protect the TSQLDatabase methods called when self is nil, which could
      occur if the database is not yet initialized (could occur if only a
      TSQLRestServerStatic exists, like in TTestSQLite3Engine._TSQLRestClientDB)
    - new SOUNDEX() function available in SQL statements (calling SoundExUTF8)
      and associated SOUNDEXFR/SOUNDEXES for french or spanish Soundex variants
    - fixed an issue found out by WladiD about all collation functions:
      If a field with your custom collate ISO8601 is empty '' (not NULL),
      then SQLite calls the registered collate function with length 0 for s1len
      or s2len, but the pointers s1 or s2 map to the string of the previous call
    - added sqlite3_result_error() call to make wrong parameter count error
      explicit during SQL statement internal functions calls
    - handles new FTS4 extension module  - see
      http://sqlite.org/fts3.html#section_1_1 - which is available since 3.7.4
    - new RANK() function available in SQL statements for ranking FTS3/FTS4
      with best performance (used by the new TSQLRest.FTSMatch() overloaded
      method) - see http://www.sqlite.org/fts3.html#appendix_a
    - fixed dual memory release in case of FTS3 use, in TSQLDataBase.Destroy
    - source code modified to be 7 bit Ansi (so will work with all encodings)

    Version 1.12
    - update SQLite3 engine to version 3.7.5
    - fixed sqlite3_result_text() implementation
    - added sqlite3_aggregate_context(), sqlite3_value_numeric_type() and
      sqlite3InternalFree() functions
    - new CONCAT() function available in SQL statements to process fast
      string concatenation
    - now handle automaticaly prepared SQL statements: the parameters must
      be surrounded with :(...): in order to use an internal pool of prepared
      TSQLRequest; example of possible inlined values are :(1234):
      :(12.34): :(12E-34): :("text"): or :('text'): (with double quoting
      inside the text, just like any SQL statement)
    - new sqlite3_stmt_readonly() function and TSQLRequest.IsReadOnly property

    Version 1.13
    - update SQLite3 engine to version 3.7.6.3
    - added sqlite3InternalFreeObject(), sqlite3_malloc/realloc/free(),
      sqlite3_memory_used/highwater(), sqlite3_set_authorizer() and
      sqlite3_update/commit/rollback_hook() functions
    - introducing TSQLVirtualTableModuleSQLite3 / TSQLVirtualTableModuleServerDB
      classes, and associated low-level sqlite3 functions and memory structures,
      in order to implement Virtual Table modules in pure Delphi code via
      common TSQLVirtualTable classes as defined in SQLite3Commons
    - new IntegerDynArrayContains(), RawUTF8DynArrayContainsCase/NoCase() and
      Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
      SQL functions, able to fast search data within T*DynArray and
      TRawUTF8DynArray published properties BLOB (Currency mapped as PInt64)
    - new TSQLDataBaseSQLFunction and TSQLDataBase.RegisterSQLFunction method,
      to implement custom SQL functions with any kind of BLOB data
    - regression test now exclude unfixed order for select (may vary,
      just like happened for 3.7.6 when e.g. indexes started to be used)
    - added regression tests for sftBlobDynArray and sftObject kind of records
    - TSQLRestServerDB now uses faster TDynArrayHashed for its internal prepared
      statement cache
    - fixed issue in TSQLRestClientDB.URI: wrong InternalState returned
    - fastest internal cdecl qsort() function (optimized for PAnsiChar)

    Version 1.14
    - update SQLite3 engine to version 3.7.7.1
    - fixed issue in produced JSON stream using '=' instead of ':'
    - TSQLDatabase.user_version now defined as a property, with a getter and
      a setter methods (not read/only any more)

    Version 1.15
    - updated SQLite3 engine to version 3.7.8
    - unit now tested with Delphi XE2 (32 Bit)
    - transactions now following a safe concurent access (both thread-safe and
      client/connection-safe) - but authentication should be enabled
    - the SQLite3 wrapper is now located in a separate SynSQLite3 unit: this
      will allow to use it as a separate database engine, e.g. using SynDB
      classes without the overhead/features of our mORMot framework
    - statement cache is now shared with SynDBSQLite3, via the new
      TSQLStatementCached object as defined in SynSQLite3.pas
    - now TSQLRestServerDB will unregister any TSQLVirtualTableModuleServerDB
      to avoid random GPF in TSQLVirtualTable.Destroy
    - TSQLRestClientDB and TSQLRestServerDB constructors now accept an optional
      Password parameter, associated to the supplied file name, in order to
      use database encryption

    Version 1.16
    - updated SQLite3 engine to version 3.7.12.1
    - unit now includes FTS3/FTS4 by default (i.e. INCLUDE_FTS3 conditional is
      set in both SQLite3.pas and SynSQLite3.pas units)
    - fixed TSQLRestServerDB.UpdateField(ByID=true) implementation
    - fixed VACUUM failure if there are one or more active SQL statements
    - new overloaded TSQLRestServerDB.UpdateField method
    - TSQLRestServerDB.EngineList()  method now handles an optional integer
      pointer, to return the count of row data
    - updated TSQLRestServerTest published methods to use the new parameter
      layout using "var aParams: TSQLRestServerCallBackParams"

    Version 1.17
    - updated SQLite3 engine to version 3.7.14
    - added overriden TSQLRestServerDB.FlushInternalDBCache method
    - added TSQLRestServerDB.BackupGZ method for live database backup into a
      compressed .gz archive file

}

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 WITHLOG

uses
  Windows,
  SysUtils,
  Classes,
{$ifndef LVCL}
  Contnrs,
{$endif}
  SynZip,
  SynCommons,
  SynSQLite3,
  SQLite3Commons;

{$define INCLUDE_FTS3}
{ define this if you want to include the FTS3/FTS4 feature into the library
  - FTS3 is an SQLite module implementing full-text search
  - will include also FTS4 extension module since 3.7.4
  - see http://www.sqlite.org/fts3.html for documentation
  - is defined by default, but can be unset to save about 50 KB of code size
  - should be defined for both SynSQLite3 and SQLite3 units }

{.$define USEFASTCALL}
{ use the fastcall calling convention to access the SQLite3 library
  - BCC32 -pr fastcall (=Delphi resgister) is buggy, don't know why
   (because of issues with BCC32 itself, or some obfuscated calls in source?)
  - should be defined for both SynSQLite3 and SQLite3 units }


{ ****************** SQLite3 database used as kernel of our mORMot framework } 

type
  /// Execute a SQL statement in the local SQLite3 database engine, and get
  // result in memory
  // - all DATA (even the BLOB fields) is converted into UTF-8 TEXT
  // - uses a TSQLTableJSON internaly: faster than sqlite3_get_table()
  // (less memory allocation/fragmentation) and allows efficient caching
  TSQLTableDB = class(TSQLTableJSON)
  private
  public
    {{ Execute a SQL statement, and init TSQLTable fields
     - FieldCount=0 if no result is returned
     - the BLOB data is converted into TEXT: you have to retrieve it with
      a special request explicitely (note that JSON format returns BLOB data)
     - uses a TSQLTableJSON internaly: all currency is transformed to its floating
       point TEXT representation, and allows efficient caching
     - if the SQL statement is in the DB cache, it's retrieved from its cached
       value: our JSON parsing is a lot faster than SQLite3 engine itself,
       and uses less memory
     - will raise an ESQLException on any error }
    constructor Create(aDB: TSQLDatabase; const Tables: array of TClass;
      const aSQL: RawUTF8; Expand: boolean);
  end;

  TSQLRestServerDBClass = class of TSQLRestServerDB;

  TSQLVirtualTableModuleServerDB = class;

  /// REST server with direct access to a SQLite3 database
  // - caching is handled at TSQLDatabase level
  // - SQL statements for record retrieval from ID are prepared for speed
  TSQLRestServerDB = class(TSQLRestServer) 
  private
    /// internal copy of the SQLite3 database engine
    fDB: TSQLDataBase;
    /// initialized by Create(aModel,aDBFileName)
    fOwnedDB: TSQLDataBase;
    /// prepared statements with parameters for faster SQLite3 execution
    // - works for SQL code with :(%): internal parameters
    fStatementCache: TSQLStatementCached;
    /// static statement used if fStatementCache is not to be used
    // - for any SQL code with no :(%): internal parameters
    // - only one global is OK: is protected via a nested DB.LockJSON/DB.Unlock
    fStaticStatement: TSQLRequest;
    /// used to unregister Module.Server to avoid GPF in TSQLVirtualTable.Destroy
    fRegisteredVirtualTableModule: array of TSQLVirtualTableModuleServerDB;
  protected
    /// retrieve a TSQLRequest instance, corresponding to any previous
    // prepared statement using :(%): internal parameters
    // - will return @fStaticStatement if no :(%): internal parameters appear:
    // in this case, the TSQLRequest.Close method must be called
    // - will return a @fStatementCache[].Statement, after having bounded the
    // :(%): parameter values; in this case, TSQLRequest.Close must not be called
    // - expect sftBlob, sftBlobDynArray and sftBlobRecord properties
    // to be encoded as ':("\uFFF0base64encodedbinary"):'
    function GetAndPrepareStatement(const SQL: RawUTF8): PSQLRequest;
    /// reset the cache if necessary
    procedure SetNoAJAXJSON(const Value: boolean); override;
    /// overriden methods for direct sqlite3 database engine call:
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false; ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    function EngineRetrieve(TableModelIndex, ID: integer): RawUTF8; override;
    function EngineAdd(Table: TSQLRecordClass; const SentData: RawUTF8): integer; override;
    function EngineUpdate(Table: TSQLRecordClass; ID: integer; const SentData: RawUTF8): boolean; override;
    function EngineDelete(Table: TSQLRecordClass; ID: integer): boolean; override;
    function EngineDeleteWhere(Table: TSQLRecordClass; const SQLWhere: RawUTF8;
      const IDs: TIntegerDynArray): boolean; override;
    function EngineRetrieveBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateField(Table: TSQLRecordClass;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    /// execute one SQL statement
    // - intercept any DB exception and return false on error, true on success
    function EngineExecute(const aSQL: RawUTF8; ValueInt: PInt64=nil; ValueUTF8: PRawUTF8=nil): boolean; overload;
    /// EngineExecute directly a SQL statement with supplied parameters
    // - expect the same format as FormatUTF8() function, that is only % with
    // strings (already quoted) and integers (not ? with parameters)
    // - return true on success
    function EngineExecuteFmt(SQLFormat: PUTF8Char; const Args: array of const): boolean;
    /// execute one SQL statement, which must return an integer (Int64) value
    // - intercept any DB exception and return false on error, true on success
    function EngineExecute(const aSQL: RawUTF8; out Value: Int64): boolean; overload;
    /// execute one SQL statement, which must return a UTF-8 encoded value
    // - intercept any DB exception and return false on error, true on success
    function EngineExecute(const aSQL: RawUTF8; out Value: RawUTF8): boolean; overload;
    /// execute one SQL statement, and apply an Event to every record
    // - lock the database during the run
    // - call a fast "stored procedure"-like method for each row of the request;
    // this method must use low-level DB access in any attempt to modify the
    // database (e.g. a prepared TSQLRequest with Reset+Bind+Step), and not
    // the TSQLRestServerDB.Engine*() methods which include a Lock(): this Lock()
    // is performed by the main loop in EngineExecute() and any attempt to
    // such high-level call will fail into an endless loop
    // - caller may use a transaction in order to speed up StoredProc() writing
    // - intercept any DB exception and return false on error, true on success
    function EngineExecute(const aSQL: RawUTF8; StoredProc: TOnSQLStoredProc): boolean; overload;
  public
    {{ begin a transaction (implements REST BEGIN Member)
     - to be used to speed up some SQL statements like Insert/Update/Delete
     - must be ended with Commit on success
     - must be aborted with Rollback if any SQL statement failed
     - return true if no transaction is active, false otherwize }
    function TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean; override;
    {{ end a transaction (implements REST END Member)
     - write all pending SQL statements to the disk }
    procedure Commit(SessionID: cardinal=1); override;
    {{ abort a transaction (implements REST ABORT Member)
     - restore the previous state of the database, before the call to TransactionBegin }
    procedure RollBack(SessionID: cardinal=1); override;
    /// overriden methods for direct sqlite3 database engine call
    function EngineExecuteAll(const aSQL: RawUTF8): boolean; override;
    {{ backup of the opened Database into an external stream (e.g. a file,
      compressed or not)
     - this method doesn't use the experimental SQLite Online Backup API
      (which restart the backup process on any write: so no good performance
      could be achieved on a working database: this method uses a faster
      lock + copy approach)
     - database is closed, VACCUUMed, copied, then reopened }
    function Backup(Dest: TStream): boolean;
    {{ backup of the opened Database into a .gz compressed file
     - database is closed, VACCUUMed, compressed into .gz file, then reopened
     - default compression level is 2, which is very fast, and good enough for
       a database file content: you may change it into the default 6 level }
    function BackupGZ(const DestFileName: TFileName; CompressionLevel: integer=2): boolean;
    {{ restore a database content on the fly
     - database is closed, source DB file is replaced by the supplied content,
       then reopened }
    function Restore(const ContentToRestore: RawByteString): boolean;
    /// initialize the associated DB connection
    // - called by Create and on Backup/Restore just after DB.DBOpen
    // - will register all *_in() functions for available TSQLRecordRTree
    // - will register all modules for available TSQLRecordVirtualTable*ID
    // with already registered modules via RegisterVirtualTableModule()
    // - you can override this method to call e.g. DB.RegisterSQLFunction()
    procedure InitializeEngine; virtual;
    /// call this method when the internal DB content is known to be invalid
    // - by default, all REST/CRUD requests and direct SQL statements are
    // scanned and identified as potentially able to change the internal SQL/JSON
    // cache used at SQLite3 database level; but some virtual tables (e.g.
    // TSQLRestServerStaticExternal classes defined in SQLite3DB) could flush
    // the database content without proper notification
    // - this overriden implementation will call TSQLDataBase.CacheFlush method
    procedure FlushInternalDBCache; override;
public
    /// initialize a REST server with a database
    // - any needed TSQLVirtualTable class should have been already registered
    // via the RegisterVirtualTableModule() method
    constructor Create(aModel: TSQLModel; aDB: TSQLDataBase;
      aHandleUserAuthentication: boolean=false); overload; virtual;
    /// initialize a REST server with a database, by specifying its filename
    // - TSQLRestServerDB will initialize a owned TSQLDataBase, and free it on Destroy
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    // - it will then call the other overloaded constructor to initialize the server
    constructor Create(aModel: TSQLModel; const aDBFileName: TFileName;
      aHandleUserAuthentication: boolean=false; const aPassword: RawUTF8=''); reintroduce; overload;
    /// close database and free used memory
    destructor Destroy; override;
    /// Missing tables are created if they don't exist yet for every TSQLRecord
    // class of the Database Model
    // - you must call explicitely this before having called StaticDataCreate()
    // - all table description (even Unique feature) is retrieved from the Model
    // - this method also create additional fields, if the TSQLRecord definition
    // has been modified; only field adding is available, field renaming or
    // field deleting are not allowed in the FrameWork (in such cases, you must
    // create a new TSQLRecord type)
    procedure CreateMissingTables(user_version: cardinal=0);

    /// associated database
    property DB: TSQLDataBase read fDB;
  end;

  /// REST client with direct access to a SQLite3 database
  // - a hidden TSQLRestServerDB server is created and called internaly
  TSQLRestClientDB = class(TSQLRestClientURI)
  private
    // use internaly a TSQLRestServerDB to access data in the proper JSON format
    fServer: TSQLRestServerDB;
    fOwnedDB: TSQLDataBase;
    function getDB: TSQLDataBase;
  protected
    /// method calling the RESTful server fServer
    function InternalURI(const url, method: RawUTF8; Resp: PRawUTF8=nil;
      Head: PRawUTF8=nil; SendData: PRawUTF8=nil): Int64Rec; override;
  public
    /// initializes the class, and creates an internal TSQLRestServerDB to
    // internaly answer to the REST queries
    // - aServerClass could be TSQLRestServerDB by default
    constructor Create(aClientModel, aServerModel: TSQLModel; aDB: TSQLDataBase;
      aServerClass: TSQLRestServerDBClass;
      aHandleUserAuthentication: boolean=false); reintroduce; overload;
    /// same as above, from a SQLite3 filename specified
    // - an internal TSQLDataBase will be created internaly and freed on Destroy
    // - aServerClass could be TSQLRestServerDB by default
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    constructor Create(aClientModel, aServerModel: TSQLModel; const aDBFileName: TFileName;
      aServerClass: TSQLRestServerDBClass;
      aHandleUserAuthentication: boolean=false; const aPassword: RawUTF8=''); reintroduce; overload;
    /// release the server
    destructor Destroy; override;

    /// retrieve a list of members as a TSQLTable (implements REST GET Collection)
    // - this overriden method call directly the database to get its result,
    // without any URI() call, but with use of DB JSON cache if available 
    // - other TSQLRestClientDB methods use URI() function and JSON conversion
    // of only one record properties values, which is very fast
    function List(const Tables: array of TSQLRecordClass; const SQLSelect: RawUTF8 = 'ID';
      const SQLWhere: RawUTF8 = ''): TSQLTableJSON; override;
    /// associated Server
    property Server: TSQLRestServerDB read fServer;
    /// associated database
    property DB: TSQLDataBase read getDB;
  end;

  /// this class defined two published methods of type TSQLRestServerCallBack in
  //  order to test the Server-Side ModelRoot/TableName/ID/MethodName RESTful model
  TSQLRestServerTest = class(TSQLRestServerDB)
  published
    {/ test ModelRoot/People/ID/DataAsHex
     - this method is called by TSQLRestServer.URI when a
       ModelRoot/People/ID/DataAsHex GET request is provided
     - Parameters values are not used here: this service only need aRecord.ID
     - SentData is set with incoming data from a PUT method
     - if called from ModelRoot/People/ID/DataAsHex with GET or PUT methods,
       TSQLRestServer.URI will create a TSQLRecord instance and set its ID
       (but won't retrieve its other field values automaticaly)
     - if called from ModelRoot/People/DataAsHex with GET or PUT methods,
       TSQLRestServer.URI will leave aRecord.ID=0 before launching it
     - if called from ModelRoot/DataAsHex with GET or PUT methods,
       TSQLRestServer.URI will leave aRecord=nil before launching it
     - implementation must return the HTTP error code (e.g. 200 as success)
     - Table is overloaded as TSQLRecordPeople here, and still match the
       TSQLRestServerCallBack prototype: but you have to check the class
       at runtime: it can be called by another similar but invalid URL, like
       ModelRoot/OtherTableName/ID/DataAsHex }
    function DataAsHex(var aParams: TSQLRestServerCallBackParams): Integer;
    {/ method used to test the Server-Side ModelRoot/Sum or
       ModelRoot/People/Sum Requests
      - implementation of this method returns the sum of two floating-points,
        named A and B, as in the public TSQLRecordPeople.Sum() method,
        which implements the Client-Side of this service
      - Table nor ID are never used here }
    function Sum(var aParams: TSQLRestServerCallBackParams): Integer;
  end;

  {{ define a Virtual Table module for a stand-alone SQLite3 engine
   - it's not needed to free this instance: it will be destroyed by the SQLite3
   engine together with the DB connection }
  TSQLVirtualTableModuleSQLite3 = class(TSQLVirtualTableModule)
  protected
    fDB: TSQLDataBase;
    /// used internaly to register the module to the SQLite3 engine
    fModule: TSQLite3Module;
    /// initialize the DB connection via sqlite3_create_module_v2(fModule)
    procedure SetDB(aDB: TSQLDataBase);
  public
    /// retrieve the file name to be used for a specific Virtual Table
    // - overriden method returning a file located in the DB file folder, and
    // '' if the main DB was created as ':memory:' (so no file should be written)
    // - of course, if a custom FilePath property value is specified, it will be
    // used, even if the DB is created as ':memory:'
    function FileName(const aTableName: RawUTF8): TFileName; override;
    /// the associated SQLite3 database connection
    // - setting the DB property will initialize the virtual table module
    // for this DB connection, calling the sqlite3_create_module_v2() function
    property DB: TSQLDataBase read fDB write SetDB;
  end;

  {{ define a Virtual Table module for a TSQLRestServerDB SQLite3 engine }
  TSQLVirtualTableModuleServerDB = class(TSQLVirtualTableModuleSQLite3)
  public
    /// register the Virtual Table to the database connection of a TSQLRestServerDB server
    constructor Create(aClass: TSQLVirtualTableClass;
      aServer: TSQLRestServer); override;
  end;

/// initialize a Virtual Table Module for a specified database
// - to be used for low-level access to a virtual module, e.g. with
// TSQLVirtualTableLog
// - when using our ORM, you should call TSQLModel.VirtualTableRegister()
// instead to associate a TSQLRecordVirtual class to a module
// - returns the created TSQLVirtualTableModule instance (which will be a
// TSQLVirtualTableModuleSQLite3 instance in fact)
function RegisterVirtualTableModule(aModule: TSQLVirtualTableClass; aDatabase: TSQLDataBase): TSQLVirtualTableModule;

/// set a TVarData into a SQlite3 result context
// - only handle varNull, varInt64, varDouble, varString (mapping a constant
// PUTF8Char), and varAny (BLOB with size = VLongs[0]) types in TVarData
// - therefore it's not a true Variant typecast, only a way of data sharing e.g.
// for the TSQLVirtualTableCursor.Column method
// - will call the corresponding sqlite3_result_*() function and return true,
// or will return false if the TVarData type is not handled
function VarDataToContext(Context: TSQLite3FunctionContext; const Res: TVarData): boolean;

/// set a SQLite3 value into a TVarData
// - only handle varNull, varInt64, varDouble, varString (mapping a constant
// PUTF8Char), and varAny (BLOB with size = VLongs[0]) types in TVarData
// - therefore it's not a true Variant typecast, only a way of data sharing e.g.
// for the TSQLVirtualTable.Insert or Update methods
// - will call the corresponding sqlite3_value_*() function to retrieve the
// data with the less overhead (e.g. memory allocation or copy) as possible
procedure VarDataFromValue(Value: TSQLite3Value; var Res: TVarData);


{ ************ Unit-Testing classes and functions }

type
  /// a parent test case which will test most functions, classes and types defined
  // and implemented in the SQLite3 unit, i.e. the SQLite3 engine itself
  // - it should not be called directly, but through TTestFileBased,
  // TTestMemoryBased and TTestMemoryBased children
  TTestSQLite3Engine = class(TSynTestCase)
  protected
    { these values are used internaly by the published methods below }
    TempFileName: TFileName;
    IsMemory: boolean;
    Demo: TSQLDataBase;
    Req: RawUTF8;
    JS: RawUTF8;
  public
    destructor Destroy; override;
  published
    /// test direct access to the SQLite3 engine
    // - i.e. via TSQLDataBase and TSQLRequest classes
    procedure DatabaseDirectAccess;
    /// test direct access to the Virtual Table features of SQLite3
    procedure VirtualTableDirectAccess;
    /// test the TSQLTableJSON table
    // - the JSON content generated must match the original data
    // - a VACCUM is performed, for testing some low-level SQLite3 engine
    // implementation
    // - the SortField feature is also tested
    procedure _TSQLTableJSON;
    /// test the TSQLRestClientDB, i.e. a local Client/Server driven usage
    // of the framework
    // - validates TSQLModel, TSQLRestServer and TSQLRestServerStatic by checking
    // the coherency of the data between client and server instances, after
    // update from both sides
    // - use all RESTful commands (GET/UDPATE/POST/DELETE...)
    // - test the 'many to many' features (i.e. TSQLRecordMany) and dynamic
    // arrays published properties handling
    // - also test FTS implementation if INCLUDE_FTS3 conditional is defined
    // - test dynamic tables
    procedure _TSQLRestClientDB;
  end;

  /// this test case will test most functions, classes and types defined and
  // implemented in the SQLite3 unit, i.e. the SQLite3 engine itself,
  // with a file-based approach
  TTestFileBased = class(TTestSQLite3Engine);

  /// this test case will test most functions, classes and types defined and
  // implemented in the SQLite3 unit, i.e. the SQLite3 engine itself,
  // with a memory-based approach
  // - this class will also test the TSQLRestServerStatic class, and its
  // 100% Delphi simple database engine
  TTestMemoryBased = class(TTestSQLite3Engine);

  /// this test case will test most functions, classes and types defined and
  // implemented in the SQLite3 unit, i.e. the SQLite3 engine itself,
  // with a file-based approach
  // - purpose of this class is to test Write-Ahead Logging for the database
  TTestFileBasedWAL = class(TTestFileBased);


implementation


{ TSQLTableDB }

constructor TSQLTableDB.Create(aDB: TSQLDataBase; const Tables: array of TClass;
  const aSQL: RawUTF8; Expand: boolean);
var JSONCached: RawUTF8;
    R: TSQLRequest;
    n: PtrInt;
begin
  JSONCached := aDB.LockJSON(aSQL,@n);
  if JSONCached='' then // not retrieved from cache -> call SQLite3 engine
    try // faster than sqlite3_get_table(): memory is allocated as a whole
      n := 0;
      JSONCached := R.ExecuteJSON(aDB.DB,aSQL,Expand,@n); // Expand=true for AJAX
      inherited Create(Tables,aSQL,JSONCached);
      Assert(n=RowCount);
    finally
      aDB.UnLockJSON(JSONCached,n);
    end
  else begin
    inherited Create(Tables,aSQL,JSONCached);
    Assert(n=RowCount);
  end;
end;


{ TSQLRestServerDB }

function TSQLRestServerDB.GetAndPrepareStatement(const SQL: RawUTF8): PSQLRequest;
var i, maxParam: integer;
    Types: TSQLFieldTypeArray;
    Values: TRawUTF8DynArray;
    GenericSQL: RawUTF8;
begin
  GenericSQL := ExtractInlineParameters(SQL,Types,Values,maxParam);
  if maxParam=0 then begin
    // SQL code with no valid :(...): internal parameters
    result := @fStaticStatement;
    result^.Prepare(DB.DB,SQL);
    DB.Log.Log(sllSQL,'% is no prepared statement',SQL,self);
    exit;
  end;
  DB.Log.Log(sllSQL,'% prepared as % with % param',[SQL,GenericSQL,maxParam],self);
  result := fStatementCache.Prepare(GenericSQL);
  // bind parameters
  assert(sqlite3_bind_parameter_count(result^.Request)=maxParam);
  for i := 0 to maxParam-1 do
  case Types[i] of
    sftDateTime, // date/time are stored as ISO-8601 TEXT in SQLite3
    sftUTF8Text: result^.Bind(i+1,Values[i]);
    sftBlob:     result^.Bind(i+1,pointer(Values[i]),length(Values[i]));
    sftInteger:  result^.Bind(i+1,GetInt64(pointer(Values[i])));
    sftFloat:    result^.Bind(i+1,GetExtended(pointer(Values[i])));
  end;
end;

function TSQLRestServerDB.EngineAdd(Table: TSQLRecordClass; const SentData: RawUTF8): integer;
var SQL: RawUTF8;
begin
  if (self=nil) or (Table=nil) then begin
    result := 0; // avoid GPF
    exit;
  end;
  SQL := 'INSERT INTO '+Table.RecordProps.SQLTableName;
  if trim(SentData)='' then
    SQL := SQL+' DEFAULT VALUES;' else
    SQL := SQL+GetJSONObjectAsSQL(SentData,false,true)+';';
  if EngineExecute(SQL) then begin
    result := DB.LastInsertRowID;
    if Assigned(OnUpdateEvent) then
       OnUpdateEvent(self,seAdd,Table,result);
  end else
    result := 0;
end;

procedure InternalRTreeIn(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef USEFASTCALL}cdecl;{$endif}
var aRTree: TSQLRecordRTreeClass;
    BlobA, BlobB: pointer;
begin
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  aRTree := sqlite3_user_data(Context);
  BlobA := sqlite3_value_blob(argv[0]);
  BlobB := sqlite3_value_blob(argv[1]);
  if (aRTree=nil) or (BlobA=nil) or (BlobB=nil) then
    sqlite3_result_error(Context,'invalid call') else
    sqlite3_result_int64(Context,byte(aRTree.ContainedIn(BlobA^,BlobB^)));
end;

constructor TSQLRestServerDB.Create(aModel: TSQLModel; aDB: TSQLDataBase;
  aHandleUserAuthentication: boolean);
begin
  fStatementCache.Init(aDB.DB);
  aDB.UseCache := true; // we better use caching in this JSON oriented use
  fDB := aDB;
  if fDB.InternalState=nil then begin // should be done once
    InternalState := 1;
    fDB.InternalState := @InternalState; // to update TSQLRestServerDB.InternalState
  end;
  inherited Create(aModel,aHandleUserAuthentication);
  InitializeEngine;
end;

procedure TSQLRestServerDB.InitializeEngine;
var i: integer;
    aModule: TSQLVirtualTableClass;
begin
  for i := 0 to high(Model.TableProps) do
    case Model.TableProps[i].Kind of
    rRTree: // register all *_in() SQL functions
      sqlite3_create_function_v2(DB.DB,
        pointer(TSQLRecordRTree(Model.Tables[i]).RTreeSQLFunctionName),
        2,SQLITE_ANY,Model.Tables[i],InternalRTreeIn,nil,nil,nil);
    rCustomForcedID, rCustomAutoID: begin
      aModule := Model.VirtualTableModule(Model.Tables[i]);
      if aModule<>nil then begin
        // perform registration via sqlite3_create_module_v2() API
        if fRegisteredVirtualTableModule=nil then
          SetLength(fRegisteredVirtualTableModule,length(Model.TableProps));
        fRegisteredVirtualTableModule[i] := TSQLVirtualTableModuleServerDB.Create(aModule,self);
      end;
    end;
    end;
end;

constructor TSQLRestServerDB.Create(aModel: TSQLModel; const aDBFileName: TFileName;
  aHandleUserAuthentication: boolean; const aPassword: RawUTF8);
begin
  fOwnedDB := TSQLDataBase.Create(aDBFileName,aPassword); // will be freed in Destroy
  Create(aModel,fOwnedDB,aHandleUserAuthentication);
end;

procedure TSQLRestServerDB.CreateMissingTables(user_version: cardinal);
var t,f,nt,nf: integer;
    TableNamesAtCreation, aFields: TRawUTF8DynArray;
    TableJustCreated: TSQLFieldTables;
    aSQL: RawUTF8;
begin
  if DB.TransactionActive then
    raise EBusinessLayerException.Create('CreateMissingTables: Transaction');
  fDB.GetTableNames(TableNamesAtCreation);
  nt := length(TableNamesAtCreation);
  QuickSortRawUTF8(TableNamesAtCreation,nt,nil,StrIComp);
  fillchar(TableJustCreated,sizeof(TSQLFieldTables),0);
  try
    // create not static and not existing tables
    for t := 0 to high(Model.Tables) do
      if ((fStaticData=nil) or (fStaticData[t]=nil)) then
      // this table is not static -> check if already existing, create if necessary
      with Model.TableProps[t] do
      if FastFindPUTF8CharSorted(pointer(TableNamesAtCreation),nt-1,pointer(SQLTableName),StrIComp)<0 then begin
        if not DB.TransactionActive then
          DB.TransactionBegin; // make initialization faster by using transaction
        DB.Execute(Model.GetSQLCreate(t)); // don't catch exception in constructor
        include(TableJustCreated,t);       // mark to be initialized below
      end else begin
        // this table is existing: check that all fields exist -> create if necessary
        DB.GetFieldNames(aFields,SQLTableName);
        nf := length(aFields);
        QuickSortRawUTF8(aFields,nf,nil,StrIComp);
        for f := 0 to high(Fields) do
          if FieldType[f] in COPIABLE_FIELDS then
          /// real database columns exist for Simple + Blob fields (not Many)
          if FastFindPUTF8CharSorted(pointer(aFields),nf-1,pointer(FieldsName[f]),StrIComp)<0 then begin
            aSQL := Model.GetSQLAddField(t,f);
            if aSQL<>'' then begin // need a true field with data
              if not DB.TransactionActive then
                DB.TransactionBegin; // make initialization faster by using transaction
              DB.Execute(aSQL);
            end;
            Model.Tables[t].InitializeTable(self,FieldsName[f]);
          end;
      end;
    if not DB.TransactionActive then
      exit;
    // database schema was modified -> update user version in SQLite3 file
    if user_version<>0 then
      DB.user_version := user_version;
    // initialize new tables AFTER creation of ALL tables
    if not IsZero(@TableJustCreated,sizeof(TSQLFieldTables)) then
      for t := 0 to high(Model.Tables) do
        if t in TableJustCreated then
          Model.Tables[t].InitializeTable(self,''); // '' for table creation
    DB.Commit;
  except
    on E: Exception do begin
      DB.RollBack; // will close any active Transaction
      raise;     // caller must handle exception
    end;
  end;
end;

function TSQLRestServerDB.EngineDelete(Table: TSQLRecordClass; ID: integer): boolean;
begin
  if (Table=nil) or (ID<=0) then begin
    result := false; // avoid GPF
    exit;
  end;
  if Assigned(OnUpdateEvent) then
     OnUpdateEvent(self,seDelete,Table,ID); // notify BEFORE deletion
  result := EngineExecuteFmt(
    'DELETE FROM % WHERE RowID=:(%):;',[Table.RecordProps.SQLTableName,ID]);
end;

function TSQLRestServerDB.EngineDeleteWhere(Table: TSQLRecordClass;
  const SQLWhere: RawUTF8; const IDs: TIntegerDynArray): boolean;
var i: integer;
begin
  if (Table=nil) or (SQLWhere='') or (IDs=nil) then begin
    result := false;
    exit;
  end;
  if Assigned(OnUpdateEvent) then
    for i := 0 to high(IDs) do
      OnUpdateEvent(self,seDelete,Table,IDs[i]); // notify BEFORE deletion
  result := EngineExecuteFmt(
    'DELETE FROM % WHERE %',[Table.RecordProps.SQLTableName,SQLWhere]);
end;

destructor TSQLRestServerDB.Destroy;
var i: integer;
begin
  try
    for i := 0 to high(fRegisteredVirtualTableModule) do
      if fRegisteredVirtualTableModule[i]<>nil then
        // to avoid GPF in TSQLVirtualTable.Destroy e.g.
        fRegisteredVirtualTableModule[i].fServer := nil;
    inherited Destroy;
  finally
    try
      fStatementCache.ReleaseAllDBStatements;
    finally
      fOwnedDB.Free;
    end;
  end;
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8;
  ValueInt: PInt64=nil; ValueUTF8: PRawUTF8=nil): boolean;
var Req: PSQLRequest;
begin
  if (self<>nil) and (DB<>nil) then 
  try
    DB.Lock(aSQL);
    try
      result := true;
      if IdemPChar(Pointer(aSQL),'VACUUM') then
        // VACUUM will fail if there are one or more active SQL statements
        fStatementCache.ReleaseAllDBStatements;
      Req := GetAndPrepareStatement(aSQL);
      if Req<>nil then
      with Req^ do
      try
        if (ValueInt=nil) and (ValueUTF8=nil) then
          // default execution: loop through all rows
          repeat until Step<>SQLITE_ROW else begin
          // get one row, and retrieve value
          if Step=SQLITE_ROW then
            if ValueInt<>nil then
              ValueInt^ := FieldInt(0) else
              ValueUTF8^ := FieldUTF8(0) else
            result := false;
          end;
      finally
        if Req=@fStaticStatement then
          Close;
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      DB.Log.Log(sllError,'% for %',[E,aSQL],self);
      {$else}
      LogToTextFile('TSQLRestServerDB.EngineExecute: '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end else
    result := false;
end;

function TSQLRestServerDB.EngineExecuteFmt(SQLFormat: PUTF8Char;
  const Args: array of const): boolean;
begin
  result := EngineExecute(FormatUTF8(SQLFormat,Args));
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8; out Value: Int64): boolean;
begin
  result:= EngineExecute(aSQL,@Value,nil);
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8; out Value: RawUTF8): boolean;
begin
  result:= EngineExecute(aSQL,nil,@Value);
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8;
  StoredProc: TOnSQLStoredProc): boolean;
var R: TSQLRequest; // we don't use fStatementCache[] here
    Res: integer;
begin
  result := false;
  if (self<>nil) and (DB<>nil) and (aSQL<>'') and Assigned(StoredProc) then
  try
    DB.Lock(''); // even if aSQL is SELECT, StoredProc may update data -> cache flush
    try
      try
        R.Prepare(DB.DB,aSQL);
        if R.FieldCount>0 then
        repeat
          res := R.Step;
          if res=SQLITE_ROW then
            StoredProc(R); // apply the stored procedure to all rows
        until res=SQLITE_DONE;
        result := true;
      finally
        R.Close; // always release statement
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      DB.Log.Log(sllError,'% for %',[E,aSQL],self);
      {$else}
      LogToTextFile('TSQLRestServerDB.EngineExecute Error: '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end;
end;

function TSQLRestServerDB.EngineExecuteAll(const aSQL: RawUTF8): boolean;
begin
  try
    if IdemPChar(Pointer(aSQL),'VACUUM') then
      // VACUUM will fail if there are one or more active SQL statements
      fStatementCache.ReleaseAllDBStatements;
    DB.ExecuteAll(aSQL); // Execute all statements (don't use fStatementCache[])
    result := true;
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      DB.Log.Log(sllError,'% for %',[E,aSQL],self);
      {$else}
      LogToTextFile('TSQLRestServerDB.EngineExecuteAll Error: '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end;
end;

function TSQLRestServerDB.EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false;
  ReturnedRowCount: PPtrInt=nil): RawUTF8;
var Req: PSQLRequest;
    MS: TRawByteStringStream;
    RowCount: integer;
begin
  result := '';
  RowCount := 0;
  if (self<>nil) and (DB<>nil) and (SQL<>'') then begin
    // need a SQL request for R.Execute() to prepare a statement
    result := DB.LockJSON(SQL,ReturnedRowCount); // lock and try from cache
    if result='' then // Execute request if was not got from cache
    try
      try
        Req := GetAndPrepareStatement(SQL);
        if Req<>nil then begin
          MS := TRawByteStringStream.Create;
          try
            try
              RowCount := Req^.Execute(0,'',MS,ForceAJAX or (not NoAJAXJSON));
              if ReturnedRowCount<>nil then
                ReturnedRowCount^ := RowCount;
              result := MS.DataString;
            finally
              if Req=@fStaticStatement then
                Req^.Close;
            end;
          finally
            MS.Free;
          end;
        end;
      except
        on ESQLite3Exception do
          result := '';
      end;
    finally
      DB.UnLockJSON(result,RowCount);
    end;
  end;
  if ReturnedRowCount<>nil then
    ReturnedRowCount^ := RowCount;
end;

function TSQLRestServerDB.EngineRetrieve(TableModelIndex, ID: integer): RawUTF8;
var SQL: RawUTF8;
begin
  if (self=nil) or (cardinal(TableModelIndex)>=cardinal(length(Model.TableProps))) then
    exit;
  with Model.TableProps[TableModelIndex] do
    SQL := FormatUTF8('SELECT % FROM % WHERE RowID=:(%):;',
      [SQLTableSimpleFields[true,false],SQLTableName,ID]);
  result := EngineList(SQL,true); // ForceAJAX=true -> '[{...}]'#10
  if result<>'' then
    if IsNotAjaxJSON(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := '' else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result,2,length(result)-3);
end;

function TSQLRestServerDB.EngineRetrieveBlob(Table: TSQLRecordClass;
  aID: integer; BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean;
var SQL: RawUTF8;
    Req: PSQLRequest;
begin
  result := false;
  if (self=nil) or (DB=nil) or (aID<=0) or (Table=nil) or not BlobField^.IsBlob then
    exit;
  try
    SQL := FormatUTF8('SELECT % FROM % WHERE RowID=?;',
      [BlobField^.Name,Table.RecordProps.SQLTableName]);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      Req := fStatementCache.Prepare(SQL);
      with Req^ do
      try
        Bind(1,aID);
        if (FieldCount=1) and (Step=SQLITE_ROW) then begin
          BlobData := FieldBlob(0);
          result := true;
        end;
      finally
        if Req=@fStaticStatement then
          Close;
      end;
    finally
      DB.UnLock;
    end;
  except
    on ESQLite3Exception do
      result := false;
  end;
end;

procedure TSQLRestServerDB.SetNoAJAXJSON(const Value: boolean);
begin
  inherited;
  if Value=NoAJAXJSON then exit;
  fDB.Cache.Reset; // we changed the JSON format -> cache must be updated
end;

function TSQLRestServerDB.EngineUpdate(Table: TSQLRecordClass; ID: integer;
  const SentData: RawUTF8): boolean;
begin
  if (self=nil) or (Table=nil) or (ID<=0) then
    result := false else begin
    // this SQL statement use :(inlined params): for all values
    result := EngineExecuteFmt('UPDATE % SET % WHERE RowID=:(%):;',
      [Table.RecordProps.SQLTableName,GetJSONObjectAsSQL(SentData,true,true),ID]);
    if Assigned(OnUpdateEvent) then
       OnUpdateEvent(self,seUpdate,Table,ID);
  end;
end;

function TSQLRestServerDB.EngineUpdateBlob(Table: TSQLRecordClass;
  aID: integer; BlobField: PPropInfo;
  const BlobData: TSQLRawBlob): boolean;
var SQL: RawUTF8;
begin
  result := false;
  if (self=nil) or (DB=nil) or (aID<=0) or (Table=nil) or not BlobField^.IsBlob then
    exit;
  try
    SQL := FormatUTF8('UPDATE % SET %=? WHERE RowID=?;',
             [Table.RecordProps.SQLTableName,BlobField^.Name]);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      with fStatementCache.Prepare(SQL)^ do begin
        Bind(1,pointer(BlobData),length(BlobData));
        Bind(2,aID);
        repeat
        until Step<>SQLITE_ROW; // Execute all steps of the first statement
        result := true;
      end;
    finally
      DB.UnLock;
    end;
  except
    on ESQLite3Exception do
      result := false;
  end;
end;

function TSQLRestServerDB.EngineUpdateField(Table: TSQLRecordClass;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var Static: TSQLRestServerStatic;
    WhereID: integer;
begin
  result := false;
  if (self=nil) or (Table=nil) then
    exit;
  Static := GetStaticDataServerOrVirtualTable(Table);
  if Static<>nil then
    result := Static.EngineUpdateField(Table,SetFieldName,SetValue,WhereFieldName,WhereValue) else
    with Table.RecordProps do 
    if FieldIndexFromRawUTF8(SetFieldName)>=0 then begin
      WhereID := 0;
      if IsRowID(pointer(WhereFieldName)) then begin
        WhereID := GetInteger(Pointer(WhereValue));
        if (WhereID<=0) or not RecordCanBeUpdated(Table,WhereID,seUpdate) then
          exit; // limitation: will only check for update from RowID
      end else
        if FieldIndexFromRawUTF8(WhereFieldName)<0 then
          exit;
      result := EngineExecuteFmt('UPDATE % SET %=:(%): WHERE %=:(%):',
        [SQLTableName,SetFieldName,SetValue,WhereFieldName,WhereValue]);
      if (WhereID>0) and Assigned(OnUpdateEvent) then
        OnUpdateEvent(self,seUpdate,Table,WhereID);
    end;
end;

procedure TSQLRestServerDB.Commit(SessionID: cardinal=1);
begin
  inherited Commit(SessionID); // reset fTransactionActive + write all TSQLVirtualTableJSON
  try
    DB.Commit;
  except
    on ESQLite3Exception do
      ; // just catch exception
  end;
end;

procedure TSQLRestServerDB.RollBack(SessionID: cardinal=1);
begin
  inherited; // reset TSQLRestServerDB.fTransactionActive flag
  try
    DB.RollBack; // reset TSQLDataBase.RollBack
  except
    on ESQLite3Exception do
      ; // just catch exception
  end;
end;

function TSQLRestServerDB.TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean;
begin
  result := inherited TransactionBegin(aTable,SessionID);
  if result then
    // fTransactionActive flag was not already set
    try
      DB.TransactionBegin;
    except
      on ESQLite3Exception do
        result := false;
    end;
end;

function TSQLRestServerDB.Backup(Dest: TStream): boolean;
var Source: TFileStream;
    Closed: boolean;
begin
  result := false;
  if (Self=nil) or (DB=nil) then
    exit;
  DB.Lock(''); // flush internal cache
  try
  try
    // perform a VACCUM to recreate the database content
    EngineExecuteAll('VACUUM');
    Closed := false;
    try
      Closed := sqlite3_close(DB.DB)=SQLITE_OK;
      // compress the database content file
      Source := TFileStream.Create(DB.FileName,fmOpenRead or fmShareDenyNone);
      try
        Dest.CopyFrom(Source,0);  // Count=0 for whole stream copy
        result := true;
      finally
        Source.Free;
      end;
    finally
      if Closed then begin
        DB.DBOpen; // reopen the database if was previously closed
        InitializeEngine; // register functions and modules
      end;
    end;
  finally
    DB.UnLock;
  end;
  except
    on E: Exception do
      result := false;
  end;
end;

function TSQLRestServerDB.BackupGZ(const DestFileName: TFileName; CompressionLevel: integer): boolean;
var D,Z: TStream;
begin
  try
    D := TFileStream.Create(DestFileName,fmCreate);
    try
      Z := TSynZipCompressor.Create(D,CompressionLevel,szcfGZ);
      try
        result := Backup(Z);
      finally
        Z.Free;
      end;
    finally
      D.Free;
    end;
  except
    result := false;
  end;
end;

function TSQLRestServerDB.Restore(const ContentToRestore: RawByteString): boolean;
var BackupFileName: TFileName;
begin
  result := false;
  if (Self=nil) or (DB=nil) or
     not IdemPChar(pointer(ContentToRestore),'SQLITE FORMAT 3') then
    exit; // invalid restore content
  DB.Lock(''); // flush internal cache
  try
    try
      fStatementCache.ReleaseAllDBStatements;
      if sqlite3_close(DB.DB)<>SQLITE_OK then
        exit; // impossible to close DB.FileName (some statement may be opened)
      BackupFileName := ChangeFileExt(DB.FileName,'.bak');
      DeleteFile(BackupFileName);
      try
        if MoveFile(pointer(DB.FileName),pointer(BackupFileName)) then
          result := FileFromString(ContentToRestore,DB.FileName,true) and
             (StringFromFile(DB.FileName)=ContentToRestore) and
             (DB.DBOpen=SQLITE_OK);
      finally
        if result then
          DeleteFile(BackupFileName) else begin
          // on error, restore previous db file
          DeleteFile(DB.FileName);
          MoveFile(pointer(BackupFileName),pointer(DB.FileName));
          DB.DBOpen; // always reopen the database
        end;
        InitializeEngine; // register functions and modules
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: Exception do
      result := false;
  end;
end;

procedure TSQLRestServerDB.FlushInternalDBCache;
begin
  inherited;
  if DB<>nil then
    DB.CacheFlush;
end;


{ TSQLRestClientDB }

constructor TSQLRestClientDB.Create(aClientModel, aServerModel: TSQLModel; aDB: TSQLDataBase;
   aServerClass: TSQLRestServerDBClass; aHandleUserAuthentication: boolean);
begin
  aDB.UseCache := true;      // we better use caching in this JSON oriented use
  inherited Create(aClientModel);
  if aServerModel=nil then
    aServerModel := TSQLModel.Create(aClientModel); // clone from client
  // next line will create aModel tables if necessary
  fServer := aServerClass.Create(aServerModel,aDB,aHandleUserAuthentication);
  fServer.NoAJAXJSON := true; // use smaller JSON size in this local use (never AJAX)
end;

constructor TSQLRestClientDB.Create(aClientModel, aServerModel: TSQLModel; const aDBFileName: TFileName;
  aServerClass: TSQLRestServerDBClass; aHandleUserAuthentication: boolean; const aPassword: RawUTF8);
begin
  fOwnedDB := TSQLDatabase.Create(aDBFileName,aPassword);
  Create(aClientModel, aServerModel,fOwnedDB,aServerClass,aHandleUserAuthentication);
end;

destructor TSQLRestClientDB.Destroy;
var M: TSQLModel;
begin
  try
    SessionClose;
  finally
    M := fServer.Model;
    if M.Owner<>nil then
      M := nil; // free associated model if it's owned by nobody
    try
      FreeAndNil(fServer);
    finally
      M.Free;
      fOwnedDB.Free;
      inherited;
    end;
  end;
end;

function TSQLRestClientDB.getDB: TSQLDataBase;
begin
  result := fServer.DB;
end;

function TSQLRestClientDB.List(const Tables: array of TSQLRecordClass;
  const SQLSelect, SQLWhere: RawUTF8): TSQLTableJSON;
var aSQL: RawUTF8;
    n: integer;
begin
  result := nil;
  n := length(Tables);
  if (self<>nil) and (n>0) then
  try // will use JSON cache if available:
    aSQL := SQLFromSelectWhere(Tables,SQLSelect,SQLWhere);
    if n=1 then
      // InternalListJSON will handle both static and DB tables
      result := fServer.InternalListJSON(TSQLRecordClass(Tables[0]),aSQL) else
      // we access localy the DB -> TSQLTableDB handle Tables parameter
      result := TSQLTableDB.Create(fServer.DB,
        RecordClassesToClasses(Tables),aSQL,not fServer.NoAJAXJSON);
    if fServer.DB.InternalState<>nil then
      result.InternalState := fServer.DB.InternalState^;
  except
    on ESQLite3Exception do
      result := nil;
  end;
end;

function TSQLRestClientDB.InternalURI(const url, method: RawUTF8;
  Resp, Head, SendData: PRawUTF8): Int64Rec;
var R,H,S: RawUTF8; // temp '' string to be used when no PString is provided
begin
  if Resp=nil then Resp := @R;
  if Head=nil then Head := @H;
  if SendData=nil then SendData := @S;
  result := fServer.URI(url,method,SendData^,Resp^,Head^,@FULL_ACCESS_RIGHTS);
  if (result.Hi=0) and (fServer.DB.InternalState<>nil) then
    result.Hi := fServer.DB.InternalState^; // manual update if necessary
end;

{ TSQLVirtualTableModuleSQLite3 }

function VarDataToContext(Context: TSQLite3FunctionContext; const Res: TVarData): boolean;
begin
  case Res.VType of
    varNull:   sqlite3_result_null(Context);
    varInt64:  sqlite3_result_int64(Context,Res.VInt64);
    varDouble: sqlite3_result_double(Context,Res.VDouble);
    varString: sqlite3_result_text(Context,Res.VString);
    varAny:    sqlite3_result_blob(Context,Res.VAny,Res.VLongs[0]);
    else begin
      result := false; // not handled type
      exit;
    end;
  end;
  result := true;
end;

procedure VarDataFromValue(Value: TSQLite3Value; var Res: TVarData);
begin
  case sqlite3_value_type(Value) of
  SQLITE_NULL:
    Res.VType := varNull;
  SQLITE_INTEGER: begin
    Res.VType := varInt64;
    Res.VInt64 := sqlite3_value_int64(Value);
  end;
  SQLITE_FLOAT: begin
    Res.VType := varDouble;
    Res.VDouble := sqlite3_value_double(Value);
  end;
  SQLITE_TEXT:  begin
    Res.VType := varString;
    Res.VString := sqlite3_value_text(Value);
  end;
  SQLITE_BLOB: begin
    Res.VType := varAny;
    Res.VLongs[0] := sqlite3_value_bytes(Value);
    Res.VAny := sqlite3_value_blob(Value);
  end;
  else
    Res.VType := varUnknown;
  end;
end;

function TSQLVirtualTableModuleSQLite3.FileName(const aTableName: RawUTF8): TFileName;
begin
  if FilePath<>'' then
    // if a file path is specified (e.g. by SynDBExplorer) -> always use this
    result := inherited FileName(aTableName) else
  if SameText(DB.FileName,':memory:') then
    // in-memory databases virtual tables should remain in memory
    result := '' else
    // change file path to current DB folder
    result := ExtractFilePath(DB.FileName)+ExtractFileName(inherited FileName(aTableName));
end;

procedure ExceptionToSqlite3Err(E: Exception; var pzErr: PAnsiChar);
var U: RawUTF8;
begin
  U := StringToUTF8(E.Message);
  pzErr := sqlite3_malloc(length(U));
  move(pointer(U)^,pzErr^,length(U));
end;

function vt_Create(DB: TSQLite3DB; pAux: Pointer;
  argc: Integer; const argv: PPUTF8CharArray;
  var ppVTab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
var Module: TSQLVirtualTableModuleSQLite3 absolute pAux;
    Table: TSQLVirtualTable;
begin
  if (Module=nil) or (Module.DB.DB<>DB) or
     (StrIComp(pointer(Module.ModuleName),argv[0])<>0) then begin
    result := SQLITE_ERROR;
    exit;
  end;
  ppVTab := sqlite3_malloc(sizeof(TSQLite3VTab));
  if ppVTab=nil then begin
    result := SQLITE_NOMEM;
    exit;
  end;
  ppVTab^.zErrMsg := nil; // to avoid GPF
  try
    Table := Module.TableClass.Create(Module,RawUTF8(argv[2]),argc-3,@argv[3]);
  except
    on E: Exception do begin
      ExceptionToSqlite3Err(E,pzErr);
      sqlite3_free(ppVTab);
      result := SQLITE_ERROR;
      exit;
    end;
  end;
  result := sqlite3_declare_vtab(DB,pointer(Table.Structure));
  if result<>SQLITE_OK then begin
    Table.Free;
    sqlite3_free(ppVTab);
    result := SQLITE_ERROR;
  end else
    ppVTab^.pInstance := Table;
end;

function vt_Disconnect(pVTab: PSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  TSQLVirtualTable(pvTab^.pInstance).Free; 
  sqlite3_free(pVTab);
  result := SQLITE_OK;
end;

function vt_Destroy(pVTab: PSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab^.pInstance).Drop then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
  vt_Disconnect(pVTab); // release memory
end;

function vt_BestIndex(var pVTab: TSQLite3VTab; var pInfo: TSQLite3IndexInfo): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif}
var Prepared: PSQLVirtualTablePrepared;
    i, n: Integer;
begin
  result := SQLITE_ERROR;
  if (cardinal(pInfo.nOrderBy)>MAX_SQLFIELDS) or
     (cardinal(pInfo.nConstraint)>MAX_SQLFIELDS) then
    exit; // avoid buffer overflow
  Prepared := sqlite3_malloc(sizeof(TSQLVirtualTablePrepared));
  try
    // encode the incoming parameters into Prepared^ record
    fillchar(Prepared^,sizeof(Prepared^),0);
    Prepared^.WhereCount := pInfo.nConstraint;
    for i := 0 to pInfo.nConstraint-1 do
    with Prepared^.Where[i], pInfo.aConstraint^[i] do
    if usable then begin
      Column := iColumn;
      case op of
        SQLITE_INDEX_CONSTRAINT_EQ:    Operator := soEqualTo;
        SQLITE_INDEX_CONSTRAINT_GT:    Operator := soGreaterThan;
        SQLITE_INDEX_CONSTRAINT_LE:    Operator := soLessThanOrEqualTo;
        SQLITE_INDEX_CONSTRAINT_LT:    Operator := soLessThan;
        SQLITE_INDEX_CONSTRAINT_GE:    Operator := soGreaterThanOrEqualTo;
        SQLITE_INDEX_CONSTRAINT_MATCH: Operator := soBeginWith;
        else exit; // invalid parameter
      end;
    end else
      Column := VIRTUAL_TABLE_IGNORE_COLUMN;
    assert(sizeof(TSQLVirtualTablePreparedOrderBy)=sizeof(TSQLite3IndexOrderBy));
    if pInfo.nOrderBy>0 then begin
      Prepared^.OrderByCount := pInfo.nOrderBy;
      Move(pInfo.aOrderBy^[0],Prepared^.OrderBy[0],pInfo.nOrderBy*sizeof(Prepared^.OrderBy[0]));
    end;
    // perform the index query
    if not TSQLVirtualTable(pvTab.pInstance).Prepare(Prepared^) then
      exit;
    // update pInfo and store Prepared into pInfo.idxStr for vt_Filter()
    n := 0;
    for i := 0 to pInfo.nConstraint-1 do
    if Prepared^.Where[i].Value.VType<>varEmpty then begin
      if i<>n then // expression needed for Search() method to be moved at [n]
        move(Prepared^.Where[i],Prepared^.Where[n],sizeof(Prepared^.Where[i]));
      inc(n);
      pInfo.aConstraintUsage[i].argvIndex := n;
      pInfo.aConstraintUsage[i].omit := Prepared^.Where[i].OmitCheck;
    end;
    Prepared^.WhereCount := n; // will match argc in vt_Filter()
    pInfo.orderByConsumed := integer(Prepared^.OmitOrderBy);
    pInfo.estimatedCost := Prepared^.EstimatedCost;
    pInfo.idxStr := pointer(Prepared);
    pInfo.needToFreeIdxStr := 1; // will do sqlite3_free(idxStr) when needed
    result := SQLITE_OK;
  finally
    if result<>SQLITE_OK then
      sqlite3_free(Prepared); // avoid memory leak on error
  end;
end;

function vt_Filter(var pVtabCursor: TSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar;
   argc: Integer; var argv: TSQLite3ValueArray): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif}
var Prepared: PSQLVirtualTablePrepared absolute idxStr; // idxNum is not used
    i: integer;
begin
  result := SQLITE_ERROR;
  if Prepared^.WhereCount<>argc then
    exit; // invalid prepared array
  for i := 0 to argc-1 do
    VarDataFromValue(argv[i],Prepared^.Where[i].Value);
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).Search(Prepared^) then
    result := SQLITE_OK;
end;

function vt_Open(var pVTab: TSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif}
var Table: TSQLVirtualTable;
begin
  ppCursor := sqlite3_malloc(sizeof(TSQLite3VTabCursor));
  if ppCursor=nil then begin
    result := SQLITE_NOMEM;
    exit;
  end;
  Table := TSQLVirtualTable(pvTab.pInstance);
  if (Table=nil) or (Table.Module=nil) or (Table.Module.CursorClass=nil) then begin
    sqlite3_free(ppCursor);
    result := SQLITE_ERROR;
    exit;
  end;
  ppCursor.pInstance := Table.Module.CursorClass.Create(Table);
  result := SQLITE_OK;
end;

function vt_Close(pVtabCursor: PSQLite3VTabCursor): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  TSQLVirtualTableCursor(pVtabCursor^.pInstance).Free;
  sqlite3_free(pVtabCursor);
  result := SQLITE_OK;
end;

function vt_next(var pVtabCursor: TSQLite3VTabCursor): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).Next then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Eof(var pVtabCursor: TSQLite3VTabCursor): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  result := integer(not TSQLVirtualTableCursor(pVtabCursor.pInstance).HasData);
end;

function vt_Column(var pVtabCursor: TSQLite3VTabCursor; sContext: TSQLite3FunctionContext;
  N: Integer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
var Res: TVarData;
begin
  if (N>=0) and TSQLVirtualTableCursor(pVtabCursor.pInstance).Column(N,Res) and
     VarDataToContext(sContext,Res) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Rowid(var pVtabCursor: TSQLite3VTabCursor; var pRowid: Int64): Integer;
      {$ifndef USEFASTCALL}cdecl;{$endif}
var Res: TVarData;
begin
  result := SQLITE_ERROR;
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).Column(-1,Res) then begin
    case Res.VType of
    varInt64:    pRowID := Res.VInt64;
    varCurrency, varDouble: pRowID := trunc(Res.VDouble);
    varString: pRowID := GetInt64(Res.VPointer);
    else exit;
    end;
    result := SQLITE_OK;
  end;
end;

function vt_Update(var pVTab: TSQLite3VTab;
  nArg: Integer; var ppArg: TSQLite3ValueArray;
  var pRowid: Int64): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
var Values: TVarDataDynArray;
    Table: TSQLVirtualTable;
    RowID0, RowID1: Int64;
    i: integer;
    OK: boolean;
begin // call Delete/Insert/Update methods according to supplied parameters
  Table := TSQLVirtualTable(pvTab.pInstance);
  result := SQLITE_ERROR;
  if nArg<=0 then
    exit;
  case sqlite3_value_type(ppArg[0]) of
    SQLITE_INTEGER: RowID0 := sqlite3_value_int64(ppArg[0]);
    SQLITE_NULL:    RowID0 := 0;
    else exit; // invalid call
  end;
  if nArg=1 then
    OK := Table.Delete(RowID0) else begin
    case sqlite3_value_type(ppArg[1]) of
      SQLITE_INTEGER: RowID1 := sqlite3_value_int64(ppArg[1]);
      SQLITE_NULL:    RowID1 := 0;
      else exit; // invalid call
    end;
    SetLength(Values,nArg-2);
    for i := 0 to nArg-3 do
      VarDataFromValue(ppArg[i+2],Values[i]);
    if RowID0=0 then
      OK := Table.Insert(RowID1,Values,pRowid) else
      OK := Table.Update(RowID0,RowID1,Values);
  end;
  if OK then
    result := SQLITE_OK;
end;

function vt_Begin(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(vttBegin,0) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Commit(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(vttCommit,0) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_RollBack(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(vttRollBack,0) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Sync(var pVTab: TSQLite3VTab): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(vttSync,0) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_SavePoint(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(vttSavePoint,iSavePoint) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Release(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(vttRelease,iSavePoint) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_RollBackTo(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(vttRollBackTo,iSavePoint) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Rename(var pVTab: TSQLite3VTab; const zNew: PAnsiChar): Integer;
  {$ifndef USEFASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Rename(RawUTF8(zNew)) then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

procedure TSQLVirtualTableModuleSQLite3.SetDB(aDB: TSQLDataBase);
begin
  fDB := aDB;
  fillchar(fModule,sizeof(fModule),0);
  fModule.iVersion := 1;
  fModule.xCreate := vt_Create;
  fModule.xConnect := vt_Create;
  fModule.xBestIndex := vt_BestIndex;
  fModule.xDisconnect := vt_Disconnect;
  fModule.xDestroy := vt_Destroy;
  fModule.xOpen := vt_Open;
  fModule.xClose := vt_Close;
  fModule.xFilter := vt_Filter;
  fModule.xNext := vt_Next;
  fModule.xEof := vt_Eof;
  fModule.xColumn := vt_Column;
  fModule.xRowid := vt_Rowid;
  if vtWrite in Features then begin
    fModule.xUpdate := vt_Update;
    if vtTransaction in Features then begin
      fModule.xBegin := vt_Begin;
      fModule.xSync := vt_Sync;
      fModule.xCommit := vt_Commit;
      fModule.xRollback := vt_RollBack;
    end;
    if vtSavePoint in Features then begin
      fModule.iVersion := 2;
      fModule.xSavePoint := vt_SavePoint;
      fModule.xRelease := vt_Release;
      fModule.xRollBackTo := vt_RollBackTo;
    end;
    fModule.xRename := vt_Rename;
  end;
  sqlite3_check(fDB.DB,sqlite3_create_module_v2(fDB.DB,pointer(fModuleName),fModule,
     self,sqlite3InternalFreeObject));
end;


{ TSQLVirtualTableModuleServerDB }

constructor TSQLVirtualTableModuleServerDB.Create(
  aClass: TSQLVirtualTableClass; aServer: TSQLRestServer);
begin
  if not aServer.InheritsFrom(TSQLRestServerDB) then
    raise EBusinessLayerException.CreateFmt('%.Create expects a DB Server',[ClassName]);
  inherited Create(aClass,aServer);
  DB := TSQLRestServerDB(aServer).DB; // SetDB setter will do the work
end;


{ ************ Unit-Testing classes and functions }

{ TTestSQLite3Engine }

destructor TTestSQLite3Engine.Destroy;
begin
  Demo.Free;
  inherited;
end;

procedure InternalSQLFunctionCharIndex(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var StartPos: integer;
begin
  case argc of
  2: StartPos := 1;
  3: begin
    StartPos := sqlite3_value_int64(argv[2]);
    if StartPos<=0 then
      StartPos := 1;
  end;
  else begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  end;
  if (sqlite3_value_type(argv[0])=SQLITE_NULL) or
     (sqlite3_value_type(argv[1])=SQLITE_NULL) then
    sqlite3_result_int64(Context,0) else
    sqlite3_result_int64(Context,SynCommons.PosEx(
      sqlite3_value_text(argv[0]),sqlite3_value_text(argv[1]),StartPos));
end;

{$ifdef UNICODE}
{$WARNINGS OFF} // don't care about implicit string cast in tests
{$endif}

const // BLOBs are stored as array of byte to avoid any charset conflict
  BlobDali: array[0..3] of byte = (97,233,224,231);
  BlobMonet: array[0..13] of byte = (224,233,231,ord('d'),ord('s'),ord('j'),
        ord('d'),ord('s'),ord('B'),ord('L'),ord('O'),ord('B'),ord('2'),ord('3'));

procedure TTestSQLite3Engine.DatabaseDirectAccess;
procedure InsertData(n: integer);
var i: integer;
    s: string;
    ins: RawUTF8;
    R: TSQLRequest;
begin
  // this code is a lot faster than sqlite3 itself, even if it use Utf8 encoding:
  // -> we test the engine speed, not the test routines speed :)
 ins := 'INSERT INTO People (FirstName,LastName,Data,YearOfBirth,YearOfDeath) VALUES (''';
 for i := 1 to n do begin
   str(i,s);
   // we put some accents in order to test UTF-8 encoding
   R.Prepare(Demo.DB,ins+'Salvador'+RawUTF8(s)+''', ''Dali'', ?, 1904, 1989);');
   R.Bind(1,@BlobDali,4); // Bind Blob
   R.Execute;
   Demo.Execute(ins+StringToUtf8('Samuel Finley Breese'+s+''', ''Morse'', ''a'+chr(233)+chr(224)+chr(231)+''', 1791, 1872);'));
   Demo.Execute(ins+StringToUtf8('Sergei'+s+''', ''Rachmaninoff'', '''+chr(233)+'z'+chr(231)+'b'', 1873, 1943);'));
   Demo.Execute(ins+StringToUtf8('Alexandre'+s+''', ''Dumas'', '''+chr(233)+chr(231)+'b'', 1802, 1870);'));
   Demo.Execute(ins+StringToUtf8('Franz'+s+''', ''Schubert'', '''+chr(233)+chr(224)+chr(231)+'a'', 1797, 1828);'));
   Demo.Execute(ins+StringToUtf8('Leonardo'+s+''', ''da Vin'+chr(231)+'i'', ''@'+chr(231)+'b'', 1452, 1519);'));
   Demo.Execute(ins+StringToUtf8('Aldous Leonard'+s+''', ''Huxley'', '''+chr(233)+chr(224)+''', 1894, 1963);'));
   R.Prepare(Demo.DB,ins+StringToUtf8('Claud'+chr(232)+s+#10#7''', ''M'+chr(244)+'net'', ?, 1840, 1926);'));
   R.Bind(1,@BlobMonet,sizeof(BlobMonet)); // Bind Blob
   R.Execute;
   Demo.Execute(ins+StringToUtf8('Albert'+s+''', ''Einstein'', '''+chr(233)+chr(231)+'p'', 1879, 1955);'));
   Demo.Execute(ins+StringToUtf8('Johannes'+s+''', ''Gutenberg'', '''+chr(234)+'mls'', 1400, 1468);'));
   Demo.Execute(ins+StringToUtf8('Jane'+s+''', ''Aust'+chr(232)+'n'', '''+chr(231)+chr(224)+chr(231)+'m'', 1775, 1817);'));
 end;
end;
const
  ReqAnsi: WinAnsiString =
    'SELECT * FROM People WHERE LastName=''M'+chr(244)+'net'' ORDER BY FirstName;';
  SoundexValues: array[0..5] of RawUTF8 =
    ('bonjour','bonchour','Bnjr','mohammad','mohhhammeeet','bonjourtr'+chr(232)+'slongmotquid'+chr(233)+'passe');
var Names: TRawUTF8DynArray;
    i1,i2: integer;
    Res: Int64;
    s: RawUTF8;
begin
  if PosEx(RawUTF8('SQlite3 engine'),Owner.CustomVersions,1)=0 then
    Owner.CustomVersions := Owner.CustomVersions+#13#10'SQlite3 engine used: '+
      sqlite3_libversion;
  IsMemory := InheritsFrom(TTestMemoryBased);
  if IsMemory then
    TempFileName := ':memory:' else begin
    TempFileName := 'test.db3';
    DeleteFile(TempFileName); // use a temporary file
    CreateSQLEncryptTable('password1');
  end;
  Demo := TSQLDataBase.Create(TempFileName);
  Demo.Synchronous := smOff;
  for i1 := 0 to 100 do
    for i2 := 1 to 100 do begin
      s := FormatUTF8('SELECT MOD(%,%);',[i1,i2]);
      Demo.Execute(s,res);
      Check(res=i1 mod i2,s);
    end;
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT SoundEx("%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=SoundExUTF8(pointer(SoundexValues[i1])),s);
  end;
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT SoundExFr("%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=SoundExUTF8(pointer(SoundexValues[i1]),nil,sndxFrench),s);
  end;
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT SoundExEs("%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=SoundExUTF8(pointer(SoundexValues[i1]),nil,sndxSpanish),s);
  end;
  Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,2,'CharIndex');
  Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,3,'CharIndex');
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT CharIndex("o","%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=PosEx('o',SoundexValues[i1]),s);
    s := FormatUTF8('SELECT CharIndex("o","%",5);',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=PosEx('o',SoundexValues[i1],5),s);
  end;
  Demo.UseCache := true; // use the cache for the JSON requests
  Demo.WALMode := InheritsFrom(TTestFileBasedWAL); // test Write-Ahead Logging
  Check(Demo.WALMode=InheritsFrom(TTestFileBasedWAL));
  Demo.Execute(
    ' CREATE TABLE IF NOT EXISTS People (' +
      ' ID INTEGER PRIMARY KEY,'+
      ' FirstName TEXT COLLATE SYSTEMNOCASE,' +
      ' LastName TEXT,' +
      ' Data BLOB,'+
      ' YearOfBirth INTEGER,' +
      ' YearOfDeath INTEGER); ');
  // Inserting data 1x without transaction ');
  InsertData(1);
  { Insert some sample data - now with transaction. Multiple records are
    inserted and not yet commited until the transaction is finally ended.
    This single transaction is very fast compared to multiple individual
    transactions. It is even faster than other database engines. }
  Demo.TransactionBegin;
  InsertData(1000);
  Demo.Commit;
  Req := WinAnsiToUtf8(ReqAnsi);
  Check(Utf8ToWinAnsi(Req)=ReqAnsi,'WinAnsiToUtf8/Utf8ToWinAnsi');
  JS := Demo.ExecuteJSON(Req); // get result in JSON format
  FileFromString(JS,'Test1.json');
  Check(Hash32(JS)=$2D6A1467,'Expected ExecuteJSON result not retrieved');
  if not IsMemory then begin // check file encryption password change
    FreeAndNil(Demo); // if any exception occurs in Create(), Demo.Free is OK
    ChangeSQLEncryptTablePassWord(TempFileName,'password1','');
    ChangeSQLEncryptTablePassWord(TempFileName,'','NewPass');
    CreateSQLEncryptTable('NewPass');
    Demo := TSQLDataBase.Create(TempFileName); // reuse the temporary file
    Demo.Synchronous := smOff;
    Demo.UseCache := true; // use the cache for the JSON requests
    Demo.WALMode := InheritsFrom(TTestFileBasedWAL); // test Write-Ahead Logging
    Check(Demo.WALMode=InheritsFrom(TTestFileBasedWAL));
    Check(Hash32(Demo.ExecuteJSON(Req))=$2D6A1467,'ExecuteJSON crypted');
  end;
  Demo.GetTableNames(Names);
  Check(length(Names)=1);
  Check(Names[0]='People');
  Demo.Execute('SELECT Concat(FirstName," and ") FROM People WHERE LastName="Einstein"',s);
  Check(Hash32(s)=$68A74D8E,'Albert1 and Albert1 and Albert2 and Albert3 and ...');
end;

procedure TTestSQLite3Engine.VirtualTableDirectAccess;
const
  LOG1: RawUTF8 = 'D:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.4 (2011-04-07)'#13#10+
    'Host=MyPC User=MySelf CPU=2*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545'#13#10+
    'TSynLog 1.13 LVCL 2011-04-07 12:04:09'#13#10#13#10+
    '20110407 12040904 debug {"TObjectList(00AF8D00)":["TObjectList(00AF8D20)",'+
    '"TObjectList(00AF8D60)","TFileVersion(00ADC0B0)","TSynMapFile(00ACC990)"]}';
var Res: Int64;
    s,s2,s3: RawUTF8;
    n: PtrInt;
begin
  // register the Log virtual table module to this connection
  RegisterVirtualTableModule(TSQLVirtualTableLog,Demo);
  // test Log virtual table module
  FileFromString(LOG1,'temptest.log');
  Demo.Execute('CREATE VIRTUAL TABLE test USING log(temptest.log);');
  Demo.Execute('select count(*) from test',Res);
  Check(Res=1);
  n := 0;
  s := Demo.ExecuteJSON('select * from test',False,@n);
  Check(s<>'');
  Check(n=Res);
  s2 := Demo.ExecuteJSON('select * from test where rowid=2',False,@n);
  Check(s2='{"fieldCount":3,"values":["DateTime","Level","Content"]}'#$A);
  Check(n=0);
  s2 := Demo.ExecuteJSON('select * from test where rowid=1',False,@n);
  Check(s2<>'');
  Check(s=s2);
  Check(n=1);
  n := 0;
  s3 := Demo.ExecuteJSON('select * from test where level=2',False,@n);
  Check(n=1);
  Check(s3='{"fieldCount":3,"values":["DateTime","Level","Content",40640.5028819444,'+
    '2,"20110407 12040904 debug {\"TObjectList(00AF8D00)\":[\"TObjectList(00AF8D20)\",'+
    '\"TObjectList(00AF8D60)\",\"TFileVersion(00ADC0B0)\",\"TSynMapFile(00ACC990)\"]}"]}'#$A);
  s3 := Demo.ExecuteJSON('select * from test where level=3',False,@n);
  Check(s3='{"fieldCount":3,"values":["DateTime","Level","Content"]}'#$A);
  Check(n=0);
end;

type
  TFV = packed record
    Major, Minor, Release, Build: integer;
    Main, Detailed: string;
  end;
  TFVs = array of TFV;
{$ifndef LVCL}
  TCollTest = class(TCollectionItem)
  private
    FLength: Integer;
    FColor: Integer;
    FName: RawUTF8;
  published
    property Color: Integer read FColor write FColor;
    property Length: Integer read FLength write FLength;
    property Name: RawUTF8 read FName write FName;
  end;
  TCollTests = class(TCollection)
  private
    function GetCollItem(Index: Integer): TCollTest;
  public
    function Add: TCollTest;
    property Item[Index: Integer]: TCollTest read GetCollItem; default;
  end;

  TCollTst = class(TPersistent)
  private
    fColl: TCollTests;
    fTCollTest: TCollTest;
    fStr: TStringList;
  public
    constructor Create; 
    destructor Destroy; override;
  published
    property One: TCollTest read fTCollTest write fTCollTest;
    property Coll: TCollTests read fColl write fColl;
    property Str: TStringList read fStr write fStr;
  end;
{$endif}
{$ifdef INCLUDE_FTS3}
  TSQLFTSTest = class(TSQLRecordFTS3)
  private
    fSubject: RawUTF8;
    fBody: RawUTF8;
  published
    property Subject: RawUTF8 read fSubject write fSubject;
    property Body: RawUTF8 read fBody write fBody;
  end;
{$endif}
  TSQLASource = class;
  TSQLADest = class;
  TSQLADests = class(TSQLRecordMany)
  private
    fTime: TDateTime;
    fDest: TSQLADest;
    fSource: TSQLASource;
  published
    property Source: TSQLASource read fSource;
    property Dest: TSQLADest read fDest;
    property AssociationTime: TDateTime read fTime write fTime;
  end;
  TSQLASource = class(TSQLRecordSigned)
  private
    fDestList: TSQLADests;
  published
    property SignatureTime;
    property Signature;
    property DestList: TSQLADests read fDestList;
  end;
  TSQLADest = class(TSQLRecordSigned)
  published
    property SignatureTime;
    property Signature;
  end;
  TSQLRecordPeopleArray = class(TSQLRecordPeople)
  private
    fInts: TIntegerDynArray;
    fCurrency: TCurrencyDynArray;
{$ifdef PUBLISHRECORD}
    fRec: TFTSMatchInfo;
{$endif PUBLISHRECORD}
    fFileVersion: TFVs;
    fUTF8: RawUTF8;
  published
{$ifdef PUBLISHRECORD}
    property Rec: TFTSMatchInfo read fRec write fRec;
{$endif PUBLISHRECORD}
    property UTF8: RawUTF8 read fUTF8 write fUTF8;
    property Ints: TIntegerDynArray index 1 read fInts write fInts;
    property Currency: TCurrencyDynArray index 2 read fCurrency write fCurrency;
    property FileVersion: TFVs index 3 read fFileVersion write fFileVersion;
  end;
{$ifndef LVCL}
  TSQLRecordPeopleObject = class(TSQLRecordPeople)
  private
    fPersistent: TCollTst;
    fUTF8: TRawUTF8List;
  public
    /// will create an internal TCollTst instance for Persistent property
    constructor Create; override;
    /// will release the internal TCollTst instance for Persistent property
    destructor Destroy; override;
  published
    property UTF8: TRawUTF8List read fUTF8;
    property Persistent: TCollTst read fPersistent;
  end;
{$endif}
  TSQLRecordDali1 = class(TSQLRecordVirtualTableAutoID)
  private
    fYearOfBirth: integer;
    fFirstName: RawUTF8;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;
  TSQLRecordDali2 = class(TSQLRecordDali1);


procedure TTestSQLite3Engine._TSQLRestClientDB;
var V,V2: TSQLRecordPeople;
    VA: TSQLRecordPeopleArray;
{$ifndef LVCL}
    VO: TSQLRecordPeopleObject;
{$endif}
    FV: TFV;
    ModelC: TSQLModel;
    Client: TSQLRestClientDB;
    ClientDist: TSQLRestClientURI;
    Server: TSQLRestServer;
    aStatic: TSQLRestServerStaticInMemory;
    Curr: Currency;
    DaVinci, s, h: RawUTF8;
    Refreshed: boolean;
    J: TSQLTableJSON;
    i, n, nupd, ndx: integer;
    IntArray, Results: TIntegerDynArray;
    Data: TSQLRawBlob;
    DataS: THeapMemoryStream;
    a,b: double;
procedure checks(Leonard: boolean; Client: TSQLRestClient; const msg: string);
var ID: integer;
begin
  ID := V.ID; // ClearProperties do ID := 0;
  V.ClearProperties; // reset values
  Check(Client.Retrieve(ID,V),msg); // internaly call URL()
  if Leonard then
    Check(V.FirstName='Leonard') else
    Check(V.FirstName='Leonardo1',msg);
  Check(V.LastName=DaVinci,msg);
  Check(V.YearOfBirth=1452,msg);
  Check(V.YearOfDeath=1519,msg);
end;
procedure TestMany(aClient: TSQLRestClient);
var MS: TSQLASource;
    MD, MD2: TSQLADest;
    i: integer;
    sID, dID: array[1..100] of Integer;
    res: TIntegerDynArray;
begin
  MS := TSQLASource.Create;
  MD := TSQLADest.Create;
  try
    MD.fSignatureTime := Iso8601Now;
    MS.fSignatureTime := MD.fSignatureTime;
    Check(MS.DestList<>nil);
    Check(MS.DestList.InheritsFrom(TSQLRecordMany));
    Check(aClient.TransactionBegin(TSQLASource)); // faster process
    for i := 1 to high(dID) do begin
      MD.fSignature := FormatUTF8('% %',[aClient.ClassName,i]);
      dID[i] := aClient.Add(MD,true);
      Check(dID[i]>0);
    end;
    for i := 1 to high(sID) do begin
      MS.fSignature := FormatUTF8('% %',[aClient.ClassName,i]);
      sID[i] := aClient.Add(MS,True);
      Check(sID[i]>0);
      MS.DestList.AssociationTime := i;
      Check(MS.DestList.ManyAdd(aClient,sID[i],dID[i])); // associate both lists
      Check(not MS.DestList.ManyAdd(aClient,sID[i],dID[i],true)); // no dup
    end;
    aClient.Commit;
    for i := 1 to high(dID) do begin
      Check(MS.DestList.SourceGet(aClient,dID[i],res));
      if not CheckFailed(length(res)=1) then
        Check(res[0]=sID[i]);
      Check(MS.DestList.ManySelect(aClient,sID[i],dID[i]));
      Check(MS.DestList.AssociationTime=i);
    end;
    for i := 1 to high(sID) do begin
      Check(MS.DestList.DestGet(aClient,sID[i],res));
      if CheckFailed(length(res)=1) then
        continue; // avoid GPF
      Check(res[0]=dID[i]);
      Check(MS.DestList.FillMany(aClient,sID[i])=1);
      Check(MS.DestList.FillOne);
      Check(Integer(MS.DestList.Source)=sID[i]);
      Check(Integer(MS.DestList.Dest)=dID[i]);
      Check(MS.DestList.AssociationTime=i);
      Check(not MS.DestList.FillOne);
      Check(MS.DestList.DestGetJoined(aClient,'',sID[i],res));
      if not CheckFailed(length(res)=1) then
        Check(res[0]=dID[i]);
      Check(MS.DestList.DestGetJoined(aClient,'ADest.SignatureTime=:(0):',sID[i],res));
      Check(length(res)=0);
      Check(MS.DestList.DestGetJoined(aClient,
        FormatUTF8('ADest.SignatureTime=?',[],[MD.SignatureTime]),sID[i],res));
//   'ADest.SignatureTime=:('+Int64ToUTF8(MD.SignatureTime)+'):',sID[i],res));
      if CheckFailed(length(res)=1) then
        continue; // avoid GPF
      Check(res[0]=dID[i]);
      MD2 := MS.DestList.DestGetJoined(aClient,
        FormatUTF8('ADest.SignatureTime=?',[],[MD.SignatureTime]),sID[i]) as TSQLADest;
//   'ADest.SignatureTime=:('+Int64ToUTF8(MD.SignatureTime)+'):',sID[i]) as TSQLADest;
      if CheckFailed(MD2<>nil) then
        continue;
      try
        Check(MD2.FillOne);
        Check(MD2.ID=dID[i]);
        Check(MD2.Signature=FormatUTF8('% %',[aClient.ClassName,i]));
      finally
        MD2.Free;
      end;
    end;
    Check(MS.FillPrepareMany(aClient,
      'DestList.Dest.SignatureTime<>% and id>=? and DestList.AssociationTime<>0 '+
      'and SignatureTime=DestList.Dest.SignatureTime '+
      'and DestList.Dest.Signature<>"DestList.AssociationTime"',[0],[sID[1]]));
    Check(MS.FillTable.RowCount=length(sID));
    for i := 1 to high(sID) do begin
      MS.SignatureTime := 0;
      MS.DestList.Dest.SignatureTime := 0;
      if CheckFailed(MS.FillOne) then
        break;
      Check(MS.fID=sID[i]);
      Check(MS.SignatureTime=MD.fSignatureTime);
      Check(MS.DestList.AssociationTime=i);
      Check(MS.DestList.Dest.fID=dID[i]);
      Check(MS.DestList.Dest.SignatureTime=MD.fSignatureTime);
      Check(MS.DestList.Dest.Signature=FormatUTF8('% %',[aClient.ClassName,i]));
    end;
    MS.FillClose;
    Check(aClient.TransactionBegin(TSQLADests)); // faster process
    for i := 1 to high(sID) shr 2 do
      Check(MS.DestList.ManyDelete(aClient,sID[i*4],dID[i*4]));
    aClient.Commit;
    for i := 1 to high(sID) do
      if i and 3<>0 then begin
        Check(MS.DestList.ManySelect(aClient,sID[i],dID[i]));
        Check(MS.DestList.AssociationTime=i);
      end else
        Check(not MS.DestList.ManySelect(aClient,sID[i],dID[i]));
  finally
    MD.Free;
    MS.Free;
  end;
end;
procedure TestDynArray(aClient: TSQLRestClient);
var i, j, k, l: integer;
    IDs: TIntegerDynArray;
begin
  VA.ClearProperties;
  for i := 1 to n do begin
    aClient.Retrieve(i,VA);
    Check(VA.ID=i);
    Check(VA.LastName='Dali');
    Check(length(VA.Ints)=i shr 5);
    Check(length(VA.Currency)=i shr 5);
    Check(length(VA.FileVersion)=i shr 5);
    if i and 31=0 then begin
      Check(VA.UTF8='');
      for j := 0 to high(VA.Ints) do
        Check(VA.Ints[j]=(j+1) shl 5);
      for j := 0 to high(VA.Currency) do
        Check(PInt64(@VA.Currency[j])^=(j+1)*3200);
      for j := 0 to high(VA.FileVersion) do
        with VA.FileVersion[j] do begin
          k := (j+1) shl 5;
          Check(Major=k);
          Check(Minor=k+2000);
          Check(Release=k+3000);
          Check(Build=k+4000);
          Check(Main=IntToStr(k));
          Check(Detailed=IntToStr(k+1000));
        end;
    end else begin
      Check(GetInteger(pointer(VA.UTF8))=i);
      for j := 0 to high(VA.FileVersion) do
        with VA.FileVersion[j] do begin
          k := (j+1) shl 5;
          Check(Major=k);
          Check(Minor=k+2000);
          Check(Release=k+3000);
          Check(Build=k+4000);
        end;
    end;
{$ifdef PUBLISHRECORD}
    Check(VA.fRec.nPhrase=i);
    Check(VA.fRec.nCol=i*2);
    Check(VA.fRec.hits[2].docs_with_hits=i*3);
{$endif PUBLISHRECORD}
  end;
  for i := 1 to n shr 5 do begin
    k := i shl 5;
    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
      FormatUTF8('IntegerDynArrayContains(Ints,?)',[],[k]),IDs);
    l := n+1-32*i;
    Check(length(IDs)=l);
    for j := 0 to high(IDs) do
      Check(IDs[j]=k+j);
    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
      FormatUTF8('CardinalDynArrayContains(Ints,?)',[],[k]),IDs);
    Check(length(IDs)=l);
    for j := 0 to high(IDs) do
      Check(IDs[j]=k+j);
    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
      FormatUTF8('MyIntegerDynArrayContains(Ints,:("%"):)',
        [BinToBase64WithMagic(@k,sizeof(k))]),IDs);
    Check(length(IDs)=l);
    for j := 0 to high(IDs) do
      Check(IDs[j]=k+j);
  end;
end;
{$ifndef LVCL}
procedure TestObject(aClient: TSQLRestClient);
var i, j, k: integer;
begin
  for i := 1 to n do begin
    VO.ClearProperties;
    aClient.Retrieve(i,VO);
    Check(VO.ID=i);
    Check(VO.LastName='Morse');
    Check(VO.UTF8.Count=i shr 5);
    for j := 0 to VO.UTF8.Count-1 do
      Check(GetInteger(pointer(VO.UTF8[j]))=(j+1) shl 5);
    Check(VO.Persistent.One.Length=i);
    Check(VO.Persistent.One.Color=i+100);
    Check(GetInteger(pointer(VO.Persistent.One.Name))=i);
    Check(VO.Persistent.Coll.Count=i shr 5);
    for j := 0 to VO.Persistent.Coll.Count-1 do
     with VO.Persistent.Coll[j] do begin
       k := (j+1) shl 5;
       Check(Color=k+1000);
       Check(Length=k*2);
       Check(GetInteger(pointer(Name))=k*3);
     end;
  end;
end;
{$endif}
{$ifdef INCLUDE_FTS3}
procedure TestFTS3(aClient: TSQLRestClient);
var FTS: TSQLFTSTest;
    StartID, i: integer;
    IntResult: TIntegerDynArray;
    c: Char;
const COUNT=400;
begin
  Check(Length(IntArray)>COUNT*2);
  FTS := TSQLFTSTest.Create;
  try
    if aClient=Client then
      StartID := 0 else
      StartID := COUNT;
    Check(aClient.TransactionBegin(TSQLFTSTest)); // MUCH faster with this
    for i := StartID to StartID+COUNT-1 do begin
      FTS.DocID := IntArray[i];
      FTS.Subject := aClient.OneFieldValue(TSQLRecordPeople,'FirstName',FTS.DocID);
      Check(IdemPChar(pointer(FTS.Subject),'SALVADOR'));
      FTS.Body := FTS.Subject+' bodY'+IntToStr(FTS.DocID);
      aClient.Add(FTS,true);
    end;
    aClient.Commit; // Commit must be BEFORE OptimizeFTS3, memory leak otherwize
    Check(FTS.OptimizeFTS3Index(Client.fServer));
    for i := StartID to StartID+COUNT-1 do begin
      Check(IdemPChar(pointer(aClient.OneFieldValue(TSQLFTSTest,'Subject',IntArray[i])),'SALVADOR'));
      FTS.DocID := 0;
      FTS.Subject := '';
      FTS.Body := '';
      Check(aClient.Retrieve(IntArray[i],FTS));
      Check(FTS.DocID=IntArray[i]);
      Check(IdemPChar(pointer(FTS.Subject),'SALVADOR'));
      Check(PosEx(Int32ToUtf8(FTS.DocID),FTS.Body,1)>0);
    end;
    Check(aClient.FTSMatch(TSQLFTSTest,'Subject MATCH "salVador1"',IntResult));
    for i := 0 to high(IntResult) do
      Check(SameTextU(aClient.OneFieldValue(
        TSQLRecordPeople,'FirstName',IntResult[i]),'SALVADOR1'));
    Check(aClient.FTSMatch(TSQLFTSTest,'Subject MATCH "salVador1*"',IntResult));
    for i := 0 to high(IntResult) do
      Check(IdemPChar(pointer(aClient.OneFieldValue(
        TSQLRecordPeople,'FirstName',IntResult[i])),'SALVADOR1'));
    Check(not aClient.FTSMatch(TSQLFTSTest,'body*',IntResult,[1]),'invalid count');
    for c := '1' to '9' do begin
      Check(aClient.FTSMatch(TSQLFTSTest,'Body MATCH "body'+c+'*"',IntResult));
      Check(length(IntResult)>0);
      for i := 0 to high(IntResult) do
        Check(IntToStr(IntResult[i])[1]=c);
      Check(aClient.FTSMatch(TSQLFTSTest,'body'+c+'*',IntResult,[1,0.5]),'rank');
      Check(length(IntResult)>0);
      for i := 0 to high(IntResult) do
        Check(IntToStr(IntResult[i])[1]=c);
    end;
  finally
    FTS.Free;
  end;
end;
{$endif}
procedure TestVirtual(aClient: TSQLRestClient; DirectSQL: boolean; const Msg: string;
  aClass: TSQLRecordClass);
var n, i, ndx: integer;
    VD, VD2: TSQLRecordDali1;
    Static: TSQLRestServerStatic;
begin
  Client.fServer.StaticVirtualTableDirect := DirectSQL;
  Check(Client.fServer.EngineExecuteFmt('DROP TABLE %',[aClass.SQLTableName]));
  Client.fServer.CreateMissingTables(0);
  VD := aClass.Create as TSQLRecordDali1;
  try
    if aClient.TransactionBegin(aClass) then
    try
      // add some items to the file
      V2.FillPrepare(aClient,'LastName=:("Dali"):');
      n := 0;
      while V2.FillOne do begin
        VD.FirstName := V2.FirstName;
        VD.YearOfBirth := V2.YearOfBirth;
        VD.YearOfDeath := V2.YearOfDeath;
        inc(n);
        Check(aClient.Add(VD,true)=n,Msg);
      end;
      // update some items in the file
      Check(aClient.TableRowCount(aClass)=1001,'Check SQL Count(*)');
      for i := 1 to n do begin
        VD.ClearProperties;
        Check(VD.ID=0);
        Check(VD.FirstName='');
        Check(VD.YearOfBirth=0);
        Check(VD.YearOfDeath=0);
        Check(aClient.Retrieve(i,VD),Msg);
        Check(VD.ID=i);
        Check(IdemPChar(pointer(VD.FirstName),'SALVADOR'));
        Check(VD.YearOfBirth=1904);
        Check(VD.YearOfDeath=1989);
        VD.YearOfBirth := VD.YearOfBirth+i;
        VD.YearOfDeath := VD.YearOfDeath+i;
        Check(aClient.Update(VD),Msg);
      end;
      // check SQL requests
      for i := 1 to n do begin
        VD.ClearProperties;
        Check(VD.ID=0);
        Check(VD.FirstName='');
        Check(VD.YearOfBirth=0);
        Check(VD.YearOfDeath=0);
        Check(aClient.Retrieve(i,VD),Msg);
        Check(IdemPChar(pointer(VD.FirstName),'SALVADOR'));
        Check(VD.YearOfBirth=1904+i);
        Check(VD.YearOfDeath=1989+i);
      end;
      Check(aClient.TableRowCount(aClass)=1001);
      aClient.Commit; // write to file
      // try to read directly from file content
      Static := Client.Server.StaticVirtualTable[aClass];
      if CheckFailed(Static<>nil) then
        exit;
      if Static.FileName<>'' then begin // no file content if ':memory' DB
        (Static as TSQLRestServerStaticInMemoryExternal).
          UpdateFile; // force update (COMMIT not always calls xCommit)
        Static := TSQLRestServerStaticInMemoryExternal.Create(aClass,nil,Static.FileName,
          aClass=TSQLRecordDali2);
        try
          Check(TSQLRestServerStaticInMemory(Static).Count=n);
          for i := 1 to n do begin
            ndx := TSQLRestServerStaticInMemory(Static).IDToIndex(i);
            if CheckFailed(ndx>=0) then
              continue;
            VD2 := TSQLRestServerStaticInMemory(Static).Items[ndx] as TSQLRecordDali1;
            if CheckFailed(VD2<>nil) then
              continue;
            Check(VD2.ID=i);
            Check(IdemPChar(pointer(VD2.FirstName),'SALVADOR'));
            Check(VD2.YearOfBirth=1904+i);
            Check(VD2.YearOfDeath=1989+i);
          end;
        finally
          Static.Free;
        end;
      end;
    except
      aClient.RollBack; // will run an error - but this code is correct
    end;
  finally
    VD.Free;
  end;
end;
function Test(T: TSQLTable): boolean;
var aR,aF: integer;
begin
  result := false;
  if T=nil then
    exit;
  with TSQLTableDB.Create(Demo,[],Req,true) do
  try
    if (RowCount<>T.RowCount) or (FieldCount<>T.FieldCount) then begin
      Check(False);
      exit;
    end;
    for aR := 0 to RowCount do // compare all result values
      for aF := 0 to FieldCount-1 do
        if StrComp(pointer(Get(aR,aF)),pointer(T.Get(aR,aF)))<>0 then begin
         Check(False);
         exit;
       end;
    result := true;
  finally
    Free;
    T.Free;
  end;
end;
begin
  V := TSQLRecordPeople.Create;
  VA := TSQLRecordPeopleArray.Create;
{$ifndef LVCL}
  VO := TSQLRecordPeopleObject.Create;
{$endif}
  V2 := nil;
  if not IsMemory then begin
    DeleteFile('dali1.json');
    DeleteFile('dali2.data');
  end;
  Demo.RegisterSQLFunction(TypeInfo(TIntegerDynArray),@SortDynArrayInteger,
    'MyIntegerDynArrayContains');
  ModelC := TSQLModel.Create(
    [TSQLRecordPeople, {$ifdef INCLUDE_FTS3} TSQLFTSTest, {$endif}
     TSQLASource, TSQLADest, TSQLADests, TSQLRecordPeopleArray
     {$ifndef LVCL}, TSQLRecordPeopleObject{$endif},
     TSQLRecordDali1,TSQLRecordDali2],'root');
  ModelC.VirtualTableRegister(TSQLRecordDali1,TSQLVirtualTableJSON);
  ModelC.VirtualTableRegister(TSQLRecordDali2,TSQLVirtualTableBinary);
  try
    Client := TSQLRestClientDB.Create(ModelC,nil,Demo,TSQLRestServerTest,true);
    try
      Client.Server.DB.Synchronous := smOff;
      with Client.fServer.Model do
        for i := 0 to high(Tables) do
          if not CheckFailed(GetTableIndex(Tables[i])=i) then
            Check(GetTableIndex(Tables[i].SQLTableName)=i);
      // direct client access test
      Client.fServer.CreateMissingTables(0); // NEED Dest,Source,Dests,...
      Check(Client.SetUser('User','synopse')); // use default user
      DaVinci := WinAnsiToUtf8('da Vin'+chr(231)+'i');
      Check(Client.Retrieve('LastName='''+DaVinci+'''',V));
      Check(V.FirstName='Leonardo1');
      Check(V.LastName=DaVinci);
      Check(V.YearOfBirth=1452);
      Check(V.YearOfDeath=1519);
      checks(false,Client,'Retrieve');
      Check(V.ID=6,'check RETRIEVE/GET');
      Check(Client.Delete(TSQLRecordPeople,V.ID),'check DELETE');
      Check(not Client.Retrieve(V.ID,V),'now this record must not be available');
      Check(Client.Add(V,true)>0,'check ADD/PUT');
      checks(false,Client,'check created value is well retrieved');
      checks(false,Client,'check caching');
      V2 := V.CreateCopy as TSQLRecordPeople;
      Check(V2.SameValues(V));
      V2.Free;
      V2 := TSQLRecordPeople.Create(Client,V.ID);
      Check(V2.SameValues(V));
      Check(Client.Retrieve(V.ID,V2,true),'with LOCK');
      Check(V2.SameValues(V));
      V.FirstName := 'Leonard';
      Check(Client.Update(V));
      Check(Client.UnLock(V),'unlock');
      checks(true,Client,'check UPDATE/POST');
      if Client.SessionUser=nil then // only if has the right for EngineExecute
        Check(Client.EngineExecute('VACUUM;'),'check direct EngineExecute') else
        Check(Client.Server.EngineExecuteAll('VACUUM;'));
      Check(V2.FirstName='Leonardo1');
      Check(not V2.SameValues(V),'V and V2 must differ');
      Check(Client.UpdateFromServer([V2],Refreshed));
      Check(Refreshed,'V2 value will be synchronized with V');
      Check(V2.SameValues(V));
      Check(Client.UpdateFromServer([V2],Refreshed));
      Check(not Refreshed);
      Req := StringReplace(Req,'*',TSQLRecordPeople.RecordProps.SQLTableSimpleFields[true,false],[]);
      s := WinAnsiToUtf8('LastName=''M'+chr(244)+'net'' ORDER BY FirstName');
      J := Client.List([TSQLRecordPeople],'*',s);
      Check(Client.UpdateFromServer([J],Refreshed));
      Check(not Refreshed);
      Check(Test(J),'incorrect TSQLTableJSON');
      Check(Client.OneFieldValues(TSQLRecordPeople,'ID','LastName=:("Dali"):',IntArray));
      Check(length(IntArray)=1001);
      for i := 0 to high(IntArray) do
        Check(Client.OneFieldValue(TSQLRecordPeople,'LastName',IntArray[i])='Dali');
      Check(Client.TransactionBegin(TSQLRecordPeople)); // for UpdateBlob() below
      for i := 0 to high(IntArray) do begin
        Check(Client.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',Data));
        Check(Length(Data)=sizeof(BlobDali));
        Check(CompareMem(pointer(Data),@BlobDali,sizeof(BlobDali)));
        Check(Client.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',DataS));
        Check((DataS.Size=4) and (PCardinal(DataS.Memory)^=$E7E0E961));
        DataS.Free;
        Check(Client.UpdateBlob(TSQLRecordPeople,IntArray[i],'Data',@IntArray[i],4));
        Check(Client.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',Data));
        Check((length(Data)=4) and (PInteger(pointer(Data))^=IntArray[i]));
        V2.ID := IntArray[i]; // debug use - do NOT set ID in your programs!
        Check(V2.DataAsHex(Client)=SynCommons.BinToHex(Data));
        a := Random;
        b := Random;
        Check(SameValue(TSQLRecordPeople.Sum(Client,a,b),a+b,1E-10));
      end;
      Client.Commit;
      Check(Client.TransactionBegin(TSQLRecordPeopleArray)); 
      V2.FillPrepare(Client,'LastName=:("Dali"):');
      n := 0;
      while V2.FillOne do begin
        VA.FillFrom(V2); // fast copy some content from TSQLRecordPeople
        inc(n);
        if n and 31=0 then begin
          VA.UTF8 := '';
          VA.DynArray('Ints').Add(n);
          Curr := n*0.01;
          VA.DynArray(2).Add(Curr);
          FV.Major := n;
          FV.Minor := n+2000;
          FV.Release := n+3000;
          FV.Build := n+4000;
          str(n,FV.Main);
          str(n+1000,FV.Detailed);
          VA.DynArray('FileVersion').Add(FV);
        end else
          str(n,VA.fUTF8);
{$ifdef PUBLISHRECORD}
        VA.fRec.nPhrase := n;
        VA.fRec.nCol := n*2;
        VA.fRec.hits[2].docs_with_hits := n*3;
{$endif PUBLISHRECORD}
        Check(Client.Add(VA,true)=n);
      end;
      Client.Commit;
{$ifndef LVCL}
      if Client.TransactionBegin(TSQLRecordPeopleObject) then
      try
        V2.FillPrepare(Client,'LastName=:("Morse"):');
        n := 0;
        while V2.FillOne do begin
          VO.FillFrom(V2); // fast copy some content from TSQLRecordPeople
          inc(n);
          VO.Persistent.One.Color := n+100;
          VO.Persistent.One.Length := n;
          VO.Persistent.One.Name := Int32ToUtf8(n);
          if n and 31=0 then begin
            VO.UTF8.Add(VO.Persistent.One.Name);
            with VO.Persistent.Coll.Add do begin
              Color := n+1000;
              Length := n*2;
              Name := Int32ToUtf8(n*3);
            end;
          end;
          Check(Client.Add(VO,true)=n);
        end;
        Client.Commit;
      except
        Client.RollBack;
      end;
{$endif}
{$ifdef INCLUDE_FTS3}
      TestFTS3(Client);
{$endif}
      TestDynArray(Client);
{$ifndef LVCL}
      TestObject(Client);
{$endif}
      TestMany(Client);
      // RegisterVirtualTableModule(TSQLVirtualTableJSON) already done
      TestVirtual(Client,false,'Virtual Table access via SQLite',TSQLRecordDali1);
      TestVirtual(Client,false,'Virtual Table access via SQLite',TSQLRecordDali2);
      TestVirtual(Client,true,'Direct Virtual Table access',TSQLRecordDali1);
      TestVirtual(Client,true,'Direct Virtual Table access',TSQLRecordDali2);
      // remote client access test
      Check(Client.fServer.ExportServerNamedPipe('Test'),'declare Test server');
      ClientDist := TSQLRestClientURINamedPipe.Create(ModelC,'Test');
      try
        Check(ClientDist.SetUser('User','synopse'));
{$ifdef INCLUDE_FTS3}
        TestFTS3(ClientDist);
{$endif}TestDynArray(ClientDist);
{$ifndef LVCL}
        TestObject(ClientDist);
{$endif}TestMany(ClientDist);
        TestVirtual(ClientDist,false,'Remote Virtual Table access via SQLite',TSQLRecordDali1);
        TestVirtual(ClientDist,false,'Remote Virtual Table access via SQLite',TSQLRecordDali2);
        TestVirtual(ClientDist,true,'Remote Direct Virtual Table access',TSQLRecordDali1);
        TestVirtual(ClientDist,true,'Remote Direct Virtual Table access',TSQLRecordDali2);
        Check(Test(ClientDist.List([TSQLRecordPeople],'*',s)),'through URI and JSON');
        for i := 0 to high(IntArray) do begin
          Check(ClientDist.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',Data));
          Check((length(Data)=4) and (PInteger(pointer(Data))^=IntArray[i]));
          V2.ID := IntArray[i]; // debug use - do NOT set ID in your programs!
          Check(V2.DataAsHex(ClientDist)=SynCommons.BinToHex(Data));
          a := Random;
          b := Random;
          CheckSame(TSQLRecordPeople.Sum(Client,a,b),a+b);
        end;
        V.FirstName := 'Leonardo1';
        Check(ClientDist.Update(V));
        checks(false,ClientDist,'check remote UPDATE/POST');
        V.FirstName := 'Leonard';
        Check(ClientDist.Update(V));
        checks(true,ClientDist,'check remote UPDATE/POST');
//          time := GetTickCount; while time=GetTickCount do; time := GetTickCount;
        for i := 1 to 400 do // speed test: named pipes are OK
          checks(true,ClientDist,'caching speed test');
//          writeln('NamedPipe connection time is ',GetTickCount-time,'ms');
      finally
        ClientDist.Free;
      end;
      if IsMemory then begin // this is a bit time consuming, so do it once
        Server := TSQLRestServerTest.Create(ModelC,false);
        try
          Server.NoAJAXJSON := true;
          DeleteFile('People.json');
          DeleteFile('People.data');
          Server.StaticDataCreate(TSQLRecordPeople,'People.data',true);
          JS := Demo.ExecuteJSON('SELECT * From People');
          aStatic := Server.StaticDataServer[TSQLRecordPeople] as TSQLRestServerStaticInMemory;
          Check(aStatic<>nil);
          aStatic.LoadFromJSON(JS); // test Add()
          for i := 0 to aStatic.Count-1 do begin
            Check(Client.Retrieve(aStatic.ID[i],V),'test statement+bind speed');
            Check(V.SameRecord(aStatic.Items[i]),'static retrieve');
          end;
          // test our 'REST-minimal' SELECT statement SQL engine
          Server.URI(Client.SessionSign('/root/People?select=%2A&where=id%3D012'),'GET','',s,h,@SUPERVISOR_ACCESS_RIGHTS);
          Check(Hash32(S)=$9B10BE36);
          Server.URI(Client.SessionSign('/root/People?select=%2A&where=id%3D:(012):'),'GET','',s,h,@SUPERVISOR_ACCESS_RIGHTS);
          Check(Hash32(S)=$9B10BE36);
          Server.URI(Client.SessionSign('/root/People?select=%2A&where=LastName%3D%22M%C3%B4net%22'),'GET','',s,h,@SUPERVISOR_ACCESS_RIGHTS);
          Check(Hash32(S)=$7D90E964);
          Server.URI(Client.SessionSign('/root/People?select=%2A&where=YearOfBirth%3D1873'),'GET','',s,h,@SUPERVISOR_ACCESS_RIGHTS);
          Check(Hash32(S)=$F0818745);
          Server.URI(Client.SessionSign('/root/People?select=%2A'),'GET','',s,h,@SUPERVISOR_ACCESS_RIGHTS);
          Check(Hash32(S)=$17AE45E3);
          // test Retrieve() and Delete()
          Server.ExportServer; // initialize URIRequest() with the aStatic database
          USEFASTMM4ALLOC := true; // getmem() is 2x faster than GlobalAlloc()
          ClientDist := TSQLRestClientURIDll.Create(ModelC,URIRequest);
          try
            SetLength(IntArray,(aStatic.Count-1)shr 2);
            for i := 0 to high(IntArray) do begin
              IntArray[i] := aStatic.ID[i*4];
              Check(ClientDist.Retrieve(IntArray[i],V));
              Check(V.SameRecord(aStatic.Items[i*4]));
            end;
            Check(V.FillPrepare(Client,IntArray));
            for i := 0 to High(IntArray) do begin
              Check(V.FillOne);
              Check(V.ID=IntArray[i]);
              Check(V.SameRecord(aStatic.Items[i*4]));
            end;
            V.FillClose; // so that BatchUpdate(V) below will set all fields
            if ClientDist.TransactionBegin(TSQLRecordPeople) then
            try
              for i := 0 to high(IntArray) do
                Check(ClientDist.Delete(TSQLRecordPeople,IntArray[i]));
              for i := 0 to high(IntArray) do
                Check(not ClientDist.Retrieve(IntArray[i],V));
              for i := 0 to aStatic.Count-1 do begin
                Check(ClientDist.Retrieve(aStatic.ID[i],V));
                V.YearOfBirth := Random(MaxInt)-Random(MaxInt);
                Check(ClientDist.Update(V));
                Check(ClientDist.Retrieve(aStatic.ID[i],V));
                Check(V.SameRecord(aStatic.Items[i]));
              end;
              ClientDist.Commit;
            except
              ClientDist.RollBack;
            end else
              Check(False,'TransactionBegin');
            // test BATCH sequence usage
            if ClientDist.TransactionBegin(TSQLRecordPeople) then
            try
              Check(ClientDist.BatchStart(TSQLRecordPeople));
              n := 0;
              for i := 0 to aStatic.Count-1 do
                if i and 7=0 then begin
                  IntArray[n] := aStatic.ID[i];
                  inc(n);
                end;
              for i := 0 to n-1 do
                Check(ClientDist.BatchDelete(IntArray[i])=i);
              nupd := 0;
              for i := 0 to aStatic.Count-1 do
                if i and 7<>0 then begin // not yet deleted in BATCH mode
                   Check(ClientDist.Retrieve(aStatic.ID[i],V));
                   V.YearOfBirth := 1800+nupd;
                   Check(ClientDist.BatchUpdate(V)=nupd+n);
                   inc(nupd);
                 end;
              V.LastName := 'New';
              for i := 0 to 1000 do begin
                V.FirstName := RandomUTF8(10);
                V.YearOfBirth := i+1000;
                Check(ClientDist.BatchAdd(V,true)=n+nupd+i);
              end;
              Check(ClientDist.BatchSend(Results)=200);
              Check(Length(Results)=9260);
              ClientDist.Commit;
              for i := 0 to n-1 do
                Check(not ClientDist.Retrieve(IntArray[i],V),'BatchDelete');
              for i := 0 to high(Results) do
                if i<nupd+n then
                  Check(Results[i]=200) else begin
                  Check(Results[i]>0);
                  ndx := aStatic.IDToIndex(Results[i]);
                  Check(ndx>=0);
                  with TSQLRecordPeople(aStatic.Items[ndx]) do begin
                    Check(LastName='New','BatchAdd');
                    Check(YearOfBirth=1000+i-nupd-n);
                  end;
                end;
              for i := 0 to aStatic.Count-1 do
                with TSQLRecordPeople(aStatic.Items[i]) do
                  if LastName='New' then
                    break else
                    Check(YearOfBirth=1800+i,'BatchUpdate');
            except
              ClientDist.RollBack;
            end else
              Check(False,'TransactionBegin');
            // test BATCH update from partial FillPrepare
            V.FillPrepare(ClientDist,'LastName=?',['New'],'ID,YearOfBirth');
            if ClientDist.TransactionBegin(TSQLRecordPeople) then
            try
              Check(ClientDist.BatchStart(TSQLRecordPeople));
              n := 0;
              V.LastName := 'NotTransmitted';
              while V.FillOne do begin
                Check(V.LastName='NotTransmitted');
                Check(V.YearOfBirth=n+1000);
                V.YearOfBirth := n;
                ClientDist.BatchUpdate(V); // will update only V.YearOfBirth
                inc(n);
              end;
              Check(n=1001);
              SetLength(Results,0);
              Check(ClientDist.BatchSend(Results)=200);
              Check(length(Results)=1001);
              for i := 0 to high(Results) do
                Check(Results[i]=200);
              ClientDist.Commit;
            except
              ClientDist.RollBack;
            end else
              Check(False,'TransactionBegin');
            V.FillPrepare(ClientDist,'LastName=?',['New'],'YearOfBirth');
            n := 0;
            while V.FillOne do begin
              Check(V.LastName='NotTransmitted');
              Check(V.YearOfBirth=n);
              V.YearOfBirth := 1000;
              inc(n);
            end;
            Check(n=length(Results));
            V.FillClose;
          finally
            ClientDist.Free;
          end;
          aStatic.Modified := true;
          aStatic.UpdateFile; // force People.data file content write
          aStatic.FileName := 'People.json';
          aStatic.BinaryFile := false;
          aStatic.Modified := true; // so aStatic.Free will write People.json file
          USEFASTMM4ALLOC := false;
        finally
          Server.Free;
        end;
      end;
    finally
      Client.Free;
    end;
  finally
    ModelC.Free;
    V.Free;
    V2.Free;
    VA.Free;
{$ifndef LVCL}
    VO.Free;
{$endif}
    FreeAndNil(Demo);
  end;
  if not IsMemory then begin
    ChangeSQLEncryptTablePassWord(TempFileName,'NewPass',''); // uncrypt file
    Check(IsSQLite3File(TempFileName));
    CreateSQLEncryptTable(''); // reset the global password, if we work after test
  end;
end;

procedure TTestSQLite3Engine._TSQLTableJSON;
var J: TSQLTableJSON;
    aR, aF: integer;
    Comp: TUTF8Compare;
begin
  J := TSQLTableJSON.Create([],'',JS);
  try
    if JS<>'' then // avoid memory leak
    with TSQLTableDB.Create(Demo,[],Req,true) do
    try
      Check(RowCount=J.RowCount);
      Check(FieldCount=J.FieldCount);
      for aR := 0 to RowCount do
        for aF := 0 to FieldCount-1 do
         if (aR>0) and (aF=3) then  // aF=3=Blob
           Check(GetBlob(aR,aF)=J.GetBlob(aR,aF)) else begin
           Check((GetW(aR,aF)=J.GetW(aR,aF)) and
                (GetA(aR,aF)=J.GetA(aR,aF)) and
                (length(GetW(aR,aF))shr 1=LengthW(aR,aF)),
                Format('Get() in Row=%d Field=%d',[aR,aF]));
            if (aR>0) and (aF>3) then
              Check(GetDateTime(aR,af)=J.GetDateTime(aR,aF));
          end;
    finally
      Free;
    end;
    Demo.Execute('VACUUM;');
    with TSQLTableDB.Create(Demo,[],Req,true) do // re-test after VACCUM
    try
      Check(RowCount=J.RowCount);
      Check(FieldCount=J.FieldCount);
      for aR := 0 to RowCount do
        for aF := 0 to FieldCount-1 do // aF=3=Blob
          Check((aF=3) or (StrIComp(Get(aR,aF),J.Get(aR,aF))=0));
    finally
      Free;
    end;
    for aF := 0 to J.FieldCount-1 do begin
      J.SortFields(aF);
      Comp := J.SortCompare(aF);
      if @Comp<>nil then // BLOB field will be ignored
        for aR := 1 to J.RowCount-1 do // ensure data sorted in increasing order
          Check(Comp(pointer(J.Get(aR,aF)),pointer(J.Get(aR+1,aF)))<=0,'SortCompare');
    end;
  finally
    J.Free;
  end;
end;

{$ifdef UNICODE}
{$WARNINGS ON} // don't care about implicit string cast in tests
{$endif}


{ TSQLRestServerTest }

function TSQLRestServerTest.DataAsHex(var aParams: TSQLRestServerCallBackParams): Integer;
var aData: TSQLRawBlob;
begin
  result := 404; // invalid Request
  if (self=nil) or (aParams.Table=nil) or
      not aParams.Table.InheritsFrom(TSQLRecord) or
    (aParams.Context.ID<0) then
    exit; // we need a valid record and its ID
  if not RetrieveBlob(TSQLRecordPeople,aParams.Context.ID,'Data',aData) then
    exit; // impossible to retrieve the Data BLOB field
  aParams.Resp := JSONEncodeResult([SynCommons.BinToHex(aData)]);
  // idem: aResp := JSONEncode(['result',BinToHex(aRecord.fData)],TempMemoryStream);
  result := 200; // success
end;

function TSQLRestServerTest.Sum(var aParams: TSQLRestServerCallBackParams): Integer;
var a,b: Extended;
begin
  if not UrlDecodeNeedParameters(aParams.Parameters,'A,B') then begin
    result := 404; // invalid Request
    exit;
  end;
  while aParams.Parameters<>nil do begin
    UrlDecodeExtended(aParams.Parameters,'A=',a);
    UrlDecodeExtended(aParams.Parameters,'B=',b,@aParams.Parameters);
  end;
  aParams.Resp := JSONEncodeResult([a+b]);
  // same as : aResp := JSONEncode(['result',a+b],TempMemoryStream);
  result := 200; // success
end;


{$ifndef LVCL}

{ TSQLRecordPeopleObject }

constructor TSQLRecordPeopleObject.Create;
begin
  inherited;
  fPersistent := TCollTst.Create;
  fUTF8 := TRawUTF8List.Create;
end;

destructor TSQLRecordPeopleObject.Destroy;
begin
  Persistent.Free;
  UTF8.Free;
  inherited;
end;

{ TCollTests }

function TCollTests.Add: TCollTest;
begin
  result := inherited Add as TCollTest;
end;

function TCollTests.GetCollItem(Index: Integer): TCollTest;
begin
  result := Items[Index] as TCollTest;
end;

{ TCollTst }

constructor TCollTst.Create;
begin
  inherited;
  fColl := TCollTests.Create(TCollTest);
  fTCollTest := TCollTest.Create(nil); 
end;

destructor TCollTst.Destroy;
begin
  fColl.Free;
  fTCollTest.Free;
  fStr.Free;
  inherited;
end;

{$endif LVCL}

function RegisterVirtualTableModule(aModule: TSQLVirtualTableClass; aDatabase: TSQLDataBase): TSQLVirtualTableModule;
begin
  result := TSQLVirtualTableModuleSQLite3.Create(aModule,nil);
  TSQLVirtualTableModuleSQLite3(result).DB := aDataBase;
end;


end.


