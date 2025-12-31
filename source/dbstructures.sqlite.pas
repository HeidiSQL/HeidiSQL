unit dbstructures.sqlite;

{$mode delphi}{$H+}

interface

uses
  dbstructures;


const
  { SQLite Result Codes
    result code definitions
    Many SQLite functions return an integer result code from the set shown
    here in order to indicate success or failure.
    New error codes may be added in future versions of SQLite.
    See also: [extended result code definitions]
  }
  SQLITE_OK           = 0;   // Successful result
  // beginning-of-error-codes
  SQLITE_ERROR        = 1;   // Generic error
  SQLITE_INTERNAL     = 2;   // Internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite3_interrupt()*/
  SQLITE_IOERR        = 10;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT      = 11;  // The database disk image is malformed
  SQLITE_NOTFOUND     = 12;  // Unknown opcode in sqlite3_file_control()
  SQLITE_FULL         = 13;  // Insertion failed because database is full
  SQLITE_CANTOPEN     = 14;  // Unable to open the database file
  SQLITE_PROTOCOL     = 15;  // Database lock protocol error
  SQLITE_EMPTY        = 16;  // Internal use only
  SQLITE_SCHEMA       = 17;  // The database schema changed
  SQLITE_TOOBIG       = 18;  // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT   = 19;  // Abort due to constraint violation
  SQLITE_MISMATCH     = 20;  // Data type mismatch
  SQLITE_MISUSE       = 21;  // Library used incorrectly
  SQLITE_NOLFS        = 22;  // Uses OS features not supported on host
  SQLITE_AUTH         = 23;  // Authorization denied
  SQLITE_FORMAT       = 24;  // Not used
  SQLITE_RANGE        = 25;  // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB       = 26;  // File opened that is not a database file
  SQLITE_NOTICE       = 27;  // Notifications from sqlite3_log()
  SQLITE_WARNING      = 28;  // Warnings from sqlite3_log()
  SQLITE_ROW          = 100; // sqlite3_step() has another row ready
  SQLITE_DONE         = 101; // sqlite3_step() has finished executing

  { SQLite Flags
    These constants define various flags that can be passed into
    "prepFlags" parameter of the [sqlite3_prepare_v3()] and
    [sqlite3_prepare16_v3()] interfaces.
    New flags may be added in future releases of SQLite.
  }
  SQLITE_PREPARE_PERSISTENT = $01; // prepared statement will be retained for a long time and probably reused many times
  SQLITE_PREPARE_NORMALIZE  = $02; // no-op
  SQLITE_PREPARE_NO_VTAB    = $04; // return an error (error code SQLITE_ERROR) if the statement uses any virtual tables



  { SQLite Fundamental Datatypes

    Every value in SQLite has one of five fundamental datatypes:
      64-bit signed integer
      64-bit IEEE floating point number
      string
      BLOB
      NULL
  }
  SQLITE_INTEGER  = 1;

  SQLITE_FLOAT    = 2;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;
  SQLITE_TEXT     = 3;
  SQLITE3_TEXT    = 3;
  { CAPI3REF: Database Connection Configuration Options
    These constants are the available integer configuration options that
    can be passed as the second argument to the [sqlite3_db_config()] interface.
  }
  SQLITE_DBCONFIG_MAINDBNAME            = 1000; // const char*
  SQLITE_DBCONFIG_LOOKASIDE             = 1001; // void* int int
  SQLITE_DBCONFIG_ENABLE_FKEY           = 1002; // int int*
  SQLITE_DBCONFIG_ENABLE_TRIGGER        = 1003; // int int*
  SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = 1004; // int int*
  SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005; // int int*
  SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE      = 1006; // int int*
  SQLITE_DBCONFIG_ENABLE_QPSG           = 1007; // int int*
  SQLITE_DBCONFIG_TRIGGER_EQP           = 1008; // int int*
  SQLITE_DBCONFIG_RESET_DATABASE        = 1009; // int int*
  SQLITE_DBCONFIG_DEFENSIVE             = 1010; // int int*
  SQLITE_DBCONFIG_WRITABLE_SCHEMA       = 1011; // int int*
  SQLITE_DBCONFIG_LEGACY_ALTER_TABLE    = 1012; // int int*
  SQLITE_DBCONFIG_DQS_DML               = 1013; // int int*
  SQLITE_DBCONFIG_DQS_DDL               = 1014; // int int*
  SQLITE_DBCONFIG_ENABLE_VIEW           = 1015; // int int*
  SQLITE_DBCONFIG_MAX                   = 1015; // Largest DBCONFIG


type

  Psqlite3 = Pointer;
  Psqlite3_stmt = Pointer;

  TSQLiteCollationNeededCallback = procedure(userData: Pointer; ppDb:Psqlite3; eTextRep: Integer; zName: PAnsiChar); cdecl;
  TSQLiteCollation = function(userData: Pointer; lenA: Integer; strA: PAnsiChar; lenB: Integer; strB: PAnsiChar): Integer; cdecl;

  TSQLiteLib = class(TDbLib)
    sqlite3_open: function(const filename: PAnsiChar; var ppDb: Psqlite3): Integer; cdecl;
    sqlite3_libversion: function(): PAnsiChar; cdecl;
    sqlite3_close: function(ppDb: Psqlite3): Integer; cdecl;
    sqlite3_db_config: function (ppDb: Psqlite3; op: Integer): Integer; cdecl varargs;
    sqlite3_enable_load_extension: function(ppDb: Psqlite3; onoff: Integer): Integer; cdecl;
    sqlite3_errmsg: function(ppDb: Psqlite3): PAnsiChar; cdecl;
    sqlite3_errcode: function(ppDb: Psqlite3): Integer; cdecl;
    sqlite3_prepare_v2: function(ppDb: Psqlite3; zSql: PAnsiChar; nByte: Integer; var ppStmt: Psqlite3_stmt; var pzTail: PAnsiChar): Integer; cdecl;
    sqlite3_prepare_v3: function(ppDb: Psqlite3; zSql: PAnsiChar; nByte: Integer; prepFlags: Cardinal; var ppStmt: Psqlite3_stmt; var pzTail: PAnsiChar): Integer; cdecl;
    sqlite3_exec: function(ppDb: Psqlite3; sql: PAnsiChar; callback: Integer; callvack_arg: Pointer; errmsg: PAnsiChar): Integer; cdecl;
    sqlite3_finalize: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_step: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_reset: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_total_changes: function(ppDb: Psqlite3): Integer; cdecl;
    sqlite3_column_text: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_count: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_column_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_decltype: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_database_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_table_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_origin_name: function(pStmt: Psqlite3_stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_type: function(pStmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
    sqlite3_next_stmt: function(ppDb: Psqlite3; pStmt: Psqlite3_stmt): Psqlite3_stmt; cdecl;
    sqlite3_table_column_metadata: function(ppDb: Psqlite3;
      zDbName, zTableName, zColumnName: PAnsiChar;
      out pzDataType, pzCollSeq: PAnsiChar; out pNotNull, pPrimaryKey, pAutoinc: Integer
      ): Integer; cdecl;
    sqlite3_collation_needed: function(ppDb: Psqlite3; userData: Pointer; Func: TSQLiteCollationNeededCallback): Integer; cdecl;
    sqlite3_create_collation: function(ppDb: Psqlite3; const zName: PAnsiChar; eTextRep: Integer; pArg: Pointer; xCompare: TSQLiteCollation): Integer; cdecl;
    // Additionally, for use in Multiple Ciphers library:
    sqlite3_key: function(ppDb: Psqlite3; const pKey: Pointer; nKey: Integer): Integer; cdecl;
    sqlite3mc_cipher_count: function(): Integer; cdecl;
    sqlite3mc_cipher_name: function(cipherIndex: Integer): PAnsiChar; cdecl;
    sqlite3mc_cipher_index: function(const cipherName: PAnsiChar): Integer; cdecl;
    sqlite3mc_config: function(ppDb: Psqlite3; const paramName: PAnsiChar; newValue: Integer): Integer; cdecl;
    sqlite3mc_config_cipher: function(ppDb: Psqlite3; const cipherName: PAnsiChar; const paramName: PAnsiChar; newValue: Integer): Integer; cdecl;
    private
      FWithMultipleCipherFunctions: Boolean;
    protected
      procedure AssignProcedures; override;
    public
      constructor Create(UsedDllFile, HintDefaultDll: String); override;
      constructor CreateWithMultipleCipherFunctions(UsedDllFile, HintDefaultDll: String);
  end;

var

  SQLiteDatatypes: Array[0..15] of TDBDatatype =
  (
    (
      Index:           dbdtUnknown;
      Name:            'UNKNOWN';
      Description:     'Unknown data type';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtTinyint;
      Name:            'TINYINT';
      Names:           'INT2|BOOLEAN|BOOL';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtInt;
      Name:            'INTEGER';
      Names:           'INT|MEDIUMINT|INT8';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtUint;
      Name:            'UINT';
      Names:           'UINT';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBigint;
      Name:            'BIGINT';
      Names:           'UNSIGNED BIG INT';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtChar;
      Name:            'CHAR';
      Names:           'CHARACTER|CHAR|NCHAR|NATIVE CHARACTER';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtVarchar;
      Name:            'VARCHAR';
      Names:           'VARCHAR|VARYING CHARACTER|NVARCHAR|CHARACTER|CHAR|NCHAR|NATIVE CHARACTER';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtText;
      Name:            'TEXT';
      Names:           'CLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtUniqueidentifier;
      Name:            'UNIQUEIDENTIFIER';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBlob;
      Name:            'BLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtReal;
      Name:            'REAL';
      Names:           'REAL|NUMERIC|DOUBLE|DOUBLE PRECISION|FLOAT|DECIMAL';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDate;
      Name:            'DATE';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTime;
      Name:            'TIME';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime;
      Name:            'DATETIME';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtEnum;
      Name:            'ENUM';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Y'',''N''';
      Category:        dtcOther;
    ),
    (
      Index:           dbdtSet;
      Name:            'SET';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Value A'',''Value B''';
      Category:        dtcOther;
    )
  );


implementation


constructor TSQLiteLib.Create(UsedDllFile, HintDefaultDll: String);
begin
  FWithMultipleCipherFunctions := False;
  inherited;
end;

constructor TSQLiteLib.CreateWithMultipleCipherFunctions(UsedDllFile, HintDefaultDll: String);
begin
  FWithMultipleCipherFunctions := True;
  inherited Create(UsedDllFile, HintDefaultDll);
end;


procedure TSQLiteLib.AssignProcedures;
begin
  AssignProc(@sqlite3_open, 'sqlite3_open');
  AssignProc(@sqlite3_libversion, 'sqlite3_libversion');
  AssignProc(@sqlite3_close, 'sqlite3_close');
  AssignProc(@sqlite3_db_config, 'sqlite3_db_config');
  AssignProc(@sqlite3_enable_load_extension, 'sqlite3_enable_load_extension');
  AssignProc(@sqlite3_errmsg, 'sqlite3_errmsg');
  AssignProc(@sqlite3_errcode, 'sqlite3_errcode');
  AssignProc(@sqlite3_prepare_v2, 'sqlite3_prepare_v2');
  AssignProc(@sqlite3_prepare_v3, 'sqlite3_prepare_v3');
  AssignProc(@sqlite3_exec, 'sqlite3_exec');
  AssignProc(@sqlite3_finalize, 'sqlite3_finalize');
  AssignProc(@sqlite3_step, 'sqlite3_step');
  AssignProc(@sqlite3_reset, 'sqlite3_reset');
  AssignProc(@sqlite3_total_changes, 'sqlite3_total_changes');
  AssignProc(@sqlite3_column_text, 'sqlite3_column_text');
  AssignProc(@sqlite3_column_count, 'sqlite3_column_count');
  AssignProc(@sqlite3_column_name, 'sqlite3_column_name');
  AssignProc(@sqlite3_column_decltype, 'sqlite3_column_decltype');
  AssignProc(@sqlite3_column_database_name, 'sqlite3_column_database_name');
  AssignProc(@sqlite3_column_table_name, 'sqlite3_column_table_name');
  AssignProc(@sqlite3_column_origin_name, 'sqlite3_column_origin_name');
  AssignProc(@sqlite3_column_type, 'sqlite3_column_type');
  AssignProc(@sqlite3_next_stmt, 'sqlite3_next_stmt');
  AssignProc(@sqlite3_table_column_metadata, 'sqlite3_table_column_metadata');
  AssignProc(@sqlite3_collation_needed, 'sqlite3_collation_needed');
  AssignProc(@sqlite3_create_collation, 'sqlite3_create_collation');
  if FWithMultipleCipherFunctions then begin
    // Additionally, for use in Multiple Ciphers library:
    AssignProc(@sqlite3_key, 'sqlite3_key', False);
    AssignProc(@sqlite3mc_cipher_count, 'sqlite3mc_cipher_count');
    AssignProc(@sqlite3mc_cipher_name, 'sqlite3mc_cipher_name');
    AssignProc(@sqlite3mc_cipher_index, 'sqlite3mc_cipher_index');
    AssignProc(@sqlite3mc_config, 'sqlite3mc_config');
    AssignProc(@sqlite3mc_config_cipher, 'sqlite3mc_config_cipher');
  end;
end;

end.
