unit dbstructures.mysql;


interface

uses
  System.Classes, System.SysUtils, dbstructures;


const
  // General declarations
  MYSQL_ERRMSG_SIZE = 512;
  SQLSTATE_LENGTH = 5;
  SCRAMBLE_LENGTH = 20;
  MYSQL_PORT = 3306;
  LOCAL_HOST = 'localhost';
  NAME_LEN = 64;
  PROTOCOL_VERSION = 10;
  FRM_VER = 6;

  // Field's flags
  NOT_NULL_FLAG = 1;
  PRI_KEY_FLAG = 2;
  UNIQUE_KEY_FLAG = 4;
  MULTIPLE_KEY_FLAG = 8;
  BLOB_FLAG = 16;
  UNSIGNED_FLAG = 32;
  ZEROFILL_FLAG = 64;
  BINARY_FLAG = 128;
  ENUM_FLAG = 256;
  AUTO_INCREMENT_FLAG = 512;
  TIMESTAMP_FLAG = 1024;
  SET_FLAG = 2048;
  NO_DEFAULT_VALUE_FLAG = 4096; // Field has no default value
  ON_UPDATE_NOW_FLAG = 8192; // If a field is updated it will get the current time value (NOW())
  NUM_FLAG = 32768; // Field is numeric
  PART_KEY_FLAG = 16384; // wrong from here on, where do these come from?
  GROUP_FLAG = 32768;
  UNIQUE_FLAG = 65536;
  BINCMP_FLAG = 131072;

  // Client connection options
  CLIENT_LONG_PASSWORD: Int64 = 0; // obsolete flag
  CLIENT_MYSQL: Int64 = 1; // mysql/old mariadb server/client
  CLIENT_FOUND_ROWS: Int64 = 2; // Found instead of affected rows
  CLIENT_LONG_FLAG: Int64 = 4; // Get all column flags
  CLIENT_CONNECT_WITH_DB: Int64 = 8; // One can specify db on connect
  CLIENT_NO_SCHEMA: Int64 = 16; // Don't allow database.table.column
  CLIENT_COMPRESS: Int64 = 32; // Can use compression protocol
  CLIENT_ODBC: Int64 = 64; // Odbc client
  CLIENT_LOCAL_FILES: Int64 = 128; // Can use LOAD DATA LOCAL
  CLIENT_IGNORE_SPACE: Int64 = 256; // Ignore spaces before '('
  CLIENT_PROTOCOL_41: Int64 = 512; // New 4.1 protocol
  CLIENT_INTERACTIVE: Int64 = 1024; // This is an interactive client
  CLIENT_SSL: Int64 = 2048; // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE: Int64 = 4096; // IGNORE sigpipes
  CLIENT_TRANSACTIONS: Int64 = 8192; // Client knows about transactions
  CLIENT_RESERVED: Int64 = 16384; // Old flag for 4.1 protocol
  CLIENT_SECURE_CONNECTION: Int64 = 32768; // New 4.1 authentication
  CLIENT_MULTI_STATEMENTS: Int64 = 1 Shl 16; // Enable/disable multi-stmt support
  CLIENT_MULTI_RESULTS: Int64 = 1 Shl 17; // Enable/disable multi-results
  CLIENT_PS_MULTI_RESULTS: Int64 = 1 Shl 18; // Multi-results in PS-protocol
  CLIENT_PLUGIN_AUTH: Int64 = 1 Shl 19; // Client supports plugin authentication
  CLIENT_CONNECT_ATTRS: Int64 = 1 Shl 20; // Client supports connection attributes
  // Enable authentication response packet to be larger than 255 bytes.
  CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA: Int64 = 1 Shl 21;
  // Don't close the connection for a connection with expired password.
  CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS: Int64 = 1 Shl 22;
  {
  Capable of handling server state change information. Its a hint to the
  server to include the state change information in Ok packet.
  }
  CLIENT_SESSION_TRACK: Int64 = 1 Shl 23;
  // Client no longer needs EOF packet
  CLIENT_DEPRECATE_EOF: Int64 = 1 Shl 24;
  CLIENT_PROGRESS_OBSOLETE: Int64 = 1 Shl 29;
  CLIENT_SSL_VERIFY_SERVER_CERT: Int64 = 1 Shl 30;
  {
  It used to be that if mysql_real_connect() failed, it would delete any
  options set by the client, unless the CLIENT_REMEMBER_OPTIONS flag was
  given.
  That behaviour does not appear very useful, and it seems unlikely that
  any applications would actually depend on this. So from MariaDB 5.5 we
  always preserve any options set in case of failed connect, and this
  option is effectively always set.
  }
  CLIENT_REMEMBER_OPTIONS: Int64 = 1 Shl 31;

  COLLATION_BINARY = 63;
  // Equivalent to COLLATION_BINARY, this is what a new driver returns when connected to a pre-4.1 server.
  COLLATION_NONE =  0;

  // Relevant MySQL error codes, taken from include/mysql/server/mysqld_error.h
  ER_MUST_CHANGE_PASSWORD = 1820;
  ER_NO_SUCH_THREAD = 1094;
  ER_NONEXISTING_GRANT = 1141;
  ER_WRONG_AUTO_KEY = 1075;

type
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
    free:              PUSED_MEM;
    used:              PUSED_MEM;
    pre_alloc:         PUSED_MEM;
    min_malloc:        Integer;
    block_size:        Integer;
    block_num:         Integer;
    first_block_usage: Integer;
    error_handler:     PERR_PROC;
  end;

  NET = record
    vio:              Pointer;
    buff:             PAnsiChar;
    buff_end:         PAnsiChar;
    write_pos:        PAnsiChar;
    read_pos:         PAnsiChar;
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
    name:             PAnsiChar;   // Name of column
    org_name:         PAnsiChar;   // Name of original column (added after 3.23.58)
    table:            PAnsiChar;   // Table of column if column was a field
    org_table:        PAnsiChar;   // Name of original table (added after 3.23.58
    db:               PAnsiChar;   // table schema (added after 3.23.58)
    catalog:	        PAnsiChar;   // table catalog (added after 3.23.58)
    def:              PAnsiChar;   // Default value (set by mysql_list_fields)
    length:           LongInt;     // Width of column
    max_length:       LongInt;     // Max width of selected set
    // added after 3.23.58
    name_length:      Cardinal;
    org_name_length:  Cardinal;
    table_length:     Cardinal;
    org_table_length: Cardinal;
    db_length:        Cardinal;
    catalog_length:   Cardinal;
    def_length:       Cardinal;
    //***********************
    flags:            Cardinal;    // Div flags
    decimals:         Cardinal;    // Number of decimals in field
    charsetnr:        Cardinal;    // char set number (added in 4.1)
    _type:            Cardinal;    // Type of field. Se mysql_com.h for types
  end;

  // Added in Oct 2023, to fix usage of mysql_fetch_lengths(). See issue #1863
  PMYSQL_LENGTHS = ^TMYSQL_LENGTHS;
  TMYSQL_LENGTHS = array[0..MaxInt div SizeOf(LongWord) - 1] of LongWord;

  MYSQL_ROW = array[0..$ffff] of PAnsiChar;
  PMYSQL_ROW = ^MYSQL_ROW;

  PMYSQL_ROWS = ^MYSQL_ROWS;
  MYSQL_ROWS = record
    next:       PMYSQL_ROWS;
    data:       PMYSQL_ROW;
  end;

  MYSQL_DATA = record
    Rows:       Int64;
    Fields:     Cardinal;
    Data:       PMYSQL_ROWS;
    Alloc:      MEM_ROOT;
  end;
  PMYSQL_DATA = ^MYSQL_DATA;

  PMYSQL = ^MYSQL;
  MYSQL = record
    _net:            NET;
    connector_fd:    Pointer;
    host:            PAnsiChar;
    user:            PAnsiChar;
    passwd:          PAnsiChar;
    unix_socket:     PAnsiChar;
    server_version:  PAnsiChar;
    host_info:       PAnsiChar;
    info:            PAnsiChar;
    db:              PAnsiChar;
    charset:         PAnsiChar;
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
    options:         Cardinal;
    status:          Byte;
    free_me:         Byte;
    reconnect:       Byte;
    scramble:        array[1..SCRAMBLE_LENGTH+1] of Char;
    rpl_pivot:       Byte;
    master:          PMYSQL;
    next_slave:      PMYSQL;
    last_used_slave: PMYSQL;
    last_used_con:   PMYSQL;
    stmts:           Pointer;
    methods:         Pointer;
    thd:             Pointer;
    unbuffered_fetch_owner: PByte;
  end;

  MYSQL_RES = record
    row_count:       Int64;
    field_count, current_field:     Integer;
    fields:          PMYSQL_FIELD;
    data:            PMYSQL_DATA;
    data_cursor:     PMYSQL_ROWS;
    field_alloc:     MEM_ROOT;
    row:             PMYSQL_ROW;     // If unbuffered read
    current_row:     PMYSQL_ROW;     // buffer to current row
    lengths:         PLongInt;       // column lengths of current row
    handle:          PMYSQL;         // for unbuffered reads
    eof:             Byte;           // Used my mysql_fetch_row
    is_ps:           Byte;
  end;
  PMYSQL_RES = ^MYSQL_RES;

  TMySQLLib = class(TDbLib)
    mysql_affected_rows: function(Handle: PMYSQL): Int64; stdcall;
    mysql_character_set_name: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_close: procedure(Handle: PMYSQL); stdcall;
    mysql_data_seek: procedure(Result: PMYSQL_RES; Offset: Int64); stdcall;
    mysql_errno: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_error: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_fetch_field_direct: function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD; stdcall;
    mysql_fetch_field: function(Result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
    mysql_fetch_lengths: function(Result: PMYSQL_RES): PMYSQL_LENGTHS; stdcall;
    mysql_fetch_row: function(Result: PMYSQL_RES): PMYSQL_ROW; stdcall;
    mysql_free_result: procedure(Result: PMYSQL_RES); stdcall;
    mysql_get_client_info: function: PAnsiChar; stdcall;
    mysql_get_server_info: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_init: function(Handle: PMYSQL): PMYSQL; stdcall;
    mysql_num_fields: function(Result: PMYSQL_RES): Integer; stdcall;
    mysql_num_rows: function(Result: PMYSQL_RES): Int64; stdcall;
    mysql_options: function(Handle: PMYSQL; Option: Integer; arg: PAnsiChar): Integer; stdcall;
    mysql_optionsv: function(Handle: PMYSQL; Option: Integer; arg, val: PAnsiChar): Integer; stdcall;
    mysql_ping: function(Handle: PMYSQL): Integer; stdcall;
    mysql_real_connect: function(Handle: PMYSQL; const Host, User, Passwd, Db: PAnsiChar; Port: Cardinal; const UnixSocket: PAnsiChar; ClientFlag: Cardinal): PMYSQL; stdcall;
    mysql_real_query: function(Handle: PMYSQL; const Query: PAnsiChar; Length: Cardinal): Integer; stdcall;
    mysql_stat: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_store_result: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    mysql_thread_id: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_next_result: function(Handle: PMYSQL): Integer; stdcall;
    mysql_set_character_set: function(Handle: PMYSQL; csname: PAnsiChar): Integer; stdcall;
    mysql_thread_init: function: Byte; stdcall;
    mysql_thread_end: procedure; stdcall;
    mysql_warning_count: function(Handle: PMYSQL): Cardinal; stdcall;
    protected
      procedure AssignProcedures; override;
    public
      MYSQL_OPT_LOCAL_INFILE,
      MYSQL_OPT_CONNECT_TIMEOUT,
      MARIADB_OPT_TLS_VERSION,
      MYSQL_OPT_TLS_VERSION,
      MYSQL_PLUGIN_DIR,
      MYSQL_OPT_SSL_KEY,
      MYSQL_OPT_SSL_CERT,
      MYSQL_OPT_SSL_CA,
      MYSQL_OPT_SSL_CIPHER,
      MYSQL_OPT_CONNECT_ATTR_ADD,
      MYSQL_ENABLE_CLEARTEXT_PLUGIN: Integer;
      constructor Create(DllFile, DefaultDll: String); override;
  end;
var
  MySQLKeywords: TStringList;
  MySQLErrorCodes: TStringList;


  // MySQL Data Type List and Properties
  MySQLDatatypes: array [0..40] of TDBDatatype =
  (
    (
      Index:           dbdtUnknown;
      NativeTypes:     '99999';
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
      NativeType:      1;
      Name:            'TINYINT';
      Description:     'TINYINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A very small integer. The signed range is -128 to 127. ' +
        'The unsigned range is 0 to 255.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         127;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtSmallint;
      NativeType:      2;
      Name:            'SMALLINT';
      Description:     'SMALLINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A small integer. The signed range is -32768 to 32767. ' +
        'The unsigned range is 0 to 65535.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         32767;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtMediumint;
      NativeType:      9;
      Name:            'MEDIUMINT';
      Description:     'MEDIUMINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A medium-sized integer. The signed range is -8388608 to 8388607. ' +
        'The unsigned range is 0 to 16777215.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         8388607;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtInt;
      NativeType:      3;
      Name:            'INT';
      Description:     'INT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A normal-size integer. The signed range is -2147483648 to 2147483647. ' +
        'The unsigned range is 0 to 4294967295.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         2147483647;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBigint;
      NativeType:      8;
      Name:            'BIGINT';
      Description:     'BIGINT[(M)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A large integer. The signed range is -9223372036854775808 to ' +
        '9223372036854775807. The unsigned range is 0 to 18446744073709551615.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         9223372036854775807;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtFloat;
      NativeType:      4;
      Name:            'FLOAT';
      Description:     'FLOAT[(M,D)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A small (single-precision) floating-point number. Allowable values are '+
        '-3.402823466E+38 to -1.175494351E-38, 0, and 1.175494351E-38 to '+
        '3.402823466E+38. These are the theoretical limits, based on the IEEE '+
        'standard. The actual range might be slightly smaller depending on your '+
        'hardware or operating system.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDouble;
      NativeType:      5;
      Name:            'DOUBLE';
      Description:     'DOUBLE[(M,D)] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A normal-size (double-precision) floating-point number. Allowable ' +
        'values are -1.7976931348623157E+308 to -2.2250738585072014E-308, 0, and ' +
        '2.2250738585072014E-308 to 1.7976931348623157E+308. These are the ' +
        'theoretical limits, based on the IEEE standard. The actual range might ' +
        'be slightly smaller depending on your hardware or operating system.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDecimal;
      NativeType:      246;
      Name:            'DECIMAL';
      Description:     'DECIMAL[(M[,D])] [UNSIGNED] [ZEROFILL]' + sLineBreak +
        'A packed "exact" fixed-point number. M is the total number of digits ' +
        '(the precision) and D is the number of digits after the decimal point ' +
        '(the scale). The decimal point and (for negative numbers) the "-" sign ' +
        'are not counted in M. If D is 0, values have no decimal point or ' +
        'fractional part. The maximum number of digits (M) for DECIMAL is 65. ' +
        'The maximum number of supported decimals (D) is 30. If D is omitted, ' +
        'the default is 0. If M is omitted, the default is 10.';
      HasLength:       True;
      RequiresLength:  True;
      MaxSize:         9223372036854775807;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '20,6';
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDate;
      NativeType:      10;
      Name:            'DATE';
      Description:     'DATE' + sLineBreak +
        'A date. The supported range is ''1000-01-01'' to ''9999-12-31''. MySQL ' +
        'displays DATE values in ''YYYY-MM-DD'' format, but allows assignment of ' +
        'values to DATE columns using either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTime;
      NativeType:      11;
      Name:            'TIME';
      Description:     'TIME' + sLineBreak +
        'A time. The range is ''-838:59:59'' to ''838:59:59''. MySQL displays TIME ' +
        'values in ''HH:MM:SS'' format, but allows assignment of values to TIME ' +
        'columns using either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtYear;
      NativeType:      13;
      Name:            'YEAR';
      Description:     'YEAR[(2|4)]' + sLineBreak +
        'A year in two-digit or four-digit format. The default is four-digit ' +
        'format. In four-digit format, the allowable values are 1901 to 2155, ' +
        'and 0000. In two-digit format, the allowable values are 70 to 69, ' +
        'representing years from 1970 to 2069. MySQL displays YEAR values in ' +
        'YYYY format, but allows you to assign values to YEAR columns using ' +
        'either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime;
      NativeType:      12;
      Name:            'DATETIME';
      Description:     'DATETIME' + sLineBreak +
        'A date and time combination. The supported range is ''1000-01-01 ' +
        '00:00:00'' to ''9999-12-31 23:59:59''. MySQL displays DATETIME values in ' +
        '''YYYY-MM-DD HH:MM:SS'' format, but allows assignment of values to ' +
        'DATETIME columns using either strings or numbers.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTimestamp;
      NativeType:      7;
      Name:            'TIMESTAMP';
      Description:     'TIMESTAMP' + sLineBreak +
        'A timestamp. The range is ''1970-01-01 00:00:01'' UTC to ''2038-01-09 ' +
        '03:14:07'' UTC. TIMESTAMP values are stored as the number of seconds ' +
        'since the epoch (''1970-01-01 00:00:00'' UTC). A TIMESTAMP cannot ' +
        'represent the value ''1970-01-01 00:00:00'' because that is equivalent to ' +
        '0 seconds from the epoch and the value 0 is reserved for representing ' +
        '''0000-00-00 00:00:00'', the "zero" TIMESTAMP value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtVarchar;
      NativeType:      253;
      Name:            'VARCHAR';
      Description:     'VARCHAR(M)' + sLineBreak +
        'A variable-length string. M represents the maximum column length in ' +
        'characters. The range of M is 0 to 65,535. The effective maximum length ' +
        'of a VARCHAR is subject to the maximum row size (65,535 bytes, which is ' +
        'shared among all columns) and the character set used. For example, utf8 ' +
        'characters can require up to three bytes per character, so a VARCHAR ' +
        'column that uses the utf8 character set can be declared to be a maximum ' +
        'of 21,844 characters. ' + sLineBreak + sLineBreak +
        '*Note*: MySQL 5.1 follows the standard SQL specification, and does not ' +
        'remove trailing spaces from VARCHAR values.';
      HasLength:       True;
      RequiresLength:  True;
      MaxSize:         255;
      HasBinary:       True; // MySQL-Help says the opposite but it's valid for older versions at least.
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtChar;
      NativeType:      254;
      Name:            'CHAR';
      Description:     'CHAR[(M)]' + sLineBreak +
        'A fixed-length string that is always right-padded with spaces to the ' +
        'specified length when stored. M represents the column length in ' +
        'characters. The range of M is 0 to 255. If M is omitted, the length is 1.' + sLineBreak + sLineBreak +
        '*Note*: Trailing spaces are removed when CHAR values are retrieved ' +
        'unless the PAD_CHAR_TO_FULL_LENGTH SQL mode is enabled.';
      HasLength:       True;
      RequiresLength:  True;
      MaxSize:         255;
      HasBinary:       True;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtTinytext;
      NativeType:      249;
      Name:            'TINYTEXT';
      Description:     'TINYTEXT' + sLineBreak +
        'A TEXT column with a maximum length of 255 (2^8 - 1) characters. The ' +
        'effective maximum length is less if the value contains multi-byte ' +
        'characters. Each TINYTEXT value is stored using a one-byte length ' +
        'prefix that indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      MaxSize:         255;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtText;
      NativeType:      252;
      Name:            'TEXT';
      Description:     'TEXT[(M)]' + sLineBreak +
        'A TEXT column with a maximum length of 65,535 (2^16 - 1) characters. The ' +
        'effective maximum length is less if the value contains multi-byte ' +
        'characters. Each TEXT value is stored using a two-byte length prefix ' +
        'that indicates the number of bytes in the value. ' + sLineBreak +
        'An optional length M can be given for this type. If this is done, MySQL ' +
        'creates the column as the smallest TEXT type large enough to hold ' +
        'values M characters long.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         65535;
      DefaultSize:     65535;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtMediumtext;
      NativeType:      250;
      Name:            'MEDIUMTEXT';
      Description:     'MEDIUMTEXT' + sLineBreak +
        'A TEXT column with a maximum length of 16,777,215 (2^24 - 1) characters. ' +
        'The effective maximum length is less if the value contains multi-byte ' +
        'characters. Each MEDIUMTEXT value is stored using a three-byte length ' +
        'prefix that indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtLongtext;
      NativeType:      251;
      Name:            'LONGTEXT';
      Description:     'LONGTEXT' + sLineBreak +
        'A TEXT column with a maximum length of 4,294,967,295 or 4GB (2^32 - 1) ' +
        'characters. The effective maximum length is less if the value contains ' +
        'multi-byte characters. The effective maximum length of LONGTEXT columns ' +
        'also depends on the configured maximum packet size in the client/server ' +
        'protocol and available memory. Each LONGTEXT value is stored using a ' +
        'four-byte length prefix that indicates the number of bytes in the ' +
        'value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       True;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtJson;
      NativeType:      245;
      Name:            'JSON';
      Description:     'JSON' + sLineBreak +
        'Documents stored in JSON columns are converted to an internal format that '+
        'permits quick read access to document elements. When the server later must '+
        'read a JSON value stored in this binary format, the value need not be parsed '+
        'from a text representation. The binary format is structured to enable the '+
        'server to look up subobjects or nested values directly by key or array index '+
        'without reading all values before or after them in the document.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtUniqueidentifier;
      NativeType:      254;
      Name:            'UUID';
      Description:     'UUID' + sLineBreak +
        'The UUID data type is intended for the storage of 128-bit UUID (Universally ' +
        'Unique Identifier) data. See the UUID function page for more details on UUIDs ' +
        'themselves.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtInet4;
      NativeType:      255;
      Name:            'INET4';
      Description:     'INET4' + sLineBreak +
        'INET4 is a data type to store IPv4 addresses, as 4-byte binary strings. '+
        'It was added in MariaDB 10.10.0';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcText;
      MinVersion:      10100;
    ),
    (
      Index:           dbdtInet6;
      NativeType:      255;
      Name:            'INET6';
      Description:     'INET6' + sLineBreak +
        'The INET6 data type is intended for storage of IPv6 addresses, as well as ' +
        'IPv4 addresses assuming conventional mapping of IPv4 addresses into IPv6 ' +
        'addresses. ' + slineBreak +
        'Both short and long IPv6 notation are permitted, according to RFC-5952. '+
        'It was added in MariaDB 10.5.0';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcText;
      MinVersion:      10050;
    ),
    (
      Index:           dbdtBinary;
      NativeType:      254;
      Name:            'BINARY';
      Description:     'BINARY(M)' + sLineBreak +
        'The BINARY type is similar to the CHAR type, but stores binary byte ' +
        'strings rather than non-binary character strings. M represents the ' +
        'column length in bytes.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '50';
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtVarbinary;
      NativeType:      253;
      Name:            'VARBINARY';
      Description:     'VARBINARY(M)' + sLineBreak +
        'The VARBINARY type is similar to the VARCHAR type, but stores binary ' +
        'byte strings rather than non-binary character strings. M represents the ' +
        'maximum column length in bytes.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtTinyblob;
      NativeType:      249;
      Name:           'TINYBLOB';
      Description:     'TINYBLOB' + sLineBreak +
        'A BLOB column with a maximum length of 255 (2^8 - 1) bytes. Each ' +
        'TINYBLOB value is stored using a one-byte length prefix that indicates ' +
        'the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBlob;
      NativeType:      252;
      Name:            'BLOB';
      Description:     'BLOB[(M)]' + sLineBreak +
        'A BLOB column with a maximum length of 65,535 (2^16 - 1) bytes. Each ' +
        'BLOB value is stored using a two-byte length prefix that indicates the ' +
        'number of bytes in the value. ' + sLineBreak +
        'An optional length M can be given for this type. If this is done, MySQL ' +
        'creates the column as the smallest BLOB type large enough to hold ' +
        'values M bytes long.';
      HasLength:       True;
      RequiresLength:  False;
      MaxSize:         65535;
      DefaultSize:     65535;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtMediumblob;
      NativeType:      250;
      Name:            'MEDIUMBLOB';
      Description:     'MEDIUMBLOB' + sLineBreak +
        'A BLOB column with a maximum length of 16,777,215 (2^24 - 1) bytes. Each ' +
        'MEDIUMBLOB value is stored using a three-byte length prefix that ' +
        'indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtLongblob;
      NativeType:      251;
      Name:            'LONGBLOB';
      Description:     'LONGBLOB' + sLineBreak +
        'A BLOB column with a maximum length of 4,294,967,295 or 4GB (2^32 - 1) ' +
        'bytes. The effective maximum length of LONGBLOB columns depends on the ' +
        'configured maximum packet size in the client/server protocol and ' +
        'available memory. Each LONGBLOB value is stored using a four-byte ' +
        'length prefix that indicates the number of bytes in the value.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtEnum;
      NativeType:      247;
      Name:            'ENUM';
      Description:     'ENUM(''value1'',''value2'',...)' + sLineBreak +
        'An enumeration. A string object that can have only one value, chosen ' +
        'from the list of values ''value1'', ''value2'', ..., NULL or the special '''' ' +
        'error value. An ENUM column can have a maximum of 65,535 distinct ' +
        'values. ENUM values are represented internally as integers.';
      HasLength:       True; // Obviously this is not meant as "length", but as "set of values"
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Y'',''N''';
      Category:        dtcOther;
    ),
    (
      Index:           dbdtSet;
      NativeType:      248;
      Name:            'SET';
      Description:     'SET(''value1'',''value2'',...)' + sLineBreak +
        'A set. A string object that can have zero or more values, each of which ' +
        'must be chosen from the list of values ''value1'', ''value2'', ... A SET ' +
        'column can have a maximum of 64 members. SET values are represented ' +
        'internally as integers.';
      HasLength:       True; // Same as for ENUM
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '''Value A'',''Value B''';
      Category:        dtcOther;
    ),
    (
      Index:           dbdtBit;
      NativeType:      16;
      Name:            'BIT';
      Description:     'BIT[(M)]' + sLineBreak +
        'A bit-field type. M indicates the number of bits per value, from 1 to ' +
        '64. The default is 1 if M is omitted.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtPoint;
      NativeType:      255;
      Name:            'POINT';
      Description:     'POINT(x,y)' + sLineBreak +
        'Constructs a WKB Point using its coordinates.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtLinestring;
      NativeType:      255;
      Name:            'LINESTRING';
      Description:     'LINESTRING(pt1,pt2,...)' + sLineBreak +
        'Constructs a WKB LineString value from a number of WKB Point arguments. ' +
        'If any argument is not a WKB Point, the return value is NULL. If the ' +
        'number of Point arguments is less than two, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtPolygon;
      NativeType:      255;
      Name:            'POLYGON';
      Description:     'POLYGON(ls1,ls2,...)' + sLineBreak +
        'Constructs a WKB Polygon value from a number of WKB LineString ' +
        'arguments. If any argument does not represent the WKB of a LinearRing ' +
        '(that is, not a closed and simple LineString) the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtGeometry;
      NativeType:      255;
      Name:            'GEOMETRY';
      Description:     '';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtMultipoint;
      NativeType:      255;
      Name:            'MULTIPOINT';
      Description:     'MULTIPOINT(pt1,pt2,...)' + sLineBreak +
        'Constructs a WKB MultiPoint value using WKB Point arguments. If any ' +
        'argument is not a WKB Point, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtMultilinestring;
      NativeType:      255;
      Name:            'MULTILINESTRING';
      Description:     'MULTILINESTRING(ls1,ls2,...)' + sLineBreak +
        'Constructs a WKB MultiLineString value using WKB LineString arguments. ' +
        'If any argument is not a WKB LineString, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtMultipolygon;
      NativeType:      255;
      Name:            'MULTIPOLYGON';
      Description:     'MULTIPOLYGON(poly1,poly2,...)' + sLineBreak +
        'Constructs a WKB MultiPolygon value from a set of WKB Polygon ' +
        'arguments. If any argument is not a WKB Polygon, the return value is ' +
        'NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtGeometrycollection;
      NativeType:      255;
      Name:            'GEOMETRYCOLLECTION';
      Description:     'GEOMETRYCOLLECTION(g1,g2,...)' + sLineBreak +
        'Constructs a WKB GeometryCollection. If any argument is not a ' +
        'well-formed WKB representation of a geometry, the return value is NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    )

  );


  MySQLVariables: array [0..417] of TServerVariable =
  (
    (
      Name: 'auto_increment_increment';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'auto_increment_offset';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'autocommit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'automatic_sp_privileges';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'back_log';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'basedir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'big_tables';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'binlog_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'binlog_checksum';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'binlog_direct_non_transactional_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'binlog_format';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'ROW,STATEMENT,MIXED';
    ),
    (
      Name: 'binlog_row_image';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FULL,MINIMAL,NOBLOB';
    ),
    (
      Name: 'binlog_stmt_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'bulk_insert_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_client';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_connection';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_database[a]';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_filesystem';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_results';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_server';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'character_set_system';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'character_sets_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'collation_connection';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'collation_database[b]';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'collation_server';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'completion_type';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'NO_CHAIN,CHAIN,RELEASE,0,1,2';
    ),
    (
      Name: 'concurrent_insert';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'NEVER,AUTO,ALWAYS,0,1,2';
    ),
    (
      Name: 'connect_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'datadir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'date_format';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'datetime_format';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'debug';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'debug_sync';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'default_storage_engine';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FEDERATED,MRG_MYISAM,MyISAM,BLACKHOLE,CSV,MEMORY,ARCHIVE,InnoDB';
    ),
    (
      Name: 'default_tmp_storage_engine';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FEDERATED,MRG_MYISAM,MyISAM,BLACKHOLE,CSV,MEMORY,ARCHIVE,InnoDB';
    ),
    (
      Name: 'default_week_format';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'delay_key_write';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'ON,OFF,ALL';
    ),
    (
      Name: 'delayed_insert_limit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'delayed_insert_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'delayed_queue_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'disable_gtid_unsafe_statements';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'div_precision_increment';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'end_markers_in_json';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'engine_condition_pushdown';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'eq_range_index_dive_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'error_count';
      IsDynamic: False;
      VarScope: vsSession;
    ),
    (
      Name: 'event_scheduler';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'ON,OFF,DISABLED';
    ),
    (
      Name: 'expire_logs_days';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'external_user';
      IsDynamic: False;
      VarScope: vsSession;
    ),
    (
      Name: 'flush';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'flush_time';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'foreign_key_checks';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'ft_boolean_syntax';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_max_word_len';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_min_word_len';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_query_expansion_limit';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ft_stopword_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'general_log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'general_log_file';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'group_concat_max_len';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'gtid_done';
      IsDynamic: False;
      VarScope: vsBoth;
    ),
    (
      Name: 'gtid_lost';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'gtid_mode';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'gtid_mode';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'gtid_next';
      IsDynamic: True;
      VarScope: vsSession;
      EnumValues: 'AUTOMATIC,ANONYMOUS';
    ),
    (
      Name: 'gtid_owned';
      IsDynamic: False;
      VarScope: vsBoth;
    ),
    (
      Name: 'have_compress';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_crypt';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_csv';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_dynamic_loading';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_geometry';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_innodb';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_ndbcluster';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_openssl';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_partitioning';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_profiling';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_query_cache';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_rtree_keys';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_ssl';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'have_symlink';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'host_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'hostname';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'identity';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'ignore_builtin_innodb';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'init_connect';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'init_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'init_slave';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_adaptive_flushing';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_adaptive_hash_index';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_adaptive_max_sleep_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_additional_mem_pool_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_analyze_is_persistent';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_api_enable_binlog';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_api_enable_mdl';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_api_trx_level';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_autoextend_increment';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_autoinc_lock_mode';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_dump_at_shutdown';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_dump_now';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_filename';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_load_abort';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_load_at_startup';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_load_now';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_buffer_pool_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_change_buffer_max_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_change_buffering';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'INSERTS,DELETES,PURGES,CHANGES,ALL,NONE';
    ),
    (
      Name: 'innodb_checksum_algorithm';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'INNODB,CRC32,NONE,STRICT_INNODB,STRICT_CRC32,STRICT_NONE';
    ),
    (
      Name: 'innodb_checksums';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_commit_concurrency';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_concurrency_tickets';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_data_file_path';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_data_home_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_doublewrite';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_fast_shutdown';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_format';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_format_check';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_format_max';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_file_per_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_flush_log_at_trx_commit';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: '0,1,2';
    ),
    (
      Name: 'innodb_flush_method';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_flush_neighbors';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_force_load_corrupted';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_force_recovery';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_aux_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_enable_stopword';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_max_token_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_min_token_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_num_word_optimize';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_server_stopword_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_sort_pll_degree';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_ft_user_stopword_table';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_io_capacity';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_large_prefix';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_lock_wait_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_locks_unsafe_for_binlog';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_buffer_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_file_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_files_in_group';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_log_group_home_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_lru_scan_depth';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_max_dirty_pages_pct';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_max_purge_lag';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_mirrored_log_groups';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_disable';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_enable';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_reset';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_monitor_reset_all';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_old_blocks_pct';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_old_blocks_time';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_open_files';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_optimize_fulltext_only';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_page_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_print_all_deadlocks';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_purge_batch_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_purge_threads';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_random_read_ahead';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_read_ahead_threshold';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_read_io_threads';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_replication_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_rollback_on_timeout';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_rollback_segments';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_sort_buffer_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_spin_wait_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_method';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'NULLS_EQUAL,NULLS_UNEQUAL,NULLS_IGNORED';
    ),
    (
      Name: 'innodb_stats_on_metadata';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_persistent_sample_pages';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_sample_pages';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_stats_transient_sample_pages';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_strict_mode';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_support_xa';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_sync_array_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_sync_spin_loops';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_table_locks';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'innodb_thread_concurrency';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_thread_sleep_delay';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_undo_directory';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_undo_logs';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_undo_tablespaces';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_use_native_aio';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_use_sys_malloc';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_version';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'innodb_write_io_threads';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'insert_id';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'interactive_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'join_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'keep_files_on_create';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'key_buffer_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'key_cache_age_threshold';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'key_cache_block_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'key_cache_division_limit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'language';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'large_files_support';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'large_page_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'large_pages';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'last_insert_id';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'lc_messages';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'lc_messages_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'lc_time_names';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'license';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'local_infile';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'lock_wait_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'locked_in_memory';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_bin';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_bin_basename';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_error';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_output';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_queries_not_using_indexes';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_slave_updates';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_slow_queries';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_throttle_queries_not_using_indexes';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'log_warnings';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'long_query_time';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'low_priority_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'lower_case_file_system';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'lower_case_table_names';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'master_info_repository';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'master_verify_checksum';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_allowed_packet';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_binlog_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_binlog_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_binlog_stmt_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_connect_errors';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_connections';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_delayed_threads';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_error_count';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_heap_table_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_insert_delayed_threads';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_join_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_length_for_sort_data';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_prepared_stmt_count';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_relay_log_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'max_seeks_for_key';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_sort_length';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_sp_recursion_depth';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_tmp_tables';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_user_connections';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'max_write_lock_count';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'memlock';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'metadata_locks_cache_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_data_pointer_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_max_sort_file_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_mmap_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_recover_options';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'myisam_repair_threads';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'myisam_sort_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'myisam_stats_method';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'NULLS_EQUAL,NULLS_UNEQUAL,NULLS_IGNORED';
    ),
    (
      Name: 'myisam_use_mmap';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'named_pipe';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'net_buffer_length';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'net_read_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'net_retry_count';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'net_write_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'new';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'old';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'old_alter_table';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'old_passwords';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'open_files_limit';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'optimizer_join_cache_level';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_prune_level';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_search_depth';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_switch';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_features';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_max_mem_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'optimizer_trace_offset';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'have_partitioning';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_accounts_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_digests_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_stages_history_long_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_stages_history_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_statements_history_long_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_statements_history_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_waits_history_long_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_events_waits_history_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_hosts_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_cond_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_cond_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_file_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_file_handles';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_file_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_mutex_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_mutex_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_rwlock_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_rwlock_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_socket_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_socket_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_stage_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_statement_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_table_handles';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_table_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_thread_classes';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_max_thread_instances';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_setup_actors_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_setup_objects_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'performance_schema_users_size';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'pid_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'plugin_dir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'port';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'preload_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'profiling';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'profiling_history_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'protocol_version';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'proxy_user';
      IsDynamic: False;
      VarScope: vsSession;
    ),
    (
      Name: 'pseudo_thread_id';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'query_alloc_block_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'query_cache_limit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'query_cache_min_res_unit';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'query_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'query_cache_type';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: '0,1,2';
    ),
    (
      Name: 'query_cache_wlock_invalidate';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'query_prealloc_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'rand_seed1';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'rand_seed2';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'range_alloc_block_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'read_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'read_only';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'read_rnd_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'relay_log_basename';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_index';
      IsDynamic: False;
      VarScope: vsBoth;
    ),
    (
      Name: 'relay_log_index';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_info_file';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_info_repository';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_purge';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_recovery';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'relay_log_space_limit';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_host';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_password';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_port';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'report_user';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_enabled';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_trace_level';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_master_wait_no_slave';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_slave_enabled';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'rpl_semi_sync_slave_trace_level';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'secure_auth';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'secure_file_priv';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'server_id';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'server_uuid';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'shared_memory';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'shared_memory_base_name';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_external_locking';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_name_resolve';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_networking';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'skip_show_database';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_compressed_protocol';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_exec_mode';
      IsDynamic: True;
      VarScope: vsGlobal;
      EnumValues: 'IDEMPOTENT,STRICT';
    ),
    (
      Name: 'slave_load_tmpdir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_net_timeout';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_parallel_workers';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_skip_errors';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_sql_verify_checksum';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_transaction_retries';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slave_type_conversions';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slow_launch_time';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slow_query_log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'slow_query_log_file';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'socket';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sort_buffer_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_auto_is_null';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_big_selects';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_big_tables';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_buffer_result';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_log_bin';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_log_off';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_low_priority_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_max_join_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_mode';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_notes';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_quote_show_create';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_safe_updates';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_select_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'sql_slave_skip_counter';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sql_warnings';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'ssl_ca';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_capath';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_cert';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_cipher';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_crl';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_crlpath';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'ssl_key';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'storage_engine';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'FEDERATED,MRG_MYISAM,MyISAM,BLACKHOLE,CSV,MEMORY,ARCHIVE,InnoDB';
    ),
    (
      Name: 'stored_program_cache';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_binlog';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_frm';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_master_info';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_relay_log';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'sync_relay_log_info';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'system_time_zone';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'table_definition_cache';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'table_open_cache';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_cache_size';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_concurrency';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_handling';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'thread_stack';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'time_format';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'time_zone';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'timed_mutexes';
      IsDynamic: True;
      VarScope: vsGlobal;
    ),
    (
      Name: 'timestamp';
      IsDynamic: True;
      VarScope: vsSession;
    ),
    (
      Name: 'tmp_table_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'tmpdir';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'transaction_alloc_block_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'transaction_prealloc_size';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'tx_isolation';
      IsDynamic: True;
      VarScope: vsBoth;
      EnumValues: 'READ-UNCOMMITTED,READ-COMMITTED,REPEATABLE-READ,SERIALIZABLE';
    ),
    (
      Name: 'tx_read_only';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'unique_checks';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'updatable_views_with_limit';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'version';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'version_comment';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'version_compile_machine';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'version_compile_os';
      IsDynamic: False;
      VarScope: vsGlobal;
    ),
    (
      Name: 'wait_timeout';
      IsDynamic: True;
      VarScope: vsBoth;
    ),
    (
      Name: 'warning_count';
      IsDynamic: False;
      VarScope: vsSession;
    )

  );


implementation

uses apphelpers;


constructor TMySQLLib.Create(DllFile, DefaultDll: String);
begin
  inherited;
  // Some MYSQL_OPT_* constants were removed in MySQL 8.0, so the offsets differ between libmysql and libmariadb.
  MYSQL_OPT_CONNECT_TIMEOUT := 0;
  MYSQL_OPT_LOCAL_INFILE := 8;
  MARIADB_OPT_TLS_VERSION := 7005;
  if String(mysql_get_client_info).StartsWith('8.') then begin
    MYSQL_PLUGIN_DIR := 16;
    MYSQL_OPT_SSL_KEY := 19;
    MYSQL_OPT_SSL_CERT := 20;
    MYSQL_OPT_SSL_CA := 21;
    MYSQL_OPT_SSL_CIPHER := 23;
    MYSQL_OPT_CONNECT_ATTR_ADD := 27;
    MYSQL_ENABLE_CLEARTEXT_PLUGIN := 30;
    MYSQL_OPT_TLS_VERSION := 34;
  end
  else begin
    MYSQL_PLUGIN_DIR := 22;
    MYSQL_OPT_SSL_KEY := 25;
    MYSQL_OPT_SSL_CERT := 26;
    MYSQL_OPT_SSL_CA := 27;
    MYSQL_OPT_SSL_CIPHER := 29;
    MYSQL_OPT_CONNECT_ATTR_ADD := 33;
    MYSQL_ENABLE_CLEARTEXT_PLUGIN := 36;
    MYSQL_OPT_TLS_VERSION := 41;
  end;
end;

procedure TMySQLLib.AssignProcedures;
begin
  AssignProc(@mysql_affected_rows, 'mysql_affected_rows');
  AssignProc(@mysql_character_set_name, 'mysql_character_set_name');
  AssignProc(@mysql_close, 'mysql_close');
  AssignProc(@mysql_data_seek, 'mysql_data_seek');
  AssignProc(@mysql_errno, 'mysql_errno');
  AssignProc(@mysql_error, 'mysql_error');
  AssignProc(@mysql_fetch_field_direct, 'mysql_fetch_field_direct');
  AssignProc(@mysql_fetch_field, 'mysql_fetch_field');
  AssignProc(@mysql_fetch_lengths, 'mysql_fetch_lengths');
  AssignProc(@mysql_fetch_row, 'mysql_fetch_row');
  AssignProc(@mysql_free_result, 'mysql_free_result');
  AssignProc(@mysql_get_client_info, 'mysql_get_client_info');
  AssignProc(@mysql_get_server_info, 'mysql_get_server_info');
  AssignProc(@mysql_init, 'mysql_init');
  AssignProc(@mysql_num_fields, 'mysql_num_fields');
  AssignProc(@mysql_num_rows, 'mysql_num_rows');
  AssignProc(@mysql_ping, 'mysql_ping');
  AssignProc(@mysql_options, 'mysql_options');
  AssignProc(@mysql_optionsv, 'mysql_optionsv', False);
  AssignProc(@mysql_real_connect, 'mysql_real_connect');
  AssignProc(@mysql_real_query, 'mysql_real_query');
  AssignProc(@mysql_stat, 'mysql_stat');
  AssignProc(@mysql_store_result, 'mysql_store_result');
  AssignProc(@mysql_thread_id, 'mysql_thread_id');
  AssignProc(@mysql_next_result, 'mysql_next_result');
  AssignProc(@mysql_set_character_set, 'mysql_set_character_set');
  AssignProc(@mysql_thread_init, 'mysql_thread_init');
  AssignProc(@mysql_thread_end, 'mysql_thread_end');
  AssignProc(@mysql_warning_count, 'mysql_warning_count');
end;


initialization

// Keywords copied from SynHighligherSQL
MySQLKeywords := TStringList.Create;
MySQLKeywords.CommaText := 'ACCESSIBLE,ACCOUNT,ACTION,ACTIVE,ADD,ADMIN,AFTER,AGAINST,AGGREGATE,' +
    'ALGORITHM,ALL,ALTER,ALWAYS,ANALYSE,ANALYZE,AND,ANY,ARRAY,AS,ASC,' +
    'ASENSITIVE,AT,ATTRIBUTE,AUTHENTICATION,AUTOEXTEND_SIZE,AUTO_INCREMENT,' +
    'AVG_ROW_LENGTH,BACKUP,BEFORE,BEGIN,BETWEEN,BINLOG,BIT,BLOCK,BOTH,BUCKETS,' +
    'BULK,BY,CACHE,CALL,CASCADE,CASCADED,CATALOG_NAME,CHAIN,CHALLENGE_RESPONSE,' +
    'CHANGE,CHANGED,CHANNEL,CHARACTER,CHARSET,CHECK,CHECKSUM,CIPHER,' +
    'CLASS_ORIGIN,CLIENT,CLONE,CODE,COLLATE,COLLATION,COLUMN,COLUMNS,' +
    'COLUMN_FORMAT,COLUMN_NAME,COMMENT,COMMIT,COMMITTED,COMPLETION,COMPONENT,' +
    'COMPRESSION,CONCURRENT,CONDITION,CONNECTION,CONSISTENT,CONSTRAINT,' +
    'CONSTRAINT_CATALOG,CONSTRAINT_NAME,CONSTRAINT_SCHEMA,CONTAINS,CONTEXT,' +
    'CONTINUE,CONVERT,CPU,CREATE,CROSS,CUBE,CUME_DIST,CURRENT,CURSOR,' +
    'CURSOR_NAME,DATA,DATABASE,DATABASES,DATAFILE,DAY_HOUR,DAY_MICROSECOND,' +
    'DAY_MINUTE,DAY_SECOND,DEALLOCATE,DEC,DECLARE,DEFAULT,DEFAULT_AUTH,DEFINER,' +
    'DEFINITION,DELAYED,DELAY_KEY_WRITE,DELETE,DENSE_RANK,DESC,DESCRIBE,' +
    'DESCRIPTION,DES_KEY_FILE,DETERMINISTIC,DIAGNOSTICS,DIRECTORY,DISABLE,' +
    'DISCARD,DISTINCT,DISTINCTROW,DIV,DO,DROP,DUAL,DUMPFILE,DUPLICATE,EACH,' +
    'ELSE,ELSEIF,EMPTY,ENABLE,ENCLOSED,ENCRYPTION,END,ENDS,ENFORCED,ENGINE,' +
    'ENGINES,ENGINE_ATTRIBUTE,ERROR,ERRORS,ESCAPE,ESCAPED,EVENT,EVENTS,EVERY,' +
    'EXCEPT,EXCHANGE,EXCLUDE,EXECUTE,EXISTS,EXPANSION,EXPIRE,EXPLAIN,EXPORT,' +
    'EXTENDED,EXTENT_SIZE,FACTOR,FAILED_LOGIN_ATTEMPTS,FALSE,FAST,FAULTS,' +
    'FIELDS,FILE,FILE_BLOCK_SIZE,FILTER,FINISH,FIRST,FIRST_VALUE,FLOAT4,FLOAT8,' +
    'FLUSH,FOLLOWING,FOLLOWS,FOR,FORCE,FOREIGN,FOUND,FROM,FULL,FULLTEXT,' +
    'FUNCTION,GENERAL,GENERATE,GENERATED,GEOMCOLLECTION,GET,' +
    'GET_MASTER_PUBLIC_KEY,GET_SOURCE_PUBLIC_KEY,GLOBAL,GRANT,GRANTS,GROUP,' +
    'GROUPING,GROUPS,GROUP_REPLICATION,GTID_ONLY,HAVING,HELP,HIGH_PRIORITY,' +
    'HISTOGRAM,HISTORY,HOST,HOSTS,HOUR_MICROSECOND,HOUR_MINUTE,HOUR_SECOND,' +
    'IDENTIFIED,IGNORE,IGNORE_SERVER_IDS,IMPORT,IN,INACTIVE,INDEX,INDEXES,' +
    'INFILE,INITIAL,INITIAL_SIZE,INITIATE,INNER,INOUT,INSENSITIVE,INSERT,' +
    'INSERT_METHOD,INSTALL,INSTANCE,INT1,INT2,INT3,INT4,INT8,INTERSECT,INTO,' +
    'INVISIBLE,INVOKER,IO,IO_AFTER_GTIDS,IO_BEFORE_GTIDS,IO_THREAD,IPC,IS,' +
    'ISOLATION,ISSUER,JOIN,JSON,JSON_TABLE,JSON_VALUE,KEY,KEYRING,KEYS,' +
    'KEY_BLOCK_SIZE,KILL,LAG,LANGUAGE,LAST,LAST_VALUE,LATERAL,LEAD,LEADING,' +
    'LEAVES,LESS,LEVEL,LIKE,LIMIT,LINEAR,LINES,LIST,LOAD,LOCAL,LOCK,LOCKED,' +
    'LOCKS,LOGFILE,LOGS,LONG,LOW_PRIORITY,MASTER,MASTER_AUTO_POSITION,' +
    'MASTER_BIND,MASTER_COMPRESSION_ALGORITHMS,MASTER_CONNECT_RETRY,' +
    'MASTER_DELAY,MASTER_HEARTBEAT_PERIOD,MASTER_HOST,MASTER_LOG_FILE,' +
    'MASTER_LOG_POS,MASTER_PASSWORD,MASTER_PORT,MASTER_PUBLIC_KEY_PATH,' +
    'MASTER_RETRY_COUNT,MASTER_SERVER_ID,MASTER_SSL,MASTER_SSL_CA,' +
    'MASTER_SSL_CAPATH,MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_CRL,' +
    'MASTER_SSL_CRLPATH,MASTER_SSL_KEY,MASTER_SSL_VERIFY_SERVER_CERT,' +
    'MASTER_TLS_CIPHERSUITES,MASTER_TLS_VERSION,MASTER_USER,' +
    'MASTER_ZSTD_COMPRESSION_LEVEL,MATCH,MAXVALUE,MAX_CONNECTIONS_PER_HOUR,' +
    'MAX_QUERIES_PER_HOUR,MAX_ROWS,MAX_SIZE,MAX_UPDATES_PER_HOUR,' +
    'MAX_USER_CONNECTIONS,MEDIUM,MEMBER,MESSAGE_TEXT,MIDDLEINT,MIGRATE,' +
    'MINUTE_MICROSECOND,MINUTE_SECOND,MIN_ROWS,MOD,MODE,MODIFIES,MODIFY,MUTEX,' +
    'MYSQL_ERRNO,NAME,NAMES,NATURAL,NCHAR,NESTED,NETWORK_NAMESPACE,NEVER,NEW,' +
    'NEXT,NO,NODEGROUP,NONE,NOT,NOWAIT,NO_WAIT,NO_WRITE_TO_BINLOG,NTH_VALUE,' +
    'NTILE,NULL,NULLS,NUMBER,NVARCHAR,OF,OFF,OFFSET,OJ,OLD,ON,ONE,ONLY,OPEN,' +
    'OPTIMIZE,OPTIMIZER_COSTS,OPTION,OPTIONAL,OPTIONALLY,OPTIONS,OR,ORDER,' +
    'ORDINALITY,ORGANIZATION,OTHERS,OUT,OUTER,OUTFILE,OVER,OWNER,PACK_KEYS,' +
    'PAGE,PARSER,PARSE_GCOL_EXPR,PARTIAL,PARTITION,PARTITIONING,PARTITIONS,' +
    'PASSWORD_LOCK_TIME,PATH,PERCENT_RANK,PERSIST,PERSIST_ONLY,PHASE,PLUGIN,' +
    'PLUGINS,PLUGIN_DIR,PORT,PRECEDES,PRECEDING,PREPARE,PRESERVE,PREV,PRIMARY,' +
    'PRIVILEGES,PRIVILEGE_CHECKS_USER,PROCEDURE,PROCESS,PROCESSLIST,PROFILE,' +
    'PROFILES,PROXY,PURGE,QUERY,QUICK,RANDOM,RANGE,RANK,READ,READS,READ_ONLY,' +
    'READ_WRITE,REBUILD,RECOVER,RECURSIVE,REDOFILE,REDO_BUFFER_SIZE,REFERENCE,' +
    'REFERENCES,REGEXP,REGISTRATION,RELAY,RELAYLOG,RELAY_LOG_FILE,' +
    'RELAY_LOG_POS,RELAY_THREAD,RELEASE,RELOAD,REMOTE,REMOVE,RENAME,REORGANIZE,' +
    'REPAIR,REPEATABLE,REPLACE,REPLICA,REPLICAS,REPLICATE_DO_DB,' +
    'REPLICATE_DO_TABLE,REPLICATE_IGNORE_DB,REPLICATE_IGNORE_TABLE,' +
    'REPLICATE_REWRITE_DB,REPLICATE_WILD_DO_TABLE,REPLICATE_WILD_IGNORE_TABLE,' +
    'REPLICATION,REQUIRE,REQUIRE_ROW_FORMAT,RESET,RESIGNAL,RESOURCE,RESPECT,' +
    'RESTART,RESTORE,RESTRICT,RESUME,RETAIN,RETURN,RETURNED_SQLSTATE,RETURNING,' +
    'RETURNS,REUSE,REVOKE,RLIKE,ROLE,ROLLBACK,ROLLUP,ROTATE,ROUTINE,ROW,ROWS,' +
    'ROW_FORMAT,ROW_NUMBER,RTREE,SAVEPOINT,SCHEDULE,SCHEMA,SCHEMAS,SCHEMA_NAME,' +
    'SECONDARY,SECONDARY_ENGINE,SECONDARY_ENGINE_ATTRIBUTE,SECONDARY_LOAD,' +
    'SECONDARY_UNLOAD,SECOND_MICROSECOND,SECURITY,SELECT,SENSITIVE,SEPARATOR,' +
    'SERIALIZABLE,SERVER,SESSION,SET,SHARE,SHOW,SHUTDOWN,SIGNAL,SIMPLE,SKIP,' +
    'SLAVE,SLOW,SNAPSHOT,SOCKET,SOME,SONAME,SOUNDS,SOURCE,SOURCE_AUTO_POSITION,' +
    'SOURCE_BIND,SOURCE_COMPRESSION_ALGORITHMS,SOURCE_CONNECT_RETRY,' +
    'SOURCE_DELAY,SOURCE_HEARTBEAT_PERIOD,SOURCE_HOST,SOURCE_LOG_FILE,' +
    'SOURCE_LOG_POS,SOURCE_PASSWORD,SOURCE_PORT,SOURCE_PUBLIC_KEY_PATH,' +
    'SOURCE_RETRY_COUNT,SOURCE_SSL,SOURCE_SSL_CA,SOURCE_SSL_CAPATH,' +
    'SOURCE_SSL_CERT,SOURCE_SSL_CIPHER,SOURCE_SSL_CRL,SOURCE_SSL_CRLPATH,' +
    'SOURCE_SSL_KEY,SOURCE_SSL_VERIFY_SERVER_CERT,SOURCE_TLS_CIPHERSUITES,' +
    'SOURCE_TLS_VERSION,SOURCE_USER,SOURCE_ZSTD_COMPRESSION_LEVEL,SPATIAL,' +
    'SPECIFIC,SQL,SQLEXCEPTION,SQLSTATE,SQLWARNING,SQL_AFTER_GTIDS,' +
    'SQL_AFTER_MTS_GAPS,SQL_BEFORE_GTIDS,SQL_BIG_RESULT,SQL_BUFFER_RESULT,' +
    'SQL_CACHE,SQL_CALC_FOUND_ROWS,SQL_NO_CACHE,SQL_SMALL_RESULT,SQL_THREAD,' +
    'SQL_TSI_DAY,SQL_TSI_HOUR,SQL_TSI_MINUTE,SQL_TSI_MONTH,SQL_TSI_QUARTER,' +
    'SQL_TSI_SECOND,SQL_TSI_WEEK,SQL_TSI_YEAR,SSL,STACKED,START,STARTING,' +
    'STARTS,STATS_AUTO_RECALC,STATS_PERSISTENT,STATS_SAMPLE_PAGES,STATUS,STOP,' +
    'STORAGE,STORED,STRAIGHT_JOIN,STREAM,SUBCLASS_ORIGIN,SUBJECT,SUBPARTITION,' +
    'SUBPARTITIONS,SUPER,SUSPEND,SWAPS,SWITCHES,SYSTEM,TABLE,TABLES,TABLESPACE,' +
    'TABLE_CHECKSUM,TABLE_NAME,TEMPORARY,TERMINATED,THAN,THREAD_PRIORITY,TIES,' +
    'TLS,TO,TRAILING,TRANSACTION,TRIGGER,TRIGGERS,TRUE,TYPE,TYPES,UNBOUNDED,' +
    'UNCOMMITTED,UNDO,UNDOFILE,UNDO_BUFFER_SIZE,UNINSTALL,UNION,UNIQUE,UNKNOWN,' +
    'UNLOCK,UNREGISTER,UPDATE,UPGRADE,URL,USAGE,USE,USER_RESOURCES,USE_FRM,' +
    'USING,VALIDATION,VALUE,VALUES,VARCHARACTER,VARIABLES,VARYING,VCPU,VIEW,' +
    'VIRTUAL,VISIBLE,WAIT,WARNINGS,WHERE,WINDOW,WITH,WITHOUT,WORK,WRAPPER,' +
    'WRITE,X509,XA,XID,XML,XOR,YEAR_MONTH,ZONE,'
  // SQL Plus commands:
  + 'CLOSE,CONDITION,CONTINUE,CURSOR,DECLARE,DO,EXIT,FETCH,FOUND,GOTO,' +
  'HANDLER,ITERATE,LANGUAGE,LEAVE,LOOP,UNTIL,WHILE';

// Error codes copied from perror.exe
MySQLErrorCodes := Explode(',', '0=No error,'+
  '1=Operation not permitted,'+
  '2=No such file or directory,'+
  '3=No such process,'+
  '4=Interrupted function call,'+
  '5=Input/output error,'+
  '6=No such device or address,'+
  '7=Arg list too long,'+
  '8=Exec format error,'+
  '9=Bad file descriptor,'+
  '10=No child processes,'+
  '11=Resource temporarily unavailable,'+
  '12=Not enough space,'+
  '13=Permission denied,'+
  '14=Bad address,'+
  '16=Resource device,'+
  '17=File exists,'+
  '18=Improper link,'+
  '19=No such device,'+
  '20=Not a directory,'+
  '21=Is a directory,'+
  '22=Invalid argument,'+
  '23=Too many open files in system,'+
  '24=Too many open files,'+
  '25=Inappropriate I/O control operation,'+
  '27=File too large,'+
  '28=No space left on device,'+
  '29=Invalid seek,'+
  '30=Read-only file system,'+
  '31=Too many links,'+
  '32=Broken pipe,'+
  '33=Domain error,'+
  '34=Result too large,'+
  '36=Resource deadlock avoided,'+
  '38=Filename too long,'+
  '39=No locks available,'+
  '40=Function not implemented,'+
  '41=Directory not empty,'+
  '42=Illegal byte sequence,'+
  '120=Didn''t find key on read or update,'+
  '121=Duplicate key on write or update,'+
  '123=Someone has changed the row since it was read (while the table was locked to prevent it),'+
  '124=Wrong index given to function,'+
  '126=Index file is crashed,'+
  '127=Record-file is crashed,'+
  '128=Out of memory,'+
  '130=Incorrect file format,'+
  '131=Command not supported by database,'+
  '132=Old database file,'+
  '133=No record read before update,'+
  '134=Record was already deleted (or record file crashed),'+
  '135=No more room in record file,'+
  '136=No more room in index file,'+
  '137=No more records (read after end of file),'+
  '138=Unsupported extension used for table,'+
  '139=Too big row,'+
  '140=Wrong create options,'+
  '141=Duplicate unique key or constraint on write or update,'+
  '142=Unknown character set used,'+
  '143=Conflicting table definitions in sub-tables of MERGE table,'+
  '144=Table is crashed and last repair failed,'+
  '145=Table was marked as crashed and should be repaired,'+
  '146=Lock timed out; Retry transaction,'+
  '147=Lock table is full;  Restart program with a larger locktable,'+
  '148=Updates are not allowed under a read only transactions,'+
  '149=Lock deadlock; Retry transaction,'+
  '150=Foreign key constraint is incorrectly formed,'+
  '151=Cannot add a child row,'+
  '152=Cannot delete a parent row');



end.
