// Common constants
const

  // Line breaks
  CRLF = #13#10;
  LB_UNIX = #10;
  LB_MAC = #13;
  LB_WIDE = WideChar($2027);

  // Placeholder text for NULL values
  TEXT_NULL = '(NULL)';

  // General things
  APPNAME = 'HeidiSQL';
  APPDOMAIN = 'https://www.heidisql.com/';
  REGKEY_SESSIONS = 'Servers';
  REGKEY_QUERYHISTORY = 'QueryHistory';
  REGKEY_RECENTFILTERS = 'RecentFilters';
  // Some unique char, used to separate e.g. selected columns in registry
  DELIM = '|';
  CHR10REPLACEMENT = '<}}}>';
  CHR13REPLACEMENT = '<{{{>';
  DELIMITER = '<|||>';

  // Used by maskSQL and fixSQL:
  SQL_VERSION_ANSI = -1;

  COLORSHIFT_NULLFIELDS = 70; // Brightness adjustment to add to normal field colors for NULL values
  COLORSHIFT_SORTCOLUMNS = 12; // Brightness adjustment to add to sorted column backgrounds

  // Various iconindexes
  ICONINDEX_PRIMARYKEY = 25;
  ICONINDEX_FIELD = 42;
  ICONINDEX_INDEXKEY = 23;
  ICONINDEX_UNIQUEKEY = 24;
  ICONINDEX_FULLTEXTKEY = 22;
  ICONINDEX_SPATIALKEY = 126;
  ICONINDEX_FOREIGNKEY = 136;
  ICONINDEX_SERVER = 36;
  ICONINDEX_DB = 5;
  ICONINDEX_HIGHLIGHTMARKER = 157;
  ICONINDEX_TABLE = 14;
  ICONINDEX_VIEW = 81;
  ICONINDEX_STOREDPROCEDURE = 119;
  ICONINDEX_STOREDFUNCTION = 35;
  ICONINDEX_TRIGGER = 137;
  ICONINDEX_FUNCTION = 13;
  ICONINDEX_EVENT = 80;
  ICONINDEX_KEYWORD = 25;

  // Size of byte units
  {Kibibyte} SIZE_KB = 1024;
  {Mebibyte} SIZE_MB = 1048576;
  {Gibibyte} SIZE_GB = 1073741824;
  {Tebibyte} SIZE_TB = 1099511627776;
  {Pebibyte} SIZE_PB = 1125899906842624;
  {Exbibyte} SIZE_EB = 1152921504606846976;

  // Size of byte units for formatting purposes
  {Kibibyte} FSIZE_KB = 1000;
  {Mebibyte} FSIZE_MB = 1024000;
  {Gibibyte} FSIZE_GB = 1048576000;
  {Tebibyte} FSIZE_TB = 1073741824000;
  {Pebibyte} FSIZE_PB = 1099511627776000;
  {Exbibyte} FSIZE_EB = 1125899906842624000;

  // Abbreviations of byte unit names
  {Bytes} NAME_BYTES = ' B';
  {Kibibyte} NAME_KB = ' KiB';
  {Mebibyte} NAME_MB = ' MiB';
  {Gibibyte} NAME_GB = ' GiB';
  {Tebibyte} NAME_TB = ' TiB';
  {Pebibyte} NAME_PB = ' PiB';
  {Exbibyte} NAME_EB = ' EiB';

  // Used by ListViews and Grids
  ORDER_ASC = 0;            // Used for tag-value of "Direction"-button
  ORDER_DESC = 1;           // dito
  TXT_ASC = 'ASC';          // Used for caption of "Direction"-button
  TXT_DESC = 'DESC';        // dito

  // Data grid: How many bytes to fetch from data fields that are potentially large.
  GRIDMAXDATA: Integer = 256;

  VTREE_NOTLOADED = 0;
  VTREE_NOTLOADED_PURGECACHE = 1;
  VTREE_LOADED = 2;

  // Modification indicator for TControl.Tag
  MODIFIEDFLAG = 10;

  SUnhandledNodeIndex = 'Unhandled tree node index';
  MSG_NOGRIDEDITING = 'Selected columns don''t contain a sufficient set of key columns to allow editing. Please select primary or unique key columns, or just all columns.';
  SIdle = 'Idle.';
  SUnsupported = 'Unsupported by this server';
  SUnsupportedSettingsDatatype = 'Unsupported datatype for setting "%s"';
  SNotImplemented = 'Method not implemented for this connection type';
  MsgSQLError: String = 'SQL Error (%d): %s';
  MsgSQLErrorMultiStatements: String = 'SQL Error (%d) in statement #%d: %s';
  MsgUnhandledNetType: String = 'Unhandled connection type (%d)';
  MsgDisconnect: String = 'Connection to %s closed at %s';
  MsgInvalidColumn: String = 'Column #%d not available. Query returned %d columns and %d rows.';
  SPortableLockFile: String = 'portable.lock';

  PKEY = 'PRIMARY';
  KEY = 'KEY';
  UKEY = 'UNIQUE';
  FKEY = 'FULLTEXT';
  SKEY = 'SPATIAL';

  SYNCOMPLETION_PATTERN = '\image{%d}\hspace{5}\color{clGrayText}%s\column{}\color{clWindowText}%s\color{clGrayText}%s';

  HELPERNODE_COLUMNS = 0;
  HELPERNODE_FUNCTIONS = 1;
  HELPERNODE_KEYWORDS = 2;
  HELPERNODE_SNIPPETS = 3;
  HELPERNODE_HISTORY = 4;
  HELPERNODE_PROFILE = 5;
  HELPERNODE_BINDING = 6;

  // A set of characters which need not to be quoted
  IDENTCHARS = ['A'..'Z', 'a'..'z', '0'..'9', '_'];

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
  NUM_FLAG = 32768;
  PART_KEY_FLAG = 16384;
  GROUP_FLAG = 32768;
  UNIQUE_FLAG = 65536;
  BINCMP_FLAG = 131072;

  // Client connection options
  CLIENT_LONG_PASSWORD = 1;
  CLIENT_FOUND_ROWS = 2; // Found instead of affected rows
  CLIENT_LONG_FLAG = 4;
  CLIENT_CONNECT_WITH_DB = 8;
  CLIENT_NO_SCHEMA = 16; // Don't allow database.table.column
  CLIENT_COMPRESS = 32;
  CLIENT_ODBC = 64;
  CLIENT_LOCAL_FILES = 128;
  CLIENT_IGNORE_SPACE = 256; // Ignore spaces before '('
  CLIENT_PROTOCOL_41 = 512;
  CLIENT_INTERACTIVE = 1024;
  CLIENT_SSL = 2048; // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE = 4096;
  CLIENT_TRANSACTIONS = 8192;
  CLIENT_RESERVED = 16384;
  CLIENT_SECURE_CONNECTION = 32768;
  CLIENT_MULTI_STATEMENTS = 65536;
  CLIENT_MULTI_RESULTS = 131072;
  CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS = 4194304;
  CLIENT_SSL_VERIFY_SERVER_CERT = 67108864;
  CLIENT_REMEMBER_OPTIONS = 134217728;

  COLLATION_BINARY = 63;
  // Equivalent to COLLATION_BINARY, this is what a new driver returns when connected to a pre-4.1 server.
  COLLATION_NONE =  0;

