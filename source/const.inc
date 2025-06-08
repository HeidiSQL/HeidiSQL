// Common constants
const

  // Line breaks
  // TODO: use sLineBreak instead
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

  COLORSHIFT_NULLFIELDS = 70; // Brightness adjustment to add to normal field colors for NULL values
  COLORSHIFT_SORTCOLUMNS = 12; // Brightness adjustment to add to sorted column backgrounds

  // Various iconindexes
  ICONINDEX_PRIMARYKEY = 25;
  ICONINDEX_FIELD = 42;
  ICONINDEX_INDEXKEY = 23;
  ICONINDEX_UNIQUEKEY = 24;
  ICONINDEX_FULLTEXTKEY = 22;
  ICONINDEX_SPATIALKEY = 126;
  ICONINDEX_VECTORKEY = 207;
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
  {Kibibyte} SIZE_KB = Int64(1024);
  {Mebibyte} SIZE_MB = Int64(1048576);
  {Gibibyte} SIZE_GB = Int64(1073741824);
  {Tebibyte} SIZE_TB = Int64(1099511627776);
  {Pebibyte} SIZE_PB = Int64(1125899906842624);
  {Exbibyte} SIZE_EB = Int64(1152921504606846976);

  // Size of byte units for formatting purposes
  {Kibibyte} FSIZE_KB = Int64(1000);
  {Mebibyte} FSIZE_MB = Int64(1024000);
  {Gibibyte} FSIZE_GB = Int64(1048576000);
  {Tebibyte} FSIZE_TB = Int64(1073741824000);
  {Pebibyte} FSIZE_PB = Int64(1099511627776000);
  {Exbibyte} FSIZE_EB = Int64(1125899906842624000);

  // Abbreviations of byte unit names
  {Bytes} NAME_BYTES = ' B';
  {Kibibyte} NAME_KB = ' KiB';
  {Mebibyte} NAME_MB = ' MiB';
  {Gibibyte} NAME_GB = ' GiB';
  {Tebibyte} NAME_TB = ' TiB';
  {Pebibyte} NAME_PB = ' PiB';
  {Exbibyte} NAME_EB = ' EiB';

  // Data grid: How many bytes to fetch from data fields that are potentially large.
  GRIDMAXDATA: Int64 = 256;

  BACKUP_MAXFILESIZE = 10 * SIZE_MB;
  BACKUP_FILEPATTERN: String = 'query-tab-%s.sql';

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
  MsgUnhandledControl: String = 'Unhandled control in %s';
  MsgDisconnect: String = 'Connection to %s closed at %s';
  MsgInvalidColumn: String = 'Column #%d not available. Query returned %d columns and %d rows.';
  FILEFILTER_SQLITEDB = '*.sqlite3;*.sqlite;*.db;*.s3db';
  FILEEXT_SQLITEDB = 'sqlite3';
  PROPOSAL_ITEM_HEIGHT = 18;
  // Note the following should be in sync to what MySQL returns from SHOW WARNINGS
  SLogPrefixWarning = 'Warning';
  SLogPrefixNote = 'Note';
  SLogPrefixInfo = 'Info';

