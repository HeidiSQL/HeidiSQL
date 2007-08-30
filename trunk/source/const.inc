// Common constants
const

  // Carriage return / Line feed
  CRLF = #13#10;

  // Names of the system tables and system databases
  PRIVTABLE_USERS           = 'user';
  PRIVTABLE_DB              = 'db';
  PRIVTABLE_TABLES          = 'tables_priv';
  PRIVTABLE_COLUMNS         = 'columns_priv';
  DBNAME_INFORMATION_SCHEMA = 'information_schema';
  DBNAME_MYSQL              = 'mysql';
  LOCAL_HOST                = '127.0.0.1';
  MYSQL_PORT                = 3306;

  // Related field things
  TBLTYPE_AUTOMATIC: String = '<Automatic>';
  TEMPFIELDNAME             = 'temp_fieldname';

  // General things
  APPNAME          = 'HeidiSQL';
  REGPATH          = 'Software\' + APPNAME;
  STATUS_MSG_READY = 'Ready.';
  STR_NOTSUPPORTED = 'Not supported by this server';
  // Used by ListViews
  COLOR_SORTCOLUMN = $00F7F7F7;

  // Used by maskSQL and fixSQL:
  SQL_VERSION_ANSI = -1;

  // Used for simulating a TTreeNode which has subnodes
  DUMMY_NODE_TEXT : String = 'Dummy node, should never be visible';

  // Used for SQL Log display limit (prevents color display errors)
  SQLLOG_CHAR_LIMIT = 2000;

  // Registry name for storing list of displayed columns
  REGNAME_DISPLAYEDCOLUMNS = 'DisplayedColumns';
  REGNAME_SORTDISPLAYEDCOLUMNS = 'DisplayedColumnsSorted';
  REGNAME_LISTTABLESCOLUMNNAMES = 'ListTablesColumnNames';

  // how much memory we're aiming to use for the
  // data grid and it's automatic limit function
  // this value should probably be user configurable
  LOAD_SIZE = 5*1024*1024;

  // Various iconindexes
  ICONINDEX_PRIMARYKEY = 26;
  ICONINDEX_FIELD = 62;
  ICONINDEX_INDEXKEY = 63;
  ICONINDEX_UNIQUEKEY = 64;
  ICONINDEX_FULLTEXTKEY = 65;

