// Common constants
const

  // Carriage return / Line feed
  CRLF = #13#10;

  // Names of the system tables and system databases
  PRIVTABLE_USERS           = 'user';
  PRIVTABLE_DB              = 'db';
  PRIVTABLE_HOST            = 'host';
  PRIVTABLE_TABLES          = 'tables_priv';
  PRIVTABLE_COLUMNS         = 'columns_priv';
  DBNAME_INFORMATION_SCHEMA = 'information_schema';
  DBNAME_MYSQL              = 'mysql';

  // Related field things
  TEMPFIELDNAME             = 'temp_fieldname';

  // Placeholder text for NULL values
  TEXT_NULL                 = '(NULL)';

  // General things
  APPNAME          = 'HeidiSQL';
  APPDOMAIN        = 'http://www.heidisql.com/';
  REGPATH          = '\Software\' + APPNAME + '\';
  REGKEY_SESSIONS  = 'Servers\';
  STATUS_MSG_READY = 'Ready.';
  STR_NOTSUPPORTED = 'Not supported by this server';

  // Used by maskSQL and fixSQL:
  SQL_VERSION_ANSI = -1;

  // Used for simulating a TTreeNode which has subnodes
  DUMMY_NODE_TEXT : String = 'Dummy node, should never be visible';

  // Various names of registry variables
  // User-changable variables have a default value
  // Note: Color values are in HEX format: $00BBGGRR
  REGNAME_DISPLAYEDCOLUMNS = 'DisplayedColumns';
  REGNAME_SORTDISPLAYEDCOLUMNS = 'DisplayedColumnsSorted';
  REGNAME_LISTTABLESCOLUMNNAMES = 'ListTablesColumnNames';
  REGNAME_LASTSESSION = 'lastcon';
  REGNAME_AUTORECONNECT = 'AutoReconnect';
  DEFAULT_AUTORECONNECT = False;
  REGNAME_RESTORELASTUSEDDB = 'RestoreLastUsedDB';
  DEFAULT_RESTORELASTUSEDDB = True;
  REGNAME_LASTUSEDDB = 'lastUsedDB';
  REGNAME_FONTNAME = 'FontName';
  DEFAULT_FONTNAME = 'Courier New';
  REGNAME_FONTSIZE = 'FontSize';
  DEFAULT_FONTSIZE = 9;
  REGNAME_DATAFONTNAME = 'DataFontName';
  DEFAULT_DATAFONTNAME = 'Tahoma';
  REGNAME_DATAFONTSIZE = 'DataFontSize';
  DEFAULT_DATAFONTSIZE = 8;
  REGNAME_DATALIMITSTART = 'DataLimitStart';
  DEFAULT_DATALIMITSTART = 0;
  REGNAME_DATALIMITEND = 'DataLimitEnd';
  DEFAULT_DATALIMITEND = 5000;
  // how much memory we're aiming to use for the
  // data grid and it's automatic limit function
  REGNAME_LOGSQLNUM = 'logsqlnum';
  DEFAULT_LOGSQLNUM = 300;
  REGNAME_LOGSQLWIDTH = 'logsqlwidth';
  DEFAULT_LOGSQLWIDTH = 2000;
  REGNAME_SQLCOLKEYATTRI = 'SQLColKeyAttri';
  DEFAULT_SQLCOLKEYATTRI = $00FF0000; // clBlue;
  REGNAME_SQLCOLFUNCTIONATTRI = 'SQLColFunctionAttri';
  DEFAULT_SQLCOLFUNCTIONATTRI = $00800000; // clNavy
  REGNAME_SQLCOLDATATYPEATTRI = 'SQLColDataTypeAttri';
  DEFAULT_SQLCOLDATATYPEATTRI = $00000080; // clMaroon;
  REGNAME_SQLCOLNUMBERATTRI = 'SQLColNumberAttri';
  DEFAULT_SQLCOLNUMBERATTRI = $00800080; // clPurple;
  REGNAME_SQLCOLSTRINGATTRI = 'SQLColStringAttri';
  DEFAULT_SQLCOLSTRINGATTRI = $00008000; // clGreen
  REGNAME_SQLCOLCOMMENTATTRI = 'SQLColCommentAttri';
  DEFAULT_SQLCOLCOMMENTATTRI = $00808080; // clGray
  REGNAME_SQLCOLTABLENAMEATTRI = 'SQLColTablenameAttri';
  DEFAULT_SQLCOLTABLENAMEATTRI = $00FF00FF; // clFuchsia
  REGNAME_SQLCOLACTIVELINE = 'SQLColActiveLine';
  DEFAULT_SQLCOLACTIVELINE = $00FFFFFF; // clWhite
  REGNAME_MAXCOLWIDTH = 'MaxColWidth';
  DEFAULT_MAXCOLWIDTH = 300;
  REGNAME_REMEMBERFILTERS = 'RememberFilters';
  DEFAULT_REMEMBERFILTERS = True;
  REGNAME_LOGTOFILE = 'LogToFile';
  DEFAULT_LOGTOFILE = False;
  REGNAME_WINDOWSTATE = 'windowstate';
  REGNAME_WINDOWLEFT = 'windowleft';
  REGNAME_WINDOWTOP = 'windowtop';
  REGNAME_WINDOWWIDTH = 'windowwidth';
  REGNAME_WINDOWHEIGHT = 'windowheight';
  REGNAME_TOOLBAR2LEFT = 'ToolBar2Left';
  REGNAME_TOOLBAR2TOP = 'ToolBar2Top';
  REGNAME_TOOLBARDBLEFT = 'ToolBarDatabaseLeft';
  REGNAME_TOOLBARDBTOP = 'ToolBarDatabaseTop';
  REGNAME_TOOLBARTABLELEFT = 'ToolBarTableLeft';
  REGNAME_TOOLBARTABLETOP = 'ToolBarTableTop';
  REGNAME_TOOLBARDATALEFT = 'ToolBarDataLeft';
  REGNAME_TOOLBARDATATOP = 'ToolBarDataTop';
  REGNAME_TOOLBARQUERYLEFT = 'ToolBarQueryLeft';
  REGNAME_TOOLBARQUERYTOP = 'ToolBarQueryTop';
  REGNAME_QUERYMEMOHEIGHT = 'querymemoheight';
  REGNAME_DBTREEWIDTH = 'dbtreewidth';
  REGNAME_SQLOUTHEIGHT = 'sqloutheight';
  REGNAME_QUERYHELPERSWIDTH = 'queryhelperswidth';
  REGNAME_EDITOR_WIDTH = 'MemoEditorWidth';
  DEFAULT_EDITOR_WIDTH = 100;
  REGNAME_EDITOR_HEIGHT = 'MemoEditorHeight';
  DEFAULT_EDITOR_HEIGHT = 100;
  REGNAME_SQLWHEREFILE = 'SQLWhereFile';
  REGNAME_DELIMITER = 'Delimiter';
  DEFAULT_DELIMITER = ';';
  REGNAME_SQLHELPWINLEFT = 'SQLHelp_WindowLeft';
  REGNAME_SQLHELPWINTOP = 'SQLHelp_WindowTop';
  REGNAME_SQLHELPWINWIDTH = 'SQLHelp_WindowWidth';
  REGNAME_SQLHELPWINHEIGHT = 'SQLHelp_WindowHeight';
  REGNAME_SQLHELPPLWIDTH = 'SQLHelp_PnlLeftWidth';
  REGNAME_SQLHELPPRHEIGHT = 'SQLHelp_PnlRightTopHeight';
  REGNAME_HOST = 'Host';
  DEFAULT_HOST = '127.0.0.1';
  REGNAME_USER = 'User';
  DEFAULT_USER = 'root';
  REGNAME_PASSWORD = 'Password';
  DEFAULT_PASSWORD = '';
  REGNAME_PORT = 'Port';
  DEFAULT_PORT = 3306;
  REGNAME_TIMEOUT = 'Timeout';
  DEFAULT_TIMEOUT = 30;
  REGNAME_COMPRESSED = 'Compressed';
  DEFAULT_COMPRESSED = False;
  REGNAME_ONLYDBS = 'OnlyDBs';
  REGNAME_ONLYDBSSORTED = 'OnlyDBsSorted';
  DEFAULT_ONLYDBSSORTED = False;
  REGNAME_EXP_STRUCTURE = 'ExportStructure';
  REGNAME_EXP_CREATEDB = 'WithCreateDatabase';
  REGNAME_EXP_CREATETABLE = 'WithCreateTable';
  REGNAME_EXP_DATA = 'ExportData';
  REGNAME_EXP_DBHOW = 'CreateDatabaseHow';
  REGNAME_EXP_TABLESHOW = 'CreateTablesHow';
  REGNAME_EXP_DATAHOW = 'CreateDataHow';
  REGNAME_EXP_COMPAT = 'Compatibility';
  REGNAME_EXP_OUTFILE = 'exportfilename';
  REGNAME_EXP_OUTDIR = 'ExportDirectory';
  REGNAME_EXP_TARGET = 'ExportSQL_OutputTo';
  REGNAME_EXP_DESTDB = 'ExportSQL_DestDB';
  REGNAME_EXP_WINWIDTH = 'ExportSQL_WindowWidth';
  REGNAME_EXP_WINHEIGHT = 'ExportSQL_WindowHeight';
  REGNAME_CSV_SEPARATOR = 'CSVSeparatorV2';
  DEFAULT_CSV_SEPARATOR = ';';
  REGNAME_CSV_ENCLOSER = 'CSVEncloserV2';
  DEFAULT_CSV_ENCLOSER = '"';
  REGNAME_CSV_TERMINATOR = 'CSVTerminator';
  DEFAULT_CSV_TERMINATOR = '\r\n';
  REGNAME_CSV_ESCAPER = 'CSVImportFieldEscaperV2';
  REGNAME_CSV_FILENAME = 'loadfilename';
  REGNAME_CSV_ENCLOPTION = 'CSVImportFieldsEnclosedOptionallyV2';
  REGNAME_CSV_IGNORELINES = 'CSVImportIgnoreLines';
  REGNAME_CSV_LOWPRIO = 'CSVImportLowPriority';
  REGNAME_CSV_REPLACE = 'CSVImportReplace';
  REGNAME_CSV_IGNORE = 'CSVImportIgnore';
  REGNAME_DO_UPDATECHECK = 'Updatecheck';
  DEFAULT_DO_UPDATECHECK = False;
  REGNAME_DO_UPDATECHECK_BUILDS = 'UpdatecheckBuilds';
  DEFAULT_DO_UPDATECHECK_BUILDS = False;
  REGNAME_UPDATECHECK_INTERVAL = 'UpdatecheckInterval';
  DEFAULT_UPDATECHECK_INTERVAL = 3;
  REGNAME_LAST_UPDATECHECK = 'UpdatecheckLastrun';
  REGNAME_MAINTWINWIDTH = 'Maintenance_WindowWidth';
  REGNAME_MAINTWINHEIGHT = 'Maintenance_WindowHeight';
  REGNAME_VIEWWINWIDTH = 'View_WindowWidth';
  REGNAME_VIEWWINHEIGHT = 'View_WindowHeight';
  REGNAME_PREFER_SHOWTABLES = 'PreferShowTables';
  DEFAULT_PREFER_SHOWTABLES = False;
  REGNAME_USERMNGR_WINWIDTH = 'Usermanager_WindowWidth';
  REGNAME_USERMNGR_WINHEIGHT = 'Usermanager_WindowHeight';
  REGNAME_SELECTDBO_WINWIDTH = 'SelectDBO_WindowWidth';
  REGNAME_SELECTDBO_WINHEIGHT = 'SelectDBO_WindowHeight';
  REGNAME_COPYTABLE_STRUCDATA = 'CopyTable_Option_StructureData';
  REGVAL_COPYTABLE_STRUCTURE = 0;
  REGVAL_COPYTABLE_STRUCTURE_AND_DATA = 1;
  DEFAULT_COPYTABLE_STRUCDATA = REGVAL_COPYTABLE_STRUCTURE_AND_DATA;
  REGNAME_COPYTABLE_INDEXES = 'CopyTable_Option_WithIndexes';
  REGNAME_COPYTABLE_ALLFIELDS = 'CopyTable_Option_WithAllFields';

  REGNAME_FIELDCOLOR_NUMERIC = 'FieldColor_Numeric';
  REGNAME_FIELDCOLOR_TEXT = 'FieldColor_Text';
  REGNAME_FIELDCOLOR_BINARY = 'FieldColor_Binary';
  REGNAME_FIELDCOLOR_DATETIME = 'FieldColor_Datetime';
  REGNAME_FIELDCOLOR_ENUM = 'FieldColor_Enum';
  DEFAULT_FIELDCOLOR_NUMERIC = $00FF0000; // clBlue
  DEFAULT_FIELDCOLOR_TEXT = $00008000; // clGreen
  DEFAULT_FIELDCOLOR_BINARY = $00800080; // clPurple
  DEFAULT_FIELDCOLOR_DATETIME = $00000080; // clMaroon
  DEFAULT_FIELDCOLOR_ENUM = $00008080; // clOlive
  COLORSHIFT_NULLFIELDS = 45; // Percentage of brightness to add to normal field colors for NULL values

  REGNAME_FIELDEDITOR_NUMERIC = 'FieldEditor_Numeric';
  REGNAME_FIELDEDITOR_TEXT = 'FieldEditor_Text';
  REGNAME_FIELDEDITOR_BINARY = 'FieldEditor_Binary';
  REGNAME_FIELDEDITOR_DATETIME = 'FieldEditor_Datetime';
  REGNAME_FIELDEDITOR_ENUM = 'FieldEditor_Enum';
  DEFAULT_FIELDEDITOR_NUMERIC = True;
  DEFAULT_FIELDEDITOR_TEXT = True;
  DEFAULT_FIELDEDITOR_BINARY = True;
  DEFAULT_FIELDEDITOR_DATETIME = True;
  DEFAULT_FIELDEDITOR_ENUM = True;

  REGNAME_SIZECOL_TREE = 'DisplayObjectSizeColumn';
  DEFAULT_SIZECOL_TREE = True;

  REGPREFIX_COLWIDTHS = 'ColWidths_';
  REGPREFIX_COLSVISIBLE = 'ColsVisible_';
  REGPREFIX_COLPOS = 'ColPositions_';
  REGPREFIX_WHERECLAUSE = 'WHERECLAUSE_';
  REGPREFIX_ORDERCLAUSE = 'ORDERCLAUSE_';

  // Various iconindexes
  ICONINDEX_PRIMARYKEY = 25;
  ICONINDEX_FIELD = 42;
  ICONINDEX_INDEXKEY = 23;
  ICONINDEX_UNIQUEKEY = 24;
  ICONINDEX_FULLTEXTKEY = 22;
  ICONINDEX_SERVER = 1;
  ICONINDEX_DB = 5;
  ICONINDEX_DB_HIGHLIGHT = 70;
  ICONINDEX_TABLE = 14;
  ICONINDEX_TABLE_HIGHLIGHT = 71;
  ICONINDEX_VIEW = 81;
  ICONINDEX_VIEW_HIGHLIGHT = 82;
  ICONINDEX_CRASHED_TABLE_HIGHLIGHT = -1;
  ICONINDEX_CRASHED_TABLE = -1;

  // Size of byte units
  {KiloByte} SIZE_KB = 1024;
  {MegaByte} SIZE_MB = 1048576;
  {GigaByte} SIZE_GB = 1073741824;
  {TeraByte} SIZE_TB = 1099511627776;
  {PetaByte} SIZE_PB = 1125899906842624;

  // Abbreviations of byte unit names
  {Bytes} NAME_BYTES = ' B';
  {KiloByte} NAME_KB = ' KB';
  {MegaByte} NAME_MB = ' MB';
  {GigaByte} NAME_GB = ' GB';
  {TeraByte} NAME_TB = ' TB';
  {PetaByte} NAME_PB = ' PB';

  // Copied constants from [delphi11]\source\win32\rtl\win\ShlObj.pas to make them
  // available in Delphi 10. We don't use the constants from ShlObj until delphi 10
  // support is removed.
  CSIDL_COMMON_APPDATA = $0023; { All Users\Application Data }
  CSIDL_APPDATA = $001a; { <user name>\Application Data }

  // Used by ListViews and Grids
  COLOR_SORTCOLUMN_ASC = $00F7F7F7; // light grey
  COLOR_SORTCOLUMN_DESC = $00EEEEEE; // medium grey
  ORDER_ASC = 0;            // Used for tag-value of "Direction"-button
  ORDER_DESC = 1;           // dito
  TXT_ASC = 'ASC';          // Used for caption of "Direction"-button
  TXT_DESC = 'DESC';        // dito

  // Node types of Virtual Tree nodes
  NODETYPE_DEFAULT = 0;
  NODETYPE_TABLE = 1;
  NODETYPE_VIEW = 2;
  NODETYPE_DB = 3;
  NODETYPE_COLUMN = 4;
  NODETYPE_CRASHED_TABLE = 5;

  // Data grid: How many bytes to fetch from data fields that are potentially large.
  GRIDMAXDATA: Integer = 256;

  // Data grid: How many rows to fetch at a time.
  GRIDMAXROWS: Cardinal = 500;

  // How many milliseconds to wait before showing a 'waiting for database' sign.
  QUERYWAITTIME: Integer = 2500;

