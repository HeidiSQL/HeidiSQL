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
  REGNAME_CONVERTHTMLENTITIES = 'ConvertHTMLEntities';
  DEFAULT_CONVERTHTMLENTITIES = True;
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
  REGNAME_DEFAULTCOLWIDTH = 'DefaultColWidth';
  DEFAULT_DEFAULTCOLWIDTH = 100;
  REGNAME_DATALIMIT = 'DataLimit';
  DEFAULT_DATALIMIT = True;
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
  REGNAME_TOOLBARDATALEFT = 'ToolBarDataLeft';
  REGNAME_TOOLBAR2TOP = 'ToolBar2Top';
  REGNAME_TOOLBARDATATOP = 'ToolBarDataTop';
  REGNAME_CHILDWINSTATE = 'childwinstate';
  REGNAME_CHILDWINLEFT = 'childwinleft';
  REGNAME_CHILDWINTOP = 'childwintop';
  REGNAME_CHILDWINWIDTH = 'childwinwidth';
  REGNAME_CHILDWINHEIGHT = 'childwinheight';
  REGNAME_QUERYMEMOHEIGHT = 'querymemoheight';
  REGNAME_DBTREEWIDTH = 'dbtreewidth';
  REGNAME_SQLOUTHEIGHT = 'sqloutheight';
  REGNAME_QUERYHELPERSWIDTH = 'queryhelperswidth';
  REGNAME_SQLWHEREFILE = 'SQLWhereFile';
  REGNAME_DELIMITERS = 'delimiters';
  REGNAME_DELIMITERSELECTED = 'delimiterselected';
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
  REGNAME_EXP_WINWIDTH = 'ExportSQL_WindowWidth';
  REGNAME_EXP_WINHEIGHT = 'ExportSQL_WindowHeight';
  REGNAME_CSV_SEPARATOR = 'CSVSeparator';
  DEFAULT_CSV_SEPARATOR = ',';
  REGNAME_CSV_ENCLOSER = 'CSVEncloser';
  DEFAULT_CSV_ENCLOSER = '';
  REGNAME_CSV_TERMINATOR = 'CSVTerminator';
  DEFAULT_CSV_TERMINATOR = '\r\n';
  REGNAME_CSV_ESCAPER = 'CSVImportFieldEscaper';
  REGNAME_CSV_FILENAME = 'loadfilename';
  REGNAME_CSV_ENCLOPTION = 'CSVImportFieldsEnclosedOptionally';
  REGNAME_CSV_IGNORELINES = 'CSVImportIgnoreLines';
  REGNAME_CSV_LOWPRIO = 'CSVImportLowPriority';
  REGNAME_CSV_REPLACE = 'CSVImportReplace';
  REGNAME_CSV_IGNORE = 'CSVImportIgnore';
  REGNAME_DO_UPDATECHECK = 'Updatecheck';
  DEFAULT_DO_UPDATECHECK = False;
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

  REGPREFIX_COLWIDTHS = 'ColWidths_';
  REGPREFIX_COLSVISIBLE = 'ColsVisible_';
  REGPREFIX_COLPOS = 'ColPositions_';
  REGPREFIX_ORDERCLAUSE = 'ORDERCLAUSE_';

  // how much memory we're aiming to use for the
  // data grid and it's automatic limit function
  // this value should probably be user configurable
  LOAD_SIZE = 5*1024*1024;

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

  // See reference: mysql.cpp Ver 14.12 Distrib 5.0.45, for Win32 (ia32): Line 112
  DEFAULT_DELIMITER = ';';

  // Copied constants from [delphi11]\source\win32\rtl\win\ShlObj.pas to make them
  // available in Delphi 10. We don't use the constants from ShlObj until delphi 10
  // support is removed.
  CSIDL_COMMON_APPDATA = $0023; { All Users\Application Data }
  CSIDL_APPDATA = $001a; { <user name>\Application Data }

  // Used by ListViews and Grids
  COLOR_SORTCOLUMN_ASC = $00F7F7F7; // light grey
  COLOR_SORTCOLUMN_DESC = $00EEEEEE; // medium grey
  COLOR_NULLVALUE = $00707070; // darker gray
  ORDER_ASC = 0;            // Used for tag-value of "Direction"-button
  ORDER_DESC = 1;           // dito
  TXT_ASC = 'ASC';          // Used for caption of "Direction"-button
  TXT_DESC = 'DESC';        // dito

  // Node types of Virtual Tree nodes
  NODETYPE_DEFAULT = 0;
  NODETYPE_BASETABLE = 1;
  NODETYPE_VIEW = 2;
  NODETYPE_DB = 3;
  NODETYPE_COLUMN = 4;
