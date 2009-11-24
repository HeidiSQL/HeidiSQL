// Common constants
const

  // Line breaks
  CRLF = #13#10;
  LB_UNIX = #10;
  LB_MAC = #13;
  LB_WIDE = WideChar($2027);

  // Names of the system tables and system databases
  PRIVTABLE_USERS           = 'user';
  PRIVTABLE_DB              = 'db';
  PRIVTABLE_TABLES          = 'tables_priv';
  PRIVTABLE_COLUMNS         = 'columns_priv';
  DBNAME_INFORMATION_SCHEMA = 'information_schema';
  DBNAME_MYSQL              = 'mysql';

  // Placeholder text for NULL values
  TEXT_NULL                 = '(NULL)';

  // General things
  APPNAME          = 'HeidiSQL';
  APPDOMAIN        = 'http://www.heidisql.com/';
  REGPATH          = '\Software\' + APPNAME + '\';
  REGKEY_SESSIONS  = 'Servers\';
  // Some unique char, used to separate e.g. selected columns in registry
  REGDELIM = '|';
  STATUS_MSG_READY = 'Ready.';
  STR_NOTSUPPORTED = 'Not supported by this server';

  // Used by maskSQL and fixSQL:
  SQL_VERSION_ANSI = -1;

  // Used for simulating a TTreeNode which has subnodes
  DUMMY_NODE_TEXT : String = 'Dummy node, should never be visible';

  // Connection types, used on session dialog
  NETTYPE_TCPIP = 0;
  NETTYPE_NAMEDPIPE = 1;

  // Various names of registry variables
  // User-changable variables have a default value
  // Note: Color values are in HEX format: $00BBGGRR
  REGNAME_HIDDENCOLUMNS = 'HiddenColumns';
  REGNAME_FILTER = 'Filter';
  REGNAME_SORT = 'Sort';
  REGNAME_SORTDISPLAYEDCOLUMNS = 'DisplayedColumnsSorted';
  REGNAME_LASTSESSION = 'lastcon';
  REGNAME_AUTORECONNECT = 'AutoReconnect';
  DEFAULT_AUTORECONNECT = False;
  REGNAME_RESTORELASTUSEDDB = 'RestoreLastUsedDB';
  DEFAULT_RESTORELASTUSEDDB = True;
  REGNAME_LASTUSEDDB = 'lastUsedDB';
  REGNAME_TREEBACKGROUND = 'TreeBackground';
  REGNAME_FONTNAME = 'FontName';
  DEFAULT_FONTNAME = 'Courier New';
  REGNAME_FONTSIZE = 'FontSize';
  DEFAULT_FONTSIZE = 9;
  REGNAME_TABWIDTH = 'TabWidth';
  DEFAULT_TABWIDTH = 3;
  REGNAME_DATAFONTNAME = 'DataFontName';
  DEFAULT_DATAFONTNAME = 'Tahoma';
  REGNAME_DATAFONTSIZE = 'DataFontSize';
  DEFAULT_DATAFONTSIZE = 8;
  // how much memory we're aiming to use for the
  // data grid and it's automatic limit function
  REGNAME_LOGSQLNUM = 'logsqlnum';
  DEFAULT_LOGSQLNUM = 300;
  REGNAME_LOGSQLWIDTH = 'logsqlwidth';
  DEFAULT_LOGSQLWIDTH = 2000;
  REGPREFIX_SQLATTRI = 'SQL Attr ';
  REGPOSTFIX_SQL_FG = ' Foreground';
  REGPOSTFIX_SQL_BG = ' Background';
  REGPOSTFIX_SQL_STYLE = ' Style';
  REGNAME_SQLCOLACTIVELINE = 'SQLColActiveLine';
  DEFAULT_SQLCOLACTIVELINE = $00FFFFFF; // clWhite
  REGNAME_MAXCOLWIDTH = 'MaxColWidth';
  DEFAULT_MAXCOLWIDTH = 300;
  REGNAME_MAXTOTALROWS = 'DatagridMaximumRows';
  DEFAULT_MAXTOTALROWS = 100000;
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
  REGNAME_DELIMITER = 'Delimiter';
  DEFAULT_DELIMITER = ';';
  REGNAME_SQLHELPWINLEFT = 'SQLHelp_WindowLeft';
  REGNAME_SQLHELPWINTOP = 'SQLHelp_WindowTop';
  REGNAME_SQLHELPWINWIDTH = 'SQLHelp_WindowWidth';
  REGNAME_SQLHELPWINHEIGHT = 'SQLHelp_WindowHeight';
  REGNAME_SQLHELPPLWIDTH = 'SQLHelp_PnlLeftWidth';
  REGNAME_SQLHELPPRHEIGHT = 'SQLHelp_PnlRightTopHeight';
  REGNAME_TABLEEDITOR_TABSHEIGHT = 'TableEditorTabsHeight';
  REGNAME_HOST = 'Host';
  DEFAULT_HOST = '127.0.0.1';
  REGNAME_USER = 'User';
  DEFAULT_USER = 'root';
  REGNAME_PASSWORD = 'Password';
  DEFAULT_PASSWORD = '';
  REGNAME_PORT = 'Port';
  DEFAULT_PORT = 3306;
  REGNAME_NETTYPE = 'NetType';
  DEFAULT_NETTYPE = NETTYPE_TCPIP;
  REGNAME_COMPRESSED = 'Compressed';
  DEFAULT_COMPRESSED = False;
  REGNAME_ONLYDBS = 'OnlyDBs';
  REGNAME_EXP_CREATEDB = 'ExportSQL_CreateDatabases';
  REGNAME_EXP_DROPDB = 'ExportSQL_DropDatabases';
  REGNAME_EXP_CREATETABLE = 'ExportSQL_CreateTables';
  REGNAME_EXP_DROPTABLE = 'ExportSQL_DropTables';
  REGNAME_EXP_DATAHOW = 'ExportSQL_DataHow';
  REGNAME_EXP_OUTFILES = 'ExportSQL_Filenames';
  REGNAME_EXP_OUTDIRS = 'ExportSQL_Directories';
  REGNAME_EXP_OUTPUT = 'ExportSQL_Output';
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
  REGNAME_COPYMAXSIZE = 'CopyDataMaxSize';
  DEFAULT_COPYMAXSIZE = 5;
  REGNAME_DO_UPDATECHECK = 'Updatecheck';
  DEFAULT_DO_UPDATECHECK = False;
  REGNAME_DO_UPDATECHECK_BUILDS = 'UpdatecheckBuilds';
  DEFAULT_DO_UPDATECHECK_BUILDS = False;
  REGNAME_UPDATECHECK_INTERVAL = 'UpdatecheckInterval';
  DEFAULT_UPDATECHECK_INTERVAL = 3;
  REGNAME_LAST_UPDATECHECK = 'UpdatecheckLastrun';
  REGNAME_TOOLSWINWIDTH = 'TableTools_WindowWidth';
  REGNAME_TOOLSWINHEIGHT = 'TableTools_WindowHeight';
  REGNAME_TOOLSTREEWIDTH = 'TableTools_TreeWidth';
  REGNAME_TOOLSFINDTEXT = 'TableTools_FindText';
  REGNAME_TOOLSSKIPMB = 'TableTools_SkipMB';
  REGNAME_TOOLSDATATYPE = 'TableTools_Datatype';
  REGNAME_USERMNGR_WINWIDTH = 'Usermanager_WindowWidth';
  REGNAME_USERMNGR_WINHEIGHT = 'Usermanager_WindowHeight';
  REGNAME_SELECTDBO_WINWIDTH = 'SelectDBO_WindowWidth';
  REGNAME_SELECTDBO_WINHEIGHT = 'SelectDBO_WindowHeight';
  REGNAME_SESSMNGR_WINWIDTH = 'SessionManager_WindowWidth';
  REGNAME_SESSMNGR_WINHEIGHT = 'SessionManager_WindowHeight';
  REGNAME_COPYTABLE_STRUCDATA = 'CopyTable_Option_StructureData';
  REGVAL_COPYTABLE_STRUCTURE = 0;
  REGVAL_COPYTABLE_STRUCTURE_AND_DATA = 1;
  DEFAULT_COPYTABLE_STRUCDATA = REGVAL_COPYTABLE_STRUCTURE_AND_DATA;
  REGNAME_COPYTABLE_INDEXES = 'CopyTable_Option_WithIndexes';
  REGNAME_COPYTABLE_ALLFIELDS = 'CopyTable_Option_WithAllFields';
  REGNAME_DEFAULTVIEW = 'DefaultView';
  REGNAME_FILTERS = 'RecentFilters';
  REGNAME_SERVERVERSION = 'ServerVersion';
  REGNAME_LASTCONNECT = 'LastConnect';
  REGNAME_CONNECTCOUNT = 'ConnectCount';
  REGNAME_REFUSEDCOUNT = 'RefusedCount';
  REGNAME_SESSIONCREATED = 'SessionCreated';
  REGNAME_DO_STATISTICS = 'DoUsageStatistics';
  DEFAULT_DO_STATISTICS = False;
  REGNAME_LAST_STATSCALL = 'LastUsageStatisticCall';
  REGNAME_FILTERACTIVE = 'FilterPanel';
  DEFAULT_FILTERACTIVE = False;

  REGNAME_FIELDCOLOR_NUMERIC = 'FieldColor_Numeric';
  REGNAME_FIELDCOLOR_TEXT = 'FieldColor_Text';
  REGNAME_FIELDCOLOR_BINARY = 'FieldColor_Binary';
  REGNAME_FIELDCOLOR_DATETIME = 'FieldColor_Datetime';
  REGNAME_FIELDCOLOR_ENUM = 'FieldColor_Enum';
  REGNAME_FIELDCOLOR_SET = 'FieldColor_Set';
  DEFAULT_FIELDCOLOR_NUMERIC = $00FF0000; // clBlue
  DEFAULT_FIELDCOLOR_TEXT = $00008000; // clGreen
  DEFAULT_FIELDCOLOR_BINARY = $00800080; // clPurple
  DEFAULT_FIELDCOLOR_DATETIME = $00000080; // clMaroon
  DEFAULT_FIELDCOLOR_ENUM = $00008080; // clOlive
  DEFAULT_FIELDCOLOR_SET = $00808000; // clTeal
  COLORSHIFT_NULLFIELDS = 45; // Percentage of brightness to add to normal field colors for NULL values

  REGNAME_FIELDEDITOR_BINARY = 'FieldEditor_Binary';
  REGNAME_FIELDEDITOR_DATETIME = 'FieldEditor_Datetime';
  REGNAME_FIELDEDITOR_ENUM = 'FieldEditor_Enum';
  REGNAME_FIELDEDITOR_SET = 'FieldEditor_Set';
  DEFAULT_FIELDEDITOR_BINARY = True;
  DEFAULT_FIELDEDITOR_DATETIME = True;
  DEFAULT_FIELDEDITOR_ENUM = True;
  DEFAULT_FIELDEDITOR_SET = True;

  REGNAME_BG_NULL = 'Field_NullBackground';
  DEFAULT_BG_NULL = $00FF00FF; // clMagenta
  REGNAME_BG_NULL_ENABLED = 'FieldNull_EnableBackground';
  DEFAULT_BG_NULL_ENABLED = False;

  REGNAME_SIZECOL_TREE = 'DisplayObjectSizeColumn';
  DEFAULT_SIZECOL_TREE = True;

  REGPREFIX_COLWIDTHS = 'ColWidths_';
  REGPREFIX_COLSVISIBLE = 'ColsVisible_';
  REGPREFIX_COLPOS = 'ColPositions_';
  REGPREFIX_CRASH_IN_DATA = 'DataCrashIndicator_';
  REGPREFIX_DATAVIEW = 'View_';
  REGPREFIX_DATEEDITOR_CURSOR = 'DateTimeEditor_CursorPos_Type';
  REGPREFIX_SHORTCUT1 = 'Shortcut1_';
  REGPREFIX_SHORTCUT2 = 'Shortcut2_';

  // Various iconindexes
  ICONINDEX_PRIMARYKEY = 25;
  ICONINDEX_FIELD = 42;
  ICONINDEX_INDEXKEY = 23;
  ICONINDEX_UNIQUEKEY = 24;
  ICONINDEX_FULLTEXTKEY = 22;
  ICONINDEX_SPATIALKEY = 126;
  ICONINDEX_SERVER = 1;
  ICONINDEX_DB = 5;
  ICONINDEX_DB_HIGHLIGHT = 70;
  ICONINDEX_TABLE = 14;
  ICONINDEX_TABLE_HIGHLIGHT = 71;
  ICONINDEX_VIEW = 81;
  ICONINDEX_VIEW_HIGHLIGHT = 82;
  ICONINDEX_CRASHED_TABLE_HIGHLIGHT = -1;
  ICONINDEX_CRASHED_TABLE = -1;
  ICONINDEX_STOREDPROCEDURE = 119;
  ICONINDEX_STOREDFUNCTION = 35;
  ICONINDEX_FUNCTION = 13;
  ICONINDEX_KEYWORD = 25;

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

  // Used by ListViews and Grids
  COLOR_SORTCOLUMN_ASC = $00F7F7F7; // light grey
  COLOR_SORTCOLUMN_DESC = $00EEEEEE; // medium grey
  ORDER_ASC = 0;            // Used for tag-value of "Direction"-button
  ORDER_DESC = 1;           // dito
  TXT_ASC = 'ASC';          // Used for caption of "Direction"-button
  TXT_DESC = 'DESC';        // dito

  // Data grid: How many bytes to fetch from data fields that are potentially large.
  GRIDMAXDATA: Integer = 256;

  // Data grid: How many rows to fetch at a time.
  GRIDMAXROWS: Cardinal = 1000;

  VTREE_NOTLOADED = 0;
  VTREE_LOADED = 1;

  MSG_COPYMAXSIZE = 'Copying data is limited to %s but more data is available. '
        + 'Only %s out of %s rows were copied.' + CRLF + CRLF
        + 'Increase the value in Tools > Preferences > Export if you need more.';
  SContainsNulCharFile = 'This file contains NUL characters. They have been converted to ASCII spaces (SP).';
  SContainsNulCharGrid = 'This cell contains NUL characters. They have been converted to ASCII spaces (SP). Press ESC to cancel editing.';

  DBO_NAME = 'Name';
  DBO_TYPE = 'Type';
  DBO_ENGINE = 'Engine';
  DBO_VERSION = 'Version';
  DBO_ROWFORMAT = 'Row_format';
  DBO_ROWS = 'Rows';
  DBO_AVGROWLEN = 'Avg_row_length';
  DBO_DATALEN = 'Data_length';
  DBO_MAXDATALEN = 'Max_data_length';
  DBO_INDEXLEN = 'Index_length';
  DBO_DATAFREE = 'Data_free';
  DBO_AUTOINC = 'Auto_increment';
  DBO_CREATED = 'Create_time';
  DBO_UPDATED = 'Update_time';
  DBO_CHECKED = 'Check_time';
  DBO_COLLATION = 'Collation';
  DBO_CHECKSUM = 'Checksum';
  DBO_CROPTIONS = 'Create_options';
  DBO_COMMENT = 'Comment';

