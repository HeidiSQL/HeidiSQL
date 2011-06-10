// Common constants
const

  // Line breaks
  CRLF = #13#10;
  LB_UNIX = #10;
  LB_MAC = #13;
  LB_WIDE = WideChar($2027);

  // Placeholder text for NULL values
  TEXT_NULL                 = '(NULL)';

  // General things
  APPNAME          = 'HeidiSQL';
  APPDOMAIN        = 'http://www.heidisql.com/';
  REGKEY_SESSIONS  = 'Servers\';
  // Some unique char, used to separate e.g. selected columns in registry
  DELIM = '|';

  // Used by maskSQL and fixSQL:
  SQL_VERSION_ANSI = -1;

  // Various names of registry variables
  // User-changable variables have a default value
  // Note: Color values are in HEX format: $00BBGGRR
  REGNAME_HIDDENCOLUMNS = 'HiddenColumns';
  REGNAME_FILTER = 'Filter';
  REGNAME_SORT = 'Sort';
  REGNAME_SORTDISPLAYEDCOLUMNS = 'DisplayedColumnsSorted';
  REGNAME_LASTSESSIONS = 'LastSessions';
  REGNAME_LASTACTIVESESSION = 'LastActiveSession';
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
  REGNAME_LOGDIR = 'SessionLogsDirectory';
  REGNAME_LOG_HORIZONTALSCROLLBAR = 'LogHorizontalScrollbar';
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
  REGNAME_ROWSPERSTEP = 'DatagridRowsPerStep';
  DEFAULT_ROWSPERSTEP = 1000;
  REGNAME_GRIDROWSLINECOUNT = 'GridRowLineCount';
  DEFAULT_GRIDROWSLINECOUNT = 1;
  REGNAME_REMEMBERFILTERS = 'RememberFilters';
  DEFAULT_REMEMBERFILTERS = True;
  REGNAME_LOGTOFILE = 'LogToFile';
  DEFAULT_LOGTOFILE = False;
  REGNAME_WINMAXIMIZED = 'MainWinMaximized';
  REGNAME_WINLEFT = 'MainWinLeft';
  REGNAME_WINTOP = 'MainWinTop';
  REGNAME_WINWIDTH = 'MainWinWidth';
  REGNAME_WINHEIGHT = 'MainWinHeight';
  REGNAME_WINONMONITOR = 'MainWinOnMonitor';
  REGNAME_TOOLBAR2LEFT = 'ToolBar2Left';
  REGNAME_TOOLBAR2TOP = 'ToolBar2Top';
  REGNAME_TOOLBARDATALEFT = 'ToolBarDataLeft';
  REGNAME_TOOLBARDATATOP = 'ToolBarDataTop';
  REGNAME_TOOLBARQUERYLEFT = 'ToolBarQueryLeft';
  REGNAME_TOOLBARQUERYTOP = 'ToolBarQueryTop';
  REGNAME_QUERYMEMOHEIGHT = 'querymemoheight';
  REGNAME_DBTREEWIDTH = 'dbtreewidth';
  REGNAME_PREVIEW_HEIGHT = 'DataPreviewHeight';
  REGNAME_PREVIEW_ENABLED = 'DataPreviewEnabled';
  REGNAME_SQLOUTHEIGHT = 'sqloutheight';
  REGNAME_QUERYHELPERSWIDTH = 'queryhelperswidth';
  REGNAME_STOPONERRORSINBATCH = 'StopOnErrorsInBatchMode';
  DEFAULT_STOPONERRORSINBATCH = True;
  REGNAME_WRAPLINES = 'WrapLongLines';
  REGNAME_BLOBASTEXT = 'DisplayBLOBsAsText';
  DEFAULT_BLOBASTEXT = False;
  REGNAME_SINGLEQUERIES = 'SingleQueries';
  REGNAME_EDITOR_WIDTH = 'MemoEditorWidth';
  DEFAULT_EDITOR_WIDTH = 100;
  REGNAME_EDITOR_HEIGHT = 'MemoEditorHeight';
  DEFAULT_EDITOR_HEIGHT = 100;
  REGNAME_EDITOR_WORDWRAP = 'MemoEditorWrap';
  REGNAME_DELIMITER = 'Delimiter';
  DEFAULT_DELIMITER = ';';
  REGNAME_SQLHELPWINLEFT = 'SQLHelp_WindowLeft';
  REGNAME_SQLHELPWINTOP = 'SQLHelp_WindowTop';
  REGNAME_SQLHELPWINWIDTH = 'SQLHelp_WindowWidth';
  REGNAME_SQLHELPWINHEIGHT = 'SQLHelp_WindowHeight';
  REGNAME_SQLHELPPLWIDTH = 'SQLHelp_PnlLeftWidth';
  REGNAME_SQLHELPPRHEIGHT = 'SQLHelp_PnlRightTopHeight';
  REGNAME_TABLEEDITOR_TABSHEIGHT = 'TableEditorTabsHeight';
  DEFAULT_TABLEEDITOR_TABSHEIGHT = 150;
  REGNAME_HOST = 'Host';
  DEFAULT_HOST = '127.0.0.1';
  REGNAME_USER = 'User';
  DEFAULT_USER = 'root';
  REGNAME_PASSWORD = 'Password';
  REGNAME_LOGINPROMPT = 'LoginPrompt';
  REGNAME_PORT = 'Port';
  DEFAULT_PORT = 3306;
  REGNAME_PLINKEXE = 'PlinkExecutable';
  REGNAME_SSHHOST = 'SSHtunnelHost';
  REGNAME_SSHPORT = 'SSHtunnelHostPort';
  DEFAULT_SSHPORT = 22;
  REGNAME_SSHLOCALPORT = 'SSHtunnelPort';
  REGNAME_SSHUSER = 'SSHtunnelUser';
  REGNAME_SSHPASSWORD = 'SSHtunnelPassword';
  REGNAME_SSHTIMEOUT = 'SSHtunnelTimeout';
  DEFAULT_SSHTIMEOUT = 4;
  REGNAME_SSHKEY = 'SSHtunnelPrivateKey';
  REGNAME_SSL_KEY = 'SSL_Key';
  REGNAME_SSL_CERT = 'SSL_Cert';
  REGNAME_SSL_CA = 'SSL_CA';
  REGNAME_NETTYPE = 'NetType';
  REGNAME_COMPRESSED = 'Compressed';
  DEFAULT_COMPRESSED = False;
  REGNAME_STARTUPSCRIPT = 'StartupScriptFilename';
  REGNAME_DATABASES = 'Databases';
  REGNAME_DATABASE_FILTER = 'DatabaseFilter';
  REGNAME_EXP_CREATEDB = 'ExportSQL_CreateDatabases';
  REGNAME_EXP_DROPDB = 'ExportSQL_DropDatabases';
  REGNAME_EXP_CREATETABLE = 'ExportSQL_CreateTables';
  REGNAME_EXP_DROPTABLE = 'ExportSQL_DropTables';
  REGNAME_EXP_DATAHOW = 'ExportSQL_DataHow';
  REGNAME_EXP_OUTFILES = 'ExportSQL_Filenames';
  REGNAME_EXP_OUTDIRS = 'ExportSQL_Directories';
  REGNAME_EXP_OUTPUT = 'ExportSQL_Output';
  REGNAME_GEXP_OUTPUTCOPY = 'GridExportOutputCopy';
  REGNAME_GEXP_OUTPUTFILE = 'GridExportOutputFile';
  REGNAME_GEXP_FILENAME = 'GridExportFilename';
  REGNAME_GEXP_RECENTFILES = 'GridExportRecentFiles';
  REGNAME_GEXP_ENCODING = 'GridExportEncoding';
  REGNAME_GEXP_FORMAT = 'GridExportFormat';
  REGNAME_GEXP_SELECTION = 'GridExportSelection';
  REGNAME_GEXP_COLUMNNAMES = 'GridExportColumnNames';
  REGNAME_GEXP_SEPARATOR = 'GridExportSeparator';
  REGNAME_GEXP_ENCLOSER = 'GridExportEncloser';
  REGNAME_GEXP_TERMINATOR = 'GridExportTerminator';
  REGNAME_CSV_SEPARATOR = 'CSVSeparatorV2';
  DEFAULT_CSV_SEPARATOR = ';';
  REGNAME_CSV_ENCLOSER = 'CSVEncloserV2';
  DEFAULT_CSV_ENCLOSER = '"';
  REGNAME_CSV_TERMINATOR = 'CSVTerminator';
  DEFAULT_CSV_TERMINATOR = '\r\n';
  REGNAME_CSV_ESCAPER = 'CSVImportFieldEscaperV2';
  REGNAME_CSV_WINDOWWIDTH = 'CSVImportWindowWidth';
  REGNAME_CSV_WINDOWHEIGHT = 'CSVImportWindowHeight';
  REGNAME_CSV_FILENAME = 'loadfilename';
  REGNAME_CSV_ENCLOPTION = 'CSVImportFieldsEnclosedOptionallyV2';
  REGNAME_CSV_IGNORELINES = 'CSVImportIgnoreLines';
  REGNAME_CSV_LOWPRIO = 'CSVImportLowPriority';
  REGNAME_CSV_LOCALNUMBERS = 'CSVImportLocalNumbers';
  REGNAME_CSV_DUPLICATES = 'CSVImportDuplicateHandling';
  REGNAME_CSV_PARSEMETHOD = 'CSVImportParseMethod';
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
  REGNAME_TOOLSDATATYPE = 'TableTools_Datatype';
  REGNAME_TOOLSCASESENSITIVE = 'TableTools_FindCaseSensitive';
  REGNAME_USERMNGR_WINWIDTH = 'Usermanager_WindowWidth';
  REGNAME_USERMNGR_WINHEIGHT = 'Usermanager_WindowHeight';
  REGNAME_USERMNGR_LISTWIDTH = 'Usermanager_ListWidth';
  REGNAME_SELECTDBO_WINWIDTH = 'SelectDBO_WindowWidth';
  REGNAME_SELECTDBO_WINHEIGHT = 'SelectDBO_WindowHeight';
  REGNAME_SESSMNGR_WINWIDTH = 'SessionManager_WindowWidth';
  REGNAME_SESSMNGR_WINHEIGHT = 'SessionManager_WindowHeight';
  REGNAME_COPYTABLE_WINHEIGHT = 'CopyTable_WindowHeight';
  REGNAME_COPYTABLE_WINWIDTH = 'CopyTable_WindowWidth';
  REGNAME_COPYTABLE_COLUMNS = 'CopyTable_Columns';
  REGNAME_COPYTABLE_KEYS = 'CopyTable_Keys';
  REGNAME_COPYTABLE_FOREIGN = 'CopyTable_ForeignKeys';
  REGNAME_COPYTABLE_DATA = 'CopyTable_Data';
  REGNAME_FILTERS = 'RecentFilters';
  REGNAME_SERVERVERSION = 'ServerVersion';
  REGNAME_SERVERVERSION_FULL = 'ServerVersionFull';
  REGNAME_LASTCONNECT = 'LastConnect';
  REGNAME_CONNECTCOUNT = 'ConnectCount';
  REGNAME_REFUSEDCOUNT = 'RefusedCount';
  REGNAME_SESSIONCREATED = 'SessionCreated';
  REGNAME_DO_STATISTICS = 'DoUsageStatistics';
  DEFAULT_DO_STATISTICS = False;
  REGNAME_LAST_STATSCALL = 'LastUsageStatisticCall';
  REGNAME_DISPLAYBARS = 'DisplayBars';
  DEFAULT_DISPLAYBARS = true;
  REGNAME_BARCOLOR = 'BarColor';
  DEFAULT_BARCOLOR = $00BBFFDD;
  REGNAME_PROMPTFILESAVE = 'PromptSaveFileOnTabClose';
  DEFAULT_PROMPTFILESAVE = True;
  REGNAME_COMPLETIONPROPOSAL = 'CompletionProposal';
  DEFAULT_COMPLETIONPROPOSAL = True;
  REGNAME_FILTERACTIVE = 'FilterPanel';
  DEFAULT_FILTERACTIVE = False;
  REGNAME_MULTI_INSTANCES = 'AllowMultipleInstances';
  DEFAULT_MULTI_INSTANCES = True;
  REGNAME_SEARCHTEXT = 'FindDialogSearchHistory';
  REGNAME_REPLACETEXT = 'FindDialogReplaceHistory';
  REGNAME_MAXQUERYRESULTS = 'MaxQueryResults';
  DEFAULT_MAXQUERYRESULTS = 10;
  REGNAME_SETEDITORWIDTH = 'SetEditorWidth';
  DEFAULT_SETEDITORWIDTH = 100;
  REGNAME_SETEDITORHEIGHT = 'SetEditorHeight';
  DEFAULT_SETEDITORHEIGHT = 130;

  // Log events
  REGNAME_LOG_ERRORS = 'LogErrors';
  DEFAULT_LOG_ERRORS = True;
  REGNAME_LOG_USERSQL = 'LogUserSQL';
  DEFAULT_LOG_USERSQL = True;
  REGNAME_LOG_SQL = 'LogSQL';
  DEFAULT_LOG_SQL = True;
  REGNAME_LOG_INFOS = 'LogInfos';
  DEFAULT_LOG_INFOS = True;
  REGNAME_LOG_DEBUG = 'LogDebug';
  DEFAULT_LOG_DEBUG = False;

  REGNAME_FIELDCOLOR_NUMERIC = 'FieldColor_Numeric';
  REGNAME_FIELDCOLOR_TEXT = 'FieldColor_Text';
  REGNAME_FIELDCOLOR_BINARY = 'FieldColor_Binary';
  REGNAME_FIELDCOLOR_DATETIME = 'FieldColor_Datetime';
  REGNAME_FIELDCOLOR_SPATIAL = 'FieldColor_Spatial';
  REGNAME_FIELDCOLOR_OTHER = 'FieldColor_Other';
  DEFAULT_FIELDCOLOR_NUMERIC = $00FF0000; // clBlue
  DEFAULT_FIELDCOLOR_TEXT = $00008000; // clGreen
  DEFAULT_FIELDCOLOR_BINARY = $00800080; // clPurple
  DEFAULT_FIELDCOLOR_DATETIME = $00000080; // clMaroon
  DEFAULT_FIELDCOLOR_SPATIAL = $00808000; // clOlive
  DEFAULT_FIELDCOLOR_OTHER = $00008080; // clTeal
  COLORSHIFT_NULLFIELDS = 70; // Brightness adjustment to add to normal field colors for NULL values
  COLORSHIFT_SORTCOLUMNS = 12; // Brightness adjustment to add to sorted column backgrounds

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
  REGPREFIX_COLSORT = 'ColSort_';
  REGPREFIX_DATEEDITOR_CURSOR = 'DateTimeEditor_CursorPos_Type';
  REGPREFIX_SHORTCUT1 = 'Shortcut1_';
  REGPREFIX_SHORTCUT2 = 'Shortcut2_';
  REGPREFIX_COPYTABLE_FILTERS = 'CopyTable_RecentFilter_';

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
  ORDER_ASC = 0;            // Used for tag-value of "Direction"-button
  ORDER_DESC = 1;           // dito
  TXT_ASC = 'ASC';          // Used for caption of "Direction"-button
  TXT_DESC = 'DESC';        // dito

  // Data grid: How many bytes to fetch from data fields that are potentially large.
  GRIDMAXDATA: Integer = 256;

  VTREE_NOTLOADED = 0;
  VTREE_NOTLOADED_PURGECACHE = 1;
  VTREE_LOADED = 2;

  SContainsNulCharFile = 'This file contains NUL characters. They have been converted to ASCII spaces (SP).';
  SContainsNulCharGrid = 'This cell contains NUL characters. They have been converted to ASCII spaces (SP). Press ESC to cancel editing.';
  SUnhandledNodeIndex = 'Unhandled tree node index';
  MSG_NOGRIDEDITING = 'Selected columns don''t contain a sufficient set of key columns to allow editing. Please select primary or unique key columns, or just all columns.';
  SIdle = 'Idle.';
  SUnsupported = 'Not supported by this server';

  PKEY = 'PRIMARY';
  KEY = 'KEY';
  UKEY = 'UNIQUE';
  FKEY = 'FULLTEXT';
  SKEY = 'SPATIAL';

  SYNCOMPLETION_PATTERN = '\image{%d}\hspace{5}\color{clGrayText}%s\column{}\color{clWindowText}%s';

  SizeGripProp = 'SizeGrip';

  // Forms are designed at 96 dpi. Use that to scale TFrames, which obviously do not auto-scale.
  FORMS_DPI = 96;

  HELPERNODE_COLUMNS = 0;
  HELPERNODE_FUNCTIONS = 1;
  HELPERNODE_KEYWORDS = 2;
  HELPERNODE_SNIPPETS = 3;
  HELPERNODE_PROFILE = 4;

  // A set of characters which need not to be quoted
  IDENTCHARS = ['A'..'Z', 'a'..'z', '0'..'9'];
