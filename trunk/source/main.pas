unit Main;


// -------------------------------------
// Main-window
// -------------------------------------

{$I compilers.inc}

interface

uses
  Synchronization,
  Communication,
  Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ImgList, ShellApi, ToolWin, Clipbrd, db,
  SynMemo, synedit, SynEditTypes, ZDataSet, ZSqlProcessor,
  HeidiComp, sqlhelp, MysqlQueryThread, VirtualTrees,
  DateUtils, PngImageList, OptimizeTables, View, Usermanager,
  SelectDBObject, Widestrings, ShlObj, SynEditMiscClasses, SynEditSearch,
  SynCompletionProposal, ZSqlMonitor, SynEditHighlighter, SynHighlighterSQL,
  TntStdCtrls, Tabs, SynUnicode, mysqlconn, EditVar, helpers, queryprogress,
  mysqlquery, createdatabase, table_editor, SynRegExpr,
  WideStrUtils, ZDbcLogging, ExtActns, CommCtrl, routine_editor, options,
  Contnrs, PngSpeedButton;

const
  // The InnoDB folks are raging over the lack of count(*) support
  // in the storage engine.  To avoid count(*), the first of these
  // constants decide how many rows the data area should estimate
  // in any table.  The second value decides how many percent above the
  // number of seen (or simulated) rows the scrollbar should project.
  SIMULATE_INITIAL_ROWS = 10000;
  SIMULATE_MORE_ROWS = 20;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    menuAbout: TMenuItem;
    Edit1: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    actCopy: TAction;
    actPaste: TAction;
    actNewWindow: TAction;
    actExitApplication: TAction;
    Extra1: TMenuItem;
    FlushUserPrivileges1: TMenuItem;
    MenuCopyCSV: TMenuItem;
    MenuExport: TMenuItem;
    N5: TMenuItem;
    MenuImportTextFile: TMenuItem;
    Flush1: TMenuItem;
    MenuFlushLogs: TMenuItem;
    MenuFlushHosts: TMenuItem;
    MenuFlushTables: TMenuItem;
    MenuFlushTableswithreadlock: TMenuItem;
    MenuFlushStatus: TMenuItem;
    N6: TMenuItem;
    MenuUserManager: TMenuItem;
    MenuPreferences: TMenuItem;
    N7a: TMenuItem;
    menuReadme: TMenuItem;
    actUserManager: TAction;
    actAboutBox: TAction;
    actMaintenance: TAction;
    menuMaintenance: TMenuItem;
    ImExport1: TMenuItem;
    CopyContentsasHTMLTable1: TMenuItem;
    actCopyAsHTML: TAction;
    actCopyAsCSV: TAction;
    menuWebsite: TMenuItem;
    N9: TMenuItem;
    N11: TMenuItem;
    actPrintList: TAction;
    actCopyTable: TAction;
    ControlBar1: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton9: TToolButton;
    tlbSep1: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton12: TToolButton;
    tlbSep2: TToolButton;
    ButtonRefresh: TToolButton;
    ButtonImportTextfile: TToolButton;
    ButtonExport: TToolButton;
    ButtonUserManager: TToolButton;
    ToolBarData: TToolBar;
    actUndo: TEditUndo;
    ToolButton14: TToolButton;
    actExecuteQuery: TAction;
    actExecuteSelection: TAction;
    SaveDialog2: TSaveDialog;
    ExportSettings1: TMenuItem;
    Importsettings1: TMenuItem;
    OpenDialog2: TOpenDialog;
    menuSupportForum: TMenuItem;
    actCopyAsXML: TAction;
    actExportData: TAction;
    Exportdata1: TMenuItem;
    CopyasXMLdata1: TMenuItem;
    actExecuteLine: TAction;
    actHTMLview: TAction;
    actInsertFiles: TAction;
    InsertfilesintoBLOBfields1: TMenuItem;
    actExportTables: TAction;
    actDropObjects: TAction;
    actLoadSQL: TAction;
    ImportSQL1: TMenuItem;
    menuConnections: TPopupMenu;
    menuWindow: TMenuItem;
    miFake: TMenuItem;
    menuBugtracker: TMenuItem;
    menuFeaturetracker: TMenuItem;
    menuDownload: TMenuItem;
    btnSQLHelp: TToolButton;
    menuSQLHelp1: TMenuItem;
    N8a: TMenuItem;
    Import1: TMenuItem;
    tlbSep6: TToolButton;
    menuUpdateCheck: TMenuItem;
    PngImageListMain: TPngImageList;
    actCreateView: TAction;
    ToolButton3: TToolButton;
    actDataFirst: TAction;
    actDataLast: TAction;
    actDataInsert: TAction;
    actDataDelete: TAction;
    actDataPostChanges: TAction;
    ToolButton4: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    actCreateTable: TAction;
    actEmptyTables: TAction;
    actCreateDatabase: TAction;
    actSQLhelp: TAction;
    actRefresh: TAction;
    actImportCSV: TAction;
    actCut: TAction;
    Cut1: TMenuItem;
    actExportSettings: TAction;
    actImportSettings: TAction;
    actSelectTreeBackground: TAction;
    actPreferences: TAction;
    actFlushHosts: TAction;
    actFlushLogs: TAction;
    actFlushPrivileges: TAction;
    actFlushTables: TAction;
    actFlushTableswithreadlock: TAction;
    actFlushStatus: TAction;
    actUpdateCheck: TAction;
    actWebMainsite: TAction;
    actWebDownloadpage: TAction;
    actWebForum: TAction;
    actWebBugtracker: TAction;
    actWebFeaturetracker: TAction;
    actReadme: TAction;
    actSaveSQL: TAction;
    actSaveSQLselection: TAction;
    actSaveSQLSnippet: TAction;
    actSaveSQLSelectionSnippet: TAction;
    actClearQueryEditor: TAction;
    actClearFilterEditor: TAction;
    actApplyFilter: TAction;
    actQueryStopOnErrors: TAction;
    actQueryWordWrap: TAction;
    actQueryFind: TAction;
    actQueryReplace: TAction;
    FindDialogQuery: TFindDialog;
    ReplaceDialogQuery: TReplaceDialog;
    ToolBarQuery: TToolBar;
    btnExecuteQuery: TToolButton;
    btnExecuteSelection: TToolButton;
    btnLoadSQL: TToolButton;
    btnSaveSQL: TToolButton;
    btnSaveSQLSnippet: TToolButton;
    btnQueryFind: TToolButton;
    btnQueryReplace: TToolButton;
    btnStopOnErrors: TToolButton;
    btnQueryWordwrap: TToolButton;
    PopupQueryLoad: TPopupMenu;
    btnExecuteLine: TToolButton;
    actSetDelimiter: TAction;
    btnSetDelimiter: TToolButton;
    actDataCancelChanges: TAction;
    ToolButton1: TToolButton;
    actRemoveFilter: TAction;
    actCopyAsSQL: TAction;
    CopyAsSQLdata: TMenuItem;
    panelTop: TPanel;
    DBtree: TVirtualStringTree;
    Splitter1: TSplitter;
    PageControlMain: TPageControl;
    tabData: TTabSheet;
    tabDatabase: TTabSheet;
    splitterTopBottom: TSplitter;
    tabQuery: TTabSheet;
    popupDB: TPopupMenu;
    menuRefreshDB: TMenuItem;
    tabHost: TTabSheet;
    PageControlHost: TPageControl;
    tabVariables: TTabSheet;
    tabProcessList: TTabSheet;
    ListVariables: TVirtualStringTree;
    ListProcesses: TVirtualStringTree;
    popupHost: TPopupMenu;
    Kill1: TMenuItem;
    ListTables: TVirtualStringTree;
    Refresh1: TMenuItem;
    pnlDataTop: TPanel;
    pnlQueryMemo: TPanel;
    SynSQLSyn1: TSynSQLSyn;
    SynMemoQuery: TSynMemo;
    spltQuery: TSplitter;
    OpenDialog1: TOpenDialog;
    TimerHostUptime: TTimer;
    N5a: TMenuItem;
    popupDataGrid: TPopupMenu;
    Refresh3: TMenuItem;
    popupResultGrid: TPopupMenu;
    Copyrecords1: TMenuItem;
    CopyasCSVData1: TMenuItem;
    N9a: TMenuItem;
    LabelResultinfo: TLabel;
    TimerConnected: TTimer;
    N12: TMenuItem;
    popupSqlLog: TPopupMenu;
    Clear2: TMenuItem;
    Copy1: TMenuItem;
    N15: TMenuItem;
    N17: TMenuItem;
    CopycontentsasHTML1: TMenuItem;
    CopycontentsasHTML2: TMenuItem;
    Copy3: TMenuItem;
    Paste2: TMenuItem;
    N4a: TMenuItem;
    DataGrid: TVirtualStringTree;
    QueryGrid: TVirtualStringTree;
    Copytableas1: TMenuItem;
    Delete1: TMenuItem;
    N6a: TMenuItem;
    QF1: TMenuItem;
    QF2: TMenuItem;
    QuickFilter1: TMenuItem;
    QF3: TMenuItem;
    QF4: TMenuItem;
    N7: TMenuItem;
    DropFilter1: TMenuItem;
    PrintList2: TMenuItem;
    N1a: TMenuItem;
    SynMemoFilter: TSynMemo;
    TimerRefresh: TTimer;
    Saveastextfile1: TMenuItem;
    QF7: TMenuItem;
    QF5: TMenuItem;
    QF6: TMenuItem;
    QF8: TMenuItem;
    QF10: TMenuItem;
    QF11: TMenuItem;
    QF9: TMenuItem;
    QF12: TMenuItem;
    CopyasXMLdata3: TMenuItem;
    CopyasXMLdata2: TMenuItem;
    Exportdata3: TMenuItem;
    Exportdata2: TMenuItem;
    SaveDialogExportData: TSaveDialog;
    N11a: TMenuItem;
    Copy4: TMenuItem;
    N14: TMenuItem;
    DataInsertDateTime: TMenuItem;
    DataTimestamp: TMenuItem;
    DataDateTime: TMenuItem;
    DataTime: TMenuItem;
    DataDate: TMenuItem;
    DataYear: TMenuItem;
    ViewasHTML1: TMenuItem;
    HTMLview1: TMenuItem;
    InsertfilesintoBLOBfields3: TMenuItem;
    N19: TMenuItem;
    setNULL1: TMenuItem;
    ZSQLMonitor1: TZSQLMonitor;
    menuExporttables: TMenuItem;
    popupDbGridHeader: TPopupMenu;
    SynCompletionProposal1: TSynCompletionProposal;
    OpenDialogSQLFile: TOpenDialog;
    SaveDialogSQLFile: TSaveDialog;
    SynEditSearch1: TSynEditSearch;
    tabCommandStats: TTabSheet;
    ListCommandStats: TVirtualStringTree;
    QF13: TMenuItem;
    QF14: TMenuItem;
    QF15: TMenuItem;
    QF16: TMenuItem;
    QF17: TMenuItem;
    QF18: TMenuItem;
    QF19: TMenuItem;
    N21: TMenuItem;
    pnlQueryHelpers: TPanel;
    tabsetQueryHelpers: TTabSet;
    lboxQueryHelpers: TTnTListBox;
    popupQuery: TPopupMenu;
    MenuRun: TMenuItem;
    MenuRunSelection: TMenuItem;
    MenuRunLine: TMenuItem;
    MenuItem1: TMenuItem;
    menucopy: TMenuItem;
    menupaste: TMenuItem;
    menuload: TMenuItem;
    menusave: TMenuItem;
    menuclear: TMenuItem;
    MenuFind: TMenuItem;
    MenuReplace: TMenuItem;
    MenuItem2: TMenuItem;
    lblDataTop: TTNTLabel;
    spltQueryHelpers: TSplitter;
    N22: TMenuItem;
    N23: TMenuItem;
    menuSaveSelectionToFile: TMenuItem;
    menuSaveAsSnippet: TMenuItem;
    menuSaveSelectionAsSnippet: TMenuItem;
    popupQueryHelpers: TPopupMenu;
    menuDeleteSnippet: TMenuItem;
    menuHelp: TMenuItem;
    menuLoadSnippet: TMenuItem;
    menuInsertSnippetAtCursor: TMenuItem;
    menuExplore: TMenuItem;
    menuSQLhelp2: TMenuItem;
    N24: TMenuItem;
    menuSQLhelpData: TMenuItem;
    menuLogToFile: TMenuItem;
    menuOpenLogFolder: TMenuItem;
    tabStatus: TTabSheet;
    ListStatus: TVirtualStringTree;
    Splitter3: TSplitter;
    pnlProcessViewBox: TPanel;
    pnlProcessView: TPanel;
    SynMemoProcessView: TSynMemo;
    pnlFilterVT: TPanel;
    editFilterVT: TEdit;
    lblFilterVT: TLabel;
    lblFilterVTInfo: TLabel;
    menuEditVariable: TMenuItem;
    menuTreeExpandAll: TMenuItem;
    menuTreeCollapseAll: TMenuItem;
    tlbDataButtons: TToolBar;
    tbtnDataSorting: TToolButton;
    tbtnDataColumns: TToolButton;
    tbtnDataFilter: TToolButton;
    pnlFilter: TPanel;
    btnFilterApply: TButton;
    lblTableFilter: TLabel;
    editFilterSearch: TEdit;
    btnFilterClear: TButton;
    popupFilter: TPopupMenu;
    menuFilterCopy: TMenuItem;
    menuFilterPaste: TMenuItem;
    N8: TMenuItem;
    menuFilterApply: TMenuItem;
    menuFilterClear: TMenuItem;
    N20: TMenuItem;
    SynMemoSQLLog: TSynMemo;
    Insert1: TMenuItem;
    Cancelediting1: TMenuItem;
    DataPost1: TMenuItem;
    menuShowSizeColumn: TMenuItem;
    tbtnDataView: TToolButton;
    popupDataView: TPopupMenu;
    menuViewSave: TMenuItem;
    N25: TMenuItem;
    menuViewDefault: TMenuItem;
    CopygriddataasSQL1: TMenuItem;
    CopygriddataasSQL2: TMenuItem;
    menuSelectBGColor: TMenuItem;
    actPreviousTab: TPreviousTab;
    actNextTab: TNextTab;
    Nexttab1: TMenuItem;
    Previoustab1: TMenuItem;
    menuConnectTo: TMenuItem;
    actSelectAll: TAction;
    actSelectAll1: TMenuItem;
    N13: TMenuItem;
    ProgressBarStatus: TProgressBar;
    menuRecentFilters: TMenuItem;
    comboRecentFilters: TTntComboBox;
    lblRecentFilters: TLabel;
    Copy2: TMenuItem;
    N26: TMenuItem;
    actSessionManager: TAction;
    Sessionmanager1: TMenuItem;
    actCreateRoutine: TAction;
    btnExit: TToolButton;
    lblSorryNoData: TLabel;
    menuPrint: TMenuItem;
    menuEditObject: TMenuItem;
    menuCreateObject: TMenuItem;
    menuDeleteObject: TMenuItem;
    menuMaintenance2: TMenuItem;
    menuEmptyTables: TMenuItem;
    actEditObject: TAction;
    menuCreateDB: TMenuItem;
    menuCreateTable: TMenuItem;
    menuCreateTableCopy: TMenuItem;
    menuCreateView: TMenuItem;
    menuCreateRoutine: TMenuItem;
    tabEditor: TTabSheet;
    popupRefresh: TPopupMenu;
    menuAutoRefreshSetInterval: TMenuItem;
    menuAutoRefresh: TMenuItem;
    popupMainTabs: TPopupMenu;
    menuNewQueryTab: TMenuItem;
    menuCloseTab: TMenuItem;
    actNewQueryTab: TAction;
    actCloseQueryTab: TAction;
    Newquerytab1: TMenuItem;
    Closetab1: TMenuItem;
    procedure refreshMonitorConfig;
    procedure loadWindowConfig;
    procedure saveWindowConfig;
    procedure setDefaultWindowConfig;
    procedure actCreateTableExecute(Sender: TObject);
    procedure actCreateViewExecute(Sender: TObject);
    procedure menuWindowClick(Sender: TObject);
    procedure focusWindow(Sender: TObject);
    procedure menuConnectionsPopup(Sender: TObject);
    procedure actExitApplicationExecute(Sender: TObject);
    procedure DisplayChange(var msg: TMessage); message WM_DISPLAYCHANGE;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Startup;
    procedure DoAfterConnect;
    procedure DoDisconnect;
    procedure FormResize(Sender: TObject);
    procedure actUserManagerExecute(Sender: TObject);
    procedure actAboutBoxExecute(Sender: TObject);
    procedure actApplyFilterExecute(Sender: TObject);
    procedure actClearEditorExecute(Sender: TObject);
    procedure actMaintenanceExecute(Sender: TObject);
    procedure actCopyAsHTMLExecute(Sender: TObject);
    procedure actCopyAsCSVExecute(Sender: TObject);
    procedure actPrintListExecute(Sender: TObject);
    procedure actCopyTableExecute(Sender: TObject);
    procedure showstatus(msg: string=''; panel: Integer=6);
    function mask(str: WideString) : WideString;
    procedure actExecuteQueryExecute(Sender: TObject);
    procedure actExecuteSelectionExecute(Sender: TObject);
    procedure actCopyAsXMLExecute(Sender: TObject);
    procedure actCreateDatabaseExecute(Sender: TObject);
    procedure actDataCancelChangesExecute(Sender: TObject);
    procedure actExportDataExecute(Sender: TObject);
    procedure actExecuteLineExecute(Sender: TObject);
    procedure actHTMLviewExecute(Sender: TObject);
    procedure actInsertFilesExecute(Sender: TObject);
    procedure actExportTablesExecute(Sender: TObject);
    procedure actDataDeleteExecute(Sender: TObject);
    procedure actDataFirstExecute(Sender: TObject);
    procedure actDataInsertExecute(Sender: TObject);
    procedure actDataLastExecute(Sender: TObject);
    procedure actDataPostChangesExecute(Sender: TObject);
    procedure actDropObjectsExecute(Sender: TObject);
    procedure actEmptyTablesExecute(Sender: TObject);
    procedure actExportSettingsExecute(Sender: TObject);
    procedure actFlushExecute(Sender: TObject);
    procedure actImportCSVExecute(Sender: TObject);
    procedure actImportSettingsExecute(Sender: TObject);
    procedure actLoadSQLExecute(Sender: TObject);
    procedure actNewWindowExecute(Sender: TObject);
    procedure actSessionManagerExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actQueryFindExecute(Sender: TObject);
    procedure actQueryReplaceExecute(Sender: TObject);
    procedure actQueryStopOnErrorsExecute(Sender: TObject);
    procedure actQueryWordWrapExecute(Sender: TObject);
    procedure actReadmeExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actRemoveFilterExecute(Sender: TObject);
    procedure actSaveSQLExecute(Sender: TObject);
    procedure actSaveSQLSnippetExecute(Sender: TObject);
    procedure actSetDelimiterExecute(Sender: TObject);
    procedure actSQLhelpExecute(Sender: TObject);
    procedure actUpdateCheckExecute(Sender: TObject);
    procedure actWebbrowse(Sender: TObject);
    function ExecuteRemoteQuery(sender: THandle; query: string): TDataSet;
    procedure ExecuteRemoteNonQuery(sender: THandle; query: string);
    procedure FindDialogQueryFind(Sender: TObject);
    procedure HandleWMComplete(var msg: TMessage); message WM_COMPLETED;
    procedure HandleWMCopyData(var msg: TWMCopyData); message WM_COPYDATA;
    procedure HandleWMProcessLog(var msg: TMessage); message WM_PROCESSLOG;
    procedure HandleWMRefill(var msg: TMessage); message WM_REFILL_SPAREBUF;
    procedure ReplaceDialogQueryFind(Sender: TObject);
    procedure ReplaceDialogQueryReplace(Sender: TObject);
    procedure actCopyAsSQLExecute(Sender: TObject);
    procedure actSelectTreeBackgroundExecute(Sender: TObject);
    procedure popupQueryPopup(Sender: TObject);
    procedure lboxQueryHelpersClick(Sender: TObject);
    procedure lboxQueryHelpersDblClick(Sender: TObject);
    procedure tabsetQueryHelpersChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure btnDataClick(Sender: TObject);
    procedure ListTablesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
      const Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1CodeCompletion(Sender: TObject;
      var Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1Execute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
      var CanExecute: Boolean);
    procedure PageControlMainChange(Sender: TObject);
    procedure PageControlHostChange(Sender: TObject);
    procedure ValidateControls(Sender: TObject);
    procedure ValidateQueryControls(Sender: TObject);
    procedure RefreshQueryHelpers;
    function FieldContent(ds: TDataSet; ColName: WideString): WideString;
    procedure LoadDatabaseProperties(db: WideString);
    procedure ShowHost;
    procedure ShowDatabase(db: WideString);
    procedure ShowDBProperties(db: WideString);
    function EnsureFullWidth(Grid: TBaseVirtualTree; Column: TColumnIndex; Node: PVirtualNode): Boolean;
    procedure EnsureNodeLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode; WhereClause: WideString);
    procedure EnsureChunkLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode; FullWidth: Boolean = False);
    procedure DiscardNodeData(Sender: TVirtualStringTree; Node: PVirtualNode);
    procedure viewdata(Sender: TObject);
    procedure LogSQL(msg: WideString = ''; comment: Boolean = true );
    procedure CheckUptime;
    procedure KillProcess(Sender: TObject);
    procedure ExecSQLClick(Sender: TObject; Selection: Boolean = false;
      CurrentLine: Boolean=false);
    procedure SynMemoQueryStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure TimerHostUptimeTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ListTablesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
    procedure TimerConnectedTimer(Sender: TObject);
    procedure Clear2Click(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    procedure popupResultGridPopup(Sender: TObject);
    procedure AutoRefreshSetInterval(Sender: TObject);
    procedure AutoRefreshToggle(Sender: TObject);
    procedure SynMemoQueryDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoQueryDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TUnicodeStrings);
    procedure popupHostPopup(Sender: TObject);
    procedure Saveastextfile1Click(Sender: TObject);
    procedure popupDBPopup(Sender: TObject);
    procedure SaveDialogExportDataTypeChange(Sender: TObject);
    procedure popupDataGridPopup(Sender: TObject);
    procedure InsertDate(Sender: TObject);
    procedure setNULL1Click(Sender: TObject);
    function GetNamedVar( SQLQuery: WideString; x: WideString;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : WideString;
    function GetVar( SQLQuery: WideString; x: Integer = 0;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : WideString;
    function GetResults( SQLQuery: WideString;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false; ForceDialog: Boolean = false): TDataSet;
    function GetCol( SQLQuery: WideString; x: Integer = 0;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : WideStrings.TWideStringList;
    procedure ZSQLMonitor1LogTrace(Sender: TObject; Event: TZLoggingEvent);
    procedure MenuTablelistColumnsClick(Sender: TObject);
    procedure CancelQuery;
    procedure CheckConnection();
    procedure QueryLoad( filename: String; ReplaceContent: Boolean = true );
    procedure ExecuteNonQuery(SQLQuery: String);
    function ExecuteQuery(query: String): TDataSet;
    function CreateOrGetRemoteQueryTab(sender: THandle): THandle;
    procedure DataGridChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DataGridCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure DataGridEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure DataGridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex);
    procedure DataGridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; var Allowed: Boolean);
    procedure DataGridFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode:
        PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure DataGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DataGridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; NewText: WideString);
    procedure GridPaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure menuDeleteSnippetClick(Sender: TObject);
    procedure menuExploreClick(Sender: TObject);
    procedure menuInsertSnippetAtCursorClick(Sender: TObject);
    procedure menuLoadSnippetClick(Sender: TObject);
    procedure RunAsyncPost(ds: TDeferDataSet);
    procedure vstGetNodeDataSize(Sender: TBaseVirtualTree; var
        NodeDataSize: Integer);
    procedure vstInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
        PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree; Node:
        PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
        Boolean; var ImageIndex: Integer);
    procedure vstHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstHeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex;
        DropPosition: TPoint);
    procedure DBtreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DBtreeDblClick(Sender: TObject);
    procedure DBtreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var
        ImageIndex: Integer);
    procedure DBtreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize:
        Integer);
    procedure DBtreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure DBtreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var
        ChildCount: Cardinal);
    procedure DBtreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
        PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DBtreePaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure editFilterSearchChange(Sender: TObject);
    procedure editFilterSearchEnter(Sender: TObject);
    procedure editFilterSearchExit(Sender: TObject);
    procedure menuLogToFileClick(Sender: TObject);
    procedure menuOpenLogFolderClick(Sender: TObject);
    procedure vstGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var
        HintText: WideString);
    procedure ProcessSqlLog;
    procedure ListCommandStatsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure ListProcessesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure editFilterVTChange(Sender: TObject);
    procedure ListVariablesDblClick(Sender: TObject);
    procedure menuEditVariableClick(Sender: TObject);
    procedure menuTreeCollapseAllClick(Sender: TObject);
    procedure menuTreeExpandAllClick(Sender: TObject);
    procedure SynMemoFilterChange(Sender: TObject);
    procedure DataGridAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure menuShowSizeColumnClick(Sender: TObject);
    procedure DataGridColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure GridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure popupDataViewPopup(Sender: TObject);
    procedure menuViewDefaultClick(Sender: TObject);
    procedure menuViewSaveClick(Sender: TObject);
    procedure QueryGridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure pnlQueryHelpersCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure pnlQueryMemoCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure DataGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure File1Click(Sender: TObject);
    procedure ListVariablesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListStatusBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListProcessesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListCommandStatsBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure vstAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure actCopyOrCutExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure EnumerateRecentFilters;
    procedure LoadRecentFilter(Sender: TObject);
    procedure actCreateRoutineExecute(Sender: TObject);
    procedure DataGridScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure ListTablesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DBtreeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure actEditObjectExecute(Sender: TObject);
    procedure ListTablesDblClick(Sender: TObject);
    procedure DataGridClick(Sender: TObject);
    procedure panelTopDblClick(Sender: TObject);
    procedure PageControlMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actNewQueryTabExecute(Sender: TObject);
    procedure actCloseQueryTabExecute(Sender: TObject);
    procedure menuCloseQueryTab(Sender: TObject);
    procedure CloseQueryTab(PageIndex: Integer);
    procedure CloseButtonOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetMainTabAt(X, Y: Integer): Integer;
    procedure FixQueryTabCloseButtons;
    function QueryTabCloseButton(PageIndex: Integer): TPngSpeedButton;
    function QueryControl(PageIndex: Integer; Base: TControl): TControl;
    function ActiveQueryControl(Base: TControl): TControl;
    function ActiveQueryMemo: TSynMemo;
    function ActiveQueryHelpers: TTntListBox;
    function ActiveQueryTabset: TTabset;
    function QueryTabActive: Boolean;
    function IsQueryTab(PageIndex: Integer; IncludeFixed: Boolean): Boolean;
    procedure popupMainTabsPopup(Sender: TObject);
  private
    ReachedEOT                 : Boolean;
    FDelimiter: String;
    ServerUptime               : Integer;
    time_connected             : Cardinal;
    viewingdata                : Boolean;
    FMysqlConn                 : TMysqlConn;
    FConn                      : TOpenConnProf;
    QueryRunningInterlock      : Integer;
    UserQueryFired             : Boolean;
    UserQueryFiring            : Boolean;
    CachedTableLists           : WideStrings.TWideStringList;
    QueryHelpersSelectedItems  : Array[0..3] of Array of Integer;
    EditVariableForm           : TfrmEditVariable;
    FileNameSessionLog         : String;
    FileHandleSessionLog       : Textfile;
    SqlMessages                : TWideStringList;
    SqlMessagesLock            : TRtlCriticalSection;
    dsShowEngines,
    dsHaveEngines,
    dsCollations               : TDataset;
    FilterPanelManuallyOpened  : Boolean;
    winName                    : String;
    FSelectedTableColumns,
    FSelectedTableKeys     : TDataset;
    DataGridDB, DataGridTable  : WideString;
    PrevTableColWidths         : WideStrings.TWideStringList;
    DataGridHasChanges         : Boolean;
    InformationSchemaTables    : TWideStringlist;
    QueryMemoLineBreaks        : TLineBreaks;
    FLastMouseUpOnPageControl  : Cardinal;
    FLastTabNumberOnMouseUp    : Integer;
    FGridResults               : TObjectList;
    function GetParamValue(const paramChar: Char; const paramName:
      string; var curIdx: Byte; out paramValue: string): Boolean;
    procedure SetDelimiter(Value: String);
    function GetQueryRunning: Boolean;
    procedure SetQueryRunning(running: Boolean);
    procedure WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery; ForceDialog: Boolean);
    function RunThreadedQuery(AQuery: WideString; ForceDialog: Boolean): TMysqlQuery;
    procedure DisplayRowCountStats(MatchingRows: Int64 = -1);
    procedure insertFunction(Sender: TObject);
    function GetActiveDatabase: WideString;
    function GetSelectedTable: TListNode;
    procedure SetSelectedDatabase(db: WideString);
    procedure SelectDBObject(Text: WideString; NodeType: TListNodeType);
    procedure SetVisibleListColumns( List: TVirtualStringTree; Columns: WideStrings.TWideStringList );
    function GetTableSize(ds: TDataSet): Int64;
    procedure ToggleFilterPanel(ForceVisible: Boolean = False);
    function GetSelectedTableColumns: TDataset;
    function GetSelectedTableKeys: TDataset;
    procedure AutoCalcColWidths(Tree: TVirtualStringTree; PrevLayout: Widestrings.TWideStringlist = nil);
    procedure PlaceObjectEditor(Which: TListNodeType);
  public
    cancelling: Boolean;
    virtualDesktopName: string;
    MaintenanceForm: TOptimize;
    ViewEditor: TfrmView;
    UserManagerForm: TUserManagerForm;
    SelectDBObjectForm: TfrmSelectDBObject;
    SQLHelpForm: TfrmSQLhelp;
    RoutineEditor: TfrmRoutineEditor;
    OptionsForm: Toptionsform;
    DatabasesWanted,
    Databases                  : Widestrings.TWideStringList;
    TemporaryDatabase          : WideString;
    dataselected               : Boolean;
    editing                    : Boolean;
    mysql_version              : Integer;
    SessionName                : String;
    VTRowDataListVariables,
    VTRowDataListStatus,
    VTRowDataListProcesses,
    VTRowDataListCommandStats,
    VTRowDataListTables        : TVTreeDataArray;

    FProgressForm              : TFrmQueryProgress;

    // Variables set by preferences dialog
    prefRememberFilters        : Boolean;
    prefLogsqlnum,
    prefLogSqlWidth,
    prefMaxColWidth,
    prefMaxTotalRows           : Integer;
    prefCSVSeparator,
    prefCSVEncloser,
    prefCSVTerminator          : String[10];
    prefLogToFile,
    prefEnableBinaryEditor,
    prefEnableDatetimeEditor,
    prefEnableEnumEditor,
    prefEnableSetEditor,
    prefEnableNullBG           : Boolean;
    prefNullColorDefault,
    prefNullBG                 : TColor;
    CreateDatabaseForm         : TCreateDatabaseForm;
    TableEditor                : TfrmTableEditor;
    FDataGridSelect            : WideStrings.TWideStringList;
    FDataGridSort              : TOrderColArray;
    DataGridCurrentSelect,
    DataGridCurrentFullSelect,
    DataGridCurrentFrom,
    DataGridCurrentFilter,
    DataGridCurrentSort        : WideString;
    btnAddTab                  : TPngSpeedButton;

    property Delimiter: String read FDelimiter write SetDelimiter;
    procedure CallSQLHelpWithKeyword( keyword: String );
    procedure AddOrRemoveFromQueryLoadHistory( filename: String;
      AddIt: Boolean = true; CheckIfFileExists: Boolean = true );
    procedure popupQueryLoadClick( sender: TObject );
    procedure FillPopupQueryLoad;
    procedure PopupQueryLoadRemoveAbsentFiles( sender: TObject );
    procedure SessionConnect(Sender: TObject);
    function InitConnection(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases: WideString): Boolean;
    //procedure HandleQueryNotification(ASender : TMysqlQuery; AEvent : Integer);

    function ExecUpdateQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): Int64;
    function ExecSelectQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false; ForceDialog: Boolean = false): TDataSet;
    procedure ExecUseQuery(db: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false);

    property FQueryRunning: Boolean read GetQueryRunning write SetQueryRunning;
    function ActiveGrid: TVirtualStringTree;
    function GridResult(Grid: TBaseVirtualTree): TGridResult; overload;
    function GridResult(PageIndex: Integer): TGridResult; overload;
    function DataGridResult: TGridResult;
    property MysqlConn : TMysqlConn read FMysqlConn;
    property Conn : TOpenConnProf read FConn;

    property ActiveDatabase : WideString read GetActiveDatabase write SetSelectedDatabase;
    property SelectedTable : TListNode read GetSelectedTable;

    function FetchActiveDbTableList: TDataSet;
    function RefreshActiveDbTableList: TDataSet;
    function FetchDbTableList(db: WideString): TDataSet;
    function RefreshDbTableList(db: WideString): TDataSet;
    procedure ClearDbTableList(db: WideString);
    function DbTableListCachedAndValid(db: WideString): Boolean;
    procedure ClearAllTableLists;
    procedure EnsureDatabase;
    procedure TestVTreeDataArray( P: PVTreeDataArray );
    function GetVTreeDataArray( VT: TBaseVirtualTree ): PVTreeDataArray;
    procedure ActivateFileLogging;
    procedure DeactivateFileLogging;
    procedure TrimSQLLog;
    procedure TableEnginesCombo(var Combobox: TCombobox);
    function GetTreeNodeType(Node: PVirtualNode): TListNodeType;
    function GetFocusedTreeNodeType: TListNodeType;
    procedure RefreshTree(DoResetTableCache: Boolean; SelectDatabase: WideString = '');
    procedure RefreshTreeDB(db: WideString);
    function FindDBNode(db: WideString): PVirtualNode;
    function GridPostUpdate(Sender: TBaseVirtualTree): Boolean;
    function GridPostInsert(Sender: TBaseVirtualTree): Boolean;
    function GridPostDelete(Sender: TBaseVirtualTree): Boolean;
    function DataGridPostUpdateOrInsert(Node: PVirtualNode): Boolean;
    procedure GridFinalizeEditing(Sender: TBaseVirtualTree);
    function GetWhereClause(Row: PGridRow; Columns: PGridColumns): WideString;
    function GetKeyColumns: WideStrings.TWideStringlist;
    function CheckUniqueKeyClause: Boolean;
    procedure DataGridInsertRow;
    procedure DataGridCancel(Sender: TObject);
    property SelectedTableColumns: TDataset read GetSelectedTableColumns write FSelectedTableColumns;
    property SelectedTableKeys: TDataset read GetSelectedTableKeys write FSelectedTableKeys;
    procedure CalcNullColors;
    procedure FillDataViewPopup;
    procedure GetDataViews(List: TStrings);
    procedure DataViewClick(Sender: TObject);
    procedure LoadDataView(ViewName: String);
    function GetRegKeyTable: String;
    procedure SaveListSetup( List: TVirtualStringTree );
    procedure RestoreListSetup( List: TVirtualStringTree );
    function GetCollations(Items: TWideStrings = nil): TDataset;
    procedure SetEditorTabCaption(Editor: TFrame; ObjName: WideString);
    procedure ResetSelectedTableStuff;
end;


procedure InheritFont(AFont: TFont);


var
  MainForm            : TMainForm;
  AppVersion          : String = 'x.y';
  AppRevision         : String = '$Rev$';
  FullAppVersion      : String;
  DirnameCommonAppData,
  DirnameUserAppData,
  DIRNAME_SNIPPETS,
  DirnameSessionLogs  : String;

const
  discname = 'not connected';
  ICON_MYSELF_CONNECTED = 38;
  ICON_MYSELF_DISCONNECTED = -1;
  ICON_OTHER_CONNECTED = 36;
  ICON_OTHER_DISCONNECTED = -1;

{$I const.inc}

type TMyKey = record
  Name     : String;
  _type    : String;
  Columns  : TWideStringList;
  SubParts : TWideStringList;
end;

type
  // Represents errors already "handled" (shown to user),
  // which can thus safely be ignored.
  THandledSQLError = class(Exception)
  end;


implementation

uses
  About,
  connections,
  exportsql,
  loaddata,
  printlist,
  copytable,
  insertfiles,
  Threading,
  mysql_structures,
  UpdateCheck,
  uVistaFuncs,
  runsqlfile,
  column_selection,
  data_sorting,
  grideditlinks,
  dataviewsave;

type
  PMethod = ^TMethod;

{$R *.DFM}


procedure InheritFont(AFont: TFont);
begin
  AFont.Name := Mainform.Font.Name;
  AFont.Size := Mainform.Font.Size;
end;

procedure TMainForm.HandleWMComplete(var msg: TMessage);
begin
  HandleWMCompleteMessage(msg);
end;

procedure TMainForm.HandleWMCopyData(var msg: TWMCopyData);
begin
  HandleWMCopyDataMessage(msg);
end;

procedure TMainForm.HandleWMProcessLog(var msg: TMessage);
begin
  ProcessSqlLog;
end;

function TMainForm.ExecuteRemoteQuery(sender: THandle; query: string): TDataSet;
//var
  //tab: THandle;
begin
  // tab := TMDIChild(ActiveMDIChild).CreateOrGetRemoteQueryTab(sender);
  // TQueryTab(tab).AddText(query);
  // tab.ExecOrQueueQuery(query);
  result := ExecuteQuery(query);
end;

procedure TMainForm.ExecuteRemoteNonQuery(sender: THandle; query: string);
//var
  //tab: THandle;
begin
  // tab := TMDIChild(ActiveMDIChild).CreateOrGetRemoteQueryTab(sender);
  // TQueryTab(tab).AddText(query);
  // tab.ExecOrQueueQuery(query);
  ExecuteNonQuery(query);
end;

procedure TMainForm.showstatus(msg: string=''; panel: Integer=6);
begin
  // show Message in statusbar
  StatusBar.Panels[panel].Text := msg;
  StatusBar.Repaint;
end;

procedure TMainForm.refreshMonitorConfig;
var
  Screen: TScreen;
  Monitor: TMonitor;
  Name: String;
  i: Integer;
begin
  debug('main: Refresh monitor configuration.');
  // Monitors are enumerated when a TScreen is constructed;
  // so we have to construct a new TScreen.
  Screen := TScreen.Create(nil);
  Name := '';
  virtualDesktopName := 'WindowPos_';
  try
    for i := 1 to Screen.MonitorCount do begin
      Monitor := Screen.Monitors[i - 1];
      Name := Name +
        IntToStr(Monitor.Left) + 'x_' +
        IntToStr(Monitor.Top) + 'y_' +
        IntToStr(Monitor.Width) + 'w_' +
        IntToStr(Monitor.Height) + 'h'
      ;
    end;
    virtualDesktopName := virtualDesktopName + Name;
  finally
    Screen.Free;
  end;
end;

procedure TMainForm.saveWindowConfig;
var
  ws: String;
begin
  OpenRegistry;
  with MainReg do begin
    if OpenKey(REGPATH + virtualDesktopName + '\', True) then begin
      // Convert set to string.
      if WindowState = wsNormal then ws := 'Normal' else
      if WindowState = wsMinimized then ws := 'Minimized' else
      if WindowState = wsMaximized then ws := 'Maximized';
      // Set WindowState to normal to put the correct restore bounds in
      // Left, Top, Width and Height; the call is processed immediately.
      WindowState := wsNormal;
      // Write out the results.
      WriteString(REGNAME_WINDOWSTATE, ws);
      WriteInteger(REGNAME_WINDOWLEFT, Left);
      WriteInteger(REGNAME_WINDOWTOP, Top);
      WriteInteger(REGNAME_WINDOWWIDTH, Width);
      WriteInteger(REGNAME_WINDOWHEIGHT, Height);
    end;
  end;
end;

procedure TMainForm.loadWindowConfig;
var
  ws: String;
begin
  // Called on application start or when monitor configuration has changed.
  OpenRegistry;
  with MainReg do begin
    if not OpenKey(REGPATH + virtualDesktopName + '\', False) then begin
      // Switch to default configuration if nothing was stored.
      setDefaultWindowConfig;
    end else begin
      // If found, load stored configuration for MainForm.
      Left := ReadInteger(REGNAME_WINDOWLEFT);
      Top := ReadInteger(REGNAME_WINDOWTOP);
      Width := ReadInteger(REGNAME_WINDOWWIDTH);
      Height := ReadInteger(REGNAME_WINDOWHEIGHT);
      ws := ReadString(REGNAME_WINDOWSTATE);
      if ws = 'Normal' then WindowState := wsNormal else
      if ws = 'Minimized' then WindowState := wsMinimized else
      if ws = 'Maximized' then WindowState := wsMaximized;
    end;
  end;
end;

procedure TMainForm.setDefaultWindowConfig;
begin
  // If there are any default adjustments for the main form
  // when no window config is found, they should go here.
end;

procedure TMainForm.actExitApplicationExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.actFlushExecute(Sender: TObject);
var
  flushwhat: String;
begin
  flushwhat := UpperCase(TAction(Sender).Caption);
  delete(flushwhat, pos('&', flushwhat), 1);
  ExecUpdateQuery('FLUSH ' + flushwhat);
  if Sender = actFlushTableswithreadlock then begin
    MessageDlg(
      'Tables have been flushed and read lock acquired.'#10 +
      'Perform backup or snapshot of table data files now.'#10 +
      'Press OK to unlock when done...',
      mtInformation, [mbOk], 0
    );
    ExecUpdateQuery('UNLOCK TABLES');
  end;
end;

procedure TMainForm.DisplayChange(var msg: TMessage);
begin
  // At this point, the virtual desktop reconfiguration is complete,
  // but windows have not yet been resized and repositioned.
  //
  // HeidiSQL could save the current config here, and do a restore
  // after the automatic resize/reposition is done; this is signalled
  // by the first WM_WINDOWPOSCHANGED event to arrive after this procedure
  // has completed.
  //
  // However, that would require a complete save/restore for all windows,
  // not just the main window, so it would be a bit annoying to code.
  //
  // So for now, HeidiSQL trusts MS-Windows to replace windows correctly,
  // which has the slight annoyance factor that a user connecting with
  // remote desktop will have an automatic replacement applied instead
  // of a save/load transition using the last parameters for that virtual
  // desktop.

  // (no save here - see above.)
  refreshMonitorConfig;
  // (no wait for WindowPosChanged + load here - see above.)
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  filename : String;
begin
  DoDisconnect;
  EnterCriticalSection(SqlMessagesLock);
  FreeAndNil(SqlMessages);
  LeaveCriticalSection(SqlMessagesLock);

  OpenRegistry;
  // Position of Toolbars
  MainReg.WriteInteger(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
  MainReg.WriteInteger(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
  MainReg.WriteInteger(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
  MainReg.WriteInteger(REGNAME_TOOLBARDATATOP, ToolBarData.Top);
  MainReg.WriteInteger(REGNAME_TOOLBARQUERYLEFT, ToolBarQuery.Left);
  MainReg.WriteInteger(REGNAME_TOOLBARQUERYTOP, ToolBarQuery.Top);

  // Save delimiter
  MainReg.WriteString( REGNAME_DELIMITER, Delimiter );

  MainReg.WriteInteger( REGNAME_QUERYMEMOHEIGHT, pnlQueryMemo.Height );
  MainReg.WriteInteger( REGNAME_QUERYHELPERSWIDTH, pnlQueryHelpers.Width );
  MainReg.WriteInteger( REGNAME_DBTREEWIDTH, DBtree.width );
  MainReg.WriteInteger( REGNAME_SQLOUTHEIGHT, SynMemoSQLLog.Height );

  // Save width of probably resized columns of all VirtualTrees
  SaveListSetup(ListVariables);
  SaveListSetup(ListStatus);
  SaveListSetup(ListProcesses);
  SaveListSetup(ListCommandStats);
  SaveListSetup(ListTables);

  FreeAndNil(RoutineEditor);
  FreeAndNil(MaintenanceForm);
  FreeAndNil(UserManagerForm);
  FreeAndNil(ViewEditor);
  FreeAndNil(SelectDBObjectForm);
  FreeAndNil(SQLHelpForm);
  FreeAndNil(OptionsForm);

  debug('mem: clearing query and browse data.');
  SetLength(DataGridResult.Rows, 0);
  SetLength(DataGridResult.Columns, 0);

  Action := caFree;

  saveWindowConfig;

  filename := GetTempDir+'\'+APPNAME+'-preview.';
  if FileExists(filename+'html') then
    deletefile(filename+'html');
  if FileExists(filename+'jpg') then
    deletefile(filename+'jpg');
  if FileExists(filename+'gif') then
    deletefile(filename+'gif');
  if FileExists(filename+'bmp') then
    deletefile(filename+'bmp');
  if MainReg <> nil then begin
    MainReg.CloseKey;
    MainReg.Free;
  end;
end;


var
  spareMemory: Pointer = nil;

procedure HandleRuntimeError(ErrorCode: Byte; ErrorAddr: Pointer);
begin
  if spareMemory <> nil then FreeMem(spareMemory);
  debug('mem: released spare block.');
  spareMemory := nil;
  if MainForm <> nil then begin
    PostMessage(MainForm.Handle, WM_REFILL_SPAREBUF, 0, 0);
  end;
  raise Exception.Create('Runtime error ' + IntToStr(ErrorCode) + ' at ' + IntToHex(Cardinal(ErrorAddr), 8) + '.');
end;

procedure SpareBufRefill;
begin
  debug('mem: reallocating spare block.');
  if spareMemory = nil then spareMemory := AllocMem(6543210);
end;

procedure TMainForm.HandleWMRefill(var msg: TMessage);
begin
  SpareBufRefill;
end;


{***
  OnCreate Event
  Important to set the windowstate here instead of in OnShow
  because possible windowstate-switching is done with an animation
  if set in Windows. This animation takes some milliseconds
  to complete and can be annoying.
}
procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  menuitem : TMenuItem;
  fontname, datafontname : String;
  fontsize, datafontsize : Integer;
  DisableProcessWindowsGhostingProc: procedure;
begin
  caption := APPNAME;
  setLocales;

  // Make Vista miniature window work.
  //Application.MainFormOnTaskBar := True;

  // Use new Vista dialogs per default.
  //UseLatestCommonDialogs := True;

  SpareBufRefill;
  ErrorProc := HandleRuntimeError;

  refreshMonitorConfig;
  loadWindowConfig;

  // Beautify AppRevision
  if Pos('$Rev: WC', AppRevision) < 1 then
    AppRevision := 'unknown'
  else begin
    AppRevision := StringReplace( AppRevision, '$Rev: WC', '', [rfIgnoreCase] );
    AppRevision := StringReplace( AppRevision, '$', '', [] );
    AppRevision := Trim( AppRevision );
  end;
  // Compose full version string
  FullAppVersion := 'Version ' + AppVersion + ', Revision ' + AppRevision;

  // "All users" folder for HeidiSQL's data (All Users\Application Data)
  DirnameCommonAppData := GetShellFolder(CSIDL_COMMON_APPDATA) + '\' + APPNAME + '\';

  // User folder for HeidiSQL's data (<user name>\Application Data)
  DirnameUserAppData := GetShellFolder(CSIDL_APPDATA) + '\' + APPNAME + '\';
  // Ensure directory exists
  ForceDirectories(DirnameUserAppData);

  // Folder which contains snippet-files
  DIRNAME_SNIPPETS := DirnameCommonAppData + 'Snippets\';

  // Folder for session logfiles
  DirnameSessionLogs := DirnameUserAppData + 'Sessionlogs\';

  QueryRunningInterlock := 0;
  UserQueryFired := False;
  UserQueryFiring := False;
  TemporaryDatabase := '';

  // SQLFiles-History
  FillPopupQueryLoad;

  CachedTableLists := WideStrings.TWideStringList.Create;

  InitializeCriticalSection(SqlMessagesLock);
  EnterCriticalSection(SqlMessagesLock);
  SqlMessages := TWideStringList.Create;
  LeaveCriticalSection(SqlMessagesLock);

  Delimiter := GetRegValue(REGNAME_DELIMITER, DEFAULT_DELIMITER);

  // Delphi work around to force usage of Vista's default font (other OSes will be unaffected)
  SetVistaFonts(Font);
  InheritFont(Font);
  InheritFont(tabsetQueryHelpers.Font);
  InheritFont(SynCompletionProposal1.Font);

  // Fix node height on Virtual Trees for current DPI settings
  FixVT(DBTree);
  FixVT(ListVariables);
  FixVT(ListStatus);
  FixVT(ListProcesses);
  FixVT(ListCommandStats);
  FixVT(ListTables);

  // Position of Toolbars
  ToolBarStandard.Left := GetRegValue(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
  ToolBarStandard.Top := GetRegValue(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
  ToolBarData.Left := GetRegValue(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
  ToolBarData.Top := GetRegValue(REGNAME_TOOLBARDATATOP, ToolBarData.Top);
  ToolBarQuery.Left := GetRegValue(REGNAME_TOOLBARQUERYLEFT, ToolBarQuery.Left);
  ToolBarQuery.Top := GetRegValue(REGNAME_TOOLBARQUERYTOP, ToolBarQuery.Top);

  pnlQueryMemo.Height := GetRegValue(REGNAME_QUERYMEMOHEIGHT, pnlQueryMemo.Height);
  pnlQueryHelpers.Width := GetRegValue(REGNAME_QUERYHELPERSWIDTH, pnlQueryHelpers.Width);
  DBtree.Width := GetRegValue(REGNAME_DBTREEWIDTH, DBtree.Width);
  SynMemoSQLLog.Height := GetRegValue(REGNAME_SQLOUTHEIGHT, SynMemoSQLLog.Height);
  // Force status bar position to below log memo 
  StatusBar.Top := SynMemoSQLLog.Top + SynMemoSQLLog.Height;
  prefMaxColWidth := GetRegValue(REGNAME_MAXCOLWIDTH, DEFAULT_MAXCOLWIDTH);
  prefMaxTotalRows := GetRegValue(REGNAME_MAXTOTALROWS, DEFAULT_MAXTOTALROWS);
  // Fix registry entry from older versions which can have 0 here which makes no sense
  // since the autosetting was removed
  if prefMaxColWidth <= 0 then
    prefMaxColWidth := DEFAULT_MAXCOLWIDTH;
  prefLogsqlnum := GetRegValue(REGNAME_LOGSQLNUM, DEFAULT_LOGSQLNUM);
  prefLogSqlWidth := GetRegValue(REGNAME_LOGSQLWIDTH, DEFAULT_LOGSQLWIDTH);
  prefCSVSeparator := GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  prefCSVEncloser := GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  prefCSVTerminator := GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  prefRememberFilters := GetRegValue(REGNAME_REMEMBERFILTERS, DEFAULT_REMEMBERFILTERS);

  // SQL-Font:
  fontname := GetRegValue(REGNAME_FONTNAME, DEFAULT_FONTNAME);
  fontsize := GetRegValue(REGNAME_FONTSIZE, DEFAULT_FONTSIZE);
  SynMemoQuery.Font.Name := fontname;
  SynMemoQuery.Font.Size := fontsize;
  SynMemoQuery.Gutter.Font.Name := fontname;
  SynMemoQuery.Gutter.Font.Size := fontsize;
  SynMemoFilter.Font.Name := fontname;
  SynMemoFilter.Font.Size := fontsize;
  SynMemoSQLLog.Font.Name := fontname;
  SynMemoSQLLog.Font.Size := fontsize;
  SynMemoSQLLog.Gutter.Font.Name := fontname;
  SynMemoSQLLog.Gutter.Font.Size := fontsize;
  SynMemoProcessView.Font.Name := fontname;
  SynMemoProcessView.Font.Size := fontsize;

  // Data-Font:
  datafontname := GetRegValue(REGNAME_DATAFONTNAME, DEFAULT_DATAFONTNAME);
  datafontsize := GetRegValue(REGNAME_DATAFONTSIZE, DEFAULT_DATAFONTSIZE);
  DataGrid.Font.Name := datafontname;
  QueryGrid.Font.Name := datafontname;
  DataGrid.Font.Size := datafontsize;
  QueryGrid.Font.Size := datafontsize;
  FixVT(DataGrid);
  FixVT(QueryGrid);
  // Load color settings
  DatatypeCategories[Integer(dtcInteger)].Color := GetRegValue(REGNAME_FIELDCOLOR_NUMERIC, DEFAULT_FIELDCOLOR_NUMERIC);
  DatatypeCategories[Integer(dtcReal)].Color := GetRegValue(REGNAME_FIELDCOLOR_NUMERIC, DEFAULT_FIELDCOLOR_NUMERIC);
  DatatypeCategories[Integer(dtcText)].Color := GetRegValue(REGNAME_FIELDCOLOR_TEXT, DEFAULT_FIELDCOLOR_TEXT);
  DatatypeCategories[Integer(dtcBinary)].Color := GetRegValue(REGNAME_FIELDCOLOR_BINARY, DEFAULT_FIELDCOLOR_BINARY);
  DatatypeCategories[Integer(dtcTemporal)].Color := GetRegValue(REGNAME_FIELDCOLOR_DATETIME, DEFAULT_FIELDCOLOR_DATETIME);
  DatatypeCategories[Integer(dtcIntegerNamed)].Color := GetRegValue(REGNAME_FIELDCOLOR_ENUM, DEFAULT_FIELDCOLOR_ENUM);
  DatatypeCategories[Integer(dtcSet)].Color := GetRegValue(REGNAME_FIELDCOLOR_SET, DEFAULT_FIELDCOLOR_SET);
  DatatypeCategories[Integer(dtcSetNamed)].Color := GetRegValue(REGNAME_FIELDCOLOR_SET, DEFAULT_FIELDCOLOR_SET);
  prefNullBG := GetRegValue(REGNAME_BG_NULL, DEFAULT_BG_NULL);
  CalcNullColors;
  // Editor enablings
  prefEnableBinaryEditor := GetRegValue(REGNAME_FIELDEDITOR_BINARY, DEFAULT_FIELDEDITOR_BINARY);
  prefEnableDatetimeEditor := GetRegValue(REGNAME_FIELDEDITOR_DATETIME, DEFAULT_FIELDEDITOR_DATETIME);
  prefEnableEnumEditor := GetRegValue(REGNAME_FIELDEDITOR_ENUM, DEFAULT_FIELDEDITOR_ENUM);
  prefEnableSetEditor := GetRegValue(REGNAME_FIELDEDITOR_SET, DEFAULT_FIELDEDITOR_SET);
  prefEnableNullBG := GetRegValue(REGNAME_BG_NULL_ENABLED, DEFAULT_BG_NULL_ENABLED);

  // Color coding:
  RestoreSyneditStyles(SynSQLSyn1);
  SynMemoQuery.ActiveLineColor := StringToColor(GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));

  // Switch off/on displaying table/db sized in tree
  menuShowSizeColumn.Checked := GetRegValue(REGNAME_SIZECOL_TREE, DEFAULT_SIZECOL_TREE);
  if menuShowSizeColumn.Checked then
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options + [coVisible]
  else
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options - [coVisible];

  // Restore width of columns of all VirtualTrees
  RestoreListSetup(ListVariables);
  RestoreListSetup(ListStatus);
  RestoreListSetup(ListProcesses);
  RestoreListSetup(ListCommandStats);
  RestoreListSetup(ListTables);

  // Generate menuitems for popupDbGridHeader (column selection for ListTables)
  popupDBGridHeader.Items.Clear;
  for i:=0 to ListTables.Header.Columns.Count-1 do
  begin
    menuitem := TMenuItem.Create( popupDBGridHeader );
    menuitem.Caption := ListTables.Header.Columns[i].Text;
    menuitem.OnClick := MenuTablelistColumnsClick;
    // Disable hiding first column
    menuitem.Enabled := i>0;
    menuitem.Checked := coVisible in ListTables.Header.Columns[i].Options;
    popupDbGridHeader.Items.Add( menuitem );
  end;

  // Place progressbar on the statusbar
  ProgressBarStatus.Parent := StatusBar;
  ProgressBarStatus.Visible := False;

  // Work around Vistas ghosting feature breaking the GUI
  DisableProcessWindowsGhostingProc := GetProcAddress(
    GetModuleHandle('user32.dll'),
    'DisableProcessWindowsGhosting');
  if Assigned(DisableProcessWindowsGhostingProc) then
    DisableProcessWindowsGhostingProc;

  QueryMemoLineBreaks := lbsNone;

  FGridResults := TObjectList.Create;
  // Add two static results for the Data and Query tab. Results for added query tabs will be created on demand.
  FGridResults.Add(TGridResult.Create);
  FGridResults.Add(TGridResult.Create);

  btnAddTab := TPngSpeedButton.Create(PageControlMain);
  btnAddTab.Parent := PageControlMain;
  btnAddTab.PngImage := PngImageListMain.PngImages[actNewQueryTab.ImageIndex].PngImage;
  btnAddTab.Height := PageControlMain.TabHeight - 2;
  btnAddTab.Width := btnAddTab.Height;
  btnAddTab.Flat := True;
  btnAddTab.Hint := actNewQueryTab.Hint;
  btnAddTab.OnClick := actNewQueryTab.OnExecute;
end;


{**
  Check for connection parameters on commandline or show connections form.
}
procedure TMainForm.Startup;
var
  curParam : Byte;
  sValue,
  parHost, parPort, parUser, parPass, parDatabase,
  parTimeout, parCompress, parDescription : String;
  LastUpdatecheck, LastStatsCall, LastConnect: TDateTime;
  UpdatecheckInterval, i: Integer;
  DefaultLastrunDate, LastSession, StatsURL: String;
  frm : TfrmUpdateCheck;
  dlgResult: Integer;
  Connected, CommandLineMode, DecideForStatistic: Boolean;
  ConnForm: TConnForm;
  StatsCall: TDownloadUrl2;
  SessionNames: TStringlist;
begin
  DefaultLastrunDate := '2000-01-01';

  // Do an updatecheck if checked in settings
  if GetRegValue(REGNAME_DO_UPDATECHECK, DEFAULT_DO_UPDATECHECK) then begin
    try
      LastUpdatecheck := StrToDateTime( GetRegValue(REGNAME_LAST_UPDATECHECK, DefaultLastrunDate) );
    except
      LastUpdatecheck := StrToDateTime( DefaultLastrunDate );
    end;
    UpdatecheckInterval := GetRegValue(REGNAME_UPDATECHECK_INTERVAL, DEFAULT_UPDATECHECK_INTERVAL);
    if DaysBetween(Now, LastUpdatecheck) >= UpdatecheckInterval then begin
      frm := TfrmUpdateCheck.Create(Self);
      frm.AutoClose := True;
      frm.CheckForBuildsInAutoMode := GetRegValue(REGNAME_DO_UPDATECHECK_BUILDS, DEFAULT_DO_UPDATECHECK_BUILDS);
      frm.ShowModal;
      FreeAndNil(frm);
    end;
  end;

  // Call user statistics if checked in settings
  if GetRegValue(REGNAME_DO_STATISTICS, DEFAULT_DO_STATISTICS) then begin
    try
      LastStatsCall := StrToDateTime( GetRegValue(REGNAME_LAST_STATSCALL, DefaultLastrunDate) );
    except
      LastStatsCall := StrToDateTime( DefaultLastrunDate );
    end;
    if DaysBetween(Now, LastStatsCall) >= 30 then begin
      // Report used SVN revision
      StatsURL := APPDOMAIN + 'savestats.php?c=' + AppRevision;
      // Enumerate actively used server versions
      SessionNames := TStringlist.Create;
      if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, true) then
        MainReg.GetKeyNames(SessionNames);
      for i:=0 to SessionNames.Count-1 do begin
        try
          LastConnect := StrToDateTime(GetRegValue(REGNAME_LASTCONNECT, DefaultLastrunDate, SessionNames[i]));
        except
          LastConnect := StrToDateTime(DefaultLastrunDate);
        end;
        if LastConnect > LastStatsCall then begin
          StatsURL := StatsURL + '&s[]=' + IntToStr(GetRegValue(REGNAME_SERVERVERSION, 0, SessionNames[i]));
        end;
      end;
      StatsCall := TDownloadUrl2.Create(Self);
      StatsCall.URL := StatsURL;
      StatsCall.SetUserAgent(APPNAME + ' ' + FullAppVersion);
      try
        StatsCall.ExecuteTarget(nil);
        OpenRegistry;
        MainReg.WriteString(REGNAME_LAST_STATSCALL, DateTimeToStr(Now));
      except
        // Silently ignore it when the url could not be called over the network.
      end;
      FreeAndNil(StatsCall);
    end;
  end;

  // Ask if we shall activate statistic calls. Would be used by noone otherwise.
  OpenRegistry;
  if not Mainreg.ValueExists(REGNAME_DO_STATISTICS) then begin
    DecideForStatistic := MessageDlg(APPNAME + ' has a new statistics feature: If activated, server and client versions '+
      'are reported once per month and displayed on heidisql.com.'+CRLF+CRLF+'Activate this feature?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    Mainreg.WriteBool(REGNAME_DO_STATISTICS, DecideForStatistic);
  end;


  Connected := False;

  // Check commandline if parameters were passed. Otherwise show connections windows
  curParam := 1;
  while curParam <= ParamCount do begin
    // -M and -d are choosen not to conflict with mysql.exe
    // http://dev.mysql.com/doc/refman/5.0/en/mysql-command-options.html
    //
    // To test all supported variants, set Run > Parameters > Parameters option to:
    // --host=192.168.0.1 --user=root --password -d "My session name" -D"test db" -C -P 2200
    if GetParamValue('h', 'host', curParam, sValue) then
      parHost := sValue
    else if GetParamValue('P', 'port', curParam, sValue) then
      parPort := sValue
    else if GetParamValue('C', 'compress', curParam, sValue) then
      parCompress := sValue
    else if GetParamValue('M', 'timeout', curParam, sValue) then
      parTimeout := sValue
    else if GetParamValue('u', 'user', curParam, sValue) then
      parUser := sValue
    else if GetParamValue('p', 'password', curParam, sValue) then
      parPass := sValue
    else if GetParamValue('D', 'database', curParam, sValue) then
      parDatabase := sValue
    else if GetParamValue('d', 'description', curParam, sValue) then
      parDescription := sValue;
    Inc(curParam);
  end;

  // Find stored session if -dSessionName was passed
  if (parDescription <> '') and (MainReg.OpenKey(REGPATH + REGKEY_SESSIONS + parDescription, False)) then begin
    parHost := GetRegValue(REGNAME_HOST, DEFAULT_HOST, parDescription);
    parUser := GetRegValue(REGNAME_USER, DEFAULT_USER, parDescription);
    parPass := decrypt(GetRegValue(REGNAME_PASSWORD, DEFAULT_PASSWORD, parDescription));
    parPort := GetRegValue(REGNAME_PORT, IntToStr(DEFAULT_PORT), parDescription);
    parTimeout := GetRegValue(REGNAME_TIMEOUT, IntToStr(DEFAULT_TIMEOUT), parDescription);
    parCompress := IntToStr(Integer(GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, parDescription)));
    parDatabase := GetRegValue(REGNAME_ONLYDBS, '', parDescription);
  end;

  // Minimal parameter for command line mode is hostname
  CommandLineMode := parHost <> '';
  if CommandLineMode then begin
    Connected := InitConnection(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, IntToStr(Integer(DEFAULT_ONLYDBSSORTED)));
    if Connected then begin
      SessionName := parDescription;
      if SessionName = '' then
        SessionName := parHost;
    end;
  end;

  // Auto connection via preference setting
  // Do not autoconnect if we're in commandline mode and the connection was not successful
  if (not CommandLineMode) and (not Connected) and GetRegValue(REGNAME_AUTORECONNECT, DEFAULT_AUTORECONNECT) then begin
    LastSession := GetRegValue(REGNAME_LASTSESSION, '');
    if LastSession <> '' then begin
      Connected := InitConnection(
        GetRegValue(REGNAME_HOST, '', LastSession),
        GetRegValue(REGNAME_PORT, '', LastSession),
        GetRegValue(REGNAME_USER, '', LastSession),
        decrypt(GetRegValue(REGNAME_PASSWORD, '', LastSession)),
        Utf8Decode(GetRegValue(REGNAME_ONLYDBS, '', LastSession)),
        GetRegValue(REGNAME_TIMEOUT, '', LastSession),
        IntToStr(Integer(GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, LastSession))),
        IntToStr(Integer(GetRegValue(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED, LastSession)))
        );
      if Connected then
        SessionName := LastSession;
    end;
  end;

  // Display session manager
  if not Connected then begin
    // Cannot be done in OnCreate because we need ready forms here:
    ConnForm := TConnForm.Create(Self);
    dlgResult := ConnForm.ShowModal;
    FreeAndNil(ConnForm);
    if dlgResult = mrCancel then begin
      Close;
      Halt;
    end;
  end;

  DoAfterConnect;

  if (not CommandLineMode) and (ParamStr(1) <> '') then begin
    // Loading SQL file by command line. Mutually exclusive to connect by command line.
    QueryLoad(ParamStr(1));
  end;
end;


procedure TMainForm.actSessionManagerExecute(Sender: TObject);
var
  ConnForm: TConnForm;
begin
  ConnForm := TConnForm.Create(Self);
  if ConnForm.ShowModal <> mrCancel then
    DoAfterConnect;
  FreeAndNil(ConnForm);
end;


procedure TMainForm.DoAfterConnect;
var
  i, j: Integer;
  lastUsedDB: WideString;
  v: String[50];
  v1, v2, v3: String;
  rx: TRegExpr;
  functioncats : TStringList;
  miGroup,
  miFilterGroup,
  miFunction,
  miFilterFunction: TMenuItem;
begin
  DataGridHasChanges := False;

  // Activate logging
  if GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE) then
    ActivateFileLogging;

  TimerConnected.Enabled := true;
  LogSQL('Connected. Thread-ID: ' + IntToStr( MySQLConn.Connection.GetThreadId ));

  // Detect server version
  // Be careful with version suffixes, for example: '4.0.31-20070605_Debian-5-log'
  v := GetVar( 'SELECT VERSION()' );
  rx := TRegExpr.Create;
  rx.ModifierG := True;
  rx.Expression := '^(\d+)\.(\d+)\.(\d+)';
  if rx.Exec(v) then begin
    v1 := rx.Match[1];
    v2 := rx.Match[2];
    v3 := rx.Match[3];
  end;
  rx.Free;
  mysql_version := MakeInt(v1) *10000 + MakeInt(v2) *100 + MakeInt(v3);
  tabHost.Caption := 'Host: '+MySQLConn.Connection.HostName;
  showstatus('MySQL '+v1+'.'+v2+'.'+v3, 3);

  // Save server version
  OpenRegistry(SessionName);
  Mainreg.WriteInteger(REGNAME_SERVERVERSION, mysql_version);
  Mainreg.WriteString(REGNAME_LASTCONNECT, DateTimeToStr(Now));

  DatabasesWanted := explode(';', FConn.DatabaseList);
  if FConn.DatabaseListSort then
    DatabasesWanted.Sort;

  DBTree.Color := GetRegValue(REGNAME_TREEBACKGROUND, clWindow, SessionName);

  CheckUptime;
  // Invoke population of database tree. It's important to do this here after
  // having filled DatabasesWanted, not at design time.
  DBtree.RootNodeCount := 1;

  // Define window properties
  SetWindowConnected( true );
  i := SetWindowName( SessionName );
  winName := SessionName;
  if ( i <> 0 ) then
  begin
    winName := winName + Format( ' (%d)', [i] );
  end;

  // Reselect last used database
  if GetRegValue( REGNAME_RESTORELASTUSEDDB, DEFAULT_RESTORELASTUSEDDB ) then begin
    lastUsedDB := Utf8Decode(GetRegValue(REGNAME_LASTUSEDDB, '', SessionName));
    if lastUsedDB <> '' then try
      ActiveDatabase := lastUsedDB;
    except
      // Suppress exception message when db was dropped externally or
      // the session was just opened with "OnlyDBs" in place and the
      // last db is not contained in this list.
    end;
  end;
  // By default, select the host node
  if not Assigned(DBtree.FocusedNode) then begin
    DBtree.Selected[DBtree.GetFirst] := true;
    DBtree.FocusedNode := DBtree.GetFirst;
  end;

  // Create function menu items in popupQuery and popupFilter
  for i:=popupQuery.Items.Count-1 downto 0 do begin
    if popupQuery.Items[i].Caption = '-' then
      break;
    popupQuery.Items.Delete(i);
  end;
  for i:=popupFilter.Items.Count-1 downto 0 do begin
    if popupFilter.Items[i].Caption = '-' then
      break;
    popupFilter.Items.Delete(i);
  end;
  functioncats := GetFunctionCategories;
  for i:=0 to functioncats.Count-1 do begin
    // Create a menu item which gets subitems later
    miGroup := TMenuItem.Create(popupQuery);
    miGroup.Caption := functioncats[i];
    popupQuery.Items.add(miGroup);
    miFilterGroup := TMenuItem.Create(popupFilter);
    miFilterGroup.Caption := miGroup.Caption;
    popupFilter.Items.add(miFilterGroup);
    for j:=0 to Length(MySqlFunctions)-1 do begin
      if MySqlFunctions[j].Category <> functioncats[i] then
        continue;
      miFunction := TMenuItem.Create(popupQuery);
      miFunction.Caption := MySqlFunctions[j].Name;
      miFunction.ImageIndex := 13;
      // Prevent generating a hotkey
      miFunction.Caption := StringReplace(miFunction.Caption, '&', '&&', [rfReplaceAll]);
      // Prevent generating a seperator line
      if miFunction.Caption = '-' then
        miFunction.Caption := '&-';
      miFunction.Hint := MySqlFunctions[j].Name + MySqlFunctions[j].Declaration;
      // Take care of needed server version
      if MySqlFunctions[j].Version <= mysql_version then begin
        if MySqlFunctions[j].Description <> '' then
          miFunction.Hint := miFunction.Hint + ' - ' + Copy(MySqlFunctions[j].Description, 0, 200 );
        miFunction.Tag := j;
        // Place menuitem on menu
        miFunction.OnClick := insertFunction;
      end else begin
        miFunction.Hint := miFunction.Hint + ' - ('+STR_NOTSUPPORTED+', needs >= '+ConvertServerVersion(MySqlFunctions[j].Version)+')';
        miFunction.Enabled := False;
      end;
      // Prevent generating a seperator for ShortHint and LongHint
      miFunction.Hint := StringReplace( miFunction.Hint, '|', '¦', [rfReplaceAll] );
      miGroup.Add(miFunction);
      // Create a copy of the menuitem for popupFilter
      miFilterFunction := TMenuItem.Create(popupFilter);
      miFilterFunction.Caption := miFunction.Caption;
      miFilterFunction.Hint := miFunction.Hint;
      miFilterFunction.ImageIndex := miFunction.ImageIndex;
      miFilterFunction.Tag := miFunction.Tag;
      miFilterFunction.OnClick := miFunction.OnClick;
      miFilterFunction.Enabled := miFunction.Enabled;
      miFilterGroup.Add(miFilterFunction);
    end;
  end;
end;


procedure TMainForm.DoDisconnect;
begin
  // Open server-specific registry-folder.
  // relative from already opened folder!
  OpenRegistry(SessionName);
  MainReg.WriteString( REGNAME_LASTUSEDDB, Utf8Encode(ActiveDatabase) );

  // Post pending UPDATE
  if DataGridHasChanges then
    actDataPostChangesExecute(Self);

  // Clear database and table lists
  DBtree.ClearSelection;
  DBtree.FocusedNode := nil;
  DBtree.Clear;
  ClearAllTableLists;
  FreeAndNil(DatabasesWanted);
  FreeAndNil(Databases);
  FreeAndNil(InformationSchemaTables);
  FreeAndNil(dsShowEngines);
  FreeAndNil(dsHaveEngines);
  FreeAndNil(dsCollations);

  // Free forms which use session based datasets, fx dsShowEngines
  FreeAndNil(TableEditor);
  FreeAndNil(CreateDatabaseForm);

  // Closing connection
  if Assigned(FMysqlConn) then begin
    LogSQL('Closing connection to "'+SessionName+'" session (' + FMysqlConn.Connection.hostname + ') ...');
    FMysqlConn.Disconnect;
    FreeAndNil(FMysqlConn);
  end;

  if prefLogToFile then
    DeactivateFileLogging;

  // Invalidate list contents
  ListVariables.Tag := VTREE_NOTLOADED;
  ListStatus.Tag := VTREE_NOTLOADED;
  ListProcesses.Tag := VTREE_NOTLOADED;
  ListCommandstats.Tag := VTREE_NOTLOADED;

  SetWindowConnected( false );
  SetWindowName( main.discname );
  Application.Title := APPNAME;

  TimerConnected.Enabled := False;
  time_connected := 0;
  TimerHostUptime.Enabled := False;
end;


procedure TMainForm.actCreateDatabaseExecute(Sender: TObject);
var
  newdb: String;
begin
  // Create database:
  // Create modal form once on demand
  if CreateDatabaseForm = nil then
    CreateDatabaseForm := TCreateDatabaseForm.Create(Self);

  // Rely on the modalresult being set correctly
  if CreateDatabaseForm.ShowModal = mrOK then
  begin
    newdb := CreateDatabaseForm.editDBName.Text;
    // Add DB to OnlyDBs-regkey if this is not empty
    if DatabasesWanted.Count > 0 then
    begin
      DatabasesWanted.Add( newdb );
      OpenRegistry(SessionName);
      MainReg.WriteString( 'OnlyDBs', ImplodeStr( ';', DatabasesWanted ) );
    end;
    // reload db nodes and switch to new one
    RefreshTree(False, newdb);
  end;
end;


procedure TMainForm.actImportCSVExecute(Sender: TObject);
begin
  // Import Textfile
  loaddataWindow(self);
end;

procedure TMainForm.actPreferencesExecute(Sender: TObject);
begin
  // Preferences
  if OptionsForm = nil then
    OptionsForm := Toptionsform.Create(Self);
  OptionsForm.ShowModal;
end;

procedure TMainForm.actReadmeExecute(Sender: TObject);
begin
  // show readme.txt
  ShellExec( 'readme.txt', ExtractFilePath(paramstr(0)) );
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  i, room: Integer;
  PanelRect: TRect;
begin
  room := 0;
  for i := 1 to Statusbar.Panels.Count - 1 do
    inc(room, Statusbar.Panels[i].Width);
  StatusBar.Panels[0].Width := Statusbar.Width - room;
  // Retreive the rectancle of the statuspanel (in our case the fifth panel)
  SendMessage(StatusBar.Handle, SB_GETRECT, 5, Integer(@PanelRect));
  // Position the progressbar over the panel on the statusbar
  with PanelRect do
    ProgressBarStatus.SetBounds(Left, Top, Right-Left, Bottom-Top);
  lblDataTop.Width := pnlDataTop.Width - tlbDataButtons.Width - 10;
end;

procedure TMainForm.actUserManagerExecute(Sender: TObject);
begin
  if UserManagerForm = nil then
    UserManagerForm := TUserManagerForm.Create(Self);
  if UserManagerForm.TestUserAdmin then
    UserManagerForm.ShowModal;
end;

procedure TMainForm.menuWindowClick(Sender: TObject);
var
  i: integer;
  list: TWindowDataArray;
  item: TMenuItem;
begin
  // Delete dynamically added connection menu items.
  // NOTE: The menu doesn't like having 0 items, so we keep one which we delete later.
  for i := menuWindow.Count - 1 downto 1 do menuWindow.Delete(i);

  // Check if all the heidisql windows are still alive.
  CheckForCrashedWindows;

  // Fetch the list of windows.
  list := GetWindowList;

  // TODO: Load "all" array with all connections

  // Re-create dynamic menu items.
  for i := 0 to High(list) do with list[i] do begin
    // TODO: Remove connection with this UID from "all" array
    item := TMenuItem.Create(self);
    if namePostfix <> 0 then name := name + Format(' (%d)', [namePostFix]);
    item.Caption := name;
    if (appHandle = Handle) and (connected) then item.ImageIndex := ICON_MYSELF_CONNECTED
    else if (appHandle = Handle) and (not connected) then item.ImageIndex := ICON_MYSELF_DISCONNECTED
    else if (appHandle <> Handle) and (connected) then item.ImageIndex := ICON_OTHER_CONNECTED
    else if (appHandle <> Handle) and (not connected) then item.ImageIndex := ICON_OTHER_DISCONNECTED;
    item.Tag := appHandle;
    item.OnClick := focusWindow;
    menuWindow.Add(item);
  end;
  // NOTE: The menu breaks if it has 0 items at any point.  Therefore we delete item 0 as the last thing.
  //       Perhaps later the Window menu will contain more items, for now it's initially filled with a fake menu item.
  menuWindow.Delete(0);
end;

procedure TMainForm.actAboutBoxExecute(Sender: TObject);
begin
  // Info-Box
  AboutWindow (Self);
end;

procedure TMainForm.actClearEditorExecute(Sender: TObject);
var
  m: TSynMemo;
begin
  if Sender = actClearQueryEditor then
    m := ActiveQueryMemo
  else begin
    m := SynMemoFilter;
    editFilterSearch.Clear;
  end;
  m.SelectAll;
  m.SelText := '';
  m.SelStart := 0;
  m.SelEnd := 0;
end;

procedure TMainForm.actMaintenanceExecute(Sender: TObject);
begin
  // optimize / repair... tables
  if MaintenanceForm = nil then
    MaintenanceForm := TOptimize.Create(Self);
  MaintenanceForm.ShowModal;
end;


{**
  Create a view
}
procedure TMainForm.actCreateViewExecute(Sender: TObject);
begin
  tabEditor.TabVisible := True;
  PagecontrolMain.ActivePage := tabEditor;
  PlaceObjectEditor(lntView);
  ViewEditor.Init;
end;


{**
  Edit view
}
procedure TMainForm.actPrintListExecute(Sender: TObject);
var
  f: TForm;
begin
  // Print contents of a list or grid
  f := TPrintlistForm.Create(Self);
  f.ShowModal;
  FreeAndNil(f);
end;


procedure TMainForm.actCopyTableExecute(Sender: TObject);
begin
  // copy table
  CopyTableWindow(self);
end;


procedure TMainForm.focusWindow(Sender: TObject);
begin
  ActivateWindow((Sender as TMenuItem).Tag);
end;


procedure TMainForm.menuConnectionsPopup(Sender: TObject);
var
  i: integer;
  list: TWindowDataArray;
  item: TMenuItem;
  Connections: TStringList;
begin
  // Delete dynamically added connection menu items.
  for i := menuConnections.Items.Count - 1 downto 0 do begin
    menuConnections.Items.Delete(i);
  end;

  // Check if all the heidisql windows are still alive.
  CheckForCrashedWindows;

  // Fetch list of heidisql windows.
  list := GetWindowList;

  // Re-create dynamic menu items.
  for i := 0 to High(list) do with list[i] do begin
    // TODO: Remove connection with this UID from "all" array
    item := TMenuItem.Create(self);
    if namePostfix <> 0 then name := name + Format(' (%d)', [namePostFix]);
    item.Caption := name;
    if (appHandle = Handle) and (connected) then item.ImageIndex := ICON_MYSELF_CONNECTED
    else if (appHandle = Handle) and (not connected) then item.ImageIndex := ICON_MYSELF_DISCONNECTED
    else if (appHandle <> Handle) and (connected) then item.ImageIndex := ICON_OTHER_CONNECTED
    else if (appHandle <> Handle) and (not connected) then item.ImageIndex := ICON_OTHER_DISCONNECTED;
    item.Tag := appHandle;
    item.OnClick := focusWindow;
    menuConnections.Items.Add(item);
  end;

  // Add separator
  item := TMenuItem.Create(menuConnections);
  item.Caption := '-';
  menuConnections.Items.Add(item);

  // "Session manager" and "New window" items
  item := TMenuItem.Create(menuConnections);
  item.Action := actSessionManager;
  item.Default := True;
  menuConnections.Items.Add(item);
  item := TMenuItem.Create(menuConnections);
  item.Action := actNewWindow;
  menuConnections.Items.Add(item);

  // All sessions
  if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, False) then begin
    Connections := TStringList.Create;
    MainReg.GetKeyNames(Connections);
    for i := 0 to Connections.Count - 1 do begin
      item := TMenuItem.Create(menuConnections);
      item.Caption := Connections[i];
      item.OnClick := SessionConnect;
      item.ImageIndex := 37;
      if Connections[i] = SessionName then begin
        item.Checked := True;
        item.ImageIndex := -1;
      end;
      menuConnections.Items.Add(item);
    end;
  end;

end;


procedure TMainForm.File1Click(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  Connections: TStringList;
begin
  // Decide if "Connect to" menu should be enabled
  menuConnectTo.Enabled := False;
  if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, False) then begin
    menuConnectTo.Enabled := MainReg.HasSubKeys;
    if menuConnectTo.Enabled then begin
      // Add all sessions to submenu
      for i := menuConnectTo.Count - 1 downto 0 do
        menuConnectTo.Delete(i);
      Connections := TStringList.Create;
      MainReg.GetKeyNames(Connections);
      for i := 0 to Connections.Count - 1 do begin
        Item := TMenuItem.Create(menuConnectTo);
        Item.Caption := Connections[i];
        Item.OnClick := SessionConnect;
        Item.ImageIndex := 37;
        if Connections[i] = SessionName then begin
          Item.Checked := True;
          Item.ImageIndex := -1;
        end;
        menuConnectTo.Add(Item);
      end;
    end;
  end;
end;


procedure TMainForm.actWebbrowse(Sender: TObject);
begin
  // Browse to URL (hint)
  ShellExec( TAction(Sender).Hint );
end;


// Escape database, table, field, index or key name.
function TMainform.mask(str: WideString) : WideString;
begin
  result := maskSql(mysql_version, str);
end;


procedure TMainForm.actExportSettingsExecute(Sender: TObject);
begin
  // Export settings to .reg-file
  if SaveDialog2.Execute then begin
    if winexec(pchar('regedit.exe /e "'+SaveDialog2.FileName+'" HKEY_CURRENT_USER'+REGPATH), SW_SHOW) = ERROR_FILE_NOT_FOUND then
      MessageDlg('File not found: regedit.exe', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.actImportSettingsExecute(Sender: TObject);
begin
  // Import settings from .reg-file
  if OpenDialog2.Execute then begin
    if winexec(pchar('regedit.exe "'+OpenDialog2.FileName+'"'), SW_SHOW) = ERROR_FILE_NOT_FOUND then
      MessageDlg('File not found: regedit.exe', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.actExecuteQueryExecute(Sender: TObject);
begin
  ExecSqlClick(sender, false);
end;

procedure TMainForm.actExecuteSelectionExecute(Sender: TObject);
begin
  ExecSqlClick(sender, true);
end;

procedure TMainForm.actExecuteLineExecute(Sender: TObject);
begin
  ExecSqlClick(sender, false, true);
end;


procedure TMainForm.actCopyAsCSVExecute(Sender: TObject);
var
  S: TMemoryStream;
begin
  // Copy data in focused grid as CSV
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  try
    GridToCsv(ActiveGrid, prefCSVSeparator, prefCSVEncloser, prefCSVTerminator, S);
    StreamToClipboard(S);
  finally
    ShowStatus('Freeing data...');
    S.Free;
    ShowStatus(STATUS_MSG_READY);
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actCopyAsHTMLExecute(Sender: TObject);
var
  S: TMemoryStream;
  Title: WideString;
begin
  // Copy data in focused grid as HTML table
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  if ActiveGrid = DataGrid then Title := SelectedTable.Text
  else Title := 'SQL query';
  try
    GridToHtml(ActiveGrid, Title, S);
    StreamToClipboard(S);
  finally
    ShowStatus('Freeing data...');
    S.Free;
    ShowStatus(STATUS_MSG_READY);
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actCopyAsXMLExecute(Sender: TObject);
var
  S: TMemoryStream;
  Root: WideString;
begin
  // Copy data in focused grid as XML
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  if ActiveGrid = DataGrid then Root := SelectedTable.Text
  else Root := 'SQL query';
  try
    GridToXml(ActiveGrid, Root, S);
    StreamToClipboard(S);
  finally
    ShowStatus('Freeing data...');
    S.Free;
    ShowStatus(STATUS_MSG_READY);
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actCopyAsSQLExecute(Sender: TObject);
var
  S: TMemoryStream;
  Tablename: WideString;
begin
  // Copy data in focused grid as SQL
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  if ActiveGrid = DataGrid then Tablename := SelectedTable.Text
  else Tablename := 'unknown';
  try
    GridToSql(ActiveGrid, Tablename, S);
    StreamToClipboard(S);
  finally
    ShowStatus('Freeing data...');
    S.Free;
    ShowStatus(STATUS_MSG_READY);
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actExportDataExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
  FS: TFileStream;
  Title: WideString;
begin
  // Save data in current dataset as CSV, HTML or XML
  Dialog := SaveDialogExportData;

  if ActiveGrid = DataGrid then
    Title := SelectedTable.Text
  else
    Title := 'SQL query';

  Dialog.FileName := Title;
  Dialog.Title := 'Export result set from '+Dialog.Filename+'...';

  if Dialog.Execute and (Dialog.FileName <> '') then try
    Screen.Cursor := crHourGlass;
    FS := openfs(Dialog.FileName);
    case Dialog.FilterIndex of
      1: GridToCsv(ActiveGrid, prefCSVSeparator, prefCSVEncloser, prefCSVTerminator, FS);
      2: GridToHtml(ActiveGrid, Title, FS);
      3: GridToXml(ActiveGrid, Title, FS);
      4: GridToSql(ActiveGrid, Title, FS);
    end;
    ShowStatus('Freeing data...');
    FS.Free;
  finally
    ShowStatus(STATUS_MSG_READY);
    Screen.Cursor := crDefault;
  end;
end;



// view HTML
procedure TMainForm.actHTMLviewExecute(Sender: TObject);
const
  msgNotBinary = 'Non-binary field selected. Only binary fields containing JPEG, PNG, GIF and BMP images are supported.';
  msgNotImage = 'Unrecognized image format.  Only JPEG, PNG, GIF and BMP are supported.';
var
  g               : TVirtualStringTree;
  filename        : String;
  f               : Textfile;
  Header, Content : String;
  IsBinary        : Boolean;
  SaveBinary      : Boolean;
begin
  g := ActiveGrid;
  if g = nil then begin messagebeep(MB_ICONASTERISK); exit; end;
  Screen.Cursor := crHourGlass;
  showstatus('Saving contents to file...');
  IsBinary := GridResult(ActiveGrid).Columns[g.FocusedColumn].DatatypeCat = dtcBinary;

  Header := WideHexToBin(Copy(g.Text[g.FocusedNode, g.FocusedColumn], 3, 20));
  SaveBinary := false;
  filename := GetTempDir+'\'+APPNAME+'-preview.';
  if IsBinary and (Copy(Header, 7, 4) = 'JFIF') then begin
    SaveBinary := true;
    filename := filename + 'jpeg';
  end else if IsBinary and (Copy(Header, 2, 3) = 'PNG') then begin
    SaveBinary := true;
    filename := filename + 'png';
  end else if IsBinary and StrCmpBegin('GIF', Header) then begin
    SaveBinary := true;
    filename := filename + 'gif';
  end else if IsBinary and StrCmpBegin('BM', Header) then begin
    SaveBinary := true;
    filename := filename + 'bmp';
  end;

  if not IsBinary then begin
    MessageDlg(msgNotBinary, mtWarning, [mbOk], 0);
  end else if not SaveBinary then begin
    MessageDlg(msgNotImage, mtWarning, [mbOk], 0);
  end;

  if SaveBinary then begin
    if not EnsureFullWidth(g, g.FocusedColumn, g.FocusedNode) then Exit;
    Content := WideHexToBin(Copy(g.Text[g.FocusedNode, g.FocusedColumn], 3, High(Integer)));
    AssignFile(f, filename);
    Rewrite(f);
    Write(f, Content);
    CloseFile(f);
  end;
  ShowStatus( STATUS_MSG_READY );
  Screen.Cursor := crDefault;
  ShellExec( filename );
end;

procedure TMainForm.actInsertFilesExecute(Sender: TObject);
begin
  InsertFilesWindow(Self);
end;

procedure TMainForm.actExportTablesExecute(Sender: TObject);
var
  f: TExportSQLForm;
  ds: TDataset;
  InDBTree: Boolean;
  Comp: TComponent;
begin
  f := TExportSQLForm.Create(Self);

  // popupDB is used in DBTree AND ListTables
  InDBTree := False;
  Comp := (Sender as TAction).ActionComponent;
  if Comp is TMenuItem then
    InDBTree := TPopupMenu((Comp as TMenuItem).GetParentMenu).PopupComponent = DBTree;
  if InDBTree then begin
    // If a table is selected, use that for preselection. If only a db was selected, use all tables inside it.
    if SelectedTable.Text <> '' then
      f.SelectedTables.Add(SelectedTable.Text)
    else if Mainform.ActiveDatabase <> '' then begin
      ds := Mainform.FetchDbTableList(ActiveDatabase);
      while not ds.Eof do begin
        f.SelectedTables.Add(ds.FieldByName(DBO_NAME).AsWideString);
        ds.Next;
      end;
    end;
  end else
    f.SelectedTables := GetVTCaptions( Mainform.ListTables, True );

  f.ShowModal;
  FreeAndNil(f);
end;

// Drop Table(s)
procedure TMainForm.actDropObjectsExecute(Sender: TObject);
var
  AllCount : Integer;
  Tables, Views, Functions, Procedures: TWideStringList;
  msg, activeDB : WideString;
  InDBTree: Boolean;
  Act: TAction;

  procedure DoDrop(Kind: String; List: TWideStringlist; MultiDrops: Boolean);
  var
    i: Integer;
    baseSql, sql: WideString;
  begin
    if List.Count > 0 then begin
      baseSql := 'DROP '+Kind+' ';
      sql := '';
      for i := 0 to List.Count - 1 do begin
        if (i > 0) and MultiDrops then sql := sql + ', ';
        sql := sql + mask(List[i]);
        if not MultiDrops then begin
          ExecUpdateQuery(baseSql + sql);
          sql := '';
        end;
      end;
      if MultiDrops then
        ExecUpdateQuery(baseSql + sql);
    end;
    FreeAndNil(List);
  end;

begin
  debug('drop objects activated');
  // Set default database name to to ActiveDatabase.
  // Can be overwritten when someone selects a table in dbtree from different database
  activeDB := ActiveDatabase;

  Tables := TWideStringlist.Create;
  Views := TWideStringlist.Create;
  Procedures := TWideStringlist.Create;
  Functions := TWideStringlist.Create;

  Act := Sender as TAction;
  InDBTree := (Act.ActionComponent is TMenuItem)
    and (TPopupMenu((Act.ActionComponent as TMenuItem).GetParentMenu).PopupComponent = DBTree);
  if InDBTree then begin
    // drop table selected in tree view.
    case GetFocusedTreeNodeType of
      lntDb: begin
        if MessageDlg('Drop Database "'+activeDB+'"?' + crlf + crlf + 'WARNING: You will lose all tables in database '+activeDB+'!', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
          Abort;
        Screen.Cursor := crHourglass;
        try
          ExecUpdateQuery( 'DROP DATABASE ' + mask(activeDB) );
          ClearDbTableList(activeDB);
          if DatabasesWanted.IndexOf(activeDB) > -1 then begin
            DatabasesWanted.Delete( DatabasesWanted.IndexOf(activeDB) );
            OpenRegistry(SessionName);
            MainReg.WriteString( 'OnlyDBs', ImplodeStr( ';', DatabasesWanted ) );
          end;
          DBtree.Selected[DBtree.GetFirst] := true;
          RefreshTree(False);
        finally
          Screen.Cursor := crDefault;
        end;
        Exit;
      end;
      lntTable, lntCrashedTable: Tables.Add(SelectedTable.Text);
      lntView: Views.Add(SelectedTable.Text);
      lntProcedure: Procedures.Add(SelectedTable.Text);
      lntFunction: Functions.Add(SelectedTable.Text);
    end;
  end else begin
    // Invoked from database tab
    Tables := GetVTCaptions(ListTables, True, 0, lntTable);
    Tables.AddStrings(GetVTCaptions(ListTables, True, 0, lntCrashedTable));
    Views := GetVTCaptions(ListTables, True, 0, lntView);
    Procedures := GetVTCaptions(ListTables, True, 0, lntProcedure);
    Functions := GetVTCaptions(ListTables, True, 0, lntFunction);
  end;

  // Fix actions temporarily enabled for popup menu.
  ValidateControls(Sender);

  AllCount := Tables.Count + Views.Count + Procedures.Count + Functions.Count;

  // Safety stop to avoid firing DROP TABLE without tablenames
  if (AllCount = 0) then
    Exit;

  // Ask user for confirmation to drop selected objects
  msg := 'Drop ' + IntToStr(AllCount) + ' object(s) in database "'+activeDB+'"?'
    + CRLF;
  if Tables.Count > 0 then msg := msg + CRLF + 'Table(s): ' + ImplodeStr(', ', Tables);
  if Views.Count > 0 then msg := msg + CRLF + 'View(s): ' + ImplodeStr(', ', Views);
  if Procedures.Count > 0 then msg := msg + CRLF + 'Procedure(s): ' + ImplodeStr(', ', Procedures);
  if Functions.Count > 0 then msg := msg + CRLF + 'Function(s): ' + ImplodeStr(', ', Functions);
  if MessageDlg(msg, mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    Exit;

  // Compose and run DROP [TABLE|VIEW|...] queries
  DoDrop('TABLE', Tables, True);
  DoDrop('VIEW', Views, True);
  DoDrop('PROCEDURE', Procedures, False);
  DoDrop('FUNCTION', Functions, False);

  // Refresh ListTables + dbtree so the dropped tables are gone:
  actRefresh.Execute;
end;


// Load SQL-file, make sure that SheetQuery is activated
procedure TMainForm.actLoadSQLExecute(Sender: TObject);
begin
  if OpenDialogSQLfile.Execute then
    QueryLoad( OpenDialogSQLfile.FileName );
end;



{**
  Parse commandline for a specific name=value pair
  @return Boolean True if parameter was found, False if not
}
function TMainForm.GetParamValue(const paramChar: Char; const paramName:
  string; var curIdx: Byte; out paramValue: string): Boolean;
var
  i, nextIdx: Integer;
  param, nextParam: string;
begin
  paramValue := '';
  param := ParamStr(curIdx);
  // Example: --user=root --session="My session name" --password
  if Pos('--' + paramName, param) = 1 then
  begin
    i := Length('--' + paramName) + 1;
    if param[i] = '=' then
      paramValue := Copy(param, i + 1, Length(param) - i);
      if (Copy(paramValue, 1, 1) = '"') and (Copy(paramValue, Length(paramValue), 1) = '"') then
        paramValue := Copy(paramValue, 2, Length(paramValue) - 2);
    result := True;

  end else if Pos('-' + paramChar, param) = 1 then
  begin
    if Length(param) > 2 then
    begin
      // Example: -uroot -s"My session name"
      paramValue := Copy(param, 3, Length(param) - 2);
      if (Copy(paramValue, 1, 1) = '"') and (Copy(paramValue, Length(paramValue), 1) = '"') then
        paramValue := Copy(paramValue, 2, Length(paramValue) - 2);
    end else
    begin
      // Example: -u root -s "My session name" -p
      nextIdx := curIdx + 1;
      if nextIdx <= ParamCount then begin
        nextParam := ParamStr(nextIdx);
        if not Pos('-', nextParam) = 1 then
          paramValue := nextParam;
      end;
    end;
    result := True;
  end else
    result := False;
end;


procedure TMainForm.SessionConnect(Sender: TObject);
var
  Session: String;
  parHost, parPort, parUser, parPass, parTimeout, parCompress, parDatabase, parSortDatabases: WideString;
begin
  Session := (Sender as TMenuItem).Caption;
  parHost := GetRegValue(REGNAME_HOST, '', Session);
  parUser := GetRegValue(REGNAME_USER, '', Session);
  parPass := decrypt(GetRegValue(REGNAME_PASSWORD, '', Session));
  parPort := GetRegValue(REGNAME_PORT, '', Session);
  parTimeout := GetRegValue(REGNAME_TIMEOUT, '', Session);
  parCompress := IntToStr(Integer(GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, Session)));
  parDatabase := Utf8Decode(GetRegValue(REGNAME_ONLYDBS, '', Session));
  parSortDatabases := IntToStr(Integer(GetRegValue(REGNAME_ONLYDBSSORTED, DEFAULT_ONLYDBSSORTED, Session)));
  if InitConnection(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases) then begin
    SessionName := Session;
    DoAfterConnect;
  end;
end;


{**
  Receive connection parameters and create the mdi-window
  Paremeters are either sent by connection-form or by commandline.
}
function TMainform.InitConnection(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases: WideString): Boolean;
var
  MysqlConnection: TMysqlConn;
  Profile: TOpenConnProf;
  UsingPass: String;
begin
  // fill structure
  ZeroMemory(@Profile, SizeOf(Profile));
  Profile.MysqlParams.Protocol := 'mysql';
  Profile.MysqlParams.Host := Trim( parHost );
  Profile.MysqlParams.Port := StrToIntDef(parPort, DEFAULT_PORT);
  Profile.MysqlParams.Database := '';
  Profile.MysqlParams.User := parUser;
  Profile.MysqlParams.Pass := parPass;
  if Integer(parCompress) > 0 then
    Profile.MysqlParams.PrpCompress := 'true'
  else
    Profile.MysqlParams.PrpCompress := 'false';
  Profile.MysqlParams.PrpTimeout := parTimeout;
  Profile.MysqlParams.PrpDbless := 'true';
  Profile.MysqlParams.PrpClientLocalFiles := 'true';
  Profile.MysqlParams.PrpClientInteractive := 'true';
  Profile.DatabaseList := parDatabase;
  Profile.DatabaseListSort := Boolean(StrToIntDef(parSortDatabases, 0));

  MysqlConnection := TMysqlConn.Create(@Profile);

  // attempt to establish connection
  if Profile.MysqlParams.Pass <> '' then UsingPass := 'Yes' else UsingPass := 'No';
  LogSQL('Connecting to '+Profile.MysqlParams.Host+
    ', username '+Profile.MysqlParams.User+
    ', using password: '+UsingPass+' ...');
  if MysqlConnection.Connect <> MCR_SUCCESS then begin
    // attempt failed -- show error
    MessageDlg ( 'Could not establish connection! Details:'+CRLF+CRLF+MysqlConnection.LastError, mtError, [mbOK], 0);
    Result := False;
    FreeAndNil(MysqlConnection);
  end else begin
    Result := True;
    Profile.MysqlConn := MysqlConnection.Connection;
    if Assigned(FMysqlConn) then
      DoDisconnect;
    // Assign global connection objects
    FConn := Profile;
    FMysqlConn := MysqlConnection;
  end;
  ShowStatus( STATUS_MSG_READY );
end;


procedure TMainForm.actDataDeleteExecute(Sender: TObject);
begin
  // Delete row(s)
  if (DataGrid.SelectedCount = 1) and
    (DataGridResult.Rows[DataGrid.GetFirstSelected.Index].State = grsInserted)
    then begin
    // Deleting the virtual row which is only in memory by stopping edit mode
    actDataCancelChanges.Execute;
  end else begin
    // The "normal" case: Delete existing rows
    if not CheckUniqueKeyClause then
      Exit;
    if DataGrid.SelectedCount = 0 then
      MessageDLG('Please select one or more rows to delete them.', mtError, [mbOK], 0)
    else if MessageDLG('Delete '+inttostr(DataGrid.SelectedCount)+' row(s)?',
        mtConfirmation, [mbOK, mbCancel], 0) = mrOK then begin
      GridPostDelete(DataGrid);
    end;
  end;
end;


procedure TMainForm.actUpdateCheckExecute(Sender: TObject);
var
  frm : TfrmUpdateCheck;
begin
  frm := TfrmUpdateCheck.Create(Self);
  frm.ShowModal;
  FreeAndNil(frm);
end;


procedure TMainForm.actCreateTableExecute(Sender: TObject);
begin
  tabEditor.TabVisible := True;
  PagecontrolMain.ActivePage := tabEditor;
  PlaceObjectEditor(lntTable);
  TableEditor.Init;
end;


procedure TMainForm.actEmptyTablesExecute(Sender: TObject);
var
  t: TWideStringList;
  i: Integer;
  sql_pattern: String;
begin
  // Add selected items/tables to helper list
  if ListTables.Focused then
    t := GetVTCaptions(ListTables, True)
  else if DBTree.Focused then begin
    t := TWideStringList.Create;
    t.Add(SelectedTable.Text);
  end else
    Exit;
  if t.Count = 0 then
    Exit;

  if MessageDlg('Empty ' + IntToStr(t.count) + ' table(s) ?' + CRLF + '(' + implodestr(', ', t) + ')',
    mtConfirmation, [mbOk, mbCancel], 0) <> mrOk then
    exit;

  Screen.Cursor := crHourglass;
  {**
    @note ansgarbecker: Empty table using faster TRUNCATE statement on newer servers
    @see http://dev.mysql.com/doc/refman/5.0/en/truncate.html
    @see https://sourceforge.net/tracker/index.php?func=detail&aid=1644143&group_id=164593&atid=832350
  }
  if mysql_version < 50003 then
    sql_pattern := 'DELETE FROM '
  else
    sql_pattern := 'TRUNCATE ';

  for i:=0 to t.count-1 do
    ExecUpdateQuery( sql_pattern + mask(t[i]) );
  t.Free;
  actRefresh.Execute;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.actNewWindowExecute(Sender: TObject);
begin
  debug('perf: new connection clicked.');
  ShellExec( ExtractFileName(paramstr(0)), ExtractFilePath(paramstr(0)) );
end;


procedure TMainForm.actQueryFindExecute(Sender: TObject);
begin
  // if something is selected search for that text
  if ActiveQueryMemo.SelAvail then
    FindDialogQuery.FindText := ActiveQueryMemo.SelText
  else
    FindDialogQuery.FindText := ActiveQueryMemo.WordAtCursor;
  FindDialogQuery.Execute;
end;


procedure TMainForm.actQueryReplaceExecute(Sender: TObject);
begin
  // if something is selected search for that text
  if ActiveQueryMemo.SelAvail then
    ReplaceDialogQuery.FindText := ActiveQueryMemo.SelText
  else
    ReplaceDialogQuery.FindText := ActiveQueryMemo.WordAtCursor;
  ReplaceDialogQuery.Execute;
end;


procedure TMainForm.actRefreshExecute(Sender: TObject);
var
  tab1, tab2: TTabSheet;
  List: TVirtualStringTree;
begin
  // Refresh
  // Force data tab update when appropriate.
  dataselected := false;
  tab1 := PageControlMain.ActivePage;
  if ActiveControl = DBtree then
    RefreshTree(True)
  else if tab1 = tabHost then begin
    tab2 := PageControlHost.ActivePage;
    if tab2 = tabVariables then
      List := ListVariables
    else if tab2 = tabStatus then
      List := ListStatus
    else if tab2 = tabProcessList then
      List := ListProcesses
    else
      List := ListCommandStats;
    List.Tag := VTREE_NOTLOADED;
    List.Repaint;
  end else if tab1 = tabDatabase then begin
    RefreshTreeDB(ActiveDatabase);
    LoadDatabaseProperties(ActiveDatabase);
  end else if tab1 = tabData then
    viewdata(Sender);
end;


procedure TMainForm.actSQLhelpExecute(Sender: TObject);
var
  keyword : String;
  ds: TDataset;
begin
  // Call SQL Help from various places
  if mysql_version < 40100 then
    exit;

  keyword := '';
  // Query-Tab
  if ActiveControl is TSynMemo then
    keyword := TSynMemo(ActiveControl).WordAtCursor
  // Data-Tab
  else if (PageControlMain.ActivePage = tabData)
    and Assigned(DataGrid.FocusedNode) then begin
    ds := SelectedTableColumns;
    ds.RecNo := DataGrid.FocusedColumn;
    keyword := ds.FieldByName('Type').AsWideString;
  end
  else if QueryTabActive and ActiveQueryHelpers.Focused then
  begin
    // Makes only sense if one of the tabs "SQL fn" or "SQL kw" was selected
    if ActiveQueryTabset.TabIndex in [1,2] then
    begin
      keyword := ActiveQueryHelpers.Items[ActiveQueryHelpers.ItemIndex];
    end;
  end;

  // Clean existing paranthesis, fx: char(64)
  if Pos( '(', keyword ) > 0 then
  begin
    keyword := Copy( keyword, 1, Pos( '(', keyword )-1 );
  end;

  // Show the window
  CallSQLHelpWithKeyword( keyword );
end;


{***
  Show SQL Help window directly using a keyword
  @param String SQL-keyword
  @see FieldeditForm.btnDatatypeHelp
}
procedure TMainform.CallSQLHelpWithKeyword( keyword: String );
begin
  if SQLHelpForm = nil then
    SQLHelpForm := TfrmSQLhelp.Create(Self);
  SQLHelpForm.Keyword := keyword;
  SQLHelpForm.ShowModal;
end;


procedure TMainForm.actSaveSQLExecute(Sender: TObject);
var
  Text, LB: WideString;
begin
  // Save SQL
  if SaveDialogSQLFile.Execute then
  begin
    Screen.Cursor := crHourGlass;
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    case (Sender as TAction).Tag of
      0: Text := ActiveQueryMemo.Text;
      1: Text := ActiveQueryMemo.SelText;
    end;
    LB := '';
    case QueryMemoLineBreaks of
      lbsUnix: LB := LB_UNIX;
      lbsMac: LB := LB_MAC;
      lbsWide: LB := LB_WIDE;
    end;
    if LB <> '' then
      Text := WideStringReplace(Text, CRLF, LB, [rfReplaceAll]);
    SaveUnicodeFile( SaveDialogSQLFile.FileName, Text );
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actSaveSQLSnippetExecute(Sender: TObject);
var
  snippetname : String;
  mayChange   : Boolean;
  Text, LB: WideString;
begin
  // Save snippet
  if InputQuery( 'Save snippet', 'Snippet name:', snippetname) then
  begin
    if Copy( snippetname, Length(snippetname)-4, 4 ) <> '.sql' then
      snippetname := snippetname + '.sql';
    // cleanup snippetname from special characters
    snippetname := DIRNAME_SNIPPETS + goodfilename(snippetname);
    if FileExists( snippetname ) then
    begin
      if MessageDlg( 'Overwrite existing snippet '+snippetname+'?', mtConfirmation, [mbOK, mbCancel], 0 ) <> mrOK then
        exit;
    end;
    Screen.Cursor := crHourglass;
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    case (Sender as TComponent).Tag of
      0: Text := ActiveQueryMemo.Text;
      1: Text := ActiveQueryMemo.SelText;
    end;
    LB := '';
    case QueryMemoLineBreaks of
      lbsUnix: LB := LB_UNIX;
      lbsMac: LB := LB_MAC;
      lbsWide: LB := LB_WIDE;
    end;
    if LB <> '' then
      Text := WideStringReplace(Text, CRLF, LB, [rfReplaceAll]);
    SaveUnicodeFile( snippetname, Text );
    FillPopupQueryLoad;
    if tabsetQueryHelpers.TabIndex = 3 then begin
      // SQL Snippets selected in query helper, refresh list
      mayChange := True; // Unused; satisfies callee parameter collection which is probably dictated by tabset.
      tabsetQueryHelpersChange(Sender, 3, mayChange);
    end;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actQueryStopOnErrorsExecute(Sender: TObject);
begin
  // Weird fix: dummy routine to avoid the sending action getting disabled
end;


procedure TMainForm.actQueryWordWrapExecute(Sender: TObject);
begin
  ActiveQueryMemo.WordWrap := TAction(Sender).Checked;
end;


procedure TMainForm.FindDialogQueryFind(Sender: TObject);
var
  Options: TSynSearchOptions;
  Search: String;
begin
  Search := FindDialogQuery.FindText;
  Options := [];
  if Sender is TReplaceDialog then
    Include(Options, ssoEntireScope);
  if not (frDown in FindDialogQuery.Options) then
    Include(Options, ssoBackwards);
  if frMatchCase in FindDialogQuery.Options then
    Include(Options, ssoMatchCase);
  if frWholeWord in FindDialogQuery.Options then
    Include(Options, ssoWholeWord);
  if ActiveQueryMemo.SearchReplace(Search, '', Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    ShowStatus( 'SearchText ''' + Search + ''' not found!', 0);
  end;
end;


procedure TMainForm.ReplaceDialogQueryFind(Sender: TObject);
begin
  FindDialogQuery.FindText := ReplaceDialogQuery.FindText;
  FindDialogQueryFind( ReplaceDialogQuery );
end;


procedure TMainForm.ReplaceDialogQueryReplace(Sender: TObject);
var
  Options: TSynSearchOptions;
  Search: String;
begin
  Search := ReplaceDialogQuery.FindText;
  Options := [ssoEntireScope];  // Do replaces always on entire scope, because the standard-dialog lacks of a down/up-option
  if frReplaceAll in ReplaceDialogQuery.Options then
    Include( Options, ssoReplaceAll );
  if not (frDown in ReplaceDialogQuery.Options) then
    Include(Options, ssoBackwards);
  if frMatchCase in ReplaceDialogQuery.Options then
    Include(Options, ssoMatchCase);
  if frWholeWord in ReplaceDialogQuery.Options then
    Include(Options, ssoWholeWord);
  if frReplace in ReplaceDialogQuery.Options then // Replace instead of ReplaceAll is pressed
    Include(Options, ssoReplace)
  else
    Include(Options, ssoReplaceAll);
  if ActiveQueryMemo.SearchReplace( Search, ReplaceDialogQuery.ReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    ShowStatus( 'SearchText ''' + Search + ''' not found!', 0);
    if ssoBackwards in Options then
      ActiveQueryMemo.BlockEnd := ActiveQueryMemo.BlockBegin
    else
      ActiveQueryMemo.BlockBegin := ActiveQueryMemo.BlockEnd;
    ActiveQueryMemo.CaretXY := ActiveQueryMemo.BlockBegin;
  end;
end;


procedure TMainform.FillPopupQueryLoad;
var
  i, j: Integer;
  menuitem, snippetsfolder: TMenuItem;
  snippets: TStringList;
  sqlFilename: String;
begin
  // Fill the popupQueryLoad menu
  popupQueryLoad.Items.Clear;

  // Snippets
  snippets := getFilesFromDir( DIRNAME_SNIPPETS, '*.sql', true );
  snippetsfolder := TMenuItem.Create( popupQueryLoad );
  snippetsfolder.Caption := 'Snippets';
  popupQueryLoad.Items.Add(snippetsfolder);
  for i := 0 to snippets.Count - 1 do begin
    menuitem := TMenuItem.Create( snippetsfolder );
    menuitem.Caption := snippets[i];
    menuitem.OnClick := popupQueryLoadClick;
    snippetsfolder.Add(menuitem);
  end;

  // Separator
  menuitem := TMenuItem.Create( popupQueryLoad );
  menuitem.Caption := '-';
  popupQueryLoad.Items.Add(menuitem);

  // Recent files
  j := 0;
  for i:=0 to 19 do begin
    sqlFilename := GetRegValue( 'SQLFile'+IntToStr(i), '' );
    if sqlFilename = '' then
      continue;
    inc(j);
    menuitem := TMenuItem.Create( popupQueryLoad );
    menuitem.Caption := IntToStr(j) + ' ' + sqlFilename;
    menuitem.OnClick := popupQueryLoadClick;
    popupQueryLoad.Items.Add(menuitem);
  end;

  // Separator + "Remove absent files"
  menuitem := TMenuItem.Create( popupQueryLoad );
  menuitem.Caption := '-';
  popupQueryLoad.Items.Add(menuitem);
  menuitem := TMenuItem.Create( popupQueryLoad );
  menuitem.Caption := 'Remove absent files';
  menuitem.OnClick := PopupQueryLoadRemoveAbsentFiles;
  popupQueryLoad.Items.Add(menuitem);

end;


procedure TMainform.PopupQueryLoadRemoveAbsentFiles( sender: TObject );
begin
  AddOrRemoveFromQueryLoadHistory( '', false, true );
  FillPopupQueryLoad;
end;

procedure TMainform.popupQueryLoadClick( sender: TObject );
var
  filename : String;
  p        : Integer;
begin
  // Click on the popupQueryLoad
  filename := (Sender as TMenuItem).Caption;
  if Pos( '\', filename ) = 0 then
  begin // assuming we load a snippet
    filename := DIRNAME_SNIPPETS + filename + '.sql';
  end
  else
  begin // assuming we load a file from the recent-list
    p := Pos( ' ', filename ) + 1;
    filename := Copy(filename, p, Length(filename));
  end;
  filename := Stringreplace(filename, '&', '', [rfReplaceAll]);
  QueryLoad( filename );
end;


procedure TMainform.AddOrRemoveFromQueryLoadHistory( filename: String; AddIt: Boolean = true; CheckIfFileExists: Boolean = true );
var
  i                     : Integer;
  Values, newfilelist   : TStringList;
  savedfilename         : String;
begin
  // Add or remove filename to/from history, avoiding duplicates

  newfilelist := TStringList.create;
  Values := TStringList.create;
  OpenRegistry;
  MainReg.GetValueNames( Values );

  // Add new filename
  if AddIt then
    newfilelist.Add( filename );

  // Add all other filenames
  for i:=0 to Values.Count-1 do begin
    if Pos( 'SQLFile', Values[i] ) <> 1 then
      continue;
    savedfilename := GetRegValue( Values[i], '' );
    MainReg.DeleteValue( Values[i] );
    if CheckIfFileExists and (not FileExists( savedfilename )) then
      continue;
    if (savedfilename <> filename) and (newfilelist.IndexOf(savedfilename)=-1) then
      newfilelist.add( savedfilename );
  end;

  // Save new list
  for i := 0 to newfilelist.Count-1 do begin
    if i >= 20 then
      break;
    MainReg.WriteString( 'SQLFile'+IntToStr(i), newfilelist[i] );
  end;
end;


{**
  Change default delimiter for SQL execution
}
procedure TMainForm.actSetDelimiterExecute(Sender: TObject);
var
  newVal: String;
  ok: Boolean;
begin
  // Use a while loop to redisplay the input dialog after setting an invalid value
  ok := False;
  while not ok do begin
    newVal := delimiter;
    if InputQuery('Set delimiter', 'SQL statement delimiter (default is ";"):', newVal) then try
      // Set new value
      Delimiter := newVal;
      ok := True;
    except on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end else // Cancel clicked
      ok := True;
  end;
end;


{**
  Validates and sets the Delimiter property plus updates the hint on actSetDelimiter
}
procedure TMainForm.SetDelimiter(Value: String);
var
  ErrMsg: String;
begin
  ErrMsg := '';
  Value := Trim(Value);
  // Test for empty delimiter.
  if Value = '' then ErrMsg := 'DELIMITER must be followed by a non-comment character or string';
  // Disallow backslash, because the MySQL CLI does so for some reason.
  // Then again, is there any reason to be bug-per-bug compatible with some random SQL parser?
  if Pos('\', Value) > 0 then ErrMsg := 'Backslash disallowed in DELIMITER (because the MySQL CLI does not accept it)';
  // Disallow stuff which would be negated by the comment parsing logic.
  if
    (Pos('/*', Value) > 0) or
    (Pos('--', Value) > 0) or
    (Pos('#', Value) > 0)
  then ErrMsg := 'Start-of-comment tokens disallowed in DELIMITER (because it would be ignored)';
  // Disallow stuff which would be negated by the SQL parser (and could slightly confuse it, if at end-of-string).
  if
    (Pos('''', Value) > 0) or
    (Pos('`', Value) > 0) or
    (Pos('"', Value) > 0)
  then ErrMsg := 'String literal markers disallowed in DELIMITER (because it would be ignored)';

  // Reset an invalid delimiter loaded from registry at startup to the default value
  if (ErrMsg <> '') and (Delimiter = '') then begin
    Value := DEFAULT_DELIMITER;
    ErrMsg := '';
  end;
  // Raise or set it
  if ErrMsg <> '' then begin
    ErrMsg := Format('Invalid delimiter %s: %s.', [Value, ErrMsg]);
    LogSQL(ErrMsg);
    Raise Exception.Create(ErrMsg);
  end else begin
    FDelimiter := Value;
    LogSQL(Format('Delimiter changed to %s.', [Delimiter]));
    actSetDelimiter.Hint := actSetDelimiter.Caption + ' (current value: '+Delimiter+')';
  end;
end;


procedure TMainForm.actApplyFilterExecute(Sender: TObject);
var
  i, nr: Integer;
  OldNumbers, Filters: TStringList;
  val: String;
begin
  if SynMemoFilter.GetTextLen > 0 then begin
    // Recreate recent filters list
    Filters := TStringList.Create;
    OldNumbers := TStringList.Create;
    Filters.Add(Trim(Utf8Encode(SynMemoFilter.Text)));
    MainReg.OpenKey(GetRegKeyTable+'\'+REGNAME_FILTERS, True);
    MainReg.GetValueNames(OldNumbers);
    OldNumbers.CustomSort(CompareNumbers);
    // Add old filters
    for i := 0 to OldNumbers.Count - 1 do begin
      nr := MakeInt(OldNumbers[i]);
      if nr = 0 then continue; // Not a valid entry, ignore that
      val := MainReg.ReadString(OldNumbers[i]);
      if Filters.IndexOf(val) = -1 then
        Filters.Add(val);
      MainReg.DeleteValue(OldNumbers[i]);
    end;
    for i := 1 to Filters.Count do begin
      MainReg.WriteString(IntToStr(i), Filters[i-1]);
      // Avoid too much registry spam with mega old filters
      if i = 20 then break;
    end;
    FreeAndNil(OldNumbers);
    FreeAndNil(Filters);
  end;
  viewdata(Sender);
end;


procedure TMainForm.actDataFirstExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := DataGrid.GetFirst;
  if Assigned(Node) then begin
    DataGrid.ClearSelection;
    DataGrid.FocusedNode := Node;
    DataGrid.Selected[Node] := True;
  end;
end;

procedure TMainForm.actDataInsertExecute(Sender: TObject);
begin
  DataGridInsertRow;
end;

procedure TMainForm.actDataLastExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := DataGrid.GetLast;
  if Assigned(Node) then begin
    DataGrid.ClearSelection;
    DataGrid.FocusedNode := Node;
    DataGrid.Selected[Node] := True;
  end;
end;

procedure TMainForm.actDataPostChangesExecute(Sender: TObject);
begin
  DataGridPostUpdateOrInsert(Datagrid.FocusedNode);
end;

procedure TMainForm.actRemoveFilterExecute(Sender: TObject);
begin
  actClearFilterEditor.Execute;
  viewdata(Sender);
end;


procedure TMainForm.actDataCancelChangesExecute(Sender: TObject);
begin
  DataGridCancel(Sender);
end;


procedure TMainForm.actSelectTreeBackgroundExecute(Sender: TObject);
var
  cs: TColorSelect;
begin
  // Select database tree background color
  cs := TColorSelect.Create(Self);
  cs.Dialog.Color := DBtree.Color;
  if cs.Execute then begin
    DBtree.Color := cs.Dialog.Color;
    OpenRegistry(SessionName);
    MainReg.WriteInteger(REGNAME_TREEBACKGROUND, cs.Dialog.Color);
  end;
end;


function TMainForm.CreateOrGetRemoteQueryTab(sender: THandle): THandle;
begin
  // Should create a tab for commands from another window,
  // or return a handle to an existing tab if one already exists for that window.
  //
  // TODO: Implement this when multiple tabs are implemented.
  //       Return a tab's handle instead of the childwin's handle.
  result := Self.Handle;
end;


function TMainForm.GetQueryRunning: Boolean;
begin
  Result := ( QueryRunningInterlock = 1 );
end;


procedure TMainForm.SetQueryRunning(running: Boolean);
var
  newValue    : Integer;
  oldValue    : Integer;
begin
  if ( running ) then
  begin
    newValue := 1;
  end
  else
  begin
    newValue := 0;
  end;

  oldValue := InterlockedExchange( QueryRunningInterlock, newValue );
  if ( newValue = oldValue ) then
  begin
    case ( newValue ) of
      1 :
      begin
        raise Exception.Create( 'Error: Default connection is ' +
        'already executing a query.' );
      end;
      0 :
      begin
        raise Exception.Create( 'Internal badness: Double reset of running ' +
        'flag.' );
      end;
    end;
  end;
end;


{**
  Add a SQL-command or comment to SynMemoSQLLog
}
procedure TMainForm.LogSQL(msg: WideString = ''; comment: Boolean = true);
var
  snip : boolean;
begin
  // Shorten very long messages
  snip := (prefLogSqlWidth > 0) and (Length(msg) > prefLogSqlWidth);
  if snip then begin
    msg :=
      Copy( msg, 0, prefLogSqlWidth ) +
      '/* large SQL query, snipped at  ' +
      FormatNumber( prefLogSqlWidth ) +
      ' characters */';
  end else if (not snip) and (not comment) then
    msg := msg + Delimiter
  else if comment then
    msg := '/* ' + msg + ' */';

  msg := WideStringReplace( msg, #9, ' ', [rfReplaceAll] );
  msg := WideStringReplace( msg, #10, ' ', [rfReplaceAll] );
  msg := WideStringReplace( msg, #13, ' ', [rfReplaceAll] );
  msg := WideStringReplace( msg, '  ', ' ', [rfReplaceAll] );

  EnterCriticalSection(SqlMessagesLock);
  try
    SqlMessages.Add(msg);
  finally
    LeaveCriticalSection(SqlMessagesLock);
  end;
  PostMessage(Handle, WM_PROCESSLOG, 0, 0);
end;

procedure TMainForm.ProcessSqlLog;
var
  msg: WideString;
begin
  EnterCriticalSection(SqlMessagesLock);
  try
    if SqlMessages = nil then Exit;
    if SqlMessages.Count < 1 then Exit;
    msg := SqlMessages[0];
    SqlMessages.Delete(0);
  finally
    LeaveCriticalSection(SqlMessagesLock);
  end;

  SynMemoSQLLog.Lines.Add( msg );

  TrimSQLLog;

  // Scroll to last line and repaint
  SynMemoSQLLog.GotoLineAndCenter( SynMemoSQLLog.Lines.Count );
  SynMemoSQLLog.Repaint;

  // Log to file?
  if prefLogToFile then
  try
    WriteLn( FileHandleSessionLog, Format('[%s] %s', [DateTimeToStr(Now), msg]) );
  except
    DeactivateFileLogging;
    MessageDlg('Error writing to session log file:'+CRLF+FileNameSessionLog+CRLF+CRLF+'Logging is disabled now.', mtError, [mbOK], 0);
  end;
end;


{**
  Delete first line(s) in SQL log and adjust LineNumberStart in gutter
  Called by LogSQL and preferences dialog
}
procedure TMainForm.TrimSQLLog;
var
  i : Integer;
begin
  i := 0;
  while SynMemoSQLLog.Lines.Count > prefLogsqlnum do
  begin
    SynMemoSQLLog.Lines.Delete(0);
    inc(i);
  end;
  // Increase first displayed number in gutter so it doesn't lie about the log entries
  if i > 0 then
    SynMemoSQLLog.Gutter.LineNumberStart := SynMemoSQLLog.Gutter.LineNumberStart + i;
end;


procedure TMainForm.ShowHost;
begin
  if (not DBTree.Dragging) and (
   (PageControlMain.ActivePage = tabDatabase) or
   (PageControlMain.ActivePage = tabData)
  ) then PageControlMain.ActivePage := tabHost;

  tabDatabase.TabVisible := false;
  tabEditor.TabVisible := false;
  tabData.TabVisible := false;

  PageControlMainChange( Self );
end;


procedure TMainForm.ShowDatabase(db: WideString);
begin
  if (not DBtree.Dragging) and (
   (PageControlMain.ActivePage = tabHost) or
   (PageControlMain.ActivePage = tabData)
  ) then PageControlMain.ActivePage := tabDatabase;

  tabDatabase.TabVisible := true;
  tabEditor.TabVisible := false;
  tabData.TabVisible := false;

  ShowDBProperties( db );
end;



procedure TMainForm.viewdata(Sender: TObject);
var
  i                    : Integer;
  select_base,
  select_base_full,
  select_from          : WideString;
  sl_query             : TWideStringList;
  KeyCols              : WideStrings.TWideStringList;
  ColName              : WideString;
  col                  : TVirtualTreeColumn;
  rx                   : TRegExpr;
  ColType              : String;
  ColExists, ShowIt    : Boolean;
  OldOffsetXY          : TPoint;

procedure InitColumn(name: WideString; ColType: String; Visible: Boolean);
var
  k: Integer;
  idx: Integer;
begin
  idx := Length(DataGridResult.Columns);
  SetLength(DataGridResult.Columns, idx+1);
  DataGridResult.Columns[idx].Name := name;
  col := DataGrid.Header.Columns.Add;
  col.Text := name;
  col.Options := col.Options + [coSmartResize];
  if not visible then col.Options := col.Options - [coVisible];
  // Sorting color and title image
  for k:=0 to Length(FDataGridSort)-1 do begin
    if FDataGridSort[k].ColumnName = name then begin
      case FDataGridSort[k].SortDirection of
        ORDER_ASC:  begin col.Color := COLOR_SORTCOLUMN_ASC;  col.ImageIndex := 109; end;
        ORDER_DESC: begin col.Color := COLOR_SORTCOLUMN_DESC; col.ImageIndex := 110; end;
      end;
    end;
  end;
  // Detect data type
  rx.Expression := '^(tiny|small|medium|big)?int\b';
  if rx.Exec(ColType) then begin
    col.Alignment := taRightJustify;
    DataGridResult.Columns[idx].DatatypeCat := dtcInteger;
  end;
  rx.Expression := '^(float|double|decimal)\b';
  if rx.Exec(ColType) then begin
    col.Alignment := taRightJustify;
    DataGridResult.Columns[idx].DatatypeCat := dtcReal;
  end;
  rx.Expression := '^(date|datetime|time(stamp)?)\b';
  if rx.Exec(ColType) then begin
    DataGridResult.Columns[idx].DatatypeCat := dtcTemporal;
    if rx.Match[1] = 'date' then DataGridResult.Columns[idx].Datatype := dtDate
    else if rx.Match[1] = 'time' then DataGridResult.Columns[idx].Datatype := dtTime
    else if rx.Match[1] = 'timestamp' then DataGridResult.Columns[idx].Datatype := dtTimestamp
    else DataGridResult.Columns[idx].Datatype := dtDatetime;
  end;
  rx.Expression := '^((tiny|medium|long)?text|(var)?char)\b(\(\d+\))?';
  if rx.Exec(ColType) then begin
    DataGridResult.Columns[idx].DatatypeCat := dtcText;
    if rx.Match[4] <> '' then
      DataGridResult.Columns[idx].MaxLength := MakeInt(rx.Match[4])
    else if ColType = 'tinytext' then
      // 255 is the width in bytes. If characters that use multiple bytes are
      // contained, the width in characters is decreased below this number.
      DataGridResult.Columns[idx].MaxLength := 255
    else if ColType = 'text' then
      DataGridResult.Columns[idx].MaxLength := 65535
    else if ColType = 'mediumtext' then
      DataGridResult.Columns[idx].MaxLength := 16777215
    else if ColType = 'longtext' then
      DataGridResult.Columns[idx].MaxLength := 4294967295
    else
      // Fallback for unknown column types
      DataGridResult.Columns[idx].MaxLength := MaxInt;
  end;
  rx.Expression := '^((tiny|medium|long)?blob|(var)?binary|bit)\b';
  if rx.Exec(ColType) then
    DataGridResult.Columns[idx].DatatypeCat := dtcBinary;
  if Copy(ColType, 1, 5) = 'enum(' then begin
    DataGridResult.Columns[idx].Datatype := dtEnum;
    DataGridResult.Columns[idx].DatatypeCat := dtcIntegerNamed;
    DataGridResult.Columns[idx].ValueList := WideStrings.TWideStringList.Create;
    DataGridResult.Columns[idx].ValueList.QuoteChar := '''';
    DataGridResult.Columns[idx].ValueList.Delimiter := ',';
    DataGridResult.Columns[idx].ValueList.DelimitedText := GetEnumValues(ColType);
  end;
  if Copy(ColType, 1, 4) = 'set(' then begin
    DataGridResult.Columns[idx].Datatype := dtSet;
    DataGridResult.Columns[idx].DatatypeCat := dtcSetNamed;
    DataGridResult.Columns[idx].ValueList := WideStrings.TWideStringList.Create;
    DataGridResult.Columns[idx].ValueList.QuoteChar := '''';
    DataGridResult.Columns[idx].ValueList.Delimiter := ',';
    DataGridResult.Columns[idx].ValueList.DelimitedText := GetEnumValues(ColType);
  end;

  SelectedTableKeys.First;
  for k := 0 to SelectedTableKeys.RecordCount - 1 do begin
    if (SelectedTableKeys.FieldByName('Key_name').AsString = 'PRIMARY')
      and (SelectedTableKeys.FieldByName('Column_name').AsWideString = name) then begin
      DataGridResult.Columns[idx].IsPriPart := True;
      break;
    end;
    SelectedTableKeys.Next;
  end;
end;

begin
  Screen.Cursor := crHourglass;
  viewingdata := true;
  sl_query := TWideStringList.Create();

  // Ensure grid has left editing mode so DataGrid.OnNewText applies its changes
  // to the old data, not to the new or some non referenced data
  if DataGrid.IsEditing then
    DataGrid.EndEditNode;

  // Post pending update and set post + cancel buttons to valid state
  if DataGridHasChanges then
    actDataPostChangesExecute(Sender);

  // Switch to <Data>
  PageControlMain.ActivePage := tabData;

  try
    if (SelectedTable.Text <> '') and (ActiveDatabase <> '') then begin
      if FDataGridSelect = nil then
        FDataGridSelect := WideStrings.TWideStringlist.Create;
      if DataGridTable <> SelectedTable.Text then begin
        FDataGridSelect.Clear;
        ResetSelectedTableStuff;
        SynMemoFilter.Clear;
        SetLength(FDataGridSort, 0);
        // Load default view settings
        OpenRegistry;
        if MainReg.OpenKey(GetRegKeyTable, False) then begin
          if MainReg.ValueExists(REGNAME_DEFAULTVIEW) then begin
            // Disable default if crash indicator on current table is found
            if MainReg.ValueExists(REGPREFIX_CRASH_IN_DATA) then begin
              MainReg.DeleteValue(REGNAME_DEFAULTVIEW);
              LogSQL('A crash in the previous data loading for this table ('+SelectedTable.Text+') was detected. Filtering was automatically reset to avoid the same crash for now.');
              // Reset crash indicator.
              MainReg.DeleteValue(REGPREFIX_CRASH_IN_DATA);
            end else begin
              LoadDataView(MainReg.ReadString(REGNAME_DEFAULTVIEW));
            end;
          end;
        end;
      end;
      FillDataViewPopup;

      SynMemoFilter.Color := clWindow;
      rx := TRegExpr.Create;
      ShowStatus('Freeing data...');
      DataGrid.BeginUpdate;
      OldOffsetXY := DataGrid.OffsetXY;
      debug('mem: clearing browse data.');
      SetLength(DataGridResult.Columns, 0);
      SetLength(DataGridResult.Rows, 0);
      DataGrid.RootNodeCount := 0;
      DataGrid.Header.Columns.BeginUpdate;
      DataGrid.Header.Options := DataGrid.Header.Options + [hoVisible];
      DataGrid.Header.Columns.Clear;

      // No data for routines
      if SelectedTableColumns = nil then begin
        DataGrid.Enabled := False;
        pnlDataTop.Enabled := False;
        pnlFilter.Enabled := False;
        lblSorryNoData.Parent := DataGrid;
        Exit; // Jump to *finally*
      end else begin
        DataGrid.Enabled := True;
        pnlDataTop.Enabled := True;
        pnlFilter.Enabled := True;
        lblSorryNoData.Parent := tabData;
      end;

      // Prepare SELECT statement
      select_base := 'SELECT ';
      select_base_full := select_base;
      // Selected columns
      if (FDataGridSelect.Count = 0) or (FDataGridSelect.Count = SelectedTableColumns.RecordCount) then begin
        tbtnDataColumns.ImageIndex := 107;
      end else begin
        for i := FDataGridSelect.Count - 1 downto 0 do begin
          ColExists := False;
          SelectedTableColumns.First;
          while not SelectedTableColumns.Eof do begin
            if FDataGridSelect[i] = SelectedTableColumns.FieldByName('Field').AsWideString then begin
              ColExists := True;
              break;
            end;
            SelectedTableColumns.Next;
          end;
          if not ColExists then
            FDataGridSelect.Delete(i);
        end;
        // Signal for the user that we now hide some columns
        tbtnDataColumns.ImageIndex := 108;
      end;
      // Ensure key columns are included to enable editing
      KeyCols := GetKeyColumns;
      // Truncate column array.
      SetLength(DataGridResult.Columns, 0);
      debug('mem: initializing browse columns.');
      SelectedTableColumns.First;
      while not SelectedTableColumns.Eof do begin
        ColName := SelectedTableColumns.FieldByName('Field').AsWideString;
        ShowIt := (FDataGridSelect.Count=0) or (FDataGridSelect.IndexOf(ColName)>-1);
        if ShowIt or (KeyCols.IndexOf(ColName)>-1) then begin
          ColType := SelectedTableColumns.FieldByName('Type').AsString;
          rx.Expression := '^((tiny|medium|long)?(text|blob)|(var)?(char|binary))\b(\(\d+\))?';
          if rx.Exec(ColType) then begin
            select_base := select_base + ' ' + 'LEFT(' + Mask(ColName) + ', ' + IntToStr(GridMaxData) + ')' + ',';
          end else begin
            select_base := select_base + ' ' + Mask(ColName) + ',';
          end;
          select_base_full := select_base_full + ' ' + Mask(ColName) + ',';
          InitColumn(ColName, SelectedTableColumns.FieldByName('Type').AsString, ShowIt);
        end;
        SelectedTableColumns.Next;
      end;
      debug('mem: browse column initialization complete.');
      // Cut last comma
      select_base := copy( select_base, 1, Length(select_base)-1 );
      select_base_full := copy( select_base_full, 1, Length(select_base_full)-1 );
      // Include db name for cases in which dbtree is switching databases and pending updates are in process
      select_from := ' FROM '+mask(ActiveDatabase)+'.'+mask(SelectedTable.Text);

      // Final SELECT segments
      DataGridCurrentSelect := select_base;
      DataGridCurrentFullSelect := select_base_full;
      DataGridCurrentFrom := select_from;
      DataGridCurrentFilter := SynMemoFilter.Text;
      if Length(FDataGridSort) > 0 then
        DataGridCurrentSort := ComposeOrderClause(FDataGridSort)
      else
        DataGridCurrentSort := '';

      // Set button icons
      if DataGridCurrentFilter <> '' then tbtnDataFilter.ImageIndex := 108
      else tbtnDataFilter.ImageIndex := 107;
      if DataGridCurrentSort <> '' then tbtnDataSorting.ImageIndex := 108
      else tbtnDataSorting.ImageIndex := 107;

      debug('mem: initializing browse rows (internal data).');
      try
        ReachedEOT := False;
        SetLength(DataGridResult.Rows, SIMULATE_INITIAL_ROWS * (100 + SIMULATE_MORE_ROWS) div 100);
        for i := 0 to SIMULATE_INITIAL_ROWS * (100 + SIMULATE_MORE_ROWS) div 100 - 1 do begin
          DataGridResult.Rows[i].Loaded := False;
        end;
        debug('mem: initializing browse rows (grid).');
        DataGrid.RootNodeCount := SIMULATE_INITIAL_ROWS * (100 + SIMULATE_MORE_ROWS) div 100;
      except
        DataGrid.RootNodeCount := 0;
        SetLength(DataGridResult.Rows, 0);
        PageControlMain.ActivePage := tabDatabase;
        raise;
      end;
      debug('mem: browse row initialization complete.');

      // Switched to another table
      if DataGridTable <> SelectedTable.Text then begin
        DataGrid.OffsetXY := Point(0, 0); // Scroll to top left
        FreeAndNil(PrevTableColWidths); // Throw away remembered, manually resized column widths
      end;
      dataselected := true;

      PageControlMainChange(Self);
    end;
  finally
    DataGrid.Header.Columns.EndUpdate;
    DataGrid.EndUpdate;
    FreeAndNil(sl_query);
    if DataGridTable = SelectedTable.Text then
      DataGrid.OffsetXY := OldOffsetXY;
    viewingdata := false;
    EnumerateRecentFilters;
    Screen.Cursor := crDefault;
  end;
  DataGridDB := ActiveDatabase;
  DataGridTable := SelectedTable.Text;
  AutoCalcColWidths(DataGrid, PrevTableColWidths);
end;


{***
  Calculate + display total rowcount and found rows matching to filter
  in data-tab
}
procedure TMainForm.DisplayRowCountStats(MatchingRows: Int64);
var
  rows_total       : Int64; // total rowcount
  IsFiltered, IsInnodb: Boolean;
  ds: TDataSet;
  i: Integer;
  s: WideString;
begin
  lblDataTop.Caption := ActiveDatabase + '.' + SelectedTable.Text;

  IsFiltered := self.DataGridCurrentFilter <> '';
  if GetFocusedTreeNodeType = lntTable then begin
    // Get rowcount from table
    ds := FetchActiveDbTableList;
    rows_total := -1;
    IsInnodb := False;
    for i := 0 to ds.RecordCount - 1 do begin
      if ds.FieldByName(DBO_NAME).AsWideString = SelectedTable.Text then begin
        s := ds.FieldByName(DBO_ROWS).AsString;
        if s <> '' then rows_total := MakeInt(s);
        IsInnodb := ds.Fields[1].AsString = 'InnoDB';
        break;
      end;
    end;

    if rows_total > -1 then begin
      lblDataTop.Caption := lblDataTop.Caption + ': ' + FormatNumber(rows_total) + ' rows total';
      if IsInnodb then lblDataTop.Caption := lblDataTop.Caption + ' (approximately)';

      if MatchingRows = prefMaxTotalRows then begin
        lblDataTop.Caption := lblDataTop.Caption + ', limited to ' + FormatNumber(prefMaxTotalRows);
      end else if IsFiltered then begin
        if MatchingRows = rows_total then begin
          lblDataTop.Caption := lblDataTop.Caption + ', filter matches all rows';
        end else if IsFiltered and (MatchingRows > -1) then begin
          lblDataTop.Caption := lblDataTop.Caption + ', ' + FormatNumber(MatchingRows) + ' matches filter';
        end;
      end;
    end;
  end;
end;


procedure TMainForm.WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery; ForceDialog: Boolean);
var
  signal: Cardinal;
begin
  debug( 'Waiting for query to complete.' );
  cancelling := false;
  if ForceDialog then begin
    debug( 'Showing progress form.' );
    WaitForm.ShowModal();
  end else begin
    signal := WaitForSingleObject(query.EventHandle, QueryWaitTime);
    if signal = 0 then debug( 'Query completed within ' + IntToStr(QueryWaitTime) + 'msec.' )
    else begin
      debug( IntToStr(QueryWaitTime) + 'msec passed, showing progress form.' );
      // Hack: Prevent dynamic loading of records in the context of the wait form's message loop.
      DataGrid.Visible := False;
      WaitForm.ShowModal();
    end;
  end;
  CloseHandle(query.EventHandle);
  debug( 'Query complete.' );
end;


{***
  Occurs when active tab has changed.
}
procedure TMainForm.PageControlMainChange(Sender: TObject);
var
  tab: TTabSheet;
begin
  tab := PageControlMain.ActivePage;

  // Move focus to relevant controls in order for them to receive keyboard events.
  // Do this only if the user clicked the new tab. Not on automatic tab changes.
  if Sender = PageControlMain then begin
    if tab = tabHost then PageControlHostChange(Sender)
    else if tab = tabDatabase then ListTables.SetFocus
    else if tab = tabData then begin
      viewdata(Sender);
      if DataGrid.CanFocus then
        DataGrid.SetFocus;
    end else if IsQueryTab(tab.PageIndex, True) then begin
      ActiveQueryMemo.SetFocus;
      ActiveQueryMemo.WordWrap := actQueryWordWrap.Checked;
      SynMemoQueryStatusChange(ActiveQueryMemo, []);
    end;
  end;

  // Ensure controls are in a valid state
  ValidateControls(Sender);
  FixQueryTabCloseButtons;
end;


procedure TMainForm.PageControlHostChange(Sender: TObject);
var
  tab: TTabSheet;
  list: TBaseVirtualTree;
begin
  tab := PageControlHost.ActivePage;
  if tab = tabVariables then list := ListVariables
  else if tab = tabStatus then list := ListStatus
  else if tab = tabProcesslist then list := ListProcesses
  else if tab = tabCommandStats then list := ListCommandStats
  else Exit; // Silence compiler warning
  list.SetFocus;
  editFilterVTChange(Sender);
end;


{***
  Ensures that we're connected to the currently selected database.
}
procedure TMainForm.EnsureDatabase;
var
  db: WideString;
begin
  // Some functions temporarily switch away from the database selected by the user, handle that.
  if TemporaryDatabase <> '' then db := TemporaryDatabase
  else db := ActiveDatabase;
  // Blank = database undefined
  if db = '' then Exit;
  if (FMysqlConn.Connection.Database <> db) or (UserQueryFired and not UserQueryFiring) then begin
    ExecUseQuery(db, false, false);
    UserQueryFired := false;
    FMysqlConn.Connection.Database := db;
  end;
end;


{***
  Look for list of tables for current database in cache.
  Retrieve from server if necessary.
  @return TDataSet The cached list of tables for the active database.
}
function TMainForm.FetchActiveDbTableList: TDataSet;
begin
  Result := FetchDbTableList(ActiveDatabase);
end;

function TMainForm.FetchDbTableList(db: WideString): TDataSet;
var
  ds: TDataSet;
  OldCursor: TCursor;
  Unions: TWideStringlist;
  ListObjectsSQL: WideString;
begin
  if not DbTableListCachedAndValid(db) then begin
    // Not in cache, load table list.
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    ShowStatus('Fetching tables from "' + db + '" ...');
    try
      if not Assigned(InformationSchemaTables) then
        InformationSchemaTables := GetCol('SHOW TABLES FROM '+mask(DBNAME_INFORMATION_SCHEMA), 0, True, False);
      if InformationSchemaTables.IndexOf('TABLES') > -1 then begin
        Unions := TWideStringlist.Create;

        // Tables and (system) views
        Unions.Add('SELECT TABLE_NAME AS '+mask(DBO_NAME)+
            ', TABLE_TYPE AS '+mask(DBO_TYPE)+
            ', ENGINE AS '+mask(DBO_ENGINE)+
            ', VERSION AS '+mask(DBO_VERSION)+
            ', ROW_FORMAT AS '+mask(DBO_ROWFORMAT)+
            ', TABLE_ROWS AS '+mask(DBO_ROWS)+
            ', AVG_ROW_LENGTH AS '+mask(DBO_AVGROWLEN)+
            ', DATA_LENGTH AS '+mask(DBO_DATALEN)+
            ', MAX_DATA_LENGTH AS '+mask(DBO_MAXDATALEN)+
            ', INDEX_LENGTH AS '+mask(DBO_INDEXLEN)+
            ', DATA_FREE AS '+mask(DBO_DATAFREE)+
            ', AUTO_INCREMENT AS '+mask(DBO_AUTOINC)+
            ', CREATE_TIME AS '+mask(DBO_CREATED)+
            ', UPDATE_TIME AS '+mask(DBO_UPDATED)+
            ', CHECK_TIME AS '+mask(DBO_CHECKED)+
            ', TABLE_COLLATION AS '+mask(DBO_COLLATION)+
            ', CHECKSUM AS '+mask(DBO_CHECKSUM)+
            ', CREATE_OPTIONS AS '+mask(DBO_CROPTIONS)+
            ', TABLE_COMMENT AS '+mask(DBO_COMMENT)+
          ' FROM '+mask(DBNAME_INFORMATION_SCHEMA)+'.TABLES ' +
          'WHERE TABLE_SCHEMA = '+esc(db));

        // Stored routines
        if InformationSchemaTables.IndexOf('ROUTINES') > -1 then begin
          Unions.Add('SELECT ROUTINE_NAME AS '+mask(DBO_NAME)+
            ', ROUTINE_TYPE AS '+mask(DBO_TYPE)+
            ', NULL AS '+mask(DBO_ENGINE)+
            ', NULL AS '+mask(DBO_VERSION)+
            ', NULL AS '+mask(DBO_ROWFORMAT)+
            ', NULL AS '+mask(DBO_ROWS)+
            ', NULL AS '+mask(DBO_AVGROWLEN)+
            ', NULL AS '+mask(DBO_DATALEN)+
            ', NULL AS '+mask(DBO_MAXDATALEN)+
            ', NULL AS '+mask(DBO_INDEXLEN)+
            ', NULL AS '+mask(DBO_DATAFREE)+
            ', NULL AS '+mask(DBO_AUTOINC)+
            ', CREATED AS '+mask(DBO_CREATED)+
            ', LAST_ALTERED AS '+mask(DBO_UPDATED)+
            ', NULL AS '+mask(DBO_CHECKED)+
            ', NULL AS '+mask(DBO_COLLATION)+
            ', NULL AS '+mask(DBO_CHECKSUM)+
            ', NULL AS '+mask(DBO_CROPTIONS)+
            ', ROUTINE_COMMENT AS '+mask(DBO_COMMENT)+
            ' FROM '+mask(DBNAME_INFORMATION_SCHEMA)+'.ROUTINES ' +
            'WHERE ROUTINE_SCHEMA = '+esc(db));
        end;
        if Unions.Count = 1 then
          ListObjectsSQL := Unions[0]
        else
          ListObjectsSQL := '(' + implodestr(') UNION (', Unions) + ')';
        ListObjectsSQL := ListObjectsSQL + ' ORDER BY `Name`';
        FreeAndNil(Unions);
      end else begin
        // For servers lacking the INFORMATION_SCHEMA or the TABLES table
        ListObjectsSQL := 'SHOW TABLE STATUS FROM ' + mask(db);
      end;
      ds := GetResults(ListObjectsSQL);
      CachedTableLists.AddObject(db, ds);
      // Add table names to SQL highlighter
      SynSQLSyn1.TableNames.BeginUpdate;
      while not ds.Eof do begin
        SynSQLSyn1.TableNames.Add(ds.FieldByName(DBO_NAME).AsWideString);
        ds.Next;
      end;
      SynSQLSyn1.TableNames.EndUpdate;
    finally
      ShowStatus(STATUS_MSG_READY);
      Screen.Cursor := OldCursor;
    end;
  end;
  Result := TDataSet(CachedTableLists.Objects[CachedTableLists.IndexOf(db)]);
  Result.First;
end;


{***
  Nukes cached table list for active database, then refreshes it.
  @return TDataSet The newly cached list of tables for the active database.
}
function TMainForm.RefreshActiveDbTableList: TDataSet;
begin
  Result := RefreshDbTableList(ActiveDatabase);
end;

function TMainForm.RefreshDbTableList(db: WideString): TDataSet;
begin
  ClearDbTableList(db);
  Result := FetchDbTableList(db);
end;

procedure TMainForm.ClearDbTableList(db: WideString);
var
  idx: Integer;
  o: TObject;
begin
  idx := CachedTableLists.IndexOf(db);
  if idx > -1 then begin
    o := CachedTableLists.Objects[idx];
    FreeAndNil(o);
    CachedTableLists.Delete(idx);
  end;
end;


{***
  Nukes the table list cache.
}
procedure TMainForm.ClearAllTableLists;
var
  idx: Integer;
  ds: TDataSet;
begin
  for idx := 0 to CachedTableLists.Count - 1 do begin
    ds := TDataSet(CachedTableLists.Objects[idx]);
    ds.Close;
    FreeAndNil(ds);
  end;
  CachedTableLists.Clear;
end;


// Fetch content from a row cell, avoiding NULLs to cause AVs
function TMainForm.FieldContent(ds: TDataSet; ColName: WideString): WideString;
begin
  Result := '';
  if
    (ds.FindField(colName) <> nil) and
    (not ds.FindField(ColName).IsNull)
  then
    Result := ds.FieldByName(ColName).AsWideString;
end;


procedure TMainForm.LoadDatabaseProperties(db: WideString);
var
  i, img          : Integer;
  bytes           : Int64;
  ds              : TDataSet;
  Cap,
  SelectedCaptions: WideStrings.TWideStringList;
begin
  // DB-Properties
  Screen.Cursor := crHourGlass;

  // Remember selected nodes
  SelectedCaptions := GetVTCaptions(ListTables, True);

  ds := FetchDbTableList(db);
  ShowStatus( 'Displaying tables from "' + db + '" ...' );

  ListTables.BeginUpdate;
  ListTables.Clear;

  SetLength(VTRowDataListTables, ds.RecordCount);
  for i := 1 to ds.RecordCount do
  begin
    VTRowDataListTables[i-1].Captions := WideStrings.TWideStringList.Create;
    Cap := VTRowDataListTables[i-1].Captions;
    // Object name
    Cap.Add( FieldContent(ds, DBO_NAME) );
    if (FieldContent(ds, DBO_ROWS) <> '') then
      Cap.Add( FormatNumber( FieldContent(ds, DBO_ROWS) ) )
    else Cap.Add('');
    // Size: Data_length + Index_length
    bytes := GetTableSize(ds);
    if bytes >= 0 then Cap.Add(FormatByteNumber(bytes))
    else Cap.Add('');
    Cap.Add( FieldContent(ds, DBO_CREATED) );
    Cap.Add( FieldContent(ds, DBO_UPDATED) );
    Cap.Add( FieldContent(ds, DBO_ENGINE) );
    Cap.Add( FieldContent(ds, DBO_COMMENT) );
    Cap.Add( FieldContent(ds, DBO_VERSION) );
    Cap.Add( FieldContent(ds, DBO_ROWFORMAT) );
    if (FieldContent(ds, DBO_AVGROWLEN) <> '') then
      Cap.Add( FormatByteNumber(FieldContent(ds, DBO_AVGROWLEN)) )
    else Cap.Add('');
    if (FieldContent(ds, DBO_MAXDATALEN) <> '') then
      Cap.Add( FormatByteNumber(FieldContent(ds, DBO_MAXDATALEN)) )
    else Cap.Add('');
    if (FieldContent(ds, DBO_INDEXLEN) <> '') then
      Cap.Add( FormatByteNumber(FieldContent(ds, DBO_INDEXLEN)) )
    else Cap.Add('');
    if (FieldContent(ds, DBO_DATAFREE) <> '') then
      Cap.Add( FormatByteNumber(FieldContent(ds, DBO_DATAFREE)) )
    else Cap.Add('');
    if (FieldContent(ds, DBO_AUTOINC) <> '') then
      Cap.Add( FormatNumber(FieldContent(ds, DBO_AUTOINC)) )
    else Cap.Add('');
    Cap.Add( FieldContent(ds, DBO_AUTOINC) );
    Cap.Add( FieldContent(ds, DBO_COLLATION) );
    Cap.Add( FieldContent(ds, DBO_CHECKSUM) );
    Cap.Add( FieldContent(ds, DBO_CROPTIONS) );
    if ds.FindField(DBO_TYPE) <> nil then
      Cap.Add(FieldContent(ds, DBO_TYPE))
    else
      Cap.Add('BASE TABLE');

    VTRowDataListTables[i-1].NodeType := GetDBObjectType( ds.Fields);
    // Find icon
    case VTRowDataListTables[i-1].NodeType of
      lntTable:         img := ICONINDEX_TABLE;
      lntCrashedTable:  img := ICONINDEX_CRASHED_TABLE;
      lntView:          img := ICONINDEX_VIEW;
      lntProcedure:     img := ICONINDEX_STOREDPROCEDURE;
      lntFunction:      img := ICONINDEX_STOREDFUNCTION;
      else              img := -1;
    end;
    VTRowDataListTables[i-1].ImageIndex := img;

    ds.Next;
  end;
  ListTables.RootNodeCount := Length(VTRowDataListTables);
  ListTables.EndUpdate;
  SetVTSelection(ListTables, SelectedCaptions);
  showstatus(db + ': ' + IntToStr(ListTables.RootNodeCount) +' table(s)', 0);
  tabDatabase.Caption := sstr('Database: ' + db, 30);
  ShowStatus(STATUS_MSG_READY);
  Screen.Cursor := crDefault;
  // Ensure tree db node displays its chidren initialized
  DBtree.ReinitChildren(FindDBNode(db), False);
  ValidateControls(Self);
end;


{ Show tables and their properties on the tabsheet "Database" }
procedure TMainForm.ShowDBProperties(db: WideString);
begin
  Screen.Cursor := crHourglass;
  PageControlMainChange(Self);
  ShowStatus( STATUS_MSG_READY );
  Screen.Cursor := crDefault;
end;


{***
  Execute a query and return a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
}
function TMainForm.ExecuteQuery(query: String): TDataSet;
begin
  result := GetResults(query, false, false);
end;


{***
  Execute a query without returning a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
}
procedure TMainForm.ExecuteNonQuery(SQLQuery: String);
begin
  ExecUpdateQuery(SQLQuery);
end;



{***
  Selection in ListTables is changing
}
procedure TMainForm.ListTablesChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  ValidateControls(Sender);
end;



{***
  Enable/disable various buttons and menu items.
  Invoked when
    - active sheet changes
    - highlighted database changes
    ...
}
procedure TMainForm.ValidateControls(Sender: TObject);
var
  inDataGrid, inDataOrQueryTab, inDataOrQueryTabNotEmpty: Boolean;
  SelectedNodes: TNodeArray;
begin
  inDataGrid := ActiveControl = DataGrid;
  inDataOrQueryTab := (PageControlMain.ActivePage = tabData) or QueryTabActive;
  inDataOrQueryTabNotEmpty := inDataOrQueryTab and (ActiveGrid.RootNodeCount > 0);

  SelectedNodes := ListTables.GetSortedSelection(False);

  actSQLhelp.Enabled := mysql_version >= 40100;
  actImportCSV.Enabled := mysql_version >= 32206;

  // Data tab - if query results are made editable, these will need
  //            to be changed to look at which tab is focused.
  actDataInsert.Enabled := inDataGrid;
  actDataDelete.Enabled := inDataGrid and (DataGrid.SelectedCount > 0);
  actDataFirst.Enabled := inDataGrid;
  actDataLast.Enabled := inDataGrid;
  actDataPostChanges.Enabled := inDataGrid and DataGridHasChanges;
  actDataCancelChanges.Enabled := inDataGrid and DataGridHasChanges;

  // Activate export-options if we're on Data- or Query-tab
  actCopyAsCSV.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsHTML.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsXML.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsSQL.Enabled := inDataOrQueryTabNotEmpty;
  actExportData.Enabled := inDataOrQueryTabNotEmpty;
  actHTMLView.Enabled := inDataOrQueryTabNotEmpty and Assigned(ActiveGrid.FocusedNode);
  setNull1.Enabled := inDataGrid and Assigned(DataGrid.FocusedNode);

  // Query tab
  // Manually invoke OnChange event of tabset to fill helper list with data
  if QueryTabActive then RefreshQueryHelpers;

  ValidateQueryControls(Sender);

  if not QueryTabActive then // Empty panel with "Line:Char"
    showstatus('', 1);
end;

procedure TMainForm.RefreshQueryHelpers;
var
  dummy: Boolean;
begin
  dummy := True;
  ActiveQueryTabset.OnChange(Self, ActiveQueryTabset.TabIndex, dummy);
end;

procedure TMainForm.ValidateQueryControls(Sender: TObject);
var
  NotEmpty, HasSelection: Boolean;
  Memo: TSynMemo;
begin
  if QueryTabActive then
    Memo := ActiveQueryMemo
  else
    Memo := nil;
  NotEmpty := QueryTabActive and (Memo.GetTextLen > 0);
  HasSelection := QueryTabActive and Memo.SelAvail;
  actExecuteQuery.Enabled := QueryTabActive and NotEmpty;
  actExecuteSelection.Enabled := QueryTabActive and HasSelection;
  actExecuteLine.Enabled := QueryTabActive and (Memo.LineText <> '');
  actSaveSQL.Enabled := QueryTabActive and NotEmpty;
  actSaveSQLselection.Enabled := QueryTabActive and HasSelection;
  actSaveSQLSnippet.Enabled := QueryTabActive and NotEmpty;
  actSaveSQLSelectionSnippet.Enabled := QueryTabActive and HasSelection;
  actQueryFind.Enabled := QueryTabActive and NotEmpty;
  actQueryReplace.Enabled := QueryTabActive and NotEmpty;
  actQueryStopOnErrors.Enabled := QueryTabActive;
  actQueryWordWrap.Enabled := QueryTabActive;
  actClearQueryEditor.Enabled := QueryTabActive and NotEmpty;
  actSetDelimiter.Enabled := QueryTabActive;
  actCloseQueryTab.Enabled := IsQueryTab(PageControlMain.ActivePageIndex, False);
end;


procedure TMainForm.CheckUptime;
begin
  ServerUptime := MakeInt(GetVar('SHOW STATUS LIKE ''Uptime''', 1));
  // Avoid division by zero
  ServerUptime := Max(ServerUptime, 1);
  TimerHostUptime.Enabled := true;
end;


procedure TMainForm.KillProcess(Sender: TObject);
var t : Boolean;
  ProcessIDs : WideStrings.TWideStringList;
  i : Integer;
begin
  t := TimerRefresh.Enabled;
  TimerRefresh.Enabled := false; // prevent av (ListProcesses.selected...)
  ProcessIDs := GetVTCaptions( ListProcesses, True );
  if MessageDlg('Kill '+inttostr(ProcessIDs.count)+' Process(es)?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
  begin
    for i := 0 to ProcessIDs.Count - 1 do
    begin
      // Don't kill own process
      if ProcessIDs[i] = IntToStr( MySQLConn.Connection.GetThreadId ) then
        LogSQL('Ignoring own process id '+ProcessIDs[i]+' when trying to kill it.')
      else
        ExecUpdateQuery( 'KILL '+ProcessIDs[i] );
    end;
    ListProcesses.Tag := VTREE_NOTLOADED;
    ListProcesses.Repaint;
  end;
  TimerRefresh.Enabled := t; // re-enable autorefresh timer
end;


procedure TMainForm.ExecSQLClick(Sender: TObject; Selection: Boolean=false; CurrentLine: Boolean=false);
var
  SQL               : WideStrings.TWideStringList;
  i, j              : Integer;
  rowsaffected      : Integer;
  SQLstart          : Integer;
  SQLend            : Integer;
  SQLscriptstart    : Integer;
  SQLscriptend      : Integer;
  SQLTime           : Double;
  LastVistaCheck    : Cardinal;
  VistaCheck        : Boolean;
  fieldcount        : Integer;
  recordcount       : Integer;
  ds                : TDataSet;
  ColName,
  Text, LB          : WideString;
  col               : TVirtualTreeColumn;
  ResultLabel       : TLabel;
  ActiveGridResult  : TGridResult;
begin
  ResultLabel := ActiveQueryControl(LabelResultInfo) as TLabel;
  if CurrentLine then Text := ActiveQueryMemo.LineText
  else if Selection then Text := ActiveQueryMemo.SelText
  else Text := ActiveQueryMemo.Text;
  // Give text back its original linebreaks if possible
  case QueryMemoLineBreaks of
    lbsUnix: LB := LB_UNIX;
    lbsMac: LB := LB_MAC;
    lbsWide: LB := LB_WIDE;
  end;
  if LB <> '' then
    Text := WideStringReplace(Text, CRLF, LB, [rfReplaceAll]);
  SQL := parseSQL(Text);

  if ( SQL.Count = 0 ) then
  begin
    ResultLabel.Caption := '(nothing to do)';
    Exit;
  end;

  SQLscriptstart := GetTickCount();
  LastVistaCheck := GetTickCount();
  ResultLabel.Caption := '';

  ds := nil;
  try
    showstatus( 'Initializing SQL...' );
    actExecuteQuery.Enabled := false;
    actExecuteSelection.Enabled := false;

    // Let EnsureActiveDatabase know that we've fired user queries.
    UserQueryFiring := true;

    rowsaffected := 0;
    fieldcount := 0;
    recordcount := 0;
    EnableProgressBar(SQL.Count);

    showstatus( 'Executing SQL...' );
    for i := 0 to (SQL.Count - 1) do
    begin
      ProgressBarStatus.StepIt;
      ProgressBarStatus.Repaint;
      if ( sql[i] = '' ) then
      begin
        continue;
      end;
      // open last query with data-aware:
      ResultLabel.Caption := '';
      // ok, let's rock
      SQLstart := GetTickCount();
      try
        VistaCheck := false;
        if GetTickCount() - LastVistaCheck > 2500 then begin
          VistaCheck := true;
          LastVistaCheck := GetTickCount();
        end;
        ds := GetResults( SQL[i], false, false, VistaCheck );
        if ( ds <> nil ) then
        begin
          fieldcount := ds.Fieldcount;
          recordcount := ds.Recordcount;
          rowsaffected := rowsaffected + TZQuery(ds).RowsAffected;
        end
        else
        begin
          fieldcount := 0;
          recordcount := 0;
          rowsaffected := FMysqlConn.Connection.GetAffectedRowsFromLastPost;
        end;
      except
        on E:Exception do
        begin
          if actQueryStopOnErrors.Checked or (i = SQL.Count - 1) then begin
            Screen.Cursor := crDefault;
            MessageDlg( E.Message, mtError, [mbOK], 0 );
            ProgressBarStatus.Hide;
            actExecuteQuery.Enabled := true;
            actExecuteSelection.Enabled := true;
            Break;
          end;
        end;
      end;

      SQLend := GetTickCount();
      SQLTime := (SQLend - SQLstart) / 1000;

      ResultLabel.Caption :=
        FormatNumber( rowsaffected ) +' row(s) affected, '+
        FormatNumber( fieldcount ) +' column(s) x '+
        FormatNumber( recordcount ) +' row(s) in last result set.';
      if ( SQL.Count = 1 ) then
      begin
        ResultLabel.Caption := ResultLabel.Caption +
          ' Query time: '+ FormatNumber( SQLTime, 3) +' sec.';
      end;
    end;

    ProgressBarStatus.Hide;
    ValidateQueryControls(Sender);

    if ( SQL.Count > 1 ) then
    begin
      SQLscriptend := GetTickCount();
      SQLTime := (SQLscriptend - SQLscriptstart) / 1000;
      ResultLabel.Caption := ResultLabel.Caption +' Batch time: '+
        FormatNumber( SQLTime, 3 ) +' sec.';
    end;

  finally
    // Let EnsureActiveDatabase know that we've fired user queries.
    UserQueryFired := true;
    UserQueryFiring := false;

    // Avoid excessive GridHighlightChanged() when flicking controls.
    viewingdata := true;

    if ds <> nil then begin
      ActiveGrid.BeginUpdate;
      ActiveGrid.Header.Options := ActiveGrid.Header.Options + [hoVisible];
      ActiveGrid.Header.Columns.BeginUpdate;
      ActiveGrid.Header.Columns.Clear;
      debug('mem: clearing and initializing query columns.');
      ActiveGridResult := GridResult(ActiveGrid);
      SetLength(ActiveGridResult.Columns, 0);
      SetLength(ActiveGridResult.Columns, ds.FieldCount);
      for i:=0 to ds.FieldCount-1 do begin
        ColName := ds.Fields[i].FieldName;
        col := ActiveGrid.Header.Columns.Add;
        col.Text := ColName;
        col.Options := col.Options - [coAllowClick];
        ActiveGridResult.Columns[i].Name := ColName;
        if ds.Fields[i].DataType in [ftSmallint, ftInteger, ftWord, ftLargeint] then begin
          ActiveGridResult.Columns[i].DatatypeCat := dtcInteger;
          col.Alignment := taRightJustify;
        end else if ds.Fields[i].DataType in [ftFloat] then begin
          ActiveGridResult.Columns[i].DatatypeCat := dtcReal;
          col.Alignment := taRightJustify;
        end else if ds.Fields[i].DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
          ActiveGridResult.Columns[i].DatatypeCat := dtcTemporal
        else if ds.Fields[i].DataType in [ftWideString, ftMemo, ftWideMemo] then
          ActiveGridResult.Columns[i].DatatypeCat := dtcText
        else if ds.Fields[i].DataType in [ftBlob] then
          ActiveGridResult.Columns[i].DatatypeCat := dtcBinary;
      end;
      debug('mem: query column initialization complete.');
      debug('mem: clearing and initializing query rows (internal data).');
      SetLength(ActiveGridResult.Rows, 0);
      SetLength(ActiveGridResult.Rows, ds.RecordCount);
      ds.First;
      for i:=0 to ds.RecordCount-1 do begin
        ActiveGridResult.Rows[i].Loaded := True;
        SetLength(ActiveGridResult.Rows[i].Cells, ds.FieldCount);
        for j:=0 to ds.FieldCount-1 do begin
          if ActiveGridResult.Columns[j].DatatypeCat = dtcBinary then
            ActiveGridResult.Rows[i].Cells[j].Text := '0x' + BinToWideHex(ds.Fields[j].AsString)
          else
            ActiveGridResult.Rows[i].Cells[j].Text := ds.Fields[j].AsWideString;
          ActiveGridResult.Rows[i].Cells[j].IsNull := ds.Fields[j].IsNull;
        end;
        ds.Next;
      end;
      ds.Free;
      debug('mem: initializing query rows (grid).');
      ActiveGrid.RootNodeCount := Length(ActiveGridResult.Rows);
      debug('mem: query row initialization complete.');
      ActiveGrid.Header.Columns.EndUpdate;
      ActiveGrid.ClearSelection;
      ActiveGrid.OffsetXY := Point(0, 0);
      ActiveGrid.EndUpdate;
      AutoCalcColWidths(ActiveGrid);
    end;
    // Ensure controls are in a valid state
    ValidateControls(Sender);
    viewingdata := false;
    Screen.Cursor := crDefault;
    ShowStatus( STATUS_MSG_READY );
  end;
end;


{ Proposal about to insert a String into synmemo }
procedure TMainForm.SynCompletionProposal1CodeCompletion(Sender: TObject;
  var Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


procedure TMainForm.SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
  const Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


{ Proposal-Combobox pops up }
procedure TMainForm.SynCompletionProposal1Execute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
  var CanExecute: Boolean);
var
  i,j              : Integer;
  ds               : TDataset;
  sql, TableClauses: WideString;
  Tables           : TStringList;
  tablename        : WideString;
  rx               : TRegExpr;
  PrevShortToken,
  PrevLongToken,
  Token            : WideString;
  Start,
  TokenTypeInt     : Integer;
  Attri            : TSynHighlighterAttributes;
  Editor           : TCustomSynEdit;
const
  ItemPattern: WideString = '\image{%d}\hspace{5}\color{clSilver}%s\column{}\color{clWindowText}%s';

  procedure addTable( Fields: TFields );
  var ObjName, ObjType: WideString; Icon: Integer;
  begin
    ObjName := Fields[0].AsWideString;
    ObjType := '';
    if Fields.FindField(DBO_TYPE) <> nil then
      ObjType := LowerCase(Fields.FieldByName(DBO_TYPE).AsString);
    case GetDBObjectType(Fields) of
      lntTable: Icon := ICONINDEX_TABLE;
      lntCrashedTable: Icon := ICONINDEX_CRASHED_TABLE;
      lntFunction: Icon := ICONINDEX_STOREDFUNCTION;
      lntProcedure: Icon := ICONINDEX_STOREDPROCEDURE;
      lntView: Icon := ICONINDEX_VIEW;
      else Icon := -1;
    end;
    SynCompletionProposal1.InsertList.Add( ObjName );
    SynCompletionProposal1.ItemList.Add( WideFormat(ItemPattern, [Icon, ObjType, ObjName]) );
  end;

  procedure addColumns( tablename: WideString );
  var
    dbname : WideString;
    i : Integer;
    ds : TDataSet;
  begin
    dbname := ActiveDatabase;
    if Pos( '.', tablename ) > -1 then
    begin
      dbname := Copy( tablename, 0, Pos( '.', tablename )-1 );
      tablename := Copy( tablename, Pos( '.', tablename )+1, Length(tablename) );
    end;
    // Do not mask db and table name to avoid double masking.
    // Rely on what the user typed is already a valid masked/quoted identifier.
    if dbname <> '' then
      tablename := dbname + '.' + tablename;
    ds := getResults( 'SHOW COLUMNS FROM '+tablename, true, false );
    if ds = nil then exit;
    for i:=0 to ds.RecordCount-1 do
    begin
      SynCompletionProposal1.InsertList.Add( ds.FieldByName( 'Field' ).AsWideString );
      SynCompletionProposal1.ItemList.Add( WideFormat(ItemPattern, [ICONINDEX_FIELD, GetFirstWord(ds.FieldByName('Type').AsString), ds.FieldByName('Field').AsWideString]) );
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
  end;

begin
  Editor := (Sender as TSynCompletionProposal).Editor;
  Editor.GetHighlighterAttriAtRowColEx(Editor.CaretXY, Token, TokenTypeInt, Start, Attri);
  if TtkTokenKind(TokenTypeInt) = tkString then begin
    CanExecute := False;
    Exit;
  end;

  SynCompletionProposal1.InsertList.Clear;
  SynCompletionProposal1.ItemList.Clear;
  PrevShortToken := SynCompletionProposal1.PreviousToken;
  PrevShortToken := WideDequotedStr(PrevShortToken, '`');

  rx := TRegExpr.Create;

  // Find longer token, ignore EndOfTokenChars, just the last chars up to a whitespace
  rx.Expression := '(\S+).$';
  PrevLongToken := Copy(Editor.LineText, 0, x);
  if rx.Exec(PrevLongToken) then
    PrevLongToken := rx.Match[1]
  else
    PrevLongToken := '';

  // Get column-names into the proposal pulldown
  // when we write sql like "SELECT t.|col FROM table [AS] t"
  // Current limitation: Identifiers (masked or not) containing
  // spaces are not detected correctly.

  // 1. find the currently edited sql-statement around the cursor position in synmemo
  j := Length(Editor.Text);
  for i := Editor.SelStart+1024 downto Editor.SelStart-1024 do
  begin
    if i > j then
      continue;
    if i < 1 then
      break;
    sql := Editor.Text[i] + sql;
  end;

  // 2. Parse FROM clause to detect relevant table/view, probably aliased
  rx.ModifierG := True;
  rx.ModifierI := True;
  rx.Expression := '\b(FROM|INTO|UPDATE)\s+(.+)(WHERE|HAVING|ORDER|GROUP)?';
  if rx.Exec(sql) then begin
    TableClauses := rx.Match[2];
    // Ensure tables in JOIN clause(s) are splitted by comma
    TableClauses := WideStringReplace(TableClauses, 'JOIN', ',', [rfReplaceAll, rfIgnoreCase]);
    // Split table clauses by commas
    Tables := TStringList.Create;
    Tables.Delimiter := ',';
    Tables.StrictDelimiter := true;
    Tables.DelimitedText := TableClauses;
    rx.Expression := '(\S+)\s+(AS\s+)?(\S+)';

    for i := 0 to Tables.Count - 1 do begin
      // If the just typed word equals the alias of this table or the
      // tablename itself, set tablename var and break loop
      if rx.Exec(Tables[i]) then begin
        if PrevShortToken = WideDequotedStr(rx.Match[3],'`') then begin
          tablename := rx.Match[1];
          break;
        end;
      end;
    end;
  end;
  rx.Free;

  if (tablename <> '') then begin
    // add columns to proposal
    addColumns( tablename );
  end else if PrevLongToken <> '' then begin
    // assuming previoustoken itself is a table
    addColumns( PrevLongToken );
  end;


  if Length(CurrentInput) = 0 then // makes only sense if the user has typed "database."
  begin
    i := Databases.IndexOf(PrevShortToken);
    if i > -1 then begin
      // Only display tables from specified db
      Screen.Cursor := crHourGlass;
      ds := FetchDbTableList(Databases[i]);
      while not ds.Eof do begin
        addTable(ds.Fields);
        ds.Next;
      end;
      Screen.Cursor := crDefault;
    end;
  end;

  if SynCompletionProposal1.ItemList.count = 0 then begin
    // Add databases
    for i := 0 to Databases.Count - 1 do begin
      SynCompletionProposal1.InsertList.Add(Databases[i]);
      SynCompletionProposal1.ItemList.Add(WideFormat(ItemPattern, [ICONINDEX_DB, 'database', Databases[i]]));
    end;

    if ActiveDatabase <> '' then begin
      // Display tables from current db
      ds := FetchActiveDbTableList;
      while not ds.Eof do begin
        addTable(ds.Fields);
        ds.Next;
      end;
      if Length(CurrentInput) = 0 then // assume that we have already a dbname in memo
        SynCompletionProposal1.Position := Databases.Count;
    end;

    // Add functions
    for i := 0 to Length(MySQLFunctions) - 1 do begin
      // Don't display unsupported functions here
      if MySqlFunctions[i].Version > mysql_version then
        continue;
      SynCompletionProposal1.InsertList.Add( MySQLFunctions[i].Name + MySQLFunctions[i].Declaration );
      SynCompletionProposal1.ItemList.Add( WideFormat(ItemPattern, [ICONINDEX_FUNCTION, 'function', MySQLFunctions[i].Name + '\color{clSilver}' + MySQLFunctions[i].Declaration] ) );
    end;

    // Add keywords
    for i := 0 to MySQLKeywords.Count - 1 do begin
      SynCompletionProposal1.InsertList.Add( MySQLKeywords[i] );
      SynCompletionProposal1.ItemList.Add( WideFormat(ItemPattern, [ICONINDEX_KEYWORD, 'keyword', MySQLKeywords[i]] ) );
    end;

  end;

end;


procedure TMainForm.SynMemoQueryStatusChange(Sender: TObject; Changes:
    TSynStatusChanges);
var
  sm: TSynMemo;
begin
  sm := Sender as TSynMemo;
  ValidateQueryControls(Sender);
  showstatus(FormatNumber(sm.CaretY)+' : '+FormatNumber(sm.CaretX), 1);
end;



procedure TMainForm.TimerHostUptimeTimer(Sender: TObject);
var
  days, hours, minutes, seconds : Integer;
  msg: string;
begin
  // Host-Uptime
  days:= ServerUptime div (60*60*24);
  seconds := ServerUptime mod (60*60*24);
  hours := seconds div (60*60);
  seconds := seconds mod (60*60);
  minutes  := seconds div 60;
  seconds := seconds mod 60;

  inc(ServerUptime);
  msg := Format('%d days, %.2d:%.2d:%.2d', [days,hours,minutes,seconds]);
  if TimerHostUptime.Enabled then msg := Format('Uptime: %s', [msg])
  else msg := '';
  showstatus(msg, 4);
end;


procedure TMainForm.FormActivate(Sender: TObject);
begin
  TimerConnected.OnTimer(self);
end;


procedure TMainForm.ListTablesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  NodeData: PVTreeData;
begin
  // Tables and views can be renamed, routines cannot
  NodeData := Sender.GetNodeData(Node);
  Allowed := NodeData.NodeType in [lntTable, lntView];
end;


{***
  Rename table after checking the new name for invalid characters
}
procedure TMainForm.ListTablesNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  NodeData : PVTreeData;
begin
  // Fetch data from node
  NodeData := Sender.GetNodeData(Node);

  // Try to rename, on any error abort and don't rename ListItem
  try
    ensureValidIdentifier( NewText );
    // rename table
    ExecUpdateQuery( 'RENAME TABLE ' + mask(NodeData.Captions[0]) + ' TO ' + mask(NewText), False, False );

    if SynSQLSyn1.TableNames.IndexOf( NewText ) = -1 then begin
      SynSQLSyn1.TableNames.Add(NewText);
    end;
    // Update nodedata
    NodeData.Captions[0] := NewText;
    // Now the active tree db has to be updated. But calling RefreshTreeDB here causes an AV
    // so we do it manually here
    RefreshActiveDbTableList;
    DBTree.InvalidateChildren(FindDBNode(ActiveDatabase), True);
  except
    On E : Exception do
    begin
      MessageDlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;
end;


procedure TMainForm.TimerConnectedTimer(Sender: TObject);
begin
  if not TimerConnected.Enabled then begin
    showstatus('Disconnected.', 2);
    exit;
  end;

  inc(time_connected);

  // calculate and display connection-time
  showstatus( 'Connected: ' + FormatTimeNumber(time_connected), 2 );
end;


procedure TMainForm.Clear2Click(Sender: TObject);
begin
  // clear history-memo
  Screen.Cursor := crHourglass;
  SynMemoSQLLog.Lines.Clear;
  Screen.Cursor := crDefault;
end;


{**
  Column-title clicked -> generate "ORDER BY"
}
procedure TMainForm.QuickFilterClick(Sender: TObject);
var
  filter,value,column : WideString;
  menuitem : TMenuItem;
  IsNull: Boolean;
begin
  // Set filter for "where..."-clause
  value := DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn];
  menuitem := (Sender as TMenuItem);
  column := mask(DataGrid.Header.Columns[DataGrid.FocusedColumn].Text);
  IsNull := DataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].IsNull;
  if (menuitem = QF1) and IsNull then
    filter := column + ' IS NULL'
  else if menuitem = QF1 then
    filter := column + ' =' + ' ' + esc( value )
  else if (menuitem = QF2) and IsNull then
    filter := column + ' IS NOT NULL'
  else if menuitem = QF2 then
    filter := column + ' !=' + ' ' + esc( value )
  else if menuitem = QF3 then
    filter := column + ' >' + ' ' + esc( value )
  else if menuitem = QF4 then
    filter := column + ' <' + ' ' + esc( value )
  else if menuitem = QF5 then
    filter := column + ' LIKE' + ' ''' + esc( value, true ) + '%'''
  else if menuitem = QF6 then
    filter := column + ' LIKE' + ' ''%' + esc( value, true ) + ''''
  else if menuitem = QF7 then
    filter := column + ' LIKE' + ' ''%' + esc( value, true ) + '%'''

  else if menuitem = QF8 then
  begin
    filter := InputBox('Specify filter-value...', column+' = ', 'Value');
    if filter = 'Value' then
      abort;
    filter := column + ' = ''' + filter + '''';
  end

  else if menuitem = QF9 then
  begin
    filter := InputBox('Specify filter-value...', column+' != ', 'Value');
    if filter = 'Value' then
      abort;
    filter := column + ' != ''' + filter + '''';
  end

  else if menuitem = QF10 then
  begin
    filter := InputBox('Specify filter-value...', column+' > ', 'Value');
    if filter = 'Value' then
      abort;
    filter := column + ' > ''' + filter + '''';
  end

  else if menuitem = QF11 then
  begin
    filter := InputBox('Specify filter-value...', column+' < ', 'Value');
    if filter = 'Value' then
      abort;
    filter := column + ' < ''' + filter + '''';
  end

  else if menuitem = QF12 then
  begin
    filter := InputBox('Specify filter-value...', column+' LIKE ', 'Value');
    if filter = 'Value' then
      abort;
    filter := column + ' LIKE ''%' + filter + '%''';
  end

  else if menuitem = QF13 then
    filter := column + ' IS NULL'

  else if menuitem = QF14 then
    filter := column + ' IS NOT NULL'

  // Filters with text from clipboard
  else if (menuitem = QF15) or (menuitem = QF16) or (menuitem = QF17) or (menuitem = QF18) or (menuitem = QF19) then
  begin
    filter := menuitem.Caption;
  end;

  SynMemoFilter.UndoList.AddGroupBreak;
  SynMemoFilter.SelectAll;
  SynmemoFilter.SelText := filter;
  ToggleFilterPanel(True);
  actApplyFilterExecute(Sender);
end;


procedure TMainForm.popupQueryPopup(Sender: TObject);
begin
  // Sets cursor into memo and activates TAction(s) like paste
  ActiveQueryMemo.SetFocus;
end;

procedure TMainForm.popupResultGridPopup(Sender: TObject);
begin
  // data available?
  // Save2CSV.enabled :=
end;

procedure TMainForm.AutoRefreshSetInterval(Sender: TObject);
var
  seconds : String;
  secondsInt : Integer;
begin
  // set interval for autorefresh-timer
  seconds := IntToStr(TimerRefresh.interval div 1000);
  if inputquery('Auto refresh','Refresh list every ... second(s):', seconds) then begin
    secondsInt := StrToIntDef(seconds, 0);
    if secondsInt > 0 then begin
      TimerRefresh.Interval := secondsInt * 1000;
      TimerRefresh.Enabled := true;
      menuAutoRefresh.Checked := true;
    end
    else
      MessageDLG('Seconds must be between 1 and ' + IntToStr(maxint) + '.', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.AutoRefreshToggle(Sender: TObject);
begin
  // enable autorefresh-timer
  TimerRefresh.Enabled := not TimerRefresh.Enabled;
  menuAutoRefresh.Checked := TimerRefresh.Enabled;
end;

procedure TMainForm.SynMemoQueryDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  src : TControl;
  Memo: TSynMemo;
begin
  // dragging an object over the query-memo
  Memo := ActiveQueryMemo;
  src := Source as TControl;
  // Accepting drag's from DBTree and QueryHelpers
  Accept := (src = DBtree) or (src = ActiveQueryHelpers);
  // set x-position of cursor
  Memo.CaretX := (x - Memo.Gutter.Width) div Memo.CharWidth - 1 + Memo.LeftChar;
  // set y-position of cursor
  Memo.CaretY := y div Memo.LineHeight + Memo.TopLine;
  if not Memo.Focused then
    Memo.SetFocus;
end;


procedure TMainForm.SynMemoQueryDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  src : TControl;
  Text : WideString;
  LoadText : Boolean;
  i: Integer;
begin
  // dropping a tree node or listbox item into the query-memo
  ActiveQueryMemo.UndoList.AddGroupBreak;
  src := Source as TControl;
  Text := 'Error: Unspecified source control in drag''n drop operation!';
  LoadText := True;
  // Check for allowed controls as source has already
  // been performed in OnDragOver. So, only do typecasting here.
  if src = DBtree then
    Text := DBtree.Text[DBtree.GetFirstSelected, 0]
  else if (src = ActiveQueryHelpers) and (ActiveQueryHelpers.ItemIndex > -1) then begin
    // Snippets tab
    if tabsetQueryHelpers.TabIndex = 3 then begin
      QueryLoad( DIRNAME_SNIPPETS + ActiveQueryHelpers.Items[ActiveQueryHelpers.ItemIndex] + '.sql', False );
      LoadText := False;
    // All other tabs
    end else begin
      Text := '';
      for i := 0 to ActiveQueryHelpers.Items.Count - 1 do begin
        if ActiveQueryHelpers.Selected[i] then
          Text := Text + ActiveQueryHelpers.Items[i] + ', ';
      end;
      Delete(Text, Length(Text)-1, 2);
    end;
  end;
  // Only insert text if no previous action did the job.
  // Should be false when dropping a snippet-file here
  if LoadText then
    ActiveQueryMemo.SelText := Text;
  ActiveQueryMemo.UndoList.AddGroupBreak;
end;



procedure TMainForm.SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TUnicodeStrings);
var
  i        : Integer;
begin
  // one or more files from explorer or somewhere else was
  // dropped onto the query-memo - let's load their contents:
  for i:=0 to AFiles.Count-1 do
  begin
    if fileExists(AFiles[i]) then
    begin
      QueryLoad( AFiles[i], false );
    end;
  end;
end;


procedure TMainForm.popupHostPopup(Sender: TObject);
begin
  Kill1.Enabled := (PageControlHost.ActivePage = tabProcessList) and Assigned(ListProcesses.FocusedNode);
  menuEditVariable.Enabled := False;
  if mysql_version >= 40003 then
    menuEditVariable.Enabled := (PageControlHost.ActivePage = tabVariables) and Assigned(ListVariables.FocusedNode)
  else
    menuEditVariable.Hint := STR_NOTSUPPORTED;
end;

procedure TMainForm.Saveastextfile1Click(Sender: TObject);
begin
  with TSaveDialog.Create(self) do begin
    Filter := 'Textfiles (*.txt)|*.txt|All Files (*.*)|*.*';
    DefaultExt := 'txt';
    FilterIndex := 1;
    Options := [ofOverwritePrompt,ofHideReadOnly,ofEnableSizing];
    if Execute then begin
      Screen.Cursor := crHourglass;
      SynMemoSQLLog.Lines.SaveToFile(Filename);
      Screen.Cursor := crdefault;
    end;
  end;
end;

procedure TMainForm.popupDBPopup(Sender: TObject);
var
  L: Cardinal;
  HasFocus, InDBTree: Boolean;
  NodeData: PVTreeData;
begin
  // DBtree and ListTables both use popupDB as menu. Find out which of them was rightclicked.
  if Sender is TPopupMenu then
    InDBTree := (Sender as TPopupMenu).PopupComponent = DBTree
  else if Sender is TMenuItem then
    InDBTree := TPopupMenu((Sender as TMenuItem).GetParentMenu).PopupComponent = DBTree
  else
    InDBTree := False;

  if InDBtree then begin
    HasFocus := Assigned(DBtree.FocusedNode);
    if HasFocus then
      L := DBtree.GetNodeLevel(DBtree.FocusedNode)
    else
      L := 0;
    actCreateDatabase.Enabled := L = 0;
    actCreateTable.Enabled := L in [1,2];
    actCreateView.Enabled := L in [1,2];
    actCreateRoutine.Enabled := L in [1,2];
    actDropObjects.Enabled := L in [1,2];
    actCopyTable.Enabled := HasFocus and (GetFocusedTreeNodeType in [lntTable, lntCrashedTable, lntView]);
    actEmptyTables.Enabled := HasFocus and (GetFocusedTreeNodeType in [lntTable, lntCrashedTable, lntView]);
    actEditObject.Enabled := L > 0;
    // Show certain items which are valid only here
    menuTreeExpandAll.Visible := True;
    menuTreeCollapseAll.Visible := True;
    menuShowSizeColumn.Visible := True;
    actSelectTreeBackground.Visible := True;
  end else begin
    HasFocus := Assigned(ListTables.FocusedNode);
    actCreateDatabase.Enabled := False;
    actCreateTable.Enabled := True;
    actCreateView.Enabled := True;
    actCreateRoutine.Enabled := True;
    actDropObjects.Enabled := ListTables.SelectedCount > 0;
    actEmptyTables.Enabled := False;
    if HasFocus then begin
      NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
      actEmptyTables.Enabled := NodeData.NodeType in [lntTable, lntCrashedTable, lntView];
    end;
    actEditObject.Enabled := HasFocus;
    // Show certain items which are valid only here
    actCopyTable.Enabled := actEmptyTables.Enabled;
    menuTreeExpandAll.Visible := False;
    menuTreeCollapseAll.Visible := False;
    menuShowSizeColumn.Visible := False;
    actSelectTreeBackground.Visible := False;
  end;
  actCreateView.Enabled := actCreateView.Enabled and (mysql_version >= 50001);
  actCreateRoutine.Enabled := actCreateRoutine.Enabled and (mysql_version >= 50003);
end;




procedure TMainForm.QueryLoad( filename: String; ReplaceContent: Boolean = true );

var
  filecontent      : WideString;
  msgtext          : String;
  LineBreaks       : TLineBreaks;
begin
  // Ask for action when loading a big file
  if FileExists(filename) and (_GetFileSize( filename ) > 5*SIZE_MB) then
  begin
    msgtext := 'The file you are about to load is bigger than '+FormatByteNumber(5*SIZE_MB, 0)+'.' + CRLF + CRLF +
      'Do you want to just run the file to avoid loading it completely into the query-editor ( = memory ) ?' + CRLF + CRLF +
      'Press' + CRLF +
      '  [Yes] to run the file without loading it into the editor' + CRLF +
      '  [No] to load the file into the query editor' + CRLF +
      '  [Cancel] to cancel file opening.';
    case MessageDlg( msgtext, mtWarning, [mbYes, mbNo, mbCancel], 0 ) of
      // Run the file, don't load it into the editor
      mrYes:
        begin
          RunSQLFileWindow( Self, filename );
          // Add filename to history menu
          if Pos( DIRNAME_SNIPPETS, filename ) = 0 then
            AddOrRemoveFromQueryLoadHistory( filename, true );
          // Don't load into editor
          Abort;
        end;
      // Do nothing here, go ahead and load the file normally into the editor
      mrNo:;
      // Cancel opening file
      mrCancel: Abort;
    end;
  end;

  // Load file and add that to the undo-history of SynEdit.
  // Normally we would do a simple SynMemo.Lines.LoadFromFile but
  // this would prevent SynEdit from adding this step to the undo-history
  // so we have to do it by replacing the SelText property
  Screen.Cursor := crHourGlass;
  try
    filecontent := ReadTextfile(filename);
    if Pos( DIRNAME_SNIPPETS, filename ) = 0 then
      AddOrRemoveFromQueryLoadHistory( filename, true );
    FillPopupQueryLoad;
    if not QueryTabActive then
      PagecontrolMain.ActivePage := tabQuery;
    ActiveQueryMemo.UndoList.AddGroupBreak;

    if ScanNulChar(filecontent) then begin
      filecontent := RemoveNulChars(filecontent);
      MessageDlg(SContainsNulCharFile, mtInformation, [mbOK], 0);
    end;

    ActiveQueryMemo.BeginUpdate;
    LineBreaks := ScanLineBreaks(filecontent);
    if ReplaceContent then begin
      ActiveQueryMemo.SelectAll;
      QueryMemoLineBreaks := LineBreaks;
    end else begin
      if (QueryMemoLineBreaks <> lbsNone) and (QueryMemoLineBreaks <> LineBreaks) then
        QueryMemoLineBreaks := lbsMixed
      else
        QueryMemoLineBreaks := LineBreaks;
    end;
    if QueryMemoLineBreaks = lbsMixed then
      MessageDlg('This file contains mixed linebreaks. They have been converted to Windows linebreaks (CR+LF).', mtInformation, [mbOK], 0);

    ActiveQueryMemo.SelText := filecontent;
    ActiveQueryMemo.SelStart := ActiveQueryMemo.SelEnd;
    ActiveQueryMemo.EndUpdate;
  except on E:Exception do
    // File does not exist, is locked or broken
    MessageDlg(E.message, mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;
end;




procedure TMainForm.SaveDialogExportDataTypeChange(Sender: TObject);
begin
  // Set default file-extension of saved file and options on the dialog to show
  with SaveDialogExportData do begin
    Case FilterIndex of
      1: DefaultExt := 'csv';
      2: DefaultExt := 'html';
      3: DefaultExt := 'xml';
      4: DefaultExt := 'sql';
    end;
  end;
end;


{**
  A cell in a DBGrid is painted. Sets custom background color NULL fields.
}
procedure TMainForm.popupDataGridPopup(Sender: TObject);
var
  y,m,d,h,i,s,ms : Word;
  cpText, selectedColumn, value : String;
  CellFocused: Boolean;
const
  CLPBRD : String = 'CLIPBOARD';
begin
  CellFocused := Assigned(DataGrid.FocusedNode) and (DataGrid.FocusedColumn > NoColumn);
  DataInsertDateTime.Enabled := CellFocused;
  if not CellFocused then
    Exit;

  decodedate(now, y, m, d);
  decodetime(now, h, i, s, ms);
  DataDateTime.Caption := Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,i,s]);
  DataDate.Caption := Format('%.4d-%.2d-%.2d', [y,m,d]);
  DataTime.Caption := Format('%.2d:%.2d:%.2d', [h,i,s]);
  DataTimestamp.caption := Format('%.4d%.2d%.2d%.2d%.2d%.2d', [y,m,d,h,i,s]);
  DataYear.Caption := Format('%.4d', [y]);

  // Manipulate the Quick-filter menuitems
  selectedColumn := mask(DataGrid.Header.Columns[DataGrid.FocusedColumn].Text);
  // 1. block: include selected columnname and value from datagrid in caption
  if DataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].IsNull then begin
    QF1.Caption := selectedColumn + ' IS NULL';
    QF2.Caption := selectedColumn + ' IS NOT NULL';
    QF3.Visible := False;
    QF4.Visible := False;
    QF5.Visible := False;
    QF6.Visible := False;
    QF7.Visible := False;
  end else begin
    value := sstr(DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn], 100);
    QF1.Caption := selectedColumn + ' = ' + esc( value );
    QF2.Caption := selectedColumn + ' != ' + esc( value );
    QF3.Caption := selectedColumn + ' > ' + esc( value );
    QF4.Caption := selectedColumn + ' < ' + esc( value );
    QF5.Caption := selectedColumn + ' LIKE ''' + esc( value, true ) + '%''';
    QF6.Caption := selectedColumn + ' LIKE ''%' + esc( value, true ) + '''';
    QF7.Caption := selectedColumn + ' LIKE ''%' + esc( value, true ) + '%''';
    QF3.Visible := True;
    QF4.Visible := True;
    QF5.Visible := True;
    QF6.Visible := True;
    QF7.Visible := True;
  end;

  // 2. block: include only selected columnname in caption
  QF8.Caption := selectedColumn + ' = "..."';
  QF9.Caption := selectedColumn + ' != "..."';
  QF10.Caption := selectedColumn + ' > "..."';
  QF11.Caption := selectedColumn + ' < "..."';
  QF12.Caption := selectedColumn + ' LIKE "%...%"';
  QF13.Caption := selectedColumn + ' IS NULL';
  QF14.Caption := selectedColumn + ' IS NOT NULL';

  // 3. block: include selected columnname and clipboard-content in caption for one-click-filtering
  cpText := Clipboard.AsText;
  if Length(cpText) < 100 then begin
    QF15.Enabled := true; QF15.Caption := selectedColumn + ' = ' + esc( cpText );
    QF16.Enabled := true; QF16.Caption := selectedColumn + ' != ' + esc( cpText );
    QF17.Enabled := true; QF17.Caption := selectedColumn + ' > ' + esc( cpText );
    QF18.Enabled := true; QF18.Caption := selectedColumn + ' < ' + esc( cpText );
    QF19.Enabled := true; QF19.Caption := selectedColumn + ' LIKE ''%' + esc( cpText, true ) + '%''';
  end else begin
    QF15.Enabled := false; QF15.Caption := selectedColumn + ' = ' + CLPBRD;
    QF16.Enabled := false; QF16.Caption := selectedColumn + ' != ' + CLPBRD;
    QF17.Enabled := false; QF17.Caption := selectedColumn + ' > ' + CLPBRD;
    QF18.Enabled := false; QF18.Caption := selectedColumn + ' < ' + CLPBRD;
    QF19.Enabled := false; QF19.Caption := selectedColumn + ' LIKE %' + CLPBRD + '%';
  end;
end;

procedure TMainForm.InsertDate(Sender: TObject);
var d : String;
begin
  // Insert date/time-value into table
  d := (sender as TMenuItem).Caption;
  delete(d, Pos('&', d), 1);
  DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn] := d;
end;


procedure TMainForm.ExecUseQuery(db: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false);
begin
  ExecUpdateQuery('USE ' + mask(db), HandleErrors, DisplayErrors);
  FConn.MysqlParams.Database := db;
end;


{***
  Execute a query without returning a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
}
function TMainForm.ExecUpdateQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): Int64;
var
  MysqlQuery : TMysqlQuery;
  ds: TDataSet;
begin
  Result := -1; // Silence compiler warning.
  MysqlQuery := nil;
  try
    try
      // Start query execution
      MysqlQuery := RunThreadedQuery(sql, false);
      Result := FMysqlConn.Connection.GetAffectedRowsFromLastPost;
      // Inspect query result code and log / notify user on failure
      if MysqlQuery.Result in [MQR_CONNECT_FAIL,MQR_QUERY_FAIL] then
      begin
        raise Exception.Create(MysqlQuery.Comment);
      end;
    except
      on E: Exception do begin
        LogSQL( E.Message, True );
        if DisplayErrors then MessageDlg( E.Message, mtError, [mbOK], 0 );
        // Recreate exception, since we free it below the caller
        // won't know what happened otherwise.
        if not HandleErrors then raise THandledSQLError.Create(MysqlQuery.Comment);
        Result := -1;
      end;
    end;
  finally
    // Cleanup the MysqlQuery object, we won't need it anymore
    if MysqlQuery <> nil then begin
    if MysqlQuery.MysqlDataset <> nil then
      MysqlQuery.MysqlDataset.Close;
      ds := MysqlQuery.MysqlDataset;
      FreeAndNil(ds);
    end;
    FreeAndNil (MysqlQuery);
  end;
end;


{***
  Execute a query which may return a resultset. The caller is responsible for
  freeing the MysqlQuery object and its Dataset member, only on returnvalue True.
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
  @return TMysqlQuery Containing the dataset and info data availability
}
function TMainForm.ExecSelectQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false; ForceDialog: Boolean = false): TDataSet;
var
  res: TMysqlQuery;
begin
  res := nil;
  result := nil;
  try
    try
      // Start query execution
      res := RunThreadedQuery(sql, ForceDialog);
      result := res.MysqlDataset;
      // Inspect query result code and log / notify user on failure
      if res.Result in [MQR_CONNECT_FAIL,MQR_QUERY_FAIL] then
      begin
        raise Exception.Create(res.Comment);
      end;
    except
      on E: Exception do begin
        LogSQL( E.Message, True );
        if DisplayErrors then MessageDlg( E.Message, mtError, [mbOK], 0 );
        if not HandleErrors then raise THandledSQLError.Create(E.Message);
        Result := nil;
      end;
    end;
  finally
    FreeAndNil(res);
  end;
end;


{***
  Executes a query.
}
function TMainForm.GetResults( SQLQuery: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false; ForceDialog: Boolean = false): TDataSet;
begin
  result := ExecSelectQuery(SQLQuery, HandleErrors, DisplayErrors, ForceDialog);
end;


{***
  Execute a query and return String from column x
}
function TMainForm.GetVar( SQLQuery: WideString; x: Integer = 0; HandleErrors: Boolean = false; DisplayErrors: Boolean = false) : WideString;
var
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors );
  if ds = nil then exit;
  Result := ds.Fields[x].AsWideString;
  ds.Close;
  FreeAndNil(ds);
end;


function TMainForm.GetNamedVar( SQLQuery: WideString; x: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false) : WideString;
var
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors );
  if ds = nil then exit;
  Result := ds.Fields.FieldByName(x).AsWideString;
  ds.Close;
  FreeAndNil(ds);
end;


{***
  Execute a query and return column x as Stringlist

  @param  String SQL query String
  @param  Integer 0-based column index in the resultset to return
  @return TStringList
}
function TMainForm.GetCol( SQLQuery: WideString; x: Integer = 0; HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : WideStrings.TWideStringList;
var
  i: Integer;
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors);
  Result := WideStrings.TWideStringList.Create;
  if ds = nil then exit;
  for i := 0 to ds.RecordCount - 1 do
  begin
    Result.Add( ds.Fields[x].AsWideString );
    ds.Next;
  end;
  ds.Close;
  FreeAndNil(ds);
end;


{***
  Event procedure handler for the ZSQLMonitor1 object
}
procedure TMainForm.ZSQLMonitor1LogTrace(Sender: TObject;
  Event: TZLoggingEvent);
begin
  LogSQL( Event.Message, (Event.Category <> lcExecute) );
end;


procedure TMainForm.RunAsyncPost(ds: TDeferDataSet);
var
  res: TMysqlQuery;
begin
  FQueryRunning := true;
  try
    try
      CheckConnection;
    except
      on E: Exception do begin
        raise Exception.Create('Failed to reconnect, giving up. (' + E.Message + ')');
      end;
    end;
    FProgressForm := TFrmQueryProgress.Create(Self);
    debug('RunThreadedQuery(): Launching asynchronous query.');
    res := ExecPostAsync(FConn,FProgressForm.Handle,ds);
    WaitForQueryCompletion(FProgressForm, res, false);
    if res.Result in [MQR_CONNECT_FAIL,MQR_QUERY_FAIL] then
    begin
      raise Exception.Create(res.Comment);
    end;
  finally
    FQueryRunning := false;
  end;
end;

{***
  Run a query in a separate thread of execution on the current connection.
}
function TMainForm.RunThreadedQuery(AQuery: WideString; ForceDialog: Boolean): TMysqlQuery;
begin
  Result := nil;
  if (Copy(AQuery, 1, 3) <> 'USE') then EnsureDatabase;
  // Indicate a querythread is active (only one thread allow at this moment)
  FQueryRunning := true;
  try
    // Check if the connection of the current window is still alive
    // Otherwise reconnect
    try
      CheckConnection;
    except
      on E: Exception do begin
        // Ensure auto-updating processlist is disabled, see bug #1865305
        if TimerRefresh.Enabled then
          AutoRefreshToggle(nil);
        Screen.Cursor := crDefault;
        raise Exception.Create('Failed to reconnect, giving up. (' + E.Message + ')');
      end;
    end;

    // Create instance of the progress form (but don't show it yet)
    FProgressForm := TFrmQueryProgress.Create(Self);

    { Launch a thread of execution that passes the query to the server

      The progressform serves as receiver of the status
      messages (WM_MYSQL_THREAD_NOTIFY) of the thread:

      * After the thread starts it notifies the progressform (MQE_INITED)
        (which calls ShowModal on itself)
      * Waits for a completion message from the thread (MQE_FINISHED) to remove itself
      * Set FQueryRunning to false
    }
    debug('RunThreadedQuery(): Launching asynchronous query.');
    Result := ExecMysqlStatementAsync (AQuery,FConn,FProgressForm.Handle,RunAsyncPost);

    { Repeatedly check if the query has finished by inspecting FQueryRunning
      Allow repainting of user interface
    }
    WaitForQueryCompletion(FProgressForm, Result, ForceDialog);
  finally
    FQueryRunning := false;
  end;
  // Hack: Un-prevent dynamic loading of records in the context of the wait form's message loop.
  if not DataGrid.Visible then DataGrid.Visible := True;
end;


procedure TMainForm.CancelQuery;
begin
  cancelling := true;
  MysqlConn.Connection.CancelQuery;
end;


// Searchbox unfocused
procedure TMainForm.CheckConnection;
var
  connected: Boolean;
  choice: Integer;
begin
  if not FMysqlConn.IsAlive then begin
    LogSQL('Connection failure detected. Trying to reconnect.', true);
    TimerConnected.Enabled := false;
    TimerConnectedTimer(self);
    TimerHostUptime.Enabled := false;
    TimerHostUptimeTimer(self);
    FQueryRunning := false;
    try
      FMysqlConn.Connection.Disconnect;
      connected := True;
      try
        // CheckConnected() doesn't really check anything, it
        // just sees if the driver has disposed of it's connection
        // by means of a Disconnect() or not.  In which case there
        // is no point in doing a Reconnect(), it will NOP.
        FMysqlConn.Connection.CheckConnected;
      except
        connected := False;
      end;
      while not FMysqlConn.IsAlive do begin
        try
          if connected then FMysqlConn.Connection.Reconnect
          else FMysqlConn.Connection.Connect;
        except
          on E: Exception do begin
            MainForm.Visible := False;
            choice := MessageDlg(
              'Connection to the server has been lost.'#10#10 +
              E.Message + #10#10 +
              'Click Abort to exit this session.',
              mtError,
              [mbRetry, mbAbort], 0
            );
            if choice = mrAbort then begin
              Close;
              Halt(1);
            end;
          end;
        end;
        if FMysqlConn.IsAlive then MainForm.Visible := True;
      end;

      time_connected := 0;
      TimerConnected.Enabled := true;
      LogSQL('Connected. Thread-ID: ' + IntToStr( MySQLConn.Connection.GetThreadId ));
      CheckUptime;
      // Try to restore active database
      if ActiveDatabase <> '' then
        ExecUseQuery(ActiveDatabase)
    finally
      FQueryRunning := true;
    end;
  end;
end;


function TMainForm.GetActiveDatabase: WideString;
var
  s: PVirtualNode;
begin
  // Find currently selected database node in database tree,
  // or the parent if a table is currently selected.
  s := DBtree.GetFirstSelected;
  if not Assigned(s) then Result := ''
  else case DBtree.GetNodeLevel(s) of
    2: Result := Databases[s.Parent.Index];
    1: Result := Databases[s.Index];
    else Result := '';
  end;
end;


function TMainForm.GetSelectedTable: TListNode;
begin
  if Assigned(DBtree.FocusedNode) and (DBtree.GetNodeLevel(DBtree.FocusedNode)=2) then begin
    Result.Text := DBtree.Text[DBtree.FocusedNode, 0];
    Result.NodeType := GetFocusedTreeNodeType;
  end else begin
    Result.Text := '';
    Result.NodeType := lntNone;
  end;
end;


function TMainForm.GetTreeNodeType(Node: PVirtualNode): TListNodeType;
var
  ds: TDataset;
begin
  Result := lntNone;
  if Assigned(Node) then case DBtree.GetNodeLevel(Node) of
    1: Result := lntDb;
    2: begin
      ds := FetchDbTableList(DBTree.Text[Node.Parent, 0]);
      ds.RecNo := Node.Index+1;
      Result := GetDBObjectType(ds.Fields);
    end;
  end;
end;

function TMainForm.GetFocusedTreeNodeType: TListNodeType;
begin
  Result := GetTreeNodeType(DBtree.FocusedNode);
end;


procedure TMainForm.SelectDBObject(Text: WideString; NodeType: TListNodeType);
var
  i: integer;
  dbnode, tnode, snode: PVirtualNode;
begin
  debug('SelectDBObject()');
  // Detect db node
  case DBtree.GetNodeLevel( DBtree.GetFirstSelected ) of
    1: dbnode := DBtree.GetFirstSelected;
    2: dbnode := DBtree.GetFirstSelected.Parent;
    else raise Exception.Create('No selection in tree, could not determine active db.');
  end;
  snode := nil;
  // 1st search, case sensitive for lower-case-tablenames=0 servers
  tnode := DBtree.GetFirstChild(dbnode);
  for i := 0 to dbnode.ChildCount - 1 do begin
    // Select table node if it has the wanted caption
    if (DBtree.Text[tnode, 0] = Text) and (GetTreeNodeType(tnode) = NodeType) then begin
      snode := tnode;
      break;
    end;
    tnode := DBtree.GetNext(tnode);
  end;
  // 2nd search, case insensitive now
  if not Assigned(snode) then begin
    tnode := DBtree.GetFirstChild(dbnode);
    for i := 0 to dbnode.ChildCount - 1 do begin
      // Select table node if it has the wanted caption
      if (AnsiCompareText(DBtree.Text[tnode, 0], Text) = 0) and (GetTreeNodeType(tnode) = NodeType) then begin
        snode := tnode;
        break;
      end;
      tnode := DBtree.GetNext(tnode);
    end;
  end;
  if Assigned(snode) then begin
    // Ensure table node will be visible
    DBTree.ScrollIntoView(snode, False);
    DBtree.Expanded[dbnode] := True;
    DBtree.Selected[snode] := True;
    // Implicitely calls OnFocusChanged:
    DBTree.FocusedNode := snode;
    exit;
  end;
  raise Exception.Create('Table node ' + Text + ' not found in tree.');
end;


procedure TMainForm.SetSelectedDatabase(db: WideString);
var
  n: PVirtualNode;
begin
  n := FindDBNode(db);
  if Assigned(n) then begin
    DBtree.Selected[n] := true;
    DBtree.FocusedNode := n;
  end else
    raise Exception.Create('Database node ' + db + ' not found in tree.');
end;


{**
  Column selection for datagrid
}
procedure TMainForm.btnDataClick(Sender: TObject);
var
  btn : TToolButton;
  frm : TForm;
begin
  btn := (Sender as TToolButton);

  if (btn = tbtnDataColumns) or (btn = tbtnDataSorting) then begin
    // Create desired form for SELECT and ORDER buttons
    btn.Down := not btn.Down;
    if not btn.Down then Exit;
    if btn = tbtnDataColumns then
      frm := TColumnSelectionForm.Create(self)
    else if btn = tbtnDataSorting then
      frm := TDataSortingForm.Create(self)
    else
      frm := TForm.Create(self); // Dummy fallback, should never get created
    // Position new form relative to btn's position
    frm.Top := btn.ClientOrigin.Y + btn.Height;
    frm.Left := btn.ClientOrigin.X + btn.Width - frm.Width;
    // Display form
    frm.Show;
  end else if btn = tbtnDataFilter then begin
    // Unhide inline filter panel
    ToggleFilterPanel;
    FilterPanelManuallyOpened := pnlFilter.Visible;
  end;
end;


{**
  Tabset right to query-memo was clicked
}
procedure TMainForm.tabsetQueryHelpersChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  i, idx : Integer;
  SnippetsAccessible : Boolean;
  Files: TStringList;
begin
  ActiveQueryHelpers.Items.BeginUpdate;
  ActiveQueryHelpers.Items.Clear;
  // By default sorted alpabetically
  ActiveQueryHelpers.Sorted := True;
  // By default disable all items in popupmenu, enable them when needed
  menuInsertSnippetAtCursor.Enabled := False;
  menuLoadSnippet.Enabled := False;
  menuDeleteSnippet.Enabled := False;
  menuExplore.Enabled := False;
  menuHelp.Enabled := False;
  ActiveQueryHelpers.MultiSelect := True;

  case NewTab of
    0: // Cols
    begin
      // Keep native order of columns
      ActiveQueryHelpers.Sorted := False;
      if (SelectedTable.Text <> '') and Assigned(SelectedTableColumns) then begin
        SelectedTableColumns.First;
        while not SelectedTableColumns.Eof do begin
          ActiveQueryHelpers.Items.Add(SelectedTableColumns.Fields[0].AsWideString);
          SelectedTableColumns.Next;
        end;
      end;
    end;

    1: // SQL functions
    begin
      // State of items in popupmenu
      menuHelp.Enabled := True;
      for i := 0 to Length(MySQLFunctions) - 1 do
      begin
        // Don't display unsupported functions here
        if MySqlFunctions[i].Version > mysql_version then
          continue;
        ActiveQueryHelpers.Items.Add( MySQLFunctions[i].Name + MySQLFunctions[i].Declaration );
      end;
    end;

    2: // SQL keywords
    begin
      // State of items in popupmenu
      menuHelp.Enabled := True;
      for i := 0 to MySQLKeywords.Count - 1 do
        ActiveQueryHelpers.Items.Add(MySQLKeywords[i]);
    end;

    3: // SQL Snippets
    begin
      ActiveQueryHelpers.MultiSelect := False;
      Files := getFilesFromDir( DIRNAME_SNIPPETS, '*.sql', true );
      for i := 0 to Files.Count - 1 do
        ActiveQueryHelpers.Items.Add(Files[i]);
	  Files.Free;
      // State of items in popupmenu
      SnippetsAccessible := ActiveQueryHelpers.Items.Count > 0;
      menuDeleteSnippet.Enabled := SnippetsAccessible;
      menuInsertSnippetAtCursor.Enabled := SnippetsAccessible;
      menuLoadSnippet.Enabled := SnippetsAccessible;
      menuExplore.Enabled := True;
    end;

  end;

  // Restore last selected item in tab
  for i := 0 to Length(QueryHelpersSelectedItems[NewTab]) - 1 do begin
    idx := QueryHelpersSelectedItems[NewTab][i];
    if idx < ActiveQueryHelpers.Count then
      ActiveQueryHelpers.Selected[idx] := True;
  end;

  ActiveQueryHelpers.Items.EndUpdate;
end;



{**
  Insert string from listbox with query helpers into SQL
  memo at doubleclick
}
procedure TMainForm.lboxQueryHelpersDblClick(Sender: TObject);
var
  text: WideString;
  i: Integer;
begin
  for i := 0 to ActiveQueryHelpers.Items.Count - 1 do begin
    if ActiveQueryHelpers.Selected[i] then
      text := text + ActiveQueryHelpers.Items[i] + ', ';
  end;
  Delete(text, Length(text)-1, 2);

  case ActiveQueryTabset.TabIndex of
    3: // Load snippet file ínto query-memo
      QueryLoad( DIRNAME_SNIPPETS + ActiveQueryHelpers.Items[ActiveQueryHelpers.ItemIndex] + '.sql', False );
    else // For all other tabs just insert the item from the list
      ActiveQueryMemo.SelText := text;
  end;

  ActiveQueryMemo.SetFocus;
end;


{**
  Remember last used items in query helper tabs
}
procedure TMainForm.lboxQueryHelpersClick(Sender: TObject);
var
  i, s, idx: Integer;
begin
  s := ActiveQueryTabset.TabIndex;
  SetLength(QueryHelpersSelectedItems[s], 0);
  for i := 0 to ActiveQueryHelpers.Count - 1 do if ActiveQueryHelpers.Selected[i] then begin
    idx := Length(QueryHelpersSelectedItems[s]);
    SetLength(QueryHelpersSelectedItems[s], idx+1);
    QueryHelpersSelectedItems[s][idx] := i;
  end;
end;


{**
  Insert function name from popupmenu to query memo
}
procedure TMainForm.insertFunction(Sender: TObject);
var
  f : String;
  sm : TSynMemo;
begin
  // Detect which memo is focused
  if SynMemoFilter.Focused then
    sm := SynMemoFilter
  else
    sm := ActiveQueryMemo;
  // Restore function name from array
  f := MySQLFunctions[TControl(Sender).tag].Name
    + MySQLFunctions[TControl(Sender).tag].Declaration;
  sm.UndoList.AddGroupBreak;
  sm.SelText := f;
  sm.UndoList.AddGroupBreak;
  if not SynMemoFilter.Focused then
    ValidateQueryControls(Sender);
end;


{**
  Delete a snippet file
}
procedure TMainForm.menuDeleteSnippetClick(Sender: TObject);
var
  snippetfile : String;
  mayChange : Boolean;
begin
  // Don't do anything if no item was selected
  if ActiveQueryHelpers.ItemIndex = -1 then
    abort;

  snippetfile := DIRNAME_SNIPPETS + ActiveQueryHelpers.Items[ ActiveQueryHelpers.ItemIndex ] + '.sql';
  if MessageDlg( 'Delete snippet file? ' + CRLF + snippetfile, mtConfirmation, [mbOk, mbCancel], 0) = mrOk then
  begin
    Screen.Cursor := crHourGlass;
    if DeleteFile( snippetfile ) then
    begin
      // Refresh list with snippets
      mayChange := True; // Unused; satisfies callee parameter collection which is probably dictated by tabset.
      tabsetQueryHelpersChange( Sender, ActiveQueryTabset.TabIndex, mayChange );
      FillPopupQueryLoad;
    end
    else
    begin
      Screen.Cursor := crDefault;
      MessageDlg( 'Failed deleting ' + snippetfile, mtError, [mbOK], 0 );
    end;
    Screen.Cursor := crDefault;
  end;
end;


{**
  Load snippet at cursor
}
procedure TMainForm.menuInsertSnippetAtCursorClick(Sender: TObject);
begin
  QueryLoad( DIRNAME_SNIPPETS + ActiveQueryHelpers.Items[ActiveQueryHelpers.ItemIndex] + '.sql', False );
end;


{**
  Load snippet and replace content
}
procedure TMainForm.menuLoadSnippetClick(Sender: TObject);
begin
  QueryLoad( DIRNAME_SNIPPETS + ActiveQueryHelpers.Items[ActiveQueryHelpers.ItemIndex] + '.sql', True );
end;


{**
  Open snippets-directory in Explorer
}
procedure TMainForm.menuExploreClick(Sender: TObject);
begin
  // Normally the snippets folder is created at installation. But it sure
  // can be the case that it has been deleted or that the application was
  // not installed properly. Ask if we should create the folder now.
  if DirectoryExists( DIRNAME_SNIPPETS ) then
    ShellExec( '', DIRNAME_SNIPPETS )
  else
    if MessageDlg( 'Snippets folder does not exist: ' + DIRNAME_SNIPPETS + CRLF + CRLF + 'This folder is normally created when you install '+appname+'.' + CRLF + CRLF + 'Shall it be created now?',
      mtWarning, [mbYes, mbNo], 0 ) = mrYes then
    try
      Screen.Cursor := crHourglass;
      ForceDirectories( DIRNAME_SNIPPETS );
    finally
      Screen.Cursor := crDefault;
    end;
end;


{**
  Tell a VirtualStringTree the mem size to allocate per node
}
procedure TMainForm.vstGetNodeDataSize(Sender: TBaseVirtualTree; var
    NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TVTreeData);
end;


{**
  Various lists initialize their nodes by calling the following procedure
  once per node
}
procedure TMainForm.vstInitNode(Sender: TBaseVirtualTree; ParentNode,
    Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  NodeData : PVTreeData;
  a : TVTreeDataArray;
begin
  // Get the pointer to the node data
  NodeData := Sender.GetNodeData(Node);
  // Fetch data array
  a := GetVTreeDataArray( Sender )^;
  // Bind data to node
  NodeData.Captions := a[Node.Index].Captions;
  NodeData.ImageIndex := a[Node.Index].ImageIndex;
  NodeData.NodeType := a[Node.Index].NodeType;
end;


{**
  Free data of a node
}
procedure TMainForm.vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  b : PVTreeDataArray;
begin
  // Detect which global array should be processed
  b := GetVTreeDataArray( Sender );
  // TODO: If you optimize 'b' out of the code, the compiler will
  //       sometimes generate code that causes a new array here instead of
  //       a reference to the global array, thus breaking SetLength.  Find
  //       out why...
  //TestVTreeDataArray(b);
  if (Low(b^) < 0) or (High(b^) < 0) then raise Exception.Create('Internal error: unsupported array bounds.');
  if Node.Index + 1 < Cardinal(High(b^)) then
  begin
    // Delete node somewhere in the middle of the array
    // Taken from http://delphi.about.com/cs/adptips2004/a/bltip0204_2.htm
    System.Move(
      b^[Node.Index + 1],
      b^[Node.Index],
      (Cardinal(Length(b^)) - (Node.Index - Cardinal(Low(b^))) - 1) * SizeOf(TVTreeData)
    );
  end;
  SetLength(b^, Length(b^) - 1);
end;


{**
  A node in a VirtualStringTree gets visible and asks which text it shall display
}
procedure TMainForm.vstGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
var
  NodeData : PVTreeData;
  i : Integer;
begin
  // Get pointer to node which gets displayed
  NodeData := Sender.GetNodeData(Node);
  // Column is -1 if no column headers are defined
  if Column = -1 then
    i := 0
  else
    i := Column;
  // Avoid AV, don't exceed Captions content
  if NodeData.Captions.Count > i then
    CellText := NodeData.Captions[i]
  else
    CellText := '';
end;


{**
  A node in a VirtualStringTree gets visible and asks which icon it shall display
}
procedure TMainForm.vstGetImageIndex(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
    Boolean; var ImageIndex: Integer);
var
  NodeData : PVTreeData;
begin
  // Display icon only for leftmost cell (0) or for tree nodes (-1)
  if Column > 0 then
    exit;
  // Get pointer to node which gets displayed
  NodeData := Sender.GetNodeData(Node);
  ImageIndex := NodeData.ImageIndex;
end;


{**
  A column header of a VirtualStringTree was clicked:
  Toggle the sort direction
}
procedure TMainForm.vstHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  // Don't call sorting procedure on right click
  // Some list-headers have a contextmenu which should popup then.
  if HitInfo.Button = mbRight then
    Exit;
  // Beginning with VT's r181, this proc is also called when doubleclicking-to-autofit
  // Seems buggy in VT as this suddenly calls it with Column=-1 in those cases.
  // See also issue #1150
  if HitInfo.Column = NoColumn then
    Exit;

  if Sender.SortColumn <> HitInfo.Column then
    Sender.SortColumn := HitInfo.Column
  else if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  Sender.Treeview.SortTree( HitInfo.Column, Sender.SortDirection );
end;


{**
  Sorting a column of a VirtualTree by comparing two cells
}
procedure TMainForm.vstCompareNodes(Sender: TBaseVirtualTree; Node1,
    Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  NodeData1, NodeData2 : PVTreeData;
  CellText1, CellText2 : String;
  Number1, Number2 : Extended;
begin
  NodeData1 := Sender.GetNodeData(Node1);
  NodeData2 := Sender.GetNodeData(Node2);

  // If captions-item from either nodes is not set, assume empty string
  if NodeData1.Captions.Count >= Column then
    CellText1 := NodeData1.Captions[Column]
  else
    CellText1 := '';
  if NodeData2.Captions.Count >= Column then
    CellText2 := NodeData2.Captions[Column]
  else
    CellText2 := '';

  // Map value "0" to "N/A" strings
  if CellText1 = '' then
    CellText1 := '0';
  if CellText2 = '' then
    CellText2 := '0';

  // Apply different comparisons for numbers and text
  if StrToIntDef( copy(CellText1,0,1), -1 ) <> -1 then
  begin
    // Assuming numeric values
    Number1 := MakeFloat( CellText1 );
    Number2 := MakeFloat( CellText2 );
    if Number1 > Number2 then
      Result := 1
    else if Number1 = Number2 then
      Result := 0
    else if Number1 < Number2 then
      Result := -1;
  end
  else begin
    // Compare Strings
    Result := AnsiCompareText( CellText1, CellText2 );
  end;
end;


{**
  VirtualTree was painted. Adjust background color of sorted column.
}
procedure TMainForm.vstAfterPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
var
  i : Integer;
  h : TVTHeader;
begin
  h := (Sender as TVirtualStringTree).Header;
  for i := 0 to h.Columns.Count - 1 do
  begin
    if h.SortColumn = i then
    case h.SortDirection of
      sdAscending: h.Columns[i].Color := COLOR_SORTCOLUMN_ASC;
      sdDescending: h.Columns[i].Color := COLOR_SORTCOLUMN_DESC;
    end else
      h.Columns[i].Color := clWindow;
  end;
end;


{**
  Return the data array which belongs to a VirtualTree component
}
function TMainForm.GetVTreeDataArray( VT: TBaseVirtualTree ): PVTreeDataArray;
begin
  if VT = ListVariables then
    Result := @VTRowDataListVariables
  else if VT = ListStatus then
    Result := @VTRowDataListStatus
  else if VT = ListCommandStats then
    Result := @VTRowDataListCommandStats
  else if VT = ListProcesses then
    Result := @VTRowDataListProcesses
  else if VT = ListTables then
    Result := @VTRowDataListTables
  else begin
    raise Exception.Create( VT.ClassName + ' "' + VT.Name + '" doesn''t have an assigned array with data.' );
  end;
end;


{**
  Internal: Test quality of code/compiler.
}
procedure TMainForm.TestVTreeDataArray( P: PVTreeDataArray );
begin
  if P = @VTRowDataListVariables then Exit;
  if P = @VTRowDataListStatus then Exit;
  if P = @VTRowDataListCommandStats then Exit;
  if P = @VTRowDataListProcesses then Exit;
  if P = @VTRowDataListTables then Exit;
  raise Exception.Create('Assertion failed: Invalid global VT array.');
end;

{**
  Click on popupDBGridHeader
}
procedure TMainForm.MenuTablelistColumnsClick(Sender: TObject);
var
  menuitem : TMenuItem;
  VisibleColumns : WideStrings.TWideStringList;
  i : Integer;
begin
  VisibleColumns := WideStrings.TWideStringList.Create;
  menuitem := TMenuItem( Sender );
  menuitem.Checked := not menuitem.Checked;
  for i := 0 to ListTables.Header.Columns.Count - 1 do
  begin
    menuitem := popupDbGridHeader.Items[i];
    if menuitem.Checked then
      VisibleColumns.Add(IntToStr(i));
  end;
  SetVisibleListColumns( ListTables, VisibleColumns );
end;


{**
  Save setup of a VirtualStringTree to registry
}
procedure TMainForm.SaveListSetup( List: TVirtualStringTree );
var
  i : Byte;
  ColWidths, ColsVisible, ColPos, Regname: String;
begin
  ColWidths := '';
  ColsVisible := '';
  ColPos := '';
  for i := 0 to List.Header.Columns.Count - 1 do
  begin
    // Column widths
    if ColWidths <> '' then
      ColWidths := ColWidths + ',';
    ColWidths := ColWidths + IntToStr(List.Header.Columns[i].Width);

    // Column visibility
    if coVisible in List.Header.Columns[i].Options then
    begin
      if ColsVisible <> '' then
        ColsVisible := ColsVisible + ',';
      ColsVisible := ColsVisible + IntToStr(i);
    end;

    // Column position
    if ColPos <> '' then
      ColPos := ColPos + ',';
    ColPos := ColPos + IntToStr(List.Header.Columns[i].Position);

  end;
  OpenRegistry;
  Regname := List.Name;
  if GetParentForm(List) <> Self then
    Regname := GetParentForm(List).Name + '.' + Regname;
  MainReg.WriteString( REGPREFIX_COLWIDTHS + Regname, ColWidths );
  MainReg.WriteString( REGPREFIX_COLSVISIBLE + Regname, ColsVisible );
  MainReg.WriteString( REGPREFIX_COLPOS + Regname, ColPos );
end;


{**
  Restore setup of VirtualStringTree from registry
}
procedure TMainForm.RestoreListSetup( List: TVirtualStringTree );
var
  i : Byte;
  colwidth, colpos : Integer;
  Value : WideString;
  ValueList : WideStrings.TWideStringList;
  Regname: String;
  frm: TCustomForm;
begin
  ValueList := WideStrings.TWideStringList.Create;

  // Column widths
  Regname := List.Name;
  frm := GetParentForm(List);
  if (frm <> Self) and (Assigned(frm)) then
    Regname := frm.Name + '.' + Regname;
  Value := GetRegValue(REGPREFIX_COLWIDTHS + Regname, '');
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    for i := 0 to ValueList.Count - 1 do
    begin
      colwidth := MakeInt(ValueList[i]);
      // Check if column number exists and width is at least 1 pixel
      if (List.Header.Columns.Count > i) and (colwidth > 0) then
        List.Header.Columns[i].Width := colwidth;
    end;
  end;

  // Column visibility
  Value := GetRegValue(REGPREFIX_COLSVISIBLE + Regname, '');
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    SetVisibleListColumns( List, ValueList );
  end;

  // Column position
  Value := GetRegValue(REGPREFIX_COLPOS + Regname, '');
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    for i := 0 to ValueList.Count - 1 do
    begin
      colpos := MakeInt(ValueList[i]);
      // Check if column number exists
      if List.Header.Columns.Count > i then
        List.Header.Columns[i].Position := colpos;
    end;
  end;

  ValueList.Free;
end;


{**
  (Un)hide columns in a VirtualStringTree.
}
procedure TMainForm.SetVisibleListColumns( List: TVirtualStringTree; Columns: WideStrings.TWideStringList );
var
  i : Integer;
begin
  for i := 0 to List.Header.Columns.Count - 1 do
  begin
    // Only ListTables' column visibility is currently customizable
    // so, make sure to unhide the newer "Comment" column in ListColumns for some users 
    if (Columns.IndexOf( IntToStr(i) ) > -1) or (List <> ListTables) then
      List.Header.Columns[i].Options := List.Header.Columns[i].Options + [coVisible]
    else
      List.Header.Columns[i].Options := List.Header.Columns[i].Options - [coVisible];
  end;
end;


{**
  Start writing logfile.
  Called either in FormShow or after closing preferences dialog
}
procedure TMainForm.ActivateFileLogging;
var
  LogfilePattern : String;
  i : Integer;
begin
  // Ensure directory exists
  ForceDirectories( DirnameSessionLogs );

  // Determine free filename
  LogfilePattern := '%s %.6u.log';
  i := 1;
  FileNameSessionLog := DirnameSessionLogs + goodfilename(Format(LogfilePattern, [SessionName, i]));
  while FileExists( FileNameSessionLog ) do
  begin
    inc(i);
    FileNameSessionLog := DirnameSessionLogs + goodfilename(Format(LogfilePattern, [SessionName, i]));
  end;

  // Create file handle for writing
  AssignFile( FileHandleSessionLog, FileNameSessionLog );
  {$I-} // Supress errors
  if FileExists(FileNameSessionLog) then
    Append(FileHandleSessionLog)
  else
    Rewrite(FileHandleSessionLog);
  {$I+}
  if IOResult <> 0 then
  begin
    MessageDlg('Error opening session log file:'+CRLF+FileNameSessionLog+CRLF+CRLF+'Logging is disabled now.', mtError, [mbOK], 0);
    prefLogToFile := False;
  end else
    prefLogToFile := True;
  // Update popupMenu items
  menuLogToFile.Checked := prefLogToFile;
  menuOpenLogFolder.Enabled := prefLogToFile;
end;


{**
  Close logfile.
  Called in FormClose, in ActivateFileLogging and on closing preferences dialog
}
procedure TMainForm.DeactivateFileLogging;
begin
  prefLogToFile := False;
  {$I-} // Supress errors
  CloseFile(FileHandleSessionLog);
  {$I+}
  // Reset IOResult so later checks in ActivateFileLogging doesn't get an old value
  IOResult;
  // Update popupMenu items
  menuLogToFile.Checked := prefLogToFile;
  menuOpenLogFolder.Enabled := prefLogToFile;
end;


{**
  Display tooltips in VirtualTrees. Imitates default behaviour of TListView.
}
procedure TMainForm.vstGetHint(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; var LineBreakStyle:
    TVTTooltipLineBreakStyle; var HintText: WideString);
var
  r : TRect;
  DisplayedWidth,
  NeededWidth : Integer;
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree(Sender);
  HintText := Tree.Text[Node, Column];
  // Check if the list has shortened the text
  r := Tree.GetDisplayRect(Node, Column, True);
  DisplayedWidth := r.Right-r.Left;
  NeededWidth := Canvas.TextWidth(HintText) + Tree.TextMargin*2;
  //debug(format('need: %d, given: %d, font: %s %d', [NeededWidth, DisplayedWidth, canvas.Font.Name, canvas.Font.Size]));
  // Disable displaying hint if text is displayed completely in list
  if NeededWidth <= DisplayedWidth then
    HintText := '';
end;


{**
  Enable/disable file logging by popupmenuclick
}
procedure TMainForm.menuLogToFileClick(Sender: TObject);
var
  OldprefLogToFile: Boolean;
begin
  OldprefLogToFile := prefLogToFile;
  if not prefLogToFile then
    ActivateFileLogging
  else
    DeactivateFileLogging;

  // Save option
  if prefLogToFile <> OldprefLogToFile then
  begin
    OpenRegistry;
    MainReg.WriteBool('LogToFile', prefLogToFile);
  end;
end;


{**
  Open folder with session logs
}
procedure TMainForm.menuOpenLogFolderClick(Sender: TObject);
begin
  ShellExec( '', DirnameSessionLogs );
end;


{**
  A header column of a VirtualTree was "dragged out", which means:
  dragged down or up, not to the left or right.
  We imitate the behaviour of various applications (fx Outlook) and
  hide this dragged column
}
procedure TMainForm.vstHeaderDraggedOut(Sender: TVTHeader; Column:
    TColumnIndex; DropPosition: TPoint);
begin
  if Sender.Treeview = ListTables then
  begin
    // Keep "Tables" column
    if Column = 0 then
      Exit;
    // Uncheck menuitem in header's contextmenu
    popupDBGridHeader.Items[Column].Checked := False;
  end;
  // Hide the draggedout column
  Sender.Columns[Column].Options := Sender.Columns[Column].Options - [coVisible];
end;


{**
  A cell in ListCommandStats gets painted.
  Draw a progress bar on it to visualize its percentage value.
}
procedure TMainForm.ListCommandStatsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  percent : Extended;
  barwidth, cellwidth: Integer;
  NodeData: PVTreeData;
begin
  // Only paint bar in percentage column
  if Column <> 4 then
    Exit;

  // Add minimal margin to cell edges
  InflateRect(CellRect, -1, -1);
  cellwidth := CellRect.Right - CellRect.Left;

  // Calculate value to display
  NodeData := Sender.GetNodeData(Node);
  percent := MakeFloat(NodeData.Captions[Column]);
  barwidth := Round(cellwidth / 100 * percent);

  // Adjust width of rect and paint the bar
  CellRect.Right := CellRect.Right - cellwidth + barwidth;
  TargetCanvas.Pen.Color := clGray;
  TargetCanvas.Brush.Color := clInfoBk;
  TargetCanvas.Rectangle(CellRect);
end;


{**
  Fetch table engines from server
  Currently used in tbl_properties and createtable
}
procedure TMainForm.TableEnginesCombo(var Combobox: TCombobox);
var
  engineName, defaultEngine, engineSupport : String;
  HaveEngineList : TStrings;
begin
  Combobox.Items.BeginUpdate;
  Combobox.Items.Clear;

  // Cache datasets
  if ((dsShowEngines = nil) or (dsShowEngines.State = dsInactive)) and
    ((dsHaveEngines = nil) or (dsHaveEngines.State = dsInactive)) then
  begin
    FreeAndNil(dsShowEngines);
    FreeAndNil(dsHaveEngines);
    dsShowEngines := GetResults('SHOW ENGINES', True);
    if dsShowEngines = nil then
      dsHaveEngines := GetResults('SHOW VARIABLES LIKE ''have%''');
  end;

  if dsShowEngines <> nil then begin
    dsShowEngines.First;
    while not dsShowEngines.Eof do begin
      engineName := dsShowEngines.FieldByName('Engine').AsString;
      engineSupport := LowerCase(dsShowEngines.FieldByName('Support').AsString);
      // Add to dropdown if supported
      if engineSupport <> 'no' then
        Combobox.Items.Add(engineName);
      // Check if this is the default engine
      if engineSupport = 'default' then
        defaultEngine := engineName;
      dsShowEngines.Next;
    end;
  end
  else begin
    // Manually fetch available engine types by analysing have_* options
    // This is for servers below 4.1 or when the SHOW ENGINES statement has
    // failed for some other reason

    // Add default engines which will not show in a have_* variable:
    Combobox.Items.CommaText := 'MyISAM,MRG_MyISAM,HEAP';
    defaultEngine := 'MyISAM';
    // Possible other engines:
    HaveEngineList := TStringList.Create;
    HaveEngineList.CommaText := 'ARCHIVE,BDB,BLACKHOLE,CSV,EXAMPLE,FEDERATED,INNODB,ISAM';
    dsHaveEngines.First;
    while not dsHaveEngines.Eof do begin
      engineName := copy(dsHaveEngines.Fields[0].AsString, 6, Length(dsHaveEngines.Fields[0].AsString) );
      // Strip additional "_engine" suffix, fx from "have_blackhole_engine"
      if Pos('_', engineName) > 0 then
        engineName := copy(engineName, 0, Pos('_', engineName)-1);
      engineName := UpperCase(engineName);
      // Add engine to dropdown if it's a) in HaveEngineList and b) activated
      if (HaveEngineList.IndexOf(engineName) > -1)
        and (LowerCase(dsHaveEngines.Fields[1].AsString) = 'yes') then
        Combobox.Items.Add(engineName);
      dsHaveEngines.Next;
    end;
  end;

  Combobox.Sorted := True;

  // Select default
  Combobox.ItemIndex := Combobox.Items.IndexOf(defaultEngine);

  Combobox.Items.EndUpdate;
end;


{**
  A row in the process list was selected. Fill SynMemoProcessView with
  the SQL of that row.
}
procedure TMainForm.ListProcessesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  NodeData : PVTreeData;
  enableSQLView : Boolean;
begin
  enableSQLView := Assigned(Node);
  SynMemoProcessView.Enabled := enableSQLView;
  pnlProcessView.Enabled := enableSQLView;
  if enableSQLView then begin
    NodeData := ListProcesses.GetNodeData(Node);
    SynMemoProcessView.Text := NodeData.Captions[7];
  end
  else SynMemoProcessView.Clear;
end;


{***
  Apply a filter to a Virtual Tree.
  Currently used for ListVariables, ListStatus and ListProcesses
}
procedure TMainForm.editFilterVTChange(Sender: TObject);
var
  Node : PVirtualNode;
  NodeData : PVTreeData;
  VT : TVirtualStringTree;
  i : Integer;
  match : Boolean;
  search : String;
  tab: TTabSheet;
  VisibleCount: Cardinal;
begin
  // Find the correct VirtualTree that shall be filtered
  tab := PageControlHost.ActivePage;
  if tab = tabVariables then
    VT := ListVariables
  else if tab = tabStatus then
    VT := ListStatus
  else if tab = tabProcesslist then
    VT := ListProcesses
  else
    VT := ListCommandStats;
  // Loop through all nodes to adjust their vsVisible state
  Node := VT.GetFirst;
  search := LowerCase( editFilterVT.Text );
  VisibleCount := 0;
  while Assigned(Node) do begin
    NodeData := VT.GetNodeData(Node);
    // Don't filter anything if the filter text is empty
    match := search = '';
    // Search for given text in node's captions
    if not match then for i := 0 to NodeData.Captions.Count - 1 do begin
      if Pos( search, LowerCase(NodeData.Captions[i]) ) > 0 then begin
        match := True;
        break;
      end;
    end;
    if match then begin
      Node.States := Node.States + [vsVisible];
      inc(VisibleCount);
    end else
      Node.States := Node.States - [vsVisible];
    Node := VT.GetNext(Node);
  end;
  // Colorize TEdit with filter string to signalize that some nodes are hidden now
  if VisibleCount <> VT.RootNodeCount then begin
    editFilterVT.Font.Color := clRed;
    editFilterVT.Color := clYellow;
  end else begin
    editFilterVT.Font.Color := clWindowText;
    editFilterVT.Color := clWindow;
  end;
  if search <> '' then begin
    lblFilterVTInfo.Caption := IntToStr(VisibleCount)+' out of '+IntToStr(VT.RootNodeCount)+' matching. '
      + IntToStr(VT.RootNodeCount - VisibleCount) + ' hidden.';
  end else
    lblFilterVTInfo.Caption := '';

  // RootNode.TotalHeight needs to be recalculated so the scrollbar has the correct
  // range, ignoring hidden nodes.
  // Similar to what is done by VT.FixupTotalHeight() which doesn't work
  // for some reason if called from within VT.UpdateVerticalScrollBar()
  VT.RootNode.TotalHeight := 0;
  Node := VT.GetFirst;
  while Assigned(Node) do begin
    if vsVisible in Node.States then
      Inc(VT.RootNode.TotalHeight, Node.TotalHeight);
    Node := Node.NextSibling;
  end;
  VT.UpdateVerticalScrollBar(True);
  VT.Repaint;
end;


procedure TMainForm.ListVariablesDblClick(Sender: TObject);
begin
  menuEditVariable.Click;
end;


{**
  Edit a server variable
}
procedure TMainForm.menuEditVariableClick(Sender: TObject);
var
  NodeData: PVTreeData;
begin
  if EditVariableForm = nil then
    EditVariableForm := TfrmEditVariable.Create(Self);
  NodeData := ListVariables.GetNodeData(ListVariables.FocusedNode);
  EditVariableForm.VarName := NodeData.Captions[0];
  EditVariableForm.VarValue := NodeData.Captions[1];
  // Refresh relevant list node
  if EditVariableForm.ShowModal = mrOK then
    NodeData.Captions[1] := GetVar('SHOW VARIABLES LIKE '+esc(NodeData.Captions[0]), 1);
end;


{**
  The database tree doesn't use any structure for its nodes.
}
procedure TMainForm.DBtreeGetNodeDataSize(Sender: TBaseVirtualTree; var
    NodeDataSize: Integer);
begin
  NodeDataSize := 0;
end;


{**
  Set text of a treenode before it gets displayed or fetched in any way
}
procedure TMainForm.DBtreeGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
var
  ds: TDataset;
  db, eng: WideString;
  i: Integer;
  Bytes: Int64;
  AllListsCached: Boolean;
begin
  case Column of
    0: case Sender.GetNodeLevel(Node) of
        0: CellText := FConn.MysqlParams.User + '@' + FConn.MysqlParams.Host;
        1: CellText := Databases[Node.Index];
        2: begin
            ds := FetchDbTableList(Databases[Node.Parent.Index]);
            ds.RecNo := Node.Index+1;
            CellText := ds.FieldByName(DBO_NAME).AsWideString;
          end;
      end;
    1: case GetTreeNodeType(Node) of
        // Calculate and display the sum of all table sizes in ALL dbs if all table lists are cached
        lntNone: begin
            AllListsCached := true;
            for i := 0 to Databases.Count - 1 do begin
              if not DbTableListCachedAndValid(Databases[i]) then begin
                AllListsCached := false;
                break;
              end;
            end;
            // Will be also set to a negative value by GetTableSize and results of SHOW TABLES
            Bytes := -1;
            if AllListsCached then begin
              Bytes := 0;
              for i := 0 to Databases.Count - 1 do begin
                ds := FetchDbTableList(Databases[i]);
                while not ds.Eof do begin
                  Bytes := Bytes + GetTableSize(ds);
                  ds.Next;
                end;
              end;
            end;
            if Bytes >= 0 then CellText := FormatByteNumber(Bytes)
            else CellText := '';
          end;
        // Calculate and display the sum of all table sizes in ONE db, if the list is already cached.
        lntDb: begin
            db := DBtree.Text[Node, 0];
            if not DbTableListCachedAndValid(db) then
              CellText := ''
            else begin
              Bytes := 0;
              ds := FetchDbTableList(db);
              while not ds.Eof do begin
                if ds.FindField('Type') <> nil then eng := FieldContent(ds, 'Type')
                else eng := FieldContent(ds, 'Engine');
                if UpperCase(eng) <> 'MRG_MYISAM' then
                  Bytes := Bytes + GetTableSize(ds);
                ds.Next;
              end;
              if Bytes >= 0 then CellText := FormatByteNumber(Bytes)
              else CellText := '';
            end;
          end;
        lntTable: begin
          db := DBtree.Text[Node.Parent, 0];
          ds := FetchDbTableList(db);
          ds.RecNo := Node.Index + 1;
          Bytes := GetTableSize(ds);
          CellText := FormatByteNumber(Bytes);
        end
        else CellText := ''; // Applies for views and crashed tables
      end;
  end;
end;


{**
  Set icon of a treenode before it gets displayed
}
procedure TMainForm.DBtreeGetImageIndex(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
    Boolean; var ImageIndex: Integer);
var
  ds: TDataset;
begin
  if Column > 0 then
    Exit;
  case Sender.GetNodeLevel(Node) of
    0: ImageIndex := ICONINDEX_SERVER;
    1: if (Kind = ikSelected) or ((Sender.GetFirstSelected<>nil) and (Node=Sender.GetFirstSelected.Parent)) then
         ImageIndex := ICONINDEX_DB_HIGHLIGHT
         else ImageIndex := ICONINDEX_DB;
    2: begin
        ds := FetchDbTableList(Databases[Node.Parent.Index]);
        ds.RecNo := Node.Index+1;
        case GetDBObjectType(ds.Fields) of
          lntTable:
            if Kind = ikSelected then
              ImageIndex := ICONINDEX_TABLE_HIGHLIGHT
              else ImageIndex := ICONINDEX_TABLE;
          lntView:
            if Kind = ikSelected then
              ImageIndex := ICONINDEX_VIEW_HIGHLIGHT
              else ImageIndex := ICONINDEX_VIEW;
          lntCrashedTable:
            if Kind = ikSelected then
              ImageIndex := ICONINDEX_CRASHED_TABLE_HIGHLIGHT
              else ImageIndex := ICONINDEX_CRASHED_TABLE;
          lntProcedure:
            ImageIndex := ICONINDEX_STOREDPROCEDURE;
          lntFunction:
            ImageIndex := ICONINDEX_STOREDFUNCTION;
        end;
      end;
  end;
end;


{**
  Set childcount of an expanding treenode
}
procedure TMainForm.DBtreeInitChildren(Sender: TBaseVirtualTree; Node:
    PVirtualNode; var ChildCount: Cardinal);
var
  ds: TDataset;
  specialDbs: WideStrings.TWideStringList;
  dbName: WideString;
  i: Integer;
begin
  case Sender.GetNodeLevel(Node) of
    // Root node has only one single child (user@host)
    0: begin
        Screen.Cursor := crHourglass;
        Showstatus( 'Reading Databases...' );
        try
          Databases := WideStrings.TWideStringList.Create;
          if DatabasesWanted.Count = 0 then begin
            ds := GetResults( 'SHOW DATABASES' );
            specialDbs := WideStrings.TWideStringList.Create;
            for i:=1 to ds.RecordCount do begin
              dbName := ds.FieldByName('Database').AsWideString;
              if dbName = DBNAME_INFORMATION_SCHEMA then specialDbs.Insert( 0, dbName )
              else Databases.Add( dbName );
              ds.Next;
            end;
            ds.Close;
            FreeAndNil(ds);
            Databases.Sort;
            // Prioritised position of system-databases
            for i := specialDbs.Count - 1 downto 0 do
              Databases.Insert( 0, specialDbs[i] );
          end else for i:=0 to DatabasesWanted.Count-1 do
            Databases.Add(DatabasesWanted[i]);
          showstatus( IntToStr( Databases.Count ) + ' Databases', 0 );
          ChildCount := Databases.Count;
          // Avoids excessive InitializeKeywordLists() calls.
          SynSQLSyn1.TableNames.BeginUpdate;
          SynSQLSyn1.TableNames.Clear;
          // Let synedit know all database names so that they can be highlighted
          // TODO: Is this right?  Adding "<db name>.<table name>" seems to make more sense..
          for i := 0 to Databases.Count - 1 do
            SynSQLSyn1.TableNames.Add(Databases[i]);
          SynSQLSyn1.TableNames.EndUpdate;
        finally
          ShowStatus( STATUS_MSG_READY );
          Screen.Cursor := crDefault;
        end;
      end;
    // DB node expanding
    1: begin
        Screen.Cursor := crHourglass;
        Showstatus( 'Reading Tables...' );
        try
          ds := FetchDbTableList(Databases[Node.Index]);
          ChildCount := ds.RecordCount;
        finally
          ShowStatus( STATUS_MSG_READY );
          Screen.Cursor := crDefault;
        end;
        // Auto resize "Size" column in dbtree when needed
        // See also OnResize
        if coVisible in (Sender as TVirtualStringTree).Header.Columns[1].Options then
          (Sender as TVirtualStringTree).Header.AutoFitColumns(False, smaUseColumnOption, 1, 1);
      end;
    else Exit;
  end;
end;


{**
  Set initial options of a treenode
}
procedure TMainForm.DBtreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
    PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  level: Cardinal;
begin
  level := Sender.GetNodeLevel(Node);
  // Ensure plus sign is visible for root and dbs
  if level in [0,1] then
    Include( InitialStates, ivsHasChildren);
  // Host node is always expanded
  if level = 0 then
    Include( InitialStates, ivsExpanded );
end;


{**
  Selection in database tree has changed
}
procedure TMainForm.DBtreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  newDb, newDbObject, Cap: WideString;
begin
  debug('DBtreeFocusChanged()');
  if not Assigned(Node) then
    Exit;
  // Post pending UPDATE
  if DataGridHasChanges then
    actDataPostChangesExecute(Sender);
  case Sender.GetNodeLevel(Node) of
    0: ShowHost;
    1: begin
        newDb := Databases[Node.Index];
        ShowDatabase( newDb );
      end;
    2: begin
        newDb := Databases[Node.Parent.Index];
        newDbObject := SelectedTable.Text;
        tabEditor.TabVisible := True;
        tabData.TabVisible := SelectedTable.NodeType in [lntTable, lntCrashedTable, lntView];
        if tabEditor.TabVisible then begin
          actEditObjectExecute(Sender);
          // When a table is clicked in the tree, and the current
          // tab is a Host or Database tab, switch to showing table columns.
          if (PagecontrolMain.ActivePage = tabHost) or (PagecontrolMain.ActivePage = tabDatabase) then
            PagecontrolMain.ActivePage := tabEditor;
          // When a table is clicked in the tree, and the data
          // tab is active, update the data tab
          if PagecontrolMain.ActivePage = tabData then
            ViewData(Sender);
          // When a table is clicked in the tree, and the query
          // tab is active, update the list of columns
          if PagecontrolMain.ActivePage = tabQuery then begin
            // Don't know why this next line is necessary, couldn't find
            // documented in the code how the refresh mechanism for it is
            // supposed to work.  It is necessary, though.
            ResetSelectedTableStuff;
            RefreshQueryHelpers;
          end;
        end;
      end;
  end;
  if newDb <> '' then
    LoadDatabaseProperties(newDb);
  FixQueryTabCloseButtons;
  // Set window caption and taskbar text
  Cap := winName;
  if newDb <> '' then
    Cap := Cap + ' /' + newDb;
  if newDbObject <> '' then
    Cap := Cap + '/' + newDbObject;
  Cap := Cap + ' - ' + APPNAME + ' ' + FullAppVersion;
  Caption := Cap;
  Application.Title := Cap;
end;


procedure TMainForm.DBtreeDblClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  // Paste DB or table name into query window on treeview double click.
  Node := DBtree.GetFirstSelected;
  if not Assigned(Node) then Exit;
  if DBtree.GetNodeLevel(Node) = 0 then Exit;
  if not QueryTabActive then Exit;
  ActiveQueryMemo.SelText := DBtree.Text[Node, 0];
  ActiveQueryMemo.SetFocus;
end;


procedure TMainForm.DBtreePaintText(Sender: TBaseVirtualTree; const
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType:
    TVSTTextType);
begin
  // Grey out rather unimportant "Size" column
  if Column <> 1 then
    Exit;
  case DBtree.GetNodeLevel(Node) of
    0: TargetCanvas.Font.Color := clWindowText;
    1: TargetCanvas.Font.Color := $008f8f8f;
    2: TargetCanvas.Font.Color := $00cfcfcf;
  end;
end;


{**
  Refresh the whole tree
}
procedure TMainForm.RefreshTree(DoResetTableCache: Boolean; SelectDatabase: WideString = '');
var
  oldActiveDatabase, db: WideString;
  oldSelectedTable: TListNode;
  Node: PVirtualNode;
  ExpandedDBs, TablesFetched: WideStrings.TWideStringList;
  i: Integer;
begin
  // Remember currently active database and table
  oldActiveDatabase := ActiveDatabase;
  oldSelectedTable := SelectedTable;
  // Temporary unselect any node to postpone event handlings
  if (DBtree.GetFirstSelected <> nil) and (DBtree.GetNodeLevel(DBtree.GetFirstSelected) > 0) then
    DBtree.ClearSelection;

  // Remember expandation status of all dbs and whether their tables were fetched
  ExpandedDBs := WideStrings.TWideStringList.Create;
  TablesFetched := WideStrings.TWideStringList.Create;
  Node := DBtree.GetFirstChild(DBtree.GetFirst);
  for i := 0 to DBtree.GetFirst.ChildCount - 1 do begin
    db := DBtree.Text[Node, 0];
    if DBtree.ChildrenInitialized[Node] then
      TablesFetched.Add(db);
    if vsExpanded in Node.States then
      ExpandedDBs.Add(db);
    Node := DBtree.GetNextSibling(Node);
  end;

  // ReInit tree population
  DBTree.BeginUpdate;
  DBtree.ReinitChildren(DBTree.GetFirst, False); // .ResetNode(DBtree.GetFirst);
  if DoResetTableCache then
    ClearAllTableLists;
  // Reselect active or new database if present. Could have been deleted or renamed.
  try
    if SelectDatabase <> '' then ActiveDatabase := SelectDatabase
    else if oldActiveDatabase <> '' then ActiveDatabase := oldActiveDatabase;
  except
  end;

  // Expand nodes which were previously expanded
  Node := DBtree.GetFirstChild(DBtree.GetFirst);
  for i := 0 to DBtree.GetFirst.ChildCount - 1 do begin
    db := DBtree.Text[Node, 0];
    if TablesFetched.IndexOf(db) > -1 then
      DBtree.ReinitChildren(Node, False);
    DBtree.Expanded[Node] := ExpandedDBs.IndexOf(db) > -1;
    Node := DBtree.GetNextSibling(Node);
  end;
  ExpandedDBs.Free;
  TablesFetched.Free;

  try
    if oldSelectedTable.Text <> '' then SelectDBObject(oldSelectedTable.Text, oldSelectedTable.NodeType);
  except
  end;
  DBTree.EndUpdate;
end;


{**
  Refresh one database node in the db tree
}
procedure TMainForm.RefreshTreeDB(db: WideString);
var
  oldActiveDatabase: WideString;
  dbnode: PVirtualNode;
begin
  oldActiveDatabase := ActiveDatabase;
  DBtree.ClearSelection;
  DBNode := FindDBNode(db);
  RefreshDbTableList(db);
  DBTree.ReinitNode(dbnode, true);
  DBtree.InvalidateChildren(dbnode, false);
  ActiveDatabase := oldActiveDatabase;
end;


{**
  Find a database node in the tree by passing its name
}
function TMainForm.FindDBNode(db: WideString): PVirtualNode;
var
  i, s: Integer;
  n: PVirtualNode;
begin
  Result := nil;
  // Ensure Databases list is instantiated (by DBtree.InitChildren)
  if Databases = nil then
    DBtree.ReinitNode(DBtree.GetFirst, False);
  // TStringList.CaseSensitive= True|False is only used in .IndexOf and .Sort procs,
  // it does not avoid or remove duplicate items
  Databases.CaseSensitive := True;
  s := Databases.IndexOf(db);
  if s = -1 then begin
    Databases.CaseSensitive := False;
    s := Databases.IndexOf(db);
  end;
  if s > -1 then begin
    n := DBtree.GetFirstChild(DBtree.GetFirst);
    for i := 0 to DBtree.GetFirst.ChildCount - 1 do begin
      if Integer(n.Index) = s then begin
        Result := n;
        Exit;
      end;
      n := DBtree.GetNextSibling(n);
    end;
  end;
end;


{**
  Expand all db nodes
}
procedure TMainForm.menuTreeExpandAllClick(Sender: TObject);
begin
  DBtree.FullExpand;
  DBtree.ScrollIntoView(DBtree.GetFirstSelected, False);
end;

{**
  Collapse all db nodes
}
procedure TMainForm.menuTreeCollapseAllClick(Sender: TObject);
var
  n: PVirtualNode;
  i: Integer;
begin
  n := DBtree.GetFirstChild(DBtree.GetFirst);
  for i := 0 to DBtree.GetFirst.ChildCount - 1 do begin
    DBtree.FullCollapse(n);
    n := DBtree.GetNextSibling(n);
  end;
  DBtree.ScrollIntoView(DBtree.GetFirstSelected, False);
end;


function TMainForm.GetTableSize(ds: TDataSet): Int64;
var
  d, i: String;
begin
  d := FieldContent(ds, 'Data_length');
  i := FieldContent(ds, 'Index_length');
  if (d = '') or (i = '') then Result := -1
  else Result := MakeInt(d) + MakeInt(i);
end;

function TMainForm.DbTableListCachedAndValid(db: WideString): Boolean;
var
  ds: TDataSet;
begin
  Result := CachedTableLists.IndexOf(db) > -1;
  if Result then begin
    ds := TDataSet(CachedTableLists.Objects[CachedTableLists.IndexOf(db)]);
    // Delphi's RTL (TDataSet in DB.pas) throws exceptions right and left
    // if the database the dataset(-derivate, aka TZDataSet) came from is
    // currently, or has been earlier been, disconnected.  Therefore, nuke
    // these datasets, they'll have to be reloaded.
    if ds.State = dsInactive then begin
      ClearDbTableList(db);
      Result := False;
    end;
  end;
end;

procedure TMainForm.editFilterSearchChange(Sender: TObject);
var
  Add, Clause: WideString;
  i: Integer;
  ed: TEdit;
begin
  ed := TEdit(Sender);
  Clause := '';
  Add := '';
  if ed.Text <> '' then begin
    SelectedTableColumns.First;
    for i := 0 to SelectedTableColumns.RecordCount - 1 do begin
      if i > 0 then
        Add := Add + ' OR ';
      Add := Add + mask(SelectedTableColumns.Fields[0].AsWideString) + ' LIKE ' + esc('%'+ed.Text+'%');
      if Length(Add) > 45 then begin
        Clause := Clause + Add + CRLF;
        Add := '';
      end;
      SelectedTableColumns.Next;
    end;
    if Add <> '' then
      Clause := Clause + Add;
  end;
  SynMemoFilter.UndoList.AddGroupBreak;
  SynMemoFilter.SelectAll;
  SynMemoFilter.SelText := Clause;
  SynMemoFilterChange(Sender);
end;


procedure TMainForm.SynMemoFilterChange(Sender: TObject);
var
  SomeText: Boolean;
begin
  SomeText := (SynMemoFilter.GetTextLen > 0) or (editFilterSearch.Text <> '');
  actClearFilterEditor.Enabled := SomeText;
end;


procedure TMainForm.ToggleFilterPanel(ForceVisible: Boolean = False);
var
  ShowIt: Boolean;
begin
  ShowIt := ForceVisible or (not pnlFilter.Visible);
  tbtnDataFilter.Down := ShowIt;
  pnlFilter.Visible := ShowIt;
end;


procedure TMainForm.editFilterSearchEnter(Sender: TObject);
begin
  // Enables triggering apply button with Enter
  btnFilterApply.Default := True;
end;


procedure TMainForm.editFilterSearchExit(Sender: TObject);
begin
  btnFilterApply.Default := False;
end;


procedure TMainForm.EnsureNodeLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode; WhereClause: WideString);
var
  res: TGridResult;
  query: WideString;
  ds: TDataSet;
  i, j: LongInt;
begin
  res := GridResult(Sender);
  if (not res.Rows[Node.Index].Loaded) and (res.Rows[Node.Index].State <> grsInserted) then begin
    query := DataGridCurrentSelect + DataGridCurrentFrom;
    // Passed WhereClause has prio over current filter, fixes bug #754
    if WhereClause <> '' then begin
      query := query + ' WHERE ' + WhereClause;
    end else if DataGridCurrentFilter <> '' then begin
      query := query + ' WHERE ' + DataGridCurrentFilter;
    end;

    // start query
    ShowStatus('Retrieving data...');
    ds := GetResults(query);
    // If new data does not match current filter, remove from tree.
    if Cardinal(ds.RecordCount) < 1 then begin
      // Remove entry from dynamic array.
      for i := Node.Index to Length(res.Rows) - 1 do begin
        if i < Length(res.Rows) - 1 then res.Rows[i] := res.Rows[i + 1];
      end;
      SetLength(res.Rows, Length(res.Rows) - 1);
      // Remove entry from node list.
      Sender.DeleteNode(Node);
    end;

    // fill in data
    ShowStatus('Filling grid with record-data...');
    if Cardinal(ds.RecordCount) > 0 then begin
      SetLength(res.Rows[Node.Index].Cells, ds.Fields.Count);
      i := Node.Index;
      for j := 0 to ds.Fields.Count - 1 do begin
        if res.Columns[j].DatatypeCat = dtcBinary then
          res.Rows[i].Cells[j].Text := '0x' + BinToWideHex(ds.Fields[j].AsString)
        else
          res.Rows[i].Cells[j].Text := ds.Fields[j].AsWideString;
        res.Rows[i].Cells[j].IsNull := ds.Fields[j].IsNull;
      end;
      res.Rows[Node.Index].Loaded := True;
    end;

    ShowStatus( STATUS_MSG_READY );
    FreeAndNil(ds);
  end;
end;

procedure TMainForm.EnsureChunkLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode; FullWidth: Boolean = False);
var
  res: TGridResult;
  start, limit: Cardinal;
  query: WideString;
  ds: TDataSet;
  i, j: LongInt;
  hi: LongInt;
  regCrashIndicName: String;
begin
  res := GridResult(Sender);
  if (not res.Rows[Node.Index].Loaded) and (res.Rows[Node.Index].State <> grsInserted) then begin
    start := Node.Index - (Node.Index mod GridMaxRows);
    limit := TVirtualStringTree(Sender).RootNodeCount - start;
    if limit > GridMaxRows then limit := GridMaxRows;
    if FullWidth then
      query := DataGridCurrentFullSelect + DataGridCurrentFrom
    else
      query := DataGridCurrentSelect + DataGridCurrentFrom;
    if DataGridCurrentFilter <> '' then query := query + ' WHERE ' + DataGridCurrentFilter;
    if DataGridCurrentSort <> '' then query := query + ' ORDER BY ' + DataGridCurrentSort;
    query := query + WideFormat(' LIMIT %d, %d', [start, limit]);

    // Set indicator for possibly crashing query
    OpenRegistry(SessionName);
    regCrashIndicName := Utf8Encode(REGPREFIX_CRASH_IN_DATA + ActiveDatabase + '.' + SelectedTable.Text);
    MainReg.WriteBool(regCrashIndicName, True);

    // start query
    ShowStatus('Retrieving data...');
    debug(Format('mem: loading data chunk from row %d to %d', [start, limit]));
    try
      ds := GetResults(query);
    except
      // if something bad happened, nuke cache, reset cursor and display error.
      TVirtualStringTree(Sender).RootNodeCount := 0;
      SetLength(res.Rows, 0);
      ReachedEOT := true;
      ShowStatus(STATUS_MSG_READY);
      Screen.Cursor := crDefault;
      raise;
    end;
    if Cardinal(ds.RecordCount) < limit then begin
      limit := ds.RecordCount;
      TVirtualStringTree(Sender).RootNodeCount := start + limit;
      SetLength(res.Rows, start + limit);
      ReachedEOT := true;
    end;
    if not ReachedEOT then begin
      hi := start + limit;
      if hi < SIMULATE_INITIAL_ROWS then hi := SIMULATE_INITIAL_ROWS;
      hi := hi * (100 + SIMULATE_MORE_ROWS) div 100;
      Sender.BeginUpdate;
      TVirtualStringTree(Sender).RootNodeCount := Cardinal(hi);
      SetLength(res.Rows, hi);
      Sender.EndUpdate;
    end;
    debug(Format('mem: loaded data chunk from row %d to %d', [start, limit]));

    // Query was completed successfully. Reset crash indicator.
    MainReg.DeleteValue(regCrashIndicName);

    // fill in data
    ShowStatus('Filling grid with record-data...');
    for i := start to start + limit - 1 do begin
      SetLength(res.Rows[i].Cells, ds.Fields.Count);
      for j := 0 to ds.Fields.Count - 1 do begin
        if res.Columns[j].DatatypeCat = dtcBinary then
          res.Rows[i].Cells[j].Text := '0x' + BinToWideHex(ds.Fields[j].AsString)
        else
          res.Rows[i].Cells[j].Text := ds.Fields[j].AsWideString;
        res.Rows[i].Cells[j].IsNull := ds.Fields[j].IsNull;
      end;
      res.Rows[i].Loaded := True;
      ds.Next;
    end;

    if res = DataGridResult then begin
      if ReachedEOT then DisplayRowCountStats(Length(res.Rows))
      else DisplayRowCountStats(-1);
    end;

    ShowStatus( STATUS_MSG_READY );
    FreeAndNil(ds);
  end;
end;

procedure TMainForm.DiscardNodeData(Sender: TVirtualStringTree; Node: PVirtualNode);
begin
  // Avoid discarding query data as it will never be reloaded.
  if Sender <> DataGrid then Exit;
  // Avoid rows being edited.
  if DataGridResult.Rows[Node.Index].State = grsDefault then begin
    DataGridResult.Rows[Node.Index].Loaded := false;
    SetLength(DataGridResult.Rows[Node.Index].Cells, 0);
  end;
end;

{**
  A grid cell fetches its text content
}
procedure TMainForm.GridGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
var
  c: PGridCell;
  gr: TGridResult;
  EditingCell: Boolean;
begin
  if Column = -1 then
    Exit;
  gr := GridResult(Sender);
  if Node.Index >= Cardinal(Length(gr.Rows)) then Exit;
  EnsureChunkLoaded(Sender, Node);
  if Node.Index >= Cardinal(Length(gr.Rows)) then Exit;
  c := @gr.Rows[Node.Index].Cells[Column];
  EditingCell := Sender.IsEditing and (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn);
  if c.Modified then begin
    if c.NewIsNull then begin
      if EditingCell then CellText := ''
      else CellText := TEXT_NULL;
    end else CellText := c.NewText;
  end else begin
    if c.IsNull then begin
      if EditingCell then CellText := ''
      else CellText := TEXT_NULL;
    end else begin
      CellText := c.Text;
      if Length(c.Text) = GridMaxData then CellText := CellText + ' [...]';
    end;
  end;
end;


procedure TMainForm.CalcNullColors;
var
  i: Integer;
begin
  for i:=Low(DatatypeCategories) to High(DatatypeCategories) do
    DatatypeCategories[i].NullColor := ColorAdjustBrightness(DatatypeCategories[i].Color, COLORSHIFT_NULLFIELDS);
end;


{**
  Cell in data- or query grid gets painted. Colorize font. This procedure is
  called extremely often for repainting the grid cells. Keep it highly optimized.
}
procedure TMainForm.GridPaintText(Sender: TBaseVirtualTree; const
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType:
    TVSTTextType);
var
  cl: TColor;
  r: TGridResult;
begin
  if Column = -1 then
    Exit;

  r := GridResult(Sender);

  if Node.Index >= Cardinal(Length(r.Rows)) then
    Exit;
    
  // Make primary key columns bold
  if r.Columns[Column].IsPriPart then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];

  // Do not apply any color on a selected, highlighted cell to keep readability
  if (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then
    cl := clHighlightText
  else if vsSelected in Node.States then
    cl := clBlack
  else if r.Rows[Node.Index].Cells[Column].IsNull then
    cl := DatatypeCategories[Integer(r.Columns[Column].DatatypeCat)].NullColor
  else
    cl := DatatypeCategories[Integer(r.Columns[Column].DatatypeCat)].Color;
  TargetCanvas.Font.Color := cl;
end;


procedure TMainForm.DataGridAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  // Don't waist time
  if Column = -1 then Exit;
  if Node.Index >= Cardinal(Length(DataGridResult.Rows)) then Exit;
  // Paint a red triangle at the top left corner of the cell
  if DataGridResult.Rows[Node.Index].Cells[Column].Modified then
    PngImageListMain.Draw(TargetCanvas, CellRect.Left, CellRect.Top, 111);
end;


{**
  Header column in datagrid clicked.
  Left button: handle ORDER BY
  Right button: show column selection box
}
procedure TMainForm.DataGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  frm: TForm;
  i, j : Integer;
  columnexists : Boolean;
  ColName: WideString;
begin
  if HitInfo.Column = NoColumn then
    Exit;
  if HitInfo.Button = mbLeft then begin
    ColName := Sender.Columns[HitInfo.Column].Text;
    // Add a new order column after a columns title has been clicked
    // Check if order column is already existant
    columnexists := False;
    for i := Low(FDataGridSort) to High(FDataGridSort) do begin
      if FDataGridSort[i].ColumnName = ColName then begin
        // AddOrderCol is already in the list. Switch its direction:
        // ASC > DESC > [delete col]
        columnexists := True;
        if FDataGridSort[i].SortDirection = ORDER_ASC then
          FDataGridSort[i].SortDirection := ORDER_DESC
        else begin
          // Delete order col
          for j := i to High(FDataGridSort) - 1 do
            FDataGridSort[j] := FDataGridSort[j+1];
          SetLength(FDataGridSort, Length(FDataGridSort)-1);
        end;
        // We found the matching column, no need to loop further
        break;
      end;
    end;

    if not columnexists then begin
      i := Length(FDataGridSort);
      SetLength(FDataGridSort, i+1);
      FDataGridSort[i] := TOrderCol.Create;
      FDataGridSort[i].ColumnName := ColName;
      FDataGridSort[i].SortDirection := ORDER_ASC;
    end;
    ViewData(Sender);
  end else begin
    frm := TColumnSelectionForm.Create(self);
    // Position new form relative to btn's position
    frm.Top := HitInfo.Y + DataGrid.ClientOrigin.Y - Integer(DataGrid.Header.Height);
    frm.Left := HitInfo.X + DataGrid.ClientOrigin.X;
    // Display form
    frm.Show;
  end;
end;


{**
  Only allow grid editing if there is a good key available
}
procedure TMainForm.setNULL1Click(Sender: TObject);
begin
  if not CheckUniqueKeyClause then
    Exit;
  // Internally calls OnNewText event:
  DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn] := '';
  DataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].NewIsNull := True;
  DataGrid.RepaintNode(DataGrid.FocusedNode);
end;


{**
  Content of a grid cell was modified
}
procedure TMainForm.DataGridNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  Row: PGridRow;
begin
  Row := @DataGridResult.Rows[Node.Index];
  // Remember new value
  Row.Cells[Column].NewText := NewText;
  Row.Cells[Column].NewIsNull := False;
  Row.Cells[Column].Modified := True;
  // Set state of row for UPDATE mode, don't touch grsInserted
  if Row.State = grsDefault then
    DataGridResult.Rows[Node.Index].State := grsModified;
  DataGridHasChanges := True;
  ValidateControls(Sender);
end;


{**
  Checks if there is a unique key available which can be used for UPDATEs and INSERTs
}
function TMainForm.CheckUniqueKeyClause: Boolean;
var
  mres: Integer;
begin
  Result := GetKeyColumns.Count > 0;
  if not Result then begin
    Screen.Cursor := crDefault;
    mres := MessageDlg('Grid editing and selective row operations are blocked because this table does not have a primary '+
      'or a unique key, or it only contains a unique key which allows NULLs which turns that '+
      'key to be non unique again. You can create or edit the keys using the index manager.'+CRLF+CRLF+
      'Press'+CRLF+
      '  [Ok] to cancel editing and call the index manager'+CRLF+
      '  [Cancel] to cancel editing.',
      mtWarning, [mbOK, mbCancel], 0);
    if mres = mrOK then
      actEditObjectExecute(actEditObject);
  end;
end;


{**
  DataGrid: node focus has changed
}
procedure TMainForm.DataGridChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  ValidateControls(Sender);
end;


procedure TMainForm.DataGridClick(Sender: TObject);
var
  VT: TVirtualStringTree;
  Click: THitInfo;
begin
  // Start editing by click
  VT := Sender as TVirtualStringTree;
  VT.GetHitTestInfoAt(Mouse.CursorPos.X-VT.ClientOrigin.X, Mouse.CursorPos.Y-VT.ClientOrigin.Y, True, Click);
  if Assigned(Click.HitNode) and (Click.HitColumn > NoColumn) then
    VT.EditNode(Click.HitNode, Click.HitColumn);
end;

{**
  DataGrid: node and/or column focus is about to change. See if we allow that.
}
procedure TMainForm.DataGridFocusChanging(Sender: TBaseVirtualTree; OldNode,
    NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed:
    Boolean);
begin
  // Detect changed focus and update row
  if Assigned(OldNode) and (OldNode <> NewNode) then
    Allowed := DataGridPostUpdateOrInsert(OldNode)
  else
    Allowed := True;
end;


{**
  DataGrid: invoke update or insert routine
}
function TMainForm.DataGridPostUpdateOrInsert(Node: PVirtualNode): Boolean;
begin
  Result := True;
  if Cardinal(High(DataGridResult.Rows)) >= Node.Index then
    case DataGridResult.Rows[Node.Index].State of
      grsModified: Result := GridPostUpdate(DataGrid);
      grsInserted: Result := GridPostInsert(DataGrid);
    end;
end;


{**
  DataGrid: compose and fire UPDATE query
}
function TMainForm.GridPostUpdate(Sender: TBaseVirtualTree): Boolean;
var
  i: Integer;
  sql, Val: WideString;
  Row: PGridRow;
begin
  sql := 'UPDATE '+mask(DataGridDB)+'.'+mask(DataGridTable)+' SET';
  Row := @DataGridResult.Rows[Sender.FocusedNode.Index];
  for i := 0 to Length(DataGridResult.Columns) - 1 do begin
    if Row.Cells[i].Modified then begin
      Val := Row.Cells[i].NewText;
      if DataGridResult.Columns[i].DatatypeCat = dtcReal then
        Val := FloatStr(Val)
      else if DataGridResult.Columns[i].DatatypeCat = dtcBinary then begin
        CheckHex(Copy(Val, 3), 'Invalid hexadecimal string given in field "' + DataGridResult.Columns[i].Name + '".');
        if Val = '0x' then Val := esc('');
      end else
        Val := esc(Val);
      if Row.Cells[i].NewIsNull then Val := 'NULL';
      sql := sql + ' ' + mask(DataGridResult.Columns[i].Name) + '=' + Val + ', ';
    end;
  end;
  // Cut trailing comma
  sql := Copy(sql, 1, Length(sql)-2);
  sql := sql + ' WHERE ' + GetWhereClause(Row, @DataGridResult.Columns);
  try
    // Send UPDATE query
    if (ExecUpdateQuery(sql, False, True) = 0) then begin
      MessageDlg('Your change did not affect any row! This can have several causes:' + CRLF + CRLF +
        'a) Your changes were silently converted by the server. For instance, if you tried to ' +
        'update an unsigned TINYINT field from its maximum value 255 to a higher value.' + CRLF + CRLF +
        'b) The server could not find the source row because it was deleted ' +
        'from outside.' + CRLF + CRLF +
        'c) The server could not find the source row because its primary key fields were modified ' +
        'from outside.',
        mtInformation, [mbOK], 0);
    end;
    Result := True;
  except
    Result := False;
  end;

  if Result then begin
    // Reselect just updated row in grid from server to ensure displaying
    // correct values which were silently converted by the server
    for i := 0 to Length(DataGridResult.Columns) - 1 do begin
      if not Row.Cells[i].Modified then
        Continue;
      Row.Cells[i].Text := Row.Cells[i].NewText;
      Row.Cells[i].IsNull := Row.Cells[i].NewIsNull;
    end;
    GridFinalizeEditing(Sender);
    Row.Loaded := false;
    EnsureNodeLoaded(Sender, Sender.FocusedNode, GetWhereClause(Row, @DataGridResult.Columns));
  end;
end;


{**
  Repaint edited node and reset state of grid row
}
procedure TMainForm.GridFinalizeEditing(Sender: TBaseVirtualTree);
var
  i, c: Integer;
begin
  c := Sender.FocusedNode.Index;
  DataGridResult.Rows[c].State := grsDefault;
  for i := 0 to Length(DataGridResult.Rows[c].Cells) - 1 do begin
    DataGridResult.Rows[c].Cells[i].NewText := '';
    DataGridResult.Rows[c].Cells[i].Modified := False;
  end;
  Sender.RepaintNode(Sender.FocusedNode);
  DataGridHasChanges := False;
  ValidateControls(Sender);
end;


{**
  Compose a WHERE clause used for UPDATEs and DELETEs
}
function TMainForm.GetWhereClause(Row: PGridRow; Columns: PGridColumns): WideString;
var
  i, j: Integer;
  KeyVal: WideString;
  KeyCols: WideStrings.TWideStringlist;
begin
  Result := '';
  KeyCols := GetKeyColumns;
  for i := 0 to KeyCols.Count - 1 do begin
    for j := 0 to Length(Columns^) - 1 do begin
      if Columns^[j].Name = KeyCols[i] then
        break;
    end;
    // Find old value of key column
    KeyVal := Row.Cells[j].Text;
    // Quote if needed
    if DataGridResult.Columns[j].DatatypeCat = dtcReal then
      KeyVal := FloatStr(KeyVal)
    else if DataGridResult.Columns[j].DatatypeCat = dtcBinary then begin
      if KeyVal = '0x' then
        KeyVal := esc('');
    end else
      KeyVal := esc(KeyVal);

    if Row.Cells[j].IsNull then KeyVal := ' IS NULL'
    else KeyVal := '=' + KeyVal;
    Result := Result + mask(KeyCols[i]) + KeyVal + ' AND ';
  end;
  // Cut trailing AND
  Result := Copy(Result, 1, Length(Result)-5);
end;


{**
  Find key columns for a WHERE clause by analysing a SHOW KEYS FROM ... resultset
}
function TMainForm.GetKeyColumns: WideStrings.TWideStringlist;
var
  i: Integer;
  AllowsNull: Boolean;

  procedure FindColumns(const KeyName: WideString);
  begin
    // Find relevant key column names
    Result.Clear;
    SelectedTableKeys.First;
    while not SelectedTableKeys.Eof do begin
      if SelectedTableKeys.FieldByName('Key_name').AsWideString = KeyName then
        Result.Add(SelectedTableKeys.FieldByName('Column_name').AsWideString);
      SelectedTableKeys.Next;
    end;
  end;

begin
  Result := WideStrings.TWideStringlist.Create;
  // Find best key for updates
  SelectedTableKeys.First;
  // 1. round: find a primary key
  while not SelectedTableKeys.Eof do begin
    if SelectedTableKeys.FieldByName('Key_name').AsWideString = 'PRIMARY' then begin
      FindColumns(SelectedTableKeys.FieldByName('Key_name').AsWideString);
      Exit;
    end;
    SelectedTableKeys.Next;
  end;
  // no primary key available -> 2. round: find a unique key
  SelectedTableKeys.First;
  while not SelectedTableKeys.Eof do begin
    if SelectedTableKeys.FieldByName('Non_unique').AsInteger = 0 then begin
      // We found a UNIQUE key - better than nothing. Check if one of the key
      // columns allows NULLs which makes it dangerous to use in UPDATES + DELETES.
      FindColumns(SelectedTableKeys.FieldByName('Key_name').AsWideString);
      SelectedTableColumns.First;
      AllowsNull := False;
      for i := 0 to Result.Count - 1 do begin
        while (not SelectedTableColumns.Eof) and (not AllowsNull) do begin
          if SelectedTableColumns.FieldByName('Field').AsWideString = Result[i] then
            AllowsNull := UpperCase(SelectedTableColumns.FieldByName('Null').AsString) = 'YES';
          SelectedTableColumns.Next;
        end;
        if AllowsNull then break;
      end;
      if AllowsNull then Result.Clear
      else break;
    end;
    SelectedTableKeys.Next;
  end;
end;


{**
  DataGrid: compose and fire UPDATE query
}
procedure TMainForm.DataGridInsertRow;
var
  i, j: Integer;
begin
  // Scroll to the bottom to ensure we append the new row at the very last DataGridResult chunk
  DataGrid.FocusedNode := DataGrid.GetLast;
  DataGrid.Repaint;
  // Steeling focus now to invoke posting a pending row update
  DataGrid.FocusedNode := nil;
  i := Length(DataGridResult.Rows);
  SetLength(DataGridResult.Rows, i+1);
  SetLength(DataGridResult.Rows[i].Cells, Length(DataGridResult.Columns));
  DataGridResult.Rows[i].State := grsInserted;
  for j := 0 to Length(DataGridResult.Rows[i].Cells) - 1 do begin
    DataGridResult.Rows[i].Cells[j].Text := '';
  end;
  DataGrid.FocusedNode := DataGrid.AddChild(nil);
  DataGrid.ClearSelection;
  DataGrid.Selected[DataGrid.FocusedNode] := True;
  DataGridHasChanges := True;
  ValidateControls(DataGrid);
end;


{**
  DataGrid: compose and fire INSERT query
}
function TMainForm.GridPostInsert(Sender: TBaseVirtualTree): Boolean;
var
  Row: PGridRow;
  sql, Cols, Val, Vals: WideString;
  i: Integer;
  Node: PVirtualNode;
begin
  Node := Sender.FocusedNode;
  Row := @DataGridResult.Rows[Node.Index];
  Cols := '';
  Vals := '';
  for i := 0 to Length(DataGridResult.Columns) - 1 do begin
    SelectedTableColumns.RecNo := i;
    if Row.Cells[i].Modified then begin
      Cols := Cols + mask(DataGridResult.Columns[i].Name) + ', ';
      Val := Row.Cells[i].NewText;
      if DataGridResult.Columns[i].DatatypeCat = dtcReal then
        Val := FloatStr(Val)
      else if DataGridResult.Columns[i].DatatypeCat = dtcBinary then begin
        CheckHex(Copy(Val, 3), 'Invalid hexadecimal string given in field "' + DataGridResult.Columns[i].Name + '".');
        if Val = '0x' then
          Val := esc('');
      end else
        Val := esc(Val);
      if Row.Cells[i].NewIsNull then Val := 'NULL';
      Vals := Vals + Val + ', ';
    end;
  end;
  if Length(Cols) = 0 then begin
    // No field was manually modified, cancel the INSERT in that case
    Sender.BeginUpdate;
    Sender.DeleteNode(Node);
    SetLength(DataGridResult.Rows, Length(DataGridResult.Rows) - 1);
    Sender.EndUpdate;
    DataGridHasChanges := False;
    ValidateControls(Sender);
    Result := True; // Important for DataGridFocusChanging to allow moving focus
  end else begin
    // At least one field was modified, assume this INSERT should be posted
    Vals := Copy(Vals, 1, Length(Vals)-2);
    Cols := Copy(Cols, 1, Length(Cols)-2);
    sql := 'INSERT INTO '+mask(DataGridDB)+'.'+mask(DataGridTable)+' ('+Cols+') VALUES ('+Vals+')';
    // Send INSERT query
    if (ExecUpdateQuery(sql) = 0) then begin
      MessageBox(Self.Handle, 'Server failed to insert row.', 'Error', 0);
    end;
    Result := True;
    Row.Loaded := false;
    EnsureNodeLoaded(Sender, Node, GetWhereClause(Row, @DataGridResult.Columns));
    GridFinalizeEditing(Sender);
  end;
end;


{**
  DataGrid: compose and fire DELETE query
}
function TMainForm.GridPostDelete(Sender: TBaseVirtualTree): Boolean;
var
  Node: PVirtualNode;
  Nodes: TNodeArray;
  sql: WideString;
  Affected: Int64;
  Selected, i, j: Integer;
  msg: String;
begin
  Node := Sender.GetFirstSelected;
  sql := 'DELETE FROM '+mask(SelectedTable.Text)+' WHERE';
  while Assigned(Node) do begin
    EnsureChunkLoaded(Sender, Node);
    sql := sql + ' (' +
      GetWhereClause(@DataGridResult.Rows[Node.Index], @DataGridResult.Columns) +
      ') OR';
    Node := Sender.GetNextSelected(Node);
  end;
  sql := Copy(sql, 1, Length(sql)-3);

  try
    // Send DELETE query
    ExecUpdateQuery(sql, False, True);
    Result := True;
  except
    Result := False;
  end;

  if Result then begin
    // Remove deleted row nodes out of the grid
    Affected := FMysqlConn.Connection.GetAffectedRowsFromLastPost;
    Selected := Sender.SelectedCount;
    if Affected = Selected then begin
      // Fine. Number of deleted rows equals the selected node count.
      // In this case, just remove the selected nodes, avoid a full reload
      Sender.BeginUpdate;
      Nodes := Sender.GetSortedSelection(True);
      for i:=High(Nodes) downto Low(Nodes) do begin
        for j := Nodes[i].Index to High(DataGridResult.Rows)-1 do begin
          // Move upper rows by one so the selected row gets overwritten
          DataGridResult.Rows[j] := DataGridResult.Rows[j+1];
        end;
      end;
      SetLength(DataGridResult.Rows, Length(DataGridResult.Rows) - Selected);
      Sender.DeleteSelectedNodes;
      Sender.EndUpdate;
    end else begin
      // Should never get called as we block DELETEs on tables without a unique key
      ViewData(Sender);
      msg := 'Warning: Consistency problem detected.' + CRLF + CRLF
        + 'The last DELETE query affected ' + FormatNumber(Affected) + ' rows, when it should have touched '+FormatNumber(Selected)+' row(s)!'
        + CRLF + CRLF
        + 'This is most likely caused by not having a primary key in the table''s definition.';
      LogSQL( msg );
      MessageDlg( msg, mtWarning, [mbOK], 0);
    end;
  end;
end;


{**
  DataGrid: cancel INSERT or UPDATE mode, reset modified node data
}
procedure TMainForm.DataGridCancel(Sender: TObject);
var
  i: Integer;
begin
  case DataGridResult.Rows[DataGrid.FocusedNode.Index].State of
    grsModified: GridFinalizeEditing(DataGrid);
    grsInserted: begin
      i := Length(DataGridResult.Rows);
      DataGrid.DeleteNode(DataGrid.FocusedNode, False);
      SetLength(DataGridResult.Rows, i-1);
      // Focus+select last node if possible
      actDataLastExecute(Sender);
    end;
  end;
  DataGridHasChanges := False;
  ValidateControls(Sender);
end;



procedure TMainForm.GridKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
var
  g: TVirtualStringTree;
begin
  g := TVirtualStringTree(Sender);
  case Key of
    VK_HOME: g.FocusedColumn := 0;
    VK_END: g.FocusedColumn := g.Header.Columns.Count-1;
    VK_RETURN: if Assigned(g.FocusedNode) then g.EditNode(g.FocusedNode, g.FocusedColumn);
    VK_DOWN: if (g = DataGrid) and Assigned(g.FocusedNode) and (g.FocusedNode.Index = g.RootNodeCount-1) then
      actDataInsertExecute(Sender);
  end;
end;


// TODO: Version of EnsureFullWidth() that fetches all width limited columns
//       for a row, and fetches 500 rows at a time, for use with GridTo{Xml,Csv,Html}.
//       Would reduce number of database roundtrips; also the per-query overhead
//       right now is horrendous for some reason (thinking mysqlquerythread).
function TMainForm.EnsureFullWidth(Grid: TBaseVirtualTree; Column: TColumnIndex; Node: PVirtualNode): Boolean;
var
  Cell: PGridCell;
  Row: PGridRow;
  Col: PGridColumn;
  sql: WideString;
  len: Int64;
  ds: TDataSet;
begin
  Result := True;

  // Only the data grid uses delayed loading of full-width data.
  if Grid <> DataGrid then Exit;

  // Load entire data for field.
  Col := @DataGridResult.Columns[Column];
  Row := @DataGridResult.Rows[Node.Index];
  Cell := @DataGridResult.Rows[Node.Index].Cells[Column];
  len := Length(Cell.Text);
  // Recalculate due to textual formatting of raw binary data.
  if (Col.DatatypeCat = dtcBinary) and (len > 2) then len := (len - 2) div 2;
  // Assume width limit in effect if data exactly at limit threshold.
  if len = GridMaxData then begin
    if CheckUniqueKeyClause then begin
      sql :=
        'SELECT ' + mask(Col.Name) +
        ' FROM ' + mask(SelectedTable.Text) +
        ' WHERE ' + GetWhereClause(Row, @DataGridResult.Columns)
      ;
      ds := GetResults(sql);
      if Col.DatatypeCat = dtcBinary then Cell.Text := '0x' + BinToWideHex(ds.Fields[0].AsString)
      else Cell.Text := ds.Fields[0].AsWideString;
      Cell.IsNull := ds.Fields[0].IsNull;
    end else
      Result := False;
  end;
end;

procedure TMainForm.DataGridEditing(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
  if DataGridResult.Rows[Node.Index].State = grsDefault then
    Allowed := CheckUniqueKeyClause;
  if Allowed then begin
    // Move Esc shortcut from "Cancel row editing" to "Cancel cell editing"
    actDataCancelChanges.ShortCut := 0;
    actDataPostChanges.ShortCut := 0;
    EnsureFullWidth(Sender, Column, Node);
  end;
end;

procedure TMainForm.DataGridEdited(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex);
begin
  // Reassign Esc to "Cancel row editing" action
  if ([tsEditing, tsEditPending] * Sender.TreeStates) = [] then begin
    actDataCancelChanges.ShortCut := TextToShortcut('Esc');
    actDataPostChanges.ShortCut := TextToShortcut('Ctrl+Enter');
  end;
  AutoCalcColWidths(DataGrid, PrevTableColWidths);
end;

procedure TMainForm.DataGridEditCancelled(Sender: TBaseVirtualTree; Column:
    TColumnIndex);
begin
  // Reassign Esc to "Cancel row editing" action
  actDataCancelChanges.ShortCut := TextToShortcut('Esc');
  actDataPostChanges.ShortCut := TextToShortcut('Ctrl+Enter');
end;

procedure TMainForm.DataGridCreateEditor(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  HexEditor: THexEditorLink;
  DateTimeEditor: TDateTimeEditorLink;
  EnumEditor: TEnumEditorLink;
  SetEditor: TSetEditorLink;
  InplaceEditor: TInplaceEditorLink;
  TypeCat: TDatatypeCategoryIndex;
begin
  VT := Sender as TVirtualStringTree;
  TypeCat := DataGridResult.Columns[Column].DatatypeCat;
  if TypeCat = dtcText then begin
    InplaceEditor := TInplaceEditorLink.Create(VT);
    InplaceEditor.DataType := DataGridResult.Columns[Column].Datatype;
    InplaceEditor.MaxLength := DataGridResult.Columns[Column].MaxLength;
    InplaceEditor.ButtonVisible := True;
    EditLink := InplaceEditor;
  end else if (TypeCat = dtcBinary) and prefEnableBinaryEditor then begin
    HexEditor := THexEditorLink.Create(VT);
    HexEditor.DataType := DataGridResult.Columns[Column].Datatype;
    HexEditor.MaxLength := DataGridResult.Columns[Column].MaxLength;
    EditLink := HexEditor;
  end else if (TypeCat = dtcTemporal) and prefEnableDatetimeEditor then begin
    DateTimeEditor := TDateTimeEditorLink.Create(VT);
    DateTimeEditor.DataType := DataGridResult.Columns[Column].Datatype;
    EditLink := DateTimeEditor;
  end else if (TypeCat = dtcIntegerNamed) and prefEnableEnumEditor then begin
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.DataType := DataGridResult.Columns[Column].Datatype;
    EnumEditor.ValueList := DataGridResult.Columns[Column].ValueList;
    EditLink := EnumEditor;
  end else if (TypeCat = dtcSetNamed) and prefEnableSetEditor then begin
    SetEditor := TSetEditorLink.Create(VT);
    SetEditor.DataType := DataGridResult.Columns[Column].Datatype;
    SetEditor.ValueList := DataGridResult.Columns[Column].ValueList;
    EditLink := SetEditor;
  end else begin
    InplaceEditor := TInplaceEditorLink.Create(VT);
    InplaceEditor.DataType := DataGridResult.Columns[Column].Datatype;
    InplaceEditor.ButtonVisible := False;
    EditLink := InplaceEditor;
  end;
end;


function TMainForm.GetSelectedTableColumns: TDataset;
begin
  if (FSelectedTableColumns = nil) or (FSelectedTableColumns.State = dsInactive) then begin
    FreeAndNil(FSelectedTableColumns);
	// Avoid SQL error on routines
    if GetFocusedTreeNodeType in [lntTable, lntView] then begin
      ShowStatus('Reading table columns ...');
      FSelectedTableColumns := GetResults( 'SHOW /*!32332 FULL */ COLUMNS FROM ' + mask(SelectedTable.Text), false );
    end;
  end;
  Result := FSelectedTableColumns;
end;

function TMainForm.GetSelectedTableKeys: TDataset;
begin
  if (FSelectedTableKeys = nil) or (FSelectedTableKeys.State = dsInactive) then begin
    FreeAndNil(FSelectedTableKeys);
	// Avoid SQL error on routines
    if GetFocusedTreeNodeType in [lntTable, lntView] then begin
      ShowStatus('Reading table keys ...');
      FSelectedTableKeys := GetResults( 'SHOW KEYS FROM ' + mask(SelectedTable.Text) );
    end;
  end;
  Result := FSelectedTableKeys;
end;


procedure TMainForm.menuShowSizeColumnClick(Sender: TObject);
var
  NewVal: Boolean;
begin
  NewVal := not TMenuItem(Sender).Checked;
  TMenuItem(Sender).Checked := newVal;
  if NewVal then
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options + [coVisible]
  else
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options - [coVisible];
  OpenRegistry;
  MainReg.WriteBool(REGNAME_SIZECOL_TREE, NewVal);
end;


procedure TMainForm.AutoCalcColWidths(Tree: TVirtualStringTree; PrevLayout: Widestrings.TWideStringlist = nil);
var
  Node: PVirtualNode;
  i, j, ColTextWidth: Integer;
  Rect: TRect;
  Col: TVirtualTreeColumn;
begin
  // Find optimal default width for columns. Needs to be done late, after the SQL
  // composing to enable text width calculation based on actual table content
  Tree.BeginUpdate;
  try
    // Weird: Fixes first time calculation always based on Tahoma/8pt font
    Tree.Canvas.Font := Tree.Font;
    for i := 0 to Tree.Header.Columns.Count - 1 do begin
      Col := Tree.Header.Columns[i];
      if not (coVisible in Col.Options) then
        continue;
      if (PrevLayout <> nil) and (PrevLayout.IndexOfName(Col.Text) > -1) then begin
        Col.Width := MakeInt(PrevLayout.Values[Col.Text]);
        continue;
      end;
      ColTextWidth := Tree.Canvas.TextWidth(Tree.Header.Columns[i].Text);
      // Add space for sort glyph
      if Col.ImageIndex > -1 then
        ColTextWidth := ColTextWidth + 20;
      Node := Tree.GetFirstVisible;
      // Go backwards 50 nodes from focused one if tree was scrolled
      j := 0;
      if Assigned(Tree.FocusedNode) then begin
        Node := Tree.FocusedNode;
        while Assigned(Node) do begin
          inc(j);
          if (Node = Tree.GetFirst) or (j > 50) then
            break;
          Node := Tree.GetPreviousVisible(Node);
        end;
      end;
      j := 0;
      while Assigned(Node) do begin
        // Note: this causes the node to load, an exception can propagate
        //       here if the query or connection dies.
        Rect := Tree.GetDisplayRect(Node, i, True, True);
        ColTextWidth := Max(ColTextWidth, Rect.Right - Rect.Left);
        inc(j);
        if j > 100 then break;
        // GetDisplayRect may have implicitely taken the node away.
        // Strange that Node keeps being assigned though, probably a timing issue.
        if Tree.RootNodeCount = 0 then break;
        Node := Tree.GetNextVisible(Node);
      end;
      // text margins and minimal extra space
      ColTextWidth := ColTextWidth + Tree.TextMargin*2 + 5;
      ColTextWidth := Min(ColTextWidth, prefMaxColWidth);
      Col.Width := ColTextWidth;
    end;
  finally
    Tree.EndUpdate;
  end;
end;


procedure TMainForm.DataGridColumnResize(Sender: TVTHeader;
  Column: TColumnIndex);
var
  col: TVirtualTreeColumn;
begin
  // Avoid AVs
  if Column < 0 then
    Exit;
  // Don't waste time storing changes while a column is automatically resized
  if tsUpdating in Sender.Treeview.TreeStates then
    Exit;
  if PrevTableColWidths = nil then
    PrevTableColWidths := WideStrings.TWideStringList.Create;
  col := Sender.Columns[Column];
  PrevTableColWidths.Values[col.Text] := inttostr(col.Width);
end;


procedure TMainForm.GridBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  gr: TGridResult;
begin
  if Column = -1 then
    Exit;
  gr := GridResult(Sender);
  EnsureChunkLoaded(Sender, Node);
  if (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then begin
    if not Sender.IsEditing then begin
	  // Editors may not cover the whole cell rectangle, so any colored area looks broken then
      TargetCanvas.Brush.Color := clHighlight;
      TargetCanvas.FillRect(CellRect);
    end;
  end else if vsSelected in Node.States then begin
    TargetCanvas.Brush.Color := $0040FFFF;
    TargetCanvas.FillRect(CellRect);
  end else if prefEnableNullBG and gr.Rows[Node.Index].Cells[Column].IsNull then begin
    TargetCanvas.Brush.Color := prefNullBG;
    TargetCanvas.FillRect(CellRect);
  end;
end;


procedure TMainForm.FillDataViewPopup;
var
  i: Integer;
  DataViews: TStringList;
  mi: TMenuItem;
begin
  // Load all view names into popupmenu
  for i := popupDataView.Items.Count-1 downto 0 do begin
    if popupDataView.Items[i].Caption = '-' then
      break;
    popupDataView.Items.Delete(i);
  end;
  // Unhide "Load xyz by default" item if default is set
  menuViewDefault.Visible := False;
  OpenRegistry;
  if MainReg.OpenKey(GetRegKeyTable, False) then begin
    if MainReg.ValueExists(REGNAME_DEFAULTVIEW) then begin
      menuViewDefault.Caption := 'Load view "'+MainReg.ReadString(REGNAME_DEFAULTVIEW)+'" by default';
      menuViewDefault.Visible := True;
    end;
  end;
  // Add views
  DataViews := TStringList.Create;
  GetDataViews(DataViews);
  for i := 0 to DataViews.Count - 1 do begin
    mi := TMenuItem.Create(popupDataView);
    mi.Caption := DataViews[i];
    mi.OnClick := DataViewClick;
    popupDataView.Items.Add(mi);
  end;
  // Highlight drop down button if views are available
  if DataViews.Count = 0 then
    tbtnDataView.ImageIndex := 113
  else
    tbtnDataView.ImageIndex := 112;
end;


procedure TMainForm.popupDataViewPopup(Sender: TObject);
begin
  // Only enable "Save view" menu if any view part is set
  menuViewSave.Enabled := (FDataGridSelect.Count > 0) or
    (Length(FDataGridSort)>0) or (SynMemoFilter.GetTextLen > 0);
end;


procedure TMainForm.GetDataViews(List: TStrings);
var
  i: Integer;
begin
  // Load all view names into popupmenu
  OpenRegistry;
  if MainReg.OpenKey(GetRegKeyTable, False) then begin
    MainReg.GetKeyNames(List);
    for i := List.Count - 1 downto 0 do begin
      if Copy(List[i], 0, Length(REGPREFIX_DATAVIEW)) <> REGPREFIX_DATAVIEW then
        List.Delete(i)
      else
        List[i] := Copy(List[i], Length(REGPREFIX_DATAVIEW)+1, Length(List[i]));
    end;
  end;
end;


procedure TMainForm.menuViewSaveClick(Sender: TObject);
var
  frm: TFrmDataViewSave;
begin
  frm := TFrmDataViewSave.Create(Self);
  if frm.ShowModal = mrOK then
    FillDataViewPopup;
  frm.Free;
end;


procedure TMainForm.menuViewDefaultClick(Sender: TObject);
begin
  menuViewDefault.Visible := False;
  OpenRegistry;
  if MainReg.OpenKey(GetRegKeyTable, False) then begin
    if MainReg.ValueExists(REGNAME_DEFAULTVIEW) then
      MainReg.DeleteValue(REGNAME_DEFAULTVIEW)
  end;
end;


procedure TMainForm.DataViewClick(Sender: TObject);
begin
  LoadDataView((Sender as TMenuItem).Caption);
  ViewData(tbtnDataView);
end;


procedure TMainForm.LoadDataView(ViewName: String);
var
  rx: TRegExpr;
  idx, i: Integer;
  Col: WideString;
  HiddenCols: TWideStringList;
begin
  OpenRegistry;
  if MainReg.OpenKey(GetRegKeyTable + '\' + REGPREFIX_DATAVIEW + ViewName, False) then begin
    // Columns
    HiddenCols := TWideStringlist.Create;
    HiddenCols.Delimiter := REGDELIM;
    HiddenCols.StrictDelimiter := True;
    HiddenCols.DelimitedText := Utf8Decode(MainReg.ReadString(REGNAME_HIDDENCOLUMNS));
    SelectedTableColumns.First;
    FDataGridSelect.Clear;
    for i := 0 to SelectedTableColumns.RecordCount - 1 do begin
      Col := SelectedTableColumns.Fields[0].AsWideString;
      if HiddenCols.IndexOf(Col) = -1 then
        FDataGridSelect.Add(Col);
      SelectedTableColumns.Next;
    end;
    FreeAndNil(HiddenCols);
    // Filter
    SynMemoFilter.Text := Utf8Decode(MainReg.ReadString(REGNAME_FILTER));
    if SynMemoFilter.GetTextLen > 0 then
      ToggleFilterPanel(True);
    // Sort
    SetLength(FDataGridSort, 0);
    rx := TRegExpr.Create;
    rx.Expression := '\b(\d)_(.+)\'+REGDELIM;
    rx.ModifierG := False;
    if rx.Exec(Utf8Decode(MainReg.ReadString(REGNAME_SORT))) then while true do begin
      idx := Length(FDataGridSort);
      SetLength(FDataGridSort, idx+1);
      FDataGridSort[idx] := TOrderCol.Create;
      FDataGridSort[idx].ColumnName := rx.Match[2];
      FDataGridSort[idx].SortDirection := StrToIntDef(rx.Match[1], ORDER_ASC);
      if not rx.ExecNext then
        break;
    end;
  end;
end;


function TMainForm.GetRegKeyTable: String;
begin
  // Return the slightly complex registry path to \Servers\ThisServer\curdb|curtable
  Result := REGPATH + REGKEY_SESSIONS + SessionName + '\' +
    Utf8Encode(ActiveDatabase) + REGDELIM + Utf8Encode(SelectedTable.Text);
end;


procedure TMainForm.QueryGridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  ValidateControls(Sender);
end;


procedure TMainForm.pnlQueryHelpersCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  // Ensure minimum width for query helpers while resizing
  Resize := NewWidth >= 20;
end;


procedure TMainForm.pnlQueryMemoCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  // Ensure visibility of query memo while resizing
  Resize := NewWidth >= pnlQueryHelpers.Width + spltQueryHelpers.Width + 40;
end;


procedure TMainForm.DataGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Grid: TVirtualStringTree;
  Hit: THitInfo;
begin
  // Detect mouse hit in grid whitespace and apply changes.
  Grid := Sender as TVirtualStringTree;
  if not Assigned(Grid.FocusedNode) then
    Exit;
  Grid.GetHitTestInfoAt(X, Y, False, Hit);
  if (Hit.HitNode = nil) or (Hit.HitColumn = NoColumn) or (Hit.HitColumn = InvalidColumn) then
    DataGridPostUpdateOrInsert(Grid.FocusedNode);
end;


procedure TMainForm.ListVariablesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  i : Integer;
  vt: TVirtualStringTree;
  ds: TDataSet;
  Sel: TWideStringList;
begin
  // Display server variables
  vt := Sender as TVirtualStringTree;
  if vt.Tag <> VTREE_NOTLOADED then
    Exit;
  Sel := GetVTCaptions(vt, True);
  ResetVTNodes(vt);
  Screen.Cursor := crHourglass;
  try
    ds := GetResults('SHOW VARIABLES');
    SetLength(VTRowDataListVariables, ds.RecordCount);
    for i:=1 to ds.RecordCount do begin
      VTRowDataListVariables[i-1].ImageIndex := 25;
      VTRowDataListVariables[i-1].Captions := WideStrings.TWideStringList.Create;
      VTRowDataListVariables[i-1].Captions.Add( ds.Fields[0].AsWideString );
      VTRowDataListVariables[i-1].Captions.Add( ds.Fields[1].AsWideString );
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
    vt.RootNodeCount := Length(VTRowDataListVariables);
    vt.SortTree(vt.Header.SortColumn, vt.Header.SortDirection);
    SetVTSelection(vt, Sel);
    // Apply or reset filter
    editFilterVTChange(Sender);
    // Display number of listed values on tab
    tabVariables.Caption := 'Variables (' + IntToStr(vt.RootNodeCount) + ')';
  finally
    // Important to flag the tree as "loaded", otherwise OnPaint will cause an endless loop
    vt.Tag := VTREE_LOADED;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.ListStatusBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  i: Integer;
  valcount: Int64;
  tmpval: Double;
  ds: TDataSet;
  val, avg_perhour, avg_persec: WideString;
  valIsBytes, valIsNumber: Boolean;
  vt: TVirtualStringTree;
  Sel: TWideStringList;
begin
  // Display server status key/value pairs
  vt := Sender as TVirtualStringTree;
  if vt.Tag <> VTREE_NOTLOADED then
    Exit;
  Sel := GetVTCaptions(vt, True);
  ResetVTNodes(vt);
  Screen.Cursor := crHourglass;
  try
    ds := GetResults( 'SHOW /*!50002 GLOBAL */ STATUS' );
    SetLength(VTRowDataListStatus, ds.RecordCount);
    for i:=1 to ds.RecordCount do begin
      VTRowDataListStatus[i-1].ImageIndex := 25;
      VTRowDataListStatus[i-1].Captions := WideStrings.TWideStringList.Create;
      VTRowDataListStatus[i-1].Captions.Add( ds.Fields[0].AsWideString );
      val := ds.Fields[1].AsWideString;
      avg_perhour := '';
      avg_persec := '';

      // Detect value type
      valIsNumber := IntToStr(MakeInt(val)) = val;
      valIsBytes := valIsNumber and (Copy(ds.Fields[0].AsWideString, 1, 6) = 'Bytes_');

      // Calculate average values ...
      if valIsNumber then begin
        valCount := MakeInt(val);
        // ... per hour
        tmpval := valCount / ( ServerUptime / 60 / 60 );
        if valIsBytes then avg_perhour := FormatByteNumber( Trunc(tmpval) )
        else avg_perhour := FormatNumber( tmpval, 1 );
        // ... per second
        tmpval := valCount / ServerUptime;
        if valIsBytes then avg_persec := FormatByteNumber( Trunc(tmpval) )
        else avg_persec := FormatNumber( tmpval, 1 );
      end;

      // Format numeric or byte values
      if valIsBytes then
        val := FormatByteNumber(val)
      else if valIsNumber then
        val := FormatNumber(val);

      VTRowDataListStatus[i-1].Captions.Add( val );
      VTRowDataListStatus[i-1].Captions.Add(avg_perhour);
      VTRowDataListStatus[i-1].Captions.Add(avg_persec);
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
    // Tell VirtualTree the number of nodes it will display
    vt.RootNodeCount := Length(VTRowDataListStatus);
    vt.SortTree(vt.Header.SortColumn, vt.Header.SortDirection);
    SetVTSelection(vt, Sel);
    // Apply or reset filter
    editFilterVTChange(Sender);
    // Display number of listed values on tab
    tabStatus.Caption := 'Status (' + IntToStr(vt.RootNodeCount) + ')';
  finally
    vt.Tag := VTREE_LOADED;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.ListProcessesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  i, j: Integer;
  ds: TDataSet;
  vt: TVirtualStringTree;
  Sel: TWideStringList;
begin
  // Display client threads
  vt := Sender as TVirtualStringTree;
  if vt.Tag <> VTREE_NOTLOADED then
    Exit;
  Sel := GetVTCaptions(vt, True);
  ResetVTNodes(vt);
  Screen.Cursor := crHourglass;
  try
    ds := GetResults('SHOW FULL PROCESSLIST', false, false);
    SetLength(VTRowDataListProcesses, ds.RecordCount);
    for i:=1 to ds.RecordCount do begin
      VTRowDataListProcesses[i-1].Captions := WideStrings.TWideStringList.Create;
      VTRowDataListProcesses[i-1].Captions.Add( ds.Fields[0].AsWideString );
      if AnsiCompareText( ds.Fields[4].AsString, 'Killed') = 0 then
        VTRowDataListProcesses[i-1].ImageIndex := 26  // killed
      else begin
        if ds.FindField('Info').AsString = '' then
          VTRowDataListProcesses[i-1].ImageIndex := 55 // idle
        else
          VTRowDataListProcesses[i-1].ImageIndex := 57 // running query
      end;
      for j := 1 to 7 do
        VTRowDataListProcesses[i-1].Captions.Add(ds.Fields[j].AsWideString);
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
    vt.RootNodeCount := Length(VTRowDataListProcesses);
    vt.SortTree(vt.Header.SortColumn, vt.Header.SortDirection);
    SetVTSelection(vt, Sel);
    // Apply or reset filter
    editFilterVTChange(Sender);
    // Display number of listed values on tab
    tabProcessList.Caption := 'Process-List (' + IntToStr(vt.RootNodeCount) + ')';
  except
    on E: Exception do begin
      LogSQL('Error loading process list (automatic refresh disabled): ' + e.Message);
      TimerRefresh.Enabled := false;
    end;
  end;
  vt.Tag := VTREE_LOADED;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.ListCommandStatsBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
  procedure addLVitem( idx: Integer; caption: WideString; commandCount: Int64; totalCount: Int64 );
  var
    tmpval : Double;
  begin
    VTRowDataListCommandStats[idx].ImageIndex := 25;
    VTRowDataListCommandStats[idx].Captions := WideStrings.TWideStringList.Create;
    caption := Copy( caption, 5, Length(caption) );
    caption := WideStringReplace( caption, '_', ' ', [rfReplaceAll] );
    VTRowDataListCommandStats[idx].Captions.Add( caption );
    // Total Frequency
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( commandCount ) );
    // Average per hour
    tmpval := commandCount / ( ServerUptime / 60 / 60 );
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( tmpval, 1 ) );
    // Average per second
    tmpval := commandCount / ServerUptime;
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( tmpval, 1 ) );
    // Percentage. Take care of division by zero errors and Int64's
    if commandCount < 1 then
      commandCount := 1;
    if totalCount < 1 then
      totalCount := 1;
    tmpval := 100 / totalCount * commandCount;
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( tmpval, 1 ) + ' %' );
  end;

var
  i: Integer;
  questions: Int64;
  ds: TDataSet;
  vt: TVirtualStringTree;
  Sel: TWideStringList;
begin
  // Display command statistics
  vt := Sender as TVirtualStringTree;
  if vt.Tag <> VTREE_NOTLOADED then
    Exit;

  Sel := GetVTCaptions(vt, True);
  ResetVTNodes(vt);
  Screen.Cursor := crHourglass;
  try
    ds := GetResults('SHOW /*!50002 GLOBAL */ STATUS LIKE ''Com\_%''' );
    questions := MakeInt(GetVar('SHOW /*!50002 GLOBAL */ STATUS LIKE ''Questions''', 1));
    if questions = 0 then
      Raise Exception.Create('Could not detect value of "Questions" status. Command statistics are not available.');
    SetLength(VTRowDataListCommandStats, ds.RecordCount+1);
    addLVitem(0, '    All commands', questions, questions );
    for i:=1 to ds.RecordCount do begin
      addLVitem(i, ds.Fields[0].AsWideString, MakeInt(ds.Fields[1].AsString), questions );
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
    // Tell VirtualTree the number of nodes it will display
    vt.RootNodeCount := Length(VTRowDataListCommandStats);
    vt.SortTree(vt.Header.SortColumn, vt.Header.SortDirection);
    SetVTSelection(vt, Sel);
    // Apply or reset filter
    editFilterVTChange(Sender);
    // Display number of listed values on tab
    tabCommandStats.Caption := 'Command-Statistics (' + IntToStr(vt.RootNodeCount) + ')';
  finally
    vt.Tag := VTREE_LOADED;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actCopyOrCutExecute(Sender: TObject);
var
  Control: TWinControl;
  Edit: TCustomEdit;
  Grid: TVirtualStringTree;
  SynMemo: TSynMemo;
  Success, DoCut: Boolean;
begin
  // Copy text from a focused control to clipboard
  Success := False;
  Control := Screen.ActiveControl;
  // Do not handle Search/replace dialog
  if not Control.Focused then Exit;
  DoCut := Sender = actCut;
  if Control is TCustomEdit then begin
    Edit := TCustomEdit(Control);
    if Edit.SelLength > 0 then begin
      if DoCut then Edit.CutToClipboard
      else Edit.CopyToClipboard;
      Success := True;
    end;
  end else if Control is TVirtualStringTree then begin
    Grid := Control as TVirtualStringTree;
    if Assigned(Grid.FocusedNode) then begin
      if Grid = ActiveGrid then
        EnsureFullWidth(Grid, Grid.FocusedColumn, Grid.FocusedNode);
      CopyToClipboard(Grid.Text[Grid.FocusedNode, Grid.FocusedColumn]);
      if (Grid = ActiveGrid) and DoCut then
        Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := '';
      Success := True;
    end;
  end else if Control is TSynMemo then begin
    SynMemo := Control as TSynMemo;
    if SynMemo.SelAvail then begin
      if DoCut then SynMemo.CutToClipboard
      else SynMemo.CopyToClipboard;
      Success := True;
    end;
  end;
  if not Success then
    MessageBeep(MB_ICONASTERISK);
end;


procedure TMainForm.actPasteExecute(Sender: TObject);
var
  Control: TWinControl;
  Edit: TCustomEdit;
  Grid: TVirtualStringTree;
  SynMemo: TSynMemo;
  Success: Boolean;
  CB: TUniClipboard;
begin
  // Paste text into the focused control
  Success := False;
  Control := Screen.ActiveControl;
  // Do not handle Search/replace dialog
  if not Control.Focused then Exit;
  if not Clipboard.HasFormat(CF_TEXT) then begin
    // Do nothing, we cannot paste a picture or so
  end else if Control is TCustomEdit then begin
    Edit := TCustomEdit(Control);
    if not Edit.ReadOnly then begin
      Edit.PasteFromClipboard;
      Success := True;
    end;
  end else if Control is TVirtualStringTree then begin
    Grid := Control as TVirtualStringTree;
    if Assigned(Grid.FocusedNode) and (Grid = ActiveGrid) then begin
      CB := TUniClipboard.Create;
      Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := CB.AsWideString;
      Success := True;
    end;
  end else if Control is TSynMemo then begin
    SynMemo := TSynMemo(Control);
    if not SynMemo.ReadOnly then begin
      SynMemo.PasteFromClipboard;
      Success := True;
    end;
  end;
  if not Success then
    MessageBeep(MB_ICONASTERISK);
end;


procedure TMainForm.actSelectAllExecute(Sender: TObject);
var
  Control: TWinControl;
  Grid: TVirtualStringTree;
  ListBox: TTNTListBox;
  Success: Boolean;
begin
  // Select all items, text or whatever
  Success := False;
  Control := Screen.ActiveControl;
  // Do not handle Search/replace dialog
  if not Control.Focused then Exit;
  if Control is TCustomEdit then begin
    TCustomEdit(Control).SelectAll;
    Success := True;
  end else if Control is TVirtualStringTree then begin
    Grid := TVirtualStringTree(Control);
    if toMultiSelect in Grid.TreeOptions.SelectionOptions then begin
      Grid.SelectAll(False);
      Success := True;
    end;
  end else if Control is TSynMemo then begin
    TSynMemo(Control).SelectAll;
    Success := True;
  end else if Control is TTNTListBox then begin
    ListBox := TTNTListBox(Control);
    if ListBox.MultiSelect then begin
      ListBox.SelectAll;
      Success := True;
    end;
  end;
  if not Success then
    MessageBeep(MB_ICONASTERISK);
end;


procedure TMainForm.EnumerateRecentFilters;
var
  flt: TStringList;
  i: Integer;
  item: TMenuItem;
  rx: TRegExpr;
  capt: String;
begin
  // Reset menu and combobox
  menuRecentFilters.Enabled := False;
  for i := menuRecentFilters.Count - 1 downto 0 do
    menuRecentFilters.Delete(i);
  comboRecentFilters.Items.Clear;
  // Enumerate recent filters from registry
  if MainReg.OpenKey(GetRegKeyTable+'\'+REGNAME_FILTERS, False) then begin
    flt := TStringList.Create;
    rx := TRegExpr.Create;
    rx.Expression := '\s+';
    MainReg.GetValueNames(flt);
    for i := 0 to flt.Count - 1 do begin
      item := TMenuItem.Create(popupFilter);
      capt := MainReg.ReadString(flt[i]);
      capt := rx.Replace(capt, ' ', True);
      item.Hint := capt;
      item.Caption := sstr(capt, 50);
      item.Tag := MakeInt(flt[i]);
      item.OnClick := LoadRecentFilter;
      menuRecentFilters.Add(item);
      capt := Utf8Decode(capt);
      comboRecentFilters.Items.Add(sstr(capt, 100));
    end;
    FreeAndNil(rx);
    FreeAndNil(flt);
    menuRecentFilters.Enabled := menuRecentFilters.Count > 0;
  end;
  comboRecentFilters.Visible := comboRecentFilters.Items.Count > 0;
  lblRecentFilters.Visible := comboRecentFilters.Visible;
  SynMemoFilter.Height := pnlFilter.Height - 3;
  SynMemoFilter.Top := comboRecentFilters.Top;
  if comboRecentFilters.Visible then begin
    SynMemoFilter.Height := SynMemoFilter.Height - comboRecentFilters.Height;
    SynMemoFilter.Top := SynMemoFilter.Top + comboRecentFilters.Height;
    comboRecentFilters.ItemIndex := 0;
  end;
end;


procedure TMainForm.LoadRecentFilter(Sender: TObject);
var
  key: Integer;
begin
  // Event handler for both dynamic popup menu items and filter combobox
  if Sender is TMenuItem then
    key := (Sender as TMenuItem).Tag
  else
    key := (Sender as TTNTComboBox).ItemIndex+1;
  if MainReg.OpenKey(GetRegKeyTable+'\'+REGNAME_FILTERS, False) then begin
    SynMemoFilter.UndoList.AddGroupBreak;
    SynMemoFilter.BeginUpdate;
    SynMemoFilter.SelectAll;
    SynMemoFilter.SelText := Utf8Decode( MainReg.ReadString(IntToStr(key)) );
    SynMemoFilter.EndUpdate;
  end;
end;


procedure TMainForm.actCreateRoutineExecute(Sender: TObject);
begin
  tabEditor.TabVisible := True;
  PagecontrolMain.ActivePage := tabEditor;
  PlaceObjectEditor(lntProcedure);
  RoutineEditor.Init;
end;


procedure TMainForm.DataGridScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
var
  query: String;
  count: Int64;
begin
  // If the user moves the scrollbar all the way to the bottom of the data grid,
  // for example by pressing CTRL+END, jump to the bottom of table data.
  if ReachedEOT then Exit;
  if tsThumbTracking in Sender.TreeStates then Exit;
  if Int64(- Sender.OffsetY - DeltaY) < Int64(Sender.RootNode.TotalHeight) then Exit;

  // First, figure out how many rows the table contains.
  ShowStatus('Counting rows...');
  query := 'SELECT COUNT(*)' + DataGridCurrentFrom;
  if DataGridCurrentFilter <> '' then query := query + ' WHERE ' + DataGridCurrentFilter;
  try
    count := MakeInt(GetVar(query));
    // Work around a memory allocation bug in VirtualTree.
    if count > prefMaxTotalRows then count := prefMaxTotalRows;
  except
    on E: Exception do begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;
  ShowStatus(STATUS_MSG_READY);

  // Then, adjust the data grid and data containers.
  debug('mem: initializing browse rows (internal data).');
  try
    SetLength(DataGridResult.Rows, count);
    debug('mem: initializing browse rows (grid).');
    DataGrid.RootNodeCount := count;
    ReachedEOT := True;
    DisplayRowCountStats(count);
  except
    DataGrid.RootNodeCount := 0;
    SetLength(DataGridResult.Rows, 0);
    PageControlMain.ActivePage := tabDatabase;
    raise;
  end;

  // Finally, jump to the last row.
  Sender.ScrollIntoView(Sender.GetLast, False);
end;


procedure TMainForm.DBtreeExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // Auto resize "Size" column in dbtree when needed
  // See also OnInitChildren
  if coVisible in DBtree.Header.Columns[1].Options then
    DBtree.Header.AutoFitColumns(False, smaUseColumnOption, 1, 1);
end;


function TMainform.GetCollations(Items: TWideStrings = nil): TDataset;
begin
  // Return cached collation list, used in several places, e.g. table editor
  if (dsCollations = nil) or (dsCollations.State = dsInactive) then begin
    FreeAndNil(dsCollations);
    dsCollations := GetResults('SHOW COLLATION', True);
  end;
  if Assigned(dsCollations) then begin
    dsCollations.First;
    if Assigned(Items) then begin
      while not dsCollations.Eof do begin
        Items.Add(dsCollations.FieldByName('Collation').AsWideString);
        dsCollations.Next;
      end;
      dsCollations.First;
    end;
  end;
  Result := dsCollations;
end;


procedure TMainForm.PlaceObjectEditor(Which: TListNodeType);
var
  frm: TFrame;
begin
  // Place the relevant editor frame onto the editor tab, hide all others
  if (not (Which in [lntTable, lntCrashedTable])) and Assigned(TableEditor) then
    FreeAndNil(TableEditor);
  if (Which <> lntView) and Assigned(ViewEditor) then
    FreeAndNil(ViewEditor);
  if (not (Which in [lntProcedure, lntFunction])) and Assigned(RoutineEditor) then
    FreeAndNil(RoutineEditor);
  if Which in [lntTable, lntCrashedTable] then begin
    if not Assigned(TableEditor) then
      TableEditor := TfrmTableEditor.Create(tabEditor);
    frm := TableEditor;
  end else if Which = lntView then begin
    if not Assigned(ViewEditor) then
      ViewEditor := TfrmView.Create(tabEditor);
    frm := ViewEditor;
  end else if Which in [lntProcedure, lntFunction] then begin
    if not Assigned(RoutineEditor) then
      RoutineEditor := TfrmRoutineEditor.Create(tabEditor);
    frm := RoutineEditor;
  end else
    Exit;
  frm.Parent := tabEditor;
end;


procedure TMainForm.SetEditorTabCaption(Editor: TFrame; ObjName: WideString);
var
  ObjType, Cap: WideString;
  IconIndex: Integer;
begin
  if Editor = TableEditor then begin
    ObjType := 'Table';
    IconIndex := ICONINDEX_TABLE;
  end else if Editor = ViewEditor then begin
    ObjType := 'View';
    IconIndex := ICONINDEX_VIEW;
  end else if Editor = RoutineEditor then begin
    ObjType := 'Routine';
    IconIndex := ICONINDEX_STOREDPROCEDURE;
  end else
    Exit;
  tabEditor.ImageIndex := IconIndex;
  Cap := ObjType+': ';
  if ObjName = '' then
    Cap := Cap + '[Untitled]'
  else
    Cap := sstr(Cap + ObjName, 30);
  tabEditor.Caption := Cap;
  FixQueryTabCloseButtons;
end;


procedure TMainForm.actEditObjectExecute(Sender: TObject);
var
  NodeData: PVTreeData;
  RoutineType: String;
begin
  debug('actEditObjectExecute()');
  if ListTables.Focused then begin
    // Got here from ListTables.OnDblClick or ListTables's context menu item "Edit"
    NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
    if (NodeData.Captions[0] <> SelectedTable.Text) or (NodeData.NodeType <> SelectedTable.NodeType) then
      SelectDBObject(NodeData.Captions[0], NodeData.NodeType);
  end;

  case GetFocusedTreeNodeType of
    lntDb: begin
      if CreateDatabaseForm = nil then
        CreateDatabaseForm := TCreateDatabaseForm.Create(Self);
      CreateDatabaseForm.modifyDB := ActiveDatabase;
      CreateDatabaseForm.ShowModal;
    end;

    lntTable, lntCrashedTable: begin
      PlaceObjectEditor(SelectedTable.NodeType);
      TableEditor.Init(SelectedTable.Text);
    end;

    lntView: begin
      PlaceObjectEditor(SelectedTable.NodeType);
      ViewEditor.Init(SelectedTable.Text);
    end;

    lntFunction, lntProcedure: begin
      PlaceObjectEditor(SelectedTable.NodeType);
      if SelectedTable.NodeType = lntFunction then
        RoutineType := 'FUNCTION'
      else
        RoutineType := 'PROCEDURE';
      RoutineEditor.Init(SelectedTable.Text, RoutineType);
    end;

  end;
end;


procedure TMainForm.ListTablesDblClick(Sender: TObject);
var
  NodeData: PVTreeData;
begin
  // DoubleClick: Display editor
  debug('ListTablesDblClick()');
  if Assigned(ListTables.FocusedNode) then begin
    NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
    SelectDBObject(ListTables.Text[ListTables.FocusedNode, ListTables.FocusedColumn], NodeData.NodeType);
  end;
end;


procedure TMainform.ResetSelectedTableStuff;
begin
  // Free selected table's cached column and key list
  FreeAndNil(FSelectedTableColumns);
  FreeAndNil(FSelectedTableKeys);
end;


procedure TMainForm.actNewQueryTabExecute(Sender: TObject);
var
  tab: TTabSheet;
  i, SharedTag: Integer;
  CloseButton: TPngSpeedButton;
  New_pnlQueryMemo: TPanel;
  New_pnlQueryHelpers: TPanel;
  New_lboxQueryHelpers: TTntListBox;
  New_tabsetQueryHelpers: TTabSet;
  New_SynMemoQuery: TSynMemo;
  New_spltQueryHelpers: TSplitter;
  New_spltQuery: TSplitter;
  New_LabelResultInfo: TLabel;
  New_QueryGrid: TVirtualStringTree;
begin
  // In order to find the matching close button to a tab in FixQueryTabCloseButtons
  // we give both (tab + button) the same .Tag property
  SharedTag := 1;
  for i:=tabQuery.PageIndex to PageControlMain.PageCount-1 do
    SharedTag := Max(SharedTag, PageControlMain.Pages[i].Tag);
  Inc(SharedTag);
  tab := TTabSheet.Create(PageControlMain);
  tab.PageControl := PageControlMain;
  tab.Tag := SharedTag;
  tab.Caption := tabQuery.Caption + ' #'+IntToStr(SharedTag)+'      ';
  tab.ImageIndex := tabQuery.ImageIndex;

  FGridResults.Add(TGridResult.Create);

  CloseButton := TPngSpeedButton.Create(tab);
  CloseButton.Parent := PageControlMain;
  CloseButton.Tag := SharedTag;
  CloseButton.Width := 16;
  CloseButton.Height := 16;
  CloseButton.Flat := True;
  CloseButton.PngImage := PngImageListMain.PngImages[134].PngImage;
  CloseButton.OnMouseUp := CloseButtonOnMouseUp;
  FixQueryTabCloseButtons;

  // Dumb code which replicates all controls from tabQuery
  New_pnlQueryMemo := TPanel.Create(tab);
  New_pnlQueryMemo.Parent := tab;
  New_pnlQueryMemo.Tag := pnlQueryMemo.Tag;
  New_pnlQueryMemo.BevelOuter := pnlQueryMemo.BevelOuter;
  New_pnlQueryMemo.Align := pnlQueryMemo.Align;

  New_SynMemoQuery := TSynMemo.Create(New_pnlQueryMemo);
  New_SynMemoQuery.Parent := New_pnlQueryMemo;
  New_SynMemoQuery.Tag := SynMemoQuery.Tag;
  New_SynMemoQuery.Align := SynMemoQuery.Align;
  New_SynMemoQuery.Options := SynMemoQuery.Options;
  New_SynMemoQuery.PopupMenu := SynMemoQuery.PopupMenu;
  New_SynMemoQuery.TabWidth := SynMemoQuery.TabWidth;
  New_SynMemoQuery.RightEdge := SynMemoQuery.RightEdge;
  New_SynMemoQuery.WantTabs := SynMemoQuery.WantTabs;
  New_SynMemoQuery.Highlighter := SynMemoQuery.Highlighter;
  New_SynMemoQuery.SearchEngine := SynMemoQuery.SearchEngine;
  New_SynMemoQuery.Gutter.Assign(SynMemoQuery.Gutter);
  New_SynMemoQuery.Font.Assign(SynMemoQuery.Font);
  New_SynMemoQuery.ActiveLineColor := SynMemoQuery.ActiveLineColor;
  New_SynMemoQuery.OnDragDrop := SynMemoQuery.OnDragDrop;
  New_SynMemoQuery.OnDragOver := SynMemoQuery.OnDragOver;
  New_SynMemoQuery.OnDropFiles := SynMemoQuery.OnDropFiles;
  New_SynMemoQuery.OnStatusChange := SynMemoQuery.OnStatusChange;
  SynCompletionProposal1.AddEditor(New_SynMemoQuery);

  New_spltQueryHelpers := TSplitter.Create(New_pnlQueryMemo);
  New_spltQueryHelpers.Parent := New_pnlQueryMemo;
  New_spltQueryHelpers.Tag := spltQueryHelpers.Tag;
  New_spltQueryHelpers.Align := spltQueryHelpers.Align;
  New_spltQueryHelpers.Cursor := spltQueryHelpers.Cursor;
  New_spltQueryHelpers.ResizeStyle := spltQueryHelpers.ResizeStyle;
  New_spltQueryHelpers.Width := spltQueryHelpers.Width;

  New_pnlQueryHelpers := TPanel.Create(New_pnlQueryMemo);
  New_pnlQueryHelpers.Parent := New_pnlQueryMemo;
  New_pnlQueryHelpers.Tag := pnlQueryHelpers.Tag;
  New_pnlQueryHelpers.BevelOuter := pnlQueryHelpers.BevelOuter;
  New_pnlQueryHelpers.Align := pnlQueryHelpers.Align;

  New_lboxQueryHelpers := TTntListBox.Create(New_pnlQueryHelpers);
  New_lboxQueryHelpers.Parent := New_pnlQueryHelpers;
  New_lboxQueryHelpers.Tag := lboxQueryHelpers.Tag;
  New_lboxQueryHelpers.Align := lboxQueryHelpers.Align;
  New_lboxQueryHelpers.PopupMenu := lboxQueryHelpers.PopupMenu;
  New_lboxQueryHelpers.MultiSelect := lboxQueryHelpers.MultiSelect;
  New_lboxQueryHelpers.DragMode := lboxQueryHelpers.DragMode;
  New_lboxQueryHelpers.Font.Assign(lboxQueryHelpers.Font);
  New_lboxQueryHelpers.OnClick := lboxQueryHelpers.OnClick;
  New_lboxQueryHelpers.OnDblClick := lboxQueryHelpers.OnDblClick;

  New_tabsetQueryHelpers := TTabSet.Create(New_pnlQueryHelpers);
  New_tabsetQueryHelpers.Parent := New_pnlQueryHelpers;
  New_tabsetQueryHelpers.Tag := tabsetQueryHelpers.Tag;
  New_tabsetQueryHelpers.Height := tabsetQueryHelpers.Height;
  New_tabsetQueryHelpers.Align := tabsetQueryHelpers.Align;
  New_tabsetQueryHelpers.Tabs := tabsetQueryHelpers.Tabs;
  New_tabsetQueryHelpers.Style := tabsetQueryHelpers.Style;
  New_tabsetQueryHelpers.Font.Assign(tabsetQueryHelpers.Font);
  New_tabsetQueryHelpers.OnChange := tabsetQueryHelpers.OnChange;

  New_spltQuery := TSplitter.Create(tab);
  New_spltQuery.Parent := tab;
  New_spltQuery.Tag := spltQuery.Tag;
  New_spltQuery.Align := spltQuery.Align;
  New_spltQuery.Height := spltQuery.Height;
  New_spltQuery.Cursor := spltQuery.Cursor;
  New_spltQuery.ResizeStyle := spltQuery.ResizeStyle;

  New_LabelResultInfo := TLabel.Create(tab);
  New_LabelResultInfo.Parent := tab;
  New_LabelResultInfo.Tag := LabelResultInfo.Tag;
  New_LabelResultInfo.Align := LabelResultInfo.Align;
  New_LabelResultInfo.Font.Assign(LabelResultInfo.Font);
  New_LabelResultInfo.Caption := '';

  New_QueryGrid := TVirtualStringTree.Create(tab);
  New_QueryGrid.Parent := tab;
  New_QueryGrid.Tag := QueryGrid.Tag;
  New_QueryGrid.Align := QueryGrid.Align;
  New_QueryGrid.TreeOptions := QueryGrid.TreeOptions;
  New_QueryGrid.PopupMenu := QueryGrid.PopupMenu;
  New_QueryGrid.LineStyle := QueryGrid.LineStyle;
  New_QueryGrid.Font.Assign(QueryGrid.Font);
  New_QueryGrid.Header.ParentFont := QueryGrid.Header.ParentFont;
  New_QueryGrid.WantTabs := QueryGrid.WantTabs;
  New_QueryGrid.OnBeforeCellPaint := QueryGrid.OnBeforeCellPaint;
  New_QueryGrid.OnFocusChanged := QueryGrid.OnFocusChanged;
  New_QueryGrid.OnGetText := QueryGrid.OnGetText;
  New_QueryGrid.OnKeyDown := QueryGrid.OnKeyDown;
  New_QueryGrid.OnPaintText := QueryGrid.OnPaintText;
  FixVT(New_QueryGrid);

  // Set splitter positions
  New_pnlQueryMemo.Height := pnlQueryMemo.Height;
  New_pnlQueryMemo.Top := pnlQueryMemo.Top;
  New_spltQuery.Top := spltQuery.Top;
  New_pnlQueryHelpers.Width := pnlQueryHelpers.Width;

  // Show new tab
  PageControlMain.ActivePage := tab;
  PageControlMainChange(Sender);
end;


procedure TMainForm.panelTopDblClick(Sender: TObject);
var
  aRect: TRect;
  aPoint: TPoint;
begin
  // Catch doubleclick on PageControlMain's underlying panel, which gets fired
  // when user clicks right besides the visible tabs
  aPoint := PageControlMain.ClientOrigin;
  aRect := Rect(aPoint.X, aPoint.Y, aPoint.X + PageControlMain.Width, aPoint.Y + PageControlMain.Height - tabQuery.Height);
  GetCursorPos(aPoint);
  if PtInRect(aRect, aPoint) then
    actNewQueryTab.Execute;
end;


procedure TMainForm.actCloseQueryTabExecute(Sender: TObject);
begin
  // Close active query tab by main action
  CloseQueryTab(PageControlMain.ActivePageIndex);
end;


procedure TMainForm.menuCloseQueryTab(Sender: TObject);
var
  aPoint: TPoint;
begin
  // Close query tab by menu item
  aPoint := PageControlMain.ScreenToClient(popupMainTabs.PopupPoint);
  CloseQueryTab(GetMainTabAt(aPoint.X, aPoint.Y));
end;


procedure TMainForm.popupMainTabsPopup(Sender: TObject);
var
  aPoint: TPoint;
  PageIndex: Integer;
begin
  // Detect if there is a tab under mouse position
  aPoint := PageControlMain.ScreenToClient(popupMainTabs.PopupPoint);
  PageIndex := GetMainTabAt(aPoint.X, aPoint.Y);
  menuCloseTab.Enabled := IsQueryTab(PageIndex, False);
end;


procedure TMainForm.CloseQueryTab(PageIndex: Integer);
var
  NewPageIndex: Integer;
begin
  if not IsQueryTab(PageIndex, False) then
    Exit;
  FGridResults.Delete(PageIndex-tabData.PageIndex);
  // Work around bugs in ComCtrls.TPageControl.RemovePage
  NewPageIndex := PageControlMain.ActivePageIndex;
  if NewPageIndex >= PageIndex then
    Dec(NewPageIndex);
  // Avoid excessive flicker:
  LockWindowUpdate(PageControlMain.Handle);
  PageControlMain.Pages[PageIndex].Free;
  PageControlMain.ActivePageIndex := NewPageIndex;
  FixQueryTabCloseButtons;
  LockWindowUpdate(0);
  PageControlMain.OnChange(PageControlMain);
end;


procedure TMainForm.CloseButtonOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aPoint: TPoint;
begin
  // Click on "Close" button of Query tab
  if Button <> mbLeft then
    Exit;
  aPoint := PageControlMain.ScreenToClient((Sender as TPngSpeedButton).ClientToScreen(Point(X,Y)));
  CloseQueryTab(GetMainTabAt(aPoint.X, aPoint.Y));
end;


procedure TMainForm.PageControlMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurTickcount: Cardinal;
  TabNumber: Integer;
begin
  // Simulate doubleclick on tab to close it
  CurTickcount := GetTickCount;
  TabNumber := GetMainTabAt(X, Y);
  if (TabNumber = FLastTabNumberOnMouseUp)
    and (CurTickcount - FLastMouseUpOnPageControl <= GetDoubleClickTime) then
    CloseQueryTab(TabNumber)
  else begin
    FLastMouseUpOnPageControl := CurTickcount;
    FLastTabNumberOnMouseUp := TabNumber;
  end;
end;


function TMainForm.GetMainTabAt(X, Y: Integer): Integer;
var
  i: Integer;
begin
  // Return page index of main tab by coordinates
  Result := PageControlMain.IndexOfTabAt(X, Y);
  for i:=0 to PageControlMain.PageCount-1 do begin
    if (i<=Result) and (not PageControlMain.Pages[i].TabVisible) then
      Inc(Result);
  end;
end;


procedure TMainForm.FixQueryTabCloseButtons;
var
  i, PageIndex, VisiblePageIndex: Integer;
  Rect: TRect;
  btn: TPngSpeedButton;
begin
  // Fix positions of "Close" buttons on Query tabs
  LockWindowUpdate(PageControlMain.Handle);
  for PageIndex:=tabQuery.PageIndex+1 to PageControlMain.PageCount-1 do begin
    VisiblePageIndex := PageIndex;
    for i:=0 to PageControlMain.PageCount-1 do begin
      if (i<=VisiblePageIndex) and (not PageControlMain.Pages[i].TabVisible) then
        Dec(VisiblePageIndex);
    end;
    Rect := PageControlMain.TabRect(VisiblePageIndex);
    btn := QueryTabCloseButton(PageIndex);
    btn.Top := Rect.Top + 2;
    btn.Left := Rect.Right - 19;
  end;
  // Set position of "Add tab" button
  VisiblePageIndex := PageControlMain.PageCount-1;
  for i:=0 to PageControlMain.PageCount-1 do begin
    if not PageControlMain.Pages[i].TabVisible then
      Dec(VisiblePageIndex);
  end;
  Rect := PageControlMain.TabRect(VisiblePageIndex);
  btnAddTab.Top := Rect.Top;
  btnAddTab.Left := Rect.Right + 5;

  LockWindowUpdate(0);
end;


function TMainForm.QueryTabCloseButton(PageIndex: Integer): TPngSpeedButton;
var
  i: Integer;
begin
  // Return close button of given query tab
  Result := nil;
  for i:=0 to PageControlMain.ControlCount-1 do begin
    if not (PageControlMain.Controls[i] is TPngSpeedButton) then
      continue;
    if PageControlMain.Controls[i].Tag = PageControlMain.Pages[PageIndex].Tag then begin
      Result := PageControlMain.Controls[i] as TPngSpeedButton;
      Break;
    end;
  end;
  if not Assigned(Result) then
    Raise Exception.Create('Couldn''t find close button on query tab #'+IntToStr(PageIndex));
end;


function TMainForm.QueryControl(PageIndex: Integer; Base: TControl): TControl;
  // Find control reference on a given query tab
  procedure FindChild(Parent: TWinControl);
  var
    i: Integer;
  begin
    if Assigned(Result) then
      Exit;
    for i:=0 to Parent.ControlCount-1 do begin
      if Parent.Controls[i].Tag = Base.Tag then begin
        Result := Parent.Controls[i];
        break;
      end;
      if Parent.Controls[i] is TPanel then
        FindChild(TWinControl(Parent.Controls[i]));
    end;
  end;
begin
  if not IsQueryTab(PageIndex, True) then
    Raise Exception.Create(PageControlMain.Pages[PageIndex].Name+' is not a Query tab.');
  Result := nil;
  FindChild(PageControlMain.Pages[PageIndex]);
  if not Assigned(Result) then
    Raise Exception.Create('Couln''t find the matching component of '+Base.Name+' on Query tab #'+IntToStr(PageIndex));
end;


function TMainForm.ActiveQueryControl(Base: TControl): TControl;
begin
  // Return component reference on current Query tab
  Result := QueryControl(PageControlMain.ActivePageIndex, Base);
end;


function TMainForm.ActiveQueryMemo: TSynMemo;
begin
  // Return current query memo
  Result := ActiveQueryControl(SynMemoQuery) as TSynMemo;
end;


function TMainForm.ActiveQueryHelpers: TTntListBox;
begin
  // Return current query helpers listbox
  Result := ActiveQueryControl(lboxQueryHelpers) as TTntListBox;
end;


function TMainForm.ActiveQueryTabset: TTabset;
begin
  // Return current query helpers tabset
  Result := ActiveQueryControl(tabsetQueryHelpers) as TTabset;
end;


function TMainForm.ActiveGrid: TVirtualStringTree;
begin
  if PageControlMain.ActivePage = tabData then Result := DataGrid
  else Result := ActiveQueryControl(QueryGrid) as TVirtualStringTree;
end;


function TMainForm.GridResult(Grid: TBaseVirtualTree): TGridResult;
begin
  // All grids (data- and query-grids) are placed directly on a TTabSheet
  Result := GridResult((Grid.Parent as TTabSheet).PageIndex)
end;


function TMainForm.GridResult(PageIndex: Integer): TGridResult;
begin
  // Return the grid result for "Data" or one of the "Query" tabs.
  // Results are enumerated like the tabs on which they get displayed, starting at tabData
  Dec(PageIndex, tabData.PageIndex);
  if PageIndex < FGridResults.Count then
    Result := FGridResults[PageIndex] as TGridResult
  else
    Result := nil;
end;


function TMainForm.DataGridResult: TGridResult;
begin
  // Dumb method for easier access to often used grid data on "Data" tab
  Result := GridResult(DataGrid);
end;


function TMainForm.QueryTabActive: Boolean;
begin
  // Find out if the active main tab is a query tab
  Result := IsQueryTab(PageControlMain.ActivePageIndex, True);
end;

function TMainForm.IsQueryTab(PageIndex: Integer; IncludeFixed: Boolean): Boolean;
var
  Min: Integer;
begin
  // Find out if the given main tab is a query tab
  Min := tabQuery.PageIndex+1;
  if IncludeFixed then Dec(Min);
  Result := PageIndex >= Min;
end;

end.

