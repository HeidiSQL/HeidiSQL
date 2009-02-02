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
  mysqlquery, createdatabase, createtable, tbl_properties, SynRegExpr,
  WideStrUtils, ZDbcLogging, ExtActns, CommCtrl;

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
    actOpenSession: TAction;
    actExitApplication: TAction;
    Extra1: TMenuItem;
    FlushUserPrivileges1: TMenuItem;
    MenuCopyCSV: TMenuItem;
    N3: TMenuItem;
    MenuRefresh1: TMenuItem;
    MenuExport: TMenuItem;
    MenuCreateDatabase: TMenuItem;
    MenuCreateTable: TMenuItem;
    N4: TMenuItem;
    MenuDropDatabase: TMenuItem;
    MenuDropTable: TMenuItem;
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
    ButtonCreateDatabase: TToolButton;
    ButtonDropDatabase: TToolButton;
    ButtonRefresh: TToolButton;
    tlbSep5: TToolButton;
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
    actDropTablesAndViews: TAction;
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
    actEditView: TAction;
    actCreateView: TAction;
    ToolButton2: TToolButton;
    Createview1: TMenuItem;
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
    ToolBarDatabase: TToolBar;
    btnDBEmptyTable: TToolButton;
    btnDBDropTable: TToolButton;
    btnDBCopyTable: TToolButton;
    btnCreateTable: TToolButton;
    btnCreateView: TToolButton;
    actCreateTable: TAction;
    actEmptyTables: TAction;
    actEditTableFields: TAction;
    actEditTableProperties: TAction;
    actEditField: TAction;
    actCreateField: TAction;
    actDropFields: TAction;
    actEditIndexes: TAction;
    actDropDatabase: TAction;
    actCreateDatabase: TAction;
    actEditDatabase: TAction;
    actSQLhelp: TAction;
    actRefresh: TAction;
    actImportCSV: TAction;
    actCut: TAction;
    Cut1: TMenuItem;
    actExportSettings: TAction;
    actImportSettings: TAction;
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
    btnEditTableProperties: TToolButton;
    btnEditDatabase: TToolButton;
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
    btnEditView: TToolButton;
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
    popupTreeView: TPopupMenu;
    menuRefreshDBTree: TMenuItem;
    tabTable: TTabSheet;
    popupDbGrid: TPopupMenu;
    menuproperties: TMenuItem;
    menudroptablea: TMenuItem;
    menuemptytable: TMenuItem;
    tabHost: TTabSheet;
    PageControlHost: TPageControl;
    tabVariables: TTabSheet;
    tabProcessList: TTabSheet;
    ListVariables: TVirtualStringTree;
    ListProcesses: TVirtualStringTree;
    popupHost: TPopupMenu;
    Kill1: TMenuItem;
    NewDatabase1: TMenuItem;
    ListTables: TVirtualStringTree;
    Refresh1: TMenuItem;
    pnlDataTop: TPanel;
    menurefresh2: TMenuItem;
    N2: TMenuItem;
    pnlQueryMemo: TPanel;
    SynSQLSyn1: TSynSQLSyn;
    SynMemoQuery: TSynMemo;
    spltQuery: TSplitter;
    menucreatetablea: TMenuItem;
    OpenDialog1: TOpenDialog;
    TimerHostUptime: TTimer;
    popupTableGrid: TPopupMenu;
    Refresh2: TMenuItem;
    DropField1: TMenuItem;
    N3a: TMenuItem;
    N5a: TMenuItem;
    PopupmenuDropDatabase: TMenuItem;
    popupDataGrid: TPopupMenu;
    Refresh3: TMenuItem;
    MenuAddField: TMenuItem;
    MenuEditField: TMenuItem;
    popupResultGrid: TPopupMenu;
    Copyrecords1: TMenuItem;
    CopyasCSVData1: TMenuItem;
    N9a: TMenuItem;
    LabelResultinfo: TLabel;
    menuAlterTable: TMenuItem;
    N10: TMenuItem;
    MenuRenameTable: TMenuItem;
    TimerConnected: TTimer;
    N12: TMenuItem;
    popupSqlLog: TPopupMenu;
    Clear2: TMenuItem;
    Copy1: TMenuItem;
    N15: TMenuItem;
    menuMaintenancea: TMenuItem;
    PopupMenuDropTable: TMenuItem;
    N17: TMenuItem;
    ListColumns: TVirtualStringTree;
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
    PrintList3: TMenuItem;
    PrintList4: TMenuItem;
    N1a: TMenuItem;
    MenuCopyTable: TMenuItem;
    SynMemoFilter: TSynMemo;
    N18: TMenuItem;
    MenuAutoupdate: TMenuItem;
    TimerRefresh: TTimer;
    Set1: TMenuItem;
    EnableAutoRefresh: TMenuItem;
    DisableAutoRefresh: TMenuItem;
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
    InsertfilesintoBLOBfields1a: TMenuItem;
    InsertfilesintoBLOBfields2: TMenuItem;
    InsertfilesintoBLOBfields3: TMenuItem;
    N19: TMenuItem;
    setNULL1: TMenuItem;
    ZSQLMonitor1: TZSQLMonitor;
    Exporttables1: TMenuItem;
    Exporttables2: TMenuItem;
    popupDbGridHeader: TPopupMenu;
    SynCompletionProposal1: TSynCompletionProposal;
    OpenDialogSQLFile: TOpenDialog;
    SaveDialogSQLFile: TSaveDialog;
    SynEditSearch1: TSynEditSearch;
    N16: TMenuItem;
    ManageIndexes1: TMenuItem;
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
    menuRenameColumn: TMenuItem;
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
    PopupMenuCreateTable: TMenuItem;
    menuSQLhelp2: TMenuItem;
    N24: TMenuItem;
    menuSQLhelpData: TMenuItem;
    menuAlterdatabase: TMenuItem;
    menuTreeAlterTable: TMenuItem;
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
    menuEditView: TMenuItem;
    Createview2: TMenuItem;
    menuTreeCreateView: TMenuItem;
    menuTreeEditView: TMenuItem;
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
    Selectbackgroundcolor1: TMenuItem;
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
    procedure refreshMonitorConfig;
    procedure loadWindowConfig;
    procedure saveWindowConfig;
    procedure setDefaultWindowConfig;
    procedure actCreateFieldExecute(Sender: TObject);
    procedure actEditTablePropertiesExecute(Sender: TObject);
    procedure actCreateTableExecute(Sender: TObject);
    procedure actCreateViewExecute(Sender: TObject);
    procedure menuWindowClick(Sender: TObject);
    procedure focusWindow(Sender: TObject);
    procedure menuConnectionsPopup(Sender: TObject);
    procedure actExitApplicationExecute(Sender: TObject);
    procedure DisplayChange(var msg: TMessage); message WM_DISPLAYCHANGE;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DoAfterConnect;
    procedure DoDisconnect;
    procedure FormResize(Sender: TObject);
    procedure actUserManagerExecute(Sender: TObject);
    procedure actAboutBoxExecute(Sender: TObject);
    procedure actApplyFilterExecute(Sender: TObject);
    procedure actClearEditorExecute(Sender: TObject);
    procedure actMaintenanceExecute(Sender: TObject);
    procedure actEditViewExecute(Sender: TObject);
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
    procedure actDropDatabaseExecute(Sender: TObject);
    procedure actDropFieldsExecute(Sender: TObject);
    procedure actDropTablesAndViewsExecute(Sender: TObject);
    procedure actEditDatabaseExecute(Sender: TObject);
    procedure actEditIndexesExecute(Sender: TObject);
    procedure actEmptyTablesExecute(Sender: TObject);
    procedure actEditFieldExecute(Sender: TObject);
    procedure actEditTableFieldsExecute(Sender: TObject);
    procedure actExportSettingsExecute(Sender: TObject);
    procedure actFlushExecute(Sender: TObject);
    procedure actImportCSVExecute(Sender: TObject);
    procedure actImportSettingsExecute(Sender: TObject);
    procedure actLoadSQLExecute(Sender: TObject);
    procedure actOpenSessionExecute(Sender: TObject);
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
    procedure menuRenameColumnClick(Sender: TObject);
    procedure ListColumnsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
    procedure popupQueryPopup(Sender: TObject);
    procedure lboxQueryHelpersClick(Sender: TObject);
    procedure lboxQueryHelpersDblClick(Sender: TObject);
    procedure tabsetQueryHelpersChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure btnDataClick(Sender: TObject);
    procedure ListTablesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ListColumnsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
      const Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1CodeCompletion(Sender: TObject;
      var Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1Execute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
      var CanExecute: Boolean);
    procedure pcChange(Sender: TObject);
    procedure ValidateControls(FrmIsFocussed: Boolean = true);
    procedure ValidateQueryControls(FrmIsFocussed: Boolean = true);
    function FieldContent(ds: TDataSet; ColName: WideString): WideString;
    procedure LoadDatabaseProperties(db: WideString);
    procedure ShowHost;
    procedure ShowDatabase(db: WideString);
    procedure ShowDBProperties(db: WideString);
    procedure ShowTable(table: WideString; tab: TTabSheet = nil);
    procedure ShowTableProperties;
    procedure ShowTableData(table: WideString);
    function EnsureFullWidth(Grid: TBaseVirtualTree; Column: TColumnIndex; Node: PVirtualNode): Boolean;
    procedure EnsureNodeLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode; WhereClause: WideString);
    procedure EnsureChunkLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
    procedure MenuRenameTableClick(Sender: TObject);
    procedure TimerConnectedTimer(Sender: TObject);
    procedure Clear2Click(Sender: TObject);
    procedure ListTablesDblClick(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    procedure popupResultGridPopup(Sender: TObject);
    procedure Autoupdate1Click(Sender: TObject);
    procedure EnableAutoRefreshClick(Sender: TObject);
    procedure DisableAutoRefreshClick(Sender: TObject);
    procedure SynMemoQueryDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoQueryDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TUnicodeStrings);
    procedure popupHostPopup(Sender: TObject);
    procedure Saveastextfile1Click(Sender: TObject);
    procedure popupTreeViewPopup(Sender: TObject);
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
    procedure DataGridHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button:
        TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    procedure vstHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
        Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstHeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex;
        DropPosition: TPoint);
    procedure DBtreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
    procedure ListColumnsDblClick(Sender: TObject);
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
    procedure QueryGridFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
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
  private
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
    dsHaveEngines              : TDataSet;
    FilterPanelManuallyOpened  : Boolean;
    winName                    : String;
    FLastSelectedTableColumns,
    FLastSelectedTableKeys     : TDataset;
    DataGridDB, DataGridTable  : WideString;
    PrevTableColWidths         : WideStrings.TWideStringList;
    DataGridHasChanges         : Boolean;
    function GetParamValue(const paramChar: Char; const paramName:
      string; var curIdx: Byte; out paramValue: string): Boolean;
    procedure SetDelimiter(Value: String);
    function GetQueryRunning: Boolean;
    procedure SetQueryRunning(running: Boolean);
    function GetActiveGrid: TVirtualStringTree;
    function GetActiveData: PGridResult;
    procedure WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery; ForceDialog: Boolean);
    function RunThreadedQuery(AQuery: WideString; ForceDialog: Boolean): TMysqlQuery;
    procedure DisplayRowCountStats(MatchingRows: Int64);
    procedure insertFunction(Sender: TObject);
    function GetActiveDatabase: WideString;
    function GetSelectedTable: WideString;
    procedure SetSelectedDatabase(db: WideString);
    procedure SetSelectedTable(table: WideString);
    procedure SaveListSetup( List: TVirtualStringTree );
    procedure RestoreListSetup( List: TVirtualStringTree );
    procedure SetVisibleListColumns( List: TVirtualStringTree; Columns: WideStrings.TWideStringList );
    function GetTableSize(ds: TDataSet): Int64;
    procedure ToggleFilterPanel(ForceVisible: Boolean = False);
    function GetSelTableColumns: TDataset;
    function GetSelTableKeys: TDataset;
    procedure AutoCalcColWidths(Tree: TVirtualStringTree; PrevLayout: Widestrings.TWideStringlist = nil);
    procedure FocusGridCol(Grid: TBaseVirtualTree; Column: TColumnIndex);
  public
    virtualDesktopName: string;
    MaintenanceForm: TOptimize;
    ViewForm: TfrmView;
    UserManagerForm: TUserManagerForm;
    SelectDBObjectForm: TfrmSelectDBObject;
    SQLHelpForm: TfrmSQLhelp;
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
    VTRowDataListTables,
    VTRowDataListColumns       : TVTreeDataArray;

    FProgressForm              : TFrmQueryProgress;

    // Variables set by preferences dialog
    prefRememberFilters        : Boolean;
    prefLogsqlnum,
    prefLogSqlWidth,
    prefMaxColWidth            : Integer;
    prefCSVSeparator,
    prefCSVEncloser,
    prefCSVTerminator          : String[10];
    prefLogToFile,
    prefPreferShowTables,
    prefEnableBinaryEditor,
    prefEnableDatetimeEditor,
    prefEnableEnumEditor,
    prefEnableSetEditor,
    prefEnableNullBG           : Boolean;
    prefFieldColorNumeric,
    prefFieldColorText,
    prefFieldColorBinary,
    prefFieldColorDatetime,
    prefFieldColorEnum,
    prefFieldColorSet,
    prefNullColorNumeric,
    prefNullColorText,
    prefNullColorBinary,
    prefNullColorDatetime,
    prefNullColorEnum,
    prefNullColorSet,
    prefNullColorDefault,
    prefNullBG                 : TColor;
    CreateDatabaseForm         : TCreateDatabaseForm;
    CreateTableForm            : TCreateTableForm;
    TablePropertiesForm        : Ttbl_properties_form;
    FDataGridResult,
    FQueryGridResult           : TGridResult;
    FDataGridSelect            : WideStrings.TWideStringList;
    FDataGridSort              : TOrderColArray;
    DataGridCurrentSelect      : WideString;
    DataGridCurrentFilter      : WideString;
    DataGridCurrentSort        : WideString;

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
    function GetVisualDataset: PGridResult;

    function ExecUpdateQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): Int64;
    function ExecSelectQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false; ForceDialog: Boolean = false): TDataSet;
    procedure ExecUseQuery(db: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false);

    property FQueryRunning: Boolean read GetQueryRunning write SetQueryRunning;
    property ActiveGrid: TVirtualStringTree read GetActiveGrid;
    property ActiveData: PGridResult read GetActiveData;
    property MysqlConn : TMysqlConn read FMysqlConn;
    property Conn : TOpenConnProf read FConn;

    property ActiveDatabase : WideString read GetActiveDatabase write SetSelectedDatabase;
    property SelectedTable : WideString read GetSelectedTable write SetSelectedTable;

    function FetchActiveDbTableList: TDataSet;
    function RefreshActiveDbTableList: TDataSet;
    function FetchDbTableList(db: WideString): TDataSet;
    function RefreshDbTableList(db: WideString): TDataSet;
    procedure ClearDbTableList(db: WideString);
    function DbTableListCached(db: WideString): Boolean;
    procedure ClearAllTableLists;
    procedure EnsureDatabase;
    procedure TestVTreeDataArray( P: PVTreeDataArray );
    function GetVTreeDataArray( VT: TBaseVirtualTree ): PVTreeDataArray;
    procedure ActivateFileLogging;
    procedure DeactivateFileLogging;
    procedure TrimSQLLog;
    procedure TableEnginesCombo(var Combobox: TCombobox);
    function GetNodeType(Node: PVirtualNode): Byte;
    function GetSelectedNodeType: Byte;
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
    property FSelectedTableColumns: TDataset read GetSelTableColumns write FLastSelectedTableColumns;
    property FSelectedTableKeys: TDataset read GetSelTableKeys write FLastSelectedTableKeys;
    procedure CalcNullColors;
    procedure FillDataViewPopup;
    procedure GetDataViews(List: TStrings);
    procedure DataViewClick(Sender: TObject);
    procedure LoadDataView(ViewName: String);
    function GetRegKeyTable: String;
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
  options,
  printlist,
  copytable,
  insertfiles,
  Threading,
  mysql_structures,
  UpdateCheck,
  fieldeditor,
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
  MainReg.WriteInteger(REGNAME_TOOLBARDBLEFT, ToolBarDatabase.Left);
  MainReg.WriteInteger(REGNAME_TOOLBARDBTOP, ToolBarDatabase.Top);
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
  SaveListSetup(ListColumns);

  FreeAndNil(CreateTableForm);

  debug('mem: clearing query and browse data.');
  SetLength(FDataGridResult.Rows, 0);
  SetLength(FDataGridResult.Columns, 0);
  SetLength(FQueryGridResult.Rows, 0);
  SetLength(FQueryGridResult.Columns, 0);

  ValidateControls(False);
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
  i, j: Integer;
  miGroup,
  miFilterGroup,
  miFunction,
  miFilterFunction,
  menuitem : TMenuItem;
  functioncats : TStringList;
  fontname, datafontname : String;
  fontsize, datafontsize : Integer;
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

  // read function-list into menu
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
      miFunction.Hint := StringReplace( miFunction.Hint, '|', '�', [rfReplaceAll] );
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
  FixVT(ListColumns);

  // Position of Toolbars
  ToolBarStandard.Left := GetRegValue(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
  ToolBarStandard.Top := GetRegValue(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
  ToolBarDatabase.Left := GetRegValue(REGNAME_TOOLBARDBLEFT, ToolBarDatabase.Left);
  ToolBarDatabase.Top := GetRegValue(REGNAME_TOOLBARDBTOP, ToolBarDatabase.Top);
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
  prefPreferShowTables := GetRegValue(REGNAME_PREFER_SHOWTABLES, DEFAULT_PREFER_SHOWTABLES);

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
  prefFieldColorNumeric := GetRegValue(REGNAME_FIELDCOLOR_NUMERIC, DEFAULT_FIELDCOLOR_NUMERIC);
  prefFieldColorText := GetRegValue(REGNAME_FIELDCOLOR_TEXT, DEFAULT_FIELDCOLOR_TEXT);
  prefFieldColorBinary := GetRegValue(REGNAME_FIELDCOLOR_BINARY, DEFAULT_FIELDCOLOR_BINARY);
  prefFieldColorDatetime := GetRegValue(REGNAME_FIELDCOLOR_DATETIME, DEFAULT_FIELDCOLOR_DATETIME);
  prefFieldColorEnum := GetRegValue(REGNAME_FIELDCOLOR_ENUM, DEFAULT_FIELDCOLOR_ENUM);
  prefFieldColorSet := GetRegValue(REGNAME_FIELDCOLOR_SET, DEFAULT_FIELDCOLOR_SET);
  prefNullBG := GetRegValue(REGNAME_BG_NULL, DEFAULT_BG_NULL);
  CalcNullColors;
  // Editor enablings
  prefEnableBinaryEditor := GetRegValue(REGNAME_FIELDEDITOR_BINARY, DEFAULT_FIELDEDITOR_BINARY);
  prefEnableDatetimeEditor := GetRegValue(REGNAME_FIELDEDITOR_DATETIME, DEFAULT_FIELDEDITOR_DATETIME);
  prefEnableEnumEditor := GetRegValue(REGNAME_FIELDEDITOR_ENUM, DEFAULT_FIELDEDITOR_ENUM);
  prefEnableSetEditor := GetRegValue(REGNAME_FIELDEDITOR_SET, DEFAULT_FIELDEDITOR_SET);
  prefEnableNullBG := GetRegValue(REGNAME_BG_NULL_ENABLED, DEFAULT_BG_NULL_ENABLED);

  // Color coding:
  SynSQLSyn1.KeyAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLKEYATTRI, ColorToString(DEFAULT_SQLCOLKEYATTRI)));
  SynSQLSyn1.FunctionAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLFUNCTIONATTRI, ColorToString(DEFAULT_SQLCOLFUNCTIONATTRI)));
  SynSQLSyn1.DataTypeAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLDATATYPEATTRI, ColorToString(DEFAULT_SQLCOLDATATYPEATTRI)));
  SynSQLSyn1.NumberAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLNUMBERATTRI, ColorToString(DEFAULT_SQLCOLNUMBERATTRI)));
  SynSQLSyn1.StringAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLSTRINGATTRI, ColorToString(DEFAULT_SQLCOLSTRINGATTRI)));
  SynSQLSyn1.CommentAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLCOMMENTATTRI, ColorToString(DEFAULT_SQLCOLCOMMENTATTRI)));
  SynSQLSyn1.ConditionalCommentAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLCONDCOMMATTRI, ColorToString(DEFAULT_SQLCOLCONDCOMMATTRI)));
  SynSQLSyn1.TablenameAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLTABLENAMEATTRI, ColorToString(DEFAULT_SQLCOLTABLENAMEATTRI)));
  SynSQLSyn1.SymbolAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLSYMBOLATTRI, ColorToString(DEFAULT_SQLCOLSYMBOLATTRI)));
  SynSQLSyn1.IdentifierAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLIDENTATTRI, ColorToString(DEFAULT_SQLCOLIDENTATTRI)));
  SynSQLSyn1.DelimitedIdentifierAttri.Foreground := StringToColor(GetRegValue(REGNAME_SQLCOLDELIMIDENTATTRI, ColorToString(DEFAULT_SQLCOLDELIMIDENTATTRI)));
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
  RestoreListSetup(ListColumns);

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
end;


{**
  Check for connection parameters on commandline or show connections form.
}
procedure TMainForm.FormShow(Sender: TObject);
var
  curParam : Byte;
  sValue,
  parHost, parPort, parUser, parPass, parDatabase,
  parTimeout, parCompress, parSortDatabases, parDescription : String;
  LastUpdatecheck : TDateTime;
  UpdatecheckInterval : Integer;
  DefaultLastrunDate, LastSession: String;
  frm : TfrmUpdateCheck;
  dlgResult: Integer;
  Connected, CommandLineMode: Boolean;
  ConnForm: TConnForm;
begin
  // Do an updatecheck if checked in settings
  if GetRegValue(REGNAME_DO_UPDATECHECK, DEFAULT_DO_UPDATECHECK) then begin
    DefaultLastrunDate := '2000-01-01';
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
      if frm.ShowModal = mrOK then
        Close;
      FreeAndNil(frm);
    end;
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
  // Minimal parameter for command line mode is hostname
  CommandLineMode := parHost <> '';
  if CommandLineMode then begin
    Connected := InitConnection(parHost, parPort, parUser, parPass, parDatabase, parTimeout, parCompress, parSortDatabases);
    if Connected then begin
      // Take care for empty description - it gets used to read/write session settings to registry!
      SessionName := parDescription;
      if SessionName = '' then
        SessionName := parHost;
      // Save session parameters to registry
      if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS + SessionName, true) then begin
        MainReg.WriteString(REGNAME_HOST, parHost);
        MainReg.WriteString(REGNAME_USER, parUser);
        MainReg.WriteString(REGNAME_PASSWORD, encrypt(parPass));
        MainReg.WriteString(REGNAME_PORT, parPort);
        MainReg.WriteString(REGNAME_TIMEOUT, parTimeout);
        MainReg.WriteBool(REGNAME_COMPRESSED, Boolean(StrToIntDef(parCompress, 0)) );
        MainReg.WriteString(REGNAME_ONLYDBS, parDatabase);
        MainReg.WriteBool(REGNAME_ONLYDBSSORTED, Boolean(StrToIntDef(parSortDatabases, 0)) );
      end;
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
      Exit;
    end;
  end;

  DoAfterConnect;

  if (not CommandLineMode) and (ParamStr(1) <> '') then begin
    // Loading SQL file by command line. Mutually exclusive to connect by command line.
    QueryLoad(ParamStr(1));
  end;

end;


procedure TMainForm.DoAfterConnect;
var
  i: Integer;
  lastUsedDB: WideString;
  v: String[50];
  v1, v2, v3: String;
  rx: TRegExpr;
begin
  DataGridHasChanges := False;

  // Activate logging
  if GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE) then
    ActivateFileLogging;

  time_connected := 0;
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
  Application.Title := winName + ' - ' + APPNAME;
  Caption := winName;

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
var
  f: Toptionsform;
begin
  // Preferences
  f := Toptionsform.Create(Self);
  f.ShowModal;
  FreeAndNil(f);
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
    m := SynMemoQuery
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
var
  NodeData: PVTreeData;
  ds: TDataset;
begin
  if ViewForm = nil then
    ViewForm := TfrmView.Create(Self);
  ViewForm.EditViewName := '';
  if (Sender as TAction) = actEditView then begin
    // Edit mode
    if actEditView.ActionComponent = menuEditView then begin
      // "Edit view" was clicked in ListTables' context menu
      NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
      ViewForm.EditViewName := NodeData.Captions[0];
    end else if actEditView.ActionComponent = menuTreeEditView then begin
      // "Edit view" was clicked in DBTree's context menu
      ds := FetchDbTableList(ActiveDatabase);
      ds.RecNo := DBtree.GetFirstSelected.Index+1;
      ViewForm.EditViewName := ds.Fields[0].AsString;
    end else
      // If we're here, there's a menu item "Edit/Create view" in an unknown location
      raise Exception.Create('Internal error in actCreateViewExexute.');
  end else begin
    // Create mode. Nothing special here.
  end;
  ViewForm.ShowModal;
end;


{**
  Edit view
}
procedure TMainForm.actEditViewExecute(Sender: TObject);
begin
  actCreateViewExecute(Sender);
end;


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

  // "New window" item
  item := TMenuItem.Create(menuConnections);
  item.Action := actOpenSession;
  item.Default := True;
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
  GridData: PGridResult;
begin
  // Copy data in focused grid as CSV
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  try
    GridData := ActiveData;
    GridToCsv(ActiveGrid, GridData, prefCSVSeparator, prefCSVEncloser, prefCSVTerminator, S);
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
  GridData: PGridResult;
begin
  // Copy data in focused grid as HTML table
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  if ActiveGrid = DataGrid then Title := SelectedTable
  else Title := 'SQL query';
  try
    GridData := ActiveData;
    GridToHtml(ActiveGrid, GridData, Title, S);
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
  GridData: PGridResult;
begin
  // Copy data in focused grid as XML
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  if ActiveGrid = DataGrid then Root := SelectedTable
  else Root := 'SQL query';
  try
    GridData := ActiveData;
    GridToXml(ActiveGrid, GridData, Root, S);
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
  GridData: PGridResult;
begin
  // Copy data in focused grid as SQL
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  if ActiveGrid = DataGrid then Tablename := SelectedTable
  else Tablename := 'unknown';
  try
    GridData := ActiveData;
    GridToSql(ActiveGrid, GridData, Tablename, S);
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
  Grid: TVirtualStringTree;
  GridData: PGridResult;
  Dialog: TSaveDialog;
  FS: TFileStream;
  Title: WideString;
begin
  // Save data in current dataset as CSV, HTML or XML
  Dialog := SaveDialogExportData;

  Grid := ActiveGrid;
  GridData := ActiveData;
  if Grid = DataGrid then
    Title := SelectedTable
  else
    Title := 'SQL query';

  Dialog.FileName := Title;
  Dialog.Title := 'Export result set from '+Dialog.Filename+'...';

  if Dialog.Execute and (Dialog.FileName <> '') then try
    Screen.Cursor := crHourGlass;
    FS := openfs(Dialog.FileName);
    case Dialog.FilterIndex of
      1: GridToCsv(Grid, GridData, prefCSVSeparator, prefCSVEncloser, prefCSVTerminator, FS);
      2: GridToHtml(Grid, GridData, Title, FS);
      3: GridToXml(Grid, GridData, Title, FS);
      4: GridToSql(Grid, GridData, Title, FS);
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
var
  g              : TVirtualStringTree;
  filename       : String;
  f              : Textfile;
  Content        : WideString;
  IsBinary       : Boolean;
begin
  g := ActiveGrid;
  if g = nil then begin messagebeep(MB_ICONASTERISK); exit; end;
  Screen.Cursor := crHourGlass;
  showstatus('Saving contents to file...');
  if not EnsureFullWidth(g, g.FocusedColumn, g.FocusedNode) then
    Exit;
  IsBinary := ActiveData.Columns[g.FocusedColumn].IsBinary;
  Content := g.Text[g.FocusedNode, g.FocusedColumn];

  filename := GetTempDir+'\'+APPNAME+'-preview.';
  if IsBinary then begin
    if pos('JFIF', copy(Content, 0, 20)) <> 0 then
      filename := filename + 'jpeg'
    else if StrCmpBegin('GIF', Content) then
      filename := filename + 'gif'
    else if StrCmpBegin('BM', Content) then
      filename := filename + 'bmp';
    AssignFile(f, filename);
    Rewrite(f);
    Write(f, Content);
    CloseFile(f);
  end else begin
    filename := filename + 'html';
    SaveUnicodeFile(filename, Content);
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
begin
  f := TExportSQLForm.Create((Sender as TAction).ActionComponent);
  f.ShowModal;
  FreeAndNil(f);
end;

// Drop Table(s)
procedure TMainForm.actDropTablesAndViewsExecute(Sender: TObject);
var
  i : Integer;
  Tables, Views : TWideStringList;
  msg, sql, activeDB : String;
begin
  debug('drop table activated');
  // Set default database name to to ActiveDatabase.
  // Can be overwritten when someone selects a table in dbtree from different database
  activeDB := ActiveDatabase;

  Tables := TWideStringlist.Create;
  Views := TWideStringlist.Create;
  if (PageControlMain.ActivePage = tabDatabase) and
    ((Sender as TAction).ActionComponent <> PopupMenuDropTable) then begin
    // Invoked from one of the various buttons, SheetDatabase is the active page, drop highlighted table(s).
    Tables := GetVTCaptions(ListTables, True, 0, NODETYPE_TABLE);
    Tables.AddStrings(GetVTCaptions(ListTables, True, 0, NODETYPE_CRASHED_TABLE));
    Views := GetVTCaptions(ListTables, True, 0, NODETYPE_VIEW);
  end else begin
    // Invoked from one of the various buttons, drop table selected in tree view.
    case GetSelectedNodeType of
      NODETYPE_TABLE: Tables.Add(SelectedTable);
      NODETYPE_CRASHED_TABLE: Tables.Add(SelectedTable);
      NODETYPE_VIEW: Views.Add(SelectedTable)
    end;
  end;

  // Fix actions temporarily enabled for popup menu.
  ValidateControls;

  // Safety stop to avoid firing DROP TABLE without tablenames
  if (Tables.Count = 0) and (Views.Count = 0) then
    Exit;

  // Ask user for confirmation to drop selected objects
  msg := 'Drop ' + IntToStr(Tables.Count+Views.Count) + ' table(s) and/or view(s) in database "'+activeDB+'"?'
    + CRLF;
  if Tables.Count > 0 then msg := msg + CRLF + 'Tables: ' + ImplodeStr(', ', Tables);
  if Views.Count > 0 then msg := msg + CRLF + 'Views: ' + ImplodeStr(', ', Views);
  if MessageDlg(msg, mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    Exit;

  // Compose and run DROP TABLE query
  if Tables.Count > 0 then begin
    sql := 'DROP TABLE ';
    for i := 0 to Tables.Count - 1 do
    begin
      if i > 0 then
        sql := sql + ', ';
      sql := sql + mask(Tables[i]);
    end;
    ExecUpdateQuery( sql );
  end;
  FreeAndNil(Tables);

  // Compose and run DROP VIEW query
  if Views.Count > 0 then begin
    sql := 'DROP VIEW ';
    for i := 0 to Views.Count - 1 do
    begin
      if i > 0 then
        sql := sql + ', ';
      sql := sql + mask(Views[i]);
    end;
    ExecUpdateQuery( sql );
  end;
  FreeAndNil(Views);

  // Refresh ListTables + dbtree so the dropped tables are gone:
  actRefresh.Execute;
end;


// Load SQL-file, make sure that SheetQuery is activated
procedure TMainForm.actLoadSQLExecute(Sender: TObject);
begin
  PageControlMain.ActivePage := tabQuery;
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
    (FDataGridResult.Rows[DataGrid.GetFirstSelected.Index].State = grsInserted)
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


procedure TMainForm.actCreateFieldExecute(Sender: TObject);
begin
  FieldEditorWindow(Self, femFieldAdd);
end;


procedure TMainForm.actEditFieldExecute(Sender: TObject);
var
  fieldname: WideString;
  fem: TFieldEditorMode;
begin
  fieldname := '';
  fem := femFieldAdd;
  if Assigned(ListColumns.FocusedNode) and (vsSelected in ListColumns.FocusedNode.States) then
    fieldname := ListColumns.Text[ListColumns.FocusedNode, 0];
  if fieldname <> '' then
    fem := femFieldUpdate;
  FieldEditorWindow(Self, fem, fieldname);
end;


procedure TMainForm.actDropFieldsExecute(Sender: TObject);
var
  i: Integer;
  dropCmd: String;
  dropList: TWideStringList;
begin
  // We allow the user to select and delete multiple listItems
  dropList := GetVTCaptions( ListColumns, True );
  // User confirmation
  if MessageDlg('Delete ' + IntToStr(dropList.Count) + ' column(s): ' + ImplodeStr( ', ', dropList ) + ' ?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
  try
    // Concat fields for ALTER query
    for i := 0 to dropList.Count - 1 do
      dropCmd := dropCmd + 'DROP ' + mask(dropList[i]) + ', ';
    // Remove trailing comma
    delete(dropCmd, Length(dropCmd)-1, 2);
    // Execute field dropping
    ExecUpdateQuery( 'ALTER TABLE '+mask(SelectedTable)+' ' + dropCmd );
    // Rely on the server respective ExecUpdateQuery has raised an exception so the
    // following code will be skipped on any error
    ListColumns.BeginUpdate;
    ListColumns.DeleteSelectedNodes;
    ListColumns.EndUpdate;
    FSelectedTableColumns := nil;
    FSelectedTableKeys := nil;
    // Set focus on first item
    ListColumns.FocusedNode := ListColumns.GetFirstVisible;
  except
    On E : Exception do begin
      MessageDlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;
end;


procedure TMainForm.actEditIndexesExecute(Sender: TObject);
begin
  FieldEditorWindow(Self, femIndexEditor);
end;


procedure TMainForm.actCreateTableExecute(Sender: TObject);
begin
  if CreateTableForm = nil then
    CreateTableForm := TCreateTableForm.Create(Self);
  CreateTableForm.ShowModal;
end;


procedure TMainForm.actEmptyTablesExecute(Sender: TObject);
var
  t: TWideStringList;
  i: Integer;
  sql_pattern: String;
begin
  if ListTables.SelectedCount = 0 then
    exit;
  // Add selected items/tables to helper list
  t := GetVTCaptions(ListTables, True);
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


procedure TMainForm.actEditTableFieldsExecute(Sender: TObject);
var
  NodeData: PVTreeData;
begin
  // table-doubleclick
  if Assigned(ListTables.FocusedNode) then begin
    NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
    SelectedTable := NodeData.Captions[0];
    PageControlMain.ActivePage := tabTable;
  end;
end;


procedure TMainForm.actEditTablePropertiesExecute(Sender: TObject);
var
  NodeData: PVTreeData;
  caller: TComponent;
begin
  if TablePropertiesForm = nil then
    TablePropertiesForm := Ttbl_properties_form.Create(Self);

  caller := TAction(Sender).ActionComponent;
  if caller = menuTreeAlterTable then
    TablePropertiesForm.TableName := SelectedTable
  else begin
    NodeData := ListTables.GetNodeData( ListTables.FocusedNode );
    TablePropertiesForm.TableName := NodeData.Captions[0];
  end;

  TablePropertiesForm.ShowModal;
end;


procedure TMainForm.actDropDatabaseExecute(Sender: TObject);
var
  tndb: PVirtualNode;
  db: String;
begin
  // Drop DB.
  case DBtree.GetNodeLevel(DBtree.GetFirstSelected) of
    1: tndb := DBtree.GetFirstSelected;
    2: tndb := DBtree.GetFirstSelected.Parent;
    else Exit;
  end;
  if not Assigned(tndb) then raise Exception.Create('Internal error: Cannot drop NIL database.');
  db := Databases[tndb.Index];

  if MessageDlg('Drop Database "'+db+'"?' + crlf + crlf + 'WARNING: You will lose all tables in database '+db+'!', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    Abort;

  Screen.Cursor := crHourglass;
  try
    ExecUpdateQuery( 'DROP DATABASE ' + mask(db) );
    ClearDbTableList(db);
    if DatabasesWanted.IndexOf(db) > -1 then begin
      DatabasesWanted.Delete( DatabasesWanted.IndexOf(db) );
      OpenRegistry(SessionName);
      MainReg.WriteString( 'OnlyDBs', ImplodeStr( ';', DatabasesWanted ) );
    end;
    DBtree.Selected[DBtree.GetFirst] := true;
    RefreshTree(False);
  except
    MessageDLG('Dropping failed.'+crlf+'Maybe '''+db+''' is not a valid database-name.', mtError, [mbOK], 0)
  end;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.actEditDatabaseExecute(Sender: TObject);
begin
  if CreateDatabaseForm = nil then
    CreateDatabaseForm := TCreateDatabaseForm.Create(Self);
  CreateDatabaseForm.modifyDB := ActiveDatabase;
  CreateDatabaseForm.ShowModal;
end;


procedure TMainForm.actOpenSessionExecute(Sender: TObject);
begin
  debug('perf: new connection clicked.');
  ShellExec( ExtractFileName(paramstr(0)), ExtractFilePath(paramstr(0)) );
end;


procedure TMainForm.actQueryFindExecute(Sender: TObject);
var
  m: TSynMemo;
begin
  FindDialogQuery.execute;
  m := SynMemoQuery;
  // if something is selected search for that text
  if m.SelAvail and (m.BlockBegin.Line = m.BlockEnd.Line)
  then
    FindDialogQuery.FindText := m.SelText
  else
    FindDialogQuery.FindText := m.GetWordAtRowCol(m.CaretXY);
end;


procedure TMainForm.actQueryReplaceExecute(Sender: TObject);
var
  m: TSynMemo;
begin
  ReplaceDialogQuery.execute;
  m := SynMemoQuery;
  // if something is selected search for that text
  if m.SelAvail and (m.BlockBegin.Line = m.BlockEnd.Line)
  then
    ReplaceDialogQuery.FindText := m.SelText
  else
    ReplaceDialogQuery.FindText := m.WordAtCursor;
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
  end else if tab1 = tabTable then
    ShowTableProperties
  else if tab1 = tabData then
    viewdata(Sender);
end;


procedure TMainForm.actSQLhelpExecute(Sender: TObject);
var
  keyword : String;
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
    keyword := VTRowDataListColumns[DataGrid.FocusedColumn].Captions[1];
  end
  // Table-Tab
  else if ListColumns.Focused and Assigned(ListColumns.FocusedNode) then
  begin
    keyword := ListColumns.Text[ListColumns.FocusedNode, 1];
  end
  else if lboxQueryHelpers.Focused then
  begin
    // Makes only sense if one of the tabs "SQL fn" or "SQL kw" was selected
    if tabsetQueryHelpers.TabIndex in [1,2] then
    begin
      keyword := lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex];
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
begin
  // Save SQL
  if SaveDialogSQLFile.Execute then
  begin
    Screen.Cursor := crHourGlass;
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    case (Sender as TAction).Tag of
      0: SaveUnicodeFile( SaveDialogSQLFile.FileName, SynMemoQuery.Text );
      1: SaveUnicodeFile( SaveDialogSQLFile.FileName, SynMemoQuery.SelText );
    end;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actSaveSQLSnippetExecute(Sender: TObject);
var
  snippetname : String;
  mayChange   : Boolean;
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
      0: SaveUnicodeFile(snippetname, SynMemoQuery.Text);
      1: SaveUnicodeFile(snippetname, SynMemoQuery.SelText);
    end;
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
  SynMemoQuery.WordWrap := TAction(Sender).Checked;
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
  if SynMemoQuery.SearchReplace(Search, '', Options) = 0 then
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
  if SynMemoQuery.SearchReplace( Search, ReplaceDialogQuery.ReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    ShowStatus( 'SearchText ''' + Search + ''' not found!', 0);
    if ssoBackwards in Options then
      SynMemoQuery.BlockEnd := SynMemoQuery.BlockBegin
    else
      SynMemoQuery.BlockBegin := SynMemoQuery.BlockEnd;
    SynMemoQuery.CaretXY := SynMemoQuery.BlockBegin;
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
  if snip then
  begin
    msg :=
      Copy( msg, 0, prefLogSqlWidth ) +
      '/* large SQL query, snipped at  ' +
      FormatNumber( prefLogSqlWidth ) +
      ' characters */';
  end;
  msg := WideStringReplace( msg, #9, ' ', [rfReplaceAll] );
  msg := WideStringReplace( msg, #10, ' ', [rfReplaceAll] );
  msg := WideStringReplace( msg, #13, ' ', [rfReplaceAll] );
  msg := WideStringReplace( msg, '  ', ' ', [rfReplaceAll] );
  if ( comment ) then
  begin
    msg := '/* ' + msg + ' */';
  end;

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
   (PageControlMain.ActivePage = tabTable) or
   (PageControlMain.ActivePage = tabData)
  ) then PageControlMain.ActivePage := tabHost;

  tabDatabase.TabVisible := false;
  tabTable.TabVisible := false;
  tabData.TabVisible := false;

  Caption := winName;
  pcChange( Self );
end;


procedure TMainForm.ShowDatabase(db: WideString);
begin
  if (not DBtree.Dragging) and (
   (PageControlMain.ActivePage = tabHost) or
   (PageControlMain.ActivePage = tabTable) or
   (PageControlMain.ActivePage = tabData)
  ) then PageControlMain.ActivePage := tabDatabase;

  tabDatabase.TabVisible := true;
  tabTable.TabVisible := false;
  tabData.TabVisible := false;

  Caption := winName + ' - /' + db;
  ShowDBProperties( db );
end;


{***
  Do the default action (show table properties or table data) for a table.
}
procedure TMainForm.ShowTable(table: WideString; tab: TTabSheet = nil);
begin
  if tab = nil then tab := tabTable; // Alternative default: tabData
  if tab = tabTable then ShowTableProperties;
  if tab = tabData then ShowTableData( table );
  Caption := winName + ' - /' + ActiveDatabase + '/' + SelectedTable;
end;

procedure TMainForm.viewdata(Sender: TObject);
var
  i                    : Integer;
  select_base          : WideString;
  select_from          : WideString;
  sl_query             : TWideStringList;
  KeyCols              : WideStrings.TWideStringList;
  ColName              : WideString;
  col                  : TVirtualTreeColumn;
  rx                   : TRegExpr;
  ColType              : String;
  ColExists, ShowIt    : Boolean;
  Count, MatchingRows  : Int64;

procedure InitColumn(name: WideString; ColType: String; Visible: Boolean);
var
  k: Integer;
  idx: Integer;
begin
  idx := Length(FDataGridResult.Columns);
  SetLength(FDataGridResult.Columns, idx+1);
  FDataGridResult.Columns[idx].Name := name;
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
    FDataGridResult.Columns[idx].IsInt := True;
  end;
  rx.Expression := '^(float|double|decimal)\b';
  if rx.Exec(ColType) then begin
    col.Alignment := taRightJustify;
    FDataGridResult.Columns[idx].IsFloat := True;
  end;
  rx.Expression := '^(date|datetime|time(stamp)?)\b';
  if rx.Exec(ColType) then begin
    FDataGridResult.Columns[idx].IsDate := True;
    if rx.Match[1] = 'date' then FDataGridResult.Columns[idx].DataType := tpDATE
    else if rx.Match[1] = 'time' then FDataGridResult.Columns[idx].DataType := tpTIME
    else if rx.Match[1] = 'timestamp' then FDataGridResult.Columns[idx].DataType := tpTIMESTAMP
    else FDataGridResult.Columns[idx].DataType := tpDATETIME;
  end;
  rx.Expression := '^((tiny|medium|long)?text|(var)?char)\b(\(\d+\))?';
  if rx.Exec(ColType) then begin
    FDataGridResult.Columns[idx].IsText := True;
    if rx.Match[4] <> '' then
      FDataGridResult.Columns[idx].MaxLength := MakeInt(rx.Match[4])
    else if ColType = 'tinytext' then
      // 255 is the width in bytes. If characters that use multiple bytes are
      // contained, the width in characters is decreased below this number.
      FDataGridResult.Columns[idx].MaxLength := 255
    else if ColType = 'text' then
      FDataGridResult.Columns[idx].MaxLength := 65535
    else if ColType = 'mediumtext' then
      FDataGridResult.Columns[idx].MaxLength := 16777215
    else if ColType = 'longtext' then
      FDataGridResult.Columns[idx].MaxLength := 4294967295
    else
      // Fallback for unknown column types
      FDataGridResult.Columns[idx].MaxLength := MaxInt;
  end;
  rx.Expression := '^((tiny|medium|long)?blob|(var)?binary|bit)\b';
  if rx.Exec(ColType) then
    FDataGridResult.Columns[idx].IsBinary := True;
  if Copy(ColType, 1, 5) = 'enum(' then begin
    FDataGridResult.Columns[idx].IsEnum := True;
    FDataGridResult.Columns[idx].ValueList := WideStrings.TWideStringList.Create;
    FDataGridResult.Columns[idx].ValueList.QuoteChar := '''';
    FDataGridResult.Columns[idx].ValueList.Delimiter := ',';
    FDataGridResult.Columns[idx].ValueList.DelimitedText := GetEnumValues(ColType);
  end;
  if Copy(ColType, 1, 4) = 'set(' then begin
    FDataGridResult.Columns[idx].IsSet := True;
    FDataGridResult.Columns[idx].ValueList := WideStrings.TWideStringList.Create;
    FDataGridResult.Columns[idx].ValueList.QuoteChar := '''';
    FDataGridResult.Columns[idx].ValueList.Delimiter := ',';
    FDataGridResult.Columns[idx].ValueList.DelimitedText := GetEnumValues(ColType);
  end;

  FSelectedTableKeys.First;
  for k := 0 to FSelectedTableKeys.RecordCount - 1 do begin
    if (FSelectedTableKeys.FieldByName('Key_name').AsString = 'PRIMARY')
      and (FSelectedTableKeys.FieldByName('Column_name').AsWideString = name) then begin
      FDataGridResult.Columns[idx].IsPriPart := True;
      break;
    end;
    FSelectedTableKeys.Next;
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

  try
    if (SelectedTable <> '') and (ActiveDatabase <> '') then begin
      // Ensure <Table> and <Data> are visible
      tabTable.TabVisible := true;
      tabData.TabVisible := true;
      // Switch to <Data>
      PageControlMain.ActivePage := tabData;

      if FDataGridSelect = nil then
        FDataGridSelect := WideStrings.TWideStringlist.Create;
      if DataGridTable <> SelectedTable then begin
        FDataGridSelect.Clear;
        SynMemoFilter.Clear;
        SetLength(FDataGridSort, 0);
        // Load default view settings
        OpenRegistry;
        if MainReg.OpenKey(GetRegKeyTable, False) then begin
          if MainReg.ValueExists(REGNAME_DEFAULTVIEW) then begin
            // Disable default if crash indicator on current table is found
            if MainReg.ValueExists(REGPREFIX_CRASH_IN_DATA) then begin
              MainReg.DeleteValue(REGNAME_DEFAULTVIEW);
              LogSQL('A crash in the previous data loading for this table ('+SelectedTable+') was detected. Filtering was automatically reset to avoid the same crash for now.');
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
      debug('mem: clearing browse data.');
      SetLength(FDataGridResult.Columns, 0);
      SetLength(FDataGridResult.Rows, 0);
      DataGrid.RootNodeCount := 0;
      DataGrid.Header.Columns.BeginUpdate;
      DataGrid.Header.Options := DataGrid.Header.Options + [hoVisible];
      DataGrid.Header.Columns.Clear;

      // Prepare SELECT statement
      select_base := 'SELECT ';
      // Selected columns
      if (FDataGridSelect.Count = 0) or (FDataGridSelect.Count = FSelectedTableColumns.RecordCount) then begin
        tbtnDataColumns.ImageIndex := 107;
      end else begin
        for i := FDataGridSelect.Count - 1 downto 0 do begin
          ColExists := False;
          FSelectedTableColumns.First;
          while not FSelectedTableColumns.Eof do begin
            if FDataGridSelect[i] = FSelectedTableColumns.FieldByName('Field').AsWideString then begin
              ColExists := True;
              break;
            end;
            FSelectedTableColumns.Next;
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
      SetLength(FDataGridResult.Columns, 0);
      debug('mem: initializing browse columns.');
      FSelectedTableColumns.First;
      while not FSelectedTableColumns.Eof do begin
        ColName := FSelectedTableColumns.FieldByName('Field').AsWideString;
        ShowIt := (FDataGridSelect.Count=0) or (FDataGridSelect.IndexOf(ColName)>-1);
        if ShowIt or (KeyCols.IndexOf(ColName)>-1) then begin
          ColType := FSelectedTableColumns.FieldByName('Type').AsString;
          rx.Expression := '^((tiny|medium|long)?(text|blob)|(var)?(char|binary))\b(\(\d+\))?';
          if rx.Exec(ColType) then begin
            select_base := select_base + ' ' + 'LEFT(' + Mask(ColName) + ', ' + IntToStr(GridMaxData) + ')' + ',';
          end else begin
            select_base := select_base + ' ' + Mask(ColName) + ',';
          end;
          InitColumn(ColName, FSelectedTableColumns.FieldByName('Type').AsString, ShowIt);
        end;
        FSelectedTableColumns.Next;
      end;
      debug('mem: browse column initialization complete.');
      // Cut last comma
      select_base := copy( select_base, 1, Length(select_base)-1 );
      // Include db name for cases in which dbtree is switching databases and pending updates are in process
      select_from := ' FROM '+mask(ActiveDatabase)+'.'+mask(SelectedTable);

      // Final SELECT segments
      DataGridCurrentSelect := select_base + select_from;
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

      try
        ShowStatus('Counting rows...');
        sl_query.Add('SELECT COUNT(*)');
        sl_query.Add(select_from);
        // Apply custom WHERE filter
        if DataGridCurrentFilter <> '' then sl_query.Add('WHERE ' + DataGridCurrentFilter);
        MatchingRows := MakeInt(GetVar(sl_query.Text));
        count := MatchingRows;
        if count > GRIDMAXTOTALROWS then
          count := GRIDMAXTOTALROWS;
      except
        on E:Exception do begin
          // Most likely we have a wrong filter-clause when this happens
          // Put the user with his nose onto the wrong filter
          // either specified by user or
          // created by HeidiSQL by using the search box
          SynMemoFilter.Color := $008080FF; // light pink
          DataGrid.Header.Options := DataGrid.Header.Options - [hoVisible];
          MessageDlg( E.Message, mtError, [mbOK], 0 );
          raise;
        end;
      end;
      ShowStatus( STATUS_MSG_READY );

      debug('mem: initializing browse rows (internal data).');
      try
        SetLength(FDataGridResult.Rows, count);
        for i := 0 to count - 1 do begin
          FDataGridResult.Rows[i].Loaded := False;
        end;
        debug('mem: initializing browse rows (grid).');
        DataGrid.RootNodeCount := count;
      except
        DataGrid.RootNodeCount := 0;
        SetLength(FDataGridResult.Rows, 0);
        PageControlMain.ActivePage := tabTable;
        raise;
      end;
      debug('mem: browse row initialization complete.');

      // Switched to another table
      if DataGridTable <> SelectedTable then begin
        DataGrid.OffsetXY := Point(0, 0); // Scroll to top left
        FreeAndNil(PrevTableColWidths); // Throw away remembered, manually resized column widths
      end;
      DisplayRowCountStats(MatchingRows);
      dataselected := true;

      pcChange(self);
    end;
  finally
    DataGrid.Header.Columns.EndUpdate;
    DataGrid.EndUpdate;
    FreeAndNil(sl_query);
    AutoCalcColWidths(DataGrid, PrevTableColWidths);
    viewingdata := false;
    EnumerateRecentFilters;
    Screen.Cursor := crDefault;
  end;
  DataGridDB := ActiveDatabase;
  DataGridTable := SelectedTable;
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
begin
  lblDataTop.Caption := ActiveDatabase + '.' + SelectedTable + ': ';

  IsFiltered := self.DataGridCurrentFilter <> '';
  if GetSelectedNodeType = NODETYPE_TABLE then begin
    // Get rowcount from table
    ds := FetchActiveDbTableList;
    rows_total := -1;
    IsInnodb := False;
    if not prefPreferShowTables then for i := 0 to ds.RecordCount - 1 do begin
      if ds.Fields[0].AsWideString = SelectedTable then begin
        rows_total := MakeInt(ds.FieldByName('Rows').AsString);
        IsInnodb := ds.Fields[1].AsString = 'InnoDB';
        break;
      end;
    end;
    if rows_total = -1 then begin
      // Fallback for cases in which the user checked the "Prefer SHOW TABLES" option
      rows_total := StrToInt64( GetVar( 'SELECT COUNT(*) FROM ' + mask( SelectedTable ), 0 ) );
    end;
    lblDataTop.Caption := lblDataTop.Caption + FormatNumber( rows_total ) + ' rows total';
    if IsInnodb then
      lblDataTop.Caption := lblDataTop.Caption + ' (approximately!)'
  end else begin
    // Don't fetch rowcount from views to fix bug #1844952
    rows_total := -1;
    lblDataTop.Caption := lblDataTop.Caption + ' [View]';
  end;

  if( MatchingRows <> rows_total ) and IsFiltered then
    lblDataTop.Caption := lblDataTop.Caption + ', ' + FormatNumber(MatchingRows) + ' matching to filter';

  if ( MatchingRows >= rows_total ) and IsFiltered then
    lblDataTop.Caption := lblDataTop.Caption + ', filter matches all rows';

  if MatchingRows > DataGrid.RootNodeCount then
    lblDataTop.Caption := lblDataTop.Caption + ', limited to '+FormatNumber(DataGrid.RootNodeCount);
end;


procedure TMainForm.WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery; ForceDialog: Boolean);
var
  signal: Cardinal;
begin
  debug( 'Waiting for query to complete.' );
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
procedure TMainForm.pcChange(Sender: TObject);
var
  tab: TTabSheet;
begin
  tab := PageControlMain.ActivePage;

  // Move focus to relevant controls in order for them to receive keyboard events.
  // Do this only if the user clicked the new tab. Not on automatic tab changes.
  if Sender = PageControlMain then begin
    if tab = tabDatabase then ListTables.SetFocus
    else if tab = tabTable then ListColumns.SetFocus
    else if tab = tabData then begin
      viewdata(Sender);
      DataGrid.SetFocus;
    end else if tab = tabQuery then SynMemoQuery.SetFocus;
  end;

  // Ensure controls are in a valid state
  ValidateControls;
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
begin
  if not DbTableListCached(db) then begin
    // Not in cache, load table list.
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    ShowStatus('Fetching tables from "' + db + '" ...');
    try
      if (mysql_version >= 32300) and (not prefPreferShowTables) then begin
        ds := GetResults('SHOW TABLE STATUS FROM ' + mask(db), false, false);
      end else begin
        // contains table names, nothing else.
        ds := GetResults('SHOW /*!50002 FULL */ TABLES FROM ' + mask(db), false, false);
        // could clean up data (rename first column to 'Name') and
        // and add row counters to data set as a new field by using
        // SELECT COUNT(*), but that would potentially be rather slow.
      end;
      CachedTableLists.AddObject(db, ds);
      // Add table names to SQL highlighter
      SynSQLSyn1.TableNames.BeginUpdate;
      while not ds.Eof do begin
        SynSQLSyn1.TableNames.Add(ds.Fields[0].AsWideString);
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
  i               : Integer;
  bytes           : Int64;
  ds              : TDataSet;
  ListCaptions,
  SelectedCaptions: WideStrings.TWideStringList;
begin
  // DB-Properties
  Screen.Cursor := crHourGlass;

  // Remember selected nodes
  SelectedCaptions := GetVTCaptions(ListTables, True);

  try
    ds := FetchDbTableList(db);
    ShowStatus( 'Displaying tables from "' + db + '" ...' );

    ListTables.BeginUpdate;
    ListTables.Clear;

    SetLength(VTRowDataListTables, ds.RecordCount);
    for i := 1 to ds.RecordCount do
    begin
      listcaptions := WideStrings.TWideStringList.Create;
      // Table
      ListCaptions.Add( ds.Fields[0].AsWideString );

      // Treat tables slightly different than views
      case GetDBObjectType( ds.Fields) of
        NODETYPE_TABLE, NODETYPE_CRASHED_TABLE: // A normal table
        begin
          if GetDBObjectType(ds.Fields) = NODETYPE_CRASHED_TABLE then begin
            VTRowDataListTables[i-1].ImageIndex := ICONINDEX_CRASHED_TABLE;
            VTRowDataListTables[i-1].NodeType := NODETYPE_CRASHED_TABLE;
          end else begin
            VTRowDataListTables[i-1].ImageIndex := ICONINDEX_TABLE;
            VTRowDataListTables[i-1].NodeType := NODETYPE_TABLE;
          end;
          // Rows
          if ds.FindField('Rows') <> nil then
            ListCaptions.Add( FormatNumber( FieldContent(ds, 'Rows') ) )
          else
            ListCaptions.Add('');
          // Size: Data_length + Index_length
          bytes := GetTableSize(ds);
          if bytes >= 0 then ListCaptions.Add(FormatByteNumber(bytes))
          else ListCaptions.Add('');
          // Created:
          ListCaptions.Add( FieldContent(ds, 'Create_time') );
          // Updated:
          ListCaptions.Add( FieldContent(ds, 'Update_time') );
          // Engine
          if ds.FindField('Type') <> nil then
            ListCaptions.Add( FieldContent(ds, 'Type') )
          else
            ListCaptions.Add( FieldContent(ds, 'Engine') );
          // Comment
          ListCaptions.Add( FieldContent(ds, 'Comment') );
          // Version
          ListCaptions.Add( FieldContent(ds, 'Version') );
          // Row format
          ListCaptions.Add( FieldContent(ds, 'Row_format') );
          // Avg row length
          if (FieldContent(ds, 'Avg_row_length') <> '') then
            ListCaptions.Add( FormatByteNumber(FieldContent(ds, 'Avg_row_length')) )
          else ListCaptions.Add('');
          // Max data length
          if (FieldContent(ds, 'Max_data_length') <> '') then
            ListCaptions.Add( FormatByteNumber(FieldContent(ds, 'Max_data_length')) )
          else ListCaptions.Add('');
          // Index length
          if (FieldContent(ds, 'Index_length') <> '') then
            ListCaptions.Add( FormatByteNumber(FieldContent(ds, 'Index_length')) )
          else ListCaptions.Add('');
          // Data free
          if (FieldContent(ds, 'Data_free') <> '') then
            ListCaptions.Add( FormatByteNumber(FieldContent(ds, 'Data_free')) )
          else ListCaptions.Add('');
          // Auto increment
          if (FieldContent(ds, 'Auto_increment') <> '') then
            ListCaptions.Add( FormatNumber(FieldContent(ds, 'Auto_increment')) )
          else ListCaptions.Add('');
          // Check time
          ListCaptions.Add( FieldContent(ds, 'Check_time') );
          // Collation
          ListCaptions.Add( FieldContent(ds, 'Collation') );
          // Checksum
          ListCaptions.Add( FieldContent(ds, 'Checksum') );
          // Create_options
          ListCaptions.Add( FieldContent(ds, 'Create_options') );
          // Object type
          ListCaptions.Add('Base table');
        end;

        NODETYPE_VIEW:
        begin // View
          VTRowDataListTables[i-1].ImageIndex := ICONINDEX_VIEW;
          VTRowDataListTables[i-1].NodeType := NODETYPE_VIEW;
          // Rows
          ListCaptions.Add('');
          // Size
          ListCaptions.Add('');
          // Created:
          ListCaptions.Add('');
          // Updated:
          ListCaptions.Add('');
          // Engine
          ListCaptions.Add('');
          // Comment
          ListCaptions.Add(FieldContent(ds, 'Comment'));
          // Version
          ListCaptions.Add('');
          // Row_format
          ListCaptions.Add('');
          // Avg_row_length
          ListCaptions.Add('');
          // Max_data_length
          ListCaptions.Add('');
          // Index_length
          ListCaptions.Add('');
          // Data_free
          ListCaptions.Add('');
          // Auto_increment
          ListCaptions.Add('');
          // Check_time
          ListCaptions.Add('');
          // Collation
          ListCaptions.Add('');
          // Checksum
          ListCaptions.Add('');
          // Create_options
          ListCaptions.Add('');
          // Object Type
          ListCaptions.Add('View');
        end;
      end;

      VTRowDataListTables[i-1].Captions := ListCaptions;
      ds.Next;
    end;
  finally
    ListTables.RootNodeCount := Length(VTRowDataListTables);
    ListTables.EndUpdate;
    SetVTSelection(ListTables, SelectedCaptions);
    showstatus(db + ': ' + IntToStr(ListTables.RootNodeCount) +' table(s)', 0);
    tabDatabase.Caption := sstr('Database: ' + db, 30);
    ShowStatus(STATUS_MSG_READY);
    Screen.Cursor := crDefault;
    // Ensure tree db node displays its chidren initialized
    DBtree.ReinitChildren(FindDBNode(db), False);
    ValidateControls;
  end;
end;


{ Show tables and their properties on the tabsheet "Database" }
procedure TMainForm.ShowDBProperties(db: WideString);
begin
  Screen.Cursor := crHourglass;
  pcChange( Self );
  ShowStatus( STATUS_MSG_READY );
  Screen.Cursor := crDefault;
end;


procedure TMainForm.ShowTableProperties;
var
  i,j : Integer;
  isFulltext : Boolean;
  dummy: Boolean;
  hasCommentColumn: Boolean;
  SelectedCaptions: WideStrings.TWideStringList;
  defaultVal: WideString;
begin
  // Table-Properties
  dataselected := false;
  Screen.Cursor := crHourGlass;

  tabTable.Caption := sstr('Table: ' + SelectedTable, 30);

  tabDatabase.TabVisible := true;
  tabTable.TabVisible := true;
  tabData.TabVisible := true;

  if (not DBtree.Dragging) and (
   (PageControlMain.ActivePage = tabHost) or
   (PageControlMain.ActivePage = tabDatabase)
  ) then PageControlMain.ActivePage := tabTable;

  ShowStatus( 'Reading table properties...' );
  // Remember selected nodes
  SelectedCaptions := GetVTCaptions(ListColumns, True);
  ListColumns.BeginUpdate;
  ListColumns.Clear;
  FSelectedTableColumns := nil;
  FSelectedTableKeys := nil;
  Try
    // Hide column "Comment" on old servers.
    hasCommentColumn := FSelectedTableColumns.FindField('Comment') <> nil;
    if not hasCommentColumn then
      ListColumns.Header.Columns[5].Options := ListColumns.Header.Columns[5].Options - [coVisible];

    SetLength(VTRowDataListColumns, FSelectedTableColumns.RecordCount);
    for i:=1 to FSelectedTableColumns.RecordCount do
    begin
      VTRowDataListColumns[i-1].ImageIndex := ICONINDEX_FIELD;
      VTRowDataListColumns[i-1].Captions := WideStrings.TWideStringList.Create;
      VTRowDataListColumns[i-1].Captions.Add( FSelectedTableColumns.FieldByName('Field').AsWideString );
      VTRowDataListColumns[i-1].Captions.Add( FSelectedTableColumns.FieldByName('Type').AsWideString );
      if lowercase( FSelectedTableColumns.FieldByName('Null').AsString ) = 'yes' then
        VTRowDataListColumns[i-1].Captions.Add('Yes')
        else VTRowDataListColumns[i-1].Captions.Add('No');

      if FSelectedTableColumns.FieldByName('Default').IsNull then
        // In MySQL, it is not possible to use fx NOW() as a column default.
        // Also, if default is NULL, then the actual default is either NULL
        // or nothing at all.  Looking at another column, "Null", can help
        // determine which one it really is, as can a SHOW CREATE TABLE.
        // According with the above, it is not possible in MySQL to create
        // a column which may be NULL but which has no default value.
        if LowerCase(FSelectedTableColumns.FieldByName('Null').AsString) = 'yes' then
          VTRowDataListColumns[i-1].Captions.Add('NULL')
        else
          // No default value.
          VTRowDataListColumns[i-1].Captions.Add('')
      else begin
        defaultVal := FSelectedTableColumns.FieldByName('Default').AsWideString;
        if UpperCase(defaultVal) <> 'CURRENT_TIMESTAMP' then
          defaultVal := '''' + defaultVal + '''';
        VTRowDataListColumns[i-1].Captions.Add(defaultVal);
      end;

      VTRowDataListColumns[i-1].Captions.Add( FSelectedTableColumns.FieldByName('Extra').AsWideString );
      if hasCommentColumn then
        VTRowDataListColumns[i-1].Captions.Add( FSelectedTableColumns.FieldByName('Comment').AsWideString )
      else
        VTRowDataListColumns[i-1].Captions.Add('');

      FSelectedTableColumns.Next;
    end;

    ListColumns.RootNodeCount := Length(VTRowDataListColumns);

    // Manually invoke OnChange event of tabset to fill helper list with data
    if tabsetQueryHelpers.TabIndex = 0 then
      tabsetQueryHelpers.OnChange( Self, tabsetQueryHelpers.TabIndex, dummy);

    Screen.Cursor := crHourglass;
    for i:=1 to FSelectedTableKeys.RecordCount do
    begin
      // Search for the column name in listColumns
      for j:=0 to Length(VTRowDataListColumns)-1 do
      begin
        if FSelectedTableKeys.FieldByName('Column_name').AsWideString = VTRowDataListColumns[j].Captions[0] then
        begin
          // Only apply a new icon if it was not already changed
          if VTRowDataListColumns[j].ImageIndex <> ICONINDEX_FIELD then
            break;

          // Check if column is part of a fulltext key
          if mysql_version < 40002 then
            isFulltext := (FSelectedTableKeys.FieldByName('Comment').AsString = 'FULLTEXT')
          else
            isFulltext := (FSelectedTableKeys.FieldByName('Index_type').AsString = 'FULLTEXT');

          // Primary key
          if FSelectedTableKeys.FieldByName('Key_name').AsString = 'PRIMARY' then
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_PRIMARYKEY
          // Fulltext index
          else if isFullText then
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_FULLTEXTKEY
          // Unique index
          else if FSelectedTableKeys.FieldByName('Non_unique').AsString = '0' then
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_UNIQUEKEY
          // Normal index
          else
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_INDEXKEY;

          // Column was found and processed
          break;
        end;
      end;
      FSelectedTableKeys.Next;
    end;
    {
      ** note, ansgarbecker, 2007-08-26
      VT has a pretty autosorting feature, which keeps the sorting even after having
      filled it with new data.
      But: Don't use this auto-sorting here, neither automatically nor manual
      because that would cause big confusion to the user if a just clicked
      table displays its fields not in the natural order.

      @todo Detect if the list was just refreshed (and then keep sorting)
      or if another table get displayed (then don't sort, as below)
    }
    ListColumns.Header.SortColumn := -1;
    ListColumns.Header.SortDirection := sdAscending;

  finally

    ListColumns.EndUpdate;
    // Reselect previous selected nodes
    SetVTSelection(ListColumns, SelectedCaptions);
    Screen.Cursor := crDefault;
  end;

  pcChange( Self );
  ShowStatus( STATUS_MSG_READY );
  showstatus(ActiveDatabase + ': '+ SelectedTable + ': ' + IntToStr(ListColumns.RootNodeCount) +' column(s)', 0);
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
  ValidateControls;
end;



{***
  Enable/disable various buttons and menu items.
  Invoked when
    - active sheet changes
    - highlighted database changes
    - ChildWindow is activated / deactivated
  @param Boolean Is this form activated in terms of our remaining MDI-functionality?
    Only used with False by FormDeactivate-procedure to
    deactivate various controls on mainform
}
procedure TMainForm.ValidateControls( FrmIsFocussed: Boolean = true );
var
  DBObjectSelected, TableSelected, ViewSelected,
  inDbTab, inTableTab, inDataTab, inQueryTab, inDataOrQueryTab, inDataOrQueryTabNotEmpty,
  FieldsSelected, FieldFocused, dummy, DBfocused : Boolean;
  NodeData: PVTreeData;
  SelectedNodes: TNodeArray;
begin
  inDbTab := FrmIsFocussed and (PageControlMain.ActivePage = tabDatabase);
  inTableTab := FrmIsFocussed and (PageControlMain.ActivePage = tabTable);
  inDataTab := FrmIsFocussed and (PageControlMain.ActivePage = tabData);
  inDataOrQueryTab := FrmIsFocussed and ((PageControlMain.ActivePage = tabData) or (PageControlMain.ActivePage = tabQuery));
  inDataOrQueryTabNotEmpty := inDataOrQueryTab and (ActiveGrid.RootNodeCount > 0);
  inQueryTab := FrmIsFocussed and (PageControlMain.ActivePage = tabQuery);

  SelectedNodes := ListTables.GetSortedSelection(False);
  DBObjectSelected := (Length(SelectedNodes)>0) and FrmIsFocussed;
  TableSelected := False;
  ViewSelected := False;

  // Check type of first selected node, to en-/disable certain menu items
  if DBObjectSelected then begin
    NodeData := ListTables.GetNodeData( SelectedNodes[0] );
    TableSelected := (NodeData.NodeType = NODETYPE_TABLE) or (NodeData.NodeType = NODETYPE_CRASHED_TABLE);
    ViewSelected := NodeData.NodeType = NODETYPE_VIEW;
  end;

  // Standard toolbar and main menu
  actRefresh.Enabled := FrmIsFocussed;
  actFlushHosts.Enabled := FrmIsFocussed;
  actFlushLogs.Enabled := FrmIsFocussed;
  actFlushPrivileges.Enabled := FrmIsFocussed;
  actFlushTables.Enabled := FrmIsFocussed;
  actFlushTableswithreadlock.Enabled := FrmIsFocussed;
  actFlushStatus.Enabled := FrmIsFocussed;
  actUserManager.Enabled := FrmIsFocussed;
  actMaintenance.Enabled := FrmIsFocussed;
  actInsertFiles.Enabled := FrmIsFocussed;
  // PrintList should only be active if we're focussing one of the ListViews,
  // at least as long we are not able to print DBGrids
  actPrintList.Enabled := FrmIsFocussed;
  actSQLhelp.Enabled := (mysql_version >= 40100) and FrmIsFocussed;
  actImportCSV.Enabled := (mysql_version >= 32206) and FrmIsFocussed;
  actExportTables.Enabled := FrmIsFocussed;

  // Database tab
  actEmptyTables.Enabled := inDbTab and TableSelected;
  actEditTableProperties.Enabled := inDbTab and TableSelected;
  MenuRenameTable.Enabled := inDbTab and DBObjectSelected;
  actCopyTable.Enabled := inDbTab and DBObjectSelected;
  actEditView.Enabled := inDbTab and ViewSelected and (mysql_version >= 50001);
  actCreateView.Enabled := FrmIsFocussed and (ActiveDatabase <> '') and (mysql_version >= 50001);
  actCreateDatabase.Enabled := FrmIsFocussed;
  DBfocused := Assigned(DBtree.FocusedNode) and (DBtree.GetNodeLevel(DBtree.FocusedNode) = 1);
  actDropDatabase.Enabled := DBfocused and FrmIsFocussed;
  actEditDatabase.Enabled := DBfocused and FrmIsFocussed and (mysql_version >= 50002);
  if mysql_version < 50002 then
    actEditDatabase.Hint := STR_NOTSUPPORTED
  else
    actEditDatabase.Hint := 'Rename and/or modify character set of database';
  actDropTablesAndViews.Enabled := (DBObjectSelected and inDbTab) or ((not inQueryTab) and (SelectedTable <> '') and FrmIsFocussed);
  actCreateTable.Enabled := (ActiveDatabase <> '') and FrmIsFocussed;
  actEditTableFields.Enabled := DBObjectSelected and inDbTab;

  // Table tab
  FieldFocused := inTableTab and Assigned(ListColumns.FocusedNode);
  FieldsSelected := inTableTab and (Length(ListColumns.GetSortedSelection(False))>0);
  // Toggle state of menuitems and buttons
  actEditField.Enabled := FieldFocused and FieldsSelected;
  actCreateField.Enabled := inTableTab;
  actDropFields.Enabled := FieldsSelected;
  actEditIndexes.Enabled := inTableTab;
  menuRenameColumn.Enabled := FieldFocused and FieldsSelected;

  // Data tab - if query results are made editable, these will need
  //            to be changed to look at which tab is focused.
  actDataInsert.Enabled := inDataTab;
  actDataDelete.Enabled := inDataTab and (DataGrid.SelectedCount > 0);
  actDataFirst.Enabled := inDataTab;
  actDataLast.Enabled := inDataTab;
  actDataPostChanges.Enabled := inDataTab and DataGridHasChanges;
  actDataCancelChanges.Enabled := inDataTab and DataGridHasChanges;

  // Activate export-options if we're on Data- or Query-tab
  actCopyAsCSV.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsHTML.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsXML.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsSQL.Enabled := inDataOrQueryTabNotEmpty;
  actExportData.Enabled := inDataOrQueryTabNotEmpty;
  actHTMLView.Enabled := inDataOrQueryTabNotEmpty and Assigned(ActiveGrid.FocusedNode);
  setNull1.Enabled := inDataTab and Assigned(DataGrid.FocusedNode);

  // Query tab
  actLoadSQL.Enabled := FrmIsFocussed;
  // Manually invoke OnChange event of tabset to fill helper list with data
  if inQueryTab and FrmIsFocussed then
    tabsetQueryHelpers.OnChange(Self, tabsetQueryHelpers.TabIndex, dummy);
  ValidateQueryControls(FrmIsFocussed);

  if not inQueryTab then // Empty panel with "Line:Char"
    showstatus('', 1);

  if not FrmIsFocussed then begin
    // Empty "connected" and "uptime"
    showstatus('', 1);
    showstatus('', 2);
    showstatus('', 3);
    showstatus('', 4);
  end;
end;

procedure TMainForm.ValidateQueryControls(FrmIsFocussed: Boolean = true);
var
  InQueryTab, NotEmpty, HasSelection: Boolean;
begin
  InQueryTab := FrmIsFocussed and (PageControlMain.ActivePage = tabQuery);
  NotEmpty := FrmIsFocussed and (SynMemoQuery.GetTextLen > 0);
  HasSelection := FrmIsFocussed and SynMemoQuery.SelAvail;
  actExecuteQuery.Enabled := InQueryTab and NotEmpty;
  actExecuteSelection.Enabled := InQueryTab and HasSelection;
  actExecuteLine.Enabled := InQueryTab and (SynMemoQuery.LineText <> '');
  actSaveSQL.Enabled := InQueryTab and NotEmpty;
  actSaveSQLselection.Enabled := InQueryTab and HasSelection;
  actSaveSQLSnippet.Enabled := InQueryTab and NotEmpty;
  actSaveSQLSelectionSnippet.Enabled := InQueryTab and HasSelection;
  actQueryFind.Enabled := InQueryTab and NotEmpty;
  actQueryReplace.Enabled := InQueryTab and NotEmpty;
  actQueryStopOnErrors.Enabled := InQueryTab;
  actQueryWordWrap.Enabled := InQueryTab;
  actClearQueryEditor.Enabled := InQueryTab and NotEmpty;
  actSetDelimiter.Enabled := InQueryTab;
end;


procedure TMainForm.ShowTableData(table: WideString);
begin
  dataselected := false;
  PageControlMain.ActivePage := tabData;
  viewdata(self);
  pcChange( Self );
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
  ColName           : WideString;
  col               : TVirtualTreeColumn;
begin
  if CurrentLine then SQL := parseSQL(SynMemoQuery.LineText)
  else if Selection then SQL := parseSQL(SynMemoQuery.SelText)
  else SQL := parseSQL(SynMemoQuery.Text);

  if ( SQL.Count = 0 ) then
  begin
    LabelResultinfo.Caption := '(nothing to do)';
    Exit;
  end;

  SQLscriptstart := GetTickCount();
  LastVistaCheck := GetTickCount();
  LabelResultinfo.Caption := '';

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
      LabelResultinfo.Caption := '';
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

      LabelResultinfo.Caption :=
        FormatNumber( rowsaffected ) +' row(s) affected, '+
        FormatNumber( fieldcount ) +' column(s) x '+
        FormatNumber( recordcount ) +' row(s) in last result set.';
      if ( SQL.Count = 1 ) then
      begin
        LabelResultinfo.Caption := LabelResultinfo.Caption +
          ' Query time: '+ FormatNumber( SQLTime, 3) +' sec.';
      end;
    end;

    ProgressBarStatus.Hide;
    ValidateQueryControls;

    if ( SQL.Count > 1 ) then
    begin
      SQLscriptend := GetTickCount();
      SQLTime := (SQLscriptend - SQLscriptstart) / 1000;
      LabelResultinfo.Caption := LabelResultinfo.Caption +' Batch time: '+
        FormatNumber( SQLTime, 3 ) +' sec.';
    end;

  finally
    // Let EnsureActiveDatabase know that we've fired user queries.
    UserQueryFired := true;
    UserQueryFiring := false;

    // Avoid excessive GridHighlightChanged() when flicking controls.
    viewingdata := true;

    if ds <> nil then begin
      QueryGrid.BeginUpdate;
      QueryGrid.Header.Options := QueryGrid.Header.Options + [hoVisible];
      QueryGrid.Header.Columns.BeginUpdate;
      QueryGrid.Header.Columns.Clear;
      debug('mem: clearing and initializing query columns.');
      SetLength(FQueryGridResult.Columns, 0);
      SetLength(FQueryGridResult.Columns, ds.FieldCount);
      for i:=0 to ds.FieldCount-1 do begin
        ColName := ds.Fields[i].FieldName;
        col := QueryGrid.Header.Columns.Add;
        col.Text := ColName;
        col.Options := col.Options - [coAllowClick];
        FQueryGridResult.Columns[i].Name := ColName;
        if ds.Fields[i].DataType in [ftSmallint, ftInteger, ftWord, ftLargeint] then begin
          FQueryGridResult.Columns[i].IsInt := True;
          col.Alignment := taRightJustify;
        end else if ds.Fields[i].DataType in [ftFloat] then begin
          FQueryGridResult.Columns[i].IsFloat := True;
          col.Alignment := taRightJustify;
        end else if ds.Fields[i].DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
          FQueryGridResult.Columns[i].IsDate := True
        else if ds.Fields[i].DataType in [ftWideString, ftMemo, ftWideMemo] then
          FQueryGridResult.Columns[i].IsText := True
        else if ds.Fields[i].DataType in [ftBlob] then
          FQueryGridResult.Columns[i].IsBinary := True;
      end;
      debug('mem: query column initialization complete.');
      debug('mem: clearing and initializing query rows (internal data).');
      SetLength(FQueryGridResult.Rows, 0);
      SetLength(FQueryGridResult.Rows, ds.RecordCount);
      ds.First;
      for i:=0 to ds.RecordCount-1 do begin
        FQueryGridResult.Rows[i].Loaded := True;
        SetLength(FQueryGridResult.Rows[i].Cells, ds.FieldCount);
        for j:=0 to ds.FieldCount-1 do begin
          if FQueryGridResult.Columns[j].IsBinary then
            FQueryGridResult.Rows[i].Cells[j].Text := '0x' + BinToWideHex(ds.Fields[j].AsString)
          else
            FQueryGridResult.Rows[i].Cells[j].Text := ds.Fields[j].AsWideString;
          FQueryGridResult.Rows[i].Cells[j].IsNull := ds.Fields[j].IsNull;
        end;
        ds.Next;
      end;
      ds.Free;
      debug('mem: initializing query rows (grid).');
      QueryGrid.RootNodeCount := Length(FQueryGridResult.Rows);
      debug('mem: query row initialization complete.');
      QueryGrid.Header.Columns.EndUpdate;
      QueryGrid.ClearSelection;
      QueryGrid.OffsetXY := Point(0, 0);
      QueryGrid.EndUpdate;
      AutoCalcColWidths(QueryGrid);
    end;
    // Ensure controls are in a valid state
    ValidateControls;
    viewingdata := false;
    Screen.Cursor := crDefault;
    ShowStatus( STATUS_MSG_READY );
  end;
end;


{**
  Clicked somewhere in the field-list of the "Table"-tabsheet
}
procedure TMainForm.ListColumnsChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  ValidateControls;
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

  procedure addTable( Fields: TFields );
  var ObjName, ObjType: WideString;
  begin
    ObjName := Fields[0].AsWideString;
    case GetDBObjectType(Fields) of
      NODETYPE_CRASHED_TABLE: ObjType := 'table';
      NODETYPE_TABLE: ObjType := 'table';
      NODETYPE_VIEW: ObjType := 'view';
      else ObjType := 'unknown';
    end;
    SynCompletionProposal1.InsertList.Add( ObjName );
    SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(SynSQLSyn1.TableNameAttri.Foreground)+'}'+ObjType+'\color{clWindowText}\column{}' + ObjName );
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
      SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(clTeal)+'}column\color{clWindowText}\column{}' + ds.FieldByName( 'Field' ).AsWideString + '\style{-B} ' + ds.FieldByName( 'Type' ).AsString );
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
  end;

begin
  SynCompletionProposal1.InsertList.Clear;
  SynCompletionProposal1.ItemList.Clear;

  // Get column-names into the proposal pulldown
  // when we write sql like "SELECT t.|col FROM table [AS] t"
  // Current limitation: Identifiers (masked or not) containing
  // spaces are not detected correctly.

  // 1. find the currently edited sql-statement around the cursor position in synmemo
  j := Length(SynCompletionProposal1.Editor.Text);
  for i := SynCompletionProposal1.Editor.SelStart+1024 downto SynCompletionProposal1.Editor.SelStart-1024 do
  begin
    if i > j then
      continue;
    if i < 1 then
      break;
    sql := SynCompletionProposal1.Editor.Text[i] + sql;
  end;

  // 2. Parse FROM clause to detect relevant table/view, probably aliased
  rx := TRegExpr.Create;
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
    rx.Expression := '`?(\w+)`?(\s+(AS\s+)?`?(\w+)`?)?';
    for i := 0 to Tables.Count - 1 do begin
      // If the just typed word equals the alias of this table or the
      // tablename itself, set tablename var and break loop
      if rx.Exec(Tables[i]) then begin
        if (WideDequotedStr(SynCompletionProposal1.PreviousToken,'`') = WideDequotedStr(rx.Match[4],'`') )
          or (SynCompletionProposal1.PreviousToken = rx.Match[1]) then begin
          tablename := Trim(rx.Match[1]);
          break;
        end;
      end;
    end;
  end;
  rx.Free;

  if (tablename <> '') then begin
    // add columns to proposal
    addColumns( tablename );
  end else if SynCompletionProposal1.PreviousToken <> '' then begin
    // assuming previoustoken itself is a table
    addColumns( SynCompletionProposal1.PreviousToken );
  end;


  if Length(CurrentInput) = 0 then // makes only sense if the user has typed "database."
  begin
    i := Databases.IndexOf( SynCompletionProposal1.PreviousToken );
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

  if (SynCompletionProposal1.ItemList.count = 0) and (Length(CurrentInput)>0) then
  begin
    // Add databases
    for i := 0 to Databases.Count - 1 do begin
      SynCompletionProposal1.InsertList.Add(Databases[i]);
      SynCompletionProposal1.ItemList.Add(Databases[i]);
    end;
    for i:=0 to SynCompletionProposal1.ItemList.count-1 do
      SynCompletionProposal1.ItemList[i] := '\hspace{2}\color{'+ColorToString(SynSQLSyn1.TableNameAttri.Foreground)+'}database\color{clWindowText}\column{}' + SynCompletionProposal1.ItemList[i];

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
    for i := 0 to Length(MySQLFunctions) - 1 do
    begin
      // Don't display unsupported functions here
      if MySqlFunctions[i].Version > mysql_version then
        continue;
      SynCompletionProposal1.InsertList.Add( MySQLFunctions[i].Name + MySQLFunctions[i].Declaration );
      SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(SynSQLSyn1.FunctionAttri.Foreground)+'}function\color{clWindowText}\column{}' + MySQLFunctions[i].Name + '\style{-B}' + MySQLFunctions[i].Declaration );
    end;

    // Add keywords
    for i := 0 to MYSQL_KEYWORDS.Count - 1 do
    begin
      SynCompletionProposal1.InsertList.Add( MYSQL_KEYWORDS[i] );
      SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(SynSQLSyn1.KeyAttri.Foreground)+'}keyword\color{clWindowText}\column{}'+MYSQL_KEYWORDS[i] );
    end;

  end;

end;


procedure TMainForm.SynMemoQueryStatusChange(Sender: TObject; Changes:
    TSynStatusChanges);
var
  sm: TSynMemo;
begin
  sm := Sender as TSynMemo;
  ValidateQueryControls;
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

procedure TMainForm.MenuRenameTableClick(Sender: TObject);
begin
  // menuitem for edit table-name
  ListTables.EditNode( ListTables.FocusedNode, 0 );
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


procedure TMainForm.ListTablesDblClick(Sender: TObject);
begin
  actEditTableFields.Execute;
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
  IsNull := FDataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].IsNull;
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
  SynMemoQuery.SetFocus;
end;

procedure TMainForm.popupResultGridPopup(Sender: TObject);
begin
  // data available?
  // Save2CSV.enabled :=
end;

procedure TMainForm.Autoupdate1Click(Sender: TObject);
var
  seconds : String;
  secondsInt : Integer;
begin
  // set interval for autorefresh-timer
  seconds := IntToStr(TimerRefresh.interval div 1000);
  if inputquery('Auto-refresh processlist','Update list every ... seconds:', seconds) then begin
    secondsInt := StrToIntDef(seconds, 0);
    if secondsInt > 0 then begin
      TimerRefresh.Interval := secondsInt * 1000;
      TimerRefresh.Enabled := true;
      EnableAutoRefresh.Checked := true;
      DisableAutoRefresh.Checked := false;
    end
    else
      MessageDLG('Seconds must be between 1 and ' + IntToStr(maxint) + '.', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.EnableAutoRefreshClick(Sender: TObject);
begin
  // enable autorefresh-timer
  TimerRefresh.Enabled := true;
  EnableAutoRefresh.Checked := true;
  DisableAutoRefresh.Checked := false;
end;

procedure TMainForm.DisableAutoRefreshClick(Sender: TObject);
begin
  // enable autorefresh-timer
  TimerRefresh.Enabled := false;
  EnableAutoRefresh.Checked := false;
  DisableAutoRefresh.Checked := true;
end;



procedure TMainForm.SynMemoQueryDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  src : TControl;
begin
  // dragging an object over the query-memo
  src := Source as TControl;
  // Accepting drag's from DBTree and QueryHelpers
  Accept := (src = DBtree) or (src = lboxQueryHelpers);
  // set x-position of cursor
  SynMemoQuery.CaretX := (x - SynMemoQuery.Gutter.Width) div SynMemoQuery.CharWidth - 1 + SynMemoQuery.LeftChar;
  // set y-position of cursor
  SynMemoQuery.CaretY := y div SynMemoQuery.LineHeight + SynMemoQuery.TopLine;
  if not SynMemoQuery.Focused then
    SynMemoQuery.SetFocus;
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
  SynMemoQuery.UndoList.AddGroupBreak;
  src := Source as TControl;
  Text := 'Error: Unspecified source control in drag''n drop operation!';
  LoadText := True;
  // Check for allowed controls as source has already
  // been performed in OnDragOver. So, only do typecasting here.
  if src = DBtree then
    Text := DBtree.Text[DBtree.GetFirstSelected, 0]
  else if (src = lboxQueryHelpers) and (lboxQueryHelpers.ItemIndex > -1) then begin
    // Snippets tab
    if tabsetQueryHelpers.TabIndex = 3 then begin
      QueryLoad( DIRNAME_SNIPPETS + lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex] + '.sql', False );
      LoadText := False;
    // All other tabs
    end else begin
      Text := '';
      for i := 0 to lboxQueryHelpers.Items.Count - 1 do begin
        if lboxQueryHelpers.Selected[i] then
          Text := Text + lboxQueryHelpers.Items[i] + ', ';
      end;
      Delete(Text, Length(Text)-1, 2);
    end;
  end;
  // Only insert text if no previous action did the job.
  // Should be false when dropping a snippet-file here
  if LoadText then
    SynMemoQuery.SelText := Text;
  SynMemoQuery.UndoList.AddGroupBreak;
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

procedure TMainForm.popupTreeViewPopup(Sender: TObject);
var
  L: Cardinal;
begin
  // toggle drop-items and remember right-clicked item
  if DBtree.GetFirstSelected = nil then
    L := 0
  else
    L := DBtree.GetNodeLevel(DBtree.GetFirstSelected);
  actCreateTable.Enabled := L in [1,2];
  actCreateView.Enabled := (L in [1,2]) and (mysql_version >= 50001);
  actEditTableProperties.Enabled := (L = 2) and ((GetSelectedNodeType = NODETYPE_TABLE) or (GetSelectedNodeType = NODETYPE_CRASHED_TABLE));
  actEditView.Enabled := (L = 2) and (GetSelectedNodeType = NODETYPE_VIEW);
  actDropTablesAndViews.Enabled := (L = 2);
end;




procedure TMainForm.QueryLoad( filename: String; ReplaceContent: Boolean = true );

var
  filecontent      : WideString;
  msgtext          : String;
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
    PagecontrolMain.ActivePage := tabQuery;
    SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
    SynMemoQuery.BeginUpdate;
    if ReplaceContent then
      SynMemoQuery.SelectAll;
    SynMemoQuery.SelText := filecontent;
    SynMemoQuery.SelStart := SynMemoQuery.SelEnd;
    SynMemoQuery.EndUpdate;
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
  if FDataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].IsNull then begin
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
  This returns the GridResult object that is currently visible to the user,
  depending on with tabsheet is active.

  @return PGridResult if data/query tab is active, nil otherwise.
}
function TMainForm.GetVisualDataset: PGridResult;
begin
  Result := nil;
  case PageControlMain.ActivePageIndex of
    3: Result := @FDataGridResult;
    4: Result := @FQueryGridResult;
  end;
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
        DisableAutoRefreshClick(self);
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


// Searchbox unfocused
procedure TMainForm.CheckConnection;
begin
  if not FMysqlConn.IsAlive then begin
    LogSQL('Connection failure detected. Trying to reconnect.', true);
    TimerConnected.Enabled := false;
    TimerConnectedTimer(self);
    TimerHostUptime.Enabled := false;
    TimerHostUptimeTimer(self);
    FQueryRunning := false;
    try
      FMysqlConn.Connection.Reconnect;
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



function TMainForm.GetActiveGrid: TVirtualStringTree;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := DataGrid
  else if PageControlMain.ActivePage = tabQuery then Result := QueryGrid;
end;

function TMainForm.GetActiveData: PGridResult;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := @FDataGridResult
  else if PageControlMain.ActivePage = tabQuery then Result := @FQueryGridResult;
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


function TMainForm.GetSelectedTable: WideString;
begin
  if DBtree.GetFirstSelected = nil then Result := ''
  else case DBtree.GetNodeLevel(DBtree.GetFirstSelected) of
      2: Result := DBtree.Text[DBtree.GetFirstSelected, 0];
    else Result := '';
  end;
end;


function TMainForm.GetNodeType(Node: PVirtualNode): Byte;
var
  ds: TDataset;
begin
  Result := NODETYPE_DEFAULT;
  if Assigned(Node) then case DBtree.GetNodeLevel(Node) of
    1: Result := NODETYPE_DB;
    2: begin
      ds := FetchDbTableList(DBTree.Text[Node.Parent, 0]);
      ds.RecNo := Node.Index+1;
      Result := GetDBObjectType(ds.Fields);
    end;
  end;
end;

function TMainForm.GetSelectedNodeType: Byte;
begin
  Result := GetNodeType(DBtree.GetFirstSelected);
end;


procedure TMainForm.SetSelectedTable(table: WideString);
var
  i: integer;
  dbnode, tnode, snode: PVirtualNode;
begin
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
    if DBtree.Text[tnode, 0] = table then begin
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
      if AnsiCompareText(DBtree.Text[tnode, 0], table) = 0 then begin
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
    exit;
  end;
  raise Exception.Create('Table node ' + table + ' not found in tree.');
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
  lboxQueryHelpers.Items.BeginUpdate;
  lboxQueryHelpers.Items.Clear;
  // By default sorted alpabetically
  lboxQueryHelpers.Sorted := True;
  // By default disable all items in popupmenu, enable them when needed
  menuInsertSnippetAtCursor.Enabled := False;
  menuLoadSnippet.Enabled := False;
  menuDeleteSnippet.Enabled := False;
  menuExplore.Enabled := False;
  menuHelp.Enabled := False;
  lboxQueryHelpers.MultiSelect := True;

  case NewTab of
    0: // Cols
    begin
      // Keep native order of columns
      lboxQueryHelpers.Sorted := False;
      if SelectedTable <> '' then for i := 0 to High(VTRowDataListColumns) do
      begin
        lboxQueryHelpers.Items.Add(VTRowDataListColumns[i].Captions[0]);
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
        lboxQueryHelpers.Items.Add( MySQLFunctions[i].Name + MySQLFunctions[i].Declaration );
      end;
    end;

    2: // SQL keywords
    begin
      // State of items in popupmenu
      menuHelp.Enabled := True;
      for i := 0 to MYSQL_KEYWORDS.Count - 1 do
        lboxQueryHelpers.Items.Add(MYSQL_KEYWORDS[i]);
    end;

    3: // SQL Snippets
    begin
      lboxQueryHelpers.MultiSelect := False;
      Files := getFilesFromDir( DIRNAME_SNIPPETS, '*.sql', true );
      for i := 0 to Files.Count - 1 do
        lboxQueryHelpers.Items.Add(Files[i]);
	  Files.Free;
      // State of items in popupmenu
      SnippetsAccessible := lboxQueryHelpers.Items.Count > 0;
      menuDeleteSnippet.Enabled := SnippetsAccessible;
      menuInsertSnippetAtCursor.Enabled := SnippetsAccessible;
      menuLoadSnippet.Enabled := SnippetsAccessible;
      menuExplore.Enabled := True;
    end;

  end;

  // Restore last selected item in tab
  for i := 0 to Length(QueryHelpersSelectedItems[NewTab]) - 1 do begin
    idx := QueryHelpersSelectedItems[NewTab][i];
    if idx < lboxQueryHelpers.Count then
      lboxQueryHelpers.Selected[idx] := True;
  end;

  lboxQueryHelpers.Items.EndUpdate;

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
  for i := 0 to lboxQueryHelpers.Items.Count - 1 do begin
    if lboxQueryHelpers.Selected[i] then
      text := text + lboxQueryHelpers.Items[i] + ', ';
  end;
  Delete(text, Length(text)-1, 2);

  case tabsetQueryHelpers.TabIndex of
    3: // Load snippet file �nto query-memo
      QueryLoad( DIRNAME_SNIPPETS + lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex] + '.sql', False );
    else // For all other tabs just insert the item from the list
      SynMemoQuery.SelText := text;
  end;

  SynMemoQuery.SetFocus;
end;


{**
  Remember last used items in query helper tabs
}
procedure TMainForm.lboxQueryHelpersClick(Sender: TObject);
var
  i, s, idx: Integer;
begin
  s := tabsetQueryHelpers.TabIndex;
  SetLength(QueryHelpersSelectedItems[s], 0);
  for i := 0 to lboxQueryHelpers.Count - 1 do if lboxQueryHelpers.Selected[i] then begin
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
    sm := SynMemoQuery;
  // Restore function name from array
  f := MySQLFunctions[TControl(Sender).tag].Name
    + MySQLFunctions[TControl(Sender).tag].Declaration;
  sm.UndoList.AddGroupBreak;
  sm.SelText := f;
  sm.UndoList.AddGroupBreak;
  if not SynMemoFilter.Focused then
    ValidateQueryControls;
end;


{**
  Activate inline-item-editor of listColumns
}
procedure TMainForm.menuRenameColumnClick(Sender: TObject);
begin
  ListColumns.EditNode(ListColumns.FocusedNode, 0);
end;


{**
  Rename a column name from within listColumns
}
procedure TMainForm.ListColumnsNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  def : TDataSet;
  sql_update, sql_null, sql_default, sql_extra, sql_comment, DefaultValue,
  InputStr, OutputStr : WideString;
  NodeData : PVTreeData;
  DataViews: TStringList;
  rx: TRegExpr;
  i: Integer;
begin
  // Try to rename, on any error abort and don't rename ListItem
  try
    ensureValidIdentifier( NewText );

    // Fetch data from listitem
    NodeData := ListColumns.GetNodeData(Node);

    // Fetch column definition
    def := GetResults( 'SHOW FULL COLUMNS FROM ' + mask(SelectedTable) + ' LIKE ' + esc(NodeData.Captions[0]), False, False );

    // Check NOT NULL
    sql_null := 'NULL ';
    if UpperCase(def.FieldByName('Null').AsString) = 'NO' then
      sql_null := 'NOT NULL ';

    // Check default value, take care of non-literals / functions
    sql_default := '';
    DefaultValue := def.FieldByName('Default').AsWideString;
    if DefaultValue <> '' then
    begin
      if (UpperCase(def.FieldByName('Type').AsString) <> 'TIMESTAMP') and (DefaultValue <> 'CURRENT_TIMESTAMP') then
        DefaultValue := esc(DefaultValue);
      sql_default := 'DEFAULT ' + DefaultValue + ' ';
    end;

    // Check extra options (auto_increment)
    sql_extra := '';
    if def.FieldByName('Extra').AsString <> '' then
      sql_extra := ' '+WideUpperCase(def.FieldByName('Extra').AsString);

    // Comment
    sql_comment := '';
    if def.FieldByName('Comment').AsWideString <> '' then
      sql_comment := ' COMMENT '+esc(def.FieldByName('Comment').AsWideString);

    // Concat column definition
    sql_update := 'ALTER TABLE ' + mask(SelectedTable) +
      ' CHANGE ' + mask(NodeData.Captions[0]) +
      ' ' + mask(NewText) + ' ' +
      def.FieldByName('Type').AsString + ' ' +
      sql_null +
      sql_default +
      sql_extra +
      sql_comment;

    // Cleanup
    def.Close;
    FreeAndNil(def);

    // Fire ALTER query
    ExecUpdateQuery( sql_update, False, False );

    FSelectedTableColumns := nil;
    FSelectedTableKeys := nil;

    // Fix perspectives, using old column name
    DataViews := TStringList.Create;
    GetDataViews(DataViews);
    for i := 0 to DataViews.Count - 1 do begin
      MainReg.OpenKey(GetRegKeyTable + '\' + REGPREFIX_DATAVIEW + DataViews[i], False);
      rx := TRegExpr.Create;
      rx.Expression := '\b(\d_)('+QuoteRegExprMetaChars(NodeData.Captions[0])+')(\'+REGDELIM+')';
      rx.ModifierG := False;
      InputStr := Utf8Decode(MainReg.ReadString(REGNAME_SORT));
      OutputStr := rx.Replace(InputStr, '$1'+NewText+'$3', True);
      if InputStr <> OutputStr then
        MainReg.WriteString(REGNAME_SORT, Utf8Encode(OutputStr));
    end;
    // Fix in memory perspective
    if DataGridTable = SelectedTable then begin
      for i:=0 to Length(FDataGridSort)-1 do begin
        if FDataGridSort[i].ColumnName = NodeData.Captions[0] then
          FDataGridSort[i].ColumnName := NewText;
      end;
    end;

    // Update listitem
    NodeData.Captions[0] := NewText;
  except
    On E : Exception do
    begin
      MessageDlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;
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
  if lboxQueryHelpers.ItemIndex = -1 then
    abort;

  snippetfile := DIRNAME_SNIPPETS + lboxQueryHelpers.Items[ lboxQueryHelpers.ItemIndex ] + '.sql';
  if MessageDlg( 'Delete snippet file? ' + CRLF + snippetfile, mtConfirmation, [mbOk, mbCancel], 0) = mrOk then
  begin
    Screen.Cursor := crHourGlass;
    if DeleteFile( snippetfile ) then
    begin
      // Refresh list with snippets
      mayChange := True; // Unused; satisfies callee parameter collection which is probably dictated by tabset.
      tabsetQueryHelpersChange( Sender, tabsetQueryHelpers.TabIndex, mayChange );
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
  QueryLoad( DIRNAME_SNIPPETS + lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex] + '.sql', False );
end;


{**
  Load snippet and replace content
}
procedure TMainForm.menuLoadSnippetClick(Sender: TObject);
begin
  QueryLoad( DIRNAME_SNIPPETS + lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex] + '.sql', True );
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
procedure TMainForm.vstHeaderClick(Sender: TVTHeader; Column:
    TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Don't call sorting procedure on right click
  // Some list-headers have a contextmenu which should popup then.
  if Button = mbRight then
    Exit;

  if Sender.SortColumn <> Column then
    Sender.SortColumn := Column
  else if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  Sender.Treeview.SortTree( Column, Sender.SortDirection );
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
  else if VT = ListColumns then
    Result := @VTRowDataListColumns
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
  if P = @VTRowDataListColumns then Exit;
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
  ColWidths, ColsVisible, ColPos : String;
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
  MainReg.WriteString( REGPREFIX_COLWIDTHS + List.Name, ColWidths );
  MainReg.WriteString( REGPREFIX_COLSVISIBLE + List.Name, ColsVisible );
  MainReg.WriteString( REGPREFIX_COLPOS + List.Name, ColPos );
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
begin
  ValueList := WideStrings.TWideStringList.Create;

  // Column widths
  Value := GetRegValue(REGPREFIX_COLWIDTHS + List.Name, '');
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
  Value := GetRegValue(REGPREFIX_COLSVISIBLE + List.Name, '');
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    SetVisibleListColumns( List, ValueList );
  end;

  // Column position
  Value := GetRegValue(REGPREFIX_COLPOS + List.Name, '');
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
  if (dsShowEngines = nil) and (dsHaveEngines = nil) then
  begin
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

  // Needs a refresh to apply visible states
  VT.Refresh;
  VT.UpdateScrollBars(True);
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
            CellText := ds.Fields[0].AsWideString;
          end;
      end;
    1: case GetNodeType(Node) of
        // Calculate and display the sum of all table sizes in ALL dbs if all table lists are cached
        NODETYPE_DEFAULT: begin
            AllListsCached := true;
            for i := 0 to Databases.Count - 1 do begin
              if not DbTableListCached(Databases[i]) then begin
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
        NODETYPE_DB: begin
            db := DBtree.Text[Node, 0];
            if not DbTableListCached(db) then
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
        NODETYPE_TABLE: begin
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
          NODETYPE_TABLE:
            if Kind = ikSelected then
              ImageIndex := ICONINDEX_TABLE_HIGHLIGHT
              else ImageIndex := ICONINDEX_TABLE;
          NODETYPE_VIEW:
            if Kind = ikSelected then
              ImageIndex := ICONINDEX_VIEW_HIGHLIGHT
              else ImageIndex := ICONINDEX_VIEW;
          NODETYPE_CRASHED_TABLE:
            if Kind = ikSelected then
              ImageIndex := ICONINDEX_CRASHED_TABLE_HIGHLIGHT
              else ImageIndex := ICONINDEX_CRASHED_TABLE;
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
procedure TMainForm.DBtreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  newDb: WideString;
begin
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
        ShowTable( (Sender as TVirtualStringTree).Text[Node, 0] );
      end;
  end;
  if newDb <> '' then
    LoadDatabaseProperties(newDb);
  if PageControlMain.ActivePage = tabData then
    viewData(self);
end;


procedure TMainForm.DBtreeDblClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  // Paste DB or table name into query window on treeview double click.
  Node := DBtree.GetFirstSelected;
  if not Assigned(Node) then Exit;
  if DBtree.GetNodeLevel(Node) = 0 then Exit;
  if PageControlMain.ActivePage <> tabQuery then Exit;
  SynMemoQuery.SelText := DBtree.Text[Node, 0];
  SynMemoQuery.SetFocus;
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
  oldActiveDatabase, oldSelectedTable, db: WideString;
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
    if oldSelectedTable <> '' then SelectedTable := oldSelectedTable;
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


function TMainForm.DbTableListCached(db: WideString): Boolean;
begin
  Result := CachedTableLists.IndexOf(db) > -1;
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
    for i := 0 to Length(VTRowDataListColumns) - 1 do begin
      if i > 0 then
        Add := Add + ' OR ';
      Add := Add + mask(VTRowDataListColumns[i].Captions[0]) + ' LIKE ' + esc('%'+ed.Text+'%');
      if Length(Add) > 45 then begin
        Clause := Clause + Add + CRLF;
        Add := '';
      end;
    end;
    if Add <> '' then
      Clause := Clause + Add;
  end;
  SynMemoFilter.UndoList.AddGroupBreak;
  SynMemoFilter.SelectAll;
  SynMemoFilter.SelText := Clause;
  SynMemoFilterChange(Sender);
end;


procedure TMainForm.ListColumnsDblClick(Sender: TObject);
begin
  actEditField.Execute;
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
  res: PGridResult;
  query: WideString;
  ds: TDataSet;
  i, j: LongInt;
begin
  if Sender = DataGrid then res := @FDataGridResult
  else res := @FQueryGridResult;
  if (not res.Rows[Node.Index].Loaded) and (res.Rows[Node.Index].State <> grsInserted) then begin
    query := DataGridCurrentSelect;
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
        if res.Columns[j].IsBinary then
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

procedure TMainForm.EnsureChunkLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  res: PGridResult;
  start, limit: Cardinal;
  query: WideString;
  ds: TDataSet;
  i, j: LongInt;
  regCrashIndicName: String;
begin
  if Sender = DataGrid then res := @FDataGridResult
  else res := @FQueryGridResult;
  if (not res.Rows[Node.Index].Loaded) and (res.Rows[Node.Index].State <> grsInserted) then begin
    start := Node.Index - (Node.Index mod GridMaxRows);
    limit := TVirtualStringTree(Sender).RootNodeCount - start;
    if limit > GridMaxRows then limit := GridMaxRows;
    query := DataGridCurrentSelect;
    if DataGridCurrentFilter <> '' then query := query + ' WHERE ' + DataGridCurrentFilter;
    if DataGridCurrentSort <> '' then query := query + ' ORDER BY ' + DataGridCurrentSort;
    query := query + WideFormat(' LIMIT %d, %d', [start, limit]);

    // Set indicator for possibly crashing query
    OpenRegistry(SessionName);
    regCrashIndicName := Utf8Encode(REGPREFIX_CRASH_IN_DATA + ActiveDatabase + '.' + SelectedTable);
    MainReg.WriteBool(regCrashIndicName, True);

    // start query
    ShowStatus('Retrieving data...');
    debug(Format('mem: loading data chunk from row %d to %d', [start, limit]));
    ds := GetResults(query);
    if Cardinal(ds.RecordCount) < limit then begin
      limit := ds.RecordCount;
      TVirtualStringTree(Sender).RootNodeCount := start + limit;
      SetLength(res.Rows, start + limit);
    end;
    debug(Format('mem: loaded data chunk from row %d to %d', [start, limit]));

    // Query was completed successfully. Reset crash indicator.
    MainReg.DeleteValue(regCrashIndicName);

    // fill in data
    ShowStatus('Filling grid with record-data...');
    for i := start to start + limit - 1 do begin
      SetLength(res.Rows[i].Cells, ds.Fields.Count);
      for j := 0 to ds.Fields.Count - 1 do begin
        if res.Columns[j].IsBinary then
          res.Rows[i].Cells[j].Text := '0x' + BinToWideHex(ds.Fields[j].AsString)
        else
          res.Rows[i].Cells[j].Text := ds.Fields[j].AsWideString;
        res.Rows[i].Cells[j].IsNull := ds.Fields[j].IsNull;
      end;
      res.Rows[i].Loaded := True;
      ds.Next;
    end;

    ShowStatus( STATUS_MSG_READY );
    FreeAndNil(ds);
  end;
end;

procedure TMainForm.DiscardNodeData(Sender: TVirtualStringTree; Node: PVirtualNode);
var
  Data: PGridResult;
begin
  // Avoid discarding query data as it will never be reloaded.
  if Sender <> DataGrid then Exit;
  Data := @FDataGridResult;
  // Avoid rows being edited.
  if Data.Rows[Node.Index].State = grsDefault then begin
    Data.Rows[Node.Index].Loaded := false;
    SetLength(Data.Rows[Node.Index].Cells, 0);
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
  gr: PGridResult;
  EditingCell: Boolean;
begin
  if Column = -1 then
    Exit;
  if Sender = DataGrid then gr := @FDataGridResult
  else gr := @FQueryGridResult;
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
begin
  prefNullColorNumeric := ColorAdjustBrightness(prefFieldColorNumeric, COLORSHIFT_NULLFIELDS);
  prefNullColorText := ColorAdjustBrightness(prefFieldColorText, COLORSHIFT_NULLFIELDS);
  prefNullColorBinary := ColorAdjustBrightness(prefFieldColorBinary, COLORSHIFT_NULLFIELDS);
  prefNullColorDatetime := ColorAdjustBrightness(prefFieldColorDatetime, COLORSHIFT_NULLFIELDS);
  prefNullColorEnum := ColorAdjustBrightness(prefFieldColorEnum, COLORSHIFT_NULLFIELDS);
  prefNullColorSet := ColorAdjustBrightness(prefFieldColorSet, COLORSHIFT_NULLFIELDS);
  prefNullColorDefault := ColorAdjustBrightness(clWindow, COLORSHIFT_NULLFIELDS);
end;


{**
  Cell in data- or query grid gets painted. Colorize font. This procedure is
  called extremely often for repainting the grid cells. Keep it highly optimized.
}
procedure TMainForm.GridPaintText(Sender: TBaseVirtualTree; const
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType:
    TVSTTextType);
var
  isNull: Boolean;
  cl: TColor;
  r: PGridResult;
begin
  if Column = -1 then
    Exit;

  if Sender = DataGrid then r := @FDataGridResult
  else r := @FQueryGridResult;

  if Node.Index >= Cardinal(Length(r.Rows)) then
    Exit;
    
  // Make primary key columns bold
  if r.Columns[Column].IsPriPart then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];

  // NULL value
  isNull := r.Rows[Node.Index].Cells[Column].IsNull;

  // Do not apply any color on a selected, highlighted cell to keep readability
  if (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then
    cl := clHighlightText
  else if vsSelected in Node.States then
    cl := clBlack
  // Numeric field
  else if r.Columns[Column].isInt or r.Columns[Column].isFloat then
    if isNull then cl := prefNullColorNumeric else cl := prefFieldColorNumeric
  // Date field
  else if r.Columns[Column].isDate then
    if isNull then cl := prefNullColorDatetime else cl := prefFieldColorDatetime
  // Text field
  else if r.Columns[Column].isText then
    if isNull then cl := prefNullColorText else cl := prefFieldColorText
  // Text field
  else if r.Columns[Column].isBinary then
    if isNull then cl := prefNullColorBinary else cl := prefFieldColorBinary
  // Enum field
  else if r.Columns[Column].isEnum then
    if isNull then cl := prefNullColorEnum else cl := prefFieldColorEnum
  // Set field
  else if r.Columns[Column].isSet then
    if isNull then cl := prefNullColorSet else cl := prefFieldColorSet
  else
    if isNull then cl := prefNullColorDefault else cl := clWindowText;
  TargetCanvas.Font.Color := cl;
end;


procedure TMainForm.DataGridAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  // Don't waist time
  if Column = -1 then Exit;
  if Node.Index >= Cardinal(Length(FDataGridResult.Rows)) then Exit;
  // Paint a red triangle at the top left corner of the cell
  if FDataGridResult.Rows[Node.Index].Cells[Column].Modified then
    PngImageListMain.Draw(TargetCanvas, CellRect.Left, CellRect.Top, 111);
end;


{**
  Header column in datagrid clicked.
  Left button: handle ORDER BY
  Right button: show column selection box
}
procedure TMainForm.DataGridHeaderClick(Sender: TVTHeader; Column:
    TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  frm: TForm;
  i, j : Integer;
  columnexists : Boolean;
  ColName: WideString;
begin
  if Button = mbLeft then begin
    ColName := Sender.Columns[Column].Text;
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
    frm.Top := Y + DataGrid.ClientOrigin.Y - Integer(DataGrid.Header.Height);
    frm.Left := X + DataGrid.ClientOrigin.X;
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
  FDataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].NewIsNull := True;
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
  Row := @FDataGridResult.Rows[Node.Index];
  // Remember new value
  Row.Cells[Column].NewText := NewText;
  Row.Cells[Column].NewIsNull := False;
  Row.Cells[Column].Modified := True;
  // Set state of row for UPDATE mode, don't touch grsInserted
  if Row.State = grsDefault then
    FDataGridResult.Rows[Node.Index].State := grsModified;
  DataGridHasChanges := True;
  ValidateControls;
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
      actEditIndexesExecute(DataGrid);
  end;
end;


{**
  DataGrid: node focus has changed
}
procedure TMainForm.DataGridChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  ValidateControls;
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
  if Allowed and (OldColumn <> NewColumn) then
    FocusGridCol(Sender, NewColumn);
end;


{**
  DataGrid: invoke update or insert routine
}
function TMainForm.DataGridPostUpdateOrInsert(Node: PVirtualNode): Boolean;
begin
  Result := True;
  if Cardinal(High(FDataGridResult.Rows)) >= Node.Index then
    case FDataGridResult.Rows[Node.Index].State of
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
  Row := @FDataGridResult.Rows[Sender.FocusedNode.Index];
  for i := 0 to Length(FDataGridResult.Columns) - 1 do begin
    if Row.Cells[i].Modified then begin
      Val := Row.Cells[i].NewText;
      if FDataGridResult.Columns[i].IsFloat then Val := FloatStr(Val);
      if not FDataGridResult.Columns[i].IsBinary then Val := esc(Val);
      if FDataGridResult.Columns[i].IsBinary then CheckHex(Copy(Val, 3), 'Invalid hexadecimal string given in field "' + FDataGridResult.Columns[i].Name + '".');
      if Val = '0x' then Val := esc('');
      if Row.Cells[i].NewIsNull then Val := 'NULL';
      sql := sql + ' ' + mask(FDataGridResult.Columns[i].Name) + '=' + Val + ', ';
    end;
  end;
  // Cut trailing comma
  sql := Copy(sql, 1, Length(sql)-2);
  sql := sql + ' WHERE ' + GetWhereClause(Row, @FDataGridResult.Columns);
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
    for i := 0 to Length(FDataGridResult.Columns) - 1 do begin
      if not Row.Cells[i].Modified then
        Continue;
      Row.Cells[i].Text := Row.Cells[i].NewText;
      Row.Cells[i].IsNull := Row.Cells[i].NewIsNull;
    end;
    GridFinalizeEditing(Sender);
    Row.Loaded := false;
    EnsureNodeLoaded(Sender, Sender.FocusedNode, GetWhereClause(Row, @FDataGridResult.Columns));
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
  FDataGridResult.Rows[c].State := grsDefault;
  for i := 0 to Length(FDataGridResult.Rows[c].Cells) - 1 do begin
    FDataGridResult.Rows[c].Cells[i].NewText := '';
    FDataGridResult.Rows[c].Cells[i].Modified := False;
  end;
  Sender.RepaintNode(Sender.FocusedNode);
  DataGridHasChanges := False;
  ValidateControls;
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
    if FDataGridResult.Columns[j].IsFloat then KeyVal := FloatStr(KeyVal);
    if not FDataGridResult.Columns[j].IsBinary then KeyVal := esc(KeyVal);
    if KeyVal = '0x' then KeyVal := esc('');
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
    FSelectedTableKeys.First;
    while not FSelectedTableKeys.Eof do begin
      if FSelectedTableKeys.FieldByName('Key_name').AsWideString = KeyName then
        Result.Add(FSelectedTableKeys.FieldByName('Column_name').AsWideString);
      FSelectedTableKeys.Next;
    end;
  end;

begin
  Result := WideStrings.TWideStringlist.Create;
  // Find best key for updates
  FSelectedTableKeys.First;
  // 1. round: find a primary key
  while not FSelectedTableKeys.Eof do begin
    if FSelectedTableKeys.FieldByName('Key_name').AsWideString = 'PRIMARY' then begin
      FindColumns(FSelectedTableKeys.FieldByName('Key_name').AsWideString);
      Exit;
    end;
    FSelectedTableKeys.Next;
  end;
  // no primary key available -> 2. round: find a unique key
  FSelectedTableKeys.First;
  while not FSelectedTableKeys.Eof do begin
    if FSelectedTableKeys.FieldByName('Non_unique').AsInteger = 0 then begin
      // We found a UNIQUE key - better than nothing. Check if one of the key
      // columns allows NULLs which makes it dangerous to use in UPDATES + DELETES.
      FindColumns(FSelectedTableKeys.FieldByName('Key_name').AsWideString);
      FSelectedTableColumns.First;
      AllowsNull := False;
      for i := 0 to Result.Count - 1 do begin
        while (not FSelectedTableColumns.Eof) and (not AllowsNull) do begin
          if FSelectedTableColumns.FieldByName('Field').AsWideString = Result[i] then
            AllowsNull := UpperCase(FSelectedTableColumns.FieldByName('Null').AsString) = 'YES';
          FSelectedTableColumns.Next;
        end;
        if AllowsNull then break;
      end;
      if AllowsNull then Result.Clear
      else break;
    end;
    FSelectedTableKeys.Next;
  end;
end;


{**
  DataGrid: compose and fire UPDATE query
}
procedure TMainForm.DataGridInsertRow;
var
  i, j: Integer;
begin
  // Scroll to the bottom to ensure we append the new row at the very last FDataGridResult chunk
  DataGrid.FocusedNode := DataGrid.GetLast;
  DataGrid.Repaint;
  // Steeling focus now to invoke posting a pending row update
  DataGrid.FocusedNode := nil;
  i := Length(FDataGridResult.Rows);
  SetLength(FDataGridResult.Rows, i+1);
  SetLength(FDataGridResult.Rows[i].Cells, Length(FDataGridResult.Columns));
  FDataGridResult.Rows[i].State := grsInserted;
  for j := 0 to Length(FDataGridResult.Rows[i].Cells) - 1 do begin
    FDataGridResult.Rows[i].Cells[j].Text := '';
  end;
  DataGrid.FocusedNode := DataGrid.AddChild(nil);
  DataGrid.ClearSelection;
  DataGrid.Selected[DataGrid.FocusedNode] := True;
  DataGridHasChanges := True;
  ValidateControls;
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
  Row := @FDataGridResult.Rows[Node.Index];
  Cols := '';
  Vals := '';
  for i := 0 to Length(FDataGridResult.Columns) - 1 do begin
    FSelectedTableColumns.RecNo := i;
    if Row.Cells[i].Modified then begin
      Cols := Cols + mask(FDataGridResult.Columns[i].Name) + ', ';
      Val := Row.Cells[i].NewText;
      if FDataGridResult.Columns[i].IsFloat then Val := FloatStr(Val);
      if not FDataGridResult.Columns[i].IsBinary then Val := esc(Val);
      if FDataGridResult.Columns[i].IsBinary then CheckHex(Copy(Val, 3), 'Invalid hexadecimal string given in field "' + FDataGridResult.Columns[i].Name + '".');
      if Val = '0x' then Val := esc('');
      if Row.Cells[i].NewIsNull then Val := 'NULL';
      Vals := Vals + Val + ', ';
    end;
  end;
  if Length(Cols) = 0 then begin
    // No field was manually modified, cancel the INSERT in that case
    Sender.BeginUpdate;
    Sender.DeleteNode(Node);
    SetLength(FDataGridResult.Rows, Length(FDataGridResult.Rows) - 1);
    Sender.EndUpdate;
    DataGridHasChanges := False;
    ValidateControls;
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
    EnsureNodeLoaded(Sender, Node, GetWhereClause(Row, @FDataGridResult.Columns));
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
  sql := 'DELETE FROM '+mask(SelectedTable)+' WHERE';
  while Assigned(Node) do begin
    EnsureChunkLoaded(Sender, Node);
    sql := sql + ' (' +
      GetWhereClause(@FDataGridResult.Rows[Node.Index], @FDataGridResult.Columns) +
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
        for j := Nodes[i].Index to High(FDataGridResult.Rows)-1 do begin
          // Move upper rows by one so the selected row gets overwritten
          FDataGridResult.Rows[j] := FDataGridResult.Rows[j+1];
        end;
      end;
      SetLength(FDataGridResult.Rows, Length(FDataGridResult.Rows) - Selected);
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
  case FDataGridResult.Rows[DataGrid.FocusedNode.Index].State of
    grsModified: GridFinalizeEditing(DataGrid);
    grsInserted: begin
      i := Length(FDataGridResult.Rows);
      DataGrid.DeleteNode(DataGrid.FocusedNode, False);
      SetLength(FDataGridResult.Rows, i-1);
      // Focus+select last node if possible
      actDataLastExecute(Sender);
    end;
  end;
  DataGridHasChanges := False;
  ValidateControls;
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
  Data: PGridResult;
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
  Data := @FDataGridResult;

  // Load entire data for field.
  Col := @Data.Columns[Column];
  Row := @Data.Rows[Node.Index];
  Cell := @Data.Rows[Node.Index].Cells[Column];
  len := Length(Cell.Text);
  // Recalculate due to textual formatting of raw binary data.
  if (Col.IsBinary) and (len > 2) then len := (len - 2) div 2;
  // Assume width limit in effect if data exactly at limit threshold.
  if len = GridMaxData then begin
    if CheckUniqueKeyClause then begin
      sql :=
        'SELECT ' + mask(Col.Name) +
        ' FROM ' + mask(SelectedTable) +
        ' WHERE ' + GetWhereClause(Row, @Data.Columns)
      ;
      ds := GetResults(sql);
      if Col.IsBinary then Cell.Text := '0x' + BinToWideHex(ds.Fields[0].AsString)
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
  if FDataGridResult.Rows[Node.Index].State = grsDefault then
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
  MemoEditor: TMemoEditorLink;
  DateTimeEditor: TDateTimeEditorLink;
  EnumEditor: TEnumEditorLink;
  SetEditor: TSetEditorLink;
  InplaceEditor: TInplaceEditorLink;
begin
  if FDataGridResult.Columns[Column].IsText then begin
    InplaceEditor := TInplaceEditorLink.Create(Sender as TVirtualStringTree);
    InplaceEditor.MaxLength := FDataGridResult.Columns[Column].MaxLength;
    InplaceEditor.ButtonVisible := true;
    EditLink := InplaceEditor;
  end else if FDataGridResult.Columns[Column].IsBinary and prefEnableBinaryEditor then begin
    MemoEditor := TMemoEditorLink.Create;
    MemoEditor.MaxLength := FDataGridResult.Columns[Column].MaxLength;
    EditLink := MemoEditor;
  end else if FDataGridResult.Columns[Column].IsDate and prefEnableDatetimeEditor then begin
    DateTimeEditor := TDateTimeEditorLink.Create;
    DateTimeEditor.DataType := FDataGridResult.Columns[Column].DataType;
    EditLink := DateTimeEditor;
  end else if FDataGridResult.Columns[Column].IsEnum and prefEnableEnumEditor then begin
    EnumEditor := TEnumEditorLink.Create;
    EnumEditor.ValueList := FDataGridResult.Columns[Column].ValueList;
    EditLink := EnumEditor;
  end else if FDataGridResult.Columns[Column].IsSet and prefEnableSetEditor then begin
    SetEditor := TSetEditorLink.Create;
    SetEditor.ValueList := FDataGridResult.Columns[Column].ValueList;
    EditLink := SetEditor;
  end else begin
    InplaceEditor := TInplaceEditorLink.Create(Sender as TVirtualStringTree);
    InplaceEditor.ButtonVisible := False;
    EditLink := InplaceEditor;
  end;
end;


function TMainForm.GetSelTableColumns: TDataset;
begin
  if FLastSelectedTableColumns = nil then
    FLastSelectedTableColumns := GetResults( 'SHOW /*!32332 FULL */ COLUMNS FROM ' + mask(SelectedTable), false );
  Result := FLastSelectedTableColumns;
end;

function TMainForm.GetSelTableKeys: TDataset;
begin
  if FLastSelectedTableKeys = nil then
    FLastSelectedTableKeys := GetResults( 'SHOW KEYS FROM ' + mask(SelectedTable) );
  Result := FLastSelectedTableKeys;
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
      Rect := Tree.GetDisplayRect(Node, i, True, True);
      ColTextWidth := Max(ColTextWidth, Rect.Right - Rect.Left);
      inc(j);
      if j > 100 then
        break;
      Node := Tree.GetNextVisible(Node);
    end;
    // text margins and minimal extra space
    ColTextWidth := ColTextWidth + Tree.TextMargin*2 + 5;
    ColTextWidth := Min(ColTextWidth, prefMaxColWidth);
    Col.Width := ColTextWidth;
  end;
  Tree.EndUpdate;
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
  gr: PGridResult;
begin
  if Column = -1 then
    Exit;
  if Sender = DataGrid then gr := @FDataGridResult
  else gr := @FQueryGridResult;
  if (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then begin
    TargetCanvas.Brush.Color := clHighlight;
    TargetCanvas.FillRect(CellRect);
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
    FSelectedTableColumns.First;
    FDataGridSelect.Clear;
    for i := 0 to FSelectedTableColumns.RecordCount - 1 do begin
      Col := FSelectedTableColumns.Fields[0].AsWideString;
      if HiddenCols.IndexOf(Col) = -1 then
        FDataGridSelect.Add(Col);
      FSelectedTableColumns.Next;
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
    Utf8Encode(ActiveDatabase) + REGDELIM + Utf8Encode(SelectedTable);
end;


procedure TMainForm.QueryGridFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  if OldColumn <> NewColumn then
    FocusGridCol(Sender, NewColumn);
  ValidateControls;
end;


procedure TMainForm.FocusGridCol(Grid: TBaseVirtualTree; Column: TColumnIndex);
var
  g: TVirtualStringTree;
  MinX, MaxX, i: Integer;
begin
  g := Grid as TVirtualStringTree;
  MinX := 0;
  for i:=0 to Column do
    MinX := MinX + g.Header.Columns[i].Width;
  MaxX := -(MinX - g.Header.Columns[Column].Width);
  MinX := -(MinX - g.Width + 20); // Assume 20px for vertical scrollbar.
  if g.OffsetX > MinX then
    g.OffsetX := MinX
  else if g.OffsetX < MaxX then
    g.OffsetX := MaxX;
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
  vt.Tag := VTREE_LOADED;
  SetVTSelection(vt, Sel);
  // Apply or reset filter
  editFilterVTChange(Sender);
  // Display number of listed values on tab
  tabVariables.Caption := 'Variables (' + IntToStr(vt.RootNodeCount) + ')';
  Screen.Cursor := crDefault;
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
  vt.Tag := VTREE_LOADED;
  SetVTSelection(vt, Sel);
  // Apply or reset filter
  editFilterVTChange(Sender);
  // Display number of listed values on tab
  tabStatus.Caption := 'Status (' + IntToStr(vt.RootNodeCount) + ')';
  Screen.Cursor := crDefault;
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
    vt.Tag := VTREE_LOADED;
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
  vt.Tag := VTREE_LOADED;
  SetVTSelection(vt, Sel);
  // Apply or reset filter
  editFilterVTChange(Sender);
  // Display number of listed values on tab
  tabCommandStats.Caption := 'Command-Statistics (' + IntToStr(vt.RootNodeCount) + ')';
  Screen.Cursor := crDefault;
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
  Control := ActiveControl;
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
  Control := ActiveControl;
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
  Control := ActiveControl;
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


end.

