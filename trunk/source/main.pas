unit Main;


// -------------------------------------
// Main-window
// -------------------------------------

{$I compilers.inc}

interface

uses
  Windows, SysUtils, Classes, GraphicEx, Graphics, GraphUtil, Forms, Controls, Menus, StdCtrls, Dialogs, Buttons,
  Messages, ExtCtrls, ComCtrls, StdActns, ActnList, ImgList, ToolWin, Clipbrd, SynMemo,
  SynEdit, SynEditTypes, SynEditKeyCmds, VirtualTrees, DateUtils,
  ShlObj, SynEditMiscClasses, SynEditSearch, SynEditRegexSearch, SynCompletionProposal, SynEditHighlighter,
  SynHighlighterSQL, Tabs, SynUnicode, SynRegExpr, WideStrUtils, ExtActns,
  CommCtrl, Contnrs, Generics.Collections, SynEditExport, SynExportHTML, Math, ExtDlgs, Registry, AppEvnts,
  routine_editor, trigger_editor, event_editor, options, EditVar, helpers, createdatabase, table_editor,
  TableTools, View, Usermanager, SelectDBObject, connections, sqlhelp, mysql_connection,
  mysql_api, insertfiles, searchreplace, loaddata, copytable, VTHeaderPopup, Cromis.DirectoryWatch;


type
  TResultTab = class(TObject)
    Results: TMySQLQuery;
    Grid: TVirtualStringTree;
    FilterText: String;
    public
      constructor Create;
      destructor Destroy; override;
  end;
  TResultTabs = TObjectList<TResultTab>;
  TQueryTab = class(TObject)
    Number: Integer;
    CloseButton: TSpeedButton;
    pnlMemo: TPanel;
    pnlHelpers: TPanel;
    treeHelpers: TVirtualStringTree;
    Memo: TSynMemo;
    MemoFileRenamed: Boolean;
    MemoLineBreaks: TLineBreaks;
    DirectoryWatch: TDirectoryWatch;
    MemofileModifiedTimer: TTimer;
    LastSaveTime: Cardinal;
    spltHelpers: TSplitter;
    spltQuery: TSplitter;
    tabsetQuery: TTabSet;
    TabSheet: TTabSheet;
    ResultTabs: TResultTabs;
    QueryProfile: TMySQLQuery;
    ProfileTime, MaxProfileTime: Extended;
    function GetActiveResultTab: TResultTab;
    procedure DirectoryWatchNotify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
    procedure MemofileModifiedTimerNotify(Sender: TObject);
    procedure SaveQueryMemo(Filename: String; OnlySelection: Boolean);
    private
      FMemoFilename: String;
      procedure SetMemoFilename(Value: String);
    public
      property ActiveResultTab: TResultTab read GetActiveResultTab;
      property MemoFilename: String read FMemoFilename write SetMemoFilename;
      constructor Create;
      destructor Destroy; override;
  end;

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
    actExecuteCurrentQuery: TAction;
    actDataPreview: TAction;
    actInsertFiles: TAction;
    InsertfilesintoBLOBfields1: TMenuItem;
    actExportTables: TAction;
    actDropObjects: TAction;
    actLoadSQL: TAction;
    ImportSQL1: TMenuItem;
    menuConnections: TPopupMenu;
    menuFeaturetracker: TMenuItem;
    menuDownload: TMenuItem;
    btnSQLHelp: TToolButton;
    menuSQLHelp1: TMenuItem;
    N8a: TMenuItem;
    Import1: TMenuItem;
    tlbSep6: TToolButton;
    menuUpdateCheck: TMenuItem;
    ImageListMain: TImageList;
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
    actWebDownloadpage: TAction;
    actWebForum: TAction;
    actWebChangelog: TAction;
    actReadme: TAction;
    actSaveSQL: TAction;
    actSaveSQLAs: TAction;
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
    ToolBarQuery: TToolBar;
    btnExecuteQuery: TToolButton;
    btnLoadSQL: TToolButton;
    btnSaveSQL: TToolButton;
    btnSaveSQLSnippet: TToolButton;
    btnQueryFind: TToolButton;
    btnQueryReplace: TToolButton;
    btnStopOnErrors: TToolButton;
    btnQueryWordwrap: TToolButton;
    PopupQueryLoad: TPopupMenu;
    actSetDelimiter: TAction;
    btnSetDelimiter: TToolButton;
    actDataCancelChanges: TAction;
    ToolButton1: TToolButton;
    actRemoveFilter: TAction;
    actCopyAsSQL: TAction;
    CopyAsSQLdata: TMenuItem;
    panelTop: TPanel;
    pnlLeft: TPanel;
    DBtree: TVirtualStringTree;
    comboDBFilter: TComboBox;
    spltDBtree: TSplitter;
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
    CopyasCSVData1: TMenuItem;
    N9a: TMenuItem;
    TimerConnected: TTimer;
    N12: TMenuItem;
    popupSqlLog: TPopupMenu;
    Clear2: TMenuItem;
    Copy1: TMenuItem;
    N15: TMenuItem;
    N17: TMenuItem;
    CopycontentsasHTML1: TMenuItem;
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
    menuQuickFilter: TMenuItem;
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
    Exportdata2: TMenuItem;
    SaveDialogExportData: TSaveDialog;
    N11a: TMenuItem;
    DataInsertValue: TMenuItem;
    DataDateTime: TMenuItem;
    DataTime: TMenuItem;
    DataDate: TMenuItem;
    DataYear: TMenuItem;
    N2: TMenuItem;
    DataGUID: TMenuItem;
    ViewasHTML1: TMenuItem;
    InsertfilesintoBLOBfields3: TMenuItem;
    N19: TMenuItem;
    setNULL1: TMenuItem;
    menuExporttables: TMenuItem;
    popupListHeader: TVTHeaderPopupMenu;
    SynCompletionProposal: TSynCompletionProposal;
    ParameterCompletionProposal: TSynCompletionProposal;
    SaveDialogSQLFile: TSaveDialog;
    SynEditSearch1: TSynEditSearch;
    SynEditRegexSearch1: TSynEditRegexSearch;
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
    popupQuery: TPopupMenu;
    MenuRun: TMenuItem;
    MenuRunSelection: TMenuItem;
    MenuRunLine: TMenuItem;
    MenuItem1: TMenuItem;
    menucopy: TMenuItem;
    menupaste: TMenuItem;
    menuload: TMenuItem;
    menusave: TMenuItem;
    menuSaveSQL: TMenuItem;
    menuclear: TMenuItem;
    MenuFind: TMenuItem;
    MenuReplace: TMenuItem;
    MenuItem2: TMenuItem;
    lblDataTop: TLabel;
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
    editFilterVT: TButtonedEdit;
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
    SynMemoSQLLog: TSynMemo;
    Insert1: TMenuItem;
    Cancelediting1: TMenuItem;
    DataPost1: TMenuItem;
    menuShowSizeColumn: TMenuItem;
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
    comboRecentFilters: TComboBox;
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
    pnlRight: TPanel;
    btnCloseFilterPanel: TSpeedButton;
    actFilterPanel: TAction;
    actFindInVT1: TMenuItem;
    TimerFilterVT: TTimer;
    actFindTextOnServer: TAction;
    actFindTextOnServer1: TMenuItem;
    Findtextonserver1: TMenuItem;
    actBulkTableEdit: TAction;
    menuBulkTableEdit: TMenuItem;
    menuQueryHelpersGenerateInsert: TMenuItem;
    menuQueryHelpersGenerateUpdate: TMenuItem;
    menuQueryHelpersGenerateDelete: TMenuItem;
    actCreateTrigger: TAction;
    menuCreateTrigger: TMenuItem;
    menuQueryCut: TMenuItem;
    menuQuerySelectall: TMenuItem;
    actDataDuplicateRow: TAction;
    Duplicaterow1: TMenuItem;
    Bulktableeditor1: TMenuItem;
    actSelectInverse: TAction;
    Inverseselection1: TMenuItem;
    actDataResetSorting: TAction;
    Resetsorting1: TMenuItem;
    actReformatSQL: TAction;
    ReformatSQL1: TMenuItem;
    btnReformatSQL: TToolButton;
    ReformatSQL2: TMenuItem;
    menuQueryInsertFunction: TMenuItem;
    menuFilterInsertFunction: TMenuItem;
    actBlobAsText: TAction;
    btnBlobAsText: TToolButton;
    actQueryFindAgain: TAction;
    Search1: TMenuItem;
    Findtext1: TMenuItem;
    actQueryFindAgain1: TMenuItem;
    Replacetext1: TMenuItem;
    lblExplainProcess: TLabel;
    menuExplainProcess: TMenuItem;
    ToolButton2: TToolButton;
    tbtnDataShowAll: TToolButton;
    tbtnDataNext: TToolButton;
    actDataShowNext: TAction;
    actDataShowAll: TAction;
    SynExporterHTML1: TSynExporterHTML;
    QFvalues: TMenuItem;
    tabDatabases: TTabSheet;
    ListDatabases: TVirtualStringTree;
    menuFetchDBitems: TMenuItem;
    actRunRoutines: TAction;
    Runroutines1: TMenuItem;
    actCreateEvent: TAction;
    Event1: TMenuItem;
    tabsetQuery: TTabSet;
    BalloonHint1: TBalloonHint;
    actDataSetNull: TAction;
    pnlPreview: TPanel;
    spltPreview: TSplitter;
    imgPreview: TImage;
    lblPreviewTitle: TLabel;
    ToolBarPreview: TToolBar;
    btnPreviewCopy: TToolButton;
    btnPreviewSaveToFile: TToolButton;
    btnPreviewClose: TToolButton;
    actDataSaveBlobToFile: TAction;
    SaveBLOBtofile1: TMenuItem;
    actCopyAsLaTeX: TAction;
    CopyselectedrowsasLaTeXtable1: TMenuItem;
    CopyselectedrowsasLaTeXtable2: TMenuItem;
    actCopyAsWiki: TAction;
    CopyselectedrowsasWikitable1: TMenuItem;
    CopyselectedrowsasWikitable2: TMenuItem;
    DataUNIXtimestamp: TMenuItem;
    btnClearFilters: TButton;
    popupClearFilters: TPopupMenu;
    menuClearFiltersTable: TMenuItem;
    menuClearFiltersSession: TMenuItem;
    menuClearFiltersAll: TMenuItem;
    treeQueryHelpers: TVirtualStringTree;
    popupExecuteQuery: TPopupMenu;
    Run1: TMenuItem;
    RunSelection1: TMenuItem;
    Runcurrentquery1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    actDisconnect: TAction;
    Copylinetonewquerytab1: TMenuItem;
    menuLogHorizontalScrollbar: TMenuItem;
    procedure actCreateDBObjectExecute(Sender: TObject);
    procedure menuConnectionsPopup(Sender: TObject);
    procedure actExitApplicationExecute(Sender: TObject);
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Startup;
    procedure FormResize(Sender: TObject);
    procedure actUserManagerExecute(Sender: TObject);
    procedure actAboutBoxExecute(Sender: TObject);
    procedure actApplyFilterExecute(Sender: TObject);
    procedure actClearEditorExecute(Sender: TObject);
    procedure actTableToolsExecute(Sender: TObject);
    procedure actCopyDataExecute(Sender: TObject);
    procedure actPrintListExecute(Sender: TObject);
    procedure actCopyTableExecute(Sender: TObject);
    procedure ShowStatusMsg(Msg: String=''; PanelNr: Integer=6);
    procedure actExecuteQueryExecute(Sender: TObject);
    procedure actCreateDatabaseExecute(Sender: TObject);
    procedure actDataCancelChangesExecute(Sender: TObject);
    procedure actExportDataExecute(Sender: TObject);
    procedure actDataPreviewExecute(Sender: TObject);
    procedure UpdatePreviewPanel;
    procedure actInsertFilesExecute(Sender: TObject);
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
    procedure actQueryFindReplaceExecute(Sender: TObject);
    procedure actQueryStopOnErrorsExecute(Sender: TObject);
    procedure actQueryWordWrapExecute(Sender: TObject);
    procedure actReadmeExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actRemoveFilterExecute(Sender: TObject);
    procedure actSaveSQLExecute(Sender: TObject);
    procedure actSaveSQLAsExecute(Sender: TObject);
    procedure actSaveSQLSnippetExecute(Sender: TObject);
    procedure actSetDelimiterExecute(Sender: TObject);
    procedure actSQLhelpExecute(Sender: TObject);
    procedure actUpdateCheckExecute(Sender: TObject);
    procedure actWebbrowse(Sender: TObject);
    procedure actSelectTreeBackgroundExecute(Sender: TObject);
    procedure popupQueryPopup(Sender: TObject);
    procedure btnDataClick(Sender: TObject);
    procedure ListTablesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SynCompletionProposalAfterCodeCompletion(Sender: TObject;
      const Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure SynCompletionProposalCodeCompletion(Sender: TObject;
      var Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure SynCompletionProposalExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);
    procedure ParameterCompletionProposalExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure PageControlMainChange(Sender: TObject);
    procedure PageControlMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PageControlHostChange(Sender: TObject);
    procedure ValidateControls(Sender: TObject);
    procedure ValidateQueryControls(Sender: TObject);
    procedure DataGridBeforePaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure LogSQL(Msg: String; Category: TMySQLLogCategory=lcInfo; Connection: TMySQLConnection=nil);
    procedure KillProcess(Sender: TObject);
    procedure SynMemoQueryStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure TimerHostUptimeTimer(Sender: TObject);
    procedure ListTablesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: String);
    procedure TimerConnectedTimer(Sender: TObject);
    procedure Clear2Click(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    procedure AutoRefreshSetInterval(Sender: TObject);
    procedure AutoRefreshToggle(Sender: TObject);
    procedure SynMemoQueryDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoQueryDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer; AFiles: TStrings);
    procedure popupHostPopup(Sender: TObject);
    procedure Saveastextfile1Click(Sender: TObject);
    procedure popupDBPopup(Sender: TObject);
    procedure SaveDialogExportDataTypeChange(Sender: TObject);
    procedure popupDataGridPopup(Sender: TObject);
    procedure QFvaluesClick(Sender: TObject);
    procedure DataInsertValueClick(Sender: TObject);
    procedure InsertValue(Sender: TObject);
    procedure actDataSetNullExecute(Sender: TObject);
    function QueryLoad(Filename: String; ReplaceContent: Boolean; Encoding: TEncoding): Boolean;
    procedure AnyGridCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure AnyGridEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure AnyGridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex);
    procedure AnyGridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; var Allowed: Boolean);
    procedure AnyGridFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode:
        PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure AnyGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure DataGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure AnyGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AnyGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);
    procedure AnyGridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; NewText: String);
    procedure AnyGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure menuDeleteSnippetClick(Sender: TObject);
    procedure menuExploreClick(Sender: TObject);
    procedure menuInsertSnippetAtCursorClick(Sender: TObject);
    procedure menuLoadSnippetClick(Sender: TObject);
    procedure vstGetNodeDataSize(Sender: TBaseVirtualTree; var
        NodeDataSize: Integer);
    procedure vstInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
        PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree; Node:
        PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
        Boolean; var ImageIndex: Integer);
    procedure vstHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstHeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex;
        DropPosition: TPoint);
    procedure vstIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: String;
      var Result: Integer);
    procedure SetMainTab(Page: TTabSheet);
    procedure DBtreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DBtreeDblClick(Sender: TObject);
    procedure DBtreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var
        ImageIndex: Integer);
    procedure DBtreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize:
        Integer);
    procedure DBtreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
        HintText: String);
    procedure ListCommandStatsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure ListTablesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure ListProcessesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure ListProcessesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure editFilterVTChange(Sender: TObject);
    procedure ListVariablesDblClick(Sender: TObject);
    procedure menuEditVariableClick(Sender: TObject);
    procedure menuTreeCollapseAllClick(Sender: TObject);
    procedure menuTreeExpandAllClick(Sender: TObject);
    procedure SynMemoFilterStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure AnyGridAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure menuShowSizeColumnClick(Sender: TObject);
    procedure AnyGridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure pnlQueryMemoCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure AnyGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure File1Click(Sender: TObject);
    procedure ListVariablesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListStatusBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListProcessesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListCommandStatsBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListTablesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListTablesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ListTablesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ListTablesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ListTablesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure actCopyOrCutExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure EnumerateRecentFilters;
    procedure LoadRecentFilter(Sender: TObject);
    procedure ListTablesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DBtreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ListTablesDblClick(Sender: TObject);
    procedure panelTopDblClick(Sender: TObject);
    procedure PageControlMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actNewQueryTabExecute(Sender: TObject);
    procedure actCloseQueryTabExecute(Sender: TObject);
    procedure menuCloseQueryTab(Sender: TObject);
    procedure CloseQueryTab(PageIndex: Integer);
    procedure CloseButtonOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CloseButtonOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetMainTabAt(X, Y: Integer): Integer;
    procedure FixQueryTabCloseButtons;
    function ActiveQueryTab: TQueryTab;
    function ActiveQueryMemo: TSynMemo;
    function ActiveQueryHelpers: TVirtualStringTree;
    function ActiveSynMemo: TSynMemo;
    function QueryTabActive: Boolean;
    function IsQueryTab(PageIndex: Integer; IncludeFixed: Boolean): Boolean;
    procedure popupMainTabsPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actFilterPanelExecute(Sender: TObject);
    procedure TimerFilterVTTimer(Sender: TObject);
    procedure PageControlMainContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure menuQueryHelpersGenerateStatementClick(Sender: TObject);
    procedure actSelectInverseExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure actDataResetSortingExecute(Sender: TObject);
    procedure actReformatSQLExecute(Sender: TObject);
    procedure DBtreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure actBlobAsTextExecute(Sender: TObject);
    procedure SynMemoQueryReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
    procedure SynMemoQueryPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure actQueryFindAgainExecute(Sender: TObject);
    procedure vstScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure lblExplainProcessClick(Sender: TObject);
    procedure actDataShowNextExecute(Sender: TObject);
    procedure actDataShowAllExecute(Sender: TObject);
    procedure AnyGridInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure editFilterVTRightButtonClick(Sender: TObject);
    procedure AnyGridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure ListTablesKeyPress(Sender: TObject; var Key: Char);
    procedure DBtreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ListDatabasesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListDatabasesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ListDatabasesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure ListDatabasesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ListDatabasesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure menuFetchDBitemsClick(Sender: TObject);
    procedure ListDatabasesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ListDatabasesDblClick(Sender: TObject);
    procedure actRunRoutinesExecute(Sender: TObject);
    procedure AnyGridGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure tabsetQueryClick(Sender: TObject);
    procedure tabsetQueryGetImageIndex(Sender: TObject; TabIndex: Integer; var ImageIndex: Integer);
    procedure tabsetQueryMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure tabsetQueryMouseLeave(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure StatusBarMouseLeave(Sender: TObject);
    procedure AnyGridStartOperation(Sender: TBaseVirtualTree; OperationKind: TVTOperationKind);
    procedure AnyGridEndOperation(Sender: TBaseVirtualTree; OperationKind: TVTOperationKind);
    procedure actDataPreviewUpdate(Sender: TObject);
    procedure spltPreviewMoved(Sender: TObject);
    procedure actDataSaveBlobToFileExecute(Sender: TObject);
    procedure DataGridColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure comboDBFilterChange(Sender: TObject);
    procedure comboDBFilterExit(Sender: TObject);
    procedure comboDBFilterDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure comboDBFilterDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure comboDBFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DBtreeAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ClearFiltersClick(Sender: TObject);
    procedure treeQueryHelpersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure treeQueryHelpersInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure treeQueryHelpersInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure treeQueryHelpersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure treeQueryHelpersBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure treeQueryHelpersDblClick(Sender: TObject);
    procedure treeQueryHelpersContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure treeQueryHelpersPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure treeQueryHelpersFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure treeQueryHelpersResize(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure menuEditObjectClick(Sender: TObject);
    procedure Copylinetonewquerytab1Click(Sender: TObject);
    procedure menuLogHorizontalScrollbarClick(Sender: TObject);
  private
    LastHintMousepos: TPoint;
    LastHintControlIndex: Integer;
    FDelimiter: String;
    FileNameSessionLog: String;
    FileHandleSessionLog: Textfile;
    FLastMouseUpOnPageControl: Cardinal;
    FLastTabNumberOnMouseUp: Integer;
    FLastMouseDownCloseButton: TObject;
    // Filter text per tab for filter panel
    FilterTextDatabases,
    FilterTextEditor,
    FilterTextVariables,
    FilterTextStatus,
    FilterTextProcessList,
    FilterTextCommandStats,
    FilterTextDatabase,
    FilterTextData: String;
    FTreeRefreshInProgress: Boolean;
    FCmdlineFilenames: TStringlist;
    FCmdlineConnectionParams: TConnectionParameters;
    FCmdlineSessionName: String;
    FSearchReplaceExecuted: Boolean;
    FDataGridColumnWidthsCustomized: Boolean;
    FSnippetFilenames: TStringList;
    FConnections: TMySQLConnectionList;
    FTreeClickHistory: TNodeArray;
    procedure ParseCommandLineParameters(Parameters: TStringlist);
    procedure SetDelimiter(Value: String);
    procedure DisplayRowCountStats(Sender: TBaseVirtualTree);
    procedure insertFunction(Sender: TObject);
    function GetRootNode(Tree: TBaseVirtualTree; Connection: TMySQLConnection): PVirtualNode;
    function GetActiveConnection: TMySQLConnection;
    function GetActiveDatabase: String;
    procedure SetActiveDatabase(db: String; Connection: TMySQLConnection);
    function GetActiveDBObj: TDBObject;
    procedure SetActiveDBObj(Obj: TDBObject);
    procedure ToggleFilterPanel(ForceVisible: Boolean = False);
    procedure AutoCalcColWidth(Tree: TVirtualStringTree; Column: TColumnIndex);
    procedure PlaceObjectEditor(Obj: TDBObject);
    procedure SetTabCaption(PageIndex: Integer; Text: String);
    function ConfirmTabClose(PageIndex: Integer): Boolean;
    procedure UpdateFilterPanel(Sender: TObject);
    procedure DBObjectsCleared(Connection: TMySQLConnection; Database: String);
    procedure DatabaseChanged(Connection: TMySQLConnection; Database: String);
    procedure DoSearchReplace;
    procedure UpdateLineCharPanel;
    procedure PaintColorBar(Value, Max: Extended; TargetCanvas: TCanvas; CellRect: TRect);
    procedure SetSnippetFilenames(Value: TStringList);
    function TreeClickHistoryPrevious(MayBeNil: Boolean=False): PVirtualNode;
  public
    AllDatabasesDetails: TMySQLQuery;
    btnAddTab: TSpeedButton;
    QueryTabs: TObjectList<TQueryTab>;
    DBObjectsMaxSize: Int64;
    DBObjectsMaxRows: Int64;
    ProcessListMaxTime: Int64;
    ActiveObjectEditor: TDBObjectEditor;
    FileEncodings: TStringList;

    // Cached forms
    TableToolsDialog: TfrmTableTools;
    UserManagerForm: TUserManagerForm;
    SelectDBObjectForm: TfrmSelectDBObject;
    SQLHelpForm: TfrmSQLhelp;
    OptionsForm: Toptionsform;
    SessionManager: TConnForm;
    CreateDatabaseForm: TCreateDatabaseForm;
    InsertFiles: TfrmInsertFiles;
    EditVariableForm: TfrmEditVariable;
    SearchReplaceDialog: TfrmSearchReplace;
    ImportTextfileDialog: Tloaddataform;
    CopyTableDialog: TCopyTableForm;

    // Virtual Tree data arrays
    VTRowDataListVariables,
    VTRowDataListStatus,
    VTRowDataListProcesses,
    VTRowDataListCommandStats: TVTreeDataArray;

    // Variables set by preferences dialog
    prefRememberFilters: Boolean;
    prefLogsqlnum: Integer;
    prefLogSqlWidth: Integer;
    prefDirnameSessionLogs: String;
    prefMaxColWidth: Integer;
    prefGridRowcountStep: Integer;
    prefGridRowcountMax: Integer;
    prefGridRowsLineCount: Word;
    prefCSVSeparator: String;
    prefCSVEncloser: String;
    prefCSVTerminator: String;
    prefLogToFile: Boolean;
    prefLogErrors: Boolean;
    prefLogUserSQL: Boolean;
    prefLogSQL: Boolean;
    prefLogInfos: Boolean;
    prefLogDebug: Boolean;
    prefEnableBinaryEditor: Boolean;
    prefEnableDatetimeEditor: Boolean;
    prefEnableEnumEditor: Boolean;
    prefEnableSetEditor: Boolean;
    prefEnableNullBG: Boolean;
    prefExportLocaleNumbers: Boolean;
    prefNullColorDefault: TColor;
    prefNullBG: TColor;
    prefDisplayBars: Boolean;
    prefBarColor: TColor;
    prefCompletionProposal: Boolean;
    prefMaxQueryResults: Integer;

    // Data grid related stuff
    DataGridHiddenColumns: TStringList;
    DataGridSortColumns: TOrderColArray;
    DataGridWantedRowCount: Int64;
    DataGridDB: String;
    DataGridTable: String;
    DataGridFocusedCell: TStringList;
    DataGridFocusedNodeIndex: Int64;
    DataGridFocusedColumnName: String;
    DataGridResult: TMySQLQuery;
    DataGridFullRowMode: Boolean;
    SelectedTableColumns: TTableColumnList;
    SelectedTableKeys: TTableKeyList;
    SelectedTableForeignKeys: TForeignKeyList;
    FilterPanelManuallyOpened: Boolean;

    // Executable file details
    AppVerMajor: Integer;
    AppVerMinor: Integer;
    AppVerRelease: Integer;
    AppVerRevision: Integer;
    AppVersion: String;
    AppDescription: String;

    // Common directories
    DirnameCommonAppData: String;
    DirnameUserAppData: String;
    DirnameSnippets: String;

    property Connections: TMySQLConnectionList read FConnections;
    property Delimiter: String read FDelimiter write SetDelimiter;
    property SnippetFilenames: TStringList read FSnippetFilenames write SetSnippetFilenames;
    procedure CallSQLHelpWithKeyword( keyword: String );
    procedure AddOrRemoveFromQueryLoadHistory(Filename: String; AddIt: Boolean; CheckIfFileExists: Boolean);
    procedure popupQueryLoadClick( sender: TObject );
    procedure FillPopupQueryLoad;
    procedure PopupQueryLoadRemoveAbsentFiles(Sender: TObject);
    procedure PopupQueryLoadRemoveAllFiles(Sender: TObject);
    procedure SessionConnect(Sender: TObject);
    function InitConnection(Params: TConnectionParameters; Session: String;
      ActivateMe: Boolean; var Connection: TMySQLConnection): Boolean;
    procedure ConnectionsNotify(Sender: TObject; const Item: TMySQLConnection; Action: TCollectionNotification);
    function ActiveGrid: TVirtualStringTree;
    function GridResult(Grid: TBaseVirtualTree): TMySQLQuery;
    property ActiveConnection: TMySQLConnection read GetActiveConnection;
    property ActiveDatabase: String read GetActiveDatabase;
    property ActiveDbObj: TDBObject read GetActiveDbObj write SetActiveDBObj;
    procedure TestVTreeDataArray( P: PVTreeDataArray );
    function GetVTreeDataArray( VT: TBaseVirtualTree ): PVTreeDataArray;
    procedure ActivateFileLogging;
    procedure DeactivateFileLogging;
    procedure TrimSQLLog;
    procedure RefreshTree(FocusNewObject: TDBObject=nil);
    function FindDBObjectNode(Tree: TBaseVirtualTree; Obj: TDBObject): PVirtualNode;
    function FindDBNode(Tree: TBaseVirtualTree; db: String): PVirtualNode;
    procedure CalcNullColors;
    procedure HandleDataGridAttributes(RefreshingData: Boolean);
    function GetRegKeyTable: String;
    procedure SaveListSetup( List: TVirtualStringTree );
    procedure RestoreListSetup( List: TVirtualStringTree );
    procedure UpdateEditorTab;
    procedure SetWindowCaption;
    procedure OnMessageHandler(var Msg: TMsg; var Handled: Boolean);
    procedure DefaultHandler(var Message); override;
    procedure SetupSynEditors;
    procedure ParseSelectedTableStructure;
    function AnyGridEnsureFullRow(Grid: TVirtualStringTree; Node: PVirtualNode): Boolean;
    procedure DataGridEnsureFullRows(Grid: TVirtualStringTree; SelectedOnly: Boolean);
    function GetEncodingByName(Name: String): TEncoding;
    function GetEncodingName(Encoding: TEncoding): String;
    function GetCharsetByEncoding(Encoding: TEncoding): String;
    procedure RefreshHelperNode(NodeIndex: Cardinal);
end;


var
  MainForm: TMainForm;
  SecondInstMsgId: UINT = 0;

const
  // Customized messages
  MSG_UPDATECHECK = WM_USER + 1;
  MSG_ABOUT = WM_USER + 2;
  CheckedStates = [csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed];

{$I const.inc}


implementation

uses
  About, printlist, mysql_structures, UpdateCheck, uVistaFuncs, runsqlfile,
  column_selection, data_sorting, grideditlinks, jpeg, GIFImg;



{$R *.DFM}


procedure TMainForm.ShowStatusMsg(Msg: String=''; PanelNr: Integer=6);
begin
  // Show message in some statusbar panel
  if (PanelNr = 6) and (Msg = '') then
    Msg := SIdle;
  StatusBar.Panels[PanelNr].Text := Msg;
  StatusBar.Repaint;
end;


procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
var
  TextRect: TRect;
  ImageIndex: Integer;
begin
  ImageIndex := -1;
  case Panel.Index of
    2: ImageIndex := 149;
    3: ImageIndex := 1;
    6: begin
      if Panel.Text = SIdle then
        ImageIndex := 151
      else
        ImageIndex := 150;
    end;
  end;
  StatusBar.Canvas.FillRect(Rect);
  if ImageIndex > -1 then begin
    ImageListMain.Draw(StatusBar.Canvas, Rect.Left, Rect.Top, ImageIndex, true);
    TextRect := Rect;
    OffsetRect(TextRect, ImageListMain.Width+2, 0);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, TextRect, dt_singleline or dt_vcenter);
  end;
end;

procedure TMainForm.StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  MouseP: TPoint;
  Bar: TStatusBar;
  PanelRect: TRect;
  i: Integer;
  Infos: TStringList;
begin
  // Display various server, client and connection related details in a hint
  if (LastHintMousepos.X = X) and (LastHintMousepos.Y = Y) then
    Exit;
  LastHintMousepos := Point(X, Y);
  MouseP := StatusBar.ClientOrigin;
  Inc(MouseP.X, X);
  Inc(MouseP.Y, Y);
  Bar := Sender as TStatusBar;
  for i:=0 to Bar.Panels.Count-1 do begin
    SendMessage(Bar.Handle, SB_GETRECT, i, Integer(@PanelRect));
    if PtInRect(PanelRect, LastHintMousepos) then
      break;
  end;
  if i = LastHintControlIndex then
    Exit;
  LastHintControlIndex := i;
  if LastHintControlIndex = 3 then begin
    Infos := ActiveConnection.ConnectionInfo;
    BalloonHint1.Description := '';
    for i:=0 to Infos.Count-1 do
      BalloonHint1.Description := BalloonHint1.Description + Infos.Names[i] + ': ' + Infos.ValueFromIndex[i] + CRLF;
    BalloonHint1.Description := Trim(BalloonHint1.Description);
    OffsetRect(PanelRect, Bar.ClientOrigin.X, Bar.ClientOrigin.Y);
    BalloonHint1.ShowHint(PanelRect);
  end else
    Bar.OnMouseLeave(Sender);
end;


procedure TMainForm.StatusBarMouseLeave(Sender: TObject);
begin
  BalloonHint1.HideHint;
  LastHintControlIndex := -1;
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
  flushwhat := StripHotkey(flushwhat);
  try
    ActiveConnection.Query('FLUSH ' + flushwhat);
    if Sender = actFlushTableswithreadlock then begin
      MessageDlg(
        'Tables have been flushed and read lock acquired.'#10 +
        'Perform backup or snapshot of table data files now.'#10 +
        'Press OK to unlock when done...',
        mtInformation, [mbOk], 0
      );
      ActiveConnection.Query('UNLOCK TABLES');
    end;
  except
    on E:EDatabaseError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;
begin
  // Prompt on modified changes
  CanClose := True;
  // Unsaved changes in some query tab?
  for i:=0 to QueryTabs.Count-1 do begin
    CanClose := ConfirmTabClose(i+tabQuery.PageIndex);
    if not CanClose then
      Exit;
  end;
  // Unsaved modified table, trigger, view or routine?
  if Assigned(ActiveObjectEditor) then
    CanClose := not (ActiveObjectEditor.DeInit in [mrAbort, mrCancel]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  WinState, OpenSessions: String;
  i: Integer;
  Connection: TMySQLConnection;
begin
  // Destroy editors and dialogs. Must be done before connection gets closed, as some destructors do SQL stuff.
  FreeAndNil(ActiveObjectEditor);
  FreeAndNil(TableToolsDialog);
  FreeAndNil(UserManagerForm);
  FreeAndNil(SelectDBObjectForm);
  FreeAndNil(SQLHelpForm);
  FreeAndNil(OptionsForm);
  FreeAndNil(SessionManager);
  FreeAndNil(CreateDatabaseForm);
  FreeAndNil(SearchReplaceDialog);
  FreeAndNil(CopyTableDialog);
  FreeAndNil(ImportTextfileDialog);

  // Save opened session names in root folder
  OpenRegistry;
  OpenSessions := '';
  for Connection in Connections do
    OpenSessions := OpenSessions + Connection.SessionName + DELIM;
  Delete(OpenSessions, Length(OpenSessions)-Length(DELIM)+1, Length(DELIM));
  MainReg.WriteString(REGNAME_LASTSESSIONS, OpenSessions);
  if Assigned(ActiveConnection) then
    MainReg.WriteString(REGNAME_LASTACTIVESESSION, ActiveConnection.SessionName);

  // Close database connections
  Connections.Clear;

  // Some grid editors access the registry - be sure these are gone before freeing MainReg
  QueryTabs.Clear;
  DataGrid.EndEditNode;

  // Clearing query and browse data.
  FreeAndNil(DataGridResult);

  // Save various settings
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
  MainReg.WriteInteger(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
  MainReg.WriteInteger(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
  MainReg.WriteInteger(REGNAME_TOOLBARDATATOP, ToolBarData.Top);
  MainReg.WriteInteger(REGNAME_TOOLBARQUERYLEFT, ToolBarQuery.Left);
  MainReg.WriteInteger(REGNAME_TOOLBARQUERYTOP, ToolBarQuery.Top);
  MainReg.WriteBool(REGNAME_STOPONERRORSINBATCH, actQueryStopOnErrors.Checked);
  MainReg.WriteBool(REGNAME_BLOBASTEXT, actBlobAsText.Checked);
  MainReg.WriteString( REGNAME_DELIMITER, Delimiter );
  MainReg.WriteInteger( REGNAME_QUERYMEMOHEIGHT, pnlQueryMemo.Height );
  MainReg.WriteInteger( REGNAME_QUERYHELPERSWIDTH, treeQueryHelpers.Width );
  MainReg.WriteInteger( REGNAME_DBTREEWIDTH, pnlLeft.width );
  MainReg.WriteString( REGNAME_DATABASE_FILTER, comboDBFilter.Items.Text );
  MainReg.WriteInteger(REGNAME_PREVIEW_HEIGHT, pnlPreview.Height);
  MainReg.WriteBool(REGNAME_PREVIEW_ENABLED, actDataPreview.Checked);
  MainReg.WriteInteger( REGNAME_SQLOUTHEIGHT, SynMemoSQLLog.Height );
  MainReg.WriteBool(REGNAME_FILTERACTIVE, pnlFilterVT.Tag=Integer(True));
  MainReg.WriteBool(REGNAME_WRAPLINES, actQueryWordWrap.Checked);
  MainReg.WriteBool(REGNAME_LOG_HORIZONTALSCROLLBAR, SynMemoSQLLog.ScrollBars = ssBoth);
  // Convert set to string.
  case WindowState of
    wsMinimized: WinState := 'Minimized';
    wsMaximized: WinState := 'Maximized';
    else WinState := 'Normal';
  end;
  MainReg.WriteString(REGNAME_WINDOWSTATE, WinState);
  // Window dimensions are only valid when WindowState is normal.
  if WindowState = wsNormal then begin
    MainReg.WriteInteger(REGNAME_WINDOWLEFT, Left);
    MainReg.WriteInteger(REGNAME_WINDOWTOP, Top);
    MainReg.WriteInteger(REGNAME_WINDOWWIDTH, Width);
    MainReg.WriteInteger(REGNAME_WINDOWHEIGHT, Height);
  end else begin
    // Ensure Left + Top values are at least set to the right monitor area for the next start
    i := GetRegValue(REGNAME_WINDOWLEFT, Left);
    if (i < Monitor.Left) or (i > Monitor.Left+Monitor.Width) then
      MainReg.WriteInteger(REGNAME_WINDOWLEFT, Monitor.Left);
    i := GetRegValue(REGNAME_WINDOWTOP, Top);
    if (i < Monitor.Top) or (i > Monitor.Top+Monitor.Height) then
      MainReg.WriteInteger(REGNAME_WINDOWTOP, Monitor.Top);
  end;
  SaveListSetup(ListDatabases);
  SaveListSetup(ListVariables);
  SaveListSetup(ListStatus);
  SaveListSetup(ListProcesses);
  SaveListSetup(ListCommandStats);
  SaveListSetup(ListTables);

  if prefLogToFile then
    DeactivateFileLogging;

  if MainReg <> nil then begin
    MainReg.CloseKey;
    // Export settings into textfile in portable mode.
    HandlePortableSettings(False);
    MainReg.Free;
  end;
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
  datafontname, WinState: String;
  datafontsize : Integer;
  DisableProcessWindowsGhostingProc: procedure;
  QueryTab: TQueryTab;
  Action: TAction;
  dwInfoSize,           // Size of VERSIONINFO structure
  dwVerSize,            // Size of Version Info Data
  dwWnd: DWORD;         // Handle for the size call.
  FI: PVSFixedFileInfo; // Delphi structure; see WINDOWS.PAS
  ptrVerBuf, Translation, Info: Pointer;
  DpiScaleFactor: Double;
  FunctionCategories: TStringList;
  miGroup, miFilterGroup, miFunction, miFilterFunction: TMenuItem;
begin
  caption := APPNAME;
  setLocales;

  // Detect version
  dwInfoSize := GetFileVersionInfoSize(PChar(Application.ExeName), dwWnd);
  GetMem(ptrVerBuf, dwInfoSize);
  GetFileVersionInfo(PChar(Application.ExeName), dwWnd, dwInfoSize, ptrVerBuf);
  VerQueryValue(ptrVerBuf, '\', Pointer(FI), dwVerSize );
  AppVerMajor := HiWord(FI.dwFileVersionMS);
  AppVerMinor := LoWord(FI.dwFileVersionMS);
  AppVerRelease := HiWord(FI.dwFileVersionLS);
  AppVerRevision := LoWord(FI.dwFileVersionLS);
  AppVersion := Format('%d.%d.%d.%d', [AppVerMajor, AppVerMinor, AppVerRelease, AppVerRevision]);
  // Fetch language code and file description
  VerQueryValue(ptrVerBuf,'\\VarFileInfo\\Translation', Translation, dwInfoSize);
  VerQueryValue(ptrVerBuf,
    PChar(Format('\\StringFileInfo\\%.4x%.4x\\%s',
      [LoWord(Longint(translation^)), HiWord(Longint(Translation^)), 'FileDescription'])),
    Info,
    dwInfoSize);
  SetString(AppDescription, PChar(Info), dwInfoSize-1);
  FreeMem(ptrVerBuf);

  // "All users" folder for HeidiSQL's data (All Users\Application Data)
  DirnameCommonAppData := GetShellFolder(CSIDL_COMMON_APPDATA) + '\' + APPNAME + '\';

  // User folder for HeidiSQL's data (<user name>\Application Data)
  DirnameUserAppData := GetShellFolder(CSIDL_APPDATA) + '\' + APPNAME + '\';
  // Ensure directory exists
  ForceDirectories(DirnameUserAppData);

  // Folder which contains snippet-files
  DirnameSnippets := DirnameCommonAppData + 'Snippets\';
  SnippetFilenames := GetFilesFromDir(DirnameSnippets, '*.sql', True);

  // SQLFiles-History
  FillPopupQueryLoad;

  // Create function menu items in popupQuery and popupFilter
  menuQueryInsertFunction.Clear;
  menuFilterInsertFunction.Clear;
  FunctionCategories := GetFunctionCategories;
  for i:=0 to FunctionCategories.Count-1 do begin
    // Create a menu item which gets subitems later
    miGroup := TMenuItem.Create(popupQuery);
    miGroup.Caption := FunctionCategories[i];
    menuQueryInsertFunction.Add(miGroup);
    miFilterGroup := TMenuItem.Create(popupFilter);
    miFilterGroup.Caption := miGroup.Caption;
    menuFilterInsertFunction.Add(miFilterGroup);
    for j:=0 to Length(MySqlFunctions)-1 do begin
      if MySqlFunctions[j].Category <> FunctionCategories[i] then
        continue;
      miFunction := TMenuItem.Create(popupQuery);
      miFunction.Caption := MySqlFunctions[j].Name;
      miFunction.ImageIndex := 13;
      // Prevent generating a hotkey
      miFunction.Caption := StringReplace(miFunction.Caption, '&', '&&', [rfReplaceAll]);
      // Prevent generating a seperator line
      if miFunction.Caption = '-' then
        miFunction.Caption := '&-';
      miFunction.Hint := MySqlFunctions[j].Name + MySqlFunctions[j].Declaration + ' - ' + sstr(MySqlFunctions[j].Description, 200);
      // Prevent generating a seperator for ShortHint and LongHint
      miFunction.Hint := StringReplace( miFunction.Hint, '|', '¦', [rfReplaceAll] );
      miFunction.Tag := j;
      // Place menuitem on menu
      miFunction.OnClick := insertFunction;
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
  FunctionCategories.Free;

  Delimiter := GetRegValue(REGNAME_DELIMITER, DEFAULT_DELIMITER);

  // Delphi work around to force usage of Vista's default font (other OSes will be unaffected)
  SetVistaFonts(Font);
  InheritFont(Font);
  InheritFont(SynCompletionProposal.Font);
  InheritFont(ParameterCompletionProposal.Font);
  // Simulated link label, has non inherited blue font color
  InheritFont(lblExplainProcess.Font);

  StatusBar.Height := GetTextHeight(StatusBar.Font)+4;
  // Upscale panels in non-96-DPI mode
  DpiScaleFactor := Screen.PixelsPerInch / FORMS_DPI;
  for i:=StatusBar.Panels.Count-1 downto 1 do
    StatusBar.Panels[i].Width := Round(StatusBar.Panels[i].Width * DpiScaleFactor);

  QueryTab := TQueryTab.Create;
  QueryTab.TabSheet := tabQuery;
  QueryTab.Number := 1;
  QueryTab.pnlMemo := pnlQueryMemo;
  QueryTab.treeHelpers := treeQueryHelpers;
  QueryTab.Memo := SynMemoQuery;
  QueryTab.MemoLineBreaks := lbsNone;
  QueryTab.spltHelpers := spltQueryHelpers;
  QueryTab.spltQuery := spltQuery;
  QueryTab.tabsetQuery := tabsetQuery;
  QueryTab.ResultTabs := TResultTabs.Create(True);

  QueryTabs := TObjectList<TQueryTab>.Create(True);
  QueryTabs.Add(QueryTab);

  // Enable auto completion in data tab, filter editor
  SynCompletionProposal.AddEditor(SynMemoFilter);
  ParameterCompletionProposal.AddEditor(SynMemoFilter);

  // Fix node height on Virtual Trees for current DPI settings
  FixVT(DBTree);
  FixVT(ListDatabases);
  FixVT(ListVariables);
  FixVT(ListStatus);
  FixVT(ListProcesses);
  FixVT(ListCommandStats);
  FixVT(ListTables);
  FixVT(treeQueryHelpers);

  // Window dimensions
  Left := GetRegValue(REGNAME_WINDOWLEFT, Left);
  Top := GetRegValue(REGNAME_WINDOWTOP, Top);
  Width := GetRegValue(REGNAME_WINDOWWIDTH, Width);
  Height := GetRegValue(REGNAME_WINDOWHEIGHT, Height);
  // Move window to left and/or top edge of monitor, if screen resolution has been decreased
  if Left > Monitor.Left+Monitor.Width-100 then
    Left := 0;
  if Top > Monitor.Top+Monitor.Height-100 then
    Top := 0;
  WinState := GetRegValue(REGNAME_WINDOWSTATE, '');
  if WinState = 'Minimized' then WindowState := wsMinimized else
  if WinState = 'Maximized' then WindowState := wsMaximized else
  WindowState := wsNormal;

  // Position of Toolbars
  ToolBarStandard.Left := GetRegValue(REGNAME_TOOLBAR2LEFT, ToolBarStandard.Left);
  ToolBarStandard.Top := GetRegValue(REGNAME_TOOLBAR2TOP, ToolBarStandard.Top);
  ToolBarData.Left := GetRegValue(REGNAME_TOOLBARDATALEFT, ToolBarData.Left);
  ToolBarData.Top := GetRegValue(REGNAME_TOOLBARDATATOP, ToolBarData.Top);
  ToolBarQuery.Left := GetRegValue(REGNAME_TOOLBARQUERYLEFT, ToolBarQuery.Left);
  ToolBarQuery.Top := GetRegValue(REGNAME_TOOLBARQUERYTOP, ToolBarQuery.Top);
  actQueryStopOnErrors.Checked := GetRegValue(REGNAME_STOPONERRORSINBATCH, DEFAULT_STOPONERRORSINBATCH);
  actBlobAsText.Checked := GetRegValue(REGNAME_BLOBASTEXT, DEFAULT_BLOBASTEXT);
  actQueryWordWrap.Checked := GetRegValue(REGNAME_WRAPLINES, actQueryWordWrap.Checked);

  pnlQueryMemo.Height := GetRegValue(REGNAME_QUERYMEMOHEIGHT, pnlQueryMemo.Height);
  treeQueryHelpers.Width := GetRegValue(REGNAME_QUERYHELPERSWIDTH, treeQueryHelpers.Width);
  pnlLeft.Width := GetRegValue(REGNAME_DBTREEWIDTH, pnlLeft.Width);
  pnlPreview.Height := GetRegValue(REGNAME_PREVIEW_HEIGHT, pnlPreview.Height);
  if GetRegValue(REGNAME_PREVIEW_ENABLED, actDataPreview.Checked) and (not actDataPreview.Checked) then
    actDataPreviewExecute(actDataPreview);
  SynMemoSQLLog.Height := GetRegValue(REGNAME_SQLOUTHEIGHT, SynMemoSQLLog.Height);
  // Force status bar position to below log memo
  StatusBar.Top := SynMemoSQLLog.Top + SynMemoSQLLog.Height;
  prefMaxColWidth := GetRegValue(REGNAME_MAXCOLWIDTH, DEFAULT_MAXCOLWIDTH);
  prefGridRowcountMax := GetRegValue(REGNAME_MAXTOTALROWS, DEFAULT_MAXTOTALROWS);
  prefGridRowcountStep := GetRegValue(REGNAME_ROWSPERSTEP, DEFAULT_ROWSPERSTEP);
  prefGridRowsLineCount := GetRegValue(REGNAME_GRIDROWSLINECOUNT, DEFAULT_GRIDROWSLINECOUNT);
  actDataShowNext.Hint := 'Show next '+FormatNumber(prefGridRowcountStep)+' rows ...';
  actAboutBox.Caption := 'About '+APPNAME+' '+AppVersion;
  // Fix registry entry from older versions which can have 0 here which makes no sense
  // since the autosetting was removed
  if prefMaxColWidth <= 0 then
    prefMaxColWidth := DEFAULT_MAXCOLWIDTH;
  prefLogsqlnum := GetRegValue(REGNAME_LOGSQLNUM, DEFAULT_LOGSQLNUM);
  prefLogSqlWidth := GetRegValue(REGNAME_LOGSQLWIDTH, DEFAULT_LOGSQLWIDTH);
  prefDirnameSessionLogs := GetRegValue(REGNAME_LOGDIR, DirnameUserAppData + 'Sessionlogs\');
  // Activate logging
  if GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE) then
    ActivateFileLogging;
  prefCSVSeparator := GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  prefCSVEncloser := GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  prefCSVTerminator := GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  prefExportLocaleNumbers := GetRegValue(REGNAME_EXPORT_LOCALENUMBERS, DEFAULT_EXPORT_LOCALENUMBERS);
  prefRememberFilters := GetRegValue(REGNAME_REMEMBERFILTERS, DEFAULT_REMEMBERFILTERS);
  if GetRegValue(REGNAME_LOG_HORIZONTALSCROLLBAR, SynMemoSQLLog.ScrollBars = ssBoth) then
    menuLogHorizontalScrollbar.OnClick(menuLogHorizontalScrollbar);
  prefLogErrors := GetRegValue(REGNAME_LOG_ERRORS, DEFAULT_LOG_ERRORS);
  prefLogUserSQL := GetRegValue(REGNAME_LOG_USERSQL, DEFAULT_LOG_USERSQL);
  prefLogSQL := GetRegValue(REGNAME_LOG_SQL, DEFAULT_LOG_SQL);
  prefLogInfos := GetRegValue(REGNAME_LOG_INFOS, DEFAULT_LOG_INFOS);
  prefLogDebug := GetRegValue(REGNAME_LOG_DEBUG, DEFAULT_LOG_DEBUG);
  prefDisplayBars := GetRegValue(REGNAME_DISPLAYBARS, DEFAULT_DISPLAYBARS);
  prefBarColor := GetRegValue(REGNAME_BARCOLOR, DEFAULT_BARCOLOR);
  prefCompletionProposal := GetRegValue(REGNAME_COMPLETIONPROPOSAL, DEFAULT_COMPLETIONPROPOSAL);
  prefMaxQueryResults := GetRegValue(REGNAME_MAXQUERYRESULTS, DEFAULT_MAXQUERYRESULTS);

  // Data-Font:
  datafontname := GetRegValue(REGNAME_DATAFONTNAME, DEFAULT_DATAFONTNAME);
  datafontsize := GetRegValue(REGNAME_DATAFONTSIZE, DEFAULT_DATAFONTSIZE);
  DataGrid.Font.Name := datafontname;
  QueryGrid.Font.Name := datafontname;
  DataGrid.Font.Size := datafontsize;
  QueryGrid.Font.Size := datafontsize;
  FixVT(DataGrid, prefGridRowsLineCount);
  FixVT(QueryGrid, prefGridRowsLineCount);
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

  // Switch off/on displaying table/db sized in tree
  menuShowSizeColumn.Checked := GetRegValue(REGNAME_SIZECOL_TREE, DEFAULT_SIZECOL_TREE);
  if menuShowSizeColumn.Checked then
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options + [coVisible]
  else
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options - [coVisible];

  // Restore width of columns of all VirtualTrees
  RestoreListSetup(ListDatabases);
  RestoreListSetup(ListVariables);
  RestoreListSetup(ListStatus);
  RestoreListSetup(ListProcesses);
  RestoreListSetup(ListCommandStats);
  RestoreListSetup(ListTables);

  // Shortcuts
  for i:=0 to ActionList1.ActionCount-1 do begin
    Action := TAction(ActionList1.Actions[i]);
    Action.ShortCut := GetRegValue(REGPREFIX_SHORTCUT1+Action.Name, Action.ShortCut);
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

  // SynMemo font, hightlighting and shortcuts
  SetupSynEditors;

  btnAddTab := TSpeedButton.Create(PageControlMain);
  btnAddTab.Parent := PageControlMain;
  ImageListMain.GetBitmap(actNewQueryTab.ImageIndex, btnAddTab.Glyph);
  btnAddTab.Height := PageControlMain.TabRect(0).Bottom - PageControlMain.TabRect(0).Top - 2;
  btnAddTab.Width := btnAddTab.Height;
  btnAddTab.Flat := True;
  btnAddTab.Hint := actNewQueryTab.Hint;
  btnAddTab.OnClick := actNewQueryTab.OnExecute;

  // Filter panel
  ImageListMain.GetBitmap(134, btnCloseFilterPanel.Glyph);
  if GetRegValue(REGNAME_FILTERACTIVE, DEFAULT_FILTERACTIVE) then
    actFilterPanelExecute(nil);
  lblFilterVTInfo.Caption := '';

  SelectedTableColumns := TTableColumnList.Create;
  SelectedTableKeys := TTableKeyList.Create;
  SelectedTableForeignKeys := TForeignKeyList.Create;

  // Set up connections list
  FConnections := TMySQLConnectionList.Create;
  FConnections.OnNotify := ConnectionsNotify;

  // Load database filter items. Was previously bound to sessions before multi connections were implemented
  comboDBFilter.Items.Text := GetRegValue(REGNAME_DATABASE_FILTER, '');
  if comboDBFilter.Items.Count > 0 then
    comboDBFilter.ItemIndex := 0
  else
    comboDBFilter.Text := '';

  FTreeRefreshInProgress := False;

  FileEncodings := Explode(',', 'Auto detect (may fail),ANSI,ASCII,Unicode,Unicode Big Endian,UTF-8,UTF-7');
end;


{**
  Check for connection parameters on commandline or show connections form.
}
procedure TMainForm.Startup;
var
  CmdlineParameters, LastSessions: TStringlist;
  Connection: TMySQLConnection;
  LoadedParams: TConnectionParameters;
  LastUpdatecheck, LastStatsCall, LastConnect: TDateTime;
  UpdatecheckInterval, i: Integer;
  DefaultLastrunDate, LastActiveSession, StatsURL: String;
  frm : TfrmUpdateCheck;
  Connected, DecideForStatistic: Boolean;
  StatsCall: TDownloadUrl2;
  SessionNames: TStringlist;
  DlgResult: TModalResult;
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

  // Get all session names
  SessionNames := TStringlist.Create;
  if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, true) then
    MainReg.GetKeyNames(SessionNames);

  // Call user statistics if checked in settings
  if GetRegValue(REGNAME_DO_STATISTICS, DEFAULT_DO_STATISTICS) then begin
    try
      LastStatsCall := StrToDateTime( GetRegValue(REGNAME_LAST_STATSCALL, DefaultLastrunDate) );
    except
      LastStatsCall := StrToDateTime( DefaultLastrunDate );
    end;
    if DaysBetween(Now, LastStatsCall) >= 30 then begin
      // Report used SVN revision
      StatsURL := APPDOMAIN + 'savestats.php?c=' + IntToStr(AppVerRevision);
      // Enumerate actively used server versions
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
      StatsCall.SetUserAgent(APPNAME + ' ' + AppVersion);
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

  OpenRegistry;
  CmdlineParameters := TStringList.Create;
  for i:=1 to ParamCount do
    CmdlineParameters.Add(ParamStr(i));
  ParseCommandLineParameters(CmdlineParameters);
  if Assigned(FCmdlineConnectionParams) then begin
    // Minimal parameter for command line mode is hostname
    Connected := InitConnection(FCmdlineConnectionParams, FCmdlineSessionName, True, Connection);
  end else if GetRegValue(REGNAME_AUTORECONNECT, DEFAULT_AUTORECONNECT) then begin
    // Auto connection via preference setting
    // Do not autoconnect if we're in commandline mode and the connection was not successful
    LastSessions := Explode(DELIM, GetRegValue(REGNAME_LASTSESSIONS, ''));
    LastActiveSession := GetRegValue(REGNAME_LASTACTIVESESSION, '');
    for i:=LastSessions.Count-1 downto 0 do begin
      if SessionNames.IndexOf(LastSessions[i]) = -1 then
        LastSessions.Delete(i);
    end;
    if LastSessions.Count > 0 then begin
      if LastSessions.IndexOf(LastActiveSession) = -1 then
        LastActiveSession := LastSessions[0];
      for i:=0 to LastSessions.Count-1 do begin
        try
          LoadedParams := LoadConnectionParams(LastSessions[i]);
          if InitConnection(LoadedParams, LastSessions[i], LastActiveSession=LastSessions[i], Connection) then
            Connected := True;
        except on E:Exception do
          MessageDlg(E.Message, mtError, [mbOK], 0);
        end;
      end;
    end;
  end;

  // Display session manager
  if not Connected then begin
    // Cannot be done in OnCreate because we need ready forms here:
    if not Assigned(SessionManager) then
      SessionManager := TConnForm.Create(Self);
    DlgResult := mrCancel;
    try
      DlgResult := SessionManager.ShowModal;
    except
      // Work around VCL bug: Suppress access violation in TCustomForm.IsFormSizeStored
      // when closing dialog via Alt+F4
    end;
    if DlgResult = mrCancel then begin
      Free;
      Exit;
    end;
  end;

  // Load SQL file(s) by command line
  for i:=0 to FCmdlineFilenames.Count-1 do begin
    if i>0 then
      actNewQueryTabExecute(Self);
    if not QueryLoad(FCmdlineFilenames[i], True, nil) then
      actCloseQueryTabExecute(Self);
  end;
end;


procedure TMainForm.ParseCommandLineParameters(Parameters: TStringlist);
var
  rx: TRegExpr;
  AllParams, Host, User, Pass, Socket: String;
  i, Port: Integer;

  function GetParamValue(ShortName, LongName: String): String;
  begin
    Result := '';
    rx.Expression := '\s(\-'+ShortName+'|\-\-'+LongName+')\s*\=?\s*([^\-]\S*)';
    if rx.Exec(AllParams) then
      Result := rx.Match[2];
  end;

begin
  // Initialize and clear variables
  if not Assigned(FCmdlineFilenames) then
    FCmdlineFilenames := TStringlist.Create;
  FCmdlineFilenames.Clear;
  FCmdlineSessionName := '';
  FreeAndNil(FCmdlineConnectionParams);

  // Prepend a space, so the regular expression can request a mandantory space
  // before each param name including the first one
  AllParams := ' ' + ImplodeStr(' ', Parameters);
  rx := TRegExpr.Create;
  FCmdlineSessionName := GetParamValue('d', 'description');
  if FCmdlineSessionName <> '' then begin
    try
      FCmdlineConnectionParams := LoadConnectionParams(FCmdlineSessionName);
    except
      on E:Exception do begin
        // Session params not found in registry
        LogSQL(E.Message);
        FCmdlineSessionName := '';
      end;
    end;

  end;

  // Test if params were passed. If given, override previous values loaded from registry.
  // Enables the user to log into a session with a different, non-stored user: -dSession -uSomeOther
  Host := GetParamValue('h', 'host');
  User := GetParamValue('u', 'user');
  Pass := GetParamValue('p', 'password');
  Socket := GetParamValue('S', 'socket');
  Port := StrToIntDef(GetParamValue('P', 'port'), 0);
  // Leave out support for startup script, seems reasonable for command line connecting

  if (Host <> '') or (User <> '') or (Pass <> '') or (Port <> 0) or (Socket <> '') then begin
    if not Assigned(FCmdlineConnectionParams) then
      FCmdlineConnectionParams := TConnectionParameters.Create;
    if Host <> '' then FCmdlineConnectionParams.Hostname := Host;
    if User <> '' then FCmdlineConnectionParams.Username := User;
    if Pass <> '' then FCmdlineConnectionParams.Password := Pass;
    if Port <> 0 then FCmdlineConnectionParams.Port := Port;
    if Socket <> '' then begin
      FCmdlineConnectionParams.Hostname := Socket;
      FCmdlineConnectionParams.NetType := ntNamedPipe;
    end;
    // Ensure we have a session name to pass to InitConnection
    if (FCmdlineSessionName = '') and (FCmdlineConnectionParams.Hostname <> '') then
      FCmdlineSessionName := FCmdlineConnectionParams.Hostname;
  end;

  // Check for valid filename(s) in parameters
  for i:=0 to Parameters.Count-1 do begin
    if FileExists(Parameters[i]) then
      FCmdlineFilenames.Add(Parameters[i]);
  end;
end;


procedure TMainForm.actSessionManagerExecute(Sender: TObject);
begin
  if not Assigned(SessionManager) then
    SessionManager := TConnForm.Create(Self);
  SessionManager.ShowModal;
end;


procedure TMainForm.actDisconnectExecute(Sender: TObject);
var
  Connection: TMySQLConnection;
  Node: PVirtualNode;
begin
  // Disconnect active connection. If it's the last, exit application
  if FConnections.Count = 1 then
    actExitApplication.Execute
  else begin
    Connection := ActiveConnection;
    // Find and remove connection node from tree
    Node := GetRootNode(DBtree, Connection);
    DBTree.DeleteNode(Node, True);
    FConnections.Remove(Connection);
    // TODO: focus last session?
    SelectNode(DBtree, GetNextNode(DBtree, nil));
  end;
end;


procedure TMainForm.ConnectionsNotify(Sender: TObject; const Item: TMySQLConnection; Action: TCollectionNotification);
var
  Results: TMySQLQuery;
  Tab: TQueryTab;
begin
  // Connection removed or added
  case Action of
    cnRemoved, cnExtracted: begin
      // Post pending UPDATE
      Results := GridResult(DataGrid);
      if Assigned(Results) and Results.Modified then
        actDataPostChangesExecute(DataGrid);

      // Remove result sets which may cause AVs when disconnected
      for Tab in QueryTabs do begin
        if Assigned(Tab.QueryProfile) and (Tab.QueryProfile.Connection = Item) then
          FreeAndNil(Tab.QueryProfile);
      end;

      {// TODO: Clear database and table lists
      DBtree.ClearSelection;
      DBtree.FocusedNode := nil;
      FreeAndNil(DataGridHiddenColumns);
      SynMemoFilter.Clear;
      SetLength(DataGridSortColumns, 0);
      RefreshHelperNode(HELPERNODE_PROFILE);
      RefreshHelperNode(HELPERNODE_COLUMNS);}

      // Last chance to access connection related properties before disconnecting
      OpenRegistry(Item.SessionName);
      MainReg.WriteString(REGNAME_LASTUSEDDB, Item.Database);

      // Disconnect
      Item.Active := False;
    end;

    // New connection
    cnAdded: DBTree.InsertNode(DBTree.GetLastChild(nil), amInsertAfter);
  end;
end;


procedure TMainForm.actCreateDatabaseExecute(Sender: TObject);
begin
  // Create database:
  // Create modal form once on demand
  if CreateDatabaseForm = nil then
    CreateDatabaseForm := TCreateDatabaseForm.Create(Self);

  // Rely on the modalresult being set correctly
  if CreateDatabaseForm.ShowModal = mrOK then
    RefreshTree;
end;


procedure TMainForm.actImportCSVExecute(Sender: TObject);
begin
  // Import Textfile
  if not Assigned(ImportTextfileDialog) then
    ImportTextfileDialog := Tloaddataform.Create(Self);
  ImportTextfileDialog.ShowModal;
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
  // Exit early when user pressed "Cancel" on connection dialog
  if csDestroying in ComponentState then
    Exit;
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
  FixQueryTabCloseButtons;
end;

procedure TMainForm.actUserManagerExecute(Sender: TObject);
begin
  if UserManagerForm = nil then
    UserManagerForm := TUserManagerForm.Create(Self);
  UserManagerForm.ShowModal;
end;

procedure TMainForm.actAboutBoxExecute(Sender: TObject);
var
  Box: TAboutBox;
begin
  // Info-Box
  Box := TAboutBox.Create(Self);
  Box.ShowModal;
  Box.Free;
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
  if QueryTabActive then begin
    ActiveQueryTab.MemoFilename := '';
    ActiveQueryTab.Memo.Modified := False;
  end;
  if m = SynMemoFilter then
    InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
end;

procedure TMainForm.actTableToolsExecute(Sender: TObject);
var
  Act: TAction;
  InDBTree: Boolean;
  Node: PVirtualNode;
  DBObj: PDBObject;
begin
  // Show table tools dialog
  if TableToolsDialog = nil then
    TableToolsDialog := TfrmTableTools.Create(Self);
  Act := Sender as TAction;
  TableToolsDialog.PreSelectObjects.Clear;
  InDBTree := (Act.ActionComponent is TMenuItem)
    and (TPopupMenu((Act.ActionComponent as TMenuItem).GetParentMenu).PopupComponent = DBTree);
  if InDBTree then
    TableToolsDialog.PreSelectObjects.Add(ActiveDbObj)
  else begin
    Node := GetNextNode(ListTables, nil, True);
    while Assigned(Node) do begin
      DBObj := ListTables.GetNodeData(Node);
      TableToolsDialog.PreSelectObjects.Add(DBObj^);
      Node := GetNextNode(ListTables, Node, True);
    end;
  end;
  if Sender = actMaintenance then
    TableToolsDialog.ToolMode := tmMaintenance
  else if Sender = actFindTextOnServer then
    TableToolsDialog.ToolMode := tmFind
  else if Sender = actExportTables then
    TableToolsDialog.ToolMode := tmSQLExport
  else if Sender = actBulkTableEdit then
    TableToolsDialog.ToolMode := tmBulkTableEdit;
  TableToolsDialog.ShowModal;
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
  if not Assigned(CopyTableDialog) then
    CopyTableDialog := TCopyTableForm.Create(Self);
  CopyTableDialog.ShowModal;
end;


procedure TMainForm.menuConnectionsPopup(Sender: TObject);
var
  i: integer;
  item: TMenuItem;
  SessionNames: TStringList;
  Connection: TMySQLConnection;
begin
  // Delete dynamically added connection menu items.
  menuConnections.Items.Clear;

  // "Session manager" and "New window" items
  item := TMenuItem.Create(menuConnections);
  item.Action := actSessionManager;
  item.Default := True;
  menuConnections.Items.Add(item);
  item := TMenuItem.Create(menuConnections);
  item.Action := actNewWindow;
  menuConnections.Items.Add(item);
  item := TMenuItem.Create(menuConnections);
  item.Caption := '-';
  menuConnections.Items.Add(item);

  // All sessions
  if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, False) then begin
    SessionNames := TStringList.Create;
    MainReg.GetKeyNames(SessionNames);
    for i:=0 to SessionNames.Count-1 do begin
      item := TMenuItem.Create(menuConnections);
      item.Caption := SessionNames[i];
      item.OnClick := SessionConnect;
      item.ImageIndex := 37;
      for Connection in Connections do begin
        if SessionNames[i] = Connection.SessionName then begin
          item.Checked := True;
          item.ImageIndex := -1;
          break;
        end;
      end;
      menuConnections.Items.Add(item);
    end;
  end;

end;


procedure TMainForm.File1Click(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  SessionNames, ConnectedSessions: TStringList;
begin
  // Decide if "Connect to" menu should be enabled
  menuConnectTo.Enabled := False;
  if MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, False) then begin
    menuConnectTo.Enabled := MainReg.HasSubKeys;
    if menuConnectTo.Enabled then begin
      // Add all sessions to submenu
      for i := menuConnectTo.Count - 1 downto 0 do
        menuConnectTo.Delete(i);
      ConnectedSessions := TStringList.Create;
      for i:=0 to Connections.Count-1 do
        ConnectedSessions.Add(Connections[i].SessionName);
      SessionNames := TStringList.Create;
      MainReg.GetKeyNames(SessionNames);
      for i:=0 to SessionNames.Count-1 do begin
        Item := TMenuItem.Create(menuConnectTo);
        Item.Caption := SessionNames[i];
        Item.OnClick := SessionConnect;
        Item.ImageIndex := 37;
        if ConnectedSessions.IndexOf(SessionNames[i]) > -1 then begin
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


procedure TMainForm.actExportSettingsExecute(Sender: TObject);
begin
  // Export settings to .reg-file
  if SaveDialog2.Execute then
    ShellExec('regedit.exe', '', '/e "'+SaveDialog2.FileName+'" HKEY_CURRENT_USER'+REGPATH);
end;

procedure TMainForm.actImportSettingsExecute(Sender: TObject);
begin
  // Import settings from .reg-file
  if OpenDialog2.Execute then
    ShellExec('regedit.exe', '', '"'+OpenDialog2.FileName+'"');
end;

procedure TMainForm.actExecuteQueryExecute(Sender: TObject);
var
  SQLBatch: TSQLBatch;
  Query: TSQLSentence;
  i, j, QueryCount, StartOffsetInMemo: Integer;
  SQLTime, SQLNetTime: Cardinal;
  RowsAffected, RowsFound: Int64;
  Results: TMySQLQuery;
  Text, LB, MetaInfo, TabCaption: String;
  QueryTab: TQueryTab;
  NewTab: TResultTab;
  col: TVirtualTreeColumn;
  DoProfile: Boolean;
  ProfileNode: PVirtualNode;
  Time: Extended;

  procedure GoToErrorPos(Err: String);
  var
    rx: TRegExpr;
    SelStart, ErrorPos: Integer;
  begin
    // Try to set memo cursor to the relevant position
    SelStart := StartOffsetInMemo;

    // "... for the right syntax to use near 'lik 123)' at line 4"
    rx := TRegExpr.Create;
    rx.Expression := 'for the right syntax to use near ''(.+)'' at line (\d+)';
    if rx.Exec(Err) then begin
      // Examine 1kb of memo text at given offset
      ErrorPos := Pos(rx.Match[1], Copy(QueryTab.Memo.Text, SelStart, SIZE_KB));
      if ErrorPos > 0 then
        Inc(SelStart, ErrorPos-1);
    end;

    QueryTab.Memo.SelLength := 0;
    QueryTab.Memo.SelStart := SelStart;
  end;

begin
  Screen.Cursor := crHourglass;
  QueryTab := ActiveQueryTab;

  ShowStatusMsg('Splitting SQL queries ...');
  if Sender = actExecuteCurrentQuery then begin
    SQLBatch := GetSQLSplitMarkers(QueryTab.Memo.Text);
    for Query in SQLBatch do begin
      if (Query.LeftOffset <= QueryTab.Memo.SelStart) and (QueryTab.Memo.SelStart < Query.RightOffset) then begin
        Text := Copy(QueryTab.Memo.Text, Query.LeftOffset, Query.RightOffset-Query.LeftOffset);
        StartOffsetInMemo := Query.LeftOffset;
        break;
      end;
    end;
  end else if Sender = actExecuteSelection then begin
    Text := QueryTab.Memo.SelText;
    StartOffsetInMemo := QueryTab.Memo.SelStart;
  end else begin
    Text := QueryTab.Memo.Text;
    StartOffsetInMemo := 0;
  end;
  // Give text back its original linebreaks if possible
  case QueryTab.MemoLineBreaks of
    lbsUnix: LB := LB_UNIX;
    lbsMac: LB := LB_MAC;
    lbsWide: LB := LB_WIDE;
  end;
  if LB <> '' then
    Text := StringReplace(Text, CRLF, LB, [rfReplaceAll]);
  SQLBatch := SplitSQL(Text);

  EnableProgressBar(SQLBatch.Count);
  SQLtime := 0;
  SQLNetTime := 0;
  QueryCount := 0;
  RowsAffected := 0;
  RowsFound := 0;
  QueryTab.ResultTabs.Clear;
  QueryTab.tabsetQuery.Tabs.Clear;
  FreeAndNil(QueryTab.QueryProfile);
  ProfileNode := FindNode(QueryTab.treeHelpers, HELPERNODE_PROFILE, nil);
  DoProfile := Assigned(ProfileNode) and (QueryTab.treeHelpers.CheckState[ProfileNode] in CheckedStates);
  if DoProfile then try
    ActiveConnection.Query('SET profiling=1');
  except
    on E:EDatabaseError do begin
      MessageDlg('Query profiling requires MySQL 5.0.37 or later, and the server must not be configured with --disable-profiling.'+CRLF+CRLF+E.Message, mtError, [mbOK], 0);
      DoProfile := False;
    end;
  end;
  for i:=0 to SQLBatch.Count-1 do begin
    ShowStatusMsg('Executing query #'+FormatNumber(i+1)+' of '+FormatNumber(SQLBatch.Count)+' ...');
    ProgressBarStatus.StepIt;
    ProgressBarStatus.Repaint;
    try
      ActiveConnection.Query(SQLBatch[i].SQL, QueryTab.ResultTabs.Count < prefMaxQueryResults, lcUserFiredSQL);
      Inc(QueryCount);
      Inc(SQLtime, ActiveConnection.LastQueryDuration);
      Inc(SQLNetTime, ActiveConnection.LastQueryNetworkDuration);
      Inc(RowsAffected, ActiveConnection.RowsAffected);
      Inc(RowsFound, ActiveConnection.RowsFound);
      if (ActiveConnection.ResultCount > 0) then for Results in ActiveConnection.GetLastResults do begin
        NewTab := TResultTab.Create;
        QueryTab.ResultTabs.Add(NewTab);
        NewTab.Results := Results;
        try
          TabCaption := NewTab.Results.TableName;
        except on E:EDatabaseError do
          TabCaption := 'Result #'+IntToStr(QueryTab.ResultTabs.Count);
        end;
        QueryTab.tabsetQuery.Tabs.Add(TabCaption);

        ShowStatusMsg('Setting up result grid ...');
        NewTab.Grid.BeginUpdate;
        NewTab.Grid.Header.Options := NewTab.Grid.Header.Options + [hoVisible];
        NewTab.Grid.Header.Columns.BeginUpdate;
        NewTab.Grid.Header.Columns.Clear;
        for j:=0 to NewTab.Results.ColumnCount-1 do begin
          col := NewTab.Grid.Header.Columns.Add;
          col.Text := NewTab.Results.ColumnNames[j];
          if NewTab.Results.DataType(j).Category in [dtcInteger, dtcReal] then
            col.Alignment := taRightJustify;
          if NewTab.Results.ColIsPrimaryKeyPart(j) then
            col.ImageIndex := ICONINDEX_PRIMARYKEY
          else if NewTab.Results.ColIsUniqueKeyPart(j) then
            col.ImageIndex := ICONINDEX_UNIQUEKEY
          else if NewTab.Results.ColIsKeyPart(j) then
            col.ImageIndex := ICONINDEX_INDEXKEY;
        end;
        NewTab.Grid.Header.Columns.EndUpdate;
        NewTab.Grid.RootNodeCount := NewTab.Results.RecordCount;
        NewTab.Grid.EndUpdate;
        for j:=0 to NewTab.Grid.Header.Columns.Count-1 do
          AutoCalcColWidth(NewTab.Grid, j);
      end;
    except
      on E:EDatabaseError do begin
        if actQueryStopOnErrors.Checked or (i = SQLBatch.Count - 1) then begin
          Screen.Cursor := crDefault;
          ProgressBarStatus.State := pbsError;
          GoToErrorPos(E.Message);
          MessageDlg( E.Message, mtError, [mbOK], 0 );
          Break;
        end;
      end;
    end;
  end;

  // Gather meta info for logging
  if QueryCount > 0 then begin
    MetaInfo := FormatNumber(RowsAffected) + ' rows affected, ' + FormatNumber(RowsFound) + ' rows found.';
    MetaInfo := MetaInfo + ' Duration for ' + IntToStr(QueryCount);
    if QueryCount < SQLBatch.Count then
      MetaInfo := MetaInfo + ' of ' + IntToStr(SQLBatch.Count);
    if SQLBatch.Count = 1 then
      MetaInfo := MetaInfo + ' query'
    else
      MetaInfo := MetaInfo + ' queries';
    MetaInfo := MetaInfo + ': '+FormatNumber(SQLTime/1000, 3) +' sec.';
    if SQLNetTime > 0 then
      MetaInfo := MetaInfo + ' (+ '+FormatNumber(SQLNetTime/1000, 3) +' sec. network)';
    LogSQL(MetaInfo);
  end;

  if DoProfile then begin
    QueryTab.QueryProfile := ActiveConnection.GetResults('SHOW PROFILE');
    QueryTab.ProfileTime := 0;
    QueryTab.MaxProfileTime := 0;
    while not QueryTab.QueryProfile.Eof do begin
      Time := MakeFloat(QueryTab.QueryProfile.Col(1));
      QueryTab.ProfileTime := QueryTab.ProfileTime + Time;
      QueryTab.MaxProfileTime := Max(Time, QueryTab.MaxProfileTime);
      QueryTab.QueryProfile.Next;
    end;
    QueryTab.treeHelpers.ReinitNode(ProfileNode, True);
    QueryTab.treeHelpers.InvalidateChildren(ProfileNode, True);
    ActiveConnection.Query('SET profiling=0');
  end;

  // Clean up
  ProgressBarStatus.Hide;
  if QueryTab.tabsetQuery.Tabs.Count > 0 then
    QueryTab.tabsetQuery.TabIndex := 0;
  Screen.Cursor := crDefault;
  ShowStatusMsg;
end;


procedure TMainForm.tabsetQueryClick(Sender: TObject);
var
  QueryTab: TQueryTab;
  i: Integer;
begin
  // Result tab clicked / changed
  Screen.Cursor := crHourGlass;
  QueryTab := ActiveQueryTab;
  for i:=0 to QueryTab.ResultTabs.Count-1 do
    QueryTab.ResultTabs[i].Grid.Hide;
  if QueryTab.ActiveResultTab <> nil then begin
    QueryTab.ActiveResultTab.Grid.Show;
    // Reset filter if filter panel was disabled
    UpdateFilterPanel(Sender);
  end;
  // Ensure controls are in a valid state
  ValidateControls(Sender);
  Screen.Cursor := crDefault;
  ShowStatusMsg;
end;


procedure TMainForm.tabsetQueryGetImageIndex(Sender: TObject; TabIndex: Integer;
  var ImageIndex: Integer);
begin
  // Give result tabs of editable results a table icon
  try
    ActiveQueryTab.ResultTabs[TabIndex].Results.TableName;
    ImageIndex := 14;
  except
    ImageIndex := -1;
  end;
end;


procedure TMainForm.actCopyDataExecute(Sender: TObject);
var
  S, HTML: TMemoryStream;
  act: TAction;
  Content: AnsiString;
  ExportFormat: TGridExportFormat;
begin
  // Copy data in focused grid as CSV
  Screen.Cursor := crHourglass;
  S := TMemoryStream.Create;
  try
    act := Sender as TAction;
    if act = actCopyAsCSV then
      ExportFormat := efCSV
    else if act = actCopyAsHTML then
      ExportFormat := efHTML
    else if act = actCopyAsXML then
      ExportFormat := efXML
    else if act = actCopyAsSQL then
      ExportFormat := efSQL
    else if act = actCopyAsLaTeX then
      ExportFormat := efLaTeX
    else if act = actCopyAsWiki then
      ExportFormat := efWiki
    else
      ExportFormat := efUnknown;
    GridExport(ActiveGrid, S, ExportFormat);
    case ExportFormat of
      efSQL: begin
        SetLength(Content, S.Size);
        S.Position := 0;
        S.Read(PAnsiChar(Content)^, S.Size);
        SynExporterHTML1.ExportAll(Explode(CRLF, UTF8ToString(Content)));
        HTML := TMemoryStream.Create;
        SynExporterHTML1.SaveToStream(HTML);
      end;
      efHTML: HTML := S;
      else HTML := nil;
    end;
    StreamToClipboard(S, HTML, (ExportFormat=efHTML) and (HTML <> nil));
  finally
    ShowStatusMsg('Freeing data...');
    S.Free;
    ShowStatusMsg;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actExportDataExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
  FS: TFileStream;
  ExportFormat: TGridExportFormat;
begin
  // Save data in current dataset into various text file formats
  Dialog := SaveDialogExportData;
  Dialog.FileName := BestTableName(GridResult(ActiveGrid));
  Dialog.Title := 'Export result set from '+Dialog.Filename+'...';
  if Dialog.Execute and (Dialog.FileName <> '') then try
    Screen.Cursor := crHourGlass;
    ExportFormat := TGridExportFormat(Dialog.FilterIndex);
    FS := TFileStream.Create(Dialog.FileName, fmCreate or fmOpenWrite);
    GridExport(ActiveGrid, FS, ExportFormat);
  finally
    ShowStatusMsg('Freeing data...');
    FreeAndNil(FS);
    ShowStatusMsg;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actDataPreviewUpdate(Sender: TObject);
var
  Grid: TVirtualStringTree;
begin
  // Enable or disable ImageView action
  Grid := ActiveGrid;
  (Sender as TAction).Enabled := (Grid <> nil)
    and (Grid.FocusedColumn <> NoColumn)
    and (GridResult(Grid).DataType(Grid.FocusedColumn).Category = dtcBinary)
end;


procedure TMainForm.actDataPreviewExecute(Sender: TObject);
var
  MakeVisible: Boolean;
begin
  // Show or hide preview area
  actDataPreview.Checked := not actDataPreview.Checked;
  MakeVisible := actDataPreview.Checked;
  pnlPreview.Visible := MakeVisible;
  spltPreview.Visible := MakeVisible;
  if MakeVisible then
    UpdatePreviewPanel;
end;


procedure TMainForm.UpdatePreviewPanel;
var
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
  RowNum: PCardinal;
  ImgType: String;
  Content, Header: AnsiString;
  ContentStream: TMemoryStream;
  StrLen: Integer;
  GraphicClass: TGraphicExGraphicClass;
  Graphic: TGraphic;
  AllExtensions: TStringList;
  i: Integer;
begin
  // Load BLOB contents into preview area
  Grid := ActiveGrid;
  Results := GridResult(Grid);
  if not Assigned(Results) then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    ShowStatusMsg('Loading contents into image viewer ...');
    lblPreviewTitle.Caption := 'Loading ...';
    lblPreviewTitle.Repaint;
    imgPreview.Picture := nil;
    AnyGridEnsureFullRow(Grid, Grid.FocusedNode);
    RowNum := Grid.GetNodeData(Grid.FocusedNode);
    Results.RecNo := RowNum^;

    Content := AnsiString(Results.Col(Grid.FocusedColumn));
    StrLen := Length(Content);
    ContentStream := TMemoryStream.Create;
    ContentStream.Write(Content[1], StrLen);
    ContentStream.Position := 0;
    GraphicClass := FileFormatList.GraphicFromContent(ContentStream);
    Graphic := nil;
    ContentStream.Position := 0;
    ImgType := 'UnknownType';
    if GraphicClass <> nil then begin
      AllExtensions := TStringList.Create;
      FileFormatList.GetExtensionList(AllExtensions);
      for i:=0 to AllExtensions.Count-1 do begin
        if FileFormatList.GraphicFromExtension(AllExtensions[i]) = GraphicClass then begin
          ImgType := UpperCase(AllExtensions[i]);
          break;
        end;
      end;
      Graphic := GraphicClass.Create;
    end else begin
      Header := Copy(Content, 1, 50);
      if Copy(Header, 7, 4) = 'JFIF' then begin
        ImgType := 'JPEG';
        Graphic := TJPEGImage.Create;
      end else if Copy(Header, 1, 3) = 'GIF' then begin
        ImgType := 'GIF';
        Graphic := TGIFImage.Create;
      end else if Copy(Header, 1, 2) = 'BM' then begin
        ImgType := 'BMP';
        Graphic := TBitmap.Create;
      end;
    end;
    if Assigned(Graphic) then begin
      try
        Graphic.LoadFromStream(ContentStream);
        imgPreview.Picture.Graphic := Graphic;
        lblPreviewTitle.Caption := ImgType+': '+
          IntToStr(Graphic.Width)+' x '+IntToStr(Graphic.Height)+' pixels, 100%, '+
          FormatByteNumber(StrLen);
        spltPreview.OnMoved(spltPreview);
      except on E:EInvalidGraphic do
        lblPreviewTitle.Caption := ImgType+': ' + E.Message;
      end;
      FreeAndNil(ContentStream);
    end else
      lblPreviewTitle.Caption := 'No image detected.';
  finally
    lblPreviewTitle.Hint := lblPreviewTitle.Caption;
    ShowStatusMsg;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.spltPreviewMoved(Sender: TObject);
var
  rx: TRegExpr;
  ZoomFactorW, ZoomFactorH: Integer;
begin
  // Do not overscale image so it's never zoomed to more than 100%
  if (imgPreview.Picture.Graphic = nil) or (imgPreview.Picture.Graphic.Empty) then
    Exit;
  imgPreview.Stretch := (imgPreview.Picture.Width > imgPreview.Width) or (imgPreview.Picture.Height > imgPreview.Height);
  ZoomFactorW := Trunc(Min(imgPreview.Picture.Width, imgPreview.Width) / imgPreview.Picture.Width * 100);
  ZoomFactorH := Trunc(Min(imgPreview.Picture.Height, imgPreview.Height) / imgPreview.Picture.Height * 100);
  rx := TRegExpr.Create;
  rx.Expression := '(\D)(\d+%)';
  lblPreviewTitle.Caption := rx.Replace(lblPreviewTitle.Caption, '${1}'+IntToStr(Min(ZoomFactorH, ZoomFactorW))+'%', true);
  lblPreviewTitle.Hint := lblPreviewTitle.Caption;
  rx.Free;
end;


procedure TMainForm.actDataSaveBlobToFileExecute(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
  RowNum: PCardinal;
  Content: AnsiString;
  FileStream: TFileStream;
  StrLen: Integer;
  Dialog: TSaveDialog;
begin
  // Save BLOB to local file
  Grid := ActiveGrid;
  Results := GridResult(Grid);
  Dialog := TSaveDialog.Create(Self);
  Dialog.Filter := 'All files (*.*)|*.*';
  Dialog.FileName := Results.ColumnOrgNames[Grid.FocusedColumn];
  if not (Results.DataType(Grid.FocusedColumn).Category in [dtcBinary, dtcSpatial]) then
    Dialog.FileName := Dialog.FileName + '.txt';
  if Dialog.Execute then begin
    Screen.Cursor := crHourGlass;
    AnyGridEnsureFullRow(Grid, Grid.FocusedNode);
    RowNum := Grid.GetNodeData(Grid.FocusedNode);
    Results.RecNo := RowNum^;
    if Results.DataType(Grid.FocusedColumn).Category in [dtcBinary, dtcSpatial] then
      Content := AnsiString(Results.Col(Grid.FocusedColumn))
    else
      Content := Utf8Encode(Results.Col(Grid.FocusedColumn));
    StrLen := Length(Content);
    try
      FileStream := TFileStream.Create(Dialog.FileName, fmCreate or fmOpenWrite);
      FileStream.Write(Content[1], StrLen);
    except on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
    FreeAndNil(FileStream);
    Screen.Cursor := crDefault;
  end;
  Dialog.Free;
end;


procedure TMainForm.actInsertFilesExecute(Sender: TObject);
begin
  if not Assigned(InsertFiles) then
    InsertFiles := TfrmInsertFiles.Create(Self);
  InsertFiles.ShowModal;
end;

// Drop Table(s)
procedure TMainForm.actDropObjectsExecute(Sender: TObject);
var
  msg, db: String;
  InDBTree: Boolean;
  Act: TAction;
  Node: PVirtualNode;
  Obj: PDBObject;
  DBObject: TDBObject;
  ObjectList: TDBObjectList;
  Editor: TDBObjectEditor;
  Conn: TMySQLConnection;
begin
  Conn := ActiveConnection;

  ObjectList := TDBobjectList.Create(TDBObjectDropComparer.Create, False);

  Act := Sender as TAction;
  InDBTree := (Act.ActionComponent is TMenuItem)
    and (TPopupMenu((Act.ActionComponent as TMenuItem).GetParentMenu).PopupComponent = DBTree);
  if InDBTree then begin
    // drop table selected in tree view.
    case ActiveDBObj.NodeType of
      lntDb: begin
        if MessageDlg('Drop Database "'+Conn.Database+'"?' + crlf + crlf + 'WARNING: You will lose all objects in database '+Conn.Database+'!', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
          Abort;
        try
          db := Conn.Database;
          Node := FindDBNode(DBtree, db);
          SetActiveDatabase('', Conn);
          Conn.Query('DROP DATABASE ' + QuoteIdent(db));
          DBtree.DeleteNode(Node);
          Conn.ClearDbObjects(db);
          Conn.RefreshAllDatabases;
          InvalidateVT(ListDatabases, VTREE_NOTLOADED_PURGECACHE, False);
        except
          on E:EDatabaseError do
            MessageDlg(E.Message, mtError, [mbOK], 0);
        end;
        Exit;
      end;
      lntTable..lntEvent: ObjectList.Add(ActiveDbObj);
    end;
  end else begin
    // Invoked from database tab
    Node := GetNextNode(ListTables, nil, True);
    while Assigned(Node) do begin
      Obj := ListTables.GetNodeData(Node);
      ObjectList.Add(Obj^);
      Node := GetNextNode(ListTables, Node, True);
    end;
  end;

  // Fix actions temporarily enabled for popup menu.
  ValidateControls(Sender);

  // Safety stop to avoid firing DROP TABLE without tablenames
  if ObjectList.Count = 0 then
    Exit;

  // Ask user for confirmation to drop selected objects
  ObjectList.Sort;
  msg := 'Drop ' + IntToStr(ObjectList.Count) + ' object(s) in database "'+Conn.Database+'"?' + CRLF + CRLF;
  for DBObject in ObjectList do
    msg := msg + DBObject.Name + ', ';
  Delete(msg, Length(msg)-1, 2);
  if MessageDlg(msg, mtConfirmation, [mbok,mbcancel], 0) = mrOk then begin
    try
      // Disable foreign key checks to avoid SQL errors
      if Conn.ServerVersionInt >= 40014 then
        Conn.Query('SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0');
      // Compose and run DROP [TABLE|VIEW|...] queries
      Editor := ActiveObjectEditor;
      for DBObject in ObjectList do begin
        Conn.Query('DROP '+UpperCase(DBObject.ObjType)+' '+QuoteIdent(DBObject.Name));
        if Assigned(Editor) and Editor.Modified and Editor.DBObject.IsSameAs(DBObject) then
          Editor.Modified := False;
      end;
      if Conn.ServerVersionInt >= 40014 then
        Conn.Query('SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS');
      // Refresh ListTables + dbtree so the dropped tables are gone:
      Conn.ClearDbObjects(ActiveDatabase);
      SetActiveDatabase(Conn.Database, Conn);
    except
      on E:EDatabaseError do
        MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
    ObjectList.Free;
  end;
end;


// Load SQL-file, make sure that SheetQuery is activated
procedure TMainForm.actLoadSQLExecute(Sender: TObject);
var
  i: Integer;
  Dialog: TOpenTextFileDialog;
  Encoding: TEncoding;
begin
  //  if IsWindowsVista then
  //    Dialog := TFileOpenDialog.Create(Self);
  Dialog := TOpenTextFileDialog.Create(Self);
  Dialog.Options := Dialog.Options + [ofAllowMultiSelect];
  Dialog.Filter := 'SQL-Scripts (*.sql)|*.sql|All files (*.*)|*.*';
  Dialog.DefaultExt := 'sql';
  Dialog.Encodings.Assign(FileEncodings);
  Dialog.EncodingIndex := 0;
  if Dialog.Execute then begin
    Encoding := GetEncodingByName(Dialog.Encodings[Dialog.EncodingIndex]);
    for i:=0 to Dialog.Files.Count-1 do begin
      if i > 0 then
        actNewQueryTabExecute(Sender);
      QueryLoad(Dialog.Files[i], True, Encoding);
    end;
  end;
  Dialog.Free;
end;



procedure TMainForm.SessionConnect(Sender: TObject);
var
  Session: String;
  Connection: TMySQLConnection;
  Params: TConnectionParameters;
  Node, SessionNode: PVirtualNode;
  DBObj: PDBObject;
  i: Integer;
begin
  // Click on quick-session menu item:
  Session := (Sender as TMenuItem).Caption;
  Node := nil;
  // Probably wanted session was clicked before: navigate to last node
  for i:=High(FTreeClickHistory) downto Low(FTreeClickHistory) do begin
    if FTreeClickHistory[i] <> nil then begin
      DBObj := DBtree.GetNodeData(FTreeClickHistory[i]);
      if DBObj.Connection.SessionName = Session then begin
        Node := FTreeClickHistory[i];
        break;
      end;
    end;
  end;
  if not Assigned(Node) then begin
    // Wanted session was not clicked yet but probably connected: navigate to root node
    SessionNode := DBtree.GetFirstChild(nil);
    while Assigned(SessionNode) do begin
      DBObj := DBtree.GetNodeData(SessionNode);
      if DBObj.Connection.SessionName = Session then begin
        Node := SessionNode;
      end;
      SessionNode := DBtree.GetNextSibling(SessionNode);
    end;
  end;
  // Finally we have a node if session is already connected
  if Assigned(Node) then
    SelectNode(DBtree, Node)
  else begin
    Params := LoadConnectionParams(Session);
    InitConnection(Params, Session, True, Connection);
  end;
end;


{**
  Receive connection parameters and create a connection tree node
  Paremeters are either sent by connection-form or by commandline.
}
function TMainform.InitConnection(Params: TConnectionParameters; Session: String;
  ActivateMe: Boolean; var Connection: TMySQLConnection): Boolean;
var
  i: Integer;
  SessionExists, RestoreLastActiveDatabase: Boolean;
  StartupScript, StartupSQL, LastActiveDatabase: String;
  StartupBatch: TSQLBatch;
  SessionNode, DBNode: PVirtualNode;
begin
  Connection := TMySQLConnection.Create(Self);
  Connection.OnLog := LogSQL;
  Connection.OnDBObjectsCleared := DBObjectsCleared;
  Connection.OnDatabaseChanged := DatabaseChanged;
  Connection.ObjectNamesInSelectedDB := SynSQLSyn1.TableNames;
  Connection.SessionName := Session;
  Connection.Parameters := Params;
  try
    Connection.Active := True;
  except
    on E:EDatabaseError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;

  // attempt to establish connection
  SessionExists := MainReg.KeyExists(REGPATH + REGKEY_SESSIONS + Session);
  if not Connection.Active then begin
    // attempt failed
    if SessionExists then begin
      // Save "refused" counter
      OpenRegistry(Session);
      MainReg.WriteInteger(REGNAME_REFUSEDCOUNT, GetRegValue(REGNAME_REFUSEDCOUNT, 0, Session)+1);
    end;
    Result := False;
    FreeAndNil(Connection);
  end else begin
    // We have a connection
    Result := True;
    FConnections.Add(Connection);

    if SessionExists then begin
      // Save "connected" counter
      OpenRegistry(Session);
      MainReg.WriteInteger(REGNAME_CONNECTCOUNT, GetRegValue(REGNAME_CONNECTCOUNT, 0, Session)+1);
      // Save server version
      Mainreg.WriteInteger(REGNAME_SERVERVERSION, Connection.ServerVersionInt);
      Mainreg.WriteString(REGNAME_LASTCONNECT, DateTimeToStr(Now));
    end;

    if ActivateMe then begin
      // Set focus on last uses db. If not wanted or db is gone, go to root node at least
      RestoreLastActiveDatabase := GetRegValue(REGNAME_RESTORELASTUSEDDB, DEFAULT_RESTORELASTUSEDDB);
      LastActiveDatabase := GetRegValue(REGNAME_LASTUSEDDB, '', Session);
      if RestoreLastActiveDatabase and (Connection.AllDatabases.IndexOf(LastActiveDatabase) >- 1) then begin
        SetActiveDatabase(LastActiveDatabase, Connection);
        DBNode := FindDBNode(DBtree, LastActiveDatabase);
        if Assigned(DBNode) then
          DBtree.Expanded[DBNode] := True;
      end else begin
        SessionNode := GetRootNode(DBtree, Connection);
        SelectNode(DBtree, SessionNode);
        DBtree.Expanded[SessionNode] := True;
      end;
    end;

    // Process startup script
    StartupScript := Trim(Connection.Parameters.StartupScriptFilename);
    if StartupScript <> '' then begin
      if not FileExists(StartupScript) then
        MessageDlg('Error: Startup script file not found: '+StartupScript, mtError, [mbOK], 0)
      else begin
        StartupSQL := ReadTextfile(StartupScript, nil);
        StartupBatch := SplitSQL(StartupSQL);
        for i:=0 to StartupBatch.Count-1 do try
          Connection.Query(StartupBatch[i].SQL);
        except
          // Suppress popup, errors get logged into SQL log
        end;
        StartupBatch.Free;
      end;
    end;

  end;
  ShowStatusMsg;
end;


procedure TMainForm.actDataDeleteExecute(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Node, FocusAfterDelete: PVirtualNode;
  RowNum: PCardinal;
  Results: TMySQLQuery;
  Nodes: TNodeArray;
  i: Integer;
begin
  // Delete row(s)
  Grid := ActiveGrid;
  Results := GridResult(Grid);
  if Grid.SelectedCount = 0 then
    MessageDLG('Please select one or more rows to delete them.', mtError, [mbOK], 0)
  else try
    Results.CheckEditable;
    if MessageDLG('Delete '+IntToStr(Grid.SelectedCount)+' row(s)?',
      mtConfirmation, [mbOK, mbCancel], 0) = mrOK then begin
      FocusAfterDelete := nil;
      EnableProgressBar(Grid.SelectedCount);
      Node := GetNextNode(Grid, nil, True);
      while Assigned(Node) do begin
        RowNum := Grid.GetNodeData(Node);
        Results.RecNo := RowNum^;
        if Results.DeleteRow then begin
          ProgressBarStatus.StepIt;
          ProgressBarStatus.Repaint;
          SetLength(Nodes, Length(Nodes)+1);
          Nodes[Length(Nodes)-1] := Node;
          FocusAfterDelete := Node;
        end;
        Node := GetNextNode(Grid, Node, True);
      end;
      if Assigned(FocusAfterDelete) then
        FocusAfterDelete := Grid.GetNext(FocusAfterDelete);
      // Remove nodes and select some nearby node
      Grid.BeginUpdate;
      for i:=Low(Nodes) to High(Nodes) do
        Grid.DeleteNode(Nodes[i]);
      Grid.EndUpdate;
      if not Assigned(FocusAfterDelete) then
        FocusAfterDelete := Grid.GetLast;
      if Assigned(FocusAfterDelete) then
        SelectNode(Grid, FocusAfterDelete);
      DisplayRowCountStats(Grid);
      ValidateControls(Sender);
    end;
  except on E:EDatabaseError do begin
      ProgressBarStatus.State := pbsError;
      MessageDlg('Grid editing error: '+E.Message, mtError, [mbOK], 0);
    end;
  end;
  Mainform.ProgressBarStatus.Visible := False;
end;


procedure TMainForm.actUpdateCheckExecute(Sender: TObject);
var
  frm : TfrmUpdateCheck;
begin
  frm := TfrmUpdateCheck.Create(Self);
  frm.ShowModal;
  FreeAndNil(frm);
end;


procedure TMainForm.actCreateDBObjectExecute(Sender: TObject);
var
  Obj: TDBObject;
  a: TAction;
begin
  // Create a new table, view, etc.
  tabEditor.TabVisible := True;
  SetMainTab(tabEditor);
  a := Sender as TAction;
  Obj := TDBObject.Create(ActiveConnection);
  Obj.Database := ActiveDatabase;
  if a = actCreateTable then Obj.NodeType := lntTable
  else if a = actCreateView then Obj.NodeType := lntView
  else if a = actCreateRoutine then Obj.NodeType := lntProcedure
  else if a = actCreateTrigger then Obj.NodeType := lntTrigger
  else if a = actCreateEvent then Obj.NodeType := lntEvent;
  PlaceObjectEditor(Obj);
end;


procedure TMainForm.actEmptyTablesExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Obj: PDBObject;
  TableOrView: TDBObject;
  Objects: TDBObjectList;
  Names: String;
begin
  // Add selected items/tables to helper list
  Objects := TDBObjectList.Create(False);
  if ListTables.Focused then begin
    Node := GetNextNode(ListTables, nil, True);
    while Assigned(Node) do begin
      Obj := ListTables.GetNodeData(Node);
      if Obj.NodeType in [lntTable, lntView] then begin
        Objects.Add(Obj^);
        Names := Names + Obj.Name + ', ';
      end;
      Node := GetNextNode(ListTables, Node, True);
    end;
    Delete(Names, Length(Names)-1, 2);
  end else if DBTree.Focused then begin
    Objects.Add(ActiveDbObj);
    Names := ActiveDbObj.Name;
  end;
  if Objects.Count = 0 then
    MessageDlg('No table(s) selected.', mtError, [mbOK], 0)
  else begin
    if MessageDlg('Empty ' + IntToStr(Objects.count) + ' table(s) and/or view(s) ?' + CRLF + '('+Names+')',
      mtConfirmation, [mbOk, mbCancel], 0) = mrOk then begin
      Screen.Cursor := crHourglass;
      EnableProgressBar(Objects.Count);
      try
        for TableOrView in Objects do begin
          ActiveConnection.Query('TRUNCATE ' + QuoteIdent(TableOrView.Name));
          ProgressBarStatus.StepIt;
        end;
        actRefresh.Execute;
      except
        on E:EDatabaseError do begin
          ProgressBarStatus.State := pbsError;
          MessageDlg(E.Message, mtError, [mbOK], 0);
        end;
      end;
      Objects.Free;
      ProgressBarStatus.Hide;
      Screen.Cursor := crDefault;
    end;
  end;
end;


procedure TMainForm.actRunRoutinesExecute(Sender: TObject);
var
  Tab: TQueryTab;
  Query, ParamInput,
  DummyStr: String;
  DummyBool: Boolean;
  i: Integer;
  pObj: PDBObject;
  Obj: TDBObject;
  Objects: TDBObjectList;
  Node: PVirtualNode;
  Parameters: TRoutineParamList;
begin
  // Run stored function(s) or procedure(s)
  Objects := TDBObjectList.Create(False);
  if ListTables.Focused then begin
    Node := GetNextNode(ListTables, nil, True);
    while Assigned(Node) do begin
      pObj := ListTables.GetNodeData(Node);
      if pObj.NodeType in [lntProcedure, lntFunction] then
        Objects.Add(pObj^);
      Node := GetNextNode(ListTables, Node, True);
    end;
  end else
    Objects.Add(ActiveDbObj);

  if Objects.Count = 0 then
    MessageDlg('Please select one or more stored function(s) or routine(s).', mtError, [mbOK], 0);

  for Obj in Objects do begin
    actNewQueryTab.Execute;
    Tab := QueryTabs[MainForm.QueryTabs.Count-1];
    case Obj.NodeType of
      lntProcedure: Query := 'CALL ';
      lntFunction: Query := 'SELECT ';
    end;
    Parameters := TRoutineParamList.Create;
    Obj.Connection.ParseRoutineStructure(Obj.CreateCode, Parameters, DummyBool, DummyStr, DummyStr, DummyStr, DummyStr, DummyStr, DummyStr);
    Query := Query + QuoteIdent(Obj.Name);
    ParamInput := '';
    for i:=0 to Parameters.Count-1 do begin
      if ParamInput <> '' then
        ParamInput := ParamInput + ', ';
      ParamInput := ParamInput + '''' + InputBox(Obj.Name, 'Parameter #'+IntToStr(i+1)+': '+Parameters[i].Name+' ('+Parameters[i].Datatype+')', '') + '''';
    end;
    Parameters.Free;
    Query := Query + '('+ParamInput+')';
    Tab.Memo.Text := Query;
    actExecuteQueryExecute(Sender);
  end;
end;


procedure TMainForm.actNewWindowExecute(Sender: TObject);
begin
  ShellExec( ExtractFileName(paramstr(0)), ExtractFilePath(paramstr(0)) );
end;


procedure TMainForm.actQueryFindReplaceExecute(Sender: TObject);
var
  DlgResult: TModalResult;
  Memo: TSynMemo;
begin
  // Display search + replace dialog
  Memo := ActiveSynMemo;
  if Memo = nil then
    MessageBeep(MB_ICONASTERISK)
  else begin
    if not Assigned(SearchReplaceDialog) then
      SearchReplaceDialog := TfrmSearchReplace.Create(Self);
    SearchReplaceDialog.Editor := Memo;
    SearchReplaceDialog.chkReplace.Checked := Sender = actQueryReplace;
    DlgResult := SearchReplaceDialog.ShowModal;
    case DlgResult of
      mrOK, mrAll: begin
        DoSearchReplace;
        FSearchReplaceExecuted := True; // Helper for later F3 hits
      end;
      mrCancel: Exit;
    end;
  end;
end;


procedure TMainForm.actQueryFindAgainExecute(Sender: TObject);
begin
  // F3 - search or replace again, using previous settings
  if not FSearchReplaceExecuted then
    actQueryFindReplaceExecute(Sender)
  else begin
    SearchReplaceDialog.Editor := ActiveSynMemo;
    if SearchReplaceDialog.Editor = nil then
      MessageBeep(MB_ICONASTERISK)
    else
      DoSearchReplace;
  end;
end;


procedure TMainForm.DoSearchReplace;
var
  Occurences: Integer;
  OldCaretXY: TBufferCoord;
begin
  if SearchReplaceDialog.chkRegularExpression.Checked then
    SearchReplaceDialog.Editor.SearchEngine := SynEditRegexSearch1
  else
    SearchReplaceDialog.Editor.SearchEngine := SynEditSearch1;
  OldCaretXY := SearchReplaceDialog.Editor.CaretXY;
  SearchReplaceDialog.Editor.BeginUpdate;
  ShowStatusMsg('Searching ...');
  Occurences := SearchReplaceDialog.Editor.SearchReplace(
    SearchReplaceDialog.comboSearch.Text,
    SearchReplaceDialog.comboReplace.Text,
    SearchReplaceDialog.Options
    );
  SearchReplaceDialog.Editor.EndUpdate;
  ShowStatusMsg;
  if ssoReplaceAll in SearchReplaceDialog.Options then
    ShowStatusMsg('Text "'+SearchReplaceDialog.comboSearch.Text+'" '+FormatNumber(Occurences)+' times replaced.', 0)
  else begin
    if (OldCaretXY.Char = SearchReplaceDialog.Editor.CaretXY.Char) and
      (OldCaretXY.Line = SearchReplaceDialog.Editor.CaretXY.Line) then
      MessageDlg('Text "'+SearchReplaceDialog.comboSearch.Text+'" not found.', mtInformation, [mbOk], 0);
  end;
end;


procedure TMainForm.SynMemoQueryReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
begin
  // Fires when "Replace all" in search dialog was pressed with activated "Prompt on replace"
  case MessageDlg('Replace this occurrence of "'+sstr(ASearch, 100)+'"?', mtConfirmation, [mbYes, mbYesToAll, mbNo, mbCancel], 0) of
    mrYes: Action := raReplace;
    mrYesToAll: Action := raReplaceAll;
    mrNo: Action := raSkip;
    mrCancel: Action := raCancel;
  end;
end;


procedure TMainForm.actRefreshExecute(Sender: TObject);
var
  tab1, tab2: TTabSheet;
  List: TVirtualStringTree;
begin
  // Refresh
  // Force data tab update when appropriate.
  tab1 := PageControlMain.ActivePage;
  if ActiveControl = DBtree then
    RefreshTree
  else if tab1 = tabHost then begin
    tab2 := PageControlHost.ActivePage;
    if tab2 = tabDatabases then
      List := ListDatabases
    else if tab2 = tabVariables then
      List := ListVariables
    else if tab2 = tabStatus then
      List := ListStatus
    else if tab2 = tabProcessList then
      List := ListProcesses
    else
      List := ListCommandStats;
    InvalidateVT(List, VTREE_NOTLOADED_PURGECACHE, True);
  end else if tab1 = tabDatabase then
    InvalidateVT(ListTables, VTREE_NOTLOADED_PURGECACHE, False)
  else if tab1 = tabData then
    InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
end;


procedure TMainForm.actSQLhelpExecute(Sender: TObject);
var
  keyword: String;
  Tree: TVirtualStringTree;
begin
  // Call SQL Help from various places
  if ActiveConnection.ServerVersionInt < 40100 then
    exit;

  keyword := '';

  // Query-Tab
  if ActiveControl is TSynMemo then
    keyword := TSynMemo(ActiveControl).WordAtCursor

  // Data-Tab
  else if (PageControlMain.ActivePage = tabData)
    and Assigned(DataGrid.FocusedNode) then begin
    keyword := SelectedTableColumns[DataGrid.FocusedColumn].DataType.Name;

  end else if ActiveControl = ActiveQueryHelpers then begin
    // Makes only sense if one of the nodes "SQL fn" or "SQL kw" was selected
    Tree := ActiveQueryHelpers;
    if Assigned(Tree.FocusedNode)
      and (Tree.GetNodeLevel(Tree.FocusedNode)=1)
      and (Tree.FocusedNode.Parent.Index in [HELPERNODE_FUNCTIONS, HELPERNODE_KEYWORDS]) then
      keyword := Tree.Text[Tree.FocusedNode, 0];
  end;

  // Clean existing paranthesis, fx: char(64)
  if Pos( '(', keyword ) > 0 then
    keyword := Copy( keyword, 1, Pos( '(', keyword )-1 );

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
  SQLHelpForm.Show;
  SQLHelpForm.Keyword := keyword;
end;


procedure TMainForm.actSaveSQLAsExecute(Sender: TObject);
var
  i: Integer;
  CanSave: TModalResult;
begin
  // Save SQL
  CanSave := mrNo;
  while (CanSave = mrNo) and SaveDialogSQLFile.Execute do begin
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    CanSave := mrYes;
    for i:=0 to QueryTabs.Count-1 do begin
      if QueryTabs[i].MemoFilename = SaveDialogSQLFile.FileName then begin
        CanSave := MessageDlg('File '+CRLF+'"'+SaveDialogSQLFile.FileName+'"'+CRLF+'is already open in query tab #'+IntToStr(QueryTabs[i].Number)+'. Overwrite it?',
          mtWarning, [mbYes, mbNo, mbCancel], 0);
        break;
      end;
    end;
  end;
  if CanSave = mrYes then begin
    ActiveQueryTab.SaveQueryMemo(SaveDialogSQLFile.FileName, (Sender as TAction).Tag = 1);
    for i:=0 to QueryTabs.Count-1 do begin
      if QueryTabs[i] = ActiveQueryTab then
        continue;
      if QueryTabs[i].MemoFilename = SaveDialogSQLFile.FileName then
        QueryTabs[i].Memo.Modified := True;
    end;
    ValidateQueryControls(Sender);
  end;
end;


procedure TMainForm.actSaveSQLExecute(Sender: TObject);
var
  i: Integer;
begin
  if ActiveQueryTab.MemoFilename <> '' then begin
    ActiveQueryTab.SaveQueryMemo(ActiveQueryTab.MemoFilename, False);
    for i:=0 to QueryTabs.Count-1 do begin
      if QueryTabs[i] = ActiveQueryTab then
        continue;
      if QueryTabs[i].MemoFilename = ActiveQueryTab.MemoFilename then
        QueryTabs[i].Memo.Modified := True;
    end;
    ValidateQueryControls(Sender);
  end else
    actSaveSQLAsExecute(Sender);
end;


procedure TMainForm.actSaveSQLSnippetExecute(Sender: TObject);
var
  snippetname : String;
  Text, LB: String;
begin
  // Save snippet
  if InputQuery( 'Save snippet', 'Snippet name:', snippetname) then
  begin
    if Copy( snippetname, Length(snippetname)-4, 4 ) <> '.sql' then
      snippetname := snippetname + '.sql';
    // cleanup snippetname from special characters
    snippetname := DirnameSnippets + goodfilename(snippetname);
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
    case ActiveQueryTab.MemoLineBreaks of
      lbsUnix: LB := LB_UNIX;
      lbsMac: LB := LB_MAC;
      lbsWide: LB := LB_WIDE;
    end;
    if LB <> '' then
      Text := StringReplace(Text, CRLF, LB, [rfReplaceAll]);
    if not DirectoryExists(DirnameSnippets) then
      ForceDirectories(DirnameSnippets);
    SaveUnicodeFile( snippetname, Text );
    FillPopupQueryLoad;
    SnippetFilenames := GetFilesFromDir(DirnameSnippets, '*.sql', True);
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actQueryStopOnErrorsExecute(Sender: TObject);
begin
  // Weird fix: dummy routine to avoid the sending action getting disabled
end;


procedure TMainForm.actQueryWordWrapExecute(Sender: TObject);
begin
  // SetupSynEditors applies all customizations to any SynEditor
  SetupSynEditors;
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
  snippets := getFilesFromDir( DirnameSnippets, '*.sql', true );
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

  menuitem := TMenuItem.Create( popupQueryLoad );
  menuitem.Caption := 'Clear file list';
  menuitem.OnClick := PopupQueryLoadRemoveAllFiles;
  popupQueryLoad.Items.Add(menuitem);

end;


procedure TMainform.PopupQueryLoadRemoveAbsentFiles(Sender: TObject);
begin
  AddOrRemoveFromQueryLoadHistory('', False, True);
  FillPopupQueryLoad;
end;


procedure TMainform.PopupQueryLoadRemoveAllFiles(Sender: TObject);
var
  Values: TStringList;
  i: Integer;
begin
  Values := TStringList.Create;
  OpenRegistry;
  MainReg.GetValueNames(Values);
  for i:=0 to Values.Count-1 do begin
    if Pos('SQLFile', Values[i]) = 1 then
      MainReg.DeleteValue(Values[i]);
  end;
  FillPopupQueryLoad;
end;


procedure TMainform.popupQueryLoadClick(Sender: TObject);
var
  Filename: String;
  p: Integer;
begin
  // Click on the popupQueryLoad
  Filename := (Sender as TMenuItem).Caption;
  Filename := StripHotkey(Filename);
  if Pos('\', Filename) = 0 then // assuming we load a snippet
    Filename := DirnameSnippets + Filename + '.sql'
  else begin // assuming we load a file from the recent-list
    p := Pos(' ', Filename) + 1;
    filename := Copy(Filename, p, Length(Filename));
  end;
  QueryLoad(Filename, True, nil);
end;


procedure TMainform.AddOrRemoveFromQueryLoadHistory(Filename: String; AddIt: Boolean; CheckIfFileExists: Boolean);
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
  Sets the Delimiter property plus updates the hint on actSetDelimiter
}
procedure TMainForm.SetDelimiter(Value: String);
var
  rx: TRegExpr;
  Msg: String;
begin
  Value := Trim(Value);
  Msg := '';
  if Value = '' then
    Msg := 'Empty value.'
  else begin
    rx := TRegExpr.Create;
    rx.Expression := '(/\*|--|#|\''|\"|`)';
    if rx.Exec(Value) then
      Msg := 'Start-of-comment tokens or string literal markers are not allowed.'
  end;
  if Msg <> '' then begin
    Msg := 'Error setting delimiter to "'+Value+'": '+Msg;
    LogSQL(Msg, lcError);
    MessageDlg(Msg, mtError, [mbOK], 0);
  end else begin
    FDelimiter := Value;
    LogSQL('Delimiter changed to '+FDelimiter, lcInfo);
    actSetDelimiter.Hint := actSetDelimiter.Caption + ' (current value: '+Delimiter+')';
  end;
end;


procedure TMainForm.actApplyFilterExecute(Sender: TObject);
var
  i, nr: Integer;
  OldNumbers, Filters: TStringList;
  val: String;
begin
  // If filter box is empty but filter generator box not, most users expect
  // the filter to be auto generated on button click
  if (SynMemoFilter.GetTextLen = 0) and (editFilterSearch.Text <> '') then
    editFilterSearchChange(editFilterSearch);

  if SynMemoFilter.GetTextLen > 0 then begin
    // Recreate recent filters list
    Filters := TStringList.Create;
    OldNumbers := TStringList.Create;
    Filters.Add(Trim(SynMemoFilter.Text));
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
  // Keep current column widths on "Quick filter" clicks, don't keep them on "Apply filter" clicks
  if (Sender is TMenuItem) and ((Sender as TMenuItem).GetParentMenu = popupDataGrid) then begin
    FDataGridColumnWidthsCustomized := True;
  end else
    FDataGridColumnWidthsCustomized := False;
  InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
end;


procedure TMainForm.actDataFirstExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := GetNextNode(ActiveGrid, nil);
  if Assigned(Node) then
    SelectNode(ActiveGrid, Node);
end;


procedure TMainForm.actDataInsertExecute(Sender: TObject);
var
  DupeNode, NewNode: PVirtualNode;
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
  RowNum: Cardinal;
  DupeNum: PCardinal;
  i: Integer;
  Value: String;
  IsNull, AllowNewNode: Boolean;
begin
  Grid := ActiveGrid;
  Results := GridResult(Grid);
  // Pre-test if changing node focus is allowed, in cases where current row modifications throw some SQL error when posting
  AllowNewNode := False;
  Grid.OnFocusChanging(Grid, Grid.FocusedNode, nil, Grid.FocusedColumn, Grid.FocusedColumn, AllowNewNode);
  if not AllowNewNode then
    exit;
  try
    Results.CheckEditable;
    DupeNode := nil;
    if Sender = actDataDuplicateRow then
      DupeNode := Grid.FocusedNode;
    RowNum := Results.InsertRow;
    NewNode := Grid.InsertNode(Grid.FocusedNode, amInsertAfter, PCardinal(RowNum));
    SelectNode(Grid, NewNode);
    if Assigned(DupeNode) then begin
      // Copy values from source row, ensure we have whole cell data
      DupeNum := Grid.GetNodeData(DupeNode);
      AnyGridEnsureFullRow(Grid, DupeNode);
      for i:=0 to Grid.Header.Columns.Count-1 do begin
        if not (coVisible in Grid.Header.Columns[i].Options) then
          continue; // Ignore invisible key column
        if Results.ColIsPrimaryKeyPart(i) then
          continue; // Empty value for primary key column
        Results.RecNo := DupeNum^;
        Value := Results.Col(i);
        IsNull := Results.IsNull(i);
        Results.RecNo := RowNum;
        Results.SetCol(i, Value, IsNull);
      end;
    end;
  except on E:EDatabaseError do
    MessageDlg('Grid editing error: '+E.Message, mtError, [mbOk], 0);
  end;
end;


procedure TMainForm.actDataLastExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Grid: TVirtualStringTree;
begin
  Grid := ActiveGrid;
  // Be sure to have all rows
  if (Grid = DataGrid) and (DatagridWantedRowCount < prefGridRowcountMax) then
    actDataShowAll.Execute;
  Node := Grid.GetLast;
  if Assigned(Node) then
    SelectNode(Grid, Node);
end;

procedure TMainForm.actDataPostChangesExecute(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
begin
  if Sender is TVirtualStringTree then
    Grid := Sender as TVirtualStringTree
  else
    Grid := ActiveGrid;
  Results := GridResult(Grid);
  Results.SaveModifications;
  // Node needs a repaint to remove red triangles
  if Assigned(Grid.FocusedNode) then
    Grid.InvalidateNode(Grid.FocusedNode);
  DisplayRowCountStats(Grid);
end;

procedure TMainForm.actRemoveFilterExecute(Sender: TObject);
begin
  actClearFilterEditor.Execute;
  InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
end;


procedure TMainForm.actDataCancelChangesExecute(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
  RowNum: PCardinal;
  Node, FocNode: PVirtualNode;
begin
  // Cancel INSERT or UPDATE mode
  Grid := ActiveGrid;
  Node := Grid.FocusedNode;
  if Assigned(Node) then begin
    Results := GridResult(Grid);
    RowNum := Grid.GetNodeData(Node);
    Results.RecNo := RowNum^;
    Results.DiscardModifications;
    if Results.Inserted then begin
      FocNode := Grid.GetPreviousSibling(Node);
      Grid.DeleteNode(Node);
      SelectNode(Grid, FocNode);
    end else
      Grid.InvalidateNode(Node);
    ValidateControls(Sender);
  end;
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
    OpenRegistry(ActiveConnection.SessionName);
    MainReg.WriteInteger(REGNAME_TREEBACKGROUND, cs.Dialog.Color);
  end;
end;


{**
  Add a SQL-command or comment to SynMemoSQLLog
}
procedure TMainForm.LogSQL(Msg: String; Category: TMySQLLogCategory=lcInfo; Connection: TMySQLConnection=nil);
var
  snip, IsSQL: Boolean;
  Len: Integer;
  Sess: String;
begin
  if csDestroying in ComponentState then
    Exit;

  // Log only wanted events
  case Category of
    lcError: if not prefLogErrors then Exit;
    lcUserFiredSQL: if not prefLogUserSQL then Exit;
    lcSQL: if not prefLogSQL then Exit;
    lcInfo: if not prefLogInfos then Exit;
    lcDebug: if not prefLogDebug then Exit;
  end;

  // Shorten very long messages
  Len := Length(Msg);
  snip := (prefLogSqlWidth > 0) and (Len > prefLogSqlWidth);
  IsSQL := Category in [lcSQL, lcUserFiredSQL];
  if snip then begin
    Msg :=
      Copy(Msg, 0, prefLogSqlWidth) +
      '/* large SQL query ('+FormatByteNumber(Len)+'), snipped at ' +
      FormatNumber(prefLogSqlWidth) +
      ' characters */';
  end else if (not snip) and IsSQL then
    Msg := Msg + Delimiter;
  if not IsSQL then
    Msg := '/* ' + Msg + ' */';

  Msg := StringReplace(Msg, #9, ' ', [rfReplaceAll]);
  Msg := StringReplace(Msg, #10, ' ', [rfReplaceAll]);
  Msg := StringReplace(Msg, #13, ' ', [rfReplaceAll]);
  Msg := StringReplace(Msg, '  ', ' ', [rfReplaceAll]);
  SynMemoSQLLog.Lines.Add(Msg);
  TrimSQLLog;

  // Scroll to last line and repaint
  SynMemoSQLLog.GotoLineAndCenter(SynMemoSQLLog.Lines.Count);
  SynMemoSQLLog.Repaint;

  // Log to file?
  if prefLogToFile then
  try
    Sess := '';
    if Assigned(Connection) then
      Sess := Connection.SessionName;
    WriteLn(FileHandleSessionLog, Format('/* %s [%s] */ %s', [DateTimeToStr(Now), Sess, msg]));
  except
    on E:Exception do begin
      DeactivateFileLogging;
      MessageDlg('Error writing to session log file:'+CRLF+FileNameSessionLog+CRLF+CRLF+E.Message+CRLF+CRLF+'Logging is disabled now.', mtError, [mbOK], 0);
    end;
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


procedure TMainForm.actDataShowNextExecute(Sender: TObject);
var
  OldRowCount: Int64;
begin
  // Show next X rows in datagrid
  OldRowCount := DatagridWantedRowCount;
  Inc(DatagridWantedRowCount, prefGridRowcountStep);
  DataGridWantedRowCount := Min(DataGridWantedRowCount, prefGridRowcountMax);
  InvalidateVT(DataGrid, VTREE_NOTLOADED, True);
  SelectNode(DataGrid, OldRowCount);
end;


procedure TMainForm.actDataShowAllExecute(Sender: TObject);
begin
  // Remove LIMIT clause
  DatagridWantedRowCount := prefGridRowcountMax;
  InvalidateVT(DataGrid, VTREE_NOTLOADED, True);
end;


function TMainForm.AnyGridEnsureFullRow(Grid: TVirtualStringTree; Node: PVirtualNode): Boolean;
var
  RowNum: PCardinal;
  Data: TMySQLQuery;
begin
  // Load remaining data on a partially loaded row in data grid
  Result := True;
  if (Grid = DataGrid) and Assigned(Node) then begin
    RowNum := Grid.GetNodeData(Node);
    Data := GridResult(Grid);
    Data.RecNo := RowNum^;
    Result := Data.EnsureFullRow;
  end;
end;


procedure TMainForm.DataGridEnsureFullRows(Grid: TVirtualStringTree; SelectedOnly: Boolean);
var
  Node: PVirtualNode;
  Results: TMySQLQuery;
  RowNum: PCardinal;
begin
  // Load remaining data of all grid rows
  Results := GridResult(Grid);
  Node := GetNextNode(Grid, nil, SelectedOnly);
  while Assigned(Node) do begin
    RowNum := Grid.GetNodeData(Node);
    Results.RecNo := RowNum^;
    if not Results.HasFullData then begin
      DataGridFullRowMode := True;
      InvalidateVT(Grid, VTREE_NOTLOADED_PURGECACHE, True);
      break;
    end;
    Node := GetNextNode(Grid, Node, SelectedOnly);
  end;
end;


procedure TMainForm.DataGridBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  vt: TVirtualStringTree;
  Select: String;
  RefreshingData, IsKeyColumn: Boolean;
  i, Offset, ColLen, ColWidth: Integer;
  KeyCols, ColWidths, WantedColumnOrgnames: TStringList;
  WantedColumns: TTableColumnList;
  c: TTableColumn;
  OldScrollOffset: TPoint;

  procedure InitColumn(idx: Integer; TblCol: TTableColumn);
  var
    k: Integer;
    Col: TVirtualTreeColumn;
  begin
    col := vt.Header.Columns.Add;
    col.Text := TblCol.Name;
    col.Hint := TblCol.Comment;
    col.Options := col.Options + [coSmartResize];
    if DatagridHiddenColumns.IndexOf(TblCol.Name) > -1 then
      col.Options := col.Options - [coVisible];
    // Sorting color and title image
    for k:=0 to Length(DataGridSortColumns)-1 do begin
      if DataGridSortColumns[k].ColumnName = TblCol.Name then begin
        col.Color := ColorAdjustBrightness(col.Color, COLORSHIFT_SORTCOLUMNS);
        case DataGridSortColumns[k].SortDirection of
          ORDER_ASC:  col.ImageIndex := 109;
          ORDER_DESC: col.ImageIndex := 110;
        end;
      end;
    end;
    if col.ImageIndex = -1 then begin
      for k:=0 to SelectedTableKeys.Count-1 do begin
        if SelectedTableKeys[k].Columns.IndexOf(TblCol.Name) > -1 then begin
          col.ImageIndex := GetIndexIcon(SelectedTableKeys[k].IndexType);
          break;
        end;
      end;
    end;

    // Data type
    col.Alignment := taLeftJustify;
    if DataGridResult.DataType(idx).Category in [dtcInteger, dtcReal] then
      col.Alignment := taRightJustify;
  end;

begin
  // Load data into data tab grid
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  Screen.Cursor := crHourglass;

  // No data for routines
  if SelectedTableColumns.Count = 0 then begin
    vt.Enabled := False;
    pnlDataTop.Enabled := False;
    pnlFilter.Enabled := False;
    lblSorryNoData.Parent := DataGrid;
  end else begin
    vt.Enabled := True;
    pnlDataTop.Enabled := True;
    pnlFilter.Enabled := True;
    lblSorryNoData.Parent := tabData;

    // Indicates whether the current table data is just refreshed or if we're in another table
    RefreshingData := (ActiveDatabase = DataGridDB) and (ActiveDbObj.Name = DataGridTable);

    // Load last view settings
    HandleDataGridAttributes(RefreshingData);
    OldScrollOffset := DataGrid.OffsetXY;

    DataGridDB := ActiveDbObj.Database;
    DataGridTable := ActiveDbObj.Name;

    Select := 'SELECT ';
    // Ensure key columns are included to enable editing
    KeyCols := ActiveConnection.GetKeyColumns(SelectedTableColumns, SelectedTableKeys);
    WantedColumns := TTableColumnList.Create(False);
    WantedColumnOrgnames := TStringList.Create;
    for i:=0 to SelectedTableColumns.Count-1 do begin
      c := SelectedTableColumns[i];
      IsKeyColumn := KeyCols.IndexOf(c.Name) > -1;
      ColLen := StrToInt64Def(c.LengthSet, 0);
      if (DatagridHiddenColumns.IndexOf(c.Name) = -1)
        or (IsKeyColumn)
        or (KeyCols.Count = 0)
        then begin
        if not DataGridFullRowMode
          and (KeyCols.Count > 0) // We need a sufficient key to be able to load remaining row data
          and (c.DataType.Category in [dtcText, dtcBinary])
          and (not IsKeyColumn) // We need full length of any key column, so DataGridLoadFullRow() has the chance to fetch the right row
          and ((ColLen > GRIDMAXDATA) or (ColLen = 0)) // No need to blow SQL with LEFT() if column is shorter anyway
          then
            Select := Select + ' LEFT(' + QuoteIdent(c.Name) + ', ' + IntToStr(GRIDMAXDATA) + '), '
          else
            Select := Select + ' ' + QuoteIdent(c.Name) + ', ';
        WantedColumns.Add(c);
        WantedColumnOrgnames.Add(c.Name);
      end;
    end;
    // Cut last comma
    Delete(Select, Length(Select)-1, 2);
    // Include db name for cases in which dbtree is switching databases and pending updates are in process
    Select := Select + ' FROM '+QuoteIdent(ActiveDatabase)+'.'+QuoteIdent(ActiveDbObj.Name);

    // Signal for the user if we hide some columns
    if WantedColumns.Count = SelectedTableColumns.Count then
      tbtnDataColumns.ImageIndex := 107
    else
      tbtnDataColumns.ImageIndex := 108;

    // Append WHERE clause
    if SynMemoFilter.GetTextLen > 0 then begin
      Select := Select + ' WHERE ' + SynMemoFilter.Text;
      tbtnDataFilter.ImageIndex := 108;
    end else
      tbtnDataFilter.ImageIndex := 107;

    // Append ORDER clause
    if Length(DataGridSortColumns) > 0 then begin
      Select := Select + ' ORDER BY ' + ComposeOrderClause(DataGridSortColumns);
      tbtnDataSorting.ImageIndex := 108;
    end else
      tbtnDataSorting.ImageIndex := 107;

    // Append LIMIT clause
    if RefreshingData and (vt.Tag <> VTREE_NOTLOADED_PURGECACHE) then
      Offset := DataGridResult.RecordCount
    else
      Offset := 0;
    Select := Select + ' LIMIT '+IntToStr(Offset)+', '+IntToStr(DatagridWantedRowCount-Offset);

    try
      ShowStatusMsg('Fetching rows ...');
      if not Assigned(DataGridResult) then
        DataGridResult := TMySQLQuery.Create(Self);
      DataGridResult.Connection := ActiveConnection;
      DataGridResult.SQL := Select;
      DataGridResult.Execute(Offset > 0);
      DataGridResult.ColumnOrgNames := WantedColumnOrgnames;
      try
        DataGridResult.PrepareEditing;
      except on E:EDatabaseError do // Do not annoy user with popup when accessing tables in information_schema
        LogSQL('Data in this table will be read-only.');
      end;

      editFilterVT.Clear;
      TimerFilterVT.OnTimer(Sender);

      // Assign new data
      vt.BeginUpdate;
      vt.Clear;
      vt.RootNodeCount := DataGridResult.RecordCount;

      // Set up grid column headers
      ShowStatusMsg('Setting up columns ...');
      ColWidths := TStringList.Create;
      if not RefreshingData then
        FDataGridColumnWidthsCustomized := False;
      if FDataGridColumnWidthsCustomized then begin
        for i:=0 to vt.Header.Columns.Count-1 do
          ColWidths.Values[vt.Header.Columns[i].Text] := IntToStr(vt.Header.Columns[i].Width);
      end;
      vt.Header.Columns.BeginUpdate;
      vt.Header.Columns.Clear;
      for i:=0 to WantedColumns.Count-1 do
        InitColumn(i, WantedColumns[i]);

      // Autoset or restore column width
      for i:=0 to vt.Header.Columns.Count-1 do begin
        ColWidth := 0;
        if RefreshingData then
          ColWidth := StrToIntDef(ColWidths.Values[vt.Header.Columns[i].Text], ColWidth);
        if ColWidth > 0 then
          vt.Header.Columns[i].Width := ColWidth
        else
          AutoCalcColWidth(vt, i);
      end;
      ColWidths.Free;

      vt.Header.Columns.EndUpdate;
      vt.EndUpdate;

      // Do not steel filter while writing filters
      if not SynMemoFilter.Focused then
        vt.SetFocus;

      DataGridFocusedNodeIndex := Min(DataGridFocusedNodeIndex, vt.RootNodeCount-1);
      SelectNode(vt, DataGridFocusedNodeIndex);
      for i:=0 to vt.Header.Columns.Count-1 do begin
        if vt.Header.Columns[i].Text = DataGridFocusedColumnName then begin
          vt.FocusedColumn := i;
          break;
        end;
      end;
      if RefreshingData then
        vt.OffsetXY := OldScrollOffset;

      ValidateControls(Sender);
      DisplayRowCountStats(vt);
      actDataShowNext.Enabled := (vt.RootNodeCount = DatagridWantedRowCount) and (DatagridWantedRowCount < prefGridRowcountMax);
      actDataShowAll.Enabled := actDataShowNext.Enabled;
      EnumerateRecentFilters;
      if Integer(vt.RootNodeCount) = prefGridRowcountMax then
        LogSQL('Browsing is currently limited to a maximum of '+FormatNumber(prefGridRowcountMax)+' rows. To see more rows, increase this maximum in Tools > Preferences > Data .', lcInfo);

    except
      // Wrong WHERE clause in most cases
      on E:EDatabaseError do
        MessageDlg(E.Message, mtError, [mbOK], 0);
    end;

  end;
  vt.Tag := VTREE_LOADED;
  DataGridFullRowMode := False;
  Screen.Cursor := crDefault;
  ShowStatusMsg;
end;


procedure TMainForm.DataGridColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  // Remember current table after last column resizing so we can auto size them as long as this did not happen
  FDataGridColumnWidthsCustomized := True;
end;


{***
  Calculate + display total rowcount and found rows matching to filter
  in data-tab
}
procedure TMainForm.DisplayRowCountStats(Sender: TBaseVirtualTree);
var
  DBObject: TDBObject;
  IsFiltered, IsLimited: Boolean;
  cap: String;
  RowsTotal: Int64;
begin
  if Sender <> DataGrid then
    Exit; // Only data tab has a top label

  DBObject := ActiveDbObj;
  cap := ActiveDatabase + '.' + DBObject.Name;
  IsLimited := DataGridWantedRowCount <= Datagrid.RootNodeCount;
  IsFiltered := SynMemoFilter.GetTextLen > 0;
  if DBObject.NodeType = lntTable then begin
    if (not IsLimited) and (not IsFiltered) then
      RowsTotal := DataGrid.RootNodeCount // No need to fetch via SHOW TABLE STATUS
    else
      RowsTotal := MakeInt(ActiveConnection.GetVar('SHOW TABLE STATUS LIKE '+esc(DBObject.Name), 'Rows'));
    if RowsTotal > -1 then begin
      cap := cap + ': ' + FormatNumber(RowsTotal) + ' rows total';
      if DBObject.Engine = 'InnoDB' then
        cap := cap + ' (approximately)';
      // Display either LIMIT or WHERE effect, not both at the same time
      if IsLimited then
        cap := cap + ', limited to ' + FormatNumber(Datagrid.RootNodeCount)
      else if IsFiltered then begin
        if Datagrid.RootNodeCount = RowsTotal then
          cap := cap + ', all rows match to filter'
        else
          cap := cap + ', ' + FormatNumber(Datagrid.RootNodeCount) + ' rows match to filter';
      end;
    end;
  end;
  lblDataTop.Caption := cap;
end;


procedure TMainForm.AnyGridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Idx: PCardinal;
begin
  // Display multiline grid rows
  if prefGridRowsLineCount = DEFAULT_GRIDROWSLINECOUNT then
    Exclude(Node.States, vsMultiLine)
  else
    Include(Node.States, vsMultiLine);
  // Node may have data already, if added via InsertRow
  if not (vsInitialUserData in Node.States) then begin
    Idx := Sender.GetNodeData(Node);
    Idx^ := Node.Index;
  end;
end;


procedure TMainForm.AnyGridGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Cardinal);
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
      if DataGrid.CanFocus then
        DataGrid.SetFocus;
    end else if IsQueryTab(tab.PageIndex, True) then begin
      ActiveQueryMemo.SetFocus;
      ActiveQueryMemo.WordWrap := actQueryWordWrap.Checked;
      SynMemoQueryStatusChange(ActiveQueryMemo, []);
    end;
  end;

  // Filter panel has one text per tab, which we need to update
  UpdateFilterPanel(Sender);

  // Ensure controls are in a valid state
  ValidateControls(Sender);
  FixQueryTabCloseButtons;
end;


procedure TMainForm.PageControlMainChanging(Sender: TObject; var AllowChange: Boolean);
var
  Grid: TVirtualStringTree;
begin
  // Leave editing mode on tab changes so the editor does not stay somewhere
  Grid := ActiveGrid;
  if Assigned(Grid) and Grid.IsEditing then
    Grid.CancelEditNode;
end;


procedure TMainForm.PageControlHostChange(Sender: TObject);
var
  tab: TTabSheet;
  list: TBaseVirtualTree;
begin
  tab := PageControlHost.ActivePage;
  if tab = tabDatabases then list := ListDatabases
  else if tab = tabVariables then list := ListVariables
  else if tab = tabStatus then list := ListStatus
  else if tab = tabProcesslist then list := ListProcesses
  else if tab = tabCommandStats then list := ListCommandStats
  else Exit; // Silence compiler warning
  list.SetFocus;
  UpdateFilterPanel(Sender);
end;


procedure TMainForm.ListTablesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  i, NumObj: Integer;
  Obj: TDBObject;
  Objects: TDBObjectList;
  NumObjects: TStringList;
  Msg: String;
  vt: TVirtualStringTree;
begin
  // DB-Properties
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;

  Screen.Cursor := crHourGlass;

  ShowStatusMsg( 'Displaying objects from "' + ActiveDatabase + '" ...' );
  Objects := ActiveConnection.GetDBObjects(ActiveDatabase, vt.Tag = VTREE_NOTLOADED_PURGECACHE);
  ListTables.BeginUpdate;
  ListTables.Clear;
  ListTables.RootNodeCount := Objects.Count;
  ListTables.EndUpdate;
  vt.Tag := VTREE_LOADED;

  NumObjects := TStringList.Create;
  DBObjectsMaxSize := 1;
  DBObjectsMaxRows := 1;
  for i:=0 to Objects.Count-1 do begin
    Obj := Objects[i];
    NumObj := StrToIntDef(NumObjects.Values[Obj.ObjType], 0);
    Inc(NumObj);
    NumObjects.Values[Obj.ObjType] := IntToStr(NumObj);
    if Obj.Size > DBObjectsMaxSize then DBObjectsMaxSize := Obj.Size;
    if Obj.Rows > DBObjectsMaxRows then DBObjectsMaxRows := Obj.Rows;
  end;
  Msg := ActiveDatabase + ': ' + FormatNumber(Objects.Count) + ' ';
  if NumObjects.Count = 1 then
    Msg := Msg + LowerCase(NumObjects.Names[0])
  else
    Msg := Msg + 'object';
  if Objects.Count <> 1 then Msg := Msg + 's';
  if (NumObjects.Count > 1) and (Objects.Count > 0) then begin
    Msg := Msg + ' (';
    for i:=0 to NumObjects.Count-1 do begin
      NumObj := StrToIntDef(NumObjects.ValueFromIndex[i], 0);
      if NumObj = 0 then
        Continue;
      Msg := Msg + FormatNumber(NumObj) + ' ' + LowerCase(NumObjects.Names[i]);
      if NumObj <> 1 then Msg := Msg + 's';
      Msg := Msg + ', ';
    end;
    Delete(Msg, Length(Msg)-1, 2);
    Msg := Msg + ')';
  end;
  ShowStatusMsg(Msg, 0);
  ShowStatusMsg;
  ValidateControls(Self);
  Screen.Cursor := crDefault;
end;


procedure TMainForm.ListTablesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Obj: PDBObject;
begin
  if not (Kind in [ikNormal, ikSelected]) then
    Exit;
  if Column <> (Sender as TVirtualStringTree).Header.MainColumn then
    Exit;
  Obj := Sender.GetNodeData(Node);
  ImageIndex := Obj.ImageIndex;
end;


procedure TMainForm.ListTablesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TDBObject);
end;


procedure TMainForm.ListTablesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Obj: PDBObject;
begin
  Obj := Sender.GetNodeData(Node);
  CellText := '';
  case Column of
    0: CellText := Obj.Name;
    1: if Obj.Rows > -1 then CellText := FormatNumber(Obj.Rows);
    2: if Obj.Size > -1 then CellText := FormatByteNumber(Obj.Size);
    3: if Obj.Created <> 0 then CellText := DateTimeToStr(Obj.Created);
    4: if Obj.Updated <> 0 then CellText := DateTimeToStr(Obj.Updated);
    5: CellText := Obj.Engine;
    6: CellText := Obj.Comment;
    7: if Obj.Version > -1 then CellText := IntToStr(Obj.Version);
    8: CellText := Obj.RowFormat;
    9: if Obj.AvgRowLen > -1 then CellText := FormatByteNumber(Obj.AvgRowLen);
    10: if Obj.MaxDataLen > -1 then CellText := FormatByteNumber(Obj.MaxDataLen);
    11: if Obj.IndexLen > -1 then CellText := FormatByteNumber(Obj.IndexLen);
    12: if Obj.DataFree > -1 then CellText := FormatByteNumber(Obj.DataFree);
    13: if Obj.AutoInc > -1 then CellText := FormatNumber(Obj.AutoInc);
    14: if Obj.LastChecked <> 0 then CellText := DateTimeToStr(Obj.LastChecked);
    15: CellText := Obj.Collation;
    16: if Obj.Checksum > -1 then CellText := IntToStr(Obj.Checksum);
    17: CellText := Obj.CreateOptions;
    18: CellText := Obj.ObjType;
  end;
end;


procedure TMainForm.ListTablesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Obj: PDBObject;
  Objects: TDBObjectList;
begin
  Obj := Sender.GetNodeData(Node);
  Objects := ActiveConnection.GetDBObjects(ActiveDatabase);
  Obj^ := Objects[Node.Index];
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
  inDataTab, inDataOrQueryTab, inDataOrQueryTabNotEmpty, inGrid, GridHasChanges: Boolean;
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
  RowNum: PCardinal;
begin
  Grid := ActiveGrid;
  Results := nil;
  GridHasChanges := False;
  if Assigned(Grid) then begin
    Results := GridResult(Grid);
    if Assigned(Grid.FocusedNode) then begin
      RowNum := Grid.GetNodeData(Grid.FocusedNode);
      Results.RecNo := RowNum^;
      GridHasChanges := Results.Modified or Results.Inserted;
    end;
  end;
  inDataTab := PageControlMain.ActivePage = tabData;
  inDataOrQueryTab := inDataTab or QueryTabActive;
  inDataOrQueryTabNotEmpty := inDataOrQueryTab and Assigned(Grid) and (Grid.RootNodeCount > 0);
  inGrid := Assigned(Grid) and (ActiveControl = Grid);

  actSQLhelp.Enabled := ActiveConnection.ServerVersionInt >= 40100;
  actImportCSV.Enabled := ActiveConnection.ServerVersionInt >= 32206;

  actDataInsert.Enabled := inGrid and Assigned(Results);
  actDataDuplicateRow.Enabled := inGrid and inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode);
  actDataDelete.Enabled := inGrid and (Grid.SelectedCount > 0);
  actDataFirst.Enabled := inDataOrQueryTabNotEmpty and inGrid;
  actDataLast.Enabled := inDataOrQueryTabNotEmpty and inGrid;
  actDataPostChanges.Enabled := GridHasChanges;
  actDataCancelChanges.Enabled := GridHasChanges;
  actDataSaveBlobToFile.Enabled := inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode);
  actDataPreview.Enabled := inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode);

  // Activate export-options if we're on Data- or Query-tab
  actCopyAsCSV.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsHTML.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsXML.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsSQL.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsLaTeX.Enabled := inDataOrQueryTabNotEmpty;
  actCopyAsWiki.Enabled := inDataOrQueryTabNotEmpty;
  actExportData.Enabled := inDataOrQueryTabNotEmpty;
  actDataSetNull.Enabled := inDataOrQueryTab and Assigned(Results) and Assigned(Grid.FocusedNode);

  ValidateQueryControls(Sender);
  UpdateLineCharPanel;
end;


procedure TMainForm.ValidateQueryControls(Sender: TObject);
var
  NotEmpty, HasSelection: Boolean;
  Tab: TQueryTab;
  cap: String;
  InQueryTab: Boolean;
  i: Integer;
begin
  InQueryTab := QueryTabActive;
  Tab := ActiveQueryTab;
  NotEmpty := InQueryTab and (Tab.Memo.GetTextLen > 0);
  HasSelection := InQueryTab and Tab.Memo.SelAvail;
  actExecuteQuery.Enabled := InQueryTab and NotEmpty;
  actExecuteSelection.Enabled := InQueryTab and HasSelection;
  actExecuteCurrentQuery.Enabled := actExecuteQuery.Enabled;
  actSaveSQLAs.Enabled := InQueryTab and NotEmpty;
  actSaveSQL.Enabled := actSaveSQLAs.Enabled and Tab.Memo.Modified;
  actSaveSQLselection.Enabled := InQueryTab and HasSelection;
  actSaveSQLSnippet.Enabled := InQueryTab and NotEmpty;
  actSaveSQLSelectionSnippet.Enabled := InQueryTab and HasSelection;
  actClearQueryEditor.Enabled := InQueryTab and NotEmpty;
  actSetDelimiter.Enabled := InQueryTab;
  actCloseQueryTab.Enabled := IsQueryTab(PageControlMain.ActivePageIndex, False);
  for i:=0 to QueryTabs.Count-1 do begin
    cap := trim(QueryTabs[i].TabSheet.Caption);
    if cap[Length(cap)] = '*' then
      cap := copy(cap, 1, Length(cap)-1);
    if QueryTabs[i].Memo.Modified then
      cap := cap + '*';
    if QueryTabs[i].TabSheet.Caption <> cap then
      SetTabCaption(QueryTabs[i].TabSheet.PageIndex, cap);
  end;
end;


procedure TMainForm.KillProcess(Sender: TObject);
var
  t: Boolean;
  pid: String;
  Node: PVirtualNode;
begin
  t := TimerRefresh.Enabled;
  TimerRefresh.Enabled := false; // prevent av (ListProcesses.selected...)
  if MessageDlg('Kill '+IntToStr(ListProcesses.SelectedCount)+' Process(es)?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
  begin
    try
      Node := GetNextNode(ListProcesses, nil, True);
      while Assigned(Node) do begin
        pid := ListProcesses.Text[Node, ListProcesses.Header.MainColumn];
        // Don't kill own process
        if pid = IntToStr(ActiveConnection.ThreadId) then
          LogSQL('Ignoring own process id #'+pid+' when trying to kill it.')
        else
          ActiveConnection.Query('KILL '+pid);
        Node := GetNextNode(ListProcesses, Node, True);
      end;
    except
      on E:EDatabaseError do
        MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
    InvalidateVT(ListProcesses, VTREE_NOTLOADED, True);
  end;
  TimerRefresh.Enabled := t; // re-enable autorefresh timer
end;


{ Proposal about to insert a String into synmemo }
procedure TMainForm.SynCompletionProposalCodeCompletion(Sender: TObject;
  var Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  (Sender as TSynCompletionProposal).Form.CurrentEditor.UndoList.AddGroupBreak;
end;


procedure TMainForm.SynCompletionProposalAfterCodeCompletion(Sender: TObject;
  const Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  (Sender as TSynCompletionProposal).Form.CurrentEditor.UndoList.AddGroupBreak;
end;


{ Proposal-Combobox pops up }
procedure TMainForm.SynCompletionProposalExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
var
  i,j: Integer;
  Results: TMySQLQuery;
  DBObjects: TDBObjectList;
  sql, TableClauses, tablename, PrevShortToken, PrevLongToken, Token: String;
  Tables: TStringList;
  rx: TRegExpr;
  Start, TokenTypeInt: Integer;
  Attri: TSynHighlighterAttributes;
  Proposal: TSynCompletionProposal;
  Editor: TCustomSynEdit;
  QueryMarkers: TSQLBatch;
  Query: TSQLSentence;

  procedure addTable(Obj: TDBObject);
  var
    DisplayText: String;
  begin
    DisplayText := Format(SYNCOMPLETION_PATTERN, [Obj.ImageIndex, LowerCase(Obj.ObjType), Obj.Name]);
    Proposal.AddItem(DisplayText, Obj.Name);
  end;

  procedure addColumns( tablename: String );
  var
    dbname : String;
    Columns: TMySQLQuery;
  begin
    dbname := ActiveDatabase;
    if Pos( '.', tablename ) > -1 then
    begin
      dbname := Copy( tablename, 0, Pos( '.', tablename )-1 );
      tablename := Copy( tablename, Pos( '.', tablename )+1, Length(tablename) );
    end;
    // Do not quote db and table name to avoid double masking.
    // Rely on what the user typed is already a valid masked/quoted identifier.
    if dbname <> '' then
      tablename := dbname + '.' + tablename;
    try
      Columns := ActiveConnection.GetResults('SHOW COLUMNS FROM '+tablename);
    except
      Exit;
    end;
    while not Columns.Eof do begin
      Proposal.InsertList.Add(Columns.Col('Field'));
      Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_FIELD, GetFirstWord(Columns.Col('Type')), Columns.Col('Field')]) );
      Columns.Next;
    end;
    FreeAndNil(Columns);
  end;

begin
  if not prefCompletionProposal then begin
    CanExecute := False;
    Exit;
  end;

  Proposal := Sender as TSynCompletionProposal;
  Editor := Proposal.Form.CurrentEditor;
  Editor.GetHighlighterAttriAtRowColEx(Editor.PrevWordPos, Token, TokenTypeInt, Start, Attri);
  if TtkTokenKind(TokenTypeInt) in [tkString, tkComment] then begin
    CanExecute := False;
    Exit;
  end;

  Proposal.InsertList.Clear;
  Proposal.ItemList.Clear;
  PrevShortToken := Proposal.PreviousToken;
  PrevShortToken := WideDequotedStr(PrevShortToken, '`');

  rx := TRegExpr.Create;

  // Find longer token, ignore EndOfTokenChars, just the last chars up to a whitespace, comma or paranthesis
  rx.Expression := '([^\s,\(\)]+)$';
  PrevLongToken := Copy(Editor.LineText, 1, Editor.CaretX-2);
  if rx.Exec(PrevLongToken) then
    PrevLongToken := rx.Match[1]
  else
    PrevLongToken := '';

  // Display list of variables
  rx.Expression := '^@@(SESSION|GLOBAL)$';
  rx.ModifierI := True;
  if rx.Exec(PrevLongToken) then begin
    try
      Results := ActiveConnection.GetResults('SHOW '+UpperCase(rx.Match[1])+' VARIABLES');
      while not Results.Eof do begin
        Proposal.InsertList.Add(Results.Col(0));
        Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_PRIMARYKEY, 'variable', Results.Col(0)+'   \color{clSilver}= '+StringReplace(Results.Col(1), '\', '\\', [rfReplaceAll])] ) );
        Results.Next;
      end;
    except
      // Just log error in sql log, do not disturb user while typing
    end;
    Exit;
  end;

  // Get column-names into the proposal pulldown
  // when we write sql like "SELECT t.|col FROM table [AS] t"
  // Current limitation: Identifiers (masked or not) containing
  // spaces are not detected correctly.

  // 1. find the currently edited sql-statement around the cursor position in synmemo
  if Editor = SynMemoFilter then begin
    // Concat query segments, so the below regular expressions can find structure
    sql := 'SELECT * FROM `'+ActiveDbObj.Name+'` WHERE ' + Editor.Text;
  end else begin
    // Proposal in one of the query tabs
    QueryMarkers := GetSQLSplitMarkers(Editor.Text);
    for Query in QueryMarkers do begin
      if (Query.LeftOffset <= Editor.SelStart) and (Editor.SelStart < Query.RightOffset) then begin
        sql := Copy(Editor.Text, Query.LeftOffset, Query.RightOffset-Query.LeftOffset);
        break;
      end;
    end;
    FreeAndNil(QueryMarkers);
  end;

  // 2. Parse FROM clause to detect relevant table/view, probably aliased
  rx.ModifierG := True;
  rx.Expression := '\b(FROM|INTO|UPDATE)\s+(.+)(WHERE|HAVING|ORDER|GROUP)?';
  if rx.Exec(sql) then begin
    TableClauses := rx.Match[2];
    // Ensure tables in JOIN clause(s) are splitted by comma
    TableClauses := StringReplace(TableClauses, 'JOIN', ',', [rfReplaceAll, rfIgnoreCase]);
    // Split table clauses by commas
    Tables := TStringList.Create;
    Tables.Delimiter := ',';
    Tables.StrictDelimiter := true;
    Tables.DelimitedText := TableClauses;
    rx.Expression := '(\S+)\s+(AS\s+)?(\S+)';

    for i := 0 to Tables.Count - 1 do begin
      // If the just typed word equals the alias of this table or the
      // tablename itself, set tablename var and break loop
      if rx.Exec(Tables[i]) then while true do begin
        if PrevShortToken = WideDequotedStr(rx.Match[3],'`') then begin
          tablename := rx.Match[1];
          break;
        end;
        if not rx.ExecNext then
          break;
      end;
      if tablename <> '' then
        break;
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
    i := ActiveConnection.AllDatabases.IndexOf(PrevShortToken);
    if i > -1 then begin
      // Only display tables from specified db
      Screen.Cursor := crHourGlass;
      DBObjects := ActiveConnection.GetDBObjects(ActiveConnection.AllDatabases[i]);
      for j:=0 to DBObjects.Count-1 do
        addTable(DBObjects[j]);
      Screen.Cursor := crDefault;
    end;
  end;

  if Proposal.ItemList.count = 0 then begin
    // Add databases
    for i := 0 to ActiveConnection.AllDatabases.Count - 1 do begin
      Proposal.InsertList.Add(ActiveConnection.AllDatabases[i]);
      Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_DB, 'database', ActiveConnection.AllDatabases[i]]));
    end;

    if ActiveDatabase <> '' then begin
      // Display tables from current db
      DBObjects := ActiveConnection.GetDBObjects(ActiveDatabase);
      for j:=0 to DBObjects.Count-1 do
        addTable(DBObjects[j]);
      if Length(CurrentInput) = 0 then // assume that we have already a dbname in memo
        Proposal.Position := ActiveConnection.AllDatabases.Count;
    end;

    // Add functions
    for i := 0 to Length(MySQLFunctions) - 1 do begin
      // Don't display unsupported functions here
      if MySqlFunctions[i].Version > ActiveConnection.ServerVersionInt then
        continue;
      Proposal.InsertList.Add( MySQLFunctions[i].Name + MySQLFunctions[i].Declaration );
      Proposal.ItemList.Add( Format(SYNCOMPLETION_PATTERN, [ICONINDEX_FUNCTION, 'function', MySQLFunctions[i].Name + '\color{clGrayText}' + MySQLFunctions[i].Declaration] ) );
    end;

    // Add keywords
    for i := 0 to MySQLKeywords.Count - 1 do begin
      Proposal.InsertList.Add( MySQLKeywords[i] );
      Proposal.ItemList.Add( Format(SYNCOMPLETION_PATTERN, [ICONINDEX_KEYWORD, 'keyword', MySQLKeywords[i]] ) );
    end;

  end;

end;


procedure TMainForm.ParameterCompletionProposalExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string;
  var x, y: Integer; var CanExecute: Boolean);
var
  LeftText, Identifier: String;
  rx: TRegExpr;
  i: Integer;
  DummyBool: Boolean;
  DbObjects: TDBObjectList;
  DbObj: TDbObject;
  Params: TRoutineParamList;
  ItemText, DummyStr: String;
  Prop: TSynCompletionProposal;
begin
  // Display hint on function and procedure parameters

  // Activated in preferences?
  if not prefCompletionProposal then begin
    CanExecute := False;
    Exit;
  end;

  Prop := TSynCompletionProposal(Sender);
  Prop.ItemList.Clear;
  LeftText := Copy(Prop.Form.CurrentEditor.LineText, 0, Prop.Form.CurrentEditor.CaretX-1);
  rx := TRegExpr.Create;
  rx.Expression := '\b([\w\d]+)[`]?\(([^\(]*)$';
  if rx.Exec(LeftText) then begin
    Identifier := rx.Match[1];
    // Tell proposal which parameter should be highlighted/bold
    Prop.Form.CurrentIndex := 0;
    for i:=1 to rx.MatchLen[2] do begin
      if rx.Match[2][i] = ',' then
        Prop.Form.CurrentIndex := Prop.Form.CurrentIndex + 1;
    end;

    // Find matching function or procedure object(s)
    DbObjects := ActiveConnection.GetDBObjects(ActiveConnection.Database, False);
    for DbObj in DbObjects do begin
      if (CompareText(DbObj.Name, Identifier)=0) and (DbObj.NodeType in [lntFunction, lntProcedure]) then begin
        Params := TRoutineParamList.Create(True);
        DbObj.Connection.ParseRoutineStructure(DbObj.CreateCode, Params, DummyBool, DummyStr, DummyStr, DummyStr, DummyStr, DummyStr, DummyStr);
        ItemText := '';
        for i:=0 to Params.Count-1 do
          ItemText := ItemText + '"' + Params[i].Name + ': ' + Params[i].Datatype + '", ';
        Delete(ItemText, Length(ItemText)-2, 2);
        Prop.ItemList.Add(ItemText);
      end;
    end;

    // Find matching server function(s)
    for i:=Low(MySqlFunctions) to High(MySqlFunctions) do begin
      if CompareText(MySqlFunctions[i].Name, Identifier)=0 then begin
        ItemText := '"' + Copy(MySqlFunctions[i].Declaration, 2, Length(MySqlFunctions[i].Declaration)-2) + '"';
        ItemText := StringReplace(ItemText, ',', '","', [rfReplaceAll]);
        Prop.ItemList.Add(ItemText);
      end;
    end;


  end;

  CanExecute := Prop.ItemList.Count > 0;
  rx.Free;
end;


procedure TMainForm.SynMemoQueryStatusChange(Sender: TObject; Changes:
    TSynStatusChanges);
begin
  ValidateQueryControls(Sender);
  UpdateLineCharPanel;
end;



procedure TMainForm.TimerHostUptimeTimer(Sender: TObject);
begin
  // Display server uptime
  if Assigned(ActiveConnection) then
    ShowStatusMsg('Uptime: '+FormatTimeNumber(ActiveConnection.ServerUptime), 4)
  else
    ShowStatusMsg('', 4);
end;


procedure TMainForm.ListTablesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Obj: PDBObject;
begin
  // Tables and views can be renamed, routines cannot
  Obj := Sender.GetNodeData(Node);
  Allowed := Obj.NodeType in [lntTable, lntView];
end;


{***
  Rename table after checking the new name for invalid characters
}
procedure TMainForm.ListTablesNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Obj: PDBObject;
begin
  // Fetch data from node
  Obj := Sender.GetNodeData(Node);

  // Try to rename, on any error abort and don't rename ListItem
  try
    // rename table
    ActiveConnection.Query('RENAME TABLE ' + QuoteIdent(Obj.Name) + ' TO ' + QuoteIdent(NewText));

    if SynSQLSyn1.TableNames.IndexOf( NewText ) = -1 then begin
      SynSQLSyn1.TableNames.Add(NewText);
    end;
    // Update nodedata
    Obj.Name := NewText;
    Obj.CreateCode := '';
    // Now the active tree db has to be updated. But calling RefreshTreeDB here causes an AV
    // so we do it manually here
    DBTree.InvalidateChildren(FindDBNode(DBtree, ActiveDatabase), True);
  except
    on E:EDatabaseError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;


procedure TMainForm.TimerConnectedTimer(Sender: TObject);
var
  ConnectedTime: Integer;
begin
  if Assigned(ActiveConnection) and ActiveConnection.Active then begin
    // Calculate and display connection-time. Also, on any connect or reconnect, update server version panel.
    ConnectedTime := ActiveConnection.ConnectionUptime;
    ShowStatusMsg('Connected: ' + FormatTimeNumber(ConnectedTime), 2);
    ShowStatusMsg('MySQL '+ActiveConnection.ServerVersionStr, 3);
  end else begin
    ShowStatusMsg('Disconnected.', 2);
  end;
end;


procedure TMainForm.Clear2Click(Sender: TObject);
begin
  // clear history-memo
  Screen.Cursor := crHourglass;
  SynMemoSQLLog.Gutter.LineNumberStart := SynMemoSQLLog.Gutter.LineNumberStart + SynMemoSQLLog.Lines.Count;
  SynMemoSQLLog.Lines.Clear;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.Copylinetonewquerytab1Click(Sender: TObject);
var
  Tab: TQueryTab;
begin
  // Create new query tab with current line in SQL log. This is for lazy mouse users.
  if actNewQueryTab.Execute then begin
    Tab := QueryTabs[MainForm.QueryTabs.Count-1];
    Tab.Memo.Text := SynMemoSQLLog.LineText;
  end;
end;


procedure TMainForm.QuickFilterClick(Sender: TObject);
var
  Filter, Val, Col: String;
  Item : TMenuItem;
begin
  // Set filter for "where..."-clause
  Item := Sender as TMenuItem;
  Col := DataGrid.Header.Columns[DataGrid.FocusedColumn].Text;
  Filter := '';

  if Item.Tag = 1 then begin
    // Item needs prompt
    Val := InputBox('Specify filter-value...', Item.Caption, 'Value');
    if Val = 'Value' then
      Filter := ''
    else if Item = QF8 then
      Filter := QuoteIdent(Col) + ' = ''' + Val + ''''
    else if Item = QF9 then
      Filter := QuoteIdent(Col) + ' != ''' + Val + ''''
    else if Item = QF10 then
      Filter := QuoteIdent(Col) + ' > ''' + Val + ''''
    else if Item = QF11 then
      Filter := QuoteIdent(Col) + ' < ''' + Val + ''''
    else if Item = QF12 then
      Filter := QuoteIdent(Col) + ' LIKE ''%' + Val + '%''';
  end else
    Filter := Item.Hint;

  if Filter <> '' then begin
    SynMemoFilter.UndoList.AddGroupBreak;
    SynMemoFilter.SelectAll;
    SynmemoFilter.SelText := filter;
    ToggleFilterPanel(True);
    actApplyFilterExecute(Sender);
  end;
end;


procedure TMainForm.popupQueryPopup(Sender: TObject);
begin
  // Sets cursor into memo and activates TAction(s) like paste
  ActiveQueryMemo.SetFocus;
end;

procedure TMainForm.AutoRefreshSetInterval(Sender: TObject);
var
  SecondsStr: String;
  Seconds: Extended;
begin
  // set interval for autorefresh-timer
  SecondsStr := FloatToStr(TimerRefresh.interval div 1000);
  if InputQuery('Auto refresh','Refresh list every ... second(s):', SecondsStr) then begin
    Seconds := StrToFloatDef(SecondsStr, 0);
    if Seconds > 0 then begin
      TimerRefresh.Interval := Trunc(Seconds * 1000);
      TimerRefresh.Enabled := true;
      menuAutoRefresh.Checked := true;
    end
    else
      MessageDLG('Seconds must be between 0 and ' + IntToStr(maxint) + '.', mtError, [mbOK], 0);
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
  H: TVirtualStringTree;
begin
  // dragging an object over the query-memo
  Memo := ActiveQueryMemo;
  src := Source as TControl;
  // Accepting drag's from DBTree and QueryHelpers
  H := ActiveQueryHelpers;
  Accept := (src = DBtree) or ((src = H) and Assigned(H.FocusedNode) and (H.GetNodeLevel(H.FocusedNode)=1));
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
  Text, ItemText: String;
  ShiftPressed: Boolean;
  Tree: TVirtualStringTree;
  Node: PVirtualNode;
begin
  // dropping a tree node or listbox item into the query-memo
  ActiveQueryMemo.UndoList.AddGroupBreak;
  src := Source as TControl;
  Text := '';
  ShiftPressed := KeyPressed(VK_SHIFT);
  Tree := ActiveQueryHelpers;
  // Check for allowed controls as source has already
  // been performed in OnDragOver. So, only do typecasting here.
  if src = DBtree then begin
    // Insert table or database name. If a table is dropped and Shift is pressed, prepend the db name.
    case ActiveDbObj.NodeType of
      lntDb: Text := QuoteIdent(ActiveDbObj.Database, False);
      lntTable..lntEvent: begin
        if ShiftPressed then
          Text := QuoteIdent(ActiveDbObj.Database, False) + '.';
        Text := Text + QuoteIdent(ActiveDbObj.Name, False);
      end;
    end;
  end else if src = Tree then begin
    if (Tree.GetNodeLevel(Tree.FocusedNode) = 1) and Assigned(Tree.FocusedNode) then begin
      case Tree.FocusedNode.Parent.Index of
        HELPERNODE_SNIPPETS:
          Text := ReadTextFile(DirnameSnippets + Tree.Text[Tree.FocusedNode, 0] + '.sql', nil);
        else begin
          Node := Tree.GetFirstChild(Tree.FocusedNode.Parent);
          while Assigned(Node) do begin
            if Tree.Selected[Node] then begin
              ItemText := Tree.Text[Node, 0];
              if Node.Parent.Index = HELPERNODE_COLUMNS then
                ItemText := QuoteIdent(ItemText, False); // Quote column names
              if ShiftPressed then
                Text := Text + ItemText + ',' + CRLF
              else
                Text := Text + ItemText + ', ';
            end;
            Node := Tree.GetNextSibling(Node);
          end;
          Delete(Text, Length(Text)-1, 2);
        end;
      end;
    end;
  end else
    raise Exception.Create('Unspecified source control in drag''n drop operation!');

  if Text <> '' then begin
    ActiveQueryMemo.SelText := Text;
    ActiveQueryMemo.UndoList.AddGroupBreak;
    // Requires to set focus, as doubleclick actions also call this procedure
    ActiveQueryMemo.SetFocus;
  end;
end;



procedure TMainForm.SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TUnicodeStrings);
var
  i: Integer;
begin
  // One or more files from explorer or somewhere else was dropped onto the
  // query-memo - load their contents into seperate tabs
  for i:=0 to AFiles.Count-1 do begin
    if i > 0 then
      actNewQueryTab.Execute;
    QueryLoad(AFiles[i], False, nil);
  end;
end;


procedure TMainForm.SynMemoQueryPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
var
  Editor : TSynEdit;
  OpenChars: array of Char;
  CloseChars: array of Char;
  P: TBufferCoord;
  Pix: TPoint;
  D: TDisplayCoord;
  S: String;
  I: Integer;
  Attri: TSynHighlighterAttributes;
  ArrayLength: Integer;
  start: Integer;
  TmpCharA, TmpCharB: Char;

  function IsCharBracket(AChar: Char): Boolean;
  begin
    Result := CharInSet(AChar, ['{','[','(','<','}',']',')','>']);
  end;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result := Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;
begin
  // Highlight matching brackets
  Editor := TSynEdit(Sender);
  if Editor.SelAvail then exit;
  ArrayLength := 3;

  SetLength(OpenChars, ArrayLength);
  SetLength(CloseChars, ArrayLength);
  for i := 0 to ArrayLength - 1 do
    Case i of
      0: begin OpenChars[i] := '('; CloseChars[i] := ')'; end;
      1: begin OpenChars[i] := '{'; CloseChars[i] := '}'; end;
      2: begin OpenChars[i] := '['; CloseChars[i] := ']'; end;
      3: begin OpenChars[i] := '<'; CloseChars[i] := '>'; end;
    end;

  P := Editor.CaretXY;
  D := Editor.DisplayXY;

  Start := Editor.SelStart;

  if (Start > 0) and (Start <= length(Editor.Text)) then
    TmpCharA := Editor.Text[Start]
  else
    TmpCharA := #0;

  if (Start < length(Editor.Text)) then
    TmpCharB := Editor.Text[Start + 1]
  else
    TmpCharB := #0;

  if not IsCharBracket(TmpCharA) and not IsCharBracket(TmpCharB) then
    Exit;
  S := TmpCharB;
  if not IsCharBracket(TmpCharB) then begin
    P.Char := P.Char - 1;
    S := TmpCharA;
  end;
  Editor.GetHighlighterAttriAtRowCol(P, S, Attri);

  if (Editor.Highlighter.SymbolAttribute = Attri) then begin
    for i:=Low(OpenChars) to High(OpenChars) do begin
      if (S = OpenChars[i]) or (S = CloseChars[i]) then begin
        Pix := CharToPixels(P);

        Editor.Canvas.Brush.Style := bsSolid;
        Editor.Canvas.Font.Assign(Editor.Font);
        Editor.Canvas.Font.Style := Attri.Style;

        if (TransientType = ttAfter) then begin
          Editor.Canvas.Font.Color := clBlack;
          Editor.Canvas.Brush.Color := clAqua;
        end else begin
          Editor.Canvas.Font.Color := Attri.Foreground;
          Editor.Canvas.Brush.Color := Attri.Background;
        end;
        if Editor.Canvas.Font.Color = clNone then
          Editor.Canvas.Font.Color := Editor.Font.Color;
        if Editor.Canvas.Brush.Color = clNone then
          Editor.Canvas.Brush.Color := Editor.Color;

        Editor.Canvas.TextOut(Pix.X, Pix.Y, S);
        P := Editor.GetMatchingBracketEx(P);

        if (P.Char > 0) and (P.Line > 0) then begin
          Pix := CharToPixels(P);
          if Pix.X > Editor.Gutter.Width then begin
            if S = OpenChars[i] then
              Editor.Canvas.TextOut(Pix.X, Pix.Y, CloseChars[i])
            else Editor.Canvas.TextOut(Pix.X, Pix.Y, OpenChars[i]);
          end;
        end;

      end;
    end;
    Editor.Canvas.Brush.Style := bsSolid;
  end;
end;


procedure TMainForm.popupHostPopup(Sender: TObject);
begin
  menuFetchDBitems.Enabled := (PageControlHost.ActivePage = tabDatabases) and (ListDatabases.SelectedCount > 0);
  Kill1.Enabled := (PageControlHost.ActivePage = tabProcessList) and (ListProcesses.SelectedCount > 0);
  menuEditVariable.Enabled := False;
  if ActiveConnection.ServerVersionInt >= 40003 then
    menuEditVariable.Enabled := (PageControlHost.ActivePage = tabVariables) and Assigned(ListVariables.FocusedNode)
  else
    menuEditVariable.Hint := SUnsupported;
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
  InDBTree: Boolean;
  Obj: PDBObject;
  HasFocus, IsDbOrObject: Boolean;
begin
  // DBtree and ListTables both use popupDB as menu. Find out which of them was rightclicked.
  if Sender is TPopupMenu then
    InDBTree := (Sender as TPopupMenu).PopupComponent = DBTree
  else if Sender is TMenuItem then
    InDBTree := TPopupMenu((Sender as TMenuItem).GetParentMenu).PopupComponent = DBTree
  else
    InDBTree := False;

  if InDBtree then begin
    Obj := DBTree.GetNodeData(DBTree.FocusedNode);
    IsDbOrObject := Obj.NodeType in [lntDb, lntTable..lntEvent];
    actCreateDatabase.Enabled := Obj.NodeType = lntNone;
    actCreateTable.Enabled := IsDbOrObject;
    actCreateView.Enabled := IsDbOrObject;
    actCreateRoutine.Enabled := IsDbOrObject;
    actCreateTrigger.Enabled := IsDbOrObject;
    actCreateEvent.Enabled := IsDbOrObject;
    actDropObjects.Enabled := IsDbOrObject;
    actCopyTable.Enabled := Obj.NodeType in [lntTable, lntView];
    actEmptyTables.Enabled := Obj.NodeType in [lntTable, lntView];
    actRunRoutines.Enabled := Obj.NodeType in [lntProcedure, lntFunction];
    menuEditObject.Enabled := IsDbOrObject;
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
    actCreateTrigger.Enabled := True;
    actCreateEvent.Enabled := True;
    actDropObjects.Enabled := ListTables.SelectedCount > 0;
    actEmptyTables.Enabled := True;
    actRunRoutines.Enabled := True;
    menuEditObject.Enabled := HasFocus;
    actCopyTable.Enabled := False;
    if HasFocus then begin
      Obj := ListTables.GetNodeData(ListTables.FocusedNode);
      actCopyTable.Enabled := Obj.NodeType in [lntTable, lntView];
    end;
    menuTreeExpandAll.Visible := False;
    menuTreeCollapseAll.Visible := False;
    menuShowSizeColumn.Visible := False;
    actSelectTreeBackground.Visible := False;
  end;
  actCreateView.Enabled := actCreateView.Enabled and (ActiveConnection.ServerVersionInt >= 50001);
  actCreateRoutine.Enabled := actCreateRoutine.Enabled and (ActiveConnection.ServerVersionInt >= 50003);
  actCreateTrigger.Enabled := actCreateTrigger.Enabled and (ActiveConnection.ServerVersionInt >= 50002);
  actCreateEvent.Enabled := actCreateEvent.Enabled and (ActiveConnection.ServerVersionInt >= 50100);
end;




function TMainForm.QueryLoad(Filename: String; ReplaceContent: Boolean; Encoding: TEncoding): Boolean;

var
  filecontent: String;
  FileSize: Int64;
  msgtext: String;
  LineBreaks: TLineBreaks;
  RunFileDialog: TRunSQLFileForm;
  Tab: TQueryTab;
begin
  Result := False;

  if not FileExists(filename) then begin
    MessageDlg('File not found: "'+filename+'"', mtError, [mbOK], 0);
    Exit;
  end;

  FileSize := _GetFileSize(filename);

  // Ask for action when loading a big file
  if FileSize > 5*SIZE_MB then
  begin
    msgtext := 'The file you are about to load is '+FormatByteNumber(FileSize)+' (> '+FormatByteNumber(5*SIZE_MB, 0)+').' + CRLF + CRLF +
      'Do you want to just run the file to avoid loading it completely into the query-editor ( = memory ) ?' + CRLF + CRLF +
      'Press' + CRLF +
      '  [Yes] to run the file without loading it into the editor' + CRLF +
      '  [No] to load the file into the query editor' + CRLF +
      '  [Cancel] to cancel file opening.';
    case MessageDlg( msgtext, mtWarning, [mbYes, mbNo, mbCancel], 0 ) of
      // Run the file, don't load it into the editor
      mrYes:
        begin
          RunFileDialog := TRunSQLFileForm.Create(Self);
          RunFileDialog.SQLFileName := Filename;
          RunFileDialog.FileEncoding := Encoding;
          RunFileDialog.ShowModal;
          RunFileDialog.Free;
          // Add filename to history menu
          if Pos( DirnameSnippets, filename ) = 0 then
            AddOrRemoveFromQueryLoadHistory(Filename, True, True);
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
  if not QueryTabActive then
    SetMainTab(tabQuery);
  Tab := ActiveQueryTab;
  LogSQL('Loading file "'+filename+'" ('+FormatByteNumber(FileSize)+') into query tab #'+IntToStr(Tab.Number)+' ...', lcInfo);
  try
    filecontent := ReadTextfile(Filename, Encoding);
    if Pos( DirnameSnippets, Filename ) = 0 then
      AddOrRemoveFromQueryLoadHistory(Filename, True, True);
    FillPopupQueryLoad;
    Tab.Memo.UndoList.AddGroupBreak;

    if ScanNulChar(filecontent) then begin
      filecontent := RemoveNulChars(filecontent);
      MessageDlg(SContainsNulCharFile, mtInformation, [mbOK], 0);
    end;

    Tab.Memo.BeginUpdate;
    LineBreaks := ScanLineBreaks(filecontent);
    if ReplaceContent then begin
      Tab.Memo.SelectAll;
      Tab.MemoLineBreaks := LineBreaks;
    end else begin
      if (Tab.MemoLineBreaks <> lbsNone) and (Tab.MemoLineBreaks <> LineBreaks) then
        Tab.MemoLineBreaks := lbsMixed
      else
        Tab.MemoLineBreaks := LineBreaks;
    end;
    if Tab.MemoLineBreaks = lbsMixed then
      MessageDlg('This file contains mixed linebreaks. They have been converted to Windows linebreaks (CR+LF).', mtInformation, [mbOK], 0);

    Tab.Memo.SelText := filecontent;
    Tab.Memo.SelStart := ActiveQueryMemo.SelEnd;
    Tab.Memo.EndUpdate;
    Tab.Memo.Modified := False;
    Tab.MemoFilename := filename;
    Result := True;
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
      5: DefaultExt := 'LaTeX';
    end;
  end;
end;


procedure TMainForm.popupDataGridPopup(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
  i: Integer;
  cpText, Col, value : String;
  CellFocused, InDataGrid: Boolean;
  RowNumber: PCardinal;
const
  CLPBRD : String = 'CLIPBOARD';
begin
  Grid := ActiveGrid;
  CellFocused := Assigned(Grid.FocusedNode) and (Grid.FocusedColumn > NoColumn);
  InDataGrid := Grid = DataGrid;
  DataInsertValue.Enabled := CellFocused;
  QFvalues.Enabled := CellFocused;
  menuQuickFilter.Enabled := InDataGrid;
  actDataResetSorting.Enabled := InDataGrid;
  menuSQLHelpData.Enabled := InDataGrid;
  Refresh3.Enabled := InDataGrid;

  if not CellFocused then
    Exit;
  Results := GridResult(Grid);

  // Manipulate the Quick-filter menuitems
  AnyGridEnsureFullRow(Grid, Grid.FocusedNode);
  RowNumber := Grid.GetNodeData(Grid.FocusedNode);
  Results.RecNo := RowNumber^;
  Col := QuoteIdent(Results.ColumnOrgNames[Grid.FocusedColumn]);
  // 1. block: include selected columnname and value from datagrid in caption
  if Results.IsNull(Grid.FocusedColumn) then begin
    QF1.Hint := Col + ' IS NULL';
    QF2.Hint := Col + ' IS NOT NULL';
    QF3.Visible := False;
    QF4.Visible := False;
    QF5.Visible := False;
    QF6.Visible := False;
    QF7.Visible := False;
  end else begin
    value := Grid.Text[Grid.FocusedNode, Grid.FocusedColumn];
    QF1.Hint := Col + ' = ' + esc( value );
    QF2.Hint := Col + ' != ' + esc( value );
    QF3.Hint := Col + ' > ' + esc( value );
    QF4.Hint := Col + ' < ' + esc( value );
    QF5.Hint := Col + ' LIKE ''' + esc( value, true ) + '%''';
    QF6.Hint := Col + ' LIKE ''%' + esc( value, true ) + '''';
    QF7.Hint := Col + ' LIKE ''%' + esc( value, true ) + '%''';
    QF3.Visible := True;
    QF4.Visible := True;
    QF5.Visible := True;
    QF6.Visible := True;
    QF7.Visible := True;
  end;

  // 2. block: include only selected columnname in caption
  QF8.Hint := Col + ' = "..."';
  QF9.Hint := Col + ' != "..."';
  QF10.Hint := Col + ' > "..."';
  QF11.Hint := Col + ' < "..."';
  QF12.Hint := Col + ' LIKE "%...%"';
  QF13.Hint := Col + ' IS NULL';
  QF14.Hint := Col + ' IS NOT NULL';

  // 3. block: include selected columnname and clipboard-content in caption for one-click-filtering
  cpText := Clipboard.AsText;
  if Length(cpText) < SIZE_KB then begin
    QF15.Enabled := true; QF15.Hint := Col + ' = ' + esc( cpText );
    QF16.Enabled := true; QF16.Hint := Col + ' != ' + esc( cpText );
    QF17.Enabled := true; QF17.Hint := Col + ' > ' + esc( cpText );
    QF18.Enabled := true; QF18.Hint := Col + ' < ' + esc( cpText );
    QF19.Enabled := true; QF19.Hint := Col + ' LIKE ''%' + esc( cpText, true ) + '%''';
  end else begin
    QF15.Enabled := false; QF15.Hint := Col + ' = ' + CLPBRD;
    QF16.Enabled := false; QF16.Hint := Col + ' != ' + CLPBRD;
    QF17.Enabled := false; QF17.Hint := Col + ' > ' + CLPBRD;
    QF18.Enabled := false; QF18.Hint := Col + ' < ' + CLPBRD;
    QF19.Enabled := false; QF19.Hint := Col + ' LIKE %' + CLPBRD + '%';
  end;

  for i:=0 to menuQuickFilter.Count-1 do begin
    if (menuQuickFilter[i].Caption <> '-') // Not a separator
      and (menuQuickFilter[i].Count = 0) // Not a menu with subitems
      and (menuQuickFilter[i].Action = nil) // Not some special item
      then
      menuQuickFilter[i].Caption := sstr(menuQuickFilter[i].Hint, 100);
  end;

end;


procedure TMainForm.QFvaluesClick(Sender: TObject);
var
  Data: TMySQLQuery;
  Col: String;
  Item: TMenuItem;
  i: Integer;
begin
  // Create a list of distinct column values in selected table
  for i:=QFvalues.Count-1 downto 1 do
    QFvalues.Delete(i);
  QFvalues[0].Caption := '';
  QFvalues[0].Hint := '';
  QFvalues[0].OnClick := nil;
  if DataGrid.FocusedColumn = NoColumn then
    Exit;
  Col := DataGridResult.ColumnOrgNames[DataGrid.FocusedColumn];
  ShowStatusMsg('Fetching distinct values ...');
  Data := ActiveConnection.GetResults('SELECT '+QuoteIdent(Col)+', COUNT(*) AS c FROM '+QuoteIdent(ActiveDbObj.Name)+
    ' GROUP BY '+QuoteIdent(Col)+' ORDER BY c DESC, '+QuoteIdent(Col)+' LIMIT 30');
  for i:=0 to Data.RecordCount-1 do begin
    if QFvalues.Count > i then
      Item := QFvalues[i]
    else begin
      Item := TMenuItem.Create(QFvalues);
      QFvalues.Add(Item);
    end;
    Item.Hint := QuoteIdent(Col)+'='+esc(Data.Col(Col));
    Item.Caption := sstr(Item.Hint, 100) + ' (' + FormatNumber(Data.Col('c')) + ')';
    Item.OnClick := QuickFilterClick;
    Data.Next;
  end;
  ShowStatusMsg;
end;


procedure TMainForm.DataInsertValueClick(Sender: TObject);
var
  y, m, d, h, i, s, ms: Word;
  Uid: TGuid;
begin
  DecodeDateTime(Now, y, m, d, h, i, s, ms);
  DataDateTime.Caption := 'DATETIME: ' + Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,i,s]);
  DataDate.Caption := 'DATE: ' + Format('%.4d-%.2d-%.2d', [y,m,d]);
  DataTime.Caption := 'TIME: ' + Format('%.2d:%.2d:%.2d', [h,i,s]);
  DataYear.Caption := 'YEAR: ' + Format('%.4d', [y]);
  DataUNIXtimestamp.Caption := 'UNIX Timestamp: ' + IntToStr(DateTimeToUnix(Now));

  CreateGuid(Uid);
  DataGUID.Caption := 'GUID: ' + GuidToString(Uid);
end;


procedure TMainForm.InsertValue(Sender: TObject);
var
  d: String;
  p: Integer;
  Grid: TVirtualStringTree;
begin
  // Insert date/time-value into table
  d := StripHotkey((Sender as TMenuItem).Caption);
  p := Pos(':', d);
  if p > 0 then
    d := Trim(Copy(d, p+1, MaxInt));
  Grid := ActiveGrid;
  try
    Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := d;
  except on E:EDatabaseError do
    MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;


function TMainForm.GetRootNode(Tree: TBaseVirtualTree; Connection: TMySQLConnection): PVirtualNode;
var
  SessionNode: PVirtualNode;
  SessionObj: PDBObject;
begin
  Result := nil;
  SessionNode := Tree.GetFirstChild(nil);
  while Assigned(SessionNode) do begin
    SessionObj := Tree.GetNodeData(SessionNode);
    if SessionObj.Connection = Connection then begin
      Result := SessionNode;
      break;
    end;
    SessionNode := Tree.GetNextSibling(SessionNode);
  end;
end;


function TMainForm.GetActiveConnection: TMySQLConnection;
begin
  Result := nil;
  if Assigned(ActiveDbObj) then
    Result := ActiveDbObj.Connection;
end;


function TMainForm.GetActiveDatabase: String;
begin
  // Find currently selected database in active connection
  Result := '';
  if (not (csDestroying in ComponentState)) and Assigned(ActiveDBObj) then
    Result := ActiveDBObj.Connection.Database;
end;


procedure TMainForm.SetActiveDatabase(db: String; Connection: TMySQLConnection);
var
  SessionNode, DBNode: PVirtualNode;
  DBObj: PDBObject;
  AlreadySelected: Boolean;
begin
  // Set focus on the wanted db node
  LogSQL('SetActiveDatabase('+db+')', lcDebug);
  SessionNode := GetRootNode(DBtree, Connection);
  if db = '' then
    SelectNode(DBtree, SessionNode)
  else begin
    DBNode := DBtree.GetFirstChild(SessionNode);
    while Assigned(DBNode) do begin
      DBObj := DBtree.GetNodeData(DBNode);
      if DBObj.Database = db then begin
        AlreadySelected := Assigned(DBtree.FocusedNode) and ((DBNode = DBtree.FocusedNode) or (DBNode = DBtree.FocusedNode.Parent));
        if not AlreadySelected then
          SelectNode(DBtree, DBNode);
        break;
      end;
      DBNode := DBtree.GetNextSibling(DBNode);
    end;
  end;
end;


function TMainForm.GetActiveDBObj: TDBObject;
var
  DBObj: PDBObject;
begin
  Result := nil;
  if Assigned(DBtree.FocusedNode) then begin
    DBObj := DBtree.GetNodeData(DBtree.FocusedNode);
    Result := DBObj^;
  end;
end;


procedure TMainForm.SetActiveDBObj(Obj: TDBObject);
var
  FoundNode: PVirtualNode;
begin
  // Find right table/view/... node in tree and select it, implicitely call OnFocusChanged
  LogSQL('SetActiveDBObj('+Obj.Name+')', lcDebug);
  FoundNode := FindDBObjectNode(DBtree, Obj);
  if Assigned(FoundNode) then
    SelectNode(DBTree, FoundNode)
  else
    LogSQL('Table node "' + Obj.Name + '" not found in tree.', lcError);
end;


function TMainForm.FindDBObjectNode(Tree: TBaseVirtualTree; Obj: TDBObject): PVirtualNode;
var
  DbNode, ObjectNode: PVirtualNode;
  DbObj, ObjectObj: PDBObject;
begin
  Result := nil;
  DbNode := Tree.GetFirstChild(GetRootNode(Tree, Obj.Connection));
  while Assigned(DbNode) do begin
    DbObj := Tree.GetNodeData(DbNode);
    if DBObj.IsSameAs(Obj) then begin
      // Caller may have searched this db node
      Result := DBNode;
      break;
    end;
    if DbObj.Database = Obj.Database then begin
      ObjectNode := Tree.GetFirstChild(DbNode);
      while Assigned(ObjectNode) do begin
        ObjectObj := Tree.GetNodeData(ObjectNode);
        if ObjectObj.IsSameAs(Obj) then begin
          // Caller asks for table/event/etc.
          Result := ObjectNode;
          break;
        end;
        ObjectNode := Tree.GetNextSibling(ObjectNode);
      end;
      break;
    end;
    DbNode := Tree.GetNextSibling(DbNode);
  end;
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
    if FilterPanelManuallyOpened then
      SynMemoFilter.SetFocus;
  end;
end;


procedure TMainForm.tabsetQueryMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
  Tabs: TTabSet;
  Rect: TRect;
  Org: TPoint;
  ResultTab: TResultTab;
  HintSQL: String;
begin
  // Display some hint with row/col count + SQL when mouse hovers over result tab
  if (LastHintMousepos.X = x) and (LastHintMousepos.Y = Y) then
    Exit;
  LastHintMousepos := Point(X, Y);
  Tabs := Sender as TTabSet;
  idx := Tabs.ItemAtPos(Point(X, Y), True);
  if (idx = -1) or (idx = LastHintControlIndex) then
    Exit;
  LastHintControlIndex := idx;
  ResultTab := ActiveQueryTab.ResultTabs[idx];
  HintSQL := sstr(ResultTab.Results.SQL, SIZE_KB);
  HintSQL := WrapText(HintSQL, CRLF, ['.',' ',#9,'-',',',';'], 100);
  HintSQL := Trim(HintSQL);
  BalloonHint1.Description := FormatNumber(ResultTab.Results.ColumnCount) + ' columns x ' +
    FormatNumber(ResultTab.Results.RecordCount) + ' rows' + CRLF +
    HintSQL;
  Rect := Tabs.ItemRect(idx);
  Org := Tabs.ClientOrigin;
  OffsetRect(Rect, Org.X, Org.Y);
  BalloonHint1.ShowHint(Rect);
end;


procedure TMainForm.tabsetQueryMouseLeave(Sender: TObject);
begin
  // BalloonHint.HideAfter is -1, so it will stay forever if we wouldn't hide it at some point
  BalloonHint1.HideHint;
  LastHintControlIndex := -1;
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
begin
  // Don't do anything if no item was selected
  if not Assigned(ActiveQueryHelpers.FocusedNode) then
    Exit;

  snippetfile := DirnameSnippets + ActiveQueryHelpers.Text[ActiveQueryHelpers.FocusedNode, 0] + '.sql';
  if MessageDlg('Delete snippet file? ' + CRLF + snippetfile, mtConfirmation, [mbOk, mbCancel], 0) = mrOk then
  begin
    Screen.Cursor := crHourGlass;
    if DeleteFile(snippetfile) then begin
      // Refresh list with snippets
      SnippetFilenames := GetFilesFromDir(DirnameSnippets, '*.sql', True);
      FillPopupQueryLoad;
    end else begin
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
  QueryLoad(DirnameSnippets + ActiveQueryHelpers.Text[ActiveQueryHelpers.FocusedNode, 0] + '.sql', False, nil);
end;


{**
  Load snippet and replace content
}
procedure TMainForm.menuLoadSnippetClick(Sender: TObject);
begin
  QueryLoad(DirnameSnippets + ActiveQueryHelpers.Text[ActiveQueryHelpers.FocusedNode, 0] + '.sql', True, nil);
end;


{**
  Open snippets-directory in Explorer
}
procedure TMainForm.menuExploreClick(Sender: TObject);
begin
  // Normally the snippets folder is created at installation. But it sure
  // can be the case that it has been deleted or that the application was
  // not installed properly. Ask if we should create the folder now.
  if DirectoryExists( DirnameSnippets ) then
    ShellExec( '', DirnameSnippets )
  else
    if MessageDlg( 'Snippets folder does not exist: ' + DirnameSnippets + CRLF + CRLF + 'This folder is normally created when you install '+appname+'.' + CRLF + CRLF + 'Shall it be created now?',
      mtWarning, [mbYes, mbNo], 0 ) = mrYes then
    try
      Screen.Cursor := crHourglass;
      ForceDirectories( DirnameSnippets );
    finally
      Screen.Cursor := crDefault;
    end;
end;


procedure TMainForm.menuFetchDBitemsClick(Sender: TObject);
var
  Node: PVirtualNode;
  db: String;
begin
  // Fill db object cache of selected databases
  try
    Screen.Cursor := crHourglass;
    Node := GetNextNode(ListDatabases, nil, True);
    while Assigned(Node) do begin
      db := ListDatabases.Text[Node, 0];
      ActiveConnection.GetDBObjects(db, True);
      ListDatabases.RepaintNode(Node);
      DBtree.RepaintNode(FindDBNode(DBtree, db));
      Node := GetNextNode(ListDatabases, Node, True);
    end;
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
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
  // Prevent state images, overlaying the normal image
  if not (Kind in [ikNormal, ikSelected]) then Exit;
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
  Screen.Cursor := crHourglass;
  Sender.Treeview.SortTree( HitInfo.Column, Sender.SortDirection );
  Screen.Cursor := crDefault;
end;


{**
  Sorting a column of a VirtualTree by comparing two cells
}
procedure TMainForm.vstCompareNodes(Sender: TBaseVirtualTree; Node1,
    Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  VT: TVirtualStringTree;
begin
  VT := Sender as TVirtualStringTree;
  Result := CompareAnyNode(VT.Text[Node1, Column], VT.Text[Node2, Column]);
end;


{**
  VirtualTree was painted. Adjust background color of sorted column.
}
procedure TMainForm.vstAfterPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
var
  i: Integer;
  h: TVTHeader;
  NewColor: TColor;
begin
  h := (Sender as TVirtualStringTree).Header;
  for i:=0 to h.Columns.Count-1 do begin
    NewColor := clWindow;
    if h.SortColumn = i then
      NewColor := ColorAdjustBrightness(NewColor, COLORSHIFT_SORTCOLUMNS);
    h.Columns[i].Color := NewColor;
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
  raise Exception.Create('Assertion failed: Invalid global VT array.');
end;


{**
  Save setup of a VirtualStringTree to registry
}
procedure TMainForm.SaveListSetup( List: TVirtualStringTree );
var
  i : Byte;
  ColWidths, ColsVisible, ColPos, Regname: String;
  OwnerForm: TWinControl;
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

  // Lists can have the same name over different forms or frames. Find parent form or frame,
  // so we can prepend its name into the registry value name.
  OwnerForm := GetParentFormOrFrame(List);
  // On a windows shutdown, GetParentForm() seems sporadically unable to find the owner form
  // In that case we would cause an exception when accessing it. Emergency break in that case.
  // See issue #1462
  // TODO: Test this, probably fixed by implementing GetParentFormOrFrame, and then again, probably not.
  if not Assigned(OwnerForm) then
    Exit;
  Regname := OwnerForm.Name + '.' + List.Name;
  OpenRegistry;
  MainReg.WriteString( REGPREFIX_COLWIDTHS + Regname, ColWidths );
  MainReg.WriteString( REGPREFIX_COLSVISIBLE + Regname, ColsVisible );
  MainReg.WriteString( REGPREFIX_COLPOS + Regname, ColPos );
  MainReg.WriteString( REGPREFIX_COLSORT + Regname, IntToStr(List.Header.SortColumn) + ',' + IntToStr(Integer(List.Header.SortDirection)));
end;


{**
  Restore setup of VirtualStringTree from registry
}
procedure TMainForm.RestoreListSetup( List: TVirtualStringTree );
var
  i : Byte;
  colwidth, colpos : Integer;
  Value : String;
  ValueList : TStringList;
  Regname: String;
  OwnerForm: TWinControl;
begin
  ValueList := TStringList.Create;

  // Column widths
  OwnerForm := GetParentFormOrFrame(List);
  Regname := OwnerForm.Name + '.' + List.Name;
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
    for i:=0 to List.Header.Columns.Count-1 do begin
      if ValueList.IndexOf( IntToStr(i) ) > -1 then
        List.Header.Columns[i].Options := List.Header.Columns[i].Options + [coVisible]
      else
        List.Header.Columns[i].Options := List.Header.Columns[i].Options - [coVisible];
    end;
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

  // Sort column and direction
  Value := GetRegValue(REGPREFIX_COLSORT + Regname, '');
  if Value <> '' then begin
    ValueList := Explode(',', Value);
    if ValueList.Count = 2 then begin
      List.Header.SortColumn := MakeInt(ValueList[0]);
      if MakeInt(ValueList[1]) = 0 then
        List.Header.SortDirection := sdAscending
      else
        List.Header.SortDirection := sdDescending;
    end;
  end;

  ValueList.Free;
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
  if prefDirnameSessionLogs[Length(prefDirnameSessionLogs)] <> '\' then
    prefDirnameSessionLogs := prefDirnameSessionLogs + '\';
  ForceDirectories(prefDirnameSessionLogs);

  // Determine free filename
  LogfilePattern := '%.6u.log';
  i := 1;
  FileNameSessionLog := prefDirnameSessionLogs + goodfilename(Format(LogfilePattern, [i]));
  while FileExists(FileNameSessionLog) do begin
    inc(i);
    FileNameSessionLog := prefDirnameSessionLogs + goodfilename(Format(LogfilePattern, [i]));
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
    TVTTooltipLineBreakStyle; var HintText: String);
var
  r : TRect;
  DisplayedWidth,
  NeededWidth : Integer;
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree(Sender);
  HintText := Tree.Text[Node, Column];
  HintText := sstr(HintText, SIZE_KB);
  LineBreakStyle := hlbForceMultiLine;
  // Check if the list has shortened the text
  r := Tree.GetDisplayRect(Node, Column, True);
  DisplayedWidth := r.Right-r.Left;
  NeededWidth := Canvas.TextWidth(HintText) + Tree.TextMargin*2;
  // Disable displaying hint if text is displayed completely in list
  if NeededWidth <= DisplayedWidth then
    HintText := '';
end;


procedure TMainForm.menuLogHorizontalScrollbarClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  // Toggle visibility of horizontal scrollbar
  Item := Sender as TMenuItem;
  Item.Checked := not Item.Checked;
  if Item.Checked then
    SynMemoSQLLog.ScrollBars := ssBoth
  else
    SynMemoSQLLog.ScrollBars := ssVertical;
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
  ShellExec('', prefDirnameSessionLogs);
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
  // Hide the draggedout column
  Sender.Columns[Column].Options := Sender.Columns[Column].Options - [coVisible];
end;


procedure TMainForm.vstIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode;
  const SearchText: String; var Result: Integer);
var
  CellText: String;
  VT: TVirtualStringTree;
begin
  // Override VT's default incremental search behaviour. Make it case insensitive.
  VT := Sender as TVirtualStringTree;
  if VT.FocusedColumn = NoColumn then
    Exit;
  CellText := VT.Text[Node, VT.FocusedColumn];
  Result := StrLIComp(PChar(CellText), PChar(SearchText), Length(SearchText));
end;


{**
  A cell in ListCommandStats gets painted.
  Draw a progress bar on it to visualize its percentage value.
}
procedure TMainForm.ListCommandStatsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  NodeData: PVTreeData;
begin
  // Only paint bar in percentage column
  if Column = 4 then begin
    NodeData := Sender.GetNodeData(Node);
    PaintColorBar(MakeFloat(NodeData.Captions[Column]), 100, TargetCanvas, CellRect);
  end;
end;


procedure TMainForm.ListTablesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  Obj: PDBObject;
begin
  // Only paint bar in rows + size column
  if Column in [1, 2] then begin
    Obj := Sender.GetNodeData(Node);
    case Column of
      1: PaintColorBar(Obj.Rows, DBObjectsMaxRows, TargetCanvas, CellRect);
      2: PaintColorBar(Obj.Size, DBObjectsMaxSize, TargetCanvas, CellRect);
    end;
  end;
end;

procedure TMainForm.ListProcessesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  NodeData: PVTreeData;
begin
  if Column = 5 then begin
    NodeData := Sender.GetNodeData(Node);
    PaintColorBar(MakeFloat(NodeData.Captions[Column]), ProcessListMaxTime, TargetCanvas, CellRect);
  end;
end;


procedure TMainForm.PaintColorBar(Value, Max: Extended; TargetCanvas: TCanvas; CellRect: TRect);
var
  BarWidth, CellWidth: Integer;
begin
  if not prefDisplayBars then
    Exit;

  // Add minimal margin to cell edges
  InflateRect(CellRect, -1, -1);
  CellWidth := CellRect.Right - CellRect.Left;

  // Avoid division by zero, when max is 0 - very rare case but reported in issue #2196.
  if (Value > 0) and (Max > 0) then begin
    BarWidth := Round(CellWidth / Max * Value);
    TargetCanvas.Brush.Color := prefBarColor;
    TargetCanvas.Pen.Color := ColorAdjustBrightness(TargetCanvas.Brush.Color, -40);
    TargetCanvas.RoundRect(CellRect.Left, CellRect.Top, CellRect.Left+BarWidth, CellRect.Bottom, 2, 2);
  end;
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
    SynMemoProcessView.Highlighter := SynSQLSyn1;
    SynMemoProcessView.Text := NodeData.Captions[7];
    SynMemoProcessView.Color := clWindow;
  end else begin
    SynMemoProcessView.Highlighter := nil;
    SynMemoProcessView.Text := 'Please select a process in the above list.';
    SynMemoProcessView.Color := clBtnFace;
  end;
  lblExplainProcess.Enabled := enableSQLView
    and (UpperCase(GetFirstWord(SynMemoProcessView.Text)) <> 'SHOW')
    and (SynMemoProcessView.GetTextLen > 0);
  menuExplainProcess.Enabled := lblExplainProcess.Enabled;
end;


{***
  Apply a filter to a Virtual Tree.
}
procedure TMainForm.editFilterVTChange(Sender: TObject);
begin
  // Reset typing timer
  TimerFilterVT.Enabled := False;
  TimerFilterVT.Enabled := True;
  editFilterVT.RightButton.Visible := editFilterVT.Text <> '';
end;


procedure TMainForm.editFilterVTRightButtonClick(Sender: TObject);
begin
  (Sender as TButtonedEdit).Clear;
end;


procedure TMainForm.TimerFilterVTTimer(Sender: TObject);
var
  Node : PVirtualNode;
  VT : TVirtualStringTree;
  i : Integer;
  match : Boolean;
  search : String;
  tab: TTabSheet;
  VisibleCount: Cardinal;
  CellText: String;
begin
  // Disable timer to avoid filtering in a loop
  TimerFilterVT.Enabled := False;
  // Find the correct VirtualTree that shall be filtered
  tab := PageControlMain.ActivePage;
  if tab = tabHost then
    tab := PageControlHost.ActivePage;
  VT := nil;
  if tab = tabDatabases then begin
    VT := ListDatabases;
    FilterTextDatabases := editFilterVT.Text;
  end else if tab = tabVariables then begin
    VT := ListVariables;
    FilterTextVariables := editFilterVT.Text;
  end else if tab = tabStatus then begin
    VT := ListStatus;
    FilterTextStatus := editFilterVT.Text;
  end else if tab = tabProcesslist then begin
    VT := ListProcesses;
    FilterTextProcessList := editFilterVT.Text;
  end else if tab = tabCommandStats then begin
    VT := ListCommandStats;
    FilterTextCommandStats := editFilterVT.Text;
  end else if tab = tabDatabase then begin
    VT := ListTables;
    FilterTextDatabase := editFilterVT.Text;
  end else if tab = tabEditor then begin
    if ActiveObjectEditor is TfrmTableEditor then
      VT := TfrmTableEditor(ActiveObjectEditor).listColumns;
    FilterTextEditor := editFilterVT.Text;
  end else if tab = tabData then begin
    VT := DataGrid;
    FilterTextData := editFilterVT.Text;
  end else if QueryTabActive and (ActiveQueryTab.ActiveResultTab <> nil) then begin
    VT := ActiveGrid;
    ActiveQueryTab.ActiveResultTab.FilterText := editFilterVT.Text;
  end;
  if not Assigned(VT) then
    Exit;
  // Loop through all nodes and hide non matching
  Node := VT.GetFirst;
  search := LowerCase( editFilterVT.Text );
  VisibleCount := 0;
  while Assigned(Node) do begin
    // Don't filter anything if the filter text is empty
    match := search = '';
    // Search for given text in node's captions
    if not match then for i := 0 to VT.Header.Columns.Count - 1 do begin
      CellText := VT.Text[Node, i];
      if Pos( search, LowerCase(CellText)) > 0 then begin
        match := True;
        break;
      end;
    end;
    VT.IsVisible[Node] := match;
    if match then
      inc(VisibleCount);
    Node := VT.GetNext(Node);
  end;
  if search <> '' then begin
    lblFilterVTInfo.Caption := IntToStr(VisibleCount)+' out of '+IntToStr(VT.RootNodeCount)+' matching. '
      + IntToStr(VT.RootNodeCount - VisibleCount) + ' hidden.';
  end else
    lblFilterVTInfo.Caption := '';
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
    NodeData.Captions[1] := ActiveConnection.GetVar('SHOW VARIABLES LIKE '+esc(NodeData.Captions[0]), 1);
end;


procedure TMainForm.DBtreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  // Set pointer size of bound TDBObjects
  NodeDataSize := SizeOf(TDBObject);
end;


{**
  Set text of a treenode before it gets displayed or fetched in any way
}
procedure TMainForm.DBtreeGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  DBObjects: TDBObjectList;
  DBObj: PDBObject;
  i: Integer;
  Bytes: Int64;
  AllListsCached: Boolean;
begin
  DBObj := Sender.GetNodeData(Node);
  case Column of
    0: case DBObj.NodeType of
        lntNone: CellText := DBObj.Connection.SessionName;
        lntDb: CellText := DBObj.Database;
        lntTable..lntEvent: CellText := DBObj.Name;
        lntColumn: CellText := DBObj.Column;
      end;
    1: if DBObj.Connection.Active then case DBObj.NodeType of
        // Calculate and display the sum of all table sizes in ALL dbs if all table lists are cached
        lntNone: begin
            AllListsCached := true;
            for i:=0 to DBObj.Connection.AllDatabases.Count-1 do begin
              if not DBObj.Connection.DbObjectsCached(DBObj.Connection.AllDatabases[i]) then begin
                AllListsCached := false;
                break;
              end;
            end;
            // Will be also set to a negative value by GetTableSize and results of SHOW TABLES
            Bytes := -1;
            if AllListsCached then begin
              Bytes := 0;
              for i:=0 to DBObj.Connection.AllDatabases.Count-1 do begin
                DBObjects := DBObj.Connection.GetDBObjects(DBObj.Connection.AllDatabases[i]);
                Inc(Bytes, DBObjects.DataSize);
              end;
            end;
            if Bytes >= 0 then CellText := FormatByteNumber(Bytes)
            else CellText := '';
          end;
        // Calculate and display the sum of all table sizes in ONE db, if the list is already cached.
        lntDb: begin
            if not DBObj.Connection.DbObjectsCached(DBObj.Database) then
              CellText := ''
            else begin
              DBObjects := DBObj.Connection.GetDBObjects(DBObj.Database);
              CellText := FormatByteNumber(DBObjects.DataSize);
            end;
          end;
        lntTable: CellText := FormatByteNumber(DBObj.Size);
        else CellText := ''; // Applies for views/procs/... which have no size
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
  DBObj: PDBObject;
begin
  if Column > 0 then
    Exit;
  DBObj := Sender.GetNodeData(Node);
  case Kind of
    ikNormal, ikSelected: begin
        ImageIndex := DBObj.ImageIndex;
        Ghosted := (DBObj.NodeType = lntNone) and (not DBObj.Connection.Active)
          or (DBObj.NodeType = lntDB) and (not DBObj.Connection.DbObjectsCached(DBObj.Database));
      end;
    ikOverlay:
      if DBObj.NodeType = lntNone then begin
        if not DBObj.Connection.Active then
          ImageIndex := 158
        else if DBObj.Connection = ActiveConnection then
          ImageIndex := ICONINDEX_HIGHLIGHTMARKER;
      end else if DBObj.NodeType = lntDb then begin
        if (DBObj.Database = DBObj.Connection.Database) then
          ImageIndex := ICONINDEX_HIGHLIGHTMARKER;
      end;
  end;
end;


{**
  Set childcount of an expanding treenode
}
procedure TMainForm.DBtreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  DBObj: PDBObject;
  Columns: TTableColumnList;
begin
  DBObj := Sender.GetNodeData(Node);
  case DBObj.NodeType of
    // Session node expanding
    lntNone: begin
        Screen.Cursor := crHourglass;
        ShowStatusMsg('Reading Databases...');
        if Sender.Tag = VTREE_NOTLOADED_PURGECACHE then try
          DBObj.Connection.RefreshAllDatabases;
        except
          on E:EDatabaseError do
            MessageDlg(E.Message+CRLF+CRLF+'You have no privilege to execute SHOW DATABASES. Please specify one or more databases in your session settings, if you want to see any.', mtError, [mbOK], 0);
        end;
        ShowStatusMsg;
        Sender.Tag := VTREE_LOADED;
        InvalidateVT(ListDatabases, VTREE_NOTLOADED, True);
        ChildCount := DBObj.Connection.AllDatabases.Count;
        Screen.Cursor := crDefault;
      end;
    // DB node expanding
    lntDb: begin
        Screen.Cursor := crHourglass;
        ShowStatusMsg( 'Reading objects ...' );
        try
          ChildCount := DBObj.Connection.GetDBObjects(DBObj.Connection.AllDatabases[Node.Index]).Count;
        finally
          ShowStatusMsg;
          Screen.Cursor := crDefault;
        end;
      end;
    lntTable, lntView:
      if Assigned(SelectDBObjectForm) and (Sender=SelectDBObjectForm.TreeDBO) then begin
        Columns := TTableColumnList.Create(True);
        DBObj.Connection.ParseTableStructure(DBObj.CreateCode, Columns, nil, nil);
        ChildCount := Columns.Count;
      end;
  end;
end;


{**
  Set initial options of a treenode and bind DBobject to node which holds the relevant
  connection object, probably its database and probably its table/view/... specific properties
}
procedure TMainForm.DBtreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
    PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Item, ParentItem: PDBObject;
  DBObjects: TDBObjectList;
  Columns: TTableColumnList;
begin
  Item := Sender.GetNodeData(Node);
  case Sender.GetNodeLevel(Node) of
    0: begin
      Item^ := TDBObject.Create(FConnections[Node.Index]);
      // Ensure plus sign is visible for root (and dbs, see below)
      Include(InitialStates, ivsHasChildren);
    end;
    1: begin
      Item^ := TDBObject.Create(FConnections[Node.Parent.Index]);
      Item.NodeType := lntDb;
      Item.Database := Item.Connection.AllDatabases[Node.Index];
      Include(InitialStates, ivsHasChildren);
    end;
    2: begin
      DBObjects := FConnections[Node.Parent.Parent.Index].GetDBObjects(FConnections[Node.Parent.Parent.Index].AllDatabases[Node.Parent.Index]);
      Item^ := DBObjects[Node.Index];
      if Assigned(SelectDBObjectForm) and (Sender=SelectDBObjectForm.TreeDBO)
        and (Item.NodeType in [lntTable, lntView]) then
        InitialStates := InitialStates + [ivsHasChildren];
    end;
    3: begin
      Item^ := TDBObject.Create(FConnections[Node.Parent.Parent.Parent.Index]);
      Item.NodeType := lntColumn;
      ParentItem := Sender.GetNodeData(Node.Parent);
      Columns := TTableColumnList.Create(True);
      ParentItem.Connection.ParseTableStructure(ParentItem.CreateCode, Columns, nil, nil);
      Item.Database := ParentItem.Database;
      Item.Name := ParentItem.Name;
      Item.Column := Columns[Node.Index].Name;
    end;
  end;
end;


{**
  Selection in database tree has changed
}
procedure TMainForm.DBtreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  DBObj, PrevDBObj: PDBObject;
  MainTabToActivate: TTabSheet;
begin
  if not Assigned(Node) then begin
    LogSQL('DBtreeFocusChanged without node.', lcDebug);
    Exit;
  end;
  LogSQL('DBtreeFocusChanged, Node level: '+IntToStr(Sender.GetNodeLevel(Node))+', FTreeRefreshInProgress: '+IntToStr(Integer(FTreeRefreshInProgress)), lcDebug);

  // Post pending UPDATE
  if Assigned(DataGridResult) and DataGridResult.Modified then
    actDataPostChangesExecute(DataGrid);

  // Set wanted main tab and call SetMainTab later, when all lists have been invalidated
  MainTabToActivate := nil;

  DBObj := Sender.GetNodeData(Node);

  case DBObj.NodeType of

    lntNone: begin
      if (not DBtree.Dragging) and (not QueryTabActive) then
        MainTabToActivate := tabHost;
      DBObj.Connection.Database := '';
    end;

    lntDb: begin
      // Selecting a database can cause an SQL error if the db was deleted from outside. Select previous node in that case.
      try
        DBObj.Connection.Database := DBObj.Database;
      except on E:EDatabaseError do begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          SelectNode(DBtree, TreeClickHistoryPrevious);
          Exit;
        end;
      end;
      if (not DBtree.Dragging) and (not QueryTabActive) then
        MainTabToActivate := tabDatabase;
    end;

    lntTable..lntEvent: begin
      try
        DBObj.Connection.Database := DBObj.Database;
      except on E:EDatabaseError do begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          SelectNode(DBtree, TreeClickHistoryPrevious);
          Exit;
        end;
      end;
      ParseSelectedTableStructure;
      if not FTreeRefreshInProgress then
        PlaceObjectEditor(DBObj^);
      // When a table is clicked in the tree, and the current
      // tab is a Host or Database tab, switch to showing table columns.
      if (PagecontrolMain.ActivePage = tabHost) or (PagecontrolMain.ActivePage = tabDatabase) then
        MainTabToActivate := tabEditor;
      if DataGrid.Tag = VTREE_LOADED then
        InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
      // Update the list of columns
      RefreshHelperNode(HELPERNODE_COLUMNS);
    end;

  end;

  if TreeClickHistoryPrevious(True) <> nil then
    PrevDBObj := Sender.GetNodeData(TreeClickHistoryPrevious(True))
  else
    PrevDBObj := Pointer(TDBObject.Create(nil));

  // When clicked node is from a different connection than before, do session specific stuff here:
  if PrevDBObj.Connection <> DBObj.Connection then begin
    LogSQL('Connection switch!', lcDebug);
    DBTree.Color := GetRegValue(REGNAME_TREEBACKGROUND, clWindow, DBObj.Connection.SessionName);
    FreeAndNil(SQLHelpForm);
    InvalidateVT(ListDatabases, VTREE_NOTLOADED, False);
    InvalidateVT(ListVariables, VTREE_NOTLOADED, False);
    InvalidateVT(ListStatus, VTREE_NOTLOADED, False);
    InvalidateVT(ListProcesses, VTREE_NOTLOADED, False);
    InvalidateVT(ListCommandstats, VTREE_NOTLOADED, False);
    InvalidateVT(ListTables, VTREE_NOTLOADED, False);
  end;
  if (DBObj.NodeType <> lntNone)
    and ((PrevDBObj.Connection <> DBObj.Connection) or (PrevDBObj.Database <> DBObj.Database)) then
    InvalidateVT(ListTables, VTREE_NOTLOADED, True);

  // Store click history item
  SetLength(FTreeClickHistory, Length(FTreeClickHistory)+1);
  FTreeClickHistory[Length(FTreeClickHistory)-1] := Node;

  // Main tab stuff
  tabHost.Caption := 'Host: '+sstr(DBObj.Connection.Parameters.HostName, 20);
  tabDatabase.Caption := 'Database: '+sstr(DBObj.Connection.Database, 20);
  // Make wanted tab visible before activating, to avoid unset tab on Wine
  if Assigned(MainTabToActivate) then
    MainTabToActivate.TabVisible := True;
  SetMainTab(MainTabToActivate);
  tabDatabase.TabVisible := DBObj.NodeType <> lntNone;
  tabEditor.TabVisible := DBObj.NodeType in [lntTable..lntEvent];
  tabData.TabVisible := DBObj.NodeType in [lntTable, lntView];

  DBTree.InvalidateColumn(0);
  FixQueryTabCloseButtons;
  SetWindowCaption;
end;


procedure TMainForm.DBtreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  // Check if some editor has unsaved changes
  if Assigned(ActiveObjectEditor) and Assigned(NewNode) and (NewNode <> OldNode) and (not FTreeRefreshInProgress) then begin
    Allowed := not (ActiveObjectEditor.DeInit in [mrAbort, mrCancel]);
    DBTree.Selected[DBTree.FocusedNode] := not Allowed;
  end else
    Allowed := True;
end;


procedure TMainForm.DBtreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
//  DBObj: PDBObject;
  i: Integer;
begin
  // Keep track of the previously selected tree node's state, to avoid AVs in OnFocusChanged()
  for i:=0 to Length(FTreeClickHistory)-1 do begin
    if Node = FTreeClickHistory[i] then
      FTreeClickHistory[i] := nil;
  end;
  // TODO: Free object if its host or db. Tables/views/... already get freed in Connection.ClearDBObjects
  // does not work here when table is focused, for some reason:
  {DBObj := Sender.GetNodeData(Node);
  if Assigned(DBObj^) and (DBObj.NodeType in [lntNone, lntDb]) then
    logsql('freeing node: type #'+inttostr(integer(dbobj.NodeType))+' name: '+dbobj.database);
    FreeAndNil(DBObj^);
  end; }
end;


function TMainForm.TreeClickHistoryPrevious(MayBeNil: Boolean=False): PVirtualNode;
var
  i: Integer;
begin
  // Navigate to previous or next existant clicked node
  Result := nil;
  for i:=High(FTreeClickHistory) downto Low(FTreeClickHistory) do begin
    if MayBeNil or (FTreeClickHistory[i] <> nil) then begin
      Result := FTreeClickHistory[i];
      break;
    end;
  end;
end;


procedure TMainForm.ParseSelectedTableStructure;
var
  DummyStr: String;
begin
  SelectedTableColumns.Clear;
  SelectedTableKeys.Clear;
  SelectedTableForeignKeys.Clear;
  InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
  try
    case ActiveDbObj.NodeType of
      lntTable:
        ActiveConnection.ParseTableStructure(ActiveDbObj.CreateCode, SelectedTableColumns, SelectedTableKeys, SelectedTableForeignKeys);
      lntView:
        ActiveConnection.ParseViewStructure(ActiveDbObj.CreateCode, ActiveDbObj.Name, SelectedTableColumns, DummyStr, DummyStr, DummyStr, DummyStr);
    end;
  except on E:EDatabaseError do
    MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;


procedure TMainForm.DBObjectsCleared(Connection: TMySQLConnection; Database: String);
var
  Node: PVirtualNode;
begin
  // Avoid AVs while processing FormDestroy
  if csDestroying in ComponentState then
    Exit;
  // Reload objects in ListTables ...
  InvalidateVT(ListTables, VTREE_NOTLOADED, False);
  // ... in database tree
  Node := FindDBNode(DBtree, Database);
  if Assigned(Node) then begin
    DBtree.ReinitNode(Node, False);
    DBtree.ReinitChildren(Node, False);
  end;  
  // ... and perhaps in table tools dialog
  if Assigned(TableToolsDialog) and TableToolsDialog.Visible then begin
    Node := FindDBNode(TableToolsDialog.TreeObjects, Database);
    if Assigned(Node) then begin
      TableToolsDialog.TreeObjects.ReinitNode(Node, False);
      TableToolsDialog.TreeObjects.ReinitChildren(Node, False);
    end;
  end;
end;


procedure TMainForm.DatabaseChanged(Connection: TMySQLConnection; Database: String);
begin
  // Immediately force db icons to repaint, so the user sees the active db state
  DBtree.Repaint;
end;


procedure TMainForm.DBtreeDblClick(Sender: TObject);
var
  DBObj: PDBObject;
  m: TSynMemo;
begin
  // Paste DB or table name into query window on treeview double click.
  if QueryTabActive and Assigned(DBtree.FocusedNode) then begin
    DBObj := DBtree.GetNodeData(DBtree.FocusedNode);
    if DBObj.NodeType in [lntDb, lntTable..lntEvent] then begin
      m := ActiveQueryMemo;
      m.DragDrop(Sender, m.CaretX, m.CaretY);
    end;
  end;
end;


procedure TMainForm.DBtreePaintText(Sender: TBaseVirtualTree; const
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType:
    TVSTTextType);
var
  DBObj: PDBObject;
begin
  // Grey out non-current connection nodes, and rather unimportant "Size" column
  DBObj := Sender.GetNodeData(Node);
  if DBObj.Connection <> ActiveConnection then
    TargetCanvas.Font.Color := $00999999
  else if (Column = 1) and (DBObj.NodeType in [lntTable..lntEvent]) then
    TargetCanvas.Font.Color := $00444444;
end;


{**
  Refresh the whole tree
}
procedure TMainForm.RefreshTree(FocusNewObject: TDBObject=nil);
var
  SessionNode, DBNode: PVirtualNode;
  OnlyDBNode: Boolean;
begin
  // This refreshes exactly one session node and all its db and table nodes.
  // Also, tries to focus the previous focused object, if present.

  // Object editors call RefreshTree in order to make a just created object visible:
  OnlyDBNode := FocusNewObject <> nil;

  // Remember currently selected object
  if FocusNewObject = nil then begin
    FocusNewObject := TDBObject.Create(ActiveConnection);
    FocusNewObject.Assign(ActiveDbObj);
  end;

  // ReInit tree population
  FTreeRefreshInProgress := True;
  try
    if not OnlyDBNode then begin
      FocusNewObject.Connection.ClearAllDbObjects;
      FocusNewObject.Connection.RefreshAllDatabases;
      SessionNode := GetRootNode(DBtree, FocusNewObject.Connection);
      DBtree.ResetNode(SessionNode);
      DBtree.Expanded[SessionNode] := True;
    end else begin
      FocusNewObject.Connection.ClearDbObjects(FocusNewObject.Database);
      DBNode := FindDbNode(DBtree, FocusNewObject.Database);
      if Assigned(DBNode) then
        DBtree.ResetNode(DBNode);
    end;

    // Reselect active or new database if present. Could have been deleted or renamed.
    try
      if FocusNewObject.NodeType in [lntTable..lntEvent] then
        ActiveDBObj := FocusNewObject;
      if not Assigned(DBtree.FocusedNode) then
        SetActiveDatabase(FocusNewObject.Database, FocusNewObject.Connection);
      if not Assigned(DBtree.FocusedNode) then
        SetActiveDatabase('', FocusNewObject.Connection);
    except
    end;
    if not Assigned(DBtree.FocusedNode) then
      raise Exception.Create('Could not find node to focus.');

  finally
    FTreeRefreshInProgress := False;
  end;
end;


{**
  Find a database node in the tree by passing its name
}
function TMainForm.FindDBNode(Tree: TBaseVirtualTree; db: String): PVirtualNode;
var
  DBObj: PDBObject;
  n, DBNode: PVirtualNode;
begin
  Result := nil;
  n := GetRootNode(Tree, ActiveConnection);
  DBNode := Tree.GetFirstChild(n);
  while Assigned(DBNode) do begin
    DBObj := Tree.GetNodeData(DBNode);
    if DBObj.Database = db then begin
      Result := DBNode;
      Break;
    end;
    DBNode := Tree.GetNextSibling(DBNode);
  end;
end;


{**
  Expand all db nodes
}
procedure TMainForm.menuTreeExpandAllClick(Sender: TObject);
begin
  DBtree.FullExpand;
  DBtree.ScrollIntoView(DBtree.FocusedNode, False);
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
  DBtree.ScrollIntoView(DBtree.FocusedNode, False);
end;


procedure TMainForm.editFilterSearchChange(Sender: TObject);
var
  Clause: String;
  i: Integer;
  ed: TEdit;
begin
  ed := TEdit(Sender);
  Clause := '';
  if ed.Text <> '' then begin
    for i:=0 to SelectedTableColumns.Count-1 do begin
      if i > 0 then
        Clause := Clause + ' OR ';
      Clause := Clause + QuoteIdent(SelectedTableColumns[i].Name) + ' LIKE ' + esc('%'+ed.Text+'%');
    end;
  end;
  // Add linebreaks at near right window edge
  Clause := WrapText(Clause, SynMemoFilter.CharsInWindow-5);
  SynMemoFilter.UndoList.AddGroupBreak;
  SynMemoFilter.SelectAll;
  SynMemoFilter.SelText := Clause;
end;


procedure TMainForm.SynMemoFilterStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  actClearFilterEditor.Enabled := (Sender as TSynMemo).GetTextLen > 0;
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



{**
  A grid cell fetches its text content
}
procedure TMainForm.AnyGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  EditingAndFocused: Boolean;
  RowNumber: PCardinal;
  Results: TMySQLQuery;
begin
  if Column = -1 then
    Exit;
  EditingAndFocused := Sender.IsEditing and (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn);
  Results := GridResult(Sender);
  RowNumber := Sender.GetNodeData(Node);
  Results.RecNo := RowNumber^;
  if Results.IsNull(Column) and (not EditingAndFocused) then
    CellText := TEXT_NULL
  else begin
    case Results.DataType(Column).Category of
      dtcInteger, dtcReal: CellText := FormatNumber(Results.Col(Column), False);
      dtcBinary, dtcSpatial: begin
        if actBlobAsText.Checked then
          CellText := Results.Col(Column)
        else
          CellText := '0x' + Results.BinColAsHex(Column);
      end;
      else begin
        CellText := Results.Col(Column);
        if (Length(CellText) = GRIDMAXDATA) and (not Results.HasFullData) then
          CellText := CellText + ' [...]';
      end;
    end;
  end;
end;


procedure TMainForm.CalcNullColors;
var
  i: Integer;
  h, l, s: Word;
begin
  for i:=Low(DatatypeCategories) to High(DatatypeCategories) do begin
    ColorRGBToHLS(DatatypeCategories[i].Color, h, l, s);
    Inc(l, COLORSHIFT_NULLFIELDS);
    s := Max(0, s-2*COLORSHIFT_NULLFIELDS);
    DatatypeCategories[i].NullColor := ColorHLSToRGB(h, l, s);
  end;
end;


{**
  Cell in data- or query grid gets painted. Colorize font. This procedure is
  called extremely often for repainting the grid cells. Keep it highly optimized.
}
procedure TMainForm.AnyGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  cl: TColor;
  r: TMySQLQuery;
  RowNumber: PCardinal;
begin
  if Column = NoColumn then
    Exit;

  r := GridResult(Sender);
  RowNumber := Sender.GetNodeData(Node);
  r.RecNo := RowNumber^;

  // Make primary key columns bold
  if r.ColIsPrimaryKeyPart(Column) then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];

  // Do not apply any color on a selected, highlighted cell to keep readability
  if (vsSelected in Node.States) and (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then
    cl := clHighlightText
  else if vsSelected in Node.States then
    cl := clBlack
  else if r.IsNull(Column) then
    cl := DatatypeCategories[Integer(r.DataType(Column).Category)].NullColor
  else
    cl := DatatypeCategories[Integer(r.DataType(Column).Category)].Color;
  TargetCanvas.Font.Color := cl;
end;


procedure TMainForm.AnyGridAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
  Results: TMySQLQuery;
  RowNum: PCardinal;
begin
  // Don't waist time
  if Column = NoColumn then Exit;
  // Paint a red triangle at the top left corner of the cell
  Results := GridResult(Sender);
  RowNum := Sender.GetNodeData(Node);
  Results.RecNo := RowNum^;
  if Results.Modified(Column) then
    ImageListMain.Draw(TargetCanvas, CellRect.Left, CellRect.Top, 111);
end;


{**
  Header column in datagrid clicked.
  Left button: handle ORDER BY
  Right button: show column selection box
}
procedure TMainForm.DataGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  frm: TForm;
  i, j, LeftColPos: Integer;
  columnexists : Boolean;
  ColName: String;
begin
  if HitInfo.Column = NoColumn then
    Exit;
  if HitInfo.Button = mbLeft then begin
    ColName := Sender.Columns[HitInfo.Column].Text;
    // Add a new order column after a columns title has been clicked
    // Check if order column is already existant
    columnexists := False;
    for i := Low(DataGridSortColumns) to High(DataGridSortColumns) do begin
      if DataGridSortColumns[i].ColumnName = ColName then begin
        // AddOrderCol is already in the list. Switch its direction:
        // ASC > DESC > [delete col]
        columnexists := True;
        if DataGridSortColumns[i].SortDirection = ORDER_ASC then
          DataGridSortColumns[i].SortDirection := ORDER_DESC
        else begin
          // Delete order col
          for j := i to High(DataGridSortColumns) - 1 do
            DataGridSortColumns[j] := DataGridSortColumns[j+1];
          SetLength(DataGridSortColumns, Length(DataGridSortColumns)-1);
        end;
        // We found the matching column, no need to loop further
        break;
      end;
    end;

    if not columnexists then begin
      i := Length(DataGridSortColumns);
      SetLength(DataGridSortColumns, i+1);
      DataGridSortColumns[i] := TOrderCol.Create;
      DataGridSortColumns[i].ColumnName := ColName;
      DataGridSortColumns[i].SortDirection := ORDER_ASC;
    end;
    // Refresh grid, and restore X scroll offset, so the just clicked column is still at the same place.
    LeftColPos := Sender.Columns[HitInfo.Column].Left;
    InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, True);
    Sender.Treeview.OffsetX := -(Sender.Columns[HitInfo.Column].Left - Sender.Treeview.OffsetX - LeftColPos);
  end else begin
    frm := TColumnSelectionForm.Create(self);
    // Position new form relative to btn's position
    frm.Top := HitInfo.Y + DataGrid.ClientOrigin.Y - Integer(DataGrid.Header.Height);
    frm.Left := HitInfo.X + DataGrid.ClientOrigin.X;
    // Display form
    frm.Show;
  end;
end;


procedure TMainForm.actDataSetNullExecute(Sender: TObject);
var
  RowNum: PCardinal;
  Grid: TVirtualStringTree;
  Results: TMySQLQuery;
begin
  // Set cell to NULL value
  Grid := ActiveGrid;
  RowNum := Grid.GetNodeData(Grid.FocusedNode);
  Results := GridResult(Grid);
  Results.RecNo := RowNum^;
  try
    Results.SetCol(Grid.FocusedColumn, '', True);
  except
    on E:EDatabaseError do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
  Grid.RepaintNode(Grid.FocusedNode);
end;


procedure TMainForm.AnyGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
var
  VT: TVirtualStringTree;
  Node: PVirtualNode;
begin
  // Advance to next or previous grid node on Shift+MouseWheel
  if KeyPressed(VK_SHIFT) then begin
    VT := Sender as TVirtualStringTree;
    if Assigned(VT.FocusedNode) then begin
      if WheelDelta > 0 then
        Node := VT.FocusedNode.PrevSibling
      else
        Node := VT.FocusedNode.NextSibling;
      if Assigned(Node) then begin
        SelectNode(VT, Node);
        Handled := True;
      end;
    end;
  end;
end;


{**
  Content of a grid cell was modified
}
procedure TMainForm.AnyGridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Results: TMySQLQuery;
  RowNum: PCardinal;
begin
  Results := GridResult(Sender);
  RowNum := Sender.GetNodeData(Node);
  Results.RecNo := RowNum^;
  try
    if Results.DataType(Column).Category in [dtcInteger, dtcReal] then
      NewText := UnformatNumber(NewText);
    Results.SetCol(Column, NewText, False);
  except
    on E:EDatabaseError do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
  ValidateControls(Sender);
end;


{**
  DataGrid: node and/or column focus is about to change. See if we allow that.
}
procedure TMainForm.AnyGridFocusChanging(Sender: TBaseVirtualTree; OldNode,
    NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed:
    Boolean);
var
  Results: TMySQLQuery;
  RowNum: PCardinal;
begin
  // Detect changed focus and update row
  Allowed := True;
  Results := GridResult(Sender);
  if Assigned(OldNode) and (OldNode <> NewNode) then begin
    RowNum := Sender.GetNodeData(OldNode);
    Results.RecNo := RowNum^;
    if Results.Modified then begin
      Allowed := Results.SaveModifications;
      DisplayRowCountStats(Sender);
    end else if Results.Inserted then begin
      Results.DiscardModifications;
      Sender.DeleteNode(OldNode);
    end;
  end;
end;


procedure TMainForm.AnyGridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  ValidateControls(Sender);
  UpdateLineCharPanel;
  if Assigned(Node) and pnlPreview.Visible then
    UpdatePreviewPanel;
end;


procedure TMainForm.AnyGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  g: TVirtualStringTree;
begin
  g := TVirtualStringTree(Sender);
  case Key of
    VK_HOME: g.FocusedColumn := 0;
    VK_END: begin
      if (ssCtrl in Shift) and (g = DataGrid) then
        actDataShowAll.Execute;
      g.FocusedColumn := g.Header.Columns.Count-1;
    end;
    VK_RETURN: if Assigned(g.FocusedNode) then g.EditNode(g.FocusedNode, g.FocusedColumn);
    VK_DOWN: if g.FocusedNode = g.GetLast then actDataInsertExecute(Sender);
    VK_NEXT: if (g = DataGrid) and (g.FocusedNode = g.GetLast) then actDataShowNext.Execute;
  end;
end;


procedure TMainForm.AnyGridEditing(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := False;
  try
    GridResult(Sender).CheckEditable;
    if not AnyGridEnsureFullRow(Sender as TVirtualStringTree, Node) then
      MessageDlg('Could not load full row data.', mtError, [mbOk], 0)
    else begin
      Allowed := True;
      // Move Esc shortcut from "Cancel row editing" to "Cancel cell editing"
      actDataCancelChanges.ShortCut := 0;
      actDataPostChanges.ShortCut := 0;
    end;
  except on E:EDatabaseError do
    MessageDlg('Grid editing error: '+E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TMainForm.AnyGridEdited(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex);
begin
  // Reassign Esc to "Cancel row editing" action
  if ([tsEditing, tsEditPending] * Sender.TreeStates) = [] then begin
    actDataCancelChanges.ShortCut := TextToShortcut('Esc');
    actDataPostChanges.ShortCut := TextToShortcut('Ctrl+Enter');
  end;
end;

procedure TMainForm.AnyGridEditCancelled(Sender: TBaseVirtualTree; Column:
    TColumnIndex);
begin
  // Reassign Esc to "Cancel row editing" action
  actDataCancelChanges.ShortCut := TextToShortcut('Esc');
  actDataPostChanges.ShortCut := TextToShortcut('Ctrl+Enter');
end;

procedure TMainForm.AnyGridCreateEditor(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  HexEditor: THexEditorLink;
  DateTimeEditor: TDateTimeEditorLink;
  EnumEditor: TEnumEditorLink;
  SetEditor: TSetEditorLink;
  InplaceEditor: TInplaceEditorLink;
  TypeCat: TDatatypeCategoryIndex;
  ForeignKey: TForeignKey;
  TblColumn: TTableColumn;
  idx: Integer;
  KeyCol, TextCol, SQL, CreateTable, NowText: String;
  Columns: TTableColumnList;
  Keys: TTableKeyList;
  ForeignKeys: TForeignKeyList;
  ForeignResults, Results: TMySQLQuery;
  RowNum: PCardinal;
begin
  VT := Sender as TVirtualStringTree;
  Results := GridResult(VT);
  RowNum := VT.GetNodeData(Node);
  Results.RecNo := RowNum^;

  // Find foreign key values on InnoDB table cells
  if Sender = DataGrid then for ForeignKey in SelectedTableForeignKeys do begin
    idx := ForeignKey.Columns.IndexOf(DataGrid.Header.Columns[Column].Text);
    if idx > -1 then try
      // Find the first text column if available and use that for displaying in the pulldown instead of using meaningless id numbers
      CreateTable := ActiveConnection.GetVar('SHOW CREATE TABLE '+QuoteIdent(ForeignKey.ReferenceTable, True, '.'), 1);
      Columns := TTableColumnList.Create;
      Keys := nil;
      ForeignKeys := nil;
      ActiveConnection.ParseTableStructure(CreateTable, Columns, Keys, ForeignKeys);
      TextCol := '';
      for TblColumn in Columns do begin
        if (TblColumn.DataType.Category = dtcText) and (TblColumn.Name <> ForeignKey.ForeignColumns[idx]) then begin
          TextCol := TblColumn.Name;
          break;
        end;
      end;

      KeyCol := QuoteIdent(ForeignKey.ForeignColumns[idx]);
      SQL := 'SELECT '+KeyCol;
      if TextCol <> '' then SQL := SQL + ', LEFT(' + QuoteIdent(TextCol) + ', 256)';
      SQL := SQL + ' FROM '+QuoteIdent(ForeignKey.ReferenceTable, True, '.')+' GROUP BY '+KeyCol+' ORDER BY ';
      if TextCol <> '' then SQL := SQL + QuoteIdent(TextCol) else SQL := SQL + KeyCol;
      SQL := SQL + ' LIMIT 1000';

      ForeignResults := ActiveConnection.GetResults(SQL);
      if ForeignResults.RecordCount < 1000 then begin
        EnumEditor := TEnumEditorLink.Create(VT);
        EnumEditor.DataType := DataGridResult.DataType(Column).Index;
        EditLink := EnumEditor;
        while not ForeignResults.Eof do begin
          EnumEditor.ValueList.Add(ForeignResults.Col(0));
          if TextCol <> '' then
            EnumEditor.DisplayList.Add(ForeignResults.Col(0)+': '+ForeignResults.Col(1));
          ForeignResults.Next;
        end;
      end;
      ForeignResults.Free;
      break;
    except on E:EDatabaseError do
      // Error gets logged, do nothing more here. All other exception types raise please.
    end;
  end;

  TypeCat := Results.DataType(Column).Category;
  if Assigned(EditLink) then
    // Editor was created above, do nothing now
  else if (TypeCat = dtcText) or ((TypeCat in [dtcBinary, dtcSpatial]) and actBlobAsText.Checked) then begin
    InplaceEditor := TInplaceEditorLink.Create(VT);
    InplaceEditor.DataType := Results.DataType(Column).Index;
    InplaceEditor.MaxLength := Results.MaxLength(Column);
    InplaceEditor.ButtonVisible := True;
    EditLink := InplaceEditor;
  end else if (TypeCat in [dtcBinary, dtcSpatial]) and prefEnableBinaryEditor then begin
    HexEditor := THexEditorLink.Create(VT);
    HexEditor.DataType := Results.DataType(Column).Index;
    HexEditor.MaxLength := Results.MaxLength(Column);
    EditLink := HexEditor;
  end else if (TypeCat = dtcTemporal) and prefEnableDatetimeEditor then begin
    // Ensure date/time editor starts with a non-empty text value
    if Results.Col(Column) = '' then begin
      NowText := ActiveConnection.GetVar('SELECT NOW()');
      case Results.DataType(Column).Index of
        dtDate: NowText := Copy(NowText, 1, 10);
        dtTime: NowText := Copy(NowText, 12, 8);
      end;
      VT.Text[Node, Column] := NowText;
    end;
    DateTimeEditor := TDateTimeEditorLink.Create(VT);
    DateTimeEditor.DataType := Results.DataType(Column).Index;
    EditLink := DateTimeEditor;
  end else if (TypeCat = dtcIntegerNamed) and prefEnableEnumEditor then begin
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.DataType := Results.DataType(Column).Index;
    EnumEditor.ValueList := Results.ValueList(Column);
    EditLink := EnumEditor;
  end else if (TypeCat = dtcSetNamed) and prefEnableSetEditor then begin
    SetEditor := TSetEditorLink.Create(VT);
    SetEditor.DataType := Results.DataType(Column).Index;
    SetEditor.ValueList := Results.ValueList(Column);
    EditLink := SetEditor;
  end else begin
    InplaceEditor := TInplaceEditorLink.Create(VT);
    InplaceEditor.DataType := Results.DataType(Column).Index;
    InplaceEditor.ButtonVisible := False;
    EditLink := InplaceEditor;
  end;
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


procedure TMainForm.AutoCalcColWidth(Tree: TVirtualStringTree; Column: TColumnIndex);
var
  Node: PVirtualNode;
  i, ColTextWidth, ContentTextWidth: Integer;
  Rect: TRect;
  Col: TVirtualTreeColumn;
begin
  // Find optimal default width for columns. Needs to be done late, after the SQL
  // composing to enable text width calculation based on actual table content
  // Weird: Fixes first time calculation always based on Tahoma/8pt font
  Tree.Canvas.Font := Tree.Font;
  Col := Tree.Header.Columns[Column];
  if not (coVisible in Col.Options) then
    Exit;
  ColTextWidth := Tree.Canvas.TextWidth(Col.Text);
  // Add space for sort glyph
  if Col.ImageIndex > -1 then
    ColTextWidth := ColTextWidth + 20;
  Node := Tree.GetFirstVisible;
  // Go backwards 50 nodes from focused one if tree was scrolled
  i := 0;
  if Assigned(Tree.FocusedNode) then begin
    Node := Tree.FocusedNode;
    while Assigned(Node) do begin
      inc(i);
      if (Node = Tree.GetFirst) or (i > 50) then
        break;
      Node := Tree.GetPreviousVisible(Node);
    end;
  end;
  i := 0;
  while Assigned(Node) do begin
    // Note: this causes the node to load, an exception can propagate
    //       here if the query or connection dies.
    Rect := Tree.GetDisplayRect(Node, Column, True, True);
    ContentTextWidth := Rect.Right - Rect.Left;
    if vsMultiLine in Node.States then
      ContentTextWidth := Max(ContentTextWidth, Tree.Canvas.TextWidth(Tree.Text[Node, Column]));
    ColTextWidth := Max(ColTextWidth, ContentTextWidth);
    inc(i);
    if i > 100 then break;
    // GetDisplayRect may have implicitely taken the node away.
    // Strange that Node keeps being assigned though, probably a timing issue.
    if Tree.RootNodeCount = 0 then break;
    Node := Tree.GetNextVisible(Node);
  end;
  // text margins and minimal extra space
  ColTextWidth := ColTextWidth + Tree.TextMargin*2 + 20;
  ColTextWidth := Min(ColTextWidth, prefMaxColWidth);
  Col.Width := ColTextWidth;
end;


procedure TMainForm.AnyGridBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  r: TMySQLQuery;
  cl: TColor;
  RowNumber: PCardinal;
begin
  if Column = -1 then
    Exit;
  r := GridResult(Sender);
  RowNumber := Sender.GetNodeData(Node);
  r.RecNo := RowNumber^;
  cl := clNone;
  if (vsSelected in Node.States) and (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then
    cl := clHighlight
  else if vsSelected in Node.States then
    cl := $00DDDDDD
  else if prefEnableNullBG and r.IsNull(Column) then
    cl := prefNullBG;
  if cl <> clNone then begin
    TargetCanvas.Brush.Color := cl;
    TargetCanvas.FillRect(CellRect);
  end;
end;


procedure TMainForm.HandleDataGridAttributes(RefreshingData: Boolean);
var
  rx: TRegExpr;
  idx, i: Integer;
  TestList: TStringList;
  Sort, KeyName, FocusedCol, CellFocus, Filter: String;
begin
  OpenRegistry;
  MainReg.OpenKey(GetRegKeyTable, True);
  actDataResetSorting.Enabled := False;
  // Clear filter, column names and sort structure if gr
  if not Assigned(DataGridHiddenColumns) then begin
    DataGridHiddenColumns := TStringList.Create;
    DataGridHiddenColumns.Delimiter := DELIM;
    DataGridHiddenColumns.StrictDelimiter := True;
  end;
  if not Assigned(DataGridFocusedCell) then
    DataGridFocusedCell := TStringList.Create;
  // Remember focused node and column for selected table
  if Assigned(DataGrid.FocusedNode) then begin
    KeyName := QuoteIdent(DataGridDB)+'.'+QuoteIdent(DataGridTable);
    FocusedCol := '';
    if DataGrid.FocusedColumn > NoColumn then
      FocusedCol := DataGrid.Header.Columns[DataGrid.FocusedColumn].Text;
    DataGridFocusedCell.Values[KeyName] := IntToStr(DataGrid.FocusedNode.Index) + DELIM + FocusedCol;
  end;
  DataGridFocusedNodeIndex := 0;
  DataGridFocusedColumnName := '';
  KeyName := QuoteIdent(ActiveDbObj.Database)+'.'+QuoteIdent(ActiveDbObj.Name);
  CellFocus := DataGridFocusedCell.Values[KeyName];
  if CellFocus <> '' then begin
    DataGridFocusedNodeIndex := MakeInt(Explode(DELIM, CellFocus)[0]);
    DataGridFocusedColumnName := Explode(DELIM, CellFocus)[1];
  end;
  if not RefreshingData then begin
    DataGridHiddenColumns.Clear;
    SynMemoFilter.Clear;
    SetLength(DataGridSortColumns, 0);
    DataGridWantedRowCount := 0;
    while DataGridFocusedNodeIndex >= DataGridWantedRowCount do
      Inc(DataGridWantedRowCount, prefGridRowcountStep);
  end else begin
    // Save current attributes if grid gets refreshed
    if DataGridHiddenColumns.Count > 0 then
      MainReg.WriteString(REGNAME_HIDDENCOLUMNS, DataGridHiddenColumns.DelimitedText)
    else if MainReg.ValueExists(REGNAME_HIDDENCOLUMNS) then
      MainReg.DeleteValue(REGNAME_HIDDENCOLUMNS);

    if SynMemoFilter.GetTextLen > 0 then
      MainReg.WriteString(REGNAME_FILTER, SynMemoFilter.Text)
    else if MainReg.ValueExists(REGNAME_FILTER) then
      MainReg.DeleteValue(REGNAME_FILTER);

    for i := 0 to High(DataGridSortColumns) do
      Sort := Sort + IntToStr(DataGridSortColumns[i].SortDirection) + '_' + DataGridSortColumns[i].ColumnName + DELIM;
    if Sort <> '' then
      MainReg.WriteString(REGNAME_SORT, Sort)
    else if MainReg.ValueExists(REGNAME_SORT) then
      MainReg.DeleteValue(REGNAME_SORT);
  end;

  // Auto remove registry spam if table folder is empty
  TestList := TStringList.Create;
  MainReg.GetValueNames(TestList);
  if (not MainReg.HasSubKeys) and (TestList.Count = 0) then
    MainReg.DeleteKey(GetRegKeyTable);

  // Do nothing if table was not filtered yet
  if not MainReg.OpenKey(GetRegKeyTable, False) then
    Exit;

  // Columns
  if MainReg.ValueExists(REGNAME_HIDDENCOLUMNS) then
    DataGridHiddenColumns.DelimitedText := MainReg.ReadString(REGNAME_HIDDENCOLUMNS);

  // Set filter, without changing cursor position
  if MainReg.ValueExists(REGNAME_FILTER) then begin
    Filter := MainReg.ReadString(REGNAME_FILTER);
    if SynMemoFilter.Text <> Filter then begin
      SynMemoFilter.Text := Filter;
      SynMemoFilter.Modified := True;
    end;
    if SynMemoFilter.GetTextLen > 0 then
      ToggleFilterPanel(True);
  end;

  // Sort
  if MainReg.ValueExists(REGNAME_SORT) then begin
    SetLength(DataGridSortColumns, 0);
    rx := TRegExpr.Create;
    rx.Expression := '\b(\d)_(.+)\'+DELIM;
    rx.ModifierG := False;
    if rx.Exec(MainReg.ReadString(REGNAME_SORT)) then while true do begin
      idx := Length(DataGridSortColumns);
      // Check if column exists, could be renamed or deleted
      for i:=0 to SelectedTableColumns.Count-1 do begin
        if SelectedTableColumns[i].Name = rx.Match[2] then begin
          SetLength(DataGridSortColumns, idx+1);
          DataGridSortColumns[idx] := TOrderCol.Create;
          DataGridSortColumns[idx].ColumnName := rx.Match[2];
          DataGridSortColumns[idx].SortDirection := StrToIntDef(rx.Match[1], ORDER_ASC);
          break;
        end;
      end;
      if not rx.ExecNext then
        break;
    end;
    actDataResetSorting.Enabled := Length(DataGridSortColumns) > 0;
  end;
end;


function TMainForm.GetRegKeyTable: String;
begin
  // Return the slightly complex registry path to \Servers\ThisServer\curdb|curtable
  Result := REGPATH + REGKEY_SESSIONS + ActiveDbObj.Connection.SessionName + '\' +
    ActiveDatabase + DELIM + ActiveDbObj.Name;
end;


procedure TMainForm.pnlQueryMemoCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  // Ensure visibility of query memo while resizing
  Resize := NewWidth >= treeQueryHelpers.Width + spltQueryHelpers.Width + 40;
end;


procedure TMainForm.AnyGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Grid: TVirtualStringTree;
  Hit: THitInfo;
  Results: TMySQLQuery;
begin
  // Detect mouse hit in grid whitespace and apply changes.
  Grid := Sender as TVirtualStringTree;
  if not Assigned(Grid.FocusedNode) then
    Exit;
  Grid.GetHitTestInfoAt(X, Y, False, Hit);
  if (Hit.HitNode = nil) or (Hit.HitColumn = NoColumn) or (Hit.HitColumn = InvalidColumn) then begin
    Results := GridResult(Grid);
    if Results.Modified then begin
      Results.SaveModifications;
      DisplayRowCountStats(Grid);
    end;
  end;
end;


procedure TMainForm.ListDatabasesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  vt: TVirtualStringTree;
  Val, Max: Extended;
  LoopNode: PVirtualNode;
begin
  // Display color bars
  if Column in [1,2,4..9] then begin
    vt := Sender as TVirtualStringTree;
    // Find out maximum value in column
    LoopNode := vt.GetFirst;
    Max := 1;
    while Assigned(LoopNode) do begin
      Val := MakeFloat(vt.Text[LoopNode, Column]);
      if Val > Max then
        Max := Val;
      LoopNode := vt.GetNext(LoopNode);
    end;
    PaintColorBar(MakeFloat(vt.Text[Node, Column]), Max, TargetCanvas, CellRect);
  end;
end;


procedure TMainForm.ListDatabasesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  vt: TVirtualStringTree;
  i: Integer;
begin
  // Invalidate list of databases, before (re)painting
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  Screen.Cursor := crHourglass;
  vt.Clear;
  try
    if ActiveConnection.InformationSchemaObjects.IndexOf('SCHEMATA') > -1 then
      AllDatabasesDetails := ActiveConnection.GetResults('SELECT * FROM '+QuoteIdent(DBNAME_INFORMATION_SCHEMA)+'.'+QuoteIdent('SCHEMATA'));
  except
    on E:EDatabaseError do
      LogSQL(E.Message, lcError);
  end;
  if vt.Tag = VTREE_NOTLOADED_PURGECACHE then begin
    for i:=0 to ActiveConnection.AllDatabases.Count-1 do begin
      if ActiveConnection.DbObjectsCached(ActiveConnection.AllDatabases[i]) then
        ActiveConnection.GetDBObjects(ActiveConnection.AllDatabases[i], True);
    end;
  end;
  vt.RootNodeCount := ActiveConnection.AllDatabases.Count;
  tabDatabases.Caption := 'Databases ('+FormatNumber(vt.RootNodeCount)+')';
  vt.Tag := VTREE_LOADED;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.ListDatabasesDblClick(Sender: TObject);
begin
  // Select database on doubleclick
  // TODO: Have DBObjects bound to ListDatabases, so we can sort nodes without breaking references
  if Assigned(ListDatabases.FocusedNode) then
    SetActiveDatabase(ListDatabases.Text[ListDatabases.FocusedNode, 0], ActiveConnection);
end;


procedure TMainForm.ListDatabasesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  db: String;
begin
  // Return icon index for databases. Ghosted if db objects not yet in cache.
  if Column <> (Sender as TVirtualStringTree).Header.MainColumn then
    Exit;
  db := ListDatabases.Text[Node, 0];
  case Kind of
    ikNormal, ikSelected: ImageIndex := ICONINDEX_DB;
    ikOverlay: if db = ActiveDatabase then ImageIndex := ICONINDEX_HIGHLIGHTMARKER;
  end;
  Ghosted := not ActiveConnection.DbObjectsCached(db);
end;


procedure TMainForm.ListDatabasesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  // Tell VirtualTree we're using a simple integer as data
  NodeDataSize := SizeOf(Int64);
end;


procedure TMainForm.ListDatabasesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Idx: PInt;
begin
  // Integers mapped to the node's index so nodes can be sorted without losing their database name
  Idx := Sender.GetNodeData(Node);
  Idx^ := Node.Index;
end;


procedure TMainForm.ListDatabasesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Idx: PInt;
  Objects: TDBObjectList;
  DBname: String;

  function GetItemCount(ItemType: TListNodeType): String;
  var
    c: Integer;
    o: TDBObject;
  begin
    if Objects <> nil then begin
      c := 0;
      for o in Objects do begin
        if (ItemType = lntNone) or (o.NodeType = ItemType) then
          Inc(c);
      end;
      Result := FormatNumber(c);
    end else
      Result := '';
  end;

begin
  // Return text for database columns
  Idx := Sender.GetNodeData(Node);
  DBname := ActiveConnection.AllDatabases[Idx^];
  if ActiveConnection.DbObjectsCached(DBname) then
    Objects := ActiveConnection.GetDBObjects(DBname);
  case Column of
    0: CellText := DBname;
    1: if Assigned(Objects) then CellText := FormatByteNumber(Objects.DataSize)
      else CellText := '';
    2: CellText := GetItemCount(lntNone);
    3: if Assigned(Objects) and (Objects.LastUpdate > 0) then CellText := DateTimeToStr(Objects.LastUpdate)
      else CellText := '';
    4: CellText := GetItemCount(lntTable);
    5: CellText := GetItemCount(lntView);
    6: CellText := GetItemCount(lntFunction);
    7: CellText := GetItemCount(lntProcedure);
    8: CellText := GetItemCount(lntTrigger);
    9: CellText := GetItemCount(lntEvent);
    10: begin
      CellText := '';
      if Assigned(AllDatabasesDetails) then begin
        AllDatabasesDetails.First;
        while not AllDatabasesDetails.Eof do begin
          if AllDatabasesDetails.Col('SCHEMA_NAME', True) = DBname then begin
            CellText := AllDatabasesDetails.Col('DEFAULT_COLLATION_NAME', True);
            break;
          end;
          AllDatabasesDetails.Next;
        end;
      end;
    end;
  end;

end;


procedure TMainForm.ListVariablesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  i : Integer;
  vt: TVirtualStringTree;
  Results: TMySQLQuery;
  OldOffset: TPoint;
begin
  // Display server variables
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  Screen.Cursor := crHourglass;
  try
    vt.BeginUpdate;
    OldOffset := vt.OffsetXY;
    vt.Clear;
    Results := ActiveConnection.GetResults('SHOW VARIABLES');
    SetLength(VTRowDataListVariables, Results.RecordCount);
    for i:=0 to Results.RecordCount-1 do begin
      VTRowDataListVariables[i].ImageIndex := 25;
      VTRowDataListVariables[i].Captions := TStringList.Create;
      VTRowDataListVariables[i].Captions.Add(Results.Col(0));
      VTRowDataListVariables[i].Captions.Add(Results.Col(1));
      Results.Next;
    end;
    FreeAndNil(Results);
    vt.RootNodeCount := Length(VTRowDataListVariables);
    vt.OffsetXY := OldOffset;
    // Apply or reset filter
    editFilterVTChange(Sender);
    // Display number of listed values on tab
    tabVariables.Caption := 'Variables (' + IntToStr(vt.RootNodeCount) + ')';
  finally
    // Important to flag the tree as "loaded", otherwise OnPaint will cause an endless loop
    vt.EndUpdate;
    vt.Tag := VTREE_LOADED;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.ListStatusBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  i: Integer;
  valcount: Int64;
  tmpval: Double;
  Results: TMySQLQuery;
  val, avg_perhour, avg_persec: String;
  valIsBytes, valIsNumber: Boolean;
  vt: TVirtualStringTree;
  OldOffset: TPoint;
begin
  // Display server status key/value pairs
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  Screen.Cursor := crHourglass;
  try
    vt.BeginUpdate;
    OldOffset := vt.OffsetXY;
    vt.Clear;
    Results := ActiveConnection.GetResults('SHOW /*!50002 GLOBAL */ STATUS');
    SetLength(VTRowDataListStatus, Results.RecordCount);
    for i:=0 to Results.RecordCount-1 do begin
      VTRowDataListStatus[i].ImageIndex := 25;
      VTRowDataListStatus[i].Captions := TStringList.Create;
      VTRowDataListStatus[i].Captions.Add(Results.Col(0));
      val := Results.Col(1);
      avg_perhour := '';
      avg_persec := '';

      // Detect value type
      try
        valIsNumber := IntToStr(MakeInt(val)) = val;
      except on E:EInvalidOp do
        valIsNumber := False;
      end;
      valIsBytes := valIsNumber and (Copy(Results.Col(0), 1, 6) = 'Bytes_');

      // Calculate average values ...
      if valIsNumber then begin
        valCount := MakeInt(val);
        // ... per hour
        tmpval := valCount / ( ActiveConnection.ServerUptime / 60 / 60 );
        if valIsBytes then avg_perhour := FormatByteNumber( Trunc(tmpval) )
        else avg_perhour := FormatNumber( tmpval, 1 );
        // ... per second
        tmpval := valCount / ActiveConnection.ServerUptime;
        if valIsBytes then avg_persec := FormatByteNumber( Trunc(tmpval) )
        else avg_persec := FormatNumber( tmpval, 1 );
      end;

      // Format numeric or byte values
      if valIsBytes then
        val := FormatByteNumber(val)
      else if valIsNumber then
        val := FormatNumber(val);

      VTRowDataListStatus[i].Captions.Add( val );
      VTRowDataListStatus[i].Captions.Add(avg_perhour);
      VTRowDataListStatus[i].Captions.Add(avg_persec);
      Results.Next;
    end;
    FreeAndNil(Results);
    // Tell VirtualTree the number of nodes it will display
    vt.RootNodeCount := Length(VTRowDataListStatus);
    vt.OffsetXY := OldOffset;
    // Apply or reset filter
    editFilterVTChange(Sender);
    // Display number of listed values on tab
    tabStatus.Caption := 'Status (' + IntToStr(vt.RootNodeCount) + ')';
  finally
    vt.EndUpdate;
    vt.Tag := VTREE_LOADED;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.ListProcessesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  i, j: Integer;
  Results: TMySQLQuery;
  vt: TVirtualStringTree;
  Text: String;
  OldOffset: TPoint;
const
  InfoLen = SIZE_KB*50;
begin
  // Display client threads
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  vt.OnFocusChanged(vt, vt.FocusedNode, vt.FocusedColumn);
  Screen.Cursor := crHourglass;
  try
    vt.BeginUpdate;
    OldOffset := vt.OffsetXY;
    vt.FocusedNode := nil;
    vt.Clear;
    if ActiveConnection.InformationSchemaObjects.IndexOf('PROCESSLIST') > -1 then begin
      // Minimize network traffic on newer servers by fetching only first KB of SQL query in "Info" column
      Results := ActiveConnection.GetResults('SELECT '+QuoteIdent('ID')+', '+QuoteIdent('USER')+', '+QuoteIdent('HOST')+', '+QuoteIdent('DB')+', '
        + QuoteIdent('COMMAND')+', '+QuoteIdent('TIME')+', '+QuoteIdent('STATE')+', LEFT('+QuoteIdent('INFO')+', '+IntToStr(InfoLen)+') AS '+QuoteIdent('Info')
        + ' FROM '+QuoteIdent(DBNAME_INFORMATION_SCHEMA)+'.'+QuoteIdent('PROCESSLIST'));
    end else begin
      // Older servers fetch the whole query length, but at least we cut them off below, so a high memory usage is just a peak
      Results := ActiveConnection.GetResults('SHOW FULL PROCESSLIST');
    end;
    SetLength(VTRowDataListProcesses, Results.RecordCount);
    ProcessListMaxTime := 1;
    for i:=0 to Results.RecordCount-1 do begin
      if AnsiCompareText(Results.Col(4), 'Killed') = 0 then
        VTRowDataListProcesses[i].ImageIndex := 26  // killed
      else begin
        if Results.Col('Info') = '' then
          VTRowDataListProcesses[i].ImageIndex := 55 // idle
        else
          VTRowDataListProcesses[i].ImageIndex := 57 // running query
      end;
      VTRowDataListProcesses[i].Captions := TStringList.Create;
      for j:=0 to Results.ColumnCount-1 do begin
        Text := Results.Col(j);
        if Results.ColumnNames[j] = 'Info' then
          Text := sstr(Text, InfoLen);
        VTRowDataListProcesses[i].Captions.Add(Text);
      end;
      ProcessListMaxTime := Max(ProcessListMaxTime, MakeInt(Results.Col(5)));
      Results.Next;
    end;
    FreeAndNil(Results);
    vt.RootNodeCount := Length(VTRowDataListProcesses);
    vt.OffsetXY := OldOffset;
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
  vt.EndUpdate;
  vt.Tag := VTREE_LOADED;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.ListCommandStatsBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
  procedure addLVitem( idx: Integer; caption: String; commandCount: Int64; totalCount: Int64 );
  var
    tmpval : Double;
  begin
    VTRowDataListCommandStats[idx].ImageIndex := 25;
    VTRowDataListCommandStats[idx].Captions := TStringList.Create;
    caption := Copy( caption, 5, Length(caption) );
    caption := StringReplace( caption, '_', ' ', [rfReplaceAll] );
    VTRowDataListCommandStats[idx].Captions.Add( caption );
    // Total Frequency
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( commandCount ) );
    // Average per hour
    tmpval := commandCount / ( ActiveConnection.ServerUptime / 60 / 60 );
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( tmpval, 1 ) );
    // Average per second
    tmpval := commandCount / ActiveConnection.ServerUptime;
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( tmpval, 1 ) );
    // Percentage. Take care of division by zero errors and Int64's
    if commandCount < 0 then
      commandCount := 0;
    if totalCount < 1 then
      totalCount := 1;
    tmpval := 100 / totalCount * commandCount;
    VTRowDataListCommandStats[idx].Captions.Add( FormatNumber( tmpval, 1 ) + ' %' );
  end;

var
  i: Integer;
  questions: Int64;
  Results: TMySQLQuery;
  vt: TVirtualStringTree;
  OldOffset: TPoint;
begin
  // Display command statistics
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;

  Screen.Cursor := crHourglass;
  try
    vt.BeginUpdate;
    OldOffset := vt.OffsetXY;
    vt.Clear;
    Results := ActiveConnection.GetResults('SHOW /*!50002 GLOBAL */ STATUS LIKE ''Com\_%''' );
    questions := 0;
    while not Results.Eof do begin
      Inc(questions, MakeInt(Results.Col(1)));
      Results.Next;
    end;
    SetLength(VTRowDataListCommandStats, Results.RecordCount+1);
    addLVitem(0, '    All commands', questions, questions );
    Results.First;
    for i:=1 to Results.RecordCount do begin
      addLVitem(i, Results.Col(0), MakeInt(Results.Col(1)), questions );
      Results.Next;
    end;
    FreeAndNil(Results);
    // Tell VirtualTree the number of nodes it will display
    vt.RootNodeCount := Length(VTRowDataListCommandStats);
    vt.OffsetXY := OldOffset;
    // Apply or reset filter
    editFilterVTChange(Sender);
    // Display number of listed values on tab
    tabCommandStats.Caption := 'Command-Statistics (' + IntToStr(vt.RootNodeCount) + ')';
  finally
    vt.EndUpdate;
    vt.Tag := VTREE_LOADED;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actCopyOrCutExecute(Sender: TObject);
var
  Control: TWinControl;
  SendingControl: TComponent;
  Edit: TCustomEdit;
  Combo: TCustomComboBox;
  Grid: TVirtualStringTree;
  SynMemo: TSynMemo;
  Success, DoCut: Boolean;
  SQLStream: TMemoryStream;
  IsResultGrid: Boolean;
  ClpFormat: Word;
  ClpData: THandle;
  APalette: HPalette;
begin
  // Copy text from a focused control to clipboard
  Success := False;
  Control := Screen.ActiveControl;
  DoCut := Sender = actCut;
  SendingControl := (Sender as TAction).ActionComponent;
  Screen.Cursor := crHourglass;
  try
    if SendingControl = btnPreviewCopy then begin
      imgPreview.Picture.SaveToClipBoardFormat(ClpFormat, ClpData, APalette);
      ClipBoard.SetAsHandle(ClpFormat, ClpData);
      Success := True;
    end else if Control is TCustomEdit then begin
      Edit := TCustomEdit(Control);
      if Edit.SelLength > 0 then begin
        if DoCut then Edit.CutToClipboard
        else Edit.CopyToClipboard;
        Success := True;
      end;
    end else if Control is TCustomComboBox then begin
      Combo := TCustomComboBox(Control);
      if Combo.SelLength > 0 then begin
        Clipboard.AsText := Combo.SelText;
        if DoCut then Combo.SelText := '';
        Success := True;
      end;
    end else if Control is TVirtualStringTree then begin
      Grid := Control as TVirtualStringTree;
      if Assigned(Grid.FocusedNode) then begin
        IsResultGrid := Grid = ActiveGrid;
        AnyGridEnsureFullRow(Grid, Grid.FocusedNode);
        Clipboard.AsText := Grid.Text[Grid.FocusedNode, Grid.FocusedColumn];
        if IsResultGrid and DoCut then
          Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := '';
        Success := True;
      end;
    end else if Control is TSynMemo then begin
      SynMemo := Control as TSynMemo;
      if SynMemo.SelAvail then begin
        // Create both text and HTML clipboard format, so rich text applications can paste highlighted SQL
        SynExporterHTML1.ExportAll(Explode(CRLF, SynMemo.SelText));
        if DoCut then SynMemo.CutToClipboard
        else SynMemo.CopyToClipboard;
        SQLStream := TMemoryStream.Create;
        SynExporterHTML1.SaveToStream(SQLStream);
        StreamToClipboard(nil, SQLStream, False);
        Success := True;
      end;
    end;
  finally
    if not Success then
      MessageBeep(MB_ICONASTERISK);
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.actPasteExecute(Sender: TObject);
var
  Control: TWinControl;
  Edit: TCustomEdit;
  Combo: TComboBox;
  Grid: TVirtualStringTree;
  SynMemo: TSynMemo;
  Success: Boolean;
begin
  // Paste text into the focused control
  Success := False;
  Control := Screen.ActiveControl;
  if not Clipboard.HasFormat(CF_TEXT) then begin
    // Do nothing, we cannot paste a picture or so
  end else if Control is TCustomEdit then begin
    Edit := TCustomEdit(Control);
    if not Edit.ReadOnly then begin
      Edit.PasteFromClipboard;
      Success := True;
    end;
  end else if Control is TComboBox then begin
    Combo := TComboBox(Control);
    if Combo.Style = csDropDown then begin
      Combo.SelText := ClipBoard.AsText;
      Success := True;
    end;
  end else if Control is TVirtualStringTree then begin
    Grid := Control as TVirtualStringTree;
    if Assigned(Grid.FocusedNode) and (Grid = ActiveGrid) then begin
      Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := ClipBoard.AsText;
      Success := True;
    end;
  end else if Control is TSynMemo then begin
    SynMemo := TSynMemo(Control);
    if not SynMemo.ReadOnly then begin
      try
        SynMemo.PasteFromClipboard;
        Success := True;
      except on E:Exception do
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  end;
  if not Success then
    MessageBeep(MB_ICONASTERISK);
end;


procedure TMainForm.actSelectAllExecute(Sender: TObject);
var
  Control: TWinControl;
  Grid: TVirtualStringTree;
  ListBox: TListBox;
  Success: Boolean;
begin
  // Select all items, text or whatever
  Success := False;
  Control := Screen.ActiveControl;
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
  end else if Control is TListBox then begin
    ListBox := TListBox(Control);
    if ListBox.MultiSelect then begin
      ListBox.SelectAll;
      Success := True;
    end;
  end;
  if not Success then
    MessageBeep(MB_ICONASTERISK);
end;


procedure TMainForm.actSelectInverseExecute(Sender: TObject);
var
  Control: TWinControl;
  Grid: TVirtualStringTree;
  ListBox: TListBox;
  Success: Boolean;
  i: Integer;
begin
  // Invert selection in grids or listboxes
  Success := False;
  Control := Screen.ActiveControl;
  if Control is TVirtualStringTree then begin
    Grid := TVirtualStringTree(Control);
    if toMultiSelect in Grid.TreeOptions.SelectionOptions then begin
      Grid.InvertSelection(False);
      Success := True;
    end;
  end else if Control is TListBox then begin
    ListBox := TListBox(Control);
    if ListBox.MultiSelect then begin
      for i:=0 to ListBox.Count-1 do
        ListBox.Selected[i] := not ListBox.Selected[i];
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
      // Legacy releases seem to store some integers here
      if MainReg.GetDataType(flt[i]) <> rdString then
        continue;
      item := TMenuItem.Create(popupFilter);
      capt := MainReg.ReadString(flt[i]);
      capt := rx.Replace(capt, ' ', True);
      item.Hint := capt;
      item.Caption := sstr(capt, 50);
      item.Tag := MakeInt(flt[i]);
      item.OnClick := LoadRecentFilter;
      menuRecentFilters.Add(item);
      comboRecentFilters.Items.Add(sstr(capt, 100));
    end;
    FreeAndNil(rx);
    FreeAndNil(flt);
    menuRecentFilters.Enabled := menuRecentFilters.Count > 0;
  end;
  comboRecentFilters.Visible := comboRecentFilters.Items.Count > 0;
  lblRecentFilters.Visible := comboRecentFilters.Visible;
  btnClearFilters.Visible := comboRecentFilters.Visible;
  btnClearFilters.Height := comboRecentFilters.Height;
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
    key := (Sender as TComboBox).ItemIndex+1;
  if MainReg.OpenKey(GetRegKeyTable+'\'+REGNAME_FILTERS, False) then begin
    SynMemoFilter.UndoList.AddGroupBreak;
    SynMemoFilter.BeginUpdate;
    SynMemoFilter.SelectAll;
    SynMemoFilter.SelText := MainReg.ReadString(IntToStr(key));
    SynMemoFilter.EndUpdate;
  end;
end;


procedure TMainForm.PlaceObjectEditor(Obj: TDBObject);
var
  EditorClass: TDBObjectEditorClass;
begin
  // Place the relevant editor frame onto the editor tab, hide all others
  if Assigned(ActiveObjectEditor) and (Obj.NodeType <> ActiveObjectEditor.DBObject.NodeType) then
    FreeAndNil(ActiveObjectEditor);
  case Obj.NodeType of
    lntTable: EditorClass := TfrmTableEditor;
    lntView: EditorClass := TfrmView;
    lntProcedure, lntFunction: EditorClass := TfrmRoutineEditor;
    lntTrigger: EditorClass := TfrmTriggerEditor;
    lntEvent: EditorClass := TfrmEventEditor;
    else Exit;
  end;
  if not Assigned(ActiveObjectEditor) then begin
    ActiveObjectEditor := EditorClass.Create(tabEditor);
    ActiveObjectEditor.Parent := tabEditor;
  end;
  ActiveObjectEditor.Init(Obj);
  UpdateFilterPanel(Self);
end;


procedure TMainForm.UpdateEditorTab;
var
  Cap: String;
begin
  tabEditor.ImageIndex := ActiveObjectEditor.DBObject.ImageIndex;
  Cap := ActiveObjectEditor.DBObject.ObjType+': ';
  if ActiveObjectEditor.DBObject.Name = '' then
    Cap := Cap + '[Untitled]'
  else
    Cap := sstr(Cap + ActiveObjectEditor.DBObject.Name, 30);
  SetTabCaption(tabEditor.PageIndex, Cap);
end;


procedure TMainForm.menuEditObjectClick(Sender: TObject);
var
  Obj: PDBObject;
begin
  if ListTables.Focused then begin
    // Got here from ListTables.OnDblClick or ListTables's context menu item "Edit"
    Obj := ListTables.GetNodeData(ListTables.FocusedNode);
    if not Obj.IsSameAs(ActiveDbObj) then
      ActiveDBObj := Obj^;
    SetMainTab(tabEditor);
  end else begin
    Obj := DBtree.GetNodeData(DBtree.FocusedNode);
    case Obj.NodeType of
      lntDb: begin
        if CreateDatabaseForm = nil then
          CreateDatabaseForm := TCreateDatabaseForm.Create(Self);
        CreateDatabaseForm.modifyDB := ActiveDatabase;
        if CreateDatabaseForm.ShowModal = mrOk then
          RefreshTree;
      end;
      lntTable..lntEvent:
        SetMainTab(tabEditor);
    end;
  end;
end;


procedure TMainForm.ListTablesKeyPress(Sender: TObject; var Key: Char);
begin
  // Open object editor on pressing Enter
  if Ord(Key) = VK_RETURN then
    ListTables.OnDblClick(Sender);
end;


procedure TMainForm.ListTablesDblClick(Sender: TObject);
var
  Obj: PDBObject;
  vt: TVirtualStringTree;
begin
  // DoubleClick: Display editor
  vt := Sender as TVirtualStringTree;
  if Assigned(vt.FocusedNode) then begin
    Obj := vt.GetNodeData(vt.FocusedNode);
    ActiveDBObj := Obj^;
    // Normally the editor tab is active now, but not when same node was focused before
    SetMainTab(tabEditor);
  end;
end;


procedure TMainForm.actNewQueryTabExecute(Sender: TObject);
var
  i: Integer;
  QueryTab: TQueryTab;
  HelperColumn: TVirtualTreeColumn;
begin
  i := QueryTabs[QueryTabs.Count-1].Number + 1;

  QueryTabs.Add(TQueryTab.Create);
  QueryTab := QueryTabs[QueryTabs.Count-1];
  QueryTab.Number := i;

  QueryTab.TabSheet := TTabSheet.Create(PageControlMain);
  QueryTab.TabSheet.PageControl := PageControlMain;
  QueryTab.TabSheet.ImageIndex := tabQuery.ImageIndex;

  QueryTab.CloseButton := TSpeedButton.Create(QueryTab.TabSheet);
  QueryTab.CloseButton.Parent := PageControlMain;
  QueryTab.CloseButton.Width := 16;
  QueryTab.CloseButton.Height := 16;
  QueryTab.CloseButton.Flat := True;
  ImageListMain.GetBitmap(134, QueryTab.CloseButton.Glyph);
  QueryTab.CloseButton.OnMouseDown := CloseButtonOnMouseDown;
  QueryTab.CloseButton.OnMouseUp := CloseButtonOnMouseUp;
  SetTabCaption(QueryTab.TabSheet.PageIndex, '');

  // Dumb code which replicates all controls from tabQuery
  QueryTab.pnlMemo := TPanel.Create(QueryTab.TabSheet);
  QueryTab.pnlMemo.Parent := QueryTab.TabSheet;
  QueryTab.pnlMemo.Tag := pnlQueryMemo.Tag;
  QueryTab.pnlMemo.BevelOuter := pnlQueryMemo.BevelOuter;
  QueryTab.pnlMemo.Align := pnlQueryMemo.Align;

  QueryTab.Memo := TSynMemo.Create(QueryTab.pnlMemo);
  QueryTab.Memo.Parent := QueryTab.pnlMemo;
  QueryTab.Memo.Tag := SynMemoQuery.Tag;
  QueryTab.Memo.Align := SynMemoQuery.Align;
  QueryTab.Memo.Options := SynMemoQuery.Options;
  QueryTab.Memo.PopupMenu := SynMemoQuery.PopupMenu;
  QueryTab.Memo.TabWidth := SynMemoQuery.TabWidth;
  QueryTab.Memo.RightEdge := SynMemoQuery.RightEdge;
  QueryTab.Memo.WantTabs := SynMemoQuery.WantTabs;
  QueryTab.Memo.Highlighter := SynMemoQuery.Highlighter;
  QueryTab.Memo.Gutter.Assign(SynMemoQuery.Gutter);
  QueryTab.Memo.Font.Assign(SynMemoQuery.Font);
  QueryTab.Memo.ActiveLineColor := SynMemoQuery.ActiveLineColor;
  QueryTab.Memo.OnDragDrop := SynMemoQuery.OnDragDrop;
  QueryTab.Memo.OnDragOver := SynMemoQuery.OnDragOver;
  QueryTab.Memo.OnDropFiles := SynMemoQuery.OnDropFiles;
  QueryTab.Memo.OnReplaceText := SynMemoQuery.OnReplaceText;
  QueryTab.Memo.OnStatusChange := SynMemoQuery.OnStatusChange;
  QueryTab.Memo.OnPaintTransient := SynMemoQuery.OnPaintTransient;
  SynCompletionProposal.AddEditor(QueryTab.Memo);
  ParameterCompletionProposal.AddEditor(QueryTab.Memo);

  QueryTab.spltHelpers := TSplitter.Create(QueryTab.pnlMemo);
  QueryTab.spltHelpers.Parent := QueryTab.pnlMemo;
  QueryTab.spltHelpers.Tag := spltQueryHelpers.Tag;
  QueryTab.spltHelpers.Align := spltQueryHelpers.Align;
  QueryTab.spltHelpers.Cursor := spltQueryHelpers.Cursor;
  QueryTab.spltHelpers.ResizeStyle := spltQueryHelpers.ResizeStyle;
  QueryTab.spltHelpers.Width := spltQueryHelpers.Width;

  QueryTab.treeHelpers := TVirtualStringTree.Create(QueryTab.pnlMemo);
  QueryTab.treeHelpers.Parent := QueryTab.pnlMemo;
  QueryTab.treeHelpers.Align := treeQueryHelpers.Align;
  QueryTab.treeHelpers.PopupMenu := treeQueryHelpers.PopupMenu;
  QueryTab.treeHelpers.Images := treeQueryHelpers.Images;
  QueryTab.treeHelpers.DragMode := treeQueryHelpers.DragMode;
  QueryTab.treeHelpers.DragType := treeQueryHelpers.DragType;
  QueryTab.treeHelpers.OnBeforeCellPaint := treeQueryHelpers.OnBeforeCellPaint;
  QueryTab.treeHelpers.OnContextPopup := treeQueryHelpers.OnContextPopup;
  QueryTab.treeHelpers.OnDblClick := treeQueryHelpers.OnDblClick;
  QueryTab.treeHelpers.OnGetImageIndex := treeQueryHelpers.OnGetImageIndex;
  QueryTab.treeHelpers.OnGetText := treeQueryHelpers.OnGetText;
  QueryTab.treeHelpers.OnInitChildren := treeQueryHelpers.OnInitChildren;
  QueryTab.treeHelpers.OnInitNode := treeQueryHelpers.OnInitNode;
  QueryTab.treeHelpers.OnPaintText := treeQueryHelpers.OnPaintText;
  QueryTab.treeHelpers.OnResize := treeQueryHelpers.OnResize;
  for i:=0 to treeQueryHelpers.Header.Columns.Count-1 do begin
    HelperColumn := QueryTab.treeHelpers.Header.Columns.Add;
    HelperColumn.Text := treeQueryHelpers.Header.Columns[i].Text;
    HelperColumn.Width := treeQueryHelpers.Header.Columns[i].Width;
  end;
  QueryTab.treeHelpers.TreeOptions := treeQueryHelpers.TreeOptions;
  QueryTab.treeHelpers.Header.Options := treeQueryHelpers.Header.Options;
  QueryTab.treeHelpers.Header.AutoSizeIndex := treeQueryHelpers.Header.AutoSizeIndex;
  QueryTab.treeHelpers.IncrementalSearch := treeQueryHelpers.IncrementalSearch;
  QueryTab.treeHelpers.RootNodeCount := treeQueryHelpers.RootNodeCount;
  QueryTab.treeHelpers.TextMargin := treeQueryHelpers.TextMargin;
  FixVT(QueryTab.treeHelpers);

  QueryTab.spltQuery := TSplitter.Create(QueryTab.TabSheet);
  QueryTab.spltQuery.Parent := QueryTab.TabSheet;
  QueryTab.spltQuery.Tag := spltQuery.Tag;
  QueryTab.spltQuery.Align := spltQuery.Align;
  QueryTab.spltQuery.Height := spltQuery.Height;
  QueryTab.spltQuery.Cursor := spltQuery.Cursor;
  QueryTab.spltQuery.ResizeStyle := spltQuery.ResizeStyle;
  QueryTab.spltQuery.AutoSnap := spltQuery.AutoSnap;

  QueryTab.ResultTabs := TResultTabs.Create(True);

  QueryTab.tabsetQuery := TTabSet.Create(QueryTab.TabSheet);
  QueryTab.tabsetQuery.Parent := QueryTab.TabSheet;
  QueryTab.tabsetQuery.Align := tabsetQuery.Align;
  QueryTab.tabsetQuery.Images := tabsetQuery.Images;
  QueryTab.tabsetQuery.Style := tabsetQuery.Style;
  QueryTab.tabsetQuery.TabHeight := tabsetQuery.TabHeight;
  QueryTab.tabsetQuery.Height := tabsetQuery.Height;
  QueryTab.tabsetQuery.TabPosition := tabsetQuery.TabPosition;
  QueryTab.tabsetQuery.SoftTop := tabsetQuery.SoftTop;
  QueryTab.tabsetQuery.DitherBackground := tabsetQuery.DitherBackground;
  QueryTab.tabsetQuery.SelectedColor := tabsetQuery.SelectedColor;
  QueryTab.tabsetQuery.UnselectedColor := tabsetQuery.UnselectedColor;
  QueryTab.tabsetQuery.OnClick := tabsetQuery.OnClick;
  QueryTab.tabsetQuery.OnGetImageIndex := tabsetQuery.OnGetImageIndex;
  QueryTab.tabsetQuery.OnMouseMove := tabsetQuery.OnMouseMove;
  QueryTab.tabsetQuery.OnMouseLeave := tabsetQuery.OnMouseLeave;

  SetupSynEditors;

  // Set splitter positions
  QueryTab.pnlMemo.Height := pnlQueryMemo.Height;
  QueryTab.pnlMemo.Top := pnlQueryMemo.Top;
  QueryTab.spltQuery.Top := spltQuery.Top;
  QueryTab.tabsetQuery.Top := tabsetQuery.Top;
  QueryTab.treeHelpers.Width := treeQueryHelpers.Width;

  // Show new tab
  SetMainTab(QueryTab.TabSheet);
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


procedure TMainForm.ClearFiltersClick(Sender: TObject);
var
  Sessions, Keys: TStringList;
  i, idx: Integer;
begin
  // Clear recent data filters
  Keys := TStringList.Create;
  if (Sender = btnClearFilters) or (Sender = menuClearFiltersTable) then begin
    Screen.Cursor := crHourGlass;
    OpenRegistry(ActiveDbObj.Connection.SessionName);
    MainReg.GetKeyNames(Keys);
    idx := Keys.IndexOf(ActiveDbObj.Database+'|'+ActiveDbObj.Name);
    if idx > -1 then
      MainReg.DeleteKey(Keys[idx]);
  end else if Sender = menuClearFiltersSession then begin
    if MessageDlg('Remove all filter stuff for this session ('+ActiveDbObj.Connection.SessionName+') ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      Screen.Cursor := crHourGlass;
      OpenRegistry(ActiveDbObj.Connection.SessionName);
      MainReg.GetKeyNames(Keys);
      for idx:=0 to Keys.Count-1 do
        MainReg.DeleteKey(Keys[idx])
    end;
  end else if Sender = menuClearFiltersAll then begin
    if MessageDlg('Remove all filters across all sessions?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      Screen.Cursor := crHourGlass;
      MainReg.OpenKey(RegPath + REGKEY_SESSIONS, True);
      Sessions := TStringList.Create;
      MainReg.GetKeyNames(Sessions);
      for i:=0 to Sessions.Count-1 do begin
        MainReg.OpenKey(RegPath + REGKEY_SESSIONS + Sessions[i], True);
        Keys.Clear;
        MainReg.GetKeyNames(Keys);
        for idx:=0 to Keys.Count-1 do
          MainReg.DeleteKey(Keys[idx])
      end;
    end;
  end;
  FreeAndNil(Keys);
  FreeAndNil(Sessions);
  EnumerateRecentFilters;
  Screen.Cursor := crDefault;
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
  if PageIndex = tabQuery.PageIndex then
    actClearQueryEditor.Execute;
  if not IsQueryTab(PageIndex, False) then
    Exit;
  // Ask user if query content shall be saved to disk
  if not ConfirmTabClose(PageIndex) then
    Exit;
  // Work around bugs in ComCtrls.TPageControl.RemovePage
  NewPageIndex := PageControlMain.ActivePageIndex;
  if NewPageIndex >= PageIndex then
    Dec(NewPageIndex);
  PageControlMain.Pages[PageIndex].Free;
  QueryTabs.Delete(PageIndex-tabQuery.PageIndex);
  PageControlMain.ActivePageIndex := NewPageIndex;
  FixQueryTabCloseButtons;
  PageControlMain.OnChange(PageControlMain);
end;


procedure TMainForm.comboDBFilterChange(Sender: TObject);
var
  SessionNode, DBNode: PVirtualNode;
  rx: TRegExpr;
  FilterError, NodeMatches: Boolean;
  VisibleCount: Cardinal;
begin
  // Immediately apply database filter
  rx := TRegExpr.Create;
  rx.Expression := '('+StringReplace(comboDBFilter.Text, ';', '|', [rfReplaceAll])+')';
  SessionNode := DBtree.GetFirst;
  VisibleCount := 0;
  FilterError := False;
  while Assigned(SessionNode) do begin
    DBNode := DBtree.GetFirstChild(SessionNode);
    while Assigned(DBNode) do begin
      try
        NodeMatches := rx.Exec(DBtree.Text[DBNode, 0]);
      except
        FilterError := True;
        NodeMatches := True;
      end;
      DBtree.IsVisible[DBNode] := NodeMatches;
      if NodeMatches then
        Inc(VisibleCount);
      DBNode := DBtree.GetNextSibling(DBNode);
    end;
    SessionNode := DBtree.GetNextSibling(SessionNode);
  end;
  rx.Free;
  if VisibleCount = 0 then
    FilterError := True;
  if FilterError then
    comboDBFilter.Color := clWebPink
  else
    comboDBFilter.Color := clWindow;
end;


procedure TMainForm.comboDBFilterExit(Sender: TObject);
var
  i, idx: Integer;
  FilterText: String;
begin
  // Add (move) custom filter text to (in) drop down history, if not empty
  FilterText := comboDBFilter.Text;
  idx := -1;
  for i:=0 to comboDBFilter.Items.Count-1 do begin
    if comboDBFilter.Items[i] = FilterText then begin
      idx := i;
      break;
    end;
  end;
  if idx > -1 then
    comboDBFilter.Items.Move(idx, 0)
  else
    comboDBFilter.Items.Insert(0, FilterText);
  comboDBFilter.Text := FilterText;
end;


procedure TMainForm.comboDBFilterDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  // DBtree dragging node over DB filter dropdown
  Accept := (Source = DBtree) and (ActiveDbObj.NodeType = lntDb);
end;


procedure TMainForm.comboDBFilterDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  dbs: TStringList;
  newdb: String;
begin
  // DBtree node dropped on DB filter dropdown
  dbs := Explode(';', comboDBFilter.Text);
  newdb := DBtree.Text[DBtree.FocusedNode, DBtree.FocusedColumn];
  if dbs.IndexOf(newdb) = -1 then begin
    if (comboDBFilter.Text <> '') and (comboDBFilter.Text[Length(comboDBFilter.Text)-1] <> ';') then
      comboDBFilter.Text := comboDBFilter.Text + ';';
    comboDBFilter.Text := comboDBFilter.Text + newdb;
    comboDBFilter.Items.Insert(0, comboDBFilter.Text);
    comboDBFilter.OnChange(Sender);
  end;
end;


procedure TMainForm.comboDBFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  // Pressing Delete key while filters are dropped down, deletes the filter from the list
  i := comboDBFilter.ItemIndex;
  if comboDBFilter.DroppedDown and (Key=VK_DELETE) and (i > -1) then begin
    Key := 0;
    comboDBFilter.Items.Delete(i);
    if comboDBFilter.Items.Count > i then
      comboDBFilter.ItemIndex := i
    else
      comboDBFilter.ItemIndex := i-1;
    comboDBFilter.OnChange(Sender);
  end;
end;


procedure TMainForm.CloseButtonOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseDownCloseButton := Sender;
end;


procedure TMainForm.CloseButtonOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aPoint: TPoint;
begin
  // Click on "Close" button of Query tab
  if Button <> mbLeft then
    Exit;
  // Between MousDown and MouseUp it is possible that the focused tab has switched. As we simulate a mouse-click
  // here, we must check if also the MouseDown event was fired on this particular button. See issue #1469.
  if (Sender <> FLastMouseDownCloseButton) then
    Exit;
  aPoint := PageControlMain.ScreenToClient((Sender as TSpeedButton).ClientToScreen(Point(X,Y)));
  CloseQueryTab(GetMainTabAt(aPoint.X, aPoint.Y));
end;


procedure TMainForm.PageControlMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurTickcount: Cardinal;
  TabNumber: Integer;
begin
  // Simulate doubleclick on tab to close it
  if Button <> mbLeft then
    Exit;
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
  btn: TSpeedButton;
begin
  // Fix positions of "Close" buttons on Query tabs
  // Avoid AV on Startup, when Mainform.OnResize is called once or twice implicitely.
  if not Assigned(btnAddTab) then
    Exit;
  for PageIndex:=tabQuery.PageIndex+1 to PageControlMain.PageCount-1 do begin
    VisiblePageIndex := PageIndex;
    for i:=0 to PageControlMain.PageCount-1 do begin
      if (i<=VisiblePageIndex) and (not PageControlMain.Pages[i].TabVisible) then
        Dec(VisiblePageIndex);
    end;
    Rect := PageControlMain.TabRect(VisiblePageIndex);
    btn := QueryTabs[PageIndex-tabQuery.PageIndex].CloseButton;
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
end;


function TMainForm.ActiveQueryTab: TQueryTab;
var
  idx: Integer;
begin
  idx := PageControlMain.ActivePageIndex-tabQuery.PageIndex;
  if (idx >= 0) and (idx < QueryTabs.Count) then
    Result := QueryTabs[idx]
  else
    Result := nil;
end;


function TMainForm.ActiveQueryMemo: TSynMemo;
var
  Tab: TQueryTab;
begin
  // Return current query memo
  Tab := ActiveQueryTab;
  Result := nil;
  if Tab <> nil then
    Result := Tab.Memo;
end;


function TMainForm.ActiveQueryHelpers: TVirtualStringTree;
var
  Tab: TQueryTab;
begin
  // Return current query helpers tree
  Tab := ActiveQueryTab;
  Result := nil;
  if Tab <> nil then
    Result := Tab.treeHelpers;
end;


function TMainForm.ActiveSynMemo: TSynMemo;
var
  Control: TWinControl;
begin
  Result := nil;
  Control := Screen.ActiveControl;
  if Control is TCustomSynEdit then begin
    Result := Control as TSynMemo;
    // We have a few readonly-SynMemos which we'll ignore here
    if Result.ReadOnly then
      Result := nil;
  end;
  if (not Assigned(Result)) and QueryTabActive then
    Result := ActiveQueryMemo;
end;


function TMainForm.ActiveGrid: TVirtualStringTree;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := DataGrid
  else if (ActiveQueryTab <> nil) and (ActiveQueryTab.ActiveResultTab <> nil) then
    Result := ActiveQueryTab.ActiveResultTab.Grid;
end;


function TMainForm.GridResult(Grid: TBaseVirtualTree): TMySQLQuery;
var
  QueryTab: TQueryTab;
  CurrentTab: TTabSheet;
  ResultTab: TResultTab;
begin
  // All grids (data- and query-grids) are placed directly on a TTabSheet
  Result := nil;
  if Grid = DataGrid then
    Result := DataGridResult
  else if Assigned(Grid) then begin
    CurrentTab := Grid.Parent as TTabSheet;
    for QueryTab in QueryTabs do begin
      if QueryTab.TabSheet = CurrentTab then begin
        for ResultTab in QueryTab.ResultTabs do begin
          if ResultTab.Grid = Grid then begin
            Result := ResultTab.Results;
            break;
          end;
        end;
      end;
    end;
  end;
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

procedure TMainForm.SetWindowCaption;
var
  Cap: String;
begin
  // Set window caption and taskbar text
  Cap := '';
  if ActiveConnection <> nil then begin
    Cap := Cap + ActiveConnection.SessionName;
    if ActiveDatabase <> '' then
      Cap := Cap + ' /' + ActiveDatabase;
    if Assigned(ActiveDbObj) and (ActiveDbObj.Name <> '') then
      Cap := Cap + '/' + ActiveDbObj.Name;
    Cap := Cap + ' - ';
  end;
  Cap := Cap + APPNAME;
  if PortableMode then
    Cap := Cap + ' Portable';
  Cap := Cap + ' ' + AppVersion;
  Caption := Cap;
  Application.Title := Cap;
end;


procedure TMainForm.OnMessageHandler(var Msg: TMsg; var Handled: Boolean);
begin
  // Clicks on system window menu get handled here
  if Msg.message = WM_SYSCOMMAND then begin
    Handled := True;
    case Msg.wParam of
      MSG_UPDATECHECK: Mainform.actUpdateCheck.Execute;
      MSG_ABOUT: Mainform.actAboutBox.Execute;
      else Handled := False;
    end;
  end;
end;


procedure TMainForm.SetMainTab(Page: TTabSheet);
begin
  // Safely switch main tab
  if (Page <> nil) and (not FTreeRefreshInProgress) then begin
    PagecontrolMain.ActivePage := Page;
    PageControlMain.OnChange(Page);
  end;
end;


procedure TMainForm.SetTabCaption(PageIndex: Integer; Text: String);
var
  Tab: TQueryTab;
begin
  // The current tab can be closed already if we're here after CloseQueryTab()
  if PageIndex >= PageControlMain.PageCount then
    Exit;
  // Some cases pass -1 which triggers a "List index out of bounds" in below cast
  if PageIndex = -1 then
    Exit;
  // Special case if passed text is empty: Reset query tab caption to "Query #123"
  if (PageIndex = tabQuery.PageIndex) and (Text = '') then
    Text := 'Query';
  if IsQueryTab(PageIndex, False) then begin
    if Text = '' then begin
      for Tab in QueryTabs do begin
        if Tab.TabSheet = PageControlMain.Pages[PageIndex] then begin
          Text := 'Query #'+IntToStr(Tab.Number);
          break;
        end;
      end;
    end;
    // Leave space for close button on closable query tabs
    Text := Text + '      ';
  end;
  PageControlMain.Pages[PageIndex].Caption := Text;
  FixQueryTabCloseButtons;
end;


function TMainForm.ConfirmTabClose(PageIndex: Integer): Boolean;
var
  msg: String;
  Tab: TQueryTab;
begin
  Tab := QueryTabs[PageIndex-tabQuery.PageIndex];
  if (not Tab.Memo.Modified) or (not GetRegValue(REGNAME_PROMPTFILESAVE, DEFAULT_PROMPTFILESAVE)) then
    Result := True
  else begin
    // Unhide tabsheet so the user sees the memo content
    Tab.TabSheet.PageControl.ActivePage := Tab.TabSheet;
    if Tab.MemoFilename <> '' then
      msg := 'Save changes to file '+CRLF+CRLF+Tab.MemoFilename+' ?'
    else
      msg := 'Save content of tab "'+Trim(Tab.TabSheet.Caption)+'" ?';
    case MessageDlg(msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrNo: Result := True;
      mrYes: begin
        if Tab.MemoFilename <> '' then
          Tab.SaveQueryMemo(Tab.MemoFilename, False)
        else if SaveDialogSQLFile.Execute then
          Tab.SaveQueryMemo(SaveDialogSQLFile.FileName, False);
        // The save dialog can be cancelled.
        Result := not Tab.Memo.Modified;
      end;
      else Result := False;
    end;
  end;
end;


procedure TMainForm.actFilterPanelExecute(Sender: TObject);
var
  MakeVisible: Boolean;
begin
  // (De-)activate or focus filter panel
  MakeVisible := Sender <> btnCloseFilterPanel;
  pnlFilterVT.Visible := MakeVisible;
  pnlFilterVT.Tag := Integer(MakeVisible);
  // On startup, we cannot SetFocus, throws exceptons. Call with nil in that special case - see FormCreate
  if Assigned(Sender) and MakeVisible and editFilterVT.CanFocus then
    editFilterVT.SetFocus;
end;


procedure TMainForm.UpdateFilterPanel(Sender: TObject);
var
  tab: TTabSheet;
  f: String;
  FilterPanelVisible: Boolean;
begin
  // Called when active tab changes
  pnlFilterVT.Enabled := (PageControlMain.ActivePage <> tabEditor) or (ActiveObjectEditor is TfrmTableEditor);
  lblFilterVT.Enabled := pnlFilterVT.Enabled;
  editFilterVT.Enabled := pnlFilterVT.Enabled;
  lblFilterVTInfo.Enabled := pnlFilterVT.Enabled;
  if pnlFilterVT.Enabled then
    editFilterVT.Color := clWindow
  else
    editFilterVT.Color := clBtnFace;

  tab := PageControlMain.ActivePage;
  if tab = tabHost then
    tab := PageControlHost.ActivePage;
  FilterPanelVisible := pnlFilterVT.Tag = Integer(True);
  if not FilterPanelVisible then begin
    if editFilterVT.Text <> '' then
      editFilterVT.Text := ''
    else
      editFilterVTChange(Sender);
  end else begin
    if tab = tabVariables then f := FilterTextVariables
    else if tab = tabStatus then f := FilterTextStatus
    else if tab = tabProcesslist then f := FilterTextProcessList
    else if tab = tabCommandStats then f := FilterTextCommandStats
    else if tab = tabDatabase then f := FilterTextDatabase
    else if tab = tabEditor then f := FilterTextEditor
    else if tab = tabData then f := FilterTextData
    else if QueryTabActive and (ActiveQueryTab.ActiveResultTab <> nil) then f := ActiveQueryTab.ActiveResultTab.FilterText;
    if editFilterVT.Text <> f then
      editFilterVT.Text := f
    else
      editFilterVTChange(Sender);
  end;
end;


procedure TMainform.SetupSynEditors;
var
  i, j: Integer;
  Editors: TObjectList;
  BaseEditor, Editor: TSynMemo;
  FontName: String;
  FontSize, TabWidth: Integer;
  KeyStroke: TSynEditKeyStroke;
  ActiveLineColor: TColor;
  Attri: TSynHighlighterAttributes;
  Shortcut1, Shortcut2: TShortcut;

  procedure FindEditors(Comp: TComponent);
  var i: Integer;
  begin
    for i:=0 to Comp.ComponentCount-1 do begin
      if Comp.Components[i] is TSynMemo then
        Editors.Add(Comp.Components[i]);
      FindEditors(Comp.Components[i]);
    end;
  end;

begin
  // Restore font, highlighter and shortcuts for each instantiated TSynMemo
  Editors := TObjectList.Create;
  BaseEditor := SynMemoQuery;
  for i:=0 to QueryTabs.Count-1 do
    Editors.Add(QueryTabs[i].Memo);
  Editors.Add(SynMemoFilter);
  Editors.Add(SynMemoProcessView);
  Editors.Add(SynMemoSQLLog);
  if Assigned(ActiveObjectEditor) then
    FindEditors(ActiveObjectEditor);
  if Assigned(CreateDatabaseForm) then
    Editors.Add(CreateDatabaseForm.SynMemoPreview);
  if Assigned(OptionsForm) then
    Editors.Add(OptionsForm.SynMemoSQLSample);
  if Assigned(SQLHelpForm) then begin
    Editors.Add(SQLHelpForm.memoDescription);
    Editors.Add(SQLHelpForm.MemoExample);
  end;
  if Assigned(CopyTableDialog) then
    Editors.Add(CopyTableDialog.MemoFilter);

  FontName := GetRegValue(REGNAME_FONTNAME, DEFAULT_FONTNAME);
  FontSize := GetRegValue(REGNAME_FONTSIZE, DEFAULT_FONTSIZE);
  TabWidth := GetRegValue(REGNAME_TABWIDTH, DEFAULT_TABWIDTH);
  ActiveLineColor := StringToColor(GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));
  for i:=0 to Editors.Count-1 do begin
    Editor := Editors[i] as TSynMemo;
    Editor.Font.Name := FontName;
    Editor.Font.Size := FontSize;
    Editor.Gutter.Font.Name := FontName;
    Editor.Gutter.Font.Size := FontSize;
    Editor.Gutter.AutoSize := BaseEditor.Gutter.AutoSize;
    Editor.Gutter.DigitCount := BaseEditor.Gutter.DigitCount;
    Editor.Gutter.LeftOffset := BaseEditor.Gutter.LeftOffset;
    Editor.Gutter.RightOffset := BaseEditor.Gutter.RightOffset;
    Editor.Gutter.ShowLineNumbers := BaseEditor.Gutter.ShowLineNumbers;
    if Editor <> SynMemoSQLLog then
      Editor.WordWrap := actQueryWordWrap.Checked;
    Editor.ActiveLineColor := ActiveLineColor;
    Editor.Options := BaseEditor.Options;
    if Editor = SynMemoSQLLog then
      Editor.Options := Editor.Options + [eoRightMouseMovesCursor];
    Editor.TabWidth := TabWidth;
    Editor.MaxScrollWidth := BaseEditor.MaxScrollWidth;
    Editor.WantTabs := BaseEditor.WantTabs;
    Editor.OnPaintTransient := BaseEditor.OnPaintTransient;
    // Shortcuts
    if Editor = BaseEditor then for j:=0 to Editor.Keystrokes.Count-1 do begin
      KeyStroke := Editor.Keystrokes[j];
      Shortcut1 := GetRegValue(REGPREFIX_SHORTCUT1+EditorCommandToCodeString(Keystroke.Command), KeyStroke.ShortCut);
      Shortcut2 := GetRegValue(REGPREFIX_SHORTCUT2+EditorCommandToCodeString(Keystroke.Command), KeyStroke.ShortCut2);
      try
        Keystroke.ShortCut := Shortcut1;
        Keystroke.ShortCut2 := Shortcut2;
      except
        on E:ESynKeyError do begin
          LogSQL('Could not apply SynEdit keystroke shortcut "'+ShortCutToText(Shortcut1)+'"' +
            ' (or secondary: "'+ShortCutToText(Shortcut2)+'") to '+EditorCommandToCodeString(Keystroke.Command)+'. '+
            E.Message + '. Please go to Tools > Preferences > Shortcuts to change this settings.', lcError);
        end;
      end;
    end else
      Editor.Keystrokes := BaseEditor.KeyStrokes;
  end;
  // Highlighting
  for i:=0 to SynSQLSyn1.AttrCount - 1 do begin
    Attri := SynSQLSyn1.Attribute[i];
    Attri.Foreground := GetRegValue(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_FG, Attri.Foreground);
    Attri.Background := GetRegValue(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_BG, Attri.Background);
    Attri.IntegerStyle := GetRegValue(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_STYLE, Attri.IntegerStyle);
    if Assigned(OptionsForm) then
      OptionsForm.SynSQLSynSQLSample.Attribute[i].AssignColorAndStyle(Attri);
  end;
end;


procedure TMainForm.actReformatSQLExecute(Sender: TObject);
var
  m: TCustomSynEdit;
  CursorPosStart, CursorPosEnd: Integer;
  NewSQL: String;
begin
  // Reformat SQL query
  m := ActiveSynMemo;
  if not Assigned(m) then begin
    MessageDlg('Please select a non-readonly SQL editor first.', mtError, [mbOK], 0);
    Exit;
  end;
  CursorPosStart := m.SelStart;
  CursorPosEnd := m.SelEnd;
  if not m.SelAvail then
    m.SelectAll;
  NewSQL := m.SelText;
  if Length(NewSQL) = 0 then
    MessageDlg('Cannot reformat anything - your editor is empty.', mtError, [mbOK], 0)
  else begin
    Screen.Cursor := crHourglass;
    m.UndoList.AddGroupBreak;
    NewSQL := ReformatSQL(NewSQL);
    m.SelText := NewSQL;
    m.SelStart := CursorPosStart;
    if CursorPosEnd > CursorPosStart then
      m.SelEnd := CursorPosStart + Length(NewSQL);
    m.UndoList.AddGroupBreak;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.PageControlMainContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  ClickPoint: TPoint;
  TabsHeight: Integer;
begin
  // Activate tab popup menu only when clicked on tabs area.
  TabsHeight := (btnAddTab.Height+2) * PageControlMain.RowCount;
  if MousePos.Y <= TabsHeight then begin
    ClickPoint := PageControlMain.ClientToScreen(MousePos);
    popupMainTabs.Popup(ClickPoint.X, ClickPoint.Y);
    Handled := True;
  end else
    Handled := False;
end;


procedure TMainForm.menuQueryHelpersGenerateStatementClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  sql, Val, WhereClause: String;
  i, idx: Integer;
  ColumnNames, DefaultValues, KeyColumns: TStringList;
  Column: TTableColumn;
  Tree: TVirtualStringTree;
  Node: PVirtualNode;
begin
  // Generate INSERT, UPDATE or DELETE query using selected columns
  MenuItem := (Sender as TMenuItem);
  ColumnNames := TStringList.Create;
  DefaultValues := TStringList.Create;
  Tree := ActiveQueryHelpers;
  Node := Tree.GetFirstChild(FindNode(Tree, HELPERNODE_COLUMNS, nil));
  while Assigned(Node) do begin
    if Tree.Selected[Node] then begin
      ColumnNames.Add(QuoteIdent(Tree.Text[Node, 0], False));
      Column := SelectedTableColumns[Node.Index];
      case Column.DataType.Category of
        dtcInteger, dtcReal: Val := '0';
        dtcText, dtcIntegerNamed, dtcSetNamed: begin
          Val := esc(Column.DefaultText);
          if Column.DefaultType in [cdtNull, cdtNullUpdateTS] then
            Val := esc('')
          else
            Val := esc(Column.DefaultText);
        end;
        dtcTemporal: Val := 'NOW()';
        else Val := 'NULL';
      end;
      if Column.DefaultType = cdtAutoInc then
        Val := 'NULL';
      DefaultValues.Add(Val);
    end;
    Node := Tree.GetNextSibling(Node);
  end;
  KeyColumns := ActiveConnection.GetKeyColumns(SelectedTableColumns, SelectedTableKeys);
  WhereClause := '';
  for i:=0 to KeyColumns.Count-1 do begin
    idx := ColumnNames.IndexOf(QuoteIdent(KeyColumns[i], False));
    if idx > -1 then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + QuoteIdent(KeyColumns[i], False)+'='+DefaultValues[idx];
    end;
  end;

  if MenuItem = menuQueryHelpersGenerateInsert then begin
    sql := 'INSERT INTO '+QuoteIdent(ActiveDbObj.Name, False)+CRLF+
      #9'('+ImplodeStr(', ', ColumnNames)+')'+CRLF+
      #9'VALUES ('+ImplodeStr(', ', DefaultValues)+')';

  end else if MenuItem = menuQueryHelpersGenerateUpdate then begin
    sql := 'UPDATE '+QuoteIdent(ActiveDbObj.Name, False)+CRLF+#9'SET'+CRLF;
    if ColumnNames.Count > 0 then begin
      for i:=0 to ColumnNames.Count-1 do begin
        sql := sql + #9#9 + ColumnNames[i] + '=' + DefaultValues[i] + ',' + CRLF;
      end;
      Delete(sql, Length(sql)-2, 1);
    end else
      sql := sql + #9#9'??? # No column names selected!'+CRLF;
    sql := sql + #9'WHERE ' + WhereClause;

  end else if MenuItem = menuQueryHelpersGenerateDelete then begin
    sql := 'DELETE FROM '+QuoteIdent(ActiveDbObj.Name, False)+' WHERE ' + WhereClause;

  end;
  ActiveQueryMemo.UndoList.AddGroupBreak;
  ActiveQueryMemo.SelText := sql;
end;


procedure TMainForm.DBtreeAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  // Tree node filtering needs a hit in special cases, e.g. after a db was dropped
  comboDBFilter.OnChange(Sender);
end;


procedure TMainForm.DBtreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  VT: TVirtualStringTree;
begin
  // Resize "Size" column in dbtree to hold widest possible byte numbers without cutting text
  VT := Sender as TVirtualStringTree;
  if coVisible in VT.Header.Columns[1].Options then
    VT.Header.Columns[1].Width := TextWidth(VT.Canvas, FormatByteNumber(SIZE_MB-1))+VT.TextMargin*2;
end;


procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Control: TWinControl;
  VT: TBaseVirtualTree;
begin
  // Wheel scrolling only works in component which has focus. Help out by doing that by hand at least for any VirtualTree.
  // See http://www.delphipraxis.net/viewtopic.php?p=1113607
  // TODO: Does not work when a SynMemo has focus, probably related to the broken solution of this issue:
  // http://sourceforge.net/tracker/index.php?func=detail&aid=1574059&group_id=3221&atid=103221
  Control := FindVCLWindow(MousePos);
  if (Control is TBaseVirtualTree) and (not Control.Focused) and PtInRect(Control.ClientRect, Control.ScreenToClient(MousePos)) then begin
    VT := Control as TBaseVirtualTree;
    VT.OffsetY := VT.OffsetY + (WheelDelta div 2); // Don't know why, but WheelDelta is twice as big as it normally appears
    VT.UpdateScrollBars(True);
    Handled := True;
  end else
    Handled := False;
end;


procedure TMainForm.actDataResetSortingExecute(Sender: TObject);
begin
  SetLength(DataGridSortColumns, 0);
  InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
end;


procedure TMainForm.WMCopyData(var Msg: TWMCopyData);
var
  i: Integer;
  Connection: TMySQLConnection;
begin
  // Probably a second instance is posting its command line parameters here
  if (Msg.CopyDataStruct.dwData = SecondInstMsgId) and (SecondInstMsgId <> 0) then begin
    ParseCommandLineParameters(ParamBlobToStr(Msg.CopyDataStruct.lpData));
    for i:=0 to FCmdlineFilenames.Count-1 do begin
      actNewQueryTabExecute(self);
      if not QueryLoad(FCmdlineFilenames[i], True, nil) then
        actCloseQueryTabExecute(Self);
    end;
    if Assigned(FCmdlineConnectionParams) then
      InitConnection(FCmdlineConnectionParams, FCmdlineSessionName, True, Connection);
  end else
    // Not the right message id
    inherited;
end;


procedure TMainForm.DefaultHandler(var Message);
begin
  if TMessage(Message).Msg = SecondInstMsgId then begin
    // A second instance asked for our handle. Post that into its message queue.
    PostThreadMessage(TMessage(Message).WParam, SecondInstMsgId, Handle, 0);
  end else
    // Otherwise do what would happen without this overridden procedure
    inherited;
end;


procedure TMainForm.actBlobAsTextExecute(Sender: TObject);
begin
  // Activate displaying BLOBs as text data, ignoring possible weird effects in grid updates/inserts
  DataGrid.InvalidateChildren(nil, True);
end;


procedure TMainForm.vstScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
begin
  // A tree gets scrolled only when the mouse is over it - see FormMouseWheel
  // Our home brewn cell editors do not reposition when the underlying tree scrolls.
  // To avoid confusion, terminate editors then.
  if Sender.IsEditing then
    Sender.EndEditNode;
end;


procedure TMainForm.lblExplainProcessClick(Sender: TObject);
var
  Tab: TQueryTab;
begin
  // Click on "Explain" link label, in process viewer
  actNewQueryTabExecute(Sender);
  Tab := QueryTabs[QueryTabs.Count-1];
  Tab.Memo.Text := 'USE '+QuoteIdent(listProcesses.Text[listProcesses.FocusedNode, 3])+';'+CRLF+
    'EXPLAIN'+CRLF+SynMemoProcessView.Text;
  Tab.TabSheet.Show;
  actExecuteQueryExecute(Sender);
end;


procedure TMainForm.UpdateLineCharPanel;
var
  x, y: Int64;
  Grid: TVirtualStringTree;
  AppendMsg: String;
begin
  // Fill panel with "Line:Char"
  x := -1;
  y := -1;
  AppendMsg := '';
  Grid := ActiveGrid;
  if Assigned(Grid) and Grid.Focused then begin
    if Assigned(Grid.FocusedNode) then
      y := Grid.FocusedNode.Index+1;
    x := Grid.FocusedColumn+1;
  end else if QueryTabActive and ActiveQueryMemo.Focused then begin
    x := ActiveQueryMemo.CaretX;
    y := ActiveQueryMemo.CaretY;
    AppendMsg := ' ('+FormatByteNumber(ActiveQueryMemo.GetTextLen)+')';
  end;
  if (x > -1) and (y > -1) then begin
    ShowStatusMsg(FormatNumber(y)+' : '+FormatNumber(x) + AppendMsg, 1)
  end else
    ShowStatusMsg('', 1);
end;

procedure TMainForm.AnyGridStartOperation(Sender: TBaseVirtualTree; OperationKind: TVTOperationKind);
begin
  // Display status message on long running sort operations
  if OperationKind = okSortTree then
    ShowStatusMsg('Sorting grid nodes ...');
end;


procedure TMainForm.AnyGridEndOperation(Sender: TBaseVirtualTree; OperationKind: TVTOperationKind);
begin
  // Reset status message after long running operations
  if OperationKind = okSortTree then
    ShowStatusMsg;
end;


function TMainForm.GetEncodingByName(Name: String): TEncoding;
begin
  Result := nil;
  case FileEncodings.IndexOf(Name) of
    1: Result := TEncoding.Default;
    2: Result := TEncoding.ASCII;
    3: Result := TEncoding.Unicode;
    4: Result := TEncoding.BigEndianUnicode;
    5: Result := TEncoding.UTF8;
    6: Result := TEncoding.UTF7;
  end;
end;


function TMainForm.GetEncodingName(Encoding: TEncoding): String;
var
  idx: Integer;
begin
  if Encoding = TEncoding.Default then idx := 1
  else if Encoding = TEncoding.ASCII then idx := 2
  else if Encoding = TEncoding.Unicode then idx := 3
  else if Encoding = TEncoding.BigEndianUnicode then idx := 4
  else if Encoding = TEncoding.UTF8 then idx := 5
  else if Encoding = TEncoding.UTF7 then idx := 6
  else idx := 0;
  Result := FileEncodings[idx];
end;


function TMainForm.GetCharsetByEncoding(Encoding: TEncoding): String;
begin
  Result := '';
  if Encoding = TEncoding.Default then begin
    // Listing taken from http://forge.mysql.com/worklog/task.php?id=1349
    case GetACP of
      437: Result := 'cp850';
      850: Result := 'cp850';
      852: Result := 'cp852';
      858: Result := 'cp850';
      866: Result := 'cp866';
      874: Result := 'tis620';
      932: Result := 'cp932';
      936: Result := 'gbk';
      949: Result := 'euckr';
      959: Result := 'big5';
      1200: Result := 'utf16le';
      1201: Result := 'utf16';
      1250: Result := 'latin2';
      1251: Result := 'cp1251';
      1252: Result := 'latin1';
      1253: Result := 'greek';
      1254: Result := 'latin5';
      1255: Result := 'hebrew';
      1256: Result := 'cp1256';
      1257: Result := 'cp1257';
      10000: Result := 'macroman';
      10001: Result := 'sjis';
      10002: Result := 'big5';
      10008: Result := 'gb2312';
      10021: Result := 'tis620';
      10029: Result := 'macce';
      12001: Result := 'utf32';
      20107: Result := 'swe7';
      20127: Result := 'ascii';
      20866: Result := 'koi8r';
      20932: Result := 'ujis';
      20936: Result := 'gb2312';
      20949: Result := 'euckr';
      21866: Result := 'koi8u';
      28591: Result := 'latin1';
      28592: Result := 'latin2';
      28597: Result := 'greek';
      28598: Result := 'hebrew';
      28599: Result := 'latin5';
      28603: Result := 'latin7';
      28605: Result := 'latin9';
      38598: Result := 'hebrew';
      51932: Result := 'ujis';
      51936: Result := 'gb2312';
      51949: Result := 'euckr';
      51950: Result := 'big5';
      54936: Result := 'gb18030';
      65001: Result := 'utf8';
    end;
  end else if Encoding = TEncoding.ASCII then
    Result := 'ascii'
  else if Encoding = TEncoding.Unicode then
    Result := 'utf16le'
  else if Encoding = TEncoding.BigEndianUnicode then
    Result := 'utf16'
  else if Encoding = TEncoding.UTF8 then
    Result := 'utf8'
  else if Encoding = TEncoding.UTF7 then
    Result := 'utf7';
  // Auto-detection not supported here
end;


procedure TMainForm.treeQueryHelpersBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  Tab: TQueryTab;
begin
  // Paint green value bar in cell
  if (Node.Parent.Index=HELPERNODE_PROFILE)
    and (Column=1)
    and (Sender.GetNodeLevel(Node)=1)
    then begin
    Tab := ActiveQueryTab;
    Tab.QueryProfile.RecNo := Node.Index;
    PaintColorBar(MakeFloat(Tab.QueryProfile.Col(Column)), Tab.MaxProfileTime, TargetCanvas, CellRect);
  end;
end;


procedure TMainForm.treeQueryHelpersPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
begin
  // Paint text in datatype's color
  if (Node.Parent.Index=HELPERNODE_COLUMNS)
    and (Column=1)
    and (Sender.GetNodeLevel(Node)=1)
    and (ActiveDbObj.NodeType in [lntView, lntTable])
    then begin
    TargetCanvas.Font.Color := DatatypeCategories[Integer(SelectedTableColumns[Node.Index].DataType.Category)].Color;
  end;
end;


procedure TMainForm.treeQueryHelpersResize(Sender: TObject);
var
  Tree: TVirtualStringTree;
begin
  Tree := Sender as TVirtualStringTree;
  Tree.Header.Columns[1].Width := Min(Tree.Width div 3, 100);
end;


procedure TMainForm.treeQueryHelpersDblClick(Sender: TObject);
var
  m: TSynMemo;
begin
  m := ActiveQueryMemo;
  m.DragDrop(Sender, m.CaretX, m.CaretY);
end;


procedure TMainForm.treeQueryHelpersFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
var
  Tree: TVirtualStringTree;
begin
  // Disable multi selection when snippet node is focused
  Tree := Sender as TVirtualStringTree;
  if not Assigned(NewNode) then
    Exit;
  if (Tree.GetNodeLevel(NewNode) = 0) or
    (NewNode.Parent.Index=HELPERNODE_SNIPPETS)
    then begin
    Tree.ClearSelection;
    Tree.TreeOptions.SelectionOptions := Tree.TreeOptions.SelectionOptions - [toMultiSelect]
  end else
    Tree.TreeOptions.SelectionOptions := Tree.TreeOptions.SelectionOptions + [toMultiSelect];
end;


procedure TMainForm.treeQueryHelpersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  // Query helpers tree fetching node icon index
  if not (Kind in [ikNormal, ikSelected]) then
    Exit;
  if Column <> 0 then
    Exit;
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
         HELPERNODE_COLUMNS: if ActiveDbObj.NodeType <> lntNone then
              ImageIndex := ActiveDbObj.ImageIndex
            else
              ImageIndex := 14;
         HELPERNODE_FUNCTIONS: ImageIndex := 13;
         HELPERNODE_KEYWORDS: ImageIndex := 25;
         HELPERNODE_SNIPPETS: ImageIndex := 51;
         HELPERNODE_PROFILE: ImageIndex := 145;
       end;
    1: case Node.Parent.Index of
         HELPERNODE_COLUMNS: ImageIndex := 42;
         HELPERNODE_FUNCTIONS: ImageIndex := 13;
         HELPERNODE_KEYWORDS: ImageIndex := 25;
         HELPERNODE_SNIPPETS: ImageIndex := 68;
         HELPERNODE_PROFILE: ImageIndex := 145;
       end;
  end;
end;


procedure TMainForm.treeQueryHelpersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  // Query helpers tree fetching node text
  CellText := '';
  case Column of
    0: case Sender.GetNodeLevel(Node) of
        0: case Node.Index of
             HELPERNODE_COLUMNS: case ActiveDbObj.NodeType of
               lntNone: CellText := 'Columns';
               lntProcedure, lntFunction: CellText := 'Parameters in '+ActiveDbObj.Name;
               else CellText := 'Columns in '+ActiveDbObj.Name;
             end;
             HELPERNODE_FUNCTIONS: CellText := 'SQL Functions';
             HELPERNODE_KEYWORDS: CellText := 'SQL Keywords';
             HELPERNODE_SNIPPETS: CellText := 'Snippets';
             HELPERNODE_PROFILE: begin
                  CellText := 'Query profile';
                  if Assigned(ActiveQueryTab.QueryProfile) then
                    CellText := CellText + ' ('+FormatNumber(ActiveQueryTab.ProfileTime, 6)+'s)';
                end;
           end;
        1: case Node.Parent.Index of
             HELPERNODE_COLUMNS: case ActiveDbObj.NodeType of
               lntTable, lntView:
                 if SelectedTableColumns.Count > Integer(Node.Index) then
                   CellText := SelectedTableColumns[Node.Index].Name;
               lntFunction, lntProcedure:
                 if Assigned(ActiveObjectEditor) then
                   CellText := TfrmRoutineEditor(ActiveObjectEditor).Parameters[Node.Index].Name;
             end;
             HELPERNODE_FUNCTIONS: CellText := MySQLFunctions[Node.Index].Name;
             HELPERNODE_KEYWORDS: CellText := MySQLKeywords[Node.Index];
             HELPERNODE_SNIPPETS: CellText := FSnippetFilenames[Node.Index];
             HELPERNODE_PROFILE: begin
                  if Assigned(ActiveQueryTab.QueryProfile) then begin
                    ActiveQueryTab.QueryProfile.RecNo := Node.Index;
                    CellText := ActiveQueryTab.QueryProfile.Col(Column);
                  end;
                end;
           end;
      end;
    1: case Sender.GetNodeLevel(Node) of
        0: CellText := '';
        1: case Node.Parent.Index of
             HELPERNODE_COLUMNS:
               if (ActiveDbObj.NodeType in [lntTable, lntView]) and (SelectedTableColumns.Count > Integer(Node.Index)) then
                 CellText := SelectedTableColumns[Node.Index].DataType.Name;
             HELPERNODE_FUNCTIONS: CellText := MySQLFunctions[Node.Index].Declaration;
             HELPERNODE_PROFILE: begin
                  if Assigned(ActiveQueryTab.QueryProfile) then begin
                    ActiveQueryTab.QueryProfile.RecNo := Node.Index;
                    CellText := FormatNumber(ActiveQueryTab.QueryProfile.Col(Column))+'s';
                  end;
                end;
             else CellText := '';
           end;
      end;
  end;
end;


procedure TMainForm.treeQueryHelpersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // Query helpers tree asking if plus/minus button should be displayed
  if Sender.GetNodeLevel(Node) = 0 then begin
    Include(InitialStates, ivsHasChildren);
    if Node.Index = HELPERNODE_PROFILE then
      Node.CheckType := ctCheckbox;
  end;
end;


procedure TMainForm.treeQueryHelpersInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
         HELPERNODE_COLUMNS: case ActiveDbObj.NodeType of
           lntTable, lntView:
             ChildCount := SelectedTableColumns.Count;
           lntFunction, lntProcedure:
             if Assigned(ActiveObjectEditor) then
               ChildCount := TfrmRoutineEditor(ActiveObjectEditor).Parameters.Count
             else
               ChildCount := 0;
           else
             ChildCount := 0;
         end;
         HELPERNODE_FUNCTIONS: ChildCount := Length(MySQLFunctions);
         HELPERNODE_KEYWORDS: ChildCount := MySQLKeywords.Count;
         HELPERNODE_SNIPPETS: ChildCount := FSnippetFilenames.Count;
         HELPERNODE_PROFILE: if not Assigned(ActiveQueryTab.QueryProfile) then ChildCount := 0
            else ChildCount := ActiveQueryTab.QueryProfile.RecordCount;
       end;
    1: ChildCount := 0;
  end;
end;


procedure TMainForm.SetSnippetFilenames(Value: TStringList);
begin
  // Refreshing list of snippet file names needs to refresh helper node too
  if Assigned(FSnippetFilenames) then
    FSnippetFilenames.Free;
  FSnippetFilenames := Value;
  RefreshHelperNode(HELPERNODE_SNIPPETS);
end;



procedure TMainForm.treeQueryHelpersContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Tree: TVirtualStringTree;
begin
  Tree := Sender as TVirtualStringTree;
  menuQueryHelpersGenerateInsert.Enabled := False;
  menuQueryHelpersGenerateUpdate.Enabled := False;
  menuQueryHelpersGenerateDelete.Enabled := False;
  menuInsertSnippetAtCursor.Enabled := False;
  menuLoadSnippet.Enabled := False;
  menuDeleteSnippet.Enabled := False;
  menuExplore.Enabled := False;
  menuHelp.Enabled := False;

  case Tree.GetNodeLevel(Tree.FocusedNode) of
    0: ;
    1: case Tree.FocusedNode.Parent.Index of
      HELPERNODE_COLUMNS: if ActiveDbObj.NodeType in [lntTable, lntView] then begin
          menuQueryHelpersGenerateInsert.Enabled := True;
          menuQueryHelpersGenerateUpdate.Enabled := True;
          menuQueryHelpersGenerateDelete.Enabled := True;
        end;
      HELPERNODE_FUNCTIONS: menuHelp.Enabled := True;
      HELPERNODE_KEYWORDS: menuHelp.Enabled := True;
      HELPERNODE_SNIPPETS: begin
        menuDeleteSnippet.Enabled := True;
        menuInsertSnippetAtCursor.Enabled := True;
        menuLoadSnippet.Enabled := True;
        menuExplore.Enabled := True;
      end;
      HELPERNODE_PROFILE: begin // Query profile

      end;
    end;
  end;
end;


procedure TMainForm.RefreshHelperNode(NodeIndex: Cardinal);
var
  Tab: TQueryTab;
  Node: PVirtualNode;
begin
  if not Assigned(QueryTabs) then
    Exit;
  for Tab in QueryTabs do begin
    Node := FindNode(Tab.treeHelpers, NodeIndex, nil);
    if vsInitialized in Node.States then begin
      Node.States := Node.States - [vsInitialized];
      Tab.treeHelpers.InvalidateNode(Node);
    end;
  end;
end;


procedure TMainForm.ApplicationEvents1Deactivate(Sender: TObject);
begin
  // Force result tab balloon hint to disappear. Does not do so when mouse was moved too fast.
  tabsetQueryMouseLeave(Sender);
end;




{ TQueryTab }


constructor TQueryTab.Create;
begin
  // Creation of a new main query tab
  DirectoryWatch := TDirectoryWatch.Create;
  DirectoryWatch.WatchSubTree := False;
  DirectoryWatch.OnNotify := DirectoryWatchNotify;
  // Timer which postpones calling waModified event code until buffers have been saved
  MemofileModifiedTimer := TTimer.Create(Memo);
  MemofileModifiedTimer.Interval := 1000;
  MemofileModifiedTimer.Enabled := False;
  MemofileModifiedTimer.OnTimer := MemofileModifiedTimerNotify;
  LastSaveTime := 0;
end;


destructor TQueryTab.Destroy;
begin
  ResultTabs.Clear;
  DirectoryWatch.Free;
end;


function TQueryTab.GetActiveResultTab: TResultTab;
var
  idx: Integer;
begin
  Result := nil;
  idx := tabsetQuery.TabIndex;
  if (idx > -1) and (idx < ResultTabs.Count) then
    Result := ResultTabs[idx];
end;


procedure TQueryTab.DirectoryWatchNotify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
var
  IsCurrentFile: Boolean;
begin
  // Notification about file changes in loaded file's directory
  IsCurrentFile := DirectoryWatch.Directory + FileName = MemoFilename;
  case Action of
    waRemoved:
      if IsCurrentFile
        and (MessageDlg('File was deleted from outside:'+CRLF+'  '+MemoFilename+CRLF+'Close file and query tab?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes) then begin
        Mainform.actClearQueryEditor.Execute;
        if Mainform.IsQueryTab(TabSheet.PageIndex, False) then
          Mainform.CloseQueryTab(TabSheet.PageIndex);
      end;

    waModified:
      if IsCurrentFile and (LastSaveTime < GetTickCount-MemofileModifiedTimer.Interval) then begin
        MemofileModifiedTimer.Enabled := False;
        MemofileModifiedTimer.Enabled := True;
      end;

    waRenamedOld:
      if IsCurrentFile then
        MemoFileRenamed := True;

    waRenamedNew:
      if (not IsCurrentFile) and (MemoFilename <> '') and MemoFileRenamed then begin
        MemoFilename := DirectoryWatch.Directory + FileName;
        MemoFileRenamed := False;
      end;

  end;
end;


procedure TQueryTab.MemofileModifiedTimerNotify(Sender: TObject);
var
  OldTopLine: Integer;
  OldCursor: TBufferCoord;
begin
  (Sender as TTimer).Enabled := False;
  if MessageDlg('File was modified from outside:'+CRLF+'  '+MemoFilename+CRLF+'Reload it?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then begin
    OldCursor := Memo.CaretXY;
    OldTopLine := Memo.TopLine;
    Mainform.QueryLoad(MemoFilename, True, nil);
    Memo.CaretXY := OldCursor;
    Memo.TopLine := OldTopLine;
  end;
end;


procedure TQueryTab.SaveQueryMemo(Filename: String; OnlySelection: Boolean);
var
  Text, LB: String;
begin
  Screen.Cursor := crHourGlass;
  MainForm.ShowStatusMsg('Saving file ...');
  if OnlySelection then
    Text := Memo.SelText
  else
    Text := Memo.Text;
  LB := '';
  case MemoLineBreaks of
    lbsUnix: LB := LB_UNIX;
    lbsMac: LB := LB_MAC;
    lbsWide: LB := LB_WIDE;
  end;
  if LB <> '' then
    Text := StringReplace(Text, CRLF, LB, [rfReplaceAll]);
  SaveUnicodeFile( Filename, Text );
  MemoFilename := Filename;
  Memo.Modified := False;
  LastSaveTime := GetTickCount;
  MainForm.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


procedure TQueryTab.SetMemoFilename(Value: String);
begin
  FMemoFilename := Value;
  MainForm.SetTabCaption(TabSheet.PageIndex, sstr(ExtractFilename(FMemoFilename), 70));
  MainForm.ValidateQueryControls(Self);
  if FMemoFilename <> '' then begin
    DirectoryWatch.Directory := ExtractFilePath(FMemoFilename);
    DirectoryWatch.Start;
  end else
    DirectoryWatch.Stop;
end;




{ TResultTab }

constructor TResultTab.Create;
var
  QueryTab: TQueryTab;
  OrgGrid: TVirtualStringTree;
begin
  inherited;
  QueryTab := Mainform.ActiveQueryTab;
  OrgGrid := Mainform.QueryGrid;
  Grid := TVirtualStringTree.Create(QueryTab.TabSheet);
  Grid.Parent := QueryTab.TabSheet;
  Grid.Visible := False;
  Grid.Tag := OrgGrid.Tag;
  Grid.BorderStyle := OrgGrid.BorderStyle;
  Grid.Align := OrgGrid.Align;
  Grid.TreeOptions := OrgGrid.TreeOptions;
  Grid.PopupMenu := OrgGrid.PopupMenu;
  Grid.LineStyle := OrgGrid.LineStyle;
  Grid.EditDelay := OrgGrid.EditDelay;
  Grid.Font.Assign(OrgGrid.Font);
  Grid.Header.Options := OrgGrid.Header.Options;
  Grid.Header.ParentFont := OrgGrid.Header.ParentFont;
  Grid.Header.Images := OrgGrid.Header.Images;
  Grid.WantTabs := OrgGrid.WantTabs;
  Grid.AutoScrollDelay := OrgGrid.AutoScrollDelay;
  // Apply events - keep in alphabetical order for overview reasons
  Grid.OnAfterCellPaint := OrgGrid.OnAfterCellPaint;
  Grid.OnAfterPaint := OrgGrid.OnAfterPaint;
  Grid.OnBeforeCellPaint := OrgGrid.OnBeforeCellPaint;
  Grid.OnCreateEditor := OrgGrid.OnCreateEditor;
  Grid.OnCompareNodes := OrgGrid.OnCompareNodes;
  Grid.OnEditCancelled := OrgGrid.OnEditCancelled;
  Grid.OnEdited := OrgGrid.OnEdited;
  Grid.OnEditing := OrgGrid.OnEditing;
  Grid.OnEndOperation := OrgGrid.OnEndOperation;
  Grid.OnEnter := OrgGrid.OnEnter;
  Grid.OnExit := OrgGrid.OnExit;
  Grid.OnFocusChanged := OrgGrid.OnFocusChanged;
  Grid.OnFocusChanging := OrgGrid.OnFocusChanging;
  Grid.OnGetNodeDataSize := OrgGrid.OnGetNodeDataSize;
  Grid.OnGetText := OrgGrid.OnGetText;
  Grid.OnHeaderClick := OrgGrid.OnHeaderClick;
  Grid.OnInitNode := OrgGrid.OnInitNode;
  Grid.OnKeyDown := OrgGrid.OnKeyDown;
  Grid.OnMouseUp := OrgGrid.OnMouseUp;
  Grid.OnMouseWheel := OrgGrid.OnMouseWheel;
  Grid.OnNewText := OrgGrid.OnNewText;
  Grid.OnPaintText := OrgGrid.OnPaintText;
  Grid.OnStartOperation := OrgGrid.OnStartOperation;
  FixVT(Grid, Mainform.prefGridRowsLineCount);
end;

destructor TResultTab.Destroy;
begin
  Results.Free;
  Grid.EndEditNode;
  // The grid itself is owned by the parent tabsheet, free it only if the tabsheet is not being closed
  if not (csDestroying in Grid.ComponentState) then
    Grid.Free;
  inherited;
end;



end.

