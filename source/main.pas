unit Main;


// -------------------------------------
// Main-window
// -------------------------------------

interface

uses
  Windows, SysUtils, Classes, Graphics, GraphUtil, Forms, Controls, Menus, StdCtrls, Dialogs, Buttons,
  Messages, ExtCtrls, ComCtrls, StdActns, ActnList, ImgList, ToolWin, Clipbrd, SynMemo,
  SynEdit, SynEditTypes, SynEditKeyCmds, VirtualTrees, DateUtils,
  ShlObj, SynEditMiscClasses, SynEditSearch, SynEditRegexSearch, SynCompletionProposal, SynEditHighlighter,
  SynHighlighterSQL, Tabs, SynUnicode, SynRegExpr, ExtActns, IOUtils, Types, Themes, ComObj,
  CommCtrl, Contnrs, Generics.Collections, Generics.Defaults, SynEditExport, SynExportHTML, SynExportRTF, Math, ExtDlgs, Registry, AppEvnts,
  routine_editor, trigger_editor, event_editor, options, EditVar, apphelpers, createdatabase, table_editor,
  TableTools, View, Usermanager, SelectDBObject, connections, sqlhelp, dbconnection,
  insertfiles, searchreplace, loaddata, copytable, VirtualTrees.HeaderPopup, Cromis.DirectoryWatch, SyncDB, gnugettext,
  JumpList, System.Actions, System.UITypes, pngimage,
  System.ImageList, Vcl.Styles.UxTheme, Vcl.Styles.Utils.Menus, Vcl.Styles.Utils.Forms,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection, System.IniFiles, extra_controls;


type
  TBindParamItem = class(TObject)
    Parameter: String;
    Value: String;
    Keep: Boolean;
  end;

  TBindParam = class(TObjectList<TBindParamItem>)
    private
    public
      constructor Create();
      function FindParameter(Param: String) : Integer;
      procedure CleanToKeep;
  end;
  TBindParamItemComparer = class(TComparer<TBindParamItem>)
    function Compare(const Left, Right: TBindParamItem): Integer; override;
  end;

  TQueryTab = class;
  TResultTab = class(TObject)
    Results: TDBQuery;
    Grid: TVirtualStringTree;
    FilterText: String;
    public
      constructor Create(AOwner: TQueryTab);
      destructor Destroy; override;
  end;
  TResultTabs = TObjectList<TResultTab>;
  TQueryTab = class(TComponent)
    private
      FMemo: TSynMemo;
      FMemoFilename: String;
      FQueryRunning: Boolean;
      FLastChange: TDateTime;
      procedure SetMemo(Value: TSynMemo);
      procedure SetMemoFilename(Value: String);
      procedure SetQueryRunning(Value: Boolean);
      procedure TimerLastChangeOnTimer(Sender: TObject);
      procedure TimerStatusUpdateOnTimer(Sender: TObject);
      procedure MemoOnChange(Sender: TObject);
    public
      Number: Integer;
      Uid: String;
      ExecutionThread: TQueryThread;
      CloseButton: TSpeedButton;
      pnlMemo: TPanel;
      pnlHelpers: TPanel;
      filterHelpers: TButtonedEdit;
      treeHelpers: TVirtualStringTree;
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
      DoProfile: Boolean;
      QueryProfile: TDBQuery;
      ProfileTime, MaxProfileTime: Extended;
      LeftOffsetInMemo: Integer;
      HistoryDays: TStringList;
      ListBindParams: TBindParam;
      TimerLastChange: TTimer;
      TimerStatusUpdate: TTimer;
      function GetActiveResultTab: TResultTab;
      procedure DirectoryWatchNotify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
      procedure MemofileModifiedTimerNotify(Sender: TObject);
      function LoadContents(Filename: String; ReplaceContent: Boolean; Encoding: TEncoding): Boolean;
      function BindParamsActivated: Boolean;
      procedure SaveContents(Filename: String; OnlySelection: Boolean);
      procedure BackupUnsavedContent;
      property ActiveResultTab: TResultTab read GetActiveResultTab;
      property Memo: TSynMemo read FMemo write SetMemo;
      property MemoFilename: String read FMemoFilename write SetMemoFilename;
      function MemoBackupFilename: String;
      property QueryRunning: Boolean read FQueryRunning write SetQueryRunning;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      class function GenerateUid: String;
  end;

  TQueryHistoryItem = class(TObject)
    Time: TDateTime;
    Database: String;
    SQL: String;
    Duration: Cardinal;
    RegValue: Integer;
  end;
  TQueryHistory = class(TObjectList<TQueryHistoryItem>)
    private
      FMaxDuration: Cardinal;
    public
      constructor Create(SessionPath: String);
      property MaxDuration: Cardinal read FMaxDuration;
  end;
  TQueryHistoryItemComparer = class(TComparer<TQueryHistoryItem>)
    function Compare(const Left, Right: TQueryHistoryItem): Integer; override;
  end;

  TThreeStateBoolean = (nbUnset, nbFalse, nbTrue);

  ITaskbarList = interface(IUnknown)
    [SID_ITaskbarList]
    function HrInit: HRESULT; stdcall;
    function AddTab(hwnd: HWND): HRESULT; stdcall;
    function DeleteTab(hwnd: HWND): HRESULT; stdcall;
    function ActivateTab(hwnd: HWND): HRESULT; stdcall;
    function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
  end;
  ITaskbarList2 = interface(ITaskbarList)
    [SID_ITaskbarList2]
    function MarkFullscreenWindow(hwnd: HWND; fFullscreen: BOOL): HRESULT; stdcall;
  end;
  ITaskbarList3 = interface(ITaskbarList2)
    [SID_ITaskbarList3]
    function SetProgressValue(hwnd: HWND; ullCompleted: ULONGLONG; ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: Integer): HRESULT; stdcall;
    function RegisterTab(hwndTab: HWND; hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab: HWND; hwndInsertBefore: HWND): HRESULT; stdcall;
    function SetTabActive(hwndTab: HWND; hwndMDI: HWND; tbatFlags: Integer): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT; pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT; pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: HWND; hIcon: HICON; pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: HWND; var prcClip: TRect): HRESULT; stdcall;
  end;

  TMainForm = class(TExtForm)
    MainMenu1: TMainMenu;
    MainMenuFile: TMenuItem;
    FileNewItem: TMenuItem;
    MainMenuHelp: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    menuAbout: TMenuItem;
    MainMenuEdit: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    actCopy: TAction;
    actPaste: TAction;
    actNewWindow: TAction;
    actExitApplication: TAction;
    MainMenuTools: TMenuItem;
    FlushUserPrivileges1: TMenuItem;
    N5: TMenuItem;
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
    actPrintList: TAction;
    actCopyTable: TAction;
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
    actUndo: TEditUndo;
    ToolButton14: TToolButton;
    actExecuteQuery: TAction;
    actExecuteSelection: TAction;
    ExportSettings1: TMenuItem;
    Importsettings1: TMenuItem;
    menuSupportForum: TMenuItem;
    actExportData: TAction;
    actExecuteCurrentQuery: TAction;
    actDataPreview: TAction;
    actInsertFiles: TAction;
    actExportTables: TAction;
    actDropObjects: TAction;
    actLoadSQL: TAction;
    menuConnections: TPopupMenu;
    menuFeaturetracker: TMenuItem;
    menuDownload: TMenuItem;
    btnSQLHelp: TToolButton;
    menuSQLHelp1: TMenuItem;
    N8a: TMenuItem;
    tlbSep6: TToolButton;
    menuUpdateCheck: TMenuItem;
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
    actHelp: TAction;
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
    panelTop: TPanel;
    pnlLeft: TPanel;
    DBtree: TVirtualStringTree;
    spltDBtree: TSplitter;
    PageControlMain: TPageControl;
    tabData: TTabSheet;
    tabDatabase: TTabSheet;
    spltTopBottom: TSplitter;
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
    SynSQLSynUsed: TSynSQLSyn;
    SynMemoQuery: TSynMemo;
    spltQuery: TSplitter;
    TimerHostUptime: TTimer;
    N5a: TMenuItem;
    popupDataGrid: TPopupMenu;
    Refresh3: TMenuItem;
    N9a: TMenuItem;
    TimerConnected: TTimer;
    popupSqlLog: TPopupMenu;
    Clear2: TMenuItem;
    Copy1: TMenuItem;
    N15: TMenuItem;
    N17: TMenuItem;
    Copy3: TMenuItem;
    Paste2: TMenuItem;
    N4a: TMenuItem;
    DataGrid: TVirtualStringTree;
    QueryGrid: TVirtualStringTree;
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
    Exportdata2: TMenuItem;
    N11a: TMenuItem;
    DataInsertValue: TMenuItem;
    DataDateTime: TMenuItem;
    DataTime: TMenuItem;
    DataDate: TMenuItem;
    DataYear: TMenuItem;
    DataGUID: TMenuItem;
    ViewasHTML1: TMenuItem;
    InsertfilesintoBLOBfields3: TMenuItem;
    setNULL1: TMenuItem;
    menuExporttables: TMenuItem;
    popupListHeader: TVTHeaderPopupMenu;
    SynCompletionProposal: TSynCompletionProposal;
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
    spltProcessList: TSplitter;
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
    menuQueryHelpersGenerateSelect: TMenuItem;
    menuQueryHelpersGenerateInsert: TMenuItem;
    menuQueryHelpersGenerateUpdate: TMenuItem;
    menuQueryHelpersGenerateDelete: TMenuItem;
    actCreateTrigger: TAction;
    menuCreateTrigger: TMenuItem;
    menuQueryCut: TMenuItem;
    menuQuerySelectall: TMenuItem;
    actDataDuplicateRowWithoutKeys: TAction;
    actDataDuplicateRowWithKeys: TAction;
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
    MainMenuSearch: TMenuItem;
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
    DataUNIXtimestamp: TMenuItem;
    popupExecuteQuery: TPopupMenu;
    Run1: TMenuItem;
    RunSelection1: TMenuItem;
    Runcurrentquery1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    actDisconnect: TAction;
    Copylinetonewquerytab1: TMenuItem;
    menuLogHorizontalScrollbar: TMenuItem;
    actBatchInOneGo: TAction;
    Runbatchinonego1: TMenuItem;
    actSingleQueries: TAction;
    Sendqueriesonebyone1: TMenuItem;
    N3: TMenuItem;
    btnCancelOperation: TToolButton;
    actCancelOperation: TAction;
    actToggleComment: TAction;
    Uncomment1: TMenuItem;
    actSynchronizeDatabase: TAction;
    Disconnect1: TMenuItem;
    N4: TMenuItem;
    ImportCSVfile1: TMenuItem;
    InsertfilesintoTEXTBLOBfields1: TMenuItem;
    N9: TMenuItem;
    ExportdatabaseasSQL1: TMenuItem;
    Exportgridrows1: TMenuItem;
    QF20: TMenuItem;
    DataDefaultValue: TMenuItem;
    actLaunchCommandline: TAction;
    Launchcommandline1: TMenuItem;
    menuClearQueryHistory: TMenuItem;
    actGridEditFunction: TAction;
    InsertSQLfunction1: TMenuItem;
    menuGroupObjects: TMenuItem;
    actLogHorizontalScrollbar: TAction;
    actGroupObjects: TAction;
    lblExplainProcessAnalyzer: TLabel;
    menuExplainAnalyzer: TMenuItem;
    menuQueryExplain: TMenuItem;
    actExplainCurrentQuery: TAction;
    actExplainAnalyzeCurrentQuery: TAction;
    Explaincurrentquery1: TMenuItem;
    Explainanalyzerforcurrentquery1: TMenuItem;
    menuAutoExpand: TMenuItem;
    menuTreeOptions: TMenuItem;
    menuClearDataTabFilter: TMenuItem;
    actUnixTimestampColumn: TAction;
    LoadSQLfile2: TMenuItem;
    N2: TMenuItem;
    Save1: TMenuItem;
    Saveassnippet1: TMenuItem;
    ToolBarTree: TToolBar;
    editDatabaseFilter: TButtonedEdit;
    editTableFilter: TButtonedEdit;
    btnTreeFavorites: TToolButton;
    actFavoriteObjectsOnly: TAction;
    ToolBarMainButtons: TToolBar;
    actFavoriteObjectsOnly1: TMenuItem;
    Fullstatusrefresh1: TMenuItem;
    N10: TMenuItem;
    actFullRefresh: TAction;
    actPreviousResult: TAction;
    actNextResult: TAction;
    Previousresulttab1: TMenuItem;
    Nextresulttab1: TMenuItem;
    actSaveSynMemoToTextfile: TAction;
    DataGUIDwobraces: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    menuDoubleClickInsertsNodeText: TMenuItem;
    actRunSQL: TAction;
    RunSQLfiles1: TMenuItem;
    actPreferencesLogging: TAction;
    Loggingpreferences1: TMenuItem;
    Gridviewoptions1: TMenuItem;
    hisisaUNIXtimestampcolumn1: TMenuItem;
    ViewbinarydataastextinsteadofHEX2: TMenuItem;
    actPreferencesData: TAction;
    Datapreferences1: TMenuItem;
    actGotoDbTree: TAction;
    actGotoFilter: TAction;
    actGotoTab1: TAction;
    actGotoTab2: TAction;
    actGotoTab3: TAction;
    actGotoTab4: TAction;
    actGotoTab5: TAction;
    MainMenuGoto: TMenuItem;
    actGotoFilter1: TMenuItem;
    actGotoDbTree1: TMenuItem;
    actGotoTab11: TMenuItem;
    actGotoTab12: TMenuItem;
    actGotoTab31: TMenuItem;
    actGotoTab41: TMenuItem;
    actGotoTab51: TMenuItem;
    actCopyRows: TAction;
    Copyselectedrows1: TMenuItem;
    actClearQueryLog: TAction;
    ControlBarMain: TControlBar;
    ImageCollectionIcons8: TImageCollection;
    VirtualImageListMain: TVirtualImageList;
    ImageCollectionSilk: TImageCollection;
    pnlQueryHelpers: TPanel;
    treeQueryHelpers: TVirtualStringTree;
    filterQueryHelpers: TButtonedEdit;
    TimerStoreTabs: TTimer;
    Duplicaterowwithkeys1: TMenuItem;
    imgDonate: TImage;
    actGoToQueryResults: TAction;
    Switchtoqueryresults1: TMenuItem;
    actGoToDataMultiFilter: TAction;
    Datatabfilter1: TMenuItem;
    actDataOpenUrl: TAction;
    OpenURL1: TMenuItem;
    procedure actCreateDBObjectExecute(Sender: TObject);
    procedure menuConnectionsPopup(Sender: TObject);
    procedure actExitApplicationExecute(Sender: TObject);
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure CMStyleChanged(var Msg: TMessage); message CM_STYLECHANGED;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AfterFormCreate;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actUserManagerExecute(Sender: TObject);
    procedure actAboutBoxExecute(Sender: TObject);
    procedure actApplyFilterExecute(Sender: TObject);
    procedure actClearEditorExecute(Sender: TObject);
    procedure actTableToolsExecute(Sender: TObject);
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
    procedure actHelpExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actRemoveFilterExecute(Sender: TObject);
    procedure actSaveSQLExecute(Sender: TObject);
    procedure actSaveSQLAsExecute(Sender: TObject);
    procedure actSetDelimiterExecute(Sender: TObject);
    procedure actSQLhelpExecute(Sender: TObject);
    procedure actUpdateCheckExecute(Sender: TObject);
    procedure actWebbrowse(Sender: TObject);
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
    procedure PageControlMainChange(Sender: TObject);
    procedure PageControlMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PageControlHostChange(Sender: TObject);
    procedure ValidateControls(Sender: TObject);
    procedure ValidateQueryControls(Sender: TObject);
    procedure DataGridBeforePaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure LogSQL(Msg: String; Category: TDBLogCategory=lcInfo; Connection: TDBConnection=nil);
    procedure KillProcess(Sender: TObject);
    procedure SynMemoQueryStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure TimerHostUptimeTimer(Sender: TObject);
    procedure ListTablesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: String);
    procedure TimerConnectedTimer(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    procedure AutoRefreshSetInterval(Sender: TObject);
    procedure AutoRefreshToggle(Sender: TObject);
    procedure SynMemoQueryDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoQueryDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer; AFiles: TStrings);
    procedure popupHostPopup(Sender: TObject);
    procedure popupDBPopup(Sender: TObject);
    procedure popupDataGridPopup(Sender: TObject);
    procedure QFvaluesClick(Sender: TObject);
    procedure DataInsertValueClick(Sender: TObject);
    procedure InsertValue(Sender: TObject);
    procedure actDataSetNullExecute(Sender: TObject);
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
    procedure AnyGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure AnyGridCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure AnyGridHeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex;
        DropPosition: TPoint);
    procedure AnyGridIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: String;
      var Result: Integer);
    procedure SetMainTab(Page: TTabSheet);
    procedure DBtreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DBtreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var
        ImageIndex: TImageIndex);
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
    procedure AnyGridGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var
        HintText: String);
    procedure ListTablesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
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
    procedure MainMenuFileClick(Sender: TObject);
    procedure HostListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure HostListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure HostListBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure HostListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure ListTablesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure ListTablesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure ListTablesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ListTablesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ListTablesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure AnyGridAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
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
    function ActiveOrEmptyQueryTab(ConsiderActiveTab: Boolean): TQueryTab;
    function GetQueryTabByNumber(Number: Integer): TQueryTab;
    function GetQueryTabByHelpers(FindTree: TBaseVirtualTree): TQueryTab;
    function ActiveQueryMemo: TSynMemo;
    function ActiveQueryHelpers: TVirtualStringTree;
    function ActiveSynMemo(AcceptReadOnlyMemo: Boolean): TSynMemo;
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
    procedure AnyGridScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
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
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
    procedure treeQueryHelpersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure treeQueryHelpersInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure treeQueryHelpersInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure treeQueryHelpersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure treeQueryHelpersBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure treeQueryHelpersDblClick(Sender: TObject);
    procedure treeQueryHelpersContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure treeQueryHelpersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeQueryHelpersPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure treeQueryHelpersFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure treeQueryHelpersResize(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure menuEditObjectClick(Sender: TObject);
    procedure Copylinetonewquerytab1Click(Sender: TObject);
    procedure actLogHorizontalScrollbarExecute(Sender: TObject);
    procedure actBatchInOneGoExecute(Sender: TObject);
    function GetFocusedObjects(Sender: TObject; NodeTypes: TListNodeTypes): TDBObjectList;
    function DBTreeClicked(Sender: TObject): Boolean;
    procedure actCancelOperationExecute(Sender: TObject);
    procedure AnyGridChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure actToggleCommentExecute(Sender: TObject);
    procedure actSynchronizeDatabaseExecute(Sender: TObject);
    procedure DBtreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure actLaunchCommandlineExecute(Sender: TObject);
    procedure menuClearQueryHistoryClick(Sender: TObject);
    procedure actGridEditFunctionExecute(Sender: TObject);
    procedure ListVariablesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure DBtreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure actGroupObjectsExecute(Sender: TObject);
    procedure lblExplainProcessAnalyzerClick(Sender: TObject);
    procedure popupSqlLogPopup(Sender: TObject);
    procedure actExplainAnalyzeCurrentQueryExecute(Sender: TObject);
    procedure menuQueryExplainClick(Sender: TObject);
    procedure menuAutoExpandClick(Sender: TObject);
    procedure pnlLeftResize(Sender: TObject);
    procedure editDatabaseTableFilterChange(Sender: TObject);
    procedure editDatabaseTableFilterLeftButtonClick(Sender: TObject);
    procedure editDatabaseTableFilterMenuClick(Sender: TObject);
    procedure editDatabaseTableFilterExit(Sender: TObject);
    procedure menuClearDataTabFilterClick(Sender: TObject);
    procedure ListVariablesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure actUnixTimestampColumnExecute(Sender: TObject);
    procedure PopupQueryLoadPopup(Sender: TObject);
    procedure DonateClick(Sender: TObject);
    procedure DBtreeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ApplicationDeActivate(Sender: TObject);
    procedure ApplicationShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure DBtreeAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure actFavoriteObjectsOnlyExecute(Sender: TObject);
    procedure DBtreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actFullRefreshExecute(Sender: TObject);
    procedure treeQueryHelpersEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure treeQueryHelpersCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure treeQueryHelpersNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure treeQueryHelpersNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure treeQueryHelpersChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure actPreviousResultExecute(Sender: TObject);
    procedure actNextResultExecute(Sender: TObject);
    procedure actSaveSynMemoToTextfileExecute(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure buttonedEditClear(Sender: TObject);
    procedure menuDoubleClickInsertsNodeTextClick(Sender: TObject);
    procedure DBtreeDblClick(Sender: TObject);
    procedure editDatabaseTableFilterKeyPress(Sender: TObject; var Key: Char);
    procedure actGotoDbTreeExecute(Sender: TObject);
    procedure actGotoFilterExecute(Sender: TObject);
    procedure actGotoTabNumberExecute(Sender: TObject);
    procedure StatusBarClick(Sender: TObject);
    procedure SynMemoQueryMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SynMemoQueryKeyPress(Sender: TObject; var Key: Char);
    procedure filterQueryHelpersChange(Sender: TObject);
    procedure TimerStoreTabsTimer(Sender: TObject);
    procedure actGoToQueryResultsExecute(Sender: TObject);
    procedure actGoToDataMultiFilterExecute(Sender: TObject);
    procedure actDataOpenUrlExecute(Sender: TObject);
  private
    // Executable file details
    FAppVerMajor: Integer;
    FAppVerMinor: Integer;
    FAppVerRelease: Integer;
    FAppVerRevision: Integer;
    FAppVersion: String;

    FLastHintMousepos: TPoint;
    FLastHintControlIndex: Integer;
    FDelimiter: String;
    FLogToFile: Boolean;
    FFileNameSessionLog: String;
    FFileHandleSessionLog: Textfile;
    FLastMouseUpOnPageControl: Cardinal;
    FLastTabNumberOnMouseUp: Integer;
    FLastMouseDownCloseButton: TObject;
    FJumpList: TJumpList;
    // Filter text per tab for filter panel
    FFilterTextDatabases,
    FFilterTextEditor,
    FFilterTextVariables,
    FFilterTextStatus,
    FFilterTextProcessList,
    FFilterTextCommandStats,
    FFilterTextDatabase,
    FFilterTextData: String;
    FTreeRefreshInProgress: Boolean;
    FDataGridColumnWidthsCustomized: Boolean;
    FDataGridLastClickedColumnHeader: Integer;
    FDataGridLastClickedColumnLeftPos: Integer;
    FSnippetFilenames: TStringList;
    FConnections: TDBConnectionList;
    FTreeClickHistory: TNodeArray;
    FOperationTicker: Cardinal;
    FOperatingGrid: TBaseVirtualTree;
    FActiveDbObj: TDBObject;
    FActiveObjectGroup: TListNodeType;
    FIsWine: Boolean;
    FBtnAddTab: TSpeedButton;
    FDBObjectsMaxSize: Int64;
    FDBObjectsMaxRows: Int64;
    FSearchReplaceDialog: TfrmSearchReplace;
    FPreferencesDialog: Toptionsform;
    FCreateDatabaseDialog: TCreateDatabaseForm;
    FGridEditFunctionMode: Boolean;
    FClipboardHasNull: Boolean;
    FTimeZoneOffset: Integer;
    FGridCopying: Boolean;
    FGridPasting: Boolean;
    FHasDonatedDatabaseCheck: TThreeStateBoolean;
    FFocusedTables: TDBObjectList;
    FLastCaptionChange: Cardinal;
    FListTablesSorted: Boolean;
    FLastPortableSettingsSave: Cardinal;
    FLastAppSettingsWrites: Integer;
    FFormatSettings: TFormatSettings;

    // Host subtabs backend structures
    FHostListResults: TDBQueryList;
    FStatusServerUptime: Integer;
    FProcessListMaxTime: Int64;
    FCommandStatsQueryCount: Int64;
    FCommandStatsServerUptime: Integer;
    FVariableNames, FSessionVars, FGlobalVars: TStringList;

    procedure SetDelimiter(Value: String);
    procedure DisplayRowCountStats(Sender: TBaseVirtualTree);
    procedure insertFunction(Sender: TObject);
    function GetActiveConnection: TDBConnection;
    function GetActiveDatabase: String;
    function GetCurrentQuery(Tab: TQueryTab): String;
    procedure SetActiveDatabase(db: String; Connection: TDBConnection);
    procedure SetActiveDBObj(Obj: TDBObject);
    procedure ToggleFilterPanel(ForceVisible: Boolean = False);
    procedure AutoCalcColWidth(Tree: TVirtualStringTree; Column: TColumnIndex);
    procedure PlaceObjectEditor(Obj: TDBObject);
    procedure SetTabCaption(PageIndex: Integer; Text: String);
    function ConfirmTabClose(PageIndex: Integer; AppIsClosing: Boolean): Boolean;
    function ConfirmTabClear(PageIndex: Integer; AppIsClosing: Boolean): Boolean;
    procedure UpdateFilterPanel(Sender: TObject);
    procedure ConnectionReady(Connection: TDBConnection; Database: String);
    procedure DatabaseChanged(Connection: TDBConnection; Database: String);
    procedure ObjectnamesChanged(Connection: TDBConnection; Database: String);
    procedure UpdateLineCharPanel;
    procedure SetSnippetFilenames;
    function TreeClickHistoryPrevious(MayBeNil: Boolean=False): PVirtualNode;
    procedure OperationRunning(Runs: Boolean);
    function RunQueryFiles(Filenames: TStrings; Encoding: TEncoding; ForceRun: Boolean): Boolean;
    procedure RunQueryFile(Filename: String; Encoding: TEncoding);
    procedure SetLogToFile(Value: Boolean);
    procedure StoreLastSessions;
    function HandleUnixTimestampColumn(Sender: TBaseVirtualTree; Column: TColumnIndex): Boolean;
    function InitTabsIniFile: TIniFile;
    procedure StoreTabs;
    function RestoreTabs: Boolean;
  public
    QueryTabs: TObjectList<TQueryTab>;
    ActiveObjectEditor: TDBObjectEditor;
    FileEncodings: TStringList;
    ImportSettingsDone: Boolean;

    // Data grid related stuff
    DataGridHiddenColumns: TStringList;
    DataGridSortColumns: TOrderColArray;
    DataGridWantedRowCount: Int64;
    DataGridTable: TDBObject;
    DataGridFocusedCell: TStringList;
    DataGridFocusedNodeIndex: Int64;
    DataGridFocusedColumnName: String;
    DataGridResult: TDBQuery;
    DataGridFullRowMode: Boolean;
    DataLocalNumberFormat: Boolean;
    SelectedTableColumns: TTableColumnList;
    SelectedTableKeys: TTableKeyList;
    SelectedTableForeignKeys: TForeignKeyList;
    SelectedTableTimestampColumns: TStringList;
    FilterPanelManuallyOpened: Boolean;
    MatchingBraceForegroundColor,
    MatchingBraceBackgroundColor: TColor;

    // Task button interface
    TaskbarList: ITaskbarList;
    TaskbarList2: ITaskbarList2;
    TaskbarList3: ITaskbarList3;
    TaskbarList4: ITaskbarList4;

    property AppVerRevision: Integer read FAppVerRevision;
    property AppVersion: String read FAppVersion;
    property Connections: TDBConnectionList read FConnections;
    property Delimiter: String read FDelimiter write SetDelimiter;
    property IsWine: Boolean read FIsWine;
    property FocusedTables: TDBObjectList read FFocusedTables;
    procedure PaintColorBar(Value, Max: Extended; TargetCanvas: TCanvas; CellRect: TRect);
    procedure CallSQLHelpWithKeyword( keyword: String );
    procedure AddOrRemoveFromQueryLoadHistory(Filename: String; AddIt: Boolean; CheckIfFileExists: Boolean);
    procedure popupQueryLoadClick( sender: TObject );
    procedure PopupQueryLoadRemoveAbsentFiles(Sender: TObject);
    procedure PopupQueryLoadRemoveAllFiles(Sender: TObject);
    procedure SessionConnect(Sender: TObject);
    function InitConnection(Params: TConnectionParameters; ActivateMe: Boolean; var Connection: TDBConnection): Boolean;
    procedure ConnectionsNotify(Sender: TObject; const Item: TDBConnection; Action: TCollectionNotification);
    function ActiveGrid: TVirtualStringTree;
    function GridResult(Grid: TBaseVirtualTree): TDBQuery;
    property ActiveConnection: TDBConnection read GetActiveConnection;
    property ActiveDatabase: String read GetActiveDatabase;
    property ActiveDbObj: TDBObject read FActiveDbObj write SetActiveDBObj;
    property LogToFile: Boolean read FLogToFile write SetLogToFile;
    procedure RefreshTree(FocusNewObject: TDBObject=nil);
    function GetRootNode(Tree: TBaseVirtualTree; Connection: TDBConnection): PVirtualNode;
    function FindDBObjectNode(Tree: TBaseVirtualTree; Obj: TDBObject): PVirtualNode;
    function FindDBNode(Tree: TBaseVirtualTree; Connection: TDBConnection; db: String): PVirtualNode;
    procedure CalcNullColors;
    procedure HandleDataGridAttributes(RefreshingData: Boolean);
    function GetRegKeyTable: String;
    procedure SaveListSetup( List: TVirtualStringTree );
    procedure RestoreListSetup( List: TVirtualStringTree );
    procedure UpdateEditorTab;
    procedure SetWindowCaption;
    procedure DefaultHandler(var Message); override;
    procedure SetupSynEditors;
    function AnyGridEnsureFullRow(Grid: TVirtualStringTree; Node: PVirtualNode): Boolean;
    procedure DataGridEnsureFullRows(Grid: TVirtualStringTree; SelectedOnly: Boolean);
    function GetEncodingByName(Name: String): TEncoding;
    function GetEncodingName(Encoding: TEncoding): String;
    function GetCharsetByEncoding(Encoding: TEncoding): String;
    procedure RefreshHelperNode(NodeIndex: Cardinal);
    procedure BeforeQueryExecution(Thread: TQueryThread);
    procedure AfterQueryExecution(Thread: TQueryThread);
    procedure FinishedQueryExecution(Thread: TQueryThread);
    procedure EnableProgress(MaxValue: Integer);
    procedure DisableProgress;
    procedure SetProgressPosition(Value: Integer);
    procedure ProgressStep;
    procedure SetProgressState(State: TProgressbarState);
    procedure TaskDialogHyperLinkClicked(Sender: TObject);
    function HasDonated(ForceCheck: Boolean): TThreeStateBoolean;
    procedure ApplyVTFilter(FromTimer: Boolean);
    procedure ApplyFontToGrids;
    procedure PrepareImageList;
end;


var
  MainForm: TMainForm;
  SecondInstMsgId: UINT = 0;
  SysLanguage: String;
  MainFormCreated: Boolean = False;

const
  CheckedStates = [csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed];

{$I const.inc}


implementation

uses
  About, printlist, dbstructures, UpdateCheck,
  column_selection, data_sorting, grideditlinks, ExportGrid, jpeg, GIFImg;



{$R *.DFM}


procedure TMainForm.ShowStatusMsg(Msg: String=''; PanelNr: Integer=6);
var
  PanelRect: TRect;
begin
  // Show message in some statusbar panel
  if (PanelNr = 6) and (Msg = '') then
    Msg := _(SIdle);
  if Msg <> StatusBar.Panels[PanelNr].Text then begin
    StatusBar.Panels[PanelNr].Text := Msg;
    if PanelNr = 6 then begin
      // Immediately repaint this special panel, as it holds critical update messages,
      // while avoiding StatusBar.Repaint which refreshes all panels
      SendMessage(StatusBar.Handle, SB_GETRECT, PanelNr, Integer(@PanelRect));
      StatusBar.OnDrawPanel(StatusBar, StatusBar.Panels[PanelNr], PanelRect);
    end;
  end;
end;


procedure TMainForm.StatusBarClick(Sender: TObject);
var
  Click: TPoint;
  i, j: Integer;
  PanelRect: TRect;
  Infos: TStringList;
  InfoText: String;
begin
  // Handle click events on specific statusbar panels
  Click := StatusBar.ScreenToClient(Mouse.CursorPos);
  for i:=0 to StatusBar.Panels.Count-1 do begin
    SendMessage(StatusBar.Handle, SB_GETRECT, i, Integer(@PanelRect));
    if PtInRect(PanelRect, Click) then begin
      // We found the clicked panel
      case i of
        3: begin
          if ActiveConnection <> nil then begin
            Infos := ActiveConnection.ConnectionInfo;
            InfoText := '';
            for j:=0 to Infos.Count-1 do begin
              InfoText := InfoText + Infos.Names[j] + ': ' + Infos.ValueFromIndex[j] + CRLF;
            end;
            MessageDialog(Trim(InfoText), mtInformation, [mbOK]);
          end;
        end;
      end;
      Break;
    end;

  end;

end;

procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
var
  PanelRect: TRect;
  ImageIndex: Integer;
  Conn: TDBConnection;
begin
  // Refresh one status bar panel, probably with icon
  ImageIndex := -1;
  case Panel.Index of
    2: ImageIndex := 149;
    3: begin
      Conn := ActiveConnection;
      if Conn <> nil then
        ImageIndex := Conn.Parameters.ImageIndex;
    end;
    5: ImageIndex := 190;
    6: begin
      if Panel.Text = _(SIdle) then
        ImageIndex := 151 // Green dot
      else
        ImageIndex := 150; // Hourglass
    end;
  end;
  PanelRect := Rect;
  StatusBar.Canvas.FillRect(PanelRect);
  if ImageIndex > -1 then begin
    VirtualImageListMain.Draw(StatusBar.Canvas, PanelRect.Left, PanelRect.Top, ImageIndex, true);
    OffsetRect(PanelRect, VirtualImageListMain.Width+2, 0);
  end;
  DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, PanelRect, DT_SINGLELINE or DT_VCENTER);
end;


procedure TMainForm.StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  MouseP: TPoint;
  Bar: TStatusBar;
  PanelRect: TRect;
  i: Integer;
  Infos: TStringList;
  HintText: String;
begin
  // Display various server, client and connection related details in a hint
  if (FLastHintMousepos.X = X) and (FLastHintMousepos.Y = Y) then
    Exit;
  FLastHintMousepos := Point(X, Y);
  MouseP := StatusBar.ClientOrigin;
  Inc(MouseP.X, X);
  Inc(MouseP.Y, Y);
  Bar := Sender as TStatusBar;
  for i:=0 to Bar.Panels.Count-1 do begin
    SendMessage(Bar.Handle, SB_GETRECT, i, Integer(@PanelRect));
    if PtInRect(PanelRect, FLastHintMousepos) then
      break;
  end;
  if i = FLastHintControlIndex then
    Exit;
  FLastHintControlIndex := i;
  if FLastHintControlIndex = 3 then begin
    if ActiveConnection <> nil then begin
      Infos := ActiveConnection.ConnectionInfo;
      HintText := '';
      for i:=0 to Infos.Count-1 do begin
        HintText := HintText + Infos.Names[i] + ': ' + sstr(Infos.ValueFromIndex[i], 200) + CRLF;
      end;
      BalloonHint1.Description := Trim(HintText);
      OffsetRect(PanelRect, Bar.ClientOrigin.X, Bar.ClientOrigin.Y);
      BalloonHint1.ShowHint(PanelRect);
    end;
  end else
    Bar.OnMouseLeave(Sender);
end;


procedure TMainForm.StatusBarMouseLeave(Sender: TObject);
begin
  BalloonHint1.HideHint;
  FLastHintControlIndex := -1;
end;


procedure TMainForm.actExitApplicationExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.actFlushExecute(Sender: TObject);
var
  FlushWhat: String;
begin
  if Sender = actFlushHosts then
    FlushWhat := 'HOSTS'
  else if Sender = actFlushLogs then
    FlushWhat := 'LOGS'
  else if Sender = actFlushPrivileges then
    FlushWhat := 'PRIVILEGES'
  else if Sender = actFlushTables then
    FlushWhat := 'TABLES'
  else if Sender = actFlushTableswithreadlock then
    FlushWhat := 'TABLES WITH READ LOCK'
  else if Sender = actFlushStatus then
    FlushWhat := 'STATUS'
  else
    raise Exception.CreateFmt(_('Unhandled sender control: %s'), [(Sender as TControl).Name]);
  try
    ActiveConnection.Query('FLUSH ' + FlushWhat);
    if Sender = actFlushTableswithreadlock then begin
      MessageDialog(
        _('Tables have been flushed and read lock acquired. Perform backup or snapshot of table data files now. Press OK to unlock when done...'),
        mtInformation, [mbOk]
      );
      ActiveConnection.Query('UNLOCK TABLES');
    end;
  except
    on E:EDbError do
      ErrorDialog(E.Message);
  end;
end;


procedure TMainForm.actFullRefreshExecute(Sender: TObject);
var
  OldFullTableStatusSetting: Boolean;
  Conn: TDBConnection;
begin
  // Temorarily enable full table status when it's disabled
  Conn := ActiveConnection;
  OldFullTableStatusSetting := Conn.Parameters.FullTableStatus;
  Conn.Parameters.FullTableStatus := True;
  actRefresh.Execute;
  Conn.Parameters.FullTableStatus := OldFullTableStatusSetting;
end;


procedure TMainForm.actGotoDbTreeExecute(Sender: TObject);
begin
  DBtree.SetFocus;
end;


procedure TMainForm.actGotoFilterExecute(Sender: TObject);
begin
  editTableFilter.SetFocus;
end;


procedure TMainForm.actGoToQueryResultsExecute(Sender: TObject);
var
  Tab: TQueryTab;
  Grid: TVirtualStringTree;
begin
  if QueryTabActive then begin
    // Switch between query editor and result grid
    Tab := ActiveQueryTab;
    if Tab.Memo.Focused then begin
      if Tab.ActiveResultTab <> nil then begin
        Grid := Tab.ActiveResultTab.Grid;
        Grid.SetFocus;
        if Grid.FocusedNode = nil then
          SelectNode(Grid, 0);
      end else begin
        MessageBeep(MB_ICONASTERISK);
      end;
    end else begin
      Tab.Memo.SetFocus;
    end;
  end else if PageControlMain.ActivePage = tabData then begin
    // Switch between data tab filter and result grid
    if SynMemoFilter.Focused then begin
      DataGrid.SetFocus;
      if DataGrid.FocusedNode = nil then
        SelectNode(DataGrid, 0);
    end else begin
      ToggleFilterPanel(True);
      SynMemoFilter.SetFocus;
    end;
  end else begin
    MessageBeep(MB_ICONASTERISK);
  end;
end;


procedure TMainForm.actGoToDataMultiFilterExecute(Sender: TObject);
begin
  // Go to multi column filter generator
  if PageControlMain.ActivePage = tabData then begin
    ToggleFilterPanel(True);
    editFilterSearch.SetFocus;
  end else begin
    MessageBeep(MB_ICONASTERISK);
  end;
end;


procedure TMainForm.actGotoTabNumberExecute(Sender: TObject);
var
  i, Visibles, WantedIndex: Integer;
begin
  // Set focus on tab by numeric index
  WantedIndex := -1;
  if Sender = actGotoTab1 then
    WantedIndex := 0
  else if Sender = actGotoTab2 then
    WantedIndex := 1
  else if Sender = actGotoTab3 then
    WantedIndex := 2
  else if Sender = actGotoTab4 then
    WantedIndex := 3
  else if Sender = actGotoTab5 then
    WantedIndex := 4;
  i := 0;
  Visibles := 0;
  while true do begin
    if i >= PageControlMain.PageCount then
      Break;
    if PageControlMain.Pages[i].TabVisible then begin
      if Visibles = WantedIndex then begin
        PageControlMain.ActivePageIndex := i;
        Break;
      end;
      Inc(Visibles);
    end;
    Inc(i);
  end;
end;


procedure TMainForm.actGridEditFunctionExecute(Sender: TObject);
begin
  // Insert SQL function in grid
  FGridEditFunctionMode := True;
  ActiveGrid.EditNode(ActiveGrid.FocusedNode, ActiveGrid.FocusedColumn);
end;


procedure TMainForm.StoreLastSessions;
var
  OpenSessions, SessionPaths, SortedSessions: TStringList;
  Connection: TDBConnection;
  JumpTask: TJumpTask;
  SessionPath: String;
  i: Integer;
  LastConnect, DummyDate: TDateTime;
begin
  // Store names of open sessions
  OpenSessions := TStringList.Create;
  for Connection in Connections do
    OpenSessions.Add(Connection.Parameters.SessionPath);
  AppSettings.WriteString(asLastSessions, ImplodeStr(DELIM, OpenSessions));
  OpenSessions.Free;
  if Assigned(ActiveConnection) then
    AppSettings.WriteString(asLastActiveSession, ActiveConnection.Parameters.SessionPath);

  // Recreate Win7 taskbar jump list with sessions used in the last month, ordered by the number of connects
  if Assigned(FJumpList) then try
    FJumpList.Clear;
    SessionPaths := TStringList.Create;
    SortedSessions := TStringList.Create;
    AppSettings.GetSessionPaths('', SessionPaths);
    for SessionPath in SessionPaths do begin
      AppSettings.SessionPath := SessionPath;
      DummyDate := StrToDateTime('2000-01-01');
      LastConnect := StrToDateTimeDef(AppSettings.ReadString(asLastConnect), DummyDate);
      if DaysBetween(LastConnect, Now) <= 30 then
        SortedSessions.Values[SessionPath] := IntToStr(AppSettings.ReadInt(asConnectCount));
    end;
    SessionPaths.Free;
    AppSettings.ResetPath;
    SortedSessions.CustomSort(StringListCompareByValue);
    for i:=0 to SortedSessions.Count-1 do begin
      JumpTask := TJumpTask.Create;
      JumpTask.Title := SortedSessions.Names[i]+' ('+FormatNumber(SortedSessions.ValueFromIndex[i], True)+')';
      JumpTask.ApplicationPath := ParamStr(0);
      JumpTask.Arguments := '-d="'+SortedSessions.Names[i]+'"';
      JumpTask.CustomCategory := _('Recent sessions');
      FJumpList.JumpItems.Add(JumpTask);
    end;
    SortedSessions.Free;
    // Seems to randomly produce access violations, not only on Wine
    // See issue #3428
    FJumpList.Apply;
  except
    on E:Exception do
      LogSQL(E.Message, lcError);
  end;
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;
begin
  // Prompt on modified changes
  CanClose := True;

  // Unsaved changes in some query tab?
  // Also backups automatically if option is activated
  for i:=0 to QueryTabs.Count-1 do begin
    CanClose := ConfirmTabClose(i+tabQuery.PageIndex, True);
    if not CanClose then
      Exit;
  end;
  // Unsaved modified table, trigger, view or routine?
  if Assigned(ActiveObjectEditor) then
    CanClose := not (ActiveObjectEditor.DeInit in [mrAbort, mrCancel]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Destroy dialogs
  FreeAndNil(FSearchReplaceDialog);

  StoreLastSessions;

  // Store tab setup for the last time, before tabs are destroyed
  if TimerStoreTabs.Enabled then begin
    TimerStoreTabs.OnTimer(Sender);
  end;

  // Some grid editors access the registry - be sure these are gone before freeing AppSettings
  QueryTabs.Clear;
  DataGrid.EndEditNode;

  // Clearing query and browse data.
  FreeAndNil(DataGridResult);

  // Close database connections
  Connections.Clear;

  // Save various settings
  AppSettings.WriteBool(asStopOnErrorsInBatchMode, actQueryStopOnErrors.Checked);
  AppSettings.WriteBool(asDisplayBLOBsAsText, actBlobAsText.Checked);
  AppSettings.WriteString(asDelimiter, FDelimiter);
  AppSettings.WriteInt(asQuerymemoheight, pnlQueryMemo.Height);
  AppSettings.WriteInt(asQueryhelperswidth, pnlQueryHelpers.Width);
  AppSettings.WriteInt(asCompletionProposalWidth, SynCompletionProposal.Width);
  AppSettings.WriteInt(asCompletionProposalNbLinesInWindow, SynCompletionProposal.NbLinesInWindow);
  AppSettings.WriteInt(asDbtreewidth, pnlLeft.width);
  AppSettings.WriteBool(asGroupTreeObjects, actGroupObjects.Checked);
  AppSettings.WriteInt(asDataPreviewHeight, pnlPreview.Height);
  AppSettings.WriteBool(asDataPreviewEnabled, actDataPreview.Checked);
  AppSettings.WriteInt(asLogHeight, SynMemoSQLLog.Height);
  AppSettings.WriteBool(asFilterPanel, actFilterPanel.Checked);
  AppSettings.WriteBool(asWrapLongLines, actQueryWordWrap.Checked);
  AppSettings.WriteBool(asSingleQueries, actSingleQueries.Checked);
  AppSettings.WriteBool(asLogHorizontalScrollbar, actLogHorizontalScrollbar.Checked);
  AppSettings.WriteBool(asMainWinMaximized, WindowState=wsMaximized);
  AppSettings.WriteInt(asMainWinOnMonitor, Monitor.MonitorNum);
  // Window dimensions are only valid when WindowState is normal.
  if WindowState = wsNormal then begin
    AppSettings.WriteInt(asMainWinLeft, Left);
    AppSettings.WriteInt(asMainWinTop, Top);
    AppSettings.WriteInt(asMainWinWidth, Width);
    AppSettings.WriteInt(asMainWinHeight, Height);
  end;
  SaveListSetup(ListDatabases);
  SaveListSetup(ListVariables);
  SaveListSetup(ListStatus);
  SaveListSetup(ListProcesses);
  SaveListSetup(ListCommandStats);
  SaveListSetup(ListTables);

  LogToFile := False;
  AppSettings.Free;
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
  i, j, MonitorIndex: Integer;
  QueryTab: TQueryTab;
  Action: TAction;
  dwInfoSize,           // Size of VERSIONINFO structure
  dwVerSize,            // Size of Version Info Data
  dwWnd: DWORD;         // Handle for the size call.
  FI: PVSFixedFileInfo; // Delphi structure; see WINDOWS.PAS
  ptrVerBuf: Pointer;
  FunctionCategories: TStringList;
  miGroup, miFilterGroup, miFunction, miFilterFunction: TMenuItem;
  NTHandle: THandle;
  TZI: TTimeZoneInformation;
  wine_nt_to_unix_file_name: procedure(p1:pointer; p2:pointer); stdcall;
  OldSnippetsDir, CurrentSnippetsDir, TargetSnippet: String;
  Files: TStringDynArray;
  dti: TDBDatatypeCategoryIndex;
begin
  caption := APPNAME;

  // Load preferred ImageCollection into VirtualImageList
  PrepareImageList;

  if AppSettings.ReadBool(asToolbarShowCaptions) then begin
    for i:=0 to ToolBarMainButtons.ButtonCount-1 do begin
      if ToolBarMainButtons.Buttons[i].Style = tbsSeparator then
        Continue;
      ToolBarMainButtons.Buttons[i].AutoSize := True;
      //ToolBarMainButtons.Buttons[i].AutoSize := False;
      //ToolBarMainButtons.Buttons[i].Width := 25;
      ToolBarMainButtons.Buttons[i].Style := tbsTextButton;
    end;
    //ToolBarMainButtons.AllowTextButtons := True;
    //ToolBarMainButtons.List := True;
    ToolBarMainButtons.ShowCaptions := true;
  end;

  // Translate menu items
  menuQueryHelpersGenerateSelect.Caption := f_('Generate %s ...', ['SELECT']);
  menuQueryHelpersGenerateInsert.Caption := f_('Generate %s ...', ['INSERT']);
  menuQueryHelpersGenerateUpdate.Caption := f_('Generate %s ...', ['UPDATE']);
  menuQueryHelpersGenerateDelete.Caption := f_('Generate %s ...', ['DELETE']);

  // Translate data type categories
  for dti:=Low(DatatypeCategories) to High(DatatypeCategories) do begin
    DatatypeCategories[dti].Name := _(DatatypeCategories[dti].Name);
  end;

  // Detect version
  dwInfoSize := GetFileVersionInfoSize(PChar(Application.ExeName), dwWnd);
  GetMem(ptrVerBuf, dwInfoSize);
  GetFileVersionInfo(PChar(Application.ExeName), dwWnd, dwInfoSize, ptrVerBuf);
  VerQueryValue(ptrVerBuf, '\', Pointer(FI), dwVerSize );
  FAppVerMajor := HiWord(FI.dwFileVersionMS);
  FAppVerMinor := LoWord(FI.dwFileVersionMS);
  FAppVerRelease := HiWord(FI.dwFileVersionLS);
  FAppVerRevision := LoWord(FI.dwFileVersionLS);
  FAppVersion := Format('%d.%d.%d.%d', [FAppVerMajor, FAppVerMinor, FAppVerRelease, FAppVerRevision]);
  FreeMem(ptrVerBuf);

  // Detect if we're running on Wine, not on native Windows
  // Idea taken from http://ruminatedrumblings.blogspot.com/2008/04/detecting-virtualized-environment.html
  NTHandle := LoadLibrary('NTDLL.DLL');
  if NTHandle>32 then
    wine_nt_to_unix_file_name := GetProcAddress(NTHandle, 'wine_nt_to_unix_file_name')
  else
    wine_nt_to_unix_file_name := nil;
  FIsWine := Assigned(wine_nt_to_unix_file_name);
  FreeLibrary(NTHandle);

  // Taskbar button interface for Windows 7
  // Possibly fails. See http://www.heidisql.com/forum.php?t=22451
  if CheckWin32Version(6, 1) then
  try
    TaskbarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList;
    TaskbarList.HrInit;
    Supports(TaskbarList, IID_ITaskbarList2, TaskbarList2);
    Supports(TaskbarList, IID_ITaskbarList3, TaskbarList3);
    Supports(TaskbarList, IID_ITaskbarList4, TaskbarList4);
  except
    on E:EOleSysError do;
  end;

  // Move files from old default snippets directory, see issue #159
  if not AppSettings.PortableMode then begin
    // This was the default folder up to r5244 / 8b2966c52efb685b00189037a0507157ed03a368
    OldSnippetsDir := GetShellFolder(CSIDL_COMMON_APPDATA) + '\' + APPNAME + '\Snippets\';
    CurrentSnippetsDir := AppSettings.DirnameSnippets;
    //showmessage('1:'+crlf+OldSnippetsDir+crlf+CurrentSnippetsDir);
    if (not OldSnippetsDir.IsEmpty) and (not CurrentSnippetsDir.IsEmpty)
        and DirectoryExists(OldSnippetsDir) and DirectoryExists(CurrentSnippetsDir)
        and (CompareText(OldSnippetsDir, CurrentSnippetsDir) <> 0)
        then begin
      Files := TDirectory.GetFiles(OldSnippetsDir, '*.sql');
      if Length(Files) > 0 then begin
        LogSQL(f_('Migrating snippet files to new folder: %s', [CurrentSnippetsDir]));
        for i:=Low(Files) to High(Files) do begin
          TargetSnippet := CurrentSnippetsDir + ExtractFileName(Files[i]);
          if MoveFile(PChar(Files[i]), PChar(TargetSnippet)) then begin
            LogSQL(f_('Successfully moved "%s" to "%s"', [Files[i], TargetSnippet]));
          end else begin
            LogSQL(f_('Error: Could not move "%s" to "%s" (Error: %s)', [Files[i], TargetSnippet, SysErrorMessage(GetLastError())]), lcError);
          end;
        end;
      end;
    end;
  end;

  // Load snippet filenames
  SetSnippetFilenames;

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

  Delimiter := AppSettings.ReadString(asDelimiter);

  InheritFont(SynCompletionProposal.Font);

  // Define static query tab as first one in our QueryTabs list
  QueryTab := TQueryTab.Create(Self);
  QueryTab.TabSheet := tabQuery;
  QueryTab.Number := 1;
  QueryTab.Uid := TQueryTab.GenerateUid;
  QueryTab.pnlMemo := pnlQueryMemo;
  QueryTab.pnlHelpers := pnlQueryHelpers;
  QueryTab.filterHelpers := filterQueryHelpers;
  QueryTab.treeHelpers := treeQueryHelpers;
  QueryTab.Memo := SynMemoQuery;
  QueryTab.MemoLineBreaks := lbsNone;
  QueryTab.spltHelpers := spltQueryHelpers;
  QueryTab.spltQuery := spltQuery;
  QueryTab.tabsetQuery := tabsetQuery;
  InheritFont(QueryTab.tabsetQuery.Font);
  QueryTab.ResultTabs := TResultTabs.Create(True);

  QueryTabs := TObjectList<TQueryTab>.Create(True);
  QueryTabs.Add(QueryTab);

  // Populate generic results for "Host" subtabs
  FHostListResults := TDBQueryList.Create(False);
  for i:=0 to PageControlHost.PageCount-1 do begin
    FHostListResults.Add(nil);
  end;

  // Enable auto completion in data tab, filter editor
  SynCompletionProposal.AddEditor(SynMemoFilter);

  // Fix node height on Virtual Trees for current DPI settings
  FixVT(DBTree);
  FixVT(ListDatabases);
  FixVT(ListVariables);
  FixVT(ListStatus);
  FixVT(ListProcesses);
  FixVT(ListCommandStats);
  FixVT(ListTables);
  FixVT(treeQueryHelpers);

  // Window position
  Left := AppSettings.ReadInt(asMainWinLeft);
  Top := AppSettings.ReadInt(asMainWinTop);
  // .. dimensions
  Width := AppSettings.ReadInt(asMainWinWidth);
  Height := AppSettings.ReadInt(asMainWinHeight);
  // ... state
  if AppSettings.ReadBool(asMainWinMaximized) then
    WindowState := wsMaximized;
  // ... and monitor placement
  MonitorIndex := AppSettings.ReadInt(asMainWinOnMonitor);
  MonitorIndex := Max(0, MonitorIndex);
  MonitorIndex := Min(Screen.MonitorCount-1, MonitorIndex);
  MakeFullyVisible(Screen.Monitors[MonitorIndex]);

  actQueryStopOnErrors.Checked := AppSettings.ReadBool(asStopOnErrorsInBatchMode);
  actBlobAsText.Checked := AppSettings.ReadBool(asDisplayBLOBsAsText);
  actQueryWordWrap.Checked := AppSettings.ReadBool(asWrapLongLines);
  actSingleQueries.Checked := AppSettings.ReadBool(asSingleQueries);
  actBatchInOneGo.Checked := not AppSettings.ReadBool(asSingleQueries);
  actPreferencesLogging.ImageIndex := actPreferences.ImageIndex;
  actPreferencesLogging.OnExecute := actPreferences.OnExecute;
  actPreferencesData.ImageIndex := actPreferences.ImageIndex;
  actPreferencesData.OnExecute := actPreferences.OnExecute;

  pnlQueryMemo.Height := AppSettings.ReadInt(asQuerymemoheight);
  pnlQueryHelpers.Width := AppSettings.ReadInt(asQueryhelperswidth);
  pnlLeft.Width := AppSettings.ReadInt(asDbtreewidth);
  pnlPreview.Height := AppSettings.ReadInt(asDataPreviewHeight);
  if AppSettings.ReadBool(asDataPreviewEnabled) then
    actDataPreviewExecute(actDataPreview);
  SynMemoSQLLog.Height := Max(AppSettings.ReadInt(asLogHeight), spltTopBottom.MinSize);
  // Force status bar position to below log memo
  StatusBar.Top := SynMemoSQLLog.Top + SynMemoSQLLog.Height;
  actDataShowNext.Hint := f_('Show next %s rows ...', [FormatNumber(AppSettings.ReadInt(asDatagridRowsPerStep))]);
  actAboutBox.Caption := f_('About %s', [APPNAME+' '+FAppVersion]);
  // Activate logging
  LogToFile := AppSettings.ReadBool(asLogToFile);
  if AppSettings.ReadBool(asLogHorizontalScrollbar) then
    actLogHorizontalScrollbar.Execute;
  if AppSettings.ReadBool(asFavoriteObjectsOnly) then
    actFavoriteObjectsOnly.Execute;

  // Data-Font:
  DataGrid.Font.Name := AppSettings.ReadString(asDataFontName);
  QueryGrid.Font.Name := AppSettings.ReadString(asDataFontName);
  DataGrid.Font.Size := AppSettings.ReadInt(asDataFontSize);
  QueryGrid.Font.Size := AppSettings.ReadInt(asDataFontSize);
  FixVT(DataGrid, AppSettings.ReadInt(asGridRowLineCount));
  FixVT(QueryGrid, AppSettings.ReadInt(asGridRowLineCount));
  // Load color settings
  DatatypeCategories[dtcInteger].Color := AppSettings.ReadInt(asFieldColorNumeric);
  DatatypeCategories[dtcReal].Color := AppSettings.ReadInt(asFieldColorReal);
  DatatypeCategories[dtcText].Color := AppSettings.ReadInt(asFieldColorText);
  DatatypeCategories[dtcBinary].Color := AppSettings.ReadInt(asFieldColorBinary);
  DatatypeCategories[dtcTemporal].Color := AppSettings.ReadInt(asFieldColorDatetime);
  DatatypeCategories[dtcSpatial].Color := AppSettings.ReadInt(asFieldColorSpatial);
  DatatypeCategories[dtcOther].Color := AppSettings.ReadInt(asFieldColorOther);
  CalcNullColors;

  DataLocalNumberFormat := AppSettings.ReadBool(asDataLocalNumberFormat);

  // Database tree options
  actGroupObjects.Checked := AppSettings.ReadBool(asGroupTreeObjects);
  if AppSettings.ReadBool(asDisplayObjectSizeColumn) then
    menuShowSizeColumn.Click;
  if AppSettings.ReadBool(asAutoExpand) then
    menuAutoExpand.Click;
  if AppSettings.ReadBool(asDoubleClickInsertsNodeText) then
    menuDoubleClickInsertsNodeText.Click;

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
    Action.ShortCut := AppSettings.ReadInt(asActionShortcut1, Action.Name, Action.ShortCut);
  end;

  // Size of completion proposal window
  SynCompletionProposal.Width := AppSettings.ReadInt(asCompletionProposalWidth);
  SynCompletionProposal.NbLinesInWindow := AppSettings.ReadInt(asCompletionProposalNbLinesInWindow);

  // Place progressbar on the statusbar
  ProgressBarStatus.Parent := StatusBar;
  ProgressBarStatus.Visible := False;

  // SynMemo font, hightlighting and shortcuts
  SetupSynEditors;

  SetMainTab(tabHost);
  FBtnAddTab := TSpeedButton.Create(PageControlMain);
  FBtnAddTab.Parent := PageControlMain;
  VirtualImageListMain.GetBitmap(actNewQueryTab.ImageIndex, FBtnAddTab.Glyph);
  FBtnAddTab.Height := PageControlMain.TabRect(0).Bottom - PageControlMain.TabRect(0).Top - 2;
  FBtnAddTab.Width := FBtnAddTab.Height;
  FBtnAddTab.Flat := True;
  FBtnAddTab.Hint := actNewQueryTab.Hint;
  FBtnAddTab.OnClick := actNewQueryTab.OnExecute;

  // Filter panel
  VirtualImageListMain.GetBitmap(134, btnCloseFilterPanel.Glyph);
  if AppSettings.ReadBool(asFilterPanel) then
    actFilterPanelExecute(nil);
  lblFilterVTInfo.Caption := '';

  SelectedTableColumns := TTableColumnList.Create;
  SelectedTableKeys := TTableKeyList.Create;
  SelectedTableForeignKeys := TForeignKeyList.Create;
  SelectedTableTimestampColumns := TStringList.Create;

  // Set up connections list
  FConnections := TDBConnectionList.Create;
  FConnections.OnNotify := ConnectionsNotify;

  FTreeRefreshInProgress := False;
  FGridCopying := False;
  FGridPasting := False;

  FileEncodings := Explode(',', _('Auto detect (may fail)')+',ANSI,ASCII,Unicode,Unicode Big Endian,UTF-8,UTF-7');

  // Detect timezone offset in seconds, once
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD: FTimeZoneOffset := (TZI.Bias + TZI.StandardBias);
    TIME_ZONE_ID_DAYLIGHT: FTimeZoneOffset := (TZI.Bias + TZI.DaylightBias);
    TIME_ZONE_ID_UNKNOWN: FTimeZoneOffset := TZI.Bias;
    else RaiseLastOSError;
  end;
  FTimeZoneOffset := FTimeZoneOffset * 60;

  // Set noderoot for query helpers box
  treeQueryHelpers.RootNodeCount := 7;

  // Initialize taskbar jump list
  if not FIsWine then begin
    FJumpList := TJumpList.Create;
    FJumpList.ApplicationId := APPNAME + IntToStr(GetExecutableBits);
  end;

  FLastCaptionChange := 0;
  FLastPortableSettingsSave := 0;
  FLastAppSettingsWrites := 0;
  FFormatSettings := TFormatSettings.Create('en-US');

  if RunningAsUwp then begin
    actUpdateCheck.Enabled := False;
    actUpdateCheck.Hint := f_('Please update %s through the Microsoft Store.', [APPNAME]);
    actWebDownloadpage.Hint := 'ms-windows-store://pdp/?PRODUCTID=9NXPRT2T0ZJF';
  end;

  // Now we are free to use certain methods, which are otherwise fired too early
  MainFormCreated := True;

  // Log some application details - useful when analyzing session logs
  LogSQL(f_('App path: "%s"', [Application.ExeName]), lcDebug);
  LogSQL(f_('Version: "%s"', [AppVersion]), lcDebug);
  LogSQL(f_('Theme: "%s"', [TStyleManager.ActiveStyle.Name]), lcDebug);
  LogSQL(f_('Pixels per inch on current monitor: %d', [Monitor.PixelsPerInch]), lcDebug);
  LogSQL(f_('Timezone offset: %d', [FTimeZoneOffset]), lcDebug);
end;


{**
  Check for connection parameters on commandline or show connections form.
}
procedure TMainForm.AfterFormCreate;
var
  LastSessions, FileNames: TStringlist;
  Connection: TDBConnection;
  LoadedParams, ConnectionParams: TConnectionParameters;
  LastUpdatecheck, LastStatsCall, LastConnect: TDateTime;
  UpdatecheckInterval, i: Integer;
  DefaultLastrunDate, LastActiveSession: String;
  frm : TfrmUpdateCheck;
  StatsCall: THttpDownload;
  SessionPaths: TStringlist;
  DlgResult: TModalResult;
  Tab: TQueryTab;
  SessionManager: TConnForm;
begin
  DefaultLastrunDate := '2000-01-01';

  // Do an updatecheck if checked in settings
  if AppSettings.ReadBool(asUpdatecheck) then begin
    try
      LastUpdatecheck := StrToDateTime(AppSettings.ReadString(asUpdatecheckLastrun));
    except
      LastUpdatecheck := StrToDateTime(DefaultLastrunDate);
    end;
    UpdatecheckInterval := AppSettings.ReadInt(asUpdatecheckInterval);
    if DaysBetween(Now, LastUpdatecheck) >= UpdatecheckInterval then begin
      frm := TfrmUpdateCheck.Create(Self);
      frm.btnCancel.Caption := _('Skip');
      try
        frm.ReadCheckFile;
        // Show the dialog if release is available, or - when wanted - build checks are activated
        if (AppSettings.ReadBool(asUpdatecheckBuilds) and frm.btnBuild.Enabled)
          or frm.btnRelease.Enabled then begin
          frm.ShowModal;
        end;
      except
        on E:Exception do
          LogSQL(f_('Error when checking for updates: %s', [E.Message]));
      end;
      frm.Free;
    end;
  end;

  // Get all session names
  SessionPaths := TStringList.Create;
  AppSettings.GetSessionPaths('', SessionPaths);

  // Probably hide image
  FHasDonatedDatabaseCheck := nbUnset;
  imgDonate.Visible := HasDonated(True) <> nbTrue;
  imgDonate.Repaint;

  // Call user statistics if checked in settings
  if AppSettings.ReadBool(asDoUsageStatistics) then begin
    try
      LastStatsCall := StrToDateTime(AppSettings.ReadString(asLastUsageStatisticCall));
    except
      LastStatsCall := StrToDateTime(DefaultLastrunDate);
    end;
    if DaysBetween(Now, LastStatsCall) >= 30 then begin
      // Report used app version, bits, and theme name (so we find mostly unused ones for removal)
      StatsCall := THttpDownload.Create(Self);
      StatsCall.URL := APPDOMAIN + 'savestats.php?c=' + IntToStr(FAppVerRevision) +
        '&bits=' + IntToStr(GetExecutableBits) +
        '&thm=' + EncodeURLParam(TStyleManager.ActiveStyle.Name);
      // Enumerate actively used server versions
      for i:=0 to SessionPaths.Count-1 do begin
        AppSettings.SessionPath := SessionPaths[i];
        try
          LastConnect := StrToDateTime(AppSettings.ReadString(asLastConnect));
        except
          LastConnect := StrToDateTime(DefaultLastrunDate);
        end;
        if LastConnect > LastStatsCall then begin
          StatsCall.URL := StatsCall.URL + '&s[]=' + IntToStr(AppSettings.ReadInt(asNetType)) + '-' + IntToStr(AppSettings.ReadInt(asServerVersion));
        end;
      end;
      AppSettings.ResetPath;
      try
        StatsCall.SendRequest('');
        AppSettings.WriteString(asLastUsageStatisticCall, DateTimeToStr(Now));
      except
        // Silently ignore it when the url could not be called over the network.
      end;
      FreeAndNil(StatsCall);
    end;
  end;

  ParseCommandLine(Windows.GetCommandLine, ConnectionParams, FileNames);
  if Assigned(ConnectionParams) then begin
    // Minimal parameter for command line mode is hostname
    InitConnection(ConnectionParams, True, Connection);
  end else if AppSettings.ReadBool(asAutoReconnect) then begin
    // Auto connection via preference setting
    // Do not autoconnect if we're in commandline mode and the connection was not successful
    LastSessions := Explode(DELIM, AppSettings.ReadString(asLastSessions));
    LastActiveSession := AppSettings.ReadString(asLastActiveSession);
    for i:=LastSessions.Count-1 downto 0 do begin
      if SessionPaths.IndexOf(LastSessions[i]) = -1 then
        LastSessions.Delete(i);
    end;
    if LastSessions.Count > 0 then begin
      if LastSessions.IndexOf(LastActiveSession) = -1 then
        LastActiveSession := LastSessions[0];
      for i:=0 to LastSessions.Count-1 do begin
        try
          LoadedParams := TConnectionParameters.Create(LastSessions[i]);
          InitConnection(LoadedParams, LastActiveSession=LastSessions[i], Connection);
        except on E:Exception do
          ErrorDialog(E.Message);
        end;
      end;
    end;
  end;

  // Display session manager
  if Connections.Count = 0 then begin
    // Cannot be done in OnCreate because we need ready forms here:
    SessionManager := TConnForm.Create(Self);
    DlgResult := mrCancel;
    try
      DlgResult := SessionManager.ShowModal;
      SessionManager.Free;
    except
      // Work around VCL bug: Suppress access violation in TCustomForm.IsFormSizeStored
      // when closing dialog via Alt+F4
    end;
    if DlgResult = mrCancel then begin
      Free;
      Exit;
    end;
  end;

  // Restore backup'ed query tabs
  if AppSettings.RestoreTabsInitValue then begin
    TimerStoreTabs.Enabled := RestoreTabs;
  end;

  // Load SQL file(s) by command line
  if not RunQueryFiles(FileNames, nil, false) then begin
    for i:=0 to FileNames.Count-1 do begin
      Tab := ActiveOrEmptyQueryTab(False);
      Tab.LoadContents(FileNames[i], True, nil);
      if i = FileNames.Count-1 then
        SetMainTab(Tab.TabSheet);
    end;
  end;
end;


function TMainForm.InitTabsIniFile: TIniFile;
var
  WaitingSince: UInt64;
  Attempts: Integer;
  TabsIniFilename: String;
begin
  // Try to open tabs.ini for writing or reading
  // Taking multiple application instances into account
  if AppSettings.PortableMode then
    TabsIniFilename := ExtractFilePath(Application.ExeName) + 'tabs.ini'
  else
    TabsIniFilename := AppSettings.DirnameUserAppData + 'tabs.ini';
  WaitingSince := GetTickCount64;
  Attempts := 0;
  while not FileIsWritable(TabsIniFilename) do begin
    if GetTickCount64 - WaitingSince > 3000 then
      Raise Exception.Create(f_('Could not open file %s', [TabsIniFilename]));
    Sleep(200);
    Inc(Attempts);
  end;
  if Attempts > 0 then begin
    LogSQL(Format('Had to wait %d ms before opening %s', [GetTickCount64 - WaitingSince, TabsIniFilename]), lcDebug);
  end;
  // Catch errors when file cannot be created
  if not FileExists(TabsIniFilename) then begin
    SaveUnicodeFile(TabsIniFilename, '');
  end;
  Result := TIniFile.Create(TabsIniFilename);
end;


procedure TMainForm.StoreTabs;
var
  Tab: TQueryTab;
  Section: String;
  Sections: TStringList;
  TabsIni: TIniFile;
  SectionTabExists: Boolean;
begin
  // Store query tab unsaved contents and setup, in tabs.ini

  try
    TabsIni := InitTabsIniFile;

    for Tab in QueryTabs do begin
      Tab.BackupUnsavedContent;
      Section := Tab.Uid;

      if Tab.Memo.GetTextLen > 0 then begin
        // Avoid writing the tabs.ini file if nothing was effectively changed
        if TabsIni.ReadString(Section, 'BackupFilename', '') <> Tab.MemoBackupFilename then
          TabsIni.WriteString(Section, 'BackupFilename', Tab.MemoBackupFilename);
        if TabsIni.ReadString(Section, 'Filename', '') <> Tab.MemoFilename then
          TabsIni.WriteString(Section, 'Filename', Tab.MemoFilename);
        if TabsIni.ReadInteger(Section, 'pid', 0) <> Integer(GetCurrentProcessId) then
          TabsIni.WriteInteger(Section, 'pid', Integer(GetCurrentProcessId));
        if TabsIni.ReadInteger(Section, 'EditorHeight', 0) <> Tab.pnlMemo.Height then
          TabsIni.WriteInteger(Section, 'EditorHeight', Tab.pnlMemo.Height);
        if TabsIni.ReadInteger(Section, 'HelpersWidth', 0) <> Tab.pnlHelpers.Width then
          TabsIni.WriteInteger(Section, 'HelpersWidth', Tab.pnlHelpers.Width);
      end;
    end;

    // Tabs with deleted files don't get restored anyway. But closed tabs with physically existing
    // files still need to be erased
    Sections := TStringList.Create;
    TabsIni.ReadSections(Sections);
    for Section in Sections do begin
      SectionTabExists := False;
      for Tab in QueryTabs do begin
        if Tab.Uid = Section then begin
          SectionTabExists := True;
          Break;
        end;
      end;
      // Delete tab section if no tab was closed
      if not SectionTabExists then begin
        TabsIni.EraseSection(Section);
      end;
    end;

    // Close file
    TabsIni.Free;
  except
    on E:Exception do begin
      TimerStoreTabs.Enabled := False;
      ErrorDialog(_('Storing tab setup failed'),
        'Tabs won''t be stored in this session.' + CRLF + CRLF +
        E.Message + CRLF + CRLF +
        SysErrorMessage(Windows.GetLastError)
        );
    end;
  end;
end;


function TMainForm.RestoreTabs: Boolean;
var
  Tab: TQueryTab;
  Sections: TStringList;
  Section, Filename, BackupFilename: String;
  TabsIni: TIniFile;
  pid: Cardinal;
  EditorHeight, HelpersWidth: Integer;
begin
  // Restore query tab setup from tabs.ini
  Result := True;

  try
    TabsIni := InitTabsIniFile;
    LogSQL('Restoring tab setup from '+TabsIni.FileName, lcDebug);

    Sections := TStringList.Create;
    TabsIni.ReadSections(Sections);

    for Section in Sections do begin

      Filename := TabsIni.ReadString(Section, 'Filename', '');
      BackupFilename := TabsIni.ReadString(Section, 'BackupFilename', '');
      pid := Cardinal(TabsIni.ReadInteger(Section, 'pid', 0));
      EditorHeight := TabsIni.ReadInteger(Section, 'EditorHeight', 0);
      HelpersWidth := TabsIni.ReadInteger(Section, 'HelpersWidth', 0);

      // Don't restore this tab if it belongs to a different running process
      if (pid > 0) and (pid <> GetCurrentProcessId) and ProcessExists(pid) then
        Continue;

      // Either we have a backup file, or a user stored file.
      // Both of them may not exist.
      if not BackupFilename.IsEmpty then begin
        if FileExists(BackupFilename) then begin
          Tab := ActiveOrEmptyQueryTab(False);
          Tab.Uid := Section;
          Tab.LoadContents(BackupFilename, True, TEncoding.UTF8);
          Tab.MemoFilename := Filename;
          Tab.Memo.Modified := True;
          if EditorHeight > 50 then
            Tab.pnlMemo.Height := EditorHeight;
          if HelpersWidth > 50 then
            Tab.pnlHelpers.Width := HelpersWidth;
        end else begin
          // Remove tab section if backup file is gone or inaccessible for some reason
          TabsIni.EraseSection(Section);
        end;
      end else if not Filename.IsEmpty then begin
        if FileExists(Filename) then begin
          Tab := ActiveOrEmptyQueryTab(False);
          Tab.Uid := Section;
          Tab.LoadContents(Filename, True, nil);
          Tab.MemoFilename := Filename;
          if EditorHeight > 50 then
            Tab.Memo.Height := EditorHeight;
          if HelpersWidth > 50 then
            Tab.pnlHelpers.Width := HelpersWidth;
        end else begin
          // Remove tab section if user stored file was deleted by user
          TabsIni.EraseSection(Section);
        end;
      end;

    end;

    Sections.Free;
    // Close file
    TabsIni.Free;
  except
    on E:Exception do begin
      Result := False;
      ErrorDialog(_('Restoring tab setup failed'),
        'Tabs won''t be stored in this session.' + CRLF + CRLF +
        E.Message + CRLF + CRLF +
        SysErrorMessage(Windows.GetLastError)
        );
    end;
  end;
end;


procedure TMainForm.TimerStoreTabsTimer(Sender: TObject);
begin
  // Backup unsaved content every 10 seconds
  StoreTabs;
end;


procedure TMainForm.actSessionManagerExecute(Sender: TObject);
var
  Dialog: TConnForm;
begin
  Dialog := TConnForm.Create(Self);
  Dialog.ShowModal;
end;


procedure TMainForm.actDisconnectExecute(Sender: TObject);
var
  Connection: TDBConnection;
  Node: PVirtualNode;
  DlgResult: Integer;
  Dialog: TConnForm;
begin
  // Disconnect active connection. If it's the last, exit application
  Connection := ActiveConnection;
  // Find and remove connection node from tree
  Node := GetRootNode(DBtree, Connection);
  DBTree.DeleteNode(Node);
  FConnections.Remove(Connection);
  // TODO: focus last session?
  SelectNode(DBtree, GetNextNode(DBtree, nil));
  if FConnections.Count = 0 then begin
    Dialog := TConnForm.Create(Self);
    DlgResult := Dialog.ShowModal;
    if DlgResult = mrCancel then
      actExitApplication.Execute;
  end;
end;


procedure TMainForm.ConnectionsNotify(Sender: TObject; const Item: TDBConnection; Action: TCollectionNotification);
var
  Results: TDBQuery;
  Tab: TQueryTab;
  ResultTab: TResultTab;
  i: Integer;
  Keys: TStringList;
  rx: TRegExpr;
begin
  // Connection removed or added
  case Action of
    cnRemoved, cnExtracted: begin
      // Post pending UPDATE and release current table with result
      Results := GridResult(DataGrid);
      if Assigned(Results) then begin
        if Results.Modified then
          actDataPostChangesExecute(DataGrid);
        if DataGridResult = Results then begin
          FreeAndNil(DataGridResult);
          DataGridTable := nil;
        end;
      end;

      // Remove result sets which may cause AVs when disconnected
      for Tab in QueryTabs do begin
        if Assigned(Tab.QueryProfile) and (Tab.QueryProfile.Connection = Item) then
          FreeAndNil(Tab.QueryProfile);
        for ResultTab in Tab.ResultTabs do begin
          if ResultTab.Results.Connection = Item then begin
            Tab.ResultTabs.Clear;
            Tab.tabsetQuery.Tabs.Clear;
            break;
          end;
        end;
      end;
      for i:=0 to FHostListResults.Count-1 do begin
        if (FHostListResults[i] <> nil) and (FHostListResults[i].Connection = Item) then begin
          FHostListResults[i].Free;
          FHostListResults[i] := nil;
        end;
      end;

      // Remove filters if unwanted
      if not AppSettings.ReadBool(asReuseEditorConfiguration) then begin
        AppSettings.SessionPath := Item.Parameters.SessionPath;
        Keys := AppSettings.GetKeyNames;
        rx := TRegExpr.Create;
        rx.Expression := '.+'+QuoteRegExprMetaChars(DELIM)+'.+';
        for i:=0 to Keys.Count-1 do begin
          if rx.Exec(Keys[i]) then begin
            AppSettings.SessionPath := Item.Parameters.SessionPath + '\' + Keys[i];
            AppSettings.DeleteCurrentKey;
          end;
        end;
        rx.Free;
      end;

      FreeAndNil(ActiveObjectEditor);
      RefreshHelperNode(HELPERNODE_PROFILE);
      RefreshHelperNode(HELPERNODE_COLUMNS);

      // Last chance to access connection related properties before disconnecting
      AppSettings.SessionPath := Item.Parameters.SessionPath;
      AppSettings.WriteString(asLastUsedDB, Item.Database);

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
  FCreateDatabaseDialog := TCreateDatabaseForm.Create(Self);
  FCreateDatabaseDialog.ShowModal;
  FreeAndNil(FCreateDatabaseDialog);
end;


procedure TMainForm.actImportCSVExecute(Sender: TObject);
var
  Dialog: Tloaddataform;
begin
  // Import Textfile
  Dialog := Tloaddataform.Create(Self);
  Dialog.ShowModal;
end;

procedure TMainForm.actPreferencesExecute(Sender: TObject);
begin
  // Preferences
  FPreferencesDialog := Toptionsform.Create(Self);
  if Sender = actPreferencesLogging then
    FPreferencesDialog.pagecontrolMain.ActivePage := FPreferencesDialog.tabLogging
  else if Sender = actPreferencesData then
    FPreferencesDialog.pagecontrolMain.ActivePage := FPreferencesDialog.tabGridFormatting;
  FPreferencesDialog.ShowModal;
  FreeAndNil(FPreferencesDialog);
end;

procedure TMainForm.actHelpExecute(Sender: TObject);
begin
  // Display help document
  Help(Sender, '');
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  PanelRect: TRect;
  Tab: TQueryTab;
  w0, w1, w2, w3, w4, w5, w6: Integer;

  function GridNeedHeight: Integer;
  begin
    // Return missing number of height pixels the query grid needs
    Result := Max(0, Tab.spltQuery.MinSize - (Tab.TabSheet.Height - Tab.pnlMemo.Height - Tab.spltQuery.Height - Tab.tabsetQuery.Height));
  end;

  function CalcPanelWidth(PreferredWidth, Percentage: Integer): Integer;
  begin
    Result := Round(Min(PreferredWidth, Width / 100 * Percentage));
  end;
begin
  // Exit early when user pressed "Cancel" on connection dialog
  if csDestroying in ComponentState then
    Exit;
  // No need to resize anything if main window is minimized (= not visible)
  if WindowState = wsMinimized then
    Exit;

  // Super intelligent calculation of status bar panel width
  w1 := CalcPanelWidth(110, 10);
  w2 := CalcPanelWidth(140, 10);
  w3 := CalcPanelWidth(170, 15);
  w4 := CalcPanelWidth(150, 15);
  w5 := CalcPanelWidth(140, 10);
  w6 := CalcPanelWidth(250, 20);
  w0 := StatusBar.Width - w1 - w2 - w3 - w4 - w5 - w6;
  StatusBar.Panels[0].Width := w0;
  StatusBar.Panels[1].Width := w1;
  StatusBar.Panels[2].Width := w2;
  StatusBar.Panels[3].Width := w3;
  StatusBar.Panels[4].Width := w4;
  StatusBar.Panels[5].Width := w5;
  StatusBar.Panels[6].Width := w6;

  // Retreive the rectancle of the statuspanel (in our case the fifth panel)
  SendMessage(StatusBar.Handle, SB_GETRECT, 5, Integer(@PanelRect));
  // Position the progressbar over the panel on the statusbar
  with PanelRect do
    ProgressBarStatus.SetBounds(Left, Top, Right-Left, Bottom-Top);
  lblDataTop.Width := pnlDataTop.Width - tlbDataButtons.Width - 10;
  FixQueryTabCloseButtons;

  // Ensure query grids are not overlapped by sql log
  for Tab in QueryTabs do begin
    // Decrease height of pnlMemo if grid has not enough height
    Tab.pnlMemo.Height := Max(Tab.pnlMemo.Height-GridNeedHeight, Tab.spltQuery.MinSize);
    // Try again and resize SQLLog if required
    SynMemoSQLLog.Height := Max(SynMemoSQLLog.Height-GridNeedHeight, spltTopBottom.MinSize);
  end;

  // Right aligned button
  if imgDonate.Visible then begin
    imgDonate.Width := 122;
    imgDonate.Height := 22;
    imgDonate.Left := ControlBarMain.Width - imgDonate.Width;
  end;

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Manually set focus to tree - otherwise the database filter as the first
  // control catches focus on startup, which is ugly.
  if DBtree.CanFocus then
    DBtree.SetFocus;

  // Apply resize event and call it once here in OnShow, when the form has its final dimensions
  OnResize := FormResize;
  OnResize(Sender);

  // Simulated link label, has non inherited blue font color
  lblExplainProcess.Font.Color := clBlue;
  lblExplainProcessAnalyzer.Font.Color := clBlue;
end;

procedure TMainForm.actUserManagerExecute(Sender: TObject);
var
  Dialog: TUserManagerForm;
begin
  Dialog := TUserManagerForm.Create(Self);
  Dialog.ShowModal;
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
  if Sender = actClearQueryEditor then begin
    m := ActiveQueryMemo
  end else if Sender = actClearQueryLog then begin
    m := SynMemoSQLLog;
    m.Gutter.LineNumberStart := m.Gutter.LineNumberStart + m.Lines.Count;
  end else begin
    m := SynMemoFilter;
    editFilterSearch.Clear;
  end;
  m.SelectAll;
  m.SelText := '';
  m.SelStart := 0;
  m.SelEnd := 0;
  if Sender = actClearQueryEditor then begin
    ActiveQueryTab.MemoFilename := '';
    ActiveQueryTab.Memo.Modified := False;
  end;
  if m = SynMemoFilter then begin
    InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
  end;
end;


procedure TMainForm.menuClearDataTabFilterClick(Sender: TObject);
begin
  // Same as "Clear filter" button, but *before* the data tab is activated
  AppSettings.SessionPath := GetRegKeyTable;
  if AppSettings.ValueExists(asFilter) then begin
    AppSettings.DeleteValue(asFilter);
    LogSQL(f_('Data filter for %s deleted', [ActiveDbObj.Name]), lcInfo);
  end;
end;


procedure TMainForm.actTableToolsExecute(Sender: TObject);
var
  Act: TAction;
  InDBTree: Boolean;
  Node: PVirtualNode;
  DBObj: PDBObject;
  Dialog: TfrmTableTools;
begin
  // Show table tools dialog
  Dialog := TfrmTableTools.Create(Self);
  Act := Sender as TAction;
  Dialog.PreSelectObjects.Clear;
  InDBTree := (Act.ActionComponent is TMenuItem)
    and (TPopupMenu((Act.ActionComponent as TMenuItem).GetParentMenu).PopupComponent = DBTree);
  if InDBTree then
    Dialog.PreSelectObjects.Add(ActiveDbObj)
  else begin
    Node := GetNextNode(ListTables, nil, True);
    while Assigned(Node) do begin
      DBObj := ListTables.GetNodeData(Node);
      Dialog.PreSelectObjects.Add(DBObj^);
      Node := GetNextNode(ListTables, Node, True);
    end;
  end;
  if Sender = actMaintenance then
    Dialog.ToolMode := tmMaintenance
  else if Sender = actFindTextOnServer then
    Dialog.ToolMode := tmFind
  else if Sender = actExportTables then
    Dialog.ToolMode := tmSQLExport
  else if Sender = actBulkTableEdit then
    Dialog.ToolMode := tmBulkTableEdit;
  Dialog.ShowModal;
end;


procedure TMainForm.actUnixTimestampColumnExecute(Sender: TObject);
var
  i: Integer;
  FocusedColumnName: String;
  GridColumn: TVirtualTreeColumn;
begin
  // Mark focused column as UNIX timestamp column
  AppSettings.SessionPath := GetRegKeyTable;
  GridColumn := DataGrid.Header.Columns[DataGrid.FocusedColumn];
  FocusedColumnName := GridColumn.Text;
  i := SelectedTableTimestampColumns.IndexOf(FocusedColumnName);
  if i > -1 then
    SelectedTableTimestampColumns.Delete(i);
  if (Sender as TAction).Checked then begin
    SelectedTableTimestampColumns.Add(FocusedColumnName);
    if GridColumn.ImageIndex = -1 then
      GridColumn.ImageIndex := 149;
  end else begin
    if GridColumn.ImageIndex = 149 then
      GridColumn.ImageIndex := -1;
  end;
  if SelectedTableTimestampColumns.Count > 0 then
    AppSettings.WriteString(asTimestampColumns, SelectedTableTimestampColumns.Text)
  else if AppSettings.ValueExists(asTimestampColumns) then
    AppSettings.DeleteValue(asTimestampColumns);

  AppSettings.ResetPath;
  DataGrid.Invalidate;
end;


function TMainForm.HandleUnixTimestampColumn(Sender: TBaseVirtualTree; Column: TColumnIndex): Boolean;
begin
  // Shorthand for various places where we would normally have to add all these conditions
  Result := (Sender = DataGrid)
    and (Column > NoColumn)
    and (DataGridResult <> nil)
    and (DataGridResult.DataType(Column).Category in [dtcInteger, dtcReal])
    and (SelectedTableTimestampColumns.IndexOf(DataGrid.Header.Columns[Column].Text) > -1);
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
var
  Dialog: TCopyTableForm;
begin
  // copy table
  Dialog := TCopyTableForm.Create(Self);
  Dialog.ShowModal;
end;


procedure TMainForm.menuConnectionsPopup(Sender: TObject);
var
  i: integer;
  item: TMenuItem;
  SessionPaths: TStringList;
  Connection: TDBConnection;
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
  SessionPaths := TStringList.Create;
  AppSettings.GetSessionPaths('', SessionPaths);
  for i:=0 to SessionPaths.Count-1 do begin
    item := TMenuItem.Create(menuConnections);
    item.Caption := SessionPaths[i];
    item.OnClick := SessionConnect;
    for Connection in Connections do begin
      if SessionPaths[i] = Connection.Parameters.SessionPath then begin
        item.Checked := True;
        break;
      end;
    end;
    menuConnections.Items.Add(item);
  end;

end;


procedure TMainForm.MainMenuFileClick(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  SessionPaths: TStringList;
  Connection: TDBConnection;
begin
  // Add all sessions to submenu
  menuConnectTo.Clear;
  SessionPaths := TStringList.Create;
  AppSettings.GetSessionPaths('', SessionPaths);
  for i:=0 to SessionPaths.Count-1 do begin
    Item := TMenuItem.Create(menuConnectTo);
    Item.Caption := SessionPaths[i];
    Item.OnClick := SessionConnect;
    for Connection in Connections do begin
      if SessionPaths[i] = Connection.Parameters.SessionPath then begin
        Item.Checked := True;
        break;
      end;
    end;
    menuConnectTo.Add(Item);
  end;
end;


procedure TMainForm.actWebbrowse(Sender: TObject);
begin
  // Browse to URL (hint)
  ShellExec( TAction(Sender).Hint );
end;


procedure TMainForm.DonateClick(Sender: TObject);
var
  Dialog: TWinControl;
  place: String;
begin
  // Click on one of the various donate buttons
  if Sender is TWinControl then begin
    Dialog := GetParentFormOrFrame(TWinControl(Sender));
  end else begin
    Dialog := Self;
  end;
  if Dialog = nil then
    ErrorDialog(f_('Could not determine parent form of this %s', [Sender.ClassName]))
  else begin
    place := LowerCase(Dialog.UnitName);
    ShellExec(APPDOMAIN + 'donatebutton.php?place=' + EncodeURLParam(place));
  end;
end;


procedure TMainForm.actExportSettingsExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  // Export settings to .txt file
  Dialog := TSaveDialog.Create(Self);
  Dialog.Title := f_('Export %s settings to file ...', [APPNAME]);
  Dialog.DefaultExt := 'txt';
  Dialog.Filter := _('Text files')+' (*.txt)|*.txt|'+_('All files')+' (*.*)|*.*';
  Dialog.Options := Dialog.Options + [ofOverwritePrompt];
  if Dialog.Execute then try
    AppSettings.ExportSettings(Dialog.FileName);
    MessageDialog(f_('Settings successfully exported to %s', [Dialog.FileName]), mtInformation, [mbOK]);
  except
    on E:Exception do
      ErrorDialog(E.Message);
  end;
  Dialog.Free;
end;


procedure TMainForm.actImportSettingsExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  // Import settings from .txt or .reg file
  Dialog := TOpenDialog.Create(Self);
  Dialog.Title := f_('Import %s settings from file ...', [APPNAME]);
  Dialog.Filter := _('Text files')+' (*.txt)|*.txt|'+_('Registry dump, deprecated')+' (*.reg)|*.reg|'+_('All files')+' (*.*)|*.*';
  ImportSettingsDone := False;
  if Dialog.Execute then try
    if LowerCase(ExtractFileExt(Dialog.FileName)) = 'reg' then
      ShellExec('regedit.exe', '', '"'+Dialog.FileName+'"')
    else begin
      AppSettings.ImportSettings(Dialog.FileName);
      MessageDialog(f_('Settings successfully restored from %s', [Dialog.FileName]), mtInformation, [mbOK]);
    end;
    ImportSettingsDone := True;
  except
    on E:Exception do
      ErrorDialog(E.Message);
  end;
  Dialog.Free;
end;


function TMainForm.GetCurrentQuery(Tab: TQueryTab): String;
var
  BatchAll: TSQLBatch;
  Query: TSQLSentence;
begin
  // Return SQL query on cursor position
  Result := '';
  BatchAll := TSQLBatch.Create;
  BatchAll.SQL := Tab.Memo.Text;
  for Query in BatchAll do begin
    if (Tab.Memo.SelStart >= Query.LeftOffset-1) and (Tab.Memo.SelStart < Query.RightOffset) then begin
      Result := Query.SQL;
      Tab.LeftOffsetInMemo := Query.LeftOffset;
      break;
    end;
  end;
  BatchAll.Free;
end;


procedure TMainForm.actExecuteQueryExecute(Sender: TObject);
var
  ProfileNode: PVirtualNode;
  Batch: TSQLBatch;
  Tab: TQueryTab;
  BindParamItem: Integer;
  NewSQL, msg, Command: String;
  Query: TSQLSentence;
  rx: TRegExpr;
  ContainsUnsafeQueries, DoExecute: Boolean;
begin
  Screen.Cursor := crHourGlass;
  Tab := ActiveQueryTab;
  OperationRunning(True);

  ShowStatusMsg(_('Splitting SQL queries ...'));
  Batch := TSQLBatch.Create;
  if Sender = actExecuteSelection then begin
    Batch.SQL := Tab.Memo.SelText;
    Tab.LeftOffsetInMemo := Tab.Memo.SelStart;
  end else if Sender = actExecuteCurrentQuery then begin
    Batch.SQL := GetCurrentQuery(Tab);
  end else if Sender = actExplainCurrentQuery then begin
    Batch.SQL := 'EXPLAIN ' + GetCurrentQuery(Tab);
  end else begin
    Batch.SQL := Tab.Memo.Text;
    Tab.LeftOffsetInMemo := 0;
  end;

  // Check if there is bind parameters
  if tab.ListBindParams.Count > 0 then begin
    NewSQL := Batch.SQL;
	  // Replace all parameters by their values
	  // by descending to avoid having problems with similar variables name (eg test & test1)
    for BindParamItem := tab.ListBindParams.Count-1 downto 0 do begin
      // Do the Replace only if there is a value
      if StrLen(PChar(tab.ListBindParams.Items[BindParamItem].Value))>0 then
        NewSQL := StringReplace(NewSQL, tab.ListBindParams.Items[BindParamItem].Parameter, tab.ListBindParams.Items[BindParamItem].Value,[rfReplaceAll]);
    end;

    Batch.SQL := NewSQL;
  end;

  DoExecute := True;
  if AppSettings.ReadBool(asWarnUnsafeUpdates) then begin
    ShowStatusMsg(_('Checking queries for unsafe UPDATEs/DELETEs ...'));
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.Expression := '\sWHERE\s';
    ContainsUnsafeQueries := False;
    for Query in Batch do begin
      Command := UpperCase(getFirstWord(Query.SQL));
      if ((Command = 'UPDATE') or (Command = 'DELETE')) and (not rx.Exec(Query.SQL)) then begin
        ContainsUnsafeQueries := True;
        Break;
      end;
    end;
    rx.Free;
    if ContainsUnsafeQueries then begin
      Screen.Cursor := crDefault;
      msg := _('Your query contains UPDATEs and/or DELETEs without a WHERE clause. Please confirm that you know what you''re doing.');
      DoExecute := MessageDialog(_('Run unsafe queries without a WHERE clause?'), msg, mtConfirmation, [mbYes, mbNo], asWarnUnsafeUpdates) = mrYes;
    end;
  end;


  if DoExecute then begin
    Screen.Cursor := crHourGlass;
    EnableProgress(Batch.Count);
    Tab.ResultTabs.Clear;
    Tab.tabsetQuery.Tabs.Clear;
    FreeAndNil(Tab.QueryProfile);
    ProfileNode := FindNode(Tab.treeHelpers, HELPERNODE_PROFILE, nil);
    Tab.DoProfile := Assigned(ProfileNode) and (Tab.treeHelpers.CheckState[ProfileNode] in CheckedStates);
    if Tab.DoProfile then try
      ActiveConnection.Query('SET profiling=1');
    except
      on E:EDbError do begin
        ErrorDialog(f_('Query profiling requires %s or later, and the server must not be configured with %s.', ['MySQL 5.0.37', '--disable-profiling']), E.Message);
        Tab.DoProfile := False;
      end;
    end;

    // Start the execution thread
    Screen.Cursor := crAppStart;
    ActiveConnection.Ping(True); // Prevents SynEdit paint exceptions if connection was killed outside
    Tab.QueryRunning := True;
    Tab.ExecutionThread := TQueryThread.Create(ActiveConnection, Batch, Tab.Number);
  end;

  ValidateQueryControls(Sender);
end;



procedure TMainForm.BeforeQueryExecution(Thread: TQueryThread);
begin
  // Update GUI stuff
  SetProgressPosition(Thread.BatchPosition);
end;


procedure TMainForm.AfterQueryExecution(Thread: TQueryThread);
var
  Tab: TQueryTab;
  NewTab: TResultTab;
  col: TVirtualTreeColumn;
  TabCaption: String;
  Results: TDBQuery;
  i: Integer;
begin
  // Single query or query packet has finished

  ShowStatusMsg(_('Setting up result grid(s) ...'));
  Tab := GetQueryTabByNumber(Thread.TabNumber);

  // Create result tabs
  for Results in Thread.Connection.GetLastResults do begin
    NewTab := TResultTab.Create(Tab);
    Tab.ResultTabs.Add(NewTab);
    NewTab.Results := Results;
    try
      TabCaption := NewTab.Results.TableName;
      // Add postfix to tab name so tab captions are unique
      i := 1;
      while Tab.tabsetQuery.Tabs.IndexOf(TabCaption) > -1 do begin
        Inc(i);
        TabCaption := NewTab.Results.TableName + ' #' + IntToStr(i);
      end;
    except on E:EDbError do
      TabCaption := _('Result')+' #'+IntToStr(Tab.ResultTabs.Count);
    end;
    TabCaption := TabCaption + ' (' + FormatNumber(Results.ColumnCount) + '↔ × ' + FormatNumber(Results.RecordCount) + '↕)';
    Tab.tabsetQuery.Tabs.Add(TabCaption);

    NewTab.Grid.BeginUpdate;
    NewTab.Grid.Header.Options := NewTab.Grid.Header.Options + [hoVisible];
    NewTab.Grid.Header.Columns.BeginUpdate;
    NewTab.Grid.Header.Columns.Clear;
    for i:=0 to NewTab.Results.ColumnCount-1 do begin
      col := NewTab.Grid.Header.Columns.Add;
      col.Text := NewTab.Results.ColumnNames[i];
      if NewTab.Results.DataType(i).Category in [dtcInteger, dtcReal] then
        col.Alignment := taRightJustify;
      if NewTab.Results.ColIsPrimaryKeyPart(i) then
        col.ImageIndex := ICONINDEX_PRIMARYKEY
      else if NewTab.Results.ColIsUniqueKeyPart(i) then
        col.ImageIndex := ICONINDEX_UNIQUEKEY
      else if NewTab.Results.ColIsKeyPart(i) then
        col.ImageIndex := ICONINDEX_INDEXKEY;
    end;
    NewTab.Grid.Header.Columns.EndUpdate;
    NewTab.Grid.RootNodeCount := NewTab.Results.RecordCount;
    NewTab.Grid.EndUpdate;
    for i:=0 to NewTab.Grid.Header.Columns.Count-1 do
      AutoCalcColWidth(NewTab.Grid, i);
    if Tab.tabsetQuery.TabIndex = -1 then
      Tab.tabsetQuery.TabIndex := 0;
  end;
  ShowStatusMsg;
end;


procedure TMainForm.FinishedQueryExecution(Thread: TQueryThread);
var
  Tab, WarningsTab: TQueryTab;
  MetaInfo, ErroneousSQL, RegName, MsgTitle, MsgText: String;
  ProfileAllTime: Extended;
  ProfileNode: PVirtualNode;
  History: TQueryHistory;
  HistoryItem: TQueryHistoryItem;
  Warnings: TDBQuery;
  HistoryNum, MaxWarnings, RegItemsSize, KeepDays: Integer;
  DoDelete, ValueFound: Boolean;
  MinDate: TDateTime;

  procedure GoToErrorPos(Err: String);
  var
    rx: TRegExpr;
    SelStart, ErrorPos: Integer;
  begin
    // Try to set memo cursor to the relevant position
    if Tab.LeftOffsetInMemo > 0 then
      SelStart := Tab.LeftOffsetInMemo-1
    else
      SelStart := Thread.Batch[Thread.BatchPosition].LeftOffset-1;

    // Extract erroneous portion of SQL out of error message
    ErroneousSQL := '';
    rx := TRegExpr.Create;
    rx.Expression := 'for the right syntax to use near ''(.+)'' at line (\d+)';
    if rx.Exec(Err) then
      ErroneousSQL := rx.Match[1];
    rx.Expression := 'Duplicate entry ''([^'']+)''';
    if rx.Exec(Err) then
      ErroneousSQL := rx.Match[1];
    rx.Free;

    if ErroneousSQL <> '' then begin
      // Examine 1kb of memo text at given offset
      ErrorPos := Pos(ErroneousSQL, Copy(Tab.Memo.Text, SelStart, SIZE_KB));
      if ErrorPos > 0 then
        Inc(SelStart, ErrorPos-1);
      Tab.Memo.SelLength := 0;
      Tab.Memo.SelStart := SelStart;
    end;
  end;

begin
  // Find right query tab
  Tab := GetQueryTabByNumber(Thread.TabNumber);

  // Error handling
  if IsNotEmpty(Thread.ErrorMessage) then begin
    SetProgressState(pbsError);
    GoToErrorPos(Thread.ErrorMessage);
    ErrorDialog(Thread.ErrorMessage);
  end;

  // Gather meta info for logging
  MetaInfo := _('Affected rows')+': '+FormatNumber(Thread.RowsAffected)+
    '  '+_('Found rows')+': '+FormatNumber(Thread.RowsFound)+
    '  '+_('Warnings')+': '+FormatNumber(Thread.WarningCount)+
    '  '+_('Duration for')+' ' + FormatNumber(Thread.BatchPosition);
  if Thread.BatchPosition < Thread.Batch.Count then
    MetaInfo := MetaInfo + ' ' + _('of') + ' ' + FormatNumber(Thread.Batch.Count);
  if Thread.Batch.Count = 1 then
    MetaInfo := MetaInfo + ' ' + _('query')
  else
    MetaInfo := MetaInfo + ' ' + _('queries');
  if Thread.QueryTime < 60*1000 then
    MetaInfo := MetaInfo + ': '+FormatNumber(Thread.QueryTime/1000, 3) +' ' + _('sec.')
  else
    MetaInfo := MetaInfo + ': '+FormatTimeNumber(Thread.QueryTime/1000, True);
  if Thread.QueryNetTime > 0 then
    MetaInfo := MetaInfo + ' (+ '+FormatNumber(Thread.QueryNetTime/1000, 3) +' ' + _('sec.') + ' ' + _('network') + ')';
  LogSQL(MetaInfo);

  // Display query profile
  if Tab.DoProfile then begin
    Tab.QueryProfile := Thread.Connection.GetResults('SHOW PROFILE');
    Tab.ProfileTime := 0;
    Tab.MaxProfileTime := 0;
    while not Tab.QueryProfile.Eof do begin
      ProfileAllTime := MakeFloat(Tab.QueryProfile.Col(1));
      Tab.ProfileTime := Tab.ProfileTime + ProfileAllTime;
      Tab.MaxProfileTime := Max(Time, Tab.MaxProfileTime);
      Tab.QueryProfile.Next;
    end;
    ProfileNode := FindNode(Tab.treeHelpers, HELPERNODE_PROFILE, nil);
    Tab.treeHelpers.ReinitNode(ProfileNode, True);
    Tab.treeHelpers.InvalidateChildren(ProfileNode, True);
    Thread.Connection.Query('SET profiling=0');
  end;

  // Show warnings
  if Thread.WarningCount > 0 then begin
    MsgTitle := f_('Your query produced %s warnings.', [FormatNumber(Thread.WarningCount)]);
    MsgText := '';
    Warnings := Thread.Connection.GetResults('SHOW WARNINGS LIMIT 5');
    if Warnings.RecordCount < 5 then
      MsgText := MsgText + _('Warnings from last query:')+CRLF
    else if Warnings.RecordCount < Thread.WarningCount then
      MsgText := MsgText + f_('First %s warnings:', [FormatNumber(Warnings.RecordCount)])+CRLF;
    while not Warnings.Eof do begin
      MsgText := MsgText + Warnings.Col('Level') + ': ' + Warnings.Col('Message') + CRLF;
      Warnings.Next;
    end;
    MsgText := Trim(MsgText);
    if (Warnings.RecordCount = Thread.WarningCount) or (Warnings.RecordCount < 5) then
      MessageDialog(MsgTitle, MsgText, mtWarning, [mbOk], asQueryWarningsMessage)
    else begin
      MsgText := MsgText + CRLF+CRLF + _('Show all warnings in a new query tab?');
      MaxWarnings := MakeInt(Thread.Connection.GetVar('SELECT @@max_error_count'));
      if MaxWarnings < Thread.WarningCount then
        MsgText := MsgText + CRLF+CRLF+ f_('The server variable %s is currently set to %d, so you won''t see all warnings.', ['@@max_error_count', MaxWarnings]);
      if MessageDialog(MsgTitle, MsgText, mtWarning, [mbYes, mbNo], asQueryWarningsMessage) = mrYes then begin
        actNewQueryTab.Execute;
        WarningsTab := QueryTabs[QueryTabs.Count-1];
        WarningsTab.Memo.Text := 'SHOW WARNINGS';
        actExecuteQueryExecute(WarningsTab);
      end;
    end;
  end;


  // Store successful query packet in history if it's not a batch.
  // Assume that a bunch of up to 5 queries is not a batch.
  AppSettings.ResetPath;
  if AppSettings.ReadBool(asQueryHistoryEnabled)
    and IsEmpty(Thread.ErrorMessage)
    and (Thread.Batch.Count <= 5)
    and (Thread.Batch.Size <= SIZE_MB)
    then begin
    ShowStatusMsg(_('Updating query history ...'));
    KeepDays := AppSettings.ReadInt(asQueryHistoryKeepDays);

    // Load all items so we can clean up
    History := TQueryHistory.Create(Thread.Connection.Parameters.SessionPath);

    // Find lowest unused item number
    HistoryNum := 0;
    while True do begin
      Inc(HistoryNum);
      RegName := IntToStr(HistoryNum);
      ValueFound := False;
      for HistoryItem in History do begin
        if HistoryItem.RegValue = HistoryNum then begin
          ValueFound := True;
          Break;
        end;
      end;
      if not ValueFound then
        break;
    end;

    // Delete identical history items to avoid spam
    // Delete old items
    // Delete items which exceed a max datasize barrier
    AppSettings.SessionPath := Thread.Connection.Parameters.SessionPath + '\' + REGKEY_QUERYHISTORY;
    MinDate := IncDay(Now, -KeepDays);
    RegItemsSize := Thread.Batch.Size;
    for HistoryItem in History do begin
      Inc(RegItemsSize, Length(HistoryItem.SQL));
      DoDelete := (HistoryItem.SQL = Thread.Batch.SQL)
        or (HistoryItem.Time < MinDate)
        or (RegItemsSize > SIZE_MB);
      if DoDelete then
        AppSettings.DeleteValue(IntToStr(HistoryItem.RegValue));
    end;
    History.Free;

    // Store history item and closing registry key to ensure writing has finished
    try
      AppSettings.WriteString(RegName, DateTimeToStr(Now) + DELIM +
        Thread.Connection.Database + DELIM +
        IntToStr(Thread.QueryTime+Thread.QueryNetTime) + DELIM +
        Thread.Batch.SQL);
    except
      // Silence sporadic boring write errors. See http://www.heidisql.com/forum.php?t=13088
      on E:ERegistryException do
        LogSQL(f_('Error when updating query history: %s', [E.Message]), lcError);
    end;

    RefreshHelperNode(HELPERNODE_HISTORY);
  end;

  // Clean up
  DisableProgress;
  Tab.QueryRunning := False;
  ValidateControls(Thread);
  OperationRunning(False);
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
  QueryTab := nil;
  for i:=0 to QueryTabs.Count-1 do begin
    if QueryTabs[i].tabsetQuery = Sender then begin
      QueryTab := QueryTabs[i];
      break;
    end;
  end;
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


procedure TMainForm.menuQueryExplainClick(Sender: TObject);
var
  SQL: String;
begin
  // Sub menu with EXPLAIN items pops up
  SQL := GetCurrentQuery(ActiveQueryTab);
  actExplainCurrentQuery.Enabled := ActiveConnection.Parameters.IsMySQL;
  actExplainAnalyzeCurrentQuery.Enabled := actExplainCurrentQuery.Enabled;
end;


procedure TMainForm.actExplainAnalyzeCurrentQueryExecute(Sender: TObject);
var
  Conn: TDBConnection;
  SQL: String;
begin
  // Send EXPLAIN output to analyzer
  Conn := ActiveConnection;
  SQL := GetCurrentQuery(ActiveQueryTab);
  try
    Conn.ExplainAnalyzer(SQL, Conn.Database);
  except
    on E:EDbError do
      ErrorDialog(E.Message);
  end;
end;


procedure TMainForm.actExportDataExecute(Sender: TObject);
var
  ExportDialog: TfrmExportGrid;
begin
  // Save data in current dataset into various text file formats
  ExportDialog := TfrmExportGrid.Create(Self);
  ExportDialog.Grid := ActiveGrid;
  ExportDialog.ShowModal;
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
  Results: TDBQuery;
  RowNum: PInt64;
  ImgType: String;
  Content, Header: AnsiString;
  ContentStream: TMemoryStream;
  StrLen: Integer;
  Graphic: TGraphic;
begin
  // Load BLOB contents into preview area
  Grid := ActiveGrid;
  Results := GridResult(Grid);
  if not Assigned(Results) then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    ShowStatusMsg(_('Loading contents into image viewer ...'));
    lblPreviewTitle.Caption := _('Loading ...');
    lblPreviewTitle.Repaint;
    imgPreview.Picture := nil;
    AnyGridEnsureFullRow(Grid, Grid.FocusedNode);
    RowNum := Grid.GetNodeData(Grid.FocusedNode);
    Results.RecNo := RowNum^;

    Content := AnsiString(Results.Col(Grid.FocusedColumn));
    StrLen := Results.ColumnLengths(Grid.FocusedColumn);
    ContentStream := TMemoryStream.Create;
    ContentStream.Write(Content[1], StrLen);
    ContentStream.Position := 0;
    Graphic := nil;
    ContentStream.Position := 0;
    ImgType := 'UnknownType';
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
    end else if Copy(Header, 2, 3) = 'PNG' then begin
      ImgType := 'PNG';
      Graphic := TPngImage.Create;
    end;
    if Assigned(Graphic) then begin
      try
        Graphic.LoadFromStream(ContentStream);
        imgPreview.Picture.Graphic := Graphic;
        lblPreviewTitle.Caption := ImgType+': '+
          IntToStr(Graphic.Width)+' x '+IntToStr(Graphic.Height)+' pixels, 100%, '+
          FormatByteNumber(StrLen);
        spltPreview.OnMoved(spltPreview);
      except
        on E:Exception do
          lblPreviewTitle.Caption := ImgType+': ' + E.Message + ' ('+E.ClassName+')';
      end;
      FreeAndNil(ContentStream);
    end else
      lblPreviewTitle.Caption := f_('No image detected, %s', [FormatByteNumber(StrLen)]);
  finally
    lblPreviewTitle.Hint := lblPreviewTitle.Caption;
    ShowStatusMsg;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.pnlLeftResize(Sender: TObject);
var
 WidthAvail: Integer;
begin
  WidthAvail := editDatabaseFilter.Parent.Width - btnTreeFavorites.Width;
  editDatabaseFilter.Left := 0;
  editDatabaseFilter.Width := (WidthAvail div 2) - 1;
  editTableFilter.Width := editDatabaseFilter.Width;
  editTableFilter.Left := editDatabaseFilter.Width + 1;
  btnTreeFavorites.Left := editTableFilter.Left + editTableFilter.Width + 1;
  spltPreview.OnMoved(Sender);
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
  Results: TDBQuery;
  RowNum: PInt64;
  Content: AnsiString;
  FileStream: TFileStream;
  StrLen: Integer;
  Dialog: TSaveDialog;
begin
  // Save BLOB to local file
  Grid := ActiveGrid;
  Results := GridResult(Grid);
  Dialog := TSaveDialog.Create(Self);
  Dialog.Filter := _('All files')+' (*.*)|*.*';
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
      ErrorDialog(E.Message);
    end;
    FreeAndNil(FileStream);
    Screen.Cursor := crDefault;
  end;
  Dialog.Free;
end;


procedure TMainForm.actInsertFilesExecute(Sender: TObject);
var
  Dialog: TfrmInsertFiles;
begin
  Dialog := TfrmInsertFiles.Create(Self);
  Dialog.ShowModal;
end;

// Drop Table(s)
procedure TMainForm.actDropObjectsExecute(Sender: TObject);
var
  msg, db: String;
  Node: PVirtualNode;
  Obj: PDBObject;
  DBObject: TDBObject;
  ObjectList: TDBObjectList;
  Editor: TDBObjectEditor;
  Conn: TDBConnection;
  DisableForeignKeys: Boolean;
begin
  Conn := ActiveConnection;

  ObjectList := TDBobjectList.Create(TDBObjectDropComparer.Create, False);

  if DBTreeClicked(Sender) then begin
    // drop table selected in tree view.
    DBObject := ActiveDBObj;
    case DBObject.NodeType of
      lntDb: begin
        if MessageDialog(f_('Drop Database "%s"?', [DBObject.Database]), f_('WARNING: You will lose all objects in database %s!', [DBObject.Database]), mtCriticalConfirmation, [mbok,mbcancel]) <> mrok then
          Abort;
        try
          db := DBObject.Database;
          Node := FindDBNode(DBtree, Conn, db);
          SetActiveDatabase('', Conn);
          Conn.Query('DROP DATABASE ' + Conn.QuoteIdent(db));
          DBtree.DeleteNode(Node);
          Conn.ClearDbObjects(db);
          Conn.RefreshAllDatabases;
          InvalidateVT(ListDatabases, VTREE_NOTLOADED_PURGECACHE, False);
        except
          on E:EDbError do
            ErrorDialog(E.Message);
        end;
        Exit;
      end;
      lntTable..lntEvent: ObjectList.Add(ActiveDBObj);
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
  msg := '';
  for DBObject in ObjectList do
    msg := msg + DBObject.Name + ', ';
  Delete(msg, Length(msg)-1, 2);
  if MessageDialog(f_('Drop %d object(s) in database "%s"?', [ObjectList.Count, Conn.Database]), msg, mtCriticalConfirmation, [mbok,mbcancel]) = mrOk then begin
    try
      // Disable foreign key checks to avoid SQL errors
      DisableForeignKeys := (Conn.Parameters.NetTypeGroup = ngMySQL) and (Conn.ServerVersionInt >= 40014);
      if DisableForeignKeys then
        Conn.Query('SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0');
      // Compose and run DROP [TABLE|VIEW|...] queries
      Editor := ActiveObjectEditor;
      for DBObject in ObjectList do begin
        DBObject.Drop;
        if Assigned(Editor) and Editor.Modified and Editor.DBObject.IsSameAs(DBObject) then
          Editor.Modified := False;
      end;
      if DisableForeignKeys then
        Conn.Query('SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS');
      // Refresh ListTables + dbtree so the dropped tables are gone:
      Conn.ClearDbObjects(ActiveDatabase);
      RefreshTree;
      SetActiveDatabase(Conn.Database, Conn);
    except
      on E:EDbError do
        ErrorDialog(E.Message);
    end;
    ObjectList.Free;
  end;
end;


procedure TMainForm.actLaunchCommandlineExecute(Sender: TObject);
var
  path, p, log, cmd: String;
  sep: Char;
  Conn: TDBConnection;
  PasswordStart, PasswordEnd: Integer;
begin
  // Launch mysql.exe
  Conn := ActiveConnection;
  if not Conn.Parameters.IsMySQL then
    ErrorDialog(_('Command line only works on MySQL connections.'))
  else begin
    if FIsWine then begin
      cmd := 'mysql';
      sep := '/';
    end else begin
      cmd := 'mysql.exe';
      sep := '\';
    end;
    path := AppSettings.ReadString(asMySQLBinaries);
    if (Length(path)>0) and (path[Length(path)] <> sep) then
      path := path + sep;
    if not FileExists(path+cmd, true) then begin
      ErrorDialog(f_('You need to tell %s where your MySQL binaries reside, in %s > %s > %s.', [APPNAME, _('Tools'), _('Preferences'), _('General')])+
        CRLF+CRLF+f_('Current setting is: "%s"', [path]));
    end else begin
      p := '';
      if FIsWine then begin
        p := ' -e '+path+cmd;
        path := '';
        cmd := '$TERM';
      end;

      if Conn.Parameters.WantSSL then
        p := p + ' --ssl';
      if not Conn.Parameters.SSLPrivateKey.IsEmpty then
        p := p + ' --ssl-key="'+Conn.Parameters.SSLPrivateKey+'"';
      if not Conn.Parameters.SSLCertificate.IsEmpty then
        p := p + ' --ssl-cert="'+Conn.Parameters.SSLCertificate+'"';
      if not Conn.Parameters.SSLCACertificate.IsEmpty then
        p := p + ' --ssl-ca="'+Conn.Parameters.SSLCACertificate+'"';

      case Conn.Parameters.NetType of
        ntMySQL_TCPIP: begin
          p := p + ' --host="'+Conn.Parameters.Hostname+'" --port='+IntToStr(Conn.Parameters.Port);
        end;
        ntMySQL_NamedPipe:
          p := p + ' --pipe --socket="'+Conn.Parameters.Hostname+'"';
        ntMySQL_SSHtunnel:
          p := p + ' --host="localhost" --port='+IntToStr(Conn.Parameters.SSHLocalPort);
      end;

      p := p + ' --user="'+Conn.Parameters.Username+'"';
      // Markers for removing password from log line
      PasswordStart := -1;
      PasswordEnd := -1;
      if Conn.Parameters.Password <> '' then begin
        PasswordStart := Length(path + cmd + p) +14;
        p := p + ' --password="'+StringReplace(Conn.Parameters.Password, '"', '\"', [rfReplaceAll])+'"';
        PasswordEnd := Length(path + cmd + p);
      end;
      if Conn.Parameters.Compressed then
        p := p + ' --compress';
      if ActiveDatabase <> '' then
        p := p + ' --database="' + ActiveDatabase + '"';
      log := path + cmd + p;
      if PasswordStart > -1 then begin
        Delete(log, PasswordStart, PasswordEnd-PasswordStart);
        Insert('********', log, PasswordStart);
      end;
      LogSQL(f_('Launching command line: %s', [log]), lcInfo);
      ShellExec(cmd, path, p);
    end;
  end;
end;


// Load SQL-file, make sure that SheetQuery is activated
procedure TMainForm.actLoadSQLExecute(Sender: TObject);
var
  i: Integer;
  Dialog: TOpenTextFileDialog;
  Encoding: TEncoding;
  Tab: TQueryTab;
  ConsiderActiveTab: Boolean;
begin
  AppSettings.ResetPath;
  Dialog := TOpenTextFileDialog.Create(Self);
  Dialog.Options := Dialog.Options + [ofAllowMultiSelect];
  Dialog.Filter := _('SQL files')+' (*.sql)|*.sql|'+_('All files')+' (*.*)|*.*';
  Dialog.DefaultExt := 'sql';
  Dialog.Encodings.Assign(FileEncodings);
  Dialog.EncodingIndex := AppSettings.ReadInt(asFileDialogEncoding, Self.Name);
  if Dialog.Execute then begin
    Encoding := GetEncodingByName(Dialog.Encodings[Dialog.EncodingIndex]);
    if not RunQueryFiles(Dialog.Files, Encoding, Sender=actRunSQL) then begin
      ConsiderActiveTab := True;
      for i:=0 to Dialog.Files.Count-1 do begin
        Tab := ActiveOrEmptyQueryTab(ConsiderActiveTab);
        ConsiderActiveTab := False;
        Tab.LoadContents(Dialog.Files[i], True, Encoding);
        if i = Dialog.Files.Count-1 then
          SetMainTab(Tab.TabSheet);
      end;
    end;
    AppSettings.WriteInt(asFileDialogEncoding, Dialog.EncodingIndex, Self.Name);
  end;
  Dialog.Free;
end;


function TMainForm.RunQueryFiles(Filenames: TStrings; Encoding: TEncoding; ForceRun: Boolean): Boolean;
var
  i: Integer;
  Filesize, FilesizeSum: Int64;
  msgtext: String;
  AbsentFiles, PopupFileList: TStringList;
  DoRunFiles: Boolean;
  Dialog: TTaskDialog;
  Btn: TTaskDialogButtonItem;
  DialogResult: TModalResult;
const
  RunFileSize = 5*SIZE_MB;
begin
  // Ask for execution when loading big files, or return false
  Result := False;

  // Remove non existant files
  AbsentFiles := TStringList.Create;
  for i:=Filenames.Count-1 downto 0 do begin
    if not FileExists(Filenames[i]) then begin
      AbsentFiles.Add(Filenames[i]);
      Filenames.Delete(i);
    end;
  end;
  // Check if one or more files are large
  DoRunFiles := ForceRun;
  PopupFileList := TStringList.Create;
  FilesizeSum := 0;
  for i:=0 to Filenames.Count-1 do begin
    FileSize := _GetFileSize(Filenames[i]);
    Inc(FilesizeSum, Filesize);
    PopupFileList.Add(ExtractFilename(Filenames[i]) + ' (' + FormatByteNumber(FileSize) + ')');
    DoRunFiles := DoRunFiles or (FileSize > RunFileSize);
  end;

  if DoRunFiles then begin
    if ForceRun then begin
      // Don't ask, just run files
      DialogResult := mrYes;
    end else if (Win32MajorVersion >= 6) and StyleServices.Enabled then begin
      Dialog := TTaskDialog.Create(Self);
      Dialog.Caption := _('Opening large files');
      Dialog.Text := f_('Selected files have a size of %s', [FormatByteNumber(FilesizeSum, 1)]);
      Dialog.ExpandButtonCaption := _('File list');
      Dialog.ExpandedText := PopupFileList.Text;
      Dialog.Flags := [tfUseCommandLinks, tfExpandFooterArea];
      Dialog.CommonButtons := [];
      Dialog.MainIcon := tdiWarning;
      Btn := TTaskDialogButtonItem(Dialog.Buttons.Add);
      Btn.Caption := _('Run file(s) directly');
      Btn.CommandLinkHint := _('... without loading into the editor');
      Btn.ModalResult := mrYes;
      Btn := TTaskDialogButtonItem(Dialog.Buttons.Add);
      Btn.Caption := _('Load file(s) into the editor');
      Btn.CommandLinkHint := _('Can cause large memory usage');
      Btn.ModalResult := mrNo;
      Btn := TTaskDialogButtonItem(Dialog.Buttons.Add);
      Btn.Caption := _('Cancel');
      Btn.ModalResult := mrCancel;
      Dialog.Execute;
      DialogResult := Dialog.ModalResult;
      Dialog.Free;
    end else begin
      msgtext := f_('One or more of the selected files are larger than %s:', [FormatByteNumber(RunFileSize, 0)]) + CRLF +
        ImplodeStr(CRLF, PopupFileList) + CRLF + CRLF +
        _('Just run these files to avoid loading them into the query-editor (= memory)?') + CRLF + CRLF +
        _('Press') + CRLF +
        _('  [Yes] to run file(s) without loading it into the editor') + CRLF +
        _('  [No] to load file(s) into the query editor') + CRLF +
        _('  [Cancel] to cancel file opening.');
      DialogResult := MessageDialog(_('Execute query file(s)?'), msgtext, mtWarning, [mbYes, mbNo, mbCancel]);
    end;

    case DialogResult of
      mrYes: begin
        Result := True;
        for i:=0 to Filenames.Count-1 do begin
          RunQueryFile(Filenames[i], Encoding);
          // Add filename to history menu
          if Pos(AppSettings.DirnameSnippets, Filenames[i]) = 0 then
            MainForm.AddOrRemoveFromQueryLoadHistory(Filenames[i], True, True);
        end;
      end;
      mrNo: Result := False;
      mrCancel: Result := True;
    end;
  end;

  if AbsentFiles.Count > 0 then
    ErrorDialog(_('Could not load file(s):'), AbsentFiles.Text);
  AbsentFiles.Free;
  PopupFileList.Free;
end;


procedure TMainForm.RunQueryFile(FileName: String; Encoding: TEncoding);
var
  Dialog: IProgressDialog;
  Dummy: Pointer;
  Stream: TFileStream;
  Lines, LinesRemain, ErrorNotice: String;
  Filesize, QueryCount, ErrorCount, RowsAffected, Position: Int64;
  Queries: TSQLBatch;
  i: Integer;
  LastProcessMessages: Cardinal;

  procedure StopProgress;
  begin
    Dialog.SetLine(1, PChar(_('Clean up ...')), False, Dummy);
    Queries.Free;
    Stream.Free;
    Dialog.StopProgressDialog;
    // BringToFront; // Not sure why I added this initially, but it steals focus from other applications
    SetFocus;
  end;

begin
  // Import single SQL file and display progress dialog
  Dialog := CreateComObject(CLSID_ProgressDialog) as IProgressDialog;
  Dialog.SetTitle(PChar(f_('Importing file %s', [ExtractFileName(FileName)])));
  Dummy := nil;
  Dialog.StartProgressDialog(Handle, nil, PROGDLG_MODAL or PROGDLG_NOMINIMIZE or PROGDLG_AUTOTIME, Dummy);

  Lines := '';
  ErrorNotice := '';
  QueryCount := 0;
  ErrorCount := 0;
  Position := 0;
  RowsAffected := 0;
  LastProcessMessages := GetTickCount;
  LinesRemain := '';
  Queries := TSQLBatch.Create;

  try
    // Start file operations
    Filesize := _GetFileSize(FileName);

    OpenTextfile(FileName, Stream, Encoding);
    while Stream.Position < Stream.Size do begin
      if Dialog.HasUserCancelled then
        Break;

      // Read lines from SQL file until buffer reaches a limit of some MB
      // This strategy performs vastly better than looping through each line
      Dialog.SetLine(1, PChar(_('Reading next chunk from file...')), False, Dummy);
      Lines := ReadTextfileChunk(Stream, Encoding, 20*SIZE_MB);

      // Split buffer into single queries
      Dialog.SetLine(1, PChar(_('Splitting queries...')), False, Dummy);
      Queries.SQL := LinesRemain + Lines;
      Lines := '';
      LinesRemain := '';

      // Execute detected queries
      for i:=0 to Queries.Count-1 do begin
        if Dialog.HasUserCancelled then
          Break;
        // Last line has to be processed in next loop if end of file is not reached
        if (i = Queries.Count-1) and (Stream.Position < Stream.Size) then begin
          LinesRemain := Queries[i].SQL;
          Break;
        end;
        Inc(QueryCount);
        Position := Position + Encoding.GetByteCount(Queries[i].SQL);
        if ErrorCount > 0 then
          ErrorNotice := '(' + FormatNumber(ErrorCount) + ' ' + _('Errors') + ')';
        Dialog.SetLine(1,
          PChar(f_('Processing query #%s. %s', [FormatNumber(QueryCount), ErrorNotice])),
          False,
          Dummy
          );
        Dialog.SetLine(2,
          PChar(f_('Position in file: %s / %s. Affected rows: %s.', [FormatByteNumber(Position), FormatByteNumber(Filesize), FormatNumber(RowsAffected)])),
          False,
          Dummy
          );
        Dialog.SetProgress64(Position, FileSize);
        if GetTickCount - LastProcessMessages > 1000 then begin
          Application.ProcessMessages;
          LastProcessMessages := GetTickCount;
        end;

        // Execute single query
        // Break or don't break loop, depending on the state of "Stop on errors" button
        try
          ActiveConnection.Query(Queries[i].SQL, False, lcScript);
          RowsAffected := RowsAffected + ActiveConnection.RowsAffected;
        except
          on E:Exception do begin
            if actQueryStopOnErrors.Checked then
              raise
            else
              Inc(ErrorCount);
          end;
        end;

      end;
    end;
    StopProgress;
    if ErrorCount > 0 then begin
      ErrorDialog(_('Errors'),
        f_('%s%% of your file has been processed, but there were %s errors when executing %s queries. Please check the SQL log panel for messages.',
          [FormatNumber(100/FileSize*Position, 0), FormatNumber(ErrorCount), FormatNumber(QueryCount)])
        );
    end;

  except
    on E:EFileStreamError do begin
      StopProgress;
      ErrorDialog(f_('Error while reading file "%s"', [FileName]), E.Message);
      AddOrRemoveFromQueryLoadHistory(FileName, False, True);
    end;
    on E:EDbError do begin
      StopProgress;
      ErrorDialog(E.Message + CRLF + CRLF +
        f_('Notice: You can disable the "%s" option to ignore such errors', [actQueryStopOnErrors.Caption])
        );
    end;
    on E:EEncodingError do begin
      StopProgress;
      ErrorDialog(E.Message);
    end;
  end;
end;


procedure TMainForm.SessionConnect(Sender: TObject);
var
  SessionPath: String;
  Connection: TDBConnection;
  Params: TConnectionParameters;
  Node, SessionNode: PVirtualNode;
  DBObj: PDBObject;
  i: Integer;
begin
  // Click on quick-session menu item:
  SessionPath := StripHotkey((Sender as TMenuItem).Caption);
  Node := nil;
  // Probably wanted session was clicked before: navigate to last node
  for i:=High(FTreeClickHistory) downto Low(FTreeClickHistory) do begin
    if FTreeClickHistory[i] <> nil then begin
      DBObj := DBtree.GetNodeData(FTreeClickHistory[i]);
      if DBObj.Connection.Parameters.SessionPath = SessionPath then begin
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
      if DBObj.Connection.Parameters.SessionPath = SessionPath then begin
        Node := SessionNode;
      end;
      SessionNode := DBtree.GetNextSibling(SessionNode);
    end;
  end;
  // Finally we have a node if session is already connected
  if Assigned(Node) then
    SelectNode(DBtree, Node)
  else begin
    Params := TConnectionParameters.Create(SessionPath);
    InitConnection(Params, True, Connection);
  end;
end;


{**
  Receive connection parameters and create a connection tree node
  Paremeters are either sent by connection-form or by commandline.
}
function TMainform.InitConnection(Params: TConnectionParameters; ActivateMe: Boolean; var Connection: TDBConnection): Boolean;
var
  RestoreLastActiveDatabase: Boolean;
  StartupScript, LastActiveDatabase: String;
  StartupBatch: TSQLBatch;
  Query: TSQLSentence;
  SessionNode, DBNode: PVirtualNode;
begin
  Connection := Params.CreateConnection(Self);
  Connection.OnLog := LogSQL;
  Connection.OnConnected := ConnectionReady;
  Connection.OnDatabaseChanged := DatabaseChanged;
  Connection.OnObjectnamesChanged := ObjectnamesChanged;
  try
    Connection.Active := True;
    // We have a connection
    Result := True;
    FConnections.Add(Connection);

    if AppSettings.SessionPathExists(Params.SessionPath) then begin
      // Save "connected" counter
      AppSettings.SessionPath := Params.SessionPath;
      AppSettings.WriteInt(asConnectCount, AppSettings.ReadInt(asConnectCount)+1);
      // Save server version
      AppSettings.WriteInt(asServerVersion, Connection.ServerVersionInt);
      AppSettings.WriteString(asLastConnect, DateTimeToStr(Now));
    end;

    if ActivateMe then begin
      // Set focus on last uses db. If not wanted or db is gone, go to root node at least
      RestoreLastActiveDatabase := AppSettings.ReadBool(asRestoreLastUsedDB);
      AppSettings.SessionPath := Params.SessionPath;
      LastActiveDatabase := AppSettings.ReadString(asLastUsedDB);
      if RestoreLastActiveDatabase
        and (Connection.AllDatabases.IndexOf(LastActiveDatabase) >- 1)
        and (Connection.GetLockedTableCount(LastActiveDatabase) = 0)
        then begin
        SetActiveDatabase(LastActiveDatabase, Connection);
        DBNode := FindDBNode(DBtree, Connection, LastActiveDatabase);
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
      StartupScript := ExpandFileName(StartupScript);
      if not FileExists(StartupScript) then
        ErrorDialog(f_('Startup script file not found: %s', [StartupScript]))
      else begin
        StartupBatch := TSQLBatch.Create;
        StartupBatch.SQL := ReadTextfile(StartupScript, nil);
        for Query in StartupBatch do try
          Connection.Query(Query.SQL);
        except
          // Suppress popup, errors get logged into SQL log
        end;
        StartupBatch.Free;
      end;
    end;

    if Params.WantSSL and not Connection.IsSSL then begin
      MessageDialog(_('SSL not used.'),
        _('Your SSL settings were not accepted by the server, or the server does not support any SSL configuration.'),
        mtWarning,
        [mbOK]
        );
    end;

    // Apply favorite object paths
    AppSettings.SessionPath := Params.SessionPath;
    Connection.Favorites.Text := AppSettings.ReadString(asFavoriteObjects);

    // Tree node filtering needs a hit once when connected
    editDatabaseTableFilterChange(Self);

  except
    on E:EDbError do begin
      ErrorDialog(E.Message);
      // attempt failed
      if AppSettings.SessionPathExists(Params.SessionPath) then begin
        // Save "refused" counter
        AppSettings.SessionPath := Params.SessionPath;
        AppSettings.WriteInt(asRefusedCount, AppSettings.ReadInt(asRefusedCount)+1);
      end;
      Result := False;
      FreeAndNil(Connection);
    end;
  end;

  StoreLastSessions;
  ShowStatusMsg;
end;


procedure TMainForm.actDataDeleteExecute(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Node, FocusAfterDelete: PVirtualNode;
  RowNum: PInt64;
  Results: TDBQuery;
  Nodes: TNodeArray;
  i: Integer;
begin
  // Delete row(s)
  Grid := ActiveGrid;
  Results := GridResult(Grid);
  if Grid.SelectedCount = 0 then
    ErrorDialog(_('No rows selected'), _('Please select one or more rows to delete them.'))
  else try
    Results.CheckEditable;
    if MessageDialog(f_('Delete %s row(s)?', [FormatNumber(Grid.SelectedCount)]),
      mtConfirmation, [mbOK, mbCancel]) = mrOK then begin
      FocusAfterDelete := nil;
      EnableProgress(Grid.SelectedCount);
      Node := GetNextNode(Grid, nil, True);
      while Assigned(Node) do begin
        RowNum := Grid.GetNodeData(Node);
        ShowStatusMsg(f_('Deleting row #%s of %s ...', [FormatNumber(ProgressBarStatus.Position+1), FormatNumber(ProgressBarStatus.Max)]));
        Results.RecNo := RowNum^;
        Results.DeleteRow;
        ProgressStep;
        SetLength(Nodes, Length(Nodes)+1);
        Nodes[Length(Nodes)-1] := Node;
        FocusAfterDelete := Node;
        Node := GetNextNode(Grid, Node, True);
      end;
      ShowStatusMsg(_('Clean up ...'));
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
  except on E:EDbError do begin
      SetProgressState(pbsError);
      ErrorDialog(_('Grid editing error'), E.Message);
    end;
  end;
  DisableProgress;
  ShowStatusMsg();
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
  FFocusedTables := GetFocusedObjects(Sender, [lntTable]);
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
  TableOrView: TDBObject;
  Objects: TDBObjectList;
  Names: String;
begin
  // Delete rows from selected tables and views

  // See issue #3166
  if (not DBtree.Focused) and (not ListTables.Focused) then
    Exit;

  Objects := GetFocusedObjects(Sender, [lntTable, lntView]);
  for TableOrView in Objects do begin
    Names := Names + TableOrView.Name + ', ';
  end;
  Delete(Names, Length(Names)-1, 2);

  if Objects.Count = 0 then
    ErrorDialog(_('No table(s) selected.'))
  else begin
    if MessageDialog(f_('Empty %d table(s) and/or view(s)?', [Objects.count]), Names,
      mtConfirmation, [mbOk, mbCancel]) = mrOk then begin
      Screen.Cursor := crHourglass;
      EnableProgress(Objects.Count);
      try
        for TableOrView in Objects do begin
          TableOrView.Connection.Query(TableOrView.Connection.GetSQLSpecifity(spEmptyTable) + TableOrView.QuotedName);
          ProgressStep;
        end;
        actRefresh.Execute;
      except
        on E:EDbError do begin
          SetProgressState(pbsError);
          ErrorDialog(E.Message);
        end;
      end;
      Objects.Free;
      DisableProgress;
      Screen.Cursor := crDefault;
    end;
  end;
end;


procedure TMainForm.actBatchInOneGoExecute(Sender: TObject);
begin
  //
end;


function TMainForm.DBTreeClicked(Sender: TObject): Boolean;
var
  ClickedControl: TComponent;
  ClickedMenu: TMenu;
begin
  // Find out if user rightclicked in tree or in database tab,
  // which is a bit complex, so outsourced here.
  Result := DBTree.Focused or (PageControlMain.ActivePage <> tabDatabase);
  if Sender is TAction then begin
    ClickedControl := (Sender as TAction).ActionComponent;
    if ClickedControl is TMenuItem then begin
      ClickedMenu := (ClickedControl as TMenuItem).GetParentMenu;
      if ClickedMenu is TPopupMenu then
        Result := (ClickedMenu as TPopupMenu).PopupComponent = DBTree;
    end;
  end else if Sender is TPopupMenu then begin
    Result := (Sender as TPopupMenu).PopupComponent = DBTree;
  end else if Sender is TMenuItem then begin
    ClickedMenu := (Sender as TMenuItem).GetParentMenu;
    if ClickedMenu is TPopupMenu then
      Result := (ClickedMenu as TPopupMenu).PopupComponent = DBTree;
  end;
end;


function TMainForm.GetFocusedObjects(Sender: TObject; NodeTypes: TListNodeTypes): TDBObjectList;
var
  Node: PVirtualNode;
  pObj: PDBObject;
begin
  // Return list of selected database objects in current area
  Result := TDBObjectList.Create(False);

  if DBTreeClicked(Sender) then begin
    if ActiveDbObj.NodeType in NodeTypes then
      Result.Add(ActiveDbObj);
  end else begin
    Node := GetNextNode(ListTables, nil, True);
    while Assigned(Node) do begin
      pObj := ListTables.GetNodeData(Node);
      if pObj.NodeType in NodeTypes then
        Result.Add(pObj^);
      Node := GetNextNode(ListTables, Node, True);
    end;
  end;
end;


procedure TMainForm.actRunRoutinesExecute(Sender: TObject);
var
  Tab: TQueryTab;
  Query, ParamValues, ParamValue: String;
  Params: TStringList;
  Obj: TDBObject;
  Objects: TDBObjectList;
  Parameters: TRoutineParamList;
  Param: TRoutineParam;
  Cancelled: Boolean;
begin
  // Run stored function(s) or procedure(s)
  Objects := GetFocusedObjects(Sender, [lntProcedure, lntFunction]);

  if Objects.Count = 0 then
    ErrorDialog(_('No stored procedure selected.'), _('Please select one or more stored function(s) or routine(s).'));

  for Obj in Objects do begin
    actNewQueryTab.Execute;
    Tab := QueryTabs[MainForm.QueryTabs.Count-1];
    case Obj.Connection.Parameters.NetTypeGroup of
      ngMySQL:
        case Obj.NodeType of
          lntProcedure: Query := 'CALL ';
          lntFunction: Query := 'SELECT ';
        end;
      ngMSSQL:
        Query := 'EXEC ';
      ngPgSQL:
        Query := 'SELECT ';
      else
        raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(Obj.Connection.Parameters.NetType)]);
    end;
    Parameters := TRoutineParamList.Create;
    Obj.Connection.ParseRoutineStructure(Obj, Parameters);
    Query := Query + Obj.QuotedName;
    Cancelled := False;
    Params := TStringList.Create;
    for Param in Parameters do begin
      ParamValue := '';
      if not InputQuery(Obj.Name, _('Parameter')+' "'+Param.Name+'" ('+Param.Datatype+')', ParamValue) then begin
        Cancelled := True;
        Break;
      end;
      ParamValue := Obj.Connection.EscapeString(ParamValue);
      Params.Add(ParamValue);
    end;
    if not Cancelled then begin
      Parameters.Free;
      ParamValues := '';
      case Obj.Connection.Parameters.NetTypeGroup of
        ngMySQL, ngPgSQL:
          ParamValues := '(' + ImplodeStr(', ', Params) + ')';
        ngMSSQL:
          ParamValues := ' ' + ImplodeStr(' ', Params);
        else
          raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(Obj.Connection.Parameters.NetType)]);
      end;
      Query := Query + ParamValues;
      Tab.Memo.Text := Query;
      actExecuteQueryExecute(Sender);
    end;
    // Also cancel the whole loop over multiple procedures
    if Cancelled then
      Break;
  end;
end;


procedure TMainForm.actNewWindowExecute(Sender: TObject);
begin
  ShellExec( ExtractFileName(paramstr(0)), ExtractFilePath(paramstr(0)) );
end;


procedure TMainForm.actQueryFindReplaceExecute(Sender: TObject);
var
  OldDataLocalNumberFormat: Boolean;
begin
  // Display search + replace dialog
  if not Assigned(FSearchReplaceDialog) then
    FSearchReplaceDialog := TfrmSearchReplace.Create(Self);
  FSearchReplaceDialog.chkReplace.Checked := Sender = actQueryReplace;
  if (ActiveSynMemo(False) <> nil) or (ActiveGrid <> nil) then begin
    OldDataLocalNumberFormat := DataLocalNumberFormat;
    DataLocalNumberFormat := False;
    FSearchReplaceDialog.ShowModal;
    DataLocalNumberFormat := OldDataLocalNumberFormat;
  end;
end;


procedure TMainForm.actQueryFindAgainExecute(Sender: TObject);
var
  NeedDialog: Boolean;
  Editor: TSynMemo;
  Grid: TVirtualStringTree;
  OldDataLocalNumberFormat: Boolean;
begin
  // F3 - search or replace again, using previous settings
  NeedDialog := not Assigned(FSearchReplaceDialog);
  if Assigned(FSearchReplaceDialog) then begin
    NeedDialog := NeedDialog or ((FSearchReplaceDialog.Grid = nil) and (FSearchReplaceDialog.Editor = nil));
    Editor := ActiveSynMemo(False);
    Grid := ActiveGrid;
    NeedDialog := NeedDialog or ((Grid = nil) and (Editor = nil));
    if (Editor <> nil) and (Editor.Focused) then
      NeedDialog := NeedDialog or (FSearchReplaceDialog.Editor<>Editor);
    if (Grid <> nil) and (Grid.Focused) then
      NeedDialog := NeedDialog or (FSearchReplaceDialog.Grid<>Grid);
  end;

  if NeedDialog then
    actQueryFindReplaceExecute(Sender)
  else begin
    OldDataLocalNumberFormat := DataLocalNumberFormat;
    DataLocalNumberFormat := False;
    Exclude(FSearchReplaceDialog.Options, ssoEntireScope);
    FSearchReplaceDialog.DoSearchReplace(Sender);
    DataLocalNumberFormat := OldDataLocalNumberFormat;
  end;
end;


procedure TMainForm.SynMemoQueryReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
begin
  // Fires when "Replace all" in search dialog was pressed with activated "Prompt on replace"
  case MessageDialog(f_('Replace this occurrence of "%s"?', [sstr(ASearch, 100)]), mtConfirmation, [mbYes, mbYesToAll, mbNo, mbCancel]) of
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
  OldDbObject: TDBObject;
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
  end else if tab1 = tabDatabase then begin
    OldDbObject := TDBObject.Create(FActiveDbObj.Connection);
    OldDbObject.Assign(FActiveDbObj);
    RefreshTree(OldDbObject);
  end else if tab1 = tabData then
    InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
end;


procedure TMainForm.actSQLhelpExecute(Sender: TObject);
var
  keyword: String;
  Tree: TVirtualStringTree;
  SynMemo: TSynMemo;
begin
  // Call SQL Help from various places
  keyword := '';

  // Query-Tab
  if ActiveControl is TSynMemo then begin
    SynMemo := TSynMemo(ActiveControl);
    keyword := SynMemo.WordAtCursor;
    if keyword.IsEmpty then begin
      keyword := SynMemo.SelText;
    end;
  end

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


procedure TMainForm.actSynchronizeDatabaseExecute(Sender: TObject);
var
  SyncForm: TfrmSyncDB;
begin
  SyncForm := TfrmSyncDB.Create(Self);
  SyncForm.ShowModal;
end;


{***
  Show SQL Help window directly using a keyword
  @param String SQL-keyword
  @see FieldeditForm.btnDatatypeHelp
}
procedure TMainform.CallSQLHelpWithKeyword( keyword: String );
begin
  if FActiveDbObj.Connection.ServerVersionInt >= 40100 then begin
    SqlHelpDialog := TfrmSQLhelp.Create(Self);
    SqlHelpDialog.Show;
    SqlHelpDialog.Keyword := keyword;
  end else
    ErrorDialog(_('SQL help not available.'), f_('HELP <keyword> requires %s or newer.', ['MySQL 4.1']));
end;


procedure TMainForm.actSaveSynMemoToTextfileExecute(Sender: TObject);
var
  Comp: TComponent;
  Memo: TSynMemo;
  Dialog: TSaveDialog;
  Item: TMenuItem;
begin
  // Save to textfile, from any TSynMemo (SQL log, "CREATE code" tab in editor, ...)
  Item := (Sender as TAction).ActionComponent as TMenuItem;
  Comp := (Item.GetParentMenu as TPopupMenu).PopupComponent;
  // Try to find memo from menu item's popup component, and if that fails, check ActiveControl.
  // See #353
  if Assigned(Comp) and (Comp is TSynMemo) then begin
    Memo := Comp as TSynMemo;
  end else if ActiveControl is TSynMemo then begin
    Memo := ActiveControl as TSynMemo;
  end;
  if Assigned(Memo) then begin
    Dialog := TSaveDialog.Create(Self);
    Dialog.Options := Dialog.Options + [ofOverwritePrompt];
    Dialog.Filter := _('SQL files')+' (*.sql)|*.sql|'+_('All files')+' (*.*)|*.*';
    Dialog.DefaultExt := 'sql';
    if Dialog.Execute then begin
      Screen.Cursor := crHourGlass;
      SaveUnicodeFile(Dialog.FileName, Memo.Text);
      Screen.Cursor := crDefault;
    end;
  end else begin
    ErrorDialog(f_('No SQL editor focused. ActiveControl is %s', [ActiveControl.Name]));
  end;
end;


procedure TMainForm.actSaveSQLAsExecute(Sender: TObject);
var
  i: Integer;
  CanSave: TModalResult;
  OnlySelection: Boolean;
  SaveDialog: TSaveDialog;
begin
  // Save SQL
  CanSave := mrNo;
  SaveDialog := TSaveDialog.Create(Self);
  SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
  if (Sender = actSaveSQLSnippet) or (Sender = actSaveSQLSelectionSnippet) then begin
    SaveDialog.InitialDir := AppSettings.DirnameSnippets;
    SaveDialog.Options := SaveDialog.Options + [ofNoChangeDir];
    SaveDialog.Title := _('Save snippet');
  end;
  SaveDialog.Filter := _('SQL files')+' (*.sql)|*.sql|'+_('All files')+' (*.*)|*.*';
  SaveDialog.DefaultExt := 'sql';
  while (CanSave = mrNo) and SaveDialog.Execute do begin
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    CanSave := mrYes;
    for i:=0 to QueryTabs.Count-1 do begin
      if QueryTabs[i].MemoFilename = SaveDialog.FileName then begin
        CanSave := MessageDialog(f_('Overwrite "%s"?', [SaveDialog.FileName]), f_('This file is already open in query tab #%d.', [QueryTabs[i].Number]),
          mtWarning, [mbYes, mbNo, mbCancel]);
        break;
      end;
    end;
  end;
  if CanSave = mrYes then begin
    OnlySelection := (Sender = actSaveSQLselection) or (Sender = actSaveSQLSelectionSnippet);
    ActiveQueryTab.SaveContents(SaveDialog.FileName, OnlySelection);
    for i:=0 to QueryTabs.Count-1 do begin
      if QueryTabs[i] = ActiveQueryTab then
        continue;
      if QueryTabs[i].MemoFilename = SaveDialog.FileName then
        QueryTabs[i].Memo.Modified := True;
    end;
    ValidateQueryControls(Sender);
    SetSnippetFilenames;
  end;
  SaveDialog.Free;
end;


procedure TMainForm.actSaveSQLExecute(Sender: TObject);
var
  i: Integer;
begin
  if ActiveQueryTab.MemoFilename <> '' then begin
    ActiveQueryTab.SaveContents(ActiveQueryTab.MemoFilename, False);
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


procedure TMainForm.actQueryStopOnErrorsExecute(Sender: TObject);
begin
  // Weird fix: dummy routine to avoid the sending action getting disabled
end;


procedure TMainForm.actQueryWordWrapExecute(Sender: TObject);
begin
  // SetupSynEditors applies all customizations to any SynEditor
  SetupSynEditors;
end;


procedure TMainForm.PopupQueryLoadPopup(Sender: TObject);
var
  i, j: Integer;
  Item, SnippetsFolder: TMenuItem;
  Filename: String;
begin
  // Fill the popupQueryLoad menu
  popupQueryLoad.Items.Clear;

  // Apply shared system image list
  popupQueryLoad.Images := GetSystemImageList;

  // Snippets
  SetSnippetFilenames;
  SnippetsFolder := TMenuItem.Create(popupQueryLoad);
  SnippetsFolder.Caption := _('Snippets');
  popupQueryLoad.Items.Add(SnippetsFolder);
  for i:=0 to FSnippetFilenames.Count-1 do begin
    Item := TMenuItem.Create(SnippetsFolder);
    Item.Caption := FSnippetFilenames[i];
    Item.OnClick := popupQueryLoadClick;
    SnippetsFolder.Add(Item);
  end;

  // Separator
  Item := TMenuItem.Create(popupQueryLoad);
  Item.Caption := '-';
  popupQueryLoad.Items.Add(Item);

  // Recent files
  j := 0;
  for i:=0 to 19 do begin
    Filename := AppSettings.ReadString(asSQLfile, IntToStr(i));
    if Filename = '' then
      continue;
    Inc(j);
    Item := TMenuItem.Create( popupQueryLoad );
    Item.Caption := IntToStr(j) + ' ' + Filename;
    Item.OnClick := popupQueryLoadClick;
    Item.ImageIndex := GetSystemImageIndex(Filename);
    popupQueryLoad.Items.Add(Item);
  end;

  // Separator + "Remove absent files"
  Item := TMenuItem.Create(popupQueryLoad);
  Item.Caption := '-';
  popupQueryLoad.Items.Add(Item);

  Item := TMenuItem.Create(popupQueryLoad);
  Item.Caption := _('Remove absent files');
  Item.OnClick := PopupQueryLoadRemoveAbsentFiles;
  popupQueryLoad.Items.Add(Item);

  Item := TMenuItem.Create(popupQueryLoad);
  Item.Caption := _('Clear file list');
  Item.OnClick := PopupQueryLoadRemoveAllFiles;
  popupQueryLoad.Items.Add(Item);
end;


procedure TMainform.PopupQueryLoadRemoveAbsentFiles(Sender: TObject);
begin
  AddOrRemoveFromQueryLoadHistory('', False, True);
end;


procedure TMainform.PopupQueryLoadRemoveAllFiles(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to 20 do begin
    if not AppSettings.DeleteValue(asSQLfile, IntToStr(i)) then
      break;
  end;
end;


procedure TMainform.popupQueryLoadClick(Sender: TObject);
var
  Filename: String;
  FileList: TStringList;
  p: Integer;
  Tab: TQueryTab;
begin
  // Click on the popupQueryLoad
  Filename := (Sender as TMenuItem).Caption;
  Filename := StripHotkey(Filename);
  if Pos('\', Filename) = 0 then // assuming we load a snippet
    Filename := AppSettings.DirnameSnippets + Filename + '.sql'
  else begin // assuming we load a file from the recent-list
    p := Pos(' ', Filename) + 1;
    filename := Copy(Filename, p, Length(Filename));
  end;
  FileList := TStringList.Create;
  FileList.Add(Filename);
  if not RunQueryFiles(FileList, nil, false) then begin
    Tab := ActiveOrEmptyQueryTab(True);
    Tab.LoadContents(Filename, True, nil);
    SetMainTab(Tab.TabSheet);
  end;
  FileList.Free;
end;


procedure TMainform.AddOrRemoveFromQueryLoadHistory(Filename: String; AddIt: Boolean; CheckIfFileExists: Boolean);
var
  i: Integer;
  newfilelist: TStringList;
  savedfilename: String;
begin
  // Add or remove filename to/from history, avoiding duplicates

  newfilelist := TStringList.create;
  AppSettings.ResetPath;

  // Add new filename
  if AddIt then
    newfilelist.Add( filename );

  // Add all other filenames
  for i:=0 to 20 do begin
    savedfilename := AppSettings.ReadString(asSQLfile, IntToStr(i));
    if IsEmpty(savedfilename) then
      Break;
    AppSettings.DeleteValue(asSQLfile, IntToStr(i));
    if CheckIfFileExists and (not FileExists( savedfilename )) then
      continue;
    if (savedfilename <> filename) and (newfilelist.IndexOf(savedfilename)=-1) then
      newfilelist.add( savedfilename );
  end;

  // Save new list
  for i := 0 to newfilelist.Count-1 do begin
    if i >= 20 then
      break;
    AppSettings.WriteString(asSQLfile, newfilelist[i], IntToStr(i));
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
    if InputQuery(_('Set delimiter'), _('SQL statement delimiter (default is ";"):'), newVal) then try
      // Set new value
      Delimiter := newVal;
      ok := True;
    except on E:Exception do
      ErrorDialog(E.Message);
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
    Msg := _('Empty value.')
  else begin
    rx := TRegExpr.Create;
    rx.Expression := '(/\*|--|#|\''|\"|`)';
    if rx.Exec(Value) then
      Msg := _('Start-of-comment tokens or string literal markers are not allowed.')
  end;
  if Msg <> '' then begin
    Msg := f_('Error setting delimiter to "%s": %s', [Value, Msg]);
    LogSQL(Msg, lcError);
    ErrorDialog(Msg);
  end else begin
    FDelimiter := Value;
    LogSQL(f_('Delimiter changed to %s', [FDelimiter]), lcInfo);
    actSetDelimiter.Hint := actSetDelimiter.Caption + ' (' + _('current value:') + ' ' + FDelimiter + ')';
  end;
end;


procedure TMainForm.actApplyFilterExecute(Sender: TObject);
var
  i: Integer;
  Filters: TStringList;
  val: String;
begin
  // If filter box is empty but filter generator box not, most users expect
  // the filter to be auto generated on button click
  if (SynMemoFilter.GetTextLen = 0) and (editFilterSearch.Text <> '') then
    editFilterSearchChange(editFilterSearch);

  if SynMemoFilter.GetTextLen > 0 then begin
    // Recreate recent filters list
    Filters := TStringList.Create;
    Filters.Add(Trim(SynMemoFilter.Text));
    AppSettings.SessionPath := GetRegKeyTable+'\'+REGKEY_RECENTFILTERS;
    // Add old filters
    for i:=1 to 20 do begin
      val := AppSettings.ReadString(asRecentFilter, IntToStr(i));
      if IsEmpty(val) then
        Continue;
      if Filters.IndexOf(val) = -1 then
        Filters.Add(val);
      AppSettings.DeleteValue(asRecentFilter, IntToStr(i));
    end;
    for i:=0 to Filters.Count-1 do
      AppSettings.WriteString(asRecentFilter, Filters[i], IntToStr(i+1));
    FreeAndNil(Filters);
    AppSettings.ResetPath;
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
  ValidateControls(Sender);
end;


procedure TMainForm.actDataInsertExecute(Sender: TObject);
var
  DupeNode, NewNode: PVirtualNode;
  Grid: TVirtualStringTree;
  Results: TDBQuery;
  RowNum: Int64;
  DupeNum: PInt64;
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
    if (Sender = actDataDuplicateRowWithoutKeys) or (Sender = actDataDuplicateRowWithKeys) then
      DupeNode := Grid.FocusedNode;
    RowNum := Results.InsertRow;
    NewNode := Grid.InsertNode(Grid.FocusedNode, amInsertAfter, PInt64(RowNum));
    SelectNode(Grid, NewNode);
    if Assigned(DupeNode) then begin
      // Copy values from source row, ensure we have whole cell data
      DupeNum := Grid.GetNodeData(DupeNode);
      AnyGridEnsureFullRow(Grid, DupeNode);
      for i:=0 to Grid.Header.Columns.Count-1 do begin
        if not (coVisible in Grid.Header.Columns[i].Options) then
          continue; // Ignore invisible key column
        if Results.ColIsPrimaryKeyPart(i) and (Sender = actDataDuplicateRowWithoutKeys) then
          continue; // Empty value for primary key column
        if Results.ColIsVirtual(i) then
          continue; // Don't copy virtual column value
        Results.RecNo := DupeNum^;
        Value := Results.Col(i);
        IsNull := Results.IsNull(i);
        Results.RecNo := RowNum;
        Results.SetCol(i, Value, IsNull, False);
      end;
    end;
  except on E:EDbError do
    ErrorDialog(_('Grid editing error'), E.Message);
  end;
end;


procedure TMainForm.actDataLastExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Grid: TVirtualStringTree;
begin
  Grid := ActiveGrid;
  // Be sure to have all rows
  if (Grid = DataGrid) and (DatagridWantedRowCount < AppSettings.ReadInt(asDatagridMaximumRows)) then
    actDataShowAll.Execute;
  Node := Grid.GetLast;
  if Assigned(Node) then
    SelectNode(Grid, Node);
  ValidateControls(Sender);
end;


procedure TMainForm.actDataOpenUrlExecute(Sender: TObject);
var
  Grid: TVirtualStringTree;
begin
  // Open grid cell url in web browser
  Grid := ActiveGrid;
  ShellExec(Grid.Text[Grid.FocusedNode, Grid.FocusedColumn]);
end;


procedure TMainForm.actDataPostChangesExecute(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Results: TDBQuery;
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
  Results: TDBQuery;
  RowNum: PInt64;
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


{**
  Add a SQL-command or comment to SynMemoSQLLog
}
procedure TMainForm.LogSQL(Msg: String; Category: TDBLogCategory=lcInfo; Connection: TDBConnection=nil);
var
  snip, IsSQL: Boolean;
  Len, i, MaxLineWidth: Integer;
  Sess, OldSettingsPath: String;
  LogIt: Boolean;
begin
  if not MainFormCreated then
    Exit;
  if csDestroying in ComponentState then
    Exit;

  OldSettingsPath := AppSettings.SessionPath;

  // Log only wanted events
  case Category of
    lcError: LogIt := AppSettings.ReadBool(asLogErrors);
    lcUserFiredSQL: LogIt := AppSettings.ReadBool(asLogUserSQL);
    lcSQL: LogIt := AppSettings.ReadBool(asLogSQL);
    lcScript: LogIt := AppSettings.ReadBool(asLogScript);
    lcInfo: LogIt := AppSettings.ReadBool(asLogInfos);
    lcDebug: LogIt := AppSettings.ReadBool(asLogDebug);
    else LogIt := False;
  end;

  if LogIt then begin
    // Shorten very long messages
    Len := Length(Msg);
    MaxLineWidth := AppSettings.ReadInt(asLogsqlwidth);
    snip := (MaxLineWidth > 0) and (Len > MaxLineWidth);
    IsSQL := Category in [lcSQL, lcUserFiredSQL];
    if snip then begin
      Msg :=
        Copy(Msg, 0, MaxLineWidth) +
        '/* '+f_('large SQL query (%s), snipped at %s characters', [FormatByteNumber(Len), FormatNumber(MaxLineWidth)]) + ' */';
    end else if (not snip) and IsSQL then
      Msg := Msg + Delimiter;
    if not IsSQL then
      Msg := '/* ' + Msg + ' */';

    SynMemoSQLLog.Lines.Add(Msg);

    // Delete first line(s) in SQL log and adjust LineNumberStart in gutter
    i := 0;
    while SynMemoSQLLog.Lines.Count > AppSettings.ReadInt(asLogsqlnum) do begin
      SynMemoSQLLog.Lines.Delete(0);
      Inc(i);
    end;
    // Increase first displayed number in gutter so it doesn't lie about the log entries
    if i > 0 then
      SynMemoSQLLog.Gutter.LineNumberStart := SynMemoSQLLog.Gutter.LineNumberStart + i;

    // Scroll to last line and repaint
    SynMemoSQLLog.GotoLineAndCenter(SynMemoSQLLog.Lines.Count);
    // Causes access violations on a reconnected session firing a user-query:
    // SynMemoSQLLog.Repaint;
    // SynMemoSQLLog.Update;
    // See TDBConnection.Log and TQueryThread.LogFromOutside
    // See https://github.com/HeidiSQL/HeidiSQL/issues/57

    // Log to file?
    if FLogToFile then
    try
      Sess := '';
      if Assigned(Connection) then
        Sess := Connection.Parameters.SessionPath;
      WriteLn(FFileHandleSessionLog, Format('/* %s [%s] */ %s', [DateTimeToStr(Now), Sess, msg]));
    except
      on E:Exception do begin
        LogToFile := False;
        AppSettings.WriteBool(asLogToFile, False);
        ErrorDialog(_('Error writing to session log file.'), E.Message+CRLF+_('Filename')+': '+FFileNameSessionLog+CRLF+CRLF+_('Logging is disabled now.'));
      end;
    end;
  end;

  // Restore possibly overwritten session path
  AppSettings.SessionPath := OldSettingsPath;
end;


procedure TMainForm.actDataShowNextExecute(Sender: TObject);
var
  OldRowCount: Int64;
begin
  // Show next X rows in datagrid
  OldRowCount := DatagridWantedRowCount;
  Inc(DatagridWantedRowCount, AppSettings.ReadInt(asDatagridRowsPerStep));
  DataGridWantedRowCount := Min(DataGridWantedRowCount, AppSettings.ReadInt(asDatagridMaximumRows));
  InvalidateVT(DataGrid, VTREE_NOTLOADED, True);
  SelectNode(DataGrid, OldRowCount);
end;


procedure TMainForm.actDataShowAllExecute(Sender: TObject);
begin
  // Remove LIMIT clause
  DatagridWantedRowCount := AppSettings.ReadInt(asDatagridMaximumRows);
  InvalidateVT(DataGrid, VTREE_NOTLOADED, True);
end;


function TMainForm.AnyGridEnsureFullRow(Grid: TVirtualStringTree; Node: PVirtualNode): Boolean;
var
  RowNum: PInt64;
  Data: TDBQuery;
begin
  // Load remaining data on a partially loaded row in data grid
  Result := True;
  if (Grid = DataGrid) and Assigned(Node) then begin
    RowNum := Grid.GetNodeData(Node);
    Data := GridResult(Grid);
    Data.RecNo := RowNum^;
    Result := Data.EnsureFullRow(False);
  end;
end;


procedure TMainForm.DataGridEnsureFullRows(Grid: TVirtualStringTree; SelectedOnly: Boolean);
var
  Node: PVirtualNode;
  Results: TDBQuery;
  RowNum: PInt64;
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
  i, ColLen, ColWidth, VisibleColumns, MaximumRows, FullColumnCount: Integer;
  Offset: Int64;
  KeyCols, ColWidths, WantedColumnOrgnames: TStringList;
  WantedColumns: TTableColumnList;
  c: TTableColumn;
  OldScrollOffset: TPoint;
  DBObj: TDBObject;
  rx: TRegExpr;

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
    // Column header icons
    for k:=0 to Length(DataGridSortColumns)-1 do begin
      if DataGridSortColumns[k].ColumnName = TblCol.Name then begin
        case DataGridSortColumns[k].SortDirection of
          ORDER_ASC:  col.ImageIndex := 109;
          ORDER_DESC: col.ImageIndex := 110;
        end;
      end;
    end;
    if col.ImageIndex = -1 then begin
      for k:=0 to SelectedTableKeys.Count-1 do begin
        if SelectedTableKeys[k].Columns.IndexOf(TblCol.Name) > -1 then begin
          col.ImageIndex := SelectedTableKeys[k].ImageIndex;
          break;
        end;
      end;
      if col.ImageIndex = -1 then begin
        if SelectedTableTimestampColumns.IndexOf(TblCol.Name) > -1 then
          col.ImageIndex := 149;
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
  DBObj := ActiveDbObj;
  if DBObj = nil then
    Exit;
  Screen.Cursor := crHourglass;
  DBObj.Connection.Ping(True);

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
    // ... or maybe in a table/database with the same name on a different server
    RefreshingData := DBObj.IsSameAs(DataGridTable);

    // Load last view settings
    HandleDataGridAttributes(RefreshingData);
    OldScrollOffset := DataGrid.OffsetXY;

    // Remember old column widths if customized
    ColWidths := TStringList.Create;
    if not RefreshingData then
      FDataGridColumnWidthsCustomized := False;
    if FDataGridColumnWidthsCustomized then begin
      for i:=0 to vt.Header.Columns.Count-1 do
        ColWidths.Values[vt.Header.Columns[i].Text] := IntToStr(vt.Header.Columns[i].Width);
    end;

    DataGridTable := DBObj;

    Select := '';
    // Ensure key columns are included to enable editing
    KeyCols := DBObj.Connection.GetKeyColumns(SelectedTableColumns, SelectedTableKeys);
    WantedColumns := TTableColumnList.Create(False);
    WantedColumnOrgnames := TStringList.Create;
    FullColumnCount := 0;
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
          and (c.DataType.LoadPart)
          and (not IsKeyColumn) // We need full length of any key column, so DataGridLoadFullRow() has the chance to fetch the right row
          and ((ColLen > GRIDMAXDATA) or (ColLen = 0)) // No need to blow SQL with LEFT() if column is shorter anyway
          then begin
            case DBObj.Connection.Parameters.NetTypeGroup of
              ngMSSQL: Select := Select + ' LEFT(CAST(' + DBObj.Connection.QuoteIdent(c.Name) + ' AS NVARCHAR('+IntToStr(GRIDMAXDATA)+')), ' + IntToStr(GRIDMAXDATA) + '), ';
              ngMySQL: Select := Select + ' LEFT(' + DBObj.Connection.QuoteIdent(c.Name) + ', ' + IntToStr(GRIDMAXDATA) + '), ';
              ngPgSQL: Select := Select + ' SUBSTR(' + DBObj.Connection.QuoteIdent(c.Name) + ', 1, ' + IntToStr(GRIDMAXDATA) + '), ';
            end;
          end else if DBObj.Connection.Parameters.IsMSSQL and (c.DataType.Index=dtTimestamp) then begin
            Select := Select + ' CAST(' + DBObj.Connection.QuoteIdent(c.Name) + ' AS INT), ';
          end else if DBObj.Connection.Parameters.IsMSSQL and (c.DataType.Index=dtHierarchyid) then begin
            Select := Select + ' CAST(' + DBObj.Connection.QuoteIdent(c.Name) + ' AS NVARCHAR('+IntToStr(GRIDMAXDATA)+')), ';
          end else begin
            Select := Select + ' ' + DBObj.Connection.QuoteIdent(c.Name) + ', ';
            Inc(FullColumnCount);
          end;
        WantedColumns.Add(c);
        WantedColumnOrgnames.Add(c.Name);
      end;
    end;
    // Cut last comma
    Delete(Select, Length(Select)-1, 2);

    // Shorten the whole query if all columns are involved
    if FullColumnCount = SelectedTableColumns.Count then
      Select := '*';

    // Include db name for cases in which dbtree is switching databases and pending updates are in process
    Select := Select + ' FROM '+DBObj.QuotedDbAndTableName;

    // Append WHERE clause, and gracefully allow superfluous WHERE from user input
    if SynMemoFilter.GetTextLen > 0 then begin
      rx := TRegExpr.Create;
      rx.ModifierI := True;
      rx.Expression := '^\s*WHERE\s+';
      SynMemoFilter.Text := rx.Replace(SynMemoFilter.Text, '');
      rx.Free;
      Select := Select + ' WHERE ' + SynMemoFilter.Text + CRLF;
      tbtnDataFilter.ImageIndex := 108;
    end else
      tbtnDataFilter.ImageIndex := 107;
    SynMemoFilter.OnStatusChange(SynMemoFilter, []);

    // Append ORDER clause
    if Length(DataGridSortColumns) > 0 then begin
      Select := Select + ' ORDER BY ' + ComposeOrderClause(DataGridSortColumns);
      tbtnDataSorting.ImageIndex := 108;
      tbtnDataSorting.Caption := _('Sorting') + ' ('+IntToStr(Length(DataGridSortColumns))+')';
    end else begin
      tbtnDataSorting.ImageIndex := 107;
      tbtnDataSorting.Caption := _('Sorting');
    end;

    // Append LIMIT clause
    Offset := 0;
    if RefreshingData and (vt.Tag <> VTREE_NOTLOADED_PURGECACHE) then begin
      case DBObj.Connection.Parameters.NetTypeGroup of
        ngMSSQL: Offset := 0; // Does not support offset in all server versions
        ngMySQL, ngPgSQL: Offset := DataGridResult.RecordCount;
      end;
    end;
    Select := DBObj.Connection.ApplyLimitClause('SELECT', Select, DatagridWantedRowCount-Offset, Offset);

    vt.BeginUpdate;
    vt.Header.Columns.Clear;
    vt.Clear;

    try
      ShowStatusMsg(_('Fetching rows ...'));
      // Result object must be of the right vendor type
      if not RefreshingData then begin
        FreeAndNil(DataGridResult);
        DataGridResult := DBObj.Connection.Parameters.CreateQuery(DBObj.Connection);
      end;
      DataGridResult.DBObject := DBObj;
      DataGridResult.SQL := Trim(Select);
      DataGridResult.Execute(Offset > 0);
      DataGridResult.ColumnOrgNames := WantedColumnOrgnames;
      try
        DataGridResult.PrepareEditing;
      except on E:EDbError do // Do not annoy user with popup when accessing tables in information_schema
        LogSQL(_('Data in this table will be read-only.'));
      end;

      editFilterVT.Clear;
      TimerFilterVT.OnTimer(Sender);

      // Assign new data
      vt.RootNodeCount := DataGridResult.RecordCount;

      // Set up grid column headers
      ShowStatusMsg(_('Setting up columns ...'));
      VisibleColumns := 0;
      for i:=0 to WantedColumns.Count-1 do begin
        InitColumn(i, WantedColumns[i]);
        if coVisible in vt.Header.Columns[i].Options then
          Inc(VisibleColumns);
      end;

      // Signal for the user if we hide some columns
      if VisibleColumns = SelectedTableColumns.Count then
        tbtnDataColumns.ImageIndex := 107
      else
        tbtnDataColumns.ImageIndex := 108;
      tbtnDataColumns.Caption := _('Columns') + ' ('+IntToStr(VisibleColumns)+'/'+IntToStr(SelectedTableColumns.Count)+')';

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

    except
      // Wrong WHERE clause in most cases
      on E:EDbError do
        ErrorDialog(E.Message);
    end;

    vt.EndUpdate;

    // Do not steel filter while writing filters
    if not SynMemoFilter.Focused then
      vt.SetFocus;

    DataGridFocusedNodeIndex := Min(DataGridFocusedNodeIndex, Int64(vt.RootNodeCount)-1);
    SelectNode(vt, DataGridFocusedNodeIndex);
    for i:=0 to vt.Header.Columns.Count-1 do begin
      if vt.Header.Columns[i].Text = DataGridFocusedColumnName then begin
        vt.FocusedColumn := i;
        break;
      end;
    end;
    if RefreshingData then begin

      if (FDataGridLastClickedColumnHeader >= 0) and (FDataGridLastClickedColumnHeader < vt.Header.Columns.Count) then begin // See issue #3309
        // Horizontal offset based on the left side of a just sorted column
        OldScrollOffset.X := -(vt.Header.Columns[FDataGridLastClickedColumnHeader].Left - FDataGridLastClickedColumnLeftPos);
        // logsql('Fixing x-offset to '+OldScrollOffset.X.ToString +
        //   ', FDataGridLastClickedColumnHeader:'+FDataGridLastClickedColumnHeader.ToString +
        //   ', FDataGridLastClickedColumnLeftPos: '+FDataGridLastClickedColumnLeftPos.ToString);
      end;

      vt.OffsetXY := OldScrollOffset;
    end;

    // Reset remembered data for last clicked column header
    FDataGridLastClickedColumnHeader := NoColumn;
    FDataGridLastClickedColumnLeftPos := -1;

    vt.Header.Invalidate(nil);
    vt.UpdateScrollBars(True);
    ValidateControls(Sender);
    DisplayRowCountStats(vt);
    MaximumRows := AppSettings.ReadInt(asDatagridMaximumRows);
    actDataShowNext.Enabled := (vt.RootNodeCount = DatagridWantedRowCount) and (DatagridWantedRowCount < MaximumRows);
    actDataShowAll.Enabled := actDataShowNext.Enabled;
    EnumerateRecentFilters;
    ColWidths.Free;
    if Integer(vt.RootNodeCount) = MaximumRows then
      LogSQL(f_('Browsing is currently limited to a maximum of %s rows. To see more rows, increase this maximum in %s > %s > %s.', [FormatNumber(MaximumRows), _('Tools'), _('Preferences'), _('Data')]), lcInfo);
  end;
  vt.Tag := VTREE_LOADED;
  DataGridFullRowMode := False;
  Screen.Cursor := crDefault;
  ShowStatusMsg;
end;


procedure TMainForm.DataGridColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  // Remember current table after last column resizing so we can auto size them as long as this did not happen
  if not (tsUpdating in Sender.Treeview.TreeStates) then
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
  if DBObject = nil then // Some cases have no object, don't let them crash
    Exit;

  cap := ActiveDatabase + '.' + DBObject.Name;
  IsLimited := DataGridWantedRowCount <= Datagrid.RootNodeCount;
  IsFiltered := SynMemoFilter.GetTextLen > 0;
  if DBObject.NodeType = lntTable then begin
    if (not IsLimited) and (not IsFiltered) then
      RowsTotal := DataGrid.RootNodeCount // No need to fetch via SHOW TABLE STATUS
    else
      RowsTotal := DBObject.RowCount;
    if RowsTotal > -1 then begin
      cap := cap + ': ' + FormatNumber(RowsTotal) + ' ' + _('rows total');
      if DBObject.Engine = 'InnoDB' then
        cap := cap + ' ('+_('approximately')+')';
      // Display either LIMIT or WHERE effect, not both at the same time
      if IsLimited then
        cap := cap + ', '+_('limited to') + ' ' + FormatNumber(Datagrid.RootNodeCount)
      else if IsFiltered then begin
        if Datagrid.RootNodeCount = RowsTotal then
          cap := cap + ', '+_('all rows match to filter')
        else
          cap := cap + ', ' + FormatNumber(Datagrid.RootNodeCount) + ' '+_('rows match to filter');
      end;
    end;
  end;
  lblDataTop.Caption := cap;
end;


procedure TMainForm.AnyGridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Idx: PInt64;
begin
  // Display multiline grid rows
  if AppSettings.ReadInt(asGridRowLineCount) = 1 then
    Exclude(Node.States, vsMultiLine)
  else
    Include(Node.States, vsMultiLine);
  // Node may have data already, if added via InsertRow
  if not (vsOnFreeNodeCallRequired in Node.States) then begin
    Idx := Sender.GetNodeData(Node);
    Idx^ := Node.Index;
  end;
end;


procedure TMainForm.AnyGridGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Int64);
end;


{***
  Occurs when active tab has changed.
}
procedure TMainForm.PageControlMainChange(Sender: TObject);
var
  tab: TTabSheet;
begin
  // Protect from crash when pressing ctrl+tab before main form is displayed
  // See #574
  if not Self.Visible then
    Exit;

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
  Conn: TDBConnection;
  ScrollOffset: TPoint;
begin
  // DB-Properties
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  LogSQL('ListTablesBeforePaint', lcDebug);
  Screen.Cursor := crHourGlass;
  Conn := ActiveConnection;
  ScrollOffset := vt.OffsetXY;
  vt.BeginUpdate;
  vt.Clear;
  Msg := '';
  if Conn <> nil then begin
    ShowStatusMsg(f_('Displaying objects from "%s" ...', [Conn.Database]));
    Objects := Conn.GetDBObjects(Conn.Database, vt.Tag = VTREE_NOTLOADED_PURGECACHE, FActiveObjectGroup);
    vt.RootNodeCount := Objects.Count;

    NumObjects := TStringList.Create;
    FDBObjectsMaxSize := 1;
    FDBObjectsMaxRows := 1;
    for i:=0 to Objects.Count-1 do begin
      Obj := Objects[i];
      NumObj := StrToIntDef(NumObjects.Values[Obj.ObjType], 0);
      Inc(NumObj);
      NumObjects.Values[Obj.ObjType] := IntToStr(NumObj);
      if Obj.Size > FDBObjectsMaxSize then FDBObjectsMaxSize := Obj.Size;
      if Obj.Rows > FDBObjectsMaxRows then FDBObjectsMaxRows := Obj.Rows;
    end;
    Msg := Conn.Database + ': ' + FormatNumber(Objects.Count) + ' ';
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
  end;
  vt.OffsetXY := ScrollOffset;
  vt.EndUpdate;
  vt.Tag := VTREE_LOADED;
  FListTablesSorted := False;
  ShowStatusMsg(Msg, 0);
  ShowStatusMsg;
  ValidateControls(Self);
  Screen.Cursor := crDefault;
end;


procedure TMainForm.ListTablesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Obj: PDBObject;
begin
  if Column <> (Sender as TVirtualStringTree).Header.MainColumn then
    Exit;
  Obj := Sender.GetNodeData(Node);
  case Kind of
    ikNormal, ikSelected:
      ImageIndex := Obj.ImageIndex;
    ikOverlay:
      ImageIndex := Obj.OverlayImageIndex;
  end;
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
    0: begin
      if Obj.Schema <> '' then
        CellText := Obj.Schema + '.' + Obj.Name
      else
        CellText := Obj.Name;
      if Sender.IsEditing and (Node = Sender.FocusedNode) then
        CellText := Obj.Name;
    end;
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
  Conn: TDBConnection;
begin
  Conn := ActiveConnection;
  Obj := Sender.GetNodeData(Node);
  if (Conn <> nil) and (not Conn.Database.IsEmpty) then begin
    Objects := Conn.GetDBObjects(Conn.Database, False, FActiveObjectGroup);
    Obj^ := Objects[Node.Index];
  end else
    Obj^ := nil;
end;



{***
  Selection in ListTables is changing
}
procedure TMainForm.ListTablesChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
var
  Msg: String;
begin
  ValidateControls(Sender);
  if ListTables.SelectedCount > 1 then
    Msg := _('Selected') + ': ' + FormatNumber(ListTables.SelectedCount)
  else
    Msg := '';
  ShowStatusMsg(Msg, 1)
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
  inDataTab, inDataOrQueryTab, inDataOrQueryTabNotEmpty, inGrid: Boolean;
  HasConnection, GridHasChanges, EnableTimestamp: Boolean;
  Grid: TVirtualStringTree;
  inSynMemo, inSynMemoEditable: Boolean;
  Results: TDBQuery;
  RowNum: PInt64;
begin
  // When adding some new TAction here, be sure to apply this procedure to its OnUpdate event

  Grid := ActiveGrid;
  HasConnection := ActiveConnection <> nil;
  Results := nil;
  GridHasChanges := False;
  EnableTimestamp := False;
  if HasConnection and Assigned(Grid) then begin
    Results := GridResult(Grid);
    if (Results<>nil) and Assigned(Grid.FocusedNode) then begin
      RowNum := Grid.GetNodeData(Grid.FocusedNode);
      Results.RecNo := RowNum^;
      GridHasChanges := Results.Modified or Results.Inserted;
      if Grid.FocusedColumn > NoColumn then
        EnableTimestamp := Results.DataType(Grid.FocusedColumn).Category in [dtcInteger, dtcReal];
    end;
  end;
  inDataTab := PageControlMain.ActivePage = tabData;
  inDataOrQueryTab := inDataTab or QueryTabActive;
  inDataOrQueryTabNotEmpty := inDataOrQueryTab and Assigned(Grid) and (Grid.RootNodeCount > 0);
  inGrid := Assigned(Grid) and (ActiveControl = Grid);

  actFullRefresh.Enabled := HasConnection and (PageControlMain.ActivePage = tabDatabase);
  actDataInsert.Enabled := HasConnection and inGrid and Assigned(Results);
  actDataDuplicateRowWithoutKeys.Enabled := HasConnection and inGrid and inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode);
  actDataDuplicateRowWithKeys.Enabled := actDataDuplicateRowWithoutKeys.Enabled;
  actDataDelete.Enabled := HasConnection and inGrid and (Grid.SelectedCount > 0);
  actDataFirst.Enabled := HasConnection and inDataOrQueryTabNotEmpty and inGrid;
  actDataLast.Enabled := HasConnection and inDataOrQueryTabNotEmpty and inGrid;
  actDataPostChanges.Enabled := HasConnection and GridHasChanges;
  actDataCancelChanges.Enabled := HasConnection and GridHasChanges;
  actDataSaveBlobToFile.Enabled := HasConnection and inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode);
  actGridEditFunction.Enabled := HasConnection and inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode);
  actDataPreview.Enabled := HasConnection and inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode);
  actDataOpenUrl.Enabled := HasConnection and inDataOrQueryTabNotEmpty and Assigned(Grid.FocusedNode)
    and ExecRegExpr('^https?://[^\s]+$', Grid.Text[Grid.FocusedNode, Grid.FocusedColumn]);
  actUnixTimestampColumn.Enabled := HasConnection and inDataTab and EnableTimestamp;
  actUnixTimestampColumn.Checked := inDataTab and HandleUnixTimestampColumn(Grid, Grid.FocusedColumn);
  actPreviousResult.Enabled := HasConnection and inDataOrQueryTabNotEmpty;
  actNextResult.Enabled := HasConnection and inDataOrQueryTabNotEmpty;

  // Activate export-options if we're on Data- or Query-tab
  actExportData.Enabled := HasConnection and inDataOrQueryTabNotEmpty;
  actDataSetNull.Enabled := HasConnection and inDataOrQueryTab and Assigned(Results) and Assigned(Grid.FocusedNode);

  inSynMemo := ActiveSynMemo(True) <> nil;
  inSynMemoEditable := inSynMemo and (not ActiveSynMemo(True).ReadOnly);
  actSaveSynMemoToTextfile.Enabled := inSynMemo;
  actToggleComment.Enabled := inSynMemoEditable;
  if inSynMemo then begin
    actCut.Enabled := inSynMemoEditable;
    actPaste.Enabled := inSynMemoEditable;
  end;

  ValidateQueryControls(Sender);
  UpdateLineCharPanel;
end;


procedure TMainForm.ValidateQueryControls(Sender: TObject);
var
  NotEmpty, HasSelection, HasConnection: Boolean;
  Tab: TQueryTab;
  cap: String;
  InQueryTab: Boolean;
begin
  for Tab in QueryTabs do begin
    cap := Trim(Tab.TabSheet.Caption);
    if cap[Length(cap)] = '*' then
      cap := Copy(cap, 1, Length(cap)-1);
    if Tab.Memo.Modified then
      cap := cap + '*';
    if Tab.TabSheet.Caption <> cap then
      SetTabCaption(Tab.TabSheet.PageIndex, cap);
  end;
  InQueryTab := QueryTabActive;
  Tab := ActiveQueryTab;
  NotEmpty := InQueryTab and (Tab.Memo.GetTextLen > 0);
  HasSelection := InQueryTab and Tab.Memo.SelAvail;
  HasConnection := ActiveConnection <> nil;
  actExecuteQuery.Enabled := HasConnection and InQueryTab and NotEmpty and (not Tab.QueryRunning);
  actExecuteSelection.Enabled := HasConnection and InQueryTab and HasSelection and (not Tab.QueryRunning);
  actExecuteCurrentQuery.Enabled := actExecuteQuery.Enabled;
  actExplainAnalyzeCurrentQuery.Enabled := actExecuteQuery.Enabled;
  actSaveSQLAs.Enabled := InQueryTab and NotEmpty;
  actSaveSQL.Enabled := actSaveSQLAs.Enabled and Tab.Memo.Modified;
  actSaveSQLselection.Enabled := InQueryTab and HasSelection;
  actSaveSQLSnippet.Enabled := InQueryTab and NotEmpty;
  actSaveSQLSelectionSnippet.Enabled := InQueryTab and HasSelection;
  actClearQueryEditor.Enabled := InQueryTab;
  actSetDelimiter.Enabled := InQueryTab;
  actCloseQueryTab.Enabled := IsQueryTab(PageControlMain.ActivePageIndex, False);
end;


procedure TMainForm.KillProcess(Sender: TObject);
var
  t: Boolean;
  pid: Int64;
  Node: PVirtualNode;
  Conn: TDBConnection;
begin
  t := TimerRefresh.Enabled;
  TimerRefresh.Enabled := false; // prevent av (ListProcesses.selected...)
  Conn := ActiveConnection;
  if MessageDialog('Kill '+IntToStr(ListProcesses.SelectedCount)+' Process(es)?', mtConfirmation, [mbok,mbcancel]) = mrok then
  begin
    Node := GetNextNode(ListProcesses, nil, True);
    while Assigned(Node) do begin
      pid := StrToInt64Def(ListProcesses.Text[Node, ListProcesses.Header.MainColumn], 0);
      // Don't kill own process
      if pid = Conn.ThreadId then
        LogSQL(f_('Ignoring own process id #%d when trying to kill it.', [pid]))
      else try
        Conn.Query(Format(Conn.GetSQLSpecifity(spKillProcess), [pid]));
      except
        on E:EDbError do begin
          if Conn.LastErrorCode <> 1094 then
            if MessageDialog(E.Message, mtError, [mbOK, mbAbort]) = mrAbort then
              break;
        end;
      end;
      Node := GetNextNode(ListProcesses, Node, True);
    end;
    InvalidateVT(ListProcesses, VTREE_NOTLOADED, True);
  end;
  TimerRefresh.Enabled := t; // re-enable autorefresh timer
end;


{ Proposal about to insert a String into synmemo }
procedure TMainForm.SynCompletionProposalCodeCompletion(Sender: TObject;
  var Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
var
  Proposal: TSynCompletionProposal;
  rx: TRegExpr;
  ImageIndex, f: Integer;
  FunctionDeclaration: String;
begin
  Proposal := Sender as TSynCompletionProposal;
  // Surround identifiers with backticks if it is a column, table, routine, db
  rx := TRegExpr.Create;
  rx.Expression := '\\image\{(\d+)\}';
  if rx.Exec(Proposal.ItemList[Index]) then begin
    ImageIndex := MakeInt(rx.Match[1]);
    if not (ImageIndex in [ICONINDEX_KEYWORD, ICONINDEX_FUNCTION, 113]) then begin
      FunctionDeclaration := '';
      f := Pos('(', Value);
      if f > 0 then begin
        FunctionDeclaration := Copy(Value, f, Length(Value));
        Delete(Value, f, Length(Value));
      end;

      rx.Expression := '^(['+QuoteRegExprMetaChars(ActiveConnection.QuoteChars)+'])(.*)$';
      if rx.Exec(Value) then begin
        // Left character of identifier is already a quote: user wants to force quoting.
        // Seperate that left quote character away from what gets now quoted automatically, and force quoting
        Value := ActiveConnection.QuoteIdent(rx.Match[2], True) + FunctionDeclaration;
      end else begin
        // Identifier without left quote - quote when required
        Value := ActiveConnection.QuoteIdent(Value, False) + FunctionDeclaration;
      end;
    end;
  end;
  rx.Free;
  Proposal.Form.CurrentEditor.UndoList.AddGroupBreak;
end;


procedure TMainForm.SynCompletionProposalAfterCodeCompletion(Sender: TObject;
  const Value: String; Shift: TShiftState; Index: Integer; EndToken: Char);
var
  Proposal: TSynCompletionProposal;
begin
  Proposal := Sender as TSynCompletionProposal;
  Proposal.Form.CurrentEditor.UndoList.AddGroupBreak;
  // Explicitly set focus again to work around a bug in Ultramon, see issue #2396
  Proposal.Form.CurrentEditor.SetFocus;
end;


{ Proposal-Combobox pops up }
procedure TMainForm.SynCompletionProposalExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
var
  i, j, ImageIndex: Integer;
  Results: TDBQuery;
  DBObjects: TDBObjectList;
  sql, TableClauses, TableName, LeftPart, Token1, Token2, Token3, Token, Ident: String;
  Tables: TStringList;
  rx: TRegExpr;
  Start, TokenTypeInt: Integer;
  Attri: TSynHighlighterAttributes;
  Proposal: TSynCompletionProposal;
  Editor: TCustomSynEdit;
  Queries: TSQLBatch;
  Query: TSQLSentence;
  Conn: TDBConnection;
  RoutineEditor: TfrmRoutineEditor;
  Param: TRoutineParam;

  procedure AddTable(Obj: TDBObject);
  var
    DisplayText: String;
    FunctionDeclaration: String;
    FuncParams: TRoutineParamList;
    FuncParam: TRoutineParam;
  begin
    // Append routine parameter declaration
    FunctionDeclaration := '';
    if Obj.NodeType in [lntProcedure, lntFunction] then begin
      FuncParams := TRoutineParamList.Create(True);
      Obj.Connection.ParseRoutineStructure(Obj, FuncParams);
      for FuncParam in FuncParams do begin
        FunctionDeclaration := FunctionDeclaration + FuncParam.Name + ', ';
      end;
      if FunctionDeclaration <> '' then begin
        Delete(FunctionDeclaration, Length(FunctionDeclaration)-1, 2);
        FunctionDeclaration := '(' + FunctionDeclaration + ')';
      end;
      FuncParams.Free;
    end;

    DisplayText := Format(SYNCOMPLETION_PATTERN,
      [Obj.ImageIndex, LowerCase(_(Obj.ObjType)), Obj.Name, FunctionDeclaration]);
    Proposal.AddItem(DisplayText, Obj.Name+FunctionDeclaration);
  end;

  procedure AddColumns(const LeftToken: String);
  var
    dbname, tblname, Dummy: String;
    Columns: TTableColumnList;
    Col: TTableColumn;
    Obj: TDBObject;
  begin
    dbname := '';
    tblname := LeftToken;
    if Pos('.', tblname) > -1 then begin
      dbname := Copy(tblname, 0, Pos('.', tblname)-1);
      tblname := Copy(tblname, Pos('.', tblname)+1, Length(tblname));
    end;
    // db and table name may already be quoted
    if dbname = '' then
      dbname := Conn.Database;
    dbname := Conn.DeQuoteIdent(dbname);
    tblname := Conn.DeQuoteIdent(tblname);
    DBObjects := Conn.GetDBObjects(dbname);
    for Obj in DBObjects do begin
      if Obj.Name = tblname then begin
        Columns := TTableColumnList.Create(True);
        case Obj.NodeType of
          lntTable:
            Conn.ParseTableStructure(Obj.CreateCode, Columns, nil, nil);
          lntView:
            Conn.ParseViewStructure(Obj.CreateCode, Obj, Columns, Dummy, Dummy, Dummy, Dummy, Dummy);
        end;
        for Col in Columns do begin
          Proposal.InsertList.Add(Col.Name);
          Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_FIELD, LowerCase(Col.DataType.Name), Col.Name, '']) );
        end;
        Columns.Free;
        break;
      end;
    end;
  end;

begin
  Proposal := Sender as TSynCompletionProposal;
  Proposal.ClearList;
  Conn := ActiveConnection;
  Editor := Proposal.Form.CurrentEditor;
  Editor.GetHighlighterAttriAtRowColEx(Editor.PrevWordPos, Token, TokenTypeInt, Start, Attri);
  CanExecute := AppSettings.ReadBool(asCompletionProposal) and
    (not (TtkTokenKind(TokenTypeInt) in [tkString, tkComment]));
  if not CanExecute then
    Exit;

  // Work around for issue #2640. See ApplicationDeActivate
  Proposal.Form.Enabled := True;

  rx := TRegExpr.Create;

  // Find token1.token2.token3, while cursor is somewhere in token3
  Ident := '[^\s,\(\)=\.]';
  rx.Expression := '(('+Ident+'+)\.)?('+Ident+'+)\.('+Ident+'*)$';
  LeftPart := Copy(Editor.LineText, 1, Editor.CaretX-1);
  if rx.Exec(LeftPart) then begin
    Token1 := Conn.DeQuoteIdent(rx.Match[2]);
    Token2 := Conn.DeQuoteIdent(rx.Match[3]);
    Token3 := Conn.DeQuoteIdent(rx.Match[4]);
  end;

  // Server variables, s'il vous plait?
  rx.Expression := '^@@(SESSION|GLOBAL)$';
  rx.ModifierI := True;
  if rx.Exec(Token2) then begin
    try
      Results := Conn.GetResults('SHOW '+UpperCase(rx.Match[1])+' VARIABLES');
      while not Results.Eof do begin
        Proposal.InsertList.Add(Results.Col(0));
        Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_PRIMARYKEY, 'variable', Results.Col(0), ' ('+StringReplace(Results.Col(1), '\', '\\', [rfReplaceAll])+')'] ) );
        Results.Next;
      end;
    except
      // Just log error in sql log, do not disturb user while typing
    end;
  end else begin
    // Get column names into the proposal pulldown
    // when we write sql like "SELECT t.|col FROM table [AS] t"
    // Current limitation: Identifiers (masked or not) containing
    // spaces are not detected correctly.

    // 1. find currently edited sql query around the cursor position in synmemo
    if Editor = SynMemoFilter then begin
      // Make sure the below regexp can find structure
      sql := 'SELECT * FROM '+ActiveDbObj.QuotedName+' WHERE ' + Editor.Text;
    end else begin
      // In a query tab
      Queries := TSQLBatch.Create;
      Queries.SQL := Editor.Text;
      for Query in Queries do begin
        if (Query.LeftOffset <= Editor.SelStart) and (Editor.SelStart < Query.RightOffset) then begin
          sql := Query.SQL;
          break;
        end;
      end;
      Queries.Free;
    end;

    // 2. Parse FROM clause, detect relevant table/view, probably aliased
    rx.ModifierG := True;
    rx.ModifierI := True;
    rx.Expression := '\b(FROM|INTO|UPDATE)\s+(IGNORE\s+)?(.+)(WHERE|HAVING|ORDER|GROUP)?';
    if rx.Exec(sql) then begin
      TableClauses := rx.Match[3];
      // Ensure tables in JOIN clause(s) are splitted by comma
      TableClauses := StringReplace(TableClauses, 'JOIN', ',', [rfReplaceAll, rfIgnoreCase]);
      // Remove surrounding parentheses
      TableClauses := StringReplace(TableClauses, '(', ' ', [rfReplaceAll]);
      TableClauses := StringReplace(TableClauses, ')', ' ', [rfReplaceAll]);
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
          if Token2 = Conn.DeQuoteIdent(rx.Match[3]) then begin
            TableName := rx.Match[1];
            break;
          end;
          if not rx.ExecNext then
            break;
        end;
        if TableName <> '' then
          break;
      end;
    end;

    if TableName <> '' then
      AddColumns(TableName)
    else if Token1 <> '' then
      AddColumns(Conn.QuoteIdent(Token1, False)+'.'+Conn.QuoteIdent(Token2, False))
    else if Token2 <> '' then
      AddColumns(Conn.QuoteIdent(Token2, False));

    if Token1 = '' then begin
      i := Conn.AllDatabases.IndexOf(Token2);
      if i > -1 then begin
        // Tables from specific database
        Screen.Cursor := crHourGlass;
        DBObjects := Conn.GetDBObjects(Conn.AllDatabases[i]);
        Conn.PrefetchCreateCode(DBObjects);
        for j:=0 to DBObjects.Count-1 do
          AddTable(DBObjects[j]);
        Conn.PurgePrefetchResults;
        Screen.Cursor := crDefault;
      end;
    end;

    if Token2 = '' then begin

      // Column names from selected table
      if Editor = SynMemoFilter then begin
        AddColumns(ActiveDbObj.QuotedName);
      end;

      // All databases
      for i:=0 to Conn.AllDatabases.Count-1 do begin
        Proposal.InsertList.Add(ActiveConnection.AllDatabases[i]);
        Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_DB, 'database', Conn.AllDatabases[i], '']));
      end;

      // Tables from current db
      if Conn.Database <> '' then begin
        DBObjects := Conn.GetDBObjects(Conn.Database);
        Conn.PrefetchCreateCode(DBObjects);
        for j:=0 to DBObjects.Count-1 do
          AddTable(DBObjects[j]);
        Conn.PurgePrefetchResults;
        if Token1 <> '' then // assume that we have already a dbname in memo
          Proposal.Position := Conn.AllDatabases.Count;
      end;

      // Functions
      for i:=0 to Length(MySQLFunctions)-1 do begin
        // Hide unsupported functions
        if MySqlFunctions[i].Version > Conn.ServerVersionInt then
          continue;
        Proposal.InsertList.Add(MySQLFunctions[i].Name + MySQLFunctions[i].Declaration);
        Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_FUNCTION, 'function', MySQLFunctions[i].Name, MySQLFunctions[i].Declaration]));
      end;

      // Keywords
      for i:=0 to MySQLKeywords.Count-1 do begin
        Proposal.InsertList.Add(MySQLKeywords[i]);
        Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_KEYWORD, 'keyword', MySQLKeywords[i], '']) );
      end;

      // Procedure params
      if GetParentFormOrFrame(Editor) is TfrmRoutineEditor then begin
        RoutineEditor := GetParentFormOrFrame(Editor) as TfrmRoutineEditor;
        for Param in RoutineEditor.Parameters do begin
          if Param.Context = 'IN' then ImageIndex := 120
          else if Param.Context = 'OUT' then ImageIndex := 121
          else if Param.Context = 'INOUT' then ImageIndex := 122
          else ImageIndex := -1;
          Proposal.InsertList.Add(Param.Name);
          Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ImageIndex, Param.Datatype, Param.Name, '']));
        end;
      end;

    end;

  end;
  rx.Free;
end;


procedure TMainForm.SynMemoQueryStatusChange(Sender: TObject; Changes:
    TSynStatusChanges);
var
  Edit: TSynMemo;
begin
  // Don't ask for saving empty contents. See issue #614
  Edit := Sender as TSynMemo;
  if Edit.GetTextLen = 0 then begin
    ActiveQueryTab.MemoFilename := '';
    ActiveQueryTab.Memo.Modified := False;
  end;
  // Update various controls
  ValidateQueryControls(Sender);
  UpdateLineCharPanel;
end;



procedure TMainForm.TimerHostUptimeTimer(Sender: TObject);
var
  Conn: TDBConnection;
  Uptime: Integer;
  ServerNow: TDateTime;
  ServerNowStr: String;
begin
  // Display server uptime and current date time
  Conn := ActiveConnection;
  if Assigned(Conn) then begin
    Uptime := Conn.ServerUptime;
    if Uptime >= 0 then
      ShowStatusMsg(_('Uptime')+': '+FormatTimeNumber(Conn.ServerUptime, False), 4)
    else
      ShowStatusMsg(_('Uptime')+': '+_('unknown'), 4);

    ServerNow := Conn.ServerNow;
    if ServerNow >= 0 then begin
      DateTimeToString(ServerNowStr, 't', ServerNow);
      ShowStatusMsg(f_('Server time: %s', [ServerNowStr]), 5)
    end else
      ShowStatusMsg(f_('Server time: %s', [_('unknown')]), 5);
  end else begin
    ShowStatusMsg('', 4);
  end;

end;


procedure TMainForm.ListTablesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Obj: PDBObject;
begin
  // Tables and views can be renamed, routines cannot
  if Assigned(Node) then begin
    Obj := Sender.GetNodeData(Node);
    Allowed := Obj.NodeType in [lntTable, lntView];
  end;
end;


{***
  Rename table after checking the new name for invalid characters
}
procedure TMainForm.ListTablesNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Obj: PDBObject;
  sql: String;
begin
  // Fetch data from node
  Obj := Sender.GetNodeData(Node);

  // Try to rename, on any error abort and don't rename ListItem
  try
    // rename table
    case Obj.NodeType of
      lntTable:
        sql := Obj.Connection.GetSQLSpecifity(spRenameTable);
      lntView:
        sql := Obj.Connection.GetSQLSpecifity(spRenameView);
      else
        raise EDbError.Create('Cannot rename '+Obj.ObjType);
    end;

    sql := Format(sql, [Obj.QuotedName(True, False), Obj.Connection.QuoteIdent(NewText)]);
    Obj.Connection.Query(sql);

    if SynSQLSynUsed.TableNames.IndexOf( NewText ) = -1 then begin
      SynSQLSynUsed.TableNames.Add(NewText);
    end;
    // Update nodedata
    Obj.Name := NewText;
    Obj.CreateCode := '';
    // Now the active tree db has to be updated. But calling RefreshTreeDB here causes an AV
    // so we do it manually here
    DBTree.InvalidateChildren(FindDBNode(DBtree, Obj.Connection, Obj.Database), True);
  except
    on E:EDbError do
      ErrorDialog(E.Message);
  end;
end;


procedure TMainForm.TimerConnectedTimer(Sender: TObject);
var
  ConnectedTime: Integer;
  Conn: TDBConnection;
begin
  Conn := ActiveConnection;
  if (Conn <> nil) and Conn.Active then begin
    // Calculate and display connection-time. Also, on any connect or reconnect, update server version panel.
    ConnectedTime := Conn.ConnectionUptime;
    ShowStatusMsg(_('Connected')+': ' + FormatTimeNumber(ConnectedTime, False), 2);
  end else begin
    ShowStatusMsg(_('Disconnected'), 2);
  end;
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
    Val := DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn];
    if InputQuery(_('Specify filter-value...'), Item.Caption, Val) then begin
      if Item = QF8 then
        Filter := ActiveConnection.QuoteIdent(Col) + ' = ''' + Val + ''''
      else if Item = QF9 then
        Filter := ActiveConnection.QuoteIdent(Col) + ' != ''' + Val + ''''
      else if Item = QF10 then
        Filter := ActiveConnection.QuoteIdent(Col) + ' > ''' + Val + ''''
      else if Item = QF11 then
        Filter := ActiveConnection.QuoteIdent(Col) + ' < ''' + Val + ''''
      else if Item = QF12 then
        Filter := ActiveConnection.QuoteIdent(Col) + ' LIKE ''%' + Val + '%''';
    end;
  end else
    Filter := Item.Hint;

  if ExecRegExpr('\s+LIKE\s+''', Filter) then
    Filter := Filter + ActiveConnection.LikeClauseTail;

  if Filter <> '' then begin
    SynMemoFilter.UndoList.AddGroupBreak;
    SynMemoFilter.SelectAll;
    if KeyPressed(VK_SHIFT) and (Pos(Filter, SynmemoFilter.Text) = 0) and (Pos(SynmemoFilter.Text, Filter) = 0) then
      SynmemoFilter.SelText := SynmemoFilter.Text + ' AND ' + Filter
    else
      SynmemoFilter.SelText := Filter;
    ToggleFilterPanel(True);
    actApplyFilterExecute(Sender);
  end;
end;


procedure TMainForm.popupQueryPopup(Sender: TObject);
begin
  // Sets cursor into memo and activates TAction(s) like paste
  ActiveQueryMemo.SetFocus;
end;


procedure TMainForm.popupSqlLogPopup(Sender: TObject);
begin
  // Update popupMenu items
  menuLogToFile.Checked := FLogToFile;
  menuOpenLogFolder.Enabled := FLogToFile;
end;


procedure TMainForm.AutoRefreshSetInterval(Sender: TObject);
var
  SecondsStr: String;
  Seconds: Extended;
begin
  // set interval for autorefresh-timer
  SecondsStr := FloatToStr(TimerRefresh.interval div 1000);
  if InputQuery(_('Auto refresh'),_('Refresh list every ... second(s):'), SecondsStr) then begin
    Seconds := StrToFloatDef(SecondsStr, 0);
    if Seconds > 0 then begin
      TimerRefresh.Interval := Trunc(Seconds * 1000);
      TimerRefresh.Enabled := true;
      menuAutoRefresh.Checked := true;
    end
    else
      ErrorDialog(f_('Seconds must be between 0 and %d.', [maxint]));
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
  // Accepting drag's from the same editor, from DBTree and from QueryHelpers
  H := ActiveQueryHelpers;
  Accept := (src = DBtree) or ((src = H) and Assigned(H.FocusedNode) and (H.GetNodeLevel(H.FocusedNode) in [1,2]));
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
  History: TQueryHistory;
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
      lntDb: Text := ActiveDbObj.QuotedDatabase(False);
      lntTable..lntEvent: begin
        if ShiftPressed then
          Text := ActiveDbObj.QuotedDatabase(False) + '.';
        Text := Text + ActiveDbObj.QuotedName(False);
      end;
    end;
  end else if src = Tree then begin
    case Tree.GetNodeLevel(Tree.FocusedNode) of
      1:
        case Tree.FocusedNode.Parent.Index of
          HELPERNODE_SNIPPETS:
            Text := ReadTextFile(AppSettings.DirnameSnippets + Tree.Text[Tree.FocusedNode, 0] + '.sql', nil);
          HELPERNODE_HISTORY:
            Text := '';
          else begin
            Node := Tree.GetFirstChild(Tree.FocusedNode.Parent);
            while Assigned(Node) do begin
              if Tree.Selected[Node] then begin
                ItemText := Tree.Text[Node, 0];
                if Node.Parent.Index = HELPERNODE_COLUMNS then
                  ItemText := ActiveConnection.QuoteIdent(ItemText, False); // Quote column names
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
      2:
        case Tree.FocusedNode.Parent.Parent.Index of
          HELPERNODE_HISTORY: begin
            History := ActiveQueryTab.HistoryDays.Objects[Tree.FocusedNode.Parent.Index] as TQueryHistory;
            Text := History[Tree.FocusedNode.Index].SQL;
          end;
        end;
    end;
  end else
    raise Exception.Create(_('Unspecified source control in drag''n drop operation!'));

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
  Tab: TQueryTab;
begin
  // One or more files from explorer or somewhere else was dropped onto the
  // query-memo - load their contents into seperate tabs
  if not RunQueryFiles(AFiles, nil, False) then begin
    for i:=0 to AFiles.Count-1 do begin
      Tab := ActiveOrEmptyQueryTab(False);
      Tab.LoadContents(AFiles[i], False, nil);
    end;
  end;
end;


procedure TMainForm.SynMemoQueryKeyPress(Sender: TObject; var Key: Char);
var
  Editor: TSynMemo;
  Token, Replacement: String;
  Attri: TSynHighlighterAttributes;
  OldCaretXY, StartOfTokenRowCol, EndOfTokenRowCol, CurrentRowCol: TBufferCoord;
  TokenTypeInt, Start, CurrentCharIndex: Integer;
  //OldSelStart, OldSelEnd: Integer;
  LineWithToken: String;
  TableIndex, ProcIndex: Integer;
const
  WordChars = ['A'..'Z', 'a'..'z', '_'];
  IgnoreChars = [#8]; // Backspace, and probably more which should not trigger uppercase
begin
  // Uppercase reserved words, functions and data types
  if CharInSet(Key, WordChars) or CharInSet(Key, IgnoreChars) then
    Exit;
  if not AppSettings.ReadBool(asAutoUppercase) then
    Exit;
  Editor := Sender as TSynMemo;
  CurrentCharIndex := Editor.RowColToCharIndex(Editor.CaretXY);
  // Go one left on trailing line feed, after which PrevWordPos doesn't work
  Dec(CurrentCharIndex, 1);
  CurrentRowCol := Editor.CharIndexToRowCol(CurrentCharIndex);
  StartOfTokenRowCol := Editor.PrevWordPosEx(CurrentRowCol);
  Editor.GetHighlighterAttriAtRowColEx(StartOfTokenRowCol, Token, TokenTypeInt, Start, Attri);
  Replacement := UpperCase(Token);

  // Check if token is preceded by a dot, so it is most probably a table, column or some alias
  LineWithToken := Editor.Lines[StartOfTokenRowCol.Line-1];
  if (StartOfTokenRowCol.Char > 1) and (LineWithToken[StartOfTokenRowCol.Char-1] = '.') then begin
    Exit;
  end;

  // Auto-fix case of known database objects
  TableIndex := SynSQLSynUsed.TableNames.IndexOf(Token);
  ProcIndex := SynSQLSynUsed.ProcNames.IndexOf(Token);
  if TableIndex > -1 then begin
    Replacement := SynSQLSynUsed.TableNames[TableIndex];
  end else if ProcIndex > -1 then begin
    Replacement := SynSQLSynUsed.ProcNames[ProcIndex];
  end else if not (TtkTokenKind(TokenTypeInt) in [tkDatatype, tkFunction, tkKey]) then begin
    // Only uppercase certain types of keywords
    Exit;
  end;

  if Token <> Replacement then begin
    OldCaretXY := Editor.CaretXY;
    //OldSelStart := Editor.SelStart;
    //OldSelEnd := Editor.SelEnd;

    EndOfTokenRowCol := Editor.WordEndEx(StartOfTokenRowCol);
    Editor.InsertBlock(StartOfTokenRowCol, EndOfTokenRowCol, PWideChar(Replacement), True);

    Editor.CaretXY := OldCaretXY;
    //Editor.SelStart := OldSelStart; // breaks at least some undo steps
    //Editor.SelEnd := OldSelEnd;
  end;

  //Editor.ExecuteCommand(ecUpperCase, #0, nil);

  {Editor.UndoList.BeginBlock; // this does not work!
  Editor.UndoList.AddGroupBreak; // neither!
  Editor.SelStart := Editor.RowColToCharIndex(StartOfTokenRowCol);
  Editor.SelEnd := Editor.SelStart + Length(Token);
  Editor.SelText := UpperCase(Token);}

  {Editor.BlockBegin := StartOfTokenRowCol;
  EndOfTokenRowCol := StartOfTokenRowCol;
  EndOfTokenRowCol.Char := EndOfTokenRowCol.Char + Length(Token);
  Editor.BlockEnd := EndOfTokenRowCol;}

  {Editor.BeginUndoBlock;
  Editor.UndoList.AddChange(crDelete, StartOfTokenRowCol, EndOfTokenRowCol, Token, smNormal);
  Editor.CommandProcessor(ecUpperCase, #0, nil);
  Editor.EndUndoBlock;}

  {Editor.BeginUndoBlock;
  Editor.ExecuteCommand(ecUpperCase, #0, nil);
  Editor.EndUndoBlock;}

  {Editor.BeginUndoBlock;
  Editor.UndoList.AddChange(crDelete, StartOfTokenRowCol, EndOfTokenRowCol, Token, smNormal);
  Editor.LockUndo;
  Editor.SelText := UpperCase(Token);
  Editor.UnlockUndo;
  Editor.UndoList.AddChange(crPaste, StartOfTokenRowCol, EndOfTokenRowCol, UpperCase(Token), smNormal);
  Editor.EndUndoBlock;}

  //Editor.UndoList.EndBlock;
end;


procedure TMainForm.SynMemoQueryMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Editor: TSynEdit;
  NewFontSize: Integer;
begin
  // Change font size with MouseWheel
  // TODO: broken in high-dpi mode, just zooms in
  if KeyPressed(VK_CONTROL) and AppSettings.ReadBool(asWheelZoom) then begin
    Editor := TSynEdit(Sender);
    NewFontSize := Editor.Font.Size;
    if WheelDelta > 0 then
      Inc(NewFontSize)
    else
      Dec(NewFontSize);
    NewFontSize := Max(NewFontSize, 1);
    AppSettings.ResetPath;
    AppSettings.WriteInt(asFontSize, NewFontSize);
    SetupSynEditors;
    Handled := True;
  end else begin
    Handled := False;
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

  if (Start >= 0) and (Start < length(Editor.Text)) then
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
          Editor.Canvas.Font.Color := MatchingBraceForegroundColor;
          Editor.Canvas.Brush.Color := MatchingBraceBackgroundColor;
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
    menuEditVariable.Hint := _(SUnsupported);
end;


procedure TMainForm.popupDBPopup(Sender: TObject);
var
  Obj: PDBObject;
  HasFocus, IsDbOrObject: Boolean;
  Version: Integer;
begin
  // DBtree and ListTables both use popupDB as menu
  if DBtreeClicked(Sender) then begin
    Obj := DBTree.GetNodeData(DBTree.FocusedNode);
    IsDbOrObject := Obj.NodeType in [lntDb, lntTable..lntEvent];
    actCreateDatabase.Enabled := Obj.NodeType = lntNone;
    actCreateTable.Enabled := IsDbOrObject or (Obj.GroupType = lntTable);
    actCreateView.Enabled := IsDbOrObject or (Obj.GroupType = lntView);
    actCreateRoutine.Enabled := IsDbOrObject or (Obj.GroupType in [lntFunction, lntProcedure]);
    actCreateTrigger.Enabled := IsDbOrObject or (Obj.GroupType = lntTrigger);
    actCreateEvent.Enabled := IsDbOrObject or (Obj.GroupType = lntEvent);
    actDropObjects.Enabled := IsDbOrObject;
    actCopyTable.Enabled := Obj.NodeType in [lntTable, lntView];
    actEmptyTables.Enabled := Obj.NodeType in [lntTable, lntView];
    actRunRoutines.Enabled := Obj.NodeType in [lntProcedure, lntFunction];
    menuClearDataTabFilter.Enabled := Obj.NodeType in [lntTable, lntView];
    menuEditObject.Enabled := IsDbOrObject;
    // Enable certain items which are valid only here
    menuTreeExpandAll.Enabled := True;
    menuTreeCollapseAll.Enabled := True;
    menuTreeOptions.Enabled := True;
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
    menuClearDataTabFilter.Enabled := False;
    menuEditObject.Enabled := HasFocus;
    actCopyTable.Enabled := False;
    if HasFocus then begin
      Obj := ListTables.GetNodeData(ListTables.FocusedNode);
      actCopyTable.Enabled := Obj.NodeType in [lntTable, lntView];
    end;
    menuTreeExpandAll.Enabled := False;
    menuTreeCollapseAll.Enabled := False;
    menuTreeOptions.Enabled := False;
  end;
  if (ActiveConnection <> nil) and (ActiveConnection.Parameters.IsMySQL) then begin
    Version := ActiveConnection.ServerVersionInt;
    actCreateView.Enabled := actCreateView.Enabled and (Version >= 50001);
    actCreateRoutine.Enabled := actCreateRoutine.Enabled and (Version >= 50003);
    actCreateTrigger.Enabled := actCreateTrigger.Enabled and (Version >= 50002);
    actCreateEvent.Enabled := actCreateEvent.Enabled and (Version >= 50100);
  end;
end;


procedure TMainForm.popupDataGridPopup(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Results: TDBQuery;
  i: Integer;
  Col, Value: String;
  CellFocused, InDataGrid, HasNullValue, HasNotNullValue: Boolean;
  RowNumber: PInt64;
  Node: PVirtualNode;
  OldDataLocalNumberFormat: Boolean;
  IncludedValues: TStringList;
const
  CLPBRD : String = 'CLIPBOARD';
begin
  // Manipulate quick filter menuitems
  Grid := ActiveGrid;
  // Make sure ValidateControls detects the grid as focused, which is not the case when
  // it has 0 nodes, even with TreeOptions.SelectionOptions.RightclickSelect enabled
  Grid.SetFocus;
  CellFocused := Assigned(Grid.FocusedNode) and (Grid.FocusedColumn > NoColumn);
  InDataGrid := Grid = DataGrid;
  DataInsertValue.Enabled := CellFocused;
  QFvalues.Enabled := CellFocused;
  menuQuickFilter.Enabled := InDataGrid;
  actDataResetSorting.Enabled := InDataGrid;
  menuSQLHelpData.Enabled := InDataGrid;
  Refresh3.Enabled := InDataGrid;
  actGridEditFunction.Enabled := CellFocused;

  if not CellFocused then
    Exit;
  Results := GridResult(Grid);

  Col := ActiveConnection.QuoteIdent(Results.ColumnOrgNames[Grid.FocusedColumn]);

  // Block 1: WHERE col IN ([focused cell values])
  QF1.Hint := '';
  QF2.Hint := '';
  QF3.Hint := '';
  QF4.Hint := '';
  QF5.Hint := '';
  QF6.Hint := '';
  QF7.Hint := '';
  Node := Grid.GetFirstSelected;
  HasNullValue := False;
  HasNotNullValue := False;
  OldDataLocalNumberFormat := DataLocalNumberFormat;
  DataLocalNumberFormat := False;
  IncludedValues := TStringList.Create;
  while Assigned(Node) do begin
    AnyGridEnsureFullRow(Grid, Node);
    RowNumber := Grid.GetNodeData(Node);
    Results.RecNo := RowNumber^;
    if Results.IsNull(Grid.FocusedColumn) then
      HasNullValue := True
    else begin
      HasNotNullValue := True;
      Value := Grid.Text[Node, Grid.FocusedColumn];
      if IncludedValues.IndexOf(Value) = -1 then begin
        QF1.Hint := QF1.Hint + esc(Value) + ', ';
        QF2.Hint := QF2.Hint + esc(Value) + ', ';
        QF5.Hint := QF5.Hint + Col + ' LIKE ''' + esc(Value, True, False) + '%'' OR ';
        QF6.Hint := QF6.Hint + Col + ' LIKE ''%' + esc(Value, True, False) + ''' OR ';
        QF7.Hint := QF7.Hint + Col + ' LIKE ''%' + esc(Value, True, False) + '%'' OR ';
        QF3.Hint := QF3.Hint + Col + ' > ' + esc(Value) + ' OR ';
        QF4.Hint := QF4.Hint + Col + ' < ' + esc(Value) + ' OR ';
        IncludedValues.Add(Value);
      end;
    end;
    Node := Grid.GetNextSelected(Node);
    if Length(QF1.Hint) > SIZE_MB then
      Break;
  end;
  DataLocalNumberFormat := OldDataLocalNumberFormat;
  if HasNotNullValue then begin
    QF1.Hint := Col + ' IN (' + Copy(QF1.Hint, 1, Length(QF1.Hint)-2) + ')';
    QF2.Hint := Col + ' NOT IN (' + Copy(QF2.Hint, 1, Length(QF2.Hint)-2) + ')';
    QF5.Hint := Copy(QF5.Hint, 1, Length(QF5.Hint)-4);
    QF6.Hint := Copy(QF6.Hint, 1, Length(QF6.Hint)-4);
    QF7.Hint := Copy(QF7.Hint, 1, Length(QF7.Hint)-4);
    QF3.Hint := Copy(QF3.Hint, 1, Length(QF3.Hint)-4);
    QF4.Hint := Copy(QF4.Hint, 1, Length(QF4.Hint)-4);
  end;
  if HasNullValue then begin
    if HasNotNullValue then begin
      QF1.Hint := QF1.Hint + ' OR ';
      QF2.Hint := QF2.Hint + ' AND ';
      QF5.Hint := QF5.Hint + ' OR ';
      QF6.Hint := QF6.Hint + ' OR ';
      QF7.Hint := QF7.Hint + ' OR ';
      QF3.Hint := QF3.Hint + ' OR ';
      QF4.Hint := QF4.Hint + ' OR ';
    end;
    QF1.Hint := QF1.Hint + Col + ' IS NULL';
    QF2.Hint := QF2.Hint + Col + ' IS NOT NULL';
    QF5.Hint := QF5.Hint + Col + ' IS NULL';
    QF6.Hint := QF6.Hint + Col + ' IS NULL';
    QF7.Hint := QF7.Hint + Col + ' IS NULL';
    QF3.Hint := QF3.Hint + Col + ' IS NULL';
    QF4.Hint := QF4.Hint + Col + ' IS NULL';
  end;
  QF1.Visible := HasNotNullValue or HasNullValue;
  QF2.Visible := HasNotNullValue or HasNullValue;
  QF3.Visible := HasNotNullValue;
  QF4.Visible := HasNotNullValue;
  QF5.Visible := HasNotNullValue;
  QF6.Visible := HasNotNullValue;
  QF7.Visible := HasNotNullValue;
  IncludedValues.Free;

  // Block 2: WHERE col = [ask user for value]
  QF8.Hint := Col + ' = "..."';
  QF9.Hint := Col + ' != "..."';
  QF10.Hint := Col + ' > "..."';
  QF11.Hint := Col + ' < "..."';
  QF12.Hint := Col + ' LIKE "%...%"';
  QF13.Hint := Col + ' IS NULL';
  QF14.Hint := Col + ' IS NOT NULL';

  // Block 3: WHERE col = [clipboard content]
  Value := Trim(Clipboard.AsText);
  if Length(Value) < SIZE_KB then begin
    QF15.Enabled := true; QF15.Hint := Col + ' = ' + esc(Value);
    QF16.Enabled := true; QF16.Hint := Col + ' != ' + esc(Value);
    QF17.Enabled := true; QF17.Hint := Col + ' > ' + esc(Value);
    QF18.Enabled := true; QF18.Hint := Col + ' < ' + esc(Value);
    QF19.Enabled := true; QF19.Hint := Col + ' LIKE ''%' + esc(Value, True, False) + '%''';
    QF20.Enabled := true; QF20.Hint := Col + ' IN (' + Value + ')';
  end else begin
    QF15.Enabled := false; QF15.Hint := Col + ' = ' + CLPBRD;
    QF16.Enabled := false; QF16.Hint := Col + ' != ' + CLPBRD;
    QF17.Enabled := false; QF17.Hint := Col + ' > ' + CLPBRD;
    QF18.Enabled := false; QF18.Hint := Col + ' < ' + CLPBRD;
    QF19.Enabled := false; QF19.Hint := Col + ' LIKE %' + CLPBRD + '%';
    QF20.Enabled := false; QF20.Hint := Col + ' IN (' + CLPBRD + ')';
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
  Data: TDBQuery;
  DbObj: TDBObject;
  Conn: TDBConnection;
  Col, Query: String;
  TableCol: TTableColumn;
  Item: TMenuItem;
  i: Integer;
  MaxSize: Int64;
  ValueList: TStringList;
  ColumnHasIndex: Boolean;
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
  ShowStatusMsg(_('Fetching distinct values ...'));
  DbObj := ActiveDbObj;
  Conn := DbObj.Connection;
  MaxSize := SIZE_GB;
  ColumnHasIndex := DataGridResult.ColIsKeyPart(DataGrid.FocusedColumn)
    or DataGridResult.ColIsUniqueKeyPart(DataGrid.FocusedColumn)
    or DataGridResult.ColIsPrimaryKeyPart(DataGrid.FocusedColumn);
  if ColumnHasIndex then begin
    MaxSize := MaxSize * 5;
  end;
  if DbObj.Size < MaxSize then begin
    Query := Conn.QuoteIdent(Col)+', COUNT(*) AS c FROM '+DbObj.QuotedName;
    if SynMemoFilter.Text <> '' then
      Query := Query + ' WHERE ' + SynMemoFilter.Text + CRLF;
    Query := Query + ' GROUP BY '+Conn.QuoteIdent(Col)+' ORDER BY c DESC, '+Conn.QuoteIdent(Col);
    Data := Conn.GetResults(Conn.ApplyLimitClause('SELECT', Query, 30, 0));
    for i:=0 to Data.RecordCount-1 do begin
      if QFvalues.Count > i then
        Item := QFvalues[i]
      else begin
        Item := TMenuItem.Create(QFvalues);
        QFvalues.Add(Item);
      end;
      if Data.IsNull(Col) then
        Item.Hint := Conn.QuoteIdent(Col)+' IS NULL'
      else
        Item.Hint := Conn.QuoteIdent(Col)+'='+esc(Data.Col(Col));
      Item.Caption := sstr(Item.Hint, 100) + ' (' + FormatNumber(Data.Col('c')) + ')';
      if SynMemoFilter.Text <> '' then begin
        if Pos(Item.Hint, SynMemoFilter.Text) > 0 then
          Item.Hint := SynMemoFilter.Text
        else
          Item.Hint := SynMemoFilter.Text + ' AND ' + Item.Hint;
      end;
      Item.OnClick := QuickFilterClick;
      Data.Next;
    end;
  end else begin
    // Table is too large for the above SELECT query.
    // Get ENUM/SET values instead if possible
    QFvalues[0].Caption := f_('Table too large (>%s), avoiding long running SELECT query', [FormatByteNumber(MaxSize)]);
    for TableCol in SelectedTableColumns do begin
      if (TableCol.Name = Col) and (TableCol.DataType.Index in [dtEnum, dtSet]) then begin
        ValueList := TableCol.ValueList;
        for i:=0 to ValueList.Count-1 do begin
          if QFvalues.Count > i then
            Item := QFvalues[i]
          else begin
            Item := TMenuItem.Create(QFvalues);
            QFvalues.Add(Item);
          end;
          Item.Hint := Conn.QuoteIdent(Col)+'='+esc(ValueList[i]);
          Item.Caption := sstr(Item.Hint, 100);
          Item.OnClick := QuickFilterClick;
        end;
        Break;
      end;
    end;
  end;
  ShowStatusMsg;
end;


procedure TMainForm.DataInsertValueClick(Sender: TObject);
var
  Timestamp: TDateTime;
  y, m, d, h, i, s, ms: Word;
  Uid: TGuid;
  StrUid: String;
  UnixTimestamp: Int64;
  SystemTime: TSystemTime;
  ColNum: TColumnIndex;
  Col: TTableColumn;
begin
  Timestamp := ActiveConnection.ParseDateTime(ActiveConnection.GetVar('SELECT CURRENT_TIMESTAMP'));
  DecodeDateTime(Timestamp, y, m, d, h, i, s, ms);
  DataDateTime.Caption := Format('%s: %.4d-%.2d-%.2d %.2d:%.2d:%.2d', [_('Date and time'), y,m,d,h,i,s]);
  DataDate.Caption := Format('%s: %.4d-%.2d-%.2d', [_('Date'), y,m,d]);
  DataTime.Caption := Format('%s: %.2d:%.2d:%.2d', [_('Time'), h,i,s]);
  DataYear.Caption := Format('%s: %.4d', [_('Year'), y]);
  GetSystemTime(SystemTime);
  UnixTimestamp := DateTimeToUnix(SystemTimeToDateTime(SystemTime));
  DataUNIXtimestamp.Caption := _('UNIX Timestamp')+': ' + IntToStr(UnixTimestamp);
  CreateGuid(Uid);
  StrUid := GuidToString(Uid);
  DataGUID.Caption := _('GUID') + ': ' + StrUid;
  DataGUIDwobraces.Caption := _('GUID without braces') + ': ' + Copy(StrUid, 2, Length(StrUid)-2);

  ColNum := DataGrid.FocusedColumn;
  DataDefaultValue.Caption := _('Default value')+': ?';
  DataDefaultValue.Enabled := False;
  if ColNum <> NOCOLUMN then begin
    for Col in SelectedTableColumns do begin
      if (Col.Name = DataGrid.Header.Columns[ColNum].Text) and (Col.DefaultType = cdtText) then begin
        DataDefaultValue.Caption := _('Default value')+': '+Col.DefaultText;
        DataDefaultValue.Enabled := True;
        break;
      end;
    end;
  end;
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
  except on E:EDbError do
    ErrorDialog(E.Message);
  end;
end;


function TMainForm.GetRootNode(Tree: TBaseVirtualTree; Connection: TDBConnection): PVirtualNode;
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


function TMainForm.GetActiveConnection: TDBConnection;
begin
  Result := nil;
  if Assigned(FActiveDBObj) and (FActiveDbObj <> nil) then
    Result := FActiveDbObj.Connection;
end;


function TMainForm.GetActiveDatabase: String;
begin
  // Find currently selected database in active connection
  Result := '';
  if (not (csDestroying in ComponentState))
    and Assigned(FActiveDBObj)
    and Assigned(FActiveDBObj.Connection) then
    Result := FActiveDBObj.Connection.Database;
end;


procedure TMainForm.SetActiveDatabase(db: String; Connection: TDBConnection);
var
  SessionNode, DBNode: PVirtualNode;
  DBObj: PDBObject;
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
        if DBNode <> DBtree.FocusedNode then
          SelectNode(DBtree, DBNode);
        break;
      end;
      DBNode := DBtree.GetNextSibling(DBNode);
    end;
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
    LogSQL(f_('Table node "%s" not found in tree.', [Obj.Name]), lcError);
end;


function TMainForm.FindDBObjectNode(Tree: TBaseVirtualTree; Obj: TDBObject): PVirtualNode;
var
  DbNode, ObjectNode, GroupedNode: PVirtualNode;
  DbObj, ObjectObj, GroupedObj: PDBObject;
begin
  Result := nil;
  DbNode := Tree.GetFirstChild(GetRootNode(Tree, Obj.Connection));
  while Assigned(DbNode) do begin
    // Search in database nodes
    DbObj := Tree.GetNodeData(DbNode);
    if DBObj.IsSameAs(Obj) then begin
      Result := DBNode;
      break;
    end;

    // Search in table/view/... nodes
    if DbObj.Database = Obj.Database then begin
      ObjectNode := Tree.GetFirstChild(DbNode);
      while Assigned(ObjectNode) do begin
        ObjectObj := Tree.GetNodeData(ObjectNode);
        if ObjectObj.IsSameAs(Obj) then begin
          Result := ObjectNode;
          break;
        end;

        // Search in grouped table/view/... nodes
        GroupedNode := Tree.GetFirstChild(ObjectNode);
        while Assigned(GroupedNode) do begin
          GroupedObj := Tree.GetNodeData(GroupedNode);
          if GroupedObj.IsSameAs(Obj) then begin
            Result := GroupedNode;
            break;
          end;
          GroupedNode := Tree.GetNextSibling(GroupedNode);
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


procedure TMainForm.filterQueryHelpersChange(Sender: TObject);
var
  rx: TRegExpr;
  Node: PVirtualNode;
  VT: TVirtualStringTree;
  i: Integer;
  match: Boolean;
  CellText: String;
  Edit: TButtonedEdit;
begin
  // Filter nodes in query helpers
  // Loop through all nodes and hide non matching
  VT := ActiveQueryHelpers;
  Edit := Sender as TButtonedEdit;
  Node := VT.GetFirst;
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.Expression := Edit.Text;
  try
    rx.Exec('abc');
  except
    on E:ERegExpr do begin
      if rx.Expression <> '' then begin
        LogSQL('Filter text is not a valid regular expression: "'+rx.Expression+'"', lcError);
        rx.Expression := '';
      end;
    end;
  end;

  VT.BeginUpdate;
  while Assigned(Node) do begin
    if (not VT.HasChildren[Node]) and (VT.GetNodeLevel(Node) > 0) then begin
      // Don't filter anything if the filter text is empty
      match := rx.Expression = '';
      // Search for given text in node's captions
      if not match then for i := 0 to VT.Header.Columns.Count - 1 do begin
        CellText := VT.Text[Node, i];
        match := rx.Exec(CellText);
        if match then
          break;
      end;
      VT.IsVisible[Node] := match;
    end;
    Node := VT.GetNext(Node);
  end;
  VT.EndUpdate;
  VT.Invalidate;
  rx.Free;

  Edit.RightButton.Visible := IsNotEmpty(Edit.Text);
end;


procedure TMainForm.tabsetQueryMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  idx, i: Integer;
  Tabs: TTabSet;
  Rect: TRect;
  Org: TPoint;
  ResultTab: TResultTab;
  HintSQL: TStringList;
begin
  // Display some hint with row/col count + SQL when mouse hovers over result tab
  if (FLastHintMousepos.X = x) and (FLastHintMousepos.Y = Y) then
    Exit;
  FLastHintMousepos := Point(X, Y);
  Tabs := Sender as TTabSet;
  idx := Tabs.ItemAtPos(Point(X, Y), True);
  if (idx = -1) or (idx = FLastHintControlIndex) then
    Exit;
  FLastHintControlIndex := idx;
  // Check if user wants these balloon hints
  if not AppSettings.ReadBool(asHintsOnResultTabs) then
    Exit;

  // Make SQL readable for the tooltip balloon. WrapText() is unsuitable here.
  // See issue #2014
  // Also, wee need to work around the awful looking balloon text:
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=73771
  ResultTab := ActiveQueryTab.ResultTabs[idx];
  HintSQL := TStringList.Create;
  HintSQL.Text := Trim(ResultTab.Results.SQL);
  for i:=0 to HintSQL.Count-1 do begin
    HintSQL[i] := sstr(HintSQL[i], 100);
    HintSQL[i] := StringReplace(HintSQL[i], #9, '    ', [rfReplaceAll]);
  end;
  BalloonHint1.Description := FormatNumber(ResultTab.Results.ColumnCount) + ' columns × ' +
    FormatNumber(ResultTab.Results.RecordCount) + ' rows' + CRLF + CRLF +
    Trim(sstr(HintSQL.Text, SIZE_KB));
  Rect := Tabs.ItemRect(idx);
  Org := Tabs.ClientOrigin;
  OffsetRect(Rect, Org.X, Org.Y);
  BalloonHint1.ShowHint(Rect);
end;


procedure TMainForm.tabsetQueryMouseLeave(Sender: TObject);
begin
  // BalloonHint.HideAfter is -1, so it will stay forever if we wouldn't hide it at some point
  BalloonHint1.HideHint;
  FLastHintControlIndex := -1;
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

  snippetfile := AppSettings.DirnameSnippets + ActiveQueryHelpers.Text[ActiveQueryHelpers.FocusedNode, 0] + '.sql';
  if MessageDialog(_('Delete snippet file?'), snippetfile, mtConfirmation, [mbOk, mbCancel]) = mrOk then
  begin
    Screen.Cursor := crHourGlass;
    if DeleteFile(snippetfile) then begin
      // Refresh list with snippets
      SetSnippetFilenames;
    end else begin
      Screen.Cursor := crDefault;
      ErrorDialog(f_('Failed deleting %s', [snippetfile]));
    end;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainForm.menuDoubleClickInsertsNodeTextClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  // Activate doubleclick node feature
  Item := Sender as TMenuItem;
  Item.Checked := not Item.Checked;
  AppSettings.ResetPath;
  AppSettings.WriteBool(asDoubleClickInsertsNodeText, Item.Checked);
end;

{**
  Load snippet at cursor
}
procedure TMainForm.menuInsertSnippetAtCursorClick(Sender: TObject);
begin
  ActiveQueryTab.LoadContents(AppSettings.DirnameSnippets + ActiveQueryHelpers.Text[ActiveQueryHelpers.FocusedNode, 0] + '.sql', False, nil);
end;


{**
  Load snippet and replace content
}
procedure TMainForm.menuLoadSnippetClick(Sender: TObject);
begin
  ActiveQueryTab.LoadContents(AppSettings.DirnameSnippets + ActiveQueryHelpers.Text[ActiveQueryHelpers.FocusedNode, 0] + '.sql', True, nil);
end;


{**
  Open snippets-directory in Explorer
}
procedure TMainForm.menuExploreClick(Sender: TObject);
begin
  ShellExec('', AppSettings.DirnameSnippets);
end;


procedure TMainForm.menuClearQueryHistoryClick(Sender: TObject);
var
  Values: TStringList;
  PathToDelete: String;
begin
  // Clear query history items in registry
  // Take care of MessageDialog, probably changing the current SessionPath
  PathToDelete := ActiveConnection.Parameters.SessionPath + '\' + REGKEY_QUERYHISTORY;
  AppSettings.SessionPath := PathToDelete;
  Values := AppSettings.GetValueNames;
  if MessageDialog(_('Clear query history?'), f_('%s history items will be deleted.', [FormatNumber(Values.Count)]), mtConfirmation, [mbYes, mbNo]) = mrYes then begin
    Screen.Cursor := crHourglass;
    AppSettings.SessionPath := PathToDelete;
    AppSettings.DeleteCurrentKey;
    RefreshHelperNode(HELPERNODE_HISTORY);
    Screen.Cursor := crDefault;
  end;
  Values.Free;
  AppSettings.ResetPath;
end;


procedure TMainForm.menuFetchDBitemsClick(Sender: TObject);
var
  Node: PVirtualNode;
  db: String;
  Conn: TDBConnection;
begin
  // Fill db object cache of selected databases
  try
    Screen.Cursor := crHourglass;
    Node := GetNextNode(ListDatabases, nil, True);
    Conn := ActiveConnection;
    while Assigned(Node) do begin
      db := ListDatabases.Text[Node, 0];
      Conn.GetDBObjects(db, True);
      ListDatabases.RepaintNode(Node);
      DBtree.RepaintNode(FindDBNode(DBtree, Conn, db));
      Node := GetNextNode(ListDatabases, Node, True);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;


{**
  A column header of a VirtualStringTree was clicked:
  Toggle the sort direction
}
procedure TMainForm.AnyGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  i: Integer;
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
  if Sender.Columns[HitInfo.Column].CheckBox then
    Exit;

  // Clear sort icons
  for i:=0 to Sender.Columns.Count-1 do begin
    Sender.Columns[i].ImageIndex := -1;
  end;

  if (Sender.SortColumn <> HitInfo.Column) or (Sender.SortDirection = sdDescending) then begin
    Sender.SortDirection := sdAscending;
    Sender.Columns[HitInfo.Column].ImageIndex := 109;
  end else if Sender.SortDirection = sdAscending then begin
    Sender.SortDirection := sdDescending;
    Sender.Columns[HitInfo.Column].ImageIndex := 110;
  end;
  Screen.Cursor := crHourglass;
  Sender.SortColumn := HitInfo.Column;
  Sender.Treeview.SortTree( HitInfo.Column, Sender.SortDirection );
  Screen.Cursor := crDefault;
end;


{**
  Sorting a column of a VirtualTree by comparing two cells
}
procedure TMainForm.AnyGridCompareNodes(Sender: TBaseVirtualTree; Node1,
    Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  VT: TVirtualStringTree;
begin
  VT := Sender as TVirtualStringTree;
  if Assigned(Node1) and Assigned(Node2) then
    Result := CompareAnyNode(VT.Text[Node1, Column], VT.Text[Node2, Column]);
end;


{**
  VirtualTree was painted. Adjust background color of sorted column.
}
procedure TMainForm.AnyGridAfterPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
var
  i: Integer;
  h: TVTHeader;
  NewColor: TColor;
begin
  h := (Sender as TVirtualStringTree).Header;
  for i:=0 to h.Columns.Count-1 do begin
    NewColor := GetThemeColor(clWindow);
    if h.SortColumn = i then
      NewColor := ColorAdjustBrightness(NewColor, COLORSHIFT_SORTCOLUMNS);
    h.Columns[i].Color := NewColor;
  end;
end;


{**
  Save setup of a VirtualStringTree to registry
}
procedure TMainForm.SaveListSetup( List: TVirtualStringTree );
var
  i, ColWidth: Integer;
  ColWidths, ColsVisible, ColPos, Regname: String;
  OwnerForm: TWinControl;
begin
  // Prevent sporadic crash on startup
  if List = nil then
    Exit;
  ColWidths := '';
  ColsVisible := '';
  ColPos := '';
  OwnerForm := GetParentFormOrFrame(List);
  for i := 0 to List.Header.Columns.Count - 1 do
  begin
    // Column widths
    if ColWidths <> '' then
      ColWidths := ColWidths + ',';
    ColWidth := List.Header.Columns[i].Width;
    ColWidths := ColWidths + IntToStr(ColWidth);

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

  // On a windows shutdown, GetParentForm() seems sporadically unable to find the owner form
  // In that case we would cause an exception when accessing it. Emergency break in that case.
  // See issue #1462
  // TODO: Test this, probably fixed by implementing GetParentFormOrFrame, and then again, probably not.
  if not Assigned(OwnerForm) then
    Exit;
  // Lists can have the same name over different forms or frames. Find parent form or frame,
  // so we can prepend its name into the registry value name.
  Regname := OwnerForm.Name + '.' + List.Name;
  AppSettings.ResetPath;
  AppSettings.WriteString(asListColWidths, ColWidths, Regname);
  AppSettings.WriteString(asListColsVisible, ColsVisible, Regname);
  AppSettings.WriteString(asListColPositions, ColPos, Regname);
  AppSettings.WriteString(asListColSort, IntToStr(List.Header.SortColumn) + ',' + IntToStr(Integer(List.Header.SortDirection)), RegName);
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
  Value := AppSettings.ReadString(asListColWidths, Regname);
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    for i := 0 to ValueList.Count - 1 do
    begin
      colwidth := MakeInt(ValueList[i]);
      // Check if column number exists and width is at least 1 pixel
      if (List.Header.Columns.Count > i) and (colwidth > 0) and (colwidth < 1000) then
        List.Header.Columns[i].Width := colwidth;
    end;
  end;

  // Column visibility
  Value := AppSettings.ReadString(asListColsVisible, Regname);
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
  Value := AppSettings.ReadString(asListColPositions, Regname);
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
  Value := AppSettings.ReadString(asListColSort, Regname);
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
procedure TMainForm.SetLogToFile(Value: Boolean);
var
  LogfilePattern, LogDir: String;
  i : Integer;
begin
  if Value = FLogToFile then
    Exit;

  if Value then begin
    // Ensure directory exists
    LogDir := AppSettings.ReadString(asSessionLogsDirectory);
    LogDir := IncludeTrailingPathDelimiter(LogDir);
    ForceDirectories(LogDir);

    // Determine free filename
    LogfilePattern := '%.6u.log';
    i := 1;
    FFileNameSessionLog := LogDir + goodfilename(Format(LogfilePattern, [i]));
    while FileExists(FFileNameSessionLog) do begin
      inc(i);
      FFileNameSessionLog := LogDir + goodfilename(Format(LogfilePattern, [i]));
    end;

    // Create file handle for writing
    AssignFile( FFileHandleSessionLog, FFileNameSessionLog );
    {$I-} // Supress errors
    if FileExists(FFileNameSessionLog) then
      Append(FFileHandleSessionLog)
    else
      Rewrite(FFileHandleSessionLog);
    {$I+}
    if IOResult <> 0 then begin
      AppSettings.WriteBool(asLogToFile, False);
      ErrorDialog(_('Error opening session log file'), FFileNameSessionLog+CRLF+CRLF+_('Logging is disabled now.'));
    end else begin
      FLogToFile := Value;
      LogSQL(f_('Writing to session log file now: %s', [FFileNameSessionLog]));
    end;
  end else begin
    {$I-} // Supress errors
    CloseFile(FFileHandleSessionLog);
    {$I+}
    // Reset IOResult so later checks in ActivateFileLogging doesn't get an old value
    IOResult;
    FLogToFile := Value;
    LogSQL(_('Writing to session log file disabled now'));
  end;
end;


{**
  Display tooltips in VirtualTrees. Imitates default behaviour of TListView.
}
procedure TMainForm.AnyGridGetHint(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; var LineBreakStyle:
    TVTTooltipLineBreakStyle; var HintText: String);
var
  Tree: TVirtualStringTree;
begin
  // Disable tooltips on Wine, as they prevent users from clicking + editing clipped cells
  if not FIsWine then begin
    Tree := TVirtualStringTree(Sender);
    HintText := Tree.Text[Node, Column];
    HintText := sstr(HintText, SIZE_KB);
    // See http://www.heidisql.com/forum.php?t=20458#p20548
    if Sender = DBtree then
      LineBreakStyle := hlbForceSingleLine
    else
      LineBreakStyle := hlbForceMultiLine;
  end;
end;


procedure TMainForm.actLogHorizontalScrollbarExecute(Sender: TObject);
begin
  // Toggle visibility of horizontal scrollbar
  if TAction(Sender).Checked then
    SynMemoSQLLog.ScrollBars := ssBoth
  else
    SynMemoSQLLog.ScrollBars := ssVertical;
end;


{**
  Enable/disable file logging by popupmenuclick
}
procedure TMainForm.menuLogToFileClick(Sender: TObject);
begin
  LogToFile := not LogToFile;
  // Save option
  AppSettings.ResetPath;
  AppSettings.WriteBool(asLogToFile, LogToFile);
end;


{**
  Open folder with session logs
}
procedure TMainForm.menuOpenLogFolderClick(Sender: TObject);
begin
  ShellExec('', AppSettings.ReadString(asSessionLogsDirectory));
end;


{**
  A header column of a VirtualTree was "dragged out", which means:
  dragged down or up, not to the left or right.
  We imitate the behaviour of various applications (fx Outlook) and
  hide this dragged column
}
procedure TMainForm.AnyGridHeaderDraggedOut(Sender: TVTHeader; Column:
    TColumnIndex; DropPosition: TPoint);
begin
  // Hide the draggedout column
  Sender.Columns[Column].Options := Sender.Columns[Column].Options - [coVisible];
end;


procedure TMainForm.AnyGridIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
      1: PaintColorBar(Obj.Rows, FDBObjectsMaxRows, TargetCanvas, CellRect);
      2: PaintColorBar(Obj.Size, FDBObjectsMaxSize, TargetCanvas, CellRect);
    end;
  end;
end;


procedure TMainForm.PaintColorBar(Value, Max: Extended; TargetCanvas: TCanvas; CellRect: TRect);
var
  BarWidth, CellWidth: Integer;
begin
  if not AppSettings.ReadBool(asDisplayBars) then
    Exit;

  // Add minimal margin to cell edges
  InflateRect(CellRect, -1, -1);
  CellWidth := CellRect.Right - CellRect.Left;

  // Avoid division by zero, when max is 0 - very rare case but reported in issue #2196.
  if (Value > 0) and (Max > 0) then begin
    BarWidth := Round(CellWidth / Max * Value);
    TargetCanvas.Brush.Color := ColorAdjustBrightness(TargetCanvas.Brush.Color, 20);
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
  NodeFocused, EnableControls: Boolean;
  SQL: String;
begin
  NodeFocused := Assigned(Node);
  SQL := '';
  if NodeFocused then begin
    SQL := ListProcesses.Text[Node, 7];
  end;
  EnableControls := not SQL.IsEmpty;
  if EnableControls then begin
    SynMemoProcessView.Highlighter := SynSQLSynUsed;
    SynMemoProcessView.Text := ListProcesses.Text[Node, 7];
    SynMemoProcessView.Color := clWindow;
  end else begin
    SynMemoProcessView.Highlighter := nil;
    SynMemoProcessView.Text := _('Please select a process with a running query');
    SynMemoProcessView.Color := clBtnFace;
  end;

  SynMemoProcessView.Enabled := EnableControls;
  pnlProcessView.Enabled := EnableControls;
  lblExplainProcess.Enabled := EnableControls and ActiveConnection.Parameters.IsMySQL;
  menuExplainProcess.Enabled := lblExplainProcess.Enabled;
  lblExplainProcessAnalyzer.Enabled := lblExplainProcess.Enabled;
  menuExplainAnalyzer.Enabled := lblExplainProcess.Enabled;
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


procedure TMainForm.editDatabaseTableFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    (Sender as TButtonedEdit).OnRightButtonClick(Sender);
end;


procedure TMainForm.TimerFilterVTTimer(Sender: TObject);
begin
  // Disable timer to avoid filtering in a loop
  TimerFilterVT.Enabled := False;

  // Code moved into this procedure in order to call it by different way
  ApplyVTFilter(True);
end;


procedure TMainForm.ApplyVTFilter(FromTimer: Boolean);
var
  Node: PVirtualNode;
  VT: TVirtualStringTree;
  i: Integer;
  match: Boolean;
  tab: TTabSheet;
  VisibleCount: Cardinal;
  CellText: String;
  rx: TRegExpr;
  OldDataLocalNumberFormat: Boolean;
  OldImageIndex: Integer;
begin
  // Find the correct VirtualTree that shall be filtered
  tab := PageControlMain.ActivePage;
  if tab = tabHost then
    tab := PageControlHost.ActivePage;
  VT := nil;
  if tab = tabDatabases then begin
    VT := ListDatabases;
    FFilterTextDatabases := editFilterVT.Text;
  end else if tab = tabVariables then begin
    VT := ListVariables;
    FFilterTextVariables := editFilterVT.Text;
  end else if tab = tabStatus then begin
    VT := ListStatus;
    FFilterTextStatus := editFilterVT.Text;
  end else if tab = tabProcesslist then begin
    VT := ListProcesses;
    FFilterTextProcessList := editFilterVT.Text;
  end else if tab = tabCommandStats then begin
    VT := ListCommandStats;
    FFilterTextCommandStats := editFilterVT.Text;
  end else if tab = tabDatabase then begin
    VT := ListTables;
    FFilterTextDatabase := editFilterVT.Text;
  end else if tab = tabEditor then begin
    if ActiveObjectEditor is TfrmTableEditor then
      VT := TfrmTableEditor(ActiveObjectEditor).listColumns;
    FFilterTextEditor := editFilterVT.Text;
  end else if tab = tabData then begin
    VT := DataGrid;
    FFilterTextData := editFilterVT.Text;
  end else if QueryTabActive and (ActiveQueryTab.ActiveResultTab <> nil) then begin
    VT := ActiveGrid;
    ActiveQueryTab.ActiveResultTab.FilterText := editFilterVT.Text;
  end;
  if not Assigned(VT) then
    Exit;
  // Loop through all nodes and hide non matching
  Node := VT.GetFirst;
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.Expression := editFilterVT.Text;
  try
    rx.Exec('abc');
  except
    on E:ERegExpr do begin
      if rx.Expression <> '' then begin
        LogSQL('Filter text is not a valid regular expression: "'+rx.Expression+'"', lcError);
        rx.Expression := '';
      end;
    end;
  end;
  VisibleCount := 0;
  OldDataLocalNumberFormat := DataLocalNumberFormat;
  DataLocalNumberFormat := False;
  // Display hour glass instead of X icon
  OldImageIndex := editFilterVT.RightButton.ImageIndex;
  editFilterVT.RightButton.ImageIndex := 150;
  editFilterVT.Repaint;

  VT.BeginUpdate;
  while Assigned(Node) do begin
    // Don't filter anything if the filter text is empty
    match := rx.Expression = '';
    // Search for given text in node's captions
    if not match then for i := 0 to VT.Header.Columns.Count - 1 do begin
      CellText := VT.Text[Node, i];
      match := rx.Exec(CellText);
      if match then
        break;
    end;
    VT.IsVisible[Node] := match;
    if match then
      inc(VisibleCount);
    Node := VT.GetNext(Node);
  end;
  VT.EndUpdate;
  if rx.Expression <> '' then begin
    lblFilterVTInfo.Caption := f_('%0:d out of %1:d matching. %2:d hidden.', [VisibleCount, VT.RootNodeCount, VT.RootNodeCount - VisibleCount]);
  end else
    lblFilterVTInfo.Caption := '';

  if FromTimer then
    VT.Invalidate
  else
    InvalidateVT(VT, VTREE_LOADED, true);
  DataLocalNumberFormat := OldDataLocalNumberFormat;
  editFilterVT.RightButton.ImageIndex := OldImageIndex;
  rx.Free;
end;


procedure TMainForm.ApplyFontToGrids;
var
  i, j: Integer;
  QueryTab: TQueryTab;
  Grid: TVirtualStringTree;
begin
  // Apply somehow changed font settings to all existing grids
  DataGrid.Font.Name := AppSettings.ReadString(asDataFontName);
  DataGrid.Font.Size := AppSettings.ReadInt(asDataFontSize);
  FixVT(Mainform.DataGrid, AppSettings.ReadInt(asGridRowLineCount));
  for i:=Mainform.tabQuery.PageIndex to Mainform.PageControlMain.PageCount-1 do begin
    QueryTab := Mainform.QueryTabs[i-Mainform.tabQuery.PageIndex];
    for j:=0 to QueryTab.ResultTabs.Count-1 do begin
      Grid := QueryTab.ResultTabs[j].Grid;
      Grid.Font.Name := AppSettings.ReadString(asDataFontName);
      Grid.Font.Size := AppSettings.ReadInt(asDataFontSize);
      FixVT(Grid, AppSettings.ReadInt(asGridRowLineCount));
    end;
  end;
end;


procedure TMainForm.PrepareImageList;
var
  IconPack: String;
  WantedImageCollection: TComponent;
begin
  // Load preferred ImageCollection into VirtualImageList
  VirtualImageListMain.Clear;
  IconPack := AppSettings.ReadString(asIconPack);
  WantedImageCollection := FindComponent('ImageCollection' + IconPack);
  if (WantedImageCollection <> nil) and (WantedImageCollection is TImageCollection) then begin
    VirtualImageListMain.ImageCollection := WantedImageCollection as TImageCollection;
  end else begin
    VirtualImageListMain.ImageCollection := ImageCollectionIcons8;
  end;
  VirtualImageListMain.Add('', 0, VirtualImageListMain.ImageCollection.Count-1);
end;


procedure TMainForm.ListVariablesBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  SessionVal, GlobalVal: String;
  vt: TVirtualStringTree;
begin
  // Highlight cell if session variable is different to global variable
  vt := Sender as TVirtualStringTree;
  if Column = 1 then begin
    SessionVal := vt.Text[Node, 1];
    GlobalVal := vt.Text[Node, 2];
    if SessionVal <> GlobalVal then begin
      TargetCanvas.Brush.Color := clWebBlanchedAlmond;
      TargetCanvas.Pen.Color := TargetCanvas.Brush.Color;
      TargetCanvas.Rectangle(CellRect);
    end;
  end;
end;


procedure TMainForm.ListVariablesDblClick(Sender: TObject);
begin
  menuEditVariable.Click;
end;


procedure TMainForm.ListVariablesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Idx: PInt64;
  i, tmp: Integer;
  VarName, SessionVal, GlobalVal: String;
  dcat: TDBDatatypeCategoryIndex;
  vt: TVirtualStringTree;
begin
  vt := Sender as TVirtualStringTree;
  Idx := Sender.GetNodeData(Node);
  VarName := FVariableNames[Idx^];
  tmp := -1;
  for i:=Low(MySQLVariables) to High(MySQLVariables) do begin
    if MySQLVariables[i].Name = VarName then begin
      tmp := i;
      break;
    end;
  end;
  if (tmp=-1) or (not MySQLVariables[tmp].IsDynamic) then begin
    // Gray out whole row if the variable is either unknown or not editable
    TargetCanvas.Font.Color := GetThemeColor(clGrayText)
  end else if Column in [1, 2] then begin
    SessionVal := vt.Text[Node, 1];
    GlobalVal := vt.Text[Node, 2];
    if IsInt(SessionVal) or IsInt(GlobalVal) then
      dcat := dtcInteger
    else if (tmp > -1) and ((MySQLVariables[tmp].EnumValues <> '') or (Pos(UpperCase(SessionVal), 'ON,OFF,0,1,YES,NO')>0)) then
      dcat := dtcOther
    else
      dcat := dtcText;
    TargetCanvas.Font.Color := DatatypeCategories[dcat].Color;
  end;
end;


procedure TMainForm.HostListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Results: TDBQuery;
  Idx: PInt64;
  IsIdle: Boolean;
begin
  if (Column <> (Sender as TVirtualStringTree).Header.MainColumn) then
    exit;
  if Sender = ListProcesses then begin
    Idx := Sender.GetNodeData(Node);
    Results := GridResult(Sender);
    Results.RecNo := Idx^;
    case Kind of
      ikNormal, ikSelected: begin
        case Results.Connection.Parameters.NetTypeGroup of
          ngMySQL: IsIdle := Results.Col('Info') = '';
          ngMSSQL: IsIdle := (Results.Col(6) <> 'running') and (Results.Col(6) <> 'runnable');
          else IsIdle := False;
        end;
        if IsIdle then begin
          if MakeInt(Results.Col(5)) < 60 then
            ImageIndex := 151 // Idle, same icon as in lower right status panel
          else
            ImageIndex := 167 // Long idle thread
        end else
          ImageIndex := actExecuteQuery.ImageIndex; // Running query
        end;
      ikOverlay: begin
        if IntToStr(Results.Connection.ThreadId) = Results.Col(0) then
          ImageIndex := 168; // Indicate users own thread id
        if CompareText(Results.Col(4), 'Killed') = 0 then
          ImageIndex := 158; // Broken
        end;
      else;
    end;
  end else begin
    case Kind of
      ikNormal, ikSelected: ImageIndex := 25;
      else;
    end;
  end;
end;


procedure TMainForm.HostListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Idx: PInt64;
  Results: TDBQuery;
  ValIsBytes, ValIsNumber: Boolean;
  ValCount, CommandCount: Int64;
  tmpval: Double;
begin
  Idx := Sender.GetNodeData(Node);
  Results := GridResult(Sender);
  // See issue #3416
  if (Results = nil) and (Sender <> ListVariables) then
    Exit;
  if Results <> nil then
    Results.RecNo := Idx^;

  if (Sender = ListStatus) and (Column in [1,2,3]) then begin
    CellText := Results.Col(1);

    // Detect value type
    try
      ValIsNumber := IntToStr(MakeInt(CellText)) = CellText;
    except
      ValIsNumber := False;
    end;
    ValIsBytes := ValIsNumber and (Copy(Results.Col(0), 1, 6) = 'Bytes_');

    // Calculate average values ...
    case Column of
      1: begin // Format numeric or byte values
        if ValIsBytes then
          CellText := FormatByteNumber(CellText)
        else if ValIsNumber then
          CellText := FormatNumber(CellText);
      end;
      2,3: begin // ... per hour/second
        if ValIsNumber then begin
          ValCount := MakeInt(CellText);
          tmpval := ValCount / (FStatusServerUptime / 60 / 60);
          if Column = 3 then
            tmpval := tmpval / 60 / 60;
          if ValIsBytes then
            CellText := FormatByteNumber(Trunc(tmpval))
          else if ValIsNumber then
            CellText := FormatNumber(tmpval, 1);
        end else
          CellText := '';
      end;
    end;

  end else if Sender = ListCommandStats then begin
    CommandCount := MakeInt(Results.Col(1));
    case Column of
      0: begin // Strip "Com_"
        CellText := Results.Col(Column);
        CellText := Copy(CellText, 5, Length(CellText));
        CellText := StringReplace(CellText, '_', ' ', [rfReplaceAll] );
      end;
      1: begin // Total Frequency
        CellText := FormatNumber(CommandCount);
      end;
      2: begin // Average per hour
        tmpval := CommandCount / (FCommandStatsServerUptime / 60 / 60);
        CellText := FormatNumber(tmpval, 1);
      end;
      3: begin // Average per second
        tmpval := CommandCount / FCommandStatsServerUptime;
        CellText := FormatNumber(tmpval, 1);
      end;
      4: begin // Percentage. Take care of division by zero errors and Int64's
        tmpval := 100 / Max(FCommandStatsQueryCount, 1) * Max(CommandCount, 1);
        CellText := FormatNumber(tmpval, 1) + ' %';
      end;
    end;

  end else if Sender = ListVariables then begin
    try
      case Column of
        0: CellText := FVariableNames[Idx^];
        1: CellText := FSessionVars.Values[FVariableNames[Idx^]];
        2: CellText := FGlobalVars.Values[FVariableNames[Idx^]];
      end;
    except
      CellText := '';
    end;

  end else begin
    // Values directly from a query result
    CellText := sstr(Results.Col(Column), SIZE_KB*50);
  end;
end;


{**
  Edit a server variable
}
procedure TMainForm.menuEditVariableClick(Sender: TObject);
var
  Dialog: TfrmEditVariable;
begin
  Dialog := TfrmEditVariable.Create(Self);
  try
    try
      Dialog.VarName := ListVariables.Text[ListVariables.FocusedNode, 0];
      Dialog.VarValue := ListVariables.Text[ListVariables.FocusedNode, 1];
      // Refresh list node
      if Dialog.ShowModal = mrOK then
        InvalidateVT(ListVariables, VTREE_NOTLOADED, False);
    except
      on E:EVariableError do
        ErrorDialog(E.Message);
    end;
  finally
    Dialog.Free;
  end;
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
        lntNone: CellText := DBObj.Connection.Parameters.SessionPath;
        lntDb: CellText := DBObj.Database;
        lntGroup: begin
          CellText := DBObj.Name;
          if Sender.ChildrenInitialized[Node] then
            CellText := CellText + ' (' + FormatNumber(Sender.ChildCount[Node]) + ')';
        end;
        lntTable..lntEvent: begin
          if (DBObj.Schema <> '') and (DBObj.Connection.Parameters.NetTypeGroup = ngMSSQL) then
            CellText := DBObj.Schema + '.' + DBObj.Name
          else
            CellText := DBObj.Name;
        end;
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
        lntTable: begin
            if DBObj.Size >= 0 then
              CellText := FormatByteNumber(DBObj.Size)
            else
              CellText := '';
          end
        else CellText := ''; // Applies for views/procs/... which have no size
      end;
  end;
end;


{**
  Set icon of a treenode before it gets displayed
}
procedure TMainForm.DBtreeGetImageIndex(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
    Boolean; var ImageIndex: TImageIndex);
var
  DBObj: PDBObject;
begin
  if Column > 0 then
    Exit;
  DBObj := Sender.GetNodeData(Node);
  case Kind of
    ikNormal, ikSelected: begin
        ImageIndex := DBObj.ImageIndex;
        Ghosted := (DBObj.NodeType = lntNone) and (not DBObj.Connection.Active);
        Ghosted := Ghosted or ((DBObj.NodeType = lntDB)
          and (not DBObj.Connection.DbObjectsCached(DBObj.Database))
          );
        Ghosted := Ghosted or ((DBObj.NodeType = lntGroup)
          and Sender.ChildrenInitialized[Node]
          and (Sender.ChildCount[Node] = 0)
          );
        Ghosted := Ghosted or ((DBObj.NodeType in [lntTable..lntEvent])
          and (not DBObj.WasSelected)
          );
      end;
    ikOverlay:
      ImageIndex := DBObj.OverlayImageIndex;
  end;
end;


{**
  Set childcount of an expanding treenode
}
procedure TMainForm.DBtreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  DBObj: PDBObject;
  Columns: TTableColumnList;
  DBObjects: TDBObjectList;
begin
  DBObj := Sender.GetNodeData(Node);
  case DBObj.NodeType of
    // Session node expanding
    lntNone: begin
        Screen.Cursor := crHourglass;
        ShowStatusMsg(_('Reading Databases...'));
        if Sender.Tag = VTREE_NOTLOADED_PURGECACHE then
          DBObj.Connection.RefreshAllDatabases;
        ShowStatusMsg;
        Sender.Tag := VTREE_LOADED;
        InvalidateVT(ListDatabases, VTREE_NOTLOADED, True);
        ChildCount := DBObj.Connection.AllDatabases.Count;
        Screen.Cursor := crDefault;
      end;
    // DB node expanding
    lntDb: begin
        if actGroupObjects.Checked then begin
          // Just tables, views, etc.
          ChildCount := 6;
        end else begin
          ShowStatusMsg(_('Reading objects ...'));
          Screen.Cursor := crHourglass;
          try
            ChildCount := DBObj.Connection.GetDBObjects(DBObj.Connection.AllDatabases[Node.Index]).Count;
          finally
            ShowStatusMsg;
            Screen.Cursor := crDefault;
          end;
        end;
      end;
    lntGroup: begin
        ChildCount := 0;
        DBObjects := DBObj.Connection.GetDBObjects(DBObj.Database, False, DBObj.GroupType);
        ChildCount := DBObjects.Count;
      end;
    lntTable:
      if GetParentFormOrFrame(Sender) is TfrmSelectDBObject then begin
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
  Item, ParentObj: PDBObject;
  DBObjects: TDBObjectList;
  Columns: TTableColumnList;
begin
  Item := Sender.GetNodeData(Node);
  if not Assigned(ParentNode) then begin
    Item^ := TDBObject.Create(FConnections[Node.Index]);
    // Ensure plus sign is visible for root (and dbs, see below)
    Include(InitialStates, ivsHasChildren);
  end else begin
    ParentObj := Sender.GetNodeData(ParentNode);
    case ParentObj.NodeType of
      lntNone: begin
        Item^ := TDBObject.Create(ParentObj.Connection);
        Item.NodeType := lntDb;
        Item.Database := Item.Connection.AllDatabases[Node.Index];
        Include(InitialStates, ivsHasChildren);
      end;
      lntDb: begin
        if actGroupObjects.Checked then begin
          Item^ := TDBObject.Create(ParentObj.Connection);
          Item.NodeType := lntGroup;
          case Node.Index of
            0: begin Item.GroupType := lntTable; Item.Name := _('Tables'); end;
            1: begin Item.GroupType := lntView; Item.Name := _('Views'); end;
            2: begin Item.GroupType := lntProcedure; Item.Name := _('Procedures'); end;
            3: begin Item.GroupType := lntFunction; Item.Name := _('Functions'); end;
            4: begin Item.GroupType := lntTrigger; Item.Name := _('Triggers'); end;
            5: begin Item.GroupType := lntEvent; Item.Name := _('Events'); end;
          end;
          Item.Database := ParentObj.Database;
          InitialStates := InitialStates + [ivsHasChildren];
        end else begin
          DBObjects := ParentObj.Connection.GetDBObjects(ParentObj.Database);
          Item^ := DBObjects[Node.Index];
          if (GetParentFormOrFrame(Sender) is TfrmSelectDBObject) and (Item.NodeType = lntTable) then
            Include(InitialStates, ivsHasChildren);
        end;
      end;
      lntGroup: begin
        DBObjects := ParentObj.Connection.GetDBObjects(ParentObj.Database, False, ParentObj.GroupType);
        Item^ := DBObjects[Node.Index];
        if (GetParentFormOrFrame(Sender) is TfrmSelectDBObject) and (Item.NodeType = lntTable) then
          Include(InitialStates, ivsHasChildren);
      end;
      lntTable: begin
        Item^ := TDBObject.Create(ParentObj.Connection);
        Item.NodeType := lntColumn;
        Columns := TTableColumnList.Create(True);
        ParentObj.Connection.ParseTableStructure(ParentObj.CreateCode, Columns, nil, nil);
        Item.Database := ParentObj.Database;
        Item.Name := ParentObj.Name;
        Item.Column := Columns[Node.Index].Name;
      end;
    end;
  end;
end;


{**
  Selection in database tree has changed
}
procedure TMainForm.DBtreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  DBObj, PrevDBObj, ParentDBObj: PDBObject;
  MainTabToActivate: TTabSheet;
  DummyStr: String;
begin
  // Set wanted main tab and call SetMainTab later, when all lists have been invalidated
  MainTabToActivate := nil;
  PrevDBObj := nil;
  ParentDBObj := nil;

  if Assigned(Node) then begin
    LogSQL('DBtreeFocusChanged, Node level: '+IntToStr(Sender.GetNodeLevel(Node))+', FTreeRefreshInProgress: '+IntToStr(Integer(FTreeRefreshInProgress)), lcDebug);

    // Post pending UPDATE
    if (DataGridResult<>nil) and DataGridResult.Modified then
      actDataPostChangesExecute(DataGrid);

    DBObj := Sender.GetNodeData(Node);
    DBObj.WasSelected := True;
    FActiveDbObj := TDBObject.Create(DBObj.Connection);
    FActiveDbObj.Assign(DBObj^);
    if Assigned(Node.Parent) then
      ParentDBObj := Sender.GetNodeData(Node.Parent);

    case FActiveDbObj.NodeType of
      lntNone: begin
        if (not DBtree.Dragging) and (not QueryTabActive) then
          MainTabToActivate := tabHost;
        FActiveDbObj.Connection.Database := '';
      end;
      lntDb, lntGroup: begin
        // Selecting a database can cause an SQL error if the db was deleted from outside. Select previous node in that case.
        try
          FActiveDbObj.Connection.Database := FActiveDbObj.Database;
        except on E:EDbError do begin
            ErrorDialog(E.Message);
            SelectNode(DBtree, TreeClickHistoryPrevious);
            Exit;
          end;
        end;
        if (not DBtree.Dragging) and (not QueryTabActive) then
          MainTabToActivate := tabDatabase;
        FActiveObjectGroup := FActiveDbObj.GroupType;
      end;
      lntTable..lntEvent: begin
        try
          FActiveDbObj.Connection.Database := FActiveDbObj.Database;
        except on E:EDbError do begin
            ErrorDialog(E.Message);
            SelectNode(DBtree, TreeClickHistoryPrevious);
            Exit;
          end;
        end;

        // Retrieve columns of current table or view. Mainly used in datagrid.
        SelectedTableColumns.Clear;
        SelectedTableKeys.Clear;
        SelectedTableForeignKeys.Clear;
        AppSettings.SessionPath := GetRegKeyTable;
        SelectedTableTimestampColumns.Text := AppSettings.ReadString(asTimestampColumns);
        InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
        try
          case FActiveDbObj.NodeType of
            lntTable:
              FActiveDbObj.Connection.ParseTableStructure(FActiveDbObj.CreateCode, SelectedTableColumns, SelectedTableKeys, SelectedTableForeignKeys);
            lntView:
              FActiveDbObj.Connection.ParseViewStructure(FActiveDbObj.CreateCode, FActiveDbObj, SelectedTableColumns, DummyStr, DummyStr, DummyStr, DummyStr, DummyStr);
          end;
        except on E:EDbError do
          ErrorDialog(E.Message);
        end;

        if not FTreeRefreshInProgress then
          PlaceObjectEditor(FActiveDbObj);
        // When a table is clicked in the tree, and the current
        // tab is a Host or Database tab, switch to showing table columns.
        if (PagecontrolMain.ActivePage = tabHost) or (PagecontrolMain.ActivePage = tabDatabase) then
          MainTabToActivate := tabEditor;
        if DataGrid.Tag = VTREE_LOADED then
          InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);
        // Update the list of columns
        RefreshHelperNode(HELPERNODE_COLUMNS);
        FActiveObjectGroup := ParentDBObj.GroupType;
      end;
    end;

    if TreeClickHistoryPrevious(True) <> nil then
      PrevDBObj := Sender.GetNodeData(TreeClickHistoryPrevious(True));

    // When clicked node is from a different connection than before, do session specific stuff here:
    if (PrevDBObj = nil) or (PrevDBObj.Connection <> FActiveDbObj.Connection) then begin
      LogSQL(f_('Entering session "%s"', [FActiveDbObj.Connection.Parameters.SessionPath]), lcInfo);
      RefreshHelperNode(HELPERNODE_HISTORY);
      case FActiveDbObj.Connection.Parameters.NetTypeGroup of
        ngMySQL:
          SynSQLSynUsed.SQLDialect := sqlMySQL;
        ngMSSQL:
          SynSQLSynUsed.SQLDialect := sqlMSSQL2K;
        ngPgSQL:
          SynSQLSynUsed.SQLDialect := sqlPostgres;
        else
          raise Exception.CreateFmt(_(MsgUnhandledNetType), [Integer(FActiveDbObj.Connection.Parameters.NetType)]);
      end;
    end;
    if (FActiveDbObj.NodeType <> lntNone)
      and (
        (PrevDBObj = nil)
        or (PrevDBObj.Connection <> FActiveDbObj.Connection)
        or (PrevDBObj.Database <> FActiveDbObj.Database)
        or (PrevDBObj.GroupType <> FActiveObjectGroup)
      ) then
      InvalidateVT(ListTables, VTREE_NOTLOADED, True);
    if FActiveDbObj.NodeType = lntGroup then
      InvalidateVT(ListTables, VTREE_NOTLOADED, True);
    tabHost.Caption := _('Host')+': '+sstr(FActiveDbObj.Connection.Parameters.HostName, 20);
    tabDatabase.Caption := _('Database')+': '+sstr(FActiveDbObj.Connection.Database, 20);
    ShowStatusMsg(FActiveDbObj.Connection.Parameters.NetTypeName(FActiveDbObj.Connection.Parameters.NetType, False)+' '+FActiveDbObj.Connection.ServerVersionStr, 3);
  end else begin
    LogSQL('DBtreeFocusChanged without node.', lcDebug);
    FreeAndNil(FActiveDbObj);
    tabHost.Caption := _('Host');
    tabDatabase.Caption := _('Database');
    // Clear server version panel
    ShowStatusMsg('', 3);
  end;

  if (FActiveDbObj = nil) or (PrevDBObj = nil) or (PrevDBObj.Connection <> FActiveDbObj.Connection) then begin
    TimerConnected.OnTimer(Sender);
    TimerHostUptime.OnTimer(Sender);
    InvalidateVT(ListDatabases, VTREE_NOTLOADED, False);
    InvalidateVT(ListVariables, VTREE_NOTLOADED, False);
    InvalidateVT(ListStatus, VTREE_NOTLOADED, False);
    InvalidateVT(ListProcesses, VTREE_NOTLOADED, False);
    InvalidateVT(ListCommandstats, VTREE_NOTLOADED, False);
    InvalidateVT(ListTables, VTREE_NOTLOADED, False);
  end;

  // Make wanted tab visible before activating, to avoid unset tab on Wine
  if Assigned(MainTabToActivate) then
    MainTabToActivate.TabVisible := True;
  if not FTreeRefreshInProgress then begin
    SetMainTab(MainTabToActivate);
    tabDatabase.TabVisible := (FActiveDbObj <> nil) and (FActiveDbObj.NodeType <> lntNone);
    tabEditor.TabVisible := (FActiveDbObj <> nil) and (FActiveDbObj.NodeType in [lntTable..lntEvent]);
    tabData.TabVisible := (FActiveDbObj <> nil) and (FActiveDbObj.NodeType in [lntTable, lntView]);
    actSQLhelp.Enabled := Assigned(Node);
  end;

  // Store click history item
  SetLength(FTreeClickHistory, Length(FTreeClickHistory)+1);
  FTreeClickHistory[Length(FTreeClickHistory)-1] := Node;

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
    Allowed := NewNode <> OldNode;
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


procedure TMainForm.ConnectionReady(Connection: TDBConnection; Database: String);
begin
  // Manually trigger changed focused tree node, to display the right server vendor
  // and version. Also required on reconnects.
  DBtree.OnFocusChanged(DBtree, DBtree.FocusedNode, DBtree.FocusedColumn);
end;


procedure TMainForm.DatabaseChanged(Connection: TDBConnection; Database: String);
begin
  // Immediately force db icons to repaint, so the user sees the active db state
  DBtree.Repaint;

  // Clear Filter issue 3466
  FFilterTextDatabase := '';

  if ActiveQueryHelpers <> nil then
    ActiveQueryHelpers.Invalidate;
end;


procedure TMainForm.ObjectnamesChanged(Connection: TDBConnection; Database: String);
var
  DBObjects: TDBObjectList;
  Obj: TDBObject;
  TableNames, ProcNames: TStringList;
begin
  // Tell SQL highlighter about names of tables and procedures in selected database
  SynSQLSynUsed.TableNames.Clear;
  SynSQLSynUsed.ProcNames.Clear;
  if Connection.DbObjectsCached(Database) then begin
    DBObjects := Connection.GetDBObjects(Database);
    TableNames := TStringList.Create;
    TableNames.BeginUpdate;
    ProcNames := TStringList.Create;
    ProcNames.BeginUpdate;
    for Obj in DBObjects do begin
      case Obj.NodeType of
        lntTable, lntView: begin
          // Slow highlighter enhanced by uschuster. See http://www.heidisql.com/forum.php?t=16307
          // ... and here: https://github.com/SynEdit/SynEdit/issues/28
          TableNames.Add(Obj.Name);
        end;
        lntProcedure, lntFunction: begin
          ProcNames.Add(Obj.Name);
        end;
      end;
    end;
    TableNames.EndUpdate;
    ProcNames.EndUpdate;
    SynSQLSynUsed.TableNames.Text := TableNames.Text;
    SynSQLSynUsed.ProcNames.Text := ProcNames.Text;
    TableNames.Free;
    ProcNames.Free;
  end;
end;


procedure TMainForm.DBtreeDblClick(Sender: TObject);
var
  DBObj: PDBObject;
  m: TSynMemo;
begin
  // Paste DB or table name into query window on treeview double click.
  if AppSettings.ReadBool(asDoubleClickInsertsNodeText) and QueryTabActive and Assigned(DBtree.FocusedNode) then begin
    DBObj := DBtree.GetNodeData(DBtree.FocusedNode);
    if DBObj.NodeType in [lntDb, lntTable..lntEvent] then begin
      m := ActiveQueryMemo;
      m.DragDrop(Sender, m.CaretX, m.CaretY);
    end;
  end;
end;


procedure TMainForm.DBtreeExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // Table and database filter both need initialized children
  Sender.ReinitChildren(Node, False);
  editDatabaseTableFilterChange(Self);
end;


procedure TMainForm.DBtreeExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var
  DBObj: PDBObject;
  GNode: PVirtualNode;
begin
  // Auto-init children of sibling groups
  DBObj := Sender.GetNodeData(Node);
  if DBObj.NodeType = lntGroup then begin
    GNode := Sender.GetFirstChild(Node.Parent);
    while Assigned(GNode) do begin
      if not Sender.ChildrenInitialized[GNode] then begin
        Sender.ReinitChildren(GNode, False);
      end;
      GNode := Sender.GetNextSibling(GNode);
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
    TargetCanvas.Font.Color := clGrayText
  else if (Column = 1) and (DBObj.NodeType in [lntTable..lntEvent]) then
    TargetCanvas.Font.Color := clGrayText;
end;


{**
  Refresh the whole tree
}
procedure TMainForm.RefreshTree(FocusNewObject: TDBObject=nil);
var
  DBNode: PVirtualNode;
  OnlyDBNode, Expanded: Boolean;
  SessNode: PVirtualNode;
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
  SelectNode(DBtree, nil);
  try
    if not OnlyDBNode then begin
      FocusNewObject.Connection.ClearAllDbObjects;
      FocusNewObject.Connection.RefreshAllDatabases;
      SessNode := GetRootNode(DBtree, FocusNewObject.Connection);
      if Assigned(SessNode) then begin
        Expanded := DBtree.Expanded[SessNode];
        DBtree.ResetNode(SessNode);
        DBtree.Expanded[SessNode] := Expanded;
      end;
    end else begin
      FocusNewObject.Connection.ClearDbObjects(FocusNewObject.Database);
      DBNode := FindDbNode(DBtree, FocusNewObject.Connection, FocusNewObject.Database);
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
      raise Exception.Create(_('Could not find node to focus.'));

  finally
    FTreeRefreshInProgress := False;
    // Tree node filtering needs a hit in special cases, e.g. after a db was dropped
    if editDatabaseFilter.Text <> '' then
      editDatabaseFilter.OnChange(editDatabaseFilter);
    if editTableFilter.Text <> '' then
      editTableFilter.OnChange(editTableFilter);
    if editFilterVT.Text <> '' then
      ApplyVTFilter(False);
  end;
end;


{**
  Find a database node in the tree by passing its name
}
function TMainForm.FindDBNode(Tree: TBaseVirtualTree; Connection: TDBConnection; db: String): PVirtualNode;
var
  DBObj: PDBObject;
  n, DBNode: PVirtualNode;
begin
  Result := nil;
  n := GetRootNode(Tree, Connection);
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
  Clause, Line, Condition: String;
  Conditions: TStringList;
  i: Integer;
  ed: TEdit;
  Conn: TDBConnection;
  rx: TRegExpr;
begin
  ed := TEdit(Sender);
  Clause := '';
  if ed.Text <> '' then begin

    Conn := ActiveConnection;
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    Conditions := TStringList.Create;
    for i:=0 to SelectedTableColumns.Count-1 do begin
      // The normal case: do a LIKE comparison
      Condition := SelectedTableColumns[i].CastAsText + ' LIKE ''%' + Conn.EscapeString(ed.Text, True, False)+'%''';
      if not SelectedTableColumns[i].DataType.ValueMustMatch.IsEmpty then begin
        // Use an exact comparison for some PostgreSQL data types to overcome SQL errors, e.g. UUID, INT etc.
        // Also, prevent other errors by matching the value against a certain regular expression.
        // If it does not match, leave this column away.
        // See http://www.heidisql.com/forum.php?t=20953
        rx.Expression := SelectedTableColumns[i].DataType.ValueMustMatch;
        if rx.Exec(ed.Text) then
          Condition := Conn.QuoteIdent(SelectedTableColumns[i].Name) + '=' + Conn.EscapeString(ed.Text)
        else
          Condition := '';
      end;
      if not Condition.IsEmpty then
        Conditions.Add(Condition);
    end;
    rx.Free;

    Line := '';
    for i:=0 to Conditions.Count-1 do begin
      if i > 0 then
        Line := Line + ' OR ';
      Line := Line + Conditions[i];
      // Add linebreak near right window edge
      if (Length(Line) > SynMemoFilter.CharsInWindow-30) or (i = Conditions.Count-1) then begin
        Clause := Clause + Line + CRLF;
        Line := '';
      end;
    end;
    Clause := Clause + Conn.LikeClauseTail;

  end;
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
  RowNumber: PInt64;
  Results: TDBQuery;
  Timestamp: Int64;
begin
  if Column = -1 then
    Exit;
  EditingAndFocused := Sender.IsEditing and (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn);
  Results := GridResult(Sender);
  // Happens in some crashes, see issue #2462
  if Column >= Results.ColumnCount then
    Exit;
  RowNumber := Sender.GetNodeData(Node);
  Results.RecNo := RowNumber^;
  if Results.IsNull(Column) and (not EditingAndFocused) then
    CellText := TEXT_NULL
  else begin
    case Results.DataType(Column).Category of
      dtcInteger, dtcReal: begin
        // This is a bit crappy...
        // UNIX timestamps get *copied* as integers, but *displayed* and *edited* as date/time values.
        // Normal integers are *copied* and *edited* as raw numbers, but probably *displayed* as formatted numbers.
        if FGridCopying then begin
          CellText := Results.Col(Column);
        end else if HandleUnixTimestampColumn(Sender, Column) then begin
          try
            Timestamp := Trunc(StrToFloat(Results.Col(Column), FFormatSettings));
          except
            // EConvertError in StrToFloat or EInvalidOp in Trunc or...
            Timestamp := 0;
          end;
          Dec(Timestamp, FTimeZoneOffset);
          CellText := DateTimeToStr(UnixToDateTime(Timestamp));
        end else begin
          if DataLocalNumberFormat and (not EditingAndFocused) then
            CellText := FormatNumber(Results.Col(Column), True)
          else
            CellText := Results.Col(Column);
        end;
      end;
      dtcBinary, dtcSpatial: begin
        if actBlobAsText.Checked then
          CellText := Results.Col(Column)
        else
          CellText := Results.HexValue(Column);
      end;
      else begin
        CellText := Results.Col(Column);
        if (Length(CellText) = GRIDMAXDATA) and (not Results.HasFullData) and (Sender = DataGrid) then
          CellText := CellText + ' [...]';
      end;
    end;
  end;
end;


procedure TMainForm.CalcNullColors;
var
  dtc: TDBDatatypeCategoryIndex;
  h, l, s: Word;
begin
  for dtc:=Low(DatatypeCategories) to High(DatatypeCategories) do begin
    ColorRGBToHLS(DatatypeCategories[dtc].Color, h, l, s);
    Inc(l, COLORSHIFT_NULLFIELDS);
    s := Max(0, s-2*COLORSHIFT_NULLFIELDS);
    DatatypeCategories[dtc].NullColor := ColorHLSToRGB(h, l, s);
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
  r: TDBQuery;
  RowNumber: PInt64;
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
    cl := GetThemeColor(clHighlightText)
  else if r.IsNull(Column) then
    cl := DatatypeCategories[r.DataType(Column).Category].NullColor
  else
    cl := DatatypeCategories[r.DataType(Column).Category].Color;
  TargetCanvas.Font.Color := cl;
end;


procedure TMainForm.AnyGridAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
  Results: TDBQuery;
  RowNum: PInt64;
begin
  // Don't waist time
  if Column = NoColumn then Exit;
  // Paint a red triangle at the top left corner of the cell
  Results := GridResult(Sender);
  RowNum := Sender.GetNodeData(Node);
  Results.RecNo := RowNum^;
  if Results.Modified(Column) then
    VirtualImageListMain.Draw(TargetCanvas, CellRect.Left, CellRect.Top, 111);
end;


{**
  Header column in datagrid clicked.
  Left button: handle ORDER BY
  Right button: show column selection box
}
procedure TMainForm.DataGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  frm: TForm;
  i, j: Integer;
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
    // Refresh grid, and remember X scroll offset, so the just clicked column is still at the same place.
    FDataGridLastClickedColumnHeader := HitInfo.Column;
    FDataGridLastClickedColumnLeftPos := Sender.Columns[HitInfo.Column].Left;
    InvalidateVT(DataGrid, VTREE_NOTLOADED_PURGECACHE, False);

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
  RowNum: PInt64;
  Grid: TVirtualStringTree;
  Results: TDBQuery;
begin
  // Set cell to NULL value
  Grid := ActiveGrid;
  RowNum := Grid.GetNodeData(Grid.FocusedNode);
  Results := GridResult(Grid);
  Results.RecNo := RowNum^;
  try
    Results.SetCol(Grid.FocusedColumn, '', True, False);
  except
    on E:EDbError do
      ErrorDialog(E.Message);
  end;
  Grid.RepaintNode(Grid.FocusedNode);
end;


procedure TMainForm.AnyGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
var
  VT: TVirtualStringTree;
  Node: PVirtualNode;
  NewFontSize: Integer;
begin
  VT := Sender as TVirtualStringTree;
  if ssAlt in Shift then begin
    // Advance to next or previous grid node on Shift+MouseWheel
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
  end else if KeyPressed(VK_CONTROL) then begin
    // Change font size with MouseWheel
    if AppSettings.ReadBool(asWheelZoom) then begin
      NewFontSize := VT.Font.Size;
      if WheelDelta > 0 then
        Inc(NewFontSize)
      else
        Dec(NewFontSize);
      NewFontSize := Max(NewFontSize, 1);
      AppSettings.ResetPath;
      AppSettings.WriteInt(asDataFontSize, NewFontSize);
      ApplyFontToGrids;
    end;
  end else if ssShift in Shift then begin
    // Horizontal scrolling with Alt+Mousewheel
    VT.OffsetX := VT.OffsetX + WheelDelta;
    Handled := True;
  end;
end;


{**
  Content of a grid cell was modified
}
procedure TMainForm.AnyGridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Results: TDBQuery;
  RowNum: PInt64;
  Timestamp: Int64;
  IsNull: Boolean;
begin
  Results := GridResult(Sender);
  RowNum := Sender.GetNodeData(Node);
  Results.RecNo := RowNum^;
  try
    if (not FGridEditFunctionMode) and (Results.DataType(Column).Category in [dtcInteger, dtcReal]) then begin
      if HandleUnixTimestampColumn(Sender, Column) then begin
        Timestamp := DateTimeToUnix(StrToDateTime(NewText));
        Inc(Timestamp, FTimeZoneOffset);
        NewText := IntToStr(Timestamp)
      end else
        NewText := NewText;
    end;
    IsNull := FGridPasting and FClipboardHasNull;
    Results.SetCol(Column, NewText, IsNull, FGridEditFunctionMode);
  except
    on E:EDbError do
      ErrorDialog(E.Message);
  end;
  FGridEditFunctionMode := False;
  ValidateControls(Sender);
end;


{**
  DataGrid: node and/or column focus is about to change. See if we allow that.
}
procedure TMainForm.AnyGridFocusChanging(Sender: TBaseVirtualTree; OldNode,
    NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed:
    Boolean);
var
  Results: TDBQuery;
  RowNum: PInt64;
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
  if Assigned(Node) and pnlPreview.Visible then
    UpdatePreviewPanel;
  // Vtree does not focus cell after tab pressing. See issue #3139.
  // Most probably a missing thing / bug in TBaseVirtualTree.SetFocusedNodeAndColumn
  Sender.ScrollIntoView(Sender.FocusedNode, False, True);
  // Required for highlighting fields with same text
  Sender.Invalidate;
end;


procedure TMainForm.AnyGridChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // Ensure "delete row" button state is valid, see issue #624
  ValidateControls(Sender);
  UpdateLineCharPanel;
end;


procedure TMainForm.AnyGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  g: TVirtualStringTree;
begin
  g := TVirtualStringTree(Sender);
  case Key of
    VK_HOME: begin
      g.FocusedColumn := g.Header.Columns.GetFirstVisibleColumn(False);
      if ssCtrl in Shift then begin
        // VT itself focuses the first node since v7.0
      end else
        Key := 0;
    end;
    VK_END: begin
      g.FocusedColumn := g.Header.Columns.GetLastVisibleColumn(False);
      if ssCtrl in Shift then begin
        if g = DataGrid then
          actDataShowAll.Execute;
        // VT itself focuses the last node since v7.0
      end else begin
        Key := 0;
      end;
    end;
    VK_RETURN: if Assigned(g.FocusedNode) then g.EditNode(g.FocusedNode, g.FocusedColumn);
    VK_DOWN: if g.FocusedNode = g.GetLast then actDataInsertExecute(actDataInsert);
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
      ErrorDialog(_('Could not load full row data.'))
    else begin
      Allowed := True;
      // Move Esc shortcut from "Cancel row editing" to "Cancel cell editing"
      actDataCancelChanges.ShortCut := 0;
      actDataPostChanges.ShortCut := 0;
    end;
  except on E:EDbError do
    ErrorDialog(_('Grid editing error'), E.Message);
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
  TypeCat: TDBDatatypeCategoryIndex;
  ForeignKey: TForeignKey;
  TblColumn: TTableColumn;
  idx, MicroSecondsPrecision: Integer;
  KeyCol, TextCol, SQL, CreateTable, NowText: String;
  Columns: TTableColumnList;
  Keys: TTableKeyList;
  ForeignKeys: TForeignKeyList;
  ForeignResults, Results: TDBQuery;
  Conn: TDBConnection;
  RowNum: PInt64;
begin
  VT := Sender as TVirtualStringTree;
  Results := GridResult(VT);
  RowNum := VT.GetNodeData(Node);
  Results.RecNo := RowNum^;
  Conn := Results.Connection;

  // Find foreign key values on InnoDB table cells
  if AppSettings.ReadBool(asForeignDropDown) and (Sender = DataGrid) then begin
    for ForeignKey in SelectedTableForeignKeys do begin
      idx := ForeignKey.Columns.IndexOf(DataGrid.Header.Columns[Column].Text);
      if idx > -1 then try
        // Find the first text column if available and use that for displaying in the pulldown instead of using meaningless id numbers
        CreateTable := Conn.GetVar('SHOW CREATE TABLE '+Conn.QuoteIdent(ForeignKey.ReferenceTable, True, '.'), 1);
        Columns := TTableColumnList.Create;
        Keys := nil;
        ForeignKeys := nil;
        Conn.ParseTableStructure(CreateTable, Columns, Keys, ForeignKeys);
        TextCol := '';
        for TblColumn in Columns do begin
          if (TblColumn.DataType.Category = dtcText) and (TblColumn.Name <> ForeignKey.ForeignColumns[idx]) then begin
            TextCol := TblColumn.Name;
            break;
          end;
        end;

        KeyCol := Conn.QuoteIdent(ForeignKey.ForeignColumns[idx]);
        SQL := 'SELECT '+KeyCol;
        if TextCol <> '' then SQL := SQL + ', LEFT(' + Conn.QuoteIdent(TextCol) + ', 256)';
        SQL := SQL + ' FROM '+Conn.QuoteIdent(ForeignKey.ReferenceTable, True, '.')+' GROUP BY '+KeyCol+' ORDER BY ';
        if TextCol <> '' then SQL := SQL + Conn.QuoteIdent(TextCol) else SQL := SQL + KeyCol;
        SQL := SQL + ' LIMIT 1000';

        ForeignResults := Conn.GetResults(SQL);
        if ForeignResults.RecordCount < 1000 then begin
          EnumEditor := TEnumEditorLink.Create(VT);
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
      except on E:EDbError do
        // Error gets logged, do nothing more here. All other exception types raise please.
      end;
    end;
  end;

  FGridEditFunctionMode := FGridEditFunctionMode or Results.IsFunction(Column);
  if FGridEditFunctionMode then begin
    EnumEditor := TEnumEditorLink.Create(VT);
    for idx:=Low(MySQLFunctions) to High(MySQLFunctions) do
      EnumEditor.ValueList.Add(MySQLFunctions[idx].Name + MySQLFunctions[idx].Declaration);
    EnumEditor.AllowCustomText := True;
    EditLink := EnumEditor;
  end;

  TypeCat := Results.DataType(Column).Category;
  if Assigned(EditLink) then
    // Editor was created above, do nothing now
  else if (Results.DataType(Column).Index in [dtEnum, dtBool]) and AppSettings.ReadBool(asFieldEditorEnum) then begin
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.ValueList := Results.ValueList(Column);
    EditLink := EnumEditor;
  end else if (TypeCat = dtcText) or ((TypeCat in [dtcBinary, dtcSpatial]) and actBlobAsText.Checked) then begin
    InplaceEditor := TInplaceEditorLink.Create(VT);
    InplaceEditor.MaxLength := Results.MaxLength(Column);
    InplaceEditor.TitleText := Results.ColumnOrgNames[Column];
    InplaceEditor.ButtonVisible := True;
    EditLink := InplaceEditor;
  end else if (TypeCat in [dtcBinary, dtcSpatial]) and AppSettings.ReadBool(asFieldEditorBinary) then begin
    HexEditor := THexEditorLink.Create(VT);
    HexEditor.MaxLength := Results.MaxLength(Column);
    HexEditor.TitleText := Results.ColumnOrgNames[Column];
    EditLink := HexEditor;
  end else if (TypeCat = dtcTemporal) and AppSettings.ReadBool(asFieldEditorDatetime) then begin
    // Ensure date/time editor starts with a non-empty text value
    if (Results.Col(Column) = '') and AppSettings.ReadBool(asFieldEditorDatetimePrefill) then begin
      case Results.DataType(Column).Index of
        dtDate: NowText := DateToStr(Now);
        dtTime: NowText := TimeToStr(Now);
        // Add this case to prevent error with datatype year and sql_mode STRICT_TRANS_TABLES
        // who absolutly want year and not date time
        // http://www.heidisql.com/forum.php?t=14728
        dtYear: NowText := FormatDateTime('yyyy',Now);
        else NowText := DateTimeToStr(Now);
      end;
      MicroSecondsPrecision := MakeInt(Results.ColAttributes(Column).LengthSet);
      // Don't generate MicroSecond when DataType is Year
      if (MicroSecondsPrecision > 0) and (Results.DataType(Column).Index <> dtYear ) then
        NowText := NowText + '.' + StringOfChar('0', MicroSecondsPrecision);
      VT.Text[Node, Column] := NowText;
    end;
    DateTimeEditor := TDateTimeEditorLink.Create(VT);
    EditLink := DateTimeEditor;
  end else if AppSettings.ReadBool(asFieldEditorDatetime) and HandleUnixTimestampColumn(Sender, Column) then begin
    DateTimeEditor := TDateTimeEditorLink.Create(VT);
    EditLink := DateTimeEditor;
  end else if (Results.DataType(Column).Index = dtSet) and AppSettings.ReadBool(asFieldEditorSet) then begin
    SetEditor := TSetEditorLink.Create(VT);
    SetEditor.ValueList := Results.ValueList(Column);
    EditLink := SetEditor;
  end else begin
    InplaceEditor := TInplaceEditorLink.Create(VT);
    InplaceEditor.ButtonVisible := False;
    EditLink := InplaceEditor;
  end;
  TBaseGridEditorLink(EditLink).TableColumn := Results.ColAttributes(Column);
end;


procedure TMainForm.menuShowSizeColumnClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  if coVisible in DBtree.Header.Columns[1].Options then
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options - [coVisible]
  else
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options + [coVisible];
  Item := Sender as TMenuItem;
  Item.Checked := coVisible in DBtree.Header.Columns[1].Options;
  AppSettings.ResetPath;
  AppSettings.WriteBool(asDisplayObjectSizeColumn, Item.Checked);
end;


procedure TMainForm.menuAutoExpandClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  // Activate expand on click tree feature
  if toAutoExpand in DBtree.TreeOptions.AutoOptions then
    DBtree.TreeOptions.AutoOptions := DBtree.TreeOptions.AutoOptions - [toAutoExpand]
  else
    DBtree.TreeOptions.AutoOptions := DBtree.TreeOptions.AutoOptions + [toAutoExpand];
  Item := Sender as TMenuItem;
  Item.Checked := toAutoExpand in DBtree.TreeOptions.AutoOptions;
  AppSettings.ResetPath;
  AppSettings.WriteBool(asAutoExpand, Item.Checked);
end;


procedure TMainForm.actGroupObjectsExecute(Sender: TObject);
begin
  // Group tree objects by type
  RefreshTree(nil);
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
  ColTextWidth := Min(ColTextWidth, AppSettings.ReadInt(asMaxColWidth));
  Col.Width := ColTextWidth;
end;


procedure TMainForm.AnyGridBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  r: TDBQuery;
  cl, clNull, clEven, clOdd, clSameData: TColor;
  RowNumber: PInt64;
  isEven, FocusedIsNull, CurrentIsNull: Boolean;
  FieldText, FocusedFieldText: String;
  VT: TVirtualStringTree;
begin
  if Column = -1 then
    Exit;

  r := GridResult(Sender);
  if not r.Connection.Active then begin
    // This event (BeforeCellPaint) is the very first one to notice a broken connection
    Sender.Enabled := False;
    Exit;
  end;

  VT := Sender as TVirtualStringTree;
  RowNumber := Sender.GetNodeData(Node);
  r.RecNo := RowNumber^;

  clEven := AppSettings.ReadInt(asRowBackgroundEven);
  clOdd := AppSettings.ReadInt(asRowBackgroundOdd);
  isEven := Node.Index mod 2 = 0;
  if IsEven and (clEven <> clNone) then
    cl := AppSettings.ReadInt(asRowBackgroundEven)
  else if (not IsEven) and (clOdd <> clNone) then
    cl := AppSettings.ReadInt(asRowBackgroundOdd)
  else
    cl := clNone;

  if (vsSelected in Node.States) and (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then begin
    // Focused cell
    cl := GetThemeColor(clHighlight)
  end else if vsSelected in Node.States then begin
    // Selected but not focused cell
    if VT.Color > ColorAdjustBrightness(clWhite, -19) then
      cl := ColorAdjustBrightness(VT.Color, -19)
    else
      cl := ColorAdjustBrightness(VT.Color, 19);
  end else if r.IsNull(Column) then begin
    // Cell with NULL value
    clNull := AppSettings.ReadInt(asFieldNullBackground);
    if clNull <> clNone then
      cl := clNull;
  end;
  if cl <> clNone then begin
    TargetCanvas.Brush.Color := cl;
    TargetCanvas.FillRect(CellRect);
  end;

  // Probably display background color on fields with same text
  // Result pointer gets moved to the focused node.. careful!
  if (Sender.FocusedNode <> nil) and (Node <> Sender.FocusedNode) and (Column = Sender.FocusedColumn) then begin
    clSameData := AppSettings.ReadInt(asHightlightSameTextBackground);
    if clSameData <> clNone then begin
      FieldText := r.Col(Column);
      CurrentIsNull := r.IsNull(Column);
      RowNumber := Sender.GetNodeData(Sender.FocusedNode);
      r.RecNo := RowNumber^; // moving result cursor
      FocusedFieldText := r.Col(Column);
      FocusedIsNull := r.IsNull(Column);
      if (CompareText(FieldText, FocusedFieldText) = 0) and (CurrentIsNull = FocusedIsNull) then begin
        TargetCanvas.Brush.Color := clSameData;
        TargetCanvas.FillRect(CellRect);
      end;
    end;
  end;

end;


procedure TMainForm.HandleDataGridAttributes(RefreshingData: Boolean);
var
  rx: TRegExpr;
  idx, i: Integer;
  Sort, KeyName, FocusedCol, CellFocus, Filter: String;
begin
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
  if Assigned(DataGrid.FocusedNode) and (ActiveConnection<>nil) and Assigned(DataGridTable) then begin
    KeyName := DataGridTable.QuotedDatabase+'.'+DataGridTable.QuotedName;
    FocusedCol := '';
    if DataGrid.FocusedColumn > NoColumn then
      FocusedCol := DataGrid.Header.Columns[DataGrid.FocusedColumn].Text;
    DataGridFocusedCell.Values[KeyName] := IntToStr(DataGrid.FocusedNode.Index) + DELIM + FocusedCol;
  end;
  DataGridFocusedNodeIndex := 0;
  DataGridFocusedColumnName := '';
  KeyName := ActiveDbObj.QuotedDatabase+'.'+ActiveDbObj.QuotedName;
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
      Inc(DataGridWantedRowCount, AppSettings.ReadInt(asDatagridRowsPerStep));
  end else begin
    // Save current attributes if grid gets refreshed
    AppSettings.SessionPath := GetRegKeyTable;
    if DataGridHiddenColumns.Count > 0 then
      AppSettings.WriteString(asHiddenColumns, DataGridHiddenColumns.DelimitedText)
    else if AppSettings.ValueExists(asHiddenColumns) then
      AppSettings.DeleteValue(asHiddenColumns);

    if SynMemoFilter.GetTextLen > 0 then
      AppSettings.WriteString(asFilter, SynMemoFilter.Text)
    else if AppSettings.ValueExists(asFilter) then
      AppSettings.DeleteValue(asFilter);

    for i := 0 to High(DataGridSortColumns) do
      Sort := Sort + IntToStr(DataGridSortColumns[i].SortDirection) + '_' + DataGridSortColumns[i].ColumnName + DELIM;
    if Sort <> '' then
      AppSettings.WriteString(asSort, Sort)
    else if AppSettings.ValueExists(asSort) then
      AppSettings.DeleteValue(asSort);
  end;

  // Auto remove registry spam if table folder is empty
  if AppSettings.SessionPathExists(GetRegKeyTable) then begin
    AppSettings.SessionPath := GetRegKeyTable;
    if AppSettings.IsEmptyKey then
      AppSettings.DeleteCurrentKey;
  end;

  // Do nothing if table was not filtered yet
  if not AppSettings.SessionPathExists(GetRegKeyTable) then
    Exit;

  // Columns
  if AppSettings.ValueExists(asHiddenColumns) then
    DataGridHiddenColumns.DelimitedText := AppSettings.ReadString(asHiddenColumns);

  // Set filter, without changing cursor position
  if AppSettings.ValueExists(asFilter) then begin
    Filter := AppSettings.ReadString(asFilter);
    if SynMemoFilter.Text <> Filter then begin
      SynMemoFilter.Text := Filter;
      SynMemoFilter.Modified := True;
    end;
    if SynMemoFilter.GetTextLen > 0 then
      ToggleFilterPanel(True);
  end;

  // Sort
  if AppSettings.ValueExists(asSort) then begin
    SetLength(DataGridSortColumns, 0);
    rx := TRegExpr.Create;
    rx.Expression := '\b(\d)_(.+)\'+DELIM;
    rx.ModifierG := False;
    if rx.Exec(AppSettings.ReadString(asSort)) then while true do begin
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

  AppSettings.ResetPath;
end;


function TMainForm.GetRegKeyTable: String;
begin
  // Return the slightly complex registry path to \Servers\CustomFolder\ActiveServer\curdb|curtable
  Result := ActiveDbObj.Connection.Parameters.SessionPath + '\' +
    ActiveDatabase + DELIM + ActiveDbObj.Name;
end;


procedure TMainForm.pnlQueryMemoCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  // Ensure visibility of query memo while resizing
  Resize := NewWidth >= pnlQueryHelpers.Width + spltQueryHelpers.Width + 40;
end;


procedure TMainForm.AnyGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Grid: TVirtualStringTree;
  Hit: THitInfo;
  Results: TDBQuery;
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
  Conn: TDBConnection;
begin
  // Invalidate list of databases, before (re)painting
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  Conn := ActiveConnection;
  Screen.Cursor := crHourglass;
  vt.Clear;
  if Conn <> nil then begin
    if vt.Tag = VTREE_NOTLOADED_PURGECACHE then begin
      for i:=0 to Conn.AllDatabases.Count-1 do begin
        if Conn.DbObjectsCached(Conn.AllDatabases[i]) then begin
          if Conn.AllDatabases[i] = ActiveDatabase then
            RefreshTree
          else
            Conn.GetDBObjects(Conn.AllDatabases[i], True);
        end;
      end;
    end;
    vt.RootNodeCount := Conn.AllDatabases.Count;
  end;
  tabDatabases.Caption := _('Databases') + ' ('+FormatNumber(vt.RootNodeCount)+')';
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
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  db: String;
  Conn: TDBConnection;
begin
  // Return icon index for databases. Ghosted if db objects not yet in cache.
  if Column <> (Sender as TVirtualStringTree).Header.MainColumn then
    Exit;
  Conn := ActiveConnection;
  db := ListDatabases.Text[Node, 0];
  case Kind of
    ikNormal, ikSelected: ImageIndex := ICONINDEX_DB;
    ikOverlay: if db = Conn.Database then ImageIndex := ICONINDEX_HIGHLIGHTMARKER;
  end;
  Ghosted := not Conn.DbObjectsCached(db);
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
  Conn: TDBConnection;

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

  Conn := ActiveConnection;
  if Idx^ < Conn.AllDatabases.Count then begin
    DBname := Conn.AllDatabases[Idx^];
  end else begin
    // Probably a database were dropped shortly before.
    DBname := '';
  end;
  if Conn.DbObjectsCached(DBname) then
    Objects := Conn.GetDBObjects(DBname)
  else
    Objects := nil;
  CellText := '';
  case Column of
    0: CellText := DBname;
    1: if Assigned(Objects) then CellText := FormatByteNumber(Objects.DataSize);
    2: CellText := GetItemCount(lntNone);
    3: if Assigned(Objects) and (Objects.LastUpdate > 0) then CellText := DateTimeToStr(Objects.LastUpdate);
    4: CellText := GetItemCount(lntTable);
    5: CellText := GetItemCount(lntView);
    6: CellText := GetItemCount(lntFunction);
    7: CellText := GetItemCount(lntProcedure);
    8: CellText := GetItemCount(lntTrigger);
    9: CellText := GetItemCount(lntEvent);
    10: if Assigned(Objects) then CellText := Objects.Collation;
  end;

end;


procedure TMainForm.HostListBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  vt: TVirtualStringTree;
  OldOffset: TPoint;
  Conn: TDBConnection;
  Tab: TTabSheet;
  Results, Variables: TDBQuery;
  i: Integer;
  SelectedCaptions: TStringList;
  IS_objects: TDBObjectList;
  Obj: TDBObject;
  ProcessColumns: TTableColumnList;
  Columns, FocusedCaption, CleanTabCaption: String;
  Col: TVirtualTreeColumn;
begin
  // Display server variables
  vt := Sender as TVirtualStringTree;
  if vt.Tag = VTREE_LOADED then
    Exit;
  Tab := vt.Parent as TTabSheet;
  Conn := ActiveConnection;

  // Status + command statistics only available in MySQL
  if ((vt=ListStatus) or (vt=ListCommandStats))
    and (Conn <> nil)
    and (not Conn.Parameters.IsMySQL) then begin
    vt.Clear;
    vt.EmptyListMessage := f_('Not available on %s', [Conn.Parameters.NetTypeName(Conn.Parameters.NetType, False)]);
    vt.Tag := VTREE_LOADED;
    Exit;
  end;

  SelectedCaptions := TStringList.Create;
  GetVTSelection(vt, SelectedCaptions, FocusedCaption);
  SelectNode(vt, nil);
  vt.BeginUpdate;
  OldOffset := vt.OffsetXY;
  vt.Clear;
  if Conn <> nil then begin
    Results := GridResult(vt);
    if Results <> nil then
      FreeAndNil(Results);
    Screen.Cursor := crHourglass;
    if vt = ListVariables then begin
      // Do not use FHostListResults on Variables tab, as we cannot query
      // session and global variables in one query
      Results := nil;
      FreeAndNil(FVariableNames);
      FreeAndNil(FSessionVars);
      FreeAndNil(FGlobalVars);
      FVariableNames := TStringList.Create;
      FVariableNames.Duplicates := dupIgnore;
      FVariableNames.Sorted := True;
      FSessionVars := TStringList.Create;
      FGlobalVars := TStringList.Create;
      Variables := Conn.GetResults(Conn.GetSQLSpecifity(spSessionVariables));
      while not Variables.Eof do begin
        FVariableNames.Add(Variables.Col(0));
        FSessionVars.Values[Variables.Col(0)] := Variables.Col(1);
        Variables.Next;
      end;
      Variables.Free;
      Variables := Conn.GetResults(Conn.GetSQLSpecifity(spGlobalVariables));
      while not Variables.Eof do begin
        FVariableNames.Add(Variables.Col(0));
        FGlobalVars.Values[Variables.Col(0)] := Variables.Col(1);
        Variables.Next;
      end;
      Variables.Free;
      vt.RootNodeCount := FVariableNames.Count;
    end else if vt = ListStatus then begin
      Results := Conn.GetResults('SHOW /*!50002 GLOBAL */ STATUS');
      FStatusServerUptime := Conn.ServerUptime;
    end else if vt = ListProcesses then begin
      case Conn.Parameters.NetTypeGroup of
        ngMySQL: begin
          if Conn.InformationSchemaObjects.IndexOf('PROCESSLIST') > -1 then begin
            // Minimize network traffic on newer servers by fetching only first KB of SQL query in "Info" column
            Columns := Conn.QuoteIdent('ID')+', '+
              Conn.QuoteIdent('USER')+', '+
              Conn.QuoteIdent('HOST')+', '+
              Conn.QuoteIdent('DB')+', '+
              Conn.QuoteIdent('COMMAND')+', '+
              Conn.QuoteIdent('TIME')+', '+
              Conn.QuoteIdent('STATE')+', '+
              'LEFT('+Conn.QuoteIdent('INFO')+', '+IntToStr(SIZE_KB*50)+') AS '+Conn.QuoteIdent('Info');
            // Get additional column names into SELECT query and ListProcesses tree
            IS_objects := Conn.GetDBObjects('information_schema');
            for Obj in IS_objects do begin
              if Obj.Name = 'PROCESSLIST' then begin
                ProcessColumns := TTableColumnList.Create;
                Conn.ParseTableStructure(Obj.CreateCode, ProcessColumns, nil, nil);
                for i:=8 to ProcessColumns.Count-1 do begin
                  Columns := Columns + ', '+Conn.QuoteIdent(ProcessColumns[i].Name);
                  if ListProcesses.Header.Columns.Count <= i then
                    Col := ListProcesses.Header.Columns.Add
                  else
                    Col := ListProcesses.Header.Columns[i];
                  Col.Options := Col.Options + [coVisible];
                  Col.Text := ProcessColumns[i].Name;
                end;
                // Hide unused tree columns
                for i:=ListProcesses.Header.Columns.Count-1 downto ProcessColumns.Count do
                  ListProcesses.Header.Columns[i].Options := ListProcesses.Header.Columns[i].Options - [coVisible];
                ProcessColumns.Free;
                break;
              end;
            end;
            Results := Conn.GetResults('SELECT '+Columns+' FROM '+
              Conn.QuoteIdent('information_schema')+'.'+Conn.QuoteIdent('PROCESSLIST'));
          end else begin
            // Older servers fetch the whole query length, but at least we cut them off below, so a high memory usage is just a peak
            Results := Conn.GetResults('SHOW FULL PROCESSLIST');
          end;
        end;
        ngMSSQL: begin
          Results := Conn.GetResults('SELECT '+
            Conn.QuoteIdent('p')+'.'+Conn.QuoteIdent('spid')+
            ', RTRIM('+Conn.QuoteIdent('p')+'.'+Conn.QuoteIdent('loginame')+') AS '+Conn.QuoteIdent('loginname')+
            ', RTRIM('+Conn.QuoteIdent('p')+'.'+Conn.QuoteIdent('hostname')+') AS '+Conn.QuoteIdent('hostname')+
            ', '+Conn.QuoteIdent('d')+'.'+Conn.QuoteIdent('name')+
            ', '+Conn.QuoteIdent('p')+'.'+Conn.QuoteIdent('cmd')+
            ', '+Conn.QuoteIdent('p')+'.'+Conn.QuoteIdent('waittime')+
            ', RTRIM('+Conn.QuoteIdent('p')+'.'+Conn.QuoteIdent('status')+'), '+
            'NULL AS '+Conn.QuoteIdent('Info')+' '+
            'FROM '+Conn.QuoteIdent('sys')+'.'+Conn.QuoteIdent('sysprocesses')+' AS '+Conn.QuoteIdent('p')+
            ', '+Conn.GetSQLSpecifity(spDatabaseTable)+' AS '+Conn.QuoteIdent('d')+
            ' WHERE '+Conn.QuoteIdent('p')+'.'+Conn.QuoteIdent('dbid')+'='+Conn.QuoteIdent('d')+'.'+Conn.GetSQLSpecifity(spDatabaseTableId)
            );
        end;
        ngPgSQL: begin
          Results := Conn.GetResults('SELECT '+
            Conn.QuoteIdent('pid')+
            ', '+Conn.QuoteIdent('usename')+
            ', '+Conn.QuoteIdent('client_addr')+
            ', '+Conn.QuoteIdent('datname')+
            ', application_name '+
            ', EXTRACT(EPOCH FROM CURRENT_TIMESTAMP - '+Conn.QuoteIdent('query_start')+')::INTEGER'+
            ', '+Conn.QuoteIdent('state')+
            ', '+Conn.QuoteIdent('query')+
            ' FROM '+Conn.QuoteIdent('pg_stat_activity')
            );
        end;
      end;
      FProcessListMaxTime := 1;
      for i:=0 to Results.RecordCount-1 do begin
        FProcessListMaxTime := Max(FProcessListMaxTime, MakeInt(Results.Col(5)));
        Results.Next;
      end;
    end else if vt = ListCommandStats then begin
      Results := Conn.GetResults('SHOW /*!50002 GLOBAL */ STATUS LIKE ''Com\_%''' );
      FCommandStatsServerUptime := Conn.ServerUptime;
      FCommandStatsQueryCount := 0;
      while not Results.Eof do begin
        Inc(FCommandStatsQueryCount, MakeInt(Results.Col(1)));
        Results.Next;
      end;
    end;

    FHostListResults[Tab.PageIndex] := Results;
    Screen.Cursor := crDefault;
    if Results <> nil then
      vt.RootNodeCount := Results.RecordCount;
    vt.OffsetXY := OldOffset;
  end;
  // Apply or reset filter
  editFilterVTChange(Sender);
  vt.EndUpdate;
  vt.Tag := VTREE_LOADED;
  // Display number of listed values on tab
  CleanTabCaption := RegExprGetMatch('^([^\(]+)', Tab.Caption, 1);
  Tab.Caption := CleanTabCaption.Trim + ' (' + IntToStr(vt.RootNodeCount) + ')';
  // Restore selection
  SetVTSelection(vt, SelectedCaptions, FocusedCaption);
end;


procedure TMainForm.HostListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  vt: TVirtualStringTree;
begin
  vt := Sender as TVirtualStringTree;
  if (Column = 5) and (vt = ListProcesses) then
    PaintColorBar(MakeFloat(vt.Text[Node, Column]), FProcessListMaxTime, TargetCanvas, CellRect);
  if (Column = 4) and (vt = ListCommandStats) then begin
    // Only paint bar in percentage column
    PaintColorBar(MakeFloat(vt.Text[Node, Column]), 100, TargetCanvas, CellRect);
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
  Success, DoCut, DoCopyRows: Boolean;
  IsResultGrid: Boolean;
  ClpFormat: Word;
  ClpData: THandle;
  APalette: HPalette;
  Exporter: TSynExporterRTF;
  Results: TDBQuery;
  RowNum: PInt64;
  ExportDialog: TfrmExportGrid;
begin
  // Copy text from a focused control to clipboard
  Success := False;
  Control := Screen.ActiveControl;
  DoCut := Sender = actCut;
  DoCopyRows := Sender = actCopyRows;
  FClipboardHasNull := False;
  SendingControl := (Sender as TAction).ActionComponent;
  Screen.Cursor := crHourglass;
  try
    if SendingControl = btnPreviewCopy then begin
      if (imgPreview.Picture.Graphic <> nil) and (not imgPreview.Picture.Graphic.Empty) then begin
        imgPreview.Picture.SaveToClipBoardFormat(ClpFormat, ClpData, APalette);
        ClipBoard.SetAsHandle(ClpFormat, ClpData);
        Success := True;
      end;
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
        FGridCopying := True;
        if IsResultGrid then begin
          if DoCopyRows then begin
            ExportDialog := TfrmExportGrid.Create(Sender as TAction);
            ExportDialog.Grid := Grid;
            ExportDialog.btnOK.Click;
            ExportDialog.Free;
          end else begin
            // Handle NULL values in grids, see issue #3171
            AnyGridEnsureFullRow(Grid, Grid.FocusedNode);
            Results := GridResult(Grid);
            RowNum := Grid.GetNodeData(Grid.FocusedNode);
            Results.RecNo := RowNum^;
            if Results.IsNull(Grid.FocusedColumn) then begin
              Clipboard.AsText := '';
              FClipboardHasNull := True;
            end else
              Clipboard.AsText := Grid.Text[Grid.FocusedNode, Grid.FocusedColumn];
            if DoCut then
              Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := '';
          end;
        end else
          Clipboard.AsText := Grid.Text[Grid.FocusedNode, Grid.FocusedColumn];
        FGridCopying := False;
        Success := True;
      end;
    end else if Control is TSynMemo then begin
      SynMemo := Control as TSynMemo;
      if SynMemo.SelAvail then begin
        // Create both text and RTF clipboard format, so rich text applications can paste highlighted SQL
        Clipboard.Open;
        Clipboard.AsText := SynMemo.SelText;
        Exporter := TSynExporterRTF.Create(Self);
        Exporter.Highlighter := SynSQLSynUsed;
        Exporter.ExportAll(Explode(CRLF, SynMemo.SelText));
        if DoCut then SynMemo.CutToClipboard
        else SynMemo.CopyToClipboard;
        Exporter.CopyToClipboard;
        Clipboard.Close;
        Exporter.Free;
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
      FGridPasting := True;
      Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := ClipBoard.AsText;
      Success := True;
      FGridPasting := False;
    end;
  end else if Control is TSynMemo then begin
    SynMemo := TSynMemo(Control);
    if not SynMemo.ReadOnly then begin
      try
        SynMemo.PasteFromClipboard;
        Success := True;
        SynMemo.Modified := True;
      except on E:Exception do
        ErrorDialog(E.Message);
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
  i: Integer;
  item: TMenuItem;
  rx: TRegExpr;
  capt, Path: String;
begin
  // Reset menu and combobox
  menuRecentFilters.Enabled := False;
  for i := menuRecentFilters.Count - 1 downto 0 do
    menuRecentFilters.Delete(i);
  comboRecentFilters.Items.Clear;
  // Enumerate recent filters from registry
  Path := GetRegKeyTable+'\'+REGKEY_RECENTFILTERS;
  if AppSettings.SessionPathExists(Path) then begin
    AppSettings.SessionPath := Path;
    rx := TRegExpr.Create;
    rx.Expression := '\s+';
    for i:=1 to 20 do begin
      // Previously introduced bugs stored some other settings here, see issue #2127
      item := TMenuItem.Create(popupFilter);
      capt := AppSettings.ReadString(asRecentFilter, IntToStr(i));
      if IsEmpty(capt) then
        Break;
      capt := rx.Replace(capt, ' ', True);
      item.Hint := capt;
      item.Caption := sstr(capt, 50);
      item.Tag := i;
      item.OnClick := LoadRecentFilter;
      menuRecentFilters.Add(item);
      comboRecentFilters.Items.Add(capt);
    end;
    FreeAndNil(rx);
    AppSettings.ResetPath;
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
  Path: String;
begin
  // Event handler for both dynamic popup menu items and filter combobox
  if Sender is TMenuItem then
    key := (Sender as TMenuItem).Tag
  else
    key := (Sender as TComboBox).ItemIndex+1;
  Path := GetRegKeyTable+'\'+REGKEY_RECENTFILTERS;
  if AppSettings.SessionPathExists(Path) then begin
    AppSettings.SessionPath := Path;
    SynMemoFilter.UndoList.AddGroupBreak;
    SynMemoFilter.BeginUpdate;
    SynMemoFilter.SelectAll;
    SynMemoFilter.SelText := AppSettings.ReadString(asRecentFilter, IntToStr(key));
    SynMemoFilter.EndUpdate;
    AppSettings.ResetPath;
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
    MainForm.SetupSynEditors;
  end;
  ActiveObjectEditor.Init(Obj);
  UpdateFilterPanel(Self);
end;


procedure TMainForm.UpdateEditorTab;
var
  Cap: String;
begin
  tabEditor.ImageIndex := ActiveObjectEditor.DBObject.ImageIndex;
  Cap := _(ActiveObjectEditor.DBObject.ObjType)+': ';
  if ActiveObjectEditor.DBObject.Name = '' then
    Cap := Cap + '['+_('Untitled')+']'
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
        FCreateDatabaseDialog := TCreateDatabaseForm.Create(Self);
        FCreateDatabaseDialog.modifyDB := ActiveDatabase;
        if FCreateDatabaseDialog.ShowModal = mrOk then
          RefreshTree;
        FreeAndNil(FCreateDatabaseDialog);
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

  QueryTabs.Add(TQueryTab.Create(Self));
  QueryTab := QueryTabs[QueryTabs.Count-1];
  QueryTab.Number := i;
  QueryTab.Uid := TQueryTab.GenerateUid;

  QueryTab.TabSheet := TTabSheet.Create(PageControlMain);
  QueryTab.TabSheet.PageControl := PageControlMain;
  QueryTab.TabSheet.ImageIndex := tabQuery.ImageIndex;

  QueryTab.CloseButton := TSpeedButton.Create(QueryTab.TabSheet);
  QueryTab.CloseButton.Parent := PageControlMain;
  QueryTab.CloseButton.Width := 16;
  QueryTab.CloseButton.Height := 16;
  QueryTab.CloseButton.Flat := True;
  VirtualImageListMain.GetBitmap(134, QueryTab.CloseButton.Glyph);
  QueryTab.CloseButton.OnMouseDown := CloseButtonOnMouseDown;
  QueryTab.CloseButton.OnMouseUp := CloseButtonOnMouseUp;
  SetTabCaption(QueryTab.TabSheet.PageIndex, '');

  // Dumb code which replicates all controls from tabQuery
  QueryTab.pnlMemo := TPanel.Create(QueryTab.TabSheet);
  QueryTab.pnlMemo.Parent := QueryTab.TabSheet;
  QueryTab.pnlMemo.BevelOuter := pnlQueryMemo.BevelOuter;
  QueryTab.pnlMemo.Align := pnlQueryMemo.Align;
  QueryTab.pnlMemo.Height := pnlQueryMemo.Height;

  QueryTab.Memo := TSynMemo.Create(QueryTab.pnlMemo);
  QueryTab.Memo.Parent := QueryTab.pnlMemo;
  QueryTab.Memo.Align := SynMemoQuery.Align;
  QueryTab.Memo.Left := SynMemoQuery.Left;
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
  QueryTab.Memo.OnKeyPress := SynMemoQuery.OnKeyPress;
  QueryTab.Memo.OnMouseWheel := SynMemoQuery.OnMouseWheel;
  QueryTab.Memo.OnReplaceText := SynMemoQuery.OnReplaceText;
  QueryTab.Memo.OnStatusChange := SynMemoQuery.OnStatusChange;
  QueryTab.Memo.OnPaintTransient := SynMemoQuery.OnPaintTransient;
  SynCompletionProposal.AddEditor(QueryTab.Memo);

  QueryTab.spltHelpers := TSplitter.Create(QueryTab.pnlMemo);
  QueryTab.spltHelpers.Parent := QueryTab.pnlMemo;
  QueryTab.spltHelpers.Align := spltQueryHelpers.Align;
  QueryTab.spltHelpers.Left := spltQueryHelpers.Left;
  QueryTab.spltHelpers.Cursor := spltQueryHelpers.Cursor;
  QueryTab.spltHelpers.ResizeStyle := spltQueryHelpers.ResizeStyle;
  QueryTab.spltHelpers.Width := spltQueryHelpers.Width;

  QueryTab.pnlHelpers := TPanel.Create(QueryTab.pnlMemo);
  QueryTab.pnlHelpers.Parent := QueryTab.pnlMemo;
  QueryTab.pnlHelpers.Align := pnlQueryHelpers.Align;
  QueryTab.pnlHelpers.BevelOuter := pnlQueryHelpers.BevelOuter;
  QueryTab.pnlHelpers.Left := pnlQueryHelpers.Left;
  QueryTab.pnlHelpers.Width := pnlQueryHelpers.Width;

  QueryTab.filterHelpers := TButtonedEdit.Create(QueryTab.pnlHelpers);
  QueryTab.filterHelpers.Parent := QueryTab.pnlHelpers;
  QueryTab.filterHelpers.Align := filterQueryHelpers.Align;
  QueryTab.filterHelpers.TextHint := filterQueryHelpers.TextHint;
  QueryTab.filterHelpers.Images := filterQueryHelpers.Images;
  QueryTab.filterHelpers.LeftButton.Visible := filterQueryHelpers.LeftButton.Visible;
  QueryTab.filterHelpers.LeftButton.ImageIndex := filterQueryHelpers.LeftButton.ImageIndex;
  QueryTab.filterHelpers.RightButton.Visible := filterQueryHelpers.RightButton.Visible;
  QueryTab.filterHelpers.RightButton.ImageIndex := filterQueryHelpers.RightButton.ImageIndex;
  QueryTab.filterHelpers.OnChange := filterQueryHelpers.OnChange;
  QueryTab.filterHelpers.OnRightButtonClick := filterQueryHelpers.OnRightButtonClick;

  QueryTab.treeHelpers := TVirtualStringTree.Create(QueryTab.pnlHelpers);
  QueryTab.treeHelpers.Parent := QueryTab.pnlHelpers;
  QueryTab.treeHelpers.Align := treeQueryHelpers.Align;
  QueryTab.treeHelpers.Left := treeQueryHelpers.Left;
  QueryTab.treeHelpers.Constraints.MinWidth := treeQueryHelpers.Constraints.MinWidth;
  QueryTab.treeHelpers.PopupMenu := treeQueryHelpers.PopupMenu;
  QueryTab.treeHelpers.Images := treeQueryHelpers.Images;
  QueryTab.treeHelpers.DragMode := treeQueryHelpers.DragMode;
  QueryTab.treeHelpers.DragType := treeQueryHelpers.DragType;
  QueryTab.treeHelpers.OnBeforeCellPaint := treeQueryHelpers.OnBeforeCellPaint;
  QueryTab.treeHelpers.OnChecking := treeQueryHelpers.OnChecking;
  QueryTab.treeHelpers.OnContextPopup := treeQueryHelpers.OnContextPopup;
  QueryTab.treeHelpers.OnCreateEditor := treeQueryHelpers.OnCreateEditor;
  QueryTab.treeHelpers.OnDblClick := treeQueryHelpers.OnDblClick;
  QueryTab.treeHelpers.OnEditing := treeQueryHelpers.OnEditing;
  QueryTab.treeHelpers.OnFreeNode := treeQueryHelpers.OnFreeNode;
  QueryTab.treeHelpers.OnGetImageIndex := treeQueryHelpers.OnGetImageIndex;
  QueryTab.treeHelpers.OnGetText := treeQueryHelpers.OnGetText;
  QueryTab.treeHelpers.OnInitChildren := treeQueryHelpers.OnInitChildren;
  QueryTab.treeHelpers.OnInitNode := treeQueryHelpers.OnInitNode;
  QueryTab.treeHelpers.OnNewText := treeQueryHelpers.OnNewText;
  QueryTab.treeHelpers.OnNodeClick := treeQueryHelpers.OnNodeClick;
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
  QueryTab.spltQuery.Top := spltQuery.Top; // Important to get it below the editor, not above. See #439
  QueryTab.spltQuery.Align := spltQuery.Align;
  QueryTab.spltQuery.Height := spltQuery.Height;
  QueryTab.spltQuery.Cursor := spltQuery.Cursor;
  QueryTab.spltQuery.ResizeStyle := spltQuery.ResizeStyle;
  QueryTab.spltQuery.AutoSnap := spltQuery.AutoSnap;

  QueryTab.ResultTabs := TResultTabs.Create(True);

  QueryTab.tabsetQuery := TTabSet.Create(QueryTab.TabSheet);
  QueryTab.tabsetQuery.Parent := QueryTab.TabSheet;
  // Prevent various problems with alignment of controls. See http://www.heidisql.com/forum.php?t=18924
  QueryTab.tabsetQuery.Top := QueryTab.spltQuery.Top + QueryTab.spltQuery.Height;
  QueryTab.tabsetQuery.Align := tabsetQuery.Align;
  InheritFont(QueryTab.tabsetQuery.Font);
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
  // Special case: the very first tab gets cleared but not closed
  if PageIndex = tabQuery.PageIndex then begin
    if ConfirmTabClear(PageIndex, False) then
      actClearQueryEditor.Execute;
  end;
  if not IsQueryTab(PageIndex, False) then
    Exit;
  // Ask user if query content shall be saved to disk
  if not ConfirmTabClose(PageIndex, False) then
    Exit;
  // Work around bugs in ComCtrls.TPageControl.RemovePage
  NewPageIndex := PageControlMain.ActivePageIndex;
  if NewPageIndex >= PageIndex then
    Dec(NewPageIndex);
  // Avoid excessive flicker:
  LockWindowUpdate(PageControlMain.Handle);
  PageControlMain.Pages[PageIndex].Free;
  QueryTabs.Delete(PageIndex-tabQuery.PageIndex);
  PageControlMain.ActivePageIndex := NewPageIndex;
  FixQueryTabCloseButtons;
  LockWindowUpdate(0);
  PageControlMain.OnChange(PageControlMain);
end;


procedure TMainForm.actFavoriteObjectsOnlyExecute(Sender: TObject);
begin
  // Click on "tree favorites" main button
  AppSettings.ResetPath;
  AppSettings.WriteBool(asFavoriteObjectsOnly, (Sender as TAction).Checked);
  editDatabaseTableFilterChange(Sender);
end;


procedure TMainForm.buttonedEditClear(Sender: TObject);
begin
  // Click on "clear" button of any TButtonedEdit control
  TButtonedEdit(Sender).Clear;
end;


procedure TMainForm.editDatabaseTableFilterChange(Sender: TObject);
var
  Node: PVirtualNode;
  Obj: PDBObject;
  rxdb, rxtable: TRegExpr;
  NodeMatches: Boolean;
  Errors: TStringList;
begin
  // Immediately apply database filter
  LogSQL('editDatabaseTableFilterChange', lcDebug);

  rxdb := TRegExpr.Create;
  rxdb.ModifierI := True;
  rxdb.Expression := '('+StringReplace(editDatabaseFilter.Text, ';', '|', [rfReplaceAll])+')';
  rxtable := TRegExpr.Create;
  rxtable.ModifierI := True;
  rxtable.Expression := '('+StringReplace(editTableFilter.Text, ';', '|', [rfReplaceAll])+')';

  Errors := TStringList.Create;

  DBtree.BeginUpdate;
  Node := DBtree.GetFirst;
  while Assigned(Node) do begin
    Obj := DBtree.GetNodeData(Node);
    NodeMatches := True;
    try
      case Obj.NodeType of
        lntDb: begin
          // Match against database filter
          if editDatabaseFilter.Text <> '' then
            NodeMatches := rxdb.Exec(DBtree.Text[Node, 0]);
        end;
        lntTable..lntEvent: begin
          // Match against table filter
          if editTableFilter.Text <> '' then
            NodeMatches := rxtable.Exec(DBtree.Text[Node, 0]);
          if actFavoriteObjectsOnly.Checked then
            // Hide non-favorite object path
            NodeMatches := NodeMatches and (Obj.Connection.Favorites.IndexOf(Obj.Path) > -1);
        end;
      end;
    except
      on E:Exception do begin
        // Log regex errors, but avoid duplicate messages
        if Errors.IndexOf(E.Message) = -1 then begin
          LogSQL(E.Message);
          Errors.Add(E.Message);
        end;
      end;
    end;
    DBtree.IsVisible[Node] := NodeMatches;

    Node := DBtree.GetNextInitialized(Node);
  end;
  DBtree.EndUpdate;

  rxdb.Free;
  rxtable.Free;

  editDatabaseFilter.RightButton.Visible := editDatabaseFilter.Text <> '';
  editTableFilter.RightButton.Visible := editTableFilter.Text <> '';
end;


procedure TMainForm.editDatabaseTableFilterLeftButtonClick(Sender: TObject);
var
  Menu: TPopupMenu;
  Item: TMenuItem;
  P: TPoint;
  Edit: TButtonedEdit;
  History: TStringList;
  ItemText: String;
  Setting: TAppSettingIndex;
begin
  // Create right menu with filter history
  Edit := Sender as TButtonedEdit;
  Menu := TPopupMenu.Create(Edit);
  Menu.AutoHotkeys := maManual;
  Menu.Images := VirtualImageListMain;
  AppSettings.SessionPath := '';
  if Edit = editDatabaseFilter then
    Setting := asDatabaseFilter
  else
    Setting := asTableFilter;
  History := TStringList.Create;
  History.Text := AppSettings.ReadString(Setting);
  for ItemText in History do begin
    Item := TMenuItem.Create(Menu);
    Item.Caption := ItemText;
    Item.OnClick := editDatabaseTableFilterMenuClick;
    Item.Checked := ItemText = Edit.Text;
    Menu.Items.Add(Item);
  end;
  History.Free;

  Item := TMenuItem.Create(Menu);
  Item.Caption := _('Clear');
  Item.ImageIndex := 193;
  Item.OnClick := editDatabaseTableFilterMenuClick;
  Item.Tag := 1;
  Item.Enabled := Edit.Text <> '';
  Menu.Items.Add(Item);

  P := Edit.ClientToScreen(Edit.ClientRect.TopLeft);
  Menu.Popup(p.X, p.Y+16);
end;


procedure TMainForm.editDatabaseTableFilterMenuClick(Sender: TObject);
var
  Menu: TPopupMenu;
  Item: TMenuItem;
  Edit: TButtonedEdit;
begin
  // Insert text from filter history menu
  Item := Sender as TMenuItem;
  Menu := Item.Owner as TPopupMenu;
  Edit := Menu.Owner as TButtonedEdit;
  if Item.Tag = 1 then
    Edit.Clear
  else
    Edit.Text := Item.Caption;
  Menu.Free;
end;


procedure TMainForm.editDatabaseTableFilterExit(Sender: TObject);
var
  History: TStringList;
  Edit: TButtonedEdit;
  i: Integer;
  Setting: TAppSettingIndex;
begin
  // Add (move) custom filter text to (in) drop down history, if not empty
  Edit := Sender as TButtonedEdit;
  AppSettings.SessionPath := '';
  if Edit = editDatabaseFilter then
    Setting := asDatabaseFilter
  else
    Setting := asTableFilter;
  History := TStringList.Create;
  History.Text := AppSettings.ReadString(Setting);
  i := History.IndexOf(Edit.Text);
  if i > -1 then
    History.Delete(i);
  History.Insert(0, Edit.Text);
  for i:=History.Count-1 downto 0 do begin
    if (i >= 20) or (Trim(History[i]) = '') then
      History.Delete(i);
  end;
  AppSettings.WriteString(Setting, History.Text);
  History.Free;
end;


procedure TMainForm.CloseButtonOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseDownCloseButton := Sender;
end;


procedure TMainForm.CloseButtonOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  // Click on "Close" button of Query tab
  if Button <> mbLeft then
    Exit;
  // Between MousDown and MouseUp it is possible that the focused tab has switched. As we simulate a mouse-click
  // here, we must check if also the MouseDown event was fired on this particular button. See issue #1469.
  if (Sender <> FLastMouseDownCloseButton) then
    Exit;
  for i:=0 to QueryTabs.Count-1 do begin
    if QueryTabs[i].CloseButton = Sender then begin
      CloseQueryTab(QueryTabs[i].TabSheet.PageIndex);
      break;
    end;
  end;
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
  if not Assigned(FBtnAddTab) then
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
  FBtnAddTab.Top := Rect.Top;
  FBtnAddTab.Left := Rect.Right + 5;
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


function TMainForm.ActiveOrEmptyQueryTab(ConsiderActiveTab: Boolean): TQueryTab;
var
  i: Integer;
begin
  // Return either a) current query tab if one is active
  // or b) the first empty one
  // or c) create a new one
  // Result should never be nil, unlike in ActiveQueryTab
  Result := nil;
  if ConsiderActiveTab then
    Result := ActiveQueryTab;
  if Result = nil then begin
    // Search empty tab
    for i:=0 to QueryTabs.Count-1 do begin
      if (QueryTabs[i].MemoFilename='') and (QueryTabs[i].Memo.GetTextLen=0) then begin
        Result := QueryTabs[i];
        break;
      end;
    end;
    // Create new tab
    if Result = nil then begin
      actNewQueryTabExecute(Self);
      Result := QueryTabs[QueryTabs.Count-1];
    end;
  end;
end;


function TMainForm.GetQueryTabByNumber(Number: Integer): TQueryTab;
var
  i: Integer;
begin
  // Find right query tab
  Result := nil;
  for i:=0 to QueryTabs.Count-1 do begin
    if QueryTabs[i].Number = Number then begin
      Result := QueryTabs[i];
      break;
    end;
  end;
end;


function TMainForm.GetQueryTabByHelpers(FindTree: TBaseVirtualTree): TQueryTab;
var
  Tab: TQueryTab;
begin
  // Find query tab where passed treeHelpers resides
  Result := nil;
  for Tab in QueryTabs do begin
    if Tab.treeHelpers = FindTree then begin
      Result := Tab;
      break;
    end;
  end;
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


function TMainForm.ActiveSynMemo(AcceptReadOnlyMemo: Boolean): TSynMemo;
var
  Control: TWinControl;
begin
  Result := nil;
  Control := Screen.ActiveControl;
  if Control is TCustomSynEdit then begin
    Result := Control as TSynMemo;
    // We have a few readonly-SynMemos which we'll ignore here
    if (not AcceptReadOnlyMemo) and Result.ReadOnly then
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


function TMainForm.GridResult(Grid: TBaseVirtualTree): TDBQuery;
var
  QueryTab: TQueryTab;
  CurrentTab: TTabSheet;
  ResultTab: TResultTab;
begin
  // All grids (data- and query-grids, also host subtabs) are placed directly on a TTabSheet
  Result := nil;
  if Grid = DataGrid then begin
    if DataGridResult<>nil then
      Result := DataGridResult;
  end else if Assigned(Grid) then begin
    CurrentTab := Grid.Parent as TTabSheet;
    if CurrentTab.Parent = PageControlHost then
      Result := FHostListResults[CurrentTab.PageIndex]
    else for QueryTab in QueryTabs do begin
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
  Cap := DBtree.Path(DBtree.FocusedNode, 0, '\') + ' - ' + APPNAME;
  if AppSettings.PortableMode then
    Cap := Cap + ' Portable';
  Cap := Cap + ' ' + FAppVersion;
  Caption := Cap;
  Application.Title := Cap;
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
    Text := _('Query');
  if IsQueryTab(PageIndex, False) then begin
    if Text = '' then begin
      for Tab in QueryTabs do begin
        if Tab.TabSheet = PageControlMain.Pages[PageIndex] then begin
          Text := _('Query')+' #'+IntToStr(Tab.Number);
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


function TMainForm.ConfirmTabClose(PageIndex: Integer; AppIsClosing: Boolean): Boolean;
var
  Tab: TQueryTab;
begin
  Tab := QueryTabs[PageIndex-tabQuery.PageIndex];
  if Tab.QueryRunning then begin
    LogSQL(_('Cannot close tab with running query. Please wait until query has finished.'));
    Result := False;
  end else if not Tab.Memo.Modified then begin
    Result := True;
  end else begin
    Result := ConfirmTabClear(PageIndex, AppIsClosing);
  end;
end;


function TMainForm.ConfirmTabClear(PageIndex: Integer; AppIsClosing: Boolean): Boolean;
var
  msg: String;
  Tab: TQueryTab;
  SaveDialog: TSaveDialog;
  MsgButtons: TMsgDlgButtons;
begin
  Tab := QueryTabs[PageIndex-tabQuery.PageIndex];
  // Unhide tabsheet so the user sees the memo content
  Tab.TabSheet.PageControl.ActivePage := Tab.TabSheet;

  // Prompt for saving unsaved contents
  if Tab.MemoFilename <> '' then
    msg := f_('Save changes to file %s ?', [Tab.MemoFilename])
  else
    msg := f_('Save content of tab "%s"?', [Trim(Tab.TabSheet.Caption)]);
  if AppSettings.RestoreTabsInitValue and AppIsClosing then begin
    msg := msg + CRLF + CRLF + _('Your code is saved anyway, as auto-restoring is activated.');
  end;

  if FConnections.Count > 0 then
    MsgButtons := [mbYes, mbNo, mbCancel]
  else
    MsgButtons := [mbYes, mbNo];

  case MessageDialog(_('Modified query'), msg, mtConfirmation, MsgButtons, asPromptSaveFileOnTabClose) of
    mrNo: Result := True;
    mrYes: begin
      SaveDialog := TSaveDialog.Create(Self);
      SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
      SaveDialog.Filter := _('SQL files')+' (*.sql)|*.sql|'+_('All files')+' (*.*)|*.*';
      SaveDialog.DefaultExt := 'sql';
      if Tab.MemoFilename <> '' then
        Tab.SaveContents(Tab.MemoFilename, False)
      else if SaveDialog.Execute then
        Tab.SaveContents(SaveDialog.FileName, False);
      // The save dialog can be cancelled.
      Result := not Tab.Memo.Modified;
      SaveDialog.Free;
    end;
    else Result := False;
  end;

  // Auto-backup logic
  if AppSettings.RestoreTabsInitValue then begin
    if AppIsClosing then begin
      // Do last backup before app closes
      Tab.BackupUnsavedContent;
    end else begin
      // Delete backup file if tab is closed by user, intentionally
      if (not Tab.MemoBackupFilename.IsEmpty) and FileExists(Tab.MemoBackupFilename) then begin
        if not DeleteFile(Tab.MemoBackupFilename) then begin
          ErrorDialog(f_('Backup file could not be deleted: %s', [Tab.MemoBackupFilename]));
        end;
      end;
    end;
  end;

  if (not Result) and (FConnections.Count = 0) then begin
    // Upper right "X" button clicked, or save dialog cancelled
    Result := True;
  end;

  ValidateControls(Self);
end;


procedure TMainForm.actFilterPanelExecute(Sender: TObject);
begin
  // (De-)activate or focus filter panel
  if Sender <> actFilterPanel then
    actFilterPanel.Checked := not actFilterPanel.Checked;
  pnlFilterVT.Visible := actFilterPanel.Checked;
  // On startup, we cannot SetFocus, throws exceptons. Call with nil in that special case - see FormCreate
  if pnlFilterVT.Visible and editFilterVT.CanFocus and (Sender <> nil) then
    editFilterVT.SetFocus;
  UpdateFilterPanel(Sender);
end;


procedure TMainForm.UpdateFilterPanel(Sender: TObject);
var
  tab: TTabSheet;
  f: String;
begin
  // Called when active tab changes
  pnlFilterVT.Enabled := (PageControlMain.ActivePage <> tabEditor) or (ActiveObjectEditor is TfrmTableEditor);
  lblFilterVT.Enabled := pnlFilterVT.Enabled;
  editFilterVT.Enabled := pnlFilterVT.Enabled;
  lblFilterVTInfo.Enabled := pnlFilterVT.Enabled;
  if pnlFilterVT.Enabled then
    editFilterVT.Color := GetThemeColor(clWindow)
  else
    editFilterVT.Color := GetThemeColor(clBtnFace);

  tab := PageControlMain.ActivePage;
  if tab = tabHost then
    tab := PageControlHost.ActivePage;
  if not pnlFilterVT.Visible then begin
    if editFilterVT.Text <> '' then
      editFilterVT.Text := ''
    else
      editFilterVTChange(Sender);
  end else begin
    if tab = tabDatabases then f := FFilterTextDatabases
    else if tab = tabVariables then f := FFilterTextVariables
    else if tab = tabStatus then f := FFilterTextStatus
    else if tab = tabProcesslist then f := FFilterTextProcessList
    else if tab = tabCommandStats then f := FFilterTextCommandStats
    else if tab = tabDatabase then f := FFilterTextDatabase
    else if tab = tabEditor then f := FFilterTextEditor
    else if tab = tabData then f := FFilterTextData
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
  if Assigned(FPreferencesDialog) then
    Editors.Add(FPreferencesDialog.SynMemoSQLSample);
  if Assigned(FCreateDatabaseDialog) then
    Editors.Add(FCreateDatabaseDialog.SynMemoCreateCode);
  if SqlHelpDialog <> nil then begin
    Editors.Add(SqlHelpDialog.memoDescription);
    Editors.Add(SqlHelpDialog.MemoExample);
  end;

  if AppSettings.ReadBool(asTabsToSpaces) then
    BaseEditor.Options := BaseEditor.Options + [eoTabsToSpaces]
  else
    BaseEditor.Options := BaseEditor.Options - [eoTabsToSpaces];
  ActiveLineColor := StringToColor(AppSettings.ReadString(asSQLColActiveLine));
  MatchingBraceForegroundColor := StringToColor(AppSettings.ReadString(asSQLColMatchingBraceForeground));
  MatchingBraceBackgroundColor := StringToColor(AppSettings.ReadString(asSQLColMatchingBraceBackground));

  for i:=0 to Editors.Count-1 do begin
    Editor := Editors[i] as TSynMemo;
    LogSQL('Setting up TSynMemo "'+Editor.Name+'"', lcDebug);
    Editor.Color := GetThemeColor(clWindow);
    Editor.ScrollHintColor := GetThemeColor(clInfoBk);
    Editor.Font.Name := AppSettings.ReadString(asFontName);
    Editor.Font.Size := AppSettings.ReadInt(asFontSize);
    Editor.Gutter.BorderColor := GetThemeColor(clWindow);
    Editor.Gutter.Color := GetThemeColor(clBtnFace);
    Editor.Gutter.Font.Name := Editor.Font.Name;
    Editor.Gutter.Font.Size := Editor.Font.Size;
    Editor.Gutter.Font.Color := BaseEditor.Gutter.Font.Color;
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
    Editor.TabWidth := AppSettings.ReadInt(asTabWidth);
    Editor.MaxScrollWidth := BaseEditor.MaxScrollWidth;
    Editor.WantTabs := BaseEditor.WantTabs;
    Editor.OnPaintTransient := BaseEditor.OnPaintTransient;
    // Shortcuts
    if Editor = BaseEditor then for j:=0 to Editor.Keystrokes.Count-1 do begin
      KeyStroke := Editor.Keystrokes[j];
      Shortcut1 := AppSettings.ReadInt(asActionShortcut1, EditorCommandToCodeString(Keystroke.Command));
      Shortcut2 := AppSettings.ReadInt(asActionShortcut2, EditorCommandToCodeString(Keystroke.Command));
      try
        if Shortcut1<>0 then
          Keystroke.ShortCut := Shortcut1;
        if Shortcut2<>0 then
          Keystroke.ShortCut2 := Shortcut2;
      except
        on E:ESynKeyError do begin
          LogSQL(f_('Could not apply SynEdit keystroke shortcut "%s" (or secondary: "%s") to %s. %s. Please go to Tools > Preferences > Shortcuts to change this settings.',
            [ShortCutToText(Shortcut1), ShortCutToText(Shortcut2), EditorCommandToCodeString(Keystroke.Command), E.Message, _('Tools'), _('Preferences'), _('Shortcuts')]), lcError);
        end;
      end;
    end else
      Editor.Keystrokes := BaseEditor.KeyStrokes;
  end;
  // Highlighting
  for i:=0 to SynSQLSynUsed.AttrCount - 1 do begin
    Attri := SynSQLSynUsed.Attribute[i];
    Attri.Foreground := AppSettings.ReadInt(asHighlighterForeground, Attri.Name, Attri.Foreground);
    Attri.Background := AppSettings.ReadInt(asHighlighterBackground, Attri.Name, Attri.Background);
    Attri.IntegerStyle := AppSettings.ReadInt(asHighlighterStyle, Attri.Name, Attri.IntegerStyle);
  end;
end;


procedure TMainForm.actReformatSQLExecute(Sender: TObject);
var
  m: TCustomSynEdit;
  CursorPosStart, CursorPosEnd: Integer;
  NewSQL: String;
begin
  // Reformat SQL query
  m := ActiveSynMemo(False);
  if not Assigned(m) then begin
    ErrorDialog(_('Cannot reformat'), _('Please select a non-readonly SQL editor first.'));
    Exit;
  end;
  CursorPosStart := m.SelStart;
  CursorPosEnd := m.SelEnd;
  if not m.SelAvail then
    m.SelectAll;
  NewSQL := m.SelText;
  if Length(NewSQL) = 0 then
    ErrorDialog(_('Cannot reformat'), _('The current editor is empty.'))
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
  TabsHeight := (FBtnAddTab.Height+2) * PageControlMain.RowCount;
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
  // Generate SELECT, INSERT, UPDATE or DELETE query using selected columns
  MenuItem := (Sender as TMenuItem);
  ColumnNames := TStringList.Create;
  DefaultValues := TStringList.Create;
  Tree := ActiveQueryHelpers;
  Node := Tree.GetFirstChild(FindNode(Tree, HELPERNODE_COLUMNS, nil));
  while Assigned(Node) do begin
    if Tree.Selected[Node] then begin
      ColumnNames.Add(ActiveConnection.QuoteIdent(Tree.Text[Node, 0], False));
      Column := SelectedTableColumns[Node.Index];
      case Column.DataType.Category of
        dtcInteger, dtcReal: Val := '0';
        dtcText, dtcOther: begin
          Val := esc(Column.DefaultText);
          if Column.DefaultType in [cdtNull] then
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
    idx := ColumnNames.IndexOf(ActiveConnection.QuoteIdent(KeyColumns[i], False));
    if idx > -1 then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + ActiveConnection.QuoteIdent(KeyColumns[i], False)+'='+DefaultValues[idx];
    end;
  end;


  if MenuItem = menuQueryHelpersGenerateSelect then begin
    sql := 'SELECT '+ImplodeStr(', ', ColumnNames)+CRLF+
      #9'FROM '+ActiveDbObj.QuotedName(False);

  end else if MenuItem = menuQueryHelpersGenerateInsert then begin
    sql := 'INSERT INTO '+ActiveDbObj.QuotedName(False)+CRLF+
      #9'('+ImplodeStr(', ', ColumnNames)+')'+CRLF+
      #9'VALUES ('+ImplodeStr(', ', DefaultValues)+')';

  end else if MenuItem = menuQueryHelpersGenerateUpdate then begin
    sql := 'UPDATE '+ActiveDbObj.QuotedName(False)+CRLF+#9'SET'+CRLF;
    if ColumnNames.Count > 0 then begin
      for i:=0 to ColumnNames.Count-1 do begin
        sql := sql + #9#9 + ColumnNames[i] + '=' + DefaultValues[i] + ',' + CRLF;
      end;
      Delete(sql, Length(sql)-2, 1);
    end else
      sql := sql + #9#9'??? # No column names selected!'+CRLF;
    sql := sql + #9'WHERE ' + WhereClause;

  end else if MenuItem = menuQueryHelpersGenerateDelete then begin
    sql := 'DELETE FROM '+ActiveDbObj.QuotedName(False)+' WHERE ' + WhereClause;

  end;
  ActiveQueryMemo.UndoList.AddGroupBreak;
  ActiveQueryMemo.SelText := sql;
end;


procedure TMainForm.DBtreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  Obj: PDBObject;
begin
  // Paint favorite icon on table node
  if Column <> 0 then
    Exit;
  Obj := Sender.GetNodeData(Node);
  if Obj.NodeType in [lntTable..lntEvent] then begin
    if Obj.Connection.Favorites.IndexOf(Obj.Path) > -1 then
      VirtualImageListMain.Draw(TargetCanvas, CellRect.Left, CellRect.Top, 168)
    else if Node = Sender.HotNode then
      VirtualImageListMain.Draw(TargetCanvas, CellRect.Left, CellRect.Top, 183);
  end;
end;


procedure TMainForm.DBtreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Obj: PDBObject;
  Node: PVirtualNode;
  idx: Integer;
begin
  // Watch out for clicks on favorite icon
  // Add or remove object path from favorite list on click
  Node := DBtree.GetNodeAt(X, Y);
  if (Button = mbLeft) and (X < VirtualImageListMain.Width) and Assigned(Node) then begin
    Obj := DBtree.GetNodeData(Node);
    if Obj.NodeType in [lntTable..lntEvent] then begin
      idx := Obj.Connection.Favorites.IndexOf(Obj.Path);
      if idx > -1 then
        Obj.Connection.Favorites.Delete(idx)
      else
        Obj.Connection.Favorites.Add(Obj.Path);
      DBtree.RepaintNode(Node);
      AppSettings.SessionPath := Obj.Connection.Parameters.SessionPath;
      AppSettings.WriteString(asFavoriteObjects, Obj.Connection.Favorites.Text);
    end;
  end;
end;


procedure TMainForm.DBtreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  DBObj: PDBObject;
  AllObjects: TDBObjectList;
begin
  if CellPaintMode=cpmPaint then begin
    DBObj := Sender.GetNodeData(Node);
    if DbObj.Connection.Parameters.SessionColor <> AppSettings.GetDefaultInt(asTreeBackground) then begin
      TargetCanvas.Brush.Color := DbObj.Connection.Parameters.SessionColor;
      TargetCanvas.FillRect(CellRect);
    end;
    if (Column=1) and DBObj.Connection.DbObjectsCached(DBObj.Database) then begin
      AllObjects := DBObj.Connection.GetDBObjects(DBObj.Database);
      PaintColorBar(DBObj.Size, AllObjects.LargestObjectSize, TargetCanvas, CellRect);
    end;
  end;
end;


procedure TMainForm.DBtreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  VT: TVirtualStringTree;
begin
  // Resize "Size" column in dbtree to hold widest possible byte numbers without cutting text
  VT := Sender as TVirtualStringTree;
  if (VT.Header.Columns.Count >= 2) and (coVisible in VT.Header.Columns[1].Options) then
    VT.Header.Columns[1].Width := TextWidth(VT.Canvas, FormatByteNumber(SIZE_MB*100)) + VT.TextMargin*2 + 8;
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
  Connection: TDBConnection;
  Tab: TQueryTab;
  ConnectionParams: TConnectionParameters;
  FileNames: TStringList;
begin
  // Probably a second instance is posting its command line parameters here
  if (Msg.CopyDataStruct.dwData = SecondInstMsgId) and (SecondInstMsgId <> 0) then begin
    LogSQL(f_('Preventing second application instance - disabled in %s > %s > %s.', [_('Tools'), _('Preferences'), _('General')]), lcInfo);
    ConnectionParams := nil;
    ParseCommandLine(ParamBlobToStr(Msg.CopyDataStruct.lpData), ConnectionParams, FileNames);
    if not RunQueryFiles(FileNames, nil, False) then begin
      for i:=0 to FileNames.Count-1 do begin
        Tab := ActiveOrEmptyQueryTab(False);
        Tab.LoadContents(FileNames[i], True, nil);
      end;
    end;
    if ConnectionParams <> nil then
      InitConnection(ConnectionParams, True, Connection);
  end else
    // Not the right message id
    inherited;
end;


procedure TMainForm.CMStyleChanged(var Msg: TMessage);
begin
  // Style theme applied, e.g. via preferences dialog
  // Ensure SynMemo's have fitting colors
  SetupSynEditors;
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


procedure TMainForm.AnyGridScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
begin
  // A tree gets scrolled only when the mouse is over it - see FormMouseWheel
  // Our home brewn cell editors do not reposition when the underlying tree scrolls.
  // To avoid confusion, terminate editors then.
  if Sender.IsEditing and (DeltaX=0) then
    Sender.EndEditNode;
end;


procedure TMainForm.lblExplainProcessAnalyzerClick(Sender: TObject);
begin
  try
    ActiveConnection.ExplainAnalyzer(SynMemoProcessView.Text, listProcesses.Text[listProcesses.FocusedNode, 3]);
  except
    on E:EDbError do
      ErrorDialog(E.Message);
  end;
end;


procedure TMainForm.lblExplainProcessClick(Sender: TObject);
var
  Tab: TQueryTab;
  UsedDatabase: String;
begin
  // Click on "Explain" link label, in process viewer
  actNewQueryTabExecute(Sender);
  Tab := QueryTabs[QueryTabs.Count-1];
  UsedDatabase := listProcesses.Text[listProcesses.FocusedNode, 3];
  if not UsedDatabase.IsEmpty then begin
    Tab.Memo.Lines.Add('USE ' + ActiveConnection.QuoteIdent(UsedDatabase) + ';');
  end;
  Tab.Memo.Lines.Add('EXPLAIN' + sLineBreak + SynMemoProcessView.Text + ';');
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
    if Grid.SelectedCount > 1 then
      AppendMsg := ' ('+FormatNumber(Grid.SelectedCount)+' sel)';
  end else if QueryTabActive and ActiveQueryMemo.Focused then begin
    x := ActiveQueryMemo.CaretX;
    y := ActiveQueryMemo.CaretY;
    AppendMsg := ' ('+FormatByteNumber(ActiveQueryMemo.GetTextLen)+')';
  end;
  if (x > -1) and (y > -1) then begin
    ShowStatusMsg(FormatNumber(x)+'↔ : '+FormatNumber(y) + '↕' + AppendMsg, 1)
  end else
    ShowStatusMsg('', 1);
end;

procedure TMainForm.AnyGridStartOperation(Sender: TBaseVirtualTree; OperationKind: TVTOperationKind);
begin
  // Display status message on long running sort operations
  if not MainFormCreated then begin
    // Do nothing before form is not ready to process messages, what OperationRunning silently does.
    // See issue #665
    Exit;
  end;
  if OperationKind = okSortTree then begin
    ShowStatusMsg(_('Sorting grid nodes ...'));
    FOperatingGrid := Sender;
    OperationRunning(True);
  end;
end;


procedure TMainForm.AnyGridEndOperation(Sender: TBaseVirtualTree; OperationKind: TVTOperationKind);
begin
  // Reset status message after long running operations
  if OperationKind = okSortTree then begin
    ShowStatusMsg;
    FOperatingGrid := nil;
    OperationRunning(False);
  end;
end;


procedure TMainForm.actCancelOperationExecute(Sender: TObject);
var
  Killer: TDBConnection;
  KillCommand: String;
  Tab: TQueryTab;
begin
  // Stop current operation (sorting grid or running user queries)
  if FOperatingGrid <> nil then begin
    FOperatingGrid.CancelOperation;
    LogSQL(_('Sorting cancelled.'));
  end;
  for Tab in QueryTabs do begin
    if Tab.QueryRunning then begin
      Tab.ExecutionThread.Aborted := True;
      Killer := ActiveConnection.Parameters.CreateConnection(Self);
      Killer.Parameters := ActiveConnection.Parameters;
      Killer.LogPrefix := _('Helper connection');
      Killer.OnLog := LogSQL;
      Killer.Active := True;
      KillCommand := Format(Killer.GetSQLSpecifity(spKillQuery), [ActiveConnection.ThreadId]);
      try
        Killer.Query(KillCommand);
      except
        on E:EDbError do begin
          MessageDialog(E.Message, mtError, [mbOK]);
        end;
      end;
      Killer.Active := False;
      Killer.Free;
    end;
  end;
end;


procedure TMainForm.OperationRunning(Runs: Boolean);
begin
  if actCancelOperation.Enabled <> Runs then begin
    actCancelOperation.ImageIndex := 159;
    actCancelOperation.Enabled := Runs;
    FOperationTicker := IfThen(Runs, GetTickCount, 0);
    Application.ProcessMessages;
  end else if Runs then begin
    if (GetTickCount-FOperationTicker) > 250 then begin
      // Signalize running operation
      if actCancelOperation.ImageIndex = 159 then
        actCancelOperation.ImageIndex := 160
      else
        actCancelOperation.ImageIndex := 159;
      Application.ProcessMessages;
      FOperationTicker := GetTickCount;
    end;
  end;
end;


function TMainForm.GetEncodingByName(Name: String): TEncoding;
begin
  Result := nil;
  case FileEncodings.IndexOf(Name) of
    1: Result := TEncoding.Default;
    2: Result := TEncoding.GetEncoding(437);
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
  else if (Encoding <> nil) and (Encoding.CodePage = 437) then idx := 2
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
  end else if (Encoding <> nil) and (Encoding.CodePage = 437) then
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
  History: TQueryHistory;
begin
  // Paint green value bar in cell
  if (Node.Parent.Index=HELPERNODE_PROFILE)
    and (Column=1)
    and (Sender.GetNodeLevel(Node)=1)
    then begin
    Tab := GetQueryTabByHelpers(Sender);
    if Tab <> nil then begin
      Tab.QueryProfile.RecNo := Node.Index;
      PaintColorBar(MakeFloat(Tab.QueryProfile.Col(Column)), Tab.MaxProfileTime, TargetCanvas, CellRect);
    end;
  end;
  if (Sender.GetNodeLevel(Node)=2)
    and (Column=1)
    and (Node.Parent.Parent.Index=HELPERNODE_HISTORY) then begin
    Tab := GetQueryTabByHelpers(Sender);
    if Tab <> nil then begin
      History := Tab.HistoryDays.Objects[Node.Parent.Index] as TQueryHistory;
      PaintColorBar(History[Node.Index].Duration, History.MaxDuration, TargetCanvas, CellRect);
    end;
  end;
end;


procedure TMainForm.treeQueryHelpersPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  History: TQueryHistory;
  Tab: TQueryTab;
begin
  // Paint text in datatype's color
  if (Node.Parent.Index=HELPERNODE_COLUMNS)
    and (Column=1)
    and (Sender.GetNodeLevel(Node)=1)
    and (ActiveDbObj.NodeType in [lntView, lntTable])
    then begin
    TargetCanvas.Font.Color := DatatypeCategories[SelectedTableColumns[Node.Index].DataType.Category].Color;
  end;
  if (Sender.GetNodeLevel(Node)=2)
    and (Node.Parent.Parent.Index=HELPERNODE_HISTORY)
    and (ActiveConnection <> nil) then begin
    Tab := GetQueryTabByHelpers(Sender);
    if Tab <> nil then begin
      History := Tab.HistoryDays.Objects[Node.Parent.Index] as TQueryHistory;
      if ActiveConnection.Database <> History[Node.Index].Database then
        TargetCanvas.Font.Color := GetThemeColor(clGrayText);
    end;
  end;

  // If there is no value for bind variable, the font style is Italic and Underline
  if (Sender.GetNodeLevel(Node)=1)
    and (Column=1)
    and (Node.Parent.Index=HELPERNODE_BINDING) then begin
      Tab := GetQueryTabByHelpers(Sender);
      if StrLen(PChar(Tab.ListBindParams.Items[Node.Index].Value)) = 0 then
        TargetCanvas.Font.Style := [fsItalic]+[fsUnderline];

      TargetCanvas.Font.Color := GetThemeColor(clGrayText);

    end;
  
end;


procedure TMainForm.treeQueryHelpersResize(Sender: TObject);
var
  Tree: TVirtualStringTree;
begin
  // Resizing query helpers box: Keep second column at a minimum width.
  Tree := Sender as TVirtualStringTree;
  if Tree.Header.Columns.Count >= 2 then begin
    // See https://github.com/HeidiSQL/HeidiSQL/issues/466
    // Column count may be 0 in an early stage of creating a new query tab
    Tree.Header.Columns[1].Width := Min(Tree.Width div 3, 100);
  end;
end;


procedure TMainForm.treeQueryHelpersDblClick(Sender: TObject);
var
  m: TSynMemo;
begin
  m := ActiveQueryMemo;
  m.DragDrop(Sender, m.CaretX, m.CaretY);
end;


procedure TMainForm.treeQueryHelpersEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  // If current column is value of Bind Param, we allow editing
  if (Column = 1)
    and (Sender.FocusedNode.Parent.Index = HELPERNODE_BINDING) then begin
      Allowed := True;
    end
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


procedure TMainForm.treeQueryHelpersFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Tab: TQueryTab;
begin
  // Free some memory, taken by probably big SQL query history items
  if (Sender.GetNodeLevel(Node)=1)
    and (Node.Parent.Index = HELPERNODE_HISTORY) then begin
    Tab := GetQueryTabByHelpers(Sender);
    if Tab <> nil then begin
      Tab.HistoryDays.Objects[Node.Index].Free;
      Tab.HistoryDays.Delete(Node.Index);
    end;
  end;
end;


procedure TMainForm.treeQueryHelpersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  // Query helpers tree fetching node icon index
  if not (Kind in [ikNormal, ikSelected]) then
    Exit;
  if Column <> 0 then
    Exit;
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
         HELPERNODE_COLUMNS: if (ActiveDbObj <> nil) and (ActiveDbObj.NodeType <> lntNone) then
              ImageIndex := ActiveDbObj.ImageIndex
            else
              ImageIndex := 14;
         HELPERNODE_FUNCTIONS: ImageIndex := 13;
         HELPERNODE_KEYWORDS: ImageIndex := 25;
         HELPERNODE_SNIPPETS: ImageIndex := 51;
         HELPERNODE_HISTORY: ImageIndex := 149;
         HELPERNODE_PROFILE: ImageIndex := 145;
         HELPERNODE_BINDING: ImageIndex := 119;
       end;
    1: case Node.Parent.Index of
         HELPERNODE_COLUMNS: ImageIndex := 42;
         HELPERNODE_FUNCTIONS: ImageIndex := 13;
         HELPERNODE_KEYWORDS: ImageIndex := 25;
         HELPERNODE_SNIPPETS: ImageIndex := 68;
         HELPERNODE_HISTORY: ImageIndex := 80;
         HELPERNODE_PROFILE: ImageIndex := 145;
         HELPERNODE_BINDING: ImageIndex := 42;
       end;
  end;
end;


procedure TMainForm.treeQueryHelpersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  History: TQueryHistory;
  Tab: TQueryTab;
begin
  // Query helpers tree fetching node text
  CellText := '';
  Tab := GetQueryTabByHelpers(Sender);
  case Column of
    0: case Sender.GetNodeLevel(Node) of
        0: case Node.Index of
             HELPERNODE_COLUMNS: begin
               CellText := _('Columns');
               if ActiveDbObj <> nil then case ActiveDbObj.NodeType of
                 lntProcedure, lntFunction: CellText := f_('Parameters in %s', [ActiveDbObj.Name]);
                 lntTable, lntView: CellText := f_('Columns in %s', [ActiveDbObj.Name]);
               end;
             end;
             HELPERNODE_FUNCTIONS: CellText := _('SQL functions');
             HELPERNODE_KEYWORDS: CellText := _('SQL keywords');
             HELPERNODE_SNIPPETS: CellText := _('Snippets');
             HELPERNODE_HISTORY: CellText := _('Query history');
             HELPERNODE_PROFILE: begin
                  CellText := _('Query profile');
                  if Assigned(Tab.QueryProfile) then
                    CellText := CellText + ' ('+FormatNumber(Tab.ProfileTime, 6)+'s)';
                end;
             HELPERNODE_BINDING: begin
                  CellText := _('Bind parameters');
                  if(Tab.BindParamsActivated) then
                    CellText := CellText + ' ('+FormatNumber(Tab.ListBindParams.Count)+')';
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
             HELPERNODE_HISTORY: begin
               CellText := Tab.HistoryDays[Node.Index];
               if CellText = DateToStr(Today) then
                 CellText := CellText + ', '+_('today')
               else if CellText = DateToStr(Yesterday) then
                 CellText := CellText + ', '+_('yesterday');
             end;
             HELPERNODE_PROFILE: begin
                  if Assigned(Tab.QueryProfile) then begin
                    Tab.QueryProfile.RecNo := Node.Index;
                    CellText := Tab.QueryProfile.Col(Column);
                  end;
                end;
             HELPERNODE_BINDING: CellText := Tab.ListBindParams[Node.Index].Parameter;
           end;
        2: case Node.Parent.Parent.Index of
             HELPERNODE_HISTORY: begin
               History := Tab.HistoryDays.Objects[Node.Parent.Index] as TQueryHistory;
               CellText := Copy(TimeToStr(History[Node.Index].Time), 1, 5)+': '+History[Node.Index].SQL;
             end
             else CellText := ''; // unused
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
                  if Assigned(Tab.QueryProfile) then begin
                    Tab.QueryProfile.RecNo := Node.Index;
                    CellText := FormatNumber(Tab.QueryProfile.Col(Column))+'s';
                  end;
                end;
             HELPERNODE_BINDING: begin
                  // If value is empty, display MsgBindParamNoValue
                  if StrLen(PChar(Tab.ListBindParams[Node.Index].Value))>0 then
                    CellText := Tab.ListBindParams[Node.Index].Value
                  else
                    CellText := _('Set value');
                end;
             else CellText := '';
           end;
        2: case Node.Parent.Parent.Index of
             HELPERNODE_HISTORY: begin
               History := Tab.HistoryDays.Objects[Node.Parent.Index] as TQueryHistory;
               CellText := FormatNumber(History[Node.Index].Duration / 1000, 3)+'s';
             end;
           end;
      end;
  end;
end;


procedure TMainForm.treeQueryHelpersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // Query helpers tree asking if plus/minus button should be displayed
  case Sender.GetNodeLevel(Node) of
    0: begin
      Include(InitialStates, ivsHasChildren);
      if Node.Index = HELPERNODE_PROFILE then
        Node.CheckType := ctCheckbox;
      if Node.Index = HELPERNODE_BINDING then
        Node.CheckType := ctCheckbox;
    end;
    1: begin
      AppSettings.ResetPath;
      if (Node.Parent.Index = HELPERNODE_HISTORY) and AppSettings.ReadBool(asQueryHistoryEnabled) then
        Include(InitialStates, ivsHasChildren);
    end;
  end;
end;


procedure TMainForm.treeQueryHelpersNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Tree: TVirtualStringTree;
  QueryTab: TQueryTab;
begin
  Tree:= Sender as TVirtualStringTree;
  QueryTab := ActiveQueryTab;

  // Save new param value
  QueryTab.ListBindParams.Items[Sender.FocusedNode.Index].Value := NewText;

  Tree.RepaintNode(Node);
end;


procedure TMainForm.treeQueryHelpersNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  Tree: TVirtualStringTree;
begin
  Tree := Sender as TVirtualStringTree;
  // If the column is clicked a parameter value, it goes directly into the edit mode
  if (HitInfo.HitNode.Parent.Index = HELPERNODE_BINDING) and (HitInfo.HitColumn = 1) then
    Tree.EditNode(Sender.FocusedNode,Sender.FocusedColumn);
end;


procedure TMainForm.treeQueryHelpersInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  QueryDay: String;
  History: TQueryHistory;
  Item: TQueryHistoryItem;
  Tab: TQueryTab;
  i: Integer;
begin
  Tab := GetQueryTabByHelpers(Sender);
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
         HELPERNODE_COLUMNS: begin
           ChildCount := 0;
           if ActiveDbObj <> nil then case ActiveDbObj.NodeType of
             lntTable, lntView:
               ChildCount := SelectedTableColumns.Count;
             lntFunction, lntProcedure:
               if Assigned(ActiveObjectEditor) then
                 ChildCount := TfrmRoutineEditor(ActiveObjectEditor).Parameters.Count
               else
                 ChildCount := 0;
           end;
         end;
         HELPERNODE_FUNCTIONS: ChildCount := Length(MySQLFunctions);
         HELPERNODE_KEYWORDS: ChildCount := MySQLKeywords.Count;
         HELPERNODE_SNIPPETS: ChildCount := FSnippetFilenames.Count;
         HELPERNODE_HISTORY: begin
           AppSettings.ResetPath;
           if AppSettings.ReadBool(asQueryHistoryEnabled) then begin
             // Find all unique days in history
             if not Assigned(Tab.HistoryDays) then
               Tab.HistoryDays := TStringList.Create;
             Tab.HistoryDays.Clear;
             History := TQueryHistory.Create(ActiveConnection.Parameters.SessionPath);
             for Item in History do begin
               QueryDay := DateToStr(Item.Time);
               if Tab.HistoryDays.IndexOf(QueryDay) = -1 then
                 Tab.HistoryDays.Add(QueryDay);
             end;
             History.Free;
             Tab.HistoryDays.CustomSort(StringListCompareAnythingDesc);
             ChildCount := Tab.HistoryDays.Count;
           end;
         end;
         HELPERNODE_PROFILE: if not Assigned(Tab.QueryProfile) then ChildCount := 0
            else ChildCount := Tab.QueryProfile.RecordCount;
         HELPERNODE_BINDING: ChildCount := Tab.ListBindParams.Count;
       end;
    1: case Node.Parent.Index of
      HELPERNODE_HISTORY: begin
        History := TQueryHistory.Create(ActiveConnection.Parameters.SessionPath);
        Tab.HistoryDays.Objects[Node.Index] := History;
        for i:=History.Count-1 downto 0 do begin
          QueryDay := DateToStr(History[i].Time);
          if QueryDay <> Tab.HistoryDays[Node.Index] then
            History.Delete(i);
        end;
        ChildCount := History.Count;
      end;
      else ChildCount := 0;
    end;
  end;
end;


procedure TMainForm.SetSnippetFilenames;
var
  Files: TStringDynArray;
  Snip: String;
  i: Integer;
begin
  // Refreshing list of snippet file names needs to refresh helper node too
  if not Assigned(FSnippetFilenames) then
    FSnippetFilenames := TStringList.Create;
  FSnippetFilenames.Clear;
  try
    Files := TDirectory.GetFiles(AppSettings.DirnameSnippets, '*.sql');
    for i:=0 to Length(Files)-1 do begin
      Snip := ExtractFilename(Files[i]);
      Snip := Copy(Snip, 1, Length(Snip)-4);
      FSnippetFilenames.Add(snip);
    end;
    FSnippetFilenames.Sort;
  except
    on E:Exception do begin
      LogSQL(f_('Error with snippets directory: %s', [E.Message]), lcError);
    end;
  end;
  RefreshHelperNode(HELPERNODE_SNIPPETS);
end;


procedure TMainForm.treeQueryHelpersChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  Tab: TQueryTab;
  Tree: TVirtualStringTree;
begin
  Tree := Sender as TVirtualStringTree;
  Tab := GetQueryTabByHelpers(Sender);

  case Sender.GetNodeLevel(Node) of

    0: case Node.Index of
      HELPERNODE_BINDING: begin
        // Disallow checkbox clicking on "Bind parameters" when text too big
        if NewState in CheckedStates then begin
          if Tab.Memo.GetTextLen < SIZE_MB then begin
            Allowed := True;
            Tab.TimerLastChange.Enabled := False;
            Tab.TimerLastChange.Enabled := True;
            LogSQL('Bind parameters enabled', lcDebug);
          end
          else begin
            Allowed := False;
            MessageDialog(_('The query is too long to enable detection of bind parameters'), mtError, [mbOK]);
          end;
        end
        else begin
          Allowed := True;
          Tab.ListBindParams.Clear;
          NewState := csUncheckedNormal;
          Tree.ResetNode(Node);
          LogSQL('Bind parameters disabled', lcDebug);
        end;
      end;
    end;

  end;

end;


procedure TMainForm.treeQueryHelpersContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Tree: TVirtualStringTree;
begin
  Tree := Sender as TVirtualStringTree;
  menuQueryHelpersGenerateSelect.Enabled := False;
  menuQueryHelpersGenerateInsert.Enabled := False;
  menuQueryHelpersGenerateUpdate.Enabled := False;
  menuQueryHelpersGenerateDelete.Enabled := False;
  menuInsertSnippetAtCursor.Enabled := False;
  menuLoadSnippet.Enabled := False;
  menuDeleteSnippet.Enabled := False;
  menuExplore.Enabled := False;
  menuHelp.Enabled := False;
  menuClearQueryHistory.Enabled := False;

  case Tree.GetNodeLevel(Tree.FocusedNode) of
    0: ;
    1: case Tree.FocusedNode.Parent.Index of
      HELPERNODE_COLUMNS: if ActiveDbObj.NodeType in [lntTable, lntView] then begin
          menuQueryHelpersGenerateSelect.Enabled := True;
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
      HELPERNODE_HISTORY:
        menuClearQueryHistory.Enabled := True;
    end;
    2: case Tree.FocusedNode.Parent.Parent.Index of
      HELPERNODE_HISTORY:
        menuClearQueryHistory.Enabled := True;
    end;
  end;
end;


procedure TMainForm.treeQueryHelpersCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
 InplaceEditor: TInplaceEditorLink;
 VT: TVirtualStringTree;
begin
  VT := Sender as TVirtualStringTree;
  InplaceEditor := TInplaceEditorLink.Create(VT);
  InplaceEditor.ButtonVisible := true;
  EditLink := InplaceEditor;
end;


procedure TMainForm.RefreshHelperNode(NodeIndex: Cardinal);
var
  Tab: TQueryTab;
  Node, Child: PVirtualNode;
  OldStates: TVirtualNodeStates;
  OldCheckState: TCheckState;
  ExpandedChildren: TStringList;
begin
  if not Assigned(QueryTabs) then
    Exit;
  for Tab in QueryTabs do begin
    Node := FindNode(Tab.treeHelpers, NodeIndex, nil);
    // Store node + children states
    OldStates := Node.States;
    OldCheckState := Node.CheckState;
    ExpandedChildren := TStringList.Create;
    Child := Tab.treeHelpers.GetFirstChild(Node);
    while Assigned(Child) do begin
      if vsExpanded in Child.States then
        ExpandedChildren.Add(IntToStr(Child.Index));
      Child := Tab.treeHelpers.GetNextSibling(Child);
    end;
    // Keep scroll offset
    Tab.treeHelpers.BeginUpdate;
    // Remove children and grandchildren
    Tab.treeHelpers.ResetNode(Node);
    // Restore old node + children states
    Tab.treeHelpers.CheckState[Node] := OldCheckState;
    Tab.treeHelpers.Expanded[Node] := vsExpanded in OldStates;
    // Do not check expansion state of children unless the parent node is expanded, to avoid
    // initializing children when not required. Accesses registry items when doing so.
    if Tab.treeHelpers.Expanded[Node] then begin
      Child := Tab.treeHelpers.GetFirstChild(Node);
      while Assigned(Child) do begin
        Tab.treeHelpers.Expanded[Child] := ExpandedChildren.IndexOf(IntToStr(Child.Index)) > -1;
        Child := Tab.treeHelpers.GetNextSibling(Child);
      end;
    end;
    ExpandedChildren.Free;
    Tab.treeHelpers.EndUpdate;
  end;
end;


procedure TMainForm.ApplicationEvents1Deactivate(Sender: TObject);
begin
  // Force result tab balloon hint to disappear. Does not do so when mouse was moved too fast.
  tabsetQueryMouseLeave(Sender);
end;


procedure TMainForm.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  if AppSettings.PortableMode
    and (not AppSettings.PortableModeReadOnly)
    and (FLastPortableSettingsSave < GetTickCount-60000) then begin
    try
      if AppSettings.Writes > FLastAppSettingsWrites then begin
        if AppSettings.ExportSettings then
          LogSQL(f_('Portable settings file written, with %d changes.', [AppSettings.Writes-FLastAppSettingsWrites]), lcDebug);
      end;
      FLastAppSettingsWrites := AppSettings.Writes;
      FLastPortableSettingsSave := GetTickCount;
    except
      on E:Exception do
        ErrorDialog(E.Message);
    end;
  end;

  // Sort list tables in idle time, so ListTables.TreeOptions.AutoSort does not crash the list
  // when dropping a right-clicked database
  if (PageControlMain.ActivePage = tabDatabase) and (not FListTablesSorted) then begin
    ListTables.SortTree(ListTables.Header.SortColumn, ListTables.Header.SortDirection);
    FListTablesSorted := True;
  end;
end;


procedure TMainForm.ApplicationDeActivate(Sender: TObject);
begin
  // Prevent completion window from showing up after Alt-Tab. See issue #2640
  // and issue #3342
  // Does not work for some reason in TApplicationEvents.OnDeactivate
  SynCompletionProposal.Form.Enabled := False;
  // Gets activated again in SynCompletionProposalExecute
end;


procedure TMainForm.ApplicationShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  MainTabIndex, QueryTabIndex: integer;
  pt: TPoint;
begin
  // Show full filename in tab hint. See issue #3527
  // Code taken from http://www.delphipraxis.net/97988-tabsheet-hint-funktioniert-nicht.html
  if HintInfo.HintControl = PageControlMain then begin
    pt := PageControlMain.ScreenToClient(Mouse.CursorPos);
    MainTabIndex := GetMainTabAt(pt.X, pt.Y);
    QueryTabIndex := MainTabIndex - tabQuery.PageIndex;
    if (QueryTabIndex >= 0) and (QueryTabIndex < QueryTabs.Count) then
      HintStr := QueryTabs[QueryTabIndex].MemoFilename;
    HintInfo.ReshowTimeout := 1000;
  end;
end;


procedure TMainForm.actToggleCommentExecute(Sender: TObject);
var
  Editor: TSynMemo;
  rx: TRegExpr;
  Sel: TStringList;
  i: Integer;
  IsComment: Boolean;
begin
  // Un/comment selected SQL
  Editor := ActiveSynMemo(False);
  Editor.UndoList.AddGroupBreak;
  rx := TRegExpr.Create;
  rx.Expression := '^(\s*)(\-\- |#)?(.*)$';
  if not Editor.SelAvail then begin
    rx.Exec(Editor.LineText);
    if rx.MatchLen[2] > 0 then
      Editor.LineText := rx.Match[1] + rx.Match[3]
    else
      Editor.LineText := '-- '+Editor.LineText;
  end else begin
    Sel := Explode(CRLF, Editor.SelText);
    IsComment := False;
    for i:=0 to Sel.Count-1 do begin
      rx.Exec(Sel[i]);
      if i = 0 then
        IsComment := rx.MatchLen[2] > 0;
      if IsComment then
        Sel[i] := rx.Match[1] + rx.Match[3]
      else
        Sel[i] := '-- '+Sel[i];
    end;
    Editor.SelText := ImplodeStr(CRLF, Sel);
  end;
  Editor.OnChange(Sender);
end;


procedure TMainForm.EnableProgress(MaxValue: Integer);
begin
  // Initialize progres bar and button
  SetProgressPosition(0);
  SetProgressState(pbsNormal);
  ProgressBarStatus.Visible := True;
  ProgressBarStatus.Max := MaxValue;
end;


procedure TMainForm.DisableProgress;
begin
  // Hide global progress bar
  SetProgressPosition(0);
  ProgressBarStatus.Hide;
  if Assigned(TaskBarList3) then
    TaskBarList3.SetProgressState(Application.MainForm.Handle, 0);
end;


procedure TMainForm.SetProgressPosition(Value: Integer);
begin
  // Advance progress bar and task progress position
  try
    ProgressBarStatus.Position := Value;
  except
    // Silence "Floating point division by zero." - see https://www.heidisql.com/forum.php?t=26218
    on E:EZeroDivide do;
  end;
  ProgressBarStatus.Repaint;
  if Assigned(TaskBarList3) then
    TaskBarList3.SetProgressValue(Application.MainForm.Handle, Value, ProgressBarStatus.Max);
end;


procedure TMainForm.ProgressStep;
begin
  SetProgressPosition(ProgressBarStatus.Position+1);
end;


procedure TMainForm.SetProgressState(State: TProgressbarState);
var
  Flag: Integer;
begin
  // Set error or pause state in progress bar or task button
  ProgressBarStatus.State := State;
  ProgressBarStatus.Repaint;
  if Assigned(TaskBarList3) then begin
    case State of
      pbsNormal: Flag := 2;
      pbsError: Flag := 4;
      pbsPaused: Flag := 8;
      else Flag := 0;
    end;
    TaskBarList3.SetProgressState(Application.MainForm.Handle, Flag);
  end;
end;


procedure TMainForm.TaskDialogHyperLinkClicked(Sender: TObject);
begin
  // Used by hyperlinks in helpers.MessageDialog()
  if Sender is TTaskDialog then
    ShellExec(TTaskDialog(Sender).URL);
end;


function TMainForm.HasDonated(ForceCheck: Boolean): TThreeStateBoolean;
var
  Email, CheckResult: String;
  rx: TRegExpr;
  CheckWebpage: THttpDownload;
begin
  Screen.Cursor := crHourGlass;
  if (FHasDonatedDatabaseCheck = nbUnset) or (ForceCheck) then begin
    Email := AppSettings.ReadString(asDonatedEmail);
    if Email = '' then begin
      // Nothing to check, we know this is not valid
      FHasDonatedDatabaseCheck := nbFalse;
    end else begin
      // Check heidisql.com/hasdonated.php?email=...
      // FHasDonatedDatabaseCheck
      //   = 0 : No check yet done
      //   = 1 : Not a donor
      //   = 2 : Valid donor
      rx := TRegExpr.Create;
      CheckWebpage := THttpDownload.Create(MainForm);
      CheckWebpage.URL := APPDOMAIN + 'hasdonated.php?email='+EncodeURLParam(Email);
      try
        try
          CheckWebpage.SendRequest('');
        except
          on E:Exception do begin
            // Try again without SSL. See issue #65
            LogSQL(E.Message, lcError);
            CheckWebpage.URL := ReplaceRegExpr('^https:', CheckWebpage.URL, 'http:');
            CheckWebpage.SendRequest('');
          end;
        end;
        CheckResult := CheckWebpage.LastContent;
        LogSQL('HTTP response: "'+CheckResult+'"', lcDebug);
        rx.Expression := '^\d';
        if rx.Exec(CheckResult) then begin
          if CheckResult = '0' then
            FHasDonatedDatabaseCheck := nbFalse
          else
            FHasDonatedDatabaseCheck := nbTrue;
        end;
      except
        on E:Exception do begin
          LogSQL(E.Message + sLineBreak + 'HTTP response: "'+CheckResult+'"', lcError);
          FHasDonatedDatabaseCheck := nbUnset; // Could have been set before, when ForceCheck=true
        end;
      end;
      CheckWebpage.Free;
      rx.Free;
    end;
  end;
  Result := FHasDonatedDatabaseCheck;
  Screen.Cursor := crDefault;
end;


procedure TMainForm.actPreviousResultExecute(Sender: TObject);
var
  Tab: TQueryTab;
begin
  // Go back to the result tab left to the active one
  Tab := ActiveQueryTab;
  if Tab <> nil then begin
    if Tab.tabsetQuery.TabIndex > 0 then
      Tab.tabsetQuery.SelectNext(False)
    else
      MessageBeep(MB_ICONEXCLAMATION);
  end;
end;


procedure TMainForm.actNextResultExecute(Sender: TObject);
var
  Tab: TQueryTab;
begin
  // Advance to the next result tab
  Tab := ActiveQueryTab;
  if Tab <> nil then begin
    if Tab.tabsetQuery.TabIndex < Tab.tabsetQuery.Tabs.Count-1 then
      Tab.tabsetQuery.SelectNext(True)
    else
      MessageBeep(MB_ICONEXCLAMATION);
  end;
end;




{ TQueryTab }


constructor TQueryTab.Create;
begin
  // Creation of a new main query tab
  DirectoryWatch := TDirectoryWatch.Create;
  DirectoryWatch.WatchSubTree := False;
  DirectoryWatch.OnNotify := DirectoryWatchNotify;
  // Do not trigger useless file deletion messages, see issue #2948
  DirectoryWatch.WatchActions := DirectoryWatch.WatchActions - [waRemoved];
  // Timer which postpones calling waModified event code until buffers have been saved
  MemofileModifiedTimer := TTimer.Create(Memo);
  MemofileModifiedTimer.Interval := 1000;
  MemofileModifiedTimer.Enabled := False;
  MemofileModifiedTimer.OnTimer := MemofileModifiedTimerNotify;
  LastSaveTime := 0;
  FLastChange := 0;
  TimerLastChange := TTimer.Create(Self);
  TimerLastChange.Enabled := True;
  TimerLastChange.OnTimer := TimerLastChangeOnTimer;
  // Contain 2 columns of String : Params & Values
  ListBindParams := TBindParam.Create;
  // Update status bar every second while query runs
  TimerStatusUpdate := TTimer.Create(Self);
  TimerStatusUpdate.Enabled := False;
  TimerStatusUpdate.Interval := 100;
  TimerStatusUpdate.OnTimer := TimerStatusUpdateOnTimer;
end;


destructor TQueryTab.Destroy;
begin
  ResultTabs.Clear;
  DirectoryWatch.Free;
  ListBindParams.Free;
  TimerLastChange.Free;
  TimerStatusUpdate.Free;
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
        and (MessageDialog(_('Close file and query tab?'), f_('File was deleted from outside: %s', [MemoFilename]), mtConfirmation, [mbYes, mbCancel]) = mrYes) then begin
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


procedure TQueryTab.SetMemo(Value: TSynMemo);
begin
  // Apply existing SynMemo and its events (only OnChange yet).
  // TODO: Move more Memo events from TMainForm.actNewQueryTabExecute here, and keep there procedures in TQueryTab.
  FMemo := Value;
  FMemo.OnChange := MemoOnChange;
end;


procedure TQueryTab.MemofileModifiedTimerNotify(Sender: TObject);
var
  OldTopLine: Integer;
  OldCursor: TBufferCoord;
begin
  (Sender as TTimer).Enabled := False;
  if MessageDialog(_('Reload file?'), f_('File was modified from outside: %s', [MemoFilename]), mtConfirmation, [mbYes, mbCancel]) = mrYes then begin
    OldCursor := Memo.CaretXY;
    OldTopLine := Memo.TopLine;
    LoadContents(MemoFilename, True, nil);
    Memo.CaretXY := OldCursor;
    Memo.TopLine := OldTopLine;
  end;
end;


function TQueryTab.LoadContents(Filename: String; ReplaceContent: Boolean; Encoding: TEncoding): Boolean;
var
  Content: String;
  Filesize: Int64;
  LineBreaks: TLineBreaks;
begin
  Result := False;
  // Load file and add that to the undo-history of SynEdit.
  // Normally we would do a simple SynMemo.Lines.LoadFromFile but
  // this would prevent SynEdit from adding this step to the undo-history
  // so we have to do it by replacing the SelText property
  Screen.Cursor := crHourGlass;
  Filesize := _GetFileSize(filename);
  MainForm.LogSQL(f_('Loading file "%s" (%s) into query tab #%d ...', [Filename, FormatByteNumber(Filesize), Number]), lcInfo);
  try
    Content := ReadTextfile(Filename, Encoding);
    if Pos(AppSettings.DirnameSnippets, Filename) = 0 then
      MainForm.AddOrRemoveFromQueryLoadHistory(Filename, True, True);
    Memo.UndoList.AddGroupBreak;
    Memo.BeginUpdate;
    LineBreaks := ScanLineBreaks(Content);
    if ReplaceContent then begin
      Memo.SelectAll;
      MemoLineBreaks := LineBreaks;
    end else begin
      if (MemoLineBreaks <> lbsNone) and (MemoLineBreaks <> LineBreaks) then
        MemoLineBreaks := lbsMixed
      else
        MemoLineBreaks := LineBreaks;
    end;
    if MemoLineBreaks = lbsMixed then
      MessageDialog(_('This file contains mixed linebreaks. They have been converted to Windows linebreaks (CR+LF).'), mtInformation, [mbOK]);

    Memo.SelText := Content;
    Memo.SelStart := Memo.SelEnd;
    Memo.EndUpdate;
    Memo.Modified := False;
    MemoFilename := Filename;
    Result := True;
  except on E:Exception do
    // File does not exist, is locked or broken
    ErrorDialog(E.message + CRLF + CRLF + Filename);
  end;
  Screen.Cursor := crDefault;
end;


procedure TQueryTab.SaveContents(Filename: String; OnlySelection: Boolean);
var
  Text, LB: String;
begin
  Screen.Cursor := crHourGlass;
  MainForm.ShowStatusMsg(_('Saving file ...'));
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


class function TQueryTab.GenerateUid: String;
begin
  // Generate fresh unique id for a new tab
  // Keep it readable by using the date with milliseconds
  DateTimeToString(Result, 'yyyy-mm-dd_hh-nn-ss-zzz', Now);
end;


function TQueryTab.MemoBackupFilename: String;
begin
  // Return filename for auto-backup feature
  if (MemoFilename <> '') and (not Memo.Modified) then begin
    Result := '';
  end else begin
    Result := IncludeTrailingBackslash(AppSettings.DirnameBackups)
      + goodfilename(Format(BACKUP_FILEPATTERN, [Uid]))
      ;
  end;
end;


procedure TQueryTab.BackupUnsavedContent;
var
  LastFileBackup: TDateTime;
begin
  // Fired before closing application, and also timer controlled

  // Check if content is a user stored file and if it has modified content:
  if MemoBackupFilename.IsEmpty then
    Exit;

  // Check if existing backup file is up-to-date:
  if FileExists(MemoBackupFilename) then begin
    FileAge(MemoBackupFilename, LastFileBackup);
    if LastFileBackup > FLastChange then
      Exit;
  end;

  if Memo.GetTextLen = 0 then begin
    // If memo is empty, remove backup file
    if FileExists(MemoBackupFilename) then begin
      if not DeleteFile(MemoBackupFilename) then begin
        MainForm.LogSQL('Could not remove empty backup file "'+MemoBackupFilename+'"', lcError);
      end;
    end;
  end else begin
    if Memo.GetTextLen < SIZE_MB*10 then begin
      MainForm.LogSQL('Saving backup file to "'+MemoBackupFilename+'"...', lcDebug);
      MainForm.ShowStatusMsg(_('Saving backup file...'));
      SaveUnicodeFile(MemoBackupFilename, Memo.Text);
    end else begin
      MainForm.LogSQL('Unsaved tab contents too large (> 10M) for creating a backup.', lcDebug);
    end;
  end;
  MainForm.ShowStatusMsg('');
end;


function TQueryTab.BindParamsActivated: Boolean;
var
  Node: PVirtualNode;
begin
  // Check if bind params checkbox is checked
  Node := FindNode(treeHelpers, HELPERNODE_BINDING, nil);
  Result := treeHelpers.CheckState[Node] in CheckedStates;
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


procedure TQueryTab.SetQueryRunning(Value: Boolean);
begin
  // Marker for query tab that it is currently executing and waiting for a query
  FQueryRunning := Value;
  TimerStatusUpdate.Enabled := Value;
end;


procedure TQueryTab.MemoOnChange(Sender: TObject);
begin
  // Check if bind param detection is enabled for text size <1M
  // Uncheck checkbox if it's bigger
  TimerLastChange.Enabled := False;
  FLastChange := Now;
  TimerLastChange.Enabled := True;
end;


procedure TQueryTab.TimerLastChangeOnTimer(Sender: TObject);
var
  rx: TRegExpr;
  BindParamItem : TBindParamItem;
  Item, ParamCountBefore: Integer;
  Node : PVirtualNode;
  FoundParam : String;
begin
  TimerLastChange.Enabled := False;

  if BindParamsActivated then begin
    if Memo.GetTextLen > SIZE_MB then begin
      MessageDialog(_('The query is too long to enable detection of bind parameters'), mtError, [mbOK]);
      Node := FindNode(treeHelpers, HELPERNODE_BINDING, nil);
      treeHelpers.CheckState[Node] := csUncheckedNormal;
    end else begin
      MainForm.LogSQL('Bind parameter detection...', lcInfo);
      ParamCountBefore := ListBindParams.Count;

      // Check current Query memo to find all parameters with regular expression ( :params )
      rx := TRegExpr.Create;
      // Can't use (?<!\w):\w+ with actuall unit so this is an other solution
      // Don't use ^:\w+|\W:[^\W:]\w+ because it detect IP v6
      rx.Expression := '([^:\w]|^):\w+';
      if rx.Exec(Memo.Text) then while true do begin

        // Don't get first char if it's not ':' because RegEx contain \W (A non-word character) before ':'
        FoundParam := Copy(rx.Match[0], Pos(':',rx.Match[0]), Length(rx.Match[0])-Pos(':',rx.Match[0])+1);

        // Prepare TBindParamItem
        BindParamItem := TBindParamItem.Create;
        BindParamItem.Parameter := FoundParam;
        BindParamItem.Value := '';
        BindParamItem.Keep := True;

        // Check if parameter already exists
        Item := ListBindParams.FindParameter(FoundParam);

        // If exists, seet Keep to true else add TBindParamItem
        if Item <> -1 then
          ListBindParams.Items[Item].Keep := True
        else
          ListBindParams.add(BindParamItem);

        // Try to find next parameter
        if not rx.ExecNext then
          break;
      end;

      // Sort list ascending
      ListBindParams.Sort;
      // Delete all parameters where variable is to False
      ListBindParams.CleanToKeep;

      // Refresh bind param tree node, so it displays its children. Expand it when it has params for the first time.
      MainForm.RefreshHelperNode(HELPERNODE_BINDING);
      if (ParamCountBefore=0) and (ListBindParams.Count>0) then begin
        Node := FindNode(treeHelpers, HELPERNODE_BINDING, nil);
        treeHelpers.Expanded[Node] := True;
      end;

      MainForm.LogSQL(IntToStr(ListBindParams.Count) + ' bind parameters found.', lcDebug);
    end;
  end;
end;


procedure TQueryTab.TimerStatusUpdateOnTimer(Sender: TObject);
var
  Msg, ElapsedMsg: String;
  Elapsed: Int64;
begin
  // Update status bar every second with elapsed time
  Msg := _('query')+' #' + FormatNumber(ExecutionThread.BatchPosition+1);
  if ExecutionThread.QueriesInPacket > 1 then
    Msg := f_('queries #%s to #%s', [FormatNumber(ExecutionThread.BatchPosition+1), FormatNumber(ExecutionThread.BatchPosition+ExecutionThread.QueriesInPacket)]);
  try
    Elapsed := MilliSecondsBetween(ExecutionThread.QueryStartedAt, Now);
    ElapsedMsg := FormatTimeNumber(Elapsed/1000, True);
    MainForm.ShowStatusMsg(ElapsedMsg + ': ' + f_('Executing %s of %s ...', [Msg, FormatNumber(ExecutionThread.Batch.Count)]));
  except;
    // Some crashes here, probably when accessing the no longer running thread.
    // See https://www.heidisql.com/forum.php?t=25418#p25484
    // See issue https://github.com/HeidiSQL/HeidiSQL/issues/490
  end;
end;






{ TResultTab }

constructor TResultTab.Create(AOwner: TQueryTab);
var
  QueryTab: TQueryTab;
  OrgGrid: TVirtualStringTree;
begin
  inherited Create;
  QueryTab := AOwner;
  OrgGrid := Mainform.QueryGrid;
  Grid := TVirtualStringTree.Create(QueryTab.TabSheet);
  Grid.Parent := QueryTab.TabSheet;
  Grid.Tag := OrgGrid.Tag;
  Grid.BorderStyle := OrgGrid.BorderStyle;
  Grid.Align := OrgGrid.Align;
  Grid.Visible := False;
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
  Grid.OnChange := OrgGrid.OnChange;
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
  FixVT(Grid, AppSettings.ReadInt(asGridRowLineCount));
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


{ TQueryHistory }

constructor TQueryHistory.Create(SessionPath: String);
var
  ValueNames: TStringList;
  i, j, p: Integer;
  Raw: String;
  Item: TQueryHistoryItem;
begin
  inherited Create(TQueryHistoryItemComparer.Create, True);
  AppSettings.SessionPath := SessionPath + '\' + REGKEY_QUERYHISTORY;
  ValueNames := AppSettings.GetValueNames;
  for i:=0 to ValueNames.Count-1 do begin
    j := StrToIntDef(ValueNames[i], -1);
    // Prevent from running into serious errors when registry has some non-numeric value
    if j<>-1 then begin
      Item := TQueryHistoryItem.Create;
      try
        Item.RegValue := j;
        Raw := AppSettings.ReadString(ValueNames[i]);
        p := Pos(DELIM, Raw);
        Item.Time := StrToDateTime(Copy(Raw, 1, p-1));
        System.Delete(Raw, 1, p);
        p := Pos(DELIM, Raw);
        Item.Database := Copy(Raw, 1, p-1);
        System.Delete(Raw, 1, p);
        p := Pos(DELIM, Raw);
        Item.Duration := StrToIntDef(Copy(Raw, 1, p-1), 0);
        FMaxDuration := Max(FMaxDuration, Item.Duration);
        Item.SQL := Copy(Raw, p+1, Length(Raw));
        Add(Item);
      except
        on E:Exception do begin
          MainForm.LogSQL(E.ClassName+': '+E.Message+'  Sessionpath: '+AppSettings.SessionPath+'  Valuename: '+ValueNames[i]+'  Raw: '+Raw, lcDebug);
          Item.Free;
          Break;
        end;
      end;
    end;
  end;
  // Sort by date
  Sort;
  ValueNames.Free;
  AppSettings.ResetPath;
end;


function TQueryHistoryItemComparer.Compare(const Left, Right: TQueryHistoryItem): Integer;
begin
  // Simple sort method for a TDBObjectList
  if Left.Time > Right.Time then
    Result := -1
  else if Left.Time = Right.Time then
    Result := 0
  else
    Result := 1;
end;


{ TBindParam }


procedure TBindParam.CleanToKeep;
var
  BindParamItem: Integer;
begin
  // Check for each parameter if there is to be deleted
  for BindParamItem := self.Count - 1 downto 0 do begin
    if self.Items[BindParamItem].Keep = False then
      Self.Delete(BindParamItem)
  end;

  // Set False all items for the next call
  for BindParamItem := 0 to self.Count - 1 do
      self.Items[BindParamItem].Keep := False;
end;


constructor TBindParam.Create;
begin
  inherited Create(TBindParamItemComparer.Create);
end;


function TBindParam.FindParameter(Param: String): Integer;
var
  ParamItem: Integer;
begin
  // Return Item index (if found) or -1
  Result := -1;
  for ParamItem := 0 to self.Count-1 do begin
    if StrComp( PChar(Param) , PChar(Self.Items[ParamItem].Parameter))= 0 then begin
      Result := ParamItem;
      Break;
    end;
  end;
end;


{ TBindParamItemComparer }


function TBindParamItemComparer.Compare(const Left,
  Right: TBindParamItem): Integer;
var
  CompResult: Integer;
begin
  // Simple sort method for a TDBObjectList
  CompResult :=   CompareText(Left.Parameter,Right.Parameter);
  if CompResult < 0 then
    Result := -1
  else if CompResult = 0 then
    Result := 0
  else
    Result := 1;
end;

end.

