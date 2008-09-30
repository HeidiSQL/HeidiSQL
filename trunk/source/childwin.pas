unit Childwin;


// -------------------------------------
// MDI-Child-Window
// -------------------------------------


interface


uses
  Synchronization,
  Contnrs,
  Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, ImgList, SysUtils, Dialogs, Menus,
  SynEdit, SynMemo, SynEditHighlighter, SynHighlighterSQL, SynEditSearch,
  SynEditTypes, Clipbrd,
  Buttons, CheckLst, ToolWin, Db,
  helpers,
  ZDataset,
  ZAbstractRODataset, ZConnection,
  ZSqlMonitor, ZDbcLogging,
  SynCompletionProposal, HeidiComp, SynEditMiscClasses, MysqlQuery,
  MysqlQueryThread, queryprogress, communication, MysqlConn, Tabs,
  VirtualTrees, createdatabase, tbl_properties, createtable,
  SynRegExpr, EditVar, PngSpeedButton, WideStrings, WideStrUtils, SynUnicode,
  TntStdCtrls;

type
  TOrderCol = class(TObject)
    ColumnName: WideString;
    SortDirection: Byte;
  end;
  TOrderColArray = Array of TOrderCol;

type
  TMDIChild = class(TForm)
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
    menudroptable: TMenuItem;
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
    menurefresh: TMenuItem;
    N2: TMenuItem;
    pnlQueryMemo: TPanel;
    SynSQLSyn1: TSynSQLSyn;
    SynMemoQuery: TSynMemo;
    spltQuery: TSplitter;
    menucreatetable: TMenuItem;
    OpenDialog1: TOpenDialog;
    TimerHostUptime: TTimer;
    popupTableGrid: TPopupMenu;
    Refresh2: TMenuItem;
    DropField1: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    PopupmenuDropDatabase: TMenuItem;
    popupDataGrid: TPopupMenu;
    Refresh3: TMenuItem;
    MenuAddField: TMenuItem;
    MenuEditField: TMenuItem;
    popupResultGrid: TPopupMenu;
    Copyrecords1: TMenuItem;
    CopyasCSVData1: TMenuItem;
    N9: TMenuItem;
    LabelResultinfo: TLabel;
    menuAlterTable: TMenuItem;
    N10: TMenuItem;
    MenuRenameTable: TMenuItem;
    TimerConnected: TTimer;
    N12: TMenuItem;
    popupSqlLog: TPopupMenu;
    Clear2: TMenuItem;
    Copy1: TMenuItem;
    N13: TMenuItem;
    EditQuery1: TMenuItem;
    Markall3: TMenuItem;
    N15: TMenuItem;
    menuMaintenance: TMenuItem;
    TimerConnectErrorCloseWindow: TTimer;
    PopupMenuDropTable: TMenuItem;
    N17: TMenuItem;
    ListColumns: TVirtualStringTree;
    CopycontentsasHTML1: TMenuItem;
    CopycontentsasHTML2: TMenuItem;
    Copy3: TMenuItem;
    Paste2: TMenuItem;
    N4: TMenuItem;
    DataGrid: TVirtualStringTree;
    QueryGrid: TVirtualStringTree;
    Copytableas1: TMenuItem;
    Delete1: TMenuItem;
    N6: TMenuItem;
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
    N1: TMenuItem;
    MenuCopyTable: TMenuItem;
    SynMemoFilter: TSynMemo;
    N18: TMenuItem;
    selectall1: TMenuItem;
    MenuAutoupdate: TMenuItem;
    TimerHost: TTimer;
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
    CopyasXMLdata1: TMenuItem;
    CopyasXMLdata2: TMenuItem;
    Exportdata1: TMenuItem;
    Exportdata2: TMenuItem;
    SaveDialogExportData: TSaveDialog;
    N11: TMenuItem;
    ProgressBarQuery: TProgressBar;
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
    InsertfilesintoBLOBfields1: TMenuItem;
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
    menuSQLhelp: TMenuItem;
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
    pnlFilterVariables: TPanel;
    lblFilterVariables: TLabel;
    editFilterVariables: TEdit;
    pnlFilterStatus: TPanel;
    lblFilterStatus: TLabel;
    editFilterStatus: TEdit;
    pnlFilterProcesses: TPanel;
    lblFilterProcesses: TLabel;
    editFilterProcesses: TEdit;
    menuEditVariable: TMenuItem;
    actView1: TMenuItem;
    Createview1: TMenuItem;
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
    procedure controlsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
      const Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1CodeCompletion(Sender: TObject;
      var Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1Execute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
      var CanExecute: Boolean);
    procedure PerformConnect;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
    procedure EnsureFullWidth(Grid: TBaseVirtualTree; Column: TColumnIndex; Node: PVirtualNode);
    procedure EnsureNodeLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode; WhereClause: String);
    procedure EnsureChunkLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DiscardNodeData(Sender: TVirtualStringTree; Node: PVirtualNode);
    procedure viewdata(Sender: TObject);
    procedure RefreshFieldListClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
    procedure LogSQL(msg: WideString = ''; comment: Boolean = true );
    procedure ShowVariablesAndProcesses(Sender: TObject);
    procedure KillProcess(Sender: TObject);
    procedure PageControlHostChange(Sender: TObject);
    procedure ExecSQLClick(Sender: TObject; Selection: Boolean = false;
      CurrentLine: Boolean=false);
    procedure SynMemoQueryStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure TimerHostUptimeTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListTablesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
    procedure MenuRenameTableClick(Sender: TObject);
    procedure TimerConnectedTimer(Sender: TObject);
    procedure Clear2Click(Sender: TObject);
    procedure EditQuery1Click(Sender: TObject);
    procedure Markall3Click(Sender: TObject);
    procedure ReadWindowOptions;
    procedure ListTablesDblClick(Sender: TObject);
    procedure TimerConnectErrorCloseWindowTimer(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    function GetFilter: WideString;
    procedure SaveFilter(Clause: WideString = '');
    procedure DropFilter1Click(Sender: TObject);
    procedure selectall1Click(Sender: TObject);
    procedure popupResultGridPopup(Sender: TObject);
    procedure Autoupdate1Click(Sender: TObject);
    procedure EnableAutoRefreshClick(Sender: TObject);
    procedure ShowProcessList(sender: TObject);
    procedure DisableAutoRefreshClick(Sender: TObject);
    procedure SynMemoQueryDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoQueryDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TWideStrings);
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
    function mask(str: WideString) : WideString;
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
    procedure vstBeforePaint(Sender: TBaseVirtualTree; TargetCanvas:
        TCanvas);
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
    procedure ListProcessesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure editFilterVTChange(Sender: TObject);
    procedure ListColumnsDblClick(Sender: TObject);
    procedure ListVariablesDblClick(Sender: TObject);
    procedure menuEditVariableClick(Sender: TObject);
    procedure menuRefreshDBTreeClick(Sender: TObject);
    procedure menuTreeCollapseAllClick(Sender: TObject);
    procedure menuTreeExpandAllClick(Sender: TObject);
    procedure SynMemoFilterChange(Sender: TObject);
    procedure tabsetQueryHelpersGetImageIndex(Sender: TObject; TabIndex: Integer;
        var ImageIndex: Integer);
    procedure DataGridAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure menuShowSizeColumnClick(Sender: TObject);
    procedure DataGridColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure DBtreeClick(Sender: TObject);
    procedure GridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

    private
      uptime                     : Integer;
      time_connected             : Cardinal;
      viewingdata                : Boolean;
      FMysqlConn                 : TMysqlConn;
      FConn                      : TOpenConnProf;
      QueryRunningInterlock      : Integer;
      lastUsedDB                 : String;
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
      ViewDataPrevTable          : WideString;
      PrevTableColWidths         : WideStrings.TWideStringList;
      DataGridHasChanges         : Boolean;

      function GetQueryRunning: Boolean;
      procedure SetQueryRunning(running: Boolean);
      function GetActiveGrid: TVirtualStringTree;
      function GetActiveData: PGridResult;
      procedure WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery; ForceDialog: Boolean);
      function RunThreadedQuery(AQuery: WideString; ForceDialog: Boolean): TMysqlQuery;
      procedure DisplayRowCountStats;
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

    public
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
      prefEnableTextEditor,
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
      DataGridCurrentSelect      : WideString;
      DataGridCurrentFilter      : WideString;
      DataGridCurrentSort        : WideString;


      procedure Init(AConn : POpenConnProf; AMysqlConn : TMysqlConn);
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
      function HandleOrderColumns( AddOrderCol: TOrderCol = nil ): TOrderColArray;
      function ComposeOrderClause( Cols: TOrderColArray ): WideString;
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
  end;

type
  // Represents errors already "handled" (shown to user),
  // which can thus safely be ignored.
  THandledSQLError = class(Exception)
  end;


// -----------------------------------------------------------------------------
implementation


uses
  Main, fieldeditor,
  copytable, sqlhelp, printlist,
  column_selection, data_sorting, runsqlfile, mysql_structures,
  Registry, grideditlinks;


type
  PMethod = ^TMethod;


{$I const.inc}


{$R *.DFM}


function TMDIChild.CreateOrGetRemoteQueryTab(sender: THandle): THandle;
begin
  // Should create a tab for commands from another window,
  // or return a handle to an existing tab if one already exists for that window.
  //
  // TODO: Implement this when multiple tabs are implemented.
  //       Return a tab's handle instead of the childwin's handle.
  result := Self.Handle;
end;


procedure TMDIChild.PerformConnect;
var
  v           : String[50];
  v1, v2, v3  : String;
  rx : TRegExpr;
begin
  try
    time_connected := 0;
    TimerConnected.Enabled := true;
    LogSQL( 'Connection established with host "' + FMysqlConn.Connection.hostname +
      '" on port ' + IntToStr(FMysqlConn.Connection.Port) );
    LogSQL( 'Connection-ID: ' + IntToStr( MySQLConn.Connection.GetThreadId ) );

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
    Mainform.showstatus('MySQL '+v1+'.'+v2+'.'+v3, 2);

    // On Re-Connection, try to restore lost properties
    if FMysqlConn.Connection.Database <> '' then
      ExecUseQuery( FMysqlConn.Connection.Database );
  except
    on E: Exception do begin
      LogSQL( E.Message, true );
      Screen.Cursor := crDefault;
      MessageDlg( E.Message, mtError, [mbOK], 0 );
      raise;
    end;
  end;
end;


function TMDIChild.GetQueryRunning: Boolean;
begin
  Result := ( QueryRunningInterlock = 1 );
end;


procedure TMDIChild.SetQueryRunning(running: Boolean);
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


procedure TMDIChild.Init(AConn : POpenConnProf; AMysqlConn : TMysqlConn);
var
  AutoReconnect    : Boolean;
  i, j             : Integer;
  miGroup,
  miFilterGroup,
  miFunction,
  miFilterFunction : TMenuItem;
  functioncats     : TStringList;
  reg              : TRegistry;
begin
  QueryRunningInterlock := 0;
  UserQueryFired := False;
  UserQueryFiring := False;
  TemporaryDatabase := '';
  CachedTableLists := WideStrings.TWideStringList.Create;
  InitializeCriticalSection(SqlMessagesLock);
  EnterCriticalSection(SqlMessagesLock);
  SqlMessages := TWideStringList.Create;
  LeaveCriticalSection(SqlMessagesLock);

  FConn := AConn^;
  FMysqlConn := AMysqlConn; // we're now responsible to free it

  FConn.MysqlConn := FMysqlConn.Connection; // use this connection (instead of zConn)

  // Initialization: establish connection and read some vars from registry
  MainForm.Showstatus( 'Creating window...' );

  // Temporarily disable AutoReconnect in Registry
  // in case of unexpected application-termination
  AutoReconnect := Mainform.GetRegValue(REGNAME_AUTORECONNECT, DEFAULT_AUTORECONNECT);
  if AutoReconnect then begin
    reg := TRegistry.Create();
    reg.OpenKey( REGPATH, true );
    reg.WriteBool( REGNAME_AUTORECONNECT, False );
    reg.CloseKey;
    FreeAndNil(reg);
  end;

  ReadWindowOptions();

  MainForm.Showstatus( 'Connecting to ' + FConn.MysqlParams.Host + '...' );

  try
    PerformConnect();
  except
    TimerConnectErrorCloseWindow.Enabled := true;
    Exit;
  end;

  SessionName := FMysqlConn.SessionName;
  DatabasesWanted := explode(';', FConn.DatabaseList);
  if FConn.DatabaseListSort then
    DatabasesWanted.Sort;

  // Fill variables-list, processlist and DB-tree
  ShowVariablesAndProcesses( Self );
  // Invoke population of database tree. It's important to do this here after
  // having filled DatabasesWanted, not at design time.
  DBtree.RootNodeCount := 1;

  // Re-enable AutoReconnect in Registry!
  if AutoReconnect then begin
    reg := TRegistry.Create;
    reg.OpenKey( REGPATH, true );
    reg.WriteBool( REGNAME_AUTORECONNECT, true );
    reg.CloseKey;
    FreeAndNil(reg);
  end;

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
  if MainForm.GetRegValue( REGNAME_RESTORELASTUSEDDB, DEFAULT_RESTORELASTUSEDDB ) and ( lastUsedDB <> '' ) then
  begin
    try
      ActiveDatabase := Utf8Decode(lastUsedDB);
    except
      // Suppress exception message when db was dropped externally or
      // the session was just opened with "OnlyDBs" in place and the
      // last db is not contained in this list.
    end;
  end else // By default, select the host node
    DBtree.Selected[DBtree.GetFirst] := true;

  // read function-list into menu
  functioncats := GetFunctionCategories;

  for i:=0 to functioncats.Count-1 do
  begin
    // Create a menu item which gets subitems later
    miGroup := TMenuItem.Create(popupQuery);
    miGroup.Caption := functioncats[i];
    popupQuery.Items.add(miGroup);
    miFilterGroup := TMenuItem.Create(popupFilter);
    miFilterGroup.Caption := miGroup.Caption;
    popupFilter.Items.add(miFilterGroup);
    for j:=0 to Length(MySqlFunctions)-1 do
    begin
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
      if MySqlFunctions[j].Version <= mysql_version then
      begin
        if MySqlFunctions[j].Description <> '' then
          miFunction.Hint := miFunction.Hint + ' - ' + Copy(MySqlFunctions[j].Description, 0, 200 );
        miFunction.Tag := j;
        // Place menuitem on menu
        miFunction.OnClick := insertFunction;
      end
      else
      begin
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


procedure TMDIChild.ReadWindowOptions;
var
  i           : Integer;
  menuitem    : Tmenuitem;
  fontname, datafontname : String;
  fontsize, datafontsize : Integer;
begin
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
  // Other values:
  pnlQueryMemo.Height := Mainform.GetRegValue(REGNAME_QUERYMEMOHEIGHT, pnlQueryMemo.Height);
  pnlQueryHelpers.Width := Mainform.GetRegValue(REGNAME_QUERYHELPERSWIDTH, pnlQueryHelpers.Width);
  DBtree.Width := Mainform.GetRegValue(REGNAME_DBTREEWIDTH, DBtree.Width);
  SynMemoSQLLog.Height := Mainform.GetRegValue(REGNAME_SQLOUTHEIGHT, SynMemoSQLLog.Height);
  prefMaxColWidth := Mainform.GetRegValue(REGNAME_MAXCOLWIDTH, DEFAULT_MAXCOLWIDTH);
  // Fix registry entry from older versions which can have 0 here which makes no sense
  // since the autosetting was removed
  if prefMaxColWidth <= 0 then
    prefMaxColWidth := DEFAULT_MAXCOLWIDTH;
  prefLogsqlnum := Mainform.GetRegValue(REGNAME_LOGSQLNUM, DEFAULT_LOGSQLNUM);
  prefLogSqlWidth := Mainform.GetRegValue(REGNAME_LOGSQLWIDTH, DEFAULT_LOGSQLWIDTH);
  prefCSVSeparator := Mainform.GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  prefCSVEncloser := Mainform.GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  prefCSVTerminator := Mainform.GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  prefRememberFilters := Mainform.GetRegValue(REGNAME_REMEMBERFILTERS, DEFAULT_REMEMBERFILTERS);
  prefPreferShowTables := Mainform.GetRegValue(REGNAME_PREFER_SHOWTABLES, DEFAULT_PREFER_SHOWTABLES);

  // SQL-Font:
  fontname := Mainform.GetRegValue(REGNAME_FONTNAME, DEFAULT_FONTNAME);
  fontsize := Mainform.GetRegValue(REGNAME_FONTSIZE, DEFAULT_FONTSIZE);
  SynMemoQuery.Font.Name := fontname;
  SynMemoQuery.Font.Size := fontsize;
  SynMemoFilter.Font.Name := fontname;
  SynMemoFilter.Font.Size := fontsize;
  SynMemoSQLLog.Font.Name := fontname;
  SynMemoSQLLog.Font.Size := fontsize;
  SynMemoProcessView.Font.Name := fontname;
  SynMemoProcessView.Font.Size := fontsize;

  // Data-Font:
  datafontname := Mainform.GetRegValue(REGNAME_DATAFONTNAME, DEFAULT_DATAFONTNAME);
  datafontsize := Mainform.GetRegValue(REGNAME_DATAFONTSIZE, DEFAULT_DATAFONTSIZE);
  DataGrid.Font.Name := datafontname;
  QueryGrid.Font.Name := datafontname;
  DataGrid.Font.Size := datafontsize;
  QueryGrid.Font.Size := datafontsize;
  FixVT(DataGrid);
  FixVT(QueryGrid);
  // Load color settings
  prefFieldColorNumeric := Mainform.GetRegValue(REGNAME_FIELDCOLOR_NUMERIC, DEFAULT_FIELDCOLOR_NUMERIC);
  prefFieldColorText := Mainform.GetRegValue(REGNAME_FIELDCOLOR_TEXT, DEFAULT_FIELDCOLOR_TEXT);
  prefFieldColorBinary := Mainform.GetRegValue(REGNAME_FIELDCOLOR_BINARY, DEFAULT_FIELDCOLOR_BINARY);
  prefFieldColorDatetime := Mainform.GetRegValue(REGNAME_FIELDCOLOR_DATETIME, DEFAULT_FIELDCOLOR_DATETIME);
  prefFieldColorEnum := Mainform.GetRegValue(REGNAME_FIELDCOLOR_ENUM, DEFAULT_FIELDCOLOR_ENUM);
  prefFieldColorSet := Mainform.GetRegValue(REGNAME_FIELDCOLOR_SET, DEFAULT_FIELDCOLOR_SET);
  prefNullBG := Mainform.GetRegValue(REGNAME_BG_NULL, DEFAULT_BG_NULL);
  CalcNullColors;
  // Editor enablings
  prefEnableTextEditor := Mainform.GetRegValue(REGNAME_FIELDEDITOR_TEXT, DEFAULT_FIELDEDITOR_TEXT);
  prefEnableBinaryEditor := Mainform.GetRegValue(REGNAME_FIELDEDITOR_BINARY, DEFAULT_FIELDEDITOR_BINARY);
  prefEnableDatetimeEditor := Mainform.GetRegValue(REGNAME_FIELDEDITOR_DATETIME, DEFAULT_FIELDEDITOR_DATETIME);
  prefEnableEnumEditor := Mainform.GetRegValue(REGNAME_FIELDEDITOR_ENUM, DEFAULT_FIELDEDITOR_ENUM);
  prefEnableSetEditor := Mainform.GetRegValue(REGNAME_FIELDEDITOR_SET, DEFAULT_FIELDEDITOR_SET);
  prefEnableNullBG := Mainform.GetRegValue(REGNAME_BG_NULL_ENABLED, DEFAULT_BG_NULL_ENABLED);

  // Color coding:
  SynSQLSyn1.KeyAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLKEYATTRI, ColorToString(DEFAULT_SQLCOLKEYATTRI)));
  SynSQLSyn1.FunctionAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLFUNCTIONATTRI, ColorToString(DEFAULT_SQLCOLFUNCTIONATTRI)));
  SynSQLSyn1.DataTypeAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLDATATYPEATTRI, ColorToString(DEFAULT_SQLCOLDATATYPEATTRI)));
  SynSQLSyn1.NumberAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLNUMBERATTRI, ColorToString(DEFAULT_SQLCOLNUMBERATTRI)));
  SynSQLSyn1.StringAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLSTRINGATTRI, ColorToString(DEFAULT_SQLCOLSTRINGATTRI)));
  SynSQLSyn1.CommentAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLCOMMENTATTRI, ColorToString(DEFAULT_SQLCOLCOMMENTATTRI)));
  SynSQLSyn1.TablenameAttri.Foreground := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLTABLENAMEATTRI, ColorToString(DEFAULT_SQLCOLTABLENAMEATTRI)));
  SynMemoQuery.ActiveLineColor := StringToColor(Mainform.GetRegValue(REGNAME_SQLCOLACTIVELINE, ColorToString(DEFAULT_SQLCOLACTIVELINE)));

  // Switch off/on displaying table/db sized in tree
  menuShowSizeColumn.Checked := Mainform.GetRegValue(REGNAME_SIZECOL_TREE, DEFAULT_SIZECOL_TREE);
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

  // Activate logging
  if Mainform.GetRegValue(REGNAME_LOGTOFILE, DEFAULT_LOGTOFILE) then
    ActivateFileLogging;

  // Set last used database, select it later in Init
  lastUsedDB := Mainform.GetRegValue(REGNAME_LASTUSEDDB, '', FConn.Description);

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
end;


procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
var
  reg   : TRegistry;
begin
  // Post pending UPDATE
  if DataGridHasChanges then
    Mainform.actDataPostChangesExecute(Sender);

  SetWindowConnected( false );
  SetWindowName( main.discname );
  Application.Title := APPNAME;

  debug('mem: clearing query and browse data.');
  SetLength(FDataGridResult.Rows, 0);
  SetLength(FDataGridResult.Columns, 0);
  SetLength(FQueryGridResult.Rows, 0);
  SetLength(FQueryGridResult.Columns, 0);

  // Closing connection
  FMysqlConn.Disconnect;
  FreeAndNil(FMysqlConn);

  EnterCriticalSection(SqlMessagesLock);
  FreeAndNil(SqlMessages);
  LeaveCriticalSection(SqlMessagesLock);

  reg := TRegistry.Create();
  if reg.OpenKey( REGPATH, true ) then
  begin
    WindowState := wsMaximized;

    reg.WriteInteger( REGNAME_QUERYMEMOHEIGHT, pnlQueryMemo.Height );
    reg.WriteInteger( REGNAME_QUERYHELPERSWIDTH, pnlQueryHelpers.Width );
    reg.WriteInteger( REGNAME_DBTREEWIDTH, DBtree.width );
    reg.WriteInteger( REGNAME_SQLOUTHEIGHT, SynMemoSQLLog.Height );

    // Save width of probably resized columns of all VirtualTrees
    SaveListSetup(ListVariables);
    SaveListSetup(ListStatus);
    SaveListSetup(ListProcesses);
    SaveListSetup(ListCommandStats);
    SaveListSetup(ListTables);
    SaveListSetup(ListColumns);

    // Open server-specific registry-folder.
    // relative from already opened folder!
    reg.OpenKey( REGKEY_SESSIONS + FConn.Description, true );
    reg.WriteString( REGNAME_LASTUSEDDB, Utf8Encode(ActiveDatabase) );
  end;
  FreeAndNil(reg);

  // Clear database and table lists
  DBtree.Clear;
  ClearAllTableLists;
  FreeAndNil(DatabasesWanted);
  FreeAndNil(Databases);
  FreeAndNil(CachedTableLists);

  ValidateControls(False);
  Action := caFree;

  SetWindowConnected( false );
  SetWindowName( main.discname );
  Application.Title := APPNAME;

  if prefLogToFile then
    DeactivateFileLogging;
end;


{**
  Add a SQL-command or comment to SynMemoSQLLog
}
procedure TMDIChild.LogSQL(msg: WideString = ''; comment: Boolean = true);
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
  PostMessage(MainForm.Handle, WM_PROCESSLOG, 0, 0);
end;

procedure TMDIChild.ProcessSqlLog;
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
procedure TMDIChild.TrimSQLLog;
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


procedure TMDIChild.ShowHost;
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


procedure TMDIChild.ShowDatabase(db: WideString);
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
procedure TMDIChild.ShowTable(table: WideString; tab: TTabSheet = nil);
begin
  if tab = nil then tab := tabTable; // Alternative default: tabData
  if tab = tabTable then ShowTableProperties;
  if tab = tabData then ShowTableData( table );
  Caption := winName + ' - /' + ActiveDatabase + '/' + SelectedTable;
end;

procedure TMDIChild.viewdata(Sender: TObject);
var
  sorting              : WideString;
  i, count             : Integer;
  OrderColumns         : TOrderColArray;
  reg_value            : String;
  select_base          : WideString;
  select_from          : WideString;
  sl_query             : TWideStringList;
  DisplayedColumnsList,
  HiddenKeyCols,
  KeyCols              : WideStrings.TWideStringList;
  Filter, ColName      : WideString;
  col                  : TVirtualTreeColumn;
  rx                   : TRegExpr;
  ColType              : String;

procedure InitColumn(idx: Integer; name: WideString);
var
  ColType: String;
  k: Integer;
begin
  FDataGridResult.Columns[idx].Name := name;
  col := DataGrid.Header.Columns.Add;
  col.Text := name;
  col.Options := col.Options + [coSmartResize];
  if HiddenKeyCols.IndexOf(name) > -1 then col.Options := col.Options - [coVisible];
  // Sorting color and title image
  for k:=0 to Length(OrderColumns)-1 do begin
    if OrderColumns[k].ColumnName = name then begin
      case OrderColumns[k].SortDirection of
        ORDER_ASC:  begin col.Color := COLOR_SORTCOLUMN_ASC;  col.ImageIndex := 109; end;
        ORDER_DESC: begin col.Color := COLOR_SORTCOLUMN_DESC; col.ImageIndex := 110; end;
      end;
    end;
  end;
  // Right alignment for numeric columns
  FSelectedTableColumns.First;
  while not FSelectedTableColumns.Eof do begin
    if FSelectedTableColumns.FieldByName('Field').AsWideString = name then begin
      ColType := FSelectedTableColumns.FieldByName('Type').AsString;
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
    end;
    FSelectedTableColumns.Next;
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
  // Post pending UPDATE
  if DataGridHasChanges then
    Mainform.actDataPostChangesExecute(Sender);
  viewingdata := true;
  sl_query := TWideStringList.Create();
  try
    // Read cached ORDER-clause and set Grid.Sortcolumns
    sorting := '';
    OrderColumns := HandleOrderColumns;
    if Length(OrderColumns) > 0 then begin
      sorting := ComposeOrderClause(OrderColumns);
      // Signal for the user that we applied an ORDER-clause
      tbtnDataSorting.ImageIndex := 108;
    end else
      tbtnDataSorting.ImageIndex := 107;

    if (SelectedTable <> '') and (ActiveDatabase <> '') then begin
      // Ensure <Table> and <Data> are visible
      tabTable.TabVisible := true;
      tabData.TabVisible := true;
      // Switch to <Data>
      PageControlMain.ActivePage := tabData;

      // Read columns to display from registry
      reg_value := Mainform.GetRegValue(REGNAME_DISPLAYEDCOLUMNS + '_' + ActiveDatabase + '.' + SelectedTable, '', SessionName);
      DisplayedColumnsList := WideStrings.TWideStringlist.Create;
      DisplayedColumnsList.Delimiter := '`';
      DisplayedColumnsList.DelimitedText := reg_value;
      HiddenKeyCols := WideStrings.TWideStringlist.Create;

      SynMemoFilter.Color := clWindow;
      rx := TRegExpr.Create;
      MainForm.ShowStatus('Freeing data...');
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
      // Try to calc the rowcount regardless of a given LIMIT
      // Only needed if the user specified a WHERE-clause
      Filter := GetFilter;
      // Selected columns
      if DisplayedColumnsList.Count = 0 then begin
        FSelectedTableColumns.First;
        while not FSelectedTableColumns.Eof do begin
          DisplayedColumnsList.Add(FSelectedTableColumns.FieldByName('Field').AsWideString);
          FSelectedTableColumns.Next;
        end;
        tbtnDataColumns.ImageIndex := 107;
      end else begin
        // Signal for the user that we now hide some columns
        tbtnDataColumns.ImageIndex := 108;
      end;
      // Ensure key columns are included to enable editing
      KeyCols := GetKeyColumns;
      for i := 0 to KeyCols.Count - 1 do
      if DisplayedColumnsList.IndexOf(KeyCols[i]) = -1 then begin
        DisplayedColumnsList.Add(KeyCols[i]);
        HiddenKeyCols.Add(KeyCols[i]);
      end;
      // Initialize column array to correct length.
      debug('mem: initializing browse columns.');
      SetLength(FDataGridResult.Columns, DisplayedColumnsList.Count);
      for i := 0 to DisplayedColumnsList.Count - 1 do begin
        ColName := DisplayedColumnsList[i];
        FSelectedTableColumns.First;
        while not FSelectedTableColumns.Eof do begin
          if FSelectedTableColumns.FieldByName('Field').AsWideString = ColName then begin
            ColType := FSelectedTableColumns.FieldByName('Type').AsString;
            rx.Expression := '^((tiny|medium|long)?(text|blob)|(var)?(char|binary))\b(\(\d+\))?';
            if rx.Exec(ColType) then begin
              select_base := select_base + ' ' + 'LEFT(' + Mask(ColName) + ', ' + IntToStr(GridMaxData) + ')' + ',';
            end else begin
              select_base := select_base + ' ' + Mask(ColName) + ',';
            end;
            Break;
          end;
          FSelectedTableColumns.Next;
        end;
      end;
      for i := 0 to DisplayedColumnsList.Count - 1 do begin
        ColName := DisplayedColumnsList[i];
        InitColumn(i, ColName);
      end;
      debug('mem: browse column initialization complete.');
      // Cut last comma
      select_base := copy( select_base, 1, Length(select_base)-1 );
      select_from := ' FROM ' + mask( SelectedTable );

      try
        MainForm.ShowStatus('Counting rows...');
        sl_query.Add('SELECT COUNT(*)');
        sl_query.Add(select_from);
        // Apply custom WHERE filter
        if Filter <> '' then sl_query.Add('WHERE ' + Filter);
        count := StrToInt(GetVar(sl_query.Text));
      except
        on E:Exception do begin
          // Most likely we have a wrong filter-clause when this happens
          // Put the user with his nose onto the wrong filter
          // either specified by user or
          // created by HeidiSQL by using the search box
          ToggleFilterPanel(True);
          SynMemoFilter.Color := $008080FF; // light pink
          DataGrid.Header.Options := DataGrid.Header.Options - [hoVisible];
          MessageDlg( E.Message, mtError, [mbOK], 0 );
          raise;
        end;
      end;
      MainForm.ShowStatus( STATUS_MSG_READY );

      DataGridCurrentSelect := select_base + select_from;
      DataGridCurrentFilter := Filter;
      DataGridCurrentSort := sorting;

      debug('mem: initializing browse rows (internal data).');
      SetLength(FDataGridResult.Rows, count);
      for i := 0 to count - 1 do begin
        FDataGridResult.Rows[i].Loaded := False;
      end;
      debug('mem: initializing browse rows (grid).');
      DataGrid.RootNodeCount := count;
      debug('mem: browse row initialization complete.');

      // Switched to another table
      if ViewDataPrevTable <> SelectedTable then begin
        DataGrid.OffsetXY := Point(0, 0); // Scroll to top left
        FreeAndNil(PrevTableColWidths); // Throw away remembered, manually resized column widths
      end;
      DisplayRowCountStats;
      dataselected := true;

      pcChange(self);
    end;
  finally
    DataGrid.Header.Columns.EndUpdate;
    DataGrid.EndUpdate;
    FreeAndNil(sl_query);
    AutoCalcColWidths(DataGrid, PrevTableColWidths);
    viewingdata := false;
    Screen.Cursor := crDefault;
  end;
  ViewDataPrevTable := SelectedTable;
end;


{***
  Calculate + display total rowcount and found rows matching to filter
  in data-tab
}
procedure TMDIChild.DisplayRowCountStats;
var
  rows_matching    : Int64; // rows matching to where-filter
  rows_total       : Int64; // total rowcount
  filter           : WideString;
begin
  lblDataTop.Caption := ActiveDatabase + '.' + SelectedTable + ': ';

  Filter := GetFilter;
  if GetSelectedNodeType = NODETYPE_TABLE then begin
    if Filter <> '' then begin
      // Get rowcount from table
      rows_total := StrToInt64( GetVar( 'SELECT COUNT(*) FROM ' + mask( SelectedTable ), 0 ) );
    end else begin
      rows_total := DataGrid.RootNodeCount
    end;
    lblDataTop.Caption := lblDataTop.Caption + FormatNumber( rows_total ) + ' rows total';
  end else begin
    // Don't fetch rowcount from views to fix bug #1844952
    rows_total := -1;
    lblDataTop.Caption := lblDataTop.Caption + ' [View]';
  end;

  rows_matching := DataGrid.RootNodeCount;

  if( rows_matching <> rows_total ) and
    (Filter <> '') then
    lblDataTop.Caption := lblDataTop.Caption + ', ' + FormatNumber(rows_matching) + ' matching to filter';

  if ( rows_matching = rows_total ) and
    (Filter <> '') then
    lblDataTop.Caption := lblDataTop.Caption + ', filter matches all rows';
end;


procedure TMDIChild.WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery; ForceDialog: Boolean);
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
procedure TMDIChild.pcChange(Sender: TObject);
begin
  // Load data.
  // Do this only if the user clicked the new tab. Not on automatic tab changes.
  if Sender = PageControlMain then begin
    if (PageControlMain.ActivePage = tabData) then viewdata(Sender);
  end;

  // Move focus to relevant controls in order for them to receive keyboard events.
  // Do this only if the user clicked the new tab. Not on automatic tab changes.
  if Sender = PageControlMain then begin
    if PageControlMain.ActivePage = tabDatabase then ListTables.SetFocus;
    if PageControlMain.ActivePage = tabTable then ListColumns.SetFocus;
    if PageControlMain.ActivePage = tabData then DataGrid.SetFocus;
    if PageControlMain.ActivePage = tabQuery then SynMemoQuery.SetFocus;
  end;

  // Ensure controls are in a valid state
  ValidateControls;

  // Show processlist if it's visible now but empty yet
  if PageControlMain.ActivePage = tabHost then begin
    if ListProcesses.RootNodeCount = 0 then
      ShowProcessList( self );
  end;
end;


{***
  Ensures that we're connected to the currently selected database.
}
procedure TMDIChild.EnsureDatabase;
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
function TMDIChild.FetchActiveDbTableList: TDataSet;
begin
  Result := FetchDbTableList(ActiveDatabase);
end;

function TMDIChild.FetchDbTableList(db: WideString): TDataSet;
var
  ds: TDataSet;
  OldCursor: TCursor;
begin
  if not DbTableListCached(db) then begin
    // Not in cache, load table list.
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    MainForm.ShowStatus('Fetching tables from "' + db + '" ...');
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
      MainForm.ShowStatus(STATUS_MSG_READY);
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
function TMDIChild.RefreshActiveDbTableList: TDataSet;
begin
  Result := RefreshDbTableList(ActiveDatabase);
end;

function TMDIChild.RefreshDbTableList(db: WideString): TDataSet;
begin
  ClearDbTableList(db);
  Result := FetchDbTableList(db);
end;

procedure TMDIChild.ClearDbTableList(db: WideString);
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
procedure TMDIChild.ClearAllTableLists;
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
function TMDIChild.FieldContent(ds: TDataSet; ColName: WideString): WideString;
begin
  Result := '';
  if
    (ds.FindField(colName) <> nil) and
    (not ds.FindField(ColName).IsNull)
  then
    Result := ds.FieldByName(ColName).AsWideString;
end;


procedure TMDIChild.LoadDatabaseProperties(db: WideString);
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
    MainForm.ShowStatus( 'Displaying tables from "' + db + '" ...' );

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
    Mainform.showstatus(db + ': ' + IntToStr(ListTables.RootNodeCount) +' table(s)', 0);
    tabDatabase.Caption := sstr('Database: ' + db, 30);
    MainForm.ShowStatus(STATUS_MSG_READY);
    Screen.Cursor := crDefault;
    // Ensure tree db node displays its chidren initialized
    DBtree.ReinitChildren(FindDBNode(db), False);
  end;
end;


{ Show tables and their properties on the tabsheet "Database" }
procedure TMDIChild.ShowDBProperties(db: WideString);
begin
  Screen.Cursor := crHourglass;
  pcChange( Self );
  MainForm.ShowStatus( STATUS_MSG_READY );
  Screen.Cursor := crDefault;
end;


{ Show columns of selected table, indicate indexed columns by certain icons }
procedure TMDIChild.RefreshFieldListClick(Sender: TObject);
begin
  ShowTableProperties;
end;


procedure TMDIChild.ShowTableProperties;
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

  MainForm.ShowStatus( 'Reading table properties...' );
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
  MainForm.ShowStatus( STATUS_MSG_READY );
  MainForm.showstatus(ActiveDatabase + ': '+ SelectedTable + ': ' + IntToStr(ListColumns.RootNodeCount) +' column(s)', 0);
  Screen.Cursor := crDefault;
end;


{***
  Execute a query and return a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
}
function TMDIChild.ExecuteQuery(query: String): TDataSet;
begin
  result := GetResults(query, false, false);
end;


{***
  Execute a query without returning a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
}
procedure TMDIChild.ExecuteNonQuery(SQLQuery: String);
begin
  ExecUpdateQuery(SQLQuery);
end;



{***
  Selection in ListTables is changing
}
procedure TMDIChild.ListTablesChange(Sender: TBaseVirtualTree; Node:
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
procedure TMDIChild.ValidateControls( FrmIsFocussed: Boolean = true );
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
  inDataOrQueryTabNotEmpty := inDataOrQueryTab and (hoVisible in ActiveGrid.Header.Options);
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
  MainForm.actRefresh.Enabled := FrmIsFocussed;
  MainForm.actFlushHosts.Enabled := FrmIsFocussed;
  MainForm.actFlushLogs.Enabled := FrmIsFocussed;
  MainForm.actFlushPrivileges.Enabled := FrmIsFocussed;
  MainForm.actFlushTables.Enabled := FrmIsFocussed;
  MainForm.actFlushTableswithreadlock.Enabled := FrmIsFocussed;
  MainForm.actFlushStatus.Enabled := FrmIsFocussed;
  MainForm.actUserManager.Enabled := FrmIsFocussed;
  MainForm.actMaintenance.Enabled := FrmIsFocussed;
  MainForm.actInsertFiles.Enabled := FrmIsFocussed;
  // PrintList should only be active if we're focussing one of the ListViews,
  // at least as long we are not able to print DBGrids
  MainForm.actPrintList.Enabled := FrmIsFocussed;
  MainForm.actSQLhelp.Enabled := (mysql_version >= 40100) and FrmIsFocussed;
  MainForm.actImportCSV.Enabled := (mysql_version >= 32206) and FrmIsFocussed;
  MainForm.actExportTables.Enabled := FrmIsFocussed;

  // Database tab
  Mainform.actEmptyTables.Enabled := inDbTab and TableSelected;
  Mainform.actEditTableProperties.Enabled := inDbTab and TableSelected;
  MenuRenameTable.Enabled := inDbTab and DBObjectSelected;
  Mainform.actCopyTable.Enabled := inDbTab and DBObjectSelected;
  Mainform.actEditView.Enabled := inDbTab and ViewSelected and (mysql_version >= 50001);
  Mainform.actCreateView.Enabled := FrmIsFocussed and (ActiveDatabase <> '') and (mysql_version >= 50001);
  MainForm.actCreateDatabase.Enabled := FrmIsFocussed;
  DBfocused := Assigned(DBtree.FocusedNode) and (DBtree.GetNodeLevel(DBtree.FocusedNode) = 1);
  MainForm.actDropDatabase.Enabled := DBfocused and FrmIsFocussed;
  MainForm.actEditDatabase.Enabled := DBfocused and FrmIsFocussed and (mysql_version >= 50002);
  if mysql_version < 50002 then
    MainForm.actEditDatabase.Hint := STR_NOTSUPPORTED
  else
    MainForm.actEditDatabase.Hint := 'Rename and/or modify character set of database';
  MainForm.actDropTablesAndViews.Enabled := (DBObjectSelected and inDbTab) or ((not inQueryTab) and (SelectedTable <> '') and FrmIsFocussed);
  MainForm.actCreateTable.Enabled := (ActiveDatabase <> '') and FrmIsFocussed;
  Mainform.actEditTableFields.Enabled := DBObjectSelected and inDbTab;

  // Table tab
  FieldFocused := inTableTab and Assigned(ListColumns.FocusedNode);
  FieldsSelected := inTableTab and (Length(ListColumns.GetSortedSelection(False))>0);
  // Toggle state of menuitems and buttons
  Mainform.actEditField.Enabled := FieldFocused and FieldsSelected;
  Mainform.actCreateField.Enabled := inTableTab;
  Mainform.actDropFields.Enabled := FieldsSelected;
  Mainform.actEditIndexes.Enabled := inTableTab;
  menuRenameColumn.Enabled := FieldFocused and FieldsSelected;

  // Data tab - if query results are made editable, these will need
  //            to be changed to look at which tab is focused.
  Mainform.actDataInsert.Enabled := inDataTab;
  Mainform.actDataDelete.Enabled := inDataTab and (DataGrid.SelectedCount > 0);
  Mainform.actDataFirst.Enabled := inDataTab;
  Mainform.actDataLast.Enabled := inDataTab;
  Mainform.actDataPostChanges.Enabled := inDataTab and DataGridHasChanges;
  Mainform.actDataCancelChanges.Enabled := inDataTab and DataGridHasChanges;

  // Activate export-options if we're on Data- or Query-tab
  MainForm.actCopyAsCSV.Enabled := inDataOrQueryTabNotEmpty;
  MainForm.actCopyAsHTML.Enabled := inDataOrQueryTabNotEmpty;
  MainForm.actCopyAsXML.Enabled := inDataOrQueryTabNotEmpty;
  MainForm.actExportData.Enabled := inDataOrQueryTabNotEmpty;
  MainForm.actHTMLView.Enabled := inDataOrQueryTabNotEmpty;

  // Query tab
  MainForm.actLoadSQL.Enabled := FrmIsFocussed;
  // Manually invoke OnChange event of tabset to fill helper list with data
  if inQueryTab and FrmIsFocussed then
    tabsetQueryHelpers.OnChange(Self, tabsetQueryHelpers.TabIndex, dummy);
  ValidateQueryControls(FrmIsFocussed);

  if not FrmIsFocussed then begin
    // Empty "connected" and "uptime"
    MainForm.showstatus('', 1);
    MainForm.showstatus('', 2);
    MainForm.showstatus('', 3);
  end;
end;

procedure TMDIChild.ValidateQueryControls(FrmIsFocussed: Boolean = true);
var
  InQueryTab, NotEmpty, HasSelection: Boolean;
begin
  InQueryTab := FrmIsFocussed and (PageControlMain.ActivePage = tabQuery);
  NotEmpty := FrmIsFocussed and (SynMemoQuery.GetTextLen > 0);
  HasSelection := FrmIsFocussed and SynMemoQuery.SelAvail;
  Mainform.actExecuteQuery.Enabled := InQueryTab and NotEmpty;
  Mainform.actExecuteSelection.Enabled := InQueryTab and HasSelection;
  Mainform.actExecuteLine.Enabled := InQueryTab and (SynMemoQuery.LineText <> '');
  MainForm.actSaveSQL.Enabled := InQueryTab and NotEmpty;
  MainForm.actSaveSQLselection.Enabled := InQueryTab and HasSelection;
  MainForm.actSaveSQLSnippet.Enabled := InQueryTab and NotEmpty;
  MainForm.actSaveSQLSelectionSnippet.Enabled := InQueryTab and HasSelection;
  MainForm.actQueryFind.Enabled := InQueryTab and NotEmpty;
  MainForm.actQueryReplace.Enabled := InQueryTab and NotEmpty;
  MainForm.actQueryStopOnErrors.Enabled := InQueryTab;
  MainForm.actQueryWordWrap.Enabled := InQueryTab;
  Mainform.actClearQueryEditor.Enabled := InQueryTab and NotEmpty;
  Mainform.actSetDelimiter.Enabled := InQueryTab;
end;


procedure TMDIChild.ShowTableData(table: WideString);
begin
  dataselected := false;
  PageControlMain.ActivePage := tabData;
  viewdata(self);
  pcChange( Self );
end;


procedure TMDIChild.ShowVariablesAndProcesses(Sender: TObject);

  procedure addLVitem( caption: WideString; commandCount: Int64; totalCount: Int64 );
  var
    i : Integer;
    tmpval : Double;
  begin
    SetLength( VTRowDataListCommandStats, Length(VTRowDataListCommandStats)+1 );
    i := Length(VTRowDataListCommandStats)-1;
    VTRowDataListCommandStats[i].ImageIndex := 25;
    VTRowDataListCommandStats[i].Captions := WideStrings.TWideStringList.Create;
    caption := Copy( caption, 5, Length(caption) );
    caption := WideStringReplace( caption, '_', ' ', [rfReplaceAll] );
    VTRowDataListCommandStats[i].Captions.Add( caption );
    // Total Frequency
    VTRowDataListCommandStats[i].Captions.Add( FormatNumber( commandCount ) );
    // Average per hour
    uptime := max(uptime, 1);
    tmpval := commandCount / ( uptime / 60 / 60 );
    VTRowDataListCommandStats[i].Captions.Add( FormatNumber( tmpval, 1 ) );
    // Average per second
    tmpval := commandCount / uptime;
    VTRowDataListCommandStats[i].Captions.Add( FormatNumber( tmpval, 1 ) );
    // Percentage. Take care of division by zero errors and Int64's
    if commandCount < 1 then
      commandCount := 1;
    if totalCount < 1 then
      totalCount := 1;
    tmpval := 100 / totalCount * commandCount;
    VTRowDataListCommandStats[i].Captions.Add( FormatNumber( tmpval, 1 ) + ' %' );
  end;

var
  i : Integer;
  questions : Int64;
  ds : TDataSet;
  SelectedCaptions: WideStrings.TWideStringList;
begin
  // Prevent auto update from executing queries if the host tab is not activated
  if (Sender is TTimer) and (PageControlMain.ActivePage <> tabHost) then
    Exit;

  // Refresh variables and process-list
  Screen.Cursor := crHourglass;

  // Remember selected nodes
  SelectedCaptions := GetVTCaptions(ListVariables, True);

  // VARIABLES
  ListVariables.BeginUpdate;
  ListVariables.Clear;
  ds := GetResults( 'SHOW VARIABLES', false );
  SetLength( VTRowDataListVariables, ds.RecordCount );
  for i:=1 to ds.RecordCount do
  begin
    VTRowDataListVariables[i-1].ImageIndex := 25;
    VTRowDataListVariables[i-1].Captions := WideStrings.TWideStringList.Create;
    VTRowDataListVariables[i-1].Captions.Add( ds.Fields[0].AsWideString );
    VTRowDataListVariables[i-1].Captions.Add( ds.Fields[1].AsWideString );
    ds.Next;
  end;
  ds.Close;
  FreeAndNil(ds);
  // Tell VirtualTree the number of nodes it will display
  ListVariables.RootNodeCount := Length(VTRowDataListVariables);
  ListVariables.EndUpdate;
  SetVTSelection( ListVariables, SelectedCaptions );
  // Apply filter
  if editFilterVariables.Text <> '' then
    editFilterVTChange(editFilterVariables);
  // Display number of listed values on tab
  tabVariables.Caption := 'Variables (' + IntToStr(ListVariables.RootNodeCount) + ')';

  // STATUS
  uptime := 1; // avoids division by zero :)
  questions := 1;
  // Remember selected nodes
  SelectedCaptions := GetVTCaptions(ListStatus, True);
  ListStatus.BeginUpdate;
  ListStatus.Clear;
  ds := GetResults( 'SHOW /*!50002 GLOBAL */ STATUS' );
  SetLength( VTRowDataListStatus, ds.RecordCount );
  for i:=1 to ds.RecordCount do
  begin
    VTRowDataListStatus[i-1].ImageIndex := 25;
    VTRowDataListStatus[i-1].Captions := WideStrings.TWideStringList.Create;
    VTRowDataListStatus[i-1].Captions.Add( ds.Fields[0].AsWideString );
    VTRowDataListStatus[i-1].Captions.Add( ds.Fields[1].AsWideString );
    if lowercase( ds.Fields[0].AsString ) = 'uptime' then
      uptime := MakeInt(ds.Fields[1].AsString);
    if lowercase( ds.Fields[0].AsString ) = 'questions' then
      questions := MakeInt(ds.Fields[1].AsString);
    ds.Next;
  end;
  // Tell VirtualTree the number of nodes it will display
  ListStatus.RootNodeCount := Length(VTRowDataListStatus);
  ListStatus.EndUpdate;
  SetVTSelection( ListStatus, SelectedCaptions );
  // Apply filter
  if editFilterStatus.Text <> '' then
    editFilterVTChange(editFilterStatus);
  // Display number of listed values on tab
  tabStatus.Caption := 'Status (' + IntToStr(ListStatus.RootNodeCount) + ')';

  // Command-Statistics
  SelectedCaptions := GetVTCaptions(ListCommandStats, True);
  ListCommandStats.BeginUpdate;
  ListCommandStats.Clear;
  SetLength( VTRowDataListCommandStats, 0 );
  addLVitem( '    All commands', questions, questions );
  ds.First;
  for i:=1 to ds.RecordCount do
  begin
    if LowerCase( Copy( ds.Fields[0].AsString, 1, 4 ) ) = 'com_' then
    begin
      addLVitem( ds.Fields[0].AsWideString, MakeInt(ds.Fields[1].AsString), questions );
    end;
    ds.Next;
  end;
  ds.Close;
  FreeAndNil(ds);

  // Tell VirtualTree the number of nodes it will display
  ListCommandStats.RootNodeCount := Length(VTRowDataListCommandStats);
  ListCommandStats.EndUpdate;
  SetVTSelection( ListCommandStats, SelectedCaptions );

  TimerHostUptime.Enabled := true;
  TimerHostUptimeTimer(self);
  TimerHostUptime.OnTimer := TimerHostUptimeTimer;

  Screen.Cursor := crDefault;

  ShowProcesslist(self); // look at next procedure
end;



procedure TMDIChild.ShowProcessList(sender: TObject);
var
  i,j : Integer;
  ds  : TDataSet;
  SelectedCaptions: WideStrings.TWideStringList;
begin
  // No need to update if it's not visible.
  if PageControlMain.ActivePage <> tabHost then exit;
  if PageControlHost.ActivePage <> tabProcesslist then exit;
  Screen.Cursor := crHourglass;
  // Remember selected nodes
  SelectedCaptions := GetVTCaptions(ListProcesses, True);
  try
    ListProcesses.BeginUpdate;
    ListProcesses.Clear;
    debug('ShowProcessList()');
    ds := GetResults('SHOW FULL PROCESSLIST', false, false);
    SetLength(VTRowDataListProcesses, ds.RecordCount);
    for i:=1 to ds.RecordCount do
    begin
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
    tabProcessList.Caption := 'Process-List (' + IntToStr(Length(VTRowDataListProcesses)) + ')';
  except
    on E: Exception do begin
      LogSQL('Error loading process list (automatic refresh disabled): ' + e.Message);
      TimerHost.Enabled := false;
    end;
  end;
  ListProcesses.RootNodeCount := Length(VTRowDataListProcesses);
  ListProcesses.EndUpdate;
  // Reselect previous selected nodes
  SetVTSelection( ListProcesses, SelectedCaptions );
  // Apply filter
  if editFilterProcesses.Text <> '' then
    editFilterVTChange(editFilterProcesses);
  Screen.Cursor := crDefault;
end;

procedure TMDIChild.KillProcess(Sender: TObject);
var t : Boolean;
  ProcessIDs : WideStrings.TWideStringList;
  i : Integer;
begin
  t := TimerHost.Enabled;
  TimerHost.Enabled := false; // prevent av (ListProcesses.selected...)
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
    ShowVariablesAndProcesses(self);
  end;
  TimerHost.Enabled := t; // re-enable autorefresh timer
end;


procedure TMDIChild.PageControlHostChange(Sender: TObject);
begin
  // Show processlist if it's visible now but empty yet
  if ListProcesses.RootNodeCount = 0 then
    ShowProcessList( self );
end;





procedure TMDIChild.ExecSQLClick(Sender: TObject; Selection: Boolean=false; CurrentLine: Boolean=false);
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
    MainForm.showstatus( 'Initializing SQL...' );
    Mainform.actExecuteQuery.Enabled := false;
    Mainform.actExecuteSelection.Enabled := false;

    // Let EnsureActiveDatabase know that we've fired user queries.
    UserQueryFiring := true;

    rowsaffected := 0;
    fieldcount := 0;
    recordcount := 0;
    ProgressBarQuery.Max := SQL.Count;
    ProgressBarQuery.Position := 0;
    ProgressBarQuery.Show();

    MainForm.showstatus( 'Executing SQL...' );
    for i := 0 to (SQL.Count - 1) do
    begin
      ProgressBarQuery.StepIt();
      ProgressBarQuery.Repaint;
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
          if Mainform.actQueryStopOnErrors.Checked or (i = SQL.Count - 1) then begin
            Screen.Cursor := crDefault;
            MessageDlg( E.Message, mtError, [mbOK], 0 );
            ProgressBarQuery.Hide();
            Mainform.actExecuteQuery.Enabled := true;
            Mainform.actExecuteSelection.Enabled := true;
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

    ProgressBarQuery.Hide();
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
    MainForm.ShowStatus( STATUS_MSG_READY );
  end;
end;


{**
  Clicked somewhere in the field-list of the "Table"-tabsheet
}
procedure TMDIChild.ListColumnsChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  ValidateControls;
end;


{ Proposal about to insert a String into synmemo }
procedure TMDIChild.SynCompletionProposal1CodeCompletion(Sender: TObject;
  var Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


procedure TMDIChild.SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
  const Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
begin
  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
end;


{ Proposal-Combobox pops up }
procedure TMDIChild.SynCompletionProposal1Execute(Kind: SynCompletionType;
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


procedure TMDIChild.SynMemoQueryStatusChange(Sender: TObject; Changes:
    TSynStatusChanges);
begin
  ValidateQueryControls;
end;



procedure TMDIChild.TimerHostUptimeTimer(Sender: TObject);
var
  days, hours, minutes, seconds : Integer;
  msg: string;
begin
  // Host-Uptime
  days:= uptime div (60*60*24);
  seconds := uptime mod (60*60*24);
  hours := seconds div (60*60);
  seconds := seconds mod (60*60);
  minutes  := seconds div 60;
  seconds := seconds mod 60;

  inc(uptime);
  msg := Format('%d days, %.2d:%.2d:%.2d', [days,hours,minutes,seconds]);
  if TimerHostUptime.Enabled then msg := Format('Uptime: %s', [msg])
  else msg := '';
  Mainform.showstatus(msg, 3);
end;


procedure TMDIChild.FormActivate(Sender: TObject);
begin
  TimerConnected.OnTimer(self);
end;

procedure TMDIChild.FormShow(Sender: TObject);
begin
  DataGridHasChanges := False;

  { TODO : only load file when autoconnected ?? }
  if (paramstr(1) <> '') and Main.loadsqlfile then
  try
    // load sql-file from paramstr
    SynMemoQuery.Lines.LoadFromFile(paramstr(1));
    Main.loadsqlfile := false;
  except
    MessageDLG('File could not be opened: ' + paramstr(1), mtError, [mbOK], 0);
  end;

  //TODO:
  //ds.DisableControls;
end;


{***
  Rename table after checking the new name for invalid characters
}
procedure TMDIChild.ListTablesNewText(Sender: TBaseVirtualTree; Node:
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

procedure TMDIChild.MenuRenameTableClick(Sender: TObject);
begin
  // menuitem for edit table-name
  ListTables.EditNode( ListTables.FocusedNode, 0 );
end;


procedure TMDIChild.TimerConnectedTimer(Sender: TObject);
begin
  if not TimerConnected.Enabled then begin
    MainForm.showstatus('Disconnected.', 1);
    exit;
  end;

  inc(time_connected);

  // calculate and display connection-time
  MainForm.showstatus( 'Connected: ' + FormatTimeNumber(time_connected), 1 );
end;


procedure TMDIChild.Clear2Click(Sender: TObject);
begin
  // clear history-memo
  Screen.Cursor := crHourglass;
  SynMemoSQLLog.Lines.Clear;
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.EditQuery1Click(Sender: TObject);
begin
  // take query from history to query-tab
  SynMemoQuery.Text := SynMemoSQLLog.SelText;
  PageControlMain.ActivePage := tabQuery;
  pcChange(self);
end;


procedure TMDIChild.Markall3Click(Sender: TObject);
begin
  // select all in history
  SynMemoSQLLog.SelectAll;
end;


procedure TMDIChild.ListTablesDblClick(Sender: TObject);
begin
  Mainform.actEditTableFields.Execute;
end;


procedure TMDIChild.TimerConnectErrorCloseWindowTimer(Sender: TObject);
begin
  // can't connect -> close MDI-Child
  TimerConnectErrorCloseWindow.Enabled := false;
  Mainform.Showstatus('', 1);
  MainForm.ShowStatus( STATUS_MSG_READY );
  close;
end;


{**
  Column-title clicked -> generate "ORDER BY"
}
procedure TMDIChild.QuickFilterClick(Sender: TObject);
var
  filter,value,column : WideString;
  menuitem : TMenuItem;
begin
  // Set filter for "where..."-clause
  value := DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn];
  menuitem := (Sender as TMenuItem);
  column := mask(DataGrid.Header.Columns[DataGrid.FocusedColumn].Text);
  if menuitem = QF1 then
    filter := column + ' =' + ' ' + esc( value )
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

  // Filters with text from clipboard
  else if (menuitem = QF13) or (menuitem = QF14) or (menuitem = QF15) or (menuitem = QF16) or (menuitem = QF17) then
  begin
    filter := menuitem.Caption;
  end;

  SynMemoFilter.UndoList.AddGroupBreak;
  SynMemoFilter.SelectAll;
  SynmemoFilter.SelText := filter;
  SaveFilter(filter);
  viewdata(Sender);
end;


function TMDIChild.GetFilter: WideString;
var
  SomeFilter: Boolean;
  reg: TRegistry;
  regCrashIndicName, regFilterName: String;
begin
  // Read cached WHERE-clause and set filter
  if prefRememberFilters then begin
    regFilterName := Utf8Encode(REGPREFIX_WHERECLAUSE + ActiveDatabase + '.' + SelectedTable);
    Result := Mainform.GetRegValue(regFilterName, '', FConn.Description );
    if Result <> '' then begin
      // Check for crash indicator on current table
      regCrashIndicName := Utf8Encode(REGPREFIX_CRASH_IN_DATA + ActiveDatabase + '.' + SelectedTable);
      if(Mainform.GetRegValue(regCrashIndicName, False, FConn.Description)) then begin
        LogSQL('A crash in the previous data loading for this table ('+SelectedTable+') was detected. Filter was automatically reset to avoid the same crash for now.');
        Result := '';
        reg := TRegistry.Create;
        reg.OpenKey( REGPATH + REGKEY_SESSIONS + FConn.Description, true );
        // Filter was nuked. Reset crash indicator.
        reg.DeleteValue(regFilterName);
        reg.CloseKey;
        reg.Free;
      end;
    end;
  end else
    Result := SynMemoFilter.Text;
  SomeFilter := Result <> '';
  if SomeFilter then tbtnDataFilter.ImageIndex := 108
  else tbtnDataFilter.ImageIndex := 107;
  // Ensure filter panel is visible
  if SomeFilter then
    ToggleFilterPanel(True);
  // Hide it if it was auto opened previously
  if (not SomeFilter) and pnlFilter.Visible and (not FilterPanelManuallyOpened) then
    ToggleFilterPanel;
  if SynMemoFilter.Text <> Result then begin
    SynMemoFilter.UndoList.AddGroupBreak;
    SynMemoFilter.SelectAll;
    SynMemoFilter.SelText := Result;
  end;
  SynMemoFilterChange(Self);
end;


procedure TMDIChild.SaveFilter(Clause: WideString = '');
var
  regname: String;
begin
  // Store whereclause in Registry
  if prefRememberFilters then begin
    Mainform.regMain.openkey( REGPATH + REGKEY_SESSIONS + FConn.Description, false );
    regname := REGPREFIX_WHERECLAUSE + ActiveDatabase + '.' + SelectedTable;
    if Clause <> '' then Mainform.regMain.WriteString( regname, Clause )
    else if Mainform.regMain.ValueExists( regname ) then Mainform.regMain.DeleteValue( regname );
  end
end;


procedure TMDIChild.DropFilter1Click(Sender: TObject);
begin
  // Drop Filter
  SaveFilter;
  viewdata(Sender);
end;


// select all tables
procedure TMDIChild.selectall1Click(Sender: TObject);
begin
  ListTables.SelectAll(False);
end;

procedure TMDIChild.popupQueryPopup(Sender: TObject);
begin
  // Sets cursor into memo and activates TAction(s) like paste
  SynMemoQuery.SetFocus;
end;

procedure TMDIChild.popupResultGridPopup(Sender: TObject);
begin
  // data available?
  // MainForm.Save2CSV.enabled :=
end;

procedure TMDIChild.controlsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Grid : TVirtualStringTree;
  CB: TUniClipboard;
begin
  // Check for F1-pressed
  if Key = VK_F1 then
    Mainform.actSQLhelp.Execute

  // Simulate Ctrl+A-behaviour of common editors
  else if ( Shift = [ssCtrl] ) and ( Key = Ord('A') ) then
  begin
    if Sender is TCustomEdit then
      TCustomEdit(Sender).SelectAll;
  end

  // Enable copy + paste shortcuts in dbgrids
  else if (Sender is TVirtualStringTree) and (not TVirtualStringTree(Sender).IsEditing)
    and (Shift = [ssCtrl]) and (Assigned(TVirtualStringTree(Sender).FocusedNode))
    then begin
    Grid := Sender as TVirtualStringTree;
    // TODO: Clipboard.AsText is not Unicode safe!
    if Key = Ord('C') then
      CopyToClipboard(Grid.Text[Grid.FocusedNode, Grid.FocusedColumn])
    else if Key = Ord('V') then begin
      CB := TUniClipboard.Create;
      Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := CB.AsWideString;
    end
    else if Key = Ord('X') then begin
      CopyToClipboard(Grid.Text[Grid.FocusedNode, Grid.FocusedColumn]);
      Grid.Text[Grid.FocusedNode, Grid.FocusedColumn] := '';
    end;
  end;
end;

procedure TMDIChild.Autoupdate1Click(Sender: TObject);
var
  seconds : String;
  secondsInt : Integer;
begin
  // set interval for autorefresh-timer
  seconds := IntToStr(TimerHost.interval div 1000);
  if inputquery('Auto-refresh processlist','Update list every ... seconds:', seconds) then begin
    secondsInt := StrToIntDef(seconds, 0);
    if secondsInt > 0 then begin
      TimerHost.Interval := secondsInt * 1000;
      TimerHost.Enabled := true;
      EnableAutoRefresh.Checked := true;
      DisableAutoRefresh.Checked := false;
    end
    else
      MessageDLG('Seconds must be between 1 and ' + IntToStr(maxint) + '.', mtError, [mbOK], 0);
  end;
end;

procedure TMDIChild.EnableAutoRefreshClick(Sender: TObject);
begin
  // enable autorefresh-timer
  TimerHost.Enabled := true;
  EnableAutoRefresh.Checked := true;
  DisableAutoRefresh.Checked := false;
end;

procedure TMDIChild.DisableAutoRefreshClick(Sender: TObject);
begin
  // enable autorefresh-timer
  TimerHost.Enabled := false;
  EnableAutoRefresh.Checked := false;
  DisableAutoRefresh.Checked := true;
end;



procedure TMDIChild.SynMemoQueryDragOver(Sender, Source: TObject; X,
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


procedure TMDIChild.SynMemoQueryDragDrop(Sender, Source: TObject; X,
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



procedure TMDIChild.SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TWideStrings);
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


procedure TMDIChild.popupHostPopup(Sender: TObject);
begin
  Kill1.Enabled := (PageControlHost.ActivePage = tabProcessList) and Assigned(ListProcesses.FocusedNode);
  menuEditVariable.Enabled := False;
  if mysql_version >= 40003 then
    menuEditVariable.Enabled := (PageControlHost.ActivePage = tabVariables) and Assigned(ListVariables.FocusedNode)
  else
    menuEditVariable.Hint := STR_NOTSUPPORTED;
end;

procedure TMDIChild.Saveastextfile1Click(Sender: TObject);
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

procedure TMDIChild.popupTreeViewPopup(Sender: TObject);
var
  L: Cardinal;
begin
  // toggle drop-items and remember right-clicked item
  if DBtree.GetFirstSelected = nil then
    L := 0
  else
    L := DBtree.GetNodeLevel(DBtree.GetFirstSelected);
  Mainform.actCreateTable.Enabled := L in [1,2];
  Mainform.actCreateView.Enabled := (L in [1,2]) and (mysql_version >= 50001);
  Mainform.actEditTableProperties.Enabled := (L = 2) and ((GetSelectedNodeType = NODETYPE_TABLE) or (GetSelectedNodeType = NODETYPE_CRASHED_TABLE));
  Mainform.actEditView.Enabled := (L = 2) and (GetSelectedNodeType = NODETYPE_VIEW);
  MainForm.actDropTablesAndViews.Enabled := (L = 2);
end;




procedure TMDIChild.QueryLoad( filename: String; ReplaceContent: Boolean = true );

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
            Mainform.AddOrRemoveFromQueryLoadHistory( filename, true );
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
      Mainform.AddOrRemoveFromQueryLoadHistory( filename, true );
    Mainform.FillPopupQueryLoad;
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




procedure TMDIChild.SaveDialogExportDataTypeChange(Sender: TObject);
begin
  // Set default file-extension of saved file and options on the dialog to show
  with SaveDialogExportData do begin
    Case FilterIndex of
      1: DefaultExt := 'csv';
      2: DefaultExt := 'html';
      3: DefaultExt := 'xml';
    end;
  end;
end;


{**
  A cell in a DBGrid is painted. Sets custom background color NULL fields.
}
procedure TMDIChild.popupDataGridPopup(Sender: TObject);
var
  y,m,d,h,i,s,ms : Word;
  cpText, selectedColumn, value : String;
const
  CLPBRD : String = 'CLIPBOARD';
begin
  {DONE  -oFrancisco -cData-browsing:Bugfix: [1650528] Access violation with F5}
  if Assigned(DataGrid.FocusedNode) and (DataGrid.FocusedColumn > -1) then
  begin
    DataInsertDateTime.Enabled := FDataGridResult.Columns[DataGrid.FocusedColumn].IsDate;
    if DataInsertDateTime.Enabled then
    begin
      decodedate(now, y, m, d);
      decodetime(now, h, i, s, ms);
      DataDateTime.Caption := Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,i,s]);
      DataDate.Caption := Format('%.4d-%.2d-%.2d', [y,m,d]);
      DataTime.Caption := Format('%.2d:%.2d:%.2d', [h,i,s]);
      DataTimestamp.caption := Format('%.4d%.2d%.2d%.2d%.2d%.2d', [y,m,d,h,i,s]);
      DataYear.Caption := Format('%.4d', [y]);
    end;

    // Manipulate the Quick-filter menuitems
    selectedColumn := mask(DataGrid.Header.Columns[DataGrid.FocusedColumn].Text);
    // 1. block: include selected columnname and value from datagrid in caption
    value := sstr(DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn], 100);
    QF1.Caption := selectedColumn + ' = ' + esc( value );
    QF2.Caption := selectedColumn + ' != ' + esc( value );
    QF3.Caption := selectedColumn + ' > ' + esc( value );
    QF4.Caption := selectedColumn + ' < ' + esc( value );
    QF5.Caption := selectedColumn + ' LIKE ''' + esc( value, true ) + '%''';
    QF6.Caption := selectedColumn + ' LIKE ''%' + esc( value, true ) + '''';
    QF7.Caption := selectedColumn + ' LIKE ''%' + esc( value, true ) + '%''';

    // 2. block: include only selected columnname in caption
    QF8.Caption := selectedColumn + ' = "..."';
    QF9.Caption := selectedColumn + ' != "..."';
    QF10.Caption := selectedColumn + ' > "..."';
    QF11.Caption := selectedColumn + ' < "..."';
    QF12.Caption := selectedColumn + ' LIKE "%...%"';

    // 3. block: include selected columnname and clipboard-content in caption for one-click-filtering
    cpText := Clipboard.AsText;
    if Length(cpText) < 100 then
    begin
      QF13.Enabled := true; QF13.Caption := selectedColumn + ' = ' + esc( cpText );
      QF14.Enabled := true; QF14.Caption := selectedColumn + ' != ' + esc( cpText );
      QF15.Enabled := true; QF15.Caption := selectedColumn + ' > ' + esc( cpText );
      QF16.Enabled := true; QF16.Caption := selectedColumn + ' < ' + esc( cpText );
      QF17.Enabled := true; QF17.Caption := selectedColumn + ' LIKE ''%' + esc( cpText, true ) + '%''';
    end
    else
    begin
      QF13.Enabled := false; QF13.Caption := selectedColumn + ' = ' + CLPBRD;
      QF14.Enabled := false; QF14.Caption := selectedColumn + ' != ' + CLPBRD;
      QF15.Enabled := false; QF15.Caption := selectedColumn + ' > ' + CLPBRD;
      QF16.Enabled := false; QF16.Caption := selectedColumn + ' < ' + CLPBRD;
      QF17.Enabled := false; QF17.Caption := selectedColumn + ' LIKE %' + CLPBRD + '%';
    end;
  end;
end;

procedure TMDIChild.InsertDate(Sender: TObject);
var d : String;
begin
  // Insert date/time-value into table
  d := (sender as TMenuItem).Caption;
  delete(d, Pos('&', d), 1);
  DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn] := d;
end;


procedure TMDIChild.ExecUseQuery(db: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false);
begin
  ExecUpdateQuery('USE ' + mask(db), HandleErrors, DisplayErrors);
  FConn.MysqlParams.Database := db;
end;


{***
  Execute a query without returning a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
}
function TMDIChild.ExecUpdateQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): Int64;
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
function TMDIChild.ExecSelectQuery(sql: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false; ForceDialog: Boolean = false): TDataSet;
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
function TMDIChild.GetResults( SQLQuery: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false; ForceDialog: Boolean = false): TDataSet;
begin
  result := ExecSelectQuery(SQLQuery, HandleErrors, DisplayErrors, ForceDialog);
end;


{***
  Execute a query and return String from column x
}
function TMDIChild.GetVar( SQLQuery: WideString; x: Integer = 0; HandleErrors: Boolean = false; DisplayErrors: Boolean = false) : WideString;
var
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors );
  if ds = nil then exit;
  Result := ds.Fields[x].AsWideString;
  ds.Close;
  FreeAndNil(ds);
end;


function TMDIChild.GetNamedVar( SQLQuery: WideString; x: WideString; HandleErrors: Boolean = false; DisplayErrors: Boolean = false) : WideString;
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
function TMDIChild.GetVisualDataset: PGridResult;
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
function TMDIChild.GetCol( SQLQuery: WideString; x: Integer = 0; HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : WideStrings.TWideStringList;
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
procedure TMDIChild.ZSQLMonitor1LogTrace(Sender: TObject;
  Event: TZLoggingEvent);
begin
  LogSQL( Event.Message, (Event.Category <> lcExecute) );
end;


procedure TMDIChild.RunAsyncPost(ds: TDeferDataSet);
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
function TMDIChild.RunThreadedQuery(AQuery: WideString; ForceDialog: Boolean): TMysqlQuery;
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
function TMDIChild.mask(str: WideString) : WideString;
begin
  result := maskSql(mysql_version, str);
end;


procedure TMDIChild.CheckConnection;
begin
  if not FMysqlConn.IsAlive then begin
    LogSQL('Connection failure detected. Trying to reconnect.', true);
    TimerConnected.Enabled := false;
    TimerConnectedTimer(self);
    TimerHostUptime.Enabled := false;
    TimerHostUptimeTimer(self);
    // 1) CheckConnection is always called from
    //    within an FQueryRunning-enabled block.
    // 2) PerformConnect (see below) will make calls
    //    that open an FQueryRunning block, causing an
    //    error message.
    //
    // Therefore, flick the state of the running
    // flag before running PerformConnect().
    FQueryRunning := false;
    try
      FMysqlConn.Connection.Reconnect;
      PerformConnect;
    finally
      FQueryRunning := true;
    end;
  end;
end;



function TMDIChild.GetActiveGrid: TVirtualStringTree;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := DataGrid
  else if PageControlMain.ActivePage = tabQuery then Result := QueryGrid;
end;

function TMDIChild.GetActiveData: PGridResult;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := @FDataGridResult
  else if PageControlMain.ActivePage = tabQuery then Result := @FQueryGridResult;
end;



function TMDIChild.GetActiveDatabase: WideString;
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


function TMDIChild.GetSelectedTable: WideString;
begin
  if DBtree.GetFirstSelected = nil then Result := ''
  else case DBtree.GetNodeLevel(DBtree.GetFirstSelected) of
      2: Result := DBtree.Text[DBtree.GetFirstSelected, 0];
    else Result := '';
  end;
end;


function TMDIChild.GetNodeType(Node: PVirtualNode): Byte;
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

function TMDIChild.GetSelectedNodeType: Byte;
begin
  Result := GetNodeType(DBtree.GetFirstSelected);
end;


procedure TMDIChild.SetSelectedTable(table: WideString);
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
    DBtree.Expanded[dbnode] := True;
    DBtree.Selected[snode] := True;
    exit;
  end;
  raise Exception.Create('Table node ' + table + ' not found in tree.');
end;


procedure TMDIChild.SetSelectedDatabase(db: WideString);
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
procedure TMDIChild.btnDataClick(Sender: TObject);
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
procedure TMDIChild.tabsetQueryHelpersChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  i : Integer;
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
  if (Length(QueryHelpersSelectedItems[NewTab]) > 0)
    and (Length(QueryHelpersSelectedItems[NewTab]) <= lboxQueryHelpers.Count) then
  begin
    for i := 0 to Length(QueryHelpersSelectedItems[NewTab]) - 1 do begin
      lboxQueryHelpers.Selected[QueryHelpersSelectedItems[NewTab][i]] := True;
    end;
  end;

  lboxQueryHelpers.Items.EndUpdate;

end;



{**
  Insert string from listbox with query helpers into SQL
  memo at doubleclick
}
procedure TMDIChild.lboxQueryHelpersDblClick(Sender: TObject);
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
    3: // Load snippet file ínto query-memo
      QueryLoad( DIRNAME_SNIPPETS + lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex] + '.sql', False );
    else // For all other tabs just insert the item from the list
      SynMemoQuery.SelText := text;
  end;

  SynMemoQuery.SetFocus;
end;


{**
  Remember last used items in query helper tabs
}
procedure TMDIChild.lboxQueryHelpersClick(Sender: TObject);
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
procedure TMDIChild.insertFunction(Sender: TObject);
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
procedure TMDIChild.menuRenameColumnClick(Sender: TObject);
begin
  ListColumns.EditNode(ListColumns.FocusedNode, 0);
end;


{**
  Rename a column name from within listColumns
}
procedure TMDIChild.ListColumnsNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  def : TDataSet;
  sql_update, sql_null, sql_default, sql_extra, sql_comment, DefaultValue : WideString;
  NodeData : PVTreeData;
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
procedure TMDIChild.menuDeleteSnippetClick(Sender: TObject);
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
      Mainform.FillPopupQueryLoad;
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
procedure TMDIChild.menuInsertSnippetAtCursorClick(Sender: TObject);
begin
  QueryLoad( DIRNAME_SNIPPETS + lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex] + '.sql', False );
end;


{**
  Load snippet and replace content
}
procedure TMDIChild.menuLoadSnippetClick(Sender: TObject);
begin
  QueryLoad( DIRNAME_SNIPPETS + lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex] + '.sql', True );
end;


{**
  Open snippets-directory in Explorer
}
procedure TMDIChild.menuExploreClick(Sender: TObject);
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
procedure TMDIChild.vstGetNodeDataSize(Sender: TBaseVirtualTree; var
    NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TVTreeData);
end;


{**
  Various lists initialize their nodes by calling the following procedure
  once per node
}
procedure TMDIChild.vstInitNode(Sender: TBaseVirtualTree; ParentNode,
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
procedure TMDIChild.vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
procedure TMDIChild.vstGetText(Sender: TBaseVirtualTree; Node:
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
procedure TMDIChild.vstGetImageIndex(Sender: TBaseVirtualTree; Node:
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
procedure TMDIChild.vstHeaderClick(Sender: TVTHeader; Column:
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
procedure TMDIChild.vstCompareNodes(Sender: TBaseVirtualTree; Node1,
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
  VirtualTree gets painted. Adjust background color of sorted column.
}
procedure TMDIChild.vstBeforePaint(Sender: TBaseVirtualTree;
    TargetCanvas: TCanvas);
var
  i : Integer;
  h : TVTHeader;
begin
  h := TVirtualStringTree(Sender).Header;
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
function TMDIChild.GetVTreeDataArray( VT: TBaseVirtualTree ): PVTreeDataArray;
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
procedure TMDIChild.TestVTreeDataArray( P: PVTreeDataArray );
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
procedure TMDIChild.MenuTablelistColumnsClick(Sender: TObject);
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
procedure TMDIChild.SaveListSetup( List: TVirtualStringTree );
var
  i : Byte;
  ColWidths, ColsVisible, ColPos : String;
  reg   : TRegistry;
begin
  reg := TRegistry.Create;
  reg.OpenKey( REGPATH, true );

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
  reg.WriteString( REGPREFIX_COLWIDTHS + List.Name, ColWidths );
  reg.WriteString( REGPREFIX_COLSVISIBLE + List.Name, ColsVisible );
  reg.WriteString( REGPREFIX_COLPOS + List.Name, ColPos );
  FreeAndNil(reg);
end;


{**
  Restore setup of VirtualStringTree from registry
}
procedure TMDIChild.RestoreListSetup( List: TVirtualStringTree );
var
  i : Byte;
  colwidth, colpos : Integer;
  Value : WideString;
  ValueList : WideStrings.TWideStringList;
begin
  ValueList := WideStrings.TWideStringList.Create;

  // Column widths
  Value := Mainform.GetRegValue(REGPREFIX_COLWIDTHS + List.Name, '');
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
  Value := Mainform.GetRegValue(REGPREFIX_COLSVISIBLE + List.Name, '');
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    SetVisibleListColumns( List, ValueList );
  end;

  // Column position
  Value := Mainform.GetRegValue(REGPREFIX_COLPOS + List.Name, '');
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
procedure TMDIChild.SetVisibleListColumns( List: TVirtualStringTree; Columns: WideStrings.TWideStringList );
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
procedure TMDIChild.ActivateFileLogging;
var
  LogfilePattern : String;
  i : Integer;
begin
  // Ensure directory exists
  ForceDirectories( DirnameSessionLogs );

  // Determine free filename if it's emtpy yet
  if FileNameSessionLog = '' then
  begin
    LogfilePattern := '%s %.6u.log';
    i := 1;
    FileNameSessionLog := DirnameSessionLogs + goodfilename(Format(LogfilePattern, [FConn.Description, i]));
    while FileExists( FileNameSessionLog ) do
    begin
      inc(i);
      FileNameSessionLog := DirnameSessionLogs + goodfilename(Format(LogfilePattern, [FConn.Description, i]));
    end;
  end;

  // Be sure file is closed before we (re-)open it
  DeactivateFileLogging;
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
procedure TMDIChild.DeactivateFileLogging;
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
procedure TMDIChild.vstGetHint(Sender: TBaseVirtualTree; Node:
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
procedure TMDIChild.menuLogToFileClick(Sender: TObject);
var
  reg : TRegistry;
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
    reg := TRegistry.Create;
    reg.OpenKey(REGPATH, true);
    reg.WriteBool('LogToFile', prefLogToFile);
    reg.Free;
  end;
end;


{**
  Open folder with session logs
}
procedure TMDIChild.menuOpenLogFolderClick(Sender: TObject);
begin
  ShellExec( '', DirnameSessionLogs );
end;


{**
  A header column of a VirtualTree was "dragged out", which means:
  dragged down or up, not to the left or right.
  We imitate the behaviour of various applications (fx Outlook) and
  hide this dragged column
}
procedure TMDIChild.vstHeaderDraggedOut(Sender: TVTHeader; Column:
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
procedure TMDIChild.ListCommandStatsBeforeCellPaint(Sender: TBaseVirtualTree;
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


function TMDIChild.HandleOrderColumns( AddOrderCol: TOrderCol = nil ): TOrderColArray;
var
  i, j : Integer;
  reg : TRegistry;
  reg_name : WideString;
  old_orderclause, new_orderclause, columnname : WideString;
  order_parts, ValidColumns : WideStrings.TWideStringList;
  columnexists : Boolean;
  regCrashIndicName: String;
begin
  SetLength( Result, 0 );

  // Read ORDER clause from registry
  reg_name := Utf8Encode(REGPREFIX_ORDERCLAUSE + ActiveDatabase + '.' + SelectedTable);
  old_orderclause := Utf8Decode(Mainform.GetRegValue(reg_name, '', FConn.Description));

  if old_orderclause <> '' then
  begin
    // Check for crash indicator on current table
    regCrashIndicName := Utf8Encode(REGPREFIX_CRASH_IN_DATA + ActiveDatabase + '.' + SelectedTable);
    if(Mainform.GetRegValue(regCrashIndicName, False, FConn.Description)) then begin
      LogSQL('A crash in the previous data loading for this table ('+SelectedTable+') was detected. A stored ORDER clause was automatically reset to avoid the same crash for now.');
      reg := TRegistry.Create;
      reg.OpenKey( REGPATH + REGKEY_SESSIONS + FConn.Description, true );
      // Remove ORDER BY clause from registry
      reg.DeleteValue(reg_name);
      reg.CloseKey;
      reg.Free;
    end else begin
      // Parse ORDER clause
      order_parts := explode( ',', old_orderclause );
      ValidColumns := GetVTCaptions( ListColumns );
      for i := 0 to order_parts.Count - 1 do
      begin
        columnname := Trim( Copy( order_parts[i], 0, LastPos( ' ', order_parts[i] ) ) );
        columnname := WideDequotedStr(columnname, '`');
        columnexists := ValidColumns.IndexOf(columnname) > -1;

        if not columnexists then
        begin
          LogSQL( 'Notice: A stored ORDER-BY clause could not be applied, '+
            'because the column "' + columnname + '" does not exist!');
          Continue;
        end;

        // Add part of order clause to result array
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := TOrderCol.Create;
        Result[Length(Result)-1].ColumnName := columnname;
        Result[Length(Result)-1].SortDirection := Integer( Copy( order_parts[i], ( Length( order_parts[i] ) - 3 ), 4 ) = 'DESC' );

      end;
    end;
  end;

  // Add a new order column after a columns title has been clicked
  if AddOrderCol <> nil then
  begin
    // Check if order column is already existant
    columnexists := False;
    for i := Low(Result) to High(Result) do
    begin
      if Result[i].ColumnName = AddOrderCol.ColumnName then
      begin
        // AddOrderCol is already in the list. Switch its direction:
        // ASC > DESC > [delete col]
        columnexists := True;
        if Result[i].SortDirection = ORDER_ASC then
          Result[i].SortDirection := ORDER_DESC
        else
        begin
          // Delete order col
          for j := i to High(Result) - 1 do
            Result[j] := Result[j+1];
          SetLength(Result, Length(Result)-1);
        end;
        // We found the matching column, no need to loop further
        break;
      end;
    end;

    if not columnexists then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := AddOrderCol;
    end;
  end;

  // Update registry
  new_orderclause := ComposeOrderClause(Result);
  if new_orderclause <> old_orderclause then
  begin
    reg := TRegistry.Create();
    reg.OpenKey( REGPATH + REGKEY_SESSIONS + FConn.Description, true );
    if new_orderclause <> '' then
      reg.WriteString(reg_name , Utf8Encode(new_orderclause))
    else
      reg.DeleteValue(reg_name);
    reg.Free;
  end;

end;


{**
  Concat all sort options to a ORDER clause
}
function TMDIChild.ComposeOrderClause(Cols: TOrderColArray): WideString;
var
  i : Integer;
  sort : String;
begin
  result := '';
  for i := 0 to Length(Cols) - 1 do
  begin
    if result <> '' then
      result := result + ', ';
    if Cols[i].SortDirection = ORDER_ASC then
      sort := TXT_ASC
    else
      sort := TXT_DESC;
    result := result + Mainform.Mask( Cols[i].ColumnName ) + ' ' + sort;
  end;
end;

{**
  Fetch table engines from server
  Currently used in tbl_properties and createtable
}
procedure TMDIChild.TableEnginesCombo(var Combobox: TCombobox);
var
  engineName, defaultEngine, engineSupport : String;
  HaveEngineList : TStrings;
begin
  Combobox.Items.BeginUpdate;
  Combobox.Items.Clear;

  // Cache datasets
  if (dsShowEngines = nil) and (dsHaveEngines = nil) then
  begin
    dsShowEngines := Mainform.Childwin.GetResults('SHOW ENGINES', True);
    if dsShowEngines = nil then
      dsHaveEngines := Mainform.Childwin.GetResults('SHOW VARIABLES LIKE ''have%''');
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
procedure TMDIChild.ListProcessesChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
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
procedure TMDIChild.editFilterVTChange(Sender: TObject);
var
  Node : PVirtualNode;
  NodeData : PVTreeData;
  VT : TVirtualStringTree;
  Edit : TEdit;
  i : Integer;
  match : Boolean;
  search : String;
  somefiltered : Boolean;
begin
  // Find the correct VirtualTree that shall be filtered
  if Sender = editFilterVariables then
    VT := ListVariables
  else if Sender = editFilterStatus then
    VT := ListStatus
  else if Sender = editFilterProcesses then
    VT := ListProcesses
  else
    Raise Exception.Create('editFilterVTChange() called with wrong sender control ('+(Sender as TControl).Name+')' );
  Edit := Sender as TEdit;
  // Loop through all nodes to adjust their vsVisible state
  Node := VT.GetFirst;
  search := LowerCase( Edit.Text );
  somefiltered := False;
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
    if match then
      Node.States := Node.States + [vsVisible]
    else
      Node.States := Node.States - [vsVisible];
    if (not somefiltered) and (not match) then
      somefiltered := True;
    Node := VT.GetNext(Node);
  end;
  // Colorize TEdit with filter string to signalize that some nodes are hidden now
  if somefiltered then begin
    Edit.Font.Color := clRed;
    Edit.Color := clYellow;
  end else begin
    Edit.Font.Color := clWindowText;
    Edit.Color := clWindow;
  end;
  // Needs a refresh to apply visible states
  VT.Refresh;
end;


procedure TMDIChild.ListVariablesDblClick(Sender: TObject);
begin
  menuEditVariable.Click;
end;


{**
  Edit a server variable
}
procedure TMDIChild.menuEditVariableClick(Sender: TObject);
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
  Apply icons to tabs of query helpers box 
}
procedure TMDIChild.tabsetQueryHelpersGetImageIndex(Sender: TObject; TabIndex:
    Integer; var ImageIndex: Integer);
begin
  case TabIndex of
    0: ImageIndex := 42;
    1: ImageIndex := 13;
    2: ImageIndex := 25;
    3: ImageIndex := 35;
  end;
end;


{**
  The database tree doesn't use any structure for its nodes.
}
procedure TMDIChild.DBtreeGetNodeDataSize(Sender: TBaseVirtualTree; var
    NodeDataSize: Integer);
begin
  NodeDataSize := 0;
end;


{**
  Set text of a treenode before it gets displayed or fetched in any way
}
procedure TMDIChild.DBtreeGetText(Sender: TBaseVirtualTree; Node:
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
procedure TMDIChild.DBtreeGetImageIndex(Sender: TBaseVirtualTree; Node:
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
procedure TMDIChild.DBtreeInitChildren(Sender: TBaseVirtualTree; Node:
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
        mainform.Showstatus( 'Reading Databases...' );
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
          Mainform.showstatus( IntToStr( Databases.Count ) + ' Databases', 0 );
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
          MainForm.ShowStatus( STATUS_MSG_READY );
          Screen.Cursor := crDefault;
        end;
      end;
    // DB node expanding
    1: begin
        Screen.Cursor := crHourglass;
        mainform.Showstatus( 'Reading Tables...' );
        try
          ds := FetchDbTableList(Databases[Node.Index]);
          ChildCount := ds.RecordCount;
        finally
          MainForm.ShowStatus( STATUS_MSG_READY );
          Screen.Cursor := crDefault;
        end;
    end;
    else Exit;
  end;
end;


{**
  Set initial options of a treenode
}
procedure TMDIChild.DBtreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
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
procedure TMDIChild.DBtreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  newDb: WideString;
begin
  if not Assigned(Node) then
    Exit;
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


procedure TMDIChild.DBtreeDblClick(Sender: TObject);
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


procedure TMDIChild.DBtreePaintText(Sender: TBaseVirtualTree; const
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
  Refresh database tab
}
procedure TMDIChild.MenuRefreshClick(Sender: TObject);
begin
  RefreshTreeDB(ActiveDatabase);
  LoadDatabaseProperties(ActiveDatabase);
end;


{**
  Refresh whole database tree
}
procedure TMDIChild.menuRefreshDBTreeClick(Sender: TObject);
begin
  RefreshTree(True);
end;


{**
  Refresh the whole tree
}
procedure TMDIChild.RefreshTree(DoResetTableCache: Boolean; SelectDatabase: WideString = '');
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
procedure TMDIChild.RefreshTreeDB(db: WideString);
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
function TMDIChild.FindDBNode(db: WideString): PVirtualNode;
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
procedure TMDIChild.menuTreeExpandAllClick(Sender: TObject);
begin
  DBtree.FullExpand;
  DBtree.ScrollIntoView(DBtree.GetFirstSelected, False);
end;

{**
  Collapse all db nodes
}
procedure TMDIChild.menuTreeCollapseAllClick(Sender: TObject);
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


function TMDIChild.GetTableSize(ds: TDataSet): Int64;
var
  d, i: String;
begin
  d := FieldContent(ds, 'Data_length');
  i := FieldContent(ds, 'Index_length');
  if (d = '') or (i = '') then Result := -1
  else Result := MakeInt(d) + MakeInt(i);
end;


function TMDIChild.DbTableListCached(db: WideString): Boolean;
begin
  Result := CachedTableLists.IndexOf(db) > -1;
end;

procedure TMDIChild.editFilterSearchChange(Sender: TObject);
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


procedure TMDIChild.ListColumnsDblClick(Sender: TObject);
begin
  Mainform.actEditField.Execute;
end;


procedure TMDIChild.SynMemoFilterChange(Sender: TObject);
var
  SomeText: Boolean;
begin
  SomeText := (SynMemoFilter.GetTextLen > 0) or (editFilterSearch.Text <> '');
  Mainform.actClearFilterEditor.Enabled := SomeText;
end;


procedure TMDIChild.ToggleFilterPanel(ForceVisible: Boolean = False);
var
  ShowIt: Boolean;
begin
  ShowIt := ForceVisible or (not pnlFilter.Visible);
  tbtnDataFilter.Down := ShowIt;
  pnlFilter.Visible := ShowIt;
end;


procedure TMDIChild.editFilterSearchEnter(Sender: TObject);
begin
  // Enables triggering apply button with Enter
  btnFilterApply.Default := True;
end;


procedure TMDIChild.editFilterSearchExit(Sender: TObject);
begin
  btnFilterApply.Default := False;
end;


procedure TMDIChild.EnsureNodeLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode; WhereClause: String);
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
    MainForm.ShowStatus('Retrieving data...');
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
    MainForm.ShowStatus('Filling grid with record-data...');
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

    MainForm.ShowStatus( STATUS_MSG_READY );
    FreeAndNil(ds);
  end;
end;

procedure TMDIChild.EnsureChunkLoaded(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  res: PGridResult;
  start, limit: Cardinal;
  query: WideString;
  ds: TDataSet;
  i, j: LongInt;
  reg: TRegistry;
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
    reg := TRegistry.Create;
    reg.OpenKey( REGPATH + REGKEY_SESSIONS + FConn.Description, true );
    regCrashIndicName := Utf8Encode(REGPREFIX_CRASH_IN_DATA + ActiveDatabase + '.' + SelectedTable);
    reg.WriteBool(regCrashIndicName, True);

    // start query
    MainForm.ShowStatus('Retrieving data...');
    debug(Format('mem: loading data chunk from row %d to %d', [start, limit]));
    ds := GetResults(query);
    if Cardinal(ds.RecordCount) < limit then begin
      limit := ds.RecordCount;
      TVirtualStringTree(Sender).RootNodeCount := start + limit;
      SetLength(res.Rows, start + limit);
    end;
    debug(Format('mem: loaded data chunk from row %d to %d', [start, limit]));

    // Query was completed successfully. Reset crash indicator.
    reg.DeleteValue(regCrashIndicName);
    reg.CloseKey;
    reg.Free;

    // fill in data
    MainForm.ShowStatus('Filling grid with record-data...');
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

    MainForm.ShowStatus( STATUS_MSG_READY );
    FreeAndNil(ds);
  end;
end;

procedure TMDIChild.DiscardNodeData(Sender: TVirtualStringTree; Node: PVirtualNode);
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
procedure TMDIChild.GridGetText(Sender: TBaseVirtualTree; Node:
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
  if prefEnableNullBG and (c.IsNull or c.NewIsNull) then begin
    // Don't display any text if NULL background was activated. In most cases better readable
    CellText := '';
  end else if c.Modified then begin
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


procedure TMDIChild.CalcNullColors;
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
procedure TMDIChild.GridPaintText(Sender: TBaseVirtualTree; const
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

  // Do not apply any color on a selected, highlighted node to keep readability
  if vsSelected in Node.States then
    Exit;

  // NULL value
  isNull := r.Rows[Node.Index].Cells[Column].IsNull;
  // Numeric field
  if r.Columns[Column].isInt or r.Columns[Column].isFloat then
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


procedure TMDIChild.DataGridAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  // Don't waist time
  if Column = -1 then Exit;
  if Node.Index >= Cardinal(Length(FDataGridResult.Rows)) then Exit;
  // Paint a red triangle at the top left corner of the cell
  if FDataGridResult.Rows[Node.Index].Cells[Column].Modified then
    Mainform.PngImageListMain.Draw(TargetCanvas, CellRect.Left, CellRect.Top, 111);
end;


{**
  Header column in datagrid clicked.
  Left button: handle ORDER BY
  Right button: show column selection box
}
procedure TMDIChild.DataGridHeaderClick(Sender: TVTHeader; Column:
    TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  c : TOrderCol;
  frm: TForm;
begin
  if Button = mbLeft then begin
    c := TOrderCol.Create;
    c.ColumnName := Sender.Columns[Column].Text;
    HandleOrderColumns(c);
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
procedure TMDIChild.setNULL1Click(Sender: TObject);
begin
  if not CheckUniqueKeyClause then
    Exit;
  DataGrid.Text[DataGrid.FocusedNode, DataGrid.FocusedColumn] := '';
  FDataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].NewIsNull := True;
  FDataGridResult.Rows[DataGrid.FocusedNode.Index].Cells[DataGrid.FocusedColumn].Modified := True;
  FDataGridResult.Rows[DataGrid.FocusedNode.Index].State := grsModified;
  DataGridHasChanges := True;
  DataGrid.RepaintNode(DataGrid.FocusedNode);
  ValidateControls;
end;


{**
  Content of a grid cell was modified
}
procedure TMDIChild.DataGridNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  Row: PGridRow;
begin
  Row := @FDataGridResult.Rows[Node.Index];
  // Remember new value
  Row.Cells[Column].NewText := NewText;
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
function TMDIChild.CheckUniqueKeyClause: Boolean;
var
  mres: Integer;
begin
  Result := GetKeyColumns.Count > 0;
  if not Result then begin
    mres := MessageDlg('Grid editing is blocked because this table does not have a primary '+
      'or a unique key, or it only contains a unique key which allows NULLs which turns that '+
      'key to be non unique again. You can create or edit the keys using the index manager.'+CRLF+CRLF+
      'Press'+CRLF+
      '  [Ok] to cancel editing and call the index manager'+CRLF+
      '  [Cancel] to cancel editing.',
      mtWarning, [mbOK, mbCancel], 0);
    if mres = mrOK then
      MainForm.actEditIndexesExecute(DataGrid);
  end;
end;


{**
  DataGrid: node focus has changed
}
procedure TMDIChild.DataGridChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  ValidateControls;
end;


{**
  DataGrid: node and/or column focus is about to change. See if we allow that.
}
procedure TMDIChild.DataGridFocusChanging(Sender: TBaseVirtualTree; OldNode,
    NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed:
    Boolean);
begin
  // Detect changed focus and update row
  if Assigned(OldNode) and Assigned(NewNode) and (OldNode <> NewNode) then
    Allowed := DataGridPostUpdateOrInsert(OldNode)
  else
    Allowed := True;
end;


{**
  DataGrid: invoke update or insert routine
}
function TMDIChild.DataGridPostUpdateOrInsert(Node: PVirtualNode): Boolean;
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
function TMDIChild.GridPostUpdate(Sender: TBaseVirtualTree): Boolean;
var
  i: Integer;
  sql, Val: WideString;
  Row: PGridRow;
begin
  sql := 'UPDATE '+mask(SelectedTable)+' SET';
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
    ExecUpdateQuery(sql, False, True);
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
procedure TMDIChild.GridFinalizeEditing(Sender: TBaseVirtualTree);
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
function TMDIChild.GetWhereClause(Row: PGridRow; Columns: PGridColumns): WideString;
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
function TMDIChild.GetKeyColumns: WideStrings.TWideStringlist;
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
procedure TMDIChild.DataGridInsertRow;
var
  i, j: Integer;
begin
  i := Length(FDataGridResult.Rows);
  SetLength(FDataGridResult.Rows, i+1);
  SetLength(FDataGridResult.Rows[i].Cells, Length(FDataGridResult.Columns));
  FDataGridResult.Rows[i].State := grsInserted;
  for j := 0 to Length(FDataGridResult.Rows[i].Cells) - 1 do begin
    FDataGridResult.Rows[i].Cells[j].Text := '';
  end;
  DataGrid.AddChild(nil);
  DataGrid.FocusedNode := DataGrid.GetLast;
  DataGrid.ClearSelection;
  DataGrid.Selected[DataGrid.FocusedNode] := True;
  DataGridHasChanges := True;
  ValidateControls;
end;


{**
  DataGrid: compose and fire INSERT query
}
function TMDIChild.GridPostInsert(Sender: TBaseVirtualTree): Boolean;
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
    sql := 'INSERT INTO '+mask(SelectedTable)+' ('+Cols+') VALUES ('+Vals+')';
    // Send INSERT query
    ExecUpdateQuery(sql, False, True);
    Result := True;
    Row.Loaded := false;
    EnsureNodeLoaded(Sender, Node, GetWhereClause(Row, @FDataGridResult.Columns));
    GridFinalizeEditing(Sender);
  end;
end;


{**
  DataGrid: compose and fire DELETE query
}
function TMDIChild.GridPostDelete(Sender: TBaseVirtualTree): Boolean;
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
procedure TMDIChild.DataGridCancel(Sender: TObject);
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
      Mainform.actDataLastExecute(Sender);
    end;
  end;
  DataGridHasChanges := False;
  ValidateControls;
end;



procedure TMDIChild.GridKeyDown(Sender: TObject; var Key: Word; Shift:
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
      Mainform.actDataInsertExecute(Sender);
  end;
end;


// TODO: Version of EnsureFullWidth() that fetches all width limited columns
//       for a row, and fetches 500 rows at a time, for use with GridTo{Xml,Csv,Html}.
//       Would reduce number of database roundtrips; also the per-query overhead
//       right now is horrendous for some reason (thinking mysqlquerythread).
procedure TMDIChild.EnsureFullWidth(Grid: TBaseVirtualTree; Column: TColumnIndex; Node: PVirtualNode);
var
  Data: PGridResult;
  Cell: PGridCell;
  Row: PGridRow;
  Col: PGridColumn;
  sql: WideString;
  len: Int64;
  ds: TDataSet;
begin
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
    sql :=
      'SELECT ' + mask(Col.Name) +
      ' FROM ' + mask(SelectedTable) +
      ' WHERE ' + GetWhereClause(Row, @Data.Columns)
    ;
    ds := GetResults(sql);
    if Col.IsBinary then Cell.Text := '0x' + BinToWideHex(ds.Fields[0].AsString)
    else Cell.Text := ds.Fields[0].AsWideString;
    Cell.IsNull := ds.Fields[0].IsNull;
  end;
end;

procedure TMDIChild.DataGridEditing(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
  if FDataGridResult.Rows[Node.Index].State = grsDefault then
    Allowed := CheckUniqueKeyClause;
  if Allowed then begin
    // Move Esc shortcut from "Cancel row editing" to "Cancel cell editing"
    Mainform.actDataCancelChanges.ShortCut := 0;
    Mainform.actDataPostChanges.ShortCut := 0;
    EnsureFullWidth(Sender, Column, Node);
  end;
end;

procedure TMDIChild.DataGridEdited(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex);
begin
  // Reassign Esc to "Cancel row editing" action
  Mainform.actDataCancelChanges.ShortCut := TextToShortcut('Esc');
  Mainform.actDataPostChanges.ShortCut := TextToShortcut('Ctrl+Enter');
  AutoCalcColWidths(DataGrid, PrevTableColWidths);
end;

procedure TMDIChild.DataGridEditCancelled(Sender: TBaseVirtualTree; Column:
    TColumnIndex);
begin
  // Reassign Esc to "Cancel row editing" action
  Mainform.actDataCancelChanges.ShortCut := TextToShortcut('Esc');
  Mainform.actDataPostChanges.ShortCut := TextToShortcut('Ctrl+Enter');
end;

procedure TMDIChild.DataGridCreateEditor(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  MemoEditor: TMemoEditorLink;
  DateTimeEditor: TDateTimeEditorLink;
  EnumEditor: TEnumEditorLink;
  SetEditor: TSetEditorLink;
begin
  if
    (FDataGridResult.Columns[Column].IsText and prefEnableTextEditor) or
    (FDataGridResult.Columns[Column].IsBinary and prefEnableBinaryEditor)
  then begin
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
  end else
    EditLink := TStringEditLink.Create;
end;


function TMDIChild.GetSelTableColumns: TDataset;
begin
  if FLastSelectedTableColumns = nil then
    FLastSelectedTableColumns := GetResults( 'SHOW /*!32332 FULL */ COLUMNS FROM ' + mask(SelectedTable), false );
  Result := FLastSelectedTableColumns;
end;

function TMDIChild.GetSelTableKeys: TDataset;
begin
  if FLastSelectedTableKeys = nil then
    FLastSelectedTableKeys := GetResults( 'SHOW KEYS FROM ' + mask(SelectedTable) );
  Result := FLastSelectedTableKeys;
end;


procedure TMDIChild.menuShowSizeColumnClick(Sender: TObject);
var
  reg: TRegistry;
  NewVal: Boolean;
begin
  NewVal := not TMenuItem(Sender).Checked;
  TMenuItem(Sender).Checked := newVal;
  if NewVal then
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options + [coVisible]
  else
    DBtree.Header.Columns[1].Options := DBtree.Header.Columns[1].Options - [coVisible];
  reg := TRegistry.Create;
  reg.OpenKey(REGPATH, true);
  reg.WriteBool(REGNAME_SIZECOL_TREE, NewVal);
  reg.CloseKey;
  FreeAndNil(reg);
end;


procedure TMDIChild.AutoCalcColWidths(Tree: TVirtualStringTree; PrevLayout: Widestrings.TWideStringlist = nil);
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


procedure TMDIChild.DataGridColumnResize(Sender: TVTHeader;
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


procedure TMDIChild.DBtreeClick(Sender: TObject);
begin
  // Auto resize "Size" column in dbtree when needed
  if coVisible in DBTree.Header.Columns[1].Options then
    DBTree.Header.AutoFitColumns(False, smaUseColumnOption, 1, 1);
end;


procedure TMDIChild.GridBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  gr: PGridResult;
begin
  if Column = -1 then
    Exit;
  if Sender = DataGrid then gr := @FDataGridResult
  else gr := @FQueryGridResult;
  if prefEnableNullBG and gr.Rows[Node.Index].Cells[Column].IsNull then begin
    TargetCanvas.Brush.Color := prefNullBG;
    TargetCanvas.FillRect(CellRect);
  end;
end;


end.
