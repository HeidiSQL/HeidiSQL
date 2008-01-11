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
  Buttons, CheckLst, ToolWin, Db, DBGrids,
  DBCtrls, helpers,
  Grids, ZDataset,
  ZAbstractRODataset, ZConnection,
  ZSqlMonitor, EDBImage, ZDbcLogging,
  SynCompletionProposal, HeidiComp, SynEditMiscClasses, MysqlQuery,
  MysqlQueryThread, queryprogress, communication, MysqlConn, Tabs,
  VirtualTrees, createdatabase, tbl_properties, createtable, TntDBGrids, TntClasses,
  SynUnicode;

type
  TOrderCol = class(TObject)
    ColumnName: String;
    SortDirection: Byte;
  end;
  TOrderColArray = Array of TOrderCol;

type
  TMDIChild = class(TForm)
    Panel1: TPanel;
    DBtree: TTreeView;
    Splitter1: TSplitter;
    TableShow: TPanel;
    PageControlMain: TPageControl;
    tabData: TTabSheet;
    tabDatabase: TTabSheet;
    Splitter2: TSplitter;
    tabQuery: TTabSheet;
    popupTreeView: TPopupMenu;
    Drop1: TMenuItem;
    pnlDatabaseTop: TPanel;
    tabTable: TTabSheet;
    pnlTableTop: TPanel;
    popupDbGrid: TPopupMenu;
    menuviewdata: TMenuItem;
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
    Panel4: TPanel;
    pnlDataTop: TPanel;
    pnlQueryTop: TPanel;
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
    MenuViewBlob: TMenuItem;
    TimerConnected: TTimer;
    N12: TMenuItem;
    popupSqlLog: TPopupMenu;
    Clear2: TMenuItem;
    Copy1: TMenuItem;
    N13: TMenuItem;
    EditQuery1: TMenuItem;
    Markall3: TMenuItem;
    N15: TMenuItem;
    MenuOptimize: TMenuItem;
    MenuCheck: TMenuItem;
    MenuAnalyze: TMenuItem;
    MenuRepair: TMenuItem;
    More1: TMenuItem;
    TimerConnectErrorCloseWindow: TTimer;
    PopupMenuDropTable: TMenuItem;
    N17: TMenuItem;
    pnlTableToolbar: TPanel;
    ListColumns: TVirtualStringTree;
    CopycontentsasHTML1: TMenuItem;
    CopycontentsasHTML2: TMenuItem;
    Copy3: TMenuItem;
    Paste2: TMenuItem;
    N4: TMenuItem;
    gridData: TTntDBGrid;
    DataSource1: TDataSource;
    gridQuery: TTntDBGrid;
    DataSource2: TDataSource;
    Copytableas1: TMenuItem;
    Filter1: TMenuItem;
    MenuLimit: TMenuItem;
    Delete1: TMenuItem;
    N6: TMenuItem;
    QF1: TMenuItem;
    QF2: TMenuItem;
    QuickFilter1: TMenuItem;
    QF3: TMenuItem;
    QF4: TMenuItem;
    N7: TMenuItem;
    DropFilter1: TMenuItem;
    Table1: TMenuItem;
    popupFilterOpenFile: TPopupMenu;
    OpenDialog2: TOpenDialog;
    PrintList2: TMenuItem;
    PrintList3: TMenuItem;
    PrintList4: TMenuItem;
    N1: TMenuItem;
    MenuCopyTable: TMenuItem;
    PageControlBottom: TPageControl;
    tabSQLLog: TTabSheet;
    tabBlobEditor: TTabSheet;
    tabFilter: TTabSheet;
    SynMemoSQLLog: TSynMemo;
    ToolBar3: TToolBar;
    btnBlobWordWrap: TToolButton;
    btnBlobLoad: TToolButton;
    btnBlobSave: TToolButton;
    PageControlBlobEditors: TPageControl;
    tabBlobEditorText: TTabSheet;
    DBMemo1: TDBMemo;
    tabBlobEditorImage: TTabSheet;
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
    pnlQueryToolbar: TPanel;
    ToolBarQuery: TToolBar;
    btnQueryRun: TToolButton;
    btnQueryRunSelected: TToolButton;
    btnQueryLoad: TToolButton;
    btnQuerySave: TToolButton;
    btnQueryFind: TToolButton;
    PanelCharsInQueryWindow: TPanel;
    btnQueryStopOnErrors: TToolButton;
    Panel10: TPanel;
    ComboBoxWhereFilters: TComboBox;
    ToolBar4: TToolBar;
    btnFilterSet: TToolButton;
    btnFilterLoad: TToolButton;
    btnFilterSave: TToolButton;
    btnFilterClear: TToolButton;
    sepFilter1: TToolButton;
    btnFilterPrevious: TToolButton;
    btnFilterNext: TToolButton;
    QF8: TMenuItem;
    QF10: TMenuItem;
    QF11: TMenuItem;
    QF9: TMenuItem;
    QF12: TMenuItem;
    CopyasXMLdata1: TMenuItem;
    CopyasXMLdata2: TMenuItem;
    Exportdata1: TMenuItem;
    Exportdata2: TMenuItem;
    SaveDialogExportData: TExportSaveDialog;
    N11: TMenuItem;
    ProgressBarQuery: TProgressBar;
    btnQueryReplace: TToolButton;
    Copy4: TMenuItem;
    N14: TMenuItem;
    DataInsertDateTime: TMenuItem;
    DataTimestamp: TMenuItem;
    DataDateTime: TMenuItem;
    DataTime: TMenuItem;
    DataDate: TMenuItem;
    DataYear: TMenuItem;
    ViewasHTML1: TMenuItem;
    btnBlobViewAsHtml: TToolButton;
    HTMLview1: TMenuItem;
    btnBlobCopy: TToolButton;
    ScrollBox1: TScrollBox;
    InsertfilesintoBLOBfields1: TMenuItem;
    InsertfilesintoBLOBfields2: TMenuItem;
    InsertfilesintoBLOBfields3: TMenuItem;
    N19: TMenuItem;
    setNULL1: TMenuItem;
    ZSQLMonitor1: TZSQLMonitor;
    EDBImage1: TEDBImage;
    Exporttables1: TMenuItem;
    Exporttables2: TMenuItem;
    EditDataSearch: TEdit;
    ButtonDataSearch: TButton;
    Find1: TMenuItem;
    popupDbGridHeader: TPopupMenu;
    SynCompletionProposal1: TSynCompletionProposal;
    popupQueryLoad: TPopupMenu;
    OpenDialogSQLFile: TOpenDialog;
    SaveDialogSQLFile: TSaveDialog;
    btnQuerySaveSnippet: TToolButton;
    FindDialogQuery: TFindDialog;
    SynEditSearch1: TSynEditSearch;
    ReplaceDialogQuery: TReplaceDialog;
    N16: TMenuItem;
    ManageIndexes1: TMenuItem;
    tabCommandStats: TTabSheet;
    ListCommandStats: TVirtualStringTree;
    CheckBoxDataSearch: TCheckBox;
    QF13: TMenuItem;
    QF14: TMenuItem;
    QF15: TMenuItem;
    QF16: TMenuItem;
    QF17: TMenuItem;
    N21: TMenuItem;
    btnUnsafeEdit: TToolButton;
    btnColumnSelection: TSpeedButton;
    pnlQueryHelpers: TPanel;
    tabsetQueryHelpers: TTabSet;
    lboxQueryHelpers: TListBox;
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
    lblDataTop: TLabel;
    btnDataSorting: TSpeedButton;
    spltQueryHelpers: TSplitter;
    menuRenameColumn: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    menuInsertFileAtCursor: TMenuItem;
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
    pnlDatabaseToolbar: TPanel;
    tlbDataLeft2: TToolBar;
    tlbDataLeft1: TToolBar;
    btnDbViewData: TToolButton;
    btnDbProperties: TToolButton;
    btnDbEmptyTable: TToolButton;
    btnDbDropTable: TToolButton;
    btnDbCopyTable: TToolButton;
    tlbTableLeft1: TToolBar;
    btnTableViewData: TToolButton;
    btnTableEditField: TToolButton;
    btnTableAddField: TToolButton;
    btnTableDropField: TToolButton;
    btnTableManageIndexes: TToolButton;
    tlbTableLeft2: TToolBar;
    menuSQLhelp: TMenuItem;
    N24: TMenuItem;
    menuSQLhelpData: TMenuItem;
    menuAlterdatabase: TMenuItem;
    PanelQueryDelimiter: TPanel;
    LabelQueryDelimiter: TLabel;
    ComboBoxQueryDelimiter: TComboBox;
    menuTreeAlterTable: TMenuItem;
    popupFilter: TPopupMenu;
    menuApplyFilter: TMenuItem;
    menuFilterCopy: TMenuItem;
    menuFilterPaste: TMenuItem;
    menuFilterClear: TMenuItem;
    N8: TMenuItem;
    N20: TMenuItem;
    menuFilterSQLhelp: TMenuItem;
    N25: TMenuItem;
    menuLogToFile: TMenuItem;
    menuOpenLogFolder: TMenuItem;
    procedure DBtreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure DBtreeChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure menuRenameColumnClick(Sender: TObject);
    procedure ListColumnsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
    procedure menuclearClick(Sender: TObject);
    procedure popupQueryPopup(Sender: TObject);
    procedure lboxQueryHelpersClick(Sender: TObject);
    procedure lboxQueryHelpersDblClick(Sender: TObject);
    procedure tabsetQueryHelpersChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure btnTableViewDataClick(Sender: TObject);
    procedure btnDataClick(Sender: TObject);
    procedure DBtreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure ListTablesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ListColumnsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DBMemo1Exit(Sender: TObject);
    procedure btnUnsafeEditClick(Sender: TObject);
    procedure gridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure controlsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CallSQLHelp(Sender: TObject);
    procedure CallSQLHelpWithKeyword(keyword: String);
    procedure ManageIndexes1Click(Sender: TObject);
    procedure ZQueryGridAfterPost(DataSet: TDataSet);
    procedure btnQueryReplaceClick(Sender: TObject);
    procedure ReplaceDialogQueryReplace(Sender: TObject);
    procedure ReplaceDialogQueryFind(Sender: TObject);
    procedure FindDialogQueryFind(Sender: TObject);
    procedure btnQuerySaveSnippetClick(Sender: TObject);
    procedure SynCompletionProposal1AfterCodeCompletion(Sender: TObject;
      const Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1CodeCompletion(Sender: TObject;
      var Value: WideString; Shift: TShiftState; Index: Integer; EndToken: WideChar);
    procedure SynCompletionProposal1Execute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
      var CanExecute: Boolean);
    procedure PerformConnect;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReadDatabasesAndTables(Sender: TObject);
    procedure DBtreeChange(Sender: TObject; Node: TTreeNode);
    procedure pcChange(Sender: TObject);
    procedure ValidateControls(FrmIsFocussed: Boolean = true);
    procedure LoadDatabaseProperties(db: string);
    procedure ShowHost;
    procedure ShowDatabase(db: String);
    procedure ShowDBProperties(db: String);
    procedure ShowTable(table: String; tab: TTabSheet = nil);
    procedure ShowTableProperties(table: string);
    procedure ShowTableData(table: string);
    procedure viewdata(Sender: TObject);
    procedure RefreshFieldListClick(Sender: TObject);
    procedure MenuViewDataClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
    procedure EmptyTable(Sender: TObject);
    procedure DropDB(Sender: TObject);
    procedure LogSQL(msg: String = ''; comment: Boolean = true );
    procedure ShowVariablesAndProcesses(Sender: TObject);
    procedure CreateDatabase(Sender: TObject);
    procedure KillProcess(Sender: TObject);
    procedure PageControlHostChange(Sender: TObject);
    procedure ExecSQLClick(Sender: TObject; Selection: Boolean = false;
      CurrentLine: Boolean=false);
    procedure DropField(Sender: TObject);
    procedure SynMemoQueryChange(Sender: TObject);
    procedure CreateTable(Sender: TObject);
    procedure TimerHostUptimeTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateField(Sender: TObject);
    procedure menuAlterTableClick(Sender: TObject);
    procedure ListTablesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
    procedure MenuRenameTableClick(Sender: TObject);
    procedure MenuViewBlobClick(Sender: TObject);
    procedure TimerConnectedTimer(Sender: TObject);
    procedure Clear2Click(Sender: TObject);
    procedure EditQuery1Click(Sender: TObject);
    procedure Markall3Click(Sender: TObject);
    procedure ReadWindowOptions;
    procedure More1Click(Sender: TObject);
    procedure MenuOptimizeClick(Sender: TObject);
    procedure MenuCheckClick(Sender: TObject);
    procedure MenuAnalyzeClick(Sender: TObject);
    procedure MenuRepairClick(Sender: TObject);
    procedure ListTablesDblClick(Sender: TObject);
    procedure TimerConnectErrorCloseWindowTimer(Sender: TObject);
    procedure gridDataTitleClick(Column: TColumn);
    procedure Filter1Click(Sender: TObject);
    procedure MenuLimitClick(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure QuickFilterClick(Sender: TObject);
    procedure btnBlobWordWrapClick(Sender: TObject);
    procedure PageControlBlobEditorsChange(Sender: TObject);
    procedure btnBlobSaveClick(Sender: TObject);
    procedure btnBlobLoadClick(Sender: TObject);
    procedure btnFilterLoadClick(Sender: TObject);
    procedure btnFilterSaveClick(Sender: TObject);
    procedure setFilter(Sender: TObject);
    procedure ClearFilter(Sender: TObject);
    procedure LoadSQLWhereFile(Sender: TObject);
    procedure DropFilter1Click(Sender: TObject);
    procedure selectall1Click(Sender: TObject);
    procedure popupResultGridPopup(Sender: TObject);
    procedure gridDataColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure Autoupdate1Click(Sender: TObject);
    procedure EnableAutoRefreshClick(Sender: TObject);
    procedure ShowProcessList(sender: TObject);
    procedure DisableAutoRefreshClick(Sender: TObject);
    procedure SynMemoQueryDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoQueryDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoQueryDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TWideStrings);
    procedure SynMemoQueryKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynMemoQueryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure popupHostPopup(Sender: TObject);
    procedure Saveastextfile1Click(Sender: TObject);
    procedure popupTreeViewPopup(Sender: TObject);
    procedure btnQueryFindClick(Sender: TObject);
    procedure btnQuerySaveClick(Sender: TObject);
    procedure btnQueryLoadClick(Sender: TObject);
    procedure btnFilterPreviousClick(Sender: TObject);
    procedure btnFilterNextClick(Sender: TObject);
    procedure ComboBoxWhereFiltersChange(Sender: TObject);
    procedure btnQueryStopOnErrorsClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure SaveDialogExportDataTypeChange(Sender: TObject);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol:
        Integer; Column: TColumn; State: TGridDrawState);
    procedure popupDataGridPopup(Sender: TObject);
    procedure InsertDate(Sender: TObject);
    procedure btnBlobCopyClick(Sender: TObject);
    procedure setNULL1Click(Sender: TObject);
    procedure MenuAddFieldClick(Sender: TObject);
    procedure ZQueryGridBeforeClose(DataSet: TDataSet);
    function GetNamedVar( SQLQuery: String; x: String;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : String;
    function GetVar( SQLQuery: String; x: Integer = 0;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : String;
    function GetResults( SQLQuery: String;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false ): TDataSet;
    function GetCol( SQLQuery: String; x: Integer = 0;
      HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : TStringList;
    procedure ZSQLMonitor1LogTrace(Sender: TObject; Event: TZLoggingEvent);
    procedure ResizeImageToFit;
    procedure Splitter2Moved(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure DBGridColEnter(Sender: TObject);
    procedure ButtonDataSearchClick(Sender: TObject);
    procedure EditDataSearchEnter(Sender: TObject);
    procedure EditDataSearchExit(Sender: TObject);
    procedure MenuTablelistColumnsClick(Sender: TObject);
    function mask(str: String) : String;
    procedure CheckConnection();
    procedure QueryLoad( filename: String; ReplaceContent: Boolean = true );
    procedure AddOrRemoveFromQueryLoadHistory( filename: String;
      AddIt: Boolean = true; CheckIfFileExists: Boolean = true );
    procedure popupQueryLoadClick( sender: TObject );
    procedure FillPopupQueryLoad;
    procedure PopupQueryLoadRemoveAbsentFiles( sender: TObject );
    procedure ExecuteNonQuery(SQLQuery: String);
    function ExecuteQuery(query: String): TDataSet;
    function CreateOrGetRemoteQueryTab(sender: THandle): THandle;
    procedure menuDeleteSnippetClick(Sender: TObject);
    function GetCalculatedLimit( Table: String ): Int64;
    procedure menuExploreClick(Sender: TObject);
    procedure menuInsertFileAtCursorClick(Sender: TObject);
    procedure menuInsertSnippetAtCursorClick(Sender: TObject);
    procedure menuLoadSnippetClick(Sender: TObject);
    procedure menuAlterdatabaseClick(Sender: TObject);
    procedure menuSQLhelpClick(Sender: TObject);
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
    procedure ComboBoxQueryDelimiterExit(Sender: TObject);
    procedure ComboBoxQueryDelimiterAdd(delimiter: String);
    procedure menuLogToFileClick(Sender: TObject);
    procedure menuOpenLogFolderClick(Sender: TObject);
    procedure vstGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var
        HintText: WideString);
    procedure popupFilterPopup(Sender: TObject);
    procedure ProcessSqlLog;
    procedure ListCommandStatsBeforeCellPaint(Sender: TBaseVirtualTree;
        TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect:
        TRect);

    private
      methodStack                : TStack;
      strHostRunning             : String;
      strHostNotRunning          : String;
      uptime                     : Integer;
      time_connected             : Cardinal;
      OnlyDBs,
      OnlyDBs2                   : TStringList;    // used on connecting
      viewingdata                : Boolean;
      WhereFilters               : TStringList;
      WhereFiltersIndex          : Integer;
      StopOnErrors, WordWrap     : Boolean;
      FMysqlConn                 : TMysqlConn;
      FConn                      : TOpenConnProf;
      QueryRunningInterlock      : Integer;
      lastUsedDB                 : String;
      UserQueryFired             : Boolean;
      UserQueryFiring            : Boolean;
      CachedTableLists           : TStringList;
      QueryHelpersSelectedItems  : Array[0..3] of Integer;
      CreateDatabaseForm         : TCreateDatabaseForm;
      TablePropertiesForm        : Ttbl_properties_form;
      CreateTableForm            : TCreateTableForm;
      FileNameSessionLog         : String;
      FileHandleSessionLog       : Textfile;
      SqlMessages                : TStringList;
      SqlMessagesLock            : TRtlCriticalSection;
      dsShowEngines,
      dsHaveEngines              : TDataSet;

      function GetQueryRunning: Boolean;
      procedure SetQueryRunning(running: Boolean);
      procedure GridHighlightChanged(Sender: TObject);
      procedure SaveBlob;
      function GetActiveGrid: TTntDBGrid;
      procedure WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery);
      function RunThreadedQuery(AQuery : String) : TMysqlQuery;
      procedure DisplayRowCountStats(ds: TDataSet);
      procedure insertFunction(Sender: TObject);
      function GetActiveDatabase: string;
      function GetSelectedTable: string;
      procedure SetSelectedDatabase(db: string);
      procedure SetSelectedTable(table: string);
      procedure ProcessClientSQL(command: String; parameter: String);
      procedure SaveListSetup( List: TVirtualStringTree );
      procedure RestoreListSetup( List: TVirtualStringTree );
      procedure SetVisibleListColumns( List: TVirtualStringTree; Columns: TStringList );
      procedure DisableTreeEvents;
      procedure EnableTreeEvents(invokeChanged: TTreeNode = nil);
      function FindTreeViewAbsoluteIndex(n: TTreeNode): Integer;

    public
      TemporaryDatabase          : String;
      dataselected               : Boolean;
      editing                    : Boolean;
      mysql_version              : Integer;
      tnodehost                  : TTreeNode;
      Description                : String;
      delimiter                  : String;
      DBRightClickSelectedItem   : TTreeNode;    // TreeNode for dropping with right-click
      VTRowDataListVariables,
      VTRowDataListProcesses,
      VTRowDataListCommandStats,
      VTRowDataListTables,
      VTRowDataListColumns       : TVTreeDataArray;

      FProgressForm              : TFrmQueryProgress;

      // Variables set by preferences dialog
      prefRememberFilters,
      prefConvertHTMLEntities    : Boolean;
      prefLogsqlnum,
      prefLogSqlWidth,
      prefDefaultColWidth        : Integer;
      prefCSVSeparator,
      prefCSVEncloser,
      prefCSVTerminator          : String[10];
      prefLogToFile              : Boolean;


      procedure Init(AConn : POpenConnProf; AMysqlConn : TMysqlConn);
      //procedure HandleQueryNotification(ASender : TMysqlQuery; AEvent : Integer);
      function GetVisualDataset() : TDataSet;

      function ExecUpdateQuery(sql: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): Int64;
      function ExecSelectQuery(sql: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): TDataSet;
      procedure ExecUseQuery(db: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false);

      property FQueryRunning: Boolean read GetQueryRunning write SetQueryRunning;
      property ActiveGrid: TTntDBGrid read GetActiveGrid;
      property MysqlConn : TMysqlConn read FMysqlConn;
      property Conn : TOpenConnProf read FConn;

      property ActiveDatabase : string read GetActiveDatabase write SetSelectedDatabase;
      property SelectedTable : string read GetSelectedTable write SetSelectedTable;

      function FetchActiveDbTableList: TDataSet;
      function RefreshActiveDbTableList: TDataSet;
      function FetchDbTableList(db: string): TDataSet;
      function RefreshDbTableList(db: string): TDataSet;
      procedure ClearAllTableLists;
      function PopulateTreeTableList(tndb: TTreeNode = nil; find: string = ''; ForceRefresh: Boolean = false): TTreeNode;
      procedure EnsureDatabase;
      procedure TestVTreeDataArray( P: PVTreeDataArray );
      function GetVTreeDataArray( VT: TBaseVirtualTree ): PVTreeDataArray;
      procedure ActivateFileLogging;
      procedure DeactivateFileLogging;
      procedure TrimSQLLog;
      function HandleOrderColumns( AddOrderCol: TOrderCol = nil ): TOrderColArray;
      function ComposeOrderClause( Cols: TOrderColArray ): String;
      procedure TableEnginesCombo(var Combobox: TCombobox);
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
  optimizetables, copytable, sqlhelp, printlist,
  column_selection, data_sorting, runsqlfile, mysql_structures,
  Registry;


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
  charset     : String;
  v           : String[10];
  versions    : TStringList;
begin
  try
    time_connected := 0;
    TimerConnected.Enabled := true;
    LogSQL( 'Connection established with host "' + FMysqlConn.Connection.hostname +
      '" on port ' + IntToStr(FMysqlConn.Connection.Port) );
    LogSQL( 'Connection-ID: ' + IntToStr( MySQLConn.Connection.GetThreadId ) );

    {***
      Detect server version
    }
    v := GetVar( 'SELECT VERSION()' );
    versions := explode( '.', v );
    mysql_version := MakeInt( versions[0] ) * 10000 + MakeInt( versions[1] ) *
      100 + MakeInt( versions[2] );
    strHostRunning := FConn.MysqlParams.Host + ' running MySQL-Version ' + v +
      ' / Uptime: %s';
    strHostNotRunning := 'Disconnected from ' + FConn.MysqlParams.Host + '.';
    // On Re-Connection, try to restore lost properties
    {***
      SET NAMES statement available since MySQL 4.1.0 .
      Older versions throw a SQL-error: "Unknown system variable 'NAMES'"
      @see http://lists.phpbar.de/pipermail/opengeodb/2005-September/002455.html
    }
    if ( mysql_version >= 40100 ) then
    begin
      charset := ConvertWindowsCodepageToMysqlCharacterSet( GetACP() );
      if ( charset = '' ) then
      begin
        LogSQL( 'Could not find a MySQL character set to match the current ' +
          'Windows ANSI codepage.', true );
        LogSQL( Format( 'Use SHOW CHARACTER SET to see MySQL character sets; ' +
          'if you can find one that you are certain matches %d, please report' +
          ' it via http://rfe.heidisql.com/.', [GetACP()] ), true );
      end
      else
      begin
        ExecuteNonQuery( 'SET NAMES ' + charset );
      end;
    end;

    if ( FMysqlConn.Connection.Database <> '' ) then
    begin
      ExecUseQuery( FMysqlConn.Connection.Database );
    end;
  except
    on E: Exception do
    begin
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
  winName          : String;
  i, j             : Integer;
  miGroup,
  miFilterGroup,
  miFunction,
  miFilterFunction : TMenuItem;
  functioncats     : TStringList;
  reg, reg2        : TRegistry;
begin
  QueryRunningInterlock := 0;
  UserQueryFired := False;
  UserQueryFiring := False;
  TemporaryDatabase := '';
  methodStack := TStack.Create;
  CachedTableLists := TStringList.Create;
  InitializeCriticalSection(SqlMessagesLock);
  EnterCriticalSection(SqlMessagesLock);
  SqlMessages := TStringList.Create;
  LeaveCriticalSection(SqlMessagesLock);

  FConn := AConn^;
  FMysqlConn := AMysqlConn; // we're now responsible to free it

  FConn.MysqlConn := FMysqlConn.Connection; // use this connection (instead of zConn)

  // Initialization: establish connection and read some vars from registry
  MainForm.Showstatus( 'Creating window...', 2, true );

  // Temporarily disable AutoReconnect in Registry
  // in case of unexpected application-termination
  AutoReconnect := false;
  reg := TRegistry.Create();
  with ( reg ) do
  begin
    OpenKey( REGPATH, true );
    if ( Valueexists( 'Autoreconnect' ) ) then
    begin
      if ( ReadBool( 'AutoReconnect' ) ) then
      begin
        AutoReconnect := true;
        WriteBool( 'AutoReconnect', false );
      end;
    end;
    CloseKey();
  end;
  FreeAndNil(reg);

  ReadWindowOptions();

  MainForm.Showstatus( 'Connecting to ' + FConn.MysqlParams.Host + '...', 2, true );

  try
    PerformConnect();
  except
    TimerConnectErrorCloseWindow.Enabled := true;
    Exit;
  end;

  Description := FMysqlConn.Description;;
  Caption := Description;
  OnlyDBs := explode( ';', FConn.DatabaseList );
  if ( FConn.DatabaseListSort ) then
  begin
    OnlyDBs.Sort();
  end;

  // Fill variables-list, processlist and DB-tree
  ShowVariablesAndProcesses( Self );
  ReadDatabasesAndTables( Self );

  // Re-enable AutoReconnect in Registry!
  if ( AutoReconnect ) then
  begin
    reg2 := TRegistry.Create();
    with ( reg2 )  do
    begin
      OpenKey( REGPATH, true );
      WriteBool( 'AutoReconnect', true );
      CloseKey();
    end;
    FreeAndNil(reg2);
  end;

  // Define window properties
  SetWindowConnected( true );
  i := SetWindowName( Description );
  winName := Description;
  if ( i <> 0 ) then
  begin
    winName := winName + Format( ' (%d)', [i] );
  end;
  Application.Title := winName + ' - ' + APPNAME;

  // Reselect last used database
  if ( ( MainForm.GetRegValue( 'RestoreLastUsedDB', true ) ) and ( lastUsedDB <> '' ) ) then
  begin
    try
      ActiveDatabase := lastUsedDB;
    except
      // Suppress exception message when db was dropped externally or
      // the session was just opened with "OnlyDBs" in place and the
      // last db is not contained in this list.
    end;
  end;

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
      miFunction.ImageIndex := 86;
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
  ws          : String;
  delimiters  : String;
  i           : Integer;
  menuitem    : Tmenuitem;
  reg         : TRegistry;

begin
  // Initialize delimiter settings
  delimiter := DEFAULT_DELIMITER;
  ComboBoxQueryDelimiter.ItemIndex := ComboBoxQueryDelimiter.Items.IndexOf( delimiter );

  reg := TRegistry.Create;
  if reg.OpenKey( REGPATH, true ) then
  begin
    ws := reg.ReadString( 'childwinstate' );
    if ws = 'Normal' then
    begin
      WindowState := wsNormal;
      if reg.ValueExists( 'childwinleft' ) then
      begin
        Left := reg.ReadInteger( 'childwinleft' );
        Top := reg.ReadInteger( 'childwintop' );
        Width := reg.ReadInteger( 'childwinwidth' );
        Height := reg.ReadInteger( 'childwinheight' );
      end;
    end
    else if ws = 'Minimized' then
      WindowState := wsMinimized
    else if ( ws = 'Maximized' ) then
      WindowState := wsMaximized;

    // Other values:
    if reg.ValueExists( 'querymemoheight' ) then
      pnlQueryMemo.Height := reg.ReadInteger('querymemoheight');
    if reg.ValueExists( 'queryhelperswidth' ) then
      pnlQueryHelpers.Width := reg.ReadInteger('queryhelperswidth');

    if reg.ValueExists( 'dbtreewidth' ) then
      DBtree.Width := reg.ReadInteger( 'dbtreewidth' );

    if reg.ValueExists( 'sqloutheight' ) then
      PageControlBottom.Height := reg.ReadInteger( 'sqloutheight' );

    if reg.ValueExists( 'DefaultColWidth' ) then
      prefDefaultColWidth := reg.ReadInteger( 'DefaultColWidth' )
    else
      prefDefaultColWidth := 100;

    if reg.ValueExists('logsqlnum') then
      prefLogsqlnum := reg.ReadInteger('logsqlnum')
    else
      prefLogsqlnum := 300;

    if reg.ValueExists('logsqlwidth') then
      prefLogSqlWidth := reg.ReadInteger('logsqlwidth')
    else
      prefLogSqlWidth := 2000;

    prefConvertHTMLEntities := true;
    if reg.Valueexists('ConvertHTMLEntities') then
      prefConvertHTMLEntities := reg.ReadBool('ConvertHTMLEntities');

    prefCSVSeparator := ',';
    prefCSVEncloser := '';
    prefCSVTerminator := '\r\n';
    if reg.Valueexists('CSVSeparator') then
      prefCSVSeparator := reg.ReadString('CSVSeparator');
    if reg.Valueexists('CSVEncloser') then
      prefCSVEncloser := reg.ReadString('CSVEncloser');
    if reg.Valueexists('CSVTerminator') then
      prefCSVTerminator := reg.ReadString('CSVTerminator');

    // SQL-Font:
    if reg.ValueExists( 'FontName' ) and reg.ValueExists('FontSize') then
    begin
      SynMemoQuery.Font.Name := reg.ReadString( 'FontName' );
      SynMemoSQLLog.Font.Name := reg.ReadString( 'FontName' );
      SynMemoQuery.Font.Size := reg.ReadInteger( 'FontSize' );
      SynMemoSQLLog.Font.Size := reg.ReadInteger( 'FontSize' );
    end;

    // Data-Font:
    if reg.ValueExists( 'DataFontName' ) and reg.ValueExists( 'DataFontSize' ) then
    begin
      gridData.Font.Name := reg.ReadString( 'DataFontName' );
      gridQuery.Font.Name := reg.ReadString( 'DataFontName' );
      DBMemo1.Font.Name := reg.ReadString( 'DataFontName' );
      gridData.Font.Size := reg.ReadInteger( 'DataFontSize' );
      gridQuery.Font.Size := reg.ReadInteger( 'DataFontSize' );
      DBMemo1.Font.Size := reg.ReadInteger( 'DataFontSize' );
    end;

    // Color coding:
    if reg.ValueExists( 'SQLColKeyAttri' ) then
      SynSQLSyn1.KeyAttri.Foreground := StringToColor( reg.ReadString( 'SQLColKeyAttri' ) );
    if reg.ValueExists( 'SQLColFunctionAttri' ) then
      SynSQLSyn1.FunctionAttri.Foreground := StringToColor( reg.ReadString( 'SQLColFunctionAttri' ) );
    if reg.ValueExists( 'SQLColDataTypeAttri' ) then
      SynSQLSyn1.DataTypeAttri.Foreground := StringToColor( reg.ReadString( 'SQLColDataTypeAttri' ) );
    if reg.ValueExists( 'SQLColNumberAttri' ) then
      SynSQLSyn1.NumberAttri.Foreground := StringToColor( reg.ReadString( 'SQLColNumberAttri' ) );
    if reg.ValueExists( 'SQLColStringAttri' ) then
      SynSQLSyn1.StringAttri.Foreground := StringToColor( reg.ReadString( 'SQLColStringAttri' ) );
    if reg.ValueExists( 'SQLColCommentAttri' ) then
      SynSQLSyn1.CommentAttri.Foreground := StringToColor( reg.ReadString( 'SQLColCommentAttri' ) );
    if reg.ValueExists( 'SQLColTablenameAttri' ) then
      SynSQLSyn1.TablenameAttri.Foreground := StringToColor( reg.ReadString( 'SQLColTablenameAttri' ) );
    if reg.ValueExists( 'SQLColActiveLine' ) then
      SynMemoQuery.ActiveLineColor := StringToColor( reg.ReadString('SQLColActiveLine') );

    // SQLFiles-History
    FillPopupQueryLoad();

    // SQL-Filter-Files-History
    i := 1;
    popupFilterOpenFile.Items.Clear();
    while ( reg.ValueExists( 'SQLWhereFile' + IntToStr(i) ) ) do
    begin
      menuitem := Tmenuitem.Create(Self);
      menuitem.Caption := IntToStr( popupFilterOpenFile.Items.count + 1) + ' ' + reg.ReadString( 'SQLWhereFile' + IntToStr(i) );
      menuitem.OnClick := LoadSQLWhereFile;
      popupFilterOpenFile.Items.Add( menuitem );
      inc( i );
    end;

    // Synchronize internal variables with defaults from DFM.
    StopOnErrors := btnQueryStopOnErrors.Down;

    // Says if the filters on the data tab shall be remembered
    if reg.ValueExists( 'RememberFilters' ) then
      prefRememberFilters := reg.ReadBool('RememberFilters')
    else
      prefRememberFilters := True;

    // Restore width of columns of all VirtualTrees
    RestoreListSetup(ListVariables);
    RestoreListSetup(ListProcesses);
    RestoreListSetup(ListCommandStats);
    RestoreListSetup(ListTables);
    RestoreListSetup(ListColumns);

    // Read the delimiters
    delimiters := Trim( reg.ReadString( 'delimiters' ) );
    if ( delimiters <> EmptyStr ) then
    begin
      ComboBoxQueryDelimiter.Items.Text := delimiters;
      ComboBoxQueryDelimiter.ItemIndex := reg.ReadInteger( 'delimiterselected' );
    end;

    // Activate logging
    if reg.ValueExists( 'LogToFile' ) and reg.ReadBool('LogToFile') then
      ActivateFileLogging;

    // Open server-specific registry-folder.
    // relative from already opened folder!
    reg.OpenKey( 'Servers\' + FConn.Description, true );

    // Set last used database, select it later in Init
    lastUsedDB := reg.ReadString( 'lastUsedDB' );
  end;
  reg.CloseKey;
  reg.Free;

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
  ws    : String;
  reg   : TRegistry;
  ds    : TDataSet;
begin
  SetWindowConnected( false );
  SetWindowName( main.discname );
  Application.Title := APPNAME;

  ds := DataSource1.DataSet;
  DataSource1.DataSet := nil;
  if ds <> nil then ds.Close;
  FreeAndNil(ds);
  ds := DataSource2.DataSet;
  DataSource2.DataSet := nil;
  if ds <> nil then ds.Close;
  FreeAndNil(ds);
  ClearAllTableLists;
  FreeAndNil(CachedTableLists);
  FreeAndNil(methodStack);

  // Closing connection
  FMysqlConn.Disconnect;
  FreeAndNil(FMysqlConn);

  EnterCriticalSection(SqlMessagesLock);
  FreeAndNil(SqlMessages);
  LeaveCriticalSection(SqlMessagesLock);

  // Saving some vars into registry
  case ( WindowState ) of
    wsNormal :
    begin
      ws := 'Normal';
    end;
    wsMinimized :
    begin
      ws := 'Minimized';
    end;
    wsMaximized :
    begin
      ws := 'Maximized';
    end;
  end;

  reg := TRegistry.Create();
  with ( reg ) do
  begin
    if ( OpenKey( REGPATH, true ) ) then
    begin
      // Window state and position
      WriteString( 'childwinstate', ws );
      WriteInteger( 'childwinleft', left );
      WriteInteger( 'childwintop', top );
      WriteInteger( 'childwinwidth', width );
      WriteInteger( 'childwinheight', height );

      WriteInteger( 'querymemoheight', pnlQueryMemo.Height );
      WriteInteger( 'queryhelperswidth', pnlQueryHelpers.Width );
      WriteInteger( 'dbtreewidth', dbtree.width );
      WriteInteger( 'sqloutheight', PageControlBottom.Height );

      // Save width of probably resized columns of all VirtualTrees
      SaveListSetup(ListVariables);
      SaveListSetup(ListProcesses);
      SaveListSetup(ListCommandStats);
      SaveListSetup(ListTables);
      SaveListSetup(ListColumns);

      // Save the delimiters
      WriteString( 'delimiters', ComboBoxQueryDelimiter.Items.Text );
      WriteInteger( 'delimiterselected', ComboBoxQueryDelimiter.ItemIndex );

      // Open server-specific registry-folder.
      // relative from already opened folder!
      OpenKey( 'Servers\' + FConn.Description, true );
      WriteString( 'lastUsedDB', ActiveDatabase );
    end;
  end;
  FreeAndNil(reg);

  FormDeactivate( Sender );
  mainform.ToolBarData.Visible := false;
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
procedure TMDIChild.LogSQL(msg: String = ''; comment: Boolean = true);
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
  msg := StringReplace( msg, #9, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #10, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, #13, ' ', [rfReplaceAll] );
  msg := StringReplace( msg, '  ', ' ', [rfReplaceAll] );
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
  msg: string;
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

  SynMemoSQLLog.Lines.Add( String(msg) );

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


function TMDIChild.FindTreeViewAbsoluteIndex(n: TTreeNode): Integer;
var
  tv: TTreeView;
  i: Integer;
begin
  Result := -1;
  tv := TTreeView(n.TreeView);
  i := 0;
  while i < tv.Items.Count do begin
    if tv.Items[i] = n then begin
      Result := i;
      Exit;
    end;
    i := i + 1;
  end;
end;

procedure TMDIChild.ReadDatabasesAndTables(Sender: TObject);
var
  tnode, found   : TTreeNode;
  i              : Integer;
  specialDbs     : TStringList;
  dbName         : String;
  ds             : TDataSet;
  select         : Integer;
  newTree        : TTreeView;
begin
  // Force data tab update when appropriate.
  dataselected := false;

  Screen.Cursor := crSQLWait;
  mainform.Showstatus( 'Reading Databases...', 2, true );
  if ( OnlyDBs.Count = 0 ) then
  begin
    OnlyDBs2 := TStringList.Create();
    specialDbs := TStringList.Create();
    ds := GetResults( 'SHOW DATABASES' );
    for i:=1 to ( ds.RecordCount ) do
    begin
      dbName := ds.FieldByName('Database').AsString;
      if ( dbName = DBNAME_INFORMATION_SCHEMA ) then specialDbs.Insert( 0, dbName )
      //else if ( dbName = DBNAME_MYSQL ) then specialDbs.Add( dbName )
      else OnlyDBs2.Add( dbName );
      ds.Next();
    end;
    ds.Close;
    FreeAndNil(ds);
    OnlyDBs2.Sort();
    // Prioritised position of system-databases
    for i := ( specialDbs.Count - 1 ) downto 0 do begin
      OnlyDBs2.Insert( 0, specialDbs[i] );
    end;
  end else OnlyDBs2 := OnlyDBs;

  Screen.Cursor := crHourGlass;
  ClearAllTableLists;

  newTree := TTreeView.Create(nil);
  newTree.Visible := false;
  newTree.Parent := self;

  tnodehost := newTree.Items.Add( nil, FConn.MysqlParams.User + '@' + FConn.MysqlParams.Host );  // Host or Root
  tnodehost.ImageIndex := 41;
  tnodehost.SelectedIndex := 41;
  select := 0;

  // Avoids excessive InitializeKeywordLists() calls.
  SynSQLSyn1.TableNames.BeginUpdate();
  SynSQLSyn1.TableNames.Clear();
  // Let synedit know all database names so that they can be highlighted
  // TODO: Is this right?  Adding "<db name>.<table name>" seems to make more sense..
  SynSQLSyn1.TableNames.AddStrings( OnlyDBs2 );
  SynSQLSyn1.TableNames.EndUpdate();

  // List Databases and Tables-Names
  for i := 0 to ( OnlyDBs2.Count - 1 ) do
  begin
    tnode := newTree.Items.AddChild( tnodehost, OnlyDBs2[i] );
    tnode.ImageIndex := 37;
    tnode.SelectedIndex := 38;
    // Add dummy-node, will be replaced by real tables on expanding
    newTree.Items.AddChild( tnode, DUMMY_NODE_TEXT );
    if i = 0 then tnodehost.Expand(false);
    // Reselect previously selected database and table.
    if ( ActiveDatabase = OnlyDBs2[i] ) then
    begin
      select := newTree.Items.Count - 2;
      found := PopulateTreeTableList(tnode, SelectedTable);
      if found <> nil then select := FindTreeViewAbsoluteIndex(found);
      tnode.Expand(false);
    end;
  end;

  mainform.showstatus( IntToStr( OnlyDBs2.Count ) + ' Databases' );

  DisableTreeEvents;
  DBTree.Items.Clear;
  DBTree.Items.Assign(newTree.Items);
  // Expansion state is discarded when assigning new items, for some reason.
  for i := 0 to newTree.Items.Count - 1 do begin
    if newTree.Items[i].Expanded then DBTree.Items[i].Expand(false);
  end;
  DBTree.ClearSelection;
  EnableTreeEvents;
  DBTree.Selected := DBTree.Items[select];
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  Screen.Cursor := crDefault;
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

  Caption := Description;
  pcChange( Self );
end;


procedure TMDIChild.ShowDatabase(db: String);
begin
  if (not DBTree.Dragging) and (
   (PageControlMain.ActivePage = tabHost) or
   (PageControlMain.ActivePage = tabTable) or
   (PageControlMain.ActivePage = tabData)
  ) then PageControlMain.ActivePage := tabDatabase;

  tabDatabase.TabVisible := true;
  tabTable.TabVisible := false;
  tabData.TabVisible := false;

  pnlTableTop.Caption := 'Table-Properties';
  Caption := Description + ' - /' + db;
  try
    ShowDBProperties( db );
  except
    // Clear selection which we couldn't satisfy.
    if ( DBtree.Items.Count < 1 ) then
    begin
      DBtree.Selected := nil
    end
    else
    begin
      DBtree.Selected := DBtree.Items[0];
    end;
  end;
end;


{***
  Do the default action (show table properties or table data) for a table.
}
procedure TMDIChild.ShowTable(table: String; tab: TTabSheet = nil);
begin
  if tab = nil then tab := tabTable; // Alternative default: tabData
  if tab = tabTable then ShowTableProperties( table );
  if tab = tabData then ShowTableData( table );
  Caption := Description + ' - /' + ActiveDatabase + '/' + SelectedTable;
end;


// React on dbtree-clicks
procedure TMDIChild.DBtreeChange(Sender: TObject; Node: TTreeNode);
begin
  if (Node = nil) then begin
    // Can nil also occur after a DBTree.ClearSelection() ?
    raise Exception.Create( 'Internal badness: No host node in object tree.' );
  end;

  case ( Node.Level ) of
    0 :                                    // Root / Host chosen
    begin
      ShowHost();
    end;
    1 :                                    // DB chosen
    begin
      ShowDatabase( Node.Text );
    end;
    2 :                                    // Table chosen
    begin
      ShowTable( Node.Text );
    end;
  end;
end;


procedure TMDIChild.DBtreeChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
var
  newDb: string;
begin
  if Node <> nil then begin
    case Node.Level of
      2: if Node.Parent.Text <> ActiveDatabase then newDb := Node.Parent.Text
        else Exit;
      1: newDb := Node.Text;
      else Exit;
    end;
    LoadDatabaseProperties(newDb);
  end;
end;


procedure TMDIChild.DBtreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  debug('context menu; storing right click item so it won''t get lost');
  DBRightClickSelectedItem := DBtree.Selected;
  popupTreeView.Popup(DBtree.ClientOrigin.X + MousePos.X, DBtree.ClientOrigin.Y + MousePos.Y);
  PostMessage(MainForm.Handle, WM_CLEAR_RIGHTCLICK_POINTER, 0, 0);
end;

{***
  A database-node is about to be expanded:
  Drop the dummy-node and add all tables
}
procedure TMDIChild.DBtreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if ( ( Node.getFirstChild <> nil ) and ( Node.getFirstChild.Text = DUMMY_NODE_TEXT ) ) then
  begin
    PopulateTreeTableList(Node);
  end;
end;


procedure TMDIChild.viewdata(Sender: TObject);
var
  sorting              : String;
  DropDown             : TTntStringList;
  i                    : Integer;
  j                    : Integer;
  OrderColumns         : TOrderColArray;
  PrimaryKeyColumns    : TStringList;
  reg, reg2            : TRegistry;
  reg_value            : String;
  select_base          : String;
  limit                : Int64;
  ds                   : TDataSet;
  sl_query             : TStringList;
  manualLimit          : boolean;
  manualLimitEnd       : integer;
  DisplayedColumnsList : TStringList;
  tmp                  : TDataSet;
begin
  viewingdata := true;
  reg := TRegistry.Create();
  sl_query := TStringList.Create();
  try
    // limit number of rows automatically if first time this table is shown
    if ( not dataselected ) then begin
      manualLimit := false;
      manualLimitEnd := -1;
      with TRegistry.Create do begin
        if OpenKey(REGPATH, true) then begin
          if Valueexists('DataLimitEnd') then begin
            manualLimitEnd := ReadInteger('DataLimitEnd');
            if Valueexists('DataLimit') then manualLimit := ReadBool('DataLimit');
          end;
        end;
        Free;
      end;

      // limit number of rows fetched according to preferences
      if manualLimit then begin
        // manual limit set in preferences
        limit := manualLimitEnd;
      end else begin
        // no tick in preferences check box - auto-limit:
        // limit number of rows fetched if more than ~ 5 MB of data
        limit := GetCalculatedLimit( SelectedTable );
      end;

      // adjust limit in GUI
      mainform.ToolBarData.Visible := true;
      if ( limit = -1 ) then
      begin
        mainform.CheckBoxLimit.Checked := false;
      end
      else
      begin
        mainform.CheckBoxLimit.Checked := true;
        mainform.EditLimitStart.Text := '0';
        mainform.EditLimitEnd.Text := IntToStr( limit );
      end;
      mainform.Repaint();
    end;

    // set db-aware-component's properties...
    DBMemo1.DataField := '';
    DBMemo1.DataSource := DataSource1;
    EDBImage1.DataField := '';
    EDBImage1.DataSource := DataSource1;

    reg.OpenKey( REGPATH + '\Servers\' + FConn.Description, true );

    if not dataselected and prefRememberFilters then
    begin
      SynMemoFilter.Text := '';
      // Read cached WHERE-clause and set filter
      reg_value := 'WHERECLAUSE_' + ActiveDatabase + '.' + SelectedTable;
      if ( reg.ValueExists( reg_value ) ) then
      begin
        SynMemoFilter.Text := reg.ReadString( reg_value );
        // Ensure the user can see its previous specified filter
        // in case of an SQL-error, it's important that he can delete it
        tabFilter.tabVisible := true;
        PageControlBottom.ActivePage := tabFilter;
      end;
    end;

    // Read cached ORDER-clause and set Grid.Sortcolumns
    sorting := '';
    OrderColumns := HandleOrderColumns;
    if Length(OrderColumns) > 0 then
    begin
      sorting := 'ORDER BY ' + ComposeOrderClause(OrderColumns);
      // Signal for the user that we applied an ORDER-clause
      btnDataSorting.Font.Color := clRed;
    end
    else
      btnDataSorting.Font.Color := clWindowText;

    MenuLimit.Checked := Mainform.CheckBoxLimit.Checked;
    PrimaryKeyColumns := TStringList.Create();

    if ( ( SelectedTable <> '' ) and ( ActiveDatabase <> '' ) ) then
    begin
      // Ensure <Table> and <Data> are visible
      tabTable.TabVisible := true;
      tabData.TabVisible := true;
      // Switch to <Data>
      PageControlMain.ActivePage := tabData;

      MainForm.ShowStatus( 'Retrieving data...', 2, true );

      // Read columns to display from registry
      reg2 := TRegistry.Create;
      with ( reg2 ) do
      begin
        OpenKey( REGPATH + '\Servers\' + FConn.Description, true );
        DisplayedColumnsList := explode( '`', ReadString(REGNAME_DISPLAYEDCOLUMNS + '_' + ActiveDatabase + '.' + SelectedTable));
      end;
      FreeAndNil(reg2);

      // Prepare SELECT statement
      select_base := 'SELECT ';
      // Try to calc the rowcount regardless of a given LIMIT
      // Only needed if the user specified a WHERE-clause
      if (
        ( mysql_version >= 40000 ) and
        ( Trim( Self.SynMemoFilter.Text ) <> '' )
      ) then
      begin
        select_base := select_base + ' SQL_CALC_FOUND_ROWS';
      end;
      // Selected columns
      if DisplayedColumnsList.Count = 0 then
      begin
        select_base := select_base + ' *';
        btnColumnSelection.Font.Color := clBtnText;
      end
      else
      begin
        for i := 0 to DisplayedColumnsList.Count - 1 do
        begin
          select_base := select_base + ' ' + mask(DisplayedColumnsList[i]) + ',';
        end;
        // Cut last comma
        select_base := copy( select_base, 1, Length(select_base)-1 );
        // Signal for the user that we now hide some fields
        btnColumnSelection.Font.Color := clRed;
      end;
      select_base := select_base + ' FROM ' + mask( SelectedTable );
      sl_query.Add( select_base );
      // Apply custom WHERE filter
      if ( Trim( Self.SynMemoFilter.Text ) <> '' ) then
      begin
        sl_query.Add( 'WHERE ' + Trim( Self.SynMemoFilter.Text ) );
      end;
      // Apply custom ORDER BY if detected in registry
      if ( sorting <> '' ) then
      begin
        sl_query.Add( sorting );
      end;
      // Apply LIMIT
      if ( mainform.CheckBoxLimit.Checked ) then
      begin
        sl_query.Add('LIMIT ' + mainform.EditLimitStart.Text + ', ' + mainform.EditLimitEnd.Text );
      end;

      try
        // Avoid excessive GUI updates.
        if ( DataSource1.DataSet <> nil ) then
        begin
          DataSource1.DataSet.DisableControls();
        end;

        // free previous resultset
        tmp := DataSource1.DataSet;
        DataSource1.DataSet := nil;
        if tmp <> nil then tmp.Close;
        FreeAndNil(tmp);

        // start query (with wait dialog)
        SynMemoFilter.Color := clWindow;
        ds := GetResults(sl_query.Text, false);

        MainForm.ShowStatus( 'Filling grid with record-data...', 2, true );
        ds.DisableControls();
        DataSource1.DataSet := ds;

        // Attach After- and Before-Events to the new dataset
        ds.AfterPost := ZQueryGridAfterPost;
        ds.AfterDelete := ZQueryGridAfterPost;
        ds.BeforeClose := ZQueryGridBeforeClose;
      except
        on E:Exception do
        begin
          // Most likely we have a wrong filter-clause when this happens
          LogSQL( E.Message, True );
          // Put the user with his nose onto the wrong filter
          // either specified by user or
          // created by HeidiSQL by using the search box
          if ( SynMemoFilter.CanFocus ) then
          begin
            SynMemoFilter.SetFocus();
          end;
          SynMemoFilter.Color := $008080FF; // light pink
          MessageDlg( E.Message, mtError, [mbOK], 0 );
          MainForm.ShowStatus( STATUS_MSG_READY, 2 );
          Screen.Cursor := crDefault;
          Exit;
        end;
      end;

      MainForm.ShowStatus( STATUS_MSG_READY, 2 );

      if DataSource1.DataSet <> nil then begin
        for i := 0 to Length(VTRowDataListColumns) - 1 do
        begin
          // give all enum-fields a PickList with its Items
          if StrCmpBegin( 'enum', VTRowDataListColumns[i].Captions[1]) then
          begin
            DropDown := TTntStringList.Create;
            DropDown.QuoteChar := '''';
            DropDown.DelimitedText := getEnumValues( VTRowDataListColumns[i].Captions[1] );

            for j := 0 to gridData.Columns.Count - 1 do
            begin
              if gridData.Columns[j].FieldName = VTRowDataListColumns[i].Captions[0] then
                gridData.Columns[j].WidePickList := DropDown;
            end;
          end;

          // make PK-columns = fsBold
          for j := 0 to gridData.Columns.Count - 1 do
          begin
            if ( gridData.Columns[j].FieldName = VTRowDataListColumns[i].Captions[0] ) and
              ( VTRowDataListColumns[i].ImageIndex = 26 ) then
              PrimaryKeyColumns.Add( VTRowDataListColumns[i].Captions[0] );
          end;
        end;

        for j := 0 to gridData.Columns.Count - 1 do
        begin
          // for letting NULLs being inserted into "NOT NULL" fields
          // in mysql5+, the server rejects inserts with NULLs in NOT NULL-fields,
          // so the Required-check on client-side is not needed at any time
          ds.Fields[j].Required := false;

          // set column-width
          if (
            (prefDefaultColWidth <> 0) and
            (gridData.Columns[j].Width > prefDefaultColWidth)
          ) then
          begin
            gridData.Columns[j].Width := prefDefaultColWidth;
          end;

          // Colorize ordered columns
          for i := Low(OrderColumns) to High(OrderColumns) do
          begin
            if OrderColumns[i].ColumnName = gridData.Columns[j].Fieldname then
            begin
              if OrderColumns[i].SortDirection = ORDER_ASC then
                gridData.Columns[j].Color := COLOR_SORTCOLUMN_ASC
              else
                gridData.Columns[j].Color := COLOR_SORTCOLUMN_DESC
            end;
          end;

          // make PK-columns = fsBold
          for i := 0 to PrimaryKeyColumns.Count - 1 do
          begin
            if PrimaryKeyColumns[i] = gridData.Columns[j].Fieldname then
              gridData.Columns[j].Font.Style := gridData.Columns[j].Font.Style + [fsBold];
          end;
        end;

        DisplayRowCountStats(ds);
        dataselected := true;
        viewingdata := false;
      end;
      ds.EnableControls();
      pcChange(self);
    end;

    Screen.Cursor := crDefault;
  finally
    FreeAndNil(reg);
    FreeAndNil(sl_query);
    viewingdata := false;
  end;
end;


{***
  Calculate + display total rowcount and found rows matching to filter
  in data-tab
}
procedure TMDIChild.DisplayRowCountStats(ds: TDataSet);
var
  rows_matching    : Int64; // rows matching to where-filter
  rows_total       : Int64; // total rowcount
begin
  if ( ActiveGrid = gridQuery ) then
  begin
    Exit;
  end;

  // Get rowcount
  rows_total := StrToInt64( GetVar( 'SELECT COUNT(*) FROM ' +
    mask( SelectedTable ), 0 ) );

  lblDataTop.Caption := ActiveDatabase + '.' + SelectedTable + ': ' +
    FormatNumber( rows_total ) + ' records total';

  {***
    @note: FOUND_ROWS() gives us a correct number, but this number
      belongs in most cases to a different query than the previous SELECT,
      because Zeos does some automagically
        SHOW TABLES LIKE '' and
        SHOW COLUMNS LIKE '%'
      between the above SELECT and a following "SELECT FOUND_ROWS()".
      This problem has been introduced with the fix of
      a faulty caching-mechanism in Zeos (rev 312).
      and those queries seem to reset the FOUND_ROWS() since MySQL 5.0
      The workaround is to store FOUND_ROWS() immediately after query-execution
      in a variable, which we are selecting here.
    @see TZMySQLResultSet:Create
    @see TZMySQLResultSet:Open
  }
  if Trim( SynMemoFilter.Text ) <> '' then
  begin
    if mysql_version >= 40000 then
    begin
      rows_matching := StrToInt64Def(GetVar('SELECT @found_rows'), 0);
    end
    else
    begin
      rows_matching := ActiveGrid.DataSource.DataSet.RecordCount;
    end;
  end
  else
  begin
    rows_matching := rows_total;
  end;

  if (
    ( rows_matching <> rows_total ) and
    ( Trim( SynMemoFilter.Text ) <> '' )
  ) then
  begin
    lblDataTop.Caption := lblDataTop.Caption + ', ' + FormatNumber(rows_matching) +
      ' matching to filter';
  end;

  if (
    ( rows_matching = rows_total ) and
    ( Trim( SynMemoFilter.Text ) <> '')
  ) then
  begin
    lblDataTop.Caption := lblDataTop.Caption + ', filter matches all records';
  end;

  if (
    ( mainform.CheckBoxLimit.Checked ) and
    ( rows_matching > StrToIntDef( mainform.EditLimitEnd.Text, 0 ) )
  ) then
  begin
    lblDataTop.Caption := lblDataTop.Caption + ', limited to ' +
      FormatNumber( ds.RecordCount );
  end;
end;


procedure TMDIChild.WaitForQueryCompletion(WaitForm: TfrmQueryProgress; query: TMySqlQuery);
var
  signal: Cardinal;
begin
  debug( 'Waiting for query to complete.' );
  signal := WaitForSingleObject(query.EventHandle, 300);
  if signal = 0 then debug( 'Query completed within 300msec.' )
  else begin
    debug( '300msec passed, showing progress form.' ); 
    WaitForm.ShowModal();
  end;
  CloseHandle(query.EventHandle);
  debug( 'Query complete.' );
end;


{***
  Occurs when active tab has changed.
}
procedure TMDIChild.pcChange(Sender: TObject);
var
  dummy : Boolean;
begin
  tabFilter.tabVisible := (PageControlMain.ActivePage = tabData);

  Mainform.ExecuteQuery.Enabled := PageControlMain.ActivePage = tabQuery;
  Mainform.ExecuteSelection.Enabled := PageControlMain.ActivePage = tabQuery;
  Mainform.ExecuteLine.Enabled := PageControlMain.ActivePage = tabQuery;
  if (PageControlMain.ActivePage = tabData) and (not dataselected) then
    viewdata(self);
  if PageControlMain.ActivePage = tabQuery then
  begin
    if ActiveDatabase <> '' then
      pnlQueryTop.Caption := 'SQL-Query on Database ' + ActiveDatabase + ':'
    else
      pnlQueryTop.Caption := 'SQL-Query on Host ' + FConn.MysqlParams.Host + ':';
    // Manually invoke OnChange event of tabset to fill helper list with data
    tabsetQueryHelpers.OnChange( Sender, tabsetQueryHelpers.TabIndex, dummy);
  end;

  // Move focus to relevant controls in order for them to receive keyboard events.
  if PageControlMain.ActivePage = tabDatabase then ListTables.SetFocus;
  if PageControlMain.ActivePage = tabTable then ListColumns.SetFocus;
  if PageControlMain.ActivePage = tabData then gridData.SetFocus;
  if PageControlMain.ActivePage = tabQuery then SynMemoQuery.SetFocus;

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
  db: String;
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

function TMDIChild.FetchDbTableList(db: string): TDataSet;
var
  ds: TDataSet;
  OldCursor: TCursor;
begin
  if CachedTableLists.IndexOf(db) = -1 then begin
    // Not in cache, load table list.
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    MainForm.ShowStatus('Displaying tables from ' + db + '...', 2, true);
    if mysql_version >= 32300 then begin
      ds := GetResults('SHOW TABLE STATUS FROM ' + mask(db), false, false);
    end else begin
      // contains table names, nothing else.
      ds := GetResults('SHOW TABLES FROM ' + mask(db), false, false);
      // could clean up data (rename first column to 'Name') and
      // and add row counters to data set as a new field by using
      // SELECT COUNT(*), but that would potentially be rather slow.
    end;
    CachedTableLists.AddObject(db, ds);
    Screen.Cursor := OldCursor;
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

function TMDIChild.RefreshDbTableList(db: string): TDataSet;
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
  Result := FetchDbTableList(db);
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


{***
  Updates tree with table list for currently selected database (or table).
}
function TMDIChild.PopulateTreeTableList(tndb: TTreeNode; find: string; ForceRefresh: Boolean): TTreeNode;
var
  ds: TDataSet;
  s: string;
  t, u: Integer;
  select, tmp: TTreeNode;
begin
  result := nil;

  // Find currently selected node if not specified, or exit.
  select := nil;
  if tndb = nil then case DBTree.Selected.Level of
    2: tndb := DBTree.Selected.Parent;
    1: tndb := DBTree.Selected;
    else Exit;
    if ActiveDatabase = tndb.Text then find := SelectedTable;
  end else begin
    // Looking for a subitem?
    // Select the parent if it cannot be found after populating tndb.
    if find <> '' then select := tndb;
  end;

  // Postpone change event handling in tree
  DisableTreeEvents;
  SynSQLSyn1.TableNames.BeginUpdate;
  try
    // Clear children and populate tree with table names.
    if tndb.Count > 0 then for u:=tndb.Count-1 downto 0 do begin
      if (select = nil) and (tndb.Item[u] = tndb.TreeView.Selected) then begin
        select := tndb;
        tndb.TreeView.Deselect(tndb.Item[u]);
      end;
      tndb.Item[u].delete;
    end;
    if ForceRefresh then ds := RefreshDbTableList(tndb.Text)
    else ds := FetchDbTableList(tndb.Text);
    for t:=0 to ds.RecordCount-1 do
    begin
      s := ds.Fields[0].AsString;
      tmp := tndb.Owner.AddChild(tndb, s);
      with tmp do
      begin
        ImageIndex := 39;
        selectedIndex := 40;
        if Text = find then select := tmp;
      end;
      // Add tables to syntax highlighter
      if SynSQLSyn1.TableNames.IndexOf(s) = -1 then SynSQLSyn1.TableNames.Add(s);
      ds.Next;
    end;
    if select <> nil then begin
      result := select;
      tndb.TreeView.Selected := select;
    end;
  finally
    // Restore change event handlers
    EnableTreeEvents;
    SynSQLSyn1.TableNames.EndUpdate;
  end;
end;


procedure TMDIChild.MenuRefreshClick(Sender: TObject);
begin
  RefreshActiveDbTableList;
  LoadDatabaseProperties(ActiveDatabase);
  PopulateTreeTableList;
end;


procedure TMDIChild.LoadDatabaseProperties(db: string);
var
  i               : Integer;
  bytes           : Extended;
  ds              : TDataSet;
  ListCaptions    : TStringList;
begin
  // DB-Properties
  Screen.Cursor := crHourGlass;
  MainForm.ShowStatus( 'Reading from database ' + db + '...', 2, true );
  Mainform.ButtonDropDatabase.Hint := 'Drop Database...|Drop Database ' + db + '...';

  try
    ds := FetchDbTableList(db);

    ListTables.BeginUpdate;
    ListTables.Clear;

    SetLength(VTRowDataListTables, ds.RecordCount);
    for i := 1 to ds.RecordCount do
    begin
      listcaptions := TStringList.Create;
      // Table
      ListCaptions.Add( ds.Fields[0].AsString );

      // SHOW TABLE STATUS only available on 3.23.0 + servers
      if mysql_version < 32300 then
        continue;

      // Default visible columns

      // Records
      ListCaptions.Add( FormatNumber( ds.FieldByName('Rows').AsString ) );
      // Size: Data_length + Index_length
      bytes := ds.FieldByName('Data_length').AsFloat + ds.FieldByName('Index_length').AsFloat;
      ListCaptions.Add( FormatByteNumber( FloatToStr(bytes) ) );
      // Created:
      if not ds.FieldByName('Create_time').IsNull then
        ListCaptions.Add( ds.FieldByName('Create_time').AsString )
      else
        ListCaptions.Add('N/A');

      // Updated:
      if not ds.FieldByName('Update_time').IsNull then
        ListCaptions.Add( ds.FieldByName('Update_time').AsString )
      else
        ListCaptions.Add('N/A');

      // Type
      if ds.FindField('Type')<>nil then
        ListCaptions.Add( ds.FieldByName('Type').AsString )
      else if ds.FindField('Engine')<>nil then
        ListCaptions.Add( ds.FieldByName('Engine').AsString )
      else
        ListCaptions.Add('');

      // Comment
      ListCaptions.Add( ds.FieldByName('Comment').AsString );

      // Add content from other columns which are not visible in ListTables by default

      if ds.FindField('Version')<>nil then
        ListCaptions.Add( ds.FieldByName('Version').AsString )
      else
        ListCaptions.Add('');

      if ds.FindField('Row_format')<>nil then
        ListCaptions.Add( ds.FieldByName('Row_format').AsString )
      else
        ListCaptions.Add('');

      if (ds.FindField('Avg_row_length')<>nil) and (ds.FieldByName('Avg_row_length').AsString<>'') then
        ListCaptions.Add( FormatByteNumber(ds.FieldByName('Avg_row_length').AsString) )
      else
        ListCaptions.Add('');

      if (ds.FindField('Max_data_length')<>nil) and (ds.FieldByName('Max_data_length').AsString<>'') then
        ListCaptions.Add( FormatByteNumber(ds.FieldByName('Max_data_length').AsString) )
      else
        ListCaptions.Add('');

      if (ds.FindField('Index_length')<>nil) and (ds.FieldByName('Index_length').AsString<>'') then
        ListCaptions.Add( FormatByteNumber(ds.FieldByName('Index_length').AsString) )
      else
        ListCaptions.Add('');

      if (ds.FindField('Data_free')<>nil) and (ds.FieldByName('Data_free').AsString<>'') then
        ListCaptions.Add( FormatByteNumber(ds.FieldByName('Data_free').AsString) )
      else
        ListCaptions.Add('');

      if (ds.FindField('Auto_increment')<>nil) and (ds.FieldByName('Auto_increment').AsString<>'') then
        ListCaptions.Add( FormatNumber(ds.FieldByName('Auto_increment').AsString) )
      else
        ListCaptions.Add('');

      if ds.FindField('Check_time')<>nil then
        ListCaptions.Add( ds.FieldByName('Check_time').AsString )
      else
        ListCaptions.Add('');

      if ds.FindField('Collation')<>nil then
        ListCaptions.Add( ds.FieldByName('Collation').AsString )
      else
        ListCaptions.Add('');

      if ds.FindField('Checksum')<>nil then
        ListCaptions.Add( ds.FieldByName('Checksum').AsString )
      else
        ListCaptions.Add('');

      if ds.FindField('Create_options')<>nil then
        ListCaptions.Add( ds.FieldByName('Create_options').AsString )
      else
        ListCaptions.Add('');

      VTRowDataListTables[i-1].ImageIndex := 39;
      VTRowDataListTables[i-1].Captions := ListCaptions;
      ds.Next;
    end;
  finally
    ListTables.RootNodeCount := Length(VTRowDataListTables);
    ListTables.EndUpdate;
    Mainform.showstatus(db + ': ' + IntToStr(ListTables.RootNodeCount) +' table(s)');
    Screen.Cursor := crDefault;
  end;
end;


{ Show tables and their properties on the tabsheet "Database" }
procedure TMDIChild.ShowDBProperties(db: string);
begin
  Screen.Cursor := crHourglass;
  pcChange( Self );
  pnlDatabaseTop.Caption := 'Database ' + db + ': ' + IntToStr(ListTables.RootNodeCount) + ' table(s)';
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  Screen.Cursor := crDefault;
end;


{ Show columns of selected table, indicate indexed columns by certain icons }
procedure TMDIChild.RefreshFieldListClick(Sender: TObject);
begin
  ShowTableProperties(SelectedTable);
end;


procedure TMDIChild.ShowTableProperties(table: string);
var
  i,j : Integer;
  isFulltext : Boolean;
  ds : TDataSet;
  dummy: Boolean;
  hasCommentColumn: Boolean;
begin
  // Table-Properties
  dataselected := false;
  Screen.Cursor := crHourGlass;

  if (not DBTree.Dragging) and (
   (PageControlMain.ActivePage = tabHost) or
   (PageControlMain.ActivePage = tabDatabase)
  ) then PageControlMain.ActivePage := tabTable;

  tabDatabase.TabVisible := true;
  tabTable.TabVisible := true;
  tabData.TabVisible := true;

  pnlTableTop.Caption := 'Table-Properties for ' + ActiveDatabase + ': ' + table;

  MainForm.ShowStatus( 'Reading table properties...', 2, true );
  ListColumns.BeginUpdate;
  ListColumns.Clear;
  Try
    ds := GetResults( 'SHOW /*!32332 FULL */ COLUMNS FROM ' + mask(table), false );

    // Hide column "Comment" on old servers.
    hasCommentColumn := ds.FindField('Comment') <> nil;
    if not hasCommentColumn then
      ListColumns.Header.Columns[5].Options := ListColumns.Header.Columns[5].Options - [coVisible];

    SetLength(VTRowDataListColumns, ds.RecordCount);
    for i:=1 to ds.RecordCount do
    begin
      VTRowDataListColumns[i-1].ImageIndex := ICONINDEX_FIELD;
      VTRowDataListColumns[i-1].Captions := TStringList.Create;
      VTRowDataListColumns[i-1].Captions.Add( ds.FieldByName('Field').AsString );
      VTRowDataListColumns[i-1].Captions.Add( ds.FieldByName('Type').AsString );
      if lowercase( ds.FieldByName('Null').AsString ) = 'yes' then
        VTRowDataListColumns[i-1].Captions.Add('Yes')
        else VTRowDataListColumns[i-1].Captions.Add('No');
      VTRowDataListColumns[i-1].Captions.Add( ds.FieldByName('Default').AsString );
      VTRowDataListColumns[i-1].Captions.Add( ds.FieldByName('Extra').AsString );
      if hasCommentColumn then
        VTRowDataListColumns[i-1].Captions.Add( ds.FieldByName('Comment').AsString )
      else
        VTRowDataListColumns[i-1].Captions.Add('');

      ds.Next;
    end;

    ListColumns.RootNodeCount := Length(VTRowDataListColumns);

    // Manually invoke OnChange event of tabset to fill helper list with data
    if tabsetQueryHelpers.TabIndex = 0 then
      tabsetQueryHelpers.OnChange( Self, tabsetQueryHelpers.TabIndex, dummy);

    {*
      TODO: Create drag-drop box next to query window with these columns.
    // add fields to dbtree for drag'n dropping purpose
    if not DBTree.Selected.HasChildren then
    begin
      ds.First;
      for i:=1 to ds.RecordCount do begin
        tn := DBtree.Items.AddChild(Dbtree.Selected, ds.FieldByName('Field').AsString );
        if ds.FieldByName('Key').AsString = 'PRI' then
          tn.ImageIndex := 26
        else
          tn.ImageIndex := 62;
        tn.SelectedIndex := tn.ImageIndex;
        ds.Next;
      end;
    end;
    *}
    ds.Close;
    FreeAndNil(ds);

    Screen.Cursor := crHourglass;
    ds := GetResults( 'SHOW KEYS FROM ' + mask(table) );
    for i:=1 to ds.RecordCount do
    begin
      // Search for the column name in listColumns
      for j:=0 to Length(VTRowDataListColumns)-1 do
      begin
        if ds.FieldByName('Column_name').AsString = VTRowDataListColumns[j].Captions[0] then
        begin
          // Only apply a new icon if it was not already changed
          if VTRowDataListColumns[j].ImageIndex <> ICONINDEX_FIELD then
            break;

          // Check if column is part of a fulltext key
          if mysql_version < 40002 then
            isFulltext := (ds.FieldByName('Comment').AsString = 'FULLTEXT')
          else
            isFulltext := (ds.FieldByName('Index_type').AsString = 'FULLTEXT');

          // Primary key
          if ds.FieldByName('Key_name').AsString = 'PRIMARY' then
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_PRIMARYKEY
          // Fulltext index
          else if isFullText then
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_FULLTEXTKEY
          // Unique index
          else if ds.FieldByName('Non_unique').AsString = '0' then
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_UNIQUEKEY
          // Normal index
          else
            VTRowDataListColumns[j].ImageIndex := ICONINDEX_INDEXKEY;

          // Column was found and processed
          break;
        end;
      end;
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
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
    Screen.Cursor := crDefault;
  end;

  pcChange( Self );
  MainForm.ShowStatus( STATUS_MSG_READY, 2, false );
  MainForm.showstatus(ActiveDatabase + ': '+ table + ': ' + IntToStr(ListColumns.RootNodeCount) +' field(s)');
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
  tableSelected : Boolean;
  inDataOrQueryTab, inDataOrQueryTabNotEmpty : Boolean;
begin
  // Make sure that main menu "drop table" affects table selected in tree view,
  // not table (now invisibly) selected on the database grid.
  if (PageControlMain.ActivePage <> tabDatabase) then ListTables.FocusedNode := nil;

  tableSelected := (Length(ListTables.GetSortedSelection(False))>0) and FrmIsFocussed;
  btnDbProperties.Enabled := tableSelected;
  menuproperties.Enabled := tableSelected;
  btnDbViewData.Enabled := tableSelected;
  menuviewdata.Enabled := tableSelected;
  btnDbEmptyTable.Enabled := tableSelected;
  menuemptytable.Enabled := tableSelected;
  menuAlterTable.Enabled := tableSelected;
  MenuRenameTable.Enabled := tableSelected;
  Mainform.CopyTable.Enabled := tableSelected;
  MenuOptimize.Enabled := tableSelected;
  MenuCheck.Enabled := tableSelected;
  MenuAnalyze.Enabled := tableSelected;
  MenuRepair.Enabled := tableSelected;

  MainForm.ButtonDropDatabase.Enabled := (ActiveDatabase <> '') and FrmIsFocussed;
  MainForm.DropTable.Enabled := tableSelected or ((PageControlMain.ActivePage <> tabDatabase) and (SelectedTable <> '') and FrmIsFocussed);
  MainForm.ButtonCreateTable.Enabled := (ActiveDatabase <> '') and FrmIsFocussed;
  MainForm.ButtonImportTextFile.Enabled := (mysql_version >= 32206) and FrmIsFocussed;
  MainForm.MenuImportTextFile.Enabled := MainForm.ButtonImportTextFile.Enabled;

  with MainForm do
  begin
    ButtonRefresh.Enabled := FrmIsFocussed;
    ButtonReload.Enabled := FrmIsFocussed;
    ExportTables.Enabled := FrmIsFocussed;
    ButtonCreateDatabase.Enabled := FrmIsFocussed;
    MenuRefresh.Enabled := FrmIsFocussed;
    MenuExport.Enabled := FrmIsFocussed;
    MenuCreateTable.Enabled := FrmIsFocussed;
    MenuCreateDatabase.Enabled := FrmIsFocussed;
    MenuDropDatabase.Enabled := FrmIsFocussed;
    LoadSQL.Enabled := FrmIsFocussed;
    MenuFlushHosts.Enabled := FrmIsFocussed;
    MenuFlushLogs.Enabled := FrmIsFocussed;
    FlushUserPrivileges1.Enabled := FrmIsFocussed;
    MenuFlushTables.Enabled := FrmIsFocussed;
    MenuFlushTableswithreadlock.Enabled := FrmIsFocussed;
    MenuFlushStatus.Enabled := FrmIsFocussed;
    UserManager.Enabled := FrmIsFocussed;
    Diagnostics.Enabled := FrmIsFocussed;
    InsertFiles.Enabled := FrmIsFocussed;
    {***
      Activate export-options if we're on Data- or Query-tab
      PrintList should only be active if we're focussing one of the ListViews,
      at least as long we are not able to print DBGrids
      @see Issue 1686582
    }
    inDataOrQueryTab := FrmIsFocussed and ((PageControlMain.ActivePage = tabData) or (PageControlMain.ActivePage = tabQuery));
    PrintList.Enabled := (not inDataOrQueryTab) and FrmIsFocussed;
    // Both the Query and the Data grid may have a nil DataSet reference,
    // either in case the relevant grid has not been used yet, or when
    // an error has occurred.
    inDataOrQueryTabNotEmpty := inDataOrQueryTab and
      not (
        (getActiveGrid.DataSource.DataSet = nil)
        or getActiveGrid.DataSource.DataSet.IsEmpty
      );
    Copy2CSV.Enabled := inDataOrQueryTabNotEmpty;
    CopyHTMLtable.Enabled := inDataOrQueryTabNotEmpty;
    Copy2XML.Enabled := inDataOrQueryTabNotEmpty;
    ExportData.Enabled := inDataOrQueryTabNotEmpty;
    HTMLView.Enabled := inDataOrQueryTabNotEmpty;
    Self.Delete1.Enabled := inDataOrQueryTabNotEmpty; // Menuitem in popupDataGrid ("Delete record(s)")
    ToolBarData.visible := (PageControlMain.ActivePage = tabData);
    if FrmIsFocussed then
      DBNavigator1.DataSource := DataSource1;
    btnSQLHelp.Enabled := (mysql_version >= 40100) and FrmIsFocussed;
    menuSQLHelp.Enabled := btnSQLHelp.Enabled and FrmIsFocussed;

    if not FrmIsFocussed then
    begin
      MainForm.showstatus('', 1); // empty connected_time
    end;
    tabBlobEditor.tabVisible := inDataOrQueryTab;
  end;
end;


procedure TMDIChild.ShowTableData(table: string);
begin
  dataselected := false;
  PageControlMain.ActivePage := tabData;
  viewdata(self);
  pcChange( Self );
end;


procedure TMDIChild.MenuViewDataClick(Sender: TObject);
var
  NodeData: PVTreeData;
begin
  if Assigned(ListTables.FocusedNode) then begin
    NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
    PopulateTreeTableList;
    SelectedTable := NodeData.Captions[0];
    ShowTable(SelectedTable);
    ShowTableData(SelectedTable);
  end;
end;


procedure TMDIChild.EmptyTable(Sender: TObject);
var
  t : TStringList;
  i : Integer;
  sql_pattern : String;
begin
  // Empty Table(s)
  if ListTables.SelectedCount = 0 then
    exit;

  // Add selected items/tables to helper list
  t := GetVTCaptions(ListTables, True);

  if MessageDlg('Empty ' + IntToStr(t.count) + ' Table(s) ?' + crlf + '(' + implodestr(', ', t) + ')', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    exit;

  Screen.Cursor := crSQLWait;

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
  MenuRefreshClick(self);
  Screen.Cursor := crDefault;
end;


procedure TMDIChild.DropDB(Sender: TObject);
var
  tndb_ : TTreeNode;
begin
  // Drop DB.
  tndb_ := nil;
  if (Sender as TComponent).Name = 'PopupmenuDropDatabase' then
    // drop cmd from popupmenu
    tndb_ := DBRightClickSelectedItem
  else case DBTree.Selected.Level of  // drop cmd from toolbar
    1 : tndb_ := DBTree.Selected;
    2 : tndb_ := DBTree.Selected.Parent;
  end;

  if tndb_ = nil then raise Exception.Create('Internal error: Cannot drop NIL database.');

  if MessageDlg('Drop Database "'+tndb_.Text+'"?' + crlf + crlf + 'WARNING: You will lose all tables in database '+tndb_.Text+'!', mtConfirmation, [mbok,mbcancel], 0) <> mrok then
    abort;

  Screen.Cursor := crSQLWait;
  try
    ExecUpdateQuery( 'DROP DATABASE ' + mask(tndb_.Text) );
    if OnlyDBs.Count > 0 then
    begin
      if OnlyDBs.IndexOf(tndb_.Text) > -1 then
      begin
        OnlyDBs.Delete( OnlyDBs.IndexOf(tndb_.Text) );
        with TRegistry.Create do
        begin
          if OpenKey(REGPATH + '\Servers\' + FConn.Description, false) then
          begin
            WriteString( 'OnlyDBs', ImplodeStr( ';', OnlyDBs ) );
            CloseKey;
          end;
          Free;
        end;
      end;
    end;
    if tndb_.Selected then begin
      DBTree.Deselect(tndb_);
      tndb_.Parent.Selected := true;
    end;
    tndb_.Delete;
  except
    MessageDLG('Dropping failed.'+crlf+'Maybe '''+tndb_.Text+''' is not a valid database-name.', mtError, [mbOK], 0)
  end;
  Screen.Cursor := crDefault;
end;



procedure TMDIChild.ShowVariablesAndProcesses(Sender: TObject);

  procedure addLVitem( caption: String; commandCount: Int64; totalCount: Int64 );
  var
    i : Integer;
    tmpval : Double;
  begin
    SetLength( VTRowDataListCommandStats, Length(VTRowDataListCommandStats)+1 );
    i := Length(VTRowDataListCommandStats)-1;
    VTRowDataListCommandStats[i].ImageIndex := 86;
    VTRowDataListCommandStats[i].Captions := TStringList.Create;
    caption := Copy( caption, 5, Length(caption) );
    caption := StringReplace( caption, '_', ' ', [rfReplaceAll] );
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
  i, rowindex : Integer;
  questions : Int64;
  ds : TDataSet;
begin
  // Refresh variables and process-list
  Screen.Cursor := crSQLWait;

  // VARIABLES
  ListVariables.BeginUpdate;
  ListVariables.Clear;
  ds := GetResults( 'SHOW VARIABLES', false );
  SetLength( VTRowDataListVariables, ds.RecordCount );
  for i:=1 to ds.RecordCount do
  begin
    VTRowDataListVariables[i-1].ImageIndex := 87;
    VTRowDataListVariables[i-1].Captions := TStringList.Create;
    VTRowDataListVariables[i-1].Captions.Add( ds.Fields[0].AsString );
    VTRowDataListVariables[i-1].Captions.Add( ds.Fields[1].AsString );
    ds.Next;
  end;
  ds.Close;
  FreeAndNil(ds);

  // STATUS
  uptime := 1; // avoids division by zero :)
  questions := 1;
  ds := GetResults( 'SHOW /*!50002 GLOBAL */ STATUS' );
  for i:=1 to ds.RecordCount do
  begin
    if LowerCase( Copy( ds.Fields[0].AsString, 1, 4 ) ) <> 'com_' then
    begin
      rowindex := Length(VTRowDataListVariables);
      SetLength( VTRowDataListVariables, rowindex+1 );
      VTRowDataListVariables[rowindex].ImageIndex := 87;
      VTRowDataListVariables[rowindex].Captions := TStringList.Create;
      VTRowDataListVariables[rowindex].Captions.Add( ds.Fields[0].AsString );
      VTRowDataListVariables[rowindex].Captions.Add( ds.Fields[1].AsString );
      if lowercase( ds.Fields[0].AsString ) = 'uptime' then
        uptime := MakeInt(ds.Fields[1].AsString);
      if lowercase( ds.Fields[0].AsString ) = 'questions' then
        questions := MakeInt(ds.Fields[1].AsString);
    end;
    ds.Next;
  end;

  // Tell VirtualTree the number of nodes it will display
  ListVariables.RootNodeCount := Length(VTRowDataListVariables);
  ListVariables.EndUpdate;
  // Display number of listed values on tab
  tabVariables.Caption := 'Variables (' + IntToStr(ListVariables.RootNodeCount) + ')';

  // Command-Statistics
  ListCommandStats.BeginUpdate;
  ListCommandStats.Clear;
  SetLength( VTRowDataListCommandStats, 0 );
  addLVitem( '    All commands', questions, questions );
  ds.First;
  for i:=1 to ds.RecordCount do
  begin
    if LowerCase( Copy( ds.Fields[0].AsString, 1, 4 ) ) = 'com_' then
    begin
      addLVitem( ds.Fields[0].AsString, MakeInt(ds.Fields[1].AsString), questions );
    end;
    ds.Next;
  end;
  ds.Close;
  FreeAndNil(ds);

  // Tell VirtualTree the number of nodes it will display
  ListCommandStats.RootNodeCount := Length(VTRowDataListCommandStats);
  ListCommandStats.EndUpdate;

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
begin
  // No need to update if it's not visible.
  if PageControlMain.ActivePage <> tabHost then exit;
  if PageControlHost.ActivePage <> tabProcesslist then exit;
  Screen.Cursor := crSQLWait;
  try
    ListProcesses.BeginUpdate;
    ListProcesses.Clear;
    debug('ShowProcessList()');
    ds := GetResults('SHOW FULL PROCESSLIST', false, false);
    SetLength(VTRowDataListProcesses, ds.RecordCount);
    for i:=1 to ds.RecordCount do
    begin
      VTRowDataListProcesses[i-1].Captions := TStringList.Create;
      VTRowDataListProcesses[i-1].Captions.Add( ds.Fields[0].AsString );
      if AnsiCompareText( ds.Fields[4].AsString, 'Killed') = 0 then
        VTRowDataListProcesses[i-1].ImageIndex := 83  // killed
      else
        VTRowDataListProcesses[i-1].ImageIndex := 82; // running
      for j := 1 to 7 do
        VTRowDataListProcesses[i-1].Captions.Add(ds.Fields[j].AsString);
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
  Screen.Cursor := crDefault;
end;

procedure TMDIChild.KillProcess(Sender: TObject);
var t : Boolean;
  ProcessIDs : TStringList;
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
  SQL               : TStringList;
  i                 : Integer;
  rowsaffected      : Integer;
  SQLstart          : Integer;
  SQLend            : Integer;
  SQLscriptstart    : Integer;
  SQLscriptend      : Integer;
  SQLTime           : Double;
  fieldcount        : Integer;
  recordcount       : Integer;
  ds                : TDataSet;
begin
  if ( CurrentLine ) then
  begin
    // Run current line
    SQL := parseSQL( SynMemoQuery.LineText, delimiter, ProcessClientSQL );
  end
  else
  if ( Selection ) then
  begin
    // Run selection
    SQL := parseSQL( SynMemoQuery.SelText, delimiter, ProcessClientSQL );
  end
  else
  begin
    // Run all
    SQL := parseSQL( SynMemoQuery.Text, delimiter, ProcessClientSQL );
  end;

  if ( SQL.Count = 0 ) then
  begin
    LabelResultinfo.Caption := '(nothing to do)';
    Exit;
  end;

  // Destroy old data set.
  ds := DataSource2.DataSet;
  DataSource2.DataSet := nil;
  if ds <> nil then ds.Close;
  FreeAndNil( ds );
  // set db-aware-component's properties..
  DBMemo1.DataField := EmptyStr;
  DBMemo1.DataSource := DataSource2;
  EDBImage1.DataField := EmptyStr;
  EDBImage1.DataSource := DataSource2;

  SQLscriptstart := GetTickCount();
  LabelResultinfo.Caption := EmptyStr;

  ds := nil;
  try
    MainForm.showstatus( 'Initializing SQL...', 2, true );
    Mainform.ExecuteQuery.Enabled := false;
    Mainform.ExecuteSelection.Enabled := false;

    // Let EnsureActiveDatabase know that we've fired user queries.
    UserQueryFiring := true;

    rowsaffected := 0;
    fieldcount := 0;
    recordcount := 0;
    ProgressBarQuery.Max := SQL.Count;
    ProgressBarQuery.Position := 0;
    ProgressBarQuery.Show();

    MainForm.showstatus( 'Executing SQL...', 2, true );
    for i := 0 to (SQL.Count - 1) do
    begin
      ProgressBarQuery.StepIt();
      ProgressBarQuery.Repaint;
      if ( sql[i] = EmptyStr ) then
      begin
        continue;
      end;
      // open last query with data-aware:
      LabelResultinfo.Caption := EmptyStr;
      // ok, let's rock
      SQLstart := GetTickCount();
      try
        ds := GetResults( SQL[i], false, false );
        gridQuery.DataSource.DataSet := ds;
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
          if (
            ( btnQueryStopOnErrors.Down ) or
            ( i = (SQL.Count - 1) )
          ) then
          begin
            Screen.Cursor := crDefault;
            MessageDlg( E.Message, mtError, [mbOK], 0 );
            ProgressBarQuery.Hide();
            Mainform.ExecuteQuery.Enabled := true;
            Mainform.ExecuteSelection.Enabled := true;
            Break;
          end;
        end;
      end;

      SQLend := GetTickCount();
      SQLTime := (SQLend - SQLstart) / 1000;

      LabelResultinfo.Caption :=
        FormatNumber( rowsaffected ) +' row(s) affected, '+
        FormatNumber( fieldcount ) +' field(s) / '+
        FormatNumber( recordcount ) +' record(s) in last result set.';
      if ( SQL.Count = 1 ) then
      begin
        LabelResultinfo.Caption := LabelResultinfo.Caption +
          ' Query time: '+ FormatNumber( SQLTime, 3) +' sec.';
      end;
    end;

    ProgressBarQuery.Hide();
    Mainform.ExecuteQuery.Enabled := true;
    Mainform.ExecuteSelection.Enabled := true;
    // count chars:
    SynMemoQuery.OnChange( Self );

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

    if ( ds <> nil ) then
    begin
      // Re-link to datasource
      DataSource2.DataSet := ds;
      // Flick controls so that column resizing will work (?)
      TZQuery(ds).EnableControls();
      TZQuery(ds).DisableControls();
      // resize all columns, if they are more wide than Mainform.DefaultColWidth
      if ( prefDefaultColWidth <> 0 ) then
      begin
        for i:=0 to gridQuery.Columns.count-1 do
        begin
          if ( gridQuery.Columns[i].Width > prefDefaultColWidth ) then
          begin
            gridQuery.Columns[i].Width := prefDefaultColWidth;
          end;
        end;
      end;
    end;
    // Ensure controls are in a valid state
    ValidateControls();
    viewingdata := false;
    if ( ds <> nil ) then
    begin
      TZQuery(ds).EnableControls();
    end;
    Screen.Cursor := crDefault;
    MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  end;
end;


{**
  Clicked somewhere in the field-list of the "Table"-tabsheet
}
procedure TMDIChild.ListColumnsChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
var
  SomeSelected, OneFocused: Boolean;
begin
  // some columns selected ?
  OneFocused := Assigned(Sender.FocusedNode);
  SomeSelected := Length(Sender.GetSortedSelection(False))>0;

  // Toggle state of menuitems and buttons
  btnTableDropField.Enabled := SomeSelected;
  DropField1.Enabled := SomeSelected;
  MenuEditField.Enabled := OneFocused;
  btnTableEditField.enabled := OneFocused;
  menuRenameColumn.Enabled := OneFocused;
end;


procedure TMDIChild.DropField(Sender: TObject);
var
  tn : TTreeNode;
  i : Integer;
  dropCmd : String;
  dropList : TStringList;
begin
  // Drop Columns

  // We allow the user to select and delete multiple listItems
  dropList := GetVTCaptions( ListColumns, True );

  // In case all listItems are selected:
  if dropList.Count = Length(VTRowDataListColumns) then
  begin
    if MessageDlg('Can''t drop all or the last Field - drop Table '+SelectedTable+'?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
    begin
      Screen.Cursor := crSQLWait;
      ExecUpdateQuery( 'DROP TABLE '+mask(SelectedTable) );
      tn := DBTree.Selected;
      DBTree.Selected := DBTree.Selected.Parent;
      tn.Destroy;
      MenuRefreshClick(self);
      Screen.Cursor := crDefault;
    end;
  end else
  if MessageDlg('Drop ' + IntToStr(dropList.Count) + ' field(s): ' + ImplodeStr( ', ', dropList ) + ' ?', mtConfirmation, [mbok,mbcancel], 0) = mrok then
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

    // Set focus on first item
    ListColumns.FocusedNode := ListColumns.GetFirstVisible;
  except
    On E : Exception do
    begin
      MessageDlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;
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
  i,j,c,t          : Integer;
  tn, child        : TTreeNode;
  sql, kw          : String;
  keywords, tables : TStringList;
  tablename        : String;

  procedure addTable( name: String );
  begin
    SynCompletionProposal1.InsertList.Add( name );
    SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(SynSQLSyn1.TableNameAttri.Foreground)+'}table\color{clWindowText}\column{}' + name );
  end;

  procedure addColumns( tablename: String );
  var
    dbname : String;
    i : Integer;
    ds : TDataSet;
  begin
    dbname := ActiveDatabase;
    if Pos( '.', tablename ) > -1 then
    begin
      dbname := Copy( tablename, 0, Pos( '.', tablename )-1 );
      tablename := Copy( tablename, Pos( '.', tablename )+1, Length(tablename) );
    end;
    tablename := mask( tablename );
    if dbname <> '' then
      tablename := mask( dbname ) + '.' + tablename;
    ds := getResults( 'SHOW COLUMNS FROM '+tablename, true, false );
    if ds = nil then exit;
    for i:=0 to ds.RecordCount-1 do
    begin
      SynCompletionProposal1.InsertList.Add( ds.FieldByName( 'Field' ).AsString );
      SynCompletionProposal1.ItemList.Add( '\hspace{2}\color{'+ColorToString(clTeal)+'}column\color{clWindowText}\column{}' + ds.FieldByName( 'Field' ).AsString + '\style{-B} ' + ds.FieldByName( 'Type' ).AsString );
      ds.Next;
    end;
    ds.Close;
    FreeAndNil(ds);
  end;

begin
  SynCompletionProposal1.InsertList.Clear;
  SynCompletionProposal1.ItemList.Clear;

  // Get column-names into the proposal (Daniel's Top1-wish...)
  // when we write sql like "SELECT t.|col FROM table [AS] t"
  // This is at no means a perfect solution which catches all kind of
  // SQL statements. But for simple SQL it should work fine.

  // 1. find the currently edited sql-statement around the cursor position in synmemo
  j := Length(SynCompletionProposal1.Editor.Text);
  c := 1024;
  for i := SynCompletionProposal1.Editor.SelStart+1024 downto SynCompletionProposal1.Editor.SelStart-1024 do
  begin
    if i > j then
      continue;
    if i < 1 then
    begin
      c := SynCompletionProposal1.Editor.SelStart;
      break;
    end;
    sql := SynCompletionProposal1.Editor.Text[i] + sql;
  end;
  sql := StringReplace( sql, CRLF, ' ', [rfReplaceall] );
  sql := StringReplace( sql, #10, ' ', [rfReplaceall] );
  sql := StringReplace( sql, #13, ' ', [rfReplaceall] );
  // 2. find the position of tablenames after the cursor-position (c)
  keywords := TStringList.Create;
  keywords.CommaText := 'FROM,INTO,UPDATE';
  t := -1;
  for i := c to Length(sql) do // forward from cursor-position
  begin
    kw := Copy( sql, i, Pos( ' ', Copy( sql, i, Length(sql) ) )-1 );
    if keywords.IndexOf( kw ) > -1 then
    begin
      t := i + Length( kw );
      break;
    end;
  end;
  if t = -1 then
  begin
    for i := c downto 0 do  // and backwards from cursor-position
    begin
      kw := Copy( sql, i, Pos( ' ', Copy( sql, i, Length(sql) ) )-1 );
      if keywords.IndexOf( kw ) > -1 then
      begin
        t := i + Length( kw );
        break;
      end;
    end;
  end;
  // 3. find tablenames and aliases, compare them with previous token
  tablename := '';
  if t>-1 then
  begin
    tables := TStringList.Create;
    tables.CommaText := StringReplace( Copy( sql, t, Length(sql)), ' ', ',', [rfReplaceall] );
    for i := 0 to tables.Count - 1 do
    begin
      if (UpperCase(tables[i]) = 'AS') and (i > 0) and (i<tables.Count-1) then
      begin
        if (SynCompletionProposal1.PreviousToken = tables[i-1]) or
          (SynCompletionProposal1.PreviousToken = tables[i+1]) then
        begin
          tablename := tables[i-1]; // Got it!
          break;
        end;
      end;
    end;
  end;
  if (tablename <> '') then
  begin
    // add columns to proposal
    addColumns( tablename );
  end
  else if SynCompletionProposal1.PreviousToken <> '' then
  begin
    // assuming that previoustoken itself is a table
    addColumns( SynCompletionProposal1.PreviousToken );
  end;


  if Length(CurrentInput) = 0 then // makes only sense if the user has typed "database."
  begin
    if OnlyDBs2.IndexOf( SynCompletionProposal1.PreviousToken ) > -1 then
    begin
      // Only display tables from specified db
      Screen.Cursor := crHourGlass;
      for i:=0 to DBTree.Items.Count-1 do
      begin
        tn := DBTree.Items[i];
        if tn.Text = SynCompletionProposal1.PreviousToken then
        begin
          // Ensure the db-node has its tables fetched
          PopulateTreeTableList(tn);
          child := tn.getFirstChild;
          for j:=0 to tn.Count-1 do
          begin
            addTable(child.Text);
            child := tn.getNextChild(child);
          end;
          // We have found the matching node, no need to look further
          Break;
        end;
      end;
      Screen.Cursor := crDefault;
    end;
  end;

  if (SynCompletionProposal1.ItemList.count = 0) and (Length(CurrentInput)>0) then
  begin
    // Add databases
    SynCompletionProposal1.InsertList.AddStrings( OnlyDBs2 );
    SynCompletionProposal1.ItemList.AddStrings( OnlyDBs2 );
    for i:=0 to SynCompletionProposal1.ItemList.count-1 do
      SynCompletionProposal1.ItemList[i] := '\hspace{2}\color{'+ColorToString(SynSQLSyn1.TableNameAttri.Foreground)+'}database\color{clWindowText}\column{}' + SynCompletionProposal1.ItemList[i];

    if ActiveDatabase <> '' then
    begin
      // Add tables
      for i:=0 to Length(VTRowDataListTables)-1 do
      begin
        addTable( VTRowDataListTables[i].Captions[0] );
      end;
      if Length(CurrentInput) = 0 then // assume that we have already a dbname in memo
        SynCompletionProposal1.Position := OnlyDBs2.Count;
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


procedure TMDIChild.SynMemoQueryChange(Sender: TObject);
var somechars : Boolean;
begin
  PanelCharsInQueryWindow.Caption := FormatByteNumber( SynMemoQuery.GetTextLen );
  somechars := Length(SynMemoQuery.Text) > 0;
  Mainform.ExecuteQuery.Enabled := somechars;
  Mainform.ExecuteSelection.Enabled := SynMemoQuery.SelAvail;
  Mainform.ExecuteLine.Enabled := SynMemoQuery.LineText <> '';
  btnQuerySave.Enabled := somechars;
  btnQuerySaveSnippet.Enabled := somechars;
end;



procedure TMDIChild.CreateTable(Sender: TObject);
var
  db : String;
begin
  if Sender = PopupMenuCreateTable then begin
    if DBTree.Selected.Level = 2 then db := DBtree.Selected.Parent.Text;
    if DBTree.Selected.Level = 1 then db := DBtree.Selected.Text;
  end else db := '';
  if CreateTableForm = nil then
    CreateTableForm := TCreateTableForm.Create(Self);
  CreateTableForm.ShowModal;
  CreateTableForm.DBComboBox.SelText := db;
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
  if TimerHostUptime.Enabled then msg := Format(strHostRunning, [msg])
  else msg := Format(strHostNotRunning, [msg]);
  Panel4.Caption := msg;
end;


procedure TMDIChild.FormActivate(Sender: TObject);
begin
  TimerConnected.OnTimer(self);
  ValidateControls;
end;

procedure TMDIChild.FormDeactivate(Sender: TObject);
begin
  ValidateControls( False );
end;


procedure TMDIChild.FormShow(Sender: TObject);
begin
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


{ Edit field }
procedure TMDIChild.UpdateField(Sender: TObject);
var
  fn : String;
  fem : TFieldEditorMode;
begin
  fn := '';
  fem := femFieldAdd;

  if Assigned(ListColumns.FocusedNode) then
    fn := ListColumns.Text[ListColumns.FocusedNode, 0];

  if fn<>'' then
    fem := femFieldUpdate;

  FieldEditorWindow (Self,fem,fn);
end;


{ Add new field }
procedure TMDIChild.MenuAddFieldClick(Sender: TObject);
begin
  FieldEditorWindow (Self,femFieldAdd);
end;


procedure TMDIChild.CreateDatabase(Sender: TObject);
begin
  // Create database:
  // Create modal form once on demand
  if CreateDatabaseForm = nil then
    CreateDatabaseForm := TCreateDatabaseForm.Create(Self);

  // Rely on the modalresult being set correctly
  if CreateDatabaseForm.ShowModal = mrOK then
  begin
    // Add DB to OnlyDBs-regkey if this is not empty
    if OnlyDBs.Count > 0 then
    begin
      OnlyDBs.Add( CreateDatabaseForm.editDBName.Text );
      with TRegistry.Create do
      begin
        if OpenKey(REGPATH + '\Servers\' + FConn.Description, false) then
        begin
          WriteString( 'OnlyDBs', ImplodeStr( ';', OnlyDBs ) );
          CloseKey;
        end;
        Free;
      end;
    end;
    // Todo: Don't expand node of old database in dbtree, just reload and switch to new one
    ReadDatabasesAndTables(self);
    ActiveDatabase := CreateDatabaseForm.editDBName.Text;
  end;
end;


{**
  "Alter table ..."
  called by popupTreeView or popupDbGrid
}
procedure TMDIChild.menuAlterTableClick(Sender: TObject);
var
  NodeData: PVTreeData;
  caller : TPopupMenu;
begin
  if TablePropertiesForm = nil then
    TablePropertiesForm := Ttbl_properties_form.Create(Self);

  TablePropertiesForm.DatabaseName := '';
  caller := TPopupMenu( TMenuItem( Sender ).GetParentMenu );
  if caller = popupTreeView then
  begin
    if DBRightClickSelectedItem.Parent.Text <> ActiveDatabase then
      TablePropertiesForm.DatabaseName := DBRightClickSelectedItem.Parent.Text;
    TablePropertiesForm.TableName := DBRightClickSelectedItem.Text;
  end
  else begin
    NodeData := ListTables.GetNodeData( ListTables.FocusedNode );
    TablePropertiesForm.TableName := NodeData.Captions[0];
  end;

  TablePropertiesForm.ShowModal;
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
    ExecUpdateQuery( 'ALTER TABLE ' + mask(NodeData.Captions[0]) + ' RENAME ' + mask(NewText), False, False );

    if SynSQLSyn1.TableNames.IndexOf( NewText ) = -1 then begin
      SynSQLSyn1.TableNames.Add(NewText);
    end;
    // Update nodedata
    NodeData.Captions[0] := NewText;
    RefreshActiveDbTableList;
    PopulateTreeTableList;
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


procedure TMDIChild.MenuViewBlobClick(Sender: TObject);
begin
  PageControlBottom.ActivePage := tabBlobEditor;
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


procedure TMDIChild.ManageIndexes1Click(Sender: TObject);
begin
  FieldEditorWindow (Self,femIndexEditor);
end;

procedure TMDIChild.Markall3Click(Sender: TObject);
begin
  // select all in history
  SynMemoSQLLog.SelectAll;
end;


procedure TMDIChild.More1Click(Sender: TObject);
begin
  TableDiagnosticsWindow(Self);
end;


procedure TMDIChild.MenuOptimizeClick(Sender: TObject);
var
  i : Integer;
  Selected : TStringList;
begin
  // Optimize tables
  Screen.Cursor := crHourGlass;
  Selected := GetVTCaptions( ListTables, True );
  try
    for i:=0 to Selected.Count-1 do
    begin
      ExecUpdateQuery( 'OPTIMIZE TABLE ' + mask(Selected[i]) );
    end;
    // Update ListTables because relevant table properties probably have changed
    Mainform.Childwin.RefreshActiveDbTableList;
  finally
    Selected.Free;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.MenuCheckClick(Sender: TObject);
var
  i : Integer;
  tables : String;
  Selected : TStringList;
begin
  // Check tables
  Screen.Cursor := crHourGlass;
  tables := '';
  Selected := GetVTCaptions( ListTables, True );
  try
    for i:=0 to Selected.Count-1 do
    begin
      if tables <> '' then
        tables := tables + ', ';
      tables := tables + mask(Selected[i]);
    end;
    ExecUpdateQuery( 'CHECK TABLE ' + tables + ' QUICK' );
  finally
    Selected.Free;
    Screen.Cursor := crDefault;
  end;
end;


{**
  Clear Query memo
}
procedure TMDIChild.menuclearClick(Sender: TObject);
var
  memo : TSynMemo;
begin
  // Clear SynMemo
  if SynMemoFilter.Focused then
    memo := SynMemoFilter
  else
    memo := SynMemoQuery;
  // Make sure to add this step to SynMemo's undo history
  memo.SelectAll;
  memo.SelText := '';
  memo.SelStart := 0;
  memo.SelEnd := 0;
end;

procedure TMDIChild.MenuAnalyzeClick(Sender: TObject);
var
  i : Integer;
  tables : String;
  Selected : TStringList;
begin
  // Analyze tables
  Screen.Cursor := crHourGlass;
  tables := '';
  Selected := GetVTCaptions( ListTables, True );
  try
    for i:=0 to Selected.Count-1 do
    begin
      if tables <> '' then
        tables := tables + ', ';
      tables := tables + mask(Selected[i]);
    end;
    ExecUpdateQuery( 'ANALYZE TABLE ' + tables );
  finally
    Selected.Free;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.MenuRepairClick(Sender: TObject);
var
  i : Integer;
  tables : String;
  Selected : TStringList;
begin
  // Repair tables
  Screen.Cursor := crHourGlass;
  tables := '';
  Selected := GetVTCaptions( ListTables, True );
  try
    for i:=0 to Selected.Count - 1 do
    begin
      if tables <> '' then
        tables := tables + ', ';
      tables := tables + mask(Selected[i]);
    end;
    ExecUpdateQuery( 'REPAIR TABLE ' + tables + ' QUICK' );
    // Update ListTables because relevant table properties probably have changed
    Mainform.Childwin.RefreshActiveDbTableList;
  finally
    Selected.Free;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.ListTablesDblClick(Sender: TObject);
var
  NodeData : PVTreeData;
begin
  // table-doubleclick
  if Assigned(ListTables.FocusedNode) then begin
    NodeData := ListTables.GetNodeData(ListTables.FocusedNode);
    PopulateTreeTableList;
    SelectedTable := NodeData.Captions[0];
    ShowTable(SelectedTable);
  end;
end;


procedure TMDIChild.TimerConnectErrorCloseWindowTimer(Sender: TObject);
begin
  // can't connect -> close MDI-Child
  TimerConnectErrorCloseWindow.Enabled := false;
  Mainform.Showstatus('', 1);
  MainForm.ShowStatus( STATUS_MSG_READY, 2 );
  close;
end;


{**
  Column-title clicked -> generate "ORDER BY"
}
procedure TMDIChild.gridDataTitleClick(Column: TColumn);
var
  c : TOrderCol;
begin
  c := TOrderCol.Create;
  c.ColumnName := Column.FieldName;
  HandleOrderColumns(c);
  ViewData(self);
end;



procedure TMDIChild.Filter1Click(Sender: TObject);
begin
  // Set WHERE-Filter
  PageControlBottom.ActivePage := tabFilter;
  SynMemoFilter.SetFocus;
end;



// open Find-Dialog
procedure TMDIChild.btnQueryFindClick(Sender: TObject);
begin
  FindDialogQuery.execute;
  // if something is selected search for that text
  if SynMemoQuery.SelAvail and (SynMemoQuery.BlockBegin.Line = SynMemoQuery.BlockEnd.Line)
  then
    FindDialogQuery.FindText := SynMemoQuery.SelText
  else
    FindDialogQuery.FindText := SynMemoQuery.GetWordAtRowCol(SynMemoQuery.CaretXY);
end;

// open Replace-Dialog
procedure TMDIChild.btnQueryReplaceClick(Sender: TObject);
begin
  ReplaceDialogQuery.execute;
  // if something is selected search for that text
  if SynMemoQuery.SelAvail and (SynMemoQuery.BlockBegin.Line = SynMemoQuery.BlockEnd.Line)
  then
    ReplaceDialogQuery.FindText := SynMemoQuery.SelText
  else
    ReplaceDialogQuery.FindText := SynMemoQuery.GetWordAtRowCol(SynMemoQuery.CaretXY);
end;

// Search-Dialog is searching...
procedure TMDIChild.FindDialogQueryFind(Sender: TObject);
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
    Mainform.ShowStatus( 'SearchText ''' + Search + ''' not found!', 0);
  end;
end;

{ Find Text for replace-dialog }
procedure TMDIChild.ReplaceDialogQueryFind(Sender: TObject);
begin
  FindDialogQuery.FindText := ReplaceDialogQuery.FindText;
  FindDialogQueryFind( ReplaceDialogQuery );
end;

{ Replace Text with replace-dialog }
procedure TMDIChild.ReplaceDialogQueryReplace(Sender: TObject);
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
    Mainform.ShowStatus( 'SearchText ''' + Search + ''' not found!', 0);
    if ssoBackwards in Options then
      SynMemoQuery.BlockEnd := SynMemoQuery.BlockBegin
    else
      SynMemoQuery.BlockBegin := SynMemoQuery.BlockEnd;
    SynMemoQuery.CaretXY := SynMemoQuery.BlockBegin;
  end;
end;

procedure TMDIChild.MenuLimitClick(Sender: TObject);
begin
  // limit or not limit
  mainform.CheckBoxLimit.Checked := not mainform.CheckBoxLimit.Checked;
  viewdata(self);
end;



procedure TMDIChild.Delete1Click(Sender: TObject);
begin
  // Delete record(s)
  if gridData.SelectedRows.Count = 0 then begin
    if MessageDLG('Delete 1 Record(s)?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
      GetVisualDataSet().Delete(); // unsafe ...
  end else
  if MessageDLG('Delete '+IntToStr(gridData.SelectedRows.count)+' Record(s)?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    gridData.SelectedRows.Delete;
end;


procedure TMDIChild.QuickFilterClick(Sender: TObject);
var
  filter,value,column : String;
  menuitem : TMenuItem;
begin
  // Set filter for "where..."-clause
  value := GetFieldValue(gridData.SelectedField);
  menuitem := (Sender as TMenuItem);
  column := mask(gridData.SelectedField.FieldName);
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

  SynMemoFilter.Text := filter;
  PageControlBottom.ActivePage := tabFilter;
  SynMemoFilter.SetFocus;
  SetFilter(self);
end;


procedure TMDIChild.btnBlobWordWrapClick(Sender: TObject);
begin
  // linebreaks
  WordWrap := not WordWrap;
  btnBlobWordWrap.Down := WordWrap;
  if btnBlobWordWrap.Down then
    DBMemo1.Scrollbars := ssVertical
  else
    DBMemo1.Scrollbars := ssBoth;
end;

procedure TMDIChild.PageControlBlobEditorsChange(Sender: TObject);
begin
  btnBlobCopy.Enabled := true;
  btnBlobLoad.Enabled := not DBMemo1.ReadOnly;
  btnBlobSave.Enabled := true;
  if PageControlBlobEditors.ActivePage = tabBlobEditorText then
  begin
    // MEMO tab activated.
    btnBlobWordWrap.Enabled := true;
  end;
  if PageControlBlobEditors.ActivePage = tabBlobEditorImage then
  begin
    // Image tab activated.
    btnBlobWordWrap.Enabled := false;
  end
end;


// Force a save of the user-edited contents of the BLOB editor.
procedure TMDIChild.SaveBlob;
begin
  if not DBMemo1.Modified then exit;
  if DBMemo1.ReadOnly then exit;
  if Length(DBMemo1.DataField) = 0 then exit;
  DBMemo1.DataSource.DataSet.Post;
  //SendMessage(DBMemo1.Handle, CM_EXIT, 0, 0);
end;


procedure TMDIChild.btnBlobSaveClick(Sender: TObject);
var
  bf: Textfile;
  grid: TTntDBGrid;
begin
  // Todo: Weird fix, we probably shouldn't even be showing the button in the first place.
  if Length(DBMemo1.DataField) = 0 then
  begin
    ShowMessage('Please choose a BLOB field in the data grid before using the "Save BLOB" function.');
    exit;
  end;

  SaveBlob();
  grid := ActiveGrid;

  with TSaveDialog.Create(self) do begin
    case PageControlBlobEditors.ActivePageIndex of
      0 : begin
            Filter := 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
            DefaultExt := 'txt';
          end;
      1 : begin
            Filter := 'Bitmaps (*.bmp)|*.bmp|All files (*.*)|*.*';
            DefaultExt := 'bmp';
          end;
    end;
    FileName := grid.SelectedField.FieldName;
    Options := [ofOverwritePrompt,ofEnableSizing];
    if execute then try
      Screen.Cursor := crHourGlass;
      case PageControlBlobEditors.ActivePageIndex of
        0 : begin
            AssignFile(bf, filename);
            Rewrite(bf);
            Write(bf, GetVisualDataset().FieldByName(DBMemo1.DataField).AsString);
            CloseFile(bf);
          end;
        1 : EDBImage1.Picture.SaveToFile(filename);
      end;
      Screen.Cursor := crDefault;
    except
      Screen.Cursor := crDefault;
      messagedlg('File could not be saved', mterror, [mbok], 0);
    end;
  end;
end;

procedure TMDIChild.btnBlobLoadClick(Sender: TObject);
begin
  // Open BLOB-File
  if not (DataSource1.State in [dsEdit, dsInsert]) then
  try
    DataSource1.Edit;
  except
    exit;
  end;

  with OpenDialog2 do begin
    case PageControlBlobEditors.ActivePageIndex of
      0 : Filter := 'Textfiles (*.txt)|*.txt|All files (*.*)|*.*';
      1 : Filter := 'All Images (*.jpg, *.jpeg, *.bmp)|*.jpg;*.jpeg;*.bmp|JPEG (*.jpg, *.jpeg)|*.jpg;*.jpeg|Bitmap (*.bmp)|*.bmp|All files (*.*)|*.*';
    end;

    if execute then
    case PageControlBlobEditors.ActivePageIndex of
      0 : DBMemo1.Lines.LoadFromFile(filename);
      1 : EDBImage1.Picture.LoadFromFile(filename);
    end;

  end;
end;


procedure TMDIChild.btnFilterLoadClick(Sender: TObject);
var
  menuitem : Tmenuitem;
  m,i : Integer;
  _filename : String;
  dontadd : Boolean;
begin
  // open where-filter
  With TOpenDialog.Create(self) do begin
    Filter := 'Textfiles (*.txt)|*.txt|SQL-Files (*.sql)|*.sql|All files (*.*)|*.*';
    if Execute and (Filename <> '') then begin
      Screen.Cursor := crHourGlass;
      try
        SynMemoFilter.Lines.LoadFromFile(FileName);
      except
        MessageDLG('Error while reading file ''' + filename + '''', mtError, [mbOK], 0);
      end;
      Screen.Cursor := crDefault;

      // don't get one filename more than one time
      dontadd := false;
      for m:=0 to popupFilterOpenFile.Items.Count-1 do begin
        _filename := popupFilterOpenFile.Items[m].Caption;
        i := 0;
        while _filename[i] <> ' ' do
          Inc( i );
        _filename := Copy(_filename, i+1, Length(_filename));
        _filename := Stringreplace(_filename, '&', '', [rfReplaceAll]);
        if _filename = FileName then
          dontadd := true;
      end;

      if not dontadd then begin
        with TRegistry.Create do begin
          openkey(REGPATH, true);
          for i:=1 to 10 do begin
            if not ValueExists('SQLWhereFile'+IntToStr(i)) then
              break;
          end;
          while i > 1 do begin
            WriteString('SQLWhereFile'+IntToStr(i), ReadString('SQLWhereFile'+IntToStr(i-1)));
            dec(i);
          end;
          WriteString('SQLWhereFile1', FileName);

          i := 1;
          popupTreeView.Items.Clear;
          while ValueExists('SQLWhereFile'+IntToStr(i)) do begin
            menuitem := Tmenuitem.Create(self);
            menuitem.Caption := IntToStr(popupFilterOpenFile.Items.count+1) + ' ' + ReadString('SQLWhereFile'+IntToStr(i));
            menuitem.OnClick := LoadSQLWhereFile;
            popupFilterOpenFile.Items.Add(menuitem);
            Inc( i );
          end;
          Free;
        end;
      end;

    end;
  end;
end;

procedure TMDIChild.btnFilterSaveClick(Sender: TObject);
begin
  // save where-filter
  With TSaveDialog.Create(self) do begin
    Filter := 'Textfiles (*.txt)|*.txt|SQL-Files (*.sql)|*.sql|All files (*.*)|*.*';
    FileName := SelectedTable;
    Options := [ofOverwritePrompt,ofEnableSizing];
    DefaultExt := 'txt';
    if Execute and (Filename <> '') then
      try
        SynMemoFilter.Lines.SaveToFile(FileName);
      except
        MessageDLG('Error while reading file ''' + filename + '''', mtError, [mbOK], 0);
      end;
  end;
end;

procedure TMDIChild.SetFilter(Sender: TObject);
var
  where : String;
  reg : TRegistry;
  reg_value : String;
begin
  // set filter for data-tab
  where := trim(self.SynMemoFilter.Text);

  // Store whereclause in Registry
  reg := TRegistry.Create;
  try
    if prefRememberFilters then begin
      reg.openkey( REGPATH + '\Servers\' + FConn.Description, false );
      reg_value := 'WHERECLAUSE_' + ActiveDatabase + '.' + SelectedTable;
      if where <> '' then reg.WriteString( reg_value, where )
      else if reg.ValueExists( reg_value ) then reg.DeleteValue( reg_value );
    end;
  finally
    reg.Free;
  end;

  // store filter for browsing purpose:
  if where <> '' then begin
    if WhereFilters = nil then begin // Create filters-list
      WhereFilters := TStringList.Create;
      btnFilterPrevious.Enabled := true;
      btnFilterNext.Enabled := true;
    end;
    if WhereFilters.IndexOf(where) > -1 then // Filter was previously used:
      WhereFiltersIndex := WhereFilters.IndexOf(where)
    else begin // New Filter:
      WhereFilters.add(where);
      WhereFiltersIndex := WhereFilters.count-1;
    end;
    ComboBoxWhereFilters.Items := WhereFilters;
    ComboBoxWhereFilters.ItemIndex := WhereFiltersIndex;
  end;
  Filter1.checked := where <> '';

  viewdata(self);
end;

procedure TMDIChild.ClearFilter(Sender: TObject);
begin
  SynMemoFilter.Lines.Clear;
  SetFilter(self);
end;


procedure TMDIChild.LoadSQLWhereFile(Sender: TObject);
var
  filename : String;
  i : Integer;
begin
  // Load SQLWhere File from Menuitem
  Screen.Cursor := crHourGlass;
  filename := (sender as TMenuItem).Caption;
  i := 0;
  while filename[i] <> ' ' do
    Inc( i );
  filename := Copy(filename, i+1, Length(filename));
  filename := Stringreplace(filename, '&', '', [rfReplaceAll]);

  try
    SynMemoFilter.Lines.LoadFromFile(filename);
  except
    MessageDLG('Error while reading file ''' + filename + '''', mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;
end;

procedure TMDIChild.DropFilter1Click(Sender: TObject);
begin
  // Drop Filter
  ClearFilter(Self);
  viewdata(self);
end;


// select all tables
procedure TMDIChild.selectall1Click(Sender: TObject);
begin
  ListTables.SelectAll(False);
end;

procedure TMDIChild.popupQueryPopup(Sender: TObject);
var
  somechars         : Boolean;
begin
  // Sets cursor into memo and activates TAction(s) like paste
  SynMemoQuery.SetFocus;
  somechars := SynMemoQuery.GetTextLen > 0;
  // Inserting file at cursor only makes sense with content
  MenuInsertFileAtCursor.Enabled := somechars;
  Menusave.Enabled := somechars;
  MenuSaveSelectionToFile.Enabled := SynMemoQuery.SelAvail;
  MenuSaveAsSnippet.Enabled := somechars;
  MenuSaveSelectionAsSnippet.Enabled := SynMemoQuery.SelAvail;
  MenuClear.Enabled := somechars;
  menuSQLHelp.Enabled := (mysql_version >= 40100) and (SynMemoQuery.WordAtCursor <> '');
  // Insert keyword into menuitem, so it's very clear what the menuitem does
  menuSQLHelp.Caption := 'Lookup "'+sstr(SynMemoQuery.WordAtCursor,50)+'" in SQL help ...';
end;

procedure TMDIChild.popupResultGridPopup(Sender: TObject);
begin
  // data available?
  // MainForm.Save2CSV.enabled :=
end;

procedure TMDIChild.gridDataColumnMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
begin
  // prevent OnTitleClick from being executed:
  abort;
end;

procedure TMDIChild.gridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ed: TCustomEdit;
begin
  if Button = mbRight then begin
    // Select text in in-place editor to make popup menu copy and paste work.
    (Sender as TDBGrid).EditorMode := true;
    ed := TCustomEdit(FindControl(GetFocus()));
    ed.SelectAll;
    //debug('right-click catched, selected all text ''' + ed.SelText + '''.');
    // Popup menu manually, mucking about with the grid causes it to stop doing it by itself.
    if sender = gridData then begin
      popupDataGrid.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end else if sender = gridQuery then begin
      popupResultGrid.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end else begin
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TMDIChild.controlsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Grid : TTNTDBGrid;
begin
  // Check for F1-pressed
  if Key = VK_F1 then
    CallSQLHelp( self )

  // Simulate Ctrl+A-behaviour of common editors
  else if ( Shift = [ssCtrl] ) and ( Key = Ord('A') ) then
  begin
    if Sender = DBMemo1 then
      DBMemo1.SelectAll
    else if Sender = EditDataSearch then
      EditDataSearch.SelectAll;
  end

  // Enable copy + paste shortcuts in dbgrids
  else if (Sender is TTNTDBGrid) and (not TTNTDBGrid(Sender).EditorMode) and (Shift = [ssCtrl]) then
  begin
    Grid := Sender as TTNTDBGrid;
    // TODO: Clipboard.AsText is not Unicode safe!
    if Key = Ord('C') then
      Clipboard.AsText := Grid.SelectedField.AsWideString
    else if Key = Ord('V') then begin
      // Ensure dataset is in editing mode, otherwise we'll get an exception while
      // trying to insert content into a cell.
      // Hint: Dataset.Edit is not the same as Grid.EditorMode=True, it just enables
      // internal editing of the record. The grid's editing mode is not activated here.
      Grid.DataSource.DataSet.Edit;
      Grid.SelectedField.Text := Clipboard.AsText;
    end
    else if Key = Ord('X') then begin
      Clipboard.AsText := Grid.SelectedField.AsWideString;
      Grid.DataSource.DataSet.Edit;
      Grid.SelectedField.Text := '';
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
  Accept := (src = DBTree) or (src = lboxQueryHelpers);
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
  Text : String;
  LoadText : Boolean;
begin
  // dropping a TTreeNode into the query-memo
  SynMemoQuery.UndoList.AddGroupBreak;
  src := Source as TControl;
  Text := 'Error: Unspecified source control in drag''n drop operation!';
  LoadText := True;
  // Check for allowed controls as source has already
  // been performed in OnDragOver. So, only do typecasting here.
  if src is TTreeView then
  begin
    Text := (src as TTreeView).Selected.Text;
  end
  else if (src = lboxQueryHelpers) and ((src as TListBox).ItemIndex > -1) then
  begin
    Text := (src as TListBox).Items[(src as TListBox).ItemIndex];
    if tabsetQueryHelpers.TabIndex = 3 then
    begin
      QueryLoad( DIRNAME_SNIPPETS + Text + '.sql', False );
      LoadText := False;
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

procedure TMDIChild.SynMemoQueryKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SynMemoQuery.OnChange(self);
  controlsKeyUp( Sender, Key, Shift );
end;

procedure TMDIChild.SynMemoQueryMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SynMemoQuery.OnChange(Self);
end;

procedure TMDIChild.popupHostPopup(Sender: TObject);
begin
  Kill1.Enabled := (PageControlHost.ActivePage = tabProcessList) and Assigned(ListProcesses.FocusedNode);
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
begin
  // toggle drop-items and remember right-clicked item
  PopupMenuDropDatabase.Enabled := DBtree.Selected.Level = 1;
  PopupMenuCreateTable.Enabled := DBtree.Selected.Level in [1,2];
  menuAlterDatabase.Enabled := (DBtree.Selected.Level = 1) and (mysql_version >= 50002);
  if mysql_version < 50002 then
    menuAlterDatabase.Hint := STR_NOTSUPPORTED
  else
    menuAlterDatabase.Hint := 'Rename and/or modify character set of database';
  menuTreeAlterTable.Enabled := DBtree.Selected.Level = 2;
  MainForm.DropTable.Enabled := DBtree.Selected.Level = 2;
end;


procedure TMDIChild.btnQuerySaveSnippetClick(Sender: TObject);
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
      0: SaveToFile( SynMemoQuery.Text, snippetname, seUTF8 );
      1: SaveToFile( SynMemoQuery.SelText, snippetname, seUTF8 );
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

procedure TMDIChild.CallSQLHelp(Sender: TObject);
var
  keyword : String;
begin
  // Call SQL Help from various places
  if mysql_version < 40100 then
    exit;
  
  keyword := '';
  // Query-Tab
  if SynMemoQuery.Focused then
    keyword := SynMemoQuery.WordAtCursor
  // LogSQL-Tab
  else if SynMemoSQLLog.Focused then
    keyword := SynMemoSQLLog.WordAtCursor
  // Filter-Tab
  else if SynMemoFilter.Focused then
    keyword := SynMemoFilter.WordAtCursor
  // Data-Tab
  else if (PageControlMain.ActivePage = tabData)
    and (-1 < gridData.SelectedField.Index)
    and (gridData.SelectedField.Index <= Length(VTRowDataListColumns)) then
  begin
    keyword := VTRowDataListColumns[gridData.SelectedField.Index].Captions[1];
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
procedure TMDIChild.CallSQLHelpWithKeyword( keyword: String );
begin
  // Set help-keyword and show window
  SQLhelpWindow(self, keyword);
end;

procedure TMDIChild.btnQuerySaveClick(Sender: TObject);
begin
  // Save SQL
  if SaveDialogSQLFile.Execute then
  begin
    Screen.Cursor := crHourGlass;
    // Save complete content or just the selected text,
    // depending on the tag of calling control
    case (Sender as TComponent).Tag of
      0: SaveToFile( SynMemoQuery.Text, SaveDialogSQLFile.FileName, seUTF8 );
      1: SaveToFile( SynMemoQuery.SelText, SaveDialogSQLFile.FileName, seUTF8 );
    end;
    Screen.Cursor := crDefault;
  end;
end;


procedure TMDIChild.popupQueryLoadClick( sender: TObject );
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


procedure TMDIChild.btnQueryLoadClick(Sender: TObject);
begin
  // Click on the btnQueryLoad
  if OpenDialogSQLfile.Execute then
  begin
    QueryLoad( OpenDialogSQLfile.FileName );
  end;
end;


{**
  Insert SQL file at cursor
}
procedure TMDIChild.menuInsertFileAtCursorClick(Sender: TObject);
begin
  if OpenDialogSQLfile.Execute then
  begin
    QueryLoad( OpenDialogSQLfile.FileName, False );
  end;
end;


procedure TMDIChild.AddOrRemoveFromQueryLoadHistory( filename: String; AddIt: Boolean = true; CheckIfFileExists: Boolean = true );
var
  i                     : Integer;
  Values, newfilelist   : TStringList;
  reg                   : TRegistry;
  savedfilename         : String;
begin
  // Add or remove filename to/from history, avoiding duplicates

  reg := TRegistry.Create;
  reg.openkey(REGPATH, true);
  newfilelist := TStringList.create;
  Values := TStringList.create;
  reg.GetValueNames( Values );

  // Add new filename
  if AddIt then
  begin
    newfilelist.Add( filename );
  end;

  // Add all other filenames
  for i:=0 to Values.Count-1 do
  begin
    if Pos( 'SQLFile', Values[i] ) <> 1 then
      continue;
    savedfilename := reg.ReadString( Values[i] );
    reg.DeleteValue( Values[i] );
    if CheckIfFileExists and (not FileExists( savedfilename )) then
      continue;
    if (savedfilename <> filename) and (newfilelist.IndexOf(savedfilename)=-1) then
      newfilelist.add( savedfilename );
  end;

  // Save new list
  for i := 0 to newfilelist.Count-1 do
  begin
    if i >= 20 then
      break;
    reg.WriteString( 'SQLFile'+IntToStr(i), newfilelist[i] );
  end;

  reg.Free;
end;


procedure TMDIChild.QueryLoad( filename: String; ReplaceContent: Boolean = true );

var
  filecontent      : WideString;
  msgtext          : String;
  WStrings         : TWideStringList;
begin
  // Ask for action when loading a big file
  if _GetFileSize( filename ) > LOAD_SIZE then
  begin
    msgtext := 'The file you are about to load is bigger than '+FormatByteNumber(LOAD_SIZE, 0)+'.' + CRLF + CRLF +
      'Do you want to just run the file to avoid loading it completely into the query-editor ( = memory ) ?' + CRLF + CRLF +
      'Press' + CRLF +
      '  [Yes] to run the file without loading it into the editor' + CRLF +
      '  [No] to load the file into the query editor' + CRLF +
      '  [Cancel] to cancel file opening.';
    case MessageDlg( msgtext, mtWarning, [mbYes, mbNo, mbCancel], 0 ) of
      mrYes: // Run the file, don't load it into the editor
        begin
          RunSQLFileWindow( Self, filename );
          // Add filename to history menu
          if Pos( DIRNAME_SNIPPETS, filename ) = 0 then
            AddOrRemoveFromQueryLoadHistory( filename, true );
          // Don't load into editor
          abort;
        end;

      mrNo:; // Do nothing, just load the file normally into the editor

      mrCancel: // Cancel opening file
        abort;

    end;
  end;

  // Load file and add that to the undo-history of SynEdit.
  // Normally we would do a simple SynMemo.Lines.LoadFromFile but
  // this would prevent SynEdit from adding this step to the undo-history
  // so we have to do it by replacing the SelText property
  Screen.Cursor := crHourGlass;
  try
    WStrings := TWideStringList.Create;
    LoadFromFile(WStrings, filename, seUTF8);
    filecontent := WStrings.Text;
  except
    on E: Exception do
    begin
      MessageDLG( 'Error while reading file ' + filename + ':' + CRLF + CRLF + E.Message, mtError, [mbOK], 0);
      AddOrRemoveFromQueryLoadHistory( filename, false );
      FillPopupQueryLoad;
      Screen.Cursor := crDefault;
      exit;
    end;
  end;

  SynCompletionProposal1.Editor.UndoList.AddGroupBreak;
  SynMemoQuery.BeginUpdate;
  if ReplaceContent then
  begin
    SynMemoQuery.SelectAll;
  end;
  SynMemoQuery.SelText := filecontent;
  SynMemoQuery.SelStart := SynMemoQuery.SelEnd;
  SynMemoQuery.EndUpdate;
  SynMemoQueryChange( self );

  if Pos( DIRNAME_SNIPPETS, filename ) = 0 then
    AddOrRemoveFromQueryLoadHistory( filename, true );
  FillPopupQueryLoad;

  Screen.Cursor := crDefault;
end;


procedure TMDIChild.FillPopupQueryLoad;
var
  i, j                       : Integer;
  menuitem, snippetsfolder   : TMenuItem;
  snippets                   : TStringList;
  sqlFilename                : String;
begin
  // Fill the popupQueryLoad menu

  popupQueryLoad.Items.Clear;

  // Snippets
  snippets := getFilesFromDir( DIRNAME_SNIPPETS, '*.sql', true );
  snippetsfolder := TMenuItem.Create( popupQueryLoad );
  snippetsfolder.Caption := 'Snippets';
  popupQueryLoad.Items.Add(snippetsfolder);
  for i := 0 to snippets.Count - 1 do
  begin
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
  with TRegistry.Create do
  begin
    openkey(REGPATH, true);
    j := 0;
    for i:=0 to 19 do
    begin
      if not ValueExists('SQLFile'+IntToStr(i)) then
        continue;
      sqlFilename := ReadString( 'SQLFile'+IntToStr(i) );
      inc(j);
      menuitem := TMenuItem.Create( popupQueryLoad );
      menuitem.Caption := IntToStr(j) + ' ' + sqlFilename;
      menuitem.OnClick := popupQueryLoadClick;
      popupQueryLoad.Items.Add(menuitem);
    end;
    Free;
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

procedure TMDIChild.PopupQueryLoadRemoveAbsentFiles( sender: TObject );
begin
  AddOrRemoveFromQueryLoadHistory( '', false, true );
  FillPopupQueryLoad;
end;



procedure TMDIChild.btnFilterPreviousClick(Sender: TObject);
begin
  // Go to previous filter
  if WhereFiltersIndex > 0 then begin
    dec(WhereFiltersIndex);
    ComboBoxWhereFilters.ItemIndex := WhereFiltersIndex;
    SynMemoFilter.Text := WhereFilters[WhereFiltersIndex];
  end;
end;

procedure TMDIChild.btnFilterNextClick(Sender: TObject);
begin
  // Go to next filter
  if WhereFiltersIndex < WhereFilters.count-1 then begin
    inc(WhereFiltersIndex);
    ComboBoxWhereFilters.ItemIndex := WhereFiltersIndex;
    SynMemoFilter.Text := WhereFilters[WhereFiltersIndex];
  end;
end;

procedure TMDIChild.ComboBoxWhereFiltersChange(Sender: TObject);
begin
  WhereFiltersIndex := ComboBoxWhereFilters.ItemIndex;
  SynMemoFilter.Text := ComboBoxWhereFilters.Items[ComboBoxWhereFilters.ItemIndex];
end;

procedure TMDIChild.btnQueryStopOnErrorsClick(Sender: TObject);
begin
  StopOnErrors := not StopOnErrors;
  btnQueryStopOnErrors.Down := StopOnErrors;
end;

procedure TMDIChild.btnTableViewDataClick(Sender: TObject);
begin
  ShowTableData(SelectedTable);
end;

procedure TMDIChild.btnUnsafeEditClick(Sender: TObject);
var
  confirmed: Boolean;
  msg: String;
begin
  msg :=
    'This will remove the first NUL character _and all text following it_.' + #13#10 +
    'All newlines will be converted to CRLF format.' + #13#10 +
    'Are you sure?';
  confirmed := MessageDlg(msg, mtConfirmation, mbYesNo, 0) = mrYes;
  if not confirmed then exit;
  DBMemo1.ReadOnly := false;
  DBMemo1.Color := clWindow;
  btnUnsafeEdit.Enabled := false;
  DBMemo1.SetFocus;
  // Sets the grid in edit-state so posting won't ignore changes
  gridData.DataSource.Edit;
end;

procedure TMDIChild.DBGridDblClick(Sender: TObject);
begin
  // If grid is not empty...
  if (Sender as TTntDBGrid).SelectedField <> nil then begin
    // Set focus on DBMemo when user doubleclicks a (MEMO)-cell
    if (sender as TTntDBGrid).SelectedField.IsBlob and (PageControlBlobEditors.ActivePage = tabBlobEditorText) then begin
      PageControlBottom.ActivePage := tabBlobEditor;
      DBMemo1.SetFocus;
    end;
  end;
end;

procedure TMDIChild.SaveDialogExportDataTypeChange(Sender: TObject);
begin
  // Set default file-extension of saved file and options on the dialog to show
  with SaveDialogExportData do begin
    Case FilterIndex of
      1 : begin
        DefaultExt := 'csv';
        VisibleOptions := voCSV;
      end;
      2 : begin
        DefaultExt := 'html';
        VisibleOptions := voHTML;
      end;
      3 : begin
        DefaultExt := 'xml';
        VisibleOptions := voHTML;
      end;
    end;
  end;
end;


{**
  A cell in a DBGrid is painted. Sets custom background color NULL fields.
}
procedure TMDIChild.GridDrawColumnCell(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Grid : TTntDBGrid;
begin
  Grid := Sender as TTntDBGrid;
  if (Grid.Fields[DataCol] <> nil) and Grid.Fields[DataCol].IsNull then
  begin
    Grid.Canvas.Font.Color := COLOR_NULLVALUE;
    // Just use the changed font color for (MEMO) and (BLOB) cells
    if Grid.Fields[DataCol].DataType in [ftMemo, ftBlob] then
      Grid.DefaultDrawColumnCell(Rect, DataCol, Grid.Columns[DataCol], State)
    // ... while the natively displayed datatypes get a grey (NULL) 
    else
      Grid.Canvas.TextOut(Rect.Left+2, Rect.Top+2, '(NULL)');
  end;
end;


procedure TMDIChild.DBMemo1Exit(Sender: TObject);
var
  ds: TDataSource;
begin
  // Save changes.  Fixes issue #1538021.
  ds := DBMemo1.DataSource;
  if ds.State in [dsEdit, dsInsert] then begin
    ds.DataSet.Post;
  end;
end;

procedure TMDIChild.popupDataGridPopup(Sender: TObject);
var
  y,m,d,h,i,s,ms : Word;
  cpText, selectedColumn, value : String;
const
  CLPBRD : String = 'CLIPBOARD';
begin
  {DONE  -oFrancisco -cData-browsing:Bugfix: [1650528] Access violation with F5}
  if (gridData.SelectedField <> nil) then
  begin
    DataInsertDateTime.Enabled := gridData.SelectedField.DataType in [ftString, ftDatetime, ftDate, ftTime];
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
    selectedColumn := mask(gridData.SelectedField.FieldName);
    // 1. block: include selected columnname and value from datagrid in caption
    value := sstr(GetFieldValue(gridData.SelectedField), 100);
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
  DataSource1.Edit;
  gridData.SelectedField.AsString := d;
end;

procedure TMDIChild.btnBlobCopyClick(Sender: TObject);
begin
  if dbmemo1.DataField = '' then exit;
  SaveBlob;
  case PageControlBlobEditors.ActivePageIndex of
    0 : clipboard.astext := GetVisualDataset().FieldByName(DBMemo1.DataField).AsString;
    1 : EDBImage1.CopyToClipboard;
  end;
end;

procedure TMDIChild.setNULL1Click(Sender: TObject);
begin
  if not (DataSource1.State in [dsEdit, dsInsert]) then
    DataSource1.Edit;
  gridData.SelectedField.Clear;
end;


procedure TMDIChild.ZQueryGridAfterPost(DataSet: TDataSet);
var
  affected_rows_str, msg  : String;
  affected_rows_int       : Int64;
begin
  affected_rows_int := FMysqlConn.Connection.GetAffectedRowsFromLastPost;
  affected_rows_str := FormatNumber( affected_rows_int );
  if affected_rows_int = 0 then
  begin
    LogSQL( 'Affected rows: ' + affected_rows_str );
    // Refresh grid to show the user that no change has been applied
    GetVisualDataset().Refresh;
    msg := 'Warning: No row was affected by the last update.' + CRLF + CRLF
      + 'This is most likely caused by entering data which the MySQL-server has converted silently.' + CRLF
      + 'For example when you enter a date like "0000-01-01" (applies only to newer MySQL-versions).';
    LogSQL( msg );
    MessageDlg( msg, mtWarning, [mbOK], 0);
  end;
  if affected_rows_int > 1 then
  begin
    LogSQL( 'Affected rows: ' + affected_rows_str );
    // Refresh grid to show the user which values the other records got
    GetVisualDataset().Refresh;
    msg := 'Warning: Consistency problem detected.' + CRLF + CRLF
      + 'The last query affected ' + affected_rows_str + ' rows, when it should have touched only 1 row!'
      + CRLF + CRLF
      + 'This is most likely caused by not having a primary key in the table''s definition.';
    LogSQL( msg );
    MessageDlg( msg, mtWarning, [mbOK], 0);
  end;

  // Display row count and filter-matchings above dbgrid
  DisplayRowCountStats(DataSet);
end;

procedure TMDIChild.ZQueryGridBeforeClose(DataSet: TDataSet);
begin
  // unassign data-aware controls
  DBMemo1.DataField := '';
  EDBImage1.DataField := '';
end;


procedure TMDIChild.ExecUseQuery(db: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false);
begin
  ExecUpdateQuery('USE ' + mask(db), HandleErrors, DisplayErrors);
  FConn.MysqlParams.Database := db;
end;


{***
  Execute a query without returning a resultset
  The currently active connection is used

  @param String The single SQL-query to be executed on the server
}
function TMDIChild.ExecUpdateQuery(sql: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): Int64;
var
  MysqlQuery : TMysqlQuery;
  ds: TDataSet;
begin
  Result := -1; // Silence compiler warning.
  MysqlQuery := nil;
  try
    try
      // Start query execution
      MysqlQuery := RunThreadedQuery(sql);
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
function TMDIChild.ExecSelectQuery(sql: string; HandleErrors: Boolean = false; DisplayErrors: Boolean = false): TDataSet;
var
  res: TMysqlQuery;
begin
  res := nil;
  result := nil;
  try
    try
      // Start query execution
      res := RunThreadedQuery(sql);
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
function TMDIChild.GetResults( SQLQuery: String; HandleErrors: Boolean = false; DisplayErrors: Boolean = false ): TDataSet;
begin
  result := ExecSelectQuery(SQLQuery, HandleErrors, DisplayErrors);
end;


{***
  Execute a query and return String from column x
}
function TMDIChild.GetVar( SQLQuery: String; x: Integer = 0; HandleErrors: Boolean = false; DisplayErrors: Boolean = false) : String;
var
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors );
  if ds = nil then exit;
  Result := ds.Fields[x].AsString;
  ds.Close;
  FreeAndNil(ds);
end;


function TMDIChild.GetNamedVar( SQLQuery: String; x: String; HandleErrors: Boolean = false; DisplayErrors: Boolean = false) : String;
var
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors );
  if ds = nil then exit;
  Result := ds.Fields.FieldByName(x).AsString;
  ds.Close;
  FreeAndNil(ds);
end;

{***
  This returns the dataset object that is currently visible to the user,
  depending on with tabsheet is active.

  @return TDataset if data/query tab is active, nil otherwise.
}
function TMDIChild.GetVisualDataset: TDataSet;
begin

  case PageControlMain.ActivePageIndex of
    3: Result := gridData.DataSource.DataSet;
    4: Result := gridQuery.DataSource.DataSet;
  else
    Result := nil;
  end;
end;


{***
  Execute a query and return column x as Stringlist

  @param  String SQL query String
  @param  Integer 0-based column index in the resultset to return
  @return TStringList
}
function TMDIChild.GetCol( SQLQuery: String; x: Integer = 0; HandleErrors: Boolean = false; DisplayErrors: Boolean = false ) : TStringList;
var
  i: Integer;
  ds: TDataSet;
begin
  ds := GetResults( SQLQuery, HandleErrors, DisplayErrors);
  Result := TStringList.create();
  if ds = nil then exit;
  for i := 0 to ds.RecordCount - 1 do
  begin
    Result.Add( ds.Fields[x].AsString );
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
  LogSQL( Trim( Event.Message ), (Event.Category <> lcExecute) );
end;


// Resize image to fit
procedure TMDIChild.ResizeImageToFit;
var
  height_needed : Cardinal;
begin
  // Abort zooming if it's not an image or image-tab is sized to minimum size
  if (EDBImage1.Picture.Width < 1) or (ScrollBox1.Height < 1) then
    exit;

  // Use all height we can get, but only as much we need to display without zooming
  if EDBImage1.Picture.Height > Scrollbox1.Height then
    height_needed := Scrollbox1.Height
  else
    height_needed := EDBImage1.Picture.Height;

  // Set size
  EDBImage1.Height := height_needed;
  EDBImage1.Width := MulDiv(height_needed, EDBImage1.Picture.Width, EDBImage1.Picture.Height);

  MainForm.showstatus('Image: ' + IntToStr( EDBImage1.Picture.width)
    + ' x ' + IntToStr( EDBImage1.Picture.Height ) + ' pixel, '
    + 'zoomed to ' + IntToStr(Round( 100 / EDBImage1.Picture.Height * EDBImage1.Height )) + '%'
  );
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
    res := ExecPostAsync(FConn,nil,FProgressForm.Handle,ds);
    WaitForQueryCompletion(FProgressForm, res);
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
function TMDIChild.RunThreadedQuery(AQuery: String): TMysqlQuery;
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
    Result := ExecMysqlStatementAsync (AQuery,FConn,nil,FProgressForm.Handle,RunAsyncPost);

    { Repeatedly check if the query has finished by inspecting FQueryRunning
      Allow repainting of user interface
    }
    WaitForQueryCompletion(FProgressForm, Result);
  finally
    FQueryRunning := false;
  end;
end;

procedure TMDIChild.Splitter2Moved(Sender: TObject);
begin
  ResizeImageToFit;
end;

{***
  Used to catch when the highlighted row in a grid changes.
  Possibly the AfterScroll event would be better suited for the purpose?
*}
procedure TMDIChild.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  //debug('DataSourceDataChange()');
  // Avoid calling when switching data source to another TDataset.
  if not viewingdata then GridHighlightChanged(Sender);
end;

procedure TMDIChild.DBGridColEnter(Sender: TObject);
begin
  //debug('DBGridColEnter()');
  GridHighlightChanged(Sender);
end;

procedure TMDIChild.GridHighlightChanged(Sender: TObject);
var
  grid: TTntDBGrid;
  ds: TDataSource;
  hasNull: Boolean;
  hasOddNewlines: Boolean;
  s: String;
begin
  // Current highlighted row or column in a grid has changed.
  // Happens twice (row, then column) if neither row nor column match previously selected field.
  grid := ActiveGrid;
  ds := grid.DataSource;
  if grid.SelectedField = nil then exit;
  debug('GridHighlightChanged');

  if DBMemo1.DataSource <> ds then
  begin
    DBMemo1.DataField := '';
    DBMemo1.DataSource := ds;
    EDBImage1.DataField := '';
    EDBImage1.DataSource := ds;
  end;

  if grid.SelectedField.IsBlob then
  begin
    // Connect the BLOB-components to this field
    DBMemo1.DataField := grid.SelectedField.FieldName;
    // Hack: If 8 bytes or less, don't try to interpret as an image.
    // Fixes BIT fields, which EDBImage otherwise think contains icons.
    if Length(grid.SelectedField.AsString) > 8 then begin
      EDBImage1.DataField := grid.SelectedField.FieldName;
    end;

    // If user is already editing then we're here by accident.
    if not DBMemo1.Focused then begin
      // Disable text editor if there's binary data or odd newlines in the field,
      // since the text editor may/will silently corrupt it if used.
      hasNull := hasNullChar(DBMemo1.Field.AsString);
      hasOddNewlines := hasIrregularNewlines(DBMemo1.Field.AsString);
      DBMemo1.ReadOnly := hasNull or hasOddNewlines;
      btnUnsafeEdit.Enabled := hasNull or hasOddNewlines;
      if btnUnsafeEdit.Enabled then
      begin
        s := 'Cannot edit text because BLOB contains ';
        if hasNull then s := s + 'NUL characters';
        if hasOddNewlines then begin
          if hasNull then s := s + ' and ';
          s := s + 'non-CRLF newlines';
        end;
        s := s + '. Click here to remove NUL plus following text and convert all newlines to CRLF format.';
      end
      else
      begin
        s := 'No irregular characters found in BLOB. It is safe to edit this text.';
      end;
      btnUnsafeEdit.Hint := s;
    end;

    // Ensure visibility of the Blob-Editor
    PageControlBottom.ActivePage := tabBlobEditor;
    MenuViewBlob.Enabled := true;

    // Detect if we have picture-data in this BLOB and
    // if yes, bring the viewer in the BLOB-editor to the front
    if grid.Focused then begin
      if EDBImage1.Picture.Height > 0 then
      begin
        PageControlBlobEditors.ActivePage := tabBlobEditorImage;
      end
      else
      begin
        PageControlBlobEditors.ActivePage := tabBlobEditorText;
      end;
    end;
    ResizeImageToFit;

  end
  else
  begin
    // No BLOB selected, so disconnect the Blob-components
    // from any field
    DBMemo1.ReadOnly := true;
    DBMemo1.DataField := '';
    EDBImage1.DataField := '';
    MenuViewBlob.Enabled := false;
  end;

  // Indicate the ReadOnly-state of the BLOB to the user
  if DBMemo1.ReadOnly then
  begin
    DBMemo1.Color := clInactiveCaptionText;
  end
  else
  begin
    DBMemo1.Color := clWindow;
  end;

  PageControlBlobEditorsChange(self);
end;


// Search with searchbox
procedure TMDIChild.ButtonDataSearchClick(Sender: TObject);
var
  i : Integer;
  where : String;
begin
  if gridData.DataSource.DataSet = nil then Exit;
  if not gridData.DataSource.DataSet.Active then Exit;
  where := '';
  if EditDataSearch.text <> '' then
  begin
    for i:=0 to gridData.FieldCount-1 do
    begin
      if where <> '' then
        where := where + CRLF + ' OR ';
      where := where + mask(gridData.Fields[i].FieldName) + ' LIKE ''%' + esc( EditDataSearch.text, true ) + '%''';
    end;
    if CheckBoxDataSearch.Checked then
      where := 'NOT (' + where + ')';
  end;

  SynMemoFilter.Text := where;

  SetFilter(self);
end;

// Searchbox focused
procedure TMDIChild.EditDataSearchEnter(Sender: TObject);
begin
  ButtonDataSearch.Default := true;
  EditDataSearch.SelectAll;
end;

// Searchbox unfocused
procedure TMDIChild.EditDataSearchExit(Sender: TObject);
begin
  ButtonDataSearch.Default := false;
end;



function TMDIChild.mask(str: String) : String;
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



function TMDIChild.GetActiveGrid: TTntDBGrid;
begin
  Result := nil;
  if PageControlMain.ActivePage = tabData then Result := gridData;
  if PageControlMain.ActivePage = tabQuery then Result := gridQuery;
end;


function TMDIChild.GetActiveDatabase: string;
begin
  // Find currently selected database node in database tree,
  // or the parent if a table is currently selected.
  if DBTree.Selected = nil then Result := ''
  else case DBTree.Selected.Level of
      2: Result := DBTree.Selected.Parent.Text;
      1: Result := DBTree.Selected.Text;
    else Result := '';
  end;
end;


function TMDIChild.GetSelectedTable: string;
begin
  if DBTree.Selected = nil then Result := ''
  else case DBTree.Selected.Level of
      2: Result := DBTree.Selected.Text;
    else Result := '';
  end;
end;


procedure TMDIChild.DisableTreeEvents;
var
  p: PMethod;
begin
  p := new(PMethod);
  p^.Code := TMethod(DBTree.OnChange).Code;
  p^.Data := TMethod(DBTree.OnChange).Data;
  methodStack.Push(p);
  p := new(PMethod);
  p^.Code := TMethod(DBTree.OnChanging).Code;
  p^.Data := TMethod(DBTree.OnChanging).Data;
  methodStack.Push(p);
  DBTree.OnChange := nil;
  DBTree.OnChanging := nil;
end;


procedure TMDIChild.EnableTreeEvents(invokeChanged: TTreeNode);
var
  p: PMethod;
  t1, t2: TMethod;
begin
  p := methodStack.Pop;
  t1.Code := p^.Code;
  t1.Data := p^.Data;
  //FreeAndNil(p);
  p := methodStack.Pop;
  t2.Code := p^.Code;
  t2.Data := p^.Data;
  //FreeAndNil(p);
  DBTree.OnChanging := TTVChangingEvent(t1);
  DBTree.OnChange := TTVChangedEvent(t2);
  if (invokeChanged <> nil) and (TMethod(DBTree.OnChange).Code <> nil) then DBTree.OnChange(Self, invokeChanged);
end;


procedure TMDIChild.SetSelectedTable(table: string);
var
  allnodes: TTreeNodes;
  i: integer;
begin
  DisableTreeEvents;
  allnodes := DBTree.Items;
  if allnodes.Count > 0 then begin
    // Round 1: Search case-sensitive.
    // Important after creating a table on servers with lower_case_table_names=0
    for i := 0 to allnodes.Count - 1 do begin
      if (allnodes[i].Level = 2) and (allnodes[i].Text = table) then begin
        allnodes[i].Selected := true;
        EnableTreeEvents;
        exit;
      end;
    end;
    // Round 2: Search case-insensitive
    // Important after creating a table on servers with lower_case_table_names=1
    for i := 0 to allnodes.Count - 1 do begin
      if (allnodes[i].Level = 2) and (AnsiCompareText(allnodes[i].Text, table) = 0) then begin
        allnodes[i].Selected := true;
        EnableTreeEvents;
        exit;
      end;
    end;
  end;
  EnableTreeEvents;
  raise Exception.Create('Node ' + table + ' not found in tree.');
end;


procedure TMDIChild.SetSelectedDatabase(db: string);
var
  allnodes: TTreeNodes;
  i: integer;
begin
  allnodes := DBTree.Items;
  if allnodes.Count > 0 then begin
    // Round 1: Search case-sensitive.
    // Important after creating a DB on servers with lower_case_table_names=0
    for i := 0 to allnodes.Count - 1 do begin
      if (allnodes[i].Level = 1) and (allnodes[i].Text = db) then begin
        allnodes[i].Selected := true;
        exit;
      end;
    end;
    // Round 2: Search case-insensitive
    // Important after creating a DB on servers with lower_case_table_names=1
    for i := 0 to allnodes.Count - 1 do begin
      if (allnodes[i].Level = 1) and (AnsiCompareText(allnodes[i].Text, db) = 0) then begin
        allnodes[i].Selected := true;
        exit;
      end;
    end;
  end;
  raise Exception.Create('Node ' + db + ' not found in tree.');
end;


{***
  Detect average row size and limit the number of rows fetched at
  once if more than ~ 5 MB of data
}
function TMDIChild.GetCalculatedLimit( Table: String ): Int64;
var
  AvgRowSize, RecordCount : Int64;
  ds: TDataSet;
const
  // how much overhead this application has per row
  ROW_SIZE_OVERHEAD : Integer = 1150;
  // average row size guess for mysql server < 5.0
  ROW_SIZE_GUESS: Integer = 2048;
  // round to nearest value when deciding limit
  ROUNDING: Integer = 1000;
begin
  result := -1;
  try
    ds := GetResults('SHOW TABLE STATUS LIKE ' + esc(Table));
    if ds = nil then exit;
    AvgRowSize := MakeInt( ds.FieldByName( 'Avg_row_length' ).AsString ) + ROW_SIZE_OVERHEAD;
    RecordCount := MakeInt( ds.FieldByName( 'Rows' ).AsString );
    if AvgRowSize * RecordCount > LOAD_SIZE then
    begin
      result := Trunc( LOAD_SIZE / AvgRowSize );
      result := (Trunc(result / ROUNDING) + 1) * ROUNDING;
      if result >= RecordCount then result := -1;
    end;
    ds.Close;
    FreeAndNil(ds);
  finally
    debug( 'GetCalculatedLimit: ' + formatnumber(result) );
  end;
end;


{**
  Column selection for datagrid
}
procedure TMDIChild.btnDataClick(Sender: TObject);
var
  btn : TSpeedButton;
  frm : TForm;
begin
  btn := (Sender as TSpeedButton);

  if btn.Down then
  begin
    // Create the desired form
    if btn = btnColumnSelection then
      frm := TColumnSelectionForm.Create(self)
    else if btn = btnDataSorting then
      frm := TDataSortingForm.Create(self)
    else
      frm := TForm.Create(self); // Dummy fallback, should never get created

    // Position new form relative to btn's position
    frm.Top := btn.ClientOrigin.Y + btn.Height;
    frm.Left := btn.ClientOrigin.X;

    // Display form and refresh data if needed
    if frm.ShowModal = mrOK then
      ViewData(self);

    btn.Down := False;
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

  case NewTab of
    0: // Cols
    begin
      // Keep native order of columns
      lboxQueryHelpers.Sorted := False;
      for i := 0 to High(VTRowDataListColumns) do
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
      lboxQueryHelpers.Items := MYSQL_KEYWORDS;
    end;

    3: // SQL Snippets
    begin
      // State of items in popupmenu
      lboxQueryHelpers.Items := getFilesFromDir( DIRNAME_SNIPPETS, '*.sql', true );
      SnippetsAccessible := lboxQueryHelpers.Items.Count > 0;
      menuDeleteSnippet.Enabled := SnippetsAccessible;
      menuInsertSnippetAtCursor.Enabled := SnippetsAccessible;
      menuLoadSnippet.Enabled := SnippetsAccessible;
      menuExplore.Enabled := True;
    end;

  end;

  // Restore last selected item in tab
  if (QueryHelpersSelectedItems[NewTab] > -1)
    and (QueryHelpersSelectedItems[NewTab] < lboxQueryHelpers.Count) then
  begin
    lboxQueryHelpers.ItemIndex := QueryHelpersSelectedItems[NewTab];
  end;

  lboxQueryHelpers.Items.EndUpdate;

end;



{**
  Insert string from listbox with query helpers into SQL
  memo at doubleclick
}
procedure TMDIChild.lboxQueryHelpersDblClick(Sender: TObject);
var
  itemtext : String;
begin
  itemtext := lboxQueryHelpers.Items[lboxQueryHelpers.ItemIndex];

  case tabsetQueryHelpers.TabIndex of
    3: // Load snippet file ínto query-memo
      QueryLoad( DIRNAME_SNIPPETS + itemtext + '.sql', False );
    else // For all other tabs just insert the item from the list
      SynMemoQuery.SelText := itemtext;
  end;

  SynMemoQuery.SetFocus;
end;


{**
  Remember last used items in query helper tabs
}
procedure TMDIChild.lboxQueryHelpersClick(Sender: TObject);
begin
  QueryHelpersSelectedItems[tabsetQueryHelpers.TabIndex] := lboxQueryHelpers.ItemIndex;
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
    SynMemoQueryChange(self);
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
  sql_update, sql_null, sql_default, sql_extra, DefaultValue : String;
  NodeData : PVTreeData;
begin
  // Try to rename, on any error abort and don't rename ListItem
  try
    ensureValidIdentifier( NewText );

    // Fetch data from listitem
    NodeData := ListColumns.GetNodeData(Node);

    // Fetch column definition
    def := GetResults( 'SHOW COLUMNS FROM ' + mask(SelectedTable) + ' LIKE ' + esc(NodeData.Captions[0]), False, False );

    // Check NOT NULL
    sql_null := 'NULL ';
    if UpperCase(def.FieldByName('Null').AsString) = 'NO' then
      sql_null := 'NOT NULL ';

    // Check default value, take care of non-literals / functions
    sql_default := '';
    DefaultValue := def.FieldByName('Default').AsString;
    if DefaultValue <> '' then
    begin
      if (UpperCase(def.FieldByName('Type').AsString) <> 'TIMESTAMP') and (DefaultValue <> 'CURRENT_TIMESTAMP') then
        DefaultValue := esc(DefaultValue);
      sql_default := 'DEFAULT ' + DefaultValue + ' ';
    end;

    // Check extra options (auto_increment)
    sql_extra := '';
    if def.FieldByName('Extra').AsString <> '' then
      sql_default := ' '+UpperCase(def.FieldByName('Extra').AsString) + ' ';

    // Concat column definition
    sql_update := 'ALTER TABLE ' + mask(SelectedTable) +
      ' CHANGE ' + mask(NodeData.Captions[0]) +
      ' ' + mask(NewText) + ' ' +
      def.FieldByName('Type').AsString + ' ' +
      sql_null +
      sql_default +
      sql_extra;

    // Cleanup
    def.Close;
    FreeAndNil(def);
    
    // Fire ALTER query
    ExecUpdateQuery( sql_update, False, False );

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
  // Display icon only for leftmost cell
  if Column <> 0 then
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
  VisibleColumns : TStringList;
  i : Integer;
begin
  VisibleColumns := TStringList.Create;
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
  Call context sensitive help from popupmenu (fx popupQuery)
}
procedure TMDIChild.menuSQLhelpClick(Sender: TObject);
begin
  CallSQLHelp( Sender );
end;


{**
  Modify existing database properties
}
procedure TMDIChild.menuAlterdatabaseClick(Sender: TObject);
begin
  if CreateDatabaseForm = nil then
    CreateDatabaseForm := TCreateDatabaseForm.Create(Self);

  CreateDatabaseForm.modifyDB := DBRightClickSelectedItem.Text;
  CreateDatabaseForm.ShowModal;
end;


{***
  Add a new query delimiter and select it
  @param term The delimiter to add and/or select
}
procedure TMDIChild.ComboBoxQueryDelimiterAdd( delimiter: String );
var
  index: Integer;
  found: Boolean;
  msg: String;
begin
  // See reference: mysql.cpp Ver 14.12 Distrib 5.0.45, for Win32 (ia32): Line 824
  // Check that delimiter does not contain a backslash
  msg := IsValidDelimiter( delimiter );
  if ( msg <> '' ) then
  begin
    // rollback the delimiter
    ComboBoxQueryDelimiter.Text := Self.delimiter;
    // notify the user
    raise Exception.Create( msg );
  end
  else
  begin
    // the delimiter is case-sensitive, following the implementation
    // in the MySQL CLI, so we must locate it by hand
    found := False;
    for index := 0 to ( ComboBoxQueryDelimiter.Items.Count - 1 ) do
    begin
      if ( ComboBoxQueryDelimiter.Items[index] = delimiter ) then
      begin
        ComboBoxQueryDelimiter.ItemIndex := index;
        found := True;
        break;
      end;
    end;

    if ( not found ) then
    begin
      ComboBoxQueryDelimiter.Items.Add( delimiter );
      ComboBoxQueryDelimiter.ItemIndex := ComboBoxQueryDelimiter.Items.Count - 1;
    end;

    Self.delimiter := ComboBoxQueryDelimiter.Text;
    LogSQL( Format( 'Delimiter changed to %s.', [delimiter] ));
  end;
end;


{***
  When ComboBoxQueryDelimiter lose the focus, defines the query delimiter

  @param Sender A object
}
procedure TMDIChild.ComboBoxQueryDelimiterExit(Sender: TObject);
var
  msg: String;
begin
  // a delimiter couldn't be empty
  ComboBoxQueryDelimiter.Text := Trim( ComboBoxQueryDelimiter.Text );

  // verify if the delimiter combobox isn't empty
  if ( ComboBoxQueryDelimiter.Text = EmptyStr ) then
  begin
    msg := 'A delimiter is needed.';
    MessageDlg( msg, mtWarning, [mbOK], 0);
    ComboBoxQueryDelimiter.SetFocus();
  end
  else
  // add the new delimiter to combobox
  begin
    ComboBoxQueryDelimiterAdd( ComboBoxQueryDelimiter.Text );
  end;
end;


{***
  Callback procedure able to handle client-side SQL statements such as DELIMITER

  @param command The command/option to be called
  @param parameter The parameter of command
}
procedure TMDIChild.ProcessClientSQL(command: String; parameter: String);
begin
  if ( command = 'DELIMITER' ) then
  begin
    ComboBoxQueryDelimiterAdd( parameter );
  end
  else
  if ( command = 'CLIENTSQL_ERROR' ) then
  begin
    LogSQL( parameter, True );
    if ( StopOnErrors ) then
      raise Exception.Create( parameter );
  end;
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
  ValueList : TStringList;
  reg : TRegistry;
begin
  reg := TRegistry.Create;
  reg.OpenKey( REGPATH, true );
  ValueList := TStringList.Create;

  // Column widths
  if reg.ValueExists( REGPREFIX_COLWIDTHS + List.Name ) then
  begin
    ValueList := Explode( ',', reg.ReadString( REGPREFIX_COLWIDTHS + List.Name ) );
    for i := 0 to ValueList.Count - 1 do
    begin
      colwidth := MakeInt(ValueList[i]);
      // Check if column number exists and width is at least 1 pixel
      if (List.Header.Columns.Count > i) and (colwidth > 0) then
        List.Header.Columns[i].Width := colwidth;
    end;
  end;

  // Column visibility
  if reg.ValueExists( REGPREFIX_COLSVISIBLE + List.Name ) then
  begin
    ValueList := Explode( ',', reg.ReadString( REGPREFIX_COLSVISIBLE + List.Name ) );
    SetVisibleListColumns( List, ValueList );
  end;

  // Column position
  if reg.ValueExists( REGPREFIX_COLPOS + List.Name ) then
  begin
    ValueList := Explode( ',', reg.ReadString( REGPREFIX_COLPOS + List.Name ) );
    for i := 0 to ValueList.Count - 1 do
    begin
      colpos := MakeInt(ValueList[i]);
      // Check if column number exists
      if List.Header.Columns.Count > i then
        List.Header.Columns[i].Position := colpos;
    end;
  end;

  reg.Free;
  ValueList.Free;
end;


{**
  (Un)hide columns in a VirtualStringTree.
}
procedure TMDIChild.SetVisibleListColumns( List: TVirtualStringTree; Columns: TStringList );
var
  i : Integer;
begin
  for i := 0 to List.Header.Columns.Count - 1 do
  begin
    if Columns.IndexOf( IntToStr(i) ) > -1 then
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
  popupFilter displays or a key was pressed which is assigned to
  it as a shortcut 
}
procedure TMDIChild.popupFilterPopup(Sender: TObject);
var
  somechars         : Boolean;
begin
  // Sets cursor into memo and activates TAction(s) like paste
  SynMemoFilter.SetFocus;
  somechars := SynMemoFilter.GetTextLen > 0;
  // Inserting file at cursor only makes sense with content
  MenuFilterClear.Enabled := somechars;
  menuFilterSQLHelp.Enabled := (mysql_version >= 40100) and (SynMemoFilter.WordAtCursor <> '');
  // Insert keyword into menuitem, so it's very clear what the menuitem does
  menuFilterSQLHelp.Caption := 'Lookup "'+sstr(SynMemoFilter.WordAtCursor,50)+'" in SQL help ...';
end;


{**
  Display tooltips in VirtualTrees. Imitates default behaviour of TListView.
}
procedure TMDIChild.vstGetHint(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; var LineBreakStyle:
    TVTTooltipLineBreakStyle; var HintText: WideString);
var
  NodeData : PVTreeData;
  r : TRect;
  DisplayedWidth,
  NeededWidth : Integer;
begin
  NodeData := Sender.GetNodeData( Node );
  if NodeData.Captions.Count > Column then
    HintText := NodeData.Captions[Column]
  else
    Exit;
  // Check if the list has shortened the text
  r := Sender.GetDisplayRect(Node, Column, True);
  DisplayedWidth := r.Right-r.Left;
  NeededWidth := Canvas.TextWidth(HintText) + TVirtualStringTree(Sender).TextMargin*2;
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
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect:
    TRect);
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
  reg_name : String;
  old_orderclause, new_orderclause, columnname : String;
  order_parts, ValidColumns : TStringList;
  columnexists : Boolean;
begin
  SetLength( Result, 0 );

  // Read ORDER clause from registry
  reg := TRegistry.Create();
  reg.OpenKey( REGPATH + '\Servers\' + FConn.Description, true );
  reg_name := REGPREFIX_ORDERCLAUSE + ActiveDatabase + '.' + SelectedTable;

  if reg.ValueExists(reg_name) then
  begin
    old_orderclause := reg.ReadString(reg_name);
    // Parse ORDER clause
    order_parts := explode( ',', old_orderclause );
    ValidColumns := GetVTCaptions( ListColumns );
    for i := 0 to order_parts.Count - 1 do
    begin
      columnname := Trim( Copy( order_parts[i], 0, LastPos( ' ', order_parts[i] ) ) );
      columnname := trimc( columnname, '`' );
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
    if new_orderclause <> '' then
      reg.WriteString(reg_name , new_orderclause)
    else
      reg.DeleteValue(reg_name);
  end;

  reg.Free;
end;


{**
  Concat all sort options to a ORDER clause
}
function TMDIChild.ComposeOrderClause(Cols: TOrderColArray): String;
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


end.
